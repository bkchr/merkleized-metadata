use crate::types::{Hash, Type};
use alloc::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    format,
    string::String,
    vec::Vec,
};
use codec::{Compact, Encode};
use core::cmp::Ordering;

/// A node of a [`MerkleTree`].
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum MerkleTreeNode {
    Node { left: Hash, right: Hash },
    Leaf { leaf_index: Compact<u32>, ty: Type },
}

impl MerkleTreeNode {
    fn hash(&self) -> Hash {
        match self {
            Self::Node { left, right } => blake3::hash(&(left, right).encode()).into(),
            Self::Leaf { ty, .. } => blake3::hash(&ty.encode()).into(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeId {
    Enumeration { type_id: u32, variant: u32 },
    Other(u32),
}

impl TypeId {
    /// Returns the actual `type_id`.
    pub fn type_id(&self) -> u32 {
        match self {
            Self::Enumeration { type_id, .. } => *type_id,
            Self::Other(id) => *id,
        }
    }
}

impl PartialOrd for TypeId {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TypeId {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (
                Self::Enumeration { type_id, variant },
                Self::Enumeration {
                    type_id: type_id_o,
                    variant: variant_o,
                },
            ) => {
                if type_id == type_id_o {
                    variant.cmp(variant_o)
                } else {
                    type_id.cmp(type_id_o)
                }
            }
            (s, o) => s.type_id().cmp(&o.type_id()),
        }
    }
}

/// A proof containing all the nodes to decode a specific extrinsic.
pub struct Proof {
    /// All the nodes required to decode the extrinsic.
    pub nodes: Vec<MerkleTreeNode>,
}

pub struct MerkleTree {
    root_hash: Hash,
    nodes: BTreeMap<Hash, MerkleTreeNode>,
    hash_to_type_ids: BTreeMap<Hash, BTreeSet<TypeId>>,
}

impl MerkleTree {
    pub fn new(leaves: impl IntoIterator<Item = (TypeId, Type)>) -> Self {
        let mut hash_to_type_ids = BTreeMap::<Hash, BTreeSet<TypeId>>::default();
        let mut nodes = BTreeMap::default();

        let mut intermediate_nodes = leaves
            .into_iter()
            .enumerate()
            .map(|(leaf_index, (type_id, ty))| {
                let element = MerkleTreeNode::Leaf {
                    ty,
                    leaf_index: (leaf_index as u32).into(),
                };

                let hash = element.hash();
                hash_to_type_ids.entry(hash).or_default().insert(type_id);

                nodes.insert(hash, element);

                hash
            })
            .collect::<VecDeque<_>>();

        while intermediate_nodes.len() > 1 {
            let right = intermediate_nodes
                .pop_back()
                .expect("We have more than one element; qed");
            let left = intermediate_nodes
                .pop_back()
                .expect("We have more than one element; qed");

            let element = MerkleTreeNode::Node { left, right };
            let hash = element.hash();

            hash_to_type_ids.insert(
                hash,
                hash_to_type_ids
                    .get(&left)
                    .cloned()
                    .unwrap_or_default()
                    .into_iter()
                    .chain(hash_to_type_ids.get(&right).cloned().unwrap_or_default())
                    .collect(),
            );

            nodes.insert(hash, element);

            intermediate_nodes.push_front(hash);
        }

        let root_hash = intermediate_nodes.pop_back().unwrap_or_default();

        Self {
            root_hash,
            nodes,
            hash_to_type_ids,
        }
    }

    pub fn root(&self) -> Hash {
        self.root_hash
    }

    pub fn build_proof(&self, type_ids: impl IntoIterator<Item = TypeId>) -> Result<Proof, String> {
        let mut nodes = BTreeSet::new();

        for type_id in type_ids.into_iter() {
            let mut process_node = self.root_hash;
            loop {
                let node = self.nodes.get(&process_node).ok_or_else(|| {
                    format!(
                        "Could not find node for hash: {}",
                        array_bytes::bytes2hex("0x", &process_node)
                    )
                })?;

                nodes.insert(node.clone());

                match node {
                    MerkleTreeNode::Node { left, right } => {
                        if self
                            .hash_to_type_ids
                            .get(left)
                            .map_or(false, |tids| tids.contains(&type_id))
                        {
                            process_node = *left;
                            continue;
                        } else if self
                            .hash_to_type_ids
                            .get(right)
                            .map_or(false, |tids| tids.contains(&type_id))
                        {
                            process_node = *right;
                            continue;
                        } else {
                            return Err(format!(
                                "Could not find type_id `{type_id:?}` from node `{}`.",
                                array_bytes::bytes2hex("0x", process_node)
                            ));
                        }
                    }
                    // We captured the leaf we are interested in.
                    MerkleTreeNode::Leaf { .. } => break,
                }
            }
        }

        Ok(Proof {
            nodes: nodes.into_iter().collect(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{TypeDef, TypeDefArray, TypeRef};

    #[test]
    fn merkle_tree_works() {
        for num_leaves in [5, 8, 10, 20, 23, 34, 37, 40] {
            println!("Running with {num_leaves}");

            let types = (0..num_leaves).map(|n| {
                (
                    TypeId::Other(n),
                    Type {
                        path: Vec::new(),
                        type_id: n.into(),
                        type_def: TypeDef::Array(TypeDefArray {
                            len: 1,
                            type_param: TypeRef::U8,
                        }),
                    },
                )
            });

            let merkle_tree = MerkleTree::new(types.clone());

            let mut levels = BTreeMap::from_iter([(0, vec![merkle_tree.root_hash])]);

            fn collect_levels(
                levels: &mut BTreeMap<u32, Vec<Hash>>,
                level: u32,
                merkle_tree: &MerkleTree,
                node_hash: Hash,
            ) {
                match merkle_tree.nodes.get(&node_hash).unwrap() {
                    MerkleTreeNode::Leaf { .. } => {}
                    MerkleTreeNode::Node { left, right } => {
                        levels.entry(level).or_default().push(*left);
                        levels.entry(level).or_default().push(*right);

                        collect_levels(levels, level + 1, merkle_tree, *left);
                        collect_levels(levels, level + 1, merkle_tree, *right);
                    }
                }
            }

            collect_levels(&mut levels, 1, &merkle_tree, merkle_tree.root_hash);
            assert!(!levels.is_empty());
            // Check that the numbers of levels is correct.
            assert_eq!(
                (merkle_tree.nodes.len() as f32).log2().ceil() as usize,
                levels.len()
            );

            // Ensure it is a complete binary tree
            while let Some((level, nodes)) = levels.pop_first() {
                if levels.is_empty() {
                    assert!(2u32.pow(level) >= nodes.len() as u32);
                } else {
                    assert_eq!(2u32.pow(level), nodes.len() as u32);
                }
            }
        }
    }
}
