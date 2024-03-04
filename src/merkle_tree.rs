use std::collections::{
    btree_map::{Entry, OccupiedEntry},
    BTreeMap, BTreeSet, VecDeque,
};

use crate::types::{Hash, Type, TypeDef};
use codec::{Compact, Encode};

/// A node of a [`MerkleTree`].
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum MerkleTreeNode {
    Node {
        left: Hash,
        right: Hash,
    },
    Leaf {
        leaf_index: Compact<u32>,
        type_id: Compact<u32>,
        ty: Type,
    },
}

impl MerkleTreeNode {
    fn hash(&self) -> Hash {
        match self {
            Self::Node { left, right } => blake3::hash(&(left, right).encode()).into(),
            Self::Leaf { ty, .. } => blake3::hash(&ty.encode()).into(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeId {
    Enumeration { type_id: u32, variant: u32 },
    Other(u32),
}

/// A proof containing all the nodes to decode a specific extrinsic.
pub struct Proof {
    /// All the nodes required to decode the extrinsic.
    pub nodes: Vec<MerkleTreeNode>,
}

pub struct MerkleTree {
    root_hash: Hash,
    nodes: BTreeMap<Hash, MerkleTreeNode>,
    hash_to_type_ids: BTreeMap<Hash, Vec<TypeId>>,
    type_id_to_hash: BTreeMap<TypeId, Hash>,
}

impl MerkleTree {
    pub fn new(leaves: impl IntoIterator<Item = (u32, Type)>) -> Self {
        let mut hash_to_type_ids = BTreeMap::default();
        let mut type_id_to_hash = BTreeMap::default();
        let mut nodes = BTreeMap::default();

        let mut intermediate_nodes = leaves
            .into_iter()
            .enumerate()
            .map(|(leaf_index, (type_id, ty))| {
                let maybe_variant_index = ty.type_def.as_enumeration().map(|v| v.index);

                let element = MerkleTreeNode::Leaf {
                    ty,
                    leaf_index: (leaf_index as u32).into(),
                    type_id: type_id.into(),
                };

                let type_id = maybe_variant_index.map_or_else(
                    || TypeId::Other(type_id),
                    |v| TypeId::Enumeration {
                        type_id,
                        variant: v as u32,
                    },
                );

                let hash = element.hash();
                hash_to_type_ids.insert(hash, vec![type_id]);

                type_id_to_hash.insert(type_id, hash);

                nodes.insert(hash, element);

                hash
            })
            .collect::<VecDeque<_>>();

        while intermediate_nodes.len() > 1 {
            let left = intermediate_nodes
                .pop_front()
                .expect("We have more than one element; qed");
            let right = intermediate_nodes
                .pop_front()
                .expect("We have more than one element; qed");

            let element = MerkleTreeNode::Node { left, right };
            let hash = element.hash();

            intermediate_nodes.push_back(hash);

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
        }

        let root_hash = intermediate_nodes.pop_back().unwrap_or_default();

        Self {
            root_hash,
            nodes,
            type_id_to_hash,
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
                let node = self
                    .nodes
                    .get(&process_node)
                    .ok_or_else(|| format!("Could not find node for hash: {process_node:?}"))?;

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
                            process_node = *left;
                            continue;
                        } else {
                            return Err(format!("Could not find type_id ({type_id:?}) from node ({process_node:?})."));
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
