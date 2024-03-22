use crate::types::{Hash, Type, TypeDef};
use alloc::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    format,
    string::String,
    vec::Vec,
};
use codec::{Compact, Encode};
use core::{cmp::Ordering, fmt::Debug};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NodeIndex(usize);

impl NodeIndex {
    fn is_root(self) -> bool {
        self.0 == 0
    }

    fn parent(self) -> Self {
        if self.0 == 0 {
            Self(0)
        } else {
            Self((self.0 - 1) / 2)
        }
    }

    fn is_left_child(self) -> bool {
        self.0 % 2 == 1
    }

    fn level(self) -> usize {
        (self.0 + 1).ilog2() as _
    }

    fn is_sibling_of(self, other: Self) -> bool {
        self.parent() == other.parent()
    }

    fn right_child(self) -> Self {
        Self(self.0 * 2 + 2)
    }

    fn left_child(self) -> Self {
        Self(self.0 * 2 + 1)
    }

    /// Returns `true` if `other` is a descendent.
    fn is_descendent(mut self, mut other: Self) -> bool {
        // If the index is `0`, it is the root
        if self.0 == 0 {
            return true;
        }

        // If the index is greater, it can not be a descendent
        if self.0 > other.0 {
            return false;
        }

        let level0 = self.level();
        let level1 = other.level();

        // Check if applying X times the parent function leads to
        // the expected `index`. X is the level difference
        self.0 + 1 == (other.0 + 1) >> (level1 - level0)
    }
}

/// A proof containing all the nodes to decode a specific extrinsic.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Proof {
    pub leaves: Vec<Type>,
    pub leaf_indices: Vec<u32>,
    pub nodes: Vec<Hash>,
}

pub struct MerkleTree {
    root_hash: Hash,
    nodes: BTreeMap<Hash, MerkleTreeNode>,
    type_id_to_leaf_index: BTreeMap<TypeId, usize>,
    node_index_to_hash: BTreeMap<NodeIndex, Hash>,
}

impl Debug for MerkleTree {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        writeln!(f, "MerkleTree")?;

        let mut level = 0;
        write!(
            f,
            "0: {}",
            array_bytes::bytes2hex(
                "0x",
                self.node_index_to_hash
                    .get(&NodeIndex(0))
                    .copied()
                    .unwrap_or(Hash::default())
            )
        )?;
        for i in 1..self.node_index_to_hash.len() {
            let node = NodeIndex(i);

            if node.level() != level {
                level = node.level();
                write!(f, "\n{level}: ")?;
            }

            write!(
                f,
                "{} ",
                array_bytes::bytes2hex(
                    "0x",
                    self.node_index_to_hash
                        .get(&node)
                        .copied()
                        .unwrap_or(Hash::default())
                )
            )?;
        }

        Ok(())
    }
}

impl MerkleTree {
    pub fn new(leaves: impl IntoIterator<Item = (TypeId, Type)>) -> Self {
        let mut nodes = BTreeMap::default();

        let mut type_id_to_leaf_index = BTreeMap::<TypeId, usize>::default();

        let mut intermediate_nodes = leaves
            .into_iter()
            .enumerate()
            .map(|(leaf_index, (type_id, ty))| {
                let element = MerkleTreeNode::Leaf {
                    ty,
                    leaf_index: (leaf_index as u32).into(),
                };

                let hash = element.hash();
                type_id_to_leaf_index.insert(type_id, leaf_index);

                nodes.insert(hash, element);

                hash
            })
            .collect::<VecDeque<_>>();

        let mut hashes = VecDeque::<Hash>::default();

        while intermediate_nodes.len() > 1 {
            let right = intermediate_nodes
                .pop_back()
                .expect("We have more than one element; qed");
            hashes.push_front(right);
            let left = intermediate_nodes
                .pop_back()
                .expect("We have more than one element; qed");
            hashes.push_front(left);

            let element = MerkleTreeNode::Node { left, right };
            let hash = element.hash();

            nodes.insert(hash, element);

            intermediate_nodes.push_front(hash);
        }

        let root_hash = intermediate_nodes.pop_back().unwrap_or_default();
        hashes.push_front(root_hash);

        Self {
            root_hash,
            nodes,
            type_id_to_leaf_index,
            node_index_to_hash: hashes
                .into_iter()
                .enumerate()
                .map(|(i, h)| (NodeIndex(i), h))
                .collect(),
        }
    }

    pub fn root(&self) -> Hash {
        self.root_hash
    }

    pub fn build_proof(&self, type_ids: impl IntoIterator<Item = TypeId>) -> Result<Proof, String> {
        let mut leaf_node_indices = Vec::new();

        for type_id in type_ids.into_iter() {
            let leaf_index = self
                .type_id_to_leaf_index
                .get(&type_id)
                .ok_or_else(|| format!("Could not find leaf index for type id `{type_id:?}`"))?;
            // The leaves have the highest node indices. Thus, we just need to
            // subtract from the last node index the reverse index of the leaf.
            let node_index =
                self.nodes.len() - 1 - (self.type_id_to_leaf_index.len() - 1 - leaf_index);

            leaf_node_indices.push(NodeIndex(node_index));
        }

        // Sort the leave node indices to get the left most leaf first.
        leaf_node_indices.sort_by(|l, r| r.level().cmp(&l.level()).then_with(|| l.0.cmp(&r.0)));

        let mut node_hashes = Vec::new();

        let mut iter = leaf_node_indices.iter().peekable();

        if let Some(leaf_node_index) = iter.next() {
            self.collect_node_hashes(
                *leaf_node_index,
                *leaf_node_index,
                &mut iter,
                &mut node_hashes,
            )?;
        }

        let leaves = leaf_node_indices
            .iter()
            .map(|node_index| {
                let hash = self
                    .node_index_to_hash
                    .get(dbg!(node_index))
                    .ok_or_else(|| format!("Could not find hash for {node_index:?}"))?;
                let node = self.nodes.get(hash).ok_or_else(|| {
                    format!(
                        "Could not find node for hash `{}`",
                        array_bytes::bytes2hex("0x", hash)
                    )
                })?;

                match node {
                    MerkleTreeNode::Leaf { ty, .. } => Ok(ty.clone()),
                    MerkleTreeNode::Node { .. } => Err(format!(
                        "Expected leaf, found node for hash `{}`",
                        array_bytes::bytes2hex("0x", hash)
                    )),
                }
            })
            .collect::<Result<Vec<_>, String>>()?;

        Ok(Proof {
            leaves,
            leaf_indices: leaf_node_indices.into_iter().map(|i| i.0 as _).collect(),
            nodes: node_hashes,
        })
    }

    fn collect_node_hashes<'a>(
        &self,
        first_node_index: NodeIndex,
        leaf_node_index: NodeIndex,
        leaves: &mut impl Iterator<Item = &'a NodeIndex>,
        node_hashes: &mut Vec<Hash>,
    ) -> Result<(), String> {
        let mut node_index = leaf_node_index;
        let node_hashes_pos = node_hashes.len();
        let mut next_leaf_opt = leaves.next();

        while !node_index.is_root() {
            let parent = dbg!(node_index.parent());

            if parent == first_node_index {
                return Ok(());
            }

            if node_index.is_left_child() {
                let right_child = parent.right_child();
                if let Some(next_leaf) = next_leaf_opt {
                    if *next_leaf == right_child {
                        next_leaf_opt = leaves.next();
                        node_index = parent;
                        continue;
                    } else if right_child.is_descendent(*next_leaf) {
                        self.collect_node_hashes(parent, *next_leaf, leaves, node_hashes)?;
                        node_index = parent;
                        continue;
                    }
                }

                let hash = self.node_index_to_hash.get(&right_child).ok_or_else(|| {
                    format!("Could not find hash for right child `{right_child:?}`.")
                })?;
                node_hashes.push(*hash);
            } else {
                // As the leaves are sorted from left to right, the left child wasn't added yet.
                let left_child = parent.left_child();
                let hash = self.node_index_to_hash.get(&left_child).ok_or_else(|| {
                    format!("Could not find hash for left child `{left_child:?}`.")
                })?;

                node_hashes.insert(node_hashes_pos, *hash);
            }

            node_index = parent;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use codec::Decode;
    use frame_metadata::RuntimeMetadataPrefixed;

    use super::*;
    use crate::{
        extrinsic_decoder::decode_extrinsic_and_collect_type_ids,
        from_frame_metadata::FrameMetadataPrepared,
        generate_proof_for_extrinsic,
        types::{TypeDef, TypeDefArray, TypeRef},
    };

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

    fn get_hash(
        leaf_indices: &mut &[u32],
        leaves: &mut &[Type],
        nodes: &mut &[Hash],
        node_index: NodeIndex,
        merkle_tree: &MerkleTree,
    ) -> Hash {
        let is_descendent = if leaf_indices.is_empty() {
            false
        } else {
            let current_leaf = NodeIndex(leaf_indices[0] as usize);

            if node_index == dbg!(current_leaf) {
                let hash = blake3::hash(&leaves[0].encode());

                *leaves = &leaves[1..];
                *leaf_indices = &leaf_indices[1..];
                return hash.into();
            }

            node_index.is_descendent(current_leaf)
        };

        if dbg!(!is_descendent) {
            let res = nodes[0];
            *nodes = &nodes[1..];
            return res;
        }

        let left_child = dbg!(node_index.left_child());
        let left = get_hash(leaf_indices, leaves, nodes, left_child, merkle_tree);

        assert_eq!(
            left,
            *merkle_tree.node_index_to_hash.get(&left_child).unwrap(),
            "Found wrong {left_child:?}"
        );

        dbg!(array_bytes::bytes2hex("left: 0x", &left));

        let right_child = dbg!(node_index.right_child());
        let right = get_hash(leaf_indices, leaves, nodes, right_child, merkle_tree);
        dbg!(array_bytes::bytes2hex("right: 0x", &right));

        assert_eq!(
            right,
            *merkle_tree.node_index_to_hash.get(&right_child).unwrap(),
            "Found wrong {right_child:?}"
        );

        blake3::hash(&(left, right).encode()).into()
    }

    #[test]
    fn generate_proof() {
        // `Balances::transfer_keep_alive`
        let ext = "0x2d028400d43593c715fdd31c61141abd04a99fd6822c8558854ccde39a5684e7a56da27d01bce7c8f572d39cee240e3d50958f68a5c129e0ac0d4eb9222de70abdfa8c44382a78eded433782e6b614a97d8fd609a3f20162f3f3b3c16e7e8489b2bd4fa98c070000000403008eaf04151687736326c9fea17e25fc5287613693c912909cb226aa4794f26a4828";
        let additional_signed = "0x00b2590f001800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";

        let metadata = String::from_utf8(
            fs::read(format!(
                "{}/fixtures/rococo_metadata_v15",
                env!("CARGO_MANIFEST_DIR")
            ))
            .unwrap(),
        )
        .unwrap();

        let metadata = Option::<Vec<u8>>::decode(
            &mut &array_bytes::hex2bytes(metadata.strip_suffix("\n").unwrap()).unwrap()[..],
        )
        .unwrap()
        .unwrap();

        let metadata = RuntimeMetadataPrefixed::decode(&mut &metadata[..])
            .unwrap()
            .1;

        let proof = generate_proof_for_extrinsic(
            &array_bytes::hex2bytes(ext).unwrap(),
            Some(&array_bytes::hex2bytes(additional_signed).unwrap()),
            &metadata,
        )
        .unwrap();

        let prepared = FrameMetadataPrepared::prepare(&metadata).unwrap();
        let type_information = prepared.as_type_information();

        // Check that we have included all the required types in the proof.
        let accessed_types = decode_extrinsic_and_collect_type_ids(
            &array_bytes::hex2bytes(ext).unwrap(),
            Some(&array_bytes::hex2bytes(additional_signed).unwrap()),
            &type_information,
            proof.leaves.iter(),
        )
        .unwrap();

        let merkle_tree = MerkleTree::new(prepared.as_type_information().types);
        let proof2 = merkle_tree.build_proof(accessed_types).unwrap();

        assert_eq!(proof, proof2);

        let mut node = NodeIndex(proof.leaf_indices[0] as _);
        while !node.is_root() {
            dbg!(node);
            node = node.parent();
        }

        let root_hash = get_hash(
            &mut &proof.leaf_indices[..],
            &mut &proof.leaves[..],
            &mut &proof.nodes[..],
            NodeIndex(0),
            &merkle_tree,
        );
        assert_eq!(
            array_bytes::bytes2hex("0x", dbg!(merkle_tree).root()),
            array_bytes::bytes2hex("0x", root_hash)
        );
    }

    #[test]
    fn lol() {
        let some = NodeIndex(2);
        let some2 = NodeIndex(3139);

        assert!(some.is_descendent(some2));
    }
}
