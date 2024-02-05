use std::collections::VecDeque;

use codec::{Compact, Encode};

/// A reference to a type in the registry.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub enum TypeRef {
    #[codec(index = 0)]
    Bool,
    #[codec(index = 1)]
    Char,
    #[codec(index = 2)]
    Str,
    #[codec(index = 3)]
    U8,
    #[codec(index = 4)]
    U16,
    #[codec(index = 5)]
    U32,
    #[codec(index = 6)]
    U64,
    #[codec(index = 7)]
    U128,
    #[codec(index = 8)]
    U256,
    #[codec(index = 9)]
    I8,
    #[codec(index = 10)]
    I16,
    #[codec(index = 11)]
    I32,
    #[codec(index = 12)]
    I64,
    #[codec(index = 13)]
    I128,
    #[codec(index = 14)]
    I256,
    #[codec(index = 15)]
    CompactU8,
    #[codec(index = 16)]
    CompactU16,
    #[codec(index = 17)]
    CompactU32,
    #[codec(index = 18)]
    CompactU64,
    #[codec(index = 19)]
    CompactU128,
    #[codec(index = 20)]
    CompactVoid,
    #[codec(index = 21)]
    ById(Compact<u32>),
}

pub type Hash = [u8; 32];

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub enum TypeDef {
    /// A composite type (e.g. a struct or a tuple)
    #[codec(index = 0)]
    Composite(Vec<Field>),
    /// An enumeration.
    #[codec(index = 1)]
    Enumeration(Hash),
    /// A sequence type with runtime known length.
    #[codec(index = 2)]
    Sequence(TypeRef),
    /// An array type with compile-time known length.
    #[codec(index = 3)]
    Array(TypeDefArray),
    /// A tuple type.
    #[codec(index = 4)]
    Tuple(Vec<TypeRef>),
    /// A type representing a sequence of bits.
    #[codec(index = 5)]
    BitSequence(TypeDefBitSequence),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub struct Field {
    pub name: Option<String>,
    pub ty: TypeRef,
    pub type_name: Option<String>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub struct TypeDefVariant {
    pub name: String,
    pub fields: Vec<Field>,
    pub index: u8,
}

impl TypeDefVariant {
    pub fn hash(&self) -> Hash {
        blake3::hash(&self.encode()).into()
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub struct TypeDefArray {
    pub len: u32,
    pub type_param: TypeRef,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Encode, Debug)]
pub struct TypeDefBitSequence {
    pub bit_store_type: TypeRef,
    pub least_significant_bit_first: bool,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub struct Type {
    /// The unique path to the type. Can be empty for built-in types
    pub path: Vec<String>,
    /// The actual type definition
    pub type_def: TypeDef,
}

impl Type {
    pub fn hash(&self) -> Hash {
        blake3::hash(&self.encode()).into()
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub struct ExtrinsicMetadata {
    /// Extrinsic version.
    pub version: u8,
    pub address_ty: TypeRef,
    pub call_ty: TypeRef,
    pub signature_ty: TypeRef,
    /// The signed extensions in the order they appear in the extrinsic.
    pub signed_extensions: Vec<SignedExtensionMetadata>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub struct SignedExtensionMetadata {
    pub identifier: String,
    pub ty: TypeRef,
    pub additional_signed: TypeRef,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub enum MetadataDigest {
    Disabled,
    V1 {
        types_tree_root: Hash,
        extrinsic_metadata_hash: Hash,
        spec_version: u32,
        spec_name: String,
        base58_prefix: u16,
        decimals: u8,
        token_symbol: String,
    },
}

impl MetadataDigest {
    pub fn hash(&self) -> Hash {
        blake3::hash(&self.encode()).into()
    }
}

#[derive(Debug, Encode)]
pub enum MerkleTree {
    Node { left: Hash, right: Hash },
    Leaf(Hash),
}

impl MerkleTree {
    pub fn hash(&self) -> Hash {
        blake3::hash(&self.encode()).into()
    }

    pub fn calculate_root(leaves: impl IntoIterator<Item = Hash>) -> Hash {
        let mut leaves = leaves
            .into_iter()
            .map(MerkleTree::Leaf)
            .collect::<VecDeque<_>>();

        let mut nodes = VecDeque::new();

        while leaves.len() > 1 {
            let left = leaves
                .pop_front()
                .expect("We have more than one element; qed");
            let right = leaves
                .pop_front()
                .expect("We have more than one element; qed");

            nodes.push_back(MerkleTree::Node {
                left: left.hash(),
                right: right.hash(),
            })
        }

        if let Some(last) = leaves.pop_front() {
            if let Some(back) = nodes.back_mut() {
                *back = MerkleTree::Node {
                    left: back.hash(),
                    right: last.hash(),
                };
            } else {
                nodes.push_back(last);
            }
        }

        while nodes.len() > 1 {
            let left = nodes
                .pop_front()
                .expect("We have more than one element; qed");
            let right = nodes
                .pop_front()
                .expect("We have more than one element; qed");

            nodes.push_back(MerkleTree::Node {
                left: left.hash(),
                right: right.hash(),
            })
        }

        nodes
            .pop_back()
            .map_or_else(|| Default::default(), |n| n.hash())
    }
}
