use codec::{Compact, Encode};

/// A reference to a type in the registry.
pub type TypeRef = Compact<u32>;
pub type Hash = [u8; 32];

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub enum TypeDef {
    /// A composite type (e.g. a struct or a tuple)
    #[codec(index = 0)]
    Composite(Vec<Field>),
    /// A variant type (e.g. an enum)
    #[codec(index = 1)]
    Variant(TypeDefVariant),
    /// A sequence type with runtime known length.
    #[codec(index = 2)]
    Sequence(TypeRef),
    /// An array type with compile-time known length.
    #[codec(index = 3)]
    Array(TypeDefArray),
    /// A tuple type.
    #[codec(index = 4)]
    Tuple(Vec<TypeRef>),
    /// A Rust primitive type.
    #[codec(index = 5)]
    Primitive(TypeDefPrimitive),
    /// A type using the [`Compact`] encoding
    #[codec(index = 6)]
    Compact(TypeRef),
    /// A type representing a sequence of bits.
    #[codec(index = 7)]
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub struct TypeDefArray {
    pub len: u32,
    pub type_param: TypeRef,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Encode, Debug)]
pub enum TypeDefPrimitive {
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
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Encode, Debug)]
pub struct TypeDefBitSequence {
    pub bit_store_type: TypeRef,
    pub bit_order_type: TypeRef,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub struct Type {
    /// The unique path to the type. Can be empty for built-in types
    pub path: Vec<String>,
    /// The generic type parameters of the type in use. Empty for non generic types
    pub type_params: Vec<TypeParameter>,
    /// The actual type definition
    pub type_def: TypeDef,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Encode)]
pub struct TypeParameter {
    /// The name of the generic type parameter e.g. "T".
    pub name: String,
    /// The concrete type for the type parameter.
    ///
    /// `None` if the type parameter is skipped.
    pub ty: Option<TypeRef>,
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
pub enum TreeElement {
    Node { left: Hash, right: Hash },
    Leaf(Hash),
}

impl TreeElement {
    pub fn hash(&self) -> Hash {
        blake3::hash(&self.encode()).into()
    }
}
