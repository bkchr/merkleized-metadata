use std::{cell::RefCell, rc::Rc};

use crate::types::{self, MerkleTree};

pub struct Intermediate {
    pub types: Vec<TypeRef>,
    pub extrinsic_metadata: ExtrinsicMetadata,
}

/// A reference to a type in the registry.
pub type TypeRef = Rc<RefCell<Option<Type>>>;

#[derive(Clone, Debug)]
pub enum TypeDef {
    Composite(Vec<Field>),
    Enumeration(Vec<Variant>),
    Sequence(TypeRef),
    Array(TypeDefArray),
    Tuple(Vec<TypeRef>),
    Primitive(scale_info::TypeDefPrimitive),
    Compact(TypeRef),
    BitSequence(TypeDefBitSequence),
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: Option<String>,
    pub ty: TypeRef,
    pub type_name: Option<String>,
}

impl Field {
    pub fn as_basic_type(&self) -> types::Field {
        types::Field {
            name: self.name.clone(),
            ty: (*self.ty.borrow().as_ref().unwrap().unique_id.borrow()).into(),
            type_name: self.type_name.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<Field>,
    pub index: u8,
}

impl Variant {
    pub fn as_basic_type(&self) -> types::TypeDefVariant {
        types::TypeDefVariant {
            name: self.name.clone(),
            fields: self.fields.iter().map(|f| f.as_basic_type()).collect(),
            index: self.index,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeDefArray {
    pub len: u32,
    pub type_param: TypeRef,
}

impl TypeDefArray {
    pub fn as_basic_type(&self) -> types::TypeDefArray {
        types::TypeDefArray {
            len: self.len,
            type_param: self
                .type_param
                .borrow()
                .as_ref()
                .unwrap()
                .as_basic_type_ref(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeDefBitSequence {
    pub bit_store_type: TypeRef,
    pub bit_order_type: TypeRef,
}

impl TypeDefBitSequence {
    pub fn as_basic_type(&self) -> types::TypeDefBitSequence {
        types::TypeDefBitSequence {
            bit_store_type: self
                .bit_store_type
                .borrow()
                .as_ref()
                .unwrap()
                .as_basic_type_ref(),
            bit_order_type: self
                .bit_order_type
                .borrow()
                .as_ref()
                .unwrap()
                .as_basic_type_ref(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Type {
    /// The unique path to the type. Can be empty for built-in types
    pub path: Vec<String>,
    /// The generic type parameters of the type in use. Empty for non generic types
    pub type_params: Vec<TypeParameter>,
    /// The actual type definition
    pub type_def: TypeDef,
    pub unique_id: RefCell<u32>,
}

impl Type {
    pub fn as_basic_type(&self) -> types::Type {
        let type_def = match &self.type_def {
            TypeDef::Enumeration(v) => {
                let mut variants = v.clone();
                variants.sort_by_key(|v| v.index);
                let variant_root_hash =
                    MerkleTree::calculate_root(variants.iter().map(|v| v.as_basic_type().hash()));
                types::TypeDef::Enumeration(variant_root_hash)
            }
            TypeDef::Array(a) => types::TypeDef::Array(a.as_basic_type()),
            TypeDef::Composite(c) => {
                types::TypeDef::Composite(c.iter().map(|f| f.as_basic_type()).collect())
            }
            TypeDef::Sequence(s) => {
                types::TypeDef::Sequence(s.borrow().as_ref().unwrap().as_basic_type_ref())
            }
            TypeDef::Tuple(t) => types::TypeDef::Tuple(
                t.iter()
                    .map(|t| t.borrow().as_ref().unwrap().as_basic_type_ref())
                    .collect(),
            ),
            TypeDef::Primitive(p) => types::TypeDef::Primitive(match p {
                scale_info::TypeDefPrimitive::Bool => types::TypeDefPrimitive::Bool,
                scale_info::TypeDefPrimitive::Char => types::TypeDefPrimitive::Char,
                scale_info::TypeDefPrimitive::Str => types::TypeDefPrimitive::Str,
                scale_info::TypeDefPrimitive::U8 => types::TypeDefPrimitive::U8,
                scale_info::TypeDefPrimitive::U16 => types::TypeDefPrimitive::U16,
                scale_info::TypeDefPrimitive::U32 => types::TypeDefPrimitive::U32,
                scale_info::TypeDefPrimitive::U64 => types::TypeDefPrimitive::U64,
                scale_info::TypeDefPrimitive::U128 => types::TypeDefPrimitive::U128,
                scale_info::TypeDefPrimitive::U256 => types::TypeDefPrimitive::U256,
                scale_info::TypeDefPrimitive::I8 => types::TypeDefPrimitive::I8,
                scale_info::TypeDefPrimitive::I16 => types::TypeDefPrimitive::I16,
                scale_info::TypeDefPrimitive::I32 => types::TypeDefPrimitive::I32,
                scale_info::TypeDefPrimitive::I64 => types::TypeDefPrimitive::I64,
                scale_info::TypeDefPrimitive::I128 => types::TypeDefPrimitive::I128,
                scale_info::TypeDefPrimitive::I256 => types::TypeDefPrimitive::I256,
            }),
            TypeDef::Compact(c) => {
                types::TypeDef::Compact(c.borrow().as_ref().unwrap().as_basic_type_ref())
            }
            TypeDef::BitSequence(b) => types::TypeDef::BitSequence(b.as_basic_type()),
        };

        vec![types::Type {
            path: self.path.clone(),
            type_params: self.type_params.iter().map(|t| t.as_basic_type()).collect(),
            type_def,
        }]
    }

    pub fn as_basic_type_ref(&self) -> types::TypeRef {
        (*self.unique_id.borrow()).into()
    }
}

#[derive(Clone, Debug)]
pub struct TypeParameter {
    /// The name of the generic type parameter e.g. "T".
    pub name: String,
    /// The concrete type for the type parameter.
    ///
    /// `None` if the type parameter is skipped.
    pub ty: Option<TypeRef>,
}

impl TypeParameter {
    pub fn as_basic_type(&self) -> types::TypeParameter {
        types::TypeParameter {
            name: self.name.clone(),
            ty: self
                .ty
                .as_ref()
                .map(|t| t.borrow().as_ref().unwrap().as_basic_type_ref()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExtrinsicMetadata {
    /// Extrinsic version.
    pub version: u8,
    pub address_ty: TypeRef,
    pub call_ty: TypeRef,
    pub signature_ty: TypeRef,
    /// The type of the extra data added to the extrinsic.
    pub extra_ty: TypeRef,
    /// The signed extensions in the order they appear in the extrinsic.
    pub signed_extensions: Vec<SignedExtensionMetadata>,
}

impl ExtrinsicMetadata {
    pub fn as_basic_type(&self) -> types::ExtrinsicMetadata {
        types::ExtrinsicMetadata {
            version: self.version,
            address_ty: self
                .address_ty
                .borrow()
                .as_ref()
                .unwrap()
                .as_basic_type_ref(),
            call_ty: self.call_ty.borrow().as_ref().unwrap().as_basic_type_ref(),
            signature_ty: self
                .signature_ty
                .borrow()
                .as_ref()
                .unwrap()
                .as_basic_type_ref(),
            signed_extensions: self
                .signed_extensions
                .iter()
                .map(|se| se.as_basic_type())
                .collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SignedExtensionMetadata {
    pub identifier: String,
    pub included_in_extrinsic: TypeRef,
    pub included_in_signed_data: TypeRef,
}

impl SignedExtensionMetadata {
    pub fn as_basic_type(&self) -> types::SignedExtensionMetadata {
        types::SignedExtensionMetadata {
            identifier: self.identifier.clone(),
            ty: self
                .included_in_extrinsic
                .borrow()
                .as_ref()
                .unwrap()
                .as_basic_type_ref(),
            additional_signed: self
                .included_in_signed_data
                .borrow()
                .as_ref()
                .unwrap()
                .as_basic_type_ref(),
        }
    }
}
