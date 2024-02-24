use std::{cell::RefCell, collections::HashSet, rc::Rc};

use scale_info::{Field, Type, TypeDef, TypeDefArray, TypeDefBitSequence, Variant};

use crate::types;

pub struct Intermediate {
    pub types: Vec<TypeRef>,
    pub extrinsic_metadata: ExtrinsicMetadata,
}

pub trait AsBasicTypeRef {
    fn as_basic_type_ref(&self) -> types::TypeRef;
}

pub trait AsBasicType {
    type BasicType;

    fn as_basic_type(&self) -> Self::BasicType;
}

pub trait IsBasicType {
    fn is_basic_type(&self) -> bool;
}

impl AsBasicTypeRef for Type {
    fn as_basic_type_ref(&self) -> types::TypeRef {
        let mut collector = CollectPrimitives::default();
        collector.visit_type(&mut Default::default(), self);

        match &self.type_def {
            TypeDef::Primitive(p) => match p {
                scale_info::TypeDefPrimitive::Bool => types::TypeRef::Bool,
                scale_info::TypeDefPrimitive::Char => types::TypeRef::Char,
                scale_info::TypeDefPrimitive::Str => types::TypeRef::Str,
                scale_info::TypeDefPrimitive::U8 => types::TypeRef::U8,
                scale_info::TypeDefPrimitive::U16 => types::TypeRef::U16,
                scale_info::TypeDefPrimitive::U32 => types::TypeRef::U32,
                scale_info::TypeDefPrimitive::U64 => types::TypeRef::U64,
                scale_info::TypeDefPrimitive::U128 => types::TypeRef::U128,
                scale_info::TypeDefPrimitive::U256 => types::TypeRef::U256,
                scale_info::TypeDefPrimitive::I8 => types::TypeRef::I8,
                scale_info::TypeDefPrimitive::I16 => types::TypeRef::I16,
                scale_info::TypeDefPrimitive::I32 => types::TypeRef::I32,
                scale_info::TypeDefPrimitive::I64 => types::TypeRef::I64,
                scale_info::TypeDefPrimitive::I128 => types::TypeRef::I128,
                scale_info::TypeDefPrimitive::I256 => types::TypeRef::I256,
            },
            TypeDef::Compact(_) => {
                if collector.found.len() > 1 {
                    panic!("Unexpected: {:?}", collector.found)
                } else if let Some(found) = collector.found.first() {
                    match found {
                        scale_info::TypeDefPrimitive::U8 => types::TypeRef::CompactU8,
                        scale_info::TypeDefPrimitive::U16 => types::TypeRef::CompactU16,
                        scale_info::TypeDefPrimitive::U32 => types::TypeRef::CompactU32,
                        scale_info::TypeDefPrimitive::U64 => types::TypeRef::CompactU64,
                        scale_info::TypeDefPrimitive::U128 => types::TypeRef::CompactU128,
                        p => panic!("Unsupported primitive type for `Compact`: {p:?}"),
                    }
                } else {
                    types::TypeRef::CompactVoid
                }
            }
            _ => types::TypeRef::ById(self.unique_id().into()),
        }
    }
}

impl AsBasicType for Type {
    type BasicType = Vec<types::Type>;

    fn as_basic_type(&self) -> Self::BasicType {
        let type_def = match &self.type_def {
            TypeDef::Compact(_) | TypeDef::Primitive(_) => return Vec::new(),
            TypeDef::Enumeration(v) => {
                let mut variants = v.clone();
                variants.sort_by_key(|v| v.index);

                return variants
                    .iter()
                    .map(|v| types::Type {
                        path: self.path.clone(),
                        type_def: types::TypeDef::Enumeration(v.as_basic_type()),
                    })
                    .collect::<Vec<_>>();
            }
            TypeDef::Array(a) => types::TypeDef::Array(a.as_basic_type()),
            TypeDef::Composite(c) => {
                types::TypeDef::Composite(c.iter().map(|f| f.as_basic_type()).collect())
            }
            TypeDef::Sequence(s) => {
                types::TypeDef::Sequence(s.borrow().expect_resolved().as_basic_type_ref())
            }
            TypeDef::Tuple(t) => types::TypeDef::Tuple(
                t.iter()
                    .map(|t| t.borrow().expect_resolved().as_basic_type_ref())
                    .collect(),
            ),
            TypeDef::BitSequence(b) => types::TypeDef::BitSequence(b.as_basic_type()),
        };

        vec![types::Type {
            path: self.path.clone(),
            type_def,
        }]
    }
}

impl IsBasicType for Type {
    fn is_basic_type(&self) -> bool {
        match self.type_def {
            TypeDef::Compact(_) | TypeDef::Primitive(_) => false,
            _ => true,
        }
    }
}

impl AsBasicType for Field {
    type BasicType = types::Field;

    fn as_basic_type(&self) -> Self::BasicType {
        types::Field {
            name: self
                .name
                .as_ref()
                .map(|n| AsRef::<str>::as_ref(n).to_string()),
            ty: self.ty.as_basic_type_ref(),
            type_name: self
                .type_name
                .as_ref()
                .map(|n| AsRef::<str>::as_ref(n).to_string()),
        }
    }
}

impl AsBasicType for Variant {
    type BasicType = types::EnumerationVariant;

    fn as_basic_type(&self) -> types::EnumerationVariant {
        types::EnumerationVariant {
            name: self
                .name
                .as_ref()
                .map(|n| AsRef::<str>::as_ref(n).to_string()),
            fields: self.fields.iter().map(|f| f.as_basic_type()).collect(),
            index: self.index,
        }
    }
}

impl AsBasicType for TypeDefArray {
    type BasicType = types::TypeDefArray;

    fn as_basic_type(&self) -> types::TypeDefArray {
        types::TypeDefArray {
            len: self.len,
            type_param: self
                .type_param
                .as_basic_type_ref(),
        }
    }
}

impl AsBasicType for TypeDefBitSequence {
    type BasicType = types::TypeDefBitSequence;

    fn as_basic_type(&self) -> types::TypeDefBitSequence {
        types::TypeDefBitSequence {
            bit_store_type: self
                .bit_store_type
                .borrow()
                .expect_resolved()
                .as_basic_type_ref(),
            least_significant_bit_first: self
                .bit_order_type
                .borrow()
                .expect_resolved()
                .path
                .iter()
                .find(|p| *p == "Lsb0" || *p == "Msb0")
                .expect("`bit_order_type` is either `Lsb0` or `Msb0`")
                == "Lsb0",
        }
    }
}

#[derive(Default)]
struct CollectPrimitives {
    found: Vec<scale_info::TypeDefPrimitive>,
}

impl Visitor for CollectPrimitives {
    fn visit_primitive(&mut self, primitive: &scale_info::TypeDefPrimitive) {
        self.found.push(primitive.clone());
    }
}

impl Type {
    pub fn unique_id(&self) -> u32 {
        *self.unique_id.borrow()
    }

    pub fn set_unique_id(&mut self, id: u32) {
        *self.unique_id.borrow_mut() = id;
    }

    pub fn as_basic_type(&self) -> Vec<types::Type> {
        let type_def = match &self.type_def {
            TypeDef::Compact(_) | TypeDef::Primitive(_) => return Vec::new(),
            TypeDef::Enumeration(v) => {
                let mut variants = v.clone();
                variants.sort_by_key(|v| v.index);

                return variants
                    .iter()
                    .map(|v| types::Type {
                        path: self.path.clone(),
                        type_def: types::TypeDef::Enumeration(v.as_basic_type()),
                    })
                    .collect::<Vec<_>>();
            }
            TypeDef::Array(a) => types::TypeDef::Array(a.as_basic_type()),
            TypeDef::Composite(c) => {
                types::TypeDef::Composite(c.iter().map(|f| f.as_basic_type()).collect())
            }
            TypeDef::Sequence(s) => {
                types::TypeDef::Sequence(s.borrow().expect_resolved().as_basic_type_ref())
            }
            TypeDef::Tuple(t) => types::TypeDef::Tuple(
                t.iter()
                    .map(|t| t.borrow().expect_resolved().as_basic_type_ref())
                    .collect(),
            ),
            TypeDef::BitSequence(b) => types::TypeDef::BitSequence(b.as_basic_type()),
        };

        vec![types::Type {
            path: self.path.clone(),
            type_def,
        }]
    }

    pub fn as_basic_type_ref(&self) -> types::TypeRef {
        let mut collector = CollectPrimitives::default();
        collector.visit_type(&mut Default::default(), self);

        match &self.type_def {
            TypeDef::Primitive(p) => match p {
                scale_info::TypeDefPrimitive::Bool => types::TypeRef::Bool,
                scale_info::TypeDefPrimitive::Char => types::TypeRef::Char,
                scale_info::TypeDefPrimitive::Str => types::TypeRef::Str,
                scale_info::TypeDefPrimitive::U8 => types::TypeRef::U8,
                scale_info::TypeDefPrimitive::U16 => types::TypeRef::U16,
                scale_info::TypeDefPrimitive::U32 => types::TypeRef::U32,
                scale_info::TypeDefPrimitive::U64 => types::TypeRef::U64,
                scale_info::TypeDefPrimitive::U128 => types::TypeRef::U128,
                scale_info::TypeDefPrimitive::U256 => types::TypeRef::U256,
                scale_info::TypeDefPrimitive::I8 => types::TypeRef::I8,
                scale_info::TypeDefPrimitive::I16 => types::TypeRef::I16,
                scale_info::TypeDefPrimitive::I32 => types::TypeRef::I32,
                scale_info::TypeDefPrimitive::I64 => types::TypeRef::I64,
                scale_info::TypeDefPrimitive::I128 => types::TypeRef::I128,
                scale_info::TypeDefPrimitive::I256 => types::TypeRef::I256,
            },
            TypeDef::Compact(_) => {
                if collector.found.len() > 1 {
                    panic!("Unexpected: {:?}", collector.found)
                } else if let Some(found) = collector.found.first() {
                    match found {
                        scale_info::TypeDefPrimitive::U8 => types::TypeRef::CompactU8,
                        scale_info::TypeDefPrimitive::U16 => types::TypeRef::CompactU16,
                        scale_info::TypeDefPrimitive::U32 => types::TypeRef::CompactU32,
                        scale_info::TypeDefPrimitive::U64 => types::TypeRef::CompactU64,
                        scale_info::TypeDefPrimitive::U128 => types::TypeRef::CompactU128,
                        p => panic!("Unsupported primitive type for `Compact`: {p:?}"),
                    }
                } else {
                    types::TypeRef::CompactVoid
                }
            }
            _ => types::TypeRef::ById(self.unique_id().into()),
        }
    }
}

pub trait Visitor {
    fn visit_type_def(&mut self, already_visited: &mut HashSet<u32>, type_def: &TypeDef) {
        visit_type_def(self, already_visited, type_def)
    }

    fn visit_type(&mut self, already_visited: &mut HashSet<u32>, ty: &Type) {
        visit_type(self, already_visited, ty)
    }

    fn visit_primitive(&mut self, _primitive: &scale_info::TypeDefPrimitive) {}
}

pub fn visit_type<V: Visitor + ?Sized>(
    visitor: &mut V,
    already_visited: &mut HashSet<u32>,
    ty: &Type,
) {
    visitor.visit_type_def(already_visited, &ty.type_def);
}

pub fn visit_type_def<V: Visitor + ?Sized>(
    visitor: &mut V,
    already_visited: &mut HashSet<u32>,
    type_def: &TypeDef,
) {
    match type_def {
        TypeDef::Enumeration(v) => {
            v.iter().for_each(|v| {
                for f in &v.fields {
                    if already_visited.insert(f.ty.borrow().expect_resolved().unique_id()) {
                        visitor.visit_type(already_visited, f.ty.borrow().expect_resolved())
                    }
                }
            });
        }
        TypeDef::Array(a) => {
            if already_visited.insert(a.type_param.borrow().expect_resolved().unique_id()) {
                visitor.visit_type(already_visited, a.type_param.borrow().expect_resolved())
            }
        }
        TypeDef::Composite(c) => {
            c.iter().for_each(|f| {
                if already_visited.insert(f.ty.borrow().expect_resolved().unique_id()) {
                    visitor.visit_type(already_visited, f.ty.borrow().expect_resolved())
                }
            });
        }
        TypeDef::Sequence(s) => {
            if already_visited.insert(s.borrow().expect_resolved().unique_id()) {
                visitor.visit_type(already_visited, s.borrow().expect_resolved())
            }
        }
        TypeDef::Tuple(t) => t.iter().for_each(|t| {
            if already_visited.insert(t.borrow().expect_resolved().unique_id()) {
                visitor.visit_type(already_visited, t.borrow().expect_resolved())
            }
        }),
        TypeDef::Compact(c) => {
            if already_visited.insert(c.borrow().expect_resolved().unique_id()) {
                visitor.visit_type(already_visited, c.borrow().expect_resolved())
            }
        }
        TypeDef::Primitive(p) => visitor.visit_primitive(p),
        TypeDef::BitSequence(b) => {
            if already_visited.insert(b.bit_order_type.borrow().expect_resolved().unique_id()) {
                visitor.visit_type(already_visited, b.bit_order_type.borrow().expect_resolved())
            }

            if already_visited.insert(b.bit_store_type.borrow().expect_resolved().unique_id()) {
                visitor.visit_type(already_visited, b.bit_store_type.borrow().expect_resolved())
            }
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
                .expect_resolved()
                .as_basic_type_ref(),
            call_ty: self.call_ty.borrow().expect_resolved().as_basic_type_ref(),
            signature_ty: self
                .signature_ty
                .borrow()
                .expect_resolved()
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
                .expect_resolved()
                .as_basic_type_ref(),
            additional_signed: self
                .included_in_signed_data
                .borrow()
                .expect_resolved()
                .as_basic_type_ref(),
        }
    }
}
