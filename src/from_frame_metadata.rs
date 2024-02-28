use std::collections::{BTreeMap, BTreeSet};

use frame_metadata::{
    v15::{ExtrinsicMetadata, SignedExtensionMetadata},
    RuntimeMetadata,
};
use scale_info::{
    form::PortableForm, interner::UntrackedSymbol, Field, PortableRegistry, Type, TypeDef,
    TypeDefArray, TypeDefBitSequence, TypeDefPrimitive, Variant,
};

use crate::types;

/// The type information generated from the FRAME metadata.
pub struct TypeInformation {
    /// The extrinsic metadata in the final form.
    pub extrinsic_metadata: types::ExtrinsicMetadata,
    /// The mapping from ID to the final types.
    ///
    /// Only [`Type::Enumeration`](types::Type::Enumeration) will have a vector with length > 1.
    pub types: BTreeMap<u32, Vec<types::Type>>,
}

pub struct FrameMetadataPrepared {
    accessible_types: BTreeSet<u32>,
    frame_type_registry: PortableRegistry,
    extrinsic_metadata: ExtrinsicMetadata<PortableForm>,
}

impl FrameMetadataPrepared {
    pub fn prepare(metadata: RuntimeMetadata) -> Result<Self, String> {
        let metadata = match metadata {
            RuntimeMetadata::V15(m) => m,
            _ => return Err("Only supports metadata V15".into()),
        };

        let frame_type_registry = metadata.types;
        let extrinsic_metadata = metadata.extrinsic;

        let mut accessible_types = Default::default();

        collect_accessible_types(
            extrinsic_metadata.call_ty.id,
            &mut accessible_types,
            &frame_type_registry,
        );
        collect_accessible_types(
            extrinsic_metadata.address_ty.id,
            &mut accessible_types,
            &frame_type_registry,
        );
        collect_accessible_types(
            extrinsic_metadata.signature_ty.id,
            &mut accessible_types,
            &frame_type_registry,
        );

        extrinsic_metadata.signed_extensions.iter().for_each(|se| {
            collect_accessible_types(se.ty.id, &mut accessible_types, &frame_type_registry);
            collect_accessible_types(
                se.additional_signed.id,
                &mut accessible_types,
                &frame_type_registry,
            );
        });

        Ok(Self {
            frame_type_registry,
            accessible_types,
            extrinsic_metadata,
        })
    }

    fn get_type(&self, id: u32) -> &Type<PortableForm> {
        &self.frame_type_registry.types[id as usize].ty
    }

    pub fn as_type_information(&self) -> TypeInformation {
        let mut next_id = 0;
        let frame_id_to_id = self
            .accessible_types
            .iter()
            .filter_map(|id| {
                self.get_type(*id).is_basic_type().then(|| {
                    let new_id = next_id;
                    next_id += 1;
                    (*id, new_id)
                })
            })
            .collect::<BTreeMap<u32, u32>>();

        let type_context = TypeContext {
            frame_id_to_id: &frame_id_to_id,
            frame_type_registry: &self.frame_type_registry,
        };

        let extrinsic_metadata = self.extrinsic_metadata.as_basic_type(type_context);
        let types = frame_id_to_id
            .iter()
            .map(|(frame_id, id)| (*id, self.get_type(*frame_id).as_basic_type(type_context)))
            .collect::<BTreeMap<_, _>>();

        TypeInformation {
            extrinsic_metadata,
            types,
        }
    }
}

fn collect_accessible_types(
    ty_id: u32,
    accessible_types: &mut BTreeSet<u32>,
    registry: &PortableRegistry,
) {
    if !accessible_types.insert(ty_id) {
        return;
    }

    let ty = &registry.types[ty_id as usize].ty;

    match &ty.type_def {
        TypeDef::Composite(c) => c
            .fields
            .iter()
            .for_each(|f| collect_accessible_types(f.ty.id, accessible_types, registry)),
        TypeDef::Variant(v) => v.variants.iter().for_each(|v| {
            v.fields
                .iter()
                .for_each(|f| collect_accessible_types(f.ty.id, accessible_types, registry))
        }),
        TypeDef::Sequence(s) => {
            collect_accessible_types(s.type_param.id, accessible_types, registry)
        }
        TypeDef::Array(a) => collect_accessible_types(a.type_param.id, accessible_types, registry),
        TypeDef::Tuple(t) => t
            .fields
            .iter()
            .for_each(|t| collect_accessible_types(t.id, accessible_types, registry)),
        // Primitive types are not individual types in the final type information.
        TypeDef::Primitive(_) => {}
        // `Compact` is converted to a primitive like type and thus, the inner type is not required.
        TypeDef::Compact(_) => {}
        // The order and store types are also not required.
        TypeDef::BitSequence(_) => {}
    };
}

#[derive(Clone, Copy)]
struct TypeContext<'a> {
    frame_type_registry: &'a PortableRegistry,
    frame_id_to_id: &'a BTreeMap<u32, u32>,
}

impl<'a> TypeContext<'a> {
    fn get_type(&self, id: u32) -> &Type<PortableForm> {
        &self.frame_type_registry.types[id as usize].ty
    }
}

trait AsBasicTypeRef {
    fn as_basic_type_ref(&self, context: TypeContext<'_>) -> types::TypeRef;
}

trait AsBasicType {
    type BasicType;

    fn as_basic_type(&self, context: TypeContext<'_>) -> Self::BasicType;
}

trait IsBasicType {
    fn is_basic_type(&self) -> bool;
}

impl<T> AsBasicTypeRef for UntrackedSymbol<T> {
    fn as_basic_type_ref(&self, context: TypeContext<'_>) -> types::TypeRef {
        let frame_type = context.get_type(self.id);

        let mut collector = CollectPrimitives::default();
        collector.visit_type(context, frame_type);

        match &frame_type.type_def {
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
                    types::TypeRef::Void
                }
            }
            TypeDef::Variant(v) if v.variants.is_empty() => types::TypeRef::Void,
            TypeDef::Composite(c) if c.fields.is_empty() => types::TypeRef::Void,
            TypeDef::Tuple(t) if t.fields.is_empty() => types::TypeRef::Void,
            _ => types::TypeRef::ById(context.frame_id_to_id.get(&self.id).unwrap().into()),
        }
    }
}

impl AsBasicType for Type<PortableForm> {
    type BasicType = Vec<types::Type>;

    fn as_basic_type(&self, context: TypeContext) -> Self::BasicType {
        let path = self
            .path
            .segments
            .iter()
            .map(|s| AsRef::<str>::as_ref(s).to_string())
            .collect::<Vec<_>>();

        let type_def = match &self.type_def {
            TypeDef::Compact(_) | TypeDef::Primitive(_) => return Vec::new(),
            TypeDef::Composite(c) if c.fields.is_empty() => return Vec::new(),
            TypeDef::Variant(v) if v.variants.is_empty() => return Vec::new(),
            TypeDef::Tuple(t) if t.fields.is_empty() => return Vec::new(),
            TypeDef::Variant(v) => {
                let mut variants = v.variants.clone();
                variants.sort_by_key(|v| v.index);

                return variants
                    .iter()
                    .map(|v| types::Type {
                        path: path.clone(),
                        type_def: types::TypeDef::Enumeration(v.as_basic_type(context)),
                    })
                    .collect::<Vec<_>>();
            }
            TypeDef::Array(a) => types::TypeDef::Array(a.as_basic_type(context)),
            TypeDef::Composite(c) => types::TypeDef::Composite(
                c.fields.iter().map(|f| f.as_basic_type(context)).collect(),
            ),
            TypeDef::Sequence(s) => {
                types::TypeDef::Sequence(s.type_param.as_basic_type_ref(context))
            }
            TypeDef::Tuple(t) => types::TypeDef::Tuple(
                t.fields
                    .iter()
                    .map(|t| t.as_basic_type_ref(context))
                    .collect(),
            ),
            TypeDef::BitSequence(b) => types::TypeDef::BitSequence(b.as_basic_type(context)),
        };

        vec![types::Type { path, type_def }]
    }
}

impl IsBasicType for Type<PortableForm> {
    fn is_basic_type(&self) -> bool {
        match &self.type_def {
            TypeDef::Compact(_) | TypeDef::Primitive(_) => false,
            TypeDef::Variant(v) if v.variants.is_empty() => false,
            TypeDef::Composite(c) if c.fields.is_empty() => false,
            TypeDef::Tuple(t) if t.fields.is_empty() => false,
            _ => true,
        }
    }
}

impl AsBasicType for Field<PortableForm> {
    type BasicType = types::Field;

    fn as_basic_type(&self, context: TypeContext) -> Self::BasicType {
        types::Field {
            name: self
                .name
                .as_ref()
                .map(|n| AsRef::<str>::as_ref(n).to_string()),
            ty: self.ty.as_basic_type_ref(context),
            type_name: self
                .type_name
                .as_ref()
                .map(|n| AsRef::<str>::as_ref(n).to_string()),
        }
    }
}

impl AsBasicType for Variant<PortableForm> {
    type BasicType = types::EnumerationVariant;

    fn as_basic_type(&self, context: TypeContext) -> types::EnumerationVariant {
        types::EnumerationVariant {
            name: AsRef::<str>::as_ref(&self.name).to_string(),
            fields: self
                .fields
                .iter()
                .map(|f| f.as_basic_type(context))
                .collect(),
            index: self.index,
        }
    }
}

impl AsBasicType for TypeDefArray<PortableForm> {
    type BasicType = types::TypeDefArray;

    fn as_basic_type(&self, context: TypeContext) -> types::TypeDefArray {
        types::TypeDefArray {
            len: self.len,
            type_param: self.type_param.as_basic_type_ref(context),
        }
    }
}

impl AsBasicType for TypeDefBitSequence<PortableForm> {
    type BasicType = types::TypeDefBitSequence;

    fn as_basic_type(&self, context: TypeContext) -> types::TypeDefBitSequence {
        let mut collector = CollectPrimitives::default();
        collector.visit_type(context, context.get_type(self.bit_store_type.id));

        let num_bytes = if collector.found.len() == 1 {
            match &collector.found[0] {
                TypeDefPrimitive::U8 => 1,
                TypeDefPrimitive::U16 => 2,
                TypeDefPrimitive::U32 => 4,
                TypeDefPrimitive::U64 => 8,
                p => {
                    panic!("Invalid primitive type {p:?} as store type for `BitSequence`: {self:?}")
                }
            }
        } else {
            panic!("Only expected to find `1` primitive type as store type for `BitSequence`: {self:?}")
        };

        types::TypeDefBitSequence {
            num_bytes,
            least_significant_bit_first: context
                .get_type(self.bit_order_type.id)
                .path
                .segments
                .iter()
                .find(|p| *p == "Lsb0" || *p == "Msb0")
                .expect("`bit_order_type` is either `Lsb0` or `Msb0`")
                == "Lsb0",
        }
    }
}

#[derive(Default)]
struct CollectPrimitives {
    found: Vec<TypeDefPrimitive>,
    already_visited: BTreeSet<u32>,
}

impl Visitor for CollectPrimitives {
    fn visit_primitive(&mut self, primitive: &scale_info::TypeDefPrimitive) {
        self.found.push(primitive.clone());
    }

    fn already_visited(&mut self, id: u32) -> bool {
        !self.already_visited.insert(id)
    }
}

trait Visitor {
    fn visit_type_def(&mut self, context: TypeContext, type_def: &TypeDef<PortableForm>) {
        visit_type_def(self, context, type_def)
    }

    fn visit_type(&mut self, context: TypeContext, ty: &Type<PortableForm>) {
        visit_type(self, context, ty)
    }

    fn visit_primitive(&mut self, _primitive: &scale_info::TypeDefPrimitive) {}

    fn already_visited(&mut self, id: u32) -> bool;
}

fn visit_type<V: Visitor + ?Sized>(visitor: &mut V, context: TypeContext, ty: &Type<PortableForm>) {
    visitor.visit_type_def(context, &ty.type_def);
}

fn visit_type_def<V: Visitor + ?Sized>(
    visitor: &mut V,
    context: TypeContext,
    type_def: &TypeDef<PortableForm>,
) {
    match type_def {
        TypeDef::Variant(v) => {
            v.variants.iter().for_each(|v| {
                for f in &v.fields {
                    if !visitor.already_visited(f.ty.id) {
                        visitor.visit_type(context, context.get_type(f.ty.id))
                    }
                }
            });
        }
        TypeDef::Array(a) => {
            if !visitor.already_visited(a.type_param.id) {
                visitor.visit_type(context, context.get_type(a.type_param.id))
            }
        }
        TypeDef::Composite(c) => {
            c.fields.iter().for_each(|f| {
                if !visitor.already_visited(f.ty.id) {
                    visitor.visit_type(context, context.get_type(f.ty.id))
                }
            });
        }
        TypeDef::Sequence(s) => {
            if !visitor.already_visited(s.type_param.id) {
                visitor.visit_type(context, context.get_type(s.type_param.id))
            }
        }
        TypeDef::Tuple(t) => t.fields.iter().for_each(|t| {
            if !visitor.already_visited(t.id) {
                visitor.visit_type(context, context.get_type(t.id))
            }
        }),
        TypeDef::Compact(c) => {
            if !visitor.already_visited(c.type_param.id) {
                visitor.visit_type(context, context.get_type(c.type_param.id))
            }
        }
        TypeDef::Primitive(p) => visitor.visit_primitive(p),
        TypeDef::BitSequence(b) => {
            if !visitor.already_visited(b.bit_order_type.id) {
                visitor.visit_type(context, context.get_type(b.bit_order_type.id))
            }

            if !visitor.already_visited(b.bit_store_type.id) {
                visitor.visit_type(context, context.get_type(b.bit_store_type.id))
            }
        }
    }
}

impl AsBasicType for frame_metadata::v15::ExtrinsicMetadata<PortableForm> {
    type BasicType = types::ExtrinsicMetadata;

    fn as_basic_type(&self, context: TypeContext) -> types::ExtrinsicMetadata {
        types::ExtrinsicMetadata {
            version: self.version,
            address_ty: self.address_ty.as_basic_type_ref(context),
            call_ty: self.call_ty.as_basic_type_ref(context),
            signature_ty: self.signature_ty.as_basic_type_ref(context),
            signed_extensions: self
                .signed_extensions
                .iter()
                .map(|se| se.as_basic_type(context))
                .collect(),
        }
    }
}

impl AsBasicType for SignedExtensionMetadata<PortableForm> {
    type BasicType = types::SignedExtensionMetadata;

    fn as_basic_type(&self, context: TypeContext) -> types::SignedExtensionMetadata {
        types::SignedExtensionMetadata {
            identifier: AsRef::<str>::as_ref(&self.identifier).to_string(),
            included_in_extrinsic: self.ty.as_basic_type_ref(context),
            included_in_signed_data: self.additional_signed.as_basic_type_ref(context),
        }
    }
}
