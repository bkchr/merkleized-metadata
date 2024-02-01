use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use crate::intermediate_repr::{
    ExtrinsicMetadata, Field, Intermediate, SignedExtensionMetadata, Type, TypeDef, TypeDefArray,
    TypeDefBitSequence, TypeParameter, TypeRef, TypeRefInner, Variant,
};
use frame_metadata::RuntimeMetadata;
use scale_info::{form::PortableForm, PortableRegistry};

fn convert_field(
    field: &scale_info::Field<PortableForm>,
    known_types: &mut BTreeMap<u32, TypeRef>,
    registry: &PortableRegistry,
) -> Field {
    Field {
        name: field
            .name
            .as_ref()
            .map(|n| AsRef::<str>::as_ref(n).to_string()),
        ty: resolve_type(field.ty.id, known_types, registry),
        type_name: field
            .type_name
            .as_ref()
            .map(|n| AsRef::<str>::as_ref(n).to_string()),
    }
}

fn resolve_type(
    ty_id: u32,
    known_types: &mut BTreeMap<u32, TypeRef>,
    registry: &PortableRegistry,
) -> TypeRef {
    let ty = &registry.types[ty_id as usize];

    if let Some(ty) = known_types.get(&ty.id) {
        return ty.clone();
    }

    known_types.insert(ty_id, Rc::new(RefCell::new(TypeRefInner::Unresolved)));

    let path = ty
        .ty
        .path
        .segments
        .iter()
        .map(|s| AsRef::<str>::as_ref(s).to_string())
        .collect::<Vec<_>>();

    let type_params = ty
        .ty
        .type_params
        .iter()
        .map(|p| {
            let name = AsRef::<str>::as_ref(&p.name).to_string();
            let ty = p.ty.as_ref().map(|t| {
                if let Some(t) = known_types.get(&t.id) {
                    return t.clone();
                }

                resolve_type(t.id, known_types, registry)
            });

            TypeParameter { name, ty }
        })
        .collect::<Vec<_>>();

    let type_def = match &ty.ty.type_def {
        scale_info::TypeDef::Composite(c) => TypeDef::Composite(
            c.fields
                .iter()
                .map(|f| convert_field(f, known_types, registry))
                .collect::<Vec<_>>(),
        ),
        scale_info::TypeDef::Variant(v) => TypeDef::Enumeration(
            v.variants
                .iter()
                .map(|v| Variant {
                    name: AsRef::<str>::as_ref(&v.name).to_string(),
                    fields: v
                        .fields
                        .iter()
                        .map(|f| convert_field(&f, known_types, registry))
                        .collect::<Vec<_>>(),
                    index: v.index,
                })
                .collect::<Vec<_>>(),
        ),
        scale_info::TypeDef::Sequence(s) => {
            TypeDef::Sequence(resolve_type(s.type_param.id, known_types, registry))
        }
        scale_info::TypeDef::Array(a) => TypeDef::Array(TypeDefArray {
            len: a.len,
            type_param: resolve_type(a.type_param.id, known_types, registry),
        }),
        scale_info::TypeDef::Tuple(t) => TypeDef::Tuple(
            t.fields
                .iter()
                .map(|t| resolve_type(t.id, known_types, registry))
                .collect::<Vec<_>>(),
        ),
        scale_info::TypeDef::Primitive(p) => TypeDef::Primitive(p.clone()),
        scale_info::TypeDef::Compact(c) => {
            TypeDef::Compact(resolve_type(c.type_param.id, known_types, registry))
        }
        scale_info::TypeDef::BitSequence(b) => TypeDef::BitSequence(TypeDefBitSequence {
            bit_store_type: resolve_type(b.bit_store_type.id, known_types, registry),
            bit_order_type: resolve_type(b.bit_order_type.id, known_types, registry),
        }),
    };

    let ty = known_types.get(&ty_id).unwrap().clone();
    *ty.borrow_mut().resolved(Type {
        path,
        type_params,
        type_def,
        unique_id: ty_id.into(),
    });

    ty
}

pub fn into_intermediate(metadata: RuntimeMetadata) -> Intermediate {
    let registry = match &metadata {
        RuntimeMetadata::V15(m) => &m.types,
        _ => unimplemented!(),
    };

    let mut known_types = BTreeMap::<u32, TypeRef>::default();

    let ext_meta = match &metadata {
        RuntimeMetadata::V15(m) => &m.extrinsic,
        _ => unimplemented!(),
    };

    let extrinsic_metadata = ExtrinsicMetadata {
        call_ty: resolve_type(ext_meta.call_ty.id, &mut known_types, registry),
        address_ty: resolve_type(ext_meta.address_ty.id, &mut known_types, registry),
        signature_ty: resolve_type(ext_meta.signature_ty.id, &mut known_types, registry),
        extra_ty: resolve_type(ext_meta.extra_ty.id, &mut known_types, registry),
        version: ext_meta.version,
        signed_extensions: ext_meta
            .signed_extensions
            .iter()
            .map(|se| SignedExtensionMetadata {
                identifier: AsRef::<str>::as_ref(&se.identifier).to_string(),
                included_in_extrinsic: resolve_type(se.ty.id, &mut known_types, &registry),
                included_in_signed_data: resolve_type(
                    se.additional_signed.id,
                    &mut known_types,
                    &registry,
                ),
            })
            .collect(),
    };

    Intermediate {
        types: known_types.into_values().collect(),
        extrinsic_metadata,
    }
}
