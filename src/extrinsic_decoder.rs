use scale_decode::{ext::scale_type_resolver::ResolvedTypeVisitor, Field, TypeResolver};

use crate::{from_frame_metadata::TypeInformation, types::{TypeDef, TypeRef}};

impl TypeResolver for TypeInformation {
    type TypeId = TypeRef;

    type Error = String;

    fn resolve_type<'this, V: ResolvedTypeVisitor<'this, TypeId = TypeRef>>(
        &'this self,
        type_id: &TypeRef,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        let ty = self.types.get(*type_id as usize).ok_or_else(|| format!("Unknown type id {type_id}"))?;

        match ty.type_def {
            TypeDef::Array(a) => Ok(visitor.visit_array(&a.type_param, a.len as usize)),
            TypeDef::Composite(c) => Ok(visitor.visit_composite(c.iter().map(|f| Field { name: f.name.as_deref(), id: &f.ty }))),
        }
    }
}

pub fn decode_extrinsic_and_collect_type_ids(
    extrinsic: &[u8],
    type_information: &TypeInformation,
) -> Vec<u32> {
    todo!()
}
