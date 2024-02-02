use crate::{
    intermediate_repr::{visit_type, Visitor},
    types::MerkleTree,
};
use codec::Encode;
use intermediate_repr::Intermediate;
use std::collections::{HashMap, HashSet};
use types::MetadataDigest;

pub mod frame_metadata;
mod intermediate_repr;
mod types;

#[derive(Default)]
struct ObserveTypeOrder {
    types: Vec<u32>,
}

impl Visitor for ObserveTypeOrder {
    fn visit_type(&mut self, already_visited: &mut HashSet<u32>, ty: &intermediate_repr::Type) {
        let unique_id = ty.unique_id();
        if !self.types.iter().any(|t| *t == unique_id) {
            self.types.push(ty.unique_id())
        }

        visit_type(self, already_visited, ty)
    }
}

pub fn calculate_metadata_digest(intermediate: Intermediate) -> MetadataDigest {
    let mut already_visited = Default::default();
    let mut visitor = ObserveTypeOrder::default();
    visitor.visit_type(
        &mut already_visited,
        intermediate
            .extrinsic_metadata
            .call_ty
            .borrow()
            .expect_resolved(),
    );
    visitor.visit_type(
        &mut already_visited,
        intermediate
            .extrinsic_metadata
            .address_ty
            .borrow()
            .expect_resolved(),
    );
    visitor.visit_type(
        &mut already_visited,
        intermediate
            .extrinsic_metadata
            .signature_ty
            .borrow()
            .expect_resolved(),
    );
    intermediate
        .extrinsic_metadata
        .signed_extensions
        .iter()
        .for_each(|se| {
            visitor.visit_type(
                &mut already_visited,
                se.included_in_extrinsic.borrow().expect_resolved(),
            );
            visitor.visit_type(
                &mut already_visited,
                se.included_in_signed_data.borrow().expect_resolved(),
            );
        });

    let id_to_types = intermediate
        .types
        .into_iter()
        .map(|t| {
            let unique_id = t.borrow().expect_resolved().unique_id();
            (unique_id, t)
        })
        .collect::<HashMap<_, _>>();
    let final_types = visitor
        .types
        .iter()
        .filter_map(|tid| {
            id_to_types.get(tid).and_then(|t| {
                t.borrow()
                    .expect_resolved()
                    .as_basic_type()
                    .map(|_| t.clone())
            })
        })
        .enumerate()
        .map(|(id, t)| {
            t.borrow_mut()
                .expect_resolved_mut()
                .set_unique_id(id as u32);
            t
        })
        .collect::<Vec<_>>();

    assert_eq!(
        final_types.iter().fold(0, |p, c| {
            let unique_id = c.borrow().expect_resolved().unique_id();

            if p + 1 == unique_id || p == 0 && unique_id == 0 {
                c.borrow().expect_resolved().unique_id()
            } else {
                panic!("Not sorted")
            }
        }),
        final_types
            .last()
            .unwrap()
            .borrow()
            .expect_resolved()
            .unique_id()
    );
    panic!("Before {}, after {}, lol {}", id_to_types.len(), final_types.len(), visitor.types.len());

    let tree_root = MerkleTree::calculate_root(
        final_types
            .iter()
            .filter_map(|t| t.borrow().expect_resolved().as_basic_type())
            .map(|t| t.hash()),
    );

    MetadataDigest::V1 {
        types_tree_root: tree_root,
        extrinsic_metadata_hash: blake3::hash(
            &intermediate.extrinsic_metadata.as_basic_type().encode(),
        )
        .into(),
        spec_version: 1,
        spec_name: "nice".into(),
        base58_prefix: 1,
        decimals: 1,
        token_symbol: "lol".into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::frame_metadata::RuntimeMetadataPrefixed;
    use codec::Decode;

    #[test]
    fn calculate_metadata_digest_works() {
        let metadata =
            String::from_utf8(include_bytes!("../fixtures/rococo_metadata_v15").to_vec()).unwrap();

        let metadata = Option::<Vec<u8>>::decode(
            &mut &array_bytes::hex2bytes(metadata.strip_suffix("\n").unwrap()).unwrap()[..],
        )
        .unwrap()
        .unwrap();

        let metadata = RuntimeMetadataPrefixed::decode(&mut &metadata[..])
            .unwrap()
            .1;

        let digest = calculate_metadata_digest(frame_metadata::into_intermediate(metadata));

        assert_eq!(
            "0x29edfbe83b9f9120402b28e9beb2a9449086118628d4735d83daa25353e171f0",
            array_bytes::bytes2hex("0x", &digest.hash())
        );
    }
}
