use crate::types::MerkleTree;
use codec::Encode;
use intermediate_repr::Intermediate;
use types::MetadataDigest;

pub mod frame_metadata;
mod intermediate_repr;
mod types;

pub fn calculate_metadata_digest(intermediate: Intermediate) -> MetadataDigest {
    let mut types = intermediate
        .types
        .iter()
        .filter(|ty| !ty.borrow().expect_resolved().as_basic_type().is_empty())
        .collect::<Vec<_>>();
    types.sort_by_key(|ty| ty.borrow().expect_resolved().unique_id());
    types.iter().enumerate().for_each(|(id, ty)| {
        ty.borrow_mut()
            .expect_resolved_mut()
            .set_unique_id(id as u32)
    });

    let tree_root = MerkleTree::calculate_root(
        types
            .iter()
            .flat_map(|t| t.borrow().expect_resolved().as_basic_type())
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
            "0x5d48d875f4c62cd65045e6ab4ec3e23bb261e5bfcca2e4ec4cef4f27683fcaa1",
            array_bytes::bytes2hex("0x", &digest.hash())
        );
    }
}
