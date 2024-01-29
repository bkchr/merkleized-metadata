use crate::types::MerkleTree;
use codec::Encode;
use intermediate_repr::Intermediate;
use types::MetadataDigest;

pub mod frame_metadata;
mod intermediate_repr;
mod types;

pub fn calculate_metadata_digest(mut intermediate: Intermediate) -> MetadataDigest {
    intermediate
        .types
        .sort_by_key(|t| *t.borrow().as_ref().unwrap().unique_id.borrow());

    let tree_root = MerkleTree::calculate_root(
        intermediate
            .types
            .iter()
            .map(|t| t.borrow().as_ref().unwrap().as_basic_type().hash()),
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
            "0x1c9360ef5514a5023efc2c8fa70f79823a4de08906d8aea785d030c9dd4c2bb3",
            array_bytes::bytes2hex("0x", &digest.hash())
        );
    }
}
