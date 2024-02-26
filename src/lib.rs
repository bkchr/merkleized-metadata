use crate::types::MerkleTree;
use codec::Encode;
use intermediate_repr::FrameMetadataPrepared;
use types::MetadataDigest;

mod intermediate_repr;
mod types;

pub fn calculate_metadata_digest(prepared: FrameMetadataPrepared) -> MetadataDigest {
    let type_information = prepared.as_type_information();

    let tree_root = MerkleTree::calculate_root(type_information.types.iter().map(|t| t.hash()));

    MetadataDigest::V1 {
        types_tree_root: tree_root,
        extrinsic_metadata_hash: blake3::hash(&type_information.extrinsic_metadata.encode()).into(),
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

        let prepared = FrameMetadataPrepared::prepare(metadata).unwrap();

        let digest = calculate_metadata_digest(prepared);

        assert_eq!(
            "0x172913580ef78c609592809055f9b7fe1d65c40b6dd37eb62570ba22606ae777",
            array_bytes::bytes2hex("0x", &digest.hash())
        );
    }
}
