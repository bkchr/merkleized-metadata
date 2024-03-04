use codec::Compact;
use extrinsic_decoder::decode_extrinsic_and_collect_type_ids;
use frame_metadata::RuntimeMetadata;
use from_frame_metadata::FrameMetadataPrepared;
use merkle_tree::{MerkleTree, MerkleTreeNode, Proof};
use types::{Hash, MetadataDigest};

mod extrinsic_decoder;
mod from_frame_metadata;
mod merkle_tree;
mod types;

/// Extra information that is required to generate the [`MetadataDigest`].
#[derive(Debug, Clone)]
pub struct ExtraInfo {
    pub spec_version: u32,
    pub spec_name: String,
    pub base58_prefix: u16,
    pub decimals: u8,
    pub token_symbol: String,
}

/// Generate the [`MetadataDigest`].
pub fn generate_metadata_digest(
    metadata: RuntimeMetadata,
    extra_info: ExtraInfo,
) -> Result<MetadataDigest, String> {
    let prepared = FrameMetadataPrepared::prepare(metadata)?;

    let type_information = prepared.as_type_information();

    let tree_root = MerkleTree::new(
        type_information
            .types
            .into_iter()
    )
    .root();

    Ok(MetadataDigest::V1 {
        types_tree_root: tree_root,
        extrinsic_metadata_hash: type_information.extrinsic_metadata.hash(),
        spec_version: extra_info.spec_version,
        spec_name: extra_info.spec_name,
        base58_prefix: extra_info.base58_prefix,
        decimals: extra_info.decimals,
        token_symbol: extra_info.token_symbol,
    })
}

pub fn generate_proof_for_extrinsic(
    extrinsic: &[u8],
    additional_signed: Option<&[u8]>,
    metadata: RuntimeMetadata,
) -> Result<Proof, String> {
    let prepared = FrameMetadataPrepared::prepare(metadata)?;

    let accessed_types = decode_extrinsic_and_collect_type_ids(
        extrinsic,
        additional_signed,
        &prepared.as_type_information(),
    )?;

    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::frame_metadata::RuntimeMetadataPrefixed;
    use codec::Decode;
    use std::fs;

    const FIXTURES: &[(&str, &str)] = &[
        (
            "rococo_metadata_v15",
            "0xa05d63fe8bc95c3b9e2aee8ecebcbad63ff1df4eff7c43d9f010baf79e607bf7",
        ),
        (
            "polkadot_metadata_v15",
            "0x3fcef0c2652a3872fbbc66088045274f861b6ca32b41f539ce10d2f56f005164",
        ),
        (
            "kusama_metadata_v15",
            "0x11ac2d8295859989d83270650809af3574e2be887f496aaadbd5a08c9027e648",
        ),
        (
            "acala_metadata_v15",
            "0x31fc11b76744a8bc32517bf0a7840b55b838744d709e85e086dfa2593e43c52f",
        ),
        (
            "moonbeam_metadata_v15",
            "0x2436f5a75162862f6026643e82b63311eb2d6f20ef64e243d8bfd037e09aadfe",
        ),
        (
            "hydradx_metadata_v15",
            "0xc22e4363353142b40d66faa738170b42a230457c521fffae857c901cf9137c12",
        ),
    ];

    #[test]
    fn calculate_metadata_digest_works() {
        let extra_info = ExtraInfo {
            spec_version: 1,
            spec_name: "nice".into(),
            base58_prefix: 1,
            decimals: 1,
            token_symbol: "lol".into(),
        };

        for (fixture, expected_hash) in FIXTURES {
            println!("Processing: {fixture}");

            let metadata = String::from_utf8(
                fs::read(format!("{}/fixtures/{fixture}", env!("CARGO_MANIFEST_DIR"))).unwrap(),
            )
            .unwrap();

            let metadata = Option::<Vec<u8>>::decode(
                &mut &array_bytes::hex2bytes(metadata.strip_suffix("\n").unwrap()).unwrap()[..],
            )
            .unwrap()
            .unwrap();

            let metadata = RuntimeMetadataPrefixed::decode(&mut &metadata[..])
                .unwrap()
                .1;

            let digest = generate_metadata_digest(metadata, extra_info.clone()).unwrap();

            assert_eq!(*expected_hash, array_bytes::bytes2hex("0x", &digest.hash()));
        }
    }
}
