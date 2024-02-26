use codec::Encode;
use from_frame_metadata::FrameMetadataPrepared;
use types::MetadataDigest;

mod from_frame_metadata;
mod types;

pub fn calculate_metadata_digest(prepared: FrameMetadataPrepared) -> MetadataDigest {
    let type_information = prepared.as_type_information();

    let tree_root = types::calculate_root(type_information.types.iter().map(|t| t.hash()));

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

            let prepared = FrameMetadataPrepared::prepare(metadata).unwrap();

            let digest = calculate_metadata_digest(prepared);

            assert_eq!(*expected_hash, array_bytes::bytes2hex("0x", &digest.hash()));
        }
    }
}
