use codec::Decode;
use frame_metadata::RuntimeMetadataPrefixed;
use honggfuzz::fuzz;
use merkleized_metadata::{generate_proof_for_extrinsic, verify_proof};
use std::fs;

fn main() {
    let metadata = String::from_utf8(
        fs::read(format!(
            "{}/../fixtures/rococo_metadata_v15",
            env!("CARGO_MANIFEST_DIR")
        ))
        .unwrap(),
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

    loop {
        fuzz!(|data: (&[u8], Option<Vec<u8>>)| {
            let (extrinsic, additional_signed) = data;

            let Ok(proof) =
                generate_proof_for_extrinsic(extrinsic, additional_signed.as_deref(), &metadata)
            else {
                // Invalid `extrinsic`, let's skip it.
                return;
            };

            println!("Generated valid extrinsic");

            verify_proof(extrinsic, additional_signed.as_deref(), &metadata, &proof).unwrap()
        });
    }
}
