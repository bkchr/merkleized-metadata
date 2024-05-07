use codec::Decode;
use frame_metadata::RuntimeMetadataPrefixed;
use honggfuzz::fuzz;
use merkleized_metadata::{generate_proof_for_extrinsic, verify_proof};
use std::fs;

fn main() {
    loop {
        fuzz!(|data: (&[u8], &[u8], Option<Vec<u8>>)| {
            let (metadata, extrinsic, additional_signed) = data;

            let Ok(metadata) = RuntimeMetadataPrefixed::decode(&mut &metadata[..]) else {
                return;
            };

            let Ok(proof) =
                generate_proof_for_extrinsic(extrinsic, additional_signed.as_deref(), &metadata.1)
            else {
                // Invalid `extrinsic`, let's skip it.
                return;
            };

            println!("Generated valid extrinsic");

            verify_proof(extrinsic, additional_signed.as_deref(), &metadata.1, &proof).unwrap()
        });
    }
}
