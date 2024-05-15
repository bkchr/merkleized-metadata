use clap::{Parser, ValueEnum};
use codec::Decode;
use frame_metadata::{RuntimeMetadata, RuntimeMetadataPrefixed};
use honggfuzz::fuzz;
use merkleized_metadata::{generate_proof_for_extrinsic, verify_proof};
use std::fs;

#[derive(ValueEnum, Clone, Copy)]
enum Method {
    /// Fuzz the metadata as well.
    FuzzMetadata,
    /// Load the metadata and only fuzz the extrinsic.
    LoadMetadata,
}

#[derive(Parser)]
struct Cli {
    #[arg(long, value_enum, default_value_t = Method::LoadMetadata)]
    method: Method,
}

fn main() {
    match Cli::parse().method {
        Method::FuzzMetadata => {
            fuzz_metadata_and_extrinsic();
        }
        Method::LoadMetadata => {
            fuzz_extrinsic_only();
        }
    }
}

/// Fuzz the metadata and the extrinsic.
fn fuzz_metadata_and_extrinsic() {
    loop {
        fuzz!(|data: (&[u8], &[u8], Option<Vec<u8>>)| {
            let (metadata, extrinsic, additional_signed) = data;

            let Ok(metadata) = RuntimeMetadataPrefixed::decode(&mut &metadata[..]) else {
                return;
            };

            generate_proof_and_verify(&metadata.1, extrinsic, additional_signed);
        });
    }
}

/// Fuzz the extrinsic only.
fn fuzz_extrinsic_only() {
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

            generate_proof_and_verify(&metadata, extrinsic, additional_signed);
        });
    }
}

fn generate_proof_and_verify(
    metadata: &RuntimeMetadata,
    extrinsic: &[u8],
    additional_signed: Option<Vec<u8>>,
) {
    let Ok(proof) =
        generate_proof_for_extrinsic(extrinsic, additional_signed.as_deref(), &metadata)
    else {
        // Invalid `extrinsic`, let's skip it.
        return;
    };

    println!("Generated valid extrinsic");

    verify_proof(extrinsic, additional_signed.as_deref(), &metadata, &proof).unwrap()
}
