use clap::{Parser, ValueEnum};
use codec::{Decode, Encode};
use frame_metadata::{RuntimeMetadata, RuntimeMetadataPrefixed};
use honggfuzz::fuzz;
use merkleized_metadata::{
    generate_metadata_digest, generate_proof_for_extrinsic,
    types::{Hash, MetadataDigest, Type},
    verify_proof, ExtraInfo, Proof,
};
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

    verify_proof(extrinsic, additional_signed.as_deref(), &metadata, &proof).unwrap();

    let proof_root = proof_root_hash(&proof);

    let metadata_root = match generate_metadata_digest(
        &metadata,
        ExtraInfo {
            spec_version: 1,
            spec_name: "fuzz".into(),
            base58_prefix: 42,
            decimals: 10,
            token_symbol: "fuzz".into(),
        },
    )
    .unwrap()
    {
        MetadataDigest::V1 {
            types_tree_root, ..
        } => types_tree_root,
        _ => panic!("Invalid digest"),
    };

    assert_eq!(metadata_root, proof_root);
}

/// Calculates the root hash of the given `proof`.
fn proof_root_hash(proof: &Proof) -> Hash {
    fn is_descendent(l: u32, r: u32) -> bool {
        // If the index is `0`, it is the root
        if l == 0 {
            return true;
        }

        // If the index is greater, it can not be a descendent
        if l > r {
            return false;
        }

        fn level(l: u32) -> u32 {
            (l + 1).ilog2() as _
        }

        let level0 = level(l);
        let level1 = level(r);

        // Check if applying X times the parent function leads to
        // the expected `index`. X is the level difference
        l + 1 == (r + 1) >> (level1 - level0)
    }

    //// Return the index of the right child.
    fn right_child(n: u32) -> u32 {
        n * 2 + 2
    }

    //// Return the index of the left child.
    fn left_child(n: u32) -> u32 {
        n * 2 + 1
    }

    fn get_hash(
        leaf_indices: &mut &[u32],
        leaves: &mut &[Type],
        nodes: &mut &[Hash],
        node_index: u32,
    ) -> Hash {
        let is_descendent = if leaf_indices.is_empty() {
            false
        } else {
            let current_leaf = leaf_indices[0];

            if node_index == current_leaf {
                let hash = blake3::hash(&leaves[0].encode());

                *leaves = &leaves[1..];
                *leaf_indices = &leaf_indices[1..];
                return hash.into();
            }

            is_descendent(node_index, current_leaf)
        };

        if !is_descendent {
            let res = nodes[0];
            *nodes = &nodes[1..];
            return res;
        }

        let left_child = left_child(node_index);
        let left = get_hash(leaf_indices, leaves, nodes, left_child);

        let right_child = right_child(node_index);
        let right = get_hash(leaf_indices, leaves, nodes, right_child);

        blake3::hash(&(left, right).encode()).into()
    }

    get_hash(
        &mut &proof.leaf_indices[..],
        &mut &proof.leaves[..],
        &mut &proof.nodes[..],
        0,
    )
}
