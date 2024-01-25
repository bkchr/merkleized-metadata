use std::collections::VecDeque;

use crate::types::TreeElement;
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

    intermediate
        .types
        .iter()
        .enumerate()
        .for_each(|(index, t)| {
            if let intermediate_repr::TypeDef::Variant(v) = &t.borrow().as_ref().unwrap().type_def {
                intermediate.types.iter().skip(index + 1).for_each(|t| {
                    *t.borrow().as_ref().unwrap().unique_id.borrow_mut() += v.len() as u32
                });
            }
        });

    let mut leaves = intermediate
        .types
        .iter()
        .map(|t| t.borrow().as_ref().unwrap().as_basic_types())
        .flatten()
        .map(|t| TreeElement::Leaf(blake3::hash(&t.encode()).into()))
        .collect::<VecDeque<_>>();

    let mut nodes = VecDeque::new();

    while leaves.len() > 1 {
        let left = leaves.pop_front().unwrap();
        let right = leaves.pop_front().unwrap();

        nodes.push_back(TreeElement::Node {
            left: left.hash(),
            right: right.hash(),
        });
    }

    if let Some(left_over) = leaves.pop_front() {
        if let Some(last_node) = nodes.back_mut() {
            *last_node = TreeElement::Node {
                left: last_node.hash(),
                right: left_over.hash(),
            }
        } else {
            nodes.push_back(left_over);
        }
    }

    while nodes.len() > 1 {
        let left = nodes.pop_front().unwrap();
        let right = nodes.pop_front().unwrap();

        nodes.push_back(TreeElement::Node {
            left: left.hash(),
            right: right.hash(),
        });
    }

    let tree_root = nodes.back().unwrap().hash();

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
            "0xfc4befe0d3e2035b99656aa5c2f3680671e2bcbeebffbaa79de36ea9ba61d683",
            array_bytes::bytes2hex("0x", &digest.hash())
        );
    }
}
