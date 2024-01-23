use std::collections::VecDeque;

use crate::types::TreeElement;
use codec::Encode;
use intermediate_repr::Intermediate;
use types::{MetadataDigest, MetadataHash};

pub mod frame_metadata;
mod intermediate_repr;
mod types;

pub fn calculate_metadata_digest(mut intermediate: Intermediate) -> MetadataDigest {
    intermediate.types.sort_by_key(|t| *t.unique_id.borrow());

    intermediate
        .types
        .iter()
        .enumerate()
        .for_each(|(index, t)| {
            if let intermediate_repr::TypeDef::Variant(v) = &t.type_def {
                intermediate
                    .types
                    .iter()
                    .skip(index + 1)
                    .for_each(|t| *t.unique_id.borrow_mut() += v.len() as u32);
            }
        });

    let mut leaves = intermediate
        .types
        .iter()
        .map(|t| t.as_basic_types())
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

    let metadata_hash = MetadataHash {
        types_tree_root: tree_root,
        extrinsic_metadata_hash: blake3::hash(
            &intermediate.extrinsic_metadata.as_basic_type().encode(),
        )
        .into(),
    };

    MetadataDigest {
        version: 1,
        metadata_hash: metadata_hash.hash(),
    }
}
