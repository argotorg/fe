use std::fmt;

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ShapeDimension {
    Structure,
    Names,
    Constants,
    Types,
    TraceEvents,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ShapeDigest(String);

impl ShapeDigest {
    pub fn new(hex: impl Into<String>) -> Result<Self, ShapeDigestError> {
        let hex = hex.into();
        if hex.len() != 16
            || !hex
                .bytes()
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
        {
            return Err(ShapeDigestError::InvalidHex);
        }
        Ok(Self(hex))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ShapeDigestError {
    InvalidHex,
}

impl fmt::Display for ShapeDigestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidHex => write!(f, "shape digest must be 16 lowercase hex characters"),
        }
    }
}

impl std::error::Error for ShapeDigestError {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ShapeHashPolicy {
    level: String,
    dimension: ShapeDimension,
}

impl ShapeHashPolicy {
    pub fn new(level: impl Into<String>, dimension: ShapeDimension) -> Result<Self, String> {
        let level = level.into();
        if level.is_empty() {
            return Err("shape hash level must not be empty".to_string());
        }
        Ok(Self { level, dimension })
    }

    pub fn level(&self) -> &str {
        &self.level
    }

    pub const fn dimension(&self) -> ShapeDimension {
        self.dimension
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ShapeGraphEdge<'a> {
    pub source_key: &'a str,
    pub label: &'a str,
    pub target_key: &'a str,
    pub source_digest: &'a ShapeDigest,
    pub target_digest: &'a ShapeDigest,
}

pub fn local_content_digest(
    policy: &ShapeHashPolicy,
    node_kind: &str,
    fields: &[(&str, &str)],
) -> ShapeDigest {
    let mut bytes = Vec::new();
    push_part(&mut bytes, "local");
    push_part(&mut bytes, policy.level());
    push_part(&mut bytes, &format!("{:?}", policy.dimension()));
    push_part(&mut bytes, node_kind);
    for (name, value) in fields {
        push_part(&mut bytes, name);
        push_part(&mut bytes, value);
    }
    digest_bytes(&bytes)
}

pub fn ordered_tree_digest(
    policy: &ShapeHashPolicy,
    local: &ShapeDigest,
    children: &[ShapeDigest],
) -> ShapeDigest {
    let mut bytes = Vec::new();
    push_part(&mut bytes, "tree");
    push_part(&mut bytes, policy.level());
    push_part(&mut bytes, &format!("{:?}", policy.dimension()));
    push_part(&mut bytes, local.as_str());
    for child in children {
        push_part(&mut bytes, child.as_str());
    }
    digest_bytes(&bytes)
}

pub fn graph_edge_digest(policy: &ShapeHashPolicy, edges: &[ShapeGraphEdge<'_>]) -> ShapeDigest {
    let mut sorted_edges = edges.iter().collect::<Vec<_>>();
    sorted_edges.sort_by(|left, right| {
        (
            left.source_key,
            left.label,
            left.target_key,
            left.source_digest.as_str(),
            left.target_digest.as_str(),
        )
            .cmp(&(
                right.source_key,
                right.label,
                right.target_key,
                right.source_digest.as_str(),
                right.target_digest.as_str(),
            ))
    });

    let mut bytes = Vec::new();
    push_part(&mut bytes, "graph_edges");
    push_part(&mut bytes, policy.level());
    push_part(&mut bytes, &format!("{:?}", policy.dimension()));
    for edge in sorted_edges {
        push_part(&mut bytes, edge.source_key);
        push_part(&mut bytes, edge.label);
        push_part(&mut bytes, edge.target_key);
        push_part(&mut bytes, edge.source_digest.as_str());
        push_part(&mut bytes, edge.target_digest.as_str());
    }
    digest_bytes(&bytes)
}

fn push_part(bytes: &mut Vec<u8>, value: &str) {
    bytes.extend_from_slice(&(value.len() as u64).to_le_bytes());
    bytes.extend_from_slice(value.as_bytes());
}

fn digest_bytes(bytes: &[u8]) -> ShapeDigest {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    ShapeDigest(format!("{hash:016x}"))
}

#[cfg(test)]
mod tests {
    use super::{
        ShapeDimension, ShapeGraphEdge, ShapeHashPolicy, graph_edge_digest, local_content_digest,
        ordered_tree_digest,
    };

    #[test]
    fn shape_hashes_are_scoped_per_ir_level() {
        let hir = ShapeHashPolicy::new("hir", ShapeDimension::Structure).unwrap();
        let mir = ShapeHashPolicy::new("mir", ShapeDimension::Structure).unwrap();

        assert_ne!(
            local_content_digest(&hir, "if", &[("condition", "x")]),
            local_content_digest(&mir, "if", &[("condition", "x")])
        );
    }

    #[test]
    fn tree_digest_includes_ordered_child_content() {
        let policy = ShapeHashPolicy::new("mir", ShapeDimension::Structure).unwrap();
        let local = local_content_digest(&policy, "block", &[]);
        let first_child = local_content_digest(&policy, "stmt", &[("value", "1")]);
        let second_child = local_content_digest(&policy, "stmt", &[("value", "2")]);

        assert_ne!(
            ordered_tree_digest(&policy, &local, std::slice::from_ref(&first_child)),
            ordered_tree_digest(&policy, &local, std::slice::from_ref(&second_child))
        );
        assert_ne!(
            ordered_tree_digest(
                &policy,
                &local,
                &[first_child.clone(), second_child.clone()]
            ),
            ordered_tree_digest(&policy, &local, &[second_child, first_child])
        );
    }

    #[test]
    fn graph_edge_digest_is_order_independent_but_label_sensitive() {
        let policy = ShapeHashPolicy::new("sonatina", ShapeDimension::Structure).unwrap();
        let source = local_content_digest(&policy, "inst", &[("opcode", "add")]);
        let target = local_content_digest(&policy, "inst", &[("opcode", "return")]);
        let first = ShapeGraphEdge {
            source_key: "a",
            label: "control",
            target_key: "b",
            source_digest: &source,
            target_digest: &target,
        };
        let second = ShapeGraphEdge {
            source_key: "b",
            label: "data",
            target_key: "a",
            source_digest: &target,
            target_digest: &source,
        };
        let relabeled = ShapeGraphEdge {
            source_key: "a",
            label: "different",
            target_key: "b",
            source_digest: &source,
            target_digest: &target,
        };

        assert_eq!(
            graph_edge_digest(&policy, &[first.clone(), second.clone()]),
            graph_edge_digest(&policy, &[second, first])
        );
        assert_ne!(
            graph_edge_digest(&policy, &[relabeled]),
            graph_edge_digest(
                &policy,
                &[ShapeGraphEdge {
                    source_key: "a",
                    label: "control",
                    target_key: "b",
                    source_digest: &source,
                    target_digest: &target,
                }]
            )
        );
    }
}
