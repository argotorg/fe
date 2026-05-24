use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};

pub const SHAPE_SCHEMA_VERSION: u32 = 1;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ShapeDimension {
    Structure,
    Names,
    Constants,
    Types,
    TraceEvents,
}

impl ShapeDimension {
    pub const ALL: [Self; 5] = [
        Self::Structure,
        Self::Names,
        Self::Constants,
        Self::Types,
        Self::TraceEvents,
    ];

    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Structure => "structure",
            Self::Names => "names",
            Self::Constants => "constants",
            Self::Types => "types",
            Self::TraceEvents => "trace_events",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ShapeDigestAlgorithm {
    Blake3_256,
    Sha2_256,
}

impl ShapeDigestAlgorithm {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Blake3_256 => "blake3-256",
            Self::Sha2_256 => "sha2-256",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ShapeViewMode {
    IdentityBound,
    AnonymousShape,
}

impl ShapeViewMode {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::IdentityBound => "identity_bound",
            Self::AnonymousShape => "anonymous_shape",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ShapeCyclePolicy {
    Reject,
    NonRecursiveGraphEdges,
    CondenseScc,
}

impl ShapeCyclePolicy {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Reject => "reject",
            Self::NonRecursiveGraphEdges => "non_recursive_graph_edges",
            Self::CondenseScc => "condense_scc",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ShapeText(String);

impl ShapeText {
    pub fn new(value: impl Into<String>, field: &'static str) -> Result<Self, ShapeError> {
        let value = value.into();
        if value.is_empty() {
            return Err(ShapeError::EmptyText { field });
        }
        Ok(Self(value))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ShapeText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

pub type ShapeLevel = ShapeText;
pub type ShapeKind = ShapeText;
pub type ShapeFieldName = ShapeText;
pub type ShapeEdgeLabel = ShapeText;
pub type ShapeLocalKey = ShapeText;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ShapeNodeKey {
    Entity(OriginExportKey),
    Derived {
        owner: OriginExportKey,
        local: ShapeLocalKey,
    },
}

impl ShapeNodeKey {
    pub fn entity(key: OriginExportKey) -> Self {
        Self::Entity(key)
    }

    pub fn derived(owner: OriginExportKey, local: impl Into<String>) -> Result<Self, ShapeError> {
        Ok(Self::Derived {
            owner,
            local: ShapeLocalKey::new(local, "shape node local key")?,
        })
    }

    pub fn owner(&self) -> &OriginExportKey {
        match self {
            Self::Entity(key) => key,
            Self::Derived { owner, .. } => owner,
        }
    }

    pub fn canonical_key(&self) -> String {
        match self {
            Self::Entity(key) => format!("entity:{}", key.canonical_storage_key()),
            Self::Derived { owner, local } => {
                format!(
                    "derived:{}:{}",
                    owner.canonical_storage_key(),
                    local.as_str()
                )
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ShapeGraphKey {
    pub owner: OriginExportKey,
    pub local: ShapeLocalKey,
}

impl ShapeGraphKey {
    pub fn new(owner: OriginExportKey, local: impl Into<String>) -> Result<Self, ShapeError> {
        Ok(Self {
            owner,
            local: ShapeLocalKey::new(local, "shape graph local key")?,
        })
    }

    pub fn canonical_key(&self) -> String {
        format!(
            "{}:{}",
            self.owner.canonical_storage_key(),
            self.local.as_str()
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeHashPolicy {
    pub schema_version: u32,
    pub algorithm: ShapeDigestAlgorithm,
    pub level: ShapeLevel,
    pub dimensions: BTreeSet<ShapeDimension>,
    pub view_mode: ShapeViewMode,
    pub cycle_policy: ShapeCyclePolicy,
}

impl ShapeHashPolicy {
    pub fn new(
        level: impl Into<String>,
        view_mode: ShapeViewMode,
        cycle_policy: ShapeCyclePolicy,
    ) -> Result<Self, ShapeError> {
        Self::with_dimensions(level, ShapeDimension::ALL, view_mode, cycle_policy)
    }

    pub fn with_dimensions(
        level: impl Into<String>,
        dimensions: impl IntoIterator<Item = ShapeDimension>,
        view_mode: ShapeViewMode,
        cycle_policy: ShapeCyclePolicy,
    ) -> Result<Self, ShapeError> {
        let dimensions = dimensions.into_iter().collect::<BTreeSet<_>>();
        if dimensions.is_empty() {
            return Err(ShapeError::EmptyDimensions);
        }
        Ok(Self {
            schema_version: SHAPE_SCHEMA_VERSION,
            algorithm: ShapeDigestAlgorithm::Blake3_256,
            level: ShapeLevel::new(level, "shape level")?,
            dimensions,
            view_mode,
            cycle_policy,
        })
    }

    pub fn includes(&self, dimension: ShapeDimension) -> bool {
        self.dimensions.contains(&dimension)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum ShapeValue {
    Text(String),
    Bool(bool),
    U64(u64),
    I64(i64),
    Bytes(Vec<u8>),
}

impl From<&str> for ShapeValue {
    fn from(value: &str) -> Self {
        Self::Text(value.to_string())
    }
}

impl From<String> for ShapeValue {
    fn from(value: String) -> Self {
        Self::Text(value)
    }
}

impl From<bool> for ShapeValue {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<u64> for ShapeValue {
    fn from(value: u64) -> Self {
        Self::U64(value)
    }
}

impl From<i64> for ShapeValue {
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeField {
    pub dimension: ShapeDimension,
    pub name: ShapeFieldName,
    pub value: ShapeValue,
}

impl ShapeField {
    pub fn new(
        dimension: ShapeDimension,
        name: impl Into<String>,
        value: impl Into<ShapeValue>,
    ) -> Result<Self, ShapeError> {
        Ok(Self {
            dimension,
            name: ShapeFieldName::new(name, "shape field name")?,
            value: value.into(),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeNode {
    pub key: ShapeNodeKey,
    pub kind: ShapeKind,
    pub fields: Vec<ShapeField>,
}

impl ShapeNode {
    pub fn new(key: ShapeNodeKey, kind: impl Into<String>) -> Result<Self, ShapeError> {
        Ok(Self {
            key,
            kind: ShapeKind::new(kind, "shape node kind")?,
            fields: Vec::new(),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeChild {
    pub parent: ShapeNodeKey,
    pub label: ShapeEdgeLabel,
    pub ordinal: u32,
    pub child: ShapeNodeKey,
}

impl ShapeChild {
    pub fn new(
        parent: ShapeNodeKey,
        label: impl Into<String>,
        ordinal: u32,
        child: ShapeNodeKey,
    ) -> Result<Self, ShapeError> {
        Ok(Self {
            parent,
            label: ShapeEdgeLabel::new(label, "shape child label")?,
            ordinal,
            child,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ShapeEdgeRole {
    Graph,
    Control,
    Data,
    Reference,
    Call,
    Dependency,
    Origin,
}

impl ShapeEdgeRole {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Graph => "graph",
            Self::Control => "control",
            Self::Data => "data",
            Self::Reference => "reference",
            Self::Call => "call",
            Self::Dependency => "dependency",
            Self::Origin => "origin",
        }
    }

    pub const fn is_recursive(self) -> bool {
        matches!(self, Self::Dependency)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeEdge {
    pub source: ShapeNodeKey,
    pub label: ShapeEdgeLabel,
    pub target: ShapeNodeKey,
    pub role: ShapeEdgeRole,
}

impl ShapeEdge {
    pub fn new(
        source: ShapeNodeKey,
        label: impl Into<String>,
        target: ShapeNodeKey,
        role: ShapeEdgeRole,
    ) -> Result<Self, ShapeError> {
        Ok(Self {
            source,
            label: ShapeEdgeLabel::new(label, "shape edge label")?,
            target,
            role,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeGraph {
    pub graph_key: ShapeGraphKey,
    pub nodes: BTreeMap<ShapeNodeKey, ShapeNode>,
    pub children: Vec<ShapeChild>,
    pub edges: Vec<ShapeEdge>,
}

impl ShapeGraph {
    pub fn new(graph_key: ShapeGraphKey) -> Self {
        Self {
            graph_key,
            nodes: BTreeMap::new(),
            children: Vec::new(),
            edges: Vec::new(),
        }
    }

    pub fn add_node(
        &mut self,
        key: ShapeNodeKey,
        kind: impl Into<String>,
    ) -> Result<(), ShapeError> {
        if self.nodes.contains_key(&key) {
            return Err(ShapeError::DuplicateNode {
                key: key.canonical_key(),
            });
        }
        let node = ShapeNode::new(key.clone(), kind)?;
        self.nodes.insert(key, node);
        Ok(())
    }

    pub fn add_field(
        &mut self,
        node: &ShapeNodeKey,
        dimension: ShapeDimension,
        name: impl Into<String>,
        value: impl Into<ShapeValue>,
    ) -> Result<(), ShapeError> {
        let Some(node) = self.nodes.get_mut(node) else {
            return Err(ShapeError::MissingNode {
                key: node.canonical_key(),
            });
        };
        node.fields.push(ShapeField::new(dimension, name, value)?);
        Ok(())
    }

    pub fn add_child(
        &mut self,
        parent: &ShapeNodeKey,
        label: impl Into<String>,
        ordinal: u32,
        child: &ShapeNodeKey,
    ) -> Result<(), ShapeError> {
        self.require_node(parent)?;
        self.require_node(child)?;
        self.children.push(ShapeChild::new(
            parent.clone(),
            label,
            ordinal,
            child.clone(),
        )?);
        Ok(())
    }

    pub fn add_edge(
        &mut self,
        source: &ShapeNodeKey,
        label: impl Into<String>,
        target: &ShapeNodeKey,
        role: ShapeEdgeRole,
    ) -> Result<(), ShapeError> {
        self.require_node(source)?;
        self.require_node(target)?;
        self.edges
            .push(ShapeEdge::new(source.clone(), label, target.clone(), role)?);
        Ok(())
    }

    pub fn validate(&self) -> Result<(), ShapeError> {
        for (key, node) in &self.nodes {
            if key != &node.key {
                return Err(ShapeError::NodeKeyMismatch {
                    map_key: key.canonical_key(),
                    node_key: node.key.canonical_key(),
                });
            }
            for field in &node.fields {
                if field.name.as_str().is_empty() {
                    return Err(ShapeError::EmptyText {
                        field: "shape field name",
                    });
                }
            }
        }
        for child in &self.children {
            self.require_node(&child.parent)?;
            self.require_node(&child.child)?;
        }
        for edge in &self.edges {
            self.require_node(&edge.source)?;
            self.require_node(&edge.target)?;
        }
        Ok(())
    }

    fn require_node(&self, key: &ShapeNodeKey) -> Result<(), ShapeError> {
        if self.nodes.contains_key(key) {
            Ok(())
        } else {
            Err(ShapeError::MissingNode {
                key: key.canonical_key(),
            })
        }
    }
}

pub trait ShapeSink {
    fn add_node(&mut self, key: ShapeNodeKey, kind: impl Into<String>) -> Result<(), ShapeError>;
    fn add_field(
        &mut self,
        node: &ShapeNodeKey,
        dimension: ShapeDimension,
        name: impl Into<String>,
        value: impl Into<ShapeValue>,
    ) -> Result<(), ShapeError>;
    fn add_child(
        &mut self,
        parent: &ShapeNodeKey,
        label: impl Into<String>,
        ordinal: u32,
        child: &ShapeNodeKey,
    ) -> Result<(), ShapeError>;
    fn add_edge(
        &mut self,
        source: &ShapeNodeKey,
        label: impl Into<String>,
        target: &ShapeNodeKey,
        role: ShapeEdgeRole,
    ) -> Result<(), ShapeError>;
}

impl ShapeSink for ShapeGraph {
    fn add_node(&mut self, key: ShapeNodeKey, kind: impl Into<String>) -> Result<(), ShapeError> {
        ShapeGraph::add_node(self, key, kind)
    }

    fn add_field(
        &mut self,
        node: &ShapeNodeKey,
        dimension: ShapeDimension,
        name: impl Into<String>,
        value: impl Into<ShapeValue>,
    ) -> Result<(), ShapeError> {
        ShapeGraph::add_field(self, node, dimension, name, value)
    }

    fn add_child(
        &mut self,
        parent: &ShapeNodeKey,
        label: impl Into<String>,
        ordinal: u32,
        child: &ShapeNodeKey,
    ) -> Result<(), ShapeError> {
        ShapeGraph::add_child(self, parent, label, ordinal, child)
    }

    fn add_edge(
        &mut self,
        source: &ShapeNodeKey,
        label: impl Into<String>,
        target: &ShapeNodeKey,
        role: ShapeEdgeRole,
    ) -> Result<(), ShapeError> {
        ShapeGraph::add_edge(self, source, label, target, role)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ShapeError {
    EmptyText { field: &'static str },
    EmptyDimensions,
    DuplicateNode { key: String },
    MissingNode { key: String },
    NodeKeyMismatch { map_key: String, node_key: String },
}

impl fmt::Display for ShapeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyText { field } => write!(f, "{field} must not be empty"),
            Self::EmptyDimensions => write!(f, "shape policy must include at least one dimension"),
            Self::DuplicateNode { key } => write!(f, "duplicate shape node key {key}"),
            Self::MissingNode { key } => write!(f, "missing shape node key {key}"),
            Self::NodeKeyMismatch { map_key, node_key } => {
                write!(
                    f,
                    "shape node stored under {map_key} but contains key {node_key}"
                )
            }
        }
    }
}

impl std::error::Error for ShapeError {}

#[cfg(test)]
mod tests {
    use super::*;

    fn origin(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn graph() -> ShapeGraph {
        ShapeGraph::new(ShapeGraphKey::new(origin("hir.body", "demo", "body:0"), "body").unwrap())
    }

    #[test]
    fn rejects_empty_shape_text() {
        assert!(matches!(
            ShapeKind::new("", "shape node kind"),
            Err(ShapeError::EmptyText {
                field: "shape node kind"
            })
        ));
    }

    #[test]
    fn policy_requires_dimensions() {
        assert!(matches!(
            ShapeHashPolicy::with_dimensions(
                "hir",
                [],
                ShapeViewMode::IdentityBound,
                ShapeCyclePolicy::Reject
            ),
            Err(ShapeError::EmptyDimensions)
        ));
    }

    #[test]
    fn graph_rejects_duplicate_nodes_and_missing_endpoints() {
        let mut graph = graph();
        let body = ShapeNodeKey::entity(origin("hir.body", "demo", "body:0"));
        let expr = ShapeNodeKey::entity(origin("hir.expr", "demo", "expr:0"));

        graph.add_node(body.clone(), "body").unwrap();
        assert!(matches!(
            graph.add_node(body.clone(), "body"),
            Err(ShapeError::DuplicateNode { .. })
        ));
        assert!(matches!(
            graph.add_child(&body, "expr", 0, &expr),
            Err(ShapeError::MissingNode { .. })
        ));
    }

    #[test]
    fn derived_node_keys_keep_owner_context() {
        let owner_a = origin("mir.body", "pkg:a", "body:0");
        let owner_b = origin("mir.body", "pkg:b", "body:0");
        let a = ShapeNodeKey::derived(owner_a, "tmp:0").unwrap();
        let b = ShapeNodeKey::derived(owner_b, "tmp:0").unwrap();

        assert_ne!(a, b);
        assert_ne!(a.canonical_key(), b.canonical_key());
    }

    #[test]
    fn graph_accepts_fields_children_and_edges() {
        let mut graph = graph();
        let body = ShapeNodeKey::entity(origin("hir.body", "demo", "body:0"));
        let expr = ShapeNodeKey::entity(origin("hir.expr", "demo", "expr:0"));

        graph.add_node(body.clone(), "body").unwrap();
        graph.add_node(expr.clone(), "literal").unwrap();
        graph
            .add_field(&expr, ShapeDimension::Constants, "value", 1_u64)
            .unwrap();
        graph.add_child(&body, "expr", 0, &expr).unwrap();
        graph
            .add_edge(&expr, "uses", &body, ShapeEdgeRole::Reference)
            .unwrap();

        graph.validate().unwrap();
    }
}
