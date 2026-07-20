use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};

pub const SHAPE_SCHEMA_VERSION: u32 = 1;

/// Reserved separator for canonical shape keys; banned in every ShapeText so
/// canonical keys parse unambiguously (mirrors OriginExportKey's storage
/// separator discipline).
pub const CANONICAL_KEY_SEPARATOR: char = '\u{1f}';

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
}

impl ShapeDigestAlgorithm {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Blake3_256 => "blake3-256",
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
}

impl ShapeCyclePolicy {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Reject => "reject",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(transparent)]
pub struct ShapeText(String);

impl ShapeText {
    pub fn new(value: impl Into<String>, field: &'static str) -> Result<Self, ShapeError> {
        let value = value.into();
        if value.is_empty() {
            return Err(ShapeError::EmptyText { field });
        }
        if value.contains(CANONICAL_KEY_SEPARATOR) {
            return Err(ShapeError::ReservedSeparator { field });
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

impl<'de> serde::Deserialize<'de> for ShapeText {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        Self::new(value, "shape text").map_err(serde::de::Error::custom)
    }
}

pub type ShapeLevel = ShapeText;
pub type ShapeKind = ShapeText;
pub type ShapeFieldName = ShapeText;
pub type ShapeEdgeLabel = ShapeText;
pub type ShapeLocalKey = ShapeText;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(transparent)]
pub struct ShapeDigest(String);

impl<'de> serde::Deserialize<'de> for ShapeDigest {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        Self::new(value).map_err(serde::de::Error::custom)
    }
}

impl ShapeDigest {
    pub fn new(hex: impl Into<String>) -> Result<Self, ShapeError> {
        let hex = hex.into();
        if hex.len() != 64
            || !hex
                .bytes()
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
        {
            return Err(ShapeError::InvalidDigest);
        }
        Ok(Self(hex))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn display_short(&self) -> &str {
        self.0.get(..16).unwrap_or(&self.0)
    }
}

impl fmt::Display for ShapeDigest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

pub type ShapePolicyId = ShapeDigest;

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
                    "derived:{}{CANONICAL_KEY_SEPARATOR}{}",
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
            "{}{CANONICAL_KEY_SEPARATOR}{}",
            self.owner.canonical_storage_key(),
            self.local.as_str()
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

    pub fn policy_id(&self) -> ShapePolicyId {
        let mut bytes = Vec::new();
        push_str(&mut bytes, "fe.shape.policy");
        push_u32(&mut bytes, self.schema_version);
        push_str(&mut bytes, self.algorithm.as_str());
        push_str(&mut bytes, self.level.as_str());
        push_str(&mut bytes, self.view_mode.as_str());
        push_str(&mut bytes, self.cycle_policy.as_str());
        for dimension in &self.dimensions {
            push_str(&mut bytes, dimension.as_str());
        }
        digest_bytes(&bytes)
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

impl From<u32> for ShapeValue {
    fn from(value: u32) -> Self {
        Self::U64(value.into())
    }
}

impl From<i64> for ShapeValue {
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

impl From<i32> for ShapeValue {
    fn from(value: i32) -> Self {
        Self::I64(value.into())
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

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeGraph {
    pub graph_key: ShapeGraphKey,
    pub nodes: BTreeMap<ShapeNodeKey, ShapeNode>,
    pub children: Vec<ShapeChild>,
}

impl ShapeGraph {
    pub fn new(graph_key: ShapeGraphKey) -> Self {
        Self {
            graph_key,
            nodes: BTreeMap::new(),
            children: Vec::new(),
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

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct DimensionDigests {
    pub values: BTreeMap<ShapeDimension, ShapeDigest>,
}

impl DimensionDigests {
    pub fn insert(&mut self, dimension: ShapeDimension, digest: ShapeDigest) {
        self.values.insert(dimension, digest);
    }

    pub fn get(&self, dimension: ShapeDimension) -> Option<&ShapeDigest> {
        self.values.get(&dimension)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ShapeDimension, &ShapeDigest)> {
        self.values.iter()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeNodeHashes {
    pub local: DimensionDigests,
    pub tree: DimensionDigests,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ShapeGraphHashes {
    pub policy_id: ShapePolicyId,
    pub nodes: BTreeMap<ShapeNodeKey, ShapeNodeHashes>,
    pub graph: DimensionDigests,
}

pub fn local_node_digests(
    policy: &ShapeHashPolicy,
    node: &ShapeNode,
) -> Result<DimensionDigests, ShapeError> {
    let mut digests = DimensionDigests::default();
    for dimension in &policy.dimensions {
        digests.insert(
            *dimension,
            digest_record(policy, *dimension, "node.local", |bytes| {
                if policy.view_mode == ShapeViewMode::IdentityBound {
                    push_node_key(bytes, &node.key);
                }
                if *dimension == ShapeDimension::Structure {
                    push_str(bytes, node.kind.as_str());
                }
                let mut fields = node
                    .fields
                    .iter()
                    .filter(|field| field.dimension == *dimension)
                    .collect::<Vec<_>>();
                fields.sort_by(|left, right| {
                    (left.name.as_str(), &left.value).cmp(&(right.name.as_str(), &right.value))
                });
                push_u32(bytes, fields.len() as u32);
                for field in fields {
                    push_str(bytes, field.name.as_str());
                    push_shape_value(bytes, &field.value);
                }
            })?,
        );
    }
    Ok(digests)
}

pub fn hash_acyclic_shape_graph(
    policy: &ShapeHashPolicy,
    graph: &ShapeGraph,
) -> Result<ShapeGraphHashes, ShapeError> {
    if policy.dimensions.is_empty() {
        return Err(ShapeError::EmptyDimensions);
    }
    graph.validate()?;
    ensure_recursive_edges_acyclic(graph)?;

    let children_by_parent = children_by_parent(graph);
    let mut local = BTreeMap::new();
    for (key, node) in &graph.nodes {
        local.insert(key.clone(), local_node_digests(policy, node)?);
    }

    let mut marks = BTreeMap::new();
    let mut tree = BTreeMap::new();
    for key in graph.nodes.keys() {
        tree_digests_for_node(
            policy,
            key,
            &children_by_parent,
            &local,
            &mut tree,
            &mut marks,
        )?;
    }

    let mut graph_digests = DimensionDigests::default();
    for dimension in &policy.dimensions {
        graph_digests.insert(
            *dimension,
            graph_digest_for_dimension(policy, graph, &tree, *dimension)?,
        );
    }

    let nodes = graph
        .nodes
        .keys()
        .map(|key| {
            (
                key.clone(),
                ShapeNodeHashes {
                    local: local.get(key).expect("local digest was computed").clone(),
                    tree: tree.get(key).expect("tree digest was computed").clone(),
                },
            )
        })
        .collect();

    Ok(ShapeGraphHashes {
        policy_id: policy.policy_id(),
        nodes,
        graph: graph_digests,
    })
}

pub fn hash_shape_graph(
    policy: &ShapeHashPolicy,
    graph: &ShapeGraph,
) -> Result<ShapeGraphHashes, ShapeError> {
    match policy.cycle_policy {
        ShapeCyclePolicy::Reject => hash_acyclic_shape_graph(policy, graph),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum VisitMark {
    Visiting,
    Done,
}

fn tree_digests_for_node(
    policy: &ShapeHashPolicy,
    key: &ShapeNodeKey,
    children_by_parent: &BTreeMap<ShapeNodeKey, Vec<&ShapeChild>>,
    local: &BTreeMap<ShapeNodeKey, DimensionDigests>,
    tree: &mut BTreeMap<ShapeNodeKey, DimensionDigests>,
    marks: &mut BTreeMap<ShapeNodeKey, VisitMark>,
) -> Result<DimensionDigests, ShapeError> {
    if let Some(cached) = tree.get(key) {
        return Ok(cached.clone());
    }
    if marks.get(key) == Some(&VisitMark::Visiting) {
        return Err(ShapeError::CycleDetected {
            key: key.canonical_key(),
        });
    }
    marks.insert(key.clone(), VisitMark::Visiting);

    let child_trees = children_by_parent
        .get(key)
        .map(|children| {
            children
                .iter()
                .map(|child| {
                    tree_digests_for_node(
                        policy,
                        &child.child,
                        children_by_parent,
                        local,
                        tree,
                        marks,
                    )
                    .map(|digests| (*child, digests))
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .transpose()?
        .unwrap_or_default();

    let mut digests = DimensionDigests::default();
    for dimension in &policy.dimensions {
        digests.insert(
            *dimension,
            digest_record(policy, *dimension, "node.tree", |bytes| {
                let local_digest = local
                    .get(key)
                    .and_then(|digests| digests.get(*dimension))
                    .expect("local digest was computed");
                push_digest(bytes, local_digest);
                push_u32(bytes, child_trees.len() as u32);
                for (child, child_digests) in &child_trees {
                    if *dimension == ShapeDimension::Structure {
                        push_u32(bytes, child.ordinal);
                        push_str(bytes, child.label.as_str());
                    }
                    let child_digest = child_digests
                        .get(*dimension)
                        .expect("child tree digest was computed");
                    push_digest(bytes, child_digest);
                }
            })?,
        );
    }

    marks.insert(key.clone(), VisitMark::Done);
    tree.insert(key.clone(), digests.clone());
    Ok(digests)
}

fn children_by_parent(graph: &ShapeGraph) -> BTreeMap<ShapeNodeKey, Vec<&ShapeChild>> {
    let mut children_by_parent: BTreeMap<ShapeNodeKey, Vec<&ShapeChild>> = BTreeMap::new();
    for child in &graph.children {
        children_by_parent
            .entry(child.parent.clone())
            .or_default()
            .push(child);
    }
    for children in children_by_parent.values_mut() {
        children.sort_by(|left, right| {
            (
                left.ordinal,
                left.label.as_str(),
                left.child.canonical_key(),
            )
                .cmp(&(
                    right.ordinal,
                    right.label.as_str(),
                    right.child.canonical_key(),
                ))
        });
    }
    children_by_parent
}

fn child_successors(graph: &ShapeGraph) -> BTreeMap<ShapeNodeKey, Vec<ShapeNodeKey>> {
    let mut successors: BTreeMap<ShapeNodeKey, Vec<ShapeNodeKey>> = BTreeMap::new();
    for child in &graph.children {
        successors
            .entry(child.parent.clone())
            .or_default()
            .push(child.child.clone());
    }
    for targets in successors.values_mut() {
        targets.sort_by_key(ShapeNodeKey::canonical_key);
        targets.dedup();
    }
    successors
}

fn ensure_recursive_edges_acyclic(graph: &ShapeGraph) -> Result<(), ShapeError> {
    let successors = child_successors(graph);
    let mut marks = BTreeMap::new();
    for key in graph.nodes.keys() {
        ensure_recursive_node_acyclic(key, &successors, &mut marks)?;
    }
    Ok(())
}

fn ensure_recursive_node_acyclic(
    key: &ShapeNodeKey,
    successors: &BTreeMap<ShapeNodeKey, Vec<ShapeNodeKey>>,
    marks: &mut BTreeMap<ShapeNodeKey, VisitMark>,
) -> Result<(), ShapeError> {
    match marks.get(key) {
        Some(VisitMark::Done) => return Ok(()),
        Some(VisitMark::Visiting) => {
            return Err(ShapeError::CycleDetected {
                key: key.canonical_key(),
            });
        }
        None => {}
    }
    marks.insert(key.clone(), VisitMark::Visiting);
    if let Some(targets) = successors.get(key) {
        for target in targets {
            ensure_recursive_node_acyclic(target, successors, marks)?;
        }
    }
    marks.insert(key.clone(), VisitMark::Done);
    Ok(())
}

fn graph_digest_for_dimension(
    policy: &ShapeHashPolicy,
    graph: &ShapeGraph,
    tree: &BTreeMap<ShapeNodeKey, DimensionDigests>,
    dimension: ShapeDimension,
) -> Result<ShapeDigest, ShapeError> {
    let mut node_records = graph
        .nodes
        .keys()
        .map(|key| {
            let digest = tree
                .get(key)
                .and_then(|digests| digests.get(dimension))
                .expect("tree digest was computed");
            (key, digest)
        })
        .collect::<Vec<_>>();
    node_records.sort_by(|left, right| match policy.view_mode {
        ShapeViewMode::IdentityBound => left.0.canonical_key().cmp(&right.0.canonical_key()),
        ShapeViewMode::AnonymousShape => left
            .1
            .as_str()
            .cmp(right.1.as_str())
            .then_with(|| left.0.owner().kind().cmp(right.0.owner().kind())),
    });

    digest_record(policy, dimension, "graph.full", |bytes| {
        if policy.view_mode == ShapeViewMode::IdentityBound {
            push_str(bytes, &graph.graph_key.canonical_key());
        }

        push_u32(bytes, node_records.len() as u32);
        for (key, digest) in &node_records {
            if policy.view_mode == ShapeViewMode::IdentityBound {
                push_node_key(bytes, key);
            }
            push_digest(bytes, digest);
        }

        // Reserved trailing count. The removed edge machinery encoded its
        // edge count here; keeping the constant zero preserves the digest
        // preimage of every edge-free graph, which the pinned golden test
        // verifies.
        push_u32(bytes, 0);
    })
}

fn digest_record(
    policy: &ShapeHashPolicy,
    dimension: ShapeDimension,
    record_tag: &str,
    write_payload: impl FnOnce(&mut Vec<u8>),
) -> Result<ShapeDigest, ShapeError> {
    let mut bytes = Vec::new();
    push_str(&mut bytes, "fe.shape");
    push_u32(&mut bytes, policy.schema_version);
    push_str(&mut bytes, policy.algorithm.as_str());
    push_str(&mut bytes, policy.level.as_str());
    push_str(&mut bytes, dimension.as_str());
    push_str(&mut bytes, policy.view_mode.as_str());
    push_str(&mut bytes, policy.cycle_policy.as_str());
    push_str(&mut bytes, record_tag);
    write_payload(&mut bytes);
    Ok(digest_bytes(&bytes))
}

fn digest_bytes(bytes: &[u8]) -> ShapeDigest {
    ShapeDigest(blake3::hash(bytes).to_hex().to_string())
}

fn push_u32(bytes: &mut Vec<u8>, value: u32) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

fn push_u64(bytes: &mut Vec<u8>, value: u64) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

fn push_i64(bytes: &mut Vec<u8>, value: i64) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

fn push_str(bytes: &mut Vec<u8>, value: &str) {
    push_u64(bytes, value.len() as u64);
    bytes.extend_from_slice(value.as_bytes());
}

fn push_digest(bytes: &mut Vec<u8>, digest: &ShapeDigest) {
    push_str(bytes, digest.as_str());
}

fn push_node_key(bytes: &mut Vec<u8>, key: &ShapeNodeKey) {
    push_str(bytes, &key.canonical_key());
}

fn push_shape_value(bytes: &mut Vec<u8>, value: &ShapeValue) {
    match value {
        ShapeValue::Text(value) => {
            push_str(bytes, "text");
            push_str(bytes, value);
        }
        ShapeValue::Bool(value) => {
            push_str(bytes, "bool");
            bytes.push(u8::from(*value));
        }
        ShapeValue::U64(value) => {
            push_str(bytes, "u64");
            push_u64(bytes, *value);
        }
        ShapeValue::I64(value) => {
            push_str(bytes, "i64");
            push_i64(bytes, *value);
        }
        ShapeValue::Bytes(value) => {
            push_str(bytes, "bytes");
            push_u64(bytes, value.len() as u64);
            bytes.extend_from_slice(value);
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ShapeError {
    EmptyText { field: &'static str },
    ReservedSeparator { field: &'static str },
    EmptyDimensions,
    InvalidDigest,
    DuplicateNode { key: String },
    MissingNode { key: String },
    NodeKeyMismatch { map_key: String, node_key: String },
    CycleDetected { key: String },
}

impl fmt::Display for ShapeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyText { field } => write!(f, "{field} must not be empty"),
            Self::ReservedSeparator { field } => write!(
                f,
                "{field} must not contain the reserved canonical-key separator (U+001F)"
            ),
            Self::EmptyDimensions => write!(f, "shape policy must include at least one dimension"),
            Self::InvalidDigest => write!(f, "shape digest must be 64 lowercase hex characters"),
            Self::DuplicateNode { key } => write!(f, "duplicate shape node key {key}"),
            Self::MissingNode { key } => write!(f, "missing shape node key {key}"),
            Self::NodeKeyMismatch { map_key, node_key } => {
                write!(
                    f,
                    "shape node stored under {map_key} but contains key {node_key}"
                )
            }
            Self::CycleDetected { key } => {
                write!(f, "shape child graph contains a cycle at {key}")
            }
        }
    }
}

impl std::error::Error for ShapeError {}

#[cfg(test)]
mod tests {
    use super::*;

    /// Pins concrete digest values so any change to the preimage encoding
    /// (field order, separators, length prefixes) fails loudly instead of
    /// silently re-keying every stored shape hash. Update these constants
    /// only for a deliberate, versioned format change.
    #[test]
    fn golden_digests_are_pinned() {
        let policy = ShapeHashPolicy::new(
            "hir",
            ShapeViewMode::IdentityBound,
            ShapeCyclePolicy::Reject,
        )
        .unwrap();
        let hashes = hash_shape_graph(&policy, &literal_graph(42)).unwrap();
        let structure = hashes
            .graph
            .get(ShapeDimension::Structure)
            .expect("structure digest must exist")
            .as_str();
        let constants = hashes
            .graph
            .get(ShapeDimension::Constants)
            .expect("constants digest must exist")
            .as_str();
        assert_eq!(structure, "06229f52387617d18938622f37772324392592015e422afb9df8525cbdeec834");
        assert_eq!(constants, "589cf4e9537fc17571f9bfd0cef18eaafec00552079afd2b4959c451e3b3b7f6");
    }

    fn origin(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn graph() -> ShapeGraph {
        ShapeGraph::new(ShapeGraphKey::new(origin("hir.body", "demo", "body:0"), "body").unwrap())
    }

    fn policy(view_mode: ShapeViewMode) -> ShapeHashPolicy {
        ShapeHashPolicy::new("hir", view_mode, ShapeCyclePolicy::Reject).unwrap()
    }

    fn literal_graph(value: u64) -> ShapeGraph {
        let mut graph = graph();
        let body = ShapeNodeKey::entity(origin("hir.body", "demo", "body:0"));
        let expr = ShapeNodeKey::entity(origin("hir.expr", "demo", "expr:0"));
        graph.add_node(body.clone(), "body").unwrap();
        graph.add_node(expr.clone(), "literal").unwrap();
        graph
            .add_field(&expr, ShapeDimension::Constants, "value", value)
            .unwrap();
        graph.add_child(&body, "expr", 0, &expr).unwrap();
        graph
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
    fn graph_accepts_fields_and_children() {
        let mut graph = graph();
        let body = ShapeNodeKey::entity(origin("hir.body", "demo", "body:0"));
        let expr = ShapeNodeKey::entity(origin("hir.expr", "demo", "expr:0"));

        graph.add_node(body.clone(), "body").unwrap();
        graph.add_node(expr.clone(), "literal").unwrap();
        graph
            .add_field(&expr, ShapeDimension::Constants, "value", 1_u64)
            .unwrap();
        graph.add_child(&body, "expr", 0, &expr).unwrap();

        graph.validate().unwrap();
    }

    #[test]
    fn local_hashes_are_dimension_pure() {
        let left = literal_graph(1);
        let right = literal_graph(2);
        let policy = policy(ShapeViewMode::AnonymousShape);

        let left_hashes = hash_acyclic_shape_graph(&policy, &left).unwrap();
        let right_hashes = hash_acyclic_shape_graph(&policy, &right).unwrap();

        assert_eq!(
            left_hashes.graph.get(ShapeDimension::Structure),
            right_hashes.graph.get(ShapeDimension::Structure)
        );
        assert_ne!(
            left_hashes.graph.get(ShapeDimension::Constants),
            right_hashes.graph.get(ShapeDimension::Constants)
        );
    }

    #[test]
    fn local_fields_are_insertion_order_independent() {
        let node_key = ShapeNodeKey::entity(origin("hir.expr", "demo", "expr:0"));
        let mut first = ShapeNode::new(node_key.clone(), "name").unwrap();
        first
            .fields
            .push(ShapeField::new(ShapeDimension::Names, "b", "second").unwrap());
        first
            .fields
            .push(ShapeField::new(ShapeDimension::Names, "a", "first").unwrap());

        let mut second = ShapeNode::new(node_key, "name").unwrap();
        second
            .fields
            .push(ShapeField::new(ShapeDimension::Names, "a", "first").unwrap());
        second
            .fields
            .push(ShapeField::new(ShapeDimension::Names, "b", "second").unwrap());

        let policy = policy(ShapeViewMode::IdentityBound);
        assert_eq!(
            local_node_digests(&policy, &first).unwrap(),
            local_node_digests(&policy, &second).unwrap()
        );
    }

    #[test]
    fn ordered_child_tree_hash_is_order_sensitive() {
        let mut left = graph();
        let mut right = graph();
        let body = ShapeNodeKey::entity(origin("hir.body", "demo", "body:0"));
        let a = ShapeNodeKey::entity(origin("hir.expr", "demo", "expr:a"));
        let b = ShapeNodeKey::entity(origin("hir.expr", "demo", "expr:b"));
        for graph in [&mut left, &mut right] {
            graph.add_node(body.clone(), "body").unwrap();
            graph.add_node(a.clone(), "literal").unwrap();
            graph.add_node(b.clone(), "name").unwrap();
            graph
                .add_field(&a, ShapeDimension::Constants, "value", 1_u64)
                .unwrap();
            graph
                .add_field(&b, ShapeDimension::Constants, "value", 2_u64)
                .unwrap();
        }
        left.add_child(&body, "expr", 0, &a).unwrap();
        left.add_child(&body, "expr", 1, &b).unwrap();
        right.add_child(&body, "expr", 0, &b).unwrap();
        right.add_child(&body, "expr", 1, &a).unwrap();

        let policy = policy(ShapeViewMode::AnonymousShape);
        assert_ne!(
            hash_acyclic_shape_graph(&policy, &left)
                .unwrap()
                .graph
                .get(ShapeDimension::Structure),
            hash_acyclic_shape_graph(&policy, &right)
                .unwrap()
                .graph
                .get(ShapeDimension::Structure)
        );
    }

    #[test]
    fn child_cycles_are_rejected_in_acyclic_hashing() {
        let mut graph = graph();
        let a = ShapeNodeKey::entity(origin("hir.expr", "demo", "expr:a"));
        let b = ShapeNodeKey::entity(origin("hir.expr", "demo", "expr:b"));
        graph.add_node(a.clone(), "a").unwrap();
        graph.add_node(b.clone(), "b").unwrap();
        graph.add_child(&a, "next", 0, &b).unwrap();
        graph.add_child(&b, "next", 0, &a).unwrap();

        assert!(matches!(
            hash_acyclic_shape_graph(&policy(ShapeViewMode::IdentityBound), &graph),
            Err(ShapeError::CycleDetected { .. })
        ));
    }

    #[test]
    fn identity_bound_and_anonymous_modes_are_separate() {
        let left = literal_graph(1);
        let mut right = ShapeGraph::new(
            ShapeGraphKey::new(origin("hir.body", "other-owner", "body:0"), "body").unwrap(),
        );
        let body = ShapeNodeKey::entity(origin("hir.body", "other-owner", "body:0"));
        let expr = ShapeNodeKey::entity(origin("hir.expr", "other-owner", "expr:0"));
        right.add_node(body.clone(), "body").unwrap();
        right.add_node(expr.clone(), "literal").unwrap();
        right
            .add_field(&expr, ShapeDimension::Constants, "value", 1_u64)
            .unwrap();
        right.add_child(&body, "expr", 0, &expr).unwrap();

        let identity_policy = policy(ShapeViewMode::IdentityBound);
        let anonymous_policy = policy(ShapeViewMode::AnonymousShape);
        assert_ne!(
            hash_acyclic_shape_graph(&identity_policy, &left)
                .unwrap()
                .graph
                .get(ShapeDimension::Structure),
            hash_acyclic_shape_graph(&identity_policy, &right)
                .unwrap()
                .graph
                .get(ShapeDimension::Structure)
        );
        assert_eq!(
            hash_acyclic_shape_graph(&anonymous_policy, &left)
                .unwrap()
                .graph
                .get(ShapeDimension::Structure),
            hash_acyclic_shape_graph(&anonymous_policy, &right)
                .unwrap()
                .graph
                .get(ShapeDimension::Structure)
        );
    }
}
