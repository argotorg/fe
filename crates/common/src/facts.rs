//! Legacy analyze-only fact projections.
//!
//! New trace diagnostics must use the `fe-trace-facts` crate as the single
//! trace fact vocabulary. This module remains temporarily for
//! `fe analyze --origin-facts` report compatibility and should not be used by
//! compiler tracing, debug maps, or performance reports.

use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::origin::OriginExportKey;

pub const LEGACY_FACTS_NOTICE: &str =
    "common::facts is legacy analyze-only; use fe-trace-facts for trace diagnostics";

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum TypedFact {
    OriginNode(OriginNodeFact),
    OriginLink(OriginLinkFact),
}

impl TypedFact {
    pub const fn relation_name(&self) -> &'static str {
        match self {
            Self::OriginNode(_) => "origin_node",
            Self::OriginLink(_) => "origin_link",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct OriginNodeFact {
    pub key: OriginExportKey,
}

impl OriginNodeFact {
    pub fn new(key: OriginExportKey) -> Self {
        Self { key }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct OriginLinkFact {
    pub source: OriginExportKey,
    pub target: OriginExportKey,
    pub label: String,
}

impl OriginLinkFact {
    pub fn new(source: OriginExportKey, target: OriginExportKey, label: impl Into<String>) -> Self {
        Self {
            source,
            target,
            label: label.into(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypedFactSet {
    facts: Vec<TypedFact>,
}

pub type OwnedTypedFactSetExport = Vec<TypedFact>;

impl TypedFactSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, fact: TypedFact) {
        self.facts.push(fact);
    }

    pub fn push_origin_node(&mut self, key: OriginExportKey) {
        self.push(TypedFact::OriginNode(OriginNodeFact::new(key)));
    }

    pub fn push_origin_link(
        &mut self,
        source: OriginExportKey,
        target: OriginExportKey,
        label: impl Into<String>,
    ) {
        self.push(TypedFact::OriginLink(OriginLinkFact::new(
            source, target, label,
        )));
    }

    pub fn len(&self) -> usize {
        self.facts.len()
    }

    pub fn is_empty(&self) -> bool {
        self.facts.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &TypedFact> {
        self.facts.iter()
    }

    pub fn origin_node_count(&self) -> usize {
        self.facts
            .iter()
            .filter(|fact| matches!(fact, TypedFact::OriginNode(_)))
            .count()
    }

    pub fn origin_link_count(&self) -> usize {
        self.facts
            .iter()
            .filter(|fact| matches!(fact, TypedFact::OriginLink(_)))
            .count()
    }

    pub fn export(&self) -> OwnedTypedFactSetExport {
        self.facts.clone()
    }

    pub fn relation_counts(&self) -> Vec<TypedFactRelationCount> {
        let mut counts = vec![
            TypedFactRelationCount::new("origin_node", self.origin_node_count()),
            TypedFactRelationCount::new("origin_link", self.origin_link_count()),
        ];
        counts.retain(|count| count.rows > 0);
        counts
    }

    pub fn relation_tables(&self) -> TypedFactRelationSet {
        let mut origin_node_rows = Vec::new();
        let mut origin_link_rows = Vec::new();

        for fact in &self.facts {
            match fact {
                TypedFact::OriginNode(node) => {
                    origin_node_rows.push(TypedFactRelationRow::new(vec![
                        node.key.kind().to_string(),
                        node.key.owner_key().to_string(),
                        node.key.local_key().to_string(),
                    ]));
                }
                TypedFact::OriginLink(link) => {
                    origin_link_rows.push(TypedFactRelationRow::new(vec![
                        link.source.kind().to_string(),
                        link.source.owner_key().to_string(),
                        link.source.local_key().to_string(),
                        link.target.kind().to_string(),
                        link.target.owner_key().to_string(),
                        link.target.local_key().to_string(),
                        link.label.clone(),
                    ]));
                }
            }
        }

        TypedFactRelationSet::new(vec![
            TypedFactRelation::new(
                "origin_node",
                vec!["kind", "owner_key", "local_key"],
                origin_node_rows,
            ),
            TypedFactRelation::new(
                "origin_link",
                vec![
                    "source_kind",
                    "source_owner_key",
                    "source_local_key",
                    "target_kind",
                    "target_owner_key",
                    "target_local_key",
                    "label",
                ],
                origin_link_rows,
            ),
        ])
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct OriginFactIndex {
    nodes: BTreeSet<OriginExportKey>,
    links: Vec<OriginLinkFact>,
}

impl OriginFactIndex {
    pub fn from_facts(facts: &TypedFactSet) -> Self {
        let mut index = Self::default();
        for fact in facts.iter() {
            match fact {
                TypedFact::OriginNode(node) => {
                    index.nodes.insert(node.key.clone());
                }
                TypedFact::OriginLink(link) => {
                    index.links.push(link.clone());
                }
            }
        }
        index
    }

    pub fn contains_node(&self, key: &OriginExportKey) -> bool {
        self.nodes.contains(key)
    }

    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub fn link_count(&self) -> usize {
        self.links.len()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypedFactRelationCount {
    pub relation: String,
    pub rows: usize,
}

impl TypedFactRelationCount {
    pub fn new(relation: impl Into<String>, rows: usize) -> Self {
        Self {
            relation: relation.into(),
            rows,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypedFactRelationRow {
    pub cells: Vec<String>,
}

impl TypedFactRelationRow {
    pub fn new(cells: Vec<String>) -> Self {
        Self { cells }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypedFactRelation {
    pub name: String,
    pub columns: Vec<String>,
    pub rows: Vec<TypedFactRelationRow>,
}

impl TypedFactRelation {
    pub fn new(
        name: impl Into<String>,
        columns: Vec<impl Into<String>>,
        rows: Vec<TypedFactRelationRow>,
    ) -> Self {
        Self {
            name: name.into(),
            columns: columns.into_iter().map(Into::into).collect(),
            rows,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypedFactRelationSet {
    pub tables: Vec<TypedFactRelation>,
}

impl TypedFactRelationSet {
    pub fn new(tables: Vec<TypedFactRelation>) -> Self {
        Self { tables }
    }

    pub fn table(&self, name: &str) -> Option<&TypedFactRelation> {
        self.tables.iter().find(|table| table.name == name)
    }
}

#[cfg(test)]
mod tests {
    use super::{OriginFactIndex, TypedFactSet};
    use crate::origin::OriginExportKey;

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    #[test]
    fn typed_fact_set_is_authority_for_origin_index() {
        let node = key("runtime.stmt", "runtime:test", "block:0:stmt:0");
        let mut facts = TypedFactSet::new();
        facts.push_origin_node(node.clone());

        let index = OriginFactIndex::from_facts(&facts);

        assert!(index.contains_node(&node));
        assert_eq!(index.node_count(), 1);
        assert_eq!(index.link_count(), 0);
    }

    #[test]
    fn relation_tables_are_generated_from_typed_facts() {
        let source = key("runtime.stmt", "runtime:test", "block:0:stmt:0");
        let target = key("runtime.terminator", "runtime:test", "block:0:terminator");
        let mut facts = TypedFactSet::new();
        facts.push_origin_node(source.clone());
        facts.push_origin_node(target.clone());
        facts.push_origin_link(source, target, "flows_to");

        let relations = facts.relation_tables();
        let node_table = relations.table("origin_node").unwrap();
        let link_table = relations.table("origin_link").unwrap();

        assert_eq!(node_table.rows.len(), 2);
        assert_eq!(link_table.rows.len(), 1);
        assert_eq!(facts.relation_counts()[0].relation, "origin_node");
    }

    #[test]
    fn module_is_marked_as_legacy_analyze_only() {
        assert!(super::LEGACY_FACTS_NOTICE.contains("legacy analyze-only"));
    }
}
