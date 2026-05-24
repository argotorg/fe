use std::collections::{BTreeMap, BTreeSet};

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};
use trace_facts::{RelationRow, RelationSchema, TraceFact, TraceSnapshot};

pub const CORE_DATALOG_RULES: &str = r#"
origin_reaches(a, b) :-
  base_origin_edge(a, b, _, _).

origin_reaches(a, c) :-
  base_origin_edge(a, b, _, _),
  origin_reaches(b, c).
"#;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DatalogBaseExport {
    pub trace_hash: String,
    pub schemas: Vec<RelationSchema>,
    pub rows: Vec<RelationRow>,
    pub rules: &'static str,
}

pub fn emit_base_relations(snapshot: &TraceSnapshot) -> DatalogBaseExport {
    let mut schemas = BTreeMap::new();
    let mut rows = Vec::new();

    for fact in snapshot.facts() {
        let schema = fact.base_relation_schema();
        schemas.entry(schema.name).or_insert(schema);
        rows.push(fact.base_relation_row());
    }

    DatalogBaseExport {
        trace_hash: snapshot.trace_hash().to_string(),
        schemas: schemas.into_values().collect(),
        rows,
        rules: CORE_DATALOG_RULES,
    }
}

pub fn origin_reaches(snapshot: &TraceSnapshot) -> BTreeSet<(OriginExportKey, OriginExportKey)> {
    let mut adjacency: BTreeMap<OriginExportKey, BTreeSet<OriginExportKey>> = BTreeMap::new();
    for fact in snapshot.facts() {
        if let TraceFact::OriginEdge(edge) = fact {
            adjacency
                .entry(edge.from.clone())
                .or_default()
                .insert(edge.to.clone());
        }
    }

    let mut reaches = BTreeSet::new();
    for start in adjacency.keys() {
        let mut stack = adjacency
            .get(start)
            .into_iter()
            .flatten()
            .cloned()
            .collect::<Vec<_>>();
        let mut seen = BTreeSet::new();
        while let Some(next) = stack.pop() {
            if !seen.insert(next.clone()) {
                continue;
            }
            reaches.insert((start.clone(), next.clone()));
            if let Some(children) = adjacency.get(&next) {
                stack.extend(children.iter().cloned());
            }
        }
    }

    reaches
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use trace_facts::{
        InstructionFact, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, OriginNodeKind,
        TraceBundle, TraceFact, TraceMetadata, TraceSnapshot,
    };

    use super::{emit_base_relations, origin_reaches};

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(key: OriginExportKey) -> TraceFact {
        let kind = OriginNodeKind::new(key.kind());
        TraceFact::OriginNode(OriginNodeFact::new(key, kind))
    }

    fn snapshot() -> TraceSnapshot {
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let mir = key("runtime.stmt", "demo", "stmt:0");
        let hir = key("hir.expr", "demo", "expr:0");
        let function = key("bytecode.function", "demo", "runtime");
        TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "trace".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![
                node(instruction.clone()),
                node(mir.clone()),
                node(hir.clone()),
                node(function.clone()),
                TraceFact::Instruction(InstructionFact::new(
                    instruction.clone(),
                    function,
                    0,
                    "STOP",
                )),
                TraceFact::OriginEdge(OriginEdgeFact::new(
                    instruction.clone(),
                    mir.clone(),
                    OriginEdgeLabel::LoweredFrom,
                    None,
                )),
                TraceFact::OriginEdge(OriginEdgeFact::new(
                    mir,
                    hir,
                    OriginEdgeLabel::LoweredFrom,
                    None,
                )),
            ],
        ))
        .unwrap()
    }

    #[test]
    fn base_relation_export_uses_typed_fact_schemas() {
        let snapshot = snapshot();
        let export = emit_base_relations(&snapshot);

        assert!(export.trace_hash.starts_with("fnv64:"));
        assert!(
            export
                .schemas
                .iter()
                .any(|schema| schema.name == "base_origin_edge")
        );
        assert!(
            export
                .rows
                .iter()
                .any(|row| row.relation == "base_instruction")
        );
        assert!(export.rules.contains("origin_reaches"));
    }

    #[test]
    fn origin_reaches_derives_transitive_origin_paths() {
        let snapshot = snapshot();
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let hir = key("hir.expr", "demo", "expr:0");

        assert!(origin_reaches(&snapshot).contains(&(instruction, hir)));
    }
}
