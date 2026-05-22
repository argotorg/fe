use cozo::{DataValue, DbInstance, NamedRows, ScriptMutability};
use common::fact_consumer::Fact;

pub struct CompilationTrace {
    db: DbInstance,
}

macro_rules! fact_tables {
    ($(
        $variant:ident => $schema:literal, $ingest_fn:expr;
    )*) => {
        const SCHEMAS: &[&str] = &[ $( $schema ),* ];

        fn ingest_fact(trace: &CompilationTrace, fact: &Fact) {
            match fact {
                $( Fact::$variant { .. } => { let f: fn(&CompilationTrace, &Fact) = $ingest_fn; f(trace, fact); } )*
            }
        }
    };
}

fact_tables! {
    NodeHash =>
        "{ :create node_hash { node_id: Int, kind: String => parent_id: Int, structure_hash: String, names_hash: String } }",
        |trace, fact| {
            let Fact::NodeHash { node_id, kind, parent_id, structure_hash, names_hash } = fact else { return };
            trace.put("node_hash",
                &["node_id", "kind", "parent_id", "structure_hash", "names_hash"],
                btree([
                    ("node_id", DataValue::from(*node_id as i64)),
                    ("kind", DataValue::Str(kind.clone().into())),
                    ("parent_id", DataValue::from(parent_id.map(|p| p as i64).unwrap_or(-1))),
                    ("structure_hash", DataValue::Str(format!("{structure_hash:032x}").into())),
                    ("names_hash", DataValue::Str(format!("{names_hash:032x}").into())),
                ]));
        };

    Origin =>
        "{ :create origin { node_id: Int => level: Int, node: Int, transform: Int } }",
        |trace, fact| {
            let Fact::Origin { node_id, origin } = fact else { return };
            trace.put("origin",
                &["node_id", "level", "node", "transform"],
                btree([
                    ("node_id", DataValue::from(*node_id as i64)),
                    ("level", DataValue::from(origin.level as i64)),
                    ("node", DataValue::from(origin.node as i64)),
                    ("transform", DataValue::from(origin.transform as i64)),
                ]));
        };

    SourceSpan =>
        "{ :create source_span { node_id: Int => file: String, line: Int, col: Int, end_line: Int, end_col: Int } }",
        |trace, fact| {
            let Fact::SourceSpan { node_id, file, line, col, end_line, end_col } = fact else { return };
            trace.put("source_span",
                &["node_id", "file", "line", "col", "end_line", "end_col"],
                btree([
                    ("node_id", DataValue::from(*node_id as i64)),
                    ("file", DataValue::Str(file.clone().into())),
                    ("line", DataValue::from(*line as i64)),
                    ("col", DataValue::from(*col as i64)),
                    ("end_line", DataValue::from(*end_line as i64)),
                    ("end_col", DataValue::from(*end_col as i64)),
                ]));
        };

    GraphEdge =>
        "{ :create graph_edge { source_id: Int, label: String => target_id: Int } }",
        |trace, fact| {
            let Fact::GraphEdge { source_id, label, target_id } = fact else { return };
            trace.put("graph_edge",
                &["source_id", "label", "target_id"],
                btree([
                    ("source_id", DataValue::from(*source_id as i64)),
                    ("label", DataValue::Str(label.clone().into())),
                    ("target_id", DataValue::from(*target_id as i64)),
                ]));
        };

    NodeEffect =>
        "{ :create node_effect { node_id: Int, effect: String } }",
        |trace, fact| {
            let Fact::NodeEffect { node_id, effect } = fact else { return };
            trace.put("node_effect",
                &["node_id", "effect"],
                btree([
                    ("node_id", DataValue::from(*node_id as i64)),
                    ("effect", DataValue::Str(effect.clone().into())),
                ]));
        };

    DataFlowEdge =>
        "{ :create data_flow { from_id: Int, to_id: Int } }",
        |trace, fact| {
            let Fact::DataFlowEdge { from_id, to_id } = fact else { return };
            trace.put("data_flow",
                &["from_id", "to_id"],
                btree([
                    ("from_id", DataValue::from(*from_id as i64)),
                    ("to_id", DataValue::from(*to_id as i64)),
                ]));
        };
}

impl CompilationTrace {
    pub fn new() -> Self {
        let db = DbInstance::new("mem", "", Default::default())
            .expect("in-memory cozo instance");

        for schema in SCHEMAS {
            db.run_script(schema, Default::default(), ScriptMutability::Mutable)
                .unwrap_or_else(|e| panic!("schema: {e}\nline: {schema}"));
        }

        Self { db }
    }

    pub fn ingest(&self, facts: &[Fact]) {
        for fact in facts {
            ingest_fact(self, fact);
        }
    }

    pub fn query(&self, script: &str) -> Result<NamedRows, String> {
        self.db.run_script(script, Default::default(), ScriptMutability::Immutable)
            .map_err(|e| format!("{e}"))
    }

    fn put(&self, table: &str, cols: &[&str], params: std::collections::BTreeMap<String, DataValue>) {
        let col_list = cols.join(", ");
        let placeholders = cols.iter().map(|c| format!("${c}")).collect::<Vec<_>>().join(", ");
        let script = format!("?[{col_list}] <- [[{placeholders}]] :put {table} {{ {col_list} }}");
        self.db.run_script(&script, params, ScriptMutability::Mutable)
            .unwrap_or_else(|e| panic!("insert {table}: {e}"));
    }
}

fn btree(pairs: impl IntoIterator<Item = (&'static str, DataValue)>) -> std::collections::BTreeMap<String, DataValue> {
    pairs.into_iter().map(|(k, v)| (k.to_string(), v)).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::fact_consumer::FactConsumer;
    use common::ir_describe::{Dim, IrConsumer};

    #[test]
    fn fact_consumer_feeds_cozo_trace() {
        let mut fc = FactConsumer::new();
        fc.enter_node("Root");
        fc.field_u64(Dim::Structure, 42);
        fc.enter_node("Child");
        fc.field_str(Dim::Names, "hello");
        fc.exit_node();
        fc.exit_node();

        let trace = CompilationTrace::new();
        trace.ingest(fc.facts());

        let result = trace.query("?[node_id, kind] := *node_hash[node_id, kind, _, _, _]")
            .expect("query");
        assert_eq!(result.rows.len(), 2, "should have 2 node_hash entries");
    }

    #[test]
    fn datalog_query_on_facts() {
        let mut fc = FactConsumer::new();

        fc.origin(&common::provenance::ProvenanceNodeId::new(
            common::provenance::IrLevel::Smir, 5,
            common::provenance::TransformTag::SmirToMir,
        ));
        fc.enter_node("Assign");
        fc.field_u64(Dim::Structure, 1);
        fc.exit_node();

        let trace = CompilationTrace::new();
        trace.ingest(fc.facts());

        let origins = trace.query("?[node_id, level, node] := *origin[node_id, level, node, _]")
            .expect("query origins");
        assert_eq!(origins.rows.len(), 1, "should have 1 origin entry");
    }

    #[test]
    fn schema_covers_all_fact_variants() {
        assert_eq!(SCHEMAS.len(), 6, "should have one schema per Fact variant");
        assert!(SCHEMAS[0].contains("node_hash"));
        assert!(SCHEMAS[5].contains("data_flow"));
    }
}
