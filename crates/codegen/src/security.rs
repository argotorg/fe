#[cfg(feature = "datalog")]
use crate::analyze::SourceAnalysis;

#[cfg(feature = "datalog")]
use crate::trace::CompilationTrace;

#[cfg(feature = "datalog")]
fn row_int(row: &[cozo::DataValue], idx: usize) -> u32 {
    row.get(idx).and_then(|v| v.get_int()).unwrap_or(0) as u32
}

#[derive(Debug, Clone)]
pub struct Finding {
    pub detector: &'static str,
    pub severity: Severity,
    pub description: &'static str,
    pub node_ids: Vec<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Info,
    Low,
    Medium,
    High,
}

#[cfg(feature = "datalog")]
pub struct SecurityAnalyzer {
    trace: CompilationTrace,
}

#[cfg(feature = "datalog")]
impl SecurityAnalyzer {
    pub fn from_analysis(analysis: &SourceAnalysis) -> Self {
        let trace = CompilationTrace::new();
        trace.ingest(&analysis.facts);
        Self { trace }
    }

    pub fn run_all(&self) -> Vec<Finding> {
        let mut findings = Vec::new();
        findings.extend(self.detect_missing_access_control());
        findings.extend(self.detect_unchecked_call_return());
        findings.extend(self.detect_tainted_calldata_to_storage());
        findings
    }

    pub fn detect_missing_access_control(&self) -> Vec<Finding> {
        self.query_findings(
            MISSING_ACCESS_CONTROL,
            "missing-access-control",
            Severity::Medium,
            "storage_write without msg_sender_read in same function subtree",
            |row| vec![row_int(row, 0)],
        )
    }

    pub fn detect_unchecked_call_return(&self) -> Vec<Finding> {
        self.query_findings(
            UNCHECKED_CALL_RETURN,
            "unchecked-call-return",
            Severity::High,
            "external_call node with no data_flow consumer",
            |row| vec![row_int(row, 0)],
        )
    }

    pub fn detect_tainted_calldata_to_storage(&self) -> Vec<Finding> {
        self.query_findings(
            TAINTED_CALLDATA_TO_STORAGE,
            "tainted-calldata-to-storage",
            Severity::Info,
            "data flows from calldata_read to storage_write",
            |row| vec![row_int(row, 0), row_int(row, 1)],
        )
    }

    fn query_findings(
        &self,
        query: &str,
        detector: &'static str,
        severity: Severity,
        description: &'static str,
        extract_ids: impl Fn(&[cozo::DataValue]) -> Vec<u32>,
    ) -> Vec<Finding> {
        match self.trace.query(query) {
            Ok(rows) => rows.rows.iter().map(|row| Finding {
                detector,
                severity,
                description,
                node_ids: extract_ids(row),
            }).collect(),
            Err(e) => {
                tracing::warn!("security query failed: {e}");
                Vec::new()
            }
        }
    }

    pub fn fe_eliminated_by_design() -> &'static [&'static str] {
        crate::analyze::FE_ELIMINATED_BY_DESIGN
    }
}

#[cfg(feature = "datalog")]
const MISSING_ACCESS_CONTROL: &str = r#"
    write_nodes[n] := *node_effect[n, 'storage_write']
    sender_nodes[n] := *node_effect[n, 'msg_sender_read']
    write_parents[w, p] := write_nodes[w], *node_hash[w, _, p, _, _], p != -1
    sender_parents[s, p] := sender_nodes[s], *node_hash[s, _, p, _, _], p != -1
    unguarded[w] := write_parents[w, p], not sender_parents[_, p]
    ?[w] := unguarded[w]
"#;

#[cfg(feature = "datalog")]
const UNCHECKED_CALL_RETURN: &str = r#"
    call_nodes[n] := *node_effect[n, 'external_call']
    consumed[n] := call_nodes[n], *data_flow[n, _]
    unchecked[n] := call_nodes[n], not consumed[n]
    ?[n] := unchecked[n]
"#;

#[cfg(feature = "datalog")]
const TAINTED_CALLDATA_TO_STORAGE: &str = r#"
    calldata_nodes[n] := *node_effect[n, 'calldata_read']
    storage_nodes[n] := *node_effect[n, 'storage_write']
    reaches[a, b] := *data_flow[a, b]
    reaches[a, c] := reaches[a, b], *data_flow[b, c]
    tainted[src, sink] := calldata_nodes[src], storage_nodes[sink], reaches[src, sink]
    ?[src, sink] := tainted[src, sink] :limit 20
"#;
