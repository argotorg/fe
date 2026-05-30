use std::collections::{BTreeMap, BTreeSet};

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};
use serde_json::json;
use trace_facts::{
    OriginEdgeFact, OriginEdgeLabel, SourceSpanFact, TraceDataSource, TraceFact, TraceSnapshot,
};

use crate::{Confidence, data_source_label};

pub const ARGOT_STATIC_CHECKS_ID: &str = "argot_static_checks_v0";
pub const ARGOT_STATIC_CHECKS_VERSION: &str = "0.1.0";
pub const RELATION_SCHEMA_VERSION: &str = "fe-datalog-base-export-v1";

pub type AttributionConfidence = Confidence;
pub type BoundaryId = String;
pub type GapId = String;
pub type WitnessId = String;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StaticAnalysisReport {
    pub metadata: AnalysisMetadata,
    pub checks: Vec<CheckResult>,
    pub bloat: Option<BloatReport>,
    pub gaps: Vec<AnalysisGap>,
    pub witnesses: Vec<AnalysisWitness>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AnalysisMetadata {
    pub trace_hash: String,
    pub data_source: String,
    pub optimization_level: Option<String>,
    pub compiler_commit: String,
    pub target: String,
    pub input_path: String,
    pub trace_schema_version: u32,
    pub query_pack_id: String,
    pub query_pack_version: String,
    pub relation_schema_version: String,
    pub source_confidence: AttributionConfidence,
    pub bytecode_attribution_confidence: AttributionConfidence,
    pub runtime_trace_source: RuntimeTraceSource,
    pub uses_inferred_boundaries: bool,
    pub uses_derived_bytecode_blocks: bool,
    pub uses_runtime_facts: bool,
    pub uses_posthoc_classification: bool,
    pub flags: Vec<String>,
}

impl AnalysisMetadata {
    pub fn from_snapshot(snapshot: &TraceSnapshot) -> Self {
        let metadata = snapshot.metadata();
        let facts = snapshot.facts();
        let coverage = ProvenanceCoverageReport::from_snapshot(snapshot);
        Self {
            trace_hash: snapshot.trace_hash().to_string(),
            data_source: data_source_label(metadata),
            optimization_level: optimization_level(&metadata.flags),
            compiler_commit: metadata.compiler_commit.clone(),
            target: metadata.target.clone(),
            input_path: metadata.input_path.clone(),
            trace_schema_version: metadata.schema_version,
            query_pack_id: ARGOT_STATIC_CHECKS_ID.to_string(),
            query_pack_version: ARGOT_STATIC_CHECKS_VERSION.to_string(),
            relation_schema_version: RELATION_SCHEMA_VERSION.to_string(),
            source_confidence: source_confidence(facts),
            bytecode_attribution_confidence: coverage.confidence,
            runtime_trace_source: runtime_trace_source(facts, metadata.data_source),
            uses_inferred_boundaries: false,
            uses_derived_bytecode_blocks: false,
            uses_runtime_facts: uses_runtime_facts(facts),
            uses_posthoc_classification: uses_posthoc_classification(facts),
            flags: metadata.flags.clone(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RuntimeTraceSource {
    None,
    Revm,
    Imported,
    Fixture,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CheckResult {
    pub check_id: String,
    pub title: String,
    pub status: CheckStatus,
    pub policy: String,
    pub confidence: AttributionConfidence,
    pub summary: String,
    pub witnesses: Vec<WitnessId>,
    pub gaps: Vec<GapId>,
    pub involved_origins: Vec<OriginExportKey>,
    pub involved_boundaries: Vec<BoundaryId>,
    pub evidence: serde_json::Value,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckStatus {
    Pass,
    Fail,
    Warning,
    Inconclusive,
    NotApplicable,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AnalysisGap {
    pub id: GapId,
    pub kind: AnalysisGapKind,
    pub summary: String,
    pub involved_origins: Vec<OriginExportKey>,
    pub suggested_next_fact: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AnalysisGapKind {
    MissingBytecodeAttribution,
    MissingSourceAttribution,
    AmbiguousSourceAttribution,
    MissingEvidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AnalysisWitness {
    pub id: WitnessId,
    pub kind: AnalysisWitnessKind,
    pub summary: String,
    pub involved_origins: Vec<OriginExportKey>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AnalysisWitnessKind {
    ExactSourcePath,
    SyntheticBackendPath,
    UnmappedInstruction,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BloatReport {
    pub findings: Vec<BloatFinding>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BloatFinding {
    pub kind: String,
    pub summary: String,
    pub confidence: AttributionConfidence,
    pub involved_origins: Vec<OriginExportKey>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProvenanceCoverageReport {
    pub code_object: Option<OriginExportKey>,
    pub total_instructions: usize,
    pub exact_source_pcs: usize,
    pub exact_sonatina_pcs: usize,
    pub sonatina_only_pcs: usize,
    pub synthetic_backend_pcs: usize,
    pub ambiguous_pcs: usize,
    pub unmapped_pcs: usize,
    pub confidence: AttributionConfidence,
    pub rows: Vec<ProvenanceCoverageRow>,
}

impl ProvenanceCoverageReport {
    pub fn from_snapshot(snapshot: &TraceSnapshot) -> Self {
        let index = CoverageIndex::new(snapshot);
        let mut rows = Vec::new();
        for instruction in &index.bytecode_instructions {
            rows.push(index.classify(instruction));
        }
        let exact_source_pcs = rows.iter().filter(|row| row.source_candidates == 1).count();
        let ambiguous_pcs = rows.iter().filter(|row| row.source_candidates > 1).count();
        let exact_sonatina_pcs = rows.iter().filter(|row| row.has_sonatina_path).count();
        let sonatina_only_pcs = rows
            .iter()
            .filter(|row| row.has_sonatina_path && row.source_candidates == 0)
            .count();
        let synthetic_backend_pcs = rows
            .iter()
            .filter(|row| row.has_synthetic_or_backend_path)
            .count();
        let unmapped_pcs = rows.iter().filter(|row| row.is_unmapped).count();
        let confidence = if rows.is_empty() {
            Confidence::Unknown
        } else if unmapped_pcs == 0 && ambiguous_pcs == 0 && sonatina_only_pcs == 0 {
            Confidence::Exact
        } else if exact_source_pcs > 0 || exact_sonatina_pcs > 0 || synthetic_backend_pcs > 0 {
            Confidence::Medium
        } else {
            Confidence::Low
        };
        Self {
            code_object: index.primary_code_object,
            total_instructions: rows.len(),
            exact_source_pcs,
            exact_sonatina_pcs,
            sonatina_only_pcs,
            synthetic_backend_pcs,
            ambiguous_pcs,
            unmapped_pcs,
            confidence,
            rows,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProvenanceCoverageRow {
    pub instruction: OriginExportKey,
    pub source_candidates: usize,
    pub has_sonatina_path: bool,
    pub has_synthetic_or_backend_path: bool,
    pub is_unmapped: bool,
    pub confidence: AttributionConfidence,
}

pub fn static_analysis_report(snapshot: &TraceSnapshot) -> StaticAnalysisReport {
    let coverage = ProvenanceCoverageReport::from_snapshot(snapshot);
    let mut gaps = Vec::new();
    if coverage.unmapped_pcs > 0 {
        gaps.push(AnalysisGap {
            id: "gap:provenance_coverage:unmapped_bytecode".to_string(),
            kind: AnalysisGapKind::MissingBytecodeAttribution,
            summary: format!(
                "{} bytecode instruction(s) have no source, Sonatina, synthetic, or backend attribution path",
                coverage.unmapped_pcs
            ),
            involved_origins: coverage
                .rows
                .iter()
                .filter(|row| row.is_unmapped)
                .map(|row| row.instruction.clone())
                .collect(),
            suggested_next_fact: Some(
                "Emit an OriginEdgeFact or InstructionExtentFact linking final bytecode to the owning Sonatina/backend origin"
                    .to_string(),
            ),
        });
    }
    if coverage.ambiguous_pcs > 0 {
        gaps.push(AnalysisGap {
            id: "gap:provenance_coverage:ambiguous_source".to_string(),
            kind: AnalysisGapKind::AmbiguousSourceAttribution,
            summary: format!(
                "{} bytecode instruction(s) map to multiple source candidates under exact attribution",
                coverage.ambiguous_pcs
            ),
            involved_origins: coverage
                .rows
                .iter()
                .filter(|row| row.source_candidates > 1)
                .map(|row| row.instruction.clone())
                .collect(),
            suggested_next_fact: Some(
                "Emit a more specific compiler event or edge label for the many-to-many merge"
                    .to_string(),
            ),
        });
    }
    if coverage.sonatina_only_pcs > 0 {
        gaps.push(AnalysisGap {
            id: "gap:provenance_coverage:sonatina_only".to_string(),
            kind: AnalysisGapKind::MissingSourceAttribution,
            summary: format!(
                "{} bytecode instruction(s) reach Sonatina but not source under exact attribution",
                coverage.sonatina_only_pcs
            ),
            involved_origins: coverage
                .rows
                .iter()
                .filter(|row| row.has_sonatina_path && row.source_candidates == 0)
                .map(|row| row.instruction.clone())
                .collect(),
            suggested_next_fact: Some(
                "Preserve LoweredFrom/EmittedFrom origin edges across the Sonatina optimization boundary"
                    .to_string(),
            ),
        });
    }

    let mut witnesses = Vec::new();
    if let Some(row) = coverage.rows.iter().find(|row| row.source_candidates == 1) {
        witnesses.push(AnalysisWitness {
            id: "witness:provenance_coverage:exact_source".to_string(),
            kind: AnalysisWitnessKind::ExactSourcePath,
            summary: "At least one bytecode instruction has an exact path to one source span"
                .to_string(),
            involved_origins: vec![row.instruction.clone()],
        });
    }
    if let Some(row) = coverage
        .rows
        .iter()
        .find(|row| row.has_synthetic_or_backend_path)
    {
        witnesses.push(AnalysisWitness {
            id: "witness:provenance_coverage:synthetic_backend".to_string(),
            kind: AnalysisWitnessKind::SyntheticBackendPath,
            summary: "At least one bytecode instruction is explicitly marked synthetic/backend"
                .to_string(),
            involved_origins: vec![row.instruction.clone()],
        });
    }
    if let Some(row) = coverage.rows.iter().find(|row| row.is_unmapped) {
        witnesses.push(AnalysisWitness {
            id: "witness:provenance_coverage:unmapped".to_string(),
            kind: AnalysisWitnessKind::UnmappedInstruction,
            summary: "At least one bytecode instruction is currently unattributed".to_string(),
            involved_origins: vec![row.instruction.clone()],
        });
    }

    let status = if coverage.total_instructions == 0 {
        CheckStatus::Inconclusive
    } else if coverage.unmapped_pcs == 0
        && coverage.ambiguous_pcs == 0
        && coverage.sonatina_only_pcs == 0
    {
        CheckStatus::Pass
    } else {
        CheckStatus::Warning
    };
    let check_gap_ids = gaps.iter().map(|gap| gap.id.clone()).collect::<Vec<_>>();
    let check_witness_ids = witnesses
        .iter()
        .map(|witness| witness.id.clone())
        .collect::<Vec<_>>();
    let check = CheckResult {
        check_id: "provenance_coverage".to_string(),
        title: "Provenance coverage".to_string(),
        status,
        policy: "exact_lowered_emitted_source_paths".to_string(),
        confidence: coverage.confidence,
        summary: provenance_coverage_summary(&coverage),
        witnesses: check_witness_ids,
        gaps: check_gap_ids,
        involved_origins: coverage
            .rows
            .iter()
            .map(|row| row.instruction.clone())
            .collect(),
        involved_boundaries: Vec::new(),
        evidence: json!({
            "coverage": coverage,
        }),
    };

    StaticAnalysisReport {
        metadata: AnalysisMetadata::from_snapshot(snapshot),
        checks: vec![check],
        bloat: None,
        gaps,
        witnesses,
    }
}

fn provenance_coverage_summary(coverage: &ProvenanceCoverageReport) -> String {
    if coverage.total_instructions == 0 {
        return "no bytecode instructions were present in the trace".to_string();
    }
    format!(
        "{} instruction(s): {} exact source, {} Sonatina-linked, {} synthetic/backend, {} ambiguous, {} unmapped",
        coverage.total_instructions,
        coverage.exact_source_pcs,
        coverage.exact_sonatina_pcs,
        coverage.synthetic_backend_pcs,
        coverage.ambiguous_pcs,
        coverage.unmapped_pcs
    )
}

struct CoverageIndex<'a> {
    bytecode_instructions: Vec<OriginExportKey>,
    primary_code_object: Option<OriginExportKey>,
    source_spans: BTreeMap<OriginExportKey, &'a SourceSpanFact>,
    edges_by_from: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
}

impl<'a> CoverageIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut bytecode_instructions = BTreeSet::new();
        let mut primary_code_object = None;
        let mut source_spans = BTreeMap::new();
        let mut edges_by_from: BTreeMap<OriginExportKey, Vec<&OriginEdgeFact>> = BTreeMap::new();
        for fact in snapshot.facts() {
            match fact {
                TraceFact::Instruction(instruction)
                    if is_bytecode_instruction(&instruction.instruction) =>
                {
                    bytecode_instructions.insert(instruction.instruction.clone());
                }
                TraceFact::InstructionExtent(extent) => {
                    if is_bytecode_instruction(&extent.instruction) {
                        primary_code_object.get_or_insert_with(|| extent.code_object.clone());
                    }
                }
                TraceFact::SourceSpan(span) => {
                    source_spans.insert(span.origin.clone(), span);
                }
                TraceFact::OriginEdge(edge) => {
                    edges_by_from
                        .entry(edge.from.clone())
                        .or_default()
                        .push(edge);
                }
                _ => {}
            }
        }
        Self {
            bytecode_instructions: bytecode_instructions.into_iter().collect(),
            primary_code_object,
            source_spans,
            edges_by_from,
        }
    }

    fn classify(&self, instruction: &OriginExportKey) -> ProvenanceCoverageRow {
        let reachable = self.reachable(instruction);
        let source_candidates = reachable
            .iter()
            .filter(|key| self.source_spans.contains_key(*key))
            .count();
        let has_sonatina_path = reachable.iter().any(|key| is_sonatina_origin(key));
        let has_synthetic_or_backend_path = self.has_synthetic_or_backend_path(instruction);
        let is_unmapped =
            source_candidates == 0 && !has_sonatina_path && !has_synthetic_or_backend_path;
        let confidence = if source_candidates == 1 {
            Confidence::Exact
        } else if source_candidates > 1 {
            Confidence::Low
        } else if has_sonatina_path || has_synthetic_or_backend_path {
            Confidence::Medium
        } else {
            Confidence::Unknown
        };
        ProvenanceCoverageRow {
            instruction: instruction.clone(),
            source_candidates,
            has_sonatina_path,
            has_synthetic_or_backend_path,
            is_unmapped,
            confidence,
        }
    }

    fn reachable(&self, root: &OriginExportKey) -> BTreeSet<OriginExportKey> {
        let mut reachable = BTreeSet::new();
        let mut stack = vec![root.clone()];
        while let Some(key) = stack.pop() {
            if !reachable.insert(key.clone()) {
                continue;
            }
            if let Some(edges) = self.edges_by_from.get(&key) {
                for edge in edges {
                    if exact_attribution_label(edge.label) {
                        stack.push(edge.to.clone());
                    }
                }
            }
        }
        reachable.remove(root);
        reachable
    }

    fn has_synthetic_or_backend_path(&self, root: &OriginExportKey) -> bool {
        let mut seen = BTreeSet::new();
        let mut stack = vec![root.clone()];
        while let Some(key) = stack.pop() {
            if !seen.insert(key.clone()) {
                continue;
            }
            if let Some(edges) = self.edges_by_from.get(&key) {
                for edge in edges {
                    if matches!(
                        edge.label,
                        OriginEdgeLabel::SyntheticFor | OriginEdgeLabel::BackendPrepared
                    ) {
                        return true;
                    }
                    if exact_attribution_label(edge.label) {
                        stack.push(edge.to.clone());
                    }
                }
            }
        }
        false
    }
}

fn exact_attribution_label(label: OriginEdgeLabel) -> bool {
    matches!(
        label,
        OriginEdgeLabel::LoweredFrom | OriginEdgeLabel::EmittedFrom
    )
}

fn is_bytecode_instruction(key: &OriginExportKey) -> bool {
    matches!(key.kind(), "bytecode.pc" | "bytecode.inst")
}

fn is_sonatina_origin(key: &OriginExportKey) -> bool {
    key.kind().starts_with("sonatina.")
}

fn source_confidence(facts: &[TraceFact]) -> Confidence {
    if facts
        .iter()
        .any(|fact| matches!(fact, TraceFact::SourceSpan(_)))
    {
        Confidence::High
    } else {
        Confidence::Unknown
    }
}

fn runtime_trace_source(facts: &[TraceFact], data_source: TraceDataSource) -> RuntimeTraceSource {
    if data_source == TraceDataSource::Fixture
        && facts.iter().any(|fact| {
            matches!(
                fact,
                TraceFact::ExecutionStep(_) | TraceFact::DynamicGasStep(_)
            )
        })
    {
        return RuntimeTraceSource::Fixture;
    }
    if facts.iter().any(|fact| {
        matches!(
            fact,
            TraceFact::ExecutionStep(_) | TraceFact::DynamicGasStep(_)
        )
    }) {
        RuntimeTraceSource::Imported
    } else {
        RuntimeTraceSource::None
    }
}

fn uses_runtime_facts(facts: &[TraceFact]) -> bool {
    facts.iter().any(|fact| {
        matches!(
            fact,
            TraceFact::ExecutionTraceSession(_)
                | TraceFact::RuntimeCodeObjectBinding(_)
                | TraceFact::ExecutionStep(_)
                | TraceFact::StackSample(_)
                | TraceFact::StorageAccess(_)
                | TraceFact::MemoryAccess(_)
                | TraceFact::Call(_)
                | TraceFact::Log(_)
                | TraceFact::ReturnData(_)
                | TraceFact::Revert(_)
                | TraceFact::PrecompileInvocation(_)
                | TraceFact::Selfdestruct(_)
                | TraceFact::DynamicGasStep(_)
        )
    })
}

fn uses_posthoc_classification(facts: &[TraceFact]) -> bool {
    facts.iter().any(|fact| match fact {
        TraceFact::InstructionCategory(category) => matches!(
            category.source,
            trace_facts::CategorySource::PosthocClassifier { .. }
        ),
        _ => false,
    })
}

fn optimization_level(flags: &[String]) -> Option<String> {
    flags.iter().find_map(|flag| {
        flag.strip_prefix("optimize=")
            .or_else(|| flag.strip_prefix("optimization_level="))
            .or_else(|| flag.strip_prefix("-O"))
            .map(ToOwned::to_owned)
    })
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use trace_facts::{
        CodeObjectFact, CodeObjectKind, InstructionExtentFact, InstructionFact, OriginEdgeFact,
        OriginEdgeLabel, OriginNodeFact, OriginNodeKind, PcRange, SourceFileFact, SourceSpanFact,
        TraceBundle, TraceFact, TraceMetadata, TraceSnapshot,
    };

    use super::{CheckStatus, Confidence, RuntimeTraceSource, static_analysis_report};

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(key: OriginExportKey) -> TraceFact {
        TraceFact::OriginNode(OriginNodeFact::new(
            key.clone(),
            OriginNodeKind::new(key.kind()),
        ))
    }

    fn snapshot(facts: Vec<TraceFact>) -> TraceSnapshot {
        TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "dev".to_string(), "trace".to_string()],
                "demo.fe",
                vec!["optimize=2".to_string()],
            ),
            facts,
        ))
        .unwrap()
    }

    #[test]
    fn provenance_coverage_classifies_exact_sonatina_synthetic_ambiguous_and_unmapped() {
        let function = key("function", "demo", "recv");
        let code_object = key("code.object", "demo", "runtime");
        let source_file = key("source.file", "demo", "demo.fe");
        let hir_exact = key("hir.expr", "demo", "expr:exact");
        let hir_alt = key("hir.expr", "demo", "expr:alt");
        let sonatina = key("sonatina.post.inst", "demo", "inst:0");
        let exact = key("bytecode.pc", "demo", "pc:0");
        let ambiguous = key("bytecode.pc", "demo", "pc:1");
        let sonatina_only = key("bytecode.pc", "demo", "pc:2");
        let synthetic = key("bytecode.pc", "demo", "pc:3");
        let unmapped = key("bytecode.pc", "demo", "pc:4");
        let facts = vec![
            node(function.clone()),
            node(code_object.clone()),
            node(source_file.clone()),
            node(hir_exact.clone()),
            node(hir_alt.clone()),
            node(sonatina.clone()),
            node(exact.clone()),
            node(ambiguous.clone()),
            node(sonatina_only.clone()),
            node(synthetic.clone()),
            node(unmapped.clone()),
            TraceFact::SourceFile(SourceFileFact::new(
                source_file.clone(),
                "file:///demo.fe",
                "demo.fe",
                "blake3:0000000000000000000000000000000000000000000000000000000000000001",
                Some(0),
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                hir_exact.clone(),
                source_file.clone(),
                0,
                1,
                1,
                1,
                1,
                2,
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                hir_alt.clone(),
                source_file,
                2,
                3,
                1,
                3,
                1,
                4,
            )),
            TraceFact::CodeObject(CodeObjectFact::new(
                code_object.clone(),
                CodeObjectKind::EvmRuntimeBytecode,
                Some(function.clone()),
                "evm/sonatina",
                None,
            )),
            TraceFact::Instruction(InstructionFact::new(
                exact.clone(),
                function.clone(),
                0,
                "ADD",
            )),
            TraceFact::Instruction(InstructionFact::new(
                ambiguous.clone(),
                function.clone(),
                1,
                "MUL",
            )),
            TraceFact::Instruction(InstructionFact::new(
                sonatina_only.clone(),
                function.clone(),
                2,
                "SUB",
            )),
            TraceFact::Instruction(InstructionFact::new(
                synthetic.clone(),
                function.clone(),
                3,
                "DUP1",
            )),
            TraceFact::Instruction(InstructionFact::new(unmapped.clone(), function, 4, "POP")),
            TraceFact::InstructionExtent(InstructionExtentFact::new(
                exact.clone(),
                code_object.clone(),
                PcRange::new(0, 1),
                1,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                exact,
                hir_exact.clone(),
                OriginEdgeLabel::LoweredFrom,
                None,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                ambiguous.clone(),
                hir_exact,
                OriginEdgeLabel::LoweredFrom,
                None,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                ambiguous,
                hir_alt,
                OriginEdgeLabel::LoweredFrom,
                None,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                sonatina_only,
                sonatina,
                OriginEdgeLabel::EmittedFrom,
                None,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                synthetic,
                code_object,
                OriginEdgeLabel::BackendPrepared,
                None,
            )),
        ];

        let report = static_analysis_report(&snapshot(facts));
        let check = &report.checks[0];
        let coverage = &check.evidence["coverage"];

        assert_eq!(check.status, CheckStatus::Warning);
        assert_eq!(coverage["total_instructions"], 5);
        assert_eq!(coverage["exact_source_pcs"], 1);
        assert_eq!(coverage["ambiguous_pcs"], 1);
        assert_eq!(coverage["exact_sonatina_pcs"], 1);
        assert_eq!(coverage["sonatina_only_pcs"], 1);
        assert_eq!(coverage["synthetic_backend_pcs"], 1);
        assert_eq!(coverage["unmapped_pcs"], 1);
        assert_eq!(report.metadata.optimization_level.as_deref(), Some("2"));
        assert_eq!(
            report.metadata.bytecode_attribution_confidence,
            Confidence::Medium
        );
        assert!(report.gaps.iter().any(|gap| gap.id.contains("unmapped")));
        assert!(report.gaps.iter().any(|gap| gap.id.contains("ambiguous")));
        assert!(
            report
                .gaps
                .iter()
                .any(|gap| gap.id.contains("sonatina_only"))
        );
    }

    #[test]
    fn empty_bytecode_trace_is_inconclusive_not_pass() {
        let source_file = key("source.file", "demo", "demo.fe");
        let facts = vec![
            node(source_file.clone()),
            TraceFact::SourceFile(SourceFileFact::new(
                source_file,
                "file:///demo.fe",
                "demo.fe",
                "blake3:0000000000000000000000000000000000000000000000000000000000000001",
                Some(0),
            )),
        ];

        let report = static_analysis_report(&snapshot(facts));

        assert_eq!(report.checks[0].status, CheckStatus::Inconclusive);
        assert_eq!(
            report.metadata.runtime_trace_source,
            RuntimeTraceSource::None
        );
        assert_eq!(
            report.metadata.bytecode_attribution_confidence,
            Confidence::Unknown
        );
    }
}
