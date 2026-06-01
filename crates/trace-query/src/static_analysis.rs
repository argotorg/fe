use std::collections::{BTreeMap, BTreeSet};

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};
use serde_json::json;
use trace_facts::{
    CompilerEventKind, OriginEdgeFact, OriginEdgeTraversalClass, TraceDataSource, TraceFact,
    TraceSnapshot,
};

use crate::{
    Confidence, data_source_label,
    trace_index::{TraceIndex, TraceReachabilityPolicy},
};

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
    PostOptAttributionGap,
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
    PostOptBytecodeAttribution,
    UnmappedInstruction,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BloatReport {
    pub total_instructions: usize,
    pub total_byte_len: u64,
    pub total_static_gas: u64,
    pub attribution_split: BloatAttributionSplit,
    pub findings: Vec<BloatFinding>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BloatFinding {
    pub kind: String,
    pub summary: String,
    pub confidence: AttributionConfidence,
    pub involved_origins: Vec<OriginExportKey>,
    pub instruction_count: usize,
    pub byte_len: u64,
    pub static_gas: u64,
    pub attribution_split: BloatAttributionSplit,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct BloatAttributionSplit {
    pub source_exact: BloatAttributionCost,
    pub source_ambiguous: BloatAttributionCost,
    pub optimized_sonatina: BloatAttributionCost,
    pub prepared_sonatina: BloatAttributionCost,
    pub synthetic_backend: BloatAttributionCost,
    pub unmapped: BloatAttributionCost,
}

impl BloatAttributionSplit {
    fn add(&mut self, primary: ProvenanceCoveragePrimary, byte_len: u64, static_gas: u64) {
        let cost = match primary {
            ProvenanceCoveragePrimary::SourceExact => &mut self.source_exact,
            ProvenanceCoveragePrimary::SourceAmbiguous => &mut self.source_ambiguous,
            ProvenanceCoveragePrimary::OptimizedSonatina => &mut self.optimized_sonatina,
            ProvenanceCoveragePrimary::PreparedSonatina => &mut self.prepared_sonatina,
            ProvenanceCoveragePrimary::SyntheticBackend => &mut self.synthetic_backend,
            ProvenanceCoveragePrimary::Unmapped => &mut self.unmapped,
        };
        cost.instruction_count += 1;
        cost.byte_len += byte_len;
        cost.static_gas += static_gas;
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct BloatAttributionCost {
    pub instruction_count: usize,
    pub byte_len: u64,
    pub static_gas: u64,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProvenanceCoverageReport {
    pub code_object: Option<OriginExportKey>,
    pub total_instructions: usize,
    pub primary_source_exact_pcs: usize,
    pub primary_source_ambiguous_pcs: usize,
    pub primary_optimized_sonatina_pcs: usize,
    pub primary_prepared_sonatina_pcs: usize,
    pub primary_synthetic_backend_pcs: usize,
    pub primary_unmapped_pcs: usize,
    pub exact_source_pcs: usize,
    pub exact_sonatina_pcs: usize,
    pub optimized_sonatina_pcs: usize,
    pub prepared_sonatina_pcs: usize,
    pub sonatina_only_pcs: usize,
    pub prepared_only_pcs: usize,
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
        let primary_source_exact_pcs = rows
            .iter()
            .filter(|row| row.primary == ProvenanceCoveragePrimary::SourceExact)
            .count();
        let primary_source_ambiguous_pcs = rows
            .iter()
            .filter(|row| row.primary == ProvenanceCoveragePrimary::SourceAmbiguous)
            .count();
        let primary_optimized_sonatina_pcs = rows
            .iter()
            .filter(|row| row.primary == ProvenanceCoveragePrimary::OptimizedSonatina)
            .count();
        let primary_prepared_sonatina_pcs = rows
            .iter()
            .filter(|row| row.primary == ProvenanceCoveragePrimary::PreparedSonatina)
            .count();
        let primary_synthetic_backend_pcs = rows
            .iter()
            .filter(|row| row.primary == ProvenanceCoveragePrimary::SyntheticBackend)
            .count();
        let primary_unmapped_pcs = rows
            .iter()
            .filter(|row| row.primary == ProvenanceCoveragePrimary::Unmapped)
            .count();
        let exact_source_pcs = rows.iter().filter(|row| row.source_candidates == 1).count();
        let ambiguous_pcs = rows.iter().filter(|row| row.source_candidates > 1).count();
        let optimized_sonatina_pcs = rows
            .iter()
            .filter(|row| row.has_optimized_sonatina_path)
            .count();
        let prepared_sonatina_pcs = rows
            .iter()
            .filter(|row| row.has_prepared_sonatina_path)
            .count();
        let exact_sonatina_pcs = optimized_sonatina_pcs;
        let sonatina_only_pcs = rows
            .iter()
            .filter(|row| row.has_optimized_sonatina_path && row.source_candidates == 0)
            .count();
        let prepared_only_pcs = rows
            .iter()
            .filter(|row| {
                row.has_prepared_sonatina_path
                    && !row.has_optimized_sonatina_path
                    && row.source_candidates == 0
            })
            .count();
        let synthetic_backend_pcs = rows
            .iter()
            .filter(|row| row.has_synthetic_or_backend_path)
            .count();
        let unmapped_pcs = rows.iter().filter(|row| row.is_unmapped).count();
        let confidence = if rows.is_empty() {
            Confidence::Unknown
        } else if unmapped_pcs == 0
            && ambiguous_pcs == 0
            && sonatina_only_pcs == 0
            && prepared_only_pcs == 0
        {
            Confidence::Exact
        } else if exact_source_pcs > 0 || exact_sonatina_pcs > 0 || synthetic_backend_pcs > 0 {
            Confidence::Medium
        } else {
            Confidence::Low
        };
        Self {
            code_object: index.primary_code_object,
            total_instructions: rows.len(),
            primary_source_exact_pcs,
            primary_source_ambiguous_pcs,
            primary_optimized_sonatina_pcs,
            primary_prepared_sonatina_pcs,
            primary_synthetic_backend_pcs,
            primary_unmapped_pcs,
            exact_source_pcs,
            exact_sonatina_pcs,
            optimized_sonatina_pcs,
            prepared_sonatina_pcs,
            sonatina_only_pcs,
            prepared_only_pcs,
            synthetic_backend_pcs,
            ambiguous_pcs,
            unmapped_pcs,
            confidence,
            rows,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ProvenanceCoveragePrimary {
    SourceExact,
    SourceAmbiguous,
    OptimizedSonatina,
    PreparedSonatina,
    SyntheticBackend,
    Unmapped,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProvenanceCoverageRow {
    pub instruction: OriginExportKey,
    pub primary: ProvenanceCoveragePrimary,
    pub source_candidates: usize,
    pub has_sonatina_path: bool,
    pub has_optimized_sonatina_path: bool,
    pub has_prepared_sonatina_path: bool,
    pub has_synthetic_or_backend_path: bool,
    pub is_unmapped: bool,
    pub confidence: AttributionConfidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PostOptAttributionGapReport {
    pub total_postopt_origins: usize,
    pub bytecode_attributed_origins: usize,
    pub explicitly_explained_origins: usize,
    pub gap_count: usize,
    pub confidence: AttributionConfidence,
    pub gaps: Vec<PostOptAttributionGap>,
}

impl PostOptAttributionGapReport {
    pub fn from_snapshot(snapshot: &TraceSnapshot) -> Self {
        let index = TraceIndex::new(snapshot);
        let postopt_origins = snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::OriginNode(node) if is_sonatina_post_origin(&node.key) => {
                    Some(node.key.clone())
                }
                _ => None,
            })
            .collect::<BTreeSet<_>>();
        let bytecode_instructions = snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::Instruction(instruction)
                    if is_bytecode_instruction(&instruction.instruction) =>
                {
                    Some(instruction.instruction.clone())
                }
                _ => None,
            })
            .collect::<BTreeSet<_>>();
        let mut bytecode_attributed = BTreeSet::new();
        for instruction in &bytecode_instructions {
            for target in index.reachable_targets(instruction, TraceReachabilityPolicy::ExactOnly) {
                if postopt_origins.contains(&target) {
                    bytecode_attributed.insert(target);
                }
            }
        }
        let explicitly_explained = explicit_postopt_explanations(snapshot);
        let gaps = postopt_origins
            .iter()
            .filter(|origin| !bytecode_attributed.contains(*origin))
            .filter(|origin| !explicitly_explained.contains(*origin))
            .map(|origin| PostOptAttributionGap {
                sonatina_origin: origin.clone(),
                reason: "missing exact post-opt Sonatina to final bytecode attribution".to_string(),
                suggested_next_fact:
                    "Emit an exact bytecode OriginEdgeFact/InstructionExtentFact or an optimizer elision/coalescing CompilerEventFact"
                        .to_string(),
            })
            .collect::<Vec<_>>();
        let confidence = if postopt_origins.is_empty() {
            Confidence::Unknown
        } else if gaps.is_empty() {
            Confidence::High
        } else if !bytecode_attributed.is_empty() {
            Confidence::Medium
        } else {
            Confidence::Low
        };
        Self {
            total_postopt_origins: postopt_origins.len(),
            bytecode_attributed_origins: bytecode_attributed.len(),
            explicitly_explained_origins: explicitly_explained
                .intersection(&postopt_origins)
                .count(),
            gap_count: gaps.len(),
            confidence,
            gaps,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PostOptAttributionGap {
    pub sonatina_origin: OriginExportKey,
    pub reason: String,
    pub suggested_next_fact: String,
}

pub fn static_analysis_report(snapshot: &TraceSnapshot) -> StaticAnalysisReport {
    let coverage = ProvenanceCoverageReport::from_snapshot(snapshot);
    let postopt = PostOptAttributionGapReport::from_snapshot(snapshot);
    let bloat = bytecode_bloat_report(snapshot);
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
                "{} bytecode instruction(s) reach optimized Sonatina but not source under exact attribution",
                coverage.sonatina_only_pcs
            ),
            involved_origins: coverage
                .rows
                .iter()
                .filter(|row| row.has_optimized_sonatina_path && row.source_candidates == 0)
                .map(|row| row.instruction.clone())
                .collect(),
            suggested_next_fact: Some(
                "Preserve LoweredFrom/EmittedFrom origin edges across the Sonatina optimization boundary"
                    .to_string(),
            ),
        });
    }
    if coverage.prepared_only_pcs > 0 {
        gaps.push(AnalysisGap {
            id: "gap:provenance_coverage:prepared_only".to_string(),
            kind: AnalysisGapKind::MissingSourceAttribution,
            summary: format!(
                "{} bytecode instruction(s) are prepared-linked but not source-exact",
                coverage.prepared_only_pcs
            ),
            involved_origins: coverage
                .rows
                .iter()
                .filter(|row| {
                    row.has_prepared_sonatina_path
                        && !row.has_optimized_sonatina_path
                        && row.source_candidates == 0
                })
                .map(|row| row.instruction.clone())
                .collect(),
            suggested_next_fact: Some(
                "Emit explicit optimized Sonatina → EVM prepared lineage, then classify that edge by exact/generated/context semantics"
                    .to_string(),
            ),
        });
    }
    if postopt.gap_count > 0 {
        gaps.push(AnalysisGap {
            id: "gap:postopt_attribution:missing_bytecode".to_string(),
            kind: AnalysisGapKind::PostOptAttributionGap,
            summary: format!(
                "{} optimized Sonatina origin(s) do not reach final bytecode and lack an explicit optimizer explanation",
                postopt.gap_count
            ),
            involved_origins: postopt
                .gaps
                .iter()
                .map(|gap| gap.sonatina_origin.clone())
                .collect(),
            suggested_next_fact: Some(
                "Emit post-opt to bytecode edges, instruction extents, or optimizer rewrite/elision events"
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
    if postopt.bytecode_attributed_origins > 0 {
        witnesses.push(AnalysisWitness {
            id: "witness:postopt_attribution:bytecode_edge".to_string(),
            kind: AnalysisWitnessKind::PostOptBytecodeAttribution,
            summary: "At least one optimized Sonatina origin is reached from final bytecode under exact attribution"
                .to_string(),
            involved_origins: Vec::new(),
        });
    }

    let coverage_status = if coverage.total_instructions == 0 {
        CheckStatus::Inconclusive
    } else if coverage.unmapped_pcs == 0
        && coverage.ambiguous_pcs == 0
        && coverage.sonatina_only_pcs == 0
    {
        CheckStatus::Pass
    } else {
        CheckStatus::Warning
    };
    let postopt_status = if postopt.total_postopt_origins == 0 {
        CheckStatus::Inconclusive
    } else if postopt.gap_count == 0 {
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
        status: coverage_status,
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
    let postopt_gap_ids = gaps
        .iter()
        .filter(|gap| gap.kind == AnalysisGapKind::PostOptAttributionGap)
        .map(|gap| gap.id.clone())
        .collect::<Vec<_>>();
    let postopt_witness_ids = witnesses
        .iter()
        .filter(|witness| witness.kind == AnalysisWitnessKind::PostOptBytecodeAttribution)
        .map(|witness| witness.id.clone())
        .collect::<Vec<_>>();
    let postopt_check = CheckResult {
        check_id: "postopt_attribution_gap".to_string(),
        title: "Post-opt attribution gap".to_string(),
        status: postopt_status,
        policy: "exact_bytecode_to_postopt_paths_or_explicit_optimizer_explanation".to_string(),
        confidence: postopt.confidence,
        summary: postopt_gap_summary(&postopt),
        witnesses: postopt_witness_ids,
        gaps: postopt_gap_ids,
        involved_origins: postopt
            .gaps
            .iter()
            .map(|gap| gap.sonatina_origin.clone())
            .collect(),
        involved_boundaries: Vec::new(),
        evidence: json!({
            "postopt_attribution": postopt,
        }),
    };

    StaticAnalysisReport {
        metadata: AnalysisMetadata::from_snapshot(snapshot),
        checks: vec![check, postopt_check],
        bloat: Some(bloat),
        gaps,
        witnesses,
    }
}

fn postopt_gap_summary(report: &PostOptAttributionGapReport) -> String {
    if report.total_postopt_origins == 0 {
        return "no optimized Sonatina origins were present in the trace".to_string();
    }
    format!(
        "{} post-opt origin(s): {} bytecode-attributed, {} explicitly explained, {} gap(s)",
        report.total_postopt_origins,
        report.bytecode_attributed_origins,
        report.explicitly_explained_origins,
        report.gap_count
    )
}

fn bytecode_bloat_report(snapshot: &TraceSnapshot) -> BloatReport {
    let coverage_index = CoverageIndex::new(snapshot);
    let mut byte_len_by_instruction = BTreeMap::<OriginExportKey, u64>::new();
    let mut static_gas_by_instruction = BTreeMap::<OriginExportKey, u64>::new();
    for fact in snapshot.facts() {
        match fact {
            TraceFact::InstructionExtent(extent)
                if is_bytecode_instruction(&extent.instruction) =>
            {
                byte_len_by_instruction
                    .insert(extent.instruction.clone(), u64::from(extent.byte_len));
            }
            TraceFact::StaticGas(gas) if is_bytecode_instruction(&gas.instruction) => {
                static_gas_by_instruction
                    .entry(gas.instruction.clone())
                    .and_modify(|total| *total += gas.base_cost)
                    .or_insert(gas.base_cost);
            }
            _ => {}
        }
    }

    let mut buckets = BTreeMap::<BloatBucketKind, BloatBucket>::new();
    let mut total_instructions = 0;
    let mut total_byte_len = 0;
    let mut total_static_gas = 0;
    let mut attribution_split = BloatAttributionSplit::default();
    for fact in snapshot.facts() {
        let TraceFact::Instruction(instruction) = fact else {
            continue;
        };
        if !is_bytecode_instruction(&instruction.instruction) {
            continue;
        }
        total_instructions += 1;
        let attribution = coverage_index.classify(&instruction.instruction).primary;
        let byte_len = byte_len_by_instruction
            .get(&instruction.instruction)
            .copied()
            .unwrap_or(0);
        let static_gas = static_gas_by_instruction
            .get(&instruction.instruction)
            .copied()
            .unwrap_or(0);
        total_byte_len += byte_len;
        total_static_gas += static_gas;
        attribution_split.add(attribution, byte_len, static_gas);
        for kind in bloat_bucket_kinds(&instruction.mnemonic) {
            let bucket = buckets.entry(kind).or_default();
            bucket.instructions.push(instruction.instruction.clone());
            bucket.byte_len += byte_len;
            bucket.static_gas += static_gas;
            bucket
                .attribution_split
                .add(attribution, byte_len, static_gas);
        }
    }

    let mut findings = buckets
        .into_iter()
        .map(|(kind, bucket)| BloatFinding {
            kind: kind.as_str().to_string(),
            summary: kind.summary(
                bucket.instructions.len(),
                bucket.byte_len,
                bucket.static_gas,
            ),
            confidence: Confidence::Medium,
            instruction_count: bucket.instructions.len(),
            byte_len: bucket.byte_len,
            static_gas: bucket.static_gas,
            attribution_split: bucket.attribution_split,
            involved_origins: bucket.instructions,
        })
        .collect::<Vec<_>>();
    findings.sort_by(|left, right| {
        right
            .instruction_count
            .cmp(&left.instruction_count)
            .then_with(|| right.byte_len.cmp(&left.byte_len))
            .then_with(|| left.kind.cmp(&right.kind))
    });
    BloatReport {
        total_instructions,
        total_byte_len,
        total_static_gas,
        attribution_split,
        findings,
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct BloatBucket {
    instructions: Vec<OriginExportKey>,
    byte_len: u64,
    static_gas: u64,
    attribution_split: BloatAttributionSplit,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum BloatBucketKind {
    StackShuffle,
    ControlFlowOverhead,
    IntegerNormalization,
    MemoryScratch,
    LiteralChurn,
}

impl BloatBucketKind {
    const fn as_str(self) -> &'static str {
        match self {
            Self::StackShuffle => "stack_shuffle_tax",
            Self::ControlFlowOverhead => "control_flow_overhead",
            Self::IntegerNormalization => "integer_normalization_tax",
            Self::MemoryScratch => "memory_scratch_tax",
            Self::LiteralChurn => "literal_churn",
        }
    }

    fn summary(self, instructions: usize, byte_len: u64, static_gas: u64) -> String {
        format!(
            "{} candidate instruction(s), {byte_len} byte(s), {static_gas} static gas; conservative taxonomy, not proof of waste",
            instructions
        )
    }
}

fn bloat_bucket_kinds(mnemonic: &str) -> Vec<BloatBucketKind> {
    let opcode = mnemonic
        .split_whitespace()
        .next()
        .unwrap_or(mnemonic)
        .to_ascii_uppercase();
    let mut kinds = Vec::new();
    if opcode.starts_with("DUP") || opcode.starts_with("SWAP") || opcode == "POP" {
        kinds.push(BloatBucketKind::StackShuffle);
    }
    if matches!(
        opcode.as_str(),
        "JUMP" | "JUMPI" | "JUMPDEST" | "RJUMP" | "RJUMPI"
    ) {
        kinds.push(BloatBucketKind::ControlFlowOverhead);
    }
    if matches!(
        opcode.as_str(),
        "AND" | "SIGNEXTEND" | "SHL" | "SHR" | "SAR" | "BYTE" | "ISZERO"
    ) {
        kinds.push(BloatBucketKind::IntegerNormalization);
    }
    if matches!(
        opcode.as_str(),
        "MLOAD" | "MSTORE" | "MSTORE8" | "MCOPY" | "CALLDATACOPY" | "CODECOPY" | "RETURNDATACOPY"
    ) {
        kinds.push(BloatBucketKind::MemoryScratch);
    }
    if opcode.starts_with("PUSH") {
        kinds.push(BloatBucketKind::LiteralChurn);
    }
    kinds
}

fn provenance_coverage_summary(coverage: &ProvenanceCoverageReport) -> String {
    if coverage.total_instructions == 0 {
        return "no bytecode instructions were present in the trace".to_string();
    }
    format!(
        "{} instruction(s). Primary attribution: {} source-exact, {} ambiguous-source, {} optimized-only, {} prepared-only, {} synthetic/backend-only, {} unmapped. Evidence overlap: {} optimized-linked, {} prepared-linked",
        coverage.total_instructions,
        coverage.primary_source_exact_pcs,
        coverage.primary_source_ambiguous_pcs,
        coverage.primary_optimized_sonatina_pcs,
        coverage.primary_prepared_sonatina_pcs,
        coverage.primary_synthetic_backend_pcs,
        coverage.primary_unmapped_pcs,
        coverage.optimized_sonatina_pcs,
        coverage.prepared_sonatina_pcs
    )
}

struct CoverageIndex<'a> {
    bytecode_instructions: Vec<OriginExportKey>,
    primary_code_object: Option<OriginExportKey>,
    edges_by_from: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
    trace_index: TraceIndex<'a>,
}

impl<'a> CoverageIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut bytecode_instructions = BTreeSet::new();
        let mut primary_code_object = None;
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
            edges_by_from,
            trace_index: TraceIndex::new(snapshot),
        }
    }

    fn classify(&self, instruction: &OriginExportKey) -> ProvenanceCoverageRow {
        let source_candidates = self
            .trace_index
            .source_candidates_for_instruction(instruction, TraceReachabilityPolicy::ExactOnly)
            .len();
        let reachable = self
            .trace_index
            .reachable_targets(instruction, TraceReachabilityPolicy::ExactOnly);
        let has_sonatina_path = reachable.iter().any(is_sonatina_origin);
        let has_optimized_sonatina_path = reachable.iter().any(is_sonatina_post_origin);
        let has_prepared_sonatina_path = reachable.iter().any(is_sonatina_prepared_origin);
        let has_synthetic_or_backend_path = self.has_synthetic_or_backend_path(instruction);
        let primary = if source_candidates == 1 {
            ProvenanceCoveragePrimary::SourceExact
        } else if source_candidates > 1 {
            ProvenanceCoveragePrimary::SourceAmbiguous
        } else if has_optimized_sonatina_path {
            ProvenanceCoveragePrimary::OptimizedSonatina
        } else if has_prepared_sonatina_path {
            ProvenanceCoveragePrimary::PreparedSonatina
        } else if has_synthetic_or_backend_path {
            ProvenanceCoveragePrimary::SyntheticBackend
        } else {
            ProvenanceCoveragePrimary::Unmapped
        };
        let is_unmapped = primary == ProvenanceCoveragePrimary::Unmapped;
        let confidence = match primary {
            ProvenanceCoveragePrimary::SourceExact => Confidence::Exact,
            ProvenanceCoveragePrimary::SourceAmbiguous => Confidence::Low,
            ProvenanceCoveragePrimary::OptimizedSonatina
            | ProvenanceCoveragePrimary::PreparedSonatina
            | ProvenanceCoveragePrimary::SyntheticBackend => Confidence::Medium,
            ProvenanceCoveragePrimary::Unmapped => Confidence::Unknown,
        };
        ProvenanceCoverageRow {
            instruction: instruction.clone(),
            primary,
            source_candidates,
            has_sonatina_path,
            has_optimized_sonatina_path,
            has_prepared_sonatina_path,
            has_synthetic_or_backend_path,
            is_unmapped,
            confidence,
        }
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
                    let class = edge.traversal_class();
                    if matches!(
                        class,
                        OriginEdgeTraversalClass::Synthetic | OriginEdgeTraversalClass::Contextual
                    ) {
                        return true;
                    }
                    if matches!(
                        class,
                        OriginEdgeTraversalClass::ExactAttribution
                            | OriginEdgeTraversalClass::SnapshotAlias
                    ) {
                        stack.push(edge.to.clone());
                    }
                }
            }
        }
        false
    }
}

fn is_bytecode_instruction(key: &OriginExportKey) -> bool {
    matches!(key.kind(), "bytecode.pc" | "bytecode.inst")
}

fn is_sonatina_origin(key: &OriginExportKey) -> bool {
    key.kind().starts_with("sonatina.")
}

fn is_sonatina_post_origin(key: &OriginExportKey) -> bool {
    key.kind().starts_with("sonatina.post")
}

fn is_sonatina_prepared_origin(key: &OriginExportKey) -> bool {
    key.kind().starts_with("sonatina.evm.prepared")
}

fn explicit_postopt_explanations(snapshot: &TraceSnapshot) -> BTreeSet<OriginExportKey> {
    snapshot
        .facts()
        .iter()
        .filter_map(|fact| match fact {
            TraceFact::CompilerEvent(event)
                if matches!(
                    event.kind,
                    CompilerEventKind::OptimizerElidedOrRewritten
                        | CompilerEventKind::OptimizerSnapshotJoin
                        | CompilerEventKind::OptimizerCreated
                ) =>
            {
                Some(
                    event
                        .inputs
                        .iter()
                        .chain(event.outputs.iter())
                        .filter(|key| is_sonatina_post_origin(key))
                        .cloned()
                        .collect::<Vec<_>>(),
                )
            }
            _ => None,
        })
        .flatten()
        .collect()
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
        CodeObjectFact, CodeObjectKind, CompilerEventFact, CompilerEventKind, CompilerPhase,
        EvmSchedule, InstructionExtentFact, InstructionFact, OriginEdgeFact, OriginEdgeLabel,
        OriginNodeFact, OriginNodeKind, PcRange, SourceFileFact, SourceSpanFact, StaticGasFact,
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
        let prepared_inst = key("sonatina.evm.prepared.inst", "demo", "inst:prepared");
        let exact = key("bytecode.pc", "demo", "pc:0");
        let ambiguous = key("bytecode.pc", "demo", "pc:1");
        let sonatina_only = key("bytecode.pc", "demo", "pc:2");
        let synthetic = key("bytecode.pc", "demo", "pc:3");
        let unmapped = key("bytecode.pc", "demo", "pc:4");
        let prepared = key("bytecode.pc", "demo", "pc:5");
        let facts = vec![
            node(function.clone()),
            node(code_object.clone()),
            node(source_file.clone()),
            node(hir_exact.clone()),
            node(hir_alt.clone()),
            node(sonatina.clone()),
            node(prepared_inst.clone()),
            node(exact.clone()),
            node(ambiguous.clone()),
            node(sonatina_only.clone()),
            node(synthetic.clone()),
            node(unmapped.clone()),
            node(prepared.clone()),
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
            TraceFact::Instruction(InstructionFact::new(
                prepared.clone(),
                key("function", "demo", "recv"),
                5,
                "ADD",
            )),
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
            TraceFact::OriginEdge(OriginEdgeFact::new(
                prepared,
                prepared_inst,
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            )),
        ];

        let report = static_analysis_report(&snapshot(facts));
        let check = &report.checks[0];
        let coverage = &check.evidence["coverage"];

        assert_eq!(check.status, CheckStatus::Warning);
        assert_eq!(coverage["total_instructions"], 6);
        assert_eq!(coverage["primary_source_exact_pcs"], 1);
        assert_eq!(coverage["primary_source_ambiguous_pcs"], 1);
        assert_eq!(coverage["primary_optimized_sonatina_pcs"], 1);
        assert_eq!(coverage["primary_prepared_sonatina_pcs"], 1);
        assert_eq!(coverage["primary_synthetic_backend_pcs"], 1);
        assert_eq!(coverage["primary_unmapped_pcs"], 1);
        assert_eq!(coverage["exact_source_pcs"], 1);
        assert_eq!(coverage["ambiguous_pcs"], 1);
        assert_eq!(coverage["exact_sonatina_pcs"], 1);
        assert_eq!(coverage["optimized_sonatina_pcs"], 1);
        assert_eq!(coverage["prepared_sonatina_pcs"], 1);
        assert_eq!(coverage["sonatina_only_pcs"], 1);
        assert_eq!(coverage["prepared_only_pcs"], 1);
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
        assert!(
            report
                .gaps
                .iter()
                .any(|gap| gap.id.contains("prepared_only"))
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

    #[test]
    fn postopt_attribution_gap_reports_unattributed_postopt_origins() {
        let function = key("function", "demo", "recv");
        let bytecode = key("bytecode.pc", "demo", "pc:0");
        let attributed = key("sonatina.post.inst", "demo", "inst:0");
        let gap = key("sonatina.post.inst", "demo", "inst:1");
        let facts = vec![
            node(function.clone()),
            node(bytecode.clone()),
            node(attributed.clone()),
            node(gap.clone()),
            TraceFact::Instruction(InstructionFact::new(bytecode.clone(), function, 0, "ADD")),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                bytecode,
                attributed,
                OriginEdgeLabel::EmittedFrom,
                None,
            )),
        ];

        let report = static_analysis_report(&snapshot(facts));
        let check = report
            .checks
            .iter()
            .find(|check| check.check_id == "postopt_attribution_gap")
            .unwrap();

        assert_eq!(check.status, CheckStatus::Warning);
        assert_eq!(
            check.evidence["postopt_attribution"]["total_postopt_origins"],
            2
        );
        assert_eq!(
            check.evidence["postopt_attribution"]["bytecode_attributed_origins"],
            1
        );
        assert_eq!(check.evidence["postopt_attribution"]["gap_count"], 1);
        assert!(
            report
                .gaps
                .iter()
                .any(|analysis_gap| analysis_gap.involved_origins == vec![gap.clone()])
        );
    }

    #[test]
    fn postopt_attribution_gap_accepts_explicit_optimizer_explanations() {
        let postopt = key("sonatina.post.inst", "demo", "inst:0");
        let event = key("compiler.event", "demo", "optimizer:0");
        let facts = vec![
            node(postopt.clone()),
            node(event.clone()),
            TraceFact::CompilerEvent(CompilerEventFact::new(
                event,
                CompilerPhase::SonatinaPostOpt,
                CompilerEventKind::OptimizerElidedOrRewritten,
                vec![postopt],
                Vec::new(),
                None,
            )),
        ];

        let report = static_analysis_report(&snapshot(facts));
        let check = report
            .checks
            .iter()
            .find(|check| check.check_id == "postopt_attribution_gap")
            .unwrap();

        assert_eq!(check.status, CheckStatus::Pass);
        assert_eq!(
            check.evidence["postopt_attribution"]["explicitly_explained_origins"],
            1
        );
        assert!(check.gaps.is_empty());
    }

    #[test]
    fn bloat_report_groups_conservative_opcode_taxonomy() {
        let function = key("function", "demo", "recv");
        let code_object = key("code.object", "demo", "runtime");
        let source_file = key("source.file", "demo", "demo.fe");
        let hir = key("hir.expr", "demo", "expr:add");
        let instructions = [
            ("pc:0", "DUP1"),
            ("pc:1", "SWAP1"),
            ("pc:2", "AND"),
            ("pc:3", "MSTORE"),
            ("pc:4", "JUMP"),
            ("pc:5", "PUSH1 0x00"),
        ]
        .into_iter()
        .enumerate()
        .map(|(index, (local, mnemonic))| {
            (
                index as u32,
                key("bytecode.pc", "demo", local),
                mnemonic.to_string(),
            )
        })
        .collect::<Vec<_>>();
        let mut facts = vec![
            node(function.clone()),
            node(code_object.clone()),
            node(source_file.clone()),
            node(hir.clone()),
            TraceFact::SourceFile(SourceFileFact::new(
                source_file.clone(),
                "file:///demo.fe",
                "demo.fe",
                "blake3:0000000000000000000000000000000000000000000000000000000000000001",
                Some(0),
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                hir.clone(),
                source_file,
                0,
                1,
                1,
                1,
                1,
                2,
            )),
        ];
        for (_, instruction, _) in &instructions {
            facts.push(node(instruction.clone()));
        }
        for (index, instruction, mnemonic) in &instructions {
            facts.push(TraceFact::Instruction(InstructionFact::new(
                instruction.clone(),
                function.clone(),
                *index,
                mnemonic,
            )));
            facts.push(TraceFact::InstructionExtent(InstructionExtentFact::new(
                instruction.clone(),
                code_object.clone(),
                PcRange::new(*index, *index + 1),
                1,
            )));
            facts.push(TraceFact::StaticGas(StaticGasFact::new(
                instruction.clone(),
                EvmSchedule::new("cancun"),
                3,
                None,
            )));
        }
        facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
            instructions[0].1.clone(),
            hir,
            OriginEdgeLabel::LoweredFrom,
            None,
        )));

        let report = static_analysis_report(&snapshot(facts));
        let bloat = report.bloat.unwrap();

        assert_eq!(bloat.total_instructions, 6);
        assert_eq!(bloat.total_byte_len, 6);
        assert_eq!(bloat.total_static_gas, 18);
        assert_eq!(bloat.attribution_split.source_exact.instruction_count, 1);
        assert_eq!(bloat.attribution_split.unmapped.instruction_count, 5);
        let stack_shuffle = bloat
            .findings
            .iter()
            .find(|finding| finding.kind == "stack_shuffle_tax")
            .unwrap();
        assert_eq!(stack_shuffle.instruction_count, 2);
        assert_eq!(
            stack_shuffle
                .attribution_split
                .source_exact
                .instruction_count,
            1
        );
        assert_eq!(
            stack_shuffle.attribution_split.unmapped.instruction_count,
            1
        );
        assert!(bloat.findings.iter().any(|finding| {
            finding.kind == "integer_normalization_tax" && finding.instruction_count == 1
        }));
        assert!(bloat.findings.iter().any(|finding| {
            finding.kind == "memory_scratch_tax" && finding.instruction_count == 1
        }));
        assert!(bloat.findings.iter().any(|finding| {
            finding.kind == "control_flow_overhead" && finding.instruction_count == 1
        }));
        assert!(
            bloat.findings.iter().any(|finding| {
                finding.kind == "literal_churn" && finding.instruction_count == 1
            })
        );
    }
}
