use std::collections::{BTreeMap, BTreeSet, VecDeque};

use common::origin::OriginExportKey;
use serde::Serialize;
use trace_facts::{
    InstructionFact, OriginEdgeFact, OriginEdgeLabel, SourceSpanFact, TraceFact, TraceSnapshot,
};

use crate::LoopContentsReport;

const CLOSURE_MAX_DEPTH: usize = 16;
const CLOSURE_MAX_NODES: usize = 512;
const CLOSURE_DEGREE_HUB_THRESHOLD: usize = 128;

#[derive(Debug, Serialize)]
pub struct OriginClosureSet {
    pub closures: Vec<OriginClosure>,
    pub edge_count: usize,
    pub instruction_count: usize,
    pub source_span_count: usize,
}

#[derive(Clone, Debug, Serialize)]
pub struct OriginClosure {
    pub class_name: String,
    pub label: String,
    pub root_key: String,
    #[serde(skip_serializing)]
    pub keys: Vec<String>,
    pub counts: OriginClosureCounts,
    pub traversal: OriginClosureTraversalSummary,
    pub gap: Option<String>,
    pub edges: Vec<OriginClosureEdge>,
    pub source_spans: Vec<OriginClosureSourceSpan>,
}

#[derive(Clone, Debug, Serialize)]
pub struct OriginClosureCounts {
    pub hir: usize,
    pub mir: usize,
    pub sonatina_pre: usize,
    pub sonatina_post: usize,
    pub bytecode: usize,
}

#[derive(Clone, Debug, Serialize)]
pub struct OriginClosureTraversalSummary {
    pub mode: String,
    pub max_depth: usize,
    pub max_nodes: usize,
    pub truncated: bool,
    pub truncation_reason: Option<String>,
    pub skipped_hubs: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct OriginClosureEdge {
    pub label: String,
    pub from: String,
    pub to: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct OriginClosureSourceSpan {
    pub origin: String,
    pub file: String,
    #[serde(skip_serializing)]
    pub file_owner: String,
    pub lines: String,
    pub start_byte: u32,
    pub end_byte: u32,
    pub start_line: u32,
    pub end_line: u32,
    pub confidence: String,
}

#[derive(Clone, Debug)]
pub struct OriginClosureSourceLine {
    pub number: u32,
    pub text: String,
}

#[derive(Debug, Serialize)]
pub struct ClosureAuditReport {
    pub input_path: String,
    pub target: String,
    pub data_source: String,
    pub total_closures: usize,
    pub span_groups: ClosureAuditSpanGroupSummary,
    pub span_group_details: Vec<ClosureAuditSpanGroupDetail>,
    pub primary_counts: BTreeMap<ClosureAuditPrimary, usize>,
    pub symptom_counts: BTreeMap<ClosureAuditSymptom, usize>,
    pub suspicious_closures: usize,
    pub closures: Vec<ClosureAuditEntry>,
}

#[derive(Debug, Serialize)]
pub struct ClosureAuditSpanGroupSummary {
    pub total_groups: usize,
    pub mixed_connectivity_groups: usize,
}

#[derive(Debug, Serialize)]
pub struct ClosureAuditSpanGroupDetail {
    pub file_owner: String,
    pub start_byte: u32,
    pub end_byte: u32,
    pub source_text: Option<String>,
    pub closures: usize,
    pub closures_with_targets: usize,
    pub source_only_closures: usize,
    pub target_connected_members: Vec<ClosureAuditSpanGroupMember>,
    pub source_only_members: Vec<ClosureAuditSpanGroupMember>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct ClosureAuditSpanGroupMember {
    pub class_name: String,
    pub label: String,
    pub root_key: String,
    pub highest_phase_reached: ClosureAuditPhase,
}

#[derive(Debug, Serialize)]
pub struct ClosureAuditEntry {
    pub class_name: String,
    pub label: String,
    pub root_key: String,
    pub primary: ClosureAuditPrimary,
    pub symptoms: Vec<ClosureAuditSymptom>,
    pub suspicious: bool,
    pub highest_phase_reached: ClosureAuditPhase,
    pub counts: OriginClosureCounts,
    pub traversal: OriginClosureTraversalSummary,
    pub key_count: usize,
    pub edge_count: usize,
    pub source_spans: Vec<String>,
    pub notes: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ClosureAuditPhase {
    Source,
    Hir,
    Mir,
    SonatinaPre,
    SonatinaPost,
    Bytecode,
}

impl ClosureAuditPhase {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Source => "source",
            Self::Hir => "hir",
            Self::Mir => "mir",
            Self::SonatinaPre => "sonatina_pre",
            Self::SonatinaPost => "sonatina_post",
            Self::Bytecode => "bytecode",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ClosureAuditPrimary {
    GoodExact,
    GoodManyToMany,
    SourceOnlyExpected,
    SourceSpanSiblingUnlowered,
    ExpectedSynthetic,
    LoweredNoTargetUnexplained,
    PreoptElisionGap,
    OptimizedAttributionGap,
    MissingSourceUnexplained,
    TruncatedUnknown,
    Unclassified,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ClosureAuditSymptom {
    TooBroad,
    MissingSourceUnexplained,
    SourceUnavailableSynthetic,
    MissingBytecode,
    ForeignSource,
    Truncated,
}

impl ClosureAuditPrimary {
    fn is_suspicious(self) -> bool {
        matches!(
            self,
            Self::OptimizedAttributionGap
                | Self::PreoptElisionGap
                | Self::LoweredNoTargetUnexplained
                | Self::MissingSourceUnexplained
                | Self::TruncatedUnknown
                | Self::Unclassified
        )
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::GoodExact => "good_exact",
            Self::GoodManyToMany => "good_many_to_many",
            Self::SourceOnlyExpected => "source_only_expected",
            Self::SourceSpanSiblingUnlowered => "source_span_sibling_unlowered",
            Self::ExpectedSynthetic => "expected_synthetic",
            Self::LoweredNoTargetUnexplained => "lowered_no_target_unexplained",
            Self::PreoptElisionGap => "preopt_elision_gap",
            Self::OptimizedAttributionGap => "optimized_attribution_gap",
            Self::MissingSourceUnexplained => "missing_source_unexplained",
            Self::TruncatedUnknown => "truncated_unknown",
            Self::Unclassified => "unclassified",
        }
    }
}

impl ClosureAuditSymptom {
    fn is_suspicious(self) -> bool {
        matches!(
            self,
            Self::TooBroad
                | Self::MissingSourceUnexplained
                | Self::MissingBytecode
                | Self::ForeignSource
                | Self::Truncated
        )
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::TooBroad => "too_broad",
            Self::MissingSourceUnexplained => "missing_source_unexplained",
            Self::SourceUnavailableSynthetic => "source_unavailable_synthetic",
            Self::MissingBytecode => "missing_bytecode",
            Self::ForeignSource => "foreign_source",
            Self::Truncated => "truncated",
        }
    }
}

pub fn build_origin_closure_set(
    input_path: &str,
    snapshot: &TraceSnapshot,
    loop_report: &LoopContentsReport,
) -> OriginClosureSet {
    let index = OriginClosureIndex::new(snapshot);
    let roots = closure_roots(input_path, &index, loop_report);
    let closures = build_closures(roots, &index);
    OriginClosureSet {
        closures,
        edge_count: index.edge_count,
        instruction_count: index.instruction_count,
        source_span_count: index.source_spans.len(),
    }
}

pub fn classes_by_origin_key(closures: &[OriginClosure]) -> BTreeMap<String, Vec<String>> {
    let mut classes = BTreeMap::<String, BTreeSet<String>>::new();
    for closure in closures {
        for key in &closure.keys {
            classes
                .entry(key.clone())
                .or_default()
                .insert(closure.class_name.clone());
        }
    }
    classes
        .into_iter()
        .map(|(key, value)| (key, value.into_iter().collect()))
        .collect()
}

pub fn source_owner_matches_input(owner: &str, input_path: &str) -> bool {
    owner == input_path || owner.ends_with(input_path)
}

pub fn highest_phase_reached(counts: &OriginClosureCounts) -> ClosureAuditPhase {
    if counts.bytecode > 0 {
        ClosureAuditPhase::Bytecode
    } else if counts.sonatina_post > 0 {
        ClosureAuditPhase::SonatinaPost
    } else if counts.sonatina_pre > 0 {
        ClosureAuditPhase::SonatinaPre
    } else if counts.mir > 0 {
        ClosureAuditPhase::Mir
    } else if counts.hir > 0 {
        ClosureAuditPhase::Hir
    } else {
        ClosureAuditPhase::Source
    }
}

pub fn audit_origin_closures(
    input_path: &str,
    target: &str,
    data_source: &str,
    loop_bytecode_count: usize,
    closures: &[OriginClosure],
    source_lines: &[OriginClosureSourceLine],
) -> ClosureAuditReport {
    let span_groups = SourceSpanGroupIndex::new(input_path, closures, source_lines);
    let closures = closures
        .iter()
        .map(|closure| audit_closure(input_path, loop_bytecode_count, closure, &span_groups))
        .collect::<Vec<_>>();
    let mut primary_counts = BTreeMap::<ClosureAuditPrimary, usize>::new();
    let mut symptom_counts = BTreeMap::<ClosureAuditSymptom, usize>::new();
    let mut suspicious = 0;
    for closure in &closures {
        if closure.suspicious {
            suspicious += 1;
        }
        *primary_counts.entry(closure.primary).or_default() += 1;
        for symptom in &closure.symptoms {
            *symptom_counts.entry(*symptom).or_default() += 1;
        }
    }
    ClosureAuditReport {
        input_path: input_path.to_string(),
        target: target.to_string(),
        data_source: data_source.to_string(),
        total_closures: closures.len(),
        span_groups: span_groups.summary(),
        span_group_details: span_groups.mixed_details(),
        primary_counts,
        symptom_counts,
        suspicious_closures: suspicious,
        closures,
    }
}

struct RawOriginClosure {
    keys: BTreeSet<OriginExportKey>,
    edges: Vec<OriginClosureEdge>,
    traversal: OriginClosureTraversalSummary,
}

struct OriginClosureIndex<'a> {
    edges_by_from: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
    edges_by_to: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
    instructions: BTreeMap<OriginExportKey, &'a InstructionFact>,
    source_spans: BTreeMap<OriginExportKey, &'a SourceSpanFact>,
    edge_count: usize,
    instruction_count: usize,
}

impl<'a> OriginClosureIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut edges_by_from = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut edges_by_to = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut instructions = BTreeMap::new();
        let mut source_spans = BTreeMap::new();
        let mut edge_count = 0;
        let mut instruction_count = 0;

        for fact in snapshot.facts() {
            match fact {
                TraceFact::OriginEdge(edge) => {
                    edge_count += 1;
                    edges_by_from
                        .entry(edge.from.clone())
                        .or_default()
                        .push(edge);
                    edges_by_to.entry(edge.to.clone()).or_default().push(edge);
                }
                TraceFact::Instruction(instruction) => {
                    instruction_count += 1;
                    instructions.insert(instruction.instruction.clone(), instruction);
                }
                TraceFact::SourceSpan(span) => {
                    source_spans.insert(span.origin.clone(), span);
                }
                _ => {}
            }
        }

        Self {
            edges_by_from,
            edges_by_to,
            instructions,
            source_spans,
            edge_count,
            instruction_count,
        }
    }
}

fn closure_roots(
    input_path: &str,
    index: &OriginClosureIndex<'_>,
    loop_report: &LoopContentsReport,
) -> Vec<OriginExportKey> {
    let mut roots = BTreeMap::<String, OriginExportKey>::new();
    for span in index.source_spans.values() {
        if matches!(span.origin.kind(), "hir.expr" | "hir.stmt")
            && source_span_matches_input(span, input_path)
        {
            roots.insert(span.origin.canonical_storage_key(), span.origin.clone());
        }
    }
    for instruction in &loop_report.target_instructions {
        roots.insert(
            instruction.key.canonical_storage_key(),
            instruction.key.clone(),
        );
    }
    for block in &loop_report.blocks {
        roots.insert(block.block.canonical_storage_key(), block.block.clone());
        for instruction in &block.instructions {
            roots.insert(
                instruction.key.canonical_storage_key(),
                instruction.key.clone(),
            );
        }
    }
    for (key, instruction) in &index.instructions {
        if key.kind() == "bytecode.pc" && has_exact_trace_edge(key, index) {
            roots.insert(
                instruction.instruction.canonical_storage_key(),
                instruction.instruction.clone(),
            );
        }
    }
    roots.into_values().collect()
}

fn has_exact_trace_edge(key: &OriginExportKey, index: &OriginClosureIndex<'_>) -> bool {
    index.edges_by_from.get(key).is_some_and(|edges| {
        edges.iter().any(|edge| {
            edge.to.kind() != "code.object"
                && matches!(
                    edge.label,
                    OriginEdgeLabel::LoweredFrom | OriginEdgeLabel::EmittedFrom
                )
        })
    })
}

fn source_span_matches_input(span: &SourceSpanFact, input_path: &str) -> bool {
    source_owner_matches_input(span.file.owner_key(), input_path)
}

fn build_closures(
    roots: Vec<OriginExportKey>,
    index: &OriginClosureIndex<'_>,
) -> Vec<OriginClosure> {
    roots
        .into_iter()
        .enumerate()
        .map(|(ordinal, root)| {
            let closure = origin_closure(&root, index);
            let source_spans = source_spans_for_keys(&closure.keys, index);
            let counts = closure_counts(&closure.keys);
            let gap = closure_gap_note(&counts);
            OriginClosure {
                class_name: format!("trace-c-{ordinal}"),
                label: closure_label(&root, index),
                root_key: root.canonical_storage_key(),
                keys: closure
                    .keys
                    .iter()
                    .map(OriginExportKey::canonical_storage_key)
                    .collect(),
                counts,
                traversal: closure.traversal,
                gap,
                edges: closure.edges,
                source_spans,
            }
        })
        .collect()
}

fn closure_counts(keys: &BTreeSet<OriginExportKey>) -> OriginClosureCounts {
    let mut counts = OriginClosureCounts {
        hir: 0,
        mir: 0,
        sonatina_pre: 0,
        sonatina_post: 0,
        bytecode: 0,
    };
    for key in keys {
        match key.kind() {
            kind if kind.starts_with("hir.") => counts.hir += 1,
            kind if kind.starts_with("runtime.") => counts.mir += 1,
            kind if kind.starts_with("sonatina.preopt.") => counts.sonatina_pre += 1,
            kind if kind.starts_with("sonatina.postopt.") => counts.sonatina_post += 1,
            "bytecode.pc" => counts.bytecode += 1,
            _ => {}
        }
    }
    counts
}

fn closure_gap_note(counts: &OriginClosureCounts) -> Option<String> {
    if counts.sonatina_post > 0 && counts.bytecode == 0 {
        Some(
            "Reached Sonatina post-opt but no bytecode PC edge was recorded. This is an optimized-code attribution gap through backend preparation or value movement, not evidence that the source is dead.".to_string(),
        )
    } else {
        None
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct SourceSpanSignature {
    file_owner: String,
    start_byte: u32,
    end_byte: u32,
}

#[derive(Default)]
struct SourceSpanGroup {
    closures: usize,
    closures_with_targets: usize,
    source_only_closures: usize,
    target_connected_members: BTreeSet<ClosureAuditSpanGroupMember>,
    source_only_members: BTreeSet<ClosureAuditSpanGroupMember>,
    source_text: Option<String>,
}

struct SourceSpanGroupIndex {
    groups: BTreeMap<SourceSpanSignature, SourceSpanGroup>,
}

impl SourceSpanGroupIndex {
    fn new(
        input_path: &str,
        closures: &[OriginClosure],
        source_lines: &[OriginClosureSourceLine],
    ) -> Self {
        let mut groups = BTreeMap::<SourceSpanSignature, SourceSpanGroup>::new();
        for closure in closures {
            let has_lowering_target = closure.counts.mir
                + closure.counts.sonatina_pre
                + closure.counts.sonatina_post
                + closure.counts.bytecode
                > 0;
            for span in &closure.source_spans {
                let Some(signature) = source_span_signature(span, input_path) else {
                    continue;
                };
                let group = groups.entry(signature).or_default();
                group.closures += 1;
                let member = ClosureAuditSpanGroupMember {
                    class_name: closure.class_name.clone(),
                    label: closure.label.clone(),
                    root_key: closure.root_key.clone(),
                    highest_phase_reached: highest_phase_reached(&closure.counts),
                };
                if has_lowering_target {
                    group.closures_with_targets += 1;
                    group.target_connected_members.insert(member);
                } else {
                    group.source_only_closures += 1;
                    group.source_only_members.insert(member);
                }
                if group.source_text.is_none() {
                    group.source_text = source_text_for_span(span, source_lines);
                }
            }
        }
        Self { groups }
    }

    fn summary(&self) -> ClosureAuditSpanGroupSummary {
        ClosureAuditSpanGroupSummary {
            total_groups: self.groups.len(),
            mixed_connectivity_groups: self
                .groups
                .values()
                .filter(|group| group.closures_with_targets > 0 && group.source_only_closures > 0)
                .count(),
        }
    }

    fn mixed_details(&self) -> Vec<ClosureAuditSpanGroupDetail> {
        self.groups
            .iter()
            .filter(|(_, group)| group.closures_with_targets > 0 && group.source_only_closures > 0)
            .map(|(signature, group)| ClosureAuditSpanGroupDetail {
                file_owner: signature.file_owner.clone(),
                start_byte: signature.start_byte,
                end_byte: signature.end_byte,
                source_text: group.source_text.clone(),
                closures: group.closures,
                closures_with_targets: group.closures_with_targets,
                source_only_closures: group.source_only_closures,
                target_connected_members: group.target_connected_members.iter().cloned().collect(),
                source_only_members: group.source_only_members.iter().cloned().collect(),
            })
            .collect()
    }
}

fn source_span_signature(
    span: &OriginClosureSourceSpan,
    input_path: &str,
) -> Option<SourceSpanSignature> {
    (span.confidence == "direct" && source_owner_matches_input(&span.file_owner, input_path)).then(
        || SourceSpanSignature {
            file_owner: span.file_owner.clone(),
            start_byte: span.start_byte,
            end_byte: span.end_byte,
        },
    )
}

fn source_text_for_span(
    span: &OriginClosureSourceSpan,
    source_lines: &[OriginClosureSourceLine],
) -> Option<String> {
    if span.start_line != span.end_line {
        return Some(format!("{}:{}", span.start_line, span.end_line));
    }
    source_lines
        .iter()
        .find(|line| line.number == span.start_line)
        .map(|line| line.text.trim().to_string())
        .filter(|text| !text.is_empty())
}

fn audit_closure(
    input_path: &str,
    loop_bytecode_count: usize,
    closure: &OriginClosure,
    span_groups: &SourceSpanGroupIndex,
) -> ClosureAuditEntry {
    let mut symptoms = BTreeSet::new();
    let mut notes = Vec::new();
    let direct_input_spans = closure
        .source_spans
        .iter()
        .filter(|span| {
            span.confidence == "direct" && source_owner_matches_input(&span.file_owner, input_path)
        })
        .collect::<Vec<_>>();
    let foreign_spans = closure
        .source_spans
        .iter()
        .filter(|span| {
            span.confidence == "direct" && !source_owner_matches_input(&span.file_owner, input_path)
        })
        .collect::<Vec<_>>();

    if !foreign_spans.is_empty() {
        symptoms.insert(ClosureAuditSymptom::ForeignSource);
        notes.push(format!(
            "{} direct source span(s) point at a file other than {input_path}",
            foreign_spans.len()
        ));
    }

    let broad_bytecode_threshold = (loop_bytecode_count.saturating_mul(3) / 4).max(64);
    if closure.counts.bytecode > broad_bytecode_threshold || closure.keys.len() > 300 {
        symptoms.insert(ClosureAuditSymptom::TooBroad);
        notes.push(format!(
            "connected trace region touches {} bytecode PC(s) and {} total key(s)",
            closure.counts.bytecode,
            closure.keys.len()
        ));
    }

    if closure.traversal.truncated {
        symptoms.insert(ClosureAuditSymptom::Truncated);
        notes.push(format!(
            "connected trace region traversal truncated: {}",
            closure
                .traversal
                .truncation_reason
                .as_deref()
                .unwrap_or("unknown")
        ));
    }

    let has_ir = closure.counts.hir
        + closure.counts.mir
        + closure.counts.sonatina_pre
        + closure.counts.sonatina_post
        > 0;
    let has_expected_synthetic = closure.edges.iter().any(|edge| {
        edge.label.starts_with("SyntheticFor") || edge.label.starts_with("BackendPrepared")
    });
    let has_lowering_target = closure.counts.mir
        + closure.counts.sonatina_pre
        + closure.counts.sonatina_post
        + closure.counts.bytecode
        > 0;
    let same_span_sibling_reaches_target = !has_lowering_target
        && direct_input_spans.iter().any(|span| {
            source_span_signature(span, input_path)
                .and_then(|signature| span_groups.groups.get(&signature))
                .is_some_and(|group| group.closures_with_targets > 0)
        });
    if closure.counts.sonatina_post > 0 && closure.counts.bytecode == 0 {
        symptoms.insert(ClosureAuditSymptom::MissingBytecode);
        notes.push("post-opt closure reaches no final bytecode PC".to_string());
    }
    if has_ir && closure.counts.bytecode > 0 && direct_input_spans.is_empty() {
        if has_expected_synthetic {
            symptoms.insert(ClosureAuditSymptom::SourceUnavailableSynthetic);
            notes.push("bytecode-linked synthetic/backend closure has no direct source span; this is expected when the edge explains generated work".to_string());
        } else {
            symptoms.insert(ClosureAuditSymptom::MissingSourceUnexplained);
            notes.push(
                "bytecode-linked closure has no direct span in the audited input file".to_string(),
            );
        }
    }

    let primary = if closure.traversal.truncated {
        ClosureAuditPrimary::TruncatedUnknown
    } else if closure.counts.sonatina_post > 0 && closure.counts.bytecode == 0 {
        if let Some(gap) = &closure.gap {
            notes.push(gap.clone());
        }
        ClosureAuditPrimary::OptimizedAttributionGap
    } else if closure.counts.sonatina_pre > 0
        && closure.counts.sonatina_post == 0
        && closure.counts.bytecode == 0
    {
        notes.push("closure reaches Sonatina pre-opt but has no post-opt or bytecode target; no explicit elision/coalescing event explains it".to_string());
        ClosureAuditPrimary::PreoptElisionGap
    } else if same_span_sibling_reaches_target {
        notes.push("source-only HIR origin shares this exact input span with another HIR origin that reaches MIR/Sonatina/bytecode; this is a display/grouping sibling, not a duplicate identity".to_string());
        ClosureAuditPrimary::SourceSpanSiblingUnlowered
    } else if closure.counts.mir > 0
        && closure.counts.sonatina_pre == 0
        && closure.counts.sonatina_post == 0
        && closure.counts.bytecode == 0
    {
        notes.push("closure reaches MIR but has no Sonatina or bytecode target; no explicit lowering/elision event explains it".to_string());
        ClosureAuditPrimary::LoweredNoTargetUnexplained
    } else if has_expected_synthetic {
        ClosureAuditPrimary::ExpectedSynthetic
    } else if has_ir && closure.counts.bytecode > 0 && direct_input_spans.is_empty() {
        ClosureAuditPrimary::MissingSourceUnexplained
    } else if !direct_input_spans.is_empty() && closure.counts.bytecode > 0 {
        let exact = direct_input_spans.len() == 1
            && closure.counts.bytecode == 1
            && closure.counts.hir <= 1
            && closure.counts.mir <= 1
            && closure.counts.sonatina_pre <= 1
            && closure.counts.sonatina_post <= 1;
        if exact {
            ClosureAuditPrimary::GoodExact
        } else {
            ClosureAuditPrimary::GoodManyToMany
        }
    } else if !direct_input_spans.is_empty()
        && closure.counts.mir == 0
        && closure.counts.sonatina_pre == 0
        && closure.counts.sonatina_post == 0
        && closure.counts.bytecode == 0
    {
        ClosureAuditPrimary::SourceOnlyExpected
    } else {
        notes.push("closure did not match a known audit bucket".to_string());
        ClosureAuditPrimary::Unclassified
    };
    let symptoms = symptoms.into_iter().collect::<Vec<_>>();
    let suspicious =
        primary.is_suspicious() || symptoms.iter().any(|symptom| symptom.is_suspicious());

    ClosureAuditEntry {
        class_name: closure.class_name.clone(),
        label: closure.label.clone(),
        root_key: closure.root_key.clone(),
        primary,
        symptoms,
        suspicious,
        highest_phase_reached: highest_phase_reached(&closure.counts),
        counts: closure.counts.clone(),
        traversal: closure.traversal.clone(),
        key_count: closure.keys.len(),
        edge_count: closure.edges.len(),
        source_spans: direct_input_spans
            .into_iter()
            .map(|span| format!("{} {} {}", span.file, span.lines, span.origin))
            .collect(),
        notes,
    }
}

fn origin_closure(start: &OriginExportKey, index: &OriginClosureIndex<'_>) -> RawOriginClosure {
    let mut keys = BTreeSet::new();
    let mut edges_out = Vec::new();
    let mut seen_edges = BTreeSet::new();
    let mut skipped_hubs = BTreeSet::new();
    let mut truncated = false;
    let mut truncation_reason = None;
    let mut queue = VecDeque::from([(start.clone(), 0_usize)]);
    while let Some((key, depth)) = queue.pop_front() {
        if depth >= CLOSURE_MAX_DEPTH {
            truncated = true;
            truncation_reason.get_or_insert_with(|| "max_depth".to_string());
            continue;
        }
        if keys.len() >= CLOSURE_MAX_NODES && !keys.contains(&key) {
            truncated = true;
            truncation_reason.get_or_insert_with(|| "max_nodes".to_string());
            continue;
        }
        if !keys.insert(key.clone()) {
            continue;
        }
        let outgoing = index.edges_by_from.get(&key).into_iter().flatten().copied();
        let incoming = index.edges_by_to.get(&key).into_iter().flatten().copied();
        for edge in outgoing.chain(incoming) {
            let from_hub = trace_hub_reason(&edge.from, index, start);
            let to_hub = trace_hub_reason(&edge.to, index, start);
            if let Some(reason) = from_hub {
                skipped_hubs.insert(format!("{} ({reason})", edge.from.display_label()));
            }
            if let Some(reason) = to_hub {
                skipped_hubs.insert(format!("{} ({reason})", edge.to.display_label()));
            }
            if from_hub.is_some() || to_hub.is_some() {
                continue;
            }
            if !matches!(
                edge.label,
                OriginEdgeLabel::LoweredFrom
                    | OriginEdgeLabel::EmittedFrom
                    | OriginEdgeLabel::SyntheticFor
                    | OriginEdgeLabel::BackendPrepared
            ) {
                continue;
            }
            let edge_key = (
                edge.from.canonical_storage_key(),
                edge.to.canonical_storage_key(),
                format!("{:?}", edge.label),
            );
            if seen_edges.insert(edge_key) {
                edges_out.push(OriginClosureEdge {
                    label: format!(
                        "{:?} / {}",
                        edge.label,
                        edge.introduced_by
                            .map(|phase| format!("{phase:?}"))
                            .unwrap_or_else(|| "unknown".to_string())
                    ),
                    from: display_key(&edge.from),
                    to: display_key(&edge.to),
                });
            }
            queue.push_back((edge.from.clone(), depth + 1));
            queue.push_back((edge.to.clone(), depth + 1));
        }
    }
    RawOriginClosure {
        keys,
        edges: edges_out,
        traversal: OriginClosureTraversalSummary {
            mode: "undirected_connected_trace_region".to_string(),
            max_depth: CLOSURE_MAX_DEPTH,
            max_nodes: CLOSURE_MAX_NODES,
            truncated,
            truncation_reason,
            skipped_hubs: skipped_hubs.into_iter().collect(),
        },
    }
}

fn trace_hub_reason(
    key: &OriginExportKey,
    index: &OriginClosureIndex<'_>,
    root: &OriginExportKey,
) -> Option<&'static str> {
    if key == root {
        return None;
    }
    if matches!(key.kind(), "code.object" | "source.file") {
        return Some("known_context_hub");
    }
    (origin_edge_degree(key, index) > CLOSURE_DEGREE_HUB_THRESHOLD)
        .then_some("high_degree_context_hub")
}

fn origin_edge_degree(key: &OriginExportKey, index: &OriginClosureIndex<'_>) -> usize {
    index.edges_by_from.get(key).map_or(0, Vec::len)
        + index.edges_by_to.get(key).map_or(0, Vec::len)
}

fn source_spans_for_keys(
    keys: &BTreeSet<OriginExportKey>,
    index: &OriginClosureIndex<'_>,
) -> Vec<OriginClosureSourceSpan> {
    let storage_keys = keys
        .iter()
        .map(OriginExportKey::canonical_storage_key)
        .collect::<BTreeSet<_>>();
    index
        .source_spans
        .iter()
        .filter(|(origin, _)| storage_keys.contains(&origin.canonical_storage_key()))
        .filter(|(_, span)| !has_finer_span(span, &storage_keys, index))
        .map(|(origin, span)| OriginClosureSourceSpan {
            origin: origin.display_label(),
            file: span.file.display_label(),
            file_owner: span.file.owner_key().to_string(),
            lines: format!("{}:{}", span.start_line, span.end_line),
            start_byte: span.start_byte,
            end_byte: span.end_byte,
            start_line: span.start_line,
            end_line: span.end_line,
            confidence: if origin.kind() == "code.object" {
                "coarse".to_string()
            } else {
                "direct".to_string()
            },
        })
        .collect()
}

fn closure_label(root: &OriginExportKey, index: &OriginClosureIndex<'_>) -> String {
    index.instructions.get(root).map_or_else(
        || root.display_label(),
        |instruction| {
            format!(
                "{} ir[{}] {}",
                root.display_label(),
                instruction.index,
                instruction.mnemonic
            )
        },
    )
}

fn has_finer_span(
    span: &SourceSpanFact,
    keys: &BTreeSet<String>,
    index: &OriginClosureIndex<'_>,
) -> bool {
    index
        .source_spans
        .iter()
        .filter(|(origin, _)| keys.contains(&origin.canonical_storage_key()))
        .any(|(_, other)| {
            other.file == span.file
                && (other.start_byte, other.end_byte) != (span.start_byte, span.end_byte)
                && other.start_byte >= span.start_byte
                && other.end_byte <= span.end_byte
                && (other.end_byte - other.start_byte) < (span.end_byte - span.start_byte)
        })
}

fn display_key(key: &OriginExportKey) -> String {
    format!("{} {}", key.kind(), key.display_label())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Confidence, ReportMetadata};
    use trace_facts::{CompilerPhase, InstructionFact, OriginEdgeFact};

    #[test]
    fn origin_closure_does_not_expand_through_code_object() {
        let pc0 = test_key("bytecode.pc", "runtime", "pc:0");
        let pc1 = test_key("bytecode.pc", "runtime", "pc:1");
        let code_object = test_key("code.object", "runtime", "runtime");
        let edges = vec![
            OriginEdgeFact::new(
                pc0.clone(),
                code_object.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            ),
            OriginEdgeFact::new(
                pc1.clone(),
                code_object.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            ),
        ];
        let mut edges_by_from = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut edges_by_to = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        for edge in &edges {
            edges_by_from
                .entry(edge.from.clone())
                .or_default()
                .push(edge);
            edges_by_to.entry(edge.to.clone()).or_default().push(edge);
        }
        let index = OriginClosureIndex {
            edges_by_from,
            edges_by_to,
            instructions: BTreeMap::new(),
            source_spans: BTreeMap::new(),
            edge_count: edges.len(),
            instruction_count: 0,
        };

        let closure = origin_closure(&pc0, &index);

        assert!(closure.keys.contains(&pc0));
        assert!(!closure.keys.contains(&pc1));
        assert!(!closure.keys.contains(&code_object));
        assert!(closure.edges.is_empty());
        assert_eq!(closure.traversal.skipped_hubs.len(), 1);
    }

    #[test]
    fn closure_roots_include_bytecode_with_exact_trace_edges() {
        let pc = test_key("bytecode.pc", "runtime", "pc:0");
        let code_object = test_key("code.object", "runtime", "runtime");
        let postopt = test_key("sonatina.postopt.inst", "sonatina", "inst:0");
        let function = test_key("bytecode.function", "runtime", "function");
        let edges = [
            OriginEdgeFact::new(
                pc.clone(),
                code_object,
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            ),
            OriginEdgeFact::new(
                pc.clone(),
                postopt,
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            ),
        ];
        let instruction = InstructionFact::new(pc.clone(), function, 0, "PUSH0");
        let mut edges_by_from = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut edges_by_to = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        for edge in &edges {
            edges_by_from
                .entry(edge.from.clone())
                .or_default()
                .push(edge);
            edges_by_to.entry(edge.to.clone()).or_default().push(edge);
        }
        let index = OriginClosureIndex {
            edges_by_from,
            edges_by_to,
            instructions: BTreeMap::from([(pc.clone(), &instruction)]),
            source_spans: BTreeMap::new(),
            edge_count: edges.len(),
            instruction_count: 1,
        };

        let roots = closure_roots("fib_demo.fe", &index, &empty_loop_report());

        assert!(roots.contains(&pc));
    }

    #[test]
    fn closure_audit_flags_optimized_gap_without_calling_it_dead() {
        let mut closure = test_closure();
        closure.counts.sonatina_post = 2;
        closure.gap = closure_gap_note(&closure.counts);

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::OptimizedAttributionGap);
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::SonatinaPost);
        assert!(
            entry
                .symptoms
                .contains(&ClosureAuditSymptom::MissingBytecode)
        );
        assert!(entry.notes.iter().any(|note| note.contains("not evidence")));
    }

    #[test]
    fn closure_audit_marks_source_to_bytecode_many_to_many() {
        let mut closure = test_closure();
        closure.counts.bytecode = 3;
        closure.keys = vec![
            "hir".to_string(),
            "mir".to_string(),
            "post".to_string(),
            "pc:1".to_string(),
            "pc:2".to_string(),
            "pc:3".to_string(),
        ];

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::GoodManyToMany);
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::Bytecode);
        assert!(entry.symptoms.is_empty());
    }

    #[test]
    fn closure_audit_marks_source_to_bytecode_exact() {
        let mut closure = test_closure();
        closure.counts.bytecode = 1;

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::GoodExact);
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::Bytecode);
        assert!(!entry.suspicious);
    }

    #[test]
    fn closure_audit_marks_source_only_expected() {
        let mut closure = test_closure();
        closure.counts.mir = 0;
        closure.counts.sonatina_pre = 0;

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::SourceOnlyExpected);
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::Hir);
        assert!(!entry.suspicious);
    }

    #[test]
    fn closure_audit_marks_same_span_source_only_sibling() {
        let mut source_only = test_closure();
        source_only.counts.mir = 0;
        source_only.counts.sonatina_pre = 0;
        let mut lowered = test_closure();
        lowered.class_name = "trace-c-1".to_string();
        lowered.root_key = "lowered".to_string();
        lowered.counts.bytecode = 1;
        let closures = vec![source_only.clone(), lowered];
        let groups = SourceSpanGroupIndex::new("fib_demo.fe", &closures, &test_source_lines());

        let entry = audit_closure("fib_demo.fe", 24, &source_only, &groups);

        assert_eq!(
            entry.primary,
            ClosureAuditPrimary::SourceSpanSiblingUnlowered
        );
        assert!(!entry.suspicious);

        let details = groups.mixed_details();
        assert_eq!(details.len(), 1);
        assert_eq!(details[0].source_text.as_deref(), Some("a = b"));
        assert_eq!(details[0].closures_with_targets, 1);
        assert_eq!(details[0].source_only_closures, 1);
        assert_eq!(details[0].target_connected_members.len(), 1);
        assert_eq!(details[0].target_connected_members[0].root_key, "lowered");
        assert_eq!(
            details[0].target_connected_members[0].highest_phase_reached,
            ClosureAuditPhase::Bytecode
        );
        assert_eq!(details[0].source_only_members.len(), 1);
        assert_eq!(details[0].source_only_members[0].root_key, "root");
        assert_eq!(
            details[0].source_only_members[0].highest_phase_reached,
            ClosureAuditPhase::Hir
        );
    }

    #[test]
    fn closure_audit_flags_preopt_elision_without_postopt() {
        let closure = test_closure();
        let groups = groups_for_closure(&closure);

        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::PreoptElisionGap);
        assert!(entry.suspicious);
    }

    #[test]
    fn closure_audit_flags_lowered_no_target_unexplained() {
        let mut closure = test_closure();
        closure.counts.sonatina_pre = 0;

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(
            entry.primary,
            ClosureAuditPrimary::LoweredNoTargetUnexplained
        );
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::Mir);
        assert!(entry.suspicious);
    }

    #[test]
    fn closure_audit_downgrades_synthetic_missing_source() {
        let mut closure = test_closure();
        closure.source_spans.clear();
        closure.counts.bytecode = 1;
        closure.edges.push(OriginClosureEdge {
            label: "BackendPrepared / Backend".to_string(),
            from: "backend event".to_string(),
            to: "bytecode.pc pc:0".to_string(),
        });
        let groups = groups_for_closure(&closure);

        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::ExpectedSynthetic);
        assert!(
            entry
                .symptoms
                .contains(&ClosureAuditSymptom::SourceUnavailableSynthetic)
        );
        assert!(!entry.suspicious);
    }

    #[test]
    fn closure_audit_flags_unexplained_missing_source() {
        let mut closure = test_closure();
        closure.source_spans.clear();
        closure.counts.bytecode = 1;

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::MissingSourceUnexplained);
        assert!(
            entry
                .symptoms
                .contains(&ClosureAuditSymptom::MissingSourceUnexplained)
        );
        assert!(entry.suspicious);
    }

    #[test]
    fn closure_audit_flags_truncated_unknown() {
        let mut closure = test_closure();
        closure.traversal.truncated = true;
        closure.traversal.truncation_reason = Some("max_nodes".to_string());

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::TruncatedUnknown);
        assert!(entry.symptoms.contains(&ClosureAuditSymptom::Truncated));
        assert!(entry.suspicious);
    }

    #[test]
    fn closure_audit_keeps_unclassified_fallback_visible() {
        let mut closure = test_closure();
        closure.source_spans.clear();
        closure.counts = OriginClosureCounts {
            hir: 0,
            mir: 0,
            sonatina_pre: 0,
            sonatina_post: 0,
            bytecode: 0,
        };

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::Unclassified);
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::Source);
        assert!(entry.suspicious);
    }

    fn test_key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn empty_loop_report() -> LoopContentsReport {
        LoopContentsReport {
            metadata: ReportMetadata {
                trace_hash: "demo".to_string(),
                data_source: "fixture".to_string(),
                target: "evm/sonatina".to_string(),
                input_path: "fib_demo.fe".to_string(),
                compiler_commit: "abc123".to_string(),
                flags: Vec::new(),
            },
            available: false,
            unavailable_reason: None,
            loop_key: None,
            loop_label: None,
            blocks: Vec::new(),
            instructions: Vec::new(),
            target_instructions: Vec::new(),
            bytecode_bridge_available: false,
            bytecode_origin_edges_available: false,
            findings: Vec::new(),
            confidence: Confidence::Unknown,
        }
    }

    fn test_closure() -> OriginClosure {
        OriginClosure {
            class_name: "trace-c-0".to_string(),
            label: "hir.expr:demo:11".to_string(),
            root_key: "root".to_string(),
            keys: Vec::new(),
            counts: OriginClosureCounts {
                hir: 1,
                mir: 1,
                sonatina_pre: 1,
                sonatina_post: 0,
                bytecode: 0,
            },
            traversal: test_traversal(),
            gap: None,
            edges: Vec::new(),
            source_spans: vec![OriginClosureSourceSpan {
                origin: "hir.expr:body:11".to_string(),
                file: "fib_demo.fe".to_string(),
                file_owner: "fib_demo.fe".to_string(),
                lines: "18:18".to_string(),
                start_byte: 0,
                end_byte: 1,
                start_line: 18,
                end_line: 18,
                confidence: "direct".to_string(),
            }],
        }
    }

    fn test_traversal() -> OriginClosureTraversalSummary {
        OriginClosureTraversalSummary {
            mode: "undirected_connected_trace_region".to_string(),
            max_depth: CLOSURE_MAX_DEPTH,
            max_nodes: CLOSURE_MAX_NODES,
            truncated: false,
            truncation_reason: None,
            skipped_hubs: Vec::new(),
        }
    }

    fn groups_for_closure(closure: &OriginClosure) -> SourceSpanGroupIndex {
        SourceSpanGroupIndex::new(
            "fib_demo.fe",
            std::slice::from_ref(closure),
            &test_source_lines(),
        )
    }

    fn test_source_lines() -> Vec<OriginClosureSourceLine> {
        vec![OriginClosureSourceLine {
            number: 18,
            text: "a = b".to_string(),
        }]
    }
}
