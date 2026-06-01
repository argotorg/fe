use std::collections::{BTreeMap, BTreeSet, VecDeque};

use common::origin::OriginExportKey;
use serde::Serialize;
use trace_facts::{
    CompilerEventKind, InstructionFact, OriginEdgeFact, OriginEdgeTraversalClass, SourceSpanFact,
    TraceFact, TraceSnapshot,
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
    pub generated_keys: Vec<String>,
    pub contextual_keys: Vec<String>,
    pub structural_keys: Vec<String>,
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
    pub sonatina_prepared: usize,
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
    pub traversal_class: OriginEdgeTraversalClass,
    pub generated_work: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct OriginClosureSourceSpan {
    #[serde(skip_serializing)]
    pub origin_key: String,
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
    SonatinaPrepared,
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
            Self::SonatinaPrepared => "sonatina_prepared",
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
    OptimizerExplained,
    PreparedOnly,
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
            Self::OptimizerExplained => "optimizer_explained",
            Self::PreparedOnly => "prepared_only",
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
    let explained_postopt_origins = explicit_postopt_explanations(snapshot)
        .into_iter()
        .map(|key| key.canonical_storage_key())
        .collect::<BTreeSet<_>>();
    let closures = build_closures(roots, &index, &explained_postopt_origins);
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
        for key in closure
            .generated_keys
            .iter()
            .chain(closure.contextual_keys.iter())
            .chain(closure.structural_keys.iter())
        {
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

pub fn component_classes_by_origin_key(snapshot: &TraceSnapshot) -> BTreeMap<String, Vec<String>> {
    let index = OriginClosureIndex::new(snapshot);
    component_classes_for_index(&index)
}

pub fn source_owner_matches_input(owner: &str, input_path: &str) -> bool {
    if owner == input_path {
        return true;
    }
    let input_path = input_path.trim();
    if input_path.is_empty() {
        return false;
    }
    owner
        .strip_suffix(input_path)
        .is_some_and(|prefix| prefix.ends_with('/') || prefix.ends_with(':'))
}

pub fn highest_phase_reached(counts: &OriginClosureCounts) -> ClosureAuditPhase {
    if counts.bytecode > 0 {
        ClosureAuditPhase::Bytecode
    } else if counts.sonatina_prepared > 0 {
        ClosureAuditPhase::SonatinaPrepared
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
    snapshot: &TraceSnapshot,
) -> ClosureAuditReport {
    let span_groups = SourceSpanGroupIndex::new(input_path, closures, source_lines);
    let explained_postopt_origins = explicit_postopt_explanations(snapshot)
        .into_iter()
        .map(|key| key.canonical_storage_key())
        .collect::<BTreeSet<_>>();
    let closures = closures
        .iter()
        .map(|closure| {
            audit_closure(
                input_path,
                loop_bytecode_count,
                closure,
                &span_groups,
                &explained_postopt_origins,
            )
        })
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
        edges
            .iter()
            .any(|edge| allows_origin_closure_edge(edge, OriginClosureTraversalMode::PreciseAudit))
    })
}

fn source_span_matches_input(span: &SourceSpanFact, input_path: &str) -> bool {
    source_owner_matches_input(span.file.owner_key(), input_path)
}

fn build_closures(
    roots: Vec<OriginExportKey>,
    index: &OriginClosureIndex<'_>,
    explained_postopt_origins: &BTreeSet<String>,
) -> Vec<OriginClosure> {
    roots
        .into_iter()
        .enumerate()
        .map(|(ordinal, root)| {
            let mut closure = origin_closure(&root, index);
            let explanation = origin_explanation_rails(&closure.keys, index);
            closure.edges.extend(explanation.edges);
            let source_spans = source_spans_for_keys(&closure.keys, index);
            let counts = closure_counts(&closure.keys);
            let storage_keys = closure
                .keys
                .iter()
                .map(OriginExportKey::canonical_storage_key)
                .collect::<Vec<_>>();
            let postopt_explained =
                storage_postopt_origins_are_explained(&storage_keys, explained_postopt_origins);
            let gap = closure_gap_note(&counts, postopt_explained);
            OriginClosure {
                class_name: format!("trace-c-{ordinal}"),
                label: closure_label(&root, index),
                root_key: root.canonical_storage_key(),
                keys: storage_keys,
                generated_keys: explanation
                    .generated
                    .iter()
                    .map(OriginExportKey::canonical_storage_key)
                    .collect(),
                contextual_keys: explanation
                    .contextual
                    .iter()
                    .map(OriginExportKey::canonical_storage_key)
                    .collect(),
                structural_keys: explanation
                    .structural
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

#[derive(Clone, Copy, Debug)]
enum OriginComponentRail {
    Exact,
    Generated,
    Prepared,
    Contextual,
    Structural,
}

impl OriginComponentRail {
    const ALL: [Self; 5] = [
        Self::Exact,
        Self::Generated,
        Self::Prepared,
        Self::Contextual,
        Self::Structural,
    ];

    const fn class_prefix(self) -> &'static str {
        match self {
            Self::Exact => "exact-c",
            Self::Generated => "generated-c",
            Self::Prepared => "prepared-c",
            Self::Contextual => "context-c",
            Self::Structural => "structural-c",
        }
    }

    fn allows_edge(self, edge: &OriginEdgeFact) -> bool {
        let class = edge.traversal_class();
        let exact = matches!(
            class,
            OriginEdgeTraversalClass::ExactAttribution | OriginEdgeTraversalClass::SnapshotAlias
        );
        match self {
            Self::Exact => exact,
            Self::Generated => matches!(class, OriginEdgeTraversalClass::Synthetic),
            Self::Prepared => is_prepared_bytecode_edge(edge),
            Self::Contextual => {
                matches!(class, OriginEdgeTraversalClass::Contextual)
                    && !is_prepared_bytecode_edge(edge)
            }
            Self::Structural => matches!(class, OriginEdgeTraversalClass::Structural),
        }
    }
}

fn component_classes_for_index(index: &OriginClosureIndex<'_>) -> BTreeMap<String, Vec<String>> {
    let mut classes = BTreeMap::<String, BTreeSet<String>>::new();
    for rail in OriginComponentRail::ALL {
        for (ordinal, component) in connected_components_for_rail(index, rail)
            .into_iter()
            .enumerate()
        {
            if suppress_row_level_component(rail, &component) {
                continue;
            }
            let class_name = component_class_name(rail, ordinal, &component);
            for key in component {
                classes
                    .entry(key.canonical_storage_key())
                    .or_default()
                    .insert(class_name.clone());
            }
        }
    }
    classes
        .into_iter()
        .map(|(key, value)| (key, value.into_iter().collect()))
        .collect()
}

fn connected_components_for_rail(
    index: &OriginClosureIndex<'_>,
    rail: OriginComponentRail,
) -> Vec<BTreeSet<OriginExportKey>> {
    let mut candidates = BTreeSet::<OriginExportKey>::new();
    for edges in index.edges_by_from.values() {
        for edge in edges {
            if rail.allows_edge(edge) {
                candidates.insert(edge.from.clone());
                candidates.insert(edge.to.clone());
            }
        }
    }

    let mut visited = BTreeSet::<OriginExportKey>::new();
    let mut components = Vec::<BTreeSet<OriginExportKey>>::new();
    for root in candidates.iter().cloned().collect::<Vec<_>>() {
        if visited.contains(&root) {
            continue;
        }
        let mut component = BTreeSet::new();
        let mut queue = VecDeque::from([root.clone()]);
        visited.insert(root);
        while let Some(key) = queue.pop_front() {
            component.insert(key.clone());
            let outgoing = index.edges_by_from.get(&key).into_iter().flatten().copied();
            let incoming = index.edges_by_to.get(&key).into_iter().flatten().copied();
            for edge in outgoing.chain(incoming) {
                if !rail.allows_edge(edge) {
                    continue;
                }
                let other = if edge.from == key {
                    &edge.to
                } else {
                    &edge.from
                };
                if visited.insert(other.clone()) {
                    queue.push_back(other.clone());
                }
            }
        }
        if component.len() > 1 {
            components.push(component);
        }
    }
    components.sort_by(|left, right| {
        component_sort_key(left)
            .unwrap_or_default()
            .cmp(&component_sort_key(right).unwrap_or_default())
    });
    components
}

fn suppress_row_level_component(
    rail: OriginComponentRail,
    component: &BTreeSet<OriginExportKey>,
) -> bool {
    match rail {
        OriginComponentRail::Generated | OriginComponentRail::Contextual => {
            component.len() > 128
                || component.iter().any(is_component_hub_origin)
                || component
                    .iter()
                    .filter(|key| is_source_like_origin_kind(key.kind()))
                    .count()
                    > 8
                || component
                    .iter()
                    .filter(|key| key.kind().starts_with("bytecode."))
                    .count()
                    > 64
        }
        OriginComponentRail::Exact
        | OriginComponentRail::Prepared
        | OriginComponentRail::Structural => false,
    }
}

fn is_component_hub_origin(key: &OriginExportKey) -> bool {
    let kind = key.kind();
    kind == "source.file"
        || kind == "code.object"
        || kind == "package"
        || kind == "module"
        || kind.ends_with(".module")
        || kind.ends_with(".contract")
        || kind.ends_with(".function")
        || kind.ends_with(".body")
}

fn is_source_like_origin_kind(kind: &str) -> bool {
    kind.starts_with("source.") || kind.starts_with("hir.")
}

fn component_sort_key(component: &BTreeSet<OriginExportKey>) -> Option<String> {
    component
        .iter()
        .next()
        .map(OriginExportKey::canonical_storage_key)
}

fn component_class_name(
    rail: OriginComponentRail,
    fallback_ordinal: usize,
    component: &BTreeSet<OriginExportKey>,
) -> String {
    let mut hash = 2166136261u32;
    for byte in rail.class_prefix().bytes() {
        hash ^= u32::from(byte);
        hash = hash.wrapping_mul(16777619);
    }
    for key in component {
        for byte in key.canonical_storage_key().bytes() {
            hash ^= u32::from(byte);
            hash = hash.wrapping_mul(16777619);
        }
    }
    if hash == 0 {
        format!("{}-{fallback_ordinal}", rail.class_prefix())
    } else {
        format!("{}-{hash:08x}", rail.class_prefix())
    }
}

fn is_prepared_bytecode_edge(edge: &OriginEdgeFact) -> bool {
    (edge.from.kind().starts_with("bytecode.") && is_prepared_origin_kind(edge.to.kind()))
        || (edge.to.kind().starts_with("bytecode.") && is_prepared_origin_kind(edge.from.kind()))
}

fn is_prepared_origin_kind(kind: &str) -> bool {
    kind.starts_with("sonatina.evm.prepared.")
        || kind.starts_with("sonatina.codegen.")
        || kind.starts_with("evm.vcode.")
        || kind.starts_with("vcode.")
}

fn closure_counts(keys: &BTreeSet<OriginExportKey>) -> OriginClosureCounts {
    let mut counts = OriginClosureCounts {
        hir: 0,
        mir: 0,
        sonatina_pre: 0,
        sonatina_post: 0,
        sonatina_prepared: 0,
        bytecode: 0,
    };
    for key in keys {
        match key.kind() {
            kind if kind.starts_with("hir.") => counts.hir += 1,
            kind if kind.starts_with("runtime.") => counts.mir += 1,
            kind if kind.starts_with("sonatina.preopt.") => counts.sonatina_pre += 1,
            kind if kind.starts_with("sonatina.postopt.") => counts.sonatina_post += 1,
            kind if kind.starts_with("sonatina.evm.prepared.") => counts.sonatina_prepared += 1,
            "bytecode.pc" => counts.bytecode += 1,
            _ => {}
        }
    }
    counts
}

#[derive(Default)]
struct OriginExplanationRails {
    generated: BTreeSet<OriginExportKey>,
    contextual: BTreeSet<OriginExportKey>,
    structural: BTreeSet<OriginExportKey>,
    edges: Vec<OriginClosureEdge>,
}

fn origin_explanation_rails(
    exact_keys: &BTreeSet<OriginExportKey>,
    index: &OriginClosureIndex<'_>,
) -> OriginExplanationRails {
    let mut rails = OriginExplanationRails::default();
    let mut seen_edges = BTreeSet::new();
    for key in exact_keys {
        let outgoing = index.edges_by_from.get(key).into_iter().flatten().copied();
        let incoming = index.edges_by_to.get(key).into_iter().flatten().copied();
        for edge in outgoing.chain(incoming) {
            let other = if &edge.from == key {
                &edge.to
            } else {
                &edge.from
            };
            if exact_keys.contains(other) {
                continue;
            }
            match edge.traversal_class() {
                OriginEdgeTraversalClass::Synthetic => {
                    rails.generated.insert(other.clone());
                }
                OriginEdgeTraversalClass::Contextual => {
                    rails.contextual.insert(other.clone());
                }
                OriginEdgeTraversalClass::Structural => {
                    rails.structural.insert(other.clone());
                }
                OriginEdgeTraversalClass::ExactAttribution
                | OriginEdgeTraversalClass::SnapshotAlias
                | OriginEdgeTraversalClass::Unmapped => continue,
            }
            let edge_key = (
                edge.from.canonical_storage_key(),
                edge.to.canonical_storage_key(),
                format!("{:?}", edge.label),
            );
            if seen_edges.insert(edge_key) {
                rails.edges.push(closure_edge(edge));
            }
        }
    }
    rails
}

fn closure_gap_note(counts: &OriginClosureCounts, postopt_explained: bool) -> Option<String> {
    if counts.sonatina_post > 0 && counts.bytecode == 0 {
        if postopt_explained {
            Some("Reached Sonatina post-opt and the optimizer emitted an explicit elision/rewrite/snapshot explanation. The missing direct bytecode PC edge is explained by compiler evidence.".to_string())
        } else {
            Some(
                "Reached Sonatina post-opt but no bytecode PC edge was recorded. This is an optimized-code attribution gap through backend preparation or value movement, not evidence that the source is dead.".to_string(),
            )
        }
    } else {
        None
    }
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
                        .filter(|key| key.kind().starts_with("sonatina.post"))
                        .cloned()
                        .collect::<Vec<_>>(),
                )
            }
            _ => None,
        })
        .flatten()
        .collect()
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
                + closure.counts.sonatina_prepared
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
    explained_postopt_origins: &BTreeSet<String>,
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
        + closure.counts.sonatina_prepared
        > 0;
    let has_prepared_only = closure.counts.sonatina_prepared > 0
        && closure.counts.hir == 0
        && closure.counts.mir == 0
        && closure.counts.sonatina_pre == 0
        && closure.counts.sonatina_post == 0;
    let has_expected_synthetic = closure.edges.iter().any(|edge| edge.generated_work)
        || closure_has_synthetic_origin(closure);
    let has_postopt_without_bytecode =
        closure.counts.sonatina_post > 0 && closure.counts.bytecode == 0;
    let postopt_explained = has_postopt_without_bytecode
        && closure_postopt_origins_are_explained(closure, explained_postopt_origins);
    let has_lowering_target = closure.counts.mir
        + closure.counts.sonatina_pre
        + closure.counts.sonatina_post
        + closure.counts.sonatina_prepared
        + closure.counts.bytecode
        > 0;
    let same_span_sibling_reaches_target = !has_lowering_target
        && direct_input_spans.iter().any(|span| {
            source_span_signature(span, input_path)
                .and_then(|signature| span_groups.groups.get(&signature))
                .is_some_and(|group| group.closures_with_targets > 0)
        });
    if has_postopt_without_bytecode && !postopt_explained {
        symptoms.insert(ClosureAuditSymptom::MissingBytecode);
        notes.push("post-opt closure reaches no final bytecode PC".to_string());
    }
    if has_ir && closure.counts.bytecode > 0 && direct_input_spans.is_empty() {
        if has_expected_synthetic {
            symptoms.insert(ClosureAuditSymptom::SourceUnavailableSynthetic);
            notes.push("bytecode-linked synthetic/backend closure has no direct source span; this is expected for compiler-generated wrapper or backend-prepared work".to_string());
        } else if has_prepared_only {
            notes.push("bytecode is linked to EVM prepared/codegen identity, but no optimized Sonatina or source lineage is present for this prepared instruction".to_string());
        } else {
            symptoms.insert(ClosureAuditSymptom::MissingSourceUnexplained);
            notes.push(
                "bytecode-linked closure has no direct span in the audited input file".to_string(),
            );
        }
    }

    let primary = if closure.traversal.truncated {
        ClosureAuditPrimary::TruncatedUnknown
    } else if has_postopt_without_bytecode && postopt_explained {
        if let Some(gap) = &closure.gap {
            notes.push(gap.clone());
        }
        ClosureAuditPrimary::OptimizerExplained
    } else if has_postopt_without_bytecode {
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
    } else if has_prepared_only && closure.counts.bytecode > 0 {
        ClosureAuditPrimary::PreparedOnly
    } else if !direct_input_spans.is_empty() && closure.counts.bytecode > 0 {
        let exact = direct_input_spans.len() == 1
            && closure.counts.bytecode == 1
            && closure.counts.hir <= 1
            && closure.counts.mir <= 1
            && closure.counts.sonatina_pre <= 1
            && closure.counts.sonatina_post <= 1
            && closure.counts.sonatina_prepared <= 1;
        if exact {
            ClosureAuditPrimary::GoodExact
        } else {
            ClosureAuditPrimary::GoodManyToMany
        }
    } else if !direct_input_spans.is_empty()
        && closure.counts.mir == 0
        && closure.counts.sonatina_pre == 0
        && closure.counts.sonatina_post == 0
        && closure.counts.sonatina_prepared == 0
        && closure.counts.bytecode == 0
    {
        ClosureAuditPrimary::SourceOnlyExpected
    } else if has_expected_synthetic {
        ClosureAuditPrimary::ExpectedSynthetic
    } else if has_ir && closure.counts.bytecode > 0 && direct_input_spans.is_empty() {
        ClosureAuditPrimary::MissingSourceUnexplained
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

fn closure_postopt_origins_are_explained(
    closure: &OriginClosure,
    explained_postopt_origins: &BTreeSet<String>,
) -> bool {
    storage_postopt_origins_are_explained(&closure.keys, explained_postopt_origins)
}

fn storage_postopt_origins_are_explained(
    keys: &[String],
    explained_postopt_origins: &BTreeSet<String>,
) -> bool {
    let postopt_keys = keys
        .iter()
        .filter(|key| key.starts_with("sonatina.post"))
        .collect::<Vec<_>>();
    !postopt_keys.is_empty()
        && postopt_keys
            .iter()
            .all(|key| explained_postopt_origins.contains(key.as_str()))
}

fn closure_has_synthetic_origin(closure: &OriginClosure) -> bool {
    closure
        .keys
        .iter()
        .any(|key| origin_storage_key_is_synthetic(key))
}

fn origin_storage_key_is_synthetic(storage_key: &str) -> bool {
    storage_key.contains("__synthetic")
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
            if !allows_origin_closure_edge(edge, OriginClosureTraversalMode::PreciseAudit) {
                continue;
            }
            let edge_key = (
                edge.from.canonical_storage_key(),
                edge.to.canonical_storage_key(),
                format!("{:?}", edge.label),
            );
            if seen_edges.insert(edge_key) {
                edges_out.push(closure_edge(edge));
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

fn closure_edge(edge: &OriginEdgeFact) -> OriginClosureEdge {
    OriginClosureEdge {
        label: format!(
            "{:?} / {}",
            edge.label,
            edge.introduced_by
                .map(|phase| format!("{phase:?}"))
                .unwrap_or_else(|| "unknown".to_string())
        ),
        from: display_key(&edge.from),
        to: display_key(&edge.to),
        traversal_class: edge.traversal_class(),
        generated_work: edge.is_generated_work_display_hint(),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum OriginClosureTraversalMode {
    PreciseAudit,
}

fn allows_origin_closure_edge(edge: &OriginEdgeFact, mode: OriginClosureTraversalMode) -> bool {
    match mode {
        OriginClosureTraversalMode::PreciseAudit => matches!(
            edge.traversal_class(),
            OriginEdgeTraversalClass::ExactAttribution | OriginEdgeTraversalClass::SnapshotAlias
        ),
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
            origin_key: origin.canonical_storage_key(),
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
    use trace_facts::{CompilerPhase, InstructionFact, OriginEdgeFact, OriginEdgeLabel};

    #[test]
    fn source_owner_matching_requires_suffix_boundary() {
        assert!(source_owner_matches_input(
            "file:///tmp/fib_demo.fe",
            "fib_demo.fe"
        ));
        assert!(source_owner_matches_input(
            "package:fib_demo.fe",
            "fib_demo.fe"
        ));
        assert!(source_owner_matches_input(
            "file:///tmp/contracts/fib_demo.fe",
            "/tmp/contracts/fib_demo.fe"
        ));
        assert!(!source_owner_matches_input(
            "file:///tmp/not_fib_demo.fe",
            "fib_demo.fe"
        ));
        assert!(!source_owner_matches_input(
            "package:not_fib_demo.fe",
            "fib_demo.fe"
        ));
    }

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
    fn closure_roots_exclude_bytecode_with_only_contextual_frontend_edges() {
        let pc = test_key("bytecode.pc", "runtime", "pc:0");
        let runtime = test_key("runtime.stmt", "runtime", "block:0:stmt:0");
        let function = test_key("bytecode.function", "runtime", "function");
        let edges = [OriginEdgeFact::new(
            pc.clone(),
            runtime,
            OriginEdgeLabel::LoweredFrom,
            Some(CompilerPhase::BytecodeEmission),
        )];
        let instruction = InstructionFact::new(pc.clone(), function, 0, "PUSH0");
        let index = test_index(&edges, [(pc.clone(), &instruction)]);

        let roots = closure_roots("fib_demo.fe", &index, &empty_loop_report());

        assert!(!roots.contains(&pc));
    }

    #[test]
    fn precise_closure_crosses_snapshot_alias() {
        let post = test_key("sonatina.postopt.inst", "sonatina", "inst:post");
        let pre = test_key("sonatina.preopt.inst", "sonatina", "inst:pre");
        let source = test_key("hir.expr", "body", "expr:0");
        let edges = [
            OriginEdgeFact::new(
                post.clone(),
                pre.clone(),
                OriginEdgeLabel::PreservedSnapshotIdentity,
                Some(CompilerPhase::SonatinaPostOpt),
            ),
            OriginEdgeFact::new(
                pre.clone(),
                source.clone(),
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::SonatinaPreOpt),
            ),
        ];
        let index = test_index(&edges, []);

        let closure = origin_closure(&post, &index);

        assert!(closure.keys.contains(&post));
        assert!(closure.keys.contains(&pre));
        assert!(closure.keys.contains(&source));
    }

    #[test]
    fn precise_closure_rejects_backend_prepared_and_synthetic_edges() {
        let pc = test_key("bytecode.pc", "runtime", "pc:0");
        let runtime = test_key("runtime.stmt", "runtime", "block:0:stmt:0");
        let synthetic = test_key("backend.synthetic", "runtime", "helper:0");
        let edges = [
            OriginEdgeFact::new(
                pc.clone(),
                runtime.clone(),
                OriginEdgeLabel::BackendPrepared,
                Some(CompilerPhase::BytecodeEmission),
            ),
            OriginEdgeFact::new(
                pc.clone(),
                synthetic.clone(),
                OriginEdgeLabel::SyntheticFor,
                Some(CompilerPhase::Backend),
            ),
        ];
        let index = test_index(&edges, []);

        let closure = origin_closure(&pc, &index);

        assert!(closure.keys.contains(&pc));
        assert!(!closure.keys.contains(&runtime));
        assert!(!closure.keys.contains(&synthetic));
    }

    #[test]
    fn explanation_rails_show_synthetic_edges_without_exact_membership() {
        let hir = test_key("hir.expr", "body", "expr:0");
        let synthetic_mir = test_key("runtime.stmt", "runtime", "block:0:stmt:0");
        let edges = [OriginEdgeFact::new(
            synthetic_mir.clone(),
            hir.clone(),
            OriginEdgeLabel::SyntheticFor,
            Some(CompilerPhase::Mir),
        )];
        let index = test_index(&edges, []);

        let exact_from_hir = origin_closure(&hir, &index);
        let hir_rails = origin_explanation_rails(&exact_from_hir.keys, &index);
        assert!(exact_from_hir.keys.contains(&hir));
        assert!(!exact_from_hir.keys.contains(&synthetic_mir));
        assert!(hir_rails.generated.contains(&synthetic_mir));

        let exact_from_synthetic = origin_closure(&synthetic_mir, &index);
        let synthetic_rails = origin_explanation_rails(&exact_from_synthetic.keys, &index);
        assert!(exact_from_synthetic.keys.contains(&synthetic_mir));
        assert!(!exact_from_synthetic.keys.contains(&hir));
        assert!(synthetic_rails.generated.contains(&hir));
    }

    #[test]
    fn component_classes_are_policy_specific_connected_islands() {
        let source = test_key("source.span", "body", "span:0");
        let hir = test_key("hir.expr", "body", "expr:0");
        let synthetic_mir = test_key("runtime.stmt", "runtime", "block:0:stmt:0");
        let bytecode = test_key("bytecode.pc", "runtime", "pc:0");
        let prepared = test_key("sonatina.evm.prepared.inst", "runtime", "inst:0");
        let code_object = test_key("code.object", "runtime", "runtime");
        let edges = [
            OriginEdgeFact::new(
                hir.clone(),
                source.clone(),
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::Hir),
            ),
            OriginEdgeFact::new(
                synthetic_mir.clone(),
                hir.clone(),
                OriginEdgeLabel::SyntheticFor,
                Some(CompilerPhase::Mir),
            ),
            OriginEdgeFact::new(
                bytecode.clone(),
                prepared.clone(),
                OriginEdgeLabel::BackendPrepared,
                Some(CompilerPhase::Backend),
            ),
            OriginEdgeFact::new(
                bytecode.clone(),
                code_object.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            ),
        ];
        let index = test_index(&edges, []);

        let classes = component_classes_for_index(&index);

        assert_eq!(
            prefixed_classes(&classes, &source, "exact-c-"),
            prefixed_classes(&classes, &hir, "exact-c-")
        );
        assert!(prefixed_classes(&classes, &synthetic_mir, "exact-c-").is_empty());
        assert!(prefixed_classes(&classes, &source, "generated-c-").is_empty());
        assert_eq!(
            prefixed_classes(&classes, &hir, "generated-c-"),
            prefixed_classes(&classes, &synthetic_mir, "generated-c-")
        );
        assert_eq!(
            prefixed_classes(&classes, &bytecode, "prepared-c-"),
            prefixed_classes(&classes, &prepared, "prepared-c-")
        );
        assert!(prefixed_classes(&classes, &prepared, "context-c-").is_empty());
        assert_eq!(
            prefixed_classes(&classes, &bytecode, "structural-c-"),
            prefixed_classes(&classes, &code_object, "structural-c-")
        );
    }

    #[test]
    fn broad_generated_components_do_not_emit_row_level_classes() {
        let source_file = test_key("source.file", "body", "file:0");
        let synthetic_mir = test_key("runtime.stmt", "runtime", "block:0:stmt:0");
        let edges = [OriginEdgeFact::new(
            synthetic_mir.clone(),
            source_file.clone(),
            OriginEdgeLabel::SyntheticFor,
            Some(CompilerPhase::Mir),
        )];
        let index = test_index(&edges, []);

        let classes = component_classes_for_index(&index);

        assert!(prefixed_classes(&classes, &source_file, "generated-c-").is_empty());
        assert!(prefixed_classes(&classes, &synthetic_mir, "generated-c-").is_empty());
    }

    #[test]
    fn closure_audit_flags_optimized_gap_without_calling_it_dead() {
        let mut closure = test_closure();
        closure.counts.sonatina_post = 2;
        closure.gap = closure_gap_note(&closure.counts, false);

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

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
    fn closure_audit_treats_optimizer_explanation_as_explained_not_missing() {
        let mut closure = test_closure();
        closure.counts.sonatina_post = 1;
        closure.keys = vec!["sonatina.postopt.inst:demo:inst:0".to_string()];
        let explained = BTreeSet::from(["sonatina.postopt.inst:demo:inst:0".to_string()]);
        closure.gap = closure_gap_note(&closure.counts, true);

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &explained);

        assert_eq!(entry.primary, ClosureAuditPrimary::OptimizerExplained);
        assert!(!entry.suspicious);
        assert!(
            !entry
                .symptoms
                .contains(&ClosureAuditSymptom::MissingBytecode)
        );
        assert!(entry.notes.iter().any(|note| note.contains("explained")));
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
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

        assert_eq!(entry.primary, ClosureAuditPrimary::GoodManyToMany);
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::Bytecode);
        assert!(entry.symptoms.is_empty());
    }

    #[test]
    fn closure_audit_marks_source_to_bytecode_exact() {
        let mut closure = test_closure();
        closure.counts.bytecode = 1;

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

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
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

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

        let entry = audit_closure("fib_demo.fe", 24, &source_only, &groups, &BTreeSet::new());

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

        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

        assert_eq!(entry.primary, ClosureAuditPrimary::PreoptElisionGap);
        assert!(entry.suspicious);
    }

    #[test]
    fn closure_audit_flags_lowered_no_target_unexplained() {
        let mut closure = test_closure();
        closure.counts.sonatina_pre = 0;

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

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
            traversal_class: OriginEdgeTraversalClass::Contextual,
            generated_work: true,
        });
        let groups = groups_for_closure(&closure);

        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

        assert_eq!(entry.primary, ClosureAuditPrimary::ExpectedSynthetic);
        assert!(
            entry
                .symptoms
                .contains(&ClosureAuditSymptom::SourceUnavailableSynthetic)
        );
        assert!(!entry.suspicious);
    }

    #[test]
    fn closure_audit_downgrades_synthetic_runtime_owner_missing_source() {
        let mut closure = test_closure();
        closure.keys = vec![
            "runtime.stmt:runtime-instance:__synthetic:contract_recv_abi:demo:block:0:stmt:0"
                .to_string(),
            "sonatina.postopt.inst:demo:function:FuncRef(1):inst:InstId(0)".to_string(),
            "bytecode.pc:demo:pc:0".to_string(),
        ];
        closure.source_spans.clear();
        closure.counts.bytecode = 1;
        closure.counts.sonatina_post = 1;

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

        assert_eq!(entry.primary, ClosureAuditPrimary::ExpectedSynthetic);
        assert!(
            entry
                .symptoms
                .contains(&ClosureAuditSymptom::SourceUnavailableSynthetic)
        );
        assert!(!entry.suspicious);
    }

    #[test]
    fn closure_audit_does_not_call_source_owned_hir_synthetic() {
        let mut closure = test_closure();
        closure.counts.mir = 0;
        closure.counts.sonatina_pre = 0;
        closure.edges.push(OriginClosureEdge {
            label: "SyntheticFor / Mir".to_string(),
            from: "runtime.stmt synthetic".to_string(),
            to: "hir.expr source".to_string(),
            traversal_class: OriginEdgeTraversalClass::Synthetic,
            generated_work: true,
        });
        let groups = groups_for_closure(&closure);

        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

        assert_eq!(entry.primary, ClosureAuditPrimary::SourceOnlyExpected);
        assert!(!entry.suspicious);
    }

    #[test]
    fn web_and_origin_closure_production_do_not_match_raw_edge_labels() {
        let query_crate = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let repo_root = query_crate
            .parent()
            .and_then(std::path::Path::parent)
            .expect("trace-query crate should live under crates/");
        let files = [
            query_crate.join("src/origin_closure.rs"),
            repo_root.join("crates/fe/src/trace/trace_web_demo.rs"),
        ];
        let forbidden = [
            "OriginEdgeLabel::",
            "LoweredFrom",
            "EmittedFrom",
            "BackendPrepared",
            "SyntheticFor",
            "\"lowered_from\"",
            "\"emitted_from\"",
            "\"backend_prepared\"",
            "\"synthetic_for\"",
        ];

        for file in files {
            let source = std::fs::read_to_string(&file)
                .unwrap_or_else(|err| panic!("failed to read {}: {err}", file.display()));
            let production = source
                .split("\n#[cfg(test)]")
                .next()
                .expect("split always yields one segment");
            for pattern in forbidden {
                assert!(
                    !production.contains(pattern),
                    "{} production code must use OriginEdgeFact::traversal_class() / TraceReachabilityPolicy, not raw edge-label pattern {pattern:?}",
                    file.display()
                );
            }
        }
    }

    #[test]
    fn closure_audit_flags_unexplained_missing_source() {
        let mut closure = test_closure();
        closure.source_spans.clear();
        closure.counts.bytecode = 1;

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

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
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

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
            sonatina_prepared: 0,
            bytecode: 0,
        };

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

        assert_eq!(entry.primary, ClosureAuditPrimary::Unclassified);
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::Source);
        assert!(entry.suspicious);
    }

    #[test]
    fn closure_audit_marks_prepared_only_bytecode() {
        let mut closure = test_closure();
        closure.source_spans.clear();
        closure.counts = OriginClosureCounts {
            hir: 0,
            mir: 0,
            sonatina_pre: 0,
            sonatina_post: 0,
            sonatina_prepared: 2,
            bytecode: 3,
        };

        let groups = groups_for_closure(&closure);
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups, &BTreeSet::new());

        assert_eq!(entry.primary, ClosureAuditPrimary::PreparedOnly);
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::Bytecode);
        assert!(!entry.suspicious);
    }

    fn test_key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn test_index<'a, I>(edges: &'a [OriginEdgeFact], instructions: I) -> OriginClosureIndex<'a>
    where
        I: IntoIterator<Item = (OriginExportKey, &'a InstructionFact)>,
    {
        let mut edges_by_from = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut edges_by_to = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        for edge in edges {
            edges_by_from
                .entry(edge.from.clone())
                .or_default()
                .push(edge);
            edges_by_to.entry(edge.to.clone()).or_default().push(edge);
        }
        let instructions = instructions.into_iter().collect();
        OriginClosureIndex {
            edges_by_from,
            edges_by_to,
            instructions,
            source_spans: BTreeMap::new(),
            edge_count: edges.len(),
            instruction_count: 0,
        }
    }

    fn prefixed_classes(
        classes: &BTreeMap<String, Vec<String>>,
        key: &OriginExportKey,
        prefix: &str,
    ) -> BTreeSet<String> {
        classes
            .get(&key.canonical_storage_key())
            .into_iter()
            .flatten()
            .filter(|class| class.starts_with(prefix))
            .cloned()
            .collect()
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
            generated_keys: Vec::new(),
            contextual_keys: Vec::new(),
            structural_keys: Vec::new(),
            counts: OriginClosureCounts {
                hir: 1,
                mir: 1,
                sonatina_pre: 1,
                sonatina_post: 0,
                sonatina_prepared: 0,
                bytecode: 0,
            },
            traversal: test_traversal(),
            gap: None,
            edges: Vec::new(),
            source_spans: vec![OriginClosureSourceSpan {
                origin_key: "hir.expr:body:11".to_string(),
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
