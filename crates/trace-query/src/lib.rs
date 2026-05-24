use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use common::origin::OriginExportKey;
use introspection_config::FeToolingConfig;
use serde::{Deserialize, Serialize};
use trace_facts::{
    CompilerEventKind, GasKind, InstructionCategory, InstructionFact, OriginEdgeLabel, StorageFact,
    StorageLocation, TraceDataSource, TraceFact, TraceMetadata, TraceSnapshot,
};

pub type QueryResult<T> = Result<T, QueryError>;

pub trait IntrospectionService {
    fn status(&self) -> IntrospectionStatus;
    fn effective_config(&self) -> FeToolingConfig;
    fn loop_cost(&self, request: LoopCostRequest) -> QueryResult<LoopCostReport>;
    fn explain_local(&self, request: ExplainLocalRequest) -> QueryResult<ExplainLocalReport>;
    fn gas_breakdown(&self, request: GasBreakdownRequest) -> QueryResult<GasBreakdownReport>;
}

#[derive(Clone, Debug)]
pub struct TraceIntrospectionService {
    snapshot: TraceSnapshot,
    config: FeToolingConfig,
}

impl TraceIntrospectionService {
    pub fn new(snapshot: TraceSnapshot) -> Self {
        Self::with_config(snapshot, FeToolingConfig::default())
    }

    pub fn with_config(snapshot: TraceSnapshot, config: FeToolingConfig) -> Self {
        Self { snapshot, config }
    }

    pub fn snapshot(&self) -> &TraceSnapshot {
        &self.snapshot
    }
}

impl IntrospectionService for TraceIntrospectionService {
    fn status(&self) -> IntrospectionStatus {
        IntrospectionStatus {
            trace_hash: self.snapshot.trace_hash().to_string(),
            fact_count: self.snapshot.validation().summary.fact_count,
            instruction_count: self.snapshot.validation().summary.instruction_count,
            data_source: data_source_label(self.snapshot.metadata()),
            target: self.snapshot.metadata().target.clone(),
            config_hash: self.config.stable_hash(),
        }
    }

    fn effective_config(&self) -> FeToolingConfig {
        self.config.clone()
    }

    fn loop_cost(&self, request: LoopCostRequest) -> QueryResult<LoopCostReport> {
        let index = TraceIndex::new(&self.snapshot);
        let loop_key = request.loop_key.or_else(|| index.loop_key.clone());
        let instructions = if let Some(loop_key) = &loop_key {
            index
                .loop_members
                .get(loop_key)
                .cloned()
                .unwrap_or_default()
        } else {
            BTreeSet::new()
        };
        let available = !instructions.is_empty();
        let instruction_keys = if available {
            instructions
        } else {
            index.all_instruction_keys()
        };
        let summary = index.category_counts(&instruction_keys);
        let repeated_zero_extends = index.zero_extends_by_local(&instruction_keys);
        let storage_impacts = index.storage_impacts(&instruction_keys);
        let findings = loop_cost_findings(
            available,
            &summary,
            &repeated_zero_extends,
            &storage_impacts,
        );

        Ok(LoopCostReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            available,
            unavailable_reason: (!available).then(|| {
                "compiler-derived LoopMembershipFact rows are not emitted yet, so the report cannot truthfully isolate the hot loop".to_string()
            }),
            loop_key,
            loop_label: index.loop_key.as_ref().map(|key| index.label(key)),
            summary,
            repeated_zero_extends,
            storage_impacts,
            findings,
            confidence: if available {
                Confidence::High
            } else {
                Confidence::Unknown
            },
        })
    }

    fn explain_local(&self, request: ExplainLocalRequest) -> QueryResult<ExplainLocalReport> {
        let index = TraceIndex::new(&self.snapshot);
        let local_key = index.locals.get(&request.local).cloned();
        let loop_instructions = index.active_loop_instructions();
        let available_locals = index.locals.keys().take(20).cloned().collect::<Vec<_>>();

        let Some(local_key) = local_key else {
            return Ok(ExplainLocalReport {
                metadata: ReportMetadata::from_snapshot(&self.snapshot),
                local: request.local,
                local_key: None,
                storage_history: Vec::new(),
                related_instructions: Vec::new(),
                zero_extends: Vec::new(),
                findings: vec![Insight::info(
                    "Local explanation unavailable",
                    "compiler-derived source-local display facts and MIR-to-codegen origin edges are not emitted yet",
                )],
                available: false,
                unavailable_reason: Some(
                    "source-local display facts or matching local identity are missing".to_string(),
                ),
                available_locals,
                confidence: Confidence::Unknown,
            });
        };

        let storage_history = index.storage_for(&local_key);
        let related_instructions = index.related_instruction_edges(&local_key, &loop_instructions);
        let zero_extends = related_instructions
            .iter()
            .filter(|related| related.edge_label == OriginEdgeLabel::IntegerLegalizationFor)
            .cloned()
            .collect::<Vec<_>>();
        let mut findings = Vec::new();
        if storage_history
            .iter()
            .any(|step| step.location.contains("stack slot"))
        {
            findings.push(Insight::hint(
                "Stack-resident local",
                "storage facts show this local was assigned a backend stack slot",
            ));
        }
        if !zero_extends.is_empty() {
            findings.push(Insight::hint(
                "Repeated integer normalization",
                "compiler events or origin edges link zero-extend instructions to this local",
            ));
        }

        Ok(ExplainLocalReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            local: request.local,
            local_key: Some(local_key),
            storage_history,
            related_instructions,
            zero_extends,
            findings,
            available: true,
            unavailable_reason: None,
            available_locals,
            confidence: Confidence::High,
        })
    }

    fn gas_breakdown(&self, request: GasBreakdownRequest) -> QueryResult<GasBreakdownReport> {
        let index = TraceIndex::new(&self.snapshot);
        let rows = index.gas_rows(&request.schedule);
        let total_gas = rows.iter().map(|row| row.gas).sum::<u64>();
        let available = !rows.is_empty();
        Ok(GasBreakdownReport {
            metadata: ReportMetadata::from_snapshot(&self.snapshot),
            schedule: request.schedule,
            available,
            total_gas: available.then_some(total_gas),
            rows,
            findings: if available {
                vec![Insight::info(
                    "Static gas estimate",
                    "gas rows are static opcode-table estimates under the named EVM schedule",
                )]
            } else {
                vec![Insight::info(
                    "Static gas unavailable",
                    "opcode gas facts are not present in this trace snapshot",
                )]
            },
            confidence: if available {
                Confidence::Medium
            } else {
                Confidence::Unknown
            },
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct IntrospectionStatus {
    pub trace_hash: String,
    pub fact_count: usize,
    pub instruction_count: usize,
    pub data_source: String,
    pub target: String,
    pub config_hash: String,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoopCostRequest {
    pub loop_key: Option<OriginExportKey>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExplainLocalRequest {
    pub local: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasBreakdownRequest {
    pub schedule: String,
}

impl Default for GasBreakdownRequest {
    fn default() -> Self {
        Self {
            schedule: "cancun".to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraceQueryHttpRequest {
    pub auth_token: String,
    pub uri: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub config_hash: Option<String>,
    #[serde(flatten)]
    pub query: TraceQueryRequest,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TraceQueryRequest {
    LoopCost {
        #[serde(default)]
        loop_key: Option<OriginExportKey>,
    },
    ExplainLocal {
        local: String,
    },
    GasBreakdown {
        #[serde(default = "default_gas_schedule")]
        schedule: String,
    },
}

impl TraceQueryRequest {
    pub fn loop_cost() -> Self {
        Self::LoopCost { loop_key: None }
    }

    pub fn explain_local(local: impl Into<String>) -> Self {
        Self::ExplainLocal {
            local: local.into(),
        }
    }

    pub fn gas_breakdown(schedule: impl Into<String>) -> Self {
        Self::GasBreakdown {
            schedule: schedule.into(),
        }
    }
}

fn default_gas_schedule() -> String {
    GasBreakdownRequest::default().schedule
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "status", rename_all = "snake_case")]
pub enum TraceQueryHttpResponse {
    Ok { report: TraceQueryReport },
    Error { reason: String },
    Unauthorized { reason: String },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", content = "report", rename_all = "snake_case")]
pub enum TraceQueryReport {
    LoopCost(LoopCostReport),
    ExplainLocal(ExplainLocalReport),
    GasBreakdown(GasBreakdownReport),
}

pub fn run_trace_query(
    service: &impl IntrospectionService,
    request: TraceQueryRequest,
) -> QueryResult<TraceQueryReport> {
    match request {
        TraceQueryRequest::LoopCost { loop_key } => service
            .loop_cost(LoopCostRequest { loop_key })
            .map(TraceQueryReport::LoopCost),
        TraceQueryRequest::ExplainLocal { local } => service
            .explain_local(ExplainLocalRequest { local })
            .map(TraceQueryReport::ExplainLocal),
        TraceQueryRequest::GasBreakdown { schedule } => service
            .gas_breakdown(GasBreakdownRequest { schedule })
            .map(TraceQueryReport::GasBreakdown),
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoopCostReport {
    pub metadata: ReportMetadata,
    pub available: bool,
    pub unavailable_reason: Option<String>,
    pub loop_key: Option<OriginExportKey>,
    pub loop_label: Option<String>,
    pub summary: InstructionSummary,
    pub repeated_zero_extends: Vec<LocalInstructionGroup>,
    pub storage_impacts: Vec<StorageImpact>,
    pub findings: Vec<Insight>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExplainLocalReport {
    pub metadata: ReportMetadata,
    pub local: String,
    pub local_key: Option<OriginExportKey>,
    pub storage_history: Vec<StorageStep>,
    pub related_instructions: Vec<RelatedInstruction>,
    pub zero_extends: Vec<RelatedInstruction>,
    pub findings: Vec<Insight>,
    pub available: bool,
    pub unavailable_reason: Option<String>,
    pub available_locals: Vec<String>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasBreakdownReport {
    pub metadata: ReportMetadata,
    pub schedule: String,
    pub available: bool,
    pub total_gas: Option<u64>,
    pub rows: Vec<GasBreakdownRow>,
    pub findings: Vec<Insight>,
    pub confidence: Confidence,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReportMetadata {
    pub trace_hash: String,
    pub data_source: String,
    pub target: String,
    pub input_path: String,
    pub compiler_commit: String,
    pub flags: Vec<String>,
}

impl ReportMetadata {
    fn from_snapshot(snapshot: &TraceSnapshot) -> Self {
        let metadata = snapshot.metadata();
        Self {
            trace_hash: snapshot.trace_hash().to_string(),
            data_source: data_source_label(metadata),
            target: metadata.target.clone(),
            input_path: metadata.input_path.clone(),
            compiler_commit: metadata.compiler_commit.clone(),
            flags: metadata.flags.clone(),
        }
    }

    pub fn function_label(&self) -> Option<&str> {
        self.flags
            .iter()
            .find_map(|flag| flag.strip_prefix("function="))
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct InstructionSummary {
    pub total_instructions: usize,
    pub loads: usize,
    pub stores: usize,
    pub zero_extends: usize,
    pub stack_loads: usize,
    pub stack_stores: usize,
    pub moves: usize,
    pub branches: usize,
    pub jumps: usize,
    pub arithmetic: usize,
}

impl InstructionSummary {
    pub fn branch_like(&self) -> usize {
        self.branches + self.jumps
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LocalInstructionGroup {
    pub local: String,
    pub instructions: Vec<InstructionRow>,
    pub reason: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StorageImpact {
    pub local: String,
    pub storage_history: Vec<StorageStep>,
    pub loads: usize,
    pub stores: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StorageStep {
    pub phase: String,
    pub location: String,
    pub reason: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RelatedInstruction {
    pub instruction: InstructionRow,
    pub edge_label: OriginEdgeLabel,
    pub reason: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct InstructionRow {
    pub key: OriginExportKey,
    pub index: u32,
    pub mnemonic: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasBreakdownRow {
    pub subject: OriginExportKey,
    pub gas: u64,
    pub label: String,
    pub confidence: String,
    pub source: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Confidence {
    Exact,
    High,
    Medium,
    Low,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Insight {
    pub severity: InsightSeverity,
    pub title: String,
    pub summary: String,
}

impl Insight {
    pub fn info(title: impl Into<String>, summary: impl Into<String>) -> Self {
        Self {
            severity: InsightSeverity::Info,
            title: title.into(),
            summary: summary.into(),
        }
    }

    pub fn hint(title: impl Into<String>, summary: impl Into<String>) -> Self {
        Self {
            severity: InsightSeverity::Hint,
            title: title.into(),
            summary: summary.into(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum InsightSeverity {
    Info,
    Hint,
    Warning,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum QueryError {
    InvalidRequest(String),
}

impl fmt::Display for QueryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidRequest(message) => f.write_str(message),
        }
    }
}

impl std::error::Error for QueryError {}

struct TraceIndex<'a> {
    snapshot: &'a TraceSnapshot,
    loop_key: Option<OriginExportKey>,
    loop_members: BTreeMap<OriginExportKey, BTreeSet<OriginExportKey>>,
    locals: BTreeMap<String, OriginExportKey>,
    instructions: BTreeMap<OriginExportKey, &'a InstructionFact>,
}

impl<'a> TraceIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut loop_key = None;
        let mut loop_members: BTreeMap<OriginExportKey, BTreeSet<OriginExportKey>> =
            BTreeMap::new();
        let mut locals = BTreeMap::new();
        let mut instructions = BTreeMap::new();

        for fact in snapshot.facts() {
            match fact {
                TraceFact::LoopMembership(membership) => {
                    loop_key.get_or_insert_with(|| membership.loop_key.clone());
                    loop_members
                        .entry(membership.loop_key.clone())
                        .or_default()
                        .insert(membership.instruction.clone());
                }
                TraceFact::OriginNode(node) if node.key.kind() == "runtime.local" => {
                    locals.insert(local_display_name(&node.key), node.key.clone());
                }
                TraceFact::Instruction(instruction) => {
                    instructions.insert(instruction.instruction.clone(), instruction);
                }
                _ => {}
            }
        }

        Self {
            snapshot,
            loop_key,
            loop_members,
            locals,
            instructions,
        }
    }

    fn active_loop_instructions(&self) -> BTreeSet<OriginExportKey> {
        self.loop_key
            .as_ref()
            .and_then(|key| self.loop_members.get(key))
            .cloned()
            .unwrap_or_default()
    }

    fn all_instruction_keys(&self) -> BTreeSet<OriginExportKey> {
        self.instructions.keys().cloned().collect()
    }

    fn instruction_row(&self, key: &OriginExportKey) -> Option<InstructionRow> {
        self.instructions
            .get(key)
            .map(|instruction| InstructionRow {
                key: instruction.instruction.clone(),
                index: instruction.index,
                mnemonic: instruction.mnemonic.clone(),
            })
    }

    fn category_counts(&self, instructions: &BTreeSet<OriginExportKey>) -> InstructionSummary {
        let mut counts = InstructionSummary {
            total_instructions: instructions.len(),
            ..Default::default()
        };
        for fact in self.snapshot.facts() {
            let TraceFact::InstructionCategory(category) = fact else {
                continue;
            };
            if !instructions.contains(&category.instruction) {
                continue;
            }
            match category.category {
                InstructionCategory::Load => counts.loads += 1,
                InstructionCategory::Store => counts.stores += 1,
                InstructionCategory::ZeroExtend => counts.zero_extends += 1,
                InstructionCategory::StackLoad => counts.stack_loads += 1,
                InstructionCategory::StackStore => counts.stack_stores += 1,
                InstructionCategory::Move => counts.moves += 1,
                InstructionCategory::Branch => counts.branches += 1,
                InstructionCategory::Jump => counts.jumps += 1,
                InstructionCategory::Arithmetic => counts.arithmetic += 1,
                _ => {}
            }
        }
        counts
    }

    fn zero_extends_by_local(
        &self,
        instructions: &BTreeSet<OriginExportKey>,
    ) -> Vec<LocalInstructionGroup> {
        let mut groups: BTreeMap<String, Vec<InstructionRow>> = BTreeMap::new();
        for fact in self.snapshot.facts() {
            let TraceFact::OriginEdge(edge) = fact else {
                continue;
            };
            if edge.label != OriginEdgeLabel::IntegerLegalizationFor
                || !instructions.contains(&edge.from)
            {
                continue;
            }
            if let Some(row) = self.instruction_row(&edge.from) {
                groups.entry(self.label(&edge.to)).or_default().push(row);
            }
        }
        groups
            .into_iter()
            .map(|(local, instructions)| {
                let reason = instructions
                    .first()
                    .and_then(|row| self.compiler_event_reason_for_output(&row.key));
                LocalInstructionGroup {
                    local,
                    instructions,
                    reason,
                }
            })
            .collect()
    }

    fn storage_impacts(&self, instructions: &BTreeSet<OriginExportKey>) -> Vec<StorageImpact> {
        let mut edge_counts: BTreeMap<OriginExportKey, (usize, usize)> = BTreeMap::new();
        for fact in self.snapshot.facts() {
            let TraceFact::OriginEdge(edge) = fact else {
                continue;
            };
            if !instructions.contains(&edge.from) {
                continue;
            }
            match edge.label {
                OriginEdgeLabel::LoadOf => edge_counts.entry(edge.to.clone()).or_default().0 += 1,
                OriginEdgeLabel::StoreOf => edge_counts.entry(edge.to.clone()).or_default().1 += 1,
                _ => {}
            }
        }

        edge_counts
            .into_iter()
            .map(|(local_key, (loads, stores))| StorageImpact {
                local: self.label(&local_key),
                storage_history: self.storage_for(&local_key),
                loads,
                stores,
            })
            .collect()
    }

    fn related_instruction_edges(
        &self,
        local_key: &OriginExportKey,
        instructions: &BTreeSet<OriginExportKey>,
    ) -> Vec<RelatedInstruction> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::OriginEdge(edge)
                    if &edge.to == local_key && instructions.contains(&edge.from) =>
                {
                    self.instruction_row(&edge.from)
                        .map(|instruction| RelatedInstruction {
                            reason: self.compiler_event_reason_for_output(&edge.from),
                            instruction,
                            edge_label: edge.label,
                        })
                }
                _ => None,
            })
            .collect()
    }

    fn storage_for(&self, key: &OriginExportKey) -> Vec<StorageStep> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::Storage(storage) if &storage.subject == key => {
                    Some(storage_step(storage))
                }
                _ => None,
            })
            .collect()
    }

    fn gas_rows(&self, schedule: &str) -> Vec<GasBreakdownRow> {
        self.snapshot
            .facts()
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::GasCost(gas)
                    if gas.schedule.as_str() == schedule
                        && gas.gas_kind == GasKind::OpcodeStatic =>
                {
                    Some(GasBreakdownRow {
                        subject: gas.subject.clone(),
                        gas: gas.gas,
                        label: self
                            .instruction_row(&gas.subject)
                            .map(|row| format!("pc[{}] {}", row.index, row.mnemonic))
                            .unwrap_or_else(|| self.label(&gas.subject)),
                        confidence: format!("{:?}", gas.confidence),
                        source: format!("{:?}", gas.source),
                    })
                }
                _ => None,
            })
            .collect()
    }

    fn compiler_event_reason_for_output(&self, output: &OriginExportKey) -> Option<String> {
        self.snapshot.facts().iter().find_map(|fact| match fact {
            TraceFact::CompilerEvent(event)
                if event.kind == CompilerEventKind::InsertIntegerZeroExtend
                    && event.outputs.iter().any(|candidate| candidate == output) =>
            {
                event
                    .reason
                    .as_ref()
                    .map(|reason| reason.as_str().to_string())
            }
            _ => None,
        })
    }

    fn label(&self, key: &OriginExportKey) -> String {
        match key.kind() {
            "runtime.local" => local_display_name(key),
            "loop" => key.local_key().replace(':', " "),
            _ => key.display_label(),
        }
    }
}

fn storage_step(storage: &StorageFact) -> StorageStep {
    StorageStep {
        phase: format!("{:?}", storage.phase),
        location: format_storage_location(&storage.location),
        reason: format!("{:?}", storage.reason),
    }
}

fn loop_cost_findings(
    available: bool,
    summary: &InstructionSummary,
    repeated_zero_extends: &[LocalInstructionGroup],
    storage_impacts: &[StorageImpact],
) -> Vec<Insight> {
    let mut findings = Vec::new();
    if !available {
        findings.push(Insight::info(
            "Loop cost unavailable",
            "required loop membership facts are missing",
        ));
    }
    if summary.zero_extends > 0 {
        findings.push(Insight::hint(
            "Repeated zero-extensions",
            format!(
                "{} zero-extend instructions are attributed to {} local group(s)",
                summary.zero_extends,
                repeated_zero_extends.len()
            ),
        ));
    }
    let stack_traffic: usize = storage_impacts
        .iter()
        .map(|impact| impact.loads + impact.stores)
        .sum();
    if stack_traffic > 0 {
        findings.push(Insight::hint(
            "Stack traffic in loop",
            format!("{stack_traffic} stack-related load/store edge(s) are attributed"),
        ));
    }
    findings
}

pub fn data_source_label(metadata: &TraceMetadata) -> String {
    match metadata.data_source {
        TraceDataSource::Fixture => {
            let marker = metadata.fixture_marker.as_deref().unwrap_or("unspecified");
            format!("fixture ({marker}; not compiler-derived)")
        }
        TraceDataSource::CompilerEmitted => "compiler_emitted".to_string(),
    }
}

fn local_display_name(key: &OriginExportKey) -> String {
    let local = key.local_key();
    local
        .strip_prefix("local:")
        .or_else(|| local.rsplit_once(':').map(|(_, name)| name))
        .unwrap_or(local)
        .to_string()
}

fn format_storage_location(location: &StorageLocation) -> String {
    match location {
        StorageLocation::SsaValue => "SSA value".to_string(),
        StorageLocation::MemoryPlace => "memory place".to_string(),
        StorageLocation::StackSlot { offset } => format!("stack slot sp+{offset}"),
        StorageLocation::VirtualRegister(name) => format!("virtual register {name}"),
        StorageLocation::PhysicalRegister(name) => format!("physical register {name}"),
        StorageLocation::Unknown => "unknown".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use trace_facts::{
        CategorySource, CompilerEventFact, CompilerEventKind, CompilerPhase, CompilerReason,
        EvmSchedule, GasConfidence, GasCostFact, GasKind, GasSource, InstructionCategory,
        InstructionCategoryFact, InstructionFact, LoopDerivation, LoopMembershipFact,
        OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, OriginNodeKind, StorageFact,
        StorageLocation, StorageReason, TraceBundle, TraceFact, TraceMetadata, TraceSnapshot,
    };

    use super::{
        ExplainLocalRequest, IntrospectionService, LoopCostRequest, TraceIntrospectionService,
        TraceQueryHttpRequest, TraceQueryReport, TraceQueryRequest, run_trace_query,
    };

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(key: OriginExportKey) -> TraceFact {
        TraceFact::OriginNode(OriginNodeFact::new(
            key.clone(),
            OriginNodeKind::new(key.kind()),
        ))
    }

    fn demo_service() -> TraceIntrospectionService {
        let function = key("function", "demo", "recv");
        let loop_key = key("loop", "demo", "while:i<n");
        let local = key("runtime.local", "demo", "local:b");
        let inst = key("bytecode.pc", "demo", "pc:0");
        let zext = key("bytecode.pc", "demo", "pc:1");
        let event = key("compiler.event", "demo", "event:0");
        let facts = vec![
            node(function.clone()),
            node(loop_key.clone()),
            node(local.clone()),
            node(inst.clone()),
            node(zext.clone()),
            node(event.clone()),
            TraceFact::Storage(StorageFact::new(
                local.clone(),
                CompilerPhase::Mir,
                StorageLocation::MemoryPlace,
                StorageReason::MutableLocalLowering,
            )),
            TraceFact::Storage(StorageFact::new(
                local.clone(),
                CompilerPhase::Backend,
                StorageLocation::StackSlot { offset: 24 },
                StorageReason::FrameSlot,
            )),
            TraceFact::Instruction(InstructionFact::new(
                inst.clone(),
                function.clone(),
                0,
                "lw",
            )),
            TraceFact::Instruction(InstructionFact::new(zext.clone(), function, 1, "slli")),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                inst.clone(),
                InstructionCategory::StackLoad,
                CategorySource::PosthocClassifier {
                    version: "test".to_string(),
                },
            )),
            TraceFact::InstructionCategory(InstructionCategoryFact::new(
                zext.clone(),
                InstructionCategory::ZeroExtend,
                CategorySource::PosthocClassifier {
                    version: "test".to_string(),
                },
            )),
            TraceFact::LoopMembership(LoopMembershipFact::new(
                loop_key.clone(),
                inst.clone(),
                LoopDerivation::BackendBlockMapping,
            )),
            TraceFact::LoopMembership(LoopMembershipFact::new(
                loop_key,
                zext.clone(),
                LoopDerivation::BackendBlockMapping,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                inst.clone(),
                local.clone(),
                OriginEdgeLabel::LoadOf,
                Some(CompilerPhase::Backend),
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                zext.clone(),
                local.clone(),
                OriginEdgeLabel::IntegerLegalizationFor,
                Some(CompilerPhase::Backend),
            )),
            TraceFact::CompilerEvent(CompilerEventFact::new(
                event,
                CompilerPhase::Backend,
                CompilerEventKind::InsertIntegerZeroExtend,
                vec![local],
                vec![zext.clone()],
                Some(CompilerReason::new("test reason")),
            )),
            TraceFact::GasCost(GasCostFact::new(
                inst.clone(),
                GasKind::OpcodeStatic,
                3,
                EvmSchedule::new("cancun"),
                GasConfidence::ConservativeStatic,
                GasSource::OpcodeTable,
            )),
            TraceFact::GasCost(GasCostFact::new(
                zext,
                GasKind::OpcodeStatic,
                3,
                EvmSchedule::new("cancun"),
                GasConfidence::ConservativeStatic,
                GasSource::OpcodeTable,
            )),
        ];
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::fixture(
                "abc123",
                "riscv64-demo",
                vec!["fe".to_string()],
                "fib_demo.fe",
                vec!["function=Fib.recv".to_string()],
                "query-test",
            ),
            facts,
        ))
        .unwrap();
        TraceIntrospectionService::new(snapshot)
    }

    #[test]
    fn loop_cost_report_counts_categories_and_evidence() {
        let report = demo_service()
            .loop_cost(LoopCostRequest::default())
            .unwrap();

        assert!(report.available);
        assert_eq!(report.summary.total_instructions, 2);
        assert_eq!(report.summary.zero_extends, 1);
        assert_eq!(report.summary.stack_loads, 1);
        assert_eq!(
            report.repeated_zero_extends[0].reason.as_deref(),
            Some("test reason")
        );
    }

    #[test]
    fn explain_local_report_uses_storage_and_instruction_edges() {
        let report = demo_service()
            .explain_local(ExplainLocalRequest {
                local: "b".to_string(),
            })
            .unwrap();

        assert!(report.available);
        assert_eq!(report.storage_history.len(), 2);
        assert_eq!(report.related_instructions.len(), 2);
        assert_eq!(report.zero_extends.len(), 1);
    }

    #[test]
    fn gas_breakdown_reports_static_opcode_rows() {
        let report = demo_service()
            .gas_breakdown(super::GasBreakdownRequest::default())
            .unwrap();

        assert!(report.available);
        assert_eq!(report.total_gas, Some(6));
        assert_eq!(report.schedule, "cancun");
        assert_eq!(report.rows.len(), 2);
    }

    #[test]
    fn live_http_request_is_typed_and_defaults_gas_schedule() {
        let request: TraceQueryHttpRequest = serde_json::from_str(
            r#"{
                "auth_token": "token",
                "uri": "file:///tmp/fib.fe",
                "kind": "gas_breakdown"
            }"#,
        )
        .unwrap();

        assert_eq!(request.auth_token, "token");
        assert_eq!(request.uri, "file:///tmp/fib.fe");
        assert!(matches!(
            request.query,
            TraceQueryRequest::GasBreakdown { ref schedule } if schedule == "cancun"
        ));
    }

    #[test]
    fn typed_query_dispatch_returns_matching_report_variant() {
        let service = demo_service();
        let report = run_trace_query(&service, TraceQueryRequest::loop_cost()).unwrap();

        assert!(matches!(report, TraceQueryReport::LoopCost(_)));
    }
}
