use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use trace_facts::{
    CompilerEventKind, OriginEdgeTraversalClass, RelationBundleData, RelationColumn,
    RelationColumnKind, RelationRow, RelationSchema, TraceFact, TraceSnapshot,
};

use crate::{
    GasAttributionPolicy, GasBySourceRequest, IntrospectionService, RuntimeGasBySourceRequest,
    RuntimeTraceFilterRequest, StorageAccessesBySlotRequest, TraceIntrospectionService,
    trace_index::{origin_edge_satisfies_phase_contract, prepared_lineage_event_pairs},
};

pub const CORE_DATALOG_RULES: &str = r#"
precise_reaches(a, b) :-
  exact_attribution_edge(a, b).

precise_reaches(a, b) :-
  snapshot_alias_edge(a, b).

precise_reaches(a, c) :-
  exact_attribution_edge(a, b),
  precise_reaches(b, c).

precise_reaches(a, c) :-
  snapshot_alias_edge(a, b),
  precise_reaches(b, c).

origin_reaches(a, b) :-
  precise_reaches(a, b).
"#;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DatalogBaseExport {
    pub trace_hash: String,
    pub schemas: Vec<RelationSchema>,
    pub rows: Vec<RelationRow>,
    pub rules: &'static str,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RulePackManifest {
    pub id: &'static str,
    pub version: &'static str,
    pub relation_schema: &'static str,
    pub engine: &'static str,
    pub required_relations: Vec<&'static str>,
    pub outputs: Vec<&'static str>,
    pub attribution_policies: Vec<&'static str>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct DatalogRunReport {
    pub status: &'static str,
    pub trace_hash: String,
    pub rulepack_id: String,
    pub rulepack_version: String,
    pub query: String,
    pub engine: String,
    pub relation_schema: String,
    pub attribution_policy: String,
    pub base_relation_count: usize,
    pub base_row_count: usize,
    pub output: Value,
    pub missing_evidence: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DatalogRunError {
    UnknownRulePack(String),
    UnknownQuery { rulepack: String, query: String },
    Query(String),
    Serialization(String),
}

impl fmt::Display for DatalogRunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownRulePack(rulepack) => {
                write!(f, "unknown built-in Datalog rulepack {rulepack:?}")
            }
            Self::UnknownQuery { rulepack, query } => {
                write!(f, "rulepack {rulepack:?} does not define query {query:?}")
            }
            Self::Query(err) => write!(f, "rulepack query failed: {err}"),
            Self::Serialization(err) => write!(f, "failed to serialize rulepack report: {err}"),
        }
    }
}

impl std::error::Error for DatalogRunError {}

pub fn emit_base_relations(snapshot: &TraceSnapshot) -> DatalogBaseExport {
    let mut bundle = RelationBundleData::from_snapshot(snapshot);
    append_semantic_edge_views(snapshot, &mut bundle.schemas, &mut bundle.rows);
    DatalogBaseExport {
        trace_hash: bundle.trace_hash,
        schemas: bundle.schemas,
        rows: bundle.rows,
        rules: CORE_DATALOG_RULES,
    }
}

fn append_semantic_edge_views(
    snapshot: &TraceSnapshot,
    schemas: &mut Vec<RelationSchema>,
    rows: &mut Vec<RelationRow>,
) {
    schemas.extend(semantic_edge_schemas());
    schemas.extend(representation_diagnostic_schemas());
    schemas.extend(prepared_lineage_schemas());
    let prepared_lineage_events = prepared_lineage_event_pairs(snapshot);
    for fact in snapshot.facts() {
        let TraceFact::OriginEdge(edge) = fact else {
            continue;
        };
        if !origin_edge_satisfies_phase_contract(edge, &prepared_lineage_events) {
            rows.push(RelationRow {
                relation: "suspicious_edge",
                values: vec![
                    edge.from.canonical_storage_key(),
                    edge.to.canonical_storage_key(),
                    phase_contract_rejection_reason(edge).to_string(),
                ],
            });
            continue;
        }
        rows.push(RelationRow {
            relation: semantic_edge_relation(edge),
            values: vec![
                edge.from.canonical_storage_key(),
                edge.to.canonical_storage_key(),
            ],
        });
        if matches!(edge.traversal_class(), OriginEdgeTraversalClass::Contextual)
            && edge.has_transform_claim_label()
        {
            rows.push(RelationRow {
                relation: "suspicious_edge",
                values: vec![
                    edge.from.canonical_storage_key(),
                    edge.to.canonical_storage_key(),
                    "exact_claim_classified_contextual".to_string(),
                ],
            });
        }
    }
    append_representation_diagnostic_views(snapshot, rows);
    append_prepared_lineage_views(snapshot, rows);
}

fn phase_contract_rejection_reason(edge: &trace_facts::OriginEdgeFact) -> &'static str {
    if edge.from.kind() == "sonatina.evm.prepared.inst"
        && edge.to.kind() == "sonatina.postopt.inst"
        && edge.from.owner_key() == edge.to.owner_key()
        && edge.from.local_key() == edge.to.local_key()
    {
        "raw_local_join_without_prepared_lineage"
    } else {
        "phase_contract_rejected"
    }
}

fn semantic_edge_schemas() -> Vec<RelationSchema> {
    let binary_schema = |name| RelationSchema {
        name,
        columns: vec![
            RelationColumn {
                name: "from",
                kind: RelationColumnKind::Key,
            },
            RelationColumn {
                name: "to",
                kind: RelationColumnKind::Key,
            },
        ],
    };
    let mut schemas = vec![
        binary_schema("exact_attribution_edge"),
        binary_schema("snapshot_alias_edge"),
        binary_schema("contextual_edge"),
        binary_schema("structural_edge"),
        binary_schema("backend_prepared_edge"),
        binary_schema("synthetic_edge"),
        binary_schema("unmapped_edge"),
    ];
    schemas.push(RelationSchema {
        name: "suspicious_edge",
        columns: vec![
            RelationColumn {
                name: "from",
                kind: RelationColumnKind::Key,
            },
            RelationColumn {
                name: "to",
                kind: RelationColumnKind::Key,
            },
            RelationColumn {
                name: "reason",
                kind: RelationColumnKind::Text,
            },
        ],
    });
    schemas
}

fn representation_diagnostic_schemas() -> Vec<RelationSchema> {
    vec![
        RelationSchema {
            name: "same_local_different_representation",
            columns: vec![
                RelationColumn {
                    name: "left",
                    kind: RelationColumnKind::Key,
                },
                RelationColumn {
                    name: "right",
                    kind: RelationColumnKind::Key,
                },
                RelationColumn {
                    name: "raw_local",
                    kind: RelationColumnKind::Text,
                },
            ],
        },
        RelationSchema {
            name: "candidate_lineage_hint",
            columns: vec![
                RelationColumn {
                    name: "from",
                    kind: RelationColumnKind::Key,
                },
                RelationColumn {
                    name: "to",
                    kind: RelationColumnKind::Key,
                },
                RelationColumn {
                    name: "hint",
                    kind: RelationColumnKind::Text,
                },
            ],
        },
    ]
}

fn append_representation_diagnostic_views(snapshot: &TraceSnapshot, rows: &mut Vec<RelationRow>) {
    let mut postopt_by_raw_local = BTreeMap::<(String, String), Vec<OriginExportKey>>::new();
    let mut prepared = BTreeSet::<OriginExportKey>::new();
    for fact in snapshot.facts() {
        let TraceFact::OriginNode(node) = fact else {
            continue;
        };
        if node.key.kind() == "sonatina.postopt.inst" {
            postopt_by_raw_local
                .entry((
                    node.key.owner_key().to_string(),
                    node.key.local_key().to_string(),
                ))
                .or_default()
                .push(node.key.clone());
        } else if node.key.kind() == "sonatina.evm.prepared.inst" {
            prepared.insert(node.key.clone());
        }
    }

    for prepared in prepared {
        let raw_local = (
            prepared.owner_key().to_string(),
            prepared.local_key().to_string(),
        );
        let Some(postopt_candidates) = postopt_by_raw_local.get(&raw_local) else {
            continue;
        };
        for postopt in postopt_candidates {
            rows.push(RelationRow {
                relation: "same_local_different_representation",
                values: vec![
                    prepared.canonical_storage_key(),
                    postopt.canonical_storage_key(),
                    prepared.local_key().to_string(),
                ],
            });
            rows.push(RelationRow {
                relation: "candidate_lineage_hint",
                values: vec![
                    prepared.canonical_storage_key(),
                    postopt.canonical_storage_key(),
                    "same_raw_local_id".to_string(),
                ],
            });
        }
    }
}

fn prepared_lineage_schemas() -> Vec<RelationSchema> {
    vec![RelationSchema {
        name: "prepared_lineage_fact",
        columns: vec![
            RelationColumn {
                name: "prepared",
                kind: RelationColumnKind::Key,
            },
            RelationColumn {
                name: "postopt",
                kind: RelationColumnKind::Key,
            },
            RelationColumn {
                name: "kind",
                kind: RelationColumnKind::Text,
            },
        ],
    }]
}

fn append_prepared_lineage_views(snapshot: &TraceSnapshot, rows: &mut Vec<RelationRow>) {
    let origin_edges = snapshot
        .facts()
        .iter()
        .filter_map(|fact| match fact {
            TraceFact::OriginEdge(edge) => Some(edge),
            _ => None,
        })
        .collect::<Vec<_>>();
    for fact in snapshot.facts() {
        let TraceFact::CompilerEvent(event) = fact else {
            continue;
        };
        if event.kind != CompilerEventKind::PreparedLineage {
            continue;
        }
        for postopt in &event.inputs {
            if postopt.kind() != "sonatina.postopt.inst" {
                continue;
            }
            for prepared in &event.outputs {
                if prepared.kind() != "sonatina.evm.prepared.inst" {
                    continue;
                }
                let kind = origin_edges
                    .iter()
                    .find(|edge| edge.from == *prepared && edge.to == *postopt)
                    .and_then(|edge| prepared_lineage_kind_for_edge(edge))
                    .unwrap_or("unknown");
                rows.push(RelationRow {
                    relation: "prepared_lineage_fact",
                    values: vec![
                        prepared.canonical_storage_key(),
                        postopt.canonical_storage_key(),
                        kind.to_string(),
                    ],
                });
            }
        }
    }
}

fn prepared_lineage_kind_for_edge(edge: &trace_facts::OriginEdgeFact) -> Option<&'static str> {
    match edge.traversal_class() {
        OriginEdgeTraversalClass::ExactAttribution => Some("exact_lowering"),
        OriginEdgeTraversalClass::SnapshotAlias => Some("snapshot_alias"),
        OriginEdgeTraversalClass::Synthetic => Some("synthetic_for"),
        OriginEdgeTraversalClass::Contextual if edge.is_backend_prepared_semantic_edge() => {
            Some("backend_prepared_for")
        }
        OriginEdgeTraversalClass::Contextual => Some("context_only"),
        OriginEdgeTraversalClass::Structural | OriginEdgeTraversalClass::Unmapped => None,
    }
}

fn semantic_edge_relation(edge: &trace_facts::OriginEdgeFact) -> &'static str {
    match edge.traversal_class() {
        OriginEdgeTraversalClass::ExactAttribution => "exact_attribution_edge",
        OriginEdgeTraversalClass::SnapshotAlias => "snapshot_alias_edge",
        OriginEdgeTraversalClass::Contextual if edge.is_backend_prepared_semantic_edge() => {
            "backend_prepared_edge"
        }
        OriginEdgeTraversalClass::Contextual => "contextual_edge",
        OriginEdgeTraversalClass::Structural => "structural_edge",
        OriginEdgeTraversalClass::Synthetic => "synthetic_edge",
        OriginEdgeTraversalClass::Unmapped => "unmapped_edge",
    }
}

pub fn builtin_rulepacks() -> Vec<RulePackManifest> {
    vec![
        RulePackManifest {
            id: "gas-v1",
            version: "1.0.0",
            relation_schema: "fe-datalog-base-export-v1",
            engine: "builtin-rust-derived-v1",
            required_relations: vec![
                "base_instruction",
                "base_instruction_extent",
                "base_static_gas",
                "base_execution_step",
                "base_origin_edge",
                "base_source_span",
            ],
            outputs: vec![
                "gas-by-source",
                "runtime-gas-by-source",
                "hot-path-by-iteration",
            ],
            attribution_policies: vec!["exclusive-primary", "runtime-step-exclusive"],
        },
        RulePackManifest {
            id: "runtime-v1",
            version: "1.0.0",
            relation_schema: "fe-datalog-base-export-v1",
            engine: "builtin-rust-derived-v1",
            required_relations: vec![
                "base_execution_trace_session",
                "base_execution_step",
                "base_storage_access",
                "base_memory_access",
                "base_runtime_call",
                "base_revert",
            ],
            outputs: vec![
                "storage-writes-by-source",
                "storage-accesses-by-slot",
                "call-cost-by-callsite",
                "memory-growth-by-source",
                "revert-attribution",
            ],
            attribution_policies: vec!["runtime-step-exclusive"],
        },
        RulePackManifest {
            id: "security-v1",
            version: "0.1.0",
            relation_schema: "fe-datalog-base-export-v1",
            engine: "builtin-rust-derived-v1",
            required_relations: vec![
                "base_execution_step",
                "base_storage_access",
                "base_runtime_call",
            ],
            outputs: vec!["runtime-storage-after-call"],
            attribution_policies: vec!["runtime-step-exclusive"],
        },
    ]
}

pub fn builtin_rulepack(id: &str) -> Option<RulePackManifest> {
    builtin_rulepacks()
        .into_iter()
        .find(|rulepack| rulepack.id == id)
}

pub fn run_builtin_rulepack(
    snapshot: &TraceSnapshot,
    rulepack_id: &str,
    query: &str,
) -> Result<DatalogRunReport, DatalogRunError> {
    let rulepack = builtin_rulepack(rulepack_id)
        .ok_or_else(|| DatalogRunError::UnknownRulePack(rulepack_id.to_string()))?;
    if !rulepack.outputs.iter().any(|output| output == &query) {
        return Err(DatalogRunError::UnknownQuery {
            rulepack: rulepack_id.to_string(),
            query: query.to_string(),
        });
    }

    let export = emit_base_relations(snapshot);
    let service = TraceIntrospectionService::new(snapshot.clone());
    let policy = match rulepack_id {
        "gas-v1" if query == "gas-by-source" => GasAttributionPolicy::ExclusivePrimary,
        _ => GasAttributionPolicy::RuntimeStepExclusive,
    };
    let output = match (rulepack_id, query) {
        ("gas-v1", "gas-by-source") => {
            serialize_query_report(service.gas_by_source(GasBySourceRequest {
                schedule: "cancun".to_string(),
                policy,
            }))?
        }
        ("gas-v1", "runtime-gas-by-source") => {
            serialize_query_report(service.runtime_gas_by_source(RuntimeGasBySourceRequest {
                trace_id: None,
                policy,
            }))?
        }
        ("gas-v1", "hot-path-by-iteration") => {
            serialize_query_report(service.hot_path_by_iteration(RuntimeTraceFilterRequest {
                trace_id: None,
                policy,
            }))?
        }
        ("runtime-v1", "storage-writes-by-source") => {
            serialize_query_report(service.storage_writes_by_source(RuntimeTraceFilterRequest {
                trace_id: None,
                policy,
            }))?
        }
        ("runtime-v1", "storage-accesses-by-slot") => serialize_query_report(
            service.storage_accesses_by_slot(StorageAccessesBySlotRequest {
                trace_id: None,
                slot: None,
                policy,
            }),
        )?,
        ("runtime-v1", "call-cost-by-callsite") => {
            serialize_query_report(service.call_cost_by_callsite(RuntimeTraceFilterRequest {
                trace_id: None,
                policy,
            }))?
        }
        ("runtime-v1", "memory-growth-by-source") => {
            serialize_query_report(service.memory_growth_by_source(RuntimeTraceFilterRequest {
                trace_id: None,
                policy,
            }))?
        }
        ("runtime-v1", "revert-attribution") => {
            serialize_query_report(service.revert_attribution(RuntimeTraceFilterRequest {
                trace_id: None,
                policy,
            }))?
        }
        ("security-v1", "runtime-storage-after-call") => {
            serde_json::to_value(runtime_storage_after_call(snapshot))
                .map_err(|err| DatalogRunError::Serialization(err.to_string()))?
        }
        _ => {
            return Err(DatalogRunError::UnknownQuery {
                rulepack: rulepack_id.to_string(),
                query: query.to_string(),
            });
        }
    };

    Ok(DatalogRunReport {
        status: "ok",
        trace_hash: snapshot.trace_hash().to_string(),
        rulepack_id: rulepack.id.to_string(),
        rulepack_version: rulepack.version.to_string(),
        query: query.to_string(),
        engine: rulepack.engine.to_string(),
        relation_schema: rulepack.relation_schema.to_string(),
        attribution_policy: policy.to_string(),
        base_relation_count: export.schemas.len(),
        base_row_count: export.rows.len(),
        output,
        missing_evidence: missing_rulepack_relations(&export, &rulepack),
    })
}

fn serialize_query_report<T: Serialize>(
    report: crate::QueryResult<T>,
) -> Result<Value, DatalogRunError> {
    let report = report.map_err(|err| DatalogRunError::Query(err.to_string()))?;
    serde_json::to_value(report).map_err(|err| DatalogRunError::Serialization(err.to_string()))
}

fn missing_rulepack_relations(
    export: &DatalogBaseExport,
    rulepack: &RulePackManifest,
) -> Vec<String> {
    let present = export
        .schemas
        .iter()
        .map(|schema| schema.name)
        .collect::<BTreeSet<_>>();
    rulepack
        .required_relations
        .iter()
        .filter(|relation| !present.contains(**relation))
        .map(|relation| format!("required base relation {relation} has no rows/schema"))
        .collect()
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct RuntimeStorageAfterCallReport {
    status: &'static str,
    call_count: usize,
    storage_write_count: usize,
    writes_after_first_call: usize,
    note: &'static str,
}

fn runtime_storage_after_call(snapshot: &TraceSnapshot) -> RuntimeStorageAfterCallReport {
    let mut step_indices = BTreeMap::new();
    let mut first_call_step = None;
    let mut call_count = 0;
    let mut storage_write_count = 0;
    let mut writes_after_first_call = 0;

    for fact in snapshot.facts() {
        if let TraceFact::ExecutionStep(step) = fact {
            step_indices.insert(step.step.clone(), step.step_index);
        }
    }
    for fact in snapshot.facts() {
        if let TraceFact::Call(call) = fact {
            call_count += 1;
            if let Some(index) = step_indices.get(&call.step).copied() {
                first_call_step =
                    Some(first_call_step.map_or(index, |current: u64| current.min(index)));
            }
        }
    }
    for fact in snapshot.facts() {
        if let TraceFact::StorageAccess(access) = fact
            && matches!(access.kind, trace_facts::StorageAccessKind::Write)
        {
            storage_write_count += 1;
            if let (Some(call_index), Some(write_index)) =
                (first_call_step, step_indices.get(&access.step).copied())
                && write_index > call_index
            {
                writes_after_first_call += 1;
            }
        }
    }

    RuntimeStorageAfterCallReport {
        status: "heuristic",
        call_count,
        storage_write_count,
        writes_after_first_call,
        note: "security-v1 is a heuristic derived view over runtime base relations, not a vulnerability proof",
    }
}

pub fn origin_reaches(snapshot: &TraceSnapshot) -> BTreeSet<(OriginExportKey, OriginExportKey)> {
    let mut adjacency: BTreeMap<OriginExportKey, BTreeSet<OriginExportKey>> = BTreeMap::new();
    let prepared_lineage_events = prepared_lineage_event_pairs(snapshot);
    for fact in snapshot.facts() {
        if let TraceFact::OriginEdge(edge) = fact
            && matches!(
                edge.traversal_class(),
                OriginEdgeTraversalClass::ExactAttribution
                    | OriginEdgeTraversalClass::SnapshotAlias
            )
            && origin_edge_satisfies_phase_contract(edge, &prepared_lineage_events)
        {
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
        CodeObjectFact, CodeObjectKind, CompilerEventFact, CompilerEventKind, CompilerPhase,
        ExecutionStepFact, ExecutionTraceSessionFact, FunctionFact, InstructionFact,
        OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, OriginNodeKind, RuntimeCaptureMode,
        RuntimeCodeObjectBindingFact, RuntimePcJoinConfidence, RuntimeTraceDataSource,
        RuntimeValuePolicy, StorageAccessFact, StorageAccessKind, TraceBundle, TraceFact,
        TraceMetadata, TraceSnapshot,
    };

    use super::{builtin_rulepack, emit_base_relations, origin_reaches, run_builtin_rulepack};

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

    fn runtime_snapshot() -> TraceSnapshot {
        let session = key("runtime.session", "tx:1", "session");
        let binding = key("runtime.binding", "tx:1", "runtime");
        let code_object = key("code.object", "demo", "runtime");
        let function = key("bytecode.function", "demo", "runtime");
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let step = key("runtime.step", "tx:1", "step:0");
        let write = key("runtime.storage", "tx:1", "write:0");
        TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "trace".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![
                node(session.clone()),
                node(binding.clone()),
                node(code_object.clone()),
                node(function.clone()),
                node(instruction.clone()),
                node(step.clone()),
                node(write.clone()),
                TraceFact::CodeObject(CodeObjectFact::new(
                    code_object.clone(),
                    CodeObjectKind::EvmRuntimeBytecode,
                    Some(function.clone()),
                    "evm/sonatina",
                    Some(
                        "blake3:000000000000000000000000000000000000000000000000000000000000beef"
                            .to_string(),
                    ),
                )),
                TraceFact::Function(FunctionFact::new(
                    function.clone(),
                    "runtime",
                    None,
                    Some(code_object.clone()),
                )),
                TraceFact::Instruction(InstructionFact::new(
                    instruction.clone(),
                    function,
                    0,
                    "SSTORE",
                )),
                TraceFact::ExecutionTraceSession(ExecutionTraceSessionFact {
                    session: session.clone(),
                    source: RuntimeTraceDataSource::RevmInspector,
                    capture_mode: RuntimeCaptureMode::Minimal,
                    value_policy: RuntimeValuePolicy::Redacted,
                    transaction_hash: None,
                    chain_id: None,
                    block_number: None,
                    entry_code_object: Some(code_object.clone()),
                }),
                TraceFact::RuntimeCodeObjectBinding(RuntimeCodeObjectBindingFact {
                    binding,
                    session: session.clone(),
                    code_object: code_object.clone(),
                    runtime_code_hash:
                        "blake3:000000000000000000000000000000000000000000000000000000000000beef"
                            .to_string(),
                    address: None,
                    confidence: RuntimePcJoinConfidence::ExactCodeHashAndPc,
                }),
                TraceFact::ExecutionStep(ExecutionStepFact {
                    step: step.clone(),
                    session,
                    step_index: 0,
                    code_object: code_object.clone(),
                    pc: 0,
                    opcode: "SSTORE".to_string(),
                    instruction: Some(instruction.clone()),
                    gas_before: 10,
                    gas_after: 7,
                    gas_cost: 3,
                    depth: 1,
                    join_confidence: RuntimePcJoinConfidence::ExactCodeHashAndPc,
                }),
                TraceFact::StorageAccess(StorageAccessFact {
                    access: write,
                    step,
                    code_object,
                    instruction: Some(instruction),
                    kind: StorageAccessKind::Write,
                    address: None,
                    slot: trace_facts::RuntimeValue::redacted(),
                    value_before: None,
                    value_after: None,
                    policy: RuntimeValuePolicy::Redacted,
                }),
            ],
        ))
        .unwrap()
    }

    #[test]
    fn base_relation_export_uses_typed_fact_schemas() {
        let snapshot = snapshot();
        let export = emit_base_relations(&snapshot);

        assert_content_digest(&export.trace_hash);
        assert!(
            export
                .schemas
                .iter()
                .any(|schema| schema.name == "base_origin_edge")
        );
        assert!(
            export
                .schemas
                .iter()
                .any(|schema| schema.name == "exact_attribution_edge")
        );
        assert!(
            export
                .rows
                .iter()
                .any(|row| row.relation == "base_instruction")
        );
        assert!(
            export
                .rows
                .iter()
                .any(|row| row.relation == "exact_attribution_edge")
        );
        assert!(export.rules.contains("origin_reaches"));
        assert!(!export.rules.contains("base_origin_edge"));
    }

    #[test]
    fn base_relation_export_includes_semantic_edge_views() {
        let pc = key("bytecode.pc", "demo", "pc:0");
        let code_object = key("code.object", "demo", "runtime");
        let hir = key("hir.expr", "demo", "expr:0");
        let synthetic = key("runtime.stmt", "demo", "stmt:synthetic");
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "trace".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![
                node(pc.clone()),
                node(code_object.clone()),
                node(hir.clone()),
                node(synthetic.clone()),
                TraceFact::OriginEdge(OriginEdgeFact::new(
                    pc.clone(),
                    code_object.clone(),
                    OriginEdgeLabel::EmittedFrom,
                    Some(CompilerPhase::BytecodeEmission),
                )),
                TraceFact::OriginEdge(OriginEdgeFact::new(
                    pc.clone(),
                    hir.clone(),
                    OriginEdgeLabel::LoweredFrom,
                    Some(CompilerPhase::BytecodeEmission),
                )),
                TraceFact::OriginEdge(OriginEdgeFact::new(
                    synthetic.clone(),
                    hir.clone(),
                    OriginEdgeLabel::SyntheticFor,
                    Some(CompilerPhase::Mir),
                )),
            ],
        ))
        .unwrap();
        let export = emit_base_relations(&snapshot);

        assert!(relation_row_exists(
            &export,
            "structural_edge",
            &[
                pc.canonical_storage_key(),
                code_object.canonical_storage_key()
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "contextual_edge",
            &[pc.canonical_storage_key(), hir.canonical_storage_key()]
        ));
        assert!(relation_row_exists(
            &export,
            "suspicious_edge",
            &[
                pc.canonical_storage_key(),
                hir.canonical_storage_key(),
                "exact_claim_classified_contextual".to_string(),
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "synthetic_edge",
            &[
                synthetic.canonical_storage_key(),
                hir.canonical_storage_key()
            ]
        ));
    }

    #[test]
    fn base_relation_export_marks_same_raw_local_id_as_candidate_only() {
        let raw_inst = "function:FuncRef(1):inst:InstId(7)";
        let postopt = key("sonatina.postopt.inst", "demo", raw_inst);
        let prepared = key("sonatina.evm.prepared.inst", "demo", raw_inst);
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "trace".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![node(postopt.clone()), node(prepared.clone())],
        ))
        .unwrap();
        let export = emit_base_relations(&snapshot);

        assert!(
            export
                .schemas
                .iter()
                .any(|schema| schema.name == "same_local_different_representation")
        );
        assert!(relation_row_exists(
            &export,
            "same_local_different_representation",
            &[
                prepared.canonical_storage_key(),
                postopt.canonical_storage_key(),
                raw_inst.to_string(),
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "candidate_lineage_hint",
            &[
                prepared.canonical_storage_key(),
                postopt.canonical_storage_key(),
                "same_raw_local_id".to_string(),
            ]
        ));
        assert!(
            !export
                .rows
                .iter()
                .any(|row| row.relation == "exact_attribution_edge")
        );
        assert!(!export.rules.contains("same_local_different_representation"));
        assert!(!export.rules.contains("candidate_lineage_hint"));
    }

    #[test]
    fn base_relation_export_includes_prepared_lineage_fact_view() {
        let raw_inst = "function:FuncRef(1):inst:InstId(7)";
        let postopt = key("sonatina.postopt.inst", "demo", raw_inst);
        let prepared = key("sonatina.evm.prepared.inst", "demo", raw_inst);
        let generated = key(
            "sonatina.evm.prepared.inst",
            "demo",
            "function:FuncRef(1):inst:InstId(8)",
        );
        let backend = key(
            "sonatina.evm.prepared.inst",
            "demo",
            "function:FuncRef(1):inst:InstId(9)",
        );
        let event = key("compiler.event", "demo", "prepared-lineage:0");
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "trace".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![
                node(postopt.clone()),
                node(prepared.clone()),
                node(generated.clone()),
                node(backend.clone()),
                node(event.clone()),
                TraceFact::CompilerEvent(CompilerEventFact::new(
                    event,
                    CompilerPhase::Backend,
                    CompilerEventKind::PreparedLineage,
                    vec![postopt.clone()],
                    vec![prepared.clone(), generated.clone(), backend.clone()],
                    None,
                )),
                TraceFact::OriginEdge(OriginEdgeFact::new(
                    prepared.clone(),
                    postopt.clone(),
                    OriginEdgeLabel::LoweredFrom,
                    Some(CompilerPhase::Backend),
                )),
                TraceFact::OriginEdge(OriginEdgeFact::new(
                    generated.clone(),
                    postopt.clone(),
                    OriginEdgeLabel::SyntheticFor,
                    Some(CompilerPhase::Backend),
                )),
                TraceFact::OriginEdge(OriginEdgeFact::new(
                    backend.clone(),
                    postopt.clone(),
                    OriginEdgeLabel::BackendPrepared,
                    Some(CompilerPhase::Backend),
                )),
            ],
        ))
        .unwrap();
        let export = emit_base_relations(&snapshot);

        assert!(
            export
                .schemas
                .iter()
                .any(|schema| schema.name == "prepared_lineage_fact")
        );
        assert!(relation_row_exists(
            &export,
            "prepared_lineage_fact",
            &[
                prepared.canonical_storage_key(),
                postopt.canonical_storage_key(),
                "exact_lowering".to_string(),
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "prepared_lineage_fact",
            &[
                generated.canonical_storage_key(),
                postopt.canonical_storage_key(),
                "synthetic_for".to_string(),
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "prepared_lineage_fact",
            &[
                backend.canonical_storage_key(),
                postopt.canonical_storage_key(),
                "backend_prepared_for".to_string(),
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "exact_attribution_edge",
            &[
                prepared.canonical_storage_key(),
                postopt.canonical_storage_key(),
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "synthetic_edge",
            &[
                generated.canonical_storage_key(),
                postopt.canonical_storage_key(),
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "backend_prepared_edge",
            &[
                backend.canonical_storage_key(),
                postopt.canonical_storage_key(),
            ]
        ));
    }

    #[test]
    fn semantic_edge_views_reject_prepared_postopt_without_lineage_event() {
        let postopt = key("sonatina.postopt.inst", "demo", "inst:0");
        let prepared = key("sonatina.evm.prepared.inst", "demo", "inst:0");
        let snapshot = TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "trace".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![
                node(postopt.clone()),
                node(prepared.clone()),
                TraceFact::OriginEdge(OriginEdgeFact::new(
                    prepared.clone(),
                    postopt.clone(),
                    OriginEdgeLabel::LoweredFrom,
                    Some(CompilerPhase::Backend),
                )),
            ],
        ))
        .unwrap();
        let export = emit_base_relations(&snapshot);

        assert!(!relation_row_exists(
            &export,
            "exact_attribution_edge",
            &[
                prepared.canonical_storage_key(),
                postopt.canonical_storage_key()
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "suspicious_edge",
            &[
                prepared.canonical_storage_key(),
                postopt.canonical_storage_key(),
                "raw_local_join_without_prepared_lineage".to_string(),
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "same_local_different_representation",
            &[
                prepared.canonical_storage_key(),
                postopt.canonical_storage_key(),
                "inst:0".to_string(),
            ]
        ));
        assert!(relation_row_exists(
            &export,
            "candidate_lineage_hint",
            &[
                prepared.canonical_storage_key(),
                postopt.canonical_storage_key(),
                "same_raw_local_id".to_string(),
            ]
        ));
        assert!(origin_reaches(&snapshot).is_empty());
    }

    #[test]
    fn base_relation_export_includes_runtime_relations() {
        let export = emit_base_relations(&runtime_snapshot());

        assert!(
            export
                .schemas
                .iter()
                .any(|schema| schema.name == "base_execution_step")
        );
        assert!(
            export
                .rows
                .iter()
                .any(|row| row.relation == "base_storage_access")
        );
    }

    #[test]
    fn builtin_rulepacks_run_read_only_runtime_queries() {
        let snapshot = runtime_snapshot();
        let report =
            run_builtin_rulepack(&snapshot, "runtime-v1", "storage-writes-by-source").unwrap();

        assert_eq!(report.status, "ok");
        assert_eq!(report.rulepack_id, "runtime-v1");
        assert_eq!(report.attribution_policy, "runtime-step-exclusive");
        assert_eq!(report.output["total_writes"], 1);
        assert!(
            builtin_rulepack("security-v1")
                .unwrap()
                .outputs
                .contains(&"runtime-storage-after-call")
        );
    }

    #[test]
    fn origin_reaches_derives_transitive_origin_paths() {
        let snapshot = snapshot();
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let hir = key("hir.expr", "demo", "expr:0");

        assert!(origin_reaches(&snapshot).contains(&(instruction, hir)));
    }

    fn assert_content_digest(value: &str) {
        let digest = value.strip_prefix("blake3:").unwrap_or(value);
        assert_eq!(digest.len(), 64);
        assert!(digest.chars().all(|ch| ch.is_ascii_hexdigit()));
        assert!(!value.starts_with("fnv64:"));
    }

    fn relation_row_exists(
        export: &super::DatalogBaseExport,
        relation: &str,
        values: &[String],
    ) -> bool {
        export
            .rows
            .iter()
            .any(|row| row.relation == relation && row.values == values)
    }
}
