use std::collections::BTreeSet;

use cranelift_entity::EntityRef;
use shape_address::{
    ShapeCyclePolicy, ShapeDimension, ShapeGraph, ShapeGraphKey, ShapeHashPolicy, ShapeNodeKey,
    ShapeViewMode, hash_shape_graph,
};
use trace_facts::{
    BlockFact, CfgEdgeFact, CfgEdgeKind, CompilerEventFact, CompilerEventKind, CompilerPhase,
    CompilerReason, DisplayNameFact, DisplayNameKind, FunctionFact, LoopBlockFact, LoopBlockRole,
    LoopConfidence, LoopDerivation, LoopFact, OriginNodeFact, OriginNodeKind, StorageFact,
    StorageLocation, StorageReason, TraceFact, TypeFact, TypeKind, ValueProperty,
    ValuePropertyFact, VariableFact, VariableStorageClass,
};

use crate::{
    MirDb, RuntimePackage,
    origin::{
        RUNTIME_BLOCK_EXPORT_KIND, RUNTIME_FUNCTION_EXPORT_KIND, RUNTIME_LOCAL_EXPORT_KIND,
        RUNTIME_LOOP_EXPORT_KIND, RUNTIME_STMT_EXPORT_KIND, RUNTIME_TERMINATOR_EXPORT_KIND,
        RUNTIME_TYPE_EXPORT_KIND, RuntimeBlockOrigin, RuntimeFunctionOrigin,
        RuntimeInstanceOwnerKey, RuntimeLocalOrigin, RuntimeLoopOrigin, RuntimeLoopSite,
        RuntimeStmtIndex, RuntimeStmtOrigin, RuntimeStmtSite, RuntimeTerminatorOrigin,
        RuntimeTerminatorSite,
    },
    runtime::{
        RBlockId, RLocalId, RStmt, RTerminator, RuntimeBody, RuntimeCarrier, RuntimeLocalLowering,
        RuntimeLocalRoot,
    },
};
use hir::{
    analysis::{semantic::borrowck::normalize_semantic_body, ty::ty_check::LocalBinding},
    hir_def::{Partial, Pat},
};

/// Emit MIR/runtime-owned trace facts for a runtime package.
///
/// MIR owns runtime statement and terminator identity. Backend storage slots,
/// registers, final instructions, and codegen events are emitted by codegen.
pub fn emit_mir_facts<'db>(db: &'db dyn MirDb, package: RuntimePackage<'db>) -> Vec<TraceFact> {
    let mut facts = Vec::new();
    for function in package.functions(db) {
        let instance = function.instance(db);
        let owner_key = RuntimeInstanceOwnerKey::for_instance(db, instance);
        let body = instance.body(db);
        let function_key = RuntimeFunctionOrigin::new(instance).export_key(&owner_key);
        facts.push(origin_node(
            function_key.clone(),
            RUNTIME_FUNCTION_EXPORT_KIND,
        ));
        facts.push(TraceFact::Function(FunctionFact::new(
            function_key.clone(),
            function.symbol(db),
            None,
            None,
        )));
        let semantic_local_info = semantic_local_trace_info(db, instance);
        for (local_index, local) in body.locals.iter().enumerate() {
            let local_key = RuntimeLocalOrigin::new(
                instance,
                crate::runtime::RLocalId::from_u32(local_index as u32),
            )
            .export_key(&owner_key);
            facts.push(origin_node(local_key.clone(), RUNTIME_LOCAL_EXPORT_KIND));
            let source_is_mut = semantic_local_info
                .get(local_index)
                .and_then(|info| info.as_ref())
                .is_some_and(|info| info.is_mut);
            if let Some(Some(info)) = semantic_local_info.get(local_index) {
                facts.push(TraceFact::DisplayName(DisplayNameFact::new(
                    local_key.clone(),
                    DisplayNameKind::SourceLocal,
                    info.name.clone(),
                )));
                let type_key = runtime_type_key(&owner_key, local_index);
                facts.push(origin_node(type_key.clone(), RUNTIME_TYPE_EXPORT_KIND));
                facts.push(TraceFact::Type(TypeFact::new(
                    type_key.clone(),
                    TypeKind::Unknown,
                    None,
                    None,
                    Vec::new(),
                )));
                facts.push(TraceFact::Variable(VariableFact::new(
                    local_key.clone(),
                    info.name.clone(),
                    type_key,
                    local_key.clone(),
                    None,
                    info.storage_class,
                )));
                if info.is_mut {
                    facts.push(TraceFact::ValueProperty(ValuePropertyFact::new(
                        local_key.clone(),
                        CompilerPhase::Mir,
                        ValueProperty::SourceMutable,
                        Some(CompilerReason::new("source binding is mutable")),
                    )));
                }
            }
            let storage_location = mir_storage_location(local);
            let storage_reason = mir_storage_reason(
                local_index,
                &body.semantic_locals,
                &storage_location,
                source_is_mut,
            );
            facts.push(TraceFact::Storage(StorageFact::new(
                local_key.clone(),
                CompilerPhase::Mir,
                storage_location.clone(),
                storage_reason,
            )));
            facts.push(TraceFact::ValueProperty(ValuePropertyFact::new(
                local_key.clone(),
                CompilerPhase::Mir,
                match storage_location {
                    StorageLocation::MemoryPlace => ValueProperty::MemoryBacked,
                    StorageLocation::SsaValue => ValueProperty::SsaValue,
                    _ => continue,
                },
                Some(CompilerReason::new("MIR storage classification")),
            )));
            let event_key = compiler_event_key(
                &owner_key,
                format!("mir:local:{local_index}:storage_classification"),
            );
            facts.push(origin_node(event_key.clone(), "compiler.event"));
            facts.push(TraceFact::CompilerEvent(CompilerEventFact::new(
                event_key,
                CompilerPhase::Mir,
                CompilerEventKind::Lowering,
                Vec::new(),
                vec![local_key],
                Some(CompilerReason::new(mir_storage_event_reason(
                    &storage_location,
                    storage_reason,
                ))),
            )));
        }
        let cfg = runtime_cfg(&body);
        let dominators = dominators(body.blocks.len(), &cfg.predecessors);
        for (block_index, runtime_block) in body.blocks.iter().enumerate() {
            let block = RBlockId::from_u32(block_index as u32);
            let block_key = RuntimeBlockOrigin::new(instance, block).export_key(&owner_key);
            facts.push(origin_node(block_key.clone(), RUNTIME_BLOCK_EXPORT_KIND));
            facts.push(TraceFact::Block(BlockFact::new(
                block_key,
                function_key.clone(),
                CompilerPhase::Mir,
                block_index as u32,
                Some(format!("bb{block_index}")),
            )));
            for (stmt_index, _) in runtime_block.stmts.iter().enumerate() {
                let site =
                    RuntimeStmtSite::new(block, RuntimeStmtIndex::from_u32(stmt_index as u32));
                facts.push(origin_node(
                    RuntimeStmtOrigin::new(instance, site).export_key(&owner_key),
                    RUNTIME_STMT_EXPORT_KIND,
                ));
            }
            let terminator_site = RuntimeTerminatorSite::new(block);
            facts.push(origin_node(
                RuntimeTerminatorOrigin::new(instance, terminator_site).export_key(&owner_key),
                RUNTIME_TERMINATOR_EXPORT_KIND,
            ));
        }
        for edge in &cfg.edges {
            let from_key = RuntimeBlockOrigin::new(instance, edge.from).export_key(&owner_key);
            let to_key = RuntimeBlockOrigin::new(instance, edge.to).export_key(&owner_key);
            let condition_origin = edge
                .condition
                .map(|local| RuntimeLocalOrigin::new(instance, local).export_key(&owner_key));
            facts.push(TraceFact::CfgEdge(CfgEdgeFact::new(
                function_key.clone(),
                from_key,
                to_key,
                cfg_edge_kind(edge, &dominators),
                condition_origin,
            )));
        }
        let cfg_hash = runtime_cfg_hash(body.blocks.len(), &cfg.edges);
        for natural_loop in natural_loops(body.blocks.len(), &cfg.predecessors, &cfg.edges) {
            let loop_key = RuntimeLoopOrigin::new(
                instance,
                RuntimeLoopSite::new(natural_loop.header, natural_loop.latch),
            )
            .export_key(&owner_key);
            let header_key =
                RuntimeBlockOrigin::new(instance, natural_loop.header).export_key(&owner_key);
            facts.push(origin_node(loop_key.clone(), RUNTIME_LOOP_EXPORT_KIND));
            facts.push(TraceFact::Loop(LoopFact::new(
                loop_key.clone(),
                function_key.clone(),
                CompilerPhase::Mir,
                header_key.clone(),
                LoopDerivation::NaturalLoopAnalysis {
                    cfg_hash: cfg_hash.clone(),
                },
                LoopConfidence::MirCfg,
            )));
            facts.push(TraceFact::LoopBlock(LoopBlockFact::new(
                loop_key.clone(),
                header_key,
                LoopBlockRole::Header,
            )));
            for block in &natural_loop.members {
                if *block == natural_loop.header {
                    continue;
                }
                let role = if *block == natural_loop.latch {
                    LoopBlockRole::Latch
                } else {
                    LoopBlockRole::Body
                };
                facts.push(TraceFact::LoopBlock(LoopBlockFact::new(
                    loop_key.clone(),
                    RuntimeBlockOrigin::new(instance, *block).export_key(&owner_key),
                    role,
                )));
            }
            push_runtime_loop_shape_facts(
                &mut facts,
                &loop_key,
                instance,
                &owner_key,
                &body,
                &natural_loop.members,
            );
        }
    }
    facts
}

fn push_runtime_loop_shape_facts<'db>(
    facts: &mut Vec<TraceFact>,
    loop_key: &common::origin::OriginExportKey,
    instance: crate::RuntimeInstance<'db>,
    owner_key: &RuntimeInstanceOwnerKey,
    body: &RuntimeBody<'db>,
    blocks: &[RBlockId],
) {
    let Ok(graph) = runtime_loop_shape_graph(loop_key, instance, owner_key, body, blocks) else {
        return;
    };
    let Ok(policy) = loop_shape_policy("mir.loop") else {
        return;
    };
    let Ok(hashes) = hash_shape_graph(&policy, &graph) else {
        return;
    };
    facts.extend(trace_facts::shape_hash_facts(&graph, &policy, &hashes));
}

fn runtime_loop_shape_graph<'db>(
    loop_key: &common::origin::OriginExportKey,
    instance: crate::RuntimeInstance<'db>,
    owner_key: &RuntimeInstanceOwnerKey,
    body: &RuntimeBody<'db>,
    blocks: &[RBlockId],
) -> Result<ShapeGraph, shape_address::ShapeError> {
    let loop_node = ShapeNodeKey::entity(loop_key.clone());
    let mut graph = ShapeGraph::new(ShapeGraphKey::new(loop_key.clone(), "mir-loop-shape")?);
    graph.add_node(loop_node.clone(), RUNTIME_LOOP_EXPORT_KIND)?;
    graph.add_field(&loop_node, ShapeDimension::Structure, "phase", "mir")?;
    for (block_ordinal, block_id) in blocks.iter().enumerate() {
        let Some(block) = body.blocks.get(block_id.index()) else {
            continue;
        };
        let block_key = RuntimeBlockOrigin::new(instance, *block_id).export_key(owner_key);
        let block_node = ShapeNodeKey::entity(block_key.clone());
        graph.add_node(block_node.clone(), RUNTIME_BLOCK_EXPORT_KIND)?;
        graph.add_child(&loop_node, "block", block_ordinal as u32, &block_node)?;
        for (stmt_index, stmt) in block.stmts.iter().enumerate() {
            let stmt_key = RuntimeStmtOrigin::new(
                instance,
                RuntimeStmtSite::new(*block_id, RuntimeStmtIndex::from_u32(stmt_index as u32)),
            )
            .export_key(owner_key);
            let stmt_node = ShapeNodeKey::entity(stmt_key);
            graph.add_node(stmt_node.clone(), RUNTIME_STMT_EXPORT_KIND)?;
            graph.add_field(
                &stmt_node,
                ShapeDimension::Structure,
                "kind",
                runtime_stmt_kind(stmt),
            )?;
            graph.add_child(&block_node, "stmt", stmt_index as u32, &stmt_node)?;
        }
        let term_key =
            RuntimeTerminatorOrigin::new(instance, RuntimeTerminatorSite::new(*block_id))
                .export_key(owner_key);
        let term_node = ShapeNodeKey::entity(term_key);
        graph.add_node(term_node.clone(), RUNTIME_TERMINATOR_EXPORT_KIND)?;
        graph.add_field(
            &term_node,
            ShapeDimension::Structure,
            "kind",
            runtime_terminator_kind(&block.terminator),
        )?;
        graph.add_child(&block_node, "terminator", 0, &term_node)?;
    }
    Ok(graph)
}

fn loop_shape_policy(level: &str) -> Result<ShapeHashPolicy, shape_address::ShapeError> {
    ShapeHashPolicy::with_dimensions(
        level,
        [ShapeDimension::Structure, ShapeDimension::Constants],
        ShapeViewMode::AnonymousShape,
        ShapeCyclePolicy::Reject,
    )
}

fn runtime_stmt_kind(stmt: &RStmt<'_>) -> &'static str {
    match stmt {
        RStmt::Assign { .. } => "assign",
        RStmt::EnumAssertVariant { .. } => "enum_assert_variant",
        RStmt::Store { .. } => "store",
        RStmt::CopyInto { .. } => "copy_into",
        RStmt::EnumSetTag { .. } => "enum_set_tag",
        RStmt::EnumWriteVariant { .. } => "enum_write_variant",
    }
}

fn runtime_terminator_kind(terminator: &RTerminator<'_>) -> &'static str {
    match terminator {
        RTerminator::Goto(_) => "goto",
        RTerminator::Branch { .. } => "branch",
        RTerminator::SwitchScalar { .. } => "switch_scalar",
        RTerminator::MatchEnumTag { .. } => "match_enum_tag",
        RTerminator::TerminalCall { .. } => "terminal_call",
        RTerminator::ReturnData { .. } => "return_data",
        RTerminator::Revert { .. } => "revert",
        RTerminator::SelfDestruct { .. } => "self_destruct",
        RTerminator::Trap => "trap",
        RTerminator::Return(_) => "return",
        RTerminator::Stop => "stop",
    }
}

#[derive(Clone, Debug)]
struct RuntimeCfg {
    edges: Vec<RuntimeCfgEdge>,
    predecessors: Vec<Vec<RBlockId>>,
}

#[derive(Clone, Copy, Debug)]
struct RuntimeCfgEdge {
    from: RBlockId,
    to: RBlockId,
    kind: CfgEdgeKind,
    condition: Option<RLocalId>,
}

#[derive(Clone, Debug)]
struct NaturalLoop {
    header: RBlockId,
    latch: RBlockId,
    members: Vec<RBlockId>,
}

fn runtime_cfg(body: &RuntimeBody<'_>) -> RuntimeCfg {
    let mut edges = Vec::new();
    let mut predecessors = vec![Vec::new(); body.blocks.len()];
    for (block_index, block) in body.blocks.iter().enumerate() {
        let from = RBlockId::from_u32(block_index as u32);
        for edge in terminator_edges(from, &block.terminator) {
            if let Some(preds) = predecessors.get_mut(edge.to.index()) {
                preds.push(edge.from);
            }
            edges.push(edge);
        }
    }
    RuntimeCfg {
        edges,
        predecessors,
    }
}

fn terminator_edges(from: RBlockId, terminator: &RTerminator<'_>) -> Vec<RuntimeCfgEdge> {
    match terminator {
        RTerminator::Goto(to) => vec![RuntimeCfgEdge {
            from,
            to: *to,
            kind: CfgEdgeKind::Jump,
            condition: None,
        }],
        RTerminator::Branch {
            cond,
            then_bb,
            else_bb,
        } => vec![
            RuntimeCfgEdge {
                from,
                to: *then_bb,
                kind: CfgEdgeKind::BranchTrue,
                condition: Some(*cond),
            },
            RuntimeCfgEdge {
                from,
                to: *else_bb,
                kind: CfgEdgeKind::BranchFalse,
                condition: Some(*cond),
            },
        ],
        RTerminator::SwitchScalar {
            discr,
            cases,
            default,
        } => {
            let mut edges = cases
                .iter()
                .map(|(_, to)| RuntimeCfgEdge {
                    from,
                    to: *to,
                    kind: CfgEdgeKind::BranchTrue,
                    condition: Some(*discr),
                })
                .collect::<Vec<_>>();
            edges.push(RuntimeCfgEdge {
                from,
                to: *default,
                kind: CfgEdgeKind::BranchFalse,
                condition: Some(*discr),
            });
            edges
        }
        RTerminator::MatchEnumTag {
            tag,
            cases,
            default,
            ..
        } => {
            let mut edges = cases
                .iter()
                .map(|(_, to)| RuntimeCfgEdge {
                    from,
                    to: *to,
                    kind: CfgEdgeKind::BranchTrue,
                    condition: Some(*tag),
                })
                .collect::<Vec<_>>();
            if let Some(default) = default {
                edges.push(RuntimeCfgEdge {
                    from,
                    to: *default,
                    kind: CfgEdgeKind::BranchFalse,
                    condition: Some(*tag),
                });
            }
            edges
        }
        RTerminator::TerminalCall { .. }
        | RTerminator::ReturnData { .. }
        | RTerminator::Revert { .. }
        | RTerminator::SelfDestruct { .. }
        | RTerminator::Trap
        | RTerminator::Return(_)
        | RTerminator::Stop => Vec::new(),
    }
}

fn cfg_edge_kind(edge: &RuntimeCfgEdge, dominators: &[BTreeSet<usize>]) -> CfgEdgeKind {
    let from = edge.from.index();
    let to = edge.to.index();
    if dominators
        .get(from)
        .is_some_and(|dominator_set| dominator_set.contains(&to))
    {
        CfgEdgeKind::Backedge
    } else {
        edge.kind
    }
}

fn natural_loops(
    block_count: usize,
    predecessors: &[Vec<RBlockId>],
    edges: &[RuntimeCfgEdge],
) -> Vec<NaturalLoop> {
    let dominators = dominators(block_count, predecessors);
    let mut seen = BTreeSet::new();
    let mut loops = Vec::new();
    for edge in edges {
        let from = edge.from.index();
        let to = edge.to.index();
        if from >= block_count || to >= block_count {
            continue;
        }
        if !dominators[from].contains(&to) || !seen.insert((to, from)) {
            continue;
        }
        loops.push(NaturalLoop {
            header: edge.to,
            latch: edge.from,
            members: natural_loop_members(block_count, predecessors, edge.to, edge.from),
        });
    }
    loops
}

fn natural_loop_members(
    block_count: usize,
    predecessors: &[Vec<RBlockId>],
    header: RBlockId,
    latch: RBlockId,
) -> Vec<RBlockId> {
    let header_index = header.index();
    let latch_index = latch.index();
    let mut members = BTreeSet::from([header_index, latch_index]);
    let mut stack = vec![latch_index];
    while let Some(block) = stack.pop() {
        for predecessor in predecessors.get(block).into_iter().flatten() {
            let predecessor = predecessor.index();
            if predecessor >= block_count || !members.insert(predecessor) {
                continue;
            }
            if predecessor != header_index {
                stack.push(predecessor);
            }
        }
    }
    members
        .into_iter()
        .map(|block| RBlockId::from_u32(block as u32))
        .collect()
}

fn dominators(block_count: usize, predecessors: &[Vec<RBlockId>]) -> Vec<BTreeSet<usize>> {
    if block_count == 0 {
        return Vec::new();
    }
    let all_blocks = (0..block_count).collect::<BTreeSet<_>>();
    let mut dominators = vec![all_blocks.clone(); block_count];
    dominators[0] = BTreeSet::from([0]);

    let mut changed = true;
    while changed {
        changed = false;
        for block in 1..block_count {
            let preds = predecessors
                .get(block)
                .into_iter()
                .flatten()
                .map(|pred| pred.index())
                .filter(|pred| *pred < block_count)
                .collect::<Vec<_>>();
            let mut next = if let Some((first, rest)) = preds.split_first() {
                let mut intersection = dominators[*first].clone();
                for pred in rest {
                    intersection = intersection
                        .intersection(&dominators[*pred])
                        .copied()
                        .collect();
                }
                intersection
            } else {
                BTreeSet::new()
            };
            next.insert(block);
            if next != dominators[block] {
                dominators[block] = next;
                changed = true;
            }
        }
    }
    dominators
}

fn runtime_cfg_hash(block_count: usize, edges: &[RuntimeCfgEdge]) -> String {
    let mut hasher = blake3::Hasher::new();
    hash_u32(&mut hasher, block_count as u32);
    for edge in edges {
        hash_u32(&mut hasher, edge.from.index() as u32);
        hash_u32(&mut hasher, edge.to.index() as u32);
        hash_bytes(&mut hasher, cfg_edge_kind_name(edge.kind).as_bytes());
        match edge.condition {
            Some(condition) => hash_u32(&mut hasher, condition.index() as u32),
            None => hash_bytes(&mut hasher, b"none"),
        }
    }
    format!("blake3:{}", hasher.finalize().to_hex())
}

fn cfg_edge_kind_name(kind: CfgEdgeKind) -> &'static str {
    match kind {
        CfgEdgeKind::Fallthrough => "fallthrough",
        CfgEdgeKind::BranchTrue => "branch_true",
        CfgEdgeKind::BranchFalse => "branch_false",
        CfgEdgeKind::Jump => "jump",
        CfgEdgeKind::Backedge => "backedge",
        CfgEdgeKind::Return => "return",
        CfgEdgeKind::Unwind => "unwind",
        CfgEdgeKind::Unknown => "unknown",
    }
}

fn hash_u32(hasher: &mut blake3::Hasher, value: u32) {
    hasher.update(&value.to_le_bytes());
}

fn hash_bytes(hasher: &mut blake3::Hasher, bytes: &[u8]) {
    hash_u32(hasher, bytes.len() as u32);
    hasher.update(bytes);
}

fn mir_storage_location(local: &crate::runtime::RLocal<'_>) -> StorageLocation {
    match (&local.carrier, &local.root) {
        (
            _,
            RuntimeLocalRoot::Slot(_) | RuntimeLocalRoot::Ref(_) | RuntimeLocalRoot::Ptr { .. },
        ) => StorageLocation::MemoryPlace,
        (RuntimeCarrier::Value(_), RuntimeLocalRoot::None) => StorageLocation::SsaValue,
        _ => StorageLocation::Unknown,
    }
}

fn mir_storage_reason(
    local_index: usize,
    semantic_locals: &[RuntimeLocalLowering<'_>],
    location: &StorageLocation,
    source_is_mut: bool,
) -> StorageReason {
    if matches!(location, StorageLocation::MemoryPlace) && source_is_mut {
        return StorageReason::MutableLocalLowering;
    }
    match semantic_locals.get(local_index) {
        Some(
            RuntimeLocalLowering::PlaceCarrier { .. }
            | RuntimeLocalLowering::PlaceBoundValue { .. },
        ) => StorageReason::MutableLocalLowering,
        _ => StorageReason::Unknown,
    }
}

fn mir_storage_event_reason(location: &StorageLocation, reason: StorageReason) -> &'static str {
    match (location, reason) {
        (StorageLocation::MemoryPlace, StorageReason::MutableLocalLowering) => {
            "semantic local lowered to MIR memory place"
        }
        (StorageLocation::SsaValue, _) => "semantic local kept as MIR SSA value",
        _ => "runtime local storage classified by MIR lowering",
    }
}

#[derive(Clone, Debug)]
struct SemanticLocalTraceInfo {
    name: String,
    is_mut: bool,
    storage_class: VariableStorageClass,
}

fn semantic_local_trace_info<'db>(
    db: &'db dyn MirDb,
    instance: crate::RuntimeInstance<'db>,
) -> Vec<Option<SemanticLocalTraceInfo>> {
    let Some(semantic) = instance.key(db).semantic(db) else {
        return Vec::new();
    };
    let typed_body = semantic.key(db).typed_body(db);
    let Some(body) = typed_body.body() else {
        return Vec::new();
    };
    let Ok(normalized) = normalize_semantic_body(db, semantic) else {
        return Vec::new();
    };
    normalized
        .locals
        .iter()
        .map(|local| {
            local
                .source
                .map(|binding| local_binding_trace_info(db, body, binding))
        })
        .collect()
}

fn local_binding_trace_info<'db>(
    db: &'db dyn MirDb,
    body: hir::hir_def::Body<'db>,
    binding: LocalBinding<'db>,
) -> SemanticLocalTraceInfo {
    SemanticLocalTraceInfo {
        name: local_binding_name(db, body, binding),
        is_mut: binding.is_mut(),
        storage_class: match binding {
            LocalBinding::Local { .. } => VariableStorageClass::Local,
            LocalBinding::Param { .. } | LocalBinding::EffectParam { .. } => {
                VariableStorageClass::Parameter
            }
        },
    }
}

fn local_binding_name<'db>(
    db: &'db dyn MirDb,
    body: hir::hir_def::Body<'db>,
    binding: LocalBinding<'db>,
) -> String {
    match binding {
        LocalBinding::Local { pat, .. } => {
            let Partial::Present(Pat::Path(Partial::Present(path), ..)) = pat.data(db, body) else {
                return "_".to_string();
            };
            path.ident(db)
                .to_opt()
                .map(|ident| ident.data(db).to_string())
                .unwrap_or_else(|| "_".to_string())
        }
        LocalBinding::Param { idx, .. } => format!("%param{idx}"),
        LocalBinding::EffectParam {
            binding_name, idx, ..
        } => binding_name
            .data(db)
            .is_empty()
            .then(|| format!("%effect{idx}"))
            .unwrap_or_else(|| binding_name.data(db).to_string()),
    }
}

fn compiler_event_key(
    owner_key: &RuntimeInstanceOwnerKey,
    local_key: impl AsRef<str>,
) -> common::origin::OriginExportKey {
    common::origin::OriginExportKey::try_from_raw_parts(
        "compiler.event",
        owner_key.as_str(),
        local_key.as_ref(),
    )
    .expect("MIR compiler event key must be valid")
}

fn runtime_type_key(
    owner_key: &RuntimeInstanceOwnerKey,
    local_index: usize,
) -> common::origin::OriginExportKey {
    common::origin::OriginExportKey::try_from_raw_parts(
        RUNTIME_TYPE_EXPORT_KIND,
        owner_key.as_str(),
        format!("local:{local_index}:type"),
    )
    .expect("MIR runtime type key must be valid")
}

fn origin_node(key: common::origin::OriginExportKey, kind: &str) -> TraceFact {
    TraceFact::OriginNode(OriginNodeFact::new(key, OriginNodeKind::new(kind)))
}
