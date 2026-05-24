use std::collections::BTreeSet;

use common::origin::OriginExportKey;
use shape_address::{
    ShapeCyclePolicy, ShapeDimension, ShapeGraph, ShapeGraphKey, ShapeHashPolicy, ShapeNodeKey,
    ShapeViewMode, hash_shape_graph,
};
use sonatina_ir::{
    CfgEdgeKind as SonatinaCfgEdgeKind, FrontendOriginKind, FrontendOriginRecord, SonatinaTraceView,
};
use trace_facts::{
    BlockFact, CategorySource, CfgEdgeFact, CfgEdgeKind, CodeObjectFact, CodeObjectKind,
    CompilerPhase, EvmSchedule, FunctionFact, GasConfidence, GasCostFact, GasKind, GasSource,
    InstructionBlockFact, InstructionCategory, InstructionCategoryFact, InstructionExtentFact,
    InstructionFact, LoopBlockFact, LoopBlockRole, LoopConfidence, LoopDerivation, LoopFact,
    OpcodeCategory, OpcodeFact, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, OriginNodeKind,
    PcRange, StaticGasFact, TraceFact,
};

use crate::debug::BytecodeSourceMapEntry;

pub const SONATINA_PREOPT_FUNCTION_KIND: &str = "sonatina.preopt.function";
pub const SONATINA_PREOPT_BLOCK_KIND: &str = "sonatina.preopt.block";
pub const SONATINA_PREOPT_INST_KIND: &str = "sonatina.preopt.inst";
pub const SONATINA_PREOPT_LOOP_KIND: &str = "sonatina.preopt.loop";
pub const SONATINA_POSTOPT_FUNCTION_KIND: &str = "sonatina.postopt.function";
pub const SONATINA_POSTOPT_BLOCK_KIND: &str = "sonatina.postopt.block";
pub const SONATINA_POSTOPT_INST_KIND: &str = "sonatina.postopt.inst";
pub const SONATINA_POSTOPT_LOOP_KIND: &str = "sonatina.postopt.loop";

/// Emit codegen-owned trace facts for bytecode/source-map records.
///
/// Codegen owns bytecode PC identity. It does not create HIR or MIR origin
/// identity; edges to those origins are emitted only when codegen has that
/// phase-owned mapping.
pub fn emit_codegen_facts<'a>(
    entries: impl IntoIterator<Item = &'a BytecodeSourceMapEntry>,
) -> Vec<TraceFact> {
    entries
        .into_iter()
        .map(|entry| {
            TraceFact::OriginNode(OriginNodeFact::new(
                entry.origin.clone(),
                OriginNodeKind::new(entry.origin.kind()),
            ))
        })
        .collect()
}

/// Emit codegen-owned instruction facts from actual emitted EVM bytecode.
pub fn emit_bytecode_instruction_facts(
    owner_key: &str,
    function_local_key: &str,
    bytecode: &[u8],
) -> Vec<TraceFact> {
    let function = bytecode_function_key(owner_key, function_local_key);
    let code_object = bytecode_code_object_key(owner_key);
    let mut facts = vec![
        origin_node(function.clone(), "bytecode.function"),
        origin_node(code_object.clone(), "code.object"),
        TraceFact::Function(trace_facts::FunctionFact::new(
            function.clone(),
            function_local_key,
            None,
            Some(code_object.clone()),
        )),
        TraceFact::CodeObject(CodeObjectFact::new(
            code_object.clone(),
            CodeObjectKind::EvmRuntimeBytecode,
            Some(function.clone()),
            "evm/sonatina",
            Some(bytecode_content_hash(bytecode)),
        )),
    ];
    let mut pc = 0;
    let mut index = 0;
    while pc < bytecode.len() {
        let opcode = bytecode[pc];
        let instruction =
            OriginExportKey::try_from_raw_parts("bytecode.pc", owner_key, format!("pc:{pc}"))
                .expect("codegen bytecode PC key must be valid");
        let mnemonic = evm_mnemonic(opcode).to_string();
        let immediate_len = evm_push_immediate_len(opcode);
        let immediate = (immediate_len > 0).then(|| {
            let end = (pc + 1 + immediate_len).min(bytecode.len());
            format!("0x{}", hex::encode(&bytecode[pc + 1..end]))
        });
        let byte_len = 1 + immediate_len.min(bytecode.len().saturating_sub(pc + 1));
        facts.push(origin_node(instruction.clone(), "bytecode.pc"));
        facts.push(TraceFact::Instruction(InstructionFact::new(
            instruction.clone(),
            function.clone(),
            index,
            mnemonic.clone(),
        )));
        facts.push(TraceFact::InstructionExtent(InstructionExtentFact::new(
            instruction.clone(),
            code_object.clone(),
            PcRange::new(pc as u32, (pc + byte_len) as u32),
            byte_len as u32,
        )));
        facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
            instruction.clone(),
            code_object.clone(),
            OriginEdgeLabel::EmittedFrom,
            Some(CompilerPhase::BytecodeEmission),
        )));
        facts.push(TraceFact::InstructionCategory(
            InstructionCategoryFact::new(
                instruction.clone(),
                evm_instruction_category(opcode),
                CategorySource::BackendEmissionReason,
            ),
        ));
        facts.push(TraceFact::Opcode(OpcodeFact::new(
            instruction.clone(),
            mnemonic,
            immediate,
            evm_opcode_category(opcode),
        )));
        facts.push(TraceFact::GasCost(GasCostFact::new(
            instruction.clone(),
            GasKind::OpcodeStatic,
            evm_static_gas(opcode),
            EvmSchedule::new("cancun"),
            GasConfidence::ConservativeStatic,
            GasSource::OpcodeTable,
        )));
        facts.push(TraceFact::StaticGas(StaticGasFact::new(
            instruction,
            EvmSchedule::new("cancun"),
            evm_static_gas(opcode),
            None,
        )));
        pc += byte_len;
        index += 1;
    }
    facts
}

pub fn emit_bytecode_shape_facts(
    owner_key: &str,
    function_local_key: &str,
    bytecode: &[u8],
) -> Vec<TraceFact> {
    let Ok(graph) = crate::shape::describe_bytecode_shape(owner_key, function_local_key, bytecode)
    else {
        return Vec::new();
    };
    let Ok(policy) = loop_shape_policy("bytecode.code-object") else {
        return Vec::new();
    };
    let Ok(hashes) = hash_shape_graph(&policy, &graph) else {
        return Vec::new();
    };
    trace_facts::shape_hash_facts(&graph, &policy, &hashes)
}

pub fn frontend_origin_record_for_export_key(
    key: &OriginExportKey,
    kind: FrontendOriginKind,
) -> FrontendOriginRecord {
    FrontendOriginRecord {
        external_key: Some(
            serde_json::to_string(key).expect("OriginExportKey serialization cannot fail"),
        ),
        source_span: None,
        display_label: Some(key.display_label()),
        kind,
    }
}

pub fn emit_sonatina_trace_view_facts(
    owner_key: &str,
    module: &sonatina_ir::Module,
    phase: CompilerPhase,
) -> Vec<TraceFact> {
    let Some((function_kind, block_kind, inst_kind)) = sonatina_phase_kinds(phase) else {
        return Vec::new();
    };
    let mut facts = Vec::new();
    for function_ref in module.trace_functions() {
        let function_key = sonatina_function_key(function_kind, owner_key, function_ref);
        push_node(&mut facts, function_key.clone());
        let function_name = module
            .ctx
            .func_sig(function_ref, |sig| sig.name().to_string());
        facts.push(TraceFact::Function(FunctionFact::new(
            function_key.clone(),
            function_name,
            None,
            None,
        )));

        let blocks = module.trace_blocks(function_ref);
        for (block_ordinal, block) in blocks.iter().copied().enumerate() {
            let block_key = sonatina_trace_block_key(block_kind, owner_key, function_ref, block);
            push_node(&mut facts, block_key.clone());
            facts.push(TraceFact::Block(BlockFact::new(
                block_key,
                function_key.clone(),
                phase,
                block_ordinal as u32,
                Some(format!("{block:?}")),
            )));
        }

        let mut instruction_index = 0u32;
        for block in blocks {
            let block_key = sonatina_trace_block_key(block_kind, owner_key, function_ref, block);
            for edge in module.trace_block_successors(function_ref, block) {
                let to_block =
                    sonatina_trace_block_key(block_kind, owner_key, function_ref, edge.to);
                facts.push(TraceFact::CfgEdge(CfgEdgeFact::new(
                    function_key.clone(),
                    block_key.clone(),
                    to_block,
                    sonatina_trace_edge_kind(edge.kind, edge.ordinal),
                    None,
                )));
            }

            for inst in module.trace_instructions(function_ref, block) {
                let inst_key = sonatina_trace_inst_key(inst_kind, owner_key, function_ref, inst);
                push_node(&mut facts, inst_key.clone());
                let mnemonic = module
                    .trace_inst_kind(function_ref, inst)
                    .map(|kind| kind.opcode.to_string())
                    .unwrap_or_else(|| "unknown".to_string());
                facts.push(TraceFact::Instruction(InstructionFact::new(
                    inst_key.clone(),
                    function_key.clone(),
                    instruction_index,
                    mnemonic,
                )));
                facts.push(TraceFact::InstructionBlock(InstructionBlockFact::new(
                    inst_key.clone(),
                    block_key.clone(),
                    phase,
                )));
                instruction_index += 1;

                if let Some(frontend_origin) =
                    sonatina_frontend_origin_for_inst(module, function_ref, inst)
                {
                    facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                        inst_key,
                        frontend_origin,
                        OriginEdgeLabel::LoweredFrom,
                        Some(phase),
                    )));
                }
            }
        }
    }
    facts
}

/// Emit Sonatina-owned CFG and lowering bridge facts from the runtime package.
///
/// This is intentionally a lowering-unit trace: it identifies which MIR
/// statement or terminator each Sonatina pre-opt unit came from. Exact
/// pass-level post-opt instruction preservation still needs Sonatina pass hooks,
/// so post-opt facts are marked as snapshot-preserved projections instead of
/// pretending to know optimizer rewrites.
pub fn emit_sonatina_cfg_facts<'db>(
    db: &'db dyn mir::MirDb,
    package: mir::RuntimePackage<'db>,
) -> Vec<TraceFact> {
    let mut facts = Vec::new();
    for function in package.functions(db) {
        let instance = function.instance(db);
        let body = instance.body(db);
        let owner = mir::origin::RuntimeInstanceOwnerKey::for_instance(db, instance);
        let pre_function = sonatina_key(SONATINA_PREOPT_FUNCTION_KIND, &owner, "function");
        let post_function = sonatina_key(SONATINA_POSTOPT_FUNCTION_KIND, &owner, "function");
        push_node(&mut facts, pre_function.clone());
        push_node(&mut facts, post_function.clone());
        facts.push(TraceFact::Function(FunctionFact::new(
            pre_function.clone(),
            function.symbol(db),
            None,
            None,
        )));
        facts.push(TraceFact::Function(FunctionFact::new(
            post_function.clone(),
            function.symbol(db),
            None,
            None,
        )));
        facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
            post_function.clone(),
            pre_function.clone(),
            OriginEdgeLabel::PreservedSnapshotIdentity,
            Some(CompilerPhase::SonatinaPostOpt),
        )));

        let cfg = runtime_cfg(&body);
        let dominators = dominators(body.blocks.len(), &cfg.predecessors);
        for (block_index, _) in body.blocks.iter().enumerate() {
            let runtime_block = mir::RBlockId::from_u32(block_index as u32);
            let runtime_block_key =
                mir::RuntimeBlockOrigin::new(instance, runtime_block).export_key(&owner);
            let pre_block = sonatina_block_key(SONATINA_PREOPT_BLOCK_KIND, &owner, block_index);
            let post_block = sonatina_block_key(SONATINA_POSTOPT_BLOCK_KIND, &owner, block_index);
            push_node(&mut facts, pre_block.clone());
            push_node(&mut facts, post_block.clone());
            facts.push(TraceFact::Block(BlockFact::new(
                pre_block.clone(),
                pre_function.clone(),
                CompilerPhase::SonatinaPreOpt,
                block_index as u32,
                Some(format!("bb{block_index}")),
            )));
            facts.push(TraceFact::Block(BlockFact::new(
                post_block.clone(),
                post_function.clone(),
                CompilerPhase::SonatinaPostOpt,
                block_index as u32,
                Some(format!("bb{block_index}")),
            )));
            facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                pre_block,
                runtime_block_key,
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::SonatinaPreOpt),
            )));
            facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                post_block,
                sonatina_block_key(SONATINA_PREOPT_BLOCK_KIND, &owner, block_index),
                OriginEdgeLabel::PreservedSnapshotIdentity,
                Some(CompilerPhase::SonatinaPostOpt),
            )));
        }

        for edge in &cfg.edges {
            let from = edge.from.as_u32() as usize;
            let to = edge.to.as_u32() as usize;
            if from >= body.blocks.len() || to >= body.blocks.len() {
                continue;
            }
            let pre_kind = cfg_edge_kind(edge, &dominators);
            facts.push(TraceFact::CfgEdge(CfgEdgeFact::new(
                pre_function.clone(),
                sonatina_block_key(SONATINA_PREOPT_BLOCK_KIND, &owner, from),
                sonatina_block_key(SONATINA_PREOPT_BLOCK_KIND, &owner, to),
                pre_kind,
                None,
            )));
            facts.push(TraceFact::CfgEdge(CfgEdgeFact::new(
                post_function.clone(),
                sonatina_block_key(SONATINA_POSTOPT_BLOCK_KIND, &owner, from),
                sonatina_block_key(SONATINA_POSTOPT_BLOCK_KIND, &owner, to),
                pre_kind,
                None,
            )));
        }

        let cfg_hash = runtime_cfg_hash(body.blocks.len(), &cfg.edges);
        for natural_loop in natural_loops(body.blocks.len(), &cfg.predecessors, &cfg.edges) {
            let runtime_loop = mir::RuntimeLoopOrigin::new(
                instance,
                mir::RuntimeLoopSite::new(natural_loop.header, natural_loop.latch),
            )
            .export_key(&owner);
            let pre_loop = sonatina_loop_key(
                SONATINA_PREOPT_LOOP_KIND,
                &owner,
                natural_loop.header,
                natural_loop.latch,
            );
            let post_loop = sonatina_loop_key(
                SONATINA_POSTOPT_LOOP_KIND,
                &owner,
                natural_loop.header,
                natural_loop.latch,
            );
            push_node(&mut facts, pre_loop.clone());
            push_node(&mut facts, post_loop.clone());
            let pre_header = sonatina_block_key(
                SONATINA_PREOPT_BLOCK_KIND,
                &owner,
                natural_loop.header.as_u32() as usize,
            );
            let post_header = sonatina_block_key(
                SONATINA_POSTOPT_BLOCK_KIND,
                &owner,
                natural_loop.header.as_u32() as usize,
            );
            facts.push(TraceFact::Loop(LoopFact::new(
                pre_loop.clone(),
                pre_function.clone(),
                CompilerPhase::SonatinaPreOpt,
                pre_header.clone(),
                LoopDerivation::NaturalLoopAnalysis {
                    cfg_hash: cfg_hash.clone(),
                },
                LoopConfidence::SonatinaCfg,
            )));
            facts.push(TraceFact::Loop(LoopFact::new(
                post_loop.clone(),
                post_function.clone(),
                CompilerPhase::SonatinaPostOpt,
                post_header.clone(),
                LoopDerivation::NaturalLoopAnalysis {
                    cfg_hash: cfg_hash.clone(),
                },
                LoopConfidence::SonatinaCfg,
            )));
            facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                pre_loop.clone(),
                runtime_loop,
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::SonatinaPreOpt),
            )));
            facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                post_loop.clone(),
                pre_loop.clone(),
                OriginEdgeLabel::PreservedSnapshotIdentity,
                Some(CompilerPhase::SonatinaPostOpt),
            )));
            push_loop_blocks(
                &mut facts,
                &owner,
                &pre_loop,
                SONATINA_PREOPT_BLOCK_KIND,
                natural_loop.header,
                natural_loop.latch,
                &natural_loop.members,
            );
            push_loop_blocks(
                &mut facts,
                &owner,
                &post_loop,
                SONATINA_POSTOPT_BLOCK_KIND,
                natural_loop.header,
                natural_loop.latch,
                &natural_loop.members,
            );
            push_sonatina_loop_shape_facts(
                &mut facts,
                &pre_loop,
                SONATINA_PREOPT_BLOCK_KIND,
                SONATINA_PREOPT_INST_KIND,
                "sonatina.preopt.loop",
                &owner,
                &body,
                &natural_loop,
            );
            push_sonatina_loop_shape_facts(
                &mut facts,
                &post_loop,
                SONATINA_POSTOPT_BLOCK_KIND,
                SONATINA_POSTOPT_INST_KIND,
                "sonatina.postopt.loop",
                &owner,
                &body,
                &natural_loop,
            );
        }

        let mut instruction_index = 0u32;
        for (block_index, block) in body.blocks.iter().enumerate() {
            for (stmt_index, stmt) in block.stmts.iter().enumerate() {
                let runtime_stmt = mir::RuntimeStmtOrigin::new(
                    instance,
                    mir::RuntimeStmtSite::new(
                        mir::RBlockId::from_u32(block_index as u32),
                        mir::RuntimeStmtIndex::from_u32(stmt_index as u32),
                    ),
                )
                .export_key(&owner);
                push_sonatina_instruction_pair(
                    &mut facts,
                    &owner,
                    &pre_function,
                    &post_function,
                    block_index,
                    format!("stmt:{stmt_index}"),
                    instruction_index,
                    rmir_stmt_mnemonic(stmt),
                    runtime_stmt,
                );
                instruction_index += 1;
            }
            let runtime_terminator = mir::RuntimeTerminatorOrigin::new(
                instance,
                mir::RuntimeTerminatorSite::new(mir::RBlockId::from_u32(block_index as u32)),
            )
            .export_key(&owner);
            push_sonatina_instruction_pair(
                &mut facts,
                &owner,
                &pre_function,
                &post_function,
                block_index,
                "terminator".to_string(),
                instruction_index,
                rmir_terminator_mnemonic(&block.terminator),
                runtime_terminator,
            );
            instruction_index += 1;
        }
    }
    facts
}

pub fn bytecode_runtime_owner_key(
    package_key: &str,
    module_key: &str,
    contract_name: &str,
) -> String {
    format!("package:{package_key}:module:{module_key}:contract:{contract_name}:section:runtime")
}

pub fn sonatina_module_owner_key(package_key: &str, module_key: &str) -> String {
    format!("package:{package_key}:module:{module_key}:sonatina")
}

pub fn bytecode_function_key(owner_key: &str, function_local_key: &str) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts("bytecode.function", owner_key, function_local_key)
        .expect("codegen bytecode function key must be valid")
}

pub fn bytecode_code_object_key(owner_key: &str) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts("code.object", owner_key, "runtime")
        .expect("codegen bytecode code object key must be valid")
}

fn origin_node(key: OriginExportKey, kind: &str) -> TraceFact {
    TraceFact::OriginNode(OriginNodeFact::new(key, OriginNodeKind::new(kind)))
}

fn push_node(facts: &mut Vec<TraceFact>, key: OriginExportKey) {
    facts.push(origin_node(key.clone(), key.kind()));
}

#[derive(Clone, Debug)]
struct RuntimeCfg {
    edges: Vec<RuntimeCfgEdge>,
    predecessors: Vec<Vec<mir::RBlockId>>,
}

#[derive(Clone, Copy, Debug)]
struct RuntimeCfgEdge {
    from: mir::RBlockId,
    to: mir::RBlockId,
    kind: CfgEdgeKind,
}

#[derive(Clone, Debug)]
struct NaturalLoop {
    header: mir::RBlockId,
    latch: mir::RBlockId,
    members: Vec<mir::RBlockId>,
}

fn runtime_cfg(body: &mir::RuntimeBody<'_>) -> RuntimeCfg {
    let mut edges = Vec::new();
    let mut predecessors = vec![Vec::new(); body.blocks.len()];
    for (block_index, block) in body.blocks.iter().enumerate() {
        let from = mir::RBlockId::from_u32(block_index as u32);
        for edge in terminator_edges(from, &block.terminator) {
            if let Some(preds) = predecessors.get_mut(edge.to.as_u32() as usize) {
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

fn terminator_edges(from: mir::RBlockId, terminator: &mir::RTerminator<'_>) -> Vec<RuntimeCfgEdge> {
    match terminator {
        mir::RTerminator::Goto(to) => vec![RuntimeCfgEdge {
            from,
            to: *to,
            kind: CfgEdgeKind::Jump,
        }],
        mir::RTerminator::Branch {
            then_bb, else_bb, ..
        } => vec![
            RuntimeCfgEdge {
                from,
                to: *then_bb,
                kind: CfgEdgeKind::BranchTrue,
            },
            RuntimeCfgEdge {
                from,
                to: *else_bb,
                kind: CfgEdgeKind::BranchFalse,
            },
        ],
        mir::RTerminator::SwitchScalar { cases, default, .. } => {
            let mut edges = cases
                .iter()
                .map(|(_, to)| RuntimeCfgEdge {
                    from,
                    to: *to,
                    kind: CfgEdgeKind::BranchTrue,
                })
                .collect::<Vec<_>>();
            edges.push(RuntimeCfgEdge {
                from,
                to: *default,
                kind: CfgEdgeKind::BranchFalse,
            });
            edges
        }
        mir::RTerminator::MatchEnumTag { cases, default, .. } => {
            let mut edges = cases
                .iter()
                .map(|(_, to)| RuntimeCfgEdge {
                    from,
                    to: *to,
                    kind: CfgEdgeKind::BranchTrue,
                })
                .collect::<Vec<_>>();
            if let Some(default) = default {
                edges.push(RuntimeCfgEdge {
                    from,
                    to: *default,
                    kind: CfgEdgeKind::BranchFalse,
                });
            }
            edges
        }
        mir::RTerminator::TerminalCall { .. }
        | mir::RTerminator::ReturnData { .. }
        | mir::RTerminator::Revert { .. }
        | mir::RTerminator::SelfDestruct { .. }
        | mir::RTerminator::Trap
        | mir::RTerminator::Return(_)
        | mir::RTerminator::Stop => Vec::new(),
    }
}

fn cfg_edge_kind(edge: &RuntimeCfgEdge, dominators: &[BTreeSet<usize>]) -> CfgEdgeKind {
    let from = edge.from.as_u32() as usize;
    let to = edge.to.as_u32() as usize;
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
    predecessors: &[Vec<mir::RBlockId>],
    edges: &[RuntimeCfgEdge],
) -> Vec<NaturalLoop> {
    let dominators = dominators(block_count, predecessors);
    let mut seen = BTreeSet::new();
    let mut loops = Vec::new();
    for edge in edges {
        let from = edge.from.as_u32() as usize;
        let to = edge.to.as_u32() as usize;
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
    predecessors: &[Vec<mir::RBlockId>],
    header: mir::RBlockId,
    latch: mir::RBlockId,
) -> Vec<mir::RBlockId> {
    let header_index = header.as_u32() as usize;
    let latch_index = latch.as_u32() as usize;
    let mut members = BTreeSet::from([header_index, latch_index]);
    let mut stack = vec![latch_index];
    while let Some(block) = stack.pop() {
        for predecessor in predecessors.get(block).into_iter().flatten() {
            let predecessor = predecessor.as_u32() as usize;
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
        .map(|block| mir::RBlockId::from_u32(block as u32))
        .collect()
}

fn dominators(block_count: usize, predecessors: &[Vec<mir::RBlockId>]) -> Vec<BTreeSet<usize>> {
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
                .map(|pred| pred.as_u32() as usize)
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
    let mut hash = 0xcbf29ce484222325u64;
    hash_u32(&mut hash, block_count as u32);
    for edge in edges {
        hash_u32(&mut hash, edge.from.as_u32());
        hash_u32(&mut hash, edge.to.as_u32());
        hash_bytes(&mut hash, cfg_edge_kind_name(edge.kind).as_bytes());
    }
    format!("fnv64:{hash:016x}")
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

fn push_loop_blocks(
    facts: &mut Vec<TraceFact>,
    owner: &mir::RuntimeInstanceOwnerKey,
    loop_key: &OriginExportKey,
    block_kind: &str,
    header: mir::RBlockId,
    latch: mir::RBlockId,
    members: &[mir::RBlockId],
) {
    facts.push(TraceFact::LoopBlock(LoopBlockFact::new(
        loop_key.clone(),
        sonatina_block_key(block_kind, owner, header.as_u32() as usize),
        LoopBlockRole::Header,
    )));
    for block in members {
        if *block == header {
            continue;
        }
        let role = if *block == latch {
            LoopBlockRole::Latch
        } else {
            LoopBlockRole::Body
        };
        facts.push(TraceFact::LoopBlock(LoopBlockFact::new(
            loop_key.clone(),
            sonatina_block_key(block_kind, owner, block.as_u32() as usize),
            role,
        )));
    }
}

#[allow(clippy::too_many_arguments)]
fn push_sonatina_loop_shape_facts(
    facts: &mut Vec<TraceFact>,
    loop_key: &OriginExportKey,
    block_kind: &str,
    instruction_kind: &str,
    level: &str,
    owner: &mir::RuntimeInstanceOwnerKey,
    body: &mir::RuntimeBody<'_>,
    natural_loop: &NaturalLoop,
) {
    let Ok(graph) = sonatina_loop_shape_graph(
        loop_key,
        block_kind,
        instruction_kind,
        owner,
        body,
        natural_loop,
    ) else {
        return;
    };
    let Ok(policy) = loop_shape_policy(level) else {
        return;
    };
    let Ok(hashes) = hash_shape_graph(&policy, &graph) else {
        return;
    };
    facts.extend(trace_facts::shape_hash_facts(&graph, &policy, &hashes));
}

fn sonatina_loop_shape_graph(
    loop_key: &OriginExportKey,
    block_kind: &str,
    instruction_kind: &str,
    owner: &mir::RuntimeInstanceOwnerKey,
    body: &mir::RuntimeBody<'_>,
    natural_loop: &NaturalLoop,
) -> Result<ShapeGraph, shape_address::ShapeError> {
    let loop_node = ShapeNodeKey::entity(loop_key.clone());
    let mut graph = ShapeGraph::new(ShapeGraphKey::new(loop_key.clone(), "sonatina-loop-shape")?);
    graph.add_node(loop_node.clone(), loop_key.kind())?;
    graph.add_field(
        &loop_node,
        ShapeDimension::Structure,
        "phase",
        loop_key.kind(),
    )?;
    for (block_ordinal, block_id) in natural_loop.members.iter().enumerate() {
        let Some(block) = body.blocks.get(block_id.as_u32() as usize) else {
            continue;
        };
        let block_index = block_id.as_u32() as usize;
        let block_key = sonatina_block_key(block_kind, owner, block_index);
        let block_node = ShapeNodeKey::entity(block_key);
        graph.add_node(block_node.clone(), block_kind)?;
        graph.add_child(&loop_node, "block", block_ordinal as u32, &block_node)?;
        for (stmt_index, stmt) in block.stmts.iter().enumerate() {
            let instruction = sonatina_key(
                instruction_kind,
                owner,
                format!("block:{block_index}:stmt:{stmt_index}"),
            );
            let instruction_node = ShapeNodeKey::entity(instruction);
            graph.add_node(instruction_node.clone(), instruction_kind)?;
            graph.add_field(
                &instruction_node,
                ShapeDimension::Structure,
                "mnemonic",
                rmir_stmt_mnemonic(stmt),
            )?;
            graph.add_child(
                &block_node,
                "instruction",
                stmt_index as u32,
                &instruction_node,
            )?;
        }
        let terminator = sonatina_key(
            instruction_kind,
            owner,
            format!("block:{block_index}:terminator"),
        );
        let terminator_node = ShapeNodeKey::entity(terminator);
        graph.add_node(terminator_node.clone(), instruction_kind)?;
        graph.add_field(
            &terminator_node,
            ShapeDimension::Structure,
            "mnemonic",
            rmir_terminator_mnemonic(&block.terminator),
        )?;
        graph.add_child(
            &block_node,
            "instruction",
            block.stmts.len() as u32,
            &terminator_node,
        )?;
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

#[allow(clippy::too_many_arguments)]
fn push_sonatina_instruction_pair(
    facts: &mut Vec<TraceFact>,
    owner: &mir::RuntimeInstanceOwnerKey,
    pre_function: &OriginExportKey,
    post_function: &OriginExportKey,
    block_index: usize,
    local_site: String,
    index: u32,
    mnemonic: &'static str,
    runtime_origin: OriginExportKey,
) {
    let pre_inst = sonatina_key(
        SONATINA_PREOPT_INST_KIND,
        owner,
        format!("block:{block_index}:{local_site}"),
    );
    let post_inst = sonatina_key(
        SONATINA_POSTOPT_INST_KIND,
        owner,
        format!("block:{block_index}:{local_site}"),
    );
    let pre_block = sonatina_block_key(SONATINA_PREOPT_BLOCK_KIND, owner, block_index);
    let post_block = sonatina_block_key(SONATINA_POSTOPT_BLOCK_KIND, owner, block_index);
    push_node(facts, pre_inst.clone());
    push_node(facts, post_inst.clone());
    facts.push(TraceFact::Instruction(InstructionFact::new(
        pre_inst.clone(),
        pre_function.clone(),
        index,
        mnemonic,
    )));
    facts.push(TraceFact::Instruction(InstructionFact::new(
        post_inst.clone(),
        post_function.clone(),
        index,
        mnemonic,
    )));
    facts.push(TraceFact::InstructionBlock(InstructionBlockFact::new(
        pre_inst.clone(),
        pre_block,
        CompilerPhase::SonatinaPreOpt,
    )));
    facts.push(TraceFact::InstructionBlock(InstructionBlockFact::new(
        post_inst.clone(),
        post_block,
        CompilerPhase::SonatinaPostOpt,
    )));
    facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
        pre_inst.clone(),
        runtime_origin,
        OriginEdgeLabel::LoweredFrom,
        Some(CompilerPhase::SonatinaPreOpt),
    )));
    facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
        post_inst,
        pre_inst,
        OriginEdgeLabel::PreservedSnapshotIdentity,
        Some(CompilerPhase::SonatinaPostOpt),
    )));
}

fn rmir_stmt_mnemonic(stmt: &mir::RStmt<'_>) -> &'static str {
    match stmt {
        mir::RStmt::Assign { .. } => "rmir.assign",
        mir::RStmt::EnumAssertVariant { .. } => "rmir.enum_assert_variant",
        mir::RStmt::Store { .. } => "rmir.store",
        mir::RStmt::CopyInto { .. } => "rmir.copy_into",
        mir::RStmt::EnumSetTag { .. } => "rmir.enum_set_tag",
        mir::RStmt::EnumWriteVariant { .. } => "rmir.enum_write_variant",
    }
}

fn rmir_terminator_mnemonic(terminator: &mir::RTerminator<'_>) -> &'static str {
    match terminator {
        mir::RTerminator::Goto(_) => "rmir.goto",
        mir::RTerminator::Branch { .. } => "rmir.branch",
        mir::RTerminator::SwitchScalar { .. } => "rmir.switch_scalar",
        mir::RTerminator::MatchEnumTag { .. } => "rmir.match_enum_tag",
        mir::RTerminator::TerminalCall { .. } => "rmir.terminal_call",
        mir::RTerminator::ReturnData { .. } => "rmir.return_data",
        mir::RTerminator::Revert { .. } => "rmir.revert",
        mir::RTerminator::SelfDestruct { .. } => "rmir.self_destruct",
        mir::RTerminator::Trap => "rmir.trap",
        mir::RTerminator::Return(_) => "rmir.return",
        mir::RTerminator::Stop => "rmir.stop",
    }
}

fn sonatina_block_key(
    kind: &str,
    owner: &mir::RuntimeInstanceOwnerKey,
    block_index: usize,
) -> OriginExportKey {
    sonatina_key(kind, owner, format!("block:{block_index}"))
}

fn sonatina_phase_kinds(
    phase: CompilerPhase,
) -> Option<(&'static str, &'static str, &'static str)> {
    match phase {
        CompilerPhase::SonatinaPreOpt => Some((
            SONATINA_PREOPT_FUNCTION_KIND,
            SONATINA_PREOPT_BLOCK_KIND,
            SONATINA_PREOPT_INST_KIND,
        )),
        CompilerPhase::SonatinaPostOpt => Some((
            SONATINA_POSTOPT_FUNCTION_KIND,
            SONATINA_POSTOPT_BLOCK_KIND,
            SONATINA_POSTOPT_INST_KIND,
        )),
        _ => None,
    }
}

fn sonatina_function_key(
    kind: &str,
    owner_key: &str,
    function: sonatina_ir::module::FuncRef,
) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(kind, owner_key, format!("function:{function:?}"))
        .expect("Sonatina function trace key must be valid")
}

fn sonatina_trace_block_key(
    kind: &str,
    owner_key: &str,
    function: sonatina_ir::module::FuncRef,
    block: sonatina_ir::BlockId,
) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(
        kind,
        owner_key,
        format!("function:{function:?}:block:{block:?}"),
    )
    .expect("Sonatina block trace key must be valid")
}

fn sonatina_trace_inst_key(
    kind: &str,
    owner_key: &str,
    function: sonatina_ir::module::FuncRef,
    inst: sonatina_ir::InstId,
) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(
        kind,
        owner_key,
        format!("function:{function:?}:inst:{inst:?}"),
    )
    .expect("Sonatina instruction trace key must be valid")
}

fn sonatina_trace_edge_kind(kind: SonatinaCfgEdgeKind, ordinal: usize) -> CfgEdgeKind {
    match kind {
        SonatinaCfgEdgeKind::Jump => CfgEdgeKind::Jump,
        SonatinaCfgEdgeKind::Branch if ordinal == 0 => CfgEdgeKind::BranchTrue,
        SonatinaCfgEdgeKind::Branch if ordinal == 1 => CfgEdgeKind::BranchFalse,
        SonatinaCfgEdgeKind::Branch | SonatinaCfgEdgeKind::BranchTable => CfgEdgeKind::Unknown,
    }
}

fn sonatina_frontend_origin_for_inst(
    module: &sonatina_ir::Module,
    function_ref: sonatina_ir::module::FuncRef,
    inst: sonatina_ir::InstId,
) -> Option<OriginExportKey> {
    module.func_store.try_view(function_ref, |function| {
        let loc = function.inst_debug_loc(inst)?;
        let origin = function
            .debug
            .debug_loc(loc)?
            .primary_origin
            .and_then(|origin| function.debug.frontend_origin(origin))?;
        let external_key = origin.external_key.as_deref()?;
        serde_json::from_str(external_key).ok()
    })?
}

fn sonatina_loop_key(
    kind: &str,
    owner: &mir::RuntimeInstanceOwnerKey,
    header: mir::RBlockId,
    latch: mir::RBlockId,
) -> OriginExportKey {
    sonatina_key(
        kind,
        owner,
        format!("loop:header:{}:latch:{}", header.as_u32(), latch.as_u32()),
    )
}

fn sonatina_key(
    kind: &str,
    owner: &mir::RuntimeInstanceOwnerKey,
    local: impl AsRef<str>,
) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(kind, owner.as_str(), local.as_ref())
        .expect("Sonatina trace key must be valid")
}

fn hash_u32(hash: &mut u64, value: u32) {
    hash_bytes(hash, &value.to_le_bytes());
}

fn hash_bytes(hash: &mut u64, bytes: &[u8]) {
    for byte in bytes {
        *hash ^= u64::from(*byte);
        *hash = hash.wrapping_mul(0x100000001b3);
    }
}

fn bytecode_content_hash(bytecode: &[u8]) -> String {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in bytecode {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("fnv64:{hash:016x}")
}

fn evm_push_immediate_len(opcode: u8) -> usize {
    if (0x60..=0x7f).contains(&opcode) {
        (opcode - 0x5f) as usize
    } else {
        0
    }
}

fn evm_mnemonic(opcode: u8) -> &'static str {
    match opcode {
        0x00 => "STOP",
        0x01 => "ADD",
        0x02 => "MUL",
        0x03 => "SUB",
        0x04 => "DIV",
        0x10 => "LT",
        0x11 => "GT",
        0x14 => "EQ",
        0x15 => "ISZERO",
        0x16 => "AND",
        0x17 => "OR",
        0x19 => "NOT",
        0x20 => "KECCAK256",
        0x35 => "CALLDATALOAD",
        0x36 => "CALLDATASIZE",
        0x37 => "CALLDATACOPY",
        0x39 => "CODECOPY",
        0x51 => "MLOAD",
        0x52 => "MSTORE",
        0x53 => "MSTORE8",
        0x54 => "SLOAD",
        0x55 => "SSTORE",
        0x56 => "JUMP",
        0x57 => "JUMPI",
        0x5b => "JUMPDEST",
        0x5f => "PUSH0",
        0x60..=0x7f => "PUSH",
        0x80..=0x8f => "DUP",
        0x90..=0x9f => "SWAP",
        0xf3 => "RETURN",
        0xfd => "REVERT",
        _ => "OP",
    }
}

fn evm_instruction_category(opcode: u8) -> InstructionCategory {
    match opcode {
        0x01..=0x07 | 0x10..=0x1d => InstructionCategory::Arithmetic,
        0x35 | 0x36 | 0x37 | 0x39 | 0x51 | 0x54 => InstructionCategory::Load,
        0x52 | 0x53 | 0x55 => InstructionCategory::Store,
        0x56 => InstructionCategory::Jump,
        0x57 => InstructionCategory::Branch,
        0x5f..=0x7f | 0x80..=0x9f => InstructionCategory::Move,
        _ => InstructionCategory::Unknown,
    }
}

fn evm_opcode_category(opcode: u8) -> OpcodeCategory {
    match opcode {
        0x01..=0x07 | 0x16..=0x1d => OpcodeCategory::Arithmetic,
        0x10..=0x15 => OpcodeCategory::Comparison,
        0x35..=0x37 => OpcodeCategory::CallData,
        0x39 | 0x51..=0x53 => OpcodeCategory::Memory,
        0x54 | 0x55 => OpcodeCategory::Storage,
        0x56 | 0x57 | 0x5b => OpcodeCategory::ControlFlow,
        0x5f..=0x7f => OpcodeCategory::Push,
        0x80..=0x9f => OpcodeCategory::Stack,
        0xf3 | 0xfd => OpcodeCategory::Return,
        _ => OpcodeCategory::Unknown,
    }
}

fn evm_static_gas(opcode: u8) -> u64 {
    match opcode {
        0x00 => 0,
        0x01..=0x03 | 0x10..=0x19 | 0x1b..=0x1d => 3,
        0x04..=0x07 => 5,
        0x20 => 30,
        0x35 | 0x36 => 3,
        0x37 | 0x39 => 3,
        0x51 | 0x52 | 0x53 => 3,
        0x54 => 100,
        0x55 => 100,
        0x56 => 8,
        0x57 => 10,
        0x5b => 1,
        0x5f..=0x7f => 3,
        0x80..=0x8f => 3,
        0x90..=0x9f => 3,
        0xf3 | 0xfd => 0,
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use common::{InputDb, origin::OriginExportKey};
    use driver::DriverDataBase;
    use trace_facts::{CompilerPhase, OriginNodeFact, OriginNodeKind, TraceFact, TraceValidator};
    use url::Url;

    use crate::{
        BytecodePcRange, BytecodeSourceMapEntry,
        trace::{
            bytecode_runtime_owner_key, emit_bytecode_instruction_facts, emit_codegen_facts,
            emit_sonatina_cfg_facts, emit_sonatina_trace_view_facts,
            frontend_origin_record_for_export_key,
        },
    };

    #[test]
    fn codegen_trace_emits_only_bytecode_origin_nodes() {
        let origin =
            OriginExportKey::try_from_raw_parts("bytecode.pc", "runtime:main", "pc:0..2").unwrap();
        let entry = BytecodeSourceMapEntry::non_source(
            origin.clone(),
            BytecodePcRange::try_new(0, 2).unwrap(),
            "abi dispatch",
        )
        .unwrap();

        let facts = emit_codegen_facts([&entry]);
        assert_eq!(TraceValidator::validate(&facts).unwrap().node_count, 1);
        assert!(matches!(
            &facts[0],
            TraceFact::OriginNode(node) if node.key == origin
        ));
    }

    #[test]
    fn codegen_trace_emits_instruction_facts_from_actual_bytecode() {
        let facts =
            emit_bytecode_instruction_facts("contract:Fib", "runtime", &[0x5f, 0x60, 0x01, 0x01]);
        let summary = TraceValidator::validate(&facts).unwrap();

        assert_eq!(summary.instruction_count, 3);
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::Instruction(instruction) if instruction.mnemonic == "ADD"
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::Opcode(opcode) if opcode.opcode == "PUSH"
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::GasCost(gas) if gas.gas > 0
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::CodeObject(code_object)
                if code_object.code_object.kind() == "code.object"
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::StaticGas(gas) if gas.base_cost > 0
        )));
        let extents = facts
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::InstructionExtent(extent) => Some(extent),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(extents.len(), 3);
        assert_eq!(extents.iter().map(|extent| extent.byte_len).sum::<u32>(), 4);
        assert!(extents.iter().any(|extent| {
            extent.instruction.local_key() == "pc:1"
                && extent.pc_range.start == 1
                && extent.pc_range.end == 3
                && extent.byte_len == 2
        }));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == "bytecode.pc"
                    && edge.to.kind() == "code.object"
                    && edge.label == trace_facts::OriginEdgeLabel::EmittedFrom
                    && edge.introduced_by == Some(trace_facts::CompilerPhase::BytecodeEmission)
        )));
        let shape_facts =
            super::emit_bytecode_shape_facts("contract:Fib", "runtime", &[0x5f, 0x60, 0x01, 0x01]);
        assert!(shape_facts.iter().any(|fact| matches!(
            fact,
            TraceFact::ShapeGraphHash(hash) if hash.graph.local.as_str() == "bytecode-shape"
        )));
    }

    #[test]
    fn sonatina_trace_view_adapter_emits_cfg_and_frontend_origin_edge() {
        use sonatina_ir::{
            DebugConfidence, DebugLoc, Linkage, Signature, Type, builder::ModuleBuilder,
            func_cursor::InstInserter, inst::arith::Add, isa::Isa, isa::evm::Evm,
            module::ModuleCtx,
        };
        use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

        let evm = Evm::new(TargetTriple::new(
            Architecture::Evm,
            Vendor::Ethereum,
            OperatingSystem::Evm(EvmVersion::London),
        ));
        let mb = ModuleBuilder::new(ModuleCtx::new(&evm));
        let func_ref = mb
            .declare_function(Signature::new_single(
                "traced",
                Linkage::Public,
                &[],
                Type::I32,
            ))
            .unwrap();
        let mut builder = mb.func_builder::<InstInserter>(func_ref);
        let block = builder.append_block();
        builder.switch_to_block(block);
        let lhs = builder.make_imm_value(1i32);
        let rhs = builder.make_imm_value(2i32);
        let value = builder.insert_inst(Add::new(evm.inst_set(), lhs, rhs), Type::I32);
        let inst = builder.func.dfg.value_inst(value).unwrap();
        let source_origin =
            OriginExportKey::try_from_raw_parts("mir.stmt", "runtime:test", "block:0:stmt:0")
                .unwrap();
        let frontend_origin =
            builder
                .func
                .debug
                .add_frontend_origin(frontend_origin_record_for_export_key(
                    &source_origin,
                    sonatina_ir::FrontendOriginKind::SourceStmt,
                ));
        let loc = builder.func.debug.add_debug_loc(DebugLoc {
            primary_origin: Some(frontend_origin),
            source_span: None,
            confidence: DebugConfidence::Exact,
        });
        builder.func.set_inst_debug_loc(inst, loc);
        builder.insert_return(value);
        builder.seal_all();
        builder.finish();
        let module = mb.build();

        let mut facts = vec![TraceFact::OriginNode(OriginNodeFact::new(
            source_origin.clone(),
            OriginNodeKind::new(source_origin.kind()),
        ))];
        facts.extend(emit_sonatina_trace_view_facts(
            "owner:test",
            &module,
            CompilerPhase::SonatinaPreOpt,
        ));
        TraceValidator::validate(&facts).unwrap();

        assert!(facts.iter().any(|fact| {
            matches!(fact, TraceFact::Instruction(instruction) if instruction.mnemonic == "add")
        }));
        assert!(facts.iter().any(|fact| {
            matches!(
                fact,
                TraceFact::OriginEdge(edge)
                    if edge.to == source_origin
                        && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
                        && edge.introduced_by == Some(CompilerPhase::SonatinaPreOpt)
            )
        }));
    }

    #[test]
    fn bytecode_owner_key_includes_package_module_contract_and_section() {
        let first = bytecode_runtime_owner_key("pkg:a", "mod:fib", "Fib");
        let same_contract_other_module = bytecode_runtime_owner_key("pkg:a", "mod:other", "Fib");
        let same_module_other_package = bytecode_runtime_owner_key("pkg:b", "mod:fib", "Fib");

        assert_ne!(first, same_contract_other_module);
        assert_ne!(first, same_module_other_package);
        assert!(first.contains("package:pkg:a"));
        assert!(first.contains("module:mod:fib"));
        assert!(first.contains("contract:Fib"));
        assert!(first.contains("section:runtime"));
    }

    #[test]
    fn sonatina_trace_bridges_mir_loop_to_sonatina_loop() {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///sonatina_trace_loop.fe").unwrap(),
            Some(
                r#"
fn main() -> u32 {
    let mut i: u32 = 0
    while i < 4 {
        i = i + 1
    }
    i
}
"#
                .to_string(),
            ),
        );
        let top_mod = db.top_mod(file);
        let package = mir::build_runtime_package(&db, top_mod).expect("runtime package");
        let mut facts = mir::trace::emit_mir_facts(&db, package);
        facts.extend(emit_sonatina_cfg_facts(&db, package));

        TraceValidator::validate(&facts).unwrap();
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::Loop(loop_fact)
                if loop_fact.phase == trace_facts::CompilerPhase::SonatinaPreOpt
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::Loop(loop_fact)
                if loop_fact.phase == trace_facts::CompilerPhase::SonatinaPostOpt
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == super::SONATINA_PREOPT_LOOP_KIND
                    && edge.to.kind() == "runtime.loop"
                    && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::InstructionBlock(block)
                if block.phase == trace_facts::CompilerPhase::SonatinaPreOpt
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == super::SONATINA_PREOPT_INST_KIND
                    && matches!(edge.to.kind(), "runtime.stmt" | "runtime.terminator")
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::ShapeGraphHash(hash) if hash.graph.local.as_str() == "sonatina-loop-shape"
        )));
    }
}
