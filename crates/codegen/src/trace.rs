use std::collections::{BTreeMap, BTreeSet};

use common::{file::IngotFileKind, origin::OriginExportKey};
use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use mir::build_runtime_package;
use shape_address::{
    ShapeCyclePolicy, ShapeDimension, ShapeGraph, ShapeGraphKey, ShapeHashPolicy, ShapeNodeKey,
    ShapeViewMode, hash_shape_graph,
};
use sonatina_codegen::object::{PcMapEntry, SectionObservability};
use sonatina_ir::{
    CfgEdgeKind as SonatinaCfgEdgeKind, DebugConfidence, FrontendOriginKind, FrontendOriginRecord,
    SonatinaTraceView,
};
use trace_facts::{
    BlockFact, CategorySource, CfgEdgeFact, CfgEdgeKind, CodeObjectFact, CodeObjectKind,
    CompilerEventFact, CompilerEventKind, CompilerPhase, CompilerReason, DynamicGasKind,
    EvmSchedule, FunctionFact, GasConfidence,
    InstructionBlockFact, InstructionCategory, InstructionCategoryFact, InstructionExtentFact,
    InstructionFact, LoopBlockFact, LoopBlockRole, LoopConfidence, LoopDerivation, LoopFact,
    LoopMembershipFact, OpcodeCategory, OpcodeFact, OriginEdgeFact, OriginEdgeLabel,
    OriginNodeFact, OriginNodeKind, PcRange, SourceFileFact, SourceSpanFact, StaticGasFact,
    TraceFact,
};

use crate::debug::BytecodeSourceMapEntry;
use crate::{OptLevel, compile_runtime_package_sonatina};

pub const SONATINA_PREOPT_FUNCTION_KIND: &str = "sonatina.preopt.function";
pub const SONATINA_PREOPT_BLOCK_KIND: &str = "sonatina.preopt.block";
pub const SONATINA_PREOPT_INST_KIND: &str = "sonatina.preopt.inst";
pub const SONATINA_PREOPT_LOOP_KIND: &str = "sonatina.preopt.loop";
pub const SONATINA_POSTOPT_FUNCTION_KIND: &str = "sonatina.postopt.function";
pub const SONATINA_POSTOPT_BLOCK_KIND: &str = "sonatina.postopt.block";
pub const SONATINA_POSTOPT_INST_KIND: &str = "sonatina.postopt.inst";
pub const SONATINA_POSTOPT_LOOP_KIND: &str = "sonatina.postopt.loop";
pub const SONATINA_EVM_PREPARED_INST_KIND: &str = "sonatina.evm.prepared.inst";
pub const EVM_VCODE_INST_KIND: &str = "evm.vcode.inst";

pub fn emit_observable_module_trace_facts(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    input_owner_key: &str,
    source_uri: impl Into<String>,
    source_display_name: impl Into<String>,
    source_text: &str,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<Vec<TraceFact>, crate::LowerError> {
    let package = build_runtime_package(db, top_mod)?;
    let package = crate::sonatina::select_runtime_package_contract(db, package, contract)?;
    emit_observable_package_trace_facts(
        db,
        package,
        top_mod,
        input_owner_key,
        source_uri,
        source_display_name,
        source_text,
        opt_level,
    )
}

/// Emit observable trace facts for every runtime module of an ingot as one
/// combined stream. Each module is keyed by its own file URL (the same
/// identity HIR span emission uses), and byte-identical facts that shared
/// bodies produce from multiple modules are deduplicated; conflicting
/// duplicates remain and fail validation.
pub fn emit_observable_ingot_trace_facts(
    db: &DriverDataBase,
    ingot: common::ingot::Ingot<'_>,
    opt_level: OptLevel,
) -> Result<Vec<TraceFact>, crate::LowerError> {
    let mut facts = Vec::new();
    let mut seen = BTreeSet::new();
    let mut traced_any_module = false;
    for (file_url, file) in ingot.files(db).iter() {
        if file.kind(db) != Some(IngotFileKind::Source) {
            continue;
        }
        let top_mod = db.top_mod(file);
        let Some(package) = mir::build_ingot_module_runtime_package(db, top_mod)? else {
            continue;
        };
        let source_text = file.text(db);
        let display_name = file_url
            .path()
            .rsplit('/')
            .next()
            .filter(|segment| !segment.is_empty())
            .unwrap_or("source.fe")
            .to_string();
        let module_facts = emit_observable_package_trace_facts(
            db,
            package,
            top_mod,
            file_url.as_str(),
            file_url.as_str(),
            display_name,
            source_text,
            opt_level,
        )?;
        traced_any_module = true;
        for fact in module_facts {
            let key = serde_json::to_string(&fact).expect("trace fact serialization cannot fail");
            if seen.insert(key) {
                facts.push(fact);
            }
        }
    }
    if !traced_any_module {
        return Err(mir::LowerError::Unsupported(
            "ingot has no runtime modules; nothing to trace".to_string(),
        )
        .into());
    }
    Ok(facts)
}

#[allow(clippy::too_many_arguments)]
fn emit_observable_package_trace_facts(
    db: &DriverDataBase,
    package: mir::RuntimePackage<'_>,
    top_mod: TopLevelMod<'_>,
    input_owner_key: &str,
    source_uri: impl Into<String>,
    source_display_name: impl Into<String>,
    source_text: &str,
    opt_level: OptLevel,
) -> Result<Vec<TraceFact>, crate::LowerError> {
    let mut facts = mir::trace::emit_mir_facts(db, package);
    let source_file = trace_source_file_key(input_owner_key);
    push_standalone_source_file_facts(
        &mut facts,
        &source_file,
        source_uri,
        source_display_name,
        source_text,
        Some(0),
    );
    let module_key = top_mod.name(db).data(db).to_string();
    let sonatina_module = compile_runtime_package_sonatina(db, &package, crate::EVM_LAYOUT)?;
    let sonatina_owner = sonatina_module_owner_key(input_owner_key, &module_key);
    facts.extend(emit_sonatina_trace_view_facts(
        &sonatina_owner,
        &sonatina_module,
        CompilerPhase::SonatinaPreOpt,
    ));
    let (bytecode, postopt_sonatina_facts) =
        crate::sonatina::emit_runtime_module_sonatina_bytecode_with_observability_and_trace(
            db,
            &package,
            sonatina_module,
            opt_level,
            &sonatina_owner,
        )?;
    let observed_bytecode_facts = emit_observed_bytecode_trace_facts(
        input_owner_key,
        &module_key,
        "function:runtime",
        &sonatina_owner,
        &bytecode,
        &postopt_sonatina_facts,
    );
    facts.extend(postopt_sonatina_facts);
    facts.extend(observed_bytecode_facts);
    for contract_name in bytecode.keys() {
        let owner_key = bytecode_runtime_owner_key(input_owner_key, &module_key, contract_name);
        let code_object = bytecode_code_object_key(&owner_key);
        if let Some(span) = whole_file_source_span(code_object, source_file.clone(), source_text) {
            facts.push(TraceFact::SourceSpan(span));
        }
    }
    Ok(facts)
}

pub fn trace_source_file_key(source_owner: &str) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts("source.file", source_owner, "file:0")
        .expect("trace source file key must be valid")
}

pub fn push_standalone_source_file_facts(
    facts: &mut Vec<TraceFact>,
    source_file: &OriginExportKey,
    uri: impl Into<String>,
    display_name: impl Into<String>,
    content: &str,
    source_id: Option<u32>,
) {
    let has_node = facts.iter().any(|fact| {
        matches!(
            fact,
            TraceFact::OriginNode(node) if node.key == *source_file
        )
    });
    if !has_node {
        facts.push(TraceFact::OriginNode(OriginNodeFact::new(
            source_file.clone(),
            OriginNodeKind::new(source_file.kind()),
        )));
    }

    let has_source_file = facts.iter().any(|fact| {
        matches!(
            fact,
            TraceFact::SourceFile(source) if source.file_key == *source_file
        )
    });
    if !has_source_file {
        facts.push(TraceFact::SourceFile(SourceFileFact::new(
            source_file.clone(),
            uri.into(),
            display_name.into(),
            trace_content_hash(content.as_bytes()),
            source_id,
        )));
    }
}

pub fn standalone_source_file_facts(
    source_file: &OriginExportKey,
    uri: impl Into<String>,
    display_name: impl Into<String>,
    content: &str,
    source_id: Option<u32>,
) -> Vec<TraceFact> {
    let mut facts = Vec::new();
    push_standalone_source_file_facts(
        &mut facts,
        source_file,
        uri,
        display_name,
        content,
        source_id,
    );
    facts
}

pub fn whole_file_source_span(
    origin: OriginExportKey,
    source_file: OriginExportKey,
    content: &str,
) -> Option<SourceSpanFact> {
    let end_byte = u32::try_from(content.len()).ok()?;
    if end_byte == 0 {
        return None;
    }
    let (end_line, end_column) = source_end_position(content);
    Some(SourceSpanFact::new(
        origin,
        source_file,
        0,
        end_byte,
        1,
        1,
        end_line,
        end_column,
    ))
}

fn source_end_position(content: &str) -> (u32, u32) {
    let mut line = 1u32;
    let mut column = 1u32;
    for ch in content.chars() {
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    (line, column)
}

fn trace_content_hash(bytes: &[u8]) -> String {
    format!("blake3:{}", blake3::hash(bytes).to_hex())
}

pub fn sonatina_postopt_inst_key(
    owner_key: &str,
    function: sonatina_ir::module::FuncRef,
    inst: sonatina_ir::InstId,
) -> OriginExportKey {
    sonatina_trace_inst_key(SONATINA_POSTOPT_INST_KIND, owner_key, function, inst)
}

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
    emit_bytecode_instruction_facts_with_observability(
        owner_key,
        function_local_key,
        bytecode,
        None,
        None,
        None,
    )
}

pub fn emit_observed_bytecode_trace_facts(
    input_owner_key: &str,
    module_key: &str,
    function_local_key: &str,
    sonatina_owner_key: &str,
    bytecode: &BTreeMap<String, crate::SonatinaContractBytecode>,
    postopt_sonatina_facts: &[TraceFact],
) -> Vec<TraceFact> {
    let postopt_sonatina_nodes = postopt_sonatina_facts
        .iter()
        .filter_map(|fact| match fact {
            TraceFact::OriginNode(node) if node.key.kind() == SONATINA_POSTOPT_INST_KIND => {
                Some(node.key.clone())
            }
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let mut facts = Vec::new();
    for (contract_name, artifact) in bytecode {
        let owner_key = bytecode_runtime_owner_key(input_owner_key, module_key, contract_name);
        facts.extend(emit_bytecode_instruction_facts_with_observability(
            &owner_key,
            function_local_key,
            &artifact.runtime,
            Some(sonatina_owner_key),
            artifact.runtime_observability.as_ref(),
            Some(&postopt_sonatina_nodes),
        ));
        let code_len = artifact
            .runtime_observability
            .as_ref()
            .map(|observability| (observability.code_bytes as usize).min(artifact.runtime.len()))
            .unwrap_or(artifact.runtime.len());
        facts.extend(emit_bytecode_shape_facts(
            &owner_key,
            function_local_key,
            &artifact.runtime[..code_len],
        ));
    }
    facts
}

pub fn emit_bytecode_instruction_facts_with_observability(
    owner_key: &str,
    function_local_key: &str,
    bytecode: &[u8],
    sonatina_owner_key: Option<&str>,
    observability: Option<&SectionObservability>,
    known_sonatina_endpoint_nodes: Option<&BTreeSet<OriginExportKey>>,
) -> Vec<TraceFact> {
    let function = bytecode_function_key(owner_key, function_local_key);
    let code_object = bytecode_code_object_key(owner_key);
    let pc_map = observability
        .map(|observability| build_pc_map(&observability.pc_map))
        .unwrap_or_default();
    let mut emitted_prepared_nodes = BTreeSet::new();
    let mut emitted_prepared_lineage_events = BTreeSet::new();
    let mut emitted_backend_edges = BTreeSet::new();
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
    // The section is code followed by data/embeds; decoding past code_bytes
    // desynchronizes the sweep (a data byte in 0x60..0x7f swallows what
    // follows as PUSH immediates) and mints garbage instruction facts.
    let code_len = observability
        .map(|observability| (observability.code_bytes as usize).min(bytecode.len()))
        .unwrap_or(bytecode.len());
    let mut pc = 0;
    let mut index = 0;
    while pc < code_len {
        let opcode = bytecode[pc];
        let instruction =
            OriginExportKey::try_from_raw_parts("bytecode.pc", owner_key, format!("pc:{pc}"))
                .expect("codegen bytecode PC key must be valid");
        let mnemonic = evm_mnemonic(opcode);
        let immediate_len = evm_push_immediate_len(opcode);
        let immediate = (immediate_len > 0).then(|| {
            let end = (pc + 1 + immediate_len).min(code_len);
            format!("0x{}", hex::encode(&bytecode[pc + 1..end]))
        });
        let byte_len = 1 + immediate_len.min(code_len.saturating_sub(pc + 1));
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
        // Entries the backend itself marks unmapped (synthetic section units,
        // unlowered code) must not mint Sonatina joins: synthetic FuncRefs are
        // numbered per section and can collide with real functions of another
        // contract in the same module, handing this pc an exact chain into the
        // wrong contract's source.
        if let Some(entry) =
            pc_map_entry_for_pc(&pc_map, pc as u32).filter(|entry| entry.unmapped_reason.is_none())
        {
            let mut prepared_inst = None;
            if let Some(sonatina_owner_key) = sonatina_owner_key {
                let vcode_key =
                    evm_vcode_inst_key(sonatina_owner_key, entry.func, entry.vcode_inst);
                if emitted_prepared_nodes.insert(vcode_key.clone()) {
                    facts.push(origin_node(vcode_key.clone(), EVM_VCODE_INST_KIND));
                }
                facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                    instruction.clone(),
                    vcode_key.clone(),
                    OriginEdgeLabel::EmittedFrom,
                    Some(CompilerPhase::BytecodeEmission),
                )));
                if let Some(ir_inst) = entry.ir_inst {
                    let key = sonatina_trace_inst_key(
                        SONATINA_EVM_PREPARED_INST_KIND,
                        sonatina_owner_key,
                        entry.func,
                        ir_inst,
                    );
                    if emitted_prepared_nodes.insert(key.clone()) {
                        facts.push(origin_node(key.clone(), SONATINA_EVM_PREPARED_INST_KIND));
                    }
                    if emitted_backend_edges.insert((vcode_key.clone(), key.clone())) {
                        facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                            vcode_key,
                            key.clone(),
                            OriginEdgeLabel::LoweredFrom,
                            Some(CompilerPhase::Backend),
                        )));
                    }
                    prepared_inst = Some(key);
                }
            }
            if let Some(frontend_origin) = entry
                .frontend_provenance
                .as_deref()
                .and_then(|raw| serde_json::from_str::<OriginExportKey>(raw).ok())
            {
                if frontend_origin.kind() == SONATINA_POSTOPT_INST_KIND {
                    if let Some(prepared_inst) = prepared_inst {
                        let endpoint_is_known = known_sonatina_endpoint_nodes
                            .is_some_and(|known| known.contains(&frontend_origin));
                        if endpoint_is_known {
                            emit_prepared_lineage_event(
                                &mut facts,
                                owner_key,
                                &prepared_inst,
                                &frontend_origin,
                                &mut emitted_prepared_lineage_events,
                            );
                            if emitted_backend_edges
                                .insert((prepared_inst.clone(), frontend_origin.clone()))
                            {
                                facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                                    prepared_inst,
                                    frontend_origin,
                                    OriginEdgeLabel::LoweredFrom,
                                    Some(CompilerPhase::Backend),
                                )));
                            }
                        }
                    }
                } else {
                    facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                        instruction.clone(),
                        frontend_origin,
                        OriginEdgeLabel::BackendPrepared,
                        Some(CompilerPhase::BytecodeEmission),
                    )));
                }
            }
        }
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
        if let Some((base_cost, dynamic_cost_kind)) = evm_static_gas(opcode) {
            let confidence = if dynamic_cost_kind.is_none() {
                GasConfidence::ExactStaticOpcode
            } else {
                GasConfidence::ConservativeStatic
            };
            facts.push(TraceFact::StaticGas(StaticGasFact::with_confidence(
                instruction,
                EvmSchedule::new("cancun"),
                base_cost,
                dynamic_cost_kind,
                confidence,
            )));
        }
        pc += byte_len;
        index += 1;
    }
    facts
}

/// Build the `pc_start`-keyed lookup used by [`pc_map_entry_for_pc`].
///
/// Two hazards are handled that a plain `.collect()` into a `BTreeMap` would
/// silently get wrong:
///   * Zero-length entries (`pc_end <= pc_start`) carry no range — the lookup's
///     `pc < pc_end` guard can never match them — but if one shares a
///     `pc_start` with a real `[start, end)` entry it would shadow it under the
///     single key, dropping the lineage link for every pc in that range. We
///     skip them entirely.
///   * Two valid entries sharing a `pc_start` (overlap) would let the
///     last-collected win arbitrarily. We keep the wider range deterministically
///     so a narrow duplicate can't truncate a real one, and assert in debug
///     builds that the surviving ranges don't overlap — the expected shape of a
///     well-formed pc_map.
fn build_pc_map(entries: &[PcMapEntry]) -> BTreeMap<u32, &PcMapEntry> {
    let mut pc_map: BTreeMap<u32, &PcMapEntry> = BTreeMap::new();
    for entry in entries {
        if entry.pc_end <= entry.pc_start {
            continue;
        }
        pc_map
            .entry(entry.pc_start)
            .and_modify(|existing| {
                if entry.pc_end > existing.pc_end {
                    *existing = entry;
                }
            })
            .or_insert(entry);
    }
    debug_assert!(
        pc_map
            .values()
            .zip(pc_map.values().skip(1))
            .all(|(a, b)| a.pc_end <= b.pc_start),
        "pc_map entries overlap after dedup: {pc_map:?}"
    );
    pc_map
}

fn pc_map_entry_for_pc<'a>(
    pc_map: &'a BTreeMap<u32, &'a PcMapEntry>,
    pc: u32,
) -> Option<&'a PcMapEntry> {
    pc_map
        .range(..=pc)
        .next_back()
        .and_then(|(_, entry)| (pc < entry.pc_end).then_some(*entry))
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
        let cfg_edges = sonatina_trace_cfg_edges(module, function_ref, &blocks);
        let predecessors = indexed_predecessors(blocks.len(), &cfg_edges);
        let dominators = indexed_dominators(blocks.len(), &predecessors);
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
        let mut block_instruction_keys = vec![Vec::new(); blocks.len()];
        for (block_index, block) in blocks.iter().copied().enumerate() {
            let block_key = sonatina_trace_block_key(block_kind, owner_key, function_ref, block);
            for edge in cfg_edges.iter().filter(|edge| edge.from == block_index) {
                let to_block =
                    sonatina_trace_block_key(block_kind, owner_key, function_ref, blocks[edge.to]);
                facts.push(TraceFact::CfgEdge(CfgEdgeFact::new(
                    function_key.clone(),
                    block_key.clone(),
                    to_block,
                    indexed_cfg_edge_kind(edge, &dominators),
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
                block_instruction_keys[block_index].push(inst_key.clone());
                instruction_index += 1;

                if let Some((frontend_origin, confidence)) =
                    sonatina_frontend_origin_for_inst(module, function_ref, inst)
                {
                    // Only Exact debug locs may claim exact lowering identity;
                    // Conservative attachments stay visible as synthetic
                    // explanation edges instead of masquerading as exact.
                    let label = match confidence {
                        DebugConfidence::Exact => OriginEdgeLabel::LoweredFrom,
                        _ => OriginEdgeLabel::SyntheticFor,
                    };
                    facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                        inst_key,
                        frontend_origin,
                        label,
                        Some(phase),
                    )));
                }
            }
        }

        let Some(loop_kind) = sonatina_loop_kind_for_phase(phase) else {
            continue;
        };
        let cfg_hash = indexed_cfg_hash(blocks.len(), &cfg_edges, &dominators);
        for natural_loop in indexed_natural_loops(blocks.len(), &predecessors, &cfg_edges) {
            let loop_key = sonatina_trace_loop_key(
                loop_kind,
                owner_key,
                function_ref,
                blocks[natural_loop.header],
                blocks[natural_loop.latch],
            );
            push_node(&mut facts, loop_key.clone());
            let header_block = sonatina_trace_block_key(
                block_kind,
                owner_key,
                function_ref,
                blocks[natural_loop.header],
            );
            let derivation = LoopDerivation::NaturalLoopAnalysis {
                cfg_hash: cfg_hash.clone(),
            };
            facts.push(TraceFact::Loop(LoopFact::new(
                loop_key.clone(),
                function_key.clone(),
                phase,
                header_block,
                derivation.clone(),
                LoopConfidence::SonatinaCfg,
            )));
            for member in &natural_loop.members {
                let role = if *member == natural_loop.header {
                    LoopBlockRole::Header
                } else if *member == natural_loop.latch {
                    LoopBlockRole::Latch
                } else {
                    LoopBlockRole::Body
                };
                let block_key =
                    sonatina_trace_block_key(block_kind, owner_key, function_ref, blocks[*member]);
                facts.push(TraceFact::LoopBlock(LoopBlockFact::new(
                    loop_key.clone(),
                    block_key,
                    role,
                )));
                for instruction in &block_instruction_keys[*member] {
                    facts.push(TraceFact::LoopMembership(LoopMembershipFact::new(
                        loop_key.clone(),
                        instruction.clone(),
                        derivation.clone(),
                    )));
                }
            }
        }
    }
    facts
}

/// Emit transitional Sonatina-surface CFG and lowering bridge facts from MIR.
///
/// Deprecated/test-only: this predates the frontend-neutral Sonatina
/// `SonatinaTraceView` adapter and is kept only as a compatibility bridge for
/// tests that still exercise the MIR-derived projection. Real trace emission
/// uses `emit_sonatina_trace_view_facts`. This must not be read as actual
/// pass-level Sonatina instrumentation or as a target bytecode membership proof.
#[deprecated(
    since = "26.1.0",
    note = "use emit_sonatina_trace_view_facts; this MIR-derived Sonatina bridge is transitional/test-only"
)]
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

fn emit_prepared_lineage_event(
    facts: &mut Vec<TraceFact>,
    owner_key: &str,
    prepared_inst: &OriginExportKey,
    postopt_inst: &OriginExportKey,
    emitted_events: &mut BTreeSet<(OriginExportKey, OriginExportKey)>,
) {
    let event_pair = (prepared_inst.clone(), postopt_inst.clone());
    if !emitted_events.insert(event_pair) {
        return;
    }
    let event = prepared_lineage_event_key(owner_key, prepared_inst, postopt_inst);
    facts.push(origin_node(event.clone(), "compiler.event"));
    facts.push(TraceFact::CompilerEvent(CompilerEventFact::new(
        event,
        CompilerPhase::Backend,
        CompilerEventKind::PreparedLineage,
        vec![postopt_inst.clone()],
        vec![prepared_inst.clone()],
        Some(CompilerReason::new(
            "explicit EVM prepared to optimized Sonatina lineage",
        )),
    )));
}

fn prepared_lineage_event_key(
    owner_key: &str,
    prepared_inst: &OriginExportKey,
    postopt_inst: &OriginExportKey,
) -> OriginExportKey {
    let digest = blake3::hash(
        format!(
            "{}\n{}",
            prepared_inst.canonical_storage_key(),
            postopt_inst.canonical_storage_key()
        )
        .as_bytes(),
    );
    let hex = digest.to_hex();
    OriginExportKey::try_from_raw_parts(
        "compiler.event",
        owner_key,
        format!("prepared-lineage:{}", &hex.as_str()[..16]),
    )
    .expect("prepared lineage compiler event key must be valid")
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
    let mut hasher = blake3::Hasher::new();
    hash_u32(&mut hasher, block_count as u32);
    for edge in edges {
        hash_u32(&mut hasher, edge.from.as_u32());
        hash_u32(&mut hasher, edge.to.as_u32());
        hash_bytes(&mut hasher, cfg_edge_kind_name(edge.kind).as_bytes());
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

fn sonatina_loop_kind_for_phase(phase: CompilerPhase) -> Option<&'static str> {
    match phase {
        CompilerPhase::SonatinaPreOpt => Some(SONATINA_PREOPT_LOOP_KIND),
        CompilerPhase::SonatinaPostOpt => Some(SONATINA_POSTOPT_LOOP_KIND),
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

fn evm_vcode_inst_key(
    owner_key: &str,
    function: sonatina_ir::module::FuncRef,
    vcode_inst: impl std::fmt::Debug,
) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(
        EVM_VCODE_INST_KIND,
        owner_key,
        format!("function:{function:?}:vcode_inst:{vcode_inst:?}"),
    )
    .expect("EVM VCode instruction trace key must be valid")
}

fn sonatina_trace_loop_key(
    kind: &str,
    owner_key: &str,
    function: sonatina_ir::module::FuncRef,
    header: sonatina_ir::BlockId,
    latch: sonatina_ir::BlockId,
) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(
        kind,
        owner_key,
        format!("function:{function:?}:loop:header:{header:?}:latch:{latch:?}"),
    )
    .expect("Sonatina loop trace key must be valid")
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
) -> Option<(OriginExportKey, DebugConfidence)> {
    module.func_store.try_view(function_ref, |function| {
        let loc = function.inst_debug_loc(inst)?;
        let debug_loc = function.debug.debug_loc(loc)?;
        let confidence = debug_loc.confidence;
        let origin = debug_loc
            .primary_origin
            .and_then(|origin| function.debug.frontend_origin(origin))?;
        let external_key = origin.external_key.as_deref()?;
        serde_json::from_str(external_key)
            .ok()
            .map(|key| (key, confidence))
    })?
}

#[derive(Clone, Copy, Debug)]
struct IndexedCfgEdge {
    from: usize,
    to: usize,
    kind: CfgEdgeKind,
}

#[derive(Clone, Debug)]
struct IndexedNaturalLoop {
    header: usize,
    latch: usize,
    members: Vec<usize>,
}

fn sonatina_trace_cfg_edges(
    module: &sonatina_ir::Module,
    function_ref: sonatina_ir::module::FuncRef,
    blocks: &[sonatina_ir::BlockId],
) -> Vec<IndexedCfgEdge> {
    let mut edges = Vec::new();
    for (from, block) in blocks.iter().copied().enumerate() {
        for edge in module.trace_block_successors(function_ref, block) {
            let Some(to) = blocks.iter().position(|candidate| *candidate == edge.to) else {
                continue;
            };
            edges.push(IndexedCfgEdge {
                from,
                to,
                kind: sonatina_trace_edge_kind(edge.kind, edge.ordinal),
            });
        }
    }
    edges
}

fn indexed_predecessors(block_count: usize, edges: &[IndexedCfgEdge]) -> Vec<Vec<usize>> {
    let mut predecessors = vec![Vec::new(); block_count];
    for edge in edges {
        if edge.from < block_count && edge.to < block_count {
            predecessors[edge.to].push(edge.from);
        }
    }
    predecessors
}

fn indexed_cfg_edge_kind(edge: &IndexedCfgEdge, dominators: &[BTreeSet<usize>]) -> CfgEdgeKind {
    if dominators
        .get(edge.from)
        .is_some_and(|dominator_set| dominator_set.contains(&edge.to))
    {
        CfgEdgeKind::Backedge
    } else {
        edge.kind
    }
}

fn indexed_natural_loops(
    block_count: usize,
    predecessors: &[Vec<usize>],
    edges: &[IndexedCfgEdge],
) -> Vec<IndexedNaturalLoop> {
    let dominators = indexed_dominators(block_count, predecessors);
    let mut seen = BTreeSet::new();
    let mut loops = Vec::new();
    for edge in edges {
        if edge.from >= block_count || edge.to >= block_count {
            continue;
        }
        if !dominators[edge.from].contains(&edge.to) || !seen.insert((edge.to, edge.from)) {
            continue;
        }
        loops.push(IndexedNaturalLoop {
            header: edge.to,
            latch: edge.from,
            members: indexed_natural_loop_members(block_count, predecessors, edge.to, edge.from),
        });
    }
    loops
}

fn indexed_natural_loop_members(
    block_count: usize,
    predecessors: &[Vec<usize>],
    header: usize,
    latch: usize,
) -> Vec<usize> {
    let mut members = BTreeSet::from([header, latch]);
    let mut stack = vec![latch];
    while let Some(block) = stack.pop() {
        for predecessor in predecessors.get(block).into_iter().flatten().copied() {
            if predecessor >= block_count || !members.insert(predecessor) {
                continue;
            }
            if predecessor != header {
                stack.push(predecessor);
            }
        }
    }
    members.into_iter().collect()
}

fn indexed_dominators(block_count: usize, predecessors: &[Vec<usize>]) -> Vec<BTreeSet<usize>> {
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
                .copied()
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

fn indexed_cfg_hash(
    block_count: usize,
    edges: &[IndexedCfgEdge],
    dominators: &[BTreeSet<usize>],
) -> String {
    let mut hasher = blake3::Hasher::new();
    hash_u32(&mut hasher, block_count as u32);
    for edge in edges {
        hash_u32(&mut hasher, edge.from as u32);
        hash_u32(&mut hasher, edge.to as u32);
        hash_bytes(
            &mut hasher,
            cfg_edge_kind_name(indexed_cfg_edge_kind(edge, dominators)).as_bytes(),
        );
    }
    format!("blake3:{}", hasher.finalize().to_hex())
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

fn hash_u32(hasher: &mut blake3::Hasher, value: u32) {
    hasher.update(&value.to_le_bytes());
}

fn hash_bytes(hasher: &mut blake3::Hasher, bytes: &[u8]) {
    hash_u32(hasher, bytes.len() as u32);
    hasher.update(bytes);
}

fn bytecode_content_hash(bytecode: &[u8]) -> String {
    format!("blake3:{}", blake3::hash(bytecode).to_hex())
}

fn evm_push_immediate_len(opcode: u8) -> usize {
    if (0x60..=0x7f).contains(&opcode) {
        (opcode - 0x5f) as usize
    } else {
        0
    }
}

fn evm_mnemonic(opcode: u8) -> String {
    match opcode {
        0x60..=0x7f => return format!("PUSH{}", opcode - 0x5f),
        0x80..=0x8f => return format!("DUP{}", opcode - 0x7f),
        0x90..=0x9f => return format!("SWAP{}", opcode - 0x8f),
        0xa0..=0xa4 => return format!("LOG{}", opcode - 0xa0),
        _ => {}
    }
    match opcode {
        0x00 => "STOP",
        0x01 => "ADD",
        0x02 => "MUL",
        0x03 => "SUB",
        0x04 => "DIV",
        0x05 => "SDIV",
        0x06 => "MOD",
        0x07 => "SMOD",
        0x08 => "ADDMOD",
        0x09 => "MULMOD",
        0x0a => "EXP",
        0x0b => "SIGNEXTEND",
        0x10 => "LT",
        0x11 => "GT",
        0x12 => "SLT",
        0x13 => "SGT",
        0x14 => "EQ",
        0x15 => "ISZERO",
        0x16 => "AND",
        0x17 => "OR",
        0x18 => "XOR",
        0x19 => "NOT",
        0x1a => "BYTE",
        0x1b => "SHL",
        0x1c => "SHR",
        0x1d => "SAR",
        0x20 => "KECCAK256",
        0x30 => "ADDRESS",
        0x31 => "BALANCE",
        0x32 => "ORIGIN",
        0x33 => "CALLER",
        0x34 => "CALLVALUE",
        0x35 => "CALLDATALOAD",
        0x36 => "CALLDATASIZE",
        0x37 => "CALLDATACOPY",
        0x38 => "CODESIZE",
        0x39 => "CODECOPY",
        0x3a => "GASPRICE",
        0x3b => "EXTCODESIZE",
        0x3c => "EXTCODECOPY",
        0x3d => "RETURNDATASIZE",
        0x3e => "RETURNDATACOPY",
        0x3f => "EXTCODEHASH",
        0x40 => "BLOCKHASH",
        0x41 => "COINBASE",
        0x42 => "TIMESTAMP",
        0x43 => "NUMBER",
        0x44 => "PREVRANDAO",
        0x45 => "GASLIMIT",
        0x46 => "CHAINID",
        0x47 => "SELFBALANCE",
        0x48 => "BASEFEE",
        0x49 => "BLOBHASH",
        0x4a => "BLOBBASEFEE",
        0x50 => "POP",
        0x51 => "MLOAD",
        0x52 => "MSTORE",
        0x53 => "MSTORE8",
        0x54 => "SLOAD",
        0x55 => "SSTORE",
        0x56 => "JUMP",
        0x57 => "JUMPI",
        0x58 => "PC",
        0x59 => "MSIZE",
        0x5a => "GAS",
        0x5b => "JUMPDEST",
        0x5c => "TLOAD",
        0x5d => "TSTORE",
        0x5e => "MCOPY",
        0x5f => "PUSH0",
        0xf0 => "CREATE",
        0xf1 => "CALL",
        0xf2 => "CALLCODE",
        0xf3 => "RETURN",
        0xf4 => "DELEGATECALL",
        0xf5 => "CREATE2",
        0xfa => "STATICCALL",
        0xfd => "REVERT",
        0xfe => "INVALID",
        0xff => "SELFDESTRUCT",
        _ => "OP",
    }
    .to_string()
}

fn evm_instruction_category(opcode: u8) -> InstructionCategory {
    match opcode {
        0x01..=0x07 | 0x10..=0x1d => InstructionCategory::Arithmetic,
        0x35 | 0x36 | 0x37 | 0x39 | 0x51 | 0x54 => InstructionCategory::Load,
        0x52 | 0x53 | 0x55 => InstructionCategory::Store,
        0x56 => InstructionCategory::Jump,
        0x57 => InstructionCategory::Branch,
        0x5f..=0x9f => InstructionCategory::Move,
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

/// Cancun base costs. `Some((base, None))` is the exact static cost;
/// `Some((base, Some(kind)))` is a guaranteed minimum with a dynamic
/// component; `None` means the opcode is not statically modeled and no
/// StaticGasFact is emitted for it.
fn evm_static_gas(opcode: u8) -> Option<(u64, Option<DynamicGasKind>)> {
    Some(match opcode {
        0x00 => (0, None),                                    // STOP
        0x01 | 0x03 => (3, None),                             // ADD, SUB
        0x02 => (5, None),                                    // MUL
        0x04..=0x07 => (5, None),                             // DIV..SMOD
        0x08 | 0x09 => (8, None),                             // ADDMOD, MULMOD
        0x0a => (10, Some(DynamicGasKind::Unknown)),          // EXP
        0x0b => (5, None),                                    // SIGNEXTEND
        0x10..=0x1d => (3, None),                             // LT..SAR incl. BYTE
        0x20 => (30, Some(DynamicGasKind::Keccak)),           // KECCAK256
        0x31 | 0x3b | 0x3f => (100, Some(DynamicGasKind::Unknown)), // warm account access
        0x3c => (100, Some(DynamicGasKind::Copy)),            // EXTCODECOPY
        0x30 | 0x32..=0x34 | 0x36 | 0x38 | 0x3a | 0x3d => (2, None),
        0x35 => (3, None),                                    // CALLDATALOAD
        0x37 | 0x39 | 0x3e => (3, Some(DynamicGasKind::Copy)),
        0x40 => (20, None),                                   // BLOCKHASH
        0x41..=0x46 | 0x48 | 0x4a => (2, None),
        0x47 => (5, None),                                    // SELFBALANCE
        0x49 => (3, None),                                    // BLOBHASH
        0x50 => (2, None),                                    // POP
        0x51..=0x53 => (3, Some(DynamicGasKind::MemoryExpansion)),
        0x54 | 0x55 => (100, Some(DynamicGasKind::StorageAccess)),
        0x56 => (8, None),                                    // JUMP
        0x57 => (10, None),                                   // JUMPI
        0x58 | 0x59 | 0x5a => (2, None),                      // PC, MSIZE, GAS
        0x5b => (1, None),                                    // JUMPDEST
        0x5c | 0x5d => (100, None),                           // TLOAD, TSTORE
        0x5e => (3, Some(DynamicGasKind::Copy)),              // MCOPY
        0x5f => (2, None),                                    // PUSH0
        0x60..=0x7f => (3, None),                             // PUSH1..32
        0x80..=0x9f => (3, None),                             // DUP, SWAP
        0xa0..=0xa4 => (
            375 * (1 + u64::from(opcode - 0xa0)),
            Some(DynamicGasKind::Unknown),
        ),
        0xf0 | 0xf5 => (32000, Some(DynamicGasKind::Unknown)), // CREATE, CREATE2
        0xf1 | 0xf2 | 0xf4 | 0xfa => (100, Some(DynamicGasKind::Call)),
        0xf3 | 0xfd => (0, Some(DynamicGasKind::MemoryExpansion)), // RETURN, REVERT
        0xff => (5000, Some(DynamicGasKind::Unknown)),        // SELFDESTRUCT
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use common::{InputDb, origin::OriginExportKey};
    use driver::DriverDataBase;
    use trace_facts::{
        CompilerEventKind, CompilerPhase, OriginNodeFact, OriginNodeKind, TraceFact, TraceValidator,
    };
    use url::Url;

    use crate::{
        BytecodePcRange, BytecodeSourceMapEntry,
        trace::{
            EVM_VCODE_INST_KIND, SONATINA_EVM_PREPARED_INST_KIND, SONATINA_POSTOPT_INST_KIND,
            build_pc_map, bytecode_code_object_key, bytecode_runtime_owner_key,
            emit_bytecode_instruction_facts,
            emit_bytecode_instruction_facts_with_observability, emit_codegen_facts,
            emit_sonatina_trace_view_facts, evm_vcode_inst_key,
            frontend_origin_record_for_export_key, pc_map_entry_for_pc,
            push_standalone_source_file_facts, sonatina_postopt_inst_key,
            standalone_source_file_facts, trace_source_file_key, whole_file_source_span,
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
            TraceFact::Opcode(opcode) if opcode.opcode.starts_with("PUSH")
        )));
        assert!(
            !facts
                .iter()
                .any(|fact| matches!(fact, TraceFact::GasCost(_))),
            "codegen emits StaticGasFact as the canonical static gas record"
        );
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::CodeObject(code_object)
                if code_object.code_object.kind() == "code.object"
                    && code_object
                        .code_hash
                        .as_deref()
                        .is_some_and(is_content_digest)
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
    fn bytecode_observability_links_prepared_instruction_to_postopt_lineage() {
        use sonatina_codegen::{
            machinst::vcode::VCodeInst,
            object::{OBSERVABILITY_SCHEMA_VERSION, PcMapEntry, SectionObservability},
        };
        use sonatina_ir::{
            BlockId, InstId, Linkage, Signature, builder::ModuleBuilder, isa::evm::Evm,
            module::ModuleCtx,
        };
        use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

        let evm = Evm::new(TargetTriple::new(
            Architecture::Evm,
            Vendor::Ethereum,
            OperatingSystem::Evm(EvmVersion::London),
        ));
        let mb = ModuleBuilder::new(ModuleCtx::new(&evm));
        let func = mb
            .declare_function(Signature::new_unit("runtime", Linkage::Public, &[]))
            .unwrap();

        let sonatina_owner = "package:fib:module:fib:sonatina";
        let postopt_inst_id = InstId(37);
        let postopt = sonatina_postopt_inst_key(sonatina_owner, func, postopt_inst_id);
        let vcode = evm_vcode_inst_key(sonatina_owner, func, VCodeInst(0));
        let known = [postopt.clone()]
            .into_iter()
            .collect::<std::collections::BTreeSet<_>>();
        let observability = SectionObservability {
            schema_version: OBSERVABILITY_SCHEMA_VERSION,
            section: "runtime".into(),
            section_bytes: 1,
            code_bytes: 1,
            data_bytes: 0,
            embed_bytes: 0,
            mapped_code_bytes: 1,
            unmapped_code_bytes: 0,
            unmapped_reason_coverage: Default::default(),
            pc_map: vec![PcMapEntry {
                pc_start: 0,
                pc_end: 1,
                func,
                func_name: "runtime".to_string(),
                block: BlockId(0),
                vcode_inst: VCodeInst(0),
                ir_inst: Some(postopt_inst_id),
                frontend_provenance: Some(
                    serde_json::to_string(&postopt)
                        .expect("OriginExportKey serialization cannot fail"),
                ),
                unmapped_reason: None,
            }],
        };

        let facts = emit_bytecode_instruction_facts_with_observability(
            "contract:Fib",
            "runtime",
            &[0x5f],
            Some(sonatina_owner),
            Some(&observability),
            Some(&known),
        );
        let mut validated_facts = vec![TraceFact::OriginNode(OriginNodeFact::new(
            postopt.clone(),
            OriginNodeKind::new(SONATINA_POSTOPT_INST_KIND),
        ))];
        validated_facts.extend(facts.clone());
        TraceValidator::validate(&validated_facts).unwrap();

        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginNode(node) if node.key.kind() == EVM_VCODE_INST_KIND
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == "bytecode.pc"
                    && edge.to == vcode
                    && edge.label == trace_facts::OriginEdgeLabel::EmittedFrom
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from == vcode
                    && edge.to.kind() == SONATINA_EVM_PREPARED_INST_KIND
                    && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == SONATINA_EVM_PREPARED_INST_KIND
                    && edge.to == postopt
                    && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
                    && edge.introduced_by == Some(trace_facts::CompilerPhase::Backend)
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::CompilerEvent(event)
                if event.kind == CompilerEventKind::PreparedLineage
                    && event.phase == trace_facts::CompilerPhase::Backend
                    && event.inputs == vec![postopt.clone()]
                    && event.outputs.iter().any(|output| output.kind() == SONATINA_EVM_PREPARED_INST_KIND)
        )));
        assert!(!facts.iter().any(|fact| {
            matches!(
                fact,
                TraceFact::OriginEdge(edge)
                    if edge.from.kind() == "bytecode.pc"
                        && edge.to.kind() == SONATINA_POSTOPT_INST_KIND
            )
        }));
    }

    #[test]
    fn build_pc_map_skips_zero_length_and_keeps_widest_on_duplicate_start() {
        use sonatina_codegen::{machinst::vcode::VCodeInst, object::PcMapEntry};
        use sonatina_ir::{
            BlockId, Linkage, Signature, builder::ModuleBuilder, isa::evm::Evm, module::ModuleCtx,
        };
        use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

        let evm = Evm::new(TargetTriple::new(
            Architecture::Evm,
            Vendor::Ethereum,
            OperatingSystem::Evm(EvmVersion::London),
        ));
        let mb = ModuleBuilder::new(ModuleCtx::new(&evm));
        let func = mb
            .declare_function(Signature::new_unit("runtime", Linkage::Public, &[]))
            .unwrap();
        let entry = |pc_start: u32, pc_end: u32| PcMapEntry {
            pc_start,
            pc_end,
            func,
            func_name: "runtime".to_string(),
            block: BlockId(0),
            vcode_inst: VCodeInst(0),
            ir_inst: None,
            frontend_provenance: None,
            unmapped_reason: None,
        };

        // A zero-length entry sharing a real entry's start must not shadow it,
        // regardless of order (a naive last-wins collect would drop the link).
        let entries = vec![entry(0, 4), entry(0, 0)];
        let map = build_pc_map(&entries);
        assert_eq!(pc_map_entry_for_pc(&map, 0).map(|e| e.pc_end), Some(4));
        assert_eq!(pc_map_entry_for_pc(&map, 3).map(|e| e.pc_end), Some(4));

        // On a duplicate start the wider range wins, in either order.
        for entries in [vec![entry(0, 2), entry(0, 6)], vec![entry(0, 6), entry(0, 2)]] {
            let map = build_pc_map(&entries);
            assert_eq!(pc_map_entry_for_pc(&map, 4).map(|e| e.pc_end), Some(6));
        }

        // Disjoint ranges still resolve, with pc == pc_end excluded (half-open).
        let entries = vec![entry(0, 4), entry(4, 8)];
        let map = build_pc_map(&entries);
        assert_eq!(pc_map_entry_for_pc(&map, 3).map(|e| e.pc_end), Some(4));
        assert_eq!(pc_map_entry_for_pc(&map, 4).map(|e| e.pc_end), Some(8));
        assert_eq!(pc_map_entry_for_pc(&map, 8), None);
    }

    #[test]
    fn bytecode_observability_applies_pc_map_ranges_to_each_instruction_start() {
        use sonatina_codegen::{
            machinst::vcode::VCodeInst,
            object::{OBSERVABILITY_SCHEMA_VERSION, PcMapEntry, SectionObservability},
        };
        use sonatina_ir::{
            BlockId, InstId, Linkage, Signature, builder::ModuleBuilder, isa::evm::Evm,
            module::ModuleCtx,
        };
        use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

        let evm = Evm::new(TargetTriple::new(
            Architecture::Evm,
            Vendor::Ethereum,
            OperatingSystem::Evm(EvmVersion::London),
        ));
        let mb = ModuleBuilder::new(ModuleCtx::new(&evm));
        let func = mb
            .declare_function(Signature::new_unit("runtime", Linkage::Public, &[]))
            .unwrap();

        let sonatina_owner = "package:fib:module:fib:sonatina";
        let vcode = evm_vcode_inst_key(sonatina_owner, func, VCodeInst(0));
        let prepared = super::sonatina_trace_inst_key(
            SONATINA_EVM_PREPARED_INST_KIND,
            sonatina_owner,
            func,
            InstId(37),
        );
        let pc0 =
            OriginExportKey::try_from_raw_parts("bytecode.pc", "contract:Fib", "pc:0").unwrap();
        let pc1 =
            OriginExportKey::try_from_raw_parts("bytecode.pc", "contract:Fib", "pc:1").unwrap();
        let observability = SectionObservability {
            schema_version: OBSERVABILITY_SCHEMA_VERSION,
            section: "runtime".into(),
            section_bytes: 2,
            code_bytes: 2,
            data_bytes: 0,
            embed_bytes: 0,
            mapped_code_bytes: 2,
            unmapped_code_bytes: 0,
            unmapped_reason_coverage: Default::default(),
            pc_map: vec![PcMapEntry {
                pc_start: 0,
                pc_end: 2,
                func,
                func_name: "runtime".to_string(),
                block: BlockId(0),
                vcode_inst: VCodeInst(0),
                ir_inst: Some(InstId(37)),
                frontend_provenance: None,
                unmapped_reason: None,
            }],
        };

        let facts = emit_bytecode_instruction_facts_with_observability(
            "contract:Fib",
            "runtime",
            &[0x5f, 0x01],
            Some(sonatina_owner),
            Some(&observability),
            None,
        );
        TraceValidator::validate(&facts).unwrap();

        for pc in [&pc0, &pc1] {
            assert!(
                facts.iter().any(|fact| matches!(
                    fact,
                    TraceFact::OriginEdge(edge)
                        if edge.from == pc.clone()
                            && edge.to == vcode
                            && edge.label == trace_facts::OriginEdgeLabel::EmittedFrom
                            && edge.introduced_by == Some(trace_facts::CompilerPhase::BytecodeEmission)
                )),
                "bytecode PC {} should inherit the containing PC-map range",
                pc.display_label()
            );
        }
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from == vcode
                    && edge.to == prepared
                    && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
                    && edge.introduced_by == Some(trace_facts::CompilerPhase::Backend)
        )));
    }

    #[test]
    fn bytecode_observability_does_not_invent_postopt_lineage_without_known_trace_nodes() {
        use sonatina_codegen::{
            machinst::vcode::VCodeInst,
            object::{OBSERVABILITY_SCHEMA_VERSION, PcMapEntry, SectionObservability},
        };
        use sonatina_ir::{
            BlockId, InstId, Linkage, Signature, builder::ModuleBuilder, isa::evm::Evm,
            module::ModuleCtx,
        };
        use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

        let evm = Evm::new(TargetTriple::new(
            Architecture::Evm,
            Vendor::Ethereum,
            OperatingSystem::Evm(EvmVersion::London),
        ));
        let mb = ModuleBuilder::new(ModuleCtx::new(&evm));
        let func = mb
            .declare_function(Signature::new_unit("runtime", Linkage::Public, &[]))
            .unwrap();

        let sonatina_owner = "package:fib:module:fib:sonatina";
        let postopt = sonatina_postopt_inst_key(sonatina_owner, func, InstId(37));
        let vcode = evm_vcode_inst_key(sonatina_owner, func, VCodeInst(0));
        let observability = SectionObservability {
            schema_version: OBSERVABILITY_SCHEMA_VERSION,
            section: "runtime".into(),
            section_bytes: 1,
            code_bytes: 1,
            data_bytes: 0,
            embed_bytes: 0,
            mapped_code_bytes: 1,
            unmapped_code_bytes: 0,
            unmapped_reason_coverage: Default::default(),
            pc_map: vec![PcMapEntry {
                pc_start: 0,
                pc_end: 1,
                func,
                func_name: "runtime".to_string(),
                block: BlockId(0),
                vcode_inst: VCodeInst(0),
                ir_inst: Some(InstId(37)),
                frontend_provenance: Some(
                    serde_json::to_string(&postopt)
                        .expect("OriginExportKey serialization cannot fail"),
                ),
                unmapped_reason: None,
            }],
        };

        let facts = emit_bytecode_instruction_facts_with_observability(
            "contract:Fib",
            "runtime",
            &[0x5f],
            Some(sonatina_owner),
            Some(&observability),
            None,
        );
        TraceValidator::validate(&facts).unwrap();

        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == "bytecode.pc"
                    && edge.to == vcode
                    && edge.label == trace_facts::OriginEdgeLabel::EmittedFrom
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from == vcode
                    && edge.to.kind() == SONATINA_EVM_PREPARED_INST_KIND
                    && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
        )));
        assert!(!facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginNode(node) if node.key == postopt
        )));
        assert!(!facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == SONATINA_EVM_PREPARED_INST_KIND
                    && edge.to == postopt
        )));
        assert!(!facts.iter().any(|fact| matches!(
            fact,
            TraceFact::CompilerEvent(event)
                if event.kind == CompilerEventKind::PreparedLineage
        )));
    }

    #[test]
    fn bytecode_observability_refuses_index_keyed_provenance() {
        use sonatina_codegen::{
            machinst::vcode::VCodeInst,
            object::{OBSERVABILITY_SCHEMA_VERSION, PcMapEntry, SectionObservability},
        };
        use sonatina_ir::{
            BlockId, InstId, Linkage, Signature, builder::ModuleBuilder, isa::evm::Evm,
            module::ModuleCtx,
        };
        use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

        let evm = Evm::new(TargetTriple::new(
            Architecture::Evm,
            Vendor::Ethereum,
            OperatingSystem::Evm(EvmVersion::London),
        ));
        let mb = ModuleBuilder::new(ModuleCtx::new(&evm));
        let func = mb
            .declare_function(Signature::new_unit("runtime", Linkage::Public, &[]))
            .unwrap();

        let sonatina_owner = "package:fib:module:fib:sonatina";
        let actual_postopt = sonatina_postopt_inst_key(sonatina_owner, func, InstId(37));
        let alias_postopt = sonatina_postopt_inst_key(sonatina_owner, func, InstId(0));
        let postopt_function = OriginExportKey::try_from_raw_parts(
            super::SONATINA_POSTOPT_FUNCTION_KIND,
            sonatina_owner,
            format!("function:{func:?}"),
        )
        .unwrap();
        let _postopt_facts = vec![TraceFact::Instruction(trace_facts::InstructionFact::new(
            actual_postopt.clone(),
            postopt_function,
            0,
            "iadd",
        ))];
        let known = [actual_postopt.clone()]
            .into_iter()
            .collect::<std::collections::BTreeSet<_>>();
        let observability = SectionObservability {
            schema_version: OBSERVABILITY_SCHEMA_VERSION,
            section: "runtime".into(),
            section_bytes: 1,
            code_bytes: 1,
            data_bytes: 0,
            embed_bytes: 0,
            mapped_code_bytes: 1,
            unmapped_code_bytes: 0,
            unmapped_reason_coverage: Default::default(),
            pc_map: vec![PcMapEntry {
                pc_start: 0,
                pc_end: 1,
                func,
                func_name: "runtime".to_string(),
                block: BlockId(0),
                vcode_inst: VCodeInst(0),
                ir_inst: Some(InstId(37)),
                frontend_provenance: Some(
                    serde_json::to_string(&alias_postopt)
                        .expect("OriginExportKey serialization cannot fail"),
                ),
                unmapped_reason: None,
            }],
        };

        let facts = emit_bytecode_instruction_facts_with_observability(
            "contract:Fib",
            "runtime",
            &[0x5f],
            Some(sonatina_owner),
            Some(&observability),
            Some(&known),
        );

        // Provenance keyed by emission index instead of a real InstId does not
        // name a known postopt node; it must be refused outright, never
        // remapped onto a different instruction.
        assert!(!facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == SONATINA_EVM_PREPARED_INST_KIND
                    && (edge.to == alias_postopt || edge.to == actual_postopt)
        )));
        assert!(!facts.iter().any(|fact| matches!(
            fact,
            TraceFact::CompilerEvent(event)
                if event.kind == CompilerEventKind::PreparedLineage
        )));
    }

    #[test]
    fn bytecode_observability_does_not_create_unknown_postopt_lineage() {
        use sonatina_codegen::{
            machinst::vcode::VCodeInst,
            object::{OBSERVABILITY_SCHEMA_VERSION, PcMapEntry, SectionObservability},
        };
        use sonatina_ir::{
            BlockId, InstId, Linkage, Signature, builder::ModuleBuilder, isa::evm::Evm,
            module::ModuleCtx,
        };
        use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

        let evm = Evm::new(TargetTriple::new(
            Architecture::Evm,
            Vendor::Ethereum,
            OperatingSystem::Evm(EvmVersion::London),
        ));
        let mb = ModuleBuilder::new(ModuleCtx::new(&evm));
        let func = mb
            .declare_function(Signature::new_unit("runtime", Linkage::Public, &[]))
            .unwrap();

        let sonatina_owner = "package:fib:module:fib:sonatina";
        let unknown_postopt = sonatina_postopt_inst_key(sonatina_owner, func, InstId(37));
        let vcode = evm_vcode_inst_key(sonatina_owner, func, VCodeInst(0));
        let known_postopt_nodes = std::collections::BTreeSet::new();
        let observability = SectionObservability {
            schema_version: OBSERVABILITY_SCHEMA_VERSION,
            section: "runtime".into(),
            section_bytes: 1,
            code_bytes: 1,
            data_bytes: 0,
            embed_bytes: 0,
            mapped_code_bytes: 1,
            unmapped_code_bytes: 0,
            unmapped_reason_coverage: Default::default(),
            pc_map: vec![PcMapEntry {
                pc_start: 0,
                pc_end: 1,
                func,
                func_name: "runtime".to_string(),
                block: BlockId(0),
                vcode_inst: VCodeInst(0),
                ir_inst: Some(InstId(37)),
                frontend_provenance: Some(
                    serde_json::to_string(&unknown_postopt)
                        .expect("OriginExportKey serialization cannot fail"),
                ),
                unmapped_reason: None,
            }],
        };

        let facts = emit_bytecode_instruction_facts_with_observability(
            "contract:Fib",
            "runtime",
            &[0x5f],
            Some(sonatina_owner),
            Some(&observability),
            Some(&known_postopt_nodes),
        );
        TraceValidator::validate(&facts).unwrap();

        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == "bytecode.pc"
                    && edge.to == vcode
                    && edge.label == trace_facts::OriginEdgeLabel::EmittedFrom
        )));
        assert!(facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from == vcode
                    && edge.to.kind() == SONATINA_EVM_PREPARED_INST_KIND
                    && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
        )));
        assert!(!facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginNode(node) if node.key == unknown_postopt
        )));
        assert!(!facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == SONATINA_EVM_PREPARED_INST_KIND
                    && edge.to == unknown_postopt
        )));
        assert!(!facts.iter().any(|fact| matches!(
            fact,
            TraceFact::CompilerEvent(event)
                if event.kind == CompilerEventKind::PreparedLineage
        )));
    }

    #[test]
    fn bytecode_frontend_provenance_fallback_is_contextual_not_exact() {
        use sonatina_codegen::{
            machinst::vcode::VCodeInst,
            object::{OBSERVABILITY_SCHEMA_VERSION, PcMapEntry, SectionObservability},
        };
        use sonatina_ir::{
            BlockId, InstId, Linkage, Signature, builder::ModuleBuilder, isa::evm::Evm,
            module::ModuleCtx,
        };
        use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

        let evm = Evm::new(TargetTriple::new(
            Architecture::Evm,
            Vendor::Ethereum,
            OperatingSystem::Evm(EvmVersion::London),
        ));
        let mb = ModuleBuilder::new(ModuleCtx::new(&evm));
        let func = mb
            .declare_function(Signature::new_unit("runtime", Linkage::Public, &[]))
            .unwrap();

        let sonatina_owner = "package:fib:module:fib:sonatina";
        let frontend_origin =
            OriginExportKey::try_from_raw_parts("hir.expr", "package:fib:module:fib", "expr:0")
                .unwrap();
        let observability = SectionObservability {
            schema_version: OBSERVABILITY_SCHEMA_VERSION,
            section: "runtime".into(),
            section_bytes: 1,
            code_bytes: 1,
            data_bytes: 0,
            embed_bytes: 0,
            mapped_code_bytes: 1,
            unmapped_code_bytes: 0,
            unmapped_reason_coverage: Default::default(),
            pc_map: vec![PcMapEntry {
                pc_start: 0,
                pc_end: 1,
                func,
                func_name: "runtime".to_string(),
                block: BlockId(0),
                vcode_inst: VCodeInst(0),
                ir_inst: Some(InstId(37)),
                frontend_provenance: Some(
                    serde_json::to_string(&frontend_origin)
                        .expect("OriginExportKey serialization cannot fail"),
                ),
                unmapped_reason: None,
            }],
        };

        let facts = emit_bytecode_instruction_facts_with_observability(
            "contract:Fib",
            "runtime",
            &[0x5f],
            Some(sonatina_owner),
            Some(&observability),
            None,
        );
        let mut validated_facts = vec![TraceFact::OriginNode(OriginNodeFact::new(
            frontend_origin.clone(),
            OriginNodeKind::new("hir.expr"),
        ))];
        validated_facts.extend(facts.clone());
        TraceValidator::validate(&validated_facts).unwrap();

        let frontend_edges = facts
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::OriginEdge(edge)
                    if edge.from.kind() == "bytecode.pc" && edge.to == frontend_origin =>
                {
                    Some(edge)
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(frontend_edges.len(), 1);
        assert_eq!(
            frontend_edges[0].label,
            trace_facts::OriginEdgeLabel::BackendPrepared
        );
        assert_eq!(
            frontend_edges[0].traversal_class(),
            trace_facts::OriginEdgeTraversalClass::Contextual
        );
        assert!(!facts.iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == "bytecode.pc"
                    && edge.to == frontend_origin
                    && edge.has_transform_claim_label()
        )));
        assert!(!facts.iter().any(|fact| matches!(
            fact,
            TraceFact::CompilerEvent(event)
                if event.kind == CompilerEventKind::PreparedLineage
        )));
    }

    fn is_content_digest(value: &str) -> bool {
        let digest = value.strip_prefix("blake3:").unwrap_or(value);
        digest.len() == 64
            && digest.chars().all(|ch| ch.is_ascii_hexdigit())
            && !value.starts_with("fnv64:")
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
    fn sonatina_trace_view_adapter_emits_natural_loop_membership() {
        use sonatina_ir::{
            Linkage, Signature, Type,
            builder::ModuleBuilder,
            func_cursor::InstInserter,
            inst::{
                arith::Add,
                control_flow::{Br, Jump, Return},
            },
            isa::Isa,
            isa::evm::Evm,
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
            .declare_function(Signature::new_unit(
                "trace_loop",
                Linkage::Public,
                &[Type::I1],
            ))
            .unwrap();
        let is = evm.inst_set();
        let mut builder = mb.func_builder::<InstInserter>(func_ref);
        let entry = builder.append_block();
        let header = builder.append_block();
        let body = builder.append_block();
        let exit = builder.append_block();
        let cond = builder.args()[0];

        builder.switch_to_block(entry);
        builder.insert_inst_no_result(Jump::new(is, header));

        builder.switch_to_block(header);
        builder.insert_inst_no_result(Br::new(is, cond, body, exit));

        builder.switch_to_block(body);
        let lhs = builder.make_imm_value(1i32);
        let rhs = builder.make_imm_value(2i32);
        builder.insert_inst(Add::new(is, lhs, rhs), Type::I32);
        builder.insert_inst_no_result(Jump::new(is, header));

        builder.switch_to_block(exit);
        builder.insert_inst_no_result(Return::new_unit(is));
        builder.seal_all();
        builder.finish();
        let module = mb.build();

        let facts =
            emit_sonatina_trace_view_facts("owner:test", &module, CompilerPhase::SonatinaPreOpt);
        TraceValidator::validate(&facts).unwrap();

        assert!(
            facts
                .iter()
                .any(|fact| matches!(fact, TraceFact::Loop(loop_fact)
                    if loop_fact.confidence == trace_facts::LoopConfidence::SonatinaCfg))
        );
        assert!(facts.iter().any(|fact| {
            matches!(fact, TraceFact::LoopMembership(membership)
                    if membership.instruction.kind() == super::SONATINA_PREOPT_INST_KIND)
        }));
        assert!(facts.iter().any(|fact| {
            matches!(fact, TraceFact::CfgEdge(edge) if edge.kind == trace_facts::CfgEdgeKind::Backedge)
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
    fn source_context_helpers_emit_stable_file_and_whole_file_span() {
        let source_file = trace_source_file_key("file:///workspace/main.fe");
        let mut facts = standalone_source_file_facts(
            &source_file,
            "file:///workspace/main.fe",
            "main.fe",
            "fn main() {}\n",
            Some(0),
        );
        push_standalone_source_file_facts(
            &mut facts,
            &source_file,
            "file:///workspace/main.fe",
            "main.fe",
            "fn main() {}\n",
            Some(0),
        );
        let code_object = bytecode_code_object_key("package:file:///workspace/main.fe:runtime");
        let span = whole_file_source_span(code_object.clone(), source_file.clone(), "a\nbc")
            .expect("non-empty source has a whole-file span");

        assert_eq!(
            facts
                .iter()
                .filter(
                    |fact| matches!(fact, TraceFact::OriginNode(node) if node.key == source_file)
                )
                .count(),
            1
        );
        assert_eq!(
            facts
                .iter()
                .filter(|fact| matches!(fact, TraceFact::SourceFile(source) if source.file_key == source_file))
                .count(),
            1
        );
        assert_eq!(span.origin, code_object);
        assert_eq!(span.file, source_file);
        assert_eq!(span.start_line, 1);
        assert_eq!(span.end_line, 2);
        assert_eq!(span.end_column, 3);
        assert!(whole_file_source_span(code_object, span.file, "").is_none());
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
        #[allow(deprecated)]
        {
            facts.extend(super::emit_sonatina_cfg_facts(&db, package));
        }

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
