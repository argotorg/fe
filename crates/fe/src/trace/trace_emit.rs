use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::{BufReader, BufWriter, Cursor};
use std::sync::{Arc, Mutex};

use camino::Utf8PathBuf;
use common::{InputDb, SalsaEventCounters, origin::OriginExportKey};
use contract_harness::{CompileOptions, ExecutionOptions, FeContractHarness, RuntimeTraceConfig};
use driver::{
    DriverDataBase,
    cli_target::{CliTarget, resolve_cli_target},
};
use salsa::Setter;
use trace_facts::{
    CodeObjectFact, CodeObjectKind, CompilerPhase, InstructionExtentFact, JsonlTraceReader,
    JsonlTraceSink, OriginNodeFact, OriginNodeKind, SourceFileFact, SourceSpanFact, TraceBundle,
    TraceFact, TraceSnapshot, TraceValidator,
};
use url::Url;

use crate::{
    DevTraceAttributionArgs, DevTraceDynamicGasArgs, DevTraceEmitArgs, DevTraceExplainLocalArgs,
    DevTraceGasArgs, DevTraceGasToSourceArgs, DevTraceInputArgs, DevTracePcArgs, DevTraceRunArgs,
    DevTraceRuntimeArgs, DevTraceRuntimeAttributionArgs, DevTraceRuntimePcArgs,
    DevTraceStorageSlotArgs,
};

pub(super) fn run_trace_emit(args: &DevTraceEmitArgs) -> Result<String, String> {
    let opt_level = args.optimize.parse::<codegen::OptLevel>()?;
    let bundle = emit_real_trace_bundle(&args.path, args.standalone, &args.profile, opt_level)?;
    let summary = TraceValidator::validate(&bundle.facts)
        .map_err(|err| format!("compiler trace emission produced invalid facts: {err}"))?;
    write_trace_bundle_jsonl(&args.out, &bundle)?;
    Ok(format!(
        "wrote compiler trace JSONL: {}\nData source: {}\nFacts: {}\nOrigin nodes: {}\nInstructions: {}\n",
        args.out,
        super::format_data_source(&bundle.metadata),
        summary.fact_count,
        summary.node_count,
        summary.instruction_count
    ))
}

pub(super) fn run_trace_run(args: &DevTraceRunArgs) -> Result<String, String> {
    let static_bundle = read_trace_bundle_jsonl_from_path(&args.static_trace)?;
    let static_snapshot = TraceSnapshot::new(static_bundle.clone())
        .map_err(|err| format!("static trace validation failed: {err}"))?;
    let contract = contract_from_entry(&args.entry)?;
    let source_path = args
        .source
        .clone()
        .unwrap_or_else(|| Utf8PathBuf::from(static_snapshot.metadata().input_path.clone()));
    let code_object = select_runtime_code_object(static_snapshot.facts(), contract)?;
    let instruction_by_pc = runtime_instruction_by_pc(static_snapshot.facts(), code_object)?;
    let opt_level = opt_level_from_metadata(static_snapshot.metadata())?;
    let harness = FeContractHarness::compile_from_file(
        contract,
        source_path.as_std_path(),
        CompileOptions { opt_level },
    )
    .map_err(|err| format!("failed to compile runtime target {contract}: {err}"))?;
    let runtime_hash = runtime_code_hash(harness.runtime_bytecode())?;
    let static_hash = code_object.code_hash.clone().ok_or_else(|| {
        format!(
            "static code object {} has no code hash",
            code_object.code_object.canonical_storage_key()
        )
    })?;
    if static_hash != runtime_hash {
        return Err(format!(
            "runtime bytecode hash mismatch for {contract}: static trace has {static_hash}, execution compile produced {runtime_hash}"
        ));
    }

    let calldata = encode_selector_call(&args.selector, &args.args)?;
    let mut check_instance = harness
        .deploy_with_init()
        .map_err(|err| format!("failed to deploy {contract}: {err}"))?;
    let call_result = check_instance
        .call_raw(&calldata, ExecutionOptions::default())
        .map_err(|err| format!("runtime execution failed for {contract}: {err}"))?;

    let trace_instance = harness
        .deploy_with_init()
        .map_err(|err| format!("failed to deploy trace instance for {contract}: {err}"))?;
    let session = runtime_session_key(&code_object.code_object, &args.entry, &args.args)?;
    let runtime_facts = trace_instance
        .call_raw_runtime_trace(
            &calldata,
            ExecutionOptions::default(),
            RuntimeTraceConfig::new(
                session,
                code_object.code_object.clone(),
                runtime_hash.clone(),
            )
            .with_address(trace_instance.address())
            .with_capture_mode(args.runtime_capture.into())
            .with_value_policy(args.value_policy.into())
            .with_instruction_by_pc(instruction_by_pc),
        )
        .map_err(|err| format!("runtime trace capture failed for {contract}: {err}"))?;

    let mut metadata = static_bundle.metadata;
    metadata.command = vec![
        "fe".to_string(),
        "dev".to_string(),
        "trace".to_string(),
        "run".to_string(),
    ];
    metadata.flags.push(format!("static={}", args.static_trace));
    metadata.flags.push(format!("entry={}", args.entry));
    metadata.flags.push(format!("selector={}", args.selector));
    metadata.flags.push(format!(
        "runtime_capture={:?}",
        trace_facts::RuntimeCaptureMode::from(args.runtime_capture)
    ));
    metadata.flags.push(format!(
        "value_policy={:?}",
        trace_facts::RuntimeValuePolicy::from(args.value_policy)
    ));
    metadata.flags.push(format!(
        "runtime_source={:?}",
        trace_facts::RuntimeTraceDataSource::RevmInspector
    ));

    let mut facts = static_bundle.facts;
    let runtime_fact_count = runtime_facts.len();
    facts.extend(runtime_facts);
    let combined = TraceBundle::new(metadata, facts);
    let summary = TraceValidator::validate(&combined.facts)
        .map_err(|err| format!("combined runtime trace is invalid: {err}"))?;
    write_trace_bundle_jsonl(&args.out, &combined)?;
    Ok(format!(
        "wrote combined runtime trace JSONL: {}\n\
         Data source: {}\n\
         Runtime source: revm_inspector\n\
         Capture mode: {:?}\n\
         Value policy: {:?}\n\
         Runtime facts appended: {}\n\
         Runtime return bytes: {}\n\
         Facts: {}\n\
         Instructions: {}\n",
        args.out,
        super::format_data_source(&combined.metadata),
        trace_facts::RuntimeCaptureMode::from(args.runtime_capture),
        trace_facts::RuntimeValuePolicy::from(args.value_policy),
        runtime_fact_count,
        call_result.return_data.len(),
        summary.fact_count,
        summary.instruction_count,
    ))
}

pub(super) fn run_trace_validate(args: &DevTraceInputArgs) -> Result<String, String> {
    let snapshot = read_trace_snapshot_jsonl_from_path(&args.from)?;
    super::trace_render::render_validation_summary_with_format(
        snapshot.metadata(),
        snapshot.validation(),
        args.format,
    )
}

pub(super) fn run_trace_loop_cost(args: &DevTraceInputArgs) -> Result<String, String> {
    super::trace_render::render_loop_cost_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.format,
    )
}

pub(super) fn run_trace_loop_contents(args: &DevTraceInputArgs) -> Result<String, String> {
    super::trace_render::render_loop_contents_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.format,
    )
}

pub(super) fn run_trace_explain_local(args: &DevTraceExplainLocalArgs) -> Result<String, String> {
    super::trace_render::render_explain_local_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        &args.local,
        args.local_key.as_deref(),
        args.format,
    )
}

pub(super) fn run_trace_gas_breakdown(args: &DevTraceGasArgs) -> Result<String, String> {
    super::trace_render::render_gas_breakdown_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        &args.schedule,
        args.format,
    )
}

pub(super) fn run_trace_explain_pc(args: &DevTracePcArgs) -> Result<String, String> {
    super::trace_render::render_explain_pc_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.pc,
        args.format,
    )
}

pub(super) fn run_trace_gas_by_source(args: &DevTraceGasArgs) -> Result<String, String> {
    super::trace_render::render_gas_by_source_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        &args.schedule,
        &args.policy,
        args.format,
    )
}

pub(super) fn run_trace_bytecode_size_by_source(
    args: &DevTraceAttributionArgs,
) -> Result<String, String> {
    super::trace_render::render_bytecode_size_by_source_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        &args.policy,
        args.format,
    )
}

pub(super) fn run_trace_dynamic_gas_by_source(
    args: &DevTraceDynamicGasArgs,
) -> Result<String, String> {
    super::trace_render::render_dynamic_gas_by_source_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.trace_id.clone(),
        &args.policy,
        args.format,
    )
}

pub(super) fn run_trace_gas_to_source(args: &DevTraceGasToSourceArgs) -> Result<String, String> {
    super::trace_render::render_gas_to_source_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        &args.schedule,
        args.trace_id.clone(),
        &args.policy,
        args.format,
    )
}

pub(super) fn run_trace_optimized_code_honesty(args: &DevTraceInputArgs) -> Result<String, String> {
    super::trace_render::render_optimized_code_honesty_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.format,
    )
}

pub(super) fn run_trace_static_analysis(args: &DevTraceInputArgs) -> Result<String, String> {
    super::trace_render::render_static_analysis_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.format,
    )
}

pub(super) fn run_trace_variables_at_pc(args: &DevTracePcArgs) -> Result<String, String> {
    super::trace_render::render_variables_at_pc_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.pc,
        args.format,
    )
}

pub(super) fn run_trace_runtime_gas_by_source(
    args: &DevTraceRuntimeAttributionArgs,
) -> Result<String, String> {
    super::trace_render::render_runtime_gas_by_source_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.trace_id.clone(),
        &args.policy,
        args.format,
    )
}

pub(super) fn run_trace_storage_writes_by_source(
    args: &DevTraceRuntimeAttributionArgs,
) -> Result<String, String> {
    super::trace_render::render_storage_writes_by_source_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.trace_id.clone(),
        &args.policy,
        args.format,
    )
}

pub(super) fn run_trace_storage_accesses_by_slot(
    args: &DevTraceStorageSlotArgs,
) -> Result<String, String> {
    super::trace_render::render_storage_accesses_by_slot_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.trace_id.clone(),
        args.slot.clone(),
        &args.policy,
        args.format,
    )
}

pub(super) fn run_trace_call_cost_by_callsite(
    args: &DevTraceRuntimeArgs,
) -> Result<String, String> {
    super::trace_render::render_call_cost_by_callsite_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.trace_id.clone(),
        args.format,
    )
}

pub(super) fn run_trace_memory_growth_by_source(
    args: &DevTraceRuntimeAttributionArgs,
) -> Result<String, String> {
    super::trace_render::render_memory_growth_by_source_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.trace_id.clone(),
        &args.policy,
        args.format,
    )
}

pub(super) fn run_trace_revert_attribution(args: &DevTraceRuntimeArgs) -> Result<String, String> {
    super::trace_render::render_revert_attribution_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.trace_id.clone(),
        args.format,
    )
}

pub(super) fn run_trace_hot_path_by_iteration(
    args: &DevTraceRuntimeAttributionArgs,
) -> Result<String, String> {
    super::trace_render::render_hot_path_by_iteration_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.trace_id.clone(),
        &args.policy,
        args.format,
    )
}

pub(super) fn run_trace_value_flow_at_pc(args: &DevTraceRuntimePcArgs) -> Result<String, String> {
    super::trace_render::render_value_flow_at_pc_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.pc,
        args.trace_id.clone(),
        args.format,
    )
}

fn contract_from_entry(entry: &str) -> Result<&str, String> {
    let contract = entry
        .split_once("::")
        .map_or(entry, |(contract, _)| contract);
    if contract.trim().is_empty() {
        Err("trace run --entry must include a contract name".to_string())
    } else {
        Ok(contract)
    }
}

fn opt_level_from_metadata(
    metadata: &trace_facts::TraceMetadata,
) -> Result<codegen::OptLevel, String> {
    metadata
        .flags
        .iter()
        .find_map(|flag| flag.strip_prefix("optimize="))
        .unwrap_or("1")
        .parse::<codegen::OptLevel>()
        .map_err(|err| format!("failed to parse static trace optimize flag: {err}"))
}

fn select_runtime_code_object<'a>(
    facts: &'a [TraceFact],
    contract: &str,
) -> Result<&'a CodeObjectFact, String> {
    let needle = format!("contract:{contract}:section:runtime");
    let matches = facts
        .iter()
        .filter_map(|fact| match fact {
            TraceFact::CodeObject(code_object)
                if code_object.kind == CodeObjectKind::EvmRuntimeBytecode
                    && code_object.code_object.owner_key().contains(&needle) =>
            {
                Some(code_object)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    match matches.as_slice() {
        [code_object] => Ok(code_object),
        [] => Err(format!(
            "static trace does not contain an EVM runtime code object for contract {contract}"
        )),
        _ => Err(format!(
            "static trace contains multiple runtime code objects for contract {contract}; refine --entry"
        )),
    }
}

fn runtime_instruction_by_pc(
    facts: &[TraceFact],
    code_object: &CodeObjectFact,
) -> Result<BTreeMap<u32, OriginExportKey>, String> {
    let mut by_pc = BTreeMap::new();
    for extent in facts.iter().filter_map(|fact| match fact {
        TraceFact::InstructionExtent(extent) if extent.code_object == code_object.code_object => {
            Some(extent)
        }
        _ => None,
    }) {
        insert_extent_pc(&mut by_pc, extent)?;
    }
    if by_pc.is_empty() {
        return Err(format!(
            "static trace has no instruction extents for runtime code object {}",
            code_object.code_object.canonical_storage_key()
        ));
    }
    Ok(by_pc)
}

fn insert_extent_pc(
    by_pc: &mut BTreeMap<u32, OriginExportKey>,
    extent: &InstructionExtentFact,
) -> Result<(), String> {
    if by_pc
        .insert(extent.pc_range.start, extent.instruction.clone())
        .is_some()
    {
        return Err(format!(
            "duplicate instruction extent start pc {} for {}",
            extent.pc_range.start,
            extent.code_object.canonical_storage_key()
        ));
    }
    Ok(())
}

fn runtime_code_hash(runtime_bytecode_hex: &str) -> Result<String, String> {
    let bytes = hex::decode(runtime_bytecode_hex)
        .map_err(|err| format!("failed to decode runtime bytecode hex: {err}"))?;
    Ok(format!("blake3:{}", blake3::hash(&bytes).to_hex()))
}

fn runtime_session_key(
    code_object: &OriginExportKey,
    entry: &str,
    args: &[String],
) -> Result<OriginExportKey, String> {
    let arg_digest = blake3::hash(args.join(",").as_bytes()).to_hex().to_string();
    OriginExportKey::try_from_raw_parts(
        "runtime.session",
        code_object.owner_key(),
        format!("entry:{entry}:args:{arg_digest}"),
    )
    .map_err(|err| format!("invalid runtime session key: {err}"))
}

fn encode_selector_call(selector: &str, args: &[String]) -> Result<Vec<u8>, String> {
    let mut calldata = parse_selector(selector)?;
    for arg in args {
        calldata.extend_from_slice(&encode_u256_word(arg)?);
    }
    Ok(calldata)
}

fn parse_selector(selector: &str) -> Result<Vec<u8>, String> {
    let trimmed = selector
        .trim()
        .strip_prefix("0x")
        .unwrap_or(selector.trim());
    if trimmed.len() > 8 {
        return Err(format!("selector {selector} is wider than 4 bytes"));
    }
    let padded = format!("{trimmed:0>8}");
    hex::decode(&padded).map_err(|err| format!("invalid selector {selector}: {err}"))
}

fn encode_u256_word(value: &str) -> Result<[u8; 32], String> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Err("empty runtime argument".to_string());
    }
    let parsed = if let Some(hex_value) = trimmed.strip_prefix("0x") {
        u128::from_str_radix(hex_value, 16)
            .map_err(|err| format!("invalid hex runtime argument {value}: {err}"))?
    } else {
        trimmed
            .parse::<u128>()
            .map_err(|err| format!("invalid decimal runtime argument {value}: {err}"))?
    };
    let mut word = [0u8; 32];
    word[16..].copy_from_slice(&parsed.to_be_bytes());
    Ok(word)
}

pub(super) fn emit_real_trace_bundle(
    path: &Utf8PathBuf,
    force_standalone: bool,
    profile: &str,
    opt_level: codegen::OptLevel,
) -> Result<TraceBundle, String> {
    let mut emitter = IncrementalTraceEmitter::new(
        path,
        force_standalone,
        profile,
        opt_level,
        vec![
            "fe".to_string(),
            "dev".to_string(),
            "trace".to_string(),
            "emit".to_string(),
        ],
    )?;
    Ok(emitter.emit_from_disk()?.bundle)
}

pub(super) struct IncrementalTraceOutput {
    pub bundle: TraceBundle,
    pub salsa_events: SalsaEventCounters,
}

pub(super) struct IncrementalTraceEmitter {
    db: DriverDataBase,
    input_path: Utf8PathBuf,
    file_url: Url,
    profile: String,
    opt_level: codegen::OptLevel,
    command: Vec<String>,
    counters: Arc<Mutex<SalsaEventCounters>>,
}

impl IncrementalTraceEmitter {
    pub fn new(
        path: &Utf8PathBuf,
        force_standalone: bool,
        profile: &str,
        opt_level: codegen::OptLevel,
        command: Vec<String>,
    ) -> Result<Self, String> {
        let mut db = DriverDataBase::default();
        db.compilation_settings()
            .set_profile(&mut db)
            .to(profile.into());
        let target = resolve_cli_target(&mut db, path, force_standalone)?;
        let input_path = match target {
            CliTarget::StandaloneFile(file_path) => file_path,
            CliTarget::Directory(_) => {
                return Err(
                    "incremental trace emission currently supports standalone .fe files; ingot tracing is not wired yet"
                        .to_string(),
                );
            }
        };
        let (file_url, content) = standalone_file_input(&input_path)?;
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(content));
        let counters = Arc::new(Mutex::new(SalsaEventCounters::default()));
        db.set_salsa_event_counters(Some(counters.clone()));
        Ok(Self {
            db,
            input_path,
            file_url,
            profile: profile.to_string(),
            opt_level,
            command,
            counters,
        })
    }

    pub fn emit_from_disk(&mut self) -> Result<IncrementalTraceOutput, String> {
        if let Ok(mut counters) = self.counters.lock() {
            *counters = SalsaEventCounters::default();
        }
        let content = fs::read_to_string(&self.input_path)
            .map_err(|err| format!("failed to read trace input {}: {err}", self.input_path))?;
        let file = self
            .db
            .workspace()
            .update(&mut self.db, self.file_url.clone(), content.clone());
        let top_mod = self.db.top_mod(file);
        let bundle = emit_real_trace_bundle_from_top_mod(
            &self.db,
            top_mod,
            &self.input_path,
            &content,
            &self.profile,
            self.opt_level,
            self.command.clone(),
        )?;
        let salsa_events = self
            .counters
            .lock()
            .map(|counters| counters.clone())
            .unwrap_or_default();
        Ok(IncrementalTraceOutput {
            bundle,
            salsa_events,
        })
    }

    pub fn input_path(&self) -> &Utf8PathBuf {
        &self.input_path
    }
}

fn emit_real_trace_bundle_from_top_mod<'db>(
    db: &'db DriverDataBase,
    top_mod: hir::hir_def::TopLevelMod<'db>,
    input_path: &Utf8PathBuf,
    input_content: &str,
    profile: &str,
    opt_level: codegen::OptLevel,
    command: Vec<String>,
) -> Result<TraceBundle, String> {
    let package = mir::build_runtime_package(db, top_mod)
        .map_err(|err| format!("failed to build runtime package for trace: {err}"))?;
    let mut facts = mir::trace::emit_mir_facts(db, package);
    let module_key = top_mod.name(db).data(db).to_string();
    let sonatina_module =
        codegen::compile_runtime_package_sonatina(db, &package, codegen::EVM_LAYOUT)
            .map_err(|err| format!("failed to compile Sonatina IR for trace: {err}"))?;
    let sonatina_owner =
        codegen::trace::sonatina_module_owner_key(input_path.as_str(), &module_key);
    facts.extend(codegen::trace::emit_sonatina_trace_view_facts(
        &sonatina_owner,
        &sonatina_module,
        CompilerPhase::SonatinaPreOpt,
    ));
    let source_file = source_file_key(input_path);
    facts.extend(emit_standalone_source_file_facts(
        input_path,
        input_content,
        &source_file,
    ));
    let (bytecode, postopt_sonatina_facts) =
        codegen::emit_module_sonatina_bytecode_with_observability_and_trace(
            db,
            top_mod,
            opt_level,
            None,
            &sonatina_owner,
        )
        .map_err(|err| format!("failed to compile bytecode for trace: {err}"))?;
    let postopt_sonatina_nodes = postopt_sonatina_facts
        .iter()
        .filter_map(|fact| match fact {
            TraceFact::OriginNode(node)
                if node.key.kind() == codegen::trace::SONATINA_POSTOPT_INST_KIND =>
            {
                Some(node.key.clone())
            }
            _ => None,
        })
        .collect::<std::collections::BTreeSet<_>>();
    let postopt_sonatina_aliases = postopt_sonatina_instruction_aliases(&postopt_sonatina_facts);
    facts.extend(postopt_sonatina_facts);
    for (contract_name, artifact) in bytecode {
        let owner_key = codegen::trace::bytecode_runtime_owner_key(
            input_path.as_str(),
            &module_key,
            &contract_name,
        );
        facts.extend(
            codegen::trace::emit_bytecode_instruction_facts_with_observability(
                &owner_key,
                "function:runtime",
                &artifact.runtime,
                Some(&sonatina_owner),
                artifact.runtime_observability.as_ref(),
                Some(&postopt_sonatina_nodes),
                Some(&postopt_sonatina_aliases),
            ),
        );
        facts.extend(codegen::trace::emit_bytecode_shape_facts(
            &owner_key,
            "function:runtime",
            &artifact.runtime,
        ));
        let code_object = codegen::trace::bytecode_code_object_key(&owner_key);
        if let Some(span) = whole_file_source_span(code_object, source_file.clone(), input_content)
        {
            facts.push(TraceFact::SourceSpan(span));
        }
    }

    let metadata = trace_facts::TraceMetadata::compiler_emitted(
        super::compiler_commit(),
        "evm/sonatina",
        command,
        input_path.as_str(),
        vec![
            format!("profile={profile}"),
            format!("optimize={opt_level}"),
        ],
    );
    Ok(TraceBundle::new(metadata, facts))
}

fn postopt_sonatina_instruction_aliases(
    facts: &[TraceFact],
) -> BTreeMap<OriginExportKey, OriginExportKey> {
    facts
        .iter()
        .filter_map(|fact| {
            let TraceFact::Instruction(instruction) = fact else {
                return None;
            };
            if instruction.instruction.kind() != codegen::trace::SONATINA_POSTOPT_INST_KIND {
                return None;
            }
            let (function_prefix, _) = instruction.instruction.local_key().rsplit_once(":inst:")?;
            let alias = OriginExportKey::try_from_raw_parts(
                instruction.instruction.kind(),
                instruction.instruction.owner_key(),
                format!("{function_prefix}:inst:InstId({})", instruction.index),
            )
            .ok()?;
            (alias != instruction.instruction).then(|| (alias, instruction.instruction.clone()))
        })
        .collect()
}

fn emit_standalone_source_file_facts(
    input_path: &Utf8PathBuf,
    content: &str,
    source_file: &OriginExportKey,
) -> Vec<TraceFact> {
    vec![
        TraceFact::OriginNode(OriginNodeFact::new(
            source_file.clone(),
            OriginNodeKind::new(source_file.kind()),
        )),
        TraceFact::SourceFile(SourceFileFact::new(
            source_file.clone(),
            input_path.as_str(),
            input_path
                .file_name()
                .map_or(input_path.as_str(), |name| name)
                .to_string(),
            trace_content_hash(content.as_bytes()),
            Some(0),
        )),
    ]
}

fn source_file_key(input_path: &Utf8PathBuf) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts("source.file", input_path.as_str(), "file:0")
        .expect("trace source file key must be valid")
}

fn whole_file_source_span(
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

fn standalone_file_input(file_path: &Utf8PathBuf) -> Result<(Url, String), String> {
    let canonical = file_path
        .canonicalize_utf8()
        .map_err(|err| format!("cannot canonicalize {file_path}: {err}"))?;
    let file_url = Url::from_file_path(&canonical)
        .map_err(|_| format!("invalid trace input path: {file_path}"))?;
    let content = fs::read_to_string(file_path)
        .map_err(|err| format!("failed to read trace input {file_path}: {err}"))?;
    Ok((file_url, content))
}

pub(super) fn roundtrip_trace_bundle_jsonl(bundle: &TraceBundle) -> Result<TraceBundle, String> {
    let mut sink = JsonlTraceSink::new(Vec::new());
    sink.write_bundle(bundle)
        .map_err(|err| format!("failed to write in-memory trace JSONL: {err}"))?;
    JsonlTraceReader::new(Cursor::new(sink.into_inner()))
        .read_bundle()
        .map_err(|err| format!("failed to read in-memory trace JSONL: {err}"))
}

fn read_trace_bundle_jsonl_from_path(path: &Utf8PathBuf) -> Result<TraceBundle, String> {
    let file =
        File::open(path.as_std_path()).map_err(|err| format!("failed to open {path}: {err}"))?;
    JsonlTraceReader::new(BufReader::new(file))
        .read_bundle()
        .map_err(|err| format!("failed to read trace JSONL {path}: {err}"))
}

fn read_trace_snapshot_jsonl_from_path(path: &Utf8PathBuf) -> Result<TraceSnapshot, String> {
    TraceSnapshot::new(read_trace_bundle_jsonl_from_path(path)?)
        .map_err(|err| format!("trace validation failed for {path}: {err}"))
}

pub(super) fn write_trace_bundle_jsonl(
    path: &Utf8PathBuf,
    bundle: &TraceBundle,
) -> Result<(), String> {
    if let Some(parent) = path.parent()
        && !parent.as_str().is_empty()
    {
        fs::create_dir_all(parent.as_std_path())
            .map_err(|err| format!("failed to create {parent}: {err}"))?;
    }
    let file = File::create(path.as_std_path())
        .map_err(|err| format!("failed to create trace JSONL {path}: {err}"))?;
    let mut sink = JsonlTraceSink::new(BufWriter::new(file));
    sink.write_bundle(bundle)
        .map_err(|err| format!("failed to write trace JSONL {path}: {err}"))?;
    sink.flush()
        .map_err(|err| format!("failed to flush trace JSONL {path}: {err}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use debug_export::{
        DebugBundle, emit_dwarf_line_table, emit_ethdebug_artifact, validate_ethdebug_artifact,
    };
    use trace_query::{
        GasAttributionPolicy, IntrospectionService, RuntimeGasBySourceRequest,
        RuntimeTraceFilterRequest, TraceIntrospectionService, datalog_emit,
    };

    #[test]
    fn real_trace_bundle_compiles_fib_demo_without_fixture_claims() {
        let path = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../fib_demo.fe");
        let bundle = emit_real_trace_bundle(&path, false, "dev", codegen::OptLevel::O1).unwrap();
        let summary = TraceValidator::validate(&bundle.facts).unwrap();

        assert_eq!(
            bundle.metadata.data_source,
            trace_facts::TraceDataSource::CompilerEmitted
        );
        assert!(summary.instruction_count > 0);
        assert!(
            bundle
                .facts
                .iter()
                .any(|fact| matches!(fact, trace_facts::TraceFact::ShapeGraphHash(_))),
            "real Fibonacci trace should include derived shape hashes"
        );
        assert!(
            bundle.facts.iter().any(|fact| matches!(
                fact,
                trace_facts::TraceFact::SourceFile(source)
                    if is_content_digest(&source.content_hash)
            )),
            "source file content hashes should be cryptographic digests"
        );
        assert!(
            bundle.facts.iter().any(|fact| matches!(
                fact,
                trace_facts::TraceFact::SourceSpan(span)
                    if matches!(span.origin.kind(), "hir.expr" | "hir.stmt")
                        && span.start_line >= 1
                        && span.end_line >= span.start_line
            )),
            "real Fibonacci trace should include exact HIR expression/statement source spans"
        );
        assert!(
            bundle.facts.iter().any(|fact| matches!(
                fact,
                trace_facts::TraceFact::OriginEdge(edge)
                    if matches!(edge.from.kind(), "runtime.stmt" | "runtime.terminator")
                        && matches!(edge.to.kind(), "hir.expr" | "hir.stmt")
                        && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
            )),
            "runtime MIR origins should link back to HIR expression/statement origins"
        );
        assert!(
            bundle
                .facts
                .iter()
                .any(|fact| matches!(fact, trace_facts::TraceFact::LoopMembership(_))),
            "real Fibonacci trace should include Sonatina CFG-derived loop membership"
        );
        assert!(
            bundle.facts.iter().any(|fact| matches!(
                fact,
                trace_facts::TraceFact::LoopMembership(membership)
                    if membership.loop_key.kind() == codegen::trace::SONATINA_POSTOPT_LOOP_KIND
            )),
            "real Fibonacci trace should include post-optimization Sonatina loop membership"
        );
        assert!(
            bundle.facts.iter().any(|fact| matches!(
                fact,
                trace_facts::TraceFact::OriginEdge(edge)
                    if edge.from.kind() == "bytecode.pc"
                        && edge.to.kind() == codegen::trace::SONATINA_POSTOPT_INST_KIND
                        && edge.label == trace_facts::OriginEdgeLabel::EmittedFrom
            )),
            "bytecode PCs should be linked to Sonatina post-opt instructions"
        );
        assert!(
            bundle.facts.iter().any(|fact| matches!(
                fact,
                trace_facts::TraceFact::OriginEdge(edge)
                    if edge.from.kind() == "bytecode.pc"
                        && matches!(edge.to.kind(), "runtime.stmt" | "runtime.terminator")
                        && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
            )),
            "bytecode PCs should carry propagated MIR runtime origins"
        );
        let loop_cost =
            super::super::trace_render::render_loop_cost_bundle(bundle.clone()).unwrap();
        assert!(loop_cost.contains("Data source: compiler_emitted"));
        assert!(loop_cost.contains("Compiler-derived loop instruction summary"));
        assert!(loop_cost.contains("target bytecode loop membership"));
        assert!(!loop_cost.contains("Loop cost unavailable from this trace"));

        let loop_contents = super::super::trace_render::render_loop_contents_snapshot(
            TraceSnapshot::new(bundle.clone()).unwrap(),
        )
        .unwrap();
        assert!(loop_contents.contains("Fe dev trace loop-contents"));
        assert!(
            loop_contents.contains("Membership source: compiler-emitted Sonatina trace-view CFG")
        );
        assert!(loop_contents.contains("bytecode PCs linked by observability origin edges"));
        assert!(loop_contents.contains("Target bytecode PCs linked to this loop:"));
        assert!(loop_contents.contains("Loop blocks:"));

        let gas_by_source = super::super::trace_render::render_gas_by_source_snapshot(
            TraceSnapshot::new(bundle.clone()).unwrap(),
            "cancun",
            "exclusive-primary",
        )
        .unwrap();
        assert!(gas_by_source.contains("fib_demo.fe"));
        assert!(gas_by_source.contains("Attribution policy: exclusive-primary"));
        assert!(!gas_by_source.contains("<unmapped>"));

        let bytecode_size = super::super::trace_render::render_bytecode_size_by_source_snapshot(
            TraceSnapshot::new(bundle.clone()).unwrap(),
            "exclusive-primary",
        )
        .unwrap();
        assert!(bytecode_size.contains("fib_demo.fe"));
        assert!(bytecode_size.contains("Total emitted bytecode bytes"));
        assert!(!bytecode_size.contains("<unmapped>"));

        let explain = super::super::trace_render::render_explain_local_bundle(bundle, "b").unwrap();
        assert!(explain.contains("Why b is memory-backed in MIR"));
        assert!(explain.contains("Mir: memory place (MutableLocalLowering)"));
        assert!(!explain.contains("stack slot sp+24"));
    }

    #[test]
    fn fib_demo_runtime_acceptance_matrix_uses_one_validated_bundle() {
        let temp = tempfile::tempdir().unwrap();
        let static_path = Utf8PathBuf::from_path_buf(temp.path().join("fib.static.trace.jsonl"))
            .expect("temp path should be utf8");
        let combined_path =
            Utf8PathBuf::from_path_buf(temp.path().join("fib.combined.trace.jsonl"))
                .expect("temp path should be utf8");
        let source_path = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../fib_demo.fe");
        let static_bundle =
            emit_real_trace_bundle(&source_path, false, "dev", codegen::OptLevel::O1).unwrap();
        write_trace_bundle_jsonl(&static_path, &static_bundle).unwrap();

        let run_output = run_trace_run(&crate::DevTraceRunArgs {
            static_trace: static_path,
            source: Some(source_path),
            entry: "Fib::Compute".to_string(),
            selector: "0x01".to_string(),
            args: vec!["8".to_string()],
            runtime_capture: crate::RuntimeCaptureModeArg::Standard,
            value_policy: crate::RuntimeValuePolicyArg::HashOnly,
            out: combined_path.clone(),
        })
        .unwrap();
        assert!(run_output.contains("Runtime source: revm_inspector"));
        assert!(run_output.contains("Runtime facts appended:"));

        let snapshot = read_trace_snapshot_jsonl_from_path(&combined_path).unwrap();
        let validation = snapshot.validation();
        assert_eq!(validation.error_count(), 0);
        assert!(
            validation.summary.fact_count > static_bundle.facts.len(),
            "combined bundle should append runtime facts to the static trace"
        );
        assert!(
            snapshot.facts().iter().any(|fact| matches!(
                fact,
                TraceFact::ExecutionStep(step)
                    if step.instruction.is_some()
                        && step.join_confidence
                            == trace_facts::RuntimePcJoinConfidence::ExactCodeObjectAndPc
            )),
            "runtime steps should join back to static bytecode instructions"
        );
        assert!(
            snapshot
                .facts()
                .iter()
                .any(|fact| matches!(fact, TraceFact::MemoryAccess(_))),
            "standard capture should include memory access ranges for runtime queries"
        );

        let service = TraceIntrospectionService::new(snapshot.clone());
        let runtime_gas = service
            .runtime_gas_by_source(RuntimeGasBySourceRequest {
                trace_id: None,
                policy: GasAttributionPolicy::RuntimeStepExclusive,
            })
            .unwrap();
        assert_eq!(runtime_gas.policy, "runtime-step-exclusive");
        assert!(runtime_gas.total_gas > 0);
        assert!(runtime_gas.runtime.session_count > 0);
        assert!(runtime_gas.runtime.exact_join_steps > 0);
        assert_eq!(runtime_gas.runtime.missing_join_steps, 0);
        assert!(!runtime_gas.rows.is_empty());

        let memory = service
            .memory_growth_by_source(RuntimeTraceFilterRequest {
                trace_id: None,
                policy: GasAttributionPolicy::RuntimeStepExclusive,
            })
            .unwrap();
        assert!(memory.total_accesses > 0);

        let storage = service
            .storage_writes_by_source(RuntimeTraceFilterRequest {
                trace_id: None,
                policy: GasAttributionPolicy::RuntimeStepExclusive,
            })
            .unwrap();
        assert_eq!(
            storage.total_writes, 0,
            "Fibonacci should not manufacture storage writes"
        );

        let datalog =
            datalog_emit::run_builtin_rulepack(&snapshot, "gas-v1", "runtime-gas-by-source")
                .unwrap();
        assert_eq!(datalog.status, "ok");
        assert_eq!(datalog.attribution_policy, "runtime-step-exclusive");
        assert!(datalog.base_row_count >= snapshot.facts().len());

        let debug_bundle = DebugBundle::from_snapshot(&snapshot);
        let dwarf = emit_dwarf_line_table(&debug_bundle).unwrap();
        assert!(!dwarf.rows.is_empty());
        let ethdebug = emit_ethdebug_artifact(&debug_bundle).unwrap();
        validate_ethdebug_artifact(&ethdebug).unwrap();
        assert!(!ethdebug.programs.is_empty());

        let agent_golden = serde_json::json!({
            "fixture": "fib_demo.fe",
            "data_source": snapshot.metadata().data_source,
            "surfaces": {
                "static_trace": true,
                "runtime_trace": runtime_gas.runtime.session_count > 0,
                "datalog": datalog.status,
                "dwarf_line_rows": !dwarf.rows.is_empty(),
                "ethdebug_programs": !ethdebug.programs.is_empty(),
            },
            "confidence": {
                "runtime_gas": runtime_gas.confidence,
                "memory_growth": memory.confidence,
                "storage_writes": storage.confidence,
            },
            "runtime": {
                "policy": runtime_gas.policy,
                "exact_join_steps": runtime_gas.runtime.exact_join_steps,
                "missing_join_steps": runtime_gas.runtime.missing_join_steps,
                "total_gas_positive": runtime_gas.total_gas > 0,
            }
        });
        assert_eq!(agent_golden["fixture"], "fib_demo.fe");
        assert_eq!(agent_golden["runtime"]["missing_join_steps"], 0);
        assert_eq!(
            agent_golden["surfaces"]["datalog"],
            serde_json::Value::String("ok".to_string())
        );
    }

    fn is_content_digest(value: &str) -> bool {
        let digest = value.strip_prefix("blake3:").unwrap_or(value);
        digest.len() == 64
            && digest.chars().all(|ch| ch.is_ascii_hexdigit())
            && !value.starts_with("fnv64:")
    }
}
