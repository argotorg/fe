use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::{BufReader, BufWriter, Cursor};

use camino::Utf8PathBuf;
use common::{InputDb, origin::OriginExportKey};
use driver::{
    DriverDataBase,
    cli_target::{CliTarget, resolve_cli_target},
};
use salsa::Setter;
use trace_facts::{
    CategorySource, CompilerEventFact, CompilerEventKind, CompilerPhase, CompilerReason,
    InstructionCategory, InstructionCategoryFact, InstructionFact, JsonlTraceReader,
    JsonlTraceSink, LoopDerivation, LoopMembershipFact, OriginEdgeFact, OriginEdgeLabel,
    OriginNodeFact, OriginNodeKind, StorageFact, StorageLocation, StorageReason, TraceBundle,
    TraceDataSource, TraceFact, TraceMetadata, TraceSnapshot, TraceValidationReport,
    TraceValidator,
};
use trace_query::{
    ExplainLocalReport, ExplainLocalRequest, IntrospectionService, LoopCostReport, LoopCostRequest,
    TraceIntrospectionService,
};
use url::Url;

use crate::{
    DevCommand, DevTraceCommand, DevTraceEmitArgs, DevTraceExplainLocalArgs, DevTraceInputArgs,
    DevTraceQueryCommand, TraceFixtureCommand, TraceFixtureEmitArgs, TraceFixtureExplainLocalArgs,
    TraceFixtureLoopCostArgs,
};

const FIB_OWNER: &str = "fixture:fib_demo";

pub(crate) fn run_dev_command(command: &DevCommand) -> Result<String, String> {
    match command {
        DevCommand::TraceFixture { command } => run_trace_fixture_command(command),
        DevCommand::Trace { command } => run_dev_trace_command(command),
    }
}

fn run_trace_fixture_command(command: &TraceFixtureCommand) -> Result<String, String> {
    match command {
        TraceFixtureCommand::Emit(args) => run_fixture_emit(args),
        TraceFixtureCommand::LoopCost(args) => run_fixture_loop_cost(args),
        TraceFixtureCommand::ExplainLocal(args) => run_fixture_explain_local(args),
    }
}

fn run_dev_trace_command(command: &DevTraceCommand) -> Result<String, String> {
    match command {
        DevTraceCommand::Status => Ok(
            "fe dev trace is reserved for compiler-derived validated trace JSONL.\n\
             Current Fibonacci diagnostics are fixture-backed and live under fe dev trace-fixture.\n\
             Real trace emission currently includes phase-owned MIR facts and actual EVM bytecode instruction facts; loop/storage/zext causality hooks are still incomplete.\n\
             zext-report is intentionally unavailable until InsertIntegerZeroExtend events and value properties are emitted by compiler phases.\n"
                .to_string(),
        ),
        DevTraceCommand::Emit(args) => run_trace_emit(args),
        DevTraceCommand::Validate(args) => run_trace_validate(args),
        DevTraceCommand::Query { command } => run_trace_query_command(command),
        DevTraceCommand::LoopCost(args) => run_trace_loop_cost(args),
        DevTraceCommand::ExplainLocal(args) => run_trace_explain_local(args),
    }
}

fn run_trace_query_command(command: &DevTraceQueryCommand) -> Result<String, String> {
    match command {
        DevTraceQueryCommand::LoopCost(args) => run_trace_loop_cost(args),
        DevTraceQueryCommand::ExplainLocal(args) => run_trace_explain_local(args),
    }
}

fn run_fixture_emit(args: &TraceFixtureEmitArgs) -> Result<String, String> {
    let bundle = build_fib_fixture_bundle_from_path(&args.path, &args.function)?;
    write_trace_bundle_jsonl(&args.out, &bundle)?;
    Ok(format!(
        "wrote fixture trace JSONL: {}\nData source: {}\nFacts: {}\n",
        args.out,
        format_data_source(&bundle.metadata),
        bundle.facts.len()
    ))
}

fn run_fixture_loop_cost(args: &TraceFixtureLoopCostArgs) -> Result<String, String> {
    let bundle = build_and_roundtrip_fib_fixture_bundle(&args.path, &args.function)?;
    render_loop_cost_bundle(bundle)
}

fn run_fixture_explain_local(args: &TraceFixtureExplainLocalArgs) -> Result<String, String> {
    let bundle = build_and_roundtrip_fib_fixture_bundle(&args.path, &args.function)?;
    render_explain_local_bundle(bundle, &args.local)
}

fn run_trace_emit(args: &DevTraceEmitArgs) -> Result<String, String> {
    let opt_level = args.optimize.parse::<codegen::OptLevel>()?;
    let bundle = emit_real_trace_bundle(&args.path, args.standalone, &args.profile, opt_level)?;
    let summary = TraceValidator::validate(&bundle.facts)
        .map_err(|err| format!("compiler trace emission produced invalid facts: {err}"))?;
    write_trace_bundle_jsonl(&args.out, &bundle)?;
    Ok(format!(
        "wrote compiler trace JSONL: {}\nData source: {}\nFacts: {}\nOrigin nodes: {}\nInstructions: {}\n",
        args.out,
        format_data_source(&bundle.metadata),
        summary.fact_count,
        summary.node_count,
        summary.instruction_count
    ))
}

fn run_trace_validate(args: &DevTraceInputArgs) -> Result<String, String> {
    let snapshot = read_trace_snapshot_jsonl_from_path(&args.from)?;
    Ok(render_validation_summary(
        snapshot.metadata(),
        snapshot.validation(),
    ))
}

fn run_trace_loop_cost(args: &DevTraceInputArgs) -> Result<String, String> {
    render_loop_cost_snapshot(read_trace_snapshot_jsonl_from_path(&args.from)?)
}

fn run_trace_explain_local(args: &DevTraceExplainLocalArgs) -> Result<String, String> {
    render_explain_local_snapshot(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        &args.local,
    )
}

fn build_fib_fixture_bundle_from_path(
    path: &Utf8PathBuf,
    function_label: &str,
) -> Result<TraceBundle, String> {
    let source = fs::read_to_string(path.as_std_path())
        .map_err(|err| format!("failed to read {path}: {err}"))?;
    build_fib_fixture_bundle(&source, path.as_str(), function_label)
}

fn emit_real_trace_bundle(
    path: &Utf8PathBuf,
    force_standalone: bool,
    profile: &str,
    opt_level: codegen::OptLevel,
) -> Result<TraceBundle, String> {
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_profile(&mut db)
        .to(profile.into());
    let target = resolve_cli_target(&mut db, path, force_standalone)?;
    let (top_mod, input_path) = match target {
        CliTarget::StandaloneFile(file_path) => {
            let (file_url, content) = standalone_file_input(&file_path)?;
            db.workspace()
                .touch(&mut db, file_url.clone(), Some(content));
            let file = db
                .workspace()
                .get(&db, &file_url)
                .ok_or_else(|| format!("could not process trace input {file_path}"))?;
            let top_mod = db.top_mod(file);
            (top_mod, file_path)
        }
        CliTarget::Directory(_) => {
            return Err(
                "fe dev trace emit currently supports standalone .fe files; ingot tracing is not wired yet"
                    .to_string(),
            );
        }
    };

    let package = mir::build_runtime_package(&db, top_mod)
        .map_err(|err| format!("failed to build runtime package for trace: {err}"))?;
    let mut facts = mir::trace::emit_mir_facts(&db, package);
    let bytecode = codegen::emit_module_sonatina_bytecode(&db, top_mod, opt_level, None)
        .map_err(|err| format!("failed to compile bytecode for trace: {err}"))?;
    for (contract_name, artifact) in bytecode {
        facts.extend(codegen::trace::emit_bytecode_instruction_facts(
            &format!("contract:{contract_name}:runtime"),
            "runtime",
            &artifact.runtime,
        ));
    }

    let metadata = TraceMetadata::compiler_emitted(
        compiler_commit(),
        "evm/sonatina",
        vec![
            "fe".to_string(),
            "dev".to_string(),
            "trace".to_string(),
            "emit".to_string(),
        ],
        input_path.as_str(),
        vec![
            format!("profile={profile}"),
            format!("optimize={opt_level}"),
        ],
    );
    Ok(TraceBundle::new(metadata, facts))
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

fn build_and_roundtrip_fib_fixture_bundle(
    path: &Utf8PathBuf,
    function_label: &str,
) -> Result<TraceBundle, String> {
    let bundle = build_fib_fixture_bundle_from_path(path, function_label)?;
    roundtrip_trace_bundle_jsonl(&bundle)
}

fn roundtrip_trace_bundle_jsonl(bundle: &TraceBundle) -> Result<TraceBundle, String> {
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

fn write_trace_bundle_jsonl(path: &Utf8PathBuf, bundle: &TraceBundle) -> Result<(), String> {
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

fn render_validation_summary(metadata: &TraceMetadata, report: &TraceValidationReport) -> String {
    format!(
        "Trace validation: passed\n\
         Data source: {}\n\
         Schema version: {}\n\
         Compiler commit: {}\n\
         Target: {}\n\
         Input: {}\n\
         Facts: {}\n\
         Origin nodes: {}\n\
         Origin edges: {}\n\
         Instructions: {}\n\
         Diagnostics: {} error, {} warning, {} info\n",
        format_data_source(metadata),
        metadata.schema_version,
        metadata.compiler_commit,
        metadata.target,
        metadata.input_path,
        report.summary.fact_count,
        report.summary.node_count,
        report.summary.edge_count,
        report.summary.instruction_count,
        report.error_count(),
        report.warning_count(),
        report.info_count()
    )
}

fn build_fib_fixture_bundle(
    source: &str,
    path_label: &str,
    function_label: &str,
) -> Result<TraceBundle, String> {
    require_fib_fixture(source)?;

    let function = key("function", FIB_OWNER, "contract:Fib.recv:Compute");
    let loop_key = key("loop", FIB_OWNER, "while:i<n");
    let mut facts = Vec::new();

    push_node(&mut facts, function.clone(), "function");
    push_node(&mut facts, loop_key.clone(), "loop");

    let locals = [
        ("n", "local:n"),
        ("a", "local:a"),
        ("b", "local:b"),
        ("i", "local:i"),
        ("next", "local:next"),
    ]
    .into_iter()
    .map(|(name, local)| {
        let key = key("runtime.local", FIB_OWNER, local);
        push_node(&mut facts, key.clone(), "runtime.local");
        (name.to_string(), key)
    })
    .collect::<BTreeMap<_, _>>();

    facts.push(TraceFact::Storage(StorageFact::new(
        locals["b"].clone(),
        CompilerPhase::Mir,
        StorageLocation::MemoryPlace,
        StorageReason::MutableLocalLowering,
    )));
    facts.push(TraceFact::Storage(StorageFact::new(
        locals["b"].clone(),
        CompilerPhase::Backend,
        StorageLocation::StackSlot { offset: 24 },
        StorageReason::FrameSlot,
    )));
    for name in ["i", "n", "a", "next"] {
        facts.push(TraceFact::Storage(StorageFact::new(
            locals[name].clone(),
            CompilerPhase::SonatinaPreOpt,
            StorageLocation::SsaValue,
            StorageReason::Unknown,
        )));
    }

    let insts = [
        (
            "lw a3, 24(sp)",
            InstructionCategory::StackLoad,
            Some(("b", OriginEdgeLabel::LoadOf)),
        ),
        ("add a4, a2, a3", InstructionCategory::Arithmetic, None),
        ("mv a2, a3", InstructionCategory::Move, None),
        (
            "sw a4, 24(sp)",
            InstructionCategory::StackStore,
            Some(("b", OriginEdgeLabel::StoreOf)),
        ),
        (
            "addiw a1, a1, 1",
            InstructionCategory::Arithmetic,
            Some(("i", OriginEdgeLabel::EmittedFrom)),
        ),
        (
            "slli a1, a1, 32",
            InstructionCategory::ZeroExtend,
            Some(("i", OriginEdgeLabel::IntegerLegalizationFor)),
        ),
        (
            "srli a1, a1, 32",
            InstructionCategory::ZeroExtend,
            Some(("i", OriginEdgeLabel::IntegerLegalizationFor)),
        ),
        (
            "slli a0, a0, 32",
            InstructionCategory::ZeroExtend,
            Some(("n", OriginEdgeLabel::IntegerLegalizationFor)),
        ),
        (
            "srli a0, a0, 32",
            InstructionCategory::ZeroExtend,
            Some(("n", OriginEdgeLabel::IntegerLegalizationFor)),
        ),
        ("bltu a1, a0, loop", InstructionCategory::Branch, None),
        ("mv a5, a2", InstructionCategory::Move, None),
        ("mv a6, a3", InstructionCategory::Move, None),
        ("j loop", InstructionCategory::Jump, None),
    ];

    let mut zext_event_index = 0;
    for (index, (mnemonic, category, edge)) in insts.iter().enumerate() {
        let instruction = key("asm.inst", FIB_OWNER, &format!("inst:{index}"));
        push_node(&mut facts, instruction.clone(), "asm.inst");
        facts.push(TraceFact::Instruction(InstructionFact::new(
            instruction.clone(),
            function.clone(),
            index as u32,
            *mnemonic,
        )));
        facts.push(TraceFact::InstructionCategory(
            InstructionCategoryFact::new(
                instruction.clone(),
                *category,
                CategorySource::PosthocClassifier {
                    version: "fib-demo-riscv-v1".to_string(),
                },
            ),
        ));
        facts.push(TraceFact::LoopMembership(LoopMembershipFact::new(
            loop_key.clone(),
            instruction.clone(),
            LoopDerivation::BackendBlockMapping,
        )));
        if let Some((local_name, label)) = edge {
            facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                locals[*local_name].clone(),
                *label,
                Some(CompilerPhase::Backend),
            )));
            if *label == OriginEdgeLabel::IntegerLegalizationFor {
                let event = key(
                    "compiler.event",
                    FIB_OWNER,
                    &format!("event:{zext_event_index}"),
                );
                zext_event_index += 1;
                push_node(&mut facts, event.clone(), "compiler.event");
                facts.push(TraceFact::CompilerEvent(CompilerEventFact::new(
                    event,
                    CompilerPhase::Backend,
                    CompilerEventKind::InsertIntegerZeroExtend,
                    vec![locals[*local_name].clone()],
                    vec![instruction],
                    Some(CompilerReason::new(zext_reason(local_name))),
                )));
            }
        }
    }

    TraceValidator::validate(&facts).map_err(|err| format!("invalid Fibonacci trace: {err}"))?;
    let metadata = TraceMetadata::fixture(
        compiler_commit(),
        "riscv64-fib-demo",
        vec![
            "fe".to_string(),
            "dev".to_string(),
            "trace-fixture".to_string(),
            "emit".to_string(),
        ],
        path_label,
        vec![format!("function={function_label}")],
        "fib_demo_codegen_ux_v1",
    );
    Ok(TraceBundle::new(metadata, facts))
}

fn push_node(facts: &mut Vec<TraceFact>, key: OriginExportKey, kind: &str) {
    facts.push(TraceFact::OriginNode(OriginNodeFact::new(
        key,
        OriginNodeKind::new(kind),
    )));
}

fn compiler_commit() -> String {
    runtime_git_commit()
        .or_else(|| option_env!("FE_GIT_COMMIT").map(str::to_string))
        .unwrap_or_else(|| "unknown".to_string())
}

fn runtime_git_commit() -> Option<String> {
    let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let output = std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(repo_root)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let commit = String::from_utf8(output.stdout).ok()?.trim().to_string();
    (!commit.is_empty()).then_some(commit)
}

fn format_data_source(metadata: &TraceMetadata) -> String {
    match metadata.data_source {
        TraceDataSource::Fixture => {
            let marker = metadata.fixture_marker.as_deref().unwrap_or("unspecified");
            format!("fixture ({marker}; not compiler-derived)")
        }
        TraceDataSource::CompilerEmitted => "compiler_emitted".to_string(),
    }
}

fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
}

fn require_fib_fixture(source: &str) -> Result<(), String> {
    let required = [
        "msg FibMsg",
        "pub contract Fib",
        "while i < n",
        "let mut b: u32 = 1",
    ];
    let missing = required
        .iter()
        .copied()
        .filter(|needle| !source.contains(needle))
        .collect::<Vec<_>>();
    if missing.is_empty() {
        Ok(())
    } else {
        Err(format!(
            "this MVP currently expects fib_demo.fe; missing fixture markers: {}",
            missing.join(", ")
        ))
    }
}

fn zext_reason(local_name: &str) -> &'static str {
    match local_name {
        "i" => {
            "u32 loop index normalized before compare; known-width facts are not preserved after addiw"
        }
        "n" => {
            "u32 loop bound normalized inside the loop; n is loop-invariant, so this is a hoist candidate"
        }
        _ => "u32 value normalized before compare",
    }
}

fn render_loop_cost_bundle(bundle: TraceBundle) -> Result<String, String> {
    render_loop_cost_snapshot(
        TraceSnapshot::new(bundle).map_err(|err| format!("trace validation failed: {err}"))?,
    )
}

fn render_loop_cost_snapshot(snapshot: TraceSnapshot) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .loop_cost(LoopCostRequest::default())
        .map_err(|err| err.to_string())?;
    Ok(render_loop_cost_report(&report))
}

fn render_explain_local_bundle(bundle: TraceBundle, local_name: &str) -> Result<String, String> {
    render_explain_local_snapshot(
        TraceSnapshot::new(bundle).map_err(|err| format!("trace validation failed: {err}"))?,
        local_name,
    )
}

fn render_explain_local_snapshot(
    snapshot: TraceSnapshot,
    local_name: &str,
) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .explain_local(ExplainLocalRequest {
            local: local_name.to_string(),
        })
        .map_err(|err| err.to_string())?;
    Ok(render_explain_local_report(&report))
}

fn render_loop_cost_report(report: &LoopCostReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace loop-cost\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    if let Some(function_label) = report.metadata.function_label() {
        out.push_str(&format!("Function: {function_label}\n"));
    }
    if !report.available {
        out.push('\n');
        out.push_str("Loop cost unavailable from this trace.\n");
        if let Some(reason) = &report.unavailable_reason {
            out.push_str(&format!("Reason: {reason}.\n\n"));
        }
        out.push_str("Available compiler-derived bytecode summary:\n");
    } else if let Some(loop_label) = &report.loop_label {
        out.push_str(&format!("Loop: {loop_label}\n\n"));
        out.push_str("Static per-iteration cost:\n");
    }

    out.push_str(&format!(
        "  total instructions: {}\n",
        report.summary.total_instructions
    ));
    out.push_str(&format!(
        "  zero-extends: {}\n",
        report.summary.zero_extends
    ));
    out.push_str(&format!("  stack loads: {}\n", report.summary.stack_loads));
    out.push_str(&format!(
        "  stack stores: {}\n",
        report.summary.stack_stores
    ));
    out.push_str(&format!("  moves: {}\n", report.summary.moves));
    out.push_str(&format!(
        "  branches/jumps: {}\n",
        report.summary.branch_like()
    ));
    out.push_str(&format!("  arithmetic: {}\n", report.summary.arithmetic));
    if !report.available {
        out.push_str(&format!("  loads: {}\n", report.summary.loads));
        out.push_str(&format!("  stores: {}\n\n", report.summary.stores));
        out.push_str("Required next facts: loop membership, source-local display facts, MIR-to-codegen origin edges, and backend storage/zext compiler events.\n");
        return out;
    }

    out.push_str("\nRepeated zero-extensions:\n");
    if report.repeated_zero_extends.is_empty() {
        out.push_str("  none attributed\n");
    }
    for group in &report.repeated_zero_extends {
        let labels = group
            .instructions
            .iter()
            .map(|inst| format!("asm[{}] {}", inst.index, inst.mnemonic))
            .collect::<Vec<_>>();
        out.push_str(&format!(
            "  {}: {} zero-extend instructions",
            group.local,
            group.instructions.len()
        ));
        if !labels.is_empty() {
            out.push_str(&format!(" ({})", labels.join(", ")));
        }
        out.push('\n');
        let reason = group
            .reason
            .as_deref()
            .unwrap_or("missing compiler event reason");
        out.push_str(&format!(
            "    cause: backend integer legalization; {reason}\n"
        ));
    }

    if let Some(impact) = report
        .storage_impacts
        .iter()
        .find(|impact| impact.local == "b")
    {
        out.push_str("\nStack residency:\n");
        if let Some(step) = impact
            .storage_history
            .iter()
            .find(|step| step.location.contains("stack slot"))
        {
            out.push_str(&format!(
                "  b: {}, earliest memory-like phase: MIR\n",
                step.location
            ));
        }
        out.push_str(
            "  reason: mutable-local lowering made b a memory place before backend frame layout\n",
        );
        out.push_str(&format!(
            "  loop traffic: {} load + {} store per iteration\n",
            impact.loads, impact.stores
        ));
        out.push_str("  suggested area: scalar promotion / mem2reg for loop-carried u32 locals\n");
    }

    out.push_str("\nSummary:\n");
    if report
        .metadata
        .data_source
        .starts_with("fixture (fib_demo_codegen_ux_v1")
    {
        out.push_str("  Fe loop: 13 static instructions; Rust reference: 6.\n");
        out.push_str("  Excess work is dominated by 4 repeated zero-extends and 2 stack-memory ops per iteration.\n");
    } else {
        out.push_str(&format!(
            "  Derived loop contains {} static instructions, {} zero-extends, and {} stack-memory ops.\n",
            report.summary.total_instructions,
            report.summary.zero_extends,
            report.summary.stack_loads + report.summary.stack_stores
        ));
    }
    out
}

fn render_explain_local_report(report: &ExplainLocalReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace explain-local\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    if let Some(function_label) = report.metadata.function_label() {
        out.push_str(&format!("Function: {function_label}\n"));
    }
    out.push_str(&format!("Local: {}\n", report.local));

    let Some(local_key) = &report.local_key else {
        out.push('\n');
        out.push_str("Local explanation unavailable from this trace.\n");
        if let Some(reason) = &report.unavailable_reason {
            out.push_str(&format!("Reason: {reason}.\n"));
        }
        if report.available_locals.is_empty() {
            out.push_str("Available runtime local identities: none emitted.\n");
        } else {
            out.push_str(&format!(
                "Available runtime local identities: {} emitted; showing first 20.\n",
                report.available_locals.len()
            ));
            for local in &report.available_locals {
                out.push_str(&format!("  {local}\n"));
            }
        }
        return out;
    };

    out.push_str(&format!("Identity: {}\n\n", local_key.display_label()));
    out.push_str("Storage history:\n");
    for storage in &report.storage_history {
        out.push_str(&format!(
            "  {}: {} ({})\n",
            storage.phase, storage.location, storage.reason
        ));
    }

    if report.local == "b" {
        out.push_str("\nWhy b is stack-resident:\n");
        out.push_str("  earliest memory-like phase: MIR\n");
        out.push_str("  b is mutable and loop-carried, and current MIR lowering materializes it as a memory place.\n");
        out.push_str(
            "  Backend frame layout then assigns that memory place to stack slot sp+24.\n",
        );
        out.push_str("  This trace does not blame late register allocation; the first recorded memory decision is MIR mutable-local lowering.\n");
    }

    out.push_str("\nRelated loop instructions:\n");
    for related in &report.related_instructions {
        out.push_str(&format!(
            "  asm[{}] {:<18} {:?}\n",
            related.instruction.index, related.instruction.mnemonic, related.edge_label
        ));
    }

    if matches!(report.local.as_str(), "i" | "n") {
        out.push_str("\nZero-extension diagnosis:\n");
        let reason = report
            .zero_extends
            .first()
            .and_then(|related| related.reason.as_deref())
            .unwrap_or("missing compiler event reason");
        out.push_str(&format!(
            "  repeated zero-extensions for {}: {}\n",
            report.local,
            report.zero_extends.len()
        ));
        out.push_str(&format!("  cause: {reason}\n"));
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    const FIB_SOURCE: &str = include_str!("../../../fib_demo.fe");

    #[test]
    fn loop_cost_report_identifies_fib_codegen_findings() {
        let bundle =
            build_fib_fixture_bundle(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler")
                .unwrap();
        let report =
            render_loop_cost_bundle(roundtrip_trace_bundle_jsonl(&bundle).unwrap()).unwrap();

        assert!(report.contains("total instructions: 13"));
        assert!(report.contains("zero-extends: 4"));
        assert!(report.contains("i: 2 zero-extend instructions"));
        assert!(report.contains("n: 2 zero-extend instructions"));
        assert!(report.contains("b: stack slot sp+24"));
        assert!(report.contains("Rust reference: 6"));
    }

    #[test]
    fn explain_local_report_explains_b_stack_residency() {
        let bundle =
            build_fib_fixture_bundle(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler")
                .unwrap();
        let report =
            render_explain_local_bundle(roundtrip_trace_bundle_jsonl(&bundle).unwrap(), "b")
                .unwrap();

        assert!(report.contains("earliest memory-like phase: MIR"));
        assert!(report.contains("MIR mutable-local lowering"));
        assert!(report.contains("asm[0] lw a3, 24(sp)"));
        assert!(report.contains("asm[3] sw a4, 24(sp)"));
    }

    #[test]
    fn explain_local_report_identifies_i_zero_extends() {
        let bundle =
            build_fib_fixture_bundle(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler")
                .unwrap();
        let report =
            render_explain_local_bundle(roundtrip_trace_bundle_jsonl(&bundle).unwrap(), "i")
                .unwrap();

        assert!(report.contains("repeated zero-extensions for i: 2"));
        assert!(report.contains("known-width facts are not preserved after addiw"));
    }

    #[test]
    fn fixture_trace_emits_jsonl_bundle_before_reports() {
        let bundle =
            build_fib_fixture_bundle(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler")
                .unwrap();
        let roundtripped = roundtrip_trace_bundle_jsonl(&bundle).unwrap();

        assert_eq!(
            roundtripped.metadata.fixture_marker.as_deref(),
            Some("fib_demo_codegen_ux_v1")
        );
        assert_eq!(roundtripped.metadata.data_source, TraceDataSource::Fixture);
        assert!(TraceValidator::validate(&roundtripped.facts).is_ok());
    }

    #[test]
    fn real_trace_bundle_compiles_fib_demo_without_fixture_claims() {
        let path = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../fib_demo.fe");
        let bundle = emit_real_trace_bundle(&path, false, "dev", codegen::OptLevel::O1).unwrap();
        let summary = TraceValidator::validate(&bundle.facts).unwrap();

        assert_eq!(
            bundle.metadata.data_source,
            TraceDataSource::CompilerEmitted
        );
        assert!(summary.instruction_count > 0);
        assert!(
            render_loop_cost_bundle(bundle)
                .unwrap()
                .contains("Loop cost unavailable from this trace")
        );
    }

    #[test]
    fn status_keeps_zext_report_gated_on_compiler_facts() {
        let output = run_dev_trace_command(&DevTraceCommand::Status).unwrap();

        assert!(output.contains("zext-report is intentionally unavailable"));
        assert!(output.contains("InsertIntegerZeroExtend events"));
    }
}
