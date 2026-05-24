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
    CompilerPhase, JsonlTraceReader, JsonlTraceSink, OriginNodeFact, OriginNodeKind,
    SourceFileFact, SourceSpanFact, TraceBundle, TraceFact, TraceSnapshot, TraceValidator,
};
use url::Url;

use crate::{
    DevTraceAttributionArgs, DevTraceDynamicGasArgs, DevTraceEmitArgs, DevTraceExplainLocalArgs,
    DevTraceGasArgs, DevTraceGasToSourceArgs, DevTraceInputArgs, DevTracePcArgs,
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

pub(super) fn run_trace_variables_at_pc(args: &DevTracePcArgs) -> Result<String, String> {
    super::trace_render::render_variables_at_pc_snapshot_with_format(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.pc,
        args.format,
    )
}

pub(super) fn emit_real_trace_bundle(
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
    let (top_mod, input_path, input_content) = match target {
        CliTarget::StandaloneFile(file_path) => {
            let (file_url, content) = standalone_file_input(&file_path)?;
            db.workspace()
                .touch(&mut db, file_url.clone(), Some(content.clone()));
            let file = db
                .workspace()
                .get(&db, &file_url)
                .ok_or_else(|| format!("could not process trace input {file_path}"))?;
            let top_mod = db.top_mod(file);
            (top_mod, file_path, content)
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
    let module_key = top_mod.name(&db).data(&db).to_string();
    let sonatina_module =
        codegen::compile_runtime_package_sonatina(&db, &package, codegen::EVM_LAYOUT)
            .map_err(|err| format!("failed to compile Sonatina IR for trace: {err}"))?;
    let sonatina_owner =
        codegen::trace::sonatina_module_owner_key(input_path.as_str(), &module_key);
    facts.extend(codegen::trace::emit_sonatina_trace_view_facts(
        &sonatina_owner,
        &sonatina_module,
        CompilerPhase::SonatinaPreOpt,
    ));
    let source_file = source_file_key(&input_path);
    facts.extend(emit_standalone_source_file_facts(
        &input_path,
        &input_content,
        &source_file,
    ));
    let bytecode = codegen::emit_module_sonatina_bytecode(&db, top_mod, opt_level, None)
        .map_err(|err| format!("failed to compile bytecode for trace: {err}"))?;
    for (contract_name, artifact) in bytecode {
        let owner_key = codegen::trace::bytecode_runtime_owner_key(
            input_path.as_str(),
            &module_key,
            &contract_name,
        );
        facts.extend(codegen::trace::emit_bytecode_instruction_facts(
            &owner_key,
            "function:runtime",
            &artifact.runtime,
        ));
        facts.extend(codegen::trace::emit_bytecode_shape_facts(
            &owner_key,
            "function:runtime",
            &artifact.runtime,
        ));
        let code_object = codegen::trace::bytecode_code_object_key(&owner_key);
        if let Some(span) = whole_file_source_span(code_object, source_file.clone(), &input_content)
        {
            facts.push(TraceFact::SourceSpan(span));
        }
    }

    let metadata = trace_facts::TraceMetadata::compiler_emitted(
        super::compiler_commit(),
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
                .map_or_else(|| input_path.as_str(), |name| name)
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
    let mut hash = 0xcbf29ce484222325u64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("fnv64:{hash:016x}")
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
            bundle
                .facts
                .iter()
                .any(|fact| matches!(fact, trace_facts::TraceFact::LoopMembership(_))),
            "real Fibonacci trace should include Sonatina CFG-derived loop membership"
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
        assert!(
            loop_contents
                .contains("target bytecode loop membership requires Sonatina-to-bytecode edges")
        );
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
}
