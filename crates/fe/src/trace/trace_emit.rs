use std::fs::{self, File};
use std::io::{BufReader, BufWriter, Cursor};

use camino::Utf8PathBuf;
use common::InputDb;
use driver::{
    DriverDataBase,
    cli_target::{CliTarget, resolve_cli_target},
};
use salsa::Setter;
use trace_facts::{JsonlTraceReader, JsonlTraceSink, TraceBundle, TraceSnapshot, TraceValidator};
use url::Url;

use crate::{
    DevTraceDynamicGasArgs, DevTraceEmitArgs, DevTraceExplainLocalArgs, DevTraceGasArgs,
    DevTraceInputArgs, DevTracePcArgs,
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
    Ok(super::trace_render::render_validation_summary(
        snapshot.metadata(),
        snapshot.validation(),
    ))
}

pub(super) fn run_trace_loop_cost(args: &DevTraceInputArgs) -> Result<String, String> {
    super::trace_render::render_loop_cost_snapshot(read_trace_snapshot_jsonl_from_path(&args.from)?)
}

pub(super) fn run_trace_explain_local(args: &DevTraceExplainLocalArgs) -> Result<String, String> {
    super::trace_render::render_explain_local_snapshot(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        &args.local,
    )
}

pub(super) fn run_trace_gas_breakdown(args: &DevTraceGasArgs) -> Result<String, String> {
    super::trace_render::render_gas_breakdown_snapshot(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        &args.schedule,
    )
}

pub(super) fn run_trace_explain_pc(args: &DevTracePcArgs) -> Result<String, String> {
    super::trace_render::render_explain_pc_snapshot(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.pc,
    )
}

pub(super) fn run_trace_gas_by_source(args: &DevTraceGasArgs) -> Result<String, String> {
    super::trace_render::render_gas_by_source_snapshot(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        &args.schedule,
    )
}

pub(super) fn run_trace_dynamic_gas_by_source(
    args: &DevTraceDynamicGasArgs,
) -> Result<String, String> {
    super::trace_render::render_dynamic_gas_by_source_snapshot(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.trace_id.clone(),
    )
}

pub(super) fn run_trace_variables_at_pc(args: &DevTracePcArgs) -> Result<String, String> {
    super::trace_render::render_variables_at_pc_snapshot(
        read_trace_snapshot_jsonl_from_path(&args.from)?,
        args.pc,
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
    let module_key = top_mod.name(&db).data(&db).to_string();
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
            super::super::trace_render::render_loop_cost_bundle(bundle.clone())
                .unwrap()
                .contains("Loop cost unavailable from this trace")
        );

        let explain = super::super::trace_render::render_explain_local_bundle(bundle, "b").unwrap();
        assert!(explain.contains("Why b is memory-backed in MIR"));
        assert!(explain.contains("Mir: memory place (MutableLocalLowering)"));
        assert!(!explain.contains("stack slot sp+24"));
    }
}
