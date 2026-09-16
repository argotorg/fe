use std::io::{BufReader, BufWriter};
use std::{
    collections::HashSet,
    fs::{self, File},
};

use camino::Utf8PathBuf;
use common::{InputDb, config::Config};
use driver::{
    DriverDataBase,
    cli_target::{CliTarget, resolve_cli_target},
};
use salsa::Setter;
use serde::Serialize;
use trace_facts::{
    JsonlTraceReader, JsonlTraceSink, TraceBundle, TraceMetadata, TraceSnapshot,
    TraceValidationReport, TraceValidator,
};
use url::Url;

use crate::{
    DevTraceEmitArgs, DevTraceInputArgs, TraceReportFormat,
    dependency_diagnostics::DependencyIssues,
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
    render_validation_summary_with_format(snapshot.metadata(), snapshot.validation(), args.format)
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
    match target {
        CliTarget::StandaloneFile(file_path) => {
            emit_standalone_trace_bundle(db, &file_path, profile, opt_level)
        }
        CliTarget::Directory(dir_path) => {
            emit_ingot_trace_bundle(db, &dir_path, profile, opt_level)
        }
    }
}

/// Refuse to emit facts for a target the compiler rejects. A bundle asserts
/// `compiler_emitted` provenance, so it must describe bytecode the compiler
/// would actually produce; emitting for source that fails `fe check` would
/// hand consumers PC-to-source mappings for a program that cannot be built.
fn ensure_target_compiles(
    db: &DriverDataBase,
    diagnostics: driver::db::DiagnosticsCollection<'_>,
    mir_diagnostics: impl FnOnce() -> Vec<common::diagnostics::CompleteDiagnostic>,
    label: &str,
) -> Result<(), String> {
    let hir_has_errors = diagnostics.has_errors(db);
    let mir_diagnostics = if hir_has_errors {
        Vec::new()
    } else {
        mir_diagnostics()
    };
    if !hir_has_errors && mir_diagnostics.is_empty() {
        return Ok(());
    }

    if !diagnostics.is_empty() {
        diagnostics.emit(db);
    }
    if !mir_diagnostics.is_empty() {
        db.emit_complete_diagnostics(&mir_diagnostics);
    }
    Err(format!(
        "cannot trace {label}: it does not compile (diagnostics above); \
         trace bundles claim compiler-emitted provenance, so the target must build first"
    ))
}

fn ensure_dependencies_compile(
    db: &DriverDataBase,
    ingot_url: &Url,
    label: &str,
) -> Result<(), String> {
    let mut seen = HashSet::from([ingot_url.clone()]);
    let dependency_issues = DependencyIssues::collect(db, ingot_url, &mut seen);
    if dependency_issues.is_empty() {
        return Ok(());
    }

    eprint!("{}", dependency_issues.format(db));
    Err(format!(
        "cannot trace {label}: its dependencies do not compile (diagnostics above); \
         trace bundles claim compiler-emitted provenance, so the target must build first"
    ))
}

fn emit_standalone_trace_bundle(
    mut db: DriverDataBase,
    input_path: &Utf8PathBuf,
    profile: &str,
    opt_level: codegen::OptLevel,
) -> Result<TraceBundle, String> {
    let (input_path, file_url, content) = standalone_file_input(input_path)?;
    let file = db
        .workspace()
        .update(&mut db, file_url.clone(), content.clone());
    let top_mod = db.top_mod(file);
    ensure_target_compiles(
        &db,
        db.run_on_top_mod(top_mod),
        || db.mir_diagnostics_for_top_mod(top_mod),
        input_path.as_str(),
    )?;

    // Key the module by the same file URL that HIR span emission uses as the
    // source-file owner; a filesystem-path owner here would mint a second,
    // divergent source.file identity for the same file.
    let source_display_name = input_path
        .file_name()
        .map_or(input_path.as_str(), |name| name)
        .to_string();
    let facts = codegen::trace::emit_observable_module_trace_facts(
        &db,
        codegen::trace::ObservableModuleTraceInput {
            top_mod,
            input_owner_key: file_url.as_str(),
            source_uri: file_url.as_str(),
            source_display_name: &source_display_name,
            source_text: &content,
            opt_level,
            contract: None,
        },
    )
    .map_err(|err| format!("failed to emit observable trace facts: {err}"))?;

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

fn emit_ingot_trace_bundle(
    mut db: DriverDataBase,
    dir_path: &Utf8PathBuf,
    profile: &str,
    opt_level: codegen::OptLevel,
) -> Result<TraceBundle, String> {
    let canonical = dir_path
        .canonicalize_utf8()
        .map_err(|err| format!("cannot canonicalize {dir_path}: {err}"))?;
    let ingot_url = Url::from_directory_path(canonical.as_str())
        .map_err(|_| format!("invalid ingot directory path: {dir_path}"))?;
    if driver::init_ingot(&mut db, &ingot_url) {
        return Err(format!(
            "cannot trace {dir_path}: the ingot has resolution errors; run `fe check` for details"
        ));
    }
    if let Some(config_file) = ingot_url
        .join("fe.toml")
        .ok()
        .and_then(|config_url| db.workspace().get(&db, &config_url))
        && matches!(
            Config::parse(config_file.text(&db)),
            Ok(Config::Workspace(_))
        )
    {
        return Err(format!(
            "{dir_path} is a workspace root; pass a member directory or member name instead"
        ));
    }
    let Some(ingot) = db.workspace().containing_ingot(&db, ingot_url.clone()) else {
        return Err(format!("no ingot found at {dir_path}"));
    };
    ensure_target_compiles(
        &db,
        db.run_on_ingot(ingot),
        || db.mir_diagnostics_for_ingot(ingot),
        canonical.as_str(),
    )?;
    ensure_dependencies_compile(&db, &ingot_url, canonical.as_str())?;
    let facts = codegen::trace::emit_observable_ingot_trace_facts(&db, ingot, opt_level)
        .map_err(|err| format!("failed to emit observable trace facts: {err}"))?;
    let metadata = trace_facts::TraceMetadata::compiler_emitted(
        super::compiler_commit(),
        "evm/sonatina",
        vec![
            "fe".to_string(),
            "dev".to_string(),
            "trace".to_string(),
            "emit".to_string(),
        ],
        canonical.as_str(),
        vec![
            format!("profile={profile}"),
            format!("optimize={opt_level}"),
        ],
    );
    Ok(TraceBundle::new(metadata, facts))
}

fn standalone_file_input(file_path: &Utf8PathBuf) -> Result<(Utf8PathBuf, Url, String), String> {
    let canonical = file_path
        .canonicalize_utf8()
        .map_err(|err| format!("cannot canonicalize {file_path}: {err}"))?;
    let file_url = Url::from_file_path(&canonical)
        .map_err(|_| format!("invalid trace input path: {file_path}"))?;
    let content = fs::read_to_string(file_path)
        .map_err(|err| format!("failed to read trace input {file_path}: {err}"))?;
    Ok((canonical, file_url, content))
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

fn render_validation_summary_with_format(
    metadata: &TraceMetadata,
    report: &TraceValidationReport,
    format: TraceReportFormat,
) -> Result<String, String> {
    if format == TraceReportFormat::Json {
        return render_json(&serde_json::json!({
            "metadata": metadata,
            "summary": {
                "fact_count": report.summary.fact_count,
                "node_count": report.summary.node_count,
                "edge_count": report.summary.edge_count,
                "instruction_count": report.summary.instruction_count,
            },
            "diagnostics": {
                "errors": report.error_count(),
                "warnings": report.warning_count(),
                "info": report.info_count(),
            }
        }));
    }
    let data_source = super::format_data_source(metadata);
    Ok(format!(
        "Trace validation: passed\n\
         Data source: {}\n\
         Fact basis: {}\n\
         Report basis: schema validation only; no inference or posthoc attribution.\n\
         Schema version: {}\n\
         Compiler commit: {}\n\
         Target: {}\n\
         Input: {}\n\
         Facts: {}\n\
         Origin nodes: {}\n\
         Origin edges: {}\n\
         Instructions: {}\n\
         Confidence: n/a (schema validation)\n\
         Diagnostics: {} error, {} warning, {} info\n",
        data_source,
        fact_basis_from_data_source(&data_source),
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
    ))
}

fn fact_basis_from_data_source(data_source: &str) -> &'static str {
    if data_source.starts_with("fixture ") {
        "fixture-backed demo facts; not compiler-derived"
    } else if data_source == "compiler_emitted" {
        "compiler-emitted base facts"
    } else {
        "metadata-declared trace facts"
    }
}

fn render_json<T: Serialize>(value: &T) -> Result<String, String> {
    serde_json::to_string_pretty(value)
        .map(|mut json| {
            json.push('\n');
            json
        })
        .map_err(|err| format!("failed to render trace report JSON: {err}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use dir_test::{Fixture, dir_test};

    #[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/trace", glob: "shared_init_runtime.fe")]
    fn shared_init_runtime_function_has_unique_backend_facts(fixture: Fixture<&str>) {
        let path = Utf8PathBuf::from(fixture.path());
        let bundle = emit_real_trace_bundle(&path, true, "dev", codegen::OptLevel::O0).unwrap();
        let snapshot = TraceSnapshot::new(bundle).unwrap();
        // Keep the regression meaningful: both sections must retain their PC
        // links to at least one shared backend instruction after deduplication.
        let mut backend_users = std::collections::BTreeMap::new();
        for fact in snapshot.facts() {
            if let trace_facts::TraceFact::OriginEdge(edge) = fact
                && edge.from.kind() == "bytecode.pc"
                && edge.to.kind() == "evm.vcode.inst"
            {
                backend_users
                    .entry(&edge.to)
                    .or_insert_with(std::collections::BTreeSet::new)
                    .insert(edge.from.owner_key());
            }
        }
        assert!(backend_users.values().any(|users| users.len() == 2));
        let debug = debug_export::DebugBundle::from_snapshot(&snapshot);
        let artifact = debug_export::emit_ethdebug_artifact(&debug).unwrap();
        assert_eq!(artifact.programs.len(), 2);
        assert!(
            artifact
                .programs
                .iter()
                .all(|program| !program.instructions.is_empty())
        );
    }

    #[test]
    fn standalone_file_input_returns_canonical_trace_identity() {
        // This test exercises actual filesystem canonicalization and URL
        // encoding, so it intentionally uses a temporary file, not dir_test.
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("Grüße Contract.fe");
        std::fs::write(&path, "pub contract Demo {}\n").unwrap();
        let input = Utf8PathBuf::from_path_buf(path).unwrap();

        let (canonical, file_url, content) = standalone_file_input(&input).unwrap();

        assert!(canonical.is_absolute());
        assert_eq!(file_url.scheme(), "file");
        // Windows canonicalization may use an extended-length prefix that
        // file URLs do not retain. Compare canonical paths, not spellings.
        assert_eq!(
            file_url.to_file_path().unwrap().canonicalize().unwrap(),
            canonical.as_std_path()
        );
        assert_eq!(content, "pub contract Demo {}\n");
    }

    #[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/trace/packages/multi_module", glob: "fe.toml")]
    fn ingot_trace_bundle_covers_all_runtime_modules(fixture: Fixture<&str>) {
        let root = Utf8PathBuf::from(fixture.path())
            .parent()
            .unwrap()
            .to_path_buf();

        let bundle = emit_real_trace_bundle(&root, false, "dev", codegen::OptLevel::O1).unwrap();
        // The validator is the dedup gate: shared std bodies emitted from both
        // modules must collapse to identical facts, or primary-key uniqueness
        // fails here.
        let summary = TraceValidator::validate(&bundle.facts).unwrap();
        assert!(summary.instruction_count > 0);

        let shared_from_word_functions = bundle
            .facts
            .iter()
            .filter_map(|fact| match fact {
                trace_facts::TraceFact::Function(function)
                    if function.function.owner_key().contains("$fn$from_word$") =>
                {
                    Some(function)
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert!(
            !shared_from_word_functions.is_empty(),
            "expected the shared corelib from_word runtime function"
        );
        assert!(
            shared_from_word_functions
                .iter()
                .all(|function| function.name == "from_word"),
            "runtime function display names must not contain package-local symbol suffixes: {shared_from_word_functions:?}"
        );

        let runtime_owners = bundle
            .facts
            .iter()
            .filter_map(|fact| match fact {
                trace_facts::TraceFact::CodeObject(code_object)
                    if code_object.kind == trace_facts::CodeObjectKind::EvmRuntimeBytecode =>
                {
                    Some(code_object.code_object.owner_key().to_string())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert!(
            runtime_owners
                .iter()
                .any(|owner| owner.contains("contract:Alpha"))
                && runtime_owners
                    .iter()
                    .any(|owner| owner.contains("contract:Beta")),
            "expected runtime code objects for both contracts, got {runtime_owners:?}"
        );

        let creation_owners = bundle
            .facts
            .iter()
            .filter_map(|fact| match fact {
                trace_facts::TraceFact::CodeObject(code_object)
                    if code_object.kind == trace_facts::CodeObjectKind::EvmCreationBytecode =>
                {
                    Some(code_object.code_object.owner_key().to_string())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert!(
            creation_owners
                .iter()
                .any(|owner| owner.contains("contract:Alpha"))
                && creation_owners
                    .iter()
                    .any(|owner| owner.contains("contract:Beta")),
            "expected creation code objects for both contracts, got {creation_owners:?}"
        );

        let source_uris = bundle
            .facts
            .iter()
            .filter_map(|fact| match fact {
                trace_facts::TraceFact::SourceFile(source) => Some(source.uri.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert!(
            source_uris.iter().any(|uri| uri.ends_with("lib.fe"))
                && source_uris.iter().any(|uri| uri.ends_with("beta.fe")),
            "expected both module files in the source registry, got {source_uris:?}"
        );

        let snapshot = TraceSnapshot::new(bundle).unwrap();
        let debug_bundle = debug_export::DebugBundle::from_snapshot(&snapshot);
        let artifact = debug_export::emit_ethdebug_artifact(&debug_bundle).unwrap();
        let environments = artifact
            .programs
            .iter()
            .map(|program| program.environment)
            .collect::<Vec<_>>();
        assert!(environments.contains(&debug_export::EthdebugEnvironment::Call));
        assert!(environments.contains(&debug_export::EthdebugEnvironment::Create));
        let create_program = artifact
            .programs
            .iter()
            .find(|program| {
                program.environment == debug_export::EthdebugEnvironment::Create
                    && program.id.contains("contract:Alpha")
            })
            .expect("creation code must become an ethdebug create program");
        assert!(!create_program.instructions.is_empty());
        assert!(
            create_program
                .instructions
                .iter()
                .any(|instruction| instruction.context.is_some()),
            "the explicit init section should retain source attribution"
        );
    }

    #[test]
    fn trace_emit_uses_shared_observable_fact_builder() {
        let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let repo_root = manifest_dir
            .parent()
            .and_then(std::path::Path::parent)
            .expect("fe crate should live under crates/");
        let file = repo_root.join("crates/fe/src/trace/trace_emit.rs");
        let forbidden = [
            "emit_module_sonatina_bytecode_with_observability_and_trace(",
            "compile_runtime_package_sonatina(",
            "mir::trace::emit_mir_facts(",
            "emit_observed_bytecode_trace_facts(",
        ];

        let source = std::fs::read_to_string(&file)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", file.display()));
        let production = source
            .split("\n#[cfg(test)]")
            .next()
            .expect("split always yields one segment");
        assert!(
            production.contains("emit_observable_module_trace_facts("),
            "{} must use the shared observable trace fact builder",
            file.display()
        );
        for pattern in forbidden {
            assert!(
                !production.contains(pattern),
                "{} must not reassemble observable trace facts outside codegen::trace; found {pattern}",
                file.display()
            );
        }

        let codegen_trace = repo_root.join("crates/codegen/src/trace.rs");
        let source = std::fs::read_to_string(&codegen_trace)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", codegen_trace.display()));
        let production = source
            .split("\n#[cfg(test)]")
            .next()
            .expect("split always yields one segment");
        assert!(
            !production.contains("emit_module_sonatina_bytecode_with_observability_and_trace("),
            "{} must not rebuild module-level bytecode state inside the shared observable builder",
            codegen_trace.display()
        );
        assert!(
            production.contains("select_runtime_package_contract("),
            "{} must apply contract selection once before emitting MIR/Sonatina/bytecode facts",
            codegen_trace.display()
        );
    }

    /// Source lines that bytecode of the given mnemonic claims in `fixture`.
    fn attributed_lines(
        fixture: &Fixture<&str>,
        mnemonic: &str,
    ) -> std::collections::BTreeSet<u32> {
        let path = Utf8PathBuf::from(fixture.path());
        let (_, source_uri, _) = standalone_file_input(&path).unwrap();
        let bundle = emit_real_trace_bundle(&path, false, "dev", codegen::OptLevel::O0).unwrap();
        let snapshot = TraceSnapshot::new(bundle).unwrap();
        let debug = debug_export::DebugBundle::from_snapshot(&snapshot);
        attributed_lines_from_debug(&debug, &source_uri, mnemonic)
    }

    fn attributed_lines_from_debug(
        debug: &debug_export::DebugBundle,
        source_uri: &Url,
        mnemonic: &str,
    ) -> std::collections::BTreeSet<u32> {
        let fixture_files = debug
            .sources
            .iter()
            .filter(|source| source.uri == source_uri.as_str())
            .map(|source| source.file_key.clone())
            .collect::<std::collections::BTreeSet<_>>();
        assert!(
            !fixture_files.is_empty(),
            "fixture source file should be registered in the bundle"
        );
        let spans = debug
            .source_spans
            .iter()
            .map(|span| (span.origin.clone(), span))
            .collect::<std::collections::BTreeMap<_, _>>();

        debug
            .instructions
            .iter()
            .filter(|instruction| instruction.key.kind() == "bytecode.pc")
            .filter(|instruction| instruction.pc_range.is_valid())
            .filter(|instruction| instruction.opcode_or_mnemonic == mnemonic)
            .filter_map(|instruction| {
                let origin = instruction.primary_source.as_ref()?;
                let span = spans.get(origin)?;
                fixture_files
                    .contains(&span.file)
                    .then_some(span.start_line)
            })
            .collect()
    }

    /// A bundle asserts compiler-emitted provenance, so emission must refuse a
    /// target the compiler rejects instead of describing bytecode that cannot
    /// be built.
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/trace", glob: "does_not_compile.fe")]
    fn emission_refuses_targets_that_do_not_compile(fixture: Fixture<&str>) {
        let path = Utf8PathBuf::from(fixture.path());

        let err = emit_real_trace_bundle(&path, false, "dev", codegen::OptLevel::O0)
            .expect_err("emission must refuse a target that does not compile");

        assert!(
            err.contains("does not compile"),
            "error should say the target does not compile, got {err:?}"
        );
    }

    #[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/trace", glob: "standalone_main.fe")]
    fn emission_refuses_standalone_main_for_contract_trace(fixture: Fixture<&str>) {
        let path = Utf8PathBuf::from(fixture.path());

        let err = emit_real_trace_bundle(&path, true, "dev", codegen::OptLevel::O0)
            .expect_err("standalone main must not be presented as contract trace");

        assert!(
            err.contains("standalone main sections"),
            "error should identify the unsupported standalone section, got {err:?}"
        );
    }

    /// `fe check` includes dependency diagnostics, so compiler-emitted trace
    /// provenance must reject an otherwise valid target with a broken
    /// dependency as well.
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/trace/packages/broken_dependency", glob: "fe.toml")]
    fn emission_refuses_targets_with_dependencies_that_do_not_compile(fixture: Fixture<&str>) {
        let app = Utf8PathBuf::from(fixture.path())
            .parent()
            .unwrap()
            .join("ingots/app");

        let err = emit_real_trace_bundle(&app, false, "dev", codegen::OptLevel::O0)
            .expect_err("emission must refuse a target with a broken dependency");

        assert!(
            err.contains("dependencies do not compile"),
            "error should identify the broken dependency, got {err:?}"
        );
    }

    /// 1-based line of the first occurrence of `needle` in a trace fixture.
    fn fixture_line(fixture: &Fixture<&str>, needle: &str) -> u32 {
        fixture
            .content()
            .lines()
            .position(|line| line.contains(needle))
            .map(|index| index as u32 + 1)
            .unwrap_or_else(|| panic!("{} should contain {needle:?}", fixture.path()))
    }

    /// A compiler-generated panic block with exactly one requesting statement
    /// is genuinely that statement's, so it keeps its attribution.
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/trace", glob: "single_panic.fe")]
    fn single_user_panic_blocks_stay_attributed(fixture: Fixture<&str>) {
        let addition = fixture_line(&fixture, "a + b");
        let revert_lines = attributed_lines(&fixture, "REVERT");

        assert!(
            revert_lines.contains(&addition),
            "the sole checked addition (line {addition}) should own its panic block, got {revert_lines:?}"
        );
    }

    /// A compiler-generated panic block is created once and reused by every
    /// checked-arithmetic site in the function. It must not be attributed to
    /// whichever statement happened to create it first: that reports the wrong
    /// source line for every later overflow, over an exact attribution chain.
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/trace", glob: "shared_panic*.fe")]
    fn shared_panic_attribution_survives_optimization_and_request_order(fixture: Fixture<&str>) {
        let path = Utf8PathBuf::from(fixture.path());
        let expected_order = match path.file_name().unwrap() {
            "shared_panic.fe" => ["a + b", "b + c", "a + c"],
            "shared_panic_reordered.fe" => ["a + c", "b + c", "a + b"],
            name => panic!("declare the expected request order for {name}"),
        };
        let lines = expected_order.map(|needle| fixture_line(&fixture, needle));
        assert!(
            lines[0] < lines[1] && lines[1] < lines[2],
            "fixture must exercise its declared request order"
        );
        for level in [codegen::OptLevel::O0, codegen::OptLevel::O2] {
            assert_shared_panic_attribution(&path, level);
        }
    }

    fn assert_shared_panic_attribution(path: &Utf8PathBuf, level: codegen::OptLevel) {
        // Locate the shared overflow path through the actual branch targets
        // of the three authored checks. Other REVERTs belong to ABI helpers
        // and may legitimately retain their own source attribution.
        let (_, source_uri, source_text) = standalone_file_input(path).unwrap();
        let bundle = emit_real_trace_bundle(path, false, "dev", level)
            .expect("shared panic fixture should compile");
        let snapshot = TraceSnapshot::new(bundle).unwrap();
        let debug = debug_export::DebugBundle::from_snapshot(&snapshot);
        let bytecode = debug
            .instructions
            .iter()
            .filter(|instruction| instruction.key.kind() == "bytecode.pc")
            .filter(|instruction| instruction.pc_range.is_valid())
            .collect::<Vec<_>>();
        let fixture_file = &debug
            .sources
            .iter()
            .find(|source| source.uri == source_uri.as_str())
            .expect("fixture source must be recorded")
            .file_key;
        let opcodes = snapshot
            .facts()
            .iter()
            .filter_map(|fact| {
                if let trace_facts::TraceFact::Opcode(opcode) = fact {
                    Some((&opcode.pc, opcode))
                } else {
                    None
                }
            })
            .collect::<std::collections::BTreeMap<_, _>>();
        let addition_lines = ["a + b", "b + c", "a + c"]
            .into_iter()
            .map(|needle| {
                source_text
                    .lines()
                    .position(|line| line.contains(needle))
                    .expect("authored addition must occur in fixture") as u32
                    + 1
            })
            .collect::<std::collections::BTreeSet<_>>();
        assert_eq!(
            addition_lines.len(),
            3,
            "fixture needs distinct authored sites"
        );
        let mut targets = std::collections::BTreeSet::new();
        for line in addition_lines {
            let checks = bytecode
                .iter()
                .filter(|instruction| {
                    instruction.opcode_or_mnemonic == "JUMPI"
                        && instruction.classification
                            == debug_export::InstructionClassification::SourceMapped
                        && instruction.primary_source.as_ref().is_some_and(|origin| {
                            debug.source_spans.iter().any(|span| {
                                &span.origin == origin
                                    && &span.file == fixture_file
                                    && span.start_line == line
                            })
                        })
                })
                .collect::<Vec<_>>();
            assert_eq!(
                checks.len(),
                1,
                "addition on line {line} must own one emitted overflow check"
            );
            let check = checks[0];
            let push = bytecode
                .iter()
                .find(|candidate| {
                    candidate.code_object == check.code_object
                        && candidate.pc_range.end == check.pc_range.start
                })
                .expect("overflow check must have an adjacent target push");
            let opcode = opcodes
                .get(&push.key)
                .expect("target push needs opcode evidence");
            assert!(opcode.opcode.starts_with("PUSH"));
            let target = u32::from_str_radix(
                opcode
                    .immediate
                    .as_deref()
                    .expect("target push needs an immediate")
                    .strip_prefix("0x")
                    .expect("hex target immediate"),
                16,
            )
            .expect("target PC must fit u32");
            targets.insert((
                check.code_object.clone().expect("check needs code object"),
                target,
            ));
        }
        assert_eq!(
            targets.len(),
            1,
            "all three checks must share one actual branch destination"
        );
        let (code_object, target) = targets.into_iter().next().unwrap();
        let mut tail = bytecode
            .iter()
            .copied()
            .filter(|instruction| {
                instruction.code_object.as_ref() == Some(&code_object)
                    && instruction.pc_range.start >= target
            })
            .collect::<Vec<_>>();
        tail.sort_by_key(|instruction| instruction.pc_range.start);
        assert_eq!(tail[0].pc_range.start, target);
        assert_eq!(tail[0].opcode_or_mnemonic, "JUMPDEST");
        let revert_index = tail
            .iter()
            .position(|instruction| instruction.opcode_or_mnemonic == "REVERT")
            .expect("shared destination must end in an emitted REVERT");
        let payload = &tail[1..revert_index];
        assert!(
            !payload.iter().any(|instruction| matches!(
                instruction.opcode_or_mnemonic.as_str(),
                "JUMPDEST" | "JUMP" | "JUMPI" | "STOP" | "RETURN" | "INVALID"
            )),
            "overflow payload must be straight-line code"
        );
        for immediate in ["0x4e487b71", "0x11"] {
            assert!(
                payload.iter().any(|instruction| opcodes
                    .get(&instruction.key)
                    .is_some_and(|opcode| opcode.immediate.as_deref() == Some(immediate))),
                "shared payload must encode Panic(0x11), missing {immediate}"
            );
        }
        let revert = tail[revert_index];
        assert!(
            revert.classification != debug_export::InstructionClassification::SourceMapped
                && revert.primary_source.is_none(),
            "shared overflow REVERT must not select one authored owner: {revert:?}"
        );
    }

    #[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/trace", glob: "fib_demo.fe")]
    fn real_trace_bundle_compiles_fib_demo_without_fixture_claims(fixture: Fixture<&str>) {
        let path = Utf8PathBuf::from(fixture.path());
        let bundle = emit_real_trace_bundle(&path, false, "dev", codegen::OptLevel::O2).unwrap();
        let summary = TraceValidator::validate(&bundle.facts).unwrap();

        assert_eq!(
            bundle.metadata.data_source,
            trace_facts::TraceDataSource::CompilerEmitted
        );
        assert!(summary.instruction_count > 0);
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
            bundle.facts.iter().any(|fact| matches!(
                fact,
                trace_facts::TraceFact::OriginEdge(edge)
                    if matches!(edge.from.kind(), "runtime.stmt" | "runtime.terminator")
                        && matches!(edge.to.kind(), "hir.expr" | "hir.stmt")
                        && edge.label == trace_facts::OriginEdgeLabel::SyntheticFor
                        && edge.traversal_class()
                            == trace_facts::OriginEdgeTraversalClass::Synthetic
            )),
            "generated runtime MIR origins should expose synthetic HIR/source explanation edges"
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
                        && edge.to.kind() == codegen::trace::EVM_VCODE_INST_KIND
                        && edge.label == trace_facts::OriginEdgeLabel::EmittedFrom
            )),
            "bytecode PCs should be linked to EVM VCode instructions"
        );
        assert!(
            bundle.facts.iter().any(|fact| matches!(
                fact,
                trace_facts::TraceFact::OriginEdge(edge)
                    if edge.from.kind() == codegen::trace::EVM_VCODE_INST_KIND
                        && edge.to.kind() == codegen::trace::SONATINA_EVM_PREPARED_INST_KIND
                        && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
            )),
            "EVM VCode instructions should link down to Sonatina EVM prepared instructions"
        );
        assert!(
            !bundle.facts.iter().any(|fact| matches!(
                fact,
                trace_facts::TraceFact::OriginEdge(edge)
                    if edge.from.kind() == "bytecode.pc"
                        && edge.to.kind() == codegen::trace::SONATINA_POSTOPT_INST_KIND
                        && edge.label == trace_facts::OriginEdgeLabel::EmittedFrom
            )),
            "bytecode PCs must not key EVM prepared instruction IDs as post-opt Sonatina IDs"
        );
        assert!(
            !bundle.facts.iter().any(|fact| matches!(
                fact,
                trace_facts::TraceFact::OriginEdge(edge)
                    if edge.from.kind() == "bytecode.pc"
                        && matches!(edge.to.kind(), "runtime.stmt" | "runtime.terminator")
                        && edge.label == trace_facts::OriginEdgeLabel::LoweredFrom
            )),
            "bytecode PCs must not upgrade contextual MIR runtime origins to exact LoweredFrom edges"
        );

        let snapshot = trace_facts::TraceSnapshot::new(bundle.clone()).unwrap();
        let debug = debug_export::DebugBundle::from_snapshot(&snapshot);
        let index = debug_export::ethdebug_origin_attribution_index(&debug).unwrap();
        let gaps = bundle
            .facts
            .iter()
            .filter_map(|fact| match fact {
                trace_facts::TraceFact::AttributionGap(gap) => Some(gap),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert!(
            !gaps.is_empty(),
            "fixture must exercise backend attribution gaps"
        );
        for gap in gaps {
            let exported = index
                .iter()
                .find(|entry| entry.instruction_key == gap.instruction.canonical_storage_key())
                .expect("every emitted gap must survive into the attribution details");
            assert_eq!(
                exported.classification_reason.as_deref(),
                Some(gap.reason.wire_enum_label())
            );
            assert_eq!(
                exported.classification,
                debug_export::InstructionClassification::Unmapped
            );
            assert!(exported.primary_source.is_none());
        }

        // HIR identity is shared per body: no per-instantiation copies. Every
        // HIR source site appears exactly once bundle-wide, and no HIR owner
        // embeds a runtime instance.
        let mut hir_span_sites = std::collections::BTreeSet::new();
        for fact in &bundle.facts {
            if let trace_facts::TraceFact::SourceSpan(span) = fact
                && matches!(span.origin.kind(), "hir.expr" | "hir.stmt")
            {
                assert!(
                    !span.origin.owner_key().contains("runtime-instance"),
                    "HIR owner must not embed a runtime instance: {}",
                    span.origin.owner_key()
                );
                assert!(
                    hir_span_sites.insert((
                        span.file.clone(),
                        span.start_byte,
                        span.end_byte,
                        span.origin.kind().to_string(),
                        span.origin.local_key().to_string(),
                    )),
                    "HIR source site emitted more than once (per-instantiation duplication): {} bytes {}..{}",
                    span.origin.display_label(),
                    span.start_byte,
                    span.end_byte
                );
            }
        }
    }

    #[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/trace", glob: "source_marker.fe")]
    fn real_trace_maps_unique_literal_to_its_emitted_push(fixture: Fixture<&str>) {
        let marker = "0x123456789abcde";
        let path = Utf8PathBuf::from(fixture.path());
        let (_, source_uri, source) = standalone_file_input(&path).unwrap();
        let marker_start = source
            .find(marker)
            .expect("source marker should be present") as u32;
        let marker_end = marker_start + marker.len() as u32;
        let bundle = emit_real_trace_bundle(&path, false, "dev", codegen::OptLevel::O2)
            .expect("source marker fixture should compile");
        let snapshot = TraceSnapshot::new(bundle).unwrap();
        let debug = debug_export::DebugBundle::from_snapshot(&snapshot);
        let authored_files = debug
            .sources
            .iter()
            .filter(|source| source.uri == source_uri.as_str())
            .map(|source| source.file_key.clone())
            .collect::<std::collections::BTreeSet<_>>();
        assert_eq!(
            authored_files.len(),
            1,
            "fixture must have one authored source file"
        );
        let spans = debug
            .source_spans
            .iter()
            .map(|span| (span.origin.clone(), span))
            .collect::<std::collections::BTreeMap<_, _>>();

        let matches = debug.instructions.iter().filter(|instruction| {
            instruction.opcode_or_mnemonic == "PUSH7"
                && instruction.pc_range.end > instruction.pc_range.start
                && instruction.classification
                    == debug_export::InstructionClassification::SourceMapped
                && instruction.confidence == debug_export::AttributionConfidence::High
                && instruction.primary_source.as_ref().is_some_and(|origin| {
                    spans.get(origin).is_some_and(|span| {
                        authored_files.contains(&span.file)
                            && (span.start_byte != 0 || span.end_byte != source.len() as u32)
                            && span.start_byte <= marker_start
                            && marker_end <= span.end_byte
                    })
                })
                && snapshot.facts().iter().any(|fact| {
                    matches!(
                        fact,
                        trace_facts::TraceFact::Opcode(opcode)
                            if opcode.pc == instruction.key
                                && opcode.opcode == "PUSH7"
                                && opcode.immediate.as_deref() == Some(marker)
                    )
                })
        });
        assert!(
            matches.count() >= 1,
            "the unique literal must own a nonzero high-confidence PUSH7 range"
        );
    }

    fn is_content_digest(value: &str) -> bool {
        let digest = value.strip_prefix("blake3:").unwrap_or(value);
        digest.len() == 64
            && digest.chars().all(|ch| ch.is_ascii_hexdigit())
            && !value.starts_with("fnv64:")
    }
}
