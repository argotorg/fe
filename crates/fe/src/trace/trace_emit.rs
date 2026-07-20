use std::fs::{self, File};
use std::io::{BufReader, BufWriter};

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

use crate::{DevTraceEmitArgs, DevTraceInputArgs, TraceReportFormat};

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

    // Key the module by the same file URL that HIR span emission uses as the
    // source-file owner; a filesystem-path owner here would mint a second,
    // divergent source.file identity for the same file.
    let facts = codegen::trace::emit_observable_module_trace_facts(
        &db,
        top_mod,
        file_url.as_str(),
        file_url.as_str(),
        input_path
            .file_name()
            .map_or(input_path.as_str(), |name| name)
            .to_string(),
        &content,
        opt_level,
        None,
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
        && matches!(Config::parse(config_file.text(&db)), Ok(Config::Workspace(_)))
    {
        return Err(format!(
            "{dir_path} is a workspace root; pass a member directory or member name instead"
        ));
    }
    let Some(ingot) = db.workspace().containing_ingot(&db, ingot_url) else {
        return Err(format!("no ingot found at {dir_path}"));
    };
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
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn standalone_file_input_returns_canonical_trace_identity() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!(
            "fe-trace-standalone-canonical-{}-{unique}",
            std::process::id()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("input.fe");
        std::fs::write(&path, "pub contract Demo {}\n").unwrap();
        let relative = Utf8PathBuf::from_path_buf(path).unwrap();

        let (canonical, file_url, content) = standalone_file_input(&relative).unwrap();

        assert!(canonical.is_absolute());
        assert_eq!(file_url.scheme(), "file");
        assert_eq!(content, "pub contract Demo {}\n");
        std::fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn ingot_trace_bundle_covers_all_runtime_modules() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!(
            "fe-trace-ingot-{}-{unique}",
            std::process::id()
        ));
        std::fs::create_dir_all(dir.join("src")).unwrap();
        std::fs::write(
            dir.join("fe.toml"),
            "[ingot]\nname = \"trace_ingot_demo\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(
            dir.join("src/lib.fe"),
            r#"
msg AlphaMsg {
    #[selector = 0x01]
    Get {} -> u32,
}

struct AlphaStore {}

pub contract Alpha {
    store: AlphaStore

    recv AlphaMsg {
        Get {} -> u32 {
            return 1
        }
    }
}
"#,
        )
        .unwrap();
        std::fs::write(
            dir.join("src/beta.fe"),
            r#"
msg BetaMsg {
    #[selector = 0x02]
    Sum { a: u32, b: u32 } -> u32,
}

struct BetaStore {}

pub contract Beta {
    store: BetaStore

    recv BetaMsg {
        Sum { a, b } -> u32 {
            return a + b
        }
    }
}
"#,
        )
        .unwrap();
        let root = Utf8PathBuf::from_path_buf(dir.clone()).unwrap();

        let bundle = emit_real_trace_bundle(&root, false, "dev", codegen::OptLevel::O1).unwrap();
        // The validator is the dedup gate: shared std bodies emitted from both
        // modules must collapse to identical facts, or primary-key uniqueness
        // fails here.
        let summary = TraceValidator::validate(&bundle.facts).unwrap();
        assert!(summary.instruction_count > 0);

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
            runtime_owners.iter().any(|owner| owner.contains("contract:Alpha"))
                && runtime_owners.iter().any(|owner| owner.contains("contract:Beta")),
            "expected runtime code objects for both contracts, got {runtime_owners:?}"
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

        std::fs::remove_dir_all(dir).unwrap();
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

    #[test]
    fn real_trace_bundle_compiles_fib_demo_without_fixture_claims() {
        let path = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../fib_demo.fe");
        let bundle = emit_real_trace_bundle(&path, false, "dev", codegen::OptLevel::O2).unwrap();
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

    fn is_content_digest(value: &str) -> bool {
        let digest = value.strip_prefix("blake3:").unwrap_or(value);
        digest.len() == 64
            && digest.chars().all(|ch| ch.is_ascii_hexdigit())
            && !value.starts_with("fnv64:")
    }
}
