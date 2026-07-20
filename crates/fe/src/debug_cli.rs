use std::fs;

use debug_export::{
    DebugBundle, ETHDEBUG_SCHEMA_VERSION, EthdebugArtifact, emit_ethdebug_artifact,
    validate_ethdebug_artifact,
};
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{DebugExportFormat, DevDebugCommand, DevDebugEmitArgs, DevDebugValidateArgs};

pub(crate) fn run_debug_command(command: &DevDebugCommand) -> Result<String, String> {
    match command {
        DevDebugCommand::Emit(args) => run_debug_emit(args),
        DevDebugCommand::Validate(args) => run_debug_validate(args),
    }
}

fn run_debug_emit(args: &DevDebugEmitArgs) -> Result<String, String> {
    let snapshot = crate::trace::read_trace_snapshot_jsonl_from_path(&args.from)?;
    let bundle = DebugBundle::from_snapshot(&snapshot);
    match args.format {
        DebugExportFormat::Ethdebug => {
            ensure_ethdebug_schema(&args.schema_version)?;
            let phase = ensure_ethdebug_phase(args.phase.as_deref())?;
            let artifact = emit_ethdebug_artifact(&bundle)?;
            // Serialize once and hash the exact bytes that land on disk, so
            // the sidecar hash is reproducible by any tool from the file
            // alone and detects any byte-level tampering.
            let artifact_text = render_json_text(&args.out, &artifact)?;
            let artifact_hash = blake3_hash_label(artifact_text.as_bytes());
            let sidecar = args
                .sidecar
                .as_ref()
                .map(|_| ethdebug_sidecar(&bundle, &artifact, artifact_hash.clone()))
                .transpose()?;
            write_text_file(&args.out, &artifact_text)?;
            if let (Some(sidecar_path), Some(sidecar)) = (&args.sidecar, sidecar)
                && let Err(err) = write_json_file(sidecar_path, &sidecar)
            {
                return Err(format!(
                    "{err} (the ethdebug artifact was already written to {})",
                    args.out
                ));
            }
            Ok(format!(
                "wrote ethdebug instruction/source artifact: {}\n\
                 Data source: {}\n\
                 Trace hash: {}\n\
                 Schema: {}\n\
                 Phase: {}\n\
                 Programs: {}\n\
                 Note: artifact is a derived view over DebugBundle; Fe origin/confidence details stay in the optional sidecar.\n",
                args.out,
                crate::trace::format_data_source(snapshot.metadata()),
                bundle.trace_hash,
                ETHDEBUG_SCHEMA_VERSION,
                phase,
                artifact.programs.len(),
            ))
        }
    }
}

fn run_debug_validate(args: &DevDebugValidateArgs) -> Result<String, String> {
    match args.format {
        DebugExportFormat::Ethdebug => {
            let outcome = validate_ethdebug_input(args);
            let verification = match &outcome {
                Ok(report) => json!({
                    "format": "ethdebug",
                    "status": "ok",
                    "schema_version": report.schema_version,
                    "program_count": report.program_count,
                    "sidecar_checked": report.sidecar_checked,
                }),
                Err(err) => json!({
                    "format": "ethdebug",
                    "status": "failed",
                    "error": err,
                }),
            };
            write_validation_json(args.verify_json.as_ref(), verification)?;
            let report = outcome?;
            Ok(format!(
                "ethdebug validation passed: {}\nPrograms: {}\nSidecar checked: {}\n",
                args.input, report.program_count, report.sidecar_checked,
            ))
        }
    }
}

struct EthdebugValidationReport {
    schema_version: String,
    program_count: usize,
    sidecar_checked: bool,
}

fn validate_ethdebug_input(
    args: &DevDebugValidateArgs,
) -> Result<EthdebugValidationReport, String> {
    ensure_ethdebug_schema(&args.schema_version)?;
    let artifact_text = fs::read_to_string(args.input.as_std_path())
        .map_err(|err| format!("failed to read {}: {err}", args.input))?;
    let artifact: EthdebugArtifact = serde_json::from_str(&artifact_text)
        .map_err(|err| format!("failed to parse {}: {err}", args.input))?;
    validate_ethdebug_artifact(&artifact)?;
    let mut sidecar_checked = false;
    if let Some(sidecar_path) = &args.sidecar {
        validate_sidecar(
            sidecar_path,
            &artifact,
            &blake3_hash_label(artifact_text.as_bytes()),
        )?;
        sidecar_checked = true;
    }
    Ok(EthdebugValidationReport {
        schema_version: artifact.schema_version,
        program_count: artifact.programs.len(),
        sidecar_checked,
    })
}

fn ensure_ethdebug_schema(value: &str) -> Result<(), String> {
    if value == "pinned" || value == ETHDEBUG_SCHEMA_VERSION {
        Ok(())
    } else {
        Err(format!(
            "unsupported ethdebug schema version {value}; expected `pinned` or {ETHDEBUG_SCHEMA_VERSION}"
        ))
    }
}

fn ensure_ethdebug_phase(value: Option<&str>) -> Result<&'static str, String> {
    match value.unwrap_or("instruction-source") {
        "instruction-source" => Ok("instruction-source"),
        other => Err(format!(
            "unsupported ethdebug phase {other}; this wrapper currently emits instruction-source only"
        )),
    }
}

fn render_json_text<T: Serialize>(path: &camino::Utf8Path, value: &T) -> Result<String, String> {
    serde_json::to_string_pretty(value)
        .map(|mut json| {
            json.push('\n');
            json
        })
        .map_err(|err| format!("failed to render JSON for {path}: {err}"))
}

fn write_text_file(path: &camino::Utf8Path, text: &str) -> Result<(), String> {
    if let Some(parent) = path.parent()
        && !parent.as_str().is_empty()
    {
        fs::create_dir_all(parent.as_std_path())
            .map_err(|err| format!("failed to create {parent}: {err}"))?;
    }
    fs::write(path.as_std_path(), text).map_err(|err| format!("failed to write {path}: {err}"))
}

fn write_json_file<T: Serialize>(path: &camino::Utf8Path, value: &T) -> Result<(), String> {
    write_text_file(path, &render_json_text(path, value)?)
}

fn blake3_hash_label(bytes: &[u8]) -> String {
    format!("blake3:{}", blake3::hash(bytes).to_hex())
}

fn read_json_file<T>(path: &camino::Utf8Path) -> Result<T, String>
where
    T: for<'de> Deserialize<'de>,
{
    let text = fs::read_to_string(path.as_std_path())
        .map_err(|err| format!("failed to read {path}: {err}"))?;
    serde_json::from_str(&text).map_err(|err| format!("failed to parse {path}: {err}"))
}

fn write_validation_json(
    path: Option<&camino::Utf8PathBuf>,
    value: serde_json::Value,
) -> Result<(), String> {
    if let Some(path) = path {
        write_json_file(path, &value)?;
    }
    Ok(())
}

fn ethdebug_sidecar(
    bundle: &DebugBundle,
    artifact: &EthdebugArtifact,
    artifact_file_hash: String,
) -> Result<EthdebugSidecar, String> {
    Ok(EthdebugSidecar {
        schema_version: "fe-ethdebug-origin-sidecar-v1".to_string(),
        trace_hash: bundle.trace_hash.clone(),
        ethdebug_schema_version: artifact.schema_version.clone(),
        ethdebug_artifact_hash: artifact_file_hash,
        instruction_origin_index: bundle
            .instructions
            .iter()
            .map(|instruction| SidecarInstruction {
                instruction_key: instruction.key.canonical_storage_key(),
                code_object: instruction
                    .code_object
                    .as_ref()
                    .map(|key| key.canonical_storage_key()),
                pc_start: instruction.pc_range.start,
                pc_end: instruction.pc_range.end,
                primary_source: instruction
                    .primary_source
                    .as_ref()
                    .map(|key| key.canonical_storage_key()),
                all_origins: instruction
                    .all_origins
                    .iter()
                    .map(|key| key.canonical_storage_key())
                    .collect(),
                classification: wire_enum_label(&instruction.classification),
                confidence: wire_enum_label(&instruction.confidence),
            })
            .collect(),
    })
}

fn validate_sidecar(
    path: &camino::Utf8Path,
    artifact: &EthdebugArtifact,
    artifact_file_hash: &str,
) -> Result<(), String> {
    let sidecar = read_json_file::<EthdebugSidecar>(path)?;
    if sidecar.schema_version != "fe-ethdebug-origin-sidecar-v1" {
        return Err(format!(
            "unsupported ethdebug sidecar schema version {}",
            sidecar.schema_version
        ));
    }
    if sidecar.ethdebug_schema_version != artifact.schema_version {
        return Err(format!(
            "ethdebug sidecar schema {} does not match artifact schema {}",
            sidecar.ethdebug_schema_version, artifact.schema_version
        ));
    }
    if sidecar.ethdebug_artifact_hash != artifact_file_hash {
        return Err(format!(
            "ethdebug sidecar artifact hash {} does not match the artifact file hash {}",
            sidecar.ethdebug_artifact_hash, artifact_file_hash
        ));
    }
    Ok(())
}

fn wire_enum_label<T: Serialize + std::fmt::Debug>(value: &T) -> String {
    serde_json::to_value(value)
        .ok()
        .and_then(|rendered| rendered.as_str().map(str::to_string))
        .unwrap_or_else(|| format!("{value:?}"))
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct EthdebugSidecar {
    schema_version: String,
    trace_hash: String,
    ethdebug_schema_version: String,
    ethdebug_artifact_hash: String,
    instruction_origin_index: Vec<SidecarInstruction>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct SidecarInstruction {
    instruction_key: String,
    code_object: Option<String>,
    pc_start: u32,
    pc_end: u32,
    primary_source: Option<String>,
    all_origins: Vec<String>,
    classification: String,
    confidence: String,
}

#[cfg(test)]
mod tests {
    use std::fs;

    use camino::Utf8PathBuf;
    use common::origin::OriginExportKey;
    use tempfile::tempdir;
    use trace_facts::{
        CodeObjectFact, CodeObjectKind, CompilerPhase, FunctionFact, InstructionExtentFact,
        InstructionFact, JsonlTraceSink, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact,
        OriginNodeKind, PcRange, SourceFileFact, SourceSpanFact, TraceBundle, TraceFact,
        TraceMetadata,
    };

    use super::*;

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(key: OriginExportKey) -> TraceFact {
        TraceFact::OriginNode(OriginNodeFact::new(
            key.clone(),
            OriginNodeKind::new(key.kind()),
        ))
    }

    fn write_debug_trace(path: &camino::Utf8Path) {
        let source_file = key("source.file", "demo", "demo.fe");
        let source_expr = key("hir.expr", "demo", "expr:add");
        let code_object = key("code.object", "demo", "runtime");
        let function = key("bytecode.function", "demo", "runtime");
        let instruction = key("bytecode.pc", "demo", "pc:4");
        let bundle = TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "dev".to_string(), "trace".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![
                node(source_file.clone()),
                node(source_expr.clone()),
                node(code_object.clone()),
                node(function.clone()),
                node(instruction.clone()),
                TraceFact::SourceFile(SourceFileFact::new(
                    source_file.clone(),
                    "file:///demo.fe",
                    "demo.fe",
                    "blake3:0000000000000000000000000000000000000000000000000000000000000001",
                    Some(0),
                )),
                TraceFact::SourceSpan(SourceSpanFact::new(
                    source_expr.clone(),
                    source_file,
                    10,
                    13,
                    2,
                    3,
                    2,
                    6,
                )),
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
                    Some(source_expr.clone()),
                    Some(code_object.clone()),
                )),
                TraceFact::Instruction(InstructionFact::new(
                    instruction.clone(),
                    function,
                    0,
                    "ADD",
                )),
                TraceFact::InstructionExtent(InstructionExtentFact::new(
                    instruction.clone(),
                    code_object,
                    PcRange::new(4, 5),
                    1,
                )),
                TraceFact::OriginEdge(OriginEdgeFact::new(
                    instruction,
                    source_expr,
                    OriginEdgeLabel::EmittedFrom,
                    Some(CompilerPhase::BytecodeEmission),
                )),
            ],
        );
        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_bundle(&bundle).unwrap();
        fs::write(path.as_std_path(), sink.into_inner()).unwrap();
    }

    #[test]
    fn ethdebug_emit_writes_artifact_and_sidecar_then_validates() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        let sidecar = Utf8PathBuf::from_path_buf(temp.path().join("debug.sidecar.json")).unwrap();
        write_debug_trace(&trace_path);

        let output = run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            sidecar: Some(sidecar.clone()),
        })
        .unwrap();

        assert!(output.contains("derived view over DebugBundle"));
        assert!(output.contains("Phase: instruction-source"));
        assert!(out.exists());
        assert!(sidecar.exists());
        let validation = run_debug_validate(&DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out,
            schema_version: "pinned".to_string(),
            sidecar: Some(sidecar),
            verify_json: None,
        })
        .unwrap();
        assert!(validation.contains("validation passed"));
    }

    #[test]
    fn sidecar_detects_artifact_file_tampering() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        let sidecar = Utf8PathBuf::from_path_buf(temp.path().join("debug.sidecar.json")).unwrap();
        write_debug_trace(&trace_path);
        run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            sidecar: Some(sidecar.clone()),
        })
        .unwrap();

        // Inject a field serde would silently ignore; the file-bytes hash
        // must still catch it.
        let tampered = fs::read_to_string(out.as_std_path())
            .unwrap()
            .replacen('{', "{\"injected_garbage\":\"x\",", 1);
        fs::write(out.as_std_path(), tampered).unwrap();

        let err = run_debug_validate(&DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out,
            schema_version: "pinned".to_string(),
            sidecar: Some(sidecar),
            verify_json: None,
        })
        .unwrap_err();
        assert!(err.contains("does not match the artifact file hash"));
    }

    #[test]
    fn ethdebug_schema_is_pinned() {
        let err = ensure_ethdebug_schema("future").unwrap_err();

        assert!(err.contains("unsupported ethdebug schema version"));
    }

    #[test]
    fn debug_emit_rejects_unimplemented_surfaces() {
        let err = ensure_ethdebug_phase(Some("types")).unwrap_err();
        assert!(err.contains("instruction-source only"));
    }
}
