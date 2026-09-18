use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
};

use common::origin::OriginExportKey;
use debug_export::{
    DebugBundle, ETHDEBUG_FALLBACK_PROGRAM_ID, ETHDEBUG_SCHEMA_VERSION, EthdebugArtifact,
    EthdebugOriginAttribution, emit_ethdebug_artifact, ethdebug_origin_attribution_hash,
    ethdebug_origin_attribution_index, validate_ethdebug_artifact,
};
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{DebugExportFormat, DevDebugCommand, DevDebugEmitArgs, DevDebugValidateArgs};

mod attribution_summary;

const ETHDEBUG_ATTRIBUTION_DETAILS_SCHEMA_VERSION: &str = "fe-ethdebug-attribution-details-v1";

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
            let summary = attribution_summary::render(&bundle, &artifact)?;
            // Serialize once and hash the exact bytes that land on disk. This
            // detects artifact/attribution details mismatches and uncoordinated changes;
            // it is a consistency check, not an authenticity guarantee.
            let artifact_text = render_json_text(&args.out, &artifact)?;
            let artifact_hash = blake3_hash_label(artifact_text.as_bytes());
            let attribution_details = args
                .attribution_details
                .as_ref()
                .map(|_| ethdebug_attribution_details(&bundle, &artifact, artifact_hash.clone()))
                .transpose()?;
            write_text_file(&args.out, &artifact_text)?;
            if let (Some(attribution_details_path), Some(attribution_details)) =
                (&args.attribution_details, attribution_details)
                && let Err(err) = write_json_file(attribution_details_path, &attribution_details)
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
                 Note: artifact is a Fe-specific experimental view over DebugBundle, not a stock ethdebug program document. Optional Fe attribution details have no compatibility guarantee.\n{}",
                args.out,
                crate::trace::format_data_source(snapshot.metadata()),
                bundle.trace_hash,
                ETHDEBUG_SCHEMA_VERSION,
                phase,
                artifact.programs.len(),
                summary,
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
                    "attribution_details_consistency_checked": report.attribution_details_consistency_checked,
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
                "Fe-specific ethdebug view validation passed: {}\nPrograms: {}\nAttribution details consistency checked: {}\nNote: this validates the pinned Fe view schema, not upstream ethdebug conformance.\n",
                args.input, report.program_count, report.attribution_details_consistency_checked,
            ))
        }
    }
}

struct EthdebugValidationReport {
    schema_version: String,
    program_count: usize,
    attribution_details_consistency_checked: bool,
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
    let mut attribution_details_consistency_checked = false;
    if let Some(attribution_details_path) = &args.attribution_details {
        validate_attribution_details(
            attribution_details_path,
            &artifact,
            &blake3_hash_label(artifact_text.as_bytes()),
        )?;
        attribution_details_consistency_checked = true;
    }
    Ok(EthdebugValidationReport {
        schema_version: artifact.schema_version,
        program_count: artifact.programs.len(),
        attribution_details_consistency_checked,
    })
}

fn ensure_ethdebug_schema(value: &str) -> Result<(), String> {
    if value == "pinned" || value == ETHDEBUG_SCHEMA_VERSION {
        Ok(())
    } else {
        Err(format!(
            "unsupported Fe-specific ethdebug view schema version {value}; expected `pinned` or {ETHDEBUG_SCHEMA_VERSION}"
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

fn ethdebug_attribution_details(
    bundle: &DebugBundle,
    artifact: &EthdebugArtifact,
    artifact_file_hash: String,
) -> Result<EthdebugAttributionDetails, String> {
    Ok(EthdebugAttributionDetails {
        schema_version: ETHDEBUG_ATTRIBUTION_DETAILS_SCHEMA_VERSION.to_string(),
        trace_hash: bundle.trace_hash.clone(),
        ethdebug_schema_version: artifact.schema_version.clone(),
        ethdebug_artifact_hash: artifact_file_hash,
        instruction_origin_index: ethdebug_origin_attribution_index(bundle)?,
    })
}

fn validate_attribution_details(
    path: &camino::Utf8Path,
    artifact: &EthdebugArtifact,
    artifact_file_hash: &str,
) -> Result<(), String> {
    let attribution_details = read_json_file::<EthdebugAttributionDetails>(path)?;
    if attribution_details.schema_version != ETHDEBUG_ATTRIBUTION_DETAILS_SCHEMA_VERSION {
        return Err(format!(
            "unsupported ethdebug attribution details schema version {}",
            attribution_details.schema_version
        ));
    }
    if attribution_details.ethdebug_schema_version != artifact.schema_version {
        return Err(format!(
            "ethdebug attribution details schema {} does not match artifact schema {}",
            attribution_details.ethdebug_schema_version, artifact.schema_version
        ));
    }
    if attribution_details.ethdebug_artifact_hash != artifact_file_hash {
        return Err(format!(
            "ethdebug attribution details artifact hash {} does not match the artifact file hash {}",
            attribution_details.ethdebug_artifact_hash, artifact_file_hash
        ));
    }
    if attribution_details.trace_hash != artifact.compilation.id {
        return Err(format!(
            "ethdebug attribution details trace hash {} does not match artifact compilation id {}",
            attribution_details.trace_hash, artifact.compilation.id
        ));
    }
    let attribution_hash =
        ethdebug_origin_attribution_hash(&attribution_details.instruction_origin_index);
    if attribution_hash != artifact.compilation.fe_origin_attribution_hash {
        return Err(format!(
            "ethdebug attribution details origin attribution hash {attribution_hash} does not match artifact attribution hash {}",
            artifact.compilation.fe_origin_attribution_hash
        ));
    }

    let mut artifact_instructions = BTreeMap::new();
    for program in &artifact.programs {
        for instruction in &program.instructions {
            if artifact_instructions
                .insert(
                    instruction.fe_origin_key.as_str(),
                    (program.id.as_str(), instruction),
                )
                .is_some()
            {
                return Err(format!(
                    "duplicate ethdebug artifact instruction key {}",
                    instruction.fe_origin_key
                ));
            }
        }
    }

    let mut attribution_details_keys = BTreeSet::new();
    for instruction in &attribution_details.instruction_origin_index {
        parse_attribution_details_origin_key("instruction_key", &instruction.instruction_key)?;
        if !attribution_details_keys.insert(instruction.instruction_key.as_str()) {
            return Err(format!(
                "duplicate ethdebug attribution details instruction key {}",
                instruction.instruction_key
            ));
        }
        if instruction.pc_start >= instruction.pc_end {
            return Err(format!(
                "ethdebug attribution details instruction {} has invalid PC range {}..{}",
                instruction.instruction_key, instruction.pc_start, instruction.pc_end
            ));
        }
        if let Some(code_object) = &instruction.code_object {
            parse_attribution_details_origin_key("code_object", code_object)?;
        }
        if let Some(primary_source) = &instruction.primary_source {
            parse_attribution_details_origin_key("primary_source", primary_source)?;
        }
        let mut origins = BTreeSet::new();
        for origin in &instruction.all_origins {
            parse_attribution_details_origin_key("all_origins", origin)?;
            if !origins.insert(origin) {
                return Err(format!(
                    "ethdebug attribution details instruction {} has duplicate origin {}",
                    instruction.instruction_key, origin
                ));
            }
        }
        let valid_attribution = matches!(
            (instruction.classification, instruction.confidence),
            (
                debug_export::InstructionClassification::SourceMapped,
                debug_export::AttributionConfidence::High
            ) | (
                debug_export::InstructionClassification::Ambiguous,
                debug_export::AttributionConfidence::Ambiguous
            ) | (
                debug_export::InstructionClassification::Synthetic
                    | debug_export::InstructionClassification::Unmapped,
                debug_export::AttributionConfidence::Unmapped
            )
        );
        if !instruction.has_consistent_reason() {
            return Err(format!(
                "ethdebug attribution details instruction {} has inconsistent classification/reason",
                instruction.instruction_key
            ));
        }
        if !valid_attribution {
            return Err(format!(
                "ethdebug attribution details instruction {} has inconsistent classification/confidence",
                instruction.instruction_key
            ));
        }
        let source_mapped =
            instruction.classification == debug_export::InstructionClassification::SourceMapped;
        if source_mapped != instruction.primary_source.is_some() {
            return Err(format!(
                "ethdebug attribution details instruction {} has primary source inconsistent with classification",
                instruction.instruction_key
            ));
        }
        if let Some(primary_source) = &instruction.primary_source
            && !origins.contains(primary_source)
        {
            return Err(format!(
                "ethdebug attribution details instruction {} primary source {} is absent from all_origins",
                instruction.instruction_key, primary_source
            ));
        }

        let Some((program_id, artifact_instruction)) =
            artifact_instructions.remove(instruction.instruction_key.as_str())
        else {
            return Err(format!(
                "ethdebug attribution details instruction {} has no artifact instruction",
                instruction.instruction_key
            ));
        };
        let code_object_matches = match instruction.code_object.as_deref() {
            Some(code_object) => code_object == program_id,
            None => program_id == ETHDEBUG_FALLBACK_PROGRAM_ID,
        };
        if !code_object_matches
            || instruction.pc_start != artifact_instruction.offset
            || instruction.classification != artifact_instruction.classification
            || instruction.confidence != artifact_instruction.confidence
            || instruction.primary_source.is_some() != artifact_instruction.context.is_some()
        {
            return Err(format!(
                "ethdebug attribution details instruction {} does not match its artifact instruction",
                instruction.instruction_key
            ));
        }
    }
    if let Some((missing, _)) = artifact_instructions.into_iter().next() {
        return Err(format!(
            "ethdebug attribution details is missing artifact instruction {missing}"
        ));
    }
    Ok(())
}

fn parse_attribution_details_origin_key(
    field: &str,
    value: &str,
) -> Result<OriginExportKey, String> {
    OriginExportKey::parse_canonical_storage_key(value)
        .map_err(|err| format!("invalid ethdebug attribution details {field} {value:?}: {err}"))
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct EthdebugAttributionDetails {
    schema_version: String,
    trace_hash: String,
    ethdebug_schema_version: String,
    ethdebug_artifact_hash: String,
    instruction_origin_index: Vec<EthdebugOriginAttribution>,
}

#[cfg(test)]
mod tests {
    use std::fs;

    use camino::Utf8PathBuf;
    use common::origin::OriginExportKey;
    use tempfile::tempdir;
    use trace_facts::{
        CodeObjectFact, CodeObjectKind, FunctionFact, InstructionExtentFact, InstructionFact,
        JsonlTraceSink, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, OriginNodeKind, PcRange,
        SourceFileFact, SourceSpanFact, TraceBundle, TraceFact, TraceMetadata,
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
        let contract = key("bytecode.contract", "demo", "Demo");
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
                node(contract.clone()),
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
                    Some(contract),
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
                    None,
                )),
            ],
        );
        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_bundle(&bundle).unwrap();
        fs::write(path.as_std_path(), sink.into_inner()).unwrap();
    }
    fn write_debug_trace_without_code_object_fact(path: &camino::Utf8Path) {
        write_debug_trace(path);
        let mut bundle = crate::trace::read_trace_snapshot_jsonl_from_path(&path.to_path_buf())
            .unwrap()
            .into_bundle();
        bundle
            .facts
            .retain(|fact| !matches!(fact, TraceFact::CodeObject(_)));
        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_bundle(&bundle).unwrap();
        fs::write(path.as_std_path(), sink.into_inner()).unwrap();
    }

    #[test]
    fn ethdebug_summary_does_not_require_attribution_details() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        write_debug_trace(&trace_path);
        let output = run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path.clone(),
            out,
            schema_version: "pinned".to_string(),
            phase: None,
            attribution_details: None,
        })
        .unwrap();
        assert!(output.contains("Attribution:"));
        for label in ["Unique exact source", "Total instructions"] {
            assert!(
                output
                    .lines()
                    .any(|line| line.trim_start().starts_with(label)
                        && line.split_whitespace().last() == Some("1"))
            );
        }
        assert!(output.contains("counts do not measure complete source coverage"));
        assert!(!output.contains("Why source attribution is missing"));
        assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 2);

        let snapshot = crate::trace::read_trace_snapshot_jsonl_from_path(&trace_path).unwrap();
        let mut bundle = DebugBundle::from_snapshot(&snapshot);
        bundle.instructions[0].classification = debug_export::InstructionClassification::Unmapped;
        bundle.instructions[0].confidence = debug_export::AttributionConfidence::Unmapped;
        bundle.instructions[0].primary_source = None;
        bundle.instructions[0].classification_reason = Some("missing_provenance".to_string());
        let artifact = emit_ethdebug_artifact(&bundle).unwrap();
        let summary = attribution_summary::render(&bundle, &artifact).unwrap();
        assert!(summary.lines().any(
            |line| line.trim_start().starts_with("No source attribution")
                && line.split_whitespace().last() == Some("1")
        ));
        assert!(summary.contains("missing_provenance: 1"));
    }

    #[test]
    fn ethdebug_emit_writes_artifact_and_attribution_details_then_validates() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        let attribution_details =
            Utf8PathBuf::from_path_buf(temp.path().join("debug.attribution-details.json")).unwrap();
        write_debug_trace(&trace_path);

        let output = run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            attribution_details: Some(attribution_details.clone()),
        })
        .unwrap();

        assert!(output.contains("view over DebugBundle"));
        assert!(output.contains("Fe-specific experimental"));
        assert!(output.contains("not a stock ethdebug program document"));
        assert!(output.contains("attribution details have no compatibility guarantee"));
        assert!(output.contains("Phase: instruction-source"));
        assert!(out.exists());
        assert!(attribution_details.exists());
        let validation = run_debug_validate(&DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out,
            schema_version: "pinned".to_string(),
            attribution_details: Some(attribution_details),
            verify_json: None,
        })
        .unwrap();
        assert!(validation.contains("validation passed"));
    }

    #[test]
    fn compiler_trace_ethdebug_attribution_details_roundtrip() {
        let temp = tempdir().unwrap();
        let fixture =
            Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/trace/fib_demo.fe");
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        let attribution_details_path =
            Utf8PathBuf::from_path_buf(temp.path().join("debug.attribution-details.json")).unwrap();

        crate::trace::run_dev_command(&crate::DevCommand::Trace {
            command: crate::DevTraceCommand::Emit(crate::DevTraceEmitArgs {
                path: fixture,
                out: trace_path.clone(),
                standalone: true,
                profile: "dev".to_string(),
                optimize: "1".to_string(),
            }),
        })
        .unwrap();
        run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            attribution_details: Some(attribution_details_path.clone()),
        })
        .unwrap();

        let artifact: EthdebugArtifact = read_json_file(&out).unwrap();
        let attribution_details: EthdebugAttributionDetails =
            read_json_file(&attribution_details_path).unwrap();
        let artifact_instruction_count = artifact
            .programs
            .iter()
            .map(|program| program.instructions.len())
            .sum::<usize>();
        assert_eq!(
            artifact_instruction_count,
            attribution_details.instruction_origin_index.len()
        );
        assert!(
            attribution_details
                .instruction_origin_index
                .iter()
                .all(|instruction| {
                    OriginExportKey::parse_canonical_storage_key(&instruction.instruction_key)
                        .is_ok_and(|key| key.kind() == "bytecode.pc")
                })
        );
        run_debug_validate(&DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out,
            schema_version: "pinned".to_string(),
            attribution_details: Some(attribution_details_path),
            verify_json: None,
        })
        .unwrap();
    }

    #[test]
    fn attribution_details_detects_artifact_file_mismatch() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        let attribution_details =
            Utf8PathBuf::from_path_buf(temp.path().join("debug.attribution-details.json")).unwrap();
        write_debug_trace(&trace_path);
        run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            attribution_details: Some(attribution_details.clone()),
        })
        .unwrap();

        let mut artifact: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(out.as_std_path()).unwrap()).unwrap();
        artifact["programs"][0]["contract"]["name"] =
            serde_json::Value::String("modified".to_string());
        fs::write(
            out.as_std_path(),
            serde_json::to_string_pretty(&artifact).unwrap(),
        )
        .unwrap();

        let err = run_debug_validate(&DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out,
            schema_version: "pinned".to_string(),
            attribution_details: Some(attribution_details),
            verify_json: None,
        })
        .unwrap_err();
        assert!(err.contains("does not match the artifact file hash"));
    }

    #[test]
    fn ethdebug_fallback_artifact_and_attribution_details_roundtrip() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        let attribution_details_path =
            Utf8PathBuf::from_path_buf(temp.path().join("debug.attribution-details.json")).unwrap();
        write_debug_trace_without_code_object_fact(&trace_path);

        run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            attribution_details: Some(attribution_details_path.clone()),
        })
        .unwrap();

        let artifact: EthdebugArtifact = read_json_file(&out).unwrap();
        assert_eq!(artifact.programs[0].id, ETHDEBUG_FALLBACK_PROGRAM_ID);
        let attribution_details: EthdebugAttributionDetails =
            read_json_file(&attribution_details_path).unwrap();
        assert!(
            attribution_details
                .instruction_origin_index
                .iter()
                .all(|instruction| instruction.code_object.is_none())
        );
        run_debug_validate(&DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out,
            schema_version: "pinned".to_string(),
            attribution_details: Some(attribution_details_path),
            verify_json: None,
        })
        .unwrap();
    }

    #[test]
    fn ethdebug_rejects_unknown_instruction_classification() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        write_debug_trace(&trace_path);
        run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            attribution_details: None,
        })
        .unwrap();

        let mut artifact: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(out.as_std_path()).unwrap()).unwrap();
        artifact["programs"][0]["instructions"][0]["classification"] =
            serde_json::Value::String("forged".to_string());
        fs::write(
            out.as_std_path(),
            serde_json::to_string_pretty(&artifact).unwrap(),
        )
        .unwrap();

        let err = run_debug_validate(&DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out,
            schema_version: "pinned".to_string(),
            attribution_details: None,
            verify_json: None,
        })
        .unwrap_err();
        assert!(err.contains("unknown variant"));
    }

    #[test]
    fn attribution_details_rejects_forged_trace_hash_and_instruction_index() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        let attribution_details =
            Utf8PathBuf::from_path_buf(temp.path().join("debug.attribution-details.json")).unwrap();
        write_debug_trace(&trace_path);
        run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            attribution_details: Some(attribution_details.clone()),
        })
        .unwrap();

        let original = fs::read_to_string(attribution_details.as_std_path()).unwrap();
        let mut forged_hash: serde_json::Value = serde_json::from_str(&original).unwrap();
        forged_hash["trace_hash"] = serde_json::Value::String("forged".to_string());
        fs::write(
            attribution_details.as_std_path(),
            serde_json::to_string_pretty(&forged_hash).unwrap(),
        )
        .unwrap();
        let args = DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out.clone(),
            schema_version: "pinned".to_string(),
            attribution_details: Some(attribution_details.clone()),
            verify_json: None,
        };
        assert!(
            run_debug_validate(&args)
                .unwrap_err()
                .contains("does not match artifact compilation id")
        );

        let mut forged_index: serde_json::Value = serde_json::from_str(&original).unwrap();
        forged_index["instruction_origin_index"][0]["pc_start"] =
            serde_json::Value::Number(999u64.into());
        forged_index["instruction_origin_index"][0]["pc_end"] =
            serde_json::Value::Number(1000u64.into());
        fs::write(
            attribution_details.as_std_path(),
            serde_json::to_string_pretty(&forged_index).unwrap(),
        )
        .unwrap();
        assert!(
            run_debug_validate(&args)
                .unwrap_err()
                .contains("does not match artifact attribution hash")
        );
    }

    #[test]
    fn attribution_details_rejects_forged_primary_source_and_all_origins() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        let attribution_details =
            Utf8PathBuf::from_path_buf(temp.path().join("debug.attribution-details.json")).unwrap();
        write_debug_trace(&trace_path);
        run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            attribution_details: Some(attribution_details.clone()),
        })
        .unwrap();

        let original = fs::read_to_string(attribution_details.as_std_path()).unwrap();
        let forged_origin = key("hir.expr", "forged", "expr:other").canonical_storage_key();
        let args = DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out,
            schema_version: "pinned".to_string(),
            attribution_details: Some(attribution_details.clone()),
            verify_json: None,
        };

        for (field, value) in [
            (
                "primary_source",
                serde_json::Value::String(forged_origin.clone()),
            ),
            ("all_origins", serde_json::json!([forged_origin])),
        ] {
            let mut forged: serde_json::Value = serde_json::from_str(&original).unwrap();
            forged["instruction_origin_index"][0][field] = value;
            fs::write(
                attribution_details.as_std_path(),
                serde_json::to_string_pretty(&forged).unwrap(),
            )
            .unwrap();

            let err = run_debug_validate(&args).unwrap_err();
            assert!(
                err.contains("origin attribution hash")
                    && err.contains("does not match artifact attribution hash"),
                "forged {field} was not rejected by the artifact attribution hash: {err}"
            );
        }
    }

    #[test]
    fn attribution_details_rejects_semantic_attribution_contradictions_when_hashes_match() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        let attribution_details_path =
            Utf8PathBuf::from_path_buf(temp.path().join("debug.attribution-details.json")).unwrap();
        write_debug_trace(&trace_path);
        run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            attribution_details: Some(attribution_details_path.clone()),
        })
        .unwrap();

        let artifact: EthdebugArtifact = read_json_file(&out).unwrap();
        let attribution_details: EthdebugAttributionDetails =
            read_json_file(&attribution_details_path).unwrap();
        let args = DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out.clone(),
            schema_version: "pinned".to_string(),
            attribution_details: Some(attribution_details_path.clone()),
            verify_json: None,
        };
        let validate_coordinated_pair =
            |mut artifact: EthdebugArtifact,
             mut attribution_details: EthdebugAttributionDetails| {
                artifact.compilation.fe_origin_attribution_hash =
                    ethdebug_origin_attribution_hash(&attribution_details.instruction_origin_index);
                let artifact_text = render_json_text(&out, &artifact).unwrap();
                attribution_details.ethdebug_artifact_hash =
                    blake3_hash_label(artifact_text.as_bytes());
                write_text_file(&out, &artifact_text).unwrap();
                write_json_file(&attribution_details_path, &attribution_details).unwrap();
                run_debug_validate(&args).unwrap_err()
            };

        let mut missing_primary = attribution_details.clone();
        missing_primary.instruction_origin_index[0].primary_source = None;
        let err = validate_coordinated_pair(artifact.clone(), missing_primary);
        assert!(err.contains("primary source inconsistent with classification"));

        let mut primary_absent_from_origins = attribution_details;
        primary_absent_from_origins.instruction_origin_index[0]
            .all_origins
            .clear();
        let err = validate_coordinated_pair(artifact, primary_absent_from_origins);
        assert!(err.contains("is absent from all_origins"));
    }

    #[test]
    fn attribution_details_rejects_instruction_without_artifact_counterpart() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug.json")).unwrap();
        let attribution_details_path =
            Utf8PathBuf::from_path_buf(temp.path().join("debug.attribution-details.json")).unwrap();
        write_debug_trace(&trace_path);
        run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Ethdebug,
            from: trace_path,
            out: out.clone(),
            schema_version: "pinned".to_string(),
            phase: None,
            attribution_details: Some(attribution_details_path.clone()),
        })
        .unwrap();

        let mut artifact: EthdebugArtifact = read_json_file(&out).unwrap();
        let mut attribution_details: EthdebugAttributionDetails =
            read_json_file(&attribution_details_path).unwrap();
        let mut extra = attribution_details.instruction_origin_index[0].clone();
        extra.instruction_key = key("bytecode.pc", "demo", "pc:extra").canonical_storage_key();
        extra.pc_start = 99;
        extra.pc_end = 100;
        attribution_details.instruction_origin_index.push(extra);

        artifact.compilation.fe_origin_attribution_hash =
            ethdebug_origin_attribution_hash(&attribution_details.instruction_origin_index);
        let artifact_text = render_json_text(&out, &artifact).unwrap();
        attribution_details.ethdebug_artifact_hash = blake3_hash_label(artifact_text.as_bytes());
        write_text_file(&out, &artifact_text).unwrap();
        write_json_file(&attribution_details_path, &attribution_details).unwrap();

        let err = run_debug_validate(&DevDebugValidateArgs {
            format: DebugExportFormat::Ethdebug,
            input: out,
            schema_version: "pinned".to_string(),
            attribution_details: Some(attribution_details_path),
            verify_json: None,
        })
        .unwrap_err();
        assert!(err.contains("has no artifact instruction"));
    }

    #[test]
    fn ethdebug_schema_is_pinned() {
        let err = ensure_ethdebug_schema("future").unwrap_err();

        assert!(err.contains("unsupported Fe-specific ethdebug view schema version"));
    }

    #[test]
    fn debug_emit_rejects_unimplemented_surfaces() {
        let err = ensure_ethdebug_phase(Some("types")).unwrap_err();
        assert!(err.contains("instruction-source only"));
    }
}
