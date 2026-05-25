use std::fs;

use debug_export::{
    DebugBundle, DwarfLineTable, ETHDEBUG_SCHEMA_VERSION, EthdebugArtifact, emit_dwarf_line_table,
    emit_ethdebug_artifact, validate_ethdebug_artifact,
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
            write_json_file(&args.out, &artifact)?;
            if let Some(sidecar_path) = &args.sidecar {
                write_json_file(sidecar_path, &ethdebug_sidecar(&bundle, &artifact)?)?;
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
        DebugExportFormat::Dwarf => {
            let sections = ensure_dwarf_sections(args.sections.as_deref())?;
            let line_table = emit_dwarf_line_table(&bundle)?;
            let section_bundle = dwarf_section_bundle(
                &bundle,
                &line_table,
                sections,
                args.unit_name.as_deref(),
                &args.language,
                &args.confidence,
            );
            write_json_file(&args.out, &section_bundle)?;
            Ok(format!(
                "wrote experimental DWARF section bundle: {}\n\
                 Data source: {}\n\
                 Trace hash: {}\n\
                 Rows: {}\n\
                 Note: this is a derived DWARF section bundle, not an object file; llvm-dwarfdump validation requires a future object-container wrapper.\n",
                args.out,
                crate::trace::format_data_source(snapshot.metadata()),
                bundle.trace_hash,
                section_bundle.rows.len(),
            ))
        }
    }
}

fn run_debug_validate(args: &DevDebugValidateArgs) -> Result<String, String> {
    match args.format {
        DebugExportFormat::Ethdebug => {
            ensure_ethdebug_schema(&args.schema_version)?;
            let artifact = read_json_file::<EthdebugArtifact>(&args.input)?;
            validate_ethdebug_artifact(&artifact)?;
            let mut sidecar_checked = false;
            if let Some(sidecar_path) = &args.sidecar {
                validate_sidecar(sidecar_path, &artifact)?;
                sidecar_checked = true;
            }
            write_validation_json(
                args.verify_json.as_ref(),
                json!({
                    "format": "ethdebug",
                    "status": "ok",
                    "schema_version": artifact.schema_version,
                    "program_count": artifact.programs.len(),
                    "sidecar_checked": sidecar_checked,
                }),
            )?;
            Ok(format!(
                "ethdebug validation passed: {}\nPrograms: {}\nSidecar checked: {}\n",
                args.input,
                artifact.programs.len(),
                sidecar_checked,
            ))
        }
        DebugExportFormat::Dwarf => {
            let section_bundle = read_json_file::<DwarfSectionBundle>(&args.input)?;
            validate_dwarf_section_bundle(&section_bundle)?;
            write_validation_json(
                args.verify_json.as_ref(),
                json!({
                    "format": "dwarf-section-bundle",
                    "status": "ok",
                    "trace_hash": section_bundle.trace_hash,
                    "row_count": section_bundle.rows.len(),
                    "note": "section bundle validation only; object-container validation is not emitted by this wrapper",
                }),
            )?;
            Ok(format!(
                "experimental DWARF section bundle validation passed: {}\nRows: {}\nNote: object-container validation requires a future wrapper.\n",
                args.input,
                section_bundle.rows.len(),
            ))
        }
    }
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

fn ensure_dwarf_sections(value: Option<&str>) -> Result<&'static str, String> {
    match value.unwrap_or("line") {
        "line" => Ok("line"),
        other => Err(format!(
            "unsupported DWARF sections {other}; this wrapper currently emits line only"
        )),
    }
}

fn write_json_file<T: Serialize>(path: &camino::Utf8Path, value: &T) -> Result<(), String> {
    if let Some(parent) = path.parent()
        && !parent.as_str().is_empty()
    {
        fs::create_dir_all(parent.as_std_path())
            .map_err(|err| format!("failed to create {parent}: {err}"))?;
    }
    let json = serde_json::to_string_pretty(value)
        .map_err(|err| format!("failed to render JSON for {path}: {err}"))?;
    fs::write(path.as_std_path(), format!("{json}\n"))
        .map_err(|err| format!("failed to write {path}: {err}"))
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
) -> Result<EthdebugSidecar, String> {
    Ok(EthdebugSidecar {
        schema_version: "fe-ethdebug-origin-sidecar-v1".to_string(),
        trace_hash: bundle.trace_hash.clone(),
        ethdebug_schema_version: artifact.schema_version.clone(),
        ethdebug_artifact_hash: artifact_hash(artifact)?,
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
                classification: format!("{:?}", instruction.classification),
                confidence: format!("{:?}", instruction.confidence),
            })
            .collect(),
    })
}

fn artifact_hash(artifact: &EthdebugArtifact) -> Result<String, String> {
    let bytes = serde_json::to_vec(artifact)
        .map_err(|err| format!("failed to serialize ethdebug artifact for hashing: {err}"))?;
    Ok(format!("blake3:{}", blake3::hash(&bytes).to_hex()))
}

fn validate_sidecar(path: &camino::Utf8Path, artifact: &EthdebugArtifact) -> Result<(), String> {
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
    let expected_hash = artifact_hash(artifact)?;
    if sidecar.ethdebug_artifact_hash != expected_hash {
        return Err(format!(
            "ethdebug sidecar artifact hash {} does not match {}",
            sidecar.ethdebug_artifact_hash, expected_hash
        ));
    }
    Ok(())
}

fn dwarf_section_bundle(
    bundle: &DebugBundle,
    table: &DwarfLineTable,
    sections_requested: &str,
    unit_name: Option<&str>,
    language: &str,
    confidence_filter: &str,
) -> DwarfSectionBundle {
    DwarfSectionBundle {
        format: "fe-dwarf-section-bundle-v1".to_string(),
        note: "experimental line-table section bundle; object-container emission and llvm-dwarfdump validation require a future wrapper".to_string(),
        trace_hash: bundle.trace_hash.clone(),
        unit_name: unit_name
            .map(str::to_string)
            .unwrap_or_else(|| bundle.compiler.input_path.clone()),
        language: language.to_string(),
        sections_requested: sections_requested.to_string(),
        confidence_filter: confidence_filter.to_string(),
        sections: DwarfSections {
            debug_abbrev: hex::encode(&table.debug_abbrev),
            debug_info: hex::encode(&table.debug_info),
            debug_line: hex::encode(&table.debug_line),
            debug_line_str: hex::encode(&table.debug_line_str),
            debug_str: hex::encode(&table.debug_str),
        },
        rows: table
            .rows
            .iter()
            .map(|row| DwarfSectionRow {
                pc_start: row.pc_range.start,
                pc_end: row.pc_range.end,
                line: row.line,
                column: row.column,
            })
            .collect(),
    }
}

fn validate_dwarf_section_bundle(bundle: &DwarfSectionBundle) -> Result<(), String> {
    if bundle.format != "fe-dwarf-section-bundle-v1" {
        return Err(format!(
            "unsupported DWARF section bundle format {}",
            bundle.format
        ));
    }
    if bundle.trace_hash.trim().is_empty() {
        return Err("DWARF section bundle trace hash is empty".to_string());
    }
    if bundle.rows.is_empty() {
        return Err("DWARF section bundle has no line rows".to_string());
    }
    if bundle.sections.debug_abbrev.is_empty()
        || bundle.sections.debug_info.is_empty()
        || bundle.sections.debug_line.is_empty()
    {
        return Err("DWARF section bundle is missing required sections".to_string());
    }
    Ok(())
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

#[derive(Clone, Debug, Serialize, Deserialize)]
struct DwarfSectionBundle {
    format: String,
    note: String,
    trace_hash: String,
    unit_name: String,
    language: String,
    sections_requested: String,
    confidence_filter: String,
    sections: DwarfSections,
    rows: Vec<DwarfSectionRow>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct DwarfSections {
    debug_abbrev: String,
    debug_info: String,
    debug_line: String,
    debug_line_str: String,
    debug_str: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct DwarfSectionRow {
    pc_start: u32,
    pc_end: u32,
    line: u32,
    column: u32,
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
            sections: None,
            unit_name: None,
            language: "fe".to_string(),
            confidence: "compiler-emitted".to_string(),
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
    fn dwarf_emit_writes_section_bundle_not_object_file() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("debug-dwarf.json")).unwrap();
        write_debug_trace(&trace_path);

        let output = run_debug_emit(&DevDebugEmitArgs {
            format: DebugExportFormat::Dwarf,
            from: trace_path,
            out: out.clone(),
            sections: Some("line".to_string()),
            unit_name: Some("demo".to_string()),
            language: "fe".to_string(),
            confidence: "compiler-emitted".to_string(),
            schema_version: "pinned".to_string(),
            phase: None,
            sidecar: None,
        })
        .unwrap();

        assert!(output.contains("not an object file"));
        let bundle = read_json_file::<DwarfSectionBundle>(&out).unwrap();
        validate_dwarf_section_bundle(&bundle).unwrap();
        assert_eq!(bundle.rows[0].pc_start, 4);
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

        let err = ensure_dwarf_sections(Some("line,types")).unwrap_err();
        assert!(err.contains("line only"));
    }
}
