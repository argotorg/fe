use std::{
    collections::{BTreeMap, BTreeSet},
    fs::{self, File},
    io::{BufWriter, Write},
};

use serde::Serialize;
use serde_json::json;
use trace_facts::{RelationRow, RelationSchema};
use trace_query::datalog_emit::{
    CORE_DATALOG_RULES, DatalogBaseExport, builtin_rulepack, builtin_rulepacks,
    emit_base_relations, run_builtin_rulepack,
};

use crate::{
    DatalogExportFormat, DevTraceDatalogCommand, DevTraceDatalogExportArgs,
    DevTraceDatalogInitArgs, DevTraceDatalogRunArgs, TraceReportFormat,
};

pub(super) fn run_export_datalog(args: &DevTraceDatalogExportArgs) -> Result<String, String> {
    let snapshot = crate::trace::read_trace_snapshot_jsonl_from_path(&args.from)?;
    let export = emit_base_relations(&snapshot);
    fs::create_dir_all(args.out.as_std_path())
        .map_err(|err| format!("failed to create Datalog export dir {}: {err}", args.out))?;
    write_relation_files(&args.out, &export, args.format)?;
    write_manifest(&args.out, &export, args.format, args.base_only)?;
    fs::write(args.out.join("core_rules.dl").as_std_path(), export.rules)
        .map_err(|err| format!("failed to write core Datalog rules: {err}"))?;

    Ok(format!(
        "wrote Datalog base relation export: {}\n\
         Data source: {}\n\
         Trace hash: {}\n\
         Relations: {}\n\
         Rows: {}\n\
         Format: {}\n\
         Note: rule execution is not wired in this phase; exported rows are typed TraceFact projections only.\n",
        args.out,
        super::format_data_source(snapshot.metadata()),
        export.trace_hash,
        export.schemas.len(),
        export.rows.len(),
        datalog_export_format_name(args.format),
    ))
}

pub(super) fn run_datalog_command(command: &DevTraceDatalogCommand) -> Result<String, String> {
    match command {
        DevTraceDatalogCommand::Run(args) => run_datalog_query(args),
        DevTraceDatalogCommand::InitRulepack(args) => init_rulepack(args),
    }
}

fn run_datalog_query(args: &DevTraceDatalogRunArgs) -> Result<String, String> {
    let snapshot = crate::trace::read_trace_snapshot_jsonl_from_path(&args.from)?;
    let export = emit_base_relations(&snapshot);
    if builtin_rulepack(&args.rulepack).is_some() {
        let report = run_builtin_rulepack(&snapshot, &args.rulepack, &args.query)
            .map_err(|err| err.to_string())?;
        return match args.format {
            TraceReportFormat::Text => Ok(render_builtin_rulepack_report(
                &report,
                &super::format_data_source(snapshot.metadata()),
            )),
            TraceReportFormat::Json => serde_json::to_string_pretty(&report)
                .map(|json| format!("{json}\n"))
                .map_err(|err| format!("failed to render Datalog result JSON: {err}")),
        };
    }
    if looks_like_rulepack_path(&args.rulepack) {
        let custom = load_custom_rulepack(&args.rulepack, &export)?;
        return render_custom_rulepack_status(custom, args, &snapshot, &export);
    }

    let known = builtin_rulepacks()
        .into_iter()
        .map(|rulepack| rulepack.id)
        .collect::<Vec<_>>()
        .join(", ");
    Err(format!(
        "unknown Datalog rulepack {:?}; known built-ins: {known}; pass a rulepack directory for custom packs",
        args.rulepack
    ))
}

fn render_builtin_rulepack_report(
    report: &trace_query::datalog_emit::DatalogRunReport,
    data_source: &str,
) -> String {
    let mut out = String::new();
    out.push_str("Datalog rulepack query\n");
    out.push_str(&format!("Status: {}\n", report.status));
    out.push_str(&format!("Data source: {data_source}\n"));
    out.push_str(&format!("Trace hash: {}\n", report.trace_hash));
    out.push_str(&format!(
        "Rulepack: {}@{}\n",
        report.rulepack_id, report.rulepack_version
    ));
    out.push_str(&format!("Engine: {}\n", report.engine));
    out.push_str(&format!("Relation schema: {}\n", report.relation_schema));
    out.push_str(&format!("Query: {}\n", report.query));
    out.push_str(&format!(
        "Attribution policy: {}\n",
        report.attribution_policy
    ));
    out.push_str(&format!("Base relations: {}\n", report.base_relation_count));
    out.push_str(&format!("Base rows: {}\n", report.base_row_count));
    if !report.missing_evidence.is_empty() {
        out.push_str("Missing evidence:\n");
        for item in &report.missing_evidence {
            out.push_str(&format!("  {item}\n"));
        }
    }
    out.push_str("Output JSON:\n");
    out.push_str(
        &serde_json::to_string_pretty(&report.output).unwrap_or_else(|_| report.output.to_string()),
    );
    out.push('\n');
    out
}

fn looks_like_rulepack_path(value: &str) -> bool {
    value.contains('/') || value.contains('\\') || value == "." || value.starts_with("..")
}

fn load_custom_rulepack(
    path: &str,
    export: &DatalogBaseExport,
) -> Result<CustomRulepackStatus, String> {
    let path = camino::Utf8PathBuf::from(path);
    let manifest_path = path.join("manifest.toml");
    let rules_path = path.join("rules.dl");
    let reports_path = path.join("reports.toml");
    let manifest = fs::read_to_string(manifest_path.as_std_path())
        .map_err(|err| format!("failed to read custom rulepack manifest {manifest_path}: {err}"))?;
    let rules = fs::read_to_string(rules_path.as_std_path())
        .map_err(|err| format!("failed to read custom rulepack rules {rules_path}: {err}"))?;
    let reports = fs::read_to_string(reports_path.as_std_path())
        .map_err(|err| format!("failed to read custom rulepack reports {reports_path}: {err}"))?;
    let relation_names = export
        .schemas
        .iter()
        .map(|schema| schema.name.to_string())
        .collect::<BTreeSet<_>>();
    Ok(CustomRulepackStatus {
        path: path.to_string(),
        manifest_bytes: manifest.len(),
        rules_bytes: rules.len(),
        reports_bytes: reports.len(),
        available_base_relations: relation_names.into_iter().collect(),
    })
}

fn render_custom_rulepack_status(
    custom: CustomRulepackStatus,
    args: &DevTraceDatalogRunArgs,
    snapshot: &trace_facts::TraceSnapshot,
    export: &DatalogBaseExport,
) -> Result<String, String> {
    let response = CustomRulepackRunReport {
        status: "loaded",
        reason: "custom Datalog rulepacks are loaded and validated for shape, but no external Datalog engine is invoked by this dev wrapper yet",
        data_source: super::format_data_source(snapshot.metadata()),
        trace_hash: snapshot.trace_hash().to_string(),
        base_relation_count: export.schemas.len(),
        base_row_count: export.rows.len(),
        rulepack: args.rulepack.clone(),
        query: args.query.clone(),
        custom,
    };
    match args.format {
        TraceReportFormat::Text => Ok(format!(
            "Custom Datalog rulepack loaded.\n\
             Data source: {}\n\
             Trace hash: {}\n\
             Base relations: {}\n\
             Base rows: {}\n\
             Rulepack: {}\n\
             Query: {}\n\
             Manifest bytes: {}\n\
             Rules bytes: {}\n\
             Reports bytes: {}\n\
             No derived claims were emitted by the custom rulepack path.\n",
            response.data_source,
            response.trace_hash,
            response.base_relation_count,
            response.base_row_count,
            response.rulepack,
            response.query,
            response.custom.manifest_bytes,
            response.custom.rules_bytes,
            response.custom.reports_bytes,
        )),
        TraceReportFormat::Json => serde_json::to_string_pretty(&response)
            .map(|json| format!("{json}\n"))
            .map_err(|err| format!("failed to render Datalog status JSON: {err}")),
    }
}

fn init_rulepack(args: &DevTraceDatalogInitArgs) -> Result<String, String> {
    if args.path.exists()
        && fs::read_dir(args.path.as_std_path())
            .map_err(|err| format!("failed to read {}: {err}", args.path))?
            .next()
            .is_some()
    {
        return Err(format!(
            "refusing to initialize Datalog rulepack in non-empty directory {}",
            args.path
        ));
    }
    fs::create_dir_all(args.path.join("tests").as_std_path())
        .map_err(|err| format!("failed to create rulepack directory {}: {err}", args.path))?;
    fs::write(
        args.path.join("manifest.toml").as_std_path(),
        r#"[rulepack]
schema_version = "fe-datalog-rulepack-v1"
name = "custom"
description = "Custom read-only rules over Fe trace base relations."

[requires]
trace_schema_version = 1
relation_schema = "fe-datalog-base-export-v1"
"#,
    )
    .map_err(|err| format!("failed to write rulepack manifest: {err}"))?;
    fs::write(args.path.join("rules.dl").as_std_path(), CORE_DATALOG_RULES)
        .map_err(|err| format!("failed to write rulepack rules: {err}"))?;
    fs::write(
        args.path.join("reports.toml").as_std_path(),
        r#"# Reports are read-only views over base TraceFact relations.
# Custom Datalog execution is intentionally external-engine-only for now.
"#,
    )
    .map_err(|err| format!("failed to write rulepack report config: {err}"))?;
    Ok(format!(
        "initialized Datalog rulepack skeleton: {}\n\
         Built-in rulepacks are gas-v1, runtime-v1, and security-v1; custom packs are loaded but not executed by this dev wrapper yet.\n",
        args.path
    ))
}

fn write_manifest(
    out: &camino::Utf8Path,
    export: &DatalogBaseExport,
    format: DatalogExportFormat,
    base_only_requested: bool,
) -> Result<(), String> {
    let relation_counts = relation_counts(&export.rows);
    let relations = export
        .schemas
        .iter()
        .map(|schema| RelationManifest {
            name: schema.name.to_string(),
            columns: schema
                .columns
                .iter()
                .map(|column| RelationColumnManifest {
                    name: column.name.to_string(),
                    kind: format!("{:?}", column.kind),
                })
                .collect(),
            row_count: relation_counts.get(schema.name).copied().unwrap_or(0),
        })
        .collect::<Vec<_>>();
    let manifest = DatalogExportManifest {
        schema_version: "fe-datalog-base-export-v1",
        input_trace_hash: export.trace_hash.clone(),
        format: datalog_export_format_name(format),
        base_only_requested,
        base_rows_only: true,
        rule_execution: "unavailable",
        relation_count: export.schemas.len(),
        row_count: export.rows.len(),
        relations,
    };
    let json = serde_json::to_string_pretty(&manifest)
        .map_err(|err| format!("failed to render Datalog export manifest: {err}"))?;
    fs::write(out.join("manifest.json").as_std_path(), format!("{json}\n"))
        .map_err(|err| format!("failed to write Datalog export manifest: {err}"))
}

fn write_relation_files(
    out: &camino::Utf8Path,
    export: &DatalogBaseExport,
    format: DatalogExportFormat,
) -> Result<(), String> {
    let rows = rows_by_relation(&export.rows);
    for schema in &export.schemas {
        match format {
            DatalogExportFormat::Csv => write_csv_relation(out, schema, &rows)?,
            DatalogExportFormat::Jsonl => write_jsonl_relation(out, schema, &rows)?,
        }
    }
    Ok(())
}

fn write_csv_relation(
    out: &camino::Utf8Path,
    schema: &RelationSchema,
    rows: &BTreeMap<&'static str, Vec<&RelationRow>>,
) -> Result<(), String> {
    let path = out.join(format!("{}.csv", schema.name));
    let mut file = BufWriter::new(
        File::create(path.as_std_path())
            .map_err(|err| format!("failed to create relation file {path}: {err}"))?,
    );
    let header = schema
        .columns
        .iter()
        .map(|column| column.name)
        .collect::<Vec<_>>()
        .join(",");
    writeln!(file, "{header}")
        .map_err(|err| format!("failed to write relation file {path}: {err}"))?;
    for row in rows.get(schema.name).into_iter().flatten() {
        let values = row
            .values
            .iter()
            .map(|value| csv_escape(value))
            .collect::<Vec<_>>()
            .join(",");
        writeln!(file, "{values}")
            .map_err(|err| format!("failed to write relation file {path}: {err}"))?;
    }
    Ok(())
}

fn write_jsonl_relation(
    out: &camino::Utf8Path,
    schema: &RelationSchema,
    rows: &BTreeMap<&'static str, Vec<&RelationRow>>,
) -> Result<(), String> {
    let path = out.join(format!("{}.jsonl", schema.name));
    let mut file = BufWriter::new(
        File::create(path.as_std_path())
            .map_err(|err| format!("failed to create relation file {path}: {err}"))?,
    );
    for row in rows.get(schema.name).into_iter().flatten() {
        let mut object = serde_json::Map::new();
        object.insert("relation".to_string(), json!(row.relation));
        for (index, column) in schema.columns.iter().enumerate() {
            object.insert(
                column.name.to_string(),
                json!(row.values.get(index).cloned().unwrap_or_default()),
            );
        }
        serde_json::to_writer(&mut file, &object)
            .map_err(|err| format!("failed to write relation file {path}: {err}"))?;
        writeln!(file).map_err(|err| format!("failed to write relation file {path}: {err}"))?;
    }
    Ok(())
}

fn rows_by_relation(rows: &[RelationRow]) -> BTreeMap<&'static str, Vec<&RelationRow>> {
    let mut grouped = BTreeMap::new();
    for row in rows {
        grouped
            .entry(row.relation)
            .or_insert_with(Vec::new)
            .push(row);
    }
    grouped
}

fn relation_counts(rows: &[RelationRow]) -> BTreeMap<&'static str, usize> {
    let mut counts = BTreeMap::new();
    for row in rows {
        *counts.entry(row.relation).or_insert(0) += 1;
    }
    counts
}

fn csv_escape(value: &str) -> String {
    if value.contains([',', '"', '\n', '\r']) {
        format!("\"{}\"", value.replace('"', "\"\""))
    } else {
        value.to_string()
    }
}

const fn datalog_export_format_name(format: DatalogExportFormat) -> &'static str {
    match format {
        DatalogExportFormat::Csv => "csv",
        DatalogExportFormat::Jsonl => "jsonl",
    }
}

#[derive(Debug, Serialize)]
struct DatalogExportManifest {
    schema_version: &'static str,
    input_trace_hash: String,
    format: &'static str,
    base_only_requested: bool,
    base_rows_only: bool,
    rule_execution: &'static str,
    relation_count: usize,
    row_count: usize,
    relations: Vec<RelationManifest>,
}

#[derive(Debug, Serialize)]
struct RelationManifest {
    name: String,
    columns: Vec<RelationColumnManifest>,
    row_count: usize,
}

#[derive(Debug, Serialize)]
struct RelationColumnManifest {
    name: String,
    kind: String,
}

#[derive(Debug, Serialize)]
struct CustomRulepackStatus {
    path: String,
    manifest_bytes: usize,
    rules_bytes: usize,
    reports_bytes: usize,
    available_base_relations: Vec<String>,
}

#[derive(Debug, Serialize)]
struct CustomRulepackRunReport {
    status: &'static str,
    reason: &'static str,
    data_source: String,
    trace_hash: String,
    base_relation_count: usize,
    base_row_count: usize,
    rulepack: String,
    query: String,
    custom: CustomRulepackStatus,
}

#[cfg(test)]
mod tests {
    use std::{fs, io::Cursor};

    use camino::Utf8PathBuf;
    use common::origin::OriginExportKey;
    use tempfile::tempdir;
    use trace_facts::{
        InstructionFact, JsonlTraceSink, OriginNodeFact, OriginNodeKind, TraceBundle, TraceFact,
        TraceMetadata,
    };

    use super::*;
    use crate::DevTraceDatalogExportArgs;

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(key: OriginExportKey) -> TraceFact {
        TraceFact::OriginNode(OriginNodeFact::new(
            key.clone(),
            OriginNodeKind::new(key.kind()),
        ))
    }

    fn write_snapshot(path: &camino::Utf8Path) {
        let function = key("bytecode.function", "demo", "runtime");
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let bundle = TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "dev".to_string(), "trace".to_string()],
                "demo.fe",
                vec![],
            ),
            vec![
                node(function.clone()),
                node(instruction.clone()),
                TraceFact::Instruction(InstructionFact::new(instruction, function, 0, "STOP")),
            ],
        );
        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_bundle(&bundle).unwrap();
        fs::write(path.as_std_path(), sink.into_inner()).unwrap();
    }

    #[test]
    fn export_datalog_writes_base_relation_manifest_and_csv() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let out = Utf8PathBuf::from_path_buf(temp.path().join("relations")).unwrap();
        write_snapshot(&trace_path);

        let output = run_export_datalog(&DevTraceDatalogExportArgs {
            from: trace_path,
            out: out.clone(),
            format: DatalogExportFormat::Csv,
            base_only: true,
        })
        .unwrap();

        assert!(output.contains("typed TraceFact projections only"));
        assert!(out.join("manifest.json").exists());
        assert!(out.join("base_instruction.csv").exists());
        let manifest = fs::read_to_string(out.join("manifest.json").as_std_path()).unwrap();
        assert!(manifest.contains("\"base_rows_only\": true"));
    }

    #[test]
    fn datalog_run_executes_builtin_rulepack_projection() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        write_snapshot(&trace_path);

        let output = run_datalog_query(&DevTraceDatalogRunArgs {
            from: trace_path,
            rulepack: "gas-v1".to_string(),
            query: "gas-by-source".to_string(),
            format: TraceReportFormat::Text,
        })
        .unwrap();

        assert!(output.contains("Datalog rulepack query"));
        assert!(output.contains("Rulepack: gas-v1@1.0.0"));
        assert!(output.contains("Engine: builtin-rust-derived-v1"));
        assert!(output.contains("Output JSON"));
    }

    #[test]
    fn datalog_run_loads_custom_rulepack_without_emitting_claims() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        let rulepack = Utf8PathBuf::from_path_buf(temp.path().join("rulepack")).unwrap();
        write_snapshot(&trace_path);
        init_rulepack(&DevTraceDatalogInitArgs {
            path: rulepack.clone(),
        })
        .unwrap();

        let output = run_datalog_query(&DevTraceDatalogRunArgs {
            from: trace_path,
            rulepack: rulepack.to_string(),
            query: "custom-query".to_string(),
            format: TraceReportFormat::Text,
        })
        .unwrap();

        assert!(output.contains("Custom Datalog rulepack loaded"));
        assert!(output.contains("No derived claims were emitted"));
    }

    #[test]
    fn init_rulepack_refuses_non_empty_directory() {
        let temp = tempdir().unwrap();
        let rulepack = Utf8PathBuf::from_path_buf(temp.path().join("rulepack")).unwrap();
        fs::create_dir_all(rulepack.as_std_path()).unwrap();
        fs::write(rulepack.join("existing").as_std_path(), b"x").unwrap();

        let err = init_rulepack(&DevTraceDatalogInitArgs { path: rulepack }).unwrap_err();

        assert!(err.contains("non-empty directory"));
    }

    #[test]
    fn csv_escape_quotes_commas_quotes_and_newlines() {
        assert_eq!(csv_escape("plain"), "plain");
        assert_eq!(csv_escape("a,b"), "\"a,b\"");
        assert_eq!(csv_escape("a\"b"), "\"a\"\"b\"");
        assert_eq!(csv_escape("a\nb"), "\"a\nb\"");
    }

    #[test]
    fn jsonl_reader_fixture_stays_valid_for_export_tests() {
        let temp = tempdir().unwrap();
        let trace_path = Utf8PathBuf::from_path_buf(temp.path().join("trace.jsonl")).unwrap();
        write_snapshot(&trace_path);
        let bytes = fs::read(trace_path.as_std_path()).unwrap();
        let bundle = trace_facts::JsonlTraceReader::new(Cursor::new(bytes))
            .read_bundle()
            .unwrap();

        assert_eq!(bundle.facts.len(), 3);
    }
}
