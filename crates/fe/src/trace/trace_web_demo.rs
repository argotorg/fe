use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    io::{BufRead, BufReader, Write},
    net::{TcpListener, TcpStream},
    sync::{Arc, Mutex},
    time::{Duration, Instant, SystemTime},
};

use camino::Utf8PathBuf;
use common::{SalsaEventCounters, origin::OriginExportKey};
use serde::Serialize;
use trace_facts::{
    InstructionFact, OriginEdgeFact, OriginEdgeTraversalClass, OriginNodeFact, SourceFileFact,
    SourceSpanFact, TraceFact, TraceMetadata, TraceSnapshot,
};
use trace_query::{
    AttributionAuditReport, IntrospectionService, LoopContentsRequest, TraceIntrospectionService,
    TraceWorkbenchProjectionRequest,
    origin_closure::{
        ClosureAuditReport, OriginClosure as DemoClosure, OriginClosureSourceLine,
        audit_origin_closures, build_origin_closure_set, classes_by_origin_key,
        component_classes_by_origin_key, source_owner_matches_input,
    },
    static_analysis::{StaticAnalysisReport, static_analysis_report},
    trace_workbench_report_projection,
};

use crate::{DevTraceAuditClosuresArgs, DevTraceWebDemoArgs, TraceReportFormat};

pub(super) fn run_trace_web_demo(args: &DevTraceWebDemoArgs) -> Result<String, String> {
    if args.serve {
        return serve_trace_web_demo(args);
    }

    let rendered = render_trace_web_demo_once(args, false)?;
    let out = args
        .out
        .as_ref()
        .ok_or_else(|| "pass --out HTML_PATH when not using --serve".to_string())?;
    fs::write(out.as_std_path(), &rendered.html)
        .map_err(|err| format!("failed to write web demo {out}: {err}"))?;
    Ok(format!(
        "wrote origin trace web demo: {}\nData source: {}\nLoop bytecode PCs: {}\nSource confidence: {}\n{}\n",
        out,
        rendered.data_source(),
        rendered.bytecode_count(),
        rendered.source_confidence(),
        rendered.salsa_summary(),
    ))
}

pub(super) fn run_trace_audit_closures(args: &DevTraceAuditClosuresArgs) -> Result<String, String> {
    let model = render_trace_audit_model_once(args)?;
    let report = model
        .audit
        .ok_or_else(|| "trace audit model did not include a closure audit".to_string())?;
    match args.format {
        TraceReportFormat::Text => Ok(render_closure_audit_report(&report)),
        TraceReportFormat::Json => serde_json::to_string_pretty(&report)
            .map(|mut json| {
                json.push('\n');
                json
            })
            .map_err(|err| format!("failed to render trace reachability audit JSON: {err}")),
    }
}

struct RenderedWebDemo {
    html: String,
    model: serde_json::Value,
}

impl RenderedWebDemo {
    fn salsa_summary(&self) -> String {
        self.model.get("salsa").map_or_else(
            || "Salsa: offline JSONL input".to_string(),
            |salsa| {
                if salsa.is_null() {
                    return "Salsa: offline JSONL input".to_string();
                }
                format!(
                    "Salsa: {} in {}ms, will_execute={}, memo_reuse={}",
                    salsa
                        .get("mode")
                        .and_then(serde_json::Value::as_str)
                        .unwrap_or("unknown"),
                    salsa
                        .get("elapsed_ms")
                        .and_then(serde_json::Value::as_u64)
                        .unwrap_or_default(),
                    salsa
                        .get("will_execute")
                        .and_then(serde_json::Value::as_u64)
                        .unwrap_or_default(),
                    salsa
                        .get("memo_reuse")
                        .and_then(serde_json::Value::as_u64)
                        .unwrap_or_default()
                )
            },
        )
    }

    fn data_source(&self) -> &str {
        self.model
            .pointer("/metadata/data_source")
            .and_then(serde_json::Value::as_str)
            .unwrap_or("unknown")
    }

    fn bytecode_count(&self) -> usize {
        self.model
            .get("bytecode_count")
            .and_then(serde_json::Value::as_u64)
            .unwrap_or_default() as usize
    }

    fn source_confidence(&self) -> &str {
        self.model
            .pointer("/source/confidence")
            .and_then(serde_json::Value::as_str)
            .unwrap_or("source text unavailable")
    }
}

fn render_trace_web_demo_once(
    args: &DevTraceWebDemoArgs,
    live_reload: bool,
) -> Result<RenderedWebDemo, String> {
    match (&args.from, &args.source) {
        (Some(from), None) => {
            let snapshot = super::read_trace_snapshot_jsonl_from_path(from)?;
            render_snapshot(&snapshot, None, live_reload)
        }
        (None, Some(source)) => {
            let mut emitter = incremental_emitter(
                source,
                args.standalone,
                &args.profile,
                &args.optimize,
                "web-demo",
            )?;
            render_incremental_trace(&mut emitter, 1, live_reload)
        }
        (None, None) => Err("pass either --from TRACE_JSONL or --source FE_FILE".to_string()),
        (Some(_), Some(_)) => Err("pass only one of --from or --source".to_string()),
    }
}

fn incremental_emitter(
    source: &Utf8PathBuf,
    standalone: bool,
    profile: &str,
    optimize: &str,
    command_leaf: &str,
) -> Result<super::trace_emit::IncrementalTraceEmitter, String> {
    let opt_level = optimize.parse::<codegen::OptLevel>()?;
    super::trace_emit::IncrementalTraceEmitter::new(
        source,
        standalone,
        profile,
        opt_level,
        vec![
            "fe".to_string(),
            "dev".to_string(),
            "trace".to_string(),
            command_leaf.to_string(),
        ],
    )
}

fn render_trace_audit_model_once(args: &DevTraceAuditClosuresArgs) -> Result<WebDemoModel, String> {
    match (&args.from, &args.source) {
        (Some(from), None) => {
            let snapshot = super::read_trace_snapshot_jsonl_from_path(from)?;
            Ok(build_demo_model(&snapshot, None))
        }
        (None, Some(source)) => {
            let mut emitter = incremental_emitter(
                source,
                args.standalone,
                &args.profile,
                &args.optimize,
                "audit-closures",
            )?;
            let started = Instant::now();
            let output = emitter.emit_from_disk()?;
            let elapsed = started.elapsed();
            let snapshot = TraceSnapshot::new(output.bundle)
                .map_err(|err| format!("incremental trace validation failed: {err}"))?;
            let salsa = Some(DemoSalsaStats::from_counters(
                "long-lived DriverDataBase",
                1,
                elapsed,
                output.salsa_events,
            ));
            Ok(build_demo_model(&snapshot, salsa))
        }
        (None, None) => Err("pass either --from TRACE_JSONL or --source FE_FILE".to_string()),
        (Some(_), Some(_)) => Err("pass only one of --from or --source".to_string()),
    }
}

fn render_incremental_trace(
    emitter: &mut super::trace_emit::IncrementalTraceEmitter,
    generation: u64,
    live_reload: bool,
) -> Result<RenderedWebDemo, String> {
    let started = Instant::now();
    let output = emitter.emit_from_disk()?;
    let elapsed = started.elapsed();
    let snapshot = TraceSnapshot::new(output.bundle)
        .map_err(|err| format!("incremental trace validation failed: {err}"))?;
    let salsa = Some(DemoSalsaStats::from_counters(
        "long-lived DriverDataBase",
        generation,
        elapsed,
        output.salsa_events,
    ));
    render_snapshot(&snapshot, salsa, live_reload)
}

fn render_snapshot(
    snapshot: &TraceSnapshot,
    salsa: Option<DemoSalsaStats>,
    live_reload: bool,
) -> Result<RenderedWebDemo, String> {
    let model = build_trace_workbench_model(snapshot, salsa.as_ref())?;
    let html = render_origin_trace_html(&model, live_reload)?;
    Ok(RenderedWebDemo { html, model })
}

fn build_trace_workbench_model(
    snapshot: &TraceSnapshot,
    salsa: Option<&DemoSalsaStats>,
) -> Result<serde_json::Value, String> {
    let service = TraceIntrospectionService::new(snapshot.clone());
    let input_path = snapshot.metadata().input_path.as_str();
    let source_text = read_source_text(input_path);
    let related_source_texts = source_text
        .as_deref()
        .map(|source_text| trace_workbench_related_source_texts(snapshot, input_path, source_text))
        .unwrap_or_default();
    let data_source = super::format_data_source(snapshot.metadata());
    let mut projection = trace_workbench_report_projection(
        &service,
        snapshot,
        TraceWorkbenchProjectionRequest {
            input_path: snapshot.metadata().input_path.clone(),
            target: snapshot.metadata().target.clone(),
            opt_level: trace_web_demo_opt_level(snapshot.metadata()),
            view: "source-postopt-bytecode".to_string(),
            include_legacy_closure_debug: false,
            source_text,
            related_source_texts,
            document_version: None,
            query_duration_ms: salsa
                .map(|salsa| salsa.elapsed_ms as u64)
                .unwrap_or_default(),
            compiler_commit: snapshot.metadata().compiler_commit.clone(),
            data_source,
        },
    );
    if let Some(salsa) = salsa {
        projection["salsa"] = serde_json::to_value(salsa)
            .map_err(|err| format!("failed to serialize Salsa stats: {err}"))?;
    }
    Ok(projection)
}

fn trace_web_demo_opt_level(metadata: &TraceMetadata) -> String {
    metadata
        .flags
        .iter()
        .find_map(|flag| {
            flag.strip_prefix("optimize=")
                .or_else(|| flag.strip_prefix("opt_level="))
        })
        .map(normalize_trace_web_demo_opt_level)
        .unwrap_or_else(|| "unknown".to_string())
}

fn normalize_trace_web_demo_opt_level(value: &str) -> String {
    match value {
        "0" => "O0".to_string(),
        "1" => "O1".to_string(),
        "2" => "O2".to_string(),
        other => other.to_string(),
    }
}

impl DemoSalsaStats {
    fn from_counters(
        mode: impl Into<String>,
        generation: u64,
        elapsed: Duration,
        counters: SalsaEventCounters,
    ) -> Self {
        Self {
            mode: mode.into(),
            generation,
            elapsed_ms: elapsed.as_millis(),
            will_execute: counters.will_execute,
            memo_reuse: counters.did_validate_memoized_value,
            stale_outputs: counters.will_discard_stale_output,
            discarded: counters.did_discard,
            cycle_iterations: counters.will_iterate_cycle,
        }
    }
}

enum LiveWebDemoInput {
    Jsonl {
        path: Utf8PathBuf,
    },
    Source {
        emitter: Box<super::trace_emit::IncrementalTraceEmitter>,
    },
}

impl LiveWebDemoInput {
    fn path(&self) -> &Utf8PathBuf {
        match self {
            Self::Jsonl { path } => path,
            Self::Source { emitter } => emitter.input_path(),
        }
    }
}

struct LiveWebDemoState {
    input: LiveWebDemoInput,
    out: Option<Utf8PathBuf>,
    last_modified: Option<SystemTime>,
    generation: u64,
    cached_html: String,
    cached_summary: String,
}

impl LiveWebDemoState {
    fn new(args: &DevTraceWebDemoArgs) -> Result<Self, String> {
        let input = match (&args.from, &args.source) {
            (Some(from), None) => LiveWebDemoInput::Jsonl { path: from.clone() },
            (None, Some(source)) => LiveWebDemoInput::Source {
                emitter: Box::new(incremental_emitter(
                    source,
                    args.standalone,
                    &args.profile,
                    &args.optimize,
                    "web-demo",
                )?),
            },
            (None, None) => return Err("pass either --from TRACE_JSONL or --source FE_FILE".into()),
            (Some(_), Some(_)) => return Err("pass only one of --from or --source".into()),
        };
        let mut state = Self {
            input,
            out: args.out.clone(),
            last_modified: None,
            generation: 0,
            cached_html: String::new(),
            cached_summary: String::new(),
        };
        state.refresh(true)?;
        Ok(state)
    }

    fn refresh(&mut self, force: bool) -> Result<bool, String> {
        let modified = file_modified(self.input.path());
        if !force && !self.cached_html.is_empty() && modified == self.last_modified {
            return Ok(false);
        }

        let generation = self.generation + 1;
        let rendered = match &mut self.input {
            LiveWebDemoInput::Jsonl { path } => {
                let snapshot = super::read_trace_snapshot_jsonl_from_path(path)?;
                render_snapshot(&snapshot, None, true)?
            }
            LiveWebDemoInput::Source { emitter } => {
                render_incremental_trace(emitter, generation, true)?
            }
        };
        if let Some(out) = &self.out {
            fs::write(out.as_std_path(), &rendered.html)
                .map_err(|err| format!("failed to write web demo {out}: {err}"))?;
        }

        self.generation = generation;
        self.last_modified = modified;
        self.cached_summary = format!(
            "generation={} source_confidence={} loop_bytecode_pcs={} {}",
            self.generation,
            rendered.source_confidence(),
            rendered.bytecode_count(),
            rendered.salsa_summary()
        );
        self.cached_html = rendered.html;
        Ok(true)
    }
}

fn serve_trace_web_demo(args: &DevTraceWebDemoArgs) -> Result<String, String> {
    let state = Arc::new(Mutex::new(LiveWebDemoState::new(args)?));
    let listener = TcpListener::bind(("127.0.0.1", args.port)).map_err(|err| {
        format!(
            "failed to bind trace web demo server on port {}: {err}",
            args.port
        )
    })?;
    eprintln!(
        "serving origin trace web demo at http://127.0.0.1:{}/\nwatching: {}\nmirroring: {}",
        args.port,
        state
            .lock()
            .map_err(|_| "trace web demo state lock poisoned".to_string())?
            .input
            .path(),
        args.out
            .as_ref()
            .map(|path| path.to_string())
            .unwrap_or_else(|| "disabled; served from memory".to_string()),
    );

    for stream in listener.incoming() {
        let stream = stream.map_err(|err| format!("trace web demo server accept failed: {err}"))?;
        let state = state.clone();
        std::thread::spawn(move || {
            if let Err(err) = handle_live_connection(stream, state) {
                eprintln!("trace web demo request failed: {err}");
            }
        });
    }
    Ok("trace web demo server stopped".to_string())
}

fn handle_live_connection(
    mut stream: TcpStream,
    state: Arc<Mutex<LiveWebDemoState>>,
) -> Result<(), String> {
    let mut first_line = String::new();
    {
        let mut reader = BufReader::new(
            stream
                .try_clone()
                .map_err(|err| format!("failed to clone HTTP stream: {err}"))?,
        );
        reader
            .read_line(&mut first_line)
            .map_err(|err| format!("failed to read HTTP request: {err}"))?;
    }
    let path = first_line
        .split_whitespace()
        .nth(1)
        .unwrap_or("/")
        .split('?')
        .next()
        .unwrap_or("/");

    match path {
        "/" | "/index.html" => {
            let (html, summary) = {
                let mut state = state
                    .lock()
                    .map_err(|_| "trace web demo state lock poisoned".to_string())?;
                state.refresh(false)?;
                (state.cached_html.clone(), state.cached_summary.clone())
            };
            write_http_response(&mut stream, "200 OK", "text/html; charset=utf-8", &html)
                .map_err(|err| format!("failed to write HTTP response: {err}"))?;
            eprintln!("served trace web demo {summary}");
        }
        "/events" => serve_event_stream(&mut stream, state)?,
        "/health" => {
            write_http_response(&mut stream, "200 OK", "text/plain; charset=utf-8", "ok\n")
                .map_err(|err| format!("failed to write health response: {err}"))?;
        }
        _ => {
            write_http_response(
                &mut stream,
                "404 Not Found",
                "text/plain; charset=utf-8",
                "not found\n",
            )
            .map_err(|err| format!("failed to write 404 response: {err}"))?;
        }
    }
    Ok(())
}

fn serve_event_stream(
    stream: &mut TcpStream,
    state: Arc<Mutex<LiveWebDemoState>>,
) -> Result<(), String> {
    stream
        .write_all(
            b"HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\nConnection: keep-alive\r\n\r\n",
        )
        .map_err(|err| format!("failed to start event stream: {err}"))?;
    let mut last_generation = {
        let state = state
            .lock()
            .map_err(|_| "trace web demo state lock poisoned".to_string())?;
        state.generation
    };
    stream
        .write_all(format!("event: ready\ndata: {last_generation}\n\n").as_bytes())
        .map_err(|err| format!("failed to write ready event: {err}"))?;
    stream
        .flush()
        .map_err(|err| format!("failed to flush ready event: {err}"))?;

    loop {
        std::thread::sleep(Duration::from_millis(700));
        let event = {
            let mut state = state
                .lock()
                .map_err(|_| "trace web demo state lock poisoned".to_string())?;
            match state.refresh(false) {
                Ok(true) if state.generation != last_generation => {
                    last_generation = state.generation;
                    format!("event: reload\ndata: {}\n\n", state.generation)
                }
                Ok(_) => ": keepalive\n\n".to_string(),
                Err(err) => format!("event: trace-error\ndata: {}\n\n", sse_escape(&err)),
            }
        };
        stream
            .write_all(event.as_bytes())
            .map_err(|err| format!("failed to write event stream: {err}"))?;
        stream
            .flush()
            .map_err(|err| format!("failed to flush event stream: {err}"))?;
    }
}

fn write_http_response(
    stream: &mut TcpStream,
    status: &str,
    content_type: &str,
    body: &str,
) -> std::io::Result<()> {
    write!(
        stream,
        "HTTP/1.1 {status}\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
    )?;
    stream.write_all(body.as_bytes())
}

fn file_modified(path: &Utf8PathBuf) -> Option<SystemTime> {
    fs::metadata(path.as_std_path())
        .ok()
        .and_then(|metadata| metadata.modified().ok())
}

fn sse_escape(value: &str) -> String {
    value.replace(['\r', '\n'], " ")
}

#[derive(Debug, Serialize)]
struct WebDemoModel {
    metadata: DemoMetadata,
    provenance: DemoProvenanceStatus,
    counts: DemoCounts,
    salsa: Option<DemoSalsaStats>,
    audit: Option<ClosureAuditReport>,
    static_analysis: Option<StaticAnalysisReport>,
    attribution_audit: Option<AttributionAuditReport>,
    source: DemoSource,
    panels: Vec<DemoPanel>,
    closures: Vec<DemoClosure>,
    bytecode_count: usize,
    notes: Vec<String>,
}

#[derive(Debug, Serialize)]
struct DemoMetadata {
    input_path: String,
    target: String,
    data_source: String,
    compiler_commit: String,
    flags: Vec<String>,
}

#[derive(Debug, Serialize)]
struct DemoCounts {
    facts: usize,
    origin_edges: usize,
    instructions: usize,
    source_spans: usize,
}

#[derive(Debug, Serialize)]
struct DemoProvenanceStatus {
    source_to_optimized: String,
    optimized_to_prepared: String,
    prepared_to_bytecode: String,
    summary: String,
}

#[derive(Debug, Serialize)]
struct DemoSalsaStats {
    mode: String,
    generation: u64,
    elapsed_ms: u128,
    will_execute: usize,
    memo_reuse: usize,
    stale_outputs: usize,
    discarded: usize,
    cycle_iterations: usize,
}

#[derive(Debug, Serialize)]
struct DemoSource {
    display_name: String,
    confidence: String,
    lines: Vec<DemoSourceLine>,
    related_sources: Vec<DemoRelatedSource>,
}

#[derive(Debug, Serialize)]
struct DemoRelatedSource {
    display_name: String,
    origin: String,
    lines: Vec<DemoSourceLine>,
}

#[derive(Debug, Serialize)]
struct DemoSourceLine {
    row_id: String,
    number: u32,
    text: String,
    classes: Vec<String>,
    display_status: Option<DemoDisplayStatus>,
}

#[derive(Debug, Serialize)]
struct DemoPanel {
    id: String,
    title: String,
    summary: String,
    rows: Vec<DemoPanelRow>,
}

#[derive(Debug, Serialize)]
struct DemoPanelRow {
    row_id: String,
    key: Option<String>,
    kind: DemoPaneRowKind,
    indent: u8,
    label: String,
    meta: String,
    text: String,
    compact_text: String,
    display_status: Option<DemoDisplayStatus>,
    rail_classes: DemoRailClasses,
    classes: Vec<String>,
    debug: DemoRowDebugInfo,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[allow(dead_code)]
#[serde(rename_all = "snake_case")]
enum DemoPaneRowKind {
    FileHeader,
    FunctionHeader,
    BlockHeader,
    Instruction,
    Statement,
    Terminator,
    BoundaryMarker,
    DerivedBytecodeBlockHeader,
    Blank,
}

#[derive(Clone, Copy, Debug, Serialize)]
#[allow(dead_code)]
#[serde(rename_all = "snake_case")]
enum DemoDisplayStatus {
    Exact,
    Generated,
    GeneratedDownstream,
    Context,
    PreparedLinked,
    MissingOptimizedToPrepared,
    MissingDownstreamLineage,
    SourceOnly,
    CompilerGenerated,
    Unmapped,
    Ambiguous,
    Invalid,
}

#[derive(Debug, Default, Serialize)]
struct DemoRailClasses {
    exact: Vec<String>,
    generated: Vec<String>,
    prepared: Vec<String>,
    context: Vec<String>,
    boundary: Vec<String>,
    legacy: Vec<String>,
}

#[derive(Debug, Serialize)]
struct DemoRowDebugInfo {
    origin_key: Option<String>,
    origin_kind: Option<String>,
    owner_key: Option<String>,
    local_key: Option<String>,
    instruction_index: Option<u32>,
    raw_text: String,
}

fn build_demo_model(snapshot: &TraceSnapshot, salsa: Option<DemoSalsaStats>) -> WebDemoModel {
    let service = TraceIntrospectionService::new(snapshot.clone());
    let loop_report = service
        .loop_contents(LoopContentsRequest::default())
        .expect("loop-contents query over validated snapshot should not fail");
    let source_text = read_source_text(&snapshot.metadata().input_path).unwrap_or_default();
    let source_texts =
        source_texts_by_file(snapshot, &snapshot.metadata().input_path, &source_text);
    let index = DemoIndex::new(snapshot, &source_texts);
    let exact_source_spans = index
        .source_spans
        .values()
        .any(|span| span.origin.kind() != "code.object");
    let source_confidence = if exact_source_spans {
        "exact source spans present".to_string()
    } else if index.source_spans.is_empty() {
        "no source spans in trace".to_string()
    } else {
        "coarse file-level fallback".to_string()
    };

    let bytecode_count = loop_report.target_instructions.len();
    let closure_set = build_origin_closure_set(
        snapshot.metadata().input_path.as_str(),
        snapshot,
        &loop_report,
    );
    let closures = closure_set.closures;
    let mut classes_by_key = classes_by_origin_key(&closures);
    let component_classes_by_key = component_classes_by_origin_key(snapshot);
    merge_classes_by_key(&mut classes_by_key, component_classes_by_key.clone());
    let rails_by_key = rails_by_origin_key(&closures);
    let source_lines = source_lines(
        snapshot.metadata().input_path.as_str(),
        &source_text,
        &closures,
        &component_classes_by_key,
    );
    let related_sources = related_source_sections(
        snapshot.metadata().input_path.as_str(),
        &source_texts,
        &index,
        &closures,
        &component_classes_by_key,
    );
    let audit_source_lines = source_lines
        .iter()
        .map(|line| OriginClosureSourceLine {
            number: line.number,
            text: line.text.clone(),
        })
        .collect::<Vec<_>>();
    let audit = audit_origin_closures(
        snapshot.metadata().input_path.as_str(),
        snapshot.metadata().target.as_str(),
        &super::format_data_source(snapshot.metadata()),
        bytecode_count,
        &closures,
        &audit_source_lines,
        snapshot,
    );
    let static_analysis = static_analysis_report(snapshot);
    let attribution_audit = service
        .attribution_audit()
        .expect("attribution-audit query over validated snapshot should not fail");
    let loop_block_roles = loop_block_roles(&loop_report);
    let mut panels = build_origin_panels(&index, &classes_by_key, &rails_by_key, &loop_block_roles);
    panels.insert(1, loop_panel(&loop_report, &classes_by_key, &rails_by_key));

    WebDemoModel {
        metadata: DemoMetadata {
            input_path: snapshot.metadata().input_path.clone(),
            target: snapshot.metadata().target.clone(),
            data_source: super::format_data_source(snapshot.metadata()),
            compiler_commit: snapshot.metadata().compiler_commit.clone(),
            flags: snapshot.metadata().flags.clone(),
        },
        provenance: demo_provenance_status(&closures, &attribution_audit),
        counts: DemoCounts {
            facts: snapshot.facts().len(),
            origin_edges: closure_set.edge_count,
            instructions: closure_set.instruction_count,
            source_spans: closure_set.source_span_count,
        },
        salsa,
        audit: Some(audit),
        static_analysis: Some(static_analysis),
        attribution_audit: Some(attribution_audit),
        source: DemoSource {
            display_name: snapshot.metadata().input_path.clone(),
            confidence: source_confidence,
            lines: source_lines,
            related_sources,
        },
        panels,
        closures,
        bytecode_count,
        notes: demo_notes(bytecode_count, exact_source_spans),
    }
}

fn demo_provenance_status(
    closures: &[DemoClosure],
    attribution_audit: &AttributionAuditReport,
) -> DemoProvenanceStatus {
    let closure_source_to_optimized = closures.iter().any(|closure| {
        closure.counts.hir > 0
            && (closure.counts.sonatina_post > 0 || closure.counts.sonatina_pre > 0)
    });
    let closure_optimized_to_prepared = closures.iter().any(|closure| {
        closure
            .keys
            .iter()
            .any(|key| key.contains("sonatina.postopt."))
            && closure
                .keys
                .iter()
                .any(|key| key.contains("sonatina.evm.prepared."))
    });
    let closure_prepared_to_bytecode = closures.iter().any(|closure| {
        closure
            .keys
            .iter()
            .any(|key| key.contains("sonatina.evm.prepared."))
            && closure.keys.iter().any(|key| key.contains("bytecode.pc"))
    });
    demo_provenance_status_from_signals(
        closure_source_to_optimized,
        closure_optimized_to_prepared,
        closure_prepared_to_bytecode,
        attribution_audit.optimized_sonatina_linked_pcs,
        attribution_audit.prepared_linked_pcs,
        attribution_audit.missing_optimized_to_prepared_lineage_pcs,
        attribution_audit.non_exact_optimized_to_prepared_lineage_pcs,
    )
}

fn demo_provenance_status_from_signals(
    closure_source_to_optimized: bool,
    closure_optimized_to_prepared: bool,
    closure_prepared_to_bytecode: bool,
    optimized_linked_pcs: usize,
    prepared_linked_pcs: usize,
    missing_optimized_to_prepared_pcs: usize,
    non_exact_optimized_to_prepared_pcs: usize,
) -> DemoProvenanceStatus {
    let source_to_optimized = closure_source_to_optimized || optimized_linked_pcs > 0;
    let optimized_to_prepared = if closure_optimized_to_prepared {
        "available"
    } else if non_exact_optimized_to_prepared_pcs > 0 {
        "non-exact"
    } else if prepared_linked_pcs > 0 && missing_optimized_to_prepared_pcs == 0 {
        "available"
    } else {
        "missing"
    };
    let prepared_to_bytecode = closure_prepared_to_bytecode || prepared_linked_pcs > 0;
    let summary = if source_to_optimized
        && optimized_to_prepared == "available"
        && prepared_to_bytecode
    {
        "source to optimized to prepared to bytecode available".to_string()
    } else if source_to_optimized && optimized_to_prepared == "non-exact" && prepared_to_bytecode {
        "bytecode has generated/context optimized to prepared explanation, but not exact source attribution".to_string()
    } else if source_to_optimized && prepared_to_bytecode {
        "bytecode is prepared-linked; exact source attribution needs optimized to prepared lineage"
            .to_string()
    } else {
        "provenance is partial; inspect gaps for missing compiler evidence".to_string()
    };
    DemoProvenanceStatus {
        source_to_optimized: status_word(source_to_optimized),
        optimized_to_prepared: optimized_to_prepared.to_string(),
        prepared_to_bytecode: status_word(prepared_to_bytecode),
        summary,
    }
}

fn rails_by_origin_key(closures: &[DemoClosure]) -> BTreeMap<String, Vec<String>> {
    let mut rails = BTreeMap::<String, BTreeSet<String>>::new();
    for closure in closures {
        for key in &closure.generated_keys {
            rails
                .entry(key.clone())
                .or_default()
                .insert("origin-generated".to_string());
        }
        for key in &closure.contextual_keys {
            rails
                .entry(key.clone())
                .or_default()
                .insert("origin-contextual".to_string());
        }
        for key in &closure.structural_keys {
            rails
                .entry(key.clone())
                .or_default()
                .insert("origin-structural".to_string());
        }
    }
    rails
        .into_iter()
        .map(|(key, value)| (key, value.into_iter().collect()))
        .collect()
}

fn merge_classes_by_key(
    classes_by_key: &mut BTreeMap<String, Vec<String>>,
    extra_classes: BTreeMap<String, Vec<String>>,
) {
    for (key, classes) in extra_classes {
        let entry = classes_by_key.entry(key).or_default();
        let mut seen = entry.iter().cloned().collect::<BTreeSet<_>>();
        for class in classes {
            if seen.insert(class.clone()) {
                entry.push(class);
            }
        }
    }
}

fn source_snippet_for_span(source_text: &str, span: &SourceSpanFact) -> Option<DemoSourceSnippet> {
    let start = span.start_byte as usize;
    let end = span.end_byte as usize;
    if start >= end || end > source_text.len() {
        return None;
    }
    let raw = source_text.get(start..end)?;
    let text = compact_source_snippet(raw)?;
    Some(DemoSourceSnippet {
        text,
        line: span.start_line,
        start_byte: span.start_byte,
    })
}

fn compact_source_snippet(raw: &str) -> Option<String> {
    let text = raw.split_whitespace().collect::<Vec<_>>().join(" ");
    if text.is_empty() {
        None
    } else if text.len() > 80 {
        let mut truncated = text.chars().take(77).collect::<String>();
        truncated.push_str("...");
        Some(truncated)
    } else {
        Some(text)
    }
}

fn source_context_for_key(
    key: &OriginExportKey,
    index: &DemoIndex<'_>,
) -> Option<DemoSourceContext> {
    if let Some(snippet) = index.source_snippets.get(key) {
        return Some(DemoSourceContext {
            snippet: snippet.clone(),
            kind: DemoSourceContextKind::Exact,
        });
    }

    let mut candidates = Vec::new();
    let outgoing = index.edges_by_from.get(key).into_iter().flatten().copied();
    let incoming = index.edges_by_to.get(key).into_iter().flatten().copied();
    for edge in outgoing.chain(incoming) {
        let Some(kind) = source_context_kind(edge.traversal_class()) else {
            continue;
        };
        let other = if &edge.from == key {
            &edge.to
        } else {
            &edge.from
        };
        if let Some(snippet) = index.source_snippets.get(other) {
            candidates.push(DemoSourceContext {
                snippet: snippet.clone(),
                kind,
            });
        }
    }
    candidates.sort_by(|left, right| {
        left.kind
            .cmp(&right.kind)
            .then(left.snippet.line.cmp(&right.snippet.line))
            .then(left.snippet.start_byte.cmp(&right.snippet.start_byte))
            .then(left.snippet.text.cmp(&right.snippet.text))
    });
    candidates.into_iter().next()
}

fn source_context_kind(class: OriginEdgeTraversalClass) -> Option<DemoSourceContextKind> {
    match class {
        OriginEdgeTraversalClass::ExactAttribution | OriginEdgeTraversalClass::SnapshotAlias => {
            Some(DemoSourceContextKind::Exact)
        }
        OriginEdgeTraversalClass::Synthetic => Some(DemoSourceContextKind::Generated),
        OriginEdgeTraversalClass::Contextual => Some(DemoSourceContextKind::Context),
        OriginEdgeTraversalClass::Structural | OriginEdgeTraversalClass::Unmapped => None,
    }
}

fn status_word(available: bool) -> String {
    if available { "available" } else { "missing" }.to_string()
}

struct DemoIndex<'a> {
    origin_nodes: BTreeMap<OriginExportKey, &'a OriginNodeFact>,
    instructions: BTreeMap<OriginExportKey, &'a InstructionFact>,
    source_files: BTreeMap<OriginExportKey, &'a SourceFileFact>,
    source_spans: BTreeMap<OriginExportKey, &'a SourceSpanFact>,
    source_snippets: BTreeMap<OriginExportKey, DemoSourceSnippet>,
    edges_by_from: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
    edges_by_to: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
}

impl<'a> DemoIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot, source_texts: &BTreeMap<OriginExportKey, String>) -> Self {
        let mut origin_nodes = BTreeMap::new();
        let mut instructions = BTreeMap::new();
        let mut source_files = BTreeMap::new();
        let mut source_spans = BTreeMap::new();
        let mut edges_by_from = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut edges_by_to = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();

        for fact in snapshot.facts() {
            match fact {
                TraceFact::OriginNode(node) => {
                    origin_nodes.insert(node.key.clone(), node);
                }
                TraceFact::Instruction(instruction) => {
                    instructions.insert(instruction.instruction.clone(), instruction);
                }
                TraceFact::SourceFile(source_file) => {
                    source_files.insert(source_file.file_key.clone(), source_file);
                }
                TraceFact::SourceSpan(span) => {
                    source_spans.insert(span.origin.clone(), span);
                }
                TraceFact::OriginEdge(edge) => {
                    edges_by_from
                        .entry(edge.from.clone())
                        .or_default()
                        .push(edge);
                    edges_by_to.entry(edge.to.clone()).or_default().push(edge);
                }
                _ => {}
            }
        }
        let source_snippets = source_spans
            .iter()
            .filter_map(|(origin, span)| {
                source_texts
                    .get(&span.file)
                    .and_then(|source_text| source_snippet_for_span(source_text, span))
                    .map(|snippet| (origin.clone(), snippet))
            })
            .collect();

        Self {
            origin_nodes,
            instructions,
            source_files,
            source_spans,
            source_snippets,
            edges_by_from,
            edges_by_to,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct DemoSourceSnippet {
    text: String,
    line: u32,
    start_byte: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum DemoSourceContextKind {
    Exact,
    Generated,
    Context,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct DemoSourceContext {
    snippet: DemoSourceSnippet,
    kind: DemoSourceContextKind,
}

fn source_lines(
    input_path: &str,
    source_text: &str,
    closures: &[DemoClosure],
    component_classes_by_key: &BTreeMap<String, Vec<String>>,
) -> Vec<DemoSourceLine> {
    let mut exact_classes_by_line = BTreeMap::<u32, BTreeSet<String>>::new();
    let mut enclosing_classes_by_line = BTreeMap::<u32, BTreeSet<String>>::new();
    for closure in closures {
        for span in &closure.source_spans {
            if span.confidence != "direct"
                || !source_owner_matches_input(&span.file_owner, input_path)
            {
                continue;
            }
            if span.start_line == span.end_line {
                let classes = exact_classes_by_line.entry(span.start_line).or_default();
                insert_source_span_classes(classes, span, closure, component_classes_by_key, true);
            } else {
                for line in span.start_line..=span.end_line {
                    let classes = enclosing_classes_by_line.entry(line).or_default();
                    insert_source_span_classes(
                        classes,
                        span,
                        closure,
                        component_classes_by_key,
                        false,
                    );
                }
            }
        }
    }
    let text = if source_text.is_empty() {
        "(source file not readable from this working directory)"
    } else {
        source_text
    };
    text.lines()
        .enumerate()
        .map(|(index, text)| {
            let number = index as u32 + 1;
            let classes = exact_classes_by_line
                .get(&number)
                .or_else(|| enclosing_classes_by_line.get(&number))
                .map(|classes| classes.iter().cloned().collect::<Vec<_>>())
                .unwrap_or_default();
            DemoSourceLine {
                row_id: demo_source_row_id(number),
                number,
                text: text.to_string(),
                display_status: display_status_for_source_line(&classes),
                classes,
            }
        })
        .collect()
}

fn display_status_for_source_line(classes: &[String]) -> Option<DemoDisplayStatus> {
    let _ = classes;
    None
}

fn demo_source_row_id(line_number: u32) -> String {
    format!("source-main-line-{line_number}")
}

fn demo_related_source_row_id(file_key: &OriginExportKey, line_number: u32) -> String {
    format!(
        "source-related-{:08x}-line-{line_number}",
        demo_stable_hash(&file_key.canonical_storage_key())
    )
}

fn demo_origin_row_id(storage_key: &str) -> String {
    format!("origin-{:08x}", demo_stable_hash(storage_key))
}

fn demo_stable_hash(value: &str) -> u32 {
    let mut hash = 2166136261u32;
    for byte in value.bytes() {
        hash ^= u32::from(byte);
        hash = hash.wrapping_mul(16777619);
    }
    hash
}

fn insert_source_span_classes(
    classes: &mut BTreeSet<String>,
    span: &trace_query::origin_closure::OriginClosureSourceSpan,
    closure: &DemoClosure,
    component_classes_by_key: &BTreeMap<String, Vec<String>>,
    include_component_classes: bool,
) {
    classes.insert(closure.class_name.clone());
    if include_component_classes
        && let Some(component_classes) = component_classes_by_key.get(&span.origin_key)
    {
        classes.extend(
            component_classes
                .iter()
                .filter(|class| is_component_class(class))
                .cloned(),
        );
    }
}

fn is_component_class(class: &str) -> bool {
    class.starts_with("exact-c-")
        || class.starts_with("generated-c-")
        || class.starts_with("prepared-c-")
        || class.starts_with("context-c-")
        || class.starts_with("structural-c-")
}

fn related_source_sections(
    input_path: &str,
    source_texts: &BTreeMap<OriginExportKey, String>,
    index: &DemoIndex<'_>,
    closures: &[DemoClosure],
    component_classes_by_key: &BTreeMap<String, Vec<String>>,
) -> Vec<DemoRelatedSource> {
    let mut classes_by_file_line =
        BTreeMap::<OriginExportKey, BTreeMap<u32, BTreeSet<String>>>::new();
    for closure in closures {
        for span in &closure.source_spans {
            if span.confidence != "direct"
                || source_owner_matches_input(&span.file_owner, input_path)
            {
                continue;
            }
            let Some(file_key) = source_file_key_for_owner(index, &span.file_owner) else {
                continue;
            };
            for line in span.start_line..=span.end_line {
                let classes = classes_by_file_line
                    .entry(file_key.clone())
                    .or_default()
                    .entry(line)
                    .or_default();
                insert_source_span_classes(
                    classes,
                    span,
                    closure,
                    component_classes_by_key,
                    span.start_line == span.end_line,
                );
            }
        }
    }

    classes_by_file_line
        .into_iter()
        .filter_map(|(file_key, classes_by_line)| {
            let source_text = source_texts.get(&file_key)?;
            let lines = source_text
                .lines()
                .enumerate()
                .filter_map(|(index, text)| {
                    let number = index as u32 + 1;
                    let classes = classes_by_line
                        .get(&number)?
                        .iter()
                        .cloned()
                        .collect::<Vec<_>>();
                    let display_status = display_status_for_source_line(&classes);
                    Some(DemoSourceLine {
                        row_id: demo_related_source_row_id(&file_key, number),
                        number,
                        text: text.to_string(),
                        classes,
                        display_status,
                    })
                })
                .collect::<Vec<_>>();
            if lines.is_empty() {
                return None;
            }
            let display_name = index
                .source_files
                .get(&file_key)
                .map(|source| source.display_name.clone())
                .unwrap_or_else(|| file_key.owner_key().to_string());
            Some(DemoRelatedSource {
                display_name,
                origin: file_key.owner_key().to_string(),
                lines,
            })
        })
        .collect()
}

fn source_file_key_for_owner(index: &DemoIndex<'_>, owner: &str) -> Option<OriginExportKey> {
    index
        .source_files
        .keys()
        .find(|file_key| file_key.owner_key() == owner)
        .cloned()
}

fn truncate_for_report(value: &str, max_chars: usize) -> String {
    if value.chars().count() <= max_chars {
        return value.to_string();
    }
    let prefix_len = max_chars.saturating_sub(3);
    let mut truncated = value.chars().take(prefix_len).collect::<String>();
    truncated.push_str("...");
    truncated
}

fn render_closure_audit_report(report: &ClosureAuditReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace audit-closures\n");
    out.push_str("Confidence: deterministic audit over derived connected trace regions; does not prove semantic completeness.\n");
    out.push_str(&format!("Input: {}\n", report.input_path));
    out.push_str(&format!("Target: {}\n", report.target));
    out.push_str(&format!("Data source: {}\n", report.data_source));
    out.push_str(&format!(
        "Trace selections: {} total, {} suspicious\n\n",
        report.total_closures, report.suspicious_closures
    ));
    out.push_str(&format!(
        "Source span groups: {} total, {} with mixed trace connectivity\n",
        report.span_groups.total_groups, report.span_groups.mixed_connectivity_groups
    ));
    if !report.span_group_details.is_empty() {
        out.push_str("Mixed source span groups:\n");
        for group in &report.span_group_details {
            out.push_str(&format!(
                "  {}:{}..{} trace_selections={} target_connected={} source_only={} text={}\n",
                group.file_owner,
                group.start_byte,
                group.end_byte,
                group.closures,
                group.closures_with_targets,
                group.source_only_closures,
                group
                    .source_text
                    .as_deref()
                    .map(|text| truncate_for_report(text, 96))
                    .unwrap_or_else(|| "unknown".to_string())
            ));
            for member in &group.target_connected_members {
                out.push_str(&format!(
                    "    target: {} highest_phase={} {}\n",
                    member.class_name,
                    member.highest_phase_reached.as_str(),
                    truncate_for_report(&member.label, 160)
                ));
            }
            for member in &group.source_only_members {
                out.push_str(&format!(
                    "    source-only: {} highest_phase={} {}\n",
                    member.class_name,
                    member.highest_phase_reached.as_str(),
                    truncate_for_report(&member.label, 160)
                ));
            }
        }
    }
    out.push_str("Primary classifications:\n");
    for (primary, count) in &report.primary_counts {
        out.push_str(&format!("  {:>31}: {count}\n", primary.as_str()));
    }
    if !report.symptom_counts.is_empty() {
        out.push_str("Symptoms (multi-label):\n");
        for (symptom, count) in &report.symptom_counts {
            out.push_str(&format!("  {:>31}: {count}\n", symptom.as_str()));
        }
    }
    out.push_str("\nSuspicious trace selections:\n");
    for closure in &report.closures {
        if !closure.suspicious {
            continue;
        }
        let symptoms = closure
            .symptoms
            .iter()
            .map(|symptom| symptom.as_str())
            .collect::<Vec<_>>()
            .join(",");
        out.push_str(&format!(
            "  {} primary={} symptoms=[{}] {}\n",
            closure.class_name,
            closure.primary.as_str(),
            symptoms,
            closure.label
        ));
        out.push_str(&format!(
            "    highest_phase={} phases: HIR={} MIR={} pre={} post={} prepared={} bytecode={} keys={} edges={}\n",
            closure.highest_phase_reached.as_str(),
            closure.counts.hir,
            closure.counts.mir,
            closure.counts.sonatina_pre,
            closure.counts.sonatina_post,
            closure.counts.sonatina_prepared,
            closure.counts.bytecode,
            closure.key_count,
            closure.edge_count
        ));
        out.push_str(&format!(
            "    traversal: mode={} truncated={} max_depth={} max_nodes={} skipped_hubs={}\n",
            closure.traversal.mode,
            closure.traversal.truncated,
            closure.traversal.max_depth,
            closure.traversal.max_nodes,
            closure.traversal.skipped_hubs.len()
        ));
        for hub in &closure.traversal.skipped_hubs {
            out.push_str(&format!("    skipped hub: {hub}\n"));
        }
        for span in &closure.source_spans {
            out.push_str(&format!("    source: {span}\n"));
        }
        for note in &closure.notes {
            out.push_str(&format!("    note: {note}\n"));
        }
    }
    if report.suspicious_closures == 0 {
        out.push_str("  none\n");
    }
    out
}

fn build_origin_panels(
    index: &DemoIndex<'_>,
    classes_by_key: &BTreeMap<String, Vec<String>>,
    rails_by_key: &BTreeMap<String, Vec<String>>,
    loop_block_roles: &BTreeMap<String, String>,
) -> Vec<DemoPanel> {
    [
        (
            "hir",
            "HIR",
            "source-level expression and statement origins",
        ),
        (
            "mir",
            "MIR",
            "runtime MIR origins and storage-level lowering",
        ),
        (
            "sonatina-pre",
            "Sonatina Pre-Opt",
            "Sonatina CFG before optimization",
        ),
        (
            "sonatina-post",
            "Sonatina Post-Opt",
            "Sonatina CFG after optimization",
        ),
        (
            "sonatina-prepared",
            "EVM Prepared",
            "Sonatina EVM prepared/backend instructions used by bytecode pc-map",
        ),
        (
            "bytecode",
            "Bytecode",
            "final runtime bytecode PCs; highlighted rows show prepared/source evidence rails",
        ),
    ]
    .into_iter()
    .map(|(id, title, summary)| DemoPanel {
        id: id.to_string(),
        title: title.to_string(),
        summary: summary.to_string(),
        rows: panel_rows(id, index, classes_by_key, rails_by_key, loop_block_roles),
    })
    .collect()
}

fn loop_block_roles(loop_report: &trace_query::LoopContentsReport) -> BTreeMap<String, String> {
    loop_report
        .blocks
        .iter()
        .map(|block| {
            (
                block.block.canonical_storage_key(),
                format!("loop {}", block.role),
            )
        })
        .collect()
}

fn panel_rows(
    panel: &str,
    index: &DemoIndex<'_>,
    classes_by_key: &BTreeMap<String, Vec<String>>,
    rails_by_key: &BTreeMap<String, Vec<String>>,
    loop_block_roles: &BTreeMap<String, String>,
) -> Vec<DemoPanelRow> {
    let mut keys = index
        .origin_nodes
        .keys()
        .filter(|key| key_belongs_to_panel(key, panel))
        .filter(|key| classes_by_key.contains_key(&key.canonical_storage_key()))
        .cloned()
        .collect::<Vec<_>>();
    keys.sort_by(|left, right| compare_panel_keys(left, right, index));
    keys.into_iter()
        .map(|key| origin_panel_row(&key, index, classes_by_key, rails_by_key, loop_block_roles))
        .collect()
}

fn compare_panel_keys(
    left: &OriginExportKey,
    right: &OriginExportKey,
    index: &DemoIndex<'_>,
) -> std::cmp::Ordering {
    match (index.instructions.get(left), index.instructions.get(right)) {
        (Some(left), Some(right)) => left
            .function
            .canonical_storage_key()
            .cmp(&right.function.canonical_storage_key())
            .then(left.index.cmp(&right.index))
            .then(
                left.instruction
                    .canonical_storage_key()
                    .cmp(&right.instruction.canonical_storage_key()),
            ),
        (Some(left), None) => left
            .function
            .canonical_storage_key()
            .as_str()
            .cmp(right.owner_key())
            .then(std::cmp::Ordering::Less),
        (None, Some(right)) => left
            .owner_key()
            .cmp(&right.function.canonical_storage_key())
            .then(std::cmp::Ordering::Greater),
        (None, None) => left
            .owner_key()
            .cmp(right.owner_key())
            .then(left.local_key().cmp(right.local_key())),
    }
}

fn origin_panel_row(
    key: &OriginExportKey,
    index: &DemoIndex<'_>,
    classes_by_key: &BTreeMap<String, Vec<String>>,
    rails_by_key: &BTreeMap<String, Vec<String>>,
    loop_block_roles: &BTreeMap<String, String>,
) -> DemoPanelRow {
    let storage_key = key.canonical_storage_key();
    let instruction = index.instructions.get(key);
    let meta = loop_block_roles
        .get(&storage_key)
        .map(|role| format!("{} · {role}", compact_origin_meta(key, Some(index))))
        .unwrap_or_else(|| compact_origin_meta(key, Some(index)));
    let mut classes = classes_by_key
        .get(&storage_key)
        .cloned()
        .unwrap_or_default();
    classes.extend(rails_by_key.get(&storage_key).cloned().unwrap_or_default());
    if origin_key_is_generated(key) {
        classes.push("origin-generated".to_string());
    }
    let kind = pane_row_kind_for_origin(key, instruction.copied());
    let compact_text = instruction
        .copied()
        .map(compact_instruction_text)
        .unwrap_or_else(|| compact_origin_text(key, index));
    let raw_text = instruction
        .map(|instruction| instruction.mnemonic.clone())
        .unwrap_or_else(|| key.canonical_storage_key());
    let display_status = display_status_for_row(key, kind, &classes);
    DemoPanelRow {
        row_id: demo_origin_row_id(&storage_key),
        key: Some(storage_key.clone()),
        kind,
        indent: pane_row_indent(kind),
        label: compact_origin_label(key, instruction.copied()),
        meta,
        text: compact_text.clone(),
        compact_text,
        display_status,
        rail_classes: split_rail_classes(&classes),
        classes,
        debug: row_debug_info(key, instruction.copied(), raw_text),
    }
}

fn pane_row_kind_for_origin(
    key: &OriginExportKey,
    instruction: Option<&InstructionFact>,
) -> DemoPaneRowKind {
    if instruction.is_some() {
        return match key.kind() {
            "runtime.stmt" => DemoPaneRowKind::Statement,
            "runtime.terminator" => DemoPaneRowKind::Terminator,
            "bytecode.pc" => DemoPaneRowKind::Instruction,
            _ => DemoPaneRowKind::Instruction,
        };
    }
    match key.kind() {
        "source.file" => DemoPaneRowKind::FileHeader,
        "runtime.function" => DemoPaneRowKind::FunctionHeader,
        "runtime.block" => DemoPaneRowKind::BlockHeader,
        "runtime.terminator" => DemoPaneRowKind::Terminator,
        kind if kind.ends_with(".function") => DemoPaneRowKind::FunctionHeader,
        kind if kind.ends_with(".block") => DemoPaneRowKind::BlockHeader,
        kind if kind.contains(".loop") => DemoPaneRowKind::BoundaryMarker,
        "bytecode.pc" => DemoPaneRowKind::Instruction,
        _ => DemoPaneRowKind::Instruction,
    }
}

fn pane_row_indent(kind: DemoPaneRowKind) -> u8 {
    match kind {
        DemoPaneRowKind::FileHeader | DemoPaneRowKind::FunctionHeader => 0,
        DemoPaneRowKind::BlockHeader
        | DemoPaneRowKind::BoundaryMarker
        | DemoPaneRowKind::DerivedBytecodeBlockHeader
        | DemoPaneRowKind::Blank => 1,
        DemoPaneRowKind::Instruction | DemoPaneRowKind::Statement | DemoPaneRowKind::Terminator => {
            2
        }
    }
}

fn display_status_for_row(
    key: &OriginExportKey,
    kind: DemoPaneRowKind,
    classes: &[String],
) -> Option<DemoDisplayStatus> {
    if kind == DemoPaneRowKind::Instruction
        && key.kind() == "bytecode.pc"
        && classes.iter().any(|class| class.starts_with("prepared-c-"))
    {
        return Some(DemoDisplayStatus::PreparedLinked);
    }
    if origin_key_is_generated(key) {
        return Some(DemoDisplayStatus::Generated);
    }
    if classes.iter().any(|class| class == "origin-generated") {
        return Some(DemoDisplayStatus::Generated);
    }
    None
}

fn split_rail_classes(classes: &[String]) -> DemoRailClasses {
    let mut rails = DemoRailClasses::default();
    for class in classes {
        if class.starts_with("exact-c-") {
            rails.exact.push(class.clone());
        } else if class.starts_with("generated-c-") || class == "origin-generated" {
            rails.generated.push(class.clone());
        } else if class.starts_with("prepared-c-") {
            rails.prepared.push(class.clone());
        } else if class.starts_with("context-c-") || class == "origin-contextual" {
            rails.context.push(class.clone());
        } else if class.starts_with("structural-c-") || class == "origin-structural" {
            rails.boundary.push(class.clone());
        } else {
            rails.legacy.push(class.clone());
        }
    }
    rails
}

fn row_debug_info(
    key: &OriginExportKey,
    instruction: Option<&InstructionFact>,
    raw_text: String,
) -> DemoRowDebugInfo {
    DemoRowDebugInfo {
        origin_key: Some(key.canonical_storage_key()),
        origin_kind: Some(key.kind().to_string()),
        owner_key: Some(key.owner_key().to_string()),
        local_key: Some(key.local_key().to_string()),
        instruction_index: instruction.map(|instruction| instruction.index),
        raw_text,
    }
}

fn compact_instruction_text(instruction: &InstructionFact) -> String {
    let mnemonic = compact_instruction_mnemonic(&instruction.mnemonic);
    match instruction.instruction.kind() {
        "bytecode.pc" => mnemonic,
        "runtime.stmt" | "runtime.terminator" => compact_mir_text(&mnemonic),
        _ => format!("%{} = {}", instruction.index, mnemonic),
    }
}

fn compact_instruction_mnemonic(text: &str) -> String {
    let text = text.trim();
    if let Some(rest) = text.strip_prefix("ir[")
        && let Some((_, mnemonic)) = rest.split_once("] ")
    {
        return mnemonic.trim().to_string();
    }
    text.to_string()
}

fn compact_origin_label(key: &OriginExportKey, instruction: Option<&InstructionFact>) -> String {
    if let Some(instruction) = instruction
        && !matches!(key.kind(), "runtime.stmt" | "runtime.terminator")
    {
        return if key.kind() == "bytecode.pc" {
            compact_pc_label(key)
        } else {
            format!("%{}", instruction.index)
        };
    }
    match key.kind() {
        "bytecode.pc" => compact_pc_label(key),
        kind if kind.contains(".block") => compact_block_label(key),
        "runtime.stmt" => compact_runtime_stmt_label(key),
        "runtime.terminator" => compact_runtime_terminator_label(key),
        kind if kind.starts_with("hir.") => compact_hir_label(key),
        kind if kind.starts_with("sonatina.") => compact_sonatina_label(key),
        _ => compact_tail_label(key.local_key()),
    }
}

fn compact_origin_meta(key: &OriginExportKey, index: Option<&DemoIndex<'_>>) -> String {
    let line_suffix = index
        .and_then(|index| source_context_for_key(key, index))
        .map(|context| format!(" · L{}", context.snippet.line))
        .unwrap_or_default();
    match key.kind() {
        "runtime.stmt" => format!("MIR{line_suffix}"),
        "runtime.terminator" => format!("MIR term{line_suffix}"),
        "runtime.block" => format!("MIR block{line_suffix}"),
        "runtime.function" => "MIR function".to_string(),
        "hir.expr" | "hir.stmt" => format!("HIR{line_suffix}"),
        kind if kind.starts_with("sonatina.postopt.") => "Optimized Sonatina".to_string(),
        kind if kind.starts_with("sonatina.preopt.") => "Sonatina pre-opt".to_string(),
        kind if kind.starts_with("sonatina.evm.prepared.") => "EVM prepared".to_string(),
        "bytecode.pc" => "bytecode".to_string(),
        _ => key.kind().to_string(),
    }
}

fn compact_origin_text(key: &OriginExportKey, index: &DemoIndex<'_>) -> String {
    if let Some(context) = source_context_for_key(key, index) {
        let snippet = context.snippet.text;
        return match key.kind() {
            kind if kind.starts_with("hir.") => snippet,
            "runtime.stmt" | "runtime.terminator" | "runtime.block" => match context.kind {
                DemoSourceContextKind::Exact => format!("lowered from {snippet}"),
                DemoSourceContextKind::Generated => format!("generated for {snippet}"),
                DemoSourceContextKind::Context => format!("context for {snippet}"),
            },
            _ => snippet,
        };
    }
    if key.kind().contains(".block") {
        compact_block_label(key)
    } else if origin_key_is_generated(key) {
        match key.kind() {
            "runtime.terminator" => "generated wrapper terminator".to_string(),
            "runtime.stmt" => "generated wrapper statement".to_string(),
            _ => "compiler-generated wrapper code".to_string(),
        }
    } else {
        compact_origin_meta(key, None)
    }
}

fn compact_pc_label(key: &OriginExportKey) -> String {
    key.local_key()
        .strip_prefix("pc:")
        .and_then(|pc| pc.parse::<u32>().ok())
        .map(|pc| format!("{pc:04}"))
        .unwrap_or_else(|| compact_tail_label(key.local_key()))
}

fn compact_block_label(key: &OriginExportKey) -> String {
    let local = key.local_key();
    if let Some(block) = local.strip_prefix("block:") {
        return format!("b{}", compact_tail_label(block));
    }
    compact_tail_label(local)
}

fn compact_runtime_stmt_label(key: &OriginExportKey) -> String {
    let local = key.local_key();
    if let Some(rest) = local.strip_prefix("block:") {
        let parts = rest.split(':').collect::<Vec<_>>();
        if parts.len() >= 3 && parts[1] == "stmt" {
            return format!("bb{}.s{}", parts[0], parts[2]);
        }
    }
    compact_tail_label(local)
}

fn compact_runtime_terminator_label(key: &OriginExportKey) -> String {
    let local = key.local_key();
    if let Some(rest) = local.strip_prefix("block:")
        && let Some(block) = rest.strip_suffix(":terminator")
    {
        return format!("bb{block}.term");
    }
    compact_tail_label(local)
}

fn compact_hir_label(key: &OriginExportKey) -> String {
    let prefix = match key.kind() {
        "hir.expr" => "e",
        "hir.stmt" => "s",
        _ => "h",
    };
    format!("{prefix}{}", compact_tail_label(key.local_key()))
}

fn compact_sonatina_label(key: &OriginExportKey) -> String {
    let local = key.local_key();
    if let Some(inst) = extract_wrapped_id(local, "InstId(") {
        return format!("%{inst}");
    }
    if let Some(block) = extract_wrapped_id(local, "BlockId(") {
        return format!("b{block}");
    }
    if let Some((_, rest)) = local.rsplit_once("header:block") {
        let header = rest
            .split(|ch: char| !ch.is_ascii_digit())
            .next()
            .unwrap_or("");
        if !header.is_empty() {
            return format!("loop b{header}");
        }
    }
    if let Some(func) = extract_wrapped_id(local, "FuncRef(") {
        return format!("fn {func}");
    }
    compact_tail_label(local)
}

fn extract_wrapped_id(value: &str, marker: &str) -> Option<String> {
    let start = value.rfind(marker)? + marker.len();
    let tail = &value[start..];
    let id = tail.split(')').next()?;
    (!id.is_empty()).then(|| id.to_string())
}

fn compact_tail_label(value: &str) -> String {
    value
        .rsplit([':', '$'])
        .next()
        .filter(|tail| !tail.is_empty())
        .unwrap_or(value)
        .replace("InstId(", "")
        .replace("FuncRef(", "")
        .replace("BlockId(", "")
        .replace(')', "")
        .to_string()
}

fn compact_mir_text(text: &str) -> String {
    let normalized = compact_instruction_mnemonic(text);
    let without_phantom = strip_phantom_data(&normalized);
    let with_locals = replace_wrapped_ids(&without_phantom, "RLocalId(", "%");
    let with_layouts = replace_wrapped_ids(&with_locals, "LayoutId(Id(", "layout#");
    let with_runtime = replace_wrapped_ids(&with_layouts, "RuntimeInstance(Id(", "runtime#");
    compact_int_literals(&with_runtime)
        .replace("))", ")")
        .replace(")(", "(")
        .replace("),", ",")
        .replace("](", "]")
}

fn strip_phantom_data(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(start) = rest.find(", PhantomData<") {
        out.push_str(&rest[..start]);
        let skip = &rest[start + 2..];
        let mut depth = 0usize;
        let mut end = skip.len();
        for (idx, ch) in skip.char_indices() {
            match ch {
                '<' => depth += 1,
                '>' => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        end = idx + ch.len_utf8();
                        break;
                    }
                }
                _ => {}
            }
        }
        rest = &skip[end..];
    }
    out.push_str(rest);
    out
}

fn replace_wrapped_ids(text: &str, marker: &str, prefix: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(start) = rest.find(marker) {
        out.push_str(&rest[..start]);
        let tail = &rest[start + marker.len()..];
        if let Some(end) = tail.find(')') {
            out.push_str(prefix);
            out.push_str(&tail[..end]);
            rest = &tail[end + 1..];
        } else {
            out.push_str(&rest[start..]);
            rest = "";
        }
    }
    out.push_str(rest);
    out
}

fn compact_int_literals(text: &str) -> String {
    let mut out = text.to_string();
    let needle = "const Int { bits: ";
    while let Some(start) = out.find(needle) {
        let bits_start = start + needle.len();
        let Some(bits_end) = out[bits_start..]
            .find(", signed: ")
            .map(|idx| bits_start + idx)
        else {
            break;
        };
        let signed_start = bits_end + ", signed: ".len();
        let Some(signed_end) = out[signed_start..]
            .find(", words: [")
            .map(|idx| signed_start + idx)
        else {
            break;
        };
        let value_start = signed_end + ", words: [".len();
        let Some(value_end) = out[value_start..].find("] }").map(|idx| value_start + idx) else {
            break;
        };
        let bits = out[bits_start..bits_end].to_string();
        let signed = out[signed_start..signed_end].trim() == "true";
        let value = out[value_start..value_end].to_string();
        let prefix = if signed { "i" } else { "u" };
        let replacement = format!("const {prefix}{bits} {value}");
        out.replace_range(start..value_end + 3, &replacement);
    }
    out
}

fn key_belongs_to_panel(key: &OriginExportKey, panel: &str) -> bool {
    match panel {
        "hir" => key.kind().starts_with("hir."),
        "mir" => key.kind().starts_with("runtime."),
        "sonatina-pre" => key.kind().starts_with("sonatina.preopt."),
        "sonatina-post" => key.kind().starts_with("sonatina.postopt."),
        "sonatina-prepared" => key.kind().starts_with("sonatina.evm.prepared."),
        "bytecode" => key.kind() == "bytecode.pc",
        _ => false,
    }
}

fn origin_key_is_generated(key: &OriginExportKey) -> bool {
    key.owner_key().contains("__synthetic") || key.local_key().contains("__synthetic")
}

fn loop_panel(
    loop_report: &trace_query::LoopContentsReport,
    classes_by_key: &BTreeMap<String, Vec<String>>,
    rails_by_key: &BTreeMap<String, Vec<String>>,
) -> DemoPanel {
    let mut rows = Vec::new();
    for block in &loop_report.blocks {
        let block_key = block.block.canonical_storage_key();
        let mut block_classes = classes_by_key.get(&block_key).cloned().unwrap_or_default();
        block_classes.extend(rails_by_key.get(&block_key).cloned().unwrap_or_default());
        rows.push(DemoPanelRow {
            row_id: demo_origin_row_id(&block_key),
            key: Some(block_key.clone()),
            kind: DemoPaneRowKind::BlockHeader,
            indent: pane_row_indent(DemoPaneRowKind::BlockHeader),
            label: compact_origin_label(&block.block, None),
            meta: format!("loop {}", block.role),
            text: "static loop boundary".to_string(),
            compact_text: "static loop boundary".to_string(),
            display_status: display_status_for_row(
                &block.block,
                DemoPaneRowKind::BlockHeader,
                &block_classes,
            ),
            rail_classes: split_rail_classes(&block_classes),
            classes: block_classes,
            debug: row_debug_info(&block.block, None, block.block.canonical_storage_key()),
        });
        for instruction in &block.instructions {
            let key = instruction.key.canonical_storage_key();
            let mut classes = classes_by_key.get(&key).cloned().unwrap_or_default();
            classes.extend(rails_by_key.get(&key).cloned().unwrap_or_default());
            let compact_text = format!(
                "%{} = {}",
                instruction.index,
                compact_instruction_mnemonic(&instruction.mnemonic)
            );
            rows.push(DemoPanelRow {
                row_id: demo_origin_row_id(&key),
                key: Some(key.clone()),
                kind: DemoPaneRowKind::Instruction,
                indent: pane_row_indent(DemoPaneRowKind::Instruction),
                label: compact_origin_label(&instruction.key, None),
                meta: "loop instruction".to_string(),
                text: compact_text.clone(),
                compact_text,
                display_status: display_status_for_row(
                    &instruction.key,
                    DemoPaneRowKind::Instruction,
                    &classes,
                ),
                rail_classes: split_rail_classes(&classes),
                classes,
                debug: row_debug_info(&instruction.key, None, instruction.mnemonic.clone()),
            });
        }
    }
    DemoPanel {
        id: "loop".to_string(),
        title: "Loop CFG".to_string(),
        summary: if loop_report.available {
            "compiler-derived static loop membership; not runtime iterations or bytecode proof"
                .to_string()
        } else {
            "loop membership unavailable".to_string()
        },
        rows,
    }
}

fn demo_notes(bytecode_count: usize, exact_source_spans: bool) -> Vec<String> {
    let mut notes = vec![
        "This page is a derived view over validated trace JSONL; it does not define compiler identity.".to_string(),
        "The bytecode pane shows final bytecode instruction facts; highlights show evidence rails, not source ownership by themselves.".to_string(),
    ];
    if bytecode_count == 0 {
        notes.push(
            "No bytecode PCs joined to the selected loop; this is an attribution gap, not evidence that the optimized source work disappeared.".to_string(),
        );
    }
    if !exact_source_spans {
        notes.push(
            "Source attribution is currently coarse: the trace has a file-level code-object span, not exact expression spans.".to_string(),
        );
    }
    notes
}

fn read_source_text(input_path: &str) -> Option<String> {
    let path = std::path::PathBuf::from(input_path);
    let path = if path.is_absolute() {
        path
    } else {
        std::env::current_dir().ok()?.join(path)
    };
    fs::read_to_string(path).ok()
}

fn source_texts_by_file(
    snapshot: &TraceSnapshot,
    input_path: &str,
    input_source_text: &str,
) -> BTreeMap<OriginExportKey, String> {
    let mut texts = BTreeMap::new();
    for fact in snapshot.facts() {
        let TraceFact::SourceFile(source_file) = fact else {
            continue;
        };
        if source_owner_matches_input(source_file.file_key.owner_key(), input_path) {
            texts.insert(source_file.file_key.clone(), input_source_text.to_string());
            continue;
        }
        if let Some(text) = read_source_file_fact_text(source_file) {
            texts.insert(source_file.file_key.clone(), text);
        }
    }
    texts
}

fn trace_workbench_related_source_texts(
    snapshot: &TraceSnapshot,
    input_path: &str,
    input_source_text: &str,
) -> BTreeMap<String, String> {
    let texts = source_texts_by_file(snapshot, input_path, input_source_text);
    let mut related = BTreeMap::new();
    for fact in snapshot.facts() {
        let TraceFact::SourceFile(source_file) = fact else {
            continue;
        };
        let Some(text) = texts.get(&source_file.file_key) else {
            continue;
        };
        related.insert(source_file.file_key.canonical_storage_key(), text.clone());
        related.insert(source_file.file_key.owner_key().to_string(), text.clone());
        related.insert(source_file.uri.clone(), text.clone());
    }
    related
}

fn read_source_file_fact_text(source_file: &SourceFileFact) -> Option<String> {
    let uri = source_file.uri.as_str();
    let path = if let Some(path) = uri.strip_prefix("builtin-std:/") {
        std::env::current_dir()
            .ok()?
            .join("ingots")
            .join("std")
            .join(path)
    } else if let Some(path) = uri.strip_prefix("builtin-core:/") {
        std::env::current_dir()
            .ok()?
            .join("ingots")
            .join("core")
            .join(path)
    } else if let Some(path) = uri.strip_prefix("file://") {
        std::path::PathBuf::from(path)
    } else {
        let candidate = std::path::PathBuf::from(uri);
        if candidate.is_absolute() {
            candidate
        } else {
            std::env::current_dir().ok()?.join(candidate)
        }
    };
    fs::read_to_string(path).ok()
}

fn render_origin_trace_html(model: &impl Serialize, live_reload: bool) -> Result<String, String> {
    let data = serde_json::to_string(model)
        .map_err(|err| format!("failed to serialize web demo model: {err}"))?;
    let html = fe_web::assets::origin_trace_html_shell("Fe Origin Trace Demo", &data);
    if live_reload {
        Ok(inject_live_reload_script(&html))
    } else {
        Ok(html)
    }
}

fn inject_live_reload_script(html: &str) -> String {
    let script = r#"<script>
(function () {
  if (!("EventSource" in window)) return;
  var events = new EventSource("/events");
  events.addEventListener("reload", function () { window.location.reload(); });
  events.addEventListener("trace-error", function (event) {
    console.warn("[fe trace web-demo]", event.data);
  });
})();
</script>"#;
    if html.contains("</body>") {
        html.replacen("</body>", &format!("{script}\n</body>"), 1)
    } else {
        format!("{html}\n{script}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use trace_query::origin_closure::{
        OriginClosureCounts as DemoClosureCounts, OriginClosureSourceSpan as DemoSourceSpan,
    };

    #[test]
    fn render_origin_trace_html_embeds_json_safely() {
        let model = WebDemoModel {
            metadata: DemoMetadata {
                input_path: "demo.fe".to_string(),
                target: "evm/sonatina".to_string(),
                data_source: "compiler_emitted".to_string(),
                compiler_commit: "abc123".to_string(),
                flags: vec![],
            },
            provenance: DemoProvenanceStatus {
                source_to_optimized: "missing".to_string(),
                optimized_to_prepared: "missing".to_string(),
                prepared_to_bytecode: "missing".to_string(),
                summary: "provenance is partial".to_string(),
            },
            counts: DemoCounts {
                facts: 1,
                origin_edges: 0,
                instructions: 0,
                source_spans: 0,
            },
            salsa: None,
            audit: None,
            static_analysis: None,
            attribution_audit: None,
            source: DemoSource {
                display_name: "demo.fe".to_string(),
                confidence: "coarse file-level fallback".to_string(),
                lines: vec![DemoSourceLine {
                    row_id: "source-main-line-1".to_string(),
                    number: 1,
                    text: "</script>".to_string(),
                    classes: Vec::new(),
                    display_status: None,
                }],
                related_sources: Vec::new(),
            },
            panels: Vec::new(),
            closures: Vec::new(),
            bytecode_count: 0,
            notes: Vec::new(),
        };

        let html = render_origin_trace_html(&model, false).unwrap();

        assert!(html.contains(r"<\/script>"));
        assert!(html.contains("fe-origin-trace"));
        assert!(html.contains("Fe Origin Trace Demo"));
    }

    #[test]
    fn render_snapshot_uses_shared_trace_workbench_projection() {
        let metadata = TraceMetadata::compiler_emitted(
            "test",
            "evm/sonatina",
            vec!["fe".to_string(), "dev".to_string(), "trace".to_string()],
            "demo.fe",
            vec!["optimize=O2".to_string()],
        );
        let snapshot = TraceSnapshot::new(trace_facts::TraceBundle::new(metadata, Vec::new()))
            .expect("empty trace snapshot should validate");

        let rendered = render_snapshot(&snapshot, None, false).unwrap();

        assert!(rendered.model.get("parity_summary").is_some());
        assert!(rendered.model.get("indexes").is_some());
        assert!(
            rendered
                .model
                .get("notes")
                .and_then(serde_json::Value::as_array)
                .into_iter()
                .flatten()
                .any(|note| note
                    == "Static and live workbench entry points use the shared trace-query projection path.")
        );
        assert_eq!(rendered.model["parity_summary"]["opt_level"], "O2");
    }

    #[test]
    fn rendered_web_demo_path_cannot_use_legacy_demo_model() {
        let source = fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/src/trace/trace_web_demo.rs"
        ))
        .expect("read trace web demo source");
        let render_snapshot_body = source
            .split("fn render_snapshot(")
            .nth(1)
            .and_then(|rest| rest.split("fn build_trace_workbench_model(").next())
            .expect("render_snapshot body should precede build_trace_workbench_model");

        assert!(
            render_snapshot_body.contains("build_trace_workbench_model(snapshot, salsa.as_ref())"),
            "web demo rendering must use the shared trace-query projection entry point"
        );
        assert!(
            !render_snapshot_body.contains("build_demo_model("),
            "legacy closure-audit model must not drive the rendered workbench path"
        );
        assert!(
            source.contains("trace_workbench_report_projection("),
            "shared trace-query workbench projection must remain the web demo source of truth"
        );
    }

    #[test]
    fn live_reload_script_connects_to_event_stream() {
        let html = inject_live_reload_script("<html><body>x</body></html>");

        assert!(html.contains("new EventSource(\"/events\")"));
        assert!(html.contains("window.location.reload()"));
    }

    #[test]
    fn compact_mir_text_removes_rust_debug_noise() {
        let text = "RLocalId(7) = call RuntimeInstance(Id(1fc03), PhantomData<&salsa::tracked_struct::Value<fe_mir::instance::runtime::RuntimeInstance<'_>>>)([RLocalId(6)])";

        assert_eq!(compact_mir_text(text), "%7 = call runtime#1fc03([%6])");
    }

    #[test]
    fn compact_mir_text_simplifies_layouts_and_ints() {
        let text = "RLocalId(28) = aggregate_make LayoutId(Id(1f40d), PhantomData<&salsa::interned::Value<fe_mir::runtime::ir::LayoutId<'_>>>), [RLocalId(26), RLocalId(27)]";
        let int_text = "RLocalId(22) = const Int { bits: 256, signed: false, words: [32] }";
        let small_int_text = "RLocalId(17) = const Int { bits: 32, signed: false, words: [0] }";

        assert_eq!(
            compact_mir_text(text),
            "%28 = aggregate_make layout#1f40d, [%26, %27]"
        );
        assert_eq!(compact_mir_text(int_text), "%22 = const u256 32");
        assert_eq!(compact_mir_text(small_int_text), "%17 = const u32 0");
    }

    #[test]
    fn compact_instruction_mnemonic_hides_ir_debug_prefix() {
        assert_eq!(compact_instruction_mnemonic("ir[71] ADD"), "ADD");
        assert_eq!(compact_instruction_mnemonic("PUSH1 0x20"), "PUSH1 0x20");
    }

    #[test]
    fn panel_rows_emit_typed_compact_listing_data() {
        let key = OriginExportKey::try_from_raw_parts(
            "runtime.stmt",
            "runtime-instance:semantic:entry",
            "block:2:stmt:7",
        )
        .unwrap();
        let function = OriginExportKey::try_from_raw_parts(
            "runtime.function",
            "runtime-instance:semantic:entry",
            "function:0",
        )
        .unwrap();
        let instruction = InstructionFact::new(
            key.clone(),
            function,
            7,
            "RLocalId(3) = const Int { bits: 32, signed: false, words: [1] }",
        );
        let mut instructions = BTreeMap::new();
        instructions.insert(key.clone(), &instruction);
        let storage_key = key.canonical_storage_key();
        let index = DemoIndex {
            origin_nodes: BTreeMap::new(),
            instructions,
            source_files: BTreeMap::new(),
            source_spans: BTreeMap::new(),
            source_snippets: BTreeMap::new(),
            edges_by_from: BTreeMap::new(),
            edges_by_to: BTreeMap::new(),
        };
        let mut classes_by_key = BTreeMap::new();
        classes_by_key.insert(
            storage_key.clone(),
            vec!["trace-c-1".to_string(), "exact-c-abc".to_string()],
        );

        let row = origin_panel_row(
            &key,
            &index,
            &classes_by_key,
            &BTreeMap::new(),
            &BTreeMap::new(),
        );

        assert_eq!(row.kind, DemoPaneRowKind::Statement);
        assert_eq!(row.row_id, demo_origin_row_id(&storage_key));
        assert_eq!(row.indent, 2);
        assert_eq!(row.label, "bb2.s7");
        assert_eq!(row.compact_text, "%3 = const u32 1");
        assert!(row.display_status.is_none());
        assert_eq!(row.rail_classes.exact, vec!["exact-c-abc"]);
        assert_eq!(row.debug.origin_key.as_deref(), Some(storage_key.as_str()));
        assert!(row.debug.raw_text.contains("RLocalId(3)"));
    }

    #[test]
    fn source_generated_status_is_downstream_not_compiler_generated() {
        let key = OriginExportKey::try_from_raw_parts(
            "source.span",
            "package:demo.fe:__synthetic",
            "span:0",
        )
        .unwrap();

        assert!(matches!(
            display_status_for_row(&key, DemoPaneRowKind::Statement, &[]),
            Some(DemoDisplayStatus::Generated)
        ));
    }

    #[test]
    fn source_line_generated_component_does_not_badge_authored_source() {
        assert!(display_status_for_source_line(&["generated-c-a".to_string()]).is_none());
        assert!(display_status_for_source_line(&["context-c-a".to_string()]).is_none());
    }

    #[test]
    fn rail_membership_does_not_create_generic_row_badges() {
        let key = OriginExportKey::try_from_raw_parts(
            "runtime.stmt",
            "runtime-instance:demo",
            "block:0:stmt:0",
        )
        .unwrap();
        let classes = [
            "generated-c-helper".to_string(),
            "context-c-neighborhood".to_string(),
            "structural-c-boundary".to_string(),
        ];

        assert!(display_status_for_row(&key, DemoPaneRowKind::Statement, &classes).is_none());

        let synthetic = OriginExportKey::try_from_raw_parts(
            "runtime.stmt",
            "runtime-instance:demo",
            "block:0:stmt:0:__synthetic",
        )
        .unwrap();
        assert!(matches!(
            display_status_for_row(&synthetic, DemoPaneRowKind::Statement, &classes),
            Some(DemoDisplayStatus::Generated)
        ));
    }

    #[test]
    fn demo_provenance_uses_attribution_audit_when_closure_islands_are_disjoint() {
        let status = demo_provenance_status_from_signals(false, false, false, 7, 11, 11, 0);

        assert_eq!(status.source_to_optimized, "available");
        assert_eq!(status.optimized_to_prepared, "missing");
        assert_eq!(status.prepared_to_bytecode, "available");
        assert_eq!(
            status.summary,
            "bytecode is prepared-linked; exact source attribution needs optimized to prepared lineage"
        );
    }

    #[test]
    fn demo_provenance_reports_non_exact_optimized_prepared_evidence() {
        let status = demo_provenance_status_from_signals(false, false, false, 7, 11, 0, 3);

        assert_eq!(status.source_to_optimized, "available");
        assert_eq!(status.optimized_to_prepared, "non-exact");
        assert_eq!(status.prepared_to_bytecode, "available");
        assert_eq!(
            status.summary,
            "bytecode has generated/context optimized to prepared explanation, but not exact source attribution"
        );
    }

    #[test]
    fn source_lines_ignore_foreign_source_spans() {
        let closure = DemoClosure {
            class_name: "trace-c-0".to_string(),
            label: "demo".to_string(),
            root_key: "root".to_string(),
            keys: Vec::new(),
            generated_keys: Vec::new(),
            contextual_keys: Vec::new(),
            structural_keys: Vec::new(),
            counts: DemoClosureCounts {
                hir: 1,
                mir: 0,
                sonatina_pre: 0,
                sonatina_post: 0,
                sonatina_prepared: 0,
                bytecode: 0,
            },
            traversal: test_traversal(),
            gap: None,
            edges: Vec::new(),
            source_spans: vec![DemoSourceSpan {
                origin_key: "hir.expr:body:1".to_string(),
                origin: "hir.expr:body:1".to_string(),
                file: "std/foo.fe".to_string(),
                file_owner: "std/foo.fe".to_string(),
                lines: "1:1".to_string(),
                start_byte: 0,
                end_byte: 1,
                start_line: 1,
                end_line: 1,
                confidence: "direct".to_string(),
            }],
        };

        let lines = source_lines("fib_demo.fe", "x", &[closure], &BTreeMap::new());

        assert!(lines[0].classes.is_empty());
        assert!(lines[0].display_status.is_none());

        let closure = DemoClosure {
            class_name: "trace-c-0".to_string(),
            label: "demo".to_string(),
            root_key: "root".to_string(),
            keys: Vec::new(),
            generated_keys: Vec::new(),
            contextual_keys: Vec::new(),
            structural_keys: Vec::new(),
            counts: DemoClosureCounts {
                hir: 1,
                mir: 0,
                sonatina_pre: 0,
                sonatina_post: 0,
                sonatina_prepared: 0,
                bytecode: 0,
            },
            traversal: test_traversal(),
            gap: None,
            edges: Vec::new(),
            source_spans: vec![DemoSourceSpan {
                origin_key: "hir.expr:body:1".to_string(),
                origin: "hir.expr:body:1".to_string(),
                file: "fib_demo.fe".to_string(),
                file_owner: "fib_demo.fe".to_string(),
                lines: "1:1".to_string(),
                start_byte: 0,
                end_byte: 1,
                start_line: 1,
                end_line: 1,
                confidence: "direct".to_string(),
            }],
        };

        let mut classes_by_key = BTreeMap::new();
        classes_by_key.insert(
            "hir.expr:body:1".to_string(),
            vec!["trace-c-0".to_string(), "exact-c-0".to_string()],
        );
        let lines = source_lines("fib_demo.fe", "x", &[closure], &classes_by_key);

        assert_eq!(
            lines[0].classes,
            vec!["exact-c-0".to_string(), "trace-c-0".to_string()]
        );
        assert!(lines[0].display_status.is_none());
    }

    #[test]
    fn related_source_sections_include_only_used_foreign_lines() {
        let file_key = OriginExportKey::try_from_raw_parts(
            "source.file",
            "builtin-std:/src/evm/calldata.fe",
            "file:0",
        )
        .unwrap();
        let source_file = SourceFileFact::new(
            file_key.clone(),
            "builtin-std:/src/evm/calldata.fe",
            "calldata.fe",
            "blake3:0000000000000000000000000000000000000000000000000000000000000001",
            None,
        );
        let mut source_files = BTreeMap::new();
        source_files.insert(file_key.clone(), &source_file);
        let index = DemoIndex {
            origin_nodes: BTreeMap::new(),
            instructions: BTreeMap::new(),
            source_files,
            source_spans: BTreeMap::new(),
            source_snippets: BTreeMap::new(),
            edges_by_from: BTreeMap::new(),
            edges_by_to: BTreeMap::new(),
        };
        let mut source_texts = BTreeMap::new();
        source_texts.insert(file_key, "unused\npulled in\nunused too\n".to_string());
        let closure = DemoClosure {
            class_name: "trace-c-7".to_string(),
            label: "foreign".to_string(),
            root_key: "root".to_string(),
            keys: Vec::new(),
            generated_keys: Vec::new(),
            contextual_keys: Vec::new(),
            structural_keys: Vec::new(),
            counts: DemoClosureCounts {
                hir: 1,
                mir: 0,
                sonatina_pre: 0,
                sonatina_post: 0,
                sonatina_prepared: 0,
                bytecode: 0,
            },
            traversal: test_traversal(),
            gap: None,
            edges: Vec::new(),
            source_spans: vec![DemoSourceSpan {
                origin_key: "hir.expr:foreign:1".to_string(),
                origin: "hir.expr:foreign:1".to_string(),
                file: "calldata.fe".to_string(),
                file_owner: "builtin-std:/src/evm/calldata.fe".to_string(),
                lines: "2:2".to_string(),
                start_byte: 7,
                end_byte: 16,
                start_line: 2,
                end_line: 2,
                confidence: "direct".to_string(),
            }],
        };

        let mut classes_by_key = BTreeMap::new();
        classes_by_key.insert(
            "hir.expr:foreign:1".to_string(),
            vec!["trace-c-7".to_string(), "exact-c-4".to_string()],
        );
        let sections = related_source_sections(
            "entry.fe",
            &source_texts,
            &index,
            &[closure],
            &classes_by_key,
        );

        assert_eq!(sections.len(), 1);
        assert_eq!(sections[0].display_name, "calldata.fe");
        assert_eq!(sections[0].lines.len(), 1);
        assert!(sections[0].lines[0].row_id.starts_with("source-related-"));
        assert_eq!(sections[0].lines[0].number, 2);
        assert_eq!(sections[0].lines[0].text, "pulled in");
        assert_eq!(sections[0].lines[0].classes, vec!["exact-c-4", "trace-c-7"]);
    }

    fn test_traversal() -> trace_query::origin_closure::OriginClosureTraversalSummary {
        trace_query::origin_closure::OriginClosureTraversalSummary {
            mode: "undirected_connected_trace_region".to_string(),
            max_depth: 16,
            max_nodes: 512,
            truncated: false,
            truncation_reason: None,
            skipped_hubs: Vec::new(),
        }
    }
}
