use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
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
    InstructionFact, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, SourceSpanFact, TraceFact,
    TraceSnapshot,
};
use trace_query::{IntrospectionService, LoopContentsRequest, TraceIntrospectionService};

use crate::{DevTraceAuditClosuresArgs, DevTraceWebDemoArgs, TraceReportFormat};

const CLOSURE_MAX_DEPTH: usize = 16;
const CLOSURE_MAX_NODES: usize = 512;
const CLOSURE_DEGREE_HUB_THRESHOLD: usize = 128;

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
        rendered.model.metadata.data_source,
        rendered.model.bytecode_count,
        rendered.model.source.confidence,
        rendered.salsa_summary(),
    ))
}

pub(super) fn run_trace_audit_closures(args: &DevTraceAuditClosuresArgs) -> Result<String, String> {
    let model = render_trace_audit_model_once(args)?;
    let report = audit_closures(&model);
    match args.format {
        TraceReportFormat::Text => Ok(render_closure_audit_report(&report)),
        TraceReportFormat::Json => serde_json::to_string_pretty(&report)
            .map(|mut json| {
                json.push('\n');
                json
            })
            .map_err(|err| format!("failed to render closure audit JSON: {err}")),
    }
}

struct RenderedWebDemo {
    html: String,
    model: WebDemoModel,
}

impl RenderedWebDemo {
    fn salsa_summary(&self) -> String {
        self.model.salsa.as_ref().map_or_else(
            || "Salsa: offline JSONL input".to_string(),
            |salsa| {
                format!(
                    "Salsa: {} in {}ms, will_execute={}, memo_reuse={}",
                    salsa.mode, salsa.elapsed_ms, salsa.will_execute, salsa.memo_reuse
                )
            },
        )
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
            let rendered = render_incremental_trace(&mut emitter, 1, false)?;
            Ok(rendered.model)
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
    let model = build_demo_model(snapshot, salsa);
    let html = render_origin_trace_html(&model, live_reload)?;
    Ok(RenderedWebDemo { html, model })
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
            rendered.model.source.confidence,
            rendered.model.bytecode_count,
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
    counts: DemoCounts,
    salsa: Option<DemoSalsaStats>,
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
}

#[derive(Debug, Serialize)]
struct DemoSourceLine {
    number: u32,
    text: String,
    classes: Vec<String>,
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
    key: Option<String>,
    label: String,
    meta: String,
    text: String,
    classes: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
struct DemoClosure {
    class_name: String,
    label: String,
    root_key: String,
    #[serde(skip_serializing)]
    keys: Vec<String>,
    counts: DemoClosureCounts,
    traversal: DemoClosureTraversalSummary,
    gap: Option<String>,
    edges: Vec<DemoClosureEdge>,
    source_spans: Vec<DemoSourceSpan>,
}

#[derive(Clone, Debug, Serialize)]
struct DemoClosureCounts {
    hir: usize,
    mir: usize,
    sonatina_pre: usize,
    sonatina_post: usize,
    bytecode: usize,
}

#[derive(Clone, Debug, Serialize)]
struct DemoClosureTraversalSummary {
    mode: String,
    max_depth: usize,
    max_nodes: usize,
    truncated: bool,
    truncation_reason: Option<String>,
    skipped_hubs: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
struct DemoClosureEdge {
    label: String,
    from: String,
    to: String,
}

struct OriginClosure {
    keys: BTreeSet<OriginExportKey>,
    edges: Vec<DemoClosureEdge>,
    traversal: DemoClosureTraversalSummary,
}

#[derive(Debug, Serialize)]
struct DemoKey {
    short: String,
    full: String,
}

#[derive(Clone, Debug, Serialize)]
struct DemoSourceSpan {
    origin: String,
    file: String,
    #[serde(skip_serializing)]
    file_owner: String,
    lines: String,
    start_byte: u32,
    end_byte: u32,
    start_line: u32,
    end_line: u32,
    confidence: String,
}

fn build_demo_model(snapshot: &TraceSnapshot, salsa: Option<DemoSalsaStats>) -> WebDemoModel {
    let service = TraceIntrospectionService::new(snapshot.clone());
    let loop_report = service
        .loop_contents(LoopContentsRequest::default())
        .expect("loop-contents query over validated snapshot should not fail");
    let index = DemoIndex::new(snapshot);
    let source_text = read_source_text(&snapshot.metadata().input_path).unwrap_or_default();
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
    let closure_roots = closure_roots(
        snapshot.metadata().input_path.as_str(),
        &index,
        &loop_report,
    );
    let closures = build_closures(closure_roots, &index);
    let classes_by_key = classes_by_key(&closures);
    let source_lines = source_lines(
        snapshot.metadata().input_path.as_str(),
        &source_text,
        &closures,
    );
    let mut panels = build_origin_panels(&index, &classes_by_key);
    panels.insert(1, loop_panel(&loop_report, &classes_by_key));

    WebDemoModel {
        metadata: DemoMetadata {
            input_path: snapshot.metadata().input_path.clone(),
            target: snapshot.metadata().target.clone(),
            data_source: super::format_data_source(snapshot.metadata()),
            compiler_commit: snapshot.metadata().compiler_commit.clone(),
            flags: snapshot.metadata().flags.clone(),
        },
        counts: DemoCounts {
            facts: snapshot.facts().len(),
            origin_edges: index.edge_count,
            instructions: index.instruction_count,
            source_spans: index.source_spans.len(),
        },
        salsa,
        source: DemoSource {
            display_name: snapshot.metadata().input_path.clone(),
            confidence: source_confidence,
            lines: source_lines,
        },
        panels,
        closures,
        bytecode_count,
        notes: demo_notes(bytecode_count, exact_source_spans),
    }
}

struct DemoIndex<'a> {
    origin_nodes: BTreeMap<OriginExportKey, &'a OriginNodeFact>,
    edges_by_from: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
    edges_by_to: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
    instructions: BTreeMap<OriginExportKey, &'a InstructionFact>,
    source_spans: BTreeMap<OriginExportKey, &'a SourceSpanFact>,
    edge_count: usize,
    instruction_count: usize,
}

impl<'a> DemoIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut origin_nodes = BTreeMap::new();
        let mut edges_by_from = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut edges_by_to = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut instructions = BTreeMap::new();
        let mut source_spans = BTreeMap::new();
        let mut edge_count = 0;
        let mut instruction_count = 0;

        for fact in snapshot.facts() {
            match fact {
                TraceFact::OriginNode(node) => {
                    origin_nodes.insert(node.key.clone(), node);
                }
                TraceFact::OriginEdge(edge) => {
                    edge_count += 1;
                    edges_by_from
                        .entry(edge.from.clone())
                        .or_default()
                        .push(edge);
                    edges_by_to.entry(edge.to.clone()).or_default().push(edge);
                }
                TraceFact::Instruction(instruction) => {
                    instruction_count += 1;
                    instructions.insert(instruction.instruction.clone(), instruction);
                }
                TraceFact::SourceSpan(span) => {
                    source_spans.insert(span.origin.clone(), span);
                }
                _ => {}
            }
        }

        Self {
            origin_nodes,
            edges_by_from,
            edges_by_to,
            instructions,
            source_spans,
            edge_count,
            instruction_count,
        }
    }
}

fn closure_roots(
    input_path: &str,
    index: &DemoIndex<'_>,
    loop_report: &trace_query::LoopContentsReport,
) -> Vec<OriginExportKey> {
    let mut roots = BTreeMap::<String, OriginExportKey>::new();
    for span in index.source_spans.values() {
        if matches!(span.origin.kind(), "hir.expr" | "hir.stmt")
            && source_span_matches_input(span, input_path)
        {
            roots.insert(span.origin.canonical_storage_key(), span.origin.clone());
        }
    }
    for instruction in &loop_report.target_instructions {
        roots.insert(
            instruction.key.canonical_storage_key(),
            instruction.key.clone(),
        );
    }
    for block in &loop_report.blocks {
        roots.insert(block.block.canonical_storage_key(), block.block.clone());
        for instruction in &block.instructions {
            roots.insert(
                instruction.key.canonical_storage_key(),
                instruction.key.clone(),
            );
        }
    }
    roots.into_values().collect()
}

fn source_span_matches_input(span: &SourceSpanFact, input_path: &str) -> bool {
    source_owner_matches_input(span.file.owner_key(), input_path)
}

fn source_owner_matches_input(owner: &str, input_path: &str) -> bool {
    owner == input_path || owner.ends_with(input_path)
}

fn build_closures(roots: Vec<OriginExportKey>, index: &DemoIndex<'_>) -> Vec<DemoClosure> {
    roots
        .into_iter()
        .enumerate()
        .map(|(ordinal, root)| {
            let closure = origin_closure(&root, index);
            let source_spans = source_spans_for_keys(&closure.keys, index);
            let counts = closure_counts(&closure.keys);
            let gap = closure_gap_note(&counts);
            DemoClosure {
                class_name: format!("trace-c-{ordinal}"),
                label: closure_label(&root, index),
                root_key: root.canonical_storage_key(),
                keys: closure
                    .keys
                    .iter()
                    .map(OriginExportKey::canonical_storage_key)
                    .collect(),
                counts,
                traversal: closure.traversal,
                gap,
                edges: closure.edges,
                source_spans,
            }
        })
        .collect()
}

fn classes_by_key(closures: &[DemoClosure]) -> BTreeMap<String, Vec<String>> {
    let mut classes = BTreeMap::<String, BTreeSet<String>>::new();
    for closure in closures {
        for key in &closure.keys {
            classes
                .entry(key.clone())
                .or_default()
                .insert(closure.class_name.clone());
        }
    }
    classes
        .into_iter()
        .map(|(key, value)| (key, value.into_iter().collect()))
        .collect()
}

fn source_lines(
    input_path: &str,
    source_text: &str,
    closures: &[DemoClosure],
) -> Vec<DemoSourceLine> {
    let mut classes_by_line = BTreeMap::<u32, BTreeSet<String>>::new();
    for closure in closures {
        for span in &closure.source_spans {
            if span.confidence != "direct"
                || !source_owner_matches_input(&span.file_owner, input_path)
            {
                continue;
            }
            for line in span.start_line..=span.end_line {
                classes_by_line
                    .entry(line)
                    .or_default()
                    .insert(closure.class_name.clone());
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
            DemoSourceLine {
                number,
                text: text.to_string(),
                classes: classes_by_line
                    .get(&number)
                    .map(|classes| classes.iter().cloned().collect())
                    .unwrap_or_default(),
            }
        })
        .collect()
}

fn closure_counts(keys: &BTreeSet<OriginExportKey>) -> DemoClosureCounts {
    let mut counts = DemoClosureCounts {
        hir: 0,
        mir: 0,
        sonatina_pre: 0,
        sonatina_post: 0,
        bytecode: 0,
    };
    for key in keys {
        match key.kind() {
            kind if kind.starts_with("hir.") => counts.hir += 1,
            kind if kind.starts_with("runtime.") => counts.mir += 1,
            kind if kind.starts_with("sonatina.preopt.") => counts.sonatina_pre += 1,
            kind if kind.starts_with("sonatina.postopt.") => counts.sonatina_post += 1,
            "bytecode.pc" => counts.bytecode += 1,
            _ => {}
        }
    }
    counts
}

fn closure_gap_note(counts: &DemoClosureCounts) -> Option<String> {
    if counts.sonatina_post > 0 && counts.bytecode == 0 {
        Some(
            "Reached Sonatina post-opt but no bytecode PC edge was recorded. This is an optimized-code attribution gap through backend preparation or value movement, not evidence that the source is dead.".to_string(),
        )
    } else {
        None
    }
}

fn highest_phase_reached(counts: &DemoClosureCounts) -> ClosureAuditPhase {
    if counts.bytecode > 0 {
        ClosureAuditPhase::Bytecode
    } else if counts.sonatina_post > 0 {
        ClosureAuditPhase::SonatinaPost
    } else if counts.sonatina_pre > 0 {
        ClosureAuditPhase::SonatinaPre
    } else if counts.mir > 0 {
        ClosureAuditPhase::Mir
    } else if counts.hir > 0 {
        ClosureAuditPhase::Hir
    } else {
        ClosureAuditPhase::Source
    }
}

#[derive(Debug, Serialize)]
struct ClosureAuditReport {
    input_path: String,
    target: String,
    data_source: String,
    total_closures: usize,
    span_groups: ClosureAuditSpanGroupSummary,
    span_group_details: Vec<ClosureAuditSpanGroupDetail>,
    primary_counts: BTreeMap<ClosureAuditPrimary, usize>,
    symptom_counts: BTreeMap<ClosureAuditSymptom, usize>,
    suspicious_closures: usize,
    closures: Vec<ClosureAuditEntry>,
}

#[derive(Debug, Serialize)]
struct ClosureAuditSpanGroupSummary {
    total_groups: usize,
    mixed_connectivity_groups: usize,
}

#[derive(Debug, Serialize)]
struct ClosureAuditSpanGroupDetail {
    file_owner: String,
    start_byte: u32,
    end_byte: u32,
    source_text: Option<String>,
    closures: usize,
    closures_with_targets: usize,
    source_only_closures: usize,
    target_connected_members: Vec<ClosureAuditSpanGroupMember>,
    source_only_members: Vec<ClosureAuditSpanGroupMember>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
struct ClosureAuditSpanGroupMember {
    class_name: String,
    label: String,
    root_key: String,
    highest_phase_reached: ClosureAuditPhase,
}

#[derive(Debug, Serialize)]
struct ClosureAuditEntry {
    class_name: String,
    label: String,
    root_key: String,
    primary: ClosureAuditPrimary,
    symptoms: Vec<ClosureAuditSymptom>,
    suspicious: bool,
    highest_phase_reached: ClosureAuditPhase,
    counts: DemoClosureCounts,
    traversal: DemoClosureTraversalSummary,
    key_count: usize,
    edge_count: usize,
    source_spans: Vec<String>,
    notes: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
enum ClosureAuditPhase {
    Source,
    Hir,
    Mir,
    SonatinaPre,
    SonatinaPost,
    Bytecode,
}

impl ClosureAuditPhase {
    fn as_str(self) -> &'static str {
        match self {
            Self::Source => "source",
            Self::Hir => "hir",
            Self::Mir => "mir",
            Self::SonatinaPre => "sonatina_pre",
            Self::SonatinaPost => "sonatina_post",
            Self::Bytecode => "bytecode",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
enum ClosureAuditPrimary {
    GoodExact,
    GoodManyToMany,
    SourceOnlyExpected,
    SourceSpanSiblingUnlowered,
    ExpectedSynthetic,
    LoweredNoTargetUnexplained,
    PreoptElisionGap,
    OptimizedAttributionGap,
    MissingSourceUnexplained,
    TruncatedUnknown,
    Unclassified,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
enum ClosureAuditSymptom {
    TooBroad,
    MissingSourceUnexplained,
    SourceUnavailableSynthetic,
    MissingBytecode,
    ForeignSource,
    Truncated,
}

impl ClosureAuditPrimary {
    fn is_suspicious(self) -> bool {
        matches!(
            self,
            Self::OptimizedAttributionGap
                | Self::PreoptElisionGap
                | Self::LoweredNoTargetUnexplained
                | Self::MissingSourceUnexplained
                | Self::TruncatedUnknown
                | Self::Unclassified
        )
    }

    fn as_str(self) -> &'static str {
        match self {
            Self::GoodExact => "good_exact",
            Self::GoodManyToMany => "good_many_to_many",
            Self::SourceOnlyExpected => "source_only_expected",
            Self::SourceSpanSiblingUnlowered => "source_span_sibling_unlowered",
            Self::ExpectedSynthetic => "expected_synthetic",
            Self::LoweredNoTargetUnexplained => "lowered_no_target_unexplained",
            Self::PreoptElisionGap => "preopt_elision_gap",
            Self::OptimizedAttributionGap => "optimized_attribution_gap",
            Self::MissingSourceUnexplained => "missing_source_unexplained",
            Self::TruncatedUnknown => "truncated_unknown",
            Self::Unclassified => "unclassified",
        }
    }
}

impl ClosureAuditSymptom {
    fn is_suspicious(self) -> bool {
        matches!(
            self,
            Self::TooBroad
                | Self::MissingSourceUnexplained
                | Self::MissingBytecode
                | Self::ForeignSource
                | Self::Truncated
        )
    }

    fn as_str(self) -> &'static str {
        match self {
            Self::TooBroad => "too_broad",
            Self::MissingSourceUnexplained => "missing_source_unexplained",
            Self::SourceUnavailableSynthetic => "source_unavailable_synthetic",
            Self::MissingBytecode => "missing_bytecode",
            Self::ForeignSource => "foreign_source",
            Self::Truncated => "truncated",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct SourceSpanSignature {
    file_owner: String,
    start_byte: u32,
    end_byte: u32,
}

#[derive(Default)]
struct SourceSpanGroup {
    closures: usize,
    closures_with_targets: usize,
    source_only_closures: usize,
    target_connected_members: BTreeSet<ClosureAuditSpanGroupMember>,
    source_only_members: BTreeSet<ClosureAuditSpanGroupMember>,
    source_text: Option<String>,
}

struct SourceSpanGroupIndex {
    groups: BTreeMap<SourceSpanSignature, SourceSpanGroup>,
}

impl SourceSpanGroupIndex {
    fn new(input_path: &str, closures: &[DemoClosure], source_lines: &[DemoSourceLine]) -> Self {
        let mut groups = BTreeMap::<SourceSpanSignature, SourceSpanGroup>::new();
        for closure in closures {
            let has_lowering_target = closure.counts.mir
                + closure.counts.sonatina_pre
                + closure.counts.sonatina_post
                + closure.counts.bytecode
                > 0;
            for span in &closure.source_spans {
                let Some(signature) = source_span_signature(span, input_path) else {
                    continue;
                };
                let group = groups.entry(signature).or_default();
                group.closures += 1;
                let member = ClosureAuditSpanGroupMember {
                    class_name: closure.class_name.clone(),
                    label: closure.label.clone(),
                    root_key: closure.root_key.clone(),
                    highest_phase_reached: highest_phase_reached(&closure.counts),
                };
                if has_lowering_target {
                    group.closures_with_targets += 1;
                    group.target_connected_members.insert(member);
                } else {
                    group.source_only_closures += 1;
                    group.source_only_members.insert(member);
                }
                if group.source_text.is_none() {
                    group.source_text = source_text_for_span(span, source_lines);
                }
            }
        }
        Self { groups }
    }

    fn summary(&self) -> ClosureAuditSpanGroupSummary {
        ClosureAuditSpanGroupSummary {
            total_groups: self.groups.len(),
            mixed_connectivity_groups: self
                .groups
                .values()
                .filter(|group| group.closures_with_targets > 0 && group.source_only_closures > 0)
                .count(),
        }
    }

    fn mixed_details(&self) -> Vec<ClosureAuditSpanGroupDetail> {
        self.groups
            .iter()
            .filter(|(_, group)| group.closures_with_targets > 0 && group.source_only_closures > 0)
            .map(|(signature, group)| ClosureAuditSpanGroupDetail {
                file_owner: signature.file_owner.clone(),
                start_byte: signature.start_byte,
                end_byte: signature.end_byte,
                source_text: group.source_text.clone(),
                closures: group.closures,
                closures_with_targets: group.closures_with_targets,
                source_only_closures: group.source_only_closures,
                target_connected_members: group.target_connected_members.iter().cloned().collect(),
                source_only_members: group.source_only_members.iter().cloned().collect(),
            })
            .collect()
    }
}

fn source_span_signature(span: &DemoSourceSpan, input_path: &str) -> Option<SourceSpanSignature> {
    (span.confidence == "direct" && source_owner_matches_input(&span.file_owner, input_path)).then(
        || SourceSpanSignature {
            file_owner: span.file_owner.clone(),
            start_byte: span.start_byte,
            end_byte: span.end_byte,
        },
    )
}

fn source_text_for_span(span: &DemoSourceSpan, source_lines: &[DemoSourceLine]) -> Option<String> {
    if span.start_line != span.end_line {
        return Some(format!("{}:{}", span.start_line, span.end_line));
    }
    source_lines
        .iter()
        .find(|line| line.number == span.start_line)
        .map(|line| line.text.trim().to_string())
        .filter(|text| !text.is_empty())
}

fn audit_closures(model: &WebDemoModel) -> ClosureAuditReport {
    let span_groups = SourceSpanGroupIndex::new(
        &model.metadata.input_path,
        &model.closures,
        &model.source.lines,
    );
    let closures = model
        .closures
        .iter()
        .map(|closure| {
            audit_closure(
                &model.metadata.input_path,
                model.bytecode_count,
                closure,
                &span_groups,
            )
        })
        .collect::<Vec<_>>();
    let mut primary_counts = BTreeMap::<ClosureAuditPrimary, usize>::new();
    let mut symptom_counts = BTreeMap::<ClosureAuditSymptom, usize>::new();
    let mut suspicious = 0;
    for closure in &closures {
        if closure.suspicious {
            suspicious += 1;
        }
        *primary_counts.entry(closure.primary).or_default() += 1;
        for symptom in &closure.symptoms {
            *symptom_counts.entry(*symptom).or_default() += 1;
        }
    }
    ClosureAuditReport {
        input_path: model.metadata.input_path.clone(),
        target: model.metadata.target.clone(),
        data_source: model.metadata.data_source.clone(),
        total_closures: closures.len(),
        span_groups: span_groups.summary(),
        span_group_details: span_groups.mixed_details(),
        primary_counts,
        symptom_counts,
        suspicious_closures: suspicious,
        closures,
    }
}

fn audit_closure(
    input_path: &str,
    loop_bytecode_count: usize,
    closure: &DemoClosure,
    span_groups: &SourceSpanGroupIndex,
) -> ClosureAuditEntry {
    let mut symptoms = BTreeSet::new();
    let mut notes = Vec::new();
    let direct_input_spans = closure
        .source_spans
        .iter()
        .filter(|span| {
            span.confidence == "direct" && source_owner_matches_input(&span.file_owner, input_path)
        })
        .collect::<Vec<_>>();
    let foreign_spans = closure
        .source_spans
        .iter()
        .filter(|span| {
            span.confidence == "direct" && !source_owner_matches_input(&span.file_owner, input_path)
        })
        .collect::<Vec<_>>();

    if !foreign_spans.is_empty() {
        symptoms.insert(ClosureAuditSymptom::ForeignSource);
        notes.push(format!(
            "{} direct source span(s) point at a file other than {input_path}",
            foreign_spans.len()
        ));
    }

    let broad_bytecode_threshold = (loop_bytecode_count.saturating_mul(3) / 4).max(64);
    if closure.counts.bytecode > broad_bytecode_threshold || closure.keys.len() > 300 {
        symptoms.insert(ClosureAuditSymptom::TooBroad);
        notes.push(format!(
            "closure touches {} bytecode PC(s) and {} total key(s)",
            closure.counts.bytecode,
            closure.keys.len()
        ));
    }

    if closure.traversal.truncated {
        symptoms.insert(ClosureAuditSymptom::Truncated);
        notes.push(format!(
            "closure traversal truncated: {}",
            closure
                .traversal
                .truncation_reason
                .as_deref()
                .unwrap_or("unknown")
        ));
    }

    let has_ir = closure.counts.hir
        + closure.counts.mir
        + closure.counts.sonatina_pre
        + closure.counts.sonatina_post
        > 0;
    let has_expected_synthetic = closure.edges.iter().any(|edge| {
        edge.label.starts_with("SyntheticFor") || edge.label.starts_with("BackendPrepared")
    });
    let has_lowering_target = closure.counts.mir
        + closure.counts.sonatina_pre
        + closure.counts.sonatina_post
        + closure.counts.bytecode
        > 0;
    let same_span_sibling_reaches_target = !has_lowering_target
        && direct_input_spans.iter().any(|span| {
            source_span_signature(span, input_path)
                .and_then(|signature| span_groups.groups.get(&signature))
                .is_some_and(|group| group.closures_with_targets > 0)
        });
    if closure.counts.sonatina_post > 0 && closure.counts.bytecode == 0 {
        symptoms.insert(ClosureAuditSymptom::MissingBytecode);
        notes.push("post-opt closure reaches no final bytecode PC".to_string());
    }
    if has_ir && closure.counts.bytecode > 0 && direct_input_spans.is_empty() {
        if has_expected_synthetic {
            symptoms.insert(ClosureAuditSymptom::SourceUnavailableSynthetic);
            notes.push("bytecode-linked synthetic/backend closure has no direct source span; this is expected when the edge explains generated work".to_string());
        } else {
            symptoms.insert(ClosureAuditSymptom::MissingSourceUnexplained);
            notes.push(
                "bytecode-linked closure has no direct span in the audited input file".to_string(),
            );
        }
    }

    let primary = if closure.traversal.truncated {
        ClosureAuditPrimary::TruncatedUnknown
    } else if closure.counts.sonatina_post > 0 && closure.counts.bytecode == 0 {
        if let Some(gap) = &closure.gap {
            notes.push(gap.clone());
        }
        ClosureAuditPrimary::OptimizedAttributionGap
    } else if closure.counts.sonatina_pre > 0
        && closure.counts.sonatina_post == 0
        && closure.counts.bytecode == 0
    {
        notes.push("closure reaches Sonatina pre-opt but has no post-opt or bytecode target; no explicit elision/coalescing event explains it".to_string());
        ClosureAuditPrimary::PreoptElisionGap
    } else if same_span_sibling_reaches_target {
        notes.push("source-only HIR origin shares this exact input span with another HIR origin that reaches MIR/Sonatina/bytecode; this is a display/grouping sibling, not a duplicate identity".to_string());
        ClosureAuditPrimary::SourceSpanSiblingUnlowered
    } else if closure.counts.mir > 0
        && closure.counts.sonatina_pre == 0
        && closure.counts.sonatina_post == 0
        && closure.counts.bytecode == 0
    {
        notes.push("closure reaches MIR but has no Sonatina or bytecode target; no explicit lowering/elision event explains it".to_string());
        ClosureAuditPrimary::LoweredNoTargetUnexplained
    } else if has_expected_synthetic {
        ClosureAuditPrimary::ExpectedSynthetic
    } else if has_ir && closure.counts.bytecode > 0 && direct_input_spans.is_empty() {
        ClosureAuditPrimary::MissingSourceUnexplained
    } else if !direct_input_spans.is_empty() && closure.counts.bytecode > 0 {
        let exact = direct_input_spans.len() == 1
            && closure.counts.bytecode == 1
            && closure.counts.hir <= 1
            && closure.counts.mir <= 1
            && closure.counts.sonatina_pre <= 1
            && closure.counts.sonatina_post <= 1;
        if exact {
            ClosureAuditPrimary::GoodExact
        } else {
            ClosureAuditPrimary::GoodManyToMany
        }
    } else if !direct_input_spans.is_empty()
        && closure.counts.mir == 0
        && closure.counts.sonatina_pre == 0
        && closure.counts.sonatina_post == 0
        && closure.counts.bytecode == 0
    {
        ClosureAuditPrimary::SourceOnlyExpected
    } else {
        notes.push("closure did not match a known audit bucket".to_string());
        ClosureAuditPrimary::Unclassified
    };
    let symptoms = symptoms.into_iter().collect::<Vec<_>>();
    let suspicious =
        primary.is_suspicious() || symptoms.iter().any(|symptom| symptom.is_suspicious());

    ClosureAuditEntry {
        class_name: closure.class_name.clone(),
        label: closure.label.clone(),
        root_key: closure.root_key.clone(),
        primary,
        symptoms,
        suspicious,
        highest_phase_reached: highest_phase_reached(&closure.counts),
        counts: closure.counts.clone(),
        traversal: closure.traversal.clone(),
        key_count: closure.keys.len(),
        edge_count: closure.edges.len(),
        source_spans: direct_input_spans
            .into_iter()
            .map(|span| format!("{} {} {}", span.file, span.lines, span.origin))
            .collect(),
        notes,
    }
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
    out.push_str("Confidence: deterministic audit over derived closure graph; does not prove semantic completeness.\n");
    out.push_str(&format!("Input: {}\n", report.input_path));
    out.push_str(&format!("Target: {}\n", report.target));
    out.push_str(&format!("Data source: {}\n", report.data_source));
    out.push_str(&format!(
        "Closures: {} total, {} suspicious\n\n",
        report.total_closures, report.suspicious_closures
    ));
    out.push_str(&format!(
        "Source span groups: {} total, {} with mixed closure connectivity\n",
        report.span_groups.total_groups, report.span_groups.mixed_connectivity_groups
    ));
    if !report.span_group_details.is_empty() {
        out.push_str("Mixed source span groups:\n");
        for group in &report.span_group_details {
            out.push_str(&format!(
                "  {}:{}..{} closures={} target_connected={} source_only={} text={}\n",
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
    out.push_str("\nSuspicious closures:\n");
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
            "    highest_phase={} phases: HIR={} MIR={} pre={} post={} bytecode={} keys={} edges={}\n",
            closure.highest_phase_reached.as_str(),
            closure.counts.hir,
            closure.counts.mir,
            closure.counts.sonatina_pre,
            closure.counts.sonatina_post,
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
        ("bytecode", "Bytecode", "final runtime bytecode PCs"),
    ]
    .into_iter()
    .map(|(id, title, summary)| DemoPanel {
        id: id.to_string(),
        title: title.to_string(),
        summary: summary.to_string(),
        rows: panel_rows(id, index, classes_by_key),
    })
    .collect()
}

fn panel_rows(
    panel: &str,
    index: &DemoIndex<'_>,
    classes_by_key: &BTreeMap<String, Vec<String>>,
) -> Vec<DemoPanelRow> {
    index
        .origin_nodes
        .keys()
        .filter(|key| key_belongs_to_panel(key, panel))
        .filter(|key| classes_by_key.contains_key(&key.canonical_storage_key()))
        .map(|key| origin_panel_row(key, index, classes_by_key))
        .collect()
}

fn origin_panel_row(
    key: &OriginExportKey,
    index: &DemoIndex<'_>,
    classes_by_key: &BTreeMap<String, Vec<String>>,
) -> DemoPanelRow {
    let storage_key = key.canonical_storage_key();
    let instruction = index.instructions.get(key);
    DemoPanelRow {
        key: Some(storage_key.clone()),
        label: key.local_key().to_string(),
        meta: key.kind().to_string(),
        text: instruction
            .map(|instruction| format!("ir[{}] {}", instruction.index, instruction.mnemonic))
            .unwrap_or_else(|| key.owner_key().to_string()),
        classes: classes_by_key
            .get(&storage_key)
            .cloned()
            .unwrap_or_default(),
    }
}

fn key_belongs_to_panel(key: &OriginExportKey, panel: &str) -> bool {
    match panel {
        "hir" => key.kind().starts_with("hir."),
        "mir" => key.kind().starts_with("runtime."),
        "sonatina-pre" => key.kind().starts_with("sonatina.preopt."),
        "sonatina-post" => key.kind().starts_with("sonatina.postopt."),
        "bytecode" => key.kind() == "bytecode.pc",
        _ => false,
    }
}

fn loop_panel(
    loop_report: &trace_query::LoopContentsReport,
    classes_by_key: &BTreeMap<String, Vec<String>>,
) -> DemoPanel {
    let mut rows = Vec::new();
    let loop_label = loop_report
        .loop_label
        .clone()
        .unwrap_or_else(|| "selected loop".to_string());
    for block in &loop_report.blocks {
        let block_key = block.block.canonical_storage_key();
        rows.push(DemoPanelRow {
            key: Some(block_key.clone()),
            label: block.block.local_key().to_string(),
            meta: format!("loop {}", block.role),
            text: loop_label.clone(),
            classes: classes_by_key.get(&block_key).cloned().unwrap_or_default(),
        });
        for instruction in &block.instructions {
            let key = instruction.key.canonical_storage_key();
            rows.push(DemoPanelRow {
                key: Some(key.clone()),
                label: instruction.key.local_key().to_string(),
                meta: format!("ir[{}]", instruction.index),
                text: instruction.mnemonic.clone(),
                classes: classes_by_key.get(&key).cloned().unwrap_or_default(),
            });
        }
    }
    DemoPanel {
        id: "loop".to_string(),
        title: "Selected Loop".to_string(),
        summary: if loop_report.available {
            "compiler-derived loop membership".to_string()
        } else {
            "loop membership unavailable".to_string()
        },
        rows,
    }
}

fn origin_closure(start: &OriginExportKey, index: &DemoIndex<'_>) -> OriginClosure {
    let mut keys = BTreeSet::new();
    let mut edges_out = Vec::new();
    let mut seen_edges = BTreeSet::new();
    let mut skipped_hubs = BTreeSet::new();
    let mut truncated = false;
    let mut truncation_reason = None;
    let mut queue = VecDeque::from([(start.clone(), 0_usize)]);
    while let Some((key, depth)) = queue.pop_front() {
        if depth >= CLOSURE_MAX_DEPTH {
            truncated = true;
            truncation_reason.get_or_insert_with(|| "max_depth".to_string());
            continue;
        }
        if keys.len() >= CLOSURE_MAX_NODES && !keys.contains(&key) {
            truncated = true;
            truncation_reason.get_or_insert_with(|| "max_nodes".to_string());
            continue;
        }
        if !keys.insert(key.clone()) {
            continue;
        }
        let outgoing = index.edges_by_from.get(&key).into_iter().flatten().copied();
        let incoming = index.edges_by_to.get(&key).into_iter().flatten().copied();
        for edge in outgoing.chain(incoming) {
            let from_hub = trace_hub_reason(&edge.from, index, start);
            let to_hub = trace_hub_reason(&edge.to, index, start);
            if let Some(reason) = from_hub {
                skipped_hubs.insert(format!("{} ({reason})", edge.from.display_label()));
            }
            if let Some(reason) = to_hub {
                skipped_hubs.insert(format!("{} ({reason})", edge.to.display_label()));
            }
            if from_hub.is_some() || to_hub.is_some() {
                continue;
            }
            if !matches!(
                edge.label,
                OriginEdgeLabel::LoweredFrom
                    | OriginEdgeLabel::EmittedFrom
                    | OriginEdgeLabel::SyntheticFor
                    | OriginEdgeLabel::BackendPrepared
            ) {
                continue;
            }
            let edge_key = (
                edge.from.canonical_storage_key(),
                edge.to.canonical_storage_key(),
                format!("{:?}", edge.label),
            );
            if seen_edges.insert(edge_key) {
                edges_out.push(DemoClosureEdge {
                    label: format!(
                        "{:?} / {}",
                        edge.label,
                        edge.introduced_by
                            .map(|phase| format!("{phase:?}"))
                            .unwrap_or_else(|| "unknown".to_string())
                    ),
                    from: demo_key(&edge.from).short,
                    to: demo_key(&edge.to).short,
                });
            }
            queue.push_back((edge.from.clone(), depth + 1));
            queue.push_back((edge.to.clone(), depth + 1));
        }
    }
    OriginClosure {
        keys,
        edges: edges_out,
        traversal: DemoClosureTraversalSummary {
            mode: "undirected_connected_origin_closure".to_string(),
            max_depth: CLOSURE_MAX_DEPTH,
            max_nodes: CLOSURE_MAX_NODES,
            truncated,
            truncation_reason,
            skipped_hubs: skipped_hubs.into_iter().collect(),
        },
    }
}

fn trace_hub_reason(
    key: &OriginExportKey,
    index: &DemoIndex<'_>,
    root: &OriginExportKey,
) -> Option<&'static str> {
    if key == root {
        return None;
    }
    if matches!(key.kind(), "code.object" | "source.file") {
        return Some("known_context_hub");
    }
    (origin_edge_degree(key, index) > CLOSURE_DEGREE_HUB_THRESHOLD)
        .then_some("high_degree_context_hub")
}

fn origin_edge_degree(key: &OriginExportKey, index: &DemoIndex<'_>) -> usize {
    index.edges_by_from.get(key).map_or(0, Vec::len)
        + index.edges_by_to.get(key).map_or(0, Vec::len)
}

fn source_spans_for_keys(
    keys: &BTreeSet<OriginExportKey>,
    index: &DemoIndex<'_>,
) -> Vec<DemoSourceSpan> {
    let storage_keys = keys
        .iter()
        .map(OriginExportKey::canonical_storage_key)
        .collect::<BTreeSet<_>>();
    index
        .source_spans
        .iter()
        .filter(|(origin, _)| storage_keys.contains(&origin.canonical_storage_key()))
        .filter(|(_, span)| !has_finer_span(span, &storage_keys, index))
        .map(|(origin, span)| DemoSourceSpan {
            origin: origin.display_label(),
            file: span.file.display_label(),
            file_owner: span.file.owner_key().to_string(),
            lines: format!("{}:{}", span.start_line, span.end_line),
            start_byte: span.start_byte,
            end_byte: span.end_byte,
            start_line: span.start_line,
            end_line: span.end_line,
            confidence: if origin.kind() == "code.object" {
                "coarse".to_string()
            } else {
                "direct".to_string()
            },
        })
        .collect()
}

fn closure_label(root: &OriginExportKey, index: &DemoIndex<'_>) -> String {
    index.instructions.get(root).map_or_else(
        || root.display_label(),
        |instruction| {
            format!(
                "{} ir[{}] {}",
                root.display_label(),
                instruction.index,
                instruction.mnemonic
            )
        },
    )
}

fn has_finer_span(span: &SourceSpanFact, keys: &BTreeSet<String>, index: &DemoIndex<'_>) -> bool {
    index
        .source_spans
        .iter()
        .filter(|(origin, _)| keys.contains(&origin.canonical_storage_key()))
        .any(|(_, other)| {
            other.file == span.file
                && (other.start_byte, other.end_byte) != (span.start_byte, span.end_byte)
                && other.start_byte >= span.start_byte
                && other.end_byte <= span.end_byte
                && (other.end_byte - other.start_byte) < (span.end_byte - span.start_byte)
        })
}

fn demo_key(key: &OriginExportKey) -> DemoKey {
    DemoKey {
        short: format!("{} {}", key.kind(), key.display_label()),
        full: key.canonical_storage_key(),
    }
}

fn demo_notes(bytecode_count: usize, exact_source_spans: bool) -> Vec<String> {
    let mut notes = vec![
        "This page is a derived view over validated trace JSONL; it does not define compiler identity.".to_string(),
        "Bytecode rows are selected from the loop-contents report, so they are tied to compiler-emitted Sonatina loop facts.".to_string(),
    ];
    if bytecode_count == 0 {
        notes.push(
            "No bytecode PCs joined to the selected loop; rebuild the trace after enabling Sonatina observability.".to_string(),
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

fn render_origin_trace_html(model: &WebDemoModel, live_reload: bool) -> Result<String, String> {
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
            counts: DemoCounts {
                facts: 1,
                origin_edges: 0,
                instructions: 0,
                source_spans: 0,
            },
            salsa: None,
            source: DemoSource {
                display_name: "demo.fe".to_string(),
                confidence: "coarse file-level fallback".to_string(),
                lines: vec![DemoSourceLine {
                    number: 1,
                    text: "</script>".to_string(),
                    classes: Vec::new(),
                }],
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
    fn live_reload_script_connects_to_event_stream() {
        let html = inject_live_reload_script("<html><body>x</body></html>");

        assert!(html.contains("new EventSource(\"/events\")"));
        assert!(html.contains("window.location.reload()"));
    }

    #[test]
    fn origin_closure_does_not_expand_through_code_object() {
        let pc0 = test_key("bytecode.pc", "runtime", "pc:0");
        let pc1 = test_key("bytecode.pc", "runtime", "pc:1");
        let code_object = test_key("code.object", "runtime", "runtime");
        let edges = vec![
            OriginEdgeFact::new(
                pc0.clone(),
                code_object.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(trace_facts::CompilerPhase::BytecodeEmission),
            ),
            OriginEdgeFact::new(
                pc1.clone(),
                code_object.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(trace_facts::CompilerPhase::BytecodeEmission),
            ),
        ];
        let mut edges_by_from = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut edges_by_to = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        for edge in &edges {
            edges_by_from
                .entry(edge.from.clone())
                .or_default()
                .push(edge);
            edges_by_to.entry(edge.to.clone()).or_default().push(edge);
        }
        let index = DemoIndex {
            origin_nodes: BTreeMap::new(),
            edges_by_from,
            edges_by_to,
            instructions: BTreeMap::new(),
            source_spans: BTreeMap::new(),
            edge_count: edges.len(),
            instruction_count: 0,
        };

        let closure = origin_closure(&pc0, &index);

        assert!(closure.keys.contains(&pc0));
        assert!(!closure.keys.contains(&pc1));
        assert!(!closure.keys.contains(&code_object));
        assert!(closure.edges.is_empty());
        assert_eq!(closure.traversal.skipped_hubs.len(), 1);
    }

    #[test]
    fn source_lines_ignore_foreign_source_spans() {
        let closure = DemoClosure {
            class_name: "trace-c-0".to_string(),
            label: "demo".to_string(),
            root_key: "root".to_string(),
            keys: Vec::new(),
            counts: DemoClosureCounts {
                hir: 1,
                mir: 0,
                sonatina_pre: 0,
                sonatina_post: 0,
                bytecode: 0,
            },
            traversal: test_traversal(),
            gap: None,
            edges: Vec::new(),
            source_spans: vec![DemoSourceSpan {
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

        let lines = source_lines("fib_demo.fe", "x", &[closure]);

        assert!(lines[0].classes.is_empty());

        let closure = DemoClosure {
            class_name: "trace-c-0".to_string(),
            label: "demo".to_string(),
            root_key: "root".to_string(),
            keys: Vec::new(),
            counts: DemoClosureCounts {
                hir: 1,
                mir: 0,
                sonatina_pre: 0,
                sonatina_post: 0,
                bytecode: 0,
            },
            traversal: test_traversal(),
            gap: None,
            edges: Vec::new(),
            source_spans: vec![DemoSourceSpan {
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

        let lines = source_lines("fib_demo.fe", "x", &[closure]);

        assert_eq!(lines[0].classes, vec!["trace-c-0".to_string()]);
    }

    #[test]
    fn closure_audit_flags_optimized_gap_without_calling_it_dead() {
        let mut closure = test_closure();
        closure.counts.sonatina_post = 2;
        closure.gap = closure_gap_note(&closure.counts);

        let groups = SourceSpanGroupIndex::new(
            "fib_demo.fe",
            std::slice::from_ref(&closure),
            &test_source_lines(),
        );
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::OptimizedAttributionGap);
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::SonatinaPost);
        assert!(
            entry
                .symptoms
                .contains(&ClosureAuditSymptom::MissingBytecode)
        );
        assert!(entry.notes.iter().any(|note| note.contains("not evidence")));
    }

    #[test]
    fn closure_audit_marks_source_to_bytecode_many_to_many() {
        let mut closure = test_closure();
        closure.counts.bytecode = 3;
        closure.keys = vec![
            "hir".to_string(),
            "mir".to_string(),
            "post".to_string(),
            "pc:1".to_string(),
            "pc:2".to_string(),
            "pc:3".to_string(),
        ];

        let groups = SourceSpanGroupIndex::new(
            "fib_demo.fe",
            std::slice::from_ref(&closure),
            &test_source_lines(),
        );
        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::GoodManyToMany);
        assert_eq!(entry.highest_phase_reached, ClosureAuditPhase::Bytecode);
        assert!(entry.symptoms.is_empty());
    }

    #[test]
    fn closure_audit_marks_same_span_source_only_sibling() {
        let mut source_only = test_closure();
        source_only.counts.mir = 0;
        source_only.counts.sonatina_pre = 0;
        let mut lowered = test_closure();
        lowered.class_name = "trace-c-1".to_string();
        lowered.root_key = "lowered".to_string();
        lowered.counts.bytecode = 1;
        let closures = vec![source_only.clone(), lowered];
        let groups = SourceSpanGroupIndex::new("fib_demo.fe", &closures, &test_source_lines());

        let entry = audit_closure("fib_demo.fe", 24, &source_only, &groups);

        assert_eq!(
            entry.primary,
            ClosureAuditPrimary::SourceSpanSiblingUnlowered
        );
        assert!(!entry.suspicious);

        let details = groups.mixed_details();
        assert_eq!(details.len(), 1);
        assert_eq!(details[0].source_text.as_deref(), Some("a = b"));
        assert_eq!(details[0].closures_with_targets, 1);
        assert_eq!(details[0].source_only_closures, 1);
        assert_eq!(details[0].target_connected_members.len(), 1);
        assert_eq!(details[0].target_connected_members[0].root_key, "lowered");
        assert_eq!(
            details[0].target_connected_members[0].highest_phase_reached,
            ClosureAuditPhase::Bytecode
        );
        assert_eq!(details[0].source_only_members.len(), 1);
        assert_eq!(details[0].source_only_members[0].root_key, "root");
        assert_eq!(
            details[0].source_only_members[0].highest_phase_reached,
            ClosureAuditPhase::Hir
        );
    }

    #[test]
    fn closure_audit_flags_preopt_elision_without_postopt() {
        let closure = test_closure();
        let groups = SourceSpanGroupIndex::new(
            "fib_demo.fe",
            std::slice::from_ref(&closure),
            &test_source_lines(),
        );

        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::PreoptElisionGap);
        assert!(entry.suspicious);
    }

    #[test]
    fn closure_audit_downgrades_synthetic_missing_source() {
        let mut closure = test_closure();
        closure.source_spans.clear();
        closure.counts.bytecode = 1;
        closure.edges.push(DemoClosureEdge {
            label: "BackendPrepared / Backend".to_string(),
            from: "backend event".to_string(),
            to: "bytecode.pc pc:0".to_string(),
        });
        let groups = SourceSpanGroupIndex::new(
            "fib_demo.fe",
            std::slice::from_ref(&closure),
            &test_source_lines(),
        );

        let entry = audit_closure("fib_demo.fe", 24, &closure, &groups);

        assert_eq!(entry.primary, ClosureAuditPrimary::ExpectedSynthetic);
        assert!(
            entry
                .symptoms
                .contains(&ClosureAuditSymptom::SourceUnavailableSynthetic)
        );
        assert!(!entry.suspicious);
    }

    fn test_key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn test_closure() -> DemoClosure {
        DemoClosure {
            class_name: "trace-c-0".to_string(),
            label: "hir.expr:demo:11".to_string(),
            root_key: "root".to_string(),
            keys: Vec::new(),
            counts: DemoClosureCounts {
                hir: 1,
                mir: 1,
                sonatina_pre: 1,
                sonatina_post: 0,
                bytecode: 0,
            },
            traversal: test_traversal(),
            gap: None,
            edges: Vec::new(),
            source_spans: vec![DemoSourceSpan {
                origin: "hir.expr:body:11".to_string(),
                file: "fib_demo.fe".to_string(),
                file_owner: "fib_demo.fe".to_string(),
                lines: "18:18".to_string(),
                start_byte: 0,
                end_byte: 1,
                start_line: 18,
                end_line: 18,
                confidence: "direct".to_string(),
            }],
        }
    }

    fn test_traversal() -> DemoClosureTraversalSummary {
        DemoClosureTraversalSummary {
            mode: "undirected_connected_origin_closure".to_string(),
            max_depth: CLOSURE_MAX_DEPTH,
            max_nodes: CLOSURE_MAX_NODES,
            truncated: false,
            truncation_reason: None,
            skipped_hubs: Vec::new(),
        }
    }

    fn test_source_lines() -> Vec<DemoSourceLine> {
        vec![DemoSourceLine {
            number: 18,
            text: "a = b".to_string(),
            classes: Vec::new(),
        }]
    }
}
