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

use crate::DevTraceWebDemoArgs;

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
            let mut emitter = incremental_emitter(source, args)?;
            render_incremental_trace(&mut emitter, 1, live_reload)
        }
        (None, None) => Err("pass either --from TRACE_JSONL or --source FE_FILE".to_string()),
        (Some(_), Some(_)) => Err("pass only one of --from or --source".to_string()),
    }
}

fn incremental_emitter(
    source: &Utf8PathBuf,
    args: &DevTraceWebDemoArgs,
) -> Result<super::trace_emit::IncrementalTraceEmitter, String> {
    let opt_level = args.optimize.parse::<codegen::OptLevel>()?;
    super::trace_emit::IncrementalTraceEmitter::new(
        source,
        args.standalone,
        &args.profile,
        opt_level,
        vec![
            "fe".to_string(),
            "dev".to_string(),
            "trace".to_string(),
            "web-demo".to_string(),
        ],
    )
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
                emitter: Box::new(incremental_emitter(source, args)?),
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

#[derive(Debug, Serialize)]
struct DemoClosure {
    class_name: String,
    label: String,
    root_key: String,
    #[serde(skip_serializing)]
    keys: Vec<String>,
    edges: Vec<DemoClosureEdge>,
    source_spans: Vec<DemoSourceSpan>,
}

#[derive(Debug, Serialize)]
struct DemoClosureEdge {
    label: String,
    from: String,
    to: String,
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
    let source_lines = source_lines(&source_text, &closures);
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
    let owner = span.file.owner_key();
    owner == input_path || owner.ends_with(input_path)
}

fn build_closures(roots: Vec<OriginExportKey>, index: &DemoIndex<'_>) -> Vec<DemoClosure> {
    roots
        .into_iter()
        .enumerate()
        .map(|(ordinal, root)| {
            let (keys, edges) = origin_closure(&root, index);
            let source_spans = source_spans_for_keys(&keys, index);
            DemoClosure {
                class_name: format!("trace-c-{ordinal}"),
                label: closure_label(&root, index),
                root_key: root.canonical_storage_key(),
                keys: keys
                    .iter()
                    .map(OriginExportKey::canonical_storage_key)
                    .collect(),
                edges,
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

fn source_lines(source_text: &str, closures: &[DemoClosure]) -> Vec<DemoSourceLine> {
    let mut classes_by_line = BTreeMap::<u32, BTreeSet<String>>::new();
    for closure in closures {
        for span in &closure.source_spans {
            if span.confidence != "direct" {
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

fn origin_closure(
    start: &OriginExportKey,
    index: &DemoIndex<'_>,
) -> (BTreeSet<OriginExportKey>, Vec<DemoClosureEdge>) {
    let mut keys = BTreeSet::new();
    let mut edges_out = Vec::new();
    let mut seen_edges = BTreeSet::new();
    let mut queue = VecDeque::from([(start.clone(), 0_usize)]);
    while let Some((key, depth)) = queue.pop_front() {
        if depth >= 8 || !keys.insert(key.clone()) {
            continue;
        }
        let outgoing = index.edges_by_from.get(&key).into_iter().flatten().copied();
        let incoming = index.edges_by_to.get(&key).into_iter().flatten().copied();
        for edge in outgoing.chain(incoming) {
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
    (keys, edges_out)
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
}
