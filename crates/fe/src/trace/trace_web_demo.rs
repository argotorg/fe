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
use trace_facts::{InstructionFact, OriginNodeFact, SourceSpanFact, TraceFact, TraceSnapshot};
use trace_query::{
    IntrospectionService, LoopContentsRequest, TraceIntrospectionService,
    origin_closure::{
        ClosureAuditReport, OriginClosure as DemoClosure, OriginClosureSourceLine,
        audit_origin_closures, build_origin_closure_set, classes_by_origin_key,
        source_owner_matches_input,
    },
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
        rendered.model.metadata.data_source,
        rendered.model.bytecode_count,
        rendered.model.source.confidence,
        rendered.salsa_summary(),
    ))
}

pub(super) fn run_trace_audit_closures(args: &DevTraceAuditClosuresArgs) -> Result<String, String> {
    let model = render_trace_audit_model_once(args)?;
    let source_lines = model
        .source
        .lines
        .iter()
        .map(|line| OriginClosureSourceLine {
            number: line.number,
            text: line.text.clone(),
        })
        .collect::<Vec<_>>();
    let report = audit_origin_closures(
        &model.metadata.input_path,
        &model.metadata.target,
        &model.metadata.data_source,
        model.bytecode_count,
        &model.closures,
        &source_lines,
    );
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
    audit: Option<ClosureAuditReport>,
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
    let closure_set = build_origin_closure_set(
        snapshot.metadata().input_path.as_str(),
        snapshot,
        &loop_report,
    );
    let closures = closure_set.closures;
    let classes_by_key = classes_by_origin_key(&closures);
    let source_lines = source_lines(
        snapshot.metadata().input_path.as_str(),
        &source_text,
        &closures,
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
            origin_edges: closure_set.edge_count,
            instructions: closure_set.instruction_count,
            source_spans: closure_set.source_span_count,
        },
        salsa,
        audit: Some(audit),
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
    instructions: BTreeMap<OriginExportKey, &'a InstructionFact>,
    source_spans: BTreeMap<OriginExportKey, &'a SourceSpanFact>,
}

impl<'a> DemoIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut origin_nodes = BTreeMap::new();
        let mut instructions = BTreeMap::new();
        let mut source_spans = BTreeMap::new();

        for fact in snapshot.facts() {
            match fact {
                TraceFact::OriginNode(node) => {
                    origin_nodes.insert(node.key.clone(), node);
                }
                TraceFact::Instruction(instruction) => {
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
            instructions,
            source_spans,
        }
    }
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
            counts: DemoCounts {
                facts: 1,
                origin_edges: 0,
                instructions: 0,
                source_spans: 0,
            },
            salsa: None,
            audit: None,
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
