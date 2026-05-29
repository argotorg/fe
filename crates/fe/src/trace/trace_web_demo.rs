use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    fs,
};

use common::origin::OriginExportKey;
use serde::Serialize;
use trace_facts::{
    InstructionFact, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, SourceSpanFact, TraceFact,
    TraceSnapshot,
};
use trace_query::{IntrospectionService, LoopContentsRequest, TraceIntrospectionService};

use crate::DevTraceWebDemoArgs;

pub(super) fn run_trace_web_demo(args: &DevTraceWebDemoArgs) -> Result<String, String> {
    let snapshot = super::read_trace_snapshot_jsonl_from_path(&args.from)?;
    let model = build_demo_model(&snapshot);
    let html = render_origin_trace_html(&model)?;
    fs::write(args.out.as_std_path(), html)
        .map_err(|err| format!("failed to write web demo {}: {err}", args.out))?;
    Ok(format!(
        "wrote origin trace web demo: {}\nData source: {}\nLoop bytecode PCs: {}\nSource confidence: {}\n",
        args.out,
        super::format_data_source(snapshot.metadata()),
        model.bytecode_count,
        model.source.confidence,
    ))
}

#[derive(Debug, Serialize)]
struct WebDemoModel {
    metadata: DemoMetadata,
    counts: DemoCounts,
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
struct DemoBytecode {
    key: String,
    pc: String,
    index: u32,
    mnemonic: String,
    byte_range: String,
    gas: Option<u64>,
    opcode: Option<String>,
    immediate: Option<String>,
    chain: Vec<DemoOriginHop>,
    source_spans: Vec<DemoSourceSpan>,
    classes: Vec<String>,
}

#[derive(Debug, Serialize)]
struct DemoOriginHop {
    depth: usize,
    label: String,
    phase: String,
    from: DemoKey,
    to: DemoKey,
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

fn build_demo_model(snapshot: &TraceSnapshot) -> WebDemoModel {
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

    let mut bytecode = loop_report
        .target_instructions
        .iter()
        .enumerate()
        .map(|(closure_index, instruction)| {
            let extent = index.instruction_extents.get(&instruction.key);
            let opcode = index.opcodes.get(&instruction.key);
            let chain = origin_chain(&instruction.key, &index);
            let source_spans = source_spans_for_chain(&chain, &index);
            let class_name = format!("trace-c-{closure_index}");
            DemoBytecode {
                key: instruction.key.canonical_storage_key(),
                pc: instruction.key.display_label(),
                index: instruction.index,
                mnemonic: instruction.mnemonic.clone(),
                byte_range: extent
                    .map(|extent| format!("{}..{}", extent.pc_range.start, extent.pc_range.end))
                    .unwrap_or_else(|| "unknown".to_string()),
                gas: index.static_gas.get(&instruction.key).copied(),
                opcode: opcode.map(|opcode| opcode.opcode.clone()),
                immediate: opcode.and_then(|opcode| opcode.immediate.clone()),
                chain,
                source_spans,
                classes: vec![class_name],
            }
        })
        .collect::<Vec<_>>();
    let closures = build_closures(&bytecode);
    for (row, closure) in bytecode.iter_mut().zip(&closures) {
        row.classes = vec![closure.class_name.clone()];
    }
    let classes_by_key = classes_by_key(&bytecode);
    let source_lines = source_lines(&source_text, &closures);
    let mut panels = build_origin_panels(&index, &classes_by_key);
    panels.insert(1, loop_panel(loop_report, &classes_by_key));

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
        source: DemoSource {
            display_name: snapshot.metadata().input_path.clone(),
            confidence: source_confidence,
            lines: source_lines,
        },
        panels,
        closures,
        bytecode_count: bytecode.len(),
        notes: demo_notes(&bytecode, exact_source_spans),
    }
}

struct DemoIndex<'a> {
    origin_nodes: BTreeMap<OriginExportKey, &'a OriginNodeFact>,
    edges_by_from: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
    instructions: BTreeMap<OriginExportKey, &'a InstructionFact>,
    instruction_extents: BTreeMap<OriginExportKey, &'a trace_facts::InstructionExtentFact>,
    opcodes: BTreeMap<OriginExportKey, &'a trace_facts::OpcodeFact>,
    static_gas: BTreeMap<OriginExportKey, u64>,
    source_spans: BTreeMap<OriginExportKey, &'a SourceSpanFact>,
    edge_count: usize,
    instruction_count: usize,
}

impl<'a> DemoIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut origin_nodes = BTreeMap::new();
        let mut edges_by_from = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut instructions = BTreeMap::new();
        let mut instruction_extents = BTreeMap::new();
        let mut opcodes = BTreeMap::new();
        let mut static_gas = BTreeMap::new();
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
                }
                TraceFact::Instruction(instruction) => {
                    instruction_count += 1;
                    instructions.insert(instruction.instruction.clone(), instruction);
                }
                TraceFact::InstructionExtent(extent) => {
                    instruction_extents.insert(extent.instruction.clone(), extent);
                }
                TraceFact::Opcode(opcode) => {
                    opcodes.insert(opcode.pc.clone(), opcode);
                }
                TraceFact::StaticGas(gas) => {
                    static_gas
                        .entry(gas.instruction.clone())
                        .or_insert(gas.base_cost);
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
            instructions,
            instruction_extents,
            opcodes,
            static_gas,
            source_spans,
            edge_count,
            instruction_count,
        }
    }
}

fn build_closures(bytecode: &[DemoBytecode]) -> Vec<DemoClosure> {
    bytecode
        .iter()
        .enumerate()
        .map(|(index, row)| DemoClosure {
            class_name: format!("trace-c-{index}"),
            label: format!("{} {}", row.pc, row.mnemonic),
            root_key: row.key.clone(),
            edges: row
                .chain
                .iter()
                .map(|hop| DemoClosureEdge {
                    label: format!("{} / {}", hop.label, hop.phase),
                    from: hop.from.short.clone(),
                    to: hop.to.short.clone(),
                })
                .collect(),
            source_spans: row.source_spans.clone(),
        })
        .collect()
}

fn classes_by_key(bytecode: &[DemoBytecode]) -> BTreeMap<String, Vec<String>> {
    let mut classes = BTreeMap::<String, BTreeSet<String>>::new();
    for row in bytecode {
        for class_name in &row.classes {
            classes
                .entry(row.key.clone())
                .or_default()
                .insert(class_name.clone());
            for hop in &row.chain {
                classes
                    .entry(hop.from.full.clone())
                    .or_default()
                    .insert(class_name.clone());
                classes
                    .entry(hop.to.full.clone())
                    .or_default()
                    .insert(class_name.clone());
            }
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
    loop_report: trace_query::LoopContentsReport,
    classes_by_key: &BTreeMap<String, Vec<String>>,
) -> DemoPanel {
    let mut rows = Vec::new();
    let loop_label = loop_report
        .loop_label
        .clone()
        .unwrap_or_else(|| "selected loop".to_string());
    for block in loop_report.blocks {
        let block_key = block.block.canonical_storage_key();
        rows.push(DemoPanelRow {
            key: Some(block_key.clone()),
            label: block.block.local_key().to_string(),
            meta: format!("loop {}", block.role),
            text: loop_label.clone(),
            classes: classes_by_key.get(&block_key).cloned().unwrap_or_default(),
        });
        for instruction in block.instructions {
            let key = instruction.key.canonical_storage_key();
            rows.push(DemoPanelRow {
                key: Some(key.clone()),
                label: instruction.key.local_key().to_string(),
                meta: format!("ir[{}]", instruction.index),
                text: instruction.mnemonic,
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

fn origin_chain(start: &OriginExportKey, index: &DemoIndex<'_>) -> Vec<DemoOriginHop> {
    let mut out = Vec::new();
    let mut seen = BTreeSet::new();
    let mut queue = VecDeque::from([(start.clone(), 0_usize)]);
    while let Some((key, depth)) = queue.pop_front() {
        if depth >= 4 || !seen.insert(key.clone()) {
            continue;
        }
        let Some(edges) = index.edges_by_from.get(&key) else {
            continue;
        };
        for edge in edges {
            if !matches!(
                edge.label,
                OriginEdgeLabel::LoweredFrom
                    | OriginEdgeLabel::EmittedFrom
                    | OriginEdgeLabel::SyntheticFor
                    | OriginEdgeLabel::BackendPrepared
            ) {
                continue;
            }
            out.push(DemoOriginHop {
                depth,
                label: format!("{:?}", edge.label),
                phase: edge
                    .introduced_by
                    .map(|phase| format!("{phase:?}"))
                    .unwrap_or_else(|| "unknown".to_string()),
                from: demo_key(&edge.from),
                to: demo_key(&edge.to),
            });
            queue.push_back((edge.to.clone(), depth + 1));
        }
    }
    out
}

fn source_spans_for_chain(chain: &[DemoOriginHop], index: &DemoIndex<'_>) -> Vec<DemoSourceSpan> {
    let mut keys = BTreeSet::new();
    for hop in chain {
        keys.insert(hop.from.full.clone());
        keys.insert(hop.to.full.clone());
    }
    index
        .source_spans
        .iter()
        .filter(|(origin, _)| keys.contains(&origin.canonical_storage_key()))
        .filter(|(_, span)| !has_finer_span(span, &keys, index))
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

fn demo_notes(bytecode: &[DemoBytecode], exact_source_spans: bool) -> Vec<String> {
    let mut notes = vec![
        "This page is a derived view over validated trace JSONL; it does not define compiler identity.".to_string(),
        "Bytecode rows are selected from the loop-contents report, so they are tied to compiler-emitted Sonatina loop facts.".to_string(),
    ];
    if bytecode.is_empty() {
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

fn render_origin_trace_html(model: &WebDemoModel) -> Result<String, String> {
    let data = serde_json::to_string(model)
        .map_err(|err| format!("failed to serialize web demo model: {err}"))?;
    Ok(fe_web::assets::origin_trace_html_shell(
        "Fe Origin Trace Demo",
        &data,
    ))
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

        let html = render_origin_trace_html(&model).unwrap();

        assert!(html.contains(r"<\/script>"));
        assert!(html.contains("fe-origin-trace"));
        assert!(html.contains("Fe Origin Trace Demo"));
    }
}
