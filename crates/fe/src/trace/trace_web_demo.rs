use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    fs,
};

use common::origin::OriginExportKey;
use fe_web::escape::escape_script_content;
use serde::Serialize;
use trace_facts::{OriginEdgeFact, OriginEdgeLabel, SourceSpanFact, TraceFact, TraceSnapshot};
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
        model.bytecode.len(),
        model.source.confidence,
    ))
}

#[derive(Debug, Serialize)]
struct WebDemoModel {
    metadata: DemoMetadata,
    counts: DemoCounts,
    source: DemoSource,
    loop_view: DemoLoop,
    bytecode: Vec<DemoBytecode>,
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
    text: String,
    confidence: String,
}

#[derive(Debug, Serialize)]
struct DemoLoop {
    available: bool,
    label: String,
    blocks: Vec<DemoLoopBlock>,
}

#[derive(Debug, Serialize)]
struct DemoLoopBlock {
    label: String,
    role: String,
    instructions: Vec<String>,
}

#[derive(Debug, Serialize)]
struct DemoBytecode {
    pc: String,
    index: u32,
    mnemonic: String,
    byte_range: String,
    gas: Option<u64>,
    opcode: Option<String>,
    immediate: Option<String>,
    chain: Vec<DemoOriginHop>,
    source_spans: Vec<DemoSourceSpan>,
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

#[derive(Debug, Serialize)]
struct DemoSourceSpan {
    origin: String,
    file: String,
    lines: String,
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

    let bytecode = loop_report
        .target_instructions
        .iter()
        .map(|instruction| {
            let extent = index.instruction_extents.get(&instruction.key);
            let opcode = index.opcodes.get(&instruction.key);
            let chain = origin_chain(&instruction.key, &index);
            let source_spans = source_spans_for_chain(&chain, &index);
            DemoBytecode {
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
            }
        })
        .collect::<Vec<_>>();

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
            text: source_text,
            confidence: source_confidence,
        },
        loop_view: DemoLoop {
            available: loop_report.available,
            label: loop_report
                .loop_label
                .unwrap_or_else(|| "no loop selected".to_string()),
            blocks: loop_report
                .blocks
                .into_iter()
                .map(|block| DemoLoopBlock {
                    label: block.block.display_label(),
                    role: block.role,
                    instructions: block
                        .instructions
                        .into_iter()
                        .map(|instruction| {
                            format!("ir[{}] {}", instruction.index, instruction.mnemonic)
                        })
                        .collect(),
                })
                .collect(),
        },
        notes: demo_notes(&bytecode, exact_source_spans),
        bytecode,
    }
}

struct DemoIndex<'a> {
    edges_by_from: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
    instruction_extents: BTreeMap<OriginExportKey, &'a trace_facts::InstructionExtentFact>,
    opcodes: BTreeMap<OriginExportKey, &'a trace_facts::OpcodeFact>,
    static_gas: BTreeMap<OriginExportKey, u64>,
    source_spans: BTreeMap<OriginExportKey, &'a SourceSpanFact>,
    edge_count: usize,
    instruction_count: usize,
}

impl<'a> DemoIndex<'a> {
    fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut edges_by_from = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut instruction_extents = BTreeMap::new();
        let mut opcodes = BTreeMap::new();
        let mut static_gas = BTreeMap::new();
        let mut source_spans = BTreeMap::new();
        let mut edge_count = 0;
        let mut instruction_count = 0;

        for fact in snapshot.facts() {
            match fact {
                TraceFact::OriginEdge(edge) => {
                    edge_count += 1;
                    edges_by_from
                        .entry(edge.from.clone())
                        .or_default()
                        .push(edge);
                }
                TraceFact::Instruction(_) => instruction_count += 1,
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
            edges_by_from,
            instruction_extents,
            opcodes,
            static_gas,
            source_spans,
            edge_count,
            instruction_count,
        }
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
        .map(|(origin, span)| DemoSourceSpan {
            origin: origin.display_label(),
            file: span.file.display_label(),
            lines: format!("{}:{}", span.start_line, span.end_line),
            confidence: if origin.kind() == "code.object" {
                "coarse".to_string()
            } else {
                "direct".to_string()
            },
        })
        .collect()
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
    Ok(format!(
        r#"<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Fe Origin Trace Demo</title>
<style>
:root {{ color-scheme: dark; --bg:#11130f; --panel:#191d16; --ink:#f4f0df; --muted:#aaa58e; --line:#343a2d; --hot:#f2b84b; --ok:#9ad97f; --bad:#ff8070; --blue:#7bb4ff; }}
* {{ box-sizing: border-box; }}
body {{ margin:0; background:radial-gradient(circle at 20% 0%, #2b2b18 0, transparent 35rem), linear-gradient(135deg,#11130f,#181410 55%,#10151a); color:var(--ink); font:15px/1.45 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace; }}
header {{ padding:28px 32px 18px; border-bottom:1px solid var(--line); }}
h1 {{ margin:0 0 8px; font:700 30px/1.1 Georgia,serif; letter-spacing:.02em; }}
.subtitle {{ color:var(--muted); max-width:1000px; }}
.cards {{ display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:12px; padding:16px 32px; }}
.card,.panel {{ background:color-mix(in srgb,var(--panel),transparent 8%); border:1px solid var(--line); border-radius:18px; box-shadow:0 20px 50px #0008; }}
.card {{ padding:14px 16px; }}
.card b {{ display:block; color:var(--hot); font-size:20px; margin-top:4px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }}
.grid {{ display:grid; grid-template-columns:minmax(320px,0.9fr) minmax(360px,1.1fr) minmax(320px,.9fr); gap:14px; padding:0 32px 32px; }}
.panel {{ min-height:360px; overflow:hidden; }}
.panel h2 {{ margin:0; padding:14px 16px; border-bottom:1px solid var(--line); color:var(--ok); font-size:14px; text-transform:uppercase; letter-spacing:.13em; }}
.source {{ max-height:690px; overflow:auto; padding:12px 0; }}
.line {{ display:grid; grid-template-columns:54px 1fr; gap:10px; padding:0 14px; white-space:pre; }}
.ln {{ color:#6f765f; text-align:right; user-select:none; }}
.code {{ color:#e9e2c7; }}
.blocks {{ padding:14px 16px; border-bottom:1px solid var(--line); max-height:260px; overflow:auto; }}
.block {{ margin:0 0 12px; }}
.block-title {{ color:var(--blue); margin-bottom:4px; }}
.inst {{ color:var(--muted); padding-left:14px; }}
table {{ width:100%; border-collapse:collapse; }}
th,td {{ padding:8px 10px; border-bottom:1px solid #2b3027; text-align:left; vertical-align:top; }}
th {{ color:var(--muted); font-weight:600; position:sticky; top:0; background:#171b14; }}
tr.bytecode-row {{ cursor:pointer; }}
tr.bytecode-row:hover, tr.bytecode-row.active {{ background:#2a2616; color:#fff4ca; }}
.bytecode-wrap {{ max-height:410px; overflow:auto; }}
.detail {{ padding:14px 16px; }}
.pill {{ display:inline-block; padding:2px 8px; border:1px solid var(--line); border-radius:999px; color:var(--muted); margin:0 5px 5px 0; }}
.hop {{ margin:10px 0; padding:10px; background:#11160f; border:1px solid #2b3326; border-radius:12px; }}
.hop .edge {{ color:var(--hot); }}
.full {{ color:#787f6c; font-size:12px; overflow-wrap:anywhere; }}
.notes {{ padding:12px 32px 30px; color:var(--muted); }}
.notes li {{ margin:6px 0; }}
@media (max-width:1100px) {{ .cards,.grid {{ grid-template-columns:1fr; }} }}
</style>
</head>
<body>
<header>
  <h1>Fe Origin Trace Demo</h1>
  <div class="subtitle">Interactive prototype over compiler-emitted trace facts: source file, Sonatina loop membership, final bytecode PCs, and origin edges. Derived view only.</div>
</header>
<section class="cards" id="cards"></section>
<main class="grid">
  <section class="panel"><h2>Source</h2><div id="source" class="source"></div></section>
  <section class="panel"><h2>Loop And Bytecode</h2><div id="blocks" class="blocks"></div><div class="bytecode-wrap"><table><thead><tr><th>pc</th><th>asm</th><th>bytes</th><th>gas</th></tr></thead><tbody id="bytecode"></tbody></table></div></section>
  <section class="panel"><h2>Origin Chain</h2><div id="detail" class="detail"></div></section>
</main>
<ul class="notes" id="notes"></ul>
<script type="application/json" id="trace-data">{}</script>
<script>
const data = JSON.parse(document.getElementById('trace-data').textContent);
const el = (tag, cls, text) => {{ const n=document.createElement(tag); if(cls) n.className=cls; if(text!==undefined) n.textContent=text; return n; }};
function card(label, value) {{ const c=el('div','card'); c.append(label, el('b','', String(value))); return c; }}
document.getElementById('cards').append(
  card('data source', data.metadata.data_source),
  card('facts', data.counts.facts),
  card('loop bytecode PCs', data.bytecode.length),
  card('source confidence', data.source.confidence)
);
const src = document.getElementById('source');
(data.source.text || '(source file not readable from this working directory)').split('\n').forEach((line,i)=>{{ const row=el('div','line'); row.append(el('span','ln',i+1), el('span','code',line)); src.append(row); }});
const blocks = document.getElementById('blocks');
blocks.append(el('div','block-title',data.loop_view.label));
data.loop_view.blocks.forEach(b=>{{ const box=el('div','block'); box.append(el('div','block-title',`${{b.label}} [${{b.role}}]`)); b.instructions.forEach(i=>box.append(el('div','inst',i))); blocks.append(box); }});
const tbody = document.getElementById('bytecode');
data.bytecode.forEach((row,i)=>{{ const tr=el('tr','bytecode-row'); tr.dataset.i=i; [row.pc,row.mnemonic,row.byte_range,row.gas ?? ''].forEach(v=>tr.append(el('td','',v))); tbody.append(tr); }});
function renderDetail(i) {{
  const row = data.bytecode[i]; const d = document.getElementById('detail'); d.textContent='';
  d.append(el('div','pill',row.pc), el('div','pill',row.mnemonic), el('div','pill',`bytes ${{row.byte_range}}`));
  if (row.immediate) d.append(el('div','pill',`imm ${{row.immediate}}`));
  if (row.source_spans.length) {{ d.append(el('h3','', 'Source spans')); row.source_spans.forEach(s=>d.append(el('div','hop',`${{s.origin}} -> ${{s.file}} lines ${{s.lines}} (${{s.confidence}})`))); }}
  d.append(el('h3','', 'Origin edges'));
  row.chain.forEach(h=>{{ const box=el('div','hop'); box.append(el('div','edge',`${{h.label}} / ${{h.phase}}`), el('div','',`${{h.from.short}} -> ${{h.to.short}}`), el('div','full',h.to.full)); d.append(box); }});
}}
tbody.addEventListener('click', e=>{{ const tr=e.target.closest('tr'); if(!tr) return; document.querySelectorAll('.bytecode-row').forEach(r=>r.classList.remove('active')); tr.classList.add('active'); renderDetail(Number(tr.dataset.i)); }});
document.getElementById('notes').append(...data.notes.map(n=>el('li','',n)));
if (data.bytecode.length) {{ tbody.querySelector('tr').classList.add('active'); renderDetail(0); }}
</script>
</body>
</html>"#,
        escape_script_content(&data)
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
                text: "</script>".to_string(),
                confidence: "coarse file-level fallback".to_string(),
            },
            loop_view: DemoLoop {
                available: true,
                label: "loop".to_string(),
                blocks: Vec::new(),
            },
            bytecode: Vec::new(),
            notes: Vec::new(),
        };

        let html = render_origin_trace_html(&model).unwrap();

        assert!(html.contains(r"<\/script>"));
        assert!(html.contains("Fe Origin Trace Demo"));
    }
}
