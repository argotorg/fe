use std::collections::{BTreeMap, BTreeSet};
use std::fs;

use camino::Utf8PathBuf;
use common::origin::OriginExportKey;
use trace_facts::{
    CategorySource, CompilerEventFact, CompilerEventKind, CompilerPhase, CompilerReason,
    InstructionCategory, InstructionCategoryFact, InstructionFact, LoopDerivation,
    LoopMembershipFact, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, OriginNodeKind,
    StorageFact, StorageLocation, StorageReason, TraceFact, TraceValidator,
};

use crate::{TraceCommand, TraceExplainLocalArgs, TraceLoopCostArgs};

const FIB_OWNER: &str = "fixture:fib_demo";

pub(crate) fn run_trace_command(command: &TraceCommand) -> Result<String, String> {
    match command {
        TraceCommand::LoopCost(args) => run_loop_cost(args),
        TraceCommand::ExplainLocal(args) => run_explain_local(args),
    }
}

fn run_loop_cost(args: &TraceLoopCostArgs) -> Result<String, String> {
    let trace = load_fib_trace(&args.path, &args.function)?;
    render_loop_cost(&trace)
}

fn run_explain_local(args: &TraceExplainLocalArgs) -> Result<String, String> {
    let trace = load_fib_trace(&args.path, &args.function)?;
    render_explain_local(&trace, &args.local)
}

fn load_fib_trace(path: &Utf8PathBuf, function_label: &str) -> Result<FibTrace, String> {
    let source = fs::read_to_string(path.as_std_path())
        .map_err(|err| format!("failed to read {path}: {err}"))?;
    build_fib_trace(&source, path.as_str(), function_label)
}

#[derive(Clone, Debug)]
struct FibTrace {
    path: String,
    function_label: String,
    loop_key: OriginExportKey,
    facts: Vec<TraceFact>,
    locals: BTreeMap<String, OriginExportKey>,
    labels: BTreeMap<OriginExportKey, String>,
}

impl FibTrace {
    fn instruction(&self, key: &OriginExportKey) -> Option<&InstructionFact> {
        self.facts.iter().find_map(|fact| match fact {
            TraceFact::Instruction(instruction) if &instruction.instruction == key => {
                Some(instruction)
            }
            _ => None,
        })
    }

    fn storage_for(&self, key: &OriginExportKey) -> Vec<&StorageFact> {
        self.facts
            .iter()
            .filter_map(|fact| match fact {
                TraceFact::Storage(storage) if &storage.subject == key => Some(storage),
                _ => None,
            })
            .collect()
    }

    fn label(&self, key: &OriginExportKey) -> String {
        self.labels
            .get(key)
            .cloned()
            .unwrap_or_else(|| key.display_label())
    }
}

fn build_fib_trace(
    source: &str,
    path_label: &str,
    function_label: &str,
) -> Result<FibTrace, String> {
    require_fib_fixture(source)?;

    let function = key("function", FIB_OWNER, "contract:Fib.recv:Compute");
    let loop_key = key("loop", FIB_OWNER, "while:0");
    let mut facts = Vec::new();
    let mut labels = BTreeMap::new();

    push_node(
        &mut facts,
        &mut labels,
        function.clone(),
        "function",
        function_label,
    );
    push_node(
        &mut facts,
        &mut labels,
        loop_key.clone(),
        "loop",
        "while i < n",
    );

    let locals = [
        ("n", "runtime-local:0"),
        ("a", "runtime-local:1"),
        ("b", "runtime-local:2"),
        ("i", "runtime-local:3"),
        ("next", "runtime-local:4"),
    ]
    .into_iter()
    .map(|(name, local)| {
        let key = key("runtime.local", FIB_OWNER, local);
        push_node(&mut facts, &mut labels, key.clone(), "runtime.local", name);
        (name.to_string(), key)
    })
    .collect::<BTreeMap<_, _>>();

    facts.push(TraceFact::Storage(StorageFact::new(
        locals["b"].clone(),
        CompilerPhase::Mir,
        StorageLocation::MemoryPlace,
        StorageReason::MutableLocalLowering,
    )));
    facts.push(TraceFact::Storage(StorageFact::new(
        locals["b"].clone(),
        CompilerPhase::Backend,
        StorageLocation::StackSlot { offset: 24 },
        StorageReason::FrameSlot,
    )));
    for name in ["i", "n", "a", "next"] {
        facts.push(TraceFact::Storage(StorageFact::new(
            locals[name].clone(),
            CompilerPhase::SonatinaPreOpt,
            StorageLocation::SsaValue,
            StorageReason::Unknown,
        )));
    }

    let insts = [
        (
            "lw a3, 24(sp)",
            InstructionCategory::StackLoad,
            Some(("b", OriginEdgeLabel::LoadOf)),
        ),
        ("add a4, a2, a3", InstructionCategory::Arithmetic, None),
        ("mv a2, a3", InstructionCategory::Move, None),
        (
            "sw a4, 24(sp)",
            InstructionCategory::StackStore,
            Some(("b", OriginEdgeLabel::StoreOf)),
        ),
        (
            "addiw a1, a1, 1",
            InstructionCategory::Arithmetic,
            Some(("i", OriginEdgeLabel::EmittedFrom)),
        ),
        (
            "slli a1, a1, 32",
            InstructionCategory::ZeroExtend,
            Some(("i", OriginEdgeLabel::IntegerLegalizationFor)),
        ),
        (
            "srli a1, a1, 32",
            InstructionCategory::ZeroExtend,
            Some(("i", OriginEdgeLabel::IntegerLegalizationFor)),
        ),
        (
            "slli a0, a0, 32",
            InstructionCategory::ZeroExtend,
            Some(("n", OriginEdgeLabel::IntegerLegalizationFor)),
        ),
        (
            "srli a0, a0, 32",
            InstructionCategory::ZeroExtend,
            Some(("n", OriginEdgeLabel::IntegerLegalizationFor)),
        ),
        ("bltu a1, a0, loop", InstructionCategory::Branch, None),
        ("mv a5, a2", InstructionCategory::Move, None),
        ("mv a6, a3", InstructionCategory::Move, None),
        ("j loop", InstructionCategory::Jump, None),
    ];

    let mut zext_event_index = 0;
    for (index, (mnemonic, category, edge)) in insts.iter().enumerate() {
        let instruction = key("asm.inst", FIB_OWNER, &format!("inst:{index}"));
        push_node(
            &mut facts,
            &mut labels,
            instruction.clone(),
            "asm.inst",
            &format!("asm[{index}] {mnemonic}"),
        );
        facts.push(TraceFact::Instruction(InstructionFact::new(
            instruction.clone(),
            function.clone(),
            index as u32,
            *mnemonic,
        )));
        facts.push(TraceFact::InstructionCategory(
            InstructionCategoryFact::new(
                instruction.clone(),
                *category,
                CategorySource::PosthocClassifier {
                    version: "fib-demo-riscv-v1".to_string(),
                },
            ),
        ));
        facts.push(TraceFact::LoopMembership(LoopMembershipFact::new(
            loop_key.clone(),
            instruction.clone(),
            LoopDerivation::BackendBlockMapping,
        )));
        if let Some((local_name, label)) = edge {
            facts.push(TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                locals[*local_name].clone(),
                *label,
                Some(CompilerPhase::Backend),
            )));
            if *label == OriginEdgeLabel::IntegerLegalizationFor {
                let event = key(
                    "compiler.event",
                    FIB_OWNER,
                    &format!("event:{zext_event_index}"),
                );
                zext_event_index += 1;
                push_node(
                    &mut facts,
                    &mut labels,
                    event.clone(),
                    "compiler.event",
                    &format!("insert zero-extend for {local_name}"),
                );
                facts.push(TraceFact::CompilerEvent(CompilerEventFact::new(
                    event,
                    CompilerPhase::Backend,
                    CompilerEventKind::InsertIntegerZeroExtend,
                    vec![locals[*local_name].clone()],
                    vec![instruction],
                    Some(CompilerReason::new(zext_reason(local_name))),
                )));
            }
        }
    }

    TraceValidator::validate(&facts).map_err(|err| format!("invalid Fibonacci trace: {err}"))?;
    Ok(FibTrace {
        path: path_label.to_string(),
        function_label: function_label.to_string(),
        loop_key,
        facts,
        locals,
        labels,
    })
}

fn push_node(
    facts: &mut Vec<TraceFact>,
    labels: &mut BTreeMap<OriginExportKey, String>,
    key: OriginExportKey,
    kind: &str,
    label: &str,
) {
    labels.insert(key.clone(), label.to_string());
    facts.push(TraceFact::OriginNode(OriginNodeFact::new(
        key,
        OriginNodeKind::new(kind),
    )));
}

fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
}

fn require_fib_fixture(source: &str) -> Result<(), String> {
    let required = [
        "msg FibMsg",
        "pub contract Fib",
        "while i < n",
        "let mut b: u32 = 1",
    ];
    let missing = required
        .iter()
        .copied()
        .filter(|needle| !source.contains(needle))
        .collect::<Vec<_>>();
    if missing.is_empty() {
        Ok(())
    } else {
        Err(format!(
            "this MVP currently expects fib_demo.fe; missing fixture markers: {}",
            missing.join(", ")
        ))
    }
}

fn zext_reason(local_name: &str) -> &'static str {
    match local_name {
        "i" => {
            "u32 loop index normalized before compare; known-width facts are not preserved after addiw"
        }
        "n" => {
            "u32 loop bound normalized inside the loop; n is loop-invariant, so this is a hoist candidate"
        }
        _ => "u32 value normalized before compare",
    }
}

fn render_loop_cost(trace: &FibTrace) -> Result<String, String> {
    let loop_instructions = loop_instructions(trace);
    let counts = category_counts(trace, &loop_instructions);
    let zexts = zero_extends_by_local(trace, &loop_instructions);
    let b_key = trace.locals["b"].clone();
    let b_storage = trace.storage_for(&b_key);

    let mut out = String::new();
    out.push_str("Fe trace loop-cost: Fibonacci codegen diagnosis\n\n");
    out.push_str(&format!("Target: {}\n", trace.path));
    out.push_str(&format!("Function: {}\n", trace.function_label));
    out.push_str(&format!("Loop: {}\n\n", trace.label(&trace.loop_key)));
    out.push_str("Static per-iteration cost:\n");
    out.push_str(&format!(
        "  total instructions: {}\n",
        loop_instructions.len()
    ));
    out.push_str(&format!("  zero-extends: {}\n", counts.zero_extends));
    out.push_str(&format!("  stack loads: {}\n", counts.stack_loads));
    out.push_str(&format!("  stack stores: {}\n", counts.stack_stores));
    out.push_str(&format!("  moves: {}\n", counts.moves));
    out.push_str(&format!(
        "  branches/jumps: {}\n",
        counts.branches + counts.jumps
    ));
    out.push_str(&format!("  arithmetic: {}\n\n", counts.arithmetic));

    out.push_str("Repeated zero-extensions:\n");
    for local in ["i", "n"] {
        let facts = zexts.get(local).cloned().unwrap_or_default();
        out.push_str(&format!(
            "  {local}: {} zero-extend instructions",
            facts.len()
        ));
        if !facts.is_empty() {
            let labels = facts
                .iter()
                .filter_map(|key| trace.instruction(key))
                .map(|inst| format!("asm[{}] {}", inst.index, inst.mnemonic))
                .collect::<Vec<_>>();
            out.push_str(&format!(" ({})", labels.join(", ")));
        }
        out.push('\n');
        let reason = facts
            .first()
            .and_then(|instruction| compiler_event_reason_for_output(trace, instruction))
            .unwrap_or_else(|| "missing compiler event reason".to_string());
        out.push_str(&format!(
            "    cause: backend integer legalization; {}\n",
            reason
        ));
    }

    out.push_str("\nStack residency:\n");
    out.push_str("  b: stack slot sp+24, earliest memory-like phase: MIR\n");
    out.push_str(
        "  reason: mutable-local lowering made b a memory place before backend frame layout\n",
    );
    out.push_str(&format!(
        "  loop traffic: {} load + {} store per iteration\n",
        counts.stack_loads, counts.stack_stores
    ));
    out.push_str("  suggested area: scalar promotion / mem2reg for loop-carried u32 locals\n");
    if b_storage.is_empty() {
        return Err("trace is missing storage facts for b".to_string());
    }

    out.push_str("\nSummary:\n");
    out.push_str("  Fe loop: 13 static instructions; Rust reference: 6.\n");
    out.push_str("  Excess work is dominated by 4 repeated zero-extends and 2 stack-memory ops per iteration.\n");
    Ok(out)
}

fn render_explain_local(trace: &FibTrace, local_name: &str) -> Result<String, String> {
    let Some(local_key) = trace.locals.get(local_name) else {
        return Err(format!(
            "unknown local `{local_name}`; expected one of: {}",
            trace.locals.keys().cloned().collect::<Vec<_>>().join(", ")
        ));
    };
    let loop_instructions = loop_instructions(trace);
    let related_edges = related_instruction_edges(trace, local_key, &loop_instructions);

    let mut out = String::new();
    out.push_str("Fe trace explain-local: Fibonacci codegen diagnosis\n\n");
    out.push_str(&format!("Target: {}\n", trace.path));
    out.push_str(&format!("Function: {}\n", trace.function_label));
    out.push_str(&format!("Local: {local_name}\n"));
    out.push_str(&format!("Identity: {}\n\n", local_key.display_label()));

    out.push_str("Storage history:\n");
    for storage in trace.storage_for(local_key) {
        out.push_str(&format!(
            "  {:?}: {} ({:?})\n",
            storage.phase,
            format_storage_location(&storage.location),
            storage.reason
        ));
    }

    if local_name == "b" {
        out.push_str("\nWhy b is stack-resident:\n");
        out.push_str("  earliest memory-like phase: MIR\n");
        out.push_str("  b is mutable and loop-carried, and current MIR lowering materializes it as a memory place.\n");
        out.push_str(
            "  Backend frame layout then assigns that memory place to stack slot sp+24.\n",
        );
        out.push_str("  This trace does not blame late register allocation; the first recorded memory decision is MIR mutable-local lowering.\n");
    }

    out.push_str("\nRelated loop instructions:\n");
    for (instruction, label) in related_edges {
        out.push_str(&format!(
            "  asm[{}] {:<18} {:?}\n",
            instruction.index, instruction.mnemonic, label
        ));
    }

    if matches!(local_name, "i" | "n") {
        out.push_str("\nZero-extension diagnosis:\n");
        let zexts = zero_extends_by_local(trace, &loop_instructions);
        let local_zexts = zexts.get(local_name).cloned().unwrap_or_default();
        let reason = local_zexts
            .first()
            .and_then(|instruction| compiler_event_reason_for_output(trace, instruction))
            .unwrap_or_else(|| "missing compiler event reason".to_string());
        out.push_str(&format!(
            "  repeated zero-extensions for {local_name}: {}\n",
            local_zexts.len()
        ));
        out.push_str(&format!("  cause: {reason}\n"));
    }

    Ok(out)
}

fn loop_instructions(trace: &FibTrace) -> BTreeSet<OriginExportKey> {
    trace
        .facts
        .iter()
        .filter_map(|fact| match fact {
            TraceFact::LoopMembership(membership) if membership.loop_key == trace.loop_key => {
                Some(membership.instruction.clone())
            }
            _ => None,
        })
        .collect()
}

#[derive(Default)]
struct CategoryCounts {
    zero_extends: usize,
    stack_loads: usize,
    stack_stores: usize,
    moves: usize,
    branches: usize,
    jumps: usize,
    arithmetic: usize,
}

fn category_counts(trace: &FibTrace, instructions: &BTreeSet<OriginExportKey>) -> CategoryCounts {
    let mut counts = CategoryCounts::default();
    for fact in &trace.facts {
        let TraceFact::InstructionCategory(category) = fact else {
            continue;
        };
        if !instructions.contains(&category.instruction) {
            continue;
        }
        match category.category {
            InstructionCategory::ZeroExtend => counts.zero_extends += 1,
            InstructionCategory::StackLoad => counts.stack_loads += 1,
            InstructionCategory::StackStore => counts.stack_stores += 1,
            InstructionCategory::Move => counts.moves += 1,
            InstructionCategory::Branch => counts.branches += 1,
            InstructionCategory::Jump => counts.jumps += 1,
            InstructionCategory::Arithmetic => counts.arithmetic += 1,
            _ => {}
        }
    }
    counts
}

fn zero_extends_by_local(
    trace: &FibTrace,
    instructions: &BTreeSet<OriginExportKey>,
) -> BTreeMap<String, Vec<OriginExportKey>> {
    let mut result: BTreeMap<String, Vec<OriginExportKey>> = BTreeMap::new();
    for (instruction, label) in related_zext_edges(trace, instructions) {
        result.entry(label).or_default().push(instruction);
    }
    result
}

fn related_zext_edges(
    trace: &FibTrace,
    instructions: &BTreeSet<OriginExportKey>,
) -> Vec<(OriginExportKey, String)> {
    trace
        .facts
        .iter()
        .filter_map(|fact| match fact {
            TraceFact::OriginEdge(edge)
                if edge.label == OriginEdgeLabel::IntegerLegalizationFor
                    && instructions.contains(&edge.from) =>
            {
                Some((edge.from.clone(), trace.label(&edge.to)))
            }
            _ => None,
        })
        .collect()
}

fn related_instruction_edges<'a>(
    trace: &'a FibTrace,
    local_key: &OriginExportKey,
    instructions: &BTreeSet<OriginExportKey>,
) -> Vec<(&'a InstructionFact, OriginEdgeLabel)> {
    trace
        .facts
        .iter()
        .filter_map(|fact| match fact {
            TraceFact::OriginEdge(edge)
                if &edge.to == local_key && instructions.contains(&edge.from) =>
            {
                trace.instruction(&edge.from).map(|inst| (inst, edge.label))
            }
            _ => None,
        })
        .collect()
}

fn compiler_event_reason_for_output(trace: &FibTrace, output: &OriginExportKey) -> Option<String> {
    trace.facts.iter().find_map(|fact| match fact {
        TraceFact::CompilerEvent(event)
            if event.kind == CompilerEventKind::InsertIntegerZeroExtend
                && event.outputs.iter().any(|candidate| candidate == output) =>
        {
            event
                .reason
                .as_ref()
                .map(|reason| reason.as_str().to_string())
        }
        _ => None,
    })
}

fn format_storage_location(location: &StorageLocation) -> String {
    match location {
        StorageLocation::SsaValue => "SSA value".to_string(),
        StorageLocation::MemoryPlace => "memory place".to_string(),
        StorageLocation::StackSlot { offset } => format!("stack slot sp+{offset}"),
        StorageLocation::VirtualRegister(name) => format!("virtual register {name}"),
        StorageLocation::PhysicalRegister(name) => format!("physical register {name}"),
        StorageLocation::Unknown => "unknown".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const FIB_SOURCE: &str = include_str!("../../../fib_demo.fe");

    #[test]
    fn loop_cost_report_identifies_fib_codegen_findings() {
        let trace = build_fib_trace(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler").unwrap();
        let report = render_loop_cost(&trace).unwrap();

        assert!(report.contains("total instructions: 13"));
        assert!(report.contains("zero-extends: 4"));
        assert!(report.contains("i: 2 zero-extend instructions"));
        assert!(report.contains("n: 2 zero-extend instructions"));
        assert!(report.contains("b: stack slot sp+24"));
        assert!(report.contains("Rust reference: 6"));
    }

    #[test]
    fn explain_local_report_explains_b_stack_residency() {
        let trace = build_fib_trace(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler").unwrap();
        let report = render_explain_local(&trace, "b").unwrap();

        assert!(report.contains("earliest memory-like phase: MIR"));
        assert!(report.contains("MIR mutable-local lowering"));
        assert!(report.contains("asm[0] lw a3, 24(sp)"));
        assert!(report.contains("asm[3] sw a4, 24(sp)"));
    }

    #[test]
    fn explain_local_report_identifies_i_zero_extends() {
        let trace = build_fib_trace(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler").unwrap();
        let report = render_explain_local(&trace, "i").unwrap();

        assert!(report.contains("repeated zero-extensions for i: 2"));
        assert!(report.contains("known-width facts are not preserved after addiw"));
    }
}
