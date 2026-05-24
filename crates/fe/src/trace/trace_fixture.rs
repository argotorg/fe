use std::collections::BTreeMap;
use std::fs;

use camino::Utf8PathBuf;
use common::origin::OriginExportKey;
use trace_facts::{
    CategorySource, CompilerEventFact, CompilerEventKind, CompilerPhase, CompilerReason,
    InstructionCategory, InstructionCategoryFact, InstructionFact, LoopDerivation,
    LoopMembershipFact, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, OriginNodeKind,
    StorageFact, StorageLocation, StorageReason, TraceBundle, TraceFact, TraceMetadata,
    TraceValidator,
};

use crate::{TraceFixtureEmitArgs, TraceFixtureExplainLocalArgs, TraceFixtureLoopCostArgs};

const FIB_OWNER: &str = "fixture:fib_demo";

pub(super) fn run_fixture_emit(args: &TraceFixtureEmitArgs) -> Result<String, String> {
    let bundle = build_fib_fixture_bundle_from_path(&args.path, &args.function)?;
    super::trace_emit::write_trace_bundle_jsonl(&args.out, &bundle)?;
    Ok(format!(
        "wrote fixture trace JSONL: {}\nData source: {}\nFacts: {}\n",
        args.out,
        super::format_data_source(&bundle.metadata),
        bundle.facts.len()
    ))
}

pub(super) fn run_fixture_loop_cost(args: &TraceFixtureLoopCostArgs) -> Result<String, String> {
    let bundle = build_and_roundtrip_fib_fixture_bundle(&args.path, &args.function)?;
    super::trace_render::render_loop_cost_bundle(bundle)
}

pub(super) fn run_fixture_explain_local(
    args: &TraceFixtureExplainLocalArgs,
) -> Result<String, String> {
    let bundle = build_and_roundtrip_fib_fixture_bundle(&args.path, &args.function)?;
    super::trace_render::render_explain_local_bundle(bundle, &args.local)
}

fn build_fib_fixture_bundle_from_path(
    path: &Utf8PathBuf,
    function_label: &str,
) -> Result<TraceBundle, String> {
    let source = fs::read_to_string(path.as_std_path())
        .map_err(|err| format!("failed to read {path}: {err}"))?;
    build_fib_fixture_bundle(&source, path.as_str(), function_label)
}

fn build_and_roundtrip_fib_fixture_bundle(
    path: &Utf8PathBuf,
    function_label: &str,
) -> Result<TraceBundle, String> {
    let bundle = build_fib_fixture_bundle_from_path(path, function_label)?;
    super::trace_emit::roundtrip_trace_bundle_jsonl(&bundle)
}

fn build_fib_fixture_bundle(
    source: &str,
    path_label: &str,
    function_label: &str,
) -> Result<TraceBundle, String> {
    require_fib_fixture(source)?;

    let function = key("function", FIB_OWNER, "contract:Fib.recv:Compute");
    let loop_key = key("loop", FIB_OWNER, "while:i<n");
    let mut facts = Vec::new();

    push_node(&mut facts, function.clone(), "function");
    push_node(&mut facts, loop_key.clone(), "loop");

    let locals = [
        ("n", "local:n"),
        ("a", "local:a"),
        ("b", "local:b"),
        ("i", "local:i"),
        ("next", "local:next"),
    ]
    .into_iter()
    .map(|(name, local)| {
        let key = key("runtime.local", FIB_OWNER, local);
        push_node(&mut facts, key.clone(), "runtime.local");
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
        push_node(&mut facts, instruction.clone(), "asm.inst");
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
                push_node(&mut facts, event.clone(), "compiler.event");
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
    let metadata = TraceMetadata::fixture(
        super::compiler_commit(),
        "riscv64-fib-demo",
        vec![
            "fe".to_string(),
            "dev".to_string(),
            "trace-fixture".to_string(),
            "emit".to_string(),
        ],
        path_label,
        vec![format!("function={function_label}")],
        "fib_demo_codegen_ux_v1",
    );
    Ok(TraceBundle::new(metadata, facts))
}

fn push_node(facts: &mut Vec<TraceFact>, key: OriginExportKey, kind: &str) {
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

#[cfg(test)]
mod tests {
    use super::*;

    const FIB_SOURCE: &str = include_str!("../../../../fib_demo.fe");

    #[test]
    fn loop_cost_report_identifies_fib_codegen_findings() {
        let bundle =
            build_fib_fixture_bundle(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler")
                .unwrap();
        let report = super::super::trace_render::render_loop_cost_bundle(
            super::super::trace_emit::roundtrip_trace_bundle_jsonl(&bundle).unwrap(),
        )
        .unwrap();

        assert!(
            report.contains("Data source: fixture (fib_demo_codegen_ux_v1; not compiler-derived)")
        );
        assert!(!report.contains("Data source: compiler_emitted"));
        assert!(report.contains("Static per-iteration cost"));
        assert!(report.contains("total instructions: 13"));
        assert!(report.contains("zero-extends: 4"));
        assert!(report.contains("i: 2 zero-extend instructions"));
        assert!(report.contains("n: 2 zero-extend instructions"));
        assert!(report.contains("b: stack slot sp+24"));
        assert!(report.contains("Rust reference: 6"));
    }

    #[test]
    fn explain_local_report_explains_b_stack_residency() {
        let bundle =
            build_fib_fixture_bundle(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler")
                .unwrap();
        let report = super::super::trace_render::render_explain_local_bundle(
            super::super::trace_emit::roundtrip_trace_bundle_jsonl(&bundle).unwrap(),
            "b",
        )
        .unwrap();

        assert!(report.contains("earliest memory-like phase: MIR"));
        assert!(report.contains("MIR mutable-local lowering"));
        assert!(report.contains("asm[0] lw a3, 24(sp)"));
        assert!(report.contains("asm[3] sw a4, 24(sp)"));
    }

    #[test]
    fn explain_local_report_identifies_i_zero_extends() {
        let bundle =
            build_fib_fixture_bundle(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler")
                .unwrap();
        let report = super::super::trace_render::render_explain_local_bundle(
            super::super::trace_emit::roundtrip_trace_bundle_jsonl(&bundle).unwrap(),
            "i",
        )
        .unwrap();

        assert!(report.contains("repeated zero-extensions for i: 2"));
        assert!(report.contains("known-width facts are not preserved after addiw"));
    }

    #[test]
    fn fixture_trace_emits_jsonl_bundle_before_reports() {
        let bundle =
            build_fib_fixture_bundle(FIB_SOURCE, "fib_demo.fe", "Fib.recv Compute handler")
                .unwrap();
        let roundtripped = super::super::trace_emit::roundtrip_trace_bundle_jsonl(&bundle).unwrap();

        assert_eq!(
            roundtripped.metadata.fixture_marker.as_deref(),
            Some("fib_demo_codegen_ux_v1")
        );
        assert_eq!(
            roundtripped.metadata.data_source,
            trace_facts::TraceDataSource::Fixture
        );
        assert!(TraceValidator::validate(&roundtripped.facts).is_ok());
    }
}
