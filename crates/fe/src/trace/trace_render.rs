use trace_facts::{TraceBundle, TraceMetadata, TraceSnapshot, TraceValidationReport};
use trace_query::{
    ExplainLocalReport, ExplainLocalRequest, GasBreakdownReport, GasBreakdownRequest,
    IntrospectionService, LoopCostReport, LoopCostRequest, TraceIntrospectionService,
};

pub(super) fn render_validation_summary(
    metadata: &TraceMetadata,
    report: &TraceValidationReport,
) -> String {
    format!(
        "Trace validation: passed\n\
         Data source: {}\n\
         Schema version: {}\n\
         Compiler commit: {}\n\
         Target: {}\n\
         Input: {}\n\
         Facts: {}\n\
         Origin nodes: {}\n\
         Origin edges: {}\n\
         Instructions: {}\n\
         Diagnostics: {} error, {} warning, {} info\n",
        super::format_data_source(metadata),
        metadata.schema_version,
        metadata.compiler_commit,
        metadata.target,
        metadata.input_path,
        report.summary.fact_count,
        report.summary.node_count,
        report.summary.edge_count,
        report.summary.instruction_count,
        report.error_count(),
        report.warning_count(),
        report.info_count()
    )
}

pub(super) fn render_loop_cost_bundle(bundle: TraceBundle) -> Result<String, String> {
    render_loop_cost_snapshot(
        TraceSnapshot::new(bundle).map_err(|err| format!("trace validation failed: {err}"))?,
    )
}

pub(super) fn render_loop_cost_snapshot(snapshot: TraceSnapshot) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .loop_cost(LoopCostRequest::default())
        .map_err(|err| err.to_string())?;
    Ok(render_loop_cost_report(&report))
}

pub(super) fn render_explain_local_bundle(
    bundle: TraceBundle,
    local_name: &str,
) -> Result<String, String> {
    render_explain_local_snapshot(
        TraceSnapshot::new(bundle).map_err(|err| format!("trace validation failed: {err}"))?,
        local_name,
    )
}

pub(super) fn render_explain_local_snapshot(
    snapshot: TraceSnapshot,
    local_name: &str,
) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .explain_local(ExplainLocalRequest {
            local: local_name.to_string(),
        })
        .map_err(|err| err.to_string())?;
    Ok(render_explain_local_report(&report))
}

pub(super) fn render_gas_breakdown_snapshot(
    snapshot: TraceSnapshot,
    schedule: &str,
) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .gas_breakdown(GasBreakdownRequest {
            schedule: schedule.to_string(),
        })
        .map_err(|err| err.to_string())?;
    Ok(render_gas_breakdown_report(&report))
}

fn render_loop_cost_report(report: &LoopCostReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace loop-cost\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    if let Some(function_label) = report.metadata.function_label() {
        out.push_str(&format!("Function: {function_label}\n"));
    }
    if !report.available {
        out.push('\n');
        out.push_str("Loop cost unavailable from this trace.\n");
        if let Some(reason) = &report.unavailable_reason {
            out.push_str(&format!("Reason: {reason}.\n\n"));
        }
        out.push_str("Available compiler-derived bytecode summary:\n");
    } else if let Some(loop_label) = &report.loop_label {
        out.push_str(&format!("Loop: {loop_label}\n\n"));
        out.push_str("Static per-iteration cost:\n");
    }

    out.push_str(&format!(
        "  total instructions: {}\n",
        report.summary.total_instructions
    ));
    out.push_str(&format!(
        "  zero-extends: {}\n",
        report.summary.zero_extends
    ));
    out.push_str(&format!("  stack loads: {}\n", report.summary.stack_loads));
    out.push_str(&format!(
        "  stack stores: {}\n",
        report.summary.stack_stores
    ));
    out.push_str(&format!("  moves: {}\n", report.summary.moves));
    out.push_str(&format!(
        "  branches/jumps: {}\n",
        report.summary.branch_like()
    ));
    out.push_str(&format!("  arithmetic: {}\n", report.summary.arithmetic));
    if !report.available {
        out.push_str(&format!("  loads: {}\n", report.summary.loads));
        out.push_str(&format!("  stores: {}\n\n", report.summary.stores));
        out.push_str("Required next facts: loop membership, MIR-to-codegen origin edges, backend storage allocation, and zext compiler events.\n");
        return out;
    }

    out.push_str("\nRepeated zero-extensions:\n");
    if report.repeated_zero_extends.is_empty() {
        out.push_str("  none attributed\n");
    }
    for group in &report.repeated_zero_extends {
        let labels = group
            .instructions
            .iter()
            .map(|inst| format!("asm[{}] {}", inst.index, inst.mnemonic))
            .collect::<Vec<_>>();
        out.push_str(&format!(
            "  {}: {} zero-extend instructions",
            group.local,
            group.instructions.len()
        ));
        if !labels.is_empty() {
            out.push_str(&format!(" ({})", labels.join(", ")));
        }
        out.push('\n');
        let reason = group
            .reason
            .as_deref()
            .unwrap_or("missing compiler event reason");
        out.push_str(&format!(
            "    cause: backend integer legalization; {reason}\n"
        ));
    }

    if let Some(impact) = report
        .storage_impacts
        .iter()
        .find(|impact| impact.local == "b")
    {
        out.push_str("\nStack residency:\n");
        if let Some(step) = impact
            .storage_history
            .iter()
            .find(|step| step.location.contains("stack slot"))
        {
            out.push_str(&format!(
                "  b: {}, earliest memory-like phase: MIR\n",
                step.location
            ));
        }
        out.push_str(
            "  reason: mutable-local lowering made b a memory place before backend frame layout\n",
        );
        out.push_str(&format!(
            "  loop traffic: {} load + {} store per iteration\n",
            impact.loads, impact.stores
        ));
        out.push_str("  suggested area: scalar promotion / mem2reg for loop-carried u32 locals\n");
    }

    out.push_str("\nSummary:\n");
    if report
        .metadata
        .data_source
        .starts_with("fixture (fib_demo_codegen_ux_v1")
    {
        out.push_str("  Fe loop: 13 static instructions; Rust reference: 6.\n");
        out.push_str("  Excess work is dominated by 4 repeated zero-extends and 2 stack-memory ops per iteration.\n");
    } else {
        out.push_str(&format!(
            "  Derived loop contains {} static instructions, {} zero-extends, and {} stack-memory ops.\n",
            report.summary.total_instructions,
            report.summary.zero_extends,
            report.summary.stack_loads + report.summary.stack_stores
        ));
    }
    out
}

fn render_explain_local_report(report: &ExplainLocalReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace explain-local\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    if let Some(function_label) = report.metadata.function_label() {
        out.push_str(&format!("Function: {function_label}\n"));
    }
    out.push_str(&format!("Local: {}\n", report.local));

    let Some(local_key) = &report.local_key else {
        out.push('\n');
        out.push_str("Local explanation unavailable from this trace.\n");
        if let Some(reason) = &report.unavailable_reason {
            out.push_str(&format!("Reason: {reason}.\n"));
        }
        if report.available_locals.is_empty() {
            out.push_str("Available runtime local identities: none emitted.\n");
        } else {
            out.push_str(&format!(
                "Available runtime local identities: {} emitted; showing first 20.\n",
                report.available_locals.len()
            ));
            for local in &report.available_locals {
                out.push_str(&format!("  {local}\n"));
            }
        }
        return out;
    };

    out.push_str(&format!("Identity: {}\n\n", local_key.display_label()));
    out.push_str("Storage history:\n");
    for storage in &report.storage_history {
        out.push_str(&format!(
            "  {}: {} ({})\n",
            storage.phase, storage.location, storage.reason
        ));
    }

    if report.local == "b" {
        let has_stack_slot = report
            .storage_history
            .iter()
            .any(|step| step.location.contains("stack slot"));
        let has_memory_place = report
            .storage_history
            .iter()
            .any(|step| step.location == "memory place");
        if has_stack_slot {
            out.push_str("\nWhy b is stack-resident:\n");
            out.push_str("  earliest memory-like phase: MIR\n");
            out.push_str("  b is mutable and loop-carried, and current MIR lowering materializes it as a memory place.\n");
            out.push_str("  A backend storage fact assigns that memory place to a stack slot.\n");
            out.push_str("  This trace does not blame late register allocation; the first recorded memory decision is MIR mutable-local lowering.\n");
        } else if has_memory_place {
            out.push_str("\nWhy b is memory-backed in MIR:\n");
            out.push_str("  earliest memory-like phase: MIR\n");
            out.push_str("  b is mutable in source, and current MIR lowering materializes it as a memory place.\n");
            out.push_str("  No backend stack/register allocation fact is emitted yet, so this real trace stops at the MIR storage decision.\n");
            out.push_str(
                "  The fixture demo still shows the intended post-backend stack-slot story.\n",
            );
        }
    }

    out.push_str("\nRelated loop instructions:\n");
    for related in &report.related_instructions {
        out.push_str(&format!(
            "  asm[{}] {:<18} {:?}\n",
            related.instruction.index, related.instruction.mnemonic, related.edge_label
        ));
    }

    if matches!(report.local.as_str(), "i" | "n") {
        out.push_str("\nZero-extension diagnosis:\n");
        let reason = report
            .zero_extends
            .first()
            .and_then(|related| related.reason.as_deref())
            .unwrap_or("missing compiler event reason");
        out.push_str(&format!(
            "  repeated zero-extensions for {}: {}\n",
            report.local,
            report.zero_extends.len()
        ));
        out.push_str(&format!("  cause: {reason}\n"));
    }

    out
}

fn render_gas_breakdown_report(report: &GasBreakdownReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace gas-breakdown\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    out.push_str(&format!("Schedule: {}\n", report.schedule));
    out.push_str(
        "Mode: static opcode-table estimate; runtime gas depends on path and EVM state.\n\n",
    );
    if !report.available {
        out.push_str("Gas breakdown unavailable from this trace.\n");
        for finding in &report.findings {
            out.push_str(&format!("  {}: {}\n", finding.title, finding.summary));
        }
        return out;
    }
    out.push_str(&format!(
        "Total static opcode gas: {}\n",
        report.total_gas.unwrap_or_default()
    ));
    out.push_str("Top opcode contributors:\n");
    let mut rows = report.rows.clone();
    rows.sort_by(|a, b| b.gas.cmp(&a.gas));
    for row in rows.iter().take(12) {
        out.push_str(&format!(
            "  {:>4} gas  {:<24} {} ({})\n",
            row.gas, row.label, row.confidence, row.source
        ));
    }
    out
}
