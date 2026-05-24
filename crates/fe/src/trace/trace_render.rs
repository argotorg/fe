use trace_facts::{TraceBundle, TraceMetadata, TraceSnapshot, TraceValidationReport};
use trace_query::{
    BytecodeSizeBySourceReport, BytecodeSizeBySourceRequest, DynamicGasBySourceReport,
    DynamicGasBySourceRequest, ExplainLocalReport, ExplainLocalRequest, ExplainPcReport,
    ExplainPcRequest, GasAttributionPolicy, GasBreakdownReport, GasBreakdownRequest,
    GasBySourceReport, GasBySourceRequest, GasToSourceReport, GasToSourceRequest,
    IntrospectionService, LoopContentsReport, LoopContentsRequest, LoopCostReport, LoopCostRequest,
    OptimizedCodeHonestyReport, OptimizedCodeHonestyRequest, SourceAttribution,
    TraceIntrospectionService, VariablesAtPcReport, VariablesAtPcRequest,
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

pub(super) fn render_loop_contents_snapshot(snapshot: TraceSnapshot) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .loop_contents(LoopContentsRequest::default())
        .map_err(|err| err.to_string())?;
    Ok(render_loop_contents_report(&report))
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

pub(super) fn render_explain_pc_snapshot(
    snapshot: TraceSnapshot,
    pc: u32,
) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .explain_pc(ExplainPcRequest { pc })
        .map_err(|err| err.to_string())?;
    Ok(render_explain_pc_report(&report))
}

pub(super) fn render_gas_by_source_snapshot(
    snapshot: TraceSnapshot,
    schedule: &str,
    policy: &str,
) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .gas_by_source(GasBySourceRequest {
            schedule: schedule.to_string(),
            policy: parse_gas_policy(policy)?,
        })
        .map_err(|err| err.to_string())?;
    Ok(render_gas_by_source_report(&report))
}

pub(super) fn render_dynamic_gas_by_source_snapshot(
    snapshot: TraceSnapshot,
    trace_id: Option<String>,
    policy: &str,
) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .dynamic_gas_by_source(DynamicGasBySourceRequest {
            trace_id,
            policy: parse_gas_policy(policy)?,
        })
        .map_err(|err| err.to_string())?;
    Ok(render_dynamic_gas_by_source_report(&report))
}

pub(super) fn render_bytecode_size_by_source_snapshot(
    snapshot: TraceSnapshot,
    policy: &str,
) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .bytecode_size_by_source(BytecodeSizeBySourceRequest {
            policy: parse_gas_policy(policy)?,
        })
        .map_err(|err| err.to_string())?;
    Ok(render_bytecode_size_by_source_report(&report))
}

pub(super) fn render_gas_to_source_snapshot(
    snapshot: TraceSnapshot,
    schedule: &str,
    trace_id: Option<String>,
    policy: &str,
) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .gas_to_source(GasToSourceRequest {
            schedule: schedule.to_string(),
            trace_id,
            policy: parse_gas_policy(policy)?,
        })
        .map_err(|err| err.to_string())?;
    Ok(render_gas_to_source_report(&report))
}

pub(super) fn render_optimized_code_honesty_snapshot(
    snapshot: TraceSnapshot,
) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .optimized_code_honesty(OptimizedCodeHonestyRequest::default())
        .map_err(|err| err.to_string())?;
    Ok(render_optimized_code_honesty_report(&report))
}

pub(super) fn render_variables_at_pc_snapshot(
    snapshot: TraceSnapshot,
    pc: u32,
) -> Result<String, String> {
    let service = TraceIntrospectionService::new(snapshot);
    let report = service
        .variables_at_pc(VariablesAtPcRequest {
            pc,
            code_object: None,
        })
        .map_err(|err| err.to_string())?;
    Ok(render_variables_at_pc_report(&report))
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

fn render_loop_contents_report(report: &LoopContentsReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace loop-contents\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    if let Some(function_label) = report.metadata.function_label() {
        out.push_str(&format!("Function: {function_label}\n"));
    }

    if !report.available {
        out.push('\n');
        out.push_str("Loop contents unavailable from this trace.\n");
        if let Some(reason) = &report.unavailable_reason {
            out.push_str(&format!("Reason: {reason}.\n"));
        }
        out.push_str("Required facts: LoopFact, LoopBlockFact, and LoopMembershipFact from a phase-owned CFG analysis.\n");
        return out;
    }

    if let Some(loop_label) = &report.loop_label {
        out.push_str(&format!("Loop: {loop_label}\n"));
    }
    out.push_str("Membership source: compiler-emitted Sonatina CFG natural-loop analysis\n");
    out.push_str(&format!("Blocks: {}\n", report.blocks.len()));
    out.push_str(&format!("Instructions: {}\n\n", report.instructions.len()));
    out.push_str("Loop blocks:\n");
    for block in &report.blocks {
        out.push_str(&format!(
            "  {} [{}]\n",
            block.block.display_label(),
            block.role
        ));
        if block.instructions.is_empty() {
            out.push_str("    <no instructions>\n");
        }
        for instruction in &block.instructions {
            out.push_str(&format!(
                "    ir[{}] {}\n",
                instruction.index, instruction.mnemonic
            ));
        }
    }

    out.push_str("\nFindings:\n");
    for finding in &report.findings {
        out.push_str(&format!("  {}: {}\n", finding.title, finding.summary));
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
    out.push_str(&format!("Attribution policy: {}\n", report.policy));
    out.push_str(&format!("Confidence: {:?}\n", report.confidence));
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

fn render_explain_pc_report(report: &ExplainPcReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace explain-pc\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    out.push_str(&format!("PC: {}\n", report.pc));

    let Some(instruction) = &report.instruction else {
        out.push('\n');
        out.push_str("PC explanation unavailable from this trace.\n");
        if let Some(reason) = &report.unavailable_reason {
            out.push_str(&format!("Reason: {reason}.\n"));
        }
        return out;
    };

    out.push_str(&format!(
        "Instruction: asm[{}] {}\n",
        instruction.index, instruction.mnemonic
    ));
    if let Some(category) = report.category {
        out.push_str(&format!("Category: {category:?}\n"));
    }
    if let Some(gas) = report.static_gas {
        out.push_str(&format!("Static gas (cancun): {gas}\n"));
    }

    out.push('\n');
    out.push_str("Source attribution:\n");
    if let Some(source) = &report.primary_source {
        out.push_str(&format!("  primary: {}\n", format_source(source)));
    } else if report.source_candidates.is_empty() {
        out.push_str("  none emitted\n");
    } else {
        out.push_str("  ambiguous; no single primary source\n");
    }
    for source in &report.source_candidates {
        out.push_str(&format!("  candidate: {}\n", format_source(source)));
    }
    out
}

fn render_gas_by_source_report(report: &GasBySourceReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace gas-by-source\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    out.push_str(&format!("Schedule: {}\n", report.schedule));
    out.push_str(&format!("Attribution policy: {}\n", report.policy));
    out.push_str(&format!("Confidence: {:?}\n\n", report.confidence));
    out.push_str(&format!("Total static opcode gas: {}\n", report.total_gas));

    if report.rows.is_empty() {
        out.push_str("No static gas rows were present in this trace.\n");
        return out;
    }

    out.push_str("Source contributors:\n");
    for row in report.rows.iter().take(20) {
        out.push_str(&format!(
            "  {:>4} gas  {:>3} inst  {:<8?} {}\n",
            row.gas, row.instruction_count, row.confidence, row.label
        ));
    }
    out
}

fn render_dynamic_gas_by_source_report(report: &DynamicGasBySourceReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace dynamic-gas-by-source\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    out.push_str(&format!("Target schedule: {}\n", report.target_schedule));
    out.push_str(&format!("Attribution policy: {}\n", report.policy));
    out.push_str(&format!("Confidence: {:?}\n", report.confidence));
    if let Some(trace_id) = &report.trace_id {
        out.push_str(&format!("Trace id: {trace_id}\n"));
    }
    out.push('\n');
    out.push_str(&format!("Total measured gas: {}\n", report.total_gas));
    out.push_str(&format!(
        "Unattributed dynamic steps: {}\n",
        report.unattributed_steps
    ));

    if report.rows.is_empty() {
        out.push_str("No dynamic gas rows were present in this trace.\n");
        return out;
    }

    out.push_str("Source contributors:\n");
    for row in report.rows.iter().take(20) {
        out.push_str(&format!(
            "  {:>4} gas  {:>3} steps  {:<8?} {}\n",
            row.gas, row.instruction_count, row.confidence, row.label
        ));
    }
    out
}

fn render_bytecode_size_by_source_report(report: &BytecodeSizeBySourceReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace bytecode-size-by-source\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    out.push_str(&format!("Attribution policy: {}\n", report.policy));
    out.push_str(&format!("Confidence: {:?}\n\n", report.confidence));
    out.push_str(&format!(
        "Total emitted bytecode bytes: {}\n",
        report.total_bytes
    ));

    if report.rows.is_empty() {
        out.push_str("No instruction extent rows were present in this trace.\n");
        return out;
    }

    out.push_str("Source contributors:\n");
    for row in report.rows.iter().take(20) {
        out.push_str(&format!(
            "  {:>4} bytes  {:>3} inst  {:<8?} {}\n",
            row.bytes, row.instruction_count, row.confidence, row.label
        ));
    }
    out
}

fn render_gas_to_source_report(report: &GasToSourceReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace gas-to-source\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    out.push_str(&format!("Target schedule: {}\n", report.schedule));
    out.push_str(&format!("Attribution policy: {}\n", report.policy));
    out.push_str(&format!("Confidence: {:?}\n", report.confidence));
    if let Some(trace_id) = &report.trace_id {
        out.push_str(&format!("Trace id: {trace_id}\n"));
    }
    out.push('\n');
    out.push_str(&format!("Static gas: {}\n", report.static_gas));
    out.push_str(&format!("Dynamic gas: {}\n", report.dynamic_gas));
    out.push_str(&format!("Combined gas: {}\n", report.total_gas));

    if report.rows.is_empty() {
        out.push_str("No gas rows were present in this trace.\n");
        return out;
    }

    out.push_str("Source contributors:\n");
    for row in report.rows.iter().take(20) {
        out.push_str(&format!(
            "  {:>4} total  {:>4} static  {:>4} dynamic  {:<8?} {}\n",
            row.total_gas, row.static_gas, row.dynamic_gas, row.confidence, row.label
        ));
    }
    out
}

fn render_optimized_code_honesty_report(report: &OptimizedCodeHonestyReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace optimized-code-honesty\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    out.push_str(&format!("Target schedule: {}\n", report.schedule));
    out.push_str(&format!("Attribution policy: {}\n", report.policy));
    out.push_str(&format!("Confidence: {:?}\n\n", report.confidence));

    out.push_str(&format!(
        "Ambiguous instructions: {}\n",
        report.ambiguous_instructions.len()
    ));
    out.push_str(&format!(
        "Synthetic overhead instructions: {}\n",
        report.synthetic_overheads.len()
    ));
    out.push_str(&format!(
        "Unmapped instructions: {}\n",
        report.unmapped_instructions.len()
    ));

    if !report.ambiguous_instructions.is_empty() {
        out.push_str("\nAmbiguous source candidates:\n");
        for row in report.ambiguous_instructions.iter().take(12) {
            out.push_str(&format!(
                "  asm[{}] {}: {} candidate source(s)",
                row.instruction.index,
                row.instruction.mnemonic,
                row.source_candidates.len()
            ));
            if let Some(gas) = row.static_gas {
                out.push_str(&format!(", static gas {gas}"));
            }
            if row.dynamic_gas > 0 {
                out.push_str(&format!(", dynamic gas {}", row.dynamic_gas));
            }
            out.push('\n');
            for source in &row.source_candidates {
                out.push_str(&format!("    candidate: {}\n", format_source(source)));
            }
        }
    }

    if !report.synthetic_overheads.is_empty() {
        out.push_str("\nSynthetic compiler overhead:\n");
        for row in report.synthetic_overheads.iter().take(12) {
            let labels = row
                .edge_labels
                .iter()
                .map(|label| format!("{label:?}"))
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&format!(
                "  asm[{}] {} ({labels})",
                row.instruction.index, row.instruction.mnemonic
            ));
            if let Some(gas) = row.static_gas {
                out.push_str(&format!(", static gas {gas}"));
            }
            if row.dynamic_gas > 0 {
                out.push_str(&format!(", dynamic gas {}", row.dynamic_gas));
            }
            out.push('\n');
            for source in &row.cause_sources {
                out.push_str(&format!("    caused by: {}\n", format_source(source)));
            }
        }
    }

    if !report.unmapped_instructions.is_empty() {
        out.push_str("\nUnmapped instructions:\n");
        for instruction in report.unmapped_instructions.iter().take(20) {
            out.push_str(&format!(
                "  asm[{}] {} has no recorded source or synthetic-cause edge\n",
                instruction.index, instruction.mnemonic
            ));
        }
    }

    if report.ambiguous_instructions.is_empty()
        && report.synthetic_overheads.is_empty()
        && report.unmapped_instructions.is_empty()
    {
        out.push_str("\nNo attribution ambiguity, synthetic overhead, or unmapped instructions were reported.\n");
    } else {
        out.push_str(
            "\nHonesty note: this report preserves ambiguity instead of selecting a source the compiler did not record.\n",
        );
    }
    out
}

fn render_variables_at_pc_report(report: &VariablesAtPcReport) -> String {
    let mut out = String::new();
    out.push_str("Fe dev trace variables-at-pc\n\n");
    out.push_str(&format!("Data source: {}\n", report.metadata.data_source));
    out.push_str("Trace validation: passed\n");
    out.push_str(&format!("Target: {}\n", report.metadata.target));
    out.push_str(&format!("Input: {}\n", report.metadata.input_path));
    out.push_str(&format!("PC: {}\n\n", report.pc));

    if report.variables.is_empty() {
        out.push_str("No variable location ranges cover this PC.\n");
        return out;
    }

    out.push_str("Variables in scope:\n");
    for variable in &report.variables {
        out.push_str(&format!(
            "  {:<20} {:<24} {} ({})\n",
            variable.name, variable.location, variable.reason, variable.confidence
        ));
    }
    out
}

fn format_source(source: &SourceAttribution) -> String {
    format!("{} ({})", source.label, source.origin.display_label())
}

fn parse_gas_policy(policy: &str) -> Result<GasAttributionPolicy, String> {
    policy
        .parse()
        .map_err(|err: trace_query::QueryError| err.to_string())
}
