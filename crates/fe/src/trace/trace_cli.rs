use crate::{DevCommand, DevTraceCommand, DevTraceQueryCommand, TraceFixtureCommand};

pub(crate) fn run_dev_command(command: &DevCommand) -> Result<String, String> {
    match command {
        DevCommand::TraceFixture { command } => run_trace_fixture_command(command),
        DevCommand::Trace { command } => run_dev_trace_command(command),
    }
}

fn run_trace_fixture_command(command: &TraceFixtureCommand) -> Result<String, String> {
    match command {
        TraceFixtureCommand::Emit(args) => super::trace_fixture::run_fixture_emit(args),
        TraceFixtureCommand::LoopCost(args) => super::trace_fixture::run_fixture_loop_cost(args),
        TraceFixtureCommand::ExplainLocal(args) => {
            super::trace_fixture::run_fixture_explain_local(args)
        }
    }
}

fn run_dev_trace_command(command: &DevTraceCommand) -> Result<String, String> {
    match command {
        DevTraceCommand::Status => Ok(
            "fe dev trace is reserved for compiler-derived validated trace JSONL.\n\
             Fixture-backed Fibonacci diagnostics remain under fe dev trace-fixture.\n\
             compiler-emitted: phase-owned MIR facts, source-local display names, MIR storage reasons, MIR lowering events, value properties, Sonatina trace-view CFG/loop facts through the Fe adapter, and actual EVM bytecode/gas facts.\n\
             coarse: source attribution may fall back to whole-file code-object spans when per-node source edges are missing.\n\
             posthoc: fixture instruction categories and demo loop membership are accepted only when metadata says fixture.\n\
             available: real Sonatina CFG loop membership when Sonatina trace-view facts are present, but not target bytecode loop membership.\n\
             deprecated: the old MIR-derived Sonatina CFG bridge is transitional/test-only and is not used by real trace emission.\n\
             unavailable: MIR-to-bytecode origin edges, backend storage allocation, target bytecode loop membership, and zext causality hooks are still incomplete.\n\
             zext-report is intentionally unavailable until InsertIntegerZeroExtend events and value properties are emitted by compiler phases.\n"
                .to_string(),
        ),
        DevTraceCommand::Emit(args) => super::trace_emit::run_trace_emit(args),
        DevTraceCommand::Validate(args) => super::trace_emit::run_trace_validate(args),
        DevTraceCommand::Query { command } => run_trace_query_command(command),
        DevTraceCommand::Live { command } => super::trace_live::run_trace_live_command(command),
        DevTraceCommand::LoopCost(args) => super::trace_emit::run_trace_loop_cost(args),
        DevTraceCommand::LoopContents(args) => super::trace_emit::run_trace_loop_contents(args),
        DevTraceCommand::ExplainLocal(args) => super::trace_emit::run_trace_explain_local(args),
    }
}

fn run_trace_query_command(command: &DevTraceQueryCommand) -> Result<String, String> {
    match command {
        DevTraceQueryCommand::LoopCost(args) => super::trace_emit::run_trace_loop_cost(args),
        DevTraceQueryCommand::LoopContents(args) => {
            super::trace_emit::run_trace_loop_contents(args)
        }
        DevTraceQueryCommand::ExplainLocal(args) => {
            super::trace_emit::run_trace_explain_local(args)
        }
        DevTraceQueryCommand::GasBreakdown(args) => {
            super::trace_emit::run_trace_gas_breakdown(args)
        }
        DevTraceQueryCommand::ExplainPc(args) => super::trace_emit::run_trace_explain_pc(args),
        DevTraceQueryCommand::GasBySource(args) => super::trace_emit::run_trace_gas_by_source(args),
        DevTraceQueryCommand::BytecodeSizeBySource(args) => {
            super::trace_emit::run_trace_bytecode_size_by_source(args)
        }
        DevTraceQueryCommand::DynamicGasBySource(args) => {
            super::trace_emit::run_trace_dynamic_gas_by_source(args)
        }
        DevTraceQueryCommand::GasToSource(args) => super::trace_emit::run_trace_gas_to_source(args),
        DevTraceQueryCommand::OptimizedCodeHonesty(args) => {
            super::trace_emit::run_trace_optimized_code_honesty(args)
        }
        DevTraceQueryCommand::VariablesAtPc(args) => {
            super::trace_emit::run_trace_variables_at_pc(args)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn status_keeps_zext_report_gated_on_compiler_facts() {
        let output = run_dev_trace_command(&DevTraceCommand::Status).unwrap();

        assert!(output.contains("Fixture-backed Fibonacci diagnostics"));
        assert!(output.contains("compiler-emitted: phase-owned MIR facts"));
        assert!(output.contains("coarse: source attribution"));
        assert!(output.contains("posthoc: fixture instruction categories"));
        assert!(output.contains("available: real Sonatina CFG loop membership"));
        assert!(output.contains("MIR-derived Sonatina CFG bridge is transitional/test-only"));
        assert!(output.contains("target bytecode loop membership"));
        assert!(output.contains("zext-report is intentionally unavailable"));
        assert!(output.contains("InsertIntegerZeroExtend events"));
    }
}
