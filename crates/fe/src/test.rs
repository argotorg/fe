//! Test runner for Fe tests.
//!
//! Discovers functions marked with `#[test]` attribute, compiles them, and
//! executes them using revm.

use crate::report::{
    PanicReportGuard, ReportStaging, copy_input_into_report, create_dir_all_utf8,
    create_report_staging_root, enable_panic_report, is_verifier_error_text,
    normalize_report_out_path, panic_payload_to_string, sanitize_filename, tar_gz_dir,
    write_report_meta,
};
use camino::Utf8PathBuf;
use codegen::{
    DebugOutputSink, ExpectedRevert, OptLevel, SonatinaTestDebugConfig, TestMetadata,
    TestModuleOutput, emit_test_module_sonatina, emit_test_module_yul,
};
use colored::Colorize;
use common::InputDb;
use contract_harness::{CallGasProfile, EvmTraceOptions, ExecutionOptions, RuntimeInstance};
use driver::DriverDataBase;
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::{fmt as mir_fmt, lower_module};
use rustc_hash::{FxHashMap, FxHashSet};
use serde_json::Value;
use solc_runner::compile_single_contract;
use std::{
    fmt::Write as _,
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc,
    },
};
use url::Url;

const YUL_VERIFY_RUNTIME: bool = true;

fn install_report_panic_hook(report: &ReportContext, filename: &str) -> PanicReportGuard {
    let dir = report.root_dir.join("errors");
    let _ = create_dir_all_utf8(&dir);
    let path = dir.join(filename);
    enable_panic_report(path)
}

/// Result of running a single test.
#[derive(Debug, Clone)]
pub struct TestResult {
    pub name: String,
    pub passed: bool,
    pub error_message: Option<String>,
    /// Runtime test-call gas (the empty-calldata call into the deployed test object).
    pub gas_used: Option<u64>,
    /// Gas used by the deployment transaction that instantiates the test object.
    pub deploy_gas_used: Option<u64>,
    /// Combined deployment + runtime-call gas, when both are available.
    pub total_gas_used: Option<u64>,
}

#[derive(Debug)]
struct TestOutcome {
    result: TestResult,
    logs: Vec<String>,
    trace: Option<contract_harness::CallTrace>,
    step_count: Option<u64>,
    runtime_metrics: Option<EvmRuntimeMetrics>,
    gas_profile: Option<CallGasProfile>,
}

#[derive(Debug, Clone)]
struct GasComparisonCase {
    display_name: String,
    symbol_name: String,
    yul: Option<TestMetadata>,
    sonatina: Option<TestMetadata>,
}

#[derive(Debug, Clone)]
struct GasMeasurement {
    gas_used: Option<u64>,
    deploy_gas_used: Option<u64>,
    total_gas_used: Option<u64>,
    step_count: Option<u64>,
    runtime_metrics: Option<EvmRuntimeMetrics>,
    gas_profile: Option<CallGasProfile>,
    passed: bool,
    error_message: Option<String>,
}

#[derive(Debug, Clone, Copy, Default)]
struct EvmRuntimeMetrics {
    byte_len: usize,
    op_count: usize,
    push_ops: usize,
    dup_ops: usize,
    swap_ops: usize,
    pop_ops: usize,
    jump_ops: usize,
    jumpi_ops: usize,
    jumpdest_ops: usize,
    iszero_ops: usize,
    mload_ops: usize,
    mstore_ops: usize,
    sload_ops: usize,
    sstore_ops: usize,
    keccak_ops: usize,
    call_ops: usize,
    staticcall_ops: usize,
    returndatacopy_ops: usize,
    calldatacopy_ops: usize,
    mcopy_ops: usize,
    return_ops: usize,
    revert_ops: usize,
}

#[derive(Debug, Clone, Copy, Default)]
struct ComparisonTotals {
    compared_with_gas: usize,
    sonatina_lower: usize,
    sonatina_higher: usize,
    equal: usize,
    incomplete: usize,
}

#[derive(Debug, Clone, Copy, Default)]
struct GasTotals {
    tests_in_scope: usize,
    vs_yul_unopt: ComparisonTotals,
    vs_yul_opt: ComparisonTotals,
}

#[derive(Debug, Clone, Copy, Default)]
struct DeltaMagnitudeTotals {
    compared_with_gas: usize,
    pct_rows: usize,
    baseline_gas_sum: u128,
    sonatina_gas_sum: u128,
    delta_gas_sum: i128,
    abs_delta_gas_sum: u128,
    delta_pct_sum: f64,
    abs_delta_pct_sum: f64,
}

#[derive(Debug, Clone, Copy, Default)]
struct GasMagnitudeTotals {
    vs_yul_unopt: DeltaMagnitudeTotals,
    vs_yul_opt: DeltaMagnitudeTotals,
}

#[derive(Debug, Clone, Copy, Default)]
struct OpcodeAggregateTotals {
    steps_sum: u128,
    runtime_bytes_sum: u128,
    runtime_ops_sum: u128,
    swap_ops_sum: u128,
    pop_ops_sum: u128,
    jump_ops_sum: u128,
    jumpi_ops_sum: u128,
    iszero_ops_sum: u128,
    mem_rw_ops_sum: u128,
    storage_rw_ops_sum: u128,
    mload_ops_sum: u128,
    mstore_ops_sum: u128,
    sload_ops_sum: u128,
    sstore_ops_sum: u128,
    keccak_ops_sum: u128,
    call_family_ops_sum: u128,
    copy_ops_sum: u128,
}

#[derive(Debug, Clone, Copy, Default)]
struct OpcodeMagnitudeTotals {
    compared_with_metrics: usize,
    yul_opt: OpcodeAggregateTotals,
    sonatina: OpcodeAggregateTotals,
}

#[derive(Debug, Clone)]
struct GasHotspotRow {
    suite: String,
    test: String,
    symbol: String,
    yul_opt_gas: Option<u64>,
    sonatina_gas: Option<u64>,
    delta_vs_yul_opt: i128,
    delta_vs_yul_opt_pct: String,
}

#[derive(Debug, Clone, Copy, Default)]
struct SuiteDeltaTotals {
    tests_with_delta: usize,
    delta_vs_yul_opt_sum: i128,
}

#[derive(Debug, Clone)]
struct SymtabEntry {
    start: u32,
    end: u32,
    symbol: String,
}

#[derive(Debug, Clone)]
struct TraceSymbolHotspotRow {
    suite: String,
    test: String,
    symbol: String,
    tail_steps_total: usize,
    tail_steps_mapped: usize,
    steps_in_symbol: usize,
}

#[derive(Debug, Clone)]
struct ObservabilityCoverageRow {
    suite: String,
    test: String,
    section: String,
    schema_version: String,
    section_bytes: u64,
    code_bytes: u64,
    data_bytes: u64,
    embed_bytes: u64,
    mapped_code_bytes: u64,
    unmapped_code_bytes: u64,
    unmapped_no_ir_inst: u64,
    unmapped_label_or_fixup_only: u64,
    unmapped_synthetic: u64,
    unmapped_unknown: u64,
}

#[derive(Debug, Clone, Copy, Default)]
struct ObservabilityCoverageTotals {
    section_bytes: u128,
    code_bytes: u128,
    data_bytes: u128,
    embed_bytes: u128,
    mapped_code_bytes: u128,
    unmapped_code_bytes: u128,
    unmapped_no_ir_inst: u128,
    unmapped_label_or_fixup_only: u128,
    unmapped_synthetic: u128,
    unmapped_unknown: u128,
}

#[derive(Debug, Clone)]
struct ObservabilityPcRange {
    start: u32,
    end: u32,
    func_name: String,
    reason: Option<String>,
}

#[derive(Debug, Clone)]
struct ObservabilityRuntimeSnapshot {
    section: String,
    schema_version: String,
    section_bytes: u64,
    code_bytes: u64,
    data_bytes: u64,
    embed_bytes: u64,
    mapped_code_bytes: u64,
    unmapped_code_bytes: u64,
    unmapped_no_ir_inst: u64,
    unmapped_label_or_fixup_only: u64,
    unmapped_synthetic: u64,
    unmapped_unknown: u64,
    pc_ranges: Vec<ObservabilityPcRange>,
}

#[derive(Debug, Clone)]
struct TraceObservabilityHotspotRow {
    suite: String,
    test: String,
    function: String,
    reason: String,
    tail_steps_total: usize,
    tail_steps_mapped: usize,
    steps_in_bucket: usize,
}

#[derive(Debug, Clone)]
struct DeploymentGasAttributionRow {
    test: String,
    symbol: String,
    yul_unopt_step_total_gas: Option<u64>,
    yul_opt_step_total_gas: Option<u64>,
    sonatina_step_total_gas: Option<u64>,
    yul_unopt_create_opcode_gas: Option<u64>,
    yul_opt_create_opcode_gas: Option<u64>,
    sonatina_create_opcode_gas: Option<u64>,
    yul_unopt_create2_opcode_gas: Option<u64>,
    yul_opt_create2_opcode_gas: Option<u64>,
    sonatina_create2_opcode_gas: Option<u64>,
    yul_unopt_constructor_frame_gas: Option<u64>,
    yul_opt_constructor_frame_gas: Option<u64>,
    sonatina_constructor_frame_gas: Option<u64>,
    yul_unopt_non_constructor_frame_gas: Option<u64>,
    yul_opt_non_constructor_frame_gas: Option<u64>,
    sonatina_non_constructor_frame_gas: Option<u64>,
    yul_unopt_create_opcode_steps: Option<u64>,
    yul_opt_create_opcode_steps: Option<u64>,
    sonatina_create_opcode_steps: Option<u64>,
    yul_unopt_create2_opcode_steps: Option<u64>,
    yul_opt_create2_opcode_steps: Option<u64>,
    sonatina_create2_opcode_steps: Option<u64>,
    note: String,
}

#[derive(Debug, Clone, Copy, Default)]
struct DeploymentGasAttributionTotals {
    compared_with_profile: usize,
    yul_opt_step_total_gas: u128,
    sonatina_step_total_gas: u128,
    yul_opt_create_opcode_gas: u128,
    sonatina_create_opcode_gas: u128,
    yul_opt_create2_opcode_gas: u128,
    sonatina_create2_opcode_gas: u128,
    yul_opt_constructor_frame_gas: u128,
    sonatina_constructor_frame_gas: u128,
    yul_opt_non_constructor_frame_gas: u128,
    sonatina_non_constructor_frame_gas: u128,
}

impl GasMeasurement {
    fn from_test_outcome(outcome: &TestOutcome) -> Self {
        Self {
            gas_used: outcome.result.gas_used,
            deploy_gas_used: outcome.result.deploy_gas_used,
            total_gas_used: outcome.result.total_gas_used,
            step_count: outcome.step_count,
            runtime_metrics: outcome.runtime_metrics,
            gas_profile: outcome.gas_profile,
            passed: outcome.result.passed,
            error_message: outcome.result.error_message.clone(),
        }
    }

    fn status_label(&self) -> String {
        if self.passed {
            "ok".to_string()
        } else if let Some(msg) = &self.error_message {
            format!("failed: {msg}")
        } else {
            "failed".to_string()
        }
    }
}

impl DeploymentGasAttributionTotals {
    fn add_observation(&mut self, yul_opt: CallGasProfile, sonatina: CallGasProfile) {
        self.compared_with_profile += 1;
        self.yul_opt_step_total_gas += yul_opt.total_step_gas as u128;
        self.sonatina_step_total_gas += sonatina.total_step_gas as u128;
        self.yul_opt_create_opcode_gas += yul_opt.create_opcode_gas as u128;
        self.sonatina_create_opcode_gas += sonatina.create_opcode_gas as u128;
        self.yul_opt_create2_opcode_gas += yul_opt.create2_opcode_gas as u128;
        self.sonatina_create2_opcode_gas += sonatina.create2_opcode_gas as u128;
        self.yul_opt_constructor_frame_gas += yul_opt.constructor_frame_gas as u128;
        self.sonatina_constructor_frame_gas += sonatina.constructor_frame_gas as u128;
        self.yul_opt_non_constructor_frame_gas += yul_opt.non_constructor_frame_gas as u128;
        self.sonatina_non_constructor_frame_gas += sonatina.non_constructor_frame_gas as u128;
    }
}

impl GasTotals {
    fn add(&mut self, other: Self) {
        self.tests_in_scope += other.tests_in_scope;
        self.vs_yul_unopt.compared_with_gas += other.vs_yul_unopt.compared_with_gas;
        self.vs_yul_unopt.sonatina_lower += other.vs_yul_unopt.sonatina_lower;
        self.vs_yul_unopt.sonatina_higher += other.vs_yul_unopt.sonatina_higher;
        self.vs_yul_unopt.equal += other.vs_yul_unopt.equal;
        self.vs_yul_unopt.incomplete += other.vs_yul_unopt.incomplete;
        self.vs_yul_opt.compared_with_gas += other.vs_yul_opt.compared_with_gas;
        self.vs_yul_opt.sonatina_lower += other.vs_yul_opt.sonatina_lower;
        self.vs_yul_opt.sonatina_higher += other.vs_yul_opt.sonatina_higher;
        self.vs_yul_opt.equal += other.vs_yul_opt.equal;
        self.vs_yul_opt.incomplete += other.vs_yul_opt.incomplete;
    }
}

impl ObservabilityCoverageTotals {
    fn add_row(&mut self, row: &ObservabilityCoverageRow) {
        self.section_bytes += row.section_bytes as u128;
        self.code_bytes += row.code_bytes as u128;
        self.data_bytes += row.data_bytes as u128;
        self.embed_bytes += row.embed_bytes as u128;
        self.mapped_code_bytes += row.mapped_code_bytes as u128;
        self.unmapped_code_bytes += row.unmapped_code_bytes as u128;
        self.unmapped_no_ir_inst += row.unmapped_no_ir_inst as u128;
        self.unmapped_label_or_fixup_only += row.unmapped_label_or_fixup_only as u128;
        self.unmapped_synthetic += row.unmapped_synthetic as u128;
        self.unmapped_unknown += row.unmapped_unknown as u128;
    }
}

impl DeltaMagnitudeTotals {
    fn add(&mut self, other: Self) {
        self.compared_with_gas += other.compared_with_gas;
        self.pct_rows += other.pct_rows;
        self.baseline_gas_sum += other.baseline_gas_sum;
        self.sonatina_gas_sum += other.sonatina_gas_sum;
        self.delta_gas_sum += other.delta_gas_sum;
        self.abs_delta_gas_sum += other.abs_delta_gas_sum;
        self.delta_pct_sum += other.delta_pct_sum;
        self.abs_delta_pct_sum += other.abs_delta_pct_sum;
    }

    fn mean_delta_gas(self) -> Option<f64> {
        if self.compared_with_gas == 0 {
            None
        } else {
            Some(self.delta_gas_sum as f64 / self.compared_with_gas as f64)
        }
    }

    fn mean_abs_delta_gas(self) -> Option<f64> {
        if self.compared_with_gas == 0 {
            None
        } else {
            Some(self.abs_delta_gas_sum as f64 / self.compared_with_gas as f64)
        }
    }

    fn mean_delta_pct(self) -> Option<f64> {
        if self.pct_rows == 0 {
            None
        } else {
            Some(self.delta_pct_sum / self.pct_rows as f64)
        }
    }

    fn mean_abs_delta_pct(self) -> Option<f64> {
        if self.pct_rows == 0 {
            None
        } else {
            Some(self.abs_delta_pct_sum / self.pct_rows as f64)
        }
    }

    fn weighted_delta_pct(self) -> Option<f64> {
        if self.baseline_gas_sum == 0 {
            None
        } else {
            Some(self.delta_gas_sum as f64 * 100.0 / self.baseline_gas_sum as f64)
        }
    }
}

impl GasMagnitudeTotals {
    fn add(&mut self, other: Self) {
        self.vs_yul_unopt.add(other.vs_yul_unopt);
        self.vs_yul_opt.add(other.vs_yul_opt);
    }
}

impl OpcodeAggregateTotals {
    fn add_observation(&mut self, steps: u64, metrics: EvmRuntimeMetrics) {
        self.steps_sum += steps as u128;
        self.runtime_bytes_sum += metrics.byte_len as u128;
        self.runtime_ops_sum += metrics.op_count as u128;
        self.swap_ops_sum += metrics.swap_ops as u128;
        self.pop_ops_sum += metrics.pop_ops as u128;
        self.jump_ops_sum += metrics.jump_ops as u128;
        self.jumpi_ops_sum += metrics.jumpi_ops as u128;
        self.iszero_ops_sum += metrics.iszero_ops as u128;
        self.mem_rw_ops_sum += metrics.mem_rw_ops_total() as u128;
        self.storage_rw_ops_sum += metrics.storage_rw_ops_total() as u128;
        self.mload_ops_sum += metrics.mload_ops as u128;
        self.mstore_ops_sum += metrics.mstore_ops as u128;
        self.sload_ops_sum += metrics.sload_ops as u128;
        self.sstore_ops_sum += metrics.sstore_ops as u128;
        self.keccak_ops_sum += metrics.keccak_ops as u128;
        self.call_family_ops_sum += metrics.call_family_ops_total() as u128;
        self.copy_ops_sum += metrics.copy_ops_total() as u128;
    }

    fn add(&mut self, other: Self) {
        self.steps_sum += other.steps_sum;
        self.runtime_bytes_sum += other.runtime_bytes_sum;
        self.runtime_ops_sum += other.runtime_ops_sum;
        self.swap_ops_sum += other.swap_ops_sum;
        self.pop_ops_sum += other.pop_ops_sum;
        self.jump_ops_sum += other.jump_ops_sum;
        self.jumpi_ops_sum += other.jumpi_ops_sum;
        self.iszero_ops_sum += other.iszero_ops_sum;
        self.mem_rw_ops_sum += other.mem_rw_ops_sum;
        self.storage_rw_ops_sum += other.storage_rw_ops_sum;
        self.mload_ops_sum += other.mload_ops_sum;
        self.mstore_ops_sum += other.mstore_ops_sum;
        self.sload_ops_sum += other.sload_ops_sum;
        self.sstore_ops_sum += other.sstore_ops_sum;
        self.keccak_ops_sum += other.keccak_ops_sum;
        self.call_family_ops_sum += other.call_family_ops_sum;
        self.copy_ops_sum += other.copy_ops_sum;
    }
}

impl OpcodeMagnitudeTotals {
    fn add(&mut self, other: Self) {
        self.compared_with_metrics += other.compared_with_metrics;
        self.yul_opt.add(other.yul_opt);
        self.sonatina.add(other.sonatina);
    }
}

impl EvmRuntimeMetrics {
    fn stack_ops_total(self) -> usize {
        self.push_ops + self.dup_ops + self.swap_ops + self.pop_ops
    }

    fn mem_rw_ops_total(self) -> usize {
        self.mload_ops + self.mstore_ops
    }

    fn storage_rw_ops_total(self) -> usize {
        self.sload_ops + self.sstore_ops
    }

    fn call_family_ops_total(self) -> usize {
        self.call_ops + self.staticcall_ops
    }

    fn copy_ops_total(self) -> usize {
        self.calldatacopy_ops + self.returndatacopy_ops + self.mcopy_ops
    }
}

fn suite_error_result(suite: &str, kind: &str, message: String) -> Vec<TestResult> {
    vec![TestResult {
        name: format!("{suite}::{kind}"),
        passed: false,
        error_message: Some(message),
        gas_used: None,
        deploy_gas_used: None,
        total_gas_used: None,
    }]
}

fn write_report_error(report: &ReportContext, filename: &str, contents: &str) {
    let dir = report.root_dir.join("errors");
    let _ = create_dir_all_utf8(&dir);
    let _ = std::fs::write(dir.join(filename), contents);
}

fn write_codegen_report_error(report: &ReportContext, contents: &str) {
    write_report_error(report, "codegen_error.txt", contents);
    if is_verifier_error_text(contents) {
        write_report_error(report, "verifier_error.txt", contents);
    }
}

#[derive(Debug, Clone)]
struct ReportContext {
    root_dir: Utf8PathBuf,
}

#[derive(Debug, Clone)]
struct SuitePlan {
    index: usize,
    path: Utf8PathBuf,
    suite: String,
    suite_key: String,
    suite_report_out: Option<Utf8PathBuf>,
}

#[derive(Debug)]
struct SuiteRunResult {
    index: usize,
    path: Utf8PathBuf,
    suite_key: String,
    output: String,
    results: Vec<TestResult>,
    aggregate_suite_staging: Option<ReportStaging>,
}

#[derive(Debug, Clone, Default)]
pub struct TestDebugOptions {
    pub trace_evm: bool,
    pub trace_evm_keep: usize,
    pub trace_evm_stack_n: usize,
    pub sonatina_symtab: bool,
    pub sonatina_evm_debug: bool,
    pub sonatina_observability: bool,
    pub debug_dir: Option<Utf8PathBuf>,
}

impl TestDebugOptions {
    fn ensure_debug_dir(&self) -> Result<(), String> {
        let Some(dir) = &self.debug_dir else {
            return Ok(());
        };
        std::fs::create_dir_all(dir)
            .map_err(|err| format!("failed to create debug dir `{dir}`: {err}"))
    }

    fn sonatina_debug_config(&self) -> Result<SonatinaTestDebugConfig, String> {
        self.ensure_debug_dir()?;
        let mut config = SonatinaTestDebugConfig::default();

        if self.sonatina_symtab {
            let sink = if let Some(dir) = &self.debug_dir {
                let path = dir.join("sonatina_symtab.txt");
                truncate_file(&path)?;
                DebugOutputSink {
                    path: Some(path.into_std_path_buf()),
                    write_stderr: false,
                }
            } else {
                DebugOutputSink {
                    path: None,
                    write_stderr: true,
                }
            };
            config.symtab_output = Some(sink);
        }

        if self.sonatina_evm_debug {
            let sink = if let Some(dir) = &self.debug_dir {
                let path = dir.join("sonatina_evm_bytecode.txt");
                truncate_file(&path)?;
                DebugOutputSink {
                    path: Some(path.into_std_path_buf()),
                    write_stderr: false,
                }
            } else {
                DebugOutputSink {
                    path: None,
                    write_stderr: true,
                }
            };
            config.evm_debug_output = Some(sink);
        }

        config.emit_observability = self.sonatina_observability;

        Ok(config)
    }

    fn evm_trace_options_for_test(
        &self,
        test_suite: Option<&str>,
        test_name: &str,
    ) -> Result<Option<EvmTraceOptions>, String> {
        if !self.trace_evm {
            return Ok(None);
        }

        let mut options = EvmTraceOptions {
            keep_steps: self.trace_evm_keep.max(1),
            stack_n: self.trace_evm_stack_n,
            out_path: None,
            write_stderr: true,
        };

        if let Some(dir) = &self.debug_dir {
            let mut file = String::new();
            if let Some(suite) = test_suite {
                let suite = sanitize_filename(suite);
                if !suite.is_empty() {
                    file.push_str(&suite);
                    file.push_str("__");
                }
            }
            file.push_str(&sanitize_filename(test_name));
            if file.is_empty() {
                file = "test".to_string();
            }
            let path = dir.join(format!("{file}.evm_trace.txt"));
            truncate_file(&path)?;
            options.out_path = Some(path.into_std_path_buf());
            options.write_stderr = false;
        }

        Ok(Some(options))
    }
}

fn truncate_file(path: &Utf8PathBuf) -> Result<(), String> {
    std::fs::write(path, "").map_err(|err| format!("failed to truncate `{path}`: {err}"))
}

fn plan_suite_report_path(
    dir: &Utf8PathBuf,
    base: &str,
    reserved: &mut FxHashSet<String>,
) -> Utf8PathBuf {
    let mut suffix = 0usize;
    loop {
        let file = if suffix == 0 {
            format!("{base}.tar.gz")
        } else {
            format!("{base}-{suffix}.tar.gz")
        };
        let candidate = dir.join(file);
        let key = candidate.as_str().to_string();
        if !candidate.exists() && reserved.insert(key) {
            return candidate;
        }
        suffix += 1;
    }
}

fn build_suite_plans(
    input_paths: Vec<Utf8PathBuf>,
    report_dir: Option<&Utf8PathBuf>,
) -> Result<Vec<SuitePlan>, String> {
    let mut plans = Vec::with_capacity(input_paths.len());
    let mut seen_suite_names: FxHashMap<String, usize> = FxHashMap::default();
    for (index, path) in input_paths.into_iter().enumerate() {
        let suite = suite_name_for_path(&path);
        let seen = seen_suite_names.entry(suite.clone()).or_insert(0);
        *seen += 1;
        let suite_key = if *seen == 1 {
            suite.clone()
        } else {
            format!("{suite}-{}", seen)
        };
        plans.push(SuitePlan {
            index,
            path,
            suite,
            suite_key,
            suite_report_out: None,
        });
    }

    if let Some(dir) = report_dir {
        let mut reserved = FxHashSet::default();
        for plan in &mut plans {
            let base = if plan.suite_key.is_empty() {
                "tests".to_string()
            } else {
                sanitize_filename(&plan.suite_key)
            };
            plan.suite_report_out = Some(plan_suite_report_path(dir, &base, &mut reserved));
        }
    }

    Ok(plans)
}

fn effective_jobs(requested: usize, suite_count: usize) -> usize {
    if suite_count == 0 {
        return 1;
    }
    let requested = if requested == 0 {
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1)
    } else {
        requested
    };
    requested.clamp(1, suite_count)
}

/// Run tests in the given path.
///
/// # Arguments
/// * `paths` - Paths to .fe files or directories containing ingots (supports globs)
/// * `filter` - Optional filter pattern for test names
/// * `show_logs` - Whether to show event logs from test execution
/// * `backend` - Codegen backend for test artifacts ("yul" or "sonatina")
/// * `report_out` - Optional report output path (`.tar.gz`)
///
/// Returns `Ok(true)` if any tests failed, `Ok(false)` if all passed,
/// or `Err` on fatal setup errors.
#[allow(clippy::too_many_arguments)]
pub fn run_tests(
    paths: &[Utf8PathBuf],
    filter: Option<&str>,
    jobs: usize,
    show_logs: bool,
    backend: &str,
    opt_level: OptLevel,
    debug: &TestDebugOptions,
    report_out: Option<&Utf8PathBuf>,
    report_dir: Option<&Utf8PathBuf>,
    report_failed_only: bool,
    call_trace: bool,
) -> Result<bool, String> {
    let input_paths = expand_test_paths(paths)?;
    let suite_plans = build_suite_plans(input_paths, report_dir)?;
    let worker_count = effective_jobs(jobs, suite_plans.len());
    let multi = suite_plans.len() > 1;
    if multi {
        println!(
            "running `fe test` for {} inputs (jobs={worker_count})\n",
            suite_plans.len()
        );
    }

    if let Some(dir) = report_dir {
        create_dir_all_utf8(dir)?;
    }

    let report_root = report_out
        .map(|out| -> Result<_, String> {
            let staging = create_run_report_staging()?;
            let out = normalize_report_out_path(out)?;
            Ok((out, staging))
        })
        .transpose()?;

    if let Some((_, staging)) = report_root.as_ref() {
        let root = &staging.root_dir;
        create_dir_all_utf8(&root.join("passed"))?;
        create_dir_all_utf8(&root.join("failed"))?;
        write_report_meta(root, "fe test report", None);
    }

    let aggregate_report = report_root.is_some();
    let mut suite_runs = Vec::with_capacity(suite_plans.len());
    if worker_count == 1 || suite_plans.len() <= 1 {
        for plan in &suite_plans {
            suite_runs.push(run_single_suite(
                plan,
                filter,
                show_logs,
                backend,
                opt_level,
                debug,
                report_failed_only,
                aggregate_report,
                call_trace,
            )?);
        }
    } else {
        let (tx, rx) = mpsc::channel::<Result<SuiteRunResult, String>>();
        let next = AtomicUsize::new(0);
        std::thread::scope(|scope| {
            for _ in 0..worker_count {
                let tx = tx.clone();
                let plans = &suite_plans;
                let next = &next;
                scope.spawn(move || {
                    loop {
                        let idx = next.fetch_add(1, Ordering::Relaxed);
                        if idx >= plans.len() {
                            break;
                        }
                        let result = run_single_suite(
                            &plans[idx],
                            filter,
                            show_logs,
                            backend,
                            opt_level,
                            debug,
                            report_failed_only,
                            aggregate_report,
                            call_trace,
                        );
                        let _ = tx.send(result);
                    }
                });
            }
        });
        drop(tx);

        for _ in 0..suite_plans.len() {
            let result = rx
                .recv()
                .map_err(|err| format!("suite worker failed: {err}"))?;
            suite_runs.push(result?);
        }
    }

    suite_runs.sort_unstable_by_key(|run| run.index);

    let mut test_results = Vec::new();
    for suite_run in suite_runs {
        let SuiteRunResult {
            path,
            suite_key,
            output,
            results,
            aggregate_suite_staging,
            ..
        } = suite_run;
        if multi {
            println!("==> {path}");
        }
        if !output.is_empty() {
            print!("{output}");
        }

        let suite_failed = results.iter().any(|r| !r.passed);
        if results.is_empty() {
            eprintln!("No tests found in {path}");
        } else {
            test_results.extend(results);
        }

        if let Some((_, root_staging)) = &report_root
            && let Some(staging) = aggregate_suite_staging
        {
            let status_dir = if suite_failed { "failed" } else { "passed" };
            let to = root_staging.root_dir.join(status_dir).join(&suite_key);
            let _ = std::fs::remove_dir_all(&to);
            match std::fs::rename(&staging.root_dir, &to) {
                Ok(()) => {
                    let _ = std::fs::remove_dir_all(&staging.temp_dir);
                }
                Err(err) => {
                    eprintln!("Error: failed to stage suite report `{suite_key}`: {err}");
                    eprintln!("Report staging directory left at `{}`", staging.temp_dir);
                }
            }
        }
    }

    if let Some((out, staging)) = report_root {
        write_run_gas_comparison_summary(&staging.root_dir, opt_level);
        write_report_manifest(&staging.root_dir, backend, opt_level, filter, &test_results);
        if let Err(err) = tar_gz_dir(&staging.root_dir, &out) {
            eprintln!("Error: failed to write report `{out}`: {err}");
            eprintln!("Report staging directory left at `{}`", staging.temp_dir);
        } else {
            // Best-effort cleanup.
            let _ = std::fs::remove_dir_all(&staging.temp_dir);
            println!("wrote report: {out}");
        }
    }

    print_summary(&test_results);
    Ok(test_results.iter().any(|r| !r.passed))
}

#[allow(clippy::too_many_arguments)]
fn run_single_suite(
    plan: &SuitePlan,
    filter: Option<&str>,
    show_logs: bool,
    backend: &str,
    opt_level: OptLevel,
    debug: &TestDebugOptions,
    report_failed_only: bool,
    aggregate_report: bool,
    call_trace: bool,
) -> Result<SuiteRunResult, String> {
    let suite_report_staging = if plan.suite_report_out.is_some() || aggregate_report {
        Some(create_suite_report_staging(&plan.suite_key)?)
    } else {
        None
    };

    let report_ctx = if let Some(staging) = suite_report_staging.as_ref() {
        let suite_dir = staging.root_dir.clone();
        write_report_meta(&suite_dir, "fe test report (suite)", Some(&plan.suite));
        let inputs_dir = suite_dir.join("inputs");
        create_dir_all_utf8(&inputs_dir)?;
        copy_input_into_report(&plan.path, &inputs_dir)?;
        Some(ReportContext {
            root_dir: suite_dir,
        })
    } else {
        None
    };

    let mut suite_debug = debug.clone();
    if report_ctx.is_some() {
        suite_debug.trace_evm = true;
        suite_debug.sonatina_symtab = true;
        suite_debug.sonatina_evm_debug = true;
        suite_debug.sonatina_observability = true;
        suite_debug.debug_dir = report_ctx.as_ref().map(|ctx| ctx.root_dir.join("debug"));
    }
    let sonatina_debug = suite_debug.sonatina_debug_config()?;

    let mut output = String::new();
    let mut db = DriverDataBase::default();
    let suite_results = if plan.path.is_file() && plan.path.extension() == Some("fe") {
        run_tests_single_file(
            &mut db,
            &plan.path,
            &plan.suite,
            filter,
            show_logs,
            backend,
            opt_level,
            &suite_debug,
            &sonatina_debug,
            report_ctx.as_ref(),
            call_trace,
            &mut output,
        )
    } else if plan.path.is_dir() {
        run_tests_ingot(
            &mut db,
            &plan.path,
            &plan.suite,
            filter,
            show_logs,
            backend,
            opt_level,
            &suite_debug,
            &sonatina_debug,
            report_ctx.as_ref(),
            call_trace,
            &mut output,
        )
    } else {
        return Err("Path must be either a .fe file or a directory containing fe.toml".to_string());
    };

    let mut aggregate_suite_staging = suite_report_staging;
    if let Some(out) = &plan.suite_report_out
        && let Some(staging) = aggregate_suite_staging.take()
    {
        let should_write = !report_failed_only || suite_results.iter().any(|r| !r.passed);
        if should_write {
            write_report_manifest(
                &staging.root_dir,
                backend,
                opt_level,
                filter,
                &suite_results,
            );
            match tar_gz_dir(&staging.root_dir, out) {
                Ok(()) => {
                    let _ = std::fs::remove_dir_all(&staging.temp_dir);
                    let _ = writeln!(&mut output, "wrote report: {out}");
                }
                Err(err) => {
                    let _ = writeln!(&mut output, "Error: failed to write report `{out}`: {err}");
                    let _ = writeln!(
                        &mut output,
                        "Report staging directory left at `{}`",
                        staging.temp_dir
                    );
                }
            }
        } else {
            let _ = std::fs::remove_dir_all(&staging.temp_dir);
        }
    }

    Ok(SuiteRunResult {
        index: plan.index,
        path: plan.path.clone(),
        suite_key: plan.suite_key.clone(),
        output,
        results: suite_results,
        aggregate_suite_staging,
    })
}

/// Runs tests defined in a single `.fe` source file.
///
/// * `db` - Driver database used for compilation.
/// * `file_path` - Path to the `.fe` file.
/// * `filter` - Optional substring filter for test names.
/// * `show_logs` - Whether to show event logs from test execution.
///
/// Returns the collected test results.
#[allow(clippy::too_many_arguments)]
fn run_tests_single_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    suite: &str,
    filter: Option<&str>,
    show_logs: bool,
    backend: &str,
    opt_level: OptLevel,
    debug: &TestDebugOptions,
    sonatina_debug: &SonatinaTestDebugConfig,
    report: Option<&ReportContext>,
    call_trace: bool,
    output: &mut String,
) -> Vec<TestResult> {
    // Create a file URL for the single .fe file
    let file_url = match Url::from_file_path(file_path.canonicalize_utf8().unwrap()) {
        Ok(url) => url,
        Err(_) => {
            return suite_error_result(suite, "setup", format!("Invalid file path: {file_path}"));
        }
    };

    // Read the file content
    let content = match std::fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            return suite_error_result(
                suite,
                "setup",
                format!("Error reading file {file_path}: {err}"),
            );
        }
    };

    // Add the file to the workspace
    db.workspace().touch(db, file_url.clone(), Some(content));

    // Get the top-level module
    let Some(file) = db.workspace().get(db, &file_url) else {
        return suite_error_result(
            suite,
            "setup",
            format!("Could not process file {file_path}"),
        );
    };

    let top_mod = db.top_mod(file);

    // Check for compilation errors first
    let diags = db.run_on_top_mod(top_mod);
    if !diags.is_empty() {
        let formatted = diags.format_diags(db);
        let _ = writeln!(output, "Compilation errors in {file_url}");
        let _ = writeln!(output);
        let _ = writeln!(output, "{formatted}");
        if let Some(report) = report {
            write_report_error(report, "compilation_errors.txt", &formatted);
        }
        return suite_error_result(
            suite,
            "compile",
            format!("Compilation errors in {file_url}"),
        );
    }

    // Discover and run tests
    maybe_write_suite_ir(db, top_mod, backend, report);
    discover_and_run_tests(
        db,
        top_mod,
        suite,
        filter,
        show_logs,
        backend,
        opt_level,
        debug,
        sonatina_debug,
        report,
        call_trace,
        output,
    )
}

/// Runs tests in an ingot directory (containing `fe.toml`).
///
/// * `db` - Driver database used for compilation.
/// * `dir_path` - Path to the ingot directory.
/// * `filter` - Optional substring filter for test names.
/// * `show_logs` - Whether to show event logs from test execution.
///
/// Returns the collected test results.
#[allow(clippy::too_many_arguments)]
fn run_tests_ingot(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    suite: &str,
    filter: Option<&str>,
    show_logs: bool,
    backend: &str,
    opt_level: OptLevel,
    debug: &TestDebugOptions,
    sonatina_debug: &SonatinaTestDebugConfig,
    report: Option<&ReportContext>,
    call_trace: bool,
    output: &mut String,
) -> Vec<TestResult> {
    let canonical_path = match dir_path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            return suite_error_result(
                suite,
                "setup",
                format!("Invalid or non-existent directory path: {dir_path}"),
            );
        }
    };

    let ingot_url = match Url::from_directory_path(canonical_path.as_str()) {
        Ok(url) => url,
        Err(_) => {
            return suite_error_result(
                suite,
                "setup",
                format!("Invalid directory path: {dir_path}"),
            );
        }
    };

    let had_init_diagnostics = driver::init_ingot(db, &ingot_url);
    if had_init_diagnostics {
        let msg = format!("Compilation errors while initializing ingot `{dir_path}`");
        let _ = writeln!(output, "{msg}");
        if let Some(report) = report {
            write_report_error(report, "compilation_errors.txt", &msg);
        }
        return suite_error_result(suite, "compile", msg);
    }

    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        return suite_error_result(
            suite,
            "setup",
            "Could not resolve ingot from directory".to_string(),
        );
    };

    // Check for compilation errors
    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        let formatted = diags.format_diags(db);
        let _ = writeln!(output, "{formatted}");
        if let Some(report) = report {
            write_report_error(report, "compilation_errors.txt", &formatted);
        }
        return suite_error_result(suite, "compile", "Compilation errors".to_string());
    }

    let root_mod = ingot.root_mod(db);
    maybe_write_suite_ir(db, root_mod, backend, report);
    discover_and_run_tests(
        db,
        root_mod,
        suite,
        filter,
        show_logs,
        backend,
        opt_level,
        debug,
        sonatina_debug,
        report,
        call_trace,
        output,
    )
}

/// Emit a test module with panic recovery and report integration.
///
/// Wraps `emit_fn` in `catch_unwind`, writes error/panic info into the report
/// staging directory when present, and returns the output or an early-return
/// error result vector.
fn emit_with_catch_unwind<E: std::fmt::Display>(
    emit_fn: impl FnOnce() -> Result<TestModuleOutput, E>,
    backend_label: &str,
    suite: &str,
    report: Option<&ReportContext>,
    output: &mut String,
) -> Result<TestModuleOutput, Vec<TestResult>> {
    let _hook = report.map(|r| install_report_panic_hook(r, "codegen_panic_full.txt"));
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(emit_fn)) {
        Ok(Ok(output)) => Ok(output),
        Ok(Err(err)) => {
            let msg = format!("Failed to emit test {backend_label}: {err}");
            let _ = writeln!(output, "{msg}");
            if let Some(report) = report {
                write_codegen_report_error(report, &msg);
            }
            Err(suite_error_result(suite, "codegen", msg))
        }
        Err(payload) => {
            let msg = format!(
                "{backend_label} backend panicked while emitting test module: {}",
                panic_payload_to_string(payload.as_ref())
            );
            let _ = writeln!(output, "{msg}");
            if let Some(report) = report {
                write_report_error(report, "codegen_panic.txt", &msg);
            }
            Err(suite_error_result(suite, "codegen", msg))
        }
    }
}

/// Discovers `#[test]` functions, compiles them, and executes each one.
///
/// * `db` - Driver database used for compilation.
/// * `top_mod` - Root module to scan for tests.
/// * `filter` - Optional substring filter for test names.
/// * `show_logs` - Whether to show event logs from test execution.
///
/// Returns the collected test results.
#[allow(clippy::too_many_arguments)]
fn discover_and_run_tests(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    suite: &str,
    filter: Option<&str>,
    show_logs: bool,
    backend: &str,
    opt_level: OptLevel,
    debug: &TestDebugOptions,
    sonatina_debug: &SonatinaTestDebugConfig,
    report: Option<&ReportContext>,
    call_trace: bool,
    output: &mut String,
) -> Vec<TestResult> {
    let backend = backend.to_lowercase();
    let emit_result = match backend.as_str() {
        "yul" => emit_with_catch_unwind(
            || emit_test_module_yul(db, top_mod),
            "Yul",
            suite,
            report,
            output,
        ),
        "sonatina" => emit_with_catch_unwind(
            || emit_test_module_sonatina(db, top_mod, opt_level, sonatina_debug),
            "Sonatina",
            suite,
            report,
            output,
        ),
        other => {
            return suite_error_result(
                suite,
                "setup",
                format!("unknown backend `{other}` (expected 'yul' or 'sonatina')"),
            );
        }
    };
    let module_output = match emit_result {
        Ok(output) => output,
        Err(results) => return results,
    };

    if module_output.tests.is_empty() {
        return Vec::new();
    }

    let gas_comparison_cases = report.map(|ctx| {
        collect_gas_comparison_cases(
            db,
            top_mod,
            suite,
            filter,
            ctx,
            backend.as_str(),
            opt_level,
            &module_output.tests,
        )
    });

    let mut results = Vec::new();
    let mut primary_measurements = FxHashMap::default();

    for case in &module_output.tests {
        if !test_case_matches_filter(case, filter) {
            continue;
        }

        let evm_trace = match debug.evm_trace_options_for_test(Some(suite), &case.display_name) {
            Ok(v) => v,
            Err(err) => {
                let _ = writeln!(output, "test {} ... {}", case.display_name, "FAILED".red());
                let _ = writeln!(output, "    {err}");
                results.push(TestResult {
                    name: case.display_name.clone(),
                    passed: false,
                    error_message: Some(err),
                    gas_used: None,
                    deploy_gas_used: None,
                    total_gas_used: None,
                });
                continue;
            }
        };

        // Compile and run the test
        let outcome = compile_and_run_test(
            case,
            show_logs,
            backend.as_str(),
            opt_level.yul_optimize(),
            evm_trace.as_ref(),
            report,
            call_trace,
            report.is_some(),
        );

        if outcome.result.passed {
            let _ = writeln!(output, "test {} ... {}", case.display_name, "ok".green());
        } else {
            let _ = writeln!(output, "test {} ... {}", case.display_name, "FAILED".red());
            if let Some(ref msg) = outcome.result.error_message {
                let _ = writeln!(output, "    {msg}");
            }
        }

        if let Some(trace) = &outcome.trace {
            let _ = writeln!(output, "--- call trace ---");
            let _ = write!(output, "{trace}");
            let _ = writeln!(output, "--- end trace ---");
        }

        if show_logs {
            if !outcome.logs.is_empty() {
                for log in &outcome.logs {
                    let _ = writeln!(output, "    log {log}");
                }
            } else if outcome.result.passed {
                let _ = writeln!(output, "    log (none)");
            } else {
                let _ = writeln!(output, "    log (unavailable for failed tests)");
            }
        }

        primary_measurements.insert(
            case.symbol_name.clone(),
            GasMeasurement::from_test_outcome(&outcome),
        );
        results.push(outcome.result);
    }

    if let (Some(report), Some(cases)) = (report, gas_comparison_cases.as_ref()) {
        write_gas_comparison_report(
            report,
            backend.as_str(),
            opt_level,
            cases,
            &primary_measurements,
        );
    }

    results
}

fn test_case_matches_filter(case: &TestMetadata, filter: Option<&str>) -> bool {
    let Some(pattern) = filter else {
        return true;
    };
    case.hir_name.contains(pattern)
        || case.symbol_name.contains(pattern)
        || case.display_name.contains(pattern)
}

fn collect_gas_comparison_cases(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    suite: &str,
    filter: Option<&str>,
    report: &ReportContext,
    primary_backend: &str,
    opt_level: OptLevel,
    primary_cases: &[TestMetadata],
) -> Vec<GasComparisonCase> {
    let mut by_symbol: FxHashMap<String, GasComparisonCase> = FxHashMap::default();
    let mut setup_errors = Vec::new();

    for case in primary_cases
        .iter()
        .filter(|case| test_case_matches_filter(case, filter))
    {
        let key = case.symbol_name.clone();
        let entry = by_symbol
            .entry(key.clone())
            .or_insert_with(|| GasComparisonCase {
                display_name: case.display_name.clone(),
                symbol_name: key,
                yul: None,
                sonatina: None,
            });

        if primary_backend.eq_ignore_ascii_case("yul") {
            entry.yul = Some(case.clone());
        } else if primary_backend.eq_ignore_ascii_case("sonatina") {
            entry.sonatina = Some(case.clone());
        }
    }

    if primary_backend.eq_ignore_ascii_case("yul") {
        let mut emit_output = String::new();
        match emit_with_catch_unwind(
            || {
                emit_test_module_sonatina(
                    db,
                    top_mod,
                    opt_level,
                    &SonatinaTestDebugConfig::default(),
                )
            },
            "Sonatina",
            suite,
            None,
            &mut emit_output,
        ) {
            Ok(output) => {
                for case in output
                    .tests
                    .into_iter()
                    .filter(|case| test_case_matches_filter(case, filter))
                {
                    let key = case.symbol_name.clone();
                    let entry = by_symbol
                        .entry(key.clone())
                        .or_insert_with(|| GasComparisonCase {
                            display_name: case.display_name.clone(),
                            symbol_name: key,
                            yul: None,
                            sonatina: None,
                        });
                    entry.sonatina = Some(case);
                }
            }
            Err(results) => {
                setup_errors.push(format!(
                    "failed to emit Sonatina tests for gas comparison:\n{}",
                    format_results_for_report(&results)
                ));
            }
        }
    } else if primary_backend.eq_ignore_ascii_case("sonatina") {
        let mut emit_output = String::new();
        match emit_with_catch_unwind(
            || emit_test_module_yul(db, top_mod),
            "Yul",
            suite,
            None,
            &mut emit_output,
        ) {
            Ok(output) => {
                for case in output
                    .tests
                    .into_iter()
                    .filter(|case| test_case_matches_filter(case, filter))
                {
                    let key = case.symbol_name.clone();
                    let entry = by_symbol
                        .entry(key.clone())
                        .or_insert_with(|| GasComparisonCase {
                            display_name: case.display_name.clone(),
                            symbol_name: key,
                            yul: None,
                            sonatina: None,
                        });
                    entry.yul = Some(case);
                }
            }
            Err(results) => {
                setup_errors.push(format!(
                    "failed to emit Yul tests for gas comparison:\n{}",
                    format_results_for_report(&results)
                ));
            }
        }
    } else {
        setup_errors.push(format!(
            "unknown backend `{primary_backend}` (expected 'yul' or 'sonatina')"
        ));
    }

    if !setup_errors.is_empty() {
        write_report_error(
            report,
            "gas_comparison_setup_error.txt",
            &setup_errors.join("\n\n"),
        );
    }

    let mut cases: Vec<_> = by_symbol.into_values().collect();
    cases.sort_by(|a, b| a.display_name.cmp(&b.display_name));
    cases
}

fn format_results_for_report(results: &[TestResult]) -> String {
    let mut out = String::new();
    for result in results {
        out.push_str(&format!("- {}\n", result.name));
        if let Some(msg) = &result.error_message {
            out.push_str(&format!("  {msg}\n"));
        }
    }
    if out.is_empty() {
        "no additional details".to_string()
    } else {
        out
    }
}

fn measure_case_gas(
    case: &TestMetadata,
    backend: &str,
    yul_optimize: bool,
    collect_step_count: bool,
) -> GasMeasurement {
    let outcome = compile_and_run_test(
        case,
        false,
        backend,
        yul_optimize,
        None,
        None,
        false,
        collect_step_count,
    );
    GasMeasurement::from_test_outcome(&outcome)
}

fn normalize_inline_text(value: &str) -> String {
    value.replace('\r', "\\r").replace('\n', "\\n")
}

fn csv_escape(value: &str) -> String {
    let value = normalize_inline_text(value);
    if value.contains(',') || value.contains('"') {
        format!("\"{}\"", value.replace('"', "\"\""))
    } else {
        value
    }
}

fn parse_csv_fields(line: &str) -> Vec<String> {
    let mut fields = Vec::new();
    let mut current = String::new();
    let mut chars = line.chars().peekable();
    let mut in_quotes = false;

    while let Some(ch) = chars.next() {
        match ch {
            '"' => {
                if in_quotes {
                    if chars.peek() == Some(&'"') {
                        current.push('"');
                        let _ = chars.next();
                    } else {
                        in_quotes = false;
                    }
                } else if current.is_empty() {
                    in_quotes = true;
                } else {
                    current.push(ch);
                }
            }
            ',' if !in_quotes => {
                fields.push(current);
                current = String::new();
            }
            _ => current.push(ch),
        }
    }
    fields.push(current);
    fields
}

fn parse_optional_u64_cell(value: &str) -> Option<u64> {
    value.trim().parse::<u64>().ok()
}

fn parse_optional_i128_cell(value: &str) -> Option<i128> {
    value.trim().parse::<i128>().ok()
}

fn parse_symtab_entries(contents: &str) -> Vec<SymtabEntry> {
    let mut rows = Vec::new();
    for line in contents.lines() {
        let line = line.trim();
        if !line.starts_with("off=") {
            continue;
        }
        // Format:
        // off=  3996 size=   700 test_erc20__StorPtr_Evm___...
        let mut parts = line.split_whitespace();
        let off_token = parts.next().unwrap_or_default();
        let off_value = parts.next().unwrap_or_default();
        let size_token = parts.next().unwrap_or_default();
        let size_value = parts.next().unwrap_or_default();
        if off_token != "off=" || size_token != "size=" {
            continue;
        }
        let Ok(start) = off_value.parse::<u32>() else {
            continue;
        };
        let Ok(size) = size_value.parse::<u32>() else {
            continue;
        };
        let symbol = parts.collect::<Vec<_>>().join(" ");
        if symbol.is_empty() {
            continue;
        }
        rows.push(SymtabEntry {
            start,
            end: start.saturating_add(size),
            symbol,
        });
    }
    rows.sort_by_key(|row| row.start);
    rows
}

fn parse_trace_tail_pcs(contents: &str) -> Vec<u32> {
    let mut pcs = Vec::new();
    for line in contents.lines() {
        let Some(rest) = line.strip_prefix("pc=") else {
            continue;
        };
        let mut parts = rest.split_whitespace();
        let Some(pc_text) = parts.next() else {
            continue;
        };
        if let Ok(pc) = pc_text.parse::<u32>() {
            pcs.push(pc);
        }
    }
    pcs
}

fn map_pc_to_symbol(pc: u32, symtab: &[SymtabEntry]) -> Option<&str> {
    let mut lo = 0usize;
    let mut hi = symtab.len();
    while lo < hi {
        let mid = (lo + hi) / 2;
        let row = &symtab[mid];
        if pc < row.start {
            hi = mid;
        } else if pc >= row.end {
            lo = mid + 1;
        } else {
            return Some(&row.symbol);
        }
    }
    None
}

fn json_u64(value: Option<&Value>) -> u64 {
    value.and_then(Value::as_u64).unwrap_or(0)
}

fn parse_observability_runtime_snapshot(contents: &str) -> Option<ObservabilityRuntimeSnapshot> {
    let root: Value = serde_json::from_str(contents).ok()?;
    let section = root
        .get("sections")
        .and_then(Value::as_array)?
        .iter()
        .find(|entry| entry.get("section").and_then(Value::as_str) == Some("runtime"))?;

    let unmapped_obj = section
        .get("unmapped_reason_coverage")
        .and_then(Value::as_object);
    let unmapped_reason = |key: &str| -> u64 {
        unmapped_obj
            .and_then(|obj| obj.get(key))
            .and_then(Value::as_u64)
            .unwrap_or(0)
    };

    let mut pc_ranges = Vec::new();
    if let Some(entries) = section.get("pc_map").and_then(Value::as_array) {
        for entry in entries {
            let start = entry.get("pc_start").and_then(Value::as_u64);
            let end = entry.get("pc_end").and_then(Value::as_u64);
            let (Some(start), Some(end)) = (start, end) else {
                continue;
            };
            if end <= start || end > u32::MAX as u64 {
                continue;
            }
            let func_name = entry
                .get("func_name")
                .and_then(Value::as_str)
                .unwrap_or("<unknown>")
                .to_string();
            let reason = entry
                .get("reason")
                .and_then(Value::as_str)
                .map(str::to_string);
            pc_ranges.push(ObservabilityPcRange {
                start: start as u32,
                end: end as u32,
                func_name,
                reason,
            });
        }
    }
    pc_ranges.sort_by_key(|range| range.start);

    Some(ObservabilityRuntimeSnapshot {
        section: section
            .get("section")
            .and_then(Value::as_str)
            .unwrap_or("runtime")
            .to_string(),
        schema_version: section
            .get("schema_version")
            .and_then(Value::as_str)
            .unwrap_or("-")
            .to_string(),
        section_bytes: json_u64(section.get("section_bytes")),
        code_bytes: json_u64(section.get("code_bytes")),
        data_bytes: json_u64(section.get("data_bytes")),
        embed_bytes: json_u64(section.get("embed_bytes")),
        mapped_code_bytes: json_u64(section.get("mapped_code_bytes")),
        unmapped_code_bytes: json_u64(section.get("unmapped_code_bytes")),
        unmapped_no_ir_inst: unmapped_reason("no_ir_inst"),
        unmapped_label_or_fixup_only: unmapped_reason("label_or_fixup_only"),
        unmapped_synthetic: unmapped_reason("synthetic"),
        unmapped_unknown: unmapped_reason("unknown"),
        pc_ranges,
    })
}

fn load_suite_observability_runtime(
    suite_dir: &Utf8PathBuf,
    suite: &str,
) -> (
    Vec<ObservabilityCoverageRow>,
    FxHashMap<String, Vec<ObservabilityPcRange>>,
) {
    let mut rows = Vec::new();
    let mut ranges_by_test: FxHashMap<String, Vec<ObservabilityPcRange>> = FxHashMap::default();
    let tests_dir = suite_dir.join("artifacts").join("tests");
    let Ok(entries) = std::fs::read_dir(&tests_dir) else {
        return (rows, ranges_by_test);
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let test = entry.file_name().to_string_lossy().to_string();
        let obs_path = path.join("sonatina").join("observability.json");
        let Ok(contents) = std::fs::read_to_string(&obs_path) else {
            continue;
        };
        let Some(snapshot) = parse_observability_runtime_snapshot(&contents) else {
            continue;
        };
        rows.push(ObservabilityCoverageRow {
            suite: suite.to_string(),
            test: test.clone(),
            section: snapshot.section,
            schema_version: snapshot.schema_version,
            section_bytes: snapshot.section_bytes,
            code_bytes: snapshot.code_bytes,
            data_bytes: snapshot.data_bytes,
            embed_bytes: snapshot.embed_bytes,
            mapped_code_bytes: snapshot.mapped_code_bytes,
            unmapped_code_bytes: snapshot.unmapped_code_bytes,
            unmapped_no_ir_inst: snapshot.unmapped_no_ir_inst,
            unmapped_label_or_fixup_only: snapshot.unmapped_label_or_fixup_only,
            unmapped_synthetic: snapshot.unmapped_synthetic,
            unmapped_unknown: snapshot.unmapped_unknown,
        });
        ranges_by_test.insert(test, snapshot.pc_ranges);
    }
    rows.sort_by(|a, b| a.test.cmp(&b.test).then_with(|| a.section.cmp(&b.section)));
    (rows, ranges_by_test)
}

fn map_pc_to_observability(
    pc: u32,
    ranges: &[ObservabilityPcRange],
) -> Option<&ObservabilityPcRange> {
    let mut lo = 0usize;
    let mut hi = ranges.len();
    while lo < hi {
        let mid = (lo + hi) / 2;
        let range = &ranges[mid];
        if pc < range.start {
            hi = mid;
        } else if pc >= range.end {
            lo = mid + 1;
        } else {
            return Some(range);
        }
    }
    None
}

fn resolve_observability_test_ranges<'a>(
    suite: &str,
    trace_test_name: &'a str,
    ranges_by_test: &'a FxHashMap<String, Vec<ObservabilityPcRange>>,
) -> Option<&'a [ObservabilityPcRange]> {
    let suite_prefixed = format!("{suite}__");
    let mut candidates: Vec<&str> = Vec::new();
    candidates.push(trace_test_name);
    if let Some(stripped) = trace_test_name.strip_prefix(&suite_prefixed) {
        candidates.push(stripped);
    }
    if let Some((_, suffix)) = trace_test_name.split_once("__") {
        candidates.push(suffix);
    }
    if let Some((_, suffix)) = trace_test_name.rsplit_once("__") {
        candidates.push(suffix);
    }

    let mut seen: Vec<&str> = Vec::new();
    for candidate in candidates {
        if seen.contains(&candidate) {
            continue;
        }
        seen.push(candidate);
        if let Some(ranges) = ranges_by_test.get(candidate) {
            return Some(ranges.as_slice());
        }
    }

    None
}

fn evm_runtime_metrics_from_bytes(bytes: &[u8]) -> EvmRuntimeMetrics {
    let mut metrics = EvmRuntimeMetrics {
        byte_len: bytes.len(),
        ..EvmRuntimeMetrics::default()
    };
    let mut idx = 0usize;
    while idx < bytes.len() {
        let op = bytes[idx];
        idx += 1;
        metrics.op_count += 1;
        match op {
            0x5f => metrics.push_ops += 1, // PUSH0
            0x50 => metrics.pop_ops += 1,
            0x51 => metrics.mload_ops += 1,
            0x52 => metrics.mstore_ops += 1,
            0x54 => metrics.sload_ops += 1,
            0x55 => metrics.sstore_ops += 1,
            0x56 => metrics.jump_ops += 1,
            0x57 => metrics.jumpi_ops += 1,
            0x5b => metrics.jumpdest_ops += 1,
            0x15 => metrics.iszero_ops += 1,
            0x20 => metrics.keccak_ops += 1,
            0x37 => metrics.calldatacopy_ops += 1,
            0x3e => metrics.returndatacopy_ops += 1,
            0x5e => metrics.mcopy_ops += 1,
            0xf1 => metrics.call_ops += 1,
            0xfa => metrics.staticcall_ops += 1,
            0xf3 => metrics.return_ops += 1,
            0xfd => metrics.revert_ops += 1,
            0x60..=0x7f => {
                metrics.push_ops += 1;
                let push_n = (op - 0x5f) as usize;
                idx = idx.saturating_add(push_n).min(bytes.len());
            }
            0x80..=0x8f => metrics.dup_ops += 1,
            0x90..=0x9f => metrics.swap_ops += 1,
            _ => {}
        }
    }
    metrics
}

fn evm_runtime_metrics_from_hex(runtime_hex: &str) -> Option<EvmRuntimeMetrics> {
    let bytes = hex::decode(runtime_hex.trim()).ok()?;
    Some(evm_runtime_metrics_from_bytes(&bytes))
}

fn usize_cell(value: Option<usize>) -> String {
    value
        .map(|v| v.to_string())
        .unwrap_or_else(|| "n/a".to_string())
}

fn u64_cell(value: Option<u64>) -> String {
    value
        .map(|v| v.to_string())
        .unwrap_or_else(|| "n/a".to_string())
}

fn ratio_cell_usize(numerator: Option<usize>, denominator: Option<usize>) -> String {
    match (numerator, denominator) {
        (Some(n), Some(d)) if d > 0 => format!("{:.2}", n as f64 / d as f64),
        _ => "n/a".to_string(),
    }
}

fn ratio_cell_u64(numerator: Option<u64>, denominator: Option<u64>) -> String {
    match (numerator, denominator) {
        (Some(n), Some(d)) if d > 0 => format!("{:.2}", n as f64 / d as f64),
        _ => "n/a".to_string(),
    }
}

fn stack_ops_pct_cell(metrics: Option<EvmRuntimeMetrics>) -> String {
    match metrics {
        Some(metrics) if metrics.op_count > 0 => {
            format!(
                "{:.2}%",
                (metrics.stack_ops_total() as f64 * 100.0) / (metrics.op_count as f64)
            )
        }
        _ => "n/a".to_string(),
    }
}

fn format_ratio_percent(numerator: usize, denominator: usize) -> String {
    if denominator == 0 {
        "n/a".to_string()
    } else {
        format!("{:.1}%", (numerator as f64 * 100.0) / (denominator as f64))
    }
}

fn format_delta_percent(delta: i128, baseline: u64) -> String {
    if baseline == 0 {
        "n/a".to_string()
    } else {
        let pct = (delta as f64 * 100.0) / (baseline as f64);
        format!("{pct:.2}%")
    }
}

fn gas_comparison_settings_text(opt_level: OptLevel) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "yul.primary.optimize={}\n",
        opt_level.yul_optimize()
    ));
    out.push_str("yul.compare.unoptimized.optimize=false\n");
    out.push_str("yul.compare.optimized.optimize=true\n");
    out.push_str(&format!("yul.solc.verify_runtime={YUL_VERIFY_RUNTIME}\n"));
    out.push_str(&format!("sonatina.opt_level={opt_level}\n"));
    out.push_str("sonatina.codegen.path=emit_test_module_sonatina (default)\n");
    out.push_str("measurement.call=RuntimeInstance::call_raw(empty calldata)\n");
    out.push_str("measurement.deploy=RuntimeInstance::deploy_tracked(init bytecode)\n");
    out.push_str("measurement.total=deploy_gas + call_gas (when both are available)\n");
    out
}

fn write_comparison_totals_rows(
    out: &mut String,
    baseline: &str,
    comparison: ComparisonTotals,
    tests_in_scope: usize,
) {
    out.push_str(&format!(
        "{baseline},compared_with_gas,{},{},{}\n",
        comparison.compared_with_gas,
        format_ratio_percent(comparison.compared_with_gas, comparison.compared_with_gas),
        format_ratio_percent(comparison.compared_with_gas, tests_in_scope)
    ));
    out.push_str(&format!(
        "{baseline},sonatina_lower,{},{},{}\n",
        comparison.sonatina_lower,
        format_ratio_percent(comparison.sonatina_lower, comparison.compared_with_gas),
        format_ratio_percent(comparison.sonatina_lower, tests_in_scope)
    ));
    out.push_str(&format!(
        "{baseline},sonatina_higher,{},{},{}\n",
        comparison.sonatina_higher,
        format_ratio_percent(comparison.sonatina_higher, comparison.compared_with_gas),
        format_ratio_percent(comparison.sonatina_higher, tests_in_scope)
    ));
    out.push_str(&format!(
        "{baseline},equal,{},{},{}\n",
        comparison.equal,
        format_ratio_percent(comparison.equal, comparison.compared_with_gas),
        format_ratio_percent(comparison.equal, tests_in_scope)
    ));
    out.push_str(&format!(
        "{baseline},incomplete,{},n/a,{}\n",
        comparison.incomplete,
        format_ratio_percent(comparison.incomplete, tests_in_scope)
    ));
}

fn write_gas_totals_csv(path: &Utf8PathBuf, totals: GasTotals) {
    let mut out = String::new();
    out.push_str("baseline,metric,count,pct_of_compared,pct_of_scope\n");
    out.push_str(&format!(
        "all,tests_in_scope,{},n/a,{}\n",
        totals.tests_in_scope,
        format_ratio_percent(totals.tests_in_scope, totals.tests_in_scope)
    ));
    write_comparison_totals_rows(
        &mut out,
        "yul_unopt",
        totals.vs_yul_unopt,
        totals.tests_in_scope,
    );
    write_comparison_totals_rows(
        &mut out,
        "yul_opt",
        totals.vs_yul_opt,
        totals.tests_in_scope,
    );

    let _ = std::fs::write(path, out);
}

fn write_magnitude_totals_rows(out: &mut String, baseline: &str, totals: DeltaMagnitudeTotals) {
    out.push_str(&format!(
        "{baseline},compared_with_gas,{}\n",
        totals.compared_with_gas
    ));
    out.push_str(&format!("{baseline},pct_rows,{}\n", totals.pct_rows));
    out.push_str(&format!(
        "{baseline},baseline_gas_sum,{}\n",
        totals.baseline_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},sonatina_gas_sum,{}\n",
        totals.sonatina_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},delta_gas_sum,{}\n",
        totals.delta_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},abs_delta_gas_sum,{}\n",
        totals.abs_delta_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},delta_pct_sum,{:.6}\n",
        totals.delta_pct_sum
    ));
    out.push_str(&format!(
        "{baseline},abs_delta_pct_sum,{:.6}\n",
        totals.abs_delta_pct_sum
    ));
}

fn write_gas_magnitude_csv(path: &Utf8PathBuf, totals: GasMagnitudeTotals) {
    let mut out = String::new();
    out.push_str("baseline,metric,value\n");
    write_magnitude_totals_rows(&mut out, "yul_unopt", totals.vs_yul_unopt);
    write_magnitude_totals_rows(&mut out, "yul_opt", totals.vs_yul_opt);
    let _ = std::fs::write(path, out);
}

fn write_gas_breakdown_magnitude_component_rows(
    out: &mut String,
    baseline: &str,
    component: &str,
    totals: DeltaMagnitudeTotals,
) {
    out.push_str(&format!(
        "{baseline},{component},compared_with_gas,{}\n",
        totals.compared_with_gas
    ));
    out.push_str(&format!(
        "{baseline},{component},pct_rows,{}\n",
        totals.pct_rows
    ));
    out.push_str(&format!(
        "{baseline},{component},baseline_gas_sum,{}\n",
        totals.baseline_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},{component},sonatina_gas_sum,{}\n",
        totals.sonatina_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},{component},delta_gas_sum,{}\n",
        totals.delta_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},{component},abs_delta_gas_sum,{}\n",
        totals.abs_delta_gas_sum
    ));
    out.push_str(&format!(
        "{baseline},{component},delta_pct_sum,{:.6}\n",
        totals.delta_pct_sum
    ));
    out.push_str(&format!(
        "{baseline},{component},abs_delta_pct_sum,{:.6}\n",
        totals.abs_delta_pct_sum
    ));
}

fn write_gas_breakdown_magnitude_csv(
    path: &Utf8PathBuf,
    call_totals: GasMagnitudeTotals,
    deploy_totals: GasMagnitudeTotals,
    total_totals: GasMagnitudeTotals,
) {
    let mut out = String::new();
    out.push_str("baseline,component,metric,value\n");
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_unopt",
        "call",
        call_totals.vs_yul_unopt,
    );
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_opt",
        "call",
        call_totals.vs_yul_opt,
    );
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_unopt",
        "deploy",
        deploy_totals.vs_yul_unopt,
    );
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_opt",
        "deploy",
        deploy_totals.vs_yul_opt,
    );
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_unopt",
        "total",
        total_totals.vs_yul_unopt,
    );
    write_gas_breakdown_magnitude_component_rows(
        &mut out,
        "yul_opt",
        "total",
        total_totals.vs_yul_opt,
    );
    let _ = std::fs::write(path, out);
}

fn write_opcode_magnitude_rows(out: &mut String, side: &str, totals: OpcodeAggregateTotals) {
    out.push_str(&format!("{side},steps_sum,{}\n", totals.steps_sum));
    out.push_str(&format!(
        "{side},runtime_bytes_sum,{}\n",
        totals.runtime_bytes_sum
    ));
    out.push_str(&format!(
        "{side},runtime_ops_sum,{}\n",
        totals.runtime_ops_sum
    ));
    out.push_str(&format!("{side},swap_ops_sum,{}\n", totals.swap_ops_sum));
    out.push_str(&format!("{side},pop_ops_sum,{}\n", totals.pop_ops_sum));
    out.push_str(&format!("{side},jump_ops_sum,{}\n", totals.jump_ops_sum));
    out.push_str(&format!("{side},jumpi_ops_sum,{}\n", totals.jumpi_ops_sum));
    out.push_str(&format!(
        "{side},iszero_ops_sum,{}\n",
        totals.iszero_ops_sum
    ));
    out.push_str(&format!(
        "{side},mem_rw_ops_sum,{}\n",
        totals.mem_rw_ops_sum
    ));
    out.push_str(&format!(
        "{side},storage_rw_ops_sum,{}\n",
        totals.storage_rw_ops_sum
    ));
    out.push_str(&format!("{side},mload_ops_sum,{}\n", totals.mload_ops_sum));
    out.push_str(&format!(
        "{side},mstore_ops_sum,{}\n",
        totals.mstore_ops_sum
    ));
    out.push_str(&format!("{side},sload_ops_sum,{}\n", totals.sload_ops_sum));
    out.push_str(&format!(
        "{side},sstore_ops_sum,{}\n",
        totals.sstore_ops_sum
    ));
    out.push_str(&format!(
        "{side},keccak_ops_sum,{}\n",
        totals.keccak_ops_sum
    ));
    out.push_str(&format!(
        "{side},call_family_ops_sum,{}\n",
        totals.call_family_ops_sum
    ));
    out.push_str(&format!("{side},copy_ops_sum,{}\n", totals.copy_ops_sum));
}

fn write_opcode_magnitude_csv(path: &Utf8PathBuf, totals: OpcodeMagnitudeTotals) {
    let mut out = String::new();
    out.push_str("side,metric,value\n");
    out.push_str(&format!(
        "all,compared_with_metrics,{}\n",
        totals.compared_with_metrics
    ));
    write_opcode_magnitude_rows(&mut out, "yul_opt", totals.yul_opt);
    write_opcode_magnitude_rows(&mut out, "sonatina", totals.sonatina);
    let _ = std::fs::write(path, out);
}

fn write_gas_hotspots_csv(path: &Utf8PathBuf, rows: &[GasHotspotRow]) {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| b.delta_vs_yul_opt.cmp(&a.delta_vs_yul_opt));

    let total_delta: i128 = sorted.iter().map(|row| row.delta_vs_yul_opt).sum();
    let mut cumulative: i128 = 0;

    let mut out = String::new();
    out.push_str("rank,suite,test,symbol,yul_opt_gas,sonatina_gas,delta_vs_yul_opt,delta_vs_yul_opt_pct,share_of_total_delta_pct,cumulative_share_pct\n");
    for (idx, row) in sorted.into_iter().enumerate() {
        cumulative += row.delta_vs_yul_opt;
        let share = ratio_percent_i128(row.delta_vs_yul_opt, total_delta)
            .map(|v| format!("{v:.2}%"))
            .unwrap_or_else(|| "n/a".to_string());
        let cumulative_share = ratio_percent_i128(cumulative, total_delta)
            .map(|v| format!("{v:.2}%"))
            .unwrap_or_else(|| "n/a".to_string());
        out.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{}\n",
            idx + 1,
            csv_escape(&row.suite),
            csv_escape(&row.test),
            csv_escape(&row.symbol),
            csv_escape(&u64_cell(row.yul_opt_gas)),
            csv_escape(&u64_cell(row.sonatina_gas)),
            row.delta_vs_yul_opt,
            csv_escape(&row.delta_vs_yul_opt_pct),
            csv_escape(&share),
            csv_escape(&cumulative_share)
        ));
    }
    let _ = std::fs::write(path, out);
}

fn write_suite_delta_summary_csv(path: &Utf8PathBuf, suite_rollup: &[(String, SuiteDeltaTotals)]) {
    let total_delta: i128 = suite_rollup
        .iter()
        .map(|(_, totals)| totals.delta_vs_yul_opt_sum)
        .sum();
    let mut out = String::new();
    out.push_str("suite,tests_with_delta,delta_vs_yul_opt_sum,avg_delta_vs_yul_opt,share_of_total_delta_pct\n");
    for (suite, totals) in suite_rollup {
        let avg = if totals.tests_with_delta == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}",
                totals.delta_vs_yul_opt_sum as f64 / totals.tests_with_delta as f64
            )
        };
        let share = ratio_percent_i128(totals.delta_vs_yul_opt_sum, total_delta)
            .map(|v| format!("{v:.2}%"))
            .unwrap_or_else(|| "n/a".to_string());
        out.push_str(&format!(
            "{},{},{},{},{}\n",
            csv_escape(suite),
            totals.tests_with_delta,
            totals.delta_vs_yul_opt_sum,
            csv_escape(&avg),
            csv_escape(&share),
        ));
    }
    let _ = std::fs::write(path, out);
}

fn write_trace_symbol_hotspots_csv(path: &Utf8PathBuf, rows: &[TraceSymbolHotspotRow]) {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| {
        b.steps_in_symbol
            .cmp(&a.steps_in_symbol)
            .then_with(|| a.suite.cmp(&b.suite))
            .then_with(|| a.test.cmp(&b.test))
    });

    let mut out = String::new();
    out.push_str("suite,test,symbol,tail_steps_total,tail_steps_mapped,steps_in_symbol,symbol_share_of_tail_pct\n");
    for row in sorted {
        let pct = if row.tail_steps_total == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.steps_in_symbol as f64 * 100.0) / row.tail_steps_total as f64
            )
        };
        out.push_str(&format!(
            "{},{},{},{},{},{},{}\n",
            csv_escape(&row.suite),
            csv_escape(&row.test),
            csv_escape(&row.symbol),
            row.tail_steps_total,
            row.tail_steps_mapped,
            row.steps_in_symbol,
            csv_escape(&pct)
        ));
    }
    let _ = std::fs::write(path, out);
}

fn write_observability_coverage_csv(path: &Utf8PathBuf, rows: &[ObservabilityCoverageRow]) {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| {
        a.suite
            .cmp(&b.suite)
            .then_with(|| a.test.cmp(&b.test))
            .then_with(|| a.section.cmp(&b.section))
    });

    let mut out = String::new();
    out.push_str("suite,test,section,schema_version,section_bytes,code_bytes,data_bytes,embed_bytes,mapped_code_bytes,unmapped_code_bytes,mapped_code_pct,unmapped_code_pct,unmapped_no_ir_inst,unmapped_label_or_fixup_only,unmapped_synthetic,unmapped_unknown\n");
    for row in sorted {
        let mapped_pct = if row.code_bytes == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.mapped_code_bytes as f64 * 100.0) / row.code_bytes as f64
            )
        };
        let unmapped_pct = if row.code_bytes == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.unmapped_code_bytes as f64 * 100.0) / row.code_bytes as f64
            )
        };
        out.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}\n",
            csv_escape(&row.suite),
            csv_escape(&row.test),
            csv_escape(&row.section),
            csv_escape(&row.schema_version),
            row.section_bytes,
            row.code_bytes,
            row.data_bytes,
            row.embed_bytes,
            row.mapped_code_bytes,
            row.unmapped_code_bytes,
            csv_escape(&mapped_pct),
            csv_escape(&unmapped_pct),
            row.unmapped_no_ir_inst,
            row.unmapped_label_or_fixup_only,
            row.unmapped_synthetic,
            row.unmapped_unknown,
        ));
    }
    let _ = std::fs::write(path, out);
}

fn write_trace_observability_hotspots_csv(
    path: &Utf8PathBuf,
    rows: &[TraceObservabilityHotspotRow],
) {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| {
        b.steps_in_bucket
            .cmp(&a.steps_in_bucket)
            .then_with(|| a.suite.cmp(&b.suite))
            .then_with(|| a.test.cmp(&b.test))
            .then_with(|| a.function.cmp(&b.function))
            .then_with(|| a.reason.cmp(&b.reason))
    });

    let mut out = String::new();
    out.push_str("suite,test,function,reason,tail_steps_total,tail_steps_mapped,steps_in_bucket,bucket_share_of_tail_pct\n");
    for row in sorted {
        let pct = if row.tail_steps_total == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.steps_in_bucket as f64 * 100.0) / row.tail_steps_total as f64
            )
        };
        out.push_str(&format!(
            "{},{},{},{},{},{},{},{}\n",
            csv_escape(&row.suite),
            csv_escape(&row.test),
            csv_escape(&row.function),
            csv_escape(&row.reason),
            row.tail_steps_total,
            row.tail_steps_mapped,
            row.steps_in_bucket,
            csv_escape(&pct),
        ));
    }
    let _ = std::fs::write(path, out);
}

fn parse_gas_totals_csv(contents: &str) -> GasTotals {
    let mut totals = GasTotals::default();
    for (idx, line) in contents.lines().enumerate() {
        if idx == 0 || line.trim().is_empty() {
            continue;
        }
        let mut parts = line.splitn(5, ',');
        let baseline = parts.next().unwrap_or_default().trim();
        let metric = parts.next().unwrap_or_default().trim();
        let count = parts
            .next()
            .and_then(|raw| raw.trim().parse::<usize>().ok())
            .unwrap_or(0);
        match (baseline, metric) {
            ("all", "tests_in_scope") => totals.tests_in_scope = count,
            ("yul_unopt", "compared_with_gas") => totals.vs_yul_unopt.compared_with_gas = count,
            ("yul_unopt", "sonatina_lower") => totals.vs_yul_unopt.sonatina_lower = count,
            ("yul_unopt", "sonatina_higher") => totals.vs_yul_unopt.sonatina_higher = count,
            ("yul_unopt", "equal") => totals.vs_yul_unopt.equal = count,
            ("yul_unopt", "incomplete") => totals.vs_yul_unopt.incomplete = count,
            ("yul_opt", "compared_with_gas") => totals.vs_yul_opt.compared_with_gas = count,
            ("yul_opt", "sonatina_lower") => totals.vs_yul_opt.sonatina_lower = count,
            ("yul_opt", "sonatina_higher") => totals.vs_yul_opt.sonatina_higher = count,
            ("yul_opt", "equal") => totals.vs_yul_opt.equal = count,
            ("yul_opt", "incomplete") => totals.vs_yul_opt.incomplete = count,
            _ => {}
        }
    }
    totals
}

fn parse_gas_magnitude_csv(contents: &str) -> GasMagnitudeTotals {
    let mut totals = GasMagnitudeTotals::default();
    for (idx, line) in contents.lines().enumerate() {
        if idx == 0 || line.trim().is_empty() {
            continue;
        }
        let mut parts = line.splitn(3, ',');
        let baseline = parts.next().unwrap_or_default().trim();
        let metric = parts.next().unwrap_or_default().trim();
        let value = parts.next().unwrap_or_default().trim();
        let target = match baseline {
            "yul_unopt" => &mut totals.vs_yul_unopt,
            "yul_opt" => &mut totals.vs_yul_opt,
            _ => continue,
        };
        match metric {
            "compared_with_gas" => {
                target.compared_with_gas = value.parse::<usize>().unwrap_or(0);
            }
            "pct_rows" => {
                target.pct_rows = value.parse::<usize>().unwrap_or(0);
            }
            "baseline_gas_sum" => {
                target.baseline_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "sonatina_gas_sum" => {
                target.sonatina_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "delta_gas_sum" => {
                target.delta_gas_sum = value.parse::<i128>().unwrap_or(0);
            }
            "abs_delta_gas_sum" => {
                target.abs_delta_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "delta_pct_sum" => {
                target.delta_pct_sum = value.parse::<f64>().unwrap_or(0.0);
            }
            "abs_delta_pct_sum" => {
                target.abs_delta_pct_sum = value.parse::<f64>().unwrap_or(0.0);
            }
            _ => {}
        }
    }
    totals
}

fn parse_gas_breakdown_magnitude_csv(
    contents: &str,
) -> (GasMagnitudeTotals, GasMagnitudeTotals, GasMagnitudeTotals) {
    let mut call_totals = GasMagnitudeTotals::default();
    let mut deploy_totals = GasMagnitudeTotals::default();
    let mut total_totals = GasMagnitudeTotals::default();
    for (idx, line) in contents.lines().enumerate() {
        if idx == 0 || line.trim().is_empty() {
            continue;
        }
        let mut parts = line.splitn(4, ',');
        let baseline = parts.next().unwrap_or_default().trim();
        let component = parts.next().unwrap_or_default().trim();
        let metric = parts.next().unwrap_or_default().trim();
        let value = parts.next().unwrap_or_default().trim();

        let component_totals = match component {
            "call" => &mut call_totals,
            "deploy" => &mut deploy_totals,
            "total" => &mut total_totals,
            _ => continue,
        };

        let target = match baseline {
            "yul_unopt" => &mut component_totals.vs_yul_unopt,
            "yul_opt" => &mut component_totals.vs_yul_opt,
            _ => continue,
        };

        match metric {
            "compared_with_gas" => {
                target.compared_with_gas = value.parse::<usize>().unwrap_or(0);
            }
            "pct_rows" => {
                target.pct_rows = value.parse::<usize>().unwrap_or(0);
            }
            "baseline_gas_sum" => {
                target.baseline_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "sonatina_gas_sum" => {
                target.sonatina_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "delta_gas_sum" => {
                target.delta_gas_sum = value.parse::<i128>().unwrap_or(0);
            }
            "abs_delta_gas_sum" => {
                target.abs_delta_gas_sum = value.parse::<u128>().unwrap_or(0);
            }
            "delta_pct_sum" => {
                target.delta_pct_sum = value.parse::<f64>().unwrap_or(0.0);
            }
            "abs_delta_pct_sum" => {
                target.abs_delta_pct_sum = value.parse::<f64>().unwrap_or(0.0);
            }
            _ => {}
        }
    }
    (call_totals, deploy_totals, total_totals)
}

fn parse_gas_opcode_magnitude_csv(contents: &str) -> OpcodeMagnitudeTotals {
    let mut totals = OpcodeMagnitudeTotals::default();
    for (idx, line) in contents.lines().enumerate() {
        if idx == 0 || line.trim().is_empty() {
            continue;
        }
        let mut parts = line.splitn(3, ',');
        let side = parts.next().unwrap_or_default().trim();
        let metric = parts.next().unwrap_or_default().trim();
        let value = parts.next().unwrap_or_default().trim();
        if side == "all" && metric == "compared_with_metrics" {
            totals.compared_with_metrics = value.parse::<usize>().unwrap_or(0);
            continue;
        }
        let target = match side {
            "yul_opt" => &mut totals.yul_opt,
            "sonatina" => &mut totals.sonatina,
            _ => continue,
        };
        let parsed = value.parse::<u128>().unwrap_or(0);
        match metric {
            "steps_sum" => target.steps_sum = parsed,
            "runtime_bytes_sum" => target.runtime_bytes_sum = parsed,
            "runtime_ops_sum" => target.runtime_ops_sum = parsed,
            "swap_ops_sum" => target.swap_ops_sum = parsed,
            "pop_ops_sum" => target.pop_ops_sum = parsed,
            "jump_ops_sum" => target.jump_ops_sum = parsed,
            "jumpi_ops_sum" => target.jumpi_ops_sum = parsed,
            "iszero_ops_sum" => target.iszero_ops_sum = parsed,
            "mem_rw_ops_sum" => target.mem_rw_ops_sum = parsed,
            "storage_rw_ops_sum" => target.storage_rw_ops_sum = parsed,
            "mload_ops_sum" => target.mload_ops_sum = parsed,
            "mstore_ops_sum" => target.mstore_ops_sum = parsed,
            "sload_ops_sum" => target.sload_ops_sum = parsed,
            "sstore_ops_sum" => target.sstore_ops_sum = parsed,
            "keccak_ops_sum" => target.keccak_ops_sum = parsed,
            "call_family_ops_sum" => target.call_family_ops_sum = parsed,
            "copy_ops_sum" => target.copy_ops_sum = parsed,
            _ => {}
        }
    }
    totals
}

fn measurement_status(has_metadata: bool, measurement: Option<&GasMeasurement>) -> &'static str {
    if !has_metadata {
        return "missing";
    }
    let Some(measurement) = measurement else {
        return "not_run";
    };
    if measurement.passed {
        if measurement.gas_used.is_some() {
            "ok"
        } else {
            "ok_no_gas"
        }
    } else {
        "failed"
    }
}

fn record_delta(
    baseline_gas: Option<u64>,
    sonatina_gas: Option<u64>,
    totals: &mut ComparisonTotals,
) -> (String, String) {
    match (baseline_gas, sonatina_gas) {
        (Some(baseline_gas), Some(sonatina_gas)) => {
            totals.compared_with_gas += 1;
            let diff = sonatina_gas as i128 - baseline_gas as i128;
            if diff < 0 {
                totals.sonatina_lower += 1;
            } else if diff > 0 {
                totals.sonatina_higher += 1;
            } else {
                totals.equal += 1;
            }
            (diff.to_string(), format_delta_percent(diff, baseline_gas))
        }
        _ => {
            totals.incomplete += 1;
            ("n/a".to_string(), "n/a".to_string())
        }
    }
}

fn delta_cells(baseline_gas: Option<u64>, sonatina_gas: Option<u64>) -> (String, String) {
    match (baseline_gas, sonatina_gas) {
        (Some(baseline_gas), Some(sonatina_gas)) => {
            let diff = sonatina_gas as i128 - baseline_gas as i128;
            (diff.to_string(), format_delta_percent(diff, baseline_gas))
        }
        _ => ("n/a".to_string(), "n/a".to_string()),
    }
}

fn record_delta_magnitude(
    baseline_gas: Option<u64>,
    sonatina_gas: Option<u64>,
    totals: &mut DeltaMagnitudeTotals,
) {
    let (Some(baseline_gas), Some(sonatina_gas)) = (baseline_gas, sonatina_gas) else {
        return;
    };

    let delta_gas = sonatina_gas as i128 - baseline_gas as i128;
    totals.compared_with_gas += 1;
    totals.baseline_gas_sum += baseline_gas as u128;
    totals.sonatina_gas_sum += sonatina_gas as u128;
    totals.delta_gas_sum += delta_gas;
    totals.abs_delta_gas_sum += if delta_gas < 0 {
        (-delta_gas) as u128
    } else {
        delta_gas as u128
    };

    if baseline_gas > 0 {
        let delta_pct = (delta_gas as f64 * 100.0) / baseline_gas as f64;
        totals.pct_rows += 1;
        totals.delta_pct_sum += delta_pct;
        totals.abs_delta_pct_sum += delta_pct.abs();
    }
}

fn append_comparison_summary(
    out: &mut String,
    label: &str,
    totals: ComparisonTotals,
    tests_in_scope: usize,
) {
    out.push_str(&format!("### {label}\n\n"));
    out.push_str(&format!(
        "- compared_with_gas: {} ({})\n",
        totals.compared_with_gas,
        format_ratio_percent(totals.compared_with_gas, tests_in_scope)
    ));
    out.push_str(&format!(
        "- sonatina_lower: {} ({})\n",
        totals.sonatina_lower,
        format_ratio_percent(totals.sonatina_lower, totals.compared_with_gas)
    ));
    out.push_str(&format!(
        "- sonatina_higher: {} ({})\n",
        totals.sonatina_higher,
        format_ratio_percent(totals.sonatina_higher, totals.compared_with_gas)
    ));
    out.push_str(&format!(
        "- equal: {} ({})\n",
        totals.equal,
        format_ratio_percent(totals.equal, totals.compared_with_gas)
    ));
    out.push_str(&format!(
        "- incomplete: {} ({})\n\n",
        totals.incomplete,
        format_ratio_percent(totals.incomplete, tests_in_scope)
    ));
}

fn format_percent_cell(value: Option<f64>) -> String {
    value
        .map(|v| format!("{v:.2}%"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn format_float_cell(value: Option<f64>) -> String {
    value
        .map(|v| format!("{v:.2}"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn append_magnitude_summary(out: &mut String, label: &str, totals: DeltaMagnitudeTotals) {
    out.push_str(&format!("### {label}\n\n"));
    out.push_str(&format!(
        "- compared_with_gas: {}\n",
        totals.compared_with_gas
    ));
    out.push_str(&format!(
        "- baseline_gas_sum: {}\n",
        totals.baseline_gas_sum
    ));
    out.push_str(&format!(
        "- sonatina_gas_sum: {}\n",
        totals.sonatina_gas_sum
    ));
    out.push_str(&format!("- total_delta_gas: {}\n", totals.delta_gas_sum));
    out.push_str(&format!(
        "- weighted_delta_pct: {}\n",
        format_percent_cell(totals.weighted_delta_pct())
    ));
    out.push_str(&format!(
        "- mean_delta_gas: {}\n",
        format_float_cell(totals.mean_delta_gas())
    ));
    out.push_str(&format!(
        "- mean_abs_delta_gas: {}\n",
        format_float_cell(totals.mean_abs_delta_gas())
    ));
    out.push_str(&format!(
        "- mean_delta_pct: {}\n",
        format_percent_cell(totals.mean_delta_pct())
    ));
    out.push_str(&format!(
        "- mean_abs_delta_pct: {}\n\n",
        format_percent_cell(totals.mean_abs_delta_pct())
    ));
}

fn delta_pct_from_sums(baseline_sum: u128, sonatina_sum: u128) -> Option<f64> {
    if baseline_sum == 0 {
        None
    } else {
        Some((sonatina_sum as f64 - baseline_sum as f64) * 100.0 / baseline_sum as f64)
    }
}

fn append_opcode_delta_metric(
    out: &mut String,
    label: &str,
    baseline_sum: u128,
    sonatina_sum: u128,
) {
    let delta = sonatina_sum as i128 - baseline_sum as i128;
    out.push_str(&format!(
        "- {label}: {} -> {} (delta {}, {})\n",
        baseline_sum,
        sonatina_sum,
        delta,
        format_percent_cell(delta_pct_from_sums(baseline_sum, sonatina_sum))
    ));
}

fn append_opcode_magnitude_summary(out: &mut String, totals: OpcodeMagnitudeTotals) {
    out.push_str("## Opcode Aggregate Delta Metrics (vs Yul optimized)\n\n");
    out.push_str(&format!(
        "- compared_with_metrics: {}\n",
        totals.compared_with_metrics
    ));
    append_opcode_delta_metric(
        out,
        "steps_sum",
        totals.yul_opt.steps_sum,
        totals.sonatina.steps_sum,
    );
    append_opcode_delta_metric(
        out,
        "runtime_bytes_sum",
        totals.yul_opt.runtime_bytes_sum,
        totals.sonatina.runtime_bytes_sum,
    );
    append_opcode_delta_metric(
        out,
        "runtime_ops_sum",
        totals.yul_opt.runtime_ops_sum,
        totals.sonatina.runtime_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "swap_ops_sum",
        totals.yul_opt.swap_ops_sum,
        totals.sonatina.swap_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "pop_ops_sum",
        totals.yul_opt.pop_ops_sum,
        totals.sonatina.pop_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "jump_ops_sum",
        totals.yul_opt.jump_ops_sum,
        totals.sonatina.jump_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "jumpi_ops_sum",
        totals.yul_opt.jumpi_ops_sum,
        totals.sonatina.jumpi_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "iszero_ops_sum",
        totals.yul_opt.iszero_ops_sum,
        totals.sonatina.iszero_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "mem_rw_ops_sum",
        totals.yul_opt.mem_rw_ops_sum,
        totals.sonatina.mem_rw_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "storage_rw_ops_sum",
        totals.yul_opt.storage_rw_ops_sum,
        totals.sonatina.storage_rw_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "mload_ops_sum",
        totals.yul_opt.mload_ops_sum,
        totals.sonatina.mload_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "mstore_ops_sum",
        totals.yul_opt.mstore_ops_sum,
        totals.sonatina.mstore_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "sload_ops_sum",
        totals.yul_opt.sload_ops_sum,
        totals.sonatina.sload_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "sstore_ops_sum",
        totals.yul_opt.sstore_ops_sum,
        totals.sonatina.sstore_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "keccak_ops_sum",
        totals.yul_opt.keccak_ops_sum,
        totals.sonatina.keccak_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "call_family_ops_sum",
        totals.yul_opt.call_family_ops_sum,
        totals.sonatina.call_family_ops_sum,
    );
    append_opcode_delta_metric(
        out,
        "copy_ops_sum",
        totals.yul_opt.copy_ops_sum,
        totals.sonatina.copy_ops_sum,
    );
    out.push('\n');
}

fn ratio_percent_i128(numerator: i128, denominator: i128) -> Option<f64> {
    if denominator == 0 {
        None
    } else {
        Some((numerator as f64 * 100.0) / denominator as f64)
    }
}

fn append_opcode_inflation_attribution(out: &mut String, totals: OpcodeMagnitudeTotals) {
    let runtime_ops_delta =
        totals.sonatina.runtime_ops_sum as i128 - totals.yul_opt.runtime_ops_sum as i128;
    let swap_delta = totals.sonatina.swap_ops_sum as i128 - totals.yul_opt.swap_ops_sum as i128;
    let pop_delta = totals.sonatina.pop_ops_sum as i128 - totals.yul_opt.pop_ops_sum as i128;
    let jump_delta = totals.sonatina.jump_ops_sum as i128 - totals.yul_opt.jump_ops_sum as i128;
    let jumpi_delta = totals.sonatina.jumpi_ops_sum as i128 - totals.yul_opt.jumpi_ops_sum as i128;
    let iszero_delta =
        totals.sonatina.iszero_ops_sum as i128 - totals.yul_opt.iszero_ops_sum as i128;
    let mem_rw_delta =
        totals.sonatina.mem_rw_ops_sum as i128 - totals.yul_opt.mem_rw_ops_sum as i128;
    let storage_rw_delta =
        totals.sonatina.storage_rw_ops_sum as i128 - totals.yul_opt.storage_rw_ops_sum as i128;

    let swap_pop_delta = swap_delta + pop_delta;
    let control_delta = jump_delta + jumpi_delta;
    let mem_storage_delta = mem_rw_delta + storage_rw_delta;
    let stack_control_delta = swap_pop_delta + control_delta;

    out.push_str("## Inflation Attribution Snapshot\n\n");
    out.push_str(&format!("- runtime_ops_delta: {runtime_ops_delta}\n"));
    out.push_str(&format!(
        "- swap_pop_delta: {} ({})\n",
        swap_pop_delta,
        format_percent_cell(ratio_percent_i128(swap_pop_delta, runtime_ops_delta))
    ));
    out.push_str(&format!(
        "- control_flow_delta (jump+jumpi): {} ({})\n",
        control_delta,
        format_percent_cell(ratio_percent_i128(control_delta, runtime_ops_delta))
    ));
    out.push_str(&format!(
        "- bool_normalization_delta (iszero): {} ({})\n",
        iszero_delta,
        format_percent_cell(ratio_percent_i128(iszero_delta, runtime_ops_delta))
    ));
    out.push_str(&format!(
        "- stack_control_delta (swap+pop+jump+jumpi): {} ({})\n",
        stack_control_delta,
        format_percent_cell(ratio_percent_i128(stack_control_delta, runtime_ops_delta))
    ));
    out.push_str(&format!(
        "- mem_storage_delta (mem_rw+storage_rw): {} ({})\n",
        mem_storage_delta,
        format_percent_cell(ratio_percent_i128(mem_storage_delta, runtime_ops_delta))
    ));
    out.push('\n');
}

fn append_deployment_attribution_summary(
    out: &mut String,
    heading: &str,
    totals: DeploymentGasAttributionTotals,
) {
    out.push_str(&format!("## {heading}\n\n"));
    out.push_str(&format!(
        "- compared_with_profile: {}\n",
        totals.compared_with_profile
    ));
    append_opcode_delta_metric(
        out,
        "step_total_gas_sum (vs Yul optimized)",
        totals.yul_opt_step_total_gas,
        totals.sonatina_step_total_gas,
    );
    append_opcode_delta_metric(
        out,
        "create_opcode_gas_sum (vs Yul optimized)",
        totals.yul_opt_create_opcode_gas,
        totals.sonatina_create_opcode_gas,
    );
    append_opcode_delta_metric(
        out,
        "create2_opcode_gas_sum (vs Yul optimized)",
        totals.yul_opt_create2_opcode_gas,
        totals.sonatina_create2_opcode_gas,
    );
    append_opcode_delta_metric(
        out,
        "constructor_frame_gas_sum (vs Yul optimized)",
        totals.yul_opt_constructor_frame_gas,
        totals.sonatina_constructor_frame_gas,
    );
    append_opcode_delta_metric(
        out,
        "non_constructor_frame_gas_sum (vs Yul optimized)",
        totals.yul_opt_non_constructor_frame_gas,
        totals.sonatina_non_constructor_frame_gas,
    );
    out.push('\n');
}

fn append_hotspot_summary(
    out: &mut String,
    hotspots: &[GasHotspotRow],
    suite_rollup: &[(String, SuiteDeltaTotals)],
) {
    let total_delta: i128 = hotspots.iter().map(|row| row.delta_vs_yul_opt).sum();
    let mut sorted = hotspots.to_vec();
    sorted.sort_by(|a, b| b.delta_vs_yul_opt.cmp(&a.delta_vs_yul_opt));

    out.push_str("## Top Gas Regressions (vs Yul optimized)\n\n");
    out.push_str(&format!("- rows_with_delta: {}\n", sorted.len()));
    out.push_str(&format!("- total_delta_vs_yul_opt: {}\n\n", total_delta));
    out.push_str("| rank | suite | test | delta_vs_yul_opt | pct_vs_yul_opt | share_of_total |\n");
    out.push_str("| ---: | --- | --- | ---: | ---: | ---: |\n");

    let mut cumulative: i128 = 0;
    for (idx, row) in sorted.iter().take(10).enumerate() {
        cumulative += row.delta_vs_yul_opt;
        let share = format_percent_cell(ratio_percent_i128(row.delta_vs_yul_opt, total_delta));
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} |\n",
            idx + 1,
            row.suite,
            row.test,
            row.delta_vs_yul_opt,
            row.delta_vs_yul_opt_pct,
            share
        ));
    }
    out.push_str(&format!(
        "\n- top_10_cumulative_share: {}\n\n",
        format_percent_cell(ratio_percent_i128(cumulative, total_delta))
    ));

    out.push_str("## Suite Delta Rollup (vs Yul optimized)\n\n");
    out.push_str("| suite | tests_with_delta | delta_vs_yul_opt_sum | avg_delta_vs_yul_opt | share_of_total |\n");
    out.push_str("| --- | ---: | ---: | ---: | ---: |\n");
    for (suite, totals) in suite_rollup.iter().take(10) {
        let avg = if totals.tests_with_delta == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}",
                totals.delta_vs_yul_opt_sum as f64 / totals.tests_with_delta as f64
            )
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} |\n",
            suite,
            totals.tests_with_delta,
            totals.delta_vs_yul_opt_sum,
            avg,
            format_percent_cell(ratio_percent_i128(totals.delta_vs_yul_opt_sum, total_delta))
        ));
    }
    out.push('\n');
}

fn append_trace_symbol_hotspots_summary(out: &mut String, rows: &[TraceSymbolHotspotRow]) {
    if rows.is_empty() {
        return;
    }
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| {
        b.steps_in_symbol
            .cmp(&a.steps_in_symbol)
            .then_with(|| a.suite.cmp(&b.suite))
            .then_with(|| a.test.cmp(&b.test))
    });

    out.push_str("## Tail Trace Symbol Attribution (Sonatina)\n\n");
    out.push_str(
        "Sampled from each suite trace artifact (`debug/*.evm_trace.txt`), which keeps the last N steps.\n\n",
    );
    out.push_str("| rank | suite | test | symbol | steps_in_symbol | symbol_share_of_tail |\n");
    out.push_str("| ---: | --- | --- | --- | ---: | ---: |\n");
    for (idx, row) in sorted.iter().take(12).enumerate() {
        let pct = if row.tail_steps_total == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.steps_in_symbol as f64 * 100.0) / row.tail_steps_total as f64
            )
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} |\n",
            idx + 1,
            row.suite,
            row.test,
            row.symbol,
            row.steps_in_symbol,
            pct
        ));
    }
    out.push('\n');
}

fn append_observability_coverage_summary(
    out: &mut String,
    rows: &[ObservabilityCoverageRow],
    totals: ObservabilityCoverageTotals,
) {
    if rows.is_empty() {
        return;
    }

    let mut test_keys: FxHashSet<(String, String)> = FxHashSet::default();
    for row in rows {
        test_keys.insert((row.suite.clone(), row.test.clone()));
    }

    out.push_str("## Sonatina Observability Coverage\n\n");
    out.push_str(&format!("- observed_sections: {}\n", rows.len()));
    out.push_str(&format!("- observed_tests: {}\n", test_keys.len()));
    out.push_str(&format!("- code_bytes_total: {}\n", totals.code_bytes));
    out.push_str(&format!(
        "- mapped_code_bytes_total: {}\n",
        totals.mapped_code_bytes
    ));
    out.push_str(&format!(
        "- unmapped_code_bytes_total: {}\n",
        totals.unmapped_code_bytes
    ));
    let mapped_pct = if totals.code_bytes == 0 {
        "n/a".to_string()
    } else {
        format!(
            "{:.2}%",
            (totals.mapped_code_bytes as f64 * 100.0) / totals.code_bytes as f64
        )
    };
    let unmapped_pct = if totals.code_bytes == 0 {
        "n/a".to_string()
    } else {
        format!(
            "{:.2}%",
            (totals.unmapped_code_bytes as f64 * 100.0) / totals.code_bytes as f64
        )
    };
    out.push_str(&format!("- mapped_code_pct: {mapped_pct}\n"));
    out.push_str(&format!("- unmapped_code_pct: {unmapped_pct}\n"));
    out.push_str(&format!(
        "- unmapped_synthetic_bytes: {} ({})\n",
        totals.unmapped_synthetic,
        format_percent_cell(ratio_percent_i128(
            totals.unmapped_synthetic as i128,
            totals.unmapped_code_bytes as i128
        ))
    ));
    out.push_str(&format!(
        "- unmapped_label_or_fixup_only_bytes: {} ({})\n",
        totals.unmapped_label_or_fixup_only,
        format_percent_cell(ratio_percent_i128(
            totals.unmapped_label_or_fixup_only as i128,
            totals.unmapped_code_bytes as i128
        ))
    ));
    out.push_str(&format!(
        "- unmapped_no_ir_inst_bytes: {} ({})\n",
        totals.unmapped_no_ir_inst,
        format_percent_cell(ratio_percent_i128(
            totals.unmapped_no_ir_inst as i128,
            totals.unmapped_code_bytes as i128
        ))
    ));
    out.push_str(&format!(
        "- unmapped_unknown_bytes: {} ({})\n\n",
        totals.unmapped_unknown,
        format_percent_cell(ratio_percent_i128(
            totals.unmapped_unknown as i128,
            totals.unmapped_code_bytes as i128
        ))
    ));

    let mut top_rows = rows.to_vec();
    top_rows.sort_by(|a, b| {
        b.unmapped_code_bytes
            .cmp(&a.unmapped_code_bytes)
            .then_with(|| a.suite.cmp(&b.suite))
            .then_with(|| a.test.cmp(&b.test))
            .then_with(|| a.section.cmp(&b.section))
    });
    out.push_str("| rank | suite | test | section | unmapped_code_bytes | unmapped_code_pct | synthetic_share_of_unmapped |\n");
    out.push_str("| ---: | --- | --- | --- | ---: | ---: | ---: |\n");
    for (idx, row) in top_rows.iter().take(10).enumerate() {
        let unmapped_pct = if row.code_bytes == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.unmapped_code_bytes as f64 * 100.0) / row.code_bytes as f64
            )
        };
        let synthetic_share = format_percent_cell(ratio_percent_i128(
            row.unmapped_synthetic as i128,
            row.unmapped_code_bytes as i128,
        ));
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} |\n",
            idx + 1,
            row.suite,
            row.test,
            row.section,
            row.unmapped_code_bytes,
            unmapped_pct,
            synthetic_share
        ));
    }
    out.push('\n');
}

fn append_trace_observability_hotspots_summary(
    out: &mut String,
    rows: &[TraceObservabilityHotspotRow],
) {
    if rows.is_empty() {
        return;
    }
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| {
        b.steps_in_bucket
            .cmp(&a.steps_in_bucket)
            .then_with(|| a.suite.cmp(&b.suite))
            .then_with(|| a.test.cmp(&b.test))
            .then_with(|| a.function.cmp(&b.function))
            .then_with(|| a.reason.cmp(&b.reason))
    });

    out.push_str("## Tail Trace Observability Attribution (Sonatina)\n\n");
    out.push_str(
        "Mapped from tail trace PCs to observability PC ranges (`debug/*.evm_trace.txt` + `artifacts/tests/<test>/sonatina/observability.json`).\n\n",
    );
    out.push_str(
        "| rank | suite | test | function | reason | steps_in_bucket | bucket_share_of_tail |\n",
    );
    out.push_str("| ---: | --- | --- | --- | --- | ---: | ---: |\n");
    for (idx, row) in sorted.iter().take(12).enumerate() {
        let pct = if row.tail_steps_total == 0 {
            "n/a".to_string()
        } else {
            format!(
                "{:.2}%",
                (row.steps_in_bucket as f64 * 100.0) / row.tail_steps_total as f64
            )
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} |\n",
            idx + 1,
            row.suite,
            row.test,
            row.function,
            row.reason,
            row.steps_in_bucket,
            pct
        ));
    }
    out.push('\n');
}

fn gas_opcode_comparison_header() -> &'static str {
    "test,symbol,yul_unopt_steps,yul_opt_steps,sonatina_steps,steps_ratio_vs_yul_unopt,steps_ratio_vs_yul_opt,yul_unopt_runtime_bytes,yul_opt_runtime_bytes,sonatina_runtime_bytes,bytes_ratio_vs_yul_unopt,bytes_ratio_vs_yul_opt,yul_unopt_runtime_ops,yul_opt_runtime_ops,sonatina_runtime_ops,ops_ratio_vs_yul_unopt,ops_ratio_vs_yul_opt,yul_unopt_stack_ops_pct,yul_opt_stack_ops_pct,sonatina_stack_ops_pct,yul_opt_swap_ops,sonatina_swap_ops,swap_ratio_vs_yul_opt,yul_opt_pop_ops,sonatina_pop_ops,pop_ratio_vs_yul_opt,yul_opt_jump_ops,sonatina_jump_ops,jump_ratio_vs_yul_opt,yul_opt_jumpi_ops,sonatina_jumpi_ops,jumpi_ratio_vs_yul_opt,yul_opt_iszero_ops,sonatina_iszero_ops,iszero_ratio_vs_yul_opt,yul_opt_mem_rw_ops,sonatina_mem_rw_ops,mem_rw_ratio_vs_yul_opt,yul_opt_storage_rw_ops,sonatina_storage_rw_ops,storage_rw_ratio_vs_yul_opt,yul_opt_keccak_ops,sonatina_keccak_ops,keccak_ratio_vs_yul_opt,yul_opt_call_family_ops,sonatina_call_family_ops,call_family_ratio_vs_yul_opt,yul_opt_copy_ops,sonatina_copy_ops,copy_ratio_vs_yul_opt,note"
}

fn write_gas_comparison_report(
    report: &ReportContext,
    primary_backend: &str,
    opt_level: OptLevel,
    cases: &[GasComparisonCase],
    primary_measurements: &FxHashMap<String, GasMeasurement>,
) {
    let artifacts_dir = report.root_dir.join("artifacts");
    let _ = create_dir_all_utf8(&artifacts_dir);
    let _ = std::fs::write(
        artifacts_dir.join("gas_comparison_settings.txt"),
        gas_comparison_settings_text(opt_level),
    );

    let mut markdown = String::new();
    markdown.push_str("# Gas Comparison\n\n");
    markdown.push_str("Comparison of runtime test-call gas usage (`gas_used`) between Yul and Sonatina backends.\n");
    markdown.push_str(
        "`delta` columns are `sonatina - yul`; negative means Sonatina used less gas.\n\n",
    );
    markdown.push_str("| test | yul_unopt | yul_opt | sonatina | delta_vs_unopt | pct_vs_unopt | delta_vs_opt | pct_vs_opt | note |\n");
    markdown.push_str("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |\n");
    let mut opcode_markdown = String::new();
    opcode_markdown.push_str("# EVM Opcode/Trace Comparison\n\n");
    opcode_markdown.push_str(
        "Static runtime opcode shape and dynamic EVM step counts for the same test call.\n\n",
    );
    opcode_markdown.push_str("| test | steps_ratio_vs_opt | bytes_ratio_vs_opt | ops_ratio_vs_opt | swap_ratio_vs_opt | pop_ratio_vs_opt | jump_ratio_vs_opt | jumpi_ratio_vs_opt | iszero_ratio_vs_opt | mem_rw_ratio_vs_opt | storage_rw_ratio_vs_opt | keccak_ratio_vs_opt | call_family_ratio_vs_opt | copy_ratio_vs_opt | note |\n");
    opcode_markdown.push_str("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |\n");
    let mut deployment_attribution_markdown = String::new();
    deployment_attribution_markdown.push_str("# Deployment Gas Attribution (Step Replay)\n\n");
    deployment_attribution_markdown.push_str(
        "CREATE/CREATE2 opcode and constructor-frame attribution from full-step call replay on cloned EVM state.\n\n",
    );
    deployment_attribution_markdown.push_str("| test | yul_opt_create_ops_gas | sonatina_create_ops_gas | delta_create_ops_vs_opt | yul_opt_constructor_frame_gas | sonatina_constructor_frame_gas | delta_constructor_frame_vs_opt | yul_opt_non_constructor_frame_gas | sonatina_non_constructor_frame_gas | delta_non_constructor_vs_opt | note |\n");
    deployment_attribution_markdown
        .push_str("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |\n");

    let mut csv = String::new();
    csv.push_str(
        "test,symbol,yul_unopt_gas,yul_opt_gas,sonatina_gas,delta_vs_yul_unopt,delta_vs_yul_unopt_pct,delta_vs_yul_opt,delta_vs_yul_opt_pct,yul_unopt_status,yul_opt_status,sonatina_status,note\n",
    );
    let mut breakdown_csv = String::new();
    breakdown_csv.push_str(
        "test,symbol,yul_unopt_call_gas,yul_opt_call_gas,sonatina_call_gas,delta_call_vs_yul_unopt,delta_call_vs_yul_unopt_pct,delta_call_vs_yul_opt,delta_call_vs_yul_opt_pct,yul_unopt_deploy_gas,yul_opt_deploy_gas,sonatina_deploy_gas,delta_deploy_vs_yul_unopt,delta_deploy_vs_yul_unopt_pct,delta_deploy_vs_yul_opt,delta_deploy_vs_yul_opt_pct,yul_unopt_total_gas,yul_opt_total_gas,sonatina_total_gas,delta_total_vs_yul_unopt,delta_total_vs_yul_unopt_pct,delta_total_vs_yul_opt,delta_total_vs_yul_opt_pct,note\n",
    );
    let mut opcode_csv = String::new();
    opcode_csv.push_str(gas_opcode_comparison_header());
    opcode_csv.push('\n');
    let mut deployment_attribution_csv = String::new();
    deployment_attribution_csv.push_str(
        "test,symbol,yul_unopt_step_total_gas,yul_opt_step_total_gas,sonatina_step_total_gas,yul_unopt_create_opcode_gas,yul_opt_create_opcode_gas,sonatina_create_opcode_gas,yul_unopt_create2_opcode_gas,yul_opt_create2_opcode_gas,sonatina_create2_opcode_gas,yul_unopt_constructor_frame_gas,yul_opt_constructor_frame_gas,sonatina_constructor_frame_gas,yul_unopt_non_constructor_frame_gas,yul_opt_non_constructor_frame_gas,sonatina_non_constructor_frame_gas,yul_unopt_create_opcode_steps,yul_opt_create_opcode_steps,sonatina_create_opcode_steps,yul_unopt_create2_opcode_steps,yul_opt_create2_opcode_steps,sonatina_create2_opcode_steps,note\n",
    );

    let mut totals = GasTotals {
        tests_in_scope: cases.len(),
        ..GasTotals::default()
    };
    let mut call_magnitude_totals = GasMagnitudeTotals::default();
    let mut deploy_magnitude_totals = GasMagnitudeTotals::default();
    let mut total_magnitude_totals = GasMagnitudeTotals::default();
    let mut opcode_magnitude_totals = OpcodeMagnitudeTotals::default();
    let mut deployment_attribution_totals = DeploymentGasAttributionTotals::default();

    for case in cases {
        // In Sonatina-primary runs, comparisons are still made against Yul.
        // Persist the exact Yul source/bytecode pair used for those baselines.
        if primary_backend.eq_ignore_ascii_case("sonatina")
            && let Some(yul_case) = case.yul.as_ref()
        {
            write_yul_case_artifacts(report, yul_case);
        }

        let yul_opt = if primary_backend.eq_ignore_ascii_case("yul") {
            primary_measurements.get(&case.symbol_name).cloned()
        } else {
            case.yul
                .as_ref()
                .map(|test| measure_case_gas(test, "yul", true, true))
        };

        let yul_unopt = case
            .yul
            .as_ref()
            .map(|test| measure_case_gas(test, "yul", false, true));

        let sonatina = if primary_backend.eq_ignore_ascii_case("sonatina") {
            primary_measurements.get(&case.symbol_name).cloned()
        } else {
            case.sonatina
                .as_ref()
                .map(|test| measure_case_gas(test, "sonatina", opt_level.yul_optimize(), true))
        };

        let yul_unopt_gas = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.gas_used);
        let yul_opt_gas = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.gas_used);
        let sonatina_gas = sonatina
            .as_ref()
            .and_then(|measurement| measurement.gas_used);
        let yul_unopt_deploy_gas = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.deploy_gas_used);
        let yul_opt_deploy_gas = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.deploy_gas_used);
        let sonatina_deploy_gas = sonatina
            .as_ref()
            .and_then(|measurement| measurement.deploy_gas_used);
        let yul_unopt_total_gas = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.total_gas_used);
        let yul_opt_total_gas = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.total_gas_used);
        let sonatina_total_gas = sonatina
            .as_ref()
            .and_then(|measurement| measurement.total_gas_used);
        let yul_unopt_cell = yul_unopt_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_opt_cell = yul_opt_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let sonatina_cell = sonatina_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_unopt_deploy_cell =
            yul_unopt_deploy_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_opt_deploy_cell =
            yul_opt_deploy_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let sonatina_deploy_cell =
            sonatina_deploy_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_unopt_total_cell =
            yul_unopt_total_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_opt_total_cell =
            yul_opt_total_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let sonatina_total_cell =
            sonatina_total_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_unopt_profile = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.gas_profile);
        let yul_opt_profile = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.gas_profile);
        let sonatina_profile = sonatina
            .as_ref()
            .and_then(|measurement| measurement.gas_profile);

        let mut notes = Vec::new();
        if case.yul.is_none() {
            notes.push("missing yul test metadata".to_string());
        }
        if case.sonatina.is_none() {
            notes.push("missing sonatina test metadata".to_string());
        }
        if let Some(measurement) = &yul_unopt
            && (!measurement.passed || measurement.gas_used.is_none())
        {
            notes.push(format!("yul_unopt {}", measurement.status_label()));
        }
        if let Some(measurement) = &yul_opt
            && (!measurement.passed || measurement.gas_used.is_none())
        {
            notes.push(format!("yul_opt {}", measurement.status_label()));
        }
        if let Some(measurement) = &sonatina
            && (!measurement.passed || measurement.gas_used.is_none())
        {
            notes.push(format!("sonatina {}", measurement.status_label()));
        }
        if yul_opt_profile.is_none() || sonatina_profile.is_none() {
            notes.push("missing step-attribution profile".to_string());
        }

        let yul_unopt_status = measurement_status(case.yul.is_some(), yul_unopt.as_ref());
        let yul_opt_status = measurement_status(case.yul.is_some(), yul_opt.as_ref());
        let sonatina_status = measurement_status(case.sonatina.is_some(), sonatina.as_ref());

        let (delta_unopt_cell, delta_unopt_pct_cell) =
            record_delta(yul_unopt_gas, sonatina_gas, &mut totals.vs_yul_unopt);
        let (delta_opt_cell, delta_opt_pct_cell) =
            record_delta(yul_opt_gas, sonatina_gas, &mut totals.vs_yul_opt);
        record_delta_magnitude(
            yul_unopt_gas,
            sonatina_gas,
            &mut call_magnitude_totals.vs_yul_unopt,
        );
        record_delta_magnitude(
            yul_opt_gas,
            sonatina_gas,
            &mut call_magnitude_totals.vs_yul_opt,
        );
        record_delta_magnitude(
            yul_unopt_deploy_gas,
            sonatina_deploy_gas,
            &mut deploy_magnitude_totals.vs_yul_unopt,
        );
        record_delta_magnitude(
            yul_opt_deploy_gas,
            sonatina_deploy_gas,
            &mut deploy_magnitude_totals.vs_yul_opt,
        );
        record_delta_magnitude(
            yul_unopt_total_gas,
            sonatina_total_gas,
            &mut total_magnitude_totals.vs_yul_unopt,
        );
        record_delta_magnitude(
            yul_opt_total_gas,
            sonatina_total_gas,
            &mut total_magnitude_totals.vs_yul_opt,
        );

        let note = if notes.is_empty() {
            String::new()
        } else {
            normalize_inline_text(&notes.join("; "))
        };

        markdown.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
            case.display_name,
            yul_unopt_cell,
            yul_opt_cell,
            sonatina_cell,
            delta_unopt_cell,
            delta_unopt_pct_cell,
            delta_opt_cell,
            delta_opt_pct_cell,
            note
        ));

        csv.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{},{},{},{}\n",
            csv_escape(&case.display_name),
            csv_escape(&case.symbol_name),
            csv_escape(&yul_unopt_cell),
            csv_escape(&yul_opt_cell),
            csv_escape(&sonatina_cell),
            csv_escape(&delta_unopt_cell),
            csv_escape(&delta_unopt_pct_cell),
            csv_escape(&delta_opt_cell),
            csv_escape(&delta_opt_pct_cell),
            csv_escape(yul_unopt_status),
            csv_escape(yul_opt_status),
            csv_escape(sonatina_status),
            csv_escape(&note),
        ));

        let (delta_call_unopt_cell, delta_call_unopt_pct_cell) =
            delta_cells(yul_unopt_gas, sonatina_gas);
        let (delta_call_opt_cell, delta_call_opt_pct_cell) = delta_cells(yul_opt_gas, sonatina_gas);
        let (delta_deploy_unopt_cell, delta_deploy_unopt_pct_cell) =
            delta_cells(yul_unopt_deploy_gas, sonatina_deploy_gas);
        let (delta_deploy_opt_cell, delta_deploy_opt_pct_cell) =
            delta_cells(yul_opt_deploy_gas, sonatina_deploy_gas);
        let (delta_total_unopt_cell, delta_total_unopt_pct_cell) =
            delta_cells(yul_unopt_total_gas, sonatina_total_gas);
        let (delta_total_opt_cell, delta_total_opt_pct_cell) =
            delta_cells(yul_opt_total_gas, sonatina_total_gas);
        breakdown_csv.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}\n",
            csv_escape(&case.display_name),
            csv_escape(&case.symbol_name),
            csv_escape(&yul_unopt_cell),
            csv_escape(&yul_opt_cell),
            csv_escape(&sonatina_cell),
            csv_escape(&delta_call_unopt_cell),
            csv_escape(&delta_call_unopt_pct_cell),
            csv_escape(&delta_call_opt_cell),
            csv_escape(&delta_call_opt_pct_cell),
            csv_escape(&yul_unopt_deploy_cell),
            csv_escape(&yul_opt_deploy_cell),
            csv_escape(&sonatina_deploy_cell),
            csv_escape(&delta_deploy_unopt_cell),
            csv_escape(&delta_deploy_unopt_pct_cell),
            csv_escape(&delta_deploy_opt_cell),
            csv_escape(&delta_deploy_opt_pct_cell),
            csv_escape(&yul_unopt_total_cell),
            csv_escape(&yul_opt_total_cell),
            csv_escape(&sonatina_total_cell),
            csv_escape(&delta_total_unopt_cell),
            csv_escape(&delta_total_unopt_pct_cell),
            csv_escape(&delta_total_opt_cell),
            csv_escape(&delta_total_opt_pct_cell),
            csv_escape(&note),
        ));

        let yul_unopt_steps = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.step_count);
        let yul_opt_steps = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.step_count);
        let sonatina_steps = sonatina
            .as_ref()
            .and_then(|measurement| measurement.step_count);

        let yul_unopt_metrics = yul_unopt
            .as_ref()
            .and_then(|measurement| measurement.runtime_metrics);
        let yul_opt_metrics = yul_opt
            .as_ref()
            .and_then(|measurement| measurement.runtime_metrics);
        let sonatina_metrics = sonatina
            .as_ref()
            .and_then(|measurement| measurement.runtime_metrics);

        let yul_unopt_bytes = yul_unopt_metrics.map(|metrics| metrics.byte_len);
        let yul_opt_bytes = yul_opt_metrics.map(|metrics| metrics.byte_len);
        let sonatina_bytes = sonatina_metrics.map(|metrics| metrics.byte_len);

        let yul_unopt_ops = yul_unopt_metrics.map(|metrics| metrics.op_count);
        let yul_opt_ops = yul_opt_metrics.map(|metrics| metrics.op_count);
        let sonatina_ops = sonatina_metrics.map(|metrics| metrics.op_count);

        let steps_ratio_unopt = ratio_cell_u64(sonatina_steps, yul_unopt_steps);
        let steps_ratio_opt = ratio_cell_u64(sonatina_steps, yul_opt_steps);
        let bytes_ratio_unopt = ratio_cell_usize(sonatina_bytes, yul_unopt_bytes);
        let bytes_ratio_opt = ratio_cell_usize(sonatina_bytes, yul_opt_bytes);
        let ops_ratio_unopt = ratio_cell_usize(sonatina_ops, yul_unopt_ops);
        let ops_ratio_opt = ratio_cell_usize(sonatina_ops, yul_opt_ops);

        let yul_opt_swap = yul_opt_metrics.map(|metrics| metrics.swap_ops);
        let sonatina_swap = sonatina_metrics.map(|metrics| metrics.swap_ops);
        let swap_ratio_opt = ratio_cell_usize(sonatina_swap, yul_opt_swap);

        let yul_opt_pop = yul_opt_metrics.map(|metrics| metrics.pop_ops);
        let sonatina_pop = sonatina_metrics.map(|metrics| metrics.pop_ops);
        let pop_ratio_opt = ratio_cell_usize(sonatina_pop, yul_opt_pop);

        let yul_opt_jump = yul_opt_metrics.map(|metrics| metrics.jump_ops);
        let sonatina_jump = sonatina_metrics.map(|metrics| metrics.jump_ops);
        let jump_ratio_opt = ratio_cell_usize(sonatina_jump, yul_opt_jump);

        let yul_opt_jumpi = yul_opt_metrics.map(|metrics| metrics.jumpi_ops);
        let sonatina_jumpi = sonatina_metrics.map(|metrics| metrics.jumpi_ops);
        let jumpi_ratio_opt = ratio_cell_usize(sonatina_jumpi, yul_opt_jumpi);

        let yul_opt_iszero = yul_opt_metrics.map(|metrics| metrics.iszero_ops);
        let sonatina_iszero = sonatina_metrics.map(|metrics| metrics.iszero_ops);
        let iszero_ratio_opt = ratio_cell_usize(sonatina_iszero, yul_opt_iszero);

        let yul_opt_mem_rw = yul_opt_metrics.map(|metrics| metrics.mem_rw_ops_total());
        let sonatina_mem_rw = sonatina_metrics.map(|metrics| metrics.mem_rw_ops_total());
        let mem_rw_ratio_opt = ratio_cell_usize(sonatina_mem_rw, yul_opt_mem_rw);

        let yul_opt_storage_rw = yul_opt_metrics.map(|metrics| metrics.storage_rw_ops_total());
        let sonatina_storage_rw = sonatina_metrics.map(|metrics| metrics.storage_rw_ops_total());
        let storage_rw_ratio_opt = ratio_cell_usize(sonatina_storage_rw, yul_opt_storage_rw);

        let yul_opt_keccak = yul_opt_metrics.map(|metrics| metrics.keccak_ops);
        let sonatina_keccak = sonatina_metrics.map(|metrics| metrics.keccak_ops);
        let keccak_ratio_opt = ratio_cell_usize(sonatina_keccak, yul_opt_keccak);

        let yul_opt_call_family = yul_opt_metrics.map(|metrics| metrics.call_family_ops_total());
        let sonatina_call_family = sonatina_metrics.map(|metrics| metrics.call_family_ops_total());
        let call_family_ratio_opt = ratio_cell_usize(sonatina_call_family, yul_opt_call_family);

        let yul_opt_copy = yul_opt_metrics.map(|metrics| metrics.copy_ops_total());
        let sonatina_copy = sonatina_metrics.map(|metrics| metrics.copy_ops_total());
        let copy_ratio_opt = ratio_cell_usize(sonatina_copy, yul_opt_copy);

        if let (Some(y_steps), Some(y_metrics), Some(s_steps), Some(s_metrics)) = (
            yul_opt_steps,
            yul_opt_metrics,
            sonatina_steps,
            sonatina_metrics,
        ) {
            opcode_magnitude_totals.compared_with_metrics += 1;
            opcode_magnitude_totals
                .yul_opt
                .add_observation(y_steps, y_metrics);
            opcode_magnitude_totals
                .sonatina
                .add_observation(s_steps, s_metrics);
        }

        opcode_markdown.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
            case.display_name,
            steps_ratio_opt,
            bytes_ratio_opt,
            ops_ratio_opt,
            swap_ratio_opt,
            pop_ratio_opt,
            jump_ratio_opt,
            jumpi_ratio_opt,
            iszero_ratio_opt,
            mem_rw_ratio_opt,
            storage_rw_ratio_opt,
            keccak_ratio_opt,
            call_family_ratio_opt,
            copy_ratio_opt,
            note
        ));

        let opcode_cells = vec![
            csv_escape(&case.display_name),
            csv_escape(&case.symbol_name),
            csv_escape(&u64_cell(yul_unopt_steps)),
            csv_escape(&u64_cell(yul_opt_steps)),
            csv_escape(&u64_cell(sonatina_steps)),
            csv_escape(&steps_ratio_unopt),
            csv_escape(&steps_ratio_opt),
            csv_escape(&usize_cell(yul_unopt_bytes)),
            csv_escape(&usize_cell(yul_opt_bytes)),
            csv_escape(&usize_cell(sonatina_bytes)),
            csv_escape(&bytes_ratio_unopt),
            csv_escape(&bytes_ratio_opt),
            csv_escape(&usize_cell(yul_unopt_ops)),
            csv_escape(&usize_cell(yul_opt_ops)),
            csv_escape(&usize_cell(sonatina_ops)),
            csv_escape(&ops_ratio_unopt),
            csv_escape(&ops_ratio_opt),
            csv_escape(&stack_ops_pct_cell(yul_unopt_metrics)),
            csv_escape(&stack_ops_pct_cell(yul_opt_metrics)),
            csv_escape(&stack_ops_pct_cell(sonatina_metrics)),
            csv_escape(&usize_cell(yul_opt_swap)),
            csv_escape(&usize_cell(sonatina_swap)),
            csv_escape(&swap_ratio_opt),
            csv_escape(&usize_cell(yul_opt_pop)),
            csv_escape(&usize_cell(sonatina_pop)),
            csv_escape(&pop_ratio_opt),
            csv_escape(&usize_cell(yul_opt_jump)),
            csv_escape(&usize_cell(sonatina_jump)),
            csv_escape(&jump_ratio_opt),
            csv_escape(&usize_cell(yul_opt_jumpi)),
            csv_escape(&usize_cell(sonatina_jumpi)),
            csv_escape(&jumpi_ratio_opt),
            csv_escape(&usize_cell(yul_opt_iszero)),
            csv_escape(&usize_cell(sonatina_iszero)),
            csv_escape(&iszero_ratio_opt),
            csv_escape(&usize_cell(yul_opt_mem_rw)),
            csv_escape(&usize_cell(sonatina_mem_rw)),
            csv_escape(&mem_rw_ratio_opt),
            csv_escape(&usize_cell(yul_opt_storage_rw)),
            csv_escape(&usize_cell(sonatina_storage_rw)),
            csv_escape(&storage_rw_ratio_opt),
            csv_escape(&usize_cell(yul_opt_keccak)),
            csv_escape(&usize_cell(sonatina_keccak)),
            csv_escape(&keccak_ratio_opt),
            csv_escape(&usize_cell(yul_opt_call_family)),
            csv_escape(&usize_cell(sonatina_call_family)),
            csv_escape(&call_family_ratio_opt),
            csv_escape(&usize_cell(yul_opt_copy)),
            csv_escape(&usize_cell(sonatina_copy)),
            csv_escape(&copy_ratio_opt),
            csv_escape(&note),
        ];
        opcode_csv.push_str(&opcode_cells.join(","));
        opcode_csv.push('\n');

        let yul_unopt_step_total = yul_unopt_profile.map(|profile| profile.total_step_gas);
        let yul_opt_step_total = yul_opt_profile.map(|profile| profile.total_step_gas);
        let sonatina_step_total = sonatina_profile.map(|profile| profile.total_step_gas);
        let yul_unopt_create_opcode_gas =
            yul_unopt_profile.map(|profile| profile.create_opcode_gas);
        let yul_opt_create_opcode_gas = yul_opt_profile.map(|profile| profile.create_opcode_gas);
        let sonatina_create_opcode_gas = sonatina_profile.map(|profile| profile.create_opcode_gas);
        let yul_unopt_create2_opcode_gas =
            yul_unopt_profile.map(|profile| profile.create2_opcode_gas);
        let yul_opt_create2_opcode_gas = yul_opt_profile.map(|profile| profile.create2_opcode_gas);
        let sonatina_create2_opcode_gas =
            sonatina_profile.map(|profile| profile.create2_opcode_gas);
        let yul_unopt_constructor_frame_gas =
            yul_unopt_profile.map(|profile| profile.constructor_frame_gas);
        let yul_opt_constructor_frame_gas =
            yul_opt_profile.map(|profile| profile.constructor_frame_gas);
        let sonatina_constructor_frame_gas =
            sonatina_profile.map(|profile| profile.constructor_frame_gas);
        let yul_unopt_non_constructor_frame_gas =
            yul_unopt_profile.map(|profile| profile.non_constructor_frame_gas);
        let yul_opt_non_constructor_frame_gas =
            yul_opt_profile.map(|profile| profile.non_constructor_frame_gas);
        let sonatina_non_constructor_frame_gas =
            sonatina_profile.map(|profile| profile.non_constructor_frame_gas);
        let yul_unopt_create_opcode_steps =
            yul_unopt_profile.map(|profile| profile.create_opcode_steps);
        let yul_opt_create_opcode_steps =
            yul_opt_profile.map(|profile| profile.create_opcode_steps);
        let sonatina_create_opcode_steps =
            sonatina_profile.map(|profile| profile.create_opcode_steps);
        let yul_unopt_create2_opcode_steps =
            yul_unopt_profile.map(|profile| profile.create2_opcode_steps);
        let yul_opt_create2_opcode_steps =
            yul_opt_profile.map(|profile| profile.create2_opcode_steps);
        let sonatina_create2_opcode_steps =
            sonatina_profile.map(|profile| profile.create2_opcode_steps);

        let yul_opt_create_ops_gas =
            yul_opt_profile.map(|profile| profile.create_opcode_gas + profile.create2_opcode_gas);
        let sonatina_create_ops_gas =
            sonatina_profile.map(|profile| profile.create_opcode_gas + profile.create2_opcode_gas);
        let (delta_create_ops_vs_opt, _) =
            delta_cells(yul_opt_create_ops_gas, sonatina_create_ops_gas);
        let (delta_constructor_vs_opt, _) = delta_cells(
            yul_opt_constructor_frame_gas,
            sonatina_constructor_frame_gas,
        );
        let (delta_non_constructor_vs_opt, _) = delta_cells(
            yul_opt_non_constructor_frame_gas,
            sonatina_non_constructor_frame_gas,
        );

        deployment_attribution_markdown.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
            case.display_name,
            u64_cell(yul_opt_create_ops_gas),
            u64_cell(sonatina_create_ops_gas),
            delta_create_ops_vs_opt,
            u64_cell(yul_opt_constructor_frame_gas),
            u64_cell(sonatina_constructor_frame_gas),
            delta_constructor_vs_opt,
            u64_cell(yul_opt_non_constructor_frame_gas),
            u64_cell(sonatina_non_constructor_frame_gas),
            delta_non_constructor_vs_opt,
            note,
        ));

        let deployment_attribution_row = DeploymentGasAttributionRow {
            test: case.display_name.clone(),
            symbol: case.symbol_name.clone(),
            yul_unopt_step_total_gas: yul_unopt_step_total,
            yul_opt_step_total_gas: yul_opt_step_total,
            sonatina_step_total_gas: sonatina_step_total,
            yul_unopt_create_opcode_gas,
            yul_opt_create_opcode_gas,
            sonatina_create_opcode_gas,
            yul_unopt_create2_opcode_gas,
            yul_opt_create2_opcode_gas,
            sonatina_create2_opcode_gas,
            yul_unopt_constructor_frame_gas,
            yul_opt_constructor_frame_gas,
            sonatina_constructor_frame_gas,
            yul_unopt_non_constructor_frame_gas,
            yul_opt_non_constructor_frame_gas,
            sonatina_non_constructor_frame_gas,
            yul_unopt_create_opcode_steps,
            yul_opt_create_opcode_steps,
            sonatina_create_opcode_steps,
            yul_unopt_create2_opcode_steps,
            yul_opt_create2_opcode_steps,
            sonatina_create2_opcode_steps,
            note: note.clone(),
        };
        deployment_attribution_csv.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}\n",
            csv_escape(&deployment_attribution_row.test),
            csv_escape(&deployment_attribution_row.symbol),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_unopt_step_total_gas
            )),
            csv_escape(&u64_cell(deployment_attribution_row.yul_opt_step_total_gas)),
            csv_escape(&u64_cell(
                deployment_attribution_row.sonatina_step_total_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_unopt_create_opcode_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_opt_create_opcode_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.sonatina_create_opcode_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_unopt_create2_opcode_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_opt_create2_opcode_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.sonatina_create2_opcode_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_unopt_constructor_frame_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_opt_constructor_frame_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.sonatina_constructor_frame_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_unopt_non_constructor_frame_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_opt_non_constructor_frame_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.sonatina_non_constructor_frame_gas
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_unopt_create_opcode_steps
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_opt_create_opcode_steps
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.sonatina_create_opcode_steps
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_unopt_create2_opcode_steps
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.yul_opt_create2_opcode_steps
            )),
            csv_escape(&u64_cell(
                deployment_attribution_row.sonatina_create2_opcode_steps
            )),
            csv_escape(&deployment_attribution_row.note),
        ));

        if let (Some(yul_opt_profile), Some(sonatina_profile)) = (yul_opt_profile, sonatina_profile)
        {
            deployment_attribution_totals.add_observation(yul_opt_profile, sonatina_profile);
        }
    }

    markdown.push_str("\n## Summary\n\n");
    markdown.push_str(&format!("- tests_in_scope: {}\n", totals.tests_in_scope));
    markdown.push('\n');
    append_comparison_summary(
        &mut markdown,
        "vs Yul (unoptimized)",
        totals.vs_yul_unopt,
        totals.tests_in_scope,
    );
    append_comparison_summary(
        &mut markdown,
        "vs Yul (optimized)",
        totals.vs_yul_opt,
        totals.tests_in_scope,
    );
    markdown.push_str("\n## Aggregate Delta Metrics\n\n");
    append_magnitude_summary(
        &mut markdown,
        "Runtime Call Gas vs Yul (unoptimized)",
        call_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut markdown,
        "Runtime Call Gas vs Yul (optimized)",
        call_magnitude_totals.vs_yul_opt,
    );
    markdown.push_str("\n## Deploy/Call/Total Breakdown\n\n");
    append_magnitude_summary(
        &mut markdown,
        "Deployment Gas vs Yul (unoptimized)",
        deploy_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut markdown,
        "Deployment Gas vs Yul (optimized)",
        deploy_magnitude_totals.vs_yul_opt,
    );
    append_magnitude_summary(
        &mut markdown,
        "Total Gas (deploy+call) vs Yul (unoptimized)",
        total_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut markdown,
        "Total Gas (deploy+call) vs Yul (optimized)",
        total_magnitude_totals.vs_yul_opt,
    );
    append_deployment_attribution_summary(
        &mut markdown,
        "Deployment Attribution (Step Replay)",
        deployment_attribution_totals,
    );
    append_opcode_magnitude_summary(&mut markdown, opcode_magnitude_totals);

    markdown.push_str("\n## Optimization Settings\n\n");
    for line in gas_comparison_settings_text(opt_level).lines() {
        markdown.push_str(&format!("- {line}\n"));
    }
    markdown.push_str(
        "\nMachine-readable aggregates: `artifacts/gas_comparison_totals.csv`, `artifacts/gas_comparison_magnitude.csv`, `artifacts/gas_breakdown_comparison.csv`, `artifacts/gas_breakdown_magnitude.csv`, `artifacts/gas_opcode_magnitude.csv`, and `artifacts/gas_deployment_attribution.csv`.\n",
    );
    markdown.push_str(
        "\n## Opcode/Trace Profile\n\nSee `artifacts/gas_opcode_comparison.md` and `artifacts/gas_opcode_comparison.csv` for bytecode shape and dynamic step-count diagnostics.\n",
    );

    let _ = std::fs::write(artifacts_dir.join("gas_comparison.md"), markdown);
    let _ = std::fs::write(artifacts_dir.join("gas_comparison.csv"), csv);
    let _ = std::fs::write(
        artifacts_dir.join("gas_breakdown_comparison.csv"),
        breakdown_csv,
    );
    let _ = std::fs::write(
        artifacts_dir.join("gas_opcode_comparison.md"),
        opcode_markdown,
    );
    let _ = std::fs::write(artifacts_dir.join("gas_opcode_comparison.csv"), opcode_csv);
    let _ = std::fs::write(
        artifacts_dir.join("gas_deployment_attribution.md"),
        deployment_attribution_markdown,
    );
    let _ = std::fs::write(
        artifacts_dir.join("gas_deployment_attribution.csv"),
        deployment_attribution_csv,
    );
    write_gas_totals_csv(&artifacts_dir.join("gas_comparison_totals.csv"), totals);
    write_gas_magnitude_csv(
        &artifacts_dir.join("gas_comparison_magnitude.csv"),
        call_magnitude_totals,
    );
    write_gas_breakdown_magnitude_csv(
        &artifacts_dir.join("gas_breakdown_magnitude.csv"),
        call_magnitude_totals,
        deploy_magnitude_totals,
        total_magnitude_totals,
    );
    write_opcode_magnitude_csv(
        &artifacts_dir.join("gas_opcode_magnitude.csv"),
        opcode_magnitude_totals,
    );
}

fn write_run_gas_comparison_summary(root_dir: &Utf8PathBuf, opt_level: OptLevel) {
    let artifacts_dir = root_dir.join("artifacts");
    let _ = create_dir_all_utf8(&artifacts_dir);
    let _ = std::fs::write(
        artifacts_dir.join("gas_comparison_settings.txt"),
        gas_comparison_settings_text(opt_level),
    );

    let mut suite_dirs: Vec<(String, Utf8PathBuf)> = Vec::new();
    for status_dir in ["passed", "failed"] {
        let dir = root_dir.join(status_dir);
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let suite = entry.file_name().to_string_lossy().to_string();
            if let Ok(path) = Utf8PathBuf::from_path_buf(path) {
                suite_dirs.push((suite, path));
            }
        }
    }
    suite_dirs.sort_by(|a, b| a.0.cmp(&b.0));

    let mut all_rows = String::new();
    all_rows.push_str("suite,test,symbol,yul_unopt_gas,yul_opt_gas,sonatina_gas,delta_vs_yul_unopt,delta_vs_yul_unopt_pct,delta_vs_yul_opt,delta_vs_yul_opt_pct,yul_unopt_status,yul_opt_status,sonatina_status,note\n");
    let mut all_breakdown_rows = String::new();
    all_breakdown_rows.push_str("suite,test,symbol,yul_unopt_call_gas,yul_opt_call_gas,sonatina_call_gas,delta_call_vs_yul_unopt,delta_call_vs_yul_unopt_pct,delta_call_vs_yul_opt,delta_call_vs_yul_opt_pct,yul_unopt_deploy_gas,yul_opt_deploy_gas,sonatina_deploy_gas,delta_deploy_vs_yul_unopt,delta_deploy_vs_yul_unopt_pct,delta_deploy_vs_yul_opt,delta_deploy_vs_yul_opt_pct,yul_unopt_total_gas,yul_opt_total_gas,sonatina_total_gas,delta_total_vs_yul_unopt,delta_total_vs_yul_unopt_pct,delta_total_vs_yul_opt,delta_total_vs_yul_opt_pct,note\n");
    let mut all_opcode_rows = String::new();
    all_opcode_rows.push_str("suite,");
    all_opcode_rows.push_str(gas_opcode_comparison_header());
    all_opcode_rows.push('\n');
    let mut all_deployment_attr_rows = String::new();
    all_deployment_attr_rows.push_str("suite,test,symbol,yul_unopt_step_total_gas,yul_opt_step_total_gas,sonatina_step_total_gas,yul_unopt_create_opcode_gas,yul_opt_create_opcode_gas,sonatina_create_opcode_gas,yul_unopt_create2_opcode_gas,yul_opt_create2_opcode_gas,sonatina_create2_opcode_gas,yul_unopt_constructor_frame_gas,yul_opt_constructor_frame_gas,sonatina_constructor_frame_gas,yul_unopt_non_constructor_frame_gas,yul_opt_non_constructor_frame_gas,sonatina_non_constructor_frame_gas,yul_unopt_create_opcode_steps,yul_opt_create_opcode_steps,sonatina_create_opcode_steps,yul_unopt_create2_opcode_steps,yul_opt_create2_opcode_steps,sonatina_create2_opcode_steps,note\n");
    let mut wrote_any_rows = false;
    let mut wrote_any_breakdown_rows = false;
    let mut wrote_any_opcode_rows = false;
    let mut wrote_any_deployment_attr_rows = false;
    let mut totals = GasTotals::default();
    let mut call_magnitude_totals = GasMagnitudeTotals::default();
    let mut deploy_magnitude_totals = GasMagnitudeTotals::default();
    let mut total_magnitude_totals = GasMagnitudeTotals::default();
    let mut opcode_magnitude_totals = OpcodeMagnitudeTotals::default();
    let mut deployment_attribution_totals = DeploymentGasAttributionTotals::default();
    let mut hotspots: Vec<GasHotspotRow> = Vec::new();
    let mut suite_rollup: FxHashMap<String, SuiteDeltaTotals> = FxHashMap::default();
    let mut trace_symbol_hotspots: Vec<TraceSymbolHotspotRow> = Vec::new();
    let mut observability_coverage_rows: Vec<ObservabilityCoverageRow> = Vec::new();
    let mut observability_coverage_totals = ObservabilityCoverageTotals::default();
    let mut trace_observability_hotspots: Vec<TraceObservabilityHotspotRow> = Vec::new();

    for (suite, suite_dir) in suite_dirs {
        let (suite_observability_rows, suite_observability_ranges) =
            load_suite_observability_runtime(&suite_dir, &suite);
        for row in suite_observability_rows {
            observability_coverage_totals.add_row(&row);
            observability_coverage_rows.push(row);
        }

        let suite_rows_path = suite_dir.join("artifacts").join("gas_comparison.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_rows_path) {
            for (idx, line) in contents.lines().enumerate() {
                if idx == 0 || line.trim().is_empty() {
                    continue;
                }
                let fields = parse_csv_fields(line);
                if fields.len() >= 9 {
                    let yul_opt_gas = parse_optional_u64_cell(&fields[3]);
                    let sonatina_gas = parse_optional_u64_cell(&fields[4]);
                    if let Some(delta_vs_yul_opt) = parse_optional_i128_cell(&fields[7]) {
                        hotspots.push(GasHotspotRow {
                            suite: suite.clone(),
                            test: fields[0].clone(),
                            symbol: fields[1].clone(),
                            yul_opt_gas,
                            sonatina_gas,
                            delta_vs_yul_opt,
                            delta_vs_yul_opt_pct: fields[8].clone(),
                        });
                        let entry = suite_rollup.entry(suite.clone()).or_default();
                        entry.tests_with_delta += 1;
                        entry.delta_vs_yul_opt_sum += delta_vs_yul_opt;
                    }
                }
                all_rows.push_str(&csv_escape(&suite));
                all_rows.push(',');
                all_rows.push_str(line);
                all_rows.push('\n');
                wrote_any_rows = true;
            }
        }

        let suite_breakdown_rows_path = suite_dir
            .join("artifacts")
            .join("gas_breakdown_comparison.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_breakdown_rows_path) {
            for (idx, line) in contents.lines().enumerate() {
                if idx == 0 || line.trim().is_empty() {
                    continue;
                }
                all_breakdown_rows.push_str(&csv_escape(&suite));
                all_breakdown_rows.push(',');
                all_breakdown_rows.push_str(line);
                all_breakdown_rows.push('\n');
                wrote_any_breakdown_rows = true;
            }
        }

        let suite_opcode_rows_path = suite_dir
            .join("artifacts")
            .join("gas_opcode_comparison.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_opcode_rows_path) {
            for (idx, line) in contents.lines().enumerate() {
                if idx == 0 || line.trim().is_empty() {
                    continue;
                }
                all_opcode_rows.push_str(&csv_escape(&suite));
                all_opcode_rows.push(',');
                all_opcode_rows.push_str(line);
                all_opcode_rows.push('\n');
                wrote_any_opcode_rows = true;
            }
        }

        let suite_deployment_attr_rows_path = suite_dir
            .join("artifacts")
            .join("gas_deployment_attribution.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_deployment_attr_rows_path) {
            for (idx, line) in contents.lines().enumerate() {
                if idx == 0 || line.trim().is_empty() {
                    continue;
                }
                let fields = parse_csv_fields(line);
                if fields.len() >= 17 {
                    let yul_opt_profile = Some(CallGasProfile {
                        total_step_gas: parse_optional_u64_cell(&fields[3]).unwrap_or(0),
                        create_opcode_gas: parse_optional_u64_cell(&fields[6]).unwrap_or(0),
                        create2_opcode_gas: parse_optional_u64_cell(&fields[9]).unwrap_or(0),
                        constructor_frame_gas: parse_optional_u64_cell(&fields[12]).unwrap_or(0),
                        non_constructor_frame_gas: parse_optional_u64_cell(&fields[15])
                            .unwrap_or(0),
                        ..CallGasProfile::default()
                    });
                    let sonatina_profile = Some(CallGasProfile {
                        total_step_gas: parse_optional_u64_cell(&fields[4]).unwrap_or(0),
                        create_opcode_gas: parse_optional_u64_cell(&fields[7]).unwrap_or(0),
                        create2_opcode_gas: parse_optional_u64_cell(&fields[10]).unwrap_or(0),
                        constructor_frame_gas: parse_optional_u64_cell(&fields[13]).unwrap_or(0),
                        non_constructor_frame_gas: parse_optional_u64_cell(&fields[16])
                            .unwrap_or(0),
                        ..CallGasProfile::default()
                    });
                    if let (Some(yul_opt_profile), Some(sonatina_profile)) =
                        (yul_opt_profile, sonatina_profile)
                    {
                        if parse_optional_u64_cell(&fields[3]).is_some()
                            && parse_optional_u64_cell(&fields[4]).is_some()
                        {
                            deployment_attribution_totals
                                .add_observation(yul_opt_profile, sonatina_profile);
                        }
                    }
                }
                all_deployment_attr_rows.push_str(&csv_escape(&suite));
                all_deployment_attr_rows.push(',');
                all_deployment_attr_rows.push_str(line);
                all_deployment_attr_rows.push('\n');
                wrote_any_deployment_attr_rows = true;
            }
        }

        let suite_totals_path = suite_dir
            .join("artifacts")
            .join("gas_comparison_totals.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_totals_path) {
            totals.add(parse_gas_totals_csv(&contents));
        }

        let suite_magnitude_path = suite_dir
            .join("artifacts")
            .join("gas_comparison_magnitude.csv");
        let mut has_legacy_call_magnitude = false;
        if let Ok(contents) = std::fs::read_to_string(&suite_magnitude_path) {
            call_magnitude_totals.add(parse_gas_magnitude_csv(&contents));
            has_legacy_call_magnitude = true;
        }

        let suite_breakdown_magnitude_path = suite_dir
            .join("artifacts")
            .join("gas_breakdown_magnitude.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_breakdown_magnitude_path) {
            let (call, deploy, total) = parse_gas_breakdown_magnitude_csv(&contents);
            if !has_legacy_call_magnitude {
                call_magnitude_totals.add(call);
            }
            deploy_magnitude_totals.add(deploy);
            total_magnitude_totals.add(total);
        }

        let suite_opcode_magnitude_path =
            suite_dir.join("artifacts").join("gas_opcode_magnitude.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_opcode_magnitude_path) {
            opcode_magnitude_totals.add(parse_gas_opcode_magnitude_csv(&contents));
        }

        let debug_dir = suite_dir.join("debug");
        let symtab_entries = std::fs::read_to_string(debug_dir.join("sonatina_symtab.txt"))
            .ok()
            .map(|contents| parse_symtab_entries(&contents))
            .unwrap_or_default();
        if let Ok(entries) = std::fs::read_dir(&debug_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if !path.is_file() {
                    continue;
                }
                let Some(file_name) = path.file_name().and_then(|name| name.to_str()) else {
                    continue;
                };
                if !file_name.ends_with(".evm_trace.txt") {
                    continue;
                }
                let Ok(contents) = std::fs::read_to_string(&path) else {
                    continue;
                };
                let pcs = parse_trace_tail_pcs(&contents);
                if pcs.is_empty() {
                    continue;
                }
                let mut counts: FxHashMap<String, usize> = FxHashMap::default();
                let mut mapped = 0usize;
                if !symtab_entries.is_empty() {
                    for pc in &pcs {
                        if let Some(symbol) = map_pc_to_symbol(*pc, &symtab_entries) {
                            *counts.entry(symbol.to_string()).or_default() += 1;
                            mapped += 1;
                        }
                    }
                }
                let test_name = file_name
                    .strip_suffix(".evm_trace.txt")
                    .unwrap_or(file_name)
                    .to_string();
                if !counts.is_empty() {
                    for (symbol, steps_in_symbol) in counts {
                        trace_symbol_hotspots.push(TraceSymbolHotspotRow {
                            suite: suite.clone(),
                            test: test_name.clone(),
                            symbol,
                            tail_steps_total: pcs.len(),
                            tail_steps_mapped: mapped,
                            steps_in_symbol,
                        });
                    }
                }

                if let Some(pc_ranges) = resolve_observability_test_ranges(
                    &suite,
                    &test_name,
                    &suite_observability_ranges,
                ) {
                    let mut obs_counts: FxHashMap<(String, String), usize> = FxHashMap::default();
                    let mut obs_mapped = 0usize;
                    for pc in &pcs {
                        if let Some(range) = map_pc_to_observability(*pc, pc_ranges) {
                            obs_mapped += 1;
                            let reason =
                                range.reason.clone().unwrap_or_else(|| "mapped".to_string());
                            *obs_counts
                                .entry((range.func_name.clone(), reason))
                                .or_default() += 1;
                        } else {
                            *obs_counts
                                .entry(("<unmapped_pc>".to_string(), "pc_not_in_map".to_string()))
                                .or_default() += 1;
                        }
                    }

                    for ((function, reason), steps_in_bucket) in obs_counts {
                        trace_observability_hotspots.push(TraceObservabilityHotspotRow {
                            suite: suite.clone(),
                            test: test_name.clone(),
                            function,
                            reason,
                            tail_steps_total: pcs.len(),
                            tail_steps_mapped: obs_mapped,
                            steps_in_bucket,
                        });
                    }
                } else {
                    trace_observability_hotspots.push(TraceObservabilityHotspotRow {
                        suite: suite.clone(),
                        test: test_name.clone(),
                        function: "<unmatched_test_name>".to_string(),
                        reason: "no_observability_test_match".to_string(),
                        tail_steps_total: pcs.len(),
                        tail_steps_mapped: 0,
                        steps_in_bucket: pcs.len(),
                    });
                }
            }
        }
    }

    let mut suite_rollup_rows: Vec<(String, SuiteDeltaTotals)> = suite_rollup.into_iter().collect();
    suite_rollup_rows.sort_by(|a, b| {
        b.1.delta_vs_yul_opt_sum
            .cmp(&a.1.delta_vs_yul_opt_sum)
            .then_with(|| a.0.cmp(&b.0))
    });

    if wrote_any_rows {
        let _ = std::fs::write(artifacts_dir.join("gas_comparison_all.csv"), all_rows);
        write_gas_hotspots_csv(
            &artifacts_dir.join("gas_hotspots_vs_yul_opt.csv"),
            &hotspots,
        );
        write_suite_delta_summary_csv(
            &artifacts_dir.join("gas_suite_delta_summary.csv"),
            &suite_rollup_rows,
        );
        write_trace_symbol_hotspots_csv(
            &artifacts_dir.join("gas_tail_trace_symbol_hotspots.csv"),
            &trace_symbol_hotspots,
        );
        if !trace_observability_hotspots.is_empty() {
            write_trace_observability_hotspots_csv(
                &artifacts_dir.join("gas_tail_trace_observability_hotspots.csv"),
                &trace_observability_hotspots,
            );
        }
    }
    if !observability_coverage_rows.is_empty() {
        write_observability_coverage_csv(
            &artifacts_dir.join("observability_coverage_all.csv"),
            &observability_coverage_rows,
        );
    }
    if wrote_any_breakdown_rows {
        let _ = std::fs::write(
            artifacts_dir.join("gas_breakdown_comparison_all.csv"),
            all_breakdown_rows,
        );
    }
    if wrote_any_opcode_rows {
        let _ = std::fs::write(
            artifacts_dir.join("gas_opcode_comparison_all.csv"),
            all_opcode_rows,
        );
    }
    if wrote_any_deployment_attr_rows {
        let _ = std::fs::write(
            artifacts_dir.join("gas_deployment_attribution_all.csv"),
            all_deployment_attr_rows,
        );
    }
    write_gas_totals_csv(&artifacts_dir.join("gas_comparison_totals.csv"), totals);
    write_gas_magnitude_csv(
        &artifacts_dir.join("gas_comparison_magnitude.csv"),
        call_magnitude_totals,
    );
    write_gas_breakdown_magnitude_csv(
        &artifacts_dir.join("gas_breakdown_magnitude.csv"),
        call_magnitude_totals,
        deploy_magnitude_totals,
        total_magnitude_totals,
    );
    write_opcode_magnitude_csv(
        &artifacts_dir.join("gas_opcode_magnitude.csv"),
        opcode_magnitude_totals,
    );

    let mut summary = String::new();
    summary.push_str("# Gas Comparison Summary\n\n");
    summary.push_str("Aggregated totals across all suite reports in this archive.\n\n");
    summary.push_str(&format!("- tests_in_scope: {}\n", totals.tests_in_scope));
    summary.push('\n');
    append_comparison_summary(
        &mut summary,
        "vs Yul (unoptimized)",
        totals.vs_yul_unopt,
        totals.tests_in_scope,
    );
    append_comparison_summary(
        &mut summary,
        "vs Yul (optimized)",
        totals.vs_yul_opt,
        totals.tests_in_scope,
    );
    summary.push_str("\n## Aggregate Delta Metrics\n\n");
    append_magnitude_summary(
        &mut summary,
        "Runtime Call Gas vs Yul (unoptimized)",
        call_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut summary,
        "Runtime Call Gas vs Yul (optimized)",
        call_magnitude_totals.vs_yul_opt,
    );
    summary.push_str("\n## Deploy/Call/Total Breakdown\n\n");
    append_magnitude_summary(
        &mut summary,
        "Deployment Gas vs Yul (unoptimized)",
        deploy_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut summary,
        "Deployment Gas vs Yul (optimized)",
        deploy_magnitude_totals.vs_yul_opt,
    );
    append_magnitude_summary(
        &mut summary,
        "Total Gas (deploy+call) vs Yul (unoptimized)",
        total_magnitude_totals.vs_yul_unopt,
    );
    append_magnitude_summary(
        &mut summary,
        "Total Gas (deploy+call) vs Yul (optimized)",
        total_magnitude_totals.vs_yul_opt,
    );
    append_deployment_attribution_summary(
        &mut summary,
        "Deployment Attribution (Step Replay, vs Yul optimized)",
        deployment_attribution_totals,
    );
    append_opcode_magnitude_summary(&mut summary, opcode_magnitude_totals);
    append_opcode_inflation_attribution(&mut summary, opcode_magnitude_totals);
    if wrote_any_rows {
        append_hotspot_summary(&mut summary, &hotspots, &suite_rollup_rows);
        append_trace_symbol_hotspots_summary(&mut summary, &trace_symbol_hotspots);
        append_trace_observability_hotspots_summary(&mut summary, &trace_observability_hotspots);
    }
    append_observability_coverage_summary(
        &mut summary,
        &observability_coverage_rows,
        observability_coverage_totals,
    );
    summary.push_str("\n## Optimization Settings\n\n");
    for line in gas_comparison_settings_text(opt_level).lines() {
        summary.push_str(&format!("- {line}\n"));
    }
    if wrote_any_rows {
        summary.push_str("\nSee `artifacts/gas_comparison_all.csv`, `artifacts/gas_comparison_totals.csv`, `artifacts/gas_comparison_magnitude.csv`, `artifacts/gas_breakdown_comparison_all.csv`, `artifacts/gas_breakdown_magnitude.csv`, `artifacts/gas_opcode_magnitude.csv`, `artifacts/gas_deployment_attribution_all.csv`, `artifacts/gas_hotspots_vs_yul_opt.csv`, `artifacts/gas_suite_delta_summary.csv`, `artifacts/gas_tail_trace_symbol_hotspots.csv`, and `artifacts/gas_tail_trace_observability_hotspots.csv` for machine-readable totals and rollups.\n");
    }
    if !observability_coverage_rows.is_empty() {
        summary.push_str(
            "See `artifacts/observability_coverage_all.csv` for per-test Sonatina observability coverage totals.\n",
        );
    }
    if wrote_any_opcode_rows {
        summary.push_str(
            "See `artifacts/gas_opcode_comparison_all.csv` for aggregated opcode and step-count diagnostics.\n",
        );
    }
    let _ = std::fs::write(artifacts_dir.join("gas_comparison_summary.md"), summary);
}

fn maybe_write_suite_ir(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    backend: &str,
    report: Option<&ReportContext>,
) {
    let Some(report) = report else {
        return;
    };

    let artifacts_dir = report.root_dir.join("artifacts");
    let _ = create_dir_all_utf8(&artifacts_dir);

    match lower_module(db, top_mod) {
        Ok(mir) => {
            let path = artifacts_dir.join("mir.txt");
            let _ = std::fs::write(&path, mir_fmt::format_module(db, &mir));
        }
        Err(err) => {
            let path = artifacts_dir.join("mir_error.txt");
            let _ = std::fs::write(&path, format!("{err}"));
        }
    }

    if backend.eq_ignore_ascii_case("sonatina") {
        match codegen::emit_module_sonatina_ir(db, top_mod) {
            Ok(ir) => {
                let path = artifacts_dir.join("sonatina_ir.txt");
                let _ = std::fs::write(&path, ir);
            }
            Err(err) => {
                let path = artifacts_dir.join("sonatina_ir_error.txt");
                let _ = std::fs::write(&path, format!("{err}"));
            }
        }

        match codegen::validate_module_sonatina_ir(db, top_mod) {
            Ok(report) => {
                let path = artifacts_dir.join("sonatina_validate.txt");
                let _ = std::fs::write(&path, report);
            }
            Err(err) => {
                let path = artifacts_dir.join("sonatina_validate_error.txt");
                let _ = std::fs::write(&path, format!("{err}"));
            }
        }
    } else if backend.eq_ignore_ascii_case("yul") {
        match codegen::emit_module_yul(db, top_mod) {
            Ok(yul) => {
                let path = artifacts_dir.join("yul_module.yul");
                let _ = std::fs::write(&path, yul);
            }
            Err(err) => {
                let path = artifacts_dir.join("yul_module_error.txt");
                let _ = std::fs::write(&path, format!("{err}"));
            }
        }
    }
}

fn suite_name_for_path(path: &Utf8PathBuf) -> String {
    let raw = if path.is_file() {
        path.file_stem()
            .map(|s| s.to_string())
            .unwrap_or_else(|| "tests".to_string())
    } else {
        path.file_name()
            .map(|s| s.to_string())
            .unwrap_or_else(|| "tests".to_string())
    };
    let sanitized = sanitize_filename(&raw);
    if sanitized.is_empty() {
        "tests".to_string()
    } else {
        sanitized
    }
}

fn expand_test_paths(inputs: &[Utf8PathBuf]) -> Result<Vec<Utf8PathBuf>, String> {
    let mut expanded = Vec::new();
    let mut seen: FxHashSet<String> = FxHashSet::default();

    for input in inputs {
        if input.exists() {
            let key = input.as_str().to_string();
            if seen.insert(key) {
                expanded.push(input.clone());
            }
            continue;
        }

        let pattern = input.as_str();
        if !looks_like_glob(pattern) {
            return Err(format!("path does not exist: {input}"));
        }

        let mut matches = Vec::new();
        let entries = glob::glob(pattern)
            .map_err(|err| format!("invalid glob pattern `{pattern}`: {err}"))?;
        for entry in entries {
            let path = entry.map_err(|err| format!("glob entry error for `{pattern}`: {err}"))?;
            let utf8 = Utf8PathBuf::from_path_buf(path)
                .map_err(|path| format!("non-utf8 path matched by `{pattern}`: {path:?}"))?;
            matches.push(utf8);
        }

        if matches.is_empty() {
            return Err(format!("glob pattern matched no paths: `{pattern}`"));
        }

        matches.sort();
        for path in matches {
            let key = path.as_str().to_string();
            if seen.insert(key) {
                expanded.push(path);
            }
        }
    }

    Ok(expanded)
}

fn looks_like_glob(pattern: &str) -> bool {
    pattern.contains('*') || pattern.contains('?') || pattern.contains('[')
}

fn create_run_report_staging() -> Result<ReportStaging, String> {
    create_report_staging_root("target/fe-test-report-staging", "fe-test-report")
}

fn create_suite_report_staging(suite: &str) -> Result<ReportStaging, String> {
    let name = format!("fe-test-report-{}", sanitize_filename(suite));
    create_report_staging_root("target/fe-test-report-staging", &name)
}

fn compile_and_run_test(
    case: &TestMetadata,
    show_logs: bool,
    backend: &str,
    yul_optimize: bool,
    evm_trace: Option<&EvmTraceOptions>,
    report: Option<&ReportContext>,
    call_trace: bool,
    collect_step_count: bool,
) -> TestOutcome {
    if case.value_param_count > 0 {
        return TestOutcome {
            result: TestResult {
                name: case.display_name.clone(),
                passed: false,
                error_message: Some(format!(
                    "tests with value parameters are not supported (found {})",
                    case.value_param_count
                )),
                gas_used: None,
                deploy_gas_used: None,
                total_gas_used: None,
            },
            logs: Vec::new(),
            trace: None,
            step_count: None,
            runtime_metrics: None,
            gas_profile: None,
        };
    }

    if case.object_name.trim().is_empty() {
        return TestOutcome {
            result: TestResult {
                name: case.display_name.clone(),
                passed: false,
                error_message: Some(format!(
                    "missing test object name for `{}`",
                    case.display_name
                )),
                gas_used: None,
                deploy_gas_used: None,
                total_gas_used: None,
            },
            logs: Vec::new(),
            trace: None,
            step_count: None,
            runtime_metrics: None,
            gas_profile: None,
        };
    }

    if backend == "sonatina" {
        if case.bytecode.is_empty() {
            return TestOutcome {
                result: TestResult {
                    name: case.display_name.clone(),
                    passed: false,
                    error_message: Some(format!(
                        "missing test bytecode for `{}`",
                        case.display_name
                    )),
                    gas_used: None,
                    deploy_gas_used: None,
                    total_gas_used: None,
                },
                logs: Vec::new(),
                trace: None,
                step_count: None,
                runtime_metrics: None,
                gas_profile: None,
            };
        }

        if let Some(report) = report {
            write_sonatina_case_artifacts(report, case);
        }

        let runtime_metrics = extract_runtime_from_sonatina_initcode(&case.bytecode)
            .map(evm_runtime_metrics_from_bytes);
        let bytecode_hex = hex::encode(&case.bytecode);
        let (result, logs, trace, step_count, gas_profile) = execute_test(
            &case.display_name,
            &bytecode_hex,
            show_logs,
            case.expected_revert.as_ref(),
            evm_trace,
            call_trace,
            collect_step_count,
        );
        return TestOutcome {
            result,
            logs,
            trace,
            step_count,
            runtime_metrics,
            gas_profile,
        };
    }

    // Default backend: compile Yul to bytecode using solc.
    if case.yul.trim().is_empty() {
        return TestOutcome {
            result: TestResult {
                name: case.display_name.clone(),
                passed: false,
                error_message: Some(format!("missing test Yul for `{}`", case.display_name)),
                gas_used: None,
                deploy_gas_used: None,
                total_gas_used: None,
            },
            logs: Vec::new(),
            trace: None,
            step_count: None,
            runtime_metrics: None,
            gas_profile: None,
        };
    }

    if let Some(report) = report {
        write_yul_case_artifacts(report, case);
    }

    let (bytecode, runtime_metrics) = match compile_single_contract(
        &case.object_name,
        &case.yul,
        yul_optimize,
        YUL_VERIFY_RUNTIME,
    ) {
        Ok(contract) => (
            contract.bytecode,
            evm_runtime_metrics_from_hex(&contract.runtime_bytecode),
        ),
        Err(err) => {
            return TestOutcome {
                result: TestResult {
                    name: case.display_name.clone(),
                    passed: false,
                    error_message: Some(format!("Failed to compile test: {}", err.0)),
                    gas_used: None,
                    deploy_gas_used: None,
                    total_gas_used: None,
                },
                logs: Vec::new(),
                trace: None,
                step_count: None,
                runtime_metrics: None,
                gas_profile: None,
            };
        }
    };

    // Execute the test bytecode in revm
    let (result, logs, trace, step_count, gas_profile) = execute_test(
        &case.display_name,
        &bytecode,
        show_logs,
        case.expected_revert.as_ref(),
        evm_trace,
        call_trace,
        collect_step_count,
    );
    TestOutcome {
        result,
        logs,
        trace,
        step_count,
        runtime_metrics,
        gas_profile,
    }
}

fn write_sonatina_case_artifacts(report: &ReportContext, case: &TestMetadata) {
    let dir = report
        .root_dir
        .join("artifacts")
        .join("tests")
        .join(sanitize_filename(&case.display_name))
        .join("sonatina");
    let _ = create_dir_all_utf8(&dir);

    let init_path = dir.join("initcode.hex");
    let _ = std::fs::write(&init_path, hex::encode(&case.bytecode));

    if let Some(runtime) = extract_runtime_from_sonatina_initcode(&case.bytecode) {
        let _ = std::fs::write(dir.join("runtime.bin"), runtime);
        let _ = std::fs::write(dir.join("runtime.hex"), hex::encode(runtime));
    }

    if let Some(text) = &case.sonatina_observability_text {
        let _ = std::fs::write(dir.join("observability.txt"), text);
    }
    if let Some(json) = &case.sonatina_observability_json {
        let _ = std::fs::write(dir.join("observability.json"), json);
    }
}

fn write_yul_case_artifacts(report: &ReportContext, case: &TestMetadata) {
    let dir = report
        .root_dir
        .join("artifacts")
        .join("tests")
        .join(sanitize_filename(&case.display_name))
        .join("yul");
    let _ = create_dir_all_utf8(&dir);

    let _ = std::fs::write(dir.join("source.yul"), &case.yul);

    let unopt = compile_single_contract(&case.object_name, &case.yul, false, YUL_VERIFY_RUNTIME);
    if let Ok(contract) = unopt {
        let _ = std::fs::write(dir.join("bytecode.unopt.hex"), &contract.bytecode);
        let _ = std::fs::write(dir.join("runtime.unopt.hex"), &contract.runtime_bytecode);
    }

    let opt = compile_single_contract(&case.object_name, &case.yul, true, YUL_VERIFY_RUNTIME);
    if let Ok(contract) = opt {
        let _ = std::fs::write(dir.join("bytecode.opt.hex"), &contract.bytecode);
        let _ = std::fs::write(dir.join("runtime.opt.hex"), &contract.runtime_bytecode);
    }
}

fn extract_runtime_from_sonatina_initcode(init: &[u8]) -> Option<&[u8]> {
    // Matches the init code produced by `fe-codegen` Sonatina tests:
    // PUSHn <len>, PUSH2 <off>, PUSH1 0, CODECOPY, PUSHn <len>, PUSH1 0, RETURN, <runtime...>
    //
    // Returns the appended runtime slice if parsing succeeds.
    let mut idx = 0;
    let push_opcode = *init.get(idx)?;
    if !(0x60..=0x7f).contains(&push_opcode) {
        return None;
    }
    let len_n = (push_opcode - 0x5f) as usize;
    idx += 1;
    if idx + len_n > init.len() {
        return None;
    }
    let mut len: usize = 0;
    for &b in init.get(idx..idx + len_n)? {
        len = (len << 8) | (b as usize);
    }
    idx += len_n;

    if *init.get(idx)? != 0x61 {
        return None;
    }
    idx += 1;
    let off_hi = *init.get(idx)? as usize;
    let off_lo = *init.get(idx + 1)? as usize;
    let off = (off_hi << 8) | off_lo;
    if off > init.len() {
        return None;
    }
    if off + len > init.len() {
        return None;
    }
    Some(&init[off..off + len])
}

fn write_report_manifest(
    staging: &Utf8PathBuf,
    backend: &str,
    opt_level: OptLevel,
    filter: Option<&str>,
    results: &[TestResult],
) {
    let mut out = String::new();
    out.push_str("fe test report\n");
    out.push_str(&format!("backend: {backend}\n"));
    out.push_str(&format!("opt_level: {opt_level}\n"));
    out.push_str(&format!("filter: {}\n", filter.unwrap_or("<none>")));
    out.push_str(&format!("fe_version: {}\n", env!("CARGO_PKG_VERSION")));
    out.push_str("details: see `meta/args.txt` and `meta/git.txt` for exact repro context\n");
    out.push_str("gas_comparison: see `artifacts/gas_comparison.md`, `artifacts/gas_comparison.csv`, `artifacts/gas_comparison_totals.csv`, `artifacts/gas_comparison_magnitude.csv`, `artifacts/gas_breakdown_comparison.csv`, `artifacts/gas_breakdown_magnitude.csv`, `artifacts/gas_opcode_magnitude.csv`, `artifacts/gas_deployment_attribution.csv`, and `artifacts/gas_comparison_settings.txt` when available\n");
    out.push_str("gas_comparison_yul_artifacts: in Sonatina comparison runs, Yul baselines are stored under `artifacts/tests/<test>/yul/{source.yul,bytecode.unopt.hex,bytecode.opt.hex,runtime.unopt.hex,runtime.opt.hex}`\n");
    out.push_str("sonatina_observability: when available, Sonatina test artifacts include `artifacts/tests/<test>/sonatina/{observability.txt,observability.json}`\n");
    out.push_str("gas_comparison_aggregate: run-level reports also include `artifacts/gas_comparison_all.csv`, `artifacts/gas_breakdown_comparison_all.csv`, `artifacts/gas_comparison_summary.md`, `artifacts/gas_comparison_magnitude.csv`, `artifacts/gas_breakdown_magnitude.csv`, `artifacts/gas_opcode_magnitude.csv`, `artifacts/gas_deployment_attribution_all.csv`, `artifacts/gas_hotspots_vs_yul_opt.csv`, `artifacts/gas_suite_delta_summary.csv`, `artifacts/gas_tail_trace_symbol_hotspots.csv`, and `artifacts/gas_tail_trace_observability_hotspots.csv`\n");
    out.push_str("sonatina_observability_aggregate: run-level reports also include `artifacts/observability_coverage_all.csv` for per-test coverage totals from observability maps\n");
    out.push_str("gas_opcode_profile: see `artifacts/gas_opcode_comparison.md` and `artifacts/gas_opcode_comparison.csv` for opcode and step-count diagnostics when available\n");
    out.push_str("gas_opcode_profile_aggregate: run-level reports also include `artifacts/gas_opcode_comparison_all.csv`\n");
    out.push_str("sonatina_evm_debug: when available, see `debug/sonatina_evm_bytecode.txt` for stackify traces and lowered EVM vcode output\n");
    out.push_str(&format!("tests: {}\n", results.len()));
    let passed = results.iter().filter(|r| r.passed).count();
    out.push_str(&format!("passed: {passed}\n"));
    out.push_str(&format!("failed: {}\n", results.len() - passed));
    out.push_str("\nfailures:\n");
    for r in results.iter().filter(|r| !r.passed) {
        out.push_str(&format!("- {}\n", r.name));
        if let Some(msg) = &r.error_message {
            out.push_str(&format!("  {}\n", msg));
        }
    }
    let _ = std::fs::write(staging.join("manifest.txt"), out);
}

/// Deploys and executes compiled test bytecode in revm.
///
/// The test passes if the function returns normally, fails if it reverts.
///
/// * `name` - Display name used for reporting.
/// * `bytecode_hex` - Hex-encoded init bytecode for the test object.
/// * `show_logs` - Whether to execute with log collection enabled.
///
/// Returns the test result and any emitted logs.
fn execute_test(
    name: &str,
    bytecode_hex: &str,
    show_logs: bool,
    expected_revert: Option<&ExpectedRevert>,
    evm_trace: Option<&EvmTraceOptions>,
    call_trace: bool,
    collect_step_count: bool,
) -> (
    TestResult,
    Vec<String>,
    Option<contract_harness::CallTrace>,
    Option<u64>,
    Option<CallGasProfile>,
) {
    // Deploy the test contract
    let (mut instance, deploy_gas_used) = match RuntimeInstance::deploy_tracked(bytecode_hex) {
        Ok(deployed) => deployed,
        Err(err) => {
            let deploy_gas_used = harness_error_gas_used(&err);
            return (
                TestResult {
                    name: name.to_string(),
                    passed: false,
                    error_message: Some(format!("Failed to deploy test: {err}")),
                    gas_used: None,
                    deploy_gas_used,
                    total_gas_used: deploy_gas_used,
                },
                Vec::new(),
                None,
                None,
                None,
            );
        }
    };
    instance.set_trace_options(evm_trace.cloned());

    // Execute the test (empty calldata since test functions take no args)
    let options = ExecutionOptions::default();

    // Capture call trace BEFORE the real execution so the cloned context
    // has the right pre-call state (contract deployed but not yet called).
    let trace = if call_trace {
        Some(instance.call_raw_traced(&[], options))
    } else {
        None
    };
    let gas_profile = if collect_step_count {
        Some(instance.call_raw_gas_profile(&[], options))
    } else {
        None
    };
    let step_count = gas_profile.map(|profile| profile.step_count);

    let call_result = if show_logs {
        instance
            .call_raw_with_logs(&[], options)
            .map(|outcome| (outcome.result.gas_used, outcome.logs))
    } else {
        instance
            .call_raw(&[], options)
            .map(|result| (result.gas_used, Vec::new()))
    };

    match (call_result, expected_revert) {
        // Normal test: execution succeeded
        (Ok((gas_used, logs)), None) => {
            let total_gas_used = Some(deploy_gas_used.saturating_add(gas_used));
            (
                TestResult {
                    name: name.to_string(),
                    passed: true,
                    error_message: None,
                    gas_used: Some(gas_used),
                    deploy_gas_used: Some(deploy_gas_used),
                    total_gas_used,
                },
                logs,
                trace,
                step_count,
                gas_profile,
            )
        }
        // Normal test: execution reverted (failure)
        (Err(err), None) => {
            let gas_used = harness_error_gas_used(&err);
            let total_gas_used = gas_used.map(|call_gas| deploy_gas_used.saturating_add(call_gas));
            (
                TestResult {
                    name: name.to_string(),
                    passed: false,
                    error_message: Some(format_harness_error(err)),
                    gas_used,
                    deploy_gas_used: Some(deploy_gas_used),
                    total_gas_used,
                },
                Vec::new(),
                trace,
                step_count,
                gas_profile,
            )
        }
        // Expected revert: execution succeeded (failure - should have reverted)
        (Ok((gas_used, _)), Some(_)) => {
            let total_gas_used = Some(deploy_gas_used.saturating_add(gas_used));
            (
                TestResult {
                    name: name.to_string(),
                    passed: false,
                    error_message: Some("Expected test to revert, but it succeeded".to_string()),
                    gas_used: Some(gas_used),
                    deploy_gas_used: Some(deploy_gas_used),
                    total_gas_used,
                },
                Vec::new(),
                trace,
                step_count,
                gas_profile,
            )
        }
        // Expected revert: execution reverted (success)
        (Err(contract_harness::HarnessError::Revert(_)), Some(ExpectedRevert::Any)) => (
            TestResult {
                name: name.to_string(),
                passed: true,
                error_message: None,
                gas_used: None,
                deploy_gas_used: Some(deploy_gas_used),
                total_gas_used: None,
            },
            Vec::new(),
            trace,
            step_count,
            gas_profile,
        ),
        // Expected revert: execution failed for a different reason (failure)
        (Err(err), Some(ExpectedRevert::Any)) => {
            let gas_used = harness_error_gas_used(&err);
            let total_gas_used = gas_used.map(|call_gas| deploy_gas_used.saturating_add(call_gas));
            (
                TestResult {
                    name: name.to_string(),
                    passed: false,
                    error_message: Some(format!(
                        "Expected test to revert, but it failed with: {}",
                        format_harness_error(err)
                    )),
                    gas_used,
                    deploy_gas_used: Some(deploy_gas_used),
                    total_gas_used,
                },
                Vec::new(),
                trace,
                step_count,
                gas_profile,
            )
        }
    }
}

/// Formats a harness error into a human-readable message.
fn format_harness_error(err: contract_harness::HarnessError) -> String {
    match err {
        contract_harness::HarnessError::Revert(data) => format!("Test reverted: {data}"),
        contract_harness::HarnessError::Halted { reason, gas_used } => {
            format!("Test halted: {reason:?} (gas: {gas_used})")
        }
        other => format!("Test execution error: {other}"),
    }
}

fn harness_error_gas_used(err: &contract_harness::HarnessError) -> Option<u64> {
    match err {
        contract_harness::HarnessError::Halted { gas_used, .. } => Some(*gas_used),
        _ => None,
    }
}

/// Prints a summary for the completed test run.
///
/// * `results` - Per-test results to summarize.
///
/// Returns nothing.
fn print_summary(results: &[TestResult]) {
    if results.is_empty() {
        return;
    }

    let passed = results.iter().filter(|r| r.passed).count();
    let failed = results.len() - passed;

    println!();
    if failed == 0 {
        println!(
            "test result: {}. {} passed; {} failed",
            "ok".green(),
            passed,
            failed
        );
    } else {
        println!(
            "test result: {}. {} passed; {} failed",
            "FAILED".red(),
            passed,
            failed
        );

        // Print failed tests
        println!();
        println!("failures:");
        for result in results.iter().filter(|r| !r.passed) {
            println!("    {}", result.name);
        }
    }
}
