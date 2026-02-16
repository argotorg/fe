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
use solc_runner::compile_single_contract;
use std::{
    fmt::Write as _,
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc,
    },
};
use url::Url;

mod gas;

pub(super) const YUL_VERIFY_RUNTIME: bool = true;

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
pub(super) struct TestOutcome {
    result: TestResult,
    logs: Vec<String>,
    trace: Option<contract_harness::CallTrace>,
    step_count: Option<u64>,
    runtime_metrics: Option<EvmRuntimeMetrics>,
    gas_profile: Option<CallGasProfile>,
}

#[derive(Debug, Clone)]
pub(super) struct GasComparisonCase {
    pub(super) display_name: String,
    pub(super) symbol_name: String,
    pub(super) yul: Option<TestMetadata>,
    pub(super) sonatina: Option<TestMetadata>,
}

#[derive(Debug, Clone)]
pub(super) struct GasMeasurement {
    pub(super) gas_used: Option<u64>,
    pub(super) deploy_gas_used: Option<u64>,
    pub(super) total_gas_used: Option<u64>,
    pub(super) step_count: Option<u64>,
    pub(super) runtime_metrics: Option<EvmRuntimeMetrics>,
    pub(super) gas_profile: Option<CallGasProfile>,
    pub(super) passed: bool,
    pub(super) error_message: Option<String>,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct EvmRuntimeMetrics {
    pub(super) byte_len: usize,
    pub(super) op_count: usize,
    pub(super) push_ops: usize,
    pub(super) dup_ops: usize,
    pub(super) swap_ops: usize,
    pub(super) pop_ops: usize,
    pub(super) jump_ops: usize,
    pub(super) jumpi_ops: usize,
    pub(super) jumpdest_ops: usize,
    pub(super) iszero_ops: usize,
    pub(super) mload_ops: usize,
    pub(super) mstore_ops: usize,
    pub(super) sload_ops: usize,
    pub(super) sstore_ops: usize,
    pub(super) keccak_ops: usize,
    pub(super) call_ops: usize,
    pub(super) staticcall_ops: usize,
    pub(super) returndatacopy_ops: usize,
    pub(super) calldatacopy_ops: usize,
    pub(super) mcopy_ops: usize,
    pub(super) return_ops: usize,
    pub(super) revert_ops: usize,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct ComparisonTotals {
    pub(super) compared_with_gas: usize,
    pub(super) sonatina_lower: usize,
    pub(super) sonatina_higher: usize,
    pub(super) equal: usize,
    pub(super) incomplete: usize,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct GasTotals {
    pub(super) tests_in_scope: usize,
    pub(super) vs_yul_unopt: ComparisonTotals,
    pub(super) vs_yul_opt: ComparisonTotals,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct DeltaMagnitudeTotals {
    pub(super) compared_with_gas: usize,
    pub(super) pct_rows: usize,
    pub(super) baseline_gas_sum: u128,
    pub(super) sonatina_gas_sum: u128,
    pub(super) delta_gas_sum: i128,
    pub(super) abs_delta_gas_sum: u128,
    pub(super) delta_pct_sum: f64,
    pub(super) abs_delta_pct_sum: f64,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct GasMagnitudeTotals {
    pub(super) vs_yul_unopt: DeltaMagnitudeTotals,
    pub(super) vs_yul_opt: DeltaMagnitudeTotals,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct OpcodeAggregateTotals {
    pub(super) steps_sum: u128,
    pub(super) runtime_bytes_sum: u128,
    pub(super) runtime_ops_sum: u128,
    pub(super) swap_ops_sum: u128,
    pub(super) pop_ops_sum: u128,
    pub(super) jump_ops_sum: u128,
    pub(super) jumpi_ops_sum: u128,
    pub(super) iszero_ops_sum: u128,
    pub(super) mem_rw_ops_sum: u128,
    pub(super) storage_rw_ops_sum: u128,
    pub(super) mload_ops_sum: u128,
    pub(super) mstore_ops_sum: u128,
    pub(super) sload_ops_sum: u128,
    pub(super) sstore_ops_sum: u128,
    pub(super) keccak_ops_sum: u128,
    pub(super) call_family_ops_sum: u128,
    pub(super) copy_ops_sum: u128,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct OpcodeMagnitudeTotals {
    pub(super) compared_with_metrics: usize,
    pub(super) yul_opt: OpcodeAggregateTotals,
    pub(super) sonatina: OpcodeAggregateTotals,
}

#[derive(Debug, Clone)]
pub(super) struct GasHotspotRow {
    pub(super) suite: String,
    pub(super) test: String,
    pub(super) symbol: String,
    pub(super) yul_opt_gas: Option<u64>,
    pub(super) sonatina_gas: Option<u64>,
    pub(super) delta_vs_yul_opt: i128,
    pub(super) delta_vs_yul_opt_pct: String,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct SuiteDeltaTotals {
    pub(super) tests_with_delta: usize,
    pub(super) delta_vs_yul_opt_sum: i128,
}

#[derive(Debug, Clone)]
pub(super) struct SymtabEntry {
    pub(super) start: u32,
    pub(super) end: u32,
    pub(super) symbol: String,
}

#[derive(Debug, Clone)]
pub(super) struct TraceSymbolHotspotRow {
    pub(super) suite: String,
    pub(super) test: String,
    pub(super) symbol: String,
    pub(super) tail_steps_total: usize,
    pub(super) tail_steps_mapped: usize,
    pub(super) steps_in_symbol: usize,
}

#[derive(Debug, Clone)]
pub(super) struct ObservabilityCoverageRow {
    pub(super) suite: String,
    pub(super) test: String,
    pub(super) section: String,
    pub(super) schema_version: String,
    pub(super) section_bytes: u64,
    pub(super) code_bytes: u64,
    pub(super) data_bytes: u64,
    pub(super) embed_bytes: u64,
    pub(super) mapped_code_bytes: u64,
    pub(super) unmapped_code_bytes: u64,
    pub(super) unmapped_no_ir_inst: u64,
    pub(super) unmapped_label_or_fixup_only: u64,
    pub(super) unmapped_synthetic: u64,
    pub(super) unmapped_unknown: u64,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct ObservabilityCoverageTotals {
    pub(super) section_bytes: u128,
    pub(super) code_bytes: u128,
    pub(super) data_bytes: u128,
    pub(super) embed_bytes: u128,
    pub(super) mapped_code_bytes: u128,
    pub(super) unmapped_code_bytes: u128,
    pub(super) unmapped_no_ir_inst: u128,
    pub(super) unmapped_label_or_fixup_only: u128,
    pub(super) unmapped_synthetic: u128,
    pub(super) unmapped_unknown: u128,
}

#[derive(Debug, Clone)]
pub(super) struct ObservabilityPcRange {
    pub(super) start: u32,
    pub(super) end: u32,
    pub(super) func_name: String,
    pub(super) reason: Option<String>,
}

#[derive(Debug, Clone)]
pub(super) struct ObservabilityRuntimeSnapshot {
    pub(super) section: String,
    pub(super) schema_version: String,
    pub(super) section_bytes: u64,
    pub(super) code_bytes: u64,
    pub(super) data_bytes: u64,
    pub(super) embed_bytes: u64,
    pub(super) mapped_code_bytes: u64,
    pub(super) unmapped_code_bytes: u64,
    pub(super) unmapped_no_ir_inst: u64,
    pub(super) unmapped_label_or_fixup_only: u64,
    pub(super) unmapped_synthetic: u64,
    pub(super) unmapped_unknown: u64,
    pub(super) pc_ranges: Vec<ObservabilityPcRange>,
}

#[derive(Debug, Clone)]
pub(super) struct TraceObservabilityHotspotRow {
    pub(super) suite: String,
    pub(super) test: String,
    pub(super) function: String,
    pub(super) reason: String,
    pub(super) tail_steps_total: usize,
    pub(super) tail_steps_mapped: usize,
    pub(super) steps_in_bucket: usize,
}

#[derive(Debug, Clone)]
pub(super) struct DeploymentGasAttributionRow {
    pub(super) test: String,
    pub(super) symbol: String,
    pub(super) yul_unopt_step_total_gas: Option<u64>,
    pub(super) yul_opt_step_total_gas: Option<u64>,
    pub(super) sonatina_step_total_gas: Option<u64>,
    pub(super) yul_unopt_create_opcode_gas: Option<u64>,
    pub(super) yul_opt_create_opcode_gas: Option<u64>,
    pub(super) sonatina_create_opcode_gas: Option<u64>,
    pub(super) yul_unopt_create2_opcode_gas: Option<u64>,
    pub(super) yul_opt_create2_opcode_gas: Option<u64>,
    pub(super) sonatina_create2_opcode_gas: Option<u64>,
    pub(super) yul_unopt_constructor_frame_gas: Option<u64>,
    pub(super) yul_opt_constructor_frame_gas: Option<u64>,
    pub(super) sonatina_constructor_frame_gas: Option<u64>,
    pub(super) yul_unopt_non_constructor_frame_gas: Option<u64>,
    pub(super) yul_opt_non_constructor_frame_gas: Option<u64>,
    pub(super) sonatina_non_constructor_frame_gas: Option<u64>,
    pub(super) yul_unopt_create_opcode_steps: Option<u64>,
    pub(super) yul_opt_create_opcode_steps: Option<u64>,
    pub(super) sonatina_create_opcode_steps: Option<u64>,
    pub(super) yul_unopt_create2_opcode_steps: Option<u64>,
    pub(super) yul_opt_create2_opcode_steps: Option<u64>,
    pub(super) sonatina_create2_opcode_steps: Option<u64>,
    pub(super) note: String,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct DeploymentGasAttributionTotals {
    pub(super) compared_with_profile: usize,
    pub(super) yul_opt_step_total_gas: u128,
    pub(super) sonatina_step_total_gas: u128,
    pub(super) yul_opt_create_opcode_gas: u128,
    pub(super) sonatina_create_opcode_gas: u128,
    pub(super) yul_opt_create2_opcode_gas: u128,
    pub(super) sonatina_create2_opcode_gas: u128,
    pub(super) yul_opt_constructor_frame_gas: u128,
    pub(super) sonatina_constructor_frame_gas: u128,
    pub(super) yul_opt_non_constructor_frame_gas: u128,
    pub(super) sonatina_non_constructor_frame_gas: u128,
}

pub(super) const DEPLOYMENT_ATTRIBUTION_CSV_HEADER: &str = "test,symbol,yul_unopt_step_total_gas,yul_opt_step_total_gas,sonatina_step_total_gas,yul_unopt_create_opcode_gas,yul_opt_create_opcode_gas,sonatina_create_opcode_gas,yul_unopt_create2_opcode_gas,yul_opt_create2_opcode_gas,sonatina_create2_opcode_gas,yul_unopt_constructor_frame_gas,yul_opt_constructor_frame_gas,sonatina_constructor_frame_gas,yul_unopt_non_constructor_frame_gas,yul_opt_non_constructor_frame_gas,sonatina_non_constructor_frame_gas,yul_unopt_create_opcode_steps,yul_opt_create_opcode_steps,sonatina_create_opcode_steps,yul_unopt_create2_opcode_steps,yul_opt_create2_opcode_steps,sonatina_create2_opcode_steps,note";
pub(super) const DEPLOYMENT_ATTRIBUTION_CSV_HEADER_WITH_SUITE: &str = "suite,test,symbol,yul_unopt_step_total_gas,yul_opt_step_total_gas,sonatina_step_total_gas,yul_unopt_create_opcode_gas,yul_opt_create_opcode_gas,sonatina_create_opcode_gas,yul_unopt_create2_opcode_gas,yul_opt_create2_opcode_gas,sonatina_create2_opcode_gas,yul_unopt_constructor_frame_gas,yul_opt_constructor_frame_gas,sonatina_constructor_frame_gas,yul_unopt_non_constructor_frame_gas,yul_opt_non_constructor_frame_gas,sonatina_non_constructor_frame_gas,yul_unopt_create_opcode_steps,yul_opt_create_opcode_steps,sonatina_create_opcode_steps,yul_unopt_create2_opcode_steps,yul_opt_create2_opcode_steps,sonatina_create2_opcode_steps,note";
pub(super) const DEPLOYMENT_ATTRIBUTION_FIELD_COUNT: usize = 24;

impl GasMeasurement {
    pub(super) fn from_test_outcome(outcome: &TestOutcome) -> Self {
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

    pub(super) fn status_label(&self) -> String {
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
    pub(super) fn add_observation(&mut self, yul_opt: CallGasProfile, sonatina: CallGasProfile) {
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
    pub(super) fn add(&mut self, other: Self) {
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
    pub(super) fn add_row(&mut self, row: &ObservabilityCoverageRow) {
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
    pub(super) fn add(&mut self, other: Self) {
        self.compared_with_gas += other.compared_with_gas;
        self.pct_rows += other.pct_rows;
        self.baseline_gas_sum += other.baseline_gas_sum;
        self.sonatina_gas_sum += other.sonatina_gas_sum;
        self.delta_gas_sum += other.delta_gas_sum;
        self.abs_delta_gas_sum += other.abs_delta_gas_sum;
        self.delta_pct_sum += other.delta_pct_sum;
        self.abs_delta_pct_sum += other.abs_delta_pct_sum;
    }

    pub(super) fn mean_delta_gas(self) -> Option<f64> {
        if self.compared_with_gas == 0 {
            None
        } else {
            Some(self.delta_gas_sum as f64 / self.compared_with_gas as f64)
        }
    }

    pub(super) fn mean_abs_delta_gas(self) -> Option<f64> {
        if self.compared_with_gas == 0 {
            None
        } else {
            Some(self.abs_delta_gas_sum as f64 / self.compared_with_gas as f64)
        }
    }

    pub(super) fn mean_delta_pct(self) -> Option<f64> {
        if self.pct_rows == 0 {
            None
        } else {
            Some(self.delta_pct_sum / self.pct_rows as f64)
        }
    }

    pub(super) fn mean_abs_delta_pct(self) -> Option<f64> {
        if self.pct_rows == 0 {
            None
        } else {
            Some(self.abs_delta_pct_sum / self.pct_rows as f64)
        }
    }

    pub(super) fn weighted_delta_pct(self) -> Option<f64> {
        if self.baseline_gas_sum == 0 {
            None
        } else {
            Some(self.delta_gas_sum as f64 * 100.0 / self.baseline_gas_sum as f64)
        }
    }
}

impl GasMagnitudeTotals {
    pub(super) fn add(&mut self, other: Self) {
        self.vs_yul_unopt.add(other.vs_yul_unopt);
        self.vs_yul_opt.add(other.vs_yul_opt);
    }
}

impl OpcodeAggregateTotals {
    pub(super) fn add_observation(&mut self, steps: u64, metrics: EvmRuntimeMetrics) {
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

    pub(super) fn add(&mut self, other: Self) {
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
    pub(super) fn add(&mut self, other: Self) {
        self.compared_with_metrics += other.compared_with_metrics;
        self.yul_opt.add(other.yul_opt);
        self.sonatina.add(other.sonatina);
    }
}

impl EvmRuntimeMetrics {
    pub(super) fn stack_ops_total(self) -> usize {
        self.push_ops + self.dup_ops + self.swap_ops + self.pop_ops
    }

    pub(super) fn mem_rw_ops_total(self) -> usize {
        self.mload_ops + self.mstore_ops
    }

    pub(super) fn storage_rw_ops_total(self) -> usize {
        self.sload_ops + self.sstore_ops
    }

    pub(super) fn call_family_ops_total(self) -> usize {
        self.call_ops + self.staticcall_ops
    }

    pub(super) fn copy_ops_total(self) -> usize {
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

pub(super) fn write_report_error(report: &ReportContext, filename: &str, contents: &str) {
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
pub(super) struct ReportContext {
    pub(super) root_dir: Utf8PathBuf,
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
        gas::write_run_gas_comparison_summary(&staging.root_dir, opt_level);
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
    let canonical = match file_path.canonicalize_utf8() {
        Ok(p) => p,
        Err(e) => {
            return suite_error_result(suite, "setup", format!("Cannot canonicalize {file_path}: {e}"));
        }
    };
    let file_url = match Url::from_file_path(&canonical) {
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
pub(super) fn emit_with_catch_unwind<E: std::fmt::Display>(
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
        gas::collect_gas_comparison_cases(
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
        gas::write_gas_comparison_report(
            report,
            backend.as_str(),
            opt_level,
            cases,
            &primary_measurements,
        );
    }

    results
}

pub(super) fn test_case_matches_filter(case: &TestMetadata, filter: Option<&str>) -> bool {
    let Some(pattern) = filter else {
        return true;
    };
    case.hir_name.contains(pattern)
        || case.symbol_name.contains(pattern)
        || case.display_name.contains(pattern)
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

pub(super) fn compile_and_run_test(
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
            .map(gas::evm_runtime_metrics_from_bytes);
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
            gas::evm_runtime_metrics_from_hex(&contract.runtime_bytecode),
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

pub(super) fn write_yul_case_artifacts(report: &ReportContext, case: &TestMetadata) {
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
