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
    ExpectedRevert, TestMetadata, TestModuleOutput, emit_test_module_sonatina, emit_test_module_yul,
};
use colored::Colorize;
use common::InputDb;
use contract_harness::{ExecutionOptions, RuntimeInstance};
use driver::DriverDataBase;
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::{fmt as mir_fmt, lower_module};
use rustc_hash::{FxHashMap, FxHashSet};
use solc_runner::compile_single_contract;
use url::Url;

const PRIMARY_YUL_OPTIMIZE: bool = true;
const YUL_VERIFY_RUNTIME: bool = true;

fn install_report_panic_hook(report: &ReportContext, filename: &str) -> PanicReportGuard {
    let dir = report.root_dir.join("errors");
    let _ = create_dir_all_utf8(&dir);
    let path = dir.join(filename);
    enable_panic_report(path)
}

/// Result of running a single test.
#[derive(Debug)]
pub struct TestResult {
    pub name: String,
    pub passed: bool,
    pub error_message: Option<String>,
    pub gas_used: Option<u64>,
}

#[derive(Debug)]
struct TestOutcome {
    result: TestResult,
    logs: Vec<String>,
    trace: Option<contract_harness::CallTrace>,
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
    passed: bool,
    error_message: Option<String>,
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

impl GasMeasurement {
    fn from_test_result(result: &TestResult) -> Self {
        Self {
            gas_used: result.gas_used,
            passed: result.passed,
            error_message: result.error_message.clone(),
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

fn suite_error_result(suite: &str, kind: &str, message: String) -> Vec<TestResult> {
    vec![TestResult {
        name: format!("{suite}::{kind}"),
        passed: false,
        error_message: Some(message),
        gas_used: None,
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

#[derive(Debug, Clone, Default)]
pub struct TestDebugOptions {
    pub trace_evm: bool,
    pub trace_evm_keep: usize,
    pub trace_evm_stack_n: usize,
    pub sonatina_symtab: bool,
    pub sonatina_stackify_trace: bool,
    pub sonatina_stackify_filter: Option<String>,
    pub sonatina_transient_malloc_trace: bool,
    pub sonatina_transient_malloc_filter: Option<String>,
    pub debug_dir: Option<Utf8PathBuf>,
}

impl TestDebugOptions {
    fn set_env<K: AsRef<std::ffi::OsStr>, V: AsRef<std::ffi::OsStr>>(key: K, value: V) {
        unsafe { std::env::set_var(key, value) }
    }

    fn configure_process_env(&self) -> Result<(), String> {
        if self.trace_evm {
            Self::set_env("FE_TRACE_EVM", "1");
            Self::set_env("FE_TRACE_EVM_KEEP", self.trace_evm_keep.to_string());
            Self::set_env("FE_TRACE_EVM_STACK_N", self.trace_evm_stack_n.to_string());
        }

        if self.sonatina_symtab {
            Self::set_env("FE_SONATINA_DUMP_SYMTAB", "1");
        }

        if self.sonatina_stackify_trace {
            Self::set_env("SONATINA_STACKIFY_TRACE", "1");
            if let Some(filter) = &self.sonatina_stackify_filter {
                Self::set_env("SONATINA_STACKIFY_TRACE_FUNC", filter);
            }
        }

        if self.sonatina_transient_malloc_trace {
            Self::set_env("SONATINA_TRANSIENT_MALLOC_TRACE", "1");
            if let Some(filter) = &self.sonatina_transient_malloc_filter {
                Self::set_env("SONATINA_TRANSIENT_MALLOC_TRACE_FUNC", filter);
            }
        }

        let Some(dir) = &self.debug_dir else {
            return Ok(());
        };

        std::fs::create_dir_all(dir)
            .map_err(|err| format!("failed to create debug dir `{dir}`: {err}"))?;

        if self.trace_evm {
            // When writing traces to files, suppress stderr spam by default.
            Self::set_env("FE_TRACE_EVM_STDERR", "0");
        }

        if self.sonatina_symtab {
            let path = dir.join("sonatina_symtab.txt");
            truncate_file(&path)?;
            Self::set_env("FE_SONATINA_DUMP_SYMTAB_OUT", path.as_str());
        }

        if self.sonatina_stackify_trace {
            let path = dir.join("sonatina_stackify_trace.txt");
            truncate_file(&path)?;
            Self::set_env("SONATINA_STACKIFY_TRACE_OUT", path.as_str());
        }

        if self.sonatina_transient_malloc_trace {
            let path = dir.join("sonatina_transient_malloc_trace.txt");
            truncate_file(&path)?;
            Self::set_env("SONATINA_TRANSIENT_MALLOC_TRACE_OUT", path.as_str());
        }

        Ok(())
    }

    fn configure_per_test_env(&self, test_suite: Option<&str>, test_name: &str) {
        let Some(dir) = &self.debug_dir else {
            return;
        };
        if !self.trace_evm {
            return;
        }

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
        if let Err(err) = truncate_file(&path) {
            eprintln!("warning: {err}");
        }
        Self::set_env("FE_TRACE_EVM_OUT", path.as_str());
    }
}

fn truncate_file(path: &Utf8PathBuf) -> Result<(), String> {
    std::fs::write(path, "").map_err(|err| format!("failed to truncate `{path}`: {err}"))
}

fn unique_report_path(dir: &Utf8PathBuf, suite: &str) -> Utf8PathBuf {
    let base = sanitize_filename(suite);
    let base = if base.is_empty() {
        "tests".to_string()
    } else {
        base
    };
    let mut candidate = dir.join(format!("{base}.tar.gz"));
    if !candidate.exists() {
        return candidate;
    }

    for idx in 1.. {
        candidate = dir.join(format!("{base}-{idx}.tar.gz"));
        if !candidate.exists() {
            return candidate;
        }
    }
    unreachable!()
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
    show_logs: bool,
    backend: &str,
    debug: &TestDebugOptions,
    report_out: Option<&Utf8PathBuf>,
    report_dir: Option<&Utf8PathBuf>,
    report_failed_only: bool,
    call_trace: bool,
) -> Result<bool, String> {
    let input_paths = expand_test_paths(paths)?;

    let mut test_results = Vec::new();
    let multi = input_paths.len() > 1;
    if multi {
        println!("running `fe test` for {} inputs\n", input_paths.len());
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
        create_dir_all_utf8(&root.join("tmp"))?;
        write_report_meta(root, "fe test report", None);
    }

    for path in input_paths {
        let suite = suite_name_for_path(&path);

        if multi {
            println!("==> {path}");
        }

        let suite_report = report_dir
            .map(|dir| -> Result<_, String> {
                let staging = create_suite_report_staging(&suite)?;
                let out = unique_report_path(dir, &suite);
                Ok((out, staging))
            })
            .transpose()?;

        let report_ctx = if let Some((_, staging)) = suite_report.as_ref() {
            let suite_dir = staging.root_dir.clone();
            write_report_meta(&suite_dir, "fe test report (suite)", Some(&suite));
            let inputs_dir = suite_dir.join("inputs");
            create_dir_all_utf8(&inputs_dir)?;
            copy_input_into_report(&path, &inputs_dir)?;
            Some(ReportContext {
                root_dir: suite_dir,
            })
        } else if let Some((_, staging)) = report_root.as_ref() {
            let root = &staging.root_dir;
            let suite_dir = root.join("tmp").join(&suite);
            create_dir_all_utf8(&suite_dir)?;
            write_report_meta(&suite_dir, "fe test report (suite)", Some(&suite));
            let inputs_dir = suite_dir.join("inputs");
            create_dir_all_utf8(&inputs_dir)?;
            copy_input_into_report(&path, &inputs_dir)?;
            Some(ReportContext {
                root_dir: suite_dir,
            })
        } else {
            None
        };

        let mut suite_debug = debug.clone();
        if report_ctx.is_some() {
            // Reports should be self-contained and actionable by default.
            suite_debug.trace_evm = true;
            suite_debug.sonatina_symtab = true;
            suite_debug.sonatina_transient_malloc_trace = true;
            suite_debug.debug_dir = report_ctx.as_ref().map(|ctx| ctx.root_dir.join("debug"));
        }
        suite_debug.configure_process_env()?;

        let mut db = DriverDataBase::default();
        let suite_results = if path.is_file() && path.extension() == Some("fe") {
            run_tests_single_file(
                &mut db,
                &path,
                &suite,
                filter,
                show_logs,
                backend,
                &suite_debug,
                report_ctx.as_ref(),
                call_trace,
            )
        } else if path.is_dir() {
            run_tests_ingot(
                &mut db,
                &path,
                &suite,
                filter,
                show_logs,
                backend,
                &suite_debug,
                report_ctx.as_ref(),
                call_trace,
            )
        } else {
            return Err("Path must be either a .fe file or a directory containing fe.toml".into());
        };

        if let Some((out, staging)) = suite_report {
            let should_write = !report_failed_only || suite_results.iter().any(|r| !r.passed);
            if should_write {
                write_report_manifest(&staging.root_dir, backend, filter, &suite_results);
                if let Err(err) = tar_gz_dir(&staging.root_dir, &out) {
                    eprintln!("Error: failed to write report `{out}`: {err}");
                    eprintln!("Report staging directory left at `{}`", staging.temp_dir);
                } else {
                    let _ = std::fs::remove_dir_all(&staging.temp_dir);
                    println!("wrote report: {out}");
                }
            } else {
                let _ = std::fs::remove_dir_all(&staging.temp_dir);
            }
        }

        if let Some((_, staging)) = &report_root
            && report_ctx.is_some()
        {
            let root = &staging.root_dir;
            let status_dir = if suite_results.iter().any(|r| !r.passed) {
                "failed"
            } else {
                "passed"
            };
            let from = root.join("tmp").join(&suite);
            let to = root.join(status_dir).join(&suite);
            if from.exists() {
                let _ = std::fs::remove_dir_all(&to);
                let _ = std::fs::rename(&from, &to);
            }
        }

        if suite_results.is_empty() {
            eprintln!("No tests found in {path}");
        } else {
            test_results.extend(suite_results);
        }
    }

    if let Some((out, staging)) = report_root {
        // Clean up tmp dir, if any suites were moved.
        let _ = std::fs::remove_dir_all(staging.root_dir.join("tmp"));

        write_run_gas_comparison_summary(&staging.root_dir);
        write_report_manifest(&staging.root_dir, backend, filter, &test_results);
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
    debug: &TestDebugOptions,
    report: Option<&ReportContext>,
    call_trace: bool,
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
        eprintln!("Compilation errors in {file_url}");
        eprintln!();
        diags.emit(db);
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
        db, top_mod, suite, filter, show_logs, backend, debug, report, call_trace,
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
    debug: &TestDebugOptions,
    report: Option<&ReportContext>,
    call_trace: bool,
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
        eprintln!("{msg}");
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
        diags.emit(db);
        if let Some(report) = report {
            write_report_error(report, "compilation_errors.txt", &formatted);
        }
        return suite_error_result(suite, "compile", "Compilation errors".to_string());
    }

    let root_mod = ingot.root_mod(db);
    maybe_write_suite_ir(db, root_mod, backend, report);
    discover_and_run_tests(
        db, root_mod, suite, filter, show_logs, backend, debug, report, call_trace,
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
) -> Result<TestModuleOutput, Vec<TestResult>> {
    let _hook = report.map(|r| install_report_panic_hook(r, "codegen_panic_full.txt"));
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(emit_fn)) {
        Ok(Ok(output)) => Ok(output),
        Ok(Err(err)) => {
            let msg = format!("Failed to emit test {backend_label}: {err}");
            eprintln!("{msg}");
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
            eprintln!("{msg}");
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
    debug: &TestDebugOptions,
    report: Option<&ReportContext>,
    call_trace: bool,
) -> Vec<TestResult> {
    let backend = backend.to_lowercase();
    let emit_result = match backend.as_str() {
        "yul" => emit_with_catch_unwind(|| emit_test_module_yul(db, top_mod), "Yul", suite, report),
        "sonatina" => emit_with_catch_unwind(
            || emit_test_module_sonatina(db, top_mod),
            "Sonatina",
            suite,
            report,
        ),
        other => {
            return suite_error_result(
                suite,
                "setup",
                format!("unknown backend `{other}` (expected 'yul' or 'sonatina')"),
            );
        }
    };
    let output = match emit_result {
        Ok(output) => output,
        Err(results) => return results,
    };

    if output.tests.is_empty() {
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
            &output.tests,
        )
    });

    let mut results = Vec::new();
    let mut primary_measurements = FxHashMap::default();

    for case in &output.tests {
        if !test_case_matches_filter(case, filter) {
            continue;
        }

        // Print test name
        print!("test {} ... ", case.display_name);

        debug.configure_per_test_env(Some(suite), &case.display_name);

        // Compile and run the test
        let outcome = compile_and_run_test(
            case,
            show_logs,
            backend.as_str(),
            PRIMARY_YUL_OPTIMIZE,
            report,
            call_trace,
        );

        if outcome.result.passed {
            println!("{}", "ok".green());
        } else {
            println!("{}", "FAILED".red());
            if let Some(ref msg) = outcome.result.error_message {
                eprintln!("    {}", msg);
            }
        }

        if let Some(trace) = &outcome.trace {
            println!("--- call trace ---");
            print!("{trace}");
            println!("--- end trace ---");
        }

        if show_logs {
            if !outcome.logs.is_empty() {
                for log in &outcome.logs {
                    println!("    log {}", log);
                }
            } else if outcome.result.passed {
                println!("    log (none)");
            } else {
                println!("    log (unavailable for failed tests)");
            }
        }

        primary_measurements.insert(
            case.symbol_name.clone(),
            GasMeasurement::from_test_result(&outcome.result),
        );
        results.push(outcome.result);
    }

    if let (Some(report), Some(cases)) = (report, gas_comparison_cases.as_ref()) {
        write_gas_comparison_report(report, backend.as_str(), cases, &primary_measurements);
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
        match emit_with_catch_unwind(
            || emit_test_module_sonatina(db, top_mod),
            "Sonatina",
            suite,
            None,
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
        match emit_with_catch_unwind(|| emit_test_module_yul(db, top_mod), "Yul", suite, None) {
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

fn measure_case_gas(case: &TestMetadata, backend: &str, yul_optimize: bool) -> GasMeasurement {
    let outcome = compile_and_run_test(case, false, backend, yul_optimize, None, false);
    GasMeasurement::from_test_result(&outcome.result)
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

fn gas_comparison_settings_text() -> String {
    let mut out = String::new();
    out.push_str(&format!("yul.primary.optimize={PRIMARY_YUL_OPTIMIZE}\n"));
    out.push_str("yul.compare.unoptimized.optimize=false\n");
    out.push_str("yul.compare.optimized.optimize=true\n");
    out.push_str(&format!("yul.solc.verify_runtime={YUL_VERIFY_RUNTIME}\n"));
    out.push_str("sonatina.codegen.path=emit_test_module_sonatina (default)\n");
    out.push_str("sonatina.optimization.pipeline=default_pipeline (enabled)\n");
    out.push_str("measurement.call=RuntimeInstance::call_raw(empty calldata)\n");
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

fn write_gas_comparison_report(
    report: &ReportContext,
    primary_backend: &str,
    cases: &[GasComparisonCase],
    primary_measurements: &FxHashMap<String, GasMeasurement>,
) {
    let artifacts_dir = report.root_dir.join("artifacts");
    let _ = create_dir_all_utf8(&artifacts_dir);
    let _ = std::fs::write(
        artifacts_dir.join("gas_comparison_settings.txt"),
        gas_comparison_settings_text(),
    );

    let mut markdown = String::new();
    markdown.push_str("# Gas Comparison\n\n");
    markdown.push_str("Comparison of runtime test-call gas usage (`gas_used`) between Yul and Sonatina backends.\n");
    markdown.push_str(
        "`delta` columns are `sonatina - yul`; negative means Sonatina used less gas.\n\n",
    );
    markdown.push_str("| test | yul_unopt | yul_opt | sonatina | delta_vs_unopt | pct_vs_unopt | delta_vs_opt | pct_vs_opt | note |\n");
    markdown.push_str("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |\n");

    let mut csv = String::new();
    csv.push_str(
        "test,symbol,yul_unopt_gas,yul_opt_gas,sonatina_gas,delta_vs_yul_unopt,delta_vs_yul_unopt_pct,delta_vs_yul_opt,delta_vs_yul_opt_pct,yul_unopt_status,yul_opt_status,sonatina_status,note\n",
    );

    let mut totals = GasTotals {
        tests_in_scope: cases.len(),
        ..GasTotals::default()
    };

    for case in cases {
        let yul_opt = if primary_backend.eq_ignore_ascii_case("yul") {
            primary_measurements.get(&case.symbol_name).cloned()
        } else {
            case.yul
                .as_ref()
                .map(|test| measure_case_gas(test, "yul", true))
        };

        let yul_unopt = case
            .yul
            .as_ref()
            .map(|test| measure_case_gas(test, "yul", false));

        let sonatina = if primary_backend.eq_ignore_ascii_case("sonatina") {
            primary_measurements.get(&case.symbol_name).cloned()
        } else {
            case.sonatina
                .as_ref()
                .map(|test| measure_case_gas(test, "sonatina", PRIMARY_YUL_OPTIMIZE))
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
        let yul_unopt_cell = yul_unopt_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let yul_opt_cell = yul_opt_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());
        let sonatina_cell = sonatina_gas.map_or_else(|| "n/a".to_string(), |gas| gas.to_string());

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

        let yul_unopt_status = measurement_status(case.yul.is_some(), yul_unopt.as_ref());
        let yul_opt_status = measurement_status(case.yul.is_some(), yul_opt.as_ref());
        let sonatina_status = measurement_status(case.sonatina.is_some(), sonatina.as_ref());

        let (delta_unopt_cell, delta_unopt_pct_cell) =
            record_delta(yul_unopt_gas, sonatina_gas, &mut totals.vs_yul_unopt);
        let (delta_opt_cell, delta_opt_pct_cell) =
            record_delta(yul_opt_gas, sonatina_gas, &mut totals.vs_yul_opt);

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

    markdown.push_str("\n## Optimization Settings\n\n");
    for line in gas_comparison_settings_text().lines() {
        markdown.push_str(&format!("- {line}\n"));
    }

    let _ = std::fs::write(artifacts_dir.join("gas_comparison.md"), markdown);
    let _ = std::fs::write(artifacts_dir.join("gas_comparison.csv"), csv);
    write_gas_totals_csv(&artifacts_dir.join("gas_comparison_totals.csv"), totals);
}

fn write_run_gas_comparison_summary(root_dir: &Utf8PathBuf) {
    let artifacts_dir = root_dir.join("artifacts");
    let _ = create_dir_all_utf8(&artifacts_dir);
    let _ = std::fs::write(
        artifacts_dir.join("gas_comparison_settings.txt"),
        gas_comparison_settings_text(),
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
    let mut wrote_any_rows = false;
    let mut totals = GasTotals::default();

    for (suite, suite_dir) in suite_dirs {
        let suite_rows_path = suite_dir.join("artifacts").join("gas_comparison.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_rows_path) {
            for (idx, line) in contents.lines().enumerate() {
                if idx == 0 || line.trim().is_empty() {
                    continue;
                }
                all_rows.push_str(&csv_escape(&suite));
                all_rows.push(',');
                all_rows.push_str(line);
                all_rows.push('\n');
                wrote_any_rows = true;
            }
        }

        let suite_totals_path = suite_dir
            .join("artifacts")
            .join("gas_comparison_totals.csv");
        if let Ok(contents) = std::fs::read_to_string(&suite_totals_path) {
            totals.add(parse_gas_totals_csv(&contents));
        }
    }

    if wrote_any_rows {
        let _ = std::fs::write(artifacts_dir.join("gas_comparison_all.csv"), all_rows);
    }
    write_gas_totals_csv(&artifacts_dir.join("gas_comparison_totals.csv"), totals);

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
    summary.push_str("\n## Optimization Settings\n\n");
    for line in gas_comparison_settings_text().lines() {
        summary.push_str(&format!("- {line}\n"));
    }
    if wrote_any_rows {
        summary.push_str(
            "\nSee `artifacts/gas_comparison_all.csv` for per-test rows and `artifacts/gas_comparison_totals.csv` for machine-readable totals.\n",
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
    report: Option<&ReportContext>,
    call_trace: bool,
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
            },
            logs: Vec::new(),
            trace: None,
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
            },
            logs: Vec::new(),
            trace: None,
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
                },
                logs: Vec::new(),
                trace: None,
            };
        }

        if let Some(report) = report {
            write_sonatina_case_artifacts(report, case);
        }

        let bytecode_hex = hex::encode(&case.bytecode);
        let (result, logs, trace) = execute_test(
            &case.display_name,
            &bytecode_hex,
            show_logs,
            case.expected_revert.as_ref(),
            call_trace,
        );
        return TestOutcome {
            result,
            logs,
            trace,
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
            },
            logs: Vec::new(),
            trace: None,
        };
    }

    if let Some(report) = report {
        write_yul_case_artifacts(report, case);
    }

    let bytecode = match compile_single_contract(
        &case.object_name,
        &case.yul,
        yul_optimize,
        YUL_VERIFY_RUNTIME,
    ) {
        Ok(contract) => contract.bytecode,
        Err(err) => {
            return TestOutcome {
                result: TestResult {
                    name: case.display_name.clone(),
                    passed: false,
                    error_message: Some(format!("Failed to compile test: {}", err.0)),
                    gas_used: None,
                },
                logs: Vec::new(),
                trace: None,
            };
        }
    };

    // Execute the test bytecode in revm
    let (result, logs, trace) = execute_test(
        &case.display_name,
        &bytecode,
        show_logs,
        case.expected_revert.as_ref(),
        call_trace,
    );
    TestOutcome {
        result,
        logs,
        trace,
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
    filter: Option<&str>,
    results: &[TestResult],
) {
    let mut out = String::new();
    out.push_str("fe test report\n");
    out.push_str(&format!("backend: {backend}\n"));
    out.push_str(&format!("filter: {}\n", filter.unwrap_or("<none>")));
    out.push_str(&format!("fe_version: {}\n", env!("CARGO_PKG_VERSION")));
    out.push_str("details: see `meta/args.txt` and `meta/git.txt` for exact repro context\n");
    out.push_str("gas_comparison: see `artifacts/gas_comparison.md`, `artifacts/gas_comparison.csv`, `artifacts/gas_comparison_totals.csv`, and `artifacts/gas_comparison_settings.txt` when available\n");
    out.push_str("gas_comparison_aggregate: run-level reports also include `artifacts/gas_comparison_all.csv` and `artifacts/gas_comparison_summary.md`\n");
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
    call_trace: bool,
) -> (TestResult, Vec<String>, Option<contract_harness::CallTrace>) {
    // Deploy the test contract
    let mut instance = match RuntimeInstance::deploy(bytecode_hex) {
        Ok(instance) => instance,
        Err(err) => {
            return (
                TestResult {
                    name: name.to_string(),
                    passed: false,
                    error_message: Some(format!("Failed to deploy test: {err}")),
                    gas_used: None,
                },
                Vec::new(),
                None,
            );
        }
    };

    // Execute the test (empty calldata since test functions take no args)
    let options = ExecutionOptions::default();

    // Capture call trace BEFORE the real execution so the cloned context
    // has the right pre-call state (contract deployed but not yet called).
    let trace = if call_trace {
        Some(instance.call_raw_traced(&[], options))
    } else {
        None
    };

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
        (Ok((gas_used, logs)), None) => (
            TestResult {
                name: name.to_string(),
                passed: true,
                error_message: None,
                gas_used: Some(gas_used),
            },
            logs,
            trace,
        ),
        // Normal test: execution reverted (failure)
        (Err(err), None) => {
            let gas_used = harness_error_gas_used(&err);
            (
                TestResult {
                    name: name.to_string(),
                    passed: false,
                    error_message: Some(format_harness_error(err)),
                    gas_used,
                },
                Vec::new(),
                trace,
            )
        }
        // Expected revert: execution succeeded (failure - should have reverted)
        (Ok((gas_used, _)), Some(_)) => (
            TestResult {
                name: name.to_string(),
                passed: false,
                error_message: Some("Expected test to revert, but it succeeded".to_string()),
                gas_used: Some(gas_used),
            },
            Vec::new(),
            trace,
        ),
        // Expected revert: execution reverted (success)
        (Err(contract_harness::HarnessError::Revert(_)), Some(ExpectedRevert::Any)) => (
            TestResult {
                name: name.to_string(),
                passed: true,
                error_message: None,
                gas_used: None,
            },
            Vec::new(),
            trace,
        ),
        // Expected revert: execution failed for a different reason (failure)
        (Err(err), Some(ExpectedRevert::Any)) => {
            let gas_used = harness_error_gas_used(&err);
            (
                TestResult {
                    name: name.to_string(),
                    passed: false,
                    error_message: Some(format!(
                        "Expected test to revert, but it failed with: {}",
                        format_harness_error(err)
                    )),
                    gas_used,
                },
                Vec::new(),
                trace,
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
