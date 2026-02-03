//! Test runner for Fe tests.
//!
//! Discovers functions marked with `#[test]` attribute, compiles them, and
//! executes them using revm.

use camino::Utf8PathBuf;
use codegen::{TestMetadata, emit_test_module_sonatina, emit_test_module_yul};
use colored::Colorize;
use common::InputDb;
use contract_harness::{ExecutionOptions, RuntimeInstance};
use driver::DriverDataBase;
use hir::hir_def::{HirIngot, TopLevelMod};
use rustc_hash::FxHashSet;
use solc_runner::compile_single_contract;
use url::Url;

/// Result of running a single test.
#[derive(Debug)]
pub struct TestResult {
    pub name: String,
    pub passed: bool,
    pub error_message: Option<String>,
}

#[derive(Debug)]
struct TestOutcome {
    result: TestResult,
    logs: Vec<String>,
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

    fn configure_process_env(&self) {
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
            return;
        };

        if let Err(err) = std::fs::create_dir_all(dir) {
            eprintln!("Error: failed to create debug dir `{dir}`: {err}");
            std::process::exit(1);
        }

        if self.trace_evm {
            // When writing traces to files, suppress stderr spam by default.
            Self::set_env("FE_TRACE_EVM_STDERR", "0");
        }

        if self.sonatina_symtab {
            let path = dir.join("sonatina_symtab.txt");
            truncate_file(&path);
            Self::set_env("FE_SONATINA_DUMP_SYMTAB_OUT", path.as_str());
        }

        if self.sonatina_stackify_trace {
            let path = dir.join("sonatina_stackify_trace.txt");
            truncate_file(&path);
            Self::set_env("SONATINA_STACKIFY_TRACE_OUT", path.as_str());
        }

        if self.sonatina_transient_malloc_trace {
            let path = dir.join("sonatina_transient_malloc_trace.txt");
            truncate_file(&path);
            Self::set_env("SONATINA_TRANSIENT_MALLOC_TRACE_OUT", path.as_str());
        }
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
        truncate_file(&path);
        Self::set_env("FE_TRACE_EVM_OUT", path.as_str());
    }
}

fn truncate_file(path: &Utf8PathBuf) {
    if let Err(err) = std::fs::write(path, "") {
        eprintln!("Error: failed to truncate `{path}`: {err}");
        std::process::exit(1);
    }
}

fn sanitize_filename(component: &str) -> String {
    component
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}

/// Run tests in the given path.
///
/// # Arguments
/// * `paths` - Paths to .fe files or directories containing ingots (supports globs)
/// * `filter` - Optional filter pattern for test names
/// * `show_logs` - Whether to show event logs from test execution
/// * `backend` - Codegen backend for test artifacts ("yul" or "sonatina")
///
/// Returns nothing; exits the process on invalid input or test failures.
pub fn run_tests(
    paths: &[Utf8PathBuf],
    filter: Option<&str>,
    show_logs: bool,
    backend: &str,
    debug: &TestDebugOptions,
) {
    debug.configure_process_env();
    let input_paths = expand_test_paths(paths);

    let mut test_results = Vec::new();
    let multi = input_paths.len() > 1;
    if multi {
        println!("running `fe test` for {} inputs\n", input_paths.len());
    }

    for path in input_paths {
        let suite = suite_name_for_path(&path);

        if multi {
            println!("==> {path}");
        }

        let mut db = DriverDataBase::default();
        let suite_results = if path.is_file() && path.extension() == Some("fe") {
            run_tests_single_file(&mut db, &path, &suite, filter, show_logs, backend, debug)
        } else if path.is_dir() {
            run_tests_ingot(&mut db, &path, &suite, filter, show_logs, backend, debug)
        } else {
            eprintln!("Error: Path must be either a .fe file or a directory containing fe.toml");
            std::process::exit(1);
        };

        if suite_results.is_empty() {
            eprintln!("No tests found in {path}");
        } else {
            test_results.extend(suite_results);
        }
    }

    // Print summary
    print_summary(&test_results);

    // Exit with code 1 if any tests failed
    if test_results.iter().any(|r| !r.passed) {
        std::process::exit(1);
    }
}

/// Runs tests defined in a single `.fe` source file.
///
/// * `db` - Driver database used for compilation.
/// * `file_path` - Path to the `.fe` file.
/// * `filter` - Optional substring filter for test names.
/// * `show_logs` - Whether to show event logs from test execution.
///
/// Returns the collected test results.
fn run_tests_single_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    suite: &str,
    filter: Option<&str>,
    show_logs: bool,
    backend: &str,
    debug: &TestDebugOptions,
) -> Vec<TestResult> {
    // Create a file URL for the single .fe file
    let file_url = match Url::from_file_path(file_path.canonicalize_utf8().unwrap()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid file path: {file_path}");
            std::process::exit(1);
        }
    };

    // Read the file content
    let content = match std::fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {file_path}: {err}");
            std::process::exit(1);
        }
    };

    // Add the file to the workspace
    db.workspace().touch(db, file_url.clone(), Some(content));

    // Get the top-level module
    let Some(file) = db.workspace().get(db, &file_url) else {
        eprintln!("Error: Could not process file {file_path}");
        std::process::exit(1);
    };

    let top_mod = db.top_mod(file);

    // Check for compilation errors first
    let diags = db.run_on_top_mod(top_mod);
    if !diags.is_empty() {
        eprintln!("Compilation errors in {file_url}");
        eprintln!();
        diags.emit(db);
        std::process::exit(1);
    }

    // Discover and run tests
    discover_and_run_tests(db, top_mod, suite, filter, show_logs, backend, debug)
}

/// Runs tests in an ingot directory (containing `fe.toml`).
///
/// * `db` - Driver database used for compilation.
/// * `dir_path` - Path to the ingot directory.
/// * `filter` - Optional substring filter for test names.
/// * `show_logs` - Whether to show event logs from test execution.
///
/// Returns the collected test results.
fn run_tests_ingot(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    suite: &str,
    filter: Option<&str>,
    show_logs: bool,
    backend: &str,
    debug: &TestDebugOptions,
) -> Vec<TestResult> {
    let canonical_path = match dir_path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {dir_path}");
            std::process::exit(1);
        }
    };

    let ingot_url = match Url::from_directory_path(canonical_path.as_str()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid directory path: {dir_path}");
            std::process::exit(1);
        }
    };

    let had_init_diagnostics = driver::init_ingot(db, &ingot_url);
    if had_init_diagnostics {
        std::process::exit(1);
    }

    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        eprintln!("Error: Could not resolve ingot from directory");
        std::process::exit(1);
    };

    // Check for compilation errors
    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        diags.emit(db);
        std::process::exit(1);
    }

    let root_mod = ingot.root_mod(db);
    discover_and_run_tests(db, root_mod, suite, filter, show_logs, backend, debug)
}

/// Discovers `#[test]` functions, compiles them, and executes each one.
///
/// * `db` - Driver database used for compilation.
/// * `top_mod` - Root module to scan for tests.
/// * `filter` - Optional substring filter for test names.
/// * `show_logs` - Whether to show event logs from test execution.
///
/// Returns the collected test results.
fn discover_and_run_tests(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    suite: &str,
    filter: Option<&str>,
    show_logs: bool,
    backend: &str,
    debug: &TestDebugOptions,
) -> Vec<TestResult> {
    let backend = backend.to_lowercase();
    let output = match backend.as_str() {
        "yul" => match emit_test_module_yul(db, top_mod) {
            Ok(output) => output,
            Err(err) => {
                eprintln!("Failed to emit test Yul: {err}");
                std::process::exit(1);
            }
        },
        "sonatina" => match emit_test_module_sonatina(db, top_mod) {
            Ok(output) => output,
            Err(err) => {
                eprintln!("Failed to emit test Sonatina bytecode: {err}");
                std::process::exit(1);
            }
        },
        other => {
            eprintln!("Error: unknown backend `{other}` (expected 'yul' or 'sonatina')");
            std::process::exit(1);
        }
    };

    if output.tests.is_empty() {
        return Vec::new();
    }

    let mut results = Vec::new();

    for case in &output.tests {
        // Apply filter if provided
        if let Some(pattern) = filter
            && !case.hir_name.contains(pattern)
            && !case.symbol_name.contains(pattern)
            && !case.display_name.contains(pattern)
        {
            continue;
        }

        // Print test name
        print!("test {} ... ", case.display_name);

        debug.configure_per_test_env(Some(suite), &case.display_name);

        // Compile and run the test
        let outcome = compile_and_run_test(case, show_logs, backend.as_str());

        if outcome.result.passed {
            println!("{}", "ok".green());
        } else {
            println!("{}", "FAILED".red());
            if let Some(ref msg) = outcome.result.error_message {
                eprintln!("    {}", msg);
            }
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

        results.push(outcome.result);
    }

    results
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

fn expand_test_paths(inputs: &[Utf8PathBuf]) -> Vec<Utf8PathBuf> {
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
            eprintln!("Error: path does not exist: {input}");
            std::process::exit(1);
        }

        let mut matches = Vec::new();
        let entries = glob::glob(pattern).unwrap_or_else(|err| {
            eprintln!("Error: invalid glob pattern `{pattern}`: {err}");
            std::process::exit(1);
        });
        for entry in entries {
            let path = match entry {
                Ok(path) => path,
                Err(err) => {
                    eprintln!("Error: glob entry error for `{pattern}`: {err}");
                    std::process::exit(1);
                }
            };
            let utf8 = match Utf8PathBuf::from_path_buf(path) {
                Ok(path) => path,
                Err(path) => {
                    eprintln!("Error: non-utf8 path matched by `{pattern}`: {path:?}");
                    std::process::exit(1);
                }
            };
            matches.push(utf8);
        }

        if matches.is_empty() {
            eprintln!("Error: glob pattern matched no paths: `{pattern}`");
            std::process::exit(1);
        }

        matches.sort();
        for path in matches {
            let key = path.as_str().to_string();
            if seen.insert(key) {
                expanded.push(path);
            }
        }
    }

    expanded
}

fn looks_like_glob(pattern: &str) -> bool {
    pattern.contains('*') || pattern.contains('?') || pattern.contains('[')
}

/// Compiles a test function to bytecode and executes it in revm.
///
/// * `case` - Test metadata describing the Yul object and parameters.
/// * `show_logs` - Whether to capture EVM logs for the test run.
/// * `backend` - Backend selection ("yul" or "sonatina").
///
/// Returns the test outcome (result + logs).
fn compile_and_run_test(case: &TestMetadata, show_logs: bool, backend: &str) -> TestOutcome {
    if case.value_param_count > 0 {
        return TestOutcome {
            result: TestResult {
                name: case.display_name.clone(),
                passed: false,
                error_message: Some(format!(
                    "tests with value parameters are not supported (found {})",
                    case.value_param_count
                )),
            },
            logs: Vec::new(),
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
            },
            logs: Vec::new(),
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
                },
                logs: Vec::new(),
            };
        }

        let bytecode_hex = hex::encode(&case.bytecode);
        let (result, logs) = execute_test(&case.display_name, &bytecode_hex, show_logs);
        return TestOutcome { result, logs };
    }

    // Default backend: compile Yul to bytecode using solc.
    if case.yul.trim().is_empty() {
        return TestOutcome {
            result: TestResult {
                name: case.display_name.clone(),
                passed: false,
                error_message: Some(format!("missing test Yul for `{}`", case.display_name)),
            },
            logs: Vec::new(),
        };
    }

    let bytecode = match compile_single_contract(&case.object_name, &case.yul, false, true) {
        Ok(contract) => contract.bytecode,
        Err(err) => {
            return TestOutcome {
                result: TestResult {
                    name: case.display_name.clone(),
                    passed: false,
                    error_message: Some(format!("Failed to compile test: {}", err.0)),
                },
                logs: Vec::new(),
            };
        }
    };

    // Execute the test bytecode in revm
    let (result, logs) = execute_test(&case.display_name, &bytecode, show_logs);
    TestOutcome { result, logs }
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
fn execute_test(name: &str, bytecode_hex: &str, show_logs: bool) -> (TestResult, Vec<String>) {
    // Deploy the test contract
    let mut instance = match RuntimeInstance::deploy(bytecode_hex) {
        Ok(instance) => instance,
        Err(err) => {
            return (
                TestResult {
                    name: name.to_string(),
                    passed: false,
                    error_message: Some(format!("Failed to deploy test: {err}")),
                },
                Vec::new(),
            );
        }
    };

    // Execute the test (empty calldata since test functions take no args)
    let options = ExecutionOptions::default();
    let call_result = if show_logs {
        instance
            .call_raw_with_logs(&[], options)
            .map(|outcome| outcome.logs)
    } else {
        instance.call_raw(&[], options).map(|_| Vec::new())
    };

    match call_result {
        Ok(logs) => (
            TestResult {
                name: name.to_string(),
                passed: true,
                error_message: None,
            },
            logs,
        ),
        Err(err) => (
            TestResult {
                name: name.to_string(),
                passed: false,
                error_message: Some(format_harness_error(err)),
            },
            Vec::new(),
        ),
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
