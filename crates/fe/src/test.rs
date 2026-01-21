//! Test runner for Fe tests.
//!
//! Discovers functions marked with `#[test]` attribute, compiles them, and
//! executes them using revm.

use camino::Utf8PathBuf;
use codegen::{ExpectedRevert, TestMetadata, emit_test_module_yul};
use colored::Colorize;
use common::InputDb;
use contract_harness::{ExecutionOptions, RuntimeInstance};
use driver::DriverDataBase;
use hir::hir_def::{HirIngot, TopLevelMod};
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

/// Run tests in the given path.
///
/// # Arguments
/// * `path` - Path to a .fe file or directory containing an ingot
/// * `filter` - Optional filter pattern for test names
/// * `show_logs` - Whether to show event logs from test execution
///
/// Returns nothing; exits the process on invalid input or test failures.
pub fn run_tests(path: &Utf8PathBuf, filter: Option<&str>, show_logs: bool) {
    let mut db = DriverDataBase::default();

    // Determine if we're dealing with a single file or an ingot directory
    let test_results = if path.is_file() && path.extension() == Some("fe") {
        run_tests_single_file(&mut db, path, filter, show_logs)
    } else if path.is_dir() {
        run_tests_ingot(&mut db, path, filter, show_logs)
    } else {
        eprintln!("Error: Path must be either a .fe file or a directory containing fe.toml");
        std::process::exit(1);
    };

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
    filter: Option<&str>,
    show_logs: bool,
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
    discover_and_run_tests(db, top_mod, filter, show_logs)
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
    filter: Option<&str>,
    show_logs: bool,
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
    discover_and_run_tests(db, root_mod, filter, show_logs)
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
    filter: Option<&str>,
    show_logs: bool,
) -> Vec<TestResult> {
    let output = match emit_test_module_yul(db, top_mod) {
        Ok(output) => output,
        Err(err) => {
            eprintln!("Failed to emit test Yul: {err}");
            std::process::exit(1);
        }
    };

    if output.tests.is_empty() {
        eprintln!("No tests found");
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

        // Compile and run the test
        let outcome = compile_and_run_test(case, show_logs);

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

/// Compiles a test function to bytecode and executes it in revm.
///
/// * `case` - Test metadata describing the Yul object and parameters.
/// * `show_logs` - Whether to capture EVM logs for the test run.
///
/// Returns the test outcome (result + logs).
fn compile_and_run_test(case: &TestMetadata, show_logs: bool) -> TestOutcome {
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

    // Compile to bytecode using solc
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
    let (result, logs) = execute_test(
        &case.display_name,
        &bytecode,
        show_logs,
        case.expected_revert.as_ref(),
    );
    TestOutcome { result, logs }
}

/// Deploys and executes compiled test bytecode in revm.
///
/// The test passes if the function returns normally, fails if it reverts.
/// When `expected_revert` is set, the logic is inverted: the test passes if it reverts.
///
/// * `name` - Display name used for reporting.
/// * `bytecode_hex` - Hex-encoded init bytecode for the test object.
/// * `show_logs` - Whether to execute with log collection enabled.
/// * `expected_revert` - If set, the test is expected to revert.
///
/// Returns the test result and any emitted logs.
fn execute_test(
    name: &str,
    bytecode_hex: &str,
    show_logs: bool,
    expected_revert: Option<&ExpectedRevert>,
) -> (TestResult, Vec<String>) {
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

    match (call_result, expected_revert) {
        // Normal test: execution succeeded
        (Ok(logs), None) => (
            TestResult {
                name: name.to_string(),
                passed: true,
                error_message: None,
            },
            logs,
        ),
        // Normal test: execution reverted (failure)
        (Err(err), None) => (
            TestResult {
                name: name.to_string(),
                passed: false,
                error_message: Some(format_harness_error(err)),
            },
            Vec::new(),
        ),
        // Expected revert: execution succeeded (failure - should have reverted)
        (Ok(_), Some(_)) => (
            TestResult {
                name: name.to_string(),
                passed: false,
                error_message: Some("Expected test to revert, but it succeeded".to_string()),
            },
            Vec::new(),
        ),
        // Expected revert: execution reverted (success)
        (Err(contract_harness::HarnessError::Revert(_)), Some(ExpectedRevert::Any)) => (
            TestResult {
                name: name.to_string(),
                passed: true,
                error_message: None,
            },
            Vec::new(),
        ),
        // Expected revert: execution failed for a different reason (failure)
        (Err(err), Some(ExpectedRevert::Any)) => (
            TestResult {
                name: name.to_string(),
                passed: false,
                error_message: Some(format!(
                    "Expected test to revert, but it failed with: {}",
                    format_harness_error(err)
                )),
            },
            Vec::new(),
        ),
        // Future: match specific revert data
        // (Err(HarnessError::Revert(data)), Some(ExpectedRevert::ExactData(expected))) => { ... }
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
