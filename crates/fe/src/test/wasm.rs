//! `fe test --backend wasm`: each test is built into a WebAssembly module and
//! run in its own Node.js process, with the host of `examples/wasm/host.mjs`.
//!
//! A module holds one test and what it reaches. A test that uses something the
//! WebAssembly backend does not support (a 256-bit integer, an EVM built-in,
//! `should_revert`) fails with an error that names it; no test is skipped
//! silently. A trap, a failed assertion or a call of `abort` fails the test with
//! the engine's message. Node.js is required: when `node` is missing or too old
//! the run ends with an error that says so.

use std::{
    fmt::Write as _,
    fs,
    io::Read,
    panic::{AssertUnwindSafe, catch_unwind},
    process::{Command, Stdio},
    sync::Arc,
    thread,
    time::{Duration, Instant},
};

use camino::Utf8PathBuf;
use codegen::{OptLevel, emit_test_wasm};
use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use mir::collect_test_funcs;
use tempfile::{Builder, TempDir};

use super::{
    CompiledTest, SingleTestJob, SuitePreparation, TestEmitSelection, TestOutcome, TestResult,
    default_test_emit_dir, suite_error_result, test_emit_stem, write_case_status_line,
    write_test_emit_artifact,
};
use crate::report::panic_payload_to_string;

/// The host, the same file that `examples/wasm/README.md` describes.
const HOST_MJS: &str = include_str!("../../../../examples/wasm/host.mjs");
const RUNNER_MJS: &str = include_str!("wasm_runner.mjs");

/// The oldest Node.js that the host supports (`crypto.subtle` is global).
const MIN_NODE_MAJOR: u32 = 20;
const TEST_TIMEOUT: Duration = Duration::from_secs(60);
const OUTPUT_LIMIT: usize = 1024 * 1024;
const DIAGNOSTIC_LIMIT: usize = 4096;

/// Ends the run with an error unless `node` can be started and is recent
/// enough. Tests are never skipped for lack of Node.js.
pub(super) fn require_node() -> Result<(), String> {
    let requirement = format!(
        "`fe test --backend wasm` runs each test in Node.js (version {MIN_NODE_MAJOR} or later)"
    );
    let output = Command::new("node")
        .arg("--version")
        .stdin(Stdio::null())
        .output()
        .map_err(|err| {
            format!("{requirement}, but `node` could not be started ({err}); install Node.js and put `node` on PATH")
        })?;
    let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let major = version
        .strip_prefix('v')
        .and_then(|rest| rest.split('.').next())
        .and_then(|major| major.parse::<u32>().ok());
    match major {
        Some(major) if output.status.success() && major >= MIN_NODE_MAJOR => Ok(()),
        _ => Err(format!(
            "{requirement}, but `node --version` printed `{version}`; install a newer Node.js"
        )),
    }
}

#[derive(Debug)]
struct SuiteDirectory {
    directory: TempDir,
}

/// One built test.
#[derive(Debug, Clone)]
pub(super) struct WasmTestCase {
    name: String,
    module_file: String,
    entry: String,
    suite: Arc<SuiteDirectory>,
    timeout: Duration,
}

impl WasmTestCase {
    pub(super) fn run(&self) -> TestOutcome {
        let started = Instant::now();
        let error_message = match self.execute() {
            Ok(None) => None,
            Ok(Some(message)) => Some(message),
            Err(err) => Some(format!("failed to run the test in Node.js: {err}")),
        };
        TestOutcome {
            result: TestResult {
                name: self.name.clone(),
                passed: error_message.is_none(),
                error_message,
            },
            logs: Vec::new(),
            trace: None,
            elapsed: started.elapsed(),
        }
    }

    /// `None` if the test passed, otherwise what to report.
    fn execute(&self) -> std::io::Result<Option<String>> {
        let mut child = Command::new("node")
            .current_dir(self.suite.directory.path())
            .arg("run_test.mjs")
            .arg(&self.module_file)
            .arg(&self.entry)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        let capture = |pipe: Option<Box<dyn Read + Send>>| {
            thread::spawn(move || {
                let mut bytes = Vec::new();
                if let Some(pipe) = pipe {
                    let _ = pipe.take(OUTPUT_LIMIT as u64).read_to_end(&mut bytes);
                }
                bytes
            })
        };
        let stdout = capture(
            child
                .stdout
                .take()
                .map(|p| Box::new(p) as Box<dyn Read + Send>),
        );
        let stderr = capture(
            child
                .stderr
                .take()
                .map(|p| Box::new(p) as Box<dyn Read + Send>),
        );
        let deadline = Instant::now() + self.timeout;
        let (status, timed_out) = loop {
            if let Some(status) = child.try_wait()? {
                break (status, false);
            }
            if Instant::now() >= deadline {
                let _ = child.kill();
                break (child.wait()?, true);
            }
            thread::sleep(Duration::from_millis(5));
        };
        let stdout = stdout.join().unwrap_or_default();
        let stderr = stderr.join().unwrap_or_default();
        if status.success() && !timed_out {
            return Ok(None);
        }
        let preview = |bytes: &[u8]| {
            let text = String::from_utf8_lossy(&bytes[..bytes.len().min(DIAGNOSTIC_LIMIT)]);
            text.trim_end().to_string()
        };
        let mut message = if timed_out {
            format!(
                "wasm test exceeded the time limit of {} seconds",
                self.timeout.as_secs()
            )
        } else if stderr.is_empty() {
            format!("wasm test failed ({status})")
        } else {
            format!("wasm test failed: {}", preview(&stderr))
        };
        if !stdout.is_empty() {
            let _ = write!(message, "\n    logged:\n{}", preview(&stdout));
        }
        Ok(Some(message))
    }
}

/// The part of a lowering error that says what is wrong. The compiler appends
/// the runtime IR of the function after a blank line.
fn reason(message: &str) -> String {
    message
        .split("\n\nrMIR context:")
        .next()
        .unwrap_or(message)
        .trim()
        .to_string()
}

/// Builds every selected test of the modules into its own module, and returns
/// the jobs that run them. A test that cannot be built is a failed result with
/// the reason, listed in the output like a failed run.
#[allow(clippy::too_many_arguments)]
pub(super) fn prepare_tests(
    db: &DriverDataBase,
    top_mods: Vec<TopLevelMod<'_>>,
    source_path: &Utf8PathBuf,
    suite: &str,
    suite_key: &str,
    filter: Option<&str>,
    opt_level: OptLevel,
    emit: TestEmitSelection,
    output: &mut String,
) -> SuitePreparation {
    let setup_error = |message: String| SuitePreparation {
        results: suite_error_result(suite, "setup", message),
        single_jobs: Vec::new(),
    };
    let tests = collect_test_funcs(db, &top_mods, filter);
    if tests.is_empty() {
        return SuitePreparation {
            results: Vec::new(),
            single_jobs: Vec::new(),
        };
    }
    let directory = match Builder::new().prefix("fe-wasm-test-").tempdir() {
        Ok(directory) => directory,
        Err(err) => return setup_error(format!("failed to create the test directory: {err}")),
    };
    for (file, contents) in [("host.mjs", HOST_MJS), ("run_test.mjs", RUNNER_MJS)] {
        if let Err(err) = fs::write(directory.path().join(file), contents) {
            return setup_error(format!("failed to write {file}: {err}"));
        }
    }
    let out_dir = match (!emit.is_empty()).then(|| default_test_emit_dir(source_path)) {
        Some(Ok(dir)) => Some(dir),
        Some(Err(err)) => return setup_error(err),
        None => None,
    };
    let suite_dir = Arc::new(SuiteDirectory { directory });

    let mut results = Vec::new();
    let mut single_jobs = Vec::new();
    for (index, test) in tests.iter().enumerate() {
        let started = Instant::now();
        let built = catch_unwind(AssertUnwindSafe(|| {
            emit_test_wasm(db, test, opt_level, emit.ir).map_err(|err| reason(&err.to_string()))
        }))
        .unwrap_or_else(|payload| {
            Err(format!(
                "the WebAssembly backend panicked: {}",
                panic_payload_to_string(payload.as_ref())
            ))
        });
        let module = match built {
            Ok(module) => module,
            Err(err) => {
                let message = format!("cannot build `{}` for WebAssembly: {err}", test.name);
                write_case_status_line(output, false, started.elapsed(), &test.name);
                let _ = writeln!(output, "    {message}");
                results.push(TestResult {
                    name: test.name.clone(),
                    passed: false,
                    error_message: Some(message),
                });
                continue;
            }
        };
        let module_file = format!("{index}.wasm");
        if let Err(err) = fs::write(suite_dir.directory.path().join(&module_file), &module.wasm) {
            return setup_error(format!("failed to write {module_file}: {err}"));
        }
        if let (Some(out_dir), Some(ir)) = (&out_dir, &module.ir) {
            let stem = format!("{}.wasm.{}", test_emit_stem(suite_key), test.name);
            if let Err(err) = write_test_emit_artifact(out_dir, &stem, "sona", ir, output) {
                return setup_error(err);
            }
        }
        single_jobs.push(SingleTestJob {
            suite_key: suite_key.to_string(),
            case: CompiledTest::Wasm(WasmTestCase {
                name: test.name.clone(),
                module_file,
                entry: module.entry,
                suite: Arc::clone(&suite_dir),
                timeout: TEST_TIMEOUT,
            }),
            evm_trace: None,
            report_root: None,
        });
    }
    SuitePreparation {
        results,
        single_jobs,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A module that exports `memory` and a function `run` that loops for ever.
    fn module_that_never_ends() -> Vec<u8> {
        let mut module = b"\0asm\x01\0\0\0".to_vec();
        module.extend([0x01, 0x04, 0x01, 0x60, 0x00, 0x00]); // type: () -> ()
        module.extend([0x03, 0x02, 0x01, 0x00]); // one function of that type
        module.extend([0x05, 0x03, 0x01, 0x00, 0x01]); // memory: one page
        module.extend([0x07, 0x10, 0x02]);
        module.extend([0x06, b'm', b'e', b'm', b'o', b'r', b'y', 0x02, 0x00]);
        module.extend([0x03, b'r', b'u', b'n', 0x00, 0x00]);
        // loop; br 0; end; end
        module.extend([
            0x0a, 0x09, 0x01, 0x07, 0x00, 0x03, 0x40, 0x0c, 0x00, 0x0b, 0x0b,
        ]);
        module
    }

    fn case_for(module: &[u8], entry: &str, timeout: Duration) -> WasmTestCase {
        let directory = Builder::new()
            .prefix("fe-wasm-test-unit-")
            .tempdir()
            .unwrap();
        fs::write(directory.path().join("host.mjs"), HOST_MJS).unwrap();
        fs::write(directory.path().join("run_test.mjs"), RUNNER_MJS).unwrap();
        fs::write(directory.path().join("m.wasm"), module).unwrap();
        WasmTestCase {
            name: "unit".to_string(),
            module_file: "m.wasm".to_string(),
            entry: entry.to_string(),
            suite: Arc::new(SuiteDirectory { directory }),
            timeout,
        }
    }

    #[test]
    fn the_runtime_ir_is_not_part_of_the_reason() {
        assert_eq!(
            reason("unsupported: while lowering `f`\n\nrMIR context:\nfn x {}"),
            "unsupported: while lowering `f`"
        );
        assert_eq!(reason("a short message"), "a short message");
    }

    #[test]
    fn a_test_that_never_ends_fails_when_its_time_is_up() {
        require_node().expect("these tests need Node.js");
        let started = Instant::now();
        let outcome = case_for(&module_that_never_ends(), "run", Duration::from_secs(1)).run();
        assert!(!outcome.result.passed);
        assert_eq!(
            outcome.result.error_message.as_deref(),
            Some("wasm test exceeded the time limit of 1 seconds")
        );
        assert!(started.elapsed() < Duration::from_secs(30));
    }

    #[test]
    fn an_export_that_does_not_exist_fails_the_test() {
        require_node().expect("these tests need Node.js");
        let outcome = case_for(&module_that_never_ends(), "absent", Duration::from_secs(30)).run();
        assert!(!outcome.result.passed);
        let message = outcome.result.error_message.unwrap();
        assert_eq!(
            message,
            "wasm test failed: host contract violation: the module does not export a function `absent`"
        );
    }
}
