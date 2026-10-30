#![cfg(feature = "wasm")]

//! `fe test --backend wasm`.
//!
//! Each test of a file is built into a module and run in Node.js with the host
//! of `examples/wasm/host.mjs`. The expected result of every test is written
//! down here from the source of the fixtures, before any run:
//!
//! - `shared.fe` has six tests that pass, `fails_on_purpose` (a failed
//!   assertion) and three tests that trap. The native backend and the
//!   WebAssembly backend, at the optimization levels 0, 1 and 2, must report
//!   the same result for every test.
//! - `wasm_only.fe` uses the host operations of `std::wasm`.
//! - `unsupported.fe` has tests that the backend cannot build; they fail with
//!   the reason and the other test still runs.
//!
//! Node must be installed: the tests fail, and do not skip, without it.

use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
};

use tempfile::tempdir;

fn fixture(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/wasm_test")
        .join(format!("{name}.fe"))
}

fn fe_test(backend: &str, level: &str, args: &[&str], path: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_fe"))
        .args([
            "test",
            "--backend",
            backend,
            "-O",
            level,
            "--color",
            "never",
        ])
        .args(args)
        .arg(path)
        .output()
        .expect("fe should run")
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// The result of each test, from lines such as `PASS  [0.0268s] name`.
fn results(output: &Output) -> BTreeMap<String, bool> {
    stdout(output)
        .lines()
        .filter_map(|line| {
            let passed = match line.split_whitespace().next()? {
                "PASS" => true,
                "FAIL" => false,
                _ => return None,
            };
            let name = line.split_whitespace().last()?;
            Some((name.to_string(), passed))
        })
        .collect()
}

fn expected(passing: &[&str], failing: &[&str]) -> BTreeMap<String, bool> {
    passing
        .iter()
        .map(|name| (name.to_string(), true))
        .chain(failing.iter().map(|name| (name.to_string(), false)))
        .collect()
}

const SHARED_PASSING: [&str; 6] = [
    "sums_a_loop",
    "passes_structs_by_value",
    "matches_on_enums",
    "sorts_an_array",
    "computes_with_wide_integers",
    "checks_narrow_arithmetic_at_the_edge",
];
const SHARED_FAILING: [&str; 4] = [
    "fails_on_purpose",
    "traps_on_overflow",
    "traps_on_division_by_zero",
    "traps_on_an_index_outside_the_array",
];

/// The text of the failure of one test: the line after its `FAIL` line.
fn failure_of(output: &Output, name: &str) -> String {
    let text = stdout(output);
    let mut lines = text.lines();
    while let Some(line) = lines.next() {
        if line.starts_with("FAIL") && line.ends_with(name) {
            return lines.next().unwrap_or_default().trim().to_string();
        }
    }
    panic!("no FAIL line for {name} in:\n{text}");
}

#[test]
fn the_wasm_backend_reports_what_was_written_down() {
    for level in ["0", "1", "2"] {
        let output = fe_test("wasm", level, &[], &fixture("shared"));
        assert_eq!(
            results(&output),
            expected(&SHARED_PASSING, &SHARED_FAILING),
            "-O{level}:\n{}",
            stdout(&output)
        );
        assert_eq!(output.status.code(), Some(1), "failing tests fail the run");
        assert!(stdout(&output).contains("test result: FAILED. 6 passed; 4 failed"));
    }
}

#[cfg(all(feature = "cranelift", target_arch = "x86_64", target_os = "linux"))]
#[test]
fn the_native_and_wasm_backends_give_the_same_results() {
    for level in ["0", "1", "2"] {
        let native = fe_test("native", level, &["--jobs", "2"], &fixture("shared"));
        let wasm = fe_test("wasm", level, &[], &fixture("shared"));
        assert_eq!(
            results(&native),
            expected(&SHARED_PASSING, &SHARED_FAILING),
            "native -O{level}:\n{}",
            stdout(&native)
        );
        assert_eq!(results(&native), results(&wasm), "-O{level}");
        assert_eq!(native.status.code(), wasm.status.code());
    }
}

#[test]
fn a_failed_assertion_and_each_trap_are_reported_with_the_engine_message() {
    let output = fe_test("wasm", "1", &[], &fixture("shared"));
    for name in SHARED_FAILING {
        let failure = failure_of(&output, name);
        assert!(
            failure.starts_with("wasm test failed: trap: unreachable"),
            "{name}: {failure}"
        );
    }
    // The failures are listed again at the end.
    let text = stdout(&output);
    let tail = &text[text.find("failures:").expect("a failure list")..];
    for name in SHARED_FAILING {
        assert!(tail.contains(&format!("    {name}")), "{name} in {tail}");
    }
}

#[test]
fn host_operations_logs_aborts_and_refusals() {
    let output = fe_test("wasm", "1", &[], &fixture("wasm_only"));
    assert_eq!(
        results(&output),
        expected(
            &[
                "logs_numbers_and_reads_the_clock",
                "a_missing_file_is_not_found",
                "mock_files_need_no_host",
            ],
            &[
                "aborts_with_a_code",
                "the_mock_abort_traps",
                "a_forged_handle_is_refused_by_the_host",
            ],
        ),
        "{}",
        stdout(&output)
    );
    assert_eq!(
        failure_of(&output, "aborts_with_a_code"),
        "wasm test failed: the test aborted with code 7"
    );
    assert!(
        failure_of(&output, "the_mock_abort_traps")
            .starts_with("wasm test failed: trap: unreachable"),
    );
    let forged = failure_of(&output, "a_forged_handle_is_refused_by_the_host");
    assert!(
        forged.starts_with("wasm test failed: host contract violation:")
            && forged.contains("handle 77 was never issued by this host"),
        "{forged}"
    );
}

#[test]
fn tests_the_backend_cannot_build_fail_with_the_reason() {
    let output = fe_test("wasm", "1", &[], &fixture("unsupported"));
    assert_eq!(
        results(&output),
        expected(
            &["builds_and_passes"],
            &["uses_an_evm_builtin", "expects_an_evm_revert"]
        ),
        "{}",
        stdout(&output)
    );
    let builtin = failure_of(&output, "uses_an_evm_builtin");
    assert!(
        builtin.starts_with("cannot build `uses_an_evm_builtin` for WebAssembly: unsupported:")
            && builtin.contains("EvmByte"),
        "{builtin}"
    );
    let revert = failure_of(&output, "expects_an_evm_revert");
    assert!(
        revert.contains("cannot build `expects_an_evm_revert` for WebAssembly")
            && revert.contains("`should_revert`"),
        "{revert}"
    );
    assert_eq!(output.status.code(), Some(1));
    assert!(stdout(&output).contains("1 passed; 2 failed"));
}

#[test]
fn a_filter_selects_tests_by_name() {
    let output = fe_test("wasm", "1", &["--filter", "sums"], &fixture("shared"));
    assert_eq!(results(&output), expected(&["sums_a_loop"], &[]));
    assert_eq!(output.status.code(), Some(0));
    assert!(stdout(&output).contains("test result: ok. 1 passed; 0 failed"));
}

#[test]
fn a_missing_node_is_an_error_that_names_the_requirement() {
    let empty = tempdir().unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_fe"))
        .env("PATH", empty.path())
        .args(["test", "--backend", "wasm", "--color", "never"])
        .arg(fixture("shared"))
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(
            "Error: `fe test --backend wasm` runs each test in Node.js (version 20 or later)"
        ) && stderr.contains("`node` could not be started"),
        "{stderr}"
    );
    assert!(
        results(&output).is_empty(),
        "no test is reported, passed or skipped"
    );
}

// The stand-in `node` is a shell script.
#[cfg(unix)]
#[test]
fn a_node_that_is_too_old_is_an_error_that_names_the_version() {
    use std::os::unix::fs::PermissionsExt;

    let bin = tempdir().unwrap();
    let node = bin.path().join("node");
    fs::write(&node, "#!/bin/sh\necho v18.19.0\n").unwrap();
    fs::set_permissions(&node, fs::Permissions::from_mode(0o755)).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_fe"))
        .env("PATH", bin.path())
        .args(["test", "--backend", "wasm", "--color", "never"])
        .arg(fixture("shared"))
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("(version 20 or later)") && stderr.contains("printed `v18.19.0`"),
        "{stderr}"
    );
}

#[test]
fn options_that_have_no_meaning_for_wasm_are_refused() {
    for (args, message) in [
        (&["--show-logs"][..], "do not support EVM logs"),
        (&["--trace-evm"][..], "do not support EVM logs"),
        (&["--call-trace"][..], "do not support EVM logs"),
        (
            &["--emit", "rmir"][..],
            "`--emit rmir` is not available with `--backend wasm`",
        ),
        (
            &["--report"][..],
            "`--report` and `--report-dir` are not available",
        ),
        (
            &["--native-timeout-secs", "5"][..],
            "native test limits require `--backend native`",
        ),
    ] {
        let output = fe_test("wasm", "1", args, &fixture("shared"));
        assert_eq!(output.status.code(), Some(1), "{args:?}");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains(message), "{args:?}: {stderr}");
    }
}

#[test]
fn emit_ir_writes_the_ir_of_each_test() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("pair.fe");
    fs::write(
        &source,
        "#[test]\nfn first() {\n    let x: u32 = 1\n    assert(x == 1)\n}\n\n#[test]\nfn second() {\n    let x: u32 = 2\n    assert(x == 2)\n}\n",
    )
    .unwrap();
    let output = fe_test("wasm", "1", &["--emit", "ir"], &source);
    assert_eq!(output.status.code(), Some(0), "{}", stdout(&output));
    for test in ["first", "second"] {
        let ir = temp.path().join(format!("out/pair.wasm.{test}.test.sona"));
        let text = fs::read_to_string(&ir).unwrap_or_else(|err| panic!("{}: {err}", ir.display()));
        assert!(text.contains("func"), "{text}");
    }
}

#[test]
fn an_ingot_with_tests_runs_on_wasm() {
    let temp = tempdir().unwrap();
    let root = temp.path();
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("fe.toml"),
        "[ingot]\nname = \"sample\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        root.join("src/lib.fe"),
        "pub fn double(_ x: u32) -> u32 { x * 2 }\n\n#[test]\nfn doubles() {\n    assert(double(21) == 42)\n}\n",
    )
    .unwrap();
    fs::write(
        root.join("src/more.fe"),
        "#[test]\nfn in_another_module() {\n    let x: u32 = 5\n    assert(x + 1 == 6)\n}\n",
    )
    .unwrap();
    let output = fe_test("wasm", "1", &[], root);
    assert_eq!(
        output.status.code(),
        Some(0),
        "{}\n{}",
        stdout(&output),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        results(&output),
        expected(&["doubles", "in_another_module"], &[]),
        "{}",
        stdout(&output)
    );
}
