#![cfg(all(
    feature = "cranelift",
    any(
        all(target_arch = "x86_64", target_os = "linux"),
        all(target_arch = "aarch64", target_os = "macos")
    )
))]

use std::{
    fs,
    path::Path,
    process::{Command, Output},
};

use tempfile::tempdir;

fn build(source: &Path, out: &Path, level: &str, extra: &[&str]) -> Output {
    let result = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args([
            "build",
            "--backend",
            "native",
            "--emit",
            "ir,executable",
            "-O",
            level,
        ])
        .arg("--out-dir")
        .arg(out)
        .args(extra)
        .arg(source)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "native build failed:\n{}\n{}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr)
    );
    result
}

#[test]
fn native_workspace_build_selects_root_entries_and_reachable_dependencies() {
    let temp = tempdir().unwrap();
    let root = temp.path();
    fs::create_dir_all(root.join("app/src")).unwrap();
    fs::create_dir_all(root.join("math/src")).unwrap();
    fs::write(
        root.join("fe.toml"),
        r#"
[workspace]
name = "native_workspace"
version = "0.1.0"
members = [{ path = "app", name = "app" }, { path = "math", name = "math" }]
"#,
    )
    .unwrap();
    fs::write(
        root.join("app/fe.toml"),
        r#"
[ingot]
name = "app"
version = "0.1.0"
[dependencies]
math = true
"#,
    )
    .unwrap();
    fs::write(
        root.join("math/fe.toml"),
        "[ingot]\nname = \"math\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        root.join("math/src/lib.fe"),
        "pub fn answer() -> i32 { 42 }\npub fn unused<T>(x: own T) -> T { x }\n#[test]\nfn math_test() { core::assert(answer() == 42) }\n",
    )
    .unwrap();
    fs::write(
        root.join("app/src/lib.fe"),
        "pub fn main() -> i32 { helper::answer() }\n#[test]\nfn root_test() { core::assert(main() == 42) }\n",
    )
    .unwrap();
    fs::write(
        root.join("app/src/helper.fe"),
        "extern { fn abs(value: i32) -> i32 }\npub fn answer() -> i32 { abs(value: -math::answer()) }\n#[test]\nfn nested_test() { core::assert(answer() == 42) }\n",
    )
    .unwrap();
    for level in ["0", "1"] {
        let out = root.join(format!("out-{level}"));
        build(root, &out, level, &["--ingot", "app"]);
        assert_eq!(
            Command::new(out.join("app")).status().unwrap().code(),
            Some(42)
        );
        let ir = fs::read_to_string(out.join("app.native.sona")).unwrap();
        assert!(!ir.contains("unused"));
        build(root, &out, level, &[]);
        assert!(!out.join("math").exists());
        build(&root.join("app"), &out, level, &[]);
        for (selection, expected_count) in [(&["--ingot", "app"][..], 2), (&[][..], 3)] {
            let result = Command::new(env!("CARGO_BIN_EXE_fe"))
                .args(["test", "--backend", "native", "-O", level])
                .args(selection)
                .arg(root)
                .output()
                .unwrap();
            assert!(result.status.success(), "{result:?}");
            assert!(
                String::from_utf8_lossy(&result.stdout)
                    .contains(&format!("{expected_count} passed; 0 failed")),
                "{result:?}"
            );
        }
    }
}

#[test]
fn native_runner_executes_tests_filters_and_retains_only_requested_artifacts() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("cases.fe");
    let scratch = temp.path().join("scratch");
    fs::create_dir(&scratch).unwrap();
    fs::write(
        &source,
        r#"
fn difference(left: u256, right: u256) -> u256 { left - right }
#[test]
fn pass_arithmetic() { core::assert(difference(left: 11, right: 3) == 8) }
#[test]
fn pass_wide() {
    let high: u256 = 1 << 192
    core::assert(difference(left: high + 7, right: high) == 7)
}
#[test]
fn fail_assertion() { core::assert(false) }
"#,
    )
    .unwrap();
    for level in ["0", "1"] {
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", "--jobs", "2", "-O", level])
            .env("TMPDIR", &scratch)
            .arg(&source)
            .output()
            .unwrap();
        let stdout = String::from_utf8_lossy(&result.stdout);
        assert!(
            !result.status.success(),
            "false assertion must fail the suite"
        );
        assert!(stdout.contains("2 passed; 1 failed"), "{result:?}");
        assert!(stdout.contains("fail_assertion"), "{result:?}");
        assert!(fs::read_dir(&scratch).unwrap().all(|entry| {
            !entry
                .unwrap()
                .file_name()
                .to_string_lossy()
                .starts_with("fe-native-test-")
        }));

        let report = temp.path().join(format!("report-{level}.tar.gz"));
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args([
                "test",
                "--backend",
                "native",
                "--grouped",
                "--filter",
                "pass_",
                "--emit",
                "ir,rmir",
                "--report",
                "--report-out",
            ])
            .arg(&report)
            .args(["-O", level])
            .env("TMPDIR", &scratch)
            .arg(&source)
            .output()
            .unwrap();
        assert!(result.status.success(), "{result:?}");
        assert!(String::from_utf8_lossy(&result.stdout).contains("2 passed; 0 failed"));
        assert!(fs::read_dir(&scratch).unwrap().all(|entry| {
            !entry
                .unwrap()
                .file_name()
                .to_string_lossy()
                .starts_with("fe-native-test-")
        }));
        let ir = fs::read_to_string(temp.path().join("out/cases.native-0.test.sona")).unwrap();
        assert!(
            !ir.contains("fail_assertion"),
            "filtered test must not be compiled"
        );
        assert!(temp.path().join("out/cases.native-0.test.rmir").exists());
        let listing = Command::new("tar")
            .arg("-tzf")
            .arg(&report)
            .output()
            .unwrap();
        assert!(listing.status.success(), "{listing:?}");
        let listing = String::from_utf8_lossy(&listing.stdout);
        assert_eq!(
            listing
                .lines()
                .filter(|line| line.ends_with("/tests.o"))
                .count(),
            1
        );
        assert!(listing.contains("test-0/status.txt"), "{listing}");
        assert!(listing.contains("test-1/status.txt"), "{listing}");
    }
}

#[test]
fn native_runner_rejects_evm_attributes_and_trace_options() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("unsupported.fe");
    for attribute in ["should_revert", "balance = 10"] {
        fs::write(
            &source,
            format!("#[test({attribute})]\nfn evm_test() {{}}\n"),
        )
        .unwrap();
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native"])
            .arg(&source)
            .output()
            .unwrap();
        assert!(!result.status.success());
        assert!(
            String::from_utf8_lossy(&result.stdout).contains("cannot use EVM"),
            "{result:?}"
        );
    }
    for option in ["--trace-evm", "--show-logs", "--call-trace"] {
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", option])
            .arg(&source)
            .output()
            .unwrap();
        assert!(!result.status.success());
        assert!(
            String::from_utf8_lossy(&result.stderr).contains("EVM logs or tracing"),
            "{result:?}"
        );
    }
}
