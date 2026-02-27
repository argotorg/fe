use dir_test::{Fixture, dir_test};
use std::{
    fs,
    io::IsTerminal,
    path::{Path, PathBuf},
    process::Command,
    sync::OnceLock,
};
use tempfile::tempdir;
use test_utils::snap_test;

// Helper function to normalize paths in output for portability
fn normalize_output(output: &str) -> String {
    // Get the project root directory
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let project_root = std::path::Path::new(manifest_dir)
        .parent()
        .expect("parent")
        .parent()
        .expect("parent");

    // Replace absolute paths with relative ones
    output.replace(&project_root.to_string_lossy().to_string(), "<project>")
}

// Helper function to run fe check
fn run_fe_check(path: &str) -> (String, i32) {
    run_fe_command("check", path)
}

// Helper function to run fe tree
fn run_fe_tree(path: &str) -> (String, i32) {
    run_fe_command("tree", path)
}

// Helper function to run fe binary with specified subcommand
fn run_fe_command(subcommand: &str, path: &str) -> (String, i32) {
    run_fe_command_with_args(subcommand, path, &[])
}

fn run_fe_command_with_args(subcommand: &str, path: &str, extra: &[&str]) -> (String, i32) {
    let mut args = Vec::with_capacity(2 + extra.len());
    args.push(subcommand);
    args.extend_from_slice(extra);
    args.push(path);
    run_fe_main(&args)
}

// Helper function to run fe binary with specified args
fn run_fe_main(args: &[&str]) -> (String, i32) {
    let out = run_fe_main_impl(args, None, &[]);
    (out.combined(), out.exit_code)
}

fn run_fe_main_with_env(args: &[&str], extra_env: &[(&str, &str)]) -> (String, i32) {
    let out = run_fe_main_impl(args, None, extra_env);
    (out.combined(), out.exit_code)
}

fn run_fe_main_in_dir(args: &[&str], cwd: &Path) -> (String, i32) {
    let out = run_fe_main_impl(args, Some(cwd), &[]);
    (out.combined(), out.exit_code)
}

fn run_fe_main_in_dir_with_env(
    args: &[&str],
    cwd: &Path,
    extra_env: &[(&str, &str)],
) -> (String, i32) {
    let out = run_fe_main_impl(args, Some(cwd), extra_env);
    (out.combined(), out.exit_code)
}

struct FeOutput {
    stdout: String,
    stderr: String,
    exit_code: i32,
}

impl FeOutput {
    /// Combined display format used by snapshot tests.
    fn combined(&self) -> String {
        let mut out = String::new();
        if !self.stdout.is_empty() {
            out.push_str("=== STDOUT ===\n");
            out.push_str(&self.stdout);
        }
        if !self.stderr.is_empty() {
            if !out.is_empty() {
                out.push('\n');
            }
            out.push_str("=== STDERR ===\n");
            out.push_str(&self.stderr);
        }
        out.push_str(&format!("\n=== EXIT CODE: {} ===", self.exit_code));
        normalize_output(&out)
    }
}

fn fe_binary() -> &'static PathBuf {
    static BIN: OnceLock<PathBuf> = OnceLock::new();
    BIN.get_or_init(|| {
        if let Some(bin) = std::env::var_os("CARGO_BIN_EXE_fe") {
            return PathBuf::from(bin);
        }

        let cargo_exe = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());
        let output = Command::new(&cargo_exe)
            .args(["build", "--bin", "fe"])
            .output()
            .expect("Failed to build fe binary");

        if !output.status.success() {
            panic!(
                "Failed to build fe binary: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }

        std::env::current_exe()
            .expect("Failed to get current exe")
            .parent()
            .expect("Failed to get parent")
            .parent()
            .expect("Failed to get parent")
            .join(format!("fe{}", std::env::consts::EXE_SUFFIX))
    })
}

fn run_fe_main_impl(args: &[&str], cwd: Option<&Path>, extra_env: &[(&str, &str)]) -> FeOutput {
    let mut cmd = Command::new(fe_binary());
    cmd.args(args).env("NO_COLOR", "1");
    for (key, value) in extra_env {
        cmd.env(key, value);
    }
    if let Some(dir) = cwd {
        cmd.current_dir(dir);
    }
    let output = cmd
        .output()
        .unwrap_or_else(|_| panic!("Failed to run fe {:?}", args));

    FeOutput {
        stdout: normalize_output(&String::from_utf8_lossy(&output.stdout)),
        stderr: normalize_output(&String::from_utf8_lossy(&output.stderr)),
        exit_code: output.status.code().unwrap_or(-1),
    }
}

#[cfg(unix)]
fn write_fake_solc(temp: &tempfile::TempDir) -> std::path::PathBuf {
    use std::os::unix::fs::PermissionsExt;

    let path = temp.path().join("fake-solc");
    let script = r#"#!/bin/sh
set -e
if [ "$1" = "--version" ]; then
  echo "solc, the Solidity compiler version 0.0.0+fake"
  exit 0
fi

INPUT="$(cat)"

EXPECTED_OPTIMIZE="${FAKE_SOLC_EXPECT_OPTIMIZE:-}"
if [ -n "$EXPECTED_OPTIMIZE" ]; then
  if [ "$EXPECTED_OPTIMIZE" = "true" ]; then
    echo "$INPUT" | grep -q '"enabled":true' || {
      echo "expected optimizer enabled" >&2
      exit 1
    }
  else
    echo "$INPUT" | grep -q '"enabled":false' || {
      echo "expected optimizer disabled" >&2
      exit 1
    }
  fi
fi

NAME="${FAKE_SOLC_CONTRACT:-}"
if [ -n "$NAME" ]; then
  cat <<EOF
{"contracts":{"input.yul":{"$NAME":{"evm":{"bytecode":{"object":"6000"},"deployedBytecode":{"object":"6000"}}}}}}
EOF
  exit 0
fi

cat <<EOF
{"contracts":{"input.yul":{"Foo":{"evm":{"bytecode":{"object":"6000"},"deployedBytecode":{"object":"6000"}}},"Bar":{"evm":{"bytecode":{"object":"6000"},"deployedBytecode":{"object":"6000"}}}}}}
EOF
"#;
    fs::write(&path, script).expect("write fake solc");
    let mut perms = fs::metadata(&path).expect("stat fake solc").permissions();
    perms.set_mode(0o755);
    fs::set_permissions(&path, perms).expect("chmod fake solc");
    path
}

fn solc_available() -> bool {
    static AVAILABLE: OnceLock<bool> = OnceLock::new();
    *AVAILABLE.get_or_init(|| {
        let solc_path = std::env::var_os("FE_SOLC_PATH")
            .map(std::path::PathBuf::from)
            .filter(|path| path.is_file())
            .unwrap_or_else(|| std::path::PathBuf::from("solc"));
        Command::new(solc_path)
            .arg("--version")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .map(|status| status.success())
            .unwrap_or(false)
    })
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/cli_output/build",
    glob: "*.fe",
)]
fn test_cli_build_contract_not_found(fixture: Fixture<&str>) {
    let fixture_path = std::path::Path::new(fixture.path());
    let fixture_name = fixture_path
        .file_stem()
        .expect("fixture should have stem")
        .to_str()
        .expect("fixture stem should be utf8");
    let snapshot_path = fixture_path
        .parent()
        .expect("fixture should have parent")
        .join(format!("{fixture_name}_build_contract_not_found.case"));
    let (output, exit_code) = run_fe_main(&["build", "--contract", "DoesNotExist", fixture.path()]);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[cfg(unix)]
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/cli_output/build",
    glob: "*.fe",
)]
fn test_cli_build_fake_solc_artifacts(fixture: Fixture<&str>) {
    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--contract",
            "Foo",
            "--out-dir",
            out_dir_str.as_str(),
            fixture.path(),
        ],
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", "Foo"),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let deploy_path = out_dir.join("Foo.bin");
    let runtime_path = out_dir.join("Foo.runtime.bin");
    let deploy = fs::read_to_string(&deploy_path).expect("read deploy bytecode");
    let runtime = fs::read_to_string(&runtime_path).expect("read runtime bytecode");

    let mut snapshot = output.replace(&out_dir_str, "<out>");
    snapshot.push_str("\n\n=== ARTIFACTS ===\n");
    snapshot.push_str(&format!("Foo.bin: {}\n", deploy.trim()));
    snapshot.push_str(&format!("Foo.runtime.bin: {}\n", runtime.trim()));

    let fixture_path = std::path::Path::new(fixture.path());
    let fixture_name = fixture_path
        .file_stem()
        .expect("fixture should have stem")
        .to_str()
        .expect("fixture stem should be utf8");
    let snapshot_path = fixture_path
        .parent()
        .expect("fixture should have parent")
        .join(format!("{fixture_name}_build_fake_solc.case"));
    snap_test!(snapshot, snapshot_path.to_str().unwrap());
}

#[cfg(unix)]
#[test]
fn test_cli_build_all_contracts_fake_solc_artifacts() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/multi_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--out-dir",
            out_dir_str.as_str(),
            fixture_path_str,
        ],
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", ""),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let bar_deploy_path = out_dir.join("Bar.bin");
    let bar_runtime_path = out_dir.join("Bar.runtime.bin");
    let foo_deploy_path = out_dir.join("Foo.bin");
    let foo_runtime_path = out_dir.join("Foo.runtime.bin");

    let bar_deploy = fs::read_to_string(&bar_deploy_path).expect("read Bar deploy bytecode");
    let bar_runtime = fs::read_to_string(&bar_runtime_path).expect("read Bar runtime bytecode");
    let foo_deploy = fs::read_to_string(&foo_deploy_path).expect("read Foo deploy bytecode");
    let foo_runtime = fs::read_to_string(&foo_runtime_path).expect("read Foo runtime bytecode");

    let mut snapshot = output.replace(&out_dir_str, "<out>");
    snapshot.push_str("\n\n=== ARTIFACTS ===\n");
    snapshot.push_str(&format!("Bar.bin: {}\n", bar_deploy.trim()));
    snapshot.push_str(&format!("Bar.runtime.bin: {}\n", bar_runtime.trim()));
    snapshot.push_str(&format!("Foo.bin: {}\n", foo_deploy.trim()));
    snapshot.push_str(&format!("Foo.runtime.bin: {}\n", foo_runtime.trim()));

    let snapshot_path = fixture_path
        .parent()
        .expect("fixture should have parent")
        .join("multi_contract_build_all_fake_solc.case");
    snap_test!(snapshot, snapshot_path.to_str().unwrap());
}

#[cfg(unix)]
#[test]
fn test_cli_build_ingot_dir_fake_solc_artifacts() {
    let fixture_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build_ingots/simple");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--contract",
            "Foo",
            "--out-dir",
            out_dir_str.as_str(),
            fixture_dir_str,
        ],
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", ""),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let deploy_path = out_dir.join("Foo.bin");
    let runtime_path = out_dir.join("Foo.runtime.bin");
    let deploy = fs::read_to_string(&deploy_path).expect("read deploy bytecode");
    let runtime = fs::read_to_string(&runtime_path).expect("read runtime bytecode");

    let mut snapshot = output.replace(&out_dir_str, "<out>");
    snapshot.push_str("\n\n=== ARTIFACTS ===\n");
    snapshot.push_str(&format!("Foo.bin: {}\n", deploy.trim()));
    snapshot.push_str(&format!("Foo.runtime.bin: {}\n", runtime.trim()));

    let snapshot_path = fixture_dir.join("build_fake_solc.case");
    snap_test!(snapshot, snapshot_path.to_str().unwrap());
}

#[cfg(unix)]
#[test]
fn test_cli_build_ingot_dir_all_contracts_multi_file_fake_solc_artifacts() {
    let fixture_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build_ingots/multi_file");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--out-dir",
            out_dir_str.as_str(),
            fixture_dir_str,
        ],
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", ""),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let bar_deploy_path = out_dir.join("Bar.bin");
    let bar_runtime_path = out_dir.join("Bar.runtime.bin");
    let foo_deploy_path = out_dir.join("Foo.bin");
    let foo_runtime_path = out_dir.join("Foo.runtime.bin");

    let bar_deploy = fs::read_to_string(&bar_deploy_path).expect("read Bar deploy bytecode");
    let bar_runtime = fs::read_to_string(&bar_runtime_path).expect("read Bar runtime bytecode");
    let foo_deploy = fs::read_to_string(&foo_deploy_path).expect("read Foo deploy bytecode");
    let foo_runtime = fs::read_to_string(&foo_runtime_path).expect("read Foo runtime bytecode");

    let mut snapshot = output.replace(&out_dir_str, "<out>");
    snapshot.push_str("\n\n=== ARTIFACTS ===\n");
    snapshot.push_str(&format!("Bar.bin: {}\n", bar_deploy.trim()));
    snapshot.push_str(&format!("Bar.runtime.bin: {}\n", bar_runtime.trim()));
    snapshot.push_str(&format!("Foo.bin: {}\n", foo_deploy.trim()));
    snapshot.push_str(&format!("Foo.runtime.bin: {}\n", foo_runtime.trim()));

    let snapshot_path = fixture_dir.join("build_all_fake_solc.case");
    snap_test!(snapshot, snapshot_path.to_str().unwrap());
}

fn assert_hex_artifact(path: &std::path::Path) {
    let contents = fs::read_to_string(path).unwrap_or_else(|err| {
        panic!("read artifact {path:?}: {err}");
    });
    assert!(
        contents.ends_with('\n'),
        "expected artifact to end with newline: {path:?}"
    );
    let trimmed = contents.trim();
    assert!(!trimmed.is_empty(), "expected non-empty hex: {path:?}");
    assert!(
        trimmed.chars().all(|c| c.is_ascii_hexdigit()),
        "expected hex bytes in artifact: {path:?}"
    );
    assert_eq!(
        trimmed.len() % 2,
        0,
        "expected even-length hex in artifact: {path:?}"
    );
}

fn assert_non_empty_text_artifact(path: &std::path::Path) {
    let contents = fs::read_to_string(path).unwrap_or_else(|err| {
        panic!("read artifact {path:?}: {err}");
    });
    assert!(
        !contents.trim().is_empty(),
        "expected non-empty text artifact: {path:?}"
    );
}

#[test]
fn test_cli_build_defaults_to_sonatina_and_writes_hex_artifacts() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_path_str,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    assert_hex_artifact(&out_dir.join("Foo.bin"));
    assert_hex_artifact(&out_dir.join("Foo.runtime.bin"));
}

#[test]
fn test_cli_build_emit_ir_only_writes_sonatina_ir() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--contract",
        "Foo",
        "--emit",
        "ir",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_path_str,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    assert_non_empty_text_artifact(&out_dir.join("simple_contract.sona"));
    assert!(
        !out_dir.join("Foo.bin").exists(),
        "did not expect deploy bytecode with --emit ir"
    );
    assert!(
        !out_dir.join("Foo.runtime.bin").exists(),
        "did not expect runtime bytecode with --emit ir"
    );
}

#[test]
fn test_cli_build_emit_bytecode_only_writes_deploy_hex() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--contract",
        "Foo",
        "--emit",
        "bytecode",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_path_str,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    assert_hex_artifact(&out_dir.join("Foo.bin"));
    assert!(
        !out_dir.join("Foo.runtime.bin").exists(),
        "did not expect runtime bytecode with --emit bytecode"
    );
}

#[test]
fn test_cli_build_emit_runtime_bytecode_only_writes_runtime_hex() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--contract",
        "Foo",
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_path_str,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    assert_hex_artifact(&out_dir.join("Foo.runtime.bin"));
    assert!(
        !out_dir.join("Foo.bin").exists(),
        "did not expect deploy bytecode with --emit runtime-bytecode"
    );
}

#[test]
fn test_cli_build_emit_ir_yul_does_not_require_solc() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--backend",
        "yul",
        "--emit",
        "ir",
        "--solc",
        "/definitely/missing/solc",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_path_str,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    assert_non_empty_text_artifact(&out_dir.join("simple_contract.yul"));
    assert!(
        !out_dir.join("Foo.bin").exists(),
        "did not expect deploy bytecode with --emit ir"
    );
    assert!(
        !out_dir.join("Foo.runtime.bin").exists(),
        "did not expect runtime bytecode with --emit ir"
    );
}

#[test]
fn test_cli_build_emit_ir_workspace_writes_per_member_ir_subdirs() {
    let root = workspace_fixture("build_workspace_root");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_in_dir(
        &["build", "--emit", "ir", "--out-dir", out_dir_str.as_str()],
        &root,
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    assert_non_empty_text_artifact(&out_dir.join("a/a.sona"));
    assert_non_empty_text_artifact(&out_dir.join("b/b.sona"));
    assert!(
        !out_dir.join("Foo.bin").exists(),
        "did not expect flat bytecode artifacts with --emit ir"
    );
    assert!(
        !out_dir.join("Bar.runtime.bin").exists(),
        "did not expect flat bytecode artifacts with --emit ir"
    );
}

#[test]
fn test_cli_build_emit_runtime_bytecode_snake_case_rejected() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let (output, exit_code) =
        run_fe_main(&["build", "--emit", "runtime_bytecode", fixture_path_str]);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("runtime_bytecode"),
        "expected clap error to mention invalid value:\n{output}"
    );
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/cli_output/single_files",
    glob: "*.fe",
)]
fn test_cli_single_file(fixture: Fixture<&str>) {
    let (output, _) = run_fe_check(fixture.path());
    snap_test!(output, fixture.path());
}

// Back to dir_test - the unstable numbers are annoying but at least it works
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/cli_output/ingots",
    glob: "**/fe.toml",
)]
fn test_cli_ingot(fixture: Fixture<&str>) {
    let ingot_dir = std::path::Path::new(fixture.path())
        .parent()
        .expect("fe.toml should have parent");

    let (output, _) = run_fe_check(ingot_dir.to_str().unwrap());

    // Use the ingot directory name for the snapshot to avoid numbering
    let ingot_name = ingot_dir.file_name().unwrap().to_str().unwrap();
    let snapshot_path = ingot_dir.join(ingot_name);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/cli_output/ingots",
    glob: "**/fe.toml",
)]
fn test_tree_output(fixture: Fixture<&str>) {
    // Skip tree snapshots when stdout isn't a TTY (e.g. in headless test runners),
    // since the tree UI assumes an interactive terminal.
    if !std::io::stdout().is_terminal() {
        return;
    }

    let ingot_dir = std::path::Path::new(fixture.path())
        .parent()
        .expect("fe.toml should have parent");

    let (output, _) = run_fe_tree(ingot_dir.to_str().unwrap());

    // Use the ingot directory name for the snapshot with _tree suffix
    let ingot_name = ingot_dir.file_name().unwrap().to_str().unwrap();
    let snapshot_path = ingot_dir.join(format!("{}_tree", ingot_name));
    snap_test!(output, snapshot_path.to_str().unwrap());
}

/// Runs both backends on each fixture, asserts each passes, and asserts they produce
/// identical test results.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test",
    glob: "*.fe",
)]
fn test_fe_test_both_backends(fixture: Fixture<&str>) {
    assert_fe_test_backends_agree(fixture.path(), false);
}

/// Runs a focused subset of fixtures with call tracing enabled to preserve
/// call-trace parity coverage without slowing down the full backend matrix.
#[test]
fn test_fe_test_both_backends_call_trace() {
    let fixtures_dir =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fe_test");
    for fixture in ["contract_call.fe", "factory.fe", "should_revert.fe"] {
        let path = fixtures_dir.join(fixture);
        let path = path
            .to_str()
            .unwrap_or_else(|| panic!("fixture path is not utf-8: {}", path.display()));
        assert_fe_test_backends_agree(path, true);
    }
}

fn assert_fe_test_backends_agree(path: &str, call_trace: bool) {
    let mut yul_args = vec!["test", "--backend", "yul", path];
    let mut sonatina_args = vec!["test", "--backend", "sonatina", path];
    if call_trace {
        yul_args.insert(1, "--call-trace");
        sonatina_args.insert(1, "--call-trace");
    }
    let trace_flag = if call_trace { "--call-trace " } else { "" };
    let sonatina = run_fe_main_impl(&sonatina_args, None, &[]);
    assert_eq!(
        sonatina.exit_code,
        0,
        "fe test (sonatina) failed for {path}:\n{so}\n\nTo reproduce:\n  cargo run --bin fe -- test {trace_flag}--backend sonatina --report {path}",
        path = path,
        trace_flag = trace_flag,
        so = sonatina.combined(),
    );

    if !solc_available() {
        #[allow(clippy::print_stdout)]
        {
            println!("skipping yul backend comparison because `solc` is missing");
        }
        return;
    }

    let yul = run_fe_main_impl(&yul_args, None, &[]);

    // Check each backend independently first for clearer diagnostics.
    assert_eq!(
        yul.exit_code,
        0,
        "fe test (yul) failed for {path}:\n{yo}\n\nTo reproduce:\n  cargo run --bin fe -- test {trace_flag}--backend yul --report {path}",
        path = path,
        trace_flag = trace_flag,
        yo = yul.combined(),
    );

    // Compare stdout (test names + pass/fail lines). Stderr may contain
    // backend-specific diagnostics so we only check stdout here.
    assert_eq!(
        yul.stdout,
        sonatina.stdout,
        "Test output mismatch for {path}:\n\n--- yul ---\n{yo}\n\n--- sonatina ---\n{so}",
        path = path,
        yo = yul.stdout,
        so = sonatina.stdout,
    );
}

/// Runs `fe test` and snapshots the output to verify behavior of passing/failing tests and logs.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test_runner",
    glob: "*.fe",
)]
fn test_fe_test_runner(fixture: Fixture<&str>) {
    let mut args = vec!["test"];
    if fixture.path().contains("logs") {
        args.push("--show-logs");
    }
    args.push(fixture.path());

    let (output, _) = run_fe_main(&args);
    snap_test!(output, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/cli_output/ingots/library",
    glob: "**/app/fe.toml",
)]
fn test_cli_library(fixture: Fixture<&str>) {
    let app_dir = std::path::Path::new(fixture.path())
        .parent()
        .expect("fe.toml should have parent");
    let (output, _) = run_fe_check(app_dir.to_str().unwrap());
    let case_name = app_dir
        .parent()
        .and_then(|parent| parent.file_name())
        .expect("library fixture parent")
        .to_str()
        .unwrap();
    let snapshot_path = app_dir.join(format!("library_{}", case_name));
    snap_test!(output, snapshot_path.to_str().unwrap());
}

fn workspace_fixture(path: &str) -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/workspaces")
        .join(path)
}

fn explicit_path_fixture(path: &str) -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/explicit_paths")
        .join(path)
}

#[test]
fn test_cli_workspace_member_by_name() {
    let root = workspace_fixture("member_resolution");
    let snapshot_path = root.join("by_name.case");
    let (output, _) = run_fe_main_in_dir(&["check", "app"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_member_by_path() {
    let root = workspace_fixture("member_resolution");
    let snapshot_path = root.join("by_path.case");
    let (output, _) = run_fe_main_in_dir(&["check", "ingots/app"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_default_path() {
    let root = workspace_fixture("member_resolution");
    let member_dir = root.join("ingots/app");
    let snapshot_path = root.join("default_path.case");
    let (output, _) = run_fe_main_in_dir(&["check"], &member_dir);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_tree_workspace_default_path() {
    let root = workspace_fixture("member_resolution");
    let member_dir = root.join("ingots/app");
    let snapshot_path = root.join("tree_default_path.case");
    let (output, _) = run_fe_main_in_dir(&["tree"], &member_dir);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_tree_workspace_root_default_path() {
    let root = workspace_fixture("member_resolution");
    let snapshot_path = root.join("tree_root_default_path.case");
    let (output, _) = run_fe_main_in_dir(&["tree"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_test_workspace_root_is_workspace_aware() {
    let root = workspace_fixture("test_workspace_fe_test_core_std_no_tests");
    let (output, exit_code) = run_fe_main_in_dir(&["test"], &root);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("running `fe test` for 2 inputs"),
        "expected workspace member expansion, got:\n{output}"
    );
    assert!(
        output.contains("No tests found in"),
        "expected no-tests warning, got:\n{output}"
    );
    assert!(
        !output.contains("Failed to emit test"),
        "unexpected codegen failure:\n{output}"
    );
    assert!(
        !output.contains("std::evm::EvmTarget"),
        "unexpected EvmTarget resolution error:\n{output}"
    );
}

#[test]
fn test_cli_test_workspace_ingot_selects_single_ingot() {
    let root = workspace_fixture("test_workspace_fe_test_core_std_no_tests");
    let (output, exit_code) = run_fe_main_in_dir(&["test", "--ingot", "app"], &root);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("ingots/core"),
        "expected selected ingot path in output, got:\n{output}"
    );
    assert!(
        !output.contains("ingots/std"),
        "did not expect non-selected ingot path in output, got:\n{output}"
    );
}

#[test]
fn test_cli_test_workspace_ingot_missing_member_is_error() {
    let root = workspace_fixture("test_workspace_fe_test_core_std_no_tests");
    let (output, exit_code) = run_fe_main_in_dir(&["test", "--ingot", "missing"], &root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("No workspace member named \"missing\""),
        "expected missing-member error, got:\n{output}"
    );
}

/// Regression test: `fe test` must discover tests in non-root modules of an
/// ingot even when `lib.fe` itself contains no `#[test]` functions.
#[test]
fn test_cli_test_ingot_discovers_tests_in_non_root_modules() {
    let fixture_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test_runner/ingot_tests_in_non_root_module");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["test", fixture_dir_str]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("test test_add"),
        "expected test_add to be discovered, got:\n{output}"
    );
    assert!(
        output.contains("1 passed"),
        "expected 1 passed test, got:\n{output}"
    );
}

#[test]
fn test_cli_test_repo_core_ingot_without_tests_is_ok() {
    let project_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("fe crate parent")
        .parent()
        .expect("workspace root");
    let core_ingot = project_root.join("ingots/core");
    let core_ingot = core_ingot.to_str().expect("core ingot path utf8");

    let (output, exit_code) = run_fe_main(&["test", core_ingot]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("No tests found in"),
        "expected no-tests warning, got:\n{output}"
    );
    assert!(
        !output.contains("std::evm::EvmTarget"),
        "unexpected EvmTarget resolution error:\n{output}"
    );
}

#[test]
fn test_tree_workspace_default_member_version() {
    let root = workspace_fixture("tree_default_member_version");
    let snapshot_path = root.join("tree_default_member_version.case");
    let (output, _) = run_fe_main_in_dir(&["tree"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_tree_exits_1_on_init_diagnostics() {
    let fixture_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/ingots/cycle_a");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["tree", fixture_dir_str]);
    assert_eq!(exit_code, 1, "expected exit code 1:\n{output}");
    assert!(
        output.contains("=== STDOUT ==="),
        "expected tree output on stdout:\n{output}"
    );
    assert!(
        output.contains("=== STDERR ==="),
        "expected diagnostics on stderr:\n{output}"
    );
    assert!(
        output.contains("Error:"),
        "expected Error diagnostics:\n{output}"
    );
}

#[test]
fn test_cli_workspace_name_path_mismatch() {
    let root = workspace_fixture("ambiguous_mismatch");
    let snapshot_path = root.join("mismatch.case");
    let (output, _) = run_fe_main_in_dir(&["check", "app"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_name_path_same() {
    let root = workspace_fixture("ambiguous_same");
    let snapshot_path = root.join("same.case");
    let (output, _) = run_fe_main_in_dir(&["check", "app"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_inter_workspace_dependency() {
    let root = workspace_fixture("inter_workspace");
    let workspace_a = root.join("workspace_a");
    let snapshot_path = root.join("app_dep.case");
    let (output, _) = run_fe_main_in_dir(&["check", "ingots/app"], &workspace_a);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_dependency_in_scope() {
    let root = workspace_fixture("dependency_scope");
    let snapshot_path = root.join("dependency_in_scope.case");
    let (output, _) = run_fe_main_in_dir(&["check", "ingots/app"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_dependency_in_scope_file_path() {
    let root = workspace_fixture("dependency_scope");
    let snapshot_path = root.join("dependency_in_scope_file_path.case");
    let (output, _) = run_fe_main_in_dir(&["check", "ingots/app/src/lib.fe"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_dependency_in_scope_file_path_standalone() {
    let root = workspace_fixture("dependency_scope");
    let snapshot_path = root.join("dependency_in_scope_file_path_standalone.case");
    let (output, exit_code) =
        run_fe_main_in_dir(&["check", "--standalone", "ingots/app/src/lib.fe"], &root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_build_workspace_contract_ambiguous() {
    let root = workspace_fixture("build_contract_ambiguity");
    let snapshot_path = root.join("build_contract_ambiguity_contract_ambiguous.case");
    let (output, exit_code) = run_fe_main_in_dir(&["build", "--contract", "Foo"], &root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_build_workspace_collisions_are_rejected() {
    let root = workspace_fixture("build_contract_ambiguity");
    let snapshot_path = root.join("build_contract_ambiguity_collisions.case");
    let (output, exit_code) = run_fe_main_in_dir(&["build"], &root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_build_workspace_case_insensitive_collisions_are_rejected() {
    let root = workspace_fixture("build_contract_case_collision");
    let snapshot_path = root.join("build_contract_case_collision.case");
    let (output, exit_code) = run_fe_main_in_dir(&["build"], &root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_build_emit_ir_workspace_member_case_collisions_are_rejected() {
    let root = workspace_fixture("build_ir_member_case_collision");
    let snapshot_path = root.join("build_ir_member_case_collision.case");
    let (output, exit_code) = run_fe_main_in_dir(&["build", "--emit", "ir"], &root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_build_workspace_root_contract_not_found() {
    let root = workspace_fixture("build_workspace_root");
    let snapshot_path = root.join("build_workspace_root_contract_not_found.case");
    let (output, exit_code) = run_fe_main_in_dir(&["build", "--contract", "DoesNotExist"], &root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_build_workspace_root_defaults_to_sonatina_and_writes_hex_artifacts() {
    let root = workspace_fixture("build_workspace_root");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) =
        run_fe_main_in_dir(&["build", "--out-dir", out_dir_str.as_str()], &root);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    assert_hex_artifact(&out_dir.join("Foo.bin"));
    assert_hex_artifact(&out_dir.join("Foo.runtime.bin"));
    assert_hex_artifact(&out_dir.join("Bar.bin"));
    assert_hex_artifact(&out_dir.join("Bar.runtime.bin"));
}

#[test]
fn test_cli_build_workspace_root_ingot_selects_single_member() {
    let root = workspace_fixture("build_workspace_root");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_in_dir(
        &["build", "--out-dir", out_dir_str.as_str(), "--ingot", "a"],
        &root,
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    assert_hex_artifact(&out_dir.join("Foo.bin"));
    assert_hex_artifact(&out_dir.join("Foo.runtime.bin"));
    assert!(
        !out_dir.join("Bar.bin").exists(),
        "did not expect artifacts for non-selected member"
    );
    assert!(
        !out_dir.join("Bar.runtime.bin").exists(),
        "did not expect artifacts for non-selected member"
    );
}

#[test]
fn test_cli_build_workspace_root_ingot_missing_member_is_error() {
    let root = workspace_fixture("build_workspace_root");
    let (output, exit_code) = run_fe_main_in_dir(&["build", "--ingot", "missing"], &root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("No workspace member named \"missing\""),
        "expected missing-member error, got:\n{output}"
    );
}

#[test]
fn test_cli_check_workspace_ingot_missing_member_is_error() {
    let root = workspace_fixture("build_workspace_root");
    let (output, exit_code) = run_fe_main_in_dir(&["check", "--ingot", "missing"], &root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("No workspace member named \"missing\""),
        "expected missing-member error, got:\n{output}"
    );
}

#[test]
fn test_cli_check_ingot_requires_workspace_root() {
    let root = workspace_fixture("build_workspace_root");
    let member = root.join("ingots/a");
    let (output, exit_code) = run_fe_main_in_dir(&["check", "--ingot", "a"], &member);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("`--ingot` requires an input path that resolves to a workspace root"),
        "expected workspace-root error, got:\n{output}"
    );
}

#[test]
fn test_cli_check_workspace_ingot_does_not_match_directory_name() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();

    fs::write(
        root.join("fe.toml"),
        r#"[workspace]
name = "ingot_member_identity"
version = "0.1.0"
members = [
  { path = "ingots/target", name = "a" },
  "libs/*",
]
"#,
    )
    .expect("write workspace fe.toml");

    let target_src = root.join("ingots/target/src");
    fs::create_dir_all(&target_src).expect("create target src dir");
    fs::write(
        root.join("ingots/target/fe.toml"),
        "[ingot]\nname = \"a\"\nversion = \"0.1.0\"\n",
    )
    .expect("write target fe.toml");
    fs::write(target_src.join("lib.fe"), "pub fn main() {}\n").expect("write target source");

    let non_target_src = root.join("libs/a/src");
    fs::create_dir_all(&non_target_src).expect("create non-target src dir");
    fs::write(
        root.join("libs/a/fe.toml"),
        "[ingot]\nname = \"not_a\"\nversion = \"0.1.0\"\n",
    )
    .expect("write non-target fe.toml");
    fs::write(non_target_src.join("lib.fe"), "pub fn broken(\n").expect("write non-target source");

    let (output, exit_code) = run_fe_main_in_dir(&["check", "--ingot", "a"], root);
    assert_eq!(exit_code, 0, "fe check --ingot a failed:\n{output}");
}

#[cfg(unix)]
#[test]
fn test_cli_build_workspace_root_fake_solc_artifacts() {
    let root = workspace_fixture("build_workspace_root");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_in_dir_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--out-dir",
            out_dir_str.as_str(),
        ],
        &root,
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", ""),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let foo_deploy_path = out_dir.join("Foo.bin");
    let foo_runtime_path = out_dir.join("Foo.runtime.bin");
    let bar_deploy_path = out_dir.join("Bar.bin");
    let bar_runtime_path = out_dir.join("Bar.runtime.bin");

    let foo_deploy = fs::read_to_string(&foo_deploy_path).expect("read Foo deploy bytecode");
    let foo_runtime = fs::read_to_string(&foo_runtime_path).expect("read Foo runtime bytecode");
    let bar_deploy = fs::read_to_string(&bar_deploy_path).expect("read Bar deploy bytecode");
    let bar_runtime = fs::read_to_string(&bar_runtime_path).expect("read Bar runtime bytecode");

    let mut snapshot = output.replace(&out_dir_str, "<out>");
    snapshot.push_str("\n\n=== ARTIFACTS ===\n");
    snapshot.push_str(&format!("Foo.bin: {}\n", foo_deploy.trim()));
    snapshot.push_str(&format!("Foo.runtime.bin: {}\n", foo_runtime.trim()));
    snapshot.push_str(&format!("Bar.bin: {}\n", bar_deploy.trim()));
    snapshot.push_str(&format!("Bar.runtime.bin: {}\n", bar_runtime.trim()));

    let snapshot_path = root.join("build_workspace_root_fake_solc.case");
    snap_test!(snapshot, snapshot_path.to_str().unwrap());
}

#[cfg(unix)]
#[test]
fn test_cli_build_workspace_root_skips_library_member_fake_solc_artifacts() {
    let root = workspace_fixture("build_workspace_root_skips_library_member");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_in_dir_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--out-dir",
            out_dir_str.as_str(),
        ],
        &root,
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", ""),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let foo_deploy_path = out_dir.join("Foo.bin");
    let foo_runtime_path = out_dir.join("Foo.runtime.bin");
    let bar_deploy_path = out_dir.join("Bar.bin");
    let bar_runtime_path = out_dir.join("Bar.runtime.bin");

    let foo_deploy = fs::read_to_string(&foo_deploy_path).expect("read Foo deploy bytecode");
    let foo_runtime = fs::read_to_string(&foo_runtime_path).expect("read Foo runtime bytecode");
    let bar_deploy = fs::read_to_string(&bar_deploy_path).expect("read Bar deploy bytecode");
    let bar_runtime = fs::read_to_string(&bar_runtime_path).expect("read Bar runtime bytecode");

    let mut snapshot = output.replace(&out_dir_str, "<out>");
    snapshot.push_str("\n\n=== ARTIFACTS ===\n");
    snapshot.push_str(&format!("Foo.bin: {}\n", foo_deploy.trim()));
    snapshot.push_str(&format!("Foo.runtime.bin: {}\n", foo_runtime.trim()));
    snapshot.push_str(&format!("Bar.bin: {}\n", bar_deploy.trim()));
    snapshot.push_str(&format!("Bar.runtime.bin: {}\n", bar_runtime.trim()));

    let snapshot_path = root.join("build_workspace_root_skips_library_member_fake_solc.case");
    snap_test!(snapshot, snapshot_path.to_str().unwrap());
}

#[cfg(unix)]
#[test]
fn test_cli_build_workspace_root_contract_filter_fake_solc_artifacts() {
    let root = workspace_fixture("build_workspace_root");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_in_dir_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--contract",
            "Foo",
            "--out-dir",
            out_dir_str.as_str(),
        ],
        &root,
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", ""),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let foo_deploy_path = out_dir.join("Foo.bin");
    let foo_runtime_path = out_dir.join("Foo.runtime.bin");
    assert!(
        !out_dir.join("Bar.bin").exists(),
        "expected contract filter to skip non-matching member"
    );

    let foo_deploy = fs::read_to_string(&foo_deploy_path).expect("read Foo deploy bytecode");
    let foo_runtime = fs::read_to_string(&foo_runtime_path).expect("read Foo runtime bytecode");

    let mut snapshot = output.replace(&out_dir_str, "<out>");
    snapshot.push_str("\n\n=== ARTIFACTS ===\n");
    snapshot.push_str(&format!("Foo.bin: {}\n", foo_deploy.trim()));
    snapshot.push_str(&format!("Foo.runtime.bin: {}\n", foo_runtime.trim()));

    let snapshot_path = root.join("build_workspace_root_filter_fake_solc.case");
    snap_test!(snapshot, snapshot_path.to_str().unwrap());
}

#[cfg(unix)]
#[test]
fn test_cli_build_workspace_member_by_name_fake_solc_artifacts() {
    let root = workspace_fixture("build_workspace_root");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_in_dir_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--contract",
            "Foo",
            "--out-dir",
            out_dir_str.as_str(),
            "a",
        ],
        &root,
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", "Foo"),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let deploy_path = out_dir.join("Foo.bin");
    let runtime_path = out_dir.join("Foo.runtime.bin");
    let deploy = fs::read_to_string(&deploy_path).expect("read deploy bytecode");
    let runtime = fs::read_to_string(&runtime_path).expect("read runtime bytecode");

    let mut snapshot = output.replace(&out_dir_str, "<out>");
    snapshot.push_str("\n\n=== ARTIFACTS ===\n");
    snapshot.push_str(&format!("Foo.bin: {}\n", deploy.trim()));
    snapshot.push_str(&format!("Foo.runtime.bin: {}\n", runtime.trim()));

    let snapshot_path = root.join("build_member_by_name_fake_solc.case");
    snap_test!(snapshot, snapshot_path.to_str().unwrap());
}

#[cfg(unix)]
#[test]
fn test_cli_build_solc_flag_overrides_env() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);
    let fake_solc_str = fake_solc.to_str().expect("fake solc utf8");

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--contract",
            "Foo",
            "--solc",
            fake_solc_str,
            "--out-dir",
            out_dir_str.as_str(),
            fixture_path_str,
        ],
        &[
            ("FE_SOLC_PATH", "/no/such/solc"),
            ("FAKE_SOLC_CONTRACT", "Foo"),
            ("FAKE_SOLC_EXPECT_OPTIMIZE", "true"),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
    assert!(
        out_dir.join("Foo.bin").is_file(),
        "expected Foo.bin artifact to be written"
    );
    assert!(
        out_dir.join("Foo.runtime.bin").is_file(),
        "expected Foo.runtime.bin artifact to be written"
    );
}

#[test]
fn test_cli_build_solc_flag_is_ignored_for_sonatina() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = temp.path().join("fake-solc-dir");
    fs::create_dir_all(&fake_solc).expect("create fake solc dir");
    let fake_solc_str = fake_solc.to_string_lossy().to_string();

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let out = run_fe_main_impl(
        &[
            "build",
            "--backend",
            "sonatina",
            "--contract",
            "Foo",
            "--solc",
            fake_solc_str.as_str(),
            "--out-dir",
            out_dir_str.as_str(),
            fixture_path_str,
        ],
        None,
        &[],
    );

    assert_eq!(out.exit_code, 0, "fe build failed:\n{}", out.combined());
    assert!(
        out.stderr
            .contains("Warning: --solc is only used with --backend yul; ignoring --solc"),
        "expected warning about ignored --solc, got:\n{}",
        out.combined()
    );
    assert!(
        out_dir.join("Foo.bin").is_file(),
        "expected Foo.bin artifact to be written"
    );
    assert!(
        out_dir.join("Foo.runtime.bin").is_file(),
        "expected Foo.runtime.bin artifact to be written"
    );
}

#[cfg(unix)]
#[test]
fn test_cli_test_solc_flag_overrides_env() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test_runner/pass.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);
    let fake_solc_str = fake_solc.to_str().expect("fake solc utf8");

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "test",
            "--backend",
            "yul",
            "--solc",
            fake_solc_str,
            fixture_path_str,
        ],
        &[
            ("FE_SOLC_PATH", "/no/such/solc"),
            ("FAKE_SOLC_CONTRACT", "test_test_pass"),
            ("FAKE_SOLC_EXPECT_OPTIMIZE", "true"),
        ],
    );
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
}

#[test]
fn test_cli_build_optimize_and_opt_level_0_is_error() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let (output, exit_code) =
        run_fe_main(&["build", "--optimize", "--opt-level", "0", fixture_path_str]);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("Error: --optimize is shorthand for"),
        "expected error about conflicting optimization flags, got:\n{output}"
    );
}

#[test]
fn test_cli_check_optimize_and_opt_level_0_is_error() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let (output, exit_code) =
        run_fe_main(&["check", "--optimize", "--opt-level", "0", fixture_path_str]);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("unexpected argument '--optimize'"),
        "expected `fe check` to reject optimization flags, got:\n{output}"
    );
}

#[test]
fn test_cli_test_optimize_and_opt_level_0_is_error() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test_runner/pass.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let (output, exit_code) =
        run_fe_main(&["test", "--optimize", "--opt-level", "0", fixture_path_str]);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("Error: --optimize is shorthand for"),
        "expected error about conflicting optimization flags, got:\n{output}"
    );
}

#[cfg(unix)]
#[test]
fn test_cli_test_optimize_flag_is_forwarded_to_solc() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test_runner/pass.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);
    let fake_solc_str = fake_solc.to_str().expect("fake solc utf8");

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "test",
            "--backend",
            "yul",
            "--optimize",
            "--solc",
            fake_solc_str,
            fixture_path_str,
        ],
        &[
            ("FE_SOLC_PATH", "/no/such/solc"),
            ("FAKE_SOLC_CONTRACT", "test_test_pass"),
            ("FAKE_SOLC_EXPECT_OPTIMIZE", "true"),
        ],
    );
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
}

#[cfg(unix)]
#[test]
fn test_cli_test_opt_level_enables_solc_optimizer_for_yul() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test_runner/pass.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);
    let fake_solc_str = fake_solc.to_str().expect("fake solc utf8");

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "test",
            "--backend",
            "yul",
            "--opt-level",
            "2",
            "--solc",
            fake_solc_str,
            fixture_path_str,
        ],
        &[
            ("FE_SOLC_PATH", "/no/such/solc"),
            ("FAKE_SOLC_CONTRACT", "test_test_pass"),
            ("FAKE_SOLC_EXPECT_OPTIMIZE", "true"),
        ],
    );
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
}

#[cfg(unix)]
#[test]
fn test_cli_test_opt_level_0_disables_solc_optimizer_for_yul() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test_runner/pass.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);
    let fake_solc_str = fake_solc.to_str().expect("fake solc utf8");

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "test",
            "--backend",
            "yul",
            "--opt-level",
            "0",
            "--solc",
            fake_solc_str,
            fixture_path_str,
        ],
        &[
            ("FE_SOLC_PATH", "/no/such/solc"),
            ("FAKE_SOLC_CONTRACT", "test_test_pass"),
            ("FAKE_SOLC_EXPECT_OPTIMIZE", "false"),
        ],
    );
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
}

#[cfg(unix)]
#[test]
fn test_cli_build_optimize_flag_is_forwarded_to_solc() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--contract",
            "Foo",
            "--optimize",
            "--out-dir",
            out_dir_str.as_str(),
            fixture_path_str,
        ],
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", "Foo"),
            ("FAKE_SOLC_EXPECT_OPTIMIZE", "true"),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
}

#[cfg(unix)]
#[test]
fn test_cli_build_opt_level_enables_solc_optimizer_for_yul() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--contract",
            "Foo",
            "--opt-level",
            "2",
            "--out-dir",
            out_dir_str.as_str(),
            fixture_path_str,
        ],
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", "Foo"),
            ("FAKE_SOLC_EXPECT_OPTIMIZE", "true"),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
}

#[cfg(unix)]
#[test]
fn test_cli_build_opt_level_0_disables_solc_optimizer_for_yul() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--contract",
            "Foo",
            "--opt-level",
            "0",
            "--out-dir",
            out_dir_str.as_str(),
            fixture_path_str,
        ],
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", "Foo"),
            ("FAKE_SOLC_EXPECT_OPTIMIZE", "false"),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
}

#[cfg(unix)]
#[test]
fn test_cli_build_workspace_dependency_in_scope_file_path_fake_solc_artifacts() {
    let root = workspace_fixture("dependency_scope");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_in_dir_with_env(
        &[
            "build",
            "--backend",
            "yul",
            "--contract",
            "Foo",
            "--out-dir",
            out_dir_str.as_str(),
            "ingots/app/src/lib.fe",
        ],
        &root,
        &[
            ("FE_SOLC_PATH", fake_solc.to_str().expect("fake solc utf8")),
            ("FAKE_SOLC_CONTRACT", "Foo"),
        ],
    );
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let deploy_path = out_dir.join("Foo.bin");
    let runtime_path = out_dir.join("Foo.runtime.bin");
    let deploy = fs::read_to_string(&deploy_path).expect("read deploy bytecode");
    let runtime = fs::read_to_string(&runtime_path).expect("read runtime bytecode");

    let mut snapshot = output.replace(&out_dir_str, "<out>");
    snapshot.push_str("\n\n=== ARTIFACTS ===\n");
    snapshot.push_str(&format!("Foo.bin: {}\n", deploy.trim()));
    snapshot.push_str(&format!("Foo.runtime.bin: {}\n", runtime.trim()));

    let snapshot_path = root.join("build_dependency_in_scope_file_path_fake_solc.case");
    snap_test!(snapshot, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_inter_workspace_requires_member_selection() {
    let root = workspace_fixture("inter_workspace_requires_selection");
    let workspace_a = root.join("workspace_a");
    let snapshot_path = root.join("requires_selection.case");
    let (output, _) = run_fe_main_in_dir(&["check", "ingots/app"], &workspace_a);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_inter_workspace_member_selected_by_name() {
    let root = workspace_fixture("inter_workspace_select_by_name");
    let workspace_a = root.join("workspace_a");
    let snapshot_path = root.join("selected_by_name.case");
    let (output, _) = run_fe_main_in_dir(&["check", "ingots/app"], &workspace_a);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_inter_workspace_member_selected_with_alias_ok() {
    let root = workspace_fixture("inter_workspace_select_alias_ok");
    let workspace_a = root.join("workspace_a");
    let snapshot_path = root.join("alias_ok.case");
    let (output, _) = run_fe_main_in_dir(&["check", "ingots/app"], &workspace_a);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_inter_workspace_member_selected_with_alias_mismatch() {
    let root = workspace_fixture("inter_workspace_select_alias_mismatch");
    let workspace_a = root.join("workspace_a");
    let snapshot_path = root.join("alias_mismatch.case");
    let (output, _) = run_fe_main_in_dir(&["check", "ingots/app"], &workspace_a);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_inter_workspace_member_selected_with_version_mismatch() {
    let root = workspace_fixture("inter_workspace_select_version_mismatch");
    let workspace_a = root.join("workspace_a");
    let snapshot_path = root.join("version_mismatch.case");
    let (output, _) = run_fe_main_in_dir(&["check", "ingots/app"], &workspace_a);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_inter_workspace_member_selected_name_not_found() {
    let root = workspace_fixture("inter_workspace_select_name_not_found");
    let workspace_a = root.join("workspace_a");
    let snapshot_path = root.join("name_not_found.case");
    let (output, _) = run_fe_main_in_dir(&["check", "ingots/app"], &workspace_a);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_duplicate_member_name_is_rejected() {
    let root = workspace_fixture("duplicate_member_name");
    let snapshot_path = root.join("duplicate_member_name.case");
    let (output, _) = run_fe_main_in_dir(&["check"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_explicit_workspace_root_path() {
    let root = explicit_path_fixture("workspace");
    let snapshot_path = explicit_path_fixture("workspace_root.case");
    let (output, _) = run_fe_main_in_dir(&["check", root.to_str().unwrap()], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_explicit_ingot_root_path() {
    let root = explicit_path_fixture("ingot_only");
    let snapshot_path = explicit_path_fixture("ingot_root.case");
    let (output, _) = run_fe_main_in_dir(&["check", root.to_str().unwrap()], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_explicit_fe_toml_path_is_rejected() {
    let root = explicit_path_fixture("ingot_only");
    let fe_toml = root.join("fe.toml");
    let snapshot_path = explicit_path_fixture("fe_toml_path.case");
    let (output, _) = run_fe_main_in_dir(&["check", fe_toml.to_str().unwrap()], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_explicit_standalone_fe_file_path() {
    let root = explicit_path_fixture("standalone");
    let file_path = root.join("standalone.fe");
    let snapshot_path = explicit_path_fixture("standalone_file.case");
    let (output, _) = run_fe_main_in_dir(&["check", file_path.to_str().unwrap()], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_tree_workspace_member_by_name() {
    let root = workspace_fixture("member_resolution");
    let snapshot_path = root.join("tree_by_name.case");
    let (output, _) = run_fe_main_in_dir(&["tree", "app"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_tree_workspace_dependency_import() {
    let root = workspace_fixture("workspace_dependency_import");
    let snapshot_path = root.join("workspace_dependency_import.case");
    let (output, _) = run_fe_main_in_dir(&["tree"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_dependency_version_mismatch() {
    let root = workspace_fixture("workspace_dependency_version_mismatch");
    let snapshot_path = root.join("workspace_dependency_version_mismatch.case");
    let (output, _) = run_fe_main_in_dir(&["check"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_dependency_alias_conflict() {
    let root = workspace_fixture("workspace_dependency_alias_conflict");
    let snapshot_path = root.join("workspace_dependency_alias_conflict.case");
    let (output, _) = run_fe_main_in_dir(&["check"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_default_members_skips_non_default() {
    let root = workspace_fixture("default_members_skip_dev");
    let snapshot_path = root.join("default_members_skip_dev.case");
    let (output, _) = run_fe_main_in_dir(&["check"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_workspace_exclude_skips_member() {
    let root = workspace_fixture("exclude_patterns_skip_member");
    let snapshot_path = root.join("exclude_patterns_skip_member.case");
    let (output, _) = run_fe_main_in_dir(&["check"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
}
