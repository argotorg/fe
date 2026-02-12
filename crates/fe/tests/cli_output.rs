use dir_test::{Fixture, dir_test};
use std::{fs, path::Path, process::Command};
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

fn assert_no_icons(output: &str) {
    let icon = output.chars().find(|c| {
        matches!(
            *c,
            '\u{2600}'..='\u{27BF}' | '\u{1F300}'..='\u{1FAFF}'
        )
    });
    if let Some(icon) = icon {
        panic!("CLI output contained an icon/emoji character {icon:?}.\n{output}");
    }
}

// Helper function to run fe check
fn run_fe_check(path: &str) -> (String, i32) {
    run_fe_command("check", path)
}

// Helper function to run fe tree
fn run_fe_tree(path: &str) -> (String, i32) {
    run_fe_command("tree", path)
}

/// Helper to run `fe test` on a fixture path.
///
/// * `path` - File or directory path passed to the CLI.
///
/// Returns the combined CLI output and exit code.
fn run_fe_test(path: &str) -> (String, i32) {
    run_fe_command("test", path)
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
    run_fe_main_with_env(args, &[])
}

fn run_fe_main_with_env(args: &[&str], extra_env: &[(&str, &str)]) -> (String, i32) {
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

    let fe_binary = std::env::current_exe()
        .expect("Failed to get current exe")
        .parent()
        .expect("Failed to get parent")
        .parent()
        .expect("Failed to get parent")
        .join("fe");

    let mut command = Command::new(&fe_binary);
    command.args(args).env("NO_COLOR", "1"); // Disable color output for consistent snapshots across environments
    for (key, value) in extra_env {
        command.env(key, value);
    }
    let output = command
        .output()
        .unwrap_or_else(|_| panic!("Failed to run fe {:?}", args));

    // Combine stdout and stderr for snapshot
    let mut full_output = String::new();
    if !output.stdout.is_empty() {
        full_output.push_str("=== STDOUT ===\n");
        full_output.push_str(&String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        if !full_output.is_empty() {
            full_output.push('\n');
        }
        full_output.push_str("=== STDERR ===\n");
        full_output.push_str(&String::from_utf8_lossy(&output.stderr));
    }
    let exit_code = output.status.code().unwrap_or(-1);
    full_output.push_str(&format!("\n=== EXIT CODE: {exit_code} ==="));

    // Normalize paths for portability
    let normalized_output = normalize_output(&full_output);
    assert_no_icons(&normalized_output);
    (normalized_output, exit_code)
}

fn run_fe_main_in_dir(args: &[&str], cwd: &Path) -> (String, i32) {
    run_fe_main_in_dir_with_env(args, cwd, &[])
}

fn run_fe_main_in_dir_with_env(
    args: &[&str],
    cwd: &Path,
    extra_env: &[(&str, &str)],
) -> (String, i32) {
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

    let fe_binary = std::env::current_exe()
        .expect("Failed to get current exe")
        .parent()
        .expect("Failed to get parent")
        .parent()
        .expect("Failed to get parent")
        .join("fe");

    let mut command = Command::new(&fe_binary);
    command
        .args(args)
        .env("NO_COLOR", "1") // Disable color output for consistent snapshots across environments
        .current_dir(cwd);
    for (key, value) in extra_env {
        command.env(key, value);
    }
    let output = command
        .output()
        .unwrap_or_else(|_| panic!("Failed to run fe {:?} in {:?}", args, cwd));

    let mut full_output = String::new();
    if !output.stdout.is_empty() {
        full_output.push_str("=== STDOUT ===\n");
        full_output.push_str(&String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        if !full_output.is_empty() {
            full_output.push('\n');
        }
        full_output.push_str("=== STDERR ===\n");
        full_output.push_str(&String::from_utf8_lossy(&output.stderr));
    }
    let exit_code = output.status.code().unwrap_or(-1);
    full_output.push_str(&format!("\n=== EXIT CODE: {exit_code} ==="));

    let normalized_output = normalize_output(&full_output);
    assert_no_icons(&normalized_output);
    (normalized_output, exit_code)
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
        &["build", "--out-dir", out_dir_str.as_str(), fixture_path_str],
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
    let ingot_dir = std::path::Path::new(fixture.path())
        .parent()
        .expect("fe.toml should have parent");

    let (output, _) = run_fe_tree(ingot_dir.to_str().unwrap());

    // Use the ingot directory name for the snapshot with _tree suffix
    let ingot_name = ingot_dir.file_name().unwrap().to_str().unwrap();
    let snapshot_path = ingot_dir.join(format!("{}_tree", ingot_name));
    snap_test!(output, snapshot_path.to_str().unwrap());
}

/// Executes `fe test` against each fixture and asserts a zero exit code.
///
/// * `fixture` - Fixture containing the test source path.
///
/// Returns nothing; asserts on the CLI exit status.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test",
    glob: "*.fe",
)]
fn test_fe_test(fixture: Fixture<&str>) {
    let (output, exit_code) = run_fe_test(fixture.path());
    // All fe test fixtures should pass (exit code 0)
    assert_eq!(exit_code, 0, "fe test failed:\n{}", output);
}

/// Runs `fe test` and snapshots the output to verify behavior of passing/failing tests and logs.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test_runner",
    glob: "*.fe",
)]
fn test_fe_test_runner(fixture: Fixture<&str>) {
    let mut args = vec!["test"];
    if fixture.path().contains("logs.fe") {
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
fn test_tree_workspace_default_member_version() {
    let root = workspace_fixture("tree_default_member_version");
    let snapshot_path = root.join("tree_default_member_version.case");
    let (output, _) = run_fe_main_in_dir(&["tree"], &root);
    snap_test!(output, snapshot_path.to_str().unwrap());
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
fn test_cli_build_workspace_root_contract_not_found() {
    let root = workspace_fixture("build_workspace_root");
    let snapshot_path = root.join("build_workspace_root_contract_not_found.case");
    let (output, exit_code) = run_fe_main_in_dir(&["build", "--contract", "DoesNotExist"], &root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    snap_test!(output, snapshot_path.to_str().unwrap());
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
        &["build", "--out-dir", out_dir_str.as_str()],
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
fn test_cli_build_workspace_root_contract_filter_fake_solc_artifacts() {
    let root = workspace_fixture("build_workspace_root");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_in_dir_with_env(
        &[
            "build",
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
fn test_cli_build_workspace_dependency_in_scope_file_path_fake_solc_artifacts() {
    let root = workspace_fixture("dependency_scope");

    let temp = tempdir().expect("tempdir");
    let fake_solc = write_fake_solc(&temp);

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main_in_dir_with_env(
        &[
            "build",
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
