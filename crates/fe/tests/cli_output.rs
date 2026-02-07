use dir_test::{Fixture, dir_test};
use std::{io::IsTerminal, path::Path, process::Command};
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
    run_fe_main_impl(args, None)
}

fn run_fe_main_in_dir(args: &[&str], cwd: &Path) -> (String, i32) {
    run_fe_main_impl(args, Some(cwd))
}

fn run_fe_main_impl(args: &[&str], cwd: Option<&Path>) -> (String, i32) {
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

    let mut cmd = Command::new(&fe_binary);
    cmd.args(args).env("NO_COLOR", "1");
    if let Some(dir) = cwd {
        cmd.current_dir(dir);
    }
    let output = cmd
        .output()
        .unwrap_or_else(|_| panic!("Failed to run fe {:?}", args));

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
    (normalized_output, exit_code)
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

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test",
    glob: "*.fe",
)]
fn test_fe_test_yul(fixture: Fixture<&str>) {
    let (output, exit_code) =
        run_fe_command_with_args("test", fixture.path(), &["--backend", "yul"]);
    assert_eq!(
        exit_code,
        0,
        "fe test (yul) failed for {path}:\n{output}\n\nTo reproduce:\n  cargo run --bin fe -- test --backend yul --report {path}",
        path = fixture.path(),
        output = output
    );
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test",
    glob: "*.fe",
)]
fn test_fe_test_sonatina(fixture: Fixture<&str>) {
    let (output, exit_code) =
        run_fe_command_with_args("test", fixture.path(), &["--backend", "sonatina"]);
    assert_eq!(
        exit_code,
        0,
        "fe test (sonatina) failed for {path}:\n{output}\n\nTo reproduce:\n  cargo run --bin fe -- test --backend sonatina --report {path}",
        path = fixture.path(),
        output = output
    );
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
