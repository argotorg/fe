use std::{path::PathBuf, process::Command, sync::OnceLock};

use tempfile::TempDir;

fn fe_binary() -> &'static PathBuf {
    static BIN: OnceLock<PathBuf> = OnceLock::new();
    BIN.get_or_init(|| {
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
            .join("fe")
    })
}

fn run_fe(args: &[&str], cwd: &std::path::Path) -> (String, i32) {
    let output = Command::new(fe_binary())
        .args(args)
        .env("NO_COLOR", "1")
        .current_dir(cwd)
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
    (full_output, exit_code)
}

#[test]
fn new_creates_ingot_layout() {
    let tmp = TempDir::new().expect("tempdir");
    let ingot_dir = tmp.path().join("my_ingot");

    let (output, exit_code) = run_fe(&["new", ingot_dir.to_str().unwrap()], tmp.path());
    assert_eq!(exit_code, 0, "fe new failed:\n{output}");

    let fe_toml = ingot_dir.join("fe.toml");
    assert!(fe_toml.is_file(), "missing fe.toml");
    let config = std::fs::read_to_string(&fe_toml).expect("read fe.toml");
    assert!(config.contains("[ingot]"));
    assert!(config.contains("name = \"my_ingot\""));
    assert!(config.contains("version = \"0.1.0\""));

    assert!(ingot_dir.join("src").is_dir(), "missing src/");
    assert!(ingot_dir.join("src/lib.fe").is_file(), "missing src/lib.fe");
}

#[test]
fn new_allows_overriding_name_and_version() {
    let tmp = TempDir::new().expect("tempdir");
    let ingot_dir = tmp.path().join("some_dir");

    let (output, exit_code) = run_fe(
        &[
            "new",
            "--name",
            "custom_name",
            "--version",
            "9.9.9",
            ingot_dir.to_str().unwrap(),
        ],
        tmp.path(),
    );
    assert_eq!(exit_code, 0, "fe new failed:\n{output}");

    let fe_toml = ingot_dir.join("fe.toml");
    let config = std::fs::read_to_string(&fe_toml).expect("read fe.toml");
    assert!(config.contains("name = \"custom_name\""));
    assert!(config.contains("version = \"9.9.9\""));
}

#[test]
fn new_workspace_creates_workspace_config_without_hardcoded_layout() {
    let tmp = TempDir::new().expect("tempdir");
    let ws_dir = tmp.path().join("my_ws");

    let (output, exit_code) = run_fe(
        &["new", "--workspace", ws_dir.to_str().unwrap()],
        tmp.path(),
    );
    assert_eq!(exit_code, 0, "fe new --workspace failed:\n{output}");

    let fe_toml = ws_dir.join("fe.toml");
    assert!(fe_toml.is_file(), "missing workspace fe.toml");
    let config = std::fs::read_to_string(&fe_toml).expect("read workspace fe.toml");
    assert!(config.contains("[workspace]"));
    assert!(config.contains("name = \"my_ws\""));
    assert!(config.contains("members = []"));

    assert!(
        !ws_dir.join("ingots").exists(),
        "workspace new should not create an ingots/ directory"
    );
}

#[test]
fn new_suggests_member_for_enclosing_workspace() {
    let tmp = TempDir::new().expect("tempdir");
    let ws_dir = tmp.path().join("ws");
    std::fs::create_dir_all(&ws_dir).expect("create ws");
    std::fs::write(
        ws_dir.join("fe.toml"),
        r#"[workspace]
name = "ws"
version = "0.1.0"
members = []
exclude = ["target"]
"#,
    )
    .expect("write fe.toml");

    let member_dir = ws_dir.join("app");
    let (output, exit_code) = run_fe(&["new", member_dir.to_str().unwrap()], tmp.path());
    assert_eq!(exit_code, 0, "fe new failed:\n{output}");
    assert!(
        output.contains("add \"app\" to [workspace].members"),
        "expected `fe new` to print a workspace member suggestion, got:\n{output}"
    );

    let updated = std::fs::read_to_string(ws_dir.join("fe.toml")).expect("read fe.toml");
    let value: toml::Value = updated.parse().expect("parse workspace fe.toml");
    let workspace = value
        .get("workspace")
        .and_then(|v| v.as_table())
        .expect("workspace table");
    let members = workspace
        .get("members")
        .and_then(|v| v.as_array())
        .expect("members array");
    assert!(
        members.is_empty(),
        "expected members to remain empty (no file writes), got: {members:?}"
    );
}

#[test]
fn new_suggests_member_for_root_level_workspace_config() {
    let tmp = TempDir::new().expect("tempdir");
    let ws_dir = tmp.path().join("ws");
    std::fs::create_dir_all(&ws_dir).expect("create ws");
    std::fs::write(
        ws_dir.join("fe.toml"),
        r#"name = "ws"
version = "0.1.0"
members = []
exclude = ["target"]
"#,
    )
    .expect("write fe.toml");

    let member_dir = ws_dir.join("app");
    let (output, exit_code) = run_fe(&["new", member_dir.to_str().unwrap()], tmp.path());
    assert_eq!(exit_code, 0, "fe new failed:\n{output}");
    assert!(
        output.contains("add \"app\" to members"),
        "expected `fe new` to suggest adding the member to root-level members, got:\n{output}"
    );

    let updated = std::fs::read_to_string(ws_dir.join("fe.toml")).expect("read fe.toml");
    let value: toml::Value = updated.parse().expect("parse workspace fe.toml");
    let members = value
        .get("members")
        .and_then(|v| v.as_array())
        .expect("members array");
    assert!(
        members.is_empty(),
        "expected members to remain empty (no file writes), got: {members:?}"
    );
}

#[test]
fn new_does_not_suggest_member_when_covered_by_existing_glob() {
    let tmp = TempDir::new().expect("tempdir");
    let ws_dir = tmp.path().join("ws");
    std::fs::create_dir_all(&ws_dir).expect("create ws");
    std::fs::write(
        ws_dir.join("fe.toml"),
        r#"[workspace]
name = "ws"
version = "0.1.0"
members = ["ingots/*"]
exclude = ["target"]
"#,
    )
    .expect("write fe.toml");

    let member_dir = ws_dir.join("ingots").join("app");
    let (output, exit_code) = run_fe(&["new", member_dir.to_str().unwrap()], tmp.path());
    assert_eq!(exit_code, 0, "fe new failed:\n{output}");
    assert!(
        !output.contains("Workspace detected at"),
        "expected `fe new` to skip workspace suggestion when member is covered by glob, got:\n{output}"
    );

    let updated = std::fs::read_to_string(ws_dir.join("fe.toml")).expect("read fe.toml");
    let value: toml::Value = updated.parse().expect("parse workspace fe.toml");
    let workspace = value
        .get("workspace")
        .and_then(|v| v.as_table())
        .expect("workspace table");
    let members = workspace
        .get("members")
        .and_then(|v| v.as_array())
        .expect("members array");
    assert!(
        members.iter().any(|m| m.as_str() == Some("ingots/*")),
        "expected members to retain glob entry, got: {members:?}"
    );
    assert!(
        !members.iter().any(|m| m.as_str() == Some("ingots/app")),
        "expected members not to contain explicit \"ingots/app\", got: {members:?}"
    );
}

#[test]
fn new_suggests_member_for_members_main_table() {
    let tmp = TempDir::new().expect("tempdir");
    let ws_dir = tmp.path().join("ws");
    std::fs::create_dir_all(&ws_dir).expect("create ws");
    std::fs::write(
        ws_dir.join("fe.toml"),
        r#"[workspace]
name = "ws"
version = "0.1.0"
members = { main = [], dev = ["examples/*"] }
exclude = ["target"]
"#,
    )
    .expect("write fe.toml");

    let member_dir = ws_dir.join("app");
    let (output, exit_code) = run_fe(&["new", member_dir.to_str().unwrap()], tmp.path());
    assert_eq!(exit_code, 0, "fe new failed:\n{output}");
    assert!(
        output.contains("add \"app\" to [workspace].members.main"),
        "expected `fe new` to print a workspace member suggestion for members.main, got:\n{output}"
    );

    let updated = std::fs::read_to_string(ws_dir.join("fe.toml")).expect("read fe.toml");
    let value: toml::Value = updated.parse().expect("parse workspace fe.toml");
    let workspace = value
        .get("workspace")
        .and_then(|v| v.as_table())
        .expect("workspace table");
    let members = workspace.get("members").expect("members value");
    let member_table = members.as_table().expect("members table");
    let main = member_table
        .get("main")
        .and_then(|v| v.as_array())
        .expect("members.main array");
    assert!(
        main.is_empty(),
        "expected members.main to remain empty (no file writes), got: {main:?}"
    );
}

#[test]
fn new_errors_when_target_path_is_file() {
    let tmp = TempDir::new().expect("tempdir");
    let target_file = tmp.path().join("not_a_dir");
    std::fs::write(&target_file, "hello").expect("write file");

    let (output, exit_code) = run_fe(&["new", target_file.to_str().unwrap()], tmp.path());
    assert_ne!(exit_code, 0, "expected `fe new` to fail, got:\n{output}");
    assert!(
        output.contains("exists and is a file; expected directory"),
        "expected file-target error, got:\n{output}"
    );
}

#[test]
fn new_refuses_to_overwrite_existing_fe_toml() {
    let tmp = TempDir::new().expect("tempdir");
    let ingot_dir = tmp.path().join("my_ingot");
    std::fs::create_dir_all(&ingot_dir).expect("create ingot dir");
    std::fs::write(
        ingot_dir.join("fe.toml"),
        "[ingot]\nname = \"x\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");

    let (output, exit_code) = run_fe(&["new", ingot_dir.to_str().unwrap()], tmp.path());
    assert_ne!(exit_code, 0, "expected `fe new` to fail, got:\n{output}");
    assert!(
        output.contains("Refusing to overwrite existing") && output.contains("fe.toml"),
        "expected overwrite refusal for fe.toml, got:\n{output}"
    );
}

#[test]
fn new_refuses_to_overwrite_existing_src_lib_fe() {
    let tmp = TempDir::new().expect("tempdir");
    let ingot_dir = tmp.path().join("my_ingot");
    std::fs::create_dir_all(ingot_dir.join("src")).expect("create src dir");
    std::fs::write(ingot_dir.join("src/lib.fe"), "pub fn main() {}\n").expect("write lib.fe");

    let (output, exit_code) = run_fe(&["new", ingot_dir.to_str().unwrap()], tmp.path());
    assert_ne!(exit_code, 0, "expected `fe new` to fail, got:\n{output}");
    assert!(
        output.contains("Refusing to overwrite existing") && output.contains("src/lib.fe"),
        "expected overwrite refusal for src/lib.fe, got:\n{output}"
    );
}

#[test]
fn new_does_not_print_workspace_suggestion_outside_workspace() {
    let tmp = TempDir::new().expect("tempdir");
    let ingot_dir = tmp.path().join("my_ingot");

    let (output, exit_code) = run_fe(&["new", ingot_dir.to_str().unwrap()], tmp.path());
    assert_eq!(exit_code, 0, "fe new failed:\n{output}");
    assert!(
        !output.contains("Workspace detected at"),
        "expected no workspace suggestion outside a workspace, got:\n{output}"
    );
}

#[test]
fn new_does_not_suggest_member_when_already_explicitly_listed() {
    let tmp = TempDir::new().expect("tempdir");
    let ws_dir = tmp.path().join("ws");
    std::fs::create_dir_all(&ws_dir).expect("create ws");
    std::fs::write(
        ws_dir.join("fe.toml"),
        r#"[workspace]
name = "ws"
version = "0.1.0"
members = ["app"]
exclude = ["target"]
"#,
    )
    .expect("write fe.toml");

    let member_dir = ws_dir.join("app");
    let (output, exit_code) = run_fe(&["new", member_dir.to_str().unwrap()], tmp.path());
    assert_eq!(exit_code, 0, "fe new failed:\n{output}");
    assert!(
        !output.contains("Workspace detected at"),
        "expected `fe new` to print no suggestion when member is already listed, got:\n{output}"
    );
}

#[test]
fn new_suggests_member_for_nested_path_when_parent_dirs_do_not_exist() {
    let tmp = TempDir::new().expect("tempdir");
    let ws_dir = tmp.path().join("ws");
    std::fs::create_dir_all(&ws_dir).expect("create ws");
    std::fs::write(
        ws_dir.join("fe.toml"),
        r#"[workspace]
name = "ws"
version = "0.1.0"
members = []
exclude = ["target"]
"#,
    )
    .expect("write fe.toml");

    let member_dir = ws_dir.join("packages").join("app");
    let (output, exit_code) = run_fe(&["new", member_dir.to_str().unwrap()], tmp.path());
    assert_eq!(exit_code, 0, "fe new failed:\n{output}");
    assert!(
        output.contains("add \"packages/app\" to [workspace].members"),
        "expected `fe new` to suggest the nested member path, got:\n{output}"
    );
    assert!(
        member_dir.join("fe.toml").is_file(),
        "expected ingot fe.toml to be created"
    );
    assert!(
        member_dir.join("src/lib.fe").is_file(),
        "expected ingot src/lib.fe to be created"
    );
}

#[test]
fn new_warns_when_workspace_members_field_is_invalid_type() {
    let tmp = TempDir::new().expect("tempdir");
    let ws_dir = tmp.path().join("ws");
    std::fs::create_dir_all(&ws_dir).expect("create ws");
    std::fs::write(
        ws_dir.join("fe.toml"),
        r#"[workspace]
name = "ws"
version = "0.1.0"
members = "oops"
exclude = ["target"]
"#,
    )
    .expect("write fe.toml");

    let member_dir = ws_dir.join("app");
    let (output, exit_code) = run_fe(&["new", member_dir.to_str().unwrap()], tmp.path());
    assert_eq!(exit_code, 0, "fe new failed:\n{output}");
    assert!(
        output.contains("failed to check workspace members"),
        "expected warning when members is invalid type, got:\n{output}"
    );
    assert!(
        member_dir.join("fe.toml").is_file(),
        "expected ingot fe.toml to be created"
    );
}
