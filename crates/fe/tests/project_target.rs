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

/// Compiles only for the native target, so it tells the targets apart.
const NATIVE_ONLY: &str = r#"
#[cfg(target = "native")]
fn answer() -> i32 { 0 }

pub fn main() -> i32 { answer() }

#[test]
fn answers() { assert!(answer() == 0) }
"#;

const EVM_ONLY: &str = r#"
#[cfg(target = "evm")]
fn answer() -> u256 { 0 }

pub fn called() -> u256 { answer() }

#[test]
fn answers() { assert!(answer() == 0) }
"#;

#[cfg(feature = "wasm")]
const WASM_ONLY: &str = r#"
#[cfg(target = "wasm")]
fn answer() -> i32 { 0 }

pub fn called() -> i32 { answer() }

#[test]
fn answers() { assert!(answer() == 0) }
"#;

fn write_ingot(dir: &Path, name: &str, config: &str, source: &str) {
    fs::create_dir_all(dir.join("src")).unwrap();
    fs::write(
        dir.join("fe.toml"),
        format!("[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n{config}"),
    )
    .unwrap();
    fs::write(dir.join("src/lib.fe"), source).unwrap();
}

fn write_workspace(dir: &Path, config: &str) {
    fs::create_dir_all(dir).unwrap();
    fs::write(
        dir.join("fe.toml"),
        format!("[workspace]\nname = \"root\"\nversion = \"0.1.0\"\n{config}"),
    )
    .unwrap();
}

fn fe(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(args)
        .current_dir(dir)
        .output()
        .unwrap()
}

fn assert_ok(output: &Output) {
    assert!(output.status.success(), "{output:?}");
}

fn assert_err(output: &Output, message: &str) {
    assert!(!output.status.success(), "{output:?}");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(text.contains(message), "{text}");
}

#[test]
fn commands_use_the_ingots_target() {
    let temp = tempdir().unwrap();
    write_ingot(temp.path(), "tool", "target = \"native\"\n", NATIVE_ONLY);

    assert_ok(&fe(temp.path(), &["check"]));
    assert_ok(&fe(temp.path(), &["test"]));
    assert_ok(&fe(temp.path(), &["build"]));
    let result = Command::new(temp.path().join("out/tool")).output().unwrap();
    assert!(result.status.success(), "{result:?}");
}

#[cfg(feature = "wasm")]
#[test]
fn commands_use_a_wasm_target() {
    let temp = tempdir().unwrap();
    write_ingot(temp.path(), "module", "target = \"wasm\"\n", WASM_ONLY);

    assert_ok(&fe(temp.path(), &["check"]));
    assert_ok(&fe(temp.path(), &["test"]));
    assert_ok(&fe(temp.path(), &["build"]));
    let module = fs::read(temp.path().join("out/module.wasm")).unwrap();
    assert_eq!(&module[..4], b"\0asm");
    assert!(temp.path().join("out/module.manifest.json").is_file());
}

#[test]
fn backend_flag_overrides_the_target() {
    let temp = tempdir().unwrap();
    write_ingot(temp.path(), "tool", "target = \"native\"\n", NATIVE_ONLY);
    assert_err(
        &fe(temp.path(), &["check", "--backend", "sonatina"]),
        "answer",
    );

    write_ingot(temp.path(), "tool", "target = \"evm\"\n", NATIVE_ONLY);
    assert_err(&fe(temp.path(), &["check"]), "answer");
    assert_ok(&fe(temp.path(), &["check", "--backend", "native"]));
    assert_ok(&fe(temp.path(), &["test", "--backend", "native"]));
}

#[test]
fn source_files_use_their_ingots_target() {
    let temp = tempdir().unwrap();
    write_ingot(temp.path(), "tool", "target = \"native\"\n", NATIVE_ONLY);
    assert_ok(&fe(temp.path(), &["check", "src/lib.fe"]));
    // A standalone file has no project.
    assert_err(
        &fe(temp.path(), &["check", "--standalone", "src/lib.fe"]),
        "answer",
    );
}

#[test]
fn workspace_members_inherit_the_target() {
    let temp = tempdir().unwrap();
    write_workspace(temp.path(), "target = \"native\"\nmembers = [\"tool\"]\n");
    write_ingot(&temp.path().join("tool"), "tool", "", NATIVE_ONLY);

    assert_ok(&fe(temp.path(), &["check"]));
    assert_ok(&fe(temp.path(), &["check", "tool"]));
    assert_ok(&fe(&temp.path().join("tool"), &["check"]));
    assert_ok(&fe(temp.path(), &["test"]));
}

#[test]
fn an_ingot_only_inherits_from_its_own_workspace() {
    let temp = tempdir().unwrap();
    write_workspace(temp.path(), "target = \"native\"\nmembers = [\"tool\"]\n");
    write_ingot(&temp.path().join("tool"), "tool", "", NATIVE_ONLY);
    write_ingot(&temp.path().join("other"), "other", "", EVM_ONLY);
    assert_ok(&fe(&temp.path().join("other"), &["check"]));
}

#[test]
fn workspace_members_must_agree_on_the_target() {
    let temp = tempdir().unwrap();
    write_workspace(temp.path(), "members = [\"tool\", \"contract\"]\n");
    write_ingot(
        &temp.path().join("tool"),
        "tool",
        "target = \"native\"\n",
        NATIVE_ONLY,
    );
    write_ingot(
        &temp.path().join("contract"),
        "contract",
        "target = \"evm\"\n",
        EVM_ONLY,
    );

    for command in ["check", "build", "test"] {
        let output = fe(temp.path(), &[command]);
        assert_err(&output, "workspace members target different machines");
        assert_err(&output, "`tool` targets `native`");
        assert_err(&output, "`contract` targets `evm`");
    }
    assert_ok(&fe(temp.path(), &["check", "--ingot", "tool"]));
    assert_ok(&fe(temp.path(), &["check", "--ingot", "contract"]));
    assert_ok(&fe(temp.path(), &["build", "--ingot", "tool"]));
    assert_ok(&fe(temp.path(), &["test", "--ingot", "tool"]));
}

#[test]
fn a_member_without_a_target_takes_the_selected_target() {
    let temp = tempdir().unwrap();
    write_workspace(temp.path(), "members = [\"tool\", \"lib\"]\n");
    write_ingot(
        &temp.path().join("tool"),
        "tool",
        "target = \"native\"\n",
        NATIVE_ONLY,
    );
    write_ingot(
        &temp.path().join("lib"),
        "lib",
        "",
        "pub fn answer() -> i32 { 0 }\n",
    );
    assert_ok(&fe(temp.path(), &["check"]));
    assert_ok(&fe(temp.path(), &["test"]));
}

#[test]
fn test_paths_must_agree_on_the_target() {
    let temp = tempdir().unwrap();
    write_ingot(
        &temp.path().join("tool"),
        "tool",
        "target = \"native\"\n",
        NATIVE_ONLY,
    );
    write_ingot(
        &temp.path().join("contract"),
        "contract",
        "target = \"evm\"\n",
        EVM_ONLY,
    );
    assert_err(
        &fe(temp.path(), &["test", "tool", "contract"]),
        "test paths target both `native` and `evm`",
    );
}

#[test]
fn dependencies_must_match_the_commands_target() {
    let temp = tempdir().unwrap();
    let dependency = "[dependencies]\nlib = { path = \"../lib\" }\n";
    let tool = temp.path().join("tool");
    write_ingot(
        &temp.path().join("lib"),
        "lib",
        "target = \"evm\"\n",
        EVM_ONLY,
    );
    write_ingot(
        &tool,
        "tool",
        &format!("target = \"native\"\n{dependency}"),
        NATIVE_ONLY,
    );
    let mismatch = "but this command compiles for `native`";
    for command in ["check", "build", "test"] {
        assert_err(&fe(&tool, &[command]), mismatch);
    }

    // The command's target applies to an ingot that sets none.
    write_ingot(&tool, "tool", dependency, "pub fn answer() -> i32 { 0 }\n");
    assert_ok(&fe(&tool, &["check"]));
    assert_err(&fe(&tool, &["check", "--backend", "native"]), mismatch);

    // An ingot without a target compiles for the command's.
    write_ingot(
        &temp.path().join("lib"),
        "lib",
        "",
        "pub fn answer() -> i32 { 0 }\n",
    );
    assert_ok(&fe(&tool, &["check", "--backend", "native"]));
}

#[test]
fn only_compiled_ingots_must_match_the_commands_target() {
    let temp = tempdir().unwrap();
    write_workspace(temp.path(), "members = [\"tool\", \"other\", \"lib\"]\n");
    write_ingot(
        &temp.path().join("tool"),
        "tool",
        "target = \"native\"\n",
        NATIVE_ONLY,
    );
    write_ingot(
        &temp.path().join("other"),
        "other",
        "[dependencies]\nlib = { path = \"../lib\" }\n",
        "pub fn answer() -> u256 { 0 }\n",
    );
    write_ingot(
        &temp.path().join("lib"),
        "lib",
        "target = \"evm\"\n",
        EVM_ONLY,
    );
    assert_ok(&fe(temp.path(), &["check", "--ingot", "tool"]));
    assert_ok(&fe(temp.path(), &["test", "--ingot", "tool"]));
}

#[test]
fn printing_the_tree_ignores_targets() {
    let temp = tempdir().unwrap();
    write_ingot(
        &temp.path().join("lib"),
        "lib",
        "target = \"native\"\n",
        NATIVE_ONLY,
    );
    let tool = temp.path().join("tool");
    write_ingot(
        &tool,
        "tool",
        "[dependencies]\nlib = { path = \"../lib\" }\n",
        "pub fn answer() -> i32 { 0 }\n",
    );
    assert_ok(&fe(&tool, &["tree"]));
}

#[test]
fn workspaces_resolve_from_outside_their_root() {
    let temp = tempdir().unwrap();
    let root = temp.path().join("project");
    write_workspace(&root, "members = [\"tool\", \"contract\"]\n");
    write_ingot(
        &root.join("tool"),
        "tool",
        "target = \"native\"\n",
        NATIVE_ONLY,
    );
    write_ingot(
        &root.join("contract"),
        "contract",
        "target = \"evm\"\n",
        EVM_ONLY,
    );
    assert_ok(&fe(temp.path(), &["check", "project", "--ingot", "tool"]));
    assert_err(
        &fe(temp.path(), &["check", "project"]),
        "workspace members target different machines",
    );
}

#[test]
fn test_globs_use_the_targets_of_their_matches() {
    let temp = tempdir().unwrap();
    write_ingot(
        &temp.path().join("projects/tool"),
        "tool",
        "target = \"native\"\n",
        NATIVE_ONLY,
    );
    assert_ok(&fe(temp.path(), &["test", "projects/*"]));

    write_ingot(
        &temp.path().join("projects/contract"),
        "contract",
        "target = \"evm\"\n",
        EVM_ONLY,
    );
    assert_err(
        &fe(temp.path(), &["test", "projects/*"]),
        "test paths target both",
    );
}

#[test]
fn an_invalid_target_is_reported() {
    let temp = tempdir().unwrap();
    write_ingot(temp.path(), "tool", "target = \"mips\"\n", EVM_ONLY);
    assert_err(&fe(temp.path(), &["check"]), "Invalid target \"mips\"");
}
