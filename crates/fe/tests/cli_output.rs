use dir_test::{Fixture, dir_test};
use serde_json::Value;
use std::{fs, io::IsTerminal, path::Path, process::Command};
use tempfile::tempdir;
use test_utils::{
    normalize::{normalize_newlines, normalize_path_separators, replace_path_token},
    snap_test,
};

// Helper function to normalize paths in output for portability
fn normalize_output(output: &str) -> String {
    let output = normalize_newlines(output);
    let output = normalize_path_separators(output.as_ref());

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let project_root = std::path::Path::new(manifest_dir)
        .parent()
        .expect("parent")
        .parent()
        .expect("parent");

    let normalized = replace_path_token(&output, project_root, "<project>");
    normalize_timing_output(&normalized)
}

fn normalize_timing_output(output: &str) -> String {
    let has_trailing_newline = output.ends_with('\n');
    let mut normalized = output
        .lines()
        .map(normalize_timing_line)
        .collect::<Vec<_>>()
        .join("\n");
    if has_trailing_newline {
        normalized.push('\n');
    }
    normalized
}

fn normalize_timing_line(line: &str) -> String {
    let Some(status_idx) = ["PASS  [", "FAIL  [", "READY [", "ERROR ["]
        .into_iter()
        .filter_map(|marker| line.find(marker))
        .min()
    else {
        return line.to_string();
    };
    let Some(open_rel) = line[status_idx..].find('[') else {
        return line.to_string();
    };
    let open = status_idx + open_rel;
    let Some(close_rel) = line[open..].find(']') else {
        return line.to_string();
    };
    let close = open + close_rel;
    let bracket = &line[open + 1..close];
    let Some(seconds) = bracket.strip_suffix('s') else {
        return line.to_string();
    };
    if seconds.is_empty()
        || !seconds
            .chars()
            .all(|ch| ch.is_ascii_digit() || ch == '.' || ch == ' ')
    {
        return line.to_string();
    }

    let mut normalized = String::new();
    normalized.push_str(&line[..open]);
    normalized.push_str("[<time>]");
    normalized.push_str(&line[close + 1..]);
    normalized
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

fn run_fe_main_in_dir(args: &[&str], cwd: &Path) -> (String, i32) {
    let out = run_fe_main_impl(args, Some(cwd), &[]);
    (out.combined(), out.exit_code)
}

#[test]
fn test_cli_check_invalid_named_const_used_in_type_position_reports_error_instead_of_panicking() {
    let temp = tempdir().expect("tempdir");
    let file = temp.path().join("invalid_const_ty_use.fe");
    fs::write(
        &file,
        r#"
const N: usize = nope()

fn f() {
    let _x: [u8; N] = [0; 1]
}
"#,
    )
    .expect("write fixture");

    let (output, exit_code) = run_fe_check(file.to_str().expect("fixture path utf8"));
    assert_eq!(exit_code, 1, "expected check failure:\n{output}");
    assert!(
        output.contains("undefined variable `nope`"),
        "expected undefined variable diagnostic instead of panic:\n{output}"
    );
    assert!(
        !output.contains("semantic lowering missing for call-like expression"),
        "unexpected semantic lowering panic:\n{output}"
    );
}

#[test]
fn test_cli_check_unresolved_record_init_path_reports_error_instead_of_panicking() {
    let temp = tempdir().expect("tempdir");
    let file = temp.path().join("unresolved_record_init_path.fe");
    fs::write(
        &file,
        r#"
fn trigger() {
    let s = missing::S {}
}
"#,
    )
    .expect("write fixture");

    let (output, exit_code) = run_fe_check(file.to_str().expect("fixture path utf8"));
    assert_eq!(exit_code, 1, "expected check failure:\n{output}");
    assert!(
        output.contains("`missing` is not found"),
        "expected unresolved path diagnostic instead of panic:\n{output}"
    );
    assert!(
        !output.contains("record init lowering missing"),
        "unexpected semantic lowering panic:\n{output}"
    );
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

fn fe_binary() -> &'static str {
    env!("CARGO_BIN_EXE_fe")
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

fn fe_test_runner_fixture_dir(name: &str) -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test_runner")
        .join(name)
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

#[test]
fn test_cli_build_sonatina_ir_respects_contract_filter() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/multi_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "ir",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_path_str,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let ir_path = out_dir.join("multi_contract.sona");
    let ir = fs::read_to_string(&ir_path).expect("read Sonatina IR");
    assert!(ir.contains("object @Foo"), "expected Foo object:\n{ir}");
    assert!(
        !ir.contains("object @Bar"),
        "contract filter should exclude Bar object:\n{ir}"
    );
}

#[test]
fn test_cli_build_nested_storage_map_effect_forwarding() {
    let temp = tempdir().expect("tempdir");
    let fixture = temp.path().join("nested_storage_map_effect_forwarding.fe");
    fs::write(
        &fixture,
        r#"
use std::evm::StorageMap

msg Msg {
    #[selector = 1]
    Check { key: u256, next: u256, initialized: bool },
}

fn is_set(_ key: u256) -> bool
    uses (map: StorageMap<u256, u256>)
{
    map.get(key: key) != 0
}

fn nested(_ key: u256) -> bool
    uses (map: StorageMap<u256, u256>)
{
    is_set(key)
}

struct Store {
    map: StorageMap<u256, u256>,
}

pub contract Test {
    mut store: Store

    recv Msg {
        Check { key, next, initialized } uses (store) {
            let mut cursor = key
            while cursor > next {
                assert!(!with (store.map) {
                    nested(cursor)
                })
                cursor = cursor - 1
            }
            assert!(with (store.map) {
                nested(next)
            } == initialized)
        }
    }
}
"#,
    )
    .expect("write fixture");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let fixture_str = fixture.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--standalone",
        "--emit",
        "bytecode",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_str.as_str(),
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
}

#[test]
fn test_cli_build_emit_abi_writes_json_artifact() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/emit_abi/abi_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "abi",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_path_str,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let abi_path = out_dir.join("Foo.abi.json");
    assert!(abi_path.is_file(), "missing ABI artifact:\n{output}");
    assert!(
        !out_dir.join("Foo.bin").exists(),
        "unexpected deploy artifact"
    );
    assert!(
        !out_dir.join("Foo.runtime.bin").exists(),
        "unexpected runtime artifact"
    );

    let abi: Value = serde_json::from_str(&fs::read_to_string(&abi_path).expect("read ABI"))
        .expect("parse ABI JSON");
    let function = abi
        .as_array()
        .expect("abi array")
        .iter()
        .find(|entry| entry["type"] == "function")
        .expect("function entry");

    assert!(
        output.contains("Foo.abi.json"),
        "unexpected output:\n{output}"
    );
    assert_eq!(function["name"], "ping");
    assert_eq!(function["inputs"][0]["name"], "value");
    assert_eq!(function["inputs"][0]["type"], "uint256");
    assert_eq!(function["outputs"][0]["type"], "uint256");
}

#[test]
fn test_cli_build_emit_metadata_standalone_writes_single_source() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/emit_abi/abi_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");
    let fixture_contents = fs::read_to_string(&fixture_path).expect("read fixture");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_path_str,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let metadata_path = out_dir.join("Foo.metadata.json");
    assert!(
        metadata_path.is_file(),
        "missing metadata artifact:\n{output}"
    );
    assert!(
        output.contains("Foo.metadata.json"),
        "expected metadata filename in output:\n{output}"
    );

    let value: Value =
        serde_json::from_str(&fs::read_to_string(&metadata_path).expect("read metadata"))
            .expect("parse metadata");
    assert_eq!(value["version"], 1);
    assert_eq!(value["language"], "Fe");
    assert!(
        value["compiler"]["version"].is_string(),
        "compiler.version must be a string: {value:?}"
    );
    assert_eq!(
        value["settings"]["compilationTarget"]["abi_contract.fe"],
        "Foo"
    );
    assert_eq!(value["settings"]["evmVersion"], "osaka");
    let sources = value["sources"].as_object().expect("sources object");
    assert_eq!(sources.len(), 1, "expected exactly one source: {sources:?}");
    assert_eq!(sources["abi_contract.fe"]["content"], fixture_contents);
    assert!(
        sources["abi_contract.fe"]["keccak256"]
            .as_str()
            .is_some_and(|h| h.starts_with("0x") && h.len() == 66),
        "expected 0x-prefixed keccak256: {sources:?}"
    );
}

#[test]
fn test_cli_build_emit_metadata_compiler_commit_matches_version_output() {
    // `compiler.commit` must mirror the git hash embedded in `fe --version` (present iff that is).
    let (version_output, version_code) = run_fe_main(&["--version"]);
    assert_eq!(version_code, 0, "fe --version failed:\n{version_output}");
    let expected_commit = version_output
        .split_once('(')
        .and_then(|(_, rest)| rest.split_once(')'))
        .map(|(hash, _)| hash.trim().to_string());

    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/emit_abi/abi_contract.fe");
    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        fixture_path.to_str().expect("fixture utf8"),
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let value: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");
    assert!(value["compiler"]["version"].is_string());
    match expected_commit {
        Some(hash) => assert_eq!(
            value["compiler"]["commit"], hash,
            "compiler.commit must equal the hash in `fe --version`"
        ),
        None => assert!(
            value["compiler"].get("commit").is_none(),
            "compiler.commit must be absent when no git hash is embedded"
        ),
    }
}

#[test]
fn test_cli_build_emit_metadata_ingot_includes_all_sources() {
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"metadata_ingot\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    let lib_src = "use ingot::counter::Counter\n";
    let counter_src = "pub contract Counter {\n}\n";
    fs::write(src_dir.join("lib.fe"), lib_src).expect("write lib.fe");
    fs::write(src_dir.join("counter.fe"), counter_src).expect("write counter.fe");

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let project_path = temp.path().to_str().expect("project path utf8");

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata",
        "--out-dir",
        out_dir_str.as_str(),
        project_path,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let metadata_path = out_dir.join("Counter.metadata.json");
    assert!(
        metadata_path.is_file(),
        "missing metadata artifact:\n{output}"
    );
    let value: Value =
        serde_json::from_str(&fs::read_to_string(&metadata_path).expect("read metadata"))
            .expect("parse metadata");
    assert_eq!(
        value["settings"]["compilationTarget"]["src/counter.fe"],
        "Counter"
    );
    let sources = value["sources"].as_object().expect("sources object");
    assert_eq!(sources["src/lib.fe"]["content"], lib_src);
    assert_eq!(sources["src/counter.fe"]["content"], counter_src);
    assert!(
        !sources
            .keys()
            .any(|k| k.starts_with("std/") || k.starts_with("core/")),
        "std/core must not appear in sources: {sources:?}"
    );
}

#[test]
fn test_cli_build_emit_metadata_includes_transitive_dependency() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    write_app_with_path_dependency(root);

    let out_dir = root.join("app/out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        root.join("app").to_str().expect("app utf8"),
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let value: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");

    let sources = value["sources"].as_object().expect("sources object");
    assert!(
        sources.contains_key("src/main.fe"),
        "root source missing: {sources:?}"
    );
    assert!(
        sources.contains_key("mylib/src/lib.fe"),
        "dependency source must be alias-namespaced: {sources:?}"
    );
    assert!(
        !sources
            .keys()
            .any(|k| k.starts_with("std/") || k.starts_with("core/")),
        "std/core must not appear in sources: {sources:?}"
    );

    let ingots = value["settings"]["ingots"]
        .as_array()
        .expect("ingots array");
    let app = ingots
        .iter()
        .find(|i| i["name"] == "app")
        .expect("app ingot entry");
    assert_eq!(app["namespace"], "");
    assert_eq!(app["dependencies"]["mylib"], "mylib");
    let mylib = ingots
        .iter()
        .find(|i| i["name"] == "mylib")
        .expect("mylib ingot entry");
    assert_eq!(mylib["namespace"], "mylib");
    assert_eq!(mylib["version"], "1.2.0");
    // mylib's fe.toml sets `arithmetic = "unchecked"`; the resolved effective value is recorded.
    assert_eq!(mylib["arithmetic"], "unchecked");
}

#[test]
fn test_cli_build_emit_metadata_settings_reflect_optimize_and_arithmetic() {
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"metadata_settings\"\nversion = \"0.1.0\"\narithmetic = \"unchecked\"\n",
    )
    .expect("write fe.toml");
    fs::write(src_dir.join("lib.fe"), "pub contract Foo {\n}\n").expect("write lib.fe");

    let out_dir = temp.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata",
        "--optimize",
        "2",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        temp.path().to_str().expect("project utf8"),
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let value: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");
    assert_eq!(value["settings"]["optimizer"]["level"], "2");
    assert_eq!(value["settings"]["arithmetic"], "unchecked");
    // `dependencyArithmetic` defaults to `defer` when unset.
    assert_eq!(value["settings"]["dependencyArithmetic"], "defer");
}

#[test]
fn test_cli_build_emit_metadata_combined_with_other_artifacts() {
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"metadata_combined\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    fs::write(
        src_dir.join("lib.fe"),
        "pub msg FooMsg {\n    #[selector = sol(\"run()\")]\n    Run -> u256,\n}\n\npub contract Foo {\n    recv FooMsg {\n        Run -> u256 {\n            1\n        }\n    }\n}\n",
    )
    .expect("write lib.fe");

    let out_dir = temp.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "bytecode,runtime-bytecode,abi,metadata",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        temp.path().to_str().expect("project utf8"),
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
    for artifact in [
        "Foo.bin",
        "Foo.runtime.bin",
        "Foo.abi.json",
        "Foo.metadata.json",
    ] {
        assert!(
            out_dir.join(artifact).is_file(),
            "missing {artifact}:\n{output}"
        );
    }

    // `output.abi` in the metadata must match the standalone `.abi.json` artifact.
    let metadata: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");
    let abi_json: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.abi.json")).expect("read abi.json"),
    )
    .expect("parse abi.json");
    assert_eq!(
        metadata["output"]["abi"], abi_json,
        "metadata output.abi must equal the .abi.json artifact"
    );
}

#[test]
fn test_cli_build_metadata_round_trip_reproduces_runtime_bytecode() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    write_app_with_path_dependency(root);

    // Original build: emit metadata + runtime bytecode.
    let out_dir = root.join("app/out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata,runtime-bytecode",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        root.join("app").to_str().expect("app utf8"),
    ]);
    assert_eq!(exit_code, 0, "original build failed:\n{output}");
    let original_runtime =
        fs::read_to_string(out_dir.join("Foo.runtime.bin")).expect("read original runtime.bin");
    let metadata: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");

    // Reconstruct a fresh project solely from the metadata, then rebuild.
    let recon = tempdir().expect("recon tempdir");
    let root_dir = reconstruct_project_from_metadata(&metadata, recon.path());
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
        root_dir.to_str().expect("root utf8"),
    ]);
    assert_eq!(exit_code, 0, "rebuild from metadata failed:\n{output}");
    let rebuilt_runtime =
        fs::read_to_string(recon_out.join("Foo.runtime.bin")).expect("read rebuilt runtime.bin");

    assert_eq!(
        original_runtime, rebuilt_runtime,
        "runtime bytecode rebuilt from metadata.json must be byte-identical"
    );
}

#[test]
fn test_cli_build_emit_metadata_workspace_collisions_are_rejected() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    fs::create_dir_all(root.join("ingots/a/src")).expect("create ingot a");
    fs::create_dir_all(root.join("ingots/b/src")).expect("create ingot b");
    fs::write(
        root.join("fe.toml"),
        r#"[workspace]
name = "metadata_workspace_collision"
version = "0.1.0"
members = [
  { path = "ingots/a", name = "a" },
  { path = "ingots/b", name = "b" },
]
"#,
    )
    .expect("write workspace fe.toml");
    fs::write(
        root.join("ingots/a/fe.toml"),
        "[ingot]\nname = \"a\"\nversion = \"0.1.0\"\n",
    )
    .expect("write ingot a fe.toml");
    fs::write(
        root.join("ingots/b/fe.toml"),
        "[ingot]\nname = \"b\"\nversion = \"0.1.0\"\n",
    )
    .expect("write ingot b fe.toml");
    fs::write(root.join("ingots/a/src/lib.fe"), "pub contract Foo {\n}\n")
        .expect("write ingot a source");
    fs::write(root.join("ingots/b/src/lib.fe"), "pub contract Foo {\n}\n")
        .expect("write ingot b source");

    let (output, exit_code) = run_fe_main_in_dir(&["build", "--emit", "metadata"], root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("Contract names collide in a flat workspace output directory"),
        "expected collision error:\n{output}"
    );
}

#[test]
fn test_cli_build_emit_metadata_workspace_scopes_each_contract_to_its_member() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    fs::create_dir_all(root.join("ingots/a/src")).expect("create ingot a");
    fs::create_dir_all(root.join("ingots/b/src")).expect("create ingot b");
    fs::write(
        root.join("fe.toml"),
        r#"[workspace]
name = "metadata_workspace_scope"
version = "0.1.0"
members = [
  { path = "ingots/a", name = "a" },
  { path = "ingots/b", name = "b" },
]
"#,
    )
    .expect("write workspace fe.toml");
    fs::write(
        root.join("ingots/a/fe.toml"),
        "[ingot]\nname = \"a\"\nversion = \"0.1.0\"\n",
    )
    .expect("write ingot a fe.toml");
    fs::write(
        root.join("ingots/b/fe.toml"),
        "[ingot]\nname = \"b\"\nversion = \"0.1.0\"\n",
    )
    .expect("write ingot b fe.toml");
    let foo_src = "pub contract Foo {\n}\n";
    let bar_src = "pub contract Bar {\n}\n";
    fs::write(root.join("ingots/a/src/lib.fe"), foo_src).expect("write a source");
    fs::write(root.join("ingots/b/src/lib.fe"), bar_src).expect("write b source");

    let out_dir = root.join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        root.to_str().expect("root utf8"),
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let foo: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read Foo metadata"),
    )
    .expect("parse Foo metadata");
    let bar: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Bar.metadata.json")).expect("read Bar metadata"),
    )
    .expect("parse Bar metadata");

    // Each contract's metadata references only its own member's sources.
    assert_eq!(foo["settings"]["compilationTarget"]["src/lib.fe"], "Foo");
    let foo_sources = foo["sources"].as_object().expect("Foo sources");
    assert_eq!(foo_sources["src/lib.fe"]["content"], foo_src);
    assert!(
        !foo_sources
            .values()
            .any(|s| s["content"].as_str().is_some_and(|c| c.contains("Bar"))),
        "Foo metadata must not contain member b's sources: {foo_sources:?}"
    );

    assert_eq!(bar["settings"]["compilationTarget"]["src/lib.fe"], "Bar");
    let bar_sources = bar["sources"].as_object().expect("Bar sources");
    assert_eq!(bar_sources["src/lib.fe"]["content"], bar_src);
    assert!(
        !bar_sources
            .values()
            .any(|s| s["content"].as_str().is_some_and(|c| c.contains("Foo"))),
        "Bar metadata must not contain member a's sources: {bar_sources:?}"
    );
}

#[test]
fn test_cli_build_emit_metadata_disambiguates_same_named_dependencies() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    // Two distinct ingots both named "util" (different versions) must not collide.
    for (dir, version, func) in [
        ("util_a", "1.0.0", "helper_one"),
        ("util_b", "2.0.0", "helper_two"),
    ] {
        fs::create_dir_all(root.join(dir).join("src")).expect("create util src");
        fs::write(
            root.join(dir).join("fe.toml"),
            format!("[ingot]\nname = \"util\"\nversion = \"{version}\"\n"),
        )
        .expect("write util fe.toml");
        fs::write(
            root.join(dir).join("src/lib.fe"),
            format!("pub fn {func}() -> u256 {{\n    return 1\n}}\n"),
        )
        .expect("write util source");
    }
    fs::create_dir_all(root.join("app/src")).expect("create app/src");
    fs::write(
        root.join("app/fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\nu1 = { path = \"../util_a\" }\nu2 = { path = \"../util_b\" }\n",
    )
    .expect("write app/fe.toml");
    fs::write(
        root.join("app/src/main.fe"),
        "use u1::helper_one\nuse u2::helper_two\n\npub msg FooMsg {\n    #[selector = sol(\"run()\")]\n    Run -> u256,\n}\n\npub contract Foo {\n    recv FooMsg {\n        Run -> u256 {\n            helper_one() + helper_two()\n        }\n    }\n}\n",
    )
    .expect("write app/src/main.fe");

    let out_dir = root.join("app/out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata,runtime-bytecode",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        root.join("app").to_str().expect("app utf8"),
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let value: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");

    // Both same-named ingots must appear under distinct namespaces, with both sources retained.
    let ingots = value["settings"]["ingots"]
        .as_array()
        .expect("ingots array");
    let util_namespaces: Vec<&str> = ingots
        .iter()
        .filter(|i| i["name"] == "util")
        .map(|i| i["namespace"].as_str().expect("namespace"))
        .collect();
    assert_eq!(
        util_namespaces.len(),
        2,
        "expected two `util` ingots: {ingots:?}"
    );
    assert_ne!(
        util_namespaces[0], util_namespaces[1],
        "same-named ingots must get distinct namespaces: {util_namespaces:?}"
    );

    let sources = value["sources"].as_object().expect("sources object");
    for ns in &util_namespaces {
        assert!(
            sources.contains_key(&format!("{ns}/src/lib.fe")),
            "missing source for namespace {ns}: {:?}",
            sources.keys().collect::<Vec<_>>()
        );
    }
    // The two helpers prove neither dependency's source overwrote the other.
    let all_content: String = sources
        .values()
        .filter_map(|s| s["content"].as_str())
        .collect();
    assert!(all_content.contains("helper_one") && all_content.contains("helper_two"));

    // Round-trip: reconstruct from metadata and rebuild to byte-identical runtime bytecode.
    let original_runtime =
        fs::read_to_string(out_dir.join("Foo.runtime.bin")).expect("read original runtime.bin");
    let recon = tempdir().expect("recon tempdir");
    let root_dir = reconstruct_project_from_metadata(&value, recon.path());
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
        root_dir.to_str().expect("root utf8"),
    ]);
    assert_eq!(exit_code, 0, "rebuild from metadata failed:\n{output}");
    let rebuilt_runtime =
        fs::read_to_string(recon_out.join("Foo.runtime.bin")).expect("read rebuilt runtime.bin");
    assert_eq!(
        original_runtime, rebuilt_runtime,
        "runtime bytecode must reproduce even with same-named dependencies"
    );
}

#[test]
fn test_cli_build_emit_metadata_disambiguates_dependency_from_root_source_path() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    fs::create_dir_all(root.join("app/src/src")).expect("create app sources");
    fs::create_dir_all(root.join("dep/src")).expect("create dependency sources");
    fs::write(
        root.join("app/fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\ndep = { path = \"../dep\" }\n",
    )
    .expect("write app fe.toml");
    fs::write(
        root.join("dep/fe.toml"),
        "[ingot]\nname = \"src\"\nversion = \"1.0.0\"\n",
    )
    .expect("write dependency fe.toml");
    let root_only_src = "pub fn root_only() -> u256 {\n    return 7\n}\n";
    let dependency_src = "pub fn helper() -> u256 {\n    return 42\n}\n";
    fs::write(root.join("app/src/src/lib.fe"), root_only_src).expect("write root source");
    fs::write(root.join("dep/src/lib.fe"), dependency_src).expect("write dependency source");
    fs::write(
        root.join("app/src/main.fe"),
        "use dep::helper\n\npub msg FooMsg {\n    #[selector = sol(\"run()\")]\n    Run -> u256,\n}\n\npub contract Foo {\n    recv FooMsg {\n        Run -> u256 {\n            helper()\n        }\n    }\n}\n",
    )
    .expect("write app main source");

    let out_dir = root.join("app/out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata,runtime-bytecode",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        root.join("app").to_str().expect("app utf8"),
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let metadata: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");
    let sources = metadata["sources"].as_object().expect("sources object");
    assert_eq!(sources["src/src/lib.fe"]["content"], root_only_src);
    assert_eq!(sources["src-2/src/lib.fe"]["content"], dependency_src);
    let dep = metadata["settings"]["ingots"]
        .as_array()
        .expect("ingots array")
        .iter()
        .find(|ingot| ingot["name"] == "src")
        .expect("dependency ingot");
    assert_eq!(dep["namespace"], "src-2");
    assert_eq!(
        metadata["settings"]["ingots"][0]["dependencies"]["dep"],
        "src-2"
    );

    let original_runtime =
        fs::read_to_string(out_dir.join("Foo.runtime.bin")).expect("read original runtime.bin");
    let recon = tempdir().expect("recon tempdir");
    let root_dir = reconstruct_project_from_metadata(&metadata, recon.path());
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
        root_dir.to_str().expect("root utf8"),
    ]);
    assert_eq!(exit_code, 0, "rebuild from metadata failed:\n{output}");
    let rebuilt_runtime =
        fs::read_to_string(recon_out.join("Foo.runtime.bin")).expect("read rebuilt runtime.bin");
    assert_eq!(original_runtime, rebuilt_runtime);
}

#[test]
fn test_cli_build_emit_metadata_preserves_dependency_arithmetic_across_edge_types() {
    // A workspace member's `dependency-arithmetic` is applied to EXTERNAL edges but not to
    // workspace-internal ones. The metadata must capture the resulting per-ingot effective
    // arithmetic so that a reconstructed (flattened) project still reproduces the bytecode.
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    fs::create_dir_all(root.join("ingots/app/src")).expect("create app");
    fs::create_dir_all(root.join("ingots/internal_lib/src")).expect("create internal_lib");
    fs::create_dir_all(root.join("ext_lib/src")).expect("create ext_lib");

    fs::write(
        root.join("fe.toml"),
        "[workspace]\nname = \"ws\"\nversion = \"0.1.0\"\nmembers = [\n  { path = \"ingots/app\", name = \"app\" },\n  { path = \"ingots/internal_lib\", name = \"internal_lib\" },\n]\n",
    )
    .expect("write workspace fe.toml");
    // `app` forces unchecked arithmetic onto its external dependencies.
    fs::write(
        root.join("ingots/app/fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\ndependency-arithmetic = \"unchecked\"\n\n[dependencies]\ninternal_lib = true\next = { path = \"../../ext_lib\" }\n",
    )
    .expect("write app fe.toml");
    fs::write(
        root.join("ingots/app/src/main.fe"),
        "use internal_lib::ilib\nuse ext::elib\n\npub msg M {\n    #[selector = sol(\"run(uint256,uint256)\")]\n    Run { a: u256, b: u256 } -> u256,\n}\n\npub contract App {\n    recv M {\n        Run { a, b } -> u256 {\n            ilib(x: a, y: b) + elib(x: a, y: b)\n        }\n    }\n}\n",
    )
    .expect("write app main.fe");
    fs::write(
        root.join("ingots/internal_lib/fe.toml"),
        "[ingot]\nname = \"internal_lib\"\nversion = \"0.1.0\"\narithmetic = \"checked\"\n",
    )
    .expect("write internal_lib fe.toml");
    fs::write(
        root.join("ingots/internal_lib/src/lib.fe"),
        "pub fn ilib(x: u256, y: u256) -> u256 {\n    return x + y\n}\n",
    )
    .expect("write internal_lib lib.fe");
    fs::write(
        root.join("ext_lib/fe.toml"),
        "[ingot]\nname = \"ext_lib\"\nversion = \"0.1.0\"\narithmetic = \"checked\"\n",
    )
    .expect("write ext_lib fe.toml");
    fs::write(
        root.join("ext_lib/src/lib.fe"),
        "pub fn elib(x: u256, y: u256) -> u256 {\n    return x + y\n}\n",
    )
    .expect("write ext_lib lib.fe");

    let out_dir = root.join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata,runtime-bytecode",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        root.to_str().expect("root utf8"),
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let value: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("App.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");
    let ingots = value["settings"]["ingots"]
        .as_array()
        .expect("ingots array");
    let arith = |name: &str| -> String {
        ingots
            .iter()
            .find(|i| i["name"] == name)
            .and_then(|i| i["arithmetic"].as_str())
            .unwrap_or("<missing>")
            .to_string()
    };
    // External edge is forced unchecked; internal (same-workspace) edge keeps its own checked.
    assert_eq!(
        arith("ext_lib"),
        "unchecked",
        "external dep must record forced arithmetic"
    );
    assert_eq!(
        arith("internal_lib"),
        "checked",
        "internal edge must not be forced"
    );

    // Round-trip: the flattened reconstruction must still reproduce the exact bytecode.
    let original_runtime =
        fs::read_to_string(out_dir.join("App.runtime.bin")).expect("read original runtime.bin");
    let recon = tempdir().expect("recon tempdir");
    let root_dir = reconstruct_project_from_metadata(&value, recon.path());
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
        root_dir.to_str().expect("root utf8"),
    ]);
    assert_eq!(exit_code, 0, "rebuild from metadata failed:\n{output}");
    let rebuilt_runtime =
        fs::read_to_string(recon_out.join("App.runtime.bin")).expect("read rebuilt runtime.bin");
    assert_eq!(
        original_runtime, rebuilt_runtime,
        "runtime bytecode must reproduce across internal/external dependency-arithmetic edges"
    );
}

/// Scaffold an `app` ingot with a third-party path dependency `mylib` under `root`.
/// `app` defines `pub contract Foo` and uses `mylib::helper`; `mylib` is `arithmetic = "unchecked"`.
fn write_app_with_path_dependency(root: &Path) {
    fs::create_dir_all(root.join("app/src")).expect("create app/src");
    fs::create_dir_all(root.join("mylib/src")).expect("create mylib/src");
    fs::write(
        root.join("app/fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\nmylib = { path = \"../mylib\" }\n",
    )
    .expect("write app/fe.toml");
    fs::write(
        root.join("app/src/main.fe"),
        "use mylib::helper\n\npub msg FooMsg {\n    #[selector = sol(\"run()\")]\n    Run -> u256,\n}\n\npub contract Foo {\n    recv FooMsg {\n        Run -> u256 {\n            helper()\n        }\n    }\n}\n",
    )
    .expect("write app/src/main.fe");
    fs::write(
        root.join("mylib/fe.toml"),
        "[ingot]\nname = \"mylib\"\nversion = \"1.2.0\"\narithmetic = \"unchecked\"\n",
    )
    .expect("write mylib/fe.toml");
    fs::write(
        root.join("mylib/src/lib.fe"),
        "pub fn helper() -> u256 {\n    return 41 + 1\n}\n",
    )
    .expect("write mylib/src/lib.fe");
}

/// Reconstruct a buildable project under `dest` purely from a `metadata.json` value: one directory
/// per `settings.ingots[]` entry (root at `dest/<name>`, deps at `dest/<namespace>`), regenerating
/// each `fe.toml` and writing every `sources` entry into its namespaced layout. Returns the root
/// ingot directory.
fn reconstruct_project_from_metadata(metadata: &Value, dest: &Path) -> std::path::PathBuf {
    let ingots = metadata["settings"]["ingots"]
        .as_array()
        .expect("ingots array");

    let dir_for = |namespace: &str, name: &str| -> std::path::PathBuf {
        if namespace.is_empty() {
            dest.join(name)
        } else {
            dest.join(namespace)
        }
    };

    // Non-root namespaces, used to route each source key to its owning ingot.
    let non_root_namespaces: Vec<String> = ingots
        .iter()
        .filter_map(|i| {
            let ns = i["namespace"].as_str().unwrap_or("");
            (!ns.is_empty()).then(|| ns.to_string())
        })
        .collect();

    let mut root_dir = dest.to_path_buf();

    // 1. Regenerate each ingot's fe.toml.
    for ingot in ingots {
        let name = ingot["name"].as_str().expect("ingot name");
        let namespace = ingot["namespace"].as_str().unwrap_or("");
        let dir = dir_for(namespace, name);
        fs::create_dir_all(dir.join("src")).expect("create ingot src");
        if namespace.is_empty() {
            root_dir = dir.clone();
        }

        let mut toml = format!("[ingot]\nname = \"{name}\"\n");
        if let Some(version) = ingot["version"].as_str() {
            toml.push_str(&format!("version = \"{version}\"\n"));
        }
        if let Some(arith) = ingot["arithmetic"].as_str() {
            toml.push_str(&format!("arithmetic = \"{arith}\"\n"));
        }
        let deps = ingot["dependencies"].as_object().expect("dependencies");
        let path_deps: Vec<(&String, &str)> = deps
            .iter()
            .filter_map(|(alias, target)| {
                let target = target.as_str()?;
                // std/core are provided by the compiler; only scaffold real path deps.
                (target != "std" && target != "core").then_some((alias, target))
            })
            .collect();
        if !path_deps.is_empty() {
            toml.push_str("\n[dependencies]\n");
            for (alias, target) in path_deps {
                toml.push_str(&format!("{alias} = {{ path = \"../{target}\" }}\n"));
            }
        }
        fs::write(dir.join("fe.toml"), toml).expect("write fe.toml");
    }

    // 2. Write every source into its owning ingot's namespaced src/ layout.
    let sources = metadata["sources"].as_object().expect("sources object");
    for (key, source) in sources {
        let content = source["content"].as_str().unwrap_or("");
        let (namespace, rel) = non_root_namespaces
            .iter()
            .find_map(|ns| {
                key.strip_prefix(&format!("{ns}/"))
                    .map(|rel| (ns.as_str(), rel))
            })
            .unwrap_or(("", key.as_str()));

        // Find the owning ingot's directory (by namespace) to resolve its name for the root case.
        let dir = ingots
            .iter()
            .find(|i| i["namespace"].as_str().unwrap_or("") == namespace)
            .map(|i| dir_for(namespace, i["name"].as_str().unwrap_or("dep")))
            .expect("owning ingot for source");
        let target = dir.join(rel);
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent).expect("create source parent");
        }
        fs::write(target, content).expect("write source");
    }

    root_dir
}

#[test]
fn test_cli_build_emit_abi_includes_constructor_and_mutability_metadata() {
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"emit_abi_mutability\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    fs::write(
        src_dir.join("lib.fe"),
        r#"
use std::abi::sol

msg WalletMsg {
    #[selector = sol("fund()")]
    Fund,

    #[selector = sol("peek()")]
    Peek -> u256,
}

pub contract Wallet {
    #[payable]
    init(seed: u256, values: [u256; 2]) {}

    recv WalletMsg {
        #[payable]
        Fund {} {}

        Peek -> u256 {
            7
        }
    }
}
"#,
    )
    .expect("write lib.fe");

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let project_path = temp.path().to_str().expect("project path utf8");

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "abi",
        "--contract",
        "Wallet",
        "--out-dir",
        out_dir_str.as_str(),
        project_path,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let abi_path = out_dir.join("Wallet.abi.json");
    assert!(abi_path.is_file(), "missing ABI artifact:\n{output}");

    let abi: Value = serde_json::from_str(&fs::read_to_string(&abi_path).expect("read ABI"))
        .expect("parse ABI JSON");
    let entries = abi.as_array().expect("abi array");
    let constructor = entries
        .iter()
        .find(|entry| entry["type"] == "constructor")
        .expect("constructor entry");
    let fund = entries
        .iter()
        .find(|entry| entry["type"] == "function" && entry["name"] == "fund")
        .expect("fund entry");
    let peek = entries
        .iter()
        .find(|entry| entry["type"] == "function" && entry["name"] == "peek")
        .expect("peek entry");

    assert_eq!(constructor["stateMutability"], "payable");
    assert_eq!(constructor["inputs"][0]["name"], "seed");
    assert_eq!(constructor["inputs"][0]["type"], "uint256");
    assert_eq!(constructor["inputs"][1]["name"], "values");
    assert_eq!(constructor["inputs"][1]["type"], "uint256[2]");
    assert_eq!(fund["stateMutability"], "payable");
    assert_eq!(peek["stateMutability"], "pure");
    assert_eq!(peek["outputs"][0]["type"], "uint256");
}

#[test]
fn test_cli_build_emit_abi_includes_imported_events() {
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"emit_abi_events\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    fs::write(
        src_dir.join("events.fe"),
        r#"
#[event]
pub struct Transfer {
    pub value: u256,
}

#[event]
pub struct UnusedEvent {
    pub value: u256,
}
"#,
    )
    .expect("write events.fe");
    fs::write(
        src_dir.join("helpers.fe"),
        r#"
use std::evm::Log
use super::events::Transfer

pub fn emit_transfer(value: u256) uses (log: mut Log) {
    log.emit(Transfer { value })
}
"#,
    )
    .expect("write helpers.fe");
    fs::write(
        src_dir.join("lib.fe"),
        r#"
use std::abi::sol
use std::evm::Log
use helpers::emit_transfer

msg FooMsg {
    #[selector = sol("ping()")]
    Ping,
}

pub contract Foo uses (log: mut Log) {
    recv FooMsg {
        Ping uses (mut log) {
            emit_transfer(value: 1)
        }
    }
}
"#,
    )
    .expect("write lib.fe");

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let project_path = temp.path().to_str().expect("project path utf8");

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "abi",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir_str.as_str(),
        project_path,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let abi_path = out_dir.join("Foo.abi.json");
    let abi: Value = serde_json::from_str(&fs::read_to_string(&abi_path).expect("read ABI"))
        .expect("parse ABI JSON");
    let event = abi
        .as_array()
        .expect("abi array")
        .iter()
        .find(|entry| entry["type"] == "event" && entry["name"] == "Transfer")
        .expect("event entry");

    assert_eq!(event["inputs"][0]["name"], "value");
    assert_eq!(event["inputs"][0]["type"], "uint256");
    assert!(
        abi.as_array()
            .expect("abi array")
            .iter()
            .all(|entry| entry["name"] != "UnusedEvent"),
        "unexpected unused event in ABI: {abi}"
    );
}

#[test]
fn test_cli_build_emit_abi_skips_hex_selectors_with_warning() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "abi",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_path_str,
    ]);

    assert_eq!(exit_code, 0, "ABI build should succeed:\n{output}");
    assert!(
        output.contains("selector signature is unknown"),
        "expected warning about unknown selector:\n{output}"
    );
    assert!(
        !out_dir.join("Foo.abi.json").exists(),
        "empty ABI artifact should not be written"
    );
}

#[test]
fn test_cli_build_emit_abi_skips_manual_msgvariant_codecs() {
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"emit_abi_manual_msgvariant\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    fs::write(
        src_dir.join("lib.fe"),
        r#"
use std::abi::sol

struct Weird {
    pub amount: u64,
    pub flag: bool,
}

impl core::abi::AbiSize for Weird {
    const HEAD_SIZE: u256 = 64
    const IS_DYNAMIC: bool = false
}

impl core::abi::Encode<std::abi::Sol> for Weird {
    const DIRECT_ENCODE: bool = false

    fn encode<E: core::abi::AbiEncoder<std::abi::Sol>>(own self, _ e: mut E) {
        self.flag.encode(e)
        self.amount.encode(e)
    }

    fn encode_to_ptr(own self, _ ptr: u256) {
        std::abi::Sol::store_word(ptr: ptr, value: if self.flag { 1 } else { 0 })
        std::abi::Sol::store_word(ptr: ptr + 32, value: self.amount as u256)
    }
}

impl core::abi::Decode<std::abi::Sol> for Weird {
    fn decode_payload<D: core::abi::AbiDecoder<std::abi::Sol>>(_ d: mut D) -> Self {
        let flag = bool::decode_payload(d)
        let amount = u64::decode_payload(d)
        Self { amount, flag }
    }
}

impl core::message::MsgVariant<std::abi::Sol> for Weird {
    const SELECTOR: u32 = sol("foo(bool,uint64)")
    type Return = ()
}

pub contract Foo {
    recv {
        Weird { amount, flag } uses () {
            let _ = amount
            let _ = flag
        }
    }
}
"#,
    )
    .expect("write lib.fe");

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let project_path = temp.path().to_str().expect("project path utf8");

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "abi",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir_str.as_str(),
        project_path,
    ]);

    assert_eq!(exit_code, 0, "ABI build should succeed:\n{output}");
    assert!(
        output.contains("manual `MsgVariant` impls"),
        "expected warning about manual MsgVariant codecs:\n{output}"
    );
    assert!(
        !out_dir.join("Foo.abi.json").exists(),
        "empty ABI artifact should not be written"
    );
}

#[test]
fn test_cli_build_default_emit_skips_empty_abi_artifact() {
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

    assert_eq!(exit_code, 0, "default build should succeed:\n{output}");
    assert!(
        output.contains("selector signature is unknown"),
        "expected warning about unknown selector:\n{output}"
    );
    assert!(
        !output.contains("Foo.abi.json"),
        "empty ABI artifact should not be reported as written:\n{output}"
    );
    assert!(
        !out_dir.join("Foo.abi.json").exists(),
        "empty ABI artifact should not be written"
    );
}

#[test]
fn test_cli_build_default_emit_removes_stale_empty_abi_artifact() {
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"stale_empty_abi\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let project_path = temp.path().to_str().expect("project path utf8");

    fs::write(
        src_dir.join("lib.fe"),
        r#"
use std::abi::sol

msg FooMsg {
    #[selector = sol("ping()")]
    Ping,
}

pub contract Foo {
    recv FooMsg {
        Ping {}
    }
}
"#,
    )
    .expect("write initial lib.fe");

    let (first_output, first_exit_code) = run_fe_main(&[
        "build",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir_str.as_str(),
        project_path,
    ]);

    assert_eq!(
        first_exit_code, 0,
        "initial build should succeed:\n{first_output}"
    );
    assert!(
        out_dir.join("Foo.abi.json").exists(),
        "expected initial ABI artifact"
    );

    fs::write(
        src_dir.join("lib.fe"),
        r#"
msg FooMsg {
    #[selector = 0x12345678]
    Ping,
}

pub contract Foo {
    recv FooMsg {
        Ping {}
    }
}
"#,
    )
    .expect("write updated lib.fe");

    let (second_output, second_exit_code) = run_fe_main(&[
        "build",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir_str.as_str(),
        project_path,
    ]);

    assert_eq!(
        second_exit_code, 0,
        "second build should succeed:\n{second_output}"
    );
    assert!(
        second_output.contains("selector signature is unknown"),
        "expected warning about unknown selector:\n{second_output}"
    );
    assert!(
        !out_dir.join("Foo.abi.json").exists(),
        "stale ABI artifact should be removed after it becomes empty"
    );
}

#[test]
fn test_cli_build_ingot_root_reexported_contract_sonatina_artifacts() {
    let fixture_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build_ingots/root_reexport_contract");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--contract",
        "KeyperSet",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_dir_str,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
    assert_hex_artifact(&out_dir.join("KeyperSet.bin"));
    assert_hex_artifact(&out_dir.join("KeyperSet.runtime.bin"));
}

#[test]
fn test_cli_build_ingot_sonatina_ir_respects_contract_filter() {
    let fixture_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build_ingots/multi_file");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "ir",
        "--contract",
        "Foo",
        "--out-dir",
        out_dir_str.as_str(),
        fixture_dir_str,
    ]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    let ir_path = out_dir.join("multi_file.sona");
    let ir = fs::read_to_string(&ir_path).expect("read Sonatina IR");
    assert!(ir.contains("object @Foo"), "expected Foo object:\n{ir}");
    assert!(
        !ir.contains("object @Bar"),
        "contract filter should exclude Bar object:\n{ir}"
    );
}

fn assert_hex_artifact(path: &std::path::Path) {
    let contents = fs::read_to_string(path).unwrap_or_else(|err| {
        panic!("read artifact {path:?}: {err}");
    });
    assert!(
        !contents.chars().any(char::is_whitespace),
        "expected artifact to contain only hex digits: {path:?}"
    );
    assert!(!contents.is_empty(), "expected non-empty hex: {path:?}");
    assert!(
        contents.chars().all(|c| c.is_ascii_hexdigit()),
        "expected hex bytes in artifact: {path:?}"
    );
    assert_eq!(
        contents.len() % 2,
        0,
        "expected even-length hex in artifact: {path:?}"
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

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test",
    glob: "*.fe",
)]
fn test_fe_test(fixture: Fixture<&str>) {
    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", fixture.path()]);
    assert_eq!(
        exit_code,
        0,
        "fe test failed for {path}:\n{output}\n\nTo reproduce:\n  cargo run --bin fe -- test --jobs 1 {path}",
        path = fixture.path(),
    );
}

#[test]
fn test_fe_test_rejects_oversized_balance_literal() {
    let temp = tempdir().expect("tempdir");
    let path = temp.path().join("oversized_balance.fe");
    fs::write(
        &path,
        r#"
#[test(balance = 0x10000000000000000000000000000000000000000000000000000000000000000)]
fn too_big_balance() {}
"#,
    )
    .expect("write fixture");
    let path = path
        .to_str()
        .unwrap_or_else(|| panic!("fixture path is not utf-8: {}", path.display()));

    let (output, exit_code) = run_fe_main(&["test", path]);
    assert_ne!(
        exit_code, 0,
        "expected fe test to reject oversized #[test(balance = ...)]:\n{output}"
    );
    assert!(
        output.contains(
            "invalid #[test] function `too_big_balance`: #[test(balance = ...)] must fit in u256"
        ),
        "expected oversized balance error, got:\n{output}"
    );
}

#[test]
fn test_fe_test_rejects_malformed_balance_literal() {
    let temp = tempdir().expect("tempdir");
    let cases = [
        (
            "missing_balance_value",
            r#"
#[test(balance)]
fn missing_balance_value() {}
"#,
            "invalid #[test] function `missing_balance_value`: #[test(balance = ...)] expects an integer literal",
        ),
        (
            "non_integer_balance_value",
            r#"
#[test(balance = true)]
fn non_integer_balance_value() {}
"#,
            "invalid #[test] function `non_integer_balance_value`: #[test(balance = ...)] expects an integer literal",
        ),
    ];

    for (filename, source, expected) in cases {
        let path = temp.path().join(format!("{filename}.fe"));
        fs::write(&path, source).expect("write fixture");
        let path = path
            .to_str()
            .unwrap_or_else(|| panic!("fixture path is not utf-8: {}", path.display()));

        let (output, exit_code) = run_fe_main(&["test", path]);
        assert_ne!(
            exit_code, 0,
            "expected fe test to reject malformed #[test(balance = ...)]:\n{output}"
        );
        assert!(
            output.contains(expected),
            "expected malformed balance error, got:\n{output}"
        );
    }
}

/// Runs `fe test` and snapshots the output to verify behavior of passing/failing tests and logs.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test_runner",
    glob: "*.fe",
)]
fn test_fe_test_runner(fixture: Fixture<&str>) {
    let mut args = vec!["test", "--jobs", "1"];
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
fn test_cli_check_dependency_arithmetic_conflict_is_error() {
    let temp = tempfile::Builder::new()
        .prefix("fe-dependency-arithmetic-conflict-")
        .tempdir()
        .expect("tempdir");
    let root = temp
        .path()
        .canonicalize()
        .expect("canonicalize tempdir")
        .join("workspace");
    let dep = root.join("dep");
    let a = root.join("ingots/a");
    let b = root.join("ingots/b");
    fs::create_dir_all(dep.join("src")).expect("create dep src");
    fs::create_dir_all(a.join("src")).expect("create a src");
    fs::create_dir_all(b.join("src")).expect("create b src");

    fs::write(
        root.join("fe.toml"),
        r#"
[workspace]
members = ["ingots/a", "ingots/b"]
"#,
    )
    .expect("write workspace fe.toml");
    fs::write(
        dep.join("fe.toml"),
        r#"
[ingot]
name = "dep"
version = "0.1.0"
"#,
    )
    .expect("write dep fe.toml");
    fs::write(
        dep.join("src/lib.fe"),
        r#"
pub fn add(x: u8, y: u8) -> u8 {
    x + y
}
"#,
    )
    .expect("write dep lib.fe");
    fs::write(
        a.join("fe.toml"),
        r#"
[ingot]
name = "a"
version = "0.1.0"
dependency-arithmetic = "checked"

[dependencies]
dep = { path = "../../dep" }
"#,
    )
    .expect("write a fe.toml");
    fs::write(
        a.join("src/lib.fe"),
        r#"
use dep::add

pub fn call(x: u8, y: u8) -> u8 {
    add(x, y)
}
"#,
    )
    .expect("write a lib.fe");
    fs::write(
        b.join("fe.toml"),
        r#"
[ingot]
name = "b"
version = "0.1.0"
dependency-arithmetic = "unchecked"

[dependencies]
dep = { path = "../../dep" }
"#,
    )
    .expect("write b fe.toml");
    fs::write(
        b.join("src/lib.fe"),
        r#"
use dep::add

pub fn call(x: u8, y: u8) -> u8 {
    add(x, y)
}
"#,
    )
    .expect("write b lib.fe");

    let (output, exit_code) = run_fe_main_in_dir(&["check"], &root);
    assert_ne!(
        exit_code, 0,
        "expected dependency arithmetic conflict:\n{output}"
    );
    assert!(
        output.contains("Dependency arithmetic conflict for")
            && output.contains("forced Checked")
            && output.contains("forced Unchecked"),
        "expected conflict diagnostic in output:\n{output}"
    );
}

#[test]
fn test_cli_test_profile_selects_ingot_arithmetic() {
    let fixture_dir = fe_test_runner_fixture_dir("arithmetic_profile_ingot");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (test_output, test_exit_code) = run_fe_main(&["test", fixture_dir_str]);
    assert_eq!(test_exit_code, 0, "fe test failed:\n{test_output}");
    assert!(
        test_output.contains("PASS  [<time>] arithmetic_profile_ingot_wraps_in_test_profile")
            && test_output.contains("1 passed"),
        "expected test profile to run unchecked arithmetic:\n{test_output}"
    );

    let (release_output, release_exit_code) =
        run_fe_main(&["test", "--profile", "release", fixture_dir_str]);
    assert_ne!(
        release_exit_code, 0,
        "expected release profile test failure:\n{release_output}"
    );
    assert!(
        release_output.contains("1 failed"),
        "expected checked arithmetic failure in release profile:\n{release_output}"
    );
}

#[test]
fn test_cli_test_profile_selects_workspace_arithmetic() {
    let fixture_dir = fe_test_runner_fixture_dir("arithmetic_profile_workspace");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (test_output, test_exit_code) = run_fe_main(&["test", fixture_dir_str]);
    assert_eq!(test_exit_code, 0, "fe test failed:\n{test_output}");
    assert!(
        test_output.contains("PASS  [<time>] arithmetic_profile_workspace_wraps_in_test_profile")
            && test_output.contains("1 passed"),
        "expected workspace test profile to run unchecked arithmetic:\n{test_output}"
    );

    let (release_output, release_exit_code) =
        run_fe_main(&["test", "--profile", "release", fixture_dir_str]);
    assert_ne!(
        release_exit_code, 0,
        "expected release profile test failure:\n{release_output}"
    );
    assert!(
        release_output.contains("1 failed"),
        "expected checked arithmetic failure in workspace release profile:\n{release_output}"
    );
}

#[test]
fn test_cli_test_dependency_arithmetic_override() {
    let fixture_dir = fe_test_runner_fixture_dir("dependency_arithmetic_override");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["test", fixture_dir_str]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("PASS  [<time>] dependency_arithmetic_override_wraps_external_overflow")
            && output.contains("1 passed"),
        "expected dependency arithmetic override test to pass, got:\n{output}"
    );
}

#[test]
fn test_cli_test_dependency_arithmetic_defer() {
    let fixture_dir = fe_test_runner_fixture_dir("dependency_arithmetic_defer");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["test", fixture_dir_str]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("PASS  [<time>] dependency_arithmetic_defer_respects_dependency_setting")
            && output.contains("1 passed"),
        "expected dependency arithmetic defer test to pass, got:\n{output}"
    );
}

#[test]
fn test_cli_test_profile_selects_workspace_dependency_arithmetic() {
    let fixture_dir = fe_test_runner_fixture_dir("dependency_arithmetic_profile");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (test_output, test_exit_code) = run_fe_main(&["test", fixture_dir_str]);
    assert_eq!(test_exit_code, 0, "fe test failed:\n{test_output}");
    assert!(
        test_output
            .contains("PASS  [<time>] dependency_arithmetic_profile_wraps_external_overflow")
            && test_output.contains("1 passed"),
        "expected test profile to force unchecked dependency arithmetic:\n{test_output}"
    );

    let (release_output, release_exit_code) =
        run_fe_main(&["test", "--profile", "release", fixture_dir_str]);
    assert_ne!(
        release_exit_code, 0,
        "expected release profile test failure:\n{release_output}"
    );
    assert!(
        release_output.contains("1 failed"),
        "expected checked dependency arithmetic in release profile:\n{release_output}"
    );
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
fn test_cli_test_fe_repo_root() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|path| path.parent())
        .expect("fe repo root");
    let (output, exit_code) = run_fe_main_in_dir(&["test"], root);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
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

#[test]
fn test_cli_test_dependency_diagnostics_block_codegen() {
    let fixture_dir = fe_test_runner_fixture_dir("dependency_diagnostic_gating");
    let fixture_dir = fixture_dir.to_str().expect("fixture path should be utf-8");
    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", "--ingot", "app", fixture_dir]);
    assert_ne!(
        exit_code, 0,
        "expected dependency diagnostic failure:\n{output}"
    );
    assert!(
        output.contains("Error: Errors in dependency"),
        "expected dependency error:\n{output}"
    );
    assert!(
        output.contains("associated const not defined in trait")
            && output.contains("missing associated const `HEAD_SIZE`"),
        "expected ABI trait diagnostics:\n{output}"
    );
    assert!(
        !output.contains("backend panicked") && !output.contains("panicked at"),
        "dependency diagnostics should block codegen before panic:\n{output}"
    );
}

/// Regression test: `create2` of a contract defined in another ingot within
/// the same workspace must compile and run correctly.
#[test]
fn test_cli_test_cross_ingot_create2() {
    let fixture_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test_runner/cross_ingot_create2");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["test", fixture_dir_str]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("PASS  [<time>]  consumer test_create2_contract_from_other_ingot"),
        "expected cross-ingot create2 test, got:\n{output}"
    );
    assert!(
        output.contains("1 passed"),
        "expected 1 passed test, got:\n{output}"
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
        output.contains("PASS  [<time>] test_add"),
        "expected test_add to be discovered, got:\n{output}"
    );
    assert!(
        output.contains("1 passed"),
        "expected 1 passed test, got:\n{output}"
    );
}

#[test]
fn test_cli_test_ingot_reports_mir_diagnostics_in_non_root_modules() {
    let temp = tempdir().expect("tempdir");
    let src = temp.path().join("src");
    fs::create_dir_all(&src).expect("create src");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"non_root_mir_diagnostic\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    fs::write(src.join("lib.fe"), "pub fn root_marker() {}\n").expect("write root module");
    fs::write(
        src.join("helper.fe"),
        r#"
struct Inner {}

fn bad(_ x: own Inner) {
    let y = x
    let z = x
}

#[test]
fn test_non_root_move_conflict() {
    bad(Inner {})
}
"#,
    )
    .expect("write helper module");

    let (output, exit_code) = run_fe_main(&["test", temp.path().to_str().expect("temp utf8")]);
    assert_ne!(exit_code, 0, "expected fe test to fail:\n{output}");
    assert!(
        output.contains("move conflict in `fn bad`"),
        "expected non-root MIR diagnostic, got:\n{output}"
    );
    assert_eq!(
        output.matches("move conflict in `fn bad`").count(),
        1,
        "expected non-root MIR diagnostic once, got:\n{output}"
    );
    assert!(
        !output.contains("Failed to emit test"),
        "expected diagnostics preflight before test emission, got:\n{output}"
    );
}

#[test]
fn test_cli_test_default_project_path_discovers_tests_in_non_root_modules() {
    let fixture_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test_runner/ingot_tests_in_non_root_module");

    let (output, exit_code) = run_fe_main_in_dir(&["test"], &fixture_dir);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("PASS  [<time>] test_add"),
        "expected test_add to be discovered, got:\n{output}"
    );
    assert!(
        output.contains("1 passed"),
        "expected 1 passed test, got:\n{output}"
    );
}

#[test]
fn test_cli_test_single_file_zero_sized_self_method_passes() {
    let fixture = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/zero_sized_self_method.fe");
    let fixture = fixture.to_str().expect("fixture path utf8");

    let (output, exit_code) = run_fe_main(&["test", fixture]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("PASS  [<time>] test_zero_sized_self_method"),
        "expected zero-sized self method test to pass, got:\n{output}"
    );
    assert!(
        output.contains("1 passed"),
        "expected 1 passed test, got:\n{output}"
    );
}

#[test]
fn test_cli_test_emit_ir_and_rmir_writes_artifacts() {
    let temp = tempdir().expect("tempdir");
    let fixture = temp.path().join("emit_test.fe");
    fs::write(&fixture, "#[test]\nfn test_pass() {}\n").expect("write fixture");
    let fixture = fixture.to_str().expect("fixture path utf8");

    let (output, exit_code) =
        run_fe_main(&["test", "--jobs", "1", "-O0", "--emit", "ir,rmir", fixture]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");

    let out_dir = temp.path().join("out");
    let sona_path = out_dir.join("emit_test.test.sona");
    let rmir_path = out_dir.join("emit_test.test.rmir");
    assert!(
        sona_path.is_file(),
        "missing Sonatina IR artifact:\n{output}"
    );
    assert!(rmir_path.is_file(), "missing rMIR artifact:\n{output}");

    let sona = fs::read_to_string(&sona_path).expect("read Sonatina IR");
    let rmir = fs::read_to_string(&rmir_path).expect("read rMIR");
    assert!(
        sona.contains("target = \"evm-ethereum-osaka\""),
        "unexpected Sonatina IR:\n{sona}"
    );
    assert!(
        rmir.contains("package"),
        "unexpected rMIR package dump:\n{rmir}"
    );
    assert!(rmir.contains("bb0:"), "unexpected rMIR body dump:\n{rmir}");
    assert!(
        output.contains("Wrote "),
        "expected artifact output, got:\n{output}"
    );
}

#[test]
fn test_cli_test_single_input_suite_setup_failure_surfaces_error_status() {
    let temp = tempdir().expect("tempdir");
    let invalid = temp.path().join("not_a_fe_input.txt");
    fs::write(&invalid, "not an fe input").expect("write invalid input");
    let invalid = invalid.to_str().expect("invalid path utf8");

    let (output, exit_code) = run_fe_main(&["test", invalid]);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains(
            "ERROR [<time>] Path must be either a .fe file or a directory containing fe.toml"
        ),
        "expected setup failure status line, got:\n{output}"
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
fn test_cli_build_emit_abi_workspace_empty_collisions_are_allowed() {
    let root = workspace_fixture("build_contract_ambiguity");
    let temp = tempdir().expect("tempdir");
    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let (output, exit_code) = run_fe_main_in_dir(
        &["build", "--emit", "abi", "--out-dir", out_dir_str.as_str()],
        &root,
    );
    assert_eq!(exit_code, 0, "expected zero exit code:\n{output}");
    assert!(
        !output.contains("Contract names collide"),
        "unexpected collision error:\n{output}"
    );
    assert!(
        !out_dir.join("Foo.abi.json").exists(),
        "empty ABI-only workspace build should not write an ABI artifact"
    );
}

#[test]
fn test_cli_build_emit_abi_workspace_nonempty_collisions_are_rejected() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    fs::create_dir_all(root.join("ingots/a/src")).expect("create ingot a");
    fs::create_dir_all(root.join("ingots/b/src")).expect("create ingot b");
    fs::write(
        root.join("fe.toml"),
        r#"[workspace]
name = "emit_abi_workspace_collision"
version = "0.1.0"
members = [
  { path = "ingots/a", name = "a" },
  { path = "ingots/b", name = "b" },
]
"#,
    )
    .expect("write workspace fe.toml");
    fs::write(
        root.join("ingots/a/fe.toml"),
        "[ingot]\nname = \"a\"\nversion = \"0.1.0\"\n",
    )
    .expect("write ingot a fe.toml");
    fs::write(
        root.join("ingots/b/fe.toml"),
        "[ingot]\nname = \"b\"\nversion = \"0.1.0\"\n",
    )
    .expect("write ingot b fe.toml");
    fs::write(
        root.join("ingots/a/src/lib.fe"),
        r#"
use std::abi::sol

msg FooMsg {
    #[selector = sol("ping()")]
    Ping,
}

pub contract Foo {
    recv FooMsg {
        Ping {}
    }
}
"#,
    )
    .expect("write ingot a source");
    fs::write(
        root.join("ingots/b/src/lib.fe"),
        r#"
use std::abi::sol

msg FooMsg {
    #[selector = sol("pong()")]
    Pong,
}

pub contract Foo {
    recv FooMsg {
        Pong {}
    }
}
"#,
    )
    .expect("write ingot b source");

    let (output, exit_code) = run_fe_main_in_dir(&["build", "--emit", "abi"], root);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("Contract names collide in a flat workspace output directory"),
        "expected ABI collision error:\n{output}"
    );
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

#[test]
fn test_cli_build_opt_level_flag_is_error() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let (output, exit_code) = run_fe_main(&["build", "--opt-level", "0", fixture_path_str]);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("unexpected argument '--opt-level'"),
        "expected `fe build` to reject `--opt-level`, got:\n{output}"
    );
}

#[test]
fn test_cli_check_optimize_flag_is_error() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/build/simple_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let (output, exit_code) = run_fe_main(&["check", "--optimize", "0", fixture_path_str]);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("unexpected argument '--optimize'"),
        "expected `fe check` to reject optimization flags, got:\n{output}"
    );
}

#[test]
fn test_cli_test_opt_level_flag_is_error() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test_runner/pass.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let (output, exit_code) = run_fe_main(&["test", "--opt-level", "0", fixture_path_str]);
    assert_ne!(exit_code, 0, "expected non-zero exit code:\n{output}");
    assert!(
        output.contains("unexpected argument '--opt-level'"),
        "expected `fe test` to reject `--opt-level`, got:\n{output}"
    );
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
