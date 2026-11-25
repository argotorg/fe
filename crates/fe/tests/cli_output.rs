use dir_test::{Fixture, dir_test};
use serde_json::Value;
use std::{fs, io::IsTerminal, path::Path, process::Command};
#[cfg(all(feature = "cranelift", native_host))]
use std::{
    path::PathBuf,
    thread,
    time::{Duration, Instant},
};
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

fn run_fe_main_with_stdin(args: &[&str], stdin_data: &str) -> (String, i32) {
    use std::io::Write;
    use std::process::Stdio;

    let mut child = Command::new(fe_binary())
        .args(args)
        .env("NO_COLOR", "1")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|_| panic!("Failed to spawn fe {args:?}"));
    child
        .stdin
        .take()
        .expect("child stdin")
        .write_all(stdin_data.as_bytes())
        .expect("write stdin");
    let output = child.wait_with_output().expect("wait for fe");
    let out = FeOutput {
        stdout: normalize_output(&String::from_utf8_lossy(&output.stdout)),
        stderr: normalize_output(&String::from_utf8_lossy(&output.stderr)),
        exit_code: output.status.code().unwrap_or(-1),
    };
    (out.combined(), out.exit_code)
}

#[test]
fn test_cli_check_color_flag_controls_diagnostic_escape_codes() {
    let temp = tempdir().expect("tempdir");
    let file = temp.path().join("type_error.fe");
    fs::write(&file, "fn f() -> u8 { true }\n").expect("write fixture");
    for (choice, colored) in [("never", false), ("always", true)] {
        let output = Command::new(fe_binary())
            .args(["check", "--color", choice])
            .arg(&file)
            .env("TERM", "xterm-256color")
            .env_remove("NO_COLOR")
            .env_remove("CLICOLOR")
            .env_remove("CLICOLOR_FORCE")
            .output()
            .expect("run fe check");
        assert_eq!(output.status.code(), Some(1));
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("error"), "{stderr}");
        assert_eq!(
            stderr.contains('\x1b'),
            colored,
            "--color {choice}:\n{stderr:?}"
        );
    }
}

#[cfg(feature = "cranelift")]
#[test]
fn test_cli_check_uses_the_backend_target() {
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("widths.fe");
    fs::write(
        &source,
        "pub fn main() -> i32 {\n    static_assert(core::size_of<*u8>() == 8)\n    0\n}\n",
    )
    .expect("write source");
    let source = source.to_str().expect("UTF-8 source path");
    let (output, exit_code) = run_fe_main(&["check", "--backend", "native", source]);
    assert_eq!(exit_code, 0, "{output}");
    // EVM pointers are words.
    let (output, exit_code) = run_fe_main(&["check", source]);
    assert_ne!(exit_code, 0, "{output}");
    assert!(output.contains("static assertion failed"), "{output}");
}

// A module's `#![cfg(..)]` drops the modules in its directory too.
#[cfg(feature = "cranelift")]
#[test]
fn test_cli_check_module_cfg_drops_submodules() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    let pointer_bytes =
        |bytes| format!("pub fn f() {{\n    static_assert(core::size_of<*u8>() == {bytes})\n}}\n");
    for (path, content) in [
        (
            "fe.toml",
            "[ingot]\nname = \"cfg_tree\"\nversion = \"0.1.0\"\n".to_string(),
        ),
        ("src/lib.fe", String::new()),
        (
            "src/native.fe",
            "#![cfg(target = \"native\")]\n".to_string(),
        ),
        ("src/native/word.fe", pointer_bytes(8)),
        ("src/native/word/inner.fe", pointer_bytes(8)),
        ("src/evm.fe", "#![cfg(target = \"evm\")]\n".to_string()),
        ("src/evm/word.fe", pointer_bytes(32)),
    ] {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).expect("create dir");
        fs::write(path, content).expect("write fixture");
    }
    let root = root.to_str().expect("UTF-8 ingot path");
    for args in [
        &["check", root][..],
        &["check", "--backend", "native", root],
    ] {
        let (output, exit_code) = run_fe_main(args);
        assert_eq!(exit_code, 0, "{args:?}:\n{output}");
    }
}

#[cfg(all(feature = "cranelift", feature = "wasm"))]
#[test]
fn test_cli_check_usize_literals_fit_the_backend_target() {
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("literals.fe");
    fs::write(
        &source,
        "pub fn beyond_32_bits() -> usize {\n    0x100000000\n}\n\npub fn beyond_64_bits() -> isize {\n    0x8000000000000000\n}\n\npub fn length(_ a: [u8; 0x100000001 - 1]) {}\n",
    )
    .expect("write source");
    let source = source.to_str().expect("UTF-8 source path");
    let (output, exit_code) = run_fe_main(&["check", source]);
    assert_eq!(exit_code, 0, "{output}");
    let (output, exit_code) = run_fe_main(&["check", "--backend", "native", source]);
    assert_ne!(exit_code, 0, "{output}");
    assert!(
        output.contains("`9223372036854775808` is not representable in `isize`")
            && !output.contains("`4294967296`")
            && !output.contains("`4294967297`"),
        "{output}"
    );
    let (output, exit_code) = run_fe_main(&["check", "--backend", "wasm", source]);
    assert_ne!(exit_code, 0, "{output}");
    assert!(
        output.contains("`4294967296` is not representable in `usize`")
            && output.contains("`4294967297` is not representable in `usize`"),
        "{output}"
    );
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
fn test_cli_check_ingot_nested_in_its_dependency() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    let write = |path: &str, text: &str| {
        let path = root.join(path);
        fs::create_dir_all(path.parent().expect("parent")).expect("create dirs");
        fs::write(path, text).expect("write fixture");
    };
    write(
        "parent/fe.toml",
        "[ingot]\nname = \"parent\"\nversion = \"0.1.0\"\n",
    );
    write(
        "parent/src/lib.fe",
        "pub use inner::answer\npub fn direct() -> u8 { 1 }\n",
    );
    write("parent/src/inner.fe", "pub fn answer() -> u8 { 42 }\n");
    let uses =
        "pub fn main() -> u8 { parent::answer() + parent::inner::answer() + parent::direct() }\n";
    // An ingot inside the directory tree of the ingot it depends on, as a
    // tool or example of a library would be.
    write(
        "parent/tools/child/fe.toml",
        "[ingot]\nname = \"child\"\nversion = \"0.1.0\"\n\n[dependencies]\nparent = { path = \"../..\" }\n",
    );
    write("parent/tools/child/src/lib.fe", uses);
    write(
        "sibling/fe.toml",
        "[ingot]\nname = \"sibling\"\nversion = \"0.1.0\"\n\n[dependencies]\nparent = { path = \"../parent\" }\n",
    );
    write("sibling/src/lib.fe", uses);
    // The same layout as members of a workspace.
    write(
        "workspace/fe.toml",
        "[workspace]\nname = \"nested\"\nversion = \"0.1.0\"\nmembers = [\n  { path = \"lib\", name = \"parent\" },\n  { path = \"lib/tools/child\", name = \"child\" },\n]\n",
    );
    write(
        "workspace/lib/fe.toml",
        "[ingot]\nname = \"parent\"\nversion = \"0.1.0\"\n",
    );
    write(
        "workspace/lib/src/lib.fe",
        "pub use inner::answer\npub fn direct() -> u8 { 1 }\n",
    );
    write(
        "workspace/lib/src/inner.fe",
        "pub fn answer() -> u8 { 42 }\n",
    );
    write(
        "workspace/lib/tools/child/fe.toml",
        "[ingot]\nname = \"child\"\nversion = \"0.1.0\"\n\n[dependencies]\nparent = true\n",
    );
    write("workspace/lib/tools/child/src/lib.fe", uses);

    for target in ["parent/tools/child", "sibling", "parent", "workspace"] {
        let path = root.join(target);
        let (output, exit_code) = run_fe_check(path.to_str().expect("fixture path utf8"));
        assert_eq!(exit_code, 0, "`fe check {target}` failed:\n{output}");
    }
}

#[test]
fn test_cli_check_assoc_item_ignores_traits_of_dependencies_not_in_scope() {
    // A dependency implements its own trait with an associated const of the
    // same name for `String<N>`. Without importing it, `Self::SIZE` in the
    // root ingot's impl must not become ambiguous.
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    fs::create_dir_all(root.join("app/src")).unwrap();
    fs::create_dir_all(root.join("dep/src")).unwrap();
    fs::write(
        root.join("fe.toml"),
        "[workspace]\nname = \"scope\"\nversion = \"0.1.0\"\nmembers = [{ path = \"app\", name = \"app\" }, { path = \"dep\", name = \"dep\" }]\n",
    )
    .unwrap();
    fs::write(
        root.join("app/fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n[dependencies]\ndep = true\n",
    )
    .unwrap();
    fs::write(
        root.join("dep/fe.toml"),
        "[ingot]\nname = \"dep\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    let trait_src = |name: &str| {
        format!(
            "pub trait {name} {{\n    const SIZE: usize\n    fn bytes(self) -> [u8; Self::SIZE]\n}}\n\
             impl<const LEN: usize> {name} for String<LEN> {{\n    const SIZE: usize = LEN\n    \
             fn bytes(self) -> [u8; Self::SIZE] {{ self.as_bytes().to_array() }}\n}}\n"
        )
    };
    fs::write(root.join("dep/src/lib.fe"), trait_src("Key")).unwrap();
    fs::write(
        root.join("app/src/lib.fe"),
        format!(
            "{}pub fn first() -> u8 {{ dep::Key::bytes(\"ab\")[0] }}\n",
            trait_src("Literal")
        ),
    )
    .unwrap();

    let (output, exit_code) = run_fe_check(root.to_str().expect("utf8 path"));
    assert_eq!(exit_code, 0, "expected a clean check:\n{output}");
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

#[test]
fn test_cli_build_generic_oversized_size_reports_error_instead_of_panicking() {
    let temp = tempdir().expect("tempdir");
    let file = temp.path().join("generic_oversized_size.fe");
    fs::write(
        &file,
        r#"
fn type_size<T>() -> usize {
    core::size_of<T>()
}

fn trigger() -> usize {
    type_size<[[u256; 0x80000000]; 0x80000000]>()
}
"#,
    )
    .expect("write fixture");

    let (output, exit_code) = run_fe_command("build", file.to_str().expect("fixture path utf8"));
    assert_eq!(exit_code, 1, "expected build failure:\n{output}");
    assert!(
        output.contains("exceeds the supported 64-bit raw-memory layout size"),
        "expected oversized type error instead of panic:\n{output}"
    );
    assert!(
        !output.contains("panicked at"),
        "unexpected panic:\n{output}"
    );
}

#[test]
fn test_cli_wide_enum_size_uses_widened_tag() {
    let temp = tempdir().expect("tempdir");
    let file = temp.path().join("wide_enum_size.fe");
    let variants = (0..257)
        .map(|index| format!("    V{index},\n"))
        .collect::<String>();
    fs::write(
        &file,
        format!(
            r#"
enum Wide {{
{variants}}}

#[test]
fn test_wide_enum_size() {{
    assert(core::size_of<Wide>() == 2)
}}
"#
        ),
    )
    .expect("write fixture");

    let (output, exit_code) = run_fe_command("test", file.to_str().expect("fixture path utf8"));
    assert_eq!(
        exit_code, 0,
        "expected wide enum size test to pass:\n{output}"
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
    cmd.args(args)
        .env("NO_COLOR", "1")
        .env_remove("FE_BORROWCK_PROFILE")
        .env_remove("FE_BORROWCK_SUBGRAPHS");
    for (key, value) in extra_env {
        cmd.env(key, value);
    }
    if let Some(dir) = cwd {
        cmd.current_dir(dir);
    }
    let output = cmd
        .output()
        .unwrap_or_else(|_| panic!("Failed to run fe {args:?}"));

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
            var cursor = key
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
fn test_cli_build_emit_sizes_accounts_for_every_code_byte() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/emit_abi/abi_contract.fe");
    let fixture_path_str = fixture_path.to_str().expect("fixture path utf8");

    let temp = tempdir().expect("tempdir");
    let build = |name: &str, emit: &str| {
        let out_dir = temp.path().join(name);
        let out_dir_str = out_dir.to_string_lossy().to_string();
        let (output, exit_code) = run_fe_main(&[
            "build",
            "--emit",
            emit,
            "--out-dir",
            out_dir_str.as_str(),
            fixture_path_str,
        ]);
        assert_eq!(exit_code, 0, "fe build failed:\n{output}");
        (out_dir, output)
    };
    let (sized, output) = build("sized", "runtime-bytecode,sizes");
    let (plain, _) = build("plain", "runtime-bytecode");

    let runtime = fs::read_to_string(sized.join("Foo.runtime.bin")).expect("read runtime");
    assert_eq!(
        runtime,
        fs::read_to_string(plain.join("Foo.runtime.bin")).expect("read runtime"),
        "emitting sizes changed the runtime bytecode"
    );
    assert!(
        !sized.join("Foo.bin").exists(),
        "unexpected deploy artifact"
    );
    assert!(
        output.contains("Foo.sizes.txt"),
        "unexpected output:\n{output}"
    );

    // Each section header gives the section's bytes and code bytes; the
    // function lines below it add up to the code bytes.
    let report = fs::read_to_string(sized.join("Foo.sizes.txt")).expect("read sizes");
    let mut sections: Vec<(&str, usize, usize, usize)> = Vec::new();
    let mut functions = Vec::new();
    for line in report.lines() {
        if let Some((section, rest)) = line.split_once(": ") {
            let numbers: Vec<usize> = rest
                .split(|c: char| !c.is_ascii_digit())
                .filter(|word| !word.is_empty())
                .map(|word| word.parse().expect("byte count"))
                .collect();
            sections.push((section, numbers[0], numbers[1], 0));
        } else {
            let (bytes, name) = line.trim_start().split_once(' ').expect("function line");
            sections.last_mut().expect("section header").3 +=
                bytes.parse::<usize>().expect("function bytes");
            functions.push(name);
        }
    }
    let names: Vec<_> = sections.iter().map(|section| section.0).collect();
    assert_eq!(
        names,
        ["runtime", "creation"],
        "unexpected report:\n{report}"
    );
    assert_eq!(sections[0].1, runtime.trim().len() / 2, "report:\n{report}");
    for (section, _, code, function_bytes) in &sections {
        assert_eq!(code, function_bytes, "{section} code bytes:\n{report}");
    }
    assert!(
        functions.contains(&"contract_runtime_root_Foo"),
        "missing runtime root:\n{report}"
    );
}

#[cfg(all(feature = "cranelift", native_host))]
#[test]
fn test_cli_build_native_executable_uses_main_return_as_exit_code() {
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("native_exit.fe");
    fs::write(&source, "pub fn main() -> i32 { 42 }\n").expect("write native source");
    let out_dir = temp.path().join("out");

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--backend",
        "native",
        "--emit",
        "ir,executable",
        "--out-dir",
        out_dir.to_str().expect("UTF-8 output path"),
        source.to_str().expect("UTF-8 source path"),
    ]);
    assert_eq!(exit_code, 0, "fe native build failed:\n{output}");

    let executable = out_dir.join("native_exit");
    assert!(executable.is_file(), "missing native executable:\n{output}");
    assert!(
        out_dir.join("native_exit.native.sona").is_file(),
        "missing native IR:\n{output}"
    );
    let status = Command::new(&executable)
        .status()
        .expect("run native executable");
    assert_eq!(status.code(), Some(42));
}

#[cfg(all(feature = "cranelift", native_host))]
#[test]
fn test_cli_build_native_executes_representative_programs_at_o0_and_o1() {
    let fixture_dir =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/cli_output/native");
    for name in [
        "arithmetic",
        "control_flow",
        "aggregates",
        "wide_integer",
        "generic_reachability",
        "entry_name_collision",
    ] {
        let source = fs::read_to_string(fixture_dir.join(format!("{name}.fe")))
            .expect("read native fixture");
        for level in ["0", "1"] {
            assert_eq!(
                native_exit_code(&source, level),
                Some(0),
                "native program {name} failed at O{level}"
            );
        }
    }
}

#[cfg(all(feature = "cranelift", native_host))]
fn native_exit_code(source: &str, level: &str) -> Option<i32> {
    let temp = tempdir().expect("tempdir");
    let executable = build_native_program(source, level, temp.path());
    native_program_exit_code(&executable, &[], level)
}

#[cfg(all(feature = "cranelift", native_host))]
fn build_native_program(source: &str, level: &str, temp: &Path) -> PathBuf {
    let source_path = temp.join("program.fe");
    fs::write(&source_path, source).expect("write native source");
    let out_dir = temp.join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--backend",
        "native",
        "-O",
        level,
        "--out-dir",
        out_dir.to_str().expect("UTF-8 output path"),
        source_path.to_str().expect("UTF-8 source path"),
    ]);
    assert_eq!(
        exit_code, 0,
        "fe native build failed at O{level}:\n{output}"
    );
    out_dir.join("program")
}

#[cfg(all(feature = "cranelift", native_host))]
fn native_program_exit_code(executable: &Path, args: &[&str], level: &str) -> Option<i32> {
    test_utils::skip_crash_reports();
    let mut child = Command::new(executable)
        .args(args)
        .spawn()
        .expect("run native executable");
    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
        if let Some(status) = child.try_wait().expect("poll native executable") {
            return status.code();
        }
        if Instant::now() >= deadline {
            child.kill().expect("kill timed out native executable");
            child.wait().expect("reap native executable");
            panic!("native program {args:?} did not finish within 10 seconds at O{level}");
        }
        thread::sleep(Duration::from_millis(10));
    }
}

#[cfg(all(feature = "cranelift", native_host))]
#[test]
fn test_cli_native_trait_provider_repeated_calls_retain_state() {
    for copy_impl in ["impl Copy for Counter {}", ""] {
        let source = format!(
            r#"
trait Tick {{ fn next(mut self) -> i32 }}
struct Counter {{ value: i32 }}
{copy_impl}
impl Tick for Counter {{
    fn next(mut self) -> i32 {{
        self.value += 1
        self.value
    }}
}}
fn counter() -> Counter {{ Counter {{ value: 0 }} }}
fn next() -> i32 uses (tick: mut Tick) {{ tick.next() }}
pub fn main() -> i32 {{
    with (Tick = counter()) {{ next() + next() }}
}}
"#
        );
        for level in ["0", "2"] {
            assert_eq!(
                native_exit_code(&source, level),
                Some(3),
                "{copy_impl}, O{level}"
            );
        }
    }
}

#[cfg(all(feature = "cranelift", native_host))]
#[test]
fn test_cli_native_trait_provider_source_capture() {
    let source = include_str!("fixtures/fe_test/trait_provider_identity.fe");
    for level in ["0", "2"] {
        assert_eq!(native_exit_code(source, level), Some(42), "O{level}");
    }
}

#[cfg(all(feature = "cranelift", native_host))]
#[test]
fn test_cli_build_native_unchecked_div_rem_integer_widths() {
    for bits in [8, 16, 32, 64, 128, 256] {
        let sign_bit = bits - 1;
        let source = format!(
            r#"
#[arithmetic(unchecked)]
fn signed(value: i{bits}, divisor: i{bits}, quotient: i{bits}, remainder: i{bits}) -> bool {{
    value / divisor == quotient && value % divisor == remainder
}}

#[arithmetic(unchecked)]
fn unsigned(value: u{bits}, divisor: u{bits}, quotient: u{bits}, remainder: u{bits}) -> bool {{
    value / divisor == quotient && value % divisor == remainder
}}

fn checked_rem(value: i{bits}, divisor: i{bits}) -> i{bits} {{
    value % divisor
}}

pub fn main() -> i32 {{
    let min: i{bits} = 1 << {sign_bit}
    if !signed(value: min, divisor: -1, quotient: min, remainder: 0) {{ return 1 }}
    if !signed(value: -17, divisor: 5, quotient: -3, remainder: -2) {{ return 2 }}
    if !signed(value: 17, divisor: -5, quotient: -3, remainder: 2) {{ return 3 }}
    if !unsigned(value: 17, divisor: 5, quotient: 3, remainder: 2) {{ return 4 }}
    if checked_rem(value: min, divisor: -1) != 0 {{ return 5 }}
    0
}}
"#
        );
        for level in ["0", "1"] {
            assert_eq!(
                native_exit_code(&source, level),
                Some(0),
                "unchecked {bits}-bit division/remainder failed at O{level}"
            );
        }
    }
}

#[cfg(all(feature = "cranelift", native_host))]
#[test]
fn test_cli_build_native_arithmetic_traps() {
    let mut cases = Vec::new();
    for bits in [8, 128, 256] {
        let sign_bit = bits - 1;
        let min = format!("1 << {sign_bit}");
        for (mode, prefix, op, value, rhs) in [
            ("checked", "u", "/", "17", "0"),
            ("checked", "u", "%", "17", "0"),
            ("checked", "i", "/", "-17", "0"),
            ("checked", "i", "%", "-17", "0"),
            ("checked", "i", "/", min.as_str(), "-1"),
            ("unchecked", "u", "/", "17", "0"),
            ("unchecked", "u", "%", "17", "0"),
            ("unchecked", "i", "/", "-17", "0"),
            ("unchecked", "i", "%", "-17", "0"),
        ] {
            cases.push((
                mode,
                format!("{prefix}{bits}"),
                op,
                value.to_owned(),
                rhs.to_owned(),
            ));
        }
        for (prefix, base, exponent) in [("u", 2, bits), ("i", 2, sign_bit), ("i", 3, -1)] {
            cases.push((
                "checked",
                format!("{prefix}{bits}"),
                "**",
                base.to_string(),
                exponent.to_string(),
            ));
        }
    }
    assert_eq!(cases.len(), 36);

    let mut source = String::from("use std::env::Args\n");
    for (index, (mode, ty, op, _, _)) in cases.iter().enumerate() {
        source.push_str(&format!(
            "#[arithmetic({mode})]\nfn calculate_{index}(value: {ty}, rhs: {ty}) -> {ty} {{ value {op} rhs }}\n"
        ));
    }
    source.push_str(
        r#"
pub fn main() -> i32 uses (args: Args) {
    if args.count() != 2 { return 97 }
    let arg = args.get(1)
    if arg.len() != 1 { return 97 }
    let selector = arg.span()[0]
"#,
    );
    let selectors: Vec<_> = cases
        .iter()
        .enumerate()
        .map(|(index, (_, _, _, value, rhs))| {
            let selector = b'A' + u8::try_from(index).expect("case selector fits u8");
            source.push_str(&format!(
                "    if selector == {selector} {{ if calculate_{index}(value: {value}, rhs: {rhs}) == 0 {{ return 0 }} else {{ return 1 }} }}\n"
            ));
            char::from(selector).to_string()
        })
        .collect();
    source.push_str("    if selector == 48 { 0 } else { 97 }\n}\n");

    // Each exceptional input still runs in a separate process, sharing one
    // compiled executable per optimization level instead of rebuilding it.
    for level in ["0", "1"] {
        let temp = tempdir().expect("tempdir");
        let executable = build_native_program(&source, level, temp.path());
        for (args, expected) in [
            (&[][..], 97),
            (&["0"][..], 0),
            (&["1"][..], 97),
            (&[""][..], 97),
        ] {
            assert_eq!(
                native_program_exit_code(&executable, args, level),
                Some(expected),
                "native arithmetic selector control {args:?} failed at O{level}"
            );
        }
        for ((mode, ty, op, value, rhs), selector) in cases.iter().zip(&selectors) {
            assert_eq!(
                native_program_exit_code(&executable, &[selector], level),
                None,
                "{mode} {ty} {value} {op} {rhs} did not trap at O{level}"
            );
        }
    }
}

fn power_edge_source(bits: u16) -> String {
    let sign_bit = bits - 1;
    let signed_exp_bit = bits - 2;
    format!(
        r#"
fn checked_unsigned(base: u{bits}, exponent: u{bits}) -> u{bits} {{ base ** exponent }}
fn checked_signed(base: i{bits}, exponent: i{bits}) -> i{bits} {{ base ** exponent }}
#[arithmetic(unchecked)]
fn wrapping_unsigned(base: u{bits}, exponent: u{bits}) -> u{bits} {{ base ** exponent }}
#[arithmetic(unchecked)]
fn wrapping_signed(base: i{bits}, exponent: i{bits}) -> i{bits} {{ base ** exponent }}

fn verify() -> bool {{
    let min: i{bits} = 1 << {sign_bit}
    let large: u{bits} = 1 << {sign_bit}
    let signed_large: i{bits} = 1 << {signed_exp_bit}
    if checked_unsigned(base: 0, exponent: 0) != 1 {{ return false }}
    if checked_unsigned(base: 200, exponent: 1) != 200 {{ return false }}
    if checked_unsigned(base: 1, exponent: large) != 1 {{ return false }}
    if checked_unsigned(base: 0, exponent: large) != 0 {{ return false }}
    if checked_signed(base: min, exponent: 0) != 1 {{ return false }}
    if checked_signed(base: min, exponent: 1) != min {{ return false }}
    if checked_signed(base: -2, exponent: {sign_bit}) != min {{ return false }}
    if checked_signed(base: -1, exponent: signed_large) != 1 {{ return false }}
    if checked_signed(base: -1, exponent: signed_large + 1) != -1 {{ return false }}
    if wrapping_unsigned(base: 2, exponent: {bits}) != 0 {{ return false }}
    if wrapping_unsigned(base: 3, exponent: 5) != 243 {{ return false }}
    if wrapping_signed(base: -1, exponent: -1) != -1 {{ return false }}
    if wrapping_signed(base: -1, exponent: -2) != 1 {{ return false }}
    if wrapping_signed(base: 2, exponent: -1) != 0 {{ return false }}
    true
}}

#[test]
fn power_edges() {{ assert!(verify()) }}

pub fn main() -> i32 {{ if verify() {{ 0 }} else {{ 1 }} }}
"#
    )
}

#[cfg(all(feature = "cranelift", native_host))]
#[test]
fn test_cli_build_native_power_edges() {
    for bits in [8, 128, 256] {
        let source = power_edge_source(bits);
        for level in ["0", "1"] {
            assert_eq!(
                native_exit_code(&source, level),
                Some(0),
                "{bits}-bit power edge case failed at O{level}"
            );
        }
    }
}

#[test]
fn test_cli_evm_power_edges() {
    let temp = tempdir().expect("tempdir");
    for bits in [8, 128, 256] {
        let path = temp.path().join(format!("power_{bits}.fe"));
        fs::write(&path, power_edge_source(bits)).expect("write power edge cases");
        for level in ["0", "1"] {
            let (output, exit_code) = run_fe_main(&[
                "test",
                "-O",
                level,
                path.to_str().expect("UTF-8 source path"),
            ]);
            assert_eq!(
                exit_code, 0,
                "{bits}-bit EVM power failed at O{level}:\n{output}"
            );
        }
    }
}

#[cfg(all(feature = "cranelift", not(native_host)))]
#[test]
fn test_cli_build_native_rejects_executable_on_unsupported_host() {
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("native_exit.fe");
    fs::write(&source, "pub fn main() -> i32 { 0 }\n").expect("write native source");

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--backend",
        "native",
        source.to_str().expect("UTF-8 source path"),
    ]);
    assert_eq!(exit_code, 1, "expected native host rejection:\n{output}");
    assert!(
        output.contains(&format!(
            "native executable and library output requires one of these hosts: {}",
            common::compilation::Platform::supported_hosts()
        )),
        "unexpected output:\n{output}"
    );
}

#[cfg(feature = "cranelift")]
#[test]
fn test_cli_build_native_rejects_evm_artifacts() {
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("native_exit.fe");
    fs::write(&source, "pub fn main() -> i32 { 0 }\n").expect("write native source");

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--backend",
        "native",
        "--emit",
        "bytecode",
        source.to_str().expect("UTF-8 source path"),
    ]);
    assert_eq!(exit_code, 1, "expected native build rejection:\n{output}");
    assert!(
        output.contains(
            "native backend only supports `--emit executable`, `--emit object`, `--emit staticlib` and `--emit ir`"
        ),
        "unexpected output:\n{output}"
    );
}

#[cfg(all(feature = "cranelift", native_host))]
#[test]
fn test_cli_build_native_library_needs_exports_and_no_executable() {
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("library.fe");
    fs::write(&source, "pub fn main() -> i32 { 0 }\n").expect("write native source");
    let out_dir = temp.path().join("out");
    for (emit, error) in [
        (
            "executable,staticlib",
            "`--emit executable` cannot be combined with `--emit object` or `--emit staticlib`, which build a library without `main`",
        ),
        (
            "staticlib",
            "native library output requires an `#[unsafe(export)]` function",
        ),
    ] {
        let (output, exit_code) = run_fe_main(&[
            "build",
            "--backend",
            "native",
            "--emit",
            emit,
            "--out-dir",
            out_dir.to_str().expect("UTF-8 out path"),
            source.to_str().expect("UTF-8 source path"),
        ]);
        assert_eq!(exit_code, 1, "{emit}: expected rejection:\n{output}");
        assert!(
            output.contains(error),
            "{emit}: unexpected output:\n{output}"
        );
        assert!(!out_dir.join("liblibrary.a").exists(), "{emit}");
    }
}

#[test]
fn test_cli_build_sonatina_rejects_executable_emit() {
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("main.fe");
    fs::write(&source, "pub fn main() -> i32 { 0 }\n").expect("write source");

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "executable",
        source.to_str().expect("UTF-8 source path"),
    ]);
    assert_eq!(
        exit_code, 1,
        "expected executable emit rejection:\n{output}"
    );
    assert!(
        output.contains("`--emit executable` requires `--backend native`"),
        "unexpected output:\n{output}"
    );
}

#[test]
fn test_cli_build_sonatina_rejects_library_emits() {
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("library.fe");
    fs::write(
        &source,
        "#[unsafe(export)]\npub fn answer() -> i64 { 42 }\n",
    )
    .expect("write source");
    for emit in ["object", "staticlib"] {
        let (output, exit_code) = run_fe_main(&[
            "build",
            "--emit",
            emit,
            source.to_str().expect("UTF-8 source path"),
        ]);
        assert_eq!(exit_code, 1, "{emit}: expected rejection:\n{output}");
        assert!(
            output.contains("`--emit object` and `--emit staticlib` require `--backend native`"),
            "{emit}: unexpected output:\n{output}"
        );
    }
}

#[test]
fn test_cli_build_link_inputs_need_native_executables() {
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("host.fe");
    fs::write(&source, "pub fn main() -> i32 { 0 }\n").expect("write source");
    let source = source.to_str().expect("UTF-8 source path");
    let mut requests = vec![
        vec!["build", "--link", "host.o", source],
        vec!["build", "--link-lib", "host", source],
    ];
    if cfg!(feature = "cranelift") {
        requests.push(vec![
            "build",
            "--backend",
            "native",
            "--emit",
            "staticlib",
            "--link",
            "host.o",
            source,
        ]);
    }
    for args in requests {
        let (output, exit_code) = run_fe_main(&args);
        assert_eq!(exit_code, 1, "{args:?}: expected rejection:\n{output}");
        assert!(
            output.contains(
                "`--link` and `--link-lib` require `--backend native` and `--emit executable`"
            ),
            "{args:?}: unexpected output:\n{output}"
        );
    }
}

#[test]
fn test_cli_build_sonatina_rejects_exported_functions() {
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("exports.fe");
    fs::write(
        &source,
        "#[unsafe(export)]\npub fn answer() -> i64 { 42 }\ncontract Counter {}\n",
    )
    .expect("write source");
    let out_dir = temp.path().join("out");

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--out-dir",
        out_dir.to_str().expect("UTF-8 out path"),
        source.to_str().expect("UTF-8 source path"),
    ]);
    assert_eq!(exit_code, 1, "expected export rejection:\n{output}");
    assert!(
        output.contains(
            "`#[unsafe(export)]` requires `--backend native`, but `exports::answer` is exported"
        ),
        "unexpected output:\n{output}"
    );
    assert!(!out_dir.exists(), "a rejected build must write nothing");
}

#[cfg(feature = "sp1")]
#[test]
fn test_cli_build_sp1_ir_needs_no_toolchain_and_elf_preflights_it() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("guest.fe");
    fs::write(&source, include_str!("../../../examples/sp1_increment.fe")).unwrap();
    for emit in ["ir", "elf", "elf,ir"] {
        let out = temp.path().join(emit);
        let result = Command::new(fe_binary())
            .args(["build", "--backend", "sp1", "--emit", emit, "--out-dir"])
            .arg(&out)
            .arg(&source)
            .env_remove("SONATINA_SP1_TOOLCHAIN")
            .output()
            .unwrap();
        let stderr = String::from_utf8_lossy(&result.stderr);
        if emit == "ir" {
            assert!(result.status.success(), "{stderr}");
            let ir = fs::read_to_string(out.join("guest.sp1.sona")).unwrap();
            assert!(ir.contains("riscv64im-succinct-zkvm-elf"));
            assert!(!out.join("guest.elf").exists());
        } else {
            assert_eq!(result.status.code(), Some(1), "{stderr}");
            assert!(stderr.contains("set SONATINA_SP1_TOOLCHAIN"), "{stderr}");
            assert!(!out.exists(), "failed preflight must not write artifacts");
        }
    }
}

#[cfg(feature = "sp1")]
#[test]
fn test_cli_build_sp1_rejects_incompatible_artifacts() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("guest.fe");
    fs::write(&source, "pub fn main() -> i32 { 0 }\n").unwrap();
    for emit in [
        "bytecode",
        "runtime-bytecode",
        "abi",
        "metadata",
        "executable",
    ] {
        let (output, exit_code) = run_fe_main(&[
            "build",
            "--backend",
            "sp1",
            "--emit",
            emit,
            source.to_str().unwrap(),
        ]);
        assert_eq!(exit_code, 1, "{output}");
        assert!(output.contains("SP1 backend only supports"), "{output}");
    }
}

#[test]
fn test_cli_test_rejects_sp1_backend() {
    let (output, exit_code) = run_fe_main(&["test", "--backend", "sp1"]);
    assert_eq!(exit_code, 2, "{output}");
    assert!(output.contains("invalid value 'sp1'"), "{output}");
}

#[test]
fn test_cli_build_sonatina_rejects_elf_emit() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("guest.fe");
    fs::write(&source, "pub fn main() -> i32 { 0 }\n").unwrap();
    let (output, exit_code) = run_fe_main(&["build", "--emit", "elf", source.to_str().unwrap()]);
    assert_eq!(exit_code, 1, "{output}");
    assert!(
        output.contains("`--emit elf` requires `--backend sp1`"),
        "{output}"
    );
}

#[cfg(not(feature = "sp1"))]
#[test]
fn test_cli_sp1_requires_feature() {
    let (output, exit_code) = run_fe_main(&["build", "--backend", "sp1"]);
    assert_eq!(exit_code, 2, "{output}");
    assert!(output.contains("invalid value 'sp1'"), "{output}");
}

#[test]
fn test_cli_build_emit_abi_dyn_string_matches_string_selector() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/emit_abi/abi_dyn_string.fe");
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
    let abi: Value = serde_json::from_str(&fs::read_to_string(&abi_path).expect("read ABI"))
        .expect("parse ABI JSON");

    // A `DynString` field under a `string` selector argument is the matching
    // pairing and must emit ABI type `string`.
    let function = abi
        .as_array()
        .expect("abi array")
        .iter()
        .find(|entry| entry["type"] == "function")
        .expect("function entry");
    assert_eq!(function["name"], "set_name");
    assert_eq!(function["inputs"][0]["name"], "name");
    assert_eq!(function["inputs"][0]["type"], "string");
}

#[test]
fn test_cli_build_emit_abi_enum_field_uses_sol_compat_type() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/emit_abi/abi_enum_field.fe");
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
    let abi: Value = serde_json::from_str(&fs::read_to_string(&abi_path).expect("read ABI"))
        .expect("parse ABI JSON");

    // An enum is represented by the Solidity type its `SolCompat` impl names.
    let function = abi
        .as_array()
        .expect("abi array")
        .iter()
        .find(|entry| entry["type"] == "function")
        .expect("function entry");
    assert_eq!(function["name"], "postOp");
    let input_types: Vec<_> = function["inputs"]
        .as_array()
        .expect("inputs")
        .iter()
        .map(|input| input["type"].as_str().expect("input type"))
        .collect();
    assert_eq!(input_types, ["uint8", "bytes", "uint256", "uint256"]);
    assert_eq!(function["inputs"][0]["name"], "mode");
    assert_eq!(function["outputs"][0]["type"], "uint8");
}

#[test]
fn test_cli_build_emit_abi_leaves_tuple_struct_fields_unnamed() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/emit_abi/abi_tuple_struct.fe");
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
    let abi: Value = serde_json::from_str(&fs::read_to_string(&abi_path).expect("read ABI"))
        .expect("parse ABI JSON");

    // Positional field names only exist for `.0` lookups; like tuple
    // components, the fields are unnamed in the ABI.
    let function = abi
        .as_array()
        .expect("abi array")
        .iter()
        .find(|entry| entry["type"] == "function")
        .expect("function entry");
    let components = function["inputs"][0]["components"]
        .as_array()
        .expect("tuple components");
    assert_eq!(components.len(), 2);
    for component in components {
        assert_eq!(component["name"], "");
        assert_eq!(component["type"], "uint256");
    }
}

#[test]
fn test_cli_build_emit_abi_follows_inherent_const_selector() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/emit_abi/abi_inherent_const_selector.fe");
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
    let abi: Value = serde_json::from_str(&fs::read_to_string(&abi_path).expect("read ABI"))
        .expect("parse ABI JSON");

    // The recv arm must survive ABI generation: the selector signature is
    // resolved by following the inherent const's `sol(...)` body.
    let function = abi
        .as_array()
        .expect("abi array")
        .iter()
        .find(|entry| entry["type"] == "function")
        .expect("function entry derived from inherent-const selector");
    assert_eq!(function["name"], "ping");
    assert_eq!(function["inputs"][0]["type"], "uint256");
}

#[test]
fn test_cli_build_emit_abi_follows_the_called_mode() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/emit_abi/abi_mut_body_events.fe");
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
    let abi: Value = serde_json::from_str(&fs::read_to_string(&abi_path).expect("read ABI"))
        .expect("parse ABI JSON");

    // The recv arm calls `slot` at `mut`, through a `mut?` function: the
    // event and the error of its `mut` body are the contract's.
    let entries = abi.as_array().expect("abi array");
    for (kind, name) in [("event", "Wrote"), ("error", "Overflow")] {
        assert!(
            entries
                .iter()
                .any(|entry| entry["type"] == kind && entry["name"] == name),
            "missing {kind} `{name}`: {abi}"
        );
    }
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
fn test_cli_build_ingot_with_multiple_library_modules() {
    // An ingot with several non-contract "library" modules plus a contract must
    // build the contract without colliding on a synthesized `main` object (one
    // was previously emitted per non-contract module). See the duplicate-`main`
    // regression.
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"dup_main\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    fs::write(
        src_dir.join("lib.fe"),
        "pub use libmod::{self, *}\npub use libmod2::{self, *}\npub use contractmod::{self, *}\n",
    )
    .expect("write lib.fe");
    fs::write(src_dir.join("libmod.fe"), "pub fn helper() -> u256 { 1 }\n").expect("write libmod");
    fs::write(
        src_dir.join("libmod2.fe"),
        "pub fn helper2() -> u256 { 2 }\n",
    )
    .expect("write libmod2");
    fs::write(
        src_dir.join("contractmod.fe"),
        "pub msg AMsg {\n    #[selector = sol(\"foo()\")]\n    Foo -> u256,\n}\n\npub contract A {\n    recv AMsg {\n        Foo -> u256 { 1 }\n    }\n}\n",
    )
    .expect("write contractmod");

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let project_path = temp.path().to_str().expect("project path utf8");

    let (output, exit_code) =
        run_fe_main(&["build", "--out-dir", out_dir_str.as_str(), project_path]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");

    assert!(out_dir.join("A.bin").is_file(), "missing A.bin:\n{output}");
    assert!(
        out_dir.join("A.runtime.bin").is_file(),
        "missing A.runtime.bin:\n{output}"
    );
    // Library modules must not produce deployable `main` artifacts.
    assert!(
        !out_dir.join("main.bin").exists() && !out_dir.join("main.runtime.bin").exists(),
        "library modules should not emit a `main` object:\n{:?}",
        fs::read_dir(&out_dir).map(|d| d
            .filter_map(|e| e.ok().map(|e| e.file_name()))
            .collect::<Vec<_>>())
    );
}

/// Writes a two-ingot project whose app record is generated by a dependency's
/// provider, with an app test and contract that use the generated method.
fn write_generation_package(root: &Path, provider_body: &str, expected_total: u32) {
    let write = |path: &str, text: &str| {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).expect("create dir");
        fs::write(path, text).expect("write file");
    };
    write(
        "lib/fe.toml",
        "[ingot]\nname = \"lib\"\nversion = \"0.1.0\"\n",
    );
    write(
        "lib/src/lib.fe",
        &format!(
            "use core::derive::Derive\nuse core::meta::{{Code, Item, Record, field_count}}\n\npub trait Score {{\n    const fn score() -> usize\n}}\n\npub struct FieldScore {{}}\n\nimpl Derive<trait Score> for FieldScore {{\n    const fn derive(_ record: Record) -> Code<Item> {{\n{provider_body}        quote {{\n            const fn score() -> usize {{ ${{n}} }}\n        }}\n    }}\n}}\n"
        ),
    );
    write(
        "app/fe.toml",
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\nlib = { path = \"../lib\" }\n",
    );
    write(
        "app/src/model.fe",
        "use lib::Score\n\npub struct Pair {\n    pub a: u256,\n    pub b: u256,\n}\n\nimpl Score for Pair with (lib::FieldScore)\n",
    );
    write(
        "app/src/lib.fe",
        &format!(
            "use std::evm::Evm\nuse ingot::model::Pair\nuse lib::Score\n\npub const fn total() -> usize {{\n    Pair::score()\n}}\n\npub msg TotalMsg {{\n    #[selector = sol(\"total()\")]\n    Total -> u256,\n}}\n\npub contract Totals {{\n    recv TotalMsg {{\n        Total -> u256 {{ total() as u256 }}\n    }}\n}}\n\n#[test]\nfn generated_total_matches() uses (evm: mut Evm) {{\n    assert!(total() == {expected_total})\n}}\n"
        ),
    );
}

#[test]
fn test_cli_package_generation_checks_builds_and_tests_the_generated_program() {
    let provider = "    let n = field_count(record)\n";
    let temp = tempdir().expect("tempdir");
    write_generation_package(temp.path(), provider, 2);
    let app = temp.path().join("app");
    let app = app.to_str().expect("utf8 path");

    let (output, exit_code) = run_fe_check(app);
    assert_eq!(exit_code, 0, "fe check failed:\n{output}");

    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", app]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(output.contains("generated_total_matches"), "{output}");

    let out_dir = temp.path().join("out");
    let out_dir = out_dir.to_str().expect("utf8 path");
    let (output, exit_code) = run_fe_main(&["build", "--out-dir", out_dir, app]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
    assert!(
        temp.path().join("out/Totals.bin").is_file(),
        "missing Totals.bin:\n{output}"
    );

    // The test really runs against the generated value: `Pair` has two
    // fields, so expecting three fails.
    let wrong = tempdir().expect("tempdir");
    write_generation_package(wrong.path(), provider, 3);
    let wrong_app = wrong.path().join("app");
    let (output, exit_code) = run_fe_main(&[
        "test",
        "--jobs",
        "1",
        wrong_app.to_str().expect("utf8 path"),
    ]);
    assert_ne!(exit_code, 0, "a wrong expectation must fail:\n{output}");

    // A faulting dependency provider refuses the package before checking.
    let faulting = tempdir().expect("tempdir");
    write_generation_package(
        faulting.path(),
        "    let zero: usize = 0\n    let n = field_count(record) / zero\n",
        2,
    );
    let faulting_app = faulting.path().join("app");
    let (output, exit_code) = run_fe_check(faulting_app.to_str().expect("utf8 path"));
    assert_ne!(exit_code, 0, "{output}");
    assert!(output.contains("division by zero"), "{output}");
}

/// Two files that derive with each other's providers, `Left`'s generated
/// method calling `Right`'s: `fe check` and `fe test` prepare both files in
/// one program, and the test reads the generated value (1 + 1).
#[test]
fn test_cli_mutually_deriving_files_check_and_test() {
    let write_package = |root: &Path, expected_total: u32| {
        let write = |path: &str, text: &str| {
            let path = root.join(path);
            fs::create_dir_all(path.parent().unwrap()).expect("create dir");
            fs::write(path, text).expect("write file");
        };
        write(
            "fe.toml",
            "[ingot]\nname = \"mutual\"\nversion = \"0.1.0\"\n",
        );
        write(
            "src/score.fe",
            "pub trait Score {\n    const fn score() -> usize\n}\n",
        );
        write(
            "src/alpha.fe",
            "use core::derive::Derive\nuse core::meta::{Code, Item, Record, field_count}\nuse ingot::score::Score\nuse ingot::beta::ProviderB\n\npub struct ProviderA {}\nimpl Derive<trait Score> for ProviderA {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let n = field_count(record)\n        quote { const fn score() -> usize { ${n} } }\n    }\n}\npub struct Left { pub only: u8 }\nimpl Score for Left with (ProviderB)\npub const fn left_total() -> usize {\n    Left::score()\n}\n",
        );
        write(
            "src/beta.fe",
            "use core::derive::Derive\nuse core::meta::{Code, Item, Record, field_count}\nuse ingot::score::Score\nuse ingot::alpha::ProviderA\n\npub struct ProviderB {}\nimpl Derive<trait Score> for ProviderB {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let n = field_count(record)\n        quote { const fn score() -> usize { ${n} + Right::score() } }\n    }\n}\npub struct Right { pub only: u8 }\nimpl Score for Right with (ProviderA)\n",
        );
        write(
            "src/lib.fe",
            &format!(
                "use std::evm::Evm\nuse ingot::alpha::left_total\n\n#[test]\nfn mutual_total_matches() uses (evm: mut Evm) {{\n    assert!(left_total() == {expected_total})\n}}\n"
            ),
        );
    };
    let temp = tempdir().expect("tempdir");
    write_package(temp.path(), 2);
    let project = temp.path().to_str().expect("utf8 path");
    let (output, exit_code) = run_fe_check(project);
    assert_eq!(exit_code, 0, "fe check failed:\n{output}");
    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", project]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(output.contains("mutual_total_matches"), "{output}");

    let wrong = tempdir().expect("tempdir");
    write_package(wrong.path(), 3);
    let (output, exit_code) = run_fe_main(&[
        "test",
        "--jobs",
        "1",
        wrong.path().to_str().expect("utf8 path"),
    ]);
    assert_ne!(exit_code, 0, "a wrong expectation must fail:\n{output}");
}

#[test]
fn test_cli_build_ingot_contract_uses_effectful_library_helper() {
    // A contract that calls a helper defined in a non-contract child module, where
    // the helper takes a `uses` effect parameter, must build: the helper is lowered
    // as a dependency of the contract, not validated as a standalone runtime root
    // (which would reject its ordinary effect parameter).
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"effect_helper\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    fs::write(
        src_dir.join("lib.fe"),
        "pub use libmod::{self, *}\n\npub msg AMsg {\n    #[selector = sol(\"foo()\")]\n    Foo -> u256,\n}\n\npub contract A {\n    recv AMsg {\n        Foo -> u256 {\n            var value: u256 = 1\n            with (value) { needs_effect() }\n        }\n    }\n}\n",
    )
    .expect("write lib.fe");
    fs::write(
        src_dir.join("libmod.fe"),
        "pub fn needs_effect() -> u256 uses (value: mut u256) {\n    value += 1\n    value\n}\n",
    )
    .expect("write libmod");

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let project_path = temp.path().to_str().expect("project path utf8");

    let (output, exit_code) =
        run_fe_main(&["build", "--out-dir", out_dir_str.as_str(), project_path]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
    assert!(out_dir.join("A.bin").is_file(), "missing A.bin:\n{output}");
    assert!(
        out_dir.join("A.runtime.bin").is_file(),
        "missing A.runtime.bin:\n{output}"
    );
}

/// A root-module function named `main` is only an ingot executable when it is a
/// valid standalone root (same validation as a single-module build). A `main`
/// with ordinary parameters is not - it must be ignored, not synthesized into a
/// `main` object (which previously panicked on an arg-count mismatch).
#[test]
fn test_cli_build_ingot_root_main_with_params_is_not_executable() {
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"param_main\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    fs::write(
        src_dir.join("lib.fe"),
        "pub use contractmod::{self, *}\n\npub fn main(_ x: u256) {}\n",
    )
    .expect("write lib.fe");
    fs::write(
        src_dir.join("contractmod.fe"),
        "pub msg AMsg {\n    #[selector = sol(\"foo()\")]\n    Foo -> u256,\n}\n\npub contract A {\n    recv AMsg {\n        Foo -> u256 { 1 }\n    }\n}\n",
    )
    .expect("write contractmod");

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let project_path = temp.path().to_str().expect("project path utf8");

    let (output, exit_code) =
        run_fe_main(&["build", "--out-dir", out_dir_str.as_str(), project_path]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
    assert!(out_dir.join("A.bin").is_file(), "missing A.bin:\n{output}");
    assert!(
        !out_dir.join("main.bin").exists() && !out_dir.join("main.runtime.bin").exists(),
        "a `main` with parameters must not produce a `main` object:\n{output}"
    );
}

/// A `#[test]`-attributed `main` in the root module is not an executable entry
/// and must not produce a deployable `main` object during `fe build`.
#[test]
fn test_cli_build_ingot_root_test_main_is_not_executable() {
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"test_main\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    fs::write(
        src_dir.join("lib.fe"),
        "pub use contractmod::{self, *}\n\n#[test]\nfn main() {}\n",
    )
    .expect("write lib.fe");
    fs::write(
        src_dir.join("contractmod.fe"),
        "pub msg AMsg {\n    #[selector = sol(\"foo()\")]\n    Foo -> u256,\n}\n\npub contract A {\n    recv AMsg {\n        Foo -> u256 { 1 }\n    }\n}\n",
    )
    .expect("write contractmod");

    let out_dir = temp.path().join("out");
    let out_dir_str = out_dir.to_string_lossy().to_string();
    let project_path = temp.path().to_str().expect("project path utf8");

    let (output, exit_code) =
        run_fe_main(&["build", "--out-dir", out_dir_str.as_str(), project_path]);
    assert_eq!(exit_code, 0, "fe build failed:\n{output}");
    assert!(out_dir.join("A.bin").is_file(), "missing A.bin:\n{output}");
    assert!(
        !out_dir.join("main.bin").exists() && !out_dir.join("main.runtime.bin").exists(),
        "a `#[test]` `main` must not produce a `main` object:\n{output}"
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
fn test_cli_build_from_metadata_preserves_default_and_explicit_emits() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    write_app_with_path_dependency(root);

    let out_dir = root.join("app/out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "-O2",
        "--emit",
        "metadata,bytecode,runtime-bytecode,abi",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        root.join("app").to_str().expect("app utf8"),
    ]);
    assert_eq!(exit_code, 0, "original build failed:\n{output}");
    let metadata_path = out_dir.join("Foo.metadata.json");
    for emit in [None, Some("runtime-bytecode")] {
        let recon = tempdir().expect("recon tempdir");
        let recon_out = recon.path().join("out");
        let mut args = vec![
            "build",
            "--from-metadata",
            metadata_path.to_str().expect("metadata utf8"),
            "--out-dir",
            recon_out.to_str().expect("out utf8"),
        ];
        if let Some(emit) = emit {
            args.extend(["--emit", emit]);
        }
        let (output, exit_code) = run_fe_main(&args);
        assert_eq!(exit_code, 0, "rebuild from metadata failed:\n{output}");
        for artifact in ["Foo.bin", "Foo.runtime.bin", "Foo.abi.json"] {
            let rebuilt = recon_out.join(artifact);
            if emit.is_none() || artifact == "Foo.runtime.bin" {
                assert_eq!(
                    fs::read(&rebuilt).unwrap_or_else(|err| panic!("read {rebuilt:?}: {err}")),
                    fs::read(out_dir.join(artifact)).expect("read original artifact"),
                    "rebuilt {artifact} must match the recorded build"
                );
            } else {
                assert!(!rebuilt.exists(), "unrequested artifact {rebuilt:?}");
            }
        }
    }
}

#[test]
fn test_cli_build_from_metadata_rejects_executable_emit() {
    let temp = tempdir().expect("tempdir");
    let file = temp.path().join("foo.fe");
    fs::write(&file, "pub contract Foo {}\n").expect("write foo.fe");
    let out_dir = temp.path().join("original");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        file.to_str().expect("file utf8"),
    ]);
    assert_eq!(exit_code, 0, "original build failed:\n{output}");
    let metadata_path = out_dir.join("Foo.metadata.json");
    let recon_out = temp.path().join("rebuilt");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        metadata_path.to_str().expect("metadata utf8"),
        "--emit",
        "executable",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
    ]);
    assert_ne!(exit_code, 0, "executable emit should fail:\n{output}");
    assert!(
        output.contains("`--emit executable` requires `--backend native`"),
        "unexpected error:\n{output}"
    );
    assert!(
        !recon_out.exists(),
        "invalid request must not write artifacts"
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

    // Round-trip via `--from-metadata`: rebuild to byte-identical runtime bytecode.
    let original_runtime =
        fs::read_to_string(out_dir.join("Foo.runtime.bin")).expect("read original runtime.bin");
    let metadata_path = out_dir.join("Foo.metadata.json");
    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        metadata_path.to_str().expect("metadata utf8"),
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
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
fn test_cli_build_emit_metadata_disambiguates_dependency_namespace_from_root_source_prefix() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    fs::create_dir_all(root.join("app/src")).expect("create app sources");
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
    let dependency_src = "pub fn helper() -> u256 {\n    return 42\n}\n";
    fs::write(root.join("dep/src/lib.fe"), dependency_src).expect("write dependency source");
    let root_src = "use dep::helper\n\npub msg FooMsg {\n    #[selector = sol(\"run()\")]\n    Run -> u256,\n}\n\npub contract Foo {\n    recv FooMsg {\n        Run -> u256 {\n            helper()\n        }\n    }\n}\n";
    fs::write(root.join("app/src/main.fe"), root_src).expect("write app main source");

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
    assert_eq!(sources["src/main.fe"]["content"], root_src);
    assert_eq!(sources["src-2/src/lib.fe"]["content"], dependency_src);
    let ingots = metadata["settings"]["ingots"]
        .as_array()
        .expect("ingots array");
    let dep = ingots
        .iter()
        .find(|ingot| ingot["name"] == "src")
        .expect("dependency ingot");
    assert_eq!(dep["namespace"], "src-2");
    let root_ingot = ingots
        .iter()
        .find(|ingot| ingot["namespace"] == "")
        .expect("root ingot");
    assert_eq!(root_ingot["dependencies"]["dep"], "src-2");

    let original_runtime =
        fs::read_to_string(out_dir.join("Foo.runtime.bin")).expect("read original runtime.bin");
    let metadata_path = out_dir.join("Foo.metadata.json");
    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        metadata_path.to_str().expect("metadata utf8"),
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
    ]);
    assert_eq!(exit_code, 0, "rebuild from metadata failed:\n{output}");
    let rebuilt_runtime =
        fs::read_to_string(recon_out.join("Foo.runtime.bin")).expect("read rebuilt runtime.bin");
    assert_eq!(original_runtime, rebuilt_runtime);
}

#[test]
fn test_cli_build_from_metadata_reproduces_trusted_unsafe_dependency() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    fs::create_dir_all(root.join("app/src")).expect("create app sources");
    fs::create_dir_all(root.join("dep/src")).expect("create dependency sources");
    fs::write(
        root.join("app/fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\ndep = { path = \"../dep\", allow_unsafe = true }\n",
    )
    .expect("write app fe.toml");
    fs::write(
        root.join("dep/fe.toml"),
        "[ingot]\nname = \"dep\"\nversion = \"1.0.0\"\n",
    )
    .expect("write dependency fe.toml");
    fs::write(
        root.join("dep/src/lib.fe"),
        "use core::abi::{load_word, store_word}\nuse core::ptr\n\npub fn round_trip(_ value: u256) -> u256 {\n    let p = ptr::alloc_bytes(32)\n    unsafe {\n        store_word(ptr: p, value)\n        load_word(p)\n    }\n}\n",
    )
    .expect("write dependency source");
    fs::write(
        root.join("app/src/main.fe"),
        "use dep::round_trip\n\npub msg FooMsg {\n    #[selector = sol(\"run()\")]\n    Run -> u256,\n}\n\npub contract Foo {\n    recv FooMsg {\n        Run -> u256 {\n            round_trip(42)\n        }\n    }\n}\n",
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

    let original_runtime =
        fs::read_to_string(out_dir.join("Foo.runtime.bin")).expect("read original runtime.bin");
    let metadata_path = out_dir.join("Foo.metadata.json");
    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        metadata_path.to_str().expect("metadata utf8"),
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
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

    // Round-trip via `--from-metadata`: the flattened reconstruction must still
    // reproduce the exact bytecode.
    let original_runtime =
        fs::read_to_string(out_dir.join("App.runtime.bin")).expect("read original runtime.bin");
    let metadata_path = out_dir.join("App.metadata.json");
    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        metadata_path.to_str().expect("metadata utf8"),
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
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

#[test]
fn test_cli_build_from_metadata_reads_stdin() {
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    write_app_with_path_dependency(root);

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
    let metadata = fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata");

    // `--from-metadata -` reads the JSON from stdin.
    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main_with_stdin(
        &[
            "build",
            "--from-metadata",
            "-",
            "--emit",
            "runtime-bytecode",
            "--out-dir",
            recon_out.to_str().expect("out utf8"),
        ],
        &metadata,
    );
    assert_eq!(
        exit_code, 0,
        "rebuild from stdin metadata failed:\n{output}"
    );
    let rebuilt_runtime =
        fs::read_to_string(recon_out.join("Foo.runtime.bin")).expect("read rebuilt runtime.bin");
    assert_eq!(
        original_runtime, rebuilt_runtime,
        "runtime bytecode rebuilt from stdin metadata must be byte-identical"
    );
}

#[test]
fn test_cli_build_from_metadata_round_trips_standalone_file() {
    // Standalone-target metadata keys its single source by file basename and
    // records `version: null` for the pseudo-ingot; the reconstruction must
    // still produce a loadable project (source under `src/`, valid fe.toml).
    let temp = tempdir().expect("tempdir");
    let file = temp.path().join("counter.fe");
    fs::write(
        &file,
        "pub msg FooMsg {\n    #[selector = sol(\"run()\")]\n    Run -> u256,\n}\n\npub contract Foo {\n    recv FooMsg {\n        Run -> u256 {\n            42\n        }\n    }\n}\n",
    )
    .expect("write counter.fe");

    let out_dir = temp.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata,runtime-bytecode",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        file.to_str().expect("file utf8"),
    ]);
    assert_eq!(exit_code, 0, "original standalone build failed:\n{output}");
    let original_runtime =
        fs::read_to_string(out_dir.join("Foo.runtime.bin")).expect("read original runtime.bin");

    let metadata_path = out_dir.join("Foo.metadata.json");
    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        metadata_path.to_str().expect("metadata utf8"),
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
    ]);
    assert_eq!(
        exit_code, 0,
        "rebuild from standalone metadata failed:\n{output}"
    );
    let rebuilt_runtime =
        fs::read_to_string(recon_out.join("Foo.runtime.bin")).expect("read rebuilt runtime.bin");
    assert_eq!(
        original_runtime, rebuilt_runtime,
        "runtime bytecode rebuilt from standalone metadata must be byte-identical"
    );
}

#[test]
fn test_cli_build_from_metadata_defaults_to_compilation_target_contract() {
    let temp = tempdir().expect("tempdir");
    let src_dir = temp.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"two_contracts\"\nversion = \"0.1.0\"\n",
    )
    .expect("write fe.toml");
    fs::write(
        src_dir.join("lib.fe"),
        "pub contract Foo {\n}\n\npub contract Bar {\n}\n",
    )
    .expect("write lib.fe");

    let out_dir = temp.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        temp.path().to_str().expect("project utf8"),
    ]);
    assert_eq!(exit_code, 0, "original build failed:\n{output}");

    // Foo's metadata records `compilationTarget: Foo`; the rebuild must only
    // produce Foo's artifacts even though the sources define Bar as well.
    let metadata_path = out_dir.join("Foo.metadata.json");
    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        metadata_path.to_str().expect("metadata utf8"),
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
    ]);
    assert_eq!(exit_code, 0, "rebuild from metadata failed:\n{output}");
    assert!(
        recon_out.join("Foo.runtime.bin").is_file(),
        "missing Foo artifact:\n{output}"
    );
    assert!(
        !recon_out.join("Bar.runtime.bin").exists(),
        "Bar must not be built by default:\n{output}"
    );

    // `--contract` overrides the recorded compilation target.
    let recon_out_bar = recon.path().join("out-bar");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        metadata_path.to_str().expect("metadata utf8"),
        "--contract",
        "Bar",
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out_bar.to_str().expect("out utf8"),
    ]);
    assert_eq!(exit_code, 0, "rebuild with --contract failed:\n{output}");
    assert!(
        recon_out_bar.join("Bar.runtime.bin").is_file(),
        "missing Bar artifact:\n{output}"
    );
}

#[test]
fn test_cli_build_from_metadata_optimize_override_warns() {
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
    assert_eq!(exit_code, 0, "original build failed:\n{output}");
    let metadata_path = out_dir.join("Foo.metadata.json");

    // The metadata records level `1` (the default); an explicit `-O 0` wins,
    // with a warning that the rebuilt bytecode deviates.
    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        metadata_path.to_str().expect("metadata utf8"),
        "-O",
        "0",
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
    ]);
    assert_eq!(exit_code, 0, "rebuild with -O override failed:\n{output}");
    assert!(
        output.contains("Warning: -O 0 overrides optimizer level 1"),
        "expected override warning:\n{output}"
    );
    assert!(
        recon_out.join("Foo.runtime.bin").is_file(),
        "missing rebuilt artifact:\n{output}"
    );
}

#[test]
fn test_cli_build_from_metadata_warns_on_compiler_version_mismatch() {
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
    assert_eq!(exit_code, 0, "original build failed:\n{output}");

    let mut metadata: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");
    metadata["compiler"]["version"] = "0.0.1".into();
    let edited_path = root.join("edited.metadata.json");
    fs::write(&edited_path, metadata.to_string()).expect("write edited metadata");

    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        edited_path.to_str().expect("metadata utf8"),
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
    ]);
    assert_eq!(exit_code, 0, "version mismatch must not abort:\n{output}");
    assert!(
        output.contains("Warning: metadata was produced by fe 0.0.1"),
        "expected version mismatch warning:\n{output}"
    );
}

#[test]
fn test_cli_build_from_metadata_warns_on_compiler_commit_mismatch() {
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
    assert_eq!(exit_code, 0, "original build failed:\n{output}");

    let mut metadata: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");
    if metadata["compiler"]["commit"].as_str().is_none() {
        // The test binary was built without git information, so the rebuild
        // has no commit to compare against.
        return;
    }
    metadata["compiler"]["commit"] = "deadbee".into();
    let edited_path = root.join("edited.metadata.json");
    fs::write(&edited_path, metadata.to_string()).expect("write edited metadata");

    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        edited_path.to_str().expect("metadata utf8"),
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
    ]);
    assert_eq!(exit_code, 0, "commit mismatch must not abort:\n{output}");
    assert!(
        output.contains("Warning: metadata was produced by fe commit deadbee"),
        "expected commit mismatch warning:\n{output}"
    );
}

#[test]
fn test_cli_build_from_metadata_honors_compilation_target_source_path() {
    // Two modules in the same ingot each define a contract named `Foo`, so
    // their artifacts are qualified by module path (`lib__a__Foo`,
    // `lib__b__Foo`). `settings.compilationTarget` records the source file and
    // the contract's path in it; the rebuild must produce the contract of that
    // module.
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    fs::create_dir_all(root.join("app/src")).expect("create app/src");
    fs::write(
        root.join("app/fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n",
    )
    .expect("write app/fe.toml");
    let contract_source = |value: &str| {
        format!(
            "pub msg FooMsg {{\n    #[selector = sol(\"run()\")]\n    Run -> u256,\n}}\n\npub contract Foo {{\n    recv FooMsg {{\n        Run -> u256 {{\n            {value}\n        }}\n    }}\n}}\n"
        )
    };
    fs::write(root.join("app/src/a.fe"), contract_source("1")).expect("write app/src/a.fe");
    fs::write(root.join("app/src/b.fe"), contract_source("2")).expect("write app/src/b.fe");

    let out_dir = root.join("app/out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        root.join("app").to_str().expect("app utf8"),
    ]);
    assert_eq!(exit_code, 0, "original build failed:\n{output}");

    let metadata: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("lib__a__Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");
    assert_eq!(
        metadata["settings"]["compilationTarget"],
        serde_json::json!({ "src/a.fe": "Foo" }),
        "metadata must record the defining source file"
    );

    let recon = tempdir().expect("recon tempdir");
    let recon_out = recon.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        out_dir
            .join("lib__a__Foo.metadata.json")
            .to_str()
            .expect("metadata utf8"),
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        recon_out.to_str().expect("out utf8"),
    ]);
    assert_eq!(
        exit_code, 0,
        "rebuild must honor the recorded source path:\n{output}"
    );
    let rebuilt_runtime = fs::read_to_string(recon_out.join("lib__a__Foo.runtime.bin"))
        .expect("read rebuilt runtime.bin");

    // The rebuilt bytecode must be the `src/a.fe` contract: it must match a
    // reference build of an identical ingot containing only that module.
    let reference = tempdir().expect("reference tempdir");
    fs::create_dir_all(reference.path().join("app/src")).expect("create reference app/src");
    fs::write(
        reference.path().join("app/fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n",
    )
    .expect("write reference fe.toml");
    fs::write(reference.path().join("app/src/a.fe"), contract_source("1"))
        .expect("write reference a.fe");
    let reference_out = reference.path().join("out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "runtime-bytecode",
        "--out-dir",
        reference_out.to_str().expect("out utf8"),
        reference.path().join("app").to_str().expect("app utf8"),
    ]);
    assert_eq!(exit_code, 0, "reference build failed:\n{output}");
    let reference_runtime = fs::read_to_string(reference_out.join("Foo.runtime.bin"))
        .expect("read reference runtime.bin");
    assert_eq!(
        rebuilt_runtime, reference_runtime,
        "rebuild must produce the contract recorded in compilationTarget"
    );
}

#[test]
fn test_cli_build_from_metadata_rejects_unresolvable_compilation_target() {
    // The recorded `compilationTarget` must resolve exactly; a stale or edited
    // path must fail the rebuild instead of silently falling back to building
    // a same-named contract from a different source file.
    let temp = tempdir().expect("tempdir");
    let root = temp.path();
    fs::create_dir_all(root.join("app/src")).expect("create app/src");
    fs::write(
        root.join("app/fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n",
    )
    .expect("write app/fe.toml");
    fs::write(root.join("app/src/a.fe"), "pub contract Foo {\n}\n").expect("write app/src/a.fe");
    fs::write(root.join("app/src/b.fe"), "pub contract Bar {\n}\n").expect("write app/src/b.fe");

    let out_dir = root.join("app/out");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--emit",
        "metadata",
        "--out-dir",
        out_dir.to_str().expect("out utf8"),
        root.join("app").to_str().expect("app utf8"),
    ]);
    assert_eq!(exit_code, 0, "original build failed:\n{output}");
    let metadata: Value = serde_json::from_str(
        &fs::read_to_string(out_dir.join("Foo.metadata.json")).expect("read metadata"),
    )
    .expect("parse metadata");

    let rebuild_with_target = |target: Value| {
        let mut edited = metadata.clone();
        edited["settings"]["compilationTarget"] = target;
        let edited_path = root.join("edited.metadata.json");
        fs::write(&edited_path, edited.to_string()).expect("write edited metadata");
        let recon = tempdir().expect("recon tempdir");
        let recon_out = recon.path().join("out");
        let (output, exit_code) = run_fe_main(&[
            "build",
            "--from-metadata",
            edited_path.to_str().expect("metadata utf8"),
            "--emit",
            "runtime-bytecode",
            "--out-dir",
            recon_out.to_str().expect("out utf8"),
        ]);
        let emitted = recon_out.join("Foo.runtime.bin").exists();
        (output, exit_code, emitted)
    };

    // The recorded source path does not exist in the metadata sources.
    let (output, exit_code, emitted) =
        rebuild_with_target(serde_json::json!({ "src/missing.fe": "Foo" }));
    assert_eq!(exit_code, 1, "expected exit 1:\n{output}");
    assert!(
        output.contains("`src/missing.fe` recorded in `settings.compilationTarget` does not exist"),
        "expected missing-path error:\n{output}"
    );
    assert!(!emitted, "no artifact must be emitted:\n{output}");

    // The recorded source path exists but its module defines a different contract.
    let (output, exit_code, emitted) =
        rebuild_with_target(serde_json::json!({ "src/b.fe": "Foo" }));
    assert_eq!(exit_code, 1, "expected exit 1:\n{output}");
    assert!(
        output.contains("`src/b.fe` does not define the contract `Foo`"),
        "expected wrong-module error:\n{output}"
    );
    assert!(!emitted, "no artifact must be emitted:\n{output}");

    // `compilationTarget` must hold exactly one source-path/contract-name pair.
    let (output, exit_code, emitted) =
        rebuild_with_target(serde_json::json!({ "src/a.fe": "Foo", "src/b.fe": "Bar" }));
    assert_eq!(exit_code, 1, "expected exit 1:\n{output}");
    assert!(
        output.contains("must contain exactly one source-path/contract-name pair"),
        "expected exactly-one-pair error:\n{output}"
    );
    assert!(!emitted, "no artifact must be emitted:\n{output}");

    // A non-string contract name is rejected the same way.
    let (output, exit_code, emitted) = rebuild_with_target(serde_json::json!({ "src/a.fe": 1 }));
    assert_eq!(exit_code, 1, "expected exit 1:\n{output}");
    assert!(
        output.contains("must contain exactly one source-path/contract-name pair"),
        "expected non-string-name error:\n{output}"
    );
    assert!(!emitted, "no artifact must be emitted:\n{output}");
}

#[test]
fn test_cli_build_from_metadata_rejects_invalid_input() {
    let temp = tempdir().expect("tempdir");

    // Broken JSON.
    let broken = temp.path().join("broken.metadata.json");
    fs::write(&broken, "{ not json").expect("write broken metadata");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        broken.to_str().expect("path utf8"),
    ]);
    assert_eq!(exit_code, 1, "expected exit 1:\n{output}");
    assert!(
        output.contains("Failed to parse metadata JSON"),
        "expected JSON parse error:\n{output}"
    );

    // Missing `sources`.
    let no_sources = temp.path().join("no_sources.metadata.json");
    fs::write(
        &no_sources,
        r#"{"version":1,"language":"Fe","settings":{"ingots":[]}}"#,
    )
    .expect("write metadata without sources");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        no_sources.to_str().expect("path utf8"),
    ]);
    assert_eq!(exit_code, 1, "expected exit 1:\n{output}");
    assert!(
        output.contains("missing the `sources` object"),
        "expected missing-sources error:\n{output}"
    );

    // Wrong language.
    let wrong_language = temp.path().join("wrong_language.metadata.json");
    fs::write(
        &wrong_language,
        r#"{"version":1,"language":"Solidity","sources":{"a.sol":{"content":""}},"settings":{"ingots":[]}}"#,
    )
    .expect("write non-Fe metadata");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        wrong_language.to_str().expect("path utf8"),
    ]);
    assert_eq!(exit_code, 1, "expected exit 1:\n{output}");
    assert!(
        output.contains("`language` must be \"Fe\""),
        "expected language error:\n{output}"
    );
}

#[test]
fn test_cli_build_from_metadata_conflicts_with_path_and_standalone() {
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        "Foo.metadata.json",
        "some/project/path",
    ]);
    assert_eq!(exit_code, 2, "expected clap error exit 2:\n{output}");
    assert!(
        output.contains("cannot be used with"),
        "expected conflict error:\n{output}"
    );

    let (output, exit_code) = run_fe_main(&[
        "build",
        "--from-metadata",
        "Foo.metadata.json",
        "--standalone",
    ]);
    assert_eq!(exit_code, 2, "expected clap error exit 2:\n{output}");
    assert!(
        output.contains("cannot be used with"),
        "expected conflict error:\n{output}"
    );
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

    assert_eq!(event["anonymous"], false);
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
    fn encode(self, _ out: core::abi::AbiFrame) {
        out.store_word(at: 0, value: if self.flag { 1 } else { 0 })
        out.store_word(at: 32, value: self.amount as u256)
    }
}

impl core::abi::Decode<std::abi::Sol> for Weird {
    fn decode_payload<D: core::abi::AbiDecoder<std::abi::Sol>, I: core::abi::ByteInput>(_ d: mut D, _ input: I) -> Self {
        let flag = bool::decode_payload(mut d, input)
        let amount = u64::decode_payload(mut d, input)
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
    let snapshot_path = ingot_dir.join(format!("{ingot_name}_tree"));
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

#[cfg(any(feature = "wasm", all(feature = "cranelift", native_host)))]
/// `fe_test` fixtures that need the EVM: contracts, storage, EVM
/// instructions or EVM effects. Sorted.
const EVM_ONLY_FE_TESTS: &[&str] = &[
    "abi_array_overflow_revert",
    "abi_array_view",
    "abi_bounded_input_len_untrusted",
    "abi_decode_field_names",
    "abi_decode_input_len_bound",
    "abi_dynamic_payload",
    "abi_nested_msg_decode_bounds",
    "abi_signed_int_boundary",
    "abi_struct",
    "abi_unpadded_dynamic_payload",
    "acceptance",
    "address_call_method",
    "address_static_method",
    "address_validation",
    "assignment_order",
    "assoc_type_fields",
    "assoc_type_lanes",
    "associated_shape_forwarding",
    "block_env",
    "bytes_view_overflow_revert",
    "call_with_default",
    "clear_place",
    "clear_zero_sized",
    "closure_effects",
    "closure_trait_effects",
    "code_backed_array_storage_copy",
    "code_backed_field_const_data",
    "code_backed_scalar_field",
    "code_struct_field_offsets",
    "coin",
    "composed_abi_decoder",
    "concrete_component_entry_codec",
    "const_trait_provider_contract",
    "contract_field_explicit_slot",
    "contract_field_mut_borrow_matrix",
    "contract_field_packing",
    "contract_field_projection_types",
    "contract_init_fixed_array_arg",
    "copied_state_words",
    "ctx",
    "delegatecall_state_effects",
    "deploy_and_call",
    "deposit_contract",
    "dyn_array_from_slice",
    "dyn_array_get",
    "dynamic_offset_overflow",
    "effect_aliasing_lanes",
    "effect_handle_field_deref",
    "eip712_domain",
    "encode_msg_calldata",
    "encode_storage_place",
    "entry_space_numbering",
    "enum",
    "enum_state_machine",
    "erc1167_clones",
    "erc20",
    "erc20_try_transfer",
    "erc4337",
    "erc721_erc1155_try_transfer",
    "escrow",
    "event_bytes",
    "event_dyn_array_address",
    "event_dyn_array_u256",
    "event_dyn_string",
    "event_dynamic_mixed",
    "event_many_fields",
    "event_zero_fields",
    "evm_init_code_hash",
    "factory",
    "fixed_bytes_constant_return",
    "for_mut_index_base",
    "forward_delegatecall",
    "forwarded_projection_spaces",
    "gcd",
    "generated_type_param_names",
    "immutable_aggregate_forwarding",
    "impl_method_does_not_shadow_free_fn",
    "invalid_enum_tag",
    "known_product_destructure_storage",
    "lane_accesses",
    "lane_holds_before_halts",
    "lane_pattern_writes",
    "lane_projection_spaces",
    "leading_zeros",
    "library_state_index",
    "long_error_event_names",
    "long_string_literals",
    "loop_base_mutation",
    "mandelbrot",
    "mandelbrot_minimal",
    "manual_root_zero_sized_effect",
    "maybe_mut_effect_calls",
    "mem_replace_take",
    "mem_vec",
    "merkle_proof",
    "method_forwards_memory_mut_borrow",
    "minimum_gas_calls",
    "mixed_space_forwarding",
    "mode_receivers_contract",
    "msg_array_arg",
    "msg_enum_field",
    "msg_multiple_dynamic_bytes",
    "msg_nested_array_arg",
    "msg_struct_sol_compat",
    "mul_div",
    "multicall",
    "mut_self_storage_receiver_regression",
    "mutex_lock_slot_no_transient_collision",
    "named_storage_codecs",
    "named_storage_deferred_codec",
    "named_storage_entry_codec_identity",
    "named_storage_evidence",
    "named_storage_local_generic_choice",
    "named_storage_local_generic_codec",
    "nested_index_assignment",
    "never_returning_recv_returns",
    "newtype_casts",
    "non_mem_effect_provider_mut_struct_receiver",
    "option_mut_scalar_match_regression",
    "ownable",
    "packed_encoding",
    "param_patterns",
    "pattern_accesses",
    "payable",
    "payable_return",
    "per_type_pins",
    "pinned_bound_layout",
    "pinned_field_spaces",
    "pinned_slots",
    "pointer_first_class",
    "precompile_in_mut_method",
    "precompile_wrappers",
    "product_params",
    "projected_storage_map_effect_forwarding",
    "projection_slides",
    "random",
    "raw_slot_constructors",
    "receiver_returns_non_mem_with_same_typed_memory_arg",
    "recv_arm_variant_paths",
    "recv_fallback",
    "recv_field_patterns",
    "recv_short_calldata_selector_guard",
    "reentrancy_mutex",
    "reentrancy_mutex_storage_map",
    "reentrancy_tstor_ptr",
    "revert_helpers",
    "root_effect_projection_key",
    "runtime_handle_preservation",
    "safe_erc20",
    "shape",
    "signature",
    "simple_amm",
    "snapshot_effects",
    "snapshot_view_call_effects",
    "sol_array_lanes",
    "sol_compat_int_abi_roundtrip",
    "sol_slot",
    "sol_storage_collections",
    "specialized_adt_array_extent",
    "state_only_sum_shapes",
    "state_pointers",
    "staticcall_decode",
    "storage_bytes_encode_return",
    "storage_bytes_to_memory",
    "storage_collection_loops",
    "storage_heap",
    "storage_lane_grants",
    "storage_layout_agreement",
    "storage_layout_helpers",
    "storage_map",
    "storage_map_aggregate_effects",
    "storage_map_allocating_key",
    "storage_map_array",
    "storage_map_effect_projection",
    "storage_map_enum_payload_methods",
    "storage_map_field_methods",
    "storage_map_field_projection",
    "storage_map_generic_pair_fields",
    "storage_map_generic_struct_methods",
    "storage_map_index",
    "storage_map_key_types",
    "storage_map_native_values",
    "storage_map_nested_solidity_layout",
    "storage_map_projection_forwarding",
    "storage_map_receiver_projection",
    "storage_map_record_patterns",
    "storage_map_recursive_views",
    "storage_map_struct_entries",
    "storage_map_tuple_keys",
    "storage_map_tuple_patterns",
    "storage_map_variant_projection",
    "storage_nested_array_stride",
    "storage_packed_array",
    "storage_packed_projections",
    "storage_packing",
    "storage_packing_value_types",
    "storage_slots_closures",
    "time_units",
    "trait_effect_lane_writes",
    "trait_effect_storage_param_provider",
    "tslot_values",
    "tstorptr_field_projection_shapes",
    "tstorptr_struct_field",
    "tuple_return_abi_params",
    "tuple_struct",
    "typed_call_returns",
    "typed_calls",
    "unchecked_signed_negation",
    "unchecked_storage_compound_assignment",
    "unsafe_splits",
    "vault",
    "voting",
    "wei_units",
    "wide_function_arguments",
    "zero_length_array_projections",
    "zero_option_result",
];

#[cfg(all(feature = "cranelift", native_host))]
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test",
    glob: "*.fe",
)]
fn test_fe_test_native(fixture: Fixture<&str>) {
    let path = fixture.path();
    let name = Path::new(path)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .expect("fixture name");
    if EVM_ONLY_FE_TESTS.binary_search(&name).is_ok() {
        let (output, exit_code) = run_fe_main(&["test", "--backend", "native", path]);
        assert!(
            exit_code != 0
                && (output.contains("needs the Evm compilation target")
                    || output.contains("does not support instruction")
                    || output.contains("the root effect `Host` does not provide")),
            "{name} no longer needs the EVM; remove it from EVM_ONLY_FE_TESTS:\n{output}"
        );
        return;
    }
    for level in ["0", "2"] {
        let (output, exit_code) = run_fe_main(&[
            "test",
            "--backend",
            "native",
            "-O",
            level,
            "--jobs",
            "1",
            path,
        ]);
        assert_eq!(
            exit_code, 0,
            "native fe test failed for {path} at O{level}:\n{output}\n\nTo reproduce:\n  cargo run --features cranelift --bin fe -- test --backend native -O {level} --jobs 1 {path}",
        );
    }
}

/// The fixtures that need no EVM also run on WebAssembly, where `usize` has
/// 32 bits. The run needs Node.js, as the other WebAssembly tests do.
#[cfg(feature = "wasm")]
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test",
    glob: "*.fe",
)]
fn test_fe_test_wasm(fixture: Fixture<&str>) {
    let path = fixture.path();
    let name = Path::new(path)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .expect("fixture name");
    // `buffer_capacity_bound` has a `usize` literal of more than 32 bits.
    if EVM_ONLY_FE_TESTS.binary_search(&name).is_ok() || name == "buffer_capacity_bound" {
        return;
    }
    let (output, exit_code) = run_fe_main(&["test", "--backend", "wasm", "--jobs", "1", path]);
    assert_eq!(
        exit_code, 0,
        "wasm fe test failed for {path}:\n{output}\n\nTo reproduce:\n  cargo run --features wasm --bin fe -- test --backend wasm {path}",
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

#[test]
fn test_fe_test_rejects_bad_block_args() {
    let temp = tempdir().expect("tempdir");
    let cases = [
        (
            "missing_timestamp",
            "#[test(timestamp)]
fn missing_timestamp() {}
",
            "invalid #[test] function `missing_timestamp`: #[test(timestamp = ...)] expects an integer literal",
        ),
        (
            "non_integer_timestamp",
            "#[test(timestamp = \"1000\")]
fn non_integer_timestamp() {}
",
            "invalid #[test] function `non_integer_timestamp`: #[test(timestamp = ...)] expects an integer literal",
        ),
        (
            "negative_timestamp",
            "#[test(timestamp = -1)]
fn negative_timestamp() {}
",
            "attribute value must be an ident or literal value",
        ),
        (
            "oversized_timestamp",
            "#[test(timestamp = 0x10000000000000000000000000000000000000000000000000000000000000000)]
fn oversized_timestamp() {}
",
            "invalid #[test] function `oversized_timestamp`: #[test(timestamp = ...)] must fit in u256",
        ),
        (
            "non_integer_block_number",
            "#[test(block_number = true)]
fn non_integer_block_number() {}
",
            "invalid #[test] function `non_integer_block_number`: #[test(block_number = ...)] expects an integer literal",
        ),
        (
            "oversized_block_number",
            "#[test(block_number = 0x10000000000000000000000000000000000000000000000000000000000000000)]
fn oversized_block_number() {}
",
            "invalid #[test] function `oversized_block_number`: #[test(block_number = ...)] must fit in u256",
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
            "expected fe test to reject {filename}:
{output}"
        );
        assert!(
            output.contains(expected),
            "expected `{expected}` for {filename}, got:
{output}"
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
    let snapshot_path = app_dir.join(format!("library_{case_name}"));
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[test]
fn test_cli_test_reports_type_error_in_path_dependency() {
    let ingot_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/cli_output/ingots/dep_with_type_error_consumer");
    let (output, _) = run_fe_command("test", ingot_dir.to_str().unwrap());
    let snapshot_path = ingot_dir.join("dep_with_type_error_consumer_test");
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
            && test_output
                .contains("PASS  [<time>] arithmetic_profile_ingot_wraps_wei_in_test_profile")
            && test_output
                .contains("PASS  [<time>] arithmetic_profile_ingot_generic_operator_is_checked")
            && test_output.contains("3 passed"),
        "expected test profile to run unchecked arithmetic:\n{test_output}"
    );

    let (release_output, release_exit_code) =
        run_fe_main(&["test", "--profile", "release", fixture_dir_str]);
    assert_ne!(
        release_exit_code, 0,
        "expected release profile test failure:\n{release_output}"
    );
    assert!(
        release_output.contains("2 failed"),
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
fn test_cli_test_dependency_inherent_const() {
    // An inherent const defined in a dependency ingot must resolve and evaluate
    // from a downstream ingot, in both value and type positions.
    let fixture_dir = fe_test_runner_fixture_dir("dependency_inherent_const");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", fixture_dir_str]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("uses_dependency_inherent_const_value")
            && output.contains("uses_dependency_inherent_const_in_type_position")
            && output.contains("2 passed"),
        "expected cross-ingot inherent const tests to pass, got:\n{output}"
    );
}

#[test]
fn test_cli_test_dependency_blanket_pin() {
    // A dependency's blanket `Pinned` implementation pins only the types its
    // bound admits, not every type of a downstream ingot.
    let fixture_dir = fe_test_runner_fixture_dir("dependency_blanket_pin");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", fixture_dir_str]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("plain_type_is_a_memory_value") && output.contains("1 passed"),
        "expected the downstream plain type to stay a memory value, got:\n{output}"
    );
}

#[test]
fn test_cli_test_library_trait_headers() {
    // A derive header implements a trait of core or std: drop glue, the
    // storage codec, an access and a loop run the members its provider wrote.
    let fixture_dir = fe_test_runner_fixture_dir("library_trait_headers");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", fixture_dir_str]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    for name in [
        "drop_glue_runs_the_member_a_provider_wrote",
        "a_copy_header_copies_the_value",
        "default_and_clone_headers_supply_their_methods",
        "a_header_sol_packed_packs_stored_elements",
        "a_header_state_index_places_the_entry",
        "a_producer_header_drives_a_loop",
        "eq_ord_and_add_headers_define_the_operators",
        "an_error_variant_header_prefixes_the_revert_data",
        "collection_and_bidirectional_headers_drive_loops",
        "abi_size_storage_key_and_word_repr_headers_supply_their_members",
        "a_lane_codec_header_decides_how_an_entry_is_read",
        "a_msg_variant_header_supplies_the_selector",
        "fn_and_fn_mut_headers_answer_a_call",
    ] {
        assert!(output.contains(name), "missing {name} in:\n{output}");
    }
    assert!(output.contains("13 passed"), "got:\n{output}");
}

#[test]
fn test_cli_test_msg_variant_is_selected_by_the_abi_of_the_recv_block() {
    // A message type with a written `MsgVariant` for a second ABI keeps the
    // selector and the return type of the implementation for the ABI of the
    // `recv` block, whichever implementation is met first.
    let fixture_dir = fe_test_runner_fixture_dir("msg_variant_by_abi");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", fixture_dir_str]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    for name in [
        "the_client_and_the_contract_use_the_selector_of_the_blocks_abi",
        "the_selectors_of_the_blocks_abi_reach_the_arms",
        "the_selector_of_another_abi_reaches_no_arm",
    ] {
        assert!(output.contains(name), "missing {name} in:\n{output}");
    }
    assert!(output.contains("3 passed"), "got:\n{output}");
}

#[test]
fn test_cli_test_a_generated_msg_variant_fills_a_gap_and_changes_no_written_arm() {
    // `MsgVariant` is selected like any trait by the ABI of the `recv` block.
    // An implementation that a provider generates for another ABI changes
    // nothing a contract answers to, and one generated for the block's ABI
    // serves the arm of a type that had none.
    let fixture_dir = fe_test_runner_fixture_dir("generated_msg_variant");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", fixture_dir_str]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    for name in [
        "the_generated_implementation_for_another_abi_can_be_read",
        "the_written_selector_of_the_blocks_abi_still_reaches_the_arm",
        "the_selector_of_the_generated_implementation_for_another_abi_reaches_no_arm",
        "the_generated_implementation_of_the_blocks_abi_serves_the_arm_that_names_it",
    ] {
        assert!(output.contains(name), "missing {name} in:\n{output}");
    }
    assert!(output.contains("4 passed"), "got:\n{output}");
}

#[test]
fn test_cli_test_headers_serve_types_that_attributes_expand() {
    // A derive header for a struct that `#[abi]`, `#[event]` or `#[error]`
    // expands, or for a `msg` variant, reads the fields as written and its
    // members run like a handwritten implementation's.
    let fixture_dir = fe_test_runner_fixture_dir("expanded_type_headers");
    let fixture_dir_str = fixture_dir.to_str().expect("fixture dir utf8");

    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", fixture_dir_str]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    for name in [
        "an_abi_struct_compares_by_its_header",
        "an_abi_struct_is_copied_by_its_header",
        "an_event_struct_compares_by_its_header",
        "an_error_struct_compares_by_its_header",
        "a_msg_variant_compares_by_its_header",
    ] {
        assert!(output.contains(name), "missing {name} in:\n{output}");
    }
    assert!(output.contains("5 passed"), "got:\n{output}");
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
fn test_cli_test_unsafe_trust_allows_trusted_and_safe_uses() {
    for (fixture, test) in [
        ("unsafe_trust_safe_api", "untrusted_safe_api"),
        ("unsafe_trust_safe_api", "untrusted_derived_encodings"),
        ("unsafe_trust_allowed", "allowed_unsafe_use"),
        ("unsafe_trust_allowed", "allowed_created_contract"),
        ("unsafe_trust_delegated", "delegated_unsafe_use"),
        (
            "unsafe_trust_generated_allowed",
            "header_member_runs_unsafe_code",
        ),
        (
            "unsafe_trust_generated_allowed",
            "generated_member_runs_unsafe_code",
        ),
        (
            "unsafe_trust_generated_allowed",
            "safe_header_member_needs_no_trust",
        ),
        (
            "unsafe_trust_own_provider",
            "own_header_member_runs_unsafe_code",
        ),
        (
            "unsafe_trust_own_provider",
            "own_generated_member_runs_unsafe_code",
        ),
        (
            "unsafe_trust_splice_allowed",
            "trusted_provider_splices_an_untrusted_fragment",
        ),
        (
            "unsafe_trust_splice_allowed",
            "own_provider_splices_an_untrusted_fragment",
        ),
        (
            "unsafe_trust_splice_allowed",
            "untrusted_provider_splices_a_trusted_fragment",
        ),
        (
            "unsafe_trust_splice_allowed",
            "untrusted_provider_splices_a_trusted_item",
        ),
        (
            "unsafe_trust_spliced_safe_fragment",
            "spliced_safe_fragment",
        ),
        ("unsafe_trust_spliced_operation_allowed", "spliced_deref"),
        ("unsafe_trust_unused_unsafe_impls", "builds_values"),
        (
            "generated_code_visibility_own_record",
            "reads_its_own_record_and_helpers",
        ),
        (
            "generated_code_visibility_provider_items",
            "provider_pub_ingot_helper",
        ),
        (
            "generated_code_visibility_provider_items",
            "provider_private_helper",
        ),
        ("generated_code_visibility_provider_members", "own_method"),
        ("generated_code_visibility_provider_members", "own_field"),
        ("generated_code_visibility_provider_cast", "cast_own"),
        ("generated_code_visibility_generated_path", "by_call"),
        ("generated_code_visibility_generated_path", "by_path"),
        ("generated_code_visibility_alias_self", "alias_pub"),
        ("generated_code_visibility_own_paths", "own_paths"),
        ("unsafe_trust_const_allowed", "via_const"),
        ("unsafe_trust_const_allowed", "assoc"),
        ("unsafe_trust_const_allowed", "pat"),
        ("unsafe_trust_const_safe", "reads_a_safe_constant"),
        ("unsafe_trust_panic_payload_allowed", "unwrap_err"),
        (
            "unsafe_trust_dispatcher_allowed",
            "dispatcher_encodes_with_trusted_code",
        ),
        (
            "unsafe_trust_init_allowed",
            "the_contract_decodes_its_init_arguments",
        ),
        ("unsafe_trust_pod_allowed", "peeks_at_a_type_of_v"),
        (
            "unsafe_trust_pod_allowed",
            "peeks_at_a_type_with_an_unsafe_impl_written_here",
        ),
        ("unsafe_trust_drop_allowed", "drops_a_value_of_u"),
        ("unsafe_trust_drop_allowed", "drops_an_array_of_values"),
        (
            "unsafe_trust_drop_empty",
            "drops_an_empty_array_of_values_of_u",
        ),
        ("unsafe_trust_drop_allowed", "drops_a_value_that_holds_one"),
        (
            "unsafe_trust_drop_allowed",
            "drops_a_value_with_a_drop_written_here",
        ),
    ] {
        let fixture_dir = fe_test_runner_fixture_dir(fixture);
        let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
        let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
        assert_eq!(exit_code, 0, "{fixture}: fe test failed:\n{output}");
        assert!(
            output.contains(&format!("PASS  [<time>] {test}")),
            "{fixture}: expected {test} to pass, got:\n{output}"
        );
    }
}

#[test]
fn test_cli_test_unsafe_trust_rejects_untrusted_unsafe_uses() {
    for (fixture, call, fix) in [
        (
            "unsafe_trust_untrusted",
            "dep::round_trip(7)",
            "to trust `dep`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml",
        ),
        (
            "unsafe_trust_workspace_member",
            "dep::round_trip(7)",
            "to trust `dep`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml",
        ),
        (
            "unsafe_trust_untrusted_voucher",
            "mid::via_dep(7)",
            "`mid` allows `dep` to use unsafe code but is not trusted itself",
        ),
        (
            "unsafe_trust_locate",
            "words[0]",
            "to trust `dep`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml",
        ),
        (
            "unsafe_trust_locate",
            "flags[1] = first == 0",
            "to trust `dep`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml",
        ),
        (
            "unsafe_trust_create",
            "evm.create2<dep::Vault>(value: Wei::zero(), args: (), salt: 0)",
            "to trust `dep`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml",
        ),
        (
            // The default of a trait constant whose declared type is `Self::X`
            // calls code of `dep` that runs unsafe code. The package wrote the
            // default, so the call is judged where it is written.
            "unsafe_trust_default_constant_own",
            "dep::peek()",
            "to trust `dep`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml",
        ),
        (
            // The default of a trait of `dep` whose declared type is `Self::X`
            // runs unsafe code of `dep`, which a read of the constant reaches.
            "unsafe_trust_default_constant_read",
            "A::K",
            "to trust `dep`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml",
        ),
    ] {
        let fixture_dir = fe_test_runner_fixture_dir(fixture);
        let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
        let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
        assert_ne!(exit_code, 0, "{fixture}: expected a trust error:\n{output}");
        assert!(
            output.contains("call reaches unsafe code in `dep`, which is not allowed to use it")
                && output.contains(call)
                && output.contains("unsafe code in `dep`")
                && output.contains(fix),
            "{fixture}: unexpected diagnostics:\n{output}"
        );
    }
}

#[test]
fn test_cli_test_checks_moves_in_the_initializer_of_every_constant() {
    // The initializer of a constant is a body that moves values like any
    // other. A module constant, an inherent constant, an override in an
    // `impl Trait` and a default in a trait are each refused when the
    // initializer moves a value twice, whether or not the constant is read.
    let prelude = "struct B { v: u256 }\nconst fn take(_ b: own B) -> u256 { b.v }\n";
    let twice =
        "{ let b = B { v: 1 }\n        let x = take(b)\n        let y = take(b)\n        x + y }";
    let cases = [
        (
            "module_constant",
            format!("{prelude}const D: u256 = {twice}\n#[test]\nfn t() {{ assert!(D == 2) }}\n"),
        ),
        (
            "nested_module_constant",
            "mod m {\n    pub struct B { v: u256 }\n    pub const fn take(_ b: own B) -> u256 { b.v }\n    pub const D: u256 = { let b = B { v: 1 }\n        let x = take(b)\n        let y = take(b)\n        x + y }\n}\n#[test]\nfn t() { assert!(m::D == 2) }\n"
                .to_string(),
        ),
        (
            "trait_default_read",
            format!(
                "{prelude}trait Tr {{\n    const D: u256 = {twice}\n}}\nstruct S {{}}\nimpl Tr for S {{}}\n#[test]\nfn t() {{ assert!(S::D == 2) }}\n"
            ),
        ),
        (
            "trait_default_unread",
            format!(
                "{prelude}trait Tr {{\n    const D: u256 = {twice}\n}}\nstruct S {{}}\nimpl Tr for S {{}}\n#[test]\nfn t() {{ assert!(true) }}\n"
            ),
        ),
        (
            "inherent_constant_read",
            format!(
                "{prelude}struct S {{}}\nimpl S {{\n    const D: u256 = {twice}\n}}\n#[test]\nfn t() {{ assert!(S::D == 2) }}\n"
            ),
        ),
        (
            "impl_override_read",
            format!(
                "{prelude}trait Tr {{ const D: u256 }}\nstruct S {{}}\nimpl Tr for S {{\n    const D: u256 = {twice}\n}}\n#[test]\nfn t() {{ assert!(S::D == 2) }}\n"
            ),
        ),
        (
            "inherent_constant_unread",
            format!(
                "{prelude}struct S {{}}\nimpl S {{\n    const D: u256 = {twice}\n}}\n#[test]\nfn t() {{ assert!(true) }}\n"
            ),
        ),
    ];
    let temp = tempdir().expect("tempdir");
    for (name, source) in cases {
        let path = temp.path().join(format!("{name}.fe"));
        fs::write(&path, source).expect("write fixture");
        let (output, exit_code) = run_fe_main(&["test", path.to_str().expect("utf-8 path")]);
        assert_ne!(exit_code, 0, "{name}: expected a move error:\n{output}");
        assert!(
            output.contains("error[16-0002]")
                && output.contains("cannot use a value after it was moved"),
            "{name}: expected a move conflict, got:\n{output}"
        );
    }

    // A default whose declared type is `Self::X` is walked through the
    // implementations that inherit it, and reports a move once, where the
    // trait wrote it, however many implementations inherit it.
    let param_typed = |impls: &str| {
        format!(
            "{prelude}trait Tr {{\n    type X\n    const D: Self::X = {{ let b = B {{ v: 1 }}\n        let x = take(b)\n        let y = take(b)\n        (-3 as i8) }}\n}}\n{impls}#[test]\nfn t() {{ assert!(true) }}\n"
        )
    };
    for (name, impls) in [
        (
            "trait_default_param_typed",
            "struct S {}\nimpl Tr for S {\n    type X = i8\n}\n",
        ),
        (
            "trait_default_param_typed_three_impls",
            "struct S {}\nimpl Tr for S {\n    type X = i8\n}\nstruct U {}\nimpl Tr for U {\n    type X = i8\n}\nstruct V {}\nimpl Tr for V {\n    type X = i8\n}\n",
        ),
    ] {
        let path = temp.path().join(format!("{name}.fe"));
        fs::write(&path, param_typed(impls)).expect("write fixture");
        let (output, exit_code) = run_fe_main(&["test", path.to_str().expect("utf-8 path")]);
        assert_ne!(exit_code, 0, "{name}: expected a move error:\n{output}");
        assert_eq!(
            output.matches("error[16-0002]").count(),
            1,
            "{name}: expected one move conflict, got:\n{output}"
        );
    }
}

#[test]
fn test_cli_test_checks_a_trait_default_against_the_type_of_each_use() {
    // `const NEG: Self::X = -3 as i8` has type `i8`. It is accepted for an
    // implementation where `X` is `i8`, and refused at an implementation where
    // `X` is `u256`, whether or not the constant is read: the error is at the
    // implementation, and a read adds none.
    let source = |x: &str, check: &str| {
        format!(
            "trait Narrow {{\n    type X\n    const NEG: Self::X = -3 as i8\n}}\nstruct A {{}}\nimpl Narrow for A {{\n    type X = {x}\n}}\n#[test]\nfn t() {{ {check} }}\n"
        )
    };
    let temp = tempdir().expect("tempdir");
    let run = |name: &str, source: String| {
        let path = temp.path().join(format!("{name}.fe"));
        fs::write(&path, source).expect("write fixture");
        run_fe_main(&["test", path.to_str().expect("utf-8 path")])
    };
    let (output, exit_code) = run("agreeing", source("i8", "assert!(A::NEG == -3)"));
    assert_eq!(exit_code, 0, "expected the read to be accepted:\n{output}");
    for (name, check) in [
        ("unread", "assert!(true)"),
        ("differing", "assert!(A::NEG == 0)"),
    ] {
        let (output, exit_code) = run(name, source("u256", check));
        assert_ne!(exit_code, 0, "{name}: expected a type error:\n{output}");
        assert!(
            output.contains("error[8-0167]")
                && output.contains("the default value of `NEG` does not type-check")
                && !output.contains("error[8-0000]")
                && !output.contains("16-0004"),
            "{name}: expected one error at the implementation, got:\n{output}"
        );
        assert_eq!(
            output.matches("error[").count(),
            1,
            "{name}: a read adds no second error:\n{output}"
        );
    }
}

#[test]
fn test_cli_checks_an_inherited_default_for_each_implementation() {
    // A default whose declared type mentions `Self`, an associated type of
    // `Self` or a parameter of the trait is checked again for each
    // implementation that inherits it, as if the implementation had written
    // the same text with its own types. A mismatch is an error at that
    // implementation, whether or not the constant is read.
    const NEG: &str =
        "trait Narrow {\n    type X\n    const NEG: Self::X = -3 as i8\n}\nstruct A {}\n";
    let cases: Vec<(&str, String, bool)> = vec![
        (
            "inner_mismatch",
            "trait Narrow {\n    type X\n    const NEG: Self::X = {\n        let x: Self::X = 200 as u8\n        (-3 as i8)\n    }\n}\nstruct A {}\nimpl Narrow for A {\n    type X = i8\n}\n#[test]\nfn t() { assert!(A::NEG == -3) }\n"
                .to_string(),
            true,
        ),
        (
            "pattern_read",
            format!(
                "{NEG}impl Narrow for A {{\n    type X = u256\n}}\nfn f(_ v: u256) -> bool {{\n    match v {{\n        A::NEG => true\n        _ => false\n    }}\n}}\n#[test]\nfn t() {{ assert!(f(0) == false) }}\n"
            ),
            true,
        ),
        (
            "unread_mismatch",
            format!("{NEG}impl Narrow for A {{\n    type X = u256\n}}\n#[test]\nfn t() {{ assert!(true) }}\n"),
            true,
        ),
        (
            "generic_reader",
            format!(
                "{NEG}impl Narrow for A {{\n    type X = i16\n}}\nfn get<T: Narrow>() -> T::X {{ T::NEG }}\n#[test]\nfn t() {{ assert!(get<A>() == -3) }}\n"
            ),
            true,
        ),
        (
            "generic_impl_open",
            "trait Narrow {\n    type X\n    const NEG: Self::X = -3 as i8\n}\nstruct W<T> {}\nimpl<T> Narrow for W<T> {\n    type X = T\n}\n#[test]\nfn t() { assert!(W<i8>::NEG == -3) }\n"
                .to_string(),
            true,
        ),
        (
            "inner_annotation_agrees",
            "trait Narrow {\n    type X\n    const NEG: Self::X = {\n        let x: Self::X = -3 as i8\n        x\n    }\n}\nstruct A {}\nimpl Narrow for A {\n    type X = i8\n}\n#[test]\nfn t() { assert!(A::NEG == -3) }\n"
                .to_string(),
            false,
        ),
        (
            "tuple_hint_read",
            "trait Narrow {\n    type X\n    const P: (Self::X, u8) = (-3 as i8, 255)\n}\nstruct A {}\nimpl Narrow for A {\n    type X = i8\n}\n#[test]\nfn t() { assert!(A::P.1 == 255) }\n"
                .to_string(),
            false,
        ),
        (
            "tuple_hint_unread",
            "trait Narrow {\n    type X\n    const P: (Self::X, u8) = (-3 as i8, 255)\n}\nstruct A {}\nimpl Narrow for A {\n    type X = i8\n}\n#[test]\nfn t() { assert!(true) }\n"
                .to_string(),
            false,
        ),
        (
            "pattern_value",
            format!(
                "{NEG}impl Narrow for A {{\n    type X = i8\n}}\nfn f(_ v: i8) -> bool {{\n    match v {{\n        A::NEG => true\n        _ => false\n    }}\n}}\n#[test]\nfn t() {{\n    assert!(f(-3) == true)\n    assert!(f(3) == false)\n}}\n"
            ),
            false,
        ),
        (
            "const_patterns_shape",
            format!(
                "{NEG}impl Narrow for A {{\n    type X = i8\n}}\nfn f(_ x: i8, _ y: u256) -> u256 {{\n    match (x, y) {{\n        (A::NEG, 1) => 1\n        _ => 0\n    }}\n}}\n#[test]\nfn t() {{ assert!(f(-3, 1) == 1) }}\n"
            ),
            false,
        ),
        (
            "generic_impl_closed",
            "trait Narrow {\n    type X\n    const NEG: Self::X = -3 as i8\n}\nstruct W<T> {}\nimpl<T> Narrow for W<T> {\n    type X = i8\n}\n#[test]\nfn t() { assert!(W<u8>::NEG == -3) }\n"
                .to_string(),
            false,
        ),
        (
            "override_never_checks_the_default",
            format!(
                "{NEG}impl Narrow for A {{\n    type X = u8\n    const NEG: u8 = 3\n}}\n#[test]\nfn t() {{ assert!(A::NEG == 3) }}\n"
            ),
            false,
        ),
        (
            "literal_default_u32",
            "trait Tr {\n    type X\n    const C: Self::X = 1\n}\nstruct A {}\nimpl Tr for A {\n    type X = u32\n}\nfn reader<T: Tr<X = u32>>() -> u32 { T::C }\n#[test]\nfn t() { assert!(A::C == 1 && reader<A>() == 1) }\n"
                .to_string(),
            false,
        ),
    ];
    let temp = tempdir().expect("tempdir");
    for (name, source, refused) in cases {
        let path = temp.path().join(format!("{name}.fe"));
        fs::write(&path, source).expect("write fixture");
        let (output, exit_code) = run_fe_main(&["test", path.to_str().expect("utf-8 path")]);
        if refused {
            assert_ne!(exit_code, 0, "{name}: expected a refusal:\n{output}");
            assert!(
                output.contains("error[8-0167]")
                    && output.contains("the default value of `NEG` does not type-check"),
                "{name}: expected the default to be refused at the implementation, got:\n{output}"
            );
            if name == "generic_reader" {
                assert!(
                    !output.contains("16-0004"),
                    "{name}: expected no internal error, got:\n{output}"
                );
            }
        } else {
            assert_eq!(exit_code, 0, "{name}: expected acceptance:\n{output}");
        }
    }

    // The default of a dependency's trait is checked for an implementation in
    // the package that checks: the error is at the implementation, with the
    // default as a secondary label in the dependency.
    let fixture_dir = fe_test_runner_fixture_dir("inherited_default_dep");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(
        exit_code, 0,
        "dependency_default: expected a refusal:\n{output}"
    );
    assert!(
        output.contains("error[8-0167]")
            && output.contains("the default value of `NEG` does not type-check")
            && output.contains("impl dep::Narrow for A")
            && output.contains("-3 as i8"),
        "dependency_default: expected the error at the implementation, got:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_judges_generated_code_by_the_package_that_wrote_it() {
    // A provider of `dep` writes unsafe code in a quotation. A trait header and
    // an inherent header publish that code into the ingot that asks, but
    // `dep` wrote it, so a call that reaches it is refused when nothing
    // trusts `dep`. A member without unsafe code needs no trust, and neither
    // does a member of an `unsafe impl` that this ingot wrote.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_generated");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected trust errors:\n{output}");
    for call in [
        "Header {}.read()",
        "Generated {}.write()",
        "VouchedUnsafely {}.vouch()",
    ] {
        assert!(
            output.contains("this call reaches unsafe code in `dep`") && output.contains(call),
            "{call}: unexpected diagnostics:\n{output}"
        );
    }
    assert_eq!(
        output
            .matches("call reaches unsafe code in `dep`, which is not allowed to use it")
            .count(),
        3,
        "one error for each call:\n{output}"
    );
    assert!(
        output.contains("to trust `dep`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml"),
        "unexpected diagnostics:\n{output}"
    );
    for call in ["Safe {}.read()", "Vouched {}.vouch()"] {
        assert!(!output.contains(call), "{call} needs no trust:\n{output}");
    }
}

#[test]
fn test_cli_test_unsafe_trust_judges_an_unsafe_impl_by_the_package_that_wrote_it() {
    // `hdr` writes `unsafe impl Vouching for Marked with (VouchesSafely)`, a
    // provider of `dep`, which the root trusts. The `unsafe impl` asserts the
    // members whoever supplied them, and nothing trusts `hdr`.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_impl_author");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected a trust error:\n{output}");
    assert!(
        output.contains("call reaches unsafe code in `hdr`, which is not allowed to use it")
            && output.contains("hdr::Marked {}.vouch()")
            && output.contains(
                "to trust `hdr`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml"
            ),
        "unexpected diagnostics:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_still_checks_the_bodies_of_generated_members() {
    // A member that a provider of an untrusted `dep` generates exists only in
    // the ingot that asked for it, so its body is checked there even when
    // nothing calls it. Trust decides what a call may reach, not whether the
    // body is checked.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_generated_body");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected a move error:\n{output}");
    assert!(
        output.contains("move conflict in `fn twice`")
            && output.contains("cannot use a value after it was moved"),
        "unexpected diagnostics:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_judges_a_spliced_fragment_by_every_package_that_contributed() {
    // `t` and `t2` are trusted, `u` is not. An `unsafe` block is accepted only
    // if the package that wrote it and each package whose quotation placed it
    // are trusted, so splicing `u`'s fragment from a trusted provider or from
    // this ingot is refused, and so is splicing the trusted `t2`'s fragment
    // from `u`'s provider. Each error names `u`, the first package that is not
    // trusted, and labels the block in the text of the package that wrote it.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_splice");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected trust errors:\n{output}");
    for call in [
        "ByTrusted {}.spliced()",
        "ByOwn {}.spliced()",
        "ByUntrusted {}.spliced()",
    ] {
        assert!(
            output.contains("this call reaches unsafe code in `u`") && output.contains(call),
            "{call}: unexpected diagnostics:\n{output}"
        );
    }
    assert_eq!(
        output
            .matches("call reaches unsafe code in `u`, which is not allowed to use it")
            .count(),
        3,
        "one error for each call:\n{output}"
    );
    assert!(
        output.contains(
            "to trust `u`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml"
        ),
        "unexpected remedy:\n{output}"
    );
    // The label shows the block where it was written: `u`'s text for the first
    // two, `t2`'s for the third.
    assert_eq!(
        output.matches("store_word(ptr: p, value: 7)").count(),
        2,
        "the label of a block of `u` shows its text:\n{output}"
    );
    assert_eq!(
        output.matches("store_word(ptr: p, value: 8)").count(),
        1,
        "the label of a block of `t2` shows its text:\n{output}"
    );
    for trusted in ["unsafe code in `t`", "unsafe code in `t2`"] {
        assert!(
            !output.contains(trusted),
            "a trusted package is never named:\n{output}"
        );
    }
}

#[test]
fn test_cli_test_unsafe_trust_judges_an_unsafe_operation_by_the_package_that_wrote_it() {
    // `t` is trusted, `u` is not. `t`'s provider writes the `unsafe` block and
    // splices `u`'s fragment `*p` into it. The dereference needs an `unsafe`
    // block, and the package that wrote its text is `u`, so the call is
    // refused although the block is `t`'s. The control `spliced_safe_fragment`
    // splices a fragment that needs no `unsafe` block, which stays accepted.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_spliced_operation");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected a trust error:\n{output}");
    assert!(
        output.contains("this call reaches unsafe code in `u`")
            && output.contains("S {}.run(p)")
            && output.contains("unsafe code in `u`")
            && output.contains("*p"),
        "unexpected diagnostics:\n{output}"
    );
    assert_eq!(
        output
            .matches("call reaches unsafe code in `u`, which is not allowed to use it")
            .count(),
        1,
        "one error for the call:\n{output}"
    );
    assert!(
        !output.contains("unsafe code in `t`"),
        "a trusted package is never named:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_still_refuses_unsafe_code_that_u_wrote_in_its_own_quotation() {
    // Controls of the per-contributor rule, refused with or without it: a
    // trusted provider whose quotation calls an ordinary function of `u`, and
    // a provider of `u` that splices its own fragment.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_splice_controls");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected trust errors:\n{output}");
    for call in ["u::raw()", "UntrustedOwn {}.spliced()"] {
        assert!(
            output.contains("this call reaches unsafe code in `u`") && output.contains(call),
            "{call}: unexpected diagnostics:\n{output}"
        );
    }
    assert_eq!(
        output
            .matches("call reaches unsafe code in `u`, which is not allowed to use it")
            .count(),
        2,
        "one error for each call:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_follows_the_constants_a_body_reads() {
    // The initializer of a constant runs wherever the constant is read, in an
    // expression or in a pattern, and when a `static_assert` of the checked
    // package is evaluated. `u` wrote the `unsafe` blocks of its constants and
    // nothing trusts `u`. `unsafe_trust_const_safe` reads a constant of `u`
    // without unsafe code, which stays accepted.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_const_reads");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected trust errors:\n{output}");
    for call in ["u::get()", "u::get_assoc()", "u::pat(0)", "u::SA == 0"] {
        assert!(
            output.contains("this call reaches unsafe code in `u`") && output.contains(call),
            "{call}: unexpected diagnostics:\n{output}"
        );
    }
    assert!(
        output.contains("call reaches unsafe code in `u`, which is not allowed to use it"),
        "unexpected diagnostics:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_follows_the_encoding_of_a_panic_value() {
    // `Result::unwrap` on an error panics with the error value, and the revert
    // data is the value's `Encode` implementation, which no call names. `u`
    // wrote an `unsafe` block in it and nothing trusts `u`.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_panic_payload");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected a trust error:\n{output}");
    assert!(
        output.contains("call reaches unsafe code in `u`, which is not allowed to use it")
            && output.contains("mk().unwrap()"),
        "unexpected diagnostics:\n{output}"
    );
}

#[test]
fn test_cli_test_generated_code_of_another_package_sees_only_what_is_public() {
    // `prov` writes `leak` and a header in `user` asks for it. Code that a
    // package writes into another package can use what is public there, the
    // fields of the requested record, and what `prov` itself generated. The
    // private method `hidden` of the requested record and the private field
    // `balance` of an unrelated struct are `user`'s, not handed over by the
    // header. `self.vault`, a field of the requested record, is.
    let fixture_dir = fe_test_runner_fixture_dir("generated_code_visibility_leak");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected visibility errors:\n{output}");
    assert!(
        output.contains("`hidden` is not visible") && output.contains("`balance` is not visible"),
        "unexpected diagnostics:\n{output}"
    );
    assert!(
        !output.contains("`vault` is not visible"),
        "a field of the requested record stays visible:\n{output}"
    );
}

#[test]
fn test_cli_test_generated_code_of_another_package_cannot_name_private_members_by_path() {
    // The same rule holds for a member named by a path through `Self`: `prov`
    // writes `Self::hidden(self)`, `Self::secret()` and `Self::K`, and the
    // private method, function and constant belong to `user`, not to the
    // record that the header hands over.
    let fixture_dir = fe_test_runner_fixture_dir("generated_code_visibility_path_leak");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected visibility errors:\n{output}");
    for name in ["hidden", "secret", "K"] {
        assert!(
            output.contains(&format!("`{name}` is not visible")),
            "expected `{name}` to be refused:\n{output}"
        );
    }
}

#[test]
fn test_cli_test_generated_code_of_another_package_cannot_name_private_members_in_types() {
    // A name in a type position is judged by the package that wrote it, as a
    // name in a value position is: the constant `K` belongs to the package that
    // asked, so a generic argument `Packed<Self::K>` written by the provider
    // is refused in a signature, in a path of a body and in an annotation. The
    // alias `Same<Self>` and a method value or pattern reach the same members.
    for (fixture, names) in [
        ("generated_code_visibility_type_sig_leak", &["K"][..]),
        ("generated_code_visibility_type_body_path_leak", &["K"][..]),
        ("generated_code_visibility_type_body_annot_leak", &["K"][..]),
        ("generated_code_visibility_alias_self_leak", &["hidden"][..]),
        (
            "generated_code_visibility_value_and_pattern_leak",
            &["hidden", "K"][..],
        ),
    ] {
        let fixture_dir = fe_test_runner_fixture_dir(fixture);
        let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
        let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
        assert_ne!(
            exit_code, 0,
            "{fixture}: expected visibility errors:\n{output}"
        );
        for name in names {
            assert!(
                output.contains(&format!("`{name}` is not visible")),
                "{fixture}: expected `{name}` to be refused:\n{output}"
            );
        }
    }
}

#[test]
fn test_cli_test_generated_code_sees_through_a_newtype_by_the_field_that_its_writer_sees() {
    // A cast sees through a newtype only if the writer of the cast can see its
    // field. `prov` writes `self.vault as u256`, and the field `balance` of
    // `Vault` is private to the package that asked, so the cast is refused with
    // the reason. A cast that `prov` writes of its own newtype `W`, whose field
    // is `pub(ingot)` in `prov`, is accepted (`generated_code_visibility_provider_cast`).
    let fixture_dir = fe_test_runner_fixture_dir("generated_code_visibility_cast_leak");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected a cast error:\n{output}");
    assert!(
        output.contains("error[8-0055]")
            && output.contains("cannot cast `Vault` to `u256`")
            && output.contains("cast is not allowed because the struct field is not `pub`"),
        "unexpected diagnostics:\n{output}"
    );
}

#[test]
fn test_cli_check_types_a_quotation_by_its_syntax() {
    // A quotation has type `Code<K>` only if its syntax admits `K`. `x + 1` is
    // an expression, not a list of items, and an item with a function is not a
    // pattern. Each was accepted until it was evaluated, then failed late or
    // with an internal error.
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    let check = |name: &str| {
        let path = fixtures.join(name);
        run_fe_main(&["check", path.to_str().expect("fixture path utf8")])
    };
    // The control: a quotation of items as `Code<Item>` is accepted.
    let (output, exit_code) = check("quote_kind_ok.fe");
    assert_eq!(exit_code, 0, "{output}");
    let (output, exit_code) = check("quote_kind_mismatch.fe");
    assert_ne!(exit_code, 0, "expected a kind error:\n{output}");
    assert!(
        output.contains("but `Code<Item>` is expected") && output.contains("error[8-0164]"),
        "unexpected diagnostics:\n{output}"
    );
    let (output, exit_code) = check("quote_splice_mismatch.fe");
    assert_ne!(exit_code, 0, "expected a kind error:\n{output}");
    assert!(
        output.contains("but `Code<Pat>` is expected")
            && output.contains("error[8-0164]")
            && !output.contains("16-0004"),
        "unexpected diagnostics:\n{output}"
    );
    // A splice that cannot fill its hole is a different error.
    let (output, exit_code) = check("quote_splice_hole_mismatch.fe");
    assert_ne!(exit_code, 0, "expected a splice error:\n{output}");
    assert!(
        output.contains("error[8-0165]") && output.contains("cannot fill its position"),
        "unexpected diagnostics:\n{output}"
    );
}

#[test]
fn test_cli_check_reports_parse_and_type_errors_beside_a_derive_header() {
    // A file with a derive header and a parse error is checked like a file
    // without a header: the parse error and the type error are both reported,
    // and generation is skipped for it, so no raw generation line and no
    // report of an unprepared header appears.
    let temp = tempdir().expect("tempdir");
    let source = temp.path().join("header_with_parse_error.fe");
    fs::write(
        &source,
        "use core::derive::DeriveInherent\n\
use core::meta::{Code, Item, Record}\n\
struct Make {}\n\
impl DeriveInherent for Make {\n\
    const fn derive(_ record: Record) -> Code<Item> {\n\
        quote { pub fn generated() -> u256 { 1 } }\n\
    }\n\
}\n\
struct S { a: u256 }\n\
impl S with (Make)\n\
fn bad() -> u256 { true }\n\
fn broken( {\n",
    )
    .expect("write source");
    let source = source.to_str().expect("UTF-8 source path");
    for args in [
        vec!["check", source],
        vec!["check", "--recovery-mode", source],
    ] {
        let (output, exit_code) = run_fe_main(&args);
        assert_ne!(exit_code, 0, "{args:?}: expected errors:\n{output}");
        assert!(
            output.contains("error[1-0001]") && output.contains("error[8-0000]"),
            "{args:?}: expected the parse error and the type error:\n{output}"
        );
        assert!(
            !output.contains("authored parse errors")
                && !output.contains("derive header was not prepared"),
            "{args:?}: generation reported a cascade:\n{output}"
        );
    }
}

#[test]
fn test_cli_lsif_indexes_the_members_a_header_generates() {
    // `S::arity` is written in a quotation and published into `S` by the
    // header. The index covers it and the call that names it, as `fe check`
    // and `fe doc` do.
    let temp = tempdir().expect("tempdir");
    let root = temp.path().join("app");
    fs::create_dir_all(root.join("src")).expect("src dir");
    fs::write(
        root.join("fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n",
    )
    .expect("fe.toml");
    let source = "use core::derive::DeriveInherent\n\
use core::meta::{Code, Item, Record, field_count}\n\
struct Count {}\n\
impl DeriveInherent for Count {\n\
    const fn derive(_ record: Record) -> Code<Item> {\n\
        let n = field_count(record)\n\
        quote { pub fn arity() -> usize { ${n} } }\n\
    }\n\
}\n\
pub struct S { a: u256 }\n\
impl S with (Count)\n\
pub fn user() -> usize { S::arity() }\n";
    fs::write(root.join("src/lib.fe"), source).expect("lib.fe");
    let root = root.to_str().expect("UTF-8 path");

    let lsif = temp.path().join("out.lsif");
    let (output, exit_code) = run_fe_main(&["lsif", root, "-o", lsif.to_str().expect("UTF-8")]);
    assert_eq!(exit_code, 0, "fe lsif failed:\n{output}");
    let lsif = fs::read_to_string(&lsif).expect("read lsif");
    // The name in the quotation and the call `S::arity()` are ranges of the
    // index: each ends five characters after the name starts.
    let ends = |line: usize| {
        let text = source.lines().nth(line).expect("line of the source");
        let start = text.find("arity").expect("arity on the line");
        format!(r#""end":{{"character":{},"line":{line}}}"#, start + 5)
    };
    for range in [ends(6), ends(11)] {
        assert!(
            lsif.contains(&range),
            "the LSIF index has no range {range}:\n{lsif}"
        );
    }
}

#[test]
fn test_cli_test_unsafe_trust_does_not_trust_a_package_by_its_name() {
    // The package `evil` calls itself `std` in its fe.toml. A package is
    // trusted as `core` or `std` only if it is the embedded library or is
    // trusted through the dependency graph, so its `unsafe` code is judged
    // like any dependency's and nothing trusts it.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_named_std");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected a trust error:\n{output}");
    assert!(
        output.contains("which is not allowed to use it") && output.contains("evil::raw()"),
        "unexpected diagnostics:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_follows_the_codec_a_dispatcher_runs() {
    // A `recv` arm that returns a type runs the `Encode` implementation of the
    // type in the dispatcher, and no call names it. `u` wrote the unsafe code
    // of the implementation that its provider generated and of the one it
    // wrote for its own type, and nothing trusts `u`.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_dispatcher");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected trust errors:\n{output}");
    for arm in ["Generated { v: 1 }", "WR { v: 1 }"] {
        assert!(
            output.contains("this call reaches unsafe code in `u`") && output.contains(arm),
            "{arm}: unexpected diagnostics:\n{output}"
        );
    }
    assert_eq!(
        output
            .matches("call reaches unsafe code in `u`, which is not allowed to use it")
            .count(),
        2,
        "one error for each arm:\n{output}"
    );
    assert!(
        output.contains(
            "to trust `u`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml"
        ),
        "unexpected remedy:\n{output}"
    );

    // The entry decodes the arguments of `init` in the same way.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_init");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected a trust error:\n{output}");
    assert!(
        output.contains("call reaches unsafe code in `u`, which is not allowed to use it")
            && output.contains("init(w: WI)"),
        "unexpected diagnostics:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_follows_the_drop_that_drop_elaboration_inserts() {
    // A value that owns drop glue is destroyed where it goes out of scope, by
    // the `drop` of its type, and no call names it. `u` wrote the unsafe code
    // in the `drop` of `WD`, `WE` and `WF`, and nothing trusts `u`: a scope
    // that drops a `WD`, one that drops a value holding a `WE`, and one that
    // drops an array of `WF` are refused. An empty array has no element to
    // destroy, so it needs no trust (`unsafe_trust_drop_empty`).
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_drop");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected trust errors:\n{output}");
    for value in [
        "let d = mk_wd()",
        "let h = Holder { w: mk_we() }",
        "let pair = [mk_wf(), mk_wf()]",
    ] {
        assert!(
            output.contains("this call reaches unsafe code in `u`") && output.contains(value),
            "{value}: unexpected diagnostics:\n{output}"
        );
    }
    assert_eq!(
        output
            .matches("call reaches unsafe code in `u`, which is not allowed to use it")
            .count(),
        3,
        "one error for each dropped type:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_judges_an_unsafe_impl_where_it_is_relied_on_without_a_call_of_its_own()
 {
    // `peek` of the trusted `t` relies on what `unsafe impl Pod` asserts. The
    // one that `v`, which nothing trusts, wrote for its own type is judged
    // where the call to `peek` passes the type, not where it is declared.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_pod_written");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected a trust error:\n{output}");
    assert!(
        output.contains("call relies on an `unsafe impl` that `v` wrote, which is not allowed to use unsafe code")
            && output.contains("peek(VType {})")
            && output.contains("unsafe impl t::Pod for VType {}")
            && output.contains(
                "to trust `v`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml"
            ),
        "unexpected diagnostics:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_judges_an_unsafe_impl_where_a_running_body_relies_on_it() {
    // An `unsafe impl` of `u`, which nothing trusts, is relied on by code that
    // runs: through a bound that it proves (with a method, and without one),
    // and through the default method of its trait. Each is refused at the call
    // that makes the code run, whether or not a method of the impl is called.
    for (fixture, call) in [
        ("unsafe_trust_relied_method_impl", "t::peek_m(u1::Evil {})"),
        ("unsafe_trust_relied_default_method", "u2::Evil2 {}.b()"),
        ("unsafe_trust_relied_marker_impl", "t::peek_e(u0::Evil0 {})"),
    ] {
        let fixture_dir = fe_test_runner_fixture_dir(fixture);
        let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
        let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
        assert_ne!(exit_code, 0, "{fixture}: expected a trust error:\n{output}");
        assert!(
            output.contains("call relies on an `unsafe impl` that `u` wrote")
                && output.contains(call),
            "{fixture}: unexpected diagnostics:\n{output}"
        );
        assert_eq!(
            output
                .matches("which is not allowed to use unsafe code")
                .count(),
            1,
            "{fixture}: one error for the call:\n{output}"
        );
    }
}

#[test]
fn test_cli_test_unsafe_trust_judges_each_shape_of_reliance_on_an_unsafe_impl() {
    // Each program relies on an `unsafe impl` of `u`, which nothing trusts,
    // without calling a method of it: through the supertrait of a bound, the
    // condition of a blanket implementation, the default constant of its
    // trait, a named implementation of `u` that `with` chooses, and the
    // provider of a trait-keyed effect.
    for (fixture, calls) in [
        (
            "unsafe_trust_relied_supertrait",
            &["t::peek_w(u::Sup {})"][..],
        ),
        (
            "unsafe_trust_relied_blanket_condition",
            &["t::show2_it(u::X {})", "u::X {}.show2()"][..],
        ),
        (
            "unsafe_trust_relied_default_constant",
            &["t::read_k2<u::K2>()", "u::K2::K"][..],
        ),
        ("unsafe_trust_relied_untrusted_choice", &["u::Y {}.b()"][..]),
        ("unsafe_trust_relied_effect_provider", &["t::charge2()"][..]),
    ] {
        let fixture_dir = fe_test_runner_fixture_dir(fixture);
        let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
        let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
        assert_ne!(exit_code, 0, "{fixture}: expected a trust error:\n{output}");
        for call in calls {
            assert!(
                output.contains("call relies on an `unsafe impl` that `u` wrote")
                    && output.contains(call),
                "{fixture}: expected a reliance error at `{call}`:\n{output}"
            );
        }
        assert_eq!(
            output
                .matches("which is not allowed to use unsafe code")
                .count(),
            calls.len(),
            "{fixture}: one error for each call:\n{output}"
        );
    }
}

#[test]
fn test_cli_test_unsafe_trust_does_not_judge_an_unsafe_impl_that_a_choice_replaces() {
    // The default method `b` of `Checked` is checked under `Self: Checked`.
    // `with (t::GoodC)` chooses the trusted named implementation `GoodC` for
    // it, so `u`'s ordinary `unsafe impl Checked for Y` never runs and needs
    // no trust.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_relied_trusted_choice");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_eq!(exit_code, 0, "fe test failed:\n{output}");
    assert!(
        output.contains("PASS  [<time>] chosen_default"),
        "expected chosen_default to pass, got:\n{output}"
    );
}

#[test]
fn test_cli_test_unsafe_trust_judges_an_unsafe_fn_by_the_packages_that_placed_it() {
    // `t2` is trusted and wrote the `unsafe fn`; `u` is not and spliced the
    // item into its own quotation, so the function is unsafe code that `u`
    // placed. The call is refused naming `u`, the first package of the chain
    // that is not trusted.
    let fixture_dir = fe_test_runner_fixture_dir("unsafe_trust_splice_item");
    let fixture_dir = fixture_dir.to_str().expect("fixture dir utf8");
    let (output, exit_code) = run_fe_main(&["test", fixture_dir]);
    assert_ne!(exit_code, 0, "expected a trust error:\n{output}");
    assert!(
        output.contains("call reaches unsafe code in `u`, which is not allowed to use it")
            && output.contains("Placed {}.unsafe_fn()")
            && output.contains(
                "to trust `u`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml"
            ),
        "unexpected diagnostics:\n{output}"
    );
    assert!(
        !output.contains("unsafe code in `t2`"),
        "a trusted package is never named:\n{output}"
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
    assert_eq!(
        output.matches("No tests found in").count(),
        2,
        "expected a no-tests warning for each workspace member, got:\n{output}"
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
fn test_cli_workspace_cache_preserves_member_and_dependency_errors_without_tests() {
    for dependency_error in [false, true] {
        let temp = tempdir().expect("workspace fixture");
        let root = temp.path();
        fs::write(root.join("fe.toml"),
            "[workspace]\nname = \"cache_errors\"\nversion = \"0.1.0\"\nmembers = [\"first\", \"second\"]\n"
        ).unwrap();
        for name in ["first", "second", "bad"] {
            fs::create_dir_all(root.join(name).join("src")).unwrap();
            let dependency = if name == "second" && dependency_error {
                "[dependencies]\nbad = { path = \"../bad\" }\n"
            } else {
                ""
            };
            fs::write(
                root.join(name).join("fe.toml"),
                format!("[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n{dependency}"),
            )
            .unwrap();
            let source = match name {
                "first" => "#[test]\nfn first_ok() { core::assert(true) }",
                "second" if dependency_error => "pub fn marker() {}",
                _ => "pub fn invalid(_ value: u256) -> bool { value }",
            };
            fs::write(root.join(name).join("src/lib.fe"), source).unwrap();
        }
        for grouped in [false, true] {
            let mut args = vec!["test", "--jobs", "1"];
            if grouped {
                args.push("--grouped");
            }
            let (output, exit_code) = run_fe_main_in_dir(&args, root);
            assert_ne!(exit_code, 0, "invalid zero-test member passed: {output}");
            assert!(
                output.contains("first_ok") && output.contains("1 passed"),
                "{output}"
            );
            assert!(output.contains("1 failed"), "{output}");
            if dependency_error {
                assert!(output.contains("Errors in dependency"), "{output}");
            }
        }
    }
}

#[test]
fn test_cli_workspace_cache_isolates_workspaces_with_identical_member_names() {
    let temp = tempdir().expect("workspace fixture");
    let mut workspaces = Vec::new();
    for (name, value) in [("left", 1), ("right", 2)] {
        let root = temp.path().join(name);
        fs::create_dir_all(&root).unwrap();
        fs::write(root.join("fe.toml"),
            format!("[workspace]\nname = \"{name}\"\nversion = \"0.1.0\"\nmembers = [\"library\", \"app\"]\n")
        ).unwrap();
        for member in ["library", "app"] {
            fs::create_dir_all(root.join(member).join("src")).unwrap();
            let dependency = if member == "app" {
                "[dependencies]\nlibrary = true\n"
            } else {
                ""
            };
            fs::write(
                root.join(member).join("fe.toml"),
                format!("[ingot]\nname = \"{member}\"\nversion = \"0.1.0\"\n{dependency}"),
            )
            .unwrap();
            let source = if member == "library" {
                format!("pub fn value() -> u256 {{ {value} }}")
            } else {
                format!(
                    "#[test]\nfn member_value() {{ core::assert(library::value() == {value}) }}"
                )
            };
            fs::write(root.join(member).join("src/lib.fe"), source).unwrap();
        }
        workspaces.push(root);
    }
    for grouped in [false, true] {
        let mut args = vec![
            "test",
            "--jobs",
            "1",
            workspaces[0].to_str().unwrap(),
            workspaces[1].to_str().unwrap(),
        ];
        if grouped {
            args.push("--grouped");
        }
        let (output, exit_code) = run_fe_main(&args);
        assert_eq!(exit_code, 0, "{output}");
        assert!(
            output.contains("2 passed") && output.contains("0 failed"),
            "{output}"
        );
    }
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

/// The fixture-directory test runs at the default level only; recursive raw
/// pointees are also executed unoptimized and fully optimized.
#[test]
fn test_recursive_raw_pointers_execute_at_o0_and_o2() {
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/recursive_raw_pointer.fe");
    let fixture = fixture.to_str().expect("fixture path utf8");
    for level in ["-O0", "-O2"] {
        let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", level, fixture]);
        assert_eq!(exit_code, 0, "fe test {level} failed:\n{output}");
        assert!(
            output.contains("9 passed; 0 failed"),
            "expected every recursive raw-pointer test to run at {level}:\n{output}"
        );
    }
}

/// Unoptimized, a fresh object holds whatever its memory last held, so
/// clearing a packed element that is a struct, as `pop` on a `SolArray` of
/// `Uint24` does, must write a zero value rather than copy a fresh object.
#[test]
fn test_sol_storage_collections_execute_at_o0() {
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/sol_storage_collections.fe");
    let fixture = fixture.to_str().expect("fixture path utf8");
    let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", "-O0", fixture]);
    assert_eq!(exit_code, 0, "fe test -O0 failed:\n{output}");
    assert!(
        output.contains("19 passed; 0 failed"),
        "expected every storage collection test to run at -O0:\n{output}"
    );
}

/// Unwinding a successful halt through the frames that hold lane copies
/// differs by optimization level: the fixture directory runs it at the
/// default level only.
#[test]
fn test_lane_holds_before_halts_execute_at_every_level() {
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/lane_holds_before_halts.fe");
    let fixture = fixture.to_str().expect("fixture path utf8");
    for level in ["-O0", "-O1", "-O2", "-Os"] {
        let (output, exit_code) = run_fe_main(&["test", "--jobs", "1", level, fixture]);
        assert_eq!(exit_code, 0, "fe test {level} failed:\n{output}");
        assert!(
            output.contains("11 passed; 0 failed"),
            "expected every halting lane test to run at {level}:\n{output}"
        );
    }
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

#[test]
fn unsupported_macro_calls_are_cli_errors() {
    let dir = tempdir().expect("temp dir");
    let source = dir.path().join("unsupported_macro.fe");
    fs::write(&source, "#[test]\nfn unsupported() { funsig!(x) }\n")
        .expect("write unsupported macro fixture");
    let source = source.to_str().expect("source path utf8");

    for command in ["check", "build", "test"] {
        for recovery in [false, true] {
            let mut args = vec![command, source];
            if command != "test" {
                args.push("--standalone");
            }
            if recovery {
                args.push("--recovery-mode");
            }
            let (output, exit_code) = run_fe_main_in_dir(&args, dir.path());
            assert_eq!(exit_code, 1, "fe {args:?}:\n{output}");
            assert!(output.contains("unsupported macro call"), "{output}");
            assert!(!output.contains("panicked at"), "{output}");
        }
    }
}

#[test]
fn immutable_aggregate_forwarding_at_all_optimization_levels() {
    let fixture = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/immutable_aggregate_forwarding.fe");
    // The fixture test covers the default level (1).
    for level in ["0", "2", "s"] {
        let (output, exit_code) = run_fe_main(&[
            "test",
            "--jobs",
            "1",
            "--optimize",
            level,
            fixture.to_str().expect("fixture path utf8"),
        ]);
        assert_eq!(
            exit_code, 0,
            "aggregate forwarding failed at -O {level}:\n{output}"
        );
    }
}

#[test]
fn effect_aliasing_at_all_optimization_levels() {
    // The fixture tests cover the default level (1).
    for name in ["effect_aliasing", "effect_aliasing_lanes"] {
        let fixture = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join(format!("tests/fixtures/fe_test/{name}.fe"));
        for level in ["0", "2", "s"] {
            let (output, exit_code) = run_fe_main(&[
                "test",
                "--jobs",
                "1",
                "--optimize",
                level,
                fixture.to_str().expect("fixture path utf8"),
            ]);
            assert_eq!(exit_code, 0, "{name} failed at -O {level}:\n{output}");
        }
    }
}

#[test]
fn returned_array_copy_at_all_optimization_levels() {
    let fixture = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/returned_array_copy.fe");
    // The fixture test covers the default level (1).
    for level in ["0", "2", "s"] {
        let (output, exit_code) = run_fe_main(&[
            "test",
            "--jobs",
            "1",
            "--optimize",
            level,
            fixture.to_str().expect("fixture path utf8"),
        ]);
        assert_eq!(exit_code, 0, "array copies failed at -O {level}:\n{output}");
    }
}

#[test]
fn wide_function_arguments_at_all_optimization_levels() {
    let fixture = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/wide_function_arguments.fe");
    // The fixture test covers the default level (1).
    for level in ["0", "2", "s"] {
        let (output, exit_code) = run_fe_main(&[
            "test",
            "--jobs",
            "1",
            "--optimize",
            level,
            fixture.to_str().expect("fixture path utf8"),
        ]);
        assert_eq!(exit_code, 0, "wide calls failed at -O {level}:\n{output}");
    }
}

#[test]
fn payable_returns_at_all_optimization_levels() {
    let fixture =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fe_test/payable_return.fe");
    // The fixture test covers the default level (1).
    for level in ["0", "2", "s"] {
        let (output, exit_code) = run_fe_main(&[
            "test",
            "--jobs",
            "1",
            "--optimize",
            level,
            fixture.to_str().expect("fixture path utf8"),
        ]);
        assert_eq!(
            exit_code, 0,
            "payable returns failed at -O {level}:\n{output}"
        );
    }
}

#[test]
fn test_cli_build_payable_return_abi() {
    let fixture =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fe_test/payable_return.fe");
    let out = tempdir().expect("output directory");
    let (output, exit_code) = run_fe_main(&[
        "build",
        "--standalone",
        "--out-dir",
        out.path().to_str().unwrap(),
        fixture.to_str().unwrap(),
    ]);
    assert_eq!(exit_code, 0, "payable build failed:\n{output}");
    let abi: Value = serde_json::from_str(
        &fs::read_to_string(out.path().join("PayableReturnBox.abi.json")).unwrap(),
    )
    .unwrap();
    let deposit = abi
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["type"] == "function" && entry["name"] == "deposit")
        .expect("deposit ABI entry");
    assert_eq!(deposit["stateMutability"], "payable");
    assert_eq!(
        deposit["outputs"],
        serde_json::json!([{"name": "", "type": "uint256"}])
    );
}

#[test]
fn named_storage_preserves_dispatch_at_both_optimization_levels() {
    for fixture in [
        "named_storage_evidence.fe",
        "named_storage_codecs.fe",
        "named_storage_deferred_codec.fe",
    ] {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/fe_test")
            .join(fixture);
        for level in ["0", "1"] {
            let (output, exit) = run_fe_main(&[
                "test",
                "--backend",
                "sonatina",
                "--jobs",
                "1",
                "-O",
                level,
                path.to_str().unwrap(),
            ]);
            assert_eq!(exit, 0, "{fixture} at O{level}: {output}");
        }
    }
}

// A program that names no implementation is unaffected by the checks that read
// implied bounds from dictionaries. A message field of a recursive type has no
// `Encode<Sol>` dictionary, so the instance of `encode_field` has none to read
// `Node: AbiSize` from; the instance stays admitted, as it was before those
// checks. The refusal where a named implementation is involved is covered by
// `instance_missing_dictionary_refuses_only_with_a_named_implementation` in
// `fe-hir`.
#[test]
fn named_checks_leave_recursive_message_fields_without_choices_alone() {
    for fixture in [
        "named_checks_recursive_field_without_choice.fe",
        "named_checks_phantom_field_without_choice.fe",
    ] {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures")
            .join(fixture);
        let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
        assert_eq!(exit, 0, "{fixture}: {output}");
        assert!(!output.contains("16-0019"), "{fixture}: {output}");
    }
}

#[test]
fn named_storage_aliases_do_not_assume_distinct_dictionaries_are_disjoint() {
    let source = include_str!("fixtures/named_storage_alias.fe");
    let temp = tempdir().unwrap();
    let path = temp.path().join("alias.fe");
    for (choice, should_pass) in [("<Words as StateIndex>", true), ("Shifted", false)] {
        fs::write(
            &path,
            source.replace("with (Shifted)", &format!("with ({choice})")),
        )
        .unwrap();
        let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
        if should_pass {
            assert_eq!(exit, 0, "ordinary distinct keys: {output}");
        } else {
            assert_ne!(
                exit, 0,
                "different dictionaries may select the same location"
            );
            assert!(
                output.contains("16-0001") && output.contains("access conflict"),
                "{output}"
            );
        }
    }
}

#[test]
fn doc_includes_generated_public_methods() {
    let temp = tempdir().unwrap();
    let package = temp.path().join("sample");
    let source_dir = package.join("src");
    fs::create_dir_all(&source_dir).unwrap();
    fs::write(
        temp.path().join("fe.toml"),
        "[workspace]\nname = \"sample_workspace\"\nversion = \"0.1.0\"\nmembers = [\"sample\"]\n",
    )
    .unwrap();
    fs::write(
        package.join("fe.toml"),
        "[ingot]\nname = \"sample\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    let source = source_dir.join("lib.fe");
    fs::write(
        &source,
        r#"
use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item, field_count}

struct Arity {}
impl DeriveInherent for Arity {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { pub const fn arity() -> usize { ${n} } }
    }
}

pub struct Version { pub major: u256, pub minor: u256 }
impl Version with (Arity)
"#,
    )
    .unwrap();

    for (name, path) in [
        ("file", source.as_path()),
        ("package", package.as_path()),
        ("workspace", temp.path()),
    ] {
        let docs = temp.path().join(name);
        let (output, exit) = run_fe_main(&[
            "doc",
            path.to_str().unwrap(),
            "-o",
            docs.to_str().unwrap(),
            "json",
        ]);
        assert_eq!(exit, 0, "{name}: {output}");
        let index: Value =
            serde_json::from_slice(&fs::read(docs.join("docs.json")).unwrap()).unwrap();
        let version = index["index"]["items"]
            .as_array()
            .unwrap()
            .iter()
            .find(|item| item["name"] == "Version")
            .expect("generated type's authored declaration");
        let has_method = version["trait_impls"].as_array().is_some_and(|impls| {
            impls.iter().any(|item| {
                item["methods"]
                    .as_array()
                    .is_some_and(|methods| methods.iter().any(|method| method["name"] == "arity"))
            })
        });
        assert!(has_method, "{name} missing Version.arity: {version}");
    }
}

#[test]
fn named_storage_failed_codec_proofs_do_not_fall_back_to_ordinary() {
    let source = include_str!("fixtures/named_storage_codec_failure.fe");
    let prerequisite = include_str!("fixtures/named_storage_codec_prerequisite.fe");
    let temp = tempdir().unwrap();
    let path = temp.path().join("codec.fe");
    for (name, source, should_pass) in [
        ("failed packed choice", source.to_string(), false),
        (
            "satisfied choice",
            format!("{source}\nimpl Missing for Arg {{}}\n"),
            true,
        ),
        (
            "unused whole-slot codec",
            source.replace("BITS: u256 = 32", "BITS: u256 = 256"),
            true,
        ),
    ] {
        fs::write(&path, source).unwrap();
        let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
        if should_pass {
            assert_eq!(exit, 0, "{name}: {output}");
        } else {
            assert_ne!(exit, 0, "failed chosen codec must not select ordinarily");
            assert!(
                output.contains("16-0008") && output.contains("implementation choices"),
                "{output}"
            );
        }
    }
    for (choice, encoded) in [
        ("Broken", "7"),
        ("Chosen<T>, Broken", "6"),
        ("Chosen, Broken", "6"),
    ] {
        let selected = prerequisite.replace("with (Broken)", &format!("with ({choice})"));
        fs::write(&path, &selected).unwrap();
        let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
        assert_ne!(exit, 0, "failed prerequisite for {choice}: {output}");
        assert!(
            output.contains("16-0008") && output.contains("implementation choices"),
            "{output}"
        );

        let runtime = selected.replace("raw == 7", &format!("raw == {encoded}"));
        fs::write(&path, format!("{runtime}\nimpl Missing for u8 {{}}\n")).unwrap();
        let (output, exit) = run_fe_main(&["test", path.to_str().unwrap()]);
        assert_eq!(
            exit, 0,
            "execute the satisfied specialization for {choice}: {output}"
        );
        assert!(output.contains("1 passed; 0 failed"), "{output}");
    }
}

#[test]
fn named_storage_keeps_choices_in_deferred_codec_proofs() {
    let direct = include_str!("fixtures/named_storage_codec_failure.fe");
    let erased = direct
        .replace(
            "fn read_bound",
            "fn fill<T>(counters: mut Counters<Codec>) {\n    with (Encoded<T>) { counters[1] = 6 }\n}\nfn read_bound",
        )
        .replace(
            "with (Encoded<Arg>) { counters[1] = 6 }",
            "fill<Arg>(counters: mut counters)",
        )
        .replace("with (Encoded<Arg>) { counters[1] }", "raw");
    let runtime = r#"
impl Missing for Arg {}

#[test]
fn deferred_argument_selects_the_codec() uses (evm: mut std::evm::Evm) {
    let c = evm.create2<C>(value: std::evm::Wei::zero(), args: (), salt: 0)
    let values: (u32, u32, u32) = evm.call(
        addr: c, gas: 400000, value: std::evm::Wei::zero(), message: M::Exercise {},
    )
    assert!(values.0 == 5 && values.2 == 17)
}
"#;
    let sol = include_str!("fixtures/named_storage_sol_codec_prerequisite.fe");
    let temp = tempdir().unwrap();
    for (name, source, satisfied, bits) in [
        (
            "erased_argument",
            erased.as_str(),
            format!("{erased}{runtime}"),
            "32",
        ),
        (
            "solidity_prerequisite",
            sol,
            format!("{sol}\nimpl Missing for u8 {{}}\n"),
            "8",
        ),
    ] {
        let path = temp.path().join(format!("{name}.fe"));
        fs::write(&path, source).unwrap();
        let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
        assert_ne!(exit, 0, "{name}: {output}");
        assert!(
            output.contains("16-0008") && output.contains("implementation choices"),
            "{name}: {output}"
        );

        fs::write(&path, satisfied).unwrap();
        let (output, exit) = run_fe_main(&["test", path.to_str().unwrap()]);
        assert_eq!(exit, 0, "{name}: {output}");
        assert!(output.contains("1 passed; 0 failed"), "{name}: {output}");

        let whole_slot = source.replace(&format!("BITS: u256 = {bits}"), "BITS: u256 = 256");
        fs::write(&path, whole_slot).unwrap();
        let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
        assert_eq!(exit, 0, "{name}, unused whole-slot codec: {output}");
    }
}

#[test]
fn named_storage_late_codec_uses_declared_bound_evidence() {
    let source = include_str!("fixtures/named_storage_codec_bound_evidence.fe");
    let repeated = source.replace("SolPacked for Elem<u8, T>", "SolPacked for Elem<T, T>");
    let applied = include_str!("fixtures/named_storage_codec_applied_choice.fe");
    let stronger = include_str!("fixtures/named_storage_codec_stronger_bound.fe");
    let projected = include_str!("fixtures/named_storage_codec_projected_bound.fe");
    let projected_stronger = projected
        .replace("trait Gate {", "trait Gate {\n    type Tag")
        .replace("trait Missing {}\n", "")
        .replace("impl Missing for u8 {}\n", "")
        .replace("impl Gate for u8 {", "impl Gate for u8 {\n    type Tag = u8")
        .replace(
            "impl<T: Missing> Gate for T as Encoded {",
            "impl Gate for u8 as Encoded {\n    type Tag = u8",
        )
        .replace("Encoded<u8>", "Encoded")
        .replace(
            "fn write<K, T: HasItem>(array: mut SolArray<Elem<K, T>>) where T::Item: Gate",
            "fn write<K, T: HasItem>(array: mut SolArray<Elem<K, T>>) where T::Item: Gate<Tag = u8>",
        );
    let blanket = stronger
        .replace("impl Gate for u8 {", "impl<T> Gate for T {")
        .replace(".1 == 6", ".1 == 7");
    let blanket_known_key = blanket.replace(
        "fn write<K, T: Gate<Tag = u8>>(array: mut SolArray<Elem<K, T>>)",
        "fn write<T: Gate<Tag = u8>>(array: mut SolArray<Elem<u8, T>>)",
    );
    let temp = tempdir().unwrap();
    for (name, source, expected) in [
        ("distinct", source, "2 passed; 0 failed"),
        ("repeated", repeated.as_str(), "2 passed; 0 failed"),
        ("applied", applied, "1 passed; 0 failed"),
        ("stronger_bound", stronger, "2 passed; 0 failed"),
        ("projected_bound", projected, "2 passed; 0 failed"),
        (
            "stronger_projected_bound",
            projected_stronger.as_str(),
            "2 passed; 0 failed",
        ),
        ("blanket_impl", blanket.as_str(), "2 passed; 0 failed"),
        (
            "blanket_impl_known_key",
            blanket_known_key.as_str(),
            "2 passed; 0 failed",
        ),
    ] {
        let path = temp.path().join(format!("{name}.fe"));
        fs::write(&path, source).unwrap();
        let (output, exit) = run_fe_main(&["test", path.to_str().unwrap()]);
        assert_eq!(exit, 0, "{name}: {output}");
        assert!(output.contains(expected), "{name}: {output}");
    }
}

#[test]
fn named_storage_deferred_lexical_choices_keep_their_source_parameters() {
    let source = include_str!("fixtures/named_storage_codec_lexical_origin.fe");
    let matching = source
        .replace("Encoded<A>", "Encoded<T>")
        .replace("raw == 7", "raw == 6");
    let unrelated_failure = source
        .replace("impl Missing for u8 {}\n", "")
        .replace("A: Missing", "A")
        .replace("T: Gate + Missing", "T: Gate");
    let temp = tempdir().unwrap();
    for (name, source) in [
        ("unrelated_choice", source),
        ("matching_choice", matching.as_str()),
        ("unrelated_failed_choice", unrelated_failure.as_str()),
    ] {
        let path = temp.path().join(format!("{name}.fe"));
        fs::write(&path, source).unwrap();
        let (output, exit) = run_fe_main(&["test", path.to_str().unwrap()]);
        assert_eq!(exit, 0, "{name}: {output}");
        assert!(output.contains("2 passed; 0 failed"), "{name}: {output}");
    }
}

#[test]
fn named_storage_qualified_clearable_opening_checks_aggregate_fields() {
    let temp = tempdir().unwrap();
    let path = temp.path().join("clearable_opening.fe");
    let source = r#"
use core::marker::Clearable
struct Protected { unsafe invariant: u8 }
fn open() { with (<(Protected,) as Clearable>) {} }
"#;
    fs::write(&path, source.replace("(Protected,)", "(u8,)")).unwrap();
    let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
    assert_eq!(exit, 0, "{output}");

    fs::write(&path, source).unwrap();
    let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
    assert_eq!(exit, 1, "{output}");
    assert!(
        output.contains("Protected") && output.contains("Clearable"),
        "{output}"
    );
}

#[test]
fn named_storage_unrelated_choices_keep_generic_sol_codec_deferral() {
    let temp = tempdir().unwrap();
    let path = temp.path().join("generic_codec.fe");
    fs::write(
        &path,
        r#"
use std::evm::SolArray
trait Marker {}
struct Item {}
impl Marker for Item as Unrelated {}
fn fill<T>(array: mut SolArray<T>, value: own T) {
    with (Unrelated) { array[0] = value }
}
pub fn root(array: mut SolArray<u8>) { fill(array: mut array, value: 1) }
"#,
    )
    .unwrap();
    let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
    assert_eq!(
        exit, 0,
        "unrelated choice must not turn a deferred codec into a failed choice: {output}"
    );
}

#[test]
fn named_storage_opaque_generic_codec_ignores_unrelated_choices() {
    let source = include_str!("fixtures/named_storage_generic_codec.fe");
    let temp = tempdir().unwrap();
    let path = temp.path().join("generic.fe");
    for source in [source.to_string(), source.replace("with (Unrelated)", "")] {
        fs::write(&path, source).unwrap();
        let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
        assert_eq!(
            exit, 0,
            "unrelated choice cannot reject ordinary generic codec: {output}"
        );
    }
}

/// A workspace whose member `tool` has a derive header naming a type that is
/// not a provider and depends on `lib`, which sets another target.
fn write_header_error_workspace(root: &Path) {
    fs::create_dir_all(root.join("lib/src")).unwrap();
    fs::create_dir_all(root.join("tool/src")).unwrap();
    fs::write(
        root.join("fe.toml"),
        "[workspace]\nname = \"ws\"\nversion = \"0.1.0\"\nmembers = [\"tool\"]\n",
    )
    .unwrap();
    fs::write(
        root.join("lib/fe.toml"),
        "[ingot]\nname = \"lib\"\nversion = \"0.1.0\"\ntarget = \"native\"\n",
    )
    .unwrap();
    fs::write(
        root.join("lib/src/lib.fe"),
        "pub fn answer() -> u256 { 0 }\n",
    )
    .unwrap();
    fs::write(
        root.join("tool/fe.toml"),
        "[ingot]\nname = \"tool\"\nversion = \"0.1.0\"\n[dependencies]\nlib = { path = \"../lib\" }\n",
    )
    .unwrap();
    fs::write(
        root.join("tool/src/lib.fe"),
        "struct NotAProvider {}\nstruct S { a: u256 }\nimpl S with (NotAProvider)\n",
    )
    .unwrap();
}

#[test]
fn test_cli_workspace_reports_a_dependency_target_mismatch_before_any_provider_runs() {
    // For a single ingot the mismatch is reported first. In a workspace the
    // header of the member was prepared first, so a provider ran against a
    // dependency that this command does not compile, and its error hid the
    // mismatch.
    let temp = tempdir().unwrap();
    write_header_error_workspace(temp.path());
    for command in ["check", "build"] {
        let (output, exit) = run_fe_main_in_dir(&[command], temp.path());
        assert_eq!(exit, 1, "{command}: {output}");
        assert!(
            output.contains("targets `native`, but this command compiles for"),
            "{command}: {output}"
        );
        assert!(!output.contains("NotAProvider"), "{command}: {output}");
    }
}

#[test]
fn test_cli_check_locates_a_generation_error_by_line_and_column() {
    let temp = tempdir().unwrap();
    let path = temp.path().join("bad_header.fe");
    fs::write(
        &path,
        "struct NotAProvider {}\nstruct S { a: u256 }\nimpl S with (NotAProvider)\n",
    )
    .unwrap();
    let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
    assert_eq!(exit, 1, "{output}");
    assert!(output.contains("bad_header.fe:3:1: "), "{output}");
    assert!(!output.contains(".fe:44..70"), "{output}");
}

#[test]
fn test_cli_report_records_a_generation_error() {
    // A report of a failed command holds the generation error, as the report
    // of `fe check` on a package does.
    let temp = tempdir().unwrap();
    let path = temp.path().join("bad_header.fe");
    fs::write(
        &path,
        "struct NotAProvider {}\nstruct S { a: u256 }\nimpl S with (NotAProvider)\n",
    )
    .unwrap();
    for command in ["check", "build"] {
        let archive = temp.path().join(format!("{command}-report.tar.gz"));
        let args = [
            command,
            "--report",
            "--report-out",
            archive.to_str().unwrap(),
            path.to_str().unwrap(),
        ];
        let (output, exit) = run_fe_main(&args);
        assert_eq!(exit, 1, "{command}: {output}");
        let listing = Command::new("tar")
            .arg("-tzf")
            .arg(&archive)
            .output()
            .expect("tar lists the report");
        let listing = String::from_utf8_lossy(&listing.stdout).to_string();
        let entry = listing
            .lines()
            .find(|line| line.ends_with("errors/generation.txt"))
            .unwrap_or_else(|| panic!("{command}: no generation error in the report:\n{listing}"));
        let text = Command::new("tar")
            .arg("-xzOf")
            .arg(&archive)
            .arg(entry)
            .output()
            .expect("tar reads the entry");
        assert!(
            String::from_utf8_lossy(&text.stdout).contains("NotAProvider"),
            "{command}: {text:?}"
        );
    }
}

#[test]
fn named_output_possible_overlap_is_refused() {
    // The named header and the ordinary one overlap where `T::Out` is `u8`,
    // and give different `Result` types there.
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/named_output_possible_overlap.fe");
    let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
    assert_ne!(exit, 0, "{output}");
    assert!(output.contains("5-0005"), "{output}");
    assert!(output.contains("may overlap"), "{output}");

    // The same shape with equal outputs is accepted and runs the named one.
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/named_output_possible_overlap_agree.fe");
    let (output, exit) = run_fe_main(&["test", "--jobs", "1", path.to_str().unwrap()]);
    assert_eq!(exit, 0, "{output}");
}

const OUTPUT_AGREEMENT_A: &str = r#"pub trait Marker {}
pub trait Foo {
    type Out
    fn make() -> Self::Out
}
impl<T: Marker> Foo for T {
    type Out = u8
    fn make() -> u8 { 1 }
}
pub impl<T: Marker> Foo for T as NamedInA {
    type Out = u8
    fn make() -> u8 { 2 }
}
"#;

fn write_output_agreement_ingots(root: &Path, b_source: &str) {
    for (path, content) in [
        ("a/fe.toml", "[ingot]\nname = \"a\"\nversion = \"0.1.0\"\n"),
        ("a/src/lib.fe", OUTPUT_AGREEMENT_A),
        (
            "b/fe.toml",
            "[ingot]\nname = \"b\"\nversion = \"0.1.0\"\n\n[dependencies]\na = { path = \"../a\" }\n",
        ),
        ("b/src/lib.fe", b_source),
    ] {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).expect("create dir");
        fs::write(path, content).expect("write fixture");
    }
}

#[test]
fn named_output_agreement_across_ingots() {
    // An ordinary implementation in a downstream ingot gives `S` other
    // outputs than the upstream named implementation a use chooses.
    let temp = tempdir().expect("tempdir");
    write_output_agreement_ingots(
        temp.path(),
        r#"use a::{Foo, Marker, NamedInA}

struct S {}
impl Foo for S {
    type Out = (u256, u256)
    fn make() -> (u256, u256) { (1, 2) }
}
impl Marker for S as FakeMarker {}

fn call<T: Foo>() -> T::Out { T::make() }
fn boom() -> (u256, u256) { with (NamedInA, FakeMarker) { call<S>() } }
"#,
    );
    let b = temp.path().join("b");
    let (output, exit) = run_fe_main(&["check", b.to_str().unwrap()]);
    assert_ne!(exit, 0, "{output}");
    assert!(output.contains("16-0020"), "{output}");
    assert!(output.contains("call<S>()"), "{output}");
    assert!(!output.contains("16-0004"), "{output}");

    // With equal outputs the use is accepted and runs the named implementation.
    let temp = tempdir().expect("tempdir");
    write_output_agreement_ingots(
        temp.path(),
        r#"use a::{Foo, Marker, NamedInA}

struct S {}
impl Foo for S {
    type Out = u8
    fn make() -> u8 { 1 }
}
impl Marker for S as FakeMarker {}

fn call<T: Foo>() -> T::Out { T::make() }
fn boom() -> u8 { with (NamedInA, FakeMarker) { call<S>() } }

#[test]
fn t() { assert(boom() == 2) }
"#,
    );
    let b = temp.path().join("b");
    let (output, exit) = run_fe_main(&["test", "--jobs", "1", b.to_str().unwrap()]);
    assert_eq!(exit, 0, "{output}");
}

#[test]
fn named_condition_cannot_change_ordinary_outputs() {
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    // An ordinary blanket implementation whose condition holds only through a
    // choice, beside an ordinary implementation with other outputs.
    let path = fixtures.join("named_condition_outputs.fe");
    let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
    assert_ne!(exit, 0, "{output}");
    assert!(output.contains("16-0020"), "{output}");
    assert!(output.contains("fn gen"), "{output}");
    assert!(output.contains("<T as Foo>::make()"), "{output}");
    assert!(!output.contains("16-0004"), "{output}");
    assert!(
        output.contains("`S: Foo` is answered here by `impl<T: Marker> Foo for T`"),
        "{output}"
    );
    assert!(!output.contains("Foo<Out"), "{output}");

    // No ordinary implementation defines the outputs at the goal.
    let path = fixtures.join("named_condition_no_ordinary_outputs.fe");
    let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
    assert_ne!(exit, 0, "{output}");
    assert!(output.contains("16-0020"), "{output}");
    assert!(
        output.contains("no ordinary implementation defines its associated outputs"),
        "{output}"
    );
    assert!(!output.contains("16-0004"), "{output}");

    // The same in a generic body whose instance picks the choice.
    let path = fixtures.join("named_condition_generic_outputs.fe");
    let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
    assert_ne!(exit, 0, "{output}");
    assert!(output.contains("16-0020"), "{output}");
    assert!(output.contains("fn g"), "{output}");
    assert!(!output.contains("16-0004"), "{output}");

    // With equal outputs the blanket implementation runs.
    let temp = tempdir().unwrap();
    let path = temp.path().join("agree.fe");
    // The fixture is read as checked out, so its line endings depend on the
    // platform. Each replaced fragment stays within one line, and each must
    // match exactly once, so a fragment that no longer matches fails here
    // instead of leaving the fixture unchanged.
    let mut agreeing = fs::read_to_string(fixtures.join("named_condition_outputs.fe")).unwrap();
    for (from, to) in [
        ("type Out = u8", "type Out = u256"),
        ("fn make() -> u8 { 2 }", "fn make() -> u256 { 2 }"),
        ("fn boom() -> u8", "fn boom() -> u256"),
        ("assert(boom() == 2)", "assert(boom() == 1)"),
    ] {
        assert_eq!(agreeing.matches(from).count(), 1, "`{from}` in {agreeing}");
        agreeing = agreeing.replace(from, to);
    }
    fs::write(&path, agreeing).unwrap();
    let (output, exit) = run_fe_main(&["test", "--jobs", "1", path.to_str().unwrap()]);
    assert_eq!(exit, 0, "{output}");
}

#[test]
fn named_storage_entry_codec_identity() {
    // Two reads of one entry of a generic collection, one under a codec
    // choice and one without, run their own codecs. Two reads under the same
    // choice share a dispatch.
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/named_storage_entry_codec_identity.fe");
    let (output, exit) = run_fe_main(&["test", "--jobs", "1", path.to_str().unwrap()]);
    assert_eq!(exit, 0, "{output}");
}

#[test]
fn named_storage_local_generic_choice() {
    // A named implementation declared inside a generic function applies, with
    // the function's own arguments, to a codec proof that completes at the
    // instance. The control has a header that matches at check time.
    for fixture in [
        "named_storage_local_generic_choice.fe",
        "named_storage_local_generic_codec.fe",
    ] {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/fe_test")
            .join(fixture);
        let (output, exit) = run_fe_main(&["test", "--jobs", "1", path.to_str().unwrap()]);
        assert_eq!(exit, 0, "{fixture}: {output}");
    }
}

#[test]
fn concrete_component_runtime() {
    // A goal on a concrete type that the dictionary of a pinned bound holds
    // reads that dictionary: in a function, an implementation's method, a
    // closure and a function item. A named implementation with a generic
    // condition opens in a function whose bound pins the condition's
    // associated type. A `for` loop and the call it sits beside read one
    // iteration, and an indexing read and a call read one lane codec.
    for fixture in [
        "concrete_component_call_bound.fe",
        "concrete_component_impl_method.fe",
        "concrete_component_pinned_named_condition.fe",
        "concrete_component_closure_item.fe",
        "concrete_component_loop_plan.fe",
        "concrete_component_entry_codec.fe",
    ] {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/fe_test")
            .join(fixture);
        let (output, exit) = run_fe_main(&["test", "--jobs", "1", path.to_str().unwrap()]);
        assert_eq!(exit, 0, "{fixture}: {output}");
    }
}

#[test]
fn named_function_item_and_spliced_row_forms() {
    // A function item passed to a callable bound runs with the proofs of its
    // bounds made where it is passed, in the forms of a trait method, a
    // method with its own bound, a function with an effect passed under
    // several providers, and a relay through a generic body. A spliced row
    // supplies its components in the forms of two components, a direct effect
    // before the row, a supertrait of the key trait and two generic bodies.
    for (fixture, passed) in [
        ("named_function_item_trait_method.fe", 7),
        ("named_function_item_forms.fe", 6),
        ("named_function_item_providers.fe", 1),
        ("named_function_item_method_bound.fe", 3),
        ("named_function_item_relay_by_choice.fe", 3),
        ("named_function_item_relay_named_only.fe", 3),
        ("named_spliced_row_forms.fe", 7),
    ] {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/fe_test")
            .join(fixture);
        let (output, exit) = run_fe_main(&["test", "--jobs", "1", path.to_str().unwrap()]);
        assert_eq!(exit, 0, "{fixture}: {output}");
        assert!(
            output.contains(&format!("test result: ok. {passed} passed; 0 failed")),
            "{fixture}: {output}"
        );
    }
}

#[test]
fn test_cli_check_overlap_sees_the_checking_ingots_implementations() {
    // Two implementations of a foreign trait overlap through an implementation
    // of a foreign bound that the checking ingot declares, which the trait's
    // own ingot cannot see.
    let conflicting = fe_test_runner_fixture_dir("overlap_foreign_trait");
    let (output, exit_code) = run_fe_main(&["check", conflicting.to_str().expect("utf8")]);
    assert_ne!(exit_code, 0, "{output}");
    assert!(
        output.contains("conflicting trait implementations"),
        "{output}"
    );

    // Where one of them names a type the bound does not cover, they stay disjoint.
    let disjoint = fe_test_runner_fixture_dir("overlap_foreign_trait_disjoint");
    let (output, exit_code) = run_fe_main(&["check", disjoint.to_str().expect("utf8")]);
    assert_eq!(exit_code, 0, "{output}");
}

/// Runs `fe` and fails the test, killing the process, if it has not finished
/// within `limit`. A compiler that does not stop is the failure under test.
fn run_fe_main_within(args: &[&str], limit: std::time::Duration) -> (String, i32) {
    use std::process::Stdio;
    use std::time::Instant;

    let dir = tempdir().expect("tempdir");
    let stdout = fs::File::create(dir.path().join("stdout")).expect("stdout file");
    let stderr = fs::File::create(dir.path().join("stderr")).expect("stderr file");
    let mut child = Command::new(fe_binary())
        .args(args)
        .env("NO_COLOR", "1")
        .stdout(Stdio::from(stdout))
        .stderr(Stdio::from(stderr))
        .spawn()
        .unwrap_or_else(|_| panic!("Failed to spawn fe {args:?}"));
    let start = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait().expect("wait for fe") {
            break Some(status);
        }
        if start.elapsed() > limit {
            child.kill().expect("kill fe");
            child.wait().expect("reap fe");
            break None;
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    };
    let read = |name: &str| {
        normalize_output(&String::from_utf8_lossy(
            &fs::read(dir.path().join(name)).expect("read output"),
        ))
    };
    let output = format!("{}{}", read("stdout"), read("stderr"));
    let Some(status) = status else {
        panic!("fe {args:?} did not finish within {limit:?}: {output}");
    };
    (output, status.code().unwrap_or(-1))
}

#[test]
fn named_polymorphic_recursion_reaches_the_instance_limit() {
    // The recursive call's dictionary holds a named implementation, and every
    // instance of `deep` is new. The instance limit ends the chain, as it does
    // for the same recursion without a choice or with the choice inside the
    // body.
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    for name in [
        "named_polyrec_control.fe",
        "named_polyrec_local_choice.fe",
        "named_polyrec_named.fe",
    ] {
        let path = fixtures.join(name);
        let (output, exit) = run_fe_main_within(
            &["check", path.to_str().unwrap()],
            std::time::Duration::from_secs(120),
        );
        assert_ne!(exit, 0, "{name}: {output}");
        assert!(output.contains("16-0016"), "{name}: {output}");
    }
}

#[test]
fn named_condition_answered_goal_needs_ordinary_outputs() {
    // A goal with outputs, answered only through a choice, is refused even
    // where the program reads no output: no ordinary implementation defines
    // the outputs the type checker would assume for it.
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/named_unused_outputs.fe");
    let (output, exit) = run_fe_main(&["check", path.to_str().unwrap()]);
    assert_ne!(exit, 0, "{output}");
    assert!(output.contains("16-0020"), "{output}");
    assert!(
        output.contains(
            "`S: Foo` is answered here by `impl<T: Marker> Foo for T`, but no ordinary implementation defines its associated outputs at `S: Foo`"
        ),
        "{output}"
    );
    assert!(!output.contains("Foo<Out"), "{output}");
    assert!(!output.contains("16-0004"), "{output}");
}

#[test]
fn named_constant_computed_under_its_own_choice() {
    // The constant of a named implementation runs a method of the same
    // implementation under `with`. Checking the constant checks the
    // computation, which uses the implementation again.
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    let limit = std::time::Duration::from_secs(120);
    let path = fixtures.join("named_const_cycle.fe");
    let (output, exit) =
        run_fe_main_within(&["test", "--jobs", "1", path.to_str().unwrap()], limit);
    assert_eq!(exit, 0, "{output}");

    let path = fixtures.join("named_const_cycle_disagree.fe");
    let (output, exit) = run_fe_main_within(&["check", path.to_str().unwrap()], limit);
    assert_ne!(exit, 0, "{output}");
    assert!(output.contains("5-0005"), "{output}");
}

#[test]
fn named_output_walk_does_not_follow_a_body_with_errors() {
    // `boom` fails internally because `gen<S>` under a named choice disagrees
    // with ordinary normalization, so admission looks for a disagreement in the
    // instances `boom` calls. The body of `deep` has an unresolved name, and
    // its calls are not followed: `deep` calls itself at an ever-growing type,
    // and no instance limit is reported for a body with errors.
    let path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/named_walk_blocked_body.fe");
    let (output, exit) = run_fe_main_within(
        &["check", path.to_str().unwrap()],
        std::time::Duration::from_secs(120),
    );
    assert_ne!(exit, 0, "{output}");
    assert!(output.contains("8-0012"), "{output}");
    assert!(!output.contains("16-"), "{output}");
}

fn inherited_default_choices_fixture(name: &str) -> String {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/inherited_default_choices")
        .join(name)
        .to_str()
        .expect("utf-8 path")
        .to_string()
}

#[test]
fn inherited_default_under_a_named_implementation_is_the_implementations_own() {
    // The default of `K` inherited by a named implementation is checked as if
    // the implementation had written the same text: `Self::m()` is the named
    // implementation's, so `K` is 7 there and the ordinary implementation's `K`
    // is 1, which is refused as it is where the named implementation writes the
    // constant itself.
    for name in ["named_inherits_default.fe", "named_writes_default.fe"] {
        let (output, exit) = run_fe_main(&["test", &inherited_default_choices_fixture(name)]);
        assert_ne!(exit, 0, "{name}: expected a refusal:\n{output}");
        assert!(
            output.contains("error[5-0005]")
                && output.contains("`K` must agree across implementations of this slot"),
            "{name}: expected 5-0005 on `K`:\n{output}"
        );
        assert!(
            !output.contains("16-0004"),
            "{name}: expected no internal error:\n{output}"
        );
    }
}

#[test]
fn inherited_default_proofs_come_from_the_implementations_bounds() {
    // Defaults checked for an implementation that have no named implementation
    // to open keep their values, read directly, under `with` and through a
    // generic reader.
    for name in [
        "supertrait_method.fe",
        "supertrait_chosen_by_with.fe",
        "generic_impl_assoc_bound.fe",
        "self_other.fe",
        "generic_impl_supertrait_condition.fe",
        "trait_param_bound.fe",
    ] {
        let (output, exit) = run_fe_main(&["test", &inherited_default_choices_fixture(name)]);
        assert_eq!(exit, 0, "{name}: expected acceptance:\n{output}");
    }
}

#[test]
fn opened_implementation_is_a_constant_candidate_only_where_its_conditions_hold() {
    // An implementation opened by `with` whose condition does not hold for the
    // receiver is not a candidate for a constant, as it is not for a method.
    let (output, exit) = run_fe_main(&[
        "test",
        &inherited_default_choices_fixture("opened_condition_unmet.fe"),
    ]);
    assert_eq!(exit, 0, "expected both reads to select `T1`:\n{output}");

    // Where it holds, the constant and the method are ambiguous alike.
    let (output, exit) = run_fe_main(&[
        "check",
        &inherited_default_choices_fixture("opened_condition_met.fe"),
    ]);
    assert_ne!(exit, 0, "{output}");
    assert!(
        output.contains("error[2-0018]") && output.contains("`K` is ambiguous"),
        "expected the constant to be ambiguous:\n{output}"
    );
    assert!(
        output.contains("error[2-0016]"),
        "expected the method to be ambiguous:\n{output}"
    );
}

#[test]
fn path_applications_of_an_inherited_default_are_checked_for_the_implementation() {
    // `Bounded<{ Self::N }>` in an inherited default is refused for an
    // implementation whose `N` is 0, as it is where the implementation writes
    // the same text. The default is refused at the implementation, with the
    // unmet requirement as the reason.
    let (output, exit) = run_fe_main(&[
        "check",
        &inherited_default_choices_fixture("path_application_written.fe"),
    ]);
    assert_ne!(exit, 0, "{output}");
    assert!(
        output.contains("error[8-0093]") && output.contains("condition evaluated to `false`"),
        "expected 8-0093:\n{output}"
    );
    let (output, exit) = run_fe_main(&[
        "check",
        &inherited_default_choices_fixture("path_application_inherited.fe"),
    ]);
    assert_ne!(exit, 0, "{output}");
    assert!(
        output.contains("error[8-0167]")
            && output.contains("the default value of `K` does not type-check")
            && output.contains("const requirement is not satisfied"),
        "expected the default refused at the implementation:\n{output}"
    );
}

fn header_generation_fixture(name: &str) -> String {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/header_generation")
        .join(name)
        .to_str()
        .expect("utf-8 path")
        .to_string()
}

#[test]
fn a_field_type_over_a_projection_or_a_compound_constant_is_spliced_under_a_generic_header() {
    // The field type of `S<T: Has>` is `T::Out` and of `S<const N>` is
    // `[u8; N + 1]`. A generic header gives the parameters its own names, and
    // the spliced type is the declared field type at the header's arguments,
    // as the handwritten implementation writes it.
    for name in [
        "field_type_projection_handwritten.fe",
        "field_type_projection.fe",
        "field_type_compound_const_handwritten.fe",
        "field_type_compound_const.fe",
    ] {
        let (output, exit) = run_fe_main(&["test", &header_generation_fixture(name)]);
        assert_eq!(exit, 0, "{name}: expected acceptance:\n{output}");
    }
}
