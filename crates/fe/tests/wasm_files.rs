#![cfg(feature = "wasm")]

//! `fe build --backend wasm` on programs that use the files contract
//! (`std::wasm::files`): text in, handles, and errors.
//!
//! `tests/fixtures/wasm_files/file_ops.fe` is built twice from one file. In the
//! host build the four operations are imports of the module `env`, and the Node
//! host of `examples/wasm/host.mjs` supplies them. In the mock build the files
//! are held in Fe (`std::wasm::files::mock`) and the module has no imports. The
//! two builds, at the four optimization levels (0, 1, 2 and s), must give the
//! replies in `CASES`, which were written down from the contents of the files
//! before any of them ran: a present file (`Ok(byte)`), a missing file
//! (`Err(NotFound)`), a read past the end (`Err(OutOfRange)`) and the rest.
//!
//! The host here is Node, driven by `tests/wasm_support/files_driver.mjs`. Node
//! must be installed: the tests fail, and do not skip, without it. A line of the
//! fixture that starts with `//host:` or `//mock:` belongs to that build only.

mod wasm_support;

use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use serde_json::{Value, json};
use tempfile::tempdir;
use wasm_support::{build_wasm, engine};
use wasmtime::{FuncType, Module, ValType};

/// The files of the host build, and the files of the mock in the fixture.
const FILES: [(&str, &str); 4] = [
    ("hello.txt", "Hello, world"),
    ("digits", "0123456789"),
    ("empty", ""),
    ("grüß", "grüß"),
];

const LEVELS: [&str; 4] = ["0", "1", "2", "s"];

// ---------------------------------------------------------------------------
// Cases.
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, Debug)]
enum Arg {
    Text(&'static str),
    Num(u64),
    /// A 64-bit parameter.
    Wide(u64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Want {
    /// `Reply::ok(value)`.
    Good(u32),
    /// A reply with the error of this name.
    Bad(&'static str),
    /// A scalar result.
    Scalar(u64),
    /// An export with no result.
    Nothing,
    /// The module traps.
    Trap,
}

use Arg::{Num, Text, Wide};
use Want::{Bad, Good, Nothing, Scalar, Trap};

struct Case {
    export: &'static str,
    args: &'static [Arg],
    want: Want,
    /// Handles the call opens, and closes unless it is `leak`.
    handles: u32,
}

const fn case(export: &'static str, args: &'static [Arg], want: Want, handles: u32) -> Case {
    Case {
        export,
        args,
        want,
        handles,
    }
}

// Byte values of the files. hello.txt: 72 101 108 108 111 44 32 119 111 114 108
// 100 (sum 1128). digits: 48 to 57 (sum 525). grüß: 103 114 195 188 195 159
// (sum 954).
const CASES: &[Case] = &[
    // A present file, a missing file, and a read past the end.
    case("byte_at", &[Text("hello.txt"), Num(0)], Good(72), 1),
    case("byte_at", &[Text("hello.txt"), Num(1)], Good(101), 1),
    case("byte_at", &[Text("hello.txt"), Num(11)], Good(100), 1),
    case(
        "byte_at",
        &[Text("hello.txt"), Num(12)],
        Bad("OutOfRange"),
        1,
    ),
    case(
        "byte_at",
        &[Text("hello.txt"), Num(4294967295)],
        Bad("OutOfRange"),
        1,
    ),
    case("byte_at", &[Text("digits"), Num(9)], Good(57), 1),
    case("byte_at", &[Text("digits"), Num(10)], Bad("OutOfRange"), 1),
    case("byte_at", &[Text("empty"), Num(0)], Bad("OutOfRange"), 1),
    case("byte_at", &[Text("grüß"), Num(2)], Good(195), 1),
    case("byte_at", &[Text("grüß"), Num(5)], Good(159), 1),
    case("byte_at", &[Text("grüß"), Num(6)], Bad("OutOfRange"), 1),
    case("byte_at", &[Text("nope"), Num(0)], Bad("NotFound"), 0),
    case("byte_at", &[Text("Hello.txt"), Num(0)], Bad("NotFound"), 0),
    case("byte_at", &[Text("hello.tx"), Num(0)], Bad("NotFound"), 0),
    case("byte_at", &[Text("hello.txt "), Num(0)], Bad("NotFound"), 0),
    // Same length as "digits" (6 bytes) and as "grüß" (6 bytes), but different bytes.
    case("byte_at", &[Text("digitz"), Num(0)], Bad("NotFound"), 0),
    case("byte_at", &[Text("grüss"), Num(0)], Bad("NotFound"), 0),
    case("byte_at", &[Text(""), Num(0)], Bad("BadName"), 0),
    case("size_of", &[Text("hello.txt")], Good(12), 1),
    case("size_of", &[Text("digits")], Good(10), 1),
    case("size_of", &[Text("empty")], Good(0), 1),
    case("size_of", &[Text("grüß")], Good(6), 1),
    case("size_of", &[Text("nope")], Bad("NotFound"), 0),
    case("size_of", &[Text("")], Bad("BadName"), 0),
    case("checksum", &[Text("hello.txt")], Good(1128), 1),
    case("checksum", &[Text("digits")], Good(525), 1),
    case("checksum", &[Text("empty")], Good(0), 1),
    case("checksum", &[Text("grüß")], Good(954), 1),
    case("checksum", &[Text("nope")], Bad("NotFound"), 0),
    case("open_n", &[Text("digits"), Num(0)], Good(0), 0),
    case("open_n", &[Text("digits"), Num(1)], Good(1), 1),
    case("open_n", &[Text("digits"), Num(50)], Good(50), 50),
    case("open_n", &[Text("nope"), Num(3)], Bad("NotFound"), 0),
    case("open_n", &[Text("nope"), Num(0)], Good(0), 0),
    case("open_n", &[Text(""), Num(2)], Bad("BadName"), 0),
    // Two handles live at once.
    case(
        "sizes_two",
        &[Text("digits"), Text("hello.txt")],
        Good(22),
        2,
    ),
    case("sizes_two", &[Text("empty"), Text("empty")], Good(0), 2),
    case("sizes_two", &[Text("grüß"), Text("grüß")], Good(12), 2),
    case(
        "sizes_two",
        &[Text("hello.txt"), Text("nope")],
        Bad("NotFound"),
        1,
    ),
    case(
        "sizes_two",
        &[Text("nope"), Text("hello.txt")],
        Bad("NotFound"),
        0,
    ),
    case("sizes_two", &[Text("digits"), Text("")], Bad("BadName"), 1),
    // Text with no host call.
    case("text_len", &[Text("")], Good(0), 0),
    case("text_len", &[Text("abc")], Good(3), 0),
    case("text_len", &[Text("grüß")], Good(6), 0),
    case("text_byte", &[Text("abc"), Num(0)], Good(97), 0),
    case("text_byte", &[Text("abc"), Num(2)], Good(99), 0),
    case("text_byte", &[Text("abc"), Num(3)], Bad("OutOfRange"), 0),
    case("text_byte", &[Text("grüß"), Num(3)], Good(188), 0),
    case("text_byte", &[Text(""), Num(0)], Bad("OutOfRange"), 0),
    // Numbers on both sides of a text.
    case("mixed", &[Num(7), Text("abc"), Wide(5)], Good(15), 0),
    case("mixed", &[Num(0), Text(""), Wide(0)], Good(0), 0),
    case(
        "mixed",
        &[Num(1), Text("grüß"), Wide(4294967299)],
        Good(10),
        0,
    ),
    case("mixed", &[Num(4294967295), Text("ab"), Wide(0)], Trap, 0),
    case("plain", &[Num(3), Num(4)], Scalar(7), 0),
    case("plain", &[Num(4294967295), Num(1)], Trap, 0),
    // An export with no result.
    case("touch", &[Text("digits")], Nothing, 1),
    case("touch", &[Text("nope")], Nothing, 0),
];

/// The calls that leave a handle open, and what they report.
const LEAKS: &[Case] = &[
    case("leak", &[Text("digits")], Good(0), 1),
    case("leak", &[Text("nope")], Bad("NotFound"), 0),
    case("leak", &[Text("")], Bad("BadName"), 0),
];

// ---------------------------------------------------------------------------
// Building and running.
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Flavour {
    Host,
    Mock,
}

impl Flavour {
    fn marker(self) -> &'static str {
        match self {
            Self::Host => "//host: ",
            Self::Mock => "//mock: ",
        }
    }

    fn other(self) -> &'static str {
        match self {
            Self::Host => "//mock: ",
            Self::Mock => "//host: ",
        }
    }
}

fn tests_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests")
}

fn fixture(file: &str) -> String {
    let path = tests_dir().join(format!("fixtures/wasm_files/{file}.fe"));
    fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

/// The fixture with the lines of the other build removed and the marker of this
/// one taken off.
fn render(source: &str, flavour: Flavour) -> String {
    let mut out = String::new();
    for line in source.lines() {
        if line.starts_with(flavour.other()) {
            continue;
        }
        out.push_str(line.strip_prefix(flavour.marker()).unwrap_or(line));
        out.push('\n');
    }
    out
}

fn build(file: &str, flavour: Flavour, level: &str, temp: &Path) -> (PathBuf, Vec<u8>) {
    let dir = temp.join(format!("{file}-{flavour:?}-{level}"));
    fs::create_dir_all(&dir).unwrap();
    let source = dir.join(format!("{file}.fe"));
    fs::write(&source, render(&fixture(file), flavour)).unwrap();
    let bytes = build_wasm(&source, level, &dir.join("out"));
    (dir.join("out").join(format!("{file}.wasm")), bytes)
}

fn json_arg(arg: &Arg) -> Value {
    match arg {
        Text(text) => json!(text),
        Num(number) => json!(number),
        Wide(number) => json!({ "u64": number.to_string() }),
    }
}

/// Runs the calls in one instance with the Node host and returns its report.
fn node_run(wasm: &Path, files: &[(&str, &str)], calls: Vec<Value>) -> Value {
    let request = json!({
        "wasm": wasm,
        "files": files.iter().cloned().collect::<std::collections::BTreeMap<_, _>>(),
        "calls": calls,
    });
    let mut child = Command::new("node")
        .arg(tests_dir().join("wasm_support/files_driver.mjs"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("node must be installed to run these tests");
    std::io::Write::write_all(
        child.stdin.as_mut().unwrap(),
        request.to_string().as_bytes(),
    )
    .unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(
        output.status.success(),
        "the Node driver failed:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).expect("the driver prints JSON")
}

fn call_json(case: &Case, reply: bool) -> Value {
    json!({
        "export": case.export,
        "args": case.args.iter().map(json_arg).collect::<Vec<_>>(),
        "reply": reply,
    })
}

/// Whether the export returns a `Reply`.
fn returns_reply(case: &Case) -> bool {
    !matches!(case.export, "plain" | "touch")
}

/// Whether `got`, a result from the driver, is what `case` wants.
fn matches_want(got: &Value, want: &Want) -> bool {
    match want {
        Good(value) => got["kind"] == "reply" && got["ok"] == true && got["value"] == *value,
        Bad(name) => got["kind"] == "reply" && got["ok"] == false && got["error"] == *name,
        Scalar(value) => got["kind"] == "value" && got["value"] == json!(value.to_string()),
        Nothing => got["kind"] == "none",
        Trap => got["kind"] == "trap",
    }
}

fn run_cases(wasm: &Path, files: &[(&str, &str)], cases: &[&Case]) -> Value {
    let calls = cases
        .iter()
        .map(|case| call_json(case, returns_reply(case)))
        .collect();
    node_run(wasm, files, calls)
}

#[test]
fn the_mock_and_the_host_give_the_replies_that_were_written_down() {
    let temp = tempdir().unwrap();
    let cases: Vec<&Case> = CASES.iter().collect();
    for level in LEVELS {
        let (host_path, _) = build("file_ops", Flavour::Host, level, temp.path());
        let (mock_path, _) = build("file_ops", Flavour::Mock, level, temp.path());
        let host = run_cases(&host_path, &FILES, &cases);
        let mock = run_cases(&mock_path, &[], &cases);
        for (idx, case) in cases.iter().enumerate() {
            for (which, report) in [("host", &host), ("mock", &mock)] {
                let got = &report["results"][idx];
                assert!(
                    matches_want(got, &case.want),
                    "{which} build at O{level}: {}({:?}) gave {got}, expected {:?}",
                    case.export,
                    case.args,
                    case.want
                );
            }
            assert_eq!(
                host["results"][idx], mock["results"][idx],
                "the builds differ at O{level} for {}({:?})",
                case.export, case.args
            );
        }
        // Every opened handle was closed once: the host's counters match the
        // handles that the cases open, and nothing is open at the end.
        let opened: u32 = cases.iter().map(|case| case.handles).sum();
        assert_eq!(host["opened"], opened, "O{level}");
        assert_eq!(host["closed"], opened, "O{level}");
        assert_eq!(host["open"], 0, "O{level}");
        // Per call, the number of handles that are open between calls is zero.
        for after in host["after"].as_array().unwrap() {
            assert_eq!(after["open"], 0, "O{level}");
        }
    }
}

#[test]
fn a_handle_that_is_not_closed_is_counted_by_the_host_and_the_mock() {
    let temp = tempdir().unwrap();
    let cases: Vec<&Case> = LEAKS.iter().collect();
    for level in LEVELS {
        let (host_path, _) = build("file_ops", Flavour::Host, level, temp.path());
        let (mock_path, _) = build("file_ops", Flavour::Mock, level, temp.path());
        let host = run_cases(&host_path, &FILES, &cases);
        let leaked: u32 = cases.iter().map(|case| case.handles).sum();
        for (idx, case) in cases.iter().enumerate() {
            assert!(
                matches_want(&host["results"][idx], &case.want),
                "O{level}: {:?}",
                host["results"][idx]
            );
        }
        assert_eq!(host["opened"], leaked);
        assert_eq!(host["closed"], 0);
        assert_eq!(
            host["open"], leaked,
            "the leak is visible to the host counter"
        );
        // The mock reports the same number through `open_after`: operation 5
        // is `leak`, and 0 to 4 are the operations that close their handles.
        let mut calls = Vec::new();
        for case in &cases {
            let name = match case.args[0] {
                Text(name) => name,
                _ => unreachable!(),
            };
            calls.push(json!({"export": "open_after", "reply": true,
                "args": [5, name, "", 0]}));
        }
        for (which, name, n) in [
            (0u64, "digits", 3u64),
            (2, "hello.txt", 0),
            (3, "digits", 7),
            (4, "digits", 0),
        ] {
            calls.push(json!({"export": "open_after", "reply": true,
                "args": [which, name, "hello.txt", n]}));
        }
        let report = node_run(&mock_path, &[], calls);
        let results = report["results"].as_array().unwrap();
        for (idx, case) in cases.iter().enumerate() {
            assert_eq!(
                results[idx],
                json!({"kind": "reply", "ok": true, "value": case.handles}),
                "mock, O{level}, {:?}",
                case.args
            );
        }
        for result in &results[cases.len()..] {
            assert_eq!(
                *result,
                json!({"kind": "reply", "ok": true, "value": 0}),
                "O{level}"
            );
        }
    }
}

#[test]
fn the_arena_is_released_after_every_call() {
    let temp = tempdir().unwrap();
    let (host_path, _) = build("file_ops", Flavour::Host, "2", temp.path());
    let calls: Vec<Value> = (0..300)
        .map(|i| {
            let name = ["hello.txt", "digits", "grüß", "nope", ""][i % 5];
            json!({"export": "sizes_two", "reply": true, "args": [name, "digits"]})
        })
        .collect();
    let report = node_run(&host_path, &FILES, calls);
    let tops: Vec<_> = report["after"]
        .as_array()
        .unwrap()
        .iter()
        .map(|after| after["top"].as_u64().unwrap())
        .collect();
    assert!(
        tops.windows(2).all(|pair| pair[0] == pair[1]),
        "the arena top moved between calls: {:?}",
        &tops[..8]
    );
    assert_eq!(report["open"], 0);
}

#[test]
fn text_may_be_long_and_contain_any_byte() {
    let temp = tempdir().unwrap();
    let (host_path, _) = build("file_ops", Flavour::Host, "1", temp.path());
    let long = "x".repeat(60000);
    let bytes: Vec<u32> = (0..=255).collect();
    let report = node_run(
        &host_path,
        &FILES,
        vec![
            json!({"export": "text_len", "reply": true, "args": [long]}),
            json!({"export": "text_byte", "reply": true, "args": [{"bytes": bytes}, 255]}),
            json!({"export": "text_byte", "reply": true, "args": [{"bytes": bytes}, 128]}),
            json!({"export": "text_byte", "reply": true, "args": [{"bytes": bytes}, 256]}),
            json!({"export": "byte_at", "reply": true, "args": [{"bytes": [0, 1, 2]}, 0]}),
        ],
    );
    let results = report["results"].as_array().unwrap();
    assert_eq!(
        results[0],
        json!({"kind": "reply", "ok": true, "value": 60000})
    );
    assert_eq!(
        results[1],
        json!({"kind": "reply", "ok": true, "value": 255})
    );
    assert_eq!(
        results[2],
        json!({"kind": "reply", "ok": true, "value": 128})
    );
    assert_eq!(results[3]["error"], "OutOfRange");
    assert_eq!(results[4]["error"], "NotFound");
}

// ---------------------------------------------------------------------------
// A module that breaks the contract.
// ---------------------------------------------------------------------------

fn call(export: &str, args: Vec<Value>, reply: bool) -> Value {
    json!({"export": export, "args": args, "reply": reply})
}

#[test]
fn the_host_refuses_a_module_that_breaks_the_contract() {
    let temp = tempdir().unwrap();
    for level in LEVELS {
        let (path, _) = build("forge", Flavour::Host, level, temp.path());
        let calls = vec![
            // 0: an honest open, to learn that the first handle is 1.
            call("open_text", vec![json!("digits")], false),
            // 1, 2: it works.
            call("size_raw", vec![json!(1)], false),
            call("read_byte_raw", vec![json!(1), json!(3)], false),
            // 3: close it; 4 to 6: a closed handle.
            call("close_raw", vec![json!(1)], false),
            call("close_raw", vec![json!(1)], false),
            call("size_raw", vec![json!(1)], false),
            call("read_byte_raw", vec![json!(1), json!(0)], false),
            // 7 to 10: handles that were never issued.
            call("size_raw", vec![json!(77)], false),
            call("close_raw", vec![json!(0)], false),
            call("read_byte_raw", vec![json!(2), json!(0)], false),
            call("close_raw", vec![json!(4294967295u64)], false),
            // 11 to 13: text outside the memory, or too long.
            call("open_raw", vec![json!(4294967040u64), json!(100)], false),
            call("open_raw", vec![json!(4294967295u64), json!(2)], false),
            call("open_raw", vec![json!(16), json!(70000)], false),
            // 14 to 17: replies that a module makes up.
            call("made_up_reply", vec![json!(9), json!(0)], true),
            call("made_up_reply", vec![json!(2), json!(5)], true),
            call("made_up_reply", vec![json!(1), json!(0)], true),
            call("made_up_reply", vec![json!(0), json!(0)], true),
            // 18 to 21: the program side of the closed set of errors.
            call("error_of", vec![json!(1)], true),
            call("error_of", vec![json!(3)], true),
            call("error_of", vec![json!(9)], true),
            call("error_of", vec![json!(0)], true),
        ];
        let report = node_run(&path, &FILES, calls);
        let results = report["results"].as_array().unwrap();
        let contract = |idx: usize, needle: &str| {
            assert_eq!(
                results[idx]["kind"], "contract",
                "O{level} call {idx}: {}",
                results[idx]
            );
            let message = results[idx]["message"].as_str().unwrap();
            assert!(message.contains(needle), "O{level} call {idx}: {message}");
        };
        assert_eq!(results[0], json!({"kind": "value", "value": "4294967296"}));
        assert_eq!(results[1], json!({"kind": "value", "value": "42949672960"}));
        assert_eq!(
            results[2],
            json!({"kind": "value", "value": "219043332096"})
        );
        assert_eq!(results[3], json!({"kind": "none"}));
        for idx in 4..=6 {
            contract(idx, "handle 1 is closed");
        }
        contract(7, "handle 77 was never issued by this host (forged handle)");
        contract(8, "handle 0 was never issued");
        contract(9, "handle 2 was never issued");
        contract(10, "handle 4294967295 was never issued");
        contract(11, "lies outside the memory");
        contract(12, "lies outside the memory");
        contract(13, "above the limit of 65536 bytes");
        contract(14, "unknown error code 9");
        contract(15, "carries no payload");
        assert_eq!(
            results[16],
            json!({"kind": "reply", "ok": false, "code": 1, "error": "NotFound"})
        );
        assert_eq!(
            results[17],
            json!({"kind": "reply", "ok": true, "value": 0})
        );
        assert_eq!(
            results[18],
            json!({"kind": "reply", "ok": false, "code": 1, "error": "NotFound"})
        );
        assert_eq!(
            results[19],
            json!({"kind": "reply", "ok": false, "code": 3, "error": "BadName"})
        );
        // Fe refuses a number outside the contract: the program traps.
        assert_eq!(results[20]["kind"], "trap", "O{level}: {}", results[20]);
        assert_eq!(results[21]["kind"], "trap", "O{level}: {}", results[21]);
        // The refused calls changed nothing: one handle was opened and closed.
        assert_eq!(report["opened"], 1);
        assert_eq!(report["closed"], 1);
        assert_eq!(report["open"], 0);
    }
}

#[test]
fn the_node_checks_of_the_host_pass() {
    let tests = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../examples/wasm/host.test.mjs");
    // The TAP reporter prints the `# pass` and `# fail` summary lines. It is
    // the default only when the output is not a terminal, and only before
    // Node 23.
    let output = Command::new("node")
        .arg("--test-reporter=tap")
        .arg("--test")
        .arg(&tests)
        .output()
        .expect("node must be installed to run these tests");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "node --test failed:\n{stdout}\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let summary = |key: &str| -> u32 {
        stdout
            .lines()
            .find_map(|line| line.strip_prefix(&format!("# {key} ")))
            .and_then(|count| count.trim().parse().ok())
            .unwrap_or_else(|| panic!("no `# {key}` line in:\n{stdout}"))
    };
    assert_eq!(summary("fail"), 0);
    assert!(summary("pass") >= 10, "{stdout}");
}

// ---------------------------------------------------------------------------
// The module.
// ---------------------------------------------------------------------------

fn type_name(ty: ValType) -> &'static str {
    match ty {
        ValType::I32 => "i32",
        ValType::I64 => "i64",
        other => panic!("unexpected type {other:?}"),
    }
}

fn signature(ty: &FuncType) -> String {
    let join = |types: Vec<ValType>| {
        types
            .into_iter()
            .map(type_name)
            .collect::<Vec<_>>()
            .join(",")
    };
    format!(
        "{}->{}",
        join(ty.params().collect()),
        join(ty.results().collect())
    )
}

fn exports_of(module: &Module) -> Vec<(String, String)> {
    let mut found: Vec<_> = module
        .exports()
        .filter_map(|export| {
            export
                .ty()
                .func()
                .map(|func| (export.name().to_string(), signature(func)))
        })
        .collect();
    found.sort();
    found
}

fn imports_of(module: &Module) -> Vec<(String, String, String)> {
    let mut found: Vec<_> = module
        .imports()
        .map(|import| {
            (
                import.module().to_string(),
                import.name().to_string(),
                signature(import.ty().func().expect("function imports only")),
            )
        })
        .collect();
    found.sort();
    found
}

fn pairs(list: &[(&str, &str)]) -> Vec<(String, String)> {
    list.iter()
        .map(|(a, b)| (a.to_string(), b.to_string()))
        .collect()
}

#[test]
fn the_signatures_of_the_module_are_the_documented_ones() {
    let temp = tempdir().unwrap();
    let (_, host) = build("file_ops", Flavour::Host, "2", temp.path());
    let (_, mock) = build("file_ops", Flavour::Mock, "2", temp.path());
    let host = Module::new(engine(), &host).unwrap();
    let mock = Module::new(engine(), &mock).unwrap();
    // Four imports, text as an address and a length, the reply as one i64.
    assert_eq!(
        imports_of(&host),
        [
            ("env", "sys_wasm_files_close", "i32->"),
            ("env", "sys_wasm_files_open", "i32,i32->i64"),
            ("env", "sys_wasm_files_read_byte", "i32,i32->i64"),
            ("env", "sys_wasm_files_size", "i32->i64"),
        ]
        .map(|(m, n, s)| (m.to_string(), n.to_string(), s.to_string()))
    );
    assert_eq!(imports_of(&mock), []);
    let mut want = pairs(&[
        ("alloc", "i32->i32"),
        ("arena_mark", "->i32"),
        ("arena_release", "i32->"),
        ("byte_at", "i32,i32,i32->i64"),
        ("checksum", "i32,i32->i64"),
        ("leak", "i32,i32->i64"),
        ("mixed", "i32,i32,i32,i64->i64"),
        ("open_n", "i32,i32,i32->i64"),
        ("plain", "i32,i32->i32"),
        ("size_of", "i32,i32->i64"),
        ("sizes_two", "i32,i32,i32,i32->i64"),
        ("text_byte", "i32,i32,i32->i64"),
        ("text_len", "i32,i32->i64"),
        ("touch", "i32,i32->"),
    ]);
    assert_eq!(exports_of(&host), want);
    want.push((
        "open_after".to_string(),
        "i32,i32,i32,i32,i32,i32->i64".to_string(),
    ));
    want.sort();
    assert_eq!(exports_of(&mock), want);
}

#[test]
fn a_module_without_text_has_no_allocator() {
    let temp = tempdir().unwrap();
    let path = temp.path().join("scalar.fe");
    fs::write(&path, "pub fn add(a: u32, b: u32) -> u32 { a + b }\n").unwrap();
    let bytes = build_wasm(&path, "2", &temp.path().join("out"));
    let module = Module::new(engine(), &bytes).unwrap();
    assert_eq!(exports_of(&module), pairs(&[("add", "i32,i32->i32")]));
}

#[test]
fn modules_validate_and_two_builds_are_identical() {
    let temp = tempdir().unwrap();
    let mut modules = 0;
    for (file, flavours) in [
        ("file_ops", vec![Flavour::Host, Flavour::Mock]),
        ("forge", vec![Flavour::Host]),
    ] {
        for flavour in flavours {
            for level in LEVELS {
                let (_, first) = build(file, flavour, level, &temp.path().join("a"));
                let (_, second) = build(file, flavour, level, &temp.path().join("b"));
                assert_eq!(first, second, "{file} {flavour:?} O{level}");
                // wasmtime validates the module when it loads it.
                Module::new(engine(), &first).unwrap();
                modules += 1;
            }
        }
    }
    println!("{modules} modules validated, each built twice with identical bytes");
}

// ---------------------------------------------------------------------------
// The language side: what safe code cannot do, and the named errors.
// ---------------------------------------------------------------------------

fn compile(source: &str) -> Result<(), String> {
    let temp = tempdir().unwrap();
    let path = temp.path().join("case.fe");
    fs::write(&path, source).unwrap();
    let out = temp.path().join("out");
    let output = wasm_support::fe(&[
        "build",
        "--backend",
        "wasm",
        "--out-dir",
        out.to_str().unwrap(),
        path.to_str().unwrap(),
    ]);
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    if output.status.success() {
        assert!(out.join("case.wasm").exists());
        return Result::Ok(());
    }
    assert!(
        !out.join("case.wasm").exists(),
        "nothing is written when the build fails"
    );
    assert!(!stderr.contains("panicked"), "{stderr}");
    Result::Err(stderr)
}

fn rejected(source: &str, needles: &[&str]) {
    let error = compile(source).expect_err("the build should fail");
    for needle in needles {
        assert!(error.contains(needle), "missing `{needle}` in:\n{error}");
    }
}

const RULE: &str = "exports take `bool` and integers of at most 64 bits, `std::wasm::HostText` parameters and a `std::wasm::Reply` result";

#[test]
fn other_shapes_are_named_errors() {
    rejected(
        "use std::abi::DynString\npub fn f(a: DynString) -> u32 { 1 }\n",
        &[
            "cannot export `f` to WebAssembly",
            "parameter `a: DynString`",
            RULE,
        ],
    );
    rejected(
        "pub fn f(a: u32) -> Option<u32> { Option::None }\n",
        &["cannot export `f`", "the result type `Option<u32>`", RULE],
    );
    rejected(
        "pub fn f(a: (u32, u32)) -> u32 { 1 }\n",
        &["cannot export `f`", "parameter `a: (u32, u32)`", RULE],
    );
    rejected(
        "use std::wasm::HostText\npub fn f(a: mut HostText) -> u32 { 1 }\n",
        &[
            "parameter `a: HostText`",
            "is a `HostText` that is not passed by value",
            RULE,
        ],
    );
    rejected(
        "use std::wasm::HostText\npub fn f(a: u32) -> HostText { unsafe { HostText::from_raw_parts(addr: a, len: 0) } }\n",
        &[
            "the result type `HostText`",
            "is a `HostText`, which can only be a parameter of an export",
            RULE,
        ],
    );
    rejected(
        "use std::wasm::Reply\npub fn f(a: Reply) -> u32 { 1 }\n",
        &[
            "parameter `a: Reply`",
            "is a `Reply`, which can only be the result of an export",
            RULE,
        ],
    );
    rejected(
        "use std::wasm::HostText\npub fn f(a: [HostText; 2]) -> u32 { 1 }\n",
        &["parameter `a: [HostText; 2]`", "is not a scalar", RULE],
    );
    rejected(
        "use std::wasm::HostText\npub fn f(a: *HostText) -> u32 { 1 }\n",
        &["is a pointer into Fe memory", RULE],
    );
}

#[test]
fn a_text_or_a_reply_cannot_cross_an_import() {
    rejected(
        "use std::wasm::HostText\nextern { fn h(_ t: HostText) -> u32 }\npub fn run(t: HostText) -> u32 { unsafe { h(t) } }\n",
        &[
            "cannot import `h` from the WebAssembly host",
            "parameter `t: HostText`",
            "which only an export can take",
            "imports take only `bool` and integers of at most 64 bits",
        ],
    );
    rejected(
        "use std::wasm::Reply\nextern { fn h(_ a: u32) -> Reply }\npub fn run(a: u32) -> u32 { unsafe { h(a) }.tag }\n",
        &[
            "cannot import `h` from the WebAssembly host",
            "the result type `Reply`",
            "imports take only `bool` and integers of at most 64 bits",
        ],
    );
}

#[test]
fn the_names_of_the_allocator_are_reserved_when_the_module_takes_text() {
    for name in ["alloc", "arena_mark", "arena_release"] {
        let with_text = format!(
            "use std::wasm::HostText\npub fn {name}(a: u32) -> u32 {{ a }}\npub fn len(t: HostText) -> u32 {{ t.len() }}\n"
        );
        rejected(
            &with_text,
            &[
                &format!("cannot export `{name}` to WebAssembly"),
                "reserved for the allocator",
            ],
        );
        // With no text the name is an ordinary export.
        compile(&format!("pub fn {name}(a: u32) -> u32 {{ a }}\n")).unwrap();
    }
}

#[test]
fn safe_code_cannot_make_a_handle_or_a_text_and_cannot_use_a_handle_twice() {
    rejected(
        "use std::wasm::files::Handle\npub fn f(a: u32) -> u32 {\n    let h = Handle { id: a }\n    a\n}\n",
        &["initializing an `unsafe` field requires an `unsafe` block"],
    );
    rejected(
        "use std::wasm::HostText\npub fn f(a: u32) -> u32 {\n    let t = HostText { addr: a, len: 1 }\n    a\n}\n",
        &["initializing an `unsafe` field requires an `unsafe` block"],
    );
    rejected(
        "use std::wasm::files::{Files, Handle, close}\nfn twice(_ h: own Handle) uses (files: mut Files) {\n    close(h)\n    close(h)\n}\npub fn f(a: u32) -> u32 { a }\n",
        &["cannot use a value after it was moved"],
    );
    rejected(
        "use std::wasm::files::{Files, Handle, close, size}\nfn late(_ h: own Handle) -> u32 uses (files: mut Files) {\n    close(h)\n    match size(h) {\n        Result::Ok(n) => n\n        Result::Err(_) => 0\n    }\n}\npub fn f(a: u32) -> u32 { a }\n",
        &["cannot use a value after it was moved"],
    );
    // A handle that is dropped is accepted: the compiler does not require the
    // close, so the host counts it (see the test of the leaked handle).
    compile(
        "use std::wasm::files::{Files, Handle, open}\nuse std::abi::DynString\nfn drop_it(_ name: DynString) -> u32 uses (files: mut Files) {\n    match open(name) {\n        Result::Ok(h) => 1\n        Result::Err(_) => 0\n    }\n}\npub fn f(a: u32) -> u32 { a }\n",
    )
    .unwrap();
}

#[test]
fn the_example_counts_the_lines_of_a_file_on_disk() {
    let examples = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../examples/wasm");
    let temp = tempdir().unwrap();
    let bytes = build_wasm(&examples.join("lines.fe"), "2", &temp.path().join("out"));
    Module::new(engine(), &bytes).unwrap();
    let module = temp.path().join("out/lines.wasm");
    for (contents, want) in [
        ("a\nb\nc\n", "3"),
        ("a\nb\nc", "3"),
        ("one line", "1"),
        ("\n\n", "2"),
        ("", "0"),
    ] {
        let file = temp.path().join("input.txt");
        fs::write(&file, contents).unwrap();
        let output = Command::new("node")
            .arg(examples.join("lines.mjs"))
            .arg(&module)
            .arg(&file)
            .output()
            .expect("node must be installed to run these tests");
        assert!(
            output.status.success(),
            "{contents:?}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert_eq!(
            String::from_utf8_lossy(&output.stdout).trim(),
            want,
            "{contents:?}"
        );
    }
}
