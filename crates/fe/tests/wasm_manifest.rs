#![cfg(feature = "wasm")]

//! The build manifest of `fe build --backend wasm`, and the Node host that
//! checks a module against it.
//!
//! The manifest is compared with three independent sources: the module's own
//! import and export sections as `wasmtime` reads them, a SHA-256 computed here
//! from the file, and `fe --version`. The Node host (`examples/wasm/host.mjs`)
//! must run a module that matches its manifest, and must refuse, with a message
//! that names the cause, a module that does not match or that imports something
//! it does not provide. Node must be installed: the tests fail without it.

mod wasm_support;

use std::{fs, path::Path};

use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use tempfile::tempdir;
use wasm_support::{Built, assert_matches_the_engine, fe, node_driver, render_flavour};

fn fixture(directory: &str, name: &str) -> String {
    let path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join(format!("tests/fixtures/{directory}/{name}.fe"));
    fs::read_to_string(&path).unwrap_or_else(|err| panic!("{}: {err}", path.display()))
}

fn build_source(source: &str, stem: &str, level: &str, temp: &Path) -> Built {
    let dir = temp.join(format!("out-{stem}-{level}"));
    // The file stem names the outputs, so the source gets its own directory.
    let src_dir = temp.join(format!("src-{stem}-{level}"));
    fs::create_dir_all(&src_dir).unwrap();
    let file = src_dir.join(format!("{stem}.fe"));
    fs::write(&file, source).unwrap();
    let output = fe(&[
        "build",
        "--backend",
        "wasm",
        "-O",
        level,
        "--emit",
        "wasm,ir",
        "--out-dir",
        dir.to_str().unwrap(),
        file.to_str().unwrap(),
    ]);
    assert!(
        output.status.success(),
        "build of {stem} failed:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    Built {
        dir,
        stem: stem.to_string(),
    }
}

#[test]
fn the_manifest_describes_every_kind_of_export_and_import() {
    let temp = tempdir().unwrap();
    let built = build_source(
        &fixture("wasm_manifest", "shapes"),
        "shapes",
        "1",
        temp.path(),
    );
    let manifest = built.manifest();
    let bytes = fs::read(built.wasm()).unwrap();
    let version = String::from_utf8(fe(&["--version"]).stdout).unwrap();

    let expected = json!({
        "manifest_version": 1,
        "module": {
            "file": "shapes.wasm",
            "sha256": hex::encode(Sha256::digest(&bytes)),
            "size": bytes.len(),
            "initial_memory_pages": manifest["module"]["initial_memory_pages"],
        },
        "contract_version": 2,
        "optimization": "1",
        "compiler": version.trim(),
        "imports": [
            { "module": "env", "name": "sys_wasm_files_close", "kind": "function",
              "type": { "params": ["i32"], "results": [] } },
            { "module": "env", "name": "sys_wasm_files_open", "kind": "function",
              "type": { "params": ["i32", "i32"], "results": ["i64"] } },
            { "module": "env", "name": "sys_wasm_log_i32", "kind": "function",
              "type": { "params": ["i32"], "results": [] } },
        ],
        "exports": [
            { "name": "add", "kind": "function",
              "params": [
                { "name": "a", "type": "u32", "wasm": ["i32"] },
                { "name": "b", "type": "u64", "wasm": ["i64"] },
              ],
              "result": { "type": "u64", "wasm": ["i64"] } },
            { "name": "alloc", "kind": "function", "role": "allocator",
              "params": [{ "name": "size", "type": "u32", "wasm": ["i32"] }],
              "result": { "type": "u32", "wasm": ["i32"] } },
            { "name": "arena_mark", "kind": "function", "role": "allocator",
              "params": [], "result": { "type": "u32", "wasm": ["i32"] } },
            { "name": "arena_release", "kind": "function", "role": "allocator",
              "params": [{ "name": "mark", "type": "u32", "wasm": ["i32"] }], "result": null },
            { "name": "has_file", "kind": "function",
              "params": [{ "name": "name", "type": "HostText", "wasm": ["i32", "i32"] }],
              "result": { "type": "Reply", "wasm": ["i64"] } },
            { "name": "is_even", "kind": "function",
              "params": [{ "name": "x", "type": "i8", "wasm": ["i32"] }],
              "result": { "type": "bool", "wasm": ["i32"] } },
            { "name": "memory", "kind": "memory" },
            { "name": "ping", "kind": "function",
              "params": [{ "name": "value", "type": "i32", "wasm": ["i32"] }], "result": null },
            { "name": "stack_top", "kind": "global", "role": "stack" },
        ],
    });
    assert_eq!(manifest, expected);
    assert!(
        manifest["module"]["initial_memory_pages"].as_u64().unwrap() >= 1,
        "the module has a memory"
    );
    assert_matches_the_engine(&built);
}

#[test]
fn the_manifest_agrees_with_the_module_for_every_kind_of_program() {
    let temp = tempdir().unwrap();
    let mut programs: Vec<(String, String)> = Vec::new();
    for name in [
        "struct_complex",
        "vec_arena",
        "trap_assert",
        "many_params",
        "narrow_ints",
    ] {
        programs.push((name.to_string(), fixture("wasm", name)));
    }
    for name in ["log_basic", "extern_narrow", "extern_many", "reentrant"] {
        let source = fixture("wasm_imports", name);
        programs.push((
            format!("{name}_host"),
            render_flavour(&source, "//host: ", "//mock: "),
        ));
        programs.push((
            format!("{name}_mock"),
            render_flavour(&source, "//mock: ", "//host: "),
        ));
    }
    let file_ops = fixture("wasm_files", "file_ops");
    programs.push((
        "file_ops_host".to_string(),
        render_flavour(&file_ops, "//host: ", "//mock: "),
    ));
    programs.push((
        "file_ops_mock".to_string(),
        render_flavour(&file_ops, "//mock: ", "//host: "),
    ));
    programs.push(("forge".to_string(), fixture("wasm_files", "forge")));
    for (name, source) in &programs {
        for level in ["0", "s"] {
            let built = build_source(source, name, level, temp.path());
            assert_eq!(built.manifest()["optimization"], level);
            assert_matches_the_engine(&built);
        }
    }
}

#[test]
fn the_manifest_is_written_only_with_the_module() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("only.fe");
    fs::write(&source, "pub fn one() -> u32 {\n    1\n}\n").unwrap();
    let run = |emit: &str, out: &str| {
        let out = temp.path().join(out);
        let output = fe(&[
            "build",
            "--backend",
            "wasm",
            "--emit",
            emit,
            "--out-dir",
            out.to_str().unwrap(),
            source.to_str().unwrap(),
        ]);
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let mut files: Vec<_> = fs::read_dir(&out)
            .unwrap()
            .map(|entry| entry.unwrap().file_name().into_string().unwrap())
            .collect();
        files.sort();
        files
    };
    assert_eq!(run("wasm", "wasm"), ["only.manifest.json", "only.wasm"]);
    assert_eq!(run("ir", "ir"), ["only.wasm.sona"]);
    assert_eq!(
        run("wasm,ir", "both"),
        ["only.manifest.json", "only.wasm", "only.wasm.sona"]
    );
    // The default is the module and its manifest.
    let out = temp.path().join("default");
    let output = fe(&[
        "build",
        "--backend",
        "wasm",
        "--out-dir",
        out.to_str().unwrap(),
        source.to_str().unwrap(),
    ]);
    assert!(output.status.success());
    assert!(out.join("only.manifest.json").exists() && out.join("only.wasm").exists());
    assert!(!out.join("only.wasm.sona").exists());
}

#[test]
fn a_failed_build_writes_no_manifest() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("bad.fe");
    fs::write(&source, "pub fn wide(_ x: u256) -> u256 {\n    x\n}\n").unwrap();
    let out = temp.path().join("out");
    let output = fe(&[
        "build",
        "--backend",
        "wasm",
        "--out-dir",
        out.to_str().unwrap(),
        source.to_str().unwrap(),
    ]);
    assert!(!output.status.success());
    assert!(!out.join("bad.manifest.json").exists());
    assert!(!out.join("bad.wasm").exists());
}

// ---------------------------------------------------------------------------
// The Node host.
// ---------------------------------------------------------------------------

#[test]
fn the_node_host_restores_the_stack_after_an_abort() {
    // `abort` is an import that throws, a hundred frames deep. Each frame
    // holds an array, so unless the host puts `stack_top` back the stack of
    // 1 MiB is gone after a few calls and the rest trap instead of aborting.
    let source = "use std::wasm::{Abort, abort}

fn dive(_ depth: u32) -> u64 uses (a: mut Abort) {
    let frame: [u64; 64] = [depth as u64; 64]
    if depth == 0 {
        abort(9)
    }
    dive(depth - 1) + frame[(depth % 64) as usize]
}

pub fn run(depth: u32) -> u64 uses (a: mut Abort) {
    dive(depth)
}
";
    let temp = tempdir().unwrap();
    for level in ["0", "1", "2"] {
        let built = build_source(source, "dive", level, temp.path());
        let calls = vec![json!({ "export": "run", "args": [100] }); 200];
        let answer = node_driver(&json!({
            "wasm": built.wasm(),
            "manifest": built.manifest(),
            "calls": calls,
        }));
        let abort = json!({
            "error": { "name": "GuestAbort", "message": "the module aborted with code 9" },
        });
        for (call, result) in answer["results"].as_array().unwrap().iter().enumerate() {
            assert_eq!(result, &abort, "-O {level}, call {call}");
        }
    }
}

fn refusal(request: &Value) -> String {
    let answer = node_driver(request);
    let error = answer
        .get("error")
        .unwrap_or_else(|| panic!("the host should refuse the module, but answered {answer}"));
    assert_eq!(error["name"], "ContractError", "{answer}");
    error["message"].as_str().unwrap().to_string()
}

#[test]
fn the_node_host_runs_a_module_that_matches_its_manifest() {
    let temp = tempdir().unwrap();
    let built = build_source(
        &fixture("wasm_manifest", "shapes"),
        "shapes",
        "1",
        temp.path(),
    );
    let answer = node_driver(&json!({
        "wasm": built.wasm(),
        "manifest": built.manifest(),
        "files": { "present.txt": "x" },
        "calls": [
            { "export": "add", "args": [40, { "u64": "4294967296" }] },
            { "export": "is_even", "args": [-4] },
            { "export": "ping", "args": [-7] },
            { "export": "has_file", "args": ["present.txt"], "reply": true },
            { "export": "has_file", "args": ["absent.txt"], "reply": true },
        ],
    }));
    assert_eq!(
        answer["results"],
        json!([
            { "value": "4294967336" },
            { "value": "1" },
            { "value": null },
            { "reply": { "ok": true, "value": 1 } },
            { "reply": { "ok": false, "code": 1, "error": "NotFound" } },
        ]),
        "{answer}"
    );
    assert_eq!(answer["logs"], json!([{ "type": "i32", "value": "-7" }]));
}

#[test]
fn the_node_host_supplies_the_streams() {
    let temp = tempdir().unwrap();
    for level in ["0", "2"] {
        let built = build_source(
            &fixture("wasm_imports", "streams"),
            "streams",
            level,
            temp.path(),
        );
        assert_matches_the_engine(&built);
        let stream = |name: &str| {
            json!({ "module": "env", "name": name, "kind": "function",
                "type": { "params": ["i32", "i32", "i32"], "results": ["i32"] } })
        };
        assert_eq!(
            built.manifest()["imports"],
            json!([
                stream("sys_wasm_stream_read"),
                stream("sys_wasm_stream_write")
            ]),
            "-O {level}"
        );
        let answer = node_driver(&json!({
            "wasm": built.wasm(),
            "manifest": built.manifest(),
            "stdin": "one\nzwölf\n",
            "calls": [
                { "export": "greet", "args": [3, { "u64": "18446744073709551615" }, -128] },
                { "export": "warn", "args": [-7] },
                { "export": "number_lines", "args": [] },
                // More than the host takes in one call.
                { "export": "digits", "args": [70000] },
            ],
        }));
        assert_eq!(
            answer["results"],
            json!([{ "value": "3" }, { "value": "-7" }, { "value": "2" }, { "value": "70000" }]),
            "-O {level}: {answer}"
        );
        let digits: String = "0123456789".repeat(7000);
        assert_eq!(
            answer["stdout"],
            format!(
                "hello 3 times\nbig 18446744073709551615, small -128, true\n1: one\n2: zwölf\n{digits}"
            ),
            "-O {level}"
        );
        assert_eq!(
            answer["stderr"], "warning -7\nno newline2 lines\n",
            "-O {level}"
        );
        // Each print is one write, and a long one is taken in two pieces.
        let writes: Vec<(&str, usize)> = answer["writes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|write| {
                (
                    write["stream"].as_str().unwrap(),
                    write["text"].as_str().unwrap().len(),
                )
            })
            .collect();
        assert_eq!(
            writes,
            [
                ("stdout", 14),
                ("stdout", 42),
                ("stdout", 1),
                ("stderr", 11),
                ("stderr", 10),
                ("stdout", 7),
                ("stdout", 10),
                ("stderr", 8),
                ("stdout", 65536),
                ("stdout", 4464),
            ],
            "-O {level}"
        );
    }
}

#[test]
fn the_node_host_names_the_import_it_cannot_supply() {
    let temp = tempdir().unwrap();
    let built = build_source(
        &fixture("wasm_manifest", "custom_import"),
        "custom_import",
        "1",
        temp.path(),
    );
    let manifest = built.manifest();
    assert_eq!(manifest["imports"][0]["name"], "custom_import");
    // With the manifest, and without it: the message names `env.custom_import`.
    let with_manifest = refusal(&json!({ "wasm": built.wasm(), "manifest": manifest }));
    assert!(
        with_manifest.contains("env.custom_import") && with_manifest.contains("does not provide"),
        "{with_manifest}"
    );
    let without = refusal(&json!({ "wasm": built.wasm(), "manifest": null }));
    assert!(
        without.contains("env.custom_import") && without.contains("does not provide"),
        "{without}"
    );
}

#[test]
fn the_node_host_refuses_a_module_that_differs_from_its_manifest() {
    let temp = tempdir().unwrap();
    let built = build_source(
        &fixture("wasm_manifest", "shapes"),
        "shapes",
        "1",
        temp.path(),
    );
    let manifest = built.manifest();

    // A changed byte: the hash no longer matches.
    let mut bytes = fs::read(built.wasm()).unwrap();
    let last = bytes.len() - 1;
    bytes[last] ^= 1;
    let tampered = temp.path().join("tampered.wasm");
    fs::write(&tampered, &bytes).unwrap();
    let message = refusal(&json!({ "wasm": tampered, "manifest": manifest }));
    assert!(
        message.contains("SHA-256")
            && message.contains(manifest["module"]["sha256"].as_str().unwrap()),
        "{message}"
    );

    // A manifest for another contract version, and for another format version.
    let mut other = manifest.clone();
    other["contract_version"] = json!(3);
    let message = refusal(&json!({ "wasm": built.wasm(), "manifest": other }));
    assert!(
        message.contains("contract version 3") && message.contains("implements version 2"),
        "{message}"
    );
    let mut other = manifest.clone();
    other["manifest_version"] = json!(9);
    let message = refusal(&json!({ "wasm": built.wasm(), "manifest": other }));
    assert!(message.contains("format version 9"), "{message}");

    // An import that the manifest does not list, and one that the module lacks.
    let mut other = manifest.clone();
    other["imports"].as_array_mut().unwrap().pop();
    let message = refusal(&json!({ "wasm": built.wasm(), "manifest": other }));
    assert!(
        message.contains("env.sys_wasm_log_i32") && message.contains("does not list"),
        "{message}"
    );
    let mut other = manifest.clone();
    other["imports"].as_array_mut().unwrap().push(json!({
        "module": "env", "name": "sys_wasm_now_ms", "kind": "function",
        "type": { "params": [], "results": ["i64"] }
    }));
    let message = refusal(&json!({ "wasm": built.wasm(), "manifest": other }));
    assert!(
        message.contains("env.sys_wasm_now_ms") && message.contains("does not have"),
        "{message}"
    );

    // An export that the manifest lists and the module lacks, and the reverse.
    let mut other = manifest.clone();
    other["exports"]
        .as_array_mut()
        .unwrap()
        .push(json!({ "name": "gone", "kind": "function" }));
    let message = refusal(&json!({ "wasm": built.wasm(), "manifest": other }));
    assert!(message.contains("`gone`"), "{message}");
    let mut other = manifest.clone();
    other["exports"]
        .as_array_mut()
        .unwrap()
        .retain(|e| e["name"] != "ping");
    let message = refusal(&json!({ "wasm": built.wasm(), "manifest": other }));
    assert!(
        message.contains("`ping`") && message.contains("does not list"),
        "{message}"
    );
}

#[test]
fn a_call_must_match_the_function_the_manifest_describes() {
    let temp = tempdir().unwrap();
    let built = build_source(
        &fixture("wasm_manifest", "shapes"),
        "shapes",
        "1",
        temp.path(),
    );
    let answer = node_driver(&json!({
        "wasm": built.wasm(),
        "manifest": built.manifest(),
        "calls": [
            { "export": "add", "args": [1] },
            { "export": "add", "args": ["text", 2] },
            { "export": "has_file", "args": [5], "reply": true },
            { "export": "nothing_like_it", "args": [] },
        ],
    }));
    let messages: Vec<_> = answer["results"]
        .as_array()
        .unwrap()
        .iter()
        .map(|r| {
            r["error"]["message"]
                .as_str()
                .unwrap_or("no error")
                .to_string()
        })
        .collect();
    assert!(
        messages[0].contains("`add` takes 2 argument(s)")
            && messages[0].contains("but 1 were given"),
        "{messages:?}"
    );
    assert!(
        messages[1].contains("argument `a` of `add` must be a number of type u32"),
        "{messages:?}"
    );
    assert!(
        messages[2].contains("argument `name` of `has_file` must be text"),
        "{messages:?}"
    );
    assert!(
        messages[3].contains("does not export a function `nothing_like_it`"),
        "{messages:?}"
    );
}
