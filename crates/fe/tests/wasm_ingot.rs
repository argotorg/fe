#![cfg(feature = "wasm")]

//! `fe build --backend wasm` for ingots and workspaces.
//!
//! An ingot is built into one module named after it. Its exports are the
//! `pub fn` items of its root module, `src/lib.fe`, which may call the other
//! modules of the ingot and its dependencies. A workspace builds one module for
//! each selected member that has exports. The modules run in Node.js with the
//! host of `examples/wasm/host.mjs`, which checks each against its manifest.
//! Node must be installed: the tests fail without it.

mod wasm_support;

use std::{fs, path::Path};

use serde_json::{Value, json};
use tempfile::tempdir;
use wasm_support::{Built, assert_matches_the_engine, expect_failure, fe, node_driver};

/// The root module calls a second module, which has a struct and a constant of
/// its own.
const CALC: [(&str, &str); 2] = [
    (
        "lib",
        "use ingot::table::{Weights, prime, weigh}\n\
         \n\
         pub fn weighted(a: u32, b: u32) -> u32 {\n\
         \x20   weigh(Weights { first: 3, second: 5 }, a, b)\n\
         }\n\
         \n\
         pub fn nth_prime(index: u32) -> u64 {\n\
         \x20   prime(index)\n\
         }\n",
    ),
    (
        "table",
        "pub struct Weights {\n\
         \x20   pub first: u32,\n\
         \x20   pub second: u32,\n\
         }\n\
         \n\
         const PRIMES: [u64; 5] = [2, 3, 5, 7, 11]\n\
         \n\
         pub fn weigh(_ weights: Weights, _ a: u32, _ b: u32) -> u32 {\n\
         \x20   weights.first * a + weights.second * b\n\
         }\n\
         \n\
         pub fn prime(_ index: u32) -> u64 {\n\
         \x20   PRIMES[index as usize]\n\
         }\n",
    ),
];

/// The root module calls the ingot `util`.
const APP: [(&str, &str); 1] = [(
    "lib",
    "use util::{double, sum}\n\
     \n\
     pub fn total(a: u64, b: u64) -> u64 {\n\
     \x20   sum([a, b, double(a), double(b)])\n\
     }\n",
)];

const APP_DEPENDENCIES: &str = "\n[dependencies]\nutil = { path = \"../util\" }\n";

/// A library for other ingots: `sum` takes an array, which no export can.
const UTIL: [(&str, &str); 1] = [(
    "lib",
    "pub fn double(_ x: u64) -> u64 {\n\
     \x20   x * 2\n\
     }\n\
     \n\
     pub fn sum(_ xs: [u64; 4]) -> u64 {\n\
     \x20   xs[0] + xs[1] + xs[2] + xs[3]\n\
     }\n",
)];

/// Nothing in the root module is an export.
const NO_EXPORTS: [(&str, &str); 1] = [(
    "lib",
    "fn private_only() -> u32 { 1 }\npub fn generic<T>(x: own T) -> T { x }\n",
)];

fn write_ingot(dir: &Path, name: &str, config: &str, modules: &[(&str, &str)]) {
    fs::create_dir_all(dir.join("src")).unwrap();
    fs::write(
        dir.join("fe.toml"),
        format!("[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n{config}"),
    )
    .unwrap();
    for (module, source) in modules {
        fs::write(dir.join(format!("src/{module}.fe")), source).unwrap();
    }
}

fn write_workspace(dir: &Path, members: &[&str]) {
    fs::create_dir_all(dir).unwrap();
    let members = members
        .iter()
        .map(|member| format!("\"{member}\""))
        .collect::<Vec<_>>()
        .join(", ");
    fs::write(
        dir.join("fe.toml"),
        format!("[workspace]\nname = \"root\"\nversion = \"0.1.0\"\nmembers = [{members}]\n"),
    )
    .unwrap();
}

fn build(args: &[&str]) {
    let output = fe(&[&["build", "--backend", "wasm"], args].concat());
    assert!(
        output.status.success(),
        "the build failed:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn files(dir: &Path) -> Vec<String> {
    let mut files: Vec<_> = fs::read_dir(dir)
        .unwrap()
        .map(|entry| entry.unwrap().file_name().into_string().unwrap())
        .collect();
    files.sort();
    files
}

fn export_names(built: &Built) -> Vec<String> {
    built.manifest()["exports"]
        .as_array()
        .unwrap()
        .iter()
        .map(|export| export["name"].as_str().unwrap().to_string())
        .collect()
}

/// The values that the Node host returns for `calls`, after it checked the
/// module against its manifest.
fn run(built: &Built, calls: Value) -> Value {
    assert_matches_the_engine(built);
    let answer = node_driver(&json!({
        "wasm": built.wasm(),
        "manifest": built.manifest(),
        "calls": calls,
    }));
    assert!(answer.get("error").is_none(), "{answer}");
    answer["results"].clone()
}

#[test]
fn an_ingot_is_one_module_of_its_root_exports() {
    let temp = tempdir().unwrap();
    let root = temp.path().join("calc");
    write_ingot(&root, "calc", "", &CALC);
    build(&[root.to_str().unwrap()]);

    let built = Built {
        dir: root.join("out"),
        stem: "calc".to_string(),
    };
    assert_eq!(files(&built.dir), ["calc.manifest.json", "calc.wasm"]);
    // The functions of `table` are not exports.
    assert_eq!(export_names(&built), ["memory", "nth_prime", "weighted"]);
    let results = run(
        &built,
        json!([
            { "export": "weighted", "args": [2, 4] },
            { "export": "nth_prime", "args": [4] },
        ]),
    );
    assert_eq!(results, json!([{ "value": "26" }, { "value": "11" }]));
}

#[test]
fn an_export_calls_into_a_path_dependency() {
    let temp = tempdir().unwrap();
    let app = temp.path().join("app");
    write_ingot(&app, "app", APP_DEPENDENCIES, &APP);
    write_ingot(&temp.path().join("util"), "util", "", &UTIL);
    let out = temp.path().join("out");
    for level in ["0", "2"] {
        let dir = out.join(level);
        build(&[
            "-O",
            level,
            "--emit",
            "wasm,ir",
            "--out-dir",
            dir.to_str().unwrap(),
            app.to_str().unwrap(),
        ]);
        assert_eq!(
            files(&dir),
            ["app.manifest.json", "app.wasm", "app.wasm.sona"]
        );
        let built = Built {
            dir,
            stem: "app".to_string(),
        };
        assert_eq!(built.manifest()["optimization"], level);
        // The functions of `util` are not exports, so `sum` may take an array.
        assert_eq!(export_names(&built), ["memory", "total"]);
        let results = run(
            &built,
            json!([{ "export": "total", "args": [{ "u64": "4294967296" }, { "u64": "1" }] }]),
        );
        assert_eq!(results, json!([{ "value": "12884901891" }]));
    }
}

#[test]
fn a_workspace_builds_the_member_that_ingot_names() {
    let temp = tempdir().unwrap();
    let root = temp.path();
    write_workspace(root, &["app", "util", "quiet"]);
    write_ingot(&root.join("app"), "app", APP_DEPENDENCIES, &APP);
    write_ingot(&root.join("util"), "util", "", &UTIL);
    write_ingot(&root.join("quiet"), "quiet", "", &NO_EXPORTS);
    let path = root.to_str().unwrap();

    build(&["--ingot", "app", path]);
    let built = Built {
        dir: root.join("out"),
        stem: "app".to_string(),
    };
    assert_eq!(files(&built.dir), ["app.manifest.json", "app.wasm"]);
    let results = run(
        &built,
        json!([{ "export": "total", "args": [{ "u64": "5" }, { "u64": "7" }] }]),
    );
    assert_eq!(results, json!([{ "value": "36" }]));

    // A member that is named must have exports.
    expect_failure(
        &["build", "--backend", "wasm", "--ingot", "quiet", path],
        &["needs at least one exported function"],
    );
    expect_failure(
        &["build", "--backend", "wasm", "--ingot", "absent", path],
        &["No workspace member named \"absent\""],
    );
    // Every root `pub fn` of a member is an export, so `util` is not a module.
    expect_failure(
        &["build", "--backend", "wasm", path],
        &["cannot export `sum` to WebAssembly"],
    );
}

#[test]
fn a_workspace_builds_its_members_with_exports() {
    let temp = tempdir().unwrap();
    let root = temp.path();
    write_workspace(root, &["calc", "quiet"]);
    write_ingot(&root.join("calc"), "calc", "", &CALC);
    write_ingot(&root.join("quiet"), "quiet", "", &NO_EXPORTS);
    build(&[root.to_str().unwrap()]);

    let built = Built {
        dir: root.join("out"),
        stem: "calc".to_string(),
    };
    assert_eq!(files(&built.dir), ["calc.manifest.json", "calc.wasm"]);
    let results = run(&built, json!([{ "export": "weighted", "args": [1, 1] }]));
    assert_eq!(results, json!([{ "value": "8" }]));
}

#[test]
fn a_workspace_without_exports_is_refused() {
    let temp = tempdir().unwrap();
    write_workspace(temp.path(), &["quiet"]);
    write_ingot(&temp.path().join("quiet"), "quiet", "", &NO_EXPORTS);
    expect_failure(
        &["build", "--backend", "wasm", temp.path().to_str().unwrap()],
        &["No WebAssembly exports found to build"],
    );
    assert!(!temp.path().join("out").exists());
}

#[test]
fn an_ingot_without_exports_is_refused_as_a_file_is() {
    let temp = tempdir().unwrap();
    let root = temp.path().join("quiet");
    write_ingot(&root, "quiet", "", &NO_EXPORTS);
    let file = temp.path().join("quiet.fe");
    fs::write(&file, NO_EXPORTS[0].1).unwrap();

    let of_ingot = expect_failure(&["build", "--backend", "wasm", root.to_str().unwrap()], &[]);
    let of_file = expect_failure(&["build", "--backend", "wasm", file.to_str().unwrap()], &[]);
    assert!(
        of_file.contains("needs at least one exported function"),
        "{of_file}"
    );
    assert_eq!(of_ingot, of_file);
    assert!(!root.join("out").exists());
}

#[test]
fn ingot_selects_a_member_of_a_workspace_only() {
    let temp = tempdir().unwrap();
    write_ingot(temp.path(), "calc", "", &CALC);
    expect_failure(
        &[
            "build",
            "--backend",
            "wasm",
            "--ingot",
            "calc",
            temp.path().to_str().unwrap(),
        ],
        &["`--ingot` requires an input path that resolves to a workspace root"],
    );
}
