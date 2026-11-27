//! Build script for fe-web: rebuilds tree-sitter-fe.wasm when grammar inputs change.

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const HASH_PREFIX: &str = "fnv1a64-relpath-v1";
const FNV1A64_OFFSET_BASIS: u64 = 0xcbf29ce484222325;
const FNV1A64_PRIME: u64 = 0x100000001b3;

fn main() {
    // Read when the script runs: a compiled script can be reused for another
    // checkout of the crate, where a path fixed at compile time would name
    // the first one's files.
    let manifest_dir =
        PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").expect("cargo sets CARGO_MANIFEST_DIR"));
    let grammar_dir = manifest_dir.join("../tree-sitter-fe");
    let vendor_wasm = manifest_dir.join("vendor/tree-sitter-fe.wasm");
    let stamp_file = manifest_dir.join("vendor/tree-sitter-fe.wasm.inputs");
    let inputs = tree_sitter_inputs(&grammar_dir);

    for (_, input) in &inputs {
        println!("cargo:rerun-if-changed={}", input.display());
    }
    println!("cargo:rerun-if-changed={}", vendor_wasm.display());
    println!("cargo:rerun-if-changed={}", stamp_file.display());

    if !inputs.iter().all(|(_, input)| input.exists()) {
        if vendor_wasm.exists() {
            println!(
                "cargo:warning=tree-sitter grammar inputs are unavailable; using vendored {}",
                vendor_wasm.display()
            );
            return;
        }

        panic!(
            "tree-sitter grammar inputs are unavailable and {} does not exist",
            vendor_wasm.display()
        );
    }

    let current_hash = input_hash(&inputs);
    let expected_stamp = format!("{HASH_PREFIX}:{current_hash:016x}");
    if vendor_wasm.exists() && stamp_matches(&stamp_file, &expected_stamp) {
        return;
    }

    rebuild_tree_sitter_wasm(&grammar_dir, &vendor_wasm);
    fs::write(&stamp_file, format!("{expected_stamp}\n"))
        .unwrap_or_else(|err| panic!("failed to write {}: {err}", stamp_file.display()));
}

fn stamp_matches(stamp_file: &Path, expected_stamp: &str) -> bool {
    fs::read_to_string(stamp_file)
        .is_ok_and(|stamp| stamp.trim_end_matches(['\r', '\n']) == expected_stamp)
}

fn tree_sitter_inputs(grammar_dir: &Path) -> Vec<(&'static str, PathBuf)> {
    [
        "grammar.js",
        "src/scanner.c",
        "tree-sitter.json",
        "package.json",
        "package-lock.json",
    ]
    .into_iter()
    .map(|path| (path, grammar_dir.join(path)))
    .collect()
}

fn input_hash(inputs: &[(&str, PathBuf)]) -> u64 {
    let mut hash = FNV1A64_OFFSET_BASIS;
    for (relative_path, input) in inputs {
        hash_bytes(&mut hash, relative_path.as_bytes());
        hash_bytes(&mut hash, &[0]);
        let contents = fs::read(input)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", input.display()));
        hash_text_bytes(&mut hash, &contents);
        hash_bytes(&mut hash, &[0xff]);
    }
    hash
}

fn hash_text_bytes(hash: &mut u64, bytes: &[u8]) {
    let mut bytes = bytes.iter().copied().peekable();
    while let Some(byte) = bytes.next() {
        if byte == b'\r' && bytes.peek() == Some(&b'\n') {
            continue;
        }
        hash_byte(hash, byte);
    }
}

fn hash_bytes(hash: &mut u64, bytes: &[u8]) {
    for byte in bytes {
        hash_byte(hash, *byte);
    }
}

fn hash_byte(hash: &mut u64, byte: u8) {
    *hash ^= u64::from(byte);
    *hash = hash.wrapping_mul(FNV1A64_PRIME);
}

fn rebuild_tree_sitter_wasm(grammar_dir: &Path, vendor_wasm: &Path) {
    let tree_sitter = grammar_dir.join("node_modules/.bin/tree-sitter");
    if !tree_sitter.exists() {
        panic!(
            "{} is stale, but {} does not exist. Run `npm ci` in {} to install the pinned tree-sitter CLI.",
            vendor_wasm.display(),
            tree_sitter.display(),
            grammar_dir.display()
        );
    }

    let output_wasm = grammar_dir.join("tree-sitter-fe.wasm");
    let output = Command::new(&tree_sitter)
        .arg("build")
        .arg("--wasm")
        .current_dir(grammar_dir)
        .output()
        .unwrap_or_else(|err| panic!("failed to run {}: {err}", tree_sitter.display()));

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        panic!("{} build --wasm failed:\n{stderr}", tree_sitter.display());
    }

    fs::copy(&output_wasm, vendor_wasm).unwrap_or_else(|err| {
        panic!(
            "failed to copy {} to {}: {err}",
            output_wasm.display(),
            vendor_wasm.display()
        )
    });
}
