//! Build script for fe-web: rebuilds tree-sitter-fe.wasm when grammar changes.

use std::path::Path;
use std::process::Command;

fn main() {
    let grammar_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../tree-sitter-fe");
    let grammar_js = grammar_dir.join("grammar.js");
    let scanner_c = grammar_dir.join("src/scanner.c");
    let vendor_wasm = Path::new(env!("CARGO_MANIFEST_DIR")).join("vendor/tree-sitter-fe.wasm");

    // Rerun if grammar sources change
    println!("cargo:rerun-if-changed={}", grammar_js.display());
    println!("cargo:rerun-if-changed={}", scanner_c.display());

    // Also rerun if the vendor WASM doesn't exist
    if !vendor_wasm.exists() {
        println!("cargo:warning=tree-sitter-fe.wasm not found, attempting build");
    }

    // Try to rebuild the WASM
    let output_wasm = grammar_dir.join("tree-sitter-fe.wasm");
    let result = Command::new("tree-sitter")
        .arg("build")
        .arg("--wasm")
        .current_dir(&grammar_dir)
        .output();

    match result {
        Ok(output) if output.status.success() => {
            if output_wasm.exists() {
                if let Err(e) = std::fs::copy(&output_wasm, &vendor_wasm) {
                    println!("cargo:warning=Failed to copy tree-sitter-fe.wasm to vendor: {e}");
                }
            }
        }
        Ok(output) => {
            let stderr = String::from_utf8_lossy(&output.stderr);
            println!(
                "cargo:warning=tree-sitter build --wasm failed (using vendored copy): {stderr}"
            );
        }
        Err(_) => {
            println!(
                "cargo:warning=tree-sitter CLI not found, using vendored tree-sitter-fe.wasm"
            );
        }
    }
}
