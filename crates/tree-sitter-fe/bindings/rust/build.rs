#![allow(clippy::print_stdout, clippy::print_stderr)]

use std::path::{Path, PathBuf};

fn main() {
    // Keep generated grammar ABI aligned with the Rust runtime dependency
    // (`tree-sitter` 0.24.x).
    const TREE_SITTER_ABI_VERSION: &str = "14";

    let src_dir = Path::new("src");
    let grammar_path = Path::new("grammar.js");
    let parser_path = src_dir.join("parser.c");
    let scanner_path = src_dir.join("scanner.c");

    // parser.c and the other `tree-sitter generate` outputs are NOT tracked in
    // git (they're build artifacts that bloat diffs). Regenerate them from
    // grammar.js when missing or stale, so a plain `cargo build`/`cargo test`
    // works from a fresh checkout as long as a tree-sitter CLI is available.
    println!("cargo:rerun-if-changed={}", grammar_path.display());

    let needs_generate = match (parser_path.metadata(), grammar_path.metadata()) {
        // parser.c exists: regenerate only if grammar.js is newer.
        (Ok(parser_meta), Ok(grammar_meta)) => {
            matches!(
                (grammar_meta.modified(), parser_meta.modified()),
                (Ok(g), Ok(p)) if g > p
            )
        }
        // parser.c missing (e.g. fresh checkout): must generate.
        _ => true,
    };

    if needs_generate {
        if !grammar_path.exists() {
            panic!(
                "{} not found and {} is missing; cannot build the Fe grammar",
                grammar_path.display(),
                parser_path.display()
            );
        }
        generate_parser(TREE_SITTER_ABI_VERSION);
    }

    // If generation didn't produce parser.c, fail with an actionable message
    // rather than letting the C compiler choke on a missing file.
    if !parser_path.exists() {
        panic!(
            "{} was not generated. Install the tree-sitter CLI \
             (run `npm ci` in crates/tree-sitter-fe to get the pinned version, \
             or install `tree-sitter` on your PATH) and rebuild.",
            parser_path.display()
        );
    }

    let mut c_config = cc::Build::new();
    c_config.std("c11").include(src_dir);

    // Always optimize parser.c — the 96K-line generated state machine is
    // ~200x slower at -O0 vs -O2, making tests unusable in debug builds.
    c_config.opt_level(2);

    c_config.file(&parser_path);
    println!("cargo:rerun-if-changed={}", parser_path.display());

    if scanner_path.exists() {
        c_config.file(&scanner_path);
        println!("cargo:rerun-if-changed={}", scanner_path.display());
    }

    c_config.compile("tree-sitter-fe");
}

/// Run `tree-sitter generate`, preferring the version pinned in `node_modules`
/// (installed via `npm ci`, used by CI for reproducible output) and falling
/// back to a `tree-sitter` on `PATH` for local development.
fn generate_parser(abi: &str) {
    let program = pinned_cli().unwrap_or_else(|| PathBuf::from("tree-sitter"));

    let status = std::process::Command::new(&program)
        .arg("generate")
        .arg(format!("--abi={abi}"))
        .status();

    match status {
        Ok(s) if s.success() => {}
        Ok(s) => panic!("`{} generate` failed with {s}", program.display()),
        Err(e) => panic!(
            "failed to run `{} generate`: {e}. Install the tree-sitter CLI \
             (run `npm ci` in crates/tree-sitter-fe, or install `tree-sitter` on your PATH).",
            program.display()
        ),
    }
}

/// Locate the tree-sitter CLI pinned in `node_modules/.bin`, accounting for the
/// platform-specific shim names npm creates.
fn pinned_cli() -> Option<PathBuf> {
    let bin_dir = Path::new("node_modules/.bin");
    ["tree-sitter", "tree-sitter.cmd", "tree-sitter.exe"]
        .into_iter()
        .map(|name| bin_dir.join(name))
        .find(|path| path.exists())
}
