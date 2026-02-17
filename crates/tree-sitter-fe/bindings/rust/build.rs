fn main() {
    let src_dir = std::path::Path::new("src");
    let grammar_path = std::path::Path::new("grammar.js");
    let parser_path = src_dir.join("parser.c");
    let scanner_path = src_dir.join("scanner.c");

    // Re-generate parser.c from grammar.js if grammar.js is newer.
    // This lets contributors just edit grammar.js and run `cargo test`
    // without needing to manually run `tree-sitter generate`.
    println!("cargo:rerun-if-changed={}", grammar_path.display());
    if grammar_path.exists() {
        let needs_generate = parser_path
            .metadata()
            .and_then(|pm| {
                grammar_path
                    .metadata()
                    .and_then(|gm| Ok(gm.modified()? > pm.modified()?))
            })
            .unwrap_or(true);

        if needs_generate {
            let status = std::process::Command::new("tree-sitter")
                .arg("generate")
                .status();
            match status {
                Ok(s) if s.success() => {}
                Ok(s) => {
                    eprintln!(
                        "cargo:warning=tree-sitter generate failed with {}",
                        s
                    );
                }
                Err(e) => {
                    eprintln!(
                        "cargo:warning=tree-sitter generate skipped (not installed: {})",
                        e
                    );
                }
            }
        }
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
