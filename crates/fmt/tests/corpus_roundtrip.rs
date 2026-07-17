//! Corpus round-trip property test.
//!
//! Walks every `.fe` file in the repository and asserts, for each file that
//! parses cleanly:
//!
//! - `fmt(file)` succeeds,
//! - `parse(fmt(file))` succeeds — the formatter must never emit output the
//!   parser rejects (e.g. a line-start `-` after wrapping),
//! - `fmt(fmt(file)) == fmt(file)` — formatting is idempotent.
//!
//! There is no span-erased AST-equality utility in the codebase, so semantic
//! preservation is covered only indirectly (reparse success + idempotence).

use std::fs;
use std::path::{Path, PathBuf};

use fe_fmt::{Config, FormatError, format_str};
use parser::{RecoveryMode, parse_source_file};

/// Directories that contain no source corpus or deliberately broken files.
const SKIP_DIRS: &[&str] = &["target", ".git", "node_modules"];

fn collect_fe_files(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("failed to read corpus directory") {
        let entry = entry.expect("failed to read corpus directory entry");
        let path = entry.path();
        if path.is_dir() {
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if !SKIP_DIRS.contains(&name.as_ref()) && !name.starts_with('.') {
                collect_fe_files(&path, out);
            }
        } else if path.extension().is_some_and(|ext| ext == "fe") {
            out.push(path);
        }
    }
}

fn parses_cleanly(source: &str) -> bool {
    let (_, errors) = parse_source_file(source, RecoveryMode::new(true));
    errors.is_empty()
}

#[test]
fn corpus_roundtrip() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mut files = Vec::new();
    collect_fe_files(&repo_root, &mut files);
    files.sort();

    // Guard against walking the wrong directory.
    assert!(
        files.len() > 100,
        "expected to find a large .fe corpus, found only {} files",
        files.len()
    );

    let config = Config::default();
    let mut checked = 0usize;
    let mut skipped = 0usize;
    let mut failures = Vec::new();

    for path in &files {
        let display = path.strip_prefix(&repo_root).unwrap_or(path).display();
        let source = fs::read_to_string(path).expect("failed to read corpus file");

        let formatted = match format_str(&source, &config) {
            Ok(formatted) => formatted,
            // Files that do not parse (e.g. deliberately broken uitest
            // fixtures) are outside the property; the formatter refuses them.
            Err(FormatError::ParseErrors(_)) => {
                skipped += 1;
                continue;
            }
            Err(err) => {
                failures.push(format!("{display}: format failed: {err:?}"));
                continue;
            }
        };
        checked += 1;

        if !parses_cleanly(&formatted) {
            failures.push(format!(
                "{display}: formatted output no longer parses:\n{formatted}"
            ));
            continue;
        }

        match format_str(&formatted, &config) {
            Ok(reformatted) => {
                if reformatted != formatted {
                    failures.push(format!("{display}: formatting is not idempotent"));
                }
            }
            Err(err) => {
                failures.push(format!("{display}: reformatting failed: {err:?}"));
            }
        }
    }

    println!("corpus round-trip: {checked} files checked, {skipped} unparsable files skipped");
    assert!(
        failures.is_empty(),
        "corpus round-trip failures ({}):\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
