use std::fs;
use std::path::{Path, PathBuf};

#[test]
fn trace_and_debug_crates_do_not_import_legacy_common_facts() {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .expect("common crate should live under crates/common");
    let guarded_roots = [
        "crates/trace-facts",
        "crates/trace-query",
        "crates/debug-export",
        "crates/language-server",
        "crates/fe/src/trace",
        "crates/fe/src/debug_cli.rs",
    ];
    let mut violations = Vec::new();

    for root in guarded_roots {
        let path = workspace.join(root);
        visit_rust_files(&path, &mut |file| {
            let text = fs::read_to_string(file).expect("rust source should be readable");
            if text.contains("common::facts")
                || text.contains("common::{facts")
                || text.contains("common::{ facts")
            {
                violations.push(
                    file.strip_prefix(workspace)
                        .unwrap_or(file)
                        .display()
                        .to_string(),
                );
            }
        });
    }

    assert!(
        violations.is_empty(),
        "legacy common::facts imports are quarantined to analyze compatibility paths: {violations:?}"
    );
}

fn visit_rust_files(path: &Path, f: &mut impl FnMut(&Path)) {
    if path.is_file() {
        if path.extension().is_some_and(|extension| extension == "rs") {
            f(path);
        }
        return;
    }
    let Ok(entries) = fs::read_dir(path) else {
        return;
    };
    for entry in entries {
        let path: PathBuf = entry.expect("directory entry should be readable").path();
        if path.is_dir() {
            visit_rust_files(&path, f);
        } else if path.extension().is_some_and(|extension| extension == "rs") {
            f(&path);
        }
    }
}
