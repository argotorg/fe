use std::path::{Path, PathBuf};
use tree_sitter::Parser;

const MAX_ERRORS_PER_FILE: usize = 5;

// Files that are intentionally broken or contain fragments (not valid top-level Fe).
const EXCLUDED_FILES: &[&str] = &[
    "parse_error.fe", // cli_output: intentional parse error
];

fn new_parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_fe::LANGUAGE.into())
        .expect("failed to load Fe grammar");
    parser
}

fn collect_fe_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    collect_fe_files_recursive(dir, &mut files);
    files.sort();
    files
}

fn collect_fe_files_recursive(dir: &Path, files: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).unwrap_or_else(|e| panic!("{}: {e}", dir.display())) {
        let path = entry.unwrap().path();
        if path.is_dir() {
            collect_fe_files_recursive(&path, files);
        } else if path.extension().is_some_and(|ext| ext == "fe") {
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                if EXCLUDED_FILES.contains(&name) {
                    continue;
                }
            }
            files.push(path);
        }
    }
}

fn collect_errors(node: tree_sitter::Node, source: &str, errors: &mut Vec<String>) {
    if errors.len() >= MAX_ERRORS_PER_FILE {
        return;
    }
    if node.is_error() {
        let start = node.start_position();
        let snippet: String = source[node.byte_range()].chars().take(40).collect();
        errors.push(format!(
            "    ERROR at {}:{}: {:?}",
            start.row + 1,
            start.column + 1,
            snippet,
        ));
    } else if node.is_missing() {
        let start = node.start_position();
        errors.push(format!(
            "    MISSING {} at {}:{}",
            node.kind(),
            start.row + 1,
            start.column + 1,
        ));
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_errors(child, source, errors);
    }
}

struct SuiteResult {
    label: String,
    total: usize,
    failures: Vec<String>,
}

fn run_suite(label: &str, dir: &Path, parser: &mut Parser) -> SuiteResult {
    let files = collect_fe_files(dir);
    assert!(!files.is_empty(), "no .fe files found in {}", dir.display());

    let mut failures = Vec::new();

    for (i, path) in files.iter().enumerate() {
        let relative = path.strip_prefix(dir).unwrap_or(path);
        eprintln!("    [{}/{}] {}", i + 1, files.len(), relative.display());
        let source = std::fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("cannot read {}: {e}", path.display()));
        let tree = parser.parse(&source, None).expect("parser returned None");

        let mut errors = Vec::new();
        collect_errors(tree.root_node(), &source, &mut errors);

        if !errors.is_empty() {
            let truncated = if errors.len() >= MAX_ERRORS_PER_FILE {
                " ..."
            } else {
                ""
            };
            failures.push(format!(
                "  {}:\n{}{}",
                relative.display(),
                errors.join("\n"),
                truncated,
            ));
        }
    }

    SuiteResult {
        label: label.to_string(),
        total: files.len(),
        failures,
    }
}

fn format_report(results: &[SuiteResult]) -> String {
    let total_files: usize = results.iter().map(|r| r.total).sum();
    let total_failures: usize = results.iter().map(|r| r.failures.len()).sum();
    let total_passed = total_files - total_failures;

    let mut report = format!(
        "\ntree-sitter: {total_passed}/{total_files} passed ({:.1}%)\n",
        100.0 * total_passed as f64 / total_files as f64,
    );
    for result in results {
        if !result.failures.is_empty() {
            report.push_str(&format!(
                "\n[{}] ({}/{} failed):\n{}\n",
                result.label,
                result.failures.len(),
                result.total,
                result.failures.join("\n"),
            ));
        }
    }
    report
}

/// Strict test: these suites must parse with zero errors.
/// Covers syntax_node fixtures, formatter fixtures, and the core/std ingots.
#[test]
fn tree_sitter_parse_strict() {
    let mut parser = new_parser();
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));

    let suites: &[(&str, PathBuf)] = &[
        ("items", manifest.join("test_files/syntax_node/items")),
        ("structs", manifest.join("test_files/syntax_node/structs")),
        ("stmts", manifest.join("test_files/syntax_node/stmts")),
        ("exprs", manifest.join("test_files/syntax_node/exprs")),
        // pats/ excluded: standalone patterns aren't valid top-level Fe.
        ("fmt", manifest.join("../fmt/tests/fixtures")),
        ("core", manifest.join("../../ingots/core/src")),
        ("std", manifest.join("../../ingots/std/src")),
    ];

    let mut results = Vec::new();
    for (label, dir) in suites {
        results.push(run_suite(label, dir, &mut parser));
    }

    let total_failures: usize = results.iter().map(|r| r.failures.len()).sum();
    if total_failures > 0 {
        panic!("{}", format_report(&results));
    }
}

/// Broader coverage test: parses all Fe fixtures across the repo.
/// Tracks progress and prevents regressions — the grammar must parse at
/// least MINIMUM_PASS_RATE percent of files. As the grammar improves,
/// ratchet this number up.
#[test]
fn tree_sitter_parse_coverage() {
    const MINIMUM_PASS_RATE: f64 = 83.0;

    let mut parser = new_parser();
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));

    let suites: &[(&str, PathBuf)] = &[
        // fe crate integration tests
        ("fe_test", manifest.join("../fe/tests/fixtures/fe_test")),
        (
            "fe_test_runner",
            manifest.join("../fe/tests/fixtures/fe_test_runner"),
        ),
        (
            "cli_output",
            manifest.join("../fe/tests/fixtures/cli_output"),
        ),
        // uitest fixtures (excluding parser/ which has intentional errors)
        (
            "uitest_mir",
            manifest.join("../uitest/fixtures/mir_check"),
        ),
        (
            "uitest_names",
            manifest.join("../uitest/fixtures/name_resolution"),
        ),
        ("uitest_ty", manifest.join("../uitest/fixtures/ty")),
        (
            "uitest_tyck",
            manifest.join("../uitest/fixtures/ty_check"),
        ),
    ];

    let mut results = Vec::new();
    for (label, dir) in suites {
        if dir.exists() {
            results.push(run_suite(label, dir, &mut parser));
        } else {
            eprintln!("  skipping {label}: {} not found", dir.display());
        }
    }

    let total_files: usize = results.iter().map(|r| r.total).sum();
    let total_failures: usize = results.iter().map(|r| r.failures.len()).sum();
    let total_passed = total_files - total_failures;
    let pass_rate = 100.0 * total_passed as f64 / total_files as f64;

    let report = format_report(&results);
    eprintln!("{report}");

    assert!(
        pass_rate >= MINIMUM_PASS_RATE,
        "tree-sitter coverage regressed: {pass_rate:.1}% < {MINIMUM_PASS_RATE}%\n{report}",
    );
}
