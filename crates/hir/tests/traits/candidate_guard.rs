//! Trait candidates are found and decided in one place,
//! `analysis/ty/candidates.rs`, and a normalization limit is an unknown
//! answer, never a reason to stop a search (law 5 of the limits design).
//!
//! These checks read the compiler's source:
//! - the removed parallel paths and the helpers that let a caller drop a
//!   limit stay removed;
//! - outside the candidate module and the trait solver, no function both
//!   lists impl candidates and proves goals, so no caller proves a candidate
//!   before the question has filtered it;
//! - lists of impls are taken only where they decide nothing, or in the
//!   candidate module and the solver.

use std::path::{Path, PathBuf};

fn sources() -> Vec<(PathBuf, String)> {
    fn visit(dir: &Path, out: &mut Vec<(PathBuf, String)>) {
        for entry in std::fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                visit(&path, out);
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                let text = std::fs::read_to_string(&path).unwrap();
                out.push((path, text));
            }
        }
    }
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
    let mut out = Vec::new();
    visit(&root, &mut out);
    out.sort();
    out
}

fn relative(path: &Path) -> String {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
    path.strip_prefix(root)
        .unwrap()
        .to_string_lossy()
        .replace('\\', "/")
}

/// The source without `#[cfg(test)]` modules, which may build impl lists
/// to inspect them.
fn production(text: &str) -> &str {
    text.find("#[cfg(test)]\nmod tests")
        .map_or(text, |end| &text[..end])
}

/// The functions of a file, split at each line that starts a function.
fn functions(text: &str) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for line in text.lines() {
        let trimmed = line.trim_start();
        let starts = trimmed.starts_with("fn ")
            || trimmed.starts_with("pub fn ")
            || trimmed.starts_with("pub(crate) fn ")
            || trimmed.starts_with("pub(super) fn ")
            || trimmed.starts_with("pub(in ");
        if starts || out.is_empty() {
            out.push(String::new());
        }
        let last = out.last_mut().unwrap();
        last.push_str(line);
        last.push('\n');
    }
    out
}

/// Calls that list impls a question may select.
const CANDIDATE_SOURCES: &[&str] = &[
    "impls_for_ty(",
    "impls_for_trait_and_ty(",
    "impls_for_trait_def(",
    "impls_for_trait_in_ingots(",
    ".impls_for_trait(",
    ".impls_for_self_key(",
    "contract_virtual_impls(",
];

/// Calls that prove a goal.
const PROVERS: &[&str] = &[
    "is_goal_satisfiable(",
    "is_goal_query_satisfiable(",
    ".select_impl(",
];

/// Where the candidate module and the solver live.
const DECIDERS: &[&str] = &[
    "analysis/ty/candidates.rs",
    "analysis/ty/trait_resolution/table_solver.rs",
];

/// Files that take impl lists without proving anything about them.
const LISTING_ONLY: &[(&str, &str)] = &[
    ("analysis/ty/trait_def.rs", "defines the lists"),
    (
        "analysis/ty/mod.rs",
        "asks whether any impl header could match",
    ),
    (
        "analysis/ty/ty_check/stmt.rs",
        "finds the `Seq` impl of a loop's iterable by header",
    ),
    (
        "analysis/ty/ty_check/contract.rs",
        "lists a contract's impls by header",
    ),
    (
        "core/semantic/mod.rs",
        "lists impls by header for reflection and coherence",
    ),
];

#[test]
fn candidates_are_found_and_decided_in_one_module() {
    let sources = sources();
    assert!(
        sources
            .iter()
            .any(|(path, _)| relative(path) == "analysis/ty/candidates.rs"),
        "the candidate module is missing"
    );

    let mut problems = Vec::new();
    for (path, text) in &sources {
        let name = relative(path);
        let text = production(text);

        for removed in [
            "impls_for_ty_with_satisfied_constraints",
            "impls_for_ty_with_constraint_mode",
            "ConstraintMode",
            "first_limit",
            "fn is_satisfied(",
            "StopReason::NormalizationLimit",
            "GoalSatisfiability::NormalizationLimit",
        ] {
            if text.contains(removed) {
                problems.push(format!("{name}: `{removed}` is back"));
            }
        }

        if DECIDERS.contains(&name.as_str()) {
            continue;
        }
        let lists = CANDIDATE_SOURCES.iter().any(|call| text.contains(call));
        if lists && !LISTING_ONLY.iter().any(|(file, _)| *file == name) {
            problems.push(format!(
                "{name}: lists impl candidates outside the candidate module"
            ));
        }
        for function in functions(text) {
            let lists = CANDIDATE_SOURCES.iter().any(|call| function.contains(call));
            let proves = PROVERS.iter().any(|call| function.contains(call));
            if lists && proves {
                let head = function
                    .lines()
                    .next()
                    .unwrap_or_default()
                    .trim()
                    .to_string();
                problems.push(format!(
                    "{name}: `{head}` lists impl candidates and proves goals"
                ));
            }
        }
    }
    assert!(problems.is_empty(), "{}", problems.join("\n"));
}
