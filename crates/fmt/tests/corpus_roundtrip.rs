//! Corpus round-trip property test.
//!
//! Walks every `.fe` file in the repository and asserts, for each file that
//! parses cleanly:
//!
//! - `fmt(file)` succeeds,
//! - `parse(fmt(file))` succeeds — the formatter must never emit output the
//!   parser rejects (e.g. a line-start `-` after wrapping),
//! - `fmt(fmt(file)) == fmt(file)` — formatting is idempotent,
//! - `fmt(file)` keeps the same tokens as `file`: the same sequence of
//!   identifiers, keywords, literals, operators and comment text, ignoring
//!   whitespace and layout punctuation (see `is_layout_token`). This catches
//!   the formatter dropping code (an attribute, a comment, a visibility
//!   restriction) even when the result still parses.

use std::fs;
use std::path::{Path, PathBuf};

use fe_fmt::{Config, FormatError, format_str};
use parser::{RecoveryMode, SyntaxKind, SyntaxNode, SyntaxToken, parse_source_file};

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

/// The tokens of a file that formatting must not change, in order: all tokens
/// except whitespace, line breaks and layout punctuation. Comments are compared
/// without trailing whitespace. `<<` in types is two `<` tokens, so `< <` and
/// `<<` compare equal.
fn significant_tokens(source: &str) -> Vec<String> {
    let (green, _) = parse_source_file(source, RecoveryMode::new(true));
    SyntaxNode::new_root(green)
        .descendants_with_tokens()
        .filter_map(|element| element.into_token())
        .filter(|token| !is_layout_token(token))
        .map(|token| token.text().trim_end().to_string())
        .collect()
}

fn is_layout_token(token: &SyntaxToken) -> bool {
    let parent = token.parent();
    let parent_kind = parent.as_ref().map(|parent| parent.kind());
    let grandparent_kind = parent
        .as_ref()
        .and_then(|parent| parent.parent())
        .map(|grandparent| grandparent.kind());
    match token.kind() {
        SyntaxKind::WhiteSpace | SyntaxKind::Newline => true,
        // List entries are separated by commas or line breaks, and trailing
        // commas come and go with the layout. Only a one-element tuple needs
        // its comma: `(x,)` is a tuple, `(x)` is not. A tuple variant's single
        // field (`Some(T,)`, `Some(x,)`) is the same either way.
        SyntaxKind::Comma => {
            let one_element_tuple = matches!(
                parent_kind,
                Some(SyntaxKind::TupleType | SyntaxKind::TupleExpr | SyntaxKind::TuplePatElemList)
            ) && !matches!(
                grandparent_kind,
                Some(SyntaxKind::VariantDef | SyntaxKind::PathTuplePat)
            ) && parent
                .is_some_and(|parent| parent.children().count() == 1);
            !one_element_tuple
        }
        // The formatter puts braces around a match arm body such as
        // `=> return x`.
        SyntaxKind::LBrace | SyntaxKind::RBrace => {
            parent_kind == Some(SyntaxKind::BlockExpr)
                && grandparent_kind == Some(SyntaxKind::MatchArm)
        }
        _ => false,
    }
}

/// Describes where two token sequences first differ, with a little context.
fn token_difference(before: &[String], after: &[String]) -> Option<String> {
    let at = before
        .iter()
        .zip(after)
        .position(|(b, a)| b != a)
        .unwrap_or(before.len().min(after.len()));
    if at == before.len() && at == after.len() {
        return None;
    }
    let window = |tokens: &[String]| {
        let start = at.saturating_sub(3);
        let end = (at + 5).min(tokens.len());
        tokens[start..end].join(" ")
    };
    Some(format!(
        "at token {at}\n  before: {}\n  after:  {}",
        window(before),
        window(after)
    ))
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

        if let Some(difference) = token_difference(
            &significant_tokens(&source),
            &significant_tokens(&formatted),
        ) {
            failures.push(format!(
                "{display}: formatting changed the code {difference}"
            ));
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
