#![allow(clippy::print_stdout, clippy::print_stderr)]

use std::path::{Path, PathBuf};

use fe_parser::{RecoveryMode, parse_source_file};
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
            if let Some(name) = path.file_name().and_then(|n| n.to_str())
                && EXCLUDED_FILES.contains(&name)
            {
                continue;
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

fn parse_errors(parser: &mut Parser, source: &str) -> Vec<String> {
    let tree = parser.parse(source, None).expect("parser returned None");
    let mut errors = Vec::new();
    collect_errors(tree.root_node(), source, &mut errors);
    errors
}

fn body_expression_kinds(parser: &mut Parser, source: &str) -> Vec<String> {
    let tree = parser.parse(source, None).expect("parser returned None");
    let mut errors = Vec::new();
    collect_errors(tree.root_node(), source, &mut errors);
    assert!(
        errors.is_empty(),
        "unexpected parse errors:\n{}",
        errors.join("\n"),
    );

    let function = tree
        .root_node()
        .named_child(0)
        .expect("source should contain a function");
    let body = function
        .child_by_field_name("body")
        .expect("function should contain a body");
    (0..body.named_child_count())
        .filter_map(|index| body.named_child(index))
        .filter(|statement| statement.kind() == "expression_statement")
        .map(|statement| {
            statement
                .named_child(0)
                .expect("expression statement should contain an expression")
                .kind()
                .to_string()
        })
        .collect()
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

#[test]
fn tree_sitter_parse_newline_lt_continuations() {
    let mut parser = new_parser();
    let cases = [
        (
            "bare_newline_lt",
            "fn f(x: i32, y: i32) {\n    let a = x\n        < y\n}\n",
            true,
        ),
        (
            "bare_newline_lshift",
            "fn f(x: i32, y: i32) {\n    let a = x\n        << y\n}\n",
            true,
        ),
        (
            "delimited_newline_lt",
            "fn f(x: i32, y: i32) {\n    let a = (\n        x\n        < y\n    )\n}\n",
            false,
        ),
        (
            "delimited_newline_lshift",
            "fn f(x: i32, y: i32) {\n    let a = (\n        x\n        << y\n    )\n}\n",
            false,
        ),
        (
            "newline_lte",
            "fn f(x: i32, y: i32) {\n    let a = x\n        <= y\n}\n",
            false,
        ),
        (
            "newline_lshift_assign",
            "fn f(x: i32, y: i32) {\n    var a = x\n    a\n        <<= y\n}\n",
            false,
        ),
        (
            "newline_nested_qualified_path",
            "trait Foo { fn assoc() {} }\ntrait Bar { fn baz() {} }\nstruct T {}\n\nfn f(x: i32) {\n    x\n    <<T as Foo>::Assoc as Bar>::baz()\n}\n",
            false,
        ),
    ];

    for (name, source, should_error) in cases {
        let errors = parse_errors(&mut parser, source);
        if should_error {
            assert!(
                !errors.is_empty(),
                "expected parse error for {name}, but parse succeeded",
            );
        } else {
            assert!(
                errors.is_empty(),
                "unexpected parse errors for {name}:\n{}",
                errors.join("\n"),
            );
        }
    }
}

/// The compiler reads a `<<` as generic arguments when a qualified path follows,
/// in expression position as well as in type position, so the grammar has to
/// agree or editors flag valid code. A `<<` with anything else after it is still
/// a shift.
#[test]
fn tree_sitter_parse_qualified_first_generic_arg_in_expressions() {
    let mut parser = new_parser();

    const PRELUDE: &str =
        "trait Model {\n    type Point\n}\nstruct Wrapped<T> {\n    value: T,\n}\nstruct M {}\n";

    let accepted = [
        (
            "associated_function_call",
            "fn f(p: u256) -> Wrapped<<M as Model>::Point> {\n    Wrapped<<M as Model>::Point>::new(p)\n}\n",
        ),
        (
            "record_literal",
            "fn f(p: u256) -> Wrapped<<M as Model>::Point> {\n    Wrapped<<M as Model>::Point> { value: p }\n}\n",
        ),
        (
            "method_call",
            "fn f(w: Wrapped<u256>, p: u256) -> u256 {\n    w.pick<<M as Model>::Point>(p)\n}\n",
        ),
    ];

    for (name, body) in accepted {
        let source = format!("{PRELUDE}{body}");
        let errors = parse_errors(&mut parser, &source);
        assert!(
            errors.is_empty(),
            "unexpected parse errors for {name}:\n{}",
            errors.join("\n"),
        );
    }

    // `<<` followed by an operand rather than a qualified path is a shift, and a
    // qualified operand whose `>` is a comparison rather than `>::` still is.
    assert_eq!(
        body_expression_kinds(
            &mut parser,
            "fn shift(value: u256, bits: u8) -> u256 {\n    value << bits as u256 >> 1\n}\n",
        ),
        ["binary_expression"],
    );
    assert_eq!(
        body_expression_kinds(
            &mut parser,
            "fn compare(value: u8, limit: u8) -> bool {\n    value << <M as Model>::BITS > limit\n}\n",
        ),
        ["binary_expression"],
    );

    // Deciding a `<<` means looking ahead for `>::`, and the lookahead runs on to
    // the end of the enclosing block before giving up. Text inside a string or a
    // comment is not code, so a `>::` there leaves an ordinary shift alone.
    let shift_then_text = [
        (
            "string_holding_gt_colon2",
            "fn f(x: u256, y: u256) -> u256 {\n    let a = x << y\n    let s = \"a>::b\"\n    a\n}\n",
        ),
        (
            "line_comment_holding_gt_colon2",
            "fn f(x: u256, y: u256) -> u256 {\n    let a = x << y\n    // note a>::b\n    a\n}\n",
        ),
        (
            "block_comment_holding_gt_colon2",
            "fn f(x: u256, y: u256) -> u256 {\n    let a = x << y\n    /* note a>::b */\n    a\n}\n",
        ),
        (
            "shift_assign_then_string",
            "fn f(x: u256, y: u256) -> u256 {\n    var a = x\n    a <<= y\n    let s = \"a>::b\"\n    a\n}\n",
        ),
    ];
    for (name, source) in shift_then_text {
        let errors = parse_errors(&mut parser, source);
        assert!(
            errors.is_empty(),
            "unexpected parse errors for {name}:\n{}",
            errors.join("\n"),
        );
    }
}

#[test]
fn tree_sitter_matches_line_start_star_policy() {
    let mut parser = new_parser();

    assert_eq!(
        body_expression_kinds(
            &mut parser,
            "fn choose(value: u256, pointer: *u256) -> u256 {\n    value\n    *pointer\n}\n",
        ),
        ["identifier", "unary_expression"],
    );
    assert_eq!(
        body_expression_kinds(
            &mut parser,
            "fn multiply(value: u256, rhs: u256) -> u256 {\n    value *\n        rhs\n}\n",
        ),
        ["binary_expression"],
    );
    assert_eq!(
        body_expression_kinds(
            &mut parser,
            "fn update(mut value: u256, rhs: u256) {\n    value\n        *= rhs\n}\n",
        ),
        ["augmented_assignment_expression"],
    );
    assert_eq!(
        body_expression_kinds(
            &mut parser,
            "fn power(value: u256, rhs: u256) -> u256 {\n    value\n        ** rhs\n}\n",
        ),
        ["binary_expression"],
    );
}

#[test]
fn tree_sitter_parse_condition_or_chains() {
    let mut parser = new_parser();
    let cases = [
        (
            "chained_or",
            "fn f(a: bool, b: bool, c: bool) {\n    if a || b || c {}\n}\n",
            false,
        ),
        (
            "let_then_or",
            "fn f(opt: Option<bool>, ready: bool) {\n    if let Some(value) = opt || ready {}\n}\n",
            true,
        ),
        (
            "or_then_let",
            "fn f(opt: Option<bool>, ready: bool) {\n    if ready || let Some(value) = opt {}\n}\n",
            true,
        ),
    ];

    for (name, source, should_error) in cases {
        let errors = parse_errors(&mut parser, source);
        assert_eq!(
            !errors.is_empty(),
            should_error,
            "unexpected parse result for {name}: {}",
            errors.join("\n"),
        );
    }
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
            "uitest_names",
            manifest.join("../uitest/fixtures/name_resolution"),
        ),
        ("uitest_ty", manifest.join("../uitest/fixtures/ty")),
        ("uitest_tyck", manifest.join("../uitest/fixtures/ty_check")),
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

#[test]
fn tree_sitter_matches_string_escape_policy() {
    let mut parser = new_parser();
    for (literal, valid) in [
        (r#""\"\\\n\r\t""#, true),
        (r#""\\n""#, true),
        (r#""tail\\""#, true),
        (r#""'é🦀""#, true),
        ("\"first\nsecond\"", true),
        (r#""""#, true),
        (r#""\'""#, false),
        (r#""\0""#, false),
        (r#""\x41""#, false),
        (r#""\u{41}""#, false),
        (r#""\q""#, false),
        (r#""\é""#, false),
    ] {
        let source = format!("fn literal() {{ let text = {literal} }}");
        let (_, errors) = parse_source_file(&source, RecoveryMode::Recover);
        assert_eq!(errors.is_empty(), valid, "{source}: {errors:?}");
        let errors = parse_errors(&mut parser, &source);
        assert_eq!(errors.is_empty(), valid, "{source}: {errors:?}");
    }
}

/// An item with a `where` clause as a parser reads it: its kind, whether a
/// function has a body, and the kind of each of its `where` predicates, in
/// order.
#[derive(Debug, PartialEq, Eq)]
struct ItemShape {
    kind: &'static str,
    /// Whether the item has a body, for functions only.
    body: Option<bool>,
    predicates: Vec<&'static str>,
}

fn parser_item_shapes(source: &str) -> Vec<ItemShape> {
    use fe_parser::{SyntaxKind, SyntaxNode};
    let (green, _) = parse_source_file(source, RecoveryMode::NoRecover);
    SyntaxNode::new_root(green)
        .descendants()
        .filter_map(|item| {
            let kind = match item.kind() {
                SyntaxKind::Func => "function",
                SyntaxKind::Struct => "struct",
                SyntaxKind::Enum => "enum",
                SyntaxKind::Impl => "impl",
                SyntaxKind::ImplTrait => "impl trait",
                SyntaxKind::Trait => "trait",
                _ => return None,
            };
            // A function's `where` clause is part of its signature.
            let clauses = item
                .children()
                .flat_map(|child| {
                    if child.kind() == SyntaxKind::FuncSignature {
                        child.children().collect()
                    } else {
                        vec![child]
                    }
                })
                .filter(|child| child.kind() == SyntaxKind::WhereClause);
            Some(ItemShape {
                kind,
                body: (kind == "function").then(|| {
                    item.children()
                        .any(|child| child.kind() == SyntaxKind::BlockExpr)
                }),
                predicates: clauses
                    .flat_map(|clause| clause.children())
                    .filter_map(|predicate| match predicate.kind() {
                        SyntaxKind::WherePredicate => Some("type bound"),
                        SyntaxKind::WhereConstPredicate => Some("condition"),
                        _ => None,
                    })
                    .collect(),
            })
        })
        .collect()
}

fn tree_sitter_item_shapes(parser: &mut Parser, source: &str) -> Vec<ItemShape> {
    fn walk(node: tree_sitter::Node, shapes: &mut Vec<ItemShape>) {
        let kind = match node.kind() {
            "function_definition" => Some("function"),
            "struct_definition" => Some("struct"),
            "enum_definition" => Some("enum"),
            "impl_block" => Some("impl"),
            "impl_trait" => Some("impl trait"),
            "trait_definition" => Some("trait"),
            _ => None,
        };
        if let Some(kind) = kind {
            let mut cursor = node.walk();
            let predicates = node
                .named_children(&mut cursor)
                .filter(|child| child.kind() == "where_clause")
                .flat_map(|clause| {
                    let mut cursor = clause.walk();
                    clause
                        .named_children(&mut cursor)
                        .filter_map(|predicate| match predicate.kind() {
                            "where_predicate" => Some("type bound"),
                            "where_const_predicate" => Some("condition"),
                            _ => None,
                        })
                        .collect::<Vec<_>>()
                })
                .collect();
            shapes.push(ItemShape {
                kind,
                body: (kind == "function").then(|| node.child_by_field_name("body").is_some()),
                predicates,
            });
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            walk(child, shapes);
        }
    }
    let tree = parser.parse(source, None).expect("parser returned None");
    let mut shapes = Vec::new();
    walk(tree.root_node(), &mut shapes);
    shapes
}

/// Files in the agreement corpus that the tree-sitter grammar cannot read for
/// reasons unrelated to `where` clauses, as on master: a block in a generic
/// parameter's default, and generic arguments on a pattern or record path.
const TREE_SITTER_UNREADABLE: &[&str] = &[
    "anonymous_constants_report_requirements_once_at_their_position/positions.fe",
    "type_parameter_defaults_check_every_written_constant/dropped_by_an_alias.fe",
    "types_written_before_a_path_separator_are_checked/expression_paths.fe",
    "written_types_report_their_conditions_once/const_parameter_defaults.fe",
];

/// The tree-sitter grammar and the compiler's parser read the `where` clause
/// of each function, struct, enum, impl and trait the same way, and each
/// function's body: which block is the body and which is a braced
/// condition, and where one predicate ends and the next starts.
#[test]
fn tree_sitter_agrees_on_where_clauses_and_function_bodies() {
    let mut parser = new_parser();
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let dirs = [
        manifest.join("test_files/syntax_node/items"),
        manifest.join("../fmt/tests/fixtures"),
        manifest.join("../uitest/fixtures/ty_check/const_where"),
        manifest.join("../../ingots/core/src"),
        manifest.join("../../ingots/std/src"),
    ];
    let mut disagreements = Vec::new();
    // The item kinds seen with at least one predicate, so that no kind
    // agrees only because neither parser found its `where` clause.
    let mut compared = std::collections::BTreeSet::new();
    for dir in &dirs {
        for path in collect_fe_files(dir) {
            if TREE_SITTER_UNREADABLE
                .iter()
                .any(|unreadable| path.ends_with(unreadable))
            {
                continue;
            }
            let source = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("{}: {e}", path.display()))
                .replace("\r\n", "\n");
            let expected = parser_item_shapes(&source);
            let found = tree_sitter_item_shapes(&mut parser, &source);
            compared.extend(
                expected
                    .iter()
                    .filter(|shape| !shape.predicates.is_empty())
                    .map(|shape| shape.kind),
            );
            if expected != found {
                disagreements.push(format!(
                    "{}:\n  parser:      {expected:?}\n  tree-sitter: {found:?}",
                    path.display()
                ));
            }
        }
    }
    assert!(disagreements.is_empty(), "{}", disagreements.join("\n"));
    assert_eq!(
        compared.into_iter().collect::<Vec<_>>(),
        ["enum", "function", "impl", "impl trait", "struct", "trait"],
    );
}
