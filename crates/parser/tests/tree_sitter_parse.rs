use dir_test::{dir_test, Fixture};
use tree_sitter::Parser;

fn parse_and_collect_errors(source: &str) -> Vec<String> {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_fe::LANGUAGE.into())
        .expect("failed to load Fe grammar");

    let tree = parser.parse(source, None).expect("failed to parse");
    let mut errors = Vec::new();
    collect_errors(tree.root_node(), source, &mut errors);
    errors
}

fn collect_errors(node: tree_sitter::Node, source: &str, errors: &mut Vec<String>) {
    if node.is_error() {
        let start = node.start_position();
        let snippet: String = source[node.byte_range()].chars().take(40).collect();
        errors.push(format!(
            "ERROR at {}:{}: {:?}",
            start.row + 1,
            start.column + 1,
            snippet,
        ));
    } else if node.is_missing() {
        let start = node.start_position();
        errors.push(format!(
            "MISSING {} at {}:{}",
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

fn assert_parses_without_errors(fixture: &Fixture<&str>) {
    let errors = parse_and_collect_errors(fixture.content());
    assert!(
        errors.is_empty(),
        "tree-sitter parse errors in {}:\n{}",
        fixture.path(),
        errors.join("\n")
    );
}

// Parser syntax node fixtures
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/items",
    glob: "*.fe"
)]
fn ts_parse_items(fixture: Fixture<&str>) {
    assert_parses_without_errors(&fixture);
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/structs",
    glob: "*.fe"
)]
fn ts_parse_structs(fixture: Fixture<&str>) {
    assert_parses_without_errors(&fixture);
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/stmts",
    glob: "*.fe"
)]
fn ts_parse_stmts(fixture: Fixture<&str>) {
    assert_parses_without_errors(&fixture);
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/exprs",
    glob: "*.fe"
)]
fn ts_parse_exprs(fixture: Fixture<&str>) {
    assert_parses_without_errors(&fixture);
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/pats",
    glob: "*.fe"
)]
fn ts_parse_pats(fixture: Fixture<&str>) {
    assert_parses_without_errors(&fixture);
}

// Formatter fixtures
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/../fmt/tests/fixtures",
    glob: "*.fe"
)]
fn ts_parse_fmt(fixture: Fixture<&str>) {
    assert_parses_without_errors(&fixture);
}

// Standard library: core ingot
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/../../ingots/core/src",
    glob: "**/*.fe"
)]
fn ts_parse_core(fixture: Fixture<&str>) {
    assert_parses_without_errors(&fixture);
}

// Standard library: std ingot
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/../../ingots/std/src",
    glob: "**/*.fe"
)]
fn ts_parse_std(fixture: Fixture<&str>) {
    assert_parses_without_errors(&fixture);
}
