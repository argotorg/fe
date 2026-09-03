use dir_test::{Fixture, dir_test};
use fe_fmt::{Config, format_str};
use parser::{RecoveryMode, SyntaxKind, SyntaxNode, parse_source_file};
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures",
    glob: "*.fe"
)]
fn format_snap(fixture: Fixture<&str>) {
    let config = Config::default();
    let output = match format_str(fixture.content(), &config) {
        Ok(formatted) => formatted,
        Err(err) => format!("FORMAT ERROR: {err:?}"),
    };

    snap_test!(output, fixture.path());
}

#[test]
fn ambiguous_binary_operators_break_after_the_operator() {
    let source = r#"
fn calculate() {
    let product = very_long_left_side_expression_name * very_long_right_side_expression_name_that_is_even_longer
    let difference = very_long_left_side_expression_name - very_long_right_side_expression_name_that_is_even_longer
}
"#;
    let formatted = format_str(source, &Config::default()).expect("format should succeed");

    assert!(
        formatted.contains("very_long_left_side_expression_name *\n"),
        "{formatted}",
    );
    assert!(
        formatted.contains("very_long_left_side_expression_name -\n"),
        "{formatted}",
    );

    let (green, errors) = parse_source_file(&formatted, RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:#?}\n{formatted}");

    let syntax = SyntaxNode::new_root(green);
    assert_eq!(
        syntax
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::BinExpr)
            .count(),
        2,
        "{syntax:#?}",
    );
    assert!(
        syntax
            .descendants()
            .all(|node| node.kind() != SyntaxKind::UnExpr),
        "{syntax:#?}",
    );
}

#[test]
fn nested_dereferences_remain_lexically_separate() {
    let source = r#"
fn load(pointer: **u256) -> u256 {
    * *pointer
}
"#;
    let formatted = format_str(source, &Config::default()).expect("format should succeed");

    assert!(formatted.contains("* *pointer"), "{formatted}");
    assert_eq!(
        format_str(&formatted, &Config::default()).expect("reformat should succeed"),
        formatted,
    );

    let (green, errors) = parse_source_file(&formatted, RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:#?}\n{formatted}");

    let syntax = SyntaxNode::new_root(green);
    assert_eq!(
        syntax
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::UnExpr)
            .count(),
        2,
        "{syntax:#?}",
    );
}
