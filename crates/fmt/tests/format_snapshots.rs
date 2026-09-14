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

#[test]
fn const_where_predicates_survive_formatting_in_source_order() {
    fn predicates(source: &str) -> Vec<(SyntaxKind, String)> {
        let (green, errors) = parse_source_file(source, RecoveryMode::NoRecover);
        assert!(errors.is_empty(), "{errors:?}\n{source}");
        SyntaxNode::new_root(green)
            .descendants()
            .filter(|node| {
                matches!(
                    node.kind(),
                    SyntaxKind::WherePredicate | SyntaxKind::WhereConstPredicate
                )
            })
            .map(|node| {
                (
                    node.kind(),
                    node.descendants_with_tokens()
                        .filter_map(|element| element.into_token())
                        .filter(|token| {
                            !matches!(token.kind(), SyntaxKind::WhiteSpace | SyntaxKind::Newline)
                        })
                        .map(|token| token.text().to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                )
            })
            .collect()
    }
    for source in [
        "fn f() where false {}",
        "struct S where FLAG { value: u256 }",
        "enum E where true { A }",
        "impl S where true { fn f() where false {} }",
        "fn f<T>() where true, T: Copy, false {}",
        "fn f<T>() where T: Copy,\n// keep this condition\nfalse, true {}",
        "trait T { fn f() where true\nfn g() where false }",
        "fn f() where ({ let n: u256 = 3\n n > 0 }) {}",
    ] {
        let formatted = format_str(source, &Config::default()).unwrap();
        assert_eq!(predicates(source), predicates(&formatted), "{formatted}");
        assert_eq!(
            formatted,
            format_str(&formatted, &Config::default()).unwrap()
        );
        if source.contains("// keep this condition") {
            assert!(formatted.contains("// keep this condition"), "{formatted}");
        }
    }
}
