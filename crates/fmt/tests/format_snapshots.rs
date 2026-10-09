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
fn bare_qualified_first_generic_arguments_do_not_become_shifts() {
    let source = r#"
fn wrap(_ x: Wrapped< <T as Model>>, _ y: Wrapped< <T as Model>::Point>) {
    let w = make< <T as Model>>(x)
}
"#;
    let formatted = format_str(source, &Config::default()).expect("format should succeed");

    assert!(formatted.contains("Wrapped< <T as Model> >"), "{formatted}");
    assert!(formatted.contains("make< <T as Model> >(x)"), "{formatted}");
    assert!(
        formatted.contains("Wrapped<<T as Model>::Point>"),
        "{formatted}"
    );
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
            .filter(|node| node.kind() == SyntaxKind::GenericArgList)
            .filter(|node| node
                .parent()
                .is_some_and(|parent| parent.kind() == SyntaxKind::PathSegment))
            .count(),
        3,
        "{syntax:#?}",
    );
    assert!(
        syntax
            .descendants()
            .all(|node| node.kind() != SyntaxKind::BinExpr),
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
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/const_where_predicates.fe");
    let source = std::fs::read_to_string(path)
        .expect("fixture should be readable")
        .replace("\r\n", "\n");
    let formatted = format_str(&source, &Config::default()).unwrap();
    assert_eq!(predicates(&source), predicates(&formatted), "{formatted}");
    assert_eq!(
        formatted,
        format_str(&formatted, &Config::default()).unwrap()
    );
    assert!(formatted.contains("// keep this condition"), "{formatted}");
}

/// Inner attributes of a module and attributes on `for` loops are kept, and
/// a multiline string argument keeps its whitespace. This source is inline
/// rather than a fixture because the tree-sitter grammar, which parses every
/// formatter fixture, does not accept these attribute positions yet.
#[test]
fn inner_and_loop_attributes_survive_formatting() {
    let source = r#"mod inner {
    #![arithmetic(unchecked)]

    pub(ingot) fn sum() -> u256 {
        let mut total: u256 = 0
        #[unroll]
        for i in 0..4 {
            total += i
        }
        total
    }
}

fn commented_loop() {
    // comment before the loop
    #[unroll]
    for i in 0..2 { // trailing comment
        let _ = i
    }
}

mod strings {
    #![example(text = "first
  second

    third")]
    #![other]

    fn looped() {
        #[example(text = "one
   two")]
        for i in 0..2 {
            let _ = i
        }
    }
}
"#;
    let expected = r#"mod inner {
    #![arithmetic(unchecked)]
    pub(ingot) fn sum() -> u256 {
        let mut total: u256 = 0
        #[unroll]
        for i in 0 .. 4 {
            total += i
        }
        total
    }
}

fn commented_loop() {
    // comment before the loop
    #[unroll]
    for i in 0 .. 2 {
        // trailing comment
        let _ = i
    }
}

mod strings {
    #![example(text = "first
  second

    third")]
    #![other]
    fn looped() {
        #[example(text = "one
   two")]
        for i in 0 .. 2 {
            let _ = i
        }
    }
}
"#;
    let formatted = format_str(source, &Config::default()).expect("format should succeed");
    assert_eq!(formatted.trim_end(), expected.trim_end());
    let reformatted = format_str(&formatted, &Config::default()).expect("reformat should succeed");
    assert_eq!(reformatted, formatted, "formatting is not stable");
}
