use fe_parser::{
    RecoveryMode, SyntaxNode,
    ast::{QuoteExpr, QuoteHoleExpr, prelude::AstNode},
    parse_source_file,
};

#[test]
fn item_quote_preserves_authored_function_and_scalar_hole_syntax() {
    let source =
        "const fn provider() { let n = 2\n quote { fn output() -> u256 { boost(${n}) } } }";
    let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
    assert!(errors.is_empty(), "{errors:?}");
    let root = SyntaxNode::new_root(green);
    assert_eq!(root.to_string(), source);
    let quotes: Vec<_> = root.descendants().filter_map(QuoteExpr::cast).collect();
    assert_eq!(quotes.len(), 1);
    assert_eq!(quotes[0].body().unwrap().items().count(), 1);
    let holes: Vec<_> = root.descendants().filter_map(QuoteHoleExpr::cast).collect();
    assert_eq!(holes.len(), 1);
    assert_eq!(holes[0].value().unwrap().syntax().to_string(), "n");
}

#[test]
fn holes_outside_quotes_reject_and_recover_without_issuing_templates() {
    let source = "fn invalid() { ${2} }\nfn valid() { 42 }";
    let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
    assert!(
        errors.iter().any(|e| e
            .msg()
            .contains("quotation holes are only allowed inside quote")),
        "{errors:?}"
    );
    let root = SyntaxNode::new_root(green);
    assert_eq!(root.descendants().filter_map(QuoteExpr::cast).count(), 0);
}

#[test]
fn ordinary_quote_identifier_and_newline_block_are_not_a_quote() {
    let source = "fn ordinary() { let quote = 2\n quote\n { 3 } }";
    let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
    assert!(errors.is_empty(), "{errors:?}");
    let root = SyntaxNode::new_root(green);
    assert_eq!(root.descendants().filter_map(QuoteExpr::cast).count(), 0);
}

#[test]
fn malformed_holes_keep_parser_errors() {
    for source in [
        "fn p() { quote { fn f() { $n } } }",
        "fn p() { quote { fn f() { ${} } } }",
        "fn p() { quote { fn f() { ${2 } }",
    ] {
        let (_, errors) = parse_source_file(source, RecoveryMode::Recover);
        assert!(!errors.is_empty(), "{source}");
    }
}
