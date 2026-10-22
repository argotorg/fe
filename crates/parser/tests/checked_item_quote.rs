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

#[test]
fn expr_arm_where_and_open_binder_quotes_parse_without_errors() {
    let source = r#"const fn provider() {
    let _e = quote(min, max) { self.${f} >= min && self.${f} <= max }
    let _w = quote { where ${bounds}, ${ty}: Eq }
    let _a = quote(other) {
        ${arms}
        ${lhs} => match other {
            ${rhs} => ${cmp}
            _ => false
        }
    }
    let _i = quote {
        impl Eq for ${record} where ${bounds} {
            fn eq(self, _ other: Self) -> bool {
                match self { ${arms} }
            }
        }
    }
}"#;
    let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
    assert!(errors.is_empty(), "{errors:?}");
    let root = SyntaxNode::new_root(green);
    let quotes: Vec<_> = root.descendants().filter_map(QuoteExpr::cast).collect();
    assert_eq!(quotes.len(), 4);
}

#[test]
fn ordinary_quote_call_with_following_block_is_not_parsed_as_quote_open_list() {
    let source = r#"fn quote(_ a: u256, _ b: u256) -> u256 { a + b }
fn caller() -> u256 {
    let x = quote(1, 2)
    if quote(x, 3) == 6 {
        42
    } else {
        0
    }
}"#;
    let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
    assert!(errors.is_empty(), "{errors:?}");
    let root = SyntaxNode::new_root(green);
    assert_eq!(root.descendants().filter_map(QuoteExpr::cast).count(), 0);

    let bad_open = "const fn p() { quote(1 + 2) { 3 } }";
    let (_, bad_errors) = parse_source_file(bad_open, RecoveryMode::Recover);
    assert!(
        bad_errors
            .iter()
            .any(|e| e.msg().contains("expected an open name")),
        "{bad_errors:?}"
    );
}

#[test]
fn quoted_item_whitelist_does_not_restrict_ordinary_local_declarations() {
    let nested = "const fn make() { quote { fn generated() { trait Local { const VALUE: u256 }\n impl Local for Pair { const VALUE: u256 = 7 } } } }";
    let (_, errors) = parse_source_file(nested, RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:?}");
    let root_trait = "const fn make() { quote { trait Local { const VALUE: u256 } } }";
    let (_, errors) = parse_source_file(root_trait, RecoveryMode::Recover);
    assert!(
        errors.iter().any(|error| error
            .msg()
            .contains("only functions, constants and impls can be quoted")),
        "{errors:?}"
    );
}
