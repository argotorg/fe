use fe_parser::{
    RecoveryMode, SyntaxNode,
    ast::{AttrListOwner, ForStmt, QuoteExpr, QuoteHoleExpr, TraitShapeItem, prelude::AstNode},
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
            .contains("only functions, constants, types and impls can be quoted")),
        "{errors:?}"
    );
    // An associated type member is quotable; its destination admits it.
    let member_type = "const fn make() { quote { type Unit = u256\n const fn f() {} } }";
    let (_, errors) = parse_source_file(member_type, RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn an_associated_shape_may_open_or_follow_other_members_of_a_quote() {
    for body in [
        "shape Step = mut? u256\n fn at(mut? self) -> mut? u256 { 1 }",
        "fn at(mut? self) -> mut? u256 { 1 }\n shape Step = mut? u256",
        "shape Step = own u256",
    ] {
        let source = format!("const fn provider() {{ quote {{ {body} }} }}");
        let (green, errors) = parse_source_file(&source, RecoveryMode::Recover);
        assert!(errors.is_empty(), "{body}: {errors:?}");
        let root = SyntaxNode::new_root(green);
        let quotes: Vec<_> = root.descendants().filter_map(QuoteExpr::cast).collect();
        assert_eq!(quotes.len(), 1, "{body}");
        let items = quotes[0].item_list().unwrap_or_else(|| panic!("{body}"));
        assert_eq!(
            items
                .syntax()
                .children()
                .filter(|child| TraitShapeItem::can_cast(child.kind()))
                .count(),
            1,
            "{body}"
        );
    }
}

#[test]
fn shape_is_an_ordinary_identifier_in_a_quoted_expression() {
    let source = "const fn provider(shape: u256) { quote { shape } }";
    let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
    assert!(errors.is_empty(), "{errors:?}");
    let root = SyntaxNode::new_root(green);
    let quotes: Vec<_> = root.descendants().filter_map(QuoteExpr::cast).collect();
    assert!(quotes[0].item_list().is_none());
}

#[test]
fn a_quote_that_starts_with_a_var_statement_is_an_expression() {
    let source = "const fn make() -> u256 { let q = quote { var x = 1\n x }\n 0 }";
    let (_, errors) = parse_source_file(source, RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn a_quoted_const_predicate_may_start_with_a_splice() {
    use fe_parser::SyntaxKind::{self, QuoteHole, WhereConstPredicate, WherePredicate};
    // The predicates of the quoted `where` clause, in order. A splice that
    // is a whole predicate list or a type bound keeps its kind; one that an
    // operator follows starts a condition.
    for (body, kinds) in [
        ("where ${n} > 0", vec![WhereConstPredicate]),
        (
            "where ${n} > 0, ${t}: Copy",
            vec![WhereConstPredicate, WherePredicate],
        ),
        ("where ${w}", vec![QuoteHole]),
        ("where ${w}, ${t}: Copy", vec![QuoteHole, WherePredicate]),
        ("where ${t}: Copy", vec![WherePredicate]),
    ] {
        let source = format!("const fn pred() {{ quote {{ {body} }} }}");
        let (green, errors) = parse_source_file(&source, RecoveryMode::NoRecover);
        assert!(errors.is_empty(), "{body}: {errors:?}");
        let root = SyntaxNode::new_root(green);
        let clause = root
            .descendants()
            .find(|node| node.kind() == SyntaxKind::WhereClause)
            .unwrap_or_else(|| panic!("{body}"));
        let found: Vec<_> = clause.children().map(|child| child.kind()).collect();
        assert_eq!(found, kinds, "{body}");
    }
}

#[test]
fn an_attribute_on_the_first_statement_of_an_expression_quote_is_kept() {
    for source in [
        "const fn p() { quote {\n #[unroll]\n for x in [1, 2] {}\n } }",
        "const fn p() { quote { let n = 1\n #[unroll]\n for x in [1, 2] {}\n } }",
    ] {
        let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
        assert!(errors.is_empty(), "{source}: {errors:?}");
        let root = SyntaxNode::new_root(green);
        assert_eq!(root.to_string(), source);
        let quote = root.descendants().find_map(QuoteExpr::cast).unwrap();
        assert!(quote.item_list().is_none(), "{source}");
        let body = quote.body().expect("an expression quotation has a block");
        let for_stmt = body
            .syntax()
            .descendants()
            .find_map(ForStmt::cast)
            .expect("the loop is a statement of the quotation");
        assert!(for_stmt.attr_list().is_some(), "{source}");
    }
}

#[test]
fn an_attribute_before_an_item_keeps_a_quotation_an_item_quotation() {
    for source in [
        "const fn p() { quote {\n #[inline]\n #[other(1, 2)]\n fn f() {}\n } }",
        "const fn p() { quote {\n #[inline]\n fn f() {}\n } }",
        "const fn p() { quote {\n #[a]\n #[b = \"]\"]\n const N: u256 = 1\n } }",
    ] {
        let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
        assert!(errors.is_empty(), "{source}: {errors:?}");
        let root = SyntaxNode::new_root(green);
        let quote = root.descendants().find_map(QuoteExpr::cast).unwrap();
        assert_eq!(
            quote.item_list().unwrap().into_iter().count(),
            1,
            "{source}"
        );
    }
}

#[test]
fn an_attribute_that_nothing_follows_is_an_error_and_not_a_panic() {
    for source in [
        "const fn p() { quote { #[x] } }",
        "const fn p() { quote { #[x]\n } }",
        "const fn p() { quote { # } }",
        "const fn p() { quote { #[x }",
    ] {
        let (_, errors) = parse_source_file(source, RecoveryMode::Recover);
        assert!(!errors.is_empty(), "{source}");
    }
}

#[test]
fn where_clause_of_a_quoted_derive_header_ends_at_the_line_end() {
    use fe_parser::{
        SyntaxKind,
        ast::{ImplTrait, WhereClauseOwner},
    };
    // A derive header has no block, so the next line starts a new member: a
    // splice, a shape member or an item. None of them is another predicate.
    for next in ["${more}", "shape Step = (own usize, T)", "fn other() {}"] {
        let source = format!(
            "const fn gen() {{ quote {{\n impl<T> Tr for S<T> with (Q) where T: Bound\n {next}\n }} }}"
        );
        let (green, errors) = parse_source_file(&source, RecoveryMode::Recover);
        assert!(errors.is_empty(), "{next}: {errors:?}");
        let root = SyntaxNode::new_root(green);
        let header = root.descendants().find_map(ImplTrait::cast).unwrap();
        assert_eq!(
            header.where_clause().unwrap().into_iter().count(),
            1,
            "{next}"
        );
        let quote = root.descendants().find_map(QuoteExpr::cast).unwrap();
        let members = quote.item_list().unwrap().syntax().children().count();
        assert_eq!(members, 2, "{next}: {:#?}", quote.syntax());
        assert!(
            root.descendants()
                .all(|node| node.kind() != SyntaxKind::Error),
            "{next}"
        );
    }
}

#[test]
fn an_attributed_or_documented_shape_member_parses_like_a_bare_one() {
    for lead in ["/// documented", "#[inline]", "/// documented\n #[inline]"] {
        let source = format!(
            "const fn provider() {{ quote {{\n {lead}\n shape Step = (own usize, u8)\n }} }}"
        );
        let (green, errors) = parse_source_file(&source, RecoveryMode::Recover);
        assert!(errors.is_empty(), "{lead}: {errors:?}");
        let root = SyntaxNode::new_root(green);
        let shapes: Vec<_> = root
            .descendants()
            .filter_map(TraitShapeItem::cast)
            .collect();
        assert_eq!(shapes.len(), 1, "{lead}");
        assert!(shapes[0].attr_list().is_some(), "{lead}");
    }
}
