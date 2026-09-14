use fe_parser::{RecoveryMode, SyntaxKind, SyntaxNode, parse_source_file};

#[test]
fn distinguishes_type_bounds_and_const_expressions_without_stealing_item_bodies() {
    for source in [
        "fn f<T>() where T: Copy, true {}",
        "fn f<T>() where T: Copy, true,\n {}",
        "struct S<T> where T: Copy, true,\n { value: T }",
        "fn f<T>() where true, T: Copy {}",
        "fn f() where check(3), (2 < 3) {}",
        "struct S where FLAG { value: u256 }",
        "enum E where FLAG { A, B }",
        "trait T { fn f() where true\nfn g() }",
        "fn f() where ({ let n: u256 = 3\n n > 0 }) {}",
    ] {
        let mut tooling_parser = tree_sitter::Parser::new();
        tooling_parser
            .set_language(&tree_sitter_fe::LANGUAGE.into())
            .unwrap();
        let tree = tooling_parser.parse(source, None).unwrap();
        assert!(
            !tree.root_node().has_error(),
            "tooling grammar rejected {source}: {}",
            tree.root_node().to_sexp()
        );
        let (green, errors) = parse_source_file(source, RecoveryMode::default());
        assert!(errors.is_empty(), "{source}: {errors:?}");
        let root = SyntaxNode::new_root(green);
        assert_eq!(root.to_string(), source);
        assert!(
            root.descendants()
                .any(|node| node.kind() == SyntaxKind::WhereConstPredicate)
        );
    }
}

#[test]
fn malformed_predicates_are_rejected() {
    for source in [
        "fn f() where {}",
        "fn f() where true false {}",
        "fn f() where check( {}",
        "fn f() where ({ true } {}",
    ] {
        let (_, errors) = parse_source_file(source, RecoveryMode::default());
        assert!(!errors.is_empty(), "accepted malformed header: {source}");
    }
}
