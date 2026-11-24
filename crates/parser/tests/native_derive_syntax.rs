use fe_parser::{
    RecoveryMode, SyntaxNode,
    ast::{self, prelude::*},
};

#[test]
fn derive_protocol_retains_its_static_const_method() {
    let source = include_str!("../../../ingots/core/src/derive.fe");
    let (green, errors) = fe_parser::parse_source_file(source, RecoveryMode::Recover);
    let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
    assert!(errors.is_empty(), "{errors:?}\n{:#?}", root.syntax());
    let definition = root
        .syntax()
        .descendants()
        .find_map(ast::Trait::cast)
        .unwrap();
    let methods = definition
        .item_list()
        .unwrap()
        .into_iter()
        .filter_map(|item| match item.kind() {
            ast::TraitItemKind::Func(function) => {
                function.sig().name().map(|name| name.text().to_string())
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(methods, ["derive"]);
}

#[test]
fn native_declared_and_named_multiline_headers_keep_provider_paths() {
    let source = "impl Score for Pair with (\n Fields\n)\nimpl Score for Pair as Zero with (mod_ :: Provider)\n";
    let (green, errors) = fe_parser::parse_source_file(source, RecoveryMode::Recover);
    let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
    assert!(errors.is_empty(), "{errors:?}");
    let headers = root
        .syntax()
        .descendants()
        .filter_map(ast::ImplTrait::cast)
        .collect::<Vec<_>>();
    assert_eq!(headers.len(), 2);
    assert!(headers[0].selection_name().is_none());
    assert_eq!(headers[1].selection_name().unwrap().text(), "Zero");
    assert!(
        headers
            .iter()
            .all(|header| header.derive_provider().is_some())
    );
}

#[test]
fn native_derive_missing_opening_parenthesis_reports_errors() {
    for source in [
        "impl Score for Pair with",
        "impl Score for Pair with Provider",
        "impl Score for Pair with Provider)",
        "impl Score for Pair with [Provider]",
    ] {
        for mode in [RecoveryMode::Recover, RecoveryMode::NoRecover] {
            let (_, errors) = fe_parser::parse_source_file(source, mode);
            assert!(!errors.is_empty(), "expected parse errors for {source:?}");
        }
    }
}

#[test]
fn native_derive_missing_closing_parenthesis_reports_errors() {
    for source in [
        "impl Score for Pair with (Provider",
        "impl Score for Pair with (mod_::Provider<u256>",
        "impl Score for Pair with (Provider]",
        "impl Score for Pair with (Provider, Other)",
    ] {
        for mode in [RecoveryMode::Recover, RecoveryMode::NoRecover] {
            let (_, errors) = fe_parser::parse_source_file(source, mode);
            assert!(!errors.is_empty(), "expected parse errors for {source:?}");
        }
    }
}

#[test]
fn native_derive_malformed_provider_clause_preserves_following_items() {
    for clause in [
        "with",
        "with Provider",
        "with Provider)",
        "with [Provider]",
        "with (",
        "with ()",
        "with (Provider",
        "with (Provider]",
        "with (mod_::)",
        "with (Provider, Other)",
    ] {
        let source = format!(
            "impl Score for Pair {clause}\nfn after() {{}}\nimpl Score for Pair as Valid with (mod_::Provider<u256>)\n"
        );
        let (green, errors) = fe_parser::parse_source_file(&source, RecoveryMode::Recover);
        let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
        assert!(!errors.is_empty(), "expected parse errors for {source:?}");
        assert_eq!(root.syntax().text().to_string(), source);
        let items = root.items().unwrap().into_iter().collect::<Vec<_>>();
        assert_eq!(items.len(), 3, "{source}\n{:#?}", root.syntax());
        let Some(ast::ItemKind::Func(function)) = items[1].kind() else {
            panic!(
                "following function was lost: {source}\n{:#?}",
                root.syntax()
            );
        };
        assert_eq!(function.sig().name().unwrap().text(), "after");
        assert!(function.body().is_some());
        let Some(ast::ItemKind::ImplTrait(header)) = items[2].kind() else {
            panic!("following impl was lost: {source}\n{:#?}", root.syntax());
        };
        assert_eq!(header.selection_name().unwrap().text(), "Valid");
        assert_eq!(
            header
                .derive_provider()
                .unwrap()
                .syntax()
                .text()
                .to_string(),
            "mod_::Provider<u256>"
        );
    }
}

#[test]
fn native_derive_missing_delimiters_preserve_enclosing_module() {
    for clause in ["with Provider", "with (Provider", "with ("] {
        let source = format!("mod inner {{\nimpl Score for Pair {clause}\n}}\nfn outside() {{}}\n");
        let (green, errors) = fe_parser::parse_source_file(&source, RecoveryMode::Recover);
        let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
        assert!(!errors.is_empty(), "expected parse errors for {source:?}");
        assert_eq!(root.syntax().text().to_string(), source);
        let items = root.items().unwrap().into_iter().collect::<Vec<_>>();
        assert_eq!(items.len(), 2, "{source}\n{:#?}", root.syntax());
        let Some(ast::ItemKind::Mod(module)) = items[0].kind() else {
            panic!("enclosing module was lost: {source}\n{:#?}", root.syntax());
        };
        assert_eq!(module.items().unwrap().into_iter().count(), 1);
        let Some(ast::ItemKind::Func(function)) = items[1].kind() else {
            panic!(
                "following function was lost: {source}\n{:#?}",
                root.syntax()
            );
        };
        assert_eq!(function.sig().name().unwrap().text(), "outside");
    }
}

#[test]
fn native_derive_valid_provider_paths_parse_without_recovery() {
    for source in [
        "impl Score for Pair with (Provider)\n",
        "pub impl Score for Pair as Chosen with (mod_::Provider<u256>)\n",
        "impl Score for Pair with (\n // provider\n mod_::Provider<u256>\n)\n",
        "impl<T> Score for Box2<T> with (Within) where T: Ord\n",
        "pub impl<T> Score for Box2<T> as Ordered with (Within) where T: Ord\n",
    ] {
        let (green, errors) = fe_parser::parse_source_file(source, RecoveryMode::NoRecover);
        let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
        assert!(errors.is_empty(), "{source}\n{errors:?}");
        assert_eq!(root.syntax().text().to_string(), source);
        let header = root
            .syntax()
            .descendants()
            .find_map(ast::ImplTrait::cast)
            .unwrap();
        assert!(header.derive_provider().is_some());
        assert!(header.item_list().is_none());
    }
}

#[test]
fn inherent_headers_with_provider_parse_without_recovery() {
    for (source, expected_ty, expected_provider, has_generics, has_where) in [
        (
            "impl Packet with (InspectAbi)\n",
            "Packet",
            "InspectAbi",
            false,
            false,
        ),
        (
            "impl<T: Ord> Box2<T> with (Within)\n",
            "Box2<T>",
            "Within",
            true,
            false,
        ),
        (
            "impl<T> Box2<T> with (pkg::Within<T>) where T: Ord\n",
            "Box2<T>",
            "pkg::Within<T>",
            true,
            true,
        ),
        (
            "impl Packet with (\n // provider\n mod_::InspectAbi<u256>\n)\n",
            "Packet",
            "mod_::InspectAbi<u256>",
            false,
            false,
        ),
    ] {
        for mode in [RecoveryMode::Recover, RecoveryMode::NoRecover] {
            let (green, errors) = fe_parser::parse_source_file(source, mode);
            let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
            assert!(errors.is_empty(), "{source}\n{errors:?}");
            assert_eq!(root.syntax().text().to_string(), source);
            let header = root
                .syntax()
                .descendants()
                .find_map(ast::Impl::cast)
                .unwrap();
            assert_eq!(
                header.ty().unwrap().syntax().text().to_string(),
                expected_ty
            );
            assert_eq!(
                header
                    .derive_provider()
                    .unwrap()
                    .syntax()
                    .text()
                    .to_string(),
                expected_provider
            );
            assert_eq!(header.generic_params().is_some(), has_generics);
            assert_eq!(header.where_clause().is_some(), has_where);
            assert!(header.item_list().is_none());
        }
    }
}

#[test]
fn inherent_header_lookahead_does_not_consume_following_for_tokens() {
    let top_level = "\
impl Packet with (InspectAbi)
impl<T: Ord> Box2<T> with (Within)
impl Score for Packet {}
fn run() {
    for x in xs {}
}
";
    let (green, errors) = fe_parser::parse_source_file(top_level, RecoveryMode::Recover);
    let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
    assert!(errors.is_empty(), "{errors:?}\n{:#?}", root.syntax());
    let items = root.items().unwrap().into_iter().collect::<Vec<_>>();
    assert_eq!(items.len(), 4);
    assert!(matches!(items[0].kind(), Some(ast::ItemKind::Impl(_))));
    assert!(matches!(items[1].kind(), Some(ast::ItemKind::Impl(_))));
    assert!(matches!(items[2].kind(), Some(ast::ItemKind::ImplTrait(_))));
    assert!(matches!(items[3].kind(), Some(ast::ItemKind::Func(_))));

    let in_block_and_quote = "\
fn local() {
    impl Packet with (InspectAbi)
    for x in xs {}
}
const fn make() {
    quote {
        impl Packet with (InspectAbi)
        impl Score for Packet {}
    }
}
";
    let (green, errors) = fe_parser::parse_source_file(in_block_and_quote, RecoveryMode::Recover);
    let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
    assert!(errors.is_empty(), "{errors:?}\n{:#?}", root.syntax());
    let inherent = root
        .syntax()
        .descendants()
        .filter_map(ast::Impl::cast)
        .collect::<Vec<_>>();
    assert_eq!(inherent.len(), 2);
    assert!(inherent.iter().all(|item| item.derive_provider().is_some()));

    let broken_generic = "\
impl Packet<T, u))> with (InspectAbi)
impl Score for Packet {}
";
    let (green, errors) = fe_parser::parse_source_file(broken_generic, RecoveryMode::Recover);
    let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
    assert!(!errors.is_empty());
    let items = root.items().unwrap().into_iter().collect::<Vec<_>>();
    assert_eq!(items.len(), 2, "{:#?}", root.syntax());
    assert!(matches!(items[0].kind(), Some(ast::ItemKind::Impl(_))));
    assert!(matches!(items[1].kind(), Some(ast::ItemKind::ImplTrait(_))));
}

#[test]
fn inherent_derive_malformed_headers_report_errors_and_preserve_following_items() {
    for invalid in [
        "impl Packet with\n",
        "impl Packet with InspectAbi\n",
        "impl Packet with InspectAbi)\n",
        "impl Packet with [InspectAbi]\n",
        "impl Packet with (\n",
        "impl Packet with ()\n",
        "impl Packet with (InspectAbi\n",
        "impl Packet with (InspectAbi]\n",
        "impl Packet with (InspectAbi, Other)\n",
        "impl Packet with (InspectAbi) {}\n",
        "impl Packet with (InspectAbi) where T: Ord {}\n",
        "pub impl Packet with (InspectAbi)\n",
        "unsafe impl Packet with (InspectAbi)\n",
        "impl Packet as Named with (InspectAbi)\n",
    ] {
        for mode in [RecoveryMode::Recover, RecoveryMode::NoRecover] {
            let (_, errors) = fe_parser::parse_source_file(invalid, mode);
            assert!(!errors.is_empty(), "expected parse errors for {invalid:?}");
        }
    }

    for clause in [
        "with",
        "with InspectAbi",
        "with InspectAbi)",
        "with [InspectAbi]",
        "with (",
        "with ()",
        "with (InspectAbi",
        "with (InspectAbi]",
        "with (mod_::)",
        "with (InspectAbi, Other)",
    ] {
        let source =
            format!("impl Packet {clause}\nfn after() {{}}\nimpl<T: Ord> Box2<T> with (Within)\n");
        let (green, errors) = fe_parser::parse_source_file(&source, RecoveryMode::Recover);
        let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
        assert!(!errors.is_empty(), "expected parse errors for {source:?}");
        assert_eq!(root.syntax().text().to_string(), source);
        let items = root.items().unwrap().into_iter().collect::<Vec<_>>();
        assert_eq!(items.len(), 3, "{source}\n{:#?}", root.syntax());
        let Some(ast::ItemKind::Func(function)) = items[1].kind() else {
            panic!(
                "following function was lost: {source}\n{:#?}",
                root.syntax()
            );
        };
        assert_eq!(function.sig().name().unwrap().text(), "after");
        let Some(ast::ItemKind::Impl(header)) = items[2].kind() else {
            panic!(
                "following inherent header was lost: {source}\n{:#?}",
                root.syntax()
            );
        };
        assert_eq!(
            header
                .derive_provider()
                .unwrap()
                .syntax()
                .text()
                .to_string(),
            "Within"
        );
    }
}

#[test]
fn constraint_kind_and_trait_bound_parse_distinctly() {
    let source = "
trait Constraint {}
trait Foo<T: Constraint, G: (Constraint), P: * -> Constraint> {}
";
    let (green, errors) = fe_parser::parse_source_file(source, RecoveryMode::NoRecover);
    let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
    assert!(errors.is_empty(), "{errors:?}\n{:#?}", root.syntax());
    let mut bounds = root.syntax().descendants().filter_map(ast::TypeBound::cast);
    let ordinary = bounds.next().unwrap();
    let goal = bounds.next().unwrap();
    let family = bounds.next().unwrap();
    assert!(bounds.next().is_none());

    assert!(ordinary.trait_bound().is_some());
    assert!(goal.kind_bound().unwrap().mono().unwrap().is_constraint());

    let family = family.kind_bound().unwrap().abs().unwrap();
    assert!(!family.lhs().unwrap().mono().unwrap().is_constraint());
    assert!(family.rhs().unwrap().mono().unwrap().is_constraint());
}

#[test]
fn where_clause_of_a_written_derive_header_keeps_the_next_trait_item_out() {
    // The header has no body, so the next line starts a new item. The trait
    // must not be read as another predicate of the clause.
    for source in [
        "impl Eq for Foo with (Derive) where T: Eq\ntrait Next {}",
        "impl Eq for Foo with (Derive) where T: Eq,\ntrait Next: Parent {}",
    ] {
        let (green, errors) = fe_parser::parse_source_file(source, RecoveryMode::Recover);
        assert!(errors.is_empty(), "{source}\n{errors:?}");
        let root = ast::Root::cast(SyntaxNode::new_root(green)).unwrap();
        let header = root
            .syntax()
            .descendants()
            .find_map(ast::ImplTrait::cast)
            .unwrap();
        assert!(header.derive_provider().is_some(), "{source}");
        let predicates = header.where_clause().unwrap().into_iter().count();
        assert_eq!(predicates, 1, "{source}");
        assert_eq!(
            root.syntax()
                .descendants()
                .filter(|node| node.kind() == fe_parser::SyntaxKind::Trait)
                .count(),
            1,
            "{source}"
        );
    }
}
