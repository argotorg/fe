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
fn ordinary_trait_bounds_do_not_introduce_public_constraint_kinds() {
    for valid in [
        "trait Constraint {}\ntrait Foo<G: Constraint> {}\n",
        "trait Foo<G: *> {}\n",
        "trait Foo<G: * -> *> {}\n",
    ] {
        let (_, errors) = fe_parser::parse_source_file(valid, RecoveryMode::NoRecover);
        assert!(
            errors.is_empty(),
            "ordinary bound `{valid}` must parse: {errors:?}"
        );
    }
    for invalid in [
        "trait Foo<G: * -> Constraint> {}\n",
        "trait Foo<G: * -> * -> Constraint> {}\n",
        "trait Foo<G: (* -> *) -> Constraint> {}\n",
        "trait Foo<G: Constraint -> *> {}\n",
        "trait Foo<G: (Constraint)> {}\n",
        "trait Foo<G: (Constraint) -> Constraint> {}\n",
        "trait Foo<G: * -> Constraint -> *> {}\n",
    ] {
        let (_, errors) = fe_parser::parse_source_file(invalid, RecoveryMode::NoRecover);
        assert!(
            !errors.is_empty(),
            "public Constraint kind remains unsupported: `{invalid}`"
        );
    }
}
