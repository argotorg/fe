use fe_parser::{
    RecoveryMode, SyntaxNode,
    ast::{ImplTrait, prelude::AstNode},
    parse_source_file,
};

#[test]
fn selection_alias_is_on_the_trait_impl_and_precedes_its_requirements() {
    let source =
        "impl<T> Produce for Wrapper<T> as Alt where T: Bound {}\nimpl Produce for Subject {}";
    let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
    assert!(errors.is_empty(), "{errors:?}");
    let root = SyntaxNode::new_root(green);
    assert_eq!(root.to_string(), source);
    let items: Vec<_> = root.descendants().filter_map(ImplTrait::cast).collect();
    assert_eq!(items.len(), 2);
    assert_eq!(items[0].selection_name().unwrap().text(), "Alt");
    assert!(items[1].selection_name().is_none());
}

#[test]
fn malformed_selection_alias_reports_its_specific_cause() {
    for (source, cause) in [
        ("impl Subject as Alt {}", "only trait impls can be named"),
        (
            "impl Produce for Subject as {}",
            "expected a name for the impl after `as`",
        ),
        (
            "impl Produce for Subject as outer::Alt {}",
            "an impl name is a single identifier",
        ),
        (
            "impl Produce for Subject as Alt<u64> {}",
            "an impl name is a single identifier",
        ),
    ] {
        let (_, errors) = parse_source_file(source, RecoveryMode::Recover);
        assert!(
            errors.iter().any(|error| error.msg() == cause),
            "{source}: {errors:?}"
        );
    }
}
