use fe_parser::{
    RecoveryMode, SyntaxNode,
    ast::{AttrArg, prelude::AstNode},
    parse_source_file,
};

/// A bare argument that is a path keeps the path as its key, including the
/// path-root keywords `self`, `super` and `ingot`.
#[test]
fn path_root_keywords_start_an_attribute_argument_key() {
    for path in [
        "make",
        "author::make",
        "super::make",
        "self::make",
        "ingot::a::make",
        "super::super::make",
    ] {
        let source = format!("mod nested {{\n    #[marker({path})]\n    struct Inner {{}}\n}}\n");
        let (green, errors) = parse_source_file(&source, RecoveryMode::Recover);
        assert!(errors.is_empty(), "{path}: {errors:?}");
        let root = SyntaxNode::new_root(green);
        assert_eq!(root.to_string(), source);
        let args: Vec<_> = root.descendants().filter_map(AttrArg::cast).collect();
        assert_eq!(args.len(), 1, "{path}");
        assert_eq!(args[0].key().unwrap().syntax().to_string(), path);
        assert!(args[0].value().is_none(), "{path}");
    }
}

/// An argument that is an expression stays one.
#[test]
fn an_expression_argument_has_no_key() {
    let source = "#[slot(BASE + 1)]\nstruct Inner {}\n";
    let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
    assert!(errors.is_empty(), "{errors:?}");
    let root = SyntaxNode::new_root(green);
    let args: Vec<_> = root.descendants().filter_map(AttrArg::cast).collect();
    assert_eq!(args.len(), 1);
    assert!(args[0].key().is_none());
    assert!(args[0].value().is_some());
}
