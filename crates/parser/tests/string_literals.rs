use fe_parser::{
    RecoveryMode, SyntaxNode,
    ast::{Lit, LitKind, prelude::AstNode},
    parse_source_file,
};

#[test]
fn string_literals_decode_without_changing_source_spelling() {
    let source = r#"fn literals() {
    let escaped = "\"\\\n\r\t"
    let literal_backslash = "\\n"
    let unicode = "é🦀\n"
    let multiline = "first
second"
    let empty = ""
}"#;
    let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
    assert!(errors.is_empty(), "{errors:?}");
    let root = SyntaxNode::new_root(green);
    assert_eq!(root.to_string(), source);
    let values: Vec<_> = root
        .descendants()
        .filter_map(Lit::cast)
        .filter_map(|lit| match lit.kind() {
            LitKind::String(string) => Some(string.value().unwrap()),
            _ => None,
        })
        .collect();
    assert_eq!(values, ["\"\\\n\r\t", "\\n", "é🦀\n", "first\nsecond", ""]);
}

#[test]
fn invalid_escapes_report_byte_spans_and_preserve_following_literals() {
    for recovery in [RecoveryMode::Recover, RecoveryMode::NoRecover] {
        for escape in [r"\q", r"\x", r"\u", r"\0", r"\é"] {
            let source =
                format!("fn literals() {{ let bad =   \"é{escape}\"\nlet good = \"ok\" }}");
            let (green, errors) = parse_source_file(&source, recovery);
            assert_eq!(errors.len(), 1, "{errors:?}");
            assert_eq!(errors[0].msg(), "invalid string escape");
            let range = errors[0].range();
            assert_eq!(
                &source[usize::from(range.start())..usize::from(range.end())],
                escape
            );
            let root = SyntaxNode::new_root(green);
            assert_eq!(root.to_string(), source);
            let values: Vec<_> = root
                .descendants()
                .filter_map(Lit::cast)
                .filter_map(|lit| match lit.kind() {
                    LitKind::String(string) => Some(string.value()),
                    _ => None,
                })
                .collect();
            assert_eq!(values.len(), 2);
            assert!(values[0].is_err());
            assert_eq!(values[1].as_deref(), Ok("ok"));
        }
    }
}

#[test]
fn incomplete_string_tokens_are_diagnosed_without_panicking() {
    for source in [
        "fn f() { let s = \"",
        "fn f() { let s = \"abc\\",
        "fn f() { let s = \"abc\\\"",
    ] {
        for recovery in [RecoveryMode::Recover, RecoveryMode::NoRecover] {
            let (_, errors) = parse_source_file(source, recovery);
            assert!(!errors.is_empty(), "{source}");
        }
    }
}
