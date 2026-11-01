use fe_parser::{
    RecoveryMode, SyntaxNode,
    ast::{Lit, LitKind, prelude::AstNode},
    parse_source_file,
};

fn byte_values(root: &SyntaxNode) -> Vec<Result<u8, &'static str>> {
    root.descendants()
        .filter_map(Lit::cast)
        .filter_map(|lit| match lit.kind() {
            LitKind::Byte(byte) => Some(byte.value().map_err(|(msg, _)| msg)),
            _ => None,
        })
        .collect()
}

#[test]
fn byte_literals_decode_characters_and_escapes() {
    let source = r#"fn bytes() {
    let chars = [b'0', b'x', b'X', b' ', b'"', b'~']
    let escapes = [b'\n', b'\r', b'\t', b'\0', b'\\', b'\'', b'\"', b'\x7f', b'\xFF', b'\xaB']
    match chars[0] {
        b'0' => (),
        _ => (),
    }
}"#;
    let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
    assert!(errors.is_empty(), "{errors:?}");
    let root = SyntaxNode::new_root(green);
    assert_eq!(root.to_string(), source);
    let expected = [
        b'0', b'x', b'X', b' ', b'"', b'~', b'\n', b'\r', b'\t', 0, b'\\', b'\'', b'"', 0x7f, 0xff,
        0xab, b'0',
    ];
    assert_eq!(byte_values(&root), expected.map(Ok));
}

#[test]
fn invalid_byte_literals_report_their_spans() {
    for (literal, message, span) in [
        ("b''", "empty byte literal", "b''"),
        ("b'ab'", "byte literal holds more than one character", "ab"),
        (
            "b'é'",
            "non-ASCII character in byte literal; use a `\\x` escape",
            "é",
        ),
        (r"b'\q'", "invalid byte escape", r"\q"),
        (r"b'\é'", "invalid byte escape", r"\é"),
        (r"b'\x4'", "invalid byte escape", r"\x4"),
        (r"b'\xg0'", "invalid byte escape", r"\xg0"),
        (r"b'\x+f'", "invalid byte escape", r"\x+f"),
        (
            r"b'\x41b'",
            "byte literal holds more than one character",
            r"\x41b",
        ),
    ] {
        let source = format!("fn f() {{ let bad = {literal}\nlet good = b'k' }}");
        for recovery in [RecoveryMode::Recover, RecoveryMode::NoRecover] {
            let (green, errors) = parse_source_file(&source, recovery);
            assert_eq!(errors.len(), 1, "{source}: {errors:?}");
            assert_eq!(errors[0].msg(), message, "{source}");
            let range = errors[0].range();
            assert_eq!(
                &source[usize::from(range.start())..usize::from(range.end())],
                span,
                "{source}"
            );
            let root = SyntaxNode::new_root(green);
            assert_eq!(root.to_string(), source);
            assert_eq!(byte_values(&root), [Err(message), Ok(b'k')]);
        }
    }
}
