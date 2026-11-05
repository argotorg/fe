use fe_parser::{RecoveryMode, SyntaxKind, SyntaxNode, parse_source_file};

#[test]
fn bodyless_signatures_preserve_the_next_items_attributes() {
    for signature in [
        "fn first()",
        "const fn first<const N: usize>()",
        "fn first() -> u256",
        "fn first() uses Context",
        "fn first<T>() where T: Bound",
    ] {
        for owner in ["trait Example", "extern"] {
            for recovery in [RecoveryMode::Recover, RecoveryMode::NoRecover] {
                let source = format!(
                    "{owner} {{\n    {signature}\n\n    #[inline(always)]\n    fn second()\n}}\n"
                );
                let (green, errors) = parse_source_file(&source, recovery);
                assert!(errors.is_empty(), "{source}\n{errors:#?}");
                let root = SyntaxNode::new_root(green);
                assert_eq!(root.to_string(), source);
                let functions = root
                    .descendants()
                    .filter(|node| node.kind() == SyntaxKind::Func)
                    .collect::<Vec<_>>();
                assert_eq!(functions.len(), 2, "{source}");
                assert!(!functions[0].to_string().contains("inline"));
                assert!(functions[1].to_string().contains("#[inline(always)]"));
            }
        }
    }
}

#[test]
fn bodyless_trait_signatures_end_before_associated_items() {
    for signature in [
        "fn first()",
        "fn first() -> u256",
        "fn first() uses Context",
        "fn first<T>() where T: Bound",
    ] {
        for item in ["type Item", "const LEN: usize"] {
            for recovery in [RecoveryMode::Recover, RecoveryMode::NoRecover] {
                let source = format!("trait Example {{\n    {signature}\n    {item}\n}}\n");
                let (green, errors) = parse_source_file(&source, recovery);
                assert!(errors.is_empty(), "{source}\n{errors:#?}");
                assert_eq!(SyntaxNode::new_root(green).to_string(), source);
            }
        }
    }
}
