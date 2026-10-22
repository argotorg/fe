// Formatting tests are in tests/fixtures/*.fe with snapshot tests.
// See tests/format_snapshots.rs for the test harness.

#[test]
fn test_pretty_group_behavior() {
    use pretty::RcDoc;

    // Simulate: struct Point { x: i32, y: i32 }
    let sep: RcDoc<()> = RcDoc::text(",").append(RcDoc::line());
    let inner: RcDoc<()> = RcDoc::text("x: i32")
        .append(sep)
        .append(RcDoc::text("y: i32"));

    let fields: RcDoc<()> = RcDoc::text("{")
        .append(RcDoc::line().append(inner).nest(4))
        .append(RcDoc::line())
        .append(RcDoc::text("}"))
        .group();

    let doc: RcDoc<()> = RcDoc::text("struct Point ").append(fields);

    let mut output = Vec::new();
    doc.render(100, &mut output).unwrap();
    let result = String::from_utf8(output).unwrap();
    assert_eq!(result, "struct Point { x: i32, y: i32 }");
}

#[test]
fn test_struct_one_line() {
    let source = "struct Point{x:i32,y:i32}";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    assert_eq!(result, "struct Point { x: i32, y: i32 }");
}

#[test]
fn test_struct_with_comments_and_blank_lines() {
    let source = "// before

struct Point{x:i32,y:i32}

// after
";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    assert!(result.contains("struct Point { x: i32, y: i32 }"));
    // Should preserve blank lines
    assert!(result.contains("\n\nstruct Point"));
    assert!(result.contains("}\n\n// after"));
}

#[test]
fn test_takes_array_single_param() {
    let source = "fn takes_array(arr:Array<i32,10>) {}";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    assert_eq!(result, "fn takes_array(arr: Array<i32, 10>) {}");
}

#[test]
fn test_with_shorthand_preserves_content() {
    // Regression: `with (expr)` shorthand (no `Key = value`) was silently
    // deleted because WithParam::to_doc only looked for a Path child.
    let source = "fn test() {\n    with (counter) {\n        bump_twice()\n    }\n}\n";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    assert!(
        result.contains("counter"),
        "formatter deleted with-param content! got:\n{result}"
    );
}
#[test]
fn quotation_splice_documents_format_values_without_losing_categories() {
    use crate::ast::ToDoc;
    use parser::{RecoveryMode, SyntaxNode, ast, ast::prelude::AstNode, parse_source_file};

    let source = r#"const fn make() {
    quote {
        impl Eq for ${type_for(1,2)} where ${bounds_for(1,2)} {
            fn eq(self) -> bool { match self { ${pattern_for(1,2)} => true } }
        }
    }
}"#;
    let (green, errors) = parse_source_file(source, RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:#?}");
    let root = SyntaxNode::new_root(green);
    let config = crate::Config::default();
    let ctx = crate::RewriteContext {
        source,
        config: &config,
        alloc: pretty::RcAllocator,
    };
    let render = |doc: crate::ast::types::Doc<'_>| {
        let mut output = Vec::new();
        doc.render(100, &mut output).unwrap();
        String::from_utf8(output).unwrap()
    };
    let ty = root
        .descendants()
        .filter_map(ast::Type::cast)
        .find(|ty| matches!(ty.kind(), ast::TypeKind::QuoteHole(_)))
        .expect("type splice");
    assert_eq!(render(ty.to_doc(&ctx)), "${type_for(1, 2)}");
    let pat = root
        .descendants()
        .filter_map(ast::Pat::cast)
        .find(|pat| matches!(pat.kind(), ast::PatKind::QuoteHole(_)))
        .expect("pattern splice");
    assert_eq!(render(pat.to_doc(&ctx)), "${pattern_for(1, 2)}");
    let clause = root
        .descendants()
        .find_map(ast::WhereClause::cast)
        .expect("where clause with predicate splice");
    assert_eq!(render(clause.to_doc(&ctx)), "where ${bounds_for(1, 2)}");
}

#[test]
fn quotation_open_binders_and_splice_comments_survive_formatting() {
    let source = r#"const fn make() { quote(other) {
    impl Eq for $// type
{record} where ${bounds} {
        fn eq(self) -> bool { match self { ${pattern} => other.${field} == ${value} } }
    }
} }"#;
    let config = crate::Config::default();
    let formatted = crate::format_str(source, &config).expect("format valid quotation");
    for fragment in [
        "quote(other)",
        "$// type\n{record}",
        "where ${bounds}",
        "${pattern} => other.${field} == ${value}",
    ] {
        assert!(formatted.contains(fragment), "{formatted}");
    }
    let (_, errors) = parser::parse_source_file(&formatted, parser::RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:#?}\n{formatted}");
    assert_eq!(
        crate::format_str(&formatted, &config).expect("format again"),
        formatted
    );
}

#[test]
fn named_impl_alias_survives_formatting_and_reparsing() {
    use parser::{
        RecoveryMode, SyntaxNode,
        ast::{ImplTrait, prelude::AstNode},
        parse_source_file,
    };
    let source = "impl<T> Produce for Wrapper<T> as Alt where T: Bound{type Output=u64}";
    let config = crate::Config::default();
    let formatted = crate::format_str(source, &config).unwrap();
    let (green, errors) = parse_source_file(&formatted, RecoveryMode::Recover);
    assert!(errors.is_empty(), "{errors:?}");
    let root = SyntaxNode::new_root(green);
    let item = root.descendants().find_map(ImplTrait::cast).unwrap();
    assert_eq!(item.selection_name().unwrap().text(), "Alt");
    assert_eq!(crate::format_str(&formatted, &config).unwrap(), formatted);
}
