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

/// Formats `source` twice and asserts the canonical form and idempotence —
/// the roundtrip discipline for new syntax.
#[cfg(test)]
fn assert_roundtrip(source: &str, expected: &str) {
    let config = crate::Config::default();
    let once = crate::format_str(source, &config).unwrap();
    assert_eq!(once, expected, "first format pass");
    let twice = crate::format_str(&once, &config).unwrap();
    assert_eq!(twice, expected, "format must be idempotent");
}

#[test]
fn test_quote_expr_roundtrips() {
    assert_roundtrip(
        "fn f() {\n    let q = quote   {   true   }\n}\n",
        "fn f() {\n    let q = quote { true }\n}\n",
    );
}

#[test]
fn test_quote_open_names_and_holes_roundtrip() {
    assert_roundtrip(
        "fn f() {\n    body = quote( other ) { ${ body } && self.${ field } == other.${ field } }\n}\n",
        "fn f() {\n    body = quote(other) { ${body} && self.${field} == other.${field} }\n}\n",
    );
}

#[test]
fn test_quote_hole_method_call_chain_roundtrips() {
    assert_roundtrip(
        "fn f() {\n    body = quote(limit) { ${body} && self.${ field }.lte( limit.${field} ) }\n}\n",
        "fn f() {\n    body = quote(limit) { ${body} && self.${field}.lte(limit.${field}) }\n}\n",
    );
}

#[test]
fn test_quote_multiline_body_roundtrips() {
    let source = "fn f() {\n    let q = quote {\n        true\n    }\n}\n";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    let again = crate::format_str(&result, &config).unwrap();
    assert_eq!(result, again, "format must be idempotent, got:\n{result}");
    assert!(
        result.contains("quote {"),
        "quote keyword/body shape lost! got:\n{result}"
    );
}

#[test]
fn test_quote_arms_template_roundtrips() {
    // The arm-fold line: a splice of the arms built so far plus one
    // `${variant}(group) => body` arm; binder groups carry no singleton
    // trailing comma.
    assert_roundtrip(
        "fn f() {\n    arms = quote {   ${arms} ,  ${variant}( lhs ) => ${inner}   }\n}\n",
        "fn f() {\n    arms = quote { ${arms}, ${variant}(lhs) => ${inner} }\n}\n",
    );
}

#[test]
fn test_quote_match_with_pattern_holes_roundtrips() {
    assert_roundtrip(
        "fn f() {\n    inner = quote( other ) { match other { ${variant}(rhs) => ${cmp}, _ => false } }\n}\n",
        "fn f() {\n    inner = quote(other) {\n        match other {\n            ${variant}(rhs) => ${cmp},\n            _ => false,\n        }\n    }\n}\n",
    );
}

#[test]
fn test_quote_match_arm_splice_roundtrips() {
    assert_roundtrip(
        "fn f() {\n    body = quote { match self { ${arms} } }\n}\n",
        "fn f() {\n    body = quote {\n        match self {\n            ${arms},\n        }\n    }\n}\n",
    );
}

#[test]
fn test_empty_quote_roundtrips() {
    // The empty quote is the seed of arm folds.
    assert_roundtrip(
        "fn f() {\n    let seed = quote {   }\n}\n",
        "fn f() {\n    let seed = quote {}\n}\n",
    );
}
