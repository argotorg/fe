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
