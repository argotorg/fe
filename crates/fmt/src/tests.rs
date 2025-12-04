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
    println!("Input:  {}", source);
    println!("Output: {}", result);
    assert_eq!(result, "fn takes_array(arr: Array<i32, 10>) {}");
}

#[test]
fn debug_generics_fixture() {
    let source = std::fs::read_to_string("tests/fixtures/generics.fe").unwrap();
    let config = crate::Config::default();
    let result = crate::format_str(&source, &config).unwrap();
    println!("=== FORMATTED OUTPUT ===");
    println!("{}", result);
    println!("=== END OUTPUT ===");
}

#[test]
fn debug_takes_array_with_comment() {
    // Test with just the comment and function
    let source = "// Type arguments with spacing\nfn takes_array(arr:Array<i32,10>) {}";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== WITH COMMENT ===");
    println!("{}", result);
    println!("===================");
}

#[test]
fn debug_takes_array_first_two() {
    // Test with first two items from the fixture
    let source = "// Generic type argument formatting tests\n\n// Type arguments with spacing\nfn takes_array(arr:Array<i32,10>) {}";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== FIRST TWO ITEMS ===");
    println!("{}", result);
    println!("=======================");
}

#[test]
fn debug_blank_line_before() {
    // Test with just a blank line before
    let source = "\nfn takes_array(arr:Array<i32,10>) {}";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== BLANK LINE BEFORE ===");
    println!("{}", result);
    println!("=========================");
}

#[test]
fn debug_comment_blank_before() {
    // Test with comment + blank line before
    let source = "// test\n\nfn takes_array(arr:Array<i32,10>) {}";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== COMMENT + BLANK BEFORE ===");
    println!("{}", result);
    println!("==============================");
}

#[test]
fn debug_two_comments_blank() {
    // Test with two comments + blank line + function
    let source = "// first\n\n// second\nfn takes_array(arr:Array<i32,10>) {}";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== TWO COMMENTS + BLANK ===");
    println!("{}", result);
    println!("============================");
}

#[test]
fn debug_exact_fixture_head() {
    // Exact first part from fixture (note: no newline after function - just like in file)
    let source = "// Generic type argument formatting tests\n\n// Type arguments with spacing\nfn takes_array(arr:Array<i32,10>) {}\n";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== EXACT FIXTURE HEAD ===");
    println!("{}", result);
    println!("==========================");
}

#[test]
fn debug_trailing_newline_effect() {
    // With trailing newline after {}
    let source1 = "fn takes_array(arr:Array<i32,10>) {}\n";
    let config = crate::Config::default();
    let result1 = crate::format_str(source1, &config).unwrap();
    println!("=== WITH TRAILING NEWLINE ===");
    println!("{}", result1);
    println!("=============================");

    // Without trailing newline after {}
    let source2 = "fn takes_array(arr:Array<i32,10>) {}";
    let result2 = crate::format_str(source2, &config).unwrap();
    println!("=== WITHOUT TRAILING NEWLINE ===");
    println!("{}", result2);
    println!("================================");
}

#[test]
fn debug_blank_after_function() {
    // Function followed by blank line (like between functions in the file)
    let source = "fn takes_array(arr:Array<i32,10>) {}\n\n";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== BLANK AFTER FUNCTION ===");
    println!("{}", result);
    println!("============================");
}

#[test]
fn debug_blank_before_and_after() {
    // Comment + blank before + function + blank after
    let source = "// test\n\nfn takes_array(arr:Array<i32,10>) {}\n\n";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== BLANK BEFORE AND AFTER ===");
    println!("{}", result);
    println!("==============================");
}

#[test]
fn debug_two_comments_with_blank_after() {
    // Two comments + blank line + function + blank after
    let source = "// first\n\n// second\nfn takes_array(arr:Array<i32,10>) {}\n\n";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== TWO COMMENTS + BLANK AFTER ===");
    println!("{}", result);
    println!("==================================");
}

#[test]
fn debug_long_comment_trigger() {
    // Exactly the first part of the fixture
    let source = "// Generic type argument formatting tests\n\n// Type arguments with spacing\nfn takes_array(arr:Array<i32,10>) {}\n\n";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== LONG COMMENT TRIGGER ===");
    println!("{}", result);
    println!("============================");
}

#[test]
fn debug_shorter_first_comment() {
    // Shorter first comment - 30 chars
    let source = "// Short comment about tests\n\n// Type arguments with spacing\nfn takes_array(arr:Array<i32,10>) {}\n\n";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== SHORTER FIRST COMMENT ===");
    println!("{}", result);
    println!("==============================");
}

#[test]
fn debug_tiny_comments() {
    // Tiny comments
    let source = "// a\n\n// b\nfn takes_array(arr:Array<i32,10>) {}\n\n";
    let config = crate::Config::default();
    let result = crate::format_str(source, &config).unwrap();
    println!("=== TINY COMMENTS ===");
    println!("{}", result);
    println!("=====================");
}
