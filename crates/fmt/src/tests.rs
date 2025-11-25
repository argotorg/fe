use crate::{Config, format_str};

#[test]
fn identity_formatting_for_valid_input() {
    let config = Config::default();
    let input = "pub fn main() {}\n";
    let output = format_str(input, &config).expect("formatting should succeed");
    assert_eq!(output, input);
}

#[test]
fn identity_formatting_preserves_comments_and_whitespace() {
    let config = Config::default();
    let input = r"// Leading comment

pub fn main() {
    // Inner comment
    let x = 1
}
";
    let output = format_str(input, &config).expect("formatting should succeed");
    assert_eq!(output, input);
}

#[test]
fn format_list_vertical_split() {
    let config = Config {
        max_width: 15, // Params len is 18. Should split.
        indent_width: 4,
    };
    let input = "fn foo(a: u256, b: u256) {}";
    let output = format_str(input, &config).expect("formatting should succeed");

    let expected = r#"fn foo(
    a: u256,
    b: u256,
) {}"#;
    assert_eq!(output, expected);
}
