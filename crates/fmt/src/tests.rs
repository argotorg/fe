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
    let input = r#"// Leading comment

pub fn main() {
    // Inner comment
    let x = 1
}
"#;
    let output = format_str(input, &config).expect("formatting should succeed");
    assert_eq!(output, input);
}
