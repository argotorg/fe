//! HTML and script-context escaping utilities.

/// Escape for embedding in HTML attribute values and general code content.
///
/// Escapes: `& < > " '`
pub fn escape_html(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#x27;")
}

/// Escape for embedding in HTML element content (e.g., `<title>`).
///
/// Only escapes `& < >` — quotes are safe in element text.
pub fn escape_html_text(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

/// Escape content for safe embedding inside a `<script>` tag.
///
/// The only dangerous sequence is `</` which can close the script element.
/// We replace `</` with `<\/` which is valid in JS string literals and JSON.
pub fn escape_script_content(s: &str) -> String {
    s.replace("</", r"<\/")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escape_html_all_chars() {
        assert_eq!(
            escape_html("a<b>c&d\"e'f"),
            "a&lt;b&gt;c&amp;d&quot;e&#x27;f"
        );
    }

    #[test]
    fn escape_html_text_no_quotes() {
        assert_eq!(escape_html_text("a<b>c&d\"e'f"), "a&lt;b&gt;c&amp;d\"e'f");
    }

    #[test]
    fn escape_script_content_closes_tag() {
        assert_eq!(escape_script_content("</script>"), r"<\/script>");
        assert_eq!(escape_script_content("hello world"), "hello world");
    }
}
