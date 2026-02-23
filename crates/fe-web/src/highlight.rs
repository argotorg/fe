//! Tree-sitter syntax highlighting for Fe code.

use tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter, HtmlRenderer};

/// Capture names from highlights.scm, in the order we assign CSS classes.
/// The index into this array becomes the `Highlight` id.
const HIGHLIGHT_NAMES: &[&str] = &[
    "attribute",
    "comment",
    "comment.doc",
    "constant",
    "function",
    "function.definition",
    "function.method",
    "keyword",
    "number",
    "operator",
    "property",
    "punctuation.bracket",
    "punctuation.delimiter",
    "punctuation.special",
    "string",
    "string.escape",
    "type",
    "type.builtin",
    "type.enum.variant",
    "type.interface",
    "variable.parameter",
];

/// Precomputed HTML attributes: `b" class=\"hl-keyword\""` etc.
/// Dots in capture names become hyphens in CSS classes.
fn build_attrs() -> Vec<Vec<u8>> {
    HIGHLIGHT_NAMES
        .iter()
        .map(|name| {
            let class = name.replace('.', "-");
            format!(" class=\"hl-{class}\"").into_bytes()
        })
        .collect()
}

fn make_config() -> HighlightConfiguration {
    let language = tree_sitter_fe::LANGUAGE.into();
    let mut config = HighlightConfiguration::new(
        language,
        "fe",
        tree_sitter_fe::HIGHLIGHTS_QUERY,
        "", // no injections
        "", // no locals
    )
    .expect("highlights.scm should parse");
    config.configure(HIGHLIGHT_NAMES);
    config
}

/// Highlight Fe source code, returning inner HTML with `<span class="hl-*">` spans.
///
/// Falls back to html-escaped plain text if highlighting fails.
pub fn highlight_fe(code: &str) -> String {
    let config = make_config();
    let attrs = build_attrs();

    let mut highlighter = Highlighter::new();
    let highlights = match highlighter.highlight(&config, code.as_bytes(), None, |_| None) {
        Ok(h) => h,
        Err(_) => return html_escape(code),
    };

    let mut renderer = HtmlRenderer::new();
    match renderer.render(highlights, code.as_bytes(), &|h| &attrs[h.0]) {
        Ok(()) => {}
        Err(_) => return html_escape(code),
    }

    // HtmlRenderer stores output as bytes
    String::from_utf8_lossy(&renderer.html).into_owned()
}

/// Highlight Fe code and wrap in a `<pre><code>` block.
pub fn highlight_fe_block(code: &str) -> String {
    let inner = highlight_fe(code);
    format!("<pre><code class=\"language-fe\">{inner}</code></pre>")
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#x27;")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn highlights_keyword() {
        let html = highlight_fe("fn main() {}");
        assert!(html.contains("hl-keyword"), "should highlight `fn`: {html}");
    }

    #[test]
    fn highlights_function_definition() {
        let html = highlight_fe("fn main() {}");
        assert!(
            html.contains("hl-function-definition"),
            "should highlight function name: {html}"
        );
    }

    #[test]
    fn highlights_type() {
        let html = highlight_fe("struct Foo {}");
        assert!(html.contains("hl-type"), "should highlight type: {html}");
    }

    #[test]
    fn highlights_string() {
        let html = highlight_fe(r#"let x = "hello""#);
        assert!(
            html.contains("hl-string"),
            "should highlight string: {html}"
        );
    }

    #[test]
    fn handles_empty_input() {
        let html = highlight_fe("");
        assert!(html.is_empty() || !html.contains("hl-"), "empty input: {html}");
    }

    #[test]
    fn block_wrapper() {
        let html = highlight_fe_block("fn f() {}");
        assert!(html.starts_with("<pre><code class=\"language-fe\">"));
        assert!(html.ends_with("</code></pre>"));
    }

    #[test]
    fn graceful_on_partial_syntax() {
        // Incomplete code should still produce output (not panic)
        let html = highlight_fe("fn incomplete(");
        assert!(!html.is_empty());
    }
}
