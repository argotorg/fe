//! Embedded assets for static documentation sites

/// The documentation site stylesheet.
pub const STYLES_CSS: &str = include_str!("../assets/styles.css");

/// The vanilla JS renderer (ports Leptos SSR components to client-side rendering).
pub const FE_WEB_JS: &str = include_str!("../assets/fe-web.js");

/// Generate the complete HTML shell for a static documentation site.
///
/// The `doc_index_json` is inlined into a `<script>` tag so the page works
/// with `file://` — no server required.
pub fn html_shell(title: &str, doc_index_json: &str) -> String {
    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{title}</title>
  <style>{css}</style>
</head>
<body>
  <script>window.FE_DOC_INDEX = {json};</script>
  <div class="doc-layout">
    <div id="sidebar"></div>
    <main id="content" class="doc-content"></main>
  </div>
  <script>{js}</script>
</body>
</html>"#,
        title = title,
        css = STYLES_CSS,
        json = doc_index_json,
        js = FE_WEB_JS,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn styles_css_is_nonempty() {
        assert!(!STYLES_CSS.is_empty());
        assert!(STYLES_CSS.contains(":root"));
    }

    #[test]
    fn fe_web_js_is_nonempty() {
        assert!(!FE_WEB_JS.is_empty());
        assert!(FE_WEB_JS.contains("renderDocItem"));
    }

    #[test]
    fn html_shell_produces_valid_output() {
        let json = r#"{"items":[],"modules":[]}"#;
        let html = html_shell("Test Docs", json);

        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("<title>Test Docs</title>"));
        assert!(html.contains(":root"));
        assert!(html.contains("renderDocItem"));
        assert!(html.contains(r#"window.FE_DOC_INDEX = {"items":[],"modules":[]}"#));
        assert!(html.contains(r#"<div id="sidebar">"#));
        assert!(html.contains(r#"<main id="content""#));
    }
}
