//! Embedded assets for static documentation sites

use crate::escape::{escape_html_text, escape_script_content};

/// The documentation site stylesheet.
pub const STYLES_CSS: &str = include_str!("../assets/styles.css");

/// The vanilla JS renderer (ports Leptos SSR components to client-side rendering).
pub const FE_WEB_JS: &str = include_str!("../assets/fe-web.js");

/// `<fe-code-block>` custom element.
pub const FE_CODE_BLOCK_JS: &str = include_str!("../assets/fe-code-block.js");

/// `<fe-signature>` custom element.
pub const FE_SIGNATURE_JS: &str = include_str!("../assets/fe-signature.js");

/// `<fe-search>` custom element.
pub const FE_SEARCH_JS: &str = include_str!("../assets/fe-search.js");

/// Standalone syntax highlighting CSS (hardcoded colors, no CSS variables).
/// For embedding in Starlight/Astro or any external site.
pub const FE_HIGHLIGHT_CSS: &str = include_str!("../assets/fe-highlight.css");

/// Generate the complete HTML shell for a static documentation site.
///
/// The `doc_index_json` is inlined into a `<script>` tag so the page works
/// with `file://` — no server required.
pub fn html_shell(title: &str, doc_index_json: &str) -> String {
    html_shell_with_scip(title, doc_index_json, None)
}

/// Generate the HTML shell with optional embedded SCIP data.
///
/// When `scip_bytes` is provided, the SCIP protobuf is base64-encoded and
/// embedded in a `<script>` tag. A WASM initialization snippet decodes
/// it and creates a `ScipStore` for interactive symbol resolution.
pub fn html_shell_with_scip(
    title: &str,
    doc_index_json: &str,
    scip_bytes: Option<&[u8]>,
) -> String {
    // Escape for safe embedding inside HTML/script contexts:
    // - Title: escape HTML special chars to prevent </title> breakout
    // - JSON: escape </ sequences to prevent </script> breakout
    let safe_title = escape_html_text(title);
    let safe_json = escape_script_content(doc_index_json);

    let scip_section = if let Some(bytes) = scip_bytes {
        use crate::escape::base64_encode;
        let b64 = base64_encode(bytes);
        format!(
            r#"
  <script id="scip-data" type="application/octet-stream">{b64}</script>
  <script type="module">
    // Progressive enhancement: decode SCIP data and create ScipStore
    // when the WASM module is available.
    (function() {{
      var el = document.getElementById('scip-data');
      if (!el) return;
      var b64 = el.textContent;
      var raw = atob(b64);
      var bytes = new Uint8Array(raw.length);
      for (var i = 0; i < raw.length; i++) bytes[i] = raw.charCodeAt(i);
      window.FE_SCIP_BYTES = bytes;
      // If WASM is loaded alongside (via separate script), it will pick up FE_SCIP_BYTES
      // and set window.FE_SCIP = new ScipStore(bytes)
    }})();
  </script>"#
        )
    } else {
        String::new()
    };

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
  <script>window.FE_DOC_INDEX = {json};</script>{scip_section}
  <script>{code_block_js}</script>
  <script>{signature_js}</script>
  <script>{search_js}</script>
  <div class="doc-layout">
    <div id="sidebar"></div>
    <main id="content" class="doc-content"></main>
  </div>
  <script>{js}</script>
</body>
</html>"#,
        title = safe_title,
        css = STYLES_CSS,
        json = safe_json,
        scip_section = scip_section,
        code_block_js = FE_CODE_BLOCK_JS,
        signature_js = FE_SIGNATURE_JS,
        search_js = FE_SEARCH_JS,
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
    fn custom_element_js_nonempty() {
        assert!(FE_CODE_BLOCK_JS.contains("fe-code-block"));
        assert!(FE_SIGNATURE_JS.contains("fe-signature"));
        assert!(FE_SEARCH_JS.contains("fe-search"));
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
        // Custom elements are loaded before the main app JS
        assert!(html.contains("fe-code-block"));
        assert!(html.contains("fe-signature"));
        assert!(html.contains("fe-search"));
    }

    #[test]
    fn html_shell_escapes_script_injection_in_json() {
        let malicious_json = r#"{"x":"</script><script>alert(1)</script>"}"#;
        let html = html_shell("Docs", malicious_json);
        // The raw </script> must not appear — it would break out of the script tag
        assert!(!html.contains("</script><script>alert"));
        assert!(html.contains(r"<\/script>"));
    }

    #[test]
    fn html_shell_escapes_title_html() {
        let html = html_shell("<script>alert(1)</script>", "{}");
        assert!(!html.contains("<title><script>"));
        assert!(html.contains("<title>&lt;script&gt;"));
    }

    #[test]
    fn escape_helpers() {
        assert_eq!(escape_html_text("a<b>c&d"), "a&lt;b&gt;c&amp;d");
        assert_eq!(escape_script_content("</script>"), r"<\/script>");
        // No escaping needed for safe content
        assert_eq!(escape_script_content("hello world"), "hello world");
    }

    #[test]
    fn html_shell_with_scip_embeds_data() {
        let json = r#"{"items":[],"modules":[]}"#;
        let scip_bytes = b"fake scip data for testing";
        let html = html_shell_with_scip("Test", json, Some(scip_bytes));

        // Contains the SCIP data element
        assert!(html.contains("id=\"scip-data\""), "should have scip-data element");
        assert!(
            html.contains("type=\"application/octet-stream\""),
            "should have octet-stream type"
        );
        // Contains base64-encoded data
        assert!(html.contains("FE_SCIP_BYTES"), "should have WASM init snippet");
        // Still contains the base DocIndex
        assert!(html.contains("FE_DOC_INDEX"), "should still have DocIndex");
    }

    #[test]
    fn html_shell_with_scip_none_matches_original() {
        let json = r#"{"items":[],"modules":[]}"#;
        let without = html_shell("Test", json);
        let with_none = html_shell_with_scip("Test", json, None);
        assert_eq!(without, with_none);
    }
}
