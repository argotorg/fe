//! Embedded assets for static documentation sites

use crate::escape::{base64_encode, escape_html_text, escape_script_content};

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

/// `<fe-doc-item>` custom element.
pub const FE_DOC_ITEM_JS: &str = include_str!("../assets/fe-doc-item.js");

/// `<fe-symbol-link>` custom element.
pub const FE_SYMBOL_LINK_JS: &str = include_str!("../assets/fe-symbol-link.js");

/// Standalone syntax highlighting CSS (hardcoded colors, no CSS variables).
/// For embedding in Starlight/Astro or any external site.
pub const FE_HIGHLIGHT_CSS: &str = include_str!("../assets/fe-highlight.css");

/// Pure-JS ScipStore class that reads pre-processed SCIP JSON.
pub const FE_SCIP_STORE_JS: &str = include_str!("../assets/fe-scip-store.js");

/// web-tree-sitter Emscripten runtime JS.
const TREE_SITTER_JS: &str = include_str!("../vendor/tree-sitter.js");

/// Client-side highlighter template (placeholders replaced at build time).
const FE_HIGHLIGHTER_TEMPLATE: &str = include_str!("../assets/fe-highlighter.js");

/// tree-sitter core WASM binary.
const TS_WASM: &[u8] = include_bytes!("../vendor/tree-sitter.wasm");

/// Fe language grammar WASM binary.
const FE_WASM: &[u8] = include_bytes!("../vendor/tree-sitter-fe.wasm");

/// tree-sitter-fe highlights.scm query source.
const HIGHLIGHTS_SCM: &str = include_str!("../../tree-sitter-fe/queries/highlights.scm");

/// Build the highlighter JS with embedded WASM binaries and query source.
fn build_highlighter_js() -> String {
    FE_HIGHLIGHTER_TEMPLATE
        .replacen(
            "\"%%TS_WASM_B64%%\"",
            &format!("\"{}\"", base64_encode(TS_WASM)),
            1,
        )
        .replacen(
            "\"%%FE_WASM_B64%%\"",
            &format!("\"{}\"", base64_encode(FE_WASM)),
            1,
        )
        .replacen(
            "\"%%HIGHLIGHTS_SCM%%\"",
            &format!("\"{}\"", js_escape_string(HIGHLIGHTS_SCM)),
            1,
        )
}

/// Escape a string for embedding in a JS string literal (double-quoted).
fn js_escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 32);
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(c),
        }
    }
    out
}

/// Generate the complete HTML shell for a static documentation site.
///
/// The `doc_index_json` is inlined into a `<script>` tag so the page works
/// with `file://` — no server required.
pub fn html_shell(title: &str, doc_index_json: &str) -> String {
    html_shell_with_scip(title, doc_index_json, None)
}

/// Generate the HTML shell with optional embedded SCIP data and source link base.
///
/// When `scip_json` is provided, the pre-processed SCIP JSON is inlined
/// into a `<script>` tag and a pure-JS `ScipStore` class is loaded to
/// provide interactive symbol resolution (no WASM required).
///
/// When `source_link_base` is provided (e.g. "https://github.com/org/repo/blob/abc123"),
/// source links in item headers become clickable GitHub links.
pub fn html_shell_with_scip(title: &str, doc_index_json: &str, scip_json: Option<&str>) -> String {
    html_shell_full(title, doc_index_json, scip_json, None)
}

/// Full HTML shell with all optional features.
pub fn html_shell_full(
    title: &str,
    doc_index_json: &str,
    scip_json: Option<&str>,
    source_link_base: Option<&str>,
) -> String {
    // Escape for safe embedding inside HTML/script contexts:
    // - Title: escape HTML special chars to prevent </title> breakout
    // - JSON: escape </ sequences to prevent </script> breakout
    let safe_title = escape_html_text(title);
    let safe_json = escape_script_content(doc_index_json);

    let scip_section = if let Some(json) = scip_json {
        let safe_scip = escape_script_content(json);
        format!(
            "\n  <script>{scip_store_js}</script>\n  <script>try {{ window.FE_SCIP_DATA = {scip_data};\nwindow.FE_SCIP = new ScipStore(window.FE_SCIP_DATA); }} catch(e) {{ console.error('[fe-scip] init failed:', e); }}</script>",
            scip_store_js = FE_SCIP_STORE_JS,
            scip_data = safe_scip,
        )
    } else {
        String::new()
    };

    let highlighter_js = build_highlighter_js();

    let source_section = if let Some(base) = source_link_base {
        let safe_base = escape_script_content(base);
        format!(
            "\n  <script>window.FE_SOURCE_BASE = \"{}\";</script>",
            safe_base
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
  <style>{highlight_css}</style>
</head>
<body>
  <script>window.FE_DOC_INDEX = {json};</script>{scip_section}{source_section}
  <script>{tree_sitter_js}</script>
  <script>{highlighter_js}</script>
  <script>{code_block_js}</script>
  <script>{signature_js}</script>
  <script>{doc_item_js}</script>
  <script>{symbol_link_js}</script>
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
        highlight_css = FE_HIGHLIGHT_CSS,
        json = safe_json,
        scip_section = scip_section,
        source_section = source_section,
        tree_sitter_js = TREE_SITTER_JS,
        highlighter_js = highlighter_js,
        code_block_js = FE_CODE_BLOCK_JS,
        signature_js = FE_SIGNATURE_JS,
        doc_item_js = FE_DOC_ITEM_JS,
        symbol_link_js = FE_SYMBOL_LINK_JS,
        search_js = FE_SEARCH_JS,
        js = FE_WEB_JS,
    )
}

/// Build a standalone fe-web.js bundle for external consumption.
///
/// This is the single JS file that consumers load via:
///   `<script type="module" src="fe-web.js" data-src="docs.json" data-docs="/api/">`
///
/// It includes: the script-tag loader (reads data-src/data-docs, fetches JSON,
/// populates the global store), ScipStore, tree-sitter + highlighter with
/// embedded WASM, and all custom element definitions.
pub fn web_component_bundle() -> String {
    let highlighter_js = build_highlighter_js();

    format!(
        r#"// fe-web.js — Fe documentation web components bundle
// Usage: <script type="module" src="fe-web.js" data-src="docs.json" data-docs="/api/"></script>

// ============================================================================
// Script-tag loader: reads data-src and data-docs, fetches JSON, populates globals
// ============================================================================
(function() {{
  "use strict";
  var script = document.currentScript || document.querySelector('script[data-src]');
  if (!script) return;

  var dataSrc = script.getAttribute('data-src');
  var dataDocs = script.getAttribute('data-docs');

  if (dataDocs) {{
    window.FE_DOCS_BASE = dataDocs;
  }}

  // Signal that the bundle is loading
  window.FE_WEB_READY = new Promise(function(resolve) {{
    window._feWebResolve = resolve;
  }});

  if (dataSrc) {{
    fetch(dataSrc)
      .then(function(r) {{ return r.json(); }})
      .then(function(data) {{
        if (data.index) {{
          window.FE_DOC_INDEX = data.index;
          if (data.scip) {{
            window.FE_SCIP_DATA = data.scip;
            if (typeof ScipStore !== 'undefined') {{
              try {{ window.FE_SCIP = new ScipStore(data.scip); }} catch(e) {{
                console.error('[fe-web] ScipStore init failed:', e);
              }}
            }}
          }}
        }} else {{
          // Plain DocIndex without SCIP wrapper
          window.FE_DOC_INDEX = data;
        }}
        window._feWebResolve();
        document.dispatchEvent(new CustomEvent('fe-web-ready'));
      }})
      .catch(function(err) {{
        console.error('[fe-web] Failed to load', dataSrc, err);
        window._feWebResolve();
      }});
  }} else {{
    // No data-src — globals may already be set (e.g. static site)
    window._feWebResolve();
  }}
}})();

// ============================================================================
// ScipStore
// ============================================================================
{scip_store_js}

// ============================================================================
// Tree-sitter runtime
// ============================================================================
{tree_sitter_js}

// ============================================================================
// Highlighter (with embedded WASM)
// ============================================================================
{highlighter_js}

// ============================================================================
// Custom elements
// ============================================================================
{code_block_js}

{signature_js}

{doc_item_js}

{symbol_link_js}

{search_js}
"#,
        scip_store_js = FE_SCIP_STORE_JS,
        tree_sitter_js = TREE_SITTER_JS,
        highlighter_js = highlighter_js,
        code_block_js = FE_CODE_BLOCK_JS,
        signature_js = FE_SIGNATURE_JS,
        doc_item_js = FE_DOC_ITEM_JS,
        symbol_link_js = FE_SYMBOL_LINK_JS,
        search_js = FE_SEARCH_JS,
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
        assert!(FE_DOC_ITEM_JS.contains("fe-doc-item"));
        assert!(FE_SYMBOL_LINK_JS.contains("fe-symbol-link"));
    }

    #[test]
    fn highlighter_js_has_embedded_data() {
        let js = build_highlighter_js();
        // Should not contain unresolved placeholders
        assert!(
            !js.contains("%%TS_WASM_B64%%"),
            "TS_WASM placeholder should be replaced"
        );
        assert!(
            !js.contains("%%FE_WASM_B64%%"),
            "FE_WASM placeholder should be replaced"
        );
        assert!(
            !js.contains("%%HIGHLIGHTS_SCM%%"),
            "HIGHLIGHTS_SCM placeholder should be replaced"
        );
        // Should contain base64-encoded data (long strings starting with typical WASM b64)
        assert!(
            js.len() > 100_000,
            "should be large with embedded WASMs: {} bytes",
            js.len()
        );
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
        assert!(html.contains("fe-doc-item"));
        assert!(html.contains("fe-symbol-link"));
        assert!(html.contains("fe-search"));
        // Tree-sitter and highlighter are loaded
        assert!(
            html.contains("TreeSitter"),
            "should contain tree-sitter runtime"
        );
        assert!(html.contains("FeHighlighter"), "should contain highlighter");
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
        let scip_json = r#"{"symbols":{},"files":{}}"#;
        let html = html_shell_with_scip("Test", json, Some(scip_json));

        // Contains the ScipStore class
        assert!(html.contains("ScipStore"), "should have ScipStore class");
        // Contains the SCIP data assignment
        assert!(
            html.contains("FE_SCIP_DATA"),
            "should have SCIP data inline"
        );
        // Contains the FE_SCIP initialization
        assert!(html.contains("window.FE_SCIP"), "should initialize FE_SCIP");
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

    #[test]
    fn js_escape_handles_special_chars() {
        assert_eq!(js_escape_string("hello\nworld"), "hello\\nworld");
        assert_eq!(js_escape_string(r#"a"b"#), r#"a\"b"#);
        assert_eq!(js_escape_string("a\\b"), "a\\\\b");
    }
}
