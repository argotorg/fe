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

/// `<fe-doc-nav>` custom element.
pub const FE_DOC_NAV_JS: &str = include_str!("../assets/fe-doc-nav.js");

/// `<fe-doc-viewer>` custom element.
pub const FE_DOC_VIEWER_JS: &str = include_str!("../assets/fe-doc-viewer.js");

/// `<fe-origin-trace>` custom element.
pub const FE_ORIGIN_TRACE_JS: &str = include_str!("../assets/fe-origin-trace.js");

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

/// Build the ScipStore JS with the schema version placeholder injected.
fn scip_store_js() -> String {
    FE_SCIP_STORE_JS.replacen(
        "%%SCHEMA_VERSION%%",
        &crate::model::SCHEMA_VERSION.to_string(),
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
            scip_store_js = scip_store_js(),
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
<html lang="en" data-theme="light">
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
  <script>{doc_nav_js}</script>
  <script>{doc_viewer_js}</script>
  <fe-doc-viewer title="{title}" routing="hash" show-search></fe-doc-viewer>
  <script>
    // Signal data is ready for components using global store
    document.dispatchEvent(new CustomEvent('fe-web-ready'));
  </script>
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
        doc_nav_js = FE_DOC_NAV_JS,
        doc_viewer_js = FE_DOC_VIEWER_JS,
    )
}

/// Generate a standalone HTML shell for the origin trace web component.
///
/// The trace view JSON is inlined so the page works from `file://` and from
/// the existing static HTTP server without a separate bundling step.
pub fn origin_trace_html_shell(title: &str, trace_view_json: &str) -> String {
    let safe_title = escape_html_text(title);
    let safe_json = escape_script_content(trace_view_json);
    format!(
        r#"<!DOCTYPE html>
<html lang="en" data-theme="dark">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{title}</title>
  <style>{css}</style>
  <style>
    html, body {{ margin: 0; min-height: 100%; background: var(--bg); }}
  </style>
  <style>{highlight_css}</style>
</head>
<body>
  <script>window.FE_ORIGIN_TRACE_DATA = {json};</script>
  <script>{origin_trace_js}</script>
  <fe-origin-trace></fe-origin-trace>
</body>
</html>"#,
        title = safe_title,
        css = STYLES_CSS,
        highlight_css = FE_HIGHLIGHT_CSS,
        json = safe_json,
        origin_trace_js = FE_ORIGIN_TRACE_JS,
    )
}

/// Generate an HTTP-backed shell for the live trace workbench.
///
/// The browser fetches the current session model from the local LSP HTTP server.
/// The token is read from the URL fragment and is not embedded into the HTML.
pub fn origin_trace_live_html_shell(title: &str) -> String {
    let safe_title = escape_html_text(title);
    format!(
        r#"<!DOCTYPE html>
<html lang="en" data-theme="dark">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{title}</title>
  <style>{css}</style>
  <style>
    html, body {{ margin: 0; min-height: 100%; background: var(--bg); }}
    .trace-live-loading {{ color: #cdd6f4; font: 13px/1.4 ui-sans-serif, system-ui, sans-serif; padding: 18px; }}
  </style>
  <style>{highlight_css}</style>
</head>
<body>
  <div class="trace-live-loading">Loading Fe trace workbench...</div>
  <script>{origin_trace_js}</script>
  <script>
  (function () {{
    var params = new URLSearchParams(window.location.search || "");
    var hash = new URLSearchParams((window.location.hash || "").replace(/^#/, ""));
    var session = params.get("session");
    var token = hash.get("token") || params.get("token") || "";
    var loading = document.querySelector(".trace-live-loading");
    var authHeaders = token ? {{ "Authorization": "Bearer " + token }} : {{}};
    var storageKey = "fe.trace.workbench.lastReady." + (session || "");
    var modelRefresh = null;
    var manifestCache = null;
    var revisionHistoryCache = null;
    function fail(message) {{
      if (loading) loading.textContent = message;
      console.error("[fe trace workbench]", message);
    }}
    function renderModel(model, staleMessage) {{
      model = model || {{}};
      if (staleMessage) {{
        model = Object.assign({{}}, model);
        model.revision = Object.assign({{}}, model.revision || {{}}, {{ status: "stale_but_usable" }});
        model.notes = (model.notes || []).concat([staleMessage]);
      }}
      window.FE_ORIGIN_TRACE_DATA = model;
      window.FE_TRACE_WORKBENCH_REVISION = model && model.revision && model.revision.id || 0;
      if (loading) loading.remove();
      var existing = document.querySelector("fe-origin-trace");
      if (existing && typeof existing.setTraceData === "function") {{
        existing.setTraceData(model);
      }} else {{
        if (existing) existing.remove();
        document.body.appendChild(document.createElement("fe-origin-trace"));
      }}
    }}
    function rememberModel(model) {{
      try {{
        window.sessionStorage && window.sessionStorage.setItem(storageKey, JSON.stringify(model));
      }} catch (_) {{}}
    }}
    function renderCachedModel(reason) {{
      try {{
      var cached = window.sessionStorage && window.sessionStorage.getItem(storageKey);
      if (!cached) return false;
      renderModel(JSON.parse(cached), "Showing last ready trace revision because live refresh failed: " + reason);
      return true;
      }} catch (_) {{
        return false;
      }}
    }}
    function fetchJson(path) {{
      return fetch(path, {{ headers: authHeaders }}).then(function (response) {{
        if (!response.ok) throw new Error(path + " fetch failed: " + response.status);
        return response.json();
      }});
    }}
    function fetchChunk(digest) {{
      return fetchJson("/trace/session/" + encodeURIComponent(session) + "/chunk/" + encodeURIComponent(digest));
    }}
    function fetchChunks(digests) {{
      return fetch("/trace/session/" + encodeURIComponent(session) + "/chunks/missing", {{
        method: "POST",
        headers: Object.assign({{ "Content-Type": "application/json" }}, authHeaders),
        body: JSON.stringify({{ digests: digests }})
      }}).then(function (response) {{
        if (!response.ok) throw new Error("chunk batch fetch failed: " + response.status);
        return response.json();
      }});
    }}
    function refreshRevisionHistory() {{
      if (!session) return Promise.resolve(null);
      return fetchJson("/trace/session/" + encodeURIComponent(session) + "/revisions")
        .then(function (history) {{
          revisionHistoryCache = history;
          window.FE_TRACE_WORKBENCH_REVISIONS = history;
          return history;
        }}, function () {{
          return revisionHistoryCache;
        }});
    }}
    function applyChunkedManifest(manifest) {{
      var previous = manifestCache;
      var current = window.FE_ORIGIN_TRACE_DATA || {{}};
      window.FE_TRACE_WORKBENCH_MODEL_DIGEST = (manifest && manifest.root_digest) || "";
      if (!previous || !current || !current.revision) {{
        manifestCache = manifest;
        return fetchJson("/trace/session/" + encodeURIComponent(session) + "/model");
      }}
      if (manifest.root_digest && previous.root_digest === manifest.root_digest) {{
        return Promise.resolve(current);
      }}
      var next = Object.assign({{}}, current);
      var requests = [];
      function useChunk(digest, previousDigest, apply) {{
        if (!digest || digest === previousDigest) return;
        requests.push({{ digest: digest, apply: apply }});
      }}
      useChunk(manifest.summary_digest, previous.summary_digest, function (value) {{
        value = value || {{}};
        ["revision", "metadata", "provenance", "counts", "salsa", "bytecode_count", "selection_remap", "notes"].forEach(function (key) {{
          if (Object.prototype.hasOwnProperty.call(value, key)) next[key] = value[key];
        }});
      }});
      useChunk(manifest.source_digest, previous.source_digest, function (value) {{
        next.source = value;
      }});
      useChunk(manifest.indexes_digest, previous.indexes_digest, function (value) {{
        next.indexes = value;
      }});
      useChunk(manifest.rail_components_digest, previous.rail_components_digest, function (value) {{
        next.rail_components = value;
      }});
      var previousPanes = previous.panes || {{}};
      var nextPanes = Object.assign({{}}, manifest.panes || {{}});
      var paneById = Object.create(null);
      (current.panels || []).forEach(function (pane) {{
        if (pane && pane.id) paneById[pane.id] = pane;
      }});
      Object.keys(nextPanes).forEach(function (id) {{
        useChunk(nextPanes[id], previousPanes[id], function (value) {{
          paneById[id] = value;
        }});
      }});
      var previousReports = previous.reports || {{}};
      var nextReports = manifest.reports || {{}};
      var reportKeys = {{
        attribution: "attribution_audit",
        static_analysis: "static_analysis",
        closure_audit: "audit",
        duplicate_shapes: "duplicate_shapes"
      }};
      Object.keys(reportKeys).forEach(function (id) {{
        useChunk(nextReports[id], previousReports[id], function (value) {{
          next[reportKeys[id]] = value;
        }});
      }});
      var chunkPromise = requests.length
        ? fetchChunks(requests.map(function (request) {{ return request.digest; }})).then(function (response) {{
            var chunksByDigest = Object.create(null);
            (response.chunks || []).forEach(function (chunk) {{
              if (chunk && chunk.digest) chunksByDigest[chunk.digest] = chunk;
            }});
            if ((response.missing || []).length) throw new Error("missing trace chunks: " + response.missing.join(","));
            requests.forEach(function (request) {{
              var chunk = chunksByDigest[request.digest];
              if (!chunk || chunk.digest !== request.digest) throw new Error("chunk digest mismatch");
              request.apply(chunk.value);
            }});
          }})
        : Promise.resolve();
      return chunkPromise.then(function () {{
        next.panels = Object.keys(nextPanes).map(function (id) {{
          return paneById[id];
        }}).filter(Boolean);
        manifestCache = manifest;
        return next;
      }});
    }}
    function applyInitialSelection(bootstrap) {{
      var selection = bootstrap
        && bootstrap.session
        && bootstrap.session.initialSelection;
      if (!selection) return;
      var current = new URLSearchParams((window.location.hash || "").replace(/^#/, ""));
      if (current.get("node") || current.get("source") || current.get("row")) return;
      current.set("source", "main:" + (Number(selection.startLine || 0) + 1));
      var nextHash = String.fromCharCode(35) + current.toString();
      if (window.history && window.history.replaceState) {{
        window.history.replaceState(null, "", nextHash);
      }} else {{
        window.location.hash = nextHash;
      }}
    }}
    function pinResolvedRow(rowId) {{
      if (!rowId) return;
      var current = new URLSearchParams((window.location.hash || "").replace(/^#/, ""));
      current.delete("node");
      current.delete("source");
      current.set("row", rowId);
      if (token && !current.get("token")) current.set("token", token);
      window.location.hash = current.toString();
    }}
    function fetchAndRenderModel() {{
      if (modelRefresh) return modelRefresh;
      modelRefresh = fetchJson("/trace/session/" + encodeURIComponent(session) + "/manifest")
        .then(function (manifest) {{
          return applyChunkedManifest(manifest);
        }})
        .catch(function () {{
          return fetchJson("/trace/session/" + encodeURIComponent(session) + "/model")
            .then(function (model) {{
              return fetchJson("/trace/session/" + encodeURIComponent(session) + "/manifest")
                .then(function (manifest) {{
                  manifestCache = manifest;
                  return model;
                }}, function () {{
                  manifestCache = null;
                  return model;
                }});
            }});
        }})
        .then(function (model) {{
          rememberModel(model);
          renderModel(model);
          refreshRevisionHistory();
          return model;
        }})
        .finally(function () {{
          modelRefresh = null;
        }});
      return modelRefresh;
    }}
    if (!session) return fail("Missing trace workbench session.");
    fetch("/trace/session/" + encodeURIComponent(session) + "/bootstrap", {{ headers: authHeaders }})
      .then(function (response) {{
        if (!response.ok) throw new Error("bootstrap fetch failed: " + response.status);
        return response.json();
      }})
      .then(function (bootstrap) {{
        applyInitialSelection(bootstrap);
        return fetchAndRenderModel();
      }})
      .catch(function (err) {{
        var reason = String(err && err.message || err);
        if (!renderCachedModel(reason)) fail(reason);
      }});
    if (window.EventSource && token) {{
      var events = new EventSource("/trace/session/" + encodeURIComponent(session) + "/events?token=" + encodeURIComponent(token));
      events.addEventListener("trace/revision", function (event) {{
        try {{
          var payload = JSON.parse(event.data || "{{}}");
          var revisionChanged = payload.revision && Number(payload.revision) !== Number(window.FE_TRACE_WORKBENCH_REVISION || 0);
          var digestChanged = payload.modelDigest && payload.modelDigest !== window.FE_TRACE_WORKBENCH_MODEL_DIGEST;
          var refreshableStatus = payload.status === "ready"
            || payload.status === "stale_but_usable"
            || payload.status === "pending";
          if (refreshableStatus && (revisionChanged || digestChanged)) {{
            fetchAndRenderModel().catch(function (err) {{
              var reason = String(err && err.message || err);
              renderCachedModel(reason);
            }});
          }}
        }} catch (_) {{}}
      }});
      events.addEventListener("trace/selection", function (event) {{
        try {{
          var payload = JSON.parse(event.data || "{{}}");
          if (payload.sessionId && payload.sessionId !== session) return;
          var rows = payload.resolvedRows || [];
          if (rows.length) pinResolvedRow(rows[0]);
        }} catch (_) {{}}
      }});
    }}
  }})();
  </script>
</body>
</html>"#,
        title = safe_title,
        css = STYLES_CSS,
        highlight_css = FE_HIGHLIGHT_CSS,
        origin_trace_js = FE_ORIGIN_TRACE_JS,
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
    let highlight_css_literal = format!("\"{}\"", js_escape_string(FE_HIGHLIGHT_CSS));

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
    (typeof feFetchJson === 'function' ? feFetchJson(dataSrc) :
      fetch(dataSrc).then(function(r) {{
        if (!r.ok) throw new Error("HTTP " + r.status + " loading " + dataSrc);
        return r.json();
      }}))
      .then(function(data) {{
        data = feMigrate(data);
        if (data && data.index) {{
          window.FE_DOC_INDEX = data.index;
          if (data.scip) {{
            window.FE_SCIP_DATA = data.scip;
            if (typeof ScipStore !== 'undefined') {{
              try {{ window.FE_SCIP = new ScipStore(data.scip); }} catch(e) {{
                console.error('[fe-web] ScipStore init failed:', e);
              }}
            }}
          }}
        }} else if (data) {{
          // Fallback — should not happen after migration
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
// Highlight CSS (injected for shadow DOM adoption — no DOM scanning needed)
// ============================================================================
var __FE_HIGHLIGHT_CSS_INJECTED__ = {highlight_css_js};

// ============================================================================
// Custom elements
// ============================================================================
{code_block_js}

{signature_js}

{doc_item_js}

{symbol_link_js}

{search_js}

{doc_nav_js}

{doc_viewer_js}
"#,
        scip_store_js = scip_store_js(),
        tree_sitter_js = TREE_SITTER_JS,
        highlighter_js = highlighter_js,
        highlight_css_js = highlight_css_literal,
        code_block_js = FE_CODE_BLOCK_JS,
        signature_js = FE_SIGNATURE_JS,
        doc_item_js = FE_DOC_ITEM_JS,
        symbol_link_js = FE_SYMBOL_LINK_JS,
        search_js = FE_SEARCH_JS,
        doc_nav_js = FE_DOC_NAV_JS,
        doc_viewer_js = FE_DOC_VIEWER_JS,
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
        assert!(FE_WEB_JS.contains("render"));
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
        assert!(html.contains(r#"window.FE_DOC_INDEX = {"items":[],"modules":[]}"#));
        // Uses <fe-doc-viewer> component instead of manual div layout
        assert!(html.contains("fe-doc-viewer"));
        // Custom elements are loaded
        assert!(html.contains("fe-code-block"));
        assert!(html.contains("fe-signature"));
        assert!(html.contains("fe-doc-item"));
        assert!(html.contains("fe-symbol-link"));
        assert!(html.contains("fe-search"));
        assert!(html.contains("fe-doc-nav"));
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
    fn live_trace_shell_fetches_manifest_and_chunks() {
        let html = origin_trace_live_html_shell("Trace");

        assert!(html.contains("/trace/session/"));
        assert!(html.contains("/manifest"));
        assert!(html.contains("/chunk/"));
        assert!(html.contains("/chunks/missing"));
        assert!(html.contains("/revisions"));
        assert!(html.contains("applyChunkedManifest"));
        assert!(html.contains("manifestCache"));
        assert!(html.contains("FE_TRACE_WORKBENCH_MODEL_DIGEST"));
        assert!(html.contains("modelDigest"));
        assert!(html.contains("FE_TRACE_WORKBENCH_REVISIONS"));
        assert!(html.contains("trace/selection"));
        assert!(html.contains("resolvedRows"));
        assert!(html.contains("refreshableStatus"));
        assert!(html.contains("stale_but_usable"));
        assert!(html.contains("pending"));
        assert!(html.contains("selection_remap"));
    }

    #[test]
    fn origin_trace_default_badges_use_product_language() {
        assert!(!FE_ORIGIN_TRACE_JS.contains("label: \"compiler-generated\""));
        assert!(!FE_ORIGIN_TRACE_JS.contains("label: \"needs evidence\""));
        assert!(!FE_ORIGIN_TRACE_JS.contains("box.append(this._railLegend());"));
        assert!(!FE_ORIGIN_TRACE_JS.contains(
            "indexOf(\"exact-c-\") === 0; })) return { kind: \"ok\", label: \"exact\" }"
        ));
        assert!(FE_ORIGIN_TRACE_JS.contains("if (kind === \"exact\") return null;"));
        assert!(FE_ORIGIN_TRACE_JS.contains("suppressExact"));
        assert!(FE_ORIGIN_TRACE_JS.contains("suppressExact: true"));
        assert!(FE_ORIGIN_TRACE_JS.contains("dataset.hoverGroups"));
        assert!(FE_ORIGIN_TRACE_JS.contains("dataset.selectionGroups"));
        assert!(FE_ORIGIN_TRACE_JS.contains("hoverClasses(row)"));
        assert!(FE_ORIGIN_TRACE_JS.contains("selectionClasses(row)"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_expandSelectionGroups(selectionClasses(row))"));
        assert!(FE_ORIGIN_TRACE_JS.contains("name.indexOf(\"prepared-c-\") !== 0"));
        assert!(FE_ORIGIN_TRACE_JS.contains("return exact.concat(generated, prepared);"));
        assert!(FE_ORIGIN_TRACE_JS.contains("return \"evm-vcode\""));
        assert!(
            !FE_ORIGIN_TRACE_JS
                .contains("_displayStatus(entries, this._railStatus(displayClasses)")
        );
        assert!(!FE_ORIGIN_TRACE_JS.contains("label: \"exact link\""));
        assert!(!FE_ORIGIN_TRACE_JS.contains("satisfied_exact: \"exact\""));
        assert!(FE_ORIGIN_TRACE_JS.contains("satisfied_exact: \"satisfied\""));
        assert!(!FE_ORIGIN_TRACE_JS.contains("MIR-only"));
        assert!(!FE_ORIGIN_TRACE_JS.contains("preopt-only"));
        assert!(FE_ORIGIN_TRACE_JS.contains("missing downstream"));
        assert!(!FE_ORIGIN_TRACE_JS.contains("rail-legend"));
        assert!(!FE_ORIGIN_TRACE_JS.contains("legend-chip"));
        assert!(FE_ORIGIN_TRACE_JS.contains("label: \"generated\""));
        assert!(FE_ORIGIN_TRACE_JS.contains("label: \"unmapped\""));
        assert!(FE_ORIGIN_TRACE_JS.contains("No row selected."));
        assert!(FE_ORIGIN_TRACE_JS.contains("Selected row"));
        assert!(FE_ORIGIN_TRACE_JS.contains("Missing Link Audit"));
        assert!(FE_ORIGIN_TRACE_JS.contains("Boundary status"));
        assert!(FE_ORIGIN_TRACE_JS.contains("linked phases, generated work, and gaps"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_componentReachedSummary"));
        assert!(FE_ORIGIN_TRACE_JS.contains("Rail component reaches"));
        assert!(FE_ORIGIN_TRACE_JS.contains("exact phase link"));
        assert!(FE_ORIGIN_TRACE_JS.contains("linked regions"));
        assert!(FE_ORIGIN_TRACE_JS.contains("need compiler evidence"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_friendlyCheckSummary"));
        assert!(FE_ORIGIN_TRACE_JS.contains("optimizer-explained"));
        assert!(
            FE_ORIGIN_TRACE_JS
                .contains("Optimized-away code should be marked by explicit optimizer events.")
        );
        assert!(!FE_ORIGIN_TRACE_JS.contains("inspect evidence paths"));
    }

    #[test]
    fn origin_trace_pane_switches_do_not_rerun_hash_navigation() {
        assert!(FE_ORIGIN_TRACE_JS.contains("hashNavigation: false"));
        assert!(FE_ORIGIN_TRACE_JS.contains("hashNavigation: true"));
        assert!(FE_ORIGIN_TRACE_JS.contains("allowHashNavigation"));
    }

    #[test]
    fn origin_trace_bloat_rows_render_attribution_split() {
        assert!(FE_ORIGIN_TRACE_JS.contains("_bloatSplitText"));
        assert!(FE_ORIGIN_TRACE_JS.contains("bloatBox.append(row);\n        }, this);"));
        assert!(FE_ORIGIN_TRACE_JS.contains("source-exact"));
        assert!(FE_ORIGIN_TRACE_JS.contains("prepared-only"));
        assert!(FE_ORIGIN_TRACE_JS.contains("generated/backend"));
    }

    #[test]
    fn origin_trace_renders_duplicate_shape_report() {
        assert!(FE_ORIGIN_TRACE_JS.contains("_duplicateShapes"));
        assert!(FE_ORIGIN_TRACE_JS.contains("Duplicate Shapes"));
        assert!(FE_ORIGIN_TRACE_JS.contains("not provenance"));
        assert!(origin_trace_live_html_shell("Trace").contains("duplicate_shapes"));
    }

    #[test]
    fn origin_trace_shell_keeps_scroll_pane_local() {
        assert!(FE_ORIGIN_TRACE_JS.contains("height:100vh"));
        assert!(FE_ORIGIN_TRACE_JS.contains("overflow:hidden"));
        assert!(FE_ORIGIN_TRACE_JS.contains("overscroll-behavior:contain"));
        assert!(FE_ORIGIN_TRACE_JS.contains(".bottom-deck::-webkit-scrollbar"));
    }

    #[test]
    fn origin_trace_keeps_trace_notes_from_crowding_selection_details() {
        assert!(FE_ORIGIN_TRACE_JS.contains("_traceNotes"));
        assert!(FE_ORIGIN_TRACE_JS.contains("Trace assumptions"));
        assert!(FE_ORIGIN_TRACE_JS.contains("trace-notes"));
        assert!(FE_ORIGIN_TRACE_JS.contains("flex:0 0 clamp(260px,38vh,520px)"));
        assert!(FE_ORIGIN_TRACE_JS.contains("flex:1 1 auto; min-height:220px"));
        assert!(!FE_ORIGIN_TRACE_JS.contains("page.append(notes);"));
    }

    #[test]
    fn origin_trace_scroll_uses_pane_offsets_and_visibility_correction() {
        assert!(FE_ORIGIN_TRACE_JS.contains("_rowScrollTop"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_offsetTopWithin"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_clampScrollTop"));
        assert!(FE_ORIGIN_TRACE_JS.contains("scroller.scrollTo"));
        assert!(FE_ORIGIN_TRACE_JS.contains("behavior: behavior"));
    }

    #[test]
    fn origin_trace_jump_controls_are_selection_scoped() {
        assert!(FE_ORIGIN_TRACE_JS.contains("activeRunKey"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_selectRow"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_selectGroups"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_clearSelection"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_revealSelectionInShell"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_setActiveRunForRow"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_runIndexNearestViewport"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_bestRunIndexForSelection"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_runGroupScore"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_runScanNodes"));
        assert!(FE_ORIGIN_TRACE_JS.contains("source-section-separator"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_markerTarget"));
        assert!(FE_ORIGIN_TRACE_JS.contains("preferSectionBoundary: false"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_withBoundaryPreference(options, false)"));
    }

    #[test]
    fn origin_trace_preserves_selection_by_stable_identity() {
        assert!(FE_ORIGIN_TRACE_JS.contains("stableIdentityToken"));
        assert!(FE_ORIGIN_TRACE_JS.contains("dataset.stableIdentities"));
        assert!(FE_ORIGIN_TRACE_JS.contains("_restoreSelectionByStableIdentity"));
        assert!(FE_ORIGIN_TRACE_JS.contains("[data-stable-identities~="));
    }

    #[test]
    fn origin_trace_suppresses_broad_source_span_badges() {
        assert!(FE_ORIGIN_TRACE_JS.contains("suppress_rail_status"));
        assert!(FE_ORIGIN_TRACE_JS.contains(
            "if (!rowStatus && rowOrClasses && rowOrClasses.suppress_rail_status) return wrap;"
        ));
        assert!(FE_ORIGIN_TRACE_JS.contains("if (!Array.isArray(rowOrClasses))"));
        assert!(FE_ORIGIN_TRACE_JS.contains("if (!rowStatus) return wrap;"));
    }

    #[test]
    fn origin_trace_shows_live_revision_state_banner() {
        assert!(FE_ORIGIN_TRACE_JS.contains("_revisionBanner"));
        assert!(FE_ORIGIN_TRACE_JS.contains("Trace update pending"));
        assert!(FE_ORIGIN_TRACE_JS.contains("Showing last ready trace"));
        assert!(FE_ORIGIN_TRACE_JS.contains("Trace update failed"));
        assert!(FE_ORIGIN_TRACE_JS.contains(".revision-banner"));
    }

    #[test]
    fn js_escape_handles_special_chars() {
        assert_eq!(js_escape_string("hello\nworld"), "hello\\nworld");
        assert_eq!(js_escape_string(r#"a"b"#), r#"a\"b"#);
        assert_eq!(js_escape_string("a\\b"), "a\\\\b");
    }
}
