//! HTTP server for serving Fe documentation
//!
//! This module provides an axum-based HTTP server that integrates with the
//! language server's Salsa database for real-time documentation.
//!
//! # Features
//!
//! - Static file serving for assets (CSS, JS, WASM)
//! - Dynamic documentation rendering (SSR mode)
//! - Client-side rendering with WASM (CSR mode)
//! - Live search API
//! - WebSocket support for live updates

use axum::{
    Router,
    extract::{Path, Query, State},
    http::StatusCode,
    response::{Html, IntoResponse, Json},
    routing::get,
};
use std::sync::Arc;
use tower_http::services::ServeDir;

use crate::model::{DocIndex, DocItem, DocItemKind};
use crate::ssr_components::{DocPage, DocNotFoundSSR};

// Embedded WASM assets for CSR mode
#[cfg(feature = "csr-assets")]
static WASM_JS: &[u8] = include_bytes!("../pkg/fe_doc_viewer.js");
#[cfg(feature = "csr-assets")]
static WASM_BG: &[u8] = include_bytes!("../pkg/fe_doc_viewer_bg.wasm");

/// Public access to WASM JS glue code for LSP integration
#[cfg(feature = "csr-assets")]
pub fn wasm_js_bytes() -> &'static [u8] {
    WASM_JS
}

/// Public access to WASM binary for LSP integration
#[cfg(feature = "csr-assets")]
pub fn wasm_bg_bytes() -> &'static [u8] {
    WASM_BG
}

/// Application state shared across handlers
pub struct DocServerState {
    /// The documentation index
    pub index: DocIndex,
    /// Path to static assets
    pub assets_path: Option<String>,
    /// Whether to use CSR (client-side rendering with WASM)
    pub csr_mode: bool,
}

impl DocServerState {
    pub fn new(index: DocIndex) -> Self {
        Self {
            index,
            assets_path: None,
            csr_mode: false,
        }
    }

    pub fn with_assets(mut self, path: impl Into<String>) -> Self {
        self.assets_path = Some(path.into());
        self
    }

    pub fn with_csr(mut self, enabled: bool) -> Self {
        self.csr_mode = enabled;
        self
    }
}

/// Create the documentation router
pub fn doc_router(state: Arc<DocServerState>) -> Router {
    let mut router = Router::new()
        .route("/", get(index_handler))
        .route("/doc/{*path}", get(doc_item_handler))
        .route("/api/search", get(search_handler))
        .route("/api/item/{*path}", get(item_api_handler))
        .route("/api/index", get(index_api_handler))
        .with_state(state.clone());

    // Serve WASM assets for CSR mode
    #[cfg(feature = "csr-assets")]
    {
        router = router
            .route("/assets/fe_doc_viewer.js", get(wasm_js_handler))
            .route("/assets/fe_doc_viewer_bg.wasm", get(wasm_bg_handler));
    }

    // Serve static assets if configured
    if let Some(ref assets_path) = state.assets_path {
        router = router.nest_service("/assets", ServeDir::new(assets_path));
    }

    router
}

/// Serve the WASM JS glue code
#[cfg(feature = "csr-assets")]
async fn wasm_js_handler() -> impl IntoResponse {
    Response::builder()
        .status(StatusCode::OK)
        .header(header::CONTENT_TYPE, "application/javascript")
        .header(header::CACHE_CONTROL, "public, max-age=31536000")
        .body(Body::from(WASM_JS))
        .unwrap()
}

/// Serve the WASM binary
#[cfg(feature = "csr-assets")]
async fn wasm_bg_handler() -> impl IntoResponse {
    Response::builder()
        .status(StatusCode::OK)
        .header(header::CONTENT_TYPE, "application/wasm")
        .header(header::CACHE_CONTROL, "public, max-age=31536000")
        .body(Body::from(WASM_BG))
        .unwrap()
}

/// Full index API handler (returns complete DocIndex as JSON)
async fn index_api_handler(
    State(state): State<Arc<DocServerState>>,
) -> impl IntoResponse {
    Json(state.index.clone())
}

/// Index page handler
async fn index_handler(State(state): State<Arc<DocServerState>>) -> impl IntoResponse {
    let title = "Fe Documentation";

    // Find the root module to start with
    let root_path = state
        .index
        .modules
        .first()
        .map(|m| m.path.clone())
        .unwrap_or_default();

    if state.csr_mode {
        Html(render_csr_shell(title, &root_path, &state.index))
    } else {
        Html(render_page(title, &root_path, &state.index))
    }
}

/// Documentation item handler
async fn doc_item_handler(
    State(state): State<Arc<DocServerState>>,
    Path(path): Path<String>,
) -> (StatusCode, Html<String>) {
    if state.csr_mode {
        // CSR mode: always return the shell, let client handle routing
        let title = "Fe Documentation";
        (StatusCode::OK, Html(render_csr_shell(title, &path, &state.index)))
    } else if let Some(item) = state.index.find_by_path(&path) {
        let title = format!("{} - Fe Documentation", item.name);
        (StatusCode::OK, Html(render_page(&title, &path, &state.index)))
    } else {
        // Render 404 using the same page template so WebSocket/auto-follow keeps working
        (StatusCode::NOT_FOUND, Html(render_page_not_found(&path, &state.index)))
    }
}

/// Search API handler
#[derive(serde::Deserialize)]
pub struct SearchQuery {
    q: String,
}

async fn search_handler(
    State(state): State<Arc<DocServerState>>,
    Query(query): Query<SearchQuery>,
) -> impl IntoResponse {
    let results: Vec<_> = state
        .index
        .search(&query.q)
        .into_iter()
        .take(20)
        .cloned()
        .collect();

    Json(results)
}

/// Item API handler (returns JSON)
async fn item_api_handler(
    State(state): State<Arc<DocServerState>>,
    Path(path): Path<String>,
) -> impl IntoResponse {
    if let Some(item) = state.index.find_by_path(&path) {
        Json(Some(item.clone()))
    } else {
        Json(None)
    }
}

/// Render a full HTML page using Leptos SSR
fn render_page(title: &str, current_path: &str, index: &DocIndex) -> String {
    use leptos::prelude::*;

    let title = title.to_string();
    let current_path = current_path.to_string();
    let index = index.clone();

    let owner = Owner::new();
    owner.with(|| {
        view! {
            <DocPage
                title=title
                index=index
                current_path=current_path
            />
        }
        .to_html()
    })
}

/// Shared page template used by both normal pages and 404
fn render_page_template(title: &str, sidebar: &str, content: &str) -> String {
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
    <div class="doc-layout">
        <nav class="doc-sidebar">
            <div class="sidebar-header">
                <h1><a href="/">Fe Docs</a></h1>
                <label class="auto-follow-toggle" title="Auto-follow cursor position in editor">
                    <input type="checkbox" id="auto-follow" onchange="toggleAutoFollow(this.checked)">
                    <span class="toggle-slider"></span>
                    <span class="toggle-label">Follow cursor</span>
                </label>
                <input type="search" id="search" placeholder="Search..." onkeyup="doSearch(this.value)">
                <div id="search-results"></div>
            </div>
            <div class="sidebar-nav">
                {sidebar}
            </div>
        </nav>
        <main class="doc-content">
            {content}
        </main>
    </div>
    {script}
</body>
</html>"#,
        title = title,
        css = CSS,
        sidebar = sidebar,
        content = content,
        script = SCRIPT,
    )
}

/// Render a CSR shell page that loads WASM and embeds initial data
fn render_csr_shell(title: &str, _initial_path: &str, index: &DocIndex) -> String {
    // Serialize the index to JSON for embedding
    let index_json = serde_json::to_string(index).unwrap_or_else(|_| "{}".to_string());

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
    <!-- Initial data embedded for WASM app -->
    <script id="__FE_DOC_DATA__" type="application/json">{index_json}</script>

    <!-- WASM loader -->
    <script type="module">
        try {{
            const loading = document.getElementById('loading');
            loading.textContent = 'Loading WASM module...';
            const mod = await import('/assets/fe_doc_viewer.js');
            loading.textContent = 'Initializing WASM...';
            await mod.default();
            loading.textContent = 'WASM initialized, mounting app...';
        }} catch (e) {{
            console.error('WASM load error:', e);
            document.getElementById('loading').innerHTML =
                '<div style="color: #ff6b6b;">Error loading documentation viewer:</div>' +
                '<pre style="color: #a0a0a0; font-size: 12px; text-align: left; padding: 1rem; background: #1a1a2e; border-radius: 4px; overflow: auto;">' +
                e.toString() + '\\n\\n' + (e.stack || '') + '</pre>';
        }}
    </script>

    <!-- Fallback loading indicator (replaced by WASM app) -->
    <noscript>
        <div style="padding: 2rem; text-align: center; color: #a0a0a0;">
            This documentation viewer requires JavaScript and WebAssembly support.
        </div>
    </noscript>
    <div id="loading" style="padding: 2rem; text-align: center; color: #a0a0a0;">
        Loading documentation...
    </div>
</body>
</html>"#,
        title = title,
        css = CSS,
        index_json = index_json,
    )
}

fn render_sidebar(index: &DocIndex, current_path: &str) -> String {
    let mut html = String::new();

    for module in &index.modules {
        let is_current = module.path == current_path;
        let class = if is_current { "nav-item current" } else { "nav-item" };
        html.push_str(&format!(
            r#"<div class="{class}"><a href="/doc/{path}" class="nav-module">{name}</a>"#,
            class = class,
            path = module.path,
            name = module.name,
        ));

        // Show items in this module
        if !module.items.is_empty() {
            html.push_str("<ul class=\"nav-items\">");
            for item in &module.items {
                let item_current = item.path == current_path;
                let item_class = if item_current { "current" } else { "" };
                html.push_str(&format!(
                    r#"<li class="{class}"><a href="/doc/{path}"><span class="kind-badge {kind}">{kind}</span> {name}</a></li>"#,
                    class = item_class,
                    path = item.path,
                    kind = item.kind.display_name(),
                    name = item.name,
                ));
            }
            html.push_str("</ul>");
        }

        html.push_str("</div>");
    }

    html
}

/// Semantic ordering for item kinds (lower = appears first)
fn kind_order(kind: DocItemKind) -> u8 {
    match kind {
        DocItemKind::Module => 0,
        DocItemKind::Trait => 1,
        DocItemKind::Contract => 2,
        DocItemKind::Struct => 3,
        DocItemKind::Enum => 4,
        DocItemKind::TypeAlias => 5,
        DocItemKind::Function => 6,
        DocItemKind::Const => 7,
        DocItemKind::Impl => 8,
        DocItemKind::ImplTrait => 9,
    }
}

fn render_index(index: &DocIndex) -> String {
    let mut html = String::from("<h1>Fe Documentation</h1>");

    // Group items by kind
    let mut by_kind: std::collections::HashMap<DocItemKind, Vec<&DocItem>> = std::collections::HashMap::new();
    for item in &index.items {
        if !matches!(item.kind, DocItemKind::Module) {
            by_kind.entry(item.kind).or_default().push(item);
        }
    }

    // Sort kind groups by semantic order
    let mut kinds: Vec<_> = by_kind.keys().cloned().collect();
    kinds.sort_by_key(|k| kind_order(*k));

    for kind in kinds {
        let mut items: Vec<_> = by_kind.get(&kind).unwrap().iter().cloned().collect();
        items.sort_by(|a, b| a.name.cmp(&b.name));

        let kind_name = kind.display_name();
        html.push_str(&format!("<h2>{kind_name}s</h2><ul class=\"item-list\">"));
        for item in items {
            let summary = item.docs.as_ref()
                .map(|d| format!(" - {}", &d.summary))
                .unwrap_or_default();
            html.push_str(&format!(
                r#"<li><a href="/doc/{path}">{name}</a>{summary}</li>"#,
                path = item.path,
                name = item.name,
                summary = html_escape(&summary),
            ));
        }
        html.push_str("</ul>");
    }

    html
}

fn render_item(item: &DocItem) -> String {
    let mut html = String::new();

    // Header with kind badge
    html.push_str(&format!(
        r#"<div class="item-header">
            <span class="kind-badge {kind}">{kind}</span>
            <h1>{name}</h1>
        </div>"#,
        kind = item.kind.display_name(),
        name = item.name,
    ));

    // Signature
    html.push_str(&format!(
        r#"<pre class="signature"><code>{sig}</code></pre>"#,
        sig = html_escape(&item.signature),
    ));

    // Documentation
    if let Some(docs) = &item.docs {
        html.push_str("<div class=\"docs\">");
        html.push_str(&crate::markdown::render_markdown(&docs.body));
        html.push_str("</div>");
    }

    // Children (fields, variants, etc.)
    if !item.children.is_empty() {
        html.push_str("<h2>Members</h2><dl class=\"members\">");
        for child in &item.children {
            let kind_name = child.kind.display_name().to_lowercase();
            html.push_str(&format!(
                r#"<dt><span class="kind-badge {kind}">{kind}</span> <code>{name}</code></dt>"#,
                kind = kind_name,
                name = child.name,
            ));
            if let Some(docs) = &child.docs {
                html.push_str(&format!("<dd>{}</dd>", html_escape(docs)));
            }
        }
        html.push_str("</dl>");
    }

    // Source location with "Go to Source" button (for LSP mode)
    if let Some(source) = &item.source {
        html.push_str(&format!(
            r#"<div class="source-link">
                Defined in <code>{file}</code>
                <button class="goto-source-btn" onclick="gotoSource('{path}')" title="Open in editor">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"></path>
                        <polyline points="15 3 21 3 21 9"></polyline>
                        <line x1="10" y1="14" x2="21" y2="3"></line>
                    </svg>
                    Go to Source
                </button>
            </div>"#,
            file = source.file,
            path = item.path,
        ));
    }

    html
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

const CSS: &str = r#"
:root {
    --bg: #1a1a2e;
    --bg-secondary: #16213e;
    --text: #e4e4e4;
    --text-muted: #a0a0a0;
    --accent: #0f9d58;
    --accent-hover: #0b7a43;
    --border: #2a2a4a;
    --code-bg: #0d1117;
}

* { box-sizing: border-box; margin: 0; padding: 0; }

body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    background: var(--bg);
    color: var(--text);
    line-height: 1.6;
}

.doc-layout {
    display: grid;
    grid-template-columns: 280px 1fr;
    min-height: 100vh;
}

.doc-sidebar {
    background: var(--bg-secondary);
    border-right: 1px solid var(--border);
    padding: 1rem;
    position: sticky;
    top: 0;
    height: 100vh;
    overflow-y: auto;
}

.sidebar-header h1 {
    font-size: 1.25rem;
    margin-bottom: 1rem;
}

.sidebar-header h1 a {
    color: var(--accent);
    text-decoration: none;
}

.auto-follow-toggle {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    margin-bottom: 0.75rem;
    cursor: pointer;
    font-size: 0.8rem;
    color: var(--text-muted);
}

.auto-follow-toggle input {
    display: none;
}

.toggle-slider {
    position: relative;
    width: 36px;
    height: 20px;
    background: var(--border);
    border-radius: 10px;
    transition: background 0.2s;
}

.toggle-slider::after {
    content: '';
    position: absolute;
    top: 2px;
    left: 2px;
    width: 16px;
    height: 16px;
    background: var(--text-muted);
    border-radius: 50%;
    transition: transform 0.2s, background 0.2s;
}

.auto-follow-toggle input:checked + .toggle-slider {
    background: var(--accent);
}

.auto-follow-toggle input:checked + .toggle-slider::after {
    transform: translateX(16px);
    background: white;
}

.toggle-label {
    user-select: none;
}

#search {
    width: 100%;
    padding: 0.5rem;
    background: var(--bg);
    border: 1px solid var(--border);
    border-radius: 4px;
    color: var(--text);
    margin-bottom: 0.5rem;
}

#search-results {
    display: flex;
    flex-direction: column;
    gap: 2px;
}

.search-result {
    display: flex;
    gap: 0.5rem;
    padding: 0.25rem 0.5rem;
    background: var(--bg);
    border-radius: 4px;
    text-decoration: none;
    color: var(--text);
    font-size: 0.875rem;
}

.search-result:hover {
    background: var(--border);
}

.sidebar-nav {
    margin-top: 1rem;
}

.nav-item {
    margin-bottom: 0.5rem;
}

.nav-module {
    color: var(--text);
    text-decoration: none;
    font-weight: 600;
}

.nav-module:hover {
    color: var(--accent);
}

.nav-items {
    list-style: none;
    margin-left: 1rem;
    margin-top: 0.25rem;
}

.nav-items li {
    margin: 0.25rem 0;
}

.nav-items a {
    color: var(--text-muted);
    text-decoration: none;
    font-size: 0.875rem;
    display: flex;
    align-items: center;
    gap: 0.5rem;
}

.nav-items a:hover, .nav-items li.current a {
    color: var(--text);
}

.doc-content {
    padding: 2rem 3rem;
    max-width: 900px;
}

.item-header {
    display: flex;
    align-items: center;
    gap: 1rem;
    margin-bottom: 1rem;
}

.item-header h1 {
    font-size: 2rem;
}

.kind-badge {
    font-size: 0.75rem;
    padding: 0.125rem 0.5rem;
    border-radius: 4px;
    background: var(--border);
    color: var(--text-muted);
    text-transform: lowercase;
}

.kind-badge.struct { background: #2d4a3e; color: #7ee787; }
.kind-badge.enum { background: #3d3a2d; color: #e7c77e; }
.kind-badge.function, .kind-badge.fn { background: #2d3a4a; color: #7eb8e7; }
.kind-badge.trait { background: #4a2d4a; color: #e77ee7; }
.kind-badge.module, .kind-badge.mod { background: #2d2d4a; color: #9d7ee7; }

.signature {
    background: var(--code-bg);
    padding: 1rem;
    border-radius: 8px;
    overflow-x: auto;
    margin-bottom: 1.5rem;
}

.signature code {
    font-family: "JetBrains Mono", "Fira Code", monospace;
    font-size: 0.9rem;
    white-space: pre;
}

.docs {
    margin-bottom: 2rem;
}

.docs p {
    margin-bottom: 1rem;
}

.docs code {
    background: var(--code-bg);
    padding: 0.125rem 0.375rem;
    border-radius: 4px;
    font-family: "JetBrains Mono", "Fira Code", monospace;
    font-size: 0.875em;
}

h2 {
    font-size: 1.25rem;
    margin: 1.5rem 0 1rem;
    padding-bottom: 0.5rem;
    border-bottom: 1px solid var(--border);
}

.members {
    display: grid;
    gap: 0.75rem;
}

.members dt {
    display: flex;
    align-items: center;
    gap: 0.5rem;
}

.members dd {
    margin-left: 2rem;
    color: var(--text-muted);
    margin-bottom: 0.5rem;
}

.item-list {
    list-style: none;
}

.item-list li {
    margin: 0.5rem 0;
}

.item-list a {
    color: var(--accent);
    text-decoration: none;
}

.item-list a:hover {
    text-decoration: underline;
}

.source-link {
    margin-top: 2rem;
    padding-top: 1rem;
    border-top: 1px solid var(--border);
    color: var(--text-muted);
    font-size: 0.875rem;
    display: flex;
    align-items: center;
    gap: 1rem;
    flex-wrap: wrap;
}

.goto-source-btn {
    display: inline-flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.4rem 0.8rem;
    background: var(--accent);
    color: white;
    border: none;
    border-radius: 4px;
    font-size: 0.8rem;
    cursor: pointer;
    transition: background 0.2s;
}

.goto-source-btn:hover {
    background: var(--accent-hover);
}

.goto-source-btn svg {
    flex-shrink: 0;
}

.not-found {
    text-align: center;
    padding: 4rem 2rem;
}

.not-found h1 {
    font-size: 2rem;
    margin-bottom: 1rem;
    color: var(--text);
}

.not-found p {
    color: var(--text-muted);
    margin-bottom: 0.5rem;
}

.not-found code {
    background: var(--code-bg);
    padding: 0.25rem 0.5rem;
    border-radius: 4px;
    font-family: "JetBrains Mono", "Fira Code", monospace;
}

.not-found-hint {
    font-size: 0.875rem;
    font-style: italic;
}

/* CSR Component Styles */
.doc-nav-entry {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.25rem 0.5rem;
    text-decoration: none;
    color: var(--text-muted);
    font-size: 0.875rem;
    border-radius: 4px;
}

.doc-nav-entry:hover {
    background: var(--bg);
    color: var(--text);
}

.doc-nav-entry.active {
    background: var(--accent);
    color: white;
}

.doc-nav-entry.module {
    font-weight: 600;
    color: var(--text);
}

.doc-nav-name {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
}

.doc-kind-badge {
    font-size: 0.7rem;
    padding: 0.1rem 0.4rem;
    border-radius: 3px;
    background: var(--border);
    color: var(--text-muted);
    text-transform: lowercase;
    flex-shrink: 0;
}

.doc-kind-badge.struct { background: #2d4a3e; color: #7ee787; }
.doc-kind-badge.enum { background: #3d3a2d; color: #e7c77e; }
.doc-kind-badge.function, .doc-kind-badge.fn { background: #2d3a4a; color: #7eb8e7; }
.doc-kind-badge.trait { background: #4a2d4a; color: #e77ee7; }
.doc-kind-badge.module, .doc-kind-badge.mod { background: #2d2d4a; color: #9d7ee7; }
.doc-kind-badge.contract { background: #4a3d2d; color: #e7b87e; }
.doc-kind-badge.type { background: #2d4a4a; color: #7ee7e7; }
.doc-kind-badge.const { background: #3a2d4a; color: #b87ee7; }

.doc-search {
    position: relative;
    margin-bottom: 1.5rem;
}

.doc-search-input {
    width: 100%;
    padding: 0.75rem 1rem;
    background: var(--bg-secondary);
    border: 1px solid var(--border);
    border-radius: 6px;
    color: var(--text);
    font-size: 1rem;
}

.doc-search-input:focus {
    outline: none;
    border-color: var(--accent);
}

.doc-search-results {
    position: absolute;
    top: 100%;
    left: 0;
    right: 0;
    background: var(--bg-secondary);
    border: 1px solid var(--border);
    border-radius: 6px;
    margin-top: 0.25rem;
    max-height: 300px;
    overflow-y: auto;
    z-index: 100;
}

.doc-search-result {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    width: 100%;
    padding: 0.5rem 0.75rem;
    background: none;
    border: none;
    color: var(--text);
    text-align: left;
    cursor: pointer;
}

.doc-search-result:hover {
    background: var(--bg);
}

.doc-search-result-name {
    font-weight: 500;
}

.doc-search-result-path {
    color: var(--text-muted);
    font-size: 0.8rem;
    margin-left: auto;
}

.doc-item {
    max-width: 800px;
}

.doc-item-header {
    margin-bottom: 1.5rem;
}

.doc-item-title {
    display: flex;
    align-items: center;
    gap: 0.75rem;
}

.doc-item-title h1 {
    font-size: 1.75rem;
    margin: 0;
}

.doc-item-path {
    color: var(--text-muted);
    font-family: monospace;
    font-size: 0.875rem;
    margin-top: 0.25rem;
}

.doc-signature {
    margin-bottom: 1.5rem;
}

.doc-code-block {
    background: var(--code-bg);
    padding: 1rem;
    border-radius: 6px;
    overflow-x: auto;
    margin: 0;
}

.doc-code-block code {
    font-family: "JetBrains Mono", "Fira Code", monospace;
    font-size: 0.9rem;
    white-space: pre;
}

.doc-body {
    line-height: 1.7;
}

.doc-body p {
    margin-bottom: 1rem;
}

.doc-body code {
    background: var(--code-bg);
    padding: 0.125rem 0.375rem;
    border-radius: 4px;
    font-family: monospace;
    font-size: 0.9em;
}

.doc-children {
    margin-top: 2rem;
}

.doc-child-section {
    margin-bottom: 2rem;
}

.doc-child-section h2 {
    font-size: 1.25rem;
    margin-bottom: 1rem;
    padding-bottom: 0.5rem;
    border-bottom: 1px solid var(--border);
}

.doc-child-list {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
}

.doc-child-item {
    background: var(--bg-secondary);
    border-radius: 6px;
    padding: 0.75rem 1rem;
}

.doc-child-header {
    display: flex;
    align-items: center;
    gap: 0.5rem;
}

.doc-child-signature {
    font-family: monospace;
    font-size: 0.9rem;
}

.doc-expand-btn {
    background: none;
    border: none;
    color: var(--text-muted);
    cursor: pointer;
    padding: 0;
    font-size: 0.8rem;
}

.doc-child-docs {
    margin-top: 0.75rem;
    padding-top: 0.75rem;
    border-top: 1px solid var(--border);
    color: var(--text-muted);
    font-size: 0.9rem;
}

.doc-visibility-badge {
    font-size: 0.7rem;
    padding: 0.1rem 0.4rem;
    border-radius: 3px;
    background: #4a2d2d;
    color: #e77e7e;
}

.doc-source {
    margin-top: 2rem;
    padding-top: 1rem;
    border-top: 1px solid var(--border);
    color: var(--text-muted);
    font-size: 0.875rem;
}

.doc-source a {
    color: var(--accent);
    text-decoration: none;
}

.doc-source a:hover {
    text-decoration: underline;
}

.doc-not-found {
    text-align: center;
    padding: 4rem 2rem;
    color: var(--text-muted);
}

.doc-not-found h1 {
    color: var(--text);
    margin-bottom: 1rem;
}
"#;

const SCRIPT: &str = r#"<script>
    // Auto-follow toggle
    function toggleAutoFollow(checked) {
        localStorage.setItem('fe-docs-auto-follow', checked);
    }

    // Go to source in editor (for LSP mode)
    function gotoSource(path) {
        fetch('/api/goto/' + encodeURIComponent(path), { method: 'POST' })
            .then(response => {
                if (!response.ok) {
                    console.warn('Go to source not available (not running through LSP)');
                }
            })
            .catch(err => {
                console.warn('Go to source failed:', err);
            });
    }

    // Restore auto-follow state on page load
    document.addEventListener('DOMContentLoaded', function() {
        const autoFollow = document.getElementById('auto-follow');
        if (autoFollow) {
            const saved = localStorage.getItem('fe-docs-auto-follow');
            autoFollow.checked = saved === 'true';
        }
    });

    // Search functionality
    let searchTimeout;
    function doSearch(query) {
        clearTimeout(searchTimeout);
        const results = document.getElementById('search-results');
        if (!query) {
            results.innerHTML = '';
            return;
        }
        searchTimeout = setTimeout(async () => {
            const resp = await fetch('/api/search?q=' + encodeURIComponent(query));
            const items = await resp.json();
            results.innerHTML = items.map(item =>
                `<a class="search-result" href="/doc/${item.path}">
                    <span class="kind-badge ${item.kind}">${item.kind}</span>
                    ${item.name}
                </a>`
            ).join('');
        }, 150);
    }

    // WebSocket live reload
    (function() {
        const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const ws = new WebSocket(wsProtocol + '//' + window.location.host + '/ws');

        ws.onmessage = function(event) {
            try {
                const msg = JSON.parse(event.data);
                if (msg.type === 'reload') {
                    window.location.reload();
                } else if (msg.type === 'update') {
                    // Re-fetch current page and replace content
                    fetch(window.location.pathname)
                        .then(r => r.text())
                        .then(html => {
                            const parser = new DOMParser();
                            const newDoc = parser.parseFromString(html, 'text/html');
                            // Replace main content
                            const newContent = newDoc.querySelector('.doc-content');
                            const oldContent = document.querySelector('.doc-content');
                            if (newContent && oldContent) {
                                oldContent.innerHTML = newContent.innerHTML;
                            }
                            // Update sidebar (items may have changed)
                            const newSidebar = newDoc.querySelector('.sidebar-nav');
                            const oldSidebar = document.querySelector('.sidebar-nav');
                            if (newSidebar && oldSidebar) {
                                oldSidebar.innerHTML = newSidebar.innerHTML;
                            }
                        });
                } else if (msg.type === 'navigate') {
                    const currentPath = window.location.pathname.replace('/doc/', '');

                    // Rename redirects (with if_on_path) always work if we're on the old page
                    if (msg.if_on_path) {
                        if (currentPath === msg.if_on_path) {
                            window.location.href = '/doc/' + msg.path;
                        }
                        return; // Don't continue to auto-follow check for redirects
                    }

                    // Regular navigation only works with auto-follow enabled
                    const autoFollow = document.getElementById('auto-follow');
                    if (autoFollow && autoFollow.checked) {
                        window.location.href = '/doc/' + msg.path;
                    }
                }
            } catch (e) {
                console.error('WebSocket message error:', e);
            }
        };

        ws.onclose = function() {
            // Attempt to reconnect after 2 seconds
            setTimeout(function() {
                window.location.reload();
            }, 2000);
        };
    })();
</script>"#;

/// Render a 404 page using Leptos SSR (keeps WebSocket/auto-follow working)
fn render_page_not_found(path: &str, index: &DocIndex) -> String {
    use leptos::prelude::*;

    let path = path.to_string();
    let index = index.clone();

    let owner = Owner::new();
    owner.with(|| {
        view! {
            <DocNotFoundSSR
                path=path
                index=index
            />
        }
        .to_html()
    })
}

/// Configuration for the documentation server
pub struct DocServerConfig {
    /// Port to listen on
    pub port: u16,
    /// Host to bind to
    pub host: String,
    /// Path to static assets
    pub assets_path: Option<String>,
    /// Enable CSR mode (client-side rendering with WASM)
    pub csr_mode: bool,
}

impl Default for DocServerConfig {
    fn default() -> Self {
        Self {
            port: 8080,
            host: "127.0.0.1".to_string(),
            assets_path: None,
            csr_mode: false,
        }
    }
}

/// Start the documentation server
pub async fn serve(index: DocIndex, config: DocServerConfig) -> Result<(), std::io::Error> {
    let mut state = DocServerState::new(index).with_csr(config.csr_mode);
    if let Some(assets_path) = config.assets_path {
        state = state.with_assets(assets_path);
    }

    let app = doc_router(Arc::new(state));

    let addr = format!("{}:{}", config.host, config.port);
    let listener = tokio::net::TcpListener::bind(&addr).await?;

    if config.csr_mode {
        tracing::info!("Documentation server (CSR mode) listening on http://{}", addr);
    } else {
        tracing::info!("Documentation server listening on http://{}", addr);
    }

    axum::serve(listener, app).await
}

/// Public function for LSP integration - renders a doc page
pub fn render_page_for_lsp(title: &str, current_path: &str, index: &DocIndex) -> String {
    render_page(title, current_path, index)
}

/// Public function for LSP integration - renders an item page
pub fn render_item_for_lsp(item: &DocItem) -> String {
    render_item(item)
}

/// Public function for LSP integration - renders a 404 page
pub fn render_page_not_found_for_lsp(path: &str, index: &DocIndex) -> String {
    render_page_not_found(path, index)
}

/// Public function for LSP integration - renders a CSR shell page
pub fn render_csr_shell_for_lsp(title: &str, path: &str, index: &DocIndex) -> String {
    render_csr_shell(title, path, index)
}
