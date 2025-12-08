//! HTTP server for serving Fe documentation
//!
//! This module provides an axum-based HTTP server that integrates with the
//! language server's Salsa database for real-time documentation.
//!
//! # Features
//!
//! - Static file serving for assets (CSS, JS)
//! - Dynamic documentation rendering
//! - Live search API
//! - WebSocket support for live updates (future)

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

/// Application state shared across handlers
pub struct DocServerState {
    /// The documentation index
    pub index: DocIndex,
    /// Path to static assets
    pub assets_path: Option<String>,
}

impl DocServerState {
    pub fn new(index: DocIndex) -> Self {
        Self {
            index,
            assets_path: None,
        }
    }

    pub fn with_assets(mut self, path: impl Into<String>) -> Self {
        self.assets_path = Some(path.into());
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
        .with_state(state.clone());

    // Serve static assets if configured
    if let Some(ref assets_path) = state.assets_path {
        router = router.nest_service("/assets", ServeDir::new(assets_path));
    }

    router
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

    Html(render_page(title, &root_path, &state.index))
}

/// Documentation item handler
async fn doc_item_handler(
    State(state): State<Arc<DocServerState>>,
    Path(path): Path<String>,
) -> (StatusCode, Html<String>) {
    if let Some(item) = state.index.find_by_path(&path) {
        let title = format!("{} - Fe Documentation", item.name);
        (StatusCode::OK, Html(render_page(&title, &path, &state.index)))
    } else {
        (StatusCode::NOT_FOUND, Html(render_not_found(&path)))
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

/// Render a full HTML page
fn render_page(title: &str, current_path: &str, index: &DocIndex) -> String {
    let sidebar = render_sidebar(index, current_path);
    let content = if let Some(item) = index.find_by_path(current_path) {
        render_item(item)
    } else {
        render_index(index)
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
    <div class="doc-layout">
        <nav class="doc-sidebar">
            <div class="sidebar-header">
                <h1><a href="/">Fe Docs</a></h1>
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
    <script>
        async function doSearch(q) {{
            const results = document.getElementById('search-results');
            if (!q || q.length < 2) {{
                results.innerHTML = '';
                return;
            }}
            const resp = await fetch('/api/search?q=' + encodeURIComponent(q));
            const items = await resp.json();
            results.innerHTML = items.slice(0, 10).map(item =>
                `<a href="/doc/${{item.path}}" class="search-result">
                    <span class="kind">${{item.kind}}</span>
                    <span class="name">${{item.name}}</span>
                </a>`
            ).join('');
        }}

        // Live reload via WebSocket (only works when served by LSP)
        (function() {{
            let wsConnected = false;
            let retryCount = 0;
            const maxRetries = 3;

            function setupLiveReload() {{
                const wsUrl = 'ws://' + window.location.host + '/ws';
                const ws = new WebSocket(wsUrl);

                ws.onopen = () => {{
                    wsConnected = true;
                    retryCount = 0;
                    console.log('Live reload connected');
                }};

                ws.onmessage = async (event) => {{
                    if (event.data === 'reload') {{
                        console.log('Docs updated, reloading content...');
                        try {{
                            const resp = await fetch(window.location.href);
                            const html = await resp.text();
                            const parser = new DOMParser();
                            const doc = parser.parseFromString(html, 'text/html');

                            const newMain = doc.querySelector('.doc-content');
                            const main = document.querySelector('.doc-content');
                            if (newMain && main) main.innerHTML = newMain.innerHTML;

                            const newSidebar = doc.querySelector('.sidebar-nav');
                            const sidebar = document.querySelector('.sidebar-nav');
                            if (newSidebar && sidebar) sidebar.innerHTML = newSidebar.innerHTML;

                            console.log('Content updated!');
                        }} catch (e) {{
                            console.log('Failed to reload content:', e);
                        }}
                    }}
                }};

                ws.onclose = () => {{
                    if (wsConnected) {{
                        // Was connected before, reconnect
                        console.log('Live reload disconnected, reconnecting...');
                        setTimeout(setupLiveReload, 2000);
                    }} else if (retryCount < maxRetries) {{
                        // Never connected, retry a few times
                        retryCount++;
                        setTimeout(setupLiveReload, 1000);
                    }}
                    // After max retries without connecting, stop (CLI mode)
                }};

                ws.onerror = () => ws.close();
            }}
            setupLiveReload();
        }})();
    </script>
</body>
</html>"#,
        title = title,
        css = CSS,
        sidebar = sidebar,
        content = content,
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

fn render_index(index: &DocIndex) -> String {
    let mut html = String::from("<h1>Fe Documentation</h1>");

    // Group items by kind
    let mut by_kind: std::collections::HashMap<&str, Vec<&DocItem>> = std::collections::HashMap::new();
    for item in &index.items {
        if !matches!(item.kind, DocItemKind::Module) {
            by_kind.entry(item.kind.display_name()).or_default().push(item);
        }
    }

    for (kind, items) in by_kind.iter() {
        html.push_str(&format!("<h2>{kind}s</h2><ul class=\"item-list\">"));
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
        html.push_str(&crate::render::markdown::render_markdown(&docs.body));
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

    // Source location
    if let Some(source) = &item.source {
        html.push_str(&format!(
            r#"<div class="source-link">Defined in <code>{file}</code></div>"#,
            file = source.file,
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
}
"#;

/// Render a 404 page
fn render_not_found(path: &str) -> String {
    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Not Found - Fe Documentation</title>
    <link rel="stylesheet" href="/assets/docs.css">
</head>
<body>
    <div class="doc-not-found">
        <h1>Item Not Found</h1>
        <p>The documentation item "{path}" could not be found.</p>
        <a href="/">Return to documentation index</a>
    </div>
</body>
</html>"#,
        path = path,
    )
}

/// Configuration for the documentation server
pub struct DocServerConfig {
    /// Port to listen on
    pub port: u16,
    /// Host to bind to
    pub host: String,
    /// Path to static assets
    pub assets_path: Option<String>,
}

impl Default for DocServerConfig {
    fn default() -> Self {
        Self {
            port: 8080,
            host: "127.0.0.1".to_string(),
            assets_path: None,
        }
    }
}

/// Start the documentation server
pub async fn serve(index: DocIndex, config: DocServerConfig) -> Result<(), std::io::Error> {
    let mut state = DocServerState::new(index);
    if let Some(assets_path) = config.assets_path {
        state = state.with_assets(assets_path);
    }

    let app = doc_router(Arc::new(state));

    let addr = format!("{}:{}", config.host, config.port);
    let listener = tokio::net::TcpListener::bind(&addr).await?;

    tracing::info!("Documentation server listening on http://{}", addr);

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
