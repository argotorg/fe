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

use crate::model::DocIndex;

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
    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{title}</title>
    <link rel="stylesheet" href="/assets/docs.css">
</head>
<body>
    <div id="app" data-path="{current_path}">
        <!-- Leptos will hydrate this -->
        <noscript>
            <p>This documentation requires JavaScript for full functionality.</p>
        </noscript>
    </div>
    <script>
        // Initial state for hydration
        window.__FE_DOC_INDEX__ = {index_json};
        window.__FE_DOC_PATH__ = "{current_path}";
    </script>
</body>
</html>"#,
        title = title,
        current_path = current_path,
        index_json = serde_json::to_string(index).unwrap_or_default(),
    )
}

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
