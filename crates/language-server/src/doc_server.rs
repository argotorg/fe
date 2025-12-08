//! Documentation server integration for the Fe language server
//!
//! This module handles:
//! - Starting an HTTP server for documentation browsing
//! - Writing server info files for CLI discovery
//! - Custom LSP methods for doc access

use std::sync::Arc;
use tokio::sync::RwLock;

use doc_engine::DocIndex;
use serde::{Deserialize, Serialize};
use tracing::{error, info};

/// Server info written to `.fe-lsp.json` for CLI discovery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LspServerInfo {
    pub pid: u32,
    pub workspace_root: Option<String>,
    pub docs_url: Option<String>,
}

impl LspServerInfo {
    pub fn new() -> Self {
        Self {
            pid: std::process::id(),
            workspace_root: None,
            docs_url: None,
        }
    }

    /// Write server info to the workspace root
    pub fn write_to_workspace(&self, workspace_root: &std::path::Path) -> std::io::Result<()> {
        let info_path = workspace_root.join(".fe-lsp.json");
        let json = serde_json::to_string_pretty(self)?;
        std::fs::write(&info_path, json)?;
        info!("Wrote server info to {:?}", info_path);
        Ok(())
    }

    /// Read server info from a workspace
    pub fn read_from_workspace(workspace_root: &std::path::Path) -> Option<Self> {
        let info_path = workspace_root.join(".fe-lsp.json");
        let json = std::fs::read_to_string(&info_path).ok()?;
        serde_json::from_str(&json).ok()
    }

    /// Clean up server info file
    pub fn remove_from_workspace(workspace_root: &std::path::Path) {
        let info_path = workspace_root.join(".fe-lsp.json");
        let _ = std::fs::remove_file(&info_path);
    }
}

/// State for the documentation server running alongside LSP
pub struct DocServerHandle {
    pub port: u16,
    pub url: String,
    index: Arc<RwLock<DocIndex>>,
}

impl DocServerHandle {
    /// Start the doc server on a random available port
    pub async fn start() -> Result<Self, std::io::Error> {
        // Bind to port 0 to get a random available port
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let port = listener.local_addr()?.port();
        let url = format!("http://127.0.0.1:{}", port);

        let index = Arc::new(RwLock::new(DocIndex::default()));
        let index_for_server = index.clone();

        // Start the server in a background task
        tokio::spawn(async move {
            let state = Arc::new(DocServerStateWrapper {
                index: index_for_server,
            });

            let app = doc_router_dynamic(state);

            info!("Documentation server listening on http://127.0.0.1:{}", port);

            if let Err(e) = axum::serve(listener, app).await {
                error!("Doc server error: {}", e);
            }
        });

        Ok(Self { port, url, index })
    }

    /// Update the documentation index
    pub async fn update_index(&self, new_index: DocIndex) {
        let mut index = self.index.write().await;
        *index = new_index;
        info!("Updated documentation index with {} items", index.items.len());
    }
}

/// Wrapper for dynamic doc index updates
struct DocServerStateWrapper {
    index: Arc<RwLock<DocIndex>>,
}

/// Create a router that reads from a dynamic index
fn doc_router_dynamic(state: Arc<DocServerStateWrapper>) -> axum::Router {
    use axum::{
        extract::{Path, Query, State},
        http::StatusCode,
        response::{Html, IntoResponse, Json},
        routing::get,
    };

    async fn index_handler(
        State(state): State<Arc<DocServerStateWrapper>>,
    ) -> impl IntoResponse {
        let index = state.index.read().await;
        Html(render_page_dynamic("Fe Documentation", "", &index))
    }

    async fn doc_item_handler(
        State(state): State<Arc<DocServerStateWrapper>>,
        Path(path): Path<String>,
    ) -> (StatusCode, Html<String>) {
        let index = state.index.read().await;
        if let Some(item) = index.find_by_path(&path) {
            let title = format!("{} - Fe Documentation", item.name);
            (StatusCode::OK, Html(render_page_dynamic(&title, &path, &index)))
        } else {
            (StatusCode::NOT_FOUND, Html(format!("Not found: {}", path)))
        }
    }

    #[derive(serde::Deserialize)]
    struct SearchQuery {
        q: String,
    }

    async fn search_handler(
        State(state): State<Arc<DocServerStateWrapper>>,
        Query(query): Query<SearchQuery>,
    ) -> impl IntoResponse {
        let index = state.index.read().await;
        let results: Vec<_> = index
            .search(&query.q)
            .into_iter()
            .take(20)
            .cloned()
            .collect();
        Json(results)
    }

    async fn item_api_handler(
        State(state): State<Arc<DocServerStateWrapper>>,
        Path(path): Path<String>,
    ) -> impl IntoResponse {
        let index = state.index.read().await;
        if let Some(item) = index.find_by_path(&path) {
            Json(Some(item.clone()))
        } else {
            Json(None)
        }
    }

    axum::Router::new()
        .route("/", get(index_handler))
        .route("/doc/{*path}", get(doc_item_handler))
        .route("/api/search", get(search_handler))
        .route("/api/item/{*path}", get(item_api_handler))
        .with_state(state)
}

// Re-use the rendering from doc_engine::server but with dynamic index
fn render_page_dynamic(title: &str, current_path: &str, index: &DocIndex) -> String {
    // Import the CSS and rendering from doc_engine
    doc_engine::server::render_page_for_lsp(title, current_path, index)
}
