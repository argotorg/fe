//! Documentation server integration for the Fe language server
//!
//! This module handles:
//! - Starting an HTTP server for documentation browsing
//! - Writing server info files for CLI discovery
//! - WebSocket-based live updates
//! - Custom LSP methods for doc access

use std::sync::Arc;
use tokio::sync::{RwLock, broadcast};

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

/// Message sent to WebSocket clients
#[derive(Debug, Clone, Serialize)]
pub struct DocMessage {
    #[serde(rename = "type")]
    pub msg_type: String,
    /// Target path for navigation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    /// Only navigate if currently on this path (for renames)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub if_on_path: Option<String>,
}

/// State for the documentation server running alongside LSP
pub struct DocServerHandle {
    pub port: u16,
    pub url: String,
    index: Arc<RwLock<DocIndex>>,
    /// Broadcast channel to notify connected clients of updates
    update_tx: broadcast::Sender<String>,
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

        // Create broadcast channel for live updates (now sends JSON strings)
        let (update_tx, _) = broadcast::channel::<String>(16);
        let update_tx_for_server = update_tx.clone();

        // Start the server in a background task
        tokio::spawn(async move {
            let state = Arc::new(DocServerStateWrapper {
                index: index_for_server,
                update_tx: update_tx_for_server,
            });

            let app = doc_router_dynamic(state);

            info!("Documentation server listening on http://127.0.0.1:{}", port);

            if let Err(e) = axum::serve(listener, app).await {
                error!("Doc server error: {}", e);
            }
        });

        Ok(Self { port, url, index, update_tx })
    }

    /// Update the documentation index and notify connected clients
    pub async fn update_index(&self, new_index: DocIndex) {
        let mut index = self.index.write().await;
        *index = new_index;
        info!("Updated documentation index with {} items", index.items.len());

        // Send update message to WebSocket clients
        let msg = DocMessage {
            msg_type: "update".to_string(),
            path: None,
            if_on_path: None,
        };
        let _ = self.update_tx.send(serde_json::to_string(&msg).unwrap_or_default());
    }

    /// Notify browser to navigate to a path
    /// If `if_on_path` is Some, only navigate if browser is currently on that path (for renames)
    pub fn notify_navigate(&self, path: &str, if_on_path: Option<&str>) {
        if let Some(from) = if_on_path {
            info!("Notifying redirect: {} -> {}", from, path);
        } else {
            info!("Notifying navigate to: {}", path);
        }
        let msg = DocMessage {
            msg_type: "navigate".to_string(),
            path: Some(path.to_string()),
            if_on_path: if_on_path.map(|s| s.to_string()),
        };
        let _ = self.update_tx.send(serde_json::to_string(&msg).unwrap_or_default());
    }

    /// Get the broadcast sender for WebSocket messages (for stream piping)
    #[allow(dead_code)]
    pub fn ws_broadcast_tx(&self) -> broadcast::Sender<String> {
        self.update_tx.clone()
    }
}

/// Wrapper for dynamic doc index updates
struct DocServerStateWrapper {
    index: Arc<RwLock<DocIndex>>,
    update_tx: broadcast::Sender<String>,
}

/// Create a router that reads from a dynamic index
fn doc_router_dynamic(state: Arc<DocServerStateWrapper>) -> axum::Router {
    use axum::{
        extract::{Path, Query, State, WebSocketUpgrade, ws::{Message, WebSocket}},
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
            (StatusCode::NOT_FOUND, Html(render_page_not_found_dynamic(&path, &index)))
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

    /// WebSocket handler for live updates
    async fn ws_handler(
        ws: WebSocketUpgrade,
        State(state): State<Arc<DocServerStateWrapper>>,
    ) -> impl IntoResponse {
        ws.on_upgrade(move |socket| handle_ws(socket, state))
    }

    async fn handle_ws(mut socket: WebSocket, state: Arc<DocServerStateWrapper>) {
        let mut rx = state.update_tx.subscribe();

        // Send updates to the client when docs change
        loop {
            tokio::select! {
                // Wait for update notification
                result = rx.recv() => {
                    match result {
                        Ok(msg) => {
                            // Send the JSON message to client
                            if socket.send(Message::Text(msg.into())).await.is_err() {
                                break; // Client disconnected
                            }
                        }
                        Err(broadcast::error::RecvError::Closed) => break,
                        Err(broadcast::error::RecvError::Lagged(_)) => continue,
                    }
                }
                // Handle incoming messages (e.g., ping/pong)
                msg = socket.recv() => {
                    match msg {
                        Some(Ok(Message::Close(_))) | None => break,
                        Some(Ok(Message::Ping(data))) => {
                            let _ = socket.send(Message::Pong(data)).await;
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    axum::Router::new()
        .route("/", get(index_handler))
        .route("/doc/{*path}", get(doc_item_handler))
        .route("/api/search", get(search_handler))
        .route("/api/item/{*path}", get(item_api_handler))
        .route("/ws", get(ws_handler))
        .with_state(state)
}

// Re-use the rendering from doc_engine::server but with dynamic index
fn render_page_dynamic(title: &str, current_path: &str, index: &DocIndex) -> String {
    doc_engine::server::render_page_for_lsp(title, current_path, index)
}

fn render_page_not_found_dynamic(path: &str, index: &DocIndex) -> String {
    doc_engine::server::render_page_not_found_for_lsp(path, index)
}
