//! Documentation server integration for the Fe language server
//!
//! This module handles:
//! - Starting an HTTP server for documentation browsing
//! - Writing server info files for CLI discovery
//! - WebSocket-based live updates
//! - Custom LSP methods for doc access

use std::sync::Arc;
use tokio::sync::{RwLock, broadcast};

use async_lsp::ClientSocket;
use doc_viewer::DocIndex;
use serde::{Deserialize, Serialize};
use tracing::{debug, error, info};

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

/// Request from doc viewer to go to source in editor
#[derive(Debug, Clone)]
pub struct GotoSourceRequest {
    pub file: String,
    pub line: u32,
    pub column: u32,
}

impl std::fmt::Display for GotoSourceRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GotoSource({}:{}:{})", self.file, self.line, self.column)
    }
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
    pub async fn start(
        client: ClientSocket,
        supports_goto_source: bool,
    ) -> Result<Self, std::io::Error> {
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
                client,
                supports_goto_source,
            });

            let app = doc_router_dynamic(state);

            info!(
                "Documentation server listening on http://127.0.0.1:{}",
                port
            );

            if let Err(e) = axum::serve(listener, app).await {
                error!("Doc server error: {}", e);
            }
        });

        Ok(Self {
            port,
            url,
            index,
            update_tx,
        })
    }

    /// Update the documentation index, detect renames, and notify connected clients
    pub async fn update_index(&self, new_index: DocIndex) {
        let mut index = self.index.write().await;

        // Detect renames by comparing source locations
        let renames = detect_renames(&index, &new_index);

        // Log and notify for each detected rename
        for (old_path, new_path) in &renames {
            info!("Detected rename: {} -> {}", old_path, new_path);
            let msg = DocMessage {
                msg_type: "navigate".to_string(),
                path: Some(new_path.clone()),
                if_on_path: Some(old_path.clone()),
            };
            let _ = self
                .update_tx
                .send(serde_json::to_string(&msg).unwrap_or_default());
        }

        *index = new_index;
        info!(
            "Updated documentation index with {} items",
            index.items.len()
        );

        // Send update message to WebSocket clients
        let msg = DocMessage {
            msg_type: "update".to_string(),
            path: None,
            if_on_path: None,
        };
        let msg_json = serde_json::to_string(&msg).unwrap_or_default();
        match self.update_tx.send(msg_json) {
            Ok(subscriber_count) => {
                info!(
                    "Sent update message to {} WebSocket subscribers",
                    subscriber_count
                );
            }
            Err(e) => {
                debug!("No WebSocket subscribers to notify: {:?}", e);
            }
        }
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
        let _ = self
            .update_tx
            .send(serde_json::to_string(&msg).unwrap_or_default());
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
    client: ClientSocket,
    /// Whether the editor supports window/showDocument for goto source
    supports_goto_source: bool,
}

/// Create a router that reads from a dynamic index (SSR mode)
fn doc_router_dynamic(state: Arc<DocServerStateWrapper>) -> axum::Router {
    use axum::{
        extract::{
            Path, Query, State, WebSocketUpgrade,
            ws::{Message, WebSocket},
        },
        http::StatusCode,
        response::{Html, IntoResponse, Json},
        routing::{get, post},
    };

    // SSR mode - server renders the full HTML for each page
    async fn index_handler(State(state): State<Arc<DocServerStateWrapper>>) -> impl IntoResponse {
        let index = state.index.read().await;
        let root_path = index
            .modules
            .first()
            .map(|m| m.url_path())
            .unwrap_or_default();
        Html(doc_viewer::server::render_page_for_lsp(
            "Fe Documentation",
            &root_path,
            &index,
            state.supports_goto_source,
        ))
    }

    // Render doc item page with SSR
    // URL format: path::to::item or path::to::item/kind
    async fn doc_item_handler(
        State(state): State<Arc<DocServerStateWrapper>>,
        Path(path): Path<String>,
    ) -> (StatusCode, Html<String>) {
        let index = state.index.read().await;
        if let Some(item) = index.find_by_url(&path) {
            let title = format!("{} - Fe Documentation", item.name);
            // Use item's url_path for proper linking
            (
                StatusCode::OK,
                Html(doc_viewer::server::render_page_for_lsp(
                    &title,
                    &item.url_path(),
                    &index,
                    state.supports_goto_source,
                )),
            )
        } else {
            (
                StatusCode::NOT_FOUND,
                Html(doc_viewer::server::render_page_not_found_for_lsp(
                    &path, &index,
                )),
            )
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
        if let Some(item) = index.find_by_url(&path) {
            Json(Some(item.clone()))
        } else {
            Json(None)
        }
    }

    /// Return the full index (for CSR mode to fetch on updates)
    async fn index_api_handler(
        State(state): State<Arc<DocServerStateWrapper>>,
    ) -> impl IntoResponse {
        let index = state.index.read().await;
        Json(index.clone())
    }

    /// Handler for "go to source" requests - tells the editor to open the file
    async fn goto_source_handler(
        State(state): State<Arc<DocServerStateWrapper>>,
        Path(path): Path<String>,
    ) -> impl IntoResponse {
        info!("Goto source request for path: {}", path);
        let index = state.index.read().await;
        if let Some(item) = index.find_by_url(&path) {
            info!(
                "Found item: {}, has_source: {}",
                item.name,
                item.source.is_some()
            );
            if let Some(ref source) = item.source {
                info!(
                    "Goto source: {} -> {}:{}:{}",
                    path, source.file, source.line, source.column
                );
                let request = GotoSourceRequest {
                    file: source.file.clone(),
                    line: source.line,
                    column: source.column,
                };
                // emit() returns Result, log any errors
                if let Err(e) = state.client.clone().emit(request) {
                    error!("Failed to emit GotoSourceRequest: {:?}", e);
                } else {
                    info!("Emitted GotoSourceRequest successfully");
                }
                return (StatusCode::OK, Json(serde_json::json!({"success": true})));
            }
            return (
                StatusCode::NOT_FOUND,
                Json(
                    serde_json::json!({"success": false, "error": format!("Item '{}' has no source location", path)}),
                ),
            );
        }
        // Log available paths for debugging
        let available_paths: Vec<_> = index.items.iter().map(|i| i.path.as_str()).collect();
        debug!("Available paths in index: {:?}", available_paths);
        info!(
            "Item not found in index: '{}' (index has {} items)",
            path,
            index.items.len()
        );
        (
            StatusCode::NOT_FOUND,
            Json(
                serde_json::json!({"success": false, "error": format!("Item not found: {}", path)}),
            ),
        )
    }

    /// WebSocket handler for live updates
    async fn ws_handler(
        ws: WebSocketUpgrade,
        State(state): State<Arc<DocServerStateWrapper>>,
    ) -> impl IntoResponse {
        ws.on_upgrade(move |socket| handle_ws(socket, state))
    }

    async fn handle_ws(mut socket: WebSocket, state: Arc<DocServerStateWrapper>) {
        info!("WebSocket client connected");
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

    /// Return server capabilities (for conditional UI features)
    async fn capabilities_handler(
        State(state): State<Arc<DocServerStateWrapper>>,
    ) -> impl IntoResponse {
        Json(serde_json::json!({
            "supports_goto_source": state.supports_goto_source
        }))
    }

    axum::Router::new()
        .route("/", get(index_handler))
        .route("/doc/{*path}", get(doc_item_handler))
        .route("/api/search", get(search_handler))
        .route("/api/item/{*path}", get(item_api_handler))
        .route("/api/index", get(index_api_handler))
        .route("/api/capabilities", get(capabilities_handler))
        .route("/api/goto/{*path}", post(goto_source_handler))
        .route("/ws", get(ws_handler))
        .with_state(state)
}

/// Detect renamed items by comparing structural identity between old and new indexes.
///
/// Identity key: (parent_path, kind, file, line)
/// - parent_path is stable during rename (only the item's own name changes)
/// - kind ensures struct doesn't match function at same location
/// - file+line disambiguates items in same parent
///
/// Returns (old_path, new_path) pairs for items that were renamed.
fn detect_renames(old_index: &DocIndex, new_index: &DocIndex) -> Vec<(String, String)> {
    use std::collections::HashMap;

    /// Identity key for an item - everything stable across a rename
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    struct ItemIdentity {
        parent_path: String,
        kind: String,
        file: String,
        line: u32,
    }

    impl ItemIdentity {
        fn from_item(item: &doc_viewer::DocItem) -> Option<Self> {
            let source = item.source.as_ref()?;
            let parent_path = item.path.rsplit_once("::").map(|(p, _)| p).unwrap_or("");

            Some(Self {
                parent_path: parent_path.to_string(),
                kind: item.kind.as_str().to_string(),
                file: source.file.clone(),
                line: source.line,
            })
        }
    }

    // Build identity -> path maps
    let old_by_identity: HashMap<ItemIdentity, &str> = old_index
        .items
        .iter()
        .filter_map(|item| Some((ItemIdentity::from_item(item)?, item.path.as_str())))
        .collect();

    let new_by_identity: HashMap<ItemIdentity, &str> = new_index
        .items
        .iter()
        .filter_map(|item| Some((ItemIdentity::from_item(item)?, item.path.as_str())))
        .collect();

    // Find renames: same identity, different path
    let mut renames: HashMap<String, String> = HashMap::new();

    for (identity, old_path) in &old_by_identity {
        if let Some(new_path) = new_by_identity.get(identity)
            && old_path != new_path
        {
            debug!(
                "Rename detected via identity {:?}: {} -> {}",
                identity, old_path, new_path
            );
            renames.insert(old_path.to_string(), new_path.to_string());
        }
    }

    // Cascade renames to children (if parent renamed, children paths change too)
    let mut cascaded: Vec<(String, String)> = Vec::new();
    for old_item in &old_index.items {
        if renames.contains_key(&old_item.path) {
            continue;
        }
        for (old_parent, new_parent) in &renames {
            if old_item.path.starts_with(&format!("{}::", old_parent)) {
                let new_path = old_item.path.replacen(old_parent, new_parent, 1);
                if new_index.find_by_path(&new_path).is_some() {
                    debug!("Cascaded rename: {} -> {}", old_item.path, new_path);
                    cascaded.push((old_item.path.clone(), new_path));
                }
            }
        }
    }

    renames.into_iter().chain(cascaded).collect()
}
