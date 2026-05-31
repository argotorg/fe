#![allow(clippy::items_after_test_module)]
//! Combined HTTP (doc pages) + WebSocket (LSP) server.
//!
//! Replaces the standalone `ws_lsp.rs` server. Serves static documentation
//! HTML on all HTTP routes and upgrades `/lsp` to a WebSocket LSP connection
//! backed by the shared Backend actor.

use std::collections::BTreeMap;
use std::convert::Infallible;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};

use act_locally::actor::ActorRef;
use act_locally::dispatcher::Dispatcher;
use act_locally::message::{Message, MessageDowncast, MessageKey, Response};
use act_locally::types::ActorError;
use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::server::LifecycleLayer;
use axum::Json;
use axum::Router;
use axum::extract::ws::{Message as AxumMessage, WebSocket};
use axum::extract::{Path, Query, WebSocketUpgrade};
use axum::http::{HeaderMap, StatusCode};
use axum::response::sse::{Event, Sse};
use axum::response::{Html, IntoResponse};
use axum::routing::{get, post};
use futures::io::{AsyncRead, AsyncWrite};
use futures::{SinkExt, StreamExt};
use tokio::sync::{Mutex, broadcast, watch};
use tower::ServiceBuilder;
use trace_query::{TraceQueryHttpRequest, TraceQueryHttpResponse};
use tracing::{info, warn};

use crate::backend::Backend;
use crate::introspection::TraceBackendQueryRequest;
use crate::lsp_actor::service::LspActorKey;
use crate::server::setup_ws_service;

/// Shared actor reference, set once the stdio MainLoop creates the Backend.
pub type SharedActor = ActorRef<Backend, LspActorKey>;

/// Run the combined HTTP+WS server.
///
/// - All HTTP requests serve `doc_html` (the static doc SPA).
/// - `/lsp` upgrades to WebSocket and bridges to the shared Backend.
/// - `fe/navigate` notifications are forwarded to WS clients from `doc_nav_tx`.
/// - `fe/docReload` notifications are forwarded from `doc_reload_tx`.
#[allow(clippy::too_many_arguments)]
pub async fn run(
    listener: tokio::net::TcpListener,
    doc_html: String,
    actor_rx: watch::Receiver<Option<SharedActor>>,
    doc_nav_tx: broadcast::Sender<String>,
    doc_reload_tx: broadcast::Sender<String>,
    tooling_config: introspection_config::FeToolingConfig,
    config_hash: String,
    workspace_root: Option<String>,
    capabilities: Vec<String>,
) {
    let html = Arc::new(tokio::sync::RwLock::new(doc_html));
    let local_addr = listener.local_addr().ok().map(|addr| addr.to_string());

    // Spawn a task to rebuild the served HTML when doc data changes
    let html_for_reload = Arc::clone(&html);
    let mut reload_rx = doc_reload_tx.subscribe();
    tokio::spawn(async move {
        loop {
            let payload = match reload_rx.recv().await {
                Ok(p) => p,
                Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
                Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
            };
            if let Ok(data) = serde_json::from_str::<serde_json::Value>(&payload) {
                let doc_index_json = data
                    .get("docIndex")
                    .map(|v| serde_json::to_string(v).unwrap_or_default())
                    .unwrap_or_default();
                let scip_json = data
                    .get("scipData")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string());

                // Extract title from the doc index
                let title = data
                    .get("docIndex")
                    .and_then(|idx| idx.get("modules"))
                    .and_then(|m| m.as_array())
                    .and_then(|a| a.first())
                    .and_then(|m| m.get("name"))
                    .and_then(|n| n.as_str())
                    .map(|n| format!("{n} — Fe Documentation"))
                    .unwrap_or_else(|| "Fe Documentation".to_string());

                let mut new_html = fe_web::assets::html_shell_full(
                    &title,
                    &doc_index_json,
                    scip_json.as_deref(),
                    None,
                );

                // Append auto-connect script — derive WS URL from page origin
                let connect_script =
                    r#"<script>window.FE_LSP = connectLsp(`${location.protocol==='https:'?'wss:':'ws:'}://${location.host}/lsp`);</script>"#
                        .to_string();
                if let Some(pos) = new_html.rfind("</body>") {
                    new_html.insert_str(pos, &connect_script);
                }

                *html_for_reload.write().await = new_html;
                info!("Updated served doc HTML with fresh data");
            }
        }
    });

    let html_for_fallback = Arc::clone(&html);
    let workspace_root_for_health = workspace_root.clone();
    let health_payload = Arc::new(serde_json::json!({
        "status": "ok",
        "workspace_root": workspace_root_for_health,
        "server_addr": local_addr,
        "capabilities": capabilities.clone(),
        "config_hash": config_hash.clone(),
    }));
    let config_payload = Arc::new(serde_json::json!({
        "config_hash": config_hash,
        "config": tooling_config,
    }));
    let app = Router::new()
        .route(
            "/health",
            get({
                let payload = Arc::clone(&health_payload);
                move || {
                    let payload = Arc::clone(&payload);
                    async move { Json((*payload).clone()) }
                }
            }),
        )
        .route(
            "/config/effective",
            get({
                let payload = Arc::clone(&config_payload);
                move || {
                    let payload = Arc::clone(&payload);
                    async move { Json((*payload).clone()) }
                }
            }),
        )
        .route(
            "/trace/workbench",
            get(|| async {
                Html(fe_web::assets::origin_trace_live_html_shell(
                    "Fe Trace Workbench",
                ))
            }),
        )
        .route(
            "/trace/session/{session_id}/bootstrap",
            get({
                let actor_rx = actor_rx.clone();
                let workspace_root = workspace_root.clone();
                move |Path(session_id): Path<String>,
                      Query(query): Query<BTreeMap<String, String>>,
                      headers: HeaderMap| {
                    let actor_rx = actor_rx.clone();
                    let workspace_root = workspace_root.clone();
                    async move {
                        handle_trace_workbench_bootstrap_http(
                            session_id,
                            query,
                            headers,
                            actor_rx,
                            workspace_root,
                        )
                        .await
                    }
                }
            }),
        )
        .route(
            "/trace/session/{session_id}/model",
            get({
                let actor_rx = actor_rx.clone();
                let workspace_root = workspace_root.clone();
                move |Path(session_id): Path<String>,
                      Query(query): Query<BTreeMap<String, String>>,
                      headers: HeaderMap| {
                    let actor_rx = actor_rx.clone();
                    let workspace_root = workspace_root.clone();
                    async move {
                        handle_trace_workbench_model_http(
                            session_id,
                            query,
                            headers,
                            actor_rx,
                            workspace_root,
                        )
                        .await
                    }
                }
            }),
        )
        .route(
            "/trace/session/{session_id}/manifest",
            get({
                let actor_rx = actor_rx.clone();
                let workspace_root = workspace_root.clone();
                move |Path(session_id): Path<String>,
                      Query(query): Query<BTreeMap<String, String>>,
                      headers: HeaderMap| {
                    let actor_rx = actor_rx.clone();
                    let workspace_root = workspace_root.clone();
                    async move {
                        handle_trace_workbench_manifest_http(
                            session_id,
                            query,
                            headers,
                            actor_rx,
                            workspace_root,
                        )
                        .await
                    }
                }
            }),
        )
        .route(
            "/trace/session/{session_id}/events",
            get({
                let actor_rx = actor_rx.clone();
                let workspace_root = workspace_root.clone();
                move |Path(session_id): Path<String>,
                      Query(query): Query<BTreeMap<String, String>>| {
                    let actor_rx = actor_rx.clone();
                    let workspace_root = workspace_root.clone();
                    async move {
                        handle_trace_workbench_events_http(
                            session_id,
                            query,
                            actor_rx,
                            workspace_root,
                        )
                        .await
                    }
                }
            }),
        )
        .route(
            "/trace/session/{session_id}/select",
            post({
                let actor_rx = actor_rx.clone();
                let workspace_root = workspace_root.clone();
                move |Path(session_id): Path<String>,
                      Query(query): Query<BTreeMap<String, String>>,
                      headers: HeaderMap,
                      Json(selection): Json<serde_json::Value>| {
                    let actor_rx = actor_rx.clone();
                    let workspace_root = workspace_root.clone();
                    async move {
                        handle_trace_workbench_select_http(
                            session_id,
                            query,
                            headers,
                            selection,
                            actor_rx,
                            workspace_root,
                        )
                        .await
                    }
                }
            }),
        )
        .route(
            "/trace/query",
            post({
                let actor_rx = actor_rx.clone();
                let workspace_root = workspace_root.clone();
                move |Json(request): Json<TraceQueryHttpRequest>| {
                    let actor_rx = actor_rx.clone();
                    let workspace_root = workspace_root.clone();
                    async move { handle_trace_query_http(request, actor_rx, workspace_root).await }
                }
            }),
        )
        .route(
            "/lsp",
            get({
                let actor_rx = actor_rx.clone();
                let doc_nav_tx = doc_nav_tx.clone();
                let doc_reload_tx = doc_reload_tx.clone();
                move |ws: WebSocketUpgrade| {
                    let actor_rx = actor_rx.clone();
                    let doc_nav_tx = doc_nav_tx.clone();
                    let doc_reload_tx = doc_reload_tx.clone();
                    async move {
                        ws.on_upgrade(|socket| {
                            handle_ws_lsp(socket, actor_rx, doc_nav_tx, doc_reload_tx)
                        })
                    }
                }
            }),
        )
        .fallback(get(move || {
            let html = Arc::clone(&html_for_fallback);
            async move { Html(html.read().await.clone()) }
        }));

    info!(
        "Combined doc+LSP server listening on http://{}",
        listener.local_addr().unwrap()
    );

    if let Err(e) = axum::serve(listener, app).await {
        warn!("Combined server error: {e}");
    }
}

async fn handle_trace_query_http(
    request: TraceQueryHttpRequest,
    actor_rx: watch::Receiver<Option<SharedActor>>,
    workspace_root: Option<String>,
) -> impl IntoResponse {
    if let Err(reason) = validate_trace_auth(workspace_root.as_deref(), &request.auth_token) {
        return (
            StatusCode::UNAUTHORIZED,
            Json(TraceQueryHttpResponse::Unauthorized { reason }),
        );
    }

    let actor_ref = match ready_actor(actor_rx).await {
        Ok(actor_ref) => actor_ref,
        Err(reason) => {
            return (
                StatusCode::SERVICE_UNAVAILABLE,
                Json(TraceQueryHttpResponse::Error {
                    reason,
                    cache_hit: false,
                    query_duration_ms: 0,
                }),
            );
        }
    };
    actor_ref.register_handler_async_mutating(
        MessageKey(LspActorKey::of::<TraceBackendQueryRequest>()),
        crate::introspection::handle_trace_query,
    );

    let backend_request = TraceBackendQueryRequest {
        uri: request.uri,
        config_hash: request.config_hash,
        query: request.query,
    };
    let dispatcher = TraceQueryDispatcher;
    match actor_ref
        .ask::<_, TraceQueryHttpResponse, _>(&dispatcher, backend_request)
        .await
    {
        Ok(response) => (StatusCode::OK, Json(response)),
        Err(err) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(TraceQueryHttpResponse::Error {
                reason: format!("live trace backend query failed: {err:?}"),
                cache_hit: false,
                query_duration_ms: 0,
            }),
        ),
    }
}

async fn handle_trace_workbench_bootstrap_http(
    session_id: String,
    query: BTreeMap<String, String>,
    headers: HeaderMap,
    actor_rx: watch::Receiver<Option<SharedActor>>,
    workspace_root: Option<String>,
) -> impl IntoResponse {
    if let Err(reason) = validate_trace_auth(
        workspace_root.as_deref(),
        &trace_auth_token(&headers, &query).unwrap_or_default(),
    ) {
        return json_response(
            StatusCode::UNAUTHORIZED,
            serde_json::json!({ "reason": reason }),
        );
    }
    let actor_ref = match ready_actor(actor_rx).await {
        Ok(actor_ref) => actor_ref,
        Err(reason) => {
            return json_response(
                StatusCode::SERVICE_UNAVAILABLE,
                serde_json::json!({ "reason": reason }),
            );
        }
    };
    actor_ref.register_handler_async_mutating(
        MessageKey(LspActorKey::of::<
            crate::introspection::TraceWorkbenchSessionRequest,
        >()),
        crate::introspection::handle_trace_workbench_bootstrap,
    );
    let dispatcher = TraceQueryDispatcher;
    let request = crate::introspection::TraceWorkbenchSessionRequest { session_id };
    match actor_ref
        .ask::<_, crate::introspection::TraceWorkbenchBootstrapResponse, _>(&dispatcher, request)
        .await
    {
        Ok(response) => json_response(StatusCode::OK, serde_json::json!(response)),
        Err(err) => json_response(
            StatusCode::INTERNAL_SERVER_ERROR,
            serde_json::json!({ "reason": format!("trace workbench bootstrap failed: {err:?}") }),
        ),
    }
}

async fn handle_trace_workbench_model_http(
    session_id: String,
    query: BTreeMap<String, String>,
    headers: HeaderMap,
    actor_rx: watch::Receiver<Option<SharedActor>>,
    workspace_root: Option<String>,
) -> impl IntoResponse {
    if let Err(reason) = validate_trace_auth(
        workspace_root.as_deref(),
        &trace_auth_token(&headers, &query).unwrap_or_default(),
    ) {
        return json_response(
            StatusCode::UNAUTHORIZED,
            serde_json::json!({ "reason": reason }),
        );
    }
    let actor_ref = match ready_actor(actor_rx).await {
        Ok(actor_ref) => actor_ref,
        Err(reason) => {
            return json_response(
                StatusCode::SERVICE_UNAVAILABLE,
                serde_json::json!({ "reason": reason }),
            );
        }
    };
    actor_ref.register_handler_async_mutating(
        MessageKey(LspActorKey::of::<
            crate::introspection::TraceWorkbenchSessionRequest,
        >()),
        crate::introspection::handle_trace_workbench_model,
    );
    let dispatcher = TraceQueryDispatcher;
    let request = crate::introspection::TraceWorkbenchSessionRequest { session_id };
    match actor_ref
        .ask::<_, serde_json::Value, _>(&dispatcher, request)
        .await
    {
        Ok(response) => json_response(StatusCode::OK, response),
        Err(err) => json_response(
            StatusCode::INTERNAL_SERVER_ERROR,
            serde_json::json!({ "reason": format!("trace workbench model failed: {err:?}") }),
        ),
    }
}

async fn handle_trace_workbench_manifest_http(
    session_id: String,
    query: BTreeMap<String, String>,
    headers: HeaderMap,
    actor_rx: watch::Receiver<Option<SharedActor>>,
    workspace_root: Option<String>,
) -> impl IntoResponse {
    if let Err(reason) = validate_trace_auth(
        workspace_root.as_deref(),
        &trace_auth_token(&headers, &query).unwrap_or_default(),
    ) {
        return json_response(
            StatusCode::UNAUTHORIZED,
            serde_json::json!({ "reason": reason }),
        );
    }
    let actor_ref = match ready_actor(actor_rx).await {
        Ok(actor_ref) => actor_ref,
        Err(reason) => {
            return json_response(
                StatusCode::SERVICE_UNAVAILABLE,
                serde_json::json!({ "reason": reason }),
            );
        }
    };
    actor_ref.register_handler_async_mutating(
        MessageKey(LspActorKey::of::<
            crate::introspection::TraceWorkbenchSessionRequest,
        >()),
        crate::introspection::handle_trace_workbench_manifest,
    );
    let dispatcher = TraceQueryDispatcher;
    let request = crate::introspection::TraceWorkbenchSessionRequest { session_id };
    match actor_ref
        .ask::<_, serde_json::Value, _>(&dispatcher, request)
        .await
    {
        Ok(response) => json_response(StatusCode::OK, response),
        Err(err) => json_response(
            StatusCode::INTERNAL_SERVER_ERROR,
            serde_json::json!({ "reason": format!("trace workbench manifest failed: {err:?}") }),
        ),
    }
}

async fn handle_trace_workbench_events_http(
    session_id: String,
    query: BTreeMap<String, String>,
    actor_rx: watch::Receiver<Option<SharedActor>>,
    workspace_root: Option<String>,
) -> impl IntoResponse {
    let stream: Pin<Box<dyn futures::Stream<Item = Result<Event, Infallible>> + Send>> =
        if let Err(reason) = validate_trace_auth(
            workspace_root.as_deref(),
            query.get("token").map(String::as_str).unwrap_or_default(),
        ) {
            let event = Event::default()
                .event("trace/error")
                .json_data(serde_json::json!({ "reason": reason }))
                .unwrap_or_else(|_| Event::default().event("trace/error"));
            Box::pin(futures::stream::once(async move { Ok(event) }))
        } else {
            Box::pin(futures::stream::unfold(
                (session_id, actor_rx, None::<u64>),
                |(session_id, actor_rx, mut last_revision)| async move {
                    loop {
                        match trace_workbench_bootstrap_for_event(
                            session_id.clone(),
                            actor_rx.clone(),
                        )
                        .await
                        {
                            Ok(response) => {
                                let revision = response.revision.id;
                                if last_revision != Some(revision) {
                                    last_revision = Some(revision);
                                    let event = Event::default()
                                        .event("trace/revision")
                                        .json_data(serde_json::json!({
                                            "event": "trace/revision",
                                            "sessionId": session_id,
                                            "revision": revision,
                                            "documentVersion": response.revision.document_version,
                                            "status": response.revision.status,
                                            "modelDeltas": response.capabilities.model_deltas,
                                            "chunks": response.capabilities.chunks,
                                        }))
                                        .unwrap_or_else(|_| {
                                            Event::default().event("trace/revision")
                                        });
                                    return Some((
                                        Ok::<_, Infallible>(event),
                                        (session_id, actor_rx, last_revision),
                                    ));
                                }
                            }
                            Err(reason) => {
                                let event = Event::default()
                                    .event("trace/error")
                                    .json_data(serde_json::json!({ "reason": reason }))
                                    .unwrap_or_else(|_| Event::default().event("trace/error"));
                                return Some((
                                    Ok::<_, Infallible>(event),
                                    (session_id, actor_rx, last_revision),
                                ));
                            }
                        }
                        tokio::time::sleep(std::time::Duration::from_millis(1500)).await;
                    }
                },
            ))
        };
    Sse::new(stream)
}

async fn trace_workbench_bootstrap_for_event(
    session_id: String,
    actor_rx: watch::Receiver<Option<SharedActor>>,
) -> Result<crate::introspection::TraceWorkbenchBootstrapResponse, String> {
    let actor_ref = ready_actor(actor_rx).await?;
    actor_ref.register_handler_async_mutating(
        MessageKey(LspActorKey::of::<
            crate::introspection::TraceWorkbenchSessionRequest,
        >()),
        crate::introspection::handle_trace_workbench_bootstrap,
    );
    let dispatcher = TraceQueryDispatcher;
    actor_ref
        .ask::<_, crate::introspection::TraceWorkbenchBootstrapResponse, _>(
            &dispatcher,
            crate::introspection::TraceWorkbenchSessionRequest { session_id },
        )
        .await
        .map_err(|err| format!("trace workbench revision lookup failed: {err:?}"))
}

async fn handle_trace_workbench_select_http(
    session_id: String,
    query: BTreeMap<String, String>,
    headers: HeaderMap,
    selection: serde_json::Value,
    actor_rx: watch::Receiver<Option<SharedActor>>,
    workspace_root: Option<String>,
) -> impl IntoResponse {
    if let Err(reason) = validate_trace_auth(
        workspace_root.as_deref(),
        &trace_auth_token(&headers, &query).unwrap_or_default(),
    ) {
        return json_response(
            StatusCode::UNAUTHORIZED,
            serde_json::json!({ "reason": reason }),
        );
    }
    let bootstrap = match trace_workbench_bootstrap_for_event(session_id.clone(), actor_rx).await {
        Ok(response) => response,
        Err(reason) => {
            return json_response(
                StatusCode::NOT_FOUND,
                serde_json::json!({ "reason": reason }),
            );
        }
    };
    let resolved_rows = trace_workbench_resolved_rows_for_selection(&selection);
    json_response(
        StatusCode::OK,
        serde_json::json!({
            "event": "trace/selection",
            "sessionId": session_id,
            "revision": bootstrap.revision.id,
            "selection": selection,
            "resolvedRows": resolved_rows,
        }),
    )
}

fn trace_workbench_resolved_rows_for_selection(selection: &serde_json::Value) -> Vec<String> {
    if let Some(row) = selection.get("row").and_then(serde_json::Value::as_str)
        && !row.trim().is_empty()
    {
        return vec![row.to_string()];
    }
    if let Some(source_ref) = selection
        .get("sourceRef")
        .or_else(|| selection.get("source_ref"))
        .and_then(serde_json::Value::as_str)
        && let Some(line) = source_ref.strip_prefix("main:")
        && let Ok(line) = line.parse::<u64>()
    {
        return vec![trace_workbench_source_row_id(line)];
    }
    let Some(line) = selection
        .get("range")
        .and_then(|range| range.get("start"))
        .and_then(|start| start.get("line"))
        .and_then(serde_json::Value::as_u64)
        .or_else(|| selection.get("line").and_then(serde_json::Value::as_u64))
    else {
        return Vec::new();
    };
    vec![trace_workbench_source_row_id(line.saturating_add(1))]
}

fn trace_workbench_source_row_id(line_number: u64) -> String {
    format!("source-main-line-{line_number}")
}

fn json_response(status: StatusCode, value: serde_json::Value) -> axum::response::Response {
    (status, Json(value)).into_response()
}

fn trace_auth_token(headers: &HeaderMap, query: &BTreeMap<String, String>) -> Option<String> {
    headers
        .get(axum::http::header::AUTHORIZATION)
        .and_then(|value| value.to_str().ok())
        .and_then(|value| value.strip_prefix("Bearer "))
        .map(str::to_string)
        .or_else(|| query.get("token").cloned())
}

async fn ready_actor(
    mut actor_rx: watch::Receiver<Option<SharedActor>>,
) -> Result<SharedActor, String> {
    if actor_rx.borrow().is_none() {
        let wait = actor_rx.wait_for(|actor| actor.is_some());
        tokio::time::timeout(std::time::Duration::from_secs(30), wait)
            .await
            .map_err(|_| "backend actor was not ready within 30s".to_string())?
            .map_err(|_| "backend actor channel closed".to_string())?;
    }
    actor_rx
        .borrow()
        .clone()
        .ok_or_else(|| "backend actor is unavailable".to_string())
}

fn validate_trace_auth(workspace_root: Option<&str>, token: &str) -> Result<(), String> {
    let root = workspace_root.ok_or_else(|| {
        "live trace endpoint has no workspace root, refusing authenticated query".to_string()
    })?;
    let token_path = std::path::Path::new(root).join(".fe-lsp.token");
    let expected = std::fs::read_to_string(&token_path).map_err(|err| {
        format!(
            "failed to read LSP auth token {}: {err}",
            token_path.display()
        )
    })?;
    let expected = expected.trim();
    let token = token.trim();
    if expected.is_empty() {
        return Err("LSP auth token file is empty".to_string());
    }
    if token.is_empty() {
        return Err("missing LSP auth token".to_string());
    }
    if token != expected {
        return Err("invalid LSP auth token".to_string());
    }
    Ok(())
}

struct TraceQueryDispatcher;

impl Dispatcher<LspActorKey> for TraceQueryDispatcher {
    fn message_key(&self, message: &dyn Message) -> Result<MessageKey<LspActorKey>, ActorError> {
        if message.is::<TraceBackendQueryRequest>() {
            Ok(MessageKey(LspActorKey::of::<TraceBackendQueryRequest>()))
        } else if message.is::<crate::introspection::TraceWorkbenchSessionRequest>() {
            Ok(MessageKey(LspActorKey::of::<
                crate::introspection::TraceWorkbenchSessionRequest,
            >()))
        } else {
            Err(ActorError::DispatchError)
        }
    }

    fn wrap(
        &self,
        message: Box<dyn Message>,
        _key: MessageKey<LspActorKey>,
    ) -> Result<Box<dyn Message>, ActorError> {
        Ok(message)
    }

    fn unwrap(
        &self,
        message: Box<dyn Response>,
        _key: MessageKey<LspActorKey>,
    ) -> Result<Box<dyn Response>, ActorError> {
        Ok(message)
    }
}

#[cfg(test)]
mod tests {
    use super::{
        trace_auth_token, trace_workbench_resolved_rows_for_selection, validate_trace_auth,
    };
    use axum::http::{HeaderMap, header};
    use std::collections::BTreeMap;

    #[test]
    fn trace_query_auth_validates_workspace_token_file() {
        let tempdir = tempfile::tempdir().unwrap();
        std::fs::write(tempdir.path().join(".fe-lsp.token"), "secret\n").unwrap();

        assert!(validate_trace_auth(tempdir.path().to_str(), "secret").is_ok());
        assert!(validate_trace_auth(tempdir.path().to_str(), "wrong").is_err());
        assert!(validate_trace_auth(None, "secret").is_err());
    }

    #[test]
    fn trace_workbench_auth_accepts_bearer_or_query_token() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::AUTHORIZATION,
            "Bearer header-secret".parse().unwrap(),
        );
        let mut query = BTreeMap::new();
        query.insert("token".to_string(), "query-secret".to_string());

        assert_eq!(
            trace_auth_token(&headers, &query).as_deref(),
            Some("header-secret")
        );

        let headers = HeaderMap::new();
        assert_eq!(
            trace_auth_token(&headers, &query).as_deref(),
            Some("query-secret")
        );
    }

    #[test]
    fn trace_workbench_selection_resolves_source_rows() {
        let selection = serde_json::json!({
            "source": "editor",
            "selectionKind": "source-range",
            "range": {
                "start": { "line": 16, "character": 0 },
                "end": { "line": 16, "character": 8 }
            }
        });
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(&selection),
            vec!["source-main-line-17".to_string()]
        );

        let selection = serde_json::json!({ "sourceRef": "main:94" });
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(&selection),
            vec!["source-main-line-94".to_string()]
        );

        let selection = serde_json::json!({ "row": "origin-abc123" });
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(&selection),
            vec!["origin-abc123".to_string()]
        );
    }
}

/// Handle a WebSocket connection for LSP.
async fn handle_ws_lsp(
    socket: WebSocket,
    mut actor_rx: watch::Receiver<Option<SharedActor>>,
    doc_nav_tx: broadcast::Sender<String>,
    doc_reload_tx: broadcast::Sender<String>,
) {
    // Wait for the shared Backend actor to be ready (with timeout)
    let ready = {
        let wait = actor_rx.wait_for(|v| v.is_some());
        tokio::time::timeout(std::time::Duration::from_secs(30), wait)
            .await
            .is_ok_and(|r| r.is_ok())
    };
    if !ready {
        warn!("Combined server: backend actor not ready within 30s, dropping WS connection");
        return;
    }
    let actor_ref = actor_rx.borrow().clone().unwrap();

    let (ws_sink, ws_source) = socket.split();
    let ws_sink = Arc::new(Mutex::new(ws_sink));

    // Create the WS-to-LSP bridge pair
    let reader = WsToLspReader::new(ws_source);
    let writer = LspToWsWriter::new(Arc::clone(&ws_sink));

    // Create a fresh LSP server + client for this WS connection
    let (server, client) = async_lsp::MainLoop::new_server(|client| {
        let lsp_service = setup_ws_service(actor_ref, client.clone());
        ServiceBuilder::new()
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client.clone()))
            .service(lsp_service)
    });

    let _logging = crate::logging::setup_default_subscriber(client);

    // Spawn a task to forward doc-navigate events as fe/navigate notifications
    let nav_sink = Arc::clone(&ws_sink);
    let mut nav_rx = doc_nav_tx.subscribe();
    let nav_task = tokio::spawn(async move {
        loop {
            let path = match nav_rx.recv().await {
                Ok(p) => p,
                Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
                Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
            };
            let notification = serde_json::json!({
                "jsonrpc": "2.0",
                "method": "fe/navigate",
                "params": { "path": path }
            });
            let text = serde_json::to_string(&notification)
                .expect("fe/navigate notification should always serialize");
            let mut sink = nav_sink.lock().await;
            if sink.send(AxumMessage::Text(text.into())).await.is_err() {
                warn!("fe/navigate: WS sink closed, stopping nav forwarding");
                break;
            }
        }
    });

    // Spawn a task to forward doc-reload events as fe/docReload notifications
    let reload_sink = Arc::clone(&ws_sink);
    let mut reload_rx = doc_reload_tx.subscribe();
    let reload_task = tokio::spawn(async move {
        loop {
            let payload = match reload_rx.recv().await {
                Ok(p) => p,
                Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
                Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
            };
            // payload is a JSON string with {docIndex, scipData}
            let params: serde_json::Value =
                serde_json::from_str(&payload).unwrap_or(serde_json::Value::Null);
            let notification = serde_json::json!({
                "jsonrpc": "2.0",
                "method": "fe/docReload",
                "params": params,
            });
            let text = serde_json::to_string(&notification)
                .expect("fe/docReload notification should always serialize");
            let mut sink = reload_sink.lock().await;
            if sink.send(AxumMessage::Text(text.into())).await.is_err() {
                warn!("fe/docReload: WS sink closed, stopping reload forwarding");
                break;
            }
        }
    });

    // Run the LSP server over the WS bridge
    match server.run_buffered(reader, writer).await {
        Ok(_) => info!("Combined WS LSP connection finished"),
        Err(e) => warn!("Combined WS LSP connection error: {e:?}"),
    }

    nav_task.abort();
    reload_task.abort();
}

// ============================================================================
// WS → LSP Reader: wraps incoming WS text messages with Content-Length headers
// ============================================================================

struct WsToLspReader<S> {
    source: S,
    buf: Vec<u8>,
    pos: usize,
}

impl<S> WsToLspReader<S> {
    fn new(source: S) -> Self {
        Self {
            source,
            buf: Vec::new(),
            pos: 0,
        }
    }
}

impl<S> AsyncRead for WsToLspReader<S>
where
    S: futures::Stream<Item = Result<AxumMessage, axum::Error>> + Unpin,
{
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut [u8],
    ) -> Poll<std::io::Result<usize>> {
        let this = self.get_mut();

        // Serve buffered data
        if this.pos < this.buf.len() {
            let available = &this.buf[this.pos..];
            let n = available.len().min(buf.len());
            buf[..n].copy_from_slice(&available[..n]);
            this.pos += n;
            if this.pos >= this.buf.len() {
                this.buf.clear();
                this.pos = 0;
            }
            return Poll::Ready(Ok(n));
        }

        // Poll the WebSocket stream
        match Pin::new(&mut this.source).poll_next(cx) {
            Poll::Ready(Some(Ok(AxumMessage::Text(text)))) => {
                let body = text.as_bytes();
                this.buf.clear();
                this.pos = 0;
                let header = format!("Content-Length: {}\r\n\r\n", body.len());
                this.buf.extend_from_slice(header.as_bytes());
                this.buf.extend_from_slice(body);

                let n = this.buf.len().min(buf.len());
                buf[..n].copy_from_slice(&this.buf[..n]);
                this.pos = n;
                if this.pos >= this.buf.len() {
                    this.buf.clear();
                    this.pos = 0;
                }
                Poll::Ready(Ok(n))
            }
            Poll::Ready(Some(Ok(AxumMessage::Close(_)))) | Poll::Ready(None) => Poll::Ready(Ok(0)),
            Poll::Ready(Some(Ok(_))) => {
                cx.waker().wake_by_ref();
                Poll::Pending
            }
            Poll::Ready(Some(Err(e))) => {
                Poll::Ready(Err(std::io::Error::new(std::io::ErrorKind::BrokenPipe, e)))
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

// ============================================================================
// LSP → WS Writer: parses Content-Length frames and sends as WS text messages
//
// Uses an mpsc channel to a single background sender task, preserving message
// ordering (unlike per-message spawns which can reorder under contention).
// ============================================================================

struct LspToWsWriter {
    tx: tokio::sync::mpsc::UnboundedSender<String>,
    buf: Vec<u8>,
}

impl LspToWsWriter {
    fn new<Sink>(sink: Arc<Mutex<Sink>>) -> Self
    where
        Sink: futures::Sink<AxumMessage, Error = axum::Error> + Unpin + Send + 'static,
    {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<String>();
        tokio::spawn(async move {
            while let Some(body) = rx.recv().await {
                let mut sink = sink.lock().await;
                if let Err(e) = sink.send(AxumMessage::Text(body.into())).await {
                    warn!("Failed to send WS message: {e}");
                    break;
                }
            }
        });
        Self {
            tx,
            buf: Vec::new(),
        }
    }
}

impl AsyncWrite for LspToWsWriter {
    fn poll_write(
        self: Pin<&mut Self>,
        _cx: &mut Context<'_>,
        data: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        let this = self.get_mut();
        this.buf.extend_from_slice(data);

        while let Some(header_end) = find_subsequence(&this.buf, b"\r\n\r\n") {
            let Ok(header) = std::str::from_utf8(&this.buf[..header_end]) else {
                break;
            };

            let content_length = header.lines().find_map(|line| {
                let (key, val) = line.split_once(':')?;
                if key.trim().eq_ignore_ascii_case("Content-Length") {
                    val.trim().parse::<usize>().ok()
                } else {
                    None
                }
            });

            let Some(content_length) = content_length else {
                break;
            };

            let body_start = header_end + 4;
            let message_end = body_start + content_length;

            if this.buf.len() < message_end {
                break;
            }

            let body = String::from_utf8_lossy(&this.buf[body_start..message_end]).into_owned();
            this.buf.drain(..message_end);

            if this.tx.send(body).is_err() {
                return Poll::Ready(Err(std::io::Error::new(
                    std::io::ErrorKind::BrokenPipe,
                    "WS sender task closed",
                )));
            }
        }

        Poll::Ready(Ok(data.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_close(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Poll::Ready(Ok(()))
    }
}

fn find_subsequence(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    haystack.windows(needle.len()).position(|w| w == needle)
}
