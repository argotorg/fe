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
use trace_query::{
    TraceQueryHttpRequest, TraceQueryHttpResponse, trace_workbench_manifest,
    trace_workbench_summary_chunk,
};
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
    let (trace_selection_tx, _) = broadcast::channel::<String>(128);
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
            "/trace/session/{session_id}/revisions",
            get({
                let actor_rx = actor_rx.clone();
                let workspace_root = workspace_root.clone();
                move |Path(session_id): Path<String>,
                      Query(query): Query<BTreeMap<String, String>>,
                      headers: HeaderMap| {
                    let actor_rx = actor_rx.clone();
                    let workspace_root = workspace_root.clone();
                    async move {
                        handle_trace_workbench_revisions_http(
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
            "/trace/session/{session_id}/chunk/{digest}",
            get({
                let actor_rx = actor_rx.clone();
                let workspace_root = workspace_root.clone();
                move |Path((session_id, digest)): Path<(String, String)>,
                      Query(query): Query<BTreeMap<String, String>>,
                      headers: HeaderMap| {
                    let actor_rx = actor_rx.clone();
                    let workspace_root = workspace_root.clone();
                    async move {
                        handle_trace_workbench_chunk_http(
                            session_id,
                            digest,
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
                let trace_selection_tx = trace_selection_tx.clone();
                move |Path(session_id): Path<String>,
                      Query(query): Query<BTreeMap<String, String>>| {
                    let actor_rx = actor_rx.clone();
                    let workspace_root = workspace_root.clone();
                    let selection_rx = trace_selection_tx.subscribe();
                    async move {
                        handle_trace_workbench_events_http(
                            session_id,
                            query,
                            actor_rx,
                            selection_rx,
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
                let trace_selection_tx = trace_selection_tx.clone();
                move |Path(session_id): Path<String>,
                      Query(query): Query<BTreeMap<String, String>>,
                      headers: HeaderMap,
                      Json(selection): Json<serde_json::Value>| {
                    let actor_rx = actor_rx.clone();
                    let workspace_root = workspace_root.clone();
                    let trace_selection_tx = trace_selection_tx.clone();
                    async move {
                        handle_trace_workbench_select_http(
                            session_id,
                            query,
                            headers,
                            selection,
                            actor_rx,
                            trace_selection_tx,
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

async fn handle_trace_workbench_revisions_http(
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
        crate::introspection::handle_trace_workbench_revisions,
    );
    let dispatcher = TraceQueryDispatcher;
    let request = crate::introspection::TraceWorkbenchSessionRequest { session_id };
    match actor_ref
        .ask::<_, crate::introspection::TraceWorkbenchRevisionHistoryResponse, _>(
            &dispatcher,
            request,
        )
        .await
    {
        Ok(response) => json_response(StatusCode::OK, serde_json::json!(response)),
        Err(err) => json_response(
            StatusCode::INTERNAL_SERVER_ERROR,
            serde_json::json!({ "reason": format!("trace workbench revisions failed: {err:?}") }),
        ),
    }
}

async fn handle_trace_workbench_chunk_http(
    session_id: String,
    digest: String,
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
    let model = match actor_ref
        .ask::<_, serde_json::Value, _>(
            &dispatcher,
            crate::introspection::TraceWorkbenchSessionRequest { session_id },
        )
        .await
    {
        Ok(model) => model,
        Err(err) => {
            return json_response(
                StatusCode::INTERNAL_SERVER_ERROR,
                serde_json::json!({ "reason": format!("trace workbench chunk model failed: {err:?}") }),
            );
        }
    };
    match trace_workbench_chunk_payload(&model, &digest) {
        Some(payload) => json_response(StatusCode::OK, payload),
        None => json_response(
            StatusCode::NOT_FOUND,
            serde_json::json!({ "reason": format!("unknown trace workbench chunk digest {digest}") }),
        ),
    }
}

fn trace_workbench_chunk_payload(
    model: &serde_json::Value,
    digest: &str,
) -> Option<serde_json::Value> {
    let manifest = trace_workbench_manifest(model);
    if manifest.root_digest == digest {
        return Some(serde_json::json!({
            "kind": "model",
            "digest": digest,
            "value": model,
        }));
    }
    if manifest.summary_digest == digest {
        return Some(serde_json::json!({
            "kind": "summary",
            "digest": digest,
            "value": trace_workbench_summary_chunk(model),
        }));
    }
    if manifest.metadata_digest == digest {
        return Some(serde_json::json!({
            "kind": "metadata",
            "digest": digest,
            "value": model.get("metadata").cloned().unwrap_or(serde_json::Value::Null),
        }));
    }
    if manifest.source_digest == digest {
        return Some(serde_json::json!({
            "kind": "source",
            "digest": digest,
            "value": model.get("source").cloned().unwrap_or(serde_json::Value::Null),
        }));
    }
    if manifest.indexes_digest == digest {
        return Some(serde_json::json!({
            "kind": "indexes",
            "digest": digest,
            "value": model.get("indexes").cloned().unwrap_or(serde_json::Value::Null),
        }));
    }
    if manifest.rail_components_digest == digest {
        return Some(serde_json::json!({
            "kind": "rail_components",
            "digest": digest,
            "value": model.get("rail_components").cloned().unwrap_or(serde_json::Value::Null),
        }));
    }
    for (pane_id, pane_digest) in &manifest.panes {
        if pane_digest != digest {
            continue;
        }
        let pane = model
            .get("panels")
            .and_then(serde_json::Value::as_array)
            .into_iter()
            .flatten()
            .find(|pane| {
                pane.get("id").and_then(serde_json::Value::as_str) == Some(pane_id.as_str())
            })
            .cloned()
            .unwrap_or(serde_json::Value::Null);
        return Some(serde_json::json!({
            "kind": "pane",
            "id": pane_id,
            "digest": digest,
            "value": pane,
        }));
    }
    for (report_id, report_digest) in &manifest.reports {
        if report_digest != digest {
            continue;
        }
        let key = match report_id.as_str() {
            "attribution" => "attribution_audit",
            "static_analysis" => "static_analysis",
            "closure_audit" => "audit",
            "duplicate_shapes" => "duplicate_shapes",
            _ => report_id.as_str(),
        };
        return Some(serde_json::json!({
            "kind": "report",
            "id": report_id,
            "digest": digest,
            "value": model.get(key).cloned().unwrap_or(serde_json::Value::Null),
        }));
    }
    None
}

async fn handle_trace_workbench_events_http(
    session_id: String,
    query: BTreeMap<String, String>,
    actor_rx: watch::Receiver<Option<SharedActor>>,
    selection_rx: broadcast::Receiver<String>,
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
            let revision_stream = futures::stream::unfold(
                (session_id, actor_rx, None::<TraceWorkbenchRevisionCursor>),
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
                                let model_digest = response.revision.model_digest.clone();
                                let next_revision = trace_workbench_revision_cursor(&response);
                                if last_revision != Some(next_revision.clone()) {
                                    last_revision = Some(next_revision);
                                    let event = Event::default()
                                        .event("trace/revision")
                                        .json_data(serde_json::json!({
                                            "event": "trace/revision",
                                            "sessionId": session_id,
                                            "revision": revision,
                                            "documentVersion": response.revision.document_version,
                                            "status": response.revision.status,
                                            "modelDigest": model_digest,
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
            );
            let selection_stream =
                futures::stream::unfold(selection_rx, |mut selection_rx| async move {
                    loop {
                        match selection_rx.recv().await {
                            Ok(payload) => {
                                let event = Event::default().event("trace/selection").data(payload);
                                return Some((Ok::<_, Infallible>(event), selection_rx));
                            }
                            Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
                            Err(tokio::sync::broadcast::error::RecvError::Closed) => return None,
                        }
                    }
                });
            Box::pin(futures::stream::select(revision_stream, selection_stream))
        };
    Sse::new(stream)
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct TraceWorkbenchRevisionCursor {
    revision: u64,
    status: String,
    model_digest: Option<String>,
}

fn trace_workbench_revision_cursor(
    response: &crate::introspection::TraceWorkbenchBootstrapResponse,
) -> TraceWorkbenchRevisionCursor {
    TraceWorkbenchRevisionCursor {
        revision: response.revision.id,
        status: response.revision.status.clone(),
        model_digest: response.revision.model_digest.clone(),
    }
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

async fn trace_workbench_model_for_event(
    session_id: String,
    actor_rx: watch::Receiver<Option<SharedActor>>,
) -> Result<serde_json::Value, String> {
    let actor_ref = ready_actor(actor_rx).await?;
    actor_ref.register_handler_async_mutating(
        MessageKey(LspActorKey::of::<
            crate::introspection::TraceWorkbenchSessionRequest,
        >()),
        crate::introspection::handle_trace_workbench_model,
    );
    let dispatcher = TraceQueryDispatcher;
    actor_ref
        .ask::<_, serde_json::Value, _>(
            &dispatcher,
            crate::introspection::TraceWorkbenchSessionRequest { session_id },
        )
        .await
        .map_err(|err| format!("trace workbench model lookup failed: {err:?}"))
}

async fn handle_trace_workbench_select_http(
    session_id: String,
    query: BTreeMap<String, String>,
    headers: HeaderMap,
    selection: serde_json::Value,
    actor_rx: watch::Receiver<Option<SharedActor>>,
    selection_tx: broadcast::Sender<String>,
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
    let bootstrap =
        match trace_workbench_bootstrap_for_event(session_id.clone(), actor_rx.clone()).await {
            Ok(response) => response,
            Err(reason) => {
                return json_response(
                    StatusCode::NOT_FOUND,
                    serde_json::json!({ "reason": reason }),
                );
            }
        };
    let model = trace_workbench_model_for_event(session_id.clone(), actor_rx)
        .await
        .ok();
    let resolved_rows = trace_workbench_resolved_rows_for_selection(&selection, model.as_ref());
    let event = trace_workbench_selection_event(
        &session_id,
        bootstrap.revision.id,
        selection,
        resolved_rows,
    );
    let _ = selection_tx.send(event.to_string());
    json_response(StatusCode::OK, event)
}

fn trace_workbench_selection_event(
    session_id: &str,
    revision: u64,
    selection: serde_json::Value,
    resolved_rows: Vec<String>,
) -> serde_json::Value {
    serde_json::json!({
        "event": "trace/selection",
        "sessionId": session_id,
        "revision": revision,
        "selection": selection,
        "resolvedRows": resolved_rows,
    })
}

fn trace_workbench_resolved_rows_for_selection(
    selection: &serde_json::Value,
    model: Option<&serde_json::Value>,
) -> Vec<String> {
    if let Some(row) = selection.get("row").and_then(serde_json::Value::as_str)
        && !row.trim().is_empty()
    {
        return vec![row.to_string()];
    }
    if let Some(stable) = selection
        .get("stableIdentity")
        .or_else(|| selection.get("stable_identity"))
        && let Some(rows) = trace_workbench_rows_for_stable_identity(stable, model)
    {
        return rows;
    }
    if let Some(token) = selection
        .get("stableIdentityToken")
        .or_else(|| selection.get("stable_identity_token"))
        .and_then(serde_json::Value::as_str)
        && let Some(rows) = trace_workbench_rows_for_stable_identity_token(token, model)
    {
        return rows;
    }
    if let Some(origin) = selection
        .get("origin")
        .or_else(|| selection.get("originKey"))
        .or_else(|| selection.get("origin_key"))
        .or_else(|| selection.get("node"))
        .and_then(serde_json::Value::as_str)
        && let Some(rows) = trace_workbench_index_rows(model, "origin_to_rows", origin)
    {
        return rows;
    }
    if let Some(pc) = selection
        .get("bytecodePc")
        .or_else(|| selection.get("bytecode_pc"))
        .or_else(|| selection.get("pc"))
        .and_then(trace_workbench_json_key_string)
    {
        if let Some(rows) = trace_workbench_index_rows(model, "bytecode_pcs", &pc) {
            return rows;
        }
        if let Ok(pc) = pc.parse::<u64>()
            && let Some(rows) = trace_workbench_rows_for_pc_interval(model, pc)
        {
            return rows;
        }
    }
    if let Some(source_ref) = selection
        .get("sourceRef")
        .or_else(|| selection.get("source_ref"))
        .and_then(serde_json::Value::as_str)
        && let Some((source, line)) = trace_workbench_parse_source_ref(source_ref)
    {
        if let Some(rows) = trace_workbench_index_rows(model, "source_lines", source_ref) {
            return rows;
        }
        if let Some(rows) = trace_workbench_rows_for_source_interval(model, source, line) {
            return rows;
        }
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
    let source_ref = format!("main:{}", line.saturating_add(1));
    let one_based_line = line.saturating_add(1);
    trace_workbench_index_rows(model, "source_lines", &source_ref)
        .or_else(|| trace_workbench_rows_for_source_interval(model, "main", one_based_line))
        .unwrap_or_else(|| vec![trace_workbench_source_row_id(one_based_line)])
}

fn trace_workbench_parse_source_ref(source_ref: &str) -> Option<(&str, u64)> {
    let (source, line) = source_ref.rsplit_once(':')?;
    let line = line.parse::<u64>().ok()?;
    Some((source, line))
}

fn trace_workbench_rows_for_stable_identity(
    stable: &serde_json::Value,
    model: Option<&serde_json::Value>,
) -> Option<Vec<String>> {
    let kind = stable.get("kind").and_then(serde_json::Value::as_str)?;
    let value = stable
        .get("value")
        .and_then(trace_workbench_json_key_string)?;
    trace_workbench_index_rows(model, "stable_identities", &format!("{kind}:{value}"))
}

fn trace_workbench_rows_for_stable_identity_token(
    token: &str,
    model: Option<&serde_json::Value>,
) -> Option<Vec<String>> {
    let (kind, value) = token.split_once('=')?;
    let value = trace_workbench_percent_decode(value)?;
    trace_workbench_index_rows(model, "stable_identities", &format!("{kind}:{value}"))
}

fn trace_workbench_percent_decode(value: &str) -> Option<String> {
    let mut bytes = Vec::with_capacity(value.len());
    let mut chars = value.as_bytes().iter().copied();
    while let Some(byte) = chars.next() {
        if byte == b'%' {
            let high = chars.next()?;
            let low = chars.next()?;
            let high = trace_workbench_hex_value(high)?;
            let low = trace_workbench_hex_value(low)?;
            bytes.push((high << 4) | low);
        } else if byte == b'+' {
            bytes.push(b' ');
        } else {
            bytes.push(byte);
        }
    }
    String::from_utf8(bytes).ok()
}

fn trace_workbench_hex_value(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        _ => None,
    }
}

fn trace_workbench_index_rows(
    model: Option<&serde_json::Value>,
    index: &str,
    key: &str,
) -> Option<Vec<String>> {
    let value = model?.get("indexes")?.get(index)?.get(key)?;
    let mut rows = match value {
        serde_json::Value::String(row) => vec![row.clone()],
        serde_json::Value::Array(rows) => rows
            .iter()
            .filter_map(serde_json::Value::as_str)
            .map(str::to_string)
            .collect::<Vec<_>>(),
        _ => Vec::new(),
    };
    rows.sort();
    rows.dedup();
    (!rows.is_empty()).then_some(rows)
}

fn trace_workbench_rows_for_source_interval(
    model: Option<&serde_json::Value>,
    source: &str,
    line: u64,
) -> Option<Vec<String>> {
    let mut rows = model?
        .get("indexes")?
        .get("source_intervals")?
        .as_array()?
        .iter()
        .filter(|interval| {
            interval.get("source").and_then(serde_json::Value::as_str) == Some(source)
                && interval
                    .get("start_line")
                    .and_then(serde_json::Value::as_u64)
                    .is_some_and(|start| start <= line)
                && interval
                    .get("end_line")
                    .and_then(serde_json::Value::as_u64)
                    .is_some_and(|end| line <= end)
        })
        .filter_map(|interval| interval.get("row_id").and_then(serde_json::Value::as_str))
        .map(str::to_string)
        .collect::<Vec<_>>();
    rows.sort();
    rows.dedup();
    (!rows.is_empty()).then_some(rows)
}

fn trace_workbench_rows_for_pc_interval(
    model: Option<&serde_json::Value>,
    pc: u64,
) -> Option<Vec<String>> {
    let mut rows = model?
        .get("indexes")?
        .get("pc_intervals")?
        .as_array()?
        .iter()
        .filter(|interval| {
            interval
                .get("pc_start")
                .and_then(serde_json::Value::as_u64)
                .is_some_and(|start| start <= pc)
                && interval
                    .get("pc_end")
                    .and_then(serde_json::Value::as_u64)
                    .is_some_and(|end| pc < end)
        })
        .filter_map(|interval| interval.get("row_id").and_then(serde_json::Value::as_str))
        .map(str::to_string)
        .collect::<Vec<_>>();
    rows.sort();
    rows.dedup();
    (!rows.is_empty()).then_some(rows)
}

fn trace_workbench_json_key_string(value: &serde_json::Value) -> Option<String> {
    value
        .as_str()
        .map(str::to_string)
        .or_else(|| value.as_u64().map(|value| value.to_string()))
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
        trace_auth_token, trace_workbench_chunk_payload,
        trace_workbench_resolved_rows_for_selection, trace_workbench_revision_cursor,
        trace_workbench_selection_event, validate_trace_auth,
    };
    use axum::http::{HeaderMap, header};
    use std::collections::BTreeMap;
    use trace_query::trace_workbench_manifest;

    fn bootstrap_response(
        revision: u64,
        status: &str,
        digest: Option<&str>,
    ) -> crate::introspection::TraceWorkbenchBootstrapResponse {
        crate::introspection::TraceWorkbenchBootstrapResponse {
            session: crate::backend::TraceViewerSession {
                id: "trace-session-1".to_string(),
                uri: "file:///workspace/lib.fe".to_string(),
                target: "evm".to_string(),
                opt_level: "O2".to_string(),
                view: "source-postopt-bytecode".to_string(),
                config_hash: "config".to_string(),
                document_version: Some(revision as i32),
                initial_selection: None,
            },
            revision: crate::introspection::TraceWorkbenchRevision {
                id: revision,
                document_version: Some(revision as i32),
                status: status.to_string(),
                config_hash: "config".to_string(),
                model_digest: digest.map(str::to_string),
            },
            capabilities: crate::introspection::TraceWorkbenchCapabilities {
                events: "sse",
                model_deltas: false,
                chunks: true,
                selection_sync: true,
                revision_history: true,
            },
        }
    }

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
            trace_workbench_resolved_rows_for_selection(&selection, None),
            vec!["source-main-line-17".to_string()]
        );

        let selection = serde_json::json!({ "sourceRef": "main:94" });
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(&selection, None),
            vec!["source-main-line-94".to_string()]
        );

        let selection = serde_json::json!({ "row": "origin-abc123" });
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(&selection, None),
            vec!["origin-abc123".to_string()]
        );
    }

    #[test]
    fn trace_workbench_selection_resolves_projection_indexes() {
        let model = serde_json::json!({
            "indexes": {
                "source_lines": { "main:17": "source-main-line-17-model" },
                "origin_to_rows": { "bytecode.pc\u{1f}demo\u{1f}pc:68": ["origin-bytecode-68"] },
                "bytecode_pcs": { "68": ["origin-bytecode-68"] },
                "source_intervals": [
                    {
                        "source": "library:source",
                        "start_line": 5,
                        "end_line": 8,
                        "row_id": "related-library-source-line-5"
                    }
                ],
                "pc_intervals": [
                    {
                        "pc_start": 70,
                        "pc_end": 73,
                        "row_id": "origin-bytecode-70-72"
                    }
                ],
                "stable_identities": {
                    "origin:bytecode.pc\u{1f}demo\u{1f}pc:68": ["origin-bytecode-68"],
                    "source_line:main:17": ["source-main-line-17-model"]
                }
            }
        });

        assert_eq!(
            trace_workbench_resolved_rows_for_selection(
                &serde_json::json!({ "sourceRef": "main:17" }),
                Some(&model),
            ),
            vec!["source-main-line-17-model".to_string()]
        );
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(
                &serde_json::json!({ "pc": 68 }),
                Some(&model),
            ),
            vec!["origin-bytecode-68".to_string()]
        );
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(
                &serde_json::json!({ "pc": 72 }),
                Some(&model),
            ),
            vec!["origin-bytecode-70-72".to_string()]
        );
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(
                &serde_json::json!({ "sourceRef": "library:source:7" }),
                Some(&model),
            ),
            vec!["related-library-source-line-5".to_string()]
        );
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(
                &serde_json::json!({ "origin": "bytecode.pc\u{1f}demo\u{1f}pc:68" }),
                Some(&model),
            ),
            vec!["origin-bytecode-68".to_string()]
        );
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(
                &serde_json::json!({
                    "stableIdentity": {
                        "kind": "origin",
                        "value": "bytecode.pc\u{1f}demo\u{1f}pc:68"
                    }
                }),
                Some(&model),
            ),
            vec!["origin-bytecode-68".to_string()]
        );
        assert_eq!(
            trace_workbench_resolved_rows_for_selection(
                &serde_json::json!({
                    "stableIdentityToken": "origin=bytecode.pc%1Fdemo%1Fpc%3A68"
                }),
                Some(&model),
            ),
            vec!["origin-bytecode-68".to_string()]
        );
    }

    #[test]
    fn trace_workbench_selection_event_carries_resolved_rows() {
        let event = trace_workbench_selection_event(
            "trace-session-1",
            42,
            serde_json::json!({ "source": "editor" }),
            vec!["source-main-line-17".to_string()],
        );

        assert_eq!(event["event"], "trace/selection");
        assert_eq!(event["sessionId"], "trace-session-1");
        assert_eq!(event["revision"], 42);
        assert_eq!(event["resolvedRows"][0], "source-main-line-17");
    }

    #[test]
    fn trace_workbench_revision_cursor_includes_status() {
        let stale = trace_workbench_revision_cursor(&bootstrap_response(
            12,
            "stale_but_usable",
            Some("blake3:same"),
        ));
        let ready =
            trace_workbench_revision_cursor(&bootstrap_response(12, "ready", Some("blake3:same")));

        assert_ne!(stale, ready);
    }

    #[test]
    fn trace_workbench_chunk_payload_resolves_manifest_digests() {
        let model = serde_json::json!({
            "revision": { "id": 7 },
            "metadata": { "input_path": "demo.fe" },
            "source": { "lines": [{ "number": 1, "text": "fn main() {}" }] },
            "indexes": { "source_lines": { "main:1": "source-main-line-1" } },
            "provenance": { "source_to_optimized": "available" },
            "rail_components": { "exact": ["exact-c-a"] },
            "panels": [
                { "id": "source", "rows": [] },
                { "id": "bytecode", "rows": [{ "text": "STOP" }] }
            ],
            "attribution_audit": { "prepared_linked_pcs": 1 },
            "static_analysis": { "checks": [] },
            "audit": { "total_closures": 0 }
        });
        let manifest = trace_workbench_manifest(&model);

        let summary = trace_workbench_chunk_payload(&model, &manifest.summary_digest).unwrap();
        assert_eq!(summary["kind"], "summary");
        assert_eq!(summary["value"]["revision"]["id"], 7);
        assert_eq!(
            summary["value"]["provenance"]["source_to_optimized"],
            "available"
        );

        let source = trace_workbench_chunk_payload(&model, &manifest.source_digest).unwrap();
        assert_eq!(source["kind"], "source");
        assert_eq!(source["value"]["lines"][0]["text"], "fn main() {}");

        let indexes = trace_workbench_chunk_payload(&model, &manifest.indexes_digest).unwrap();
        assert_eq!(indexes["kind"], "indexes");
        assert_eq!(
            indexes["value"]["source_lines"]["main:1"],
            "source-main-line-1"
        );

        let pane_digest = manifest.panes.get("bytecode").unwrap();
        let pane = trace_workbench_chunk_payload(&model, pane_digest).unwrap();
        assert_eq!(pane["kind"], "pane");
        assert_eq!(pane["id"], "bytecode");
        assert_eq!(pane["value"]["rows"][0]["text"], "STOP");

        let report_digest = manifest.reports.get("attribution").unwrap();
        let report = trace_workbench_chunk_payload(&model, report_digest).unwrap();
        assert_eq!(report["kind"], "report");
        assert_eq!(report["id"], "attribution");
        assert_eq!(report["value"]["prepared_linked_pcs"], 1);

        assert!(trace_workbench_chunk_payload(&model, "blake3:missing").is_none());
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
