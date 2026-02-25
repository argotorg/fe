//! Combined HTTP (doc pages) + WebSocket (LSP) server.
//!
//! Replaces the standalone `ws_lsp.rs` server. Serves static documentation
//! HTML on all HTTP routes and upgrades `/lsp` to a WebSocket LSP connection
//! backed by the shared Backend actor.

use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};

use act_locally::actor::ActorRef;
use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::server::LifecycleLayer;
use axum::Router;
use axum::extract::WebSocketUpgrade;
use axum::extract::ws::{Message as AxumMessage, WebSocket};
use axum::response::Html;
use axum::routing::get;
use futures::io::{AsyncRead, AsyncWrite};
use futures::{SinkExt, StreamExt};
use tokio::sync::{Mutex, broadcast, watch};
use tower::ServiceBuilder;
use tracing::{info, warn};

use crate::backend::Backend;
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
pub async fn run(
    listener: tokio::net::TcpListener,
    doc_html: String,
    actor_rx: watch::Receiver<Option<SharedActor>>,
    doc_nav_tx: broadcast::Sender<String>,
    doc_reload_tx: broadcast::Sender<String>,
) {
    let port = listener.local_addr().unwrap().port();
    let html = Arc::new(tokio::sync::RwLock::new(doc_html));

    // Spawn a task to rebuild the served HTML when doc data changes
    let html_for_reload = Arc::clone(&html);
    let mut reload_rx = doc_reload_tx.subscribe();
    tokio::spawn(async move {
        while let Ok(payload) = reload_rx.recv().await {
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

                // Append auto-connect script
                let connect_script = format!(
                    r#"<script>window.FE_LSP = connectLsp("ws://127.0.0.1:{port}/lsp");</script>"#,
                );
                if let Some(pos) = new_html.rfind("</body>") {
                    new_html.insert_str(pos, &connect_script);
                }

                *html_for_reload.write().await = new_html;
                info!("Updated served doc HTML with fresh data");
            }
        }
    });

    let html_for_fallback = Arc::clone(&html);
    let app = Router::new()
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
        while let Ok(path) = nav_rx.recv().await {
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
        while let Ok(payload) = reload_rx.recv().await {
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
