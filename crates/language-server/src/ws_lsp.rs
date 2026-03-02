//! WebSocket-to-LSP transport bridge.
//!
//! Bridges WebSocket text messages (JSON-RPC) to the byte-stream framing
//! that `async_lsp::MainLoop::run_buffered` expects (Content-Length headers).
//!
//! Architecture:
//! - Incoming WS text messages → prepend Content-Length header → feed to reader
//! - LSP output bytes → parse Content-Length frames → send as WS text messages

use std::net::SocketAddr;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};

use futures::io::{AsyncRead, AsyncWrite};
use futures::{SinkExt, StreamExt};
use tokio::net::TcpListener;
use tokio::sync::Mutex;
use tokio_tungstenite::tungstenite::Message;
use tracing::{error, info, warn};

use crate::server::setup;

/// Start the LSP-over-WebSocket server.
///
/// Each WebSocket connection gets a full LSP server instance (same as TCP).
/// The WS messages carry raw JSON-RPC (no Content-Length framing) —
/// the bridge adds/strips framing for async_lsp compatibility.
pub async fn run_ws_lsp_server(port: u16) {
    let addr = SocketAddr::from(([127, 0, 0, 1], port));
    let listener = match TcpListener::bind(&addr).await {
        Ok(l) => l,
        Err(e) => {
            error!("LSP WebSocket server failed to bind to {addr}: {e}");
            return;
        }
    };

    info!("LSP WebSocket server listening on ws://{addr}");

    loop {
        match listener.accept().await {
            Ok((stream, peer)) => {
                info!("LSP WebSocket client connected: {peer}");
                tokio::spawn(async move {
                    if let Err(e) = handle_ws_lsp_client(stream, peer).await {
                        warn!("LSP WebSocket client {peer} error: {e}");
                    }
                    info!("LSP WebSocket client {peer} disconnected");
                });
            }
            Err(e) => {
                warn!("LSP WebSocket accept error: {e}");
            }
        }
    }
}

async fn handle_ws_lsp_client(
    stream: tokio::net::TcpStream,
    peer: SocketAddr,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    use async_lsp::client_monitor::ClientProcessMonitorLayer;
    use async_lsp::concurrency::ConcurrencyLayer;
    use async_lsp::panic::CatchUnwindLayer;
    use async_lsp::server::LifecycleLayer;
    use tower::ServiceBuilder;

    let ws_stream = tokio_tungstenite::accept_async(stream).await?;
    let (ws_sink, ws_source) = ws_stream.split();

    // Shared sink for sending WS messages from the output bridge
    let ws_sink = Arc::new(Mutex::new(ws_sink));

    // Create the bridge pair (implements futures::io::{AsyncRead, AsyncWrite})
    let reader = WsToLspReader::new(ws_source);
    let writer = LspToWsWriter::new(ws_sink);

    let (server, client) = async_lsp::MainLoop::new_server(|client| {
        let lsp_service = setup(client.clone(), format!("LSP WS actor for {peer}"));
        ServiceBuilder::new()
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client.clone()))
            .service(lsp_service)
    });

    let _logging = crate::logging::setup_default_subscriber(client);

    // run_buffered expects futures::io::{AsyncRead, AsyncWrite}
    match server.run_buffered(reader, writer).await {
        Ok(_) => info!("LSP WS server for {peer} finished"),
        Err(e) => warn!("LSP WS server for {peer} error: {e:?}"),
    }

    Ok(())
}

// ============================================================================
// WS → LSP Reader: wraps incoming WS text messages with Content-Length headers
// ============================================================================

/// Reads incoming WebSocket text messages and presents them as a
/// Content-Length-framed byte stream for `run_buffered`.
struct WsToLspReader<S> {
    source: S,
    /// Buffer of framed bytes ready to be read
    buf: Vec<u8>,
    /// Current read position in buf
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
    S: futures::Stream<Item = Result<Message, tokio_tungstenite::tungstenite::Error>> + Unpin,
{
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut [u8],
    ) -> Poll<std::io::Result<usize>> {
        let this = self.get_mut();

        // If we have buffered data, serve it
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

        // Need more data: poll the WebSocket stream
        match Pin::new(&mut this.source).poll_next(cx) {
            Poll::Ready(Some(Ok(Message::Text(text)))) => {
                // Frame the JSON-RPC message with Content-Length header
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
            Poll::Ready(Some(Ok(Message::Close(_)))) | Poll::Ready(None) => {
                // Connection closed
                Poll::Ready(Ok(0))
            }
            Poll::Ready(Some(Ok(_))) => {
                // Ignore ping/pong/binary, try again
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
// ============================================================================

/// Accepts Content-Length-framed LSP output and sends each body as a
/// WebSocket text message.
///
/// Uses a single ordered mpsc channel to ensure messages are sent in the
/// order they were written (a per-message `tokio::spawn` would race).
struct LspToWsWriter {
    tx: tokio::sync::mpsc::UnboundedSender<String>,
    /// Accumulator for incoming bytes (may contain partial headers/bodies)
    buf: Vec<u8>,
}

impl LspToWsWriter {
    fn new<Sink>(sink: Arc<Mutex<Sink>>) -> Self
    where
        Sink: futures::Sink<Message, Error = tokio_tungstenite::tungstenite::Error>
            + Unpin
            + Send
            + 'static,
    {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<String>();
        tokio::spawn(async move {
            while let Some(body) = rx.recv().await {
                let mut sink = sink.lock().await;
                if let Err(e) = sink.send(Message::Text(body.into())).await {
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

        // Try to extract complete Content-Length framed messages
        while let Some(header_end) = find_subsequence(&this.buf, b"\r\n\r\n") {
            // Parse Content-Length from header
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

            let body_start = header_end + 4; // skip \r\n\r\n
            let message_end = body_start + content_length;

            if this.buf.len() < message_end {
                break; // Incomplete body, wait for more data
            }

            // Extract the complete JSON-RPC body
            let body = String::from_utf8_lossy(&this.buf[body_start..message_end]).into_owned();

            // Remove the consumed message from buffer
            this.buf.drain(..message_end);

            // Send through ordered channel (preserves message ordering)
            let _ = this.tx.send(body);
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

/// Find the first occurrence of `needle` in `haystack`.
fn find_subsequence(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    haystack.windows(needle.len()).position(|w| w == needle)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_subsequence_basic() {
        assert_eq!(
            find_subsequence(b"hello\r\n\r\nworld", b"\r\n\r\n"),
            Some(5)
        );
        assert_eq!(find_subsequence(b"no delimiter", b"\r\n\r\n"), None);
    }
}
