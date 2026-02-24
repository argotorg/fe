//! WebSocket notification server for browser-side doc viewers.
//!
//! Runs alongside the LSP server, broadcasting file-change and navigation
//! events to connected WebSocket clients (browser tabs, dev tools).

use std::net::SocketAddr;

use futures::stream::SplitSink;
use futures::{SinkExt, StreamExt};
use serde::{Deserialize, Serialize};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::broadcast;
use tokio_tungstenite::WebSocketStream;
use tokio_tungstenite::tungstenite::Message;
use tracing::{error, info, warn};

/// Server → client messages.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum WsServerMsg {
    /// A file was changed (saved/edited).
    Update { uri: String },
    /// Navigate the doc viewer to a specific path.
    Navigate { path: String },
}

/// Client → server messages.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum WsClientMsg {
    /// Request the editor to navigate to a source location.
    GotoSource { path: String },
}

pub type WsBroadcast = broadcast::Sender<WsServerMsg>;

/// Create a new broadcast channel for WebSocket notifications.
pub fn new_broadcast() -> (WsBroadcast, broadcast::Receiver<WsServerMsg>) {
    broadcast::channel(64)
}

/// Start the WebSocket notification server on the given port.
///
/// Returns a `JoinHandle` that runs until dropped.
pub fn start_ws_server(port: u16, broadcast_tx: WsBroadcast) -> tokio::task::JoinHandle<()> {
    tokio::spawn(async move {
        let addr = SocketAddr::from(([127, 0, 0, 1], port));
        let listener = match TcpListener::bind(&addr).await {
            Ok(l) => l,
            Err(e) => {
                error!("WebSocket server failed to bind to {addr}: {e}");
                return;
            }
        };

        info!("WebSocket notification server listening on ws://{addr}");

        loop {
            match listener.accept().await {
                Ok((stream, peer)) => {
                    info!("WebSocket client connected: {peer}");
                    let rx = broadcast_tx.subscribe();
                    tokio::spawn(handle_ws_client(stream, rx));
                }
                Err(e) => {
                    warn!("WebSocket accept error: {e}");
                }
            }
        }
    })
}

async fn handle_ws_client(stream: TcpStream, mut broadcast_rx: broadcast::Receiver<WsServerMsg>) {
    let ws_stream = match tokio_tungstenite::accept_async(stream).await {
        Ok(ws) => ws,
        Err(e) => {
            warn!("WebSocket handshake failed: {e}");
            return;
        }
    };

    let (mut ws_tx, mut ws_rx) = ws_stream.split();

    loop {
        tokio::select! {
            // Forward broadcast messages to this WebSocket client
            msg = broadcast_rx.recv() => {
                match msg {
                    Ok(server_msg) => {
                        if let Err(e) = send_json(&mut ws_tx, &server_msg).await {
                            info!("WebSocket send error (client disconnected?): {e}");
                            break;
                        }
                    }
                    Err(broadcast::error::RecvError::Lagged(n)) => {
                        warn!("WebSocket client lagged by {n} messages");
                    }
                    Err(broadcast::error::RecvError::Closed) => {
                        info!("Broadcast channel closed, shutting down WS client handler");
                        break;
                    }
                }
            }
            // Read messages from the WebSocket client
            frame = ws_rx.next() => {
                match frame {
                    Some(Ok(Message::Text(text))) => {
                        match serde_json::from_str::<WsClientMsg>(text.as_ref()) {
                            Ok(client_msg) => {
                                info!("WebSocket client message: {client_msg:?}");
                                // Client messages can be handled here in the future
                                // (e.g., GotoSource -> trigger editor navigation)
                            }
                            Err(e) => {
                                warn!("Invalid WebSocket message: {e}");
                            }
                        }
                    }
                    Some(Ok(Message::Close(_))) | None => {
                        info!("WebSocket client disconnected");
                        break;
                    }
                    Some(Ok(_)) => {} // ignore ping/pong/binary
                    Some(Err(e)) => {
                        warn!("WebSocket read error: {e}");
                        break;
                    }
                }
            }
        }
    }
}

async fn send_json(
    ws_tx: &mut SplitSink<WebSocketStream<TcpStream>, Message>,
    msg: &WsServerMsg,
) -> Result<(), tokio_tungstenite::tungstenite::Error> {
    let json = serde_json::to_string(msg).unwrap_or_default();
    ws_tx.send(Message::Text(json.into())).await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn server_msg_update_serialization() {
        let msg = WsServerMsg::Update {
            uri: "file:///foo.fe".into(),
        };
        let json = serde_json::to_string(&msg).unwrap();
        assert!(json.contains(r#""type":"Update""#));
        assert!(json.contains(r#""uri":"file:///foo.fe""#));

        let decoded: WsServerMsg = serde_json::from_str(&json).unwrap();
        match decoded {
            WsServerMsg::Update { uri } => assert_eq!(uri, "file:///foo.fe"),
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn server_msg_navigate_serialization() {
        let msg = WsServerMsg::Navigate {
            path: "mylib::Foo/struct".into(),
        };
        let json = serde_json::to_string(&msg).unwrap();
        assert!(json.contains(r#""type":"Navigate""#));
        let decoded: WsServerMsg = serde_json::from_str(&json).unwrap();
        match decoded {
            WsServerMsg::Navigate { path } => assert_eq!(path, "mylib::Foo/struct"),
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn client_msg_goto_source_serialization() {
        let msg = WsClientMsg::GotoSource {
            path: "src/lib.fe:42".into(),
        };
        let json = serde_json::to_string(&msg).unwrap();
        let decoded: WsClientMsg = serde_json::from_str(&json).unwrap();
        match decoded {
            WsClientMsg::GotoSource { path } => assert_eq!(path, "src/lib.fe:42"),
        }
    }

    #[test]
    fn broadcast_channel_works() {
        let (tx, _rx) = new_broadcast();
        let mut rx2 = tx.subscribe();
        tx.send(WsServerMsg::Update { uri: "test".into() }).unwrap();
        let msg = rx2.try_recv().unwrap();
        match msg {
            WsServerMsg::Update { uri } => assert_eq!(uri, "test"),
            _ => panic!("wrong variant"),
        }
    }
}
