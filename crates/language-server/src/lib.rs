mod backend;
pub mod cli;
pub mod combined_server;
mod fallback;
mod functionality;
pub mod logging;
mod lsp_actor;
mod lsp_diagnostics;
mod lsp_streams;
pub(crate) mod panic_context;
mod server;
#[cfg(test)]
mod test_utils;
mod util;
mod virtual_files;
pub mod ws_lsp;

#[cfg(test)]
mod mock_client_tests;

use std::net::{IpAddr, SocketAddr};
use std::time::Duration;

use async_compat::CompatExt;
use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::server::LifecycleLayer;
use async_std::net::TcpListener;
use futures::StreamExt;
use futures::io::AsyncReadExt;
use server::{setup, setup_service, spawn_backend};
use tokio::sync::{broadcast, watch};
use tower::ServiceBuilder;
use tracing::instrument::WithSubscriber;
use tracing::{error, info};

pub use logging::setup_panic_hook;

/// Configuration for the combined HTTP+WS doc/LSP server.
pub struct CombinedServerConfig {
    pub listener: tokio::net::TcpListener,
    pub doc_html: String,
    /// Base URL for the documentation server (e.g. "http://127.0.0.1:9000").
    /// Passed to Backend so `fe.openDocs` can construct full URLs.
    pub docs_url: Option<String>,
}

pub async fn run_stdio_server(combined: Option<CombinedServerConfig>) {
    // Channels for sharing the Backend actor with the combined server
    let (actor_tx, actor_rx) = watch::channel(None);
    let (doc_nav_tx, _doc_nav_rx) = broadcast::channel::<String>(64);

    // Doc reload channel: Backend sends reload payloads, combined server forwards to WS clients
    let (doc_reload_tx, _doc_reload_rx) = broadcast::channel::<String>(16);

    let docs_url = combined.as_ref().and_then(|c| c.docs_url.clone());
    let has_combined = combined.is_some();

    // Start the combined server if configured
    if let Some(config) = combined {
        let combined_actor_rx = actor_rx.clone();
        let combined_nav_tx = doc_nav_tx.clone();
        let combined_reload_tx = doc_reload_tx.clone();
        tokio::spawn(async move {
            combined_server::run(
                config.listener,
                config.doc_html,
                combined_actor_rx,
                combined_nav_tx,
                combined_reload_tx,
            )
            .await;
        });
    }

    let doc_nav_tx_for_backend = doc_nav_tx.clone();
    let doc_reload_tx_for_backend = if has_combined {
        Some(doc_reload_tx.clone())
    } else {
        None
    };

    let (server, client) = async_lsp::MainLoop::new_server(|client| {
        let actor_ref = spawn_backend(
            client.clone(),
            "LSP actor".to_string(),
            Some(doc_nav_tx_for_backend.clone()),
            doc_reload_tx_for_backend.clone(),
            docs_url.clone(),
        );
        // Publish the actor ref so the combined server can use it
        let _ = actor_tx.send(Some(actor_ref.clone()));

        let lsp_service = setup_service(actor_ref, client.clone());
        ServiceBuilder::new()
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client))
            .service(lsp_service)
    });

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    let (stdin, stdout) = (stdin.compat(), stdout.compat());

    let logging = logging::setup_default_subscriber(client);
    // Emit the startup diagnostic line to the freshly-installed subscriber.
    // This is the first line in the file log and carries everything
    // needed to identify which process this is (pid, parent_pid, argv,
    // cwd, binary path, log file path, version).
    logging::log_startup_info();
    match server.run_buffered(stdin, stdout).await {
        Ok(_) => info!("Server finished successfully"),
        Err(e) => error!("Server error: {:?}", e),
    }
    drop(logging);
}

/// Serves LSP over TCP. The server has no authentication, so `host` should be a
/// loopback address unless every machine that can reach the port is trusted.
pub async fn run_tcp_server(host: IpAddr, port: u16, timeout: Duration) {
    let addr = SocketAddr::new(host, port);
    let listener = TcpListener::bind(&addr)
        .await
        .expect("Failed to bind to address");
    info!("LSP server is listening on {}", addr);
    serve_tcp(listener, timeout).await;
    info!(
        "No clients connected for {:?}. Shutting down server.",
        timeout
    );
    std::process::exit(0);
}

/// Accepts clients until none has been connected for `timeout`.
async fn serve_tcp(listener: TcpListener, timeout: Duration) {
    let (clients, activity) = watch::channel(ClientActivity::default());
    let accept = accept_clients(listener, clients);
    let idle = wait_until_idle(activity, timeout);
    futures::pin_mut!(accept, idle);
    futures::future::select(accept, idle).await;
}

/// What the idle check needs to know about the clients.
#[derive(Clone, Copy, Default)]
struct ClientActivity {
    /// The number of clients that are connected.
    connected: usize,
    /// The number of clients that have connected since the start. A client that
    /// connects and leaves before the idle check runs again still changes it.
    accepted: u64,
}

/// Counts a client as connected from the moment it is accepted until it is
/// dropped, and tells `wait_until_idle` about every change.
struct ConnectedClient(watch::Sender<ClientActivity>);

impl ConnectedClient {
    fn new(clients: &watch::Sender<ClientActivity>) -> Self {
        clients.send_modify(|activity| {
            activity.connected += 1;
            activity.accepted += 1;
        });
        Self(clients.clone())
    }

    fn total(&self) -> usize {
        self.0.borrow().connected
    }
}

impl Drop for ConnectedClient {
    fn drop(&mut self) {
        self.0.send_modify(|activity| activity.connected -= 1);
        info!("Client disconnected. Total clients: {}", self.total());
    }
}

async fn accept_clients(listener: TcpListener, clients: watch::Sender<ClientActivity>) {
    let mut incoming = listener.incoming();

    while let Some(accepted) = incoming.next().with_current_subscriber().await {
        // A connection can be reset while it waits to be accepted. Depending on
        // the platform, that shows up as a failed accept or as a stream without
        // a peer address. Neither may stop the server.
        let stream = match accepted {
            Ok(stream) => stream,
            Err(e) => {
                error!("Failed to accept a connection: {e}");
                // Do not spin when accepting keeps failing, for example when no
                // file descriptors are left.
                tokio::time::sleep(Duration::from_millis(100)).await;
                continue;
            }
        };
        let client_address = match stream.peer_addr() {
            Ok(address) => address,
            Err(e) => {
                info!("Dropping a connection that closed before it was accepted: {e}");
                continue;
            }
        };
        // Counted before the session is set up, which takes a moment, so that the
        // idle check sees a client that has just connected.
        let connected_client = ConnectedClient::new(&clients);
        let task = async move {
            let (server, client) = async_lsp::MainLoop::new_server(|client| {
                let router = setup(client.clone(), format!("LSP actor for {client_address}"));
                ServiceBuilder::new()
                    .layer(LifecycleLayer::default())
                    .layer(CatchUnwindLayer::default())
                    .layer(ConcurrencyLayer::default())
                    .layer(ClientProcessMonitorLayer::new(client))
                    .service(router)
            });
            let logging = logging::setup_default_subscriber(client);
            info!(
                "New client connected. Total clients: {}",
                connected_client.total()
            );

            let (read, write) = stream.split();
            if let Err(e) = server.run_buffered(read, write).await {
                error!("Server error for client {}: {:?}", client_address, e);
            } else {
                info!("Client {} disconnected", client_address);
            }
            drop(connected_client);
            drop(logging);
        };
        tokio::spawn(task.with_current_subscriber());
    }
}

/// Returns once no client has been connected for `timeout`. The time counts from
/// the start, or from the moment the last client left, and a client that
/// connects starts it again, also one that has left again before this runs.
async fn wait_until_idle(mut activity: watch::Receiver<ClientActivity>, timeout: Duration) {
    loop {
        let Ok(idle) = activity
            .wait_for(|activity| activity.connected == 0)
            .await
            .map(|activity| *activity)
        else {
            return;
        };
        if tokio::time::timeout(
            timeout,
            activity.wait_for(|now| now.accepted != idle.accepted),
        )
        .await
        .is_err()
        {
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_lsp::lsp_types::Url;

    /// A connection can be reset while it waits to be accepted. It must not stop the
    /// server from serving the clients that follow.
    #[tokio::test]
    async fn tcp_server_serves_clients_after_a_connection_reset_before_accept() {
        use tokio::io::{AsyncReadExt, AsyncWriteExt};

        let listener = async_std::net::TcpListener::bind("127.0.0.1:0")
            .await
            .unwrap();
        let address = listener.local_addr().unwrap();
        let reset = tokio::net::TcpStream::connect(address).await.unwrap();
        // Closing a socket with a zero linger time sends a reset. The method is
        // deprecated because lingering can block a thread, which a zero time does not.
        #[allow(deprecated)]
        reset.set_linger(Some(Duration::ZERO)).unwrap();
        drop(reset);
        tokio::time::sleep(Duration::from_millis(200)).await;
        let server = tokio::spawn(serve_tcp(listener, Duration::from_secs(3600)));

        let temp = tempfile::tempdir().unwrap();
        let root = Url::from_directory_path(temp.path().canonicalize().unwrap()).unwrap();
        let request = serde_json::json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "processId": null,
                "rootUri": null,
                "workspaceFolders": [{ "uri": root, "name": "w" }],
                "capabilities": {},
            },
        })
        .to_string();
        let answered = tokio::time::timeout(Duration::from_secs(30), async {
            let mut stream = tokio::net::TcpStream::connect(address).await.ok()?;
            let framed = format!("Content-Length: {}\r\n\r\n{request}", request.len());
            stream.write_all(framed.as_bytes()).await.ok()?;
            let mut received = Vec::new();
            let mut buffer = [0u8; 4096];
            while !String::from_utf8_lossy(&received).contains("\"capabilities\"") {
                let read = stream.read(&mut buffer).await.ok()?;
                if read == 0 {
                    return None;
                }
                received.extend_from_slice(&buffer[..read]);
            }
            Some(())
        })
        .await;
        let finished = server.is_finished();
        server.abort();
        assert!(
            !finished,
            "the TCP server stopped after a connection was reset before it was accepted"
        );
        assert_eq!(answered.ok().flatten(), Some(()), "no answer to initialize");
    }

    /// `--timeout` shuts the server down when no client is connected.
    #[tokio::test]
    async fn tcp_server_stops_when_no_client_is_connected_for_the_timeout() {
        let listener = async_std::net::TcpListener::bind("127.0.0.1:0")
            .await
            .unwrap();
        let stopped = tokio::time::timeout(
            Duration::from_secs(10),
            serve_tcp(listener, Duration::from_millis(100)),
        )
        .await;
        assert!(
            stopped.is_ok(),
            "the TCP server kept running although no client was connected"
        );
    }

    #[tokio::test]
    async fn tcp_server_keeps_running_while_a_client_is_connected() {
        let listener = async_std::net::TcpListener::bind("127.0.0.1:0")
            .await
            .unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(serve_tcp(listener, Duration::from_millis(100)));
        let _client = tokio::net::TcpStream::connect(address).await.unwrap();
        // Many times the timeout.
        tokio::time::sleep(Duration::from_millis(2500)).await;
        let finished = server.is_finished();
        server.abort();
        assert!(
            !finished,
            "the TCP server stopped while a client was connected"
        );
    }

    /// The timeout counts from the moment the last client left, not from the start.
    #[tokio::test]
    async fn tcp_server_waits_for_the_timeout_after_the_last_client_leaves() {
        let timeout = Duration::from_millis(1500);
        let listener = async_std::net::TcpListener::bind("127.0.0.1:0")
            .await
            .unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(serve_tcp(listener, timeout));

        tokio::time::sleep(Duration::from_millis(1000)).await;
        let client = tokio::net::TcpStream::connect(address).await.unwrap();
        tokio::time::sleep(Duration::from_millis(300)).await;
        drop(client);
        let left = std::time::Instant::now();

        tokio::time::timeout(Duration::from_secs(10), server)
            .await
            .expect("the TCP server kept running after the last client left")
            .unwrap();
        assert!(
            left.elapsed() >= timeout,
            "the TCP server stopped {:?} after the last client left",
            left.elapsed()
        );
    }

    /// A client that connects and leaves before the idle check runs again still
    /// restarts the idle time.
    #[tokio::test]
    async fn tcp_server_counts_a_client_that_connects_and_leaves_between_checks() {
        let timeout = Duration::from_millis(300);
        let (clients, activity) = watch::channel(ClientActivity::default());
        let idle = tokio::spawn(wait_until_idle(activity, timeout));

        tokio::time::sleep(Duration::from_millis(200)).await;
        // The idle check cannot run between the two updates on this runtime.
        drop(ConnectedClient::new(&clients));
        let left = std::time::Instant::now();

        tokio::time::timeout(Duration::from_secs(10), idle)
            .await
            .expect("the idle check did not finish")
            .unwrap();
        assert!(
            left.elapsed() >= timeout,
            "the idle time did not start again after the client left: {:?}",
            left.elapsed()
        );
    }
}
