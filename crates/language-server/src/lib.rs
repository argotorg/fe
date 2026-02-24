mod backend;
pub mod cli;
mod fallback;
mod functionality;
pub mod logging;
mod lsp_actor;
mod lsp_diagnostics;
mod lsp_streams;
mod server;
#[cfg(test)]
mod test_utils;
mod util;
mod virtual_files;
pub mod ws_lsp;
pub mod ws_notify;

#[cfg(test)]
mod mock_client_tests;

use std::net::SocketAddr;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Duration;

use async_compat::CompatExt;
use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::server::LifecycleLayer;
use async_std::net::TcpListener;
use futures::StreamExt;
use futures::io::AsyncReadExt;
use server::setup;
use tower::ServiceBuilder;
use tracing::instrument::WithSubscriber;
use tracing::{error, info};

pub use logging::setup_panic_hook;

pub async fn run_stdio_server(ws_port: Option<u16>) {
    // Optionally start the WebSocket notification server
    let ws_broadcast = ws_port.map(|port| {
        let (tx, _rx) = ws_notify::new_broadcast();
        ws_notify::start_ws_server(port, tx.clone());
        tx
    });

    let (server, client) = async_lsp::MainLoop::new_server(|client| {
        let lsp_service = setup(
            client.clone(),
            "LSP actor".to_string(),
            ws_broadcast.clone(),
        );
        ServiceBuilder::new()
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client.clone()))
            .service(lsp_service)
    });

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    let (stdin, stdout) = (stdin.compat(), stdout.compat());

    let logging = logging::setup_default_subscriber(client);
    match server.run_buffered(stdin, stdout).await {
        Ok(_) => info!("Server finished successfully"),
        Err(e) => error!("Server error: {:?}", e),
    }
    drop(logging);
}

pub async fn run_tcp_server(port: u16, timeout: Duration, ws_port: Option<u16>) {
    // Optionally start the WebSocket notification server
    let ws_broadcast = ws_port.map(|port| {
        let (tx, _rx) = ws_notify::new_broadcast();
        ws_notify::start_ws_server(port, tx.clone());
        tx
    });

    let addr = SocketAddr::from(([0, 0, 0, 0], port));
    let listener = TcpListener::bind(&addr)
        .await
        .expect("Failed to bind to address");
    let mut incoming = listener.incoming();
    let connections_count = Arc::new(AtomicUsize::new(0));

    info!("LSP server is listening on {}", addr);

    while let Some(Ok(stream)) = incoming.next().with_current_subscriber().await {
        let client_address = stream.peer_addr().unwrap();
        let connections_count = Arc::clone(&connections_count);
        let ws_broadcast = ws_broadcast.clone();
        let task = async move {
            let (server, client) = async_lsp::MainLoop::new_server(|client| {
                let router = setup(
                    client.clone(),
                    format!("LSP actor for {client_address}"),
                    ws_broadcast.clone(),
                );
                ServiceBuilder::new()
                    .layer(LifecycleLayer::default())
                    .layer(CatchUnwindLayer::default())
                    .layer(ConcurrencyLayer::default())
                    .layer(ClientProcessMonitorLayer::new(client.clone()))
                    .service(router)
            });
            let logging = logging::setup_default_subscriber(client);
            let current_connections = connections_count.fetch_add(1, Ordering::SeqCst) + 1;
            info!(
                "New client connected. Total clients: {}",
                current_connections
            );

            let (read, write) = stream.split();
            if let Err(e) = server.run_buffered(read, write).await {
                error!("Server error for client {}: {:?}", client_address, e);
            } else {
                info!("Client {} disconnected", client_address);
            }
            let current_connections = connections_count.fetch_sub(1, Ordering::SeqCst) - 1;
            info!(
                "Client disconnected. Total clients: {}",
                current_connections
            );
            drop(logging);
        };
        tokio::spawn(task.with_current_subscriber());
    }

    let timeout_task = {
        let connections_count = Arc::clone(&connections_count);
        tokio::spawn(async move {
            loop {
                tokio::time::sleep(Duration::from_secs(1)).await;
                if connections_count.load(Ordering::Relaxed) == 0 {
                    tokio::time::sleep(timeout).await;
                    if connections_count.load(Ordering::Relaxed) == 0 {
                        info!(
                            "No clients connected for {:?}. Shutting down server.",
                            timeout
                        );
                        std::process::exit(0);
                    }
                }
            }
        })
    };

    timeout_task.await.unwrap();
}
