mod backend;
mod cli;
mod fallback;
mod functionality;
mod logging;
mod lsp_actor;
mod lsp_diagnostics;
mod lsp_streams;
mod server;
#[cfg(test)]
mod test_utils;
mod util;

use std::net::SocketAddr;
use std::time::Duration;

use async_compat::CompatExt;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use async_std::net::TcpListener;
use clap::Parser;
use cli::{CliArgs, Commands};
use futures::StreamExt;
use futures::io::AsyncReadExt;
use logging::setup_panic_hook;
use server::setup;
use tracing::instrument::WithSubscriber;
use tracing::{error, info};

use async_lsp::client_monitor::ClientProcessMonitorLayer;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use tower::ServiceBuilder;

#[tokio::main]
async fn main() {
    unsafe {
        std::env::set_var("RUST_BACKTRACE", "full");
    }
    setup_panic_hook();

    // Parse CLI arguments
    let args = CliArgs::parse();

    match args.command {
        Some(Commands::Tcp(tcp_args)) => {
            // Start server with TCP listener
            start_tcp_server(tcp_args.port, Duration::from_secs(tcp_args.timeout)).await;
        }
        None => {
            // Start server with stdio
            start_stdio_server().await;
        }
    }
}

async fn start_stdio_server() {
    let (server, client) = async_lsp::MainLoop::new_server(|client| {
        let tracing_layer = TracingLayer::default();

        let pid = std::process::id();
        let lsp_service = setup(client.clone(), format!("LSP actor {}", pid));
        ServiceBuilder::new()
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(tracing_layer)
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

async fn start_tcp_server(port: u16, timeout: Duration) {
    let addr = SocketAddr::from(([0, 0, 0, 0], port));
    let listener = TcpListener::bind(&addr)
        .await
        .expect("Failed to bind to address");
    let mut incoming = listener.incoming();
    let connections_count = Arc::new(AtomicUsize::new(0)); // we will timeout if no clients are connected

    info!("LSP server is listening on {}", addr);

    while let Some(Ok(stream)) = incoming.next().with_current_subscriber().await {
        let client_address = stream.peer_addr().unwrap();
        let tracing_layer = TracingLayer::default();
        let connections_count = Arc::clone(&connections_count);
        let task = async move {
            let (server, client) = async_lsp::MainLoop::new_server(|client| {
                let router = setup(client.clone(), format!("LSP actor for {client_address}"));
                ServiceBuilder::new()
                    .layer(tracing_layer)
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
