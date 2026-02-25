use std::time::Duration;

use clap::Parser;
use fe_language_server::cli::{CliArgs, Commands};
use fe_language_server::setup_panic_hook;

#[tokio::main]
async fn main() {
    unsafe {
        std::env::set_var("RUST_BACKTRACE", "full");
    }
    setup_panic_hook();

    let args = CliArgs::parse();

    // Optionally start the LSP-over-WebSocket server in background.
    // When active, it owns the WS notify server on args.ws_port,
    // so we pass None to stdio/tcp to avoid a duplicate bind.
    let ws_lsp_active = args.lsp_ws_port.is_some();
    if let Some(lsp_ws_port) = args.lsp_ws_port {
        tokio::spawn(fe_language_server::ws_lsp::run_ws_lsp_server(
            lsp_ws_port,
            args.ws_port,
        ));
    }

    let ws_port = if ws_lsp_active { None } else { args.ws_port };

    match args.command {
        Some(Commands::Tcp(tcp_args)) => {
            fe_language_server::run_tcp_server(
                tcp_args.port,
                Duration::from_secs(tcp_args.timeout),
                ws_port,
            )
            .await;
        }
        None => {
            fe_language_server::run_stdio_server(ws_port, None).await;
        }
    }
}
