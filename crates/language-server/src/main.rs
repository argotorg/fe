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

    if let Some(lsp_ws_port) = args.lsp_ws_port {
        tokio::spawn(fe_language_server::ws_lsp::run_ws_lsp_server(lsp_ws_port));
    }

    match args.command {
        Some(Commands::Tcp(tcp_args)) => {
            fe_language_server::run_tcp_server(
                tcp_args.port,
                Duration::from_secs(tcp_args.timeout),
            )
            .await;
        }
        None => {
            fe_language_server::run_stdio_server(None).await;
        }
    }
}
