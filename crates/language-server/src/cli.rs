use clap::{Parser, Subcommand};

/// Language Server Protocol (LSP) Server
#[derive(Parser, Debug)]
#[command(name = "fe-analyzer")]
#[command(author = "Your Name <you@example.com>")]
#[command(version = "1.0")]
#[command(about = "LSP server for the Fe language", long_about = None)]
pub struct CliArgs {
    /// Start a WebSocket notification server on this port.
    #[arg(long)]
    pub ws_port: Option<u16>,

    /// Start a full LSP-over-WebSocket server on this port.
    ///
    /// Browser clients can connect and use standard LSP protocol
    /// (initialize, textDocument/*, etc.) over WebSocket transport.
    #[arg(long)]
    pub lsp_ws_port: Option<u16>,

    /// Choose the communication method
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Start the LSP server with a TCP listener
    Tcp(TcpArgs),
}

#[derive(Parser, Debug)]
pub struct TcpArgs {
    /// Port to listen on (default: 4242)
    #[arg(short, long, default_value_t = 4242)]
    pub port: u16,

    /// Timeout in seconds to shut down the server if no peers are connected (default: 10)
    #[arg(short, long, default_value_t = 10)]
    pub timeout: u64,
}
