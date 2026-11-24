use std::net::{IpAddr, Ipv4Addr};

use clap::{Parser, Subcommand};

/// Language Server Protocol (LSP) Server
#[derive(Parser, Debug)]
#[command(name = "fe-analyzer")]
#[command(author = "Your Name <you@example.com>")]
#[command(version = "1.0")]
#[command(about = "LSP server for the Fe language", long_about = None)]
pub struct CliArgs {
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

/// The options of the TCP mode, shared by `fe-language-server tcp` and `fe lsp tcp`.
#[derive(Parser, Debug, Clone)]
pub struct TcpArgs {
    /// Address to listen on (default: 127.0.0.1). The server has no
    /// authentication, so use another address only on a trusted network.
    #[arg(long, default_value_t = IpAddr::V4(Ipv4Addr::LOCALHOST))]
    pub host: IpAddr,

    /// Port to listen on (default: 4242)
    #[arg(short, long, default_value_t = 4242)]
    pub port: u16,

    /// Timeout in seconds to shut down the server if no peers are connected (default: 10)
    #[arg(short, long, default_value_t = 10)]
    pub timeout: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tcp_args(arguments: &[&str]) -> TcpArgs {
        let args = CliArgs::try_parse_from(arguments).unwrap();
        match args.command {
            Some(Commands::Tcp(tcp)) => tcp,
            other => panic!("expected the tcp command, got {other:?}"),
        }
    }

    #[test]
    fn tcp_mode_listens_on_the_loopback_address_by_default() {
        assert!(tcp_args(&["fe-analyzer", "tcp"]).host.is_loopback());
    }

    #[test]
    fn tcp_mode_listens_on_the_given_host() {
        let tcp = tcp_args(&["fe-analyzer", "tcp", "--host", "0.0.0.0"]);
        assert!(tcp.host.is_unspecified());
    }
}
