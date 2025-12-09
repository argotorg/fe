use std::path::PathBuf;

use async_lsp::ClientSocket;
use driver::DriverDataBase;

use crate::doc_server::{DocServerHandle, LspServerInfo};

pub struct Backend {
    pub(super) client: ClientSocket,
    pub(super) db: DriverDataBase,
    #[allow(dead_code)] // TODO: salsa3-compatible parallelism
    pub(super) workers: tokio::runtime::Runtime,
    /// Documentation server handle (started on init)
    pub(super) doc_server: Option<DocServerHandle>,
    /// Workspace root path for server info file
    pub(super) workspace_root: Option<PathBuf>,
    /// Server info for CLI discovery
    pub(super) server_info: LspServerInfo,
    /// Whether client supports window/showDocument (LSP 3.16+)
    pub(super) supports_show_document: bool,
}

impl Backend {
    pub fn new(client: ClientSocket) -> Self {
        let db = DriverDataBase::default();

        let workers = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(1)
            .enable_all()
            .build()
            .unwrap();
        Self {
            client,
            db,
            workers,
            doc_server: None,
            workspace_root: None,
            server_info: LspServerInfo::new(),
            supports_show_document: false,
        }
    }

    /// Get the documentation URL if the server is running
    pub fn docs_url(&self) -> Option<&str> {
        self.doc_server.as_ref().map(|s| s.url.as_str())
    }
}
