use async_lsp::ClientSocket;
use driver::DriverDataBase;
use rustc_hash::FxHashSet;
use url::Url;

use crate::virtual_files::{VirtualFiles, materialize_builtins};
use crate::ws_notify::{WsBroadcast, WsServerMsg};

pub struct Backend {
    pub(super) client: ClientSocket,
    pub(super) db: DriverDataBase,
    pub(super) workers: tokio::runtime::Runtime,
    pub(super) virtual_files: Option<VirtualFiles>,
    pub(super) readonly_warnings: FxHashSet<Url>,
    pub(super) definition_link_support: bool,
    pub(super) ws_broadcast: Option<WsBroadcast>,
}

impl Backend {
    pub fn new(client: ClientSocket, ws_broadcast: Option<WsBroadcast>) -> Self {
        let db = DriverDataBase::default();
        let mut virtual_files = VirtualFiles::new("fe-language-server-").ok();
        if let Some(vfs) = virtual_files.as_mut()
            && let Err(e) = materialize_builtins(vfs, &db)
        {
            tracing::warn!("failed to materialize builtins: {e}");
            virtual_files = None;
        }

        let workers = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(1)
            .enable_all()
            .build()
            .unwrap();
        Self {
            client,
            db,
            workers,
            virtual_files,
            readonly_warnings: FxHashSet::default(),
            definition_link_support: false,
            ws_broadcast,
        }
    }

    /// Send a notification to all connected WebSocket clients.
    pub fn notify_ws(&self, msg: WsServerMsg) {
        if let Some(tx) = &self.ws_broadcast {
            // Ignore send errors — just means no one is listening
            let _ = tx.send(msg);
        }
    }

    pub fn map_internal_uri_to_client(&self, uri: Url) -> Url {
        if let Some(vfs) = self.virtual_files.as_ref() {
            return vfs.map_internal_to_client(uri);
        }
        uri
    }

    pub fn map_client_uri_to_internal(&self, uri: Url) -> Url {
        if let Some(vfs) = self.virtual_files.as_ref() {
            return vfs.map_client_to_internal(uri);
        }
        uri
    }

    pub fn is_virtual_uri(&self, uri: &Url) -> bool {
        self.virtual_files
            .as_ref()
            .is_some_and(|vfs| vfs.is_virtual_uri(uri))
    }

    pub fn virtual_files_mut(&mut self) -> Option<&mut VirtualFiles> {
        self.virtual_files.as_mut()
    }

    pub fn supports_definition_link(&self) -> bool {
        self.definition_link_support
    }

    /// Spawn CPU-bound work on the worker pool with a cloned database (salsa snapshot).
    ///
    /// The closure receives a `DriverDataBase` snapshot that shares cached query
    /// results with the main database but can safely run on a separate thread.
    /// Returns a future that resolves when the work completes.
    pub fn spawn_on_workers<F, T>(&self, f: F) -> futures::channel::oneshot::Receiver<T>
    where
        F: FnOnce(&DriverDataBase) -> T + Send + 'static,
        T: Send + 'static,
    {
        let (tx, rx) = futures::channel::oneshot::channel();
        let db = self.db.clone();
        self.workers.handle().spawn_blocking(move || {
            let result = f(&db);
            drop(db); // Release salsa snapshot before sending result
            let _ = tx.send(result);
        });
        rx
    }
}
