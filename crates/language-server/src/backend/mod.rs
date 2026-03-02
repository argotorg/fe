use async_lsp::ClientSocket;
use driver::DriverDataBase;
use rustc_hash::FxHashSet;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use tokio::sync::broadcast;
use url::Url;

use crate::virtual_files::{VirtualFiles, materialize_builtins};

/// Closure type for regenerating doc+SCIP data from a read-only db snapshot.
///
/// Receives a salsa snapshot of the Backend's `DriverDataBase`. The snapshot
/// shares cached query results so incremental queries are fast, and read-only
/// access avoids the deadlock that would occur if we tried to mutate a snapshot
/// while the original db is still alive.
pub type DocRegenerateFn = Arc<dyn Fn(&DriverDataBase) -> (String, Option<String>) + Send + Sync>;

pub struct Backend {
    pub(super) client: ClientSocket,
    pub(super) db: DriverDataBase,
    pub(super) workers: tokio::runtime::Runtime,
    pub(super) virtual_files: Option<VirtualFiles>,
    pub(super) readonly_warnings: FxHashSet<Url>,
    pub(super) definition_link_support: bool,
    pub(super) doc_nav_tx: Option<broadcast::Sender<String>>,
    pub(super) doc_regenerate_fn: Option<DocRegenerateFn>,
    pub(super) doc_reload_tx: Option<broadcast::Sender<String>>,
    pub(super) doc_reload_generation: Arc<AtomicU64>,
    pub(super) docs_url: Option<String>,
    pub(super) lsp_workspace_root: Option<PathBuf>,
}

impl Backend {
    pub fn new(
        client: ClientSocket,
        doc_nav_tx: Option<broadcast::Sender<String>>,
        doc_regenerate_fn: Option<DocRegenerateFn>,
        doc_reload_tx: Option<broadcast::Sender<String>>,
        docs_url: Option<String>,
    ) -> Self {
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
            doc_nav_tx,
            doc_regenerate_fn,
            doc_reload_tx,
            doc_reload_generation: Arc::new(AtomicU64::new(0)),
            docs_url,
            lsp_workspace_root: None,
        }
    }

    /// Broadcast a doc-navigate event (path like "mylib::Foo/struct").
    pub fn notify_doc_navigate(&self, path: String) {
        if let Some(tx) = &self.doc_nav_tx {
            let _ = tx.send(path);
        }
    }

    /// Broadcast a doc reload with fresh doc_index_json and scip_json.
    pub fn notify_doc_reload(&self, doc_index_json: String, scip_json: Option<String>) {
        if let Some(tx) = &self.doc_reload_tx {
            let payload = serde_json::json!({
                "docIndex": serde_json::from_str::<serde_json::Value>(&doc_index_json)
                    .unwrap_or(serde_json::Value::Null),
                "scipData": scip_json,
            });
            let _ = tx.send(payload.to_string());
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
