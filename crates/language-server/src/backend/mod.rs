use async_lsp::{ClientSocket, lsp_types::Diagnostic};
use common::{InputDb, ingot::IngotKind};
use driver::DriverDataBase;
use rustc_hash::{FxHashMap, FxHashSet};
use std::panic::{self, AssertUnwindSafe};
use std::path::PathBuf;
use std::sync::atomic::AtomicU64;
use std::sync::{Arc, Mutex};
use tokio::sync::broadcast;
use url::Url;

use crate::virtual_files::{VirtualFiles, materialize_builtins};

/// Failure modes for work dispatched via [`Backend::spawn_on_workers`].
///
/// Callers need to distinguish a genuine panic in the analysis pipeline
/// (which is a real bug to report) from a cancelled receiver (which can
/// happen when a request is superseded by a newer one and the awaiting
/// future is dropped before the worker finishes).
#[derive(Debug)]
pub enum WorkerError {
    /// The worker closure panicked. The string is the best-effort extracted
    /// panic payload (via `panic_info.downcast_ref::<&str>()` or `String`),
    /// or `"<non-string panic>"` when the payload isn't a string.
    Panicked(String),
    /// The oneshot was cancelled before the worker finished — normally this
    /// means the caller dropped its future or the worker pool is shutting
    /// down.
    Cancelled,
}

impl std::fmt::Display for WorkerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WorkerError::Panicked(msg) => write!(f, "worker panicked: {msg}"),
            WorkerError::Cancelled => write!(f, "worker cancelled"),
        }
    }
}

impl std::error::Error for WorkerError {}

type PreparedViews =
    FxHashMap<Url, Result<Option<DriverDataBase>, driver::generation::GenerationError>>;
type DiagnosticsByFile = FxHashMap<Url, Vec<Diagnostic>>;

pub struct Backend {
    pub(super) client: ClientSocket,
    pub(super) db: DriverDataBase,
    /// Prepared views, keyed by standalone file URL or by package base URL.
    /// The authored `db` continues to index every file in the workspace and
    /// receives edits; any edit clears every view.
    prepared_views: Mutex<PreparedViews>,
    /// The most recent nonempty diagnostics from each analysis root. A
    /// package can report a diagnostic in one of its dependencies.
    diagnostics_by_root: Mutex<FxHashMap<Url, DiagnosticsByFile>>,
    pub(super) workers: tokio::runtime::Runtime,
    pub(super) virtual_files: Option<VirtualFiles>,
    pub(super) readonly_warnings: FxHashSet<Url>,
    pub(super) definition_link_support: bool,
    pub(super) doc_nav_tx: Option<broadcast::Sender<String>>,
    pub(super) doc_reload_tx: Option<broadcast::Sender<String>>,
    pub(super) doc_reload_generation: Arc<AtomicU64>,
    pub(super) docs_url: Option<String>,
    pub(super) lsp_workspace_root: Option<PathBuf>,
    /// The ingots and workspaces whose `fe.toml` sets the database's target.
    pub(super) project_ingots: Vec<Url>,
    /// The target conflict the client was last warned about.
    pub(super) target_conflict: Option<String>,
}

impl Backend {
    pub fn new(
        client: ClientSocket,
        doc_nav_tx: Option<broadcast::Sender<String>>,
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
            prepared_views: Mutex::new(FxHashMap::default()),
            diagnostics_by_root: Mutex::new(FxHashMap::default()),
            workers,
            virtual_files,
            readonly_warnings: FxHashSet::default(),
            definition_link_support: false,
            doc_nav_tx,
            doc_reload_tx,
            doc_reload_generation: Arc::new(AtomicU64::new(0)),
            docs_url,
            lsp_workspace_root: None,
            project_ingots: Vec::new(),
            target_conflict: None,
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
            let mapped = vfs.map_client_to_internal(uri);
            if mapped.scheme() != "file" {
                return mapped;
            }
            return normalize_file_uri(mapped);
        }
        normalize_file_uri(uri)
    }

    pub fn is_virtual_uri(&self, uri: &Url) -> bool {
        self.virtual_files
            .as_ref()
            .is_some_and(|vfs| vfs.is_virtual_uri(uri))
    }

    pub fn virtual_files_mut(&mut self) -> Option<&mut VirtualFiles> {
        self.virtual_files.as_mut()
    }

    /// Return the checked output view for a file: its standalone view, the one
    /// prepared view of its package, or the authored workspace when nothing
    /// is generated. Generation failures are kept for diagnostics; semantic
    /// requests can use the authored view while an edit is incomplete.
    pub fn try_analysis_db_for_url(
        &self,
        url: &Url,
    ) -> Result<DriverDataBase, driver::generation::GenerationError> {
        let Some(file) = self.db.workspace().get(&self.db, url) else {
            return Ok(self.db.clone());
        };
        let Some(ingot) = file.containing_ingot(&self.db) else {
            return Ok(self.db.clone());
        };
        // Standalone files are prepared one at a time; every file of a package
        // shares the view prepared for that package and its dependencies.
        let (key, standalone) = match ingot.kind(&self.db) {
            IngotKind::StandAlone => (url.clone(), true),
            IngotKind::Local | IngotKind::External => (ingot.base(&self.db).clone(), false),
            IngotKind::Core | IngotKind::Std => return Ok(self.db.clone()),
        };
        if let Some(view) = self.prepared_views.lock().unwrap().get(&key) {
            return (*view)
                .clone()
                .map(|prepared| prepared.unwrap_or_else(|| self.db.clone()));
        }
        let result = if standalone {
            driver::generation::prepare_file(&self.db, file)
        } else {
            driver::generation::prepare_ingots(&self.db, std::slice::from_ref(&key))
        };
        self.prepared_views
            .lock()
            .unwrap()
            .insert(key, result.clone());
        result.map(|prepared| prepared.unwrap_or_else(|| self.db.clone()))
    }

    pub fn analysis_db_for_url(&self, url: &Url) -> DriverDataBase {
        self.try_analysis_db_for_url(url)
            .unwrap_or_else(|_| self.db.clone())
    }

    pub fn invalidate_analysis_views(&self) {
        self.prepared_views.lock().unwrap().clear();
    }

    /// Replace the affected analysis roots' diagnostics and return one
    /// publication per touched file. A dependency's own empty report must
    /// leave diagnostics from its consumers in place.
    pub(super) fn update_diagnostics(
        &self,
        mut affected_files: FxHashSet<Url>,
        updates: Vec<(Url, DiagnosticsByFile)>,
    ) -> DiagnosticsByFile {
        let indexed = self.db.workspace().all_files(&self.db);
        let mut saved = self.diagnostics_by_root.lock().unwrap();
        saved.retain(|root, files| {
            let still_indexed = if root.path().ends_with('/') {
                root.join("fe.toml")
                    .ok()
                    .is_some_and(|config| indexed.get(&config).is_some())
            } else {
                indexed.get(root).is_some()
            };
            if !still_indexed {
                affected_files.extend(files.keys().cloned());
            }
            still_indexed
        });
        for (root, mut files) in updates {
            affected_files.extend(files.keys().cloned());
            if let Some(previous) = saved.remove(&root) {
                affected_files.extend(previous.into_keys());
            }
            files.retain(|_, diagnostics| !diagnostics.is_empty());
            if !files.is_empty() {
                saved.insert(root, files);
            }
        }
        let mut batch: DiagnosticsByFile = affected_files
            .into_iter()
            .map(|url| (url, Vec::new()))
            .collect();
        for files in saved.values() {
            for (url, diagnostics) in files {
                if let Some(combined) = batch.get_mut(url) {
                    for diagnostic in diagnostics {
                        if !combined.contains(diagnostic) {
                            combined.push(diagnostic.clone());
                        }
                    }
                }
            }
        }
        batch
    }

    pub fn supports_definition_link(&self) -> bool {
        self.definition_link_support
    }

    /// Spawn CPU-bound work on the worker pool with a cloned database (salsa snapshot).
    ///
    /// The closure receives a `DriverDataBase` snapshot that shares cached query
    /// results with the main database but can safely run on a separate thread.
    /// Returns a future resolving to `Ok(T)` on success, `Err(WorkerError::Panicked)`
    /// if the closure panicked (the message is the best-effort extracted panic
    /// payload), or `Err(WorkerError::Cancelled)` if the oneshot was cancelled
    /// before the worker finished.
    ///
    /// The worker closure's panic is caught via `catch_unwind` + `AssertUnwindSafe`.
    /// Salsa's `Cancelled` is re-raised so salsa's built-in query cancellation
    /// mechanics still work — callers will see cancellation as a panic at the
    /// tokio join boundary, which will surface here as `WorkerError::Panicked`
    /// containing the cancelled marker. (Diagnostics handlers that need to
    /// distinguish cancellation from other panics should check the message.)
    pub fn spawn_on_workers<F, T>(
        &self,
        f: F,
    ) -> impl std::future::Future<Output = Result<T, WorkerError>> + 'static
    where
        F: FnOnce(&DriverDataBase) -> T + Send + 'static,
        T: Send + 'static,
    {
        self.spawn_on_workers_with_db(self.db.clone(), f)
    }

    pub fn spawn_on_workers_with_db<F, T>(
        &self,
        db: DriverDataBase,
        f: F,
    ) -> impl std::future::Future<Output = Result<T, WorkerError>> + 'static
    where
        F: FnOnce(&DriverDataBase) -> T + Send + 'static,
        T: Send + 'static,
    {
        let (tx, rx) = futures::channel::oneshot::channel();
        self.workers.handle().spawn_blocking(move || {
            // catch_unwind the actual work so a panic inside `f` becomes a
            // logged, observable error instead of a dropped tx that looks like
            // normal cancellation to the caller.
            let outcome = panic::catch_unwind(AssertUnwindSafe(|| f(&db)));
            drop(db); // Release salsa snapshot before sending result

            let result: Result<T, WorkerError> = match outcome {
                Ok(value) => Ok(value),
                Err(panic_payload) => {
                    let msg = panic_payload
                        .downcast_ref::<&str>()
                        .copied()
                        .map(ToOwned::to_owned)
                        .or_else(|| panic_payload.downcast_ref::<String>().cloned())
                        .unwrap_or_else(|| "<non-string panic>".to_owned());
                    tracing::error!("spawn_on_workers closure panicked: {msg}");
                    Err(WorkerError::Panicked(msg))
                }
            };
            let _ = tx.send(result);
        });

        async move {
            match rx.await {
                Ok(result) => result,
                Err(_cancelled) => Err(WorkerError::Cancelled),
            }
        }
    }
}

fn normalize_file_uri(uri: Url) -> Url {
    if uri.scheme() != "file" {
        return uri;
    }

    let Ok(path) = uri.to_file_path() else {
        return uri;
    };

    Url::from_file_path(&path).unwrap_or(uri)
}

#[cfg(test)]
mod tests {
    use super::{Backend, WorkerError, normalize_file_uri};
    use async_lsp::MainLoop;
    use async_lsp::router::Router;
    use url::Url;

    #[test]
    fn normalize_file_uri_leaves_non_file_urls_unchanged() {
        let uri = Url::parse("fe-builtin://core/src/lib.fe").unwrap();
        assert_eq!(normalize_file_uri(uri.clone()), uri);
    }

    #[cfg(windows)]
    #[test]
    fn normalize_file_uri_canonicalizes_percent_encoded_drive_paths() {
        let client_uri = Url::parse("file:///c%3A/Users/sean/Downloads/erc20/src/lib.fe").unwrap();
        let expected = Url::from_file_path(r"C:\Users\sean\Downloads\erc20\src\lib.fe").unwrap();

        assert_eq!(normalize_file_uri(client_uri), expected);
    }

    /// Construct a Backend wired to a dummy ClientSocket for unit testing.
    /// The MainLoop is dropped immediately — we never run it. Backend only
    /// stores the client socket; it doesn't send anything through it during
    /// these tests.
    fn test_backend() -> Backend {
        let (_main_loop, client_socket) = MainLoop::new_server(|_client| Router::<()>::new(()));
        Backend::new(client_socket, None, None, None)
    }

    /// Regression test: `spawn_on_workers` must `catch_unwind` inside the
    /// blocking closure so that panics in the worker surface as a
    /// `WorkerError::Panicked` carrying the panic message — not a silent
    /// oneshot cancellation indistinguishable from legitimate cancellation.
    ///
    /// Before this was fixed, a panic caused `tx` to drop without sending,
    /// the receiver saw `Canceled`, and all 5 `spawn_on_workers` callers
    /// logged some variant of "worker cancelled" with no panic message.
    /// That made real analysis-pipeline bugs invisible to both users and
    /// logs — the worst kind of silent failure.
    ///
    /// `multi_thread` flavor is required because `Backend` owns a nested
    /// tokio runtime; see the closing `spawn_blocking(drop(backend))` below
    /// for the other half of that accommodation — the runtime's Drop impl
    /// blocks, so we move the drop off the async context.
    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn spawn_on_workers_surfaces_panics() {
        let backend = test_backend();

        let result: Result<(), WorkerError> = backend
            .spawn_on_workers::<_, ()>(|_db| {
                panic!("intentional test panic: __spawn_on_workers_panic_probe__");
            })
            .await;

        match result {
            Err(WorkerError::Panicked(msg)) => {
                assert!(
                    msg.contains("__spawn_on_workers_panic_probe__"),
                    "panic message should be threaded through to the caller; \
                     expected to contain the probe marker, got: {msg:?}"
                );
            }
            Err(WorkerError::Cancelled) => {
                panic!(
                    "regression: panicking worker surfaced as Cancelled — \
                     spawn_on_workers is no longer catching panics. Check \
                     backend/mod.rs::spawn_on_workers for a missing catch_unwind."
                );
            }
            Ok(()) => {
                panic!("worker closure panicked but caller got Ok — this is impossible");
            }
        }

        // Backend owns a nested tokio::runtime::Runtime (`workers`) whose
        // `Drop` performs a blocking shutdown. Dropping from within an async
        // context triggers "Cannot drop a runtime in a context where blocking
        // is not allowed", so move the drop onto a dedicated blocking thread.
        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    /// Happy path: a non-panicking closure returns its value through the
    /// new double-unwrap path unchanged.
    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn spawn_on_workers_returns_value_on_success() {
        let backend = test_backend();

        let result: Result<u64, WorkerError> = backend.spawn_on_workers(|_db| 42u64).await;

        assert!(matches!(result, Ok(42)), "expected Ok(42), got {result:?}");

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }
}
