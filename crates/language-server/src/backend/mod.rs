use async_lsp::ClientSocket;
use driver::DriverDataBase;
use introspection_config::FeToolingConfig;
use rustc_hash::{FxHashMap, FxHashSet};
use std::panic::{self, AssertUnwindSafe};
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use tokio::sync::broadcast;
use trace_query::TraceIntrospectionService;
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

pub struct Backend {
    pub(super) client: ClientSocket,
    pub(super) db: DriverDataBase,
    pub(super) workers: tokio::runtime::Runtime,
    pub(super) virtual_files: Option<VirtualFiles>,
    pub(super) readonly_warnings: FxHashSet<Url>,
    pub(super) definition_link_support: bool,
    pub(super) doc_nav_tx: Option<broadcast::Sender<String>>,
    pub(super) doc_reload_tx: Option<broadcast::Sender<String>>,
    pub(super) doc_reload_generation: Arc<AtomicU64>,
    pub(super) docs_url: Option<String>,
    pub(super) lsp_workspace_root: Option<PathBuf>,
    pub(super) tooling_config: FeToolingConfig,
    document_versions: FxHashMap<Url, i32>,
    trace_cache: FxHashMap<TraceCacheKey, TraceIntrospectionService>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct TraceCacheKey {
    uri: Url,
    document_version: i32,
    config_hash: String,
}

impl Backend {
    pub fn new(
        client: ClientSocket,
        doc_nav_tx: Option<broadcast::Sender<String>>,
        doc_reload_tx: Option<broadcast::Sender<String>>,
        docs_url: Option<String>,
        tooling_config: FeToolingConfig,
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
            doc_reload_tx,
            doc_reload_generation: Arc::new(AtomicU64::new(0)),
            docs_url,
            lsp_workspace_root: None,
            tooling_config,
            document_versions: FxHashMap::default(),
            trace_cache: FxHashMap::default(),
        }
    }

    pub fn tooling_config(&self) -> &FeToolingConfig {
        &self.tooling_config
    }

    pub fn set_document_version(&mut self, uri: Url, version: i32) {
        self.document_versions.insert(uri.clone(), version);
        self.clear_trace_cache_for_uri(&uri);
    }

    pub fn clear_document_version(&mut self, uri: &Url) {
        self.document_versions.remove(uri);
        self.clear_trace_cache_for_uri(uri);
    }

    pub fn document_version(&self, uri: &Url) -> Option<i32> {
        self.document_versions.get(uri).copied()
    }

    pub fn cached_trace_service(
        &self,
        uri: &Url,
        document_version: i32,
        config_hash: &str,
    ) -> Option<TraceIntrospectionService> {
        self.trace_cache
            .get(&TraceCacheKey {
                uri: uri.clone(),
                document_version,
                config_hash: config_hash.to_string(),
            })
            .cloned()
    }

    pub fn cache_trace_service(
        &mut self,
        uri: Url,
        document_version: i32,
        config_hash: String,
        service: TraceIntrospectionService,
    ) {
        self.trace_cache.insert(
            TraceCacheKey {
                uri,
                document_version,
                config_hash,
            },
            service,
        );
    }

    pub fn clear_trace_cache_for_uri(&mut self, uri: &Url) {
        self.trace_cache.retain(|key, _| &key.uri != uri);
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
        let (tx, rx) = futures::channel::oneshot::channel();
        let db = self.db.clone();
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
        Backend::new(
            client_socket,
            None,
            None,
            None,
            introspection_config::FeToolingConfig::default(),
        )
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn document_version_tracking_records_and_clears_lsp_versions() {
        let mut backend = test_backend();
        let uri = Url::parse("file:///workspace/src/lib.fe").unwrap();

        assert_eq!(backend.document_version(&uri), None);
        backend.set_document_version(uri.clone(), 7);
        assert_eq!(backend.document_version(&uri), Some(7));
        backend.clear_document_version(&uri);
        assert_eq!(backend.document_version(&uri), None);

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
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
