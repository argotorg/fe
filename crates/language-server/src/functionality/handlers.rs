use crate::backend::Backend;
use crate::doc_server::{DocServerHandle, GotoSourceRequest};

use async_lsp::lsp_types::FileChangeType;
use async_lsp::{
    ErrorCode, LanguageClient, ResponseError,
    lsp_types::{
        DocumentFormattingParams, Hover, HoverParams, InitializeParams, InitializeResult,
        InitializedParams, LogMessageParams, MessageType, Position, Range, ShowMessageParams,
        TextEdit,
    },
};

use common::InputDb;
use doc_engine::DocExtractor;
use driver::init_ingot;
use resolver::{
    ResolutionHandler, Resolver,
    files::{FilesResolver, FilesResource},
};
use rustc_hash::FxHashSet;
use url::Url;

use super::{capabilities::server_capabilities, hover::hover_helper};

use tracing::{error, info, warn};

#[derive(Debug)]
pub struct FilesNeedDiagnostics(pub Vec<NeedsDiagnostics>);

#[derive(Debug)]
pub struct NeedsDiagnostics(pub url::Url);

impl std::fmt::Display for FilesNeedDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FilesNeedDiagnostics({:?})", self.0)
    }
}

impl std::fmt::Display for NeedsDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FileNeedsDiagnostics({})", self.0)
    }
}

#[derive(Debug)]
pub struct FileChange {
    pub uri: url::Url,
    pub kind: ChangeKind,
}

#[derive(Debug)]
pub enum ChangeKind {
    Open(String),
    Create,
    Edit(Option<String>),
    Delete,
}

/// Event to navigate the doc browser to a specific path.
/// Used for: renames, go-to-definition, newly created items, etc.
#[derive(Debug, Clone)]
pub struct DocNavigate {
    /// Target path to navigate to (e.g., "mymod::MyStruct")
    pub path: String,
    /// Optional: if set, only redirect if browser is currently on this path
    pub if_on_path: Option<String>,
}

/// Custom LSP notification for cursor position updates.
/// The extension sends this when the text cursor moves in an Fe file.
/// Method: "fe/cursorPosition"
pub enum CursorPositionNotification {}

impl async_lsp::lsp_types::notification::Notification for CursorPositionNotification {
    type Params = async_lsp::lsp_types::TextDocumentPositionParams;
    const METHOD: &'static str = "fe/cursorPosition";
}

impl DocNavigate {
    /// Navigate unconditionally to a path
    pub fn to(path: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            if_on_path: None,
        }
    }

    /// Navigate only if the browser is currently viewing old_path (for renames)
    pub fn redirect(old_path: impl Into<String>, new_path: impl Into<String>) -> Self {
        Self {
            path: new_path.into(),
            if_on_path: Some(old_path.into()),
        }
    }
}

impl std::fmt::Display for DocNavigate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref from) = self.if_on_path {
            write!(f, "DocNavigate({} -> {})", from, self.path)
        } else {
            write!(f, "DocNavigate({})", self.path)
        }
    }
}

// Implementation moved to backend/mod.rs

async fn discover_and_load_ingots(
    backend: &mut Backend,
    root_path: &std::path::Path,
) -> Result<(), ResponseError> {
    let root_url = Url::from_directory_path(root_path).map_err(|_| {
        ResponseError::new(
            ErrorCode::INTERNAL_ERROR,
            format!("Invalid workspace root path: {root_path:?}"),
        )
    })?;

    driver::discover_and_init(&mut backend.db, &root_url);

    Ok(())
}

fn read_file_text_optional(path: &std::path::Path) -> Option<String> {
    // Synchronous I/O is fine here: this runs on the dedicated actor thread
    // (act_locally), NOT inside a Tokio runtime. Using tokio::spawn_blocking
    // would panic with "no reactor running".
    struct FileContent;

    impl ResolutionHandler<FilesResolver> for FileContent {
        type Item = Option<String>;

        fn handle_resolution(&mut self, _description: &Url, resource: FilesResource) -> Self::Item {
            resource.files.into_iter().next().map(|file| file.content)
        }
    }

    let file_url = Url::from_file_path(path).ok()?;
    let mut resolver = FilesResolver::new();
    let mut handler = FileContent;
    resolver.resolve(&mut handler, &file_url).ok().flatten()
}

pub async fn initialize(
    backend: &mut Backend,
    message: InitializeParams,
) -> Result<InitializeResult, ResponseError> {
    info!("initializing language server!");

    backend.definition_link_support = message
        .capabilities
        .text_document
        .as_ref()
        .and_then(|text| text.definition.as_ref())
        .and_then(|def| def.link_support)
        .unwrap_or(false);

    let root = message
        .workspace_folders
        .and_then(|folders| folders.first().cloned())
        .and_then(|folder| folder.uri.to_file_path().ok())
        .unwrap_or_else(|| std::env::current_dir().unwrap());

    backend.workspace_root = Some(root.clone());

    // Check if client supports window/showDocument
    backend.supports_show_document = message
        .capabilities
        .window
        .as_ref()
        .and_then(|w| w.show_document.as_ref())
        .map(|sd| sd.support)
        .unwrap_or(false);
    info!(
        "Client supports window/showDocument: {}",
        backend.supports_show_document
    );

    // Discover and load all ingots in the workspace
    discover_and_load_ingots(backend, &root).await?;

    // Note: Doc server is started lazily on first file change to avoid blocking init
    // Store workspace root for later use
    backend.server_info.workspace_root = Some(root.to_string_lossy().to_string());

    let capabilities = server_capabilities();
    let initialize_result = InitializeResult {
        capabilities,
        server_info: Some(async_lsp::lsp_types::ServerInfo {
            name: String::from("fe-language-server"),
            version: Some(String::from(env!("CARGO_PKG_VERSION"))),
        }),
    };
    Ok(initialize_result)
}

pub async fn initialized(
    backend: &mut Backend,
    _message: InitializedParams,
) -> Result<(), ResponseError> {
    info!("language server initialized! recieved notification!");
    info!("workspace_root: {:?}", backend.workspace_root);

    // Start doc server immediately
    update_docs(backend).await;
    info!(
        "update_docs completed, doc_server running: {}",
        backend.doc_server.is_some()
    );

    // Register file watchers so the client notifies us when .fe or fe.toml
    // files are created, changed, or deleted on disk (e.g. `fe new counter`).
    {
        use async_lsp::lsp_types::{
            DidChangeWatchedFilesRegistrationOptions, FileSystemWatcher, GlobPattern, Registration,
            RegistrationParams,
        };

        let watchers = vec![
            FileSystemWatcher {
                glob_pattern: GlobPattern::String("**/*.fe".to_string()),
                kind: None, // Create | Change | Delete
            },
            FileSystemWatcher {
                glob_pattern: GlobPattern::String("**/fe.toml".to_string()),
                kind: None,
            },
        ];

        let registration = Registration {
            id: "fe-file-watchers".to_string(),
            method: "workspace/didChangeWatchedFiles".to_string(),
            register_options: Some(
                serde_json::to_value(DidChangeWatchedFilesRegistrationOptions { watchers })
                    .expect("serialization should not fail"),
            ),
        };

        let mut client = backend.client.clone();
        if let Err(e) = client
            .register_capability(RegistrationParams {
                registrations: vec![registration],
            })
            .await
        {
            warn!("Failed to register file watchers: {:?}", e);
        } else {
            info!("Registered file watchers for *.fe and fe.toml");
        }
    }

    // Get all files from the workspace and emit diagnostics requests for one
    // representative `.fe` file per ingot in the opened workspace root.
    //
    // This avoids scheduling work for built-in core/std files on startup (which
    // can be large and delay workspace diagnostics).
    let mut seen_ingots = FxHashSet::default();
    let mut emitted_any = false;
    for (url, _file) in backend.db.workspace().all_files(&backend.db).iter() {
        if url.scheme() != "file" || !url.path().ends_with(".fe") {
            continue;
        }

        if let Some(root) = backend.workspace_root.as_ref() {
            let Ok(path) = url.to_file_path() else {
                continue;
            };
            if !path.starts_with(root) {
                continue;
            }
        }

        let Some(ingot) = backend
            .db
            .workspace()
            .containing_ingot(&backend.db, url.clone())
        else {
            continue;
        };

        if seen_ingots.insert(ingot) {
            emitted_any = true;
            let _ = backend.client.emit(NeedsDiagnostics(url.clone()));
        }
    }

    if !emitted_any {
        for (url, _file) in backend.db.workspace().all_files(&backend.db).iter() {
            let _ = backend.client.emit(NeedsDiagnostics(url.clone()));
        }
    }

    let _ = backend.client.clone().log_message(LogMessageParams {
        typ: async_lsp::lsp_types::MessageType::INFO,
        message: "language server initialized!".to_string(),
    });
    Ok(())
}

pub async fn handle_exit(_backend: &Backend, _message: ()) -> Result<(), ResponseError> {
    info!("shutting down language server");
    Ok(())
}

pub async fn handle_did_change_watched_files(
    backend: &Backend,
    message: async_lsp::lsp_types::DidChangeWatchedFilesParams,
) -> Result<(), ResponseError> {
    for event in message.changes {
        let kind = match event.typ {
            FileChangeType::CHANGED => ChangeKind::Edit(None),
            FileChangeType::CREATED => ChangeKind::Create,
            FileChangeType::DELETED => ChangeKind::Delete,
            _ => {
                tracing::warn!("unknown FileChangeType {:?}, skipping", event.typ);
                continue;
            }
        };
        let _ = backend.client.clone().emit(FileChange {
            uri: event.uri,
            kind,
        });
    }
    Ok(())
}

pub async fn handle_did_open_text_document(
    backend: &Backend,
    message: async_lsp::lsp_types::DidOpenTextDocumentParams,
) -> Result<(), ResponseError> {
    info!("file opened: {:?}", message.text_document.uri);
    let _ = backend.client.clone().emit(FileChange {
        uri: message.text_document.uri,
        kind: ChangeKind::Open(message.text_document.text),
    });
    Ok(())
}

pub async fn handle_did_change_text_document(
    backend: &Backend,
    message: async_lsp::lsp_types::DidChangeTextDocumentParams,
) -> Result<(), ResponseError> {
    info!("file changed: {:?}", message.text_document.uri);
    if message.content_changes.is_empty() {
        warn!(
            "didChange with no content changes for {:?}",
            message.text_document.uri
        );
        return Ok(());
    }
    let last = message.content_changes.last().expect("checked non-empty");
    if last.range.is_some() {
        warn!(
            "client sent incremental change while server advertises FULL sync; uri={:?}",
            message.text_document.uri
        );
    }
    backend.notify_ws(crate::ws_notify::WsServerMsg::Update {
        uri: message.text_document.uri.to_string(),
    });
    let _ = backend.client.clone().emit(FileChange {
        uri: message.text_document.uri,
        kind: ChangeKind::Edit(Some(last.text.clone())),
    });
    Ok(())
}

pub async fn handle_did_save_text_document(
    backend: &Backend,
    message: async_lsp::lsp_types::DidSaveTextDocumentParams,
) -> Result<(), ResponseError> {
    info!("file saved: {:?}", message.text_document.uri);
    backend.notify_ws(crate::ws_notify::WsServerMsg::Update {
        uri: message.text_document.uri.to_string(),
    });
    Ok(())
}

pub async fn handle_file_change(
    backend: &mut Backend,
    message: FileChange,
) -> Result<(), ResponseError> {
    if backend.is_virtual_uri(&message.uri) {
        if matches!(message.kind, ChangeKind::Edit(_))
            && backend.readonly_warnings.insert(message.uri.clone())
        {
            let _ = backend.client.clone().show_message(ShowMessageParams {
                typ: MessageType::ERROR,
                message: "Built-in library files are read-only in the editor; edits are ignored."
                    .to_string(),
            });
        }
        return Ok(());
    }

    let path = match message.uri.to_file_path() {
        Ok(p) => p,
        Err(_) => {
            error!("Failed to convert URI to path: {:?}", message.uri);
            return Err(ResponseError::new(
                ErrorCode::INVALID_PARAMS,
                format!("Invalid file URI: {}", message.uri),
            ));
        }
    };

    let path_str = match path.to_str() {
        Some(p) => p,
        None => {
            error!("Path contains invalid UTF-8: {:?}", path);
            return Err(ResponseError::new(
                ErrorCode::INVALID_PARAMS,
                "Path contains invalid UTF-8".to_string(),
            ));
        }
    };

    // Check if this is a fe.toml file
    let is_fe_toml = path
        .file_name()
        .and_then(|name| name.to_str())
        .map(|name| name == "fe.toml")
        .unwrap_or(false);

    match message.kind {
        ChangeKind::Open(contents) => {
            info!("file opened: {:?}", &path_str);
            if let Ok(url) = url::Url::from_file_path(&path) {
                backend
                    .db
                    .workspace()
                    .update(&mut backend.db, url.clone(), contents);
            }
        }
        ChangeKind::Create => {
            info!("file created: {:?}", &path_str);
            let Some(contents) = read_file_text_optional(&path) else {
                error!("Failed to read file {}", path_str);
                return Ok(());
            };
            if let Ok(url) = url::Url::from_file_path(&path) {
                backend
                    .db
                    .workspace()
                    .update(&mut backend.db, url.clone(), contents);

                // If a fe.toml was created, discover and load all files in the new ingot
                if is_fe_toml && let Some(ingot_dir) = path.parent() {
                    load_ingot_files(backend, ingot_dir)?;
                }
            }
        }
        ChangeKind::Edit(contents) => {
            info!("file edited: {:?}", &path_str);
            let contents = if let Some(text) = contents {
                text
            } else {
                let Some(contents) = read_file_text_optional(&path) else {
                    error!("Failed to read file {}", path_str);
                    return Ok(());
                };
                contents
            };
            if let Ok(url) = url::Url::from_file_path(&path) {
                backend
                    .db
                    .workspace()
                    .update(&mut backend.db, url.clone(), contents);

                // If fe.toml was modified, re-scan the ingot for any new files
                if is_fe_toml && let Some(ingot_dir) = path.parent() {
                    load_ingot_files(backend, ingot_dir)?;
                }
            }
        }
        ChangeKind::Delete => {
            info!("file deleted: {:?}", path_str);
            if let Ok(url) = url::Url::from_file_path(&path) {
                backend.db.workspace().remove(&mut backend.db, &url);
            }

            // When a fe.toml is deleted, re-init the parent workspace so that
            // dependents get their diagnostics recomputed (the removed ingot's
            // imports will now fail in other members).
            if is_fe_toml {
                if let Ok(ingot_url) = Url::from_directory_path(path.parent().unwrap_or(&path)) {
                    let workspace_root = backend
                        .db
                        .dependency_graph()
                        .workspace_roots(&backend.db)
                        .into_iter()
                        .filter(|root| {
                            ingot_url.as_str().starts_with(root.as_str()) && *root != ingot_url
                        })
                        .max_by_key(|root| root.as_str().len());

                    if let Some(ref workspace_root) = workspace_root {
                        info!(
                            "Re-initializing workspace {:?} after ingot deletion",
                            workspace_root
                        );
                        let _ = init_ingot(&mut backend.db, workspace_root);
                    }
                }

                // Emit diagnostics for all workspace files
                let all_files: Vec<_> = backend
                    .db
                    .workspace()
                    .all_files(&backend.db)
                    .iter()
                    .map(|(url, _file)| url)
                    .collect();
                for url in all_files {
                    let _ = backend.client.emit(NeedsDiagnostics(url));
                }
                return Ok(());
            }
        }
    }

    // Update documentation (starts server on first change if needed)
    update_docs(backend).await;

    let _ = backend.client.emit(NeedsDiagnostics(message.uri));
    Ok(())
}

fn load_ingot_files(
    backend: &mut Backend,
    ingot_dir: &std::path::Path,
) -> Result<(), ResponseError> {
    info!("Loading ingot files from: {:?}", ingot_dir);

    let ingot_url = Url::from_directory_path(ingot_dir).map_err(|_| {
        ResponseError::new(
            ErrorCode::INTERNAL_ERROR,
            format!("Invalid ingot path: {ingot_dir:?}"),
        )
    })?;

    // If this ingot is under a known workspace root, re-init the workspace so
    // that the new member gets registered and dependency edges from other
    // members (e.g. counter_test → counter) are established.
    let workspace_root = backend
        .db
        .dependency_graph()
        .workspace_roots(&backend.db)
        .into_iter()
        .filter(|root| ingot_url.as_str().starts_with(root.as_str()) && *root != ingot_url)
        .max_by_key(|root| root.as_str().len());

    if let Some(ref workspace_root) = workspace_root {
        info!(
            "Re-initializing workspace {:?} after new member ingot {:?}",
            workspace_root, ingot_dir
        );
        let had_diagnostics = init_ingot(&mut backend.db, workspace_root);
        if had_diagnostics {
            warn!(
                "Workspace re-initialization produced diagnostics for {:?}",
                workspace_root
            );
        }
    }

    let had_diagnostics = init_ingot(&mut backend.db, &ingot_url);
    if had_diagnostics {
        warn!(
            "Ingot initialization produced diagnostics for {:?}",
            ingot_dir
        );
    }

    // Emit diagnostics for all files that were loaded
    let all_files: Vec<_> = backend
        .db
        .workspace()
        .all_files(&backend.db)
        .iter()
        .map(|(url, _file)| url)
        .collect();

    for url in all_files {
        let _ = backend.client.emit(NeedsDiagnostics(url));
    }

    Ok(())
}

pub async fn handle_files_need_diagnostics(
    backend: &Backend,
    message: FilesNeedDiagnostics,
) -> Result<(), ResponseError> {
    let t_handler = std::time::Instant::now();
    let FilesNeedDiagnostics(need_diagnostics) = message;
    let mut client = backend.client.clone();

    // Track all requested URIs so we can clear stale diagnostics for any that
    // don't appear in the computed diagnostics (e.g. deleted files, fixed errors)
    let mut pending_clear: FxHashSet<url::Url> = need_diagnostics
        .iter()
        .map(|NeedsDiagnostics(u)| u.clone())
        .collect();

    let ingots_need_diagnostics: FxHashSet<_> = need_diagnostics
        .iter()
        .filter_map(|NeedsDiagnostics(url)| {
            let url = backend.map_client_uri_to_internal(url.clone());
            backend
                .db
                .workspace()
                .containing_ingot(&backend.db, url.clone())
        })
        .collect();

    tracing::debug!(
        "[fe:timing] handle_files_need_diagnostics: {} URIs -> {} ingots",
        need_diagnostics.len(),
        ingots_need_diagnostics.len()
    );

    for ingot in ingots_need_diagnostics {
        // Test-only: trigger an induced panic to verify the catch_unwind
        // recovery path. This lives here (not in diagnostics_for_ingot) so
        // that unit tests calling diagnostics_for_ingot directly never
        // interact with the latch — only the full LSP handler path does.
        #[cfg(test)]
        if crate::lsp_diagnostics::FORCE_DIAGNOSTIC_PANIC
            .swap(false, std::sync::atomic::Ordering::SeqCst)
        {
            panic!("__test_induced_diagnostic_panic__");
        }

        // Wrap diagnostics computation in catch_unwind: analysis passes
        // (parsing, type checking, etc.) can panic on malformed intermediate
        // text during editing. Without this, a panic kills the Backend actor
        // and all subsequent LSP requests fail with SendError.
        use crate::lsp_diagnostics::LspDiagnostics;
        let diagnostics_map = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            backend.db.diagnostics_for_ingot(ingot)
        })) {
            Ok(map) => map,
            Err(panic_info) => {
                // Salsa uses panics for query cancellation — never swallow them.
                if panic_info.is::<salsa::Cancelled>() {
                    std::panic::resume_unwind(panic_info);
                }
                let msg = panic_info
                    .downcast_ref::<&str>()
                    .copied()
                    .or_else(|| panic_info.downcast_ref::<String>().map(|s| s.as_str()))
                    .unwrap_or("<non-string panic>");
                error!("diagnostics_for_ingot panicked (skipping): {msg}");
                continue;
            }
        };

        for (internal_uri, diags) in diagnostics_map.iter() {
            let uri = backend.map_internal_uri_to_client(internal_uri.clone());
            pending_clear.remove(&uri);
            let mut diagnostic = diags.clone();
            map_related_info_uris(backend, &mut diagnostic);
            let diagnostics_params = async_lsp::lsp_types::PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics: diagnostic,
                version: None,
            };
            if let Err(e) = client.publish_diagnostics(diagnostics_params) {
                error!("Failed to publish diagnostics for {}: {:?}", uri, e);
            }
        }
    }

    // Clear diagnostics for any requested URIs that weren't covered above
    for uri in pending_clear {
        let diagnostics_params = async_lsp::lsp_types::PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics: Vec::new(),
            version: None,
        };
        info!("Clearing stale diagnostics for {:?}", uri);
        if let Err(e) = client.publish_diagnostics(diagnostics_params) {
            error!("Failed to clear diagnostics for {}: {:?}", uri, e);
        }
    }

    tracing::debug!(
        "[fe:timing] handle_files_need_diagnostics total: {:?}",
        t_handler.elapsed()
    );
    Ok(())
}

fn map_related_info_uris(backend: &Backend, diagnostics: &mut [async_lsp::lsp_types::Diagnostic]) {
    for diagnostic in diagnostics.iter_mut() {
        let Some(related) = diagnostic.related_information.as_mut() else {
            continue;
        };
        for info in related.iter_mut() {
            info.location.uri = backend.map_internal_uri_to_client(info.location.uri.clone());
        }
    }
}

/// Handle doc navigation events - notify the browser to navigate
pub async fn handle_doc_navigate(
    backend: &mut Backend,
    event: DocNavigate,
) -> Result<(), ResponseError> {
    info!("Doc navigate: {}", event);

    // Notify the doc server to send navigation message to browser
    if let Some(ref doc_server) = backend.doc_server {
        doc_server.notify_navigate(&event.path, event.if_on_path.as_deref());
    }

    // If this was a rename (conditional redirect), also update the doc index
    if event.if_on_path.is_some() {
        update_docs(backend).await;
    }

    Ok(())
}

/// Handle goto source requests from the doc viewer.
/// This tells the editor to navigate to the source location.
pub async fn handle_goto_source(
    backend: &Backend,
    request: GotoSourceRequest,
) -> Result<(), ResponseError> {
    info!("Goto source: {}", request);

    // Convert the file path to a URL
    let file_url = Url::from_file_path(&request.file).map_err(|_| {
        ResponseError::new(
            ErrorCode::INVALID_PARAMS,
            format!("Invalid file path: {}", request.file),
        )
    })?;

    // LSP uses 0-based line/column, but source locations are typically 1-based
    let line = request.line.saturating_sub(1);
    let column = request.column.saturating_sub(1);
    let position = Position::new(line, column);

    if backend.supports_show_document {
        // Use LSP window/showDocument (LSP 3.16+)
        let params = ShowDocumentParams {
            uri: file_url,
            external: None,
            take_focus: Some(true),
            selection: Some(Range::new(position, position)),
        };

        let mut client = backend.client.clone();
        match client.show_document(params).await {
            Ok(result) => {
                if result.success {
                    info!("Successfully opened document in editor");
                } else {
                    warn!("Editor reported failure opening document");
                }
            }
            Err(e) => {
                error!("Failed to open document: {:?}", e);
            }
        }
    } else {
        // Fallback: try opening the file directly
        // Many editors support file:///path#L<line> format
        let url_with_line = format!("{}:{}", request.file, request.line);
        info!(
            "window/showDocument not supported, trying fallback: {}",
            url_with_line
        );

        if let Err(e) = open::that(&url_with_line) {
            // If that fails, try just opening the file
            warn!("Failed to open with line number ({}), trying plain file", e);
            if let Err(e) = open::that(&request.file) {
                error!("Failed to open file: {}", e);
            }
        }
    }

    Ok(())
}

pub async fn handle_hover_request(
    backend: &Backend,
    message: HoverParams,
) -> Result<Option<Hover>, ResponseError> {
    let url = backend.map_client_uri_to_internal(
        message
            .text_document_position_params
            .text_document
            .uri
            .clone(),
    );
    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        warn!("handle_hover_request failed to get file for url: `{url}`");
        return Ok(None);
    };

    info!("handling hover request in file: {:?}", file);
    let result = hover_helper(&backend.db, file, message).unwrap_or_else(|e| {
        error!("Error handling hover: {:?}", e);
        super::hover::HoverResult {
            hover: None,
            doc_path: None,
        }
    });

    // Emit navigation event for auto-follow mode (browser will ignore if disabled)
    if let Some(doc_path) = result.doc_path {
        let _ = backend.client.clone().emit(DocNavigate::to(doc_path));
    }

    info!("sending hover response: {:?}", result.hover);
    Ok(result.hover)
}

/// Handle cursor position notifications from the extension.
/// This enables text cursor following for the documentation viewer.
pub async fn handle_cursor_position(
    backend: &Backend,
    message: async_lsp::lsp_types::TextDocumentPositionParams,
) -> Result<(), ResponseError> {
    use crate::util::to_offset_from_position;
    use hir::{core::semantic::reference::Target, lower::map_file_to_mod};

    let path_str = message.text_document.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(());
    };
    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(());
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(message.position, file_text.as_str());
    let top_mod = map_file_to_mod(&backend.db, file);

    // Look up the symbol at cursor position
    let resolution = top_mod.target_at(&backend.db, cursor);

    // Skip navigation if ambiguous (don't auto-follow to an arbitrary choice)
    if resolution.is_ambiguous() {
        return Ok(());
    }

    if let Some(Target::Scope(scope)) = resolution.first() {
        // Use shared doc-engine function for qualified path
        if let Some(doc_path) = doc_engine::scope_to_doc_path(&backend.db, *scope) {
            // Emit navigation event (browser will ignore if auto-follow is disabled)
            let _ = backend.client.clone().emit(DocNavigate::to(doc_path));
        }
    }

    Ok(())
}

pub async fn handle_shutdown(backend: &Backend, _message: ()) -> Result<(), ResponseError> {
    info!("received shutdown request");

    // Clean up server info file
    if let Some(ref root) = backend.workspace_root {
        crate::doc_server::LspServerInfo::remove_from_workspace(root);
    }

    Ok(())
}

pub async fn handle_formatting(
    backend: &Backend,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>, ResponseError> {
    if backend.is_virtual_uri(&params.text_document.uri) {
        return Ok(None);
    }

    let url = backend.map_client_uri_to_internal(params.text_document.uri.clone());

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        warn!("handle_formatting: file not found `{url}`");
        return Ok(None);
    };

    let source = file.text(&backend.db);

    match fmt::format_str(source, &fmt::Config::default()) {
        Ok(formatted) => {
            let end_line = source.split('\n').count().saturating_sub(1) as u32;
            let end_character = source.rsplit('\n').next().map_or(0, |l| l.len()) as u32;
            let range = Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: end_line,
                    character: end_character,
                },
            };
            Ok(Some(vec![TextEdit {
                range,
                new_text: formatted,
            }]))
        }
        Err(fmt::FormatError::ParseErrors(errs)) => {
            info!("formatting skipped: {} parse error(s)", errs.len());
            Ok(None)
        }
        Err(fmt::FormatError::Io(_)) => Ok(None),
    }
}
