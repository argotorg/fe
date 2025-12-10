use crate::backend::Backend;
use crate::doc_server::{DocServerHandle, GotoSourceRequest};

use async_lsp::lsp_types::FileChangeType;
use async_lsp::{
    ErrorCode, LanguageClient, ResponseError,
    lsp_types::{
        ExecuteCommandParams, Hover, HoverParams, InitializeParams, InitializeResult,
        InitializedParams, LogMessageParams, Position, Range, ShowDocumentParams,
    },
};

use common::InputDb;
use doc_engine::DocExtractor;
use driver::init_ingot;
use hir::hir_def::HirIngot;
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
    // Find all fe.toml files in the workspace
    let pattern = format!("{}/**/fe.toml", root_path.to_string_lossy());
    let config_paths = glob::glob(&pattern)
        .map_err(|e| ResponseError::new(ErrorCode::INTERNAL_ERROR, format!("Glob error: {e}")))?
        .filter_map(Result::ok)
        .collect::<Vec<_>>();

    // Initialize each ingot using the driver's init_ingot function
    for config_path in &config_paths {
        let ingot_dir = config_path.parent().unwrap();
        let ingot_url = Url::from_directory_path(ingot_dir).map_err(|_| {
            ResponseError::new(
                ErrorCode::INTERNAL_ERROR,
                format!("Invalid ingot path: {ingot_dir:?}"),
            )
        })?;

        let diagnostics = init_ingot(&mut backend.db, &ingot_url);

        // Log any diagnostics
        for diagnostic in diagnostics {
            warn!(
                "Ingot initialization diagnostic for {:?}: {}",
                ingot_dir, diagnostic
            );
        }
    }

    // Also check if the root itself is an ingot (no fe.toml in subdirectories)
    if config_paths.is_empty() {
        let root_url = Url::from_directory_path(root_path).map_err(|_| {
            ResponseError::new(
                ErrorCode::INTERNAL_ERROR,
                format!("Invalid workspace root path: {root_path:?}"),
            )
        })?;

        let diagnostics = init_ingot(&mut backend.db, &root_url);

        // Log any diagnostics
        for diagnostic in diagnostics {
            warn!(
                "Ingot initialization diagnostic for workspace root: {}",
                diagnostic
            );
        }
    }

    Ok(())
}

pub async fn initialize(
    backend: &mut Backend,
    message: InitializeParams,
) -> Result<InitializeResult, ResponseError> {
    info!("initializing language server!");

    let root = message
        .workspace_folders
        .and_then(|folders| folders.first().cloned())
        .and_then(|folder| folder.uri.to_file_path().ok())
        .unwrap_or_else(|| std::env::current_dir().unwrap());

    // Store workspace root
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

    // Get all files from the workspace
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
            _ => unreachable!(),
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
    let _ = backend.client.clone().emit(FileChange {
        uri: message.text_document.uri,
        kind: ChangeKind::Edit(Some(message.content_changes[0].text.clone())),
    });
    Ok(())
}

pub async fn handle_did_save_text_document(
    _backend: &Backend,
    message: async_lsp::lsp_types::DidSaveTextDocumentParams,
) -> Result<(), ResponseError> {
    info!("file saved: {:?}", message.text_document.uri);
    Ok(())
}

pub async fn handle_file_change(
    backend: &mut Backend,
    message: FileChange,
) -> Result<(), ResponseError> {
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
            let contents = match tokio::fs::read_to_string(&path).await {
                Ok(c) => c,
                Err(e) => {
                    error!("Failed to read file {}: {}", path_str, e);
                    return Ok(());
                }
            };
            if let Ok(url) = url::Url::from_file_path(&path) {
                backend
                    .db
                    .workspace()
                    .update(&mut backend.db, url.clone(), contents);

                // If a fe.toml was created, discover and load all files in the new ingot
                if is_fe_toml && let Some(ingot_dir) = path.parent() {
                    load_ingot_files(backend, ingot_dir).await?;
                }
            }
        }
        ChangeKind::Edit(contents) => {
            info!("file edited: {:?}", &path_str);
            let contents = if let Some(text) = contents {
                text
            } else {
                match tokio::fs::read_to_string(&path).await {
                    Ok(c) => c,
                    Err(e) => {
                        error!("Failed to read file {}: {}", path_str, e);
                        return Ok(());
                    }
                }
            };
            if let Ok(url) = url::Url::from_file_path(&path) {
                backend
                    .db
                    .workspace()
                    .update(&mut backend.db, url.clone(), contents);

                // If fe.toml was modified, re-scan the ingot for any new files
                if is_fe_toml && let Some(ingot_dir) = path.parent() {
                    load_ingot_files(backend, ingot_dir).await?;
                }
            }
        }
        ChangeKind::Delete => {
            info!("file deleted: {:?}", path_str);
            if let Ok(url) = url::Url::from_file_path(path) {
                backend.db.workspace().remove(&mut backend.db, &url);
            }
        }
    }

    // Update documentation (starts server on first change if needed)
    update_docs(backend).await;

    let _ = backend.client.emit(NeedsDiagnostics(message.uri));
    Ok(())
}

async fn load_ingot_files(
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

    let diagnostics = init_ingot(&mut backend.db, &ingot_url);

    // Log any diagnostics
    for diagnostic in diagnostics {
        warn!(
            "Ingot initialization diagnostic for {:?}: {}",
            ingot_dir, diagnostic
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
    let FilesNeedDiagnostics(need_diagnostics) = message;
    let mut client = backend.client.clone();

    let ingots_need_diagnostics: FxHashSet<_> = need_diagnostics
        .iter()
        .filter_map(|NeedsDiagnostics(url)| {
            backend
                .db
                .workspace()
                .containing_ingot(&backend.db, url.clone())
        })
        .collect();

    for ingot in ingots_need_diagnostics {
        // Get diagnostics per file
        use crate::lsp_diagnostics::LspDiagnostics;
        let diagnostics_map = backend.db.diagnostics_for_ingot(ingot);

        for uri in diagnostics_map.keys() {
            let diagnostic = diagnostics_map.get(uri).cloned().unwrap_or_default();
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
    Ok(())
}

/// Start the doc server if not running, and update the documentation index.
/// This is called after file changes to keep docs in sync.
async fn update_docs(backend: &mut Backend) {
    info!(
        "update_docs called, doc_server exists: {}",
        backend.doc_server.is_some()
    );

    // Start doc server if not already running
    if backend.doc_server.is_none() {
        info!("Starting doc server...");
        // Use the workers runtime since act_locally runs on a separate thread without tokio
        let client = backend.client.clone();
        let supports_goto_source = backend.supports_show_document;
        let handle_result = backend
            .workers
            .block_on(DocServerHandle::start(client, supports_goto_source));
        match handle_result {
            Ok(handle) => {
                info!("Started documentation server at {}", handle.url);

                // Log to client so user can see the URL
                let _ = backend.client.clone().log_message(LogMessageParams {
                    typ: async_lsp::lsp_types::MessageType::INFO,
                    message: format!("Fe docs available at {}", handle.url),
                });

                // Update server info
                backend.server_info.docs_url = Some(handle.url.clone());

                // Write server info file if we have a workspace root
                if let Some(ref root) = backend.workspace_root {
                    info!("Writing .fe-lsp.json to {:?}", root);
                    match backend.server_info.write_to_workspace(root) {
                        Ok(()) => info!("Successfully wrote .fe-lsp.json"),
                        Err(e) => warn!("Failed to write server info: {}", e),
                    }
                } else {
                    warn!("No workspace root set, cannot write .fe-lsp.json");
                }

                backend.doc_server = Some(handle);
            }
            Err(e) => {
                error!("Failed to start doc server: {}", e);
                return;
            }
        }
    }

    // Extract docs from all ingots
    let extractor = if let Some(ref root) = backend.workspace_root {
        DocExtractor::new(&backend.db).with_root_path(root)
    } else {
        DocExtractor::new(&backend.db)
    };
    let mut combined_index = doc_engine::DocIndex::default();

    // Get all unique ingots from workspace files
    let ingots: FxHashSet<_> = backend
        .db
        .workspace()
        .all_files(&backend.db)
        .iter()
        .filter_map(|(url, _)| {
            backend
                .db
                .workspace()
                .containing_ingot(&backend.db, url.clone())
        })
        .collect();

    // Collect trait impl links from all ingots
    let mut all_trait_impl_links = Vec::new();

    for ingot in ingots {
        // Skip ingots without valid config (no fe.toml or missing name)
        let has_valid_config = ingot
            .config(&backend.db)
            .and_then(|c| c.metadata.name)
            .is_some();
        if !has_valid_config {
            continue;
        }

        // Extract items from ALL modules in the ingot (not just root)
        for top_mod in ingot.all_modules(&backend.db) {
            for item in top_mod.children_nested(&backend.db) {
                if let Some(doc_item) = extractor.extract_item_for_ingot(item, ingot) {
                    combined_index.items.push(doc_item);
                }
            }
        }
        // Build module tree from root (it handles file-based children)
        let root_mod = ingot.root_mod(&backend.db);
        let module_tree = extractor.build_module_tree_for_ingot(ingot, root_mod);
        combined_index.modules.extend(module_tree);

        // Extract trait implementation links
        all_trait_impl_links.extend(extractor.extract_trait_impl_links(ingot));
    }

    // Link trait implementations to their target types
    combined_index.link_trait_impls(all_trait_impl_links);

    // Link types in signatures to their documentation pages
    combined_index.link_signature_types();

    // Sort for consistent ordering
    combined_index.items.sort_by(|a, b| a.path.cmp(&b.path));
    combined_index.modules.sort_by(|a, b| a.path.cmp(&b.path));

    // Update the doc server (use workers runtime for tokio async)
    if let Some(ref doc_server) = backend.doc_server {
        backend
            .workers
            .block_on(doc_server.update_index(combined_index));
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
    let path_str = message // Renamed to path_str to avoid confusion with Url
        .text_document_position_params
        .text_document
        .uri
        .path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        warn!("handle_hover_request failed to convert path to URL: `{path_str}`");
        return Ok(None);
    };
    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        warn!(
            "handle_hover_request failed to get file for url: `{url}` (original path: `{path_str}`)"
        );
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

pub async fn handle_execute_command(
    backend: &mut Backend,
    params: ExecuteCommandParams,
) -> Result<Option<serde_json::Value>, ResponseError> {
    info!("execute command: {:?}", params.command);

    match params.command.as_str() {
        "fe.openDocs" => {
            // Start doc server if not running
            if backend.doc_server.is_none() {
                info!("Doc server not running, starting it...");
                update_docs(backend).await;
            }

            if let Some(ref doc_server) = backend.doc_server {
                // Extract optional item path from arguments
                let item_path = params.arguments.first().and_then(|arg| arg.as_str());

                // Build the full URL
                let full_url = if let Some(path) = item_path {
                    format!("{}/doc/{}", doc_server.url.trim_end_matches('/'), path)
                } else {
                    doc_server.url.clone()
                };

                info!("Opening docs at {}", full_url);

                // Use open crate to open in default browser
                if let Err(e) = open::that(&full_url) {
                    error!("Failed to open browser: {}", e);
                    return Err(ResponseError::new(
                        ErrorCode::INTERNAL_ERROR,
                        format!("Failed to open browser: {}", e),
                    ));
                }

                Ok(Some(serde_json::json!({ "opened": full_url })))
            } else {
                Err(ResponseError::new(
                    ErrorCode::INTERNAL_ERROR,
                    "Failed to start documentation server".to_string(),
                ))
            }
        }
        _ => Err(ResponseError::new(
            ErrorCode::INVALID_PARAMS,
            format!("Unknown command: {}", params.command),
        )),
    }
}
