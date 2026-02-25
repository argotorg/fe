use crate::backend::Backend;

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
use driver::init_ingot;
use resolver::workspace::discover_context;
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

    let discovery = discover_context(&root_url, true).map_err(|e| {
        ResponseError::new(ErrorCode::INTERNAL_ERROR, format!("Discovery error: {e}"))
    })?;

    if let Some(workspace_root) = discovery.workspace_root.as_ref() {
        let had_diagnostics = init_ingot(&mut backend.db, workspace_root);
        if had_diagnostics {
            warn!("Ingot initialization produced diagnostics for workspace root");
        }
    }

    for ingot_url in &discovery.ingot_roots {
        // Skip if already initialized as workspace root above
        if discovery.workspace_root.as_ref() == Some(ingot_url) {
            continue;
        }
        let had_diagnostics = init_ingot(&mut backend.db, ingot_url);
        if had_diagnostics {
            warn!(
                "Ingot initialization produced diagnostics for {:?}",
                ingot_url
            );
        }
    }

    if discovery.workspace_root.is_none() && discovery.ingot_roots.is_empty() {
        let had_diagnostics = init_ingot(&mut backend.db, &root_url);
        if had_diagnostics {
            warn!("Ingot initialization produced diagnostics for workspace root");
        }
    }

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

    // Discover and load all ingots in the workspace
    discover_and_load_ingots(backend, &root).await?;

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
    backend: &Backend,
    _message: InitializedParams,
) -> Result<(), ResponseError> {
    info!("language server initialized! recieved notification!");

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
    let _ = backend.client.clone().emit(FileChange {
        uri: message.text_document.uri,
        kind: ChangeKind::Edit(Some(last.text.clone())),
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
    let response = hover_helper(&backend.db, file, message).unwrap_or_else(|e| {
        error!("Error handling hover: {:?}", e);
        None
    });
    info!("sending hover response: {:?}", response);
    Ok(response)
}

pub async fn handle_shutdown(_backend: &Backend, _message: ()) -> Result<(), ResponseError> {
    info!("received shutdown request");
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
