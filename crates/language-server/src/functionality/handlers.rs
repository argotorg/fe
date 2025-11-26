use crate::backend::Backend;

use async_lsp::lsp_types::FileChangeType;
use async_lsp::{
    ErrorCode, LanguageClient, ResponseError,
    lsp_types::{
        Hover, HoverParams, InitializeParams, InitializeResult, InitializedParams, LogMessageParams,
    },
};

use common::InputDb;
use driver::init_ingot;
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

                // If this is a .fe file, check if its ingot is loaded
                if path.extension().and_then(|s| s.to_str()) == Some("fe") {
                    // Use init_ingot to find and load the ingot root
                    // It will walk up from the file's directory to find fe.toml
                    let diagnostics = init_ingot(&mut backend.db, &url);

                    // Log any diagnostics
                    for diagnostic in diagnostics {
                        warn!(
                            "Ingot initialization diagnostic for file {:?}: {}",
                            path, diagnostic
                        );
                    }
                }
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
            // url is already a url::Url
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

        info!(
            "Computed diagnostics: {:?}",
            diagnostics_map.keys().collect::<Vec<_>>()
        );
        for uri in diagnostics_map.keys() {
            let diagnostic = diagnostics_map.get(uri).cloned().unwrap_or_default();
            let diagnostics_params = async_lsp::lsp_types::PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics: diagnostic,
                version: None,
            };
            info!("Publishing diagnostics for URI: {:?}", uri);
            let _ = client
                .publish_diagnostics(diagnostics_params)
                .map_err(|e| error!("Failed to publish diagnostics for {}: {:?}", uri, e));
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
