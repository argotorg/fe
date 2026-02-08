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

    let discovery = discover_context(&root_url).map_err(|e| {
        ResponseError::new(ErrorCode::INTERNAL_ERROR, format!("Discovery error: {e}"))
    })?;

    if let Some(workspace_root) = discovery.workspace_root.as_ref() {
        let had_diagnostics = init_ingot(&mut backend.db, workspace_root);
        if had_diagnostics {
            warn!("Ingot initialization produced diagnostics for workspace root");
        }
    }

    for ingot_url in &discovery.ingot_roots {
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

async fn read_file_text_optional(path: std::path::PathBuf) -> Option<String> {
    tokio::task::spawn_blocking(move || {
        struct FileContent;

        impl ResolutionHandler<FilesResolver> for FileContent {
            type Item = Option<String>;

            fn handle_resolution(
                &mut self,
                _description: &Url,
                resource: FilesResource,
            ) -> Self::Item {
                resource.files.into_iter().next().map(|file| file.content)
            }
        }

        let file_url = Url::from_file_path(path).ok()?;
        let mut resolver = FilesResolver::new();
        let mut handler = FileContent;
        resolver.resolve(&mut handler, &file_url).ok().flatten()
    })
    .await
    .ok()
    .flatten()
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
    if backend.is_builtin_tmp_uri(&message.uri) {
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

    if backend.classify_git_cache_path(&message.uri, &path) {
        if matches!(message.kind, ChangeKind::Edit(_))
            && backend.readonly_warnings.insert(message.uri.clone())
        {
            let _ = backend.client.clone().show_message(ShowMessageParams {
                typ: MessageType::ERROR,
                message:
                    "Git dependency cache files are read-only in the editor; edits are ignored."
                        .to_string(),
            });
        }
        return Ok(());
    }

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
            let Some(contents) = read_file_text_optional(path.clone()).await else {
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
                    load_ingot_files(backend, ingot_dir).await?;
                }
            }
        }
        ChangeKind::Edit(contents) => {
            info!("file edited: {:?}", &path_str);
            let contents = if let Some(text) = contents {
                text
            } else {
                let Some(contents) = read_file_text_optional(path.clone()).await else {
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
    let FilesNeedDiagnostics(need_diagnostics) = message;
    let mut client = backend.client.clone();

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

    for ingot in ingots_need_diagnostics {
        // Get diagnostics per file
        use crate::lsp_diagnostics::LspDiagnostics;
        let diagnostics_map = backend.db.diagnostics_for_ingot(ingot);

        for (internal_uri, diags) in diagnostics_map.iter() {
            let uri = backend.map_internal_uri_to_client(internal_uri.clone());
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
    if backend.is_builtin_tmp_uri(&params.text_document.uri)
        || backend.is_git_cache_uri(&params.text_document.uri)
    {
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
