use async_lsp::lsp_types::{
    ApplyWorkspaceEditParams, CreateFile, CreateFileOptions, DocumentChangeOperation,
    DocumentChanges, ExecuteCommandParams, MessageType, OneOf,
    OptionalVersionedTextDocumentIdentifier, Position, Range, ResourceOp, ShowDocumentParams,
    ShowMessageParams, TextDocumentEdit, TextEdit, WorkspaceEdit,
};
use async_lsp::{ErrorCode, LanguageClient, ResponseError};
use common::InputDb;
use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use hir::lower::map_file_to_mod;
use mir::build_runtime_package;
use serde::Deserialize;
use serde_json::Value;
use url::Url;

use crate::backend::{Backend, TraceViewerSelection};

enum CodegenKind {
    Mir,
    SonatinaIr,
}

impl CodegenKind {
    fn extension(&self) -> &'static str {
        match self {
            CodegenKind::Mir => "mir",
            CodegenKind::SonatinaIr => "sonatina",
        }
    }

    fn label(&self) -> &'static str {
        match self {
            CodegenKind::Mir => "MIR",
            CodegenKind::SonatinaIr => "Sonatina IR",
        }
    }
}

fn generate_codegen_string(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    kind: &CodegenKind,
) -> Result<String, String> {
    match kind {
        CodegenKind::Mir => {
            let package = build_runtime_package(db, top_mod)
                .map_err(|e| format!("runtime package lowering: {e}"))?;
            Ok(format!("{package:#?}"))
        }
        CodegenKind::SonatinaIr => codegen::emit_module_sonatina_ir(db, top_mod)
            .map_err(|e| format!("Sonatina IR emit: {e}")),
    }
}

pub async fn handle_execute_command(
    backend: &mut Backend,
    params: ExecuteCommandParams,
) -> Result<Option<Value>, ResponseError> {
    // Handle fe.openDocs separately — it opens a URL, not codegen
    if params.command == "fe.openDocs" {
        return handle_open_docs(backend, &params.arguments).await;
    }
    if params.command == "fe.trace.openWorkbench" {
        return handle_open_trace_workbench(backend, &params.arguments).await;
    }
    if matches!(
        params.command.as_str(),
        "fe.traceLoop" | "fe.explainLocal" | "fe.gasBreakdown" | "fe.openOriginGraph"
    ) {
        return Ok(Some(serde_json::json!({
            "status": "trace command accepted",
            "command": params.command,
            "note": "live CLI/LSP HTTP execution is wired in the next phase; reports are served by trace-query"
        })));
    }

    let kind = match params.command.as_str() {
        "fe.viewMir" => CodegenKind::Mir,
        "fe.viewSonatinaIr" => CodegenKind::SonatinaIr,
        other => {
            return Err(ResponseError::new(
                ErrorCode::INVALID_PARAMS,
                format!("unknown command: {other}"),
            ));
        }
    };

    let uri_str = params
        .arguments
        .first()
        .and_then(|v| v.as_str())
        .ok_or_else(|| {
            ResponseError::new(
                ErrorCode::INVALID_PARAMS,
                "missing URI argument".to_string(),
            )
        })?;

    let uri = Url::parse(uri_str)
        .map_err(|e| ResponseError::new(ErrorCode::INVALID_PARAMS, format!("invalid URI: {e}")))?;
    let internal_uri = backend.map_client_uri_to_internal(uri.clone());

    // Derive a relative source path for the output filename
    let source_path = uri.path().rsplit('/').next().unwrap_or("output");
    let ext = kind.extension();
    let output_path = if let Some(stem) = source_path.strip_suffix(".fe") {
        format!("{stem}.{ext}")
    } else {
        format!("{source_path}.{ext}")
    };

    // Generate codegen content (borrows db immutably)
    let content = {
        let file = backend
            .db
            .workspace()
            .get(&backend.db, &internal_uri)
            .ok_or_else(|| {
                ResponseError::new(
                    ErrorCode::INVALID_PARAMS,
                    format!("file not in workspace: {uri}"),
                )
            })?;

        let top_mod = map_file_to_mod(&backend.db, file);
        match generate_codegen_string(&backend.db, top_mod, &kind) {
            Ok(content) => content,
            Err(e) => {
                let _ = backend.client.clone().show_message(ShowMessageParams {
                    typ: MessageType::WARNING,
                    message: format!(
                        "Can't generate {} - fix errors first:\n{}",
                        kind.label(),
                        if e.len() > 300 { &e[..300] } else { &e }
                    ),
                });
                return Ok(None);
            }
        }
    };

    // Write to VFS (borrows backend mutably)
    let vfs = backend.virtual_files_mut().ok_or_else(|| {
        ResponseError::new(
            ErrorCode::INTERNAL_ERROR,
            "virtual files not available".to_string(),
        )
    })?;

    let tmp_url = vfs
        .write_file("codegen", &output_path, &content, false)
        .map_err(|e| {
            ResponseError::new(
                ErrorCode::INTERNAL_ERROR,
                format!("write codegen output: {e}"),
            )
        })?;

    // Try window/showDocument first (works in VS Code)
    let show_result = backend
        .client
        .show_document(ShowDocumentParams {
            uri: tmp_url.clone(),
            external: None,
            take_focus: Some(true),
            selection: None,
        })
        .await;

    if let Ok(result) = show_result
        && result.success
    {
        return Ok(None);
    }

    // Fallback: workspace/applyEdit to open the file in editors that don't
    // support showDocument (e.g. Zed). CreateFile + TextDocumentEdit forces
    // the editor to open it. Note: Zed opens two tabs for this due to
    // lacking window/showDocument support (tracked: zed-industries/zed#24852).
    let line_count = content.lines().count() as u32;
    let edit = WorkspaceEdit {
        changes: None,
        document_changes: Some(DocumentChanges::Operations(vec![
            DocumentChangeOperation::Op(ResourceOp::Create(CreateFile {
                uri: tmp_url.clone(),
                options: Some(CreateFileOptions {
                    overwrite: Some(true),
                    ignore_if_exists: None,
                }),
                annotation_id: None,
            })),
            DocumentChangeOperation::Edit(TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier {
                    uri: tmp_url,
                    version: None,
                },
                edits: vec![OneOf::Left(TextEdit {
                    range: Range::new(Position::new(0, 0), Position::new(line_count, 0)),
                    new_text: content,
                })],
            }),
        ])),
        change_annotations: None,
    };

    let _ = backend
        .client
        .apply_edit(ApplyWorkspaceEditParams {
            label: Some(format!("View {}", kind.label())),
            edit,
        })
        .await;

    Ok(None)
}

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TraceOpenWorkbenchArgs {
    uri: Option<String>,
    #[serde(default)]
    range: Option<TraceOpenWorkbenchRange>,
    #[serde(default)]
    target: Option<String>,
    #[serde(default)]
    opt_level: Option<String>,
    #[serde(default)]
    view: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TraceOpenWorkbenchRange {
    start: TraceOpenWorkbenchPosition,
    end: TraceOpenWorkbenchPosition,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TraceOpenWorkbenchPosition {
    line: u32,
    character: u32,
}

async fn handle_open_trace_workbench(
    backend: &mut Backend,
    arguments: &[Value],
) -> Result<Option<Value>, ResponseError> {
    let args = parse_trace_open_workbench_args(arguments)?;
    let uri_str = args.uri.ok_or_else(|| {
        ResponseError::new(
            ErrorCode::INVALID_PARAMS,
            "missing URI argument for fe.trace.openWorkbench".to_string(),
        )
    })?;
    let client_uri = Url::parse(&uri_str).map_err(|err| {
        ResponseError::new(ErrorCode::INVALID_PARAMS, format!("invalid URI: {err}"))
    })?;
    validate_trace_workbench_uri(backend, &client_uri)?;
    let base_url = backend.docs_url.clone().ok_or_else(|| {
        ResponseError::new(
            ErrorCode::INTERNAL_ERROR,
            "trace workbench requires the combined local HTTP server".to_string(),
        )
    })?;
    let token = read_trace_auth_token(backend)?;
    let internal_uri = backend.map_client_uri_to_internal(client_uri.clone());
    let selection = args.range.map(|range| TraceViewerSelection {
        start_line: range.start.line,
        start_character: range.start.character,
        end_line: range.end.line,
        end_character: range.end.character,
    });
    let session = backend.create_trace_viewer_session(
        internal_uri,
        args.target.unwrap_or_else(|| "evm".to_string()),
        args.opt_level.unwrap_or_else(|| "O2".to_string()),
        args.view
            .unwrap_or_else(|| "source-postopt-bytecode".to_string()),
        selection,
    );
    let url = format!(
        "{}/trace/workbench?session={}#token={}",
        base_url.trim_end_matches('/'),
        session.id,
        token
    );
    Ok(Some(serde_json::json!({
        "sessionId": session.id,
        "url": url,
        "uri": client_uri,
        "configHash": session.config_hash,
        "capabilities": {
            "events": "sse",
            "modelDeltas": false,
            "chunks": false,
            "selectionSync": true
        }
    })))
}

fn parse_trace_open_workbench_args(
    arguments: &[Value],
) -> Result<TraceOpenWorkbenchArgs, ResponseError> {
    if let Some(first) = arguments.first() {
        if let Some(uri) = first.as_str() {
            return Ok(TraceOpenWorkbenchArgs {
                uri: Some(uri.to_string()),
                ..TraceOpenWorkbenchArgs::default()
            });
        }
        if first.is_object() {
            return serde_json::from_value(first.clone()).map_err(|err| {
                ResponseError::new(
                    ErrorCode::INVALID_PARAMS,
                    format!("invalid fe.trace.openWorkbench arguments: {err}"),
                )
            });
        }
    }
    Ok(TraceOpenWorkbenchArgs::default())
}

fn validate_trace_workbench_uri(backend: &Backend, uri: &Url) -> Result<(), ResponseError> {
    if uri.scheme() != "file" {
        return Err(ResponseError::new(
            ErrorCode::INVALID_PARAMS,
            format!("trace workbench only supports file:// URIs, got {uri}"),
        ));
    }
    let path = uri.to_file_path().map_err(|()| {
        ResponseError::new(
            ErrorCode::INVALID_PARAMS,
            format!("trace workbench URI is not a local file path: {uri}"),
        )
    })?;
    if path.extension().is_none_or(|ext| ext != "fe") {
        return Err(ResponseError::new(
            ErrorCode::INVALID_PARAMS,
            format!("trace workbench URI must point to a .fe file: {uri}"),
        ));
    }
    if let Some(root) = backend.lsp_workspace_root.as_ref()
        && !path.starts_with(root)
    {
        return Err(ResponseError::new(
            ErrorCode::INVALID_PARAMS,
            format!("trace workbench URI is outside the LSP workspace root: {uri}"),
        ));
    }
    Ok(())
}

fn read_trace_auth_token(backend: &Backend) -> Result<String, ResponseError> {
    let root = backend.lsp_workspace_root.as_ref().ok_or_else(|| {
        ResponseError::new(
            ErrorCode::INTERNAL_ERROR,
            "trace workbench requires a workspace root for local auth".to_string(),
        )
    })?;
    let token_path = root.join(".fe-lsp.token");
    let token = std::fs::read_to_string(&token_path).map_err(|err| {
        ResponseError::new(
            ErrorCode::INTERNAL_ERROR,
            format!(
                "failed to read LSP auth token {}: {err}",
                token_path.display()
            ),
        )
    })?;
    let token = token.trim().to_string();
    if token.is_empty() {
        return Err(ResponseError::new(
            ErrorCode::INTERNAL_ERROR,
            "LSP auth token file is empty".to_string(),
        ));
    }
    Ok(token)
}

/// Handle `fe.openDocs` — open the documentation page for an item.
///
/// Arguments: `[path]` where `path` is a doc URL path like `"mylib::Foo/struct"`,
/// or no arguments to open the docs root.
async fn handle_open_docs(
    backend: &mut Backend,
    arguments: &[Value],
) -> Result<Option<Value>, ResponseError> {
    let base = match &backend.docs_url {
        Some(url) => url.clone(),
        None => {
            let _ = backend.client.clone().show_message(ShowMessageParams {
                typ: MessageType::INFO,
                message: "Documentation server is not running. Start with `fe lsp` to enable."
                    .to_string(),
            });
            return Ok(None);
        }
    };

    let doc_path = arguments
        .first()
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    // If a browser doc client is connected, navigate there instead of opening a new window.
    // receiver_count() > 1 because the placeholder receiver in run_stdio_server always exists.
    if backend
        .doc_nav_tx
        .as_ref()
        .is_some_and(|tx| tx.receiver_count() > 1)
    {
        backend.notify_doc_navigate(doc_path);
        return Ok(None);
    }

    let url_str = if doc_path.is_empty() {
        base
    } else {
        format!("{base}#{doc_path}")
    };

    let uri = Url::parse(&url_str).map_err(|e| {
        ResponseError::new(ErrorCode::INTERNAL_ERROR, format!("invalid docs URL: {e}"))
    })?;

    // Try window/showDocument first (works in VS Code)
    let show_result = backend
        .client
        .show_document(ShowDocumentParams {
            uri: uri.clone(),
            external: Some(true),
            take_focus: Some(true),
            selection: None,
        })
        .await;

    if let Ok(result) = show_result
        && result.success
    {
        return Ok(None);
    }

    // Fallback: open in system browser directly (Zed doesn't support showDocument)
    open_in_browser(uri.as_str());

    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::parse_trace_open_workbench_args;

    #[test]
    fn trace_open_workbench_accepts_string_or_object_args() {
        let parsed =
            parse_trace_open_workbench_args(&[serde_json::json!("file:///workspace/demo.fe")])
                .unwrap();
        assert_eq!(parsed.uri.as_deref(), Some("file:///workspace/demo.fe"));

        let parsed = parse_trace_open_workbench_args(&[serde_json::json!({
            "uri": "file:///workspace/demo.fe",
            "target": "evm",
            "optLevel": "O2",
            "view": "source-postopt-bytecode"
        })])
        .unwrap();
        assert_eq!(parsed.uri.as_deref(), Some("file:///workspace/demo.fe"));
        assert_eq!(parsed.target.as_deref(), Some("evm"));
        assert_eq!(parsed.opt_level.as_deref(), Some("O2"));
        assert_eq!(parsed.view.as_deref(), Some("source-postopt-bytecode"));
    }
}

fn open_in_browser(url: &str) {
    #[cfg(target_os = "linux")]
    {
        let _ = std::process::Command::new("xdg-open")
            .arg(url)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn();
    }
    #[cfg(target_os = "macos")]
    {
        let _ = std::process::Command::new("open")
            .arg(url)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn();
    }
    #[cfg(target_os = "windows")]
    {
        // `start` is a cmd.exe builtin, not a standalone executable
        let _ = std::process::Command::new("cmd")
            .args(["/C", "start", "", url])
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn();
    }
}
