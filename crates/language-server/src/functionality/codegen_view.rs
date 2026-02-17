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
use serde_json::Value;
use url::Url;

use crate::backend::Backend;

enum CodegenKind {
    Mir,
    Yul,
    SonatinaIr,
}

impl CodegenKind {
    fn extension(&self) -> &'static str {
        match self {
            CodegenKind::Mir => "mir",
            CodegenKind::Yul => "yul",
            CodegenKind::SonatinaIr => "sonatina",
        }
    }

    fn label(&self) -> &'static str {
        match self {
            CodegenKind::Mir => "MIR",
            CodegenKind::Yul => "Yul",
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
            let module =
                mir::lower_module(db, top_mod).map_err(|e| format!("MIR lowering: {e}"))?;
            Ok(mir::fmt::format_module(db, &module))
        }
        CodegenKind::Yul => {
            codegen::emit_module_yul(db, top_mod).map_err(|e| format!("Yul emit: {e}"))
        }
        CodegenKind::SonatinaIr => codegen::emit_module_sonatina_ir(db, top_mod)
            .map_err(|e| format!("Sonatina IR emit: {e}")),
    }
}

pub async fn handle_execute_command(
    backend: &mut Backend,
    params: ExecuteCommandParams,
) -> Result<Option<Value>, ResponseError> {
    let kind = match params.command.as_str() {
        "fe.viewMir" => CodegenKind::Mir,
        "fe.viewYul" => CodegenKind::Yul,
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
