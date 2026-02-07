use async_lsp::lsp_types::{ExecuteCommandParams, ShowDocumentParams};
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
            .get(&backend.db, &uri)
            .ok_or_else(|| {
                ResponseError::new(
                    ErrorCode::INVALID_PARAMS,
                    format!("file not in workspace: {uri}"),
                )
            })?;

        let top_mod = map_file_to_mod(&backend.db, file);
        generate_codegen_string(&backend.db, top_mod, &kind).map_err(|e| {
            ResponseError::new(
                ErrorCode::INTERNAL_ERROR,
                format!("codegen failed ({}): {e}", kind.label()),
            )
        })?
    };

    // Write to VFS (borrows backend mutably)
    let vfs = backend.virtual_files_mut().ok_or_else(|| {
        ResponseError::new(
            ErrorCode::INTERNAL_ERROR,
            "virtual files not available".to_string(),
        )
    })?;

    let tmp_url = vfs
        .write_file("codegen", &output_path, &content, true)
        .map_err(|e| {
            ResponseError::new(
                ErrorCode::INTERNAL_ERROR,
                format!("write codegen output: {e}"),
            )
        })?;

    let _ = backend
        .client
        .show_document(ShowDocumentParams {
            uri: tmp_url,
            external: None,
            take_focus: Some(true),
            selection: None,
        })
        .await;

    Ok(None)
}
