use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use fe_semantic_query::SemanticIndex;
use hir::lower::map_file_to_mod;
use tracing::info;

use super::goto::Cursor;
use crate::util::to_offset_from_position;
use driver::DriverDataBase;

pub fn hover_helper(
    db: &DriverDataBase,
    file: File,
    params: async_lsp::lsp_types::HoverParams,
) -> Result<Option<Hover>, Error> {
    info!("handling hover");
    let file_text = file.text(db);

    let cursor: Cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(db, file);

    // Prefer structured hover; fall back to legacy Markdown string
    if let Some(h) = SemanticIndex::hover_info_for_symbol_at_cursor(db, db, top_mod, cursor) {
        let mut parts: Vec<String> = Vec::new();
        if let Some(sig) = h.signature {
            parts.push(format!("```fe\n{}\n```", sig));
        }
        if let Some(doc) = h.documentation { parts.push(doc); }
        let value = if parts.is_empty() { String::new() } else { parts.join("\n\n") };
        let result = async_lsp::lsp_types::Hover {
            contents: async_lsp::lsp_types::HoverContents::Markup(async_lsp::lsp_types::MarkupContent {
                kind: async_lsp::lsp_types::MarkupKind::Markdown,
                value,
            }),
            range: None,
        };
        return Ok(Some(result));
    } else if let Some(h) = SemanticIndex::hover_at_cursor(db, db, top_mod, cursor) {
        let result = async_lsp::lsp_types::Hover {
            contents: async_lsp::lsp_types::HoverContents::Markup(async_lsp::lsp_types::MarkupContent {
                kind: async_lsp::lsp_types::MarkupKind::Markdown,
                value: h.contents,
            }),
            range: None,
        };
        return Ok(Some(result));
    }
    Ok(None)
}
