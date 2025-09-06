use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use fe_semantic_query::SemanticQuery;
use hir::lower::map_file_to_mod;
use hir::span::LazySpan;
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

    // Use unified SemanticQuery API
    let query = SemanticQuery::at_cursor(db, top_mod, cursor);
    if let Some(h) = query.hover_info() {
        let mut parts: Vec<String> = Vec::new();
        if let Some(sig) = h.signature {
            parts.push(format!("```fe\n{}\n```", sig));
        }
        if let Some(doc) = h.documentation {
            parts.push(doc);
        }
        let value = if parts.is_empty() {
            String::new()
        } else {
            parts.join("\n\n")
        };
        let range = h
            .span
            .resolve(db)
            .and_then(|sp| crate::util::to_lsp_range_from_span(sp, db).ok());
        let result = async_lsp::lsp_types::Hover {
            contents: async_lsp::lsp_types::HoverContents::Markup(
                async_lsp::lsp_types::MarkupContent {
                    kind: async_lsp::lsp_types::MarkupKind::Markdown,
                    value,
                },
            ),
            range,
        };
        return Ok(Some(result));
    }
    Ok(None)
}
