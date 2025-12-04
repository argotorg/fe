use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use hir::{core::semantic::reference::Target, lower::map_file_to_mod};
use tracing::info;

use super::{
    goto::Cursor,
    item_info::{get_docstring, get_item_definition_markdown, get_item_path_markdown},
};
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

    // Get the target at cursor - only show hover for scope-based targets
    let scope = top_mod
        .reference_at(db, cursor)
        .and_then(|r| r.target_at(db, cursor))
        .and_then(|target| match target {
            Target::Scope(s) => Some(s),
            Target::Span(_) => None, // Local bindings don't have hover info yet
        });

    let info = scope
        .map(|scope| {
            let item = scope.item();
            let pretty_path = get_item_path_markdown(db, item);
            let definition_source = get_item_definition_markdown(db, item);
            let docs = get_docstring(db, scope);

            [pretty_path, definition_source, docs]
                .iter()
                .filter_map(|info| info.clone().map(|info| format!("{info}\n")))
                .collect::<Vec<String>>()
                .join("\n")
        })
        .unwrap_or_default();

    let result = async_lsp::lsp_types::Hover {
        contents: async_lsp::lsp_types::HoverContents::Markup(
            async_lsp::lsp_types::MarkupContent {
                kind: async_lsp::lsp_types::MarkupKind::Markdown,
                value: info,
            },
        ),
        range: None,
    };
    Ok(Some(result))
}
