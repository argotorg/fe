use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use hir::{
    core::semantic::reference::{ReferenceView, Target},
    lower::map_file_to_mod,
    span::LazySpan,
};
use tracing::info;

use super::{
    goto::Cursor,
    item_info::{get_docstring, get_item_definition_markdown, get_item_path_markdown},
};
use crate::util::{to_lsp_range_from_span, to_offset_from_position};
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

    // Get the reference at cursor and resolve it
    let Some(r) = top_mod.reference_at(db, cursor) else {
        return Ok(None);
    };

    let resolution = r.target_at(db, cursor);

    // Compute the hover range from the reference span at the cursor position.
    // For paths, use the specific segment span containing the cursor.
    let hover_range = match &r {
        ReferenceView::Path(pv) => {
            let mut seg_range = None;
            for idx in 0..=pv.path.segment_index(db) {
                if let Some(resolved) = pv.span.clone().segment(idx).resolve(db) {
                    if resolved.range.contains(cursor) {
                        seg_range = to_lsp_range_from_span(resolved, db).ok();
                        break;
                    }
                }
            }
            seg_range
        }
        _ => r
            .span()
            .resolve(db)
            .and_then(|s| to_lsp_range_from_span(s, db).ok()),
    };

    // Build hover content
    let info = if resolution.is_ambiguous() {
        let mut sections = vec!["**Multiple definitions**\n\n".to_string()];

        for (i, target) in resolution.as_slice().iter().enumerate() {
            match target {
                Target::Scope(scope) => {
                    let item = scope.item();
                    let path = get_item_path_markdown(db, item);
                    let def = get_item_definition_markdown(db, item);
                    let docs = get_docstring(db, *scope);

                    for info in [path, def, docs].iter().filter_map(|x| x.as_ref()) {
                        sections.push(format!("{}\n\n", info));
                    }
                }
                Target::Local { ty, .. } => {
                    let name = match &r {
                        ReferenceView::Path(pv) => {
                            let Some(ident) = pv.path.ident(db).to_opt() else {
                                continue;
                            };
                            ident.data(db).to_string()
                        }
                        _ => continue,
                    };
                    let ty_str = ty.pretty_print(db);
                    sections.push(format!("```fe\nlet {name}: {ty_str}\n```\n\n"));
                }
            }

            if i < resolution.as_slice().len() - 1 {
                sections.push("---\n\n".to_string());
            }
        }

        sections.join("")
    } else {
        let Some(target) = resolution.first() else {
            return Ok(None);
        };
        match target {
            Target::Scope(scope) => {
                let item = scope.item();
                let pretty_path = get_item_path_markdown(db, item);
                let definition_source = get_item_definition_markdown(db, item);
                let docs = get_docstring(db, *scope);

                [pretty_path, definition_source, docs]
                    .iter()
                    .filter_map(|info| info.clone().map(|info| format!("{info}\n")))
                    .collect::<Vec<String>>()
                    .join("\n")
            }
            Target::Local { ty, .. } => {
                let name = match &r {
                    ReferenceView::Path(pv) => {
                        let Some(ident) = pv.path.ident(db).to_opt() else {
                            return Ok(None);
                        };
                        ident.data(db).to_string()
                    }
                    _ => return Ok(None),
                };
                let ty_str = ty.pretty_print(db);
                format!("```fe\nlet {name}: {ty_str}\n```")
            }
        }
    };

    let result = async_lsp::lsp_types::Hover {
        contents: async_lsp::lsp_types::HoverContents::Markup(
            async_lsp::lsp_types::MarkupContent {
                kind: async_lsp::lsp_types::MarkupKind::Markdown,
                value: info,
            },
        ),
        range: hover_range,
    };
    Ok(Some(result))
}
