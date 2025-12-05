use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use hir::{
    core::semantic::reference::{ReferenceView, Target},
    lower::map_file_to_mod,
};
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

    // Get the reference at cursor and resolve it
    let info = top_mod
        .reference_at(db, cursor)
        .and_then(|r| {
            let resolution = r.target_at(db, cursor);

            // Check if ambiguous
            if resolution.is_ambiguous() {
                // Show all candidates
                let mut sections = vec!["**Ambiguous reference - multiple candidates:**\n".to_string()];

                for (i, target) in resolution.as_slice().iter().enumerate() {
                    match target {
                        Target::Scope(scope) => {
                            let item = scope.item();
                            let pretty_path = get_item_path_markdown(db, item);
                            let definition_source = get_item_definition_markdown(db, item);

                            sections.push(format!("\n### Candidate {}\n", i + 1));
                            if let Some(path) = pretty_path {
                                sections.push(format!("{}\n", path));
                            }
                            if let Some(def) = definition_source {
                                sections.push(format!("{}\n", def));
                            }
                        }
                        Target::Local { ty, .. } => {
                            let name = match r {
                                ReferenceView::Path(pv) => pv.path.ident(db).to_opt()?.data(db).to_string(),
                                _ => continue,
                            };
                            let ty_str = ty.pretty_print(db);
                            sections.push(format!("\n### Candidate {}\n", i + 1));
                            sections.push(format!("```fe\nlet {name}: {ty_str}\n```\n"));
                        }
                    }
                }

                Some(sections.join(""))
            } else {
                // Single target - show as before
                let target = resolution.first()?;
                match target {
                    Target::Scope(scope) => {
                        // Scope-based target: show item info
                        let item = scope.item();
                        let pretty_path = get_item_path_markdown(db, item);
                        let definition_source = get_item_definition_markdown(db, item);
                        let docs = get_docstring(db, *scope);

                        Some(
                            [pretty_path, definition_source, docs]
                                .iter()
                                .filter_map(|info| info.clone().map(|info| format!("{info}\n")))
                                .collect::<Vec<String>>()
                                .join("\n"),
                        )
                    }
                    Target::Local { ty, .. } => {
                        // Local binding: get name from the reference, type from target
                        let name = match r {
                            ReferenceView::Path(pv) => pv.path.ident(db).to_opt()?.data(db).to_string(),
                            _ => return None,
                        };
                        let ty_str = ty.pretty_print(db);
                        Some(format!("```fe\nlet {name}: {ty_str}\n```"))
                    }
                }
            }
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

