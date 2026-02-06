use async_lsp::ResponseError;
use async_lsp::lsp_types::{SelectionRange, SelectionRangeParams};
use common::InputDb;
use hir::{
    hir_def::ItemKind,
    lower::map_file_to_mod,
    span::LazySpan,
};

use crate::{
    backend::Backend,
    util::{to_lsp_range_from_span, to_offset_from_position},
};

/// Handle textDocument/selectionRange.
///
/// Builds nested selection ranges from the AST hierarchy at each requested position.
/// The ranges go from innermost (identifier/expression) to outermost (module).
pub async fn handle_selection_range(
    backend: &Backend,
    params: SelectionRangeParams,
) -> Result<Option<Vec<SelectionRange>>, ResponseError> {
    let path_str = params.text_document.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let top_mod = map_file_to_mod(&backend.db, file);
    let scope_graph = top_mod.scope_graph(&backend.db);

    let mut results = Vec::new();

    for position in &params.positions {
        let cursor = to_offset_from_position(*position, file_text.as_str());

        // Collect all items and their spans, sorted by span size (smallest first)
        let mut containing_spans = Vec::new();

        for item in scope_graph.items_dfs(&backend.db) {
            // Get the full item span
            let item_span: hir::span::DynLazySpan = item.span().into();
            if let Some(span) = item_span.resolve(&backend.db) {
                let start: usize = span.range.start().into();
                let end: usize = span.range.end().into();
                if start <= cursor.into() && usize::from(cursor) <= end {
                    if let Ok(range) = to_lsp_range_from_span(span.clone(), &backend.db) {
                        containing_spans.push((end - start, range));
                    }
                }
            }

            // Also add the name span if available (narrower selection)
            if let Some(name_span) = item.name_span() {
                if let Some(span) = name_span.resolve(&backend.db) {
                    let start: usize = span.range.start().into();
                    let end: usize = span.range.end().into();
                    if start <= cursor.into() && usize::from(cursor) <= end {
                        if let Ok(range) = to_lsp_range_from_span(span, &backend.db) {
                            containing_spans.push((end - start, range));
                        }
                    }
                }
            }

            // For functions, also add parameter list and body spans
            if let ItemKind::Func(func) = item {
                if let Some(body) = func.body(&backend.db) {
                    let body_span: hir::span::DynLazySpan = body.span().into();
                    if let Some(span) = body_span.resolve(&backend.db) {
                        let start: usize = span.range.start().into();
                        let end: usize = span.range.end().into();
                        if start <= cursor.into() && usize::from(cursor) <= end {
                            if let Ok(range) = to_lsp_range_from_span(span, &backend.db) {
                                containing_spans.push((end - start, range));
                            }
                        }
                    }
                }
            }
        }

        // Sort by span size: smallest first (most specific)
        containing_spans.sort_by_key(|(size, _)| *size);
        containing_spans.dedup_by_key(|(_, range)| *range);

        // Build nested SelectionRange from outermost to innermost
        let selection_range = containing_spans
            .into_iter()
            .rev()
            .fold(None, |parent, (_, range)| {
                Some(SelectionRange {
                    range,
                    parent: parent.map(Box::new),
                })
            });

        // If we found no containing items, create a trivial selection at the position
        results.push(selection_range.unwrap_or(SelectionRange {
            range: async_lsp::lsp_types::Range {
                start: *position,
                end: *position,
            },
            parent: None,
        }));
    }

    Ok(Some(results))
}
