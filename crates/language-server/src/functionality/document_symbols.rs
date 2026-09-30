use crate::{
    backend::Backend,
    util::{named_item_symbol, to_lsp_range_from_span},
};
use async_lsp::ResponseError;
use async_lsp::lsp_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind};
use common::InputDb;
use hir::{
    hir_def::{ItemKind, TopLevelMod},
    lower::map_file_to_mod,
    span::LazySpan,
};

pub async fn handle_document_symbols(
    backend: &Backend,
    params: async_lsp::lsp_types::DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>, ResponseError> {
    let url = backend.map_client_uri_to_internal(params.text_document.uri);

    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| {
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found: {url}"),
            )
        })?;

    let top_mod = map_file_to_mod(&backend.db, file);
    let mut symbols = Vec::new();

    collect_symbols(&backend.db, top_mod, &mut symbols);

    Ok(Some(DocumentSymbolResponse::Nested(symbols)))
}

fn collect_symbols(
    db: &dyn hir::SpannedHirDb,
    top_mod: TopLevelMod,
    symbols: &mut Vec<DocumentSymbol>,
) {
    let items = top_mod.scope_graph(db).items_dfs(db);

    for item in items {
        if let Some(symbol) = item_to_symbol(db, item) {
            symbols.push(symbol);
        }
    }
}

fn item_to_symbol(db: &dyn hir::SpannedHirDb, item: ItemKind) -> Option<DocumentSymbol> {
    let (kind, name) = match item {
        ItemKind::Impl(_) => (SymbolKind::CLASS, "impl".to_string()),
        ItemKind::ImplTrait(_) => (SymbolKind::CLASS, "impl trait".to_string()),
        _ => named_item_symbol(db, item)?,
    };

    let span = item.name_span()?.resolve(db)?;
    let range = to_lsp_range_from_span(span, db).ok()?;

    #[allow(deprecated)]
    Some(DocumentSymbol {
        name,
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: None,
    })
}
