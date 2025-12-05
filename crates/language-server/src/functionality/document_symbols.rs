use async_lsp::lsp_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind};
use async_lsp::ResponseError;
use common::InputDb;
use hir::{
    hir_def::{ItemKind, TopLevelMod},
    lower::map_file_to_mod,
    span::LazySpan,
};
use tracing::info;

use crate::{backend::Backend, util::to_lsp_range_from_span};

pub async fn handle_document_symbols(
    backend: &Backend,
    params: async_lsp::lsp_types::DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>, ResponseError> {
    info!("handling document symbols request");

    let file_path_str = params.text_document.uri.path();
    let url = url::Url::from_file_path(file_path_str).map_err(|()| {
        ResponseError::new(
            async_lsp::ErrorCode::INTERNAL_ERROR,
            format!("Invalid file path: {file_path_str}"),
        )
    })?;

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
    let (kind, label) = match item {
        ItemKind::Func(func) => {
            let name = func.name(db).to_opt()?.data(db).to_string();
            (SymbolKind::FUNCTION, format!("fn {}", name))
        }
        ItemKind::Struct(s) => {
            let name = s.name(db).to_opt()?.data(db).to_string();
            (SymbolKind::STRUCT, format!("struct {}", name))
        }
        ItemKind::Enum(e) => {
            let name = e.name(db).to_opt()?.data(db).to_string();
            (SymbolKind::ENUM, format!("enum {}", name))
        }
        ItemKind::Trait(t) => {
            let name = t.name(db).to_opt()?.data(db).to_string();
            (SymbolKind::INTERFACE, format!("trait {}", name))
        }
        ItemKind::TypeAlias(ta) => {
            let name = ta.name(db).to_opt()?.data(db).to_string();
            (SymbolKind::CLASS, format!("type {}", name))
        }
        ItemKind::Const(c) => {
            let name = c.name(db).to_opt()?.data(db).to_string();
            (SymbolKind::CONSTANT, format!("const {}", name))
        }
        ItemKind::Mod(m) => {
            let name = m.name(db).to_opt()?.data(db).to_string();
            (SymbolKind::MODULE, format!("mod {}", name))
        }
        ItemKind::Impl(_) => (SymbolKind::CLASS, "impl".to_string()),
        ItemKind::ImplTrait(_) => (SymbolKind::CLASS, "impl trait".to_string()),
        ItemKind::Contract(c) => {
            let name = c.name(db).to_opt()?.data(db).to_string();
            (SymbolKind::CLASS, format!("contract {}", name))
        }
        _ => return None,
    };

    let span = item.name_span()?.resolve(db)?;
    let range = to_lsp_range_from_span(span, db).ok()?;

    #[allow(deprecated)]
    Some(DocumentSymbol {
        name: label,
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: None,
    })
}
