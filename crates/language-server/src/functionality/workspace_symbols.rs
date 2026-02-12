use crate::{backend::Backend, util::to_lsp_range_from_span};
use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    Location, SymbolInformation, SymbolKind, WorkspaceSymbolParams, WorkspaceSymbolResponse,
};
use common::InputDb;
use hir::{hir_def::ItemKind, lower::map_file_to_mod, span::LazySpan};

pub async fn handle_workspace_symbols(
    backend: &Backend,
    params: WorkspaceSymbolParams,
) -> Result<Option<WorkspaceSymbolResponse>, ResponseError> {
    let mut symbols = Vec::new();
    let query = params.query.to_lowercase();

    // Iterate through all files in the workspace
    let workspace = backend.db.workspace();
    for (url, file) in workspace.all_files(&backend.db).iter() {
        // Only process .fe files
        if !url.path().ends_with(".fe") {
            continue;
        }

        let top_mod = map_file_to_mod(&backend.db, file);
        collect_matching_symbols(&backend.db, top_mod, &query, &mut symbols);
    }

    for symbol in symbols.iter_mut() {
        symbol.location.uri = backend.map_internal_uri_to_client(symbol.location.uri.clone());
    }

    Ok(Some(WorkspaceSymbolResponse::Flat(symbols)))
}

fn collect_matching_symbols(
    db: &dyn hir::SpannedHirDb,
    top_mod: hir::hir_def::TopLevelMod,
    query: &str,
    symbols: &mut Vec<SymbolInformation>,
) {
    let items = top_mod.scope_graph(db).items_dfs(db);

    for item in items {
        if let Some(symbol) = item_to_workspace_symbol(db, item, query) {
            symbols.push(symbol);
        }
    }
}

fn item_to_workspace_symbol(
    db: &dyn hir::SpannedHirDb,
    item: ItemKind,
    query: &str,
) -> Option<SymbolInformation> {
    let (kind, name) = match item {
        ItemKind::Func(func) => (
            SymbolKind::FUNCTION,
            format!("fn {}", func.name(db).to_opt()?.data(db)),
        ),
        ItemKind::Struct(s) => (
            SymbolKind::STRUCT,
            format!("struct {}", s.name(db).to_opt()?.data(db)),
        ),
        ItemKind::Enum(e) => (
            SymbolKind::ENUM,
            format!("enum {}", e.name(db).to_opt()?.data(db)),
        ),
        ItemKind::Trait(t) => (
            SymbolKind::INTERFACE,
            format!("trait {}", t.name(db).to_opt()?.data(db)),
        ),
        ItemKind::TypeAlias(ta) => (
            SymbolKind::CLASS,
            format!("type {}", ta.name(db).to_opt()?.data(db)),
        ),
        ItemKind::Const(c) => (
            SymbolKind::CONSTANT,
            format!("const {}", c.name(db).to_opt()?.data(db)),
        ),
        ItemKind::Mod(m) => (
            SymbolKind::MODULE,
            format!("mod {}", m.name(db).to_opt()?.data(db)),
        ),
        ItemKind::Contract(c) => (
            SymbolKind::CLASS,
            format!("contract {}", c.name(db).to_opt()?.data(db)),
        ),
        _ => return None,
    };

    // Filter by query
    if !query.is_empty() && !name.to_lowercase().contains(query) {
        return None;
    }

    let span = item.name_span()?.resolve(db)?;
    let uri = span.file.url(db)?;
    let range = to_lsp_range_from_span(span, db).ok()?;

    #[allow(deprecated)]
    Some(SymbolInformation {
        name,
        kind,
        tags: None,
        deprecated: None,
        location: Location { uri, range },
        container_name: None,
    })
}
