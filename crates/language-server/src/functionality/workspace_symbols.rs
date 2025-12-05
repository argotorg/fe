use async_lsp::lsp_types::{
    Location, SymbolInformation, SymbolKind, WorkspaceSymbolParams, WorkspaceSymbolResponse,
};
use async_lsp::ResponseError;
use common::InputDb;
use hir::{hir_def::ItemKind, lower::map_file_to_mod, span::LazySpan};
use tracing::info;

use crate::{backend::Backend, util::to_lsp_range_from_span};

pub async fn handle_workspace_symbols(
    backend: &Backend,
    params: WorkspaceSymbolParams,
) -> Result<Option<WorkspaceSymbolResponse>, ResponseError> {
    info!("handling workspace symbols request: query={}", params.query);

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
    let (kind, name, prefix) = match item {
        ItemKind::Func(func) => (
            SymbolKind::FUNCTION,
            func.name(db).to_opt()?.data(db).to_string(),
            "fn ",
        ),
        ItemKind::Struct(s) => (
            SymbolKind::STRUCT,
            s.name(db).to_opt()?.data(db).to_string(),
            "struct ",
        ),
        ItemKind::Enum(e) => (
            SymbolKind::ENUM,
            e.name(db).to_opt()?.data(db).to_string(),
            "enum ",
        ),
        ItemKind::Trait(t) => (
            SymbolKind::INTERFACE,
            t.name(db).to_opt()?.data(db).to_string(),
            "trait ",
        ),
        ItemKind::TypeAlias(ta) => (
            SymbolKind::CLASS,
            ta.name(db).to_opt()?.data(db).to_string(),
            "type ",
        ),
        ItemKind::Const(c) => (
            SymbolKind::CONSTANT,
            c.name(db).to_opt()?.data(db).to_string(),
            "const ",
        ),
        ItemKind::Mod(m) => (
            SymbolKind::MODULE,
            m.name(db).to_opt()?.data(db).to_string(),
            "mod ",
        ),
        ItemKind::Contract(c) => (
            SymbolKind::CLASS,
            c.name(db).to_opt()?.data(db).to_string(),
            "contract ",
        ),
        _ => return None,
    };

    let label = format!("{}{}", prefix, name);

    // Filter by query
    if !query.is_empty() && !name.to_lowercase().contains(query) {
        return None;
    }

    let span = item.name_span()?.resolve(db)?;
    let uri = span.file.url(db)?;
    let range = to_lsp_range_from_span(span, db).ok()?;

    #[allow(deprecated)]
    Some(SymbolInformation {
        name: label,
        kind,
        tags: None,
        deprecated: None,
        location: Location {
            uri: uri.to_string().parse().ok()?,
            range,
        },
        container_name: None,
    })
}
