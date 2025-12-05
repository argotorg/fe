use async_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
};
use async_lsp::ResponseError;
use common::InputDb;
use driver::DriverDataBase;
use hir::{
    analysis::name_resolution::{NameDomain, NameResKind},
    hir_def::{scope_graph::ScopeId, ItemKind, TopLevelMod},
    lower::map_file_to_mod,
};
use tracing::info;

use crate::{backend::Backend, util::to_offset_from_position};

pub async fn handle_completion(
    backend: &Backend,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>, ResponseError> {
    info!("handling completion request");

    let file_path_str = params.text_document_position.text_document.uri.path();
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

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(params.text_document_position.position, &file_text);
    let top_mod = map_file_to_mod(&backend.db, file);

    let mut items = Vec::new();

    // Check if this is a member access completion (triggered by '.')
    let is_member_access = params
        .context
        .as_ref()
        .and_then(|ctx| ctx.trigger_character.as_ref())
        .map(|c| c == ".")
        .unwrap_or(false);

    if is_member_access {
        // TODO: Implement member access completion
        // Need to resolve type of expression before '.' and show its fields/methods
        // This requires understanding the proper public API for accessing type members
        info!("Member access completion not yet implemented");
    } else {
        // Regular completion: show items visible in scope
        let scope = find_scope_at_cursor(&backend.db, top_mod, cursor);
        if let Some(scope) = scope {
            collect_items_from_scope(&backend.db, scope, &mut items);
        }
    }

    Ok(Some(CompletionResponse::Array(items)))
}

/// Find the most specific scope containing the cursor position.
fn find_scope_at_cursor<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: parser::TextSize,
) -> Option<ScopeId<'db>> {
    use hir::span::LazySpan;

    // Find the smallest enclosing item
    let items = top_mod.scope_graph(db).items_dfs(db);
    let mut best_scope = None;
    let mut best_size = None;

    for item in items {
        let span = match item.span().resolve(db) {
            Some(s) => s,
            None => continue,
        };

        if span.range.contains(cursor) {
            let size = span.range.len();

            match best_size {
                None => {
                    best_scope = Some(ScopeId::from_item(item));
                    best_size = Some(size);
                }
                Some(current_best) if size < current_best => {
                    best_scope = Some(ScopeId::from_item(item));
                    best_size = Some(size);
                }
                _ => {}
            }
        }
    }

    best_scope.or(Some(top_mod.scope()))
}

/// Collect completion items from a scope.
fn collect_items_from_scope<'db>(
    db: &'db DriverDataBase,
    scope: ScopeId<'db>,
    items: &mut Vec<CompletionItem>,
) {
    // Get all visible items (both values and types)
    let visible_items = scope.items_in_scope(db, NameDomain::VALUE | NameDomain::TYPE);

    for (name, name_res) in visible_items {
        if let Some(completion) = name_res_to_completion(db, name, name_res) {
            items.push(completion);
        }
    }
}

/// Convert a NameRes to a CompletionItem.
fn name_res_to_completion<'db>(
    db: &'db DriverDataBase,
    name: &str,
    name_res: &hir::analysis::name_resolution::NameRes<'db>,
) -> Option<CompletionItem> {
    let scope = match &name_res.kind {
        NameResKind::Scope(scope) => *scope,
        NameResKind::Prim(_) => {
            // Primitive types
            return Some(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            });
        }
        _ => return None,
    };

    // Determine the completion kind based on the scope
    let kind = match scope.to_item() {
        Some(ItemKind::Func(_)) => CompletionItemKind::FUNCTION,
        Some(ItemKind::Struct(_)) => CompletionItemKind::STRUCT,
        Some(ItemKind::Enum(_)) => CompletionItemKind::ENUM,
        Some(ItemKind::Trait(_)) => CompletionItemKind::INTERFACE,
        Some(ItemKind::TypeAlias(_)) => CompletionItemKind::CLASS,
        Some(ItemKind::Const(_)) => CompletionItemKind::CONSTANT,
        Some(ItemKind::Mod(_) | ItemKind::TopMod(_)) => CompletionItemKind::MODULE,
        Some(ItemKind::Contract(_)) => CompletionItemKind::CLASS,
        _ => match scope {
            ScopeId::FuncParam(_, _) => CompletionItemKind::VARIABLE,
            ScopeId::GenericParam(_, _) => CompletionItemKind::TYPE_PARAMETER,
            ScopeId::Variant(_) => CompletionItemKind::ENUM_MEMBER,
            ScopeId::Field(_, _) => CompletionItemKind::FIELD,
            _ => CompletionItemKind::VALUE,
        },
    };

    Some(CompletionItem {
        label: name.to_string(),
        kind: Some(kind),
        insert_text: Some(name.to_string()),
        insert_text_format: Some(async_lsp::lsp_types::InsertTextFormat::PLAIN_TEXT),
        insert_text_mode: Some(async_lsp::lsp_types::InsertTextMode::ADJUST_INDENTATION),
        ..Default::default()
    })
}
