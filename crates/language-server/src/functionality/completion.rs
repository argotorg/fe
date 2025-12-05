use async_lsp::lsp_types::{CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse};
use async_lsp::ResponseError;
use common::InputDb;
use hir::{hir_def::{ItemKind, TopLevelMod}, lower::map_file_to_mod};
use tracing::info;

use crate::backend::Backend;

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

    let top_mod = map_file_to_mod(&backend.db, file);
    let mut items = Vec::new();

    // Collect completions from items in scope
    collect_scope_completions(&backend.db, top_mod, &mut items);

    Ok(Some(CompletionResponse::Array(items)))
}

fn collect_scope_completions(
    db: &dyn hir::SpannedHirDb,
    top_mod: TopLevelMod,
    items: &mut Vec<CompletionItem>,
) {
    let scope_items = top_mod.scope_graph(db).items_dfs(db);

    for item in scope_items {
        if let Some(completion) = item_to_completion(db, item) {
            items.push(completion);
        }
    }
}

fn item_to_completion(db: &dyn hir::SpannedHirDb, item: ItemKind) -> Option<CompletionItem> {
    let (kind, label) = match item {
        ItemKind::Func(func) => {
            let name = func.name(db).to_opt()?.data(db).to_string();
            (CompletionItemKind::FUNCTION, name)
        }
        ItemKind::Struct(s) => {
            let name = s.name(db).to_opt()?.data(db).to_string();
            (CompletionItemKind::STRUCT, name)
        }
        ItemKind::Enum(e) => {
            let name = e.name(db).to_opt()?.data(db).to_string();
            (CompletionItemKind::ENUM, name)
        }
        ItemKind::Trait(t) => {
            let name = t.name(db).to_opt()?.data(db).to_string();
            (CompletionItemKind::INTERFACE, name)
        }
        ItemKind::TypeAlias(ta) => {
            let name = ta.name(db).to_opt()?.data(db).to_string();
            (CompletionItemKind::TYPE_PARAMETER, name)
        }
        ItemKind::Const(c) => {
            let name = c.name(db).to_opt()?.data(db).to_string();
            (CompletionItemKind::CONSTANT, name)
        }
        ItemKind::Mod(m) => {
            let name = m.name(db).to_opt()?.data(db).to_string();
            (CompletionItemKind::MODULE, name)
        }
        ItemKind::Contract(c) => {
            let name = c.name(db).to_opt()?.data(db).to_string();
            (CompletionItemKind::CLASS, name)
        }
        _ => return None,
    };

    Some(CompletionItem {
        label,
        label_details: None,
        kind: Some(kind),
        detail: None,
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: None,
        filter_text: None,
        insert_text: None,
        insert_text_format: None,
        insert_text_mode: None,
        text_edit: None,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
    })
}
