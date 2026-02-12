use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    Position, Range, TextEdit, WorkspaceEdit,
};
use common::InputDb;
use driver::DriverDataBase;
use hir::{
    hir_def::{ItemKind, TopLevelMod},
    lower::map_file_to_mod,
    span::LazySpan,
};
use std::collections::HashMap;

use crate::{backend::Backend, util::to_offset_from_position};

pub async fn handle_code_action(
    backend: &Backend,
    params: CodeActionParams,
) -> Result<Option<CodeActionResponse>, ResponseError> {
    let lsp_uri = params.text_document.uri.clone();
    if backend.is_builtin_tmp_uri(&lsp_uri) || backend.is_git_cache_uri(&lsp_uri) {
        return Ok(None);
    }

    let url = backend.map_client_uri_to_internal(lsp_uri.clone());

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
    let top_mod = map_file_to_mod(&backend.db, file);

    let mut actions = Vec::new();

    // Get cursor range
    let start = to_offset_from_position(params.range.start, file_text);
    let end = to_offset_from_position(params.range.end, file_text);

    // Collect code actions for functions without return type annotations
    collect_return_type_actions(
        &backend.db,
        top_mod,
        start,
        end,
        file_text,
        &lsp_uri,
        &mut actions,
    );

    if actions.is_empty() {
        Ok(None)
    } else {
        // Convert CodeAction to CodeActionOrCommand
        let response: Vec<CodeActionOrCommand> = actions
            .into_iter()
            .map(CodeActionOrCommand::CodeAction)
            .collect();
        Ok(Some(response))
    }
}

/// Collect code actions for adding return type annotations to functions.
fn collect_return_type_actions<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    start: parser::TextSize,
    end: parser::TextSize,
    file_text: &str,
    uri: &url::Url,
    actions: &mut Vec<CodeAction>,
) {
    // Find functions at the cursor position
    for item in top_mod.scope_graph(db).items_dfs(db) {
        let ItemKind::Func(func) = item else {
            continue;
        };

        // Check if function has a body (not just a declaration)
        if func.body(db).is_none() {
            continue;
        }

        // Check if the function already has an explicit return type
        if func.has_explicit_return_ty(db) {
            continue;
        }

        // Check if cursor is within the function
        let Some(func_span) = func.span().resolve(db) else {
            continue;
        };

        if !func_span.range.contains(start) && !func_span.range.contains(end) {
            continue;
        }

        // Get the inferred return type
        let ret_ty = func.return_ty(db);
        let ret_ty_str = ret_ty.pretty_print(db);

        // Skip if return type is unit (no annotation needed) or invalid
        if ret_ty_str == "()" || ret_ty_str.is_empty() || ret_ty.has_invalid(db) {
            continue;
        }

        // Find the position to insert the return type annotation
        // This should be after the closing paren of the params and before the opening brace
        let Some(insert_pos) = find_return_type_insert_position(db, func, file_text) else {
            continue;
        };

        // Create the text edit
        let edit = TextEdit {
            range: Range {
                start: insert_pos,
                end: insert_pos,
            },
            new_text: format!(" -> {}", ret_ty_str),
        };

        let mut changes = HashMap::new();
        changes.insert(uri.clone(), vec![edit]);

        actions.push(CodeAction {
            title: format!("Add return type: -> {}", ret_ty_str),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: None,
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }),
            command: None,
            is_preferred: Some(true),
            disabled: None,
            data: None,
        });
    }
}

/// Find the position to insert a return type annotation (after params, before body).
fn find_return_type_insert_position<'db>(
    db: &'db DriverDataBase,
    func: hir::hir_def::Func<'db>,
    file_text: &str,
) -> Option<Position> {
    // Get the function's params span
    let params_span = func.span().params().resolve(db)?;
    let params_end = params_span.range.end();

    // Convert to line/column position
    let mut line = 0u32;
    let mut col = 0u32;
    let params_end_offset = usize::from(params_end);

    for (i, c) in file_text.char_indices() {
        if i >= params_end_offset {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    Some(Position {
        line,
        character: col,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::load_ingot_from_directory;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use std::path::PathBuf;

    #[test]
    fn test_return_type_code_action() {
        let mut db = DriverDataBase::default();

        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_files")
            .join("hoverable");

        load_ingot_from_directory(&mut db, &fixture_path);

        let lib_url = url::Url::from_file_path(fixture_path.join("src").join("lib.fe")).unwrap();
        let file = db.workspace().get(&db, &lib_url).expect("file not found");
        let file_text = file.text(&db);
        let top_mod = map_file_to_mod(&db, file);

        // Test collecting return type actions for functions without annotations
        let mut actions = Vec::new();
        let start = parser::TextSize::from(0);
        let end = parser::TextSize::from(file_text.len() as u32);

        collect_return_type_actions(&db, top_mod, start, end, file_text, &lib_url, &mut actions);

        // Verify we get code actions for functions without return types
        // The hoverable fixture has functions that should trigger this
        for action in &actions {
            assert!(action.title.starts_with("Add return type:"));
            assert!(action.edit.is_some());
        }
    }
}
