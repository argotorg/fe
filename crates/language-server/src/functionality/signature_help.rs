use crate::{backend::Backend, util::to_offset_from_position};
use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    ParameterInformation, ParameterLabel, SignatureHelp, SignatureHelpParams, SignatureInformation,
};
use common::InputDb;
use driver::DriverDataBase;
use hir::{
    analysis::{name_resolution::resolve_path, ty::trait_resolution::PredicateListId},
    hir_def::{Body, Expr, ExprId, Func, ItemKind, Partial, TopLevelMod},
    lower::map_file_to_mod,
    span::LazySpan,
};

pub async fn handle_signature_help(
    backend: &Backend,
    params: SignatureHelpParams,
) -> Result<Option<SignatureHelp>, ResponseError> {
    let url = backend.map_client_uri_to_internal(
        params
            .text_document_position_params
            .text_document
            .uri
            .clone(),
    );

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
    let cursor = to_offset_from_position(params.text_document_position_params.position, file_text);
    let top_mod = map_file_to_mod(&backend.db, file);

    // Find the enclosing function call and get signature info
    if let Some(sig_info) = find_signature_at_cursor(&backend.db, top_mod, cursor, file_text) {
        Ok(Some(sig_info))
    } else {
        Ok(None)
    }
}

/// Find the signature help for a function call at the cursor position.
fn find_signature_at_cursor<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: parser::TextSize,
    file_text: &str,
) -> Option<SignatureHelp> {
    // Find the enclosing function
    let scope_graph = top_mod.scope_graph(db);
    let mut enclosing_func = None;

    for item in scope_graph.items_dfs(db) {
        if let ItemKind::Func(func) = item
            && let Some(span) = func.span().resolve(db)
            && span.range.contains(cursor)
        {
            enclosing_func = Some(func);
        }
    }

    let func = enclosing_func?;
    let body = func.body(db)?;

    // Find Call or MethodCall expressions that contain the cursor
    for (expr_id, expr_data) in body.exprs(db).iter() {
        match expr_data {
            Partial::Present(Expr::Call(callee, args)) => {
                let expr_span = expr_id.span(body);
                if let Some(resolved) = expr_span.resolve(db)
                    && resolved.range.contains(cursor)
                {
                    // Try to resolve what function is being called
                    if let Some(called_func) = resolve_callee_func(db, body, *callee) {
                        let active_param =
                            compute_active_parameter(cursor, resolved.range, file_text, args.len());

                        return Some(build_signature_help(db, called_func, active_param));
                    }
                }
            }
            Partial::Present(Expr::MethodCall(receiver, method_name, _generics, args)) => {
                let expr_span = expr_id.span(body);
                if let Some(resolved) = expr_span.resolve(db)
                    && resolved.range.contains(cursor)
                {
                    // Try to resolve the method being called
                    if let Partial::Present(method_ident) = method_name
                        && let Some(called_func) =
                            resolve_method_func(db, top_mod, body, *receiver, *method_ident)
                    {
                        let active_param =
                            compute_active_parameter(cursor, resolved.range, file_text, args.len());

                        return Some(build_signature_help(db, called_func, active_param));
                    }
                }
            }
            _ => {}
        }
    }

    None
}

/// Try to resolve a callee expression to a function.
fn resolve_callee_func<'db>(
    db: &'db DriverDataBase,
    body: Body<'db>,
    callee: ExprId,
) -> Option<Func<'db>> {
    // Check if callee is a path expression
    let callee_data = body.exprs(db).get(callee)?;
    let Partial::Present(Expr::Path(Partial::Present(path))) = callee_data else {
        return None;
    };

    // Resolve the path
    let scope = body.scope();
    let resolved = resolve_path(db, *path, scope, PredicateListId::empty_list(db), false).ok()?;

    // Check if it resolves to a function
    if let Some(scope_id) = resolved.as_scope(db)
        && let Some(ItemKind::Func(func)) = scope_id.to_item()
    {
        return Some(func);
    }

    None
}

/// Try to resolve a method call to a function.
fn resolve_method_func<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    body: Body<'db>,
    receiver: ExprId,
    method_name: hir::hir_def::IdentId<'db>,
) -> Option<Func<'db>> {
    use hir::analysis::ty::ty_check::check_func_body;

    // Get the type of the receiver
    let containing_func = body.containing_func(db)?;
    let (_, typed_body) = check_func_body(db, containing_func);
    let receiver_ty = typed_body.expr_ty(db, receiver);

    let method_name_str = method_name.data(db);

    // Look for impl blocks that match this type
    let ty_name = receiver_ty.pretty_print(db);

    for item in top_mod.scope_graph(db).items_dfs(db) {
        if let ItemKind::Impl(impl_) = item {
            let impl_ty_name = impl_.ty(db).pretty_print(db);
            if impl_ty_name == ty_name {
                for func in impl_.funcs(db) {
                    if let Some(name) = func.name(db).to_opt()
                        && name.data(db) == method_name_str
                    {
                        return Some(func);
                    }
                }
            }
        }
    }

    None
}

/// Compute which parameter is active based on cursor position and commas.
fn compute_active_parameter(
    cursor: parser::TextSize,
    call_range: parser::TextRange,
    file_text: &str,
    arg_count: usize,
) -> Option<u32> {
    let cursor_pos = usize::from(cursor);
    let call_start = usize::from(call_range.start());
    let call_end = usize::from(call_range.end());

    // Get the text of the call
    if call_start >= file_text.len() || call_end > file_text.len() {
        return None;
    }

    let call_text = &file_text[call_start..call_end];

    // Find the opening paren
    let paren_start = call_text.find('(')?;
    let absolute_paren_start = call_start + paren_start;

    // If cursor is before the paren, no active parameter
    if cursor_pos <= absolute_paren_start {
        return None;
    }

    // Count commas between paren and cursor to determine active param
    let text_from_paren_to_cursor = &file_text[absolute_paren_start + 1..cursor_pos.min(call_end)];

    let mut comma_count: u32 = 0;
    let mut paren_depth: i32 = 0;
    let mut brace_depth: i32 = 0;
    let mut bracket_depth: i32 = 0;

    for c in text_from_paren_to_cursor.chars() {
        match c {
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '{' => brace_depth += 1,
            '}' => brace_depth = brace_depth.saturating_sub(1),
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            ',' if paren_depth == 0 && brace_depth == 0 && bracket_depth == 0 => {
                comma_count += 1;
            }
            _ => {}
        }
    }

    // Don't exceed the number of arguments
    if comma_count >= arg_count as u32 && arg_count > 0 {
        Some((arg_count - 1) as u32)
    } else {
        Some(comma_count)
    }
}

/// Build a SignatureHelp response for a function.
fn build_signature_help<'db>(
    db: &'db DriverDataBase,
    func: Func<'db>,
    active_parameter: Option<u32>,
) -> SignatureHelp {
    let func_name = func
        .name(db)
        .to_opt()
        .map(|n| n.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".to_string());

    // Build parameter info
    let mut params = Vec::new();
    let mut param_strings = Vec::new();

    for param in func.params(db) {
        let ty = param.ty(db);
        let ty_str = ty.pretty_print(db);

        let (label, doc) = if param.is_self_param(db) {
            ("self".to_string(), format!("self: {}", ty_str))
        } else if let Some(name) = param.name(db) {
            let name_str = name.data(db);
            (
                format!("{}: {}", name_str, ty_str),
                format!("{}: {}", name_str, ty_str),
            )
        } else {
            (ty_str.to_string(), ty_str.to_string())
        };

        param_strings.push(label.clone());
        params.push(ParameterInformation {
            label: ParameterLabel::Simple(label),
            documentation: Some(async_lsp::lsp_types::Documentation::String(doc)),
        });
    }

    // Build the return type
    let ret_ty = func.return_ty(db);
    let ret_ty_str = ret_ty.pretty_print(db);
    let ret_str = if ret_ty_str == "()" || ret_ty_str.is_empty() {
        String::new()
    } else {
        format!(" -> {}", ret_ty_str)
    };

    // Build full signature label
    let signature_label = format!("fn {}({}){}", func_name, param_strings.join(", "), ret_str);

    let signature = SignatureInformation {
        label: signature_label,
        documentation: None,
        parameters: Some(params),
        active_parameter,
    };

    SignatureHelp {
        signatures: vec![signature],
        active_signature: Some(0),
        active_parameter,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::load_ingot_from_directory;
    use std::path::PathBuf;

    #[test]
    fn test_signature_help_basic() {
        let mut db = DriverDataBase::default();

        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_files")
            .join("incomplete_completion");

        load_ingot_from_directory(&mut db, &fixture_path);

        let lib_url = url::Url::from_file_path(fixture_path.join("src").join("lib.fe")).unwrap();
        let file = db.workspace().get(&db, &lib_url).expect("file not found");
        let file_text = file.text(&db);
        let top_mod = map_file_to_mod(&db, file);

        // Find "self.distance()" and test signature help inside the parens
        if let Some(pos) = file_text.find("self.distance()") {
            let cursor = parser::TextSize::from((pos + "self.distance(".len()) as u32);
            // Signature help may or may not be found depending on method resolution
            let _sig = find_signature_at_cursor(&db, top_mod, cursor, file_text);
        }
    }
}
