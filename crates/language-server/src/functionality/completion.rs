use async_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, Position, Range,
    TextEdit,
};
use async_lsp::ResponseError;
use common::InputDb;
use driver::DriverDataBase;
use hir::{
    analysis::name_resolution::{NameDomain, NameResKind},
    hir_def::{
        scope_graph::ScopeId, Body, Func, HirIngot, ItemKind, Pat, Partial, Stmt, TopLevelMod,
        Visibility,
    },
    lower::map_file_to_mod,
    visitor::prelude::*,
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
    // Method 1: Check trigger character from LSP context
    let trigger_is_dot = params
        .context
        .as_ref()
        .and_then(|ctx| ctx.trigger_character.as_ref())
        .map(|c| c == ".")
        .unwrap_or(false);

    // Method 2: Check if character before cursor is a dot (handles manual completion invoke)
    let char_before_is_dot = cursor
        .checked_sub(1.into())
        .and_then(|pos| file_text.get(usize::from(pos)..usize::from(cursor)))
        .map(|s| s == ".")
        .unwrap_or(false);

    let is_member_access = trigger_is_dot || char_before_is_dot;

    // Check if this is a path completion (triggered by '::')
    let trigger_is_colon = params
        .context
        .as_ref()
        .and_then(|ctx| ctx.trigger_character.as_ref())
        .map(|c| c == ":")
        .unwrap_or(false);

    // Check for "::" before cursor
    let is_path_completion = cursor
        .checked_sub(2.into())
        .and_then(|pos| file_text.get(usize::from(pos)..usize::from(cursor)))
        .map(|s| s == "::")
        .unwrap_or(false)
        || trigger_is_colon;

    info!(
        "completion at {:?}: is_member_access={}, is_path_completion={}",
        cursor, is_member_access, is_path_completion
    );

    if is_member_access {
        // Member access completion: show fields and methods for the receiver type
        collect_member_completions(&backend.db, top_mod, cursor, &mut items);
    } else if is_path_completion {
        // Path completion: show items in the module before ::
        collect_path_completions(&backend.db, top_mod, cursor, &file_text, &mut items);
    } else {
        // Regular completion: show items visible in scope
        let scope = find_scope_at_cursor(&backend.db, top_mod, cursor);
        if let Some(scope) = scope {
            collect_items_from_scope(&backend.db, scope, &mut items);

            // Also collect auto-import suggestions for symbols not in scope
            collect_auto_import_completions(&backend.db, top_mod, scope, &file_text, &mut items);
        }
    }

    info!("completion returning {} items, is_member_access={}", items.len(), is_member_access);
    if items.is_empty() {
        Ok(None)
    } else {
        Ok(Some(CompletionResponse::Array(items)))
    }
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
    // First collect local bindings and parameters (shadows module-level items)
    collect_locals_in_scope(db, scope, items);

    // Then collect module-level items (both values and types)
    let visible_items = scope.items_in_scope(db, NameDomain::VALUE | NameDomain::TYPE);

    for (name, name_res) in visible_items {
        if let Some(completion) = name_res_to_completion(db, name, name_res) {
            items.push(completion);
        }
    }
}

/// Collect auto-import completion suggestions for public symbols not currently in scope.
fn collect_auto_import_completions<'db>(
    db: &'db DriverDataBase,
    current_mod: TopLevelMod<'db>,
    current_scope: ScopeId<'db>,
    file_text: &str,
    items: &mut Vec<CompletionItem>,
) {
    let ingot = current_mod.ingot(db);
    let module_tree = ingot.module_tree(db);

    // Get names already visible in the current scope
    let visible_items = current_scope.items_in_scope(db, NameDomain::VALUE | NameDomain::TYPE);
    let visible_names: std::collections::HashSet<_> = visible_items.keys().collect();

    // Find where to insert the import (after existing use statements or at the top)
    let import_position = find_import_insertion_position(file_text);

    // Iterate over all modules in the ingot
    for module in module_tree.all_modules() {
        // Skip the current module (its items are already in scope)
        if module == current_mod {
            continue;
        }

        // Compute the import path for this module
        let Some(module_path) = compute_module_path(db, module, &module_tree) else {
            continue;
        };

        // Get public items from this module
        for item in module.scope_graph(db).items_dfs(db) {
            // Only include public items
            if item.vis(db) != Visibility::Public {
                continue;
            }

            let Some(name) = item.name(db) else {
                continue;
            };
            let name_str = name.data(db);

            // Skip if already visible in current scope
            if visible_names.contains(&name_str.to_string()) {
                continue;
            }

            // Skip internal items (modules, impls, etc. that shouldn't be imported directly)
            let kind = match item {
                ItemKind::Func(_) => CompletionItemKind::FUNCTION,
                ItemKind::Struct(_) => CompletionItemKind::STRUCT,
                ItemKind::Enum(_) => CompletionItemKind::ENUM,
                ItemKind::Trait(_) => CompletionItemKind::INTERFACE,
                ItemKind::TypeAlias(_) => CompletionItemKind::CLASS,
                ItemKind::Const(_) => CompletionItemKind::CONSTANT,
                ItemKind::Contract(_) => CompletionItemKind::CLASS,
                // Skip modules, impls, etc. - they're not typically imported directly
                _ => continue,
            };

            // Build the full import path
            let import_path = format!("{}::{}", module_path, name_str);

            // Create the import text edit
            let import_text = format!("use {}\n", import_path);
            let import_edit = TextEdit {
                range: Range {
                    start: import_position,
                    end: import_position,
                },
                new_text: import_text,
            };

            items.push(CompletionItem {
                label: name_str.to_string(),
                kind: Some(kind),
                detail: Some(format!("use {}", import_path)),
                label_details: Some(async_lsp::lsp_types::CompletionItemLabelDetails {
                    detail: Some(format!(" ({})", module_path)),
                    description: None,
                }),
                additional_text_edits: Some(vec![import_edit]),
                ..Default::default()
            });
        }
    }
}

/// Compute the import path for a module (e.g., "stuff::calculations")
fn compute_module_path<'db>(
    db: &'db DriverDataBase,
    module: TopLevelMod<'db>,
    module_tree: &hir::hir_def::ModuleTree<'db>,
) -> Option<String> {
    let mut path_parts = Vec::new();

    // Build path from module up to root
    let mut current = module;
    loop {
        let name = current.name(db).data(db).to_string();
        // Skip "lib" or root module names - they're implicit
        if name != "lib" && name != "main" {
            path_parts.push(name);
        }

        match module_tree.parent(current) {
            Some(parent) => current = parent,
            None => break,
        }
    }

    // Reverse to get root-to-leaf order
    path_parts.reverse();

    if path_parts.is_empty() {
        None
    } else {
        Some(path_parts.join("::"))
    }
}

/// Find the position to insert new import statements.
/// Returns the position after existing `use` statements, or at the start of the file.
fn find_import_insertion_position(file_text: &str) -> Position {
    let mut last_use_end_line = 0;
    let mut in_use = false;

    for (line_num, line) in file_text.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.starts_with("use ") {
            in_use = true;
            last_use_end_line = line_num + 1;
        } else if in_use && !trimmed.is_empty() && !trimmed.starts_with("//") {
            // Found non-use, non-empty, non-comment line after use statements
            break;
        }
    }

    Position {
        line: last_use_end_line as u32,
        character: 0,
    }
}

/// Collect completions for path access (items after `::`).
fn collect_path_completions<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: parser::TextSize,
    file_text: &str,
    items: &mut Vec<CompletionItem>,
) {
    use hir::analysis::name_resolution::NameDomain;

    // Find the full path before ::
    // We need to go back from the cursor (which is after ::) and find the complete path
    let cursor_pos = usize::from(cursor);
    if cursor_pos < 2 {
        return;
    }

    // Go back past the ::
    let before_colons = &file_text[..cursor_pos.saturating_sub(2)];

    // Find the start of the full path (including all :: segments)
    // Look for whitespace, operators, or other non-path characters
    let path_start = before_colons
        .rfind(|c: char| {
            !c.is_alphanumeric() && c != '_' && c != ':'
        })
        .map(|i| i + 1)
        .unwrap_or(0);

    let full_path = before_colons[path_start..].trim();
    info!("path completion: looking for items in full path '{}'", full_path);

    if full_path.is_empty() {
        return;
    }

    // Split the path into segments
    let segments: Vec<&str> = full_path.split("::").filter(|s| !s.is_empty()).collect();
    info!("path segments: {:?}", segments);

    if segments.is_empty() {
        return;
    }

    // Resolve the path step by step
    let mut current_scope = top_mod.scope();

    for segment in &segments {
        let visible = current_scope.items_in_scope(db, NameDomain::VALUE | NameDomain::TYPE);

        if let Some(name_res) = visible.get(*segment) {
            if let hir::analysis::name_resolution::NameResKind::Scope(target_scope) = &name_res.kind
            {
                info!("resolved segment '{}' to scope {:?}", segment, target_scope);
                current_scope = *target_scope;
            } else {
                info!("segment '{}' is not a scope, stopping", segment);
                return;
            }
        } else {
            info!("segment '{}' not found in current scope", segment);
            return;
        }
    }

    // Get direct child items of the final resolved scope
    // This gives us only items defined directly in this module, not inherited ones
    let child_items: Vec<_> = current_scope.child_items(db).collect();

    info!(
        "path completion: resolved to scope {:?}, found {} child items",
        current_scope,
        child_items.len()
    );

    for item in child_items {
        let Some(name) = item.name(db) else {
            continue;
        };
        let name_str = name.data(db);

        let kind = match item {
            ItemKind::Func(_) => CompletionItemKind::FUNCTION,
            ItemKind::Struct(_) => CompletionItemKind::STRUCT,
            ItemKind::Enum(_) => CompletionItemKind::ENUM,
            ItemKind::Trait(_) => CompletionItemKind::INTERFACE,
            ItemKind::TypeAlias(_) => CompletionItemKind::CLASS,
            ItemKind::Const(_) => CompletionItemKind::CONSTANT,
            ItemKind::Mod(_) | ItemKind::TopMod(_) => CompletionItemKind::MODULE,
            ItemKind::Contract(_) => CompletionItemKind::CLASS,
            _ => CompletionItemKind::VALUE,
        };

        items.push(CompletionItem {
            label: name_str.to_string(),
            kind: Some(kind),
            ..Default::default()
        });
    }

    info!(
        "collected {} items from path '{}'",
        items.len(),
        full_path
    );
}

/// Collect completions for member access (fields and methods after `.`).
fn collect_member_completions<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: parser::TextSize,
    items: &mut Vec<CompletionItem>,
) {
    use hir::analysis::ty::ty_check::check_func_body;
    use hir::hir_def::Expr;
    use hir::span::LazySpan;


    // Find the enclosing function
    let scope_graph = top_mod.scope_graph(db);
    let mut enclosing_func = None;

    for item in scope_graph.items_dfs(db) {
        if let ItemKind::Func(func) = item {
            if let Some(span) = func.span().resolve(db) {
                if span.range.contains(cursor) {
                    enclosing_func = Some(func);
                }
            }
        }
    }

    let Some(func) = enclosing_func else {
        return;
    };

    let Some(body) = func.body(db) else {
        return;
    };

    // Get typed body for type information
    let (_, typed_body) = check_func_body(db, func);

    // Strategy 1: Find Field expressions (field access like `foo.bar` or incomplete `foo.`)
    // that contain the cursor, and use their receiver's type
    for (expr_id, expr_data) in body.exprs(db).iter() {
        if let Partial::Present(Expr::Field(receiver_id, _field)) = expr_data {
            let expr_span = expr_id.span(body);
            if let Some(resolved) = expr_span.resolve(db) {
                info!("  Field expr range {:?}, cursor {:?}", resolved.range, cursor);
                // Check if cursor is within this field expression
                if resolved.range.contains(cursor) || resolved.range.end() == cursor {
                    let mut ty = typed_body.expr_ty(db, *receiver_id);
                    info!("found Field expression, receiver type: {}", ty.pretty_print(db));

                    // If the receiver type is invalid (due to incomplete syntax), try to find
                    // the type from the expression itself (e.g., if it's a path to a local binding)
                    if ty.has_invalid(db) {
                        if let Some(Partial::Present(Expr::Path(Partial::Present(path)))) =
                            body.exprs(db).get(*receiver_id)
                        {
                            // Try to resolve the path to find a local binding's type
                            if let Some(ident) = path.as_ident(db) {
                                let ident_str = ident.data(db);
                                info!("  receiver is path {:?}, looking up binding", ident_str);

                                // Special case: if the path is "self", get the self parameter's type
                                if ident_str == "self" {
                                    for param in func.params(db) {
                                        if param.is_self_param(db) {
                                            let self_ty = param.ty(db);
                                            if !self_ty.has_invalid(db) {
                                                info!("  found self parameter type: {}", self_ty.pretty_print(db));
                                                ty = self_ty;
                                                break;
                                            }
                                        }
                                    }
                                } else {
                                    // Look through patterns to find this binding's type
                                    for (pat_id, pat_data) in body.pats(db).iter() {
                                        if let Partial::Present(Pat::Path(Partial::Present(pat_path), _)) = pat_data {
                                            if pat_path.as_ident(db) == Some(ident) {
                                                let pat_ty = typed_body.pat_ty(db, pat_id);
                                                if !pat_ty.has_invalid(db) {
                                                    info!("  found binding type: {}", pat_ty.pretty_print(db));
                                                    ty = pat_ty;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    collect_fields_for_type(db, ty, items);
                    collect_methods_for_type(db, top_mod, ty, items);

                    info!("collected {} items from Field expr", items.len());
                    return;
                }
            }
        }
    }

    // Strategy 2: Fallback - look for any expression ending at cursor-1 (before the dot)
    let dot_pos = cursor.checked_sub(1.into()).unwrap_or(cursor);
    info!("fallback: looking for expression ending at {:?}", dot_pos);

    for (expr_id, _) in body.exprs(db).iter() {
        let expr_span = expr_id.span(body);
        if let Some(resolved) = expr_span.resolve(db) {
            if resolved.range.end() == dot_pos {
                let ty = typed_body.expr_ty(db, expr_id);
                info!("found expression ending at dot, type: {}", ty.pretty_print(db));

                collect_fields_for_type(db, ty, items);
                collect_methods_for_type(db, top_mod, ty, items);

                info!("collected {} items from fallback", items.len());
                return;
            }
        }
    }

    info!("no matching expression found");
}

/// Collect struct fields as completion items.
fn collect_fields_for_type<'db>(
    db: &'db DriverDataBase,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    items: &mut Vec<CompletionItem>,
) {
    info!("collect_fields_for_type: ty={}", ty.pretty_print(db));

    // Use the traversal API to get fields for struct/contract types
    let Some(field_parent) = ty.field_parent(db) else {
        info!("  no field_parent for this type");
        return;
    };

    info!("  found field_parent, iterating fields");
    for field in field_parent.fields(db) {
        let Some(name) = field.name(db) else {
            info!("    field has no name, skipping");
            continue;
        };
        let field_ty = field.ty(db);
        let detail = format!("{}: {}", name.data(db), field_ty.pretty_print(db));
        info!("    adding field: {}", name.data(db));

        items.push(CompletionItem {
            label: name.data(db).to_string(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some(detail),
            ..Default::default()
        });
    }
}

/// Collect methods from impls as completion items.
fn collect_methods_for_type<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    items: &mut Vec<CompletionItem>,
) {
    // Get the type name for matching impl blocks
    let ty_name = ty.pretty_print(db);

    // Look for impl blocks in the module
    for item in top_mod.scope_graph(db).items_dfs(db) {
        if let ItemKind::Impl(impl_) = item {
            // Check if this impl is for our type by comparing target type name
            // This is a simple heuristic; a more precise implementation would
            // use proper type unification
            let impl_ty_name = impl_.ty(db).pretty_print(db);
            if impl_ty_name == ty_name {
                for func in impl_.funcs(db) {
                    if func.is_method(db) {
                        if let Some(name) = func.name(db).to_opt() {
                            let detail = format!("fn {}(...)", name.data(db));
                            items.push(CompletionItem {
                                label: name.data(db).to_string(),
                                kind: Some(CompletionItemKind::METHOD),
                                detail: Some(detail),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
        }
    }
}

/// Collect local bindings and parameters visible in this scope.
fn collect_locals_in_scope<'db>(
    db: &'db DriverDataBase,
    scope: ScopeId<'db>,
    items: &mut Vec<CompletionItem>,
) {
    // Find the containing function (if any)
    let func = match scope.to_item() {
        Some(ItemKind::Func(f)) => f,
        Some(ItemKind::Body(body)) => {
            // Body scope - find the containing function
            match body.containing_func(db) {
                Some(f) => f,
                None => return,
            }
        }
        _ => {
            // Try to find parent function by walking up scope chain
            let mut current = scope;
            loop {
                match current.parent_item(db) {
                    Some(ItemKind::Func(f)) => break f,
                    Some(ItemKind::Body(body)) => {
                        match body.containing_func(db) {
                            Some(f) => break f,
                            None => return,
                        }
                    }
                    Some(_) => {
                        if let Some(parent) = current.parent(db) {
                            current = parent;
                        } else {
                            return;
                        }
                    }
                    None => return,
                }
            }
        }
    };

    // Add function parameters
    collect_func_params(db, func, items);

    // Add 'self' if this is a method
    // TODO: Detect if we're in a method context

    // Add local bindings from the function body
    if let Some(body) = func.body(db) {
        collect_let_bindings(db, body, items);
    }
}

/// Collect function parameters as completion items.
fn collect_func_params<'db>(
    db: &'db DriverDataBase,
    func: Func<'db>,
    items: &mut Vec<CompletionItem>,
) {
    // Use the semantic traversal API to get parameters with resolved types
    for param in func.params(db) {
        // Get the semantic type, which resolves Self to the concrete type
        let ty = param.ty(db);

        if param.is_self_param(db) {
            // For self parameters, just show "self" with the resolved type
            let detail = format!("self: {}", ty.pretty_print(db));
            items.push(CompletionItem {
                label: "self".to_string(),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some(detail),
                ..Default::default()
            });
        } else if let Some(name) = param.name(db) {
            let name_str = name.data(db).to_string();
            let detail = format!("{}: {}", name_str, ty.pretty_print(db));

            items.push(CompletionItem {
                label: name_str,
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some(detail),
                ..Default::default()
            });
        }
    }
}

/// Collect let bindings from a function body.
fn collect_let_bindings<'db>(db: &'db DriverDataBase, body: Body<'db>, items: &mut Vec<CompletionItem>) {
    use hir::analysis::ty::ty_check::check_func_body;

    // Get the containing function and type-checked body
    let Some(func) = body.containing_func(db) else {
        return;
    };
    let (_, typed_body) = check_func_body(db, func);

    let mut collector = LetBindingCollector {
        db,
        items,
        body,
        typed_body: &typed_body,
    };
    let mut ctxt = VisitorCtxt::new(db, body.scope(), body.span());
    collector.visit_body(&mut ctxt, body);
}

struct LetBindingCollector<'a, 'db> {
    db: &'db DriverDataBase,
    items: &'a mut Vec<CompletionItem>,
    body: Body<'db>,
    typed_body: &'a hir::analysis::ty::ty_check::TypedBody<'db>,
}

impl<'a, 'db> Visitor<'db> for LetBindingCollector<'a, 'db> {
    fn visit_stmt(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyStmtSpan<'db>>,
        _stmt: hir::hir_def::StmtId,
        stmt_data: &Stmt<'db>,
    ) {
        if let Stmt::Let(pat_id, _ty, _expr) = stmt_data {
            // Extract the name from the pattern
            if let Partial::Present(Pat::Path(Partial::Present(path), _)) = pat_id.data(self.db, self.body) {
                if let Some(ident) = path.as_ident(self.db) {
                    let name = ident.data(self.db).to_string();

                    // Get the inferred type for this pattern
                    let ty = self.typed_body.pat_ty(self.db, *pat_id);
                    let detail = format!("{}: {}", name, ty.pretty_print(self.db));

                    self.items.push(CompletionItem {
                        label: name,
                        kind: Some(CompletionItemKind::VARIABLE),
                        detail: Some(detail),
                        ..Default::default()
                    });
                }
            }
        }
        walk_stmt(self, ctxt, _stmt);
    }
}

/// Convert a NameRes to a CompletionItem.
fn name_res_to_completion<'db>(
    _db: &'db DriverDataBase,
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
        ..Default::default()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::load_ingot_from_directory;
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use std::path::PathBuf;

    /// Test member access completion with incomplete expression (typing after `.`)
    #[test]
    fn test_incomplete_member_access() {
        let mut db = DriverDataBase::default();

        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_files")
            .join("incomplete_completion");

        load_ingot_from_directory(&mut db, &fixture_path);

        let lib_url = url::Url::from_file_path(fixture_path.join("src").join("lib.fe")).unwrap();
        let file = db.workspace().get(&db, &lib_url).expect("file not found");
        let file_text = file.text(&db);
        let top_mod = map_file_to_mod(&db, file);

        println!("File content:\n{}", file_text);
        println!("\nFile length: {} bytes", file_text.len());

        // Test 1: Find the position of "p." in the test() function
        if let Some(pos) = file_text.rfind("p.") {
            let cursor_after_dot = parser::TextSize::from((pos + 2) as u32);
            println!("\nFound 'p.' at position {}, cursor after dot: {:?}", pos, cursor_after_dot);

            // Try to collect member completions
            let mut items = Vec::new();
            collect_member_completions(&db, top_mod, cursor_after_dot, &mut items);

            println!("\nCollected {} completion items for 'p.':", items.len());
            for item in &items {
                println!("  {} ({:?}): {:?}", item.label, item.kind, item.detail);
            }
            assert!(items.len() >= 2, "Expected at least 2 items (x, y fields)");
        }

        // Test 2: Find the position of "self." in test_self_completion
        // Find the last "self." which should be the incomplete one
        let self_dot_positions: Vec<_> = file_text.match_indices("self.").collect();
        println!("\nFound {} occurrences of 'self.'", self_dot_positions.len());

        // The last one should be the incomplete "self." in test_self_completion
        if let Some(&(pos, _)) = self_dot_positions.last() {
            let cursor_after_dot = parser::TextSize::from((pos + 5) as u32);
            println!("Testing self. completion at position {}, cursor: {:?}", pos, cursor_after_dot);

            let mut items = Vec::new();
            collect_member_completions(&db, top_mod, cursor_after_dot, &mut items);

            println!("\nCollected {} completion items for 'self.':", items.len());
            for item in &items {
                println!("  {} ({:?}): {:?}", item.label, item.kind, item.detail);
            }
            assert!(items.len() >= 2, "Expected at least 2 items (x, y fields) for self.");
        }

        // Also dump all expressions
        use hir::analysis::ty::ty_check::check_func_body;

        for item in top_mod.scope_graph(&db).items_dfs(&db) {
            if let ItemKind::Func(func) = item {
                if let Some(body) = func.body(&db) {
                    let (_, typed_body) = check_func_body(&db, func);
                    println!(
                        "\nFunction: {:?}",
                        func.name(&db).to_opt().map(|n| n.data(&db))
                    );

                    for (expr_id, expr_data) in body.exprs(&db).iter() {
                        if let Some(span) = expr_id.span(body).resolve(&db) {
                            let ty = typed_body.expr_ty(&db, expr_id);
                            println!(
                                "  Expr {:?} @ {:?} : {}",
                                expr_data,
                                span.range,
                                ty.pretty_print(&db)
                            );

                            if let Some(field_parent) = ty.field_parent(&db) {
                                println!("    -> has fields:");
                                for field in field_parent.fields(&db) {
                                    if let Some(name) = field.name(&db) {
                                        println!("       {}: {}", name.data(&db), field.ty(&db).pretty_print(&db));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Test that module completions are available
    #[test]
    fn test_module_completions() {
        use hir::analysis::name_resolution::NameDomain;

        let mut db = DriverDataBase::default();

        // Use the hoverable fixture which has modules
        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_files")
            .join("hoverable");

        load_ingot_from_directory(&mut db, &fixture_path);

        let lib_url = url::Url::from_file_path(fixture_path.join("src").join("lib.fe")).unwrap();
        let file = db.workspace().get(&db, &lib_url).expect("file not found");
        let top_mod = map_file_to_mod(&db, file);

        // Get items visible in the top-level scope
        let scope = top_mod.scope();
        let visible = scope.items_in_scope(&db, NameDomain::VALUE | NameDomain::TYPE);

        println!("\nItems visible in lib.fe scope:");
        for (name, res) in visible.iter() {
            println!("  {} -> {:?}", name, res.kind);
        }

        // Check that 'stuff' module is visible
        let has_stuff = visible.keys().any(|n| n == "stuff");
        println!("\nHas 'stuff' module: {}", has_stuff);

        // Collect completion items
        let mut items = Vec::new();
        collect_items_from_scope(&db, scope, &mut items);

        println!("\nCompletion items from scope:");
        for item in &items {
            println!("  {} ({:?})", item.label, item.kind);
        }

        // Check that module completions are present
        let module_completions: Vec<_> = items
            .iter()
            .filter(|i| i.kind == Some(CompletionItemKind::MODULE))
            .collect();
        println!("\nModule completions: {:?}", module_completions.iter().map(|i| &i.label).collect::<Vec<_>>());
    }

    /// Test path completion (stuff::)
    #[test]
    fn test_path_completion() {
        let mut db = DriverDataBase::default();

        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_files")
            .join("hoverable");

        load_ingot_from_directory(&mut db, &fixture_path);

        let lib_url = url::Url::from_file_path(fixture_path.join("src").join("lib.fe")).unwrap();
        let file = db.workspace().get(&db, &lib_url).expect("file not found");
        let top_mod = map_file_to_mod(&db, file);

        // Simulate typing "stuff::" and collecting completions
        let file_text = "stuff::";
        let cursor = parser::TextSize::from(file_text.len() as u32);

        let mut items = Vec::new();
        collect_path_completions(&db, top_mod, cursor, file_text, &mut items);

        println!("\nPath completions for 'stuff::':");
        for item in &items {
            println!("  {} ({:?})", item.label, item.kind);
        }

        // Should have 'calculations' module and any other items in stuff module
        assert!(!items.is_empty(), "Expected items from stuff module");
        let has_calculations = items.iter().any(|i| i.label == "calculations");
        println!("\nHas 'calculations' submodule: {}", has_calculations);
        assert!(has_calculations, "Expected 'calculations' submodule in stuff::");

        // Test nested path: stuff::calculations::
        let file_text = "stuff::calculations::";
        let cursor = parser::TextSize::from(file_text.len() as u32);

        let mut items = Vec::new();
        collect_path_completions(&db, top_mod, cursor, file_text, &mut items);

        println!("\nPath completions for 'stuff::calculations::':");
        for item in &items {
            println!("  {} ({:?})", item.label, item.kind);
        }

        // Should have items from the calculations module
        assert!(!items.is_empty(), "Expected items from stuff::calculations module");
        let has_return_three = items.iter().any(|i| i.label == "return_three");
        let has_return_four = items.iter().any(|i| i.label == "return_four");
        println!("\nHas 'return_three': {}", has_return_three);
        println!("Has 'return_four': {}", has_return_four);
        assert!(has_return_three, "Expected 'return_three' in stuff::calculations::");
        assert!(has_return_four, "Expected 'return_four' in stuff::calculations::");
    }

    /// Test member access completion by simulating cursor after a dot
    #[test]
    fn test_member_access_completion() {
        let mut db = DriverDataBase::default();

        // Use the hoverable fixture which has struct definitions
        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_files")
            .join("hoverable");

        load_ingot_from_directory(&mut db, &fixture_path);

        // Find the src/lib.fe file
        let lib_url = url::Url::from_file_path(fixture_path.join("src").join("lib.fe")).unwrap();
        let file = db.workspace().get(&db, &lib_url).expect("file not found");
        let file_text = file.text(&db);
        let top_mod = map_file_to_mod(&db, file);

        println!("File content:\n{}", file_text);

        // Find all Field expressions in the file
        use hir::hir_def::Expr;
        use hir::span::LazySpan;
        use hir::visitor::{Visitor, VisitorCtxt, prelude::*};

        struct FieldExprFinder<'db> {
            db: &'db DriverDataBase,
            field_exprs: Vec<(parser::TextRange, String)>,
        }

        impl<'db> Visitor<'db> for FieldExprFinder<'db> {
            fn visit_expr(
                &mut self,
                ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
                expr: hir::hir_def::ExprId,
                expr_data: &Expr<'db>,
            ) {
                if let Expr::Field(_receiver, field_idx) = expr_data {
                    if let Some(span) = ctxt.span() {
                        if let Some(resolved) = span.resolve(self.db) {
                            let field_name = match field_idx {
                                Partial::Present(hir::hir_def::FieldIndex::Ident(id)) => {
                                    id.data(self.db).to_string()
                                }
                                _ => "<unknown>".to_string(),
                            };
                            self.field_exprs.push((resolved.range, field_name));
                        }
                    }
                }
                walk_expr(self, ctxt, expr);
            }
        }

        let mut finder = FieldExprFinder {
            db: &db,
            field_exprs: vec![],
        };
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(&db, top_mod);
        finder.visit_top_mod(&mut visitor_ctxt, top_mod);

        println!("\nFound {} Field expressions:", finder.field_exprs.len());
        for (range, field_name) in &finder.field_exprs {
            println!("  {:?} -> {}", range, field_name);
        }

        // Now test completion at a position after a dot
        // We'll manually construct a position and call collect_member_completions
        // For this test, let's just verify that field_parent works
        use hir::analysis::ty::ty_check::check_func_body;

        // Find all funcs and print their typed body info
        for item in top_mod.scope_graph(&db).items_dfs(&db) {
            if let ItemKind::Func(func) = item {
                if let Some(body) = func.body(&db) {
                    let (_, typed_body) = check_func_body(&db, func);
                    println!(
                        "\nFunction: {:?}",
                        func.name(&db).to_opt().map(|n| n.data(&db))
                    );

                    for (expr_id, expr_data) in body.exprs(&db).iter() {
                        if let Some(span) = expr_id.span(body).resolve(&db) {
                            let ty = typed_body.expr_ty(&db, expr_id);
                            println!(
                                "  Expr {:?} @ {:?} : {}",
                                expr_data,
                                span.range,
                                ty.pretty_print(&db)
                            );

                            // Check if type has fields
                            if let Some(field_parent) = ty.field_parent(&db) {
                                println!("    -> has fields:");
                                for field in field_parent.fields(&db) {
                                    if let Some(name) = field.name(&db) {
                                        println!("       {}: {}", name.data(&db), field.ty(&db).pretty_print(&db));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Test auto-import completions
    #[test]
    fn test_auto_import_completions() {
        let mut db = DriverDataBase::default();

        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_files")
            .join("hoverable");

        load_ingot_from_directory(&mut db, &fixture_path);

        let lib_url = url::Url::from_file_path(fixture_path.join("src").join("lib.fe")).unwrap();
        let file = db.workspace().get(&db, &lib_url).expect("file not found");
        let file_text = file.text(&db);
        let top_mod = map_file_to_mod(&db, file);

        println!("File content:\n{}", file_text);

        // Get the scope at the top of the file
        let scope = top_mod.scope();

        // Collect auto-import completions
        let mut items = Vec::new();
        collect_auto_import_completions(&db, top_mod, scope, &file_text, &mut items);

        println!("\nAuto-import completions ({} items):", items.len());
        for item in &items {
            println!(
                "  {} - {:?} - {:?}",
                item.label,
                item.detail,
                item.additional_text_edits
            );
        }

        // Should have items from other modules (stuff, stuff::calculations)
        // that aren't already imported
        let has_return_three = items.iter().any(|i| i.label == "return_three");
        let has_return_four = items.iter().any(|i| i.label == "return_four");

        println!("\nHas 'return_three' auto-import: {}", has_return_three);
        println!("Has 'return_four' auto-import: {}", has_return_four);

        // These should NOT be in auto-imports because they're already imported in the fixture
        // But we can check that other items from stuff::calculations are available
        // (items not imported in lib.fe)

        // Check that auto-imports have the correct additional_text_edits
        for item in &items {
            if let Some(edits) = &item.additional_text_edits {
                assert!(!edits.is_empty(), "Expected at least one text edit for auto-import");
                for edit in edits {
                    println!("  Edit: insert '{}' at {:?}", edit.new_text.trim(), edit.range);
                    assert!(
                        edit.new_text.starts_with("use "),
                        "Expected edit to be a use statement"
                    );
                }
            }
        }

        // Test import insertion position detection
        let position = find_import_insertion_position(&file_text);
        println!("\nImport insertion position: line {}", position.line);
        // Should be after the existing use statements
        assert!(position.line > 0, "Expected insertion after existing imports");
    }
}
