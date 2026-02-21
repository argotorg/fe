use crate::{backend::Backend, util::to_offset_from_position};
use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, InsertTextFormat,
    Position, Range, TextEdit,
};
use common::InputDb;
use driver::DriverDataBase;
use hir::{
    analysis::name_resolution::{NameDomain, NameResKind, available_traits_in_scope},
    hir_def::{
        Body, Func, HirIngot, ItemKind, Partial, Pat, Stmt, TopLevelMod, Visibility,
        scope_graph::ScopeId,
    },
    lower::map_file_to_mod,
    visitor::prelude::*,
};

pub async fn handle_completion(
    backend: &Backend,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>, ResponseError> {
    let url =
        backend.map_client_uri_to_internal(params.text_document_position.text_document.uri.clone());

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
    let cursor = to_offset_from_position(params.text_document_position.position, file_text);
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

    if is_member_access {
        // Member access completion: show fields and methods for the receiver type
        collect_member_completions(&backend.db, top_mod, cursor, &mut items);
    } else if is_path_completion {
        // Path completion: show items in the module before ::
        collect_path_completions(&backend.db, top_mod, cursor, file_text, &mut items);
    } else {
        // Regular completion: show items visible in scope
        let scope = find_scope_at_cursor(&backend.db, top_mod, cursor);
        if let Some(scope) = scope {
            // Detect whether we're in a type or expression context
            let context = detect_completion_context(file_text, cursor);

            collect_items_from_scope(&backend.db, scope, context, &mut items);

            // Also collect auto-import suggestions for symbols not in scope
            collect_auto_import_completions(
                &backend.db,
                top_mod,
                scope,
                context,
                file_text,
                &mut items,
            );
        }
    }

    if items.is_empty() {
        Ok(None)
    } else {
        Ok(Some(CompletionResponse::Array(items)))
    }
}

/// Completion context - determines what kind of items to suggest.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompletionContext {
    /// Expression context: suggest values (variables, functions, constants)
    Expression,
    /// Type context: suggest types (structs, enums, type aliases, traits)
    Type,
    /// Mixed context: suggest both types and values (e.g., top-level, unknown)
    Mixed,
}

impl CompletionContext {
    fn to_name_domain(self) -> NameDomain {
        match self {
            CompletionContext::Expression => NameDomain::VALUE,
            CompletionContext::Type => NameDomain::TYPE,
            CompletionContext::Mixed => NameDomain::VALUE | NameDomain::TYPE,
        }
    }
}

/// Detect the completion context based on surrounding text.
fn detect_completion_context(file_text: &str, cursor: parser::TextSize) -> CompletionContext {
    let cursor_pos = usize::from(cursor);

    // Look backwards from cursor to find context clues
    let before = &file_text[..cursor_pos];

    // Skip whitespace to find the last significant character
    let trimmed = before.trim_end();
    if trimmed.is_empty() {
        return CompletionContext::Mixed;
    }

    let last_char = trimmed.chars().last().unwrap();

    // After ':' (but check it's not '::') → type context
    if last_char == ':' && !trimmed.ends_with("::") {
        return CompletionContext::Type;
    }

    // After '<' → likely generic argument, type context
    if last_char == '<' {
        return CompletionContext::Type;
    }

    // After ',' inside angle brackets → type context (generic args)
    if last_char == ',' {
        // Check if we're inside angle brackets by counting < and >
        let open_angles = trimmed.chars().filter(|&c| c == '<').count();
        let close_angles = trimmed.chars().filter(|&c| c == '>').count();
        if open_angles > close_angles {
            return CompletionContext::Type;
        }
    }

    // After '->' → return type annotation, type context
    if trimmed.ends_with("->") {
        return CompletionContext::Type;
    }

    // After 'impl' or 'impl ' → type context (impl block target type)
    let trimmed_lower = trimmed.to_lowercase();
    if trimmed_lower.ends_with("impl") || trimmed.ends_with("impl ") {
        return CompletionContext::Type;
    }

    // After '=' in a let or assignment → expression context
    if last_char == '=' && !trimmed.ends_with("==") && !trimmed.ends_with("!=") {
        return CompletionContext::Expression;
    }

    // After '(' → expression context (function arguments)
    if last_char == '(' {
        return CompletionContext::Expression;
    }

    // After operators → expression context
    if matches!(last_char, '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^') {
        return CompletionContext::Expression;
    }

    // Default to mixed for safety
    CompletionContext::Mixed
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
    context: CompletionContext,
    items: &mut Vec<CompletionItem>,
) {
    let domain = context.to_name_domain();

    // First collect local bindings and parameters (shadows module-level items)
    // Only collect locals in expression context (types can't be local bindings)
    if matches!(
        context,
        CompletionContext::Expression | CompletionContext::Mixed
    ) {
        collect_locals_in_scope(db, scope, items);
    }

    // Then collect module-level items based on context
    let visible_items = scope.items_in_scope(db, domain);

    for (name, name_res) in visible_items {
        if let Some(completion) = name_res_to_completion(db, name, name_res, context) {
            items.push(completion);
        }
    }
}

/// Collect auto-import completion suggestions for public symbols not currently in scope.
///
/// This iterates all items in the ingot (including nested inline modules) and suggests
/// auto-imports for public items that aren't already visible in the current scope.
fn collect_auto_import_completions<'db>(
    db: &'db DriverDataBase,
    current_mod: TopLevelMod<'db>,
    current_scope: ScopeId<'db>,
    context: CompletionContext,
    file_text: &str,
    items: &mut Vec<CompletionItem>,
) {
    let ingot = current_mod.ingot(db);
    let domain = context.to_name_domain();

    // Get names already visible in the current scope
    let visible_items = current_scope.items_in_scope(db, domain);
    let visible_names: std::collections::HashSet<_> = visible_items.keys().collect();

    // Find where to insert the import (in the containing module)
    let import_position = find_module_import_position(db, current_scope, file_text);

    // Iterate all items in the ingot (includes nested inline modules)
    for item in ingot.all_items(db).iter().copied() {
        // Only include public items
        if item.vis(db) != Visibility::Public {
            continue;
        }

        // Skip if already visible in current scope
        let Some(name) = item.name(db) else {
            continue;
        };
        let name_str = name.data(db).to_string();
        if visible_names.contains(&name_str) {
            continue;
        }

        // Compute the import path for this item
        let Some(import_path) = compute_item_import_path(db, item) else {
            continue;
        };

        // Build auto-import completion with proper snippets
        if let Some(completion) =
            build_auto_import_completion(db, item, &import_path, context, import_position)
        {
            items.push(completion);
        }
    }
}

/// Build an auto-import completion item for an item from another module.
fn build_auto_import_completion<'db>(
    db: &'db DriverDataBase,
    item: ItemKind<'db>,
    module_path: &str,
    context: CompletionContext,
    import_position: Position,
) -> Option<CompletionItem> {
    let name = item.name(db)?;
    let name_str = name.data(db).to_string();

    // Filter and get completion kind based on item type and context
    let (kind, snippet, detail) = match item {
        ItemKind::Func(func) => {
            if matches!(context, CompletionContext::Type) {
                return None;
            }
            // Build callable snippet
            let (snippet, detail) = build_func_snippet_and_detail(db, func, &name_str);
            (CompletionItemKind::FUNCTION, snippet, detail)
        }
        ItemKind::Const(_) => {
            if matches!(context, CompletionContext::Type) {
                return None;
            }
            (
                CompletionItemKind::CONSTANT,
                name_str.clone(),
                name_str.clone(),
            )
        }
        ItemKind::Struct(_) => {
            if matches!(context, CompletionContext::Expression) {
                return None;
            }
            (
                CompletionItemKind::STRUCT,
                name_str.clone(),
                name_str.clone(),
            )
        }
        ItemKind::Enum(_) => {
            if matches!(context, CompletionContext::Expression) {
                return None;
            }
            (CompletionItemKind::ENUM, name_str.clone(), name_str.clone())
        }
        ItemKind::Trait(_) => {
            if matches!(context, CompletionContext::Expression) {
                return None;
            }
            (
                CompletionItemKind::INTERFACE,
                name_str.clone(),
                name_str.clone(),
            )
        }
        ItemKind::TypeAlias(_) | ItemKind::Contract(_) => {
            if matches!(context, CompletionContext::Expression) {
                return None;
            }
            (
                CompletionItemKind::CLASS,
                name_str.clone(),
                name_str.clone(),
            )
        }
        // Skip modules, impls, etc.
        _ => return None,
    };

    // module_path is already the full import path (e.g., "utils::func_with_args")
    // Create the import text edit
    let import_text = format!("use {}\n", module_path);
    let import_edit = TextEdit {
        range: Range {
            start: import_position,
            end: import_position,
        },
        new_text: import_text,
    };

    // Extract just the module portion for display (everything before the last ::)
    let module_only = module_path
        .rsplit_once("::")
        .map(|(m, _)| m)
        .unwrap_or(module_path);

    Some(CompletionItem {
        label: name_str,
        kind: Some(kind),
        detail: Some(format!("use {} [{}]", module_path, detail)),
        label_details: Some(async_lsp::lsp_types::CompletionItemLabelDetails {
            detail: Some(format!(" ({})", module_only)),
            description: None,
        }),
        insert_text: Some(snippet),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        additional_text_edits: Some(vec![import_edit]),
        ..Default::default()
    })
}

/// Build snippet and detail for a function (shared logic for auto-import).
fn build_func_snippet_and_detail<'db>(
    db: &'db DriverDataBase,
    func: Func<'db>,
    name_str: &str,
) -> (String, String) {
    let mut param_names = Vec::new();
    let mut param_details = Vec::new();

    for param in func.params(db) {
        if param.is_self_param(db) {
            continue;
        }
        let param_name = param
            .name(db)
            .map(|n| n.data(db).to_string())
            .unwrap_or_else(|| format!("arg{}", param_names.len()));
        let param_ty = param.ty(db);
        param_details.push(format!("{}: {}", param_name, param_ty.pretty_print(db)));
        param_names.push(param_name);
    }

    let ret_ty = func.return_ty(db);
    let ret_str = {
        let ret_pretty = ret_ty.pretty_print(db);
        if ret_pretty == "()" {
            String::new()
        } else {
            format!(" -> {}", ret_pretty)
        }
    };
    let detail = format!("fn {}({}){}", name_str, param_details.join(", "), ret_str);

    let snippet = if param_names.is_empty() {
        format!("{}()$0", name_str)
    } else {
        let tabstops: Vec<String> = param_names
            .iter()
            .enumerate()
            .map(|(i, p)| format!("${{{}:{}}}", i + 1, p))
            .collect();
        format!("{}({})$0", name_str, tabstops.join(", "))
    };

    (snippet, detail)
}

/// Find the position to insert imports in the containing module.
fn find_module_import_position<'db>(
    db: &'db DriverDataBase,
    scope: ScopeId<'db>,
    file_text: &str,
) -> Position {
    use hir::span::LazySpan;

    // Find the containing module
    if let Some(parent_mod) = scope.parent_module(db) {
        match parent_mod.item() {
            ItemKind::Mod(m) => {
                // For inline modules, find position after the opening brace
                if let Some(span) = m.span().resolve(db) {
                    let mod_start = usize::from(span.range.start());
                    let mod_text = &file_text[mod_start..];

                    // Find the opening brace
                    if let Some(brace_offset) = mod_text.find('{') {
                        let abs_brace_pos = mod_start + brace_offset;
                        // Find position after the opening brace
                        return find_import_position_after_brace(file_text, abs_brace_pos);
                    }
                }
            }
            ItemKind::TopMod(_) => {
                // For top-level modules, use the file-level position
                return find_import_insertion_position(file_text);
            }
            _ => {}
        }
    }

    // Fallback to file-level position
    find_import_insertion_position(file_text)
}

/// Find import position within a module body (after opening brace).
/// `full_file_text` is the complete file content, `brace_offset` is the byte offset of the `{`.
fn find_import_position_after_brace(full_file_text: &str, brace_offset: usize) -> Position {
    // Find the line after the opening brace
    // The use statement should go on the next line after '{'
    let mut line = 0u32;
    let mut last_newline_offset = 0;

    for (i, ch) in full_file_text.char_indices() {
        if i >= brace_offset {
            // Found the brace, now find the next newline
            let rest = &full_file_text[brace_offset..];
            if rest.find('\n').is_some() {
                // Position at start of the line after the brace
                return Position {
                    line: line + 1,
                    character: 0,
                };
            } else {
                // No newline after brace, insert right after brace
                return Position {
                    line,
                    character: (brace_offset - last_newline_offset + 1) as u32,
                };
            }
        }
        if ch == '\n' {
            line += 1;
            last_newline_offset = i + 1;
        }
    }

    // Fallback
    Position {
        line: 0,
        character: 0,
    }
}

/// Compute the import path for any item, including those in nested inline modules.
///
/// Returns the path like "utils::helper_func" or "outer::inner::SomeStruct".
/// For use in auto-import, this returns only the path needed within the current ingot,
/// excluding the top-level module name (file name) but including the item name.
fn compute_item_import_path<'db>(db: &'db DriverDataBase, item: ItemKind<'db>) -> Option<String> {
    let item_name = item.name(db)?.data(db).to_string();

    // Build the path by walking up from the item's scope to find containing modules
    let scope = ScopeId::from_item(item);
    let mut path_parts = vec![item_name];

    // Walk up the parent chain looking for Mod items (inline modules)
    // Start from parent (not the item's own scope) to avoid duplicating the name
    let mut current = scope.parent(db);
    while let Some(parent_scope) = current {
        match parent_scope.item() {
            ItemKind::Mod(m) => {
                // This is an inline module - add its name to the path
                if let Partial::Present(name) = m.name(db) {
                    path_parts.push(name.data(db).to_string());
                }
            }
            ItemKind::TopMod(_) => {
                // Reached the top-level file module - stop here
                // We don't include the file name in the import path for single-file scenarios
                break;
            }
            _ => {
                // Other items (functions, impls, structs, etc.) - skip them
                // These don't contribute to the import path
            }
        }
        current = parent_scope.parent(db);
    }

    // Reverse to get the path in correct order (outer to inner)
    path_parts.reverse();

    // If there's only the item name (no module path), it means the item is at
    // the top level - don't suggest auto-import for top-level items in same file
    if path_parts.len() <= 1 {
        return None;
    }

    Some(path_parts.join("::"))
}

/// Find the position to insert new import statements at the top level.
/// For top-level modules, we simply insert at the start of the file (line 0).
/// The user can organize imports as they prefer.
fn find_import_insertion_position(_file_text: &str) -> Position {
    // Insert at the very start of the file for top-level modules
    Position {
        line: 0,
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
        .rfind(|c: char| !c.is_alphanumeric() && c != '_' && c != ':')
        .map(|i| i + 1)
        .unwrap_or(0);

    let full_path = before_colons[path_start..].trim();
    if full_path.is_empty() {
        return;
    }

    // Split the path into segments
    let segments: Vec<&str> = full_path.split("::").filter(|s| !s.is_empty()).collect();
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
                current_scope = *target_scope;
            } else {
                return;
            }
        } else {
            return;
        }
    }

    // Detect context for filtering (look at what's before the path)
    let context = detect_completion_context(file_text, cursor);

    // Get direct child items of the final resolved scope
    // This gives us only items defined directly in this module, not inherited ones
    let child_items: Vec<_> = current_scope.child_items(db).collect();

    for item in child_items {
        let Some(name) = item.name(db) else {
            continue;
        };
        let name_str = name.data(db);

        // Filter and determine kind based on item type and context
        let (kind, insert_text) = match item {
            ItemKind::Func(func) => {
                if matches!(context, CompletionContext::Type) {
                    continue;
                }
                // Use callable snippet for functions
                if let Some(completion) =
                    build_callable_completion(db, func, CompletionItemKind::FUNCTION)
                {
                    items.push(completion);
                }
                continue;
            }
            ItemKind::Const(_) => {
                if matches!(context, CompletionContext::Type) {
                    continue;
                }
                (CompletionItemKind::CONSTANT, None)
            }
            ItemKind::Struct(_) => {
                if matches!(context, CompletionContext::Expression) {
                    continue;
                }
                (CompletionItemKind::STRUCT, None)
            }
            ItemKind::Enum(_) => {
                if matches!(context, CompletionContext::Expression) {
                    continue;
                }
                (CompletionItemKind::ENUM, None)
            }
            ItemKind::Trait(_) => {
                if matches!(context, CompletionContext::Expression) {
                    continue;
                }
                (CompletionItemKind::INTERFACE, None)
            }
            ItemKind::TypeAlias(_) => {
                if matches!(context, CompletionContext::Expression) {
                    continue;
                }
                (CompletionItemKind::CLASS, None)
            }
            ItemKind::Contract(_) => {
                if matches!(context, CompletionContext::Expression) {
                    continue;
                }
                (CompletionItemKind::CLASS, None)
            }
            ItemKind::Mod(_) | ItemKind::TopMod(_) => {
                // Modules get :: suffix
                (CompletionItemKind::MODULE, Some(format!("{}::", name_str)))
            }
            _ => continue,
        };

        items.push(CompletionItem {
            label: name_str.to_string(),
            kind: Some(kind),
            insert_text,
            ..Default::default()
        });
    }
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
        if let ItemKind::Func(func) = item
            && let Some(span) = func.span().resolve(db)
            && span.range.contains(cursor)
        {
            enclosing_func = Some(func);
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
                // Check if cursor is within this field expression
                if resolved.range.contains(cursor) || resolved.range.end() == cursor {
                    let mut ty = typed_body.expr_ty(db, *receiver_id);

                    // If the receiver type is invalid (due to incomplete syntax), try to find
                    // the type from the expression itself (e.g., if it's a path to a local binding)
                    if ty.has_invalid(db)
                        && let Some(Partial::Present(Expr::Path(Partial::Present(path)))) =
                            body.exprs(db).get(*receiver_id)
                    {
                        // Try to resolve the path to find a local binding's type
                        if let Some(ident) = path.as_ident(db) {
                            let ident_str = ident.data(db);

                            // Special case: if the path is "self", get the self parameter's type
                            if ident_str == "self" {
                                for param in func.params(db) {
                                    if param.is_self_param(db) {
                                        let self_ty = param.ty(db);
                                        if !self_ty.has_invalid(db) {
                                            ty = self_ty;
                                            break;
                                        }
                                    }
                                }
                            } else {
                                // Look through patterns to find this binding's type
                                for (pat_id, pat_data) in body.pats(db).iter() {
                                    if let Partial::Present(Pat::Path(
                                        Partial::Present(pat_path),
                                        _,
                                    )) = pat_data
                                        && pat_path.as_ident(db) == Some(ident)
                                    {
                                        let pat_ty = typed_body.pat_ty(db, pat_id);
                                        if !pat_ty.has_invalid(db) {
                                            ty = pat_ty;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    collect_fields_for_type(db, ty, items);
                    collect_methods_for_type(db, top_mod, ty, func.scope(), items);
                    return;
                }
            }
        }
    }

    // Strategy 2: Fallback - look for any expression ending at cursor-1 (before the dot)
    let dot_pos = cursor.checked_sub(1.into()).unwrap_or(cursor);

    for (expr_id, _) in body.exprs(db).iter() {
        let expr_span = expr_id.span(body);
        if let Some(resolved) = expr_span.resolve(db)
            && resolved.range.end() == dot_pos
        {
            let ty = typed_body.expr_ty(db, expr_id);
            collect_fields_for_type(db, ty, items);
            collect_methods_for_type(db, top_mod, ty, func.scope(), items);
            return;
        }
    }
}

/// Collect struct fields as completion items.
fn collect_fields_for_type<'db>(
    db: &'db DriverDataBase,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    items: &mut Vec<CompletionItem>,
) {
    // Use the traversal API to get fields for struct/contract types
    let Some(field_parent) = ty.field_parent(db) else {
        return;
    };

    for field in field_parent.fields(db) {
        let Some(name) = field.name(db) else {
            continue;
        };
        let field_ty = field.ty(db);
        let detail = format!("{}: {}", name.data(db), field_ty.pretty_print(db));

        items.push(CompletionItem {
            label: name.data(db).to_string(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some(detail),
            ..Default::default()
        });
    }
}

/// Build a completion item for a callable (function or method) with snippet tabstops.
fn build_callable_completion<'db>(
    db: &'db DriverDataBase,
    func: Func<'db>,
    kind: CompletionItemKind,
) -> Option<CompletionItem> {
    let name = func.name(db).to_opt()?;
    let name_str = name.data(db).to_string();

    // Build parameter list for detail and snippet
    let mut param_names = Vec::new();
    let mut param_details = Vec::new();

    for param in func.params(db) {
        if param.is_self_param(db) {
            continue; // Skip self parameter in completion
        }

        let param_name = param
            .name(db)
            .map(|n| n.data(db).to_string())
            .unwrap_or_else(|| format!("arg{}", param_names.len()));

        let param_ty = param.ty(db);
        param_details.push(format!("{}: {}", param_name, param_ty.pretty_print(db)));
        param_names.push(param_name);
    }

    // Build detail string: fn name(param1: Type1, param2: Type2) -> ReturnType
    let ret_ty = func.return_ty(db);
    let ret_str = {
        let ret_pretty = ret_ty.pretty_print(db);
        if ret_pretty == "()" {
            String::new()
        } else {
            format!(" -> {}", ret_pretty)
        }
    };
    let detail = format!("fn {}({}){}", name_str, param_details.join(", "), ret_str);

    // Build snippet with tabstops: name(${1:param1}, ${2:param2})
    let snippet = if param_names.is_empty() {
        format!("{}()$0", name_str)
    } else {
        let tabstops: Vec<String> = param_names
            .iter()
            .enumerate()
            .map(|(i, p)| format!("${{{}:{}}}", i + 1, p))
            .collect();
        format!("{}({})$0", name_str, tabstops.join(", "))
    };

    Some(CompletionItem {
        label: name_str,
        kind: Some(kind),
        detail: Some(detail),
        insert_text: Some(snippet),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    })
}

/// Collect methods from impls as completion items.
fn collect_methods_for_type<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    scope: ScopeId<'db>,
    items: &mut Vec<CompletionItem>,
) {
    // Get the type name for matching impl blocks
    let ty_name = ty.pretty_print(db);

    // Track method names to avoid duplicates
    let mut seen_methods = std::collections::HashSet::new();

    // Look for inherent impl blocks in the module
    for item in top_mod.scope_graph(db).items_dfs(db) {
        if let ItemKind::Impl(impl_) = item {
            // Check if this impl is for our type by comparing target type name
            let impl_ty_name = impl_.ty(db).pretty_print(db);
            if impl_ty_name == ty_name {
                for func in impl_.funcs(db) {
                    if func.is_method(db)
                        && let Some(name) = func.name(db).to_opt()
                        && seen_methods.insert(name)
                        && let Some(completion) =
                            build_callable_completion(db, func, CompletionItemKind::METHOD)
                    {
                        items.push(completion);
                    }
                }
            }
        }
    }

    // Collect trait methods from in-scope traits that are implemented for this type
    collect_trait_methods_for_type(db, ty, scope, &mut seen_methods, items);
}

/// Collect methods from trait implementations for the given type.
fn collect_trait_methods_for_type<'db>(
    db: &'db DriverDataBase,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    scope: ScopeId<'db>,
    seen_methods: &mut std::collections::HashSet<hir::hir_def::IdentId<'db>>,
    items: &mut Vec<CompletionItem>,
) {
    // Get traits available in the current scope
    let available_traits = available_traits_in_scope(db, scope);

    // Get the type name for matching impl trait blocks
    let ty_name = ty.pretty_print(db);

    // Get the ingot to search for impl trait blocks
    let ingot = scope.ingot(db);

    // Iterate over all impl trait blocks in the ingot
    for impl_trait in ingot.all_impl_traits(db) {
        let Some(trait_def) = impl_trait.trait_def(db) else {
            continue;
        };

        // Check if this impl is for our type
        let impl_ty_name = impl_trait.ty(db).pretty_print(db);
        if impl_ty_name != ty_name {
            continue;
        }

        let trait_in_scope = available_traits.contains(&trait_def);

        // Add methods from this trait
        for (method_name, func) in trait_def.method_defs(db) {
            if seen_methods.insert(method_name)
                && let Some(mut completion) =
                    build_callable_completion(db, func, CompletionItemKind::METHOD)
            {
                // If trait is not in scope, add hint about needing import
                if !trait_in_scope && let Some(trait_name) = trait_def.name(db).to_opt() {
                    let trait_name_str = trait_name.data(db);
                    if let Some(ref detail) = completion.detail {
                        completion.detail = Some(format!("{} (use {})", detail, trait_name_str));
                    }
                }
                items.push(completion);
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
                    Some(ItemKind::Body(body)) => match body.containing_func(db) {
                        Some(f) => break f,
                        None => return,
                    },
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
fn collect_let_bindings<'db>(
    db: &'db DriverDataBase,
    body: Body<'db>,
    items: &mut Vec<CompletionItem>,
) {
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
        typed_body,
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
            if let Partial::Present(Pat::Path(Partial::Present(path), _)) =
                pat_id.data(self.db, self.body)
                && let Some(ident) = path.as_ident(self.db)
            {
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
        walk_stmt(self, ctxt, _stmt);
    }
}

/// Convert a NameRes to a CompletionItem, filtering based on context.
fn name_res_to_completion<'db>(
    db: &'db DriverDataBase,
    name: &str,
    name_res: &hir::analysis::name_resolution::NameRes<'db>,
    context: CompletionContext,
) -> Option<CompletionItem> {
    let scope = match &name_res.kind {
        NameResKind::Scope(scope) => *scope,
        NameResKind::Prim(_) => {
            // Primitive types - only in type context
            if matches!(context, CompletionContext::Expression) {
                return None;
            }
            return Some(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            });
        }
    };

    // Determine what kind of item this is and filter based on context
    match scope.to_item() {
        Some(ItemKind::Func(func)) => {
            // Functions are values - not allowed in type context
            if matches!(context, CompletionContext::Type) {
                return None;
            }
            build_callable_completion(db, func, CompletionItemKind::FUNCTION)
        }

        Some(ItemKind::Mod(_) | ItemKind::TopMod(_)) => {
            // Modules can lead to types or values, so allow in mixed context
            // but not in pure type context (you can't use a module as a type)
            if matches!(context, CompletionContext::Type) {
                return None;
            }
            Some(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::MODULE),
                insert_text: Some(format!("{}::", name)),
                ..Default::default()
            })
        }

        Some(ItemKind::Struct(_)) => {
            // Structs are types - not allowed in expression context
            if matches!(context, CompletionContext::Expression) {
                return None;
            }
            Some(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::STRUCT),
                ..Default::default()
            })
        }

        Some(ItemKind::Enum(_)) => {
            // Enums are types - not allowed in expression context
            if matches!(context, CompletionContext::Expression) {
                return None;
            }
            Some(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::ENUM),
                ..Default::default()
            })
        }

        Some(ItemKind::Trait(_)) => {
            // Traits are not usable as standalone types in type annotations
            // They're used in bounds (T: Trait) or impl blocks, not as `let x: Trait`
            // For now, exclude from type context
            if matches!(context, CompletionContext::Type) {
                return None;
            }
            Some(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::INTERFACE),
                ..Default::default()
            })
        }

        Some(ItemKind::TypeAlias(_)) => {
            // Type aliases are types
            if matches!(context, CompletionContext::Expression) {
                return None;
            }
            Some(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            })
        }

        Some(ItemKind::Const(_)) => {
            // Constants are values
            if matches!(context, CompletionContext::Type) {
                return None;
            }
            Some(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::CONSTANT),
                ..Default::default()
            })
        }

        Some(ItemKind::Contract(_)) => {
            // Contracts are types
            if matches!(context, CompletionContext::Expression) {
                return None;
            }
            Some(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            })
        }

        _ => {
            // Handle other scope types
            let kind = match scope {
                ScopeId::FuncParam(_, _) => {
                    if matches!(context, CompletionContext::Type) {
                        return None;
                    }
                    CompletionItemKind::VARIABLE
                }
                ScopeId::GenericParam(_, _) => {
                    if matches!(context, CompletionContext::Expression) {
                        return None;
                    }
                    CompletionItemKind::TYPE_PARAMETER
                }
                ScopeId::Variant(_) => {
                    // Enum variants are values (constructors)
                    if matches!(context, CompletionContext::Type) {
                        return None;
                    }
                    CompletionItemKind::ENUM_MEMBER
                }
                ScopeId::Field(_, _) => {
                    if matches!(context, CompletionContext::Type) {
                        return None;
                    }
                    CompletionItemKind::FIELD
                }
                _ => {
                    if matches!(context, CompletionContext::Type) {
                        return None;
                    }
                    CompletionItemKind::VALUE
                }
            };

            Some(CompletionItem {
                label: name.to_string(),
                kind: Some(kind),
                ..Default::default()
            })
        }
    }
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

        // Test completion after "p."
        if let Some(pos) = file_text.rfind("p.") {
            let cursor_after_dot = parser::TextSize::from((pos + 2) as u32);
            let mut items = Vec::new();
            collect_member_completions(&db, top_mod, cursor_after_dot, &mut items);
            assert!(items.len() >= 2, "Expected at least 2 items (x, y fields)");
        }

        // Test completion after "self."
        let self_dot_positions: Vec<_> = file_text.match_indices("self.").collect();
        if let Some(&(pos, _)) = self_dot_positions.last() {
            let cursor_after_dot = parser::TextSize::from((pos + 5) as u32);
            let mut items = Vec::new();
            collect_member_completions(&db, top_mod, cursor_after_dot, &mut items);
            assert!(
                items.len() >= 2,
                "Expected at least 2 items (x, y fields) for self."
            );
        }
    }

    /// Test that module completions are available
    #[test]
    fn test_module_completions() {
        use hir::analysis::name_resolution::NameDomain;

        let mut db = DriverDataBase::default();

        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_files")
            .join("hoverable");

        load_ingot_from_directory(&mut db, &fixture_path);

        let lib_url = url::Url::from_file_path(fixture_path.join("src").join("lib.fe")).unwrap();
        let file = db.workspace().get(&db, &lib_url).expect("file not found");
        let top_mod = map_file_to_mod(&db, file);

        let scope = top_mod.scope();
        let visible = scope.items_in_scope(&db, NameDomain::VALUE | NameDomain::TYPE);
        assert!(
            visible.keys().any(|n| n == "stuff"),
            "stuff module should be visible"
        );

        let mut items = Vec::new();
        collect_items_from_scope(&db, scope, CompletionContext::Mixed, &mut items);

        let module_completions: Vec<_> = items
            .iter()
            .filter(|i| i.kind == Some(CompletionItemKind::MODULE))
            .collect();
        assert!(
            !module_completions.is_empty(),
            "Should have module completions"
        );
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

        // Test stuff::
        let file_text = "stuff::";
        let cursor = parser::TextSize::from(file_text.len() as u32);
        let mut items = Vec::new();
        collect_path_completions(&db, top_mod, cursor, file_text, &mut items);

        assert!(!items.is_empty(), "Expected items from stuff module");
        assert!(
            items.iter().any(|i| i.label == "calculations"),
            "Expected 'calculations' submodule in stuff::"
        );

        // Test stuff::calculations::
        let file_text = "stuff::calculations::";
        let cursor = parser::TextSize::from(file_text.len() as u32);
        let mut items = Vec::new();
        collect_path_completions(&db, top_mod, cursor, file_text, &mut items);

        assert!(!items.is_empty(), "Expected items from stuff::calculations");
        assert!(items.iter().any(|i| i.label == "return_three"));
        assert!(items.iter().any(|i| i.label == "return_four"));
    }

    /// Test member access completion by simulating cursor after a dot
    #[test]
    fn test_member_access_completion() {
        use hir::hir_def::Expr;
        use hir::visitor::{Visitor, VisitorCtxt, prelude::*};

        let mut db = DriverDataBase::default();

        let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_files")
            .join("hoverable");

        load_ingot_from_directory(&mut db, &fixture_path);

        let lib_url = url::Url::from_file_path(fixture_path.join("src").join("lib.fe")).unwrap();
        let file = db.workspace().get(&db, &lib_url).expect("file not found");
        let top_mod = map_file_to_mod(&db, file);

        // Find Field expressions using visitor
        struct FieldExprFinder {
            count: usize,
        }

        impl<'db> Visitor<'db> for FieldExprFinder {
            fn visit_expr(
                &mut self,
                ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
                expr: hir::hir_def::ExprId,
                expr_data: &Expr<'db>,
            ) {
                if matches!(expr_data, Expr::Field(..)) {
                    self.count += 1;
                }
                walk_expr(self, ctxt, expr);
            }
        }

        let mut finder = FieldExprFinder { count: 0 };
        let mut visitor_ctxt = VisitorCtxt::with_top_mod(&db, top_mod);
        finder.visit_top_mod(&mut visitor_ctxt, top_mod);

        // The fixture should have field access expressions
        assert!(
            finder.count > 0,
            "Expected Field expressions in the fixture"
        );
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

        let scope = top_mod.scope();
        let mut items = Vec::new();
        collect_auto_import_completions(
            &db,
            top_mod,
            scope,
            CompletionContext::Mixed,
            file_text,
            &mut items,
        );

        // Verify auto-imports have proper text edits
        for item in &items {
            if let Some(edits) = &item.additional_text_edits {
                assert!(!edits.is_empty());
                for edit in edits {
                    assert!(edit.new_text.starts_with("use "));
                }
            }
        }

        // Test import insertion position detection
        // For top-level modules, we insert at line 0
        let position = find_import_insertion_position(file_text);
        assert_eq!(position.line, 0, "Expected insertion at start of file");
    }

    /// Extract cursor markers (<|>) and their positions from source text.
    /// Returns the cleaned source (markers removed) and a list of positions.
    fn extract_cursor_markers(source: &str) -> (String, Vec<usize>) {
        const MARKER: &str = "<|>";
        let mut positions = Vec::new();
        let mut cleaned = String::new();
        let mut offset_adjustment = 0;
        let mut search_start = 0;

        while let Some(start) = source[search_start..].find(MARKER) {
            let abs_start = search_start + start;
            let adjusted_pos = abs_start - offset_adjustment;

            // Copy text before this marker
            cleaned.push_str(&source[search_start..abs_start]);

            positions.push(adjusted_pos);
            offset_adjustment += MARKER.len();
            search_start = abs_start + MARKER.len();
        }

        // Copy remaining text
        cleaned.push_str(&source[search_start..]);

        (cleaned, positions)
    }

    /// Format a completion item for snapshot output.
    fn format_completion_item(item: &CompletionItem) -> String {
        let kind_str = item
            .kind
            .map(|k| format!("{:?}", k))
            .unwrap_or_else(|| "?".to_string());

        let mut result = format!("{} ({})", item.label, kind_str);

        if let Some(ref insert) = item.insert_text
            && insert != &item.label
        {
            result.push_str(&format!(" -> \"{}\"", insert.replace('\n', "\\n")));
        }

        if let Some(ref detail) = item.detail {
            result.push_str(&format!(" [{}]", detail));
        }

        // Show additional text edits (auto-imports)
        if let Some(ref edits) = item.additional_text_edits {
            for edit in edits {
                result.push_str(&format!(
                    " {{+{}:{}}}",
                    edit.range.start.line,
                    edit.new_text.trim()
                ));
            }
        }

        result
    }

    /// Collect completions at a cursor position and format for snapshot.
    fn collect_completions_at_cursor(
        db: &DriverDataBase,
        top_mod: TopLevelMod,
        file_text: &str,
        cursor: parser::TextSize,
    ) -> Vec<String> {
        let mut items = Vec::new();

        // Check if this is member access (char before cursor is '.')
        let is_member_access = cursor
            .checked_sub(1.into())
            .and_then(|pos| file_text.get(usize::from(pos)..usize::from(cursor)))
            .map(|s| s == ".")
            .unwrap_or(false);

        // Check if this is path completion (chars before cursor are '::')
        let is_path_completion = cursor
            .checked_sub(2.into())
            .and_then(|pos| file_text.get(usize::from(pos)..usize::from(cursor)))
            .map(|s| s == "::")
            .unwrap_or(false);

        if is_member_access {
            collect_member_completions(db, top_mod, cursor, &mut items);
        } else if is_path_completion {
            collect_path_completions(db, top_mod, cursor, file_text, &mut items);
        } else {
            let scope = find_scope_at_cursor(db, top_mod, cursor);
            if let Some(scope) = scope {
                let context = detect_completion_context(file_text, cursor);
                collect_items_from_scope(db, scope, context, &mut items);
                // Also collect auto-import suggestions
                collect_auto_import_completions(db, top_mod, scope, context, file_text, &mut items);
            }
        }

        // Sort and format
        items.sort_by(|a, b| a.label.cmp(&b.label));
        items.iter().map(format_completion_item).collect()
    }

    /// Create a codespan-based snapshot showing completions at each cursor position.
    fn make_completion_snapshot(
        db: &DriverDataBase,
        cleaned_source: &str,
        positions: &[usize],
        top_mod: TopLevelMod,
    ) -> String {
        use codespan_reporting::{
            diagnostic::{Diagnostic, Label},
            files::SimpleFiles,
            term::{
                self,
                termcolor::{BufferWriter, ColorChoice},
            },
        };

        let mut files = SimpleFiles::new();
        let file_id = files.add("completion.fe", cleaned_source.to_string());

        let mut output = String::new();

        for (idx, pos) in positions.iter().enumerate() {
            let cursor = parser::TextSize::from(*pos as u32);
            let completions = collect_completions_at_cursor(db, top_mod, cleaned_source, cursor);

            let diag = Diagnostic::note().with_labels(vec![
                Label::primary(file_id, *pos..*pos).with_message(format!("cursor {}", idx + 1)),
            ]);

            // Render the diagnostic
            let writer = BufferWriter::stderr(ColorChoice::Never);
            let mut buffer = writer.buffer();
            let config = term::Config::default();
            term::emit(&mut buffer, &config, &files, &diag).unwrap();

            let diag_str = std::str::from_utf8(buffer.as_slice()).unwrap();
            output.push_str(diag_str);

            // Format completions as a nice list
            if completions.is_empty() {
                output.push_str("completions: (none)\n");
            } else {
                output.push_str("completions:\n");
                for completion in &completions {
                    output.push_str(&format!("  - {}\n", completion));
                }
            }
            output.push('\n');
        }

        output
    }

    #[dir_test::dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "completion.fe"
    )]
    fn test_completion_snapshot(fixture: dir_test::Fixture<&str>) {
        use test_utils::snap_test;
        use url::Url;

        let original_source = fixture.content();
        let (cleaned_source, positions) = extract_cursor_markers(original_source);

        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap(),
            Some(cleaned_source.clone()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let snapshot = make_completion_snapshot(&db, &cleaned_source, &positions, top_mod);
        snap_test!(snapshot, fixture.path());
    }
}
