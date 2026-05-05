use std::collections::HashMap;

use common::InputDb;
use common::diagnostics::Span;
use common::semantic_index::{IndexLocation, IndexReference, IndexSymbol};
use hir::{
    core::semantic::SymbolView,
    hir_def::{ItemKind, scope_graph::ScopeId},
    span::LazySpan,
};
use scip::types::{descriptor, symbol_information};

use crate::index_util::{self, LineIndex};

/// Result of indexing a single module.
pub struct ModuleIndexResult {
    pub symbols: Vec<(String, IndexSymbol)>,
    pub references: Vec<(String, Vec<IndexReference>)>,
}

/// Index a single top-level module, producing entries for the SemanticIndex trie.
pub fn index_module<'db>(
    db: &'db driver::DriverDataBase,
    top_mod: hir::hir_def::TopLevelMod<'db>,
    ctx: &index_util::IngotContext<'db>,
    file_relative_paths: &HashMap<String, String>,
) -> Option<ModuleIndexResult> {
    use hir::core::semantic::{SymbolKind, scope_to_doc_path};

    let scope_graph = top_mod.scope_graph(db);
    let doc_url = top_mod_url(db, &top_mod)?.to_string();

    let mut symbols: Vec<(String, IndexSymbol)> = Vec::new();
    let mut refs_by_symbol: HashMap<String, Vec<IndexReference>> = HashMap::new();
    let mut line_index_cache: HashMap<common::file::File, LineIndex> = HashMap::new();

    for item in scope_graph.items_dfs(db) {
        let scope = ScopeId::from_item(item);
        if let Some(parent) = scope.parent_item(db)
            && index_util::is_container_item(parent)
        {
            continue;
        }

        let maybe_symbol = item_symbol(db, item, &ctx.name, &ctx.version);

        let Some((ref symbol_str, ref display_name)) = maybe_symbol else {
            index_unnamed_generic_params(
                db,
                item,
                ctx,
                &doc_url,
                file_relative_paths,
                &mut symbols,
                &mut refs_by_symbol,
                &mut line_index_cache,
            );
            continue;
        };

        let item_doc_url = scope_to_doc_path(db, scope);
        let kind = item_symbol_kind(item) as i32;
        let documentation = index_util::hover_parts(db, item).to_scip_documentation();

        let def_location = item
            .name_span()
            .and_then(|span| span.resolve(db))
            .and_then(|name_span| {
                span_to_location(&name_span, db, file_relative_paths, &mut line_index_cache)
            })
            .unwrap_or_else(|| IndexLocation {
                relative_path: file_relative_paths
                    .get(&doc_url)
                    .cloned()
                    .unwrap_or_default(),
                file_url: doc_url.clone(),
                range: vec![0, 0, 0],
            });

        symbols.push((
            symbol_str.clone(),
            IndexSymbol {
                symbol: symbol_str.clone(),
                display_name: display_name.clone(),
                kind,
                documentation,
                enclosing_symbol: String::new(),
                def_location,
                doc_url: item_doc_url.clone(),
            },
        ));

        // Reference occurrences
        let scope = ScopeId::from_item(item);
        for indexed_ref in ctx.ref_index.references_to(&scope) {
            if let Some(resolved) = indexed_ref.span.resolve(db) {
                if let Some(loc) =
                    span_to_location(&resolved, db, file_relative_paths, &mut line_index_cache)
                {
                    refs_by_symbol
                        .entry(symbol_str.clone())
                        .or_default()
                        .push(IndexReference {
                            symbol: symbol_str.clone(),
                            location: loc,
                            role: 0,
                        });
                }
            }
        }

        // Child items (fields, variants, methods)
        if !matches!(item, ItemKind::Mod(_) | ItemKind::TopMod(_)) {
            let sym_view = SymbolView::from_item(item);
            for child in sym_view.children(db) {
                let child_scope = child.scope();
                let Some(child_name) = child.name(db) else {
                    continue;
                };

                let child_symbol = child_scip_symbol(symbol_str, &child_name, child_scope);
                let child_kind = child_symbol_kind(child_scope) as i32;
                let child_docs = child.docs(db).map(|d| vec![d]).unwrap_or_default();

                let child_sym_kind = SymbolKind::from(child_scope);
                let child_doc_url = if let (Some(parent_url), Some(anchor)) =
                    (&item_doc_url, child_sym_kind.doc_anchor_prefix())
                {
                    Some(format!("{}~{}.{}", parent_url, anchor, child_name))
                } else {
                    None
                };

                if let Some(name_span) = child.name_span(db) {
                    if let Some(loc) = span_to_location(
                        &name_span,
                        db,
                        file_relative_paths,
                        &mut line_index_cache,
                    ) {
                        symbols.push((
                            child_symbol.clone(),
                            IndexSymbol {
                                symbol: child_symbol.clone(),
                                display_name: child_name.clone(),
                                kind: child_kind,
                                documentation: child_docs,
                                enclosing_symbol: symbol_str.clone(),
                                def_location: loc,
                                doc_url: child_doc_url,
                            },
                        ));
                    }
                }

                for indexed_ref in ctx.ref_index.references_to(&child_scope) {
                    if let Some(resolved) = indexed_ref.span.resolve(db) {
                        if let Some(loc) = span_to_location(
                            &resolved,
                            db,
                            file_relative_paths,
                            &mut line_index_cache,
                        ) {
                            refs_by_symbol
                                .entry(child_symbol.clone())
                                .or_default()
                                .push(IndexReference {
                                    symbol: child_symbol.clone(),
                                    location: loc,
                                    role: 0,
                                });
                        }
                    }
                }

                // Generic params of child items
                let child_view = SymbolView::new(child_scope);
                index_generic_params(
                    db,
                    &child_view,
                    &child_symbol,
                    ctx,
                    &doc_url,
                    file_relative_paths,
                    &mut symbols,
                    &mut refs_by_symbol,
                    &mut line_index_cache,
                );
            }
        }

        // Generic params of the item itself
        let sym_view = SymbolView::from_item(item);
        index_generic_params(
            db,
            &sym_view,
            symbol_str,
            ctx,
            &doc_url,
            file_relative_paths,
            &mut symbols,
            &mut refs_by_symbol,
            &mut line_index_cache,
        );
    }

    let references: Vec<_> = refs_by_symbol.into_iter().collect();
    Some(ModuleIndexResult {
        symbols,
        references,
    })
}

// --- Helper functions (shared with scip_index.rs) ---

fn top_mod_url(
    db: &driver::DriverDataBase,
    top_mod: &hir::hir_def::TopLevelMod<'_>,
) -> Option<url::Url> {
    top_mod.span().resolve(db)?.file.url(db)
}

fn span_to_location(
    span: &Span,
    db: &dyn InputDb,
    file_relative_paths: &HashMap<String, String>,
    line_index_cache: &mut HashMap<common::file::File, LineIndex>,
) -> Option<IndexLocation> {
    let file_url = span.file.url(db)?.to_string();
    let relative_path = file_relative_paths
        .get(&file_url)
        .cloned()
        .unwrap_or_default();

    let line_index = line_index_cache
        .entry(span.file)
        .or_insert_with(|| LineIndex::new(span.file.text(db)));

    let start = line_index.position(span.range.start().into());
    let end = line_index.position(span.range.end().into());

    let start_col = start.byte_offset.checked_sub(start.line_start_offset)?;
    let end_col = end.byte_offset.checked_sub(end.line_start_offset)?;

    let range = if start.line == end.line {
        vec![start.line as i32, start_col as i32, end_col as i32]
    } else {
        vec![
            start.line as i32,
            start_col as i32,
            end.line as i32,
            end_col as i32,
        ]
    };

    Some(IndexLocation {
        relative_path,
        file_url,
        range,
    })
}

fn item_symbol_kind(item: ItemKind<'_>) -> symbol_information::Kind {
    match item {
        ItemKind::Struct(_) => symbol_information::Kind::Struct,
        ItemKind::Enum(_) => symbol_information::Kind::Enum,
        ItemKind::Trait(_) => symbol_information::Kind::Trait,
        ItemKind::Func(_) => symbol_information::Kind::Function,
        _ => symbol_information::Kind::UnspecifiedKind,
    }
}

fn child_symbol_kind(scope: ScopeId<'_>) -> symbol_information::Kind {
    use hir::core::semantic::SymbolKind;
    match SymbolKind::from(scope) {
        SymbolKind::Field => symbol_information::Kind::Field,
        SymbolKind::Variant => symbol_information::Kind::EnumMember,
        SymbolKind::Func => symbol_information::Kind::Method,
        SymbolKind::TraitType => symbol_information::Kind::TypeAlias,
        SymbolKind::TraitConst => symbol_information::Kind::Constant,
        _ => symbol_information::Kind::UnspecifiedKind,
    }
}

fn item_descriptor_suffix(item: ItemKind<'_>) -> descriptor::Suffix {
    match item {
        ItemKind::Struct(_) | ItemKind::Enum(_) | ItemKind::Trait(_) | ItemKind::Contract(_) => {
            descriptor::Suffix::Type
        }
        ItemKind::Func(_) => descriptor::Suffix::Term,
        _ => descriptor::Suffix::Meta,
    }
}

fn child_descriptor_suffix(scope: ScopeId<'_>) -> descriptor::Suffix {
    use hir::core::semantic::SymbolKind;
    match SymbolKind::from(scope) {
        SymbolKind::Field | SymbolKind::Variant | SymbolKind::Func => descriptor::Suffix::Term,
        SymbolKind::TraitType => descriptor::Suffix::Type,
        _ => descriptor::Suffix::Meta,
    }
}

fn child_scip_symbol(parent_symbol: &str, child_name: &str, scope: ScopeId<'_>) -> String {
    let suffix = child_descriptor_suffix(scope);
    let suffix_char = match suffix {
        descriptor::Suffix::Type => "#",
        descriptor::Suffix::Term => ".",
        descriptor::Suffix::Meta => ":",
        _ => ".",
    };
    format!("{parent_symbol}{child_name}{suffix_char}")
}

fn item_symbol<'db>(
    db: &driver::DriverDataBase,
    item: ItemKind<'db>,
    package_name: &str,
    package_version: &str,
) -> Option<(String, String)> {
    use scip::symbol::format_symbol;
    use scip::types;

    let pretty_path = ScopeId::from_item(item).pretty_path(db)?.to_string();
    let mut descriptors = Vec::new();
    let mut parts = pretty_path.split("::").peekable();
    while let Some(part) = parts.next() {
        let suffix = if parts.peek().is_some() {
            descriptor::Suffix::Namespace
        } else {
            item_descriptor_suffix(item)
        };
        descriptors.push(types::Descriptor {
            name: part.to_string(),
            disambiguator: String::new(),
            suffix: suffix.into(),
            special_fields: Default::default(),
        });
    }

    let symbol = types::Symbol {
        scheme: "fe".to_string(),
        package: Some(types::Package {
            manager: "fe".to_string(),
            name: package_name.to_string(),
            version: package_version.to_string(),
            special_fields: Default::default(),
        })
        .into(),
        descriptors,
        special_fields: Default::default(),
    };
    let display_name = pretty_path
        .rsplit("::")
        .next()
        .unwrap_or(pretty_path.as_str())
        .to_string();
    Some((format_symbol(symbol), display_name))
}

fn index_generic_params<'db>(
    db: &'db driver::DriverDataBase,
    sym_view: &SymbolView<'db>,
    parent_symbol: &str,
    ctx: &index_util::IngotContext<'db>,
    _doc_url: &str,
    file_relative_paths: &HashMap<String, String>,
    symbols: &mut Vec<(String, IndexSymbol)>,
    refs_by_symbol: &mut HashMap<String, Vec<IndexReference>>,
    line_index_cache: &mut HashMap<common::file::File, LineIndex>,
) {
    for gp_scope in sym_view.generic_params(db) {
        let Some(gp_name) = gp_scope.name(db) else {
            continue;
        };
        let gp_name_str = gp_name.data(db).to_string();
        let gp_symbol = format!("{}[{}]", parent_symbol, gp_name_str);

        if let Some(name_span) = gp_scope.name_span(db)
            && let Some(resolved) = name_span.resolve(db)
            && let Some(loc) =
                span_to_location(&resolved, db, file_relative_paths, line_index_cache)
        {
            symbols.push((
                gp_symbol.clone(),
                IndexSymbol {
                    symbol: gp_symbol.clone(),
                    display_name: gp_name_str,
                    kind: symbol_information::Kind::TypeParameter as i32,
                    documentation: Vec::new(),
                    enclosing_symbol: parent_symbol.to_string(),
                    def_location: loc,
                    doc_url: None,
                },
            ));
        }

        for indexed_ref in ctx.ref_index.references_to(&gp_scope) {
            if let Some(resolved) = indexed_ref.span.resolve(db) {
                if let Some(loc) =
                    span_to_location(&resolved, db, file_relative_paths, line_index_cache)
                {
                    refs_by_symbol
                        .entry(gp_symbol.clone())
                        .or_default()
                        .push(IndexReference {
                            symbol: gp_symbol.clone(),
                            location: loc,
                            role: 0,
                        });
                }
            }
        }
    }
}

/// Populate the SemanticIndex for all ingots in a workspace.
pub fn populate_semantic_index(
    db: &mut driver::DriverDataBase,
    ingot_urls: &[url::Url],
) {
    use camino::Utf8PathBuf;
    use common::InputDb;
    use hir::hir_def::HirIngot;

    // Collect all results first (borrows db immutably), then upsert (borrows mutably).
    let mut all_symbols: Vec<(String, IndexSymbol)> = Vec::new();
    let mut all_references: Vec<(String, Vec<IndexReference>)> = Vec::new();

    for ingot_url in ingot_urls {
        let Ok(ctx) = index_util::IngotContext::resolve(db, ingot_url) else {
            continue;
        };

        let project_root = ingot_url
            .to_file_path()
            .ok()
            .and_then(|p| Utf8PathBuf::from_path_buf(p).ok())
            .unwrap_or_default();

        let file_relative_paths: HashMap<String, String> = ctx
            .ingot
            .all_modules(db)
            .iter()
            .filter_map(|top_mod| {
                let doc_url = top_mod_url(db, top_mod)?;
                let file_path = doc_url.to_file_path().ok()?;
                let utf8 = Utf8PathBuf::from_path_buf(file_path).ok()?;
                let relative = utf8
                    .strip_prefix(&project_root)
                    .ok()
                    .map(|p| p.to_string())
                    .unwrap_or_else(|| utf8.to_string());
                Some((doc_url.to_string(), relative))
            })
            .collect();

        for top_mod in ctx.ingot.all_modules(db).iter() {
            if let Some(result) = index_module(db, *top_mod, &ctx, &file_relative_paths) {
                all_symbols.extend(result.symbols);
                all_references.extend(result.references);
            }
        }
    }

    let semantic_index = db.semantic_index();
    semantic_index.upsert_module_symbols(db, all_symbols);
    semantic_index.upsert_module_references(db, all_references);
}

fn index_unnamed_generic_params<'db>(
    db: &'db driver::DriverDataBase,
    item: ItemKind<'db>,
    ctx: &index_util::IngotContext<'db>,
    doc_url: &str,
    file_relative_paths: &HashMap<String, String>,
    symbols: &mut Vec<(String, IndexSymbol)>,
    refs_by_symbol: &mut HashMap<String, Vec<IndexReference>>,
    line_index_cache: &mut HashMap<common::file::File, LineIndex>,
) {
    let impl_offset = item
        .span()
        .resolve(db)
        .map(|s| u32::from(s.range.start()))
        .unwrap_or(0);
    let parent_symbol = format!("fe fe {} {} __impl_{} ", ctx.name, ctx.version, impl_offset);

    let sym_view = SymbolView::from_item(item);
    index_generic_params(
        db,
        &sym_view,
        &parent_symbol,
        ctx,
        doc_url,
        file_relative_paths,
        symbols,
        refs_by_symbol,
        line_index_cache,
    );

    for child in sym_view.children(db) {
        let child_scope = child.scope();
        let Some(child_name) = child.name(db) else {
            continue;
        };
        let child_symbol = format!("{}{}", parent_symbol, child_name);
        let child_view = SymbolView::new(child_scope);
        index_generic_params(
            db,
            &child_view,
            &child_symbol,
            ctx,
            doc_url,
            file_relative_paths,
            symbols,
            refs_by_symbol,
            line_index_cache,
        );
    }
}
