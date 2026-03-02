use std::collections::{HashMap, HashSet};
use std::io;

use rayon::prelude::*;

use camino::{Utf8Path, Utf8PathBuf};
use common::InputDb;
use common::diagnostics::Span;
use hir::{
    core::semantic::SymbolView,
    hir_def::{HirIngot, ItemKind, scope_graph::ScopeId},
    span::LazySpan,
};

use crate::index_util::{self, LineIndex};
use fe_web::model::DocIndex;
use scip::{
    symbol::format_symbol,
    types::{self, descriptor, symbol_information},
};

#[derive(Default)]
struct ScipDocumentBuilder {
    relative_path: String,
    occurrences: Vec<types::Occurrence>,
    symbols: Vec<types::SymbolInformation>,
    seen_symbols: HashSet<String>,
    /// Tracks (range, symbol) pairs to prevent duplicate occurrences.
    seen_occurrences: HashSet<(Vec<i32>, String)>,
}

impl ScipDocumentBuilder {
    fn new(relative_path: String) -> Self {
        Self {
            relative_path,
            ..Default::default()
        }
    }

    /// Merge another builder's contents into this one (for combining parallel results).
    fn merge(&mut self, other: ScipDocumentBuilder) {
        self.occurrences.extend(other.occurrences);
        for si in other.symbols {
            if self.seen_symbols.insert(si.symbol.clone()) {
                self.symbols.push(si);
            }
        }
    }

    fn into_document(self) -> types::Document {
        types::Document {
            language: "fe".to_string(),
            relative_path: self.relative_path,
            occurrences: self.occurrences,
            symbols: self.symbols,
            text: String::new(),
            position_encoding: types::PositionEncoding::UTF8CodeUnitOffsetFromLineStart.into(),
            special_fields: Default::default(),
        }
    }
}

fn span_to_scip_range(span: &Span, db: &dyn InputDb) -> Option<Vec<i32>> {
    let text = span.file.text(db);
    let line_index = LineIndex::new(text);

    let start = line_index.position(span.range.start().into());
    let end = line_index.position(span.range.end().into());

    let start_col = start.byte_offset.checked_sub(start.line_start_offset)?;
    let end_col = end.byte_offset.checked_sub(end.line_start_offset)?;

    Some(if start.line == end.line {
        vec![start.line as i32, start_col as i32, end_col as i32]
    } else {
        vec![
            start.line as i32,
            start_col as i32,
            end.line as i32,
            end_col as i32,
        ]
    })
}

fn top_mod_url(
    db: &driver::DriverDataBase,
    top_mod: &hir::hir_def::TopLevelMod<'_>,
) -> Option<url::Url> {
    top_mod.span().resolve(db)?.file.url(db)
}

fn relative_path(project_root: &Utf8Path, doc_url: &url::Url) -> Option<String> {
    if let Ok(file_path) = doc_url.to_file_path() {
        let file_path = Utf8PathBuf::from_path_buf(file_path).ok()?;
        let relative = file_path.strip_prefix(project_root).ok()?;
        Some(relative.to_string())
    } else {
        // Non-file URLs (e.g. builtin-core:///src/lib.fe): use URL path,
        // stripping the leading slash.
        let path = doc_url.path();
        Some(path.strip_prefix('/').unwrap_or(path).to_string())
    }
}

fn item_descriptor_suffix(item: ItemKind<'_>) -> descriptor::Suffix {
    match item {
        ItemKind::Struct(_) | ItemKind::Enum(_) | ItemKind::Trait(_) => descriptor::Suffix::Type,
        ItemKind::Func(_) => descriptor::Suffix::Term,
        _ => descriptor::Suffix::Meta,
    }
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

fn item_symbol<'db>(
    db: &driver::DriverDataBase,
    item: ItemKind<'db>,
    package_name: &str,
    package_version: &str,
) -> Option<(String, String)> {
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

fn child_descriptor_suffix(scope: ScopeId<'_>) -> descriptor::Suffix {
    use hir::core::semantic::SymbolKind;
    match SymbolKind::from(scope) {
        SymbolKind::Field | SymbolKind::Variant | SymbolKind::Func => descriptor::Suffix::Term,
        SymbolKind::TraitType => descriptor::Suffix::Type,
        _ => descriptor::Suffix::Meta,
    }
}

/// Build a SCIP symbol string for a child (field, variant, method, etc.)
/// by appending a descriptor to the parent's symbol.
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

fn push_occurrence(
    doc: &mut ScipDocumentBuilder,
    range: Vec<i32>,
    symbol: String,
    symbol_roles: i32,
) {
    let key = (range.clone(), symbol.clone());
    if !doc.seen_occurrences.insert(key) {
        return;
    }
    doc.occurrences.push(types::Occurrence {
        range,
        symbol,
        symbol_roles,
        override_documentation: Vec::new(),
        syntax_kind: types::SyntaxKind::Identifier.into(),
        diagnostics: Vec::new(),
        enclosing_range: Vec::new(),
        special_fields: Default::default(),
    });
}

pub fn generate_scip(
    db: &driver::DriverDataBase,
    ingot_url: &url::Url,
) -> io::Result<types::Index> {
    let ctx = index_util::IngotContext::resolve(db, ingot_url)?;

    // For file:// URLs, use the real filesystem path as project root.
    // For non-file URLs (e.g. builtin-core:///), use "/" as a virtual root
    // so that relative_path() can strip it from URL paths.
    let project_root_path = ingot_url
        .to_file_path()
        .ok()
        .and_then(|p| Utf8PathBuf::from_path_buf(p).ok())
        .unwrap_or_else(|| Utf8PathBuf::from("/"));

    // Pre-compute relative paths for all module files
    let file_relative_paths: HashMap<String, String> = ctx
        .ingot
        .all_modules(db)
        .iter()
        .filter_map(|top_mod| {
            let doc_url = top_mod_url(db, top_mod)?;
            let relative = relative_path(&project_root_path, &doc_url)?;
            Some((doc_url.to_string(), relative))
        })
        .collect();

    // Process modules in parallel using rayon with Salsa DB forks.
    // Each clone shares cached query results via Arc, making
    // IngotContext::resolve (incl. ReferenceIndex::build) cheap.
    // We create one fork per rayon thread (not per module) so each
    // fork builds the IngotContext once for its chunk of modules.
    let module_count = ctx.ingot.all_modules(db).len();
    let fork_count = rayon::current_num_threads().max(1).min(module_count);
    let db_forks: Vec<driver::DriverDataBase> = (0..fork_count).map(|_| db.clone()).collect();

    let parallel_results: Vec<Vec<HashMap<String, ScipDocumentBuilder>>> = db_forks
        .into_par_iter()
        .enumerate()
        .map(|(thread_idx, fork)| {
            let ctx = index_util::IngotContext::resolve(&fork, ingot_url)
                .expect("IngotContext::resolve should succeed in forked db");
            let modules = ctx.ingot.all_modules(&fork);
            let start = thread_idx * module_count / fork_count;
            let end = ((thread_idx + 1) * module_count / fork_count).min(module_count);
            (start..end)
                .filter_map(|idx| process_module(&fork, modules[idx], &ctx, &file_relative_paths))
                .collect()
        })
        .collect();

    let mut documents: HashMap<String, ScipDocumentBuilder> = HashMap::new();
    for chunk in parallel_results {
        for result in chunk {
            for (url, builder) in result {
                match documents.entry(url) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {
                        e.get_mut().merge(builder);
                    }
                    std::collections::hash_map::Entry::Vacant(e) => {
                        e.insert(builder);
                    }
                }
            }
        }
    }

    // Emit cross-ingot references.
    //
    // The ReferenceIndex tracks all scope references originating from this
    // ingot's code, including references to items in other ingots (e.g.
    // `Option` from `core`).  process_module() above only queries for
    // targets that appear in this ingot's scope graphs.  Here we iterate
    // remaining targets in foreign ingots and emit reference occurrences
    // so that cross-ingot type usages are visible in the SCIP data.
    emit_cross_ingot_references(db, &ctx, &file_relative_paths, &mut documents);

    let mut index = types::Index::new();
    index.metadata = Some(types::Metadata {
        version: types::ProtocolVersion::UnspecifiedProtocolVersion.into(),
        tool_info: Some(types::ToolInfo {
            name: "fe-scip".to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            arguments: Vec::new(),
            special_fields: Default::default(),
        })
        .into(),
        project_root: ingot_url.to_string(),
        text_document_encoding: types::TextEncoding::UTF8.into(),
        special_fields: Default::default(),
    })
    .into();

    let mut docs: Vec<_> = documents
        .into_values()
        .map(ScipDocumentBuilder::into_document)
        .collect();
    docs.sort_by(|a, b| a.relative_path.cmp(&b.relative_path));
    index.documents = docs;

    Ok(index)
}

/// Process a single module and return document fragments for all files it touches.
///
/// Each module produces definitions for its own file and reference occurrences
/// potentially spanning other files. Returns a map of file URL → document builder.
fn process_module<'db>(
    db: &'db driver::DriverDataBase,
    top_mod: hir::hir_def::TopLevelMod<'db>,
    ctx: &index_util::IngotContext<'db>,
    file_relative_paths: &HashMap<String, String>,
) -> Option<HashMap<String, ScipDocumentBuilder>> {
    let scope_graph = top_mod.scope_graph(db);
    let doc_url = top_mod_url(db, &top_mod)?.to_string();

    let mut documents: HashMap<String, ScipDocumentBuilder> = HashMap::new();

    // Ensure this module's file has a builder
    if let Some(relative) = file_relative_paths.get(&doc_url) {
        documents
            .entry(doc_url.clone())
            .or_insert_with(|| ScipDocumentBuilder::new(relative.clone()));
    }

    for item in scope_graph.items_dfs(db) {
        let maybe_symbol = item_symbol(db, item, &ctx.name, &ctx.version);

        // For unnamed items (Impl, ImplTrait, and functions inside them),
        // item_symbol() returns None because pretty_path() can't traverse
        // through unnamed parents. Still index their generic params below.
        let Some((ref symbol, ref display_name)) = maybe_symbol else {
            index_unnamed_item_generic_params(
                db,
                item,
                scope_graph,
                ctx,
                &doc_url,
                file_relative_paths,
                &mut documents,
            );
            continue;
        };

        if let Some(doc) = documents.get_mut(&doc_url) {
            if doc.seen_symbols.insert(symbol.clone()) {
                doc.symbols.push(types::SymbolInformation {
                    symbol: symbol.clone(),
                    documentation: index_util::hover_parts(db, item).to_scip_documentation(),
                    relationships: Vec::new(),
                    kind: item_symbol_kind(item).into(),
                    display_name: display_name.clone(),
                    signature_documentation: None.into(),
                    enclosing_symbol: String::new(),
                    special_fields: Default::default(),
                });
            }

            if let Some(name_span) = item.name_span().and_then(|span| span.resolve(db))
                && let Some(range) = span_to_scip_range(&name_span, db)
            {
                push_occurrence(
                    doc,
                    range,
                    symbol.clone(),
                    types::SymbolRole::Definition as i32,
                );
            }
        }

        // Reference occurrences (may span other files)
        let scope = ScopeId::from_item(item);
        for indexed_ref in ctx.ref_index.references_to(&scope) {
            if let Some(resolved) = indexed_ref.span.resolve(db) {
                let ref_url = match resolved.file.url(db) {
                    Some(url) => url.to_string(),
                    None => continue,
                };
                let ref_doc = documents.entry(ref_url.clone()).or_insert_with(|| {
                    let relative = file_relative_paths
                        .get(&ref_url)
                        .cloned()
                        .unwrap_or_default();
                    ScipDocumentBuilder::new(relative)
                });
                if let Some(range) = span_to_scip_range(&resolved, db) {
                    push_occurrence(ref_doc, range, symbol.clone(), 0);
                }
            }
        }

        // Index sub-items (fields, variants, methods, associated types).
        // Skip children of Mod/TopMod — those are top-level items already
        // handled by items_dfs, and indexing them here would create duplicate
        // symbols with different descriptor suffixes.
        if !matches!(item, ItemKind::Mod(_) | ItemKind::TopMod(_)) {
            let sym_view = SymbolView::from_item(item);
            for child in sym_view.children(db) {
                let child_scope = child.scope();
                let Some(child_name) = child.name(db) else {
                    continue;
                };

                let child_symbol = child_scip_symbol(symbol, &child_name, child_scope);
                let child_kind = child_symbol_kind(child_scope);

                if let Some(doc) = documents.get_mut(&doc_url) {
                    if doc.seen_symbols.insert(child_symbol.clone()) {
                        let child_docs = child.docs(db).map(|d| vec![d]).unwrap_or_default();

                        doc.symbols.push(types::SymbolInformation {
                            symbol: child_symbol.clone(),
                            documentation: child_docs,
                            relationships: Vec::new(),
                            kind: child_kind.into(),
                            display_name: child_name.clone(),
                            signature_documentation: None.into(),
                            enclosing_symbol: symbol.clone(),
                            special_fields: Default::default(),
                        });
                    }

                    if let Some(name_span) = child.name_span(db)
                        && let Some(range) = span_to_scip_range(&name_span, db)
                    {
                        push_occurrence(
                            doc,
                            range,
                            child_symbol.clone(),
                            types::SymbolRole::Definition as i32,
                        );
                    }
                }

                // Reference occurrences for the child
                for indexed_ref in ctx.ref_index.references_to(&child_scope) {
                    if let Some(resolved) = indexed_ref.span.resolve(db) {
                        let ref_url = match resolved.file.url(db) {
                            Some(url) => url.to_string(),
                            None => continue,
                        };
                        let ref_doc = documents.entry(ref_url.clone()).or_insert_with(|| {
                            let relative = file_relative_paths
                                .get(&ref_url)
                                .cloned()
                                .unwrap_or_default();
                            ScipDocumentBuilder::new(relative)
                        });
                        if let Some(range) = span_to_scip_range(&resolved, db) {
                            push_occurrence(ref_doc, range, child_symbol.clone(), 0);
                        }
                    }
                }
            }
        } // end if !Mod/TopMod

        // Index generic parameters (type params like T, A, etc.)
        let item_scope = ScopeId::from_item(item);
        for child_scope in scope_graph.children(item_scope) {
            let ScopeId::GenericParam(_, _) = child_scope else {
                continue;
            };
            let Some(param_name) = child_scope.name(db) else {
                continue;
            };
            let param_name_str = param_name.data(db).to_string();

            // SCIP TypeParameter descriptor: parent_symbol[name]
            let param_symbol = format!("{}[{}]", symbol, param_name_str);

            if let Some(doc) = documents.get_mut(&doc_url) {
                if doc.seen_symbols.insert(param_symbol.clone()) {
                    doc.symbols.push(types::SymbolInformation {
                        symbol: param_symbol.clone(),
                        documentation: Vec::new(),
                        relationships: Vec::new(),
                        kind: symbol_information::Kind::TypeParameter.into(),
                        display_name: param_name_str,
                        signature_documentation: None.into(),
                        enclosing_symbol: symbol.clone(),
                        special_fields: Default::default(),
                    });
                }

                if let Some(name_span) = child_scope.name_span(db)
                    && let Some(resolved) = name_span.resolve(db)
                    && let Some(range) = span_to_scip_range(&resolved, db)
                {
                    push_occurrence(
                        doc,
                        range,
                        param_symbol.clone(),
                        types::SymbolRole::Definition as i32,
                    );
                }
            }

            // Reference occurrences for generic params
            for indexed_ref in ctx.ref_index.references_to(&child_scope) {
                if let Some(resolved) = indexed_ref.span.resolve(db) {
                    let ref_url = match resolved.file.url(db) {
                        Some(url) => url.to_string(),
                        None => continue,
                    };
                    let ref_doc = documents.entry(ref_url.clone()).or_insert_with(|| {
                        let relative = file_relative_paths
                            .get(&ref_url)
                            .cloned()
                            .unwrap_or_default();
                        ScipDocumentBuilder::new(relative)
                    });
                    if let Some(range) = span_to_scip_range(&resolved, db) {
                        push_occurrence(ref_doc, range, param_symbol.clone(), 0);
                    }
                }
            }
        }
    }

    Some(documents)
}

/// Index generic parameters for items that lack a resolvable symbol.
///
/// `item_symbol()` returns None for Impl/ImplTrait blocks (no name) and also
/// for items nested inside them (pretty_path fails through unnamed parent).
/// Their type params still need SCIP occurrences so virtual sig files can resolve them.
fn index_unnamed_item_generic_params<'db>(
    db: &'db driver::DriverDataBase,
    item: ItemKind<'db>,
    scope_graph: &hir::core::hir_def::scope_graph::ScopeGraph<'db>,
    ctx: &index_util::IngotContext<'db>,
    doc_url: &str,
    file_relative_paths: &HashMap<String, String>,
    documents: &mut HashMap<String, ScipDocumentBuilder>,
) {
    // Construct a synthetic parent symbol from the impl's byte offset.
    // The exact string doesn't matter — it just needs to be unique so type param
    // symbols like `parent[E]` are distinguishable.
    let item_scope = ScopeId::from_item(item);
    let impl_offset = item
        .span()
        .resolve(db)
        .map(|s| u32::from(s.range.start()))
        .unwrap_or(0);
    let parent_symbol = format!("fe fe {} {} __impl_{} ", ctx.name, ctx.version, impl_offset);

    for child_scope in scope_graph.children(item_scope) {
        let ScopeId::GenericParam(_, _) = child_scope else {
            continue;
        };
        let Some(param_name) = child_scope.name(db) else {
            continue;
        };
        let param_name_str = param_name.data(db).to_string();
        let param_symbol = format!("{}[{}]", parent_symbol, param_name_str);

        if let Some(doc) = documents.get_mut(doc_url) {
            if doc.seen_symbols.insert(param_symbol.clone()) {
                doc.symbols.push(types::SymbolInformation {
                    symbol: param_symbol.clone(),
                    documentation: Vec::new(),
                    relationships: Vec::new(),
                    kind: symbol_information::Kind::TypeParameter.into(),
                    display_name: param_name_str,
                    signature_documentation: None.into(),
                    enclosing_symbol: parent_symbol.clone(),
                    special_fields: Default::default(),
                });
            }

            if let Some(name_span) = child_scope.name_span(db)
                && let Some(resolved) = name_span.resolve(db)
                && let Some(range) = span_to_scip_range(&resolved, db)
            {
                push_occurrence(
                    doc,
                    range,
                    param_symbol.clone(),
                    types::SymbolRole::Definition as i32,
                );
            }
        }

        // Reference occurrences
        for indexed_ref in ctx.ref_index.references_to(&child_scope) {
            if let Some(resolved) = indexed_ref.span.resolve(db) {
                let ref_url = match resolved.file.url(db) {
                    Some(url) => url.to_string(),
                    None => continue,
                };
                let ref_doc = documents.entry(ref_url.clone()).or_insert_with(|| {
                    let relative = file_relative_paths
                        .get(&ref_url)
                        .cloned()
                        .unwrap_or_default();
                    ScipDocumentBuilder::new(relative)
                });
                if let Some(range) = span_to_scip_range(&resolved, db) {
                    push_occurrence(ref_doc, range, param_symbol.clone(), 0);
                }
            }
        }
    }
}

/// Construct a SCIP symbol string for a cross-ingot scope target.
///
/// For `Item` scopes, delegates to `item_symbol`.  For child scopes (Field,
/// Variant, GenericParam, TraitType, TraitConst), constructs the parent item
/// symbol first, then appends the child descriptor.
fn scope_to_scip_symbol<'db>(
    db: &'db driver::DriverDataBase,
    scope: &ScopeId<'db>,
    ingot_name: &str,
    ingot_version: &str,
) -> Option<String> {
    match scope {
        ScopeId::Item(item) => item_symbol(db, *item, ingot_name, ingot_version).map(|(s, _)| s),
        ScopeId::GenericParam(item, _) => {
            let (parent_sym, _) = item_symbol(db, *item, ingot_name, ingot_version)?;
            let param_name = scope.name(db)?;
            Some(format!("{}[{}]", parent_sym, param_name.data(db)))
        }
        ScopeId::Field(_, _)
        | ScopeId::Variant(_)
        | ScopeId::TraitType(_, _)
        | ScopeId::TraitConst(_, _) => {
            let parent_item = scope.item();
            let (parent_sym, _) = item_symbol(db, parent_item, ingot_name, ingot_version)?;
            let child_name = scope.name(db)?;
            Some(child_scip_symbol(&parent_sym, child_name.data(db), *scope))
        }
        _ => None,
    }
}

/// Emit reference occurrences for cross-ingot targets.
///
/// The `ReferenceIndex` contains references to ALL scopes reachable from this
/// ingot's code — including types from other ingots like `core::option::Option`.
/// `process_module()` only emits occurrences for targets within the current
/// ingot.  This function handles the remaining cross-ingot targets.
fn emit_cross_ingot_references<'db>(
    db: &'db driver::DriverDataBase,
    ctx: &index_util::IngotContext<'db>,
    file_relative_paths: &HashMap<String, String>,
    documents: &mut HashMap<String, ScipDocumentBuilder>,
) {
    // Cache target ingot metadata to avoid repeated lookups
    let mut ingot_meta: HashMap<common::ingot::Ingot<'db>, (String, String)> = HashMap::new();

    for (target_scope, refs) in ctx.ref_index.iter() {
        // Skip targets within the current ingot (already handled by process_module)
        let target_ingot = target_scope.ingot(db);
        if target_ingot == ctx.ingot {
            continue;
        }

        let (target_name, target_version) = ingot_meta.entry(target_ingot).or_insert_with(|| {
            let name = target_ingot
                .config(db)
                .and_then(|c| c.metadata.name)
                .map(|n| n.to_string())
                .unwrap_or_else(|| "unknown".to_string());
            let version = target_ingot
                .version(db)
                .map(|v| v.to_string())
                .unwrap_or_else(|| "0.0.0".to_string());
            (name, version)
        });

        let Some(symbol) = scope_to_scip_symbol(db, target_scope, target_name, target_version)
        else {
            continue;
        };

        // Get the display name for SymbolInformation
        let display_name = target_scope
            .name(db)
            .map(|n| n.data(db).to_string())
            .unwrap_or_default();
        let kind = match target_scope {
            ScopeId::Item(item) => item_symbol_kind(*item),
            _ => child_symbol_kind(*target_scope),
        };

        // Emit reference occurrences into the appropriate source file documents
        for indexed_ref in refs {
            if let Some(resolved) = indexed_ref.span.resolve(db) {
                let ref_url = match resolved.file.url(db) {
                    Some(url) => url.to_string(),
                    None => continue,
                };
                let ref_doc = documents.entry(ref_url.clone()).or_insert_with(|| {
                    let relative = file_relative_paths
                        .get(&ref_url)
                        .cloned()
                        .unwrap_or_default();
                    ScipDocumentBuilder::new(relative)
                });
                if let Some(range) = span_to_scip_range(&resolved, db) {
                    push_occurrence(ref_doc, range, symbol.clone(), 0);
                }

                // Add SymbolInformation entry so the symbol appears in the JSON
                // `symbols` map (needed for inject_doc_urls and browser lookups).
                // Only needs to be added once per symbol.
                if ref_doc.seen_symbols.insert(symbol.clone()) {
                    ref_doc.symbols.push(types::SymbolInformation {
                        symbol: symbol.clone(),
                        documentation: Vec::new(),
                        relationships: Vec::new(),
                        kind: kind.into(),
                        display_name: display_name.clone(),
                        signature_documentation: None.into(),
                        enclosing_symbol: String::new(),
                        special_fields: Default::default(),
                    });
                }
            }
        }
    }
}

/// Convert a SCIP Index into a compact JSON string for browser embedding.
///
/// The JSON has two top-level keys:
/// - `symbols`: map from SCIP symbol string to metadata
/// - `files`: map from relative file path to sorted occurrence arrays
pub fn scip_to_json_data(index: &types::Index) -> String {
    use serde_json::{Map, Value, json};

    let mut symbols = Map::new();
    let mut files: HashMap<String, Vec<Value>> = HashMap::new();

    for doc in &index.documents {
        // Collect symbol info from this document
        for si in &doc.symbols {
            if symbols.contains_key(&si.symbol) {
                continue;
            }
            let kind = si.kind.value();
            let docs: Vec<Value> = si
                .documentation
                .iter()
                .map(|d| Value::String(d.clone()))
                .collect();
            let mut entry = Map::new();
            entry.insert("name".into(), Value::String(si.display_name.clone()));
            entry.insert("kind".into(), Value::Number(kind.into()));
            if !docs.is_empty() {
                entry.insert("docs".into(), Value::Array(docs));
            }
            if !si.enclosing_symbol.is_empty() {
                entry.insert(
                    "enclosing".into(),
                    Value::String(si.enclosing_symbol.clone()),
                );
            }
            symbols.insert(si.symbol.clone(), Value::Object(entry));
        }

        // Collect occurrences
        let file_occs = files.entry(doc.relative_path.clone()).or_default();
        for occ in &doc.occurrences {
            if occ.range.is_empty() || occ.symbol.is_empty() {
                continue;
            }
            let line = occ.range[0];
            let cs = occ.range[1];
            let ce = if occ.range.len() == 3 {
                occ.range[2]
            } else if occ.range.len() >= 4 {
                occ.range[3]
            } else {
                continue;
            };
            let is_def = (occ.symbol_roles & (types::SymbolRole::Definition as i32)) != 0;
            let mut obj = json!({
                "line": line,
                "cs": cs,
                "ce": ce,
                "sym": occ.symbol,
            });
            if is_def {
                obj.as_object_mut()
                    .unwrap()
                    .insert("def".into(), Value::Bool(true));
            }
            file_occs.push(obj);
        }
    }

    // Sort occurrences by line, then column
    for occs in files.values_mut() {
        occs.sort_by(|a, b| {
            let al = a["line"].as_i64().unwrap_or(0);
            let bl = b["line"].as_i64().unwrap_or(0);
            al.cmp(&bl).then(
                a["cs"]
                    .as_i64()
                    .unwrap_or(0)
                    .cmp(&b["cs"].as_i64().unwrap_or(0)),
            )
        });
    }

    let mut root = Map::new();
    root.insert("symbols".into(), Value::Object(symbols));
    root.insert(
        "files".into(),
        serde_json::to_value(files).unwrap_or(Value::Object(Map::new())),
    );
    Value::Object(root).to_string()
}

/// Inject `doc_url` fields into a SCIP JSON string by matching SCIP symbols
/// against items in the DocIndex. Sub-items get parent page + anchor URLs.
pub fn inject_doc_urls(scip_json: &str, doc_index: &DocIndex) -> String {
    let mut root: serde_json::Value = match serde_json::from_str(scip_json) {
        Ok(v) => v,
        Err(_) => return scip_json.to_string(),
    };

    // Build name → url_path lookups from DocIndex items.
    // Key by both full path and simple name; full path takes priority in lookups
    // to avoid collisions between same-named items (e.g. a::Foo vs b::Foo).
    let mut path_to_url: HashMap<&str, String> = HashMap::new();
    let mut name_to_url: HashMap<&str, String> = HashMap::new();
    for item in &doc_index.items {
        let url = item.url_path();
        path_to_url.insert(&item.path, url.clone());
        name_to_url.insert(&item.name, url);
    }

    // Build child lookups keyed by "parent_path::child_name" (qualified) and
    // simple child name (fallback). Uses `~` separator for SPA hash anchors.
    let mut qualified_child_to_url: HashMap<String, String> = HashMap::new();
    let mut child_to_url: HashMap<String, String> = HashMap::new();
    for item in &doc_index.items {
        let parent_url = item.url_path();
        for child in &item.children {
            let anchor = format!("{}.{}", child.kind.anchor_prefix(), child.name);
            let url = format!("{}~{}", parent_url, anchor);
            qualified_child_to_url.insert(format!("{}::{}", item.path, child.name), url.clone());
            child_to_url.insert(child.name.clone(), url);
        }
        // Also include methods from trait impl blocks
        for trait_impl in &item.trait_impls {
            for method in &trait_impl.methods {
                let anchor = format!("method.{}", method.name);
                let url = format!("{}~{}", parent_url, anchor);
                qualified_child_to_url
                    .insert(format!("{}::{}", item.path, method.name), url.clone());
                child_to_url.insert(method.name.clone(), url);
            }
        }
    }

    if let Some(symbols) = root.get_mut("symbols").and_then(|s| s.as_object_mut()) {
        for (sym_str, entry) in symbols.iter_mut() {
            if let Some(obj) = entry.as_object_mut() {
                let name = obj
                    .get("name")
                    .and_then(|n| n.as_str())
                    .unwrap_or("")
                    .to_string();
                let has_enclosing = obj
                    .get("enclosing")
                    .and_then(|e| e.as_str())
                    .is_some_and(|e| !e.is_empty());

                // Extract qualified path from SCIP symbol for precise lookup.
                // SCIP symbols look like "fe fe <pkg> <ver> Mod/Foo#" — extract
                // the descriptor chain and join with "::" for doc path matching.
                let qualified = scip_symbol_to_qualified_path(sym_str);

                // Try all lookup maps in order of specificity.
                // Items with an enclosing symbol may be either true sub-items
                // (fields/methods → child maps) or module-level items that happen
                // to have a parent module in SCIP (e.g. core::option::Option).
                let doc_url = qualified
                    .as_deref()
                    .and_then(|q| qualified_child_to_url.get(q).or_else(|| path_to_url.get(q)))
                    .cloned()
                    .or_else(|| {
                        if has_enclosing {
                            child_to_url.get(&name).cloned()
                        } else {
                            None
                        }
                    })
                    .or_else(|| name_to_url.get(name.as_str()).cloned());
                if let Some(url) = doc_url {
                    obj.insert("doc_url".to_string(), serde_json::Value::String(url));
                }
            }
        }
    }

    root.to_string()
}

/// Extract a qualified path (e.g. "mylib::Foo::bar") from a SCIP symbol string.
///
/// SCIP symbols look like: `fe fe <package> <version> Mod/Struct#method.`
/// Descriptor suffixes: `/` = namespace, `#` = type, `.` = term/method, `()` = macro
/// We strip the suffix char and join parts with `::`.
fn scip_symbol_to_qualified_path(sym: &str) -> Option<String> {
    // Skip the "fe fe <package> <version> " prefix — 4 space-separated tokens
    let mut parts_iter = sym.splitn(5, ' ');
    let _scheme = parts_iter.next()?;
    let _manager = parts_iter.next()?;
    let _package = parts_iter.next()?;
    let _version = parts_iter.next()?;
    let descriptor_part = parts_iter.next()?.trim();

    if descriptor_part.is_empty() {
        return None;
    }

    // Split on SCIP descriptor suffixes and collect the names
    let mut path_parts = Vec::new();
    let mut current = descriptor_part;
    while !current.is_empty() {
        // Find the next descriptor suffix: / # . ( [
        let end = current
            .find(['/', '#', '.', '(', '['])
            .unwrap_or(current.len());
        if end > 0 {
            path_parts.push(&current[..end]);
        }
        // Skip the suffix character(s)
        current = &current[end..];
        if current.starts_with("().") {
            current = &current[3..];
        } else if !current.is_empty() {
            current = &current[1..];
        }
    }

    if path_parts.is_empty() {
        None
    } else {
        Some(path_parts.join("::"))
    }
}

/// Enrich `rich_signature` fields in a DocIndex using SCIP occurrence positions,
/// and inject virtual SCIP documents for each signature's code block.
///
/// For each item/child/method that has a `signature_span`, finds SCIP occurrences
/// within that byte range, skips definitions, and builds linked signature parts.
/// This replaces the old name-matching tokenizer with compiler-resolved references.
///
/// Also emits virtual SCIP documents (paths prefixed `__sig__/`) containing all
/// occurrences within each signature's byte range.  The browser sets `data-file`
/// on the corresponding `<fe-code-block>` so the ScipStore can resolve symbols
/// positionally — enabling highlight-all for type parameters, Self, etc.
pub fn enrich_signatures(
    db: &driver::DriverDataBase,
    project_root: &Utf8Path,
    index: &mut DocIndex,
    scip_index: &mut types::Index,
) {
    enrich_signatures_with_base(db, project_root, None, index, scip_index);
}

/// Like `enrich_signatures`, but accepts an optional URL base for non-file ingots
/// (e.g. builtin-core:///). When `base_url` is Some, file URLs are resolved by
/// joining relative paths onto it instead of constructing file:// URLs.
pub fn enrich_signatures_with_base(
    db: &driver::DriverDataBase,
    project_root: &Utf8Path,
    base_url: Option<&url::Url>,
    index: &mut DocIndex,
    scip_index: &mut types::Index,
) {
    // Step 1: Build SCIP symbol → doc_url map.
    // Build a name→url map with ambiguity tracking (same logic as build_type_links):
    // if the same display name maps to different URLs, mark it ambiguous and exclude.
    let mut symbol_to_url: HashMap<String, String> = HashMap::new();
    let mut name_seen: HashMap<String, Option<String>> = HashMap::new();
    for item in &index.items {
        let url = item.url_path();
        name_seen
            .entry(item.name.clone())
            .and_modify(|existing| {
                if existing.as_deref() != Some(url.as_str()) {
                    *existing = None;
                }
            })
            .or_insert(Some(url));
    }
    for item in &index.items {
        let parent_url = item.url_path();
        for child in &item.children {
            let anchor = format!("{}.{}", child.kind.anchor_prefix(), child.name);
            let url = format!("{}~{}", parent_url, anchor);
            name_seen
                .entry(child.name.clone())
                .and_modify(|existing| {
                    if existing.as_deref() != Some(url.as_str()) {
                        *existing = None;
                    }
                })
                .or_insert(Some(url));
        }
        // Also include methods from trait impl blocks
        for trait_impl in &item.trait_impls {
            for method in &trait_impl.methods {
                let anchor = format!("method.{}", method.name);
                let url = format!("{}~{}", parent_url, anchor);
                name_seen
                    .entry(method.name.clone())
                    .and_modify(|existing| {
                        if existing.as_deref() != Some(url.as_str()) {
                            *existing = None;
                        }
                    })
                    .or_insert(Some(url));
            }
        }
    }
    for doc in &scip_index.documents {
        for si in &doc.symbols {
            if si.symbol.is_empty() {
                continue;
            }
            if let Some(Some(url)) = name_seen.get(&si.display_name) {
                symbol_to_url.insert(si.symbol.clone(), url.clone());
            }
        }
    }

    // Step 2: Build per-file byte-indexed occurrence lists from SCIP documents.
    // We need file text to convert SCIP line/col → byte offsets.
    let mut file_occurrences: HashMap<String, Vec<ByteOccurrence>> = HashMap::new();
    for doc in &scip_index.documents {
        let file_url = if let Some(base) = base_url {
            // Non-file URL base (e.g. builtin-core:///): join relative path onto it
            match base.join(&doc.relative_path) {
                Ok(u) => u,
                Err(_) => continue,
            }
        } else {
            let abs_path = project_root.join(&doc.relative_path);
            match url::Url::from_file_path(abs_path.as_std_path()) {
                Ok(u) => u,
                Err(_) => continue,
            }
        };
        let Some(file) = db.workspace().get(db, &file_url) else {
            continue;
        };
        let text = file.text(db);
        let line_index = LineIndex::new(text);

        let occs = file_occurrences
            .entry(doc.relative_path.clone())
            .or_default();

        for occ in &doc.occurrences {
            if occ.symbol.is_empty() || occ.range.is_empty() {
                continue;
            }
            let is_def = (occ.symbol_roles & (types::SymbolRole::Definition as i32)) != 0;
            let (byte_start, byte_end) = scip_range_to_byte_range(&line_index, &occ.range);
            occs.push(ByteOccurrence {
                byte_start,
                byte_end,
                symbol: occ.symbol.clone(),
                is_definition: is_def,
            });
        }
    }
    // Sort each file's occurrences by position
    for occs in file_occurrences.values_mut() {
        occs.sort_by_key(|o| (o.byte_start, o.byte_end));
    }

    // Step 3: For each item with a signature_span, overlay occurrences.
    for item in &mut index.items {
        if let Some(ref span) = item.signature_span {
            let parts = overlay_occurrences(
                span,
                &item.signature,
                project_root,
                &file_occurrences,
                &symbol_to_url,
            );
            if parts.iter().any(|p| p.link.is_some()) {
                item.rich_signature = parts;
            }
        }

        // Children (methods have spans; fields/variants don't)
        for child in &mut item.children {
            if let Some(ref span) = child.signature_span {
                let parts = overlay_occurrences(
                    span,
                    &child.signature,
                    project_root,
                    &file_occurrences,
                    &symbol_to_url,
                );
                if parts.iter().any(|p| p.link.is_some()) {
                    child.rich_signature = parts;
                }
            }
        }

        // Trait impl signatures and methods
        for trait_impl in &mut item.trait_impls {
            if let Some(ref span) = trait_impl.signature_span {
                let parts = overlay_occurrences(
                    span,
                    &trait_impl.signature,
                    project_root,
                    &file_occurrences,
                    &symbol_to_url,
                );
                if parts.iter().any(|p| p.link.is_some()) {
                    trait_impl.rich_signature = parts;
                }
            }
            for method in &mut trait_impl.methods {
                if let Some(ref span) = method.signature_span {
                    let parts = overlay_occurrences(
                        span,
                        &method.signature,
                        project_root,
                        &file_occurrences,
                        &symbol_to_url,
                    );
                    if parts.iter().any(|p| p.link.is_some()) {
                        method.rich_signature = parts;
                    }
                }
            }
        }
    }

    // Step 4: Build virtual SCIP documents for each signature's code block.
    // These contain ALL occurrences (defs, refs, type params, Self, etc.) so the
    // browser can resolve them positionally via character offsets.
    let mut virtual_docs: Vec<types::Document> = Vec::new();

    for item in &mut index.items {
        let parent_url = item.url_path();

        // Item signature
        if let Some(ref span) = item.signature_span {
            let scope = format!("__sig__/{}", parent_url);
            let occs =
                build_virtual_occurrences(span, &item.signature, project_root, &file_occurrences);
            if !occs.is_empty() {
                item.sig_scope = Some(scope.clone());
                virtual_docs.push(types::Document {
                    language: "fe".to_string(),
                    relative_path: scope,
                    occurrences: occs,
                    position_encoding: types::PositionEncoding::UTF8CodeUnitOffsetFromLineStart
                        .into(),
                    ..Default::default()
                });
            }
        }

        // Children (methods, fields, variants)
        for child in &mut item.children {
            if let Some(ref span) = child.signature_span {
                let anchor = format!("{}.{}", child.kind.anchor_prefix(), child.name);
                let scope = format!("__sig__/{}/{}", parent_url, anchor);
                let sig = if child.signature.is_empty() {
                    &child.name
                } else {
                    &child.signature
                };
                let occs = build_virtual_occurrences(span, sig, project_root, &file_occurrences);
                if !occs.is_empty() {
                    child.sig_scope = Some(scope.clone());
                    virtual_docs.push(types::Document {
                        language: "fe".to_string(),
                        relative_path: scope,
                        occurrences: occs,
                        position_encoding: types::PositionEncoding::UTF8CodeUnitOffsetFromLineStart
                            .into(),
                        ..Default::default()
                    });
                }
            }
        }

        // Trait impl signatures and their methods
        for trait_impl in &mut item.trait_impls {
            let impl_anchor = if trait_impl.trait_name.is_empty() {
                // Inherent impls: use a short hash of the signature for a stable scope path.
                // This avoids brittle index-based naming that breaks on reordering.
                let h = simple_hash(&trait_impl.signature);
                format!("impl-{:x}", h & 0xFFFF)
            } else {
                format!("impl-{}", sanitize_anchor_name(&trait_impl.trait_name))
            };

            if let Some(ref span) = trait_impl.signature_span {
                let scope = format!("__sig__/{}/{}", parent_url, impl_anchor);
                let occs = build_virtual_occurrences(
                    span,
                    &trait_impl.signature,
                    project_root,
                    &file_occurrences,
                );
                if !occs.is_empty() {
                    trait_impl.sig_scope = Some(scope.clone());
                    virtual_docs.push(types::Document {
                        language: "fe".to_string(),
                        relative_path: scope,
                        occurrences: occs,
                        position_encoding: types::PositionEncoding::UTF8CodeUnitOffsetFromLineStart
                            .into(),
                        ..Default::default()
                    });
                }
            }

            for method in &mut trait_impl.methods {
                if let Some(ref span) = method.signature_span {
                    let scope = format!(
                        "__sig__/{}/{}/method.{}",
                        parent_url, impl_anchor, method.name
                    );
                    let occs = build_virtual_occurrences(
                        span,
                        &method.signature,
                        project_root,
                        &file_occurrences,
                    );
                    if !occs.is_empty() {
                        method.sig_scope = Some(scope.clone());
                        virtual_docs.push(types::Document {
                            language: "fe".to_string(),
                            relative_path: scope,
                            occurrences: occs,
                            position_encoding:
                                types::PositionEncoding::UTF8CodeUnitOffsetFromLineStart.into(),
                            ..Default::default()
                        });
                    }
                }
            }
        }

        // Implementors (shown on trait pages)
        for imp in &mut item.implementors {
            if let Some(ref span) = imp.signature_span {
                let type_anchor = imp.type_name.replace(['<', '>', ' ', ','], "_");
                let scope = format!("__sig__/{}/impl-{}", parent_url, type_anchor);
                let occs = build_virtual_occurrences(
                    span,
                    &imp.signature,
                    project_root,
                    &file_occurrences,
                );
                if !occs.is_empty() {
                    imp.sig_scope = Some(scope.clone());
                    virtual_docs.push(types::Document {
                        language: "fe".to_string(),
                        relative_path: scope,
                        occurrences: occs,
                        position_encoding: types::PositionEncoding::UTF8CodeUnitOffsetFromLineStart
                            .into(),
                        ..Default::default()
                    });
                }
            }
        }
    }

    scip_index.documents.extend(virtual_docs);
}

/// A single SCIP occurrence with byte offsets (converted from line/col).
struct ByteOccurrence {
    byte_start: usize,
    byte_end: usize,
    symbol: String,
    is_definition: bool,
}

/// Convert a SCIP range `[line, col_start, col_end]` or `[start_line, start_col,
/// end_line, end_col]` to a `(byte_start, byte_end)` pair using a `LineIndex`.
fn scip_range_to_byte_range(line_index: &LineIndex, range: &[i32]) -> (usize, usize) {
    if range.len() == 3 {
        // Same-line: [line, col_start, col_end]
        let line = range[0] as usize;
        let cs = range[1] as usize;
        let ce = range[2] as usize;
        (
            line_index.byte_offset_from_line_col(line, cs),
            line_index.byte_offset_from_line_col(line, ce),
        )
    } else if range.len() >= 4 {
        // Multi-line: [start_line, start_col, end_line, end_col]
        let sl = range[0] as usize;
        let sc = range[1] as usize;
        let el = range[2] as usize;
        let ec = range[3] as usize;
        (
            line_index.byte_offset_from_line_col(sl, sc),
            line_index.byte_offset_from_line_col(el, ec),
        )
    } else {
        (0, 0)
    }
}

/// Build `rich_signature` parts by overlaying SCIP occurrences on a signature span.
///
/// Finds non-definition occurrences within the signature's byte range, maps their
/// SCIP symbols to doc URLs, and splits the signature text into alternating
/// plain-text and linked parts.
fn overlay_occurrences(
    span: &fe_web::model::SignatureSpanData,
    sig_text: &str,
    project_root: &Utf8Path,
    file_occurrences: &HashMap<String, Vec<ByteOccurrence>>,
    symbol_urls: &HashMap<String, String>,
) -> Vec<fe_web::model::SignaturePart> {
    use fe_web::model::SignaturePart;

    // Convert the span's file URL to a relative path for lookup
    let rel_path = match url::Url::parse(&span.file_url) {
        Ok(u) => relative_path(project_root, &u),
        Err(_) => None,
    };
    let Some(rel_path) = rel_path else {
        return vec![];
    };
    let Some(occs) = file_occurrences.get(&rel_path) else {
        return vec![];
    };

    // Filter to non-definition occurrences within the signature's byte range
    // that have known doc URLs.
    let mut sig_occs: Vec<&ByteOccurrence> = occs
        .iter()
        .filter(|o| {
            o.byte_start >= span.byte_start
                && o.byte_end <= span.byte_end
                && !o.is_definition
                && symbol_urls.contains_key(&o.symbol)
        })
        .collect();
    sig_occs.sort_by_key(|o| o.byte_start);

    if sig_occs.is_empty() {
        return vec![];
    }

    let mut parts = Vec::new();
    let mut pos = 0usize; // position within sig_text

    for occ in &sig_occs {
        let occ_start = occ.byte_start.saturating_sub(span.byte_start);
        let occ_end = occ.byte_end.saturating_sub(span.byte_start);

        if occ_start > sig_text.len() || occ_end > sig_text.len() || occ_start < pos {
            continue;
        }

        // Plain text before this occurrence
        if occ_start > pos {
            parts.push(SignaturePart::text(&sig_text[pos..occ_start]));
        }

        // Linked occurrence
        let occ_text = &sig_text[occ_start..occ_end];
        let url = &symbol_urls[&occ.symbol];
        parts.push(SignaturePart::link(occ_text, url));

        pos = occ_end;
    }

    // Remaining text
    if pos < sig_text.len() {
        parts.push(SignaturePart::text(&sig_text[pos..]));
    }

    parts
}

/// Convert a byte offset within a signature text to (line, col) relative to the
/// signature start.  Line 0 is the first line; col is byte offset from last newline.
fn byte_offset_to_sig_line_col(sig_text: &str, byte_offset: usize) -> (i32, i32) {
    let clamped = byte_offset.min(sig_text.len());
    let prefix = &sig_text[..clamped];
    let line = prefix.bytes().filter(|&b| b == b'\n').count() as i32;
    let last_newline = prefix.rfind('\n').map(|p| p + 1).unwrap_or(0);
    let col = (clamped - last_newline) as i32;
    (line, col)
}

/// Build SCIP occurrences for a virtual signature document.
///
/// Unlike `overlay_occurrences` (which filters to non-definition references with
/// known doc URLs), this returns ALL occurrences within the byte range — defs,
/// refs, type params, Self, etc. — so the browser can highlight everything.
fn build_virtual_occurrences(
    span: &fe_web::model::SignatureSpanData,
    sig_text: &str,
    project_root: &Utf8Path,
    file_occurrences: &HashMap<String, Vec<ByteOccurrence>>,
) -> Vec<types::Occurrence> {
    let rel_path = match url::Url::parse(&span.file_url) {
        Ok(u) => relative_path(project_root, &u),
        Err(_) => None,
    };
    let Some(rel_path) = rel_path else {
        return vec![];
    };
    let Some(occs) = file_occurrences.get(&rel_path) else {
        return vec![];
    };

    // Deduplicate by position: multiple SCIP symbols can overlap at the same
    // source position (e.g. namespace-path vs item symbol).  Keep one per
    // (byte_start, byte_end), preferring definitions over references.
    let mut seen_positions: HashMap<(usize, usize), usize> = HashMap::new();
    let mut result: Vec<types::Occurrence> = Vec::new();
    for occ in occs {
        if occ.byte_start < span.byte_start || occ.byte_end > span.byte_end {
            continue;
        }
        let occ_start = occ.byte_start - span.byte_start;
        let occ_end = occ.byte_end - span.byte_start;
        if occ_end > sig_text.len() {
            continue;
        }

        let pos_key = (occ_start, occ_end);
        if let Some(&existing_idx) = seen_positions.get(&pos_key) {
            // If new occurrence is a definition and existing is not, replace it
            if occ.is_definition && result[existing_idx].symbol_roles == 0 {
                let (start_line, start_col) = byte_offset_to_sig_line_col(sig_text, occ_start);
                let (end_line, end_col) = byte_offset_to_sig_line_col(sig_text, occ_end);
                let range = if start_line == end_line {
                    vec![start_line, start_col, end_col]
                } else {
                    vec![start_line, start_col, end_line, end_col]
                };
                result[existing_idx] = types::Occurrence {
                    range,
                    symbol: occ.symbol.clone(),
                    symbol_roles: types::SymbolRole::Definition as i32,
                    syntax_kind: types::SyntaxKind::Identifier.into(),
                    ..Default::default()
                };
            }
            continue;
        }

        let (start_line, start_col) = byte_offset_to_sig_line_col(sig_text, occ_start);
        let (end_line, end_col) = byte_offset_to_sig_line_col(sig_text, occ_end);

        let range = if start_line == end_line {
            vec![start_line, start_col, end_col]
        } else {
            vec![start_line, start_col, end_line, end_col]
        };

        let symbol_roles = if occ.is_definition {
            types::SymbolRole::Definition as i32
        } else {
            0
        };

        seen_positions.insert(pos_key, result.len());
        result.push(types::Occurrence {
            range,
            symbol: occ.symbol.clone(),
            symbol_roles,
            syntax_kind: types::SyntaxKind::Identifier.into(),
            ..Default::default()
        });
    }

    // Fallback: scan signature text for names that appear in existing occurrences
    // but have missing references.  Covers:
    //   - Type params (T in Self<T>) not tracked by the ref_index
    //   - Self references in arg positions that the HIR doesn't emit
    // Collect (text_in_signature, scip_symbol) pairs from existing occurrences.
    let mut name_to_symbol: HashMap<String, String> = HashMap::new();
    for occ in &result {
        if occ.symbol.is_empty() {
            continue;
        }
        // Extract the text this occurrence covers in the signature
        let (line, cs, ce) = match occ.range.len() {
            3 => (occ.range[0], occ.range[1], occ.range[2]),
            4 => (occ.range[0], occ.range[1], occ.range[3]),
            _ => continue,
        };
        // For single-line occurrences on line 0, extract text directly
        if occ.range.len() == 3 {
            let line_start = sig_text
                .bytes()
                .enumerate()
                .filter(|&(_, b)| b == b'\n')
                .nth(line as usize)
                .map(|(i, _)| i + 1)
                .unwrap_or(if line == 0 { 0 } else { sig_text.len() });
            let start = line_start + cs as usize;
            let end = line_start + ce as usize;
            if end <= sig_text.len() {
                let text = &sig_text[start..end];
                // Only track short identifiers (type params, Self, etc.)
                if !text.is_empty()
                    && text.len() <= 20
                    && text.chars().all(|c| c.is_alphanumeric() || c == '_')
                {
                    name_to_symbol
                        .entry(text.to_string())
                        .or_insert_with(|| occ.symbol.clone());
                }
            }
        }
    }

    let sig_bytes = sig_text.as_bytes();
    for (name, symbol) in &name_to_symbol {
        let name_bytes = name.as_bytes();
        let mut search_start = 0;
        while search_start + name_bytes.len() <= sig_bytes.len() {
            if let Some(pos) = sig_text[search_start..].find(name.as_str()) {
                let abs_pos = search_start + pos;
                let end_pos = abs_pos + name_bytes.len();
                let prev_ok = abs_pos == 0
                    || !sig_bytes[abs_pos - 1].is_ascii_alphanumeric()
                        && sig_bytes[abs_pos - 1] != b'_';
                let next_ok = end_pos >= sig_bytes.len()
                    || !sig_bytes[end_pos].is_ascii_alphanumeric() && sig_bytes[end_pos] != b'_';
                if prev_ok && next_ok && !seen_positions.contains_key(&(abs_pos, end_pos)) {
                    let (sl, sc) = byte_offset_to_sig_line_col(sig_text, abs_pos);
                    let (el, ec) = byte_offset_to_sig_line_col(sig_text, end_pos);
                    let range = if sl == el {
                        vec![sl, sc, ec]
                    } else {
                        vec![sl, sc, el, ec]
                    };
                    seen_positions.insert((abs_pos, end_pos), result.len());
                    result.push(types::Occurrence {
                        range,
                        symbol: symbol.clone(),
                        symbol_roles: 0,
                        syntax_kind: types::SyntaxKind::Identifier.into(),
                        ..Default::default()
                    });
                }
                search_start = abs_pos + 1;
            } else {
                break;
            }
        }
    }

    result
}

/// Sanitize a trait name for use in anchor IDs (matches the JS side).
fn sanitize_anchor_name(name: &str) -> String {
    name.replace(['<', '>', ' ', ','], "_")
}

/// djb2 hash for generating stable short identifiers from strings.
fn simple_hash(s: &str) -> u32 {
    let mut h: u32 = 5381;
    for b in s.bytes() {
        h = h.wrapping_mul(33).wrapping_add(b as u32);
    }
    h
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generate_test_scip(code: &str) -> types::Index {
        let mut db = driver::DriverDataBase::default();
        let url = url::Url::parse("file:///test.fe").unwrap();
        db.workspace()
            .touch(&mut db, url.clone(), Some(code.to_string()));
        let ingot_url = url::Url::parse("file:///").unwrap();
        generate_scip(&db, &ingot_url).expect("generate scip index")
    }

    #[test]
    fn test_scip_basic_structure() {
        let index = generate_test_scip("fn hello() -> i32 {\n    42\n}\n");

        let metadata = index.metadata.as_ref().expect("metadata");
        assert_eq!(
            metadata.tool_info.as_ref().expect("tool info").name,
            "fe-scip"
        );
        assert_eq!(
            metadata.text_document_encoding.value(),
            types::TextEncoding::UTF8 as i32
        );
        assert!(!index.documents.is_empty(), "should contain documents");
    }

    #[test]
    fn test_scip_contains_symbols_and_occurrences() {
        let code = r#"struct Point {
    x: i32
    y: i32
}

fn make_point() -> Point {
    Point { x: 1, y: 2 }
}
"#;
        let index = generate_test_scip(code);
        let doc = index
            .documents
            .iter()
            .find(|d| d.relative_path == "test.fe")
            .expect("document");

        assert!(
            doc.symbols.iter().any(|s| s.display_name == "Point"),
            "expected Point symbol"
        );
        assert!(
            doc.occurrences
                .iter()
                .any(|o| (o.symbol_roles & (types::SymbolRole::Definition as i32)) != 0),
            "expected at least one definition occurrence"
        );
    }

    #[test]
    fn test_scip_indexes_sub_items() {
        let code = r#"struct Point {
    pub x: i32
    pub y: i32
}

fn make_point() -> Point {
    Point { x: 1, y: 2 }
}
"#;
        let index = generate_test_scip(code);
        let doc = index
            .documents
            .iter()
            .find(|d| d.relative_path == "test.fe")
            .expect("document");

        // Fields should be indexed as sub-items
        let field_names: Vec<&str> = doc
            .symbols
            .iter()
            .filter(|s| !s.enclosing_symbol.is_empty())
            .map(|s| s.display_name.as_str())
            .collect();
        assert!(
            field_names.contains(&"x"),
            "expected field 'x' in symbols, found: {field_names:?}"
        );
        assert!(
            field_names.contains(&"y"),
            "expected field 'y' in symbols, found: {field_names:?}"
        );

        // Field symbols should have enclosing_symbol pointing to Point
        for si in &doc.symbols {
            if si.display_name == "x" || si.display_name == "y" {
                assert!(
                    si.enclosing_symbol.contains("Point"),
                    "field '{}' should have Point as enclosing: {}",
                    si.display_name,
                    si.enclosing_symbol
                );
            }
        }
    }

    #[test]
    fn test_scip_to_json_data() {
        let code = r#"struct Point {
    pub x: i32
}

fn make_point() -> Point {
    Point { x: 1 }
}
"#;
        let index = generate_test_scip(code);
        let json = scip_to_json_data(&index);

        // Parse and check structure
        let parsed: serde_json::Value = serde_json::from_str(&json).expect("valid JSON");
        assert!(parsed.get("symbols").is_some(), "should have symbols key");
        assert!(parsed.get("files").is_some(), "should have files key");

        let symbols = parsed["symbols"].as_object().unwrap();
        // Should have Point, x, and make_point
        let names: Vec<&str> = symbols
            .values()
            .filter_map(|v| v.get("name").and_then(|n| n.as_str()))
            .collect();
        assert!(names.contains(&"Point"), "should contain Point: {names:?}");
        assert!(
            names.contains(&"make_point"),
            "should contain make_point: {names:?}"
        );
    }

    #[test]
    fn test_scip_symbol_to_qualified_path() {
        // Type descriptor: Mod/Struct#
        assert_eq!(
            scip_symbol_to_qualified_path("fe fe mylib 0.1.0 Mod/Struct#"),
            Some("Mod::Struct".into())
        );
        // Method descriptor: Mod/Struct#method.
        assert_eq!(
            scip_symbol_to_qualified_path("fe fe mylib 0.1.0 Mod/Struct#method."),
            Some("Mod::Struct::method".into())
        );
        // Deeply nested: a/b/Foo#
        assert_eq!(
            scip_symbol_to_qualified_path("fe fe pkg 1.0 a/b/Foo#"),
            Some("a::b::Foo".into())
        );
        // Single item
        assert_eq!(
            scip_symbol_to_qualified_path("fe fe pkg 1.0 Foo#"),
            Some("Foo".into())
        );
        // Empty descriptor part
        assert_eq!(scip_symbol_to_qualified_path("fe fe pkg 1.0 "), None);
    }
}
