//! Unified symbol intelligence view for HIR items.
//!
//! `SymbolView` provides a single entry point for extracting documentation,
//! signature text, canonical paths, visibility, and other metadata from any
//! HIR item. It exists so that consumers (SCIP, LSIF, doc extraction, LSP hover)
//! can call one shared API instead of each reimplementing the same HIR walk.

use crate::HirDb;
use crate::SpannedHirDb;
use crate::analysis::HirAnalysisDb;
use crate::hir_def::{Attr, EnumVariant, FieldParent, HirIngot, ItemKind, TopLevelMod, Visibility};
use crate::hir_def::scope_graph::ScopeId;
use crate::span::{DynLazySpan, LazySpan};
use common::diagnostics::Span;
use common::ingot::Ingot;
use rustc_hash::FxHashMap;

/// Kind of symbol, derived from `ItemKind` but also covering sub-item scopes
/// like fields, variants, and parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Module,
    Func,
    Struct,
    Contract,
    Enum,
    TypeAlias,
    Trait,
    Impl,
    ImplTrait,
    Const,
    Use,
    Field,
    Variant,
    GenericParam,
    FuncParam,
    TraitType,
    TraitConst,
}

impl SymbolKind {
    /// Short string label for the kind (matches `ItemKind::kind_name()` where applicable).
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Module => "mod",
            Self::Func => "fn",
            Self::Struct => "struct",
            Self::Contract => "contract",
            Self::Enum => "enum",
            Self::TypeAlias => "type",
            Self::Trait => "trait",
            Self::Impl => "impl",
            Self::ImplTrait => "impl trait",
            Self::Const => "const",
            Self::Use => "use",
            Self::Field => "field",
            Self::Variant => "variant",
            Self::GenericParam => "generic",
            Self::FuncParam => "param",
            Self::TraitType => "type",
            Self::TraitConst => "const",
        }
    }
}

impl<'db> From<ItemKind<'db>> for SymbolKind {
    fn from(item: ItemKind<'db>) -> Self {
        match item {
            ItemKind::TopMod(_) | ItemKind::Mod(_) => SymbolKind::Module,
            ItemKind::Func(_) => SymbolKind::Func,
            ItemKind::Struct(_) => SymbolKind::Struct,
            ItemKind::Contract(_) => SymbolKind::Contract,
            ItemKind::Enum(_) => SymbolKind::Enum,
            ItemKind::TypeAlias(_) => SymbolKind::TypeAlias,
            ItemKind::Trait(_) => SymbolKind::Trait,
            ItemKind::Impl(_) => SymbolKind::Impl,
            ItemKind::ImplTrait(_) => SymbolKind::ImplTrait,
            ItemKind::Const(_) => SymbolKind::Const,
            ItemKind::Use(_) => SymbolKind::Use,
            ItemKind::Body(_) => SymbolKind::Func, // Bodies are function-like
        }
    }
}

impl<'db> From<ScopeId<'db>> for SymbolKind {
    fn from(scope: ScopeId<'db>) -> Self {
        match scope {
            ScopeId::Item(item) => item.into(),
            ScopeId::GenericParam(..) => SymbolKind::GenericParam,
            ScopeId::TraitType(..) => SymbolKind::TraitType,
            ScopeId::TraitConst(..) => SymbolKind::TraitConst,
            ScopeId::FuncParam(..) => SymbolKind::FuncParam,
            ScopeId::Field(..) => SymbolKind::Field,
            ScopeId::Variant(_) => SymbolKind::Variant,
            ScopeId::Block(..) => SymbolKind::Func,
        }
    }
}

/// Lightweight view over a single HIR symbol (item, field, variant, etc.).
///
/// Constructed from a `ScopeId` and provides unified access to the metadata
/// that SCIP, LSIF, doc-engine, and LSP hover all need.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolView<'db> {
    scope: ScopeId<'db>,
}

impl<'db> SymbolView<'db> {
    pub fn new(scope: ScopeId<'db>) -> Self {
        Self { scope }
    }

    pub fn from_item(item: ItemKind<'db>) -> Self {
        Self {
            scope: ScopeId::from_item(item),
        }
    }

    /// The underlying scope.
    pub fn scope(&self) -> ScopeId<'db> {
        self.scope
    }

    /// The symbol kind.
    pub fn kind(&self) -> SymbolKind {
        self.scope.into()
    }

    /// Name of the symbol, if it has one.
    pub fn name(&self, db: &'db dyn HirDb) -> Option<String> {
        self.scope.name(db).map(|id| id.data(db).clone())
    }

    /// Fully qualified path (e.g. `ingot::module::Item`).
    pub fn pretty_path(&self, db: &dyn HirDb) -> Option<String> {
        self.scope.pretty_path(db)
    }

    /// Visibility of the symbol.
    pub fn visibility(&self, db: &dyn HirDb) -> Visibility {
        match self.scope {
            ScopeId::Item(item) => item.vis(db),
            // Fields inherit visibility from their parent in Fe,
            // but for doc purposes we report Public by default.
            ScopeId::Field(parent, idx) => {
                // Check the parent item's visibility as a proxy.
                let parent_item: ItemKind<'db> = match parent {
                    FieldParent::Struct(s) => ItemKind::Struct(s),
                    FieldParent::Contract(c) => ItemKind::Contract(c),
                    FieldParent::Variant(v) => ItemKind::Enum(v.enum_),
                };
                // Individual field visibility isn't tracked in Fe yet;
                // report the parent's visibility.
                let _ = idx;
                parent_item.vis(db)
            }
            ScopeId::Variant(v) => {
                // Variants inherit enum visibility.
                ItemKind::Enum(v.enum_).vis(db)
            }
            _ => Visibility::Private,
        }
    }

    /// Extract doc comments from attributes.
    pub fn docs(&self, db: &'db dyn HirDb) -> Option<String> {
        let attrs = self.scope.attrs(db)?;
        let doc_parts: Vec<String> = attrs
            .data(db)
            .iter()
            .filter_map(|attr| {
                if let Attr::DocComment(doc) = attr {
                    Some(doc.text.data(db).clone())
                } else {
                    None
                }
            })
            .collect();
        if doc_parts.is_empty() {
            None
        } else {
            Some(doc_parts.join("\n"))
        }
    }

    /// Extract the definition/signature text from source.
    ///
    /// Returns the source text from the beginning of the name's line up to
    /// (but not including) the body block. For items without bodies, returns
    /// the full item text.
    pub fn signature(&self, db: &'db dyn SpannedHirDb) -> Option<String> {
        let item = match self.scope {
            ScopeId::Item(item) => item,
            _ => return self.name(db),
        };
        get_item_signature(db, item)
    }

    /// Resolve the name span to a concrete `Span`.
    pub fn name_span(&self, db: &'db dyn SpannedHirDb) -> Option<Span> {
        self.scope.name_span(db)?.resolve(db)
    }

    /// Resolve the full item span to a concrete `Span`.
    pub fn def_span(&self, db: &'db dyn SpannedHirDb) -> Option<Span> {
        match self.scope {
            ScopeId::Item(item) => item.span().resolve(db),
            _ => self.scope.name_span(db)?.resolve(db),
        }
    }

    /// Source location as (file_path, line, column), 0-indexed.
    pub fn source_location(&self, db: &'db dyn SpannedHirDb) -> Option<SourceLocation> {
        let span = self.name_span(db)?;
        let text = span.file.text(db);
        let offset: usize = span.range.start().into();
        let (line, col) = byte_offset_to_line_col(text, offset);
        let file_path = span.file.url(db)?.to_file_path().ok()?;
        Some(SourceLocation {
            file: file_path.to_string_lossy().into_owned(),
            line: line as u32,
            column: col as u32,
        })
    }

    /// Iterate child scopes that are direct items (for structs: fields,
    /// for enums: variants, for traits: methods + assoc types, etc.).
    pub fn children(&self, db: &'db dyn HirDb) -> Vec<SymbolView<'db>> {
        match self.scope {
            ScopeId::Item(item) => item_children(db, item),
            _ => Vec::new(),
        }
    }
}

/// Source location in a file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub file: String,
    pub line: u32,
    pub column: u32,
}

// --- Internal helpers ---

/// Extract signature text for an item, trimming the body.
fn get_item_signature<'db>(db: &'db dyn SpannedHirDb, item: ItemKind<'db>) -> Option<String> {
    let span = item.span().resolve(db)?;
    let file_text = span.file.text(db);
    let text = file_text.as_str();

    let mut start: usize = span.range.start().into();
    let mut end: usize = span.range.end().into();

    // Trim body for items that have one
    let body_start = match item {
        ItemKind::Func(func) => func
            .body(db)
            .and_then(|b| b.span().resolve(db))
            .map(|s| s.range.start()),
        ItemKind::Mod(module) => module
            .scope()
            .name_span(db)
            .and_then(|s| s.resolve(db))
            .map(|s| s.range.end()),
        _ => None,
    };
    if let Some(body_start) = body_start {
        end = usize::from(body_start);
    }

    // Start at the beginning of the line where the name is defined
    if let Some(name_span) = item.name_span().and_then(|s| s.resolve(db)) {
        let mut name_line_start: usize = name_span.range.start().into();
        while name_line_start > 0 && text.as_bytes().get(name_line_start - 1) != Some(&b'\n') {
            name_line_start -= 1;
        }
        start = name_line_start;
    }

    // Bounds check
    if end > text.len() {
        end = text.len();
    }
    if start > end {
        start = end;
    }

    let mut sig = text[start..end].trim().to_string();

    // For impl blocks, truncate at opening brace
    if matches!(item, ItemKind::Impl(_) | ItemKind::ImplTrait(_)) {
        if let Some(brace_pos) = sig.find('{') {
            sig.truncate(brace_pos);
            sig = sig.trim_end().to_string();
        }
    }

    Some(sig)
}

/// Collect direct children of an item as SymbolViews.
fn item_children<'db>(db: &'db dyn HirDb, item: ItemKind<'db>) -> Vec<SymbolView<'db>> {
    let mut children = Vec::new();
    match item {
        ItemKind::Struct(s) => {
            let parent = FieldParent::Struct(s);
            for field_view in parent.fields(db) {
                children.push(SymbolView::new(field_view.scope()));
            }
        }
        ItemKind::Contract(c) => {
            let parent = FieldParent::Contract(c);
            for field_view in parent.fields(db) {
                children.push(SymbolView::new(field_view.scope()));
            }
        }
        ItemKind::Enum(e) => {
            for variant_view in e.variants(db) {
                let variant = EnumVariant::new(variant_view.owner, variant_view.idx);
                children.push(SymbolView::new(ScopeId::Variant(variant)));
            }
        }
        ItemKind::Trait(t) => {
            for method in t.methods(db) {
                children.push(SymbolView::from_item(ItemKind::Func(method)));
            }
            for assoc_type in t.assoc_types(db) {
                children.push(SymbolView::new(ScopeId::TraitType(t, assoc_type.idx as u16)));
            }
            for assoc_const in t.assoc_consts(db) {
                children.push(SymbolView::new(ScopeId::TraitConst(t, assoc_const.idx as u16)));
            }
        }
        ItemKind::Impl(i) => {
            for func in i.funcs(db) {
                children.push(SymbolView::from_item(ItemKind::Func(func)));
            }
        }
        ItemKind::ImplTrait(it) => {
            for method in it.methods(db) {
                children.push(SymbolView::from_item(ItemKind::Func(method)));
            }
        }
        ItemKind::Mod(m) => {
            let scope = m.scope();
            let scope_graph = scope.top_mod(db).scope_graph(db);
            for child_item in scope_graph.child_items(scope) {
                children.push(SymbolView::from_item(child_item));
            }
        }
        ItemKind::TopMod(tm) => {
            let scope = ScopeId::Item(ItemKind::TopMod(tm));
            let scope_graph = tm.scope_graph(db);
            for child_item in scope_graph.child_items(scope) {
                children.push(SymbolView::from_item(child_item));
            }
        }
        _ => {}
    }
    children
}

/// Convert a byte offset to 0-indexed (line, column).
fn byte_offset_to_line_col(text: &str, offset: usize) -> (usize, usize) {
    let mut line = 0;
    let mut col = 0;
    for (i, byte) in text.bytes().enumerate() {
        if i == offset {
            return (line, col);
        }
        if byte == b'\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (line, col)
}

// ---------------------------------------------------------------------------
// Doc-path utilities — map HIR scopes to documentation URL paths
// ---------------------------------------------------------------------------

/// Convert a ScopeId to its documentation URL path.
///
/// This is the single source of truth for mapping HIR scopes to doc URLs.
/// It qualifies paths with the ingot name (replacing "lib" prefix) and
/// includes the item kind suffix for disambiguation.
///
/// Returns the qualified URL path, e.g.:
/// - "ingot_name::Struct/struct" for a struct
/// - "ingot_name::module/mod" for a module
/// - "ingot_name::module::function/fn" for a function
pub fn scope_to_doc_path(db: &dyn SpannedHirDb, scope: ScopeId) -> Option<String> {
    let item = scope.item();
    let path = item.scope().pretty_path(db)?;
    let ingot = scope.top_mod(db).ingot(db);
    let qualified_path = qualify_path_with_ingot_name(db, &path, ingot);

    let kind_suffix = item_kind_to_url_suffix(item)?;

    Some(format!("{}/{}", qualified_path, kind_suffix))
}

/// Map HIR ItemKind to URL suffix string.
pub fn item_kind_to_url_suffix(item: ItemKind) -> Option<&'static str> {
    match item {
        ItemKind::TopMod(_) | ItemKind::Mod(_) => Some("mod"),
        ItemKind::Func(_) => Some("fn"),
        ItemKind::Struct(_) => Some("struct"),
        ItemKind::Enum(_) => Some("enum"),
        ItemKind::Trait(_) => Some("trait"),
        ItemKind::Contract(_) => Some("contract"),
        ItemKind::TypeAlias(_) => Some("type"),
        ItemKind::Const(_) => Some("const"),
        ItemKind::Impl(_) => Some("impl"),
        ItemKind::ImplTrait(_) => Some("impl"),
        ItemKind::Use(_) | ItemKind::Body(_) => None,
    }
}

/// Qualify a module path with the ingot's configured name.
///
/// Replaces "lib" prefix with the ingot's name from fe.toml.
/// - "lib" -> "ingot_name"
/// - "lib::Foo" -> "ingot_name::Foo"
/// - Other paths pass through unchanged
pub fn qualify_path_with_ingot_name(db: &dyn SpannedHirDb, path: &str, ingot: Ingot) -> String {
    let ingot_name = ingot
        .config(db)
        .and_then(|c| c.metadata.name)
        .map(|s| s.to_string());

    if let Some(name) = ingot_name {
        if path == "lib" {
            name
        } else if let Some(rest) = path.strip_prefix("lib::") {
            format!("{}::{}", name, rest)
        } else {
            path.to_string()
        }
    } else {
        path.to_string()
    }
}

// ---------------------------------------------------------------------------
// Reference Index — inverted index: target scope → reference spans
// ---------------------------------------------------------------------------

/// A single reference to a symbol, with its span and metadata.
#[derive(Debug, Clone)]
pub struct IndexedReference<'db> {
    /// The span of the reference occurrence.
    pub span: DynLazySpan<'db>,
    /// Whether this is a `Self` type reference (rename should skip these).
    pub is_self_ty: bool,
    /// The module containing this reference.
    pub module: TopLevelMod<'db>,
}

/// Inverted reference index for an ingot: maps each target scope to all
/// reference spans across the entire ingot.
///
/// Built once per ingot, eliminates the O(items × modules) scan that SCIP
/// and LSIF currently perform.
pub struct ReferenceIndex<'db> {
    /// Target scope → list of references pointing at it.
    index: FxHashMap<ScopeId<'db>, Vec<IndexedReference<'db>>>,
}

impl<'db> ReferenceIndex<'db> {
    /// Build the inverted reference index for an entire ingot.
    ///
    /// Walks every item in every module once, resolving all scope-level
    /// references and inverting them into a target → refs map.
    pub fn build<DB>(db: &'db DB, ingot: impl HirIngot<'db>) -> Self
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        use super::reference::resolver::resolved_item_scope_targets;

        let mut index: FxHashMap<ScopeId<'db>, Vec<IndexedReference<'db>>> = FxHashMap::default();

        for top_mod in ingot.all_modules(db) {
            let scope_graph = top_mod.scope_graph(db);
            for item in scope_graph.items_dfs(db) {
                for resolved in resolved_item_scope_targets(db, item) {
                    index
                        .entry(resolved.scope)
                        .or_default()
                        .push(IndexedReference {
                            span: resolved.span.clone(),
                            is_self_ty: resolved.is_self_ty,
                            module: *top_mod,
                        });
                }
            }
        }

        Self { index }
    }

    /// Look up all references to a given scope.
    pub fn references_to(&self, scope: &ScopeId<'db>) -> &[IndexedReference<'db>] {
        self.index.get(scope).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Iterate all (target, references) pairs in the index.
    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (&ScopeId<'db>, &[IndexedReference<'db>])> {
        self.index.iter().map(|(k, v)| (k, v.as_slice()))
    }

    /// Number of distinct targets in the index.
    pub fn len(&self) -> usize {
        self.index.len()
    }

    /// Whether the index is empty.
    pub fn is_empty(&self) -> bool {
        self.index.is_empty()
    }
}
