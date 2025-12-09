//! HasReferences trait and implementations.

use parser::TextSize;

use crate::{
    HirDb, SpannedHirDb,
    analysis::HirAnalysisDb,
    analysis::ty::ty_check::{LocalBinding, check_func_body},
    hir_def::scope_graph::ScopeId,
    hir_def::{Body, Func, ItemKind, TopLevelMod},
    span::{DynLazySpan, LazySpan},
};

use super::collector::{
    body_references, enum_references, func_signature_references, impl_references,
    impl_trait_references, struct_references, trait_references, type_alias_references,
    use_references,
};
use super::{ReferenceView, Target};

/// Empty reference slice for types that don't contain references.
static EMPTY_REFS: &[ReferenceView<'static>] = &[];

/// Trait for types that contain symbolic references.
///
/// This provides a unified interface for accessing references in different
/// parts of the HIR (bodies, items, scopes).
pub trait HasReferences<'db> {
    /// Returns all symbolic references within this node.
    fn references(&self, db: &'db dyn HirDb) -> &'db [ReferenceView<'db>];
}

impl<'db> HasReferences<'db> for Body<'db> {
    fn references(&self, db: &'db dyn HirDb) -> &'db [ReferenceView<'db>] {
        body_references(db, *self)
    }
}

impl<'db> HasReferences<'db> for ScopeId<'db> {
    fn references(&self, db: &'db dyn HirDb) -> &'db [ReferenceView<'db>] {
        match self {
            ScopeId::Item(item) => item.references(db),
            ScopeId::Block(body, _) => body.references(db),
            ScopeId::GenericParam(_, _) => EMPTY_REFS,
            ScopeId::TraitType(_, _) => EMPTY_REFS,
            ScopeId::TraitConst(_, _) => EMPTY_REFS,
            ScopeId::FuncParam(_, _) => EMPTY_REFS,
            ScopeId::Field(_, _) => EMPTY_REFS,
            ScopeId::Variant(_) => EMPTY_REFS,
        }
    }
}

impl<'db> ScopeId<'db> {
    /// Find the reference at a cursor position within this scope.
    pub fn reference_at(
        self,
        db: &'db dyn SpannedHirDb,
        cursor: TextSize,
    ) -> Option<&'db ReferenceView<'db>> {
        self.references(db).iter().find(|r| {
            r.span()
                .resolve(db)
                .is_some_and(|s| s.range.contains(cursor))
        })
    }
}

impl<'db> HasReferences<'db> for ItemKind<'db> {
    fn references(&self, db: &'db dyn HirDb) -> &'db [ReferenceView<'db>] {
        match self {
            ItemKind::Body(body) => body.references(db),
            ItemKind::Func(func) => func_signature_references(db, *func),
            ItemKind::Struct(struct_) => struct_references(db, *struct_),
            ItemKind::Enum(enum_) => enum_references(db, *enum_),
            ItemKind::TypeAlias(alias) => type_alias_references(db, *alias),
            ItemKind::Impl(impl_) => impl_references(db, *impl_),
            ItemKind::Trait(trait_) => trait_references(db, *trait_),
            ItemKind::ImplTrait(impl_trait) => impl_trait_references(db, *impl_trait),
            ItemKind::Use(use_item) => use_references(db, *use_item),
            ItemKind::Const(c) => c.body(db).to_opt().map_or(EMPTY_REFS, |b| b.references(db)),
            // Modules don't contain references themselves
            ItemKind::TopMod(_) | ItemKind::Mod(_) => EMPTY_REFS,
            // Contract references could be added in the future
            ItemKind::Contract(_) => EMPTY_REFS,
        }
    }
}

impl<'db> TopLevelMod<'db> {
    /// Resolve what's at a cursor position to its definition target(s).
    ///
    /// This is the unified entry point for goto-definition and find-all-references.
    /// It checks (in order):
    /// 1. If cursor is on a param/local binding site, return Target::Local
    ///    (checked first because `self` params have a fallback `Self` type path
    ///    that overlaps with the param name span)
    /// 2. If cursor is on an item definition name, return that item's scope
    /// 3. If cursor is on a reference, resolve it to its target(s) - may be ambiguous
    pub fn target_at<DB>(self, db: &'db DB, cursor: TextSize) -> super::TargetResolution<'db>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        // 1. Check if cursor is on a param/local binding site
        // (must be checked before references because `self` params have a fallback
        // `Self` type that overlaps with the param name position)
        if let Some(target) = self.binding_at(db, cursor) {
            return super::TargetResolution::Single(target);
        }

        // 2. Check if cursor is on an item definition name
        if let Some(scope) = self.definition_at(db, cursor) {
            return super::TargetResolution::Single(Target::Scope(scope));
        }

        // 3. Check if cursor is on a reference - preserve ambiguity
        if let Some(reference) = self.reference_at(db, cursor) {
            return reference.target_at(db, cursor);
        }

        super::TargetResolution::None
    }

    /// Check if cursor is on a function parameter name.
    ///
    /// Returns a Target::Local if the cursor is on a param binding site.
    /// Note: local variable bindings (let x = ...) are handled via PathView
    /// with body_ctx: PatBinding, so they go through the reference_at path.
    fn binding_at<DB>(self, db: &'db DB, cursor: TextSize) -> Option<Target<'db>>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        // Find the enclosing function
        let func = self.find_enclosing_func(db, cursor)?;

        // Check if cursor is on a param name
        // (must be checked before references because `self` params have a fallback
        // `Self` type that overlaps with the param name position)
        self.param_binding_at(db, func, cursor)
    }

    /// Check if cursor is on a function parameter name.
    fn param_binding_at<DB>(
        self,
        db: &'db DB,
        func: Func<'db>,
        cursor: TextSize,
    ) -> Option<Target<'db>>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        let (_, typed_body) = check_func_body(db, func);

        // Check each param's name span
        for (idx, param_view) in func.params(db).enumerate() {
            let param_span = param_view.span();
            let name_span = param_span.name();

            if let Some(resolved) = name_span.resolve(db)
                && resolved.range.contains(cursor)
            {
                // Found cursor on param name - get binding from type checker
                let binding = typed_body.param_binding(idx)?;
                let LocalBinding::Param { ty, .. } = binding else {
                    return None;
                };

                return Some(Target::Local {
                    span: name_span.into(),
                    ty,
                    func,
                    binding,
                });
            }
        }

        None
    }

    /// Find the innermost function containing the cursor.
    fn find_enclosing_func(self, db: &'db dyn SpannedHirDb, cursor: TextSize) -> Option<Func<'db>> {
        let items = self.find_enclosing_items(db, cursor);
        for item in items {
            match item {
                ItemKind::Func(func) => return Some(func),
                ItemKind::Body(body) => {
                    // Body is inside a function - get the parent func
                    if let Some(func) = body.containing_func(db) {
                        return Some(func);
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Find the reference at a cursor position anywhere in this module.
    pub fn reference_at(
        self,
        db: &'db dyn SpannedHirDb,
        cursor: TextSize,
    ) -> Option<&'db ReferenceView<'db>> {
        for item in self.find_enclosing_items(db, cursor) {
            if let Some(reference) = ScopeId::from_item(item).reference_at(db, cursor) {
                return Some(reference);
            }
        }
        None
    }

    /// Find the item definition at cursor position (cursor on name token).
    pub fn definition_at(
        self,
        db: &'db dyn SpannedHirDb,
        cursor: TextSize,
    ) -> Option<ScopeId<'db>> {
        for item in self.find_enclosing_items(db, cursor) {
            if let Some(name_span) = item.name_span()
                && let Some(resolved) = name_span.resolve(db)
                && resolved.range.contains(cursor)
            {
                return Some(ScopeId::from_item(item));
            }
        }
        None
    }

    /// Find items with the smallest span enclosing the cursor.
    pub fn find_enclosing_items(
        self,
        db: &'db dyn SpannedHirDb,
        cursor: TextSize,
    ) -> Vec<ItemKind<'db>> {
        let items = self.scope_graph(db).items_dfs(db);

        let mut smallest_items = Vec::new();
        let mut smallest_range_size = None;

        for item in items {
            let lazy_item_span = DynLazySpan::from(item.span());
            let Some(item_span) = lazy_item_span.resolve(db) else {
                continue;
            };

            if item_span.range.contains(cursor) {
                let range_size = item_span.range.end() - item_span.range.start();

                match smallest_range_size {
                    None => {
                        smallest_items.push(item);
                        smallest_range_size = Some(range_size);
                    }
                    Some(size) if range_size < size => {
                        smallest_items.clear();
                        smallest_items.push(item);
                        smallest_range_size = Some(range_size);
                    }
                    Some(size) if range_size == size => {
                        smallest_items.push(item);
                    }
                    _ => {}
                }
            }
        }

        smallest_items
    }

    /// Find all references to a target, returning ReferenceViews (for rename, etc.).
    pub fn references_to_target<DB>(
        self,
        db: &'db DB,
        target: &Target<'db>,
    ) -> Vec<&'db ReferenceView<'db>>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        match target {
            Target::Scope(scope) => {
                let mut results = Vec::new();
                for item in self.scope_graph(db).items_dfs(db) {
                    for reference in ScopeId::from_item(item).references(db) {
                        for t in reference.target(db).as_slice() {
                            if let Target::Scope(s) = t
                                && s == scope
                            {
                                results.push(reference);
                                break;
                            }
                        }
                    }
                }
                results
            }
            Target::Local { func, binding, .. } => {
                let (_, typed_body) = check_func_body(db, *func);
                let Some(body) = typed_body.body() else {
                    return vec![];
                };

                // Get the set of expression IDs that reference this binding
                let expr_ids: rustc_hash::FxHashSet<_> = typed_body
                    .references_by_binding(*binding)
                    .into_iter()
                    .collect();

                // Filter body references by their expression ID context
                // This avoids calling target() which triggers expensive path resolution
                body.references(db)
                    .iter()
                    .filter(|r| {
                        if let ReferenceView::Path(path_view) = r
                            && let Some(super::BodyPathContext::Expr(expr_id)) = path_view.body_ctx
                        {
                            expr_ids.contains(&expr_id)
                        } else {
                            false
                        }
                    })
                    .collect()
            }
        }
    }
}
