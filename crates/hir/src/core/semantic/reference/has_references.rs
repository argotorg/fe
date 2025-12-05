//! HasReferences trait and implementations.
//!
//! This module provides the unified interface for accessing references
//! in different parts of the HIR (bodies, items, scopes).

use parser::TextSize;

use crate::{
    HirDb, SpannedHirDb,
    analysis::HirAnalysisDb,
    analysis::ty::ty_check::check_func_body,
    hir_def::scope_graph::ScopeId,
    hir_def::{Body, ItemKind, TopLevelMod},
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
    /// Find the reference at a cursor position anywhere in this module.
    ///
    /// This is the primary entry point for goto-definition - it finds the
    /// smallest enclosing item containing the cursor and returns the reference
    /// at that position.
    pub fn reference_at(
        self,
        db: &'db dyn SpannedHirDb,
        cursor: TextSize,
    ) -> Option<&'db ReferenceView<'db>> {
        let items = self.find_enclosing_items(db, cursor);

        // When multiple items have the same span (e.g., decomposed use statements),
        // check each one's references to find which actually contains the cursor
        for item in items {
            let scope = ScopeId::from_item(item);
            if let Some(reference) = scope.reference_at(db, cursor) {
                return Some(reference);
            }
        }

        None
    }

    /// Find the item definition at cursor position.
    ///
    /// Returns the ScopeId if the cursor is on an item's name token in its definition.
    /// This is useful for find-all-references and similar operations that need to
    /// identify when the cursor is on a definition rather than a reference.
    ///
    /// # Example
    /// ```fe
    /// pub trait Hasher { // cursor on "Hasher" returns Some(ScopeId::Trait)
    ///     fn hash() -> u256
    /// }
    /// ```
    pub fn definition_at(
        self,
        db: &'db dyn SpannedHirDb,
        cursor: TextSize,
    ) -> Option<ScopeId<'db>> {
        let items = self.find_enclosing_items(db, cursor);

        for item in items {
            if let Some(name_span) = item.name_span()
                && let Some(resolved) = name_span.resolve(db)
                && resolved.range.contains(cursor)
            {
                return Some(ScopeId::from_item(item));
            }
        }

        None
    }

    /// Find all items with the smallest span enclosing the cursor position.
    ///
    /// Returns all items that share the smallest enclosing span. This handles
    /// cases like decomposed use statements where multiple items have identical spans.
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
                        // Found a smaller item
                        smallest_items.clear();
                        smallest_items.push(item);
                        smallest_range_size = Some(range_size);
                    }
                    Some(size) if range_size == size => {
                        // Multiple items with same size (e.g., decomposed uses)
                        smallest_items.push(item);
                    }
                    _ => {}
                }
            }
        }

        smallest_items
    }

    /// Find all references to a given scope within this module.
    ///
    /// This is the primary entry point for find-all-references - it collects
    /// all references across the module that resolve to the target scope.
    pub fn references_to(
        self,
        db: &'db dyn HirAnalysisDb,
        target: ScopeId<'db>,
    ) -> Vec<&'db ReferenceView<'db>> {
        self.references_to_target(db, Target::Scope(target))
    }

    /// Find all references to a target (scope or local) within this module.
    ///
    /// For scopes, this searches all items in the module.
    /// For locals, this searches only within the containing function's body,
    /// since locals are only visible within their function scope.
    pub fn references_to_target(
        self,
        db: &'db dyn HirAnalysisDb,
        target: Target<'db>,
    ) -> Vec<&'db ReferenceView<'db>> {
        match target {
            Target::Scope(scope) => {
                let items = self.scope_graph(db).items_dfs(db);
                let mut results = Vec::new();

                for item in items {
                    let item_scope = ScopeId::from_item(item);
                    for reference in item_scope.references(db) {
                        // Check if this reference resolves to our target scope
                        // For ambiguous references, we check if any candidate matches
                        let resolution = reference.target(db);
                        for target in resolution.as_slice() {
                            if let Target::Scope(s) = target
                                && *s == scope
                            {
                                results.push(reference);
                                break;
                            }
                        }
                    }
                }

                results
            }
            Target::Local { func, expr, .. } => {
                // For locals, search only within the containing function's body
                let (_, typed_body) = check_func_body(db, func);
                let Some(body) = typed_body.body() else {
                    return vec![];
                };

                // Get all expressions that reference the same local binding
                let expr_ids = typed_body.local_references(expr);

                // Get references from the function's body (not the func scope, which only has signature refs)
                let body_refs = body.references(db);

                body_refs
                    .iter()
                    .filter(|r| {
                        // Match references that resolve to the same local binding
                        // Locals are never ambiguous, so we just check the first target
                        if let Some(Target::Local {
                            expr: ref_expr,
                            func: ref_func,
                            ..
                        }) = r.target(db).first()
                        {
                            *ref_func == func && expr_ids.contains(ref_expr)
                        } else {
                            false
                        }
                    })
                    .collect()
            }
        }
    }
}
