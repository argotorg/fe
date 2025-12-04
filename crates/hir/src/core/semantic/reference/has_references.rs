//! HasReferences trait and implementations.
//!
//! This module provides the unified interface for accessing references
//! in different parts of the HIR (bodies, items, scopes).

use parser::TextSize;

use crate::{
    HirDb, SpannedHirDb,
    analysis::HirAnalysisDb,
    hir_def::{Body, ItemKind, TopLevelMod},
    hir_def::scope_graph::ScopeId,
    span::{DynLazySpan, LazySpan},
};

use super::{ReferenceView, Target};
use super::collector::{
    body_references, func_signature_references, struct_references, enum_references,
    type_alias_references, impl_references, trait_references, impl_trait_references, use_references,
};

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
    pub fn reference_at(self, db: &'db dyn SpannedHirDb, cursor: TextSize) -> Option<&'db ReferenceView<'db>> {
        self.references(db)
            .iter()
            .find(|r| r.span().resolve(db).is_some_and(|s| s.range.contains(cursor)))
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
    pub fn reference_at(self, db: &'db dyn SpannedHirDb, cursor: TextSize) -> Option<&'db ReferenceView<'db>> {
        // Find the smallest item containing the cursor
        let item = self.find_enclosing_item(db, cursor)?;
        let scope = ScopeId::from_item(item);
        scope.reference_at(db, cursor)
    }

    /// Find the smallest item enclosing the cursor position.
    fn find_enclosing_item(self, db: &'db dyn SpannedHirDb, cursor: TextSize) -> Option<ItemKind<'db>> {
        let items = self.scope_graph(db).items_dfs(db);

        let mut smallest_enclosing_item = None;
        let mut smallest_range_size = None;

        for item in items {
            let lazy_item_span = DynLazySpan::from(item.span());
            let Some(item_span) = lazy_item_span.resolve(db) else {
                continue;
            };

            if item_span.range.contains(cursor) {
                let range_size = item_span.range.end() - item_span.range.start();
                if smallest_range_size.is_none() || range_size < smallest_range_size.unwrap() {
                    smallest_enclosing_item = Some(item);
                    smallest_range_size = Some(range_size);
                }
            }
        }

        smallest_enclosing_item
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
        let items = self.scope_graph(db).items_dfs(db);
        let mut results = Vec::new();

        for item in items {
            let scope = ScopeId::from_item(item);
            for reference in scope.references(db) {
                // Check if this reference resolves to our target
                if let Some(resolved) = reference.target(db) {
                    let matches = match resolved {
                        Target::Scope(s) => s == target,
                        Target::Local { .. } => false, // Local bindings don't match scope targets
                    };
                    if matches {
                        results.push(reference);
                    }
                }
            }
        }

        results
    }
}
