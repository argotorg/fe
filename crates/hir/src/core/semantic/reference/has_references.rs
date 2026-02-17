//! HasReferences trait and implementations.

use parser::TextSize;

use crate::{
    HirDb, SpannedHirDb,
    analysis::HirAnalysisDb,
    analysis::ty::ty_check::LocalBinding,
    hir_def::scope_graph::ScopeId,
    hir_def::{Body, Func, ItemKind, TopLevelMod},
    span::{DynLazySpan, LazySpan},
};

use super::resolve_path_with_recv_fallback;
use super::typed_body_for_body;
use super::{ReferenceView, Target};

use super::collector::{
    body_references, contract_references, enum_references, func_signature_references,
    impl_references, impl_trait_references, struct_references, trait_references,
    type_alias_references, use_references,
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
    /// Find the most specific reference at a cursor position within this scope.
    ///
    /// When references are nested (e.g. `C::InitArgs` inside
    /// `encoded_size<C::InitArgs>()`), selects the narrowest span containing
    /// the cursor so that goto-definition resolves the inner type argument
    /// rather than the outer function call.
    pub fn reference_at(
        self,
        db: &'db dyn SpannedHirDb,
        cursor: TextSize,
    ) -> Option<&'db ReferenceView<'db>> {
        self.references(db)
            .iter()
            .filter_map(|r| {
                let span = r.span().resolve(db)?;
                if span.range.contains(cursor) {
                    Some((r, span.range.len()))
                } else {
                    None
                }
            })
            .min_by_key(|(_, len)| *len)
            .map(|(r, _)| r)
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
            ItemKind::Contract(contract) => contract_references(db, *contract),
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
        if let Some(func) = self.find_enclosing_func(db, cursor) {
            return self.func_param_binding_at(db, func, cursor);
        }

        // Try contract init params
        if let Some(contract) = self.find_enclosing_contract(db, cursor) {
            return self.contract_init_param_binding_at(db, contract, cursor);
        }

        None
    }

    /// Check if cursor is on a function parameter name.
    fn func_param_binding_at<DB>(
        self,
        db: &'db DB,
        func: Func<'db>,
        cursor: TextSize,
    ) -> Option<Target<'db>>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        let body = func.body(db)?;
        let typed_body = typed_body_for_body(db, body)?;

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
                    body,
                    binding,
                });
            }
        }

        None
    }

    /// Check if cursor is on a contract init parameter name.
    fn contract_init_param_binding_at<DB>(
        self,
        db: &'db DB,
        contract: crate::hir_def::Contract<'db>,
        cursor: TextSize,
    ) -> Option<Target<'db>>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        let init = contract.init(db)?;
        let body = init.body(db);
        let typed_body = typed_body_for_body(db, body)?;

        // Check each init param's name span
        for (idx, _param) in init.params(db).data(db).iter().enumerate() {
            let name_span = contract.span().init_block().params().param(idx).name();

            if let Some(resolved) = name_span.resolve(db)
                && resolved.range.contains(cursor)
            {
                let binding = typed_body.param_binding(idx)?;
                let LocalBinding::Param { ty, .. } = binding else {
                    return None;
                };

                return Some(Target::Local {
                    span: name_span.into(),
                    ty,
                    body,
                    binding,
                });
            }
        }

        None
    }

    /// Find the innermost function containing the cursor.
    ///
    /// Skips desugared functions (e.g., encode/decode from msg blocks) because
    /// their spans overlap with other items (like the msg variant struct name)
    /// and would cause false binding matches.
    fn find_enclosing_func(self, db: &'db dyn SpannedHirDb, cursor: TextSize) -> Option<Func<'db>> {
        let items = self.find_enclosing_items(db, cursor);
        for item in items {
            match item {
                ItemKind::Func(func) => {
                    if matches!(func.origin(db), crate::span::HirOrigin::Desugared(_)) {
                        continue;
                    }
                    return Some(func);
                }
                ItemKind::Body(body) => {
                    if let Some(func) = body.containing_func(db) {
                        if matches!(func.origin(db), crate::span::HirOrigin::Desugared(_)) {
                            continue;
                        }
                        return Some(func);
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Find the innermost contract containing the cursor.
    fn find_enclosing_contract(
        self,
        db: &'db dyn SpannedHirDb,
        cursor: TextSize,
    ) -> Option<crate::hir_def::Contract<'db>> {
        let items = self.find_enclosing_items(db, cursor);
        for item in items {
            if let ItemKind::Contract(contract) = item {
                return Some(contract);
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
    ///
    /// Checks both item names and non-item children (variants, fields,
    /// generic params, trait types/consts) so that find-references works
    /// when the cursor is on e.g. an enum variant definition site.
    pub fn definition_at(
        self,
        db: &'db dyn SpannedHirDb,
        cursor: TextSize,
    ) -> Option<ScopeId<'db>> {
        for item in self.find_enclosing_items(db, cursor) {
            // Check the item's own name
            if let Some(name_span) = item.name_span()
                && let Some(resolved) = name_span.resolve(db)
                && resolved.range.contains(cursor)
            {
                return Some(ScopeId::from_item(item));
            }

            // Check variant and field children — these have precise name-only
            // spans.  Other non-item scopes (generic params, func params, trait
            // types/consts) have broader spans that may overlap with references
            // we'd rather resolve through reference_at.
            let scope_graph = self.scope_graph(db);
            for child in scope_graph.children(ScopeId::from_item(item)) {
                match child {
                    ScopeId::Variant(_) | ScopeId::Field(_, _) => {}
                    _ => continue,
                }
                if let Some(name_span) = child.name_span(db)
                    && let Some(resolved) = name_span.resolve(db)
                    && resolved.range.contains(cursor)
                {
                    return Some(child);
                }
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

    /// Find all references to a target, with segment-level precision for paths.
    ///
    /// For `Target::Scope`, resolves each segment of every path reference using
    /// HIR path resolution. This means `MyEnum::Variant` produces a match for
    /// both `MyEnum` and `Variant` at their respective segment spans. Non-path
    /// references (field access, method calls, use paths) use direct target
    /// matching.
    ///
    /// For `Target::Local`, filters body references by binding identity.
    ///
    /// Returns `MatchedReference` pairs so callers get both the original
    /// `ReferenceView` (for rename, self-ty checks) and the precise matched
    /// span.
    pub fn references_to_target<DB>(
        self,
        db: &'db DB,
        target: &Target<'db>,
    ) -> Vec<MatchedReference<'db>>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        match target {
            Target::Scope(scope) => {
                let mut results = Vec::new();
                for item in self.scope_graph(db).items_dfs(db) {
                    for reference in ScopeId::from_item(item).references(db) {
                        match reference {
                            ReferenceView::Path(pv) => {
                                // Resolve each segment independently via path
                                // resolution so that both MyEnum and Variant
                                // are found as references in MyEnum::Variant.
                                // Uses recv fallback for bare paths in recv arm
                                // patterns (e.g., `Mint` → `TokenMsg::Mint`).
                                let last_idx = pv.path.segment_index(db);
                                for idx in 0..=last_idx {
                                    if let Some(seg_path) = pv.path.segment(db, idx)
                                        && resolve_path_with_recv_fallback(db, seg_path, pv.scope)
                                            .contains(scope)
                                    {
                                        results.push(MatchedReference {
                                            view: reference,
                                            span: pv.span.clone().segment(idx).ident().into(),
                                        });
                                    }
                                }
                            }
                            _ => {
                                // Non-path references: direct target match
                                for t in reference.target(db).as_slice() {
                                    if let Target::Scope(s) = t
                                        && s == scope
                                    {
                                        results.push(MatchedReference {
                                            view: reference,
                                            span: reference.span(),
                                        });
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                results
            }
            Target::Local { body, binding, .. } => {
                let Some(typed_body) = typed_body_for_body(db, *body) else {
                    return vec![];
                };

                // Get the set of expression IDs that reference this binding
                let expr_ids: rustc_hash::FxHashSet<_> = typed_body
                    .references_by_binding(*binding)
                    .into_iter()
                    .collect();

                // Filter body references by their expression ID context
                body.references(db)
                    .iter()
                    .filter_map(|r| {
                        if let ReferenceView::Path(path_view) = r
                            && let Some(super::BodyPathContext::Expr(expr_id)) = path_view.body_ctx
                            && expr_ids.contains(&expr_id)
                        {
                            Some(MatchedReference {
                                view: r,
                                span: r.span(),
                            })
                        } else {
                            None
                        }
                    })
                    .collect()
            }
        }
    }
}

/// A reference matched against a search target, with the precise span
/// of the matched portion.
pub struct MatchedReference<'db> {
    /// The original reference view (for rename, self-ty checks, etc.).
    pub view: &'db ReferenceView<'db>,
    /// The span of the matched portion — segment-level for path segment
    /// matches, or the full reference span for direct matches.
    pub span: DynLazySpan<'db>,
}
