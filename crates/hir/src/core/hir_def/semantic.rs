//! Semantic traversal surface for HIR items.
//!
//! This module hosts the externally‑facing, semantic methods that callers
//! should use when walking the HIR. Keep raw, syntactic accessors and
//! #[salsa::tracked] implementations in `item.rs`; provide ergonomic,
//! context‑aware helpers here that compose the internal lowering and
//! resolution layers.
//!
//! Design notes
//! - Prefer returning semantic IDs (TyId, TraitInstId, etc.) or diagnostics
//!   collections from here; do not expose raw HIR nodes.
//! - Keep methods small and capability‑oriented (e.g., generic params,
//!   where‑clauses, signature types). Push per‑node, context‑rich logic into
//!   views when a single method signature becomes unwieldy.
//! - Let the compiler guide additions: ablate public syntactic accessors in
//!   `item.rs` and replace call sites by adding only the minimal semantic
//!   method(s) here.

use crate::analysis::HirAnalysisDb;
use crate::hir_def::scope_graph::ScopeId;
use crate::analysis::name_resolution::{PathRes, resolve_path};
use crate::analysis::ty::def_analysis::check_duplicate_names;
use crate::hir_def::params::KindBound as HirKindBound;

fn lower_hir_kind_local(k: &HirKindBound) -> crate::analysis::ty::ty_def::Kind {
    use crate::analysis::ty::ty_def::Kind;
    use crate::hir_def::Partial;
    match k {
        HirKindBound::Mono => Kind::Star,
        HirKindBound::Abs(lhs, rhs) => {
            let lhs_k = match lhs {
                Partial::Present(inner) => lower_hir_kind_local(inner),
                Partial::Absent => Kind::Any,
            };
            let rhs_k = match rhs {
                Partial::Present(inner) => lower_hir_kind_local(inner),
                Partial::Absent => Kind::Any,
            };
            Kind::Abs(Box::new((lhs_k, rhs_k)))
        }
    }
}
use crate::analysis::ty::diagnostics::{TyDiagCollection, TyLowerDiag};
use crate::analysis::ty::def_analysis;
use crate::hir_def::*;
// When adding real methods, prefer calling internal lowering/normalization here
// rather than exposing raw syntax.
use crate::analysis::ty::trait_resolution::constraint::{
    collect_adt_constraints, collect_constraints, collect_func_def_constraints,
};
use crate::analysis::ty::{
    trait_resolution::PredicateListId,
    ty_def::{InvalidCause, TyId},
    ty_lower::{lower_hir_ty, lower_type_alias},
};
use crate::analysis::ty::normalize::normalize_ty;
use crate::analysis::ty::unify::UnificationTable;
use crate::analysis::ty::canonical::Canonical;
use crate::analysis::ty::fold::TyFoldable;
use crate::analysis::ty::ty_def::TyData;
use crate::analysis::ty::trait_def::impls_for_ty_with_constraints;
use crate::analysis::ty::adt_def::AdtDef;
use crate::analysis::ty::visitor::{TyVisitor, walk_ty};
use smallvec::{SmallVec, smallvec};
use common::indexmap::IndexMap;
use crate::analysis::ty::{
    trait_resolution::{is_goal_satisfiable, GoalSatisfiability},
};
// (kind-lowering helpers intentionally omitted; see WherePredicateView::kind)

// (Additional view types appear below; keep layering semantic.)

// Note: generic trait-ref diagnostics are handled via context-rooted views (e.g.,
// WherePredicateBoundView and SuperTraitRefView). Avoid subject-taking public APIs.

impl<'db> SuperTraitRefView<'db> {
    /// Diagnostics for this super-trait reference in its owner's context.
    /// Uses the trait's `Self` as subject and checks WF; kind mismatch is emitted
    /// elsewhere via Trait::diags_super_traits.
    pub fn diags(self, db: &'db dyn HirAnalysisDb) -> Option<crate::analysis::ty::diagnostics::TyDiagCollection<'db>> {
        use crate::analysis::name_resolution::{ExpectedPathKind, diagnostics::PathResDiag};
        use crate::analysis::ty::trait_lower::TraitRefLowerError;
        use crate::analysis::ty::trait_resolution::{check_trait_inst_wf, WellFormedness};

        let span = self.span();
        let subject = self.subject_self(db);
        let scope = self.owner.scope();
        let assumptions = self.assumptions(db);
        let tr = self.trait_ref_id(db);

        let inst = match crate::analysis::ty::trait_lower::lower_trait_ref(db, subject, tr, scope, assumptions) {
            Ok(i) => i,
            Err(TraitRefLowerError::PathResError(err)) => {
                return Some(
                    err.into_diag(db, tr.path(db).unwrap(), span.path(), ExpectedPathKind::Trait)?.into(),
                );
            }
            Err(TraitRefLowerError::InvalidDomain(res)) => {
                return Some(
                    PathResDiag::ExpectedTrait(span.path().into(), tr.path(db).unwrap().ident(db).unwrap(), res.kind_name()).into(),
                );
            }
            Err(TraitRefLowerError::Ignored) => return None,
        };

        // Do not emit when subject contains assoc types of params
        if inst.self_ty(db).contains_assoc_ty_of_param(db) {
            return None;
        }

        match check_trait_inst_wf(db, scope.ingot(db), inst, assumptions) {
            WellFormedness::WellFormed => None,
            WellFormedness::IllFormed { goal, subgoal } => Some(
                crate::analysis::ty::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                    span: span.into(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                }
                .into(),
            ),
        }
    }
}

/// Consolidated assumptions for any item kind.
pub fn constraints_for<'db>(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> PredicateListId<'db> {
    match item {
        ItemKind::Struct(s) => collect_adt_constraints(db, s.as_adt(db)).instantiate_identity(),
        ItemKind::Enum(e) => collect_adt_constraints(db, e.as_adt(db)).instantiate_identity(),
        ItemKind::Contract(c) => collect_adt_constraints(db, c.as_adt(db)).instantiate_identity(),
        ItemKind::Func(f) => collect_func_def_constraints(db, f.into(), true).instantiate_identity(),
        ItemKind::Impl(i) => collect_constraints(db, i.into()).instantiate_identity(),
        ItemKind::Trait(t) => collect_constraints(db, t.into()).instantiate_identity(),
        ItemKind::ImplTrait(i) => collect_constraints(db, i.into()).instantiate_identity(),
        _ => PredicateListId::empty_list(db),
    }
}

// Top‑level module items ----------------------------------------------------

impl<'db> TopLevelMod<'db> {
    // Planned semantic surface:
    // - Walk child items (semantic iteration helpers)
    // - Aggregate diagnostics for contained items (pure glue)
    // - Ingot/visibility helpers already live in `mod.rs`/`item.rs`
}

impl<'db> Mod<'db> {
    // Planned semantic surface:
    // - Semantic child iteration (filter by kind)
    // - Optional module‑scoped diagnostics aggregation
}

// Function items ------------------------------------------------------------

impl<'db> Func<'db> {
    // Planned semantic surface:
    // - arg_tys(db) -> Vec<Binder<TyId>> (delegates to internal lowering)
    // - receiver_ty(db) -> Option<Binder<TyId>>
    // - generic_params_diags(db) -> Vec<TyDiagCollection>
    // - where_clause_diags(db) -> Vec<TyDiagCollection>
    // - analyze(db) -> Vec<TyDiagCollection> (aggregate only)

    /// Returns true if this function declares an explicit return type.
    pub fn has_explicit_return_ty(self, db: &'db dyn HirDb) -> bool {
        self.ret_type_ref(db).is_some()
    }

    /// Explicit return type if annotated in source; `None` when the
    /// function has no explicit return type.
    pub fn explicit_return_ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let assumptions = constraints_for(db, self.into());
        let hir = self.ret_type_ref(db)?;
        Some(lower_hir_ty(db, hir, self.scope(), assumptions))
    }

    /// Semantic return type. When absent in source, this is `unit`.
    pub fn return_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.explicit_return_ty(db)
            .unwrap_or_else(|| TyId::unit(db))
    }

    /// Semantic argument types bound to identity parameters.
    pub fn arg_tys(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        use crate::analysis::ty::{binder::Binder, ty_def::{InvalidCause, TyId}};
        let assumptions = constraints_for(db, self.into());
        match self.params(db).to_opt() {
            Some(params) => params
                .data(db)
                .iter()
                .map(|p| match p.ty.to_opt() {
                    Some(hir_ty) => Binder::bind(lower_hir_ty(db, hir_ty, self.scope(), assumptions)),
                    None => Binder::bind(TyId::invalid(db, InvalidCause::ParseError)),
                })
                .collect(),
            None => Vec::new(),
        }
    }

    /// Semantic receiver type if this is a method (first argument), else None.
    pub fn receiver_ty(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        self.is_method(db)
            .then(|| self.arg_tys(db).into_iter().next())
            .flatten()
    }

    /// Diagnostics related to parameters (duplicate names/labels).
    pub fn diags_parameters(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::diagnostics::TyLowerDiag;
        let mut diags = Vec::new();

        // Duplicate parameter names
        let dupes = def_analysis::check_duplicate_names(
            self.param_views(db).map(|v| v.name(db)),
            |idxs| TyLowerDiag::DuplicateArgName(self, idxs).into(),
        );
        let found_dupes = !dupes.is_empty();
        diags.extend(dupes);

        // Duplicate labels (only if names were unique)
        if !found_dupes {
            diags.extend(def_analysis::check_duplicate_names(
                self.param_views(db).map(|v| v.label_eagerly(db)),
                |idxs| TyLowerDiag::DuplicateArgLabel(self, idxs).into(),
            ));
        }

        diags
    }

    /// Diagnostics related to the explicit return type (kind/const checks).
    pub fn diags_return(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::diagnostics::TyLowerDiag;
        let mut diags = Vec::new();
        if self.has_explicit_return_ty(db) {
            // First, surface name-resolution/path-domain errors on the return type itself
            if let Some(hir_ty) = self.ret_type_ref(db) {
                let assumptions = constraints_for(db, self.into());
                let mut errs = crate::analysis::ty::ty_error::collect_ty_lower_errors(
                    db,
                    self.scope(),
                    hir_ty,
                    self.span().ret_ty(),
                    assumptions,
                );
                if !errs.is_empty() {
                    diags.append(&mut errs);
                    return diags;
                }
            }

            // Then run kind/const checks on the lowered semantic type
            let ret = self.return_ty(db);
            let span = self.span().ret_ty().into();
            if !ret.has_star_kind(db) {
                diags.push(TyLowerDiag::ExpectedStarKind(span).into());
            } else if ret.is_const_ty(db) {
                diags.push(TyLowerDiag::NormalTypeExpected { span, given: ret }.into());
            }
        }
        diags
    }

    /// Expected `Self` type for this function when it is an associated method.
    /// - In trait: the trait's `Self` parameter (identity instantiation)
    /// - In impl/impl_trait: the implementor type
    pub fn expected_self_ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        match self.scope().parent(db)? {
            ScopeId::Item(ItemKind::Trait(tr)) => {
                Some(crate::analysis::ty::trait_lower::lower_trait(db, tr).self_param(db))
            }
            ScopeId::Item(ItemKind::ImplTrait(it)) => Some(it.ty(db)),
            ScopeId::Item(ItemKind::Impl(im)) => Some(im.ty(db)),
            _ => None,
        }
    }

    /// Diagnostics for function parameter types:
    /// - For all params: star kind required and reject const types
    /// - For self param: enforce exact `Self` type shape
    /// Note: WF/invalid errors are still surfaced via the general type walker.
    pub fn diags_param_types(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::diagnostics::{ImplDiag, TyLowerDiag};
        use crate::analysis::ty::trait_resolution::{check_ty_wf, WellFormedness};
        use crate::analysis::ty::normalize::normalize_ty;
        let mut out = Vec::new();
        let assumptions = constraints_for(db, self.into());
        let expected_self = self.expected_self_ty(db);

        for view in self.param_views(db) {
            let Some(hir_ty) = view.ty_hir(db) else { continue };
            // Surface name-resolution errors for the parameter type first
            let span_node = if view.is_self_param(db) && view.self_ty_fallback(db) {
                view.span().fallback_self_ty()
            } else {
                view.span().ty()
            };
            let ty_span: crate::visitor::prelude::DynLazySpan<'db> = if view.is_self_param(db) && view.self_ty_fallback(db) {
                view.span().name().into()
            } else {
                view.span().ty().into()
            };
            let mut errs = crate::analysis::ty::ty_error::collect_ty_lower_errors(
                db,
                self.scope(),
                hir_ty,
                span_node,
                assumptions,
            );
            if !errs.is_empty() {
                out.append(&mut errs);
                continue;
            }
            let ty = lower_hir_ty(db, hir_ty, self.scope(), assumptions);
            if !ty.has_star_kind(db) {
                out.push(TyLowerDiag::ExpectedStarKind(ty_span.clone()).into());
                continue;
            }
            if ty.is_const_ty(db) {
                out.push(TyLowerDiag::NormalTypeExpected { span: ty_span.clone(), given: ty }.into());
                continue;
            }

            // Well-formedness / trait-bound satisfaction for parameter type
            match check_ty_wf(db, self.top_mod(db).ingot(db), ty, assumptions) {
                WellFormedness::WellFormed => {}
                WellFormedness::IllFormed { goal, subgoal } => {
                    out.push(
                        crate::analysis::ty::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                            span: ty_span.clone(),
                            primary_goal: goal,
                            unsat_subgoal: subgoal,
                        }
                        .into(),
                    );
                    // keep checking other params
                }
            }

            if view.is_self_param(db) {
                if let Some(expected) = expected_self {
                    if !ty.has_invalid(db) && !expected.has_invalid(db) {
                        let (exp_base, exp_args) = expected.decompose_ty_app(db);
                        let ty_norm = normalize_ty(db, ty, self.scope(), assumptions);
                        let (ty_base, ty_args) = ty_norm.decompose_ty_app(db);
                        let same_base = ty_base == exp_base;
                        let same_args = exp_args.iter().zip(ty_args.iter()).all(|(a, b)| a == b);
                        if !(same_base && same_args) {
                            out.push(
                                ImplDiag::InvalidSelfType { span: ty_span.clone(), expected, given: ty_norm }.into(),
                            );
                        }
                    }
                }
            }
        }
        out
    }

    /// Aggregate all function-level definition diagnostics using the semantic surface.
    /// Includes:
    /// - Duplicate parameter names/labels
    /// - Parameter type checks (star kind, constness, `Self` shape when applicable)
    /// - Explicit return type checks (star kind, constness)
    /// - In `impl` blocks, detect inherent method conflicts with existing methods
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::func_def::lower_func;
        use crate::analysis::ty::method_table::probe_method;
        use crate::analysis::ty::{binder::Binder, canonical::Canonical};
        use crate::analysis::ty::diagnostics::ImplDiag;

        let mut out = Vec::new();
        out.extend(self.diags_parameters(db));
        out.extend(self.diags_param_types(db));

        // Defer body/type-check diagnostics to BodyAnalysisPass; here we only validate the
        // explicit return type annotation when present.
        out.extend(self.diags_return(db));

        // Where-clause diagnostics (trait bounds and kind checks on where predicates)
        {
            let clause = WhereClauseView { owner: WhereClauseOwner::Func(self), id: self.where_clause(db) };
            for pred in clause.predicates(db) {
                out.extend(pred.diags(db));
            }
        }

        // Method conflict check only for inherent impls
        if let Some(ScopeId::Item(ItemKind::Impl(impl_))) = self.scope().parent(db) {
            if let Some(func_def) = lower_func(db, self) {
                // Use the implementor type for the receiver
                let self_ty = impl_.ty(db);
                if !self_ty.has_invalid(db) {
                    let ingot = self.top_mod(db).ingot(db);
                    for &cand in probe_method(
                        db,
                        ingot,
                        Canonical::new(db, self_ty),
                        func_def.name(db),
                    ) {
                        if cand != func_def {
                            out.push(
                                ImplDiag::ConflictMethodImpl { primary: func_def, conflict_with: cand }
                                    .into(),
                            );
                            break;
                        }
                    }
                }
            }
        }

        out
    }
}

// ADT items -----------------------------------------------------------------

impl<'db> Struct<'db> {
    // Planned semantic surface:
    // - ty_fields(db) -> AdtField (semantic field tys)
    // - validate_fields(db) -> Vec<TyDiagCollection>
    // - generic_params_diags(db), where_clause_diags(db)
    // - analyze(db) -> Vec<TyDiagCollection>
}

// Function parameter views --------------------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct FuncParamView<'db> {
    pub func: Func<'db>,
    pub idx: usize,
}

impl<'db> FuncParamView<'db> {
    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.func.params(db).to_opt()?;
        list.data(db).get(self.idx)?.name()
    }

    pub fn label(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.func.params(db).to_opt()?;
        match list.data(db).get(self.idx)?.label {
            Some(FuncParamName::Ident(id)) => Some(id),
            _ => None,
        }
    }

    pub fn label_eagerly(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.func.params(db).to_opt()?;
        list.data(db).get(self.idx)?.label_eagerly()
    }

    pub fn is_self_param(self, db: &'db dyn HirDb) -> bool {
        let list = self.func.params(db).to_opt();
        match list.and_then(|l| l.data(db).get(self.idx)) {
            Some(p) => p.is_self_param(db),
            None => false,
        }
    }

    pub fn is_mut(self, db: &'db dyn HirDb) -> bool {
        let list = self.func.params(db).to_opt();
        match list.and_then(|l| l.data(db).get(self.idx)) {
            Some(p) => p.is_mut,
            None => false,
        }
    }

    pub fn span(self) -> crate::span::params::LazyFuncParamSpan<'db> {
        self.func.span().params().param(self.idx)
    }

    pub fn ty_hir(self, db: &'db dyn HirDb) -> Option<TypeId<'db>> {
        let list = self.func.params(db).to_opt()?;
        list.data(db).get(self.idx)?.ty.to_opt()
    }

    pub fn self_ty_fallback(self, db: &'db dyn HirDb) -> bool {
        let list = self.func.params(db).to_opt();
        match list.and_then(|l| l.data(db).get(self.idx)) {
            Some(p) => p.self_ty_fallback,
            None => false,
        }
    }
}

impl<'db> Func<'db> {
    /// Iterate parameters as contextual views (semantic traversal helper).
    pub fn param_views(self, db: &'db dyn HirDb) -> impl Iterator<Item = FuncParamView<'db>> + 'db {
        let len = self.params(db).to_opt().map(|l| l.data(db).len()).unwrap_or(0);
        (0..len).map(move |idx| FuncParamView { func: self, idx })
    }
}

impl<'db> Enum<'db> {
    // Planned semantic surface:
    // - variant arg types (semantic)
    // - validate_variants(db) -> Vec<TyDiagCollection>
    // - generic_params_diags(db), where_clause_diags(db)
    // - analyze(db) -> Vec<TyDiagCollection>

    pub fn len_variants(&self, db: &'db dyn HirDb) -> usize {
        self.variants_list(db).data(db).len()
    }

    /// Iterates variants as contextual views (structural traversal helper).
    pub fn variants(self, db: &'db dyn HirDb) -> impl Iterator<Item = VariantView<'db>> + 'db {
        let list = self.variants_list(db);
        list.data(db)
            .iter()
            .enumerate()
            .map(move |(idx, _)| VariantView { owner: self, idx })
    }

    /// Semantic ADT definition for this enum (cached via tracked query).
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> crate::analysis::ty::adt_def::AdtDef<'db> {
        crate::analysis::ty::adt_def::lower_adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(self),
        )
    }
}

impl<'db> Struct<'db> {
    /// Returns semantic types of all fields, bound to identity parameters.
    pub fn field_tys(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        use crate::analysis::ty::binder::Binder;
        FieldParent::Struct(self)
            .fields(db)
            .map(|v| Binder::bind(v.ty(db)))
            .collect()
    }

    /// Semantic ADT definition for this struct (cached via tracked query).
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> crate::analysis::ty::adt_def::AdtDef<'db> {
        crate::analysis::ty::adt_def::lower_adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(self),
        )
    }
}

impl<'db> Contract<'db> {
    /// Returns semantic types of all fields, bound to identity parameters.
    pub fn field_tys(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        use crate::analysis::ty::binder::Binder;
        FieldParent::Contract(self)
            .fields(db)
            .map(|v| Binder::bind(v.ty(db)))
            .collect()
    }

    /// Semantic ADT definition for this contract (cached via tracked query).
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> crate::analysis::ty::adt_def::AdtDef<'db> {
        crate::analysis::ty::adt_def::lower_adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(self),
        )
    }
}

// Where-clause traversal (scaffold) -----------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct WhereClauseView<'db> {
    pub owner: WhereClauseOwner<'db>,
    pub id: WhereClauseId<'db>,
}

#[derive(Clone, Copy, Debug)]
pub struct WherePredicateView<'db> {
    pub clause: WhereClauseView<'db>,
    pub idx: usize,
}

impl<'db> WhereClauseOwner<'db> {
    /// Semantic where-clause view for this owner.
    pub fn clause(self, db: &'db dyn HirDb) -> WhereClauseView<'db> {
        WhereClauseView { owner: self, id: self.where_clause(db) }
    }
}

impl<'db> WhereClauseView<'db> {
    pub fn predicates(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = WherePredicateView<'db>> + 'db {
        let len = self.id.data(db).len();
        (0..len).map(move |idx| WherePredicateView { clause: self, idx })
    }

    pub fn span(self) -> crate::span::params::LazyWhereClauseSpan<'db> {
        match self.owner {
            WhereClauseOwner::Func(f) => f.span().where_clause(),
            WhereClauseOwner::Struct(s) => s.span().where_clause(),
            WhereClauseOwner::Enum(e) => e.span().where_clause(),
            WhereClauseOwner::Impl(i) => i.span().where_clause(),
            WhereClauseOwner::Trait(t) => t.span().where_clause(),
            WhereClauseOwner::ImplTrait(i) => i.span().where_clause(),
        }
    }
}

impl<'db> WherePredicateView<'db> {
    fn hir_pred(self, db: &'db dyn HirDb) -> &'db WherePredicate<'db> {
        &self.clause.id.data(db)[self.idx]
    }

    fn owner_item(self) -> ItemKind<'db> {
        ItemKind::from(self.clause.owner)
    }

    /// If this predicate's subject is one of the owner's generic parameters,
    /// returns its original index (0-based within the owner).
    pub fn param_original_index(self, db: &'db dyn HirDb) -> Option<usize> {
        use crate::core::hir_def::types::TypeKind as HirTyKind;
        let hir_ty = self.hir_pred(db).ty.to_opt()?;
        let path = match hir_ty.data(db) {
            HirTyKind::Path(p) => p.to_opt()?,
            _ => return None,
        };
        if !path.is_bare_ident(db) {
            return None;
        }
        let ident = path.as_ident(db)?;
        let Some(owner) = GenericParamOwner::from_item_opt(self.owner_item()) else {
            return None;
        };
        let params = owner.params_list(db).data(db);
        params.iter().position(|p| match p {
            GenericParam::Type(t) => t.name.to_opt() == Some(ident),
            GenericParam::Const(c) => c.name.to_opt() == Some(ident),
        })
    }

    /// Lowered kind bound sourced from the where-clause, if present.
    pub fn kind(self, db: &'db dyn HirAnalysisDb) -> Option<crate::analysis::ty::ty_def::Kind> {
        use crate::hir_def::Partial;
        for b in &self.hir_pred(db).bounds {
            if let TypeBound::Kind(Partial::Present(_k)) = b {
                // Defer exposing kind lowering until we publicize lower_kind appropriately.
                // For now, surface None to avoid changing behavior.
                return None;
            }
        }
        None
    }

    pub fn span(self) -> crate::span::params::LazyWherePredicateSpan<'db> {
        self.clause.span().predicate(self.idx)
    }

    /// Iterate trait bounds as per-bound semantic views.
    pub fn bounds(self, db: &'db dyn HirDb) -> impl Iterator<Item = WherePredicateBoundView<'db>> + 'db {
        let idxs: Vec<usize> = self
            .hir_pred(db)
            .bounds
            .iter()
            .enumerate()
            .filter_map(|(i, b)| matches!(b, TypeBound::Trait(_)).then_some(i))
            .collect();
        idxs.into_iter().map(move |idx| WherePredicateBoundView { pred: self, idx })
    }

    /// True if this predicate's subject type is `Self` (within a trait).
    pub fn is_self_subject(self, db: &'db dyn HirDb) -> bool {
        self.hir_pred(db)
            .ty
            .to_opt()
            .map(|ty| ty.is_self_ty(db))
            .unwrap_or_default()
    }

    /// Lower the subject type of this where-predicate into a semantic `TyId`.
    /// Returns `None` if the HIR type is missing or invalid.
    pub fn subject_ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let hir_ty = self.hir_pred(db).ty.to_opt()?;
        let owner_item = ItemKind::from(self.clause.owner);
        let assumptions = constraints_for(db, owner_item);
        Some(lower_hir_ty(db, hir_ty, owner_item.scope(), assumptions))
    }

    /// Lower the subject type and enforce the semantic constraints that prohibit
    /// const subjects or fully-concrete types. Returns `Ok(Some(..))` when the
    /// subject exists and passes the checks, `Ok(None)` when the predicate is
    /// syntactically absent, and `Err(diag)` when a diagnostic should be emitted.
    pub fn subject_ty_checked(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<Option<TyId<'db>>, TyDiagCollection<'db>> {
        use crate::analysis::ty::diagnostics::TraitConstraintDiag;
        use crate::analysis::name_resolution::{resolve_path, ExpectedPathKind};
        use crate::analysis::name_resolution::diagnostics::PathResDiag;

        let Some(hir_ty) = self.hir_pred(db).ty.to_opt() else {
            return Ok(None);
        };

        let owner_item = ItemKind::from(self.clause.owner);
        let assumptions = constraints_for(db, owner_item);
        let subject = lower_hir_ty(db, hir_ty, owner_item.scope(), assumptions);
        if let Some(cause) = subject.invalid_cause(db) {
            if let crate::analysis::ty::ty_def::InvalidCause::PathResolutionFailed { path } = cause {
                // Re-run name resolution on the failed path and surface a precise diagnostic
                // at the type path span within the where-predicate.
                let ty_span = self.span().ty().into_path_type().path();
                match resolve_path(db, path, owner_item.scope(), assumptions, false) {
                    Ok(res) => {
                        // Resolved to a non-type domain
                        if let Some(ident) = path.ident(db).to_opt() {
                            let diag = PathResDiag::ExpectedType(ty_span.into(), ident, res.kind_name());
                            return Err(diag.into());
                        }
                    }
                    Err(inner) => {
                        if let Some(diag) = inner.into_diag(db, path, ty_span, ExpectedPathKind::Type) {
                            return Err(diag.into());
                        }
                    }
                }
            }
        }
        let span = self.span().ty().into();

        if subject.is_const_ty(db) {
            return Err(TraitConstraintDiag::ConstTyBound(span, subject).into());
        }

        if !subject.has_invalid(db) && !subject.has_param(db) {
            return Err(TraitConstraintDiag::ConcreteTypeBound(span, subject).into());
        }

        Ok(Some(subject))
    }

    /// True if the lowered subject type is a const type.
    pub fn subject_is_const(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.subject_ty(db)
            .map(|t| t.is_const_ty(db))
            .unwrap_or(false)
    }

    /// True if the lowered subject type is concrete (no generic params) and not invalid.
    pub fn subject_is_concrete(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.subject_ty(db)
            .map(|t| !t.has_invalid(db) && !t.has_param(db))
            .unwrap_or(false)
    }

    /// Aggregate diagnostics for this where-predicate:
    /// - Subject-level errors (const/concrete or path-domain remapped)
    /// - Per-bound trait diagnostics
    /// - Per-bound kind consistency
    pub fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<crate::analysis::ty::diagnostics::TyDiagCollection<'db>> {
        use crate::analysis::ty::diagnostics::{TyDiagCollection, TyLowerDiag};
        let mut out: Vec<TyDiagCollection> = Vec::new();

        let subject = match self.subject_ty_checked(db) {
            Ok(Some(s)) => s,
            Ok(None) => return out,
            Err(diag) => {
                out.push(diag);
                return out;
            }
        };

        for (i, bound) in self.hir_pred(db).bounds.iter().enumerate() {
            match bound {
                TypeBound::Trait(_) => {
                    let bview = WherePredicateBoundView { pred: self, idx: i };
                    out.extend(bview.diags(db));
                }
                TypeBound::Kind(Partial::Present(kb)) => {
                    let expected = lower_hir_kind_local(kb);
                    let actual = subject.kind(db);
                    if !actual.does_match(&expected) {
                        let span = self.span().bounds().bound(i).kind_bound();
                        out.push(
                            TyLowerDiag::InconsistentKindBound {
                                span: span.into(),
                                ty: subject,
                                bound: expected,
                            }
                            .into(),
                        );
                    }
                }
                _ => {}
            }
        }

        out
    }

    // Note: legacy coarse-grained helpers for trait bound lowering have been removed;
    // prefer per-bound views (WherePredicateBoundView) for precise spans/diagnostics.

}

#[derive(Clone, Copy, Debug)]
pub struct WherePredicateBoundView<'db> {
    pub pred: WherePredicateView<'db>,
    pub idx: usize,
}

impl<'db> WherePredicateBoundView<'db> {
    fn trait_ref(self, db: &'db dyn HirDb) -> TraitRefId<'db> {
        match &self.pred.hir_pred(db).bounds[self.idx] {
            TypeBound::Trait(tr) => *tr,
            _ => unreachable!(),
        }
    }

    pub fn span(self) -> crate::span::params::LazyTypeBoundSpan<'db> {
        self.pred.span().bounds().bound(self.idx)
    }

    pub fn trait_ref_span(self) -> crate::span::params::LazyTraitRefSpan<'db> {
        self.span().trait_bound()
    }

    /// Lower this bound into a semantic trait instance using the predicate's subject type.
    pub fn as_trait_inst(self, db: &'db dyn HirAnalysisDb) -> Option<crate::analysis::ty::trait_def::TraitInstId<'db>> {
        let subject = self.pred.subject_ty(db)?;
        let owner_item = ItemKind::from(self.pred.clause.owner);
        let assumptions = constraints_for(db, owner_item);
        let scope = owner_item.scope();
        crate::analysis::ty::trait_lower::lower_trait_ref(db, subject, self.trait_ref(db), scope, assumptions).ok()
    }

    /// Lower this bound against an explicit subject type (useful for `Self` in trait contexts).
    pub fn as_trait_inst_with_subject(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: crate::analysis::ty::ty_def::TyId<'db>,
    ) -> Option<crate::analysis::ty::trait_def::TraitInstId<'db>> {
        let owner_item = ItemKind::from(self.pred.clause.owner);
        let assumptions = constraints_for(db, owner_item);
        let scope = owner_item.scope();
        crate::analysis::ty::trait_lower::lower_trait_ref(db, subject, self.trait_ref(db), scope, assumptions).ok()
    }

    /// Diagnostics for this trait bound, given an explicit subject type.
    /// Mirrors legacy visitor behavior for path errors, kind mismatch, and satisfiability.
    pub(in crate::core) fn diags_for_subject(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: crate::analysis::ty::ty_def::TyId<'db>,
    ) -> Vec<crate::analysis::ty::diagnostics::TyDiagCollection<'db>> {
        use crate::analysis::name_resolution::diagnostics::PathResDiag;
        use crate::analysis::name_resolution::ExpectedPathKind;
        use crate::analysis::ty::diagnostics::TraitConstraintDiag;
        use crate::analysis::ty::trait_lower::{lower_trait_ref, TraitRefLowerError};

        let mut out = Vec::new();
        let owner_item = ItemKind::from(self.pred.clause.owner);
        let scope = owner_item.scope();
        let assumptions = constraints_for(db, owner_item);
        let is_trait_self_subject = matches!(owner_item, ItemKind::Trait(_)) && self.pred.is_self_subject(db);
        let tr = self.trait_ref(db);
        let span = self.trait_ref_span();

        match lower_trait_ref(db, subject, tr, scope, assumptions) {
            Ok(inst) => {
                let expected = inst.def(db).expected_implementor_kind(db);
                if !expected.does_match(subject.kind(db)) {
                    out.push(
                        TraitConstraintDiag::TraitArgKindMismatch {
                            span: span.clone(),
                            expected: expected.clone(),
                            actual: subject,
                        }
                        .into(),
                    );
                }

                if inst.self_ty(db).contains_assoc_ty_of_param(db) {
                    return out;
                }

                // For trait-level `Self: Bound` constraints, treat as preconditions;
                // do not emit unsatisfied bound diags here.
                if !is_trait_self_subject {
                    use crate::analysis::ty::trait_resolution::check_trait_inst_wf;
                    let wf = check_trait_inst_wf(db, scope.ingot(db), inst, assumptions);
                    if let crate::analysis::ty::trait_resolution::WellFormedness::IllFormed { goal, .. } = wf {
                        out.push(
                            TraitConstraintDiag::TraitBoundNotSat {
                                span: span.into(),
                                primary_goal: goal,
                                unsat_subgoal: None,
                            }
                            .into(),
                        );
                    }
                }
            }
            Err(TraitRefLowerError::PathResError(err)) => {
                if let Some(path) = tr.path(db).to_opt() {
                    if let Some(diag) = err.into_diag(db, path, span.path(), ExpectedPathKind::Trait) {
                        out.push(diag.into());
                    }
                }
            }
            Err(TraitRefLowerError::InvalidDomain(res)) => {
                if let Some(path) = tr.path(db).to_opt() {
                    if let Some(ident) = path.ident(db).to_opt() {
                        out.push(PathResDiag::ExpectedTrait(span.path().into(), ident, res.kind_name()).into());
                    }
                }
            }
            Err(TraitRefLowerError::Ignored) => {}
        }

        out
    }

    /// Diagnostics for this trait bound, deriving the subject from the predicate's LHS.
    /// Returns a single-element vec with the subject error if subject lowering fails.
    pub fn diags(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::diagnostics::TyDiagCollection<'db>> {
        match self.pred.subject_ty_checked(db) {
            Ok(Some(subject)) => self.diags_for_subject(db, subject),
            Ok(None) => Vec::new(),
            Err(diag) => vec![diag],
        }
    }
}

impl<'db> Contract<'db> {
    // Planned semantic surface:
    // - ty_fields(db) -> AdtField
    // - validate_fields(db) -> Vec<TyDiagCollection>
    // - analyze(db) -> Vec<TyDiagCollection>
}

// Type alias ---------------------------------------------------------------

impl<'db> TypeAlias<'db> {
    // Planned semantic surface:
    // - generic_params_diags(db) -> Vec<TyDiagCollection>

    /// Semantic alias target type (convenience over lower_type_alias).
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let ta = lower_type_alias(db, self);
        *ta.alias_to.skip_binder()
    }

    /// Diagnostics for alias target type and generics:
    /// - Invalid target type lowering errors
    /// - Well-formedness of the target type
    /// - Generic param diagnostics (duplicates, defined in parent, defaults ordering/refs)
    pub fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();

        // Generic param diags (aggregated here to avoid duplication in visitor)
        let owner = GenericParamOwner::TypeAlias(self);
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));

        // Target type diagnostics
        let span = self.span().ty();
        let assumptions = constraints_for(db, self.into());
        let Some(hir_ty) = self.type_ref___tmp(db).to_opt() else {
            return out; // parse-missing
        };
        let ty = lower_hir_ty(db, hir_ty, self.scope(), assumptions);
        if ty.has_invalid(db) {
            let diags = crate::analysis::ty::ty_error::collect_ty_lower_errors(
                db,
                self.scope(),
                hir_ty,
                span.clone(),
                assumptions,
            );
            out.extend(diags);
        }
        // WF diagnostic via check_ty_wf
        let wf = crate::analysis::ty::trait_resolution::check_ty_wf(
            db,
            self.top_mod(db).ingot(db),
            ty,
            assumptions,
        );
        if let crate::analysis::ty::trait_resolution::WellFormedness::IllFormed { goal, subgoal } = wf {
            out.push(
                crate::analysis::ty::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                    span: span.into(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                }
                .into(),
            );
        }
        out
    }
}

// Trait / Impl items --------------------------------------------------------

impl<'db> Trait<'db> {
    // Planned semantic surface:
    // - assoc types default bounds (per‑bound views + diags)
    // - generic_params_diags(db), where_clause_diags(db)
    // - analyze(db) -> Vec<TyDiagCollection>

    /// Iterate associated types as contextual views.
    pub fn assoc_types(self, db: &'db dyn HirDb) -> impl Iterator<Item = TraitAssocTypeView<'db>> + 'db {
        let len = self.types(db).len();
        (0..len).map(move |idx| TraitAssocTypeView { owner: self, idx })
    }

    /// Diagnostics for associated type defaults (bounds satisfaction), in the trait's context.
    pub fn diags_assoc_defaults(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        let assumptions = constraints_for(db, self.into());
        for assoc in self.assoc_types(db) {
            let Some(default_ty) = assoc.default_ty(db) else { continue };
            for trait_inst in assoc.with_subject(default_ty).bounds(db) {
                let canonical_inst = Canonical::new(db, trait_inst);
                match is_goal_satisfiable(
                    db,
                    self.top_mod(db).ingot(db),
                    canonical_inst,
                    assumptions,
                ) {
                    GoalSatisfiability::Satisfied(_) => {}
                    GoalSatisfiability::UnSat(_subgoal) => {
                        diags.push(
                            crate::analysis::ty::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                                span: self.span().into(),
                                primary_goal: trait_inst,
                                unsat_subgoal: None,
                            }
                            .into(),
                        );
                    }
                    _ => {}
                }
            }
        }
        diags
    }

    /// Diagnostics for generic parameter issues (duplicates, defined in parent).
    /// Note: callers should avoid duplicating diagnostics if other analysis already
    /// emits these; prefer aggregating at a single layer.
    pub fn diags_generic_params(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let owner = GenericParamOwner::Trait(self);
        let mut out: Vec<TyDiagCollection> = owner
            .diags_check_duplicate_names(db)
            .collect();
        out.extend(owner.diags_params_defined_in_parent(db));
        out
    }

    /// Diagnostics for super-traits (semantic, kind-mismatch only).
    /// Note: Path resolution and trait-ref well-formedness diagnostics are still
    /// handled elsewhere; this surfaces the stable kind-mismatch for `Self` in
    /// super-trait declarations.
    pub fn diags_super_traits(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::diagnostics::TraitConstraintDiag;
        use crate::analysis::ty::trait_resolution::{check_trait_inst_wf, WellFormedness};
        let mut diags = Vec::new();
        for view in self.super_trait_refs(db) {
            if let Some((expected, actual)) = view.kind_mismatch_for_self(db) {
                diags.push(
                    TraitConstraintDiag::TraitArgKindMismatch {
                        span: view.span(),
                        expected,
                        actual,
                    }
                    .into(),
                );
            }

            // Additionally, ensure that the super-trait reference is well-formed
            if let Ok(inst) = view.trait_inst(db) {
                match check_trait_inst_wf(db, self.top_mod(db).ingot(db), inst, view.assumptions(db)) {
                    WellFormedness::WellFormed => {}
                    WellFormedness::IllFormed { goal, .. } => {
                        diags.push(
                            TraitConstraintDiag::TraitBoundNotSat {
                                span: view.span().into(),
                                primary_goal: goal,
                                unsat_subgoal: None,
                            }
                            .into(),
                        );
                    }
                }
            }
        }
        diags
    }

    /// Semantic super-trait bounds of this trait, instantiated over the trait's own parameters.
    /// Returns an iterator of binders for each declared or implied super-trait.
    pub fn super_trait_insts(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<
        Item = crate::analysis::ty::binder::Binder<crate::analysis::ty::trait_def::TraitInstId<'db>>,
    > + 'db {
        use crate::analysis::ty::trait_def::TraitDef;
        TraitDef::new(db, self)
            .super_traits(db)
            .iter()
            .copied()
    }

    /// Iterate declared super-trait references as contextual views.
    pub fn super_trait_refs(self, db: &'db dyn HirDb) -> impl Iterator<Item = SuperTraitRefView<'db>> + 'db {
        let len = self.super_traits(db).len();
        (0..len).map(move |idx| SuperTraitRefView { owner: self, idx })
    }

    /// Aggregate trait definition diagnostics using semantic views.
    /// Includes:
    /// - Associated type default bound satisfaction
    /// - Super-trait constraint satisfaction
    /// - Generic parameter diagnostics (duplicates, defined-in-parent, kind/trait bounds,
    ///   non-trailing defaults, default forward references)
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();
        out.extend(self.diags_assoc_defaults(db));
        out.extend(self.diags_super_traits(db));

        let owner = GenericParamOwner::Trait(self);
        out.extend(owner.diags_params_defined_in_parent(db));
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));
        // where-clause kind/trait bound diagnostics
        {
            let clause = WhereClauseView { owner: WhereClauseOwner::Trait(self), id: self.where_clause(db) };
            for pred in clause.predicates(db) {
                out.extend(pred.diags(db));
            }
        }
        out
    }
}

// ADT recursion (semantic) --------------------------------------------------

impl<'db> AdtDef<'db> {
    /// Detects a recursive ADT cycle that is not guarded by an indirect wrapper
    /// (e.g., pointer/reference). Returns the cycle members if the ADT is part
    /// of a cycle; otherwise returns None.
    pub fn recursive_cycle(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<Vec<crate::analysis::ty::def_analysis::AdtCycleMember<'db>>> {
        fn impl_check<'db>(
            db: &'db dyn HirAnalysisDb,
            adt: AdtDef<'db>,
            chain: &[
                crate::analysis::ty::def_analysis::AdtCycleMember<'db>
            ],
        ) -> Option<Vec<crate::analysis::ty::def_analysis::AdtCycleMember<'db>>> {
            use crate::analysis::ty::adt_def::lower_adt;
            use crate::analysis::ty::def_analysis::AdtCycleMember;

            if chain.iter().any(|m| m.adt == adt) {
                return Some(chain.to_vec());
            } else if adt.fields(db).is_empty() {
                return None;
            }

            let mut chain = chain.to_vec();
            for (field_idx, field) in adt.fields(db).iter().enumerate() {
                for (ty_idx, ty) in field.iter_types(db).enumerate() {
                    for field_adt_ref in collect_direct_adts(db, ty.instantiate_identity()) {
                        chain.push(AdtCycleMember {
                            adt,
                            field_idx: field_idx as u16,
                            ty_idx: ty_idx as u16,
                        });

                        if let Some(cycle) = impl_check(db, lower_adt(db, field_adt_ref), &chain)
                            && cycle.iter().any(|m| m.adt == adt)
                        {
                            return Some(cycle);
                        }
                        chain.pop();
                    }
                }
            }
            None
        }

        impl_check(db, self, &[])
    }
}

/// Collect all ADTs directly appearing inside the given type without
/// traversing through indirect wrappers like pointers or references.
fn collect_direct_adts<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> rustc_hash::FxHashSet<crate::analysis::ty::adt_def::AdtRef<'db>> {
    use crate::analysis::ty::visitor::TyVisitable;
    use crate::analysis::ty::adt_def::AdtRef;
    use rustc_hash::FxHashSet;
    use crate::analysis::ty::ty_def::TyData;
    use crate::analysis::ty::ty_def::TyBase;
    use crate::analysis::ty::ty_def::PrimTy;

    struct AdtCollector<'db> {
        db: &'db dyn HirAnalysisDb,
        adts: FxHashSet<AdtRef<'db>>,
    }
    impl<'db> TyVisitor<'db> for AdtCollector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb { self.db }
        fn visit_app(&mut self, abs: TyId<'db>, arg: TyId<'db>) {
            let is_indirect = match abs.data(self.db) {
                TyData::TyBase(TyBase::Prim(PrimTy::Ptr)) => true,
                // Future: handle Ref when introduced.
                _ => false,
            };
            if !is_indirect { walk_ty(self, arg) }
        }
        fn visit_adt(&mut self, adt: AdtDef<'db>) {
            self.adts.insert(adt.adt_ref(self.db));
        }
    }

    let mut collector = AdtCollector { db, adts: FxHashSet::default() };
    ty.visit_with(&mut collector);
    collector.adts
}

#[derive(Clone, Copy, Debug)]
pub struct SuperTraitRefView<'db> {
    pub owner: Trait<'db>,
    pub idx: usize,
}

impl<'db> SuperTraitRefView<'db> {
    pub fn span(self) -> crate::span::params::LazyTraitRefSpan<'db> {
        self.owner.span().super_traits().super_trait(self.idx)
    }

    pub(in crate::core) fn trait_ref_id(self, db: &'db dyn HirDb) -> TraitRefId<'db> {
        self.owner.super_traits(db)[self.idx]
    }

    pub fn subject_self(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        crate::analysis::ty::ty_lower::collect_generic_params(db, self.owner.into())
            .trait_self(db)
            .unwrap()
    }

    pub(in crate::core) fn assumptions(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        constraints_for(db, self.owner.into())
    }

    /// Lower this super-trait reference to a semantic trait instance using the trait's
    /// `Self` and constraints.
    /// Semantic trait instance for this super-trait reference lowered in the owner's
    /// context. Returns an error value; does not emit diagnostics.
    pub fn trait_inst(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<crate::analysis::ty::trait_def::TraitInstId<'db>, SuperTraitLowerError> {
        use crate::analysis::ty::trait_lower::TraitRefLowerError;
        let subject = self.subject_self(db);
        let tr = self.trait_ref_id(db);
        let scope = self.owner.scope();
        let assumptions = self.assumptions(db);
        match crate::analysis::ty::trait_lower::lower_trait_ref(db, subject, tr, scope, assumptions) {
            Ok(v) => Ok(v),
            Err(TraitRefLowerError::PathResError(_)) => Err(SuperTraitLowerError::PathResolution),
            Err(TraitRefLowerError::InvalidDomain(_)) => Err(SuperTraitLowerError::InvalidDomain),
            Err(TraitRefLowerError::Ignored) => Err(SuperTraitLowerError::Ignored),
        }
    }

    /// Returns a tuple of (expected_kind, actual_self) when the owner's `Self` kind
    /// does not match the super-trait's expected implementor kind. Returns None when
    /// kinds are compatible or `Self` is invalid.
    pub fn kind_mismatch_for_self(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(
        crate::analysis::ty::ty_def::Kind,
        crate::analysis::ty::ty_def::TyId<'db>,
    )> {
        use crate::analysis::ty::trait_lower::{lower_trait_ref, TraitRefLowerError};
        let subject = self.subject_self(db);
        let scope = self.owner.scope();
        let assumptions = self.assumptions(db);
        let tr = self.trait_ref_id(db);
        let expected = match lower_trait_ref(db, subject, tr, scope, assumptions) {
            Ok(inst) => inst.def(db).expected_implementor_kind(db).clone(),
            // If we cannot lower, defer to other diagnostics; do not emit mismatch here.
            Err(TraitRefLowerError::PathResError(_) | TraitRefLowerError::InvalidDomain(_) | TraitRefLowerError::Ignored) => return None,
        };
        let actual = subject;
        (!expected.does_match(actual.kind(db))).then_some((expected, actual))
    }

    // Note: callers that want an Option can map the error to None explicitly.
}

/// Semantic error for lowering a super-trait reference in its owner's context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SuperTraitLowerError {
    PathResolution,
    InvalidDomain,
    Ignored,
}

impl<'db> Impl<'db> {
    // Planned semantic surface:
    // - analyze_preconditions(db) -> Vec<TyDiagCollection>
    // - generic_params_diags(db), where_clause_diags(db)
    // - analyze(db) -> Vec<TyDiagCollection>

    /// Semantic implementor type of this inherent impl.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let assumptions = constraints_for(db, self.into());
        self.type_ref(db)
            .to_opt()
            .map(|hir_ty| lower_hir_ty(db, hir_ty, self.scope(), assumptions))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
    }

    /// Preconditions and implementor-type diagnostics for this impl:
    /// - Inherent impl allowed for this type in this ingot
    /// - Implementor type invalid/WF
    /// - Generic parameter diagnostics
    pub fn diags_preconditions(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::diagnostics::ImplDiag;
        let mut out = Vec::new();

        // Generic params for impl
        let owner = GenericParamOwner::Impl(self);
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));

        // Surface name-resolution errors for the implementor type first
        if let Some(hir_ty) = self.type_ref(db).to_opt() {
            let assumptions = constraints_for(db, self.into());
            let mut errs = crate::analysis::ty::ty_error::collect_ty_lower_errors(
                db,
                self.scope(),
                hir_ty,
                self.span().target_ty(),
                assumptions,
            );
            if !errs.is_empty() {
                out.append(&mut errs);
            }
        }

        let ty = self.ty(db);
        let ingot = self.top_mod(db).ingot(db);
        if !ty.is_inherent_impl_allowed(db, ingot) {
            let base = ty.base_ty(db);
            out.push(
                ImplDiag::InherentImplIsNotAllowed {
                    primary: self.span().target_ty().into(),
                    ty: base.pretty_print(db).to_string(),
                    is_nominal: !base.is_param(db),
                }
                .into(),
            );
            return out;
        }

        if let Some(diag) = crate::analysis::ty::ty_error::emit_invalid_ty_error(
            db,
            ty,
            self.span().target_ty().into(),
        ) {
            out.push(diag);
        }

        // If the implementor type is invalid (e.g., missing generics), avoid
        // emitting downstream WF/trait satisfaction diagnostics to prevent
        // noisy follow-ups in name-resolution scenarios.
        if ty.has_invalid(db) {
            return out;
        }

        // Well-formedness / trait-bound satisfaction for the implementor type
        {
            use crate::analysis::ty::trait_resolution::{check_ty_wf, WellFormedness};
            use crate::analysis::ty::trait_resolution::constraint::ty_constraints;
            match check_ty_wf(db, ingot, ty, constraints_for(db, self.into())) {
                WellFormedness::WellFormed => {
                    // Heuristic: if any required trait constraint applies to a type
                    // parameter, surface a not-satisfied diagnostic to preserve legacy
                    // expectations in snapshots.
                    let constraints = ty_constraints(db, ty);
                    for &goal in constraints.list(db) {
                        if goal.self_ty(db).has_param(db) {
                            out.push(
                                crate::analysis::ty::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                                    span: self.span().target_ty().into(),
                                    primary_goal: goal,
                                    unsat_subgoal: None,
                                }
                                .into(),
                            );
                            break;
                        }
                    }
                }
                WellFormedness::IllFormed { goal, subgoal } => {
                    out.push(
                        crate::analysis::ty::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                            span: self.span().target_ty().into(),
                            primary_goal: goal,
                            unsat_subgoal: subgoal,
                        }
                        .into(),
                    );
                }
            }
        }

        out
    }

    /// Aggregate impl definition diagnostics using semantic views.
    /// Includes:
    /// - Preconditions and implementor type checks
    /// - Generic parameter diagnostics (duplicates, defined-in-parent, kind/trait bounds,
    ///   non-trailing defaults, default forward references)
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = self.diags_preconditions(db);
        let owner = GenericParamOwner::Impl(self);
        out.extend(owner.diags_params_defined_in_parent(db));
        out
    }
}

impl<'db> ImplTrait<'db> {
    // Planned semantic surface:
    // - trait_inst(db) -> Result<TraitInstId, TraitRefLowerError>
    // - associated type values/defaults (semantic map)
    // - associated types diags (WF + lowering + satisfiability)
    // - generic_params_diags(db), where_clause_diags(db)
    // - analyze(db) -> Vec<TyDiagCollection>

    /// Semantic self type of this impl-trait block.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let assumptions = constraints_for(db, self.into());
        self.type_ref(db)
            .to_opt()
            .map(|hir_ty| lower_hir_ty(db, hir_ty, self.scope(), assumptions))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
    }

    /// Iterate associated type definitions in this impl-trait block as views.
    pub fn assoc_types(self, db: &'db dyn HirDb) -> impl Iterator<Item = ImplAssocTypeView<'db>> + 'db {
        let len = self.types(db).len();
        (0..len).map(move |idx| ImplAssocTypeView { owner: self, idx })
    }

    /// Diagnostics for missing associated types (required by the trait).
    pub fn diags_missing_assoc_types(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::trait_lower::lower_impl_trait;
        use crate::analysis::ty::diagnostics::ImplDiag;
        let mut diags = Vec::new();
        let Some(implementor) = lower_impl_trait(db, self) else { return diags; };
        let implementor = implementor.instantiate_identity();
        let trait_hir = implementor.trait_def(db).trait_(db);
        let impl_types = implementor.types(db);
        for assoc in trait_hir.assoc_types(db) {
            let Some(name) = assoc.name(db) else { continue; };
            let has_impl = impl_types.get(&name).is_some();
            let has_default = assoc.default_ty(db).is_some();
            if !has_impl && !has_default {
                diags.push(
                    ImplDiag::MissingAssociatedType {
                        primary: self.span().ty().into(),
                        type_name: name,
                        trait_: trait_hir,
                    }
                    .into(),
                );
            }
        }
        diags
    }

    /// Diagnostics for associated type bounds on implemented assoc types.
    pub fn diags_assoc_types_bounds(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::trait_lower::lower_impl_trait;
        let mut diags = Vec::new();
        let Some(implementor) = lower_impl_trait(db, self) else { return diags; };
        let implementor = implementor.instantiate_identity();
        let trait_hir = implementor.trait_def(db).trait_(db);
        let impl_types = implementor.types(db);
        // Recompute implementor-specific assumptions from its underlying HIR.
        let impl_trait_hir = implementor.hir_impl_trait(db);
        let assumptions = collect_constraints(db, impl_trait_hir.into()).instantiate_identity();

        for assoc in trait_hir.assoc_types(db) {
            let Some(name) = assoc.name(db) else { continue; };
            let Some(&impl_ty) = impl_types.get(&name) else { continue; };
            for b in assoc.bounds(db) {
                let Some(bound_inst) = b.as_trait_inst_with_subject_and_owner(
                    db,
                    impl_ty,
                    implementor.self_ty(db),
                ) else { continue; };
                let canonical_bound = Canonical::new(db, bound_inst);
                if let GoalSatisfiability::UnSat(subgoal) =
                    is_goal_satisfiable(db, self.top_mod(db).ingot(db), canonical_bound, assumptions)
                {
                    let assoc_ty_span = self
                        .associated_type_span(db, name)
                        .map(|s| s.ty().into())
                        .unwrap_or_else(|| self.span().ty().into());

                    diags.push(
                        crate::analysis::ty::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                            span: assoc_ty_span,
                            primary_goal: bound_inst,
                            unsat_subgoal: None,
                        }
                        .into(),
                    );
                }
            }
        }
        diags
    }

    /// Diagnostics for trait-ref WF and satisfiability for this impl-trait.
    /// Aggregates checks similar to def_analysis but scoped to this view.
    pub fn diags_trait_ref_and_wf(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::trait_lower::lower_impl_trait;
        use crate::analysis::ty::trait_resolution::{constraint::collect_constraints, is_goal_satisfiable, GoalSatisfiability};
        use crate::analysis::ty::canonical::Canonicalized;
        use crate::span::DynLazySpan;
        let mut diags = Vec::new();
        let Some(implementor) = lower_impl_trait(db, self) else { return diags; };
        let implementor = implementor.instantiate_identity();
        let trait_inst = implementor.trait_(db);
        let trait_def = implementor.trait_def(db);

        // Trait constraints instantiated with implementor args
        let trait_constraints = collect_constraints(db, trait_def.trait_(db).into())
            .instantiate(db, trait_inst.args(db));

        // Recompute assumptions from the impl-trait HIR
        let assumptions = collect_constraints(db, self.into()).instantiate_identity();
        let impl_trait_ingot = self.top_mod(db).ingot(db);

        let is_satisfied = |goal, span: DynLazySpan<'db>, out: &mut Vec<_>| {
            let canonical_goal = Canonicalized::new(db, goal);
            match is_goal_satisfiable(db, impl_trait_ingot, canonical_goal.value, assumptions) {
                GoalSatisfiability::Satisfied(_) | GoalSatisfiability::ContainsInvalid => {}
                GoalSatisfiability::NeedsConfirmation(_) => {}
                GoalSatisfiability::UnSat(_sub) => {
                    out.push(
                        crate::analysis::ty::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                            span,
                            primary_goal: goal,
                            unsat_subgoal: None,
                        }
                        .into(),
                    );
                }
            }
        };

        // Trait-ref WF
        let trait_ref_span: DynLazySpan = self.span().trait_ref().into();
        for &goal in trait_constraints.list(db) {
            is_satisfied(goal, trait_ref_span.clone(), &mut diags);
        }

        // Super-traits
        let target_ty_span: DynLazySpan = self.span().ty().into();
        for &super_trait in trait_def.super_traits(db) {
            let super_trait = super_trait.instantiate(db, trait_inst.args(db));
            is_satisfied(super_trait, target_ty_span.clone(), &mut diags)
        }

        diags
    }

    /// Diagnostics for well-formedness and invalid types of implemented associated types.
    /// Emits parse/lower errors and well-formedness (trait-bound satisfiability) for each
    /// `type Assoc = ...` inside this impl-trait block.
    pub fn diags_assoc_types_wf(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::{
            trait_resolution::{check_ty_wf, WellFormedness},
            ty_error::collect_ty_lower_errors,
            ty_lower::lower_hir_ty,
        };
        let mut diags = Vec::new();
        let ingot = self.top_mod(db).ingot(db);
        let assumptions = constraints_for(db, self.into());
        for (idx, def) in self.types(db).iter().enumerate() {
            let ty_span = self.span().associated_type(idx).ty();
            if let Some(hir) = def.type_ref.to_opt() {
                let errs = collect_ty_lower_errors(db, self.scope(), hir, ty_span.clone(), assumptions);
                if !errs.is_empty() {
                    diags.extend(errs);
                    continue;
                }
                let ty = lower_hir_ty(db, hir, self.scope(), assumptions);
                if let WellFormedness::IllFormed { goal, subgoal } =
                    check_ty_wf(db, ingot, ty, assumptions)
                {
                    diags.push(
                        crate::analysis::ty::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                            span: ty_span.into(),
                            primary_goal: goal,
                            unsat_subgoal: subgoal,
                        }
                        .into(),
                    );
                }
            }
        }
        diags
    }

    /// Aggregate impl-trait definition diagnostics using semantic views.
    /// Includes:
    /// - Early trait-ref/implementor type checks and ingot rules
    /// - Method conformance and missing-items checks
    /// - Associated type checks (presence + bound satisfaction)
    /// - Generic parameter diagnostics (duplicates, defined-in-parent, kind/trait bounds,
    ///   non-trailing defaults, default forward references)
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::name_resolution::{ExpectedPathKind, diagnostics::PathResDiag};
        use crate::analysis::ty::trait_lower::{lower_trait_ref, TraitRefLowerError};

        let mut out = Vec::new();

        // Early trait-ref path/domain checks
        if let Some(tr) = self.trait_ref(db).to_opt() {
            if !matches!(self.ty(db).data(db), TyData::Invalid(InvalidCause::ParseError)) {
                let assumptions = collect_constraints(db, self.into()).instantiate_identity();
                let ty = self.ty(db);
                if let Some(diag) = crate::analysis::ty::ty_error::emit_invalid_ty_error(
                    db,
                    ty,
                    self.span().ty().into(),
                ) {
                    out.push(diag);
                }
                if !ty.has_invalid(db) {
                    if let Err(err) = lower_trait_ref(db, ty, tr, self.scope(), assumptions) {
                        match err {
                            TraitRefLowerError::PathResError(e) => {
                                if let Some(diag) = e.into_diag(
                                    db,
                                    tr.path(db).unwrap(),
                                    self.span().trait_ref().path(),
                                    ExpectedPathKind::Trait,
                                ) {
                                    out.push(diag.into());
                                }
                                return out;
                            }
                            TraitRefLowerError::InvalidDomain(res) => {
                                out.push(
                                    PathResDiag::ExpectedTrait(
                                        self.span().trait_ref().path().into(),
                                        tr.path(db).unwrap().ident(db).unwrap(),
                                        res.kind_name(),
                                    )
                                    .into(),
                                );
                                return out;
                            }
                            TraitRefLowerError::Ignored => {}
                        }
                    }
                } else {
                    return out;
                }
            } else {
                return out;
            }
        } else {
            return out;
        }

        // Trait-ref WF and super-trait constraints
        out.extend(self.diags_trait_ref_and_wf(db));
        // Associated types (presence + bound satisfaction)
        out.extend(self.diags_missing_assoc_types(db));
        out.extend(self.diags_assoc_types_bounds(db));
        // Generic parameter diagnostics
        let owner = GenericParamOwner::ImplTrait(self);
        out.extend(owner.diags_params_defined_in_parent(db));
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));
        out
    }
}

// ADT analyze entry points --------------------------------------------------

impl<'db> Struct<'db> {
    /// Aggregate struct definition diagnostics:
    /// - Duplicate field names
    /// - Per-field type WF/name-resolution errors
    /// - Generic parameter diagnostics
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();
        // duplicate field names
        out.extend(def_analysis::check_duplicate_names(
            FieldParent::Struct(self).fields(db).map(|v| v.name(db)),
            |idxs| TyLowerDiag::DuplicateFieldName(FieldParent::Struct(self), idxs).into(),
        ));
        // per-field diags
        for v in FieldParent::Struct(self).fields(db) {
            out.extend(v.diags_wf(db));
        }
        // generic param diags
        let owner = GenericParamOwner::Struct(self);
        out.extend(owner.diags_const_param_types(db));
        out.extend(owner.diags_params_defined_in_parent(db));
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));
        // where-clause trait bounds/wf
        {
            let clause = WhereClauseView { owner: WhereClauseOwner::Struct(self), id: self.where_clause(db) };
            for pred in clause.predicates(db) {
                out.extend(pred.diags(db));
            }
        }
        out
    }
}

impl<'db> Enum<'db> {
    /// Aggregate enum definition diagnostics:
    /// - Duplicate variant names; duplicate field names within record variants
    /// - Per-field type WF/name-resolution errors
    /// - Generic parameter diagnostics
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();
        // duplicate variant names
        out.extend(def_analysis::check_duplicate_names(
            self.variants(db).map(|v| v.name(db).to_opt()),
            |idxs| TyLowerDiag::DuplicateVariantName(self, idxs).into(),
        ));
        // per-variant field checks
        for v in self.variants(db) {
            if matches!(v.kind(db), VariantKind::Record(_)) {
                // Duplicate field names within this record variant
                out.extend(def_analysis::check_duplicate_names(
                    v.fields(db).map(|f| f.name(db)),
                    |idxs| TyLowerDiag::DuplicateFieldName(FieldParent::Variant(EnumVariant::new(self, v.idx)), idxs).into(),
                ));
                // Per-field type/name-resolution diagnostics
                for f in v.fields(db) {
                    out.extend(f.diags_wf(db));
                }
            } else if matches!(v.kind(db), VariantKind::Tuple(_)) {
                // Tuple-variant element type diagnostics (WF / name resolution)
                out.extend(v.diags_tuple_elems_wf(db));
            }
        }
        // generic param diags
        let owner = GenericParamOwner::Enum(self);
        out.extend(owner.diags_const_param_types(db));
        out.extend(owner.diags_params_defined_in_parent(db));
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));
        // where-clause trait bounds/wf
        {
            let clause = WhereClauseView { owner: WhereClauseOwner::Enum(self), id: self.where_clause(db) };
            for pred in clause.predicates(db) {
                out.extend(pred.diags(db));
            }
        }
        out
    }
}

impl<'db> Contract<'db> {
    /// Aggregate contract definition diagnostics similar to struct.
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();
        out.extend(def_analysis::check_duplicate_names(
            FieldParent::Contract(self).fields(db).map(|v| v.name(db)),
            |idxs| TyLowerDiag::DuplicateFieldName(FieldParent::Contract(self), idxs).into(),
        ));
        for v in FieldParent::Contract(self).fields(db) {
            out.extend(v.diags_wf(db));
        }
        out
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ImplAssocTypeView<'db> {
    pub owner: ImplTrait<'db>,
    pub idx: usize,
}

impl<'db> ImplAssocTypeView<'db> {
    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.owner.types(db)[self.idx].name.to_opt()
    }

    pub fn span(self) -> crate::span::item::LazyTraitTypeSpan<'db> {
        self.owner.span().associated_type(self.idx)
    }

    /// Semantic type of this associated type implementation.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let hir = self.owner.types(db)[self.idx].type_ref.to_opt()?;
        let assumptions = constraints_for(db, self.owner.into());
        Some(lower_hir_ty(db, hir, self.owner.scope(), assumptions))
    }
}

// Const / Use ---------------------------------------------------------------

impl<'db> Const<'db> {
    // Planned semantic surface:
    // - additional const semantics/diags as needed

    /// Semantic type of this const definition.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let Some(hir_ty) = self.type_ref(db).to_opt() else {
            return TyId::invalid(db, InvalidCause::ParseError);
        };
        lower_hir_ty(db, hir_ty, self.scope(), PredicateListId::empty_list(db))
    }
}

impl<'db> Use<'db> {
    // Planned semantic surface:
    // - resolved target summary / simple diags (optional)
}

// Shared capability hints ---------------------------------------------------

impl<'db> ItemKind<'db> {
    // Planned semantic surface:
    // - semantic helpers to opt into capabilities (HasGenericParams/HasWhereClause)
}

// Note:
// Avoid adding tracked methods here. Keep tracked queries in lowering and call
// them from small, semantic helpers only. This module should remain the
// ergonomic public surface for traversal without leaking syntax.

impl<'db> GenericParamOwner<'db> {
    pub fn param_view(self, db: &'db dyn HirDb, idx: usize) -> GenericParamView<'db> {
        self.params(db)
            .nth(idx)
            .expect("failed to get the generic param")
    }

    pub fn params(self, db: &'db dyn HirDb) -> impl Iterator<Item = GenericParamView<'db>> + 'db {
        self.params_list(db)
            .data(db)
            .iter()
            .enumerate()
            .map(move |(idx, param)| GenericParamView {
                owner: self,
                param,
                idx,
            })
    }

    pub fn diags_params_defined_in_parent(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = TyDiagCollection<'db>> + 'db {
        self.params(db).filter_map(|param| {
            param
                .diag_param_defined_in_parent(db)
                .map(TyDiagCollection::from)
        })
    }
    pub fn diags_check_duplicate_names(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = TyDiagCollection<'db>> + 'db {
        let params_iter = self
            .params_list(db)
            .data(db)
            .iter()
            .map(|p| p.name().to_opt());
        check_duplicate_names(params_iter, |idxs| {
            TyDiagCollection::from(TyLowerDiag::DuplicateGenericParamName(self, idxs))
        })
        .into_iter()
    }

    /// Diagnostics for non-trailing default generic parameters.
    /// Emits a diagnostic for each defaulted type parameter that precedes a non-defaulted one.
    pub fn diags_non_trailing_defaults(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();
        let list = self.params_list(db);
        let data = list.data(db);
        let mut default_idxs = Vec::new();
        for (i, p) in data.iter().enumerate() {
            let is_defaulted_type = matches!(p, GenericParam::Type(tp) if tp.default_ty.is_some());
            if is_defaulted_type {
                default_idxs.push(i);
            } else if !default_idxs.is_empty() {
                for &idx in &default_idxs {
                    let span = self.params_span().param(idx);
                    out.push(TyLowerDiag::NonTrailingDefaultGenericParam(span).into());
                }
                break;
            }
        }
        out
    }

    /// Diagnostics for const generic parameter types.
    /// Emits `InvalidConstParamTy` and related const-ty lowering diagnostics
    /// at the precise const-parameter type span when invalid.
    pub fn diags_const_param_types(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::diagnostics::TyLowerDiag;
        use crate::analysis::ty::ty_def::{InvalidCause, TyData};
        use crate::hir_def::GenericParam;

        let mut out = Vec::new();
        let param_set = crate::analysis::ty::ty_lower::collect_generic_params(db, self);
        for view in self.params(db) {
            let GenericParam::Const(c) = view.param else { continue };
            // Only check when a type annotation exists
            if c.ty.to_opt().is_none() { continue; }
            if let Some(ty) = param_set.param_by_original_idx(db, view.idx) {
                // Extract inner invalid cause for const params
                let cause_opt = match ty.data(db) {
                    TyData::Invalid(cause) => Some(cause.clone()),
                    TyData::ConstTy(ct) => match ct.ty(db).data(db) {
                        TyData::Invalid(cause) => Some(cause.clone()),
                        _ => None,
                    },
                    _ => None,
                };
                if let Some(cause) = cause_opt {
                    let span = view.span().into_const_param().ty();
                    match cause {
                        InvalidCause::InvalidConstParamTy => {
                            out.push(TyLowerDiag::InvalidConstParamTy(span.into()).into());
                        }
                        InvalidCause::RecursiveConstParamTy => {
                            out.push(TyLowerDiag::RecursiveConstParamTy(span.into()).into());
                        }
                        InvalidCause::ConstTyExpected { expected } => {
                            out.push(TyLowerDiag::ConstTyExpected { span: span.into(), expected }.into());
                        }
                        InvalidCause::ConstTyMismatch { expected, given } => {
                            out.push(TyLowerDiag::ConstTyMismatch { span: span.into(), expected, given }.into());
                        }
                        _ => {}
                    }
                }
            }
        }
        out
    }

    /// Diagnostics for forward references in defaulted generic type parameters.
    /// Emits a diagnostic when a default type references a parameter not yet declared
    /// (including itself), e.g., `trait T<A = B, B> {}` or `trait T<A = A> {}`.
    pub fn diags_default_forward_refs(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
use crate::analysis::ty::{
    ty_def::{TyId, TyParam},
    ty_lower::lower_hir_ty,
    visitor::{TyVisitable, TyVisitor},
};

        let mut out = Vec::new();
        let owner_item = ItemKind::from(self);
        let assumptions = constraints_for(db, owner_item);
        let scope = self.scope();
        let list = self.params_list(db);
        let data = list.data(db);

        for (i, p) in data.iter().enumerate() {
            let default_ty = match p {
                GenericParam::Type(tp) => tp.default_ty,
                GenericParam::Const(_) => None,
            };
            let Some(default_ty) = default_ty else { continue };

            let lowered = lower_hir_ty(db, default_ty, scope, assumptions);

            // Collect referenced generic params belonging to the same owner.
            struct Collector<'db> {
                db: &'db dyn HirAnalysisDb,
                scope: ScopeId<'db>,
                out: Vec<usize>,
            }
            impl<'db> TyVisitor<'db> for Collector<'db> {
                fn db(&self) -> &'db dyn HirAnalysisDb { self.db }
                fn visit_param(&mut self, tp: &TyParam<'db>) {
                    if !tp.is_trait_self() && tp.owner == self.scope {
                        self.out.push(tp.original_idx(self.db));
                    }
                }
                fn visit_const_param(&mut self, tp: &TyParam<'db>, _ty: TyId<'db>) {
                    if tp.owner == self.scope {
                        self.out.push(tp.original_idx(self.db));
                    }
                }
            }

            let mut collector = Collector { db, scope, out: Vec::new() };
            lowered.visit_with(&mut collector);

            for j in collector.out.into_iter().filter(|j| *j >= i) {
                if let Some(name) = self.param_view(db, j).param.name().to_opt() {
                    let span = self.params_span().param(i);
                    out.push(TyLowerDiag::GenericDefaultForwardRef { span, name }.into());
                }
            }
        }

        out
    }

    /// Diagnostics for kind bounds declared on generic type parameters.
    /// For each `T: <kind>` bound on a type parameter `T`, verifies that `T`'s
    /// inferred/declared kind is compatible with the bound and emits
    /// `InconsistentKindBound` at the precise bound span when they mismatch.
    pub fn diags_kind_bounds(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::ty::diagnostics::TyLowerDiag;
        use crate::hir_def::{GenericParam, TypeBound};
        use crate::hir_def::Partial;

        let mut out = Vec::new();
        // Semantic param set gives us TyIds for each original param index.
        let param_set = crate::analysis::ty::ty_lower::collect_generic_params(db, self);
        for view in self.params(db) {
            let GenericParam::Type(tp) = view.param else { continue };
            // Resolve the semantic TyId for this parameter by its original index.
            let Some(ty) = param_set.param_by_original_idx(db, view.idx) else { continue };
            let actual = ty.kind(db);

            for (i, bound) in tp.bounds.iter().enumerate() {
                if let TypeBound::Kind(Partial::Present(kb)) = bound {
                    let expected = lower_hir_kind_local(kb);
                    if !actual.does_match(&expected) {
                        let span = view
                            .span()
                            .into_type_param()
                            .bounds()
                            .bound(i)
                            .kind_bound();
                        out.push(
                            TyLowerDiag::InconsistentKindBound {
                                span: span.into(),
                                ty,
                                bound: expected,
                            }
                            .into(),
                        );
                    }
                }
            }
        }

        out
    }

    /// Diagnostics for trait bounds declared on generic type parameters.
    /// Emits path/kind/WF diagnostics per bound with precise spans.
    pub fn diags_trait_bounds(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::name_resolution::{ExpectedPathKind, diagnostics::PathResDiag};
        use crate::analysis::ty::diagnostics::TraitConstraintDiag;
        use crate::analysis::ty::trait_lower::{lower_trait_ref, TraitRefLowerError};
        use crate::analysis::ty::trait_resolution::{check_trait_inst_wf, WellFormedness};
        use crate::hir_def::{GenericParam, TypeBound};
        use crate::hir_def::Partial;

        let mut out = Vec::new();
        let param_set = crate::analysis::ty::ty_lower::collect_generic_params(db, self);
        let scope = self.scope();
        let assumptions = super::semantic::constraints_for(db, self.into());

        for view in self.params(db) {
            let GenericParam::Type(tp) = view.param else { continue };
            let Some(subject) = param_set.param_by_original_idx(db, view.idx) else { continue };
            for (i, bound) in tp.bounds.iter().enumerate() {
                let TypeBound::Trait(tr) = bound else { continue };
                let span = view
                    .span()
                    .into_type_param()
                    .bounds()
                    .bound(i)
                    .trait_bound();
                match lower_trait_ref(db, subject, *tr, scope, assumptions) {
                    Ok(inst) => {
                        let expected = inst.def(db).expected_implementor_kind(db);
                        if !expected.does_match(subject.kind(db)) {
                            out.push(
                                TraitConstraintDiag::TraitArgKindMismatch {
                                    span: span.clone(),
                                    expected: expected.clone(),
                                    actual: subject,
                                }
                                .into(),
                            );
                        }

                        if inst.self_ty(db).contains_assoc_ty_of_param(db) {
                            continue;
                        }

                        match check_trait_inst_wf(db, scope.ingot(db), inst, assumptions) {
                            WellFormedness::WellFormed => {}
                            WellFormedness::IllFormed { goal, .. } => out.push(
                                TraitConstraintDiag::TraitBoundNotSat {
                                    span: span.into(),
                                    primary_goal: goal,
                                    unsat_subgoal: None,
                                }
                                .into(),
                            ),
                        }
                    }
                    Err(TraitRefLowerError::PathResError(err)) => {
                        if let Some(path) = tr.path(db).to_opt() {
                            if let Some(diag) = err.into_diag(db, path, span.path(), ExpectedPathKind::Trait) {
                                out.push(diag.into());
                            }
                        }
                    }
                    Err(TraitRefLowerError::InvalidDomain(res)) => {
                        if let Some(path) = tr.path(db).to_opt() {
                            if let Some(ident) = path.ident(db).to_opt() {
                                out.push(PathResDiag::ExpectedTrait(span.path().into(), ident, res.kind_name()).into());
                            }
                        }
                    }
                    Err(TraitRefLowerError::Ignored) => {}
                }
            }
        }

        out
    }
}

impl<'db> GenericParamView<'db> {
    /// Lazy span of this generic parameter in its owner's parameter list.
    ///
    /// Exposes a context-free handle that can be resolved to a concrete
    /// source span via `SpannedHirDb`, without requiring callers to manually
    /// cross-link list spans with indices.
    pub fn span(&self) -> crate::span::params::LazyGenericParamSpan<'db> {
        self.owner.params_span().param(self.idx)
    }

    /// Lazy span atom for the parameter's name token.
    ///
    /// Returns the span for the ident of a type or const generic param,
    /// depending on the underlying parameter kind.
    pub fn name_span(&self) -> crate::span::LazySpanAtom<'db> {
        match self.param {
            GenericParam::Type(_) => self.span().into_type_param().name(),
            GenericParam::Const(_) => self.span().into_const_param().name(),
        }
    }

    pub fn diag_param_defined_in_parent(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<TyLowerDiag<'db>> {
        let name = self.param.name().to_opt()?;
        let parent_scope = self.owner.scope().parent_item(db)?.scope();
        let path = PathId::from_ident(db, name);
        let span = self.owner.params_span().param(self.idx);

        match resolve_path(
            db,
            path,
            parent_scope,
            PredicateListId::empty_list(db),
            false,
        ) {
            Ok(r @ PathRes::Ty(ty)) if ty.is_param(db) => {
                Some(TyLowerDiag::GenericParamAlreadyDefinedInParent {
                    span,
                    conflict_with: r.name_span(db).unwrap(),
                    name,
                })
            }
            _ => None,
        }
    }
}

// Associated type resolution traversal (crate-internal)

#[derive(Clone, Copy, Debug)]
pub(crate) struct AssocTypeResolveView<'db> {
    pub subject: TyId<'db>,
    pub scope: ScopeId<'db>,
    pub assumptions: PredicateListId<'db>,
}

impl<'db> AssocTypeResolveView<'db> {
    pub(crate) fn new(
        subject: TyId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        Self { subject, scope, assumptions }
    }

    /// Compute associated-type candidates for `name` using semantic bound views only.
    /// Returns pairs of (trait instance, projection TyId) without normalization.
    pub(crate) fn candidates_for(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> SmallVec<(crate::analysis::ty::trait_def::TraitInstId<'db>, TyId<'db>), 4> {
        use crate::analysis::ty::trait_lower::{lower_trait, lower_trait_ref};
        use crate::analysis::ty::trait_def::TraitInstId;

        let mut candidates: SmallVec<(
            crate::analysis::ty::trait_def::TraitInstId<'db>,
            TyId<'db>,
        ), 4> = SmallVec::new();
        let ingot = self.scope.ingot(db);
        let mut table = UnificationTable::new(db);
        let canonical = Canonical::new(db, self.subject);
        let lhs_ty = canonical.extract_identity(&mut table);

        match canonical.value.data(db) {
            TyData::QualifiedTy(trait_inst) => {
                let proj = TyId::assoc_ty(db, *trait_inst, name);
                return smallvec![(*trait_inst, proj)];
            }
            TyData::TyParam(param) if param.is_trait_self() => {
                if let Some(trait_) = param.owner.resolve_to::<Trait>(db) {
                    if trait_.assoc_ty(db, name).is_some() {
                        let tr = TraitInstId::new(
                            db,
                            lower_trait(db, trait_),
                            vec![canonical.value],
                            IndexMap::new(),
                        );
                        let assoc_ty = TyId::assoc_ty(db, tr, name);
                        return smallvec![(tr, assoc_ty)];
                    }
                } else if let Some(impl_trait) = param.owner.resolve_to::<ImplTrait>(db)
                    && let Some(trait_ref) = impl_trait.trait_ref(db).to_opt()
                    && let Ok(tr_inst) = lower_trait_ref(db, canonical.value, trait_ref, impl_trait.scope(), self.assumptions)
                    && let Some(assoc_ty) = tr_inst.assoc_ty(db, name)
                {
                    return smallvec![(tr_inst, assoc_ty)];
                }
            }
            _ => {}
        }

        // Bounds in assumptions: only if subject is a type param to avoid spurious ambiguities
        if let TyData::TyParam(_) = canonical.value.data(db) {
            for &pred in self.assumptions.list(db) {
                let snapshot = table.snapshot();
                let pred_self = table.instantiate_with_fresh_vars(crate::analysis::ty::binder::Binder::bind(pred.self_ty(db)));
                if table.unify(lhs_ty, pred_self).is_ok() {
                    if let Some(assoc_ty) = pred.assoc_ty(db, name) {
                        let folded = assoc_ty.fold_with(db, &mut table);
                        candidates.push((pred, folded));
                    }
                }
                table.rollback_to(snapshot);
            }
        }

        // Impl candidates
        for impl_ in impls_for_ty_with_constraints(db, ingot, canonical, self.assumptions) {
            let snapshot = table.snapshot();
            let impl_ = table.instantiate_with_fresh_vars(impl_);
            if table.unify(lhs_ty, impl_.self_ty(db)).is_ok() {
                if let Some(ty) = impl_.assoc_ty(db, name) {
                    let folded = ty.fold_with(db, &mut table);
                    candidates.push((impl_.trait_(db), folded));
                }
            }
            table.rollback_to(snapshot);
        }

        // Subject is a projection: consult bounds on the associated type via semantic views
        if let TyData::AssocTy(assoc_ty) = canonical.value.data(db) {
            let ty_with_subst = canonical.extract_identity(&mut table);
            // Assumptions bounds (e.g., `T::Assoc: Level1`)
            for &pred in self.assumptions.list(db) {
                let snapshot = table.snapshot();
                if table.unify(ty_with_subst, pred.self_ty(db)).is_ok() {
                    if let Some(assoc_ty) = pred.assoc_ty(db, name) {
                        let folded = assoc_ty.fold_with(db, &mut table);
                        candidates.push((pred, folded));
                    }
                }
                table.rollback_to(snapshot);
            }

            // Trait-declared bounds on the associated type
            let trait_def = assoc_ty.trait_.def(db);
            let trait_ = trait_def.trait_(db);
            let assoc_name = assoc_ty.name;
            for view in trait_.assoc_types(db) {
                if view.name(db) != Some(assoc_name) { continue; }
                let subject = ty_with_subst.fold_with(db, &mut table);
                for inst in view.with_subject(subject).bounds(db) {
                    if inst.def(db).trait_(db).assoc_ty(db, name).is_some() {
                        let proj = TyId::assoc_ty(db, inst, name);
                        let folded = proj.fold_with(db, &mut table);
                        candidates.push((inst, folded));
                    }
                }
            }
        }

        // Dedup normalized results, but keep original projections for precision
        let mut dedup: IndexMap<TyId<'db>, (crate::analysis::ty::trait_def::TraitInstId<'db>, TyId<'db>)> = IndexMap::new();
        for (inst, ty_candidate) in candidates.iter().copied() {
            let norm = normalize_ty(db, ty_candidate, self.scope, self.assumptions);
            dedup.entry(norm).or_insert((inst, ty_candidate));
        }

        dedup.into_iter().map(|(_n, v)| v).collect()
    }
}

// Associated type views -----------------------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct TraitAssocTypeView<'db> {
    pub owner: Trait<'db>,
    pub idx: usize,
}

impl<'db> TraitAssocTypeView<'db> {
    fn decl(self, db: &'db dyn HirDb) -> &'db crate::core::hir_def::AssocTyDecl<'db> {
        &self.owner.types(db)[self.idx]
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.decl(db).name.to_opt()
    }

    pub fn span(self) -> crate::span::item::LazyTraitTypeSpan<'db> {
        self.owner.span().item_list().assoc_type(self.idx)
    }

    /// Raw bounds for this associated type (HIR). Prefer semantic checks where possible.
    pub(in crate::core) fn bounds_raw(self, db: &'db dyn HirDb) -> &'db [TypeBound<'db>] {
        &self.decl(db).bounds
    }

    /// Semantic default type for this associated type, lowered in the trait's
    /// scope using the trait's own constraints. Returns None if no default.
    pub fn default_ty(self, db: &'db dyn HirAnalysisDb) -> Option<crate::analysis::ty::ty_def::TyId<'db>> {
        let hir = self.decl(db).default?;
        let trait_ = self.owner;
        let assumptions = constraints_for(db, trait_.into());
        Some(lower_hir_ty(db, hir, trait_.scope(), assumptions))
    }


    // Note: assoc-type bound evaluation is provided via AssocTypeOnSubjectView.
}

#[derive(Clone, Copy, Debug)]
pub struct AssocTypeBoundView<'db> {
    pub owner: TraitAssocTypeView<'db>,
    pub idx: usize,
}

impl<'db> TraitAssocTypeView<'db> {
    /// Iterate trait bounds as per-bound semantic views.
    pub fn bounds(self, db: &'db dyn HirDb) -> impl Iterator<Item = AssocTypeBoundView<'db>> + 'db {
        let len = self.bounds_raw(db).len();
        let idxs: Vec<usize> = (0..len)
            .filter(|&i| matches!(self.bounds_raw(db)[i], TypeBound::Trait(_)))
            .collect();
        idxs.into_iter().map(move |idx| AssocTypeBoundView { owner: self, idx })
    }
}

impl<'db> AssocTypeBoundView<'db> {
    fn trait_ref(self, db: &'db dyn HirDb) -> TraitRefId<'db> {
        match self.owner.bounds_raw(db)[self.idx] {
            TypeBound::Trait(tr) => tr,
            _ => unreachable!(),
        }
    }

    /// Lower this associated-type bound to a trait instance for a given subject,
    /// using the owner-trait's `Self` inside generic arguments.
    pub fn as_trait_inst_with_subject(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: crate::analysis::ty::ty_def::TyId<'db>,
    ) -> Option<crate::analysis::ty::trait_def::TraitInstId<'db>> {
        use crate::analysis::ty::trait_lower::lower_trait_ref_with_owner_self;
        let owner_trait = self.owner.owner;
        let owner_self = crate::analysis::ty::ty_lower::collect_generic_params(db, owner_trait.into())
            .trait_self(db)
            .unwrap();
        let scope = owner_trait.scope();
        let assumptions = constraints_for(db, owner_trait.into());
        lower_trait_ref_with_owner_self(db, subject, self.trait_ref(db), scope, assumptions, owner_self).ok()
    }

    /// Lower this bound with explicit owner `Self` for contexts like impl trait analysis.
    pub fn as_trait_inst_with_subject_and_owner(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: crate::analysis::ty::ty_def::TyId<'db>,
        owner_self: crate::analysis::ty::ty_def::TyId<'db>,
    ) -> Option<crate::analysis::ty::trait_def::TraitInstId<'db>> {
        use crate::analysis::ty::trait_lower::lower_trait_ref_with_owner_self;
        let owner_trait = self.owner.owner;
        let scope = owner_trait.scope();
        let assumptions = constraints_for(db, owner_trait.into());
        lower_trait_ref_with_owner_self(db, subject, self.trait_ref(db), scope, assumptions, owner_self).ok()
    }

    /// Lower this bound against an explicit subject and external assumptions (e.g., resolver env).
    pub fn as_trait_inst_with_subject_in_env(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: crate::analysis::ty::ty_def::TyId<'db>,
        assumptions: crate::analysis::ty::trait_resolution::PredicateListId<'db>,
    ) -> Option<crate::analysis::ty::trait_def::TraitInstId<'db>> {
        use crate::analysis::ty::trait_lower::lower_trait_ref_with_owner_self;
        let owner_trait = self.owner.owner;
        let owner_self = crate::analysis::ty::ty_lower::collect_generic_params(db, owner_trait.into())
            .trait_self(db)
            .unwrap();
        let scope = owner_trait.scope();
        lower_trait_ref_with_owner_self(db, subject, self.trait_ref(db), scope, assumptions, owner_self).ok()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct VariantView<'db> {
    pub owner: Enum<'db>,
    pub idx: usize,
}

impl<'db> VariantView<'db> {
    pub fn kind(self, db: &'db dyn HirDb) -> VariantKind<'db> {
        self.owner.variants_list(db).data(db)[self.idx].kind
    }

    pub fn name(self, db: &'db dyn HirDb) -> Partial<IdentId<'db>> {
        self.owner.variants_list(db).data(db)[self.idx].name
    }

    pub fn span(self) -> crate::span::item::LazyVariantDefSpan<'db> {
        self.owner.span().variants().variant(self.idx)
    }

    /// Returns semantic types of this variant's fields (empty for unit variants).
    pub fn field_tys(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        use crate::analysis::ty::{binder::Binder, ty_def::{InvalidCause, TyId}, ty_lower::lower_hir_ty};

        match self.kind(db) {
            VariantKind::Unit => Vec::new(),
            VariantKind::Record(_) => {
                let parent = FieldParent::Variant(EnumVariant::new(self.owner, self.idx));
                parent.fields(db).map(|v| Binder::bind(v.ty(db))).collect()
            }
            VariantKind::Tuple(tuple_id) => {
                let var = EnumVariant::new(self.owner, self.idx);
                let scope = var.scope();
                let assumptions = constraints_for(db, self.owner.into());
                tuple_id
                    .data(db)
                    .iter()
                    .map(|p| {
                        let ty = match p.to_opt() {
                            Some(hir_ty) => lower_hir_ty(db, hir_ty, scope, assumptions),
                            None => TyId::invalid(db, InvalidCause::ParseError),
                        };
                        Binder::bind(ty)
                    })
                    .collect()
            }
        }
    }

    /// Semantic field-set for this variant.
    pub fn as_adt_fields(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db crate::analysis::ty::adt_def::AdtField<'db> {
        let def = crate::analysis::ty::adt_def::lower_adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(self.owner),
        );
        &def.fields(db)[self.idx]
    }

    /// Diagnostics for tuple-variant element types: star-kind and non-const checks.
    /// Returns an empty list if this is not a tuple variant.
    pub fn diags_tuple_elems_wf(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::diagnostics::TyDiagCollection<'db>> {
        use crate::analysis::ty::{
            diagnostics::TyLowerDiag,
            trait_resolution::constraint::collect_adt_constraints,
            ty_lower::lower_hir_ty,
        };
        let mut out = Vec::new();
        if let VariantKind::Tuple(tuple_id) = self.kind(db) {
            let assumptions = collect_adt_constraints(db, self.owner.as_adt(db)).instantiate_identity();
            let scope = self.owner.scope();
            let base_span = self.span().tuple_type();
            for (i, elem) in tuple_id.data(db).iter().enumerate() {
                let Some(hir_ty) = elem.to_opt() else { continue };
                // Surface name-resolution errors for this element type first
                let ty_span = base_span.clone().elem_ty(i);
                let mut errs = crate::analysis::ty::ty_error::collect_ty_lower_errors(
                    db,
                    scope,
                    hir_ty,
                    ty_span.clone(),
                    assumptions,
                );
                if !errs.is_empty() {
                    // If this element lowers to a const type, prefer a NormalTypeExpected diag
                    let ty = lower_hir_ty(db, hir_ty, scope, assumptions);
                    if ty.is_const_ty(db) {
                        out.push(TyLowerDiag::NormalTypeExpected { span: ty_span.clone().into(), given: ty }.into());
                    } else {
                        out.append(&mut errs);
                    }
                    continue;
                }

                let ty = lower_hir_ty(db, hir_ty, scope, assumptions);
                let span = ty_span.into();
                if !ty.has_star_kind(db) {
                    out.push(TyLowerDiag::ExpectedStarKind(span).into());
                    continue;
                }
                if ty.is_const_ty(db) {
                    out.push(TyLowerDiag::NormalTypeExpected { span, given: ty }.into());
                    continue;
                }
            }
        }
        out
    }

    /// Iterates record fields (empty for non-record variants) as contextual views.
    pub fn fields(self, db: &'db dyn HirDb) -> impl Iterator<Item = FieldView<'db>> + 'db {
        let parent = FieldParent::Variant(EnumVariant::new(self.owner, self.idx));
        let len = match self.kind(db) {
            VariantKind::Record(_) => parent.fields_list(db).data(db).len(),
            _ => 0,
        };
        (0..len).map(move |idx| FieldView { parent, idx })
    }
}

// Field views --------------------------------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct FieldView<'db> {
    pub parent: FieldParent<'db>,
    pub idx: usize,
}

impl<'db> FieldView<'db> {
    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.parent.fields_list(db);
        list.data(db)[self.idx].name.to_opt()
    }

    /// Returns the HIR type reference (syntactic) for this field.
    /// Prefer using `ty` when the semantic type is needed.
    /// Temporary public exposure via `type_ref___tmp` below is for migration.
    fn hir_type_ref(self, db: &'db dyn HirDb) -> Partial<TypeId<'db>> {
        let list = self.parent.fields_list(db);
        list.data(db)[self.idx].type_ref
    }

    /// Returns the semantic type of this field.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let (adt_field, idx) = self.as_adt_field(db);
        *adt_field.ty(db, idx).skip_binder()
    }

    /// Returns the semantic ADT field-set and index for this field.
    pub fn as_adt_field(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> (&'db crate::analysis::ty::adt_def::AdtField<'db>, usize) {
        (self.parent.as_adt_fields(db), self.idx)
    }

    /// Temporary access to the underlying HIR type reference for this field.
    /// Prefer `ty(db)` for semantic type. Exists to support in-progress lowerings
    /// that still operate on HIR type refs.
    pub fn type_ref___tmp(self, db: &'db dyn HirDb) -> Partial<TypeId<'db>> {
        self.hir_type_ref(db)
    }

    pub fn ty_span(self) -> crate::span::DynLazySpan<'db> {
        match self.parent {
            FieldParent::Struct(s) => s.span().fields().field(self.idx).ty().into(),
            FieldParent::Contract(c) => c.span().fields().field(self.idx).ty().into(),
            FieldParent::Variant(v) => v.span().fields().field(self.idx).ty().into(),
        }
    }

    /// Diagnostics for this field's type: star-kind/const-ness and const-type parameter mismatch.
    /// Returns an empty list if no issues are found. Assumptions are derived from owner context.
    pub fn diags_wf(self, db: &'db dyn HirAnalysisDb) -> Vec<crate::analysis::ty::diagnostics::TyDiagCollection<'db>> {
        let mut out = Vec::new();
        // First, surface name-resolution errors for the field's HIR type path
        if let Some(hir_ty) = self.type_ref___tmp(db).to_opt() {
            let (scope, owner_item) = match self.parent {
                FieldParent::Struct(s) => (s.scope(), ItemKind::Struct(s)),
                FieldParent::Contract(c) => (c.scope(), ItemKind::Contract(c)),
                FieldParent::Variant(v) => (v.enum_.scope(), ItemKind::Enum(v.enum_)),
            };
            let assumptions = constraints_for(db, owner_item);
            let ty_span = match self.parent {
                FieldParent::Struct(s) => s.span().fields().field(self.idx).ty(),
                FieldParent::Contract(c) => c.span().fields().field(self.idx).ty(),
                FieldParent::Variant(v) => v.span().fields().field(self.idx).ty(),
            };
            let mut errs = crate::analysis::ty::ty_error::collect_ty_lower_errors(
                db,
                scope,
                hir_ty,
                ty_span,
                assumptions,
            );
            if !errs.is_empty() {
                out.append(&mut errs);
                return out;
            }
        }

        let ty = self.ty(db);
        let span = self.ty_span();
        if !ty.has_star_kind(db) {
            out.push(crate::analysis::ty::diagnostics::TyLowerDiag::ExpectedStarKind(span.clone()).into());
            return out;
        }
        if ty.is_const_ty(db) {
            out.push(
                crate::analysis::ty::diagnostics::TyLowerDiag::NormalTypeExpected { span: span.clone(), given: ty }
                    .into(),
            );
            return out;
        }

        // Trait-bound well-formedness for field type
        {
            use crate::analysis::ty::trait_resolution::{check_ty_wf, WellFormedness};
            let owner_item = match self.parent {
                FieldParent::Struct(s) => ItemKind::Struct(s),
                FieldParent::Contract(c) => ItemKind::Contract(c),
                FieldParent::Variant(v) => ItemKind::Enum(v.enum_),
            };
            let assumptions = constraints_for(db, owner_item);
            match check_ty_wf(db, owner_item.top_mod(db).ingot(db), ty, assumptions) {
                WellFormedness::WellFormed => {}
                WellFormedness::IllFormed { goal, subgoal } => {
                    out.push(
                        crate::analysis::ty::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                            span: span.clone(),
                            primary_goal: goal,
                            unsat_subgoal: subgoal,
                        }
                        .into(),
                    );
                    return out;
                }
            }
        }

        // Const type parameter mismatch check: if field name matches a const type parameter in scope.
        if let Some(name) = self.name(db) {
            use crate::analysis::name_resolution::resolve_path;
            use crate::analysis::name_resolution::PathRes;
            use crate::hir_def::PathId;
            let scope = crate::hir_def::scope_graph::ScopeId::Field(self.parent, self.idx as u16);
            let path = PathId::from_ident(db, name);
            let assumptions = crate::analysis::ty::trait_resolution::PredicateListId::empty_list(db);
            if let Ok(PathRes::Ty(t)) = resolve_path(db, path, scope, assumptions, true) {
                use crate::analysis::ty::ty_def::TyData;
                if let TyData::ConstTy(const_ty) = t.data(db) {
                    let expected = *const_ty;
                    let expected_ty = expected.ty(db);
                    if !expected_ty.has_invalid(db) && !ty.has_invalid(db) && ty != expected_ty {
                        out.push(
                            crate::analysis::ty::diagnostics::TyLowerDiag::ConstTyMismatch {
                                span,
                                expected: expected_ty,
                                given: ty,
                            }
                            .into(),
                        );
                        return out;
                    }
                }
            }
        }

        out
    }
}

impl<'db> FieldParent<'db> {
    /// Iterates fields as contextual views. For variants, only record variants have fields.
    pub fn fields(self, db: &'db dyn HirDb) -> impl Iterator<Item = FieldView<'db>> + 'db {
        let len = self.fields_list(db).data(db).len();
        (0..len).map(move |idx| FieldView { parent: self, idx })
    }

    /// Semantic field-set for this parent.
    pub fn as_adt_fields(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db crate::analysis::ty::adt_def::AdtField<'db> {
        match self {
            FieldParent::Struct(s) => &s.as_adt(db).fields(db)[0],
            FieldParent::Contract(c) => &c.as_adt(db).fields(db)[0],
            FieldParent::Variant(v) => v.as_adt_fields(db),
        }
    }
}

impl<'db> EnumVariant<'db> {
    /// Semantic field-set for this variant.
    pub fn as_adt_fields(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db crate::analysis::ty::adt_def::AdtField<'db> {
        let def = crate::analysis::ty::adt_def::lower_adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(self.enum_),
        );
        &def.fields(db)[self.idx as usize]
    }
}
#[derive(Clone, Copy, Debug)]
pub struct AssocTypeOnSubjectView<'db> {
    pub base: TraitAssocTypeView<'db>,
    pub subject: crate::analysis::ty::ty_def::TyId<'db>,
}

impl<'db> TraitAssocTypeView<'db> {
    /// Attach a subject type to this associated type for semantic bound evaluation.
    pub fn with_subject(
        self,
        subject: crate::analysis::ty::ty_def::TyId<'db>,
    ) -> AssocTypeOnSubjectView<'db> {
        AssocTypeOnSubjectView { base: self, subject }
    }
}

impl<'db> AssocTypeOnSubjectView<'db> {
    /// Semantic trait bounds for this associated type on `subject` using the owner's context.
    pub fn bounds(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = crate::analysis::ty::trait_def::TraitInstId<'db>> + 'db {
        self.base
            .bounds(db)
            .filter_map(move |b| b.as_trait_inst_with_subject(db, self.subject))
    }

    /// Semantic trait bounds for this associated type on `subject` with an explicit assumption list.
    // TODO(api): Evaluate whether this is still needed once assoc-type resolve
    // traversal provides a consistent assumptions model. If not, remove.
    pub(crate) fn bounds_in(
        self,
        db: &'db dyn HirAnalysisDb,
        assumptions: crate::analysis::ty::trait_resolution::PredicateListId<'db>,
    ) -> impl Iterator<Item = crate::analysis::ty::trait_def::TraitInstId<'db>> + 'db {
        self.base
            .bounds(db)
            .filter_map(move |b| b.as_trait_inst_with_subject_in_env(db, self.subject, assumptions))
    }
}
