//! Semantic diagnostics helpers.
//!
//! This module is the home for traversal API helpers that produce
//! `TyDiagCollection` / diagnostics. Over time, diagnostic-focused
//! logic from `core::semantic` is being migrated here to keep the main
//! traversal surface free of diagnostic concerns.

use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution;
use crate::analysis::ty;
use crate::analysis::ty::def_analysis::check_duplicate_names;
use crate::analysis::ty::diagnostics::{TyDiagCollection, TyLowerDiag, TraitConstraintDiag};
use crate::hir_def::{
    Contract, Enum, EnumVariant, FieldParent, GenericParam, GenericParamOwner, ItemKind, Struct,
    Trait, TypeAlias, TypeBound, VariantKind,
};
use crate::hir_def::{Partial, PathId};
use crate::hir_def::scope_graph::ScopeId;
use crate::span::DynLazySpan;

use super::{
    constraints_for, FieldView, Func, GenericParamView, Impl, ImplTrait, SuperTraitRefView,
    TraitAssocTypeView, VariantView, WhereClauseOwner, WhereClauseView, WherePredicateBoundView,
    WherePredicateView,
};

impl<'db> SuperTraitRefView<'db> {
    /// Diagnostics for this super-trait reference in its owner's context.
    /// Uses the trait's `Self` as subject and checks WF; kind mismatch is emitted
    /// elsewhere via `Trait::diags_super_traits`.
    pub fn diags(self, db: &'db dyn HirAnalysisDb) -> Option<TyDiagCollection<'db>> {
        use name_resolution::{diagnostics::PathResDiag, ExpectedPathKind};
        use ty::trait_lower::{self, TraitRefLowerError};
        use ty::trait_resolution::{check_trait_inst_wf, WellFormedness};

        let span = self.span();
        let subject = self.subject_self(db);
        let scope = self.owner.scope();
        let assumptions = self.assumptions(db);
        let tr = self.trait_ref_id(db);

        let inst = match trait_lower::lower_trait_ref(db, subject, tr, scope, assumptions) {
            Ok(i) => i,
            Err(TraitRefLowerError::PathResError(err)) => {
                let path = tr.path(db).unwrap();
                let diag = err.into_diag(db, path, span.path(), ExpectedPathKind::Trait)?;
                return Some(diag.into());
            }
            Err(TraitRefLowerError::InvalidDomain(res)) => {
                let path = tr.path(db).unwrap();
                let ident = path.ident(db).unwrap();
                return Some(
                    PathResDiag::ExpectedTrait(span.path().into(), ident, res.kind_name()).into(),
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
                TraitConstraintDiag::TraitBoundNotSat {
                    span: span.into(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                }
                .into(),
            ),
        }
    }
}

impl<'db> WherePredicateView<'db> {
    /// Aggregate diagnostics for this where-predicate:
    /// - Subject-level errors (const/concrete or path-domain remapped)
    /// - Per-bound trait diagnostics
    /// - Per-bound kind consistency
    pub fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();

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
                crate::hir_def::TypeBound::Trait(_) => {
                    let bview = WherePredicateBoundView::new(self, i);
                    out.extend(bview.diags(db));
                }
                crate::hir_def::TypeBound::Kind(crate::hir_def::Partial::Present(kb)) => {
                    let expected = super::lower_hir_kind_local(kb);
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
}

impl<'db> WherePredicateBoundView<'db> {
    /// Diagnostics for this trait bound, given an explicit subject type.
    /// Mirrors legacy visitor behavior for path errors, kind mismatch, and satisfiability.
    pub(in crate::core) fn diags_for_subject(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: ty::ty_def::TyId<'db>,
    ) -> Vec<TyDiagCollection<'db>> {
        use name_resolution::{diagnostics::PathResDiag, ExpectedPathKind};
        use ty::trait_lower::{self, TraitRefLowerError};
        use ty::trait_resolution::{check_trait_inst_wf, WellFormedness};

        let mut out = Vec::new();
        let owner_item = ItemKind::from(self.pred.clause.owner);
        let scope = owner_item.scope();
        let assumptions = constraints_for(db, owner_item);
        let is_trait_self_subject =
            matches!(owner_item, ItemKind::Trait(_)) && self.pred.is_self_subject(db);
        let tr = self.trait_ref(db);
        let span = self.trait_ref_span();

        match trait_lower::lower_trait_ref(db, subject, tr, scope, assumptions) {
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
                // do not emit unsatisfied bound diagnostics here.
                if !is_trait_self_subject {
                    match check_trait_inst_wf(db, scope.ingot(db), inst, assumptions) {
                        WellFormedness::WellFormed => {}
                        WellFormedness::IllFormed { goal, .. } => {
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
            }
            Err(TraitRefLowerError::PathResError(err)) => {
                if let Some(path) = tr.path(db).to_opt() {
                    if let Some(diag) =
                        err.into_diag(db, path, span.path(), ExpectedPathKind::Trait)
                    {
                        out.push(diag.into());
                    }
                }
            }
            Err(TraitRefLowerError::InvalidDomain(res)) => {
                if let Some(path) = tr.path(db).to_opt() {
                    if let Some(ident) = path.ident(db).to_opt() {
                        out.push(
                            PathResDiag::ExpectedTrait(span.path().into(), ident, res.kind_name())
                                .into(),
                        );
                    }
                }
            }
            Err(TraitRefLowerError::Ignored) => {}
        }

        out
    }

    /// Diagnostics for this trait bound, deriving the subject from the predicate's LHS.
    /// Returns a single-element vec with the subject error if subject lowering fails.
    pub fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        match self.pred.subject_ty_checked(db) {
            Ok(Some(subject)) => self.diags_for_subject(db, subject),
            Ok(None) => Vec::new(),
            Err(diag) => vec![diag],
        }
    }
}

impl<'db> Func<'db> {
    /// Diagnostics related to parameters (duplicate names/labels).
    pub fn diags_parameters(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();

        // Duplicate parameter names
        let dupes = ty::def_analysis::check_duplicate_names(
            self.param_views(db).map(|v| v.name(db)),
            |idxs| TyLowerDiag::DuplicateArgName(self, idxs).into(),
        );
        let found_dupes = !dupes.is_empty();
        diags.extend(dupes);

        // Duplicate labels (only if names were unique)
        if !found_dupes {
            diags.extend(ty::def_analysis::check_duplicate_names(
                self.param_views(db).map(|v| v.label_eagerly(db)),
                |idxs| TyLowerDiag::DuplicateArgLabel(self, idxs).into(),
            ));
        }

        diags
    }

    /// Diagnostics related to the explicit return type (kind/const checks).
    pub fn diags_return(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        if self.has_explicit_return_ty(db) {
            // First, surface name-resolution/path-domain errors on the return type itself
            if let Some(hir_ty) = self.ret_type_ref(db) {
                let assumptions = self.assumptions(db);
                let mut errs = ty::ty_error::collect_ty_lower_errors(
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

    /// Diagnostics for function parameter types:
    /// - For all params: star kind required and reject const types
    /// - For self param: enforce exact `Self` type shape
    /// Note: WF/invalid errors are still surfaced via the general type walker.
    pub fn diags_param_types(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use ty::diagnostics::ImplDiag;
        use ty::normalize::normalize_ty;
        use ty::trait_resolution::{check_ty_wf, WellFormedness};

        let mut out = Vec::new();
        let assumptions = self.assumptions(db);
        let expected_self = self.expected_self_ty(db);

        for view in self.param_views(db) {
            let Some(hir_ty) = view.hir_ty(db) else { continue };

            // Surface name-resolution errors for the parameter type first
            let span_node = if view.is_self_param(db) && view.self_ty_fallback(db) {
                view.span().fallback_self_ty()
            } else {
                view.span().ty()
            };
            let ty_span: DynLazySpan<'db> =
                if view.is_self_param(db) && view.self_ty_fallback(db) {
                    view.span().name().into()
                } else {
                    view.span().ty().into()
                };

            let mut errs = ty::ty_error::collect_ty_lower_errors(
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

            let ty = ty::ty_lower::lower_hir_ty(db, hir_ty, self.scope(), assumptions);
            if !ty.has_star_kind(db) {
                out.push(TyLowerDiag::ExpectedStarKind(ty_span.clone()).into());
                continue;
            }
            if ty.is_const_ty(db) {
                out.push(
                    TyLowerDiag::NormalTypeExpected {
                        span: ty_span.clone(),
                        given: ty,
                    }
                    .into(),
                );
                continue;
            }

            // Well-formedness / trait-bound satisfaction for parameter type
            match check_ty_wf(db, self.top_mod(db).ingot(db), ty, assumptions) {
                WellFormedness::WellFormed => {}
                WellFormedness::IllFormed { goal, subgoal } => {
                    out.push(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span: ty_span.clone(),
                            primary_goal: goal,
                            unsat_subgoal: subgoal,
                        }
                        .into(),
                    );
                }
            }

            if view.is_self_param(db) {
                if let Some(expected) = expected_self {
                    if !ty.has_invalid(db) && !expected.has_invalid(db) {
                        let (exp_base, exp_args) = expected.decompose_ty_app(db);
                        let ty_norm = normalize_ty(db, ty, self.scope(), assumptions);
                        let (ty_base, ty_args) = ty_norm.decompose_ty_app(db);
                        let same_base = ty_base == exp_base;
                        let same_args =
                            exp_args.iter().zip(ty_args.iter()).all(|(a, b)| a == b);
                        if !(same_base && same_args) {
                            out.push(
                                ImplDiag::InvalidSelfType {
                                    span: ty_span.clone(),
                                    expected,
                                    given: ty_norm,
                                }
                                .into(),
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
        use ty::func_def::lower_func;
        use ty::method_table::probe_method;
        use ty::{binder::Binder, canonical::Canonical};

        let mut out = Vec::new();
        out.extend(self.diags_parameters(db));
        out.extend(self.diags_param_types(db));

        // Defer body/type-check diagnostics to BodyAnalysisPass; here we only validate the
        // explicit return type annotation when present.
        out.extend(self.diags_return(db));

        // Where-clause diagnostics (trait bounds and kind checks on where predicates)
        {
            let clause =
                WhereClauseView { owner: WhereClauseOwner::Func(self), id: self.where_clause(db) };
            for pred in clause.predicates(db) {
                out.extend(pred.diags(db));
            }
        }

        // Method conflict check only for inherent impls
        if let Some(crate::hir_def::scope_graph::ScopeId::Item(ItemKind::Impl(impl_))) =
            self.scope().parent(db)
        {
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
                                ty::diagnostics::ImplDiag::ConflictMethodImpl {
                                    primary: func_def,
                                    conflict_with: cand,
                                }
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

impl<'db> TypeAlias<'db> {
    /// Diagnostics for alias target type and generics.
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
        let Some(hir_ty) = self.type_ref(db).to_opt() else {
            return out;
        };
        let ty = ty::ty_lower::lower_hir_ty(db, hir_ty, self.scope(), assumptions);
        if ty.has_invalid(db) {
            let diags = ty::ty_error::collect_ty_lower_errors(
                db,
                self.scope(),
                hir_ty,
                span.clone(),
                assumptions,
            );
            out.extend(diags);
        }
        let wf = ty::trait_resolution::check_ty_wf(
            db,
            self.top_mod(db).ingot(db),
            ty,
            assumptions,
        );
        if let ty::trait_resolution::WellFormedness::IllFormed { goal, subgoal } = wf {
            out.push(
                TraitConstraintDiag::TraitBoundNotSat {
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

impl<'db> Trait<'db> {
    /// Diagnostics for associated type defaults (bounds satisfaction), in the trait's context.
    pub fn diags_assoc_defaults(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        let assumptions = constraints_for(db, self.into());
        for assoc in self.assoc_types(db) {
            let Some(default_ty) = assoc.default_ty(db) else { continue };
            for trait_inst in assoc.with_subject(default_ty).bounds(db) {
                let canonical_inst = ty::canonical::Canonical::new(db, trait_inst);
                match ty::trait_resolution::is_goal_satisfiable(
                    db,
                    self.top_mod(db).ingot(db),
                    canonical_inst,
                    assumptions,
                ) {
                    ty::trait_resolution::GoalSatisfiability::Satisfied(_) => {}
                    ty::trait_resolution::GoalSatisfiability::UnSat(_) => {
                        diags.push(
                            TraitConstraintDiag::TraitBoundNotSat {
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
    pub fn diags_generic_params(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let owner = GenericParamOwner::Trait(self);
        let mut out: Vec<TyDiagCollection> = owner.diags_check_duplicate_names(db).collect();
        out.extend(owner.diags_params_defined_in_parent(db));
        out
    }

    /// Diagnostics for super-traits (semantic, kind-mismatch only).
    pub fn diags_super_traits(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use ty::trait_resolution::{check_trait_inst_wf, WellFormedness};

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
                match check_trait_inst_wf(
                    db,
                    self.top_mod(db).ingot(db),
                    inst,
                    view.assumptions(db),
                ) {
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
}

impl<'db> Impl<'db> {
    /// Preconditions and implementor-type diagnostics for this impl.
    pub fn diags_preconditions(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use ty::diagnostics::ImplDiag;
        use ty::trait_resolution::{check_ty_wf, WellFormedness};
        use ty::trait_resolution::constraint::ty_constraints;

        let mut out = Vec::new();

        let owner = GenericParamOwner::Impl(self);
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));

        if let Some(hir_ty) = self.type_ref(db).to_opt() {
            let assumptions = constraints_for(db, self.into());
            let mut errs = ty::ty_error::collect_ty_lower_errors(
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

        if let Some(diag) =
            ty::ty_error::emit_invalid_ty_error(db, ty, self.span().target_ty().into())
        {
            out.push(diag);
        }

        if ty.has_invalid(db) {
            return out;
        }

        match check_ty_wf(db, ingot, ty, constraints_for(db, self.into())) {
            WellFormedness::WellFormed => {
                let constraints = ty_constraints(db, ty);
                for &goal in constraints.list(db) {
                    if goal.self_ty(db).has_param(db) {
                        out.push(
                            TraitConstraintDiag::TraitBoundNotSat {
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
                    TraitConstraintDiag::TraitBoundNotSat {
                        span: self.span().target_ty().into(),
                        primary_goal: goal,
                        unsat_subgoal: subgoal,
                    }
                    .into(),
                );
            }
        }

        out
    }
}

impl<'db> ImplTrait<'db> {
    /// Diagnostics for missing associated types (required by the trait).
    pub fn diags_missing_assoc_types(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use ty::diagnostics::ImplDiag;
        use ty::trait_lower::lower_impl_trait;

        let mut diags = Vec::new();
        let Some(implementor) = lower_impl_trait(db, self) else {
            return diags;
        };
        let implementor = implementor.instantiate_identity();
        let trait_hir = implementor.trait_def(db).trait_(db);
        let impl_types = implementor.types(db);

        for assoc in trait_hir.assoc_types(db) {
            let Some(name) = assoc.name(db) else { continue };
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
    pub fn diags_assoc_types_bounds(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use ty::trait_lower::lower_impl_trait;

        let mut diags = Vec::new();
        let Some(implementor) = lower_impl_trait(db, self) else {
            return diags;
        };
        let implementor = implementor.instantiate_identity();
        let trait_hir = implementor.trait_def(db).trait_(db);
        let impl_types = implementor.types(db);
        let impl_trait_hir = implementor.hir_impl_trait(db);
        let assumptions = ty::trait_resolution::constraint::collect_constraints(db, impl_trait_hir.into())
            .instantiate_identity();

        for assoc in trait_hir.assoc_types(db) {
            let Some(name) = assoc.name(db) else { continue };
            let Some(&impl_ty) = impl_types.get(&name) else { continue };

            for b in assoc.bounds(db) {
                let Some(bound_inst) =
                    b.as_trait_inst_with_subject_and_owner(db, impl_ty, implementor.self_ty(db))
                else {
                    continue;
                };
                let canonical_bound = ty::canonical::Canonical::new(db, bound_inst);
                use ty::trait_resolution::{is_goal_satisfiable, GoalSatisfiability};
                if let GoalSatisfiability::UnSat(_) =
                    is_goal_satisfiable(db, self.top_mod(db).ingot(db), canonical_bound, assumptions)
                {
                    let assoc_ty_span = self
                        .associated_type_span(db, name)
                        .map(|s| s.ty().into())
                        .unwrap_or_else(|| self.span().ty().into());

                    diags.push(
                        TraitConstraintDiag::TraitBoundNotSat {
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
    pub fn diags_trait_ref_and_wf(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use ty::canonical::Canonicalized;
        use ty::trait_lower::lower_impl_trait;
        use ty::trait_resolution::{self, constraint::collect_constraints, GoalSatisfiability};

        let mut diags = Vec::new();
        let Some(implementor) = lower_impl_trait(db, self) else {
            return diags;
        };
        let implementor = implementor.instantiate_identity();
        let trait_inst = implementor.trait_(db);
        let trait_def = implementor.trait_def(db);

        let trait_constraints = collect_constraints(db, trait_def.trait_(db).into())
            .instantiate(db, trait_inst.args(db));

        let assumptions = collect_constraints(db, self.into()).instantiate_identity();
        let ingot = self.top_mod(db).ingot(db);

        let is_satisfied = |goal, span: DynLazySpan<'db>, out: &mut Vec<_>| {
            let canonical_goal = Canonicalized::new(db, goal);
            match trait_resolution::is_goal_satisfiable(db, ingot, canonical_goal.value, assumptions)
            {
                GoalSatisfiability::Satisfied(_) | GoalSatisfiability::ContainsInvalid => {}
                GoalSatisfiability::NeedsConfirmation(_) => {}
                GoalSatisfiability::UnSat(_) => {
                    out.push(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span,
                            primary_goal: goal,
                            unsat_subgoal: None,
                        }
                        .into(),
                    );
                }
            }
        };

        let trait_ref_span: DynLazySpan<'db> = self.span().trait_ref().into();
        for &goal in trait_constraints.list(db) {
            is_satisfied(goal, trait_ref_span.clone(), &mut diags);
        }

        let target_ty_span: DynLazySpan<'db> = self.span().ty().into();
        for &super_trait in trait_def.super_traits(db) {
            let super_trait = super_trait.instantiate(db, trait_inst.args(db));
            is_satisfied(super_trait, target_ty_span.clone(), &mut diags)
        }

        diags
    }

    /// Diagnostics for implemented associated types’ WF and invalid types.
    pub fn diags_assoc_types_wf(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use ty::trait_resolution::{check_ty_wf, WellFormedness};
        use ty::ty_error::collect_ty_lower_errors;
        use ty::ty_lower::lower_hir_ty;

        let mut diags = Vec::new();
        let ingot = self.top_mod(db).ingot(db);
        let assumptions = constraints_for(db, self.into());

        for (idx, def) in self.types(db).iter().enumerate() {
            let ty_span = self.span().associated_type(idx).ty();
            if let Some(hir) = def.type_ref.to_opt() {
                let errs =
                    collect_ty_lower_errors(db, self.scope(), hir, ty_span.clone(), assumptions);
                if !errs.is_empty() {
                    diags.extend(errs);
                    continue;
                }
                let ty = lower_hir_ty(db, hir, self.scope(), assumptions);
                if let WellFormedness::IllFormed { goal, subgoal } =
                    check_ty_wf(db, ingot, ty, assumptions)
                {
                    diags.push(
                        TraitConstraintDiag::TraitBoundNotSat {
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
}

impl<'db> Struct<'db> {
    /// Aggregate struct definition diagnostics.
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();

        out.extend(ty::def_analysis::check_duplicate_names(
            FieldParent::Struct(self).fields(db).map(|v| v.name(db)),
            |idxs| TyLowerDiag::DuplicateFieldName(FieldParent::Struct(self), idxs).into(),
        ));

        for v in FieldParent::Struct(self).fields(db) {
            out.extend(v.diags_wf(db));
        }

        let owner = GenericParamOwner::Struct(self);
        out.extend(owner.diags_const_param_types(db));
        out.extend(owner.diags_params_defined_in_parent(db));
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));

        {
            let clause =
                WhereClauseView { owner: WhereClauseOwner::Struct(self), id: self.where_clause(db) };
            for pred in clause.predicates(db) {
                out.extend(pred.diags(db));
            }
        }
        out
    }
}

impl<'db> VariantView<'db> {
    /// Diagnostics for tuple-variant element types: star-kind and non-const checks.
    /// Returns an empty list if this is not a tuple variant.
    pub fn diags_tuple_elems_wf(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::hir_def::types::TypeKind as HirTyKind;
        use name_resolution::{resolve_path, PathRes};
        use ty::ty_lower::lower_hir_ty;
        use ty::trait_resolution::{check_ty_wf, WellFormedness};

        let mut out = Vec::new();
        let VariantKind::Tuple(tuple_id) = self.kind(db) else {
            return out;
        };

        let enum_ = self.owner;
        let var = EnumVariant::new(enum_, self.idx);
        let scope = var.scope();
        let assumptions = constraints_for(db, enum_.into());

        for (elem_idx, p) in tuple_id.data(db).iter().enumerate() {
            let Some(hir_ty) = p.to_opt() else {
                continue;
            };

            let span = self.span().tuple_type().elem_ty(elem_idx);

            // For non-const subjects, surface name-resolution/path-domain errors first.
            let is_const_path = match hir_ty.data(db) {
                HirTyKind::Path(path) => {
                    if let Some(path) = path.to_opt() {
                        matches!(
                            resolve_path(db, path, scope, assumptions, true),
                            Ok(PathRes::Const(..))
                        )
                    } else {
                        false
                    }
                }
                _ => false,
            };

            if !is_const_path {
                let mut errs =
                    ty::ty_error::collect_ty_lower_errors(db, scope, hir_ty, span.clone(), assumptions);
                if !errs.is_empty() {
                    out.append(&mut errs);
                    continue;
                }
            }

            let ty = lower_hir_ty(db, hir_ty, scope, assumptions);
            if ty.has_invalid(db) {
                continue;
            }
            if !ty.has_star_kind(db) {
                out.push(TyLowerDiag::ExpectedStarKind(span.clone().into()).into());
                continue;
            }
            if ty.is_const_ty(db) {
                out.push(
                    TyLowerDiag::NormalTypeExpected {
                        span: span.clone().into(),
                        given: ty,
                    }
                    .into(),
                );
                continue;
            }

            // Trait-bound well-formedness for element type.
            match check_ty_wf(db, enum_.top_mod(db).ingot(db), ty, assumptions) {
                WellFormedness::WellFormed => {}
                WellFormedness::IllFormed { goal, subgoal } => {
                    out.push(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span: span.clone().into(),
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
}

impl<'db> FieldView<'db> {
    /// Diagnostics for this field's type: star-kind/const-ness and const-type parameter mismatch.
    /// Returns an empty list if no issues are found. Assumptions are derived from owner context.
    pub fn diags_wf(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use name_resolution::{resolve_path, PathRes};
        use ty::ty_def::TyData;
        use ty::ty_error::collect_ty_lower_errors;
        use ty::trait_resolution::{check_ty_wf, PredicateListId, WellFormedness};

        let mut out = Vec::new();

        // First, surface name-resolution errors for the field's HIR type path.
        if let Some(hir_ty) = self.hir_type_ref(db).to_opt() {
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
            let mut errs = collect_ty_lower_errors(db, scope, hir_ty, ty_span, assumptions);
            if !errs.is_empty() {
                out.append(&mut errs);
                return out;
            }
        }

        let ty = self.ty(db);
        let span = self.ty_span();
        if !ty.has_star_kind(db) {
            out.push(TyLowerDiag::ExpectedStarKind(span.clone()).into());
            return out;
        }
        if ty.is_const_ty(db) {
            out.push(
                TyLowerDiag::NormalTypeExpected {
                    span: span.clone(),
                    given: ty,
                }
                .into(),
            );
            return out;
        }

        // Trait-bound well-formedness for field type.
        {
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
                        TraitConstraintDiag::TraitBoundNotSat {
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

        // Const type parameter mismatch check: if field name matches a const type parameter.
        if let Some(name) = self.name(db) {
            let scope = crate::hir_def::scope_graph::ScopeId::Field(self.parent, self.idx as u16);
            let path = PathId::from_ident(db, name);
            let assumptions = PredicateListId::empty_list(db);
            if let Ok(PathRes::Ty(t)) = resolve_path(db, path, scope, assumptions, true) {
                if let TyData::ConstTy(const_ty) = t.data(db) {
                    let expected = *const_ty;
                    let expected_ty = expected.ty(db);
                    if !expected_ty.has_invalid(db) && !ty.has_invalid(db) && ty != expected_ty {
                        out.push(
                            TyLowerDiag::ConstTyMismatch {
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

impl<'db> Enum<'db> {
    /// Aggregate enum definition diagnostics.
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();

        out.extend(ty::def_analysis::check_duplicate_names(
            self.variants(db).map(|v| v.name(db)),
            |idxs| TyLowerDiag::DuplicateVariantName(self, idxs).into(),
        ));

        for v in self.variants(db) {
            if matches!(v.kind(db), VariantKind::Record(_)) {
                out.extend(ty::def_analysis::check_duplicate_names(
                    v.fields(db).map(|f| f.name(db)),
                    |idxs| {
                        TyLowerDiag::DuplicateFieldName(
                            FieldParent::Variant(EnumVariant::new(self, v.idx)),
                            idxs,
                        )
                        .into()
                    },
                ));
                for f in v.fields(db) {
                    out.extend(f.diags_wf(db));
                }
            } else if matches!(v.kind(db), VariantKind::Tuple(_)) {
                out.extend(v.diags_tuple_elems_wf(db));
            }
        }

        let owner = GenericParamOwner::Enum(self);
        out.extend(owner.diags_const_param_types(db));
        out.extend(owner.diags_params_defined_in_parent(db));
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));

        {
            let clause =
                WhereClauseView { owner: WhereClauseOwner::Enum(self), id: self.where_clause(db) };
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
        out.extend(ty::def_analysis::check_duplicate_names(
            FieldParent::Contract(self).fields(db).map(|v| v.name(db)),
            |idxs| TyLowerDiag::DuplicateFieldName(FieldParent::Contract(self), idxs).into(),
        ));
        for v in FieldParent::Contract(self).fields(db) {
            out.extend(v.diags_wf(db));
        }
        out
    }
}

impl<'db> GenericParamOwner<'db> {
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

    pub fn diags_const_param_types(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use ty::ty_def::{InvalidCause, TyData};

        let mut out = Vec::new();
        let param_set = ty::ty_lower::collect_generic_params(db, self);
        for view in self.params(db) {
            let GenericParam::Const(c) = view.param else {
                continue;
            };
            if c.ty.to_opt().is_none() {
                continue;
            }
            if let Some(ty) = param_set.param_by_original_idx(db, view.idx) {
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
                            out.push(
                                TyLowerDiag::ConstTyExpected {
                                    span: span.into(),
                                    expected,
                                }
                                .into(),
                            );
                        }
                        InvalidCause::ConstTyMismatch { expected, given } => {
                            out.push(
                                TyLowerDiag::ConstTyMismatch {
                                    span: span.into(),
                                    expected,
                                    given,
                                }
                                .into(),
                            );
                        }
                        _ => {}
                    }
                }
            }
        }
        out
    }

    pub fn diags_default_forward_refs(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use ty::{
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

            struct Collector<'db> {
                db: &'db dyn HirAnalysisDb,
                scope: ScopeId<'db>,
                out: Vec<usize>,
            }
            impl<'db> TyVisitor<'db> for Collector<'db> {
                fn db(&self) -> &'db dyn HirAnalysisDb {
                    self.db
                }
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

    pub fn diags_kind_bounds(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();
        let param_set = ty::ty_lower::collect_generic_params(db, self);

        for view in self.params(db) {
            let GenericParam::Type(tp) = view.param else { continue };
            let Some(ty) = param_set.param_by_original_idx(db, view.idx) else { continue };
            let actual = ty.kind(db);

            for (i, bound) in tp.bounds.iter().enumerate() {
                if let TypeBound::Kind(Partial::Present(kb)) = bound {
                    let expected = super::lower_hir_kind_local(kb);
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

    pub fn diags_trait_bounds(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use name_resolution::{diagnostics::PathResDiag, ExpectedPathKind};
        use ty::trait_lower::{self, TraitRefLowerError};
        use ty::trait_resolution::{check_trait_inst_wf, WellFormedness};

        let mut out = Vec::new();
        let param_set = ty::ty_lower::collect_generic_params(db, self);
        let scope = self.scope();
        let assumptions = constraints_for(db, self.into());

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
                match trait_lower::lower_trait_ref(db, subject, *tr, scope, assumptions) {
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
                            if let Some(diag) =
                                err.into_diag(db, path, span.path(), ExpectedPathKind::Trait)
                            {
                                out.push(diag.into());
                            }
                        }
                    }
                    Err(TraitRefLowerError::InvalidDomain(res)) => {
                        if let Some(path) = tr.path(db).to_opt() {
                            if let Some(ident) = path.ident(db).to_opt() {
                                out.push(
                                    PathResDiag::ExpectedTrait(
                                        span.path().into(),
                                        ident,
                                        res.kind_name(),
                                    )
                                    .into(),
                                );
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
