//! Semantic diagnostics helpers.
//!
//! This module is the home for traversal API helpers that produce
//! `TyDiagCollection` / diagnostics. Over time, diagnostic-focused
//! logic from `core::semantic` is being migrated here to keep the main
//! traversal surface free of diagnostic concerns.

use rustc_hash::FxHashMap;
use smallvec1::SmallVec;

use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution;
use crate::analysis::ty;
use crate::analysis::ty::diagnostics::{TraitConstraintDiag, TyDiagCollection, TyLowerDiag};
use crate::analysis::ty::generic_defaults::{default_dependencies, type_default_diags};
use crate::analysis::ty::method_table::{MethodProbe, probe_method};
use crate::analysis::ty::normalize::normalize_ty;
use crate::analysis::ty::trait_lower::lower_impl_trait;
use crate::analysis::ty::ty_def::{InvalidCause, TyId};
use crate::analysis::ty::ty_error::{collect_ty_lower_errors, emit_invalid_ty_error};
use crate::analysis::ty::ty_lower::generic_param_owner_assumptions;
use crate::hir_def::scope_graph::AssocTypeOwner;
use crate::hir_def::{
    Contract, Enum, EnumVariant, FieldParent, Func, GenericParam, GenericParamOwner,
    GenericParamView, IdentId, Impl, ImplTrait, ItemKind, Partial, PathId, Struct, Trait,
    TypeAlias, TypeBound, VariantKind, WhereClauseOwner,
};
use crate::span::DynLazySpan;

use crate::analysis::ty::adt_def::AdtRef;
use crate::analysis::ty::binder::Binder;
use crate::analysis::ty::trait_def::ImplementorId;
use crate::semantic::{
    FieldView, FuncParamView, ImplAssocTypeView, InherentImplAdmissibility, SuperTraitRefView,
    VariantView, WherePredicateBoundView, WherePredicateView, constraints_for,
    header_constraints_for, lower_hir_kind_local, param_env,
};

/// Unified "pull" diagnostics surface for HIR items and views.
pub trait Diagnosable<'db> {
    type Diagnostic;
    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic>;
}

fn associated_family_parameter_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: AssocTypeOwner<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let params = owner.generic_params(db).data(db);
    let mut out: Vec<TyDiagCollection<'db>> =
        check_duplicate_names(params.iter().map(|param| param.name().to_opt()), |idxs| {
            TyLowerDiag::DuplicateGenericParamName(
                ty::diagnostics::GenericParamListOwner::AssocType(owner),
                idxs,
            )
            .into()
        })
        .into_iter()
        .collect();
    let parent_scope = owner.scope().item().scope();
    for (idx, param) in params.iter().enumerate() {
        let span = owner.span().generic_params().param(idx);
        if let Some(name) = param.name().to_opt()
            && let Some(diag) = param_defined_in_parent(db, name, parent_scope, span.clone())
        {
            out.push(diag.into());
        }
        let GenericParam::Type(param) = param else {
            out.push(TyLowerDiag::AssocTypeConstParam(span.into()).into());
            continue;
        };
        if param.default_ty.is_some() {
            out.push(
                TyLowerDiag::AssocTypeParamDefault(
                    span.clone().into_type_param().default_ty().into(),
                )
                .into(),
            );
        }
        let subject = ty::ty_lower::assoc_type_param(db, owner, idx);
        for (bound_idx, bound) in param.bounds.iter().enumerate() {
            let TypeBound::Trait(trait_ref) = bound else {
                continue;
            };
            let bound_span = span
                .clone()
                .into_type_param()
                .bounds()
                .bound(bound_idx)
                .trait_bound();
            let AssocTypeOwner::Trait(trait_, _) = owner else {
                out.push(
                    ty::diagnostics::ImplDiag::AssocTypeParamBoundInImpl {
                        primary: bound_span.into(),
                    }
                    .into(),
                );
                continue;
            };
            let invalid = || -> TyDiagCollection<'db> {
                TyLowerDiag::InvalidAssocTypeParamBound {
                    span: bound_span.clone().into(),
                    param: param
                        .name
                        .to_opt()
                        .unwrap_or_else(|| IdentId::new(db, "_".to_string())),
                }
                .into()
            };
            let assumptions = constraints_for(db, trait_.into());
            let written = WrittenBound {
                trait_ref: *trait_ref,
                span: bound_span.clone(),
                scope: owner.scope(),
                assumptions,
            };
            let AssocTypeOwner::Trait(_, decl_idx) = owner else {
                continue;
            };
            let Some(lowered) = crate::core::semantic::lowered_assoc_type_parameter_bounds(
                db,
                trait_,
                decl_idx as u32,
            )
            .iter()
            .find(|lowered| lowered.param == idx && lowered.index == bound_idx) else {
                continue;
            };
            match lowered.bound.clone() {
                Ok(inst) => {
                    let solve_cx = ty::trait_resolution::TraitSolveCx::new(db, owner.scope())
                        .with_assumptions(
                            owner.with_parameter_bounds(db, param_env(db, trait_.into())),
                        );
                    out.extend(written.diags(db, subject, inst, solve_cx, invalid));
                }
                Err(error) => out.extend(written.lowering_diags(
                    db,
                    error,
                    "associated type parameter bound",
                    invalid,
                )),
            }
        }
    }
    out
}

/// A trait bound written on an associated type with type parameters, or on
/// one of its parameters.
struct WrittenBound<'db> {
    trait_ref: crate::hir_def::TraitRefId<'db>,
    span: crate::span::params::LazyTraitRefSpan<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: ty::trait_resolution::PredicateListId<'db>,
}

impl<'db> WrittenBound<'db> {
    /// Diagnostics for the bound once it has lowered to `inst`: the subject's
    /// kind, the bound's arguments, then the bound's own requirements and
    /// those of the family applications written in it, both proved in
    /// `solve_cx`.
    fn diags(
        &self,
        db: &'db dyn HirAnalysisDb,
        subject: TyId<'db>,
        inst: ty::trait_def::TraitInstId<'db>,
        solve_cx: ty::trait_resolution::TraitSolveCx<'db>,
        invalid: impl FnOnce() -> TyDiagCollection<'db>,
    ) -> Vec<TyDiagCollection<'db>> {
        let expected = inst.def(db).self_param(db).kind(db);
        if !expected.does_match(subject.kind(db)) {
            return vec![
                TraitConstraintDiag::TraitArgKindMismatch {
                    span: self.span.clone(),
                    expected: expected.clone(),
                    actual: subject,
                }
                .into(),
            ];
        }
        if inst.args(db).iter().any(|ty| ty.has_invalid(db))
            || inst
                .assoc_type_bindings(db)
                .values()
                .any(|ty| ty.has_invalid(db))
        {
            return self.argument_diags(db, invalid);
        }
        let wf = ty::trait_resolution::check_trait_inst_wf(db, solve_cx, inst);
        if wf.is_wf() {
            // Lowering may have resolved a family application written in the
            // arguments; its requirements are checked on the bound as written.
            return ty::ty_error::collect_trait_ref_application_errors(
                db,
                self.scope,
                self.trait_ref,
                self.span.clone(),
                solve_cx.assumptions(),
            );
        }
        wf.without_subgoal()
            .into_diag(self.span.clone().into())
            .into_iter()
            .collect()
    }

    /// Diagnostics for the bound when it did not lower to a trait.
    fn lowering_diags(
        &self,
        db: &'db dyn HirAnalysisDb,
        error: ty::trait_lower::TraitRefLowerError<'db>,
        context: &str,
        invalid: impl FnOnce() -> TyDiagCollection<'db>,
    ) -> Vec<TyDiagCollection<'db>> {
        let diags = trait_bound_lowering_diags(
            db,
            self.trait_ref,
            self.span.clone(),
            error,
            context,
            self.scope,
            self.assumptions,
        );
        if diags.is_empty() {
            vec![invalid()]
        } else {
            diags
        }
    }

    /// The errors in the types written as the bound's arguments, each where
    /// it is written, or `invalid` if none of them has one of its own.
    fn argument_diags(
        &self,
        db: &'db dyn HirAnalysisDb,
        invalid: impl FnOnce() -> TyDiagCollection<'db>,
    ) -> Vec<TyDiagCollection<'db>> {
        let diags = ty::ty_error::collect_trait_ref_arg_errors(
            db,
            self.scope,
            self.trait_ref,
            self.span.clone(),
            self.assumptions,
        );
        if diags.is_empty() {
            vec![invalid()]
        } else {
            diags
        }
    }
}

/// Shared helper for duplicate name diagnostics.
pub(crate) fn check_duplicate_names<'db, F>(
    names: impl Iterator<Item = Option<IdentId<'db>>>,
    create_diag: F,
) -> SmallVec<[TyDiagCollection<'db>; 2]>
where
    F: Fn(SmallVec<[u16; 4]>) -> TyDiagCollection<'db>,
{
    let mut defs = FxHashMap::<IdentId<'db>, SmallVec<[u16; 4]>>::default();
    for (i, name) in names.enumerate() {
        if let Some(name) = name {
            defs.entry(name).or_default().push(i as u16);
        }
    }
    defs.into_values()
        .filter_map(|idxs| (idxs.len() > 1).then_some(create_diag(idxs)))
        .collect()
}

fn const_ty_mismatch_diag<'db>(
    span: DynLazySpan<'db>,
    expected: TyId<'db>,
    given: TyId<'db>,
) -> TyDiagCollection<'db> {
    TyLowerDiag::ConstTyMismatch {
        span,
        expected,
        given,
    }
    .into()
}

fn cyclic_trait_ref_diag<'db>(span: DynLazySpan<'db>, context: &str) -> TyDiagCollection<'db> {
    TraitConstraintDiag::InfiniteBoundRecursion(
        span,
        format!("cyclic trait reference prevented lowering this {context}"),
    )
    .into()
}

/// The diagnostics for a written trait bound that did not lower to a trait.
/// A path that fails to resolve inside the bound's arguments, such as
/// `Missing` in `Needs<Missing>`, is reported where it is written.
fn trait_bound_lowering_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_ref: crate::hir_def::TraitRefId<'db>,
    span: crate::span::params::LazyTraitRefSpan<'db>,
    error: ty::trait_lower::TraitRefLowerError<'db>,
    context: &str,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: ty::trait_resolution::PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    use name_resolution::{ExpectedPathKind, diagnostics::PathResDiag};
    use ty::trait_lower::TraitRefLowerError;

    let Some(path) = trait_ref.path(db).to_opt() else {
        return Vec::new();
    };
    match error {
        TraitRefLowerError::PathResError(err)
            if std::iter::successors(Some(path), |path| path.parent(db))
                .any(|prefix| prefix == err.failed_at) =>
        {
            err.into_diag(db, path, span.path(), ExpectedPathKind::Trait)
                .map(Into::into)
                .into_iter()
                .collect()
        }
        TraitRefLowerError::PathResError(_) => {
            ty::ty_error::collect_trait_ref_arg_errors(db, scope, trait_ref, span, assumptions)
        }
        TraitRefLowerError::InvalidDomain(res) => path
            .ident(db)
            .to_opt()
            .map(|ident| {
                PathResDiag::ExpectedTrait(span.path().into(), ident, res.kind_name()).into()
            })
            .into_iter()
            .collect(),
        TraitRefLowerError::Cycle => vec![cyclic_trait_ref_diag(span.path().into(), context)],
        TraitRefLowerError::UnsafeLocalBoundBlanketImpl | TraitRefLowerError::Ignored => Vec::new(),
    }
}

impl<'db> SuperTraitRefView<'db> {
    /// Diagnostics for this super-trait reference in its owner's context.
    /// Uses the trait's `Self` as subject and checks WF; kind mismatch is emitted
    /// elsewhere via `Trait::diags_super_traits`.
    pub fn diags(self, db: &'db dyn HirAnalysisDb) -> Option<TyDiagCollection<'db>> {
        use name_resolution::{ExpectedPathKind, diagnostics::PathResDiag};
        use ty::trait_lower::{self, TraitRefLowerError};
        use ty::trait_resolution::check_trait_inst_wf;

        let span = self.span();
        let subject = self.subject_self(db);
        let scope = self.owner.scope();
        let assumptions = self.assumptions(db);
        let tr = self.trait_ref(db);

        let inst = match trait_lower::lower_trait_ref(db, subject, tr, scope, assumptions, None) {
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
            Err(TraitRefLowerError::Cycle) => {
                return Some(cyclic_trait_ref_diag(
                    span.path().into(),
                    "super-trait bound",
                ));
            }
            Err(TraitRefLowerError::UnsafeLocalBoundBlanketImpl | TraitRefLowerError::Ignored) => {
                return None;
            }
        };

        // Do not emit when subject contains assoc types of params
        if inst.self_ty(db).contains_assoc_ty_of_param(db) {
            return None;
        }

        check_trait_inst_wf(
            db,
            ty::trait_resolution::TraitSolveCx::new(db, scope)
                .with_assumptions(param_env(db, self.owner.into())),
            inst,
        )
        .into_diag(span.into())
    }
}

impl<'db> WherePredicateView<'db> {
    /// Aggregate diagnostics for this where-predicate:
    /// - Subject-level errors (const/concrete or path-domain remapped)
    /// - Per-bound trait diagnostics
    /// - Per-bound kind consistency
    pub fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let Some(subject) = self.subject_ty(db) else {
            return Vec::new();
        };

        if let Some(diag) = self.diag_subject_ty(db, subject) {
            return vec![diag];
        }

        let errors = self.subject_application_diags(db);
        if !errors.is_empty() {
            return errors;
        }

        self.bound_diags(db, subject)
    }

    /// The requirements of the family applications written in this
    /// predicate's subject. The subject is a written type like any other,
    /// whether or not its bounds can be decided.
    fn subject_application_diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let Some(hir_ty) = self.hir_pred(db).ty.to_opt() else {
            return Vec::new();
        };
        let owner_item = ItemKind::from(self.clause.owner);
        ty::ty_error::collect_application_requirement_errors(
            db,
            owner_item.scope(),
            hir_ty,
            self.span().ty(),
            header_constraints_for(db, owner_item),
        )
    }

    /// Only the requirements of the family applications written in this
    /// predicate, for owners whose where-clauses have no other checks.
    pub fn application_diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = self.subject_application_diags(db);
        for (idx, bound) in self.hir_pred(db).bounds.iter().enumerate() {
            if matches!(bound, TypeBound::Trait(_)) {
                out.extend(WherePredicateBoundView::new(self, idx).application_diags(db));
            }
        }
        out
    }

    /// Diagnostic for this predicate's subject type, if any:
    /// - Path-resolution domain errors are remapped to precise diagnostics.
    /// - Const subjects are rejected.
    /// - Fully concrete, non-generic subjects are rejected.
    fn diag_subject_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: TyId<'db>,
    ) -> Option<TyDiagCollection<'db>> {
        use crate::analysis::name_resolution::diagnostics::PathResDiag;
        use crate::analysis::name_resolution::{ExpectedPathKind, resolve_path};

        // Path-resolution failures are carried via the subject's InvalidCause.
        let owner_item = ItemKind::from(self.clause.owner);
        let assumptions = header_constraints_for(db, owner_item);
        if let Some(InvalidCause::PathResolutionFailed { path }) = subject.invalid_cause(db) {
            // Re-run name resolution on the failed path and surface a precise diagnostic
            // at the type path span within the where-predicate.
            let ty_span = self.span().ty().into_path_type().path();
            match resolve_path(db, path, owner_item.scope(), assumptions, false) {
                Ok(res) => {
                    // Resolved to a non-type domain
                    if let Some(ident) = path.ident(db).to_opt() {
                        let diag =
                            PathResDiag::ExpectedType(ty_span.into(), ident, res.kind_name());
                        return Some(diag.into());
                    }
                }
                Err(inner) => {
                    if let Some(diag) = inner.into_diag(db, path, ty_span, ExpectedPathKind::Type) {
                        return Some(diag.into());
                    }
                }
            }
        }
        let span: DynLazySpan<'db> = self.span().ty().into();

        // A limit in the written subject is reported here.
        if let Err(limit) = crate::analysis::ty::normalize::normalize_ty(
            db,
            subject,
            owner_item.scope(),
            assumptions,
        ) {
            return Some(limit.report(span).0);
        }

        if subject.is_const_ty(db) {
            return Some(TraitConstraintDiag::ConstTyBound(span, subject).into());
        }

        if !subject.has_invalid(db) && !subject.has_param(db) && !subject.has_projection(db) {
            return Some(TraitConstraintDiag::ConcreteTypeBound(span, subject).into());
        }

        None
    }
}

impl<'db> WherePredicateBoundView<'db> {
    /// Diagnostics for this trait bound, given an explicit subject type.
    /// Mirrors legacy visitor behavior for path errors, kind mismatch, and satisfiability.
    pub(crate) fn diags_for_subject(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: ty::ty_def::TyId<'db>,
    ) -> Vec<TyDiagCollection<'db>> {
        use ty::trait_lower;
        use ty::trait_resolution::check_trait_inst_wf;

        let mut out = Vec::new();
        let owner_item = ItemKind::from(self.pred.clause.owner);
        let scope = owner_item.scope();
        let assumptions = header_constraints_for(db, owner_item);
        let is_trait_self_subject =
            matches!(owner_item, ItemKind::Trait(_)) && self.pred.is_self_subject(db);
        let tr = self.trait_ref(db);
        let span = self.trait_ref_span();

        match trait_lower::lower_trait_ref(
            db,
            subject,
            tr,
            scope,
            assumptions,
            ty::trait_resolution::constraint::enclosing_trait_self_ty(db, scope),
        ) {
            Ok(inst) => {
                let expected = inst.def(db).self_param(db).kind(db);
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
                if !is_trait_self_subject
                    && let Some(diag) = check_trait_inst_wf(
                        db,
                        ty::trait_resolution::TraitSolveCx::new(db, scope)
                            .with_assumptions(param_env(db, owner_item)),
                        inst,
                    )
                    .without_subgoal()
                    .into_diag(span.into())
                {
                    out.push(diag);
                }
            }
            Err(error) => out.extend(trait_bound_lowering_diags(
                db,
                tr,
                span,
                error,
                "trait bound",
                scope,
                assumptions,
            )),
        }

        out
    }

    /// Diagnostics for this trait bound, deriving the subject from the predicate's LHS.
    /// Returns a single-element vec with the subject error if subject lowering fails.
    pub fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let subject = match self.pred.subject_ty(db) {
            Some(s) => s,
            None => return Vec::new(),
        };
        let mut out = self.diags_for_subject(db, subject);
        if out.is_empty() {
            // The bound's checks skip subjects they cannot decide, but the
            // arguments written in the bound are still applications to check.
            out.extend(self.application_diags(db));
        }
        out
    }

    /// The requirements of the family applications written in the bound's
    /// trait reference.
    pub fn application_diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let owner_item = ItemKind::from(self.pred.clause.owner);
        ty::ty_error::collect_trait_ref_application_errors(
            db,
            owner_item.scope(),
            self.trait_ref(db),
            self.trait_ref_span(),
            header_constraints_for(db, owner_item),
        )
    }
}

impl<'db> Func<'db> {
    pub fn diags_const_fn(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        // Const-safety diagnostics are handled by the const-check pass on the body.
        let _ = db;
        Vec::new()
    }

    /// Diagnostics related to parameters (duplicate names/labels).
    pub fn diags_parameters(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        check_duplicate_names(self.params(db).map(|v| v.name(db)), |idxs| {
            TyLowerDiag::DuplicateArgName(self, idxs).into()
        })
        .into_iter()
        .collect()
    }

    /// Diagnostics related to the explicit return type (kind/const checks).
    pub fn diags_return(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        if self.has_explicit_return_ty(db) {
            // First, surface name-resolution/path-domain errors on the return type itself
            let errs = self.ret_ty_errors(db);
            if !errs.is_empty() {
                return errs;
            }

            // Then run kind/const checks on the lowered semantic type
            let ret = self.return_ty(db);
            let span: DynLazySpan<'db> = self.span().ret_ty().into();
            let solve_cx = ty::trait_resolution::TraitSolveCx::new(db, self.scope())
                .with_assumptions(param_env(db, self.into()));
            if let Some(diag) = ty::ty_error::normalization_limit_diag(
                db,
                ret,
                self.scope(),
                self.assumptions(db),
                span.clone(),
            ) {
                return vec![diag];
            }
            if !ret.has_star_kind(db) {
                diags.push(TyLowerDiag::ExpectedStarKind(span).into());
            } else if ret.is_const_ty(db) {
                diags.push(TyLowerDiag::NormalTypeExpected { span, given: ret }.into());
            } else if ty::ty_contains_const_hole(db, ret) {
                diags.push(TyLowerDiag::ConstHoleInValuePosition { span, ty: ret }.into());
            } else if let wf = ty::trait_resolution::check_ty_wf(db, solve_cx, ret)
                && !wf.is_wf()
            {
                // Point at the written type inside a qualified path when that
                // is what is ill-formed.
                let precise = self.ret_ty_qualified_path_wf_diags(db, solve_cx);
                if precise.is_empty() {
                    diags.extend(wf.into_diag(span));
                } else {
                    diags.extend(precise);
                }
            }
        }
        diags
    }

    /// Diagnostics for function parameter types:
    /// - For all params: star kind required and reject const types
    /// - For self param: enforce exact `Self` type shape
    ///   Note: WF/invalid errors are still surfaced via the general type walker.
    pub fn diags_param_types(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        self.params(db).flat_map(|v| v.diags(db)).collect()
    }
}

impl<'db> Diagnosable<'db> for FuncParamView<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        self.ty_diags(db)
    }
}

impl<'db> Diagnosable<'db> for TypeAlias<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        let mut out = self.ty_errors(db);
        out.extend(self.ty_wf_errors(db));
        out.extend(GenericParamOwner::TypeAlias(self).diags(db));
        out
    }
}

/// Whether the default of `assoc` reaches itself through the trait's
/// defaults: through `Self::Other` (any arguments) where `Other` has a
/// default of this trait, and so on.
fn default_names_itself<'db>(
    db: &'db dyn HirAnalysisDb,
    assoc: crate::semantic::TraitAssocTypeView<'db>,
) -> bool {
    use crate::analysis::ty::ty_def::TyData;
    use crate::analysis::ty::visitor::{TyVisitor, walk_ty};
    struct SelfProjections<'db> {
        db: &'db dyn HirAnalysisDb,
        trait_: Trait<'db>,
        names: Vec<IdentId<'db>>,
    }
    impl<'db> TyVisitor<'db> for SelfProjections<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_ty(&mut self, ty: TyId<'db>) {
            if let TyData::AssocTy(projection) = ty.data(self.db)
                && projection.trait_.def(self.db) == self.trait_
                && projection.trait_.self_ty(self.db).is_trait_self(self.db)
            {
                self.names.push(projection.name);
            }
            walk_ty(self, ty);
        }
    }
    let crate::hir_def::scope_graph::AssocTypeOwner::Trait(trait_, _) = assoc.assoc_owner() else {
        return false;
    };
    let Some(start) = assoc.name(db) else {
        return false;
    };
    let defaults: FxHashMap<IdentId<'db>, TyId<'db>> = trait_
        .assoc_types(db)
        .filter_map(|other| Some((other.name(db)?, other.default_ty(db)?)))
        .collect();
    let mut seen = rustc_hash::FxHashSet::default();
    let mut pending = vec![start];
    while let Some(name) = pending.pop() {
        let Some(&default) = defaults.get(&name) else {
            continue;
        };
        let mut found = SelfProjections {
            db,
            trait_,
            names: Vec::new(),
        };
        found.visit_ty(default);
        for next in found.names {
            if next == start {
                return true;
            }
            if seen.insert(next) {
                pending.push(next);
            }
        }
    }
    false
}

/// Diagnostics for the default of an associated type with type parameters,
/// written over those parameters: the written type, its normalization and
/// its well-formedness.
fn family_default_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    assoc: crate::semantic::TraitAssocTypeView<'db>,
    default_ty: TyId<'db>,
    hir_ty: crate::hir_def::TypeId<'db>,
    assumptions: ty::trait_resolution::PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let span = assoc.span().ty();
    let source_diags =
        ty::ty_error::collect_hir_ty_diags(db, assoc.scope(), hir_ty, span.clone(), assumptions);
    if !source_diags.is_empty() {
        return source_diags;
    }
    // A default is one definition for every implementing type that does not
    // override it. One that names itself through the trait's own defaults,
    // with the same `Self`, needs itself for every such type: a cycle,
    // reported here once rather than at each impl that uses it.
    if default_names_itself(db, assoc) {
        return vec![
            ty::normalize::NormalizationLimit::Cycle
                .report(span.into())
                .0,
        ];
    }
    match ty::normalize::normalize_ty(db, default_ty, assoc.scope(), assumptions) {
        Err(limit) => return vec![limit.report(span.into()).0],
        Ok(normalized) => {
            if let Some(diag) = normalized.emit_diag(db, span.clone().into()) {
                return vec![diag];
            }
        }
    }
    let solve_cx =
        ty::trait_resolution::TraitSolveCx::new(db, assoc.scope()).with_assumptions(assumptions);
    ty::trait_resolution::check_ty_wf(db, solve_cx, default_ty)
        .into_diag(span.into())
        .into_iter()
        .collect()
}

impl<'db> Trait<'db> {
    /// Diagnostics for the bounds declared on associated types with type
    /// parameters. Such a bound is promised for every argument, so it is
    /// checked here, once, over the declaration's own parameters.
    pub fn diags_assoc_output_bounds(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        for assoc in self.assoc_types(db) {
            if assoc.generic_params(db).data(db).is_empty() {
                // Nothing else checks the arguments written in these bounds.
                for bound in assoc.bounds(db) {
                    diags.extend(ty::ty_error::collect_trait_ref_application_errors(
                        db,
                        assoc.scope(),
                        bound.trait_ref(db),
                        assoc.span().bounds().bound(bound.index()).trait_bound(),
                        constraints_for(db, self.into()),
                    ));
                }
                continue;
            }
            let (Some(name), Some(subject)) = (assoc.name(db), assoc.formal_subject(db)) else {
                continue;
            };
            let assumptions = assoc.with_parameter_bounds(db, param_env(db, self.into()));
            let solve_cx = ty::trait_resolution::TraitSolveCx::new(db, assoc.scope())
                .with_assumptions(assumptions);
            for bound in assoc.bounds(db) {
                let span = assoc.span().bounds().bound(bound.index()).trait_bound();
                let invalid = || -> TyDiagCollection<'db> {
                    TyLowerDiag::InvalidAssocTypeBound {
                        span: span.clone().into(),
                        name,
                    }
                    .into()
                };
                let written = WrittenBound {
                    trait_ref: bound.trait_ref(db),
                    span: span.clone(),
                    scope: assoc.scope(),
                    assumptions,
                };
                match bound.lower_trait_inst(
                    db,
                    subject,
                    self.self_param(db),
                    assoc.scope(),
                    assumptions,
                ) {
                    Ok(inst) => diags.extend(written.diags(db, subject, inst, solve_cx, invalid)),
                    Err(error) => diags.extend(written.lowering_diags(
                        db,
                        error,
                        "associated type bound",
                        invalid,
                    )),
                }
            }
        }
        diags
    }

    /// Diagnostics for associated type defaults (bounds satisfaction), in the trait's context.
    pub fn diags_assoc_defaults(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        let assumptions = param_env(db, self.into());
        for assoc in self.assoc_types(db) {
            let Some(default_ty) = assoc.default_ty(db) else {
                continue;
            };
            if !assoc.assoc_owner().lowers_body(db) {
                // Reported once, at the parameter.
                continue;
            }
            let Some(hir_ty) = assoc.default_hir_ty(db) else {
                continue;
            };
            let assumptions = assoc.with_parameter_bounds(db, assumptions);
            let is_family = !assoc.generic_params(db).data(db).is_empty();
            let written = if is_family {
                family_default_diags(db, assoc, default_ty, hir_ty, assumptions)
            } else {
                // A plain default is checked where it is used; here only the
                // family applications written in it.
                ty::ty_error::collect_application_requirement_errors(
                    db,
                    assoc.scope(),
                    hir_ty,
                    assoc.span().ty(),
                    assumptions,
                )
            };
            if !written.is_empty() {
                diags.extend(written);
                continue;
            }
            for trait_inst in assoc.bounds_on_subject(db, default_ty) {
                let solve_cx = ty::trait_resolution::TraitSolveCx::new(db, self.scope())
                    .with_assumptions(assumptions);
                if is_family {
                    let holds = ty::trait_resolution::bound_is_proved(db, solve_cx, trait_inst);
                    if let Err(limit) = holds {
                        diags.push(limit.report(assoc.span().ty().into()).0);
                    } else if holds == Ok(false) {
                        diags.push(
                            TraitConstraintDiag::TraitBoundNotSat {
                                span: assoc.span().ty().into(),
                                primary_goal: trait_inst,
                                unsat_subgoal: None,
                                required_by: None,
                                capability_hint: None,
                            }
                            .into(),
                        );
                    }
                    continue;
                }
                match ty::trait_resolution::is_goal_satisfiable(db, solve_cx, trait_inst) {
                    Ok(ty::trait_resolution::GoalSatisfiability::UnSat(_)) => {
                        diags.push(
                            TraitConstraintDiag::TraitBoundNotSat {
                                span: self.span().into(),
                                primary_goal: trait_inst,
                                unsat_subgoal: None,
                                required_by: None,
                                capability_hint: None,
                            }
                            .into(),
                        );
                    }
                    Err(limit) => diags.push(limit.report(assoc.span().ty().into()).0),
                    Ok(
                        ty::trait_resolution::GoalSatisfiability::Satisfied(_)
                        | ty::trait_resolution::GoalSatisfiability::NeedsConfirmation { .. }
                        | ty::trait_resolution::GoalSatisfiability::ContainsInvalid,
                    ) => {}
                }
            }
        }
        diags
    }

    /// Diagnostics for trait bounds on associated types whose path does not
    /// name a trait, such as `type Out: Missing`.
    pub fn diags_assoc_type_bounds(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        let scope = self.scope();
        let assumptions = constraints_for(db, self.into());
        let self_ty = self.self_param(db);
        for assoc in self.assoc_types(db) {
            // The bounds of an associated type with type parameters are
            // lowered over those parameters, in `diags_assoc_output_bounds`.
            if !assoc.generic_params(db).data(db).is_empty() {
                continue;
            }
            for bound in assoc.bounds(db) {
                let trait_ref = bound.trait_ref(db);
                let Err(error) = ty::trait_lower::lower_trait_ref(
                    db,
                    self_ty,
                    trait_ref,
                    scope,
                    assumptions,
                    Some(self_ty),
                ) else {
                    continue;
                };
                let span = assoc.span().bounds().bound(bound.index()).trait_bound();
                diags.extend(trait_bound_lowering_diags(
                    db,
                    trait_ref,
                    span,
                    error,
                    "associated type bound",
                    scope,
                    assumptions,
                ));
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
        use ty::trait_resolution::check_trait_inst_wf;

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
                let wf = check_trait_inst_wf(
                    db,
                    ty::trait_resolution::TraitSolveCx::new(db, self.scope())
                        .with_assumptions(param_env(db, self.into())),
                    inst,
                );
                if wf.is_wf() {
                    diags.extend(ty::ty_error::collect_trait_ref_application_errors(
                        db,
                        self.scope(),
                        view.trait_ref(db),
                        view.span(),
                        param_env(db, self.into()),
                    ));
                } else {
                    diags.extend(wf.without_subgoal().into_diag(view.span().into()));
                }
            }
        }
        diags
    }
}

impl<'db> Impl<'db> {
    /// Impl-specific preconditions and implementor-type diagnostics.
    /// Generic parameter diagnostics are handled by `Diagnosable::diags`.
    pub fn diags_preconditions(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use ty::diagnostics::ImplDiag;

        let mut out = self.ty_errors(db);
        match self.inherent_impl_admissibility(db) {
            InherentImplAdmissibility::Admissible { .. } => {}
            InherentImplAdmissibility::NotAllowed { ty, is_nominal } => {
                let base = ty.base_ty(db);
                out.push(
                    ImplDiag::InherentImplIsNotAllowed {
                        primary: self.span().target_ty().into(),
                        ty: base.pretty_print(db).to_string(),
                        is_nominal,
                    }
                    .into(),
                );
                return out;
            }
            InherentImplAdmissibility::InvalidTy { ty } => {
                if out.is_empty()
                    && let Some(diag) =
                        ty::ty_error::emit_invalid_ty_error(db, ty, self.span().target_ty().into())
                {
                    out.push(diag);
                }
                return out;
            }
            // An error found in the written type already explains it.
            InherentImplAdmissibility::IllFormed { .. } if !out.is_empty() => {}
            InherentImplAdmissibility::IllFormed { error, .. } => {
                out.extend(error.into_diag(self.span().target_ty().into()));
            }
        }

        out
    }

    /// Declaration diagnostics for associated consts in inherent impl blocks:
    /// every const must have a value, and body checking runs in `BodyAnalysisPass`.
    pub fn diags_assoc_consts(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use ty::diagnostics::ImplDiag;

        let mut diags = Vec::new();
        let assumptions = constraints_for(db, self.into());
        let target_enum = self
            .admissible_inherent_impl_ty(db)
            .and_then(|ty| ty.as_enum(db));
        let mut seen: rustc_hash::FxHashMap<IdentId<'db>, DynLazySpan<'db>> = Default::default();
        for impl_const in self.assoc_consts(db) {
            let Some(name) = impl_const.name(db) else {
                continue;
            };

            let name_span: DynLazySpan = impl_const.span().name().into();
            // Duplicate within this same impl block.
            if let Some(first_span) = seen.get(&name) {
                diags.push(
                    ImplDiag::InherentConstConflict {
                        primary: name_span.clone(),
                        conflict_with: first_span.clone(),
                        const_name: name,
                    }
                    .into(),
                );
                continue;
            }
            seen.insert(name, name_span.clone());

            // Conflict with an overlapping *other* inherent impl block. Caught
            // here so an unreferenced duplicate is still diagnosed (path
            // resolution would otherwise only report it at a use site).
            if let Some(other) =
                crate::analysis::name_resolution::earliest_conflicting_inherent_const_impl(
                    db, self, name,
                )
                && let Some(conflict_with) = other
                    .assoc_consts(db)
                    .find(|c| c.name(db) == Some(name))
                    .map(|c| c.span().name().into())
            {
                diags.push(
                    ImplDiag::InherentConstConflict {
                        primary: name_span,
                        conflict_with,
                        const_name: name,
                    }
                    .into(),
                );
                continue;
            }

            // A const sharing a variant's name could never be referenced
            // (variants take precedence in path resolution), so reject it.
            if let Some(enum_) = target_enum
                && let Some(variant) = enum_.variants(db).find(|v| v.name(db) == Some(name))
            {
                let variant_span = EnumVariant::new(enum_, variant.idx).span().name().into();
                diags.push(
                    ImplDiag::InherentConstShadowsVariant {
                        primary: name_span,
                        variant_span,
                        const_name: name,
                    }
                    .into(),
                );
                continue;
            }

            // A const that shares a name with an inherent function shadows it:
            // `S::name` resolves to the const, so the function is unreachable.
            if let Some(fn_span) = name_resolution::shadowed_inherent_fn_for_const(db, self, name) {
                diags.push(
                    ImplDiag::InherentConstShadowsFn {
                        primary: name_span,
                        fn_span,
                        const_name: name,
                    }
                    .into(),
                );
                continue;
            }

            if impl_const.value_body(db).is_none() {
                diags.push(
                    ImplDiag::InherentConstMissingValue {
                        primary: impl_const.span().ty().into(),
                        const_name: name,
                    }
                    .into(),
                );
                continue;
            }

            // Report unresolvable/invalid type annotations; checking the body
            // against an invalid expected type would only produce noise.
            if let Some(hir_ty) = impl_const.hir_ty(db) {
                let ty_diags = ty::ty_error::collect_hir_ty_diags(
                    db,
                    self.scope(),
                    hir_ty,
                    impl_const.span().ty(),
                    assumptions,
                );
                if !ty_diags.is_empty() {
                    diags.extend(ty_diags);
                }
            }

            // The const value body is type-checked by `BodyAnalysisPass`, which
            // surfaces a value/declared-type mismatch as a plain `TypeMismatch`
            // (same as a top-level `const`); nothing is reported for it here.
        }
        diags
    }
}

impl<'db> ImplTrait<'db> {
    pub fn diags_associated_family_signatures(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use ty::diagnostics::ImplDiag;
        let mut out = Vec::new();
        let trait_ = self.trait_def(db);
        for (idx, definition) in self.assoc_types(db).enumerate() {
            let owner = AssocTypeOwner::Impl(self, idx as u16);
            out.extend(associated_family_parameter_diags(db, owner));
            let (Some(trait_), Some(name)) = (trait_, definition.name(db)) else {
                continue;
            };
            let Some(declaration) = trait_
                .assoc_types(db)
                .find(|decl| decl.name(db) == Some(name))
            else {
                // diags_assoc_types already reports the undeclared member.
                continue;
            };
            let declared_owner = declaration.assoc_owner();
            let expected = declaration.generic_params(db).data(db);
            let given = definition.generic_params(db).data(db);
            if expected.len() != given.len() {
                out.push(
                    ImplDiag::AssocTypeParamNumMismatch {
                        primary: definition.span().name().into(),
                        expected: expected.len(),
                        given: given.len(),
                    }
                    .into(),
                );
                continue;
            }
            for (param_idx, (expected, given)) in expected.iter().zip(given).enumerate() {
                // A const parameter is reported where it is written.
                if matches!(expected, GenericParam::Const(_))
                    || matches!(given, GenericParam::Const(_))
                {
                    continue;
                }
                let expected = ty::ty_lower::assoc_type_param_kind(db, declared_owner, param_idx);
                let given = ty::ty_lower::assoc_type_param_kind(db, owner, param_idx);
                if expected != given {
                    out.push(
                        ImplDiag::AssocTypeParamKindMismatch {
                            primary: definition.span().generic_params().param(param_idx).into(),
                            expected,
                            given,
                        }
                        .into(),
                    );
                }
            }
        }
        out
    }

    fn diags_effect_handle_raw(
        self,
        db: &'db dyn HirAnalysisDb,
        implementor: ImplementorId<'db>,
    ) -> Vec<TyDiagCollection<'db>> {
        // A limit in `Target` or `Raw` is reported where the impl defines
        // them, as for every associated type definition.
        let Some((raw_ty, failure)) = ty::provider::effect_handle_impl_raw_failure(db, implementor)
        else {
            return Vec::new();
        };
        let raw = IdentId::new(db, "Raw".to_string());
        vec![
            ty::diagnostics::ImplDiag::InvalidEffectHandleRaw {
                primary: self
                    .associated_type_span(db, raw)
                    .map_or_else(|| self.span().ty().into(), |span| span.ty().into()),
                raw_ty,
                failure,
            }
            .into(),
        ]
    }

    /// Normalization limits in the impl's where clauses, reported where they
    /// are written, with the impl's parameters kept abstract. Only limits:
    /// an impl's where clauses are conditions for using it, so a clause that
    /// does not hold is not an error here.
    fn diags_where_clause_limits(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let owner = ItemKind::from(self);
        let scope = owner.scope();
        let assumptions = header_constraints_for(db, owner);
        let mut out = Vec::new();
        for pred in WhereClauseOwner::ImplTrait(self).clause(db).predicates(db) {
            let Some(subject) = pred.subject_ty(db) else {
                continue;
            };
            if let Some(diag) = ty::ty_error::normalization_limit_diag(
                db,
                subject,
                scope,
                assumptions,
                pred.span().ty().into(),
            ) {
                out.push(diag);
                continue;
            }
            for bound in pred.bounds(db) {
                let Ok(inst) = ty::trait_lower::lower_trait_ref(
                    db,
                    subject,
                    bound.trait_ref(db),
                    scope,
                    assumptions,
                    None,
                ) else {
                    continue;
                };
                if let Some(diag) = inst.args(db).iter().skip(1).find_map(|&arg| {
                    ty::ty_error::normalization_limit_diag(
                        db,
                        arg,
                        scope,
                        assumptions,
                        bound.trait_ref_span().into(),
                    )
                }) {
                    out.push(diag);
                }
            }
        }
        out
    }

    /// Lower the implementor view and report validity diagnostics (WF, conflicts, kind mismatch).
    /// Returns the implementor view if successful, or None if critical errors occurred.
    pub(crate) fn diags_implementor_validity(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> (Option<ImplementorId<'db>>, Vec<TyDiagCollection<'db>>) {
        self.implementor_with_errors(db)
    }

    /// Diagnostics for missing associated types and types not declared in the trait.
    pub fn diags_assoc_types(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use ty::diagnostics::ImplDiag;
        use ty::trait_lower::lower_impl_trait;

        let mut diags = Vec::new();
        let Some(implementor) = lower_impl_trait(db, self) else {
            return diags;
        };
        let trait_hir = implementor.trait_def(db);
        let impl_types = implementor.types(db);

        for (idx, assoc) in self.types(db).iter().enumerate() {
            let Some(name) = assoc.name.to_opt() else {
                continue;
            };
            if trait_hir.assoc_ty(db, name).is_none() {
                diags.push(
                    ImplDiag::TypeNotDefinedInTrait {
                        primary: self.span().associated_type(idx).name().into(),
                        trait_: trait_hir,
                        type_name: name,
                    }
                    .into(),
                );
            }
        }

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

    /// Diagnostics for missing associated consts (required by the trait).
    pub fn diags_missing_assoc_consts(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use ty::diagnostics::ImplDiag;
        use ty::trait_lower::lower_impl_trait;

        let mut diags = Vec::new();
        let Some(implementor) = lower_impl_trait(db, self) else {
            return diags;
        };
        let trait_hir = implementor.trait_def(db);

        // Check that all required trait consts are implemented
        for trait_const in trait_hir.assoc_consts(db) {
            let Some(name) = trait_const.name(db) else {
                continue;
            };
            let has_impl = self.const_(db, name).is_some();
            let has_default = trait_const.has_default(db);
            if !has_impl && !has_default {
                diags.push(
                    ImplDiag::MissingAssociatedConst {
                        primary: self.span().ty().into(),
                        const_name: name,
                        trait_: trait_hir,
                    }
                    .into(),
                );
            }
        }
        diags
    }

    /// Diagnostics for associated const values and validity.
    pub fn diags_assoc_consts(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use ty::diagnostics::ImplDiag;

        let Some(implementor) = lower_impl_trait(db, self) else {
            return Vec::new();
        };
        let trait_hir = implementor.trait_def(db);
        let trait_args = implementor.trait_(db).args(db);

        let mut diags = Vec::new();
        for impl_const in self.assoc_consts(db) {
            let Some(name) = impl_const.name(db) else {
                continue;
            };

            if trait_hir.const_(db, name).is_some() {
                // Const is defined in trait - check it has a value
                if !impl_const.has_value(db) {
                    diags.push(
                        ImplDiag::MissingAssociatedConstValue {
                            primary: impl_const.span().ty().into(),
                            const_name: name,
                            trait_: trait_hir,
                        }
                        .into(),
                    );
                }
            } else {
                // Const is not defined in trait
                diags.push(
                    ImplDiag::ConstNotDefinedInTrait {
                        primary: impl_const.span().name().into(),
                        trait_: trait_hir,
                        const_name: name,
                    }
                    .into(),
                );
            }
        }

        // Validate the impl const's declared (header) type: surface lowering
        // errors, and require it to match the trait's declaration. Body
        // checking lives in the body analysis pass.
        for impl_const in self.assoc_consts(db) {
            let Some(name) = impl_const.name(db) else {
                continue;
            };
            let Some(impl_header_ty) = impl_const.ty(db) else {
                continue;
            };
            let scope = self.scope();
            let assumptions = constraints_for(db, self.into());
            if impl_header_ty.has_invalid(db) {
                let errs = impl_const.hir_ty(db).map(|hir_ty| {
                    collect_ty_lower_errors(db, scope, hir_ty, impl_const.span().ty(), assumptions)
                });
                match errs {
                    Some(errs) if !errs.is_empty() => diags.extend(errs),
                    _ => {
                        if let Some(diag) =
                            impl_header_ty.emit_diag(db, impl_const.span().ty().into())
                        {
                            diags.push(diag);
                        }
                    }
                }
                continue;
            }

            let Some(trait_const) = trait_hir.const_(db, name) else {
                continue;
            };
            let Some(expected) = trait_const.ty_binder(db) else {
                continue;
            };
            let expected_ty = expected.instantiate(db, trait_args);
            if expected_ty.has_invalid(db) {
                continue;
            }

            let span: DynLazySpan<'db> = impl_const.span().ty().into();
            let normalized =
                normalize_ty(db, expected_ty, scope, assumptions).and_then(|expected| {
                    normalize_ty(db, impl_header_ty, scope, assumptions)
                        .map(|header| (expected, header))
                });
            let (expected_ty, impl_header_ty) = match normalized {
                Ok(pair) => pair,
                Err(limit) => {
                    // The types cannot be compared: report the limit at the
                    // impl's constant.
                    diags.push(limit.report(span).0);
                    continue;
                }
            };
            if expected_ty != impl_header_ty {
                diags.push(
                    ImplDiag::ConstTyMismatchWithTrait {
                        primary: impl_const.span().ty().into(),
                        trait_decl_span: trait_const.span().ty().into(),
                        const_name: name,
                        trait_ty: expected_ty,
                        impl_ty: impl_header_ty,
                    }
                    .into(),
                );
            }
        }

        // Const value bodies are type-checked by the body analysis pass
        // (`check_impl_trait_const_bodies`), which surfaces a value/declared-type
        // mismatch as a plain `TypeMismatch`, same as a top-level `const`.

        diags
    }

    /// Diagnostics for associated consts with recursive definitions
    /// (`const C: u32 = Self::C`, or cycles through other consts).
    ///
    /// On concrete impls every const is forced and must reach a concrete
    /// value: recursion either surfaces as a `RecursiveConst` evaluation
    /// error (through the eval-query cycle recovery) or "evaluates" to a
    /// symbolic self-reference (the salsa cycle in `evaluate_const_ty`
    /// recovers with the unevaluated form). On generic impls a
    /// param-dependent const legitimately stays abstract, so recursion is
    /// instead detected by walking the abstract form's resolution chain.
    pub fn diags_assoc_const_evaluability(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        use ty::assoc_const::AssocConstUse;
        use ty::const_ty::{
            ConstTyData, const_body_resolution_reenters, const_ty_from_assoc_const_use,
        };
        use ty::diagnostics::ImplDiag;

        // Recursion is user-written; expanded impls are compiler output.
        if !matches!(self.origin(db), crate::span::HirOrigin::Raw(_)) {
            return Vec::new();
        }
        let Some(implementor) = lower_impl_trait(db, self) else {
            return Vec::new();
        };
        let trait_hir = implementor.trait_def(db);
        let inst = implementor.trait_inst(db);
        let scope = self.scope();
        let assumptions = constraints_for(db, self.into());

        let mut diags = Vec::new();
        for trait_const in trait_hir.assoc_consts(db) {
            let Some(name) = trait_const.name(db) else {
                continue;
            };
            let assoc = AssocConstUse::new(scope, assumptions, inst, name);
            let Some(const_ty) = const_ty_from_assoc_const_use(db, assoc) else {
                continue;
            };
            let declared_ty = trait_const
                .ty_binder(db)
                .map(|binder| binder.instantiate(db, inst.args(db)));
            let evaluated = const_ty.evaluate(db, declared_ty);
            if matches!(
                evaluated.data(db),
                ConstTyData::Value(..) | ConstTyData::Description(..)
            ) {
                continue;
            }
            if evaluated.ty(db).has_invalid(db) {
                // Other invalid causes are reported by the body/header
                // checks; recursion surfacing as an eval error is this
                // diagnostic's job.
                if !matches!(
                    evaluated.ty(db).invalid_cause(db),
                    Some(ty::ty_def::InvalidCause::ConstEvalRecursiveConst { .. })
                ) {
                    continue;
                }
            } else {
                // A non-evaluated, non-invalid result is only recursion when
                // the const-ref resolution chain actually loops back to this
                // body. Anything else — generic-impl deferral, ambiguous or
                // otherwise erroneous references — is the body checks' job.
                let ConstTyData::UnEvaluated {
                    body: start_body,
                    ty: Some(start_ty),
                    capture,
                    ..
                } = const_ty.data(db)
                else {
                    continue;
                };
                if start_ty.has_invalid(db)
                    || !const_body_resolution_reenters(db, *start_body, *start_ty, capture)
                {
                    continue;
                }
            }
            let primary = match self.const_(db, name) {
                Some(impl_const) => impl_const.span().name().into(),
                None => self.span().ty().into(),
            };
            diags.push(
                ImplDiag::RecursiveAssocConst {
                    primary,
                    const_name: name,
                }
                .into(),
            );
        }
        diags
    }

    /// Diagnostics for associated type bounds on implemented assoc types.
    pub fn diags_assoc_types_bounds(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        let Some(implementor) = lower_impl_trait(db, self) else {
            return diags;
        };
        let trait_args = implementor.trait_(db).args(db);
        let assumptions = param_env(db, self.into());

        for assoc in implementor.assoc_type_views(db) {
            let Some(name) = assoc.name(db) else { continue };
            let assumptions = assumptions.extended_with(db, assoc.parameter_bounds(db));

            for bound_inst in assoc.bounds(db) {
                let bound_inst = Binder::bind(implementor.trait_def(db).into(), bound_inst)
                    .instantiate(db, trait_args);
                use ty::trait_resolution::{
                    GoalSatisfiability, TraitSolveCx, bound_is_proved, is_goal_satisfiable,
                };
                let solve_cx = TraitSolveCx::new(db, self.scope()).with_assumptions(assumptions);
                let assoc_ty_span = || -> crate::span::DynLazySpan<'db> {
                    self.associated_type_span(db, name)
                        .map_or_else(|| self.span().ty().into(), |s| s.ty().into())
                };
                // A family's bound is promised for every argument, so it needs
                // a completed proof; a plain type's needs only not to fail.
                let failed = if assoc.impl_ty().is_type_family(db) {
                    bound_is_proved(db, solve_cx, bound_inst).map(|holds| !holds)
                } else {
                    is_goal_satisfiable(db, solve_cx, bound_inst)
                        .map(|answer| matches!(answer, GoalSatisfiability::UnSat(_)))
                };
                match failed {
                    Ok(false) => continue,
                    Err(limit) => {
                        diags.push(limit.report(assoc_ty_span()).0);
                        continue;
                    }
                    Ok(true) => {}
                }
                {
                    let assoc_ty_span = assoc_ty_span();

                    diags.push(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span: assoc_ty_span,
                            primary_goal: bound_inst,
                            unsat_subgoal: None,
                            required_by: None,
                            capability_hint: None,
                        }
                        .into(),
                    );
                }
            }
        }
        diags
    }

    /// Diagnostics for trait-ref WF and satisfiability for this impl-trait.
    pub fn diags_trait_ref_and_wf(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use ty::trait_lower::lower_impl_trait;
        use ty::trait_resolution::{self, GoalSatisfiability, check_trait_inst_wf};

        let mut diags = Vec::new();
        let Some(implementor) = lower_impl_trait(db, self) else {
            return diags;
        };
        let trait_inst = implementor.trait_(db);
        let trait_def = implementor.trait_def(db);

        let solve_cx = trait_resolution::TraitSolveCx::new(db, self.scope())
            .with_assumptions(param_env(db, self.into()));

        if let Some(diag) =
            check_trait_inst_wf(db, solve_cx, trait_inst).into_diag(self.span().trait_ref().into())
        {
            diags.push(diag);
            return diags;
        }

        let is_satisfied = |goal, span: DynLazySpan<'db>, out: &mut Vec<_>| {
            match trait_resolution::is_goal_satisfiable(db, solve_cx, goal) {
                Ok(
                    GoalSatisfiability::Satisfied(_)
                    | GoalSatisfiability::ContainsInvalid
                    | GoalSatisfiability::NeedsConfirmation { .. },
                ) => {}
                Ok(GoalSatisfiability::UnSat(_)) => {
                    out.push(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span,
                            primary_goal: goal,
                            unsat_subgoal: None,
                            required_by: None,
                            capability_hint: None,
                        }
                        .into(),
                    );
                }
                Err(limit) => {
                    out.push(limit.report(span).0);
                }
            }
        };

        let target_ty_span: DynLazySpan<'db> = self.span().ty().into();
        for super_trait in trait_def.super_traits(db) {
            let super_trait = super_trait.instantiate(db, trait_inst.args(db));
            is_satisfied(super_trait, target_ty_span.clone(), &mut diags)
        }

        diags
    }

    /// Diagnostics for implemented associated types' WF and invalid types.
    pub fn diags_assoc_types_wf(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        // A trait default is one generic definition, checked once at the
        // trait; an impl that does not override it gets it by substitution
        // and is not checked again here.
        self.assoc_types(db)
            .flat_map(|view| view.diags(db))
            .collect()
    }
}

impl<'db> Diagnosable<'db> for ImplAssocTypeView<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        self.ty_diags(db)
    }
}

impl<'db> Diagnosable<'db> for Struct<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        let mut out = Vec::new();

        out.extend(check_duplicate_names(
            FieldParent::Struct(self).fields(db).map(|v| v.name(db)),
            |idxs| TyLowerDiag::DuplicateFieldName(FieldParent::Struct(self), idxs).into(),
        ));

        for v in FieldParent::Struct(self).fields(db) {
            out.extend(v.diags(db));
        }

        for pred in WhereClauseOwner::Struct(self).clause(db).predicates(db) {
            out.extend(pred.diags(db));
        }

        out.extend(GenericParamOwner::Struct(self).diags(db));
        out
    }
}

impl<'db> VariantView<'db> {
    /// Diagnostics for tuple-variant element types: star-kind and non-const checks.
    /// Returns an empty list if this is not a tuple variant.
    pub fn diags_tuple_elems_wf(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::hir_def::types::TypeKind as HirTyKind;
        use name_resolution::{PathRes, resolve_path};
        use ty::trait_resolution::{TraitSolveCx, check_ty_wf};
        use ty::ty_lower::lower_hir_ty;

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
                let mut errs = ty::ty_error::collect_ty_lower_errors(
                    db,
                    scope,
                    hir_ty,
                    span.clone(),
                    assumptions,
                );
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
            if let Some(diag) = check_ty_wf(
                db,
                TraitSolveCx::new(db, scope).with_assumptions(param_env(db, enum_.into())),
                ty,
            )
            .into_diag(span.clone().into())
            {
                out.push(diag);
            }
        }

        out
    }
}

impl<'db> Diagnosable<'db> for FieldView<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        self.ty_diags(db)
    }
}

impl<'db> Diagnosable<'db> for Enum<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        let mut out = Vec::new();

        out.extend(check_duplicate_names(
            self.variants(db).map(|v| v.name(db)),
            |idxs| TyLowerDiag::DuplicateVariantName(self, idxs).into(),
        ));

        for v in self.variants(db) {
            if matches!(v.kind(db), VariantKind::Record(_)) {
                out.extend(check_duplicate_names(
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
                    out.extend(f.diags(db));
                }
            } else if matches!(v.kind(db), VariantKind::Tuple(_)) {
                out.extend(v.diags_tuple_elems_wf(db));
            }
        }

        for pred in WhereClauseOwner::Enum(self).clause(db).predicates(db) {
            out.extend(pred.diags(db));
        }

        out.extend(GenericParamOwner::Enum(self).diags(db));
        out
    }
}

impl<'db> Diagnosable<'db> for Contract<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        let mut out = Vec::new();
        out.extend(check_duplicate_names(
            FieldParent::Contract(self).fields(db).map(|v| v.name(db)),
            |idxs| TyLowerDiag::DuplicateFieldName(FieldParent::Contract(self), idxs).into(),
        ));
        for v in FieldParent::Contract(self).fields(db) {
            out.extend(v.diags(db));
        }
        out
    }
}

impl<'db> Diagnosable<'db> for AdtRef<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        match self {
            AdtRef::Struct(s) => s.diags(db),
            AdtRef::Enum(e) => e.diags(db),
        }
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
        let params_iter = self.params(db).map(|v| v.name().to_opt());
        check_duplicate_names(params_iter, |idxs| {
            TyDiagCollection::from(TyLowerDiag::DuplicateGenericParamName(self.into(), idxs))
        })
        .into_iter()
    }

    pub fn diags_non_trailing_defaults(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();
        let mut default_idxs = Vec::new();
        for view in self.params(db) {
            if view.param.has_default() {
                default_idxs.push(view.idx);
            } else if !default_idxs.is_empty() {
                for &idx in &default_idxs {
                    let span = self.param_view(db, idx).span();
                    out.push(TyLowerDiag::NonTrailingDefaultGenericParam(span).into());
                }
                break;
            }
        }
        out
    }

    pub fn diags_const_param_types(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use ty::ty_def::{InvalidCause, TyData};

        let mut out = Vec::new();
        let param_set = ty::ty_lower::collect_generic_params(db, self);
        for view in self.params(db) {
            let GenericParam::Const(c) = view.param else {
                continue;
            };
            let Some(hir_ty) = c.ty.to_opt() else {
                continue;
            };
            let span = view.span().into_const_param().ty();
            let mut source_diags = collect_ty_lower_errors(
                db,
                self.scope(),
                hir_ty,
                span.clone(),
                generic_param_owner_assumptions(db, self.scope()),
            );
            if !source_diags.is_empty() {
                out.append(&mut source_diags);
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
                            out.push(const_ty_mismatch_diag(span.into(), expected, given));
                        }
                        cause => {
                            if let Some(diag) =
                                emit_invalid_ty_error(db, TyId::invalid(db, cause), span.into())
                            {
                                out.push(diag);
                            }
                        }
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
        let mut out = Vec::new();
        for view in self.params(db) {
            for &j in default_dependencies(db, self, view.idx)
                .iter()
                .filter(|&&j| j >= view.idx)
            {
                if let Some(name) = self.param_view(db, j).param.name().to_opt() {
                    let span = view.span();
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
            let GenericParam::Type(tp) = view.param else {
                continue;
            };
            let Some(ty) = param_set.param_by_original_idx(db, view.idx) else {
                continue;
            };
            let actual = ty.kind(db);

            for (i, bound) in tp.bounds.iter().enumerate() {
                if let TypeBound::Kind(Partial::Present(kb)) = bound {
                    let expected = lower_hir_kind_local(kb);
                    if !actual.does_match(&expected) {
                        let span = view.span().into_type_param().bounds().bound(i).kind_bound();
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
        use ty::trait_lower;
        use ty::trait_resolution::check_trait_inst_wf;

        let mut out = Vec::new();
        let param_set = ty::ty_lower::collect_generic_params(db, self);
        let scope = self.scope();
        let assumptions = header_constraints_for(db, self.into());

        for view in self.params(db) {
            let GenericParam::Type(tp) = view.param else {
                continue;
            };
            let Some(subject) = param_set.param_by_original_idx(db, view.idx) else {
                continue;
            };

            for (i, bound) in tp.bounds.iter().enumerate() {
                let TypeBound::Trait(tr) = bound else {
                    continue;
                };
                let span = view
                    .span()
                    .into_type_param()
                    .bounds()
                    .bound(i)
                    .trait_bound();
                match trait_lower::lower_trait_ref(
                    db,
                    subject,
                    *tr,
                    scope,
                    assumptions,
                    ty::trait_resolution::constraint::enclosing_trait_self_ty(db, scope),
                ) {
                    Ok(inst) => {
                        let expected = inst.def(db).self_param(db).kind(db);
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

                        let wf = if inst.self_ty(db).contains_assoc_ty_of_param(db) {
                            ty::trait_resolution::WellFormedness::WellFormed
                        } else {
                            check_trait_inst_wf(
                                db,
                                ty::trait_resolution::TraitSolveCx::new(db, scope)
                                    .with_assumptions(param_env(db, self.into())),
                                inst,
                            )
                        };
                        if wf.is_wf() {
                            // The bound's own check skips subjects it cannot
                            // decide; the types written in it are still checked.
                            out.extend(ty::ty_error::collect_trait_ref_application_errors(
                                db,
                                scope,
                                *tr,
                                span,
                                assumptions,
                            ));
                        } else {
                            out.extend(wf.without_subgoal().into_diag(span.into()));
                        }
                    }
                    Err(error) => {
                        out.extend(trait_bound_lowering_diags(
                            db,
                            *tr,
                            span,
                            error,
                            "trait bound",
                            scope,
                            assumptions,
                        ));
                    }
                }
            }
        }

        out
    }
}

impl<'db> GenericParamView<'db> {
    pub fn diag_param_defined_in_parent(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<TyLowerDiag<'db>> {
        let name = self.param.name().to_opt()?;
        let parent_scope = self.owner.scope().parent_item(db)?.scope();
        param_defined_in_parent(db, name, parent_scope, self.span())
    }
}

/// A generic parameter named `name` that hides a parameter of the item at
/// `parent_scope`.
fn param_defined_in_parent<'db>(
    db: &'db dyn HirAnalysisDb,
    name: IdentId<'db>,
    parent_scope: crate::hir_def::scope_graph::ScopeId<'db>,
    span: crate::span::params::LazyGenericParamSpan<'db>,
) -> Option<TyLowerDiag<'db>> {
    use crate::analysis::name_resolution::{PathRes, resolve_path};
    use crate::analysis::ty::trait_resolution::PredicateListId;

    let path = PathId::from_ident(db, name);
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

impl<'db> Diagnosable<'db> for GenericParamOwner<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        let mut out = Vec::new();
        out.extend(self.diags_check_duplicate_names(db));
        out.extend(self.diags_const_param_types(db));
        out.extend(self.diags_params_defined_in_parent(db));
        out.extend(self.diags_kind_bounds(db));
        out.extend(self.diags_trait_bounds(db));
        out.extend(self.diags_non_trailing_defaults(db));
        out.extend(self.diags_default_forward_refs(db));
        out.extend(type_default_diags(db, self));
        out
    }
}

impl<'db> Diagnosable<'db> for Func<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        let mut out = Vec::new();
        out.extend(self.diags_const_fn(db));
        out.extend(self.diags_parameters(db));
        out.extend(self.diags_param_types(db));
        out.extend(self.diags_return(db));

        for pred in WhereClauseOwner::Func(self).clause(db).predicates(db) {
            out.extend(pred.diags(db));
        }

        // Method conflict check only for inherent impls
        if let Some(crate::hir_def::scope_graph::ScopeId::Item(ItemKind::Impl(impl_))) =
            self.scope().parent(db)
            && let Some(func_def) = self.as_callable(db)
            && let Some(self_ty) = impl_.admissible_inherent_impl_ty(db)
        {
            let ingot = self.top_mod(db).ingot(db);
            // A limit here is in the impl's self type, which is reported
            // where it is written.
            for cand in probe_method(
                db,
                ingot,
                MethodProbe {
                    receiver: self_ty,
                    assumptions: param_env(db, impl_.into()),
                },
                self.scope(),
                func_def.name(db).expect("impl methods have names"),
            )
            .unwrap_or_default()
            {
                if cand.def != func_def {
                    out.push(
                        ty::diagnostics::ImplDiag::ConflictMethodImpl {
                            primary: func_def,
                            conflict_with: cand.def,
                        }
                        .into(),
                    );
                    break;
                }
            }
        }

        out.extend(GenericParamOwner::Func(self).diags(db));
        out
    }
}

impl<'db> Diagnosable<'db> for Trait<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        let mut out = Vec::new();
        let assumptions = constraints_for(db, self.into());
        for assoc in self.assoc_consts(db) {
            if let Some(hir_ty) = assoc.hir_ty(db) {
                out.extend(ty::ty_error::collect_hir_ty_diags(
                    db,
                    self.scope(),
                    hir_ty,
                    assoc.span().ty(),
                    assumptions,
                ));
            }
        }
        for (idx, _) in self.assoc_types(db).enumerate() {
            out.extend(associated_family_parameter_diags(
                db,
                AssocTypeOwner::Trait(self, idx as u16),
            ));
        }
        out.extend(self.diags_assoc_output_bounds(db));
        out.extend(self.diags_assoc_defaults(db));
        out.extend(self.diags_assoc_type_bounds(db));
        out.extend(self.diags_super_traits(db));

        for pred in WhereClauseOwner::Trait(self).clause(db).predicates(db) {
            out.extend(pred.diags(db));
        }

        out.extend(GenericParamOwner::Trait(self).diags(db));
        out
    }
}

impl<'db> Diagnosable<'db> for Impl<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        let mut out = self.diags_preconditions(db);
        out.extend(self.diags_assoc_consts(db));
        for pred in WhereClauseOwner::Impl(self).clause(db).predicates(db) {
            out.extend(pred.application_diags(db));
        }
        out.extend(GenericParamOwner::Impl(self).diags(db));
        out
    }
}

impl<'db> Diagnosable<'db> for ImplTrait<'db> {
    type Diagnostic = TyDiagCollection<'db>;

    fn diags(self, db: &'db dyn HirAnalysisDb) -> Vec<Self::Diagnostic> {
        // Early path/domain/WF checks; bail out on errors to avoid noisy follow-ups
        let mut signature_diags = self.diags_associated_family_signatures(db);
        let (implementor_opt, validity_diags) = self.diags_implementor_validity(db);
        let Some(implementor) = implementor_opt else {
            signature_diags.extend(validity_diags);
            return signature_diags;
        };

        let mut out = validity_diags;
        out.extend(signature_diags);
        out.extend(implementor.diags_method_conformance(db));
        out.extend(self.diags_where_clause_limits(db));
        out.extend(self.diags_effect_handle_raw(db, implementor));
        out.extend(self.diags_trait_ref_and_wf(db));
        out.extend(self.diags_assoc_types_wf(db));
        out.extend(self.diags_assoc_types(db));
        out.extend(self.diags_assoc_types_bounds(db));
        out.extend(self.diags_missing_assoc_consts(db));
        out.extend(self.diags_assoc_consts(db));
        out.extend(self.diags_assoc_const_evaluability(db));
        for pred in WhereClauseOwner::ImplTrait(self).clause(db).predicates(db) {
            out.extend(pred.application_diags(db));
        }
        out.extend(GenericParamOwner::ImplTrait(self).diags(db));
        out
    }
}
