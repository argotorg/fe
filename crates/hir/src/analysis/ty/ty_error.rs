use crate::{
    hir_def::{GenericArg, PathId, TypeId, TypeKind, scope_graph::ScopeId},
    span::{params::LazyGenericArgSpan, path::LazyPathSpan, types::LazyTySpan},
    visitor::{Visitor, VisitorCtxt, prelude::DynLazySpan, walk_generic_arg, walk_path, walk_type},
};

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        ExpectedPathKind, PathRes, diagnostics::PathResDiag, resolve_path,
        resolve_path_with_observer,
    },
    ty::visitor::TyVisitor,
};

use super::{
    diagnostics::{TyDiagCollection, TyLowerDiag},
    trait_resolution::PredicateListId,
    ty_def::{InvalidCause, TyData, TyId},
    ty_lower::lower_hir_ty,
};
use crate::visitor::prelude::LazyTraitRefSpan;

/// Collect all type-lowering diagnostics for a HIR type.
///
/// This encapsulates the two-phase error collection strategy:
/// 1. First tries to get precise HIR-based errors (path resolution, visibility)
/// 2. Falls back to semantic `InvalidCause` errors if none found
pub fn collect_hir_ty_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    hir_ty: TypeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    // Try precise HIR-based errors first
    let diags = collect_ty_lower_errors(db, scope, hir_ty, span.clone(), assumptions);
    if !diags.is_empty() {
        return diags;
    }

    // Fall back to semantic errors
    let ty = lower_hir_ty(db, hir_ty, scope, assumptions);
    emit_invalid_ty_error(db, ty, span.into())
        .into_iter()
        .collect()
}

pub fn collect_ty_lower_errors<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    hir_ty: TypeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let mut vis = HirTyErrVisitor {
        db,
        assumptions,
        diags: Vec::new(),
    };
    let mut ctxt = VisitorCtxt::new(db, scope, span);
    vis.visit_ty(&mut ctxt, hir_ty);
    vis.diags
}

struct HirTyErrVisitor<'db> {
    db: &'db dyn HirAnalysisDb,
    diags: Vec<TyDiagCollection<'db>>,
    assumptions: PredicateListId<'db>,
}

impl<'db> HirTyErrVisitor<'db> {
    fn push_opt_diag(&mut self, diag: Option<TyDiagCollection<'db>>) {
        if let Some(diag) = diag {
            self.diags.push(diag)
        }
    }
}

impl<'db> Visitor<'db> for HirTyErrVisitor<'db> {
    fn visit_generic_arg(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericArgSpan<'db>>,
        arg: &GenericArg<'db>,
    ) {
        // Generic args are syntactically ambiguous: `String<N>` may parse `N` as a type
        // even when `String` expects a const generic arg. Avoid emitting spurious
        // type-expected diagnostics for const-like paths in generic-arg position.
        if let GenericArg::Type(type_arg) = arg
            && let Some(hir_ty) = type_arg.ty.to_opt()
            && let TypeKind::Path(path_partial) = hir_ty.data(self.db)
            && let Some(path) = path_partial.to_opt()
            && let Ok(resolved) = resolve_path(self.db, path, ctxt.scope(), self.assumptions, true)
        {
            let is_const_like = match resolved {
                PathRes::Const(..) | PathRes::TraitConst(..) => true,
                PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                    matches!(ty.data(self.db), TyData::ConstTy(_))
                }
                PathRes::EnumVariant(v) => v.ty.is_unit_variant_only_enum(self.db),
                _ => false,
            };

            if is_const_like {
                if let Some(span) = ctxt.span() {
                    let path_span = span.into_type_arg().ty().into_path_type().path();

                    // Preserve path visibility diagnostics even though we suppress
                    // "expected type" errors for const-like paths in generic-arg
                    // position.
                    let scope = ctxt.scope();
                    let mut invisible = None;
                    let mut check_visibility = |path: PathId<'db>, reso: &PathRes<'db>| {
                        if invisible.is_some() {
                            return;
                        }
                        if !reso.is_visible_from(self.db, scope) {
                            invisible = Some((path, reso.name_span(self.db)));
                        }
                    };

                    match resolve_path_with_observer(
                        self.db,
                        path,
                        scope,
                        self.assumptions,
                        true,
                        &mut check_visibility,
                    ) {
                        Ok(_) => {
                            if let Some((path, deriv_span)) = invisible
                                && let Some(ident) = path.ident(self.db).to_opt()
                            {
                                let span = path_span
                                    .clone()
                                    .segment(path.segment_index(self.db))
                                    .ident();
                                let diag = PathResDiag::Invisible(span.into(), ident, deriv_span);
                                self.diags.push(diag.into());
                            }
                        }
                        Err(err) => {
                            if let Some(diag) = err.into_diag(
                                self.db,
                                path,
                                path_span.clone(),
                                ExpectedPathKind::Value,
                            ) {
                                self.diags.push(diag.into());
                            }
                        }
                    }

                    // Walk the underlying path to validate any nested generic arguments,
                    // but don't validate the path itself as a type.
                    let mut path_ctxt = VisitorCtxt::new(ctxt.db(), ctxt.scope(), path_span);
                    walk_path(self, &mut path_ctxt, path);
                }
                return;
            }
        }

        walk_generic_arg(self, ctxt, arg);
    }

    fn visit_body(
        &mut self,
        _ctxt: &mut VisitorCtxt<'db, crate::core::span::item::LazyBodySpan<'db>>,
        _body: crate::core::hir_def::Body<'db>,
    ) {
        // Skip traversing bodies when collecting type-lowering errors from type
        // positions. This avoids emitting name-resolution diagnostics for const
        // expressions that appear inside types (e.g., array lengths or const
        // generic arguments), which are handled by dedicated const-ty lowering.
    }

    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>, hir_ty: TypeId<'db>) {
        let ty = lower_hir_ty(self.db, hir_ty, ctxt.scope(), self.assumptions);

        // This will report errors with nested types that are fundamental to the nested type,
        // but will not catch cases where the nested type is fine on its own, but incompatible
        // with the current type we're visiting. If !did_find_child_err, we use a TyVisitor to
        // report a diag about a nested invalid type; the downside of this is that the diag's
        // span will be too wide (it'll be the span of the current type, not the nested type).
        let before = self.diags.len();
        // If the semantic type is a const type, don't traverse the underlying HIR type
        // structure as a normal type. Const expressions inside types are validated via
        // const-ty lowering/evaluation, and walking their HIR representation as a type
        // produces spurious "expected type" diagnostics for paths like `SALT`.
        if !ty.is_const_ty(self.db) {
            walk_type(self, ctxt, hir_ty);
        }
        let did_fild_child_err = self.diags.len() > before;

        let span = ctxt.span().unwrap().into();
        match ty.data(self.db) {
            TyData::TyApp(base, _) if matches!(base.data(self.db), TyData::Invalid(..)) => {
                let TyData::Invalid(cause) = base.data(self.db) else {
                    unreachable!()
                };
                self.push_opt_diag(diag_from_invalid_cause(span, cause));
            }
            TyData::Invalid(cause) => self.push_opt_diag(diag_from_invalid_cause(span, cause)),
            _ => {
                // The span of a diag found here will cover the current type, not the nested
                // type. For example:
                //   Foo<true>
                //   ^^^^^^^^^ expected `u32`, but `bool` is given
                // (ideally this would only underline `true`).
                //
                // We could match other TyId structures manually to refine the spans
                // for common error cases.
                if !did_fild_child_err && ty.has_invalid(self.db) {
                    self.push_opt_diag(emit_invalid_ty_error(self.db, ty, span))
                }
            }
        }
    }

    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        let scope = ctxt.scope();
        let path_span = ctxt.span().unwrap();

        let mut invisible = None;
        let mut check_visibility = |path: PathId<'db>, reso: &PathRes<'db>| {
            if invisible.is_some() {
                return;
            }
            if !reso.is_visible_from(self.db, scope) {
                invisible = Some((path, reso.name_span(self.db)));
            }
        };

        let res = match resolve_path_with_observer(
            self.db,
            path,
            scope,
            self.assumptions,
            false,
            &mut check_visibility,
        ) {
            Ok(res) => res,

            Err(err) => {
                if let Some(diag) =
                    err.into_diag(self.db, path, path_span.clone(), ExpectedPathKind::Type)
                {
                    self.diags.push(diag.into());
                }
                return;
            }
        };

        if !matches!(res, PathRes::Ty(_) | PathRes::TyAlias(..)) {
            let ident = path.ident(self.db).unwrap();
            let span = path_span.clone().segment(path.segment_index(self.db));
            self.diags
                .push(PathResDiag::ExpectedType(span.into(), ident, res.kind_name()).into());
        }
        if let Some((path, deriv_span)) = invisible {
            let span = path_span.segment(path.segment_index(self.db)).ident();
            let ident = path.ident(self.db);
            let diag = PathResDiag::Invisible(span.into(), ident.unwrap(), deriv_span);
            self.diags.push(diag.into());
        }

        walk_path(self, ctxt, path);
    }

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
        trait_ref: crate::core::hir_def::TraitRefId<'db>,
    ) {
        let scope = ctxt.scope();
        let span = ctxt.span().unwrap();
        let Some(path) = trait_ref.path(self.db).to_opt() else {
            return;
        };

        // Visibility check for trait paths mirrors visit_path but expects a Trait.
        let mut invisible = None;
        let mut check_visibility =
            |p: PathId<'db>, reso: &crate::analysis::name_resolution::PathRes<'db>| {
                if invisible.is_some() {
                    return;
                }
                if !reso.is_visible_from(self.db, scope) {
                    invisible = Some((p, reso.name_span(self.db)));
                }
            };

        // TODO(diags): In the future, refine this to walk only generic args
        // under the trait ref and surface kind/arg mismatches with precise
        // spans when available from semantic lowering.
        match crate::analysis::name_resolution::resolve_path_with_observer(
            self.db,
            path,
            scope,
            self.assumptions,
            false,
            &mut check_visibility,
        ) {
            Ok(res) => {
                if !matches!(res, crate::analysis::name_resolution::PathRes::Trait(_)) {
                    // Expected a trait in this context
                    let ident = path.ident(self.db).unwrap();
                    let seg_span = span.clone().name();
                    self.diags.push(
                        crate::analysis::name_resolution::diagnostics::PathResDiag::ExpectedTrait(
                            seg_span.into(),
                            ident,
                            res.kind_name(),
                        )
                        .into(),
                    );
                }

                if let Some((_p, deriv_span)) = invisible {
                    let seg_span = span.name();
                    let ident = path.ident(self.db).unwrap();
                    let diag =
                        crate::analysis::name_resolution::diagnostics::PathResDiag::Invisible(
                            seg_span.into(),
                            ident,
                            deriv_span,
                        );
                    self.diags.push(diag.into());
                }
            }
            Err(err) => {
                if let Some(diag) = err.into_diag(
                    self.db,
                    path,
                    span.clone().path(),
                    crate::analysis::name_resolution::ExpectedPathKind::Trait,
                ) {
                    self.diags.push(diag.into());
                }
            }
        }

        // Do not recurse into the trait path via visit_path to avoid emitting
        // type-expected diagnostics for trait-qualified segments. Generic
        // arguments under the trait path are checked elsewhere during lowering.
    }
}

pub fn emit_invalid_ty_error<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    span: DynLazySpan<'db>,
) -> Option<TyDiagCollection<'db>> {
    struct EmitDiagVisitor<'db> {
        db: &'db dyn HirAnalysisDb,
        diag: Option<TyDiagCollection<'db>>,
        span: DynLazySpan<'db>,
    }
    impl<'db> TyVisitor<'db> for EmitDiagVisitor<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_invalid(&mut self, cause: &InvalidCause<'db>) {
            if let Some(diag) = diag_from_invalid_cause(self.span.clone(), cause) {
                self.diag.get_or_insert(diag);
            }
        }
    }

    if !ty.has_invalid(db) {
        return None;
    }

    let mut visitor = EmitDiagVisitor {
        db,
        diag: None,
        span,
    };

    visitor.visit_ty(ty);
    visitor.diag
}

fn diag_from_invalid_cause<'db>(
    span: DynLazySpan<'db>,
    cause: &InvalidCause<'db>,
) -> Option<TyDiagCollection<'db>> {
    Some(match cause.clone() {
        InvalidCause::NotFullyApplied => TyLowerDiag::ExpectedStarKind(span).into(),

        InvalidCause::KindMismatch { expected, given } => TyLowerDiag::InvalidTypeArgKind {
            span,
            expected,
            given,
        }
        .into(),

        InvalidCause::TooManyGenericArgs { expected, given } => TyLowerDiag::TooManyGenericArgs {
            span,
            expected,
            given,
        }
        .into(),

        InvalidCause::InvalidConstParamTy => TyLowerDiag::InvalidConstParamTy(span).into(),

        InvalidCause::RecursiveConstParamTy => TyLowerDiag::RecursiveConstParamTy(span).into(),

        InvalidCause::ConstTyMismatch { expected, given } => TyLowerDiag::ConstTyMismatch {
            span,
            expected,
            given,
        }
        .into(),

        InvalidCause::ConstTyExpected { expected } => {
            TyLowerDiag::ConstTyExpected { span, expected }.into()
        }

        InvalidCause::NormalTypeExpected { given } => {
            TyLowerDiag::NormalTypeExpected { span, given }.into()
        }

        InvalidCause::UnboundTypeAliasParam {
            alias,
            n_given_args,
        } => TyLowerDiag::UnboundTypeAliasParam {
            span,
            alias,
            n_given_args,
        }
        .into(),

        InvalidCause::AliasCycle(cycle) => TyLowerDiag::TypeAliasCycle {
            cycle: cycle.to_vec(),
        }
        .into(),

        InvalidCause::InvalidConstTyExpr { body } => {
            TyLowerDiag::InvalidConstTyExpr(body.span().into()).into()
        }

        InvalidCause::ConstEvalUnsupported { body, expr } => {
            TyLowerDiag::ConstEvalUnsupported(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalNonConstCall { body, expr } => {
            TyLowerDiag::ConstEvalNonConstCall(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalDivisionByZero { body, expr } => {
            TyLowerDiag::ConstEvalDivisionByZero(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalStepLimitExceeded { body, expr } => {
            TyLowerDiag::ConstEvalStepLimitExceeded(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalRecursionLimitExceeded { body, expr } => {
            TyLowerDiag::ConstEvalRecursionLimitExceeded(expr.span(body).into()).into()
        }

        InvalidCause::NotAType(_) => return None,

        // These errors should be caught and reported elsewhere
        InvalidCause::PathResolutionFailed { .. }
        | InvalidCause::ParseError
        | InvalidCause::Other => return None,
    })
}
