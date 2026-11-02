use crate::{
    hir_def::{
        GenericArg, PathId, PathKind, TraitRefId, TypeId, TypeKind, TypeMode, scope_graph::ScopeId,
    },
    span::{params::LazyGenericArgSpan, path::LazyPathSpan, types::LazyTySpan},
    visitor::{Visitor, VisitorCtxt, prelude::DynLazySpan, walk_generic_arg, walk_path, walk_type},
};

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        ExpectedPathKind, PathRes, diagnostics::PathResDiag, resolve_path,
        resolve_path_with_minter, resolve_path_with_observer_and_minter,
        resolve_trait_path_with_observer_and_minter, trait_arg_path_diag,
    },
    ty::visitor::{TyVisitor, walk_ty},
};

use super::{
    const_ty::{ConstBodyLowering, ConstTyData, LoweringContext, ty_is_fully_ground},
    diagnostics::{ImplDiag, TraitConstraintDiag, TyDiagCollection, TyLowerDiag},
    normalize::normalize_ty,
    shape::{lower_shape_list, lower_shape_value, param_product_diags, shape_value_diags},
    trait_resolution::{PredicateListId, TraitSolveCx, WellFormedness, check_ty_wf},
    ty_def::{InvalidCause, Kind, ShapeMode, TyData, TyId},
    ty_lower::{lower_hir_ty, lower_hir_ty_in_mode, lower_kind_in_bounds},
};
use crate::core::semantic::mut_grant;
use crate::visitor::prelude::LazyTraitRefSpan;

/// The generic arguments written on `path`'s last segment, with their spans.
pub(crate) fn path_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    path_span: LazyPathSpan<'db>,
) -> impl Iterator<Item = (&'db GenericArg<'db>, LazyGenericArgSpan<'db>)> {
    let spans = path_span.segment(path.segment_index(db)).generic_args();
    path.generic_args(db)
        .data(db)
        .iter()
        .enumerate()
        .map(move |(idx, arg)| (arg, spans.clone().arg(idx)))
}

/// Diagnostics for the types a trait reference writes as generic arguments,
/// each at its own span. The reference's own checks cover the
/// well-formedness of the arguments it lowers.
pub(crate) fn trait_ref_arg_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    tr: TraitRefId<'db>,
    span: LazyTraitRefSpan<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    tr.path(db).to_opt().map_or_else(Vec::new, |path| {
        path_arg_diags(db, path, span.path(), scope, assumptions)
    })
}

/// Diagnostics for the types `path`'s last segment writes as generic
/// arguments, each at its own span: a path in one that names no type as
/// resolving the trait path reports it, and otherwise what lowering the
/// argument finds, where a path may name a constant as in a type's generic
/// arguments. An associated-type binding, which lowering never checks,
/// reports any error.
pub(crate) fn path_arg_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    path_span: LazyPathSpan<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    // A bound fixes each associated item, at each mode, once.
    let mut bound: Vec<(_, LazyGenericArgSpan<'db>)> = Vec::new();
    let mut duplicates = Vec::new();
    for (arg, span) in path_generic_args(db, path, path_span.clone()) {
        if let GenericArg::AssocType(binding) = arg
            && let Some(name) = binding.name.to_opt()
        {
            match bound.iter().find(|(key, _)| *key == (name, binding.mode)) {
                Some((_, first)) => duplicates.push(
                    TyLowerDiag::DuplicateAssocBinding {
                        span: span.into_assoc_type_arg().name().into(),
                        first: first.clone().into_assoc_type_arg().name().into(),
                        name,
                        mode: binding.mode,
                    }
                    .into(),
                ),
                None => bound.push(((name, binding.mode), span)),
            }
        }
    }
    path_generic_args(db, path, path_span.clone())
        .enumerate()
        .flat_map(|(idx, (arg, span))| {
            if let Some(diag) =
                trait_arg_path_diag(db, path, path_span.clone(), idx, scope, assumptions)
            {
                return vec![diag];
            }
            match arg {
                // A tuple whose parts carry modes or name a shape is a shape,
                // checked as a product parameter's type is.
                GenericArg::Type(ty_arg)
                    if let Some(ty) = ty_arg.ty.to_opt()
                        && lower_shape_list(db, ty, scope, assumptions).is_some() =>
                {
                    let span = span.into_type_arg().ty();
                    let mut diags = maybe_mut_parts(db, ty, span.clone());
                    diags.extend(param_product_diags(db, ty, scope, span, assumptions));
                    diags
                }
                GenericArg::Type(_) => {
                    let mut vis = HirTyErrVisitor {
                        db,
                        assumptions,
                        diags: Vec::new(),
                        const_bodies: ConstBodyLowering::Eager,
                    };
                    vis.visit_generic_arg(&mut VisitorCtxt::new(db, scope, span), arg);
                    vis.diags
                }
                GenericArg::AssocType(binding) => {
                    let Some(ty) = binding.ty.to_opt() else {
                        return Vec::new();
                    };
                    let span = span.into_assoc_type_arg();
                    let decl = binding.name.to_opt().and_then(|name| {
                        match resolve_path(db, path, scope, assumptions, false) {
                            Ok(PathRes::Trait(inst)) => inst.def(db).assoc_ty(db, name),
                            _ => None,
                        }
                    });
                    // An associated shape is bound to a shape at a mode, and
                    // nothing else is bound at one.
                    match (decl.is_some_and(|decl| decl.is_shape()), binding.mode) {
                        (true, Some(mode)) => {
                            // A bound names concrete modes, and at `ref` no
                            // access is `mut`.
                            let span = span.ty();
                            let diags = maybe_mut_parts(db, ty, span.clone());
                            if !diags.is_empty() {
                                return diags;
                            }
                            let diags = shape_value_diags(db, ty, scope, span.clone(), assumptions);
                            if !diags.is_empty() || mode == TypeMode::Mut {
                                return diags;
                            }
                            let value = lower_shape_value(
                                db,
                                ty,
                                scope,
                                assumptions,
                                &LoweringContext::new(),
                            )
                            .at_shape_mode(db, ShapeMode::Ref)
                            .as_shape_part(db);
                            binding
                                .name
                                .to_opt()
                                .zip(mut_grant(db, &value))
                                .map(|(name, param)| {
                                    ImplDiag::ShapeGrantsMut {
                                        primary: span.into(),
                                        name,
                                        param,
                                        bound: true,
                                    }
                                    .into()
                                })
                                .into_iter()
                                .collect()
                        }
                        (true, None) => {
                            vec![TyLowerDiag::ShapeWithoutMode(span.name().into()).into()]
                        }
                        (false, Some(_)) => {
                            vec![TyLowerDiag::ModeArgNotShape(span.generic_args().into()).into()]
                        }
                        (false, None) => {
                            let span = span.ty();
                            let diags =
                                collect_hir_ty_diags(db, scope, ty, span.clone(), assumptions);
                            // A type is of its declaration's kind: `*`, unless
                            // a bound gives another.
                            let expected = decl
                                .and_then(|decl| lower_kind_in_bounds(&decl.bounds))
                                .unwrap_or(Kind::Star);
                            let given = lower_hir_ty(db, ty, scope, assumptions);
                            if diags.is_empty()
                                && !given.has_invalid(db)
                                && !given.kind(db).does_match(&expected)
                            {
                                vec![
                                    TyLowerDiag::InvalidTypeArgKind {
                                        span: span.into(),
                                        expected: Some(expected),
                                        given,
                                    }
                                    .into(),
                                ]
                            } else {
                                diags
                            }
                        }
                    }
                }
                GenericArg::Const(_) => Vec::new(),
                GenericArg::Mode(_) => vec![TyLowerDiag::ModeArgNotShape(span.into()).into()],
            }
        })
        .chain(duplicates)
        .collect()
}

/// Diagnostics for the types written as generic arguments of `path`'s
/// segments, each at its own span, as a written type reports them. `path`
/// itself isn't resolved: its resolution reports its own segments.
pub(crate) fn path_generic_arg_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    path_span: LazyPathSpan<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let mut vis = HirTyErrVisitor {
        db,
        assumptions,
        diags: Vec::new(),
        const_bodies: ConstBodyLowering::Eager,
    };
    walk_path(&mut vis, &mut VisitorCtxt::new(db, scope, path_span), path);
    vis.diags
}

/// The parts of a shape argument written `mut?`, at any depth: a shape's
/// modes are concrete, and an argument names no mode `mut?` could take.
fn maybe_mut_parts<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TypeId<'db>,
    span: LazyTySpan<'db>,
) -> Vec<TyDiagCollection<'db>> {
    match ty.data(db) {
        TypeKind::Mode(TypeMode::MaybeMut, _) => {
            vec![TyLowerDiag::MaybeMutInShape(span.into()).into()]
        }
        TypeKind::Tuple(elems) => elems
            .data(db)
            .iter()
            .enumerate()
            .filter_map(|(idx, elem)| Some((elem.to_opt()?, idx)))
            .flat_map(|(elem, idx)| {
                maybe_mut_parts(db, elem, span.clone().into_tuple_type().elem_ty(idx))
            })
            .collect(),
        _ => Vec::new(),
    }
}

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
    collect_hir_ty_diags_in_mode(
        db,
        scope,
        hir_ty,
        span,
        assumptions,
        ConstBodyLowering::Eager,
    )
}

/// Collect structural diagnostics for a declaration template. Anonymous const
/// bodies are checked by the declaration's body-validation owner.
pub(crate) fn collect_hir_ty_diags_deferred<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    hir_ty: TypeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    collect_hir_ty_diags_in_mode(
        db,
        scope,
        hir_ty,
        span,
        assumptions,
        ConstBodyLowering::Deferred,
    )
}

fn collect_hir_ty_diags_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    hir_ty: TypeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
    const_bodies: ConstBodyLowering,
) -> Vec<TyDiagCollection<'db>> {
    // Try precise HIR-based errors first
    let diags =
        collect_ty_lower_errors_in_mode(db, scope, hir_ty, span.clone(), assumptions, const_bodies);
    if !diags.is_empty() {
        return diags;
    }

    // Fall back to semantic errors
    let ty = lower_hir_ty_in_mode(db, hir_ty, scope, assumptions, const_bodies);
    if let Some(diag) = emit_invalid_ty_error(db, ty, span.clone().into()) {
        return vec![diag];
    }

    normalization_limit_diag(db, ty, scope, assumptions, span.into())
        .into_iter()
        .collect()
}

/// Projections in a written type are resolved only when the type is
/// normalized. Report one that cannot be resolved within the normalization
/// limits where it is written: elsewhere it would be an invalid type, which
/// matches any type.
pub(crate) fn normalization_limit_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    span: DynLazySpan<'db>,
) -> Option<TyDiagCollection<'db>> {
    (ty.has_projection(db)
        && first_invalid_ty_cause(db, normalize_ty(db, ty, scope, assumptions))
            == Some(InvalidCause::TypeNormalizationLimit))
    .then(|| TyLowerDiag::TypeNormalizationLimit(span).into())
}

pub fn collect_ty_lower_errors<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    hir_ty: TypeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    collect_ty_lower_errors_in_mode(
        db,
        scope,
        hir_ty,
        span,
        assumptions,
        ConstBodyLowering::Eager,
    )
}

fn collect_ty_lower_errors_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    hir_ty: TypeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
    const_bodies: ConstBodyLowering,
) -> Vec<TyDiagCollection<'db>> {
    let mut vis = HirTyErrVisitor {
        db,
        assumptions,
        diags: Vec::new(),
        const_bodies,
    };
    let mut ctxt = VisitorCtxt::new(db, scope, span);
    vis.visit_ty(&mut ctxt, hir_ty);
    vis.diags
}

struct HirTyErrVisitor<'db> {
    db: &'db dyn HirAnalysisDb,
    diags: Vec<TyDiagCollection<'db>>,
    assumptions: PredicateListId<'db>,
    const_bodies: ConstBodyLowering,
}

impl<'db> HirTyErrVisitor<'db> {
    fn path_context(&self) -> LoweringContext<'db> {
        LoweringContext::for_const_bodies(self.const_bodies)
    }

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
            && let Ok(resolved) = resolve_path_with_minter(
                self.db,
                path,
                ctxt.scope(),
                self.assumptions,
                true,
                &self.path_context(),
            )
        {
            let is_const_like = match resolved {
                PathRes::Const(..) | PathRes::TraitConst(..) | PathRes::InherentConst(..) => true,
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

                    let resolved = resolve_path_with_observer_and_minter(
                        self.db,
                        path,
                        scope,
                        self.assumptions,
                        true,
                        &mut check_visibility,
                        &self.path_context(),
                    );
                    match resolved {
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
                                self.diags.push(diag);
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
        let ty = lower_hir_ty_in_mode(
            self.db,
            hir_ty,
            ctxt.scope(),
            self.assumptions,
            self.const_bodies,
        );

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

        let resolved = resolve_path_with_observer_and_minter(
            self.db,
            path,
            scope,
            self.assumptions,
            false,
            &mut check_visibility,
            &self.path_context(),
        );
        let res = match resolved {
            Ok(res) => res,

            Err(err) => {
                if let Some(diag) =
                    err.into_diag(self.db, path, path_span.clone(), ExpectedPathKind::Type)
                {
                    self.diags.push(diag);
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
        let resolved = resolve_trait_path_with_observer_and_minter(
            self.db,
            path,
            scope,
            self.assumptions,
            &mut check_visibility,
            &self.path_context(),
        );
        match resolved {
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
                    self.diags.push(diag);
                }
            }
        }

        // Do not recurse into the trait path via visit_path to avoid emitting
        // type-expected diagnostics for trait-qualified segments. Generic
        // arguments under the trait path are checked elsewhere during lowering.
    }
}

/// Well-formedness of the types written inside qualified paths in `hir_ty`:
/// the self type and the trait's generic arguments of `<T as Trait<..>>::Assoc`.
///
/// Resolving the projection replaces the qualified path with the associated
/// type's definition, so a later check of the resolved type never sees these
/// types. Each ill-formed one is reported at its own span.
pub(crate) fn qualified_path_wf_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    hir_ty: TypeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
    solve_cx: TraitSolveCx<'db>,
) -> Vec<TyDiagCollection<'db>> {
    struct QualifiedPathWf<'db> {
        db: &'db dyn HirAnalysisDb,
        /// Assumptions for lowering the written types.
        assumptions: PredicateListId<'db>,
        solve_cx: TraitSolveCx<'db>,
        /// How many qualified paths enclose the current type.
        qualified_depth: usize,
        diags: Vec<TyDiagCollection<'db>>,
    }

    impl<'db> Visitor<'db> for QualifiedPathWf<'db> {
        fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>, hir_ty: TypeId<'db>) {
            if self.qualified_depth > 0
                && let Some(span) = ctxt.span()
            {
                let ty = lower_hir_ty(self.db, hir_ty, ctxt.scope(), self.assumptions);
                if !ty.has_invalid(self.db)
                    && let WellFormedness::IllFormed { goal, subgoal } =
                        check_ty_wf(self.db, self.solve_cx, ty)
                {
                    self.diags.push(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span: span.into(),
                            primary_goal: goal,
                            unsat_subgoal: subgoal,
                            required_by: None,
                        }
                        .into(),
                    );
                    return;
                }
            }
            walk_type(self, ctxt, hir_ty);
        }

        fn visit_path(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>,
            path: PathId<'db>,
        ) {
            let qualified = std::iter::successors(Some(path), |path| path.parent(self.db))
                .any(|segment| matches!(segment.kind(self.db), PathKind::QualifiedType { .. }));
            if qualified {
                self.qualified_depth += 1;
            }
            walk_path(self, ctxt, path);
            if qualified {
                self.qualified_depth -= 1;
            }
        }
    }

    let mut visitor = QualifiedPathWf {
        db,
        assumptions,
        solve_cx,
        qualified_depth: 0,
        diags: Vec::new(),
    };
    let mut ctxt = VisitorCtxt::new(db, scope, span);
    visitor.visit_ty(&mut ctxt, hir_ty);
    visitor.diags
}

pub(crate) fn first_invalid_ty_cause<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<InvalidCause<'db>> {
    struct InvalidCauseVisitor<'db> {
        db: &'db dyn HirAnalysisDb,
        first: Option<InvalidCause<'db>>,
        reportable: Option<InvalidCause<'db>>,
    }
    impl<'db> TyVisitor<'db> for InvalidCauseVisitor<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_invalid(&mut self, cause: &InvalidCause<'db>) {
            self.first.get_or_insert_with(|| cause.clone());
            if !matches!(
                cause,
                InvalidCause::NotAType(_)
                    | InvalidCause::PathResolutionFailed { .. }
                    | InvalidCause::ParseError
                    | InvalidCause::Other
            ) {
                self.reportable.get_or_insert_with(|| cause.clone());
            }
        }
    }

    if !ty.has_invalid(db) {
        return None;
    }

    let mut visitor = InvalidCauseVisitor {
        db,
        first: None,
        reportable: None,
    };
    visitor.visit_ty(ty);
    visitor.reportable.or(visitor.first)
}

/// The first failure among deferred const terms whose captures are fully
/// ground. A concrete target demands them so a failing specialization is
/// reported where it is written rather than at a later layout demand.
pub(crate) fn demanded_ground_const_cause<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<InvalidCause<'db>> {
    struct GroundConstDemand<'db> {
        db: &'db dyn HirAnalysisDb,
        cause: Option<InvalidCause<'db>>,
    }
    impl<'db> TyVisitor<'db> for GroundConstDemand<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.cause.is_some() {
                return;
            }
            if let TyData::ConstTy(const_ty) = ty.data(self.db)
                && matches!(
                    const_ty.data(self.db),
                    ConstTyData::UnEvaluated { .. } | ConstTyData::Computation { .. }
                )
                && ty_is_fully_ground(self.db, ty)
            {
                self.cause =
                    first_invalid_ty_cause(self.db, const_ty.evaluate(self.db, None).ty(self.db));
                return;
            }
            walk_ty(self, ty);
        }
    }

    let mut visitor = GroundConstDemand { db, cause: None };
    visitor.visit_ty(ty);
    visitor.cause
}

pub fn emit_invalid_ty_error<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    span: DynLazySpan<'db>,
) -> Option<TyDiagCollection<'db>> {
    first_invalid_ty_cause(db, ty).and_then(|cause| diag_from_invalid_cause(span, &cause))
}

pub(crate) fn diag_from_invalid_cause<'db>(
    span: DynLazySpan<'db>,
    cause: &InvalidCause<'db>,
) -> Option<TyDiagCollection<'db>> {
    Some(match cause.clone() {
        InvalidCause::NotFullyApplied => TyLowerDiag::ExpectedStarKind(span).into(),

        InvalidCause::ModeNotType => TyLowerDiag::ModeNotType(span).into(),

        InvalidCause::ModeArgNotShape => TyLowerDiag::ModeArgNotShape(span).into(),

        InvalidCause::ViewPart { view } => TyLowerDiag::ViewTypeMode {
            span,
            ty: view,
            mode: "part of another type",
        }
        .into(),

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

        InvalidCause::StringTooLarge { max, given } => {
            TyLowerDiag::StringTooLarge { span, max, given }.into()
        }

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

        InvalidCause::ConstEvalAssertionFailed {
            body,
            expr,
            message,
        } => TyLowerDiag::ConstEvalAssertionFailed {
            span: expr.span(body).into(),
            message,
        }
        .into(),

        InvalidCause::ConstEvalPanicked { body, expr, code } => TyLowerDiag::ConstEvalPanicked {
            span: expr.span(body).into(),
            code,
        }
        .into(),

        InvalidCause::ConstEvalNonConstCall { body, expr } => {
            TyLowerDiag::ConstEvalNonConstCall(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalDivisionByZero { body, expr } => {
            TyLowerDiag::ConstEvalDivisionByZero(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalOutOfBounds { body, expr } => {
            TyLowerDiag::ConstEvalOutOfBounds(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalInvalidOperation {
            body,
            expr,
            message,
        } => TyLowerDiag::ConstEvalInvalidOperation {
            span: expr.span(body).into(),
            message,
        }
        .into(),

        InvalidCause::ConstEvalInvalidBorrow { body, expr } => {
            TyLowerDiag::ConstEvalInvalidBorrow(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalInvalidProviderUse { body, expr } => {
            TyLowerDiag::ConstEvalInvalidProviderUse(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalVariantMismatch { body, expr } => {
            TyLowerDiag::ConstEvalVariantMismatch(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalUninitializedLocal { body, expr } => {
            TyLowerDiag::ConstEvalUninitializedLocal(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalInvariant {
            body,
            expr,
            message,
        } => TyLowerDiag::ConstEvalInvariant {
            span: expr.span(body).into(),
            message,
        }
        .into(),

        InvalidCause::ConstEvalArithmeticOverflow { body, expr } => {
            TyLowerDiag::ConstEvalArithmeticOverflow(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalNegativeExponent { body, expr } => {
            TyLowerDiag::ConstEvalNegativeExponent(expr.span(body).into()).into()
        }

        InvalidCause::IntLiteralOutOfRange {
            body,
            expr,
            literal,
            ty,
        } => TyLowerDiag::IntLiteralOutOfRange {
            span: expr.span(body).into(),
            literal,
            ty,
        }
        .into(),

        InvalidCause::ConstEvalStepLimitExceeded { body, expr } => {
            TyLowerDiag::ConstEvalStepLimitExceeded(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalRecursionLimitExceeded { body, expr } => {
            TyLowerDiag::ConstEvalRecursionLimitExceeded(expr.span(body).into()).into()
        }

        InvalidCause::ConstEvalRecursiveConst { body, expr } => {
            TyLowerDiag::ConstEvalRecursiveConst(expr.span(body).into()).into()
        }

        InvalidCause::TypeLoweringCycle => TyLowerDiag::TypeLoweringCycle(span).into(),

        InvalidCause::TypeNormalizationLimit => TyLowerDiag::TypeNormalizationLimit(span).into(),

        InvalidCause::NotAType(_) => return None,

        // These errors should be caught and reported elsewhere
        InvalidCause::PathResolutionFailed { .. }
        | InvalidCause::ParseError
        | InvalidCause::Other => return None,
    })
}
