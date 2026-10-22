use crate::core::hir_def::{
    Body, CallableDef, Expr, GenericArg, GenericArgListId, GenericParam, GenericParamOwner,
    GenericParamView, IdentId, KindBound as HirKindBound, Partial, PathId, Stmt,
    TypeAlias as HirTypeAlias, TypeBound, TypeId as HirTyId, TypeKind as HirTyKind, TypeMode,
    scope_graph::ScopeId,
};
use salsa::SalsaValue;
use smallvec::smallvec;

use super::{
    assoc_const::{AssocConstUse, InherentConstUse},
    const_ty::{
        ConstBodyLowering, ConstCaptureEnv, ConstTyData, ConstTyId, LoweringContext,
        UnevaluatedConstPolicy, const_ty_from_sem_const,
    },
    generic_defaults::DefaultApplication,
    normalize::normalize_ty,
    provider::AddressSpace,
    shape::lower_shape_list,
    trait_def::TraitInstId,
    trait_resolution::{
        PredicateListId,
        constraint::{
            collect_candidate_constraints, collect_constraints, collect_func_decl_constraints,
        },
    },
    ty_def::{InvalidCause, Kind, TyData, TyId, TyParam},
};
use crate::analysis::name_resolution::{
    NameDomain, NameResKind, PathRes, TypePosition, path_resolver::PathResolutionResult,
    resolve_ident_to_bucket, resolve_path_with_minter, resolve_type_position_path_with_minter,
};
use crate::analysis::{
    HirAnalysisDb,
    semantic::{VariantIndex, enum_const},
    ty::binder::Binder,
};

/// Lowers the given HirTy to `TyId`.
#[salsa::tracked(returns(copy), cycle_initial=lower_hir_ty_cycle_initial)]
pub fn lower_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let lowering_cx = LoweringContext::new();
    lower_hir_ty_impl(db, ty, scope, assumptions, &lowering_cx)
}

fn lower_hir_ty_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> TyId<'db> {
    let lower_child =
        |child_ty, _slot| lower_opt_hir_ty_impl(db, child_ty, scope, assumptions, lowering_cx);

    let lowered = match ty.data(db) {
        HirTyKind::Ptr(pointee) => TyId::ptr_to(db, lower_child(*pointee, 0)),

        HirTyKind::Mode(TypeMode::Own, inner) => lower_child(*inner, 0),
        HirTyKind::Mode(TypeMode::Mut | TypeMode::MaybeMut | TypeMode::Ref, _) => {
            TyId::invalid(db, InvalidCause::ModeNotType)
        }

        HirTyKind::Path(path) => lower_path_impl(
            db,
            scope,
            *path,
            assumptions,
            TypePosition::Type,
            lowering_cx,
        ),

        HirTyKind::Tuple(tuple_id) => {
            let elems = tuple_id.data(db);
            let len = elems.len();
            let tuple = TyId::tuple(db, len);
            elems.iter().enumerate().fold(tuple, |acc, (idx, &elem)| {
                let elem_ty = lower_child(elem, idx);
                if !elem_ty.is_star_kind(db) {
                    return TyId::invalid(db, InvalidCause::NotFullyApplied);
                }

                TyId::app(db, acc, elem_ty)
            })
        }

        HirTyKind::Array(hir_elem_ty, len) => {
            let elem_ty = lower_child(*hir_elem_ty, 0);
            let len_ty = lower_opt_const_body(db, *len, scope, assumptions, lowering_cx);
            let len_ty = TyId::const_ty(db, len_ty);
            let array = TyId::array(db, elem_ty);
            TyId::app(db, array, len_ty)
        }

        HirTyKind::Primitive(primitive) => {
            TyId::new(db, TyData::TyBase(super::ty_def::TyBase::from(*primitive)))
        }
        HirTyKind::Never => TyId::never(db),
    };
    // A `#[view]` type exists only behind accesses, never inside a value.
    match lowered.view_part(db) {
        Some(view) if !lowered.has_invalid(db) => {
            TyId::invalid(db, InvalidCause::ViewPart { view })
        }
        _ => lowered,
    }
}

/// Lowers `ty` as [`lower_hir_ty`] or [`lower_hir_ty_deferred`] does, as
/// `const_bodies` says, and returns every path segment the lowering resolved,
/// with its resolution: the segments of the paths written in `ty`, including
/// generic arguments and qualified types, at every prefix. Aliases and
/// associated types are lowered by their own queries, so the paths written in
/// their declarations are not included.
pub(crate) fn lower_hir_ty_with_resolutions<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    const_bodies: ConstBodyLowering,
) -> (TyId<'db>, Vec<(PathId<'db>, PathRes<'db>)>) {
    let lowering_cx = LoweringContext::for_const_bodies(const_bodies).recording_resolutions();
    let lowered = lower_hir_ty_impl(db, ty, scope, assumptions, &lowering_cx);
    (lowered, lowering_cx.into_resolutions())
}

/// Lowers `ty` with the caller's lowering context, which sets how const
/// bodies lower and can record the paths the lowering resolves.
pub(crate) fn lower_hir_ty_with_minter<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> TyId<'db> {
    lower_hir_ty_impl(db, ty, scope, assumptions, lowering_cx)
}

/// Lowers an item-signature type without validating or evaluating anonymous
/// const bodies. The resulting const nodes retain their HIR bodies and are
/// checked when a concrete candidate is normalized or the item is diagnosed.
pub(crate) fn lower_hir_ty_deferred<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let lowering_cx = LoweringContext::deferred();
    lower_hir_ty_impl(db, ty, scope, assumptions, &lowering_cx)
}

pub(crate) fn lower_hir_ty_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    const_bodies: ConstBodyLowering,
) -> TyId<'db> {
    match const_bodies {
        ConstBodyLowering::Eager => lower_hir_ty(db, ty, scope, assumptions),
        ConstBodyLowering::Deferred => lower_hir_ty_deferred(db, ty, scope, assumptions),
    }
}

pub fn lower_opt_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Partial<HirTyId<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let Some(hir_ty) = ty.to_opt() else {
        return TyId::invalid(db, InvalidCause::ParseError);
    };
    let lowering_cx = LoweringContext::new();
    lower_hir_ty_impl(db, hir_ty, scope, assumptions, &lowering_cx)
}

fn lower_opt_hir_ty_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Partial<HirTyId<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> TyId<'db> {
    ty.to_opt().map_or_else(
        || TyId::invalid(db, InvalidCause::ParseError),
        |hir_ty| lower_hir_ty_impl(db, hir_ty, scope, assumptions, lowering_cx),
    )
}

fn const_body_simple_path<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> Option<PathId<'db>> {
    fn expr_simple_path<'db>(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: &Expr<'db>,
    ) -> Option<PathId<'db>> {
        match expr {
            Expr::Path(path) => path.to_opt(),
            Expr::Block(stmts, _) => {
                let [stmt] = stmts.as_slice() else {
                    return None;
                };
                let Partial::Present(Stmt::Expr(expr)) = stmt.data(db, body) else {
                    return None;
                };
                let Partial::Present(expr) = expr.data(db, body) else {
                    return None;
                };
                expr_simple_path(db, body, expr)
            }
            _ => None,
        }
    }

    let expr = body.expr(db).data(db, body).clone().to_opt()?;
    expr_simple_path(db, body, &expr)
}

/// Extends `assumptions` with the enclosing trait's implicit `Self: Trait`
/// predicate, mirroring the body-checking environment. Signature-position
/// paths and const bodies like `Slot<Self::N>` and `Slot<{ Self::N }>` must
/// resolve the trait's constants using the same implicit bound.
fn with_enclosing_trait_self_predicate<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> PredicateListId<'db> {
    let mut item = Some(scope.item());
    while let Some(current) = item {
        match current {
            crate::hir_def::ItemKind::Trait(trait_) => {
                let pred = crate::core::semantic::trait_self_predicate(db, trait_);
                if assumptions.list(db).contains(&pred) {
                    return assumptions;
                }
                let mut merged = assumptions.list(db).clone();
                merged.push(pred);
                return PredicateListId::new(db, merged);
            }
            // `Self` inside impls resolves to the implementor type directly.
            crate::hir_def::ItemKind::Impl(_) | crate::hir_def::ItemKind::ImplTrait(_) => {
                return assumptions;
            }
            _ => {}
        }
        item = current.scope().parent_item(db);
    }
    assumptions
}

pub(crate) fn lower_opt_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Partial<Body<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> ConstTyId<'db> {
    let Some(body) = body.to_opt() else {
        return ConstTyId::invalid(db, InvalidCause::ParseError);
    };
    if lowering_cx.const_bodies() == ConstBodyLowering::Deferred {
        if let Some(path) = const_body_simple_path(db, body)
            && path.parent(db).is_none()
            && matches!(
                resolve_ident_to_bucket(db, path, scope)
                    .pick(NameDomain::TYPE)
                    .as_ref()
                    .map(|name_res| name_res.kind),
                Ok(NameResKind::Scope(ScopeId::GenericParam(..)))
            )
            && let Ok(PathRes::Ty(ty)) =
                resolve_path_with_minter(db, path, scope, assumptions, true, lowering_cx)
            && let TyData::ConstTy(const_ty) = ty.data(db)
        {
            return *const_ty;
        }
        return ConstTyId::unevaluated(
            db,
            Partial::Present(body),
            None,
            None,
            ConstCaptureEnv::identity_for_body(db, body, Some(lowering_cx)),
            UnevaluatedConstPolicy::DeferValidation,
        );
    }
    lower_const_body_path(db, body, scope, assumptions, lowering_cx)
        .unwrap_or_else(|| ConstTyId::from_body(db, body, None, None))
}

/// Whether lowering reads an anonymous constant as the constant, trait
/// constant, const type or unit variant its lone path names, rather than
/// checking it as a body (see `lower_const_body_path`).
pub(crate) fn const_body_names_a_constant<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> bool {
    const_body_simple_path(db, body).is_some()
        && lower_const_body_path(db, body, scope, assumptions, &LoweringContext::new()).is_some()
}

/// How an anonymous constant that is a lone path lowers: to the constant,
/// trait constant, const type or unit variant it names. `None` when the body
/// is not such a path, and lowering checks it as a body of its own.
pub(crate) fn lower_const_body_path<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> Option<ConstTyId<'db>> {
    let path = const_body_simple_path(db, body)?;
    let assumptions = with_enclosing_trait_self_predicate(db, scope, assumptions);
    Some(
        match lowering_cx.without_recording(|| {
            resolve_path_with_minter(db, path, scope, assumptions, true, lowering_cx)
        }) {
            Ok(PathRes::Const(const_def, ty)) => {
                if let Some(body) = const_def.body(db).to_opt() {
                    ConstTyId::from_body(db, body, Some(ty), Some(const_def))
                } else {
                    ConstTyId::invalid(db, InvalidCause::ParseError)
                }
            }
            Ok(PathRes::TraitConst(recv_ty, inst, name)) => {
                let mut args = inst.args(db).clone();
                if let Some(self_arg) = args.first_mut() {
                    *self_arg = recv_ty;
                }
                let inst = TraitInstId::new(
                    db,
                    inst.def(db),
                    args,
                    inst.assoc_type_bindings(db).clone(),
                    inst.mode(db),
                );

                if let Some(expected_ty) = inst
                    .def(db)
                    .const_(db, name)
                    .and_then(|v| v.ty_binder(db))
                    .map(|b| b.instantiate(db, inst.args(db)))
                {
                    // Defer evaluation: the use position's expected type may
                    // differ in integer shape from the const's declared type
                    // (e.g. a `u256` trait const used as an array length).
                    let assoc = AssocConstUse::new(scope, assumptions, inst, name);
                    super::const_ty::abstract_const_ty_from_assoc_const_use(db, assoc, expected_ty)
                } else {
                    ConstTyId::invalid(db, InvalidCause::Other)
                }
            }
            Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => match ty.data(db) {
                TyData::ConstTy(const_ty) => *const_ty,
                _ => return None,
            },
            Ok(PathRes::EnumVariant(variant)) if variant.ty.is_unit_variant_only_enum(db) => {
                const_ty_from_sem_const(
                    db,
                    enum_const(
                        db,
                        variant.ty,
                        VariantIndex(variant.variant.idx),
                        Box::new([]),
                    ),
                )
            }
            _ => return None,
        },
    )
}

fn lower_path_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: Partial<PathId<'db>>,
    assumptions: PredicateListId<'db>,
    position: TypePosition,
    lowering_cx: &LoweringContext<'db>,
) -> TyId<'db> {
    let Some(path) = path.to_opt() else {
        return TyId::invalid(db, InvalidCause::ParseError);
    };
    lower_type_position_path(db, path, scope, assumptions, position, lowering_cx)
        .unwrap_or_else(|_| TyId::invalid(db, InvalidCause::PathResolutionFailed { path }))
}

/// Lowers `path`, written in a type `position`, from its single resolution
/// ([`resolve_type_position_path_with_minter`]): to the type or constant it
/// names, or to `NotAType` for anything else. The error is why `path` does
/// not resolve.
pub(crate) fn lower_type_position_path<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    position: TypePosition,
    lowering_cx: &LoweringContext<'db>,
) -> PathResolutionResult<'db, TyId<'db>> {
    let assumptions = with_enclosing_trait_self_predicate(db, scope, assumptions);
    let res = resolve_type_position_path_with_minter(
        db,
        path,
        scope,
        assumptions,
        position,
        lowering_cx,
    )?;
    Ok(match res {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) | PathRes::Func(ty) => ty,
        PathRes::Const(const_def, ty) => {
            if let Some(body) = const_def.body(db).to_opt() {
                let const_ty = ConstTyId::from_body(db, body, Some(ty), Some(const_def));
                TyId::const_ty(db, const_ty)
            } else {
                TyId::invalid(db, InvalidCause::ParseError)
            }
        }
        PathRes::TraitConst(recv_ty, inst, name) => {
            let mut args = inst.args(db).clone();
            if let Some(self_arg) = args.first_mut() {
                *self_arg = recv_ty;
            }
            let inst = TraitInstId::new(
                db,
                inst.def(db),
                args,
                inst.assoc_type_bindings(db).clone(),
                inst.mode(db),
            );

            if let Some(expected_ty) = inst
                .def(db)
                .const_(db, name)
                .and_then(|v| v.ty_binder(db))
                .map(|b| b.instantiate(db, inst.args(db)))
            {
                let assoc = AssocConstUse::new(scope, assumptions, inst, name);
                if let Some(const_ty) = super::const_ty::const_ty_or_abstract_from_assoc_const_use(
                    db,
                    assoc,
                    expected_ty,
                ) {
                    TyId::const_ty(db, const_ty)
                } else {
                    TyId::invalid(db, InvalidCause::Other)
                }
            } else {
                TyId::invalid(db, InvalidCause::Other)
            }
        }
        PathRes::InherentConst(recv_ty, impl_, name) => {
            if let Some(expected_ty) =
                super::const_ty::inherent_const_expected_ty(db, impl_, recv_ty, name)
            {
                let use_ = InherentConstUse::new(scope, assumptions, impl_, recv_ty, name);
                if let Some(const_ty) =
                    super::const_ty::const_ty_or_abstract_from_inherent_const_use(
                        db,
                        use_,
                        expected_ty,
                    )
                {
                    TyId::const_ty(db, const_ty)
                } else {
                    TyId::invalid(db, InvalidCause::Other)
                }
            } else {
                TyId::invalid(db, InvalidCause::Other)
            }
        }
        PathRes::EnumVariant(variant)
            if position == TypePosition::GenericArg && variant.ty.is_unit_variant_only_enum(db) =>
        {
            let const_ty = const_ty_from_sem_const(
                db,
                enum_const(
                    db,
                    variant.ty,
                    VariantIndex(variant.variant.idx),
                    Box::new([]),
                ),
            );
            TyId::const_ty(db, const_ty)
        }
        res => TyId::invalid(db, InvalidCause::NotAType(res)),
    })
}

fn lower_hir_ty_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _ty: HirTyId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    // On cycles during type lowering, treat the type as invalid. The cause
    // renders a diagnostic at the use site: cyclic shapes are normally
    // rejected by dedicated checks first (alias cycles, recursive types,
    // cyclic trait bounds), so any cycle that converges to this value is a
    // shape those checks missed and must not be silently invalid.
    TyId::invalid(db, InvalidCause::TypeLoweringCycle)
}

fn lower_const_ty_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: HirTyId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let HirTyKind::Path(path) = ty.data(db) else {
        return TyId::invalid(db, InvalidCause::InvalidConstParamTy);
    };

    let Some(path) = path.to_opt() else {
        return TyId::invalid(db, InvalidCause::ParseError);
    };
    if !path.generic_args(db).is_empty(db) {
        return TyId::invalid(db, InvalidCause::InvalidConstParamTy);
    }
    let ty = normalize_ty(
        db,
        lower_path_impl(
            db,
            scope,
            Partial::Present(path),
            assumptions,
            TypePosition::Type,
            &LoweringContext::new(),
        ),
        scope,
        assumptions,
    );

    if ty.has_invalid(db)
        || ty.is_integral(db)
        || ty.is_bool(db)
        || ty.is_unit_variant_only_enum(db)
    {
        ty
    } else {
        TyId::invalid(db, InvalidCause::InvalidConstParamTy)
    }
}

pub(crate) fn generic_param_owner_assumptions<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> PredicateListId<'db> {
    GenericParamOwner::from_item_opt(scope.item()).map_or_else(
        || PredicateListId::empty_list(db),
        |owner| match owner {
            GenericParamOwner::Func(func) => {
                collect_func_decl_constraints(db, func.into(), true).instantiate_identity()
            }
            _ => collect_constraints(db, owner).instantiate_identity(),
        },
    )
}

/// Collects the generic parameters of the given generic parameter owner.
#[salsa::tracked(returns(copy), cycle_initial=collect_generic_params_cycle_initial)]
pub(crate) fn collect_generic_params<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> GenericParamTypeSet<'db> {
    GenericParamCollector::new(db, owner, true).finalize()
}

/// Stable declaration parameters for shape discovery. This query must never
/// depend on the hidden slots whose number is being discovered.
#[salsa::tracked(returns(copy))]
pub(crate) fn collect_source_generic_params<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> GenericParamTypeSet<'db> {
    GenericParamCollector::new(db, owner, false).finalize()
}

fn collect_generic_params_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    owner: GenericParamOwner<'db>,
) -> GenericParamTypeSet<'db> {
    // Explicit parameters are available before signature-derived parameters.
    // Retaining them lets anonymous signature constants resolve their binders
    // while the implicit layout plan converges.
    match owner {
        GenericParamOwner::Func(func) if func.is_free_or_inherent(db) => {
            GenericParamCollector::new(db, owner, false).finalize()
        }
        _ => GenericParamTypeSet::empty(db, owner.scope()),
    }
}

/// The parameters a function adds after its inherited ones: one provider per
/// keyed effect, in effect order.
#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub(crate) struct FuncImplicitParamPlan<'db> {
    pub(crate) implicit_precursors: Vec<TyParamPrecursor<'db>>,
    pub(crate) provider_param_index_by_effect: Vec<Option<usize>>,
}

fn func_inherited_param_precursors<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> Vec<TyParamPrecursor<'db>> {
    if !func.is_associated_func(db) {
        return Vec::new();
    }

    let parent = GenericParamOwner::Func(func).parent(db).unwrap();
    collect_generic_params(db, parent)
        .params_precursor(db)
        .to_vec()
}

pub(crate) fn func_implicit_param_plan<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> FuncImplicitParamPlan<'db> {
    let prefix_len = func_inherited_param_precursors(db, func).len();
    let mut implicit_precursors = Vec::new();
    let mut provider_param_index_by_effect = vec![None; func.effects(db).data(db).len()];
    // Reserve providers from syntax, including invalid keys. Key validation depends
    // on the explicit parameters after this prefix; letting it add/remove slots
    // makes their indices oscillate during generic-parameter query recovery.
    for (provider_idx, effect) in func
        .effect_params(db)
        .filter(|effect| effect.key_ty(db).is_some())
        .enumerate()
    {
        let lowered_idx = prefix_len + implicit_precursors.len();
        let name = IdentId::new(db, format!("__effprov{provider_idx}"));
        implicit_precursors.push(TyParamPrecursor::effect_provider_param(Partial::Present(
            name,
        )));
        provider_param_index_by_effect[effect.index()] = Some(lowered_idx);
    }

    FuncImplicitParamPlan {
        implicit_precursors,
        provider_param_index_by_effect,
    }
}

/// Lowers the given type alias to [`TyAlias`].
#[salsa::tracked(cycle_initial=lower_type_alias_cycle_initial)]
pub(crate) fn lower_type_alias<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
) -> TyAlias<'db> {
    crate::core::semantic::lower_type_alias_body(db, alias)
}

#[salsa::tracked(cycle_initial=lower_type_alias_cycle_initial)]
pub(crate) fn lower_type_alias_deferred<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
) -> TyAlias<'db> {
    crate::core::semantic::lower_type_alias_body_deferred(db, alias)
}

pub(crate) fn lower_type_alias_from_hir<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
    alias_type_ref: Option<HirTyId<'db>>,
) -> TyAlias<'db> {
    lower_type_alias_from_hir_in_mode(db, alias, alias_type_ref, ConstBodyLowering::Eager)
}

pub(crate) fn lower_type_alias_from_hir_deferred<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
    alias_type_ref: Option<HirTyId<'db>>,
) -> TyAlias<'db> {
    lower_type_alias_from_hir_in_mode(db, alias, alias_type_ref, ConstBodyLowering::Deferred)
}

fn lower_type_alias_from_hir_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
    alias_type_ref: Option<HirTyId<'db>>,
    const_bodies: ConstBodyLowering,
) -> TyAlias<'db> {
    let param_set = collect_generic_params(db, alias.into());

    let Some(hir_ty) = alias_type_ref else {
        return TyAlias {
            alias,
            alias_to: Binder::bind(alias.into(), TyId::invalid(db, InvalidCause::ParseError)),
            param_set,
        };
    };

    let assumptions = match const_bodies {
        ConstBodyLowering::Eager => collect_constraints(db, alias.into()),
        ConstBodyLowering::Deferred => collect_candidate_constraints(db, alias.into()),
    }
    .instantiate_identity();
    let lowering_cx = LoweringContext::for_const_bodies(const_bodies);
    let alias_to = match const_bodies {
        ConstBodyLowering::Eager => lower_hir_ty(db, hir_ty, alias.scope(), assumptions),
        ConstBodyLowering::Deferred => {
            lower_hir_ty_impl(db, hir_ty, alias.scope(), assumptions, &lowering_cx)
        }
    };
    let alias_to = if let TyData::Invalid(InvalidCause::AliasCycle(cycle)) = alias_to.data(db) {
        if cycle.contains(&alias) {
            alias_to
        } else {
            let mut cycle = cycle.clone();
            cycle.push(alias);
            TyId::invalid(db, InvalidCause::AliasCycle(cycle))
        }
    } else if alias_to.has_invalid(db) {
        // Should be reported by TypeAliasAnalysisPass
        TyId::invalid(db, InvalidCause::Other)
    } else if let Some(space) = crate::core::semantic::type_alias_pointer_space(db, alias) {
        match alias_to.as_ptr(db) {
            Some((target, AddressSpace::Memory)) => TyId::ptr_in(db, space, target),
            _ => TyId::invalid(db, InvalidCause::Other),
        }
    } else {
        alias_to
    };
    TyAlias {
        alias,
        alias_to: Binder::bind(alias.into(), alias_to),
        param_set,
    }
}

fn lower_type_alias_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    alias: HirTypeAlias<'db>,
) -> TyAlias<'db> {
    TyAlias {
        alias,
        alias_to: Binder::bind(
            alias.into(),
            TyId::invalid(db, InvalidCause::AliasCycle(smallvec![alias])),
        ),
        param_set: GenericParamTypeSet::empty(db, alias.scope()),
    }
}

#[doc(hidden)]
#[salsa::tracked(cycle_initial=evaluate_params_precursor_cycle_initial)]
pub(crate) fn evaluate_params_precursor<'db>(
    db: &'db dyn HirAnalysisDb,
    set: GenericParamTypeSet<'db>,
) -> Vec<TyId<'db>> {
    set.params_precursor(db)
        .iter()
        .enumerate()
        .map(|(i, p)| p.evaluate(db, set.scope(db), i, set.offset_to_explicit(db)))
        .collect()
}

fn evaluate_params_precursor_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    set: GenericParamTypeSet<'db>,
) -> Vec<TyId<'db>> {
    set.params_precursor(db)
        .iter()
        .map(|_| TyId::invalid(db, InvalidCause::Other))
        .collect()
}

/// Represents a lowered type alias. `TyAlias` itself isn't a type, but
/// can be instantiated to a `TyId` by substituting its type
/// parameters with actual types.
///
/// NOTE: `TyAlias` can't become an alias to partial applied types, i.e., the
/// right hand side of the alias declaration must be a fully applied type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct TyAlias<'db> {
    pub alias: HirTypeAlias<'db>,
    pub alias_to: Binder<'db, TyId<'db>>,
    pub param_set: GenericParamTypeSet<'db>,
}

impl<'db> TyAlias<'db> {
    pub fn params(&self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set.params(db)
    }

    pub(crate) fn instantiate(
        &self,
        db: &'db dyn HirAnalysisDb,
        args: &[TyId<'db>],
        lowering_cx: &LoweringContext<'db>,
    ) -> TyId<'db> {
        let expected = self.param_set.explicit_param_count(db);
        debug_assert!(
            args.len() <= expected,
            "type alias path arity should be checked before instantiation"
        );
        let completed = self
            .param_set
            .complete_args(
                db,
                &[],
                args,
                DefaultApplication::StructuralMetadata(lowering_cx),
            )
            .map_err(|error| error.cause)
            .and_then(|args| {
                if args.len() < expected {
                    Err(InvalidCause::UnboundTypeAliasParam {
                        alias: self.alias,
                        n_given_args: args.len(),
                    })
                } else {
                    Ok(args)
                }
            });
        match completed {
            Ok(args) => self.alias_to.instantiate(db, &args),
            Err(cause) => TyId::invalid(db, cause),
        }
    }
}

pub(crate) fn lower_generic_arg_list<'db>(
    db: &'db dyn HirAnalysisDb,
    args: GenericArgListId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> Vec<TyId<'db>> {
    args.data(db)
        .iter()
        .map(|arg| match arg {
            // Generic args are syntactically ambiguous: `String<N>` may parse `N` as a type
            // even when `String` expects a const generic arg, so a path is lowered as the
            // constant it names, if it names one.
            GenericArg::Type(ty_arg) => match ty_arg.ty.to_opt().map(|ty| (ty, ty.data(db))) {
                Some((_, HirTyKind::Path(path))) => lower_path_impl(
                    db,
                    scope,
                    *path,
                    assumptions,
                    TypePosition::GenericArg,
                    lowering_cx,
                ),
                // A tuple whose parts carry modes or name a shape is a
                // parameter list, a shape: `Fn<(own A, B)>`.
                Some((ty, HirTyKind::Tuple(_)))
                    if let Some(list) = lower_shape_list(db, ty, scope, assumptions) =>
                {
                    list
                }
                _ => lower_opt_hir_ty_impl(db, ty_arg.ty, scope, assumptions, lowering_cx),
            },
            GenericArg::Mode(_) => TyId::invalid(db, InvalidCause::ModeArgNotShape),
            GenericArg::Const(const_arg) => TyId::const_ty(
                db,
                lower_opt_const_body(db, const_arg.value, scope, assumptions, lowering_cx),
            ),

            GenericArg::AssocType(_assoc_type_arg) => {
                // TODO: ?
                TyId::invalid(db, InvalidCause::Other)
            }
        })
        .collect()
}

#[salsa::interned]
#[derive(Debug)]
pub struct GenericParamTypeSet<'db> {
    pub(crate) params_precursor: Vec<TyParamPrecursor<'db>>,
    #[returns(copy)]
    pub(crate) scope: ScopeId<'db>,
    #[returns(copy)]
    offset_to_explicit: usize,
    #[returns(copy)]
    pub(crate) basis: ParamBasis,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum ParamBasis {
    Source,
    Full,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct SourceParamIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct LoweredSlot(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum ParamKey<'db> {
    TraitSelf(crate::hir_def::Trait<'db>),
    Source {
        owner: GenericParamOwner<'db>,
        index: SourceParamIndex,
    },
    EffectProvider {
        func: crate::hir_def::Func<'db>,
        effect_idx: usize,
    },
}

/// A view of the existing structural parameter plan. Source and full bases
/// share logical keys, while hidden slots appear only in the full view.
#[salsa::interned]
#[derive(Debug)]
pub struct ParamSchemaId<'db> {
    #[returns(copy)]
    pub owner: GenericParamOwner<'db>,
    #[returns(copy)]
    pub basis: ParamBasis,
    pub keys: Vec<ParamKey<'db>>,
}

impl<'db> ParamSchemaId<'db> {
    pub fn full(db: &'db dyn HirAnalysisDb, owner: GenericParamOwner<'db>) -> Self {
        param_schema(db, owner, ParamBasis::Full)
    }

    pub fn callable(db: &'db dyn HirAnalysisDb, callable: CallableDef<'db>) -> Self {
        Self::full(db, callable.generic_owner())
    }

    fn param_set(self, db: &'db dyn HirAnalysisDb) -> GenericParamTypeSet<'db> {
        match self.basis(db) {
            ParamBasis::Source => collect_source_generic_params(db, self.owner(db)),
            ParamBasis::Full => collect_generic_params(db, self.owner(db)),
        }
    }

    pub fn slot_for(self, db: &'db dyn HirAnalysisDb, key: ParamKey<'db>) -> Option<LoweredSlot> {
        self.keys(db)
            .iter()
            .position(|candidate| *candidate == key)
            .map(LoweredSlot)
    }

    pub fn key_at(self, db: &'db dyn HirAnalysisDb, slot: LoweredSlot) -> Option<ParamKey<'db>> {
        self.keys(db).get(slot.0).copied()
    }

    pub fn source_key(
        self,
        db: &'db dyn HirAnalysisDb,
        index: SourceParamIndex,
    ) -> Option<ParamKey<'db>> {
        let key = ParamKey::Source {
            owner: self.owner(db),
            index,
        };
        self.slot_for(db, key).map(|_| key)
    }

    pub fn formal_at(self, db: &'db dyn HirAnalysisDb, slot: LoweredSlot) -> Option<TyId<'db>> {
        self.param_set(db).params(db).get(slot.0).copied()
    }

    /// Resolves an original parameter occurrence in this schema's coordinate
    /// basis. Associated methods can also refer to their parent's original
    /// parameters, even though inherited formals are reminted in method scope.
    pub fn original_key(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<ParamKey<'db>> {
        let param = ty.as_generic_param(db)?;
        let slot = LoweredSlot(param.idx);
        if self
            .formal_at(db, slot)
            .and_then(|formal| formal.as_generic_param(db))
            == Some(param)
        {
            return self.key_at(db, slot);
        }
        let key = self.parent_schema(db)?.original_key(db, ty)?;
        self.slot_for(db, key).map(|_| key)
    }

    /// The formal for `slot` in declaration coordinates. An associated
    /// method's inherited slots use its parent's formals, since that is how
    /// the parent's declarations refer to them.
    pub fn declared_formal_at(
        self,
        db: &'db dyn HirAnalysisDb,
        slot: LoweredSlot,
    ) -> Option<TyId<'db>> {
        self.parent_schema(db)
            .zip(self.key_at(db, slot))
            .and_then(|(parent, key)| parent.formal_at(db, parent.slot_for(db, key)?))
            .or_else(|| self.formal_at(db, slot))
    }

    fn parent_schema(self, db: &'db dyn HirAnalysisDb) -> Option<Self> {
        match self.owner(db) {
            GenericParamOwner::Func(func) if func.is_associated_func(db) => {
                Some(Self::full(db, self.owner(db).parent(db)?))
            }
            GenericParamOwner::ImplTrait(implementation)
                if implementation.selection_name(db).is_some() =>
            {
                Some(Self::full(db, self.owner(db).parent(db)?))
            }
            _ => None,
        }
    }

    /// Resolves a declared occurrence in an explicitly chosen source basis.
    /// Source-only discovery must not query the full slot plan; only a full
    /// mapping may rebase a source occurrence into its corresponding key.
    pub fn original_key_in_basis(
        self,
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        basis: ParamBasis,
    ) -> Option<ParamKey<'db>> {
        if self.basis(db) == basis {
            return self.original_key(db, ty);
        }
        if self.basis(db) == ParamBasis::Full && basis == ParamBasis::Source {
            let source = param_schema(db, self.owner(db), ParamBasis::Source);
            let key = source.original_key(db, ty)?;
            return self.slot_for(db, key).map(|_| key);
        }
        None
    }

    pub fn allowed_default_dependencies(
        self,
        db: &'db dyn HirAnalysisDb,
        index: SourceParamIndex,
    ) -> Option<ParamDomainId<'db>> {
        let set = self.param_set(db);
        (index.0 < set.explicit_param_count(db)).then(|| {
            ParamDomainId::prefix(
                db,
                self,
                set.offset_to_explicit_params_position(db) + index.0,
            )
        })
    }

    /// The source parameter whose allowed default dependencies are the
    /// leading `len` slots; the inverse of `allowed_default_dependencies`.
    pub(crate) fn default_dependency_index(
        self,
        db: &'db dyn HirAnalysisDb,
        len: usize,
    ) -> Option<SourceParamIndex> {
        let set = self.param_set(db);
        len.checked_sub(set.offset_to_explicit_params_position(db))
            .filter(|&index| index < set.explicit_param_count(db))
            .map(SourceParamIndex)
    }
}

/// The leading `len` slots of one parameter schema: the full schema, a
/// default's allowed prefix, or a deferred const's capture set.
#[salsa::interned]
#[derive(Debug)]
pub struct ParamDomainId<'db> {
    #[returns(copy)]
    pub schema: ParamSchemaId<'db>,
    #[returns(copy)]
    pub len: usize,
}

impl<'db> ParamDomainId<'db> {
    pub fn full(db: &'db dyn HirAnalysisDb, schema: ParamSchemaId<'db>) -> Self {
        Self::prefix(db, schema, schema.keys(db).len())
    }

    fn prefix(db: &'db dyn HirAnalysisDb, schema: ParamSchemaId<'db>, len: usize) -> Self {
        assert!(
            len <= schema.keys(db).len(),
            "parameter domain exceeds schema"
        );
        Self::new(db, schema, len)
    }

    pub fn slots(self, db: &'db dyn HirAnalysisDb) -> impl Iterator<Item = LoweredSlot> {
        (0..self.len(db)).map(LoweredSlot)
    }

    pub fn position_for(self, db: &'db dyn HirAnalysisDb, key: ParamKey<'db>) -> Option<usize> {
        let slot = self.schema(db).slot_for(db, key)?;
        (slot.0 < self.len(db)).then_some(slot.0)
    }
}

#[derive(Debug, Clone)]
pub enum SubstError<'db> {
    InvalidDomain(ParamDomainId<'db>),
    WrongArity {
        domain: ParamDomainId<'db>,
        expected: usize,
        given: usize,
    },
    KeyOutsideDomain {
        domain: ParamDomainId<'db>,
        key: ParamKey<'db>,
    },
    ConflictingBinding {
        domain: ParamDomainId<'db>,
        key: ParamKey<'db>,
        first: TyId<'db>,
        second: TyId<'db>,
    },
    MissingArgument {
        domain: ParamDomainId<'db>,
        key: ParamKey<'db>,
    },
    WrongBasis {
        schema: ParamSchemaId<'db>,
        occurrence: TyId<'db>,
    },
}

#[derive(Debug, Clone)]
pub struct PartialSubst<'db> {
    domain: ParamDomainId<'db>,
    values: Vec<Option<TyId<'db>>>,
}

impl<'db> PartialSubst<'db> {
    pub fn domain(&self) -> ParamDomainId<'db> {
        self.domain
    }

    pub fn new(db: &'db dyn HirAnalysisDb, domain: ParamDomainId<'db>) -> Self {
        Self {
            domain,
            values: vec![None; domain.len(db)],
        }
    }

    pub fn bind(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        key: ParamKey<'db>,
        value: TyId<'db>,
    ) -> Result<(), SubstError<'db>> {
        let position = self
            .domain
            .position_for(db, key)
            .ok_or(SubstError::KeyOutsideDomain {
                domain: self.domain,
                key,
            })?;
        if let Some(first) = self.values[position]
            && first != value
        {
            return Err(SubstError::ConflictingBinding {
                domain: self.domain,
                key,
                first,
                second: value,
            });
        }
        self.values[position] = Some(value);
        Ok(())
    }

    pub fn get(&self, db: &'db dyn HirAnalysisDb, key: ParamKey<'db>) -> Option<TyId<'db>> {
        self.domain
            .position_for(db, key)
            .and_then(|position| self.values[position])
    }

    /// Completes the substitution, leaving unbound slots at their declaration
    /// formals.
    pub fn residualize(&self, db: &'db dyn HirAnalysisDb) -> CompleteSubst<'db> {
        CompleteSubst::from_optional_values(db, self.domain, |slot| self.values[slot.0])
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct CompleteSubst<'db> {
    domain: ParamDomainId<'db>,
    values: Vec<TyId<'db>>,
}

impl<'db> CompleteSubst<'db> {
    pub fn new(
        domain: ParamDomainId<'db>,
        db: &'db dyn HirAnalysisDb,
        values: Vec<TyId<'db>>,
    ) -> Result<Self, SubstError<'db>> {
        if values.len() != domain.len(db) {
            return Err(SubstError::WrongArity {
                domain,
                expected: domain.len(db),
                given: values.len(),
            });
        }
        Ok(Self { domain, values })
    }

    /// A substitution over `owner`'s full parameter schema.
    pub fn for_owner(
        db: &'db dyn HirAnalysisDb,
        owner: GenericParamOwner<'db>,
        values: Vec<TyId<'db>>,
    ) -> Result<Self, SubstError<'db>> {
        Self::new(
            ParamDomainId::full(db, ParamSchemaId::full(db, owner)),
            db,
            values,
        )
    }

    /// Binds the leading slots of `domain` to `prefix` and leaves the remaining
    /// slots at their declaration formals.
    pub fn with_prefix(
        db: &'db dyn HirAnalysisDb,
        domain: ParamDomainId<'db>,
        prefix: &[TyId<'db>],
    ) -> Self {
        assert!(
            prefix.len() <= domain.len(db),
            "substitution prefix exceeds its domain"
        );
        Self::from_optional_values(db, domain, |slot| prefix.get(slot.0).copied())
    }

    fn from_optional_values(
        db: &'db dyn HirAnalysisDb,
        domain: ParamDomainId<'db>,
        mut value: impl FnMut(LoweredSlot) -> Option<TyId<'db>>,
    ) -> Self {
        let schema = domain.schema(db);
        let values = domain
            .slots(db)
            .map(|slot| {
                value(slot).unwrap_or_else(|| {
                    schema
                        .formal_at(db, slot)
                        .expect("domain slot has a formal")
                })
            })
            .collect();
        Self { domain, values }
    }

    /// Maps each value while keeping the domain.
    pub fn map_values(&self, f: impl FnMut(TyId<'db>) -> TyId<'db>) -> Self {
        Self {
            domain: self.domain,
            values: self.values.iter().copied().map(f).collect(),
        }
    }

    pub fn domain(&self) -> ParamDomainId<'db> {
        self.domain
    }

    pub fn get(&self, db: &'db dyn HirAnalysisDb, key: ParamKey<'db>) -> Option<TyId<'db>> {
        self.domain
            .position_for(db, key)
            .and_then(|position| self.values.get(position).copied())
    }

    pub fn values(&self) -> &[TyId<'db>] {
        &self.values
    }

    pub fn into_values(self) -> Vec<TyId<'db>> {
        self.values
    }
}

#[salsa::tracked(returns(copy))]
pub fn param_schema<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    basis: ParamBasis,
) -> ParamSchemaId<'db> {
    let set = match basis {
        ParamBasis::Source => collect_source_generic_params(db, owner),
        ParamBasis::Full => collect_generic_params(db, owner),
    };
    debug_assert_eq!(set.basis(db), basis);
    let mut keys = vec![None; set.params_precursor(db).len()];
    if matches!(owner, GenericParamOwner::Func(func) if func.is_associated_func(db))
        || matches!(owner, GenericParamOwner::ImplTrait(implementation) if implementation.selection_name(db).is_some())
    {
        if let Some(parent) = owner.parent(db) {
            let parent_schema = param_schema(db, parent, ParamBasis::Full);
            for (slot, key) in parent_schema.keys(db).iter().copied().enumerate() {
                keys[slot] = Some(key);
            }
        }
    } else if let GenericParamOwner::Trait(trait_) = owner
        && let Some(first) = keys.first_mut()
    {
        *first = Some(ParamKey::TraitSelf(trait_));
    }

    if let (GenericParamOwner::Func(func), ParamBasis::Full) = (owner, basis) {
        let plan = func_implicit_param_plan(db, func);
        for (effect_idx, slot) in plan.provider_param_index_by_effect.iter().enumerate() {
            if let Some(slot) = slot {
                keys[*slot] = Some(ParamKey::EffectProvider { func, effect_idx });
            }
        }
    }

    let offset = set.offset_to_explicit_params_position(db);
    for source_idx in 0..set.explicit_param_count(db) {
        keys[offset + source_idx] = Some(ParamKey::Source {
            owner,
            index: SourceParamIndex(source_idx),
        });
    }
    ParamSchemaId::new(
        db,
        owner,
        basis,
        keys.into_iter()
            .map(|key| key.expect("structural parameter has no logical identity"))
            .collect::<Vec<_>>(),
    )
}

impl<'db> GenericParamTypeSet<'db> {
    pub(crate) fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        evaluate_params_precursor(db, self)
    }

    pub(crate) fn explicit_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        let offset = self.offset_to_explicit(db);
        &self.params(db)[offset..]
    }

    pub(crate) fn explicit_param_count(self, db: &'db dyn HirAnalysisDb) -> usize {
        self.params_precursor(db)
            .len()
            .saturating_sub(self.offset_to_explicit(db))
    }

    pub(crate) fn required_explicit_param_count(self, db: &'db dyn HirAnalysisDb) -> usize {
        self.params_precursor(db)[self.offset_to_explicit(db)..]
            .iter()
            .rposition(|param| param.default_hir_ty.is_none() && param.default_hir_const.is_none())
            .map_or(0, |idx| idx + 1)
    }

    pub(crate) fn empty(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Self {
        Self::new(db, Vec::new(), scope, 0, ParamBasis::Full)
    }

    pub(crate) fn trait_self(&self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let params = self.params_precursor(db);
        let cand = params.first()?;

        if cand.is_trait_self() {
            Some(cand.evaluate(db, self.scope(db), 0, self.offset_to_explicit(db)))
        } else {
            None
        }
    }

    pub(crate) fn offset_to_explicit_params_position(&self, db: &dyn HirAnalysisDb) -> usize {
        self.offset_to_explicit(db)
    }

    pub(crate) fn param_by_original_idx(
        &self,
        db: &'db dyn HirAnalysisDb,
        original_idx: usize,
    ) -> Option<TyId<'db>> {
        let idx = self.offset_to_explicit(db) + original_idx;
        self.params_precursor(db)
            .get(idx)
            .map(|p| p.evaluate(db, self.scope(db), idx, self.offset_to_explicit(db)))
    }
}

struct GenericParamCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    params: Vec<TyParamPrecursor<'db>>,
    offset_to_original: usize,
    basis: ParamBasis,
}

impl<'db> GenericParamCollector<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        owner: GenericParamOwner<'db>,
        include_func_implicit_params: bool,
    ) -> Self {
        let mut params = match owner {
            GenericParamOwner::Trait(_) => {
                vec![TyParamPrecursor::trait_self(db, None)]
            }

            GenericParamOwner::Func(func) if func.is_associated_func(db) => {
                func_inherited_param_precursors(db, func)
            }
            GenericParamOwner::ImplTrait(implementation)
                if implementation.selection_name(db).is_some() =>
            {
                owner.parent(db).map_or_else(Vec::new, |parent| {
                    collect_generic_params(db, parent)
                        .params_precursor(db)
                        .to_vec()
                })
            }

            _ => vec![],
        };

        if include_func_implicit_params && let GenericParamOwner::Func(func) = owner {
            params.extend(func_implicit_param_plan(db, func).implicit_precursors);
        }

        let offset_to_original = params.len();
        Self {
            db,
            owner,
            params,
            offset_to_original,
            basis: if include_func_implicit_params {
                ParamBasis::Full
            } else {
                ParamBasis::Source
            },
        }
    }

    fn collect_generic_params(&mut self) {
        let hir_db = self.db;
        let params = self.owner.params(hir_db);
        for GenericParamView { param, .. } in params {
            match param {
                GenericParam::Type(param) => {
                    let name = param.name;

                    let kind = lower_kind_in_bounds(param.bounds.as_slice());
                    let default_hir_ty = param.default_ty;
                    self.params
                        .push(TyParamPrecursor::ty_param(name, kind, default_hir_ty));
                }

                GenericParam::Const(param) => {
                    let name = param.name;
                    let hir_ty = param.ty.to_opt();
                    let default = param.default;

                    self.params
                        .push(TyParamPrecursor::const_ty_param(name, hir_ty, default))
                }
            }
        }
    }

    fn collect_kind_in_where_clause(&mut self) {
        let Some(where_clause_owner) = self.owner.where_clause_owner() else {
            return;
        };

        let hir_db = self.db;
        let where_clause = where_clause_owner.clause(hir_db);
        for pred in where_clause.predicates(hir_db) {
            let Some(kind) = pred.kind(self.db) else {
                continue;
            };

            // Kind bound on a concrete type parameter in this owner.
            if let Some(orig_idx) = pred.param_original_index(hir_db) {
                let idx = orig_idx + self.offset_to_original;
                if let Some(param) = self.params.get_mut(idx)
                    && param.kind.is_none()
                    && !param.is_const_ty()
                {
                    param.kind = Some(kind.clone());
                }
                continue;
            }

            // Kind bound on `Self` in a trait owner.
            if pred.is_self_subject(hir_db)
                && matches!(self.owner, GenericParamOwner::Trait(_))
                && let Some(trait_self) = self.trait_self_ty_mut()
                && trait_self.kind.is_none()
            {
                trait_self.kind = Some(kind);
            }
        }
    }

    fn finalize(mut self) -> GenericParamTypeSet<'db> {
        self.collect_generic_params();
        self.collect_kind_in_where_clause();

        GenericParamTypeSet::new(
            self.db,
            self.params,
            self.owner.scope(),
            self.offset_to_original,
            self.basis,
        )
    }

    fn trait_self_ty_mut(&mut self) -> Option<&mut TyParamPrecursor<'db>> {
        let cand = self.params.get_mut(0)?;
        cand.is_trait_self().then_some(cand)
    }
}

#[doc(hidden)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct TyParamPrecursor<'db> {
    name: Partial<IdentId<'db>>,
    kind: Option<Kind>,
    variant: Variant<'db>,
    default_hir_ty: Option<HirTyId<'db>>, // Only used for type params
    default_hir_const: Option<Body<'db>>, // Only used for const params
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
enum Variant<'db> {
    TraitSelf,
    Normal,
    Const(Option<HirTyId<'db>>),
    EffectProvider,
}

impl<'db> TyParamPrecursor<'db> {
    fn evaluate(
        &self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        lowered_idx: usize,
        explicit_offset: usize,
    ) -> TyId<'db> {
        let Partial::Present(name) = self.name else {
            return TyId::invalid(db, InvalidCause::Other);
        };

        let kind = self.kind.clone().unwrap_or(Kind::Star);

        match self.variant {
            Variant::TraitSelf => {
                let param = TyParam::trait_self(db, kind, scope);
                TyId::new(db, TyData::TyParam(param))
            }
            Variant::Normal => {
                let param = TyParam::normal_param(
                    name,
                    lowered_idx,
                    kind,
                    scope,
                    lowered_idx.checked_sub(explicit_offset),
                );
                TyId::new(db, TyData::TyParam(param))
            }
            Variant::EffectProvider => {
                let param = TyParam::effect_provider_param(name, lowered_idx, scope);
                TyId::new(db, TyData::TyParam(param))
            }
            Variant::Const(Some(_)) => {
                let param = TyParam::normal_param(
                    name,
                    lowered_idx,
                    kind,
                    scope,
                    lowered_idx.checked_sub(explicit_offset),
                );
                let ty = self
                    .declared_const_ty(db, scope)
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other));
                let const_ty = ConstTyId::new(db, ConstTyData::TyParam(param, ty));
                TyId::new(db, TyData::ConstTy(const_ty))
            }
            Variant::Const(None) => TyId::invalid(db, InvalidCause::Other),
        }
    }

    fn ty_param(
        name: Partial<IdentId<'db>>,
        kind: Option<Kind>,
        default_hir_ty: Option<HirTyId<'db>>,
    ) -> Self {
        Self {
            name,
            kind,
            variant: Variant::Normal,
            default_hir_ty,
            default_hir_const: None,
        }
    }

    fn const_ty_param(
        name: Partial<IdentId<'db>>,
        ty: Option<HirTyId<'db>>,
        default: Option<Body<'db>>,
    ) -> Self {
        Self {
            name,
            kind: None,
            variant: Variant::Const(ty),
            default_hir_ty: None,
            default_hir_const: default,
        }
    }

    fn effect_provider_param(name: Partial<IdentId<'db>>) -> Self {
        Self {
            name,
            kind: Some(Kind::Star),
            variant: Variant::EffectProvider,
            default_hir_ty: None,
            default_hir_const: None,
        }
    }

    fn trait_self(db: &'db dyn HirAnalysisDb, kind: Option<Kind>) -> Self {
        let name = Partial::Present(IdentId::make_self_ty(db));
        Self {
            name,
            kind,
            variant: Variant::TraitSelf,
            default_hir_ty: None,
            default_hir_const: None,
        }
    }

    fn is_trait_self(&self) -> bool {
        matches!(self.variant, Variant::TraitSelf)
    }

    fn is_const_ty(&self) -> bool {
        matches!(self.variant, Variant::Const(_))
    }

    fn declared_const_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> Option<TyId<'db>> {
        let Variant::Const(Some(ty)) = self.variant else {
            return None;
        };
        let assumptions = generic_param_owner_assumptions(db, scope);
        Some(lower_const_ty_ty(db, scope, ty, assumptions))
    }
}

pub(super) fn lower_kind(kind: &HirKindBound) -> Kind {
    match kind {
        HirKindBound::Mono => Kind::Star,
        HirKindBound::Shape => Kind::Shape,
        HirKindBound::Abs(lhs, rhs) => match (lhs, rhs) {
            (Partial::Present(lhs), Partial::Present(rhs)) => {
                Kind::Abs(Box::new((lower_kind(lhs), lower_kind(rhs))))
            }
            (Partial::Present(lhs), Partial::Absent) => {
                Kind::Abs(Box::new((lower_kind(lhs), Kind::Any)))
            }
            (Partial::Absent, Partial::Present(rhs)) => {
                Kind::Abs(Box::new((Kind::Any, lower_kind(rhs))))
            }
            (Partial::Absent, Partial::Absent) => Kind::Abs(Box::new((Kind::Any, Kind::Any))),
        },
    }
}

/// Helper for extracting a lowered kind from a slice of HIR `TypeBound`s.
/// Returns the first kind bound if present.
pub(crate) fn lower_kind_in_bounds<'db>(bounds: &[TypeBound<'db>]) -> Option<Kind> {
    for bound in bounds {
        if let TypeBound::Kind(Partial::Present(k)) = bound {
            return Some(lower_kind(k));
        }
    }
    None
}
