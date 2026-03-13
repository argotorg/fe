use crate::core::hir_def::{
    ConstGenericArgValue, GenericArg, GenericArgListId, GenericParam, GenericParamOwner,
    GenericParamView, IdentId, KindBound as HirKindBound, Partial, PathId,
    TypeAlias as HirTypeAlias, TypeBound, TypeId as HirTyId, TypeKind as HirTyKind, TypeMode,
    scope_graph::ScopeId,
};
use rustc_hash::FxHashMap;
use salsa::Update;
use smallvec::smallvec;

use super::{
    const_expr::{ConstExpr, ConstExprId},
    const_ty::{
        CallableInputHoleCx, CallableInputLayoutHoleOrigin, ConstTyData, ConstTyId,
        EvaluatedConstTy, LayoutHoleArgSite,
    },
    effects::ResolvedEffectKey,
    fold::{TyFoldable, TyFolder},
    layout_holes::{collect_unique_layout_placeholders_in_order, layout_hole_fallback_ty},
    trait_def::TraitInstId,
    trait_resolution::{
        PredicateListId, TraitSolveCx,
        constraint::{collect_constraints, collect_func_decl_constraints},
    },
    ty_def::{InvalidCause, Kind, TyData, TyId, TyParam},
};
use crate::analysis::name_resolution::{PathRes, PathResErrorKind, resolve_path};
use crate::analysis::{HirAnalysisDb, ty::binder::Binder};

/// Lowers the given HirTy to `TyId`.
#[salsa::tracked(cycle_fn=lower_hir_ty_cycle_recover, cycle_initial=lower_hir_ty_cycle_initial)]
pub fn lower_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    lower_hir_ty_impl(db, ty, scope, assumptions, None)
}

pub(crate) fn lower_hir_ty_with_callable_holes<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    callable_holes: &CallableInputHoleCx<'db>,
) -> TyId<'db> {
    lower_hir_ty_impl(db, ty, scope, assumptions, Some(callable_holes))
}

fn lower_hir_ty_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    callable_holes: Option<&CallableInputHoleCx<'db>>,
) -> TyId<'db> {
    match ty.data(db) {
        HirTyKind::Ptr(pointee) => {
            let pointee = lower_opt_hir_ty_impl(db, *pointee, scope, assumptions, callable_holes);
            let ptr = TyId::ptr(db);
            TyId::app(db, ptr, pointee)
        }

        HirTyKind::Mode(mode, inner) => {
            let inner = lower_opt_hir_ty_impl(db, *inner, scope, assumptions, callable_holes);
            match mode {
                TypeMode::Mut => TyId::borrow_mut_of(db, inner),
                TypeMode::Ref => TyId::borrow_ref_of(db, inner),
                TypeMode::Own => inner,
            }
        }

        HirTyKind::Path(path) => lower_path_impl(db, scope, *path, assumptions, callable_holes),

        HirTyKind::Tuple(tuple_id) => {
            let elems = tuple_id.data(db);
            let len = elems.len();
            let tuple = TyId::tuple(db, len);
            elems.iter().fold(tuple, |acc, &elem| {
                let elem_ty = lower_opt_hir_ty_impl(db, elem, scope, assumptions, callable_holes);
                if !elem_ty.has_star_kind(db) {
                    return TyId::invalid(db, InvalidCause::NotFullyApplied);
                }

                TyId::app(db, acc, elem_ty)
            })
        }

        HirTyKind::Array(hir_elem_ty, len) => {
            let elem_ty =
                lower_opt_hir_ty_impl(db, *hir_elem_ty, scope, assumptions, callable_holes);
            let len_ty = ConstTyId::from_opt_body(db, *len);
            let len_ty = TyId::const_ty(db, len_ty);
            let array = TyId::array(db, elem_ty);
            TyId::app(db, array, len_ty)
        }

        HirTyKind::Never => TyId::never(db),
    }
}

pub fn lower_opt_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Partial<HirTyId<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    lower_opt_hir_ty_impl(db, ty, scope, assumptions, None)
}

pub(crate) fn lower_opt_hir_ty_with_callable_holes<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Partial<HirTyId<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    callable_holes: &CallableInputHoleCx<'db>,
) -> TyId<'db> {
    lower_opt_hir_ty_impl(db, ty, scope, assumptions, Some(callable_holes))
}

fn lower_opt_hir_ty_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Partial<HirTyId<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    callable_holes: Option<&CallableInputHoleCx<'db>>,
) -> TyId<'db> {
    ty.to_opt()
        .map(|hir_ty| lower_hir_ty_impl(db, hir_ty, scope, assumptions, callable_holes))
        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
}

fn lower_path_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: Partial<PathId<'db>>,
    assumptions: PredicateListId<'db>,
    callable_holes: Option<&CallableInputHoleCx<'db>>,
) -> TyId<'db> {
    let Some(path) = path.to_opt() else {
        return TyId::invalid(db, InvalidCause::ParseError);
    };

    match crate::analysis::name_resolution::resolve_path_with_callable_holes(
        db,
        path,
        scope,
        assumptions,
        false,
        callable_holes,
    ) {
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty) | PathRes::Func(ty)) => ty,
        Ok(res) => TyId::invalid(db, InvalidCause::NotAType(res)),
        Err(err) => {
            // Try to resolve as a value, to find a matching `const` definition
            if matches!(err.kind, PathResErrorKind::NotFound { .. })
                && let Ok(resolved) =
                    crate::analysis::name_resolution::resolve_path_with_callable_holes(
                        db,
                        path,
                        scope,
                        assumptions,
                        true,
                        callable_holes,
                    )
            {
                return match resolved {
                    PathRes::Const(const_def, ty) => {
                        if let Some(body) = const_def.body(db).to_opt() {
                            let const_ty =
                                ConstTyId::from_body(db, body, Some(ty), Some(const_def));
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
                        );

                        let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
                        if let Some(const_ty) =
                            super::const_ty::const_ty_from_trait_const(db, solve_cx, inst, name)
                        {
                            TyId::const_ty(db, const_ty)
                        } else if let Some(expected_ty) = inst
                            .def(db)
                            .const_(db, name)
                            .and_then(|v| v.ty_binder(db))
                            .map(|b| b.instantiate(db, inst.args(db)))
                        {
                            let expr = ConstExprId::new(db, ConstExpr::TraitConst { inst, name });
                            let const_ty =
                                ConstTyId::new(db, ConstTyData::Abstract(expr, expected_ty));
                            TyId::const_ty(db, const_ty)
                        } else {
                            TyId::invalid(db, InvalidCause::Other)
                        }
                    }
                    other => TyId::invalid(db, InvalidCause::NotAType(other)),
                };
            }

            TyId::invalid(db, InvalidCause::PathResolutionFailed { path })
        }
    }
}

fn lower_hir_ty_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _ty: HirTyId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    // On cycles during type lowering, treat the type as invalid so that
    // callers can emit a suitable diagnostic without recursing further.
    TyId::invalid(db, InvalidCause::Other)
}

fn lower_hir_ty_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &TyId<'db>,
    _count: u32,
    _ty: HirTyId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
) -> salsa::CycleRecoveryAction<TyId<'db>> {
    // Keep iterating until we reach a fixpoint; the initial value is
    // already marked invalid, so subsequent iterations will converge
    // quickly without panicking.
    salsa::CycleRecoveryAction::Iterate
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

    if !path
        .to_opt()
        .map(|p| p.generic_args(db).is_empty(db))
        .unwrap_or(true)
    {
        return TyId::invalid(db, InvalidCause::InvalidConstParamTy);
    }
    let ty = lower_path(db, scope, *path, assumptions);

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

fn lower_path<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: Partial<PathId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    lower_path_impl(db, scope, path, assumptions, None)
}

fn generic_param_owner_assumptions<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> PredicateListId<'db> {
    GenericParamOwner::from_item_opt(scope.item())
        .map(|owner| match owner {
            GenericParamOwner::Func(func) => {
                collect_func_decl_constraints(db, func.into(), true).instantiate_identity()
            }
            _ => collect_constraints(db, owner).instantiate_identity(),
        })
        .unwrap_or_else(|| PredicateListId::empty_list(db))
}

/// Collects the generic parameters of the given generic parameter owner.
#[salsa::tracked(
    cycle_initial=collect_generic_params_cycle_initial,
    cycle_fn=collect_generic_params_cycle_recover
)]
pub(crate) fn collect_generic_params<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> GenericParamTypeSet<'db> {
    GenericParamCollector::new(db, owner, true).finalize()
}

#[salsa::tracked(
    cycle_initial=base_generic_params_cycle_initial,
    cycle_fn=base_generic_params_cycle_recover
)]
pub(crate) fn base_generic_params<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> GenericParamTypeSet<'db> {
    GenericParamCollector::new(db, owner, false).finalize()
}

fn collect_generic_params_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> GenericParamTypeSet<'db> {
    GenericParamTypeSet::empty(db, owner.scope())
}

fn collect_generic_params_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &GenericParamTypeSet<'db>,
    _count: u32,
    _owner: GenericParamOwner<'db>,
) -> salsa::CycleRecoveryAction<GenericParamTypeSet<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

fn base_generic_params_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> GenericParamTypeSet<'db> {
    GenericParamTypeSet::empty(db, owner.scope())
}

fn base_generic_params_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &GenericParamTypeSet<'db>,
    _count: u32,
    _owner: GenericParamOwner<'db>,
) -> salsa::CycleRecoveryAction<GenericParamTypeSet<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct CallableInputLayoutHoleGroup<'db> {
    pub(crate) origin: CallableInputLayoutHoleOrigin,
    pub(crate) placeholders: Vec<TyId<'db>>,
}

pub(crate) struct FuncImplicitParamPlan<'db> {
    pub(crate) implicit_precursors: Vec<TyParamPrecursor<'db>>,
    pub(crate) bindings_by_origin:
        FxHashMap<CallableInputLayoutHoleOrigin, Vec<(TyId<'db>, TyId<'db>)>>,
    pub(crate) provider_param_index_by_effect: Vec<Option<usize>>,
}

pub(crate) fn lower_callable_input_param_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    origin: CallableInputLayoutHoleOrigin,
    hir_ty: HirTyId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    with_callable_input_holes(func, origin, |callable_holes| {
        lower_hir_ty_with_callable_holes(db, hir_ty, func.scope(), assumptions, callable_holes)
    })
}

pub(crate) fn resolve_callable_input_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    effect_idx: usize,
    key_path: PathId<'db>,
    assumptions: PredicateListId<'db>,
) -> ResolvedEffectKey<'db> {
    with_callable_input_holes(
        func,
        CallableInputLayoutHoleOrigin::Effect(effect_idx),
        |callable_holes| {
            super::effects::resolve_effect_key_with_callable_holes(
                db,
                key_path,
                func.scope(),
                assumptions,
                Some(callable_holes),
            )
        },
    )
}

fn with_callable_input_holes<'db, T>(
    func: crate::hir_def::Func<'db>,
    origin: CallableInputLayoutHoleOrigin,
    f: impl FnOnce(&CallableInputHoleCx<'db>) -> T,
) -> T {
    let callable_holes = CallableInputHoleCx::new(func, origin);
    f(&callable_holes)
}

pub(crate) fn callable_input_layout_hole_groups<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> Vec<CallableInputLayoutHoleGroup<'db>> {
    let mut groups = Vec::new();
    let assumptions = collect_func_decl_constraints(db, func.into(), true).instantiate_identity();

    if func.is_method(db)
        && let Some(param) = func.params(db).next()
    {
        let receiver_ty = if param.self_ty_fallback(db) {
            func.expected_self_ty(db)
        } else {
            param.hir_ty(db).map(|hir_ty| {
                lower_callable_input_param_ty(
                    db,
                    func,
                    CallableInputLayoutHoleOrigin::Receiver,
                    hir_ty,
                    assumptions,
                )
            })
        };
        if let Some(receiver_ty) = receiver_ty {
            let placeholders = collect_unique_layout_placeholders_in_order(db, receiver_ty);
            if !placeholders.is_empty() {
                groups.push(CallableInputLayoutHoleGroup {
                    origin: CallableInputLayoutHoleOrigin::Receiver,
                    placeholders,
                });
            }
        }
    }
    for param in func.params(db) {
        if param.is_self_param(db) {
            continue;
        }
        let Some(hir_ty) = param.hir_ty(db) else {
            continue;
        };

        let ty = lower_callable_input_param_ty(
            db,
            func,
            CallableInputLayoutHoleOrigin::ValueParam(param.index()),
            hir_ty,
            assumptions,
        );
        let placeholders = collect_unique_layout_placeholders_in_order(db, ty);
        if placeholders.is_empty() {
            continue;
        }

        groups.push(CallableInputLayoutHoleGroup {
            origin: CallableInputLayoutHoleOrigin::ValueParam(param.index()),
            placeholders,
        });
    }

    for effect in func.effect_params(db) {
        let Some(key_path) = effect.key_path(db) else {
            continue;
        };
        let placeholders = match resolve_callable_input_effect_key(
            db,
            func,
            effect.index(),
            key_path,
            assumptions,
        ) {
            ResolvedEffectKey::Type(key_ty) => {
                collect_unique_layout_placeholders_in_order(db, key_ty)
            }
            ResolvedEffectKey::Trait(trait_inst) => {
                collect_unique_layout_placeholders_in_order(db, trait_inst)
            }
            ResolvedEffectKey::Other => continue,
        };
        if placeholders.is_empty() {
            continue;
        }

        groups.push(CallableInputLayoutHoleGroup {
            origin: CallableInputLayoutHoleOrigin::Effect(effect.index()),
            placeholders,
        });
    }

    groups
}

fn callable_input_layout_param_name<'db>(
    db: &'db dyn HirAnalysisDb,
    origin: CallableInputLayoutHoleOrigin,
    layout_idx: usize,
) -> IdentId<'db> {
    match origin {
        CallableInputLayoutHoleOrigin::Receiver => {
            IdentId::new(db, format!("__self_layout{layout_idx}"))
        }
        CallableInputLayoutHoleOrigin::ValueParam(param_idx) => {
            IdentId::new(db, format!("__arglayout{param_idx}_{layout_idx}"))
        }
        CallableInputLayoutHoleOrigin::Effect(effect_idx) => {
            IdentId::new(db, format!("__efflayout{effect_idx}_{layout_idx}"))
        }
    }
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
    let groups = callable_input_layout_hole_groups(db, func);
    let prefix_len = func_inherited_param_precursors(db, func).len();
    let mut implicit_precursors = Vec::new();
    let mut bindings_by_origin = FxHashMap::default();
    let assumptions = collect_func_decl_constraints(db, func.into(), true).instantiate_identity();

    for group in groups {
        let mut bindings = Vec::with_capacity(group.placeholders.len());
        for (layout_idx, placeholder) in group.placeholders.into_iter().enumerate() {
            let TyData::ConstTy(const_ty) = placeholder.data(db) else {
                unreachable!("callable layout placeholder was not a const type");
            };
            let ConstTyData::Hole(hole_ty, _) = const_ty.data(db) else {
                unreachable!("callable layout placeholder was not a hole");
            };
            let precursor = TyParamPrecursor::implicit_const_param(
                db,
                Partial::Present(callable_input_layout_param_name(
                    db,
                    group.origin,
                    layout_idx,
                )),
                layout_hole_fallback_ty(db, *hole_ty),
            );
            let lowered_idx = prefix_len + implicit_precursors.len();
            let implicit_arg = precursor.evaluate(db, func.scope(), lowered_idx);
            implicit_precursors.push(precursor);
            bindings.push((placeholder, implicit_arg));
        }
        bindings_by_origin.insert(group.origin, bindings);
    }

    let mut provider_param_index_by_effect = vec![None; func.effects(db).data(db).len()];
    let mut provider_idx = 0usize;
    for effect in func.effect_params(db) {
        let Some(key_path) = effect.key_path(db) else {
            continue;
        };
        if !matches!(
            resolve_callable_input_effect_key(db, func, effect.index(), key_path, assumptions),
            ResolvedEffectKey::Type(_) | ResolvedEffectKey::Trait(_)
        ) {
            continue;
        }

        let lowered_idx = prefix_len + implicit_precursors.len();
        let name = IdentId::new(db, format!("__effprov{provider_idx}"));
        provider_idx += 1;
        implicit_precursors.push(TyParamPrecursor::effect_provider_param(
            Partial::Present(name),
            lowered_idx,
        ));
        provider_param_index_by_effect[effect.index()] = Some(lowered_idx);
    }

    FuncImplicitParamPlan {
        implicit_precursors,
        bindings_by_origin,
        provider_param_index_by_effect,
    }
}

/// Lowers the given type alias to [`TyAlias`].
#[salsa::tracked(return_ref, cycle_fn=lower_type_alias_cycle_recover, cycle_initial=lower_type_alias_cycle_initial)]
pub(crate) fn lower_type_alias<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
) -> TyAlias<'db> {
    crate::core::semantic::lower_type_alias_body(db, alias)
}

pub(crate) fn lower_type_alias_from_hir<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
    alias_type_ref: Option<HirTyId<'db>>,
) -> TyAlias<'db> {
    let param_set = collect_generic_params(db, alias.into());

    let Some(hir_ty) = alias_type_ref else {
        return TyAlias {
            alias,
            alias_to: Binder::bind(TyId::invalid(db, InvalidCause::ParseError)),
            param_set,
        };
    };

    let assumptions = collect_constraints(db, alias.into()).instantiate_identity();
    let alias_to = lower_hir_ty(db, hir_ty, alias.scope(), assumptions);
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
    } else {
        alias_to
    };
    TyAlias {
        alias,
        alias_to: Binder::bind(alias_to),
        param_set,
    }
}

fn lower_type_alias_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
) -> TyAlias<'db> {
    TyAlias {
        alias,
        alias_to: Binder::bind(TyId::invalid(
            db,
            InvalidCause::AliasCycle(smallvec![alias]),
        )),
        param_set: GenericParamTypeSet::empty(db, alias.scope()),
    }
}

fn lower_type_alias_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &TyAlias<'db>,
    _count: u32,
    _alias: HirTypeAlias<'db>,
) -> salsa::CycleRecoveryAction<TyAlias<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

#[doc(hidden)]
#[salsa::tracked(return_ref, cycle_initial=evaluate_params_precursor_cycle_initial, cycle_fn=evaluate_params_precursor_cycle_recover)]
pub(crate) fn evaluate_params_precursor<'db>(
    db: &'db dyn HirAnalysisDb,
    set: GenericParamTypeSet<'db>,
) -> Vec<TyId<'db>> {
    set.params_precursor(db)
        .iter()
        .enumerate()
        .map(|(i, p)| p.evaluate(db, set.scope(db), i))
        .collect()
}

fn evaluate_params_precursor_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    set: GenericParamTypeSet<'db>,
) -> Vec<TyId<'db>> {
    set.params_precursor(db)
        .iter()
        .map(|_| TyId::invalid(db, InvalidCause::Other))
        .collect()
}

fn evaluate_params_precursor_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Vec<TyId<'db>>,
    _count: u32,
    _set: GenericParamTypeSet<'db>,
) -> salsa::CycleRecoveryAction<Vec<TyId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

/// Represents a lowered type alias. `TyAlias` itself isn't a type, but
/// can be instantiated to a `TyId` by substituting its type
/// parameters with actual types.
///
/// NOTE: `TyAlias` can't become an alias to partial applied types, i.e., the
/// right hand side of the alias declaration must be a fully applied type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct TyAlias<'db> {
    pub alias: HirTypeAlias<'db>,
    pub alias_to: Binder<TyId<'db>>,
    pub param_set: GenericParamTypeSet<'db>,
}

impl<'db> TyAlias<'db> {
    pub fn params(&self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set.params(db)
    }

    pub(crate) fn instantiate_from_path(
        &self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        args: &[TyId<'db>],
        assumptions: PredicateListId<'db>,
        callable_holes: Option<&CallableInputHoleCx<'db>>,
    ) -> TyId<'db> {
        let expected = self.param_set.explicit_param_count(db);
        debug_assert!(
            args.len() <= expected,
            "type alias path arity should be checked before instantiation"
        );
        let completed = self.param_set.complete_checked_explicit_args(
            db,
            None,
            args,
            assumptions,
            ConstDefaultCompletion::metadata(Some(path)).with_callable_holes(callable_holes),
        );
        if completed.len() < expected {
            return TyId::invalid(
                db,
                InvalidCause::UnboundTypeAliasParam {
                    alias: self.alias,
                    n_given_args: args.len(),
                },
            );
        }
        if let Some(cause) = completed.iter().find_map(|arg| arg.invalid_cause(db)) {
            return TyId::invalid(db, cause);
        }

        self.instantiate_completed_args(db, &completed, callable_holes)
    }

    fn instantiate_completed_args(
        &self,
        db: &'db dyn HirAnalysisDb,
        completed: &[TyId<'db>],
        callable_holes: Option<&CallableInputHoleCx<'db>>,
    ) -> TyId<'db> {
        let Some(callable_holes) = callable_holes else {
            return self.alias_to.instantiate(db, completed);
        };

        self.alias
            .hir_ty(db)
            .map(|hir_ty| {
                let assumptions = collect_constraints(db, self.alias.into()).instantiate_identity();
                let alias_to = lower_hir_ty_with_callable_holes(
                    db,
                    hir_ty,
                    self.alias.scope(),
                    assumptions,
                    callable_holes,
                );
                Binder::bind(alias_to).instantiate(db, completed)
            })
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
    }
}

pub(crate) fn lower_generic_arg_list<'db>(
    db: &'db dyn HirAnalysisDb,
    args: GenericArgListId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    hole_site: LayoutHoleArgSite<'db>,
) -> Vec<TyId<'db>> {
    lower_generic_arg_list_with_callable_holes(db, args, scope, assumptions, hole_site, None)
}

pub(crate) fn lower_generic_arg_list_with_callable_holes<'db>(
    db: &'db dyn HirAnalysisDb,
    args: GenericArgListId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    hole_site: LayoutHoleArgSite<'db>,
    callable_holes: Option<&CallableInputHoleCx<'db>>,
) -> Vec<TyId<'db>> {
    args.data(db)
        .iter()
        .enumerate()
        .map(|(arg_idx, arg)| match arg {
            GenericArg::Type(ty_arg) => {
                // Generic args are syntactically ambiguous: `String<N>` may parse `N` as a type
                // even when `String` expects a const generic arg. When a type-arg is a path that
                // resolves as a value const/trait-const, lower it as a const-ty argument so
                // downstream `TyId::app` sees a const generic.
                if let Some(hir_ty) = ty_arg.ty.to_opt()
                    && let HirTyKind::Path(path) = hir_ty.data(db)
                    && let Some(path) = path.to_opt()
                    && let Ok(resolved) = resolve_path(db, path, scope, assumptions, true)
                {
                    match resolved {
                        PathRes::Const(const_def, ty) => {
                            if let Some(body) = const_def.body(db).to_opt() {
                                let const_ty =
                                    ConstTyId::from_body(db, body, Some(ty), Some(const_def));
                                return TyId::const_ty(db, const_ty);
                            }
                            return TyId::invalid(db, InvalidCause::ParseError);
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
                            );

                            let solve_cx =
                                TraitSolveCx::new(db, scope).with_assumptions(assumptions);
                            if let Some(const_ty) =
                                super::const_ty::const_ty_from_trait_const(db, solve_cx, inst, name)
                            {
                                return TyId::const_ty(db, const_ty);
                            }
                            if let Some(expected_ty) = inst
                                .def(db)
                                .const_(db, name)
                                .and_then(|v| v.ty_binder(db))
                                .map(|b| b.instantiate(db, inst.args(db)))
                            {
                                let expr =
                                    ConstExprId::new(db, ConstExpr::TraitConst { inst, name });
                                let const_ty =
                                    ConstTyId::new(db, ConstTyData::Abstract(expr, expected_ty));
                                return TyId::const_ty(db, const_ty);
                            }
                        }
                        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                            if let TyData::ConstTy(const_ty) = ty.data(db) {
                                return TyId::const_ty(db, *const_ty);
                            }
                        }
                        PathRes::EnumVariant(variant)
                            if variant.ty.is_unit_variant_only_enum(db) =>
                        {
                            let evaluated = EvaluatedConstTy::EnumVariant(variant.variant);
                            let const_ty =
                                ConstTyId::new(db, ConstTyData::Evaluated(evaluated, variant.ty));
                            return TyId::const_ty(db, const_ty);
                        }
                        _ => {}
                    }
                }
                lower_opt_hir_ty_impl(db, ty_arg.ty, scope, assumptions, callable_holes)
            }
            GenericArg::Const(const_arg) => match const_arg.value {
                ConstGenericArgValue::Expr(body) => {
                    let const_ty = ConstTyId::from_opt_body(db, body);
                    TyId::const_ty(db, const_ty)
                }
                ConstGenericArgValue::Hole => TyId::const_ty(
                    db,
                    ConstTyId::hole_with_explicit_arg(
                        db,
                        TyId::invalid(db, InvalidCause::Other),
                        hole_site,
                        arg_idx,
                    ),
                ),
            },

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
    #[return_ref]
    pub(crate) params_precursor: Vec<TyParamPrecursor<'db>>,
    pub(crate) scope: ScopeId<'db>,
    offset_to_explicit: usize,
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

    pub(crate) fn explicit_const_param_default_hole_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        explicit_idx: usize,
    ) -> Option<TyId<'db>> {
        let idx = self.offset_to_explicit(db) + explicit_idx;
        let param = self.params_precursor(db).get(idx)?;
        matches!(param.default_hir_const, Some(ConstGenericArgValue::Hole))
            .then(|| param.declared_const_ty(db, self.scope(db)))
            .flatten()
    }

    pub(crate) fn empty(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Self {
        Self::new(db, Vec::new(), scope, 0)
    }

    pub(crate) fn trait_self(&self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let params = self.params_precursor(db);
        let cand = params.first()?;

        if cand.is_trait_self() {
            Some(cand.evaluate(db, self.scope(db), 0))
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
            .map(|p| p.evaluate(db, self.scope(db), idx))
    }

    /// Given explicit generic args provided at the use site, append any trailing
    /// defaults from this param set and return the completed explicit arg list.
    ///
    /// - `provided_explicit`: args corresponding to the explicit params (i.e.,
    ///   skipping implicit ones like trait `Self`).
    /// - `implicit_bindings`: mapping of (lowered_idx -> TyId) for implicit
    ///   parameters that should be available when evaluating defaults (e.g.,
    ///   trait `Self` at index 0).
    pub(crate) fn complete_explicit_args(
        self,
        db: &'db dyn HirAnalysisDb,
        trait_self: Option<TyId<'db>>,
        provided_explicit: &[TyId<'db>],
        assumptions: PredicateListId<'db>,
        completion: ConstDefaultCompletion<'_, 'db>,
    ) -> Vec<TyId<'db>> {
        self.complete_explicit_args_with_defaults_in_mode(
            db,
            trait_self,
            provided_explicit,
            assumptions,
            completion,
            false,
        )
    }

    fn complete_checked_explicit_args(
        self,
        db: &'db dyn HirAnalysisDb,
        trait_self: Option<TyId<'db>>,
        provided_explicit: &[TyId<'db>],
        assumptions: PredicateListId<'db>,
        completion: ConstDefaultCompletion<'_, 'db>,
    ) -> Vec<TyId<'db>> {
        self.complete_explicit_args_with_defaults_in_mode(
            db,
            trait_self,
            provided_explicit,
            assumptions,
            completion,
            true,
        )
    }

    fn checked_explicit_arg(
        self,
        db: &'db dyn HirAnalysisDb,
        explicit_idx: usize,
        ty: TyId<'db>,
    ) -> TyId<'db> {
        let lowered_idx = self.offset_to_explicit(db) + explicit_idx;
        let Some(param) = self.params_precursor(db).get(lowered_idx) else {
            return ty;
        };
        if !param.is_const_ty() {
            return ty;
        }

        ty.check_const_ty_without_eval(db, param.declared_const_ty(db, self.scope(db)))
            .unwrap_or_else(|cause| TyId::invalid(db, cause))
    }

    fn complete_explicit_args_with_defaults_in_mode(
        self,
        db: &'db dyn HirAnalysisDb,
        trait_self: Option<TyId<'db>>,
        provided_explicit: &[TyId<'db>],
        assumptions: PredicateListId<'db>,
        completion: ConstDefaultCompletion<'_, 'db>,
        checked_explicit: bool,
    ) -> Vec<TyId<'db>> {
        let total = self.params_precursor(db).len();
        let offset = self.offset_to_explicit(db);

        // mapping from lowered param idx -> bound arg, used to substitute in defaults
        let mut mapping = vec![];
        let mut result = Vec::with_capacity(provided_explicit.len());
        if let Some(self_ty) = trait_self {
            mapping.push(Some(self_ty));
        }
        for (explicit_idx, ty) in provided_explicit.iter().enumerate() {
            let checked = self.checked_explicit_arg(db, explicit_idx, *ty);
            mapping.push(Some(checked));
            result.push(if checked_explicit { checked } else { *ty });
        }
        mapping.resize(total, None);
        let scope = self.scope(db);

        let mapped_generic_args = |mapping: &[Option<TyId<'db>>], end: usize| {
            self.params_precursor(db)
                .iter()
                .take(end)
                .enumerate()
                .map(|(idx, param)| {
                    let arg = mapping[idx]
                        .expect("generic-default metadata args should only capture bound prefix");
                    if idx >= offset + provided_explicit.len() || !param.is_const_ty() {
                        return arg;
                    }
                    arg.evaluate_const_ty(db, param.declared_const_ty(db, scope))
                        .unwrap_or(arg)
                })
                .collect()
        };

        // Helper folder to substitute known params when lowering defaults
        struct ParamSubst<'a, 'db> {
            db: &'db dyn HirAnalysisDb,
            mapping: &'a [Option<TyId<'db>>],
        }
        impl<'a, 'db> TyFolder<'db> for ParamSubst<'a, 'db> {
            fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
                match ty.data(self.db) {
                    TyData::TyParam(param) => {
                        if let Some(Some(rep)) = self.mapping.get(param.idx) {
                            return *rep;
                        }
                        ty.super_fold_with(db, self)
                    }
                    TyData::ConstTy(const_ty) => {
                        if let super::const_ty::ConstTyData::TyParam(param, _) =
                            const_ty.data(self.db)
                            && let Some(Some(rep)) = self.mapping.get(param.idx)
                        {
                            return *rep;
                        }
                        ty.super_fold_with(db, self)
                    }
                    _ => ty.super_fold_with(db, self),
                }
            }
        }

        let substitute_known_params = |mapping: &[Option<TyId<'db>>], ty: TyId<'db>| {
            let mut subst = ParamSubst { db, mapping };
            ty.fold_with(db, &mut subst)
        };

        // Build the returned explicit arg list, appending defaults where available.
        for i in (offset + provided_explicit.len())..total {
            let prec = &self.params_precursor(db)[i];

            if let Some(hir_ty) = prec.default_hir_ty {
                let lowered =
                    lower_hir_ty_impl(db, hir_ty, scope, assumptions, completion.callable_holes);
                let lowered = substitute_known_params(&mapping, lowered);
                mapping[i] = Some(lowered);
                result.push(lowered);
                continue;
            }

            if let Some(default) = prec.default_hir_const {
                let expected = prec.declared_const_ty(db, scope);
                let lowered = match default {
                    ConstGenericArgValue::Expr(default) => {
                        let lowered = TyId::const_ty(
                            db,
                            ConstTyId::from_opt_body_with_ty_and_generic_args(
                                db,
                                default,
                                expected,
                                mapped_generic_args(&mapping, i),
                                matches!(completion.mode, ConstDefaultCompletionMode::MetadataOnly),
                            ),
                        );
                        match completion.mode {
                            ConstDefaultCompletionMode::MetadataOnly => lowered
                                .check_const_ty_without_eval(db, expected)
                                .unwrap_or_else(|cause| TyId::invalid(db, cause)),
                            ConstDefaultCompletionMode::Evaluate => lowered
                                .evaluate_const_ty(db, expected)
                                .unwrap_or_else(|cause| TyId::invalid(db, cause)),
                        }
                    }
                    ConstGenericArgValue::Hole => TyId::const_ty(
                        db,
                        completion
                            .callable_holes
                            .map(|callable_holes| {
                                ConstTyId::hole_with_layout_id(
                                    db,
                                    expected
                                        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),
                                    callable_holes.fresh_hole_id(),
                                )
                            })
                            .or_else(|| {
                                completion.application_path.map(|path| {
                                    ConstTyId::hole_with_path_arg(
                                        db,
                                        expected.unwrap_or_else(|| {
                                            TyId::invalid(db, InvalidCause::Other)
                                        }),
                                        path,
                                        i,
                                    )
                                })
                            })
                            .unwrap_or_else(|| {
                                ConstTyId::hole_with_ty(
                                    db,
                                    expected
                                        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),
                                )
                            }),
                    ),
                };

                let lowered = substitute_known_params(&mapping, lowered);

                mapping[i] = Some(lowered);
                result.push(lowered);
                continue;
            }

            break; // Missing non-default; stop filling further params
        }

        result
    }
}

#[derive(Clone, Copy)]
enum ConstDefaultCompletionMode {
    MetadataOnly,
    Evaluate,
}

#[derive(Clone, Copy)]
pub(crate) struct ConstDefaultCompletion<'a, 'db> {
    mode: ConstDefaultCompletionMode,
    application_path: Option<PathId<'db>>,
    callable_holes: Option<&'a CallableInputHoleCx<'db>>,
}

impl<'a, 'db> ConstDefaultCompletion<'a, 'db> {
    pub(crate) fn metadata(application_path: Option<PathId<'db>>) -> Self {
        Self {
            mode: ConstDefaultCompletionMode::MetadataOnly,
            application_path,
            callable_holes: None,
        }
    }

    pub(crate) fn evaluate(application_path: Option<PathId<'db>>) -> Self {
        Self {
            mode: ConstDefaultCompletionMode::Evaluate,
            application_path,
            callable_holes: None,
        }
    }

    pub(crate) fn with_callable_holes(
        mut self,
        callable_holes: Option<&'a CallableInputHoleCx<'db>>,
    ) -> Self {
        self.callable_holes = callable_holes;
        self
    }
}

struct GenericParamCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    params: Vec<TyParamPrecursor<'db>>,
    offset_to_original: usize,
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
        }
    }

    fn collect_generic_params(&mut self) {
        let hir_db = self.db;
        let params = self.owner.params(hir_db);
        for (idx, param) in params
            .map(|GenericParamView { param, .. }| param)
            .enumerate()
        {
            let idx = idx + self.offset_to_original;

            match param {
                GenericParam::Type(param) => {
                    let name = param.name;

                    let kind = lower_kind_in_bounds(param.bounds.as_slice());
                    let default_hir_ty = param.default_ty;
                    self.params
                        .push(TyParamPrecursor::ty_param(name, idx, kind, default_hir_ty));
                }

                GenericParam::Const(param) => {
                    let name = param.name;
                    let hir_ty = param.ty.to_opt();
                    let default = param.default;

                    self.params
                        .push(TyParamPrecursor::const_ty_param(name, idx, hir_ty, default))
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
        )
    }

    fn trait_self_ty_mut(&mut self) -> Option<&mut TyParamPrecursor<'db>> {
        let cand = self.params.get_mut(0)?;
        cand.is_trait_self().then_some(cand)
    }
}

#[doc(hidden)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyParamPrecursor<'db> {
    name: Partial<IdentId<'db>>,
    original_idx: Option<usize>,
    kind: Option<Kind>,
    variant: Variant<'db>,
    default_hir_ty: Option<HirTyId<'db>>, // Only used for type params
    default_hir_const: Option<ConstGenericArgValue<'db>>, // Only used for const params
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Variant<'db> {
    TraitSelf,
    Normal,
    Const(Option<HirTyId<'db>>),
    EffectProvider,
    ImplicitConst(TyId<'db>),
}

impl<'db> TyParamPrecursor<'db> {
    fn evaluate(
        &self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        lowered_idx: usize,
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
                let param = TyParam::normal_param(name, lowered_idx, kind, scope);
                TyId::new(db, TyData::TyParam(param))
            }
            Variant::EffectProvider => {
                let param = TyParam::effect_provider_param(name, lowered_idx, scope);
                TyId::new(db, TyData::TyParam(param))
            }
            Variant::Const(Some(_)) => {
                let param = TyParam::normal_param(name, lowered_idx, kind, scope);
                let ty = self
                    .declared_const_ty(db, scope)
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other));
                let const_ty = ConstTyId::new(db, ConstTyData::TyParam(param, ty));
                TyId::new(db, TyData::ConstTy(const_ty))
            }
            Variant::Const(None) => TyId::invalid(db, InvalidCause::Other),
            Variant::ImplicitConst(const_ty_ty) => {
                let param = TyParam::implicit_param(name, lowered_idx, kind, scope);
                let const_ty = ConstTyId::new(db, ConstTyData::TyParam(param, const_ty_ty));
                TyId::new(db, TyData::ConstTy(const_ty))
            }
        }
    }

    fn ty_param(
        name: Partial<IdentId<'db>>,
        idx: usize,
        kind: Option<Kind>,
        default_hir_ty: Option<HirTyId<'db>>,
    ) -> Self {
        Self {
            name,
            original_idx: idx.into(),
            kind,
            variant: Variant::Normal,
            default_hir_ty,
            default_hir_const: None,
        }
    }

    fn const_ty_param(
        name: Partial<IdentId<'db>>,
        idx: usize,
        ty: Option<HirTyId<'db>>,
        default: Option<ConstGenericArgValue<'db>>,
    ) -> Self {
        Self {
            name,
            original_idx: idx.into(),
            kind: None,
            variant: Variant::Const(ty),
            default_hir_ty: None,
            default_hir_const: default,
        }
    }

    fn effect_provider_param(name: Partial<IdentId<'db>>, idx: usize) -> Self {
        Self {
            name,
            original_idx: idx.into(),
            kind: Some(Kind::Star),
            variant: Variant::EffectProvider,
            default_hir_ty: None,
            default_hir_const: None,
        }
    }

    fn implicit_const_param(
        db: &'db dyn HirAnalysisDb,
        name: Partial<IdentId<'db>>,
        ty: TyId<'db>,
    ) -> Self {
        Self {
            name,
            original_idx: None,
            kind: Some(ty.kind(db).clone()),
            variant: Variant::ImplicitConst(ty),
            default_hir_ty: None,
            default_hir_const: None,
        }
    }

    fn trait_self(db: &'db dyn HirAnalysisDb, kind: Option<Kind>) -> Self {
        let name = Partial::Present(IdentId::make_self_ty(db));
        Self {
            name,
            original_idx: None,
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
        matches!(self.variant, Variant::Const(_) | Variant::ImplicitConst(_))
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
pub(super) fn lower_kind_in_bounds<'db>(bounds: &[TypeBound<'db>]) -> Option<Kind> {
    for bound in bounds {
        if let TypeBound::Kind(Partial::Present(k)) = bound {
            return Some(lower_kind(k));
        }
    }
    None
}
