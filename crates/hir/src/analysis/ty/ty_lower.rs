use crate::core::hir_def::{
    Body, CallableDef, ConstGenericArgValue, Expr, GenericArg, GenericArgListId, GenericParam,
    GenericParamOwner, GenericParamView, IdentId, KindBound as HirKindBound, Partial, PathId, Stmt,
    TypeAlias as HirTypeAlias, TypeBound, TypeId as HirTyId, TypeKind as HirTyKind, TypeMode,
    scope_graph::ScopeId,
};
use rustc_hash::FxHashMap;
use salsa::Update;
use smallvec::smallvec;

use super::{
    assoc_const::AssocConstUse,
    const_ty::{
        CallableInputLayoutHoleOrigin, ConstTyData, ConstTyId, EvaluatedConstTy, InstSite,
        LayoutHoleArgSite, LexSite, ProvenanceId, ProvenanceSite, StructuralHoleOrigin,
    },
    effects::{ResolvedEffectKey, TraitKeySchema},
    fold::{TyFoldable, TyFolder},
    layout_holes::{
        callable_input_layout_bindings_by_origin,
        collect_unique_app_bound_structural_holes_in_order,
        collect_unique_layout_placeholders_in_order, layout_hole_fallback_ty, push_provenance,
        push_provenance_filtered, rewrite_structural_holes,
    },
    trait_def::TraitInstId,
    trait_resolution::{
        PredicateListId,
        constraint::{collect_constraints, collect_func_decl_constraints},
    },
    ty_def::{InvalidCause, Kind, TyData, TyId, TyParam},
    visitor::TyVisitable,
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
    lower_hir_ty_impl(db, ty, scope, assumptions)
}

fn lower_hir_ty_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let lower_child = |child_ty, slot| {
        let lowered = lower_opt_hir_ty_impl(db, child_ty, scope, assumptions);
        // No `Lex(HirType(ty))` alongside: the component site already carries
        // `ty`, so the extra node cannot distinguish any two chains.
        push_provenance(
            db,
            lowered,
            &[ProvenanceSite::Lex(LexSite::TypeComponent { ty, slot })],
        )
    };

    match ty.data(db) {
        HirTyKind::Ptr(pointee) => {
            let pointee = lower_child(*pointee, 0);
            let ptr = TyId::ptr(db);
            TyId::app(db, ptr, pointee)
        }

        HirTyKind::Mode(mode, inner) => {
            let inner = lower_child(*inner, 0);
            match mode {
                TypeMode::Mut => TyId::borrow_mut_of(db, inner),
                TypeMode::Ref => TyId::borrow_ref_of(db, inner),
                TypeMode::Own => inner,
            }
        }

        HirTyKind::Path(path) => push_provenance(
            db,
            lower_path_impl(db, scope, *path, assumptions),
            &[ProvenanceSite::Lex(LexSite::HirType(ty))],
        ),

        HirTyKind::Tuple(tuple_id) => {
            let elems = tuple_id.data(db);
            let len = elems.len();
            let tuple = TyId::tuple(db, len);
            elems.iter().enumerate().fold(tuple, |acc, (idx, &elem)| {
                let elem_ty = lower_child(elem, idx);
                if !elem_ty.has_star_kind(db) {
                    return TyId::invalid(db, InvalidCause::NotFullyApplied);
                }

                TyId::app(db, acc, elem_ty)
            })
        }

        HirTyKind::Array(hir_elem_ty, len) => {
            let elem_ty = lower_child(*hir_elem_ty, 0);
            let len_ty = lower_opt_const_body(db, *len, scope, assumptions);
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
    lower_opt_hir_ty_impl(db, ty, scope, assumptions)
}

fn lower_opt_hir_ty_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Partial<HirTyId<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    ty.to_opt()
        .map(|hir_ty| lower_hir_ty_impl(db, hir_ty, scope, assumptions))
        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
}

fn const_body_simple_path<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> Option<PathId<'db>> {
    fn expr_simple_path<'db>(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: &Expr<'db>,
    ) -> Option<PathId<'db>> {
        match expr {
            Expr::Path(path) => path.to_opt(),
            Expr::Block(stmts) => {
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
/// const bodies like `Slot<{ Self::N }>` in a trait method must resolve
/// `Self::N` to a trait const the same way the body checker later does, or
/// the const falls back to an unevaluated body whose CTFE cannot resolve the
/// trait const reference.
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

fn lower_opt_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Partial<Body<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> ConstTyId<'db> {
    let Some(body) = body.to_opt() else {
        return ConstTyId::invalid(db, InvalidCause::ParseError);
    };
    let Some(path) = const_body_simple_path(db, body) else {
        return ConstTyId::from_body(db, body, None, None);
    };

    let assumptions = with_enclosing_trait_self_predicate(db, scope, assumptions);
    match resolve_path(db, path, scope, assumptions, true) {
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
            let inst =
                TraitInstId::new(db, inst.def(db), args, inst.assoc_type_bindings(db).clone());

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
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => {
            if let TyData::ConstTy(const_ty) = ty.data(db) {
                *const_ty
            } else {
                ConstTyId::from_body(db, body, None, None)
            }
        }
        Ok(PathRes::EnumVariant(variant)) if variant.ty.is_unit_variant_only_enum(db) => {
            ConstTyId::new(
                db,
                ConstTyData::Evaluated(EvaluatedConstTy::EnumVariant(variant.variant), variant.ty),
            )
        }
        _ => ConstTyId::from_body(db, body, None, None),
    }
}

fn lower_path_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: Partial<PathId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let Some(path) = path.to_opt() else {
        return TyId::invalid(db, InvalidCause::ParseError);
    };

    match crate::analysis::name_resolution::resolve_path(db, path, scope, assumptions, false) {
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty) | PathRes::Func(ty)) => ty,
        Ok(res) => TyId::invalid(db, InvalidCause::NotAType(res)),
        Err(err) => {
            // Try to resolve as a value, to find a matching `const` definition
            if matches!(err.kind, PathResErrorKind::NotFound { .. })
                && let Ok(resolved) = crate::analysis::name_resolution::resolve_path(
                    db,
                    path,
                    scope,
                    assumptions,
                    true,
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

                        if let Some(expected_ty) = inst
                            .def(db)
                            .const_(db, name)
                            .and_then(|v| v.ty_binder(db))
                            .map(|b| b.instantiate(db, inst.args(db)))
                        {
                            let assoc = AssocConstUse::new(scope, assumptions, inst, name);
                            if let Some(const_ty) =
                                super::const_ty::const_ty_or_abstract_from_assoc_const_use(
                                    db,
                                    assoc,
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
    lower_path_impl(db, scope, path, assumptions)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct CallableInputLayoutHoleGroup<'db> {
    pub(crate) origin: CallableInputLayoutHoleOrigin,
    pub(crate) placeholders: Vec<TyId<'db>>,
}

fn bind_callable_input_layout_holes<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    func: crate::hir_def::Func<'db>,
    origin: CallableInputLayoutHoleOrigin,
) -> T
where
    T: TyFoldable<'db> + TyVisitable<'db> + Copy,
{
    let value = push_provenance(
        db,
        value,
        &[ProvenanceSite::Inst(InstSite::CallableInput {
            func,
            origin,
        })],
    );
    let ordinals = collect_unique_app_bound_structural_holes_in_order(db, value)
        .into_iter()
        .enumerate()
        .map(|(ordinal, hole_id)| (hole_id, ordinal))
        .collect::<FxHashMap<_, _>>();

    rewrite_structural_holes(db, value, |hole_id, hole_ty| {
        ordinals.get(&hole_id).map(|ordinal| {
            TyId::const_ty(
                db,
                ConstTyId::bound_callable_hole(db, hole_ty, func, origin, *ordinal),
            )
        })
    })
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
    bind_callable_input_layout_holes(
        db,
        lower_hir_ty(db, hir_ty, func.scope(), assumptions),
        func,
        origin,
    )
}

pub(crate) fn resolve_callable_input_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    effect_idx: usize,
    key_path: PathId<'db>,
    assumptions: PredicateListId<'db>,
) -> ResolvedEffectKey<'db> {
    match super::effects::resolve_effect_key(db, key_path, func.scope(), assumptions) {
        ResolvedEffectKey::Type(mut schema) => {
            schema.carrier = bind_callable_input_layout_holes(
                db,
                schema.carrier,
                func,
                CallableInputLayoutHoleOrigin::Effect(effect_idx),
            );
            ResolvedEffectKey::Type(schema)
        }
        ResolvedEffectKey::Trait(schema) => {
            let inst = bind_callable_input_layout_holes(
                db,
                schema.into_trait_inst(db),
                func,
                CallableInputLayoutHoleOrigin::Effect(effect_idx),
            );
            ResolvedEffectKey::Trait(TraitKeySchema::from_canonical_trait_binding(db, inst))
        }
        ResolvedEffectKey::Invalid => ResolvedEffectKey::Invalid,
        ResolvedEffectKey::Other => ResolvedEffectKey::Other,
    }
}

pub(crate) fn instantiate_callable_effect_layout_args<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    effect_idx: usize,
    actual_key_ty: TyId<'db>,
    subst_args: &mut [TyId<'db>],
) {
    let assumptions = collect_func_decl_constraints(db, func.into(), true).instantiate_identity();
    let Some(key_path) = func
        .effect_params(db)
        .nth(effect_idx)
        .and_then(|effect| effect.key_path(db))
    else {
        return;
    };
    let ResolvedEffectKey::Type(expected_key) =
        resolve_callable_input_effect_key(db, func, effect_idx, key_path, assumptions)
    else {
        return;
    };
    let bindings = callable_input_layout_bindings_by_origin(db, CallableDef::Func(func));
    let Some(bindings) = bindings.get(&CallableInputLayoutHoleOrigin::Effect(effect_idx)) else {
        return;
    };
    let mut actual_layout_args = Vec::with_capacity(bindings.len());
    if !collect_layout_args_in_order(
        db,
        expected_key.carrier,
        actual_key_ty,
        &mut actual_layout_args,
    ) || actual_layout_args.len() != bindings.len()
    {
        return;
    }

    for ((_, implicit_arg), actual_arg) in bindings.iter().zip(actual_layout_args) {
        let implicit_idx = match implicit_arg.data(db) {
            TyData::TyParam(param) => Some(param.idx),
            TyData::ConstTy(const_ty) => match const_ty.data(db) {
                ConstTyData::TyParam(param, _) => Some(param.idx),
                _ => None,
            },
            _ => None,
        };
        if let Some(implicit_idx) = implicit_idx
            && let Some(slot) = subst_args.get_mut(implicit_idx)
        {
            *slot = actual_arg;
        }
    }
}

fn collect_layout_args_in_order<'db>(
    db: &'db dyn HirAnalysisDb,
    expected: TyId<'db>,
    actual: TyId<'db>,
    out: &mut Vec<TyId<'db>>,
) -> bool {
    if matches!(
        expected.data(db),
        TyData::ConstTy(const_ty) if matches!(const_ty.data(db), ConstTyData::Hole(..))
    ) {
        out.push(actual);
        return true;
    }

    let (expected_base, expected_args) = expected.decompose_ty_app(db);
    let (actual_base, actual_args) = actual.decompose_ty_app(db);
    if expected_args.len() != actual_args.len() {
        return false;
    }
    if expected_args.is_empty() {
        return expected == actual;
    }
    if expected_base != actual_base {
        return false;
    }

    expected_args
        .iter()
        .zip(actual_args.iter())
        .all(|(expected_arg, actual_arg)| {
            collect_layout_args_in_order(db, *expected_arg, *actual_arg, out)
        })
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
            ResolvedEffectKey::Type(schema) => {
                collect_unique_layout_placeholders_in_order(db, schema.carrier)
            }
            ResolvedEffectKey::Trait(schema) => {
                collect_unique_layout_placeholders_in_order(db, schema.into_trait_inst(db))
            }
            ResolvedEffectKey::Invalid | ResolvedEffectKey::Other => continue,
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

fn provenance_contains_alias_template<'db>(
    db: &'db dyn HirAnalysisDb,
    provenance: ProvenanceId<'db>,
    alias: HirTypeAlias<'db>,
) -> bool {
    provenance.contains(
        db,
        |site| matches!(site, ProvenanceSite::Lex(LexSite::AliasTemplate(found)) if found == alias),
    )
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
        push_provenance(
            db,
            alias_to,
            &[ProvenanceSite::Lex(LexSite::AliasTemplate(alias))],
        )
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
            ConstDefaultCompletion::metadata(Some(path)),
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

        self.instantiate_completed_args(db, &completed, path)
    }

    fn instantiate_completed_args(
        &self,
        db: &'db dyn HirAnalysisDb,
        completed: &[TyId<'db>],
        use_path: PathId<'db>,
    ) -> TyId<'db> {
        push_provenance_filtered(
            db,
            self.alias_to.instantiate(db, completed),
            &[ProvenanceSite::Inst(InstSite::RootPath(use_path))],
            |hole_id| provenance_contains_alias_template(db, hole_id.provenance(db), self.alias),
        )
    }
}

pub(crate) fn lower_generic_arg_list<'db>(
    db: &'db dyn HirAnalysisDb,
    args: GenericArgListId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    hole_site: LayoutHoleArgSite<'db>,
) -> Vec<TyId<'db>> {
    let hole_provenance_root = match hole_site {
        LayoutHoleArgSite::Path(path) => {
            ProvenanceId::root(db, ProvenanceSite::Lex(LexSite::RootPath(path)))
        }
        LayoutHoleArgSite::GenericArgList(args) => {
            ProvenanceId::root(db, ProvenanceSite::Lex(LexSite::GenericArgList(args)))
        }
    };
    let inst_root_site = match hole_site {
        LayoutHoleArgSite::Path(path) => ProvenanceSite::Inst(InstSite::RootPath(path)),
        LayoutHoleArgSite::GenericArgList(args) => {
            ProvenanceSite::Inst(InstSite::GenericArgList(args))
        }
    };

    args.data(db)
        .iter()
        .enumerate()
        .map(|(arg_idx, arg)| match arg {
            GenericArg::Type(ty_arg) => {
                let arg_sites = ty_arg.ty.to_opt().map(|hir_ty| {
                    [
                        ProvenanceSite::Inst(InstSite::TypeComponent {
                            ty: hir_ty,
                            slot: arg_idx,
                        }),
                        inst_root_site,
                    ]
                });
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

                            if let Some(expected_ty) = inst
                                .def(db)
                                .const_(db, name)
                                .and_then(|v| v.ty_binder(db))
                                .map(|b| b.instantiate(db, inst.args(db)))
                            {
                                let assoc = AssocConstUse::new(scope, assumptions, inst, name);
                                if let Some(const_ty) =
                                    super::const_ty::const_ty_or_abstract_from_assoc_const_use(
                                        db,
                                        assoc,
                                        expected_ty,
                                    )
                                {
                                    return TyId::const_ty(db, const_ty);
                                }
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
                let ty = lower_opt_hir_ty(db, ty_arg.ty, scope, assumptions);
                arg_sites.map_or(ty, |sites| push_provenance(db, ty, &sites))
            }
            GenericArg::Const(const_arg) => match const_arg.value {
                ConstGenericArgValue::Expr(body) => {
                    let const_ty = lower_opt_const_body(db, body, scope, assumptions);
                    TyId::const_ty(db, const_ty)
                }
                ConstGenericArgValue::Hole => TyId::const_ty(
                    db,
                    ConstTyId::structural_hole(
                        db,
                        TyId::invalid(db, InvalidCause::Other),
                        StructuralHoleOrigin::ExplicitWildcard {
                            site: hole_site,
                            arg_idx,
                        },
                        hole_provenance_root,
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
        completion: ConstDefaultCompletion<'db>,
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
        completion: ConstDefaultCompletion<'db>,
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
        completion: ConstDefaultCompletion<'db>,
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
                let lowered = if hir_ty.is_self_ty(db) && trait_self.is_none() {
                    TyId::invalid(db, InvalidCause::Other)
                } else {
                    lower_hir_ty(db, hir_ty, scope, assumptions)
                };
                let lowered = completion
                    .default_type_sites(db, hir_ty, i)
                    .map_or(lowered, |sites| push_provenance(db, lowered, &sites));
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
                            .application_provenance(db)
                            .and_then(|provenance| {
                                let owner = prec.owner?;
                                let param_idx = prec.original_idx?;
                                Some(ConstTyId::structural_hole(
                                    db,
                                    expected
                                        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),
                                    StructuralHoleOrigin::DefaultHoleParam { owner, param_idx },
                                    provenance,
                                ))
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
pub(crate) struct ConstDefaultCompletion<'db> {
    mode: ConstDefaultCompletionMode,
    application_path: Option<PathId<'db>>,
}

impl<'db> ConstDefaultCompletion<'db> {
    pub(crate) fn metadata(application_path: Option<PathId<'db>>) -> Self {
        Self {
            mode: ConstDefaultCompletionMode::MetadataOnly,
            application_path,
        }
    }

    pub(crate) fn evaluate(application_path: Option<PathId<'db>>) -> Self {
        Self {
            mode: ConstDefaultCompletionMode::Evaluate,
            application_path,
        }
    }

    /// Provenance for a hole minted by completing a defaulted `= _` param at
    /// this application: the use-site path lexically, plus the same path as
    /// the instantiation context.
    fn application_provenance(self, db: &'db dyn HirAnalysisDb) -> Option<ProvenanceId<'db>> {
        let path = self.application_path?;
        Some(
            ProvenanceId::root(db, ProvenanceSite::Lex(LexSite::RootPath(path)))
                .push(db, ProvenanceSite::Inst(InstSite::RootPath(path))),
        )
    }

    fn default_type_sites(
        self,
        db: &'db dyn HirAnalysisDb,
        hir_ty: HirTyId<'db>,
        lowered_param_idx: usize,
    ) -> Option<[ProvenanceSite<'db>; 2]> {
        let _ = db;
        let path = self.application_path?;
        Some([
            ProvenanceSite::Inst(InstSite::TypeComponent {
                ty: hir_ty,
                slot: lowered_param_idx,
            }),
            ProvenanceSite::Inst(InstSite::RootPath(path)),
        ])
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
                    self.params.push(TyParamPrecursor::ty_param(
                        self.owner,
                        name,
                        idx,
                        kind,
                        default_hir_ty,
                    ));
                }

                GenericParam::Const(param) => {
                    let name = param.name;
                    let hir_ty = param.ty.to_opt();
                    let default = param.default;

                    self.params.push(TyParamPrecursor::const_ty_param(
                        self.owner, name, idx, hir_ty, default,
                    ))
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
    owner: Option<GenericParamOwner<'db>>,
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
        owner: GenericParamOwner<'db>,
        name: Partial<IdentId<'db>>,
        idx: usize,
        kind: Option<Kind>,
        default_hir_ty: Option<HirTyId<'db>>,
    ) -> Self {
        Self {
            owner: Some(owner),
            name,
            original_idx: idx.into(),
            kind,
            variant: Variant::Normal,
            default_hir_ty,
            default_hir_const: None,
        }
    }

    fn const_ty_param(
        owner: GenericParamOwner<'db>,
        name: Partial<IdentId<'db>>,
        idx: usize,
        ty: Option<HirTyId<'db>>,
        default: Option<ConstGenericArgValue<'db>>,
    ) -> Self {
        Self {
            owner: Some(owner),
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
            owner: None,
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
            owner: None,
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
            owner: None,
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
