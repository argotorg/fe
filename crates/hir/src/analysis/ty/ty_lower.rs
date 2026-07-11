use crate::core::hir_def::{
    Body, CallableDef, ConstGenericArgValue, Expr, GenericArg, GenericArgListId, GenericParam,
    GenericParamOwner, GenericParamView, IdentId, KindBound as HirKindBound, Partial, PathId, Stmt,
    TypeAlias as HirTypeAlias, TypeBound, TypeId as HirTyId, TypeKind as HirTyKind, TypeMode,
    scope_graph::ScopeId,
};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;
use smallvec::smallvec;

use super::{
    adt_def::{AdtDef, AdtRef, instantiate_adt_field_layout, instantiate_adt_field_shape},
    assoc_const::{AssocConstUse, InherentConstUse},
    const_ty::{
        CallableInputLayoutHoleOrigin, ConstBodyLowering, ConstTyData, ConstTyId, EvaluatedConstTy,
        HoleAnchor, HoleId, HoleMinter, LayoutBoundaryIdentity, LayoutHoleArgSite,
        LayoutInstantiationContext, LayoutInstantiationId, LayoutIntroSite, LayoutOccurrencePath,
        LayoutOccurrenceStep, LayoutRootId, LayoutRootIdentity, StructuralHoleOrigin,
    },
    effects::{ResolvedEffectKey, TraitKeySchema},
    fold::{TyFoldable, TyFolder},
    layout_bundle::{
        CallableLayoutBundleInput, CallableLayoutBundleSignature, LayoutBundleComponent,
        LayoutBundleComponentDeclaration, LayoutBundleComponentId, LayoutBundleComponentKey,
        LayoutBundlePath, LayoutBundlePathStep, LayoutBundleSchema, LayoutBundleTransport,
        LayoutEvidencePath, LayoutEvidencePathStep, LayoutPortKey, LayoutRootPort, LayoutViewAlias,
        NonRegularLayoutViewCycle,
    },
    layout_holes::{
        LayoutInstantiation, LayoutRootUse, LayoutViewRecurrence,
        callable_input_layout_bindings_by_origin, classify_layout_view_recurrence,
        collect_unique_app_bound_structural_holes_in_order,
        collect_unique_layout_placeholders_in_order, instantiate_layout_template,
        layout_hole_fallback_ty, layout_root_id, reanchor_template_holes, rewrite_structural_holes,
        structural_hole_id, substitute_layout_holes_by_placeholder,
        substitute_layout_holes_by_placeholder_in,
    },
    provider::{EffectHandleTargetResolution, resolve_effect_handle_target},
    trait_def::{ImplementorId, TraitInstId},
    trait_resolution::{
        PredicateListId,
        constraint::{
            collect_candidate_constraints, collect_constraints, collect_func_decl_constraints,
        },
    },
    ty_def::{InvalidCause, Kind, PrimTy, TyBase, TyData, TyId, TyParam},
    visitor::{TyVisitable, TyVisitor},
};
use crate::analysis::name_resolution::{
    NameDomain, NameResKind, PathRes, PathResErrorKind, resolve_ident_to_bucket, resolve_path,
    resolve_path_with_minter,
};
use crate::analysis::{HirAnalysisDb, ty::binder::Binder};

/// Lowers the given HirTy to `TyId`.
#[salsa::tracked(cycle_fn=lower_hir_ty_cycle_recover, cycle_initial=lower_hir_ty_cycle_initial)]
pub fn lower_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let minter = HoleMinter::new(HoleAnchor::TemplateTy {
        ty,
        scope,
        assumptions,
    });
    lower_hir_ty_impl(db, ty, scope, assumptions, &minter)
}

fn lower_hir_ty_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    minter: &HoleMinter<'db>,
) -> TyId<'db> {
    let lower_child =
        |child_ty, _slot| lower_opt_hir_ty_impl(db, child_ty, scope, assumptions, minter);

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

        HirTyKind::Path(path) => lower_path_impl(db, scope, *path, assumptions, minter),

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
            let len_ty = lower_opt_const_body(db, *len, scope, assumptions, minter);
            let len_ty = TyId::const_ty(db, len_ty);
            let array = TyId::array(db, elem_ty);
            TyId::app(db, array, len_ty)
        }

        HirTyKind::Never => TyId::never(db),
    }
}

/// Lowers `ty` minting structural-hole identities through the caller's
/// minter, so holes are keyed to the enclosing lowering execution instead of
/// this type's content-interned identity. Use this instead of the memoized
/// [`lower_hir_ty`] whenever the result flows into a larger type under
/// construction: a memoized result reused at two positions carries the same
/// hole identities at both.
pub(crate) fn lower_hir_ty_with_minter<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    minter: &HoleMinter<'db>,
) -> TyId<'db> {
    lower_hir_ty_impl(db, ty, scope, assumptions, minter)
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
    let minter = HoleMinter::deferred(HoleAnchor::TemplateTy {
        ty,
        scope,
        assumptions,
    });
    lower_hir_ty_impl(db, ty, scope, assumptions, &minter)
}

pub(crate) fn lower_opt_hir_ty_with_minter<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Partial<HirTyId<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    minter: &HoleMinter<'db>,
) -> TyId<'db> {
    lower_opt_hir_ty_impl(db, ty, scope, assumptions, minter)
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
    // Anchor at the same memo key the tracked entry would use, so holes get
    // the same identity whether or not this lowering is memoized.
    let minter = HoleMinter::new(HoleAnchor::TemplateTy {
        ty: hir_ty,
        scope,
        assumptions,
    });
    lower_hir_ty_impl(db, hir_ty, scope, assumptions, &minter)
}

pub(crate) fn lower_layout_root_uses_in_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    minter: &HoleMinter<'db>,
) -> Vec<super::layout_holes::LayoutRootUse<'db>> {
    let mut uses = Vec::new();
    collect_layout_root_uses_in_hir_ty(db, ty, scope, assumptions, minter, &mut uses);
    uses
}

fn collect_layout_root_uses_in_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    minter: &HoleMinter<'db>,
    uses: &mut Vec<super::layout_holes::LayoutRootUse<'db>>,
) {
    match ty.data(db) {
        HirTyKind::Ptr(pointee) | HirTyKind::Mode(_, pointee) => {
            if let Some(pointee) = pointee.to_opt() {
                collect_layout_root_uses_in_hir_ty(db, pointee, scope, assumptions, minter, uses);
            }
        }
        HirTyKind::Tuple(tuple) => {
            for elem in tuple.data(db).iter().filter_map(|elem| elem.to_opt()) {
                collect_layout_root_uses_in_hir_ty(db, elem, scope, assumptions, minter, uses);
            }
        }
        HirTyKind::Array(element, _) => {
            if let Some(element) = element.to_opt() {
                collect_layout_root_uses_in_hir_ty(db, element, scope, assumptions, minter, uses);
            }
        }
        HirTyKind::Path(path) => {
            let Some(path) = path.to_opt() else {
                return;
            };
            let args = lower_generic_arg_list(
                db,
                path.generic_args(db),
                scope,
                assumptions,
                LayoutHoleArgSite::Path(path),
                minter,
            );
            if let Ok(PathRes::TyAlias(alias, _)) =
                resolve_path_with_minter(db, path, scope, assumptions, false, minter)
            {
                let instantiated =
                    alias.instantiate_layout_from_path(db, path, &args, assumptions, minter);
                uses.extend(
                    instantiated
                        .root_uses
                        .into_iter()
                        .filter(|root_use| root_use.root(db).is_none()),
                );
            }
            for arg in path.generic_args(db).data(db) {
                if let GenericArg::Type(arg) = arg
                    && let Some(arg) = arg.ty.to_opt()
                {
                    collect_layout_root_uses_in_hir_ty(db, arg, scope, assumptions, minter, uses);
                }
            }
        }
        HirTyKind::Never => {}
    }
}

fn lower_opt_hir_ty_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Partial<HirTyId<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    minter: &HoleMinter<'db>,
) -> TyId<'db> {
    ty.to_opt()
        .map(|hir_ty| lower_hir_ty_impl(db, hir_ty, scope, assumptions, minter))
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
    minter: &HoleMinter<'db>,
) -> ConstTyId<'db> {
    let Some(body) = body.to_opt() else {
        return ConstTyId::invalid(db, InvalidCause::ParseError);
    };
    if minter.const_bodies() == ConstBodyLowering::Deferred {
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
                resolve_path_with_minter(db, path, scope, assumptions, true, minter)
            && let TyData::ConstTy(const_ty) = ty.data(db)
        {
            return *const_ty;
        }
        return ConstTyId::from_opt_body_deferred(db, Partial::Present(body), None, Vec::new());
    }
    let Some(path) = const_body_simple_path(db, body) else {
        return ConstTyId::from_body(db, body, None, None);
    };

    let assumptions = with_enclosing_trait_self_predicate(db, scope, assumptions);
    match resolve_path_with_minter(db, path, scope, assumptions, true, minter) {
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
    minter: &HoleMinter<'db>,
) -> TyId<'db> {
    let Some(path) = path.to_opt() else {
        return TyId::invalid(db, InvalidCause::ParseError);
    };

    match resolve_path_with_minter(db, path, scope, assumptions, false, minter) {
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty) | PathRes::Func(ty)) => ty,
        Ok(res) => TyId::invalid(db, InvalidCause::NotAType(res)),
        Err(err) => {
            // Try to resolve as a value, to find a matching `const` definition
            if matches!(err.kind, PathResErrorKind::NotFound { .. })
                && let Ok(resolved) =
                    resolve_path_with_minter(db, path, scope, assumptions, true, minter)
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
                    PathRes::InherentConst(recv_ty, impl_, name) => {
                        if let Some(expected_ty) =
                            super::const_ty::inherent_const_expected_ty(db, impl_, recv_ty, name)
                        {
                            let use_ =
                                InherentConstUse::new(scope, assumptions, impl_, recv_ty, name);
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
    // On cycles during type lowering, treat the type as invalid. The cause
    // renders a diagnostic at the use site: cyclic shapes are normally
    // rejected by dedicated checks first (alias cycles, recursive types,
    // cyclic trait bounds), so any cycle that converges to this value is a
    // shape those checks missed and must not be silently invalid.
    TyId::invalid(db, InvalidCause::TypeLoweringCycle)
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
    let Some(p) = path.to_opt() else {
        return TyId::invalid(db, InvalidCause::ParseError);
    };
    let minter = HoleMinter::new(HoleAnchor::TemplatePath {
        path: p,
        scope,
        assumptions,
    });
    lower_path_impl(db, scope, path, assumptions, &minter)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct CallableInputLayoutBackingSource {
    pub origin: CallableInputLayoutHoleOrigin,
    pub projection: LayoutBundlePath,
}

#[derive(Default)]
struct CallableLayoutProjections<'db> {
    placeholders: Vec<TyId<'db>>,
    component_placeholders: Vec<Vec<TyId<'db>>>,
    /// Nonterminal declaration ports replaced by each terminal component.
    /// This preserves specialization topology without transporting both an
    /// aggregate carrier alias and its physical descendant.
    component_refined_ports: Vec<Vec<LayoutPortKey>>,
    schema: LayoutBundleSchema<'db>,
    tys: FxHashMap<LayoutBundlePath, TyId<'db>>,
    paths: Vec<LayoutBundlePath>,
    index_lengths: FxHashMap<LayoutBundlePath, Vec<usize>>,
    port_tys: FxHashMap<LayoutPortKey, TyId<'db>>,
}

struct CallableLayoutProjectionCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    site: CallableLayoutSchemaSite<'db>,
    next_ordinal: usize,
    placeholders: Vec<TyId<'db>>,
    seen_placeholders: FxHashSet<TyId<'db>>,
    transport_roots: FxHashMap<TyId<'db>, LayoutRootId<'db>>,
    placeholder_roots: FxHashMap<TyId<'db>, LayoutRootId<'db>>,
    transport_declarations: FxHashMap<TyId<'db>, LayoutBundleComponentDeclaration<'db>>,
    tys: FxHashMap<LayoutBundlePath, TyId<'db>>,
    paths: Vec<LayoutBundlePath>,
    index_lengths: FxHashMap<LayoutBundlePath, Vec<usize>>,
    value_occurrences: Vec<CallableLayoutOccurrence<'db>>,
    port_tys: FxHashMap<LayoutPortKey, TyId<'db>>,
    adt_stack: Vec<CallableLayoutAdtFrame<'db>>,
    view_aliases: Vec<LayoutViewAlias>,
    non_regular_view_cycle: Option<NonRegularLayoutViewCycle>,
    expand_effect_targets: bool,
    bound_roots: FxHashMap<LayoutRootId<'db>, TyId<'db>>,
}

struct CallableLayoutAdtFrame<'db> {
    ty: TyId<'db>,
    family: CallableLayoutExpansionFamily<'db>,
    evidence_path: LayoutEvidencePath,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum CallableLayoutExpansionFamily<'db> {
    Adt(AdtDef<'db>),
    Provider(ImplementorId<'db>),
}

#[derive(Clone)]
struct CallableLayoutOccurrence<'db> {
    representative: LayoutBundleComponentKey<'db>,
    declaration: LayoutBundleComponentDeclaration<'db>,
    ty: TyId<'db>,
    port: LayoutPortKey,
    dimensions: Vec<usize>,
    structural_root: Option<LayoutRootId<'db>>,
    can_refine_to_descendant: bool,
}

fn layout_root_descends_from<'db>(
    db: &'db dyn HirAnalysisDb,
    mut root: LayoutRootId<'db>,
    ancestor: LayoutRootId<'db>,
) -> bool {
    loop {
        if root == ancestor {
            return true;
        }
        match root.identity(db) {
            LayoutRootIdentity::Source { .. } => return false,
            LayoutRootIdentity::Landing { source, .. } => root = source,
        }
    }
}

fn callable_layout_occurrence_descends_from<'db>(
    db: &'db dyn HirAnalysisDb,
    candidate: &CallableLayoutOccurrence<'db>,
    ancestor: &CallableLayoutOccurrence<'db>,
) -> bool {
    ancestor.can_refine_to_descendant
        && ancestor.declaration == candidate.declaration
        && ancestor.ty == candidate.ty
        && candidate.dimensions.starts_with(&ancestor.dimensions)
        && ancestor.port.value_path.len() < candidate.port.value_path.len()
        && candidate
            .port
            .value_path
            .starts_with(&ancestor.port.value_path)
        && !candidate.port.value_path[ancestor.port.value_path.len()..]
            .contains(&LayoutEvidencePathStep::EffectTarget)
        && match (ancestor.structural_root, candidate.structural_root) {
            (Some(ancestor), Some(root)) => layout_root_descends_from(db, root, ancestor),
            (None, None) => ancestor.representative == candidate.representative,
            (Some(_), None) | (None, Some(_)) => false,
        }
}

#[derive(Clone, Copy)]
enum CallableLayoutSchemaSite<'db> {
    Input {
        func: crate::hir_def::Func<'db>,
        origin: CallableInputLayoutHoleOrigin,
    },
    Output {
        func: crate::hir_def::Func<'db>,
    },
    Value {
        body: crate::hir_def::Body<'db>,
        local: u32,
    },
}

impl<'db> CallableLayoutSchemaSite<'db> {
    fn func(self, db: &'db dyn HirAnalysisDb) -> Option<crate::hir_def::Func<'db>> {
        match self {
            Self::Input { func, .. } | Self::Output { func } => Some(func),
            Self::Value { body, .. } => body.containing_func(db),
        }
    }

    /// Associated functions clone their parent generic parameters into the
    /// function's lowered parameter list. Canonicalize those clones back to
    /// the parent declaration so signature and body schemas share one stable
    /// layout declaration identity.
    fn canonical_param(self, db: &'db dyn HirAnalysisDb, value: TyId<'db>) -> TyId<'db> {
        let TyData::ConstTy(const_ty) = value.data(db) else {
            return value;
        };
        let ConstTyData::TyParam(param, _) = const_ty.data(db) else {
            return value;
        };
        let Some((_, parent)) = self
            .func(db)
            .filter(|func| {
                func.is_associated_func(db)
                    && param.owner == func.scope()
                    && param.idx < func_inherited_param_precursors(db, *func).len()
            })
            .and_then(|func| {
                GenericParamOwner::Func(func)
                    .parent(db)
                    .map(|parent| (func, parent))
            })
        else {
            return value;
        };
        collect_generic_params(db, parent)
            .params(db)
            .get(param.idx)
            .copied()
            .unwrap_or(value)
    }

    fn scope(self) -> ScopeId<'db> {
        match self {
            Self::Input { func, .. } | Self::Output { func } => func.scope(),
            Self::Value { body, .. } => body.scope(),
        }
    }

    fn assumptions(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        generic_param_owner_assumptions(db, self.scope())
    }
}

/// Canonicalizes only the landing chain that preserves one indexed physical
/// transport. Crossing any other boundary can fan one source out into sibling
/// roots, so those landings must remain distinct callable parameters.
fn callable_indexed_transport_root<'db>(
    db: &'db dyn HirAnalysisDb,
    mut root: LayoutRootId<'db>,
) -> LayoutRootId<'db> {
    loop {
        match root.identity(db) {
            LayoutRootIdentity::Landing { source, instance }
                if instance.boundary(db) == LayoutBoundaryIdentity::ArrayElement =>
            {
                root = source;
            }
            LayoutRootIdentity::Source { .. } | LayoutRootIdentity::Landing { .. } => return root,
        }
    }
}

/// Returns const parameters whose value is physically forwarded into a field.
///
/// This declaration-level proof is intentionally separate from instantiated
/// root equality: distinct formal parameters may specialize to the same root,
/// but only the parameter named by a field may be refined to that descendant.
fn forwarded_layout_params<'db>(db: &'db dyn HirAnalysisDb, adt: AdtDef<'db>) -> FxHashSet<usize> {
    fn collect<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        candidates: &FxHashMap<TyId<'db>, usize>,
        stack: &mut Vec<AdtDef<'db>>,
        forwarded: &mut FxHashSet<usize>,
    ) {
        let ty = ty.as_capability(db).map_or(ty, |(_, inner)| inner);
        if let Some(candidate) = candidates.get(&ty) {
            forwarded.insert(*candidate);
            return;
        }
        if ty.is_tuple(db) {
            for field in ty.field_types(db) {
                collect(db, field, candidates, stack, forwarded);
            }
            return;
        }
        if ty.is_array(db) {
            if ty.array_len(db).is_some_and(|len| len > 0)
                && let Some(element) = ty.generic_args(db).first()
            {
                collect(db, *element, candidates, stack, forwarded);
            }
            return;
        }
        let Some(adt) = ty.adt_def(db) else {
            return;
        };
        let args = ty.generic_args(db);
        for arg in args.iter().copied() {
            collect(db, arg, candidates, stack, forwarded);
        }
        if stack.contains(&adt) {
            return;
        }
        stack.push(adt);
        for (variant_idx, variant) in adt.fields(db).iter().enumerate() {
            for field_idx in 0..variant.num_types() {
                collect(
                    db,
                    instantiate_adt_field_shape(db, adt, variant_idx, field_idx, args),
                    candidates,
                    stack,
                    forwarded,
                );
            }
        }
        stack.pop();
    }

    let params = adt.params(db);
    let candidates = params
        .iter()
        .copied()
        .enumerate()
        .filter(|(_, param)| matches!(param.data(db), TyData::ConstTy(_)))
        .map(|(idx, param)| (param, idx))
        .collect::<FxHashMap<_, _>>();
    let mut forwarded = FxHashSet::default();
    let mut stack = vec![adt];
    for (variant_idx, variant) in adt.fields(db).iter().enumerate() {
        for field_idx in 0..variant.num_types() {
            collect(
                db,
                instantiate_adt_field_shape(db, adt, variant_idx, field_idx, params),
                &candidates,
                &mut stack,
                &mut forwarded,
            );
        }
    }
    forwarded
}

impl<'db> CallableLayoutProjectionCollector<'db> {
    fn record_ty(
        &mut self,
        path: &LayoutBundlePath,
        ty: TyId<'db>,
        index_lengths: &[usize],
    ) -> TyId<'db> {
        let ty = self.bind_ty(ty);
        if self.tys.insert(path.clone(), ty).is_none() {
            self.paths.push(path.clone());
        }
        self.index_lengths
            .insert(path.clone(), index_lengths.to_vec());
        ty
    }

    fn collect_placeholder(&mut self, placeholder: TyId<'db>) {
        if self.seen_placeholders.insert(placeholder) {
            self.placeholders.push(placeholder);
        }
    }

    fn bind_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
        let db = self.db;
        let site = self.site;
        let bound_roots = &mut self.bound_roots;
        let next_ordinal = &mut self.next_ordinal;
        let mut created = Vec::new();
        let ty = rewrite_structural_holes(db, ty, |hole, hole_ty| {
            let root = hole.root(db);
            if let Some(placeholder) = bound_roots.get(&root) {
                return Some(*placeholder);
            }
            let ordinal = *next_ordinal;
            *next_ordinal += 1;
            let placeholder = match site {
                CallableLayoutSchemaSite::Input { func, origin } => TyId::const_ty(
                    db,
                    ConstTyId::bound_callable_hole(
                        db,
                        layout_hole_fallback_ty(db, hole_ty),
                        func,
                        origin,
                        ordinal,
                    ),
                ),
                CallableLayoutSchemaSite::Output { .. }
                | CallableLayoutSchemaSite::Value { .. } => TyId::const_ty(
                    db,
                    ConstTyId::hole_with_id(db, hole_ty, HoleId::Structural(hole)),
                ),
            };
            bound_roots.insert(root, placeholder);
            created.push((
                placeholder,
                callable_indexed_transport_root(db, root),
                root,
                LayoutBundleComponentDeclaration::Structural {
                    origin: hole.origin(db),
                    introduced_at: hole.introduced_at(db).clone(),
                },
            ));
            Some(placeholder)
        });
        for (placeholder, transport_root, root, declaration) in created {
            self.collect_placeholder(placeholder);
            self.transport_roots.insert(placeholder, transport_root);
            self.placeholder_roots.insert(placeholder, root);
            self.transport_declarations.insert(placeholder, declaration);
        }
        ty
    }

    fn walk(
        &mut self,
        ty: TyId<'db>,
        parent: LayoutInstantiationId<'db>,
        path: &mut LayoutBundlePath,
        evidence_path: &mut LayoutEvidencePath,
        index_lengths: &mut Vec<usize>,
    ) {
        self.bind_ty(ty);
        let ty = ty.as_capability(self.db).map_or(ty, |(_, inner)| inner);
        if ty.is_tuple(self.db) {
            for (idx, field) in ty.field_types(self.db).into_iter().enumerate() {
                let Ok(idx) = u16::try_from(idx) else {
                    continue;
                };
                path.push(LayoutBundlePathStep::Field(idx));
                evidence_path.push(LayoutEvidencePathStep::Field(idx));
                self.record_ty(path, field, index_lengths);
                self.walk(field, parent, path, evidence_path, index_lengths);
                evidence_path.pop();
                path.pop();
            }
            return;
        }
        if ty.is_array(self.db) {
            if let (Some(len), Some(&element)) =
                (ty.array_len(self.db), ty.generic_args(self.db).first())
            {
                if len == 0 {
                    return;
                }
                let element = instantiate_layout_template(
                    self.db,
                    element,
                    &[],
                    &[],
                    LayoutInstantiationContext::Nested(parent),
                    LayoutBoundaryIdentity::ArrayElement,
                    vec![LayoutOccurrenceStep::ArrayDimension(
                        path.iter()
                            .filter(|step| matches!(step, LayoutBundlePathStep::Index))
                            .count() as u32,
                    )],
                );
                path.push(LayoutBundlePathStep::Index);
                evidence_path.push(LayoutEvidencePathStep::Index);
                index_lengths.push(len);
                self.record_ty(path, element.ty, index_lengths);
                self.walk(
                    element.ty,
                    element.instance,
                    path,
                    evidence_path,
                    index_lengths,
                );
                index_lengths.pop();
                evidence_path.pop();
                path.pop();
            }
            return;
        }
        let Some(adt) = ty.adt_def(self.db) else {
            return;
        };
        let effect_target = self.expand_effect_targets.then(|| {
            resolve_effect_handle_target(
                self.db,
                self.site.scope(),
                self.site.assumptions(self.db),
                ty,
            )
        });
        let family = match &effect_target {
            Some(EffectHandleTargetResolution::Resolved { impl_instance, .. }) => {
                CallableLayoutExpansionFamily::Provider(impl_instance.selected())
            }
            Some(
                EffectHandleTargetResolution::NotHandle
                | EffectHandleTargetResolution::Ambiguous
                | EffectHandleTargetResolution::UnresolvedTarget,
            )
            | None => CallableLayoutExpansionFamily::Adt(adt),
        };
        match classify_layout_view_recurrence(
            self.db,
            ty,
            family,
            self.adt_stack
                .iter()
                .enumerate()
                .rev()
                .map(|(idx, frame)| (idx, frame.ty, frame.family)),
        ) {
            LayoutViewRecurrence::BackEdge { ancestor } => {
                let canonical = self.adt_stack[ancestor].evidence_path.clone();
                let alias = LayoutViewAlias {
                    alias: evidence_path.clone(),
                    canonical,
                };
                if alias.alias != alias.canonical && !self.view_aliases.contains(&alias) {
                    self.view_aliases.push(alias);
                }
                return;
            }
            LayoutViewRecurrence::NonRegular { ancestor } => {
                let frame = &self.adt_stack[ancestor];
                self.non_regular_view_cycle
                    .get_or_insert_with(|| NonRegularLayoutViewCycle {
                        canonical: frame.evidence_path.clone(),
                        recursive: evidence_path.clone(),
                    });
                return;
            }
            LayoutViewRecurrence::Expand => {}
        }
        self.adt_stack.push(CallableLayoutAdtFrame {
            ty,
            family,
            evidence_path: evidence_path.clone(),
        });
        let args = ty.generic_args(self.db);
        let forwarded_params = forwarded_layout_params(self.db, adt);
        for (param_idx, arg) in args.iter().copied().enumerate() {
            if !matches!(arg.data(self.db), TyData::ConstTy(_)) {
                continue;
            }
            let Ok(param_step) = u16::try_from(param_idx) else {
                continue;
            };
            path.push(LayoutBundlePathStep::ConstParam(param_step));
            let declared_layout_param = adt
                .param_set(self.db)
                .const_param_default_is_slot_layout_hole(self.db, param_idx);
            let bound_arg = self.record_ty(path, arg, index_lengths);
            let placeholders = collect_unique_layout_placeholders_in_order(self.db, bound_arg);
            if placeholders.is_empty() {
                self.port_tys
                    .entry(LayoutPortKey {
                        value_path: evidence_path.clone(),
                        root: LayoutRootPort {
                            param: param_idx,
                            ordinal: 0,
                        },
                    })
                    .or_insert(bound_arg);
            }
            for (ordinal, placeholder) in placeholders.iter().copied().enumerate() {
                let Some(root) = self.transport_roots.get(&placeholder) else {
                    continue;
                };
                let Some(declaration) = self.transport_declarations.get(&placeholder) else {
                    continue;
                };
                let Some(structural_root) = self.placeholder_roots.get(&placeholder) else {
                    continue;
                };
                let TyData::ConstTy(const_ty) = placeholder.data(self.db) else {
                    continue;
                };
                let ConstTyData::Hole(ty, _) = const_ty.data(self.db) else {
                    continue;
                };
                self.value_occurrences.push(CallableLayoutOccurrence {
                    representative: LayoutBundleComponentKey::Root(*root),
                    declaration: declaration.clone(),
                    ty: layout_hole_fallback_ty(self.db, *ty),
                    port: LayoutPortKey {
                        value_path: evidence_path.clone(),
                        root: LayoutRootPort {
                            param: param_idx,
                            ordinal,
                        },
                    },
                    dimensions: index_lengths.clone(),
                    structural_root: Some(*structural_root),
                    can_refine_to_descendant: forwarded_params.contains(&param_idx),
                });
                self.port_tys.insert(
                    LayoutPortKey {
                        value_path: evidence_path.clone(),
                        root: LayoutRootPort {
                            param: param_idx,
                            ordinal,
                        },
                    },
                    bound_arg,
                );
            }
            if declared_layout_param
                && placeholders.is_empty()
                && let TyData::ConstTy(const_ty) = arg.data(self.db)
            {
                let (key, ty) = match const_ty.data(self.db) {
                    ConstTyData::Hole(_, HoleId::Structural(_)) => (None, None),
                    ConstTyData::Hole(ty, _)
                    | ConstTyData::TyParam(_, ty)
                    | ConstTyData::TyVar(_, ty)
                    | ConstTyData::Abstract(_, ty) => (
                        Some(LayoutBundleComponentKey::Param(arg)),
                        Some(layout_hole_fallback_ty(self.db, *ty)),
                    ),
                    ConstTyData::Evaluated(_, ty) => (
                        Some(if arg.has_param(self.db) || arg.has_var(self.db) {
                            LayoutBundleComponentKey::Param(arg)
                        } else {
                            LayoutBundleComponentKey::Static(arg)
                        }),
                        Some(layout_hole_fallback_ty(self.db, *ty)),
                    ),
                    ConstTyData::UnEvaluated { ty: Some(ty), .. } => (
                        Some(if arg.has_param(self.db) || arg.has_var(self.db) {
                            LayoutBundleComponentKey::Param(arg)
                        } else {
                            LayoutBundleComponentKey::Static(arg)
                        }),
                        Some(layout_hole_fallback_ty(self.db, *ty)),
                    ),
                    ConstTyData::UnEvaluated { ty: None, .. } => (None, None),
                };
                if let Some((key, ty)) = key.zip(ty) {
                    let declaration = match key {
                        LayoutBundleComponentKey::Root(_) => unreachable!(
                            "direct layout arguments cannot contain an unbound structural root"
                        ),
                        LayoutBundleComponentKey::Param(value) => {
                            LayoutBundleComponentDeclaration::Param(
                                self.site.canonical_param(self.db, value),
                            )
                        }
                        LayoutBundleComponentKey::Static(value) => {
                            LayoutBundleComponentDeclaration::Static(value)
                        }
                    };
                    self.value_occurrences.push(CallableLayoutOccurrence {
                        representative: key,
                        declaration,
                        ty,
                        port: LayoutPortKey {
                            value_path: evidence_path.clone(),
                            root: LayoutRootPort {
                                param: param_idx,
                                ordinal: placeholders.len(),
                            },
                        },
                        dimensions: index_lengths.clone(),
                        structural_root: None,
                        can_refine_to_descendant: forwarded_params.contains(&param_idx),
                    });
                    self.port_tys.insert(
                        LayoutPortKey {
                            value_path: evidence_path.clone(),
                            root: LayoutRootPort {
                                param: param_idx,
                                ordinal: placeholders.len(),
                            },
                        },
                        bound_arg,
                    );
                }
            }
            path.pop();
        }
        match adt.adt_ref(self.db) {
            AdtRef::Struct(_) => {
                for field_idx in 0..adt.fields(self.db)[0].num_types() {
                    let Ok(field_idx) = u16::try_from(field_idx) else {
                        continue;
                    };
                    let field = instantiate_adt_field_layout(
                        self.db,
                        adt,
                        0,
                        field_idx as usize,
                        args,
                        parent,
                        vec![LayoutOccurrenceStep::StructField(field_idx as u32)],
                    );
                    path.push(LayoutBundlePathStep::Field(field_idx));
                    evidence_path.push(LayoutEvidencePathStep::Field(field_idx));
                    self.record_ty(path, field.ty, index_lengths);
                    self.walk(field.ty, field.instance, path, evidence_path, index_lengths);
                    evidence_path.pop();
                    path.pop();
                }
            }
            AdtRef::Enum(_) => {
                for (variant_idx, variant) in adt.fields(self.db).iter().enumerate() {
                    let Ok(variant_idx) = u16::try_from(variant_idx) else {
                        continue;
                    };
                    path.push(LayoutBundlePathStep::Variant(variant_idx));
                    evidence_path.push(LayoutEvidencePathStep::Variant(variant_idx));
                    for field_idx in 0..variant.num_types() {
                        let Ok(field_idx) = u16::try_from(field_idx) else {
                            continue;
                        };
                        let field = instantiate_adt_field_layout(
                            self.db,
                            adt,
                            variant_idx as usize,
                            field_idx as usize,
                            args,
                            parent,
                            vec![
                                LayoutOccurrenceStep::EnumVariant(variant_idx as u32),
                                LayoutOccurrenceStep::EnumPayloadField(field_idx as u32),
                            ],
                        );
                        path.push(LayoutBundlePathStep::Field(field_idx));
                        evidence_path.push(LayoutEvidencePathStep::Field(field_idx));
                        self.record_ty(path, field.ty, index_lengths);
                        self.walk(field.ty, field.instance, path, evidence_path, index_lengths);
                        evidence_path.pop();
                        path.pop();
                    }
                    evidence_path.pop();
                    path.pop();
                }
            }
        }
        if let Some(EffectHandleTargetResolution::Resolved {
            impl_instance,
            target_ty,
            ..
        }) = effect_target
        {
            let target = instantiate_layout_template(
                self.db,
                target_ty,
                &[],
                &[],
                LayoutInstantiationContext::Nested(parent),
                LayoutBoundaryIdentity::ProviderTarget(impl_instance.selected()),
                vec![LayoutOccurrenceStep::Normalization],
            );
            evidence_path.push(LayoutEvidencePathStep::EffectTarget);
            self.walk(
                target.ty,
                target.instance,
                path,
                evidence_path,
                index_lengths,
            );
            evidence_path.pop();
        }
        self.adt_stack.pop();
    }

    fn finish(self) -> CallableLayoutProjections<'db> {
        let value_occurrences = self
            .value_occurrences
            .iter()
            .enumerate()
            .filter(|(idx, occurrence)| {
                !self
                    .value_occurrences
                    .iter()
                    .enumerate()
                    .any(|(candidate_idx, candidate)| {
                        idx != &candidate_idx
                            && callable_layout_occurrence_descends_from(
                                self.db, candidate, occurrence,
                            )
                    })
            })
            .map(|(_, occurrence)| occurrence.clone())
            .collect::<Vec<_>>();
        let mut all_components = Vec::<(
            LayoutBundleComponent<'db>,
            Vec<TyId<'db>>,
            Vec<LayoutPortKey>,
        )>::new();
        for occurrence in value_occurrences {
            let placeholders = match &occurrence.representative {
                LayoutBundleComponentKey::Root(root) => self
                    .placeholders
                    .iter()
                    .copied()
                    .filter(|placeholder| self.transport_roots.get(placeholder) == Some(root))
                    .collect(),
                LayoutBundleComponentKey::Param(_) | LayoutBundleComponentKey::Static(_) => {
                    Vec::new()
                }
            };
            let refined_ports = self
                .value_occurrences
                .iter()
                .filter(|ancestor| {
                    callable_layout_occurrence_descends_from(self.db, &occurrence, ancestor)
                })
                .map(|ancestor| ancestor.port.clone())
                .collect::<Vec<_>>();
            if let Some((component, existing_placeholders, existing_refined_ports)) = all_components
                .iter_mut()
                .find(|(component, _, _)| component.port == occurrence.port)
            {
                assert_eq!(
                    component.representative,
                    Some(occurrence.representative),
                    "one layout port has multiple root values"
                );
                assert_eq!(
                    component.declaration, occurrence.declaration,
                    "one layout port has multiple root declarations"
                );
                assert_eq!(
                    component.ty, occurrence.ty,
                    "one layout port has multiple scalar types"
                );
                assert_eq!(
                    component.dimensions, occurrence.dimensions,
                    "one layout port has multiple dimension shapes"
                );
                for placeholder in placeholders {
                    if !existing_placeholders.contains(&placeholder) {
                        existing_placeholders.push(placeholder);
                    }
                }
                for port in refined_ports {
                    if !existing_refined_ports.contains(&port) {
                        existing_refined_ports.push(port);
                    }
                }
            } else {
                all_components.push((
                    LayoutBundleComponent {
                        id: LayoutBundleComponentId(all_components.len() as u32),
                        port: occurrence.port,
                        declaration: occurrence.declaration,
                        transport: if matches!(
                            occurrence.representative,
                            LayoutBundleComponentKey::Static(_)
                        ) {
                            LayoutBundleTransport::CompileTime
                        } else {
                            LayoutBundleTransport::Runtime
                        },
                        representative: Some(occurrence.representative),
                        ty: occurrence.ty,
                        supplied_const_params: Vec::new(),
                        dependent_const_params: Vec::new(),
                        dimensions: occurrence.dimensions,
                    },
                    placeholders,
                    refined_ports,
                ));
            }
        }

        for (idx, (component, _, _)) in all_components.iter_mut().enumerate() {
            component.id = LayoutBundleComponentId(idx as u32);
        }
        let mut components = Vec::with_capacity(all_components.len());
        let mut component_placeholders = Vec::with_capacity(all_components.len());
        let mut component_refined_ports = Vec::with_capacity(all_components.len());
        for (component, placeholders, refined_ports) in all_components {
            components.push(component);
            component_placeholders.push(placeholders);
            component_refined_ports.push(refined_ports);
        }

        let schema = LayoutBundleSchema {
            components,
            view_aliases: self.view_aliases,
            non_regular_view_cycle: self.non_regular_view_cycle,
        };
        debug_assert!(
            schema.non_regular_view_cycle.is_some() || schema.validate().is_ok(),
            "callable layout collector produced an invalid schema: {schema:#?}",
        );
        CallableLayoutProjections {
            placeholders: self.placeholders,
            component_placeholders,
            component_refined_ports,
            schema,
            tys: self.tys,
            paths: self.paths,
            index_lengths: self.index_lengths,
            port_tys: self.port_tys,
        }
    }
}

fn callable_layout_projections_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    site: CallableLayoutSchemaSite<'db>,
    ty: TyId<'db>,
) -> CallableLayoutProjections<'db> {
    callable_layout_projections_for_ty_with_effect_targets(db, site, ty, true)
}

fn callable_layout_projections_for_ty_with_effect_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    site: CallableLayoutSchemaSite<'db>,
    ty: TyId<'db>,
    expand_effect_targets: bool,
) -> CallableLayoutProjections<'db> {
    let (anchor, boundary) = match site {
        CallableLayoutSchemaSite::Input { func, origin } => (
            HoleAnchor::CallableInput { func, origin },
            LayoutBoundaryIdentity::CallableInput { func, origin },
        ),
        CallableLayoutSchemaSite::Output { func } => (
            HoleAnchor::CallableOutput { func },
            LayoutBoundaryIdentity::CallableOutput(func),
        ),
        CallableLayoutSchemaSite::Value { body, local } => (
            HoleAnchor::SemanticValue { body, local },
            LayoutBoundaryIdentity::SemanticValue { body, local },
        ),
    };
    let root = if matches!(site, CallableLayoutSchemaSite::Value { .. }) {
        LayoutInstantiation {
            ty,
            root_uses: Vec::new(),
            instance: LayoutInstantiationId::new(
                db,
                LayoutInstantiationContext::Lowering(anchor),
                boundary,
                vec![LayoutOccurrenceStep::Instantiation(0)],
            ),
        }
    } else {
        instantiate_layout_template(
            db,
            ty,
            &[],
            &[],
            LayoutInstantiationContext::Lowering(anchor),
            boundary,
            vec![LayoutOccurrenceStep::Instantiation(0)],
        )
    };
    let mut collector = CallableLayoutProjectionCollector {
        db,
        site,
        next_ordinal: 0,
        placeholders: Vec::new(),
        seen_placeholders: FxHashSet::default(),
        transport_roots: FxHashMap::default(),
        placeholder_roots: FxHashMap::default(),
        transport_declarations: FxHashMap::default(),
        tys: FxHashMap::default(),
        paths: Vec::new(),
        index_lengths: FxHashMap::default(),
        value_occurrences: Vec::new(),
        port_tys: FxHashMap::default(),
        adt_stack: Vec::new(),
        view_aliases: Vec::new(),
        non_regular_view_cycle: None,
        expand_effect_targets,
        bound_roots: FxHashMap::default(),
    };
    collector.walk(
        root.ty,
        root.instance,
        &mut Vec::new(),
        &mut Vec::new(),
        &mut Vec::new(),
    );
    collector.finish()
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
    let mut ordinals = FxHashMap::default();
    for hole in collect_unique_app_bound_structural_holes_in_order(db, value) {
        let next = ordinals.len();
        ordinals.entry(hole.root(db)).or_insert(next);
    }

    rewrite_structural_holes(db, value, |hole_id, hole_ty| {
        ordinals.get(&hole_id.root(db)).map(|ordinal| {
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
    layout_bundle_schemas_by_origin:
        FxHashMap<CallableInputLayoutHoleOrigin, LayoutBundleSchema<'db>>,
    layout_projected_tys_by_origin:
        FxHashMap<CallableInputLayoutHoleOrigin, FxHashMap<LayoutBundlePath, TyId<'db>>>,
    layout_port_tys_by_origin:
        FxHashMap<CallableInputLayoutHoleOrigin, FxHashMap<LayoutPortKey, TyId<'db>>>,
    carrier_projected_tys_by_origin:
        FxHashMap<CallableInputLayoutHoleOrigin, FxHashMap<LayoutBundlePath, TyId<'db>>>,
    projected_paths_by_origin: FxHashMap<CallableInputLayoutHoleOrigin, Vec<LayoutBundlePath>>,
    projected_index_lengths_by_origin:
        FxHashMap<CallableInputLayoutHoleOrigin, FxHashMap<LayoutBundlePath, Vec<usize>>>,
}

fn callable_input_layout_types<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> Vec<(CallableInputLayoutHoleOrigin, TyId<'db>)> {
    let assumptions = collect_func_decl_constraints(db, func.into(), true).instantiate_identity();
    let mut inputs = Vec::new();
    if func.is_method(db)
        && let Some(param) = func.params(db).next()
    {
        let origin = CallableInputLayoutHoleOrigin::Receiver;
        let ty = if param.self_ty_fallback(db) {
            func.expected_self_ty(db)
        } else {
            param
                .hir_ty(db)
                .map(|hir_ty| lower_hir_ty(db, hir_ty, func.scope(), assumptions))
        };
        if let Some(ty) = ty {
            inputs.push((origin, ty));
        }
    }
    for param in func.params(db).filter(|param| !param.is_self_param(db)) {
        let Some(hir_ty) = param.hir_ty(db) else {
            continue;
        };
        let origin = CallableInputLayoutHoleOrigin::ValueParam(param.index());
        let ty = lower_hir_ty(db, hir_ty, func.scope(), assumptions);
        inputs.push((origin, ty));
    }
    for effect in func.effect_params(db) {
        let Some(key_path) = effect.key_path(db) else {
            continue;
        };
        let ResolvedEffectKey::Type(schema) =
            resolve_callable_input_effect_key(db, func, effect.index(), key_path, assumptions)
        else {
            continue;
        };
        let origin = CallableInputLayoutHoleOrigin::Effect(effect.index());
        inputs.push((origin, schema.carrier));
    }
    inputs
}

fn callable_input_layout_projections<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> Vec<(
    CallableInputLayoutHoleOrigin,
    CallableLayoutProjections<'db>,
)> {
    callable_input_layout_types(db, func)
        .into_iter()
        .map(|(origin, ty)| {
            (
                origin,
                callable_layout_projections_for_ty(
                    db,
                    CallableLayoutSchemaSite::Input { func, origin },
                    ty,
                ),
            )
        })
        .collect()
}

fn callable_input_carrier_projections<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> Vec<(
    CallableInputLayoutHoleOrigin,
    CallableLayoutProjections<'db>,
)> {
    callable_input_layout_types(db, func)
        .into_iter()
        .map(|(origin, ty)| {
            (
                origin,
                callable_layout_projections_for_ty_with_effect_targets(
                    db,
                    CallableLayoutSchemaSite::Input { func, origin },
                    ty,
                    false,
                ),
            )
        })
        .collect()
}

fn bind_callable_layout_bundle_schema<'db>(
    db: &'db dyn HirAnalysisDb,
    site: CallableLayoutSchemaSite<'db>,
    projection: &CallableLayoutProjections<'db>,
    bindings: &[(TyId<'db>, TyId<'db>)],
) -> LayoutBundleSchema<'db> {
    let mut schema = projection.schema.clone();
    bind_direct_layout_const_params(db, &mut schema);
    bind_projected_component_const_metadata(db, site, &mut schema, &projection.port_tys);
    for (component, placeholders) in schema
        .components
        .iter_mut()
        .zip(&projection.component_placeholders)
    {
        for param in placeholders.iter().filter_map(|placeholder| {
            bindings
                .iter()
                .find_map(|(candidate, param)| (candidate == placeholder).then_some(*param))
        }) {
            if !component.supplied_const_params.contains(&param) {
                component.supplied_const_params.push(param);
            }
            if !component.dependent_const_params.contains(&param) {
                component.dependent_const_params.push(param);
            }
        }
        if matches!(
            component.representative,
            Some(LayoutBundleComponentKey::Root(_))
        ) && let Some(param) = placeholders.iter().find_map(|placeholder| {
            bindings
                .iter()
                .find_map(|(candidate, param)| (candidate == placeholder).then_some(*param))
        }) {
            component.representative = Some(LayoutBundleComponentKey::Param(param));
        }
    }
    schema
}

fn bind_direct_layout_const_params<'db>(
    db: &'db dyn HirAnalysisDb,
    schema: &mut LayoutBundleSchema<'db>,
) {
    for component in &mut schema.components {
        if let Some(LayoutBundleComponentKey::Param(param)) = component.representative
            && matches!(
                param.data(db),
                TyData::ConstTy(const_ty)
                    if matches!(const_ty.data(db), ConstTyData::TyParam(_, _))
            )
            && !component.supplied_const_params.contains(&param)
        {
            component.supplied_const_params.push(param);
            if !component.dependent_const_params.contains(&param) {
                component.dependent_const_params.push(param);
            }
        }
    }
}

struct FormalConstDependencyCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    params: Vec<TyId<'db>>,
}

impl<'db> TyVisitor<'db> for FormalConstDependencyCollector<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    fn visit_const_param(&mut self, param: &TyParam<'db>, const_ty_ty: TyId<'db>) {
        let param = TyId::const_ty(
            self.db,
            ConstTyId::new(self.db, ConstTyData::TyParam(param.clone(), const_ty_ty)),
        );
        if !self.params.contains(&param) {
            self.params.push(param);
        }
    }
}

fn bind_projected_component_const_metadata<'db>(
    db: &'db dyn HirAnalysisDb,
    site: CallableLayoutSchemaSite<'db>,
    schema: &mut LayoutBundleSchema<'db>,
    port_tys: &FxHashMap<LayoutPortKey, TyId<'db>>,
) {
    for component in &mut schema.components {
        let Some(ty) = port_tys.get(&component.port) else {
            continue;
        };
        let mut collector = FormalConstDependencyCollector {
            db,
            params: Vec::new(),
        };
        ty.visit_with(&mut collector);
        for param in collector.params {
            let param = site.canonical_param(db, param);
            if param.const_ty_ty(db) == Some(component.ty)
                && !component.dependent_const_params.contains(&param)
            {
                component.dependent_const_params.push(param);
            }
        }
        let param = site.canonical_param(db, *ty);
        if matches!(
            param.data(db),
            TyData::ConstTy(const_ty) if matches!(const_ty.data(db), ConstTyData::TyParam(_, _))
        ) && param.const_ty_ty(db) == Some(component.ty)
        {
            if !component.supplied_const_params.contains(&param) {
                component.supplied_const_params.push(param);
            }
            if !component.dependent_const_params.contains(&param) {
                component.dependent_const_params.push(param);
            }
        }
    }
}

fn preserve_declared_component_metadata<'db>(
    db: &'db dyn HirAnalysisDb,
    site: CallableLayoutSchemaSite<'db>,
    schema: &mut LayoutBundleSchema<'db>,
    component_refined_ports: &[Vec<LayoutPortKey>],
    declared: &LayoutBundleSchema<'db>,
    declared_port_tys: &FxHashMap<LayoutPortKey, TyId<'db>>,
) {
    for (component, refined_ports) in schema.components.iter_mut().zip(component_refined_ports) {
        if let Some(declared) = declared
            .components
            .iter()
            .find(|candidate| candidate.port == component.port)
        {
            component.declaration = declared.declaration.clone();
            component.supplied_const_params = declared.supplied_const_params.clone();
            component.dependent_const_params = declared.dependent_const_params.clone();
        } else {
            for param in declared
                .components
                .iter()
                .filter(|declared| refined_ports.contains(&declared.port))
                .flat_map(|declared| &declared.supplied_const_params)
            {
                if !component.supplied_const_params.contains(param) {
                    component.supplied_const_params.push(*param);
                }
            }
            for param in declared
                .components
                .iter()
                .filter(|declared| refined_ports.contains(&declared.port))
                .flat_map(|declared| &declared.dependent_const_params)
            {
                if !component.dependent_const_params.contains(param) {
                    component.dependent_const_params.push(*param);
                }
            }
        }
    }
    bind_projected_component_const_metadata(db, site, schema, declared_port_tys);
}

fn output_witness_layout_schema<'db>(
    inputs: &[CallableLayoutBundleInput<'db>],
    output: &LayoutBundleSchema<'db>,
) -> LayoutBundleSchema<'db> {
    let mut components = output
        .components
        .iter()
        .filter(|component| {
            if !component.is_runtime() {
                return false;
            }
            !inputs
                .iter()
                .flat_map(|input| &input.schema.components)
                .any(|candidate| component.formally_derivable_from(candidate))
        })
        .cloned()
        .collect::<Vec<_>>();
    for (idx, component) in components.iter_mut().enumerate() {
        component.id = LayoutBundleComponentId(idx as u32);
    }
    LayoutBundleSchema {
        components,
        view_aliases: output.view_aliases.clone(),
        non_regular_view_cycle: output.non_regular_view_cycle.clone(),
    }
}

pub fn callable_layout_bundle_signature<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> CallableLayoutBundleSignature<'db> {
    let projections = callable_input_layout_projections(db, func);
    let plan = func_implicit_param_plan(db, func);
    let inputs = projections
        .into_iter()
        .filter_map(|(origin, _)| {
            let schema = plan.layout_bundle_schemas_by_origin.get(&origin)?.clone();
            (!schema.components.is_empty()).then_some(CallableLayoutBundleInput { origin, schema })
        })
        .collect::<Vec<_>>();
    let mut output = callable_layout_projections_for_ty(
        db,
        CallableLayoutSchemaSite::Output { func },
        func.return_ty(db),
    );
    bind_direct_layout_const_params(db, &mut output.schema);
    bind_projected_component_const_metadata(
        db,
        CallableLayoutSchemaSite::Output { func },
        &mut output.schema,
        &output.port_tys,
    );
    let output = output.schema;
    let output_witnesses = output_witness_layout_schema(&inputs, &output);
    CallableLayoutBundleSignature {
        inputs,
        output_witnesses,
        output,
    }
}

fn specialized_layout_component_key<'db>(
    db: &'db dyn HirAnalysisDb,
    value: TyId<'db>,
) -> LayoutBundleComponentKey<'db> {
    if let Some(root) = layout_root_id(db, value) {
        LayoutBundleComponentKey::Root(root)
    } else if value.has_param(db) || value.has_var(db) {
        LayoutBundleComponentKey::Param(value)
    } else {
        LayoutBundleComponentKey::Static(value)
    }
}

/// Retains the declaration's terminal component topology when specialization
/// makes independent const arguments equal. Components introduced by an
/// opaque generic type still use the specialized topology. Instantiated const
/// equality must never decide whether a declaration-relative port exists.
fn preserve_declared_component_ports<'db>(
    projection: &mut CallableLayoutProjections<'db>,
    declared: &LayoutBundleSchema<'db>,
) -> Vec<LayoutPortKey> {
    let missing = declared
        .components
        .iter()
        .filter(|declared| {
            !projection
                .schema
                .components
                .iter()
                .zip(&projection.component_refined_ports)
                .any(|(component, refined)| {
                    component.port == declared.port || refined.contains(&declared.port)
                })
        })
        .cloned()
        .collect::<Vec<_>>();
    let missing_ports = missing
        .iter()
        .map(|component| component.port.clone())
        .collect::<Vec<_>>();
    for component in missing {
        projection.schema.components.push(component);
        projection.component_placeholders.push(Vec::new());
        projection.component_refined_ports.push(Vec::new());
    }
    for alias in &declared.view_aliases {
        if !projection.schema.view_aliases.contains(alias) {
            projection.schema.view_aliases.push(alias.clone());
        }
    }
    if projection.schema.non_regular_view_cycle.is_none() {
        projection.schema.non_regular_view_cycle = declared.non_regular_view_cycle.clone();
    }
    for (idx, component) in projection.schema.components.iter_mut().enumerate() {
        component.id = LayoutBundleComponentId(idx as u32);
    }
    for (component, refined) in projection
        .schema
        .components
        .iter_mut()
        .zip(&projection.component_refined_ports)
    {
        component.transport = declared
            .components
            .iter()
            .find(|declared| declared.port == component.port)
            .or_else(|| {
                declared
                    .components
                    .iter()
                    .find(|declared| refined.contains(&declared.port))
            })
            .map_or(LayoutBundleTransport::Runtime, |declared| {
                declared.transport
            });
    }
    missing_ports
}

fn specialize_component_representative<'db>(
    db: &'db dyn HirAnalysisDb,
    component: &mut LayoutBundleComponent<'db>,
    args: &[TyId<'db>],
) {
    if let Some(LayoutBundleComponentKey::Param(value)) = component.representative {
        let value = Binder::bind(value).instantiate(db, args);
        component.representative = Some(specialized_layout_component_key(db, value));
    }
    component.ty = Binder::bind(component.ty).instantiate(db, args);
}

fn specialize_callable_input_layout_schema<'db>(
    db: &'db dyn HirAnalysisDb,
    site: CallableLayoutSchemaSite<'db>,
    mut projection: CallableLayoutProjections<'db>,
    declared: Option<&LayoutBundleSchema<'db>>,
    declared_port_tys: Option<&FxHashMap<LayoutPortKey, TyId<'db>>>,
    bindings: &[(TyId<'db>, TyId<'db>)],
    args: &[TyId<'db>],
) -> LayoutBundleSchema<'db> {
    let restored_ports = if let Some(declared) = declared {
        preserve_declared_component_ports(&mut projection, declared)
    } else {
        for component in &mut projection.schema.components {
            component.transport = LayoutBundleTransport::Runtime;
        }
        Vec::new()
    };
    for (component, placeholders) in projection
        .schema
        .components
        .iter_mut()
        .zip(&projection.component_placeholders)
    {
        let values = placeholders
            .iter()
            .filter_map(|placeholder| {
                let (_, bound) = bindings
                    .iter()
                    .find(|(candidate, _)| candidate == placeholder)?;
                let TyData::ConstTy(const_ty) = bound.data(db) else {
                    return Some(*bound);
                };
                let ConstTyData::TyParam(param, _) = const_ty.data(db) else {
                    return Some(*bound);
                };
                Some(args.get(param.idx).copied().unwrap_or(*bound))
            })
            .collect::<Vec<_>>();
        if let Some(value) = values.first().copied() {
            component.representative = values
                .iter()
                .copied()
                .all(|candidate| same_layout_argument(db, value, candidate))
                .then(|| specialized_layout_component_key(db, value));
        }
        if restored_ports.contains(&component.port) {
            specialize_component_representative(db, component, args);
        }
    }
    let component_refined_ports = projection.component_refined_ports;
    let mut schema = projection.schema;
    if let Some(declared) = declared {
        preserve_declared_component_metadata(
            db,
            site,
            &mut schema,
            &component_refined_ports,
            declared,
            declared_port_tys.expect("declared input schema must have projected types"),
        );
    }
    debug_assert!(
        schema.non_regular_view_cycle.is_some() || schema.validate().is_ok(),
        "layout specialization produced an invalid schema: {schema:#?}",
    );
    schema
}

/// Derives the complete layout ABI for one monomorphized callable type.
///
/// Generic type parameters are expanded only after substitution, so an opaque
/// declaration such as `fn peek<T>(self: Mutex<T>) -> T` transports layout
/// components introduced by the concrete `T`. Ports remain anchored to the
/// declaration's input/output sites and therefore still match caller values.
pub(crate) fn specialized_callable_layout_bundle_signature<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    args: &[TyId<'db>],
) -> CallableLayoutBundleSignature<'db> {
    specialized_callable_layout_bundle_signature_with_normalizer(db, func, args, |ty| ty)
}

pub(crate) fn specialized_callable_layout_bundle_signature_with_normalizer<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    args: &[TyId<'db>],
    normalize: impl Fn(TyId<'db>) -> TyId<'db>,
) -> CallableLayoutBundleSignature<'db> {
    let plan = func_implicit_param_plan(db, func);
    let inputs = callable_input_layout_types(db, func)
        .into_iter()
        .filter_map(|(origin, ty)| {
            let bindings: &[(TyId<'db>, TyId<'db>)] = plan
                .bindings_by_origin
                .get(&origin)
                .map_or(&[], Vec::as_slice);
            let placeholder_args = bindings.iter().copied().collect::<FxHashMap<_, _>>();
            let ty = substitute_layout_holes_by_placeholder(db, ty, &placeholder_args);
            let ty = Binder::bind(ty).instantiate(db, args);
            let ty = normalize(ty);
            let projection = callable_layout_projections_for_ty(
                db,
                CallableLayoutSchemaSite::Input { func, origin },
                ty,
            );
            let schema = specialize_callable_input_layout_schema(
                db,
                CallableLayoutSchemaSite::Input { func, origin },
                projection,
                plan.layout_bundle_schemas_by_origin.get(&origin),
                plan.layout_port_tys_by_origin.get(&origin),
                bindings,
                args,
            );
            (!schema.components.is_empty()).then_some(CallableLayoutBundleInput { origin, schema })
        })
        .collect::<Vec<_>>();
    let output_ty = normalize(Binder::bind(func.return_ty(db)).instantiate(db, args));
    let output_ty = normalize(output_ty);
    let mut output = callable_layout_projections_for_ty(
        db,
        CallableLayoutSchemaSite::Output { func },
        output_ty,
    );
    let mut declared_output = callable_layout_projections_for_ty(
        db,
        CallableLayoutSchemaSite::Output { func },
        func.return_ty(db),
    );
    bind_direct_layout_const_params(db, &mut declared_output.schema);
    let restored_ports = preserve_declared_component_ports(&mut output, &declared_output.schema);
    for component in &mut output.schema.components {
        if restored_ports.contains(&component.port) {
            specialize_component_representative(db, component, args);
        }
    }
    let component_refined_ports = output.component_refined_ports;
    let mut output = output.schema;
    preserve_declared_component_metadata(
        db,
        CallableLayoutSchemaSite::Output { func },
        &mut output,
        &component_refined_ports,
        &declared_output.schema,
        &declared_output.port_tys,
    );
    debug_assert!(
        output.non_regular_view_cycle.is_some() || output.validate().is_ok(),
        "layout output specialization produced an invalid schema: {output:#?}",
    );
    let output_witnesses = output_witness_layout_schema(&inputs, &output);
    CallableLayoutBundleSignature {
        inputs,
        output_witnesses,
        output,
    }
}

pub fn layout_bundle_schema_for_semantic_value<'db>(
    db: &'db dyn HirAnalysisDb,
    body: crate::hir_def::Body<'db>,
    local: u32,
    ty: TyId<'db>,
    template_ty: TyId<'db>,
) -> LayoutBundleSchema<'db> {
    let mut schema =
        callable_layout_projections_for_ty(db, CallableLayoutSchemaSite::Value { body, local }, ty);
    let mut template = callable_layout_projections_for_ty(
        db,
        CallableLayoutSchemaSite::Value { body, local },
        template_ty,
    );
    bind_direct_layout_const_params(db, &mut template.schema);
    preserve_declared_component_ports(&mut schema, &template.schema);
    let component_refined_ports = schema.component_refined_ports;
    let mut schema = schema.schema;
    preserve_declared_component_metadata(
        db,
        CallableLayoutSchemaSite::Value { body, local },
        &mut schema,
        &component_refined_ports,
        &template.schema,
        &template.port_tys,
    );
    schema
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
    if !collect_layout_arg_bindings(
        db,
        expected_key.carrier,
        actual_key_ty,
        &mut actual_layout_args,
    ) {
        return;
    }

    for (expected, actual_arg) in actual_layout_args {
        let Some((_, implicit_arg)) = bindings
            .iter()
            .find(|(placeholder, implicit)| *placeholder == expected || *implicit == expected)
        else {
            continue;
        };
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

fn same_layout_argument<'db>(db: &'db dyn HirAnalysisDb, lhs: TyId<'db>, rhs: TyId<'db>) -> bool {
    match (
        super::layout_holes::layout_root_placeholder(db, lhs),
        super::layout_holes::layout_root_placeholder(db, rhs),
    ) {
        (Some(lhs), Some(rhs)) => lhs == rhs,
        _ => lhs == rhs,
    }
}

fn collect_layout_arg_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    expected: TyId<'db>,
    actual: TyId<'db>,
    out: &mut Vec<(TyId<'db>, TyId<'db>)>,
) -> bool {
    let is_layout_arg = match expected.data(db) {
        TyData::ConstTy(const_ty) => match const_ty.data(db) {
            ConstTyData::Hole(..) => true,
            ConstTyData::TyParam(param, _) => param.is_implicit(),
            _ => false,
        },
        _ => false,
    };
    if is_layout_arg {
        if let Some((_, bound)) = out.iter().find(|(placeholder, _)| *placeholder == expected) {
            return same_layout_argument(db, *bound, actual);
        }
        out.push((expected, actual));
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
        .zip(actual_args)
        .all(|(expected, actual)| collect_layout_arg_bindings(db, *expected, *actual, out))
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
    let mut groups = callable_input_layout_hole_groups(db, func);
    let projections = callable_input_layout_projections(db, func);
    for (origin, projection) in &projections {
        let origin = *origin;
        if let Some(group) = groups.iter_mut().find(|group| group.origin == origin) {
            for &placeholder in &projection.placeholders {
                if !group.placeholders.contains(&placeholder) {
                    group.placeholders.push(placeholder);
                }
            }
        } else if !projection.placeholders.is_empty() {
            groups.push(CallableInputLayoutHoleGroup {
                origin,
                placeholders: projection.placeholders.clone(),
            });
        }
    }
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

    let mut layout_projected_tys_by_origin = FxHashMap::default();
    let mut layout_port_tys_by_origin = FxHashMap::default();
    let mut carrier_projected_tys_by_origin = FxHashMap::default();
    let mut projected_paths_by_origin = FxHashMap::default();
    let mut projected_index_lengths_by_origin = FxHashMap::default();
    let mut layout_bundle_schemas_by_origin = FxHashMap::default();
    for (origin, projection) in projections {
        let args = bindings_by_origin
            .get(&origin)
            .into_iter()
            .flatten()
            .copied()
            .collect::<FxHashMap<_, _>>();
        let schema = bind_callable_layout_bundle_schema(
            db,
            CallableLayoutSchemaSite::Input { func, origin },
            &projection,
            bindings_by_origin.get(&origin).map_or(&[], Vec::as_slice),
        );
        let tys = projection
            .tys
            .into_iter()
            .map(|(path, ty)| (path, substitute_layout_holes_by_placeholder(db, ty, &args)))
            .collect();
        let port_tys = projection
            .port_tys
            .into_iter()
            .map(|(port, ty)| (port, substitute_layout_holes_by_placeholder(db, ty, &args)))
            .collect();
        layout_projected_tys_by_origin.insert(origin, tys);
        layout_port_tys_by_origin.insert(origin, port_tys);
        projected_index_lengths_by_origin.insert(origin, projection.index_lengths);
        projected_paths_by_origin.insert(origin, projection.paths);
        layout_bundle_schemas_by_origin.insert(origin, schema);
    }
    for (origin, projection) in callable_input_carrier_projections(db, func) {
        let args = bindings_by_origin
            .get(&origin)
            .into_iter()
            .flatten()
            .copied()
            .collect::<FxHashMap<_, _>>();
        let tys = projection
            .tys
            .into_iter()
            .map(|(path, ty)| (path, substitute_layout_holes_by_placeholder(db, ty, &args)))
            .collect();
        carrier_projected_tys_by_origin.insert(origin, tys);
    }

    FuncImplicitParamPlan {
        implicit_precursors,
        bindings_by_origin,
        provider_param_index_by_effect,
        layout_bundle_schemas_by_origin,
        layout_projected_tys_by_origin,
        layout_port_tys_by_origin,
        carrier_projected_tys_by_origin,
        projected_paths_by_origin,
        projected_index_lengths_by_origin,
    }
}

pub fn callable_input_layout_bundle_schema<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    origin: CallableInputLayoutHoleOrigin,
) -> Option<LayoutBundleSchema<'db>> {
    func_implicit_param_plan(db, func)
        .layout_bundle_schemas_by_origin
        .get(&origin)
        .cloned()
}

/// Returns terminal callable input projections that can physically back a
/// returned value containing the given generic layout parameter.
///
/// This is ownership provenance for borrow checking, not runtime layout
/// evidence. Aggregate ancestors are omitted when a descendant names the
/// concrete carrier place. Indexed families retain the projection needed to
/// decide whether a unique backing place exists.
pub fn callable_input_layout_backing_sources<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    param_idx: usize,
) -> Vec<CallableInputLayoutBackingSource> {
    let plan = func_implicit_param_plan(db, func);
    let mut sources = Vec::new();
    let mut origins = Vec::new();
    for (idx, ty) in func.arg_tys(db).into_iter().enumerate() {
        let origin = if func.is_method(db) && idx == 0 {
            CallableInputLayoutHoleOrigin::Receiver
        } else {
            CallableInputLayoutHoleOrigin::ValueParam(idx)
        };
        origins.push(origin);
        if value_contains_generic_param(db, ty.skip_binder(), param_idx) {
            sources.push(CallableInputLayoutBackingSource {
                origin,
                projection: Vec::new(),
            });
        }
    }
    let assumptions = collect_func_decl_constraints(db, func.into(), true).instantiate_identity();
    for effect in func.effect_params(db) {
        let Some(key_path) = effect.key_path(db) else {
            continue;
        };
        let resolved =
            resolve_callable_input_effect_key(db, func, effect.index(), key_path, assumptions);
        let origin = CallableInputLayoutHoleOrigin::Effect(effect.index());
        let bindings = plan
            .bindings_by_origin
            .get(&origin)
            .into_iter()
            .flatten()
            .copied()
            .collect::<FxHashMap<_, _>>();
        let contains = match resolved {
            ResolvedEffectKey::Type(schema) => value_contains_generic_param(
                db,
                &substitute_layout_holes_by_placeholder(db, schema.carrier, &bindings),
                param_idx,
            ),
            ResolvedEffectKey::Trait(schema) => value_contains_generic_param(
                db,
                &substitute_layout_holes_by_placeholder_in(
                    db,
                    schema.into_trait_inst(db),
                    &bindings,
                ),
                param_idx,
            ),
            ResolvedEffectKey::Invalid | ResolvedEffectKey::Other => false,
        };
        if contains {
            sources.push(CallableInputLayoutBackingSource {
                origin,
                projection: Vec::new(),
            });
        }
    }
    origins.extend((0..func.effects(db).data(db).len()).map(CallableInputLayoutHoleOrigin::Effect));
    for origin in origins {
        let Some(paths) = plan.projected_paths_by_origin.get(&origin) else {
            continue;
        };
        let projected_tys = &plan.layout_projected_tys_by_origin[&origin];
        for path in paths {
            if value_contains_generic_param(db, &projected_tys[path], param_idx) {
                let source = CallableInputLayoutBackingSource {
                    origin,
                    projection: path.clone(),
                };
                if !sources.contains(&source) {
                    sources.push(source);
                }
            }
        }
    }
    sources
        .iter()
        .filter(|source| {
            let lexical_descendant = sources.iter().any(|descendant| {
                descendant.origin == source.origin
                    && descendant.projection.len() > source.projection.len()
                    && descendant.projection.starts_with(&source.projection)
            });
            let nonterminal_const = matches!(
                source.projection.last(),
                Some(LayoutBundlePathStep::ConstParam(_))
            ) && {
                let owner = &source.projection[..source.projection.len() - 1];
                sources.iter().any(|descendant| {
                    descendant.origin == source.origin
                        && descendant.projection.starts_with(owner)
                        && matches!(
                            descendant.projection.get(owner.len()),
                            Some(
                                LayoutBundlePathStep::Field(_)
                                    | LayoutBundlePathStep::Variant(_)
                                    | LayoutBundlePathStep::Index
                            )
                        )
                })
            };
            !lexical_descendant && !nonterminal_const
        })
        .cloned()
        .collect()
}

pub fn callable_input_layout_backing_index_lengths<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    source: &CallableInputLayoutBackingSource,
) -> Option<Vec<usize>> {
    if source.projection.is_empty() {
        return Some(Vec::new());
    }
    let plan = func_implicit_param_plan(db, func);
    plan.projected_index_lengths_by_origin
        .get(&source.origin)?
        .get(&source.projection)
        .cloned()
}

fn value_contains_generic_param<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: &T,
    param_idx: usize,
) -> bool
where
    T: TyVisitable<'db>,
{
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        param_idx: usize,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_param(&mut self, param: &TyParam<'db>) {
            if param.idx == self.param_idx {
                self.found = true;
            }
        }

        fn visit_const_param(&mut self, param: &TyParam<'db>, _: TyId<'db>) {
            self.visit_param(param);
        }
    }

    let mut finder = Finder {
        db,
        param_idx,
        found: false,
    };
    value.visit_with(&mut finder);
    finder.found
}

fn layout_projection_paths_in_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    param_idx: usize,
) -> Vec<(LayoutBundlePath, Vec<usize>)> {
    struct Collector<'db> {
        db: &'db dyn HirAnalysisDb,
        param_idx: usize,
        adt_stack: Vec<AdtDef<'db>>,
        out: Vec<(LayoutBundlePath, Vec<usize>)>,
    }

    impl<'db> Collector<'db> {
        fn contains(&self, ty: TyId<'db>) -> bool {
            value_contains_generic_param(self.db, &ty, self.param_idx)
        }

        fn walk(
            &mut self,
            ty: TyId<'db>,
            path: &mut LayoutBundlePath,
            index_lengths: &mut Vec<usize>,
        ) {
            let ty = ty.as_capability(self.db).map_or(ty, |(_, inner)| inner);
            if !self.contains(ty) {
                return;
            }
            if ty.is_tuple(self.db) {
                let mut found_descendant = false;
                for (idx, field) in ty.field_types(self.db).into_iter().enumerate() {
                    if !self.contains(field) {
                        continue;
                    }
                    let Ok(idx) = u16::try_from(idx) else {
                        continue;
                    };
                    found_descendant = true;
                    path.push(LayoutBundlePathStep::Field(idx));
                    self.walk(field, path, index_lengths);
                    path.pop();
                }
                if !found_descendant {
                    self.out.push((path.clone(), index_lengths.clone()));
                }
                return;
            }
            if ty.is_array(self.db) {
                let Some(&element) = ty.generic_args(self.db).first() else {
                    return;
                };
                if !self.contains(element) {
                    self.out.push((path.clone(), index_lengths.clone()));
                    return;
                }
                let Some(len) = ty.array_len(self.db) else {
                    return;
                };
                path.push(LayoutBundlePathStep::Index);
                index_lengths.push(len);
                self.walk(element, path, index_lengths);
                index_lengths.pop();
                path.pop();
                return;
            }
            let Some(adt) = ty.adt_def(self.db) else {
                self.out.push((path.clone(), index_lengths.clone()));
                return;
            };
            if self.adt_stack.contains(&adt) {
                self.out.push((path.clone(), index_lengths.clone()));
                return;
            }
            self.adt_stack.push(adt);
            let args = ty.generic_args(self.db);
            let mut found_descendant = false;
            for (param_idx, arg) in args.iter().copied().enumerate() {
                if !matches!(arg.data(self.db), TyData::ConstTy(_)) || !self.contains(arg) {
                    continue;
                }
                let Ok(param_idx) = u16::try_from(param_idx) else {
                    continue;
                };
                found_descendant = true;
                path.push(LayoutBundlePathStep::ConstParam(param_idx));
                self.out.push((path.clone(), index_lengths.clone()));
                path.pop();
            }
            match adt.adt_ref(self.db) {
                AdtRef::Struct(_) => {
                    for field_idx in 0..adt.fields(self.db)[0].num_types() {
                        let field = instantiate_adt_field_shape(self.db, adt, 0, field_idx, args);
                        if !self.contains(field) {
                            continue;
                        }
                        let Ok(field_idx) = u16::try_from(field_idx) else {
                            continue;
                        };
                        found_descendant = true;
                        path.push(LayoutBundlePathStep::Field(field_idx));
                        self.walk(field, path, index_lengths);
                        path.pop();
                    }
                }
                AdtRef::Enum(_) => {
                    for (variant_idx, variant) in adt.fields(self.db).iter().enumerate() {
                        let Ok(variant_step) = u16::try_from(variant_idx) else {
                            continue;
                        };
                        for field_idx in 0..variant.num_types() {
                            let field = instantiate_adt_field_shape(
                                self.db,
                                adt,
                                variant_idx,
                                field_idx,
                                args,
                            );
                            if !self.contains(field) {
                                continue;
                            }
                            let Ok(field_step) = u16::try_from(field_idx) else {
                                continue;
                            };
                            found_descendant = true;
                            path.push(LayoutBundlePathStep::Variant(variant_step));
                            path.push(LayoutBundlePathStep::Field(field_step));
                            self.walk(field, path, index_lengths);
                            path.pop();
                            path.pop();
                        }
                    }
                }
            }
            self.adt_stack.pop();
            if !found_descendant {
                self.out.push((path.clone(), index_lengths.clone()));
            }
        }
    }

    let mut collector = Collector {
        db,
        param_idx,
        adt_stack: Vec::new(),
        out: Vec::new(),
    };
    collector.walk(ty, &mut Vec::new(), &mut Vec::new());
    collector.out
}

pub(crate) fn layout_param_projection_paths_in_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    param_idx: usize,
) -> Vec<(LayoutBundlePath, Vec<usize>)> {
    layout_projection_paths_in_ty(db, ty, param_idx)
}

pub(crate) fn callable_input_projected_layout_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    origin: CallableInputLayoutHoleOrigin,
    path: &[LayoutBundlePathStep],
) -> Option<TyId<'db>> {
    func_implicit_param_plan(db, func)
        .layout_projected_tys_by_origin
        .get(&origin)?
        .get(path)
        .copied()
}

pub(crate) fn callable_input_carrier_projected_layout_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    origin: CallableInputLayoutHoleOrigin,
    path: &[LayoutBundlePathStep],
) -> Option<TyId<'db>> {
    func_implicit_param_plan(db, func)
        .carrier_projected_tys_by_origin
        .get(&origin)?
        .get(path)
        .copied()
}

pub(crate) fn callable_input_layout_origin_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    origin: CallableInputLayoutHoleOrigin,
) -> Option<TyId<'db>> {
    match origin {
        CallableInputLayoutHoleOrigin::Receiver | CallableInputLayoutHoleOrigin::ValueParam(_) => {
            let idx = match origin {
                CallableInputLayoutHoleOrigin::Receiver => 0,
                CallableInputLayoutHoleOrigin::ValueParam(idx) => idx,
                CallableInputLayoutHoleOrigin::Effect(_) => unreachable!(),
            };
            func.arg_tys(db)
                .get(idx)
                .map(|ty| ty.instantiate_identity())
        }
        CallableInputLayoutHoleOrigin::Effect(effect_idx) => {
            let assumptions =
                collect_func_decl_constraints(db, func.into(), true).instantiate_identity();
            let key_path = func.effect_params(db).nth(effect_idx)?.key_path(db)?;
            let ResolvedEffectKey::Type(schema) =
                resolve_callable_input_effect_key(db, func, effect_idx, key_path, assumptions)
            else {
                return None;
            };
            let bindings = func_implicit_param_plan(db, func)
                .bindings_by_origin
                .get(&origin)
                .into_iter()
                .flatten()
                .copied()
                .collect::<FxHashMap<_, _>>();
            Some(substitute_layout_holes_by_placeholder(
                db,
                schema.carrier,
                &bindings,
            ))
        }
    }
}

pub(crate) fn callable_input_layout_projection_paths<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    origin: CallableInputLayoutHoleOrigin,
) -> Vec<LayoutBundlePath> {
    func_implicit_param_plan(db, func)
        .projected_paths_by_origin
        .get(&origin)
        .cloned()
        .unwrap_or_default()
}

pub(crate) fn instantiate_callable_projection_layout_args<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    origin: CallableInputLayoutHoleOrigin,
    path: &[LayoutBundlePathStep],
    actual_ty: TyId<'db>,
    subst_args: &mut [TyId<'db>],
) {
    let Some(expected_ty) = callable_input_projected_layout_ty(db, func, origin, path) else {
        return;
    };
    let mut bindings = Vec::new();
    if !collect_layout_arg_bindings(db, expected_ty, actual_ty, &mut bindings) {
        return;
    }
    for (expected, actual) in bindings {
        let TyData::ConstTy(const_ty) = expected.data(db) else {
            continue;
        };
        let ConstTyData::TyParam(param, _) = const_ty.data(db) else {
            continue;
        };
        if param.is_implicit()
            && let Some(slot) = subst_args.get_mut(param.idx)
        {
            *slot = actual;
        }
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

#[salsa::tracked(return_ref, cycle_fn=lower_type_alias_cycle_recover, cycle_initial=lower_type_alias_cycle_initial)]
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
            alias_to: Binder::bind(TyId::invalid(db, InvalidCause::ParseError)),
            layout_root_uses: Vec::new(),
            param_set,
        };
    };

    let assumptions = match const_bodies {
        ConstBodyLowering::Eager => collect_constraints(db, alias.into()),
        ConstBodyLowering::Deferred => collect_candidate_constraints(db, alias.into()),
    }
    .instantiate_identity();
    let minter = match const_bodies {
        ConstBodyLowering::Eager => HoleMinter::new(HoleAnchor::AliasTemplate(alias)),
        ConstBodyLowering::Deferred => HoleMinter::deferred(HoleAnchor::AliasTemplate(alias)),
    };
    let layout_root_uses =
        lower_layout_root_uses_in_hir_ty(db, hir_ty, alias.scope(), assumptions, &minter);
    let alias_to = match const_bodies {
        ConstBodyLowering::Eager => lower_hir_ty(db, hir_ty, alias.scope(), assumptions),
        ConstBodyLowering::Deferred => {
            lower_hir_ty_impl(db, hir_ty, alias.scope(), assumptions, &minter)
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
    } else {
        // Mark template ownership: use sites replace alias-anchored holes
        // with fresh holes from their own minter.
        reanchor_template_holes(db, alias_to, HoleAnchor::AliasTemplate(alias))
    };
    TyAlias {
        alias,
        alias_to: Binder::bind(alias_to),
        layout_root_uses,
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
        layout_root_uses: Vec::new(),
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
    pub layout_root_uses: Vec<LayoutRootUse<'db>>,
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
        minter: &HoleMinter<'db>,
    ) -> TyId<'db> {
        self.instantiate_layout_from_path(db, path, args, assumptions, minter)
            .ty
    }

    pub(crate) fn instantiate_layout_from_path(
        &self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        args: &[TyId<'db>],
        assumptions: PredicateListId<'db>,
        minter: &HoleMinter<'db>,
    ) -> super::layout_holes::LayoutInstantiation<'db> {
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
            Some(minter),
        );
        if completed.len() < expected {
            return instantiate_layout_template(
                db,
                TyId::invalid(
                    db,
                    InvalidCause::UnboundTypeAliasParam {
                        alias: self.alias,
                        n_given_args: args.len(),
                    },
                ),
                &[],
                &[],
                LayoutInstantiationContext::Lowering(minter.anchor()),
                LayoutBoundaryIdentity::AliasUse(self.alias),
                vec![LayoutOccurrenceStep::Instantiation(
                    minter.next_instantiation_ordinal(),
                )],
            );
        }
        if let Some(cause) = completed.iter().find_map(|arg| arg.invalid_cause(db)) {
            return instantiate_layout_template(
                db,
                TyId::invalid(db, cause),
                &[],
                &[],
                LayoutInstantiationContext::Lowering(minter.anchor()),
                LayoutBoundaryIdentity::AliasUse(self.alias),
                vec![LayoutOccurrenceStep::Instantiation(
                    minter.next_instantiation_ordinal(),
                )],
            );
        }

        let occurrence = vec![LayoutOccurrenceStep::Instantiation(
            minter.next_instantiation_ordinal(),
        )];
        let mut instantiated = instantiate_layout_template(
            db,
            self.alias_to.instantiate_identity(),
            self.params(db),
            &completed,
            LayoutInstantiationContext::Lowering(minter.anchor()),
            LayoutBoundaryIdentity::AliasUse(self.alias),
            occurrence.clone(),
        );
        for root_use in &self.layout_root_uses {
            let mut selector = occurrence.clone();
            selector.extend(&root_use.selector);
            let root_use = LayoutRootUse {
                value: Binder::bind(root_use.value).instantiate(db, &completed),
                owner: root_use
                    .owner
                    .map(|owner| Binder::bind(owner).instantiate(db, &completed)),
                selector,
                index_dimensions: root_use.index_dimensions.clone(),
            };
            if !instantiated.root_uses.contains(&root_use) {
                instantiated.root_uses.push(root_use);
            }
        }
        let root_params = self
            .params(db)
            .iter()
            .enumerate()
            .filter_map(|(idx, _)| {
                self.param_set
                    .const_param_default_is_slot_layout_hole(db, idx)
                    .then_some(idx)
            })
            .collect::<FxHashSet<_>>();
        for root_use in layout_param_root_uses(
            db,
            self.alias_to.instantiate_identity(),
            self.params(db),
            &completed,
            &root_params,
            occurrence,
        ) {
            if !instantiated.root_uses.contains(&root_use) {
                instantiated.root_uses.push(root_use);
            }
        }
        instantiated
    }
}

struct LayoutParamRootUseCollector<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    concrete_roots: FxHashMap<TyId<'db>, (usize, TyId<'db>)>,
    args: &'a [TyId<'db>],
    occurrence: LayoutOccurrencePath,
    path: LayoutOccurrencePath,
    visiting: FxHashSet<TyId<'db>>,
    uses: Vec<LayoutRootUse<'db>>,
}

impl<'a, 'db> LayoutParamRootUseCollector<'a, 'db> {
    fn collect(&mut self, ty: TyId<'db>, direct_owner: Option<TyId<'db>>) {
        if let Some((idx, value)) = self.concrete_roots.get(&ty) {
            let mut selector = self.occurrence.clone();
            selector.extend(self.path.iter().copied());
            selector.push(LayoutOccurrenceStep::ConstParam(*idx as u32));
            self.uses.push(LayoutRootUse {
                value: *value,
                owner: direct_owner,
                selector,
                index_dimensions: Vec::new(),
            });
            return;
        }
        if !self.visiting.insert(ty) {
            return;
        }
        let (_, ty_args) = ty.decompose_ty_app(self.db);
        if !ty_args.is_empty() {
            let owner = Binder::bind(ty).instantiate(self.db, self.args);
            let tuple = ty.is_tuple(self.db);
            for (idx, arg) in ty_args.iter().enumerate() {
                self.path.push(if tuple {
                    LayoutOccurrenceStep::TupleElem(idx as u32)
                } else {
                    LayoutOccurrenceStep::GenericArg(idx as u32)
                });
                self.collect(*arg, Some(owner));
                self.path.pop();
            }
        }
        self.visiting.remove(&ty);
    }
}

pub(crate) fn layout_param_root_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    template: TyId<'db>,
    params: &[TyId<'db>],
    args: &[TyId<'db>],
    root_params: &FxHashSet<usize>,
    occurrence: LayoutOccurrencePath,
) -> Vec<LayoutRootUse<'db>> {
    let concrete_roots = params
        .iter()
        .zip(args)
        .enumerate()
        .filter_map(|(idx, (param, value))| {
            (root_params.contains(&idx) && structural_hole_id(db, *value).is_none())
                .then_some((*param, (idx, *value)))
        })
        .collect::<FxHashMap<_, _>>();
    let mut collector = LayoutParamRootUseCollector {
        db,
        concrete_roots,
        args,
        occurrence,
        path: Vec::new(),
        visiting: FxHashSet::default(),
        uses: Vec::new(),
    };
    collector.collect(template, None);
    collector.uses
}

pub(crate) fn lower_generic_arg_list<'db>(
    db: &'db dyn HirAnalysisDb,
    args: GenericArgListId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    hole_site: LayoutHoleArgSite<'db>,
    minter: &HoleMinter<'db>,
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
                        PathRes::InherentConst(recv_ty, impl_, name) => {
                            if let Some(expected_ty) = super::const_ty::inherent_const_expected_ty(
                                db, impl_, recv_ty, name,
                            ) {
                                let use_ =
                                    InherentConstUse::new(scope, assumptions, impl_, recv_ty, name);
                                if let Some(const_ty) =
                                    super::const_ty::const_ty_or_abstract_from_inherent_const_use(
                                        db,
                                        use_,
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
                lower_opt_hir_ty_impl(db, ty_arg.ty, scope, assumptions, minter)
            }
            GenericArg::Const(const_arg) => match const_arg.value {
                ConstGenericArgValue::Expr(body) => {
                    let const_ty = lower_opt_const_body(db, body, scope, assumptions, minter);
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
                        LayoutIntroSite::lowering(hole_site, arg_idx),
                        minter.mint(db),
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

    pub(crate) fn const_param_default_is_layout_hole(
        self,
        db: &'db dyn HirAnalysisDb,
        param_idx: usize,
    ) -> bool {
        self.params_precursor(db)
            .get(param_idx)
            .is_some_and(|param| {
                matches!(param.default_hir_const, Some(ConstGenericArgValue::Hole))
            })
    }

    pub(crate) fn const_param_default_is_slot_layout_hole(
        self,
        db: &'db dyn HirAnalysisDb,
        param_idx: usize,
    ) -> bool {
        self.params_precursor(db)
            .get(param_idx)
            .filter(|param| matches!(param.default_hir_const, Some(ConstGenericArgValue::Hole)))
            .and_then(|param| param.declared_const_ty(db, self.scope(db)))
            .is_some_and(|ty| {
                matches!(
                    ty.data(db),
                    TyData::TyBase(TyBase::Prim(PrimTy::U256 | PrimTy::Usize))
                )
            })
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
        minter: Option<&HoleMinter<'db>>,
    ) -> Vec<TyId<'db>> {
        self.complete_explicit_args_with_defaults_in_mode(
            db,
            trait_self,
            provided_explicit,
            assumptions,
            completion,
            false,
            minter,
        )
    }

    fn complete_checked_explicit_args(
        self,
        db: &'db dyn HirAnalysisDb,
        trait_self: Option<TyId<'db>>,
        provided_explicit: &[TyId<'db>],
        assumptions: PredicateListId<'db>,
        completion: ConstDefaultCompletion<'db>,
        minter: Option<&HoleMinter<'db>>,
    ) -> Vec<TyId<'db>> {
        self.complete_explicit_args_with_defaults_in_mode(
            db,
            trait_self,
            provided_explicit,
            assumptions,
            completion,
            true,
            minter,
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

    #[allow(clippy::too_many_arguments)]
    fn complete_explicit_args_with_defaults_in_mode(
        self,
        db: &'db dyn HirAnalysisDb,
        trait_self: Option<TyId<'db>>,
        provided_explicit: &[TyId<'db>],
        assumptions: PredicateListId<'db>,
        completion: ConstDefaultCompletion<'db>,
        checked_explicit: bool,
        minter: Option<&HoleMinter<'db>>,
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
                } else if let Some(minter) = minter {
                    // Mint through the application's minter: the memoized
                    // lowering would hand two applications of the same
                    // default the same hole identities.
                    lower_hir_ty_impl(db, hir_ty, scope, assumptions, minter)
                } else {
                    lower_hir_ty(db, hir_ty, scope, assumptions)
                };
                let lowered = substitute_known_params(&mapping, lowered);
                mapping[i] = Some(lowered);
                result.push(lowered);
                continue;
            }

            if let Some(default) = prec.default_hir_const {
                let expected = prec.declared_const_ty(db, scope);
                let lowered = match default {
                    ConstGenericArgValue::Expr(default) => {
                        let generic_args = mapped_generic_args(&mapping, i);
                        let const_ty = if minter.is_some_and(|minter| {
                            minter.const_bodies() == ConstBodyLowering::Deferred
                        }) {
                            ConstTyId::from_opt_body_deferred(db, default, expected, generic_args)
                        } else {
                            ConstTyId::from_opt_body_with_ty_and_generic_args(
                                db,
                                default,
                                expected,
                                generic_args,
                                matches!(completion.mode, ConstDefaultCompletionMode::MetadataOnly),
                            )
                        };
                        let lowered = TyId::const_ty(db, const_ty);
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
                            .application_path
                            .and_then(|_| {
                                let owner = prec.owner?;
                                let param_idx = prec.original_idx?;
                                Some(ConstTyId::structural_hole(
                                    db,
                                    expected
                                        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),
                                    StructuralHoleOrigin::DefaultHoleParam { owner, param_idx },
                                    LayoutIntroSite::definition(owner, param_idx),
                                    minter?.mint(db),
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
