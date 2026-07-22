use smallvec1::SmallVec;

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{PathRes, resolve_ident_to_bucket, resolve_path},
        ty::{
            const_ty::{ConstTyData, ConstTyId},
            effects::{
                EffectForwarder, EffectPatternKey, EffectQuery, EffectQueryMode,
                EffectRequirementDecl, EffectRequirementKey, ForwardedEffectKey, ForwardedTraitKey,
                ForwardedTypeKey, PatternSlot, PatternSlots, StoredEffectKey, StoredTraitKey,
                StoredTypeKey, TraitPatternKey, TypePatternKey, WitnessTransport,
                effect_family_for_trait, effect_family_for_type, instantiate_trait_effect_key,
                instantiate_type_effect_key,
                match_::{instantiate_trait_pattern_in, instantiate_type_pattern_in},
                normalize_effect_identity_trait, normalize_effect_identity_ty, resolve_effect_key,
                stored_trait_key_is_rigid, stored_type_key_is_rigid,
            },
            fold::{TyFoldable, TyFolder},
            layout_holes::{LayoutPlaceholderPolicy, layout_hole_fallback_ty},
            trait_def::TraitInstId,
            trait_resolution::PredicateListId,
            ty_check::{Callable, TyChecker},
            ty_def::{InvalidCause, TyBase, TyData, TyId, TyParam, inference_keys},
            ty_lower::collect_generic_params,
            visitor::{TyVisitable, TyVisitor, walk_ty},
        },
    },
    core::semantic::EffectRequirement,
    hir_def::{
        CallableDef, GenericArgListId, GenericParamOwner, IdentId, PathId, PathKind, Trait,
        scope_graph::ScopeId,
    },
};
use common::indexmap::IndexMap;
use rustc_hash::FxHashMap;

pub fn effect_requirement_decls_for_callable<'db>(
    db: &'db dyn HirAnalysisDb,
    callable_def: CallableDef<'db>,
) -> SmallVec<[EffectRequirementDecl<'db>; 2]> {
    let bindings: &[EffectRequirement<'db>] = match callable_def {
        CallableDef::Func(func) => func.effective_effect_requirements(db),
        CallableDef::VariantCtor(_) => &[],
    };
    bindings
        .iter()
        .filter_map(|binding| EffectRequirementDecl::from_effect_requirement(db, binding))
        .collect()
}

pub fn build_effect_query_for_call<'db>(
    tc: &mut TyChecker<'db>,
    callable: &Callable<'db>,
    req: &EffectRequirementDecl<'db>,
) -> Option<EffectQuery<'db>> {
    // Call queries are pattern keys: they may carry existential slots for omitted
    // explicit args and hidden layout holes, but `Precise` queries must not retain
    // ordinary unresolved inference, projections, or invalid state after construction.
    let key = match &req.key {
        EffectRequirementKey::Type(schema) => {
            let carrier =
                instantiate_type_effect_key(tc.db, schema.carrier, callable.generic_args())
                    .fold_with(tc.db, &mut tc.table);
            let carrier = normalize_effect_identity_ty(
                tc.db,
                carrier,
                tc.env.scope(),
                tc.env.assumptions(),
                callable.trait_inst(),
            );
            EffectPatternKey::Type(type_pattern_key_from_carrier(tc.db, carrier, []))
        }
        EffectRequirementKey::Trait(schema) => {
            let key = instantiate_trait_effect_key(
                tc.db,
                schema.clone().into_trait_inst(tc.db),
                callable.generic_args(),
                true,
                None,
            )
            .fold_with(tc.db, &mut tc.table);
            let key = normalize_effect_identity_trait(
                tc.db,
                key,
                tc.env.scope(),
                tc.env.assumptions(),
                callable.trait_inst(),
            );
            EffectPatternKey::Trait(trait_pattern_key_from_inst(tc.db, key, []))
        }
    };

    Some(EffectQuery {
        binding_idx: req.binding_idx,
        required_mut: req.required_mut,
        mode: classify_effect_query_mode(tc.db, &key),
        key,
    })
}

pub fn build_pattern_from_requirement_decl<'db>(
    db: &'db dyn HirAnalysisDb,
    req: &EffectRequirementDecl<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> EffectPatternKey<'db> {
    match &req.key {
        EffectRequirementKey::Type(schema) => {
            let carrier =
                normalize_effect_identity_ty(db, schema.carrier, scope, assumptions, None);
            EffectPatternKey::Type(type_pattern_key_from_carrier(db, carrier, []))
        }
        EffectRequirementKey::Trait(schema) => {
            let key = normalize_effect_identity_trait(
                db,
                schema.clone().into_trait_inst(db),
                scope,
                assumptions,
                None,
            );
            EffectPatternKey::Trait(trait_pattern_key_from_inst(db, key, []))
        }
    }
}

pub fn build_barrier_pattern_for_with_key<'db>(
    tc: &mut TyChecker<'db>,
    key_path: PathId<'db>,
) -> Option<EffectPatternKey<'db>> {
    build_barrier_pattern_for_key_in_scope(tc.db, tc.env.scope(), tc.env.assumptions(), key_path)
}

pub fn build_conservative_same_family_barrier_pattern_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    key_path: PathId<'db>,
) -> Option<EffectPatternKey<'db>> {
    // Conservative barriers are shadow-only same-family fallbacks when an exact invalid
    // keyed barrier is not precise. They preserve only the normalized top-level family and
    // intentionally over-approximate within that family for both ADT-backed and non-ADT
    // type families.
    build_conservative_invalid_type_key_barrier_pattern_in_scope(db, scope, assumptions, key_path)
        .or_else(|| {
            build_conservative_invalid_trait_key_barrier_pattern_in_scope(
                db,
                scope,
                assumptions,
                key_path,
            )
        })
}

fn build_conservative_invalid_type_key_barrier_pattern_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    key_path: PathId<'db>,
) -> Option<EffectPatternKey<'db>> {
    let ty = match resolve_effect_key(db, key_path, scope, assumptions) {
        super::ResolvedEffectKey::Type(schema) => Some(schema.carrier),
        super::ResolvedEffectKey::Trait(_) => None,
        super::ResolvedEffectKey::Invalid | super::ResolvedEffectKey::Other => {
            match resolve_path(db, key_path, scope, assumptions, false).ok()? {
                PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => Some(ty),
                _ => None,
            }
        }
    }?;
    let (base, args) = ty.decompose_ty_app(db);
    let spec = generic_param_owner_for_effect_family_base(db, base).map_or_else(
        || {
            ConservativeTypeBarrierSpec::FromExistingArgs(
                args.iter().copied().collect::<SmallVec<[TyId<'db>; 2]>>(),
            )
        },
        ConservativeTypeBarrierSpec::FromParamOwner,
    );
    let (wildcard_args, wildcard_slots) =
        wildcard_args_and_slots_for_conservative_type_barrier(db, scope, spec);
    let carrier = TyId::foldl(db, base, &wildcard_args);
    let carrier = normalize_effect_identity_ty(db, carrier, scope, assumptions, None);
    Some(EffectPatternKey::Type(type_pattern_key_from_carrier(
        db,
        carrier,
        wildcard_slots,
    )))
}

enum ConservativeTypeBarrierSpec<'db> {
    FromParamOwner(GenericParamOwner<'db>),
    FromExistingArgs(SmallVec<[TyId<'db>; 2]>),
}

fn wildcard_args_and_slots_for_conservative_type_barrier<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    spec: ConservativeTypeBarrierSpec<'db>,
) -> (SmallVec<[TyId<'db>; 2]>, SmallVec<[PatternSlot<'db>; 2]>) {
    match spec {
        ConservativeTypeBarrierSpec::FromParamOwner(owner) => collect_generic_params(db, owner)
            .explicit_params(db)
            .iter()
            .copied()
            .enumerate()
            .map(|(idx, param)| wildcard_pattern_slot_for_source(db, scope, idx, param))
            .unzip(),
        ConservativeTypeBarrierSpec::FromExistingArgs(args) => args
            .into_iter()
            .enumerate()
            .map(|(idx, arg)| wildcard_pattern_slot_for_source(db, scope, idx, arg))
            .unzip(),
    }
}

fn generic_param_owner_for_effect_family_base<'db>(
    db: &'db dyn HirAnalysisDb,
    base: TyId<'db>,
) -> Option<GenericParamOwner<'db>> {
    match base.data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => adt.as_generic_param_owner(db),
        TyData::TyBase(TyBase::Func(func)) => match func {
            CallableDef::Func(def) => Some((*def).into()),
            CallableDef::VariantCtor(_) => None,
        },
        _ => None,
    }
}

fn build_conservative_invalid_trait_key_barrier_pattern_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    key_path: PathId<'db>,
) -> Option<EffectPatternKey<'db>> {
    let trait_ = resolve_trait_symbol_ignoring_args(db, key_path, scope, assumptions)?;
    let (args_no_self, slots) = trait_
        .params(db)
        .iter()
        .copied()
        .skip(1)
        .enumerate()
        .map(|(idx, param)| wildcard_pattern_slot_for_source(db, scope, idx, param))
        .unzip::<_, _, SmallVec<[TyId<'db>; 2]>, SmallVec<[crate::analysis::ty::effects::PatternSlot<'db>; 2]>>();
    Some(EffectPatternKey::Trait(trait_pattern_key_from_inst(
        db,
        TraitInstId::new(
            db,
            trait_,
            std::iter::once(TyId::invalid(db, InvalidCause::Other))
                .chain(args_no_self.iter().copied())
                .collect::<Vec<_>>(),
            IndexMap::new(),
        ),
        slots,
    )))
}

fn wildcard_const_fallback_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<TyId<'db>> {
    match ty.data(db) {
        TyData::ConstTy(const_ty) => Some(match const_ty.data(db) {
            ConstTyData::TyParam(_, fallback_ty) | ConstTyData::TyVar(_, fallback_ty) => {
                *fallback_ty
            }
            ConstTyData::Hole(hole_ty, _) => layout_hole_fallback_ty(db, *hole_ty),
            ConstTyData::Evaluated(_, fallback_ty) | ConstTyData::Abstract(_, fallback_ty) => {
                *fallback_ty
            }
            ConstTyData::UnEvaluated {
                ty: Some(fallback_ty),
                ..
            } => *fallback_ty,
            ConstTyData::UnEvaluated { ty: None, .. } => const_ty.ty(db),
        }),
        _ => ty.invalid_cause(db).and_then(|cause| match cause {
            InvalidCause::ConstTyExpected { expected } => Some(expected),
            _ => None,
        }),
    }
}

fn wildcard_pattern_slot_for_source<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    idx: usize,
    source: TyId<'db>,
) -> (TyId<'db>, PatternSlot<'db>) {
    let name = IdentId::new(db, format!("__invalid_effect_key_{idx}"));
    let (placeholder, fallback_ty) =
        if let Some(fallback_ty) = wildcard_const_fallback_ty(db, source) {
            let implicit = TyParam::implicit_param(name, idx, fallback_ty.kind(db).clone(), scope);
            (
                TyId::new(
                    db,
                    TyData::ConstTy(ConstTyId::new(
                        db,
                        ConstTyData::TyParam(implicit, fallback_ty),
                    )),
                ),
                fallback_ty,
            )
        } else {
            let implicit = TyParam::implicit_param(name, idx, source.kind(db).clone(), scope);
            let placeholder = implicit.ty(db);
            (placeholder, placeholder)
        };
    (
        placeholder,
        PatternSlot {
            id: crate::analysis::ty::effects::PatternSlotId(idx as u32),
            kind: crate::analysis::ty::effects::PatternSlotKind::OmittedExplicitArg,
            placeholder,
            fallback_ty,
        },
    )
}

pub fn build_barrier_pattern_for_key_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    key_path: PathId<'db>,
) -> Option<EffectPatternKey<'db>> {
    match resolve_effect_key(db, key_path, scope, assumptions) {
        super::ResolvedEffectKey::Type(schema) => Some(EffectPatternKey::Type(
            build_type_barrier_pattern_key(db, scope, assumptions, schema.carrier),
        )),
        super::ResolvedEffectKey::Trait(schema) => Some(EffectPatternKey::Trait(
            build_trait_barrier_pattern_key(db, scope, assumptions, schema.into_trait_inst(db)),
        )),
        super::ResolvedEffectKey::Invalid | super::ResolvedEffectKey::Other => {
            match resolve_path(db, key_path, scope, assumptions, false).ok()? {
                crate::analysis::name_resolution::PathRes::Ty(ty)
                | crate::analysis::name_resolution::PathRes::TyAlias(_, ty) => {
                    Some(EffectPatternKey::Type(build_type_barrier_pattern_key(
                        db,
                        scope,
                        assumptions,
                        ty,
                    )))
                }
                crate::analysis::name_resolution::PathRes::Trait(trait_inst) => {
                    Some(EffectPatternKey::Trait(build_trait_barrier_pattern_key(
                        db,
                        scope,
                        assumptions,
                        trait_inst,
                    )))
                }
                _ => None,
            }
        }
    }
}

fn build_type_barrier_pattern_key<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    ty: TyId<'db>,
) -> TypePatternKey<'db> {
    let (carrier, omitted_slots) = rewrite_omitted_type_args_for_pattern_key(db, scope, ty);
    let carrier = normalize_effect_identity_ty(db, carrier, scope, assumptions, None);
    type_pattern_key_from_carrier(db, carrier, omitted_slots)
}

fn build_trait_barrier_pattern_key<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_inst: TraitInstId<'db>,
) -> TraitPatternKey<'db> {
    let (key, omitted_slots) = rewrite_omitted_type_args_for_pattern_key(db, scope, trait_inst);
    let key = normalize_effect_identity_trait(db, key, scope, assumptions, None);
    trait_pattern_key_from_inst(db, key, omitted_slots)
}

fn type_pattern_key_from_carrier<'db>(
    db: &'db dyn HirAnalysisDb,
    carrier: TyId<'db>,
    extra_slots: impl IntoIterator<Item = PatternSlot<'db>>,
) -> TypePatternKey<'db> {
    TypePatternKey {
        carrier,
        family: effect_family_for_type(db, carrier),
        slots: PatternSlots::from_value_with_extra(
            db,
            carrier,
            LayoutPlaceholderPolicy::HolesAndImplicitParams,
            extra_slots,
        ),
    }
}

fn trait_pattern_key_from_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    key: TraitInstId<'db>,
    extra_slots: impl IntoIterator<Item = PatternSlot<'db>>,
) -> TraitPatternKey<'db> {
    TraitPatternKey {
        def: key.def(db),
        args_no_self: key.args(db)[1..].iter().copied().collect(),
        assoc_bindings: key.assoc_ty_bindings(db).into_iter().collect(),
        family: effect_family_for_trait(key.def(db)),
        slots: PatternSlots::from_value_with_extra(
            db,
            key,
            LayoutPlaceholderPolicy::HolesAndImplicitParams,
            extra_slots,
        ),
    }
}

fn rewrite_omitted_type_args_for_pattern_key<'db, T>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    value: T,
) -> (
    T,
    SmallVec<[crate::analysis::ty::effects::PatternSlot<'db>; 2]>,
)
where
    T: TyFoldable<'db>,
{
    struct OmittedExplicitArgElaborator<'db> {
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        omitted_slots: SmallVec<[crate::analysis::ty::effects::PatternSlot<'db>; 2]>,
    }

    impl<'db> OmittedExplicitArgElaborator<'db> {
        fn param_fallback_ty(&self, param: TyId<'db>) -> TyId<'db> {
            let TyData::ConstTy(const_ty) = param.data(self.db) else {
                return param;
            };
            match const_ty.data(self.db) {
                ConstTyData::TyParam(_, fallback_ty) | ConstTyData::TyVar(_, fallback_ty) => {
                    *fallback_ty
                }
                ConstTyData::Hole(hole_ty, _) => layout_hole_fallback_ty(self.db, *hole_ty),
                _ => param,
            }
        }

        fn fresh_omitted_placeholder(&self, param: TyId<'db>, slot_idx: usize) -> TyId<'db> {
            let name = IdentId::new(self.db, format!("__omitted_effect_key_{slot_idx}"));
            let fallback_ty = self.param_fallback_ty(param);
            let implicit = TyParam::implicit_param(
                name,
                slot_idx,
                fallback_ty.kind(self.db).clone(),
                self.scope,
            );
            if matches!(param.data(self.db), TyData::ConstTy(..)) {
                TyId::new(
                    self.db,
                    TyData::ConstTy(ConstTyId::new(
                        self.db,
                        ConstTyData::TyParam(implicit, fallback_ty),
                    )),
                )
            } else {
                implicit.ty(self.db)
            }
        }

        fn record_omitted_slot(&mut self, placeholder: TyId<'db>, fallback_ty: TyId<'db>) {
            if self
                .omitted_slots
                .iter()
                .any(|slot| slot.placeholder == placeholder)
            {
                return;
            }
            self.omitted_slots
                .push(crate::analysis::ty::effects::PatternSlot {
                    id: crate::analysis::ty::effects::PatternSlotId(
                        self.omitted_slots.len() as u32,
                    ),
                    kind: crate::analysis::ty::effects::PatternSlotKind::OmittedExplicitArg,
                    placeholder,
                    fallback_ty,
                });
        }

        fn rewrite_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
            let (base, args) = ty.decompose_ty_app(self.db);
            let TyData::TyBase(TyBase::Adt(adt)) = base.data(self.db) else {
                return ty;
            };

            let Some(owner) = adt.as_generic_param_owner(self.db) else {
                return ty;
            };
            let param_set = collect_generic_params(self.db, owner);
            let explicit_param_count = param_set.explicit_param_count(self.db);
            if args.len() >= explicit_param_count {
                return ty;
            }

            let mut completed_args = args.to_vec();
            for explicit_idx in args.len()..explicit_param_count {
                let Some(param) = param_set.param_by_original_idx(self.db, explicit_idx) else {
                    return ty;
                };
                let fallback_ty = self.param_fallback_ty(param);
                let placeholder = self.fresh_omitted_placeholder(param, self.omitted_slots.len());
                completed_args.push(placeholder);
                self.record_omitted_slot(placeholder, fallback_ty);
            }
            TyId::foldl(self.db, base, &completed_args)
        }
    }

    impl<'db> TyFolder<'db> for OmittedExplicitArgElaborator<'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            let ty = match ty.data(db) {
                TyData::TyApp(..) => {
                    let (base, args) = ty.decompose_ty_app(db);
                    let base = base.super_fold_with(db, self);
                    let args = args
                        .iter()
                        .map(|arg| self.fold_ty(db, *arg))
                        .collect::<Vec<_>>();
                    TyId::foldl(db, base, &args)
                }
                _ => ty.super_fold_with(db, self),
            };
            self.rewrite_ty(ty)
        }
    }

    let mut elaborator = OmittedExplicitArgElaborator {
        db,
        scope,
        omitted_slots: SmallVec::new(),
    };
    let value = value.fold_with(db, &mut elaborator);
    (value, elaborator.omitted_slots)
}

pub fn finalize_stored_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: StoredEffectKey<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<StoredEffectKey<'db>> {
    // Stored witnesses are rigid, authoritative keys. Finalization must normalize and
    // rigidify them up front so later lookup can treat them as fixed evidence.
    match key {
        StoredEffectKey::Type(key) => {
            let carrier = normalize_effect_identity_ty(db, key.carrier, scope, assumptions, None);
            let carrier = rigidify_layout_holes_for_storage(db, scope, carrier);
            let key = StoredTypeKey {
                carrier,
                family: effect_family_for_type(db, carrier),
            };
            stored_type_key_is_rigid(db, key).then_some(StoredEffectKey::Type(key))
        }
        StoredEffectKey::Trait(key) => {
            let key = normalize_stored_trait_key(db, key, scope, assumptions);
            stored_trait_key_is_rigid(db, key.clone()).then_some(StoredEffectKey::Trait(key))
        }
    }
}

pub fn seed_forwarder_from_requirement<'db, P: Copy>(
    tc: &mut TyChecker<'db>,
    req: &EffectRequirementDecl<'db>,
    provider: P,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<EffectForwarder<'db, P>> {
    // Forwarders are body-local requirement schemas with persistent specialization, not
    // rigid witnesses. Instantiate the requirement pattern into the main table once and
    // reuse those hidden vars across the body.
    let pattern = build_pattern_from_requirement_decl(tc.db, req, scope, assumptions);
    let (key, transport) = match pattern {
        EffectPatternKey::Type(pattern) => (
            ForwardedEffectKey::Type(ForwardedTypeKey {
                carrier: instantiate_type_pattern_in(tc.db, &mut tc.table, pattern.clone()),
                family: pattern.family,
            }),
            WitnessTransport::Direct,
        ),
        EffectPatternKey::Trait(pattern) => {
            let instantiated = instantiate_trait_pattern_in(tc.db, &mut tc.table, pattern.clone());
            (
                ForwardedEffectKey::Trait(ForwardedTraitKey {
                    def: instantiated.def(tc.db),
                    args_no_self: instantiated.args(tc.db)[1..].iter().copied().collect(),
                    assoc_bindings: instantiated
                        .assoc_type_bindings(tc.db)
                        .iter()
                        .map(|(&name, &ty)| (name, ty))
                        .collect(),
                    family: pattern.family,
                }),
                WitnessTransport::ByValue,
            )
        }
    };
    if !key.is_well_formed(tc.db) {
        return None;
    }
    Some(EffectForwarder {
        key,
        provider,
        transport,
    })
}

fn normalize_stored_trait_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: StoredTraitKey<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> StoredTraitKey<'db> {
    let args_no_self = key
        .args_no_self
        .into_iter()
        .map(|ty| {
            let ty = normalize_effect_identity_ty(db, ty, scope, assumptions, None);
            rigidify_layout_holes_for_storage(db, scope, ty)
        })
        .collect();
    let mut assoc_bindings: SmallVec<[(crate::hir_def::IdentId<'db>, TyId<'db>); 2]> = key
        .assoc_bindings
        .into_iter()
        .map(|(name, ty)| {
            let ty = normalize_effect_identity_ty(db, ty, scope, assumptions, None);
            (name, rigidify_layout_holes_for_storage(db, scope, ty))
        })
        .collect();
    assoc_bindings.sort_by_key(|(lhs, _)| *lhs);
    StoredTraitKey {
        def: key.def,
        args_no_self,
        assoc_bindings,
        family: effect_family_for_trait(key.def),
    }
}

fn rigidify_layout_holes_for_storage<'db, T>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    value: T,
) -> T
where
    T: TyFoldable<'db>,
{
    struct Folder<'db> {
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        next_idx: usize,
        replacements: FxHashMap<TyId<'db>, TyId<'db>>,
    }

    impl<'db> TyFolder<'db> for Folder<'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            if let Some(replacement) = self.replacements.get(&ty).copied() {
                return replacement;
            }
            let TyData::ConstTy(const_ty) = ty.data(db) else {
                return ty.super_fold_with(db, self);
            };
            let ConstTyData::Hole(hole_ty, _) = const_ty.data(db) else {
                return ty.super_fold_with(db, self);
            };

            let idx = self.next_idx;
            self.next_idx += 1;
            let param = TyParam::implicit_param(
                IdentId::new(self.db, format!("__effect_key_{idx}")),
                idx,
                hole_ty.kind(db).clone(),
                self.scope,
            );
            let replacement = TyId::const_ty(
                db,
                ConstTyId::new(db, ConstTyData::TyParam(param, *hole_ty)),
            );
            self.replacements.insert(ty, replacement);
            replacement
        }
    }

    value.fold_with(
        db,
        &mut Folder {
            db,
            scope,
            next_idx: 0,
            replacements: FxHashMap::default(),
        },
    )
}

fn resolve_trait_symbol_ignoring_args<'db>(
    db: &'db dyn HirAnalysisDb,
    key_path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<Trait<'db>> {
    let parent_scope = if let Some(parent) = key_path.parent(db) {
        resolve_path(db, parent.strip_generic_args(db), scope, assumptions, false)
            .ok()?
            .as_scope(db)?
    } else {
        scope
    };
    let ident = key_path.ident(db).to_opt()?;
    let query_path = PathId::new(
        db,
        PathKind::Ident {
            ident: Some(ident).into(),
            generic_args: GenericArgListId::none(db),
        },
        None,
    );
    let mut traits = resolve_ident_to_bucket(db, query_path, parent_scope)
        .iter_ok()
        .filter(|res| res.is_visible(db, parent_scope))
        .filter_map(|res| res.trait_());
    let trait_ = traits.next()?;
    traits.next().is_none().then_some(trait_)
}

pub fn classify_effect_query_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    key: &EffectPatternKey<'db>,
) -> EffectQueryMode {
    // A query is precise only if the query itself contains no unresolved ordinary
    // inference, projections, or invalid state outside its pattern slots.
    if query_contains_unresolved_inference(db, key)
        || contains_projection_or_invalid_query_state(db, key)
    {
        EffectQueryMode::FamilyFallback
    } else {
        EffectQueryMode::Precise
    }
}

pub(crate) fn query_contains_unresolved_inference<'db>(
    db: &'db dyn HirAnalysisDb,
    key: &EffectPatternKey<'db>,
) -> bool {
    let inference = match key {
        EffectPatternKey::Type(key) => inference_keys(db, &key.carrier),
        EffectPatternKey::Trait(key) => {
            let mut tys = key.args_no_self.to_vec();
            tys.extend(key.assoc_bindings.iter().map(|(_, ty)| *ty));
            inference_keys(db, &tys)
        }
    };
    !inference.is_empty()
}

pub(crate) fn contains_projection_or_invalid_query_state<'db>(
    db: &'db dyn HirAnalysisDb,
    key: &EffectPatternKey<'db>,
) -> bool {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_assoc_ty(&mut self, _: &crate::analysis::ty::ty_def::AssocTy<'db>) {
            self.found = true;
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }
            if let Some(cause) = ty.invalid_cause(self.db) {
                if matches!(
                    cause,
                    crate::analysis::ty::ty_def::InvalidCause::ConstTyExpected { .. }
                ) {
                    walk_ty(self, ty);
                    return;
                }
                self.found = true;
                return;
            }
            walk_ty(self, ty);
        }
    }

    let mut finder = Finder { db, found: false };
    match key {
        EffectPatternKey::Type(key) => key.carrier.visit_with(&mut finder),
        EffectPatternKey::Trait(key) => {
            for ty in &key.args_no_self {
                ty.visit_with(&mut finder);
            }
            for (_, ty) in &key.assoc_bindings {
                ty.visit_with(&mut finder);
            }
        }
    }
    finder.found
}
