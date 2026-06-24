use super::{
    EffectProviderSubst, GenericSubst, ImplEnv, SemanticInstance, SemanticInstanceKey,
    provisional_provider_binding_for_instance_effect, provisional_provider_idx_for_requirement,
    resolved_effect_binding_ty_for_instance_effect, resolved_provider_binding_for_instance_effect,
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{SemOrigin, SemanticConstRef},
        ty::{
            assoc_const::{AssocConstUse, InherentConstUse},
            const_ty::inherent_const_body_and_impl_args,
            effects::place_effect_provider_param_index_map,
            trait_def::{
                ImplementorId, TraitInstId, assoc_const_body_and_impl_args_for_trait_inst,
                resolve_trait_method_instance, resolve_trait_method_instance_with_implementor,
            },
            trait_resolution::{PredicateListId, ProvisionEnv},
            ty_check::{
                BodyOwner, Callable, ConstRef, DischargeRoute, EffectParamSite,
                EffectProviderSpecialization, TypedBody,
            },
            ty_def::TyId,
            ty_lower::instantiate_callable_effect_layout_args,
        },
    },
    core::semantic::{EffectEnvView, ProviderBinding},
    hir_def::{CallableDef, Const, ExprId},
};
use common::indexmap::IndexSet;
use rustc_hash::FxHashMap;

#[derive(Clone, Copy)]
enum ProviderResolutionMode {
    Final,
    Provisional,
}

pub(crate) fn semantic_callee_key_with_effect_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    callable: &Callable<'db>,
    effect_providers: &[EffectProviderSpecialization<'db>],
    call_expr: Option<ExprId>,
) -> Option<SemanticInstanceKey<'db>> {
    let assumptions = SemanticInstance::new(db, caller_key).assumptions(db);
    semantic_callee_key_with_assumptions(
        db,
        caller_key,
        callable,
        effect_providers,
        assumptions,
        ProviderResolutionMode::Final,
        call_expr,
    )
}

pub(crate) fn provisional_semantic_callee_key<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    callable: &Callable<'db>,
    assumptions: PredicateListId<'db>,
    call_expr: Option<ExprId>,
) -> Option<SemanticInstanceKey<'db>> {
    semantic_callee_key_with_assumptions(
        db,
        caller_key,
        callable,
        callable.effect_providers(),
        assumptions,
        ProviderResolutionMode::Provisional,
        call_expr,
    )
}

fn semantic_callee_key_with_assumptions<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    callable: &Callable<'db>,
    effect_providers: &[EffectProviderSpecialization<'db>],
    assumptions: PredicateListId<'db>,
    provider_resolution_mode: ProviderResolutionMode,
    call_expr: Option<ExprId>,
) -> Option<SemanticInstanceKey<'db>> {
    let impl_env = caller_key.impl_env(db);
    // The per-goal impl overrides the typeck solver committed to at this
    // instantiation-time resolution, carried onto the instance's `ImplEnv` below.
    // EMPTY unless the call was SCOPE-SELECTED (`with (<T as Trait>)`). Two
    // sources populate it:
    //
    //   * the TRAIT-METHOD callee (`trait_inst().is_some()`) discharged its own
    //     trait obligation FROM an in-scope scoped provision — one `(inst, impl)`
    //     entry (cascade C3d, the direct-receiver / M2 case);
    //   * the GENERIC-HELPER callee (`trait_inst().is_none()`, the `else` branch
    //     below) carried its CALLER's scoped-provision discharges for THIS call
    //     site — one `(goal, impl)` entry per goal a `with (<T as Trait>)` scope
    //     selected, so the helper body's inner trait calls each look their
    //     override up BY GOAL (M3).
    //
    // Under the empty-only `ImplEnv` identity (see the IDENTITY INVARIANT on
    // `ImplEnv`), an EMPTY carry is byte-identical to the pre-cascade behavior,
    // while a NON-EMPTY carry makes the scope selection observable (a distinct
    // key/symbol consumed by the MIR C1 rail).
    let mut selected_implementors: Vec<(TraitInstId<'db>, ImplementorId<'db>)> = Vec::new();
    let (owner, mut subst_args) = match callable.callable_def() {
        CallableDef::Func(func) => {
            let mut subst_args = callable.generic_args().to_vec();
            let owner = if let Some(inst) = callable.trait_inst()
                && let Some(name) = func.name(db).to_opt()
            {
                let solve_cx = ProvisionEnv::for_scope(
                    impl_env.normalization_scope(db),
                    assumptions,
                )
                .solve_cx(db);
                // The typeck twin of the MIR `selected_implementor` source
                // (cascade C3d/M3). `Some(override)` from one of two sources:
                //
                //   * the caller discharged THIS call's trait obligation FROM an
                //     in-scope scoped provision (`with (<T as Trait>)`), readable
                //     in the caller's own typed body keyed to `call_expr` (the
                //     direct-receiver / M2 case); OR
                //   * the CALLER INSTANCE already carries an override for this
                //     goal `inst` on its `ImplEnv` per-goal carrier (M3): this is
                //     how a scoped selection reaches an inner trait call that is
                //     resolved to a concrete impl func at instance-build time
                //     (e.g. `x.method()` inside a generic helper whose `T` is now
                //     concrete). The helper instance got that carrier from its own
                //     call site's scoped discharges (the `else` branch below), so
                //     the override propagates through the helper boundary into this
                //     inner resolution.
                //
                // When present, the call's instance becomes identity-distinct
                // (empty-only `ImplEnv` identity — see `template.rs`) AND its body
                // is resolved FROM the override (not the default-tier re-solve), so
                // the override runs inside the scope.
                //
                // `None` otherwise (every non-scope-selected call): the instance
                // is BYTE-IDENTICAL to the pre-cascade behavior (empty-only
                // identity makes an empty env hash/compare/serialize exactly as
                // before) and the body comes from the ordinary impl-table re-solve
                // — today the deterministic default tier for a 1-impl or
                // derive-default goal. This default keeps EVERY existing
                // instance/symbol unchanged.
                let scoped = call_expr
                    .and_then(|call_expr| {
                        scoped_provision_implementor(caller_key.typed_body(db), call_expr, inst)
                    })
                    .or_else(|| impl_env.selected_implementor_for_goal(db, inst));
                // Resolve the body: from the scope-named override when present,
                // else through the ordinary impl-table re-solve. Both reconstruct
                // an identical `ResolvedTraitMethod` shape; only the implementor
                // SOURCE differs (the cascade C3d twin of `classify.rs`).
                let resolved = match scoped {
                    Some(override_impl) => resolve_trait_method_instance_with_implementor(
                        db, solve_cx, inst, name, override_impl,
                    ),
                    None => resolve_trait_method_instance(db, solve_cx, inst, name),
                };
                if let Some(resolved) = resolved {
                    if let Some(override_impl) = scoped {
                        selected_implementors.push((inst, override_impl));
                    }
                    let trait_arg_len = inst.args(db).len();
                    let mut resolved_args = resolved.impl_args;
                    let tail = subst_args
                        .get(trait_arg_len..)
                        .unwrap_or(subst_args.as_slice());
                    resolved_args.extend_from_slice(tail);
                    subst_args = resolved_args;
                    BodyOwner::Func(resolved.func)
                } else {
                    BodyOwner::Func(func)
                }
            } else {
                // GENERIC HELPER (M3): the callee is a plain `fn` with a trait
                // bound (`trait_inst() == None`), so its body's inner `x.method()`
                // calls will RE-RESOLVE through the trait solver unless we carry
                // the scope selection onto this helper instance. Read THIS call
                // site's scoped-provision discharges — the helper's `T: Trait`
                // bound, discharged FROM the in-scope `with (<T as Trait>)`
                // provision and recorded keyed to `call_expr` — and carry each
                // `(goal, override)` onto the helper's `ImplEnv`. The MIR C1 rail
                // then looks each override up BY GOAL when lowering the inner call
                // (`classify.rs`), so the scope-selected impl propagates through
                // the helper boundary. Each entry is still validated by the C1
                // rail's `recorded_implementor_is_valid_candidate` backstop.
                //
                // EMPTY when the helper is called outside any selecting scope
                // (every non-cascade call) — byte-identical to the pre-M3 instance
                // (empty-only `ImplEnv` identity — see `template.rs`).
                if let Some(call_expr) = call_expr {
                    selected_implementors
                        .extend(scoped_provision_implementors(caller_key.typed_body(db), call_expr));
                }
                BodyOwner::Func(func)
            };
            (owner, subst_args)
        }
        CallableDef::VariantCtor(_) => return None,
    };
    let effect_providers = resolve_callable_effect_providers(
        db,
        caller_key,
        owner,
        &mut subst_args,
        effect_providers,
        provider_resolution_mode,
    );

    let mut witnesses: IndexSet<_> = impl_env.witnesses(db).iter().copied().collect();
    if let Some(witness) = callable.trait_inst() {
        witnesses.insert(witness);
    }
    // Carry the instantiation-time-selected implementors into the semantic
    // instance. Under the empty-only `ImplEnv` identity, an EMPTY carry leaves the
    // `SemanticInstanceKey` byte-identical to before (every non-scope-selected
    // call), while a NON-EMPTY carry (a `with (<T as Trait>)` scoped call) mints a
    // distinct key/symbol so MIR's C1 rail lowers it against the selected impl(s);
    // rung 3.3 asserts MIR re-resolution agrees on the empty (re-resolve) path.
    // `with_selected_implementors` canonically orders the carrier so equal
    // selection sets mint equal keys.
    let impl_env = ImplEnv::new(
        db,
        impl_env.normalization_scope(db),
        assumptions,
        witnesses.into_iter().collect::<Vec<_>>(),
    )
    .with_selected_implementors(selected_implementors);

    Some(SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, subst_args),
        EffectProviderSubst::new(db, effect_providers),
        impl_env,
    ))
}

/// The implementor a scoped provision named for THIS call's trait obligation,
/// if the caller discharged that obligation via [`DischargeRoute::ScopedProvision`]
/// (cascade C3d — the typeck twin of the MIR `selected_implementor` source).
///
/// Reads the caller's `discharged_obligations` for `call_expr` and returns the
/// implementor of the `ScopedProvision`-routed discharge whose goal is the
/// callable's originating trait instance `inst`. Returns `None` when no such
/// discharge exists — the call was not scope-selected, so the instance carries
/// `None` (byte-identical to the pre-cascade behavior; MIR re-resolves). Returns
/// `Some(override)` for a DIRECT receiver call inside a `with (<T as Trait>)`
/// block: the gate registers a call-keyed `MethodSelection` obligation (cascade
/// C3d), so its `ScopedProvision` discharge is readable here per call
/// (`discharged_obligations_for_call`) and the named override is carried into the
/// instance, making the scope selection observable at runtime.
fn scoped_provision_implementor<'db>(
    typed_body: &TypedBody<'db>,
    call_expr: ExprId,
    inst: TraitInstId<'db>,
) -> Option<ImplementorId<'db>> {
    typed_body
        .discharged_obligations_for_call(call_expr)
        .find(|discharged| {
            discharged.route == DischargeRoute::ScopedProvision && discharged.goal == inst
        })
        .and_then(|discharged| discharged.solution)
        .map(|solution| solution.implementor)
}

/// Every `(goal, override)` a scoped provision named for THIS call's trait
/// obligations, when the caller discharged them via
/// [`DischargeRoute::ScopedProvision`] (M3 — the generic-helper twin of the MIR
/// per-goal carrier).
///
/// Reads the caller's `discharged_obligations` for `call_expr` and yields the
/// `(goal, implementor)` of EACH `ScopedProvision`-routed discharge that
/// committed a concrete implementor. For a `with (<T as Trait>) { helper(x) }`
/// call, the helper's `T: Trait` bound is enqueued as a call-keyed
/// `CallConstraint` obligation (one per bound) and discharged from the in-scope
/// provision, so its `ScopedProvision` discharge is readable here per call
/// (`discharged_obligations_for_call`); the named override is carried onto the
/// helper instance's `ImplEnv` so the helper body's inner trait call resolves it
/// BY GOAL through the MIR C1 rail. Yields NOTHING when the helper is called
/// outside any selecting scope — the instance carries an empty carrier
/// (byte-identical to the pre-M3 behavior; MIR re-resolves).
fn scoped_provision_implementors<'a, 'db>(
    typed_body: &'a TypedBody<'db>,
    call_expr: ExprId,
) -> impl Iterator<Item = (TraitInstId<'db>, ImplementorId<'db>)> + 'a {
    typed_body
        .discharged_obligations_for_call(call_expr)
        .filter(|discharged| discharged.route == DischargeRoute::ScopedProvision)
        .filter_map(|discharged| {
            discharged
                .solution
                .map(|solution| (discharged.goal, solution.implementor))
        })
}

fn resolve_callable_effect_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    owner: BodyOwner<'db>,
    subst_args: &mut [TyId<'db>],
    effect_providers: &[EffectProviderSpecialization<'db>],
    provider_resolution_mode: ProviderResolutionMode,
) -> Vec<EffectProviderSpecialization<'db>> {
    let caller = SemanticInstance::new(db, caller_key);
    let mut providers = effect_providers
        .iter()
        .map(|specialization| {
            let provider_idx = specialization.provider.provider_idx;
            let provider = match specialization.provenance {
                crate::analysis::ty::ty_check::EffectProviderProvenance::Binding {
                    binding,
                    ..
                } => provider_resolution_mode
                    .resolve_binding(db, caller, binding)
                    .filter(|provider| {
                        provider.effective_target_ty()
                            == specialization.provider.effective_target_ty()
                    })
                    .map(|provider| crate::semantic::ProviderBinding {
                        provider_idx,
                        ..provider
                    })
                    .unwrap_or(specialization.provider.clone()),
                crate::analysis::ty::ty_check::EffectProviderProvenance::Expr { .. } => {
                    specialization.provider.clone()
                }
            };
            EffectProviderSpecialization {
                provider,
                provenance: specialization.provenance,
            }
        })
        .collect::<Vec<_>>();
    providers.sort_by_key(|provider| provider.provider.provider_idx);
    if let BodyOwner::Func(func) = owner {
        let effect_env = EffectEnvView::new(EffectParamSite::Func(func));
        let resolution_by_req = match provider_resolution_mode {
            ProviderResolutionMode::Final => effect_env
                .resolutions(db)
                .into_iter()
                .map(|resolution| (resolution.requirement_idx as usize, resolution.provider_idx))
                .collect::<FxHashMap<_, _>>(),
            ProviderResolutionMode::Provisional => effect_env
                .requirements(db)
                .into_iter()
                .filter_map(|requirement| {
                    provisional_provider_idx_for_requirement(
                        db,
                        EffectParamSite::Func(func),
                        requirement.binding_idx,
                    )
                    .map(|provider_idx| (requirement.binding_idx as usize, provider_idx))
                })
                .collect::<FxHashMap<_, _>>(),
        };
        let provider_by_idx = providers
            .iter()
            .map(|provider| (provider.provider.provider_idx, provider))
            .collect::<FxHashMap<_, _>>();
        for (effect_idx, param_idx) in place_effect_provider_param_index_map(db, func)
            .iter()
            .enumerate()
            .filter_map(|(effect_idx, param_idx)| {
                param_idx.map(|param_idx| (effect_idx, param_idx))
            })
        {
            let Some(provider_idx) = resolution_by_req.get(&effect_idx).copied() else {
                continue;
            };
            let Some(provider) = provider_by_idx.get(&provider_idx) else {
                continue;
            };
            if let Some(slot) = subst_args.get_mut(param_idx) {
                *slot = provider.provider.provider_ty;
            }
            let actual_key_ty = effect_provider_target_ty(db, caller, provider);
            instantiate_callable_effect_layout_args(
                db,
                func,
                effect_idx,
                actual_key_ty,
                subst_args,
            );
        }
    }
    providers
}

fn effect_provider_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: SemanticInstance<'db>,
    provider: &EffectProviderSpecialization<'db>,
) -> TyId<'db> {
    let fallback = provider.provider.effective_target_ty();
    let crate::analysis::ty::ty_check::EffectProviderProvenance::Binding { binding, .. } =
        provider.provenance
    else {
        return fallback;
    };
    resolved_effect_binding_ty_for_instance_effect(db, caller, binding).unwrap_or(fallback)
}

impl ProviderResolutionMode {
    fn resolve_binding<'db>(
        self,
        db: &'db dyn HirAnalysisDb,
        caller: SemanticInstance<'db>,
        binding: crate::analysis::ty::ty_check::LocalBinding<'db>,
    ) -> Option<ProviderBinding<'db>> {
        match self {
            Self::Final => resolved_provider_binding_for_instance_effect(db, caller, binding),
            Self::Provisional => {
                provisional_provider_binding_for_instance_effect(db, caller, binding)
            }
        }
    }
}

pub(crate) fn resolve_semantic_const_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ref: ConstRef<'db>,
    ty: TyId<'db>,
    origin: SemOrigin<'db>,
) -> Option<SemanticConstRef<'db>> {
    let instance = match const_ref {
        ConstRef::Const(const_) => semantic_const_key_for_const(db, const_),
        ConstRef::TraitConst(assoc) => semantic_const_key_for_assoc_const(db, assoc, ty),
        ConstRef::InherentConst(use_) => semantic_const_key_for_inherent_const(db, use_, ty),
    }?;
    Some(SemanticConstRef::new(db, instance, ty, origin))
}

fn semantic_const_key_for_const<'db>(
    db: &'db dyn HirAnalysisDb,
    const_: Const<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let owner = BodyOwner::Const(const_);
    Some(SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::empty(db),
        EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
    ))
}

fn semantic_const_key_for_inherent_const<'db>(
    db: &'db dyn HirAnalysisDb,
    use_: InherentConstUse<'db>,
    ty: TyId<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let (body, impl_args) =
        inherent_const_body_and_impl_args(db, use_.impl_(), use_.receiver_ty(), use_.name())?;
    Some(SemanticInstanceKey::new(
        db,
        BodyOwner::AnonConstBody { body, expected: ty },
        GenericSubst::new(db, impl_args),
        EffectProviderSubst::empty(db),
        ImplEnv::new(db, use_.origin_scope(), use_.assumptions(), vec![]),
    ))
}

fn semantic_const_key_for_assoc_const<'db>(
    db: &'db dyn HirAnalysisDb,
    assoc: AssocConstUse<'db>,
    ty: TyId<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let (body, impl_args) = assoc_const_body_and_impl_args_for_trait_inst(
        db,
        ProvisionEnv::for_scope(assoc.origin_scope(), assoc.assumptions()).solve_cx(db),
        assoc.inst(),
        assoc.name(),
    )?;
    Some(SemanticInstanceKey::new(
        db,
        BodyOwner::AnonConstBody { body, expected: ty },
        GenericSubst::new(db, impl_args),
        EffectProviderSubst::empty(db),
        ImplEnv::new(
            db,
            assoc.origin_scope(),
            assoc.assumptions(),
            vec![assoc.inst()],
        ),
    ))
}

#[cfg(test)]
mod scoped_provision_const_ref_tests {
    use camino::Utf8PathBuf;
    use cranelift_entity::EntityRef;

    use super::{DischargeRoute, ImplementorId, TraitInstId, scoped_provision_implementor};
    use crate::{
        analysis::{
            name_resolution::{PathRes, resolve_path},
            ty::{
                trait_def::impls_for_trait_def,
                trait_resolution::{PredicateListId, TraitGoalSolution},
                ty_check::{DischargedObligation, TraitObligationOrigin, TypedBody},
                ty_def::TyId,
            },
        },
        hir_def::{CallableDef, ExprId, Func, PathId, TopLevelMod, scope_graph::ScopeId},
        test_db::HirAnalysisTestDb,
    };

    /// A `Point` struct with a real `impl Eq for Point` (the global `Hir` impl a
    /// scoped provision names under cascade C2), an `Eq`-bound `requires_eq` to
    /// own the synthetic caller body, and NO `impl Ord for Point` — so an
    /// `Ord<Point>` discharge can only be hand-built, exercising the goal-specific
    /// arm of the const_ref read.
    const FIXTURE: &str = r#"
use core::ops::Eq
use core::ops::Ord

struct Point {
    x: u256,
    y: u256,
}

impl Eq for Point {
    fn eq(self, _ other: Point) -> bool {
        self.x == other.x
    }
}

fn requires_eq<T: Eq>(_ t: T) {}
"#;

    fn resolve<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod_scope: ScopeId<'db>,
        segments: &[&str],
    ) -> PathRes<'db> {
        let path = PathId::from_segments(db, segments);
        resolve_path(db, path, top_mod_scope, PredicateListId::empty_list(db), false)
            .unwrap_or_else(|e| panic!("expected {segments:?} to resolve, got {e:?}"))
    }

    /// `<subject>: <trait><subject>` (e.g. `Point: Eq<Point>`) — the saturated
    /// `[Self, T]` arg shape a `T: Eq` constraint instantiates to when `T :=
    /// Point`, identical to the goal a real `CallConstraint` obligation carries.
    fn concrete_inst<'db>(
        db: &'db HirAnalysisTestDb,
        trait_inst: TraitInstId<'db>,
        subject: TyId<'db>,
    ) -> TraitInstId<'db> {
        TraitInstId::new_simple(db, trait_inst.def(db), vec![subject, subject])
    }

    fn find_func<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
        name: &str,
    ) -> Func<'db> {
        top_mod
            .all_funcs(db)
            .iter()
            .copied()
            .find(|f| f.name(db).to_opt().is_some_and(|n| n.data(db) == name))
            .unwrap_or_else(|| panic!("missing `{name}` function"))
    }

    /// The REAL global `Hir` [`ImplementorId`] of the sole `impl <trait_inst.def>`
    /// for `subject` — what a scoped provision names under cascade C2.
    fn sole_implementor<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
        trait_inst: TraitInstId<'db>,
        subject: TyId<'db>,
    ) -> ImplementorId<'db> {
        impls_for_trait_def(db, top_mod.ingot(db), trait_inst.def(db))
            .iter()
            .map(|binder| binder.instantiate_identity())
            .find(|implementor| implementor.self_ty(db) == subject)
            .unwrap_or_else(|| panic!("expected a real impl of the trait for the subject type"))
    }

    /// A `ScopedProvision`-routed call-constraint discharge for `call_expr`,
    /// witnessing `goal` and naming `implementor` — exactly the record a caller
    /// would write when it discharges THIS call's trait obligation from an
    /// in-scope `Evidence` provision (cascade C2). When `route` is `ImplTable`
    /// instead, it is the ordinary trait-solver discharge.
    fn call_discharge<'db>(
        call_expr: ExprId,
        callable_def: CallableDef<'db>,
        goal: TraitInstId<'db>,
        implementor: ImplementorId<'db>,
        route: DischargeRoute,
    ) -> DischargedObligation<'db> {
        DischargedObligation {
            origin: TraitObligationOrigin::CallConstraint {
                call_expr,
                callable_def,
                constraint_idx: 0,
            },
            goal,
            solution: Some(TraitGoalSolution { inst: goal, implementor }),
            route,
        }
    }

    /// Cascade C3d: `scoped_provision_implementor` — the read that feeds
    /// `selected_implementor` in const_ref's typeck twin — returns the REAL
    /// implementor of a `ScopedProvision`-routed discharge keyed to THIS call
    /// whose goal matches the callable's trait inst, never the re-solve. The
    /// negative arms prove it is keyed on call expr, route, AND goal.
    #[test]
    fn scoped_provision_discharge_is_preferred_for_const_ref() {
        let mut db = HirAnalysisTestDb::default();
        let file =
            db.new_stand_alone(Utf8PathBuf::from("scoped_provision_const_ref.fe"), FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let scope = top_mod.scope();

        let PathRes::Trait(eq_inst) = resolve(&db, scope, &["core", "ops", "Eq"]) else {
            panic!("core::ops::Eq must resolve to a trait");
        };
        let PathRes::Trait(ord_inst) = resolve(&db, scope, &["core", "ops", "Ord"]) else {
            panic!("core::ops::Ord must resolve to a trait");
        };
        let PathRes::Ty(point_ty) = resolve(&db, scope, &["Point"]) else {
            panic!("Point must resolve to a type");
        };

        let eq_point = concrete_inst(&db, eq_inst, point_ty);
        let ord_point = concrete_inst(&db, ord_inst, point_ty);
        let eq_point_impl = sole_implementor(&db, top_mod, eq_inst, point_ty);

        let requires_eq = find_func(&db, top_mod, "requires_eq");
        let callable_def = CallableDef::Func(requires_eq);

        // The call expr the discharge is keyed to (synthetic id — the const_ref
        // read only filters on it, never dereferences it against a real body).
        let call_expr = ExprId::new(0);
        let other_expr = ExprId::new(1);

        // POSITIVE: a `ScopedProvision` discharge for `call_expr` whose goal is the
        // callable trait inst `Eq<Point>` is preferred — the read returns the REAL
        // `impl Eq for Point` the provision named.
        let typed_body = TypedBody::with_discharged_obligations_for_test(
            &db,
            vec![call_discharge(
                call_expr,
                callable_def,
                eq_point,
                eq_point_impl,
                DischargeRoute::ScopedProvision,
            )],
        );
        assert_eq!(
            scoped_provision_implementor(&typed_body, call_expr, eq_point),
            Some(eq_point_impl),
            "the const_ref read must record the implementor the scoped provision named",
        );

        // NEGATIVE (goal): the same `ScopedProvision` discharge does NOT answer for
        // an unrelated `Ord<Point>` goal — the read is goal-specific.
        assert_eq!(
            scoped_provision_implementor(&typed_body, call_expr, ord_point),
            None,
            "a scoped discharge must not answer for a different goal",
        );

        // NEGATIVE (call expr): the discharge is keyed to `call_expr`, so a query
        // for a different call expr finds nothing — the caller falls back to the
        // re-solve.
        assert_eq!(
            scoped_provision_implementor(&typed_body, other_expr, eq_point),
            None,
            "a scoped discharge must not answer for a different call expr",
        );

        // NEGATIVE (route): an ordinary `ImplTable` discharge for the same call and
        // goal is NOT a scoped provision — the read ignores it so const_ref falls
        // back to the impl-table re-solve (preserving today's behavior).
        let impl_table_body = TypedBody::with_discharged_obligations_for_test(
            &db,
            vec![call_discharge(
                call_expr,
                callable_def,
                eq_point,
                eq_point_impl,
                DischargeRoute::ImplTable,
            )],
        );
        assert_eq!(
            scoped_provision_implementor(&impl_table_body, call_expr, eq_point),
            None,
            "an ImplTable discharge must not be treated as a scoped provision",
        );
    }
}
