//! This module contains all trait related types definitions.

use crate::{
    analysis::ty::{
        method_cmp::compare_impl_method,
        trait_lower::collect_trait_impls,
        trait_resolution::{GoalSatisfiability, PredicateListId, Selection},
    },
    hir_def::{Contract, Func, HirIngot, IdentId, ImplTrait, Trait, scope_graph::ScopeId},
};
use common::{
    indexmap::{IndexMap, IndexSet},
    ingot::{Ingot, IngotKind},
};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    canonical::Canonical,
    diagnostics::{ImplDiag, TyDiagCollection},
    fold::TyFoldable as _,
    trait_lower::collect_implementor_methods,
    trait_resolution::{
        ProvisionEnv, TraitSolveCx, constraint::collect_constraints, is_goal_satisfiable,
        normalize_trait_inst_preserving_validity,
    },
    ty_def::TyId,
    unify::UnificationTable,
};
use crate::analysis::HirAnalysisDb;

/// Returns [`TraitEnv`] for the given ingot.
#[salsa::tracked(return_ref, cycle_fn=ingot_trait_env_cycle_recover, cycle_initial=ingot_trait_env_cycle_initial)]
pub(crate) fn ingot_trait_env<'db>(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> TraitEnv<'db> {
    TraitEnv::collect(db, ingot)
}

/// Returns all implementors for the given trait definition.
///
/// Note: this intentionally does **not** pre-filter implementors by unifying with a
/// specific goal instance. Projection-heavy goals (e.g. involving associated type
/// projections) often only become unifiable after normalization, and unification
/// rejects unresolved associated types. The solver normalizes before unifying
/// candidates, so any filtering here must be an over-approximation.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_trait_def<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_def: Trait<'db>,
) -> Vec<Binder<ImplementorId<'db>>> {
    let env = ingot_trait_env(db, ingot);
    let mut out = env.impls.get(&trait_def).cloned().unwrap_or_default();

    if is_std_evm_contract_trait_def(db, trait_def) {
        out.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    out
}

/// Returns all implementors for the given trait inst, searching across a
/// deterministic set of ingots.
///
/// This is used to avoid "pick an ingot" footguns where impl lookup depends on
/// the caller's current module ingot and can miss impls that live in either:
/// - the trait's ingot, or
/// - the implementor type's ingot.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_trait_in_ingots<'db>(
    db: &'db dyn HirAnalysisDb,
    primary: Ingot<'db>,
    secondary: Option<Ingot<'db>>,
    trait_: Canonical<TraitInstId<'db>>,
) -> Vec<Binder<ImplementorId<'db>>> {
    let mut table = UnificationTable::new(db);
    let trait_def = trait_.extract_identity(&mut table).def(db);
    let mut dedup: IndexSet<Binder<ImplementorId<'db>>> = IndexSet::default();
    dedup.extend(impls_for_trait_def(db, primary, trait_def).iter().copied());
    if let Some(secondary) = secondary {
        dedup.extend(
            impls_for_trait_def(db, secondary, trait_def)
                .iter()
                .copied(),
        );
    }
    dedup.into_iter().collect()
}

fn is_std_evm_contract_trait_def<'db>(db: &'db dyn HirAnalysisDb, trait_def: Trait<'db>) -> bool {
    let Some(name) = trait_def.name(db).to_opt() else {
        return false;
    };
    if name.data(db) != "Contract" {
        return false;
    }
    if trait_def.top_mod(db).ingot(db).kind(db) != IngotKind::Std {
        return false;
    }
    true
}

#[salsa::tracked(return_ref)]
fn contract_virtual_impls<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> Vec<Binder<ImplementorId<'db>>> {
    let Some(contract_trait) = std_evm_contract_trait_def(db, ingot) else {
        return Vec::new();
    };

    let init_args_ident = IdentId::new(db, "InitArgs".to_string());

    let mut out = Vec::new();
    for top_mod in ingot.all_modules(db) {
        for &contract in top_mod.all_contracts(db).iter() {
            let self_ty = TyId::contract(db, contract);
            let trait_inst = TraitInstId::new(db, contract_trait, vec![self_ty], IndexMap::new());

            let init_args_ty = contract.init_args_ty(db);
            let mut types = IndexMap::new();
            types.insert(init_args_ident, init_args_ty);

            let implementor = ImplementorId::new(
                db,
                trait_inst,
                Vec::new(),
                types,
                ImplementorOrigin::VirtualContract(contract),
            );
            out.push(Binder::bind(implementor));
        }
    }

    out
}

#[salsa::tracked]
fn std_evm_contract_trait_def<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> Option<Trait<'db>> {
    use crate::analysis::name_resolution::resolve_path;
    use common::ingot::IngotKind;

    let scope = ingot.root_mod(db).scope();
    let assumptions = PredicateListId::empty_list(db);

    let std_root = if ingot.kind(db) == IngotKind::Std {
        IdentId::make_ingot(db)
    } else {
        IdentId::new(db, "std".to_string())
    };

    let path = crate::hir_def::PathId::from_ident(db, std_root)
        .push_ident(db, IdentId::new(db, "evm".to_string()))
        .push_ident(db, IdentId::new(db, "Contract".to_string()));

    match resolve_path(db, path, scope, assumptions, false).ok()? {
        crate::analysis::name_resolution::PathRes::Trait(inst) => Some(inst.def(db)),
        _ => None,
    }
}

/// `pub` (rather than `pub(crate)`) because it is the `origin` field of
/// [`ImplementorId`], which is now public provenance carried out of typeck's
/// impl selection (rung 3.2). Salsa's generated accessor for that field would
/// otherwise leak this crate-private type through a public interface (E0446).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ImplementorOrigin<'db> {
    Hir(ImplTrait<'db>),
    VirtualContract(Contract<'db>),
    Assumption,
}

fn ingot_trait_env_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> TraitEnv<'db> {
    // Return an empty trait environment when we detect a cycle
    TraitEnv {
        impls: FxHashMap::default(),
        ty_to_implementors: FxHashMap::default(),
        ingot,
    }
}

fn ingot_trait_env_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &TraitEnv<'db>,
    _count: u32,
    _ingot: Ingot<'db>,
) -> salsa::CycleRecoveryAction<TraitEnv<'db>> {
    // Continue iterating to try to resolve the cycle
    salsa::CycleRecoveryAction::Iterate
}

/// The result of resolving a trait method to its concrete HIR function: the
/// function, the impl's instantiated generic arguments, and the selected
/// implementor.
///
/// `implementor` is provenance carried for rung 3.2 — it records which impl
/// typeck's solver committed to so a later rung (3.3) can assert that MIR
/// re-resolution picks the same one. It is **not consulted** in rung 3.2:
/// callers that only need the function/args ignore it. The instantiation-time
/// caller (`const_ref.rs`) stores it into the semantic instance's `ImplEnv`.
#[derive(Debug, Clone)]
pub struct ResolvedTraitMethod<'db> {
    pub func: Func<'db>,
    pub impl_args: Vec<TyId<'db>>,
    pub implementor: ImplementorId<'db>,
}

/// Rung 3.3 MIR re-resolution determinism check (pure).
///
/// Compares the implementor type-checking committed to at instantiation time
/// (`typeck_selected`, carried on the semantic instance's `ImplEnv`) against the
/// implementor monomorphization just re-resolved (`mono_resolved`):
///
/// - `typeck_selected == None` — type-checking did not commit a concrete impl
///   for this callable (genuinely generic / not a trait method). Nothing to
///   enforce → `Ok(())`.
/// - both present and **equal** — the expected path under coherence → `Ok(())`.
/// - both present and **differ** — a determinism violation: monomorphization
///   picked a *different* impl than type-checking. Returns the offending pair so
///   the caller can raise a hard, precise diagnostic (never silently proceed).
///
/// This is its own pure function so the (otherwise un-triggerable under
/// coherence) violation branch is unit-testable directly — a source-level
/// mismatch is not constructible in valid Fe (coherence guarantees a single
/// valid impl per concrete trait inst, and re-resolution with identical inputs
/// is deterministic), so the assertion is a defensive invariant that earns its
/// keep when rung 3.4 broadens "provision" and could re-resolve differently.
pub fn check_reresolution_determinism<'db>(
    typeck_selected: Option<ImplementorId<'db>>,
    mono_resolved: ImplementorId<'db>,
) -> Result<(), (ImplementorId<'db>, ImplementorId<'db>)> {
    match typeck_selected {
        Some(typeck) if typeck != mono_resolved => Err((typeck, mono_resolved)),
        _ => Ok(()),
    }
}

/// Resolves the concrete HIR function that implements `method` for the given
/// trait instance, returning the function, the impl's instantiated generic
/// arguments, and the selected implementor (see [`ResolvedTraitMethod`]).
pub fn resolve_trait_method_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    method: IdentId<'db>,
) -> Option<ResolvedTraitMethod<'db>> {
    let assumptions = solve_cx.assumptions();
    let norm_scope = solve_cx.normalization_scope_for_trait_inst(db, inst);
    let inst = normalize_trait_inst_preserving_validity(db, inst, norm_scope, assumptions);

    let implementor = match solve_cx.select_impl(db, inst) {
        Selection::Unique(implementor) => implementor,
        Selection::Ambiguous(_ambiguous) => return None,
        Selection::NotFound => return None,
    };
    resolve_trait_method_against_implementor(db, inst, method, implementor)
}

/// Reconstructs a [`ResolvedTraitMethod`] for `method` against a **given**
/// (recorded) implementor, WITHOUT re-running impl selection (`select_impl`).
///
/// This is the recorded-source entry point of the FCO "slide" cascade (C1).
/// When a callee instance carries the implementor typeck's solver committed to
/// at instantiation time (`ImplEnv::selected_implementor`), MIR consumes that
/// recorded implementor as the resolution SOURCE instead of re-resolving
/// through the global impl table.
///
/// Normalization of `inst` is performed here, identically to
/// [`resolve_trait_method_instance`] (`normalization_scope_for_trait_inst` +
/// `normalize_trait_inst_preserving_validity`), so the subsequent
/// `trait_inst`-unification is byte-identical to the re-resolve path. The ONLY
/// difference between the two paths is the source of `implementor`: here it is
/// the recorded one; in `resolve_trait_method_instance` it is `select_impl`'s
/// result. Under coherence (today's ≤1-impl invariant, enforced for the
/// recorded path in MIR by `check_reresolution_determinism`) the recorded
/// implementor is exactly the one `select_impl` would return, so this produces
/// the identical `func`/`impl_args`.
pub fn resolve_trait_method_instance_with_implementor<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    method: IdentId<'db>,
    implementor: ImplementorId<'db>,
) -> Option<ResolvedTraitMethod<'db>> {
    let assumptions = solve_cx.assumptions();
    let norm_scope = solve_cx.normalization_scope_for_trait_inst(db, inst);
    let inst = normalize_trait_inst_preserving_validity(db, inst, norm_scope, assumptions);
    resolve_trait_method_against_implementor(db, inst, method, implementor)
}

/// Shared reconstruction tail of [`resolve_trait_method_instance`] and
/// [`resolve_trait_method_instance_with_implementor`]: given a concrete
/// `implementor` and the **already-normalized** trait instance `inst`, build
/// the [`ResolvedTraitMethod`] (method lookup + fresh-var instantiation +
/// `trait_inst` unification + param folding / trait-arg fallback). Factored out
/// so both the `select_impl` path and the recorded-implementor path run the
/// exact same code, guaranteeing byte-identical `func`/`impl_args`.
fn resolve_trait_method_against_implementor<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    method: IdentId<'db>,
    implementor: ImplementorId<'db>,
) -> Option<ResolvedTraitMethod<'db>> {
    // The given implementor is provenance: carried out for rung 3.2, never
    // consulted here (the resolution result below is unchanged from before).
    let selected_implementor = implementor;
    let explicit_method = implementor.methods(db).get(&method).copied();
    let trait_method = implementor
        .trait_def(db)
        .method_defs(db)
        .get(&method)
        .copied()
        .filter(|method| method.body(db).is_some());

    let mut table = UnificationTable::new(db);
    let implementor = table.instantiate_with_fresh_vars(Binder::bind(implementor));
    table.unify(implementor.trait_inst(db), inst).ok()?;
    if let Some(func) = explicit_method {
        let impl_args = implementor
            .params(db)
            .iter()
            .map(|&ty| ty.fold_with(db, &mut table))
            .collect();
        return Some(ResolvedTraitMethod {
            func,
            impl_args,
            implementor: selected_implementor,
        });
    }

    let func = trait_method?;
    let trait_args = inst.args(db).to_vec();
    Some(ResolvedTraitMethod {
        func,
        impl_args: trait_args,
        implementor: selected_implementor,
    })
}

/// Returns all implementors for the given `ty` that satisfy the given assumptions.
pub(crate) fn impls_for_ty_with_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Vec<Binder<ImplementorId<'db>>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);

    let env = ingot_trait_env(db, ingot);
    let solve_cx = ProvisionEnv::for_scope(ingot.root_mod(db).scope(), assumptions).solve_cx(db);
    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }

    let mut cands = vec![];
    for (key, insts) in env.ty_to_implementors.iter() {
        let snapshot = table.snapshot();
        let key = table.instantiate_with_fresh_vars(*key);
        if table.unify(key, ty.base_ty(db)).is_ok() {
            cands.push(insts);
        }

        table.rollback_to(snapshot);
    }

    let mut raw_impls: Vec<Binder<ImplementorId<'db>>> =
        cands.into_iter().flatten().copied().collect();

    if ty.as_contract(db).is_some() {
        raw_impls.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    raw_impls
        .into_iter()
        .filter(|impl_| {
            let snapshot = table.snapshot();

            let inst = table.instantiate_with_fresh_vars(*impl_);
            let impl_ty = table.instantiate_to_term(inst.self_ty(db));
            let ty_term = table.instantiate_to_term(ty);
            let unifies = table.unify(impl_ty, ty_term).is_ok();

            if unifies {
                // Filter out impls that don't satisfy assumptions
                let impl_constraints = inst.constraints(db);
                if impl_constraints.is_empty(db) {
                    table.rollback_to(snapshot);
                    return true;
                }

                for &constraint in impl_constraints.list(db) {
                    // The implementor was instantiated with fresh vars and its
                    // self type unified with `ty`, so fold the predicate through
                    // the table to get the *concrete* goal (e.g. `Point: Copy`,
                    // not `?T: Copy`) before checking satisfiability — otherwise a
                    // conditional blanket's guard (`impl<T: Copy> Clone for T`)
                    // is never judged UnSat and the impl is kept spuriously.
                    let constraint = constraint.fold_with(db, &mut table);
                    match is_goal_satisfiable(db, solve_cx, constraint) {
                        GoalSatisfiability::UnSat(_) => {
                            table.rollback_to(snapshot);
                            return false;
                        }
                        _ => {
                            // Ignoring the NeedsConfirmation case for now
                        }
                    }
                }
            }

            table.rollback_to(snapshot);
            unifies
        })
        .collect()
}

/// Returns all implementors for the given `ty`.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
) -> Vec<Binder<ImplementorId<'db>>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);

    let env = ingot_trait_env(db, ingot);
    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }

    let mut cands = vec![];
    for (key, insts) in env.ty_to_implementors.iter() {
        let snapshot = table.snapshot();
        let key = table.instantiate_with_fresh_vars(*key);
        if table.unify(key, ty.base_ty(db)).is_ok() {
            cands.push(insts);
        }
        table.rollback_to(snapshot);
    }

    let mut raw_impls: Vec<Binder<ImplementorId<'db>>> =
        cands.into_iter().flatten().copied().collect();

    if ty.as_contract(db).is_some() {
        raw_impls.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    raw_impls
        .into_iter()
        .filter(|impl_| {
            let snapshot = table.snapshot();

            let inst = table.instantiate_with_fresh_vars(*impl_);
            let impl_ty = table.instantiate_to_term(inst.self_ty(db));
            let ty_term = table.instantiate_to_term(ty);
            let is_ok = table.unify(impl_ty, ty_term).is_ok();

            table.rollback_to(snapshot);

            is_ok
        })
        .collect()
}

/// Finds the implementor of `trait_def` for `ty`, searching the trait env
/// **visible from `scope`** — the scope's ingot first, then `ty`'s own ingot
/// (for an external trait implemented in the type's ingot). Returns the first
/// match.
///
/// PS1a (read-path unification): this is the first scope-indexed *provision
/// lookup* primitive. It consolidates the duplicated
/// `search_ingots = [scope_ingot, ty_ingot]` + `impls_for_ty(..).find(trait)`
/// pattern that was open-coded at several call sites (msg-variant / contract
/// resolution). It is a deliberate **read-path** unification of the global trait
/// env behind one entry; it does NOT change resolution outcomes. The mutable,
/// non-salsa `EffectEnv` frame stack is intentionally NOT folded in here — it is
/// a separate resolution surface (unifying it would be a facade).
pub(crate) fn find_implementor_of_trait_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
    trait_def: Trait<'db>,
) -> Option<Binder<ImplementorId<'db>>> {
    let canonical_ty = Canonical::new(db, ty);
    let scope_ingot = scope.ingot(db);
    let search_ingots = [
        Some(scope_ingot),
        ty.ingot(db).filter(|&ingot| ingot != scope_ingot),
    ];
    search_ingots.into_iter().flatten().find_map(|ingot| {
        impls_for_ty(db, ingot, canonical_ty)
            .iter()
            .find(|impl_| impl_.skip_binder().trait_def(db) == trait_def)
            .copied()
    })
}

/// Looks up the HIR body for an associated const defined in the selected trait impl, if unique.
pub fn assoc_const_body_for_trait_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    const_name: IdentId<'db>,
) -> Option<crate::hir_def::Body<'db>> {
    assoc_const_body_and_impl_args_for_trait_inst(db, solve_cx, inst, const_name)
        .map(|(body, _)| body)
}

/// Looks up the HIR body for an associated const defined in the selected trait impl, if unique,
/// returning both the body and the impl's instantiated generic arguments.
///
/// The returned generic args correspond to the impl's own generic parameters (not the trait's),
/// and are suitable for CTFE/type checking of the impl const body.
pub fn assoc_const_body_and_impl_args_for_trait_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    const_name: IdentId<'db>,
) -> Option<(crate::hir_def::Body<'db>, Vec<TyId<'db>>)> {
    let assumptions = solve_cx.assumptions();
    let norm_scope = solve_cx.normalization_scope_for_trait_inst(db, inst);
    let inst = normalize_trait_inst_preserving_validity(db, inst, norm_scope, assumptions);

    let implementor = match solve_cx.select_impl(db, inst) {
        Selection::Unique(implementor) => implementor,
        Selection::Ambiguous(_ambiguous) => return None,
        Selection::NotFound => return None,
    };
    let hir_impl = match implementor.origin(db) {
        ImplementorOrigin::Hir(impl_trait) => impl_trait,
        ImplementorOrigin::VirtualContract(_) | ImplementorOrigin::Assumption => return None,
    };
    let def = hir_impl
        .hir_consts(db)
        .iter()
        .find(|c| c.name.to_opt() == Some(const_name))?;
    let body = def.value.to_opt()?;

    let mut table = UnificationTable::new(db);
    let implementor = table.instantiate_with_fresh_vars(Binder::bind(implementor));
    table.unify(implementor.trait_inst(db), inst).ok()?;
    let impl_args = implementor
        .params(db)
        .iter()
        .map(|&ty| ty.fold_with(db, &mut table))
        .collect();
    Some((body, impl_args))
}

/// Whether `inst` is satisfied by a uniquely-selected concrete impl rather than
/// only by an assumption (e.g. `T: Trait` inside a generic function).
///
/// This gates use of a trait associated const's default value: the default may
/// stand in for the const only when a concrete impl is selected (and inherits
/// it). For an assumption-satisfied instance the const must stay abstract, so a
/// concrete impl that overrides it specializes correctly instead of being fixed
/// to the default.
pub fn trait_inst_selects_concrete_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> bool {
    let assumptions = solve_cx.assumptions();
    let norm_scope = solve_cx.normalization_scope_for_trait_inst(db, inst);
    let inst = normalize_trait_inst_preserving_validity(db, inst, norm_scope, assumptions);
    match solve_cx.select_impl(db, inst) {
        Selection::Unique(implementor) => {
            !matches!(implementor.origin(db), ImplementorOrigin::Assumption)
        }
        Selection::Ambiguous(_) | Selection::NotFound => false,
    }
}

/// Represents the trait environment of an ingot, which maintain all trait
/// implementors which can be used in the ingot.
#[derive(Debug, PartialEq, Eq, Clone, Update)]
pub(crate) struct TraitEnv<'db> {
    /// Implementors grouped by trait definition.
    pub(crate) impls: FxHashMap<Trait<'db>, Vec<Binder<ImplementorId<'db>>>>,

    /// This maintains a mapping from the base type to the implementors.
    ty_to_implementors: FxHashMap<Binder<TyId<'db>>, Vec<Binder<ImplementorId<'db>>>>,

    ingot: Ingot<'db>,
}

impl<'db> TraitEnv<'db> {
    fn collect(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Self {
        let mut impls: FxHashMap<Trait<'db>, Vec<Binder<ImplementorId<'db>>>> =
            FxHashMap::default();
        let mut ty_to_implementors: FxHashMap<Binder<TyId>, Vec<Binder<ImplementorId<'db>>>> =
            FxHashMap::default();

        for impl_map in ingot
            .resolved_external_ingots(db)
            .iter()
            .map(|(_, external)| collect_trait_impls(db, *external))
            .chain(std::iter::once(collect_trait_impls(db, ingot)))
        {
            // Raw impl collection only lowers visible implementors. Any overlap diagnostics run
            // separately on top of this assembled environment.
            for (trait_def, implementors) in impl_map.iter() {
                impls
                    .entry(*trait_def)
                    .or_default()
                    .extend(implementors.iter().copied());

                for implementor in implementors {
                    let self_ty = implementor.instantiate_identity().self_ty(db);
                    ty_to_implementors
                        .entry(Binder::bind(self_ty.base_ty(db)))
                        .or_default()
                        .push(*implementor);
                }
            }
        }

        Self {
            impls,
            ty_to_implementors,
            ingot,
        }
    }
}

/// Represents a slim, internal view of a trait impl, derived from an
/// `ImplTrait` item and its lowered trait instance.
///
/// `pub` (rather than `pub(crate)`) because it is the provenance carried out of
/// typeck's impl selection (rung 3.2): `resolve_trait_method_instance` reports
/// the selected `ImplementorId` so the instantiation-time caller can record
/// which impl typeck committed to in the semantic instance's `ImplEnv`. The
/// carry is read-only in rung 3.2 (never compared, hashed into a key, or
/// serialized into a codegen symbol); rung 3.3 will assert MIR re-resolves the
/// same one.
#[salsa::interned]
#[derive(Debug)]
pub struct ImplementorId<'db> {
    /// The trait instance that this impl realizes.
    pub(crate) trait_: TraitInstId<'db>,

    /// The type parameters of this implementor.
    #[return_ref]
    pub(crate) params: Vec<TyId<'db>>,

    #[return_ref]
    pub(crate) types: IndexMap<IdentId<'db>, TyId<'db>>,

    pub(crate) origin: ImplementorOrigin<'db>,
}

impl<'db> ImplementorId<'db> {
    pub(crate) fn assumption(db: &'db dyn HirAnalysisDb, inst: TraitInstId<'db>) -> Self {
        ImplementorId::new(
            db,
            inst,
            Vec::new(),
            IndexMap::new(),
            ImplementorOrigin::Assumption,
        )
    }

    pub(crate) fn hir_impl_trait(self, db: &'db dyn HirAnalysisDb) -> ImplTrait<'db> {
        match self.origin(db) {
            ImplementorOrigin::Hir(impl_trait) => impl_trait,
            ImplementorOrigin::VirtualContract(contract) => panic!(
                "requested HIR impl-trait for virtual implementor (contract={})",
                contract
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string())
            ),
            ImplementorOrigin::Assumption => {
                panic!("requested HIR impl-trait for assumption-based implementor")
            }
        }
    }

    /// Associated type defined in this impl, if any.
    pub(crate) fn assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.types(db).get(&name).copied()
    }

    /// Trait definition implemented by this impl.
    pub(crate) fn trait_def(self, db: &'db dyn HirAnalysisDb) -> Trait<'db> {
        self.trait_(db).def(db)
    }

    /// Semantic self type of this impl.
    ///
    /// `pub` so codegen-symbol minting (`mir::runtime::stable_key`) can build a
    /// stable per-implementor discriminator for a SCOPE-SELECTED
    /// `ImplEnv::selected_implementor` (cascade C3d Some-only identity).
    pub fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.trait_(db).self_ty(db)
    }

    /// The HIR `impl` item this implementor was lowered from, as an `ItemKind`,
    /// or `None` for a virtual / assumption-based implementor (which never carry
    /// a `selected_implementor`). It is the stable discriminator the cascade C3d
    /// Some-only codegen-symbol identity needs: a derived default and a
    /// hand-written override of the same `(Trait, Type)` are DISTINCT HIR `impl`
    /// items, so this distinguishes them (`mir::runtime::stable_key`).
    pub fn hir_item(self, db: &'db dyn HirAnalysisDb) -> Option<crate::hir_def::ItemKind<'db>> {
        match self.origin(db) {
            ImplementorOrigin::Hir(impl_trait) => Some(impl_trait.into()),
            ImplementorOrigin::VirtualContract(_) | ImplementorOrigin::Assumption => None,
        }
    }

    /// Trait instance realized by this impl, including its associated type definitions.
    ///
    /// `pub` for the same reason as [`Self::self_ty`] (cascade C3d codegen-symbol
    /// discriminator).
    pub fn trait_inst(self, db: &'db dyn HirAnalysisDb) -> TraitInstId<'db> {
        let trait_inst = self.trait_(db);
        if self.types(db).is_empty() {
            return trait_inst;
        }

        let mut assoc_type_bindings = trait_inst.assoc_type_bindings(db).clone();
        for (name, ty) in self.types(db) {
            assoc_type_bindings.insert(*name, *ty);
        }

        TraitInstId::new(
            db,
            trait_inst.def(db),
            trait_inst.args(db).to_vec(),
            assoc_type_bindings,
        )
    }

    /// Returns the constraints that the implementor requires when the
    /// implementation is selected.
    pub(crate) fn constraints(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        match self.origin(db) {
            ImplementorOrigin::Hir(impl_trait) => {
                collect_constraints(db, impl_trait.into()).instantiate(db, self.params(db))
            }
            ImplementorOrigin::VirtualContract(_) | ImplementorOrigin::Assumption => {
                PredicateListId::empty_list(db)
            }
        }
    }

    /// Method map for this impl, keyed by name.
    pub(crate) fn methods(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db IndexMap<IdentId<'db>, Func<'db>> {
        collect_implementor_methods(db, self)
    }

    /// Compare impl methods vs. trait methods and report missing/mismatched ones.
    pub(crate) fn diags_method_conformance(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        let ImplementorOrigin::Hir(impl_trait) = self.origin(db) else {
            return Vec::new();
        };
        // A derive provider declared in the `impl Derive<Goal> for Provider`
        // form is NOT an ordinary trait impl: its `derive` fn is the
        // compile-time command-language entry point (checked by the provider
        // executor, exempt from ordinary signature/body analysis via
        // `is_derive_provider_fn`), so its signature intentionally diverges from
        // `core::derive::Derive::derive` (extra type params for live abstract
        // heads, the `uses (..)` capability clause, etc.). It must therefore
        // skip ordinary method conformance.
        if crate::core::lower::impl_trait_provider_goal_path(db, impl_trait).is_some() {
            return Vec::new();
        }
        let mut diags = vec![];
        let impl_methods = self.methods(db);
        let hir_trait = self.trait_def(db);
        let trait_methods = self.trait_def(db).method_defs(db);
        let mut required_methods: IndexSet<_> = trait_methods
            .iter()
            .filter_map(|(name, &trait_method)| trait_method.body(db).is_none().then_some(*name))
            .collect();

        for (name, impl_m) in impl_methods {
            let Some(trait_m) = trait_methods.get(name) else {
                diags.push(
                    ImplDiag::MethodNotDefinedInTrait {
                        primary: self.hir_impl_trait(db).span().trait_ref().into(),
                        method_name: *name,
                        trait_: hir_trait,
                    }
                    .into(),
                );
                continue;
            };
            compare_impl_method(
                db,
                impl_m.as_callable(db).unwrap(),
                trait_m.as_callable(db).unwrap(),
                self.trait_(db),
                &mut diags,
            );
            required_methods.remove(name);
        }

        if !required_methods.is_empty() {
            diags.push(
                ImplDiag::NotAllTraitItemsImplemented {
                    primary: self.hir_impl_trait(db).span().ty().into(),
                    not_implemented: required_methods.into_iter().collect(),
                }
                .into(),
            );
        }

        diags
    }
}

/// Returns `true` if the given two implementors conflict.
///
/// This mirrors the legacy `Implementor`-based semantics:
/// - instantiate both implementors with fresh vars and unify them;
/// - then check that the merged constraints are satisfiable.
pub(crate) fn does_impl_trait_conflict<'db>(
    db: &'db dyn HirAnalysisDb,
    a: Binder<ImplementorId<'db>>,
    b: Binder<ImplementorId<'db>>,
) -> bool {
    let mut table = UnificationTable::new(db);
    let a = table.instantiate_with_fresh_vars(a);
    let b = table.instantiate_with_fresh_vars(b);

    if table.unify(a, b).is_err() {
        return false;
    }

    let a_constraints = a.constraints(db);
    let b_constraints = b.constraints(db);

    if a_constraints.is_empty(db) && b_constraints.is_empty(db) {
        return true;
    }

    // Check if all constraints from both implementations would be satisfiable
    // when the types are unified.
    let merged_constraints = a_constraints.merge(db, b_constraints);
    let solve_cx =
        ProvisionEnv::for_scope(a.trait_def(db).scope(), PredicateListId::empty_list(db))
            .solve_cx(db);

    for &constraint in merged_constraints.list(db) {
        let constraint = constraint.fold_with(db, &mut table);

        match is_goal_satisfiable(db, solve_cx, constraint) {
            GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid => {
                return false;
            }
            _ => {
                // Constraint is satisfiable or needs more information, continue checking.
            }
        }
    }

    true
}

/// Whether a goal's resolved trait-def identity is in the v1 CANONICAL set: the
/// narrow tier of traits whose impl is consensus-/layout-critical, so a second
/// in-scope impl is a genuine coherence conflict (a wrong impl silently
/// relocates storage slots or changes the ABI wire format = fund loss), NOT a
/// benign customization. Recognition keys on the FULL resolved identity (the
/// trait's own name + its defining ingot kind), NEVER on the bare spelling —
/// EXACTLY like [`is_std_evm_contract_trait_def`] and the `core::derive` item
/// recognizer (`scope_is_core_derive_item`, `provider_goal.rs`): a user trait
/// merely *named* `AbiSize` / `StorageKey`, defined in a `Local` ingot, resolves
/// to a DIFFERENT `Trait` def and is NOT canonical.
///
/// The v1 set (tunable allowlist; v1 = storage-layout + ABI-layout):
///   - storage-layout: `std::evm::storage_map::StorageKey` (the storage-slot
///     witness — a wrong `write_key` relocates every mapping slot);
///   - ABI-layout family: `core::abi::AbiSize` (the layout/size metadata:
///     `HEAD_SIZE` / `IS_DYNAMIC` / `payload_size`), plus the codecs built on it,
///     `core::abi::Encode` / `core::abi::Decode` (a wrong impl produces wrong
///     on-the-wire bytes = consensus break). `AbiSize` lives in the `Core` ingot
///     and `StorageKey` in the `Std` ingot, so the kind discriminates per trait.
///
/// Deliberately NON-canonical in v1: `Ord` / `Hash` (`core::ops`). These are
/// commonly and legitimately customized via providers (e.g. `StableOrd`), and
/// the "consensus Ord/Hash" worry is a use-context sub-case (a specific contract
/// relying on a specific ordering), not a property of the trait def — deferred as
/// a tunable refinement rather than blanket-canonicalized here.
///
/// LIVE (FCO slide C3c-1): the coherence conflict gate in `lowered_implementor`
/// (`core/semantic/mod.rs`) now computes this predicate and branches the conflict
/// path on it. As of C3c-1 BOTH branches preserve today's behavior — a conflict
/// still returns `Conflict`/emits `5-0001` for canonical AND non-canonical goals
/// (byte-identical) — so the seam is planted and the predicate is genuinely
/// consumed. The coherence demotion that turns "the goal is NON-canonical" into
/// "a second impl is permitted (provider-overridable)" is increment C3c-3, which
/// flips only the non-canonical branch. The unit test
/// `is_single_impl_keys_on_resolved_identity_not_name` exercises the v1 set
/// (StorageKey / AbiSize = true) and the non-canonical cases (Ord / Eq /
/// user-defined = false), each via a real trait resolution so neither arm passes
/// vacuously.
pub(crate) fn is_single_impl<'db>(db: &'db dyn HirAnalysisDb, trait_def: Trait<'db>) -> bool {
    // A goal trait is "single-impl" (one-of-a-kind: at most one impl program-wide,
    // the money floor) iff its DECLARATION carries the `#[fixed]` attribute. The
    // policy thus lives in Fe, on the trait decl (the storage-/ABI-layout traits in
    // core/std), not a hardcoded compiler allowlist; adding or removing a goal is a
    // one-line Fe change (`fco-guts-over-sugar`: the SET is a deferred-tunable
    // default).
    //
    // Two properties this spelling buys for free:
    // - SCOPE-FREE / salsa-safe: it reads an attribute of the trait HIR node, never
    //   the lexical scope, so the scarcity key at `lowered_implementor` stays
    //   scope-free (`trait_resolution/mod.rs` key invariant).
    // - ORPHAN-SAFE by construction: you can only attribute a trait you are
    //   DECLARING (one you own), so single-impl-marking a FOREIGN trait to seize
    //   sole authority over its impls is unrepresentable.
    crate::hir_def::ItemKind::Trait(trait_def)
        .attrs(db)
        .is_some_and(|attrs| attrs.has_attr(db, "fixed"))
}

/// Represents an instantiated trait, which can be thought of as a trait
/// reference from a HIR perspective.
#[salsa::interned]
#[derive(Debug)]
pub struct TraitInstId<'db> {
    pub key: Trait<'db>,
    /// Regular type and const parameters: [Self, ExplicitTypeParam1, ..., ExplicitConstParamN]
    #[return_ref]
    pub args: Vec<TyId<'db>>,

    /// Associated type bounds specified by user, eg `Iterator<Item=i32>`
    #[return_ref]
    pub assoc_type_bindings: IndexMap<IdentId<'db>, TyId<'db>>,
}

impl<'db> TraitInstId<'db> {
    pub fn def(self, db: &'db dyn HirAnalysisDb) -> Trait<'db> {
        self.key(db)
    }

    pub fn new_simple(db: &'db dyn HirAnalysisDb, def: Trait<'db>, args: Vec<TyId<'db>>) -> Self {
        Self::new(db, def, args, IndexMap::new())
    }

    pub fn with_fresh_vars(
        db: &'db dyn HirAnalysisDb,
        def: Trait<'db>,
        table: &mut UnificationTable<'db>,
    ) -> Self {
        let args = def
            .params(db)
            .iter()
            .map(|ty| table.new_var_from_param(*ty))
            .collect::<Vec<_>>();
        Self::new(db, def, args, IndexMap::new())
    }

    pub fn assoc_ty_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<(IdentId<'db>, TyId<'db>)> {
        self.assoc_type_bindings(db)
            .iter()
            .map(|(&name, &ty)| (name, ty))
            .collect()
    }

    pub fn assoc_ty(self, db: &'db dyn HirAnalysisDb, name: IdentId<'db>) -> Option<TyId<'db>> {
        if let Some(ty) = self.assoc_type_bindings(db).get(&name) {
            return Some(*ty);
        }
        if self.def(db).assoc_ty(db, name).is_some() {
            return Some(TyId::assoc_ty(db, self, name));
        }
        None
    }

    /// Normalize arguments of this trait instance.
    pub(crate) fn normalize(
        self,
        db: &'db dyn HirAnalysisDb,
        scope: crate::core::hir_def::scope_graph::ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        let normalized_args: Vec<_> = self
            .args(db)
            .iter()
            .map(|&arg| crate::analysis::ty::normalize::normalize_ty(db, arg, scope, assumptions))
            .collect();
        Self::new(
            db,
            self.def(db),
            normalized_args,
            self.assoc_type_bindings(db).clone(),
        )
    }

    pub fn pretty_print(self, db: &dyn HirAnalysisDb, as_pred: bool) -> String {
        if as_pred {
            let inst = self.pretty_print(db, false);
            let self_ty = self.self_ty(db);
            format! {"{}: {}", self_ty.pretty_print(db), inst}
        } else {
            let mut s = self
                .def(db)
                .name(db)
                .to_opt()
                .map(|n| n.data(db).as_str())
                .unwrap_or("<unknown>")
                .to_string();

            let mut args = self.args(db).iter().map(|ty| ty.pretty_print(db));
            // Skip the first type parameter since it's the implementor type.
            args.next();

            let mut has_generics = false;
            if let Some(first) = args.next() {
                s.push('<');
                s.push_str(first);
                for arg in args {
                    s.push_str(", ");
                    s.push_str(arg);
                }
                has_generics = true;
            }

            // Add associated type bindings
            if !self.assoc_type_bindings(db).is_empty() {
                if !has_generics {
                    s.push('<');
                } else {
                    s.push_str(", ");
                }

                let mut first_assoc = true;
                for (name, ty) in self.assoc_type_bindings(db) {
                    if !first_assoc {
                        s.push_str(", ");
                    }
                    first_assoc = false;
                    s.push_str(name.data(db));
                    s.push_str(" = ");
                    s.push_str(ty.pretty_print(db));
                }
                has_generics = true;
            }

            if has_generics {
                s.push('>');
            }

            s
        }
    }

    pub fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.args(db)[0]
    }
}

// Represents a trait definition.
// (TraitDef struct and impl removed)

// (TraitMethod struct and impl removed)

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use super::{check_reresolution_determinism, impls_for_trait_def, is_single_impl};
    use crate::analysis::name_resolution::{PathRes, resolve_path};
    use crate::analysis::ty::trait_resolution::PredicateListId;
    use crate::hir_def::{ItemKind, PathId};
    use crate::test_db::HirAnalysisTestDb;

    /// Rung 3.3: the MIR re-resolution determinism assertion fires on a genuine
    /// impl mismatch and is a no-op otherwise. A source-level mismatch is NOT
    /// constructible in valid Fe (coherence guarantees a single valid impl per
    /// concrete trait inst, and re-resolution with identical inputs is
    /// deterministic), so we cannot drive this via a `.fe` fixture — we instead
    /// build two genuinely distinct `ImplementorId`s (two types implementing the
    /// same trait) and exercise the pure check directly. This keeps the
    /// otherwise-untriggerable violation branch under test for rung 3.4.
    #[test]
    fn reresolution_determinism_check_detects_impl_mismatch() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("reresolution_determinism_check_detects_impl_mismatch.fe"),
            r#"
trait Foo {}

struct A {}
struct B {}

impl Foo for A {}
impl Foo for B {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let trait_ = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_)
                    if trait_
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "Foo") =>
                {
                    Some(trait_)
                }
                _ => None,
            })
            .expect("missing `Foo` trait");

        let impls = impls_for_trait_def(&db, top_mod.ingot(&db), trait_);
        assert_eq!(impls.len(), 2, "expected the two distinct `Foo` impls");
        // Two genuinely distinct implementors (`Foo for A` and `Foo for B`).
        let impl_a = impls[0].instantiate_identity();
        let impl_b = impls[1].instantiate_identity();
        assert_ne!(impl_a, impl_b, "impls of `Foo` for A vs B must differ");

        // Carried `None` (typeck committed nothing concrete) → no assertion.
        assert!(check_reresolution_determinism(None, impl_a).is_ok());
        // Carried and re-resolved agree (the coherent path) → no assertion.
        assert!(check_reresolution_determinism(Some(impl_a), impl_a).is_ok());
        // Carried and re-resolved DIFFER → the hard-fail branch, returning the
        // offending pair (mapped to `LowerError::NondeterministicReResolution`
        // at the MIR site, never a silent divergence / panic).
        assert_eq!(
            check_reresolution_determinism(Some(impl_a), impl_b),
            Err((impl_a, impl_b)),
        );
    }

    #[test]
    fn trait_env_collects_overlapping_constrained_impls_without_cycle() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("trait_env_collects_overlapping_constrained_impls_without_cycle.fe"),
            r#"
trait Marker {}
trait Foo {}

impl<T> Foo for T where T: Marker {}
impl<T> Foo for T where T: Marker {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let trait_ = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_)
                    if trait_
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "Foo") =>
                {
                    Some(trait_)
                }
                _ => None,
            })
            .expect("missing `Foo` trait");

        let impls = impls_for_trait_def(&db, top_mod.ingot(&db), trait_);
        assert_eq!(impls.len(), 2);
    }

    /// Resolve `path` (as segment strings) from `file`'s top-module scope to its
    /// trait def, asserting it resolves to a TRAIT (anti-vacuous: a non-`Trait`
    /// resolution — or a failure — would make either polarity of the canonical
    /// check pass for free), then return `is_single_impl` for that def.
    fn resolve_is_single_impl(
        db: &mut HirAnalysisTestDb,
        file_name: &str,
        text: &str,
        path: &[&str],
    ) -> bool {
        let file = db.new_stand_alone(Utf8PathBuf::from(file_name), text);
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod.scope();
        let assumptions = PredicateListId::empty_list(db);
        let path_id = PathId::from_segments(db, path);
        let trait_def = match resolve_path(db, path_id, scope, assumptions, false) {
            Ok(PathRes::Trait(inst)) => inst.def(db),
            res => panic!("expected {path:?} to resolve to a trait, got {res:?}"),
        };
        is_single_impl(db, trait_def)
    }

    /// FCO: the single-impl (money-floor) predicate is driven by the `#[fixed]`
    /// attribute on the trait DECLARATION, never by the bare spelling. It
    /// recognizes exactly the traits that carry `#[fixed]` (the v1 storage-/ABI-
    /// layout set in core/std).
    ///
    /// Positive (anti-vacuous): the real `std::evm::StorageKey` and
    /// `core::abi::AbiSize` traits carry `#[fixed]`, so they ARE single-impl.
    ///
    /// Negative (anti-vacuous): `core::ops::Ord` and `core::ops::Eq` are real
    /// traits with NO `#[fixed]` (confirming they are deliberately non-single-impl
    /// in v1), and a LOCAL `trait AbiSize {}` declared in the fixture, same
    /// spelling but no `#[fixed]`, is NOT single-impl. The local case is the
    /// load-bearing one: it proves the predicate reads the attribute, not the name.
    /// `resolve_is_single_impl` asserts each path resolves to a real trait, so
    /// neither arm can pass vacuously.
    #[test]
    fn is_single_impl_reads_fixed_attribute() {
        // Positive: storage-layout — std::evm::StorageKey.
        let mut db = HirAnalysisTestDb::default();
        assert!(
            resolve_is_single_impl(
                &mut db,
                "is_single_impl_storage_key.fe",
                "struct ImplPermit {}\n",
                &["std", "evm", "StorageKey"],
            ),
            "std::evm::StorageKey (storage-layout) must be canonical in v1"
        );

        // Positive: ABI-layout — core::abi::AbiSize.
        let mut db = HirAnalysisTestDb::default();
        assert!(
            resolve_is_single_impl(
                &mut db,
                "is_single_impl_abi_size.fe",
                "struct ImplPermit {}\n",
                &["core", "abi", "AbiSize"],
            ),
            "core::abi::AbiSize (ABI-layout) must be canonical in v1"
        );

        // Negative: core::ops::Ord is a real trait but deliberately NON-canonical
        // in v1 (commonly customized via providers).
        let mut db = HirAnalysisTestDb::default();
        assert!(
            !resolve_is_single_impl(
                &mut db,
                "is_single_impl_ord.fe",
                "struct ImplPermit {}\n",
                &["core", "ops", "Ord"],
            ),
            "core::ops::Ord must be NON-canonical in v1"
        );

        // Negative: core::ops::Eq is also a real, non-canonical trait.
        let mut db = HirAnalysisTestDb::default();
        assert!(
            !resolve_is_single_impl(
                &mut db,
                "is_single_impl_eq.fe",
                "struct ImplPermit {}\n",
                &["core", "ops", "Eq"],
            ),
            "core::ops::Eq must be NON-canonical in v1"
        );

        // Negative (anti-vacuous on identity): a LOCAL `trait AbiSize {}` — same
        // spelling, `Local` ingot — resolves to a DIFFERENT def and is NOT
        // canonical. The identity is keyed on (name, defining-ingot), not name.
        let mut db = HirAnalysisTestDb::default();
        assert!(
            !resolve_is_single_impl(
                &mut db,
                "is_single_impl_local_abi_size.fe",
                "trait AbiSize {}\n",
                &["AbiSize"],
            ),
            "a local `trait AbiSize` (not core::abi::AbiSize) must be NON-canonical"
        );
    }
}
