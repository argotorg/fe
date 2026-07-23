//! This module contains all trait related types definitions.

use crate::{
    analysis::ty::{
        method_cmp::compare_impl_method,
        trait_lower::{
            ImplSelfKey, collect_trait_impls, complete_impl_trait, complete_selected_impl,
            lower_impl_trait_header,
        },
        trait_resolution::{GoalSatisfiability, PredicateListId, Selection},
    },
    hir_def::{Contract, Func, HirIngot, IdentId, ImplTrait, Trait},
};
use common::{
    indexmap::{IndexMap, IndexSet},
    ingot::{Ingot, IngotKind},
};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use super::{
    binder::Binder,
    canonical::Canonical,
    diagnostics::{ImplDiag, TyDiagCollection},
    fold::{TyFoldable, TyFolder},
    layout_holes::LayoutRootUse,
    trait_lower::collect_implementor_methods,
    trait_resolution::{
        TraitSolveCx, constraint::collect_candidate_constraints, is_goal_satisfiable,
        normalize_trait_inst_preserving_validity,
    },
    ty_def::{TyBase, TyData, TyId},
    ty_lower::collect_generic_params,
    ty_lower::layout_param_root_uses,
    unify::UnificationTable,
    visitor::{TyVisitable, TyVisitor},
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
    let mut out = env.impls_for_trait(db, trait_def);

    if is_std_evm_contract_trait_def(db, trait_def) {
        out.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    out.into_iter()
        .map(|implementor| complete_impl_trait(db, implementor))
        .collect()
}

/// Returns implementors for `trait_def` whose self type can apply to `ty`.
///
/// Method lookup knows both halves of this key for explicitly selected traits
/// (notably operator traits). Intersecting them before completing candidates is
/// important: receiver-only lookup necessarily includes every blanket and
/// otherwise unindexed impl, whose signatures may themselves require method
/// lookup while being lowered.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_trait_and_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_def: Trait<'db>,
    ty: Canonical<TyId<'db>>,
) -> Vec<Binder<ImplementorId<'db>>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);

    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }

    let env = ingot_trait_env(db, ingot);
    let mut implementors = env.impls_for_trait(db, trait_def);

    if is_std_evm_contract_trait_def(db, trait_def) && ty.as_contract(db).is_some() {
        implementors.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    implementors
        .into_iter()
        .filter(|implementor| {
            let snapshot = table.snapshot();
            let instantiated = table.instantiate_with_fresh_vars(*implementor);
            let impl_ty = table.instantiate_to_term(instantiated.self_ty(db));
            let ty = table.instantiate_to_term(ty);
            let unifies = table.unify(impl_ty, ty).is_ok();
            table.rollback_to(snapshot);
            unifies
        })
        .map(|implementor| complete_impl_trait(db, implementor))
        .collect()
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
        impls_by_self: FxHashMap::default(),
        unindexed_self_impls: Vec::new(),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ResolvedImplInstance<'db> {
    selected: ImplementorId<'db>,
    instantiated: ImplementorId<'db>,
    trait_inst: TraitInstId<'db>,
}

impl<'db> ResolvedImplInstance<'db> {
    pub fn selected(self) -> ImplementorId<'db> {
        self.selected
    }

    pub fn trait_inst(self) -> TraitInstId<'db> {
        self.trait_inst
    }

    pub fn impl_args(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.instantiated.params(db)
    }

    pub fn impl_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.selected.params(db)
    }

    /// Resolves an implemented trait method using this already-selected impl.
    ///
    /// The returned generic arguments match the selected implementation for an
    /// explicit method and the resolved trait instance for a default method.
    pub fn method_instance(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<(Func<'db>, Vec<TyId<'db>>)> {
        let implementor = self.selected();
        if let Some(func) = implementor.methods(db).get(&name).copied() {
            return Some((func, self.impl_args(db).to_vec()));
        }
        let func = implementor
            .trait_def(db)
            .method_defs(db)
            .get(&name)
            .copied()
            .filter(|method| method.body(db).is_some())?;
        Some((func, self.trait_inst().args(db).to_vec()))
    }

    pub fn assoc_ty_template(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.selected.assoc_ty(db, name)
    }

    pub fn assoc_ty_layout_root_uses(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Vec<LayoutRootUse<'db>> {
        let mut uses = match self.selected.origin(db) {
            ImplementorOrigin::Hir(impl_trait) => {
                if let Some(assoc) = impl_trait
                    .assoc_types(db)
                    .find(|assoc| assoc.name(db) == Some(name))
                {
                    assoc.layout_root_uses(db)
                } else {
                    let trait_args = self.selected.trait_(db).args(db);
                    self.selected
                        .trait_def(db)
                        .assoc_types(db)
                        .find(|assoc| assoc.name(db) == Some(name))
                        .map_or_else(Vec::new, |assoc| {
                            assoc
                                .layout_root_uses(db)
                                .into_iter()
                                .map(|root_use| LayoutRootUse {
                                    value: Binder::bind(root_use.value).instantiate(db, trait_args),
                                    owner: root_use.owner.map(|owner| {
                                        Binder::bind(owner).instantiate(db, trait_args)
                                    }),
                                    selector: root_use.selector,
                                    index_dimensions: root_use.index_dimensions,
                                })
                                .collect()
                        })
                }
            }
            ImplementorOrigin::VirtualContract(_) | ImplementorOrigin::Assumption => Vec::new(),
        };
        let Some(template) = self.assoc_ty_template(db, name) else {
            return uses;
        };
        let root_params = self.forwarded_layout_root_params(db);
        for root_use in layout_param_root_uses(
            db,
            template,
            self.impl_params(db),
            self.impl_params(db),
            &root_params,
            Vec::new(),
        ) {
            if !uses.contains(&root_use) {
                uses.push(root_use);
            }
        }
        uses
    }

    fn forwarded_layout_root_params(self, db: &'db dyn HirAnalysisDb) -> FxHashSet<usize> {
        fn collect<'db>(
            db: &'db dyn HirAnalysisDb,
            ty: TyId<'db>,
            impl_params: &[TyId<'db>],
            visiting: &mut FxHashSet<TyId<'db>>,
            roots: &mut FxHashSet<usize>,
        ) {
            if !visiting.insert(ty) {
                return;
            }
            let args = ty.generic_args(db);
            if let Some(adt) = ty.adt_def(db)
                && args.len() == adt.params(db).len()
            {
                for (idx, arg) in args.iter().enumerate() {
                    if adt
                        .param_set(db)
                        .const_param_default_is_slot_layout_hole(db, idx)
                        && let Some(impl_idx) = impl_params.iter().position(|param| param == arg)
                    {
                        roots.insert(impl_idx);
                    }
                }
            }
            for arg in args {
                collect(db, *arg, impl_params, visiting, roots);
            }
            visiting.remove(&ty);
        }

        let mut roots = FxHashSet::default();
        collect(
            db,
            self.selected.self_ty(db),
            self.impl_params(db),
            &mut FxHashSet::default(),
            &mut roots,
        );
        roots
    }

    pub fn instantiated_assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.instantiated.assoc_ty(db, name)
    }
}

impl<'db> TyFoldable<'db> for ResolvedImplInstance<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            selected: self.selected,
            instantiated: self.instantiated.fold_with(db, folder),
            trait_inst: self.trait_inst.fold_with(db, folder),
        }
    }
}

impl<'db> TyVisitable<'db> for ResolvedImplInstance<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.instantiated.visit_with(visitor);
        self.trait_inst.visit_with(visitor);
    }
}

fn instantiate_selected_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    selected: ImplementorId<'db>,
    inst: TraitInstId<'db>,
) -> Option<ResolvedImplInstance<'db>> {
    if matches!(selected.origin(db), ImplementorOrigin::Assumption) {
        return Some(ResolvedImplInstance {
            selected,
            instantiated: selected,
            trait_inst: inst,
        });
    }

    let mut table = UnificationTable::new(db);
    let instantiated = table.instantiate_with_fresh_vars(Binder::bind(selected));
    table.unify(instantiated.trait_inst(db), inst).ok()?;
    Some(ResolvedImplInstance {
        selected,
        instantiated: instantiated.fold_with(db, &mut table),
        trait_inst: inst.fold_with(db, &mut table),
    })
}

pub(crate) fn resolve_trait_impl_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> Selection<ResolvedImplInstance<'db>> {
    let assumptions = solve_cx.assumptions();
    let norm_scope = solve_cx.normalization_scope_for_trait_inst(db, inst);
    let inst = normalize_trait_inst_preserving_validity(db, inst, norm_scope, assumptions);
    match solve_cx.select_impl(db, inst) {
        Selection::Unique(selected) => complete_selected_impl(db, selected)
            .and_then(|selected| instantiate_selected_impl(db, selected, inst))
            .map_or(Selection::NotFound, Selection::Unique),
        // There is deliberately no resolved instance for a non-unique
        // selection. Re-unifying each candidate would both fabricate evidence
        // the caller must not consume and attempt to fold inference variables
        // owned by the caller through a fresh local table.
        Selection::Ambiguous(_) => Selection::Ambiguous(IndexSet::new()),
        Selection::NotFound => Selection::NotFound,
    }
}

/// Resolves the concrete HIR function that implements `method` for the given
/// trait instance, returning both the function and the impl's instantiated
/// generic arguments.
pub fn resolve_trait_method_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    method: IdentId<'db>,
) -> Option<(Func<'db>, Vec<TyId<'db>>)> {
    let resolved = match resolve_trait_impl_instance(db, solve_cx, inst) {
        Selection::Unique(resolved) => resolved,
        Selection::Ambiguous(_ambiguous) => return None,
        Selection::NotFound => return None,
    };
    resolved.method_instance(db, method)
}

pub fn complete_resolved_trait_method_args<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_func: Func<'db>,
    mut impl_args: Vec<TyId<'db>>,
    caller_args: &[TyId<'db>],
    trait_arg_len: usize,
) -> Vec<TyId<'db>> {
    let expected_len = collect_generic_params(db, impl_func.into())
        .params(db)
        .len();
    let missing_len = expected_len.saturating_sub(impl_args.len());
    let tail = caller_args.get(trait_arg_len..).unwrap_or(caller_args);
    impl_args.extend(tail.iter().copied().take(missing_len));
    impl_args
}

/// Returns all implementors for the given `ty` whose constraints are fully proven.
pub(crate) fn impls_for_ty_with_satisfied_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Vec<Binder<ImplementorId<'db>>> {
    impls_for_ty_with_constraint_mode(db, ingot, None, ty, assumptions, false)
}

/// Returns implementors of `trait_def` whose self type can apply to `ty` and
/// whose constraints are not known to be unsatisfied.
pub(crate) fn impls_for_trait_and_ty_with_possible_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_def: Trait<'db>,
    ty: Canonical<TyId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Vec<Binder<ImplementorId<'db>>> {
    impls_for_ty_with_constraint_mode(db, ingot, Some(trait_def), ty, assumptions, true)
}

fn impls_for_ty_with_constraint_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_def: Option<Trait<'db>>,
    ty: Canonical<TyId<'db>>,
    assumptions: PredicateListId<'db>,
    allow_needs_confirmation: bool,
) -> Vec<Binder<ImplementorId<'db>>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);

    let solve_cx = TraitSolveCx::new(db, ingot.root_mod(db).scope()).with_assumptions(assumptions);
    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }
    let env = ingot_trait_env(db, ingot);
    let mut raw_impls = match trait_def {
        Some(trait_def) => env.impls_for_trait(db, trait_def),
        None => env.impls_for_self_key(db, ty.base_ty(db)),
    };

    if ty.as_contract(db).is_some()
        && trait_def.is_none_or(|trait_def| is_std_evm_contract_trait_def(db, trait_def))
    {
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
                let impl_constraints = inst.constraints(db);
                if impl_constraints.is_empty(db) {
                    table.rollback_to(snapshot);
                    return true;
                }

                for &constraint in impl_constraints.list(db) {
                    let constraint = constraint.fold_with(db, &mut table);
                    let satisfiability = is_goal_satisfiable(db, solve_cx, constraint);
                    let constraint_holds =
                        matches!(satisfiability, GoalSatisfiability::Satisfied(_))
                            || (allow_needs_confirmation
                                && matches!(
                                    satisfiability,
                                    GoalSatisfiability::NeedsConfirmation { .. }
                                ));
                    if !constraint_holds {
                        table.rollback_to(snapshot);
                        return false;
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

    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }
    let env = ingot_trait_env(db, ingot);
    let mut raw_impls = env.impls_for_self_key(db, ty.base_ty(db));

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
        .map(|implementor| complete_impl_trait(db, implementor))
        .collect()
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
    let resolved = match resolve_trait_impl_instance(db, solve_cx, inst) {
        Selection::Unique(resolved) => resolved,
        Selection::Ambiguous(_ambiguous) => return None,
        Selection::NotFound => return None,
    };
    let implementor = resolved.selected();
    let hir_impl = match implementor.origin(db) {
        ImplementorOrigin::Hir(impl_trait) => impl_trait,
        ImplementorOrigin::VirtualContract(_) | ImplementorOrigin::Assumption => return None,
    };
    let def = hir_impl
        .hir_consts(db)
        .iter()
        .find(|c| c.name.to_opt() == Some(const_name))?;
    let body = def.value.to_opt()?;

    Some((body, resolved.impl_args(db).to_vec()))
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
    match resolve_trait_impl_instance(db, solve_cx, inst) {
        Selection::Unique(resolved) => !matches!(
            resolved.selected().origin(db),
            ImplementorOrigin::Assumption
        ),
        Selection::Ambiguous(_) | Selection::NotFound => false,
    }
}

/// Represents the trait environment of an ingot, which maintain all trait
/// implementors which can be used in the ingot.
#[derive(Debug, PartialEq, Eq, Clone, Update)]
pub(crate) struct TraitEnv<'db> {
    /// Syntax-only impl candidates grouped by trait definition.
    pub(crate) impls: FxHashMap<Trait<'db>, Vec<ImplTrait<'db>>>,

    /// Syntax-only impl candidates grouped by their nominal self-type root.
    impls_by_self: FxHashMap<ImplSelfKey<'db>, Vec<ImplTrait<'db>>>,

    /// Blanket, alias-rooted, projection-rooted, or otherwise unindexable impls
    /// that must be considered for every receiver type.
    unindexed_self_impls: Vec<ImplTrait<'db>>,

    ingot: Ingot<'db>,
}

impl<'db> TraitEnv<'db> {
    fn collect(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Self {
        let mut impls: FxHashMap<Trait<'db>, Vec<ImplTrait<'db>>> = FxHashMap::default();
        let mut impls_by_self: FxHashMap<ImplSelfKey<'db>, Vec<ImplTrait<'db>>> =
            FxHashMap::default();
        let mut unindexed_self_impls = Vec::new();

        for impl_map in ingot
            .resolved_external_ingots(db)
            .iter()
            .map(|(_, external)| collect_trait_impls(db, *external))
            .chain(std::iter::once(collect_trait_impls(db, ingot)))
        {
            // Raw collection is syntax-only. Candidate lookup performs semantic
            // lowering after this environment has been fully assembled.
            for (trait_def, implementors) in &impl_map.by_trait {
                impls
                    .entry(*trait_def)
                    .or_default()
                    .extend(implementors.iter().copied());
            }
            for (self_key, implementors) in &impl_map.by_self {
                impls_by_self
                    .entry(*self_key)
                    .or_default()
                    .extend(implementors.iter().copied());
            }
            unindexed_self_impls.extend(impl_map.unindexed_self.iter().copied());
        }

        Self {
            impls,
            impls_by_self,
            unindexed_self_impls,
            ingot,
        }
    }

    fn impls_for_self_key(
        &self,
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Vec<Binder<ImplementorId<'db>>> {
        let key = match ty.data(db) {
            TyData::TyBase(TyBase::Prim(prim)) => Some(ImplSelfKey::Prim(*prim)),
            TyData::TyBase(TyBase::Adt(adt)) => Some(ImplSelfKey::Item(adt.scope(db))),
            TyData::TyBase(TyBase::Contract(contract)) => Some(ImplSelfKey::Item(contract.scope())),
            _ => None,
        };
        key.and_then(|key| self.impls_by_self.get(&key))
            .into_iter()
            .flatten()
            .chain(&self.unindexed_self_impls)
            .filter_map(|impl_trait| lower_impl_trait_header(db, *impl_trait))
            .collect()
    }

    fn impls_for_trait(
        &self,
        db: &'db dyn HirAnalysisDb,
        trait_def: Trait<'db>,
    ) -> Vec<Binder<ImplementorId<'db>>> {
        self.impls
            .get(&trait_def)
            .into_iter()
            .flatten()
            .filter_map(|impl_trait| lower_impl_trait_header(db, *impl_trait))
            .collect()
    }
}

/// Represents a slim, internal view of a trait impl, derived from an
/// `ImplTrait` item and its lowered trait instance.
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
    pub(crate) fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.trait_(db).self_ty(db)
    }

    /// Trait instance realized by this impl, including its associated type definitions.
    pub(crate) fn trait_inst(self, db: &'db dyn HirAnalysisDb) -> TraitInstId<'db> {
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
                collect_candidate_constraints(db, impl_trait.into())
                    .instantiate(db, self.params(db))
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
        if !matches!(self.origin(db), ImplementorOrigin::Hir(_)) {
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
    let solve_cx = TraitSolveCx::new(db, a.trait_def(db).scope())
        .with_assumptions(PredicateListId::empty_list(db));

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
    use common::indexmap::IndexMap;

    use super::{TraitInstId, impls_for_trait_def, resolve_trait_impl_instance};
    use crate::analysis::ty::trait_resolution::{Selection, TraitSolveCx};
    use crate::hir_def::{IdentId, ItemKind};
    use crate::test_db::HirAnalysisTestDb;

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

    #[test]
    fn resolved_impl_instance_preserves_default_assoc_template_and_concrete_args() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "resolved_impl_instance_preserves_default_assoc_template_and_concrete_args.fe",
            ),
            r#"
trait Projects<T> {
    type Output = T
}

struct Wrapper<T> {}

impl<T> Projects<T> for Wrapper<T> {}

fn probe(value: own Wrapper<u16>) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let trait_ = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_) => Some(trait_),
                _ => None,
            })
            .expect("missing Projects trait");
        let func = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func),
                _ => None,
            })
            .expect("missing probe function");
        let self_ty = func
            .params(&db)
            .next()
            .expect("missing probe parameter")
            .ty(&db);
        let projected_arg = self_ty.generic_args(&db)[0];
        let inst = TraitInstId::new(&db, trait_, vec![self_ty, projected_arg], IndexMap::new());
        let Selection::Unique(resolved) =
            resolve_trait_impl_instance(&db, TraitSolveCx::new(&db, func.scope()), inst)
        else {
            panic!("expected one concrete Projects implementation");
        };
        let output = IdentId::new(&db, "Output");
        let template = resolved
            .assoc_ty_template(&db, output)
            .expect("missing default associated type template");
        let instantiated = resolved
            .instantiated_assoc_ty(&db, output)
            .expect("missing instantiated default associated type");

        assert!(template.has_param(&db));
        assert_eq!(instantiated.pretty_print(&db).to_string(), "u16");
        assert_eq!(resolved.impl_args(&db), &[instantiated]);
    }
}
