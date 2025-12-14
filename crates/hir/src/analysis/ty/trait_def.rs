//! This module contains all trait related types definitions.

use crate::{
    analysis::ty::{
        method_cmp::compare_impl_method,
        trait_lower::collect_trait_impls,
        trait_resolution::{GoalSatisfiability, PredicateListId},
    },
    hir_def::{Contract, Func, HirIngot, IdentId, ImplTrait, Trait},
};
use common::{
    indexmap::{IndexMap, IndexSet},
    ingot::{Ingot, IngotKind},
};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    canonical::{Canonical, Canonicalized},
    diagnostics::{ImplDiag, TyDiagCollection},
    fold::TyFoldable as _,
    trait_lower::collect_implementor_methods,
    trait_resolution::{constraint::collect_constraints, is_goal_satisfiable},
    ty_def::TyId,
    unify::UnificationTable,
};
use crate::analysis::HirAnalysisDb;

/// Returns [`TraitEnv`] for the given ingot.
#[salsa::tracked(return_ref, cycle_fn=ingot_trait_env_cycle_recover, cycle_initial=ingot_trait_env_cycle_initial)]
pub(crate) fn ingot_trait_env<'db>(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> TraitEnv<'db> {
    TraitEnv::collect(db, ingot)
}

/// Returns all implementors for the given trait inst.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_: Canonical<TraitInstId<'db>>,
) -> Vec<Binder<ImplementorId<'db>>> {
    let mut table = UnificationTable::new(db);
    let trait_ = trait_.extract_identity(&mut table);

    let env = ingot_trait_env(db, ingot);
    let mut out: Vec<_> = env
        .impls
        .get(&trait_.def(db))
        .into_iter()
        .flatten()
        .filter(|impl_| {
            let snapshot = table.snapshot();
            let inst = table.instantiate_with_fresh_vars(**impl_);
            let is_ok = table.unify(inst.trait_inst(db), trait_).is_ok();
            table.rollback_to(snapshot);
            is_ok
        })
        .cloned()
        .collect();

    if is_std_evm_contract_trait_def(db, trait_.def(db)) {
        for impl_ in contract_virtual_impls(db, ingot).iter().copied() {
            let snapshot = table.snapshot();
            let inst = table.instantiate_with_fresh_vars(impl_);
            let is_ok = table.unify(inst.trait_inst(db), trait_).is_ok();
            table.rollback_to(snapshot);
            if is_ok {
                out.push(impl_);
            }
        }
    }

    out
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
pub(crate) enum ImplementorOrigin<'db> {
    Hir(ImplTrait<'db>),
    VirtualContract(Contract<'db>),
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

/// Resolves the concrete HIR function that implements `method` for the given trait inst.
pub fn resolve_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    method: IdentId<'db>,
) -> Option<Func<'db>> {
    let canonical = Canonical::new(db, inst);

    // Search Self's ingot, and the trait's ingot.
    for ingot in [inst.self_ty(db).ingot(db), Some(inst.def(db).ingot(db))] {
        let Some(ingot) = ingot else { continue };

        for implementor in impls_for_trait(db, ingot, canonical) {
            let implementor = implementor.instantiate_identity();
            if let Some(&func) = implementor.methods(db).get(&method) {
                return Some(func);
            }
        }
    }
    None
}

/// Resolves the concrete HIR function that implements `method` for the given
/// trait instance, returning both the function and the impl's instantiated
/// generic arguments.
pub fn resolve_trait_method_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    method: IdentId<'db>,
) -> Option<(Func<'db>, Vec<TyId<'db>>)> {
    // Normalize the trait instance arguments before searching for implementors.
    //
    // This is important for cases where the `Self` type is an associated type
    // projection (e.g. `<Sol as Abi>::Decoder<I>`). Without normalization, the
    // projection has no declaring ingot, which prevents us from searching the
    // ingot that contains the actual implementor type (e.g. `SolDecoder<I>`).
    //
    // Monomorphization happens with concrete substitutions, so we can safely
    // normalize using an ingot picked from the instantiated arguments.
    let norm_scope = inst
        .self_ty(db)
        .ingot(db)
        .or_else(|| inst.args(db).iter().find_map(|arg| arg.ingot(db)))
        .map(|ingot| ingot.root_mod(db).scope())
        .unwrap_or_else(|| {
            // Fall back to the trait definition's ingot.
            inst.def(db).ingot(db).root_mod(db).scope()
        });
    let assumptions = PredicateListId::empty_list(db);
    let inst = inst.normalize(db, norm_scope, assumptions);

    let canonical = Canonical::new(db, inst);

    // Search Self's ingot, and the trait's ingot.
    for ingot in [inst.self_ty(db).ingot(db), Some(inst.def(db).ingot(db))] {
        let Some(ingot) = ingot else { continue };

        for implementor in impls_for_trait(db, ingot, canonical) {
            let mut table = UnificationTable::new(db);
            let implementor = table.instantiate_with_fresh_vars(*implementor);
            if table.unify(implementor.trait_inst(db), inst).is_err() {
                continue;
            }
            if let Some(func) = implementor.methods(db).get(&method) {
                let impl_args = implementor
                    .params(db)
                    .iter()
                    .map(|&ty| ty.fold_with(db, &mut table))
                    .collect::<Vec<_>>();
                return Some((*func, impl_args));
            }
        }
    }
    None
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
    if ty.has_invalid(db) {
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
                    let constraint = Canonicalized::new(db, constraint);
                    match is_goal_satisfiable(db, ingot, constraint.value, assumptions) {
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
    if ty.has_invalid(db) {
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

/// Looks up the HIR body for an associated const defined in the selected trait impl, if unique.
pub fn assoc_const_body_for_trait_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    const_name: IdentId<'db>,
) -> Option<crate::hir_def::Body<'db>> {
    let mut match_body: Option<crate::hir_def::Body<'db>> = None;
    let canonical_self_ty = Canonical::new(db, inst.self_ty(db));

    for ingot in [inst.self_ty(db).ingot(db), Some(inst.def(db).ingot(db))] {
        let Some(ingot) = ingot else { continue };
        for implementor in impls_for_ty(db, ingot, canonical_self_ty).iter() {
            // Instantiate and unify against the requested trait instance. This allows
            // associated const lookups to work for generic impls like
            // `impl<const N: usize> AbiSize for String<N>`.
            let mut table = UnificationTable::new(db);
            let implementor = table.instantiate_with_fresh_vars(*implementor);
            if table.unify(implementor.trait_inst(db), inst).is_err() {
                continue;
            }

            let hir_impl = match implementor.origin(db) {
                ImplementorOrigin::Hir(impl_trait) => impl_trait,
                ImplementorOrigin::VirtualContract(_) => continue,
            };
            let Some(def) = hir_impl
                .hir_consts(db)
                .iter()
                .find(|c| c.name.to_opt() == Some(const_name))
            else {
                continue;
            };

            let Some(body) = def.value.to_opt() else {
                continue;
            };

            match match_body {
                None => match_body = Some(body),
                Some(existing) if existing == body => {}
                Some(_) => return None,
            }
        }
    }
    match_body
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
            // `collect_trait_impls` ensures that there are no conflicting impls, so we can
            // just extend the map.
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
#[salsa::interned]
#[derive(Debug)]
pub(crate) struct ImplementorId<'db> {
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
                collect_constraints(db, impl_trait.into()).instantiate(db, self.params(db))
            }
            ImplementorOrigin::VirtualContract(_) => PredicateListId::empty_list(db),
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

    let ingot = a.trait_def(db).ingot(db);

    // Check if all constraints from both implementations would be satisfiable
    // when the types are unified.
    let merged_constraints = a_constraints.merge(db, b_constraints);

    for &constraint in merged_constraints.list(db) {
        let constraint = Canonicalized::new(db, constraint.fold_with(db, &mut table));

        match is_goal_satisfiable(db, ingot, constraint.value, PredicateListId::empty_list(db)) {
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

    pub(crate) fn ingot(self, db: &'db dyn HirAnalysisDb) -> Ingot<'db> {
        self.def(db).ingot(db)
    }
}

// Represents a trait definition.
// (TraitDef struct and impl removed)

// (TraitMethod struct and impl removed)
