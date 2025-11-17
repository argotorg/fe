//! This module contains all trait related types definitions.

use crate::{
    hir_def::{HirIngot, IdentId, ImplTrait, Trait},
    span::DynLazySpan,
};
use common::{
    indexmap::{IndexMap, IndexSet},
    ingot::Ingot,
};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    canonical::{Canonical, Canonicalized},
    diagnostics::{TraitConstraintDiag, TyDiagCollection},
    fold::TyFoldable as _,
    func_def::{CallableDef, lower_func},
    trait_lower::collect_implementor_methods,
    trait_resolution::{
        GoalSatisfiability, PredicateListId, WellFormedness, check_trait_inst_wf,
        constraint::{collect_constraints, collect_super_traits},
        is_goal_satisfiable,
    },
    ty_def::{Kind, TyId},
    ty_lower::GenericParamTypeSet,
    unify::UnificationTable,
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        trait_lower::collect_trait_impls, trait_resolution::constraint::super_trait_cycle,
        ty_lower::collect_generic_params,
    },
};

/// Returns [`TraitEnv`] for the given ingot.
#[salsa::tracked(return_ref, cycle_fn=ingot_trait_env_cycle_recover, cycle_initial=ingot_trait_env_cycle_initial)]
pub(crate) fn ingot_trait_env<'db>(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> TraitEnv<'db> {
    TraitEnv::collect(db, ingot)
}

/// Returns all [`Implementor`] for the given trait inst.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_: Canonical<TraitInstId<'db>>,
) -> Vec<Binder<Implementor<'db>>> {
    let mut table = UnificationTable::new(db);
    let trait_ = trait_.extract_identity(&mut table);

    let env = ingot_trait_env(db, ingot);
    let Some(impls) = env.impls.get(&trait_.def(db)) else {
        return vec![];
    };

    impls
        .iter()
        .filter(|impl_| {
            let snapshot = table.snapshot();
            let impl_ = table.instantiate_with_fresh_vars(**impl_);
            let is_ok = table.unify(impl_.trait_(db), trait_).is_ok();
            table.rollback_to(snapshot);
            is_ok
        })
        .cloned()
        .collect()
}

/// Returns all [`Implementor`] for the given `ty` that satisfy the given assumptions.
pub(crate) fn impls_for_ty_with_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Vec<Binder<Implementor<'db>>> {
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

    cands
        .into_iter()
        .flatten()
        .copied()
        .filter(|impl_| {
            let snapshot = table.snapshot();

            let impl_ = table.instantiate_with_fresh_vars(*impl_);
            let impl_ty = table.instantiate_to_term(impl_.self_ty(db));
            let ty = table.instantiate_to_term(ty);
            let unifies = table.unify(impl_ty, ty).is_ok();

            if unifies {
                // Filter out impls that don't satisfy assumptions
                let impl_constraints = impl_.constraints(db);
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

/// Returns all [`Implementor`] for the given `ty`.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
) -> Vec<Binder<Implementor<'db>>> {
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

    cands
        .into_iter()
        .flatten()
        .copied()
        .filter(|impl_| {
            let snapshot = table.snapshot();

            let impl_ = table.instantiate_with_fresh_vars(*impl_);
            let impl_ty = table.instantiate_to_term(impl_.self_ty(db));
            let ty = table.instantiate_to_term(ty);
            let is_ok = table.unify(impl_ty, ty).is_ok();

            table.rollback_to(snapshot);

            is_ok
        })
        .collect()
}

/// Represents the trait environment of an ingot, which maintain all trait
/// implementors which can be used in the ingot.
#[derive(Debug, PartialEq, Eq, Clone, Update)]
pub(crate) struct TraitEnv<'db> {
    pub(super) impls: FxHashMap<TraitDef<'db>, Vec<Binder<Implementor<'db>>>>,
    hir_to_implementor: FxHashMap<ImplTrait<'db>, Binder<Implementor<'db>>>,

    /// This maintains a mapping from the base type to the implementors.
    ty_to_implementors: FxHashMap<Binder<TyId<'db>>, Vec<Binder<Implementor<'db>>>>,

    ingot: Ingot<'db>,
}

impl<'db> TraitEnv<'db> {
    fn collect(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Self {
        let mut impls: FxHashMap<_, Vec<Binder<Implementor>>> = FxHashMap::default();
        let mut hir_to_implementor: FxHashMap<ImplTrait, Binder<Implementor>> =
            FxHashMap::default();
        let mut ty_to_implementors: FxHashMap<Binder<TyId>, Vec<Binder<Implementor>>> =
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

                hir_to_implementor.extend(implementors.iter().map(|implementor| {
                    (implementor.skip_binder().hir_impl_trait(db), *implementor)
                }));

                for implementor in implementors {
                    ty_to_implementors
                        .entry(Binder::bind(
                            implementor.instantiate_identity().self_ty(db).base_ty(db),
                        ))
                        .or_default()
                        .push(*implementor);
                }
            }
        }

        Self {
            impls,
            hir_to_implementor,
            ty_to_implementors,
            ingot,
        }
    }

    /// Returns the corresponding implementor of the given `impl Trait` type.
    pub(crate) fn map_impl_trait(&self, trait_ref: ImplTrait) -> Option<Binder<Implementor<'_>>> {
        self.hir_to_implementor.get(&trait_ref).copied()
    }
}

/// Represents an implementor of a trait, which can be thought of as a lowered
/// `impl Trait`.
#[salsa::interned]
#[derive(Debug)]
pub(crate) struct Implementor<'db> {
    /// The trait that this implementor implements.
    pub(crate) trait_: TraitInstId<'db>,

    /// The type parameters of this implementor.
    #[return_ref]
    pub(crate) params: Vec<TyId<'db>>,

    #[return_ref]
    pub(crate) types: IndexMap<IdentId<'db>, TyId<'db>>,

    /// The original hir.
    pub(crate) hir_impl_trait: ImplTrait<'db>,
}

impl<'db> Implementor<'db> {
    pub(crate) fn assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.types(db).get(&name).copied()
    }

    /// Returns the trait definition that this implementor implements.
    pub(crate) fn trait_def(self, db: &'db dyn HirAnalysisDb) -> TraitDef<'db> {
        self.trait_(db).def(db)
    }

    /// The self type of the impl trait.
    pub(crate) fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.trait_(db).self_ty(db)
    }

    /// Returns the constraints that the implementor requires when the
    /// implementation is selected.
    pub(super) fn constraints(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        collect_constraints(db, self.hir_impl_trait(db).into()).instantiate(db, self.params(db))
    }

    pub(super) fn methods(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db IndexMap<IdentId<'db>, CallableDef<'db>> {
        collect_implementor_methods(db, self)
    }

    #[allow(dead_code)]
    /// Pretty print the implementor in the format `impl Foo<u16> for SomeType { type A = X; type B = Y }`
    pub(crate) fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        let mut s = String::from("impl ");

        // Add the trait name with its generic arguments
        s.push_str(&self.trait_(db).pretty_print(db, false));

        // Add "for" and the self type
        s.push_str(" for ");
        s.push_str(self.self_ty(db).pretty_print(db));

        // Add associated types if any
        if !self.types(db).is_empty() {
            s.push_str(" { ");
            let mut first = true;
            for (name, ty) in self.types(db) {
                if !first {
                    s.push_str("; ");
                }
                first = false;
                s.push_str("type ");
                s.push_str(name.data(db));
                s.push_str(" = ");
                s.push_str(ty.pretty_print(db));
            }
            s.push_str(" }");
        }

        s
    }
}

/// Returns `true` if the given two implementor conflicts.
pub(super) fn does_impl_trait_conflict(
    db: &dyn HirAnalysisDb,
    a: Binder<Implementor>,
    b: Binder<Implementor>,
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
    // when the types are unified
    let merged_constraints = a_constraints.merge(db, b_constraints);

    // Check each constraint to see if it would be satisfiable
    for &constraint in merged_constraints.list(db) {
        let constraint = Canonicalized::new(db, constraint.fold_with(db, &mut table));

        // Check if this constraint is satisfiable
        match is_goal_satisfiable(db, ingot, constraint.value, PredicateListId::empty_list(db)) {
            GoalSatisfiability::UnSat(_) => {
                return false;
            }
            GoalSatisfiability::ContainsInvalid => {
                return false;
            }
            _ => {
                // Constraint is satisfiable or needs more information, continue checking
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
    pub def: TraitDef<'db>,
    /// Regular type and const parameters: [Self, ExplicitTypeParam1, ..., ExplicitConstParamN]
    #[return_ref]
    pub args: Vec<TyId<'db>>,

    /// Associated type bounds specified by user, eg `Iterator<Item=i32>`
    #[return_ref]
    pub assoc_type_bindings: IndexMap<IdentId<'db>, TyId<'db>>,
}

impl<'db> TraitInstId<'db> {
    pub fn with_fresh_vars(
        db: &'db dyn HirAnalysisDb,
        def: TraitDef<'db>,
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
        if self.def(db).trait_(db).assoc_ty(db, name).is_some() {
            return Some(TyId::assoc_ty(db, self, name));
        }
        None
    }

    pub fn pretty_print(self, db: &dyn HirAnalysisDb, as_pred: bool) -> String {
        if as_pred {
            let inst = self.pretty_print(db, false);
            let self_ty = self.self_ty(db);
            format! {"{}: {}", self_ty.pretty_print(db), inst}
        } else {
            let mut s = self.def(db).name(db).unwrap_or("<unknown>").to_string();

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

    pub(super) fn ingot(self, db: &'db dyn HirAnalysisDb) -> Ingot<'db> {
        self.def(db).ingot(db)
    }

    pub(super) fn emit_sat_diag(
        self,
        db: &'db dyn HirAnalysisDb,
        ingot: Ingot<'db>,
        assumptions: PredicateListId<'db>,
        span: DynLazySpan<'db>,
    ) -> Option<TyDiagCollection<'db>> {
        if let WellFormedness::IllFormed { goal, subgoal } =
            check_trait_inst_wf(db, ingot, self, assumptions)
        {
            Some(
                TraitConstraintDiag::TraitBoundNotSat {
                    span,
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                }
                .into(),
            )
        } else {
            None
        }
    }
}

/// Represents a trait definition.
#[salsa::tracked]
#[derive(Debug)]
pub struct TraitDef<'db> {
    pub trait_: Trait<'db>,
}

#[salsa::tracked]
impl<'db> TraitDef<'db> {
    #[salsa::tracked(return_ref)]
    pub fn methods(self, db: &'db dyn HirAnalysisDb) -> IndexMap<IdentId<'db>, TraitMethod<'db>> {
        let mut methods = IndexMap::<IdentId<'db>, TraitMethod<'db>>::default();
        for method in self.trait_(db).methods(db) {
            let Some(func) = lower_func(db, method) else {
                continue;
            };
            let Some(name) = func.name(db) else {
                continue;
            };
            let trait_method = TraitMethod(func);
            // We can simply ignore the conflict here because it's already
            // handled by the def analysis pass
            methods.entry(name).or_insert(trait_method);
        }
        methods
    }

    pub fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).params(db)
    }

    #[salsa::tracked(return_ref)]
    pub fn param_set(self, db: &'db dyn HirAnalysisDb) -> GenericParamTypeSet<'db> {
        collect_generic_params(db, self.trait_(db).into())
    }

    pub fn self_param(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.param_set(db).trait_self(db).unwrap()
    }

    pub fn original_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).explicit_params(db)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, salsa::Update)]
pub struct TraitMethod<'db>(pub CallableDef<'db>);

impl TraitMethod<'_> {
    pub fn has_default_impl(self, db: &dyn HirAnalysisDb) -> bool {
        match self.0 {
            CallableDef::Func(func) => func.body(db).is_some(),
            CallableDef::VariantCtor(_) => false,
        }
    }
}

impl<'db> TraitDef<'db> {
    /// Returns the type kind that implementor type must have.
    pub(crate) fn expected_implementor_kind(self, db: &'db dyn HirAnalysisDb) -> &'db Kind {
        self.self_param(db).kind(db)
    }

    /// Returns `ingot` in which this trait is defined.
    pub(crate) fn ingot(self, db: &'db dyn HirAnalysisDb) -> Ingot<'db> {
        self.trait_(db).top_mod(db).ingot(db)
    }

    pub fn super_traits(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db IndexSet<Binder<TraitInstId<'db>>> {
        use std::sync::OnceLock;
        static EMPTY: OnceLock<IndexSet<Binder<TraitInstId>>> = OnceLock::new();

        if super_trait_cycle(db, self).is_some() {
            EMPTY.get_or_init(IndexSet::new)
        } else {
            collect_super_traits(db, self)
        }
    }

    fn name(self, db: &'db dyn HirAnalysisDb) -> Option<&'db str> {
        self.trait_(db)
            .name(db)
            .to_opt()
            .map(|name| name.data(db).as_str())
    }
}

fn ingot_trait_env_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> TraitEnv<'db> {
    // Return an empty trait environment when we detect a cycle
    TraitEnv {
        impls: FxHashMap::default(),
        hir_to_implementor: FxHashMap::default(),
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
