use super::{
    canonical::{Canonical, Canonicalized, Solution},
    fold::{AssocTySubst, TyFoldable},
    trait_def::TraitInstId,
    ty_def::{TyFlags, TyId},
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        normalize::normalize_ty,
        trait_resolution::{constraint::ty_constraints, proof_forest::ProofForest},
        unify::UnificationTable,
        visitor::collect_flags,
    },
};
use crate::{Ingot, hir_def::HirIngot};
use common::indexmap::IndexSet;
use constraint::collect_constraints;
use salsa::Update;

pub(crate) mod constraint;
mod proof_forest;

#[salsa::tracked(return_ref)]
pub fn is_goal_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    goal: Canonical<TraitInstId<'db>>,
    assumptions: PredicateListId<'db>,
) -> GoalSatisfiability<'db> {
    let flags = collect_flags(db, goal.value);
    if flags.contains(TyFlags::HAS_INVALID) {
        return GoalSatisfiability::ContainsInvalid;
    };

    ProofForest::new(db, ingot, goal, assumptions).solve()
}

/// Checks if the given type is well-formed, i.e., the arguments of the given
/// type applications satisfies the constraints under the given assumptions.
#[salsa::tracked]
pub(crate) fn check_ty_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: TyId<'db>,
    assumptions: PredicateListId<'db>,
) -> WellFormedness<'db> {
    let (_, args) = ty.decompose_ty_app(db);

    for &arg in args {
        let wf = check_ty_wf(db, ingot, arg, assumptions);
        if !wf.is_wf() {
            return wf;
        }
    }

    let constraints = ty_constraints(db, ty);

    // Normalize constraints to resolve associated types
    let normalized_constraints = {
        // Get a reasonable scope for normalization
        let scope = ingot.root_mod(db).scope();
        let normalized_list: Vec<_> = constraints
            .list(db)
            .iter()
            .map(|&goal| {
                // Normalize each argument in the goal
                let normalized_args: Vec<_> = goal
                    .args(db)
                    .iter()
                    .map(|&arg| normalize_ty(db, arg, scope, assumptions))
                    .collect();
                TraitInstId::new(
                    db,
                    goal.def(db),
                    normalized_args,
                    goal.assoc_type_bindings(db).clone(),
                )
            })
            .collect();
        PredicateListId::new(db, normalized_list)
    };

    for &goal in normalized_constraints.list(db) {
        let mut table = UnificationTable::new(db);
        let canonical_goal = Canonicalized::new(db, goal);

        if let GoalSatisfiability::UnSat(subgoal) =
            is_goal_satisfiable(db, ingot, canonical_goal.value, assumptions)
        {
            let subgoal =
                subgoal.map(|subgoal| canonical_goal.extract_solution(&mut table, subgoal));
            return WellFormedness::IllFormed { goal, subgoal };
        }
    }

    WellFormedness::WellFormed
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
pub(crate) enum WellFormedness<'db> {
    WellFormed,
    IllFormed {
        goal: TraitInstId<'db>,
        subgoal: Option<TraitInstId<'db>>,
    },
}

impl WellFormedness<'_> {
    fn is_wf(self) -> bool {
        matches!(self, WellFormedness::WellFormed)
    }
}

/// Checks if the given trait instance are well-formed, i.e., the arguments of
/// the trait satisfies all constraints under the given assumptions.
#[salsa::tracked]
pub(crate) fn check_trait_inst_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_inst: TraitInstId<'db>,
    assumptions: PredicateListId<'db>,
) -> WellFormedness<'db> {
    let constraints = collect_constraints(db, trait_inst.def(db).trait_(db).into())
        .instantiate(db, trait_inst.args(db));

    // Normalize constraints after instantiation to resolve associated types
    let normalized_constraints = {
        let scope = trait_inst.ingot(db).root_mod(db).scope();
        let normalized_list: Vec<_> = constraints
            .list(db)
            .iter()
            .map(|&goal| {
                // Normalize each argument in the goal
                let normalized_args: Vec<_> = goal
                    .args(db)
                    .iter()
                    .map(|&arg| normalize_ty(db, arg, scope, assumptions))
                    .collect();
                TraitInstId::new(
                    db,
                    goal.def(db),
                    normalized_args,
                    goal.assoc_type_bindings(db).clone(),
                )
            })
            .collect();
        PredicateListId::new(db, normalized_list)
    };

    for &goal in normalized_constraints.list(db) {
        let mut table = UnificationTable::new(db);
        let canonical_goal = Canonicalized::new(db, goal);
        if let GoalSatisfiability::UnSat(subgoal) =
            is_goal_satisfiable(db, ingot, canonical_goal.value, assumptions)
        {
            let subgoal =
                subgoal.map(|subgoal| canonical_goal.extract_solution(&mut table, subgoal));
            return WellFormedness::IllFormed { goal, subgoal };
        }
    }

    WellFormedness::WellFormed
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub enum GoalSatisfiability<'db> {
    /// Goal is satisfied with the unique solution.
    Satisfied(Solution<TraitInstId<'db>>),
    /// Goal might be satisfied, but needs more type information to determine
    /// satisfiability and uniqueness.
    NeedsConfirmation(IndexSet<Solution<TraitInstId<'db>>>),

    /// Goal contains invalid.
    ContainsInvalid,
    /// The gaol is not satisfied.
    /// It contains an unsatisfied subgoal if we can know the exact subgoal
    /// that makes the proof step stuck.
    UnSat(Option<Solution<TraitInstId<'db>>>),
}

impl GoalSatisfiability<'_> {
    pub fn is_satisfied(&self) -> bool {
        matches!(
            self,
            Self::Satisfied(_) | Self::NeedsConfirmation(_) | Self::ContainsInvalid
        )
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct PredicateListId<'db> {
    #[return_ref]
    pub list: Vec<TraitInstId<'db>>,
}

impl<'db> PredicateListId<'db> {
    pub fn pretty_print(&self, db: &'db dyn HirAnalysisDb) -> String {
        format!(
            "{{{}}}",
            self.list(db)
                .iter()
                .map(|pred| pred.pretty_print(db, true))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    pub(super) fn merge(self, db: &'db dyn HirAnalysisDb, other: Self) -> Self {
        let mut predicates = self.list(db).clone();
        predicates.extend(other.list(db));
        PredicateListId::new(db, predicates)
    }

    pub fn empty_list(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new())
    }

    pub fn is_empty(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.list(db).is_empty()
    }

    /// Transitively extends the predicate list with all implied bounds:
    /// - Super trait bounds
    /// - Associated type bounds from trait definitions
    pub fn extend_all_bounds(self, db: &'db dyn HirAnalysisDb) -> Self {
        let mut all_predicates: IndexSet<TraitInstId<'db>> =
            self.list(db).iter().copied().collect();

        let mut worklist: Vec<TraitInstId<'db>> = self.list(db).to_vec();

        while let Some(pred) = worklist.pop() {
            // 1. Collect super traits
            for &super_trait in pred.def(db).super_traits(db).iter() {
                // Instantiate with current predicate's args
                let inst = super_trait.instantiate(db, pred.args(db));

                // Also substitute `Self` and associated types using current predicate's
                // assoc-type bindings so derived bounds are as concrete as possible.
                let mut subst = AssocTySubst::new(pred);
                let inst = inst.fold_with(db, &mut subst);

                if all_predicates.insert(inst) {
                    // New predicate added, add to worklist for further processing
                    worklist.push(inst);
                }
            }

            // 2. Collect associated type bounds
            let hir_trait = pred.def(db).trait_(db);
            for trait_type in hir_trait.assoc_types(db) {
                // Get the associated type name
                let Some(assoc_ty_name) = trait_type.name(db) else {
                    continue;
                };

                // Create the associated type: Self::AssocType
                let assoc_ty = TyId::assoc_ty(db, pred, assoc_ty_name);

                let _assumptions =
                    PredicateListId::new(db, all_predicates.iter().copied().collect::<Vec<_>>());

                for mut trait_inst in trait_type.with_subject(assoc_ty).bounds(db) {
                        // Substitute `Self` and associated types using the original predicate instance
                        let mut subst = AssocTySubst::new(pred);
                        trait_inst = trait_inst.fold_with(db, &mut subst);
                        if all_predicates.insert(trait_inst) {
                            worklist.push(trait_inst);
                        }
                }
            }
        }

        Self::new(db, all_predicates.into_iter().collect::<Vec<_>>())
    }
}
