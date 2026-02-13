use super::{
    canonical::{Canonical, Canonicalized, Solution},
    fold::{AssocTySubst, TyFoldable},
    trait_def::{ImplementorId, TraitInstId},
    ty_def::{TyData, TyFlags, TyId},
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        trait_resolution::{constraint::ty_constraints, proof_forest::ProofForest},
        unify::UnificationTable,
        visitor::collect_flags,
    },
};
use crate::{
    Ingot,
    hir_def::{HirIngot, scope_graph::ScopeId},
};
use common::indexmap::IndexSet;
use constraint::collect_constraints;
use salsa::Update;

pub(crate) mod constraint;
mod proof_forest;

#[derive(Debug, Clone)]
pub(crate) enum Selection<T> {
    Unique(T),
    Ambiguous(IndexSet<T>),
    NotFound,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct TraitSolveCx<'db> {
    origin_ingot: Ingot<'db>,
    assumptions: PredicateListId<'db>,
}

impl<'db> TraitSolveCx<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Self {
        Self {
            origin_ingot: scope.ingot(db),
            assumptions: PredicateListId::empty_list(db),
        }
    }

    pub fn with_assumptions(self, assumptions: PredicateListId<'db>) -> Self {
        Self {
            assumptions,
            ..self
        }
    }

    pub fn assumptions(self) -> PredicateListId<'db> {
        self.assumptions
    }

    pub(crate) fn select_impl(
        self,
        db: &'db dyn HirAnalysisDb,
        inst: TraitInstId<'db>,
    ) -> Selection<ImplementorId<'db>> {
        let scope = self.normalization_scope_for_trait_inst(db, inst);
        let inst = inst.normalize(db, scope, self.assumptions);
        let goal = Canonical::new(db, inst);
        match is_goal_satisfiable(db, self, goal) {
            GoalSatisfiability::Satisfied(solution) => {
                Selection::Unique(solution.value.implementor)
            }
            GoalSatisfiability::NeedsConfirmation(ambiguous) => {
                Selection::Ambiguous(ambiguous.iter().map(|s| s.value.implementor).collect())
            }
            GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_) => {
                Selection::NotFound
            }
        }
    }

    pub(crate) fn search_ingots_for_trait_inst(
        self,
        db: &'db dyn HirAnalysisDb,
        inst: TraitInstId<'db>,
    ) -> (Ingot<'db>, Option<Ingot<'db>>) {
        let trait_ingot = inst.def(db).ingot(db);
        let self_ty = inst.self_ty(db);
        let self_ingot = self_ty.ingot(db).or_else(|| {
            // For projection `Self` types that still don't yield an ingot (e.g. all-trait-param
            // args), fall back to other trait arguments as a best-effort proxy.
            match self_ty.data(db) {
                TyData::AssocTy(_) | TyData::QualifiedTy(_) => {
                    inst.args(db).iter().skip(1).find_map(|ty| ty.ingot(db))
                }
                _ => None,
            }
        });

        let primary = self_ingot.unwrap_or(self.origin_ingot);
        if primary == trait_ingot {
            (primary, None)
        } else {
            (primary, Some(trait_ingot))
        }
    }

    pub(crate) fn normalization_scope_for_trait_inst(
        self,
        db: &'db dyn HirAnalysisDb,
        inst: TraitInstId<'db>,
    ) -> ScopeId<'db> {
        let norm_ingot = inst
            .self_ty(db)
            .ingot(db)
            .or_else(|| inst.args(db).iter().find_map(|ty| ty.ingot(db)))
            .unwrap_or(self.origin_ingot);
        norm_ingot.root_mod(db).scope()
    }

    pub(crate) fn origin_scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.origin_ingot.root_mod(db).scope()
    }
}

#[salsa::tracked(return_ref)]
pub fn is_goal_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: Canonical<TraitInstId<'db>>,
) -> GoalSatisfiability<'db> {
    let flags = collect_flags(db, goal.value);
    if flags.contains(TyFlags::HAS_INVALID) {
        return GoalSatisfiability::ContainsInvalid;
    };

    ProofForest::new(db, solve_cx, goal).solve()
}

/// Checks if the given type is well-formed, i.e., the arguments of the given
/// type applications satisfies the constraints under the given assumptions.
#[salsa::tracked]
pub(crate) fn check_ty_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    ty: TyId<'db>,
) -> WellFormedness<'db> {
    let (_, args) = ty.decompose_ty_app(db);

    for &arg in args {
        let wf = check_ty_wf(db, solve_cx, arg);
        if !wf.is_wf() {
            return wf;
        }
    }

    let constraints = ty_constraints(db, ty);
    let assumptions = solve_cx.assumptions();

    // Normalize constraints to resolve associated types
    let normalized_constraints = {
        let scope = solve_cx.origin_scope(db);
        let normalized_list: Vec<_> = constraints
            .list(db)
            .iter()
            .map(|&goal| goal.normalize(db, scope, assumptions))
            .collect();
        PredicateListId::new(db, normalized_list)
    };

    for &goal in normalized_constraints.list(db) {
        let mut table = UnificationTable::new(db);
        let canonical_goal = Canonicalized::new(db, goal);

        if let GoalSatisfiability::UnSat(subgoal) =
            is_goal_satisfiable(db, solve_cx, canonical_goal.value)
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
    solve_cx: TraitSolveCx<'db>,
    trait_inst: TraitInstId<'db>,
) -> WellFormedness<'db> {
    let constraints =
        collect_constraints(db, trait_inst.def(db).into()).instantiate(db, trait_inst.args(db));
    let assumptions = solve_cx.assumptions();

    // Normalize constraints after instantiation to resolve associated types
    let normalized_constraints = {
        let scope = solve_cx.normalization_scope_for_trait_inst(db, trait_inst);
        let normalized_list: Vec<_> = constraints
            .list(db)
            .iter()
            .map(|&goal| goal.normalize(db, scope, assumptions))
            .collect();
        PredicateListId::new(db, normalized_list)
    };

    for &goal in normalized_constraints.list(db) {
        let mut table = UnificationTable::new(db);
        let canonical_goal = Canonicalized::new(db, goal);
        if let GoalSatisfiability::UnSat(subgoal) =
            is_goal_satisfiable(db, solve_cx, canonical_goal.value)
        {
            let subgoal =
                subgoal.map(|subgoal| canonical_goal.extract_solution(&mut table, subgoal));
            return WellFormedness::IllFormed { goal, subgoal };
        }
    }

    WellFormedness::WellFormed
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct TraitGoalSolution<'db> {
    pub(crate) inst: TraitInstId<'db>,
    pub(crate) implementor: ImplementorId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub enum GoalSatisfiability<'db> {
    /// Goal is satisfied with the unique solution.
    Satisfied(Solution<TraitGoalSolution<'db>>),
    /// Goal might be satisfied, but needs more type information to determine
    /// satisfiability and uniqueness.
    NeedsConfirmation(IndexSet<Solution<TraitGoalSolution<'db>>>),

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
            for super_trait in pred.def(db).super_traits(db) {
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
            let hir_trait = pred.def(db);
            for trait_type in hir_trait.assoc_types(db) {
                // Get the associated type name
                let Some(assoc_ty_name) = trait_type.name(db) else {
                    continue;
                };

                // Create the associated type: Self::AssocType
                let assoc_ty = TyId::assoc_ty(db, pred, assoc_ty_name);

                let _assumptions =
                    PredicateListId::new(db, all_predicates.iter().copied().collect::<Vec<_>>());

                for mut trait_inst in assoc_ty.assoc_type_bounds(db, trait_type) {
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
