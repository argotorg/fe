use super::{
    canonical::{Canonical, Canonicalized, Solution},
    const_expr::ConstExpr,
    const_ty::{ConstTyData, EvaluatedConstTy},
    fold::{AssocTySubst, TyFoldable},
    trait_def::{ImplementorId, TraitInstId},
    ty_def::{TyData, TyFlags, TyId},
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        trait_resolution::{constraint::ty_constraints, proof_forest::ProofForest},
        unify::UnificationTable,
    },
};
use crate::{
    Ingot,
    hir_def::{HirIngot, scope_graph::ScopeId},
};
use common::indexmap::IndexSet;
use constraint::collect_constraints;
use rustc_hash::FxHashSet;
use salsa::Update;

pub(crate) mod constraint;
mod proof_forest;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct TraitSolverQuery<'db> {
    pub goal: TraitInstId<'db>,
    pub assumptions: PredicateListId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CanonicalGoalQuery<'db> {
    raw: TraitSolverQuery<'db>,
    canonical: Canonical<TraitSolverQuery<'db>>,
    original: Canonicalized<'db, TraitSolverQuery<'db>>,
}

impl<'db> CanonicalGoalQuery<'db> {
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        Self::from_query(
            db,
            TraitSolverQuery {
                goal,
                assumptions: assumptions.extend_all_bounds(db),
            },
        )
    }

    pub fn from_query(db: &'db dyn HirAnalysisDb, raw: TraitSolverQuery<'db>) -> Self {
        let original = Canonicalized::new(db, raw);
        Self {
            raw,
            canonical: original.canonical(),
            original,
        }
    }

    pub fn goal(&self) -> TraitInstId<'db> {
        self.raw.goal
    }

    pub fn assumptions(&self) -> PredicateListId<'db> {
        self.raw.assumptions
    }

    pub fn canonical(&self) -> Canonical<TraitSolverQuery<'db>> {
        self.canonical
    }

    pub fn extract_solution<S, U>(
        &self,
        table: &mut crate::analysis::ty::unify::UnificationTableBase<'db, S>,
        solution: Solution<U>,
    ) -> U
    where
        S: crate::analysis::ty::unify::UnificationStore<'db>,
        U: TyFoldable<'db> + Update,
    {
        self.original.extract_solution(table, solution)
    }

    pub fn extract_subgoal<S>(
        &self,
        table: &mut crate::analysis::ty::unify::UnificationTableBase<'db, S>,
        solution: Solution<TraitInstId<'db>>,
    ) -> TraitInstId<'db>
    where
        S: crate::analysis::ty::unify::UnificationStore<'db>,
    {
        self.extract_solution(table, solution)
    }
}

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

    pub(crate) fn origin_ingot(self) -> Ingot<'db> {
        self.origin_ingot
    }

    pub(crate) fn select_impl(
        self,
        db: &'db dyn HirAnalysisDb,
        inst: TraitInstId<'db>,
    ) -> Selection<ImplementorId<'db>> {
        let scope = self.normalization_scope_for_trait_inst(db, inst);
        let inst = normalize_trait_inst_preserving_validity(db, inst, scope, self.assumptions);
        match is_goal_satisfiable(db, self, inst) {
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
        Self::search_ingots_for_trait_inst_with_origin(db, self.origin_ingot, inst)
    }

    pub(crate) fn search_ingots_for_trait_inst_with_origin(
        db: &'db dyn HirAnalysisDb,
        origin_ingot: Ingot<'db>,
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

        let primary = self_ingot.unwrap_or(origin_ingot);
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
        Self::normalization_scope_for_trait_inst_with_origin(db, self.origin_ingot, inst)
    }

    pub(crate) fn normalization_scope_for_trait_inst_with_origin(
        db: &'db dyn HirAnalysisDb,
        origin_ingot: Ingot<'db>,
        inst: TraitInstId<'db>,
    ) -> ScopeId<'db> {
        let norm_ingot = inst
            .self_ty(db)
            .ingot(db)
            .or_else(|| inst.args(db).iter().find_map(|ty| ty.ingot(db)))
            .unwrap_or(origin_ingot);
        norm_ingot.root_mod(db).scope()
    }

    pub(crate) fn origin_scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.origin_ingot.root_mod(db).scope()
    }
}

pub(crate) fn normalize_trait_inst_preserving_validity<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TraitInstId<'db> {
    let normalized = inst.normalize(db, scope, assumptions);
    let original_has_invalid = inst.args(db).iter().copied().any(|ty| ty.has_invalid(db))
        || inst
            .assoc_type_bindings(db)
            .values()
            .copied()
            .any(|ty| ty.has_invalid(db));
    let normalized_has_invalid = normalized
        .args(db)
        .iter()
        .copied()
        .any(|ty| ty.has_invalid(db))
        || normalized
            .assoc_type_bindings(db)
            .values()
            .copied()
            .any(|ty| ty.has_invalid(db));
    if !original_has_invalid && normalized_has_invalid {
        inst
    } else {
        normalized
    }
}

#[salsa::tracked(return_ref)]
fn is_query_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    origin_ingot: Ingot<'db>,
    query: Canonical<TraitSolverQuery<'db>>,
) -> GoalSatisfiability<'db> {
    if query.flags(db).contains(TyFlags::HAS_INVALID) {
        return GoalSatisfiability::ContainsInvalid;
    };

    ProofForest::new(db, origin_ingot, query).solve()
}

pub fn is_goal_query_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    query: &CanonicalGoalQuery<'db>,
) -> GoalSatisfiability<'db> {
    is_query_satisfiable(db, solve_cx.origin_ingot(), query.canonical()).clone()
}

pub fn is_goal_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> GoalSatisfiability<'db> {
    let query = CanonicalGoalQuery::new(db, goal, solve_cx.assumptions());
    is_goal_query_satisfiable(db, solve_cx, &query)
}

/// Checks if the given type is well-formed, i.e., the arguments of the given
/// type applications satisfies the constraints under the given assumptions.
#[salsa::tracked]
pub(crate) fn check_ty_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    ty: TyId<'db>,
) -> WellFormedness<'db> {
    // Check the arguments and the structural content of the application base
    // (projections and const expressions). The base's *constraints* are not
    // checked here: `ty_constraints` of a partial application instantiates
    // the constraint binder with missing arguments, producing spurious
    // unsatisfied goals; the fully-applied type's constraints are checked
    // below.
    let (base, args) = ty.decompose_ty_app(db);
    for &arg in args {
        let wf = check_ty_wf(db, solve_cx, arg);
        if !wf.is_wf() {
            return wf;
        }
    }
    match base.data(db) {
        TyData::AssocTy(assoc) => {
            let wf = check_projected_trait_use_wf(db, solve_cx, assoc.trait_);
            if !wf.is_wf() {
                return wf;
            }
        }
        TyData::QualifiedTy(inst) => {
            let wf = check_projected_trait_use_wf(db, solve_cx, *inst);
            if !wf.is_wf() {
                return wf;
            }
        }
        TyData::ConstTy(const_ty) => {
            let wf = check_const_ty_wf(db, solve_cx, *const_ty);
            if !wf.is_wf() {
                return wf;
            }
        }
        TyData::TyApp(..)
        | TyData::TyVar(_)
        | TyData::TyParam(_)
        | TyData::TyBase(_)
        | TyData::Never
        | TyData::Invalid(_) => {}
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
        let query = CanonicalGoalQuery::new(db, goal, assumptions);

        if let GoalSatisfiability::UnSat(subgoal) = is_goal_query_satisfiable(db, solve_cx, &query)
        {
            let subgoal = subgoal.map(|subgoal| query.extract_subgoal(&mut table, subgoal));
            return WellFormedness::IllFormed { goal, subgoal };
        }
    }

    WellFormedness::WellFormed
}

fn check_const_ty_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    const_ty: super::const_ty::ConstTyId<'db>,
) -> WellFormedness<'db> {
    let wf = check_ty_wf(db, solve_cx, const_ty.ty(db));
    if !wf.is_wf() {
        return wf;
    }

    match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::Tuple(elems), _)
        | ConstTyData::Evaluated(EvaluatedConstTy::Array(elems), _)
        | ConstTyData::Evaluated(EvaluatedConstTy::Record(elems), _) => {
            for &elem in elems {
                let wf = check_ty_wf(db, solve_cx, elem);
                if !wf.is_wf() {
                    return wf;
                }
            }
        }
        ConstTyData::Abstract(expr, _) => {
            let wf = check_const_expr_wf(db, solve_cx, *expr);
            if !wf.is_wf() {
                return wf;
            }
        }
        ConstTyData::TyVar(..)
        | ConstTyData::TyParam(..)
        | ConstTyData::Hole(..)
        | ConstTyData::Evaluated(..)
        | ConstTyData::UnEvaluated { .. } => {}
    }

    WellFormedness::WellFormed
}

fn check_const_expr_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    expr: super::const_expr::ConstExprId<'db>,
) -> WellFormedness<'db> {
    match expr.data(db) {
        ConstExpr::ExternConstFnCall {
            generic_args, args, ..
        }
        | ConstExpr::UserConstFnCall {
            generic_args, args, ..
        } => {
            for &ty in generic_args.iter().chain(args.iter()) {
                let wf = check_ty_wf(db, solve_cx, ty);
                if !wf.is_wf() {
                    return wf;
                }
            }
        }
        ConstExpr::ArithBinOp { lhs, rhs, .. } => {
            for ty in [*lhs, *rhs] {
                let wf = check_ty_wf(db, solve_cx, ty);
                if !wf.is_wf() {
                    return wf;
                }
            }
        }
        ConstExpr::UnOp { expr, .. } => {
            let wf = check_ty_wf(db, solve_cx, *expr);
            if !wf.is_wf() {
                return wf;
            }
        }
        ConstExpr::Cast { expr, to } => {
            for ty in [*expr, *to] {
                let wf = check_ty_wf(db, solve_cx, ty);
                if !wf.is_wf() {
                    return wf;
                }
            }
        }
        ConstExpr::TraitConst(assoc) => {
            let wf = check_projected_trait_use_wf(db, solve_cx, assoc.inst());
            if !wf.is_wf() {
                return wf;
            }
        }
        ConstExpr::LocalBinding(_) => {}
    }

    WellFormedness::WellFormed
}

fn check_projected_trait_use_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> WellFormedness<'db> {
    for &arg in inst.args(db) {
        let wf = check_ty_wf(db, solve_cx, arg);
        if !wf.is_wf() {
            return wf;
        }
    }
    for &ty in inst.assoc_type_bindings(db).values() {
        let wf = check_ty_wf(db, solve_cx, ty);
        if !wf.is_wf() {
            return wf;
        }
    }

    unsatisfied_goal(db, solve_cx, inst).unwrap_or(WellFormedness::WellFormed)
}

fn unsatisfied_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Option<WellFormedness<'db>> {
    let assumptions = solve_cx.assumptions();
    let mut table = UnificationTable::new(db);
    let query = CanonicalGoalQuery::new(db, goal, assumptions);
    if let GoalSatisfiability::UnSat(subgoal) = is_goal_query_satisfiable(db, solve_cx, &query) {
        let subgoal = subgoal.map(|subgoal| query.extract_subgoal(&mut table, subgoal));
        Some(WellFormedness::IllFormed { goal, subgoal })
    } else {
        None
    }
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
    for &arg in trait_inst.args(db) {
        let wf = check_ty_wf(db, solve_cx, arg);
        if !wf.is_wf() {
            return wf;
        }
    }
    for &ty in trait_inst.assoc_type_bindings(db).values() {
        let wf = check_ty_wf(db, solve_cx, ty);
        if !wf.is_wf() {
            return wf;
        }
    }

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
        let query = CanonicalGoalQuery::new(db, goal, assumptions);
        if let GoalSatisfiability::UnSat(subgoal) = is_goal_query_satisfiable(db, solve_cx, &query)
        {
            let subgoal = subgoal.map(|subgoal| query.extract_subgoal(&mut table, subgoal));
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
                if predicate_has_recursive_assoc_projection(db, inst) {
                    continue;
                }

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
                    if predicate_has_recursive_assoc_projection(db, trait_inst) {
                        continue;
                    }
                    if all_predicates.insert(trait_inst) {
                        worklist.push(trait_inst);
                    }
                }
            }
        }

        Self::new(db, all_predicates.into_iter().collect::<Vec<_>>())
    }
}

fn predicate_has_recursive_assoc_projection<'db>(
    db: &'db dyn HirAnalysisDb,
    pred: TraitInstId<'db>,
) -> bool {
    pred.args(db)
        .iter()
        .any(|&arg| ty_has_recursive_assoc_projection(db, arg))
}

fn ty_has_recursive_assoc_projection<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    fn impl_<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        visited_tys: &mut FxHashSet<TyId<'db>>,
        seen_assoc_keys: &mut FxHashSet<(crate::hir_def::Trait<'db>, crate::hir_def::IdentId<'db>)>,
    ) -> bool {
        if !visited_tys.insert(ty) {
            return false;
        }

        let has_cycle = match ty.data(db) {
            TyData::ConstTy(const_ty) => impl_(db, const_ty.ty(db), visited_tys, seen_assoc_keys),
            TyData::AssocTy(assoc_ty) => {
                let key = (assoc_ty.trait_.def(db), assoc_ty.name);
                if !seen_assoc_keys.insert(key) {
                    true
                } else {
                    let has_cycle = assoc_ty
                        .trait_
                        .args(db)
                        .iter()
                        .copied()
                        .any(|arg| impl_(db, arg, visited_tys, seen_assoc_keys));
                    seen_assoc_keys.remove(&key);
                    has_cycle
                }
            }
            TyData::QualifiedTy(trait_inst) => {
                let args_have_cycle = trait_inst
                    .args(db)
                    .iter()
                    .copied()
                    .any(|arg| impl_(db, arg, visited_tys, seen_assoc_keys));
                let assoc_bindings_have_cycle = trait_inst
                    .assoc_type_bindings(db)
                    .values()
                    .copied()
                    .any(|ty| impl_(db, ty, visited_tys, seen_assoc_keys));
                args_have_cycle || assoc_bindings_have_cycle
            }
            TyData::TyApp(lhs, rhs) => {
                impl_(db, *lhs, visited_tys, seen_assoc_keys)
                    || impl_(db, *rhs, visited_tys, seen_assoc_keys)
            }
            _ => false,
        };

        visited_tys.remove(&ty);
        has_cycle
    }

    impl_(db, ty, &mut FxHashSet::default(), &mut FxHashSet::default())
}

#[cfg(test)]
mod tests {
    use common::indexmap::IndexMap;

    use super::{
        CanonicalGoalQuery, GoalSatisfiability, TraitInstId, TraitSolveCx,
        is_goal_query_satisfiable,
    };
    use crate::{
        analysis::ty::{
            trait_resolution::constraint::collect_func_def_constraints, ty_def::TyId,
            ty_lower::collect_generic_params,
        },
        hir_def::{Func, Trait},
        test_db::HirAnalysisTestDb,
    };

    #[test]
    fn solver_query_includes_assumptions() {
        fn query_for<'db>(
            db: &'db HirAnalysisTestDb,
            func: Func<'db>,
            needs_a: Trait<'db>,
        ) -> (CanonicalGoalQuery<'db>, TraitSolveCx<'db>) {
            let ty_param = collect_generic_params(db, func.into()).explicit_params(db)[0];
            let assumptions =
                collect_func_def_constraints(db, func.into(), true).instantiate_identity();
            let goal =
                TraitInstId::new(db, needs_a, vec![TyId::unit(db), ty_param], IndexMap::new());
            let query = CanonicalGoalQuery::new(db, goal, assumptions);
            let solve_cx = TraitSolveCx::new(db, func.scope()).with_assumptions(assumptions);
            (query, solve_cx)
        }

        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "trait_solver_query_includes_assumptions.fe".into(),
            r#"
trait A {}
trait NeedsA<T> {}

impl<T: A> NeedsA<T> for () {}

fn with_a<T: A>() -> bool {
    true
}

fn without_a<T>() -> bool {
    true
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let needs_a = top_mod
            .all_traits(&db)
            .iter()
            .copied()
            .find(|trait_| {
                trait_
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "NeedsA")
            })
            .unwrap();
        let with_a = top_mod
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "with_a")
            })
            .unwrap();
        let without_a = top_mod
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "without_a")
            })
            .unwrap();

        let (with_query, with_cx) = query_for(&db, with_a, needs_a);
        let (without_query, without_cx) = query_for(&db, without_a, needs_a);

        assert_eq!(
            with_query.goal().pretty_print(&db, true),
            without_query.goal().pretty_print(&db, true)
        );
        assert_ne!(with_query.canonical(), without_query.canonical());
        assert!(matches!(
            is_goal_query_satisfiable(&db, with_cx, &with_query),
            GoalSatisfiability::Satisfied(_)
        ));
        assert!(matches!(
            is_goal_query_satisfiable(&db, without_cx, &without_query),
            GoalSatisfiability::UnSat(_)
        ));
    }
}
