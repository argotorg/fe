//! Fe's adapter for the reusable [`tablesolve`] proof-forest engine.
//!
//! The external crate owns table creation, scheduling, consumer replay, and
//! answer deduplication. This module keeps the language-specific parts:
//! canonicalization, candidate lookup, unification, evidence construction, and
//! the small amount of diagnostic state needed for an unsatisfied subgoal.

use std::convert::Infallible;

use common::indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHashMap;
use tablesolve::{
    AnswerlessMode, CallbackOutcome, Canonical as TabledCanonical, CanonicalizeOutcome, Completion,
    Config, ConsumerId, ContextTransition, Event, Limits, Observer, ReportOptions,
    ResolutionContext, ResumeProvenance, Scheduling, Transition, solve_with_observer_and_options,
    solve_with_options,
};

use super::{
    CanonicalGoalQuery, GoalSatisfiability, TRAIT_SOLVER_ROOT_ANSWER_LIMIT, TraitGoalSolution,
    TraitSolveCompletion, TraitSolveCx, TraitSolverQuery, normalize_trait_inst_preserving_validity,
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        binder::Binder,
        canonical::{Canonical, Solution},
        fold::TyFoldable,
        trait_def::{ImplementorId, TraitInstId, impls_for_trait_in_ingots},
        ty_def::{TyData, TyId},
        unify::{PersistentUnificationTable, UnificationError, UnificationResult},
        visitor::{TyVisitable, TyVisitor},
    },
};

/// Temporary bounded-term-size guard for non-regular recursive goals.
///
/// The initial query may itself contain a legitimately deep finite type, so
/// bound growth relative to that query instead of imposing an absolute depth.
const MAXIMUM_TYPE_GROWTH: usize = 256;

type Query<'db> = Canonical<TraitSolverQuery<'db>>;
type GoalSolution<'db> = Solution<TraitGoalSolution<'db>>;
type UnsatSubgoal<'db> = Solution<TraitInstId<'db>>;

fn trait_inst_head<'db>(db: &'db dyn HirAnalysisDb, inst: TraitInstId<'db>) -> TraitInstId<'db> {
    TraitInstId::new(db, inst.def(db), inst.args(db).to_vec(), IndexMap::new())
}

fn normalize_assoc_binding<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut PersistentUnificationTable<'db>,
    ty: TyId<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: super::PredicateListId<'db>,
) -> TyId<'db> {
    let ty = ty.fold_with(db, table);
    crate::analysis::ty::normalize::normalize_ty(db, ty, scope, assumptions)
}

fn unify_trait_inst_with_normalized_assoc_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut PersistentUnificationTable<'db>,
    candidate: TraitInstId<'db>,
    goal: TraitInstId<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: super::PredicateListId<'db>,
) -> UnificationResult {
    table.unify(trait_inst_head(db, candidate), trait_inst_head(db, goal))?;

    for (name, &candidate_assoc_ty) in candidate.assoc_type_bindings(db) {
        if let Some(&goal_assoc_ty) = goal.assoc_type_bindings(db).get(name) {
            let candidate_assoc_ty =
                normalize_assoc_binding(db, table, candidate_assoc_ty, scope, assumptions);
            let goal_assoc_ty =
                normalize_assoc_binding(db, table, goal_assoc_ty, scope, assumptions);
            table.unify(candidate_assoc_ty, goal_assoc_ty)?;
        }
    }

    if goal
        .assoc_type_bindings(db)
        .keys()
        .any(|name| !candidate.assoc_type_bindings(db).contains_key(name))
    {
        return Err(UnificationError::TypeMismatch);
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StopReason {
    MaximumTypeDepth,
    TargetFound,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, salsa::Update)]
pub(crate) enum TargetSolutionStatus {
    Found,
    NotFound,
    Incomplete,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub(crate) enum TargetSolutionMatch {
    Equal,
    NotEqual,
}

#[derive(Clone, Copy)]
enum Clause<'db> {
    Implementor(Binder<ImplementorId<'db>>),
    Assumption(usize),
}

#[derive(Clone)]
struct Branch<'db> {
    table: PersistentUnificationTable<'db>,
    root_goal: TraitInstId<'db>,
    remaining_goals: Vec<TraitInstId<'db>>,
    selected_impl: ImplementorId<'db>,
}

#[derive(Clone)]
struct PreparedQuery<'db> {
    table: PersistentUnificationTable<'db>,
    query: TraitSolverQuery<'db>,
    normalized_goal: TraitInstId<'db>,
}

#[derive(Clone, Copy)]
struct TargetAnswer<'db> {
    root: Query<'db>,
    inst: Canonical<TraitInstId<'db>>,
    relation: TargetSolutionMatch,
}

#[derive(Debug, Clone, Copy)]
struct TypeDepthBudget {
    initial_max_depth: usize,
}

impl TypeDepthBudget {
    fn new<'db>(
        db: &'db dyn HirAnalysisDb,
        root: TraitSolverQuery<'db>,
        target: Option<TargetAnswer<'db>>,
    ) -> Self {
        let mut initial_max_depth = maximum_ty_depth(db, root);
        if let Some(target) = target {
            initial_max_depth = initial_max_depth.max(maximum_ty_depth(db, target.inst.value()));
        }
        Self { initial_max_depth }
    }

    fn exceeded<'db, V>(self, db: &'db dyn HirAnalysisDb, value: V) -> bool
    where
        V: TyVisitable<'db>,
    {
        type_depth_growth_exceeded(self.initial_max_depth, maximum_ty_depth(db, value))
    }
}

fn type_depth_growth_exceeded(initial_depth: usize, current_depth: usize) -> bool {
    current_depth.saturating_sub(initial_depth) > MAXIMUM_TYPE_GROWTH
}

struct TraitResolutionContext<'db> {
    db: &'db dyn HirAnalysisDb,
    origin_ingot: crate::Ingot<'db>,
    prepared_queries: FxHashMap<Query<'db>, PreparedQuery<'db>>,
    target: Option<TargetAnswer<'db>>,
    type_depth_budget: TypeDepthBudget,
}

impl<'db> TraitResolutionContext<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        origin_ingot: crate::Ingot<'db>,
        root: TraitSolverQuery<'db>,
        target: Option<TargetAnswer<'db>>,
    ) -> Self {
        Self {
            db,
            origin_ingot,
            prepared_queries: FxHashMap::default(),
            target,
            type_depth_budget: TypeDepthBudget::new(db, root, target),
        }
    }

    fn prepare_query(&mut self, key: Query<'db>) -> PreparedQuery<'db> {
        if let Some(prepared) = self.prepared_queries.get(&key) {
            return prepared.clone();
        }

        let mut table = PersistentUnificationTable::new(self.db);
        let query = key.extract_identity(&mut table);
        let scope = TraitSolveCx::normalization_scope_for_trait_inst_with_origin(
            self.db,
            self.origin_ingot,
            query.goal,
        );
        let normalized_goal =
            normalize_trait_inst_preserving_validity(self.db, query.goal, scope, query.assumptions);
        let prepared = PreparedQuery {
            table,
            query,
            normalized_goal,
        };
        self.prepared_queries.insert(key, prepared.clone());
        prepared
    }

    fn goal_can_use_assumptions(&self, goal: TraitInstId<'db>) -> bool {
        goal.args(self.db).iter().copied().any(|ty| {
            ty.has_param(self.db)
                || ty.has_var(self.db)
                || matches!(
                    ty.data(self.db),
                    TyData::AssocTy(_) | TyData::QualifiedTy(_)
                )
        })
    }

    fn answer(
        &self,
        parent: &Query<'db>,
        branch: &mut Branch<'db>,
        goal: TraitInstId<'db>,
    ) -> GoalSolution<'db> {
        parent.canonicalize_solution(
            self.db,
            &mut branch.table,
            TraitGoalSolution {
                inst: goal,
                implementor: branch.selected_impl,
            },
        )
    }

    fn finish_branch(
        &self,
        parent: &Query<'db>,
        mut branch: Branch<'db>,
    ) -> ContextTransition<Self> {
        let root_goal = branch.root_goal;
        let answer = self.answer(parent, &mut branch, root_goal);
        if self.target.is_some_and(|target| {
            if target.root != *parent {
                return false;
            }
            let equal = Canonical::new(self.db, answer.value.inst) == target.inst;
            match target.relation {
                TargetSolutionMatch::Equal => equal,
                TargetSolutionMatch::NotEqual => !equal,
            }
        }) {
            Transition::Stop(StopReason::TargetFound)
        } else {
            Transition::Answer(answer)
        }
    }

    fn continue_branch(
        &self,
        parent: &Query<'db>,
        mut branch: Branch<'db>,
        assumptions: super::PredicateListId<'db>,
    ) -> ContextTransition<Self> {
        let Some(next_goal) = branch.remaining_goals.pop() else {
            return self.finish_branch(parent, branch);
        };

        // `tablesolve` keys a suspension before retaining its branch state. Fold
        // through every substitution accumulated so far so table sharing never
        // sees a stale, pre-unification subgoal.
        let next_goal = next_goal.fold_with(self.db, &mut branch.table);
        let assumptions = assumptions.fold_with(self.db, &mut branch.table);
        Transition::Suspend {
            goal: TraitSolverQuery {
                goal: next_goal,
                assumptions,
            },
            state: branch,
        }
    }
}

impl<'db> ResolutionContext for TraitResolutionContext<'db> {
    type Goal = TraitSolverQuery<'db>;
    type Key = Query<'db>;
    type Clause = Clause<'db>;
    type Answer = GoalSolution<'db>;
    type AnswerKey = GoalSolution<'db>;
    type Output = GoalSolution<'db>;
    type State = Branch<'db>;
    type Rebase = CanonicalGoalQuery<'db>;
    type Error = Infallible;
    type StopReason = StopReason;

    fn canonicalize(
        &mut self,
        goal: Self::Goal,
    ) -> Result<CanonicalizeOutcome<Self::Key, Self::Rebase, Self::StopReason>, Self::Error> {
        // Assumptions participate in the table key, so they must be bounded
        // together with the goal; otherwise substitutions can create an
        // unbounded sequence of keys while the goal itself stays shallow.
        let exceeds_type_depth = self.type_depth_budget.exceeded(self.db, goal);
        let query = CanonicalGoalQuery::from_query(self.db, goal);
        let canonical = TabledCanonical::new(query.canonical(), query);
        if exceeds_type_depth {
            Ok(CanonicalizeOutcome::Stop {
                canonical,
                reason: StopReason::MaximumTypeDepth,
            })
        } else {
            Ok(CanonicalizeOutcome::Continue(canonical))
        }
    }

    fn clauses(
        &mut self,
        key: &Self::Key,
    ) -> Result<CallbackOutcome<Vec<Self::Clause>, Self::StopReason>, Self::Error> {
        let prepared = self.prepare_query(*key);
        let (primary, secondary) = TraitSolveCx::search_ingots_for_trait_inst_with_origin(
            self.db,
            self.origin_ingot,
            prepared.query.goal,
        );
        let implementors = impls_for_trait_in_ingots(
            self.db,
            primary,
            secondary,
            Canonical::new(self.db, prepared.query.goal),
        );

        let mut clauses =
            Vec::with_capacity(implementors.len() + prepared.query.assumptions.list(self.db).len());
        clauses.extend(implementors.iter().copied().map(Clause::Implementor));
        if self.goal_can_use_assumptions(prepared.normalized_goal) {
            clauses.extend(
                (0..prepared.query.assumptions.list(self.db).len()).map(Clause::Assumption),
            );
        }
        Ok(CallbackOutcome::Continue(clauses))
    }

    fn apply_clause(
        &mut self,
        key: &Self::Key,
        clause: Self::Clause,
    ) -> Result<ContextTransition<Self>, Self::Error> {
        let PreparedQuery {
            mut table,
            query,
            normalized_goal,
        } = self.prepare_query(*key);

        let selected_impl = match clause {
            Clause::Implementor(candidate) => {
                let selected_impl = candidate.instantiate_identity();
                let candidate = table.instantiate_with_fresh_vars(candidate);
                let scope = TraitSolveCx::normalization_scope_for_trait_inst_with_origin(
                    self.db,
                    self.origin_ingot,
                    query.goal,
                );
                let normalized_candidate = normalize_trait_inst_preserving_validity(
                    self.db,
                    candidate.trait_inst(self.db),
                    scope,
                    query.assumptions,
                );
                if unify_trait_inst_with_normalized_assoc_bindings(
                    self.db,
                    &mut table,
                    normalized_candidate,
                    normalized_goal,
                    scope,
                    query.assumptions,
                )
                .is_err()
                {
                    return Ok(Transition::Reject);
                }

                let constraints = candidate.constraints(self.db);
                let remaining_goals = constraints
                    .list(self.db)
                    .iter()
                    .map(|constraint| constraint.fold_with(self.db, &mut table))
                    .collect();
                let branch = Branch {
                    table,
                    root_goal: query.goal,
                    remaining_goals,
                    selected_impl,
                };
                return Ok(self.continue_branch(key, branch, query.assumptions));
            }
            Clause::Assumption(index) => {
                let Some(&assumption) = query.assumptions.list(self.db).get(index) else {
                    return Ok(Transition::Reject);
                };
                let scope = TraitSolveCx::normalization_scope_for_trait_inst_with_origin(
                    self.db,
                    self.origin_ingot,
                    query.goal,
                );
                if unify_trait_inst_with_normalized_assoc_bindings(
                    self.db,
                    &mut table,
                    assumption,
                    normalized_goal,
                    scope,
                    query.assumptions,
                )
                .is_err()
                {
                    return Ok(Transition::Reject);
                }
                ImplementorId::assumption(self.db, query.goal.fold_with(self.db, &mut table))
            }
        };

        let branch = Branch {
            table,
            root_goal: query.goal,
            remaining_goals: Vec::new(),
            selected_impl,
        };
        Ok(self.finish_branch(key, branch))
    }

    fn resume(
        &mut self,
        parent: &Self::Key,
        mut branch: Self::State,
        answer: Self::Answer,
        rebase: Self::Rebase,
    ) -> Result<ContextTransition<Self>, Self::Error> {
        let pending_goal = rebase.goal();
        let solution = rebase.extract_solution(&mut branch.table, answer).inst;

        let normalized_pending = {
            let scope = TraitSolveCx::normalization_scope_for_trait_inst_with_origin(
                self.db,
                self.origin_ingot,
                pending_goal,
            );
            normalize_trait_inst_preserving_validity(
                self.db,
                pending_goal.fold_with(self.db, &mut branch.table),
                scope,
                rebase.assumptions(),
            )
        };
        let normalized_solution = {
            let scope = TraitSolveCx::normalization_scope_for_trait_inst_with_origin(
                self.db,
                self.origin_ingot,
                solution,
            );
            normalize_trait_inst_preserving_validity(
                self.db,
                solution.fold_with(self.db, &mut branch.table),
                scope,
                rebase.assumptions(),
            )
        };
        if branch
            .table
            .unify(normalized_pending, normalized_solution)
            .is_err()
        {
            return Ok(Transition::Reject);
        }
        let resumed_root = branch.root_goal.fold_with(self.db, &mut branch.table);
        let resumed_assumptions = rebase.assumptions().fold_with(self.db, &mut branch.table);
        if self.type_depth_budget.exceeded(
            self.db,
            TraitSolverQuery {
                goal: resumed_root,
                assumptions: resumed_assumptions,
            },
        ) {
            return Ok(Transition::Stop(StopReason::MaximumTypeDepth));
        }

        Ok(self.continue_branch(parent, branch, rebase.assumptions()))
    }

    fn rebase_answer(
        &mut self,
        answer: &Self::Answer,
        _rebase: &Self::Rebase,
    ) -> Result<Self::Output, Self::Error> {
        // Solver entry points materialize an already-canonical query as the
        // root goal. Root answers therefore stay in that canonical coordinate
        // system until the outer `CanonicalGoalQuery` extracts them.
        Ok(*answer)
    }

    fn answer_key(&self, _key: &Self::Key, answer: &Self::Answer) -> Self::AnswerKey {
        *answer
    }
}

struct SuspendedGoal<'db> {
    query: CanonicalGoalQuery<'db>,
    table: PersistentUnificationTable<'db>,
    children: Vec<ConsumerId>,
}

#[derive(Default)]
struct UnresolvedGoalObserver<'db> {
    root_table: Option<tablesolve::TableId>,
    root_children: Vec<ConsumerId>,
    suspended: Vec<Option<SuspendedGoal<'db>>>,
}

impl<'db> UnresolvedGoalObserver<'db> {
    fn record_suspension(
        &mut self,
        consumer: ConsumerId,
        predecessor: Option<ResumeProvenance>,
        parent_table: tablesolve::TableId,
        state: &Branch<'db>,
        query: &CanonicalGoalQuery<'db>,
    ) {
        let index = consumer.index();
        if self.suspended.len() <= index {
            self.suspended.resize_with(index + 1, || None);
        }
        self.suspended[index] = Some(SuspendedGoal {
            query: query.clone(),
            table: state.table.clone(),
            children: Vec::new(),
        });

        if let Some(predecessor) = predecessor {
            if let Some(parent) = self
                .suspended
                .get_mut(predecessor.consumer.index())
                .and_then(Option::as_mut)
            {
                parent.children.push(consumer);
            }
        } else if self.root_table == Some(parent_table) {
            self.root_children.push(consumer);
        }
    }

    fn unresolved_subgoal(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        root: Query<'db>,
    ) -> Option<UnsatSubgoal<'db>> {
        let [consumer] = self.root_children.as_slice() else {
            return None;
        };
        let mut consumer = *consumer;

        loop {
            let suspended = self
                .suspended
                .get_mut(consumer.index())
                .and_then(Option::as_mut)?;
            let [child] = suspended.children.as_slice() else {
                return Some(root.canonicalize_solution(
                    db,
                    &mut suspended.table,
                    suspended.query.goal(),
                ));
            };
            consumer = *child;
        }
    }
}

impl<'db> Observer<TraitResolutionContext<'db>> for UnresolvedGoalObserver<'db> {
    fn observe(&mut self, event: Event<'_, TraitResolutionContext<'db>>) {
        match event {
            Event::TableCreated { table_id, .. } if self.root_table.is_none() => {
                self.root_table = Some(table_id);
            }
            Event::Suspended {
                consumer_id,
                predecessor,
                parent_table_id,
                state,
                rebase,
                ..
            } => self.record_suspension(consumer_id, predecessor, parent_table_id, state, rebase),
            _ => {}
        }
    }
}

fn map_completion(completion: Completion<StopReason>) -> TraitSolveCompletion {
    match completion {
        Completion::Saturated => TraitSolveCompletion::Saturated,
        Completion::RootAnswerLimit { limit } => TraitSolveCompletion::RootAnswerLimit { limit },
        Completion::StepLimit { limit } => TraitSolveCompletion::StepLimit { limit },
        Completion::TableLimit { limit } => TraitSolveCompletion::TableLimit { limit },
        Completion::PendingWorkLimit { limit } => TraitSolveCompletion::PendingWorkLimit { limit },
        Completion::Adapter(StopReason::MaximumTypeDepth) => TraitSolveCompletion::MaximumTypeDepth,
        Completion::Adapter(StopReason::TargetFound) => {
            unreachable!("ordinary trait solving never installs a target")
        }
    }
}

pub(super) fn solve<'db>(
    db: &'db dyn HirAnalysisDb,
    origin_ingot: crate::Ingot<'db>,
    query: Query<'db>,
) -> GoalSatisfiability<'db> {
    let mut root_table = PersistentUnificationTable::new(db);
    let root_goal = query.extract_identity(&mut root_table);
    let mut context = TraitResolutionContext::new(db, origin_ingot, root_goal, None);
    let mut observer = UnresolvedGoalObserver::default();
    let config = Config {
        limits: Limits {
            max_root_answers: Some(TRAIT_SOLVER_ROOT_ANSWER_LIMIT),
            ..Limits::default()
        },
        ..Config::default()
    };
    let options = ReportOptions::default().with_answerless(AnswerlessMode::Omit);
    let report = match solve_with_observer_and_options(
        &mut context,
        root_goal,
        config,
        options,
        &mut observer,
    ) {
        Ok(report) => report,
        Err(never) => match never {},
    };
    let root = report.root;
    let solutions: IndexSet<_> = report.answers.into_iter().collect();
    let completion = map_completion(report.completion);

    match (completion, solutions.len()) {
        (TraitSolveCompletion::Saturated, 1) => {
            GoalSatisfiability::Satisfied(solutions.into_iter().next().unwrap())
        }
        (TraitSolveCompletion::Saturated, 0) => {
            GoalSatisfiability::UnSat(observer.unresolved_subgoal(db, root))
        }
        _ => GoalSatisfiability::NeedsConfirmation {
            solutions,
            completion,
        },
    }
}

pub(super) fn has_solution<'db>(
    db: &'db dyn HirAnalysisDb,
    origin_ingot: crate::Ingot<'db>,
    query: Query<'db>,
    target: Canonical<TraitInstId<'db>>,
    relation: TargetSolutionMatch,
) -> TargetSolutionStatus {
    let mut root_table = PersistentUnificationTable::new(db);
    let root_goal = query.extract_identity(&mut root_table);
    let target = TargetAnswer {
        root: query,
        inst: target,
        relation,
    };
    let mut context = TraitResolutionContext::new(db, origin_ingot, root_goal, Some(target));
    let options = ReportOptions::default().with_answerless(AnswerlessMode::Omit);
    // A target can occur after a non-regular recursive clause. Fair scheduling
    // keeps that branch from starving later root clauses before the type-depth
    // guard makes the search incomplete.
    let config = Config {
        scheduling: Scheduling::Fair,
        ..Config::default()
    };
    let report = match solve_with_options(&mut context, root_goal, config, options) {
        Ok(report) => report,
        Err(never) => match never {},
    };
    match report.completion {
        Completion::Adapter(StopReason::TargetFound) => TargetSolutionStatus::Found,
        Completion::Saturated => TargetSolutionStatus::NotFound,
        Completion::RootAnswerLimit { .. }
        | Completion::StepLimit { .. }
        | Completion::TableLimit { .. }
        | Completion::PendingWorkLimit { .. }
        | Completion::Adapter(StopReason::MaximumTypeDepth) => TargetSolutionStatus::Incomplete,
    }
}

#[salsa::tracked]
pub(crate) fn ty_depth_impl<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> usize {
    match ty.data(db) {
        TyData::ConstTy(cty) => ty_depth_impl(db, cty.ty(db)),
        TyData::Invalid(_)
        | TyData::Never
        | TyData::TyBase(_)
        | TyData::TyParam(_)
        | TyData::AssocTy { .. }
        | TyData::TyVar(_) => 1,
        TyData::QualifiedTy(trait_inst) => ty_depth_impl(db, trait_inst.self_ty(db)) + 1,
        TyData::TyApp(lhs, rhs) => {
            let lhs_depth = ty_depth_impl(db, *lhs);
            let rhs_depth = ty_depth_impl(db, *rhs);
            std::cmp::max(lhs_depth, rhs_depth) + 1
        }
    }
}

fn maximum_ty_depth<'db, V>(db: &'db dyn HirAnalysisDb, value: V) -> usize
where
    V: TyVisitable<'db>,
{
    struct DepthVisitor<'db> {
        db: &'db dyn HirAnalysisDb,
        max_depth: usize,
    }

    impl<'db> TyVisitor<'db> for DepthVisitor<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId) {
            self.max_depth = self.max_depth.max(ty_depth_impl(self.db, ty));
        }
    }

    let mut visitor = DepthVisitor { db, max_depth: 0 };
    value.visit_with(&mut visitor);
    visitor.max_depth
}

#[cfg(test)]
mod tests {
    use super::{
        Completion, MAXIMUM_TYPE_GROWTH, StopReason, TraitSolveCompletion, map_completion,
        type_depth_growth_exceeded,
    };

    #[test]
    fn completion_mapping_preserves_engine_stop_reasons() {
        assert_eq!(
            map_completion(Completion::Saturated),
            TraitSolveCompletion::Saturated
        );
        assert_eq!(
            map_completion(Completion::RootAnswerLimit { limit: 2 }),
            TraitSolveCompletion::RootAnswerLimit { limit: 2 }
        );
        assert_eq!(
            map_completion(Completion::StepLimit { limit: 3 }),
            TraitSolveCompletion::StepLimit { limit: 3 }
        );
        assert_eq!(
            map_completion(Completion::TableLimit { limit: 4 }),
            TraitSolveCompletion::TableLimit { limit: 4 }
        );
        assert_eq!(
            map_completion(Completion::PendingWorkLimit { limit: 5 }),
            TraitSolveCompletion::PendingWorkLimit { limit: 5 }
        );
        assert_eq!(
            map_completion(Completion::Adapter(StopReason::MaximumTypeDepth)),
            TraitSolveCompletion::MaximumTypeDepth
        );
    }

    #[test]
    fn type_depth_growth_budget_includes_its_boundary() {
        let initial_depth = 300;
        assert!(!type_depth_growth_exceeded(
            initial_depth,
            initial_depth + MAXIMUM_TYPE_GROWTH,
        ));
        assert!(type_depth_growth_exceeded(
            initial_depth,
            initial_depth + MAXIMUM_TYPE_GROWTH + 1,
        ));
        assert!(!type_depth_growth_exceeded(initial_depth, 1));
    }
}
