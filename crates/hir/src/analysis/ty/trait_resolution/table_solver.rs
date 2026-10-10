//! Fe's adapter for the reusable [`tablesolve`] proof-forest engine.
//!
//! The external crate owns table creation, scheduling, consumer replay, and
//! answer deduplication. This module keeps the language-specific parts:
//! canonicalization, candidate lookup, unification, evidence construction, and
//! the small amount of diagnostic state needed for an unsatisfied subgoal.

use std::convert::Infallible;

use common::indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHashMap;

use crate::analysis::ty::normalize::NormalizationLimit;
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
        canonical::{Canonical, Solution},
        fold::TyFoldable,
        trait_def::{ImplementorId, TraitInstId, impls_for_trait_in_ingots},
        ty_def::{TyData, TyId},
        unify::PersistentUnificationTable,
        visitor::{TyVisitable, TyVisitor},
    },
};
use crate::hir_def::scope_graph::ScopeId;

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

/// Whether the impl `header` can match the normalized `goal`, checked before
/// the header is instantiated and normalized.
///
/// Instantiation rebuilds types structurally, and normalization of a valid
/// header never makes it invalid, so the header's base types and applications
/// reach unification unchanged, where different ones never unify. Parameters,
/// projections, consts, and error types are not compared.
fn impl_header_may_match<'db>(
    db: &'db dyn HirAnalysisDb,
    header: ImplementorId<'db>,
    goal: TraitInstId<'db>,
) -> bool {
    fn may_unify<'db>(db: &'db dyn HirAnalysisDb, header: TyId<'db>, goal: TyId<'db>) -> bool {
        match (header.data(db), goal.data(db)) {
            (TyData::TyApp(header_abs, header_arg), TyData::TyApp(goal_abs, goal_arg)) => {
                may_unify(db, *header_abs, *goal_abs) && may_unify(db, *header_arg, *goal_arg)
            }
            (TyData::TyBase(_) | TyData::TyApp(..), TyData::TyBase(_) | TyData::TyApp(..)) => {
                header == goal
            }
            _ => true,
        }
    }

    let inst = header.trait_(db);
    inst.args(db)
        .iter()
        .chain(inst.assoc_type_bindings(db).values())
        .chain(header.types(db).values())
        .any(|ty| ty.has_invalid(db))
        || inst
            .args(db)
            .iter()
            .zip(goal.args(db))
            .all(|(&header, &goal)| may_unify(db, header, goal))
}

fn normalize_assoc_binding<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut PersistentUnificationTable<'db>,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: super::PredicateListId<'db>,
) -> Result<TyId<'db>, NormalizationLimit> {
    let ty = ty.fold_with(db, table);
    crate::analysis::ty::normalize::normalize_ty_in_solver(db, ty, scope, assumptions)
}

/// Whether a candidate matches a goal. `Unknown` means the heads unify but an
/// associated-type binding could not be compared within the limits.
enum CandidateMatch {
    Matches,
    Mismatch,
    Unknown(NormalizationLimit),
}

fn unify_trait_inst_with_normalized_assoc_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut PersistentUnificationTable<'db>,
    candidate: TraitInstId<'db>,
    goal: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: super::PredicateListId<'db>,
) -> CandidateMatch {
    if table
        .unify(trait_inst_head(db, candidate), trait_inst_head(db, goal))
        .is_err()
    {
        return CandidateMatch::Mismatch;
    }

    if goal
        .assoc_type_bindings(db)
        .keys()
        .any(|name| !candidate.assoc_type_bindings(db).contains_key(name))
    {
        return CandidateMatch::Mismatch;
    }

    // A binding that cannot be compared leaves the match unknown, but a later
    // binding that differs still rules the candidate out.
    let mut unknown = None;
    for (name, &candidate_assoc_ty) in candidate.assoc_type_bindings(db) {
        if let Some(&goal_assoc_ty) = goal.assoc_type_bindings(db).get(name) {
            let pair = normalize_assoc_binding(db, table, candidate_assoc_ty, scope, assumptions)
                .and_then(|candidate| {
                    normalize_assoc_binding(db, table, goal_assoc_ty, scope, assumptions)
                        .map(|goal| (candidate, goal))
                });
            match pair {
                Ok((candidate_assoc_ty, goal_assoc_ty)) => {
                    if table.unify(candidate_assoc_ty, goal_assoc_ty).is_err() {
                        return CandidateMatch::Mismatch;
                    }
                }
                Err(limit) => unknown = Some(NormalizationLimit::join(unknown, limit)),
            }
        }
    }

    match unknown {
        Some(limit) => CandidateMatch::Unknown(limit),
        None => CandidateMatch::Matches,
    }
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
    Implementor(ImplementorId<'db>),
    Assumption(usize),
    /// The goal could not be normalized within the limits. Its one answer is
    /// the goal itself, unknown.
    Unknown(NormalizationLimit),
}

#[derive(Clone)]
struct Branch<'db> {
    table: PersistentUnificationTable<'db>,
    root_goal: TraitInstId<'db>,
    remaining_goals: Vec<TraitInstId<'db>>,
    selected_impl: ImplementorId<'db>,
    /// A step of this branch reached a limit: if the branch completes, its
    /// answer is unknown. A later goal that fails still ends the branch.
    unknown: Option<NormalizationLimit>,
}

/// An answer of a table, and the limit it depends on, if any.
///
/// A limit is an unknown, not a failure (law 5 of the limits design): an
/// unknown answer flows through the tables like any other, so a branch that
/// uses it is unknown too, and a branch that fails for another reason still
/// fails. Only the root decides what unknown answers mean for the goal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TabledAnswer<'db> {
    solution: GoalSolution<'db>,
    unknown: Option<NormalizationLimit>,
}

#[derive(Clone)]
struct PreparedQuery<'db> {
    table: PersistentUnificationTable<'db>,
    query: TraitSolverQuery<'db>,
    scope: ScopeId<'db>,
    normalized_goal: Result<TraitInstId<'db>, NormalizationLimit>,
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
    /// Whether the target was matched by an unknown answer, which cannot
    /// count as found.
    unknown_target: bool,
    /// The root goal's table, whose unknown answers are kept here.
    root: Option<Query<'db>>,
    /// The limit the root goal's unknown answers depend on, if any.
    root_unknown: Option<NormalizationLimit>,
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
            unknown_target: false,
            root: None,
            root_unknown: None,
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
            scope,
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
        &mut self,
        parent: &Query<'db>,
        mut branch: Branch<'db>,
    ) -> ContextTransition<Self> {
        let root_goal = branch.root_goal;
        let solution = self.answer(parent, &mut branch, root_goal);
        if self.target.is_some_and(|target| {
            if target.root != *parent {
                return false;
            }
            let equal = Canonical::new(self.db, solution.value.inst) == target.inst;
            match target.relation {
                TargetSolutionMatch::Equal => equal,
                TargetSolutionMatch::NotEqual => !equal,
            }
        }) {
            if branch.unknown.is_none() {
                return Transition::Stop(StopReason::TargetFound);
            }
            self.unknown_target = true;
        }
        // An unknown answer of the root goal is kept aside, not in the root
        // table: it cannot show the goal ambiguous, so it must not count
        // toward the root answer limit, and anything derived from it through
        // a cycle back to the root would be unknown too.
        if let Some(limit) = branch.unknown
            && self.root == Some(*parent)
        {
            self.root_unknown = Some(NormalizationLimit::join(self.root_unknown, limit));
            return Transition::Reject;
        }
        Transition::Answer(TabledAnswer {
            solution,
            unknown: branch.unknown,
        })
    }

    /// The answer of a goal that a limit leaves undecided: the goal itself,
    /// unknown.
    fn unknown_answer(
        &mut self,
        parent: &Query<'db>,
        table: PersistentUnificationTable<'db>,
        goal: TraitInstId<'db>,
        limit: NormalizationLimit,
    ) -> ContextTransition<Self> {
        let branch = Branch {
            table,
            root_goal: goal,
            remaining_goals: Vec::new(),
            selected_impl: ImplementorId::assumption(self.db, goal),
            unknown: Some(limit),
        };
        self.finish_branch(parent, branch)
    }

    fn continue_branch(
        &mut self,
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
                require_impl: false,
            },
            state: branch,
        }
    }
}

impl<'db> ResolutionContext for TraitResolutionContext<'db> {
    type Goal = TraitSolverQuery<'db>;
    type Key = Query<'db>;
    type Clause = Clause<'db>;
    type Answer = TabledAnswer<'db>;
    type AnswerKey = TabledAnswer<'db>;
    type Output = TabledAnswer<'db>;
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
        let normalized_goal = match prepared.normalized_goal {
            Ok(goal) => goal,
            Err(limit) => return Ok(CallbackOutcome::Continue(vec![Clause::Unknown(limit)])),
        };
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
        if !prepared.query.require_impl && self.goal_can_use_assumptions(normalized_goal) {
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
            scope,
            normalized_goal,
        } = self.prepare_query(*key);
        let normalized_goal = match normalized_goal {
            Ok(goal) => goal,
            Err(limit) => return Ok(self.unknown_answer(key, table, query.goal, limit)),
        };

        let selected_impl = match clause {
            Clause::Unknown(limit) => {
                return Ok(self.unknown_answer(key, table, query.goal, limit));
            }
            Clause::Implementor(selected_impl) => {
                if !impl_header_may_match(self.db, selected_impl, normalized_goal) {
                    return Ok(Transition::Reject);
                }
                let candidate = table.instantiate_with_fresh_vars(selected_impl);
                // A header that cannot be normalized cannot be compared with
                // the goal: whether this impl applies is unknown.
                let normalized_candidate = match normalize_trait_inst_preserving_validity(
                    self.db,
                    candidate.trait_inst(self.db),
                    scope,
                    query.assumptions,
                ) {
                    Ok(candidate) => candidate,
                    Err(limit) => return Ok(self.unknown_answer(key, table, query.goal, limit)),
                };
                let unknown = match unify_trait_inst_with_normalized_assoc_bindings(
                    self.db,
                    &mut table,
                    normalized_candidate,
                    normalized_goal,
                    scope,
                    query.assumptions,
                ) {
                    CandidateMatch::Matches => None,
                    CandidateMatch::Mismatch => return Ok(Transition::Reject),
                    CandidateMatch::Unknown(limit) => Some(limit),
                };

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
                    unknown,
                };
                return Ok(self.continue_branch(key, branch, query.assumptions));
            }
            Clause::Assumption(index) => {
                let Some(&assumption) = query.assumptions.list(self.db).get(index) else {
                    return Ok(Transition::Reject);
                };
                let unknown = match unify_trait_inst_with_normalized_assoc_bindings(
                    self.db,
                    &mut table,
                    assumption,
                    normalized_goal,
                    scope,
                    query.assumptions,
                ) {
                    CandidateMatch::Matches => None,
                    CandidateMatch::Mismatch => return Ok(Transition::Reject),
                    CandidateMatch::Unknown(limit) => Some(limit),
                };
                (
                    ImplementorId::assumption(self.db, query.goal.fold_with(self.db, &mut table)),
                    unknown,
                )
            }
        };
        let (selected_impl, unknown) = selected_impl;

        let branch = Branch {
            table,
            root_goal: query.goal,
            remaining_goals: Vec::new(),
            selected_impl,
            unknown,
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
        let solution = rebase
            .extract_solution(&mut branch.table, answer.solution)
            .inst;
        if let Some(limit) = answer.unknown {
            branch.unknown = Some(NormalizationLimit::join(branch.unknown, limit));
        }

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
        // An answer that cannot be compared with the goal it answers leaves
        // the branch unknown; its remaining goals are still checked.
        match (normalized_pending, normalized_solution) {
            (Ok(pending), Ok(solution)) => {
                if branch.table.unify(pending, solution).is_err() {
                    return Ok(Transition::Reject);
                }
            }
            (Err(limit), _) | (_, Err(limit)) => {
                branch.unknown = Some(NormalizationLimit::join(branch.unknown, limit));
            }
        }
        let resumed_root = branch.root_goal.fold_with(self.db, &mut branch.table);
        let resumed_assumptions = rebase.assumptions().fold_with(self.db, &mut branch.table);
        if self.type_depth_budget.exceeded(
            self.db,
            TraitSolverQuery {
                goal: resumed_root,
                assumptions: resumed_assumptions,
                require_impl: parent.value().require_impl,
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
) -> Result<GoalSatisfiability<'db>, NormalizationLimit> {
    let mut root_table = PersistentUnificationTable::new(db);
    let root_goal = query.extract_identity(&mut root_table);
    let mut context = TraitResolutionContext::new(db, origin_ingot, root_goal, None);
    context.root = Some(query);
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
    let solutions: IndexSet<_> = report
        .answers
        .into_iter()
        .map(|answer| answer.solution)
        .collect();
    let unknown = context.root_unknown;
    let completion = map_completion(report.completion);

    // Unknown answers matter only if they could change the result. A goal
    // without inference variables holds once one complete proof is found;
    // coherence rules out a second impl proving it. Otherwise two distinct
    // complete answers leave the goal ambiguous whatever the unknown ones
    // are, and in every other case the result depends on them.
    if let Some(limit) = unknown {
        let has_vars = root_goal.goal.args(db).iter().any(|ty| ty.has_var(db))
            || root_goal
                .goal
                .assoc_type_bindings(db)
                .values()
                .any(|ty| ty.has_var(db));
        if has_vars || solutions.is_empty() {
            let distinct: IndexSet<_> = solutions
                .iter()
                .map(|solution| solution.value.inst)
                .collect();
            if distinct.len() < 2 {
                return Err(limit);
            }
            return Ok(GoalSatisfiability::NeedsConfirmation {
                solutions,
                completion: TraitSolveCompletion::NormalizationLimit(limit),
            });
        }
    }

    Ok(match (completion, solutions.len()) {
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
    })
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
        // An unknown answer that matches the target may or may not be one.
        Completion::Saturated if context.unknown_target => TargetSolutionStatus::Incomplete,
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
