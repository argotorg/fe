//! Fe's adapter for the reusable [`tablesolve`] proof-forest engine.
//!
//! The external crate owns table creation, scheduling, consumer replay, and
//! answer deduplication. This module keeps the language-specific parts:
//! canonicalization, candidate lookup, unification, evidence construction, and
//! the small amount of diagnostic state needed for an unsatisfied subgoal.

use std::convert::Infallible;

use common::indexmap::IndexSet;
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
        canonical::{Canonical, Solution},
        clearable::clearable_implementor,
        closure::callable_implementor,
        fold::TyFoldable,
        normalize::{normalize_from_assumptions, normalize_ty},
        sol_codec::sol_codec_implementor,
        trait_def::{ImplementorId, TraitInstId, impls_for_trait_in_ingots},
        trait_lower::complete_candidate_impl_assoc_ty,
        ty_def::{BorrowKind, TyData, TyId},
        unify::{PersistentUnificationTable, Unifiable},
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

/// The predicate's trait and arguments, without its associated equalities
/// and mode, which unification compares on their own.
fn trait_inst_head<'db>(db: &'db dyn HirAnalysisDb, inst: TraitInstId<'db>) -> TraitInstId<'db> {
    TraitInstId::new_simple(db, inst.def(db), inst.args(db).to_vec())
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
    resolve_impls: bool,
) -> TyId<'db> {
    let ty = ty.fold_with(db, table);
    if resolve_impls {
        normalize_ty(db, ty, scope, assumptions)
    } else {
        normalize_from_assumptions(db, ty, scope, assumptions)
    }
}

/// Why a candidate does not answer a goal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mismatch {
    /// It does not.
    Rejected,
    /// A projection that only an impl could resolve is in the way, in a search
    /// that doesn't resolve projections through impls.
    Undecided,
}

/// Whether `value` names a projection.
fn names_projection<'db>(db: &'db dyn HirAnalysisDb, value: impl TyVisitable<'db>) -> bool {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }
    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_ty(&mut self, ty: TyId<'db>) {
            self.found |= ty.has_projection(self.db);
        }
    }
    let mut finder = Finder { db, found: false };
    value.visit_with(&mut finder);
    finder.found
}

/// Unifies `a` with `b`, which is undecided rather than a mismatch when
/// impls weren't asked to resolve a projection in either.
fn unify_or_undecided<'db, T>(
    db: &'db dyn HirAnalysisDb,
    table: &mut PersistentUnificationTable<'db>,
    a: T,
    b: T,
    resolve_impls: bool,
) -> Result<(), Mismatch>
where
    T: Copy + TyVisitable<'db> + Unifiable<'db>,
{
    if table.unify(a, b).is_ok() {
        Ok(())
    } else if !resolve_impls && (names_projection(db, a) || names_projection(db, b)) {
        Err(Mismatch::Undecided)
    } else {
        Err(Mismatch::Rejected)
    }
}

fn unify_trait_inst_with_normalized_assoc_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut PersistentUnificationTable<'db>,
    candidate: TraitInstId<'db>,
    goal: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: super::PredicateListId<'db>,
    resolve_impls: bool,
) -> Result<(), Mismatch> {
    unify_or_undecided(
        db,
        table,
        trait_inst_head(db, candidate),
        trait_inst_head(db, goal),
        resolve_impls,
    )?;

    // Each of the goal's equalities holds by the candidate's, which may bind
    // an associated shape at every mode where the goal names one.
    for (&binding, &goal_assoc_ty) in goal.assoc_type_bindings(db) {
        let candidate_assoc_ty = candidate
            .bound_assoc_ty(db, binding)
            .ok_or(Mismatch::Rejected)?;
        let candidate_assoc_ty = normalize_assoc_binding(
            db,
            table,
            candidate_assoc_ty,
            scope,
            assumptions,
            resolve_impls,
        );
        let goal_assoc_ty =
            normalize_assoc_binding(db, table, goal_assoc_ty, scope, assumptions, resolve_impls);
        unify_or_undecided(db, table, candidate_assoc_ty, goal_assoc_ty, resolve_impls)?;
    }

    Ok(())
}

/// Whether `assumption` answers the normalized `goal`: a `mut` goal holds
/// only by a `mut` assumption.
fn assumption_answers<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut PersistentUnificationTable<'db>,
    assumption: TraitInstId<'db>,
    goal: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: super::PredicateListId<'db>,
    resolve_impls: bool,
) -> Result<(), Mismatch> {
    if goal.is_mut(db) && !assumption.is_mut(db) {
        return Err(Mismatch::Rejected);
    }
    unify_trait_inst_with_normalized_assoc_bindings(
        db,
        table,
        assumption,
        goal,
        scope,
        assumptions,
        resolve_impls,
    )
}

/// The position, in `solve_cx`'s extended assumption list, of the first
/// assumption that answers `goal`, in the order the solver tries them.
pub(crate) fn answering_assumption<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Option<usize> {
    let scope = solve_cx.normalization_scope_for_trait_inst(db, goal);
    let assumptions = solve_cx.assumptions().extend_all_bounds(db);
    let resolve_impls = solve_cx.resolve_impls();
    let goal =
        normalize_trait_inst_preserving_validity(db, goal, scope, assumptions, resolve_impls);
    assumptions.list(db).iter().position(|&assumption| {
        assumption_answers(
            db,
            &mut PersistentUnificationTable::new(db),
            assumption,
            goal,
            scope,
            assumptions,
            resolve_impls,
        )
        .is_ok()
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StopReason {
    MaximumTypeDepth,
    TargetFound,
    UnresolvedProjection,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, salsa::SalsaValue)]
pub(crate) enum TargetSolutionStatus {
    Found,
    NotFound,
    Incomplete,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub(crate) enum TargetSolutionMatch {
    Equal,
    NotEqual,
}

#[derive(Clone, Copy)]
enum Clause<'db> {
    Implementor(ImplementorId<'db>),
    Assumption(usize),
    /// The builtin implementation of the callable value, a closure or a
    /// function item, that is the goal's self type (`ty::closure`), built
    /// from the goal in its own table.
    Callable,
    /// The compiler's `Clearable` implementation for the goal's self type
    /// (`ty::clearable`), proved through its component types.
    Clearable,
    /// The compiler's `LaneCodec<T>` implementation for std's Solidity codec
    /// and a `T` without a `SolPacked` encoding (`ty::sol_codec`).
    SolCodec,
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
    scope: ScopeId<'db>,
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
    /// Whether goals normalize their projections through impls (see
    /// [`TraitSolveCx::without_impl_normalization`]).
    resolve_impls: bool,
    prepared_queries: FxHashMap<Query<'db>, PreparedQuery<'db>>,
    target: Option<TargetAnswer<'db>>,
    type_depth_budget: TypeDepthBudget,
}

impl<'db> TraitResolutionContext<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        origin_ingot: crate::Ingot<'db>,
        resolve_impls: bool,
        root: TraitSolverQuery<'db>,
        target: Option<TargetAnswer<'db>>,
    ) -> Self {
        Self {
            db,
            origin_ingot,
            resolve_impls,
            prepared_queries: FxHashMap::default(),
            target,
            type_depth_budget: TypeDepthBudget::new(db, root, target),
        }
    }

    /// How the search goes on after a candidate does not answer a goal.
    fn mismatch_transition(mismatch: Mismatch) -> ContextTransition<Self> {
        match mismatch {
            Mismatch::Rejected => Transition::Reject,
            Mismatch::Undecided => Transition::Stop(StopReason::UnresolvedProjection),
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
        let normalized_goal = normalize_trait_inst_preserving_validity(
            self.db,
            query.goal,
            scope,
            query.assumptions,
            self.resolve_impls,
        );
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
        // Without impl normalization, a goal that still names a projection is
        // undecided: an impl could resolve the projection.
        let goal = prepared.normalized_goal;
        if !self.resolve_impls
            && (goal
                .args(self.db)
                .iter()
                .any(|ty| ty.has_projection(self.db))
                || goal
                    .assoc_type_bindings(self.db)
                    .values()
                    .any(|ty| ty.has_projection(self.db)))
        {
            return Ok(CallbackOutcome::Stop(StopReason::UnresolvedProjection));
        }
        // An impl lives in its normalized self type's ingot or its trait's
        // (`trait_impl_admissibility`): `<Option<P> as Unwrap>::Out: Copy` is
        // `P: Copy`, whose impl is in `P`'s ingot, not `Option`'s.
        let (primary, secondary) = TraitSolveCx::search_ingots_for_trait_inst_with_origin(
            self.db,
            self.origin_ingot,
            prepared.normalized_goal,
        );
        let implementors = impls_for_trait_in_ingots(
            self.db,
            primary,
            secondary,
            Canonical::new(self.db, prepared.normalized_goal),
        );

        let mut clauses =
            Vec::with_capacity(implementors.len() + prepared.query.assumptions.list(self.db).len());
        clauses.extend(implementors.iter().copied().map(Clause::Implementor));
        if callable_implementor(self.db, prepared.normalized_goal).is_some() {
            clauses.push(Clause::Callable);
        }
        if clearable_implementor(self.db, prepared.normalized_goal).is_some() {
            clauses.push(Clause::Clearable);
        }
        if sol_codec_implementor(
            self.db,
            prepared.normalized_goal,
            prepared.scope,
            prepared.query.assumptions,
        )
        .is_some()
        {
            clauses.push(Clause::SolCodec);
        }
        if !prepared.query.require_impl && self.goal_can_use_assumptions(prepared.normalized_goal) {
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

        // A `mut` goal holds by an implementation that gives the `mut` modes,
        // with its `mut?` bounds at `mut`, or by a `mut` assumption.
        let goal_mode = Some(if normalized_goal.is_mut(self.db) {
            BorrowKind::Mut
        } else {
            BorrowKind::Ref
        });
        let selected_impl = match clause {
            Clause::Implementor(selected_impl) => {
                if !impl_header_may_match(self.db, selected_impl, normalized_goal)
                    || normalized_goal.is_mut(self.db) && !selected_impl.gives_mut(self.db)
                {
                    return Ok(Transition::Reject);
                }
                // Candidates are headers; lower only the associated types
                // the goal binds (see `impls_for_trait_def`).
                let Some(candidate) = normalized_goal
                    .assoc_type_bindings(self.db)
                    .keys()
                    .try_fold(selected_impl, |implementor, binding| {
                        complete_candidate_impl_assoc_ty(self.db, implementor, binding.name)
                    })
                else {
                    return Ok(Transition::Reject);
                };
                let candidate = table.instantiate_with_fresh_vars(candidate);
                let normalized_candidate = normalize_trait_inst_preserving_validity(
                    self.db,
                    candidate.trait_inst(self.db),
                    scope,
                    query.assumptions,
                    self.resolve_impls,
                );
                if let Err(mismatch) = unify_trait_inst_with_normalized_assoc_bindings(
                    self.db,
                    &mut table,
                    normalized_candidate,
                    normalized_goal,
                    scope,
                    query.assumptions,
                    self.resolve_impls,
                ) {
                    return Ok(Self::mismatch_transition(mismatch));
                }

                let constraints = candidate.constraints(self.db);
                let remaining_goals = constraints
                    .list(self.db)
                    .iter()
                    .map(|constraint| {
                        constraint
                            .at_mode(self.db, goal_mode)
                            .fold_with(self.db, &mut table)
                    })
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
                if let Err(mismatch) = assumption_answers(
                    self.db,
                    &mut table,
                    assumption,
                    normalized_goal,
                    scope,
                    query.assumptions,
                    self.resolve_impls,
                ) {
                    return Ok(Self::mismatch_transition(mismatch));
                }
                ImplementorId::assumption(self.db, query.goal.fold_with(self.db, &mut table))
            }
            Clause::Clearable => {
                let Some((selected_impl, remaining_goals)) =
                    clearable_implementor(self.db, normalized_goal)
                else {
                    return Ok(Transition::Reject);
                };
                let branch = Branch {
                    table,
                    root_goal: query.goal,
                    remaining_goals,
                    selected_impl,
                };
                return Ok(self.continue_branch(key, branch, query.assumptions));
            }
            Clause::SolCodec => {
                let Some(implementor) =
                    sol_codec_implementor(self.db, normalized_goal, scope, query.assumptions)
                else {
                    return Ok(Transition::Reject);
                };
                implementor
            }
            Clause::Callable => {
                let Some(implementor) = callable_implementor(self.db, normalized_goal) else {
                    return Ok(Transition::Reject);
                };
                if let Err(mismatch) = unify_trait_inst_with_normalized_assoc_bindings(
                    self.db,
                    &mut table,
                    implementor.trait_inst(self.db),
                    normalized_goal,
                    scope,
                    query.assumptions,
                    self.resolve_impls,
                ) {
                    return Ok(Self::mismatch_transition(mismatch));
                }
                implementor
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
                self.resolve_impls,
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
                self.resolve_impls,
            )
        };
        if let Err(mismatch) = unify_or_undecided(
            self.db,
            &mut branch.table,
            normalized_pending,
            normalized_solution,
            self.resolve_impls,
        ) {
            return Ok(Self::mismatch_transition(mismatch));
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
        Completion::Adapter(StopReason::UnresolvedProjection) => {
            TraitSolveCompletion::UnresolvedProjection
        }
        Completion::Adapter(StopReason::TargetFound) => {
            unreachable!("ordinary trait solving never installs a target")
        }
    }
}

pub(super) fn solve<'db>(
    db: &'db dyn HirAnalysisDb,
    origin_ingot: crate::Ingot<'db>,
    query: Query<'db>,
    resolve_impls: bool,
) -> GoalSatisfiability<'db> {
    let mut root_table = PersistentUnificationTable::new(db);
    let root_goal = query.extract_identity(&mut root_table);
    let mut context = TraitResolutionContext::new(db, origin_ingot, resolve_impls, root_goal, None);
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
    let mut context = TraitResolutionContext::new(db, origin_ingot, true, root_goal, Some(target));
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
        | Completion::Adapter(StopReason::MaximumTypeDepth | StopReason::UnresolvedProjection) => {
            TargetSolutionStatus::Incomplete
        }
    }
}

#[salsa::tracked(returns(copy))]
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
