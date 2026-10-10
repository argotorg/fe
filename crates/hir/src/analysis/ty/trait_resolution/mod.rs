use super::{
    binder::Binder,
    canonical::{Canonical, Canonicalized, Solution},
    const_expr::ConstExpr,
    const_ty::ConstTyData,
    fold::TyFoldable,
    normalize::{NormalizationLimit, normalize_from_assumptions},
    trait_def::{ImplementorId, TraitInstId},
    ty_def::{TyData, TyFlags, TyId},
    visitor::{TyVisitable, TyVisitor},
};
use crate::analysis::{
    HirAnalysisDb,
    semantic::{ConstRepr, SemConstId, SemConstValue, sem_const_ty},
    ty::{
        trait_resolution::{
            constraint::ty_constraints,
            table_solver::{TargetSolutionMatch, TargetSolutionStatus, has_solution, solve},
        },
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
mod table_solver;

pub(crate) const TRAIT_SOLVER_ROOT_ANSWER_LIMIT: usize = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct TraitSolverQuery<'db> {
    pub goal: TraitInstId<'db>,
    pub assumptions: PredicateListId<'db>,
    /// Select an implementation at this goal; obligations still use assumptions.
    pub require_impl: bool,
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
                require_impl: false,
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
pub enum Selection<T> {
    Unique(T),
    Ambiguous(IndexSet<T>),
    NotFound,
    /// Selecting reached a normalization limit: no answer, and no other
    /// candidate is chosen instead.
    NormalizationLimit(NormalizationLimit),
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
        let inst = match normalize_trait_inst_preserving_validity(db, inst, scope, self.assumptions)
        {
            Ok(inst) => inst,
            Err(limit) => return Selection::NormalizationLimit(limit),
        };
        // An assumption proves a bound; it is not a second implementation.
        // Keep inference goals on the ordinary proof query: an assumption may
        // select a different substitution from the implementations in scope.
        let result = if inst.args(db).iter().any(|ty| ty.has_var(db))
            || inst
                .assoc_type_bindings(db)
                .values()
                .any(|ty| ty.has_var(db))
        {
            is_goal_satisfiable(db, self, inst)
        } else {
            let query = CanonicalGoalQuery::from_query(
                db,
                TraitSolverQuery {
                    goal: inst,
                    assumptions: self.assumptions.extend_all_bounds(db),
                    require_impl: true,
                },
            );
            match is_goal_query_satisfiable(db, self, &query) {
                // A bound with no provable implementation can still be supplied
                // by the caller. Incomplete searches cannot establish this.
                Ok(GoalSatisfiability::UnSat(_)) => is_goal_satisfiable(db, self, inst),
                result => result,
            }
        };
        match result {
            Ok(GoalSatisfiability::Satisfied(solution)) => {
                Selection::Unique(solution.value.implementor)
            }
            Ok(GoalSatisfiability::NeedsConfirmation { solutions, .. }) => {
                Selection::Ambiguous(solutions.iter().map(|s| s.value.implementor).collect())
            }
            Ok(GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_)) => {
                Selection::NotFound
            }
            Err(limit) => Selection::NormalizationLimit(limit),
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
) -> Result<TraitInstId<'db>, NormalizationLimit> {
    let normalized = inst.normalize(db, scope, assumptions)?;
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
    Ok(if !original_has_invalid && normalized_has_invalid {
        inst
    } else {
        normalized
    })
}

#[salsa::tracked(
    return_ref,
    cycle_fn=is_query_satisfiable_cycle_recover,
    cycle_initial=is_query_satisfiable_cycle_initial
)]
fn is_query_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    origin_ingot: Ingot<'db>,
    query: Canonical<TraitSolverQuery<'db>>,
) -> Result<GoalSatisfiability<'db>, NormalizationLimit> {
    if query.flags(db).contains(TyFlags::HAS_INVALID) {
        return Ok(GoalSatisfiability::ContainsInvalid);
    };

    solve(db, origin_ingot, query)
}

fn is_query_satisfiable_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _origin_ingot: Ingot<'db>,
    _query: Canonical<TraitSolverQuery<'db>>,
) -> Result<GoalSatisfiability<'db>, NormalizationLimit> {
    // A cycle can arise while collecting an impl whose constraints contain an associated-type
    // projection: resolving the projection needs the trait environment that is currently being
    // assembled for the outer goal. Treat the incomplete pass as ambiguous so callers keep the
    // candidate alive; the next fixpoint iteration can decide it once impl collection converges.
    Ok(GoalSatisfiability::NeedsConfirmation {
        solutions: IndexSet::default(),
        completion: TraitSolveCompletion::Cycle,
    })
}

fn is_query_satisfiable_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Result<GoalSatisfiability<'db>, NormalizationLimit>,
    _count: u32,
    _origin_ingot: Ingot<'db>,
    _query: Canonical<TraitSolverQuery<'db>>,
) -> salsa::CycleRecoveryAction<Result<GoalSatisfiability<'db>, NormalizationLimit>> {
    salsa::CycleRecoveryAction::Iterate
}

#[salsa::tracked(
    cycle_fn=query_has_solution_cycle_recover,
    cycle_initial=query_has_solution_cycle_initial
)]
fn query_has_solution<'db>(
    db: &'db dyn HirAnalysisDb,
    origin_ingot: Ingot<'db>,
    query: Canonical<TraitSolverQuery<'db>>,
    target: Canonical<TraitInstId<'db>>,
    relation: TargetSolutionMatch,
) -> TargetSolutionStatus {
    if query.flags(db).contains(TyFlags::HAS_INVALID) {
        return TargetSolutionStatus::NotFound;
    }
    has_solution(db, origin_ingot, query, target, relation)
}

fn query_has_solution_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _origin_ingot: Ingot<'db>,
    _query: Canonical<TraitSolverQuery<'db>>,
    _target: Canonical<TraitInstId<'db>>,
    _relation: TargetSolutionMatch,
) -> TargetSolutionStatus {
    TargetSolutionStatus::Incomplete
}

fn query_has_solution_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &TargetSolutionStatus,
    _count: u32,
    _origin_ingot: Ingot<'db>,
    _query: Canonical<TraitSolverQuery<'db>>,
    _target: Canonical<TraitInstId<'db>>,
    _relation: TargetSolutionMatch,
) -> salsa::CycleRecoveryAction<TargetSolutionStatus> {
    salsa::CycleRecoveryAction::Iterate
}

pub(crate) fn goal_query_has_solution<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    query: &CanonicalGoalQuery<'db>,
    target: Canonical<TraitInstId<'db>>,
) -> bool {
    matches!(
        query_has_solution(
            db,
            solve_cx.origin_ingot(),
            query.canonical(),
            target,
            TargetSolutionMatch::Equal,
        ),
        TargetSolutionStatus::Found
    )
}

pub(crate) fn goal_query_has_no_distinct_solution<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    query: &CanonicalGoalQuery<'db>,
    target: Canonical<TraitInstId<'db>>,
) -> bool {
    matches!(
        query_has_solution(
            db,
            solve_cx.origin_ingot(),
            query.canonical(),
            target,
            TargetSolutionMatch::NotEqual,
        ),
        TargetSolutionStatus::NotFound
    )
}

/// Whether the goal holds. A goal whose answer depends on a normalization
/// limit has no answer: the limit is returned, for the caller to report where
/// the goal arises or to keep as unknown (law 5 of the limits design).
pub fn is_goal_query_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    query: &CanonicalGoalQuery<'db>,
) -> Result<GoalSatisfiability<'db>, NormalizationLimit> {
    is_query_satisfiable(db, solve_cx.origin_ingot(), query.canonical()).clone()
}

/// [`is_goal_query_satisfiable`] for `goal` under the context's assumptions.
pub fn is_goal_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Result<GoalSatisfiability<'db>, NormalizationLimit> {
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
    let mut join = WfJoin::default();
    let (base, args) = ty.decompose_ty_app(db);
    for &arg in args {
        if let Some(wf) = join.add(check_ty_wf(db, solve_cx, arg)) {
            return wf;
        }
    }
    let family_wf = check_family_requirements(db, solve_cx, ty);
    if !family_wf.is_wf() {
        return family_wf;
    }
    match base.data(db) {
        // The body is checked with its declaration, over its own parameters,
        // not here with the caller's types.
        TyData::TypeFamily { .. } => {}
        TyData::AssocTy(assoc) => {
            if let Some(wf) = join.add(check_projected_trait_use_wf(
                db,
                solve_cx,
                assoc.trait_.as_predicate(db),
            )) {
                return wf;
            }
        }
        TyData::QualifiedTy(inst) => {
            if let Some(wf) = join.add(check_projected_trait_use_wf(db, solve_cx, *inst)) {
                return wf;
            }
        }
        TyData::ConstTy(const_ty) => {
            if let Some(wf) = join.add(check_const_ty_wf(db, solve_cx, *const_ty)) {
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
    let scope = solve_cx.origin_scope(db);
    if let Some(wf) = join.add(constraints_wf(db, solve_cx, scope, constraints)) {
        return wf;
    }
    join.finish()
}

/// Whether `constraints` hold, each normalized first to resolve associated
/// types. A constraint that does not hold decides, even after one that
/// reached a limit.
fn constraints_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    scope: ScopeId<'db>,
    constraints: PredicateListId<'db>,
) -> WellFormedness<'db> {
    let mut join = WfJoin::default();
    for &goal in constraints.list(db) {
        let wf = match goal.normalize(db, scope, solve_cx.assumptions()) {
            Ok(goal) => unsatisfied_goal(db, solve_cx, goal).unwrap_or(WellFormedness::WellFormed),
            Err(limit) => WellFormedness::NormalizationLimit(limit),
        };
        if let Some(wf) = join.add(wf) {
            return wf;
        }
    }
    join.finish()
}

/// Joins the well-formedness of the parts of one thing: an ill-formed part
/// decides, even after a part that reached a limit, since the answer is then
/// the same whatever the limited part is; otherwise a part that reached a
/// limit leaves the whole undecided, and its limit is the answer.
#[derive(Default)]
struct WfJoin<'db> {
    limit: Option<WellFormedness<'db>>,
}

impl<'db> WfJoin<'db> {
    /// Adds a part; returns the answer once a part decides it.
    fn add(&mut self, wf: WellFormedness<'db>) -> Option<WellFormedness<'db>> {
        match wf {
            WellFormedness::WellFormed => None,
            WellFormedness::IllFormed { .. } | WellFormedness::Undecided { .. } => Some(wf),
            WellFormedness::NormalizationLimit(limit) => {
                let earlier = match self.limit {
                    Some(WellFormedness::NormalizationLimit(earlier)) => Some(earlier),
                    _ => None,
                };
                self.limit = Some(WellFormedness::NormalizationLimit(
                    NormalizationLimit::join(earlier, limit),
                ));
                None
            }
        }
    }

    /// Adds the last part and returns the answer.
    fn last(mut self, wf: WellFormedness<'db>) -> WellFormedness<'db> {
        self.add(wf).unwrap_or_else(|| self.finish())
    }

    fn finish(self) -> WellFormedness<'db> {
        self.limit.unwrap_or(WellFormedness::WellFormed)
    }
}

fn check_const_ty_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    const_ty: super::const_ty::ConstTyId<'db>,
) -> WellFormedness<'db> {
    let mut join = WfJoin::default();
    if let Some(wf) = join.add(check_ty_wf(db, solve_cx, const_ty.ty(db))) {
        return wf;
    }

    match const_ty.data(db) {
        ConstTyData::Computation { description, .. } => {
            let evaluated = const_ty.evaluate(db, Some(description.ty()));
            if evaluated != const_ty {
                return join.last(check_ty_wf(db, solve_cx, TyId::const_ty(db, evaluated)));
            }
            if let ConstRepr::Term(term) = description.repr() {
                return join.last(check_ty_wf(db, solve_cx, TyId::const_ty(db, *term)));
            }
        }
        ConstTyData::Value(value) => {
            return join.last(check_sem_const_wf(db, solve_cx, value.value()));
        }
        ConstTyData::Description(value) => {
            return join.last(check_sem_const_wf(db, solve_cx, *value));
        }
        ConstTyData::Abstract(expr, _) => {
            if let Some(wf) = join.add(check_const_expr_wf(db, solve_cx, *expr)) {
                return wf;
            }
        }
        ConstTyData::TyVar(..)
        | ConstTyData::TyParam(..)
        | ConstTyData::Hole(..)
        | ConstTyData::Invalid(..)
        | ConstTyData::UnEvaluated { .. } => {}
    }

    join.finish()
}

fn check_sem_const_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    value: SemConstId<'db>,
) -> WellFormedness<'db> {
    let mut join = WfJoin::default();
    match value.value(db) {
        SemConstValue::Description(term) => check_ty_wf(db, solve_cx, TyId::const_ty(db, *term)),
        SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => {
            for child in elems.iter().copied() {
                if let Some(wf) = join.add(check_ty_wf(db, solve_cx, sem_const_ty(db, child))) {
                    return wf;
                }
                if let Some(wf) = join.add(check_sem_const_wf(db, solve_cx, child)) {
                    return wf;
                }
            }
            join.finish()
        }
        SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => {
            for child in fields.iter().copied() {
                if let Some(wf) = join.add(check_ty_wf(db, solve_cx, sem_const_ty(db, child))) {
                    return wf;
                }
                if let Some(wf) = join.add(check_sem_const_wf(db, solve_cx, child)) {
                    return wf;
                }
            }
            join.finish()
        }
        SemConstValue::Unit | SemConstValue::Scalar { .. } => WellFormedness::WellFormed,
    }
}

fn check_const_expr_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    expr: super::const_expr::ConstExprId<'db>,
) -> WellFormedness<'db> {
    struct TyCollector<'db> {
        db: &'db dyn HirAnalysisDb,
        tys: Vec<TyId<'db>>,
    }

    impl<'db> TyVisitor<'db> for TyCollector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            self.tys.push(ty);
        }
    }

    let mut join = WfJoin::default();
    match expr.data(db) {
        ConstExpr::Invocation(invocation) => {
            let mut collector = TyCollector {
                db,
                tys: Vec::new(),
            };
            invocation.key.visit_with(&mut collector);
            invocation.args.visit_with(&mut collector);
            for ty in collector.tys {
                if let Some(wf) = join.add(check_ty_wf(db, solve_cx, ty)) {
                    return wf;
                }
            }
        }
        ConstExpr::ArithBinOp { lhs, rhs, .. }
        | ConstExpr::ArrayRepeat {
            value: lhs,
            len: rhs,
        }
        | ConstExpr::ArrayIndex {
            array: lhs,
            index: rhs,
        } => {
            for ty in [*lhs, *rhs] {
                if let Some(wf) = join.add(check_ty_wf(db, solve_cx, ty)) {
                    return wf;
                }
            }
        }
        ConstExpr::UnOp { expr, .. } | ConstExpr::Field { value: expr, .. } => {
            if let Some(wf) = join.add(check_ty_wf(db, solve_cx, *expr)) {
                return wf;
            }
        }
        ConstExpr::Cast { expr, to } => {
            for ty in [*expr, *to] {
                if let Some(wf) = join.add(check_ty_wf(db, solve_cx, ty)) {
                    return wf;
                }
            }
        }
        ConstExpr::TraitConst(assoc) => {
            if let Some(wf) = join.add(check_projected_trait_use_wf(db, solve_cx, assoc.inst())) {
                return wf;
            }
        }
        ConstExpr::InherentConst(use_) => {
            if let Some(wf) = join.add(check_ty_wf(db, solve_cx, use_.receiver_ty())) {
                return wf;
            }
        }
    }

    join.finish()
}

/// Whether `goal` is proved. Used for a bound promised for every argument of
/// a family, one of its parameter bounds or declared bounds: an ambiguous or
/// unfinished proof does not show that, so it counts as a failure. A limit is
/// neither, and is returned.
pub(crate) fn bound_is_proved<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Result<bool, NormalizationLimit> {
    Ok(matches!(
        is_goal_satisfiable(db, solve_cx, goal)?,
        GoalSatisfiability::Satisfied(_)
    ))
}

/// Check a family application's parameter requirements before normalization
/// can erase the application. Source-path diagnostics also use this, since
/// they see intermediate projections.
pub(crate) fn check_family_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    ty: TyId<'db>,
) -> WellFormedness<'db> {
    match ty.family_parameter_bounds(db) {
        None => {}
        Some(Err(goal)) => {
            return WellFormedness::IllFormed {
                goal,
                subgoal: None,
            };
        }
        Some(Ok(requirements)) => {
            // A requirement that fails decides, even after an unknown one.
            let mut join = WfJoin::default();
            for &goal in requirements.list(db) {
                let wf = match bound_is_proved(db, solve_cx, goal) {
                    Ok(true) => WellFormedness::WellFormed,
                    Ok(false) => WellFormedness::IllFormed {
                        goal,
                        subgoal: None,
                    },
                    Err(limit) => WellFormedness::NormalizationLimit(limit),
                };
                if let Some(wf) = join.add(wf) {
                    return wf;
                }
            }
            return join.finish();
        }
    }
    WellFormedness::WellFormed
}

fn check_projected_trait_use_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> WellFormedness<'db> {
    let mut join = WfJoin::default();
    for &arg in inst.args(db) {
        if let Some(wf) = join.add(check_ty_wf(db, solve_cx, arg)) {
            return wf;
        }
    }
    for &ty in inst.assoc_type_bindings(db).values() {
        if let Some(wf) = join.add(check_ty_wf(db, solve_cx, ty)) {
            return wf;
        }
    }

    join.last(unsatisfied_goal(db, solve_cx, inst).unwrap_or(WellFormedness::WellFormed))
}

fn unsatisfied_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Option<WellFormedness<'db>> {
    let assumptions = solve_cx.assumptions();
    let mut table = UnificationTable::new(db);
    let query = CanonicalGoalQuery::new(db, goal, assumptions);
    match is_goal_query_satisfiable(db, solve_cx, &query) {
        Ok(GoalSatisfiability::UnSat(subgoal)) => {
            let subgoal = subgoal.map(|subgoal| query.extract_subgoal(&mut table, subgoal));
            Some(WellFormedness::IllFormed { goal, subgoal })
        }
        Err(limit) => Some(WellFormedness::NormalizationLimit(limit)),
        // The solver stopped on one of its own budgets before it found any
        // proof: the goal is not proved, so the type is not accepted. A goal
        // with answers, complete or not, is proved.
        Ok(GoalSatisfiability::NeedsConfirmation {
            solutions,
            completion,
        }) if solutions.is_empty() && completion.is_budget_stop() => {
            Some(WellFormedness::Undecided {
                goal,
                stop: completion,
            })
        }
        Ok(
            GoalSatisfiability::Satisfied(_)
            | GoalSatisfiability::NeedsConfirmation { .. }
            | GoalSatisfiability::ContainsInvalid,
        ) => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
pub(crate) enum WellFormedness<'db> {
    WellFormed,
    IllFormed {
        goal: TraitInstId<'db>,
        subgoal: Option<TraitInstId<'db>>,
    },
    /// Deciding it reached a normalization limit.
    NormalizationLimit(NormalizationLimit),
    /// The trait solver stopped on one of its budgets before it found a
    /// proof of `goal`.
    Undecided {
        goal: TraitInstId<'db>,
        stop: TraitSolveCompletion,
    },
}

impl<'db> WellFormedness<'db> {
    pub(crate) fn is_wf(self) -> bool {
        matches!(self, WellFormedness::WellFormed)
    }

    /// Drops the unsatisfied subgoal, for sites that report only the goal.
    pub(crate) fn without_subgoal(self) -> Self {
        match self {
            Self::IllFormed { goal, .. } => Self::IllFormed {
                goal,
                subgoal: None,
            },
            other => other,
        }
    }

    pub(crate) fn into_diag(
        self,
        span: crate::span::DynLazySpan<'db>,
    ) -> Option<super::diagnostics::TyDiagCollection<'db>> {
        match self {
            Self::WellFormed => None,
            Self::IllFormed { goal, subgoal } => Some(
                super::diagnostics::TraitConstraintDiag::TraitBoundNotSat {
                    span,
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                    required_by: None,
                    capability_hint: None,
                }
                .into(),
            ),
            Self::NormalizationLimit(limit) => Some(limit.report(span).0),
            Self::Undecided { goal, stop } => Some(
                super::diagnostics::TraitConstraintDiag::TraitBoundUndecided { span, goal, stop }
                    .into(),
            ),
        }
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
    let mut join = WfJoin::default();
    for &arg in trait_inst.args(db) {
        if let Some(wf) = join.add(check_ty_wf(db, solve_cx, arg)) {
            return wf;
        }
    }
    for &ty in trait_inst.assoc_type_bindings(db).values() {
        if let Some(wf) = join.add(check_ty_wf(db, solve_cx, ty)) {
            return wf;
        }
    }

    let constraints =
        collect_constraints(db, trait_inst.def(db).into()).instantiate(db, trait_inst.args(db));
    let scope = solve_cx.normalization_scope_for_trait_inst(db, trait_inst);
    join.last(constraints_wf(db, solve_cx, scope, constraints))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct TraitGoalSolution<'db> {
    pub(crate) inst: TraitInstId<'db>,
    pub(crate) implementor: ImplementorId<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum TraitSolveCompletion {
    /// The complete least-fixpoint answer set was computed.
    Saturated,
    /// The configured number of root proof identities was reached.
    RootAnswerLimit { limit: usize },
    /// The engine's work-item budget was exhausted.
    StepLimit { limit: usize },
    /// The engine's canonical-table budget was exhausted.
    TableLimit { limit: usize },
    /// The engine's pending-work budget was exhausted.
    PendingWorkLimit { limit: usize },
    /// Fe's bounded-type-growth guard stopped the search.
    MaximumTypeDepth,
    /// Salsa is iterating a recursive query to a fixpoint.
    Cycle,
    /// Normalizing a goal or a candidate reached a limit.
    NormalizationLimit(NormalizationLimit),
}

impl TraitSolveCompletion {
    pub fn is_saturated(self) -> bool {
        matches!(self, Self::Saturated)
    }

    pub fn hit_root_answer_limit(self) -> bool {
        matches!(self, Self::RootAnswerLimit { .. })
    }

    /// Whether the search stopped on one of the solver's own budgets (steps,
    /// subgoals, growing types) rather than finishing.
    pub fn is_budget_stop(self) -> bool {
        matches!(
            self,
            Self::StepLimit { .. }
                | Self::TableLimit { .. }
                | Self::PendingWorkLimit { .. }
                | Self::MaximumTypeDepth
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub enum GoalSatisfiability<'db> {
    /// Goal is satisfied with the unique solution.
    Satisfied(Solution<TraitGoalSolution<'db>>),
    /// The goal has multiple complete answers or resolution stopped before
    /// satisfiability and uniqueness could be decided.
    NeedsConfirmation {
        /// Complete or partial answers proved before resolution stopped.
        solutions: IndexSet<Solution<TraitGoalSolution<'db>>>,
        /// Whether the answer set is complete, or why resolution stopped.
        completion: TraitSolveCompletion,
    },

    /// Goal contains invalid.
    ContainsInvalid,
    /// The goal is not satisfied.
    /// It contains an unsatisfied subgoal if we can know the exact subgoal
    /// that makes the proof step stuck.
    UnSat(Option<Solution<TraitInstId<'db>>>),
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
        let mut predicates: IndexSet<_> = self.list(db).iter().copied().collect();
        predicates.extend(other.list(db).iter().copied());
        PredicateListId::new(db, predicates.into_iter().collect::<Vec<_>>())
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
        extend_all_bounds_query(db, self)
    }

    /// Adds `extra` and then every bound implied by the combined list.
    pub(crate) fn extended_with(self, db: &'db dyn HirAnalysisDb, extra: Self) -> Self {
        let mut predicates = self.list(db).to_vec();
        predicates.extend_from_slice(extra.list(db));
        Self::new(db, predicates).extend_all_bounds(db)
    }

    fn extend_all_bounds_uncached(self, db: &'db dyn HirAnalysisDb) -> Self {
        let mut all_predicates: IndexSet<TraitInstId<'db>> =
            self.list(db).iter().copied().collect();

        let mut worklist: Vec<TraitInstId<'db>> = self.list(db).to_vec();

        while let Some(pred) = worklist.pop() {
            let hir_trait = pred.def(db);
            let evidence = PredicateListId::new(db, vec![pred]);
            // 1. Collect super traits
            for super_trait in hir_trait.super_traits(db) {
                let inst = normalize_from_assumptions(
                    db,
                    super_trait.instantiate(db, pred.args(db)),
                    hir_trait.scope(),
                    evidence,
                );
                if predicate_has_recursive_assoc_projection(db, inst) {
                    continue;
                }

                if all_predicates.insert(inst) {
                    // New predicate added, add to worklist for further processing
                    worklist.push(inst);
                }
            }

            // 2. Collect associated type bounds
            let formal_trait =
                TraitInstId::new_simple(db, hir_trait, hir_trait.params(db).to_vec());
            for trait_type in hir_trait.assoc_types(db) {
                // A family's bounds hold for its applications, under the bounds
                // on its parameters, not for the family itself, so they are not
                // bounds this predicate implies.
                if !trait_type.generic_params(db).data(db).is_empty() {
                    continue;
                }
                // Get the associated type name
                let Some(assoc_ty_name) = trait_type.name(db) else {
                    continue;
                };

                // Keep the entire bound in declaration coordinates until every
                // formal, including non-Self trait parameters, is instantiated.
                let assoc_ty = TyId::assoc_ty(db, formal_trait.trait_ref(db), assoc_ty_name);

                for bound in assoc_ty.assoc_type_bounds(db, trait_type) {
                    let trait_inst = normalize_from_assumptions(
                        db,
                        Binder::bind(hir_trait.into(), bound).instantiate(db, pred.args(db)),
                        hir_trait.scope(),
                        evidence,
                    );
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

// Normalizing implied bounds can resolve traits under assumptions that lead
// back here. Iterate from the unextended list; extension only adds bounds.
#[salsa::tracked(
    cycle_initial = extend_all_bounds_cycle_initial,
    cycle_fn = extend_all_bounds_cycle_recover
)]
fn extend_all_bounds_query<'db>(
    db: &'db dyn HirAnalysisDb,
    predicates: PredicateListId<'db>,
) -> PredicateListId<'db> {
    predicates.extend_all_bounds_uncached(db)
}

fn extend_all_bounds_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    predicates: PredicateListId<'db>,
) -> PredicateListId<'db> {
    predicates
}

fn extend_all_bounds_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &PredicateListId<'db>,
    _count: u32,
    _predicates: PredicateListId<'db>,
) -> salsa::CycleRecoveryAction<PredicateListId<'db>> {
    salsa::CycleRecoveryAction::Iterate
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
    use common::indexmap::{IndexMap, IndexSet};

    use super::{
        CanonicalGoalQuery, GoalSatisfiability, Selection, TraitInstId, TraitSolveCompletion,
        TraitSolveCx, goal_query_has_solution, is_goal_query_satisfiable, is_goal_satisfiable,
    };
    use crate::{
        analysis::ty::{
            adt_def::AdtRef,
            canonical::Canonical,
            trait_def::{ImplementorOrigin, resolve_trait_impl_instance},
            trait_resolution::{PredicateListId, constraint::collect_func_def_constraints},
            ty_def::{Kind, TyId, TyVarSort},
            ty_lower::collect_generic_params,
            unify::UnificationTable,
        },
        hir_def::{Func, IdentId, TopLevelMod, Trait},
        test_db::{HirAnalysisTestDb, find_func},
    };

    fn named_trait<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
        name: &str,
    ) -> Trait<'db> {
        top_mod
            .all_traits(db)
            .iter()
            .copied()
            .find(|trait_| {
                trait_
                    .name(db)
                    .to_opt()
                    .is_some_and(|ident| ident.data(db) == name)
            })
            .unwrap_or_else(|| panic!("missing `{name}` trait"))
    }

    fn named_struct_ty<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
        name: &str,
    ) -> TyId<'db> {
        let struct_ = top_mod
            .all_structs(db)
            .iter()
            .copied()
            .find(|struct_| {
                struct_
                    .name(db)
                    .to_opt()
                    .is_some_and(|ident| ident.data(db) == name)
            })
            .unwrap_or_else(|| panic!("missing `{name}` struct"));
        TyId::adt(db, AdtRef::from(struct_).as_adt(db))
    }

    fn nested_ty<'db>(
        db: &'db HirAnalysisTestDb,
        constructor: TyId<'db>,
        mut inner: TyId<'db>,
        depth: usize,
    ) -> TyId<'db> {
        for _ in 0..depth {
            inner = TyId::app(db, constructor, inner);
        }
        inner
    }

    #[test]
    fn predicate_merge_is_stable_and_idempotent() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("predicate_merge.fe".into(), "trait A {}\ntrait B {}");
        let (top_mod, _) = db.top_mod(file);
        let a = TraitInstId::new(
            &db,
            named_trait(&db, top_mod, "A"),
            vec![TyId::bool(&db)],
            IndexMap::new(),
        );
        let b = TraitInstId::new(
            &db,
            named_trait(&db, top_mod, "B"),
            vec![TyId::bool(&db)],
            IndexMap::new(),
        );
        let left = PredicateListId::new(&db, vec![a, b, a]);
        let right = PredicateListId::new(&db, vec![b, a]);
        let merged = left.merge(&db, right);
        assert_eq!(merged.list(&db), &[a, b]);
        assert_eq!(merged.merge(&db, left), merged);
    }

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
            is_goal_query_satisfiable(&db, with_cx, &with_query).unwrap(),
            GoalSatisfiability::Satisfied(_)
        ));
        assert!(matches!(
            is_goal_query_satisfiable(&db, without_cx, &without_query).unwrap(),
            GoalSatisfiability::UnSat(_)
        ));
    }

    #[test]
    fn tablesolve_classifies_cycles_and_distinct_implementors() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "tablesolve_classifies_cycles_and_distinct_implementors.fe".into(),
            r#"
trait Foo {}

struct Seed {}
struct SeedPeer {}
impl Foo for Seed {}
impl Foo for Seed where SeedPeer: Foo {}
impl Foo for SeedPeer where Seed: Foo {}

struct Dead {}
struct DeadPeer {}
impl Foo for Dead where DeadPeer: Foo {}
impl Foo for DeadPeer where Dead: Foo {}

struct Ambiguous {}
impl Foo for Ambiguous {}
impl Foo for Ambiguous {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let foo = named_trait(&db, top_mod, "Foo");
        let solve = |name| {
            let self_ty = named_struct_ty(&db, top_mod, name);
            let goal = TraitInstId::new(&db, foo, vec![self_ty], IndexMap::new());
            is_goal_satisfiable(&db, TraitSolveCx::new(&db, top_mod.scope()), goal).unwrap()
        };

        assert!(matches!(
            solve("SeedPeer"),
            GoalSatisfiability::Satisfied(_)
        ));
        assert!(matches!(solve("Dead"), GoalSatisfiability::UnSat(Some(_))));
        assert!(matches!(
            solve("Ambiguous"),
            GoalSatisfiability::NeedsConfirmation {
                solutions,
                completion: TraitSolveCompletion::Saturated,
            } if solutions.len() == 2
        ));
    }

    #[test]
    fn tablesolve_bounds_growing_seedless_cycles() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "tablesolve_bounds_growing_seedless_cycles.fe".into(),
            r#"
trait Foo {}
struct Dead {}
struct Wrap<T> {}
impl<T> Foo for T where Wrap<T>: Foo {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let foo = named_trait(&db, top_mod, "Foo");
        let dead = named_struct_ty(&db, top_mod, "Dead");
        let wrap = named_struct_ty(&db, top_mod, "Wrap");
        let deep_root = nested_ty(&db, wrap, dead, 300);

        for self_ty in [dead, deep_root] {
            let goal = TraitInstId::new(&db, foo, vec![self_ty], IndexMap::new());
            assert!(matches!(
                is_goal_satisfiable(&db, TraitSolveCx::new(&db, top_mod.scope()), goal).unwrap(),
                GoalSatisfiability::NeedsConfirmation {
                    solutions,
                    completion: TraitSolveCompletion::MaximumTypeDepth,
                } if solutions.is_empty()
            ));
        }
    }

    #[test]
    fn tablesolve_allows_initially_deep_finite_queries() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "tablesolve_allows_initially_deep_finite_queries.fe".into(),
            r#"
trait Foo {}
trait Noise<T> {}

struct Leaf {}
struct Subject {}
struct Wrap<T> {}

impl<T> Foo for Wrap<T> {}
impl Foo for Subject {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let foo = named_trait(&db, top_mod, "Foo");
        let noise = named_trait(&db, top_mod, "Noise");
        let leaf = named_struct_ty(&db, top_mod, "Leaf");
        let subject = named_struct_ty(&db, top_mod, "Subject");
        let wrap = named_struct_ty(&db, top_mod, "Wrap");
        let deep = nested_ty(&db, wrap, leaf, 300);

        let deep_goal = TraitInstId::new(&db, foo, vec![deep], IndexMap::new());
        assert!(matches!(
            is_goal_satisfiable(&db, TraitSolveCx::new(&db, top_mod.scope()), deep_goal,).unwrap(),
            GoalSatisfiability::Satisfied(_)
        ));

        let unrelated_deep_assumption =
            TraitInstId::new(&db, noise, vec![subject, deep], IndexMap::new());
        let assumptions = PredicateListId::new(&db, vec![unrelated_deep_assumption]);
        let shallow_goal = TraitInstId::new(&db, foo, vec![subject], IndexMap::new());
        assert!(matches!(
            is_goal_satisfiable(
                &db,
                TraitSolveCx::new(&db, top_mod.scope()).with_assumptions(assumptions),
                shallow_goal,
            )
            .unwrap(),
            GoalSatisfiability::Satisfied(_)
        ));
    }

    #[test]
    fn tablesolve_preserves_answers_when_growth_limit_stops_search() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "tablesolve_preserves_answers_when_growth_limit_stops_search.fe".into(),
            r#"
trait Seed {}
trait Grow {}

struct Wrap<T> {}

impl<T> Grow for T where T: Seed {}
impl<T> Grow for T where Wrap<T>: Grow {}

fn probe<T: Seed>() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let grow = named_trait(&db, top_mod, "Grow");
        let probe = top_mod
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "probe")
            })
            .expect("missing `probe` function");
        let ty_param = collect_generic_params(&db, probe.into()).explicit_params(&db)[0];
        let assumptions =
            collect_func_def_constraints(&db, probe.into(), true).instantiate_identity();
        let goal = TraitInstId::new(&db, grow, vec![ty_param], IndexMap::new());

        assert!(matches!(
            is_goal_satisfiable(
                &db,
                TraitSolveCx::new(&db, top_mod.scope()).with_assumptions(assumptions),
                goal,
            ).unwrap(),
            GoalSatisfiability::NeedsConfirmation {
                solutions,
                completion: TraitSolveCompletion::MaximumTypeDepth,
            } if solutions.len() == 1
        ));
    }

    #[test]
    fn tablesolve_propagates_bindings_between_impl_constraints() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "tablesolve_propagates_bindings_between_impl_constraints.fe".into(),
            r#"
trait Goal<T> {}
trait Choose<T> {}
trait Accept<T> {}

struct Subject {}
struct A {}
struct B {}

impl Choose<A> for Subject {}
impl Accept<A> for Subject {}
impl Accept<B> for Subject {}
impl<SelfT, T> Goal<T> for SelfT where SelfT: Accept<T>, SelfT: Choose<T> {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let goal_trait = named_trait(&db, top_mod, "Goal");
        let subject = named_struct_ty(&db, top_mod, "Subject");
        let a = named_struct_ty(&db, top_mod, "A");
        let assumptions = PredicateListId::empty_list(&db);
        let solve_cx = TraitSolveCx::new(&db, top_mod.scope()).with_assumptions(assumptions);
        let mut table = UnificationTable::new(&db);
        let selected = table.new_var(TyVarSort::General, &Kind::Star);
        let goal = TraitInstId::new(&db, goal_trait, vec![subject, selected], IndexMap::new());
        let query = CanonicalGoalQuery::new(&db, goal, assumptions);
        let GoalSatisfiability::Satisfied(solution) =
            is_goal_query_satisfiable(&db, solve_cx, &query).unwrap()
        else {
            panic!("the second constraint must observe the first constraint's binding");
        };
        let actual = query.extract_solution(&mut table, solution).inst;
        let expected = TraitInstId::new(&db, goal_trait, vec![subject, a], IndexMap::new());

        assert_eq!(Canonical::new(&db, actual), Canonical::new(&db, expected));
    }

    #[test]
    fn target_search_finds_an_answer_beyond_the_ambiguity_cutoff() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "target_search_finds_an_answer_beyond_the_ambiguity_cutoff.fe".into(),
            r#"
trait Foo {}
struct First {}
struct Second {}
struct Third {}
struct Wrap<T> {}
impl Foo for First {}
impl Foo for First {}
impl<T> Foo for T where Wrap<T>: Foo {}
impl Foo for Second {}
impl Foo for Third {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let foo = named_trait(&db, top_mod, "Foo");
        let assumptions = PredicateListId::empty_list(&db);
        let solve_cx = TraitSolveCx::new(&db, top_mod.scope()).with_assumptions(assumptions);
        let mut table = UnificationTable::new(&db);
        let self_ty = table.new_var(TyVarSort::General, &Kind::Star);
        let goal = TraitInstId::new(&db, foo, vec![self_ty], IndexMap::new());
        let query = CanonicalGoalQuery::new(&db, goal, assumptions);
        let GoalSatisfiability::NeedsConfirmation {
            solutions,
            completion: TraitSolveCompletion::RootAnswerLimit { limit: 2 },
        } = is_goal_query_satisfiable(&db, solve_cx, &query).unwrap()
        else {
            panic!("the unconstrained goal must reach the configured answer cutoff");
        };
        assert_eq!(solutions.len(), 2);

        let returned: IndexSet<_> = solutions
            .iter()
            .map(|solution| Canonical::new(&db, solution.value.inst))
            .collect();
        assert_eq!(
            returned.len(),
            1,
            "the cutoff can contain two implementors of the same instance"
        );
        let target = ["First", "Second", "Third"]
            .into_iter()
            .map(|name| {
                let self_ty = named_struct_ty(&db, top_mod, name);
                Canonical::new(
                    &db,
                    TraitInstId::new(&db, foo, vec![self_ty], IndexMap::new()),
                )
            })
            .find(|candidate| !returned.contains(candidate))
            .expect("the two-answer cutoff must omit one implementation");

        assert!(goal_query_has_solution(&db, solve_cx, &query, target));
    }
    #[test]
    fn implementation_selection_uses_assumptions_for_obligations() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "implementation_selection.fe".into(),
            r#"
trait Bound {}
trait Picks { type Output }
struct Wrap<T> {}
impl<T: Bound> Picks for Wrap<T> { type Output = T }
fn supported<T: Bound>() {}
fn unsupported<T>() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let picks = named_trait(&db, top_mod, "Picks");
        let wrap = named_struct_ty(&db, top_mod, "Wrap");
        for (name, implementation) in [("supported", true), ("unsupported", false)] {
            let func = find_func(&db, top_mod, name);
            let parameter = collect_generic_params(&db, func.into()).explicit_params(&db)[0];
            let self_ty = TyId::app(&db, wrap, parameter);
            let goal = TraitInstId::new(&db, picks, vec![self_ty], IndexMap::new());
            let constraints =
                collect_func_def_constraints(&db, func.into(), true).instantiate_identity();
            let assumptions = PredicateListId::new(
                &db,
                constraints
                    .list(&db)
                    .iter()
                    .copied()
                    .chain([goal])
                    .collect::<Vec<_>>(),
            );
            let solve = TraitSolveCx::new(&db, func.scope()).with_assumptions(assumptions);
            let Selection::Unique(resolved) = resolve_trait_impl_instance(&db, solve, goal) else {
                panic!("{name}: expected unique evidence");
            };
            assert_eq!(
                !matches!(
                    resolved.selected().origin(&db),
                    ImplementorOrigin::Assumption
                ),
                implementation
            );
            if implementation {
                assert_eq!(
                    resolved.instantiated_assoc_ty(&db, IdentId::new(&db, "Output")),
                    Some(parameter)
                );
            }
        }
    }

    #[test]
    fn implementation_selection_preserves_competing_impls_with_an_assumption() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "competing_implementation_selection.fe".into(),
            r#"
trait Picks {}
struct Wrap<T> {}
impl<T> Picks for Wrap<T> {}
impl<T> Picks for Wrap<T> {}
fn probe<T>() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let func = find_func(&db, top_mod, "probe");
        let parameter = collect_generic_params(&db, func.into()).explicit_params(&db)[0];
        let self_ty = TyId::app(&db, named_struct_ty(&db, top_mod, "Wrap"), parameter);
        let goal = TraitInstId::new(
            &db,
            named_trait(&db, top_mod, "Picks"),
            vec![self_ty],
            IndexMap::new(),
        );
        let solve = TraitSolveCx::new(&db, func.scope())
            .with_assumptions(PredicateListId::new(&db, vec![goal]));
        assert!(
            matches!(solve.select_impl(&db, goal), Selection::Ambiguous(implementors) if implementors.len() == 2)
        );
    }

    #[test]
    fn implementation_selection_does_not_discard_inference_answers_from_assumptions() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "inferred_implementation_selection.fe".into(),
            r#"
trait Picks {}
struct Concrete {}
impl Picks for Concrete {}
fn probe<T: Picks>() {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let func = find_func(&db, top_mod, "probe");
        let assumptions =
            collect_func_def_constraints(&db, func.into(), true).instantiate_identity();
        let mut table = UnificationTable::new(&db);
        let self_ty = table.new_var(TyVarSort::General, &Kind::Star);
        let goal = TraitInstId::new(
            &db,
            named_trait(&db, top_mod, "Picks"),
            vec![self_ty],
            IndexMap::new(),
        );
        let solve = TraitSolveCx::new(&db, func.scope()).with_assumptions(assumptions);
        assert!(matches!(
            solve.select_impl(&db, goal),
            Selection::Ambiguous(_)
        ));
    }
}
