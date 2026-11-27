use super::{
    binder::Binder,
    canonical::{Canonical, Canonicalized, Solution},
    const_expr::ConstExpr,
    const_ty::ConstTyData,
    fold::TyFoldable,
    normalize::{normalize_from_assumptions, normalize_ty},
    trait_def::{ImplementorId, TraitInstId},
    ty_def::{BorrowKind, TyData, TyFlags, TyId},
    visitor::{TyVisitable, TyVisitor, walk_ty},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::Tier,
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
    hir_def::{HirIngot, IdentId, ModeMarker, scope_graph::ScopeId},
};
use common::indexmap::IndexSet;
use constraint::collect_constraints;
use rustc_hash::FxHashSet;
use salsa::SalsaValue;

mod component_walk;
pub(crate) mod constraint;
pub(crate) mod derive;
mod table_solver;

pub(in crate::analysis::ty) use component_walk::component_path_goal;
pub(crate) use table_solver::{
    ChoiceMatch, answering_assumption, assumption_matcher, common_choice_goal, match_chosen_header,
    match_header_args, match_header_args_possible, match_prepared_header, match_prepared_type,
};

pub(crate) const TRAIT_SOLVER_ROOT_ANSWER_LIMIT: usize = 2;

/// How many associated types below the first concrete bound a derivation is
/// followed, and how many bounds beyond the slots the walk keeps.
pub(crate) const COMPONENT_DEPTH_LIMIT: u32 = 4;
const COMPONENT_MEMBER_LIMIT: usize = 1024;

/// The arguments a named implementation declared inside a generic item
/// inherits from it: the item's own generic parameters, as they stand where
/// the implementation is opened. Folding a choice by the arguments a body is
/// instantiated with substitutes them.
#[salsa::interned]
#[derive(Debug)]
pub struct InheritedArgs<'db> {
    pub args: Vec<TyId<'db>>,
}

impl<'db> TyFoldable<'db> for InheritedArgs<'db> {
    fn super_fold_with<F: super::fold::TyFolder<'db>>(
        self,
        db: &'db dyn HirAnalysisDb,
        folder: &mut F,
    ) -> Self {
        Self::new(db, self.args(db).clone().fold_with(db, folder))
    }
}

impl<'db> TyVisitable<'db> for InheritedArgs<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        self.args(visitor.db()).as_slice().visit_with(visitor);
    }
}

/// A template, optionally applied to explicit lexical arguments. Applied
/// arguments belong to the caller and must never be freshened as impl binders.
/// An unapplied local template keeps the arguments it inherits from its
/// enclosing item, so that substituting the choice substitutes them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct ChosenImpl<'db> {
    pub template: ImplementorId<'db>,
    pub applied: Option<ImplementorId<'db>>,
    pub inherited: Option<InheritedArgs<'db>>,
}
impl<'db> ChosenImpl<'db> {
    /// A template with the arguments it inherits from the item it is
    /// declared in, if it is a named implementation declared in a generic
    /// item.
    pub fn unapplied(db: &'db dyn HirAnalysisDb, template: ImplementorId<'db>) -> Self {
        let inherited = match template.origin(db) {
            super::trait_def::ImplementorOrigin::Hir(owner)
                if owner.selection_name(db).is_some() =>
            {
                crate::hir_def::GenericParamOwner::from(owner)
                    .parent(db)
                    .map(|parent| {
                        InheritedArgs::new(
                            db,
                            super::ty_lower::collect_generic_params(db, parent)
                                .params(db)
                                .to_vec(),
                        )
                    })
            }
            _ => None,
        };
        Self {
            template,
            applied: None,
            inherited,
        }
    }

    pub fn implementation(self) -> ImplementorId<'db> {
        self.applied.unwrap_or(self.template)
    }
    pub(crate) fn instantiate<S: super::unify::UnificationStore<'db>>(
        self,
        table: &mut super::unify::UnificationTableBase<'db, S>,
    ) -> ImplementorId<'db> {
        self.applied.unwrap_or_else(|| {
            let db = table.db;
            if let super::trait_def::ImplementorOrigin::Hir(owner) = self.template.origin(db) {
                let mut params = self
                    .inherited
                    .map(|inherited| inherited.args(db).clone())
                    .unwrap_or_default();
                params.extend(table.instantiate_with_fresh_vars(
                    self.template.params(db)[params.len()..].to_vec(),
                ));
                super::binder::Binder::bind(owner.into(), self.template).instantiate(db, &params)
            } else {
                table.instantiate_with_fresh_vars(self.template)
            }
        })
    }
}
impl<'db> TyFoldable<'db> for ChosenImpl<'db> {
    fn super_fold_with<F: super::fold::TyFolder<'db>>(
        self,
        db: &'db dyn HirAnalysisDb,
        folder: &mut F,
    ) -> Self {
        Self {
            template: self.template,
            applied: self.applied.map(|applied| applied.fold_with(db, folder)),
            inherited: self
                .inherited
                .map(|inherited| inherited.fold_with(db, folder)),
        }
    }
}
fn is_named<'db>(db: &'db dyn HirAnalysisDb, implementation: ImplementorId<'db>) -> bool {
    matches!(implementation.origin(db), super::trait_def::ImplementorOrigin::Hir(item)
        if item.selection_name(db).is_some())
}

impl<'db> TyVisitable<'db> for ChosenImpl<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        if let Some(applied) = self.applied {
            applied.visit_with(visitor);
        }
        if let Some(inherited) = self.inherited {
            inherited.visit_with(visitor);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum ChoiceEntry<'db> {
    Impl(ChosenImpl<'db>),
}

impl<'db> ChoiceEntry<'db> {
    pub fn implementation(self) -> Option<ChosenImpl<'db>> {
        match self {
            Self::Impl(choice) => Some(choice),
        }
    }
}

impl<'db> TyFoldable<'db> for ChoiceEntry<'db> {
    fn super_fold_with<F: super::fold::TyFolder<'db>>(
        self,
        db: &'db dyn HirAnalysisDb,
        folder: &mut F,
    ) -> Self {
        match self {
            Self::Impl(choice) => Self::Impl(choice.fold_with(db, folder)),
        }
    }
}

impl<'db> TyVisitable<'db> for ChoiceEntry<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        match self {
            Self::Impl(choice) => choice.visit_with(visitor),
        }
    }
}

/// Explicit implementations and provider offers in lexical order, outermost first. This is input
/// to proof construction, never an ambient environment attached to a callee.
#[salsa::interned]
#[derive(Debug)]
pub struct ChosenImpls<'db> {
    pub items: Vec<ChoiceEntry<'db>>,
}

impl<'db> ChosenImpls<'db> {
    pub fn push(
        db: &'db dyn HirAnalysisDb,
        outer: Option<Self>,
        implementation: ImplementorId<'db>,
    ) -> Self {
        let mut items = outer
            .map(|outer| outer.items(db).clone())
            .unwrap_or_default();
        items.push(ChoiceEntry::Impl(ChosenImpl::unapplied(db, implementation)));
        Self::new(db, items)
    }
    pub fn push_applied(
        db: &'db dyn HirAnalysisDb,
        outer: Option<Self>,
        template: ImplementorId<'db>,
        applied: ImplementorId<'db>,
    ) -> Self {
        let mut items = outer
            .map(|outer| outer.items(db).clone())
            .unwrap_or_default();
        items.push(ChoiceEntry::Impl(ChosenImpl {
            template,
            applied: Some(applied),
            inherited: None,
        }));
        Self::new(db, items)
    }
}

impl<'db> TyFoldable<'db> for ChosenImpls<'db> {
    fn super_fold_with<F: super::fold::TyFolder<'db>>(
        self,
        db: &'db dyn HirAnalysisDb,
        folder: &mut F,
    ) -> Self {
        Self::new(db, self.items(db).clone().fold_with(db, folder))
    }
}

impl<'db> TyVisitable<'db> for ChosenImpls<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        self.items(visitor.db()).as_slice().visit_with(visitor);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct TraitSolverQuery<'db> {
    pub goal: TraitInstId<'db>,
    pub assumptions: PredicateListId<'db>,
    /// Select an implementation at this goal; obligations still use assumptions.
    pub require_impl: bool,
    pub choices: Option<ChosenImpls<'db>>,
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
                choices: None,
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
        U: TyFoldable<'db> + SalsaValue,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct TraitSolveCx<'db> {
    origin_ingot: Ingot<'db>,
    assumptions: PredicateListId<'db>,
    choices: Option<ChosenImpls<'db>>,
    /// Whether the implementations generated in the origin ingot count. A
    /// bound only gains solutions as implementations are added, so goals are
    /// solved with every implementation; a selection among alternatives that
    /// asks whether a candidate applies solves in the authored tier first.
    tier: Tier,
}

impl<'db> TraitSolveCx<'db> {
    /// A solver context for goals that start in `scope`, solving in `tier` of
    /// the origin ingot. The tier is required: there is no default, so that a
    /// new site has to decide whether the implementations generated in the
    /// ingot may answer it.
    pub fn new(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>, tier: Tier) -> Self {
        Self {
            origin_ingot: scope.ingot(db),
            assumptions: PredicateListId::empty_list(db),
            choices: None,
            tier,
        }
    }

    /// The tier in which the implementations of `ingot` count for this
    /// context: only the origin ingot's own generated implementations are
    /// hidden, because those of any other ingot are sealed.
    pub(crate) fn table_tier(self, db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Tier {
        self.tier.for_table(db, self.origin_ingot, ingot)
    }

    pub(crate) fn tier(self) -> Tier {
        self.tier
    }

    pub fn with_assumptions(self, assumptions: PredicateListId<'db>) -> Self {
        Self {
            assumptions,
            ..self
        }
    }

    pub fn with_choices(self, choices: Option<ChosenImpls<'db>>) -> Self {
        Self { choices, ..self }
    }

    pub(crate) fn has_matching_choice(
        self,
        db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
    ) -> bool {
        self.matching_choice(db, goal).is_some()
    }

    pub(super) fn matching_choice(
        self,
        db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
    ) -> Option<ChoiceMatch<'db>> {
        self.choices?;
        let (query, mut table) = self.prepare_goal(db, goal);
        let scope = self.normalization_scope_for_trait_inst(db, query.goal);
        let goal =
            normalize_trait_inst_preserving_validity(db, query.goal, scope, query.assumptions);
        table_solver::matching_choice_in(
            db,
            query.choices,
            goal,
            &mut table,
            scope,
            query.assumptions,
        )
    }

    pub(super) fn matching_choice_at(
        self,
        db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
        index: usize,
    ) -> Option<ChoiceMatch<'db>> {
        self.choices?;
        let (query, mut table) = self.prepare_goal(db, goal);
        let entry = *query.choices?.items(db).get(index)?;
        let scope = self.normalization_scope_for_trait_inst(db, query.goal);
        let goal =
            normalize_trait_inst_preserving_validity(db, query.goal, scope, query.assumptions);
        table_solver::match_choice_entry(
            db,
            index,
            entry,
            goal,
            &mut table,
            scope,
            query.assumptions,
        )
    }

    pub(super) fn prepare_goal(
        self,
        db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
    ) -> (
        TraitSolverQuery<'db>,
        super::unify::PersistentUnificationTable<'db>,
    ) {
        let mut table = super::unify::PersistentUnificationTable::new(db);
        let query = self
            .query(db, goal)
            .canonical()
            .extract_identity(&mut table);
        (query, table)
    }

    pub(crate) fn matching_named_choice(
        self,
        db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
    ) -> bool {
        matches!(
            self.matching_choice(db, goal),
            Some(ChoiceMatch::Implementation(_, choice))
                if is_named(db, choice.template)
        )
    }

    /// Remove named overrides while retaining ordinary openings in lexical order.
    pub(crate) fn without_named_choices(self, db: &'db dyn HirAnalysisDb) -> Option<Self> {
        let choices = self.choices?.items(db);
        let ordinary = choices
            .iter()
            .copied()
            .filter(|entry| {
                !entry
                    .implementation()
                    .is_some_and(|choice| is_named(db, choice.template))
            })
            .collect::<Vec<_>>();
        (ordinary.len() != choices.len()).then(|| {
            let retained = (!ordinary.is_empty()).then(|| ChosenImpls::new(db, ordinary));
            self.with_choices(retained)
        })
    }

    pub fn choices(self) -> Option<ChosenImpls<'db>> {
        self.choices
    }

    pub fn query(
        self,
        db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
    ) -> CanonicalGoalQuery<'db> {
        CanonicalGoalQuery::from_query(
            db,
            TraitSolverQuery {
                goal,
                assumptions: self.assumptions.extend_all_bounds(db),
                require_impl: false,
                choices: self.choices,
            },
        )
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
        match self.select_impl_solution(db, inst) {
            Selection::Unique(solution) => Selection::Unique(solution.implementor),
            Selection::Ambiguous(solutions) => Selection::Ambiguous(
                solutions
                    .iter()
                    .map(|solution| solution.implementor)
                    .collect(),
            ),
            Selection::NotFound => Selection::NotFound,
        }
    }

    pub(super) fn select_impl_solution(
        self,
        db: &'db dyn HirAnalysisDb,
        inst: TraitInstId<'db>,
    ) -> Selection<TraitGoalSolution<'db>> {
        let scope = self.normalization_scope_for_trait_inst(db, inst);
        let inst = normalize_trait_inst_preserving_validity(db, inst, scope, self.assumptions);
        // An assumption proves a bound; it is not a second implementation.
        // Keep inference goals on the ordinary proof query: an assumption may
        // select a different substitution from the implementations in scope.
        let require_impl = !inst.args(db).iter().any(|ty| ty.has_var(db))
            && !inst
                .assoc_type_bindings(db)
                .values()
                .any(|ty| ty.has_var(db));
        let mut query = CanonicalGoalQuery::from_query(
            db,
            TraitSolverQuery {
                goal: inst,
                assumptions: self.assumptions.extend_all_bounds(db),
                require_impl,
                choices: self.choices,
            },
        );
        let mut result = is_goal_query_satisfiable(db, self, &query);
        // A bound with no provable implementation can still be supplied by
        // the caller. Incomplete searches cannot establish this.
        if require_impl && matches!(result, GoalSatisfiability::UnSat(_)) {
            query = self.query(db, inst);
            result = is_goal_query_satisfiable(db, self, &query);
        }
        match result {
            GoalSatisfiability::Satisfied(solution) => {
                let solution = if solution.value.chosen.is_some() {
                    let mut table = super::unify::UnificationTable::new(db);
                    query.extract_solution(&mut table, solution)
                } else {
                    solution.value
                };
                Selection::Unique(solution)
            }
            GoalSatisfiability::NeedsConfirmation { solutions, .. } => {
                Selection::Ambiguous(solutions.iter().map(|s| s.value).collect())
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

#[salsa::tracked(cycle_initial=is_query_satisfiable_cycle_initial)]
fn is_query_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    origin_ingot: Ingot<'db>,
    tier: Tier,
    query: Canonical<TraitSolverQuery<'db>>,
) -> GoalSatisfiability<'db> {
    if query.flags(db).contains(TyFlags::HAS_INVALID) {
        return GoalSatisfiability::ContainsInvalid;
    };

    solve(db, origin_ingot, tier, query)
}

fn is_query_satisfiable_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _origin_ingot: Ingot<'db>,
    _tier: Tier,
    _query: Canonical<TraitSolverQuery<'db>>,
) -> GoalSatisfiability<'db> {
    // A cycle can arise while collecting an impl whose constraints contain an associated-type
    // projection: resolving the projection needs the trait environment that is currently being
    // assembled for the outer goal. Treat the incomplete pass as ambiguous so callers keep the
    // candidate alive; the next fixpoint iteration can decide it once impl collection converges.
    GoalSatisfiability::NeedsConfirmation {
        solutions: IndexSet::default(),
        completion: TraitSolveCompletion::Cycle,
    }
}

#[salsa::tracked(returns(copy), cycle_initial=query_has_solution_cycle_initial)]
fn query_has_solution<'db>(
    db: &'db dyn HirAnalysisDb,
    origin_ingot: Ingot<'db>,
    tier: Tier,
    query: Canonical<TraitSolverQuery<'db>>,
    target: Canonical<TraitInstId<'db>>,
    relation: TargetSolutionMatch,
) -> TargetSolutionStatus {
    if query.flags(db).contains(TyFlags::HAS_INVALID) {
        return TargetSolutionStatus::NotFound;
    }
    has_solution(db, origin_ingot, tier, query, target, relation)
}

fn query_has_solution_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _origin_ingot: Ingot<'db>,
    _tier: Tier,
    _query: Canonical<TraitSolverQuery<'db>>,
    _target: Canonical<TraitInstId<'db>>,
    _relation: TargetSolutionMatch,
) -> TargetSolutionStatus {
    TargetSolutionStatus::Incomplete
}

/// Whether a search for `target` among the answers of `query` finds it. A search
/// that stops on a pending request finds nothing and leaves the request in
/// `pending`.
pub(crate) fn goal_query_has_solution<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    query: &CanonicalGoalQuery<'db>,
    target: Canonical<TraitInstId<'db>>,
) -> bool {
    match query_has_solution(
        db,
        solve_cx.origin_ingot(),
        solve_cx.tier(),
        query.canonical(),
        target,
        TargetSolutionMatch::Equal,
    ) {
        TargetSolutionStatus::Found => true,
        TargetSolutionStatus::NotFound | TargetSolutionStatus::Incomplete => false,
    }
}

/// Whether the answers of `query` contain nothing but `target`. A search that
/// stops on a pending request proves nothing and leaves the request in
/// `pending`.
pub(crate) fn goal_query_has_no_distinct_solution<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    query: &CanonicalGoalQuery<'db>,
    target: Canonical<TraitInstId<'db>>,
) -> bool {
    match query_has_solution(
        db,
        solve_cx.origin_ingot(),
        solve_cx.tier(),
        query.canonical(),
        target,
        TargetSolutionMatch::NotEqual,
    ) {
        TargetSolutionStatus::NotFound => true,
        TargetSolutionStatus::Found | TargetSolutionStatus::Incomplete => false,
    }
}

pub fn is_goal_query_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    query: &CanonicalGoalQuery<'db>,
) -> GoalSatisfiability<'db> {
    is_query_satisfiable(
        db,
        solve_cx.origin_ingot(),
        solve_cx.tier(),
        query.canonical(),
    )
    .clone()
}

pub fn is_goal_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> GoalSatisfiability<'db> {
    let query = solve_cx.query(db, goal);
    is_goal_query_satisfiable(db, solve_cx, &query)
}

/// Whether `goal` holds. Answers to a goal without inference variables are
/// proofs of the one instance, so several of them, as an impl and an
/// assumption restating its bound give, still decide it; with variables they
/// may be different instances.
pub fn goal_holds<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> bool {
    match is_goal_satisfiable(db, solve_cx, goal) {
        GoalSatisfiability::Satisfied(_) => true,
        GoalSatisfiability::NeedsConfirmation { solutions, .. } => {
            !solutions.is_empty() && !goal.args(db).iter().any(|ty| ty.has_var(db))
        }
        GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_) => false,
    }
}

/// Checks if the given type is well-formed, i.e., the arguments of the given
/// type applications satisfies the constraints under the given assumptions.
#[salsa::tracked(returns(copy))]
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
            let wf = check_projected_trait_use_wf(db, solve_cx, assoc.trait_.as_predicate(db));
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
        TyData::ConstraintTerm(goal) => {
            for &arg in goal
                .args(db)
                .iter()
                .chain(goal.assoc_type_bindings(db).values())
            {
                let wf = check_ty_wf(db, solve_cx, arg);
                if !wf.is_wf() {
                    return wf;
                }
            }
            // The arguments satisfy the requirements of the trait that do
            // not mention the subject.
            let wf = trait_inst_wf(db, solve_cx, *goal, false);
            if !wf.is_wf() {
                return wf;
            }
        }
        TyData::TraitFamily(family) => {
            for &arg in family
                .arguments(db)
                .iter()
                .chain(family.associated(db).values())
            {
                let wf = check_ty_wf(db, solve_cx, arg);
                if !wf.is_wf() {
                    return wf;
                }
            }
            // The family's goal, for a placeholder subject, is well formed
            // apart from the requirements that mention the subject.
            let subject = family.def(db).self_param(db);
            if let TyData::ConstraintTerm(goal) = family.apply(db, subject).data(db) {
                let wf = trait_inst_wf(db, solve_cx, *goal, false);
                if !wf.is_wf() {
                    return wf;
                }
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
        let query = solve_cx.query(db, goal);

        match is_goal_query_satisfiable(db, solve_cx, &query) {
            GoalSatisfiability::UnSat(subgoal) => {
                let subgoal = subgoal.map(|subgoal| query.extract_subgoal(&mut table, subgoal));
                return WellFormedness::IllFormed { goal, subgoal };
            }
            GoalSatisfiability::Satisfied(_)
            | GoalSatisfiability::NeedsConfirmation { .. }
            | GoalSatisfiability::ContainsInvalid => {}
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
        ConstTyData::Computation { description, .. } => {
            let evaluated = const_ty.evaluate(db, Some(description.ty()));
            if evaluated != const_ty {
                return check_ty_wf(db, solve_cx, TyId::const_ty(db, evaluated));
            }
            if let ConstRepr::Term(term) = description.repr() {
                return check_ty_wf(db, solve_cx, TyId::const_ty(db, *term));
            }
        }
        ConstTyData::Value(value) => return check_sem_const_wf(db, solve_cx, value.value()),
        ConstTyData::Description(value) => return check_sem_const_wf(db, solve_cx, *value),
        ConstTyData::Abstract(expr, _) => {
            let wf = check_const_expr_wf(db, solve_cx, *expr);
            if !wf.is_wf() {
                return wf;
            }
        }
        ConstTyData::TyVar(..)
        | ConstTyData::TyParam(..)
        | ConstTyData::Invalid(..)
        | ConstTyData::UnEvaluated { .. } => {}
    }

    WellFormedness::WellFormed
}

fn check_sem_const_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    value: SemConstId<'db>,
) -> WellFormedness<'db> {
    match value.value(db) {
        SemConstValue::ItemCode { ty, .. } | SemConstValue::Meta { ty, .. } => {
            check_ty_wf(db, solve_cx, *ty)
        }
        SemConstValue::Description(term) => check_ty_wf(db, solve_cx, TyId::const_ty(db, *term)),
        SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => {
            for child in elems.iter().copied() {
                let wf = check_ty_wf(db, solve_cx, sem_const_ty(db, child));
                if !wf.is_wf() {
                    return wf;
                }
                let wf = check_sem_const_wf(db, solve_cx, child);
                if !wf.is_wf() {
                    return wf;
                }
            }
            WellFormedness::WellFormed
        }
        SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => {
            for child in fields.iter().copied() {
                let wf = check_ty_wf(db, solve_cx, sem_const_ty(db, child));
                if !wf.is_wf() {
                    return wf;
                }
                let wf = check_sem_const_wf(db, solve_cx, child);
                if !wf.is_wf() {
                    return wf;
                }
            }
            WellFormedness::WellFormed
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

    match expr.data(db) {
        ConstExpr::Invocation(invocation) => {
            let mut collector = TyCollector {
                db,
                tys: Vec::new(),
            };
            invocation.key.visit_with(&mut collector);
            invocation.args.visit_with(&mut collector);
            for ty in collector.tys {
                let wf = check_ty_wf(db, solve_cx, ty);
                if !wf.is_wf() {
                    return wf;
                }
            }
        }
        ConstExpr::ArithBinOp { lhs, rhs, .. }
        | ConstExpr::Compare { lhs, rhs, .. }
        | ConstExpr::ArrayRepeat {
            value: lhs,
            len: rhs,
        }
        | ConstExpr::ArrayIndex {
            array: lhs,
            index: rhs,
        } => {
            for ty in [*lhs, *rhs] {
                let wf = check_ty_wf(db, solve_cx, ty);
                if !wf.is_wf() {
                    return wf;
                }
            }
        }
        ConstExpr::Select {
            cond,
            then,
            otherwise,
        } => {
            for ty in [*cond, *then, *otherwise] {
                let wf = check_ty_wf(db, solve_cx, ty);
                if !wf.is_wf() {
                    return wf;
                }
            }
        }
        ConstExpr::UnOp { expr, .. } | ConstExpr::Field { value: expr, .. } => {
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
        ConstExpr::InherentConst(use_) => {
            let wf = check_ty_wf(db, solve_cx, use_.receiver_ty());
            if !wf.is_wf() {
                return wf;
            }
        }
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
    let mut table = UnificationTable::new(db);
    let query = solve_cx.query(db, goal);
    match is_goal_query_satisfiable(db, solve_cx, &query) {
        GoalSatisfiability::UnSat(subgoal) => {
            let subgoal = subgoal.map(|subgoal| query.extract_subgoal(&mut table, subgoal));
            Some(WellFormedness::IllFormed { goal, subgoal })
        }
        GoalSatisfiability::Satisfied(_)
        | GoalSatisfiability::NeedsConfirmation { .. }
        | GoalSatisfiability::ContainsInvalid => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, SalsaValue)]
pub(crate) enum WellFormedness<'db> {
    WellFormed,
    IllFormed {
        goal: TraitInstId<'db>,
        subgoal: Option<TraitInstId<'db>>,
    },
}

impl WellFormedness<'_> {
    pub(crate) fn is_wf(self) -> bool {
        matches!(self, WellFormedness::WellFormed)
    }
}

/// Checks if the given trait instance are well-formed, i.e., the arguments of
/// the trait satisfies all constraints under the given assumptions.
#[salsa::tracked(returns(copy))]
pub(crate) fn check_trait_inst_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    trait_inst: TraitInstId<'db>,
) -> WellFormedness<'db> {
    trait_inst_wf(db, solve_cx, trait_inst, true)
}

/// [`check_trait_inst_wf`] for a trait reference whose `Self` stands for a
/// type not known where it is written, as an associated type's bound or an
/// effect key's trait: the trait's requirements that mention that `Self`,
/// once instantiated, fall to what provides it.
pub(crate) fn check_trait_inst_wf_without_self<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    trait_inst: TraitInstId<'db>,
) -> WellFormedness<'db> {
    trait_inst_wf(db, solve_cx, trait_inst, false)
}

fn trait_inst_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    trait_inst: TraitInstId<'db>,
    with_self: bool,
) -> WellFormedness<'db> {
    let args = &trait_inst.args(db)[usize::from(!with_self)..];
    for &ty in args
        .iter()
        .chain(trait_inst.assoc_type_bindings(db).values())
    {
        let wf = check_ty_wf(db, solve_cx, ty);
        if !wf.is_wf() {
            return wf;
        }
    }

    let constraints =
        collect_constraints(db, trait_inst.def(db).into()).instantiate(db, trait_inst.args(db));
    let self_ty = trait_inst.self_ty(db);
    let assumptions = solve_cx.assumptions();

    // Normalize constraints after instantiation to resolve associated types
    let normalized_constraints = {
        let scope = solve_cx.normalization_scope_for_trait_inst(db, trait_inst);
        let normalized_list: Vec<_> = constraints
            .list(db)
            .iter()
            .filter(|&&goal| with_self || !mentions_ty(db, goal, self_ty))
            .map(|&goal| goal.normalize(db, scope, assumptions))
            .collect();
        PredicateListId::new(db, normalized_list)
    };

    for &goal in normalized_constraints.list(db) {
        let mut table = UnificationTable::new(db);
        let query = solve_cx.query(db, goal);
        match is_goal_query_satisfiable(db, solve_cx, &query) {
            GoalSatisfiability::UnSat(subgoal) => {
                let subgoal = subgoal.map(|subgoal| query.extract_subgoal(&mut table, subgoal));
                return WellFormedness::IllFormed { goal, subgoal };
            }
            GoalSatisfiability::Satisfied(_)
            | GoalSatisfiability::NeedsConfirmation { .. }
            | GoalSatisfiability::ContainsInvalid => {}
        }
    }

    WellFormedness::WellFormed
}

/// Whether `value` mentions `ty`, in a projection or constant as well.
pub(crate) fn mentions_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    value: impl TyVisitable<'db>,
    ty: TyId<'db>,
) -> bool {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        found: bool,
    }
    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if ty == self.ty {
                self.found = true;
            } else if !self.found {
                walk_ty(self, ty);
            }
        }
    }
    let mut finder = Finder {
        db,
        ty,
        found: false,
    };
    value.visit_with(&mut finder);
    finder.found
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct TraitGoalSolution<'db> {
    pub(crate) inst: TraitInstId<'db>,
    pub(crate) implementor: ImplementorId<'db>,
    /// The application of an opening selected before proving its conditions.
    pub(super) chosen: Option<ImplementorId<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
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
    /// A preferred header would decide an inference argument prematurely.
    UnresolvedChoice,
    /// Salsa is iterating a recursive query to a fixpoint.
    Cycle,
}

impl TraitSolveCompletion {
    pub fn is_saturated(self) -> bool {
        matches!(self, Self::Saturated)
    }

    pub fn hit_root_answer_limit(self) -> bool {
        matches!(self, Self::RootAnswerLimit { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
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

impl GoalSatisfiability<'_> {
    pub fn is_satisfied(&self) -> bool {
        matches!(
            self,
            Self::Satisfied(_) | Self::NeedsConfirmation { .. } | Self::ContainsInvalid
        )
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct PredicateListId<'db> {
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

    /// The predicates of an item used at `mode`: its `mut?` bounds are
    /// `mut` at `mut`, and plain otherwise.
    pub fn at_mode(self, db: &'db dyn HirAnalysisDb, mode: Option<BorrowKind>) -> Self {
        if self
            .list(db)
            .iter()
            .all(|pred| pred.mode(db) != ModeMarker::MaybeMut)
        {
            return self;
        }
        Self::new(
            db,
            self.list(db)
                .iter()
                .map(|pred| pred.at_mode(db, mode))
                .collect::<Vec<_>>(),
        )
    }

    pub fn is_empty(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.list(db).is_empty()
    }

    /// Transitively extends the predicate list with all implied bounds:
    /// - Super trait bounds
    /// - Associated type bounds from trait definitions
    pub fn extend_all_bounds(self, db: &'db dyn HirAnalysisDb) -> Self {
        self.bound_walk(db).list
    }

    /// As `extend_all_bounds`, with the derivations the walk followed.
    pub(crate) fn bound_walk(self, db: &'db dyn HirAnalysisDb) -> &'db BoundWalk<'db> {
        bound_walk_query(db, self)
    }

    /// As `extend_all_bounds`, but a bound an equality makes concrete is kept,
    /// with the derivations the walk followed. It is evidence an implementation
    /// proves, never an assumption, so only the projection of an implementation's
    /// own components reads it.
    fn bound_walk_keeping_concrete(self, db: &'db dyn HirAnalysisDb) -> &'db BoundWalk<'db> {
        component_walk::bound_walk_keeping_concrete_query(db, self)
    }

    /// The derivations among the members of this list, as positions in it:
    /// the walk of the list records each bound's derivation from the bound
    /// whose trait declares it. A list that is not its own extension (a slot
    /// layout that is a prefix, or a concatenation) keeps the edges whose two
    /// ends it holds; an edge names the first position of an equal member.
    pub(crate) fn implied_edges(self, db: &'db dyn HirAnalysisDb) -> &'db [BoundEdge<'db>] {
        implied_edges_query(db, self)
    }

    fn walk_bounds(
        self,
        db: &'db dyn HirAnalysisDb,
        components: Option<component_walk::Components>,
    ) -> BoundWalk<'db> {
        let keep_concrete = components.is_some();
        let mut all_predicates: IndexSet<TraitInstId<'db>> =
            self.list(db).iter().copied().collect();
        let mut edges: Vec<BoundEdge<'db>> = Vec::new();
        let mut truncated: Vec<u32> = Vec::new();
        let mut capped = false;
        let slots = all_predicates.len();

        // Each entry says how many associated types the predicate lies below the
        // first concrete bound that derived it: none for a bound no concrete
        // bound derives, and a concrete bound's own bounds one more. A kept
        // concrete bound is expanded through associated types to a depth limit.
        // The plain walk meets no concrete bound, and takes the entries last
        // first, as it always has; the walk that keeps them takes them first
        // first, so that a bound is first met by the shortest derivation.
        let mut worklist: std::collections::VecDeque<(TraitInstId<'db>, u32)> =
            self.list(db).iter().map(|&pred| (pred, 0)).collect();

        while let Some((pred, depth)) = if keep_concrete {
            worklist.pop_front()
        } else {
            worklist.pop_back()
        } {
            let below_concrete = keep_concrete && is_concrete_bound(db, pred);
            let hir_trait = pred.def(db);
            let parent = all_predicates
                .get_index_of(&pred)
                .and_then(|index| u32::try_from(index).ok())
                .expect("a predicate on the worklist is in the list");
            // The plain walk reads no implementation. Only the walk that keeps
            // concrete bounds holds the value that lets a step below a concrete
            // member do so.
            let parent_is_member = parent as usize >= slots;
            let derive = |step| match &components {
                None => step_bound(db, pred, step),
                Some(components) => components.child(db, pred, parent_is_member, step),
            };
            let mut record = |child: usize, step: BoundStep<'db>| {
                let edge = BoundEdge {
                    parent,
                    child: u32::try_from(child).expect("predicate position"),
                    step,
                };
                if !edges.contains(&edge) {
                    edges.push(edge);
                }
            };
            // 1. Collect super traits
            for index in 0..hir_trait.super_traits(db).len() {
                let step = BoundStep::Super(index as u32);
                let Some(inst) = derive(step) else {
                    continue;
                };
                if predicate_has_recursive_assoc_projection(db, inst) {
                    continue;
                }
                let (child, inserted) = all_predicates.insert_full(inst);
                record(child, step);
                if inserted {
                    // New predicate added, add to worklist for further processing
                    worklist.push_back((inst, depth));
                }
            }

            // 2. Collect associated type bounds
            let formal_trait =
                TraitInstId::new_simple(db, hir_trait, hir_trait.params(db).to_vec());
            if below_concrete && depth >= COMPONENT_DEPTH_LIMIT {
                // The bounds below this one are derived as below, but one that
                // is not a member yet is not added. A member is cut exactly
                // when following it would add a member: a program whose
                // dictionaries hold a named implementation is refused for it.
                // A member whose bounds lead only to members already present
                // is followed completely, and its derivations are recorded.
                let mut cut = false;
                for trait_type in hir_trait.assoc_types(db) {
                    let Some(assoc) = trait_type.name(db) else {
                        continue;
                    };
                    let assoc_ty = TyId::assoc_ty(db, formal_trait.trait_ref(db), assoc);
                    for index in 0..assoc_ty.assoc_type_bounds(db, trait_type).count() {
                        let step = BoundStep::AssocBound {
                            assoc,
                            index: index as u32,
                        };
                        let Some(trait_inst) = derive(step) else {
                            continue;
                        };
                        if predicate_has_recursive_assoc_projection(db, trait_inst) {
                            continue;
                        }
                        match all_predicates.get_index_of(&trait_inst) {
                            Some(child) => record(child, step),
                            None => cut = true,
                        }
                    }
                }
                if cut {
                    truncated.push(parent);
                }
                continue;
            }
            for trait_type in hir_trait.assoc_types(db) {
                // Get the associated type name
                let Some(assoc) = trait_type.name(db) else {
                    continue;
                };
                let assoc_ty = TyId::assoc_ty(db, formal_trait.trait_ref(db), assoc);
                for index in 0..assoc_ty.assoc_type_bounds(db, trait_type).count() {
                    let step = BoundStep::AssocBound {
                        assoc,
                        index: index as u32,
                    };
                    let Some(trait_inst) = derive(step) else {
                        continue;
                    };
                    // An equality can make the bound a concrete one,
                    // `[u256; 3]: Collection` from `H: Holds<Base = [u256; 3]>`.
                    // An implementation proves that, so it is not assumed: as
                    // a written bound on a concrete type would be (6-0005), it
                    // would be a second candidate for every method of the impl.
                    // A bound still generic in an argument or a binding,
                    // `Reader: Read<T>`, is evidence no implementation gives.
                    let concrete = is_concrete_bound(db, trait_inst);
                    if (concrete && !keep_concrete)
                        || predicate_has_recursive_assoc_projection(db, trait_inst)
                    {
                        continue;
                    }
                    // A kept concrete bound is extended through its associated
                    // types to a depth, `Item: Pick` from `Item: Middle`, which
                    // ends the walk where a concrete bound names a type that
                    // grows with each step.
                    let (child, inserted) = all_predicates.insert_full(trait_inst);
                    record(child, step);
                    if inserted {
                        let below = if below_concrete { depth + 1 } else { 0 };
                        worklist.push_back((trait_inst, below));
                        if keep_concrete && all_predicates.len() - slots > COMPONENT_MEMBER_LIMIT {
                            // Every bound still waiting is left unfollowed.
                            capped = true;
                            worklist.clear();
                        }
                    }
                }
            }
        }

        BoundWalk {
            list: Self::new(db, all_predicates.into_iter().collect::<Vec<_>>()),
            edges,
            truncated,
            capped,
        }
    }
}

/// Whether `bound` names no type parameter, projection or invalid type, in an
/// argument or an associated-type binding. An equality can make a bound that
/// an associated type declares concrete: `Foo: Show` from `B: Tr<A = Foo>`.
fn is_concrete_bound<'db>(db: &'db dyn HirAnalysisDb, bound: TraitInstId<'db>) -> bool {
    let concrete =
        |ty: &TyId<'db>| !ty.has_invalid(db) && !ty.has_param(db) && !ty.has_projection(db);
    bound.args(db).iter().all(concrete) && bound.assoc_type_bindings(db).values().all(concrete)
}

/// Whether the bounds in `slots` can derive a concrete bound. A concrete
/// child of a slot needs an associated type that the parent's own equalities
/// make concrete, so a list in which no bound states an equality has none.
pub(in crate::analysis::ty) fn has_concrete_components<'db>(
    db: &'db dyn HirAnalysisDb,
    slots: PredicateListId<'db>,
) -> bool {
    slots
        .list(db)
        .iter()
        .any(|pred| !pred.assoc_type_bindings(db).is_empty())
}

/// A member of the walk of a list of slots that is not one of the slots.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub(in crate::analysis::ty) struct Component<'db> {
    pub(crate) goal: TraitInstId<'db>,
    /// Whether the goal is concrete. Other members only carry a derivation
    /// that continues through them.
    pub(crate) concrete: bool,
    /// The position of the slot the member's first derivation starts from,
    /// and the steps from that slot's dictionary to the member's.
    pub(crate) root: u32,
    pub(crate) path: Vec<BoundStep<'db>>,
    /// Every derivation that enters the member, as the position in the walk
    /// of the bound it is read from and the step that reads it.
    pub(crate) entering: Vec<(u32, BoundStep<'db>)>,
    /// Whether the walk did not follow the associated-type bounds of the
    /// member, at its depth limit.
    pub(crate) truncated: bool,
}

/// The concrete bounds a list of slots derives and are not slots, with where
/// each is read from. A body proves a goal on a concrete type that one of its
/// bounds implies by the component of that bound's dictionary (the
/// projection commit's rule, applied through the steps that name it), never
/// by selecting an implementation.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub(in crate::analysis::ty) struct ComponentTable<'db> {
    /// The number of slots; the walk position of `members[0]`.
    pub(crate) slots: u32,
    /// The members of the walk of the slots after the slots, in walk order.
    pub(crate) members: Vec<Component<'db>>,
    /// The concrete members of each trait, as positions in `members`.
    by_trait: Vec<(crate::hir_def::Trait<'db>, Vec<u32>)>,
    /// Whether the walk reached its member limit. The bounds it had not yet
    /// followed were dropped without a record, so no member is known to be
    /// complete: the whole table counts as truncated.
    pub(crate) capped: bool,
}

impl<'db> ComponentTable<'db> {
    pub(crate) fn empty() -> Self {
        Self {
            slots: 0,
            members: Vec::new(),
            by_trait: Vec::new(),
            capped: false,
        }
    }

    /// The concrete members that a path reaches, in walk order: the ones a
    /// reference can name.
    pub(crate) fn components(&self) -> impl Iterator<Item = &Component<'db>> {
        self.members
            .iter()
            .filter(|member| member.concrete && !member.path.is_empty())
    }

    /// The concrete members of `trait_`, in walk order.
    pub(crate) fn of_trait(
        &self,
        trait_: crate::hir_def::Trait<'db>,
    ) -> impl Iterator<Item = (usize, &Component<'db>)> {
        self.by_trait
            .iter()
            .find(|(candidate, _)| *candidate == trait_)
            .into_iter()
            .flat_map(|(_, members)| members.iter())
            .map(|&member| (member as usize, &self.members[member as usize]))
    }

    /// The canonical member that is the same implementation as `goal`: the
    /// first, in walk order, of the concrete members of its trait that
    /// `same_identity` equates with it, however the associated-type equalities
    /// of the two are spelled. Every reader of a component finds it here, so
    /// that one identity has one reference.
    pub(crate) fn answering(
        &self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        goal: TraitInstId<'db>,
    ) -> Option<(usize, &Component<'db>)> {
        self.of_trait(goal.def(db))
            .find(|(_, member)| same_identity(db, scope, member.goal, goal))
    }
}

/// Whether two goals are one implementation's: the same trait at the same
/// arguments, after normalizing them in `scope`. Associated-type equalities and
/// the mode are not part of it, since a proof names no goal (`Evidence::Impl`).
pub(crate) fn same_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    a: TraitInstId<'db>,
    b: TraitInstId<'db>,
) -> bool {
    a == b
        || (a.def(db) == b.def(db)
            && a.args(db).len() == b.args(db).len()
            && a.args(db).iter().zip(b.args(db)).all(|(&x, &y)| {
                let empty = PredicateListId::empty_list(db);
                x == y || normalize_ty(db, x, scope, empty) == normalize_ty(db, y, scope, empty)
            }))
}

/// The components of the extended list of slots `slots`: one walk of the whole
/// list that keeps the concrete bounds the plain walk drops (`Foo: Show` from
/// `B: Tr<A = Foo>`), with each member's canonical address, the slot and steps
/// of its first derivation. Empty unless a slot states an equality.
#[salsa::tracked(returns(ref), cycle_initial = component_table_cycle_initial)]
pub(in crate::analysis::ty) fn concrete_components<'db>(
    db: &'db dyn HirAnalysisDb,
    slots: PredicateListId<'db>,
) -> ComponentTable<'db> {
    if !has_concrete_components(db, slots) {
        return ComponentTable::empty();
    }
    let walk = slots.bound_walk_keeping_concrete(db);
    let (slot_list, list) = (slots.list(db), walk.list.list(db));
    // The walk's list starts with the list it was given, so a position below
    // the number of slots is that slot.
    if !list.starts_with(slot_list) {
        return ComponentTable::empty();
    }
    let n = slot_list.len();
    let mut entering: Vec<Vec<(u32, BoundStep<'db>)>> = vec![Vec::new(); list.len()];
    for edge in &walk.edges {
        if let Some(into) = entering.get_mut(edge.child as usize) {
            into.push((edge.parent, edge.step));
        }
    }
    let mut members: Vec<Component<'db>> = Vec::with_capacity(list.len() - n);
    for (position, &goal) in list.iter().enumerate().skip(n) {
        let entering = std::mem::take(&mut entering[position]);
        // The first derivation is the one that found the member, so its
        // parent comes earlier in the walk.
        let (root, path) = match entering.first() {
            Some(&(parent, step)) if (parent as usize) < n => (parent, vec![step]),
            Some(&(parent, step)) if (parent as usize) < position => {
                let from = &members[parent as usize - n];
                let mut path = from.path.clone();
                path.push(step);
                (from.root, path)
            }
            // The walk records a member's first derivation when it inserts
            // the member, from a bound already in the list.
            _ => {
                debug_assert!(
                    false,
                    "the walk's member at position {position} has no derivation from an earlier bound"
                );
                (0, Vec::new())
            }
        };
        members.push(Component {
            goal,
            concrete: is_concrete_bound(db, goal),
            root,
            path,
            entering,
            truncated: walk.truncated.contains(&(position as u32)),
        });
    }
    let mut by_trait: Vec<(crate::hir_def::Trait<'db>, Vec<u32>)> = Vec::new();
    for (position, member) in members.iter().enumerate() {
        if !member.concrete || member.path.is_empty() {
            continue;
        }
        let trait_ = member.goal.def(db);
        match by_trait
            .iter_mut()
            .find(|(candidate, _)| *candidate == trait_)
        {
            Some((_, positions)) => positions.push(position as u32),
            None => by_trait.push((trait_, vec![position as u32])),
        }
    }
    ComponentTable {
        slots: n as u32,
        members,
        by_trait,
        capped: walk.capped,
    }
}

fn component_table_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _slots: PredicateListId<'db>,
) -> ComponentTable<'db> {
    ComponentTable::empty()
}

/// One derivation step of the bound walk: how a bound is read from the
/// dictionary of the bound that implies it. The coordinates are those of the
/// parent predicate's trait, so a step names the same component of every
/// dictionary of that trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) enum BoundStep<'db> {
    /// The supertrait at this position among the parent trait's supertraits.
    Super(u32),
    /// The bound at `index` among those declared on the associated type
    /// `assoc` of the parent trait.
    AssocBound { assoc: IdentId<'db>, index: u32 },
}

/// A derivation recorded by the walk, in positions of the extended list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) struct BoundEdge<'db> {
    pub(crate) parent: u32,
    pub(crate) child: u32,
    pub(crate) step: BoundStep<'db>,
}

/// An extended predicate list with the derivations that produced it. Every
/// derivation is recorded, including one whose child already existed.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) struct BoundWalk<'db> {
    pub(crate) list: PredicateListId<'db>,
    pub(crate) edges: Vec<BoundEdge<'db>>,
    /// The positions in `list` of the bounds whose associated-type bounds the
    /// walk that keeps concrete bounds did not follow, at its depth limit.
    pub(crate) truncated: Vec<u32>,
    /// Whether the walk stopped at its member limit, which leaves every bound
    /// that had not yet been followed unfollowed.
    pub(crate) capped: bool,
}

impl<'db> BoundWalk<'db> {
    /// The steps from the first predicate to the member at `target`, along
    /// the shortest chain of recorded derivations.
    pub(crate) fn path_to(
        &self,
        db: &'db dyn HirAnalysisDb,
        target: usize,
    ) -> Option<Vec<BoundStep<'db>>> {
        let len = self.list.list(db).len();
        let mut via: Vec<Option<(usize, BoundStep<'db>)>> = vec![None; len];
        let mut seen = vec![false; len];
        let mut queue = std::collections::VecDeque::new();
        *seen.first_mut()? = true;
        queue.push_back(0usize);
        while let Some(at) = queue.pop_front() {
            if at == target {
                break;
            }
            for edge in self.edges.iter().filter(|edge| edge.parent as usize == at) {
                let child = edge.child as usize;
                if child < len && !seen[child] {
                    seen[child] = true;
                    via[child] = Some((at, edge.step));
                    queue.push_back(child);
                }
            }
        }
        if !*seen.get(target)? {
            return None;
        }
        let mut steps = Vec::new();
        let mut at = target;
        while let Some((parent, step)) = via[at] {
            steps.push(step);
            at = parent;
        }
        steps.reverse();
        Some(steps)
    }
}

/// The bound that `step` derives from `pred`, in the coordinates of `pred`'s
/// arguments, normalized by `pred`'s own equalities. The walk and every
/// projection read a derivation through this one computation. A bound that an
/// equality makes concrete is returned as it is: dropping it is the walk's
/// decision. Reads no implementation: header lowering reaches it.
pub(crate) fn step_bound<'db>(
    db: &'db dyn HirAnalysisDb,
    pred: TraitInstId<'db>,
    step: BoundStep<'db>,
) -> Option<TraitInstId<'db>> {
    let hir_trait = pred.def(db);
    let evidence = PredicateListId::new(db, vec![pred]);
    match step {
        BoundStep::Super(index) => {
            let super_trait = hir_trait
                .super_traits(db)
                .get_index(index as usize)
                .copied()?;
            Some(normalize_from_assumptions(
                db,
                super_trait.instantiate(db, pred.args(db)),
                hir_trait.scope(),
                evidence,
            ))
        }
        BoundStep::AssocBound { assoc, index } => {
            let trait_type = hir_trait
                .assoc_types(db)
                .find(|trait_type| trait_type.name(db) == Some(assoc))?;
            // Keep the entire bound in declaration coordinates until every
            // formal, including non-Self trait parameters, is instantiated.
            let formal_trait =
                TraitInstId::new_simple(db, hir_trait, hir_trait.params(db).to_vec());
            let assoc_ty = TyId::assoc_ty(db, formal_trait.trait_ref(db), assoc);
            let bound = assoc_ty
                .assoc_type_bounds(db, trait_type)
                .nth(index as usize)?;
            Some(normalize_from_assumptions(
                db,
                Binder::bind(hir_trait.into(), bound).instantiate(db, pred.args(db)),
                hir_trait.scope(),
                evidence,
            ))
        }
    }
}

// Normalizing implied bounds can resolve traits under assumptions that lead
// back here. Iterate from the unextended list; extension only adds bounds.
#[salsa::tracked(returns(ref), cycle_initial = bound_walk_cycle_initial)]
fn bound_walk_query<'db>(
    db: &'db dyn HirAnalysisDb,
    predicates: PredicateListId<'db>,
) -> BoundWalk<'db> {
    predicates.walk_bounds(db, None)
}

fn bound_walk_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    predicates: PredicateListId<'db>,
) -> BoundWalk<'db> {
    BoundWalk {
        list: predicates,
        edges: Vec::new(),
        truncated: Vec::new(),
        capped: false,
    }
}

#[salsa::tracked(returns(ref))]
fn implied_edges_query<'db>(
    db: &'db dyn HirAnalysisDb,
    layout: PredicateListId<'db>,
) -> Vec<BoundEdge<'db>> {
    let walk = layout.bound_walk(db);
    if walk.list == layout {
        return walk.edges.clone();
    }
    let members = walk.list.list(db);
    let position = |at: u32| {
        let member = members.get(at as usize)?;
        let at = layout.list(db).iter().position(|slot| slot == member)?;
        u32::try_from(at).ok()
    };
    walk.edges
        .iter()
        .filter_map(|edge| {
            Some(BoundEdge {
                parent: position(edge.parent)?,
                child: position(edge.child)?,
                step: edge.step,
            })
        })
        .collect()
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
    use crate::analysis::name_resolution::Tier;
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
            crate::hir_def::ModeMarker::None,
        );
        let b = TraitInstId::new(
            &db,
            named_trait(&db, top_mod, "B"),
            vec![TyId::bool(&db)],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
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
            let goal = TraitInstId::new(
                db,
                needs_a,
                vec![TyId::unit(db), ty_param],
                IndexMap::new(),
                crate::hir_def::ModeMarker::None,
            );
            let query = CanonicalGoalQuery::new(db, goal, assumptions);
            let solve_cx =
                TraitSolveCx::new(db, func.scope(), Tier::All).with_assumptions(assumptions);
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
            let goal = TraitInstId::new(
                &db,
                foo,
                vec![self_ty],
                IndexMap::new(),
                crate::hir_def::ModeMarker::None,
            );
            is_goal_satisfiable(
                &db,
                TraitSolveCx::new(&db, top_mod.scope(), Tier::All),
                goal,
            )
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
            let goal = TraitInstId::new(
                &db,
                foo,
                vec![self_ty],
                IndexMap::new(),
                crate::hir_def::ModeMarker::None,
            );
            assert!(matches!(
                is_goal_satisfiable(&db, TraitSolveCx::new(&db, top_mod.scope(), Tier::All), goal),
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

        let deep_goal = TraitInstId::new(
            &db,
            foo,
            vec![deep],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
        );
        assert!(matches!(
            is_goal_satisfiable(
                &db,
                TraitSolveCx::new(&db, top_mod.scope(), Tier::All),
                deep_goal,
            ),
            GoalSatisfiability::Satisfied(_)
        ));

        let unrelated_deep_assumption = TraitInstId::new(
            &db,
            noise,
            vec![subject, deep],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
        );
        let assumptions = PredicateListId::new(&db, vec![unrelated_deep_assumption]);
        let shallow_goal = TraitInstId::new(
            &db,
            foo,
            vec![subject],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
        );
        assert!(matches!(
            is_goal_satisfiable(
                &db,
                TraitSolveCx::new(&db, top_mod.scope(), Tier::All).with_assumptions(assumptions),
                shallow_goal,
            ),
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
        let goal = TraitInstId::new(
            &db,
            grow,
            vec![ty_param],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
        );

        assert!(matches!(
            is_goal_satisfiable(
                &db,
                TraitSolveCx::new(&db, top_mod.scope(), Tier::All).with_assumptions(assumptions),
                goal,
            ),
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
        let solve_cx =
            TraitSolveCx::new(&db, top_mod.scope(), Tier::All).with_assumptions(assumptions);
        let mut table = UnificationTable::new(&db);
        let selected = table.new_var(TyVarSort::General, &Kind::Star);
        let goal = TraitInstId::new(
            &db,
            goal_trait,
            vec![subject, selected],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
        );
        let query = CanonicalGoalQuery::new(&db, goal, assumptions);
        let GoalSatisfiability::Satisfied(solution) =
            is_goal_query_satisfiable(&db, solve_cx, &query)
        else {
            panic!("the second constraint must observe the first constraint's binding");
        };
        let actual = query.extract_solution(&mut table, solution).inst;
        let expected = TraitInstId::new(
            &db,
            goal_trait,
            vec![subject, a],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
        );

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
        let solve_cx =
            TraitSolveCx::new(&db, top_mod.scope(), Tier::All).with_assumptions(assumptions);
        let mut table = UnificationTable::new(&db);
        let self_ty = table.new_var(TyVarSort::General, &Kind::Star);
        let goal = TraitInstId::new(
            &db,
            foo,
            vec![self_ty],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
        );
        let query = CanonicalGoalQuery::new(&db, goal, assumptions);
        let GoalSatisfiability::NeedsConfirmation {
            solutions,
            completion: TraitSolveCompletion::RootAnswerLimit { limit: 2 },
        } = is_goal_query_satisfiable(&db, solve_cx, &query)
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
                    TraitInstId::new(
                        &db,
                        foo,
                        vec![self_ty],
                        IndexMap::new(),
                        crate::hir_def::ModeMarker::None,
                    ),
                )
            })
            .find(|candidate| !returned.contains(candidate))
            .expect("the two-answer cutoff must omit one implementation");

        assert!(goal_query_has_solution(&db, solve_cx, &query, target,));
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
            let goal = TraitInstId::new(
                &db,
                picks,
                vec![self_ty],
                IndexMap::new(),
                crate::hir_def::ModeMarker::None,
            );
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
            let solve =
                TraitSolveCx::new(&db, func.scope(), Tier::All).with_assumptions(assumptions);
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
            crate::hir_def::ModeMarker::None,
        );
        let solve = TraitSolveCx::new(&db, func.scope(), Tier::All)
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
            crate::hir_def::ModeMarker::None,
        );
        let solve = TraitSolveCx::new(&db, func.scope(), Tier::All).with_assumptions(assumptions);
        assert!(matches!(
            solve.select_impl(&db, goal),
            Selection::Ambiguous(_)
        ));
    }

    /// The plain walk is a function of the predicates and the trait
    /// declarations alone: it reads no implementation, so a closed bound's
    /// associated-type bound stays a projection, however an implementation
    /// defines the type.
    #[test]
    fn plain_walk_of_a_closed_bound_reads_no_implementation() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "plain_walk_closed.fe".into(),
            r#"
trait Show {}
trait Hold { type A: Show }
struct Foo {}
impl Show for Foo {}
struct X {}
impl Hold for X { type A = Foo }
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let (x, foo) = (
            named_struct_ty(&db, top_mod, "X"),
            named_struct_ty(&db, top_mod, "Foo"),
        );
        let hold = TraitInstId::new(
            &db,
            named_trait(&db, top_mod, "Hold"),
            vec![x],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
        );
        let show = named_trait(&db, top_mod, "Show");
        let walked = PredicateListId::new(&db, vec![hold]).extend_all_bounds(&db);
        let shows: Vec<_> = walked
            .list(&db)
            .iter()
            .filter(|bound| bound.def(&db) == show)
            .collect();
        assert!(
            shows
                .iter()
                .any(|bound| bound.args(&db)[0].has_projection(&db)),
            "`<X as Hold>::A: Show` is derived"
        );
        assert!(
            !shows.iter().any(|bound| bound.args(&db)[0] == foo),
            "`Foo: Show` needs the implementation of `Hold` for `X`"
        );
    }

    const LIMIT_CHAIN: &str = r#"
pub trait Chain { type Next: Chain }
pub struct End0 {}
impl Chain for End0 { type Next = End0 }
pub struct E {}
impl Chain for E { type Next = End0 }
pub struct L<T> {}
impl<T: Chain> Chain for L<T> { type Next = T }
pub trait Holder { type Out: Chain }
pub struct Bx<T> {}
impl<T: Chain> Holder for Bx<T> { type Out = L<L<L<T>>> }
fn g<H: Holder<Out = L<L<L<E>>>>>() {}
fn h<H: Holder<Out = L<L<L<L<E>>>>>>() {}
"#;

    /// A member at the depth limit is cut exactly when following it would add
    /// a member. `End0: Chain` has `Next = End0`, so its bounds lead only to
    /// itself: it is followed completely, and its derivation from itself is
    /// recorded. In the longer chain the member at the limit is `E: Chain`,
    /// whose `Next` is the new member `End0: Chain`, and the cut is real.
    #[test]
    fn a_member_at_the_limit_whose_bounds_are_members_is_not_cut() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("limit_chain.fe".into(), LIMIT_CHAIN);
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let chain = |name: &str| {
            let signature = crate::analysis::ty::ty_check::func_signature(
                &db,
                find_func(&db, top_mod, name),
                None,
            );
            signature.bound_walk_keeping_concrete(&db)
        };
        let end0 = |walk: &super::BoundWalk<'_>| {
            walk.list
                .list(&db)
                .iter()
                .position(|bound| bound.pretty_print(&db, true) == "End0: Chain")
        };
        let short = chain("g");
        assert!(short.truncated.is_empty() && !short.capped);
        let end0_at = end0(short).expect("`End0: Chain` is a member") as u32;
        assert!(
            short
                .edges
                .iter()
                .any(|edge| edge.parent == end0_at && edge.child == end0_at),
            "`End0: Chain` is derived from itself"
        );
        let long = chain("h");
        let e_at = long
            .list
            .list(&db)
            .iter()
            .position(|bound| bound.pretty_print(&db, true) == "E: Chain")
            .expect("`E: Chain` is a member") as u32;
        assert_eq!(long.truncated, vec![e_at]);
    }
}
