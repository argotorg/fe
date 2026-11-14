//! Evidence: how a trait goal holds.
//!
//! The checker proves each trait goal a body uses once, in the body's
//! context, and records the proof. A proof is an implementation with a proof
//! of each of its conditions, or one of the body's assumptions, named by its
//! position in the body's assumption list.
//!
//! A call also proves the bounds its callee declares. The callee's instance
//! receives those proofs (`SemanticInstanceKey::evidence`) and puts them in
//! place of the assumptions its body's proofs name, so a call the body
//! dispatches through a bound reaches its implementation without selecting
//! it again.

use salsa::SalsaValue;

use super::{
    const_ty::const_body_assumptions,
    fold::{TyFoldable, TyFolder},
    method_cmp::trait_to_impl_param_subst,
    normalize::normalize_ty,
    subst::substitute_complete,
    trait_def::{
        ImplementorId, ImplementorOrigin, ResolvedImplInstance, TraitInstId,
        resolve_trait_impl_instance,
    },
    trait_lower::lower_impl_trait,
    trait_resolution::{
        PredicateListId, Selection, TraitSolveCx, answering_assumption,
        constraint::collect_candidate_constraints, normalize_trait_inst_preserving_validity,
    },
    ty_check::{BodyOwner, func_declared_bounds, func_signature},
    ty_def::{BorrowKind, TyFlags},
    ty_lower::CompleteSubst,
    visitor::{TyVisitable, TyVisitor, collect_flags, mentions_effect_provider},
};
use crate::{
    analysis::{HirAnalysisDb, semantic::GenericSubst},
    hir_def::{Body, CallableDef, Func, ItemKind, scope_graph::ScopeId},
};

#[salsa::interned]
#[derive(Debug)]
pub struct EvidenceId<'db> {
    pub data: Evidence<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub enum Evidence<'db> {
    /// An implementation, `selected` and as `instantiated` for the goal,
    /// with a proof of each of its conditions in the order
    /// `ImplementorId::constraints` lists them. A compiler-provided
    /// implementation has none. The proof names no goal: goals that differ
    /// only in their mode or associated-type equalities share it, and each
    /// use answers its own.
    Impl {
        selected: ImplementorId<'db>,
        instantiated: ImplementorId<'db>,
        conds: Vec<EvidenceId<'db>>,
    },
    /// The assumption at this position in the extended assumption list of
    /// the body the goal was proved in.
    Assumption(u32),
}

impl<'db> EvidenceId<'db> {
    /// The implementation this proof applies, answering `goal`; none for an
    /// assumption.
    pub fn answering(
        self,
        db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
    ) -> Option<ResolvedImplInstance<'db>> {
        match self.data(db) {
            Evidence::Impl {
                selected,
                instantiated,
                ..
            } => Some(ResolvedImplInstance::new(*selected, *instantiated, goal)),
            Evidence::Assumption(_) => None,
        }
    }

    /// This proof with each assumption it names replaced by `bound`'s proof
    /// of it; absent if `bound` has no proof of one.
    fn rebase(self, db: &'db dyn HirAnalysisDb, bound: EvidenceList<'db>) -> Option<Self> {
        Some(match self.data(db) {
            Evidence::Assumption(index) => bound.get(db, *index as usize)?,
            Evidence::Impl {
                selected,
                instantiated,
                conds,
            } => EvidenceId::new(
                db,
                Evidence::Impl {
                    selected: *selected,
                    instantiated: *instantiated,
                    conds: conds
                        .iter()
                        .map(|cond| cond.rebase(db, bound))
                        .collect::<Option<_>>()?,
                },
            ),
        })
    }

    /// Whether this proof names no assumption: it holds in any body.
    pub fn is_closed(self, db: &'db dyn HirAnalysisDb) -> bool {
        evidence_is_closed(db, self)
    }

    /// Whether this tree selects a named implementation at any depth.
    pub fn has_named_impl(self, db: &'db dyn HirAnalysisDb) -> bool {
        evidence_has_named_impl(db, self)
    }

    /// The proofs of an implementation's conditions; none for an assumption.
    pub fn conds(self, db: &'db dyn HirAnalysisDb) -> &'db [EvidenceId<'db>] {
        match self.data(db) {
            Evidence::Impl { conds, .. } => conds,
            Evidence::Assumption(_) => &[],
        }
    }

    /// This proof with each assumption it names replaced by `bound`'s proof
    /// of that assumption, where `bound` has one.
    pub fn bind(self, db: &'db dyn HirAnalysisDb, bound: EvidenceList<'db>) -> Self {
        match self.data(db) {
            Evidence::Assumption(index) => bound.get(db, *index as usize).unwrap_or(self),
            Evidence::Impl {
                selected,
                instantiated,
                conds,
            } => EvidenceId::new(
                db,
                Evidence::Impl {
                    selected: *selected,
                    instantiated: *instantiated,
                    conds: conds.iter().map(|cond| cond.bind(db, bound)).collect(),
                },
            ),
        }
    }
}

/// Proofs form a shared DAG. Cache this property so repeated tuple/condition
/// children do not turn ground-key construction into an exponential walk.
#[salsa::tracked(returns(copy))]
fn evidence_has_named_impl<'db>(db: &'db dyn HirAnalysisDb, evidence: EvidenceId<'db>) -> bool {
    match evidence.data(db) {
        Evidence::Impl {
            selected, conds, ..
        } => {
            matches!(selected.origin(db), ImplementorOrigin::Hir(implementation) if implementation.selection_name(db).is_some())
                || conds.iter().any(|cond| cond.has_named_impl(db))
        }
        Evidence::Assumption(_) => false,
    }
}

#[salsa::tracked(returns(copy))]
fn evidence_is_closed<'db>(db: &'db dyn HirAnalysisDb, evidence: EvidenceId<'db>) -> bool {
    match evidence.data(db) {
        Evidence::Assumption(_) => false,
        Evidence::Impl { conds, .. } => conds.iter().all(|cond| cond.is_closed(db)),
    }
}

/// Proofs of the assumptions a body is checked under, one per entry of its
/// signature (`TypedBody::signature`), each absent where the body proves
/// that assumption itself.
#[salsa::interned]
#[derive(Debug)]
pub struct EvidenceList<'db> {
    pub entries: Vec<Option<EvidenceId<'db>>>,
}

impl<'db> EvidenceList<'db> {
    /// A list that supplies no proofs.
    pub fn none(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new())
    }

    /// The list of `entries`, without the absent proofs that end it, so
    /// lists that supply the same proofs are equal.
    pub fn of(db: &'db dyn HirAnalysisDb, mut entries: Vec<Option<EvidenceId<'db>>>) -> Self {
        while entries.last().is_some_and(Option::is_none) {
            entries.pop();
        }
        Self::new(db, entries)
    }

    /// The proof of the assumption at `index`, if the list supplies one.
    pub fn get(self, db: &'db dyn HirAnalysisDb, index: usize) -> Option<EvidenceId<'db>> {
        self.entries(db).get(index).copied().flatten()
    }

    /// Missing dictionaries in a settled signature. Still-schematic effect
    /// providers and unresolved goals belong to their existing checker paths.
    pub(crate) fn missing_bound_goals(
        self,
        db: &'db dyn HirAnalysisDb,
        signature: PredicateListId<'db>,
        subst: &CompleteSubst<'db>,
    ) -> Vec<TraitInstId<'db>> {
        signature
            .list(db)
            .iter()
            .enumerate()
            .filter_map(|(index, &bound)| {
                if self.get(db, index).is_some() {
                    return None;
                }
                let goal = substitute_complete(db, bound, subst).ok()?;
                if mentions_effect_provider(db, goal) {
                    return None;
                }
                (!collect_flags(db, goal).intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID))
                    .then_some(goal)
            })
            .collect()
    }

    /// Each proof with the assumptions it names replaced by `bound`'s proofs.
    pub fn bind(self, db: &'db dyn HirAnalysisDb, bound: EvidenceList<'db>) -> Self {
        if bound.entries(db).is_empty() {
            return self;
        }
        Self::new(
            db,
            self.entries(db)
                .iter()
                .map(|entry| entry.map(|evidence| evidence.bind(db, bound)))
                .collect::<Vec<_>>(),
        )
    }
}

impl<'db> TyFoldable<'db> for EvidenceList<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self::new(
            db,
            self.entries(db)
                .iter()
                .map(|entry| entry.map(|evidence| evidence.fold_with(db, folder)))
                .collect::<Vec<_>>(),
        )
    }
}

impl<'db> TyVisitable<'db> for EvidenceList<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        for evidence in self.entries(visitor.db()).iter().flatten() {
            evidence.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for EvidenceId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self.data(db) {
            Evidence::Impl {
                selected,
                instantiated,
                conds,
            } => EvidenceId::new(
                db,
                Evidence::Impl {
                    selected: *selected,
                    instantiated: instantiated.fold_with(db, folder),
                    conds: conds
                        .iter()
                        .map(|cond| cond.fold_with(db, folder))
                        .collect(),
                },
            ),
            Evidence::Assumption(_) => self,
        }
    }
}

impl<'db> TyVisitable<'db> for EvidenceId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        if let Evidence::Impl {
            instantiated,
            conds,
            ..
        } = self.data(visitor.db())
        {
            instantiated.visit_with(visitor);
            for cond in conds {
                cond.visit_with(visitor);
            }
        }
    }
}

/// Whether `goal` names no parameter or inference variable. A supplied
/// dictionary can still distinguish two proofs of such a concrete goal.
fn is_closed_goal<'db>(db: &'db dyn HirAnalysisDb, goal: TraitInstId<'db>) -> bool {
    !collect_flags(db, goal).intersects(TyFlags::HAS_PARAM | TyFlags::HAS_VAR)
}

/// Proves `goal` in `solve_cx`, the context of the body that uses it. A goal
/// or proof that still holds inference variables, such as a generic function
/// item's own, has no evidence: it is proved where the body is instantiated.
pub fn prove<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Option<EvidenceId<'db>> {
    // Inference variables belong to the checker's unification table. Header
    // matching uses its own table and must only receive settled proof inputs.
    // Unresolved calls retain their ordinary inference diagnostics; evidence
    // is recorded after their inputs have been instantiated.
    let unsettled = TyFlags::HAS_VAR | TyFlags::HAS_INVALID;
    if collect_flags(db, goal).intersects(unsettled)
        || solve_cx.choices().is_some_and(|choices| {
            collect_flags(db, choices.items(db).as_slice()).intersects(unsettled)
        })
    {
        return None;
    }
    // A body's declared dictionary remains authoritative even when its type
    // is concrete. A lexical override explicitly replaces that dictionary.
    if !solve_cx.has_matching_choice(db, goal)
        && let Some(index) = solve_cx
            .assumptions()
            .extend_all_bounds(db)
            .list(db)
            .iter()
            .position(|&bound| bound == goal)
    {
        return Some(EvidenceId::new(db, Evidence::Assumption(index as u32)));
    }
    // Closed goals without supplied dictionaries or choices share a proof.
    let solve_cx = if is_closed_goal(db, goal) && solve_cx.choices().is_none() {
        solve_cx.with_assumptions(PredicateListId::empty_list(db))
    } else {
        solve_cx
    };
    prove_in(db, solve_cx, goal)
}

/// A use's proofs, in its body's context `solve_cx`, of the bounds a body it
/// reaches declares (`declared`), instantiated by `subst`, by their places
/// among the assumptions that body is checked under (`signature`). A bound
/// whose goal is closed can be reconstructed ordinarily only when no
/// lexical choice or supplied dictionary affects it. Resolved effect-provider
/// arguments supply proofs through these same slots. Implied bounds project from their declared
/// root dictionaries rather than selecting independently in the caller.
pub(crate) fn declared_bound_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    declared: PredicateListId<'db>,
    signature: PredicateListId<'db>,
    subst: &CompleteSubst<'db>,
    solve_cx: TraitSolveCx<'db>,
) -> EvidenceList<'db> {
    let assumptions = solve_cx.assumptions().extend_all_bounds(db);
    let declared_proof = |goal| prove(db, solve_cx, goal);
    EvidenceList::of(
        db,
        signature
            .list(db)
            .iter()
            .map(|&bound| {
                let goal = substitute_complete(db, bound, subst).ok()?;
                let directly_declared = declared.list(db).contains(&bound);
                if directly_declared
                    && is_closed_goal(db, goal)
                    && solve_cx.choices().is_none()
                    && !assumptions.list(db).contains(&goal)
                {
                    return None;
                }
                let mut answer = if directly_declared {
                    Some(declared_proof(goal)?)
                } else {
                    None
                };
                // A slot must agree with every declared dictionary that implies it,
                // including when the same bound is also written explicitly.
                for &root in declared.list(db) {
                    if root == bound
                        || !PredicateListId::new(db, vec![root])
                            .extend_all_bounds(db)
                            .list(db)
                            .contains(&bound)
                    {
                        continue;
                    }
                    let source_goal = substitute_complete(db, root, subst).ok()?;
                    let proof = declared_proof(source_goal)?;
                    let projected = implied_bound_evidence(db, proof, goal, assumptions)?;
                    if answer.is_some_and(|previous| previous != projected) {
                        return None;
                    }
                    answer = Some(projected);
                }
                answer
            })
            .collect(),
    )
}

/// An implementation's implied bound is proved in its declaration context,
/// then supplied with the actual proofs of that implementation's conditions.
/// This is projection from a dictionary, not fresh caller-side selection.
#[salsa::tracked(returns(copy))]
fn implied_bound_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    proof: EvidenceId<'db>,
    goal: TraitInstId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<EvidenceId<'db>> {
    if matches!(proof.data(db), Evidence::Assumption(_)) {
        let index = assumptions
            .extend_all_bounds(db)
            .list(db)
            .iter()
            .position(|&bound| bound == goal)?;
        return Some(EvidenceId::new(db, Evidence::Assumption(index as u32)));
    }
    let Evidence::Impl {
        selected,
        instantiated,
        ..
    } = proof.data(db)
    else {
        return None;
    };
    let ImplementorOrigin::Hir(implementation) = selected.origin(db) else {
        return None;
    };
    let subst =
        CompleteSubst::for_owner(db, implementation.into(), instantiated.params(db).clone())
            .ok()?;
    let roots = PredicateListId::new(db, vec![selected.trait_inst(db)]).extend_all_bounds(db);
    let declaration_goal = roots.list(db).iter().copied().find(|&candidate| {
        substitute_complete(db, candidate, &subst).is_ok_and(|candidate| candidate == goal)
    })?;
    let declared = PredicateListId::new(db, vec![declaration_goal]);
    let proofs = basis_proofs(
        db,
        implementation.scope(),
        selected.constraints(db).extend_all_bounds(db),
        declared,
        declared,
    );
    let supplied = expanded_condition_evidence(db, proof, assumptions);
    instantiate_proofs(db, &proofs, &subst, supplied).get(db, 0)
}

/// Extend an implementation's actual prerequisite dictionaries with their
/// implied dictionaries. Every path to a shared slot must agree. This is
/// projection through supplied proofs, never selection in the caller's scope.
#[salsa::tracked(returns(copy))]
fn expanded_condition_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    proof: EvidenceId<'db>,
    assumptions: PredicateListId<'db>,
) -> EvidenceList<'db> {
    let Evidence::Impl {
        selected,
        instantiated,
        conds,
    } = proof.data(db)
    else {
        return EvidenceList::none(db);
    };
    let ImplementorOrigin::Hir(implementation) = selected.origin(db) else {
        return EvidenceList::none(db);
    };
    let Ok(subst) =
        CompleteSubst::for_owner(db, implementation.into(), instantiated.params(db).clone())
    else {
        return EvidenceList::none(db);
    };
    let basis = selected.constraints(db);
    EvidenceList::of(
        db,
        basis
            .extend_all_bounds(db)
            .list(db)
            .iter()
            .map(|&bound| {
                let goal = substitute_complete(db, bound, &subst).ok()?;
                let mut answer = None;
                for (index, &root) in basis.list(db).iter().enumerate() {
                    if !PredicateListId::new(db, vec![root])
                        .extend_all_bounds(db)
                        .list(db)
                        .contains(&bound)
                    {
                        continue;
                    }
                    let source = *conds.get(index)?;
                    let projected = if root == bound {
                        source
                    } else {
                        implied_bound_evidence(db, source, goal, assumptions)?
                    };
                    if answer.is_some_and(|previous| previous != projected) {
                        return None;
                    }
                    answer = Some(projected);
                }
                answer
            })
            .collect(),
    )
}

#[salsa::tracked(returns(copy), cycle_initial = prove_cycle_initial)]
fn prove_in<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Option<EvidenceId<'db>> {
    let settled = |flags: TyFlags| !flags.intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID);
    if !settled(collect_flags(db, goal)) {
        return None;
    }
    let Selection::Unique(resolved) = resolve_trait_impl_instance(db, solve_cx, goal) else {
        return None;
    };
    if !settled(collect_flags(db, resolved)) {
        return None;
    }
    let evidence = match resolved.selected().origin(db) {
        ImplementorOrigin::Assumption => {
            Evidence::Assumption(answering_assumption(db, solve_cx, goal)? as u32)
        }
        ImplementorOrigin::Hir(_) => {
            // A `mut` goal's conditions are its implementation's `mut?`
            // bounds at `mut`.
            let mode = Some(if goal.is_mut(db) {
                BorrowKind::Mut
            } else {
                BorrowKind::Ref
            });
            Evidence::Impl {
                selected: resolved.selected(),
                instantiated: resolved.instantiated(),
                conds: resolved
                    .instantiated_constraints(db)
                    .list(db)
                    .iter()
                    .map(|&cond| prove(db, solve_cx, cond.at_mode(db, mode)))
                    .collect::<Option<_>>()?,
            }
        }
        ImplementorOrigin::VirtualContract(_)
        | ImplementorOrigin::Callable
        | ImplementorOrigin::Structural => Evidence::Impl {
            selected: resolved.selected(),
            instantiated: resolved.instantiated(),
            conds: Vec::new(),
        },
    };
    let proof = EvidenceId::new(db, evidence);
    // A selected implementation also receives its private prerequisite
    // dictionaries. Validate their shared implied slots here, while this
    // producer owns the selection, before any body could reconstruct a
    // missing slot with an ordinary proof.
    if proof.has_named_impl(db) {
        let supplied = expanded_condition_evidence(db, proof, solve_cx.assumptions());
        let count = resolved
            .selected()
            .constraints(db)
            .extend_all_bounds(db)
            .list(db)
            .len();
        if (0..count).any(|index| supplied.get(db, index).is_none()) {
            return None;
        }
    }
    Some(proof)
}

/// The implementation answering `inst` where a checked body is
/// instantiated: the one the checker's `evidence` applies. A goal the
/// checker proved by one of the body's bounds holds by that bound in every
/// instance: by the proof the instance's caller gave of it, or by the
/// instance's own bound where the caller gave none, so it is dispatched
/// through the bound, unless the instantiation closes it, when it holds by
/// the implementation every body proves it with. A goal the checker did not
/// prove is selected in `solve_cx`.
pub(crate) fn instantiated_selection<'db>(
    db: &'db dyn HirAnalysisDb,
    evidence: Option<EvidenceId<'db>>,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> Option<ResolvedImplInstance<'db>> {
    let scope = solve_cx.normalization_scope_for_trait_inst(db, inst);
    let goal = || normalize_trait_inst_preserving_validity(db, inst, scope, solve_cx.assumptions());
    match evidence.map(|evidence| evidence.data(db)) {
        // The proof names no goal: the use answers its own, normalized as
        // selection resolves it. An instantiated body can expose projections
        // its checker could not resolve.
        Some(Evidence::Impl {
            selected,
            instantiated,
            ..
        }) => {
            let instantiated = if collect_flags(db, *instantiated).contains(TyFlags::HAS_PROJECTION)
            {
                normalize_ty(db, *instantiated, scope, solve_cx.assumptions())
            } else {
                *instantiated
            };
            Some(ResolvedImplInstance::new(*selected, instantiated, goal()))
        }
        Some(Evidence::Assumption(_)) if is_closed_goal(db, inst) => {
            let ordinary = solve_cx.with_assumptions(PredicateListId::empty_list(db));
            instantiated_selection(db, prove(db, ordinary, inst), solve_cx, inst)
        }
        // As selection answers a goal by an assumption.
        Some(Evidence::Assumption(_)) => {
            let goal = goal();
            let assumption = ImplementorId::assumption(db, goal);
            Some(ResolvedImplInstance::new(assumption, assumption, goal))
        }
        None => match resolve_trait_impl_instance(db, solve_cx, inst) {
            Selection::Unique(resolved) => Some(resolved),
            Selection::Ambiguous(_) | Selection::NotFound => None,
        },
    }
}

/// How the method `body` of an implementation proves, at `mode`, the
/// assumptions it is checked under (`func_signature`) from what a call of the
/// trait's method `nominal` supplies. Each proof names as its assumptions the
/// implementation's extended conditions, including their implied dictionary
/// slots, followed by the assumptions `nominal`'s body is checked
/// under, as the call proves them. Absent for a method of no implementation.
#[salsa::tracked]
pub(crate) fn impl_method_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Func<'db>,
    nominal: Func<'db>,
    mode: Option<BorrowKind>,
) -> Option<Vec<Option<EvidenceId<'db>>>> {
    let Some(ItemKind::ImplTrait(impl_trait)) = body.scope().parent_item(db) else {
        return None;
    };
    let implementor = lower_impl_trait(db, impl_trait)?;
    let conditions = collect_candidate_constraints(db, impl_trait.into())
        .instantiate_identity()
        .extend_all_bounds(db);
    let nominal_mode = BodyOwner::func_at(db, nominal, mode).mode();
    let mode = BodyOwner::func_at(db, body, mode).mode();
    let (subst, _, _) = trait_to_impl_param_subst(
        db,
        (CallableDef::Func(body), mode),
        (CallableDef::Func(nominal), nominal_mode),
        implementor.trait_inst(db),
    );
    let nominal_signature = substitute_complete(
        db,
        func_signature(db, nominal, nominal_mode),
        &subst.residualize(db),
    )
    .ok()?;
    let basis = PredicateListId::new(
        db,
        conditions
            .list(db)
            .iter()
            .chain(nominal_signature.list(db))
            .copied()
            .collect::<Vec<_>>(),
    );
    Some(basis_proofs(
        db,
        body.scope(),
        basis,
        func_declared_bounds(db, body, mode),
        func_signature(db, body, mode),
    ))
}

/// How the value `body` of an implementation's constant proves the
/// assumptions it is checked under from the implementation's conditions, as
/// a proof selecting the implementation proves them. Absent for a body of no
/// implementation.
#[salsa::tracked]
pub(crate) fn impl_const_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> Option<Vec<Option<EvidenceId<'db>>>> {
    let Some(ItemKind::ImplTrait(impl_trait)) = body.scope().parent_item(db) else {
        return None;
    };
    let declared = const_body_assumptions(db, body.scope());
    Some(basis_proofs(
        db,
        body.scope(),
        collect_candidate_constraints(db, impl_trait.into())
            .instantiate_identity()
            .extend_all_bounds(db),
        declared,
        declared.extend_all_bounds(db),
    ))
}

/// Proofs, in `scope`, of the entries of `signature`, a body's assumptions,
/// that the body declares (`declared`), from the assumptions `basis`, each
/// naming the assumptions it uses by their positions in `basis` itself. A
/// bound `basis` states is that assumption, including supplied supertrait
/// dictionaries. Other implied bounds are left for the instance to prove.
fn basis_proofs<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    basis: PredicateListId<'db>,
    declared: PredicateListId<'db>,
    signature: PredicateListId<'db>,
) -> Vec<Option<EvidenceId<'db>>> {
    let assumption = |index: usize| EvidenceId::new(db, Evidence::Assumption(index as u32));
    // A proof names its assumptions by position in the extended basis.
    let to_basis = || {
        EvidenceList::of(
            db,
            basis
                .extend_all_bounds(db)
                .list(db)
                .iter()
                .map(|pred| Some(assumption(basis.list(db).iter().position(|p| p == pred)?)))
                .collect::<Vec<_>>(),
        )
    };
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(basis);
    signature
        .list(db)
        .iter()
        .map(|&goal| {
            if let Some(index) = basis.list(db).iter().position(|&pred| pred == goal) {
                return Some(assumption(index));
            }
            if !declared.list(db).contains(&goal) {
                return None;
            }
            prove(db, solve_cx, goal)?.rebase(db, to_basis())
        })
        .collect()
}

/// `proofs` of a body's assumptions, in its own terms, instantiated by
/// `subst`, with each basis assumption they name replaced by `supplied`'s
/// proof of it.
fn instantiate_proofs<'db>(
    db: &'db dyn HirAnalysisDb,
    proofs: &[Option<EvidenceId<'db>>],
    subst: &CompleteSubst<'db>,
    supplied: EvidenceList<'db>,
) -> EvidenceList<'db> {
    EvidenceList::of(
        db,
        proofs
            .iter()
            .map(|proof| {
                substitute_complete(db, (*proof)?, subst)
                    .ok()?
                    .rebase(db, supplied)
            })
            .collect::<Vec<_>>(),
    )
}

/// The proofs a call supplies to the method `body`, of the implementation
/// its `selection` proof selects, of the assumptions `body` is checked under
/// at `mode`: from the selection's proofs of the implementation's conditions
/// and the call's proofs of `nominal`'s assumptions, `bound`. `subst`
/// instantiates `body`. Calls of one method with one substitution and one
/// selection share them.
#[salsa::tracked(returns(copy))]
pub(crate) fn impl_method_bound_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    (body, nominal, mode): (Func<'db>, Func<'db>, Option<BorrowKind>),
    selection: EvidenceId<'db>,
    bound: EvidenceList<'db>,
    subst: GenericSubst<'db>,
    assumptions: PredicateListId<'db>,
) -> EvidenceList<'db> {
    let (Some(proofs), Some(subst)) = (
        impl_method_evidence(db, body, nominal, mode),
        subst.mapping(db).as_ref(),
    ) else {
        return EvidenceList::none(db);
    };
    let Evidence::Impl { selected, .. } = selection.data(db) else {
        return EvidenceList::none(db);
    };
    let conditions = expanded_condition_evidence(db, selection, assumptions);
    let condition_count = selected
        .constraints(db)
        .extend_all_bounds(db)
        .list(db)
        .len();
    let supplied = EvidenceList::of(
        db,
        (0..condition_count)
            .map(|index| conditions.get(db, index))
            .chain(bound.entries(db).iter().copied())
            .collect::<Vec<_>>(),
    );
    instantiate_proofs(db, proofs, subst, supplied)
}

/// The proofs a use supplies to the value `body` of the implementation's
/// constant its `selection` proof selects: from the selection's proofs of
/// the implementation's conditions. `subst` instantiates `body`'s owner.
#[salsa::tracked(returns(copy))]
pub(crate) fn impl_const_bound_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    selection: EvidenceId<'db>,
    subst: GenericSubst<'db>,
    assumptions: PredicateListId<'db>,
) -> EvidenceList<'db> {
    let (Some(proofs), Some(subst)) = (impl_const_evidence(db, body), subst.mapping(db).as_ref())
    else {
        return EvidenceList::none(db);
    };
    let supplied = expanded_condition_evidence(db, selection, assumptions);
    instantiate_proofs(db, proofs, subst, supplied)
}

/// The evidence an instance runs with: its key's proofs, `supplied`, of the
/// assumptions its body is checked under, instantiated as `goals`, with a
/// proof in `solve_cx` of each closed goal `supplied` gives no closed proof
/// of. Closed supplied proofs are authoritative. A residual assumption from
/// another body cannot be used as an instance dictionary.
#[salsa::tracked(returns(copy))]
pub(crate) fn instance_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    goals: PredicateListId<'db>,
    supplied: EvidenceList<'db>,
    solve_cx: TraitSolveCx<'db>,
) -> EvidenceList<'db> {
    EvidenceList::of(
        db,
        goals
            .list(db)
            .iter()
            .enumerate()
            .map(|(index, &goal)| {
                let supplied = supplied.get(db, index);
                if is_closed_goal(db, goal) && !supplied.is_some_and(|proof| proof.is_closed(db)) {
                    prove(db, solve_cx, goal)
                } else {
                    supplied
                }
            })
            .collect(),
    )
}

fn prove_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _solve_cx: TraitSolveCx<'db>,
    _goal: TraitInstId<'db>,
) -> Option<EvidenceId<'db>> {
    None
}
