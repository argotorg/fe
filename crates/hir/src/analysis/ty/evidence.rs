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
    effects::rows::RowPath,
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

/// Declaration that owns the numbering of a proof basis. A method adapter's
/// combined implementation/nominal basis is not its checked body's signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum BoundOwner<'db> {
    Body(BodyOwner<'db>),
    MethodBasis {
        body: Func<'db>,
        nominal: Func<'db>,
        mode: Option<BorrowKind>,
    },
    ConstBasis(Body<'db>),
    ImplementationConditions(ImplementorId<'db>),
    Context,
}

/// The namespace of assumption slots. Declaration ownership does not depend on
/// an instance key. Slots describe the declaration layout; instantiated goals
/// live in the body's signature and a slot's applied view, not its namespace.
/// `Context` is a proof producer's temporary basis, not a checked body.
#[salsa::interned]
#[derive(Debug)]
pub struct BoundSchemaId<'db> {
    #[returns(copy)]
    pub owner: BoundOwner<'db>,
    #[returns(copy)]
    pub slots: PredicateListId<'db>,
}

impl<'db> BoundSchemaId<'db> {
    fn context(db: &'db dyn HirAnalysisDb, slots: PredicateListId<'db>) -> Self {
        Self::new(db, BoundOwner::Context, slots)
    }

    pub fn slot(self, db: &'db dyn HirAnalysisDb, index: usize) -> Option<BoundSlotRef<'db>> {
        (index < self.slots(db).list(db).len()).then_some(BoundSlotRef {
            schema: self,
            index: u32::try_from(index).ok()?,
            applied: self.slots(db),
        })
    }

    fn assumption(self, db: &'db dyn HirAnalysisDb, index: usize) -> EvidenceId<'db> {
        EvidenceId::new(
            db,
            Evidence::Assumption(self.slot(db, index).expect("bound slot")),
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct BoundSlotRef<'db> {
    pub schema: BoundSchemaId<'db>,
    pub index: u32,
    /// Applied goals in declaration order, including equal entries. Extending
    /// a substituted list deduplicates it and cannot be used as a slot address.
    pub applied: PredicateListId<'db>,
}

/// Supplies for a particular namespace. Index equality alone never authorizes
/// replacement. A prefix supply leaves closure-added slots unresolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct BoundEvidence<'db> {
    pub schema: BoundSchemaId<'db>,
    pub entries: EvidenceList<'db>,
}

impl<'db> BoundEvidence<'db> {
    fn get(self, db: &'db dyn HirAnalysisDb, slot: BoundSlotRef<'db>) -> Option<EvidenceId<'db>> {
        let same_basis = self.schema == slot.schema
            || (matches!(self.schema.owner(db), BoundOwner::Body(_))
                && self.schema.owner(db) == slot.schema.owner(db)
                && self
                    .schema
                    .slots(db)
                    .list(db)
                    .starts_with(slot.schema.slots(db).list(db)));
        same_basis
            .then(|| self.entries.get(db, slot.index as usize))
            .flatten()
    }
}

/// A checked use retains the goal as well as the selected proof: modes and
/// associated equalities belong to the use, not the implementation identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct ProvedGoal<'db> {
    pub goal: TraitInstId<'db>,
    pub proof: EvidenceId<'db>,
}

/// The common transport contract for calls and associated constants. `bounds`
/// names the destination declaration; leaves in its proofs name their source.
/// Transport never selects an implementation or erases a failed completion.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue, Default)]
pub struct UseEvidence<'db> {
    pub dispatch: Option<ProvedGoal<'db>>,
    pub bounds: Option<BoundEvidence<'db>>,
}

impl<'db> UseEvidence<'db> {
    pub fn dispatch_proof(self) -> Option<EvidenceId<'db>> {
        self.dispatch.map(|dispatch| dispatch.proof)
    }

    pub fn bound_proofs(self) -> Option<EvidenceList<'db>> {
        self.bounds.map(|bounds| bounds.entries)
    }

    pub fn record_dispatch(&mut self, goal: TraitInstId<'db>, proof: Option<EvidenceId<'db>>) {
        self.dispatch = proof.map(|proof| ProvedGoal { goal, proof });
    }

    pub fn bind(&mut self, db: &'db dyn HirAnalysisDb, supplied: BoundEvidence<'db>) {
        if let Some(dispatch) = &mut self.dispatch {
            dispatch.proof = dispatch.proof.bind(db, supplied);
        }
        if let Some(bounds) = &mut self.bounds {
            bounds.entries = bounds.entries.bind(db, supplied);
        }
    }

    pub fn qualify(&mut self, db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) {
        if let Some(dispatch) = &mut self.dispatch {
            dispatch.proof = dispatch.proof.qualify(db, owner);
        }
        if let Some(bounds) = &mut self.bounds {
            bounds.entries = bounds.entries.qualify(db, owner);
        }
    }

    pub fn has_deferred(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.dispatch
            .is_some_and(|dispatch| dispatch.proof.has_deferred(db))
            || self
                .bounds
                .is_some_and(|bounds| bounds.entries.has_deferred(db))
    }

    /// Explicit instance-boundary completion. A present but uncompletable
    /// selected tree is an error, never an absent proof eligible for re-selection.
    pub fn complete_deferred(&mut self, db: &'db dyn HirAnalysisDb) -> Option<()> {
        if let Some(dispatch) = &mut self.dispatch {
            dispatch.proof = complete_deferred_evidence(db, dispatch.proof)?;
        }
        if let Some(bounds) = &mut self.bounds {
            bounds.entries = bounds.entries.complete_deferred(db)?;
        }
        Some(())
    }
}

impl<'db> TyFoldable<'db> for UseEvidence<'db> {
    fn super_fold_with<F: TyFolder<'db>>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self {
        Self {
            dispatch: self.dispatch.map(|dispatch| ProvedGoal {
                goal: dispatch.goal.fold_with(db, folder),
                proof: dispatch.proof.fold_with(db, folder),
            }),
            bounds: self.bounds.map(|bounds| BoundEvidence {
                schema: bounds.schema,
                entries: bounds.entries.fold_with(db, folder),
            }),
        }
    }
}

impl<'db> TyVisitable<'db> for UseEvidence<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        if let Some(dispatch) = self.dispatch {
            dispatch.goal.visit_with(visitor);
            dispatch.proof.visit_with(visitor);
        }
        if let Some(bounds) = self.bounds {
            bounds.entries.visit_with(visitor);
        }
    }
}

/// One explicit change of predicate basis. Both method and constant adapters
/// interpret these recipes through the same substitution/rebase operation.
#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub(crate) struct BoundAdaptation<'db> {
    source: BoundSchemaId<'db>,
    proofs: Vec<Option<EvidenceId<'db>>>,
}

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
    Assumption(BoundSlotRef<'db>),
    /// The compiler's provisional ordinary Solidity codec obligation. Its
    /// element is still generic, so the whole-slot fallback is not a selection.
    /// Substitution closes this leaf without replacing any selected parent.
    DeferredSolCodec {
        goal: TraitInstId<'db>,
        scope: ScopeId<'db>,
    },
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
            Evidence::Assumption(_) | Evidence::DeferredSolCodec { .. } => None,
        }
    }

    /// This proof with each assumption it names replaced by `bound`'s proof
    /// of it; absent if `bound` has no proof of one.
    fn rebase(self, db: &'db dyn HirAnalysisDb, bound: BoundEvidence<'db>) -> Option<Self> {
        Some(match self.data(db) {
            Evidence::Assumption(index) => bound.get(db, *index)?,
            Evidence::DeferredSolCodec { .. } => self,
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

    /// Attach a checked body's ownership to producer-local assumption slots.
    /// Existing ownership is never changed: this operation cannot relabel a
    /// dictionary arriving from another checked body.
    pub(crate) fn qualify(self, db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Self {
        match self.data(db) {
            Evidence::Assumption(slot) if slot.schema.owner(db) == BoundOwner::Context => {
                EvidenceId::new(
                    db,
                    Evidence::Assumption(BoundSlotRef {
                        schema: BoundSchemaId::new(
                            db,
                            BoundOwner::Body(owner),
                            slot.schema.slots(db),
                        ),
                        ..*slot
                    }),
                )
            }
            Evidence::Impl {
                selected,
                instantiated,
                conds,
            } => EvidenceId::new(
                db,
                Evidence::Impl {
                    selected: *selected,
                    instantiated: *instantiated,
                    conds: conds.iter().map(|proof| proof.qualify(db, owner)).collect(),
                },
            ),
            Evidence::Assumption(_) | Evidence::DeferredSolCodec { .. } => self,
        }
    }

    /// Whether this proof names no assumption: it holds in any body.
    pub fn is_closed(self, db: &'db dyn HirAnalysisDb) -> bool {
        evidence_is_closed(db, self)
    }

    /// Whether this tree selects a named implementation at any depth.
    pub fn has_named_impl(self, db: &'db dyn HirAnalysisDb) -> bool {
        evidence_has_named_impl(db, self)
    }

    pub(crate) fn has_deferred(self, db: &'db dyn HirAnalysisDb) -> bool {
        evidence_has_deferred(db, self)
    }

    /// The proofs of an implementation's conditions; none for an assumption.
    pub fn conds(self, db: &'db dyn HirAnalysisDb) -> &'db [EvidenceId<'db>] {
        match self.data(db) {
            Evidence::Impl { conds, .. } => conds,
            Evidence::Assumption(_) | Evidence::DeferredSolCodec { .. } => &[],
        }
    }

    /// This proof with each assumption it names replaced by `bound`'s proof
    /// of that assumption, where `bound` has one.
    pub fn bind(self, db: &'db dyn HirAnalysisDb, bound: BoundEvidence<'db>) -> Self {
        match self.data(db) {
            Evidence::Assumption(index) => bound.get(db, *index).unwrap_or(self),
            Evidence::DeferredSolCodec { .. } => self,
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

#[salsa::tracked(returns(copy))]
fn evidence_has_deferred<'db>(db: &'db dyn HirAnalysisDb, proof: EvidenceId<'db>) -> bool {
    match proof.data(db) {
        Evidence::DeferredSolCodec { .. } => true,
        Evidence::Impl { conds, .. } => conds.iter().any(|proof| proof.has_deferred(db)),
        Evidence::Assumption(_) => false,
    }
}

/// Complete only the ordinary obligations explicitly retained by the producer.
/// This runs at instance finalization, never in generic substitution/folding.
#[salsa::tracked(returns(copy))]
pub(crate) fn complete_deferred_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    proof: EvidenceId<'db>,
) -> Option<EvidenceId<'db>> {
    if !proof.has_deferred(db) {
        return Some(proof);
    }
    match proof.data(db) {
        Evidence::Assumption(_) => Some(proof),
        Evidence::DeferredSolCodec { goal, scope } => {
            if collect_flags(db, *goal).intersects(
                TyFlags::HAS_PARAM
                    | TyFlags::HAS_VAR
                    | TyFlags::HAS_PROJECTION
                    | TyFlags::HAS_INVALID,
            ) {
                Some(proof)
            } else {
                prove(db, TraitSolveCx::new(db, *scope), *goal)
            }
        }
        Evidence::Impl {
            selected,
            instantiated,
            conds,
        } => Some(EvidenceId::new(
            db,
            Evidence::Impl {
                selected: *selected,
                instantiated: *instantiated,
                conds: conds
                    .iter()
                    .map(|&proof| complete_deferred_evidence(db, proof))
                    .collect::<Option<_>>()?,
            },
        )),
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
        Evidence::Assumption(_) | Evidence::DeferredSolCodec { .. } => false,
    }
}

#[salsa::tracked(returns(copy))]
fn evidence_is_closed<'db>(db: &'db dyn HirAnalysisDb, evidence: EvidenceId<'db>) -> bool {
    match evidence.data(db) {
        Evidence::Assumption(_) | Evidence::DeferredSolCodec { .. } => false,
        Evidence::Impl { conds, .. } => conds.iter().all(|cond| cond.is_closed(db)),
    }
}

/// An effect row's dictionary is addressed by its structural path, not the
/// transient component index produced by one expansion of an abstract row.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct RowEvidence<'db> {
    pub path: RowPath,
    pub goal: TraitInstId<'db>,
    pub proof: Option<EvidenceId<'db>>,
}

impl<'db> TyFoldable<'db> for RowEvidence<'db> {
    fn super_fold_with<F: TyFolder<'db>>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self {
        Self {
            path: self.path,
            goal: self.goal.fold_with(db, folder),
            proof: self.proof.map(|proof| proof.fold_with(db, folder)),
        }
    }
}
impl<'db> TyVisitable<'db> for RowEvidence<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        self.goal.visit_with(visitor);
        if let Some(proof) = self.proof {
            proof.visit_with(visitor);
        }
    }
}

/// Supplies for declaration slots and for components of open effect rows.
/// Row paths stay stable when substitution expands an earlier abstract row.
#[salsa::interned]
#[derive(Debug)]
pub struct EvidenceList<'db> {
    pub entries: Vec<Option<EvidenceId<'db>>>,
    pub rows: Vec<RowEvidence<'db>>,
}

impl<'db> EvidenceList<'db> {
    pub fn none(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new(), Vec::new())
    }

    pub fn of(db: &'db dyn HirAnalysisDb, mut entries: Vec<Option<EvidenceId<'db>>>) -> Self {
        while entries.last().is_some_and(Option::is_none) {
            entries.pop();
        }
        Self::new(db, entries, Vec::new())
    }

    pub fn with_rows(self, db: &'db dyn HirAnalysisDb, mut rows: Vec<RowEvidence<'db>>) -> Self {
        rows.sort_by(|a, b| (a.path.entry, &a.path.steps).cmp(&(b.path.entry, &b.path.steps)));
        debug_assert!(rows.windows(2).all(|pair| pair[0].path != pair[1].path));
        Self::new(db, self.entries(db).clone(), rows)
    }

    pub fn get(self, db: &'db dyn HirAnalysisDb, index: usize) -> Option<EvidenceId<'db>> {
        self.entries(db).get(index).copied().flatten()
    }

    pub(crate) fn qualify(self, db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Self {
        self.map_proofs(db, |proof| proof.qualify(db, owner))
    }

    fn map_proofs(
        self,
        db: &'db dyn HirAnalysisDb,
        mut map: impl FnMut(EvidenceId<'db>) -> EvidenceId<'db>,
    ) -> Self {
        Self::of(
            db,
            self.entries(db)
                .iter()
                .map(|entry| entry.map(&mut map))
                .collect(),
        )
        .with_rows(
            db,
            self.rows(db)
                .iter()
                .map(|row| RowEvidence {
                    proof: row.proof.map(&mut map),
                    ..row.clone()
                })
                .collect(),
        )
    }

    pub fn has_deferred(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.entries(db)
            .iter()
            .flatten()
            .copied()
            .chain(self.rows(db).iter().filter_map(|row| row.proof))
            .any(|proof| proof.has_deferred(db))
    }

    pub fn complete_deferred(self, db: &'db dyn HirAnalysisDb) -> Option<Self> {
        let complete = |proof| match proof {
            Some(proof) => complete_deferred_evidence(db, proof).map(Some),
            None => Some(None),
        };
        let entries = self
            .entries(db)
            .iter()
            .map(|proof| complete(*proof))
            .collect::<Option<Vec<_>>>()?;
        let rows = self
            .rows(db)
            .iter()
            .map(|row| {
                Some(RowEvidence {
                    proof: complete(row.proof)?,
                    ..row.clone()
                })
            })
            .collect::<Option<Vec<_>>>()?;
        Some(Self::of(db, entries).with_rows(db, rows))
    }

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
            .chain(self.rows(db).iter().filter_map(|row| {
                (row.proof.is_none()
                    && !collect_flags(db, row.goal)
                        .intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID)
                    && !mentions_effect_provider(db, row.goal))
                .then_some(row.goal)
            }))
            .collect()
    }

    pub fn bind(self, db: &'db dyn HirAnalysisDb, bound: BoundEvidence<'db>) -> Self {
        self.map_proofs(db, |proof| proof.bind(db, bound))
    }
}

impl<'db> TyFoldable<'db> for EvidenceList<'db> {
    fn super_fold_with<F: TyFolder<'db>>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self {
        let entries = self
            .entries(db)
            .iter()
            .map(|entry| entry.map(|proof| proof.fold_with(db, folder)))
            .collect();
        Self::of(db, entries).with_rows(db, self.rows(db).clone().fold_with(db, folder))
    }
}
impl<'db> TyVisitable<'db> for EvidenceList<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        for proof in self.entries(visitor.db()).iter().flatten() {
            proof.visit_with(visitor);
        }
        self.rows(visitor.db()).visit_with(visitor);
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
            Evidence::DeferredSolCodec { goal, scope } => EvidenceId::new(
                db,
                Evidence::DeferredSolCodec {
                    goal: goal.fold_with(db, folder),
                    scope: *scope,
                },
            ),
            Evidence::Assumption(slot) => EvidenceId::new(
                db,
                Evidence::Assumption(BoundSlotRef {
                    applied: slot.applied.fold_with(db, folder),
                    ..*slot
                }),
            ),
        }
    }
}

impl<'db> TyVisitable<'db> for EvidenceId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        match self.data(visitor.db()) {
            Evidence::Impl {
                instantiated,
                conds,
                ..
            } => {
                instantiated.visit_with(visitor);
                for cond in conds {
                    cond.visit_with(visitor);
                }
            }
            Evidence::DeferredSolCodec { goal, .. } => goal.visit_with(visitor),
            Evidence::Assumption(slot) => slot.applied.visit_with(visitor),
        }
    }
}

/// Whether `goal` names no parameter or inference variable. A supplied
/// dictionary can still distinguish two proofs of such a concrete goal.
fn is_closed_goal<'db>(db: &'db dyn HirAnalysisDb, goal: TraitInstId<'db>) -> bool {
    !collect_flags(db, goal).intersects(TyFlags::HAS_PARAM | TyFlags::HAS_VAR)
}

/// Result owned by proof production. A deliberately deferred ordinary layout
/// obligation is distinct from a proof that the checked context cannot supply.
/// Consumers must not turn an unavailable lexical proof into ordinary dispatch.
#[derive(Debug, Clone, Copy, PartialEq, Eq, SalsaValue)]
pub(crate) enum ProofOutcome<'db> {
    NeedsGeneration,
    Proven(EvidenceId<'db>),
    Deferred,
    Unavailable,
}

impl<'db> ProofOutcome<'db> {
    pub(crate) fn evidence(self) -> Option<EvidenceId<'db>> {
        match self {
            Self::Proven(evidence) => Some(evidence),
            Self::Deferred | Self::Unavailable | Self::NeedsGeneration => None,
        }
    }
}

/// Proves `goal` in `solve_cx`, the context of the body that uses it. A goal
/// or proof that still holds inference variables, such as a generic function
/// item's own, has no evidence: it is proved where the body is instantiated.
pub fn prove<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Option<EvidenceId<'db>> {
    prove_outcome(db, solve_cx, goal).evidence()
}

pub(crate) fn prove_outcome<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> ProofOutcome<'db> {
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
        return ProofOutcome::Deferred;
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
        return ProofOutcome::Proven(
            BoundSchemaId::context(db, solve_cx.assumptions().extend_all_bounds(db))
                .assumption(db, index),
        );
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
    declared_bound_evidence_with_pending(db, declared, signature, subst, solve_cx).0
}

/// Preserve preparation state from the same proof production that fills the
/// slots. A missing pending slot is not an unsatisfied trait obligation.
pub(crate) fn declared_bound_evidence_with_pending<'db>(
    db: &'db dyn HirAnalysisDb,
    declared: PredicateListId<'db>,
    signature: PredicateListId<'db>,
    subst: &CompleteSubst<'db>,
    solve_cx: TraitSolveCx<'db>,
) -> (EvidenceList<'db>, bool) {
    let assumptions = solve_cx.assumptions().extend_all_bounds(db);
    let mut pending = false;
    let mut declared_proof = |goal| {
        let outcome = prove_outcome(db, solve_cx, goal);
        pending |= matches!(outcome, ProofOutcome::NeedsGeneration);
        outcome.evidence()
    };
    let evidence = EvidenceList::of(
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
    );
    (evidence, pending)
}

/// An implementation's implied bound is proved in its declaration context,
/// then supplied with the actual proofs of that implementation's conditions.
/// This is projection from a dictionary, not fresh caller-side selection.
#[salsa::tracked(returns(copy))]
pub(crate) fn implied_bound_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    proof: EvidenceId<'db>,
    goal: TraitInstId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<EvidenceId<'db>> {
    if let Evidence::Assumption(slot) = proof.data(db) {
        // Use the producer's positional view, not a newly deduplicated list.
        // Substitution can make two different declaration slots equal.
        let index = slot
            .applied
            .list(db)
            .iter()
            .position(|&bound| bound == goal)?;
        return Some(EvidenceId::new(
            db,
            Evidence::Assumption(BoundSlotRef {
                index: u32::try_from(index).ok()?,
                ..*slot
            }),
        ));
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
        BoundOwner::ImplementationConditions(*selected),
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
) -> ProofOutcome<'db> {
    let settled = |flags: TyFlags| !flags.intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID);
    if !settled(collect_flags(db, goal)) {
        return ProofOutcome::Deferred;
    }
    let resolved = match resolve_trait_impl_instance(db, solve_cx, goal) {
        Selection::Unique(resolved) => resolved,
        Selection::NeedsGeneration => return ProofOutcome::NeedsGeneration,
        Selection::NotFound | Selection::Ambiguous(_) => {
            return if collect_flags(db, goal)
                .intersects(TyFlags::HAS_PARAM | TyFlags::HAS_PROJECTION)
                && !solve_cx.has_matching_choice(db, goal)
            {
                ProofOutcome::Deferred
            } else {
                ProofOutcome::Unavailable
            };
        }
    };
    if !settled(collect_flags(db, resolved)) {
        return ProofOutcome::Deferred;
    }
    if matches!(
        resolved.selected().origin(db),
        ImplementorOrigin::Structural
    ) && super::sol_codec::sol_codec_proof_is_deferred(db, resolved.trait_inst())
    {
        return ProofOutcome::Proven(EvidenceId::new(
            db,
            Evidence::DeferredSolCodec {
                goal: resolved.trait_inst(),
                scope: solve_cx.origin_scope(db),
            },
        ));
    }
    let mut conditions = Vec::new();
    if matches!(resolved.selected().origin(db), ImplementorOrigin::Hir(_)) {
        let mode = Some(if goal.is_mut(db) {
            BorrowKind::Mut
        } else {
            BorrowKind::Ref
        });
        for &condition in resolved.instantiated_constraints(db).list(db) {
            match prove_outcome(db, solve_cx, condition.at_mode(db, mode)) {
                ProofOutcome::Proven(proof) => conditions.push(proof),
                ProofOutcome::NeedsGeneration => return ProofOutcome::NeedsGeneration,
                ProofOutcome::Deferred | ProofOutcome::Unavailable => {
                    return ProofOutcome::Unavailable;
                }
            }
        }
    }
    let selected_proof = || -> Option<EvidenceId<'db>> {
        let evidence = match resolved.selected().origin(db) {
            ImplementorOrigin::Assumption => Evidence::Assumption(
                BoundSchemaId::context(db, solve_cx.assumptions().extend_all_bounds(db))
                    .slot(db, answering_assumption(db, solve_cx, goal)?)?,
            ),
            ImplementorOrigin::Hir(_) => Evidence::Impl {
                selected: resolved.selected(),
                instantiated: resolved.instantiated(),
                conds: conditions,
            },
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
    };
    selected_proof().map_or(ProofOutcome::Unavailable, ProofOutcome::Proven)
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
        Some(Evidence::DeferredSolCodec { .. }) => None,
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
            Selection::Ambiguous(_) | Selection::NotFound | Selection::NeedsGeneration => None,
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
) -> Option<BoundAdaptation<'db>> {
    let Some(ItemKind::ImplTrait(impl_trait)) = body.scope().parent_item(db) else {
        return None;
    };
    let implementor = lower_impl_trait(db, impl_trait)?;
    let conditions = collect_candidate_constraints(db, impl_trait.into())
        .instantiate_identity()
        .extend_all_bounds(db);
    let nominal_mode = BodyOwner::func_at(db, nominal, mode).mode();
    let body_mode = BodyOwner::func_at(db, body, mode).mode();
    let (subst, _, _) = trait_to_impl_param_subst(
        db,
        (CallableDef::Func(body), body_mode),
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
    let origin = BoundOwner::MethodBasis {
        body,
        nominal,
        mode,
    };
    Some(basis_proofs(
        db,
        body.scope(),
        origin,
        basis,
        func_declared_bounds(db, body, body_mode),
        func_signature(db, body, body_mode),
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
) -> Option<BoundAdaptation<'db>> {
    let Some(ItemKind::ImplTrait(impl_trait)) = body.scope().parent_item(db) else {
        return None;
    };
    let declared = const_body_assumptions(db, body.scope());
    Some(basis_proofs(
        db,
        body.scope(),
        BoundOwner::ConstBasis(body),
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
    owner: BoundOwner<'db>,
    basis: PredicateListId<'db>,
    declared: PredicateListId<'db>,
    signature: PredicateListId<'db>,
) -> BoundAdaptation<'db> {
    let source = BoundSchemaId::new(db, owner, basis);
    let assumption = |index: usize| source.assumption(db, index);
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
    let proofs = signature
        .list(db)
        .iter()
        .map(|&goal| {
            if let Some(index) = basis.list(db).iter().position(|&pred| pred == goal) {
                return Some(assumption(index));
            }
            if !declared.list(db).contains(&goal) {
                return None;
            }
            prove(db, solve_cx, goal)?.rebase(
                db,
                BoundEvidence {
                    schema: BoundSchemaId::context(db, basis.extend_all_bounds(db)),
                    entries: to_basis(),
                },
            )
        })
        .collect();
    BoundAdaptation { source, proofs }
}

/// `proofs` of a body's assumptions, in its own terms, instantiated by
/// `subst`, with each basis assumption they name replaced by `supplied`'s
/// proof of it.
fn instantiate_proofs<'db>(
    db: &'db dyn HirAnalysisDb,
    adaptation: &BoundAdaptation<'db>,
    subst: &CompleteSubst<'db>,
    supplied: EvidenceList<'db>,
) -> EvidenceList<'db> {
    let supplied = BoundEvidence {
        schema: adaptation.source,
        entries: supplied,
    };
    EvidenceList::of(
        db,
        adaptation
            .proofs
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
                if let Some(proof) = supplied
                    && proof.has_deferred(db)
                {
                    // A selected parent is authoritative even if one retained
                    // obligation cannot close. Never replace that tree.
                    let completed = complete_deferred_evidence(db, proof)?;
                    return (!is_closed_goal(db, goal) || completed.is_closed(db))
                        .then_some(completed);
                }
                if is_closed_goal(db, goal) && !supplied.is_some_and(|proof| proof.is_closed(db)) {
                    prove(db, solve_cx, goal)
                } else {
                    supplied
                }
            })
            .collect(),
    )
    .with_rows(db, supplied.rows(db).clone())
}

fn prove_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _solve_cx: TraitSolveCx<'db>,
    _goal: TraitInstId<'db>,
) -> ProofOutcome<'db> {
    ProofOutcome::Unavailable
}

#[cfg(test)]
mod schema_tests {
    use super::super::ty_def::TyId;
    use super::*;
    use crate::test_db::{HirAnalysisTestDb, find_func};

    #[test]
    fn implied_projection_keeps_declaration_slot_after_parameter_collapse() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "schema_collapse.fe".into(),
            r#"
trait A {}
trait Base {}
trait Sub: Base {}
fn context<T: A, U: A, V: Sub>() {}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = find_func(&db, module, "context");
        let signature = func_signature(&db, func, None);
        assert_eq!(signature.list(&db).len(), 4);
        let schema =
            BoundSchemaId::new(&db, BoundOwner::Body(BodyOwner::func(&db, func)), signature);
        let original = schema.assumption(&db, 2);
        let subst = CompleteSubst::for_owner(
            &db,
            func.into(),
            vec![TyId::u256(&db), TyId::u256(&db), TyId::bool(&db)],
        )
        .unwrap();
        let applied = substitute_complete(&db, signature, &subst).unwrap();
        let supplied = substitute_complete(&db, original, &subst).unwrap();
        let goal = applied.list(&db)[3];
        let deduplicated = applied.extend_all_bounds(&db);
        assert_eq!(deduplicated.list(&db).len(), 3);
        assert_eq!(
            deduplicated
                .list(&db)
                .iter()
                .position(|&candidate| candidate == goal),
            Some(2)
        );
        let projected = implied_bound_evidence(&db, supplied, goal, deduplicated).unwrap();
        let Evidence::Assumption(slot) = projected.data(&db) else {
            panic!("projected slot")
        };
        assert_eq!(slot.schema, schema);
        assert_eq!(
            slot.index, 3,
            "applied-list position must not address the declaration schema"
        );
    }
}
