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

use crate::analysis::name_resolution::Tier;
use rustc_hash::FxHashMap;
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
        instantiate_selected_choice, instantiate_selected_impl, ordinary_trait_impl_candidates,
        resolve_trait_impl_instance,
    },
    trait_lower::lower_impl_trait,
    trait_resolution::{
        ChoiceMatch, ChosenImpls, PredicateListId, Selection, TraitSolveCx, answering_assumption,
        assumption_matcher, constraint::collect_candidate_constraints,
        generation::GenerationRequest, normalize_trait_inst_preserving_validity,
    },
    ty_check::{BodyOwner, func_declared_bounds, func_signature},
    ty_def::{BorrowKind, TyFlags, TyId},
    ty_lower::CompleteSubst,
    unify::UnificationTable,
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

impl<'db> BoundOwner<'db> {
    fn slots(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        match self {
            Self::Body(BodyOwner::Func(func, mode)) => {
                func_signature(db, func, BodyOwner::func_at(db, func, mode).mode())
            }
            Self::Body(BodyOwner::AnonConstBody { body, .. }) => {
                const_body_assumptions(db, body.scope()).extend_all_bounds(db)
            }
            Self::Body(BodyOwner::Const(const_)) => {
                const_body_assumptions(db, const_.scope()).extend_all_bounds(db)
            }
            Self::Body(
                owner @ (BodyOwner::ContractInit { .. }
                | BodyOwner::ContractRecvArm { .. }
                | BodyOwner::Closure { .. }),
            ) => super::ty_check::infer_body(db, owner).1.assumptions(),
            Self::MethodBasis {
                body,
                nominal,
                mode,
            } => impl_method_evidence(db, body, nominal, mode)
                .as_ref()
                .map_or_else(
                    || PredicateListId::empty_list(db),
                    |adaptation| adaptation.source.slots(db),
                ),
            Self::ConstBasis(body) => impl_const_evidence(db, body).as_ref().map_or_else(
                || PredicateListId::empty_list(db),
                |adaptation| adaptation.source.slots(db),
            ),
            Self::ImplementationConditions(selected) => {
                selected.constraints(db).extend_all_bounds(db)
            }
            Self::Context => PredicateListId::empty_list(db),
        }
    }
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

    pub fn nominal(self, db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, self.owner(db), PredicateListId::empty_list(db))
    }

    fn basis_slots(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        let slots = self.slots(db);
        if slots.is_empty(db) {
            self.owner(db).slots(db)
        } else {
            slots
        }
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

impl<'db> BoundSlotRef<'db> {
    pub fn nominal(self, db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            schema: self.schema.nominal(db),
            index: self.index,
            applied: PredicateListId::empty_list(db),
        }
    }

    fn applied_slots(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        if self.applied.is_empty(db) {
            self.schema.basis_slots(db)
        } else {
            self.applied
        }
    }
}

/// Supplies for a particular namespace. Index equality alone never authorizes
/// replacement. A prefix supply leaves closure-added slots unresolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct BoundEvidence<'db> {
    pub schema: BoundSchemaId<'db>,
    pub entries: EvidenceList<'db>,
}

impl<'db> BoundEvidence<'db> {
    pub fn nominal(self, db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            schema: self.schema.nominal(db),
            entries: self.entries.nominal(db),
        }
    }

    fn get(self, db: &'db dyn HirAnalysisDb, slot: BoundSlotRef<'db>) -> Option<EvidenceId<'db>> {
        let same_basis = self.schema == slot.schema
            || (matches!(self.schema.owner(db), BoundOwner::Body(_))
                && self.schema.owner(db) == slot.schema.owner(db)
                && self
                    .schema
                    .basis_slots(db)
                    .list(db)
                    .starts_with(slot.schema.basis_slots(db).list(db)));
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
    pub(crate) fn complete_deferred(
        &mut self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<(), EvidenceCompletionError<'db>> {
        if let Some(dispatch) = &mut self.dispatch {
            dispatch.proof = complete_deferred_evidence(db, dispatch.proof)?;
        }
        if let Some(bounds) = &mut self.bounds {
            bounds.entries = bounds.entries.complete_deferred(db)?;
        }
        Ok(())
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

/// Declared slots in source order and their values at the current stage.
/// Keep the source goal and predicates rigid when specializing their values:
/// distinct source parameters can acquire the same concrete type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct DeferredProofBounds<'db> {
    source_goal: TraitInstId<'db>,
    source_choices: Option<ChosenImpls<'db>>,
    source_slots: PredicateListId<'db>,
    applied_slots: PredicateListId<'db>,
    entries: EvidenceList<'db>,
}

enum SourceProof<'db> {
    Bound(EvidenceId<'db>),
    Choice(usize, Option<PredicateListId<'db>>),
    Implementation(ImplementorId<'db>),
    Open,
}

impl<'db> DeferredProofBounds<'db> {
    fn capture(
        db: &'db dyn HirAnalysisDb,
        source_goal: TraitInstId<'db>,
        cx: TraitSolveCx<'db>,
    ) -> Self {
        let slots = cx.assumptions().extend_all_bounds(db);
        let schema = BoundSchemaId::context(db, slots);
        let entries = EvidenceList::of(
            db,
            (0..slots.list(db).len())
                .map(|index| Some(schema.assumption(db, index)))
                .collect(),
        );
        Self {
            source_goal,
            source_choices: cx.choices(),
            source_slots: slots,
            applied_slots: slots,
            entries,
        }
    }

    fn map_entries(self, entries: EvidenceList<'db>) -> Self {
        Self { entries, ..self }
    }

    fn source_proof(
        self,
        db: &'db dyn HirAnalysisDb,
        cx: TraitSolveCx<'db>,
        goal: TraitInstId<'db>,
    ) -> Result<SourceProof<'db>, ()> {
        let source_cx = cx
            .with_assumptions(self.source_slots)
            .with_choices(self.source_choices);
        if let Some(choice) = source_cx.matching_choice(db, self.source_goal) {
            let conditions = match choice {
                ChoiceMatch::Implementation(_, matched) => {
                    Some(matched.implementation().constraints(db))
                }
                _ => None,
            };
            return Ok(SourceProof::Choice(choice.index(), conditions));
        }
        let exact = self.source_slots.list(db).contains(&self.source_goal);
        if !exact
            && let Selection::Unique(resolved) =
                resolve_trait_impl_instance(db, source_cx, self.source_goal)
        {
            let selected = resolved.selected();
            match selected.origin(db) {
                ImplementorOrigin::Assumption => {}
                ImplementorOrigin::Structural
                    if super::sol_codec::sol_codec_proof_is_deferred(db, resolved.trait_inst()) =>
                {
                    return Ok(SourceProof::Open);
                }
                _ => return Ok(SourceProof::Implementation(selected)),
            }
        }
        match self.proof_for(db, source_cx, cx, goal)? {
            Some(proof) => Ok(SourceProof::Bound(proof)),
            None if exact => Err(()),
            None => Ok(SourceProof::Open),
        }
    }

    fn proof_for(
        self,
        db: &'db dyn HirAnalysisDb,
        source_cx: TraitSolveCx<'db>,
        cx: TraitSolveCx<'db>,
        goal: TraitInstId<'db>,
    ) -> Result<Option<EvidenceId<'db>>, ()> {
        let matching = self
            .source_slots
            .list(db)
            .iter()
            .zip(self.applied_slots.list(db));
        let source_answers = assumption_matcher(db, source_cx, self.source_goal);
        let applied_answers = assumption_matcher(db, cx, goal);
        let mut answer = None;
        for (index, (&source, &applied)) in matching.enumerate() {
            if source_answers(source) && applied_answers(applied) {
                let proof = self.entries.get(db, index).ok_or(())?;
                if answer.is_some_and(|other| other != proof) {
                    return Err(());
                }
                answer = Some(proof);
            }
        }
        Ok(answer)
    }
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
    /// The compiler's provisional Solidity codec obligation. Its
    /// element is still generic, so the whole-slot fallback is not a selection.
    /// Substitution closes this leaf without replacing any selected parent.
    DeferredSolCodec {
        goal: TraitInstId<'db>,
        scope: ScopeId<'db>,
        choices: Option<ChosenImpls<'db>>,
        bounds: Option<DeferredProofBounds<'db>>,
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
            Evidence::DeferredSolCodec {
                goal,
                scope,
                choices,
                bounds,
            } => Self::new(
                db,
                Evidence::DeferredSolCodec {
                    goal: *goal,
                    scope: *scope,
                    choices: *choices,
                    bounds: bounds.map(|bounds| {
                        bounds.map_entries(EvidenceList::of(
                            db,
                            bounds
                                .entries
                                .entries(db)
                                .iter()
                                .map(|entry| entry.and_then(|proof| proof.rebase(db, bound)))
                                .collect(),
                        ))
                    }),
                },
            ),
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
            Evidence::DeferredSolCodec {
                goal,
                scope,
                choices,
                bounds,
            } => Self::new(
                db,
                Evidence::DeferredSolCodec {
                    goal: *goal,
                    scope: *scope,
                    choices: *choices,
                    bounds: bounds
                        .map(|bounds| bounds.map_entries(bounds.entries.qualify(db, owner))),
                },
            ),
            Evidence::Assumption(_) => self,
        }
    }

    /// This proof with every assumption named only by its owner and position.
    pub fn nominal(self, db: &'db dyn HirAnalysisDb) -> Self {
        match self.data(db) {
            Evidence::Assumption(slot) => {
                EvidenceId::new(db, Evidence::Assumption(slot.nominal(db)))
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
                    conds: conds.iter().map(|proof| proof.nominal(db)).collect(),
                },
            ),
            Evidence::DeferredSolCodec {
                goal,
                scope,
                choices,
                bounds,
            } => Self::new(
                db,
                Evidence::DeferredSolCodec {
                    goal: *goal,
                    scope: *scope,
                    choices: *choices,
                    bounds: bounds.map(|bounds| bounds.map_entries(bounds.entries.nominal(db))),
                },
            ),
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
            Evidence::DeferredSolCodec {
                goal,
                scope,
                choices,
                bounds,
            } => Self::new(
                db,
                Evidence::DeferredSolCodec {
                    goal: *goal,
                    scope: *scope,
                    choices: *choices,
                    bounds: bounds.map(|bounds| bounds.map_entries(bounds.entries.bind(db, bound))),
                },
            ),
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

/// Complete only the obligations explicitly retained by the producer.
/// This runs at instance finalization, never in generic substitution/folding.
#[salsa::tracked(returns(copy))]
fn complete_deferred_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    proof: EvidenceId<'db>,
) -> Result<EvidenceId<'db>, EvidenceCompletionError<'db>> {
    if !proof.has_deferred(db) {
        return Ok(proof);
    }
    match proof.data(db) {
        Evidence::Assumption(_) => Ok(proof),
        Evidence::DeferredSolCodec {
            goal,
            scope,
            choices,
            bounds,
        } => {
            let open = TyFlags::HAS_PARAM
                | TyFlags::HAS_VAR
                | TyFlags::HAS_PROJECTION
                | TyFlags::HAS_INVALID;
            if collect_flags(db, *goal).intersects(open)
                || choices.is_some_and(|choices| collect_flags(db, choices).intersects(open))
            {
                Ok(proof)
            } else {
                let cx = TraitSolveCx::new(db, *scope, Tier::All).with_choices(*choices);
                let outcome = match bounds {
                    Some(bounds) => prove_in(
                        db,
                        cx.with_assumptions(bounds.applied_slots),
                        *goal,
                        Some(*bounds),
                    ),
                    None => prove_outcome(db, cx, *goal),
                };
                match outcome {
                    ProofOutcome::Proven(proof) => Ok(proof),
                    ProofOutcome::NeedsGeneration(request) => {
                        Err(EvidenceCompletionError::NeedsGeneration(request))
                    }
                    ProofOutcome::Deferred | ProofOutcome::Unavailable => {
                        Err(EvidenceCompletionError::Unavailable)
                    }
                }
            }
        }
        Evidence::Impl {
            selected,
            instantiated,
            conds,
        } => Ok(EvidenceId::new(
            db,
            Evidence::Impl {
                selected: *selected,
                instantiated: *instantiated,
                conds: conds
                    .iter()
                    .map(|&proof| complete_deferred_evidence(db, proof))
                    .collect::<Result<_, _>>()?,
            },
        )),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) enum EvidenceCompletionError<'db> {
    Unavailable,
    NeedsGeneration(GenerationRequest<'db>),
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

    pub fn nominal(self, db: &'db dyn HirAnalysisDb) -> Self {
        self.map_proofs(db, |proof| proof.nominal(db))
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

    fn complete_deferred(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<Self, EvidenceCompletionError<'db>> {
        let complete = |proof: Option<EvidenceId<'db>>| {
            proof
                .map(|proof| complete_deferred_evidence(db, proof))
                .transpose()
        };
        let entries = self
            .entries(db)
            .iter()
            .map(|proof| complete(*proof))
            .collect::<Result<Vec<_>, _>>()?;
        let rows = self
            .rows(db)
            .iter()
            .map(|row| {
                Ok(RowEvidence {
                    proof: complete(row.proof)?,
                    ..row.clone()
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self::of(db, entries).with_rows(db, rows))
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
            Evidence::DeferredSolCodec {
                goal,
                scope,
                choices,
                bounds,
            } => EvidenceId::new(
                db,
                Evidence::DeferredSolCodec {
                    goal: goal.fold_with(db, folder),
                    scope: *scope,
                    choices: choices.map(|choices| choices.fold_with(db, folder)),
                    bounds: bounds.map(|bounds| DeferredProofBounds {
                        applied_slots: bounds.applied_slots.fold_with(db, folder),
                        entries: bounds.entries.fold_with(db, folder),
                        ..bounds
                    }),
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
            Evidence::DeferredSolCodec {
                goal,
                choices,
                bounds,
                ..
            } => {
                goal.visit_with(visitor);
                if let Some(choices) = choices {
                    choices.visit_with(visitor);
                }
                if let Some(bounds) = bounds {
                    bounds.applied_slots.visit_with(visitor);
                    bounds.entries.visit_with(visitor);
                }
            }
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
    /// Preparation must admit the requested candidate before the goal can be proved.
    NeedsGeneration(GenerationRequest<'db>),
    Proven(EvidenceId<'db>),
    Deferred,
    Unavailable,
}

impl<'db> ProofOutcome<'db> {
    /// A type constant may not depend on a named choice. If a proof fails,
    /// check whether removing named choices restores a proof. This catches
    /// failures inside prerequisites as well as at the root goal.
    pub(crate) fn reads_named_choice(
        self,
        db: &'db dyn HirAnalysisDb,
        cx: TraitSolveCx<'db>,
        goal: TraitInstId<'db>,
    ) -> bool {
        match self {
            Self::Proven(proof) => proof.has_named_impl(db),
            Self::Unavailable => {
                cx.matching_named_choice(db, goal)
                    || cx.without_named_choices(db).is_some_and(|ordinary| {
                        matches!(prove_outcome(db, ordinary, goal), Self::Proven(_))
                    })
            }
            Self::NeedsGeneration(_) | Self::Deferred => false,
        }
    }

    pub(crate) fn evidence(self) -> Option<EvidenceId<'db>> {
        match self {
            Self::Proven(evidence) => Some(evidence),
            Self::Deferred | Self::Unavailable | Self::NeedsGeneration(_) => None,
        }
    }

    /// The request that stopped the proof, if preparation must serve one.
    pub(crate) fn request(self) -> Option<GenerationRequest<'db>> {
        match self {
            Self::NeedsGeneration(request) => Some(request),
            Self::Proven(_) | Self::Deferred | Self::Unavailable => None,
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
    prove_in(db, solve_cx, goal, None)
}

/// A use's proofs, in its body's context `solve_cx`, of the bounds a body it
/// reaches declares (`declared`), instantiated by `subst`, by their places
/// among the assumptions that body is checked under (`signature`). A bound
/// whose goal is closed can be reconstructed ordinarily only when no
/// lexical choice or supplied dictionary affects it. Resolved effect-provider
/// arguments supply proofs through these same slots. Implied bounds project from their declared
/// root dictionaries rather than selecting independently in the caller.
/// The generation requests the proofs stopped on are returned with the slots:
/// a missing slot that waits on one is not an unsatisfied trait obligation.
pub(crate) fn declared_bound_evidence_with_pending<'db>(
    db: &'db dyn HirAnalysisDb,
    declared: PredicateListId<'db>,
    signature: PredicateListId<'db>,
    subst: &CompleteSubst<'db>,
    solve_cx: TraitSolveCx<'db>,
) -> (EvidenceList<'db>, Vec<GenerationRequest<'db>>) {
    let assumptions = solve_cx.assumptions().extend_all_bounds(db);
    let mut pending = Vec::new();
    let evidence = declared_bound_evidence_from(
        db,
        declared,
        signature,
        subst,
        assumptions,
        |_, goal, direct| {
            if direct
                && is_closed_goal(db, goal)
                && solve_cx.choices().is_none()
                && !assumptions.list(db).contains(&goal)
            {
                return None;
            }
            let outcome = prove_outcome(db, solve_cx, goal);
            pending.extend(outcome.request());
            outcome.evidence()
        },
    );
    (evidence, pending)
}

pub(crate) fn declared_bound_evidence_from<'db>(
    db: &'db dyn HirAnalysisDb,
    declared: PredicateListId<'db>,
    signature: PredicateListId<'db>,
    subst: &CompleteSubst<'db>,
    assumptions: PredicateListId<'db>,
    mut declared_proof: impl FnMut(TraitInstId<'db>, TraitInstId<'db>, bool) -> Option<EvidenceId<'db>>,
) -> EvidenceList<'db> {
    EvidenceList::of(
        db,
        signature
            .list(db)
            .iter()
            .map(|&bound| {
                let goal = substitute_complete(db, bound, subst).ok()?;
                let directly_declared = declared.list(db).contains(&bound);
                let mut answer = if directly_declared {
                    Some(declared_proof(bound, goal, true)?)
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
                    let proof = declared_proof(root, source_goal, false)?;
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
            .applied_slots(db)
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
    // `goal` is in the coordinates of the callee's signature, whose implied
    // bounds come from a trait bound without associated-type definitions: a
    // bound that a trait declares on an associated type names the
    // projection, `<[u256; 3] as Collection>::Cursor: Copy`. The trait
    // instance carrying this implementation's definitions rewrites that
    // bound to the defined type (`usize: Copy`), and `extend_all_bounds`
    // drops it when the type is concrete. So the bounds implied by the
    // header alone are searched as well, and when no candidate is equal,
    // candidates are compared after normalization. Associated types agree
    // across implementations, so normalization reads no choice. The first
    // search is the previous one, so a goal found before is found the same.
    let instantiate = |candidate| substitute_complete(db, candidate, &subst).ok();
    let defined = PredicateListId::new(db, vec![selected.trait_inst(db)]).extend_all_bounds(db);
    let header = PredicateListId::new(db, vec![selected.trait_(db)]).extend_all_bounds(db);
    let candidates = || defined.list(db).iter().chain(header.list(db)).copied();
    let declaration_goal = candidates()
        .find(|&candidate| instantiate(candidate) == Some(goal))
        .or_else(|| {
            let scope = implementation.scope();
            let normalized =
                |inst| normalize_trait_inst_preserving_validity(db, inst, scope, assumptions);
            let target = normalized(goal);
            candidates().find(|&candidate| {
                instantiate(candidate).is_some_and(|candidate| normalized(candidate) == target)
            })
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

/// Recover each selected condition in the original goal's type coordinates.
/// Only the selected header may specialize a source variable here. In
/// particular, two source variables with equal runtime arguments stay apart.
fn source_conditions<'db>(
    db: &'db dyn HirAnalysisDb,
    selected: ImplementorId<'db>,
    mut bounds: DeferredProofBounds<'db>,
) -> Option<(PredicateListId<'db>, DeferredProofBounds<'db>)> {
    let mut table = UnificationTable::new(db);
    let (source, origins) = table.freshen_with_origins(bounds.source_goal);
    let implementation = table.instantiate_with_fresh_vars(selected);
    for (&selected, &source) in implementation
        .trait_inst(db)
        .args(db)
        .iter()
        .zip(source.args(db))
    {
        table.unify(selected, source).ok()?;
    }
    struct Restore<'db>(FxHashMap<TyId<'db>, TyId<'db>>);
    impl<'db> TyFolder<'db> for Restore<'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            self.0
                .get(&ty)
                .copied()
                .unwrap_or_else(|| ty.super_fold_with(db, self))
        }
    }
    let mut restore = Restore(FxHashMap::default());
    let mut aliases = Restore(FxHashMap::default());
    for (param, origin) in origins {
        let origin = origin.fold_with(db, &mut table);
        if origin.has_var(db) {
            let representative = *restore.0.entry(origin).or_insert(param);
            aliases.0.insert(param, representative);
        }
    }
    // Apply header equalities to the source predicates without reordering
    // their slots. The proof lookup can require their actual entries to agree.
    bounds.source_slots = bounds.source_slots.fold_with(db, &mut aliases);
    bounds.source_choices = bounds
        .source_choices
        .map(|choices| choices.fold_with(db, &mut aliases));
    let conditions = implementation
        .constraints(db)
        .fold_with(db, &mut table)
        .fold_with(db, &mut restore);
    Some((conditions, bounds))
}

#[salsa::tracked(returns(copy), cycle_initial = prove_cycle_initial)]
fn prove_in<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
    bounds: Option<DeferredProofBounds<'db>>,
) -> ProofOutcome<'db> {
    if !proof_inputs_settled(collect_flags(db, goal)) {
        return ProofOutcome::Deferred;
    }
    let (selection, chosen, source_conditions) = if let Some(bounds) = bounds {
        let source = match bounds.source_proof(db, solve_cx, goal) {
            Ok(SourceProof::Bound(proof)) => return ProofOutcome::Proven(proof),
            Ok(source) => source,
            Err(()) => return ProofOutcome::Unavailable,
        };
        let (matching, source_conditions) = match source {
            SourceProof::Choice(index, conditions) => {
                let Some(choice) = solve_cx.matching_choice_at(db, goal, index) else {
                    return ProofOutcome::Unavailable;
                };
                (Some(choice), conditions)
            }
            SourceProof::Open => (solve_cx.matching_choice(db, goal), None),
            _ => (None, None),
        };
        let (choice, chosen) = match matching {
            Some(ChoiceMatch::Implementation(index, choice)) => (Some(choice), Some(index)),
            Some(ChoiceMatch::NeedsGeneration(_, request)) => {
                return ProofOutcome::NeedsGeneration(request);
            }
            Some(ChoiceMatch::Unsettled(_)) => return ProofOutcome::Deferred,
            None => (None, None),
        };
        let selection = if let Some(choice) = choice {
            instantiate_selected_choice(db, solve_cx, choice, goal)
                .map_or(Selection::NotFound, Selection::Unique)
        } else if let SourceProof::Implementation(selected) = source {
            let scope = solve_cx.normalization_scope_for_trait_inst(db, goal);
            let goal =
                normalize_trait_inst_preserving_validity(db, goal, scope, solve_cx.assumptions());
            instantiate_selected_impl(db, solve_cx, selected, goal)
                .map_or(Selection::NotFound, Selection::Unique)
        } else {
            let selected = resolve_trait_impl_instance(db, solve_cx, goal);
            if matches!(selected, Selection::NotFound) {
                // Applied choices can make the table reject a parent before
                // the children's source proofs are consulted. Count viable
                // implementations, including blanket implementations, only
                // after checking their prerequisites with source evidence.
                let mut candidates = ordinary_trait_impl_candidates(db, solve_cx, goal);
                // Compiler-provided clauses keep the decisions that were
                // made by the table solver itself.
                candidates.retain(|candidate| {
                    matches!(candidate.selected().origin(db), ImplementorOrigin::Hir(_))
                });
                if !candidates.is_empty() {
                    return prove_deferred_candidates(db, solve_cx, goal, bounds, candidates);
                }
            }
            selected
        };
        (selection, chosen, source_conditions)
    } else {
        (resolve_trait_impl_instance(db, solve_cx, goal), None, None)
    };
    let resolved = match selection {
        Selection::Unique(resolved) => resolved,
        Selection::NeedsGeneration(request) => return ProofOutcome::NeedsGeneration(request),
        Selection::NotFound | Selection::Ambiguous(_) => {
            if chosen.is_some() {
                return ProofOutcome::Unavailable;
            }
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
    prove_selected(db, solve_cx, goal, bounds, source_conditions, resolved)
}

fn proof_inputs_settled(flags: TyFlags) -> bool {
    !flags.intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID)
}

fn prove_deferred_candidates<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
    bounds: DeferredProofBounds<'db>,
    candidates: impl IntoIterator<Item = ResolvedImplInstance<'db>>,
) -> ProofOutcome<'db> {
    let mut proof = None;
    let mut deferred = false;
    let mut request = None;
    for candidate in candidates {
        match prove_selected(db, solve_cx, goal, Some(bounds), None, candidate) {
            ProofOutcome::Proven(candidate) => {
                if proof.replace(candidate).is_some() {
                    return ProofOutcome::Unavailable;
                }
            }
            ProofOutcome::Deferred => deferred = true,
            ProofOutcome::NeedsGeneration(candidate) => {
                request.get_or_insert(candidate);
            }
            ProofOutcome::Unavailable => {}
        }
    }
    if let Some(request) = request {
        return ProofOutcome::NeedsGeneration(request);
    }
    if deferred {
        return ProofOutcome::Deferred;
    }
    proof.map_or(ProofOutcome::Unavailable, ProofOutcome::Proven)
}

fn prove_selected<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
    bounds: Option<DeferredProofBounds<'db>>,
    chosen_source_conditions: Option<PredicateListId<'db>>,
    resolved: ResolvedImplInstance<'db>,
) -> ProofOutcome<'db> {
    if !proof_inputs_settled(collect_flags(db, resolved)) {
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
                choices: solve_cx.choices(),
                bounds: Some(DeferredProofBounds::capture(db, goal, solve_cx)),
            },
        ));
    }
    let mut conditions = Vec::new();
    if matches!(
        resolved.selected().origin(db),
        ImplementorOrigin::Hir(_) | ImplementorOrigin::Structural
    ) {
        let mode = Some(if goal.is_mut(db) {
            BorrowKind::Mut
        } else {
            BorrowKind::Ref
        });
        let source_conditions = bounds.and_then(|bounds| {
            chosen_source_conditions
                .map(|conditions| (conditions, bounds))
                .or_else(|| source_conditions(db, resolved.selected(), bounds))
        });
        let unbound_cx = if bounds.is_some() {
            solve_cx.with_assumptions(PredicateListId::empty_list(db))
        } else {
            solve_cx
        };
        for (index, &condition) in resolved
            .instantiated_constraints(db)
            .list(db)
            .iter()
            .enumerate()
        {
            let condition = condition.at_mode(db, mode);
            let source = source_conditions.and_then(|(conditions, bounds)| {
                let source_goal = conditions.list(db).get(index)?.at_mode(db, mode);
                proof_inputs_settled(collect_flags(db, source_goal)).then_some(
                    DeferredProofBounds {
                        source_goal,
                        ..bounds
                    },
                )
            });
            let outcome = match source {
                Some(bounds) => prove_in(db, solve_cx, condition, Some(bounds)),
                None => prove_outcome(db, unbound_cx, condition),
            };
            match outcome {
                ProofOutcome::Proven(proof) => conditions.push(proof),
                ProofOutcome::NeedsGeneration(request) => {
                    return ProofOutcome::NeedsGeneration(request);
                }
                ProofOutcome::Deferred | ProofOutcome::Unavailable => {
                    return ProofOutcome::Unavailable;
                }
            }
        }
    }
    let selected_proof = || -> Option<EvidenceId<'db>> {
        let evidence = match resolved.selected().origin(db) {
            ImplementorOrigin::Assumption if bounds.is_some() => return None,
            ImplementorOrigin::Assumption => Evidence::Assumption(
                BoundSchemaId::context(db, solve_cx.assumptions().extend_all_bounds(db))
                    .slot(db, answering_assumption(db, solve_cx, goal)?)?,
            ),
            ImplementorOrigin::Hir(_) | ImplementorOrigin::Structural => Evidence::Impl {
                selected: resolved.selected(),
                instantiated: resolved.instantiated(),
                conds: conditions,
            },
            ImplementorOrigin::VirtualContract(_) | ImplementorOrigin::Callable => Evidence::Impl {
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
            Selection::Ambiguous(_) | Selection::NotFound | Selection::NeedsGeneration(_) => None,
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
    let solve_cx = TraitSolveCx::new(db, scope, Tier::All).with_assumptions(basis);
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
                    let completed = complete_deferred_evidence(db, proof).ok()?;
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
    _bounds: Option<DeferredProofBounds<'db>>,
) -> ProofOutcome<'db> {
    ProofOutcome::Unavailable
}

#[cfg(test)]
mod schema_tests {
    use super::super::ty_def::TyId;
    use super::*;
    use crate::test_db::{HirAnalysisTestDb, find_func};

    #[test]
    fn deferred_structural_proof_respects_aggregate_prerequisites() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "structural_prerequisites.fe".into(),
            "struct Protected { unsafe invariant: u8 }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let protected = module
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Struct(protected) => Some(protected),
                _ => None,
            })
            .unwrap();
        let clearable = super::super::corelib::resolve_core_trait(
            &db,
            module.scope(),
            &["marker", "Clearable"],
        )
        .unwrap();
        let cx = TraitSolveCx::new(&db, module.scope(), Tier::All);
        let protected = super::super::adt_def::AdtRef::Struct(protected).as_adt(&db);
        for (field, expected) in [(TyId::u256(&db), true), (TyId::adt(&db, protected), false)] {
            let aggregate = TyId::tuple_with_elems(&db, &[field]);
            let goal = TraitInstId::new_simple(&db, clearable, vec![aggregate]);
            assert!(super::super::clearable::clearable_implementor(&db, goal).is_some());
            assert_eq!(
                matches!(
                    resolve_trait_impl_instance(&db, cx, goal),
                    Selection::Unique(_)
                ),
                expected,
            );
            let bounds = DeferredProofBounds::capture(&db, goal, cx);
            assert_eq!(
                prove_in(&db, cx, goal, Some(bounds)).evidence().is_some(),
                expected
            );
        }
    }

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
