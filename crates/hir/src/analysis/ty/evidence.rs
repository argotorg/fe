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

use rustc_hash::FxHashMap;
use salsa::SalsaValue;

use super::{
    closure::settle_function_item_providers,
    const_ty::const_body_assumptions,
    effects::rows::RowPath,
    fold::{TyFoldable, TyFolder},
    method_cmp::trait_to_impl_param_subst,
    normalize::normalize_ty,
    subst::substitute_complete,
    trait_def::{
        ImplementorId, ImplementorOrigin, NamedOutputMismatch, ResolvedImplInstance, TraitInstId,
        concrete_goal_output_mismatch, instantiate_selected_choice, instantiate_selected_impl,
        ordinary_trait_impl_candidates, resolve_trait_impl_instance,
    },
    trait_lower::lower_impl_trait,
    trait_resolution::{
        BoundStep, ChosenImpls, Component, PredicateListId, Selection, TraitSolveCx,
        answering_assumption, assumption_matcher, concrete_components,
        constraint::collect_candidate_constraints, has_concrete_components,
        normalize_trait_inst_preserving_validity, step_bound,
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

    pub(crate) fn basis_slots(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
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
            component: None,
        })
    }

    fn assumption(self, db: &'db dyn HirAnalysisDb, index: usize) -> EvidenceId<'db> {
        EvidenceId::new(
            db,
            Evidence::Assumption(self.slot(db, index).expect("bound slot")),
        )
    }
}

/// The derivation steps from a slot to a concrete bound that the slot's
/// dictionary holds but that is not a slot itself (`Foo: Show` from
/// `B: Tr<A = Foo>`): the bounds of the first associated type, then the first
/// supertrait, and so on. The path holds no type, so substitution leaves it.
#[salsa::interned]
#[derive(Debug)]
pub(crate) struct ComponentPath<'db> {
    #[returns(ref)]
    pub(crate) steps: Vec<BoundStep<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct BoundSlotRef<'db> {
    pub schema: BoundSchemaId<'db>,
    /// A slot of `schema`'s basis. A component reference names the slot its
    /// derivation starts from.
    pub index: u32,
    /// Applied goals in declaration order, including equal entries. Extending
    /// a substituted list deduplicates it and cannot be used as a slot address.
    pub applied: PredicateListId<'db>,
    /// The derivation from the slot to the bound this reference names, when
    /// that bound is a component of the slot's dictionary and no slot.
    pub(crate) component: Option<ComponentPath<'db>>,
}

impl<'db> BoundSlotRef<'db> {
    pub fn nominal(self, db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            schema: self.schema.nominal(db),
            applied: PredicateListId::empty_list(db),
            ..self
        }
    }

    /// The steps after the slot's index, as a stable name: `.A0.S1` for the
    /// first bound of associated type `A`, then the second supertrait.
    pub fn component_label(self, db: &'db dyn HirAnalysisDb) -> String {
        let Some(path) = self.component else {
            return String::new();
        };
        path.steps(db)
            .iter()
            .map(|step| match step {
                BoundStep::Super(index) => format!(".S{index}"),
                BoundStep::AssocBound { assoc, index } => {
                    format!(".{}{index}", assoc.data(db))
                }
            })
            .collect()
    }
}

/// The bound a reference names: the goal of its slot, followed along its
/// path. In the coordinates of the schema's declaration, where a component is
/// concrete.
fn component_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    slot: BoundSlotRef<'db>,
) -> Option<TraitInstId<'db>> {
    let root = *slot
        .schema
        .basis_slots(db)
        .list(db)
        .get(slot.index as usize)?;
    match slot.component {
        None => Some(root),
        Some(path) => path
            .steps(db)
            .iter()
            .try_fold(root, |goal, &step| step_bound(db, goal, step)),
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
        let root = same_basis
            .then(|| self.entries.get(db, slot.index as usize))
            .flatten()?;
        // A component is the field of the slot's dictionary at the end of its
        // path. No step reads an assumption of the caller.
        let Some(path) = slot.component else {
            return Some(root);
        };
        let mut goal = *slot
            .schema
            .basis_slots(db)
            .list(db)
            .get(slot.index as usize)?;
        let mut proof = root;
        for &step in path.steps(db) {
            goal = step_bound(db, goal, step)?;
            proof = project_step(db, proof, step, Some(goal), PredicateListId::empty_list(db))
                .proof()?;
        }
        Some(proof)
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
    Choice(usize, ImplementorId<'db>),
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
        if let Some((index, application)) = source_cx.matching_choice(db, self.source_goal) {
            return Ok(SourceProof::Choice(index, application));
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
pub(crate) fn complete_deferred_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    proof: EvidenceId<'db>,
) -> Option<EvidenceId<'db>> {
    if !proof.has_deferred(db) {
        return Some(proof);
    }
    match proof.data(db) {
        Evidence::Assumption(_) => Some(proof),
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
                Some(proof)
            } else {
                let cx = TraitSolveCx::new(db, *scope).with_choices(*choices);
                match bounds {
                    Some(bounds) => prove_in(
                        db,
                        cx.with_assumptions(bounds.applied_slots),
                        *goal,
                        Some(*bounds),
                    )
                    .evidence(),
                    None => prove(db, cx, *goal),
                }
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

/// The first implementation in `proof`, applied at a concrete goal, whose
/// outputs differ from those ordinary normalization gives that goal (see
/// `concrete_goal_output_mismatch`), with the mismatch. A proof that selects
/// no named implementation holds the implementations ordinary resolution
/// finds, so it has none.
#[salsa::tracked(returns(clone))]
pub(crate) fn proof_output_mismatch<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    proof: EvidenceId<'db>,
) -> Option<(ResolvedImplInstance<'db>, NamedOutputMismatch<'db>)> {
    if !proof.has_named_impl(db) {
        return None;
    }
    let Evidence::Impl {
        selected,
        instantiated,
        conds,
    } = proof.data(db)
    else {
        return None;
    };
    let own = ResolvedImplInstance::new(*selected, *instantiated, instantiated.trait_inst(db));
    if let Some(mismatch) = concrete_goal_output_mismatch(db, scope, own) {
        return Some((own, mismatch));
    }
    conds
        .iter()
        .find_map(|&cond| proof_output_mismatch(db, scope, cond))
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

    /// The goals of this list's effect rows that have no proof and no unsettled
    /// type. Under a lexical choice they are refused.
    pub(crate) fn unproved_rows(self, db: &'db dyn HirAnalysisDb) -> Vec<TraitInstId<'db>> {
        self.rows(db)
            .iter()
            .filter_map(|row| {
                (row.proof.is_none()
                    && !collect_flags(db, row.goal)
                        .intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID)
                    && !mentions_effect_provider(db, row.goal))
                .then_some(row.goal)
            })
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
            Self::Deferred => false,
        }
    }

    pub(crate) fn evidence(self) -> Option<EvidenceId<'db>> {
        match self {
            Self::Proven(evidence) => Some(evidence),
            Self::Deferred | Self::Unavailable => None,
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

/// The slot of `slots`, an extended assumption list, that answers `goal`, as a
/// reference in the namespace of that list: the first slot that is `goal`; or,
/// for a closed goal, the concrete component of the slots' dictionaries that is
/// the same implementation (`same_identity`), addressed by its path. A closed
/// goal that a component answers is never proved afresh: a proof names no goal,
/// so the dictionary that holds the component holds the implementation.
pub(crate) fn slot_answering<'db>(
    db: &'db dyn HirAnalysisDb,
    slots: PredicateListId<'db>,
    scope: ScopeId<'db>,
    goal: TraitInstId<'db>,
) -> Option<BoundSlotRef<'db>> {
    let schema = BoundSchemaId::context(db, slots);
    if let Some(index) = slots.list(db).iter().position(|&bound| bound == goal) {
        return schema.slot(db, index);
    }
    if !has_concrete_components(db, slots) {
        return None;
    }
    // A goal written on a projection that a slot's equality settles,
    // `<B::A as Show>` under `B: Tr<A = Foo>`, is the closed goal it gives.
    let goal = if collect_flags(db, goal).contains(TyFlags::HAS_PROJECTION) {
        normalize_trait_inst_preserving_validity(db, goal, scope, slots)
    } else {
        goal
    };
    if !is_closed_goal(db, goal) {
        return None;
    }
    let table = concrete_components(db, slots);
    let (_, member) = table
        .of_trait(goal.def(db))
        .find(|(_, member)| same_identity(db, scope, member.goal, goal))?;
    Some(BoundSlotRef {
        component: Some(ComponentPath::new(db, member.path.clone())),
        ..schema.slot(db, member.root as usize)?
    })
}

pub(crate) fn prove_outcome<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> ProofOutcome<'db> {
    let goal = settle_function_item_providers(db, goal);
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
        && let Some(slot) = slot_answering(
            db,
            solve_cx.assumptions().extend_all_bounds(db),
            solve_cx.normalization_scope_for_trait_inst(db, goal),
            goal,
        )
    {
        return ProofOutcome::Proven(EvidenceId::new(db, Evidence::Assumption(slot)));
    }
    // Closed goals without supplied dictionaries or choices share a proof.
    let solve_cx = if is_closed_goal(db, goal) && solve_cx.choices().is_none() {
        solve_cx.with_assumptions(PredicateListId::empty_list(db))
    } else {
        solve_cx
    };
    prove_in(db, solve_cx, goal, None)
}

/// Why a goal is refused.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) enum Refusal<'db> {
    /// No proof of the goal can be had, or the dictionary of `root` (when
    /// known) has no such component.
    NoComponent { root: Option<TraitInstId<'db>> },
    /// The goal is implemented, but the dictionaries of the bounds `a` and `b`
    /// (or a proof written for the goal, `b`) supply different implementations.
    Differs {
        a: TraitInstId<'db>,
        b: TraitInstId<'db>,
    },
}

/// A goal that the dictionaries of a use or an instance refuse, with the reason.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) struct RefusedGoal<'db> {
    pub(crate) goal: TraitInstId<'db>,
    pub(crate) reason: Refusal<'db>,
}

/// A use's proofs of the bounds a body it reaches declares, and the slots
/// among them that are refused. A slot is exactly one of a proof, pending (no
/// proof yet, which an instance supplies) or refused. An empty slot is never
/// read as anything else.
#[derive(Debug, Clone)]
pub(crate) struct DeclaredBounds<'db> {
    pub(crate) proofs: EvidenceList<'db>,
    refused: Vec<(usize, Refusal<'db>)>,
}

impl<'db> DeclaredBounds<'db> {
    /// The refused slots, with the goals instantiated by `subst`, that name no
    /// unsettled type and no effect provider.
    pub(crate) fn refused_goals(
        &self,
        db: &'db dyn HirAnalysisDb,
        signature: PredicateListId<'db>,
        subst: &CompleteSubst<'db>,
    ) -> Vec<RefusedGoal<'db>> {
        self.refused
            .iter()
            .filter_map(|&(index, reason)| {
                let goal = substitute_complete(db, *signature.list(db).get(index)?, subst).ok()?;
                (!mentions_effect_provider(db, goal)
                    && !collect_flags(db, goal).intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID))
                .then_some(RefusedGoal { goal, reason })
            })
            .collect()
    }
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
) -> DeclaredBounds<'db> {
    let assumptions = solve_cx.assumptions().extend_all_bounds(db);
    declared_bound_evidence_from(
        db,
        declared,
        signature,
        subst,
        assumptions,
        |_, goal, direct| {
            // Without a lexical choice, a closed goal that no slot and no
            // component of the dictionaries answers is left to the instance.
            // Only a use under a choice reports a refused slot.
            if direct
                && is_closed_goal(db, goal)
                && solve_cx.choices().is_none()
                && slot_answering(
                    db,
                    assumptions,
                    solve_cx.normalization_scope_for_trait_inst(db, goal),
                    goal,
                )
                .is_none()
            {
                return None;
            }
            prove(db, solve_cx, goal)
        },
    )
}

/// The candidates for a slot are the proof written for it, when the body
/// declares the bound directly, and the component of the dictionary of each
/// bound whose derivation the walk recorded as entering the slot: a declared
/// bound, or a slot settled before it. A written proof is never dropped: the
/// entry is it, or else the first projection. Two closed candidates that
/// differ refuse the slot, as does a proof that cannot be had or projected. A
/// projection an assumption cannot give yet (a concrete goal an equality made)
/// waits for the instance, which compares it with the written proof.
pub(crate) fn declared_bound_evidence_from<'db>(
    db: &'db dyn HirAnalysisDb,
    declared: PredicateListId<'db>,
    signature: PredicateListId<'db>,
    subst: &CompleteSubst<'db>,
    assumptions: PredicateListId<'db>,
    mut declared_proof: impl FnMut(TraitInstId<'db>, TraitInstId<'db>, bool) -> Option<EvidenceId<'db>>,
) -> DeclaredBounds<'db> {
    #[derive(Clone, Copy)]
    enum Slot<'db> {
        Proof(EvidenceId<'db>),
        Pending,
        Refused(Refusal<'db>),
    }
    let bounds = signature.list(db);
    let edges = signature.implied_edges(db);
    // The dictionary each bound offers to the bounds it implies. A declared
    // bound offers the proof of it in the use's context, whether or not its own
    // slot is left to the instance; a derived bound offers its own slot.
    let mut offered: Vec<Option<Slot<'db>>> = vec![None; bounds.len()];
    let mut slots: Vec<Slot<'db>> = Vec::with_capacity(bounds.len());
    for (index, &bound) in bounds.iter().enumerate() {
        let is_declared = declared.list(db).contains(&bound);
        let Ok(goal) = substitute_complete(db, bound, subst) else {
            slots.push(Slot::Pending);
            offered[index] = Some(Slot::Pending);
            continue;
        };
        let mut answer = None;
        let mut refuse: Option<Refusal<'db>> = None;
        if is_declared {
            match declared_proof(bound, goal, true) {
                Some(written) => answer = Some(written),
                None => refuse = Some(Refusal::NoComponent { root: None }),
            }
        }
        for edge in edges.iter().filter(|edge| edge.child as usize == index) {
            let parent = edge.parent as usize;
            let parent_declared = declared.list(db).contains(&bounds[parent]);
            // A derived bound is read from the slots settled before it. A
            // declared bound is read wherever it stands in the list.
            if refuse.is_some() || parent == index || (parent > index && !parent_declared) {
                continue;
            }
            let parent_goal = substitute_complete(db, bounds[parent], subst).ok();
            let offer = *offered[parent].get_or_insert_with(|| {
                match substitute_complete(db, bounds[parent], subst) {
                    Ok(source_goal) => declared_proof(bounds[parent], source_goal, false).map_or(
                        Slot::Refused(Refusal::NoComponent { root: None }),
                        Slot::Proof,
                    ),
                    Err(_) => Slot::Pending,
                }
            });
            let proof = match offer {
                Slot::Proof(proof) => proof,
                // The caller offers no dictionary for it yet.
                Slot::Pending => continue,
                Slot::Refused(_) => {
                    refuse = Some(Refusal::NoComponent { root: parent_goal });
                    continue;
                }
            };
            match project_step(db, proof, edge.step, Some(goal), assumptions) {
                Projection::Proof(projected) => match answer {
                    Some(previous)
                        if previous.is_closed(db)
                            && projected.is_closed(db)
                            && !same_dictionary(db, previous, projected, assumptions) =>
                    {
                        refuse = Some(match parent_goal {
                            Some(root) => Refusal::Differs { a: root, b: goal },
                            None => Refusal::NoComponent { root: None },
                        });
                    }
                    Some(_) => {}
                    None => answer = Some(projected),
                },
                // The dictionary is an assumption of the caller that has no
                // slot for the goal yet: the instance supplies it.
                Projection::Pending => {}
                Projection::Refused => refuse = Some(Refusal::NoComponent { root: parent_goal }),
            }
        }
        let slot = match refuse {
            Some(reason) => Slot::Refused(reason),
            None => answer.map_or(Slot::Pending, Slot::Proof),
        };
        if !is_declared {
            offered[index] = Some(slot);
        }
        slots.push(slot);
    }
    let mut refused = Vec::new();
    let proofs = slots
        .iter()
        .enumerate()
        .map(|(index, slot)| match slot {
            Slot::Proof(proof) => Some(*proof),
            Slot::Pending => None,
            Slot::Refused(reason) => {
                refused.push((index, *reason));
                None
            }
        })
        .collect();
    DeclaredBounds {
        proofs: EvidenceList::of(db, proofs),
        refused,
    }
}

/// What a dictionary gives for an implied goal: its component, nothing yet
/// (an assumption of the caller that has no slot for the goal, which the
/// instance supplies), or no component at all.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) enum Projection<'db> {
    Proof(EvidenceId<'db>),
    Pending,
    Refused,
}

impl<'db> Projection<'db> {
    pub(crate) fn proof(self) -> Option<EvidenceId<'db>> {
        match self {
            Self::Proof(proof) => Some(proof),
            Self::Pending | Self::Refused => None,
        }
    }
}

/// Whether two proofs are one dictionary: the same proof, or the same
/// implementation at the same arguments (equal after normalization) with the
/// same dictionary for each condition.
#[salsa::tracked(returns(copy))]
fn same_dictionary<'db>(
    db: &'db dyn HirAnalysisDb,
    a: EvidenceId<'db>,
    b: EvidenceId<'db>,
    assumptions: PredicateListId<'db>,
) -> bool {
    if a == b {
        return true;
    }
    match (a.data(db), b.data(db)) {
        (Evidence::Assumption(a), Evidence::Assumption(b)) => a.nominal(db) == b.nominal(db),
        (
            Evidence::Impl {
                selected,
                instantiated,
                conds,
            },
            Evidence::Impl {
                selected: other_selected,
                instantiated: other_instantiated,
                conds: other_conds,
            },
        ) => {
            selected == other_selected
                && conds.len() == other_conds.len()
                && (instantiated == other_instantiated
                    || same_arguments(
                        db,
                        *selected,
                        *instantiated,
                        *other_instantiated,
                        assumptions,
                    ))
                && conds
                    .iter()
                    .zip(other_conds)
                    .all(|(&a, &b)| same_dictionary(db, a, b, assumptions))
        }
        _ => false,
    }
}

fn same_arguments<'db>(
    db: &'db dyn HirAnalysisDb,
    selected: ImplementorId<'db>,
    a: ImplementorId<'db>,
    b: ImplementorId<'db>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let ImplementorOrigin::Hir(implementation) = selected.origin(db) else {
        return false;
    };
    let scope = implementation.scope();
    let (a, b) = (a.params(db), b.params(db));
    a.len() == b.len()
        && a.iter().zip(b).all(|(&x, &y)| {
            x == y
                || normalize_ty(db, x, scope, assumptions)
                    == normalize_ty(db, y, scope, assumptions)
        })
}

/// Where an implementation's component for a declared bound comes from, in
/// the implementation's own coordinates (no caller types).
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
enum ImplComponent<'db> {
    /// The implementation's own component, proved where it is declared.
    Own,
    /// Held by the dictionaries of conditions that state a bound answering the
    /// declared one: the condition's position, and the steps from it.
    Held(Vec<(u32, Vec<BoundStep<'db>>)>),
}

/// A condition whose bound (or a bound it implies, including a bound an
/// equality makes concrete) answers `declared` holds that component in its
/// dictionary, so the implementation does not prove it again. An open
/// `declared` is answered by a candidate with more equalities or a stronger
/// mode; the reverse does not answer it. A closed one is answered by the
/// candidate of the same trait and arguments (`same_identity`), the rule by
/// which a body's goal reads a component, so that both read one dictionary.
#[salsa::tracked(returns(ref))]
fn impl_component<'db>(
    db: &'db dyn HirAnalysisDb,
    selected: ImplementorId<'db>,
    declared: TraitInstId<'db>,
) -> ImplComponent<'db> {
    let ImplementorOrigin::Hir(implementation) = selected.origin(db) else {
        return ImplComponent::Own;
    };
    let basis = selected.constraints(db);
    let cx = TraitSolveCx::new(db, implementation.scope()).with_assumptions(basis);
    let answers = assumption_matcher(db, cx, declared);
    let closed = is_closed_goal(db, declared);
    let scope = implementation.scope();
    let mut held = Vec::new();
    for (index, &condition) in basis.list(db).iter().enumerate() {
        let walk = PredicateListId::new(db, vec![condition]).bound_walk_keeping_concrete(db);
        for (member, &candidate) in walk.list.list(db).iter().enumerate().skip(1) {
            let answering = if closed {
                same_identity(db, scope, candidate, declared)
            } else {
                answers(candidate)
            };
            if basis.list(db).contains(&candidate) || !answering {
                continue;
            }
            if let Some(path) = walk.path_to(db, member) {
                held.push((index as u32, path));
            }
        }
    }
    if held.is_empty() {
        ImplComponent::Own
    } else {
        ImplComponent::Held(held)
    }
}

/// The component of the dictionary `proof` that `step` names, with `goal` the
/// bound it proves where the caller knows it. A step follows a derivation the
/// walk recorded: from an assumption, to the slot the walk derived; from an
/// implementation, to the proof of the bound the implementation's own trait
/// instance declares there (`step_bound`), read from the dictionary of the
/// condition that states it or proved where the implementation is declared. A
/// compiler-provided contract implementation answers where the contract is
/// declared; any other compiler-provided implementation implies nothing.
#[salsa::tracked(returns(copy))]
pub(crate) fn project_step<'db>(
    db: &'db dyn HirAnalysisDb,
    proof: EvidenceId<'db>,
    step: BoundStep<'db>,
    goal: Option<TraitInstId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Projection<'db> {
    let (selected, instantiated, conds) = match proof.data(db) {
        Evidence::Assumption(slot) => {
            let slots = slot.schema.basis_slots(db);
            // The child is a slot the walk derived, or a concrete component
            // of the slot's dictionary, which is addressed by its path.
            // Otherwise the walk filtered it: a generic bound it did not keep.
            if slot.component.is_none()
                && let Some(edge) = slots
                    .implied_edges(db)
                    .iter()
                    .find(|edge| edge.parent == slot.index && edge.step == step)
            {
                return Projection::Proof(EvidenceId::new(
                    db,
                    Evidence::Assumption(BoundSlotRef {
                        index: edge.child,
                        component: None,
                        ..*slot
                    }),
                ));
            }
            let Some(child) = component_goal(db, *slot).and_then(|goal| step_bound(db, goal, step))
            else {
                return Projection::Pending;
            };
            let table = concrete_components(db, slots);
            return table
                .find(db, child)
                .map_or(Projection::Pending, |(_, member)| {
                    Projection::Proof(EvidenceId::new(
                        db,
                        Evidence::Assumption(BoundSlotRef {
                            index: member.root,
                            component: Some(ComponentPath::new(db, member.path.clone())),
                            ..*slot
                        }),
                    ))
                });
        }
        Evidence::Impl {
            selected,
            instantiated,
            conds,
        } => (selected, instantiated, conds),
        Evidence::DeferredSolCodec { .. } => return Projection::Refused,
    };
    let implementation = match selected.origin(db) {
        ImplementorOrigin::Hir(implementation) => implementation,
        ImplementorOrigin::VirtualContract(contract) => {
            let Some(goal) = goal.or_else(|| step_bound(db, selected.trait_inst(db), step)) else {
                return Projection::Refused;
            };
            return prove(db, TraitSolveCx::new(db, contract.scope()), goal)
                .map_or(Projection::Refused, Projection::Proof);
        }
        _ => return Projection::Refused,
    };
    let Some(declared) = step_bound(db, selected.trait_inst(db), step) else {
        return Projection::Refused;
    };
    match impl_component(db, *selected, declared) {
        ImplComponent::Held(held) => {
            let mut agreed: Option<EvidenceId<'db>> = None;
            let mut pending = false;
            for (condition, steps) in held {
                let Some(&source) = conds.get(*condition as usize) else {
                    return Projection::Refused;
                };
                let mut current = Projection::Proof(source);
                for (position, &next) in steps.iter().enumerate() {
                    let Projection::Proof(from) = current else {
                        break;
                    };
                    let last = position + 1 == steps.len();
                    current = project_step(db, from, next, goal.filter(|_| last), assumptions);
                }
                match current {
                    Projection::Proof(component) => {
                        if agreed.is_some_and(|other| {
                            !same_dictionary(db, other, component, assumptions)
                        }) {
                            return Projection::Refused;
                        }
                        agreed.get_or_insert(component);
                    }
                    Projection::Pending => pending = true,
                    Projection::Refused => return Projection::Refused,
                }
            }
            match (pending, agreed) {
                (true, _) => Projection::Pending,
                (false, Some(agreed)) => Projection::Proof(agreed),
                (false, None) => Projection::Refused,
            }
        }
        ImplComponent::Own => {
            let Ok(subst) = CompleteSubst::for_owner(
                db,
                implementation.into(),
                instantiated.params(db).clone(),
            ) else {
                return Projection::Refused;
            };
            let declared_bounds = selected.constraints(db).extend_all_bounds(db);
            let declared = PredicateListId::new(db, vec![declared]);
            let proofs = basis_proofs(
                db,
                implementation.scope(),
                BoundOwner::ImplementationConditions(*selected),
                declared_bounds,
                declared,
                declared,
            );
            let supplied = expanded_condition_evidence(db, proof, assumptions);
            instantiate_proofs(db, &proofs, &subst, supplied)
                .get(db, 0)
                .map_or(Projection::Refused, Projection::Proof)
        }
    }
}

/// The proof of `goal`, which `root`'s dictionary `proof` implies (or is): the
/// proof itself where the root answers the goal, otherwise the component at
/// the end of the derivation the walk of `root` recorded. Used where a row
/// supplies the dictionary of a provider's trait and a slot asks for a bound
/// that trait implies.
pub(crate) fn implied_bound_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    proof: EvidenceId<'db>,
    root: TraitInstId<'db>,
    goal: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Projection<'db> {
    let cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    let answers = assumption_matcher(db, cx, goal);
    if answers(root) {
        return Projection::Proof(proof);
    }
    let walk = PredicateListId::new(db, vec![root]).bound_walk(db);
    let members = walk.list.list(db);
    let Some(member) = members
        .iter()
        .position(|&member| member == goal)
        .or_else(|| {
            members
                .iter()
                .skip(1)
                .position(|&member| answers(member))
                .map(|at| at + 1)
        })
    else {
        return Projection::Refused;
    };
    let Some(steps) = walk.path_to(db, member) else {
        return Projection::Refused;
    };
    let mut current = Projection::Proof(proof);
    for (position, &step) in steps.iter().enumerate() {
        let Projection::Proof(from) = current else {
            break;
        };
        let last = position + 1 == steps.len();
        current = project_step(db, from, step, last.then_some(goal), assumptions);
    }
    current
}

/// Extend an implementation's actual prerequisite dictionaries with their
/// implied dictionaries, along the derivations the walk of its conditions
/// recorded. A slot that a condition states and another condition's dictionary
/// implies, or that two dictionaries imply, must have one dictionary. This is
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
    // The conditions of a function item's callable implementation are written
    // at the item's own arguments: it has no parameters to substitute.
    let subst = match selected.origin(db) {
        ImplementorOrigin::Hir(implementation) => {
            let Ok(subst) = CompleteSubst::for_owner(
                db,
                implementation.into(),
                instantiated.params(db).clone(),
            ) else {
                return EvidenceList::none(db);
            };
            Some(subst)
        }
        ImplementorOrigin::Callable => None,
        _ => return EvidenceList::none(db),
    };
    let basis = selected.constraints(db);
    let slots = basis.extend_all_bounds(db);
    let edges = slots.implied_edges(db);
    let goals: Vec<Option<TraitInstId<'db>>> = slots
        .list(db)
        .iter()
        .map(|&bound| match &subst {
            Some(subst) => substitute_complete(db, bound, subst).ok(),
            None => Some(bound),
        })
        .collect();
    let mut entries: Vec<Option<EvidenceId<'db>>> = vec![None; goals.len()];
    for (index, &bound) in slots.list(db).iter().enumerate() {
        let mut answer = basis
            .list(db)
            .iter()
            .position(|&condition| condition == bound)
            .and_then(|condition| conds.get(condition).copied());
        let mut failed = false;
        for edge in edges
            .iter()
            .filter(|edge| edge.child as usize == index && (edge.parent as usize) < index)
        {
            let projected = entries[edge.parent as usize]
                .map(|source| project_step(db, source, edge.step, goals[index], assumptions));
            match projected {
                Some(Projection::Proof(component)) => match answer {
                    Some(previous) if !same_dictionary(db, previous, component, assumptions) => {
                        failed = true;
                        break;
                    }
                    Some(_) => {}
                    None => answer = Some(component),
                },
                _ => {
                    failed = true;
                    break;
                }
            }
        }
        entries[index] = if failed { None } else { answer };
    }
    // A slot that a later slot also implies (conditions that imply one
    // another) must read the same dictionary from it.
    for edge in edges.iter().filter(|edge| edge.parent > edge.child) {
        let (parent, child) = (edge.parent as usize, edge.child as usize);
        let (Some(source), Some(entry)) = (entries[parent], entries[child]) else {
            continue;
        };
        match project_step(db, source, edge.step, goals[child], assumptions) {
            Projection::Proof(component) if same_dictionary(db, component, entry, assumptions) => {}
            _ => entries[child] = None,
        }
    }
    EvidenceList::of(db, entries)
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
        let (chosen, source_conditions) = match source {
            SourceProof::Choice(index, application) => {
                (Some(index), Some(application.constraints(db)))
            }
            SourceProof::Open => {
                let matching = solve_cx.matching_choice(db, goal);
                // A later match can select an opening that was already
                // explicitly applied at the source. Its arguments may be
                // absent from the header, so recover their original bounds
                // from the opening that supplied this application.
                let conditions = matching.and_then(|(index, application)| {
                    let source = bounds.source_choices?.items(db).get(index)?.applied?;
                    (source.origin(db) == application.origin(db)).then(|| source.constraints(db))
                });
                (matching.map(|(index, _)| index), conditions)
            }
            _ => (None, None),
        };
        let selection = if let Some(index) = chosen {
            let applied = solve_cx
                .choices()
                .and_then(|choices| choices.items(db).get(index));
            applied
                .and_then(|&choice| instantiate_selected_choice(db, solve_cx, choice, goal))
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
    let Selection::Unique(resolved) = selection else {
        if chosen.is_some() {
            return ProofOutcome::Unavailable;
        }
        return if collect_flags(db, goal).intersects(TyFlags::HAS_PARAM | TyFlags::HAS_PROJECTION)
            && !solve_cx.has_matching_choice(db, goal)
        {
            ProofOutcome::Deferred
        } else {
            ProofOutcome::Unavailable
        };
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
    for candidate in candidates {
        match prove_selected(db, solve_cx, goal, Some(bounds), None, candidate) {
            ProofOutcome::Proven(candidate) => {
                if proof.replace(candidate).is_some() {
                    return ProofOutcome::Unavailable;
                }
            }
            ProofOutcome::Deferred => deferred = true,
            ProofOutcome::Unavailable => {}
        }
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
    let selected_proof = || -> Option<EvidenceId<'db>> {
        let evidence = match resolved.selected().origin(db) {
            ImplementorOrigin::Assumption if bounds.is_some() => return None,
            ImplementorOrigin::Assumption => Evidence::Assumption(
                BoundSchemaId::context(db, solve_cx.assumptions().extend_all_bounds(db))
                    .slot(db, answering_assumption(db, solve_cx, goal)?)?,
            ),
            ImplementorOrigin::Hir(_)
            | ImplementorOrigin::Structural
            | ImplementorOrigin::Callable => {
                // A `mut` goal's conditions are its implementation's `mut?`
                // bounds at `mut`.
                let mode = Some(if goal.is_mut(db) {
                    BorrowKind::Mut
                } else {
                    BorrowKind::Ref
                });
                let conditions = bounds.and_then(|bounds| {
                    if let Some(conditions) = chosen_source_conditions {
                        Some((conditions, bounds))
                    } else {
                        source_conditions(db, resolved.selected(), bounds)
                    }
                });
                Evidence::Impl {
                    selected: resolved.selected(),
                    instantiated: resolved.instantiated(),
                    conds: resolved
                        .instantiated_constraints(db)
                        .list(db)
                        .iter()
                        .enumerate()
                        .map(|(index, &cond)| {
                            let cond = cond.at_mode(db, mode);
                            if let Some((conditions, bounds)) = conditions
                                && let Some(&source) = conditions.list(db).get(index)
                            {
                                let source_goal = source.at_mode(db, mode);
                                if proof_inputs_settled(collect_flags(db, source_goal)) {
                                    return prove_in(
                                        db,
                                        solve_cx,
                                        cond,
                                        Some(DeferredProofBounds {
                                            source_goal,
                                            ..bounds
                                        }),
                                    )
                                    .evidence();
                                }
                            }
                            let cx = if bounds.is_some() {
                                solve_cx.with_assumptions(PredicateListId::empty_list(db))
                            } else {
                                solve_cx
                            };
                            prove(db, cx, cond)
                        })
                        .collect::<Option<_>>()?,
                }
            }

            ImplementorOrigin::VirtualContract(_) => Evidence::Impl {
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
) -> Option<BoundAdaptation<'db>> {
    let Some(ItemKind::ImplTrait(impl_trait)) = body.scope().parent_item(db) else {
        return None;
    };
    let implementor = lower_impl_trait(db, impl_trait)?;
    let nominal_mode = BodyOwner::func_at(db, nominal, mode).mode();
    let body_mode = BodyOwner::func_at(db, body, mode).mode();
    // The implementation's `mut?` conditions are read at the mode the body
    // is used at, as the body's own signature and selection read them.
    let conditions = collect_candidate_constraints(db, impl_trait.into())
        .instantiate_identity()
        .extend_all_bounds(db)
        .at_mode(db, body_mode);
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

/// The evidence an instance runs with, and the first goal whose dictionaries
/// refuse it. A refused instance is rejected where it is admitted; no slot is
/// proved ordinarily in its place.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) struct InstanceEvidence<'db> {
    pub(crate) proofs: EvidenceList<'db>,
    pub(crate) refused: Option<RefusedGoal<'db>>,
}

/// The evidence an instance runs with: its key's proofs, `supplied`, of the
/// assumptions its body is checked under, instantiated as `goals`. `declared`
/// is the body's slot layout in declaration coordinates, whose first
/// `goals.len()` slots the goals instantiate, and the walk of it records which
/// slot each other slot derives from. A closed goal that has a derivation
/// entering it is never proved afresh: its entry is the component, along each
/// recorded derivation, of the dictionary of the slot it derives from, and
/// every such component, and any closed proof supplied for the slot, must be
/// one dictionary (`same_dictionary`). A slot with no earlier derivation
/// entering it is a root: it keeps a closed supplied proof, takes the
/// component that slots deriving it from later positions give (slots that
/// imply one another, such as `Left::Out: Right<Back = Self>` and
/// `Right::Back: Left<Out = Self>`) when they have a supplied proof, or is
/// proved in `solve_cx`. Every derivation whose two ends are closed is then
/// checked again, so each slot reads what every dictionary that implies it
/// holds. A slot whose parent holds no dictionary, whose projection fails, or
/// whose dictionaries disagree refuses the instance, and is never proved
/// ordinarily in its place. A compiler-provided contract implementation answers
/// where the contract is declared. A residual assumption from another body
/// cannot be used as an instance dictionary.
#[salsa::tracked(returns(copy))]
pub(crate) fn instance_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    goals: PredicateListId<'db>,
    declared: PredicateListId<'db>,
    supplied: EvidenceList<'db>,
    solve_cx: TraitSolveCx<'db>,
) -> InstanceEvidence<'db> {
    // A body that contains closures declares their bounds after its own
    // signature, which the goals are the prefix of.
    let count = goals.list(db).len();
    debug_assert!(declared.list(db).len() >= count);
    let layout = PredicateListId::new(
        db,
        declared
            .list(db)
            .iter()
            .take(count)
            .copied()
            .collect::<Vec<_>>(),
    );
    let edges = layout.implied_edges(db);
    let assumptions = solve_cx.assumptions();
    let goal_at = |index: usize| goals.list(db)[index];
    let closed: Vec<bool> = (0..count)
        .map(|index| is_closed_goal(db, goal_at(index)))
        .collect();
    // A closed supplied proof of a closed goal, or any proof of an open one.
    let own: Vec<Option<EvidenceId<'db>>> = (0..count)
        .map(|index| {
            let proof = supplied.get(db, index)?;
            if proof.has_deferred(db) {
                // A selected parent is authoritative even if one retained
                // obligation cannot close. Never replace that tree.
                complete_deferred_evidence(db, proof)
                    .filter(|completed| !closed[index] || completed.is_closed(db))
            } else {
                (!closed[index] || proof.is_closed(db)).then_some(proof)
            }
        })
        .collect();
    // A closed dictionary has no pending slot: it projects or refuses.
    let project = |proof, step, index: usize| {
        let projection = project_step(db, proof, step, Some(goal_at(index)), assumptions);
        debug_assert_ne!(projection, Projection::Pending);
        projection.proof()
    };
    let no_component = |parent: usize| Refusal::NoComponent {
        root: Some(goal_at(parent)),
    };
    let mut entries: Vec<Option<EvidenceId<'db>>> = vec![None; count];
    let mut refused: Option<RefusedGoal<'db>> = None;
    for index in 0..count {
        if !closed[index] {
            entries[index] = own[index];
            continue;
        }
        let mut candidates: Vec<(usize, EvidenceId<'db>)> = Vec::new();
        let mut refusal = None;
        let mut open_parent = false;
        for edge in edges
            .iter()
            .filter(|edge| edge.child as usize == index && (edge.parent as usize) < index)
        {
            let parent = edge.parent as usize;
            if !closed[parent] {
                // An instance of a generic body has no dictionary yet for a
                // parent whose own goal is open.
                open_parent = true;
                continue;
            }
            // A closed parent with no dictionary cannot be compared with the
            // other parents: the slot is refused rather than answered by the
            // parents that have one.
            match entries[parent].and_then(|proof| project(proof, edge.step, index)) {
                Some(component) => candidates.push((parent, component)),
                None => refusal = refusal.or(Some(no_component(parent))),
            }
        }
        if refusal.is_some() {
            entries[index] = None;
        } else if let Some(&(first_parent, first)) = candidates.first() {
            // A proof supplied for the goal, such as an explicit bound under a
            // choice, must be the one the implying dictionaries hold.
            let differing = candidates[1..]
                .iter()
                .find(|&&(_, other)| !same_dictionary(db, first, other, assumptions))
                .map(|&(parent, _)| goal_at(parent))
                .or_else(|| {
                    own[index]
                        .filter(|&other| !same_dictionary(db, first, other, assumptions))
                        .map(|_| goal_at(index))
                });
            refusal = differing.map(|b| Refusal::Differs {
                a: goal_at(first_parent),
                b,
            });
            entries[index] = refusal.is_none().then_some(first);
        } else if own[index].is_some() || open_parent {
            entries[index] = own[index];
        } else {
            // A root, or implied only by slots later in the list: those that
            // have a supplied proof give the component.
            let mut later = edges
                .iter()
                .filter(|edge| edge.child as usize == index && (edge.parent as usize) > index)
                .filter_map(|edge| {
                    let parent = edge.parent as usize;
                    let proof = own[parent].filter(|proof| proof.is_closed(db))?;
                    closed[parent].then(|| (parent, project(proof, edge.step, index)))
                });
            match later.next() {
                None => entries[index] = prove(db, solve_cx, goal_at(index)),
                Some((parent, None)) => refusal = Some(no_component(parent)),
                Some((first_parent, Some(first))) => {
                    refusal = later.find_map(|(parent, other)| match other {
                        None => Some(no_component(parent)),
                        Some(other) if !same_dictionary(db, first, other, assumptions) => {
                            Some(Refusal::Differs {
                                a: goal_at(first_parent),
                                b: goal_at(parent),
                            })
                        }
                        Some(_) => None,
                    });
                    entries[index] = refusal.is_none().then_some(first);
                }
            }
        }
        if let Some(reason) = refusal {
            entries[index] = None;
            refused.get_or_insert(RefusedGoal {
                goal: goal_at(index),
                reason,
            });
        }
    }
    // Slots that imply one another hold one implementation each: every slot
    // is what the dictionary of each slot that implies it projects for it.
    for edge in edges {
        let (parent, child) = (edge.parent as usize, edge.child as usize);
        if parent == child || !closed[parent] || !closed[child] {
            continue;
        }
        let (Some(source), Some(entry)) = (entries[parent], entries[child]) else {
            continue;
        };
        let reason = match project(source, edge.step, child) {
            None => Some(no_component(parent)),
            Some(component) if !same_dictionary(db, component, entry, assumptions) => {
                Some(Refusal::Differs {
                    a: goal_at(parent),
                    b: goal_at(child),
                })
            }
            Some(_) => None,
        };
        if let Some(reason) = reason {
            entries[child] = None;
            refused.get_or_insert(RefusedGoal {
                goal: goal_at(child),
                reason,
            });
        }
    }
    // The concrete bounds the slots' dictionaries hold are fields of those
    // dictionaries as well. Each derivation of one gives it a dictionary, and
    // they, and the components of one implementation, must all be one.
    if refused.is_none() && has_concrete_components(db, layout) {
        refused = component_agreement(db, layout, goals, &closed, &entries, solve_cx);
    }
    InstanceEvidence {
        proofs: EvidenceList::of(db, entries).with_rows(db, supplied.rows(db).clone()),
        refused,
    }
}

/// Whether two goals are one implementation's: the same trait at the same
/// arguments, after normalizing them in `scope`. Associated-type equalities and
/// the mode are not part of it, since a proof names no goal (`Evidence::Impl`).
fn same_identity<'db>(
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

/// The first concrete component of `layout`'s dictionaries, held in an
/// instance's `entries`, that the instance cannot give one dictionary: a
/// derivation that fails, two derivations of one component that differ, or two
/// components of one implementation that differ. A component derived from a
/// slot that is open, or that has no entry in an instance still generic, is not
/// compared.
fn component_agreement<'db>(
    db: &'db dyn HirAnalysisDb,
    layout: PredicateListId<'db>,
    goals: PredicateListId<'db>,
    closed: &[bool],
    entries: &[Option<EvidenceId<'db>>],
    solve_cx: TraitSolveCx<'db>,
) -> Option<RefusedGoal<'db>> {
    let table = concrete_components(db, layout);
    let assumptions = solve_cx.assumptions();
    let count = entries.len();
    let goal_at = |index: usize| goals.list(db)[index];
    // The slot the derivation that enters a member from `parent` starts from.
    let root_goal = |parent: usize| {
        goal_at(if parent < count {
            parent
        } else {
            table.members[parent - count].root as usize
        })
    };
    let mut values: Vec<Option<EvidenceId<'db>>> = vec![None; table.members.len()];
    // The dictionary a derivation entering `member` from `parent` gives.
    let derive = |values: &[Option<EvidenceId<'db>>],
                  member: &Component<'db>,
                  (parent, step): (u32, BoundStep<'db>)|
     -> Result<Option<EvidenceId<'db>>, Refusal<'db>> {
        let parent = parent as usize;
        let source = if parent < count {
            if !closed[parent] {
                return Ok(None);
            }
            match entries[parent] {
                Some(source) => source,
                None => {
                    return Err(Refusal::NoComponent {
                        root: Some(goal_at(parent)),
                    });
                }
            }
        } else {
            match values.get(parent - count).copied().flatten() {
                Some(source) => source,
                None => return Ok(None),
            }
        };
        match project_step(db, source, step, Some(member.goal), assumptions) {
            Projection::Proof(proof) => Ok(Some(proof)),
            Projection::Pending => Ok(None),
            Projection::Refused => Err(Refusal::NoComponent {
                root: Some(root_goal(parent)),
            }),
        }
    };
    let refuse = |member: &Component<'db>, reason| RefusedGoal {
        goal: member.goal,
        reason,
    };
    // Each member is read from the members before it, which hold the walk's
    // first derivation of it.
    for (position, member) in table.members.iter().enumerate() {
        if !member.concrete {
            continue;
        }
        let mut agreed: Option<(EvidenceId<'db>, u32)> = None;
        for &(parent, step) in &member.entering {
            if parent as usize >= count && (parent as usize - count) >= position {
                continue;
            }
            match derive(&values, member, (parent, step)) {
                Err(reason) => return Some(refuse(member, reason)),
                Ok(None) => {}
                Ok(Some(proof)) => match agreed {
                    Some((first, first_parent))
                        if !same_dictionary(db, first, proof, assumptions) =>
                    {
                        return Some(refuse(
                            member,
                            Refusal::Differs {
                                a: root_goal(first_parent as usize),
                                b: root_goal(parent as usize),
                            },
                        ));
                    }
                    Some(_) => {}
                    None => agreed = Some((proof, parent)),
                },
            }
        }
        values[position] = agreed.map(|(proof, _)| proof);
    }
    // A derivation from a member that comes later in the walk must agree too.
    for (position, member) in table.members.iter().enumerate() {
        let Some(value) = values[position] else {
            continue;
        };
        for &(parent, step) in &member.entering {
            if (parent as usize) < count || (parent as usize - count) < position {
                continue;
            }
            match derive(&values, member, (parent, step)) {
                Err(reason) => return Some(refuse(member, reason)),
                Ok(Some(proof)) if !same_dictionary(db, value, proof, assumptions) => {
                    return Some(refuse(
                        member,
                        Refusal::Differs {
                            a: root_goal(member.entering[0].0 as usize),
                            b: root_goal(parent as usize),
                        },
                    ));
                }
                Ok(_) => {}
            }
        }
    }
    // Components of one implementation hold one dictionary in an instance.
    let scope = solve_cx.origin_scope(db);
    for bucket in table.buckets() {
        let mut seen: Vec<(usize, EvidenceId<'db>)> = Vec::new();
        for &member in bucket {
            let member = member as usize;
            let Some(value) = values[member] else {
                continue;
            };
            let goal = table.members[member].goal;
            if let Some(&(first, other)) = seen
                .iter()
                .find(|&&(first, _)| same_identity(db, scope, table.members[first].goal, goal))
                && !same_dictionary(db, other, value, assumptions)
            {
                return Some(refuse(
                    &table.members[member],
                    Refusal::Differs {
                        a: root_goal(table.members[first].entering[0].0 as usize),
                        b: root_goal(table.members[member].entering[0].0 as usize),
                    },
                ));
            }
            seen.push((member, value));
        }
    }
    None
}

impl<'db> EvidenceId<'db> {
    /// The first component reference this proof still holds whose slot's goal
    /// is closed: a closed instance resolves every such reference where it binds
    /// its dictionaries, so one left over names a dictionary no supply held.
    pub(crate) fn unresolved_component(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<RefusedGoal<'db>> {
        match self.data(db) {
            Evidence::Assumption(slot) => {
                slot.component?;
                let root = *slot.applied.list(db).get(slot.index as usize)?;
                is_closed_goal(db, root).then_some(())?;
                Some(RefusedGoal {
                    goal: component_goal(db, *slot)?,
                    reason: Refusal::NoComponent { root: Some(root) },
                })
            }
            Evidence::Impl { conds, .. } => {
                conds.iter().find_map(|cond| cond.unresolved_component(db))
            }
            Evidence::DeferredSolCodec { bounds, .. } => {
                bounds.and_then(|bounds| bounds.entries.unresolved_component(db))
            }
        }
    }
}

impl<'db> EvidenceList<'db> {
    /// As `EvidenceId::unresolved_component`, over the entries and rows.
    pub(crate) fn unresolved_component(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<RefusedGoal<'db>> {
        self.entries(db)
            .iter()
            .flatten()
            .copied()
            .chain(self.rows(db).iter().filter_map(|row| row.proof))
            .find_map(|proof| proof.unresolved_component(db))
    }
}

impl<'db> UseEvidence<'db> {
    /// As `EvidenceId::unresolved_component`, over a use's proofs.
    pub(crate) fn unresolved_component(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<RefusedGoal<'db>> {
        self.dispatch_proof()
            .and_then(|proof| proof.unresolved_component(db))
            .or_else(|| {
                self.bounds
                    .and_then(|bounds| bounds.entries.unresolved_component(db))
            })
    }
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
    use crate::analysis::ty::trait_resolution::step_bound;
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
        let cx = TraitSolveCx::new(&db, module.scope());
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
        let edge = signature
            .implied_edges(&db)
            .iter()
            .find(|edge| edge.parent == 2 && edge.child == 3)
            .expect("the supertrait of the third bound is the fourth slot");
        let projected = project_step(&db, supplied, edge.step, Some(goal), deduplicated)
            .proof()
            .unwrap();
        let Evidence::Assumption(slot) = projected.data(&db) else {
            panic!("projected slot")
        };
        assert_eq!(slot.schema, schema);
        assert_eq!(
            slot.index, 3,
            "applied-list position must not address the declaration schema"
        );
    }

    #[test]
    fn every_derived_slot_has_an_entering_edge_that_reproduces_it() {
        for source in [
            r#"
trait Show { type K }
trait Tr { type K
    type A: Show<K = Self::K> }
fn pinned<T: Tr<K = u8>>() {}
fn chained<T: Tr>() {}
"#,
            r#"
trait Node { type Next: Node<Next = Self> }
fn node<T: Node>() {}
fn both<T: Node<Next = U>, U: Node<Next = T>>() {}
"#,
            r#"
trait Left { type Out: Right<Back = Self> }
trait Right { type Back: Left<Out = Self> }
fn only_left<T: Left>() {}
fn only_right<U: Right>() {}
fn call<T: Left<Out = U>, U: Right<Back = T>>() {}
"#,
            r#"
trait Top {}
trait L: Top {}
trait R: Top {}
trait Holds { type Base: L + R }
fn diamond<T: L + R>() {}
fn holds<H: Holds>() {}
"#,
        ] {
            let mut db = HirAnalysisTestDb::default();
            let file = db.new_stand_alone("edges.fe".into(), source);
            let (module, _) = db.top_mod(file);
            db.assert_no_diags(module);
            let mut checked = 0;
            for item in module.children_non_nested(&db) {
                let ItemKind::Func(func) = item else {
                    continue;
                };
                let declared = func_declared_bounds(&db, func, None);
                let signature = func_signature(&db, func, None);
                let slots = signature.list(&db);
                let edges = signature.implied_edges(&db);
                for (index, &bound) in slots.iter().enumerate().skip(declared.list(&db).len()) {
                    assert!(
                        edges.iter().any(|edge| edge.child as usize == index
                            && step_bound(&db, slots[edge.parent as usize], edge.step)
                                == Some(bound)),
                        "slot {index} of {source} has no entering edge that reproduces it"
                    );
                    checked += 1;
                }
            }
            assert!(checked > 0, "{source}");
        }
    }

    const PINNED: &str = r#"
trait Show { fn show() -> u256 }
struct Foo {}
impl Show for Foo { fn show() -> u256 { 100 } }
impl Show for Foo as Loud { fn show() -> u256 { 70 } }
struct W<T> {}
trait Tr { type A: Show }
impl<T: Show> Tr for W<T> { type A = T }
fn caller<B: Tr<A = Foo>>() {}
fn plain<B: Tr>() {}
"#;

    /// The type `name` applied to `args`, and the trait `name`, of a module.
    fn adt_ty<'db>(
        db: &'db HirAnalysisTestDb,
        module: crate::hir_def::TopLevelMod<'db>,
        name: &str,
        args: &[TyId<'db>],
    ) -> TyId<'db> {
        let adt = module
            .children_non_nested(db)
            .find_map(|item| match item {
                ItemKind::Struct(item)
                    if item
                        .name(db)
                        .to_opt()
                        .is_some_and(|found| found.data(db) == name) =>
                {
                    Some(item)
                }
                _ => None,
            })
            .expect("a struct");
        let adt = super::super::adt_def::AdtRef::Struct(adt).as_adt(db);
        TyId::foldl(db, TyId::adt(db, adt), args)
    }

    fn trait_named<'db>(
        db: &'db HirAnalysisTestDb,
        module: crate::hir_def::TopLevelMod<'db>,
        name: &str,
    ) -> crate::hir_def::Trait<'db> {
        module
            .children_non_nested(db)
            .find_map(|item| match item {
                ItemKind::Trait(item)
                    if item
                        .name(db)
                        .to_opt()
                        .is_some_and(|found| found.data(db) == name) =>
                {
                    Some(item)
                }
                _ => None,
            })
            .expect("a trait")
    }

    #[test]
    fn component_table_of_pinned_caller() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("pinned.fe".into(), PINNED);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = find_func(&db, module, "caller");
        let signature = func_signature(&db, func, None);
        assert_eq!(signature.list(&db).len(), 1);
        let table = concrete_components(&db, signature);
        assert_eq!(table.members.len(), 1);
        let member = &table.members[0];
        assert_eq!(member.goal.pretty_print(&db, true), "Foo: Show");
        assert!(member.concrete);
        assert_eq!(member.root, 0);
        assert!(matches!(
            member.path.as_slice(),
            [BoundStep::AssocBound { index: 0, .. }]
        ));
        // The dictionary of the slot projects it as a reference to that path.
        let schema =
            BoundSchemaId::new(&db, BoundOwner::Body(BodyOwner::func(&db, func)), signature);
        let projected = project_step(
            &db,
            schema.assumption(&db, 0),
            member.path[0],
            Some(member.goal),
            PredicateListId::empty_list(&db),
        )
        .proof()
        .expect("a component of the slot");
        let Evidence::Assumption(slot) = projected.data(&db) else {
            panic!("a reference")
        };
        assert_eq!((slot.schema, slot.index), (schema, 0));
        assert_eq!(slot.component_label(&db), ".A0");
    }

    /// A reference moved between schemas keeps its path and resolves to the
    /// component of the dictionary the last schema supplies.
    #[test]
    fn component_round_trip() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("pinned.fe".into(), PINNED);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = find_func(&db, module, "caller");
        let signature = func_signature(&db, func, None);
        let schema =
            BoundSchemaId::new(&db, BoundOwner::Body(BodyOwner::func(&db, func)), signature);
        let member = concrete_components(&db, signature).members[0].clone();
        let path = ComponentPath::new(&db, member.path.clone());
        let component = EvidenceId::new(
            &db,
            Evidence::Assumption(BoundSlotRef {
                schema,
                index: 0,
                applied: signature,
                component: Some(path),
            }),
        );
        let foo = adt_ty(&db, module, "Foo", &[]);
        let goal = TraitInstId::new_simple(
            &db,
            trait_named(&db, module, "Tr"),
            vec![adt_ty(&db, module, "W", &[foo])],
        );
        let dictionary =
            prove(&db, TraitSolveCx::new(&db, module.scope()), goal).expect("W<Foo> implements Tr");
        let expected = dictionary.conds(&db)[0];
        let supplied = BoundEvidence {
            schema,
            entries: EvidenceList::of(&db, vec![Some(dictionary)]),
        };
        assert_eq!(component.bind(&db, supplied), expected);
        assert_eq!(
            component.nominal(&db).bind(&db, supplied.nominal(&db)),
            expected
        );
        // A supply for a longer list that starts with the reference's list.
        let longer = PredicateListId::new(&db, vec![signature.list(&db)[0], member.goal]);
        let prefix = BoundEvidence {
            schema: BoundSchemaId::new(&db, schema.owner(&db), longer),
            entries: EvidenceList::of(&db, vec![Some(dictionary), None]),
        };
        assert_eq!(component.bind(&db, prefix), expected);
        // A supply that is itself a slot of another schema: the reference
        // moves there, with its path, and resolves when that schema is bound.
        let basis = BoundSchemaId::context(&db, signature);
        let middle = BoundEvidence {
            schema,
            entries: EvidenceList::of(&db, vec![Some(basis.assumption(&db, 0))]),
        };
        let moved = component.rebase(&db, middle).expect("a reference");
        let Evidence::Assumption(slot) = moved.data(&db) else {
            panic!("a reference")
        };
        assert_eq!((slot.schema, slot.index), (basis, 0));
        assert_eq!(slot.component, Some(path));
        let last = BoundEvidence {
            schema: basis,
            entries: EvidenceList::of(&db, vec![Some(dictionary)]),
        };
        assert_eq!(moved.rebase(&db, last), Some(expected));
    }

    #[test]
    fn component_table_empty_without_equalities() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("pinned.fe".into(), PINNED);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let signature = func_signature(&db, find_func(&db, module, "plain"), None);
        assert!(!has_concrete_components(&db, signature));
        assert!(concrete_components(&db, signature).members.is_empty());
    }

    /// A component reference that a closed instance leaves unresolved, such as
    /// one into a schema its dictionaries do not supply, is refused.
    #[test]
    fn component_unresolved_reference_refuses() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("pinned.fe".into(), PINNED);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = find_func(&db, module, "caller");
        let signature = func_signature(&db, func, None);
        let schema =
            BoundSchemaId::new(&db, BoundOwner::Body(BodyOwner::func(&db, func)), signature);
        let member = concrete_components(&db, signature).members[0].clone();
        let path = ComponentPath::new(&db, member.path.clone());
        let reference = |applied| {
            EvidenceId::new(
                &db,
                Evidence::Assumption(BoundSlotRef {
                    schema,
                    index: 0,
                    applied,
                    component: Some(path),
                }),
            )
        };
        let foo = adt_ty(&db, module, "Foo", &[]);
        let w_foo = adt_ty(&db, module, "W", &[foo]);
        let subst = CompleteSubst::for_owner(&db, func.into(), vec![w_foo]).unwrap();
        let closed = substitute_complete(&db, signature, &subst).unwrap();
        let refused = reference(closed)
            .unresolved_component(&db)
            .expect("a closed slot's reference is refused");
        assert_eq!(refused.goal.pretty_print(&db, true), "Foo: Show");
        assert_eq!(
            refused.reason,
            Refusal::NoComponent {
                root: Some(closed.list(&db)[0])
            }
        );
        // Under an implementation's conditions it is found as well.
        let goal = TraitInstId::new_simple(&db, trait_named(&db, module, "Tr"), vec![w_foo]);
        let dictionary =
            prove(&db, TraitSolveCx::new(&db, module.scope()), goal).expect("W<Foo> implements Tr");
        assert!(dictionary.unresolved_component(&db).is_none());
        let Evidence::Impl {
            selected,
            instantiated,
            ..
        } = dictionary.data(&db)
        else {
            panic!("an implementation")
        };
        let wrapped = EvidenceId::new(
            &db,
            Evidence::Impl {
                selected: *selected,
                instantiated: *instantiated,
                conds: vec![reference(closed)],
            },
        );
        assert_eq!(wrapped.unresolved_component(&db), Some(refused));
        // A root that is still open is left to the instance that closes it.
        assert!(reference(signature).unresolved_component(&db).is_none());
    }
}
