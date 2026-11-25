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
    closure::{is_function_item_callable_goal, settle_function_item_providers},
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
        answering_assumption, assumption_matcher, component_path_goal, concrete_components,
        constraint::collect_candidate_constraints, has_concrete_components,
        normalize_trait_inst_preserving_validity, same_identity, step_bound,
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
        Some(path) => component_path_goal(db, root, path.steps(db)),
    }
}

/// The bound that `step` derives from the bound `slot` names: one more step
/// along its path.
fn component_step_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    slot: BoundSlotRef<'db>,
    step: BoundStep<'db>,
) -> Option<TraitInstId<'db>> {
    let root = *slot
        .schema
        .basis_slots(db)
        .list(db)
        .get(slot.index as usize)?;
    let mut steps = slot
        .component
        .map_or_else(Vec::new, |path| path.steps(db).clone());
    steps.push(step);
    component_path_goal(db, root, &steps)
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
        let root_goal = *slot
            .schema
            .basis_slots(db)
            .list(db)
            .get(slot.index as usize)?;
        let mut proof = root;
        for (position, &step) in path.steps(db).iter().enumerate() {
            let goal = component_path_goal(db, root_goal, &path.steps(db)[..=position])?;
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
    /// The goals of the components that follow the slots in `entries`, in
    /// entry order, as the slots stood at capture. The slots can be rewritten
    /// afterwards (`source_conditions`), which the table of the rewritten list
    /// cannot follow, so the entries are addressed by these goals.
    component_goals: PredicateListId<'db>,
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
        let table = concrete_components(db, slots);
        // The components of the slots' dictionaries follow the slots, in the
        // table's order, so a goal a component answers reads the entry the
        // projection of its path gives, after the instance binds them.
        let entries = EvidenceList::of(
            db,
            (0..slots.list(db).len())
                .map(|index| Some(schema.assumption(db, index)))
                .chain(table.components().map(|member| {
                    schema.slot(db, member.root as usize).map(|slot| {
                        EvidenceId::new(
                            db,
                            Evidence::Assumption(BoundSlotRef {
                                component: Some(ComponentPath::new(db, member.path.clone())),
                                ..slot
                            }),
                        )
                    })
                }))
                .collect(),
        );
        Self {
            source_goal,
            source_choices: cx.choices(),
            source_slots: slots,
            applied_slots: slots,
            component_goals: PredicateListId::new(
                db,
                table
                    .components()
                    .map(|member| member.goal)
                    .collect::<Vec<_>>(),
            ),
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
        // A closed goal that a component of the captured dictionaries answers.
        if let Some(proof) = self.component_proof(db, cx) {
            return Ok(SourceProof::Bound(proof));
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

    /// The entry of the component of the captured slots that is the same
    /// implementation as the source goal.
    fn component_proof(
        self,
        db: &'db dyn HirAnalysisDb,
        cx: TraitSolveCx<'db>,
    ) -> Option<EvidenceId<'db>> {
        let scope = cx.normalization_scope_for_trait_inst(db, self.source_goal);
        let goal = closed_component_goal(db, self.source_slots, scope, self.source_goal)?;
        let position = self
            .component_goals
            .list(db)
            .iter()
            .position(|&component| same_identity(db, scope, component, goal))?;
        self.entries
            .get(db, self.source_slots.list(db).len() + position)
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
    ///
    /// The callable implementation of a function item (`ImplementorOrigin::Callable`)
    /// has the proofs of the function's own bounds as conditions, except those
    /// on the providers of its effects. Where the item is passed, a provider
    /// that is still open is a placeholder (`()`) in the goal proved
    /// (`settle_function_item_providers`): the proof does not depend on the
    /// provider, and the call or the row that supplies it makes its own proof.
    /// No condition may name a provider, and nothing may read the provider
    /// positions of the proved goal's function item type as a type.
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
                        None,
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

/// Whether a dictionary holds a named implementation. The one test that decides
/// whether a refusal that turns on the contents of dictionaries applies: a
/// component that cannot be read from one (`Refusal::NoComponent`) or a walk
/// that was cut (`Refusal::TooDeep`). Where no dictionary a check involves
/// holds one, the program names no implementation the check could contradict,
/// and the slot keeps the proof ordinary resolution gives it.
fn names_impl<'db>(db: &'db dyn HirAnalysisDb, proof: Option<EvidenceId<'db>>) -> bool {
    proof.is_some_and(|proof| proof.has_named_impl(db))
}

/// Whether the dictionary of the closed slot `slot` holds a named
/// implementation.
fn slot_names_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    closed: &[bool],
    entries: &[Option<EvidenceId<'db>>],
    slot: usize,
) -> bool {
    closed[slot] && names_impl(db, entries[slot])
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
    let goal = closed_component_goal(db, slots, scope, goal)?;
    let (_, member) = concrete_components(db, slots).answering(db, scope, goal)?;
    Some(BoundSlotRef {
        component: Some(ComponentPath::new(db, member.path.clone())),
        ..schema.slot(db, member.root as usize)?
    })
}

/// The closed goal that a component of `slots` can answer, which `goal` is or
/// gives. A goal written on a projection that a slot's equality settles,
/// `<B::A as Show>` under `B: Tr<A = Foo>`, is the closed goal it gives. Every
/// reader of a component takes its goal from here.
fn closed_component_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    slots: PredicateListId<'db>,
    scope: ScopeId<'db>,
    goal: TraitInstId<'db>,
) -> Option<TraitInstId<'db>> {
    if !has_concrete_components(db, slots) {
        return None;
    }
    let goal = if collect_flags(db, goal).contains(TyFlags::HAS_PROJECTION) {
        normalize_trait_inst_preserving_validity(db, goal, scope, slots)
    } else {
        goal
    };
    is_closed_goal(db, goal).then_some(goal)
}

pub(crate) fn prove_outcome<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> ProofOutcome<'db> {
    let dict = solve_cx.assumptions().extend_all_bounds(db);
    prove_outcome_in(
        db,
        solve_cx,
        goal,
        has_concrete_components(db, dict).then_some(dict),
    )
}

/// As `prove_outcome`, for a goal that is a condition of an implementation
/// selected in a body whose dictionaries hold components: `solve_cx` is the
/// context selection uses, which a closed goal strips of its assumptions, and
/// `dict` is the extended list of the body's slots, which still answers a goal
/// that one of its concrete components is.
fn prove_outcome_in<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
    dict: Option<PredicateListId<'db>>,
) -> ProofOutcome<'db> {
    // Only the callable goal of a function item has hidden provider arguments
    // that a call or a row supplies later.
    let goal = if is_function_item_callable_goal(db, goal) {
        settle_function_item_providers(db, goal)
    } else {
        goal
    };
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
            dict.unwrap_or_else(|| solve_cx.assumptions().extend_all_bounds(db)),
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
    prove_in(db, solve_cx, goal, None, dict)
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
    /// The goal is a component that two conditions of the implementation
    /// selected for a bound hold from different dictionaries: `a` and `b` are
    /// the conditions, as the implementation states them at the call's types.
    ConditionsDiffer {
        a: TraitInstId<'db>,
        b: TraitInstId<'db>,
    },
    /// The goal lies below the limit to which the bounds of associated types
    /// are followed in the dictionary of `root`, and that dictionary holds a
    /// named implementation: no value is given where the walk cannot say
    /// whether they read it.
    TooDeep { root: TraitInstId<'db> },
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
            // Without a lexical choice, a closed goal that no slot answers is
            // left to the instance, unless the dictionaries hold components:
            // a component answers the goal, or a condition of the
            // implementation it selects, and only the body's own context
            // reads them. Only a use under a choice reports a refused slot.
            if direct
                && is_closed_goal(db, goal)
                && solve_cx.choices().is_none()
                && !has_concrete_components(db, assumptions)
                && !assumptions.list(db).contains(&goal)
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
                Projection::ConditionsDiffer { a, b } => {
                    refuse = Some(Refusal::ConditionsDiffer { a, b });
                }
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
    /// Two conditions of a selected implementation hold the component from
    /// different dictionaries; `a` and `b` are the conditions' goals.
    ConditionsDiffer {
        a: TraitInstId<'db>,
        b: TraitInstId<'db>,
    },
}

impl<'db> Projection<'db> {
    pub(crate) fn proof(self) -> Option<EvidenceId<'db>> {
        match self {
            Self::Proof(proof) => Some(proof),
            Self::Pending | Self::Refused | Self::ConditionsDiffer { .. } => None,
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
    /// As `Own`, but the walk of a condition stopped at its limit, so the
    /// bound may be held below it. A closed bound only.
    Truncated,
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
    let mut truncated = false;
    for (index, &condition) in basis.list(db).iter().enumerate() {
        let walk = PredicateListId::new(db, vec![condition]).bound_walk_keeping_concrete(db);
        truncated |= walk.is_truncated();
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
    if !held.is_empty() {
        ImplComponent::Held(held)
    } else if truncated && closed {
        ImplComponent::Truncated
    } else {
        ImplComponent::Own
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
            let Some(child) = component_step_goal(db, *slot, step) else {
                return Projection::Pending;
            };
            // The child is the canonical member of its identity, the one every
            // other reader of that identity finds.
            let scope = TraitSolveCx::normalization_scope_for_trait_inst_with_origin(
                db,
                child.def(db).scope().top_mod(db).ingot(db),
                child,
            );
            return concrete_components(db, slots)
                .answering(db, scope, child)
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
    let component = impl_component(db, *selected, declared);
    // A dictionary that holds a named implementation is not read where the
    // walk of its conditions stopped before the bound: it is refused.
    if matches!(component, ImplComponent::Truncated) && proof.has_named_impl(db) {
        return Projection::Refused;
    }
    match component {
        ImplComponent::Held(held) => {
            let mut agreed: Option<(EvidenceId<'db>, u32)> = None;
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
                    Projection::Proof(component) => match agreed {
                        Some((other, first))
                            if !same_dictionary(db, other, component, assumptions) =>
                        {
                            // The conditions, as the selected implementation
                            // states them at the instantiated arguments.
                            let conditions = instantiated.constraints(db);
                            return match (
                                conditions.list(db).get(first as usize),
                                conditions.list(db).get(*condition as usize),
                            ) {
                                (Some(&a), Some(&b)) => Projection::ConditionsDiffer { a, b },
                                _ => Projection::Refused,
                            };
                        }
                        Some(_) => {}
                        None => agreed = Some((component, *condition)),
                    },
                    Projection::Pending => pending = true,
                    Projection::Refused | Projection::ConditionsDiffer { .. } => return current,
                }
            }
            match (pending, agreed) {
                (true, _) => Projection::Pending,
                (false, Some((agreed, _))) => Projection::Proof(agreed),
                (false, None) => Projection::Refused,
            }
        }
        ImplComponent::Own | ImplComponent::Truncated => {
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
    dict: Option<PredicateListId<'db>>,
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
    prove_selected(
        db,
        solve_cx,
        goal,
        bounds,
        source_conditions,
        resolved,
        dict,
    )
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
        match prove_selected(db, solve_cx, goal, Some(bounds), None, candidate, None) {
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
    dict: Option<PredicateListId<'db>>,
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
                                        dict,
                                    )
                                    .evidence();
                                }
                            }
                            let cx = if bounds.is_some() {
                                solve_cx.with_assumptions(PredicateListId::empty_list(db))
                            } else {
                                solve_cx
                            };
                            prove_outcome_in(db, cx, cond, dict).evidence()
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

/// The evidence an instance runs with, and the goals whose dictionaries refuse
/// it, the first of them first. A refused instance is rejected where it is
/// admitted; no slot is proved ordinarily in its place.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) struct InstanceEvidence<'db> {
    pub(crate) proofs: EvidenceList<'db>,
    pub(crate) refused: Option<RefusedGoal<'db>>,
}

/// The evidence an instance runs with: its key's proofs, `supplied`, of the
/// assumptions its body is checked under, instantiated as `goals`. `declared`
/// is the body's slot layout in declaration coordinates, whose first
/// `goals.len()` slots the goals instantiate. The proofs are the received
/// proofs of the instance, and `received` fills and checks them.
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
    // A closed supplied proof of a closed goal, or any proof of an open one.
    let own: Vec<Option<EvidenceId<'db>>> = (0..count)
        .map(|index| {
            let proof = supplied.get(db, index)?;
            let closed = is_closed_goal(db, goals.list(db)[index]);
            if proof.has_deferred(db) {
                // A selected parent is authoritative even if one retained
                // obligation cannot close. Never replace that tree.
                // Known gap: the completed proof is not compared with ordinary
                // normalization (`proof_output_mismatch`), as the proofs
                // completed in `entry_codec_callees` are; see
                // `semantic_callee_key_with_assumptions`.
                complete_deferred_evidence(db, proof)
                    .filter(|completed| !closed || completed.is_closed(db))
            } else {
                (!closed || proof.is_closed(db)).then_some(proof)
            }
        })
        .collect();
    let received = received(
        db,
        layout,
        goals,
        &own,
        supplied.rows(db),
        solve_cx,
        Fallback::Instance,
    );
    InstanceEvidence {
        proofs: EvidenceList::of(db, received.entries).with_rows(db, supplied.rows(db).clone()),
        refused: received.refused.first().copied(),
    }
}

/// What a missing dictionary means to the caller of `received`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Fallback {
    /// A closed goal that no route reaches and no proof was supplied for is
    /// proved in the instance's own context. A projection is preferred to the
    /// supplied proof as the entry.
    Instance,
}

/// The proofs a body receives, one dictionary for each goal of the body.
pub(crate) struct Received<'db> {
    /// The dictionary of each slot; `None` for a slot with none yet or refused.
    pub(crate) entries: Vec<Option<EvidenceId<'db>>>,
    /// The dictionary of each member of `concrete_components(layout)`.
    pub(crate) members: Vec<Option<EvidenceId<'db>>>,
    /// The goals whose routes refuse the instance, the first found first.
    pub(crate) refused: Vec<RefusedGoal<'db>>,
}

/// Record a refused goal once, in the order the routes were followed.
fn refuse_goal<'db>(
    refused: &mut Vec<RefusedGoal<'db>>,
    goal: TraitInstId<'db>,
    reason: Refusal<'db>,
) {
    if !refused.iter().any(|known| known.goal == goal) {
        refused.push(RefusedGoal { goal, reason });
    }
}

/// The one agreement check. The proofs a body receives are a map from each of
/// its goals to a dictionary, and the map is a function: every route that
/// reaches a goal gives one dictionary. A goal of the body is a slot of
/// `layout` (the declared bounds and the bounds they imply, instantiated as
/// `goals`), or a component, a concrete bound that the slots' dictionaries
/// hold and that is no slot (`concrete_components`). Two goals are one goal
/// when they have one identity (`same_identity`) in the coordinates of the
/// declaration, so bounds that the signature does not identify stay separate
/// even where an instantiation makes them equal. A route is a slot followed by
/// the derivation steps the walk of the layout recorded.
///
/// `own` is the proof supplied for each slot. A slot with a derivation entering
/// it takes the dictionary that derivation gives (`project_step`); a root keeps
/// its supplied proof, takes the component that slots deriving it from later
/// positions give, or is proved in `cx`.
///
/// Two dictionaries agree when `same_dictionary` says so, or when neither
/// selects a named implementation at any depth: two proofs of one closed goal
/// that select only ordinary implementations select the same implementations.
/// A refusal needs a named implementation, so when no proof supplied for the
/// body selects one (the gate), only the slots are filled and nothing is
/// compared. A route that cannot be followed refuses only when a dictionary on
/// it names an implementation; a route cut at the walk's depth or member limit
/// refuses on the same condition.
pub(crate) fn received<'db>(
    db: &'db dyn HirAnalysisDb,
    layout: PredicateListId<'db>,
    goals: PredicateListId<'db>,
    own: &[Option<EvidenceId<'db>>],
    rows: &[RowEvidence<'db>],
    cx: TraitSolveCx<'db>,
    fallback: Fallback,
) -> Received<'db> {
    let Fallback::Instance = fallback;
    let count = goals.list(db).len();
    let edges = layout.implied_edges(db);
    let assumptions = cx.assumptions();
    let goal_at = |index: usize| goals.list(db)[index];
    let closed: Vec<bool> = (0..count)
        .map(|index| is_closed_goal(db, goal_at(index)))
        .collect();
    let agree = |a: EvidenceId<'db>, b: EvidenceId<'db>| {
        same_dictionary(db, a, b, assumptions) || (!a.has_named_impl(db) && !b.has_named_impl(db))
    };
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
    let mut refused: Vec<RefusedGoal<'db>> = Vec::new();
    // Pass 1: the slots, in walk order.
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
                .find(|&&(_, other)| !agree(first, other))
                .map(|&(parent, _)| goal_at(parent))
                .or_else(|| {
                    own[index]
                        .filter(|&other| !agree(first, other))
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
                None => entries[index] = prove(db, cx, goal_at(index)),
                Some((parent, None)) => refusal = Some(no_component(parent)),
                Some((first_parent, Some(first))) => {
                    refusal = later.find_map(|(parent, other)| match other {
                        None => Some(no_component(parent)),
                        Some(other) if !agree(first, other) => Some(Refusal::Differs {
                            a: goal_at(first_parent),
                            b: goal_at(parent),
                        }),
                        Some(_) => None,
                    });
                    entries[index] = refusal.is_none().then_some(first);
                }
            }
        }
        // A parent with no dictionary, or one that gives no component, refuses
        // the slot only where a dictionary involved holds a named
        // implementation: the slot's own proof, or the dictionary of a slot
        // that implies it. Otherwise the slot keeps the proof it would have
        // had without implied-bound agreement, and a goal no proof answers is
        // reported where it is used.
        let ordinary = matches!(refusal, Some(Refusal::NoComponent { .. })) && {
            let implying = edges
                .iter()
                .filter(|edge| edge.child as usize == index)
                .map(|edge| edge.parent as usize)
                .filter(|&parent| parent != index && closed[parent]);
            !names_impl(db, own[index])
                && !implying.into_iter().any(|parent| {
                    let dictionary = if parent < index {
                        entries[parent]
                    } else {
                        own[parent]
                    };
                    names_impl(db, dictionary)
                })
        };
        if ordinary {
            entries[index] = own[index].or_else(|| prove(db, cx, goal_at(index)));
        } else if let Some(reason) = refusal {
            entries[index] = None;
            refuse_goal(&mut refused, goal_at(index), reason);
        }
    }
    // The gate. A refusal needs a named implementation in a dictionary the
    // instance received: where there is none, the slots are all there is.
    let active = own.iter().any(|&proof| names_impl(db, proof))
        || rows.iter().any(|row| names_impl(db, row.proof));
    if !active {
        return Received {
            entries,
            members: Vec::new(),
            refused,
        };
    }
    // Pass 2: every derivation whose two ends are closed. Slots that imply
    // one another hold one implementation each: every slot is what the
    // dictionary of each slot that implies it projects for it.
    for edge in edges {
        let (parent, child) = (edge.parent as usize, edge.child as usize);
        if parent == child || !closed[parent] || !closed[child] {
            continue;
        }
        let (Some(source), Some(entry)) = (entries[parent], entries[child]) else {
            continue;
        };
        let reason = match project(source, edge.step, child) {
            None if names_impl(db, Some(source)) || names_impl(db, Some(entry)) => {
                Some(no_component(parent))
            }
            None => None,
            Some(component) if !agree(component, entry) => Some(Refusal::Differs {
                a: goal_at(parent),
                b: goal_at(child),
            }),
            Some(_) => None,
        };
        if let Some(reason) = reason {
            entries[child] = None;
            refuse_goal(&mut refused, goal_at(child), reason);
        }
    }
    // The concrete bounds the slots' dictionaries hold are fields of those
    // dictionaries as well. Each derivation of one gives it a dictionary, and
    // they must all be one. Members are read only from slots that were not
    // refused.
    let table = concrete_components(db, layout);
    let mut values: Vec<Option<EvidenceId<'db>>> = vec![None; table.members.len()];
    if refused.is_empty() {
        // The slot a derivation entering `parent` starts from.
        let root_of = |parent: usize| {
            if parent < count {
                parent
            } else {
                table.members[parent - count].root as usize
            }
        };
        let root_goal = |parent: usize| goal_at(root_of(parent));
        // The slots whose dictionaries `member` is read from: the root of its
        // walk and the root of each derivation that enters it.
        let roots = |member: &Component<'db>| {
            std::iter::once(member.root as usize)
                .chain(
                    member
                        .entering
                        .iter()
                        .map(|&(parent, _)| root_of(parent as usize)),
                )
                .collect::<Vec<_>>()
        };
        let named = |slot: usize| slot_names_impl(db, &closed, &entries, slot);
        // A derivation that finds no dictionary or no component refuses only a
        // member read from a dictionary that holds a named implementation.
        let applies = |member: &Component<'db>, reason: &Refusal<'db>| {
            !matches!(reason, Refusal::NoComponent { .. }) || roots(member).into_iter().any(named)
        };
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
                Projection::Refused | Projection::ConditionsDiffer { .. } => {
                    Err(Refusal::NoComponent {
                        root: Some(root_goal(parent)),
                    })
                }
            }
        };
        // Each member is read from the members before it, which hold the
        // walk's first derivation of it.
        for (position, member) in table.members.iter().enumerate() {
            if !member.concrete {
                continue;
            }
            let mut agreed: Option<(EvidenceId<'db>, u32)> = None;
            let mut failed = false;
            for &(parent, step) in &member.entering {
                if parent as usize >= count && (parent as usize - count) >= position {
                    continue;
                }
                match derive(&values, member, (parent, step)) {
                    Err(reason) if applies(member, &reason) => {
                        refuse_goal(&mut refused, member.goal, reason);
                        failed = true;
                        break;
                    }
                    Err(_) | Ok(None) => {}
                    Ok(Some(proof)) => match agreed {
                        Some((first, first_parent)) if !agree(first, proof) => {
                            refuse_goal(
                                &mut refused,
                                member.goal,
                                Refusal::Differs {
                                    a: root_goal(first_parent as usize),
                                    b: root_goal(parent as usize),
                                },
                            );
                            failed = true;
                            break;
                        }
                        Some(_) => {}
                        None => agreed = Some((proof, parent)),
                    },
                }
            }
            values[position] = if failed {
                None
            } else {
                agreed.map(|(proof, _)| proof)
            };
        }
        // A derivation from a member that comes later in the walk must agree
        // too.
        for (position, member) in table.members.iter().enumerate() {
            let Some(value) = values[position] else {
                continue;
            };
            for &(parent, step) in &member.entering {
                if (parent as usize) < count || (parent as usize - count) < position {
                    continue;
                }
                match derive(&values, member, (parent, step)) {
                    Err(reason) if applies(member, &reason) => {
                        refuse_goal(&mut refused, member.goal, reason);
                    }
                    Ok(Some(proof)) if !agree(value, proof) => {
                        refuse_goal(
                            &mut refused,
                            member.goal,
                            Refusal::Differs {
                                a: root_goal(member.entering[0].0 as usize),
                                b: root_goal(parent as usize),
                            },
                        );
                    }
                    Err(_) | Ok(_) => {}
                }
            }
        }
        // A walk that stopped at its limit left bounds below its frontier
        // unread: a dictionary that holds a named implementation is refused,
        // not given a value that mixes the component it holds with an
        // ordinary proof. Every member that was cut is checked, with the root
        // of each derivation that enters it, so the answer does not depend on
        // which bound comes first; a walk that reached its member limit cut
        // every member.
        for (position, member) in table.members.iter().enumerate() {
            if !(member.truncated || table.capped) {
                continue;
            }
            let root = roots(member).into_iter().find(|&root| named(root));
            let value_named = names_impl(db, values[position]);
            if let Some(root) = root.or_else(|| value_named.then_some(member.root as usize)) {
                refuse_goal(
                    &mut refused,
                    member.goal,
                    Refusal::TooDeep {
                        root: goal_at(root),
                    },
                );
            }
        }
    }
    // Pass 3: one dictionary for each goal. The slots and the members of one
    // trait are compared by the identity of their goals in the coordinates of
    // the declaration, so two bounds of the signature that are one goal after
    // normalization hold one dictionary, however the walk reached them.
    let scope = cx.origin_scope(db);
    let declared_goal = |node: usize| {
        if node < count {
            layout.list(db)[node]
        } else {
            table.members[node - count].goal
        }
    };
    let value_of = |node: usize, entries: &[Option<EvidenceId<'db>>]| {
        if node < count {
            closed[node].then(|| entries[node]).flatten()
        } else {
            values[node - count]
        }
    };
    let mut by_trait: Vec<(crate::hir_def::Trait<'db>, Vec<usize>)> = Vec::new();
    for node in 0..count + table.members.len() {
        if value_of(node, &entries).is_none() {
            continue;
        }
        let trait_ = declared_goal(node).def(db);
        match by_trait.iter_mut().find(|(known, _)| *known == trait_) {
            Some((_, nodes)) => nodes.push(node),
            None => by_trait.push((trait_, vec![node])),
        }
    }
    let root_slot = |node: usize| {
        if node < count {
            node
        } else {
            table.members[node - count].root as usize
        }
    };
    for (_, nodes) in &by_trait {
        let mut seen: Vec<usize> = Vec::new();
        for &node in nodes {
            let Some(value) = value_of(node, &entries) else {
                continue;
            };
            let goal = declared_goal(node);
            if let Some(&first) = seen
                .iter()
                .find(|&&first| same_identity(db, scope, declared_goal(first), goal))
                && let Some(other) = value_of(first, &entries)
                && !agree(other, value)
            {
                let at = if node < count { goal_at(node) } else { goal };
                refuse_goal(
                    &mut refused,
                    at,
                    Refusal::Differs {
                        a: goal_at(root_slot(first)),
                        b: goal_at(root_slot(node)),
                    },
                );
                if node < count {
                    entries[node] = None;
                }
                continue;
            }
            seen.push(node);
        }
    }
    Received {
        entries,
        members: values,
        refused,
    }
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
    _dict: Option<PredicateListId<'db>>,
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
                prove_in(&db, cx, goal, Some(bounds), None)
                    .evidence()
                    .is_some(),
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

    /// A closed slot whose implying slot has no dictionary is refused by the
    /// instance only where a dictionary involved holds a named implementation.
    /// `Item: Sub` has no implementation, so the instance of `ctx<Item>` has no
    /// dictionary to read `Item: Base` from. A supplied proof of `Item: Base`
    /// that is the ordinary implementation leaves the slot with that proof, as
    /// before implied bounds were read from dictionaries; the named
    /// implementation refuses it.
    #[test]
    fn instance_missing_dictionary_refuses_only_with_a_named_implementation() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "missing_dictionary.fe".into(),
            r#"
trait Base {}
trait Sub: Base {}
struct Item {}
impl Base for Item {}
impl Base for Item as Named {}
fn ctx<V: Sub>() {}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = find_func(&db, module, "ctx");
        let signature = func_signature(&db, func, None);
        assert_eq!(signature.list(&db).len(), 2);
        let item = adt_ty(&db, module, "Item", &[]);
        let subst = CompleteSubst::for_owner(&db, func.into(), vec![item]).unwrap();
        let goals = substitute_complete(&db, signature, &subst).unwrap();
        let cx = TraitSolveCx::new(&db, module.scope());
        let base = goals.list(&db)[1];
        let named = module
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::ImplTrait(implementation)
                    if implementation.selection_name(&db).is_some() =>
                {
                    lower_impl_trait(&db, implementation)
                }
                _ => None,
            })
            .expect("the named implementation");
        let chosen = ChosenImpls::push(&db, None, named);
        let ordinary_proof = prove(&db, cx, base).expect("the ordinary implementation");
        let named_proof =
            prove(&db, cx.with_choices(Some(chosen)), base).expect("the named implementation");
        assert!(!ordinary_proof.has_named_impl(&db));
        assert!(named_proof.has_named_impl(&db));
        let instance = |proof| {
            instance_evidence(
                &db,
                goals,
                signature,
                EvidenceList::of(&db, vec![None, Some(proof)]),
                cx,
            )
        };
        let ordinary = instance(ordinary_proof);
        assert_eq!(ordinary.refused, None);
        assert_eq!(ordinary.proofs.get(&db, 1), Some(ordinary_proof));
        let refused = instance(named_proof)
            .refused
            .expect("a named implementation is refused");
        assert_eq!(refused.goal, base);
        assert_eq!(
            refused.reason,
            Refusal::NoComponent {
                root: Some(goals.list(&db)[0])
            }
        );
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

    /// A row goal that differs from the slot's goal by a concrete component has
    /// no derivation the plain walk of the root records: it is refused, never
    /// read from an ordinary proof.
    #[test]
    fn component_row_goal_is_refused() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("pinned.fe".into(), PINNED);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = find_func(&db, module, "caller");
        let signature = func_signature(&db, func, None);
        let root = signature.list(&db)[0];
        let member = concrete_components(&db, signature).members[0].clone();
        let foo = adt_ty(&db, module, "Foo", &[]);
        let w_foo = adt_ty(&db, module, "W", &[foo]);
        let goal = TraitInstId::new_simple(&db, trait_named(&db, module, "Tr"), vec![w_foo]);
        let dictionary =
            prove(&db, TraitSolveCx::new(&db, module.scope()), goal).expect("W<Foo> implements Tr");
        assert_eq!(
            implied_bound_evidence(
                &db,
                dictionary,
                root,
                member.goal,
                module.scope(),
                PredicateListId::empty_list(&db),
            ),
            Projection::Refused
        );
    }

    /// A component goal of a deferred capture is found in the goals recorded
    /// when the capture was made. A header that unifies two parameters of the
    /// source slots collapses them into one, which the walk of the rewritten
    /// list cannot lay out, so the table it would give is empty.
    #[test]
    fn deferred_capture_finds_component_after_aliasing() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "aliased.fe".into(),
            r#"
trait Show {}
struct Foo {}
impl Show for Foo {}
struct W<T> {}
trait Tr { type A: Show }
impl<T: Show> Tr for W<T> { type A = T }
struct Pair<T, U> {}
trait Tx {}
impl<T: Tr<A = Foo>> Tx for Pair<T, T> {}
fn ctx<B: Tr<A = Foo>, C: Tr<A = Foo>>() {}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let signature = func_signature(&db, find_func(&db, module, "ctx"), None);
        let (b, c) = (
            signature.list(&db)[0].self_ty(&db),
            signature.list(&db)[1].self_ty(&db),
        );
        let cx = TraitSolveCx::new(&db, module.scope()).with_assumptions(signature);
        let tx = trait_named(&db, module, "Tx");
        let source = TraitInstId::new_simple(&db, tx, vec![adt_ty(&db, module, "Pair", &[b, c])]);
        let bounds = DeferredProofBounds::capture(&db, source, cx);
        // Two slots, then the one component `Foo: Show` they both derive.
        assert_eq!(bounds.source_slots.list(&db).len(), 2);
        let foo = adt_ty(&db, module, "Foo", &[]);
        let show = TraitInstId::new_simple(&db, trait_named(&db, module, "Show"), vec![foo]);
        let component = bounds
            .entries
            .get(&db, 2)
            .expect("an entry for the component");
        assert_eq!(bounds.entries.get(&db, 3), None);
        assert_eq!(
            bounds.component_proof(&db, cx),
            None,
            "a goal that no component answers"
        );
        let closed = DeferredProofBounds {
            source_goal: show,
            ..bounds
        };
        assert_eq!(closed.component_proof(&db, cx), Some(component));
        // The header of `Pair<T, T>` makes the two parameters one.
        let w_foo = adt_ty(&db, module, "W", &[foo]);
        let goal =
            TraitInstId::new_simple(&db, tx, vec![adt_ty(&db, module, "Pair", &[w_foo, w_foo])]);
        let Selection::Unique(resolved) =
            resolve_trait_impl_instance(&db, TraitSolveCx::new(&db, module.scope()), goal)
        else {
            panic!("Pair<W<Foo>, W<Foo>> implements Tx")
        };
        let (_, aliased) = source_conditions(&db, resolved.selected(), bounds)
            .expect("the header unifies with the source goal");
        assert_eq!(aliased.source_slots.list(&db).len(), 2);
        assert_eq!(
            aliased.source_slots.list(&db)[0],
            aliased.source_slots.list(&db)[1],
            "the two parameters are one after the header is applied"
        );
        let closed = DeferredProofBounds {
            source_goal: show,
            ..aliased
        };
        assert_eq!(closed.component_proof(&db, cx), Some(component));
    }

    /// A bound that an associated type declares, below a concrete bound, is
    /// fixed by the implementation that defines the associated type: `Foo: Show`
    /// lies two levels below `C: Tr2<A = U<Foo>>`, through `U<Foo>: Tr`.
    #[test]
    fn component_through_implementation_type_is_concrete() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "through.fe".into(),
            r#"
trait Show { fn show() -> u256 }
struct Foo {}
impl Show for Foo { fn show() -> u256 { 100 } }
struct U<T> {}
trait Tr { type A: Show }
impl<T: Show> Tr for U<T> { type A = T }
trait Tr2 { type A: Tr }
fn caller<C: Tr2<A = U<Foo>>>() {}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let signature = func_signature(&db, find_func(&db, module, "caller"), None);
        assert_eq!(signature.list(&db).len(), 1);
        let table = concrete_components(&db, signature);
        let member = table
            .members
            .iter()
            .find(|member| member.concrete && member.goal.pretty_print(&db, true) == "Foo: Show")
            .expect("`Foo: Show` is a component");
        assert_eq!(member.root, 0);
        assert!(matches!(
            member.path.as_slice(),
            [
                BoundStep::AssocBound { index: 0, .. },
                BoundStep::AssocBound { index: 0, .. }
            ]
        ));
    }

    /// Where no proof supplied to an instance selects a named implementation,
    /// nothing is compared: two ordinary proofs of one goal that differ only in
    /// structure give no refusal. A named proof against an ordinary one of the
    /// same goal does.
    #[test]
    fn received_gate_skips_ordinary_instances() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "gate.fe".into(),
            r#"
trait Left { type Out: Right
    fn l() -> u256 }
trait Right { type Back: Left
    fn r() -> u256 }
struct A {}
struct B {}
impl Left for A { type Out = B
    fn l() -> u256 { 1 } }
impl Left for A as BigA { type Out = B
    fn l() -> u256 { 3 } }
impl Right for B { type Back = A
    fn r() -> u256 { 2 } }
fn ctx<T: Left<Out = U>, U: Right<Back = T>>() {}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = find_func(&db, module, "ctx");
        let signature = func_signature(&db, func, None);
        let a = adt_ty(&db, module, "A", &[]);
        let b = adt_ty(&db, module, "B", &[]);
        let subst = CompleteSubst::for_owner(&db, func.into(), vec![a, b]).unwrap();
        let goals = substitute_complete(&db, signature, &subst).unwrap();
        let cx = TraitSolveCx::new(&db, module.scope());
        let named = module
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::ImplTrait(implementation)
                    if implementation.selection_name(&db).is_some() =>
                {
                    lower_impl_trait(&db, implementation)
                }
                _ => None,
            })
            .expect("the named implementation");
        let chosen = Some(ChosenImpls::push(&db, None, named));
        // `T: Left<Out = U>` and the `T: Left` that `U: Right<Back = T>`
        // implies are one goal of the body.
        let first = goals
            .list(&db)
            .iter()
            .position(|goal| goal.pretty_print(&db, true) == "A: Left<Out = B>")
            .expect("the declared bound");
        let second = goals
            .list(&db)
            .iter()
            .position(|goal| goal.pretty_print(&db, true) == "A: Left")
            .expect("the implied bound");
        let ordinary = |index: usize| prove(&db, cx, goals.list(&db)[index]).expect("ordinary");
        let named_proof = prove(&db, cx.with_choices(chosen), goals.list(&db)[first])
            .expect("the named implementation");
        assert!(named_proof.has_named_impl(&db));
        // The same implementation spelled with one more (repeated) condition.
        let respelled = {
            let proof = ordinary(second);
            let Evidence::Impl {
                selected,
                instantiated,
                ..
            } = proof.data(&db)
            else {
                panic!("an implementation")
            };
            EvidenceId::new(
                &db,
                Evidence::Impl {
                    selected: *selected,
                    instantiated: *instantiated,
                    conds: vec![proof],
                },
            )
        };
        assert!(!same_dictionary(
            &db,
            ordinary(second),
            respelled,
            PredicateListId::empty_list(&db)
        ));
        let instance = |at_first, at_second| {
            let supplied = (0..goals.list(&db).len())
                .map(|index| {
                    Some(if index == first {
                        at_first
                    } else if index == second {
                        at_second
                    } else {
                        ordinary(index)
                    })
                })
                .collect::<Vec<_>>();
            instance_evidence(&db, goals, signature, EvidenceList::of(&db, supplied), cx)
        };
        assert_eq!(instance(ordinary(first), respelled).refused, None);
        let refused = instance(named_proof, ordinary(second))
            .refused
            .expect("two implementations of one goal are refused");
        assert!(matches!(refused.reason, Refusal::Differs { .. }));
    }
}
