use crate::core::hir_def::{IdentId, Trait, scope_graph::ScopeId};
use common::indexmap::IndexSet;
use rustc_hash::FxHashSet;
use thin_vec::ThinVec;

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{available_traits_in_scope, is_scope_visible_from},
    ty::{
        binder::Binder,
        canonical::{Canonical, Canonicalized, Solution},
        method_table::probe_method,
        trait_def::{TraitInstId, impls_for_ty},
        trait_resolution::{GoalSatisfiability, PredicateListId, is_goal_satisfiable},
        ty_def::{TyData, TyId},
        unify::UnificationTable,
    },
};
use crate::hir_def::{CallableDef, Func};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum MethodCandidate<'db> {
    InherentMethod(CallableDef<'db>),
    TraitMethod(TraitMethodCand<'db>),
    NeedsConfirmation(TraitMethodCand<'db>),
}

impl<'db> MethodCandidate<'db> {
    pub fn name(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        match self {
            MethodCandidate::InherentMethod(func_def) => {
                func_def.name(db).expect("inherent methods have names")
            }
            MethodCandidate::TraitMethod(cand) | MethodCandidate::NeedsConfirmation(cand) => cand
                .method
                .name(db)
                .to_opt()
                .expect("trait methods have names"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct TraitMethodCand<'db> {
    pub inst: Solution<TraitInstId<'db>>,
    pub method: Func<'db>,
}

impl<'db> TraitMethodCand<'db> {
    fn new(inst: Solution<TraitInstId<'db>>, method: Func<'db>) -> Self {
        Self { inst, method }
    }
}

pub(crate) fn select_method_candidate<'db>(
    db: &'db dyn HirAnalysisDb,
    receiver: Canonical<TyId<'db>>,
    method_name: IdentId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_: Option<Trait<'db>>,
) -> Result<MethodCandidate<'db>, MethodSelectionError<'db>> {
    if receiver.value.is_ty_var(db) {
        return Err(MethodSelectionError::ReceiverTypeMustBeKnown);
    }

    let candidates =
        assemble_method_candidates(db, receiver, method_name, scope, assumptions, trait_);

    let selector = MethodSelector {
        db,
        receiver,
        scope,
        candidates,
        assumptions,
    };

    selector.select()
}

fn assemble_method_candidates<'db>(
    db: &'db dyn HirAnalysisDb,
    receiver_ty: Canonical<TyId<'db>>,
    method_name: IdentId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_: Option<Trait<'db>>,
) -> AssembledCandidates<'db> {
    CandidateAssembler {
        db,
        receiver_ty,
        method_name,
        scope,
        assumptions,
        trait_,
        candidates: AssembledCandidates::default(),
    }
    .assemble()
}

struct CandidateAssembler<'db> {
    db: &'db dyn HirAnalysisDb,
    /// The type that method is being called on.
    receiver_ty: Canonical<TyId<'db>>,
    /// The name of the method being called.
    method_name: IdentId<'db>,
    /// The scope that candidates are being assembled in.
    scope: ScopeId<'db>,
    /// The assumptions for the type bound in the current scope.
    assumptions: PredicateListId<'db>,
    trait_: Option<Trait<'db>>,
    candidates: AssembledCandidates<'db>,
}

impl<'db> CandidateAssembler<'db> {
    fn assemble(mut self) -> AssembledCandidates<'db> {
        if self.trait_.is_none() {
            self.assemble_inherent_method_candidates();
        }
        self.assemble_trait_method_candidates();
        self.candidates
    }

    fn assemble_inherent_method_candidates(&mut self) {
        let ingot = self
            .receiver_ty
            .value
            .ingot(self.db)
            .unwrap_or_else(|| self.scope.ingot(self.db));
        for &method in probe_method(self.db, ingot, self.receiver_ty, self.method_name) {
            self.candidates.insert_inherent_method(method);
        }
    }

    fn assemble_trait_method_candidates(&mut self) {
        let ingot = self.scope.ingot(self.db);

        // When the receiver is a type parameter (e.g. `D` in `fn f<D: Trait>(d: D)`),
        // we don't know its concrete type yet, so probing impls would pull in many
        // unrelated candidates and frequently lead to spurious ambiguity.
        //
        // In that case, rely on in-scope bounds (`assumptions`) to provide method
        // candidates.
        let receiver_is_ty_param = matches!(
            self.receiver_ty.value.base_ty(self.db).data(self.db),
            TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
        );

        if !receiver_is_ty_param {
            for &imp in impls_for_ty(self.db, ingot, self.receiver_ty) {
                self.insert_trait_method_cand(imp.skip_binder().trait_(self.db));
            }
        }

        let mut table = UnificationTable::new(self.db);
        let extracted_receiver_ty = self.receiver_ty.extract_identity(&mut table);

        for &pred in self.assumptions.list(self.db) {
            let snapshot = table.snapshot();
            let self_ty = pred.self_ty(self.db);
            let self_ty = table.instantiate_to_term(self_ty);

            if table.unify(extracted_receiver_ty, self_ty).is_ok() {
                self.insert_trait_method_cand(pred);
                for super_trait in pred.def(self.db).super_traits(self.db) {
                    let super_trait = super_trait.instantiate(self.db, pred.args(self.db));
                    self.insert_trait_method_cand(super_trait);
                }
            }

            table.rollback_to(snapshot);
        }
    }

    fn allow_trait(&self, trait_def: Trait<'db>) -> bool {
        self.trait_.map(|t| t == trait_def).unwrap_or(true)
    }

    fn insert_trait_method_cand(&mut self, inst: TraitInstId<'db>) {
        let trait_def = inst.def(self.db);
        if !self.allow_trait(trait_def) {
            return;
        }
        if let Some(&trait_method) = trait_def.method_defs(self.db).get(&self.method_name) {
            self.candidates.traits.insert((inst, trait_method));
        }
    }
}

struct MethodSelector<'db> {
    db: &'db dyn HirAnalysisDb,
    receiver: Canonical<TyId<'db>>,
    scope: ScopeId<'db>,
    candidates: AssembledCandidates<'db>,
    assumptions: PredicateListId<'db>,
}

impl<'db> MethodSelector<'db> {
    fn select(self) -> Result<MethodCandidate<'db>, MethodSelectionError<'db>> {
        if let Some(res) = self.select_inherent_method() {
            return res;
        }

        self.select_trait_methods()
    }

    fn select_inherent_method(
        &self,
    ) -> Option<Result<MethodCandidate<'db>, MethodSelectionError<'db>>> {
        let inherent_methods = &self.candidates.inherent_methods;
        let visible_inherent_methods: Vec<_> = inherent_methods
            .iter()
            .copied()
            .filter(|cand| self.is_inherent_method_visible(*cand))
            .collect();

        match visible_inherent_methods.len() {
            0 => {
                if inherent_methods.is_empty() {
                    None
                } else {
                    Some(Err(MethodSelectionError::InvisibleInherentMethod(
                        *inherent_methods.iter().next().unwrap(),
                    )))
                }
            }
            1 => Some(Ok(MethodCandidate::InherentMethod(
                visible_inherent_methods[0],
            ))),

            _ => Some(Err(MethodSelectionError::AmbiguousInherentMethod(
                inherent_methods.iter().copied().collect(),
            ))),
        }
    }

    /// Selects the most appropriate trait method candidate.
    ///
    /// This function checks the available trait method candidates and attempts
    /// to find the best match. If there is only one candidate, it is returned.
    /// If there are multiple candidates, it checks for visibility and
    /// ambiguity.
    ///
    /// **NOTE**: If there is no ambiguity, the trait does not need to be
    /// visible.
    ///
    /// # Returns
    ///
    /// * `Ok(Candidate)` - The selected method candidate.
    /// * `Err(MethodSelectionError)` - An error indicating the reason for
    ///   failure.
    fn select_trait_methods(&self) -> Result<MethodCandidate<'db>, MethodSelectionError<'db>> {
        let traits = &self.candidates.traits;

        if traits.len() == 1 {
            let (inst, method) = traits.iter().next().unwrap();
            return Ok(self.check_inst(*inst, *method));
        }

        let available_traits = self.available_traits();
        let visible_traits: Vec<_> = traits
            .iter()
            .copied()
            .filter(|(inst, _method)| available_traits.contains(&inst.def(self.db)))
            .collect();

        match visible_traits.len() {
            0 => {
                if traits.is_empty() {
                    Err(MethodSelectionError::NotFound)
                } else {
                    // Suggests trait imports.
                    let traits = traits.iter().map(|(inst, _)| inst.def(self.db)).collect();
                    Err(MethodSelectionError::InvisibleTraitMethod(traits))
                }
            }

            1 => {
                let (def, method) = visible_traits[0];
                Ok(self.check_inst(def, method))
            }

            _ => Err(MethodSelectionError::AmbiguousTraitMethod(
                visible_traits.into_iter().map(|cand| cand.0).collect(),
            )),
        }
    }

    /// Finds an instance of a trait method for the given trait definition and
    /// method.
    ///
    /// This function attempts to unify the receiver type with the method's self
    /// type, and assigns type variables to the trait parameters. It then
    /// checks if the goal is satisfiable given the current assumptions.
    /// Depending on the result, it either returns a confirmed trait method
    /// candidate or one that needs further confirmation.
    fn check_inst(&self, inst: TraitInstId<'db>, method: Func<'db>) -> MethodCandidate<'db> {
        let mut table = UnificationTable::new(self.db);
        // Seed the table with receiver's canonical variables so that subsequent
        // canonicalization can safely probe them.
        let _ = self.receiver.extract_identity(&mut table);

        // If the receiver is a type parameter (e.g. `D` in `fn f<D: Trait>(d: D)`),
        // prefer preserving any trait arguments coming from bounds rather than
        // introducing fresh inference vars. Otherwise, unconstrained trait args
        // can trigger spurious "type annotation needed" diagnostics on method calls
        // whose signatures don't mention those args (e.g. `AbiDecoder<A>::read_word`).
        let receiver_is_ty_param = matches!(
            self.receiver.value.base_ty(self.db).data(self.db),
            TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
        );

        let canonical_cand = Canonicalized::new(self.db, inst);
        let inst = if receiver_is_ty_param {
            inst
        } else {
            table.instantiate_with_fresh_vars(Binder::bind(inst))
        };

        match is_goal_satisfiable(
            self.db,
            self.scope.ingot(self.db),
            canonical_cand.value,
            self.assumptions,
        ) {
            GoalSatisfiability::Satisfied(solution) => {
                // Map back the solution to the current context.
                let solution = canonical_cand.extract_solution(&mut table, *solution);
                // Replace TyParams in the solved instance with fresh inference vars so
                // downstream unification can bind them (e.g., T = u32). For receiver type
                // parameters, keep the bound's args intact.
                let solution = if receiver_is_ty_param {
                    solution
                } else {
                    table.instantiate_with_fresh_vars(Binder::bind(solution))
                };

                MethodCandidate::TraitMethod(TraitMethodCand::new(
                    self.receiver
                        .canonicalize_solution(self.db, &mut table, solution),
                    method,
                ))
            }

            &GoalSatisfiability::NeedsConfirmation(_)
            | GoalSatisfiability::ContainsInvalid
            | GoalSatisfiability::UnSat(_) => {
                MethodCandidate::NeedsConfirmation(TraitMethodCand::new(
                    self.receiver
                        .canonicalize_solution(self.db, &mut table, inst),
                    method,
                ))
            }
        }
    }

    fn is_inherent_method_visible(&self, def: CallableDef) -> bool {
        is_scope_visible_from(self.db, def.scope(), self.scope)
    }

    fn available_traits(&self) -> IndexSet<Trait<'db>> {
        let mut traits = IndexSet::default();

        let mut insert_trait = |trait_def: Trait<'db>| {
            traits.insert(trait_def);

            for trait_ in trait_def.super_traits(self.db) {
                traits.insert(trait_.skip_binder().def(self.db));
            }
        };

        for &trait_ in available_traits_in_scope(self.db, self.scope) {
            let trait_def = trait_;
            insert_trait(trait_def);
        }

        for pred in self.assumptions.list(self.db) {
            let trait_def = pred.def(self.db);
            insert_trait(trait_def)
        }

        traits
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum MethodSelectionError<'db> {
    AmbiguousInherentMethod(ThinVec<CallableDef<'db>>),
    AmbiguousTraitMethod(ThinVec<TraitInstId<'db>>),
    NotFound,
    InvisibleInherentMethod(CallableDef<'db>),
    InvisibleTraitMethod(ThinVec<Trait<'db>>),
    ReceiverTypeMustBeKnown,
}

#[derive(Default)]
struct AssembledCandidates<'db> {
    inherent_methods: FxHashSet<CallableDef<'db>>,
    traits: IndexSet<(TraitInstId<'db>, Func<'db>)>,
}

impl<'db> AssembledCandidates<'db> {
    fn insert_inherent_method(&mut self, method: CallableDef<'db>) {
        self.inherent_methods.insert(method);
    }
}
