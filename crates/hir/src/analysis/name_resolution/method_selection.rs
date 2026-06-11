use crate::core::hir_def::{IdentId, Trait, scope_graph::ScopeId};
use common::indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHashSet;
use thin_vec::ThinVec;

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{available_traits_in_scope, is_scope_visible_from},
    ty::{
        binder::Binder,
        canonical::{Canonical, Canonicalized, Solution},
        method_table::{ProbedMethod, probe_method},
        trait_def::{TraitInstId, impls_for_ty},
        trait_resolution::{
            CanonicalGoalQuery, GoalSatisfiability, PredicateListId, TraitSolveCx,
            is_goal_query_satisfiable,
        },
        ty_def::{TyData, TyId},
        unify::UnificationTable,
    },
};
use crate::hir_def::{CallableDef, Func};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum MethodCandidate<'db> {
    InherentMethod(ProbedMethod<'db>),
    TraitMethod(TraitMethodCand<'db>),
    NeedsConfirmation(TraitMethodCand<'db>),
}

impl<'db> MethodCandidate<'db> {
    pub fn name(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        match self {
            MethodCandidate::InherentMethod(cand) => {
                cand.def.name(db).expect("inherent methods have names")
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct AmbiguousTraitMethodCand<'db> {
    pub cand: TraitMethodCand<'db>,
    pub needs_confirmation: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AmbiguousTraitMethods<'db> {
    pub candidates: ThinVec<AmbiguousTraitMethodCand<'db>>,
    pub diagnostic_traits: ThinVec<TraitInstId<'db>>,
}

pub(crate) fn select_method_candidate<'db>(
    db: &'db dyn HirAnalysisDb,
    receiver: &Canonicalized<'db, TyId<'db>>,
    method_name: IdentId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_: Option<Trait<'db>>,
) -> Result<MethodCandidate<'db>, MethodSelectionError<'db>> {
    let receiver_ty = receiver.original();
    if receiver_ty.is_ty_var(db) {
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
    receiver: &Canonicalized<'db, TyId<'db>>,
    method_name: IdentId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_: Option<Trait<'db>>,
) -> AssembledCandidates<'db> {
    CandidateAssembler {
        db,
        receiver,
        method_name,
        scope,
        assumptions,
        trait_,
        candidates: AssembledCandidates::default(),
    }
    .assemble()
}

struct CandidateAssembler<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    /// The type that method is being called on.
    receiver: &'a Canonicalized<'db, TyId<'db>>,
    /// The name of the method being called.
    method_name: IdentId<'db>,
    /// The scope that candidates are being assembled in.
    scope: ScopeId<'db>,
    /// The assumptions for the type bound in the current scope.
    assumptions: PredicateListId<'db>,
    trait_: Option<Trait<'db>>,
    candidates: AssembledCandidates<'db>,
}

fn receiver_is_ty_param_like<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    let receiver_ty = ty.as_capability(db).map(|(_, inner)| inner).unwrap_or(ty);
    matches!(
        receiver_ty.base_ty(db).data(db),
        TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
    )
}

impl<'db, 'a> CandidateAssembler<'db, 'a> {
    fn assemble(mut self) -> AssembledCandidates<'db> {
        if self.trait_.is_none() {
            self.assemble_inherent_method_candidates();
        }
        self.assemble_trait_method_candidates();
        self.candidates
    }

    fn assemble_inherent_method_candidates(&mut self) {
        let ingot = self
            .receiver
            .original()
            .ingot(self.db)
            .unwrap_or_else(|| self.scope.ingot(self.db));
        for &method in probe_method(self.db, ingot, self.receiver.canonical(), self.method_name) {
            self.candidates.insert_inherent_method(method);
        }
    }

    fn assemble_trait_method_candidates(&mut self) {
        let scope_ingot = self.scope.ingot(self.db);

        // When the receiver is a type parameter (e.g. `D` in `fn f<D: Trait>(d: D)`),
        // we don't know its concrete type yet, so probing impls would pull in many
        // unrelated candidates and frequently lead to spurious ambiguity.
        //
        // In that case, rely on in-scope bounds (`assumptions`) to provide method
        // candidates.
        let receiver_is_ty_param = receiver_is_ty_param_like(self.db, self.receiver.original());

        if !receiver_is_ty_param {
            let search_ingots = [
                Some(scope_ingot),
                self.receiver
                    .original()
                    .ingot(self.db)
                    .filter(|&ingot| ingot != scope_ingot),
            ];
            for ingot in search_ingots.into_iter().flatten() {
                for &imp in impls_for_ty(self.db, ingot, self.receiver.canonical()) {
                    self.insert_trait_method_cand(imp.skip_binder().trait_(self.db));
                }
            }
        }

        self.receiver.with_materialized(self.db, |cx| {
            let receiver = cx.query();
            for &pred in self.assumptions.list(self.db) {
                let snapshot = cx.snapshot();
                // `*`-kind receivers need a fully applied self type before
                // bound unification, otherwise abstract constructors can never
                // match the concrete receiver term.
                let self_ty = if receiver.is_star_kind(self.db) {
                    cx.materialize_to_term(pred.self_ty(self.db))
                } else {
                    cx.materialize(pred.self_ty(self.db))
                };

                if cx.unify::<TyId<'db>>(receiver, self_ty).is_ok() {
                    self.insert_trait_method_cand(pred);
                    for super_trait in pred.def(self.db).super_traits(self.db) {
                        let super_trait = super_trait.instantiate(self.db, pred.args(self.db));
                        self.insert_trait_method_cand(super_trait);
                    }
                }

                cx.rollback_to(snapshot);
            }
        });
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

struct MethodSelector<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    receiver: &'a Canonicalized<'db, TyId<'db>>,
    scope: ScopeId<'db>,
    candidates: AssembledCandidates<'db>,
    assumptions: PredicateListId<'db>,
}

impl<'db, 'a> MethodSelector<'db, 'a> {
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
            .filter(|cand| self.is_inherent_method_visible(cand.def))
            .collect();

        match visible_inherent_methods.len() {
            0 => {
                if inherent_methods.is_empty() {
                    None
                } else {
                    Some(Err(MethodSelectionError::InvisibleInherentMethod(
                        inherent_methods.iter().next().unwrap().def,
                    )))
                }
            }
            1 => Some(Ok(MethodCandidate::InherentMethod(
                visible_inherent_methods[0],
            ))),

            _ => Some(Err(MethodSelectionError::AmbiguousInherentMethod(
                inherent_methods.iter().map(|cand| cand.def).collect(),
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

            _ => {
                // Some candidates are equivalent after trait solving (e.g., an explicit
                // bound and an implied/blanket-derived bound for the same method), but we
                // must still treat distinct methods as ambiguous so later return-type
                // constraints can disambiguate them.
                let mut selected = IndexMap::default();
                for (inst, method) in visible_traits.iter().copied() {
                    match self.check_inst(inst, method) {
                        MethodCandidate::TraitMethod(cand) => {
                            selected.insert(cand, true);
                        }
                        MethodCandidate::NeedsConfirmation(cand) => {
                            selected.entry(cand).or_insert(false);
                        }
                        MethodCandidate::InherentMethod(_) => unreachable!(),
                    }
                }

                if selected.len() == 1 {
                    let (cand, confirmed) = selected.into_iter().next().unwrap();
                    return Ok(if confirmed {
                        MethodCandidate::TraitMethod(cand)
                    } else {
                        MethodCandidate::NeedsConfirmation(cand)
                    });
                }

                let confirmed: Vec<_> = selected
                    .iter()
                    .filter_map(|(&cand, &is_confirmed)| is_confirmed.then_some(cand))
                    .collect();
                if confirmed.len() == 1
                    && (self.receiver.original().has_var(self.db)
                        || selected
                            .iter()
                            .filter_map(|(&cand, &is_confirmed)| (!is_confirmed).then_some(cand))
                            .all(|cand| self.candidate_specializes_to(cand, confirmed[0])))
                {
                    return Ok(MethodCandidate::TraitMethod(confirmed[0]));
                }

                let diagnostic_traits = visible_traits.iter().map(|cand| cand.0).collect();
                let candidates = selected
                    .into_iter()
                    .map(|(cand, confirmed)| AmbiguousTraitMethodCand {
                        cand,
                        needs_confirmation: !confirmed,
                    })
                    .collect();
                Err(MethodSelectionError::AmbiguousTraitMethod(
                    AmbiguousTraitMethods {
                        candidates,
                        diagnostic_traits,
                    },
                ))
            }
        }
    }

    fn candidate_specializes_to(
        &self,
        candidate: TraitMethodCand<'db>,
        confirmed: TraitMethodCand<'db>,
    ) -> bool {
        let mut table = UnificationTable::new(self.db);
        let candidate_inst = self.receiver.extract_solution(&mut table, candidate.inst);
        let confirmed_inst = self.receiver.extract_solution(&mut table, confirmed.inst);
        if candidate_inst.def(self.db) != confirmed_inst.def(self.db)
            || candidate.method.name(self.db) != confirmed.method.name(self.db)
        {
            return false;
        }

        let solve_cx = TraitSolveCx::new(self.db, self.scope).with_assumptions(self.assumptions);
        let query = CanonicalGoalQuery::new(self.db, candidate_inst, self.assumptions);
        let confirmed = Canonical::new(self.db, confirmed_inst);
        let mut table = UnificationTable::new(self.db);
        match is_goal_query_satisfiable(self.db, solve_cx, &query) {
            GoalSatisfiability::Satisfied(solution) => {
                Canonical::new(self.db, query.extract_solution(&mut table, solution).inst)
                    == confirmed
            }
            GoalSatisfiability::NeedsConfirmation(solutions) => {
                solutions.into_iter().any(|solution| {
                    Canonical::new(self.db, query.extract_solution(&mut table, solution).inst)
                        == confirmed
                })
            }
            GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_) => false,
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
        let _ = self.receiver.canonical().extract_identity(&mut table);

        // If the receiver is a type parameter (e.g. `D` in `fn f<D: Trait>(d: D)`),
        // prefer preserving any trait arguments coming from bounds rather than
        // introducing fresh inference vars. Otherwise, unconstrained trait args
        // can trigger spurious "type annotation needed" diagnostics on method calls
        // whose signatures don't mention those args (e.g. `AbiDecoder<A>::read_word`).
        let receiver_is_ty_param = receiver_is_ty_param_like(self.db, self.receiver.original());

        let query = CanonicalGoalQuery::new(self.db, inst, self.assumptions);
        let inst = if receiver_is_ty_param {
            inst
        } else {
            table.instantiate_with_fresh_vars(Binder::bind(inst))
        };

        match is_goal_query_satisfiable(
            self.db,
            TraitSolveCx::new(self.db, self.scope).with_assumptions(self.assumptions),
            &query,
        ) {
            GoalSatisfiability::Satisfied(solution) => {
                // Map back the solution to the current context.
                let solution = query.extract_solution(&mut table, solution).inst;
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

            GoalSatisfiability::NeedsConfirmation(_)
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
    AmbiguousTraitMethod(AmbiguousTraitMethods<'db>),
    NotFound,
    InvisibleInherentMethod(CallableDef<'db>),
    InvisibleTraitMethod(ThinVec<Trait<'db>>),
    ReceiverTypeMustBeKnown,
}

#[derive(Default)]
struct AssembledCandidates<'db> {
    inherent_methods: FxHashSet<ProbedMethod<'db>>,
    traits: IndexSet<(TraitInstId<'db>, Func<'db>)>,
}

impl<'db> AssembledCandidates<'db> {
    fn insert_inherent_method(&mut self, method: ProbedMethod<'db>) {
        self.inherent_methods.insert(method);
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use crate::{
        analysis::{
            name_resolution::{PathRes, resolve_path},
            ty::{
                canonical::Canonical, trait_def::impls_for_ty, trait_resolution::PredicateListId,
            },
        },
        hir_def::{IdentId, PathId},
        test_db::HirAnalysisTestDb,
    };

    #[test]
    fn address_has_wordrepr_impl_in_std_trait_env() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("address_has_wordrepr_impl_in_std_trait_env.fe"),
            r#"
use std::evm::word::WordRepr

fn test_it() {
    let _ = Address::zero()
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let assumptions = PredicateListId::empty_list(&db);
        let scope = top_mod.scope();

        let address = match resolve_path(
            &db,
            PathId::from_ident(&db, IdentId::new(&db, "Address".to_string())),
            scope,
            assumptions,
            false,
        )
        .unwrap()
        {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => ty,
            res => panic!("expected Address to resolve to a type, got {res:?}"),
        };
        let wordrepr = match resolve_path(
            &db,
            PathId::from_ident(&db, IdentId::new(&db, "WordRepr".to_string())),
            scope,
            assumptions,
            false,
        )
        .unwrap()
        {
            PathRes::Trait(inst) => inst.def(&db),
            res => panic!("expected WordRepr to resolve to a trait, got {res:?}"),
        };

        let std_ingot = address.ingot(&db).expect("Address should come from std");
        let impls = impls_for_ty(&db, std_ingot, Canonical::new(&db, address));
        let impl_trait_names: Vec<_> = impls
            .iter()
            .map(|imp| {
                imp.skip_binder()
                    .trait_(&db)
                    .pretty_print(&db, false)
                    .to_string()
            })
            .collect();

        assert!(
            impls
                .iter()
                .any(|imp| imp.skip_binder().trait_def(&db) == wordrepr),
            "expected WordRepr impl for Address, found {impl_trait_names:?}"
        );
    }

    #[test]
    fn address_wordrepr_method_resolves_across_std_modules() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("address_wordrepr_method_resolves_across_std_modules.fe"),
            r#"
use std::evm::word::WordRepr

fn test_it() {
    let a = Address { inner: 42 }
    let _w = a.to_word()
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
    }

    #[test]
    fn storage_map_address_value_uses_wordrepr_impl() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("storage_map_address_value_uses_wordrepr_impl.fe"),
            r#"
use std::evm::StorageMap

fn test_it() {
    let _map: StorageMap<Address, Address, 0> = StorageMap::new()
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
    }
}
