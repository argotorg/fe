use crate::core::hir_def::{IdentId, Trait, scope_graph::ScopeId};
use common::indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHashSet;
use thin_vec::ThinVec;

use crate::analysis::ty::normalize::NormalizationLimit;
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{available_traits_in_scope, is_scope_visible_from},
    ty::{
        candidates::{self, Counting, Decided, Holds},
        canonical::{Canonical, Canonicalized, Solution},
        fold::TyFoldable as _,
        method_table::{MethodProbe, ProbedMethod, probe_method},
        trait_def::{ImplementorId, TraitInstId},
        trait_resolution::{
            CanonicalGoalQuery, GoalSatisfiability, PredicateListId, TraitSolveCx,
            goal_query_has_solution, is_goal_query_satisfiable,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum AssembledTraitMethodCand<'db> {
    Impl {
        implementor: ImplementorId<'db>,
        method: Func<'db>,
    },
    Assumption {
        inst: TraitInstId<'db>,
        method: Func<'db>,
    },
}

impl<'db> AssembledTraitMethodCand<'db> {
    fn trait_def(self, db: &'db dyn HirAnalysisDb) -> Trait<'db> {
        match self {
            AssembledTraitMethodCand::Impl { implementor, .. } => implementor.trait_def(db),
            AssembledTraitMethodCand::Assumption { inst, .. } => inst.def(db),
        }
    }

    fn diagnostic_inst(self, db: &'db dyn HirAnalysisDb) -> TraitInstId<'db> {
        match self {
            AssembledTraitMethodCand::Impl { implementor, .. } => implementor.trait_(db),
            AssembledTraitMethodCand::Assumption { inst, .. } => inst,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct AmbiguousTraitMethodCand<'db> {
    pub cand: TraitMethodCand<'db>,
    pub needs_confirmation: bool,
    /// Whether the candidate applies is unknown: checking it reached this
    /// limit. If a later filter (the expected type, an operand) keeps it, its
    /// obligation decides it.
    pub unknown: Option<NormalizationLimit>,
}

impl<'db> AmbiguousTraitMethods<'db> {
    /// The limit to report instead of an ambiguity among `candidates`: when
    /// at most one of them is known to apply, the ambiguity exists only if
    /// some unknown one applies, so the answer depends on the limit.
    pub fn limit_of(candidates: &[AmbiguousTraitMethodCand<'db>]) -> Option<NormalizationLimit> {
        let known = candidates
            .iter()
            .filter(|cand| cand.unknown.is_none())
            .count();
        let unknown = candidates
            .iter()
            .filter_map(|cand| cand.unknown)
            .fold(None, |earlier, limit| {
                Some(NormalizationLimit::join(earlier, limit))
            });
        if known <= 1 { unknown } else { None }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AmbiguousTraitMethods<'db> {
    pub candidates: ThinVec<AmbiguousTraitMethodCand<'db>>,
    pub diagnostic_traits: ThinVec<TraitInstId<'db>>,
}

#[derive(Clone, Copy, PartialEq)]
enum TraitCandidateCheck<'db> {
    Confirmed(TraitMethodCand<'db>),
    NeedsConfirmation(TraitMethodCand<'db>),
    Unsatisfied(TraitMethodCand<'db>),
    Rejected,
    /// Checking the candidate reached a normalization limit and nothing ruled
    /// it out: whether it applies is unknown.
    Unknown(TraitMethodCand<'db>, NormalizationLimit),
}

type CheckedCands<'db> = Vec<(AssembledTraitMethodCand<'db>, TraitCandidateCheck<'db>)>;

/// The limits of the unknown candidates in `checked`, in order.
fn unknown_limits(checked: &CheckedCands<'_>) -> Vec<NormalizationLimit> {
    checked
        .iter()
        .filter_map(|(_, check)| match check {
            TraitCandidateCheck::Unknown(_, limit) => Some(*limit),
            _ => None,
        })
        .collect()
}

/// `checked` with each unknown candidate counted as applying or not, as
/// `setting` gives in order.
fn settle<'db>(checked: &CheckedCands<'db>, setting: &[bool]) -> CheckedCands<'db> {
    let mut setting = setting.iter();
    checked
        .iter()
        .map(|&(assembled, check)| match check {
            TraitCandidateCheck::Unknown(cand, _) => {
                let check = if *setting.next().unwrap() {
                    TraitCandidateCheck::Confirmed(cand)
                } else {
                    TraitCandidateCheck::Unsatisfied(cand)
                };
                (assembled, check)
            }
            _ => (assembled, check),
        })
        .collect()
}

/// Marks the candidates of `ambiguous` that are unknown in `checked`.
fn mark_unknown<'db>(ambiguous: &mut AmbiguousTraitMethods<'db>, checked: &CheckedCands<'db>) {
    for candidate in &mut ambiguous.candidates {
        for (_, check) in checked {
            if let TraitCandidateCheck::Unknown(cand, limit) = *check
                && cand == candidate.cand
            {
                candidate.needs_confirmation = true;
                candidate.unknown = Some(limit);
            }
        }
    }
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
    if let Some(limit) = candidates.limit {
        return Err(MethodSelectionError::NormalizationLimit(limit));
    }

    let selector = MethodSelector {
        db,
        receiver,
        scope,
        candidates,
        assumptions,
    };

    selector.select()
}

pub(crate) fn select_trait_method_candidates<'db>(
    db: &'db dyn HirAnalysisDb,
    receiver: &Canonicalized<'db, TyId<'db>>,
    method_name: IdentId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_: Trait<'db>,
) -> Result<AmbiguousTraitMethods<'db>, MethodSelectionError<'db>> {
    let receiver_ty = receiver.original();
    if receiver_ty.is_ty_var(db) {
        return Err(MethodSelectionError::ReceiverTypeMustBeKnown);
    }

    let candidates =
        assemble_method_candidates(db, receiver, method_name, scope, assumptions, Some(trait_));
    if let Some(limit) = candidates.limit {
        return Err(MethodSelectionError::NormalizationLimit(limit));
    }

    let selector = MethodSelector {
        db,
        receiver,
        scope,
        candidates,
        assumptions,
    };

    selector.select_visible_trait_method_candidates()
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
    let receiver_ty = ty.as_capability(db).map_or(ty, |(_, inner)| inner);
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
        match probe_method(
            self.db,
            ingot,
            MethodProbe {
                receiver: self.receiver.original(),
                assumptions: self.assumptions,
            },
            self.scope,
            self.method_name,
        ) {
            Ok(methods) => {
                for method in methods {
                    self.candidates.insert_inherent_method(method);
                }
            }
            Err(limit) => self.candidates.limit = Some(limit),
        }
    }

    fn assemble_trait_method_candidates(&mut self) {
        let scope_ingot = self.scope.ingot(self.db);

        // Discovery does not grant these bounds. check_inst sends each
        // candidate through the solver, including the family rule's premises.
        if let Some((_, bounds)) = self.receiver.original().family_declared_bounds(self.db) {
            for &bound in bounds.list(self.db) {
                self.insert_assumption_trait_method_cand(bound);
            }
        }

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
            for imp in candidates::method_impl_candidates(
                self.db,
                self.receiver.canonical(),
                self.trait_,
                self.method_name,
                search_ingots,
            ) {
                self.insert_impl_trait_method_cand(imp);
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
                    self.insert_assumption_trait_method_cand(pred);
                    for super_trait in pred.def(self.db).super_traits(self.db) {
                        let super_trait = super_trait.instantiate(self.db, pred.args(self.db));
                        self.insert_assumption_trait_method_cand(super_trait);
                    }
                }

                cx.rollback_to(snapshot);
            }
        });
    }

    fn allow_trait(&self, trait_def: Trait<'db>) -> bool {
        self.trait_.is_none_or(|t| t == trait_def)
    }

    fn insert_impl_trait_method_cand(&mut self, implementor: ImplementorId<'db>) {
        let trait_def = implementor.trait_def(self.db);
        if !self.allow_trait(trait_def) {
            return;
        }
        if let Some(&trait_method) = trait_def.method_defs(self.db).get(&self.method_name) {
            self.candidates
                .traits
                .insert(AssembledTraitMethodCand::Impl {
                    implementor,
                    method: trait_method,
                });
        }
    }

    fn insert_assumption_trait_method_cand(&mut self, inst: TraitInstId<'db>) {
        let trait_def = inst.def(self.db);
        if self.allow_trait(trait_def)
            && let Some(&trait_method) = trait_def.method_defs(self.db).get(&self.method_name)
        {
            self.candidates
                .traits
                .insert(AssembledTraitMethodCand::Assumption {
                    inst,
                    method: trait_method,
                });
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
        // For a fully-known receiver, drop trait candidates whose impl is
        // provably inapplicable: either the self type cannot unify
        // (`Rejected`) or the impl's `where`-clause is unsatisfiable
        // (`Unsatisfied`). Otherwise a blanket `impl<T: Marker> Trait for T`
        // that cannot apply to this receiver would still count as a competing
        // candidate below and could, for instance, turn an otherwise
        // unambiguous `concrete.method()` call into a spurious "import the
        // trait" error (the single-candidate path resolves without requiring
        // the trait to be in scope, but a second, inapplicable candidate
        // defeats it).
        //
        // With an inference-variable receiver we cannot prove inapplicability
        // yet, so everything is kept (matching the prior behavior). If pruning
        // would remove every candidate, the originals are kept so the
        // unsatisfied-bound diagnostics below still fire instead of reporting
        // "not found".
        //
        // Each candidate is checked exactly once here and the result is carried
        // through pruning and selection, so the trait solver isn't re-run for
        // the same candidate.
        let checked: CheckedCands<'db> = self
            .candidates
            .traits
            .iter()
            .copied()
            .map(|cand| (cand, self.check_trait_cand(cand)))
            .collect();
        // A candidate whose check reached a limit decides the lookup only if
        // the choice depends on it.
        match candidates::decide(&unknown_limits(&checked), |setting| {
            self.select_checked_trait_methods(settle(&checked, setting))
        }) {
            Decided::Same(selected) => selected,
            Decided::Differs {
                results,
                limit,
                exhaustive,
            } => {
                // The same method in every setting: it is chosen, and its
                // obligation decides whether it applies.
                let chosen = |result: &Result<MethodCandidate<'db>, _>| match result {
                    Ok(MethodCandidate::TraitMethod(cand))
                    | Ok(MethodCandidate::NeedsConfirmation(cand)) => Some(*cand),
                    _ => None,
                };
                if exhaustive
                    && let Some(cand) = chosen(&results[0])
                    && results.iter().all(|result| chosen(result) == Some(cand))
                {
                    return Ok(MethodCandidate::NeedsConfirmation(cand));
                }
                // An ambiguity that the expected type may settle later: kept,
                // with the unknown candidates marked.
                match results.into_iter().next_back() {
                    Some(Err(MethodSelectionError::AmbiguousTraitMethod(mut ambiguous))) => {
                        mark_unknown(&mut ambiguous, &checked);
                        Err(MethodSelectionError::AmbiguousTraitMethod(ambiguous))
                    }
                    _ => Err(MethodSelectionError::NormalizationLimit(limit)),
                }
            }
        }
    }

    /// The trait method chosen among checked candidates, none of them
    /// unknown.
    fn select_checked_trait_methods(
        &self,
        checked: CheckedCands<'db>,
    ) -> Result<MethodCandidate<'db>, MethodSelectionError<'db>> {
        let checked = self.prune_inapplicable_trait_checks(checked);

        if checked.len() == 1 {
            return Self::finalize_sole_check(checked[0].1);
        }

        let available_traits = self.available_traits();
        let visible: Vec<_> = checked
            .iter()
            .copied()
            .filter(|(cand, _)| available_traits.contains(&cand.trait_def(self.db)))
            .collect();

        match visible.len() {
            0 => {
                if checked.is_empty() {
                    Err(MethodSelectionError::NotFound)
                } else {
                    // Suggests trait imports.
                    let traits = checked
                        .iter()
                        .map(|(cand, _)| cand.trait_def(self.db))
                        .collect();
                    Err(MethodSelectionError::InvisibleTraitMethod(traits))
                }
            }

            1 => Self::finalize_sole_check(visible[0].1),

            _ => {
                // Some candidates are equivalent after trait solving (e.g., an explicit
                // bound and an implied/blanket-derived bound for the same method), but we
                // must still treat distinct methods as ambiguous so later return-type
                // constraints can disambiguate them.
                let mut selected = IndexMap::default();
                let mut unsatisfied = IndexSet::default();
                for (_, check) in visible.iter().copied() {
                    match check {
                        TraitCandidateCheck::Confirmed(cand) => {
                            selected
                                .entry(cand)
                                .and_modify(|confirmed| *confirmed = true)
                                .or_insert(true);
                        }
                        TraitCandidateCheck::NeedsConfirmation(cand) => {
                            selected.entry(cand).or_insert(false);
                        }
                        TraitCandidateCheck::Unsatisfied(cand) => {
                            unsatisfied.insert(cand);
                        }
                        TraitCandidateCheck::Rejected | TraitCandidateCheck::Unknown(..) => {}
                    }
                }

                if selected.is_empty() {
                    if unsatisfied.len() == 1 {
                        return Ok(MethodCandidate::NeedsConfirmation(
                            *unsatisfied.iter().next().unwrap(),
                        ));
                    }
                    if !unsatisfied.is_empty() {
                        let diagnostic_traits = visible
                            .iter()
                            .map(|(cand, _)| cand.diagnostic_inst(self.db))
                            .collect();
                        let candidates = unsatisfied
                            .into_iter()
                            .map(|cand| AmbiguousTraitMethodCand {
                                cand,
                                needs_confirmation: true,
                                unknown: None,
                            })
                            .collect();
                        return Err(MethodSelectionError::AmbiguousTraitMethod(
                            AmbiguousTraitMethods {
                                candidates,
                                diagnostic_traits,
                            },
                        ));
                    }
                    return Err(MethodSelectionError::NotFound);
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
                    .filter_map(|(&cand, &confirmed)| confirmed.then_some(cand))
                    .collect();
                if confirmed.len() == 1 {
                    // An unconfirmed candidate that does not specialize keeps
                    // the ambiguity, whatever the others turn out to be.
                    let specializes = if self.receiver.original().has_var(self.db) {
                        true
                    } else {
                        candidates::all_of(
                            selected
                                .iter()
                                .filter_map(|(&cand, &confirmed)| (!confirmed).then_some(cand))
                                .map(|cand| self.candidate_specializes_to(cand, confirmed[0])),
                        )
                        .map_err(MethodSelectionError::NormalizationLimit)?
                    };
                    if specializes {
                        return Ok(MethodCandidate::TraitMethod(confirmed[0]));
                    }
                }

                let diagnostic_traits = visible
                    .iter()
                    .map(|(cand, _)| cand.diagnostic_inst(self.db))
                    .collect();
                let candidates = selected
                    .into_iter()
                    .map(|(cand, confirmed)| AmbiguousTraitMethodCand {
                        cand,
                        needs_confirmation: !confirmed,
                        unknown: None,
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

    /// Resolves the outcome when a single trait candidate remains (the only
    /// assembled candidate, or the only visible one). An unsatisfiable
    /// candidate is still surfaced as `NeedsConfirmation` so the unmet bound is
    /// reported downstream rather than as a bare "method not found".
    fn finalize_sole_check(
        check: TraitCandidateCheck<'db>,
    ) -> Result<MethodCandidate<'db>, MethodSelectionError<'db>> {
        match check {
            TraitCandidateCheck::Confirmed(cand) => Ok(MethodCandidate::TraitMethod(cand)),
            TraitCandidateCheck::NeedsConfirmation(cand)
            | TraitCandidateCheck::Unsatisfied(cand) => {
                Ok(MethodCandidate::NeedsConfirmation(cand))
            }
            TraitCandidateCheck::Rejected => Err(MethodSelectionError::NotFound),
            TraitCandidateCheck::Unknown(cand, _) => Ok(MethodCandidate::NeedsConfirmation(cand)),
        }
    }

    fn select_visible_trait_method_candidates(
        &self,
    ) -> Result<AmbiguousTraitMethods<'db>, MethodSelectionError<'db>> {
        let traits = &self.candidates.traits;

        if traits.len() == 1 {
            return self.trait_method_candidates(traits.iter().copied());
        }

        let available_traits = self.available_traits();
        let visible_traits: Vec<_> = traits
            .iter()
            .copied()
            .filter(|cand| available_traits.contains(&cand.trait_def(self.db)))
            .collect();

        match visible_traits.len() {
            0 => {
                if traits.is_empty() {
                    Err(MethodSelectionError::NotFound)
                } else {
                    let traits = traits.iter().map(|cand| cand.trait_def(self.db)).collect();
                    Err(MethodSelectionError::InvisibleTraitMethod(traits))
                }
            }
            _ => self.trait_method_candidates(visible_traits),
        }
    }

    fn trait_method_candidates(
        &self,
        traits: impl IntoIterator<Item = AssembledTraitMethodCand<'db>>,
    ) -> Result<AmbiguousTraitMethods<'db>, MethodSelectionError<'db>> {
        let checked: CheckedCands<'db> = traits
            .into_iter()
            .map(|cand| (cand, self.check_trait_cand(cand)))
            .collect();
        // The candidates are filtered later by the operand or expected type.
        // If the list depends on an unknown candidate, the list with every
        // unknown candidate in it is kept, marked, so that the later filter
        // decides whether it matters.
        Ok(
            match candidates::decide(&unknown_limits(&checked), |setting| {
                self.checked_trait_method_candidates(settle(&checked, setting))
            }) {
                Decided::Same(list) => list,
                Decided::Differs { results, .. } => {
                    let mut list = results.into_iter().next_back().unwrap();
                    mark_unknown(&mut list, &checked);
                    list
                }
            },
        )
    }

    /// The candidates left after pruning checked ones, none of them unknown.
    fn checked_trait_method_candidates(
        &self,
        checked: CheckedCands<'db>,
    ) -> AmbiguousTraitMethods<'db> {
        let checked = self.prune_inapplicable_trait_checks(checked);
        let mut selected = IndexMap::default();
        let mut diagnostic_traits = ThinVec::new();
        for (cand, check) in checked {
            diagnostic_traits.push(cand.diagnostic_inst(self.db));
            match check {
                TraitCandidateCheck::Confirmed(cand) => {
                    selected.insert(cand, true);
                }
                TraitCandidateCheck::NeedsConfirmation(cand)
                | TraitCandidateCheck::Unsatisfied(cand) => {
                    selected.entry(cand).or_insert(false);
                }
                TraitCandidateCheck::Rejected | TraitCandidateCheck::Unknown(..) => {}
            }
        }

        let candidates = selected
            .into_iter()
            .map(|(cand, confirmed)| AmbiguousTraitMethodCand {
                cand,
                needs_confirmation: !confirmed,
                unknown: None,
            })
            .collect();
        AmbiguousTraitMethods {
            candidates,
            diagnostic_traits,
        }
    }

    fn prune_inapplicable_trait_checks(
        &self,
        checked: Vec<(AssembledTraitMethodCand<'db>, TraitCandidateCheck<'db>)>,
    ) -> Vec<(AssembledTraitMethodCand<'db>, TraitCandidateCheck<'db>)> {
        if checked.len() <= 1 || self.receiver.original().has_var(self.db) {
            return checked;
        }

        let applicable: Vec<_> = checked
            .iter()
            .copied()
            .filter(|(_, check)| {
                !matches!(
                    check,
                    TraitCandidateCheck::Rejected | TraitCandidateCheck::Unsatisfied(_)
                )
            })
            .collect();
        if applicable.is_empty() {
            checked
        } else {
            applicable
        }
    }

    fn candidate_specializes_to(
        &self,
        candidate: TraitMethodCand<'db>,
        confirmed: TraitMethodCand<'db>,
    ) -> Result<bool, NormalizationLimit> {
        let mut table = UnificationTable::new(self.db);
        let candidate_inst = self.receiver.extract_solution(&mut table, candidate.inst);
        let confirmed_inst = self.receiver.extract_solution(&mut table, confirmed.inst);
        if candidate_inst.def(self.db) != confirmed_inst.def(self.db)
            || candidate.method.name(self.db) != confirmed.method.name(self.db)
        {
            return Ok(false);
        }

        let solve_cx = TraitSolveCx::new(self.db, self.scope).with_assumptions(self.assumptions);
        let query = CanonicalGoalQuery::new(self.db, candidate_inst, self.assumptions);
        let confirmed = Canonical::new(self.db, confirmed_inst);
        let mut table = UnificationTable::new(self.db);
        Ok(
            match is_goal_query_satisfiable(self.db, solve_cx, &query)? {
                GoalSatisfiability::Satisfied(solution) => {
                    Canonical::new(self.db, query.extract_solution(&mut table, solution).inst)
                        == confirmed
                }
                GoalSatisfiability::NeedsConfirmation {
                    solutions,
                    completion,
                } => {
                    let reached_answer_cutoff = completion.hit_root_answer_limit();
                    let contains_confirmed = solutions.into_iter().any(|solution| {
                        Canonical::new(self.db, query.extract_solution(&mut table, solution).inst)
                            == confirmed
                    });
                    contains_confirmed
                        || (reached_answer_cutoff
                            && goal_query_has_solution(self.db, solve_cx, &query, confirmed))
                }
                GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_) => false,
            },
        )
    }

    fn check_trait_cand(&self, cand: AssembledTraitMethodCand<'db>) -> TraitCandidateCheck<'db> {
        match cand {
            AssembledTraitMethodCand::Impl {
                implementor,
                method,
            } => self.check_impl_cand(implementor, method),
            AssembledTraitMethodCand::Assumption { inst, method } => {
                match self.check_inst(inst, method) {
                    (MethodCandidate::TraitMethod(cand), None) => {
                        TraitCandidateCheck::Confirmed(cand)
                    }
                    (MethodCandidate::NeedsConfirmation(cand), None) => {
                        TraitCandidateCheck::NeedsConfirmation(cand)
                    }
                    (MethodCandidate::TraitMethod(cand), Some(limit))
                    | (MethodCandidate::NeedsConfirmation(cand), Some(limit)) => {
                        TraitCandidateCheck::Unknown(cand, limit)
                    }
                    (MethodCandidate::InherentMethod(_), _) => unreachable!(),
                }
            }
        }
    }

    fn check_impl_cand(
        &self,
        implementor: ImplementorId<'db>,
        method: Func<'db>,
    ) -> TraitCandidateCheck<'db> {
        let mut table = UnificationTable::new(self.db);
        let receiver_ty = self.receiver.canonical().extract_identity(&mut table);
        let implementor = table.instantiate_with_fresh_vars(implementor);
        let impl_ty = table.instantiate_to_term(implementor.self_ty(self.db));
        let receiver_ty = table.instantiate_to_term(receiver_ty);
        if table.unify(impl_ty, receiver_ty).is_err() {
            return TraitCandidateCheck::Rejected;
        }

        let solve_cx = TraitSolveCx::new(self.db, self.scope).with_assumptions(self.assumptions);
        let holds = candidates::all_hold(
            self.db,
            solve_cx,
            &mut table,
            implementor.constraints(self.db).list(self.db),
            Counting::METHOD,
        );

        let inst = implementor
            .trait_inst(self.db)
            .fold_with(self.db, &mut table);
        let cand = TraitMethodCand::new(
            self.receiver
                .canonicalize_solution(self.db, &mut table, inst),
            method,
        );
        match holds {
            Holds::Yes => TraitCandidateCheck::Confirmed(cand),
            Holds::Undecided => TraitCandidateCheck::NeedsConfirmation(cand),
            Holds::No => TraitCandidateCheck::Unsatisfied(cand),
            Holds::Unknown(limit) => TraitCandidateCheck::Unknown(cand, limit),
        }
    }

    /// Finds an instance of a trait method for the given trait definition and
    /// method.
    ///
    /// This function attempts to unify the receiver type with the method's self
    /// type, and assigns type variables to the trait parameters. It then
    /// checks if the goal is satisfiable given the current assumptions.
    /// Depending on the result, it either returns a confirmed trait method
    /// candidate or one that needs further confirmation, with the limit that
    /// leaves it unknown, if any.
    fn check_inst(
        &self,
        inst: TraitInstId<'db>,
        method: Func<'db>,
    ) -> (MethodCandidate<'db>, Option<NormalizationLimit>) {
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
            table.instantiate_with_fresh_vars(inst)
        };

        let (result, limit) = match is_goal_query_satisfiable(
            self.db,
            TraitSolveCx::new(self.db, self.scope).with_assumptions(self.assumptions),
            &query,
        ) {
            Ok(result) => (result, None),
            Err(limit) => (GoalSatisfiability::ContainsInvalid, Some(limit)),
        };
        (
            match result {
                GoalSatisfiability::Satisfied(solution) => {
                    // Map back the solution to the current context.
                    let solution = query.extract_solution(&mut table, solution).inst;
                    // Replace TyParams in the solved instance with fresh inference vars so
                    // downstream unification can bind them (e.g., T = u32). For receiver type
                    // parameters, keep the bound's args intact.
                    let solution = if receiver_is_ty_param {
                        solution
                    } else {
                        table.instantiate_with_fresh_vars(solution)
                    };

                    MethodCandidate::TraitMethod(TraitMethodCand::new(
                        self.receiver
                            .canonicalize_solution(self.db, &mut table, solution),
                        method,
                    ))
                }

                GoalSatisfiability::NeedsConfirmation { .. }
                | GoalSatisfiability::ContainsInvalid
                | GoalSatisfiability::UnSat(_) => {
                    MethodCandidate::NeedsConfirmation(TraitMethodCand::new(
                        self.receiver
                            .canonicalize_solution(self.db, &mut table, inst),
                        method,
                    ))
                }
            },
            limit,
        )
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
    /// Checking a candidate reached a normalization limit.
    NormalizationLimit(NormalizationLimit),
}

#[derive(Default)]
struct AssembledCandidates<'db> {
    inherent_methods: FxHashSet<ProbedMethod<'db>>,
    traits: IndexSet<AssembledTraitMethodCand<'db>>,
    /// A limit reached matching an inherent method's receiver.
    limit: Option<NormalizationLimit>,
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
            .map(|imp| imp.trait_(&db).pretty_print(&db, false))
            .collect();

        assert!(
            impls.iter().any(|imp| imp.trait_def(&db) == wordrepr),
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
use std::evm::{RawStorage, StorageMap}

fn test_it() uses (storage: mut RawStorage) {
    let _map: StorageMap<Address, Address, 0> = StorageMap::new()
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
    }
}
