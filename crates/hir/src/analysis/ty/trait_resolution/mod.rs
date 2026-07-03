use super::{
    binder::Binder,
    canonical::{Canonical, Canonicalized, Solution},
    const_expr::ConstExpr,
    const_ty::{ConstTyData, EvaluatedConstTy},
    fold::{AssocTySubst, TyFoldable},
    trait_def::{ImplementorId, ImplementorOrigin, TraitInstId, impls_for_trait_in_ingots},
    ty_def::{TyData, TyFlags, TyId},
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        diagnostics::{TraitConstraintDiag, TyDiagCollection},
        trait_resolution::{constraint::ty_constraints, proof_forest::ProofForest},
        ty_check::ty_const_predicate_violation,
        unify::UnificationTable,
    },
};
use crate::{
    Ingot,
    hir_def::{Body, HirIngot, scope_graph::ScopeId},
    span::DynLazySpan,
};
use common::indexmap::IndexSet;
use constraint::collect_constraints;
use rustc_hash::FxHashSet;
use salsa::Update;

pub(crate) mod constraint;
mod proof_forest;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct TraitSolverQuery<'db> {
    pub goal: TraitInstId<'db>,
    pub assumptions: PredicateListId<'db>,
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
pub(crate) enum Selection<T> {
    Unique(T),
    Ambiguous(IndexSet<T>),
    NotFound,
}

// SALSA-KEY INVARIANT (rung 3.1): `TraitSolveCx` is passed BY VALUE into the
// `#[salsa::tracked]` fns `check_ty_wf`/`check_trait_inst_wf` (below), where its
// `Hash`/`Eq` form part of the salsa cache key and its `Update` governs
// revalidation. `scope` is pure carry-context — nothing in any tracked
// computation reads it (rung 3.4 will), and it is always functionally
// determined by `origin_ingot == scope.ingot(db)`. Two contexts that differ
// ONLY in `scope` therefore produce identical results from every tracked fn, so
// `scope` MUST be excluded from `PartialEq`/`Eq`/`Hash`/`Update`; otherwise
// impl-resolution memoization would shatter across distinct scopes sharing an
// ingot. Hence the manual impls below instead of `#[derive(...)]`.
#[derive(Debug, Clone, Copy)]
pub struct TraitSolveCx<'db> {
    origin_ingot: Ingot<'db>,
    assumptions: PredicateListId<'db>,
    /// Lexical scope the query was raised in. Pure carry-context in rung 3.1 —
    /// retained for scope-chain provision lookup (innermost-wins) in rung 3.4;
    /// deliberately excluded from the salsa cache key (see invariant above).
    scope: ScopeId<'db>,
}

impl<'db> PartialEq for TraitSolveCx<'db> {
    fn eq(&self, other: &Self) -> bool {
        // `scope` excluded — see SALSA-KEY INVARIANT above.
        self.origin_ingot == other.origin_ingot && self.assumptions == other.assumptions
    }
}

impl<'db> Eq for TraitSolveCx<'db> {}

impl<'db> std::hash::Hash for TraitSolveCx<'db> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // `scope` excluded — must stay consistent with `PartialEq` above.
        self.origin_ingot.hash(state);
        self.assumptions.hash(state);
    }
}

unsafe impl<'db> Update for TraitSolveCx<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        // `scope` excluded from the change decision (consistent with `Eq`): a
        // context differing only in `scope` is NOT a salsa change. We still
        // refresh the stored `scope` to the latest value so the carry-context
        // never goes stale, but report "unchanged" so downstream memoized
        // results are not invalidated.
        if old_value.origin_ingot == new_value.origin_ingot
            && old_value.assumptions == new_value.assumptions
        {
            old_value.scope = new_value.scope;
            false
        } else {
            *old_value = new_value;
            true
        }
    }
}

/// SSOT read-wrapper for the `(scope, assumptions)` pair that every body-checker
/// provision query needs. Building the solver context goes through exactly one
/// place ([`ProvisionEnv::solve_cx`]) so the `(scope, assumptions) -> TraitSolveCx`
/// triple is never hand-assembled at call sites.
#[derive(Debug, Clone, Copy)]
pub(crate) struct ProvisionEnv<'db> {
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
}

impl<'db> ProvisionEnv<'db> {
    /// Build a provision environment for an explicit `(scope, assumptions)` pair.
    ///
    /// The body checker's *current* provision env comes from
    /// [`TyCheckEnv::provision_env`]; legs that verify a candidate in a
    /// *specific* effect scope (e.g. the effect-provider verify leg, which scans
    /// a lexical scope to enumerate a provider and then verifies `ProviderTy:
    /// Trait` against that scope's assumptions) supply their own `(scope,
    /// assumptions)` here so solver-context construction still flows through the
    /// single [`ProvisionEnv::solve_cx`] site rather than being hand-assembled.
    ///
    /// [`TyCheckEnv::provision_env`]: crate::analysis::ty::ty_check::env::TyCheckEnv::provision_env
    pub(crate) fn for_scope(
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> ProvisionEnv<'db> {
        ProvisionEnv { scope, assumptions }
    }

    /// Build the trait-solver context for this provision environment. This is the
    /// single construction site for a body-checker `TraitSolveCx`.
    pub(crate) fn solve_cx(&self, db: &'db dyn HirAnalysisDb) -> TraitSolveCx<'db> {
        TraitSolveCx::new(db, self.scope).with_assumptions(self.assumptions)
    }

    // `assumptions`/`scope` round out the SSOT read surface so rung 3.1+ can read
    // either dimension through `ProvisionEnv` instead of `TyCheckEnv` directly.
    // In rung 3.0 only `solve_cx` has non-test callers yet.
    #[allow(dead_code)]
    pub(crate) fn assumptions(&self) -> PredicateListId<'db> {
        self.assumptions
    }

    #[allow(dead_code)]
    pub(crate) fn scope(&self) -> ScopeId<'db> {
        self.scope
    }
}

impl<'db> TraitSolveCx<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Self {
        Self {
            origin_ingot: scope.ingot(db),
            assumptions: PredicateListId::empty_list(db),
            scope,
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

    /// The lexical scope this solver context was raised in. Consumed in rung 3.4
    /// for scope-chain provision lookup (innermost-wins); nothing reads it for
    /// resolution in rung 3.1, and it is excluded from the salsa cache key.
    #[allow(dead_code)]
    pub(crate) fn scope(self) -> ScopeId<'db> {
        self.scope
    }

    pub(crate) fn select_impl(
        self,
        db: &'db dyn HirAnalysisDb,
        inst: TraitInstId<'db>,
    ) -> Selection<ImplementorId<'db>> {
        let scope = self.normalization_scope_for_trait_inst(db, inst);
        let inst = normalize_trait_inst_preserving_validity(db, inst, scope, self.assumptions);
        match is_goal_satisfiable(db, self, inst) {
            GoalSatisfiability::Satisfied(solution) => {
                Selection::Unique(solution.value.implementor)
            }
            GoalSatisfiability::NeedsConfirmation(ambiguous) => {
                // FCO "slide" cascade C3c-2 — the DEFAULT-TIER rule, consulted
                // OUTSIDE the tracked `is_goal_satisfiable` solve (this match arm
                // runs on the solve's *result*, so the salsa-key invariant at
                // `mod.rs:109-164` — `scope` excluded from the tracked key — is
                // untouched). When >1 REAL coexisting impl applies to a concrete
                // non-canonical goal and exactly one is default-marked
                // (CoreDerives-origin), collapse the ambiguity to that default so
                // an unscoped call resolves deterministically (never a MIR
                // `select_impl`→`Ambiguous`→panic). None/many marked, or any
                // candidate carrying inference vars, leaves the ambiguity intact
                // (a clean diagnostic upstream). With C3c-3 LIVE the demotion only
                // admits coexistence in the cascade's default+override shape (the
                // gate at `core/semantic/mod.rs` requires exactly one default), so
                // this engages exactly there; for code without a coexisting pair
                // `default_tier_selection` finds ≤1 applying candidate ⇒ `None` ⇒
                // this falls through to `Ambiguous` (byte-identical).
                if let Some(Selection::Unique(default)) = self.default_tier_selection(db, inst) {
                    return Selection::Unique(default);
                }
                Selection::Ambiguous(ambiguous.iter().map(|s| s.value.implementor).collect())
            }
            GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_) => {
                Selection::NotFound
            }
        }
    }

    /// FCO "slide" cascade C3c-2 — the DEFAULT-TIER rule (LIVE with C3c-3).
    ///
    /// When >1 real coexisting `Hir` impl applies to one concrete goal and no
    /// in-scope provision selected one (the unscoped/default tier), this picks the
    /// DEFAULT-marked impl: the one whose provenance is a canonical core-derive
    /// provider (see [`Self::implementor_is_default_marked`]). It is the
    /// disambiguator consumed by [`Self::select_impl`]'s `Ambiguous` arm —
    /// scope-free, run OUTSIDE the tracked `is_query_satisfiable` solve so the
    /// cache-safety invariant (`trait_resolution/mod.rs:109-164`) is untouched.
    ///
    /// Returns:
    /// - `None` — the rule DOES NOT ENGAGE: ≤1 coexisting impl applies to the
    ///   goal, OR the goal still carries inference vars (the inference-ambiguity
    ///   path). For code WITHOUT a cascade default+override pair the applying set
    ///   is ≤1, so this is `None` and the caller falls back to today's path
    ///   unchanged → byte-identical. Coexistence is admitted only in the cascade's
    ///   default+override shape (the gate in `core/semantic/mod.rs` requires
    ///   exactly one default-marked impl), so the engage branch fires there.
    /// - `Some(Selection::Unique(impl))` — >1 applied AND exactly one is
    ///   default-marked: select it deterministically (recorded so MIR consumes it
    ///   via the C1 rail, never reaching `select_impl`→`LowerError`→panic at
    ///   `classify.rs:2297`).
    /// - `Some(Selection::Ambiguous(applying))` — >1 applied but none or >1 is
    ///   default-marked: this function does not itself diagnose or panic here —
    ///   it hands `Ambiguous` back to `select_impl`'s caller. Some callers (e.g.
    ///   `resolve_trait_method_instance`) just return `None`/`false` on
    ///   `Ambiguous`, with no diagnostic emitted at this point. What actually
    ///   prevents a backend panic on this path today is a separate runtime
    ///   pre-flight check added for the B-1 de-panic fix
    ///   (`check_runtime_trait_calls_resolvable`, `crates/mir/src/runtime/lower/body.rs`),
    ///   extended to walk the transitive callee graph so a call reached only
    ///   through return-class inference on a callee is covered too
    ///   (`check_reachable_runtime_trait_calls_resolvable`, same file), which
    ///   surfaces an unresolved selection as a clean
    ///   `LowerError::UnresolvedTraitSelection` pointing at `with (...)`
    ///   disambiguation on every reachable resolution route — not a guarantee
    ///   this function makes on its own. The
    ///   ty_check-level `AmbiguousTraitInst` diagnostic (`ty_check/mod.rs:1478`)
    ///   is a separate leg that can go unreached for concrete-goal coexistence
    ///   today (see the dedup note at `ty_check/mod.rs:1133`); making it
    ///   reliably reachable is a deferred cascade follow-up.
    /// - `Some(Selection::NotFound)` never occurs (the engage gate already
    ///   requires applying candidates).
    ///
    /// SCOPING (the feasibility gate): this looks ONLY at the impl-table candidate
    /// set (`impls_for_trait_in_ingots`) — how many *real coexisting impls* apply
    /// to a concrete goal — and NEVER at a `GoalSatisfiability`. So it cannot
    /// perturb the inference-variable ambiguity path
    /// (`process_trait_obligation`'s `NeedsConfirmation` →
    /// `BodyDiag::AmbiguousTraitInst`, `ty_check/mod.rs:1385`), which is fed by
    /// inference vars, not by coexisting coherent impls. The two cases are
    /// structurally distinct here.
    ///
    /// Consumed (C3c-2 LIVE) by [`Self::select_impl`]'s `Ambiguous` arm — the
    /// single impl-selection SSOT — so an unscoped call over >1 coexisting impl
    /// resolves deterministically to the default; and unit-tested synthetically
    /// (`default_tier_rule_tests`). LATENT until C3c-3 demotes coherence: today
    /// `applying.len() <= 1` ⇒ `None` ⇒ byte-identical.
    pub(crate) fn default_tier_selection(
        self,
        db: &'db dyn HirAnalysisDb,
        inst: TraitInstId<'db>,
    ) -> Option<Selection<ImplementorId<'db>>> {
        // CONCRETE-GOAL GATE: the default-tier rule is for the UNSCOPED-call case
        // (a concrete goal with >1 coexisting coherent impl). A goal still carrying
        // inference variables belongs to the inference-ambiguity path
        // (`NeedsConfirmation` → `AmbiguousTraitInst`), NOT here: its vars live in
        // the CALLER's unification table, so instantiating candidates against it in
        // our fresh table (`implementor_applies_to_goal`) would mix foreign keys
        // (out-of-bounds in `TyVarResolver`). Leave such goals to the caller's
        // normal path. Checked on the raw goal, before normalization touches it.
        if crate::analysis::ty::visitor::collect_flags(db, inst).contains(TyFlags::HAS_VAR) {
            return None;
        }

        let scope = self.normalization_scope_for_trait_inst(db, inst);
        let inst = normalize_trait_inst_preserving_validity(db, inst, scope, self.assumptions);

        // The impl-table candidate set for this goal's trait, across the same
        // deterministic ingots the proof forest searches.
        let (primary, secondary) = self.search_ingots_for_trait_inst(db, inst);
        let cands = impls_for_trait_in_ingots(db, primary, secondary, Canonical::new(db, inst));

        // Restrict to candidates that ACTUALLY apply to the goal (instantiate with
        // fresh vars, normalize, unify) — mirroring the proof forest's candidate
        // match (`proof_forest.rs:382-396`). This is the set of coexisting
        // coherent impls for the goal.
        let applying: Vec<ImplementorId<'db>> = cands
            .iter()
            .copied()
            .filter(|&cand| {
                Self::implementor_applies_to_goal(db, cand, inst, scope, self.assumptions)
            })
            .map(|cand| cand.instantiate_identity())
            .collect();

        // ENGAGE GATE: only >1 coexisting impl reaches the default-tier decision.
        // Without a cascade default+override pair the demotion (C3c-3) still
        // forbids a second impl, so `applying` is ≤1 and the rule no-ops here
        // (byte-identical for non-cascade code).
        if applying.len() <= 1 {
            return None;
        }

        let discriminated: Vec<(ImplementorId<'db>, SelDiscriminator<'db>)> = applying
            .into_iter()
            .map(|implementor| (implementor, selection_discriminator(db, implementor)))
            .collect();

        Some(default_tier_decision(discriminated))
    }

    /// FCO "slide" cascade C1 SOUNDNESS BACKSTOP — whether `implementor` is a
    /// REAL, valid impl for `goal`: a member of the same impl-table candidate set
    /// the solver searches (`impls_for_trait_in_ingots`) that ACTUALLY APPLIES to
    /// the (normalized) goal (`implementor_applies_to_goal`).
    ///
    /// This is the validity predicate the MIR C1 rail (`classify.rs`) checks
    /// before it CONSUMES a recorded `ImplEnv::selected_implementor` as the
    /// resolution source on its `Some` branch — the one path that otherwise trusts
    /// a recorded implementor with no cross-check. It is NOT "recorded == default
    /// re-resolution": a scoped override (`with (<T as Trait>)`) legitimately picks
    /// a non-default candidate, and any such override is STILL a member of this set
    /// (it is one of the goal's coexisting coherent impls), so this accepts every
    /// legitimate override while rejecting a forged/mismatched record (an impl that
    /// is not in the goal's candidate set, or does not unify with the goal/self-ty).
    ///
    /// Membership keys on interned `ImplementorId` identity: both the candidate set
    /// (`cand.instantiate_identity()`) and a recorded implementor (a solver
    /// solution, registered as `cand.instantiate_identity()` in
    /// `proof_forest.rs::step`) are the raw, un-substituted candidate id, so a
    /// genuinely-selected impl always compares equal here. BYTE-IDENTICAL today:
    /// every recorded implementor is a solver solution = a candidate that applied
    /// to the goal, so this is always `true` on valid Fe and never makes the MIR
    /// check fire. The reuse of `implementor_applies_to_goal` (the over-approximate
    /// apply test) keeps the predicate from ever under-counting — it can only
    /// admit, never spuriously reject, a real applying candidate.
    ///
    /// `pub` (rather than `pub(crate)`) so the MIR C1 rail (`fe-mir`) can run the
    /// backstop through the same `TraitSolveCx` it already builds for resolution,
    /// mirroring the cross-crate `check_reresolution_determinism` entry point.
    pub fn recorded_implementor_is_valid_candidate(
        self,
        db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
        implementor: ImplementorId<'db>,
    ) -> bool {
        let scope = self.normalization_scope_for_trait_inst(db, goal);
        let goal = normalize_trait_inst_preserving_validity(db, goal, scope, self.assumptions);

        // The impl-table candidate set for this goal's trait, across the same
        // deterministic ingots the proof forest searches.
        let (primary, secondary) = self.search_ingots_for_trait_inst(db, goal);
        let cands = impls_for_trait_in_ingots(db, primary, secondary, Canonical::new(db, goal));

        // The recorded implementor is valid iff it is one of those candidates AND
        // it actually applies to the goal (instantiate + normalize + unify) — the
        // exact membership the solver would have established when it selected it.
        cands.iter().copied().any(|cand| {
            cand.instantiate_identity() == implementor
                && Self::implementor_applies_to_goal(db, cand, goal, scope, self.assumptions)
        })
    }

    /// Whether a candidate implementor applies to `goal`: instantiate it with
    /// fresh vars, normalize its trait instance, and unify it against the
    /// (already-normalized) goal — exactly the proof forest's candidate match
    /// (`proof_forest.rs:382-396`), minus the constraint sub-solve (an
    /// over-approximation that is sound for the latency gate: it can only
    /// over-count applying candidates, never under-count, so it cannot make the
    /// `<= 1` gate fire spuriously when >1 genuinely coexist).
    fn implementor_applies_to_goal(
        db: &'db dyn HirAnalysisDb,
        cand: Binder<ImplementorId<'db>>,
        goal: TraitInstId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> bool {
        let mut table = UnificationTable::new(db);
        let gen_cand = table.instantiate_with_fresh_vars(cand);
        let normalized_cand = normalize_trait_inst_preserving_validity(
            db,
            gen_cand.trait_inst(db),
            scope,
            assumptions,
        );
        table.unify(normalized_cand, goal).is_ok()
    }

    /// DEFAULT-marked recognition (precise): an implementor is default-marked iff
    /// it is the output of a CANONICAL core-derive provider — i.e. its
    /// [`ImplementorOrigin`] is `Hir(impl_trait)`, that impl was generated from a
    /// derive site (`Desugared(Derive(..))`), and the provider that produced it
    /// lives in a [`IngotKind::CoreDerives`] ingot. This is the "derive the
    /// default" half of the cascade: a `#[derive(Eq)]`/`derive Eq for T` impl
    /// (whose generator is the canonical `core_derives` `StableEq`) is the
    /// DEFAULT; a hand-written `impl Eq for T` is the OVERRIDE.
    ///
    /// Recognition keys on the RESOLVED PROVENANCE PROVIDER's ingot
    /// ([`derived_impl_provenance`] — the same provider re-identification the
    /// derive selection used), never on the generated impl's own ingot (which is
    /// the *user's* file, not `core_derives`) and never on a bare name. So:
    /// - a hand-written impl (no `Desugared(Derive)` origin) → NOT marked;
    /// - a derive backed by a NON-core (`using MyProvider`) provider → NOT marked
    ///   (it is a custom default, not the canonical core one);
    /// - a `VirtualContract` or an `Assumption` → NEVER marked.
    ///
    /// [`derived_impl_provenance`]: crate::core::lower::derived_impl_provenance
    pub(crate) fn implementor_is_default_marked(
        db: &'db dyn HirAnalysisDb,
        implementor: ImplementorId<'db>,
    ) -> bool {
        match implementor.origin(db) {
            ImplementorOrigin::Hir(impl_trait) => crate::core::lower::derived_impl_provenance(
                db, impl_trait,
            )
            .is_some_and(|provenance| {
                provenance.provider.top_mod(db).ingot(db).kind(db)
                    == common::ingot::IngotKind::CoreDerives
            }),
            ImplementorOrigin::VirtualContract(_) | ImplementorOrigin::Assumption => false,
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

/// FCO T-Nway — the UNIFIED SELECTION DISCRIMINATOR for a non-canonical
/// `(Trait, Type)` impl. The cascade's three coordinated selection sites
/// (coherence, the unscoped default tier, scoped `with` selection) all branch on
/// this single per-impl discriminator, GENERALIZING the old binary
/// `implementor_is_default_marked` ({default, override}) into the N-way shape:
///
/// - [`SelDiscriminator::Default`] — the impl is the CoreDerives-origin derived
///   default ([`TraitSolveCx::implementor_is_default_marked`]). At most one per
///   goal.
/// - [`SelDiscriminator::Alias`]`(name)` — the impl carries an `as Name` user
///   alias (inc1, [`ImplTrait::hir_alias`]). Distinct per name; `with (Name)`
///   selects it.
/// - [`SelDiscriminator::Anonymous`] — a hand-written, unaliased,
///   non-default-marked impl. At most one (the hand-written default/override).
///
/// EQUALITY (the coherence / selection key): `Default == Default`,
/// `Anonymous == Anonymous`, `Alias(a) == Alias(a)`; everything else `!=`.
/// Restricted to the no-alias case the only discriminators are `Default` /
/// `Anonymous`, so this is byte-identical to the old `is_default_marked` binary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SelDiscriminator<'db> {
    Default,
    Alias(crate::hir_def::IdentId<'db>),
    Anonymous,
}

/// Compute the [`SelDiscriminator`] for `implementor` (see the type docs).
///
/// `Default` takes precedence (the CoreDerives-origin default is never *also*
/// selectable by an `as Name` alias — a derived impl carries no user alias).
/// Otherwise, an `as Name` alias makes it `Alias(name)`; failing that it is
/// `Anonymous`. A `VirtualContract` / `Assumption` implementor (no HIR item, no
/// alias, not default-marked) is `Anonymous`.
pub(crate) fn selection_discriminator<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
) -> SelDiscriminator<'db> {
    if TraitSolveCx::implementor_is_default_marked(db, implementor) {
        return SelDiscriminator::Default;
    }
    if let ImplementorOrigin::Hir(impl_trait) = implementor.origin(db)
        && let Some(crate::hir_def::Partial::Present(name)) = impl_trait.hir_alias(db)
    {
        return SelDiscriminator::Alias(name);
    }
    SelDiscriminator::Anonymous
}

/// FCO "slide" cascade C3c-2 / T-Nway — the pure DEFAULT-TIER decision over a set
/// of ≥2 coexisting candidate implementors that all apply to one goal, each paired
/// with its [`SelDiscriminator`]. Factored out so the decision is unit-testable
/// synthetically without needing the impl table to physically hold >1 impl.
///
/// The UNSCOPED default (no selecting `with` in view) is:
/// - the sole [`SelDiscriminator::Default`] impl if present (the derived default);
/// - ELSE the sole [`SelDiscriminator::Anonymous`] impl (the hand-written default);
/// - else [`Selection::Ambiguous`] over ALL candidates. This function does not
///   itself diagnose or panic on that path — see the caveat on the `Ambiguous`
///   case of `default_tier_selection`, above: whether the caller turns this
///   into a clean "disambiguate with `with`" diagnostic depends on which
///   caller and which leg reaches it, and a backend panic is avoided today by
///   a separate runtime pre-flight guard added for B-1, not by this decision.
///
/// `Alias`'d impls are NEVER the unscoped default — only `with (Name)` selects
/// them — so they are excluded from the default candidates here (but still appear
/// in the `Ambiguous` list, which enumerates everything that applies).
///
/// BYTE-IDENTICAL today: the only non-aliased shape coherence currently permits is
/// {`Default`, `Anonymous`}, where exactly one `Default` exists → `Unique(Default)`
/// — identical to the old "exactly one default-marked → `Unique`". The
/// `Anonymous`-fallback is reachable only once aliases let ≥2 non-marked impls
/// coexist (the new N-way case).
fn default_tier_decision<'db>(
    discriminated: Vec<(ImplementorId<'db>, SelDiscriminator<'db>)>,
) -> Selection<ImplementorId<'db>> {
    let with_disc = |want: fn(&SelDiscriminator<'db>) -> bool| -> Vec<ImplementorId<'db>> {
        discriminated
            .iter()
            .filter_map(|&(implementor, disc)| want(&disc).then_some(implementor))
            .collect()
    };

    let defaults = with_disc(|d| matches!(d, SelDiscriminator::Default));
    if let [unique] = defaults.as_slice() {
        return Selection::Unique(*unique);
    }
    // No clear `Default`: fall back to the sole hand-written `Anonymous` default.
    if defaults.is_empty() {
        let anonymous = with_disc(|d| matches!(d, SelDiscriminator::Anonymous));
        if let [unique] = anonymous.as_slice() {
            return Selection::Unique(*unique);
        }
    }
    Selection::Ambiguous(
        discriminated
            .into_iter()
            .map(|(implementor, _)| implementor)
            .collect(),
    )
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

#[salsa::tracked(return_ref)]
fn is_query_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    origin_ingot: Ingot<'db>,
    query: Canonical<TraitSolverQuery<'db>>,
) -> GoalSatisfiability<'db> {
    if query.flags(db).contains(TyFlags::HAS_INVALID) {
        return GoalSatisfiability::ContainsInvalid;
    };

    ProofForest::new(db, origin_ingot, query).solve()
}

pub fn is_goal_query_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    query: &CanonicalGoalQuery<'db>,
) -> GoalSatisfiability<'db> {
    is_query_satisfiable(db, solve_cx.origin_ingot(), query.canonical()).clone()
}

pub fn is_goal_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> GoalSatisfiability<'db> {
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
    let (base, args) = ty.decompose_ty_app(db);
    for &arg in args {
        let wf = check_ty_wf(db, solve_cx, arg);
        if !wf.is_wf() {
            return wf;
        }
    }
    match base.data(db) {
        TyData::AssocTy(assoc) => {
            let wf = check_projected_trait_use_wf(db, solve_cx, assoc.trait_);
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
        | TyData::ConstraintTerm(_)
        | TyData::TraitCtor(_)
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
        let query = CanonicalGoalQuery::new(db, goal, assumptions);

        if let GoalSatisfiability::UnSat(subgoal) = is_goal_query_satisfiable(db, solve_cx, &query)
        {
            let subgoal = subgoal.map(|subgoal| query.extract_subgoal(&mut table, subgoal));
            return WellFormedness::IllFormed { goal, subgoal };
        }
    }

    // A concrete ADT application is also ill-formed if its own `where`-clause
    // const predicates are refuted under its arguments. CTFE runs here, at the
    // well-formedness layer, never inside the proof forest.
    if let Some(predicate) = ty_const_predicate_violation(db, ty) {
        return WellFormedness::IllFormedConstPredicate { predicate };
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
        ConstTyData::Evaluated(EvaluatedConstTy::Tuple(elems), _)
        | ConstTyData::Evaluated(EvaluatedConstTy::Array(elems), _)
        | ConstTyData::Evaluated(EvaluatedConstTy::Record(elems), _) => {
            for &elem in elems {
                let wf = check_ty_wf(db, solve_cx, elem);
                if !wf.is_wf() {
                    return wf;
                }
            }
        }
        ConstTyData::Abstract(expr, _) => {
            let wf = check_const_expr_wf(db, solve_cx, *expr);
            if !wf.is_wf() {
                return wf;
            }
        }
        ConstTyData::TyVar(..)
        | ConstTyData::TyParam(..)
        | ConstTyData::Hole(..)
        | ConstTyData::Evaluated(..)
        | ConstTyData::UnEvaluated { .. } => {}
    }

    WellFormedness::WellFormed
}

fn check_const_expr_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    expr: super::const_expr::ConstExprId<'db>,
) -> WellFormedness<'db> {
    match expr.data(db) {
        ConstExpr::ExternConstFnCall {
            generic_args, args, ..
        }
        | ConstExpr::UserConstFnCall {
            generic_args, args, ..
        } => {
            for &ty in generic_args.iter().chain(args.iter()) {
                let wf = check_ty_wf(db, solve_cx, ty);
                if !wf.is_wf() {
                    return wf;
                }
            }
        }
        ConstExpr::ArithBinOp { lhs, rhs, .. } => {
            for ty in [*lhs, *rhs] {
                let wf = check_ty_wf(db, solve_cx, ty);
                if !wf.is_wf() {
                    return wf;
                }
            }
        }
        ConstExpr::UnOp { expr, .. } => {
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
        ConstExpr::LocalBinding(_) => {}
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

    // The projection base `<X as Tr>` is well-formed when `X: Tr` holds by definition, even
    // when the surrounding header assumptions deliberately omit the corresponding predicate
    // (see `header_constraints_for`, which drops the `Self: Trait` self-predicate to keep
    // projection from recursing through the in-progress trait). Two such structural cases:
    let self_ty = inst.self_ty(db);

    // 1. `<Self as ThisTrait>`: a trait's own self-parameter implements the trait by
    //    definition, so projecting the trait's own associated types in its header
    //    (e.g. `Self::Item` in `trait Direct: A<Self::Item>`) is always well-formed.
    if Some(self_ty)
        == crate::analysis::ty::ty_lower::collect_generic_params(db, inst.def(db).into())
            .trait_self(db)
    {
        return WellFormedness::WellFormed;
    }

    // 2. `<T::Name as Tr>` where `type Name: Tr` is a declared bound on the associated type:
    //    every projection of `Name` satisfies its declared bounds for any `T`, so the
    //    projected trait use is well-formed (e.g. the `Self::Item::Assoc` second hop in
    //    `trait RecursiveSuper: A<Self::Item::Assoc> { type Item: RecursiveSuper }`).
    if let TyData::AssocTy(assoc) = self_ty.data(db)
        && let Some(assoc_view) = assoc
            .trait_
            .def(db)
            .assoc_types(db)
            .find(|t| t.name(db) == Some(assoc.name))
        && self_ty.assoc_type_bounds(db, assoc_view).any(|b| b == inst)
    {
        return WellFormedness::WellFormed;
    }

    unsatisfied_goal(db, solve_cx, inst).unwrap_or(WellFormedness::WellFormed)
}

fn unsatisfied_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goal: TraitInstId<'db>,
) -> Option<WellFormedness<'db>> {
    let assumptions = solve_cx.assumptions();
    let mut table = UnificationTable::new(db);
    let query = CanonicalGoalQuery::new(db, goal, assumptions);
    if let GoalSatisfiability::UnSat(subgoal) = is_goal_query_satisfiable(db, solve_cx, &query) {
        let subgoal = subgoal.map(|subgoal| query.extract_subgoal(&mut table, subgoal));
        Some(WellFormedness::IllFormed { goal, subgoal })
    } else {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
pub(crate) enum WellFormedness<'db> {
    WellFormed,
    IllFormed {
        goal: TraitInstId<'db>,
        subgoal: Option<TraitInstId<'db>>,
    },
    /// A concrete ADT application whose `where`-clause const predicate is
    /// refuted under its arguments (e.g. `Bounded<4, 1>` where `MIN <= MAX`).
    IllFormedConstPredicate {
        predicate: Body<'db>,
    },
}

impl<'db> WellFormedness<'db> {
    fn is_wf(self) -> bool {
        matches!(self, WellFormedness::WellFormed)
    }

    /// Renders this well-formedness result as a diagnostic at `span`, or `None`
    /// when well-formed. Every well-formedness consumer goes through this, so a
    /// new kind of ill-formedness cannot be silently dropped at a use site.
    pub(crate) fn into_diag(self, span: DynLazySpan<'db>) -> Option<TyDiagCollection<'db>> {
        match self {
            WellFormedness::WellFormed => None,
            WellFormedness::IllFormed { goal, subgoal } => Some(
                TraitConstraintDiag::TraitBoundNotSat {
                    span,
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                    required_by: None,
                }
                .into(),
            ),
            WellFormedness::IllFormedConstPredicate { predicate } => {
                Some(TraitConstraintDiag::ConstPredicateNotSat { span, predicate }.into())
            }
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
    for &arg in trait_inst.args(db) {
        let wf = check_ty_wf(db, solve_cx, arg);
        if !wf.is_wf() {
            return wf;
        }
    }
    for &ty in trait_inst.assoc_type_bindings(db).values() {
        let wf = check_ty_wf(db, solve_cx, ty);
        if !wf.is_wf() {
            return wf;
        }
    }

    let constraints =
        collect_constraints(db, trait_inst.def(db).into()).instantiate(db, trait_inst.args(db));
    let assumptions = solve_cx.assumptions();

    // Normalize constraints after instantiation to resolve associated types
    let normalized_constraints = {
        let scope = solve_cx.normalization_scope_for_trait_inst(db, trait_inst);
        let normalized_list: Vec<_> = constraints
            .list(db)
            .iter()
            .map(|&goal| goal.normalize(db, scope, assumptions))
            .collect();
        PredicateListId::new(db, normalized_list)
    };

    for &goal in normalized_constraints.list(db) {
        let mut table = UnificationTable::new(db);
        let query = CanonicalGoalQuery::new(db, goal, assumptions);
        if let GoalSatisfiability::UnSat(subgoal) = is_goal_query_satisfiable(db, solve_cx, &query)
        {
            let subgoal = subgoal.map(|subgoal| query.extract_subgoal(&mut table, subgoal));
            return WellFormedness::IllFormed { goal, subgoal };
        }
    }

    WellFormedness::WellFormed
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct TraitGoalSolution<'db> {
    pub(crate) inst: TraitInstId<'db>,
    pub(crate) implementor: ImplementorId<'db>,
}

impl<'db> TraitGoalSolution<'db> {
    /// The trait instance the solver committed to when it discharged the
    /// goal, instantiated in the environment the goal was raised in.
    pub fn inst(&self) -> TraitInstId<'db> {
        self.inst
    }

    /// Renders the impl that discharged the goal for human consumption, e.g.
    /// `impl Eq for Point`.
    ///
    /// Compiler-generated impls carry their provenance: impls expanded from
    /// `#[derive(..)]` or a standalone `derive` declaration are suffixed with
    /// `(derived)`, and goals proven directly from bounds in scope render as
    /// the assumed predicate instead of an impl.
    pub fn describe_implementor(&self, db: &'db dyn HirAnalysisDb) -> String {
        let trait_inst = self.implementor.trait_(db);
        let trait_str = trait_inst.pretty_print(db, false);
        let self_ty = trait_inst.self_ty(db).pretty_print(db);
        match self.implementor.origin(db) {
            ImplementorOrigin::Hir(impl_trait) => {
                let is_derived = matches!(
                    impl_trait.origin(db),
                    crate::span::HirOrigin::Desugared(crate::span::DesugaredOrigin::Derive(_))
                );
                if is_derived {
                    format!("impl {trait_str} for {self_ty} (derived)")
                } else {
                    format!("impl {trait_str} for {self_ty}")
                }
            }
            ImplementorOrigin::VirtualContract(_) => {
                format!("built-in impl {trait_str} for {self_ty}")
            }
            ImplementorOrigin::Assumption => {
                format!("bound {}", trait_inst.pretty_print(db, true))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub enum GoalSatisfiability<'db> {
    /// Goal is satisfied with the unique solution.
    Satisfied(Solution<TraitGoalSolution<'db>>),
    /// Goal might be satisfied, but needs more type information to determine
    /// satisfiability and uniqueness.
    NeedsConfirmation(IndexSet<Solution<TraitGoalSolution<'db>>>),

    /// Goal contains invalid.
    ContainsInvalid,
    /// The gaol is not satisfied.
    /// It contains an unsatisfied subgoal if we can know the exact subgoal
    /// that makes the proof step stuck.
    UnSat(Option<Solution<TraitInstId<'db>>>),
}

impl GoalSatisfiability<'_> {
    pub fn is_satisfied(&self) -> bool {
        matches!(
            self,
            Self::Satisfied(_) | Self::NeedsConfirmation(_) | Self::ContainsInvalid
        )
    }
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
        let mut predicates = self.list(db).clone();
        predicates.extend(other.list(db));
        PredicateListId::new(db, predicates)
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
        let mut all_predicates: IndexSet<TraitInstId<'db>> =
            self.list(db).iter().copied().collect();

        let mut worklist: Vec<TraitInstId<'db>> = self.list(db).to_vec();

        while let Some(pred) = worklist.pop() {
            // 1. Collect super traits
            for super_trait in pred.def(db).super_traits(db) {
                // Instantiate with current predicate's args
                let inst = super_trait.instantiate(db, pred.args(db));

                // Also substitute `Self` and associated types using current predicate's
                // assoc-type bindings so derived bounds are as concrete as possible.
                let mut subst = AssocTySubst::new(pred);
                let inst = inst.fold_with(db, &mut subst);
                if predicate_has_recursive_assoc_projection(db, inst) {
                    continue;
                }

                if all_predicates.insert(inst) {
                    // New predicate added, add to worklist for further processing
                    worklist.push(inst);
                }
            }

            // 2. Collect associated type bounds
            let hir_trait = pred.def(db);
            for trait_type in hir_trait.assoc_types(db) {
                // Get the associated type name
                let Some(assoc_ty_name) = trait_type.name(db) else {
                    continue;
                };

                // Create the associated type: Self::AssocType
                let assoc_ty = TyId::assoc_ty(db, pred, assoc_ty_name);

                let _assumptions =
                    PredicateListId::new(db, all_predicates.iter().copied().collect::<Vec<_>>());

                for mut trait_inst in assoc_ty.assoc_type_bounds(db, trait_type) {
                    // Substitute `Self` and associated types using the original predicate instance
                    let mut subst = AssocTySubst::new(pred);
                    trait_inst = trait_inst.fold_with(db, &mut subst);
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
    use common::indexmap::IndexMap;

    use super::{
        CanonicalGoalQuery, GoalSatisfiability, TraitInstId, TraitSolveCx,
        is_goal_query_satisfiable,
    };
    use crate::{
        analysis::ty::{
            trait_resolution::constraint::collect_func_def_constraints, ty_def::TyId,
            ty_lower::collect_generic_params,
        },
        hir_def::{Func, Trait},
        test_db::HirAnalysisTestDb,
    };

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
            is_goal_query_satisfiable(&db, with_cx, &with_query),
            GoalSatisfiability::Satisfied(_)
        ));
        assert!(matches!(
            is_goal_query_satisfiable(&db, without_cx, &without_query),
            GoalSatisfiability::UnSat(_)
        ));
    }
}

/// FCO "slide" cascade C3c-2 — the DEFAULT-TIER rule, exercised synthetically.
///
/// The rule fires ONLY in the future cascade state (>1 coexisting coherent impl
/// for one goal), which today's coherence forbids, so it cannot be reached on
/// real code. These tests therefore drive the two pieces directly:
///  - [`super::default_tier_decision`]: the selection over ≥2 coexisting
///    candidates (exactly-one-marked → `Unique`; none/two-marked → `Ambiguous`);
///  - [`TraitSolveCx::implementor_is_default_marked`]: the canonical-provenance
///    recognizer, proven anti-vacuously to REJECT a real `Local`-ingot `Hir` impl
///    and an `Assumption` implementor (the only origins constructible without a
///    `CoreDerives` ingot in the test DB).
#[cfg(test)]
mod default_tier_rule_tests {
    use camino::Utf8PathBuf;

    use super::{SelDiscriminator, Selection, TraitSolveCx, default_tier_decision};
    use crate::{
        analysis::{
            name_resolution::{PathRes, resolve_path},
            ty::{
                trait_def::{ImplementorId, TraitInstId, impls_for_trait_def},
                trait_resolution::PredicateListId,
                ty_def::TyId,
            },
        },
        hir_def::PathId,
        test_db::HirAnalysisTestDb,
    };

    /// A `Point` struct with a single real `impl Eq for Point` — a `Local`-ingot
    /// `Hir` implementor (NOT a canonical core-derive provider).
    const FIXTURE: &str = r#"
use core::ops::Eq

struct Point {
    x: u256,
    y: u256,
}

impl Eq for Point {
    fn eq(self, _ other: Point) -> bool {
        self.x == other.x
    }
}
"#;

    fn resolve<'db>(
        db: &'db HirAnalysisTestDb,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        segments: &[&str],
    ) -> PathRes<'db> {
        let path = PathId::from_segments(db, segments);
        resolve_path(db, path, scope, PredicateListId::empty_list(db), false)
            .unwrap_or_else(|e| panic!("expected {segments:?} to resolve, got {e:?}"))
    }

    /// The sole real `impl Eq for Point` implementor, via the impl table.
    fn sole_eq_point_impl<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: crate::hir_def::TopLevelMod<'db>,
        eq_inst: TraitInstId<'db>,
        point_ty: TyId<'db>,
    ) -> ImplementorId<'db> {
        impls_for_trait_def(db, top_mod.ingot(db), eq_inst.def(db))
            .iter()
            .map(|binder| binder.instantiate_identity())
            .find(|implementor| implementor.self_ty(db) == point_ty)
            .expect("expected a real impl Eq for Point")
    }

    /// The recognizer keys on RESOLVED canonical provenance, never on a name: a
    /// real `Local`-ingot `Hir` impl is NOT default-marked, and an `Assumption`
    /// implementor is NOT default-marked. (Anti-vacuous: if the recognizer always
    /// returned `true`, the `none-marked → Ambiguous` decision case below could
    /// pass for the wrong reason.)
    #[test]
    fn default_marked_recognizer_rejects_non_canonical_origins() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("default_tier_recognizer.fe"), FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let scope = top_mod.scope();

        let PathRes::Trait(eq_inst) = resolve(&db, scope, &["core", "ops", "Eq"]) else {
            panic!("core::ops::Eq must resolve to a trait");
        };
        let PathRes::Ty(point_ty) = resolve(&db, scope, &["Point"]) else {
            panic!("Point must resolve to a type");
        };

        let eq_point_impl = sole_eq_point_impl(&db, top_mod, eq_inst, point_ty);
        assert!(
            !TraitSolveCx::implementor_is_default_marked(&db, eq_point_impl),
            "a Local-ingot Hir impl must NOT be default-marked",
        );

        let eq_point = TraitInstId::new_simple(&db, eq_inst.def(&db), vec![point_ty, point_ty]);
        let assumption_impl = ImplementorId::assumption(&db, eq_point);
        assert!(
            !TraitSolveCx::implementor_is_default_marked(&db, assumption_impl),
            "an Assumption implementor must NOT be default-marked",
        );
    }

    /// The decision over ≥2 coexisting candidates: exactly-one default-marked →
    /// that one is selected `Unique`; none or two default-marked → a CLEAN
    /// `Ambiguous` over all candidates (never a panic). The default-marked bit is
    /// fed explicitly here because the test DB has no `CoreDerives` ingot to mint a
    /// genuinely canonical second impl; the recognizer itself is checked
    /// independently above.
    #[test]
    fn default_tier_decision_selects_unique_marked_else_ambiguous() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("default_tier_decision.fe"), FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let scope = top_mod.scope();

        let PathRes::Trait(eq_inst) = resolve(&db, scope, &["core", "ops", "Eq"]) else {
            panic!("core::ops::Eq must resolve to a trait");
        };
        let PathRes::Ty(point_ty) = resolve(&db, scope, &["Point"]) else {
            panic!("Point must resolve to a type");
        };

        // Two distinct candidate implementors for the same `Eq<Point>` goal: the
        // real impl, and a synthetic `Assumption` standing in for a second
        // coexisting impl (the future cascade state). Distinct so `Ambiguous` can
        // be observed to carry both.
        let real = sole_eq_point_impl(&db, top_mod, eq_inst, point_ty);
        let eq_point = TraitInstId::new_simple(&db, eq_inst.def(&db), vec![point_ty, point_ty]);
        let other = ImplementorId::assumption(&db, eq_point);
        assert_ne!(real, other, "candidates must be distinct");

        use crate::hir_def::IdentId;
        let casual = SelDiscriminator::Alias(IdentId::new(&db, "Casual".to_string()));
        let formal = SelDiscriminator::Alias(IdentId::new(&db, "Formal".to_string()));

        // Exactly one `Default` → select it (the byte-identical 2-slot case:
        // {Default, Anonymous}).
        match default_tier_decision(vec![
            (real, SelDiscriminator::Default),
            (other, SelDiscriminator::Anonymous),
        ]) {
            Selection::Unique(selected) => assert_eq!(
                selected, real,
                "the single `Default` candidate must be selected",
            ),
            other => panic!("expected Unique, got {other:?}"),
        }

        // No `Default`, sole `Anonymous` → the hand-written `Anonymous` is the
        // unscoped default (the new N-way fallback: an `Anonymous` coexisting with
        // an `Alias`, no derived default).
        match default_tier_decision(vec![(real, SelDiscriminator::Anonymous), (other, casual)]) {
            Selection::Unique(selected) => assert_eq!(
                selected, real,
                "the sole `Anonymous` is the unscoped default when no `Default` exists",
            ),
            other => panic!("expected Unique, got {other:?}"),
        }

        // Aliases are NEVER the unscoped default: two distinct aliases, no
        // `Default`/`Anonymous` → clean ambiguity over BOTH (use `with (Name)`).
        match default_tier_decision(vec![(real, casual), (other, formal)]) {
            Selection::Ambiguous(cands) => {
                assert_eq!(cands.len(), 2, "ambiguity must list every candidate");
                assert!(cands.contains(&real) && cands.contains(&other));
            }
            other => panic!("expected Ambiguous, got {other:?}"),
        }

        // None `Default`, two `Anonymous` → clean ambiguity over BOTH candidates
        // (no unique hand-written default).
        match default_tier_decision(vec![
            (real, SelDiscriminator::Anonymous),
            (other, SelDiscriminator::Anonymous),
        ]) {
            Selection::Ambiguous(cands) => {
                assert_eq!(cands.len(), 2, "ambiguity must list every candidate");
                assert!(cands.contains(&real) && cands.contains(&other));
            }
            other => panic!("expected Ambiguous, got {other:?}"),
        }

        // Two `Default` → also clean ambiguity (no unique default).
        match default_tier_decision(vec![
            (real, SelDiscriminator::Default),
            (other, SelDiscriminator::Default),
        ]) {
            Selection::Ambiguous(cands) => assert_eq!(cands.len(), 2),
            other => panic!("expected Ambiguous, got {other:?}"),
        }
    }
}
