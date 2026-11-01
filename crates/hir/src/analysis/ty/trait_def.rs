//! This module contains all trait related types definitions.

use crate::{
    analysis::ty::{
        method_cmp::{compare_impl_method, method_param_correspondence},
        trait_lower::{
            ImplSelfKey, collect_trait_impls, complete_impl_trait, complete_selected_impl,
            lower_impl_trait_header,
        },
        trait_resolution::{GoalSatisfiability, PredicateListId, Selection},
        ty_check::BodyOwner,
    },
    hir_def::{
        Body, Contract, Func, GenericParamOwner, HirIngot, IdentId, ImplTrait, ModeMarker, Trait,
    },
};
use common::{
    indexmap::{IndexMap, IndexSet},
    ingot::{Ingot, IngotKind},
};
use rustc_hash::FxHashMap;
use salsa::SalsaValue;

use super::{
    canonical::Canonical,
    closure::{CallableTrait, callable_params},
    diagnostics::{ImplDiag, TyDiagCollection},
    fold::{TyFoldable, TyFolder},
    shape::Shape,
    trait_lower::collect_implementor_methods,
    trait_resolution::{
        TraitSolveCx, constraint::collect_candidate_constraints, is_goal_satisfiable,
        normalize_trait_inst_preserving_validity,
    },
    ty_def::{BorrowKind, ShapeMode, TyBase, TyData, TyId},
    ty_lower::{CompleteSubst, ParamBasis, ParamDomainId, ParamKey, PartialSubst, param_schema},
    unify::UnificationTable,
    visitor::{TyVisitable, TyVisitor},
};
use crate::analysis::HirAnalysisDb;

/// Returns [`TraitEnv`] for the given ingot.
#[salsa::tracked(cycle_initial=ingot_trait_env_cycle_initial)]
pub(crate) fn ingot_trait_env<'db>(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> TraitEnv<'db> {
    TraitEnv::collect(db, ingot)
}

/// Returns all implementors for the given trait definition.
///
/// Note: this intentionally does **not** pre-filter implementors by unifying with a
/// specific goal instance. Projection-heavy goals (e.g. involving associated type
/// projections) often only become unifiable after normalization, and unification
/// rejects unresolved associated types. The solver normalizes before unifying
/// candidates, so any filtering here must be an over-approximation.
///
/// The implementors are headers; their associated types are not lowered.
/// Lowering `type Space = Inner<P>::Space` proves the bounds of `Inner`'s
/// impl, `P: Pinned`, which looks up this trait's impls again. Consumers lower
/// the associated types they read, one binding at a time.
#[salsa::tracked]
pub(crate) fn impls_for_trait_def<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_def: Trait<'db>,
) -> Vec<ImplementorId<'db>> {
    let env = ingot_trait_env(db, ingot);
    let mut out = env.impls_for_trait(db, trait_def);

    if is_std_evm_contract_trait_def(db, trait_def) {
        out.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    out
}

/// Different nominal heads cannot unify after freshening or saturation.
/// Keep error types and unknown heads for the full unification check.
fn impl_self_ty_may_match<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_ty: TyId<'db>,
    ty: TyId<'db>,
) -> bool {
    if impl_ty.has_invalid(db) {
        return true;
    }
    let impl_base = impl_ty.base_ty(db);
    let ty_base = ty.base_ty(db);
    match (impl_base.data(db), ty_base.data(db)) {
        (TyData::TyBase(_), TyData::TyBase(_)) => impl_base == ty_base,
        _ => true,
    }
}

/// Returns implementors for `trait_def` whose self type can apply to `ty`.
///
/// Method lookup knows both halves of this key for explicitly selected traits
/// (notably operator traits). Intersecting them before completing candidates is
/// important: receiver-only lookup necessarily includes every blanket and
/// otherwise unindexed impl, whose signatures may themselves require method
/// lookup while being lowered.
#[salsa::tracked]
pub(crate) fn impls_for_trait_and_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    trait_def: Trait<'db>,
    ty: Canonical<TyId<'db>>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);

    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }

    let env = ingot_trait_env(db, ingot);
    let mut implementors = env.impls_for_trait(db, trait_def);

    if is_std_evm_contract_trait_def(db, trait_def) && ty.as_contract(db).is_some() {
        implementors.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    implementors
        .into_iter()
        .filter(|implementor| {
            if !impl_self_ty_may_match(db, implementor.self_ty(db), ty) {
                return false;
            }
            let snapshot = table.snapshot();
            let instantiated = table.instantiate_with_fresh_vars(*implementor);
            let impl_ty = table.instantiate_to_term(instantiated.self_ty(db));
            let ty = table.instantiate_to_term(ty);
            let unifies = table.unify(impl_ty, ty).is_ok();
            table.rollback_to(snapshot);
            unifies
        })
        .map(|implementor| complete_impl_trait(db, implementor))
        .collect()
}

/// Returns all implementors for the given trait inst, searching across a
/// deterministic set of ingots.
///
/// This is used to avoid "pick an ingot" footguns where impl lookup depends on
/// the caller's current module ingot and can miss impls that live in either:
/// - the trait's ingot, or
/// - the implementor type's ingot.
#[salsa::tracked]
pub(crate) fn impls_for_trait_in_ingots<'db>(
    db: &'db dyn HirAnalysisDb,
    primary: Ingot<'db>,
    secondary: Option<Ingot<'db>>,
    trait_: Canonical<TraitInstId<'db>>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let trait_def = trait_.extract_identity(&mut table).def(db);
    let mut dedup: IndexSet<ImplementorId<'db>> = IndexSet::default();
    dedup.extend(impls_for_trait_def(db, primary, trait_def).iter().copied());
    if let Some(secondary) = secondary {
        dedup.extend(
            impls_for_trait_def(db, secondary, trait_def)
                .iter()
                .copied(),
        );
    }
    dedup.into_iter().collect()
}

fn is_std_evm_contract_trait_def<'db>(db: &'db dyn HirAnalysisDb, trait_def: Trait<'db>) -> bool {
    let Some(name) = trait_def.name(db).to_opt() else {
        return false;
    };
    if name.data(db) != "Contract" {
        return false;
    }
    if trait_def.top_mod(db).ingot(db).kind(db) != IngotKind::Std {
        return false;
    }
    true
}

#[salsa::tracked]
fn contract_virtual_impls<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> Vec<ImplementorId<'db>> {
    let Some(contract_trait) = std_evm_contract_trait_def(db, ingot) else {
        return Vec::new();
    };

    let init_args_ident = IdentId::new(db, "InitArgs".to_string());

    let mut out = Vec::new();
    for top_mod in ingot.all_modules(db) {
        for &contract in top_mod.all_contracts(db).iter() {
            let self_ty = TyId::contract(db, contract);
            let trait_inst = TraitInstId::new_simple(db, contract_trait, vec![self_ty]);

            let init_args_ty = contract.init_args_ty(db);
            let mut types = IndexMap::new();
            types.insert(init_args_ident, init_args_ty);

            let implementor = ImplementorId::new(
                db,
                trait_inst,
                Vec::new(),
                types,
                ImplementorOrigin::VirtualContract(contract),
            );
            out.push(implementor);
        }
    }

    out
}

#[salsa::tracked(returns(copy))]
fn std_evm_contract_trait_def<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> Option<Trait<'db>> {
    use crate::analysis::name_resolution::resolve_path;
    use common::ingot::IngotKind;

    let scope = ingot.root_mod(db).scope();
    let assumptions = PredicateListId::empty_list(db);

    let std_root = if ingot.kind(db) == IngotKind::Std {
        IdentId::make_ingot(db)
    } else {
        IdentId::new(db, "std".to_string())
    };

    let path = crate::hir_def::PathId::from_ident(db, std_root)
        .push_ident(db, IdentId::new(db, "evm".to_string()))
        .push_ident(db, IdentId::new(db, "Contract".to_string()));

    match resolve_path(db, path, scope, assumptions, false).ok()? {
        crate::analysis::name_resolution::PathRes::Trait(inst) => Some(inst.def(db)),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum ImplementorOrigin<'db> {
    Hir(ImplTrait<'db>),
    VirtualContract(Contract<'db>),
    Assumption,
    /// A callable value's implementation of its callable traits: a
    /// closure's or a function item's (`ty::closure`).
    Callable,
    /// The compiler's structural implementation of a marker trait
    /// (`ty::clearable`).
    Structural,
}

fn ingot_trait_env_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    ingot: Ingot<'db>,
) -> TraitEnv<'db> {
    // Return an empty trait environment when we detect a cycle
    TraitEnv {
        impls: FxHashMap::default(),
        impls_by_self: FxHashMap::default(),
        unindexed_self_impls: Vec::new(),
        ingot,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct ResolvedImplInstance<'db> {
    selected: ImplementorId<'db>,
    instantiated: ImplementorId<'db>,
    trait_inst: TraitInstId<'db>,
}

impl<'db> ResolvedImplInstance<'db> {
    /// The implementation `selected`, as `instantiated` for `trait_inst`,
    /// answering `trait_inst`.
    pub(crate) fn new(
        selected: ImplementorId<'db>,
        instantiated: ImplementorId<'db>,
        trait_inst: TraitInstId<'db>,
    ) -> Self {
        Self {
            selected,
            instantiated,
            trait_inst,
        }
    }

    pub fn selected(self) -> ImplementorId<'db> {
        self.selected
    }

    pub(crate) fn instantiated(self) -> ImplementorId<'db> {
        self.instantiated
    }

    pub fn trait_inst(self) -> TraitInstId<'db> {
        self.trait_inst
    }

    pub fn impl_args(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.instantiated.params(db)
    }

    pub fn impl_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.selected.params(db)
    }

    /// The trait method `name` of this already-selected impl, called at
    /// `mode`, with the body it runs. Absent when the trait has no such
    /// method.
    ///
    /// The body's generic arguments match the selected implementation for an
    /// explicit method and the resolved trait instance for a default method.
    pub fn method(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
        mode: Option<BorrowKind>,
    ) -> Option<ResolvedMethodInstance<'db>> {
        let implementor = self.selected();
        let declaration = implementor
            .trait_def(db)
            .method_defs(db)
            .get(&name)
            .copied()?;
        let (body, body_args) = if let Some(func) = implementor.method_at(db, name, mode) {
            (Some(func), self.impl_args(db).to_vec())
        } else {
            (
                declaration.body(db).is_some().then_some(declaration),
                self.trait_inst().args(db).to_vec(),
            )
        };
        let (body_to_nominal, effect_pairs) = body.map_or_else(
            || (Vec::new(), Some(Vec::new())),
            |body| {
                if body == declaration {
                    (
                        (0..param_schema(db, body.into(), ParamBasis::Full)
                            .keys(db)
                            .len())
                            .map(Some)
                            .collect(),
                        None,
                    )
                } else {
                    let correspondence = method_param_correspondence(
                        db,
                        body.as_callable(db).expect("selected method is callable"),
                        BodyOwner::func_at(db, body, mode).mode(),
                        declaration
                            .as_callable(db)
                            .expect("trait method is callable"),
                        BodyOwner::func_at(db, declaration, mode).mode(),
                        implementor.trait_inst(db),
                    );
                    (
                        correspondence.body_to_trait_slots.clone(),
                        Some(correspondence.trait_effect_to_body.clone()),
                    )
                }
            },
        );
        Some(ResolvedMethodInstance {
            resolved: self,
            declaration,
            body,
            body_args,
            body_to_nominal,
            effect_pairs,
        })
    }

    pub fn assoc_ty_template(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.selected.assoc_ty(db, name)
    }

    /// The selected implementation's conditions, instantiated for the goal.
    pub(crate) fn instantiated_constraints(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> PredicateListId<'db> {
        self.instantiated.constraints(db)
    }

    pub fn instantiated_assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.instantiated.assoc_ty(db, name)
    }
}

impl<'db> TyFoldable<'db> for ResolvedImplInstance<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            selected: self.selected,
            instantiated: self.instantiated.fold_with(db, folder),
            trait_inst: self.trait_inst.fold_with(db, folder),
        }
    }
}

impl<'db> TyVisitable<'db> for ResolvedImplInstance<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.instantiated.visit_with(visitor);
        self.trait_inst.visit_with(visitor);
    }
}

pub(crate) fn instantiate_selected_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    selected: ImplementorId<'db>,
    inst: TraitInstId<'db>,
) -> Option<ResolvedImplInstance<'db>> {
    if matches!(selected.origin(db), ImplementorOrigin::Assumption) {
        return Some(ResolvedImplInstance {
            selected,
            instantiated: selected,
            trait_inst: inst,
        });
    }

    // The selection proved the goal's associated-type bindings, which only
    // normalization relates to the implementation's; its arguments follow
    // from the trait's.
    let mut table = UnificationTable::new(db);
    let instantiated = table.instantiate_with_fresh_vars(selected);
    for (&impl_arg, &arg) in instantiated
        .trait_inst(db)
        .args(db)
        .iter()
        .zip(inst.args(db))
    {
        table.unify(impl_arg, arg).ok()?;
    }
    Some(ResolvedImplInstance {
        selected,
        instantiated: instantiated.fold_with(db, &mut table),
        trait_inst: inst.fold_with(db, &mut table),
    })
}

pub(crate) fn resolve_trait_impl_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> Selection<'db, ResolvedImplInstance<'db>> {
    match resolve_trait_impl_instance_query(db, solve_cx, inst) {
        ImplSelection::Unique(resolved) => Selection::Unique(resolved),
        ImplSelection::Ambiguous => Selection::Ambiguous(IndexSet::new()),
        ImplSelection::NotFound => Selection::NotFound,
        ImplSelection::NeedsGeneration(request) => Selection::NeedsGeneration(request),
    }
}

/// An implementation selection, without candidates for an ambiguous one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
enum ImplSelection<'db> {
    NeedsGeneration(super::trait_resolution::generation::GenerationRequest<'db>),
    Unique(ResolvedImplInstance<'db>),
    Ambiguous,
    NotFound,
}

/// Call sites and method checks select the same implementations repeatedly.
#[salsa::tracked(returns(copy))]
fn resolve_trait_impl_instance_query<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
) -> ImplSelection<'db> {
    let assumptions = solve_cx.assumptions();
    let norm_scope = solve_cx.normalization_scope_for_trait_inst(db, inst);
    let inst = normalize_trait_inst_preserving_validity(db, inst, norm_scope, assumptions);
    match solve_cx.select_impl(db, inst) {
        Selection::Unique(selected) => {
            let Some(selected) = complete_selected_impl(db, selected) else {
                return ImplSelection::NotFound;
            };
            // A selected name can supply arguments absent from the trait head.
            // Keep its application (and captured outer parameters) instead of
            // attempting to reconstruct all implementation arguments from `inst`.
            if let Some(super::trait_resolution::ChoiceMatch::Implementation(choice)) =
                super::trait_resolution::matching_choice(db, solve_cx.choices(), inst)
                && choice.template.origin(db) == selected.origin(db)
            {
                let mut table = UnificationTable::new(db);
                let instantiated = super::trait_resolution::ChosenImpl {
                    template: selected,
                    ..choice
                }
                .instantiate(&mut table);
                if instantiated
                    .trait_inst(db)
                    .args(db)
                    .iter()
                    .zip(inst.args(db))
                    .all(|(&candidate, &goal)| table.unify(candidate, goal).is_ok())
                {
                    return ImplSelection::Unique(ResolvedImplInstance {
                        selected,
                        instantiated: instantiated.fold_with(db, &mut table),
                        trait_inst: inst.fold_with(db, &mut table),
                    });
                }
            }
            instantiate_selected_impl(db, selected, inst)
                .map_or(ImplSelection::NotFound, ImplSelection::Unique)
        }
        // There is deliberately no resolved instance for a non-unique
        // selection. Re-unifying each candidate would both fabricate evidence
        // the caller must not consume and attempt to fold inference variables
        // owned by the caller through a fresh local table.
        Selection::Ambiguous(_) => ImplSelection::Ambiguous,
        Selection::NotFound => ImplSelection::NotFound,
        Selection::NeedsGeneration(request) => ImplSelection::NeedsGeneration(request),
    }
}

/// A selected method retains the proof origin and the nominal trait reference.
/// A trait default body selected through an assumption remains generic-bound
/// dispatch until a concrete implementation is chosen.
#[derive(Debug, Clone)]
pub struct ResolvedMethodInstance<'db> {
    resolved: ResolvedImplInstance<'db>,
    declaration: Func<'db>,
    body: Option<Func<'db>>,
    body_args: Vec<TyId<'db>>,
    body_to_nominal: Vec<Option<usize>>,
    /// The declaration's effects paired with the body's, absent when the body
    /// is the declaration.
    effect_pairs: Option<Vec<(usize, usize)>>,
}

#[derive(Debug, Clone)]
pub enum MethodArgMapError<'db> {
    MissingBody,
    NominalArity {
        expected: usize,
        given: usize,
    },
    SelectedPrefixArity {
        expected: usize,
        given: usize,
    },
    MissingNominalRole(ParamKey<'db>),
    MissingEffectRole(usize),
    MissingProviderRole(u32),
    InconsistentProviderRole {
        nominal: u32,
        first: u32,
        second: u32,
    },
    SignatureArity {
        func: Func<'db>,
        expected_generics: usize,
        given_generics: usize,
        declared_inputs: usize,
        parameter_inputs: usize,
    },
    /// A spread instantiated with a shape that lists no parameters.
    SpreadNotListed {
        func: Func<'db>,
        shape: TyId<'db>,
    },
    SignatureMismatch {
        nominal: Func<'db>,
        body: Func<'db>,
        nominal_args: Vec<TyId<'db>>,
        body_args: Vec<TyId<'db>>,
        evidence: PredicateListId<'db>,
        nominal_signature: String,
        body_signature: String,
    },
}

impl<'db> ResolvedMethodInstance<'db> {
    pub fn resolved(&self) -> ResolvedImplInstance<'db> {
        self.resolved
    }

    pub fn body(&self) -> Option<Func<'db>> {
        self.body
    }

    pub fn body_args(&self) -> &[TyId<'db>] {
        &self.body_args
    }

    pub fn effect_pairs(&self) -> Option<&[(usize, usize)]> {
        self.effect_pairs.as_deref()
    }

    pub fn origin(&self, db: &'db dyn HirAnalysisDb) -> ImplementorOrigin<'db> {
        self.resolved.selected().origin(db)
    }

    /// Rebase an already-completed nominal method instance into the selected
    /// body schema. Inherited values come from implementation selection; the
    /// method's own and hidden slots are matched by structural role.
    pub fn complete_body_args(
        &self,
        db: &'db dyn HirAnalysisDb,
        nominal_args: &[TyId<'db>],
    ) -> Result<CompleteSubst<'db>, MethodArgMapError<'db>> {
        let body = self.body.ok_or(MethodArgMapError::MissingBody)?;
        let nominal_schema = param_schema(db, self.declaration.into(), ParamBasis::Full);
        if nominal_args.len() != nominal_schema.keys(db).len() {
            return Err(MethodArgMapError::NominalArity {
                expected: nominal_schema.keys(db).len(),
                given: nominal_args.len(),
            });
        }
        let body_schema = param_schema(db, body.into(), ParamBasis::Full);
        let inherited_len = GenericParamOwner::Func(body)
            .parent(db)
            .map_or(0, |parent| {
                param_schema(db, parent, ParamBasis::Full).keys(db).len()
            });
        if self.body_args.len() != inherited_len {
            return Err(MethodArgMapError::SelectedPrefixArity {
                expected: inherited_len,
                given: self.body_args.len(),
            });
        }

        let domain = ParamDomainId::full(db, body_schema);
        let mut args = PartialSubst::new(db, domain);
        for (slot, &arg) in self.body_args.iter().enumerate() {
            args.bind(db, body_schema.keys(db)[slot], arg)
                .expect("selected inherited parameter key");
        }
        for (body_slot, &key) in body_schema.keys(db).iter().enumerate().skip(inherited_len) {
            if let Some(nominal_slot) = self.body_to_nominal.get(body_slot).copied().flatten() {
                args.bind(db, key, nominal_args[nominal_slot])
                    .expect("selected nominal parameter key");
            } else if !matches!(key, ParamKey::EffectProvider { func, effect_idx }
                    if func.effect_from_row(db, effect_idx))
            {
                return Err(MethodArgMapError::MissingNominalRole(key));
            }
        }
        // The call provides the effects of an implementation's rows, so their
        // provider slots keep their body formals.
        Ok(args.residualize(db))
    }
}

/// Returns all implementors for the given `ty` whose constraints are fully proven.
pub(crate) fn impls_for_ty_with_satisfied_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);
    impls_with_constraint_mode(db, ingot, &mut table, ty, None, assumptions, false)
}

/// Returns implementors of `inst`'s trait that can apply to `inst` and whose
/// constraints, under `inst`'s arguments, are not known to be unsatisfied.
pub(crate) fn impls_for_trait_inst_with_possible_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    inst: Canonical<TraitInstId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let inst = inst.extract_identity(&mut table);
    let ty = inst.self_ty(db);
    impls_with_constraint_mode(db, ingot, &mut table, ty, Some(inst), assumptions, true)
}

fn impls_with_constraint_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    table: &mut UnificationTable<'db>,
    ty: TyId<'db>,
    trait_inst: Option<TraitInstId<'db>>,
    assumptions: PredicateListId<'db>,
    allow_needs_confirmation: bool,
) -> Vec<ImplementorId<'db>> {
    let solve_cx = TraitSolveCx::new(db, ingot.root_mod(db).scope()).with_assumptions(assumptions);
    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }
    let trait_def = trait_inst.map(|inst| inst.def(db));
    let env = ingot_trait_env(db, ingot);
    let mut raw_impls = match trait_def {
        Some(trait_def) => env.impls_for_trait(db, trait_def),
        None => env.impls_for_self_key(db, ty.base_ty(db)),
    };

    if ty.as_contract(db).is_some()
        && trait_def.is_none_or(|trait_def| is_std_evm_contract_trait_def(db, trait_def))
    {
        raw_impls.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    raw_impls
        .into_iter()
        .filter(|impl_| {
            if !impl_self_ty_may_match(db, impl_.self_ty(db), ty) {
                return false;
            }
            let snapshot = table.snapshot();

            let inst = table.instantiate_with_fresh_vars(*impl_);
            let impl_ty = table.instantiate_to_term(inst.self_ty(db));
            let ty_term = table.instantiate_to_term(ty);
            // The constraints are checked under the target's arguments too:
            // `impl<T, R: Read<T>> Read<T> for Buf<R>` needs its `T`.
            let unifies =
                table.unify(impl_ty, ty_term).is_ok()
                    && trait_inst.is_none_or(|target| {
                        inst.trait_(db).args(db).iter().zip(target.args(db)).all(
                            |(&impl_arg, &arg)| {
                                let arg = table.instantiate_to_term(arg);
                                table.unify(impl_arg, arg).is_ok()
                            },
                        )
                    });

            if unifies {
                let impl_constraints = inst.constraints(db);
                if impl_constraints.is_empty(db) {
                    table.rollback_to(snapshot);
                    return true;
                }

                for &constraint in impl_constraints.list(db) {
                    let constraint = constraint.fold_with(db, table);
                    let satisfiability = is_goal_satisfiable(db, solve_cx, constraint);
                    let constraint_holds =
                        matches!(satisfiability, GoalSatisfiability::Satisfied(_))
                            || (allow_needs_confirmation
                                && matches!(
                                    satisfiability,
                                    GoalSatisfiability::NeedsConfirmation { .. }
                                ));
                    if !constraint_holds {
                        table.rollback_to(snapshot);
                        return false;
                    }
                }
            }

            table.rollback_to(snapshot);
            unifies
        })
        .collect()
}

/// Returns all implementors for the given `ty`.
#[salsa::tracked]
pub(crate) fn impls_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.extract_identity(&mut table);

    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return vec![];
    }
    let env = ingot_trait_env(db, ingot);
    let mut raw_impls = env.impls_for_self_key(db, ty.base_ty(db));

    if ty.as_contract(db).is_some() {
        raw_impls.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    raw_impls
        .into_iter()
        .filter(|impl_| {
            let snapshot = table.snapshot();

            let inst = table.instantiate_with_fresh_vars(*impl_);
            let impl_ty = table.instantiate_to_term(inst.self_ty(db));
            let ty_term = table.instantiate_to_term(ty);
            let is_ok = table.unify(impl_ty, ty_term).is_ok();

            table.rollback_to(snapshot);

            is_ok
        })
        .map(|implementor| complete_impl_trait(db, implementor))
        .collect()
}

/// Returns the selected body, its declared template type, and the checked
/// substitution over the body's owner. Explicit impl bodies use impl
/// arguments; inherited defaults use trait arguments.
pub fn assoc_const_body_template_for_trait_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    const_name: IdentId<'db>,
) -> Option<(Body<'db>, TyId<'db>, CompleteSubst<'db>)> {
    let resolved = match resolve_trait_impl_instance(db, solve_cx, inst) {
        Selection::Unique(resolved) => resolved,
        Selection::Ambiguous(_ambiguous) => return None,
        Selection::NotFound | Selection::NeedsGeneration(_) => return None,
    };
    selected_assoc_const_body_template(db, resolved, const_name)
}

pub(crate) fn selected_assoc_const_body_template<'db>(
    db: &'db dyn HirAnalysisDb,
    resolved: ResolvedImplInstance<'db>,
    const_name: IdentId<'db>,
) -> Option<(Body<'db>, TyId<'db>, CompleteSubst<'db>)> {
    let explicit = match resolved.selected().origin(db) {
        ImplementorOrigin::Hir(impl_trait) => impl_trait
            .assoc_consts(db)
            .into_iter()
            .find(|constant| constant.name(db) == Some(const_name))
            .and_then(|constant| {
                Some((
                    constant.value_body(db)?,
                    constant.ty(db)?,
                    CompleteSubst::for_owner(
                        db,
                        impl_trait.into(),
                        resolved.impl_args(db).to_vec(),
                    )
                    .ok()?,
                ))
            }),
        ImplementorOrigin::VirtualContract(_)
        | ImplementorOrigin::Callable
        | ImplementorOrigin::Structural => None,
        ImplementorOrigin::Assumption => return None,
    };
    explicit.or_else(|| {
        let inst = resolved.trait_inst();
        let trait_ = inst.def(db);
        let constant = trait_.const_(db, const_name)?;
        Some((
            constant.default_body(db)?,
            constant.ty(db)?,
            CompleteSubst::for_owner(db, trait_.into(), inst.args(db).to_vec()).ok()?,
        ))
    })
}

/// What a named implementation's declaration gives differently from another
/// implementation of its slot.
pub(crate) enum NamedOutputMismatch<'db> {
    NoOrdinaryOutputs,
    /// An associated type; `with` is the implementation compared with.
    AssocType {
        with: ImplTrait<'db>,
        ordinary: bool,
        name: IdentId<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
    },
    AssocConst {
        with: ImplTrait<'db>,
        name: IdentId<'db>,
    },
    /// An associated effect row.
    Row {
        with: ImplTrait<'db>,
        ordinary: bool,
        name: IdentId<'db>,
    },
}

/// A slot's associated types and effect rows do not depend on which implementation answers it. A named
/// declaration is compared with every other implementation of the trait,
/// ordinary or named, whose header overlaps its own, at their overlap and
/// whatever their conditions: a condition that does not hold here can hold
/// where a `with` adds a premise, and two named implementations that both
/// answer a slot would otherwise give it outputs that depend on which is
/// found first. This is definition admission, independent of any opening.
pub(crate) fn named_output_mismatch<'db>(
    db: &'db dyn HirAnalysisDb,
    implementation: ImplTrait<'db>,
    candidate: ImplementorId<'db>,
) -> Option<NamedOutputMismatch<'db>> {
    let context = TraitSolveCx::new(db, implementation.scope())
        .with_assumptions(crate::semantic::param_env(db, implementation.into()));
    let header = candidate.trait_inst(db);
    let goal = TraitInstId::new_simple(db, header.def(db), header.args(db).clone());
    if goal.def(db).has_assoc_outputs(db) || goal.def(db).assoc_consts(db).next().is_some() {
        // Types still normalize ordinarily. Equality at overlaps is insufficient
        // when some named applications have no ordinary output representative.
        let ordinary = context
            .with_assumptions(candidate.constraints(db))
            .select_impl(db, goal);
        if !matches!(ordinary, Selection::Unique(implementation) if matches!(implementation.origin(db), ImplementorOrigin::Hir(source) if source.selection_name(db).is_none()))
        {
            return Some(NamedOutputMismatch::NoOrdinaryOutputs);
        }
    }
    let (primary, secondary) = context.search_ingots_for_trait_inst(db, goal);
    let ingots = [Some(primary), secondary]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    let ordinary = ingots
        .iter()
        .flat_map(|ingot| impls_for_trait_def(db, *ingot, goal.def(db)).to_vec())
        .filter(|other| matches!(other.origin(db), ImplementorOrigin::Hir(_)))
        .map(|other| (other, true));
    // Every admitted named header has an ordinary output representative.
    // Agreement with all overlapping ordinary headers therefore also implies
    // named-to-named agreement. Comparing named pairs again would blame a
    // conforming declaration for another declaration's ordinary-output error.
    for (other, ordinary) in ordinary {
        let ImplementorOrigin::Hir(with) = other.origin(db) else {
            continue;
        };
        let Some(other) = complete_selected_impl(db, other) else {
            continue;
        };
        let Some((own, other)) = at_overlap(db, candidate, other) else {
            continue;
        };
        for assoc in goal.def(db).assoc_types(db) {
            let Some(name) = assoc.name(db) else {
                continue;
            };
            let (Some(expected), Some(given)) = (
                other.instantiated_assoc_ty(db, name),
                own.instantiated_assoc_ty(db, name),
            ) else {
                continue;
            };
            let expected = consts_evaluated(
                db,
                super::normalize::normalize_ty(
                    db,
                    expected,
                    implementation.scope(),
                    context.assumptions(),
                ),
            );
            let given = consts_evaluated(
                db,
                super::normalize::normalize_ty(
                    db,
                    given,
                    implementation.scope(),
                    context.assumptions(),
                ),
            );
            if expected.has_invalid(db) || given.has_invalid(db) {
                continue;
            }
            // Equal at the overlap, for every value of what the overlap
            // leaves open: the two are compared as they are, not unified.
            if expected != given {
                return Some(NamedOutputMismatch::AssocType {
                    with,
                    ordinary,
                    name,
                    expected,
                    given,
                });
            }
        }
        for constant in goal.def(db).assoc_consts(db) {
            let Some(name) = constant.name(db) else {
                continue;
            };
            let value = |resolved| {
                super::const_ty::const_ty_from_resolved_trait_const(db, resolved, name)
                    .map(|value| consts_evaluated(db, TyId::const_ty(db, value)))
            };
            if value(own) != value(other) {
                return Some(NamedOutputMismatch::AssocConst { with, name });
            }
        }
        for row in goal.def(db).rows(db) {
            let Some(name) = row.name.to_opt() else {
                continue;
            };
            let components = |resolved| {
                super::effects::rows::implementation_row_components(db, resolved, name).map(
                    |components| {
                        components
                            .into_iter()
                            .map(|component| (component.name, component.key, component.is_mut))
                            .collect::<Vec<_>>()
                    },
                )
            };
            if components(other) != components(own) {
                return Some(NamedOutputMismatch::Row {
                    with,
                    ordinary,
                    name,
                });
            }
        }
    }
    None
}

/// `ty` with each constant in it evaluated, so two written forms of one type
/// (`Arr<{ 100 }>` and `Arr<100>`, or one braced constant written twice)
/// compare equal.
fn consts_evaluated<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
    struct Evaluate;
    impl<'db> TyFolder<'db> for Evaluate {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            let ty = ty.super_fold_with(db, self);
            super::const_ty::normalize_const_tys_for_comparison(db, ty)
        }
    }
    ty.fold_with(db, &mut Evaluate)
}

/// Two implementations instantiated at their overlap: each header with fresh
/// variables, unified with the other. `None` when the headers cannot both
/// match one goal.
fn at_overlap<'db>(
    db: &'db dyn HirAnalysisDb,
    own: ImplementorId<'db>,
    other: ImplementorId<'db>,
) -> Option<(ResolvedImplInstance<'db>, ResolvedImplInstance<'db>)> {
    let mut table = UnificationTable::new(db);
    let own_fresh = table.instantiate_with_fresh_vars(own);
    let other_fresh = table.instantiate_with_fresh_vars(other);
    for (&a, &b) in own_fresh
        .trait_inst(db)
        .args(db)
        .iter()
        .zip(other_fresh.trait_inst(db).args(db))
    {
        table.unify(a, b).ok()?;
    }
    let resolved = |selected, fresh: ImplementorId<'db>, table: &mut UnificationTable<'db>| {
        let instantiated = fresh.fold_with(db, table);
        ResolvedImplInstance {
            selected,
            instantiated,
            trait_inst: instantiated.trait_inst(db),
        }
    };
    let own = resolved(own, own_fresh, &mut table);
    let other = resolved(other, other_fresh, &mut table);
    Some((own, other))
}

/// Represents the trait environment of an ingot, which maintain all trait
/// implementors which can be used in the ingot.
#[derive(Debug, PartialEq, Eq, Clone, SalsaValue)]
pub(crate) struct TraitEnv<'db> {
    /// Syntax-only impl candidates grouped by trait definition.
    pub(crate) impls: FxHashMap<Trait<'db>, Vec<ImplTrait<'db>>>,

    /// Syntax-only impl candidates grouped by their nominal self-type root.
    impls_by_self: FxHashMap<ImplSelfKey<'db>, Vec<ImplTrait<'db>>>,

    /// Blanket, alias-rooted, projection-rooted, or otherwise unindexable impls
    /// that must be considered for every receiver type.
    unindexed_self_impls: Vec<ImplTrait<'db>>,

    ingot: Ingot<'db>,
}

impl<'db> TraitEnv<'db> {
    fn collect(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Self {
        let mut impls: FxHashMap<Trait<'db>, Vec<ImplTrait<'db>>> = FxHashMap::default();
        let mut impls_by_self: FxHashMap<ImplSelfKey<'db>, Vec<ImplTrait<'db>>> =
            FxHashMap::default();
        let mut unindexed_self_impls = Vec::new();

        for impl_map in ingot
            .resolved_external_ingots(db)
            .iter()
            .map(|(_, external)| collect_trait_impls(db, *external))
            .chain(std::iter::once(collect_trait_impls(db, ingot)))
        {
            // Raw collection is syntax-only. Candidate lookup performs semantic
            // lowering after this environment has been fully assembled.
            for (trait_def, implementors) in &impl_map.by_trait {
                impls
                    .entry(*trait_def)
                    .or_default()
                    .extend(implementors.iter().copied());
            }
            for (self_key, implementors) in &impl_map.by_self {
                impls_by_self
                    .entry(*self_key)
                    .or_default()
                    .extend(implementors.iter().copied());
            }
            unindexed_self_impls.extend(impl_map.unindexed_self.iter().copied());
        }

        Self {
            impls,
            impls_by_self,
            unindexed_self_impls,
            ingot,
        }
    }

    fn impls_for_self_key(
        &self,
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Vec<ImplementorId<'db>> {
        let key = match ty.data(db) {
            TyData::TyBase(TyBase::Prim(prim)) => Some(ImplSelfKey::Prim(*prim)),
            TyData::TyBase(TyBase::Adt(adt)) => Some(ImplSelfKey::Item(adt.scope(db))),
            TyData::TyBase(TyBase::Contract(contract)) => Some(ImplSelfKey::Item(contract.scope())),
            _ => None,
        };
        key.and_then(|key| self.impls_by_self.get(&key))
            .into_iter()
            .flatten()
            .chain(&self.unindexed_self_impls)
            .filter(|impl_trait| impl_trait.selection_name(db).is_none())
            .filter_map(|impl_trait| lower_impl_trait_header(db, *impl_trait))
            .collect()
    }

    fn impls_for_trait(
        &self,
        db: &'db dyn HirAnalysisDb,
        trait_def: Trait<'db>,
    ) -> Vec<ImplementorId<'db>> {
        self.impls
            .get(&trait_def)
            .into_iter()
            .flatten()
            .filter(|impl_trait| impl_trait.selection_name(db).is_none())
            .filter_map(|impl_trait| lower_impl_trait_header(db, *impl_trait))
            .collect()
    }
}

/// Represents a slim, internal view of a trait impl, derived from an
/// `ImplTrait` item and its lowered trait instance.
#[salsa::interned]
#[derive(Debug, PartialOrd, Ord)]
pub struct ImplementorId<'db> {
    /// The trait instance that this impl realizes.
    #[returns(copy)]
    pub(crate) trait_: TraitInstId<'db>,

    /// The type parameters of this implementor.
    pub(crate) params: Vec<TyId<'db>>,

    pub(crate) types: IndexMap<IdentId<'db>, TyId<'db>>,

    #[returns(copy)]
    pub origin: ImplementorOrigin<'db>,
}

impl<'db> ImplementorId<'db> {
    pub(crate) fn assumption(db: &'db dyn HirAnalysisDb, inst: TraitInstId<'db>) -> Self {
        ImplementorId::new(
            db,
            inst,
            Vec::new(),
            IndexMap::new(),
            ImplementorOrigin::Assumption,
        )
    }

    pub(crate) fn hir_impl_trait(self, db: &'db dyn HirAnalysisDb) -> ImplTrait<'db> {
        match self.origin(db) {
            ImplementorOrigin::Hir(impl_trait) => impl_trait,
            ImplementorOrigin::VirtualContract(contract) => panic!(
                "requested HIR impl-trait for virtual implementor (contract={})",
                contract
                    .name(db)
                    .to_opt()
                    .map_or_else(|| "<unknown>".to_string(), |n| n.data(db).to_string())
            ),
            ImplementorOrigin::Assumption => {
                panic!("requested HIR impl-trait for assumption-based implementor")
            }
            ImplementorOrigin::Callable => {
                panic!("requested HIR impl-trait for a callable value's builtin implementor")
            }
            ImplementorOrigin::Structural => {
                panic!("requested HIR impl-trait for a structural builtin implementor")
            }
        }
    }

    /// Associated type defined in this impl, if any.
    pub(crate) fn assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.types(db).get(&name).copied()
    }

    /// Trait definition implemented by this impl.
    pub(crate) fn trait_def(self, db: &'db dyn HirAnalysisDb) -> Trait<'db> {
        self.trait_(db).def(db)
    }

    /// Semantic self type of this impl.
    pub(crate) fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.trait_(db).self_ty(db)
    }

    /// Trait instance realized by this impl, including its associated type definitions.
    pub fn trait_inst(self, db: &'db dyn HirAnalysisDb) -> TraitInstId<'db> {
        let trait_inst = self.trait_(db);
        if self.types(db).is_empty() {
            return trait_inst;
        }

        let mut assoc_type_bindings = trait_inst.assoc_type_bindings(db).clone();
        for (name, ty) in self.types(db) {
            assoc_type_bindings.insert(AssocBinding::of(*name), *ty);
        }

        TraitInstId::new(
            db,
            trait_inst.def(db),
            trait_inst.args(db).to_vec(),
            assoc_type_bindings,
            trait_inst.mode(db),
        )
    }

    /// Returns the constraints that the implementor requires when the
    /// implementation is selected.
    pub(crate) fn constraints(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        match self.origin(db) {
            ImplementorOrigin::Hir(impl_trait) => {
                collect_candidate_constraints(db, impl_trait.into())
                    .instantiate(db, self.params(db))
            }
            ImplementorOrigin::VirtualContract(_)
            | ImplementorOrigin::Assumption
            | ImplementorOrigin::Callable
            | ImplementorOrigin::Structural => PredicateListId::empty_list(db),
        }
    }

    /// Method map for this impl, keyed by name: for a method with one body
    /// per mode, its `ref` body.
    pub(crate) fn methods(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db IndexMap<IdentId<'db>, Func<'db>> {
        &collect_implementor_methods(db, self).methods
    }

    /// The impl's body of the method `name` at `mode`: the `mut` body of a
    /// method with one body per mode at `mut`, and its method otherwise.
    pub(crate) fn method_at(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
        mode: Option<BorrowKind>,
    ) -> Option<Func<'db>> {
        let methods = collect_implementor_methods(db, self);
        mode.filter(|&mode| mode == BorrowKind::Mut)
            .and_then(|_| methods.mut_bodies.get(&name))
            .or_else(|| methods.methods.get(&name))
            .copied()
    }

    /// Whether the implementation gives the trait's `mut?` method `trait_m`,
    /// named `name`, its `mut` mode: by a `mut?` method, a `mut` body, or
    /// the trait method's default body.
    fn gives_mut_mode(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
        trait_m: Func<'db>,
    ) -> bool {
        let methods = collect_implementor_methods(db, self);
        methods.mut_bodies.contains_key(&name)
            || methods
                .methods
                .get(&name)
                .map_or(trait_m.body(db).is_some(), |method| method.is_maybe_mut(db))
    }

    /// Whether the implementation gives the trait's `mut?` methods their
    /// `mut` modes, so `T: mut Trait` holds by it under its `mut?` bounds:
    /// each such method is `mut?` or keeps its `mut?` default body. A
    /// compiler-made implementor's trait has no `mut?` methods.
    pub(crate) fn gives_mut(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.trait_def(db)
            .method_defs(db)
            .iter()
            .filter(|(_, method)| method.is_maybe_mut(db))
            .all(|(&name, &method)| self.gives_mut_mode(db, name, method))
    }

    /// Compare impl methods vs. trait methods and report missing/mismatched ones.
    pub(crate) fn diags_method_conformance(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        if !matches!(self.origin(db), ImplementorOrigin::Hir(_)) {
            return Vec::new();
        }
        let mut diags = vec![];
        let impl_methods = self.methods(db);
        let hir_trait = self.trait_def(db);
        let trait_methods = self.trait_def(db).method_defs(db);
        let mut required_methods: IndexSet<_> = trait_methods
            .iter()
            .filter_map(|(name, &trait_method)| trait_method.body(db).is_none().then_some(*name))
            .collect();

        let methods = collect_implementor_methods(db, self);
        for (first, other) in &methods.conflicts {
            diags.push(
                ImplDiag::ConflictMethodImpl {
                    primary: other.as_callable(db).unwrap(),
                    conflict_with: first.as_callable(db).unwrap(),
                }
                .into(),
            );
        }
        for (name, impl_m) in impl_methods {
            let Some(trait_m) = trait_methods.get(name) else {
                diags.push(
                    ImplDiag::MethodNotDefinedInTrait {
                        primary: self.hir_impl_trait(db).span().trait_ref().into(),
                        method_name: *name,
                        trait_: hir_trait,
                    }
                    .into(),
                );
                continue;
            };
            let trait_m = trait_m.as_callable(db).unwrap();
            compare_impl_method(
                db,
                impl_m.as_callable(db).unwrap(),
                trait_m,
                self.trait_inst(db),
                BorrowKind::Ref,
                &mut diags,
            );
            if let Some(mut_body) = methods.mut_bodies.get(name) {
                compare_impl_method(
                    db,
                    mut_body.as_callable(db).unwrap(),
                    trait_m,
                    self.trait_inst(db),
                    BorrowKind::Mut,
                    &mut diags,
                );
            }
            required_methods.remove(name);
        }

        // Either every `mut?` method of the trait has its `mut` mode here, or
        // none has.
        let modes: Vec<_> = trait_methods
            .iter()
            .filter(|(_, method)| method.is_maybe_mut(db))
            .map(|(&name, &method)| (name, self.gives_mut_mode(db, name, method)))
            .collect();
        if let Some(&(given, _)) = modes.iter().find(|(_, gives)| *gives)
            && let Some(&(missing, _)) = modes.iter().find(|(_, gives)| !gives)
        {
            diags.push(
                ImplDiag::PartialMutImpl {
                    primary: self.hir_impl_trait(db).span().trait_ref().into(),
                    given,
                    missing,
                }
                .into(),
            );
        }

        if !required_methods.is_empty() {
            diags.push(
                ImplDiag::NotAllTraitItemsImplemented {
                    primary: self.hir_impl_trait(db).span().ty().into(),
                    not_implemented: required_methods.into_iter().collect(),
                }
                .into(),
            );
        }

        diags
    }
}

/// Returns `true` if the given two implementors conflict.
///
/// This mirrors the legacy `Implementor`-based semantics:
/// - instantiate both implementors with fresh vars and unify them;
/// - then check that the merged constraints are satisfiable.
pub(crate) fn does_impl_trait_conflict<'db>(
    db: &'db dyn HirAnalysisDb,
    a: ImplementorId<'db>,
    b: ImplementorId<'db>,
) -> bool {
    let mut table = UnificationTable::new(db);
    let a = table.instantiate_with_fresh_vars(a);
    let b = table.instantiate_with_fresh_vars(b);

    if table.unify(a, b).is_err() {
        return false;
    }

    let a_constraints = a.constraints(db);
    let b_constraints = b.constraints(db);

    if a_constraints.is_empty(db) && b_constraints.is_empty(db) {
        return true;
    }

    // Check if all constraints from both implementations would be satisfiable
    // when the types are unified.
    let merged_constraints = a_constraints.merge(db, b_constraints);
    let solve_cx = TraitSolveCx::new(db, a.trait_def(db).scope())
        .with_assumptions(PredicateListId::empty_list(db));

    for &constraint in merged_constraints.list(db) {
        let constraint = constraint.fold_with(db, &mut table);

        match is_goal_satisfiable(db, solve_cx, constraint) {
            GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid => {
                return false;
            }
            _ => {
                // Constraint is satisfiable or needs more information, continue checking.
            }
        }
    }

    true
}

/// The positional identity of a trait application. Associated equalities live
/// in predicates and resolution evidence, not in a projection's identity.
#[salsa::interned]
#[derive(Debug)]
pub struct TraitRefId<'db> {
    #[returns(copy)]
    pub key: Trait<'db>,
    pub args: Vec<TyId<'db>>,
}

impl<'db> TraitRefId<'db> {
    pub fn def(self, db: &'db dyn HirAnalysisDb) -> Trait<'db> {
        self.key(db)
    }

    pub fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.args(db)[0]
    }

    pub fn as_predicate(self, db: &'db dyn HirAnalysisDb) -> TraitInstId<'db> {
        TraitInstId::new_simple(db, self.def(db), self.args(db).to_vec())
    }

    pub fn project_assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.def(db)
            .assoc_ty(db, name)
            .map(|_| TyId::assoc_ty(db, self, name))
    }
}

/// What an associated equality binds: an associated type, or an associated
/// shape, at one mode (`Step<ref> = ref u256`) or at every mode as an impl
/// gives it (`shape Step = mut? u256`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct AssocBinding<'db> {
    pub name: IdentId<'db>,
    pub mode: Option<ShapeMode>,
}

impl<'db> AssocBinding<'db> {
    /// The binding of the associated type `name`, or of the associated shape
    /// `name` at every mode.
    pub fn of(name: IdentId<'db>) -> Self {
        Self { name, mode: None }
    }

    /// The order equalities are compared in: by name, then mode.
    pub fn canonical_cmp(&self, db: &'db dyn HirAnalysisDb, other: &Self) -> std::cmp::Ordering {
        (self.name.data(db), self.mode).cmp(&(other.name.data(db), other.mode))
    }

    pub fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        let mode = match self.mode {
            Some(ShapeMode::Ref) => "<ref>",
            Some(ShapeMode::Mut) => "<mut>",
            Some(ShapeMode::Inherit) | None => "",
        };
        format!("{}{mode}", self.name.data(db))
    }
}

/// A trait predicate, including associated equality evidence when present.
#[salsa::interned]
#[derive(Debug, PartialOrd, Ord)]
pub struct TraitInstId<'db> {
    #[returns(copy)]
    pub key: Trait<'db>,
    /// Regular type and const parameters: [Self, ExplicitTypeParam1, ..., ExplicitConstParamN]
    pub args: Vec<TyId<'db>>,

    /// Associated equalities specified by user, eg `Iterator<Item=i32>` or
    /// `Driver<Step<ref> = ref u256>`
    pub assoc_type_bindings: IndexMap<AssocBinding<'db>, TyId<'db>>,

    /// The modes the predicate grants the trait's `mut?` methods at:
    /// `T: Trait` grants `ref`, `T: mut Trait` both. `mut?` (`T: mut? Trait`)
    /// is `mut` where the item it bounds is used at `mut`, and stands only
    /// in an item's declared bounds.
    #[returns(copy)]
    pub mode: ModeMarker,
}

impl<'db> TraitInstId<'db> {
    pub fn def(self, db: &'db dyn HirAnalysisDb) -> Trait<'db> {
        self.key(db)
    }

    pub fn new_simple(db: &'db dyn HirAnalysisDb, def: Trait<'db>, args: Vec<TyId<'db>>) -> Self {
        Self::new(db, def, args, IndexMap::new(), ModeMarker::None)
    }

    /// The predicate with `mode` in place of its own.
    pub fn with_mode(self, db: &'db dyn HirAnalysisDb, mode: ModeMarker) -> Self {
        Self::new(
            db,
            self.def(db),
            self.args(db).clone(),
            self.assoc_type_bindings(db).clone(),
            mode,
        )
    }

    /// The predicate with `self_ty` as its `Self` argument.
    pub fn with_self_ty(self, db: &'db dyn HirAnalysisDb, self_ty: TyId<'db>) -> Self {
        let mut args = self.args(db).clone();
        match args.first_mut() {
            Some(arg) => *arg = self_ty,
            None => args.push(self_ty),
        }
        Self::new(
            db,
            self.def(db),
            args,
            self.assoc_type_bindings(db).clone(),
            self.mode(db),
        )
    }

    /// The predicate a bound of an item used at `mode` is: a `mut?` bound is
    /// `mut` at `mut`, and plain otherwise.
    pub fn at_mode(self, db: &'db dyn HirAnalysisDb, mode: Option<BorrowKind>) -> Self {
        match (self.mode(db), mode) {
            (ModeMarker::MaybeMut, Some(BorrowKind::Mut)) => self.with_mode(db, ModeMarker::Mut),
            (ModeMarker::MaybeMut, _) => self.with_mode(db, ModeMarker::None),
            _ => self,
        }
    }

    /// The goal that the predicate's self type grants the trait's `mut?`
    /// methods at `mut`: `T: mut Trait`, which the predicate's associated
    /// equalities, checked with it, need not repeat.
    pub fn mut_goal(self, db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(
            db,
            self.def(db),
            self.args(db).clone(),
            IndexMap::new(),
            ModeMarker::Mut,
        )
    }

    /// Whether the predicate grants the `mut` modes of the trait's `mut?`
    /// methods.
    pub fn is_mut(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.mode(db) == ModeMarker::Mut
    }

    /// Extract positional identity only. The caller remains responsible for
    /// preserving this predicate's associated equalities as evidence.
    pub fn trait_ref(self, db: &'db dyn HirAnalysisDb) -> TraitRefId<'db> {
        TraitRefId::new(db, self.def(db), self.args(db).to_vec())
    }

    pub fn with_fresh_vars(
        db: &'db dyn HirAnalysisDb,
        def: Trait<'db>,
        table: &mut UnificationTable<'db>,
    ) -> Self {
        let args = def
            .params(db)
            .iter()
            .map(|ty| table.new_var_from_param(*ty))
            .collect::<Vec<_>>();
        Self::new_simple(db, def, args)
    }

    pub fn assoc_ty_bindings(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<(AssocBinding<'db>, TyId<'db>)> {
        self.assoc_type_bindings(db)
            .iter()
            .map(|(&binding, &ty)| (binding, ty))
            .collect()
    }

    /// The type the predicate binds `binding` to: its own equality, or, for
    /// an associated shape at a mode, the shape it binds at every mode, at
    /// that mode.
    pub fn bound_assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        binding: AssocBinding<'db>,
    ) -> Option<TyId<'db>> {
        let bindings = self.assoc_type_bindings(db);
        if let Some(&ty) = bindings.get(&binding) {
            return Some(ty);
        }
        let shape = *bindings.get(&AssocBinding::of(binding.name))?;
        Some(
            binding
                .mode
                .map_or(shape, |mode| shape.at_shape_mode(db, mode)),
        )
    }

    pub fn project_assoc_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        self.trait_ref(db).project_assoc_ty(db, name)
    }

    /// Normalize arguments of this trait instance.
    pub(crate) fn normalize(
        self,
        db: &'db dyn HirAnalysisDb,
        scope: crate::core::hir_def::scope_graph::ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        let normalized_args: Vec<_> = self
            .args(db)
            .iter()
            .map(|&arg| crate::analysis::ty::normalize::normalize_ty(db, arg, scope, assumptions))
            .collect();
        Self::new(
            db,
            self.def(db),
            normalized_args,
            self.assoc_type_bindings(db).clone(),
            self.mode(db),
        )
    }

    pub fn pretty_print(self, db: &dyn HirAnalysisDb, as_pred: bool) -> String {
        if as_pred {
            let inst = self.pretty_print(db, false);
            let self_ty = self.self_ty(db);
            format!("{}: {}", self_ty.pretty_print(db), inst)
        } else if let Some(callable) = CallableTrait::of(db, self.def(db))
            && let Some(params) = callable_params(db, self)
        {
            // A callable trait whose shape lists its parameters prints as the
            // sugar that names it, `Fn(own A, B) -> R`; `-> _` when the
            // result is left open.
            let ret =
                self.bound_assoc_ty(db, AssocBinding::of(IdentId::new(db, "Out".to_string())));
            let params: Vec<_> = params
                .iter()
                .map(|param| match param {
                    Shape::Access(BorrowKind::Ref, ty) => ty.pretty_print(db).to_string(),
                    _ => param.pretty_print_product(db),
                })
                .collect();
            let name = if callable.mut_receiver { "FnMut" } else { "Fn" };
            let ret = match ret {
                Some(ret) if ret == TyId::unit(db) => String::new(),
                Some(ret) => format!(" -> {}", ret.pretty_print(db)),
                None => " -> _".to_string(),
            };
            format!("{name}({}){ret}", params.join(", "))
        } else {
            let mut s = self.mode(db).prefix().to_string();
            s.push_str(
                self.def(db)
                    .name(db)
                    .to_opt()
                    .map_or("<unknown>", |n| n.data(db).as_str()),
            );

            let mut args = self.args(db).iter().map(|ty| ty.pretty_print(db));
            // Skip the first type parameter since it's the implementor type.
            args.next();

            let mut has_generics = false;
            if let Some(first) = args.next() {
                s.push('<');
                s.push_str(first);
                for arg in args {
                    s.push_str(", ");
                    s.push_str(arg);
                }
                has_generics = true;
            }

            // Add associated type bindings
            if !self.assoc_type_bindings(db).is_empty() {
                if !has_generics {
                    s.push('<');
                } else {
                    s.push_str(", ");
                }

                let mut first_assoc = true;
                for (binding, ty) in self.assoc_type_bindings(db) {
                    if !first_assoc {
                        s.push_str(", ");
                    }
                    first_assoc = false;
                    s.push_str(&binding.pretty_print(db));
                    s.push_str(" = ");
                    s.push_str(ty.pretty_print(db));
                }
                has_generics = true;
            }

            if has_generics {
                s.push('>');
            }

            s
        }
    }

    pub fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.args(db)[0]
    }
}

// Represents a trait definition.
// (TraitDef struct and impl removed)

// (TraitMethod struct and impl removed)

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use common::indexmap::IndexMap;

    use super::{TraitInstId, impls_for_trait_def, resolve_trait_impl_instance};
    use crate::analysis::ty::trait_resolution::{Selection, TraitSolveCx};
    use crate::hir_def::{IdentId, ItemKind};
    use crate::test_db::HirAnalysisTestDb;

    #[test]
    fn trait_env_collects_overlapping_constrained_impls_without_cycle() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("trait_env_collects_overlapping_constrained_impls_without_cycle.fe"),
            r#"
trait Marker {}
trait Foo {}

impl<T> Foo for T where T: Marker {}
impl<T> Foo for T where T: Marker {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let trait_ = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_)
                    if trait_
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "Foo") =>
                {
                    Some(trait_)
                }
                _ => None,
            })
            .expect("missing `Foo` trait");

        let impls = impls_for_trait_def(&db, top_mod.ingot(&db), trait_);
        assert_eq!(impls.len(), 2);
    }

    #[test]
    fn resolved_impl_instance_preserves_default_assoc_template_and_concrete_args() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "resolved_impl_instance_preserves_default_assoc_template_and_concrete_args.fe",
            ),
            r#"
trait Projects<T> {
    type Output = T
}

struct Wrapper<T> {}

impl<T> Projects<T> for Wrapper<T> {}

fn probe(value: own Wrapper<u16>) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let trait_ = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_) => Some(trait_),
                _ => None,
            })
            .expect("missing Projects trait");
        let func = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func),
                _ => None,
            })
            .expect("missing probe function");
        let self_ty = func
            .params(&db)
            .next()
            .expect("missing probe parameter")
            .ty(&db);
        let projected_arg = self_ty.generic_args(&db)[0];
        let inst = TraitInstId::new(
            &db,
            trait_,
            vec![self_ty, projected_arg],
            IndexMap::new(),
            crate::hir_def::ModeMarker::None,
        );
        let Selection::Unique(resolved) =
            resolve_trait_impl_instance(&db, TraitSolveCx::new(&db, func.scope()), inst)
        else {
            panic!("expected one concrete Projects implementation");
        };
        let output = IdentId::new(&db, "Output");
        let template = resolved
            .assoc_ty_template(&db, output)
            .expect("missing default associated type template");
        let instantiated = resolved
            .instantiated_assoc_ty(&db, output)
            .expect("missing instantiated default associated type");

        assert!(template.has_param(&db));
        assert_eq!(instantiated.pretty_print(&db).to_string(), "u16");
        assert_eq!(resolved.impl_args(&db), &[instantiated]);
    }
}
