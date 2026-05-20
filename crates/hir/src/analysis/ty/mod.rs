use crate::analysis::ty::diagnostics::BodyDiag;
use crate::analysis::ty::effects::{ResolvedEffectKey, resolve_effect_key};
use crate::analysis::ty::trait_resolution::{
    GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
};
use crate::analysis::ty::ty_check::EffectParamOwner;
use crate::core::adt_lower::lower_adt;
use crate::core::hir_def::{
    IdentId, ItemKind, PathId, TopLevelMod, Trait, TypeAlias,
    scope_graph::{ScopeGraph, ScopeId},
};
use adt_def::{AdtDef, AdtRef};
use common::indexmap::IndexMap;
use diagnostics::{DefConflictError, TraitLowerDiag, TyLowerDiag};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec1::SmallVec;
use trait_def::impls_for_trait_def;
use trait_resolution::constraint::super_trait_cycle;
use ty_def::{BorrowKind, InvalidCause, TyBase, TyData, TyId, instantiate_adt_field_ty};
use ty_lower::lower_type_alias;

use crate::analysis::name_resolution::{PathRes, resolve_path};
use crate::analysis::{
    HirAnalysisDb, analysis_pass::ModuleAnalysisPass, diagnostics::DiagnosticVoucher,
};
use crate::semantic::diagnostics::Diagnosable;

pub mod adt_def;
pub mod assoc_const;
pub mod binder;
pub mod canonical;
pub(crate) mod const_check;
pub mod const_expr;
pub mod const_ty;
pub mod corelib;
pub mod effects;
pub mod msg_selector;

pub mod decision_tree;
pub mod diagnostics;
pub mod fold;
pub(crate) mod layout_holes;
pub(crate) mod method_cmp;
pub mod method_table;
pub mod normalize;
pub mod pattern_analysis;
pub mod pattern_ir;
pub mod pattern_types;
pub mod provider;
pub(crate) mod scratch;
pub mod trait_def;
pub mod trait_lower;
pub mod trait_resolution; // This line was previously 'pub mod name_resolution;'
pub mod ty_check;
pub mod ty_def;
pub mod ty_error;
pub mod ty_lower;
pub mod unify;
pub mod visitor;

pub use layout_holes::ty_contains_const_hole;
pub use msg_selector::MsgSelectorAnalysisPass;
pub use provider::{
    ProviderAddressSpace, ProviderKind, ProviderSemantics, ProviderTransport,
    RootProviderRegistration, RootProviderSiteKind, address_space_from_ty, provider_semantics,
    registered_root_providers,
};

const DEFAULT_TARGET_TY_PATH: &[&str] = &["std", "evm", "EvmTarget"];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EffectHandleMetadata<'db> {
    pub address_space: TyId<'db>,
    pub target_ty: TyId<'db>,
}

fn ty_may_require_effect_handle_metadata<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    matches!(
        ty.base_ty(db).data(db),
        TyData::TyParam(_)
            | TyData::AssocTy(_)
            | TyData::QualifiedTy(_)
            | TyData::TyBase(TyBase::Adt(_))
    )
}

pub fn ty_is_borrow<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<(BorrowKind, TyId<'db>)> {
    match ty.as_capability(db) {
        Some((ty_def::CapabilityKind::Mut, inner)) => Some((BorrowKind::Mut, inner)),
        Some((ty_def::CapabilityKind::Ref | ty_def::CapabilityKind::View, inner)) => {
            Some((BorrowKind::Ref, inner))
        }
        None => None,
    }
}

pub fn ty_is_copy<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
    assumptions: PredicateListId<'db>,
) -> bool {
    // Borrow/view handles (`mut`/`ref`/`view`) are always copyable, even without an explicit
    // `Copy` impl.
    if ty.as_capability(db).is_some() {
        return true;
    }

    // Built-in primitives are always `Copy`, independent of trait solving.
    if ty == TyId::unit(db) || ty.is_bool(db) || ty.is_integral(db) {
        return true;
    }

    ty_is_copy_query(db, scope, ty, assumptions)
}

#[salsa::tracked]
fn ty_is_copy_query<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let Some(copy_trait) = corelib::resolve_core_trait(db, scope, &["marker", "Copy"]) else {
        return false;
    };
    let inst = trait_def::TraitInstId::new_simple(db, copy_trait, vec![ty]);
    let inst = inst.normalize(db, scope, assumptions);
    if assumptions
        .list(db)
        .iter()
        .any(|&pred| pred.normalize(db, scope, assumptions) == inst)
    {
        return true;
    }
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    if !copy_goal_has_possible_impl(db, solve_cx, inst) {
        return false;
    }
    matches!(
        is_goal_satisfiable(db, solve_cx, inst),
        GoalSatisfiability::Satisfied(_)
    )
}

fn copy_goal_has_possible_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: trait_def::TraitInstId<'db>,
) -> bool {
    let Some(self_base) = concrete_copy_candidate_base(db, inst.self_ty(db)) else {
        return true;
    };

    let trait_def = inst.def(db);
    let (primary, secondary) = solve_cx.search_ingots_for_trait_inst(db, inst);
    [Some(primary), secondary]
        .into_iter()
        .flatten()
        .any(|ingot| {
            impls_for_trait_def(db, ingot, trait_def)
                .iter()
                .any(|implementor| {
                    let impl_self = implementor.skip_binder().self_ty(db);
                    copy_impl_self_may_match(db, impl_self, self_base)
                })
        })
}

fn concrete_copy_candidate_base<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    if ty.has_param(db) || ty.has_var(db) {
        return None;
    }
    let base = ty.base_ty(db);
    match base.data(db) {
        TyData::AssocTy(_) | TyData::QualifiedTy(_) => None,
        _ => Some(base),
    }
}

fn copy_impl_self_may_match<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_self: TyId<'db>,
    target_base: TyId<'db>,
) -> bool {
    let impl_base = impl_self.base_ty(db);
    if impl_base.has_param(db)
        || impl_base.has_var(db)
        || matches!(
            impl_base.data(db),
            TyData::AssocTy(_) | TyData::QualifiedTy(_)
        )
    {
        return true;
    }
    impl_base == target_base
}

pub fn ty_is_noesc<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    fn inner<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        visiting: &mut FxHashSet<TyId<'db>>,
    ) -> bool {
        if !visiting.insert(ty) {
            return false;
        }

        let result = if ty.as_capability(db).is_some() {
            true
        } else if ty.is_tuple(db) {
            ty.field_types(db)
                .into_iter()
                .any(|field_ty| inner(db, field_ty, visiting))
        } else if ty.is_array(db) {
            let (_, args) = ty.decompose_ty_app(db);
            args.first()
                .copied()
                .is_some_and(|elem_ty| inner(db, elem_ty, visiting))
        } else if let Some(adt_def) = ty.adt_def(db) {
            match adt_def.adt_ref(db) {
                AdtRef::Struct(_) => ty
                    .field_types(db)
                    .into_iter()
                    .any(|field_ty| inner(db, field_ty, visiting)),
                AdtRef::Enum(_) => {
                    let args = ty.generic_args(db);
                    adt_def
                        .fields(db)
                        .iter()
                        .enumerate()
                        .any(|(variant_idx, variant)| {
                            variant.iter_types(db).enumerate().any(|(field_idx, _)| {
                                inner(
                                    db,
                                    instantiate_adt_field_ty(
                                        db,
                                        adt_def,
                                        variant_idx,
                                        field_idx,
                                        args,
                                    ),
                                    visiting,
                                )
                            })
                        })
                }
            }
        } else {
            false
        };

        visiting.remove(&ty);
        result
    }

    match ty.data(db) {
        TyData::TyVar(_) | TyData::Invalid(_) => false,
        _ => inner(db, ty, &mut FxHashSet::default()),
    }
}

/// An analysis pass for type definitions.
pub struct AdtDefAnalysisPass {}

impl ModuleAnalysisPass for AdtDefAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let adts = top_mod
            .all_structs(db)
            .iter()
            .copied()
            .map(AdtRef::from)
            .chain(top_mod.all_enums(db).iter().copied().map(AdtRef::from));

        let mut diags = vec![];
        let mut cycle_participants = FxHashSet::<AdtDef<'db>>::default();

        for adt_ref in adts {
            diags.extend(adt_ref.diags(db).into_iter().map(|d| d.to_voucher()));
            let adt = lower_adt(db, adt_ref);
            if !cycle_participants.contains(&adt)
                && let Some(cycle) = adt.recursive_cycle(db)
            {
                diags.push(Box::new(TyLowerDiag::RecursiveType(cycle.clone())) as _);
                cycle_participants.extend(cycle.iter().map(|m| m.adt));
            }
        }
        diags
    }
}

/// Checks for name conflicts of item definitions.
pub struct DefConflictAnalysisPass {}

impl ModuleAnalysisPass for DefConflictAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let graph = top_mod.scope_graph(db);

        walk(db, graph, top_mod.scope())
            .into_iter()
            .map(|d| Box::new(d) as _)
            .collect()
    }
}

fn walk<'db>(
    db: &'db dyn HirAnalysisDb,
    graph: &ScopeGraph<'db>,
    scope: ScopeId<'db>,
) -> Vec<DefConflictError<'db>> {
    let mut work: Vec<ScopeId<'db>> = vec![scope];

    #[derive(Hash, PartialEq, Eq)]
    enum Domain {
        Type,
        Val,
    }

    let mut defs = FxHashMap::<(Domain, IdentId<'db>), SmallVec<[ItemKind<'db>; 2]>>::default();
    let mut diags = vec![];

    while let Some(scope) = work.pop() {
        for item in graph.child_items(scope).filter(|i| i.name(db).is_some()) {
            let domain = match item {
                ItemKind::Func(_) | ItemKind::Const(_) => Domain::Val,

                ItemKind::Mod(_)
                | ItemKind::Struct(_)
                | ItemKind::Contract(_)
                | ItemKind::Enum(_)
                | ItemKind::TypeAlias(_)
                | ItemKind::Trait(_) => Domain::Type,

                ItemKind::TopMod(_)
                | ItemKind::StaticAssert(_)
                | ItemKind::Use(_)
                | ItemKind::Impl(_)
                | ItemKind::ImplTrait(_)
                | ItemKind::Body(_) => continue,
            };
            defs.entry((domain, item.name(db).unwrap()))
                .or_default()
                .push(item);
            if matches!(item, ItemKind::Mod(_)) {
                work.push(item.scope());
            }
        }
        diags.extend(
            defs.drain()
                .filter_map(|(_k, v)| (v.len() > 1).then_some(v))
                .map(DefConflictError),
        )
    }
    diags
}

pub struct BodyAnalysisPass {}

impl ModuleAnalysisPass for BodyAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        // Check function and const bodies; contract-specific analysis is handled separately.
        let mut diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = top_mod
            .all_funcs(db)
            .iter()
            .flat_map(|func| &ty_check::check_func_body(db, *func).0)
            .map(|diag| diag.to_voucher())
            .collect();

        diags.extend(
            top_mod
                .all_items(db)
                .iter()
                .filter_map(|item| match item {
                    ItemKind::Const(const_) => Some(*const_),
                    _ => None,
                })
                .flat_map(|const_| &ty_check::check_const_body(db, const_).0)
                .map(|diag| diag.to_voucher()),
        );

        diags.extend(
            top_mod
                .all_static_asserts(db)
                .iter()
                .flat_map(|assert_| ty_check::check_static_assert(db, *assert_).iter())
                .map(|diag| diag.to_voucher()),
        );

        diags
    }
}

/// An analysis pass for contract definitions.
/// This pass handles all contract-specific analysis:
/// - Contract field type validation
/// - Contract effects validation
/// - Recv blocks validation
pub struct ContractAnalysisPass {}

impl ModuleAnalysisPass for ContractAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = vec![];

        for &contract in top_mod.all_contracts(db) {
            // 1. Validate contract field types
            diags.extend(contract.diags(db).into_iter().map(|d| d.to_voucher()));

            // 2. Validate contract-level effects (`contract Foo uses (ctx: Ctx)`).
            let assumptions = PredicateListId::empty_list(db);
            let root_effect_ty = registered_root_providers(
                db,
                crate::analysis::ty::ty_check::EffectParamSite::Contract(contract),
            )
            .first()
            .map(|registration| registration.provider_ty);
            for (idx, effect) in contract.effects(db).data(db).iter().enumerate() {
                let Some(key_path) = effect
                    .key_path
                    .to_opt()
                    .filter(|path| path.ident(db).is_present())
                else {
                    continue;
                };

                match resolve_effect_key(db, key_path, contract.scope(), assumptions) {
                    ResolvedEffectKey::Trait(schema) => {
                        let Some(root_effect_ty) = root_effect_ty else {
                            continue;
                        };

                        let trait_req =
                            instantiate_trait_self(db, schema.into_trait_inst(db), root_effect_ty);
                        if matches!(
                            is_goal_satisfiable(
                                db,
                                TraitSolveCx::new(db, contract.scope())
                                    .with_assumptions(assumptions),
                                trait_req
                            ),
                            GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid
                        ) {
                            diags.push(Box::new(BodyDiag::ContractRootEffectTraitNotImplemented {
                                owner: EffectParamOwner::Contract(contract),

                                idx,
                                root_ty: root_effect_ty,
                                trait_req,
                            }) as _);
                        }
                    }
                    ResolvedEffectKey::Type(schema) => {
                        let given = normalize::normalize_ty(
                            db,
                            schema.carrier,
                            contract.scope(),
                            assumptions,
                        );
                        if !given.is_zero_sized(db) {
                            diags.push(Box::new(BodyDiag::ContractRootEffectTypeNotZeroSized {
                                owner: EffectParamOwner::Contract(contract),
                                key: key_path,
                                idx,
                                given,
                            }) as _);
                        }
                    }
                    ResolvedEffectKey::Invalid | ResolvedEffectKey::Other => {
                        diags.push(Box::new(BodyDiag::InvalidEffectKey {
                            owner: EffectParamOwner::Contract(contract),
                            key: key_path,
                            idx,
                        }) as _);
                    }
                }
            }

            // 3. Validate recv blocks
            diags.extend(
                ty_check::check_contract_recv_blocks(db, contract)
                    .iter()
                    .map(|diag| diag.to_voucher()),
            );

            diags.extend(
                ty_check::check_contract_immutable_fields_initialized(db, contract)
                    .iter()
                    .map(|diag| diag.to_voucher()),
            );

            if contract.init(db).is_some() {
                diags.extend(
                    ty_check::check_contract_init_body(db, contract)
                        .0
                        .iter()
                        .map(|diag| diag.to_voucher()),
                );
            }

            let recvs = contract.recvs(db);
            for (recv_idx, recv) in recvs.data(db).iter().enumerate() {
                diags.extend(
                    ty_check::check_contract_recv_block(db, contract, recv_idx as u32)
                        .iter()
                        .map(|diag| diag.to_voucher()),
                );

                for (arm_idx, _) in recv.arms.data(db).iter().enumerate() {
                    diags.extend(
                        ty_check::check_contract_recv_arm_body(
                            db,
                            contract,
                            recv_idx as u32,
                            arm_idx as u32,
                        )
                        .0
                        .iter()
                        .map(|diag| diag.to_voucher()),
                    );
                }
            }
        }

        diags
    }
}

#[salsa::tracked]
pub fn resolve_default_root_effect_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<TyId<'db>> {
    let target_path = PathId::from_segments(db, DEFAULT_TARGET_TY_PATH);
    let target_ty = match resolve_path(db, target_path, scope, assumptions, false).ok()? {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => ty,
        _ => return None,
    };

    let target_trait = corelib::resolve_core_trait(db, scope, &["contracts", "Target"])?;
    let inst_target =
        trait_def::TraitInstId::new(db, target_trait, vec![target_ty], IndexMap::new());
    let root_ident = IdentId::new(db, "RootEffect".to_owned());
    Some(normalize::normalize_ty(
        db,
        TyId::assoc_ty(db, inst_target, root_ident),
        scope,
        assumptions,
    ))
}

pub fn effect_handle_metadata<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    ty: TyId<'db>,
) -> Option<EffectHandleMetadata<'db>> {
    let ty = ty_def::strip_derived_adt_layout_args(db, ty);
    if ty.as_capability(db).is_some() || !ty_may_require_effect_handle_metadata(db, ty) {
        return None;
    }
    let effect_handle = corelib::resolve_core_trait(db, scope, &["EffectHandle"])?;
    let address_space_ident = IdentId::new(db, "AddressSpace".to_string());
    let target_ident = IdentId::new(db, "Target".to_string());
    let inst = trait_def::TraitInstId::new(db, effect_handle, vec![ty], IndexMap::new());
    match is_goal_satisfiable(
        db,
        TraitSolveCx::new(db, scope).with_assumptions(assumptions),
        inst,
    ) {
        GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_) => None,
        GoalSatisfiability::Satisfied(_) | GoalSatisfiability::NeedsConfirmation(_) => {
            let address_space = normalize::normalize_ty(
                db,
                inst.assoc_ty(db, address_space_ident)?,
                scope,
                assumptions,
            );
            let target_ty = normalize::normalize_ty(
                db,
                inst.assoc_ty(db, target_ident).unwrap_or(ty),
                scope,
                assumptions,
            );
            (!address_space.has_invalid(db)
                && !ty_contains_const_hole(db, address_space)
                && !target_ty.has_invalid(db))
            .then_some(EffectHandleMetadata {
                address_space,
                target_ty,
            })
        }
    }
}

pub(crate) fn instantiate_trait_self<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: trait_def::TraitInstId<'db>,
    self_ty: TyId<'db>,
) -> trait_def::TraitInstId<'db> {
    let def = inst.def(db);
    let mut args = inst.args(db).to_vec();
    if args.is_empty() {
        args.push(self_ty);
    } else {
        args[0] = self_ty;
    }
    trait_def::TraitInstId::new(db, def, args, inst.assoc_type_bindings(db).clone())
}

/// An analysis pass for trait definitions.
pub struct TraitAnalysisPass {}

impl ModuleAnalysisPass for TraitAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut diags = vec![];
        let mut cycle_participants = FxHashSet::<Trait<'db>>::default();

        for hir_trait in top_mod.all_traits(db) {
            let trait_ = *hir_trait;
            if !cycle_participants.contains(&trait_)
                && let Some(cycle) = super_trait_cycle(db, trait_)
            {
                diags.push(Box::new(TraitLowerDiag::CyclicSuperTraits(cycle.clone())) as _);
                cycle_participants.extend(cycle.iter());
            }
            diags.extend(hir_trait.diags(db).into_iter().map(|d| d.to_voucher()))
        }
        diags
    }
}

pub struct ImplAnalysisPass {}

impl ModuleAnalysisPass for ImplAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        top_mod
            .all_impls(db)
            .iter()
            .flat_map(|impl_| impl_.diags(db))
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for `ImplTrait'.
pub struct ImplTraitAnalysisPass {}

impl ModuleAnalysisPass for ImplTraitAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        top_mod
            .all_impl_traits(db)
            .iter()
            .flat_map(|impl_trait| impl_trait.diags(db))
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for `ImplTrait'.
pub struct FuncAnalysisPass {}

impl ModuleAnalysisPass for FuncAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        // Function diagnostics are handled here; contract-specific diagnostics are separate.
        top_mod
            .all_funcs(db)
            .iter()
            .flat_map(|func| func.diags(db))
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for type aliases.
pub struct TypeAliasAnalysisPass {}

/// This function implements analysis for the type alias definition.
/// The analysis includes the following:
/// - Check if the type alias is not recursive.
/// - Check if the type in the type alias is well-formed.
///
/// NOTE: This function doesn't check the satisfiability of the type since our
/// type system treats the alias as kind of macro, meaning type alias isn't
/// included in the type system. Satisfiability is checked where the type alias
/// is used.
impl ModuleAnalysisPass for TypeAliasAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut diags = vec![];
        let mut cycle_participants = FxHashSet::<TypeAlias>::default();

        for &alias in top_mod.all_type_aliases(db) {
            if cycle_participants.contains(&alias) {
                continue;
            }

            let ta = lower_type_alias(db, alias);
            let ty = ta.alias_to.skip_binder();
            if let TyData::Invalid(InvalidCause::AliasCycle(cycle)) = ty.data(db) {
                if let Some(diag) = ty.emit_diag(db, alias.span().ty().into()) {
                    diags.push(diag.to_voucher());
                }
                cycle_participants.extend(cycle.iter());
            } else {
                // Delegate to semantic alias diagnostics
                diags.extend(alias.diags(db).into_iter().map(|d| Box::new(d) as _));
            }
        }
        diags
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use common::indexmap::IndexMap;

    use super::{
        PredicateListId, TraitSolveCx, adt_def::AdtRef, copy_goal_has_possible_impl,
        corelib::resolve_core_trait, trait_def::TraitInstId, ty_def::TyId, ty_is_copy,
    };
    use crate::{hir_def::ItemKind, test_db::HirAnalysisTestDb};

    fn named_struct_ty<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: crate::hir_def::TopLevelMod<'db>,
        name: &str,
    ) -> TyId<'db> {
        let struct_ = top_mod
            .children_non_nested(db)
            .find_map(|item| match item {
                ItemKind::Struct(struct_)
                    if struct_
                        .name(db)
                        .to_opt()
                        .is_some_and(|ident| ident.data(db) == name) =>
                {
                    Some(struct_)
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("missing `{name}` struct"));
        TyId::adt(db, AdtRef::from(struct_).as_adt(db))
    }

    #[test]
    fn copy_prefilter_rejects_concrete_noncopy_struct() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("copy_prefilter_rejects_concrete_noncopy_struct.fe"),
            r#"
struct Plain {
    value: u256,
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let copy_trait = resolve_core_trait(&db, top_mod.scope(), &["marker", "Copy"])
            .expect("missing Copy trait");
        let plain = named_struct_ty(&db, top_mod, "Plain");
        let copy_plain = TraitInstId::new(&db, copy_trait, vec![plain], IndexMap::new());
        let assumptions = PredicateListId::empty_list(&db);
        let solve_cx = TraitSolveCx::new(&db, top_mod.scope()).with_assumptions(assumptions);

        assert!(!copy_goal_has_possible_impl(&db, solve_cx, copy_plain));
        assert!(!ty_is_copy(&db, top_mod.scope(), plain, assumptions));
    }

    #[test]
    fn copy_prefilter_preserves_explicit_copy_struct() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("copy_prefilter_preserves_explicit_copy_struct.fe"),
            r#"
struct Explicit {
    value: u256,
}

impl Copy for Explicit {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let copy_trait = resolve_core_trait(&db, top_mod.scope(), &["marker", "Copy"])
            .expect("missing Copy trait");
        let explicit = named_struct_ty(&db, top_mod, "Explicit");
        let copy_explicit = TraitInstId::new(&db, copy_trait, vec![explicit], IndexMap::new());
        let assumptions = PredicateListId::empty_list(&db);
        let solve_cx = TraitSolveCx::new(&db, top_mod.scope()).with_assumptions(assumptions);

        assert!(copy_goal_has_possible_impl(&db, solve_cx, copy_explicit));
        assert!(ty_is_copy(&db, top_mod.scope(), explicit, assumptions));
    }
}
