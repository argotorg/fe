use crate::analysis::ty::canonical::Canonicalized;
use crate::analysis::ty::diagnostics::BodyDiag;
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
use trait_resolution::constraint::super_trait_cycle;
use ty_def::{InvalidCause, TyData, TyId};
use ty_lower::lower_type_alias;

use crate::analysis::name_resolution::{PathRes, resolve_path};
use crate::analysis::{
    HirAnalysisDb, analysis_pass::ModuleAnalysisPass, diagnostics::DiagnosticVoucher,
};
use crate::semantic::diagnostics::Diagnosable;

pub mod adt_def;
pub mod binder;
pub mod canonical;
pub(crate) mod const_check;
pub mod const_eval;
pub mod const_expr;
pub mod const_ty;
pub mod corelib;
pub(crate) mod ctfe;
pub mod effects;
pub mod msg_selector;

pub mod decision_tree;
pub mod diagnostics;
pub mod fold;
pub(crate) mod method_cmp;
pub mod method_table;
pub mod normalize;
pub mod pattern_analysis;
pub mod simplified_pattern;
pub mod trait_def;
pub mod trait_lower;
pub mod trait_resolution; // This line was previously 'pub mod name_resolution;'
pub mod ty_check;
pub mod ty_def;
pub mod ty_error;
pub mod ty_lower;
pub mod unify;
pub mod visitor;

pub use msg_selector::MsgSelectorAnalysisPass;

const DEFAULT_TARGET_TY_PATH: &[&str] = &["std", "evm", "EvmTarget"];

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
            let root_effect_ty = resolve_default_root_effect_ty(db, contract.scope(), assumptions);
            for (idx, effect) in contract.effects(db).data(db).iter().enumerate() {
                let Some(key_path) = effect.key_path.to_opt() else {
                    continue;
                };

                let resolved = resolve_path(db, key_path, contract.scope(), assumptions, false);
                match resolved {
                    Ok(PathRes::Trait(trait_inst)) => {
                        let Some(root_effect_ty) = root_effect_ty else {
                            continue;
                        };

                        let trait_req = instantiate_trait_self(db, trait_inst, root_effect_ty);
                        let goal = Canonicalized::new(db, trait_req).value;
                        if matches!(
                            is_goal_satisfiable(
                                db,
                                TraitSolveCx::new(db, contract.scope())
                                    .with_assumptions(assumptions),
                                goal
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
                    Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => {
                        let given = normalize::normalize_ty(db, ty, contract.scope(), assumptions);
                        if !given.is_zero_sized(db) {
                            diags.push(Box::new(BodyDiag::ContractRootEffectTypeNotZeroSized {
                                owner: EffectParamOwner::Contract(contract),
                                key: key_path,
                                idx,
                                given,
                            }) as _);
                        }
                    }
                    Ok(_) | Err(_) => {
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

fn resolve_default_root_effect_ty<'db>(
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

fn instantiate_trait_self<'db>(
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
