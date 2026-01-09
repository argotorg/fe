use crate::analysis::ty::diagnostics::BodyDiag;
use crate::core::adt_lower::lower_adt;
use crate::core::hir_def::{
    IdentId, ItemKind, TopLevelMod, Trait, TypeAlias,
    scope_graph::{ScopeGraph, ScopeId},
};
use crate::span::{DesugaredOrigin, HirOrigin};
use adt_def::{AdtDef, AdtRef};
use diagnostics::{DefConflictError, TraitLowerDiag, TyLowerDiag};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec1::SmallVec;
use trait_resolution::constraint::super_trait_cycle;
use ty_def::{InvalidCause, TyData};
use ty_lower::lower_type_alias;

use crate::analysis::name_resolution::{PathRes, resolve_path};
use crate::analysis::{
    HirAnalysisDb, analysis_pass::ModuleAnalysisPass, diagnostics::DiagnosticVoucher,
};
use crate::semantic::diagnostics::Diagnosable;

pub mod adt_def;
pub mod binder;
pub mod canonical;
pub mod const_eval;
pub mod const_ty;
pub mod corelib;

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
        // Only check non-contract functions. Contract desugared functions
        // are checked in ContractAnalysisPass.
        top_mod
            .all_funcs(db)
            .iter()
            .filter(|func| {
                !matches!(
                    func.origin(db),
                    HirOrigin::Desugared(DesugaredOrigin::ContractLowering(_))
                )
            })
            .flat_map(|func| &ty_check::check_func_body(db, *func).0)
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for contract definitions.
/// This pass handles all contract-specific analysis:
/// - Contract field type validation
/// - Contract effects validation
/// - Recv blocks validation
/// - Desugared function type-checking (only if no prior errors)
pub struct ContractAnalysisPass {}

impl ModuleAnalysisPass for ContractAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let contract_desugared_funcs: Vec<_> = top_mod
            .all_funcs(db)
            .iter()
            .filter(|func| {
                matches!(
                    func.origin(db),
                    HirOrigin::Desugared(DesugaredOrigin::ContractLowering(_))
                )
            })
            .copied()
            .collect();

        let mut diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = vec![];

        for &contract in top_mod.all_contracts(db) {
            // 1. Validate contract field types
            diags.extend(contract.diags(db).into_iter().map(|d| d.to_voucher()));

            // 2. Validate contract-level effects (`contract Foo uses (ctx: Ctx)`).
            let assumptions = trait_resolution::PredicateListId::empty_list(db);
            for (idx, effect) in contract.effects(db).data(db).iter().enumerate() {
                let Some(key_path) = effect.key_path.to_opt() else {
                    continue;
                };

                if !matches!(
                    resolve_path(db, key_path, contract.scope(), assumptions, false),
                    Ok(PathRes::Ty(_) | PathRes::TyAlias(_, _) | PathRes::Trait(_))
                ) {
                    diags.push(Box::new(BodyDiag::InvalidEffectKey {
                        owner: ty_check::EffectParamOwner::Contract(contract),
                        key: key_path,
                        idx,
                    }) as _);
                }
            }

            // 3. Validate recv blocks
            diags.extend(
                ty_check::check_contract_recv_blocks(db, contract)
                    .iter()
                    .map(|diag| diag.to_voucher()),
            );

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

        // 4. Only type-check desugared functions if there are no contract errors.
        // This prevents cascading errors from malformed contract types.
        if diags.is_empty() {
            let desugared_diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = contract_desugared_funcs
                .iter()
                .flat_map(|func| &ty_check::check_func_body(db, *func).0)
                .map(|diag| diag.to_voucher())
                .collect();

            if !desugared_diags.is_empty() {
                tracing::error!("Desugared contract functions have diagnostics");
                diags.extend(desugared_diags);
            }
        }

        diags
    }
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
        // Skip contract-desugared functions. Their parameter types may contain
        // synthetic paths (e.g., `Msg::Variant::Args`) that can produce misleading
        // diagnostics if the msg type doesn't exist. Contract-specific diagnostics
        // are handled in ContractAnalysisPass.
        top_mod
            .all_funcs(db)
            .iter()
            .filter(|func| {
                !matches!(
                    func.origin(db),
                    HirOrigin::Desugared(DesugaredOrigin::ContractLowering(_))
                )
            })
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
