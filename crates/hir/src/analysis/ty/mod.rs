use crate::hir_def::{
    IdentId, ItemKind, TopLevelMod, Trait, TypeAlias,
    scope_graph::{ScopeGraph, ScopeId},
};
use adt_def::AdtRef;
use def_analysis::check_recursive_adt;
use diagnostics::{DefConflictError, TraitLowerDiag, TyLowerDiag};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec1::SmallVec;
// TraitDef and lower_trait have been eliminated
use trait_resolution::constraint::super_trait_cycle;
use ty_def::{InvalidCause, TyData};
use ty_lower::lower_type_alias;

use crate::{
    analysis::ty::def_analysis::validate_generic_params_for_owner,
    hir_def::GenericParamOwner,
};
use crate::analysis::{
    HirAnalysisDb, analysis_pass::ModuleAnalysisPass, diagnostics::DiagnosticVoucher,
};

pub mod adt_def;
pub mod binder;
pub mod canonical;
pub mod const_ty;

pub mod decision_tree;
pub mod def_analysis;
pub mod diagnostics;
pub mod fold;
pub mod func_def;
mod method_cmp;
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
        let mut diags = vec![];
        let mut cycle_participants = FxHashSet::<AdtRef<'db>>::default();

        for &s in top_mod.all_structs(db) {
            diags.extend(s.analyze(db).iter().map(|d| d.to_voucher()));
            let adt = AdtRef::from(s);
            if !cycle_participants.contains(&adt)
                && let Some(cycle) = check_recursive_adt(db, adt)
            {
                diags.push(Box::new(TyLowerDiag::RecursiveType(cycle.clone())) as _);
                cycle_participants.extend(cycle.iter().map(|m| m.adt));
            }
        }
        for &e in top_mod.all_enums(db) {
            diags.extend(e.analyze(db).iter().map(|d| d.to_voucher()));
            let adt = AdtRef::from(e);
            if !cycle_participants.contains(&adt)
                && let Some(cycle) = check_recursive_adt(db, adt)
            {
                diags.push(Box::new(TyLowerDiag::RecursiveType(cycle.clone())) as _);
                cycle_participants.extend(cycle.iter().map(|m| m.adt));
            }
        }
        for &c in top_mod.all_contracts(db) {
            diags.extend(c.analyze(db).iter().map(|d| d.to_voucher()));
            let adt = AdtRef::from(c);
            if !cycle_participants.contains(&adt)
                && let Some(cycle) = check_recursive_adt(db, adt)
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
        top_mod
            .all_funcs(db)
            .iter()
            .flat_map(|func| &ty_check::check_func_body(db, *func).0)
            .map(|diag| diag.to_voucher())
            .collect()
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
            if !cycle_participants.contains(hir_trait)
                && let Some(cycle) = super_trait_cycle(db, *hir_trait)
            {
                diags.push(Box::new(TraitLowerDiag::CyclicSuperTraits(cycle.clone())) as _);
                cycle_participants.extend(cycle.iter());
            }
            diags.extend(hir_trait.analyze(db).iter().map(|d| d.to_voucher()))
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
            .flat_map(|impl_| impl_.analyze(db))
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
            .flat_map(|trait_| trait_.analyze(db))
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
        top_mod
            .all_funcs(db)
            .iter()
            .flat_map(|func| func.analyze(db))
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
                // Generic param validations on owner
                diags.extend(
                    validate_generic_params_for_owner(db, GenericParamOwner::TypeAlias(alias))
                        .into_iter()
                        .map(|d| Box::new(d) as _),
                );
                diags.extend(alias.def_analyzer(db).analyze().into_iter().map(|d| Box::new(d) as _));
            }
        }
        diags
    }
}
