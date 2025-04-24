use common::indexmap::IndexSet;
use hir::hir_def::{GenericParam, GenericParamOwner, Impl, ItemKind, TypeBound};

use crate::{
    ty::{
        adt_def::{lower_adt, AdtDef},
        binder::Binder,
        func_def::{FuncDef, HirFuncDefKind},
        trait_def::{Implementor, TraitDef, TraitInstId},
        trait_lower::{lower_impl_trait, lower_trait, lower_trait_ref},
        trait_resolution::PredicateListId,
        ty_def::{TyBase, TyData, TyId, TyVarSort},
        ty_lower::{collect_generic_params, lower_hir_ty},
        unify::InferenceKey,
    },
    HirAnalysisDb,
};

/// Returns a constraints list which is derived from the given type.
#[salsa::tracked]
pub(crate) fn ty_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> PredicateListId<'db> {
    let (base, args) = ty.decompose_ty_app(db);
    let (params, base_constraints) = match base.data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => (adt.params(db), collect_adt_constraints(db, *adt)),
        TyData::TyBase(TyBase::Func(func_def)) => (
            func_def.params(db),
            collect_func_def_constraints(db, *func_def, true),
        ),
        _ => {
            return PredicateListId::empty_list(db);
        }
    };

    let mut args = args.to_vec();

    // Generalize unbound type parameters.
    for &arg in params.iter().skip(args.len()) {
        let key = InferenceKey(args.len() as u32, Default::default());
        let ty_var = TyId::ty_var(db, TyVarSort::General, arg.kind(db).clone(), key);
        args.push(ty_var);
    }

    base_constraints.instantiate(db, &args)
}

/// Collect super traits of the given trait.
/// The returned trait ref is bound by the given trait's generic parameters.
#[salsa::tracked(return_ref)]
pub(crate) fn collect_super_traits<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: TraitDef<'db>,
) -> IndexSet<Binder<TraitInstId<'db>>> {
    let hir_trait = trait_.trait_(db);
    let self_param = trait_.self_param(db);
    let scope = trait_.trait_(db).scope();

    let mut super_traits = IndexSet::new();

    for &super_ in hir_trait.super_traits(db).iter() {
        if let Ok(inst) = lower_trait_ref(db, self_param, super_, scope) {
            super_traits.insert(Binder::bind(inst));
        }
    }

    for pred in hir_trait.where_clause(db).data(db) {
        if pred
            .ty
            .to_opt()
            .map(|ty| ty.is_self_ty(db))
            .unwrap_or_default()
        {
            for bound in &pred.bounds {
                if let TypeBound::Trait(bound) = bound {
                    if let Ok(inst) = lower_trait_ref(db, self_param, *bound, scope) {
                        super_traits.insert(Binder::bind(inst));
                    }
                }
            }
        }
    }

    super_traits
}

#[salsa::tracked(return_ref)]
pub fn super_trait_cycle<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: TraitDef<'db>,
) -> Option<Vec<TraitDef<'db>>> {
    super_trait_cycle_impl(db, trait_, &[])
}

pub fn super_trait_cycle_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: TraitDef<'db>,
    chain: &[TraitDef<'db>],
) -> Option<Vec<TraitDef<'db>>> {
    if chain.contains(&trait_) {
        return Some(chain.to_vec());
    }
    let bounds = collect_super_traits(db, trait_);
    if bounds.is_empty() {
        return None;
    }

    let chain = [chain, &[trait_]].concat();
    for t in bounds {
        if let Some(cycle) = super_trait_cycle_impl(db, t.skip_binder().def(db), &chain) {
            if cycle.contains(&trait_) {
                return Some(cycle.clone());
            }
        }
    }
    None
}

/// Collect trait constraints that are specified by the given trait definition.
/// This constraints describes 1. the constraints about self type(i.e.,
/// implementor type), and 2. the generic parameter constraints.
#[salsa::tracked]
pub(crate) fn collect_trait_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: TraitDef<'db>,
) -> Binder<PredicateListId<'db>> {
    let hir_trait = trait_.trait_(db);
    let collector = ConstraintCollector::new(db, hir_trait.into());

    Binder::bind(collector.collect())
}

/// Collect constraints that are specified by the given ADT definition.
#[salsa::tracked]
pub(crate) fn collect_adt_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
) -> Binder<PredicateListId<'db>> {
    let Some(owner) = adt.as_generic_param_owner(db) else {
        return Binder::bind(PredicateListId::empty_list(db));
    };
    let collector = ConstraintCollector::new(db, owner);

    Binder::bind(collector.collect())
}

#[salsa::tracked]
pub(crate) fn collect_impl_block_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: Impl<'db>,
) -> Binder<PredicateListId<'db>> {
    Binder::bind(ConstraintCollector::new(db, impl_.into()).collect())
}

/// Collect constraints that are specified by the given implementor(i.e., impl
/// trait).
#[salsa::tracked]
pub(crate) fn collect_implementor_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: Implementor<'db>,
) -> Binder<PredicateListId<'db>> {
    let impl_trait = implementor.hir_impl_trait(db);
    let collector = ConstraintCollector::new(db, impl_trait.into());

    Binder::bind(collector.collect())
}

#[salsa::tracked]
pub(crate) fn collect_func_def_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    func: FuncDef<'db>,
    include_parent: bool,
) -> Binder<PredicateListId<'db>> {
    let hir_func = match func.hir_def(db) {
        HirFuncDefKind::Func(func) => func,
        HirFuncDefKind::VariantCtor(var) => {
            let adt = lower_adt(db, var.enum_.into());
            if include_parent {
                return collect_adt_constraints(db, adt);
            } else {
                return Binder::bind(PredicateListId::empty_list(db));
            }
        }
    };

    let func_constraints = collect_func_def_constraints_impl(db, func);
    if !include_parent {
        return func_constraints;
    }

    let parent_constraints = match hir_func.scope().parent_item(db) {
        Some(ItemKind::Trait(trait_)) => collect_trait_constraints(db, lower_trait(db, trait_)),

        Some(ItemKind::Impl(impl_)) => collect_impl_block_constraints(db, impl_),

        Some(ItemKind::ImplTrait(impl_trait)) => {
            let Some(implementor) = lower_impl_trait(db, impl_trait) else {
                return func_constraints;
            };
            collect_implementor_constraints(db, implementor.instantiate_identity())
        }

        _ => return func_constraints,
    };

    Binder::bind(
        func_constraints
            .instantiate_identity()
            .merge(db, parent_constraints.instantiate_identity()),
    )
}

#[salsa::tracked]
pub(crate) fn collect_func_def_constraints_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    func: FuncDef<'db>,
) -> Binder<PredicateListId<'db>> {
    let hir_func = match func.hir_def(db) {
        HirFuncDefKind::Func(func) => func,
        HirFuncDefKind::VariantCtor(var) => {
            let adt_ref = var.enum_.into();
            let adt = lower_adt(db, adt_ref);
            return collect_adt_constraints(db, adt);
        }
    };

    Binder::bind(ConstraintCollector::new(db, hir_func.into()).collect())
}

struct ConstraintCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    predicates: IndexSet<TraitInstId<'db>>,
}

impl<'db> ConstraintCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, owner: GenericParamOwner<'db>) -> Self {
        Self {
            db,
            owner,

            predicates: IndexSet::new(),
        }
    }

    fn collect(mut self) -> PredicateListId<'db> {
        self.collect_constraints_from_generic_params();
        self.collect_constraints_from_where_clause();

        // Collect super traits from the trait definition and add them to the predicate
        // list.
        if let GenericParamOwner::Trait(trait_) = self.owner {
            let trait_def = lower_trait(self.db, trait_);
            self.push_predicate(TraitInstId::new(
                self.db,
                trait_def,
                collect_generic_params(self.db, self.owner)
                    .params(self.db)
                    .to_vec(),
            ));
        }

        PredicateListId::new(self.db, self.predicates.into_iter().collect::<Vec<_>>())
    }

    fn push_predicate(&mut self, pred: TraitInstId<'db>) {
        self.predicates.insert(pred);
    }

    fn collect_constraints_from_where_clause(&mut self) {
        let Some(where_clause) = self.owner.where_clause(self.db) else {
            return;
        };

        for hir_pred in where_clause.data(self.db) {
            let Some(hir_ty) = hir_pred.ty.to_opt() else {
                continue;
            };

            let ty = lower_hir_ty(self.db, hir_ty, self.owner.scope());

            // We don't need to collect super traits, please refer to
            // [`collect_super_traits`] function for details.
            if ty.has_invalid(self.db) || ty.is_trait_self(self.db) {
                continue;
            }

            self.add_bounds(ty, &hir_pred.bounds);
        }
    }

    fn collect_constraints_from_generic_params(&mut self) {
        let param_set = collect_generic_params(self.db, self.owner);
        let param_list = self.owner.params(self.db);

        for (i, hir_param) in param_list.data(self.db).iter().enumerate() {
            let GenericParam::Type(hir_param) = hir_param else {
                continue;
            };

            let ty = param_set.param_by_original_idx(self.db, i).unwrap();
            let bounds = &hir_param.bounds;
            self.add_bounds(ty, bounds)
        }
    }

    fn add_bounds(&mut self, bound_ty: TyId<'db>, bounds: &[TypeBound<'db>]) {
        for bound in bounds {
            let TypeBound::Trait(trait_ref) = bound else {
                continue;
            };

            let Ok(trait_inst) = lower_trait_ref(self.db, bound_ty, *trait_ref, self.owner.scope())
            else {
                continue;
            };

            self.push_predicate(trait_inst);
        }
    }
}
