use crate::hir_def::{HirIngot, IdentId, Impl};
use common::ingot::Ingot;
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    canonical::Canonical,
    func_def::{FuncDef, lower_func},
    trait_resolution::constraint::collect_constraints,
    ty_def::{InvalidCause, TyBase, TyId},
    ty_lower::lower_hir_ty,
    unify::UnificationTable,
};
use crate::analysis::{HirAnalysisDb, ty::ty_def::TyData};

#[salsa::tracked(return_ref)]
pub(crate) fn collect_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> MethodTable<'db> {
    let mut collector = MethodCollector::new(db, ingot);

    let impls = ingot.all_impls(db);
    collector.collect_impls(impls);
    collector.finalize()
}

#[salsa::tracked(return_ref)]
pub(crate) fn probe_method<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
    name: IdentId<'db>,
) -> Vec<FuncDef<'db>> {
    let table = collect_methods(db, ingot);
    table.probe(db, ty, name)
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct MethodTable<'db> {
    buckets: FxHashMap<TyBase<'db>, MethodBucket<'db>>,
}

impl<'db> MethodTable<'db> {
    fn probe(
        &self,
        db: &'db dyn HirAnalysisDb,
        ty: Canonical<TyId<'db>>,
        name: IdentId<'db>,
    ) -> Vec<FuncDef<'db>> {
        let mut table = UnificationTable::new(db);
        let ty = ty.extract_identity(&mut table);
        let Some(base) = Self::extract_ty_base(ty, db) else {
            return vec![];
        };

        if let Some(bucket) = self.buckets.get(base) {
            bucket.probe(&mut table, ty, name)
        } else {
            vec![]
        }
    }

    fn new() -> Self {
        Self {
            buckets: FxHashMap::default(),
        }
    }

    fn finalize(self) -> Self {
        self
    }

    fn insert(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>, func: FuncDef<'db>) {
        let Some(base) = Self::extract_ty_base(ty, db) else {
            return;
        };

        let name = func.name(db);
        let bucket = self.buckets.entry(*base).or_insert_with(MethodBucket::new);
        let methods = bucket.methods.entry(Binder::bind(ty)).or_default();
        methods.insert(name, func);
    }

    fn extract_ty_base(ty: TyId<'db>, db: &'db dyn HirAnalysisDb) -> Option<&'db TyBase<'db>> {
        let base = ty.base_ty(db);
        match base.data(db) {
            TyData::TyBase(base) => Some(base),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
struct MethodBucket<'db> {
    methods: FxHashMap<Binder<TyId<'db>>, FxHashMap<IdentId<'db>, FuncDef<'db>>>,
}

impl<'db> MethodBucket<'db> {
    fn new() -> Self {
        Self {
            methods: FxHashMap::default(),
        }
    }

    fn probe(
        &self,
        table: &mut UnificationTable<'db>,
        ty: TyId<'db>,
        name: IdentId<'db>,
    ) -> Vec<FuncDef<'db>> {
        let mut methods = vec![];
        for (&cand_ty, funcs) in self.methods.iter() {
            let snapshot = table.snapshot();

            let ty = table.instantiate_to_term(ty);
            let cand_ty = table.instantiate_with_fresh_vars(cand_ty);
            let cand_ty = table.instantiate_to_term(cand_ty);

            if table.unify(cand_ty, ty).is_ok()
                && let Some(func) = funcs.get(&name)
            {
                methods.push(*func)
            }
            table.rollback_to(snapshot);
        }

        methods
    }
}

struct MethodCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    method_table: MethodTable<'db>,
}

impl<'db> MethodCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Self {
        Self {
            db,
            ingot,
            method_table: MethodTable::new(),
        }
    }

    fn collect_impls(&mut self, impls: &[Impl<'db>]) {
        for &impl_ in impls {
            let assumptions = collect_constraints(self.db, impl_.into()).instantiate_identity();

            let ty = match impl_.ty(self.db).to_opt() {
                Some(ty) => lower_hir_ty(self.db, ty, impl_.scope(), assumptions),
                None => TyId::invalid(self.db, InvalidCause::ParseError),
            };

            if ty.has_invalid(self.db) | !ty.is_inherent_impl_allowed(self.db, self.ingot) {
                continue;
            }

            for func in impl_.funcs(self.db) {
                let Some(func) = lower_func(self.db, func) else {
                    continue;
                };

                self.insert(ty, func)
            }
        }
    }

    fn finalize(self) -> MethodTable<'db> {
        self.method_table.finalize()
    }

    fn insert(&mut self, ty: TyId<'db>, func: FuncDef<'db>) {
        let ty = match func.receiver_ty(self.db) {
            Some(ty) => ty.instantiate_identity(),
            None => ty,
        };

        if self
            .method_table
            .probe(self.db, Canonical::new(self.db, ty), func.name(self.db))
            .is_empty()
        {
            self.method_table.insert(self.db, ty, func)
        }
    }
}
