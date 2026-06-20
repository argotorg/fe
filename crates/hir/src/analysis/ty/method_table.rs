use crate::core::hir_def::{HirIngot, IdentId, Impl};
use common::ingot::Ingot;
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    canonical::{Canonical, Solution},
    fold::{TyFoldable, TyFolder},
    ty_def::{TyBase, TyId, strip_derived_adt_layout_args},
    unify::UnificationTable,
    visitor::{TyVisitable, TyVisitor},
};
use crate::analysis::{HirAnalysisDb, ty::ty_def::TyData};
use crate::hir_def::CallableDef;

/// An inherent-method candidate returned by [`probe_method`], carrying the
/// binding the probe proved alongside the definition.
///
/// `bound` is the candidate as matched against the receiver, canonicalized
/// over the probe's receiver query: consumers extract it into their own
/// inference context instead of re-deriving the binder args from the
/// receiver. This keeps the matching rule (which params, which probe key,
/// which normalizations) in exactly one place — the probe.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ProbedMethod<'db> {
    pub def: CallableDef<'db>,
    pub bound: Solution<BoundInherentMethod<'db>>,
}

/// The bound form of an inherent-method candidate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct BoundInherentMethod<'db> {
    /// The candidate's function type with all binder args applied (solved
    /// where the receiver determined them, fresh variables otherwise).
    pub func_ty: TyId<'db>,
    /// The probe key (the method's self-param type, or the impl self type
    /// for associated functions) under the same binder args. Consumers unify
    /// this against their receiver to propagate the solution's bindings into
    /// receiver inference variables.
    pub key_ty: TyId<'db>,
}

impl<'db> TyVisitable<'db> for BoundInherentMethod<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.func_ty.visit_with(visitor);
        self.key_ty.visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for BoundInherentMethod<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            func_ty: self.func_ty.fold_with(db, folder),
            key_ty: self.key_ty.fold_with(db, folder),
        }
    }
}

#[salsa::tracked(return_ref, cycle_fn=collect_methods_cycle_recover, cycle_initial=collect_methods_cycle_initial)]
pub(crate) fn collect_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> MethodTable<'db> {
    let mut collector = MethodCollector::new(db, ingot);

    for (_, external) in ingot.resolved_external_ingots(db).iter() {
        collector.collect_impls(external.all_impls(db));
    }
    collector.collect_impls(ingot.all_impls(db));
    collector.finalize()
}

fn collect_methods_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _ingot: Ingot<'db>,
) -> MethodTable<'db> {
    MethodTable::new()
}

fn collect_methods_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &MethodTable<'db>,
    _count: u32,
    _ingot: Ingot<'db>,
) -> salsa::CycleRecoveryAction<MethodTable<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

#[salsa::tracked(return_ref)]
pub(crate) fn probe_method<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    ty: Canonical<TyId<'db>>,
    name: IdentId<'db>,
) -> Vec<ProbedMethod<'db>> {
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
    ) -> Vec<ProbedMethod<'db>> {
        let mut table = UnificationTable::new(db);
        // The table is fresh, so the extracted receiver vars share keys with
        // the canonical query vars — the precondition for canonicalizing
        // solutions against `ty` below.
        let extracted = ty.extract_identity(&mut table);
        let Some(base) = Self::extract_ty_base(extracted, db) else {
            return vec![];
        };

        if let Some(bucket) = self.buckets.get(base) {
            bucket.probe(ty, &mut table, extracted, name)
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

    fn insert(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>, func: CallableDef<'db>) {
        let Some(base) = Self::extract_ty_base(ty, db) else {
            return;
        };

        let name = func
            .name(db)
            .expect("callables inserted in table have a name");
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
    methods: FxHashMap<Binder<TyId<'db>>, FxHashMap<IdentId<'db>, CallableDef<'db>>>,
}

impl<'db> MethodBucket<'db> {
    fn new() -> Self {
        Self {
            methods: FxHashMap::default(),
        }
    }

    fn probe(
        &self,
        canonical_ty: Canonical<TyId<'db>>,
        table: &mut UnificationTable<'db>,
        ty: TyId<'db>,
        name: IdentId<'db>,
    ) -> Vec<ProbedMethod<'db>> {
        let db = table.db;
        let mut methods = vec![];
        let ty = strip_derived_adt_layout_args(db, ty);
        let ty = saturate_ty_for_method_probe(table, ty);
        for (&cand_key, funcs) in self.methods.iter() {
            let Some(&func) = funcs.get(&name) else {
                continue;
            };
            let snapshot = table.snapshot();

            let ty = table.instantiate_to_term(ty);
            // Apply fresh vars along the candidate's full binder spine, then
            // express the probe key in terms of those same vars (key params
            // index into the callable's arg list), so a successful match
            // yields the bound candidate, not just its identity.
            let mut func_ty = TyId::func(db, func);
            while let Some(prop) = func_ty.applicable_ty(db) {
                let arg = table.new_var_for(prop);
                func_ty = TyId::app(db, func_ty, arg);
            }
            let key_ty = cand_key.instantiate(db, func_ty.generic_args(db));
            let key_ty = strip_derived_adt_layout_args(db, key_ty);
            let key_ty = table.instantiate_to_term(key_ty);

            if table.unify(key_ty, ty).is_ok() {
                let bound = canonical_ty.canonicalize_solution(
                    db,
                    table,
                    BoundInherentMethod { func_ty, key_ty },
                );
                methods.push(ProbedMethod { def: func, bound });
            }
            table.rollback_to(snapshot);
        }

        methods
    }
}

fn saturate_ty_for_method_probe<'db>(
    table: &mut UnificationTable<'db>,
    mut ty: TyId<'db>,
) -> TyId<'db> {
    let db = table.db;
    while !ty.is_star_kind(db) {
        let Some(prop) = ty.applicable_ty(db) else {
            break;
        };

        // Fresh vars, not invalid/hole placeholders: the padding args can be
        // unified into the candidate's binder args during the probe, and the
        // bound candidate is part of the probe's result — a baked invalid
        // type poisons downstream bound checks (goals containing invalids
        // are skipped) and a baked layout hole pins a const arg that the
        // call site would otherwise infer.
        let arg = table.new_var_for(prop);

        ty = TyId::app(db, ty, arg);
    }
    ty
}

struct MethodCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    method_table: MethodTable<'db>,
}

impl<'db> MethodCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, _ingot: Ingot<'db>) -> Self {
        Self {
            db,
            method_table: MethodTable::new(),
        }
    }

    fn collect_impls(&mut self, impls: &[Impl<'db>]) {
        for &impl_ in impls {
            let Some(ty) = impl_.admissible_inherent_impl_ty(self.db) else {
                continue;
            };

            for func in impl_.funcs(self.db) {
                let Some(func) = func.as_callable(self.db) else {
                    continue;
                };

                self.insert(ty, func)
            }
        }
    }

    fn finalize(self) -> MethodTable<'db> {
        self.method_table.finalize()
    }

    fn insert(&mut self, ty: TyId<'db>, func: CallableDef<'db>) {
        let ty = if let Some(receiver) = func.receiver_ty(self.db) {
            let receiver_ty = receiver.instantiate_identity();
            receiver_ty
                .as_capability(self.db)
                .map(|(_, inner)| inner)
                .unwrap_or(receiver_ty)
        } else {
            ty
        };

        if self
            .method_table
            .probe(
                self.db,
                Canonical::new(self.db, ty),
                func.name(self.db).expect("callable has name"),
            )
            .is_empty()
        {
            self.method_table.insert(self.db, ty, func)
        }
    }
}
