use common::indexmap::IndexSet;

use super::{
    adt_def::AdtDef,
    const_ty::{ConstTyData, ConstTyId},
    func_def::FuncDef,
    trait_def::{Implementor, TraitInstId},
    trait_resolution::PredicateListId,
    ty_check::ExprProp,
    ty_def::{AssocTy, InvalidCause, PrimTy, TyBase, TyData, TyFlags, TyId, TyParam, TyVar},
};
use crate::analysis::HirAnalysisDb;

pub trait TyVisitable<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized;
}

pub trait TyVisitor<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb;

    fn visit_ty(&mut self, ty: TyId<'db>) {
        walk_ty(self, ty)
    }

    #[allow(unused_variables)]
    fn visit_var(&mut self, var: &TyVar<'db>) {}

    #[allow(unused_variables)]
    fn visit_param(&mut self, ty_param: &TyParam<'db>) {}

    #[allow(unused_variables)]
    fn visit_assoc_ty(&mut self, assoc_ty: &AssocTy<'db>) {
        walk_assoc_ty(self, assoc_ty);
    }

    #[allow(unused_variables)]
    fn visit_const_param(&mut self, ty_param: &TyParam<'db>, const_ty_ty: TyId<'db>) {}

    fn visit_app(&mut self, abs: TyId<'db>, arg: TyId<'db>) {
        self.visit_ty(abs);
        self.visit_ty(arg);
    }

    #[allow(unused_variables)]
    fn visit_ty_base(&mut self, ty_base: &TyBase<'db>) {
        walk_ty_base(self, ty_base);
    }

    #[allow(unused_variables)]
    fn visit_invalid(&mut self, cause: &InvalidCause<'db>) {}

    #[allow(unused_variables)]
    fn visit_prim(&mut self, prim: &PrimTy) {}

    #[allow(unused_variables)]
    fn visit_adt(&mut self, adt: AdtDef<'db>) {}

    #[allow(unused_variables)]
    fn visit_func(&mut self, func: FuncDef<'db>) {}

    #[allow(unused_variables)]
    fn visit_const_ty(&mut self, const_ty: &ConstTyId<'db>) {
        walk_const_ty(self, const_ty)
    }
}

pub fn walk_ty<'db, V>(visitor: &mut V, ty: TyId<'db>)
where
    V: TyVisitor<'db> + ?Sized,
{
    match ty.data(visitor.db()) {
        TyData::TyVar(var) => visitor.visit_var(var),
        TyData::TyParam(param) => visitor.visit_param(param),
        TyData::AssocTy(assoc_ty) => visitor.visit_assoc_ty(assoc_ty),
        TyData::QualifiedTy(trait_inst) => {
            visitor.visit_ty(trait_inst.self_ty(visitor.db()));
            trait_inst.visit_with(visitor);
        }
        TyData::TyApp(abs, arg) => visitor.visit_app(*abs, *arg),
        TyData::TyBase(ty_con) => visitor.visit_ty_base(ty_con),
        TyData::ConstTy(const_ty) => visitor.visit_const_ty(const_ty),
        TyData::Never => {}
        TyData::Invalid(cause) => visitor.visit_invalid(cause),
    }
}

pub fn walk_ty_base<'db, V>(visitor: &mut V, ty_con: &TyBase<'db>)
where
    V: TyVisitor<'db> + ?Sized,
{
    match ty_con {
        TyBase::Prim(prim) => visitor.visit_prim(prim),
        TyBase::Adt(adt) => visitor.visit_adt(*adt),
        TyBase::Func(func) => visitor.visit_func(*func),
    }
}

pub fn walk_const_ty<'db, V>(visitor: &mut V, const_ty: &ConstTyId<'db>)
where
    V: TyVisitor<'db> + ?Sized,
{
    let db = visitor.db();
    visitor.visit_ty(const_ty.ty(db));
    match &const_ty.data(db) {
        ConstTyData::TyVar(var, _) => visitor.visit_var(var),
        ConstTyData::TyParam(param, ty) => visitor.visit_const_param(param, *ty),
        ConstTyData::Evaluated(..) | ConstTyData::UnEvaluated { .. } => {}
    }
}

pub fn walk_assoc_ty<'db, V>(visitor: &mut V, assoc_ty: &AssocTy<'db>)
where
    V: TyVisitor<'db> + ?Sized,
{
    assoc_ty.trait_.visit_with(visitor);
}

impl<'db> TyVisitable<'db> for TyId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        visitor.visit_ty(*self)
    }
}

impl<'db, T> TyVisitable<'db> for Vec<T>
where
    T: TyVisitable<'db>,
{
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.iter().for_each(|ty| ty.visit_with(visitor))
    }
}

impl<'db, T> TyVisitable<'db> for &[T]
where
    T: TyVisitable<'db>,
{
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.iter().for_each(|ty| ty.visit_with(visitor))
    }
}

impl<'db, T> TyVisitable<'db> for IndexSet<T>
where
    T: TyVisitable<'db>,
{
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.iter().for_each(|ty| ty.visit_with(visitor))
    }
}

impl<'db> TyVisitable<'db> for TraitInstId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        let db = visitor.db();
        self.args(db).visit_with(visitor);
        for (_, ty) in self.assoc_type_bindings(db) {
            ty.visit_with(visitor);
        }
    }
}

impl<'db> TyVisitable<'db> for Implementor<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        let db = visitor.db();
        self.params(db).visit_with(visitor);
    }
}

impl<'db> TyVisitable<'db> for PredicateListId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.list(visitor.db()).visit_with(visitor)
    }
}

impl<'db> TyVisitable<'db> for ExprProp<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.ty.visit_with(visitor)
    }
}

pub fn collect_flags<'db, V: TyVisitable<'db>>(db: &'db dyn HirAnalysisDb, v: V) -> TyFlags {
    struct Collector<'db> {
        db: &'db dyn HirAnalysisDb,
        flags: TyFlags,
    }
    impl<'db> TyVisitor<'db> for Collector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId) {
            let ty_flags = ty.flags(self.db);
            self.flags = self.flags.union(ty_flags);
        }
    }

    let mut collector = Collector {
        db,
        flags: TyFlags::empty(),
    };
    v.visit_with(&mut collector);

    collector.flags
}
