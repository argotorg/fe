#![allow(non_snake_case)]
#![deprecated(
    note = "Temporary syntactic shim; migrate callers to hir_def::semantic and remove this module."
)]
//! Temporary syntactic HIR shim
//!
//! Centralizes public wrappers for raw syntactic access that is being phased
//! out in favor of `hir_def::semantic` methods. Intended for the visitor and
//! other in-progress areas only. Please do not add new dependencies on this
//! module; migrate to semantic methods instead.

use crate::HirDb;
use crate::hir_def::item::{AssocTyDef, Const, FieldDef, Func, Impl, ImplTrait, TypeAlias};
use crate::hir_def::semantic::FieldView;
use crate::hir_def::{Partial, TypeId};

impl<'db> Func<'db> {
    pub fn ret_type_ref___tmp(self, db: &'db dyn HirDb) -> Option<TypeId<'db>> {
        self.ret_type_ref(db)
    }
}

impl<'db> TypeAlias<'db> {
    pub fn type_ref___tmp(self, db: &'db dyn HirDb) -> Partial<TypeId<'db>> {
        self.type_ref(db)
    }
}

impl<'db> Impl<'db> {
    pub fn type_ref___tmp(self, db: &'db dyn HirDb) -> Partial<TypeId<'db>> {
        self.type_ref(db)
    }
}

impl<'db> ImplTrait<'db> {
    pub fn type_ref___tmp(self, db: &'db dyn HirDb) -> Partial<TypeId<'db>> {
        self.type_ref(db)
    }
}

impl<'db> Const<'db> {
    pub fn type_ref___tmp(self, db: &'db dyn HirDb) -> Partial<TypeId<'db>> {
        self.type_ref(db)
    }
}

impl<'db> FieldDef<'db> {
    pub fn field_type_ref___tmp(&self) -> Partial<TypeId<'db>> {
        self.type_ref
    }
}

impl<'db> FieldView<'db> {
    pub fn type_ref___tmp(self, db: &'db dyn HirDb) -> Partial<TypeId<'db>> {
        self.hir_type_ref(db)
    }
}

impl<'db> AssocTyDef<'db> {
    pub fn type_ref___tmp(&self) -> Partial<TypeId<'db>> {
        self.type_ref
    }
}
