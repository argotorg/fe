use crate::{
    hir_def::{EnumVariant, Func, IdentId, scope_graph::ScopeId},
    span::DynLazySpan,
};
use common::ingot::Ingot;

use super::{binder::Binder, ty_def::TyId};
use crate::analysis::HirAnalysisDb;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, salsa::Update)]
pub enum CallableDef<'db> {
    Func(Func<'db>),
    VariantCtor(EnumVariant<'db>),
}

impl<'db> CallableDef<'db> {
    pub fn name_span(self) -> DynLazySpan<'db> {
        match self {
            Self::Func(func) => func.span().name().into(),
            Self::VariantCtor(v) => v.span().name().into(),
        }
    }

    pub fn is_method(self, db: &dyn HirAnalysisDb) -> bool {
        match self {
            Self::Func(func) => func.is_method(db),
            Self::VariantCtor(..) => false,
        }
    }

    pub fn ingot(self, db: &'db dyn HirAnalysisDb) -> Ingot<'db> {
        let top_mod = match self {
            Self::Func(func) => func.top_mod(db),
            Self::VariantCtor(v) => v.enum_.top_mod(db),
        };

        top_mod.ingot(db)
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            Self::Func(func) => func.scope(),
            Self::VariantCtor(v) => ScopeId::Variant(v),
        }
    }

    pub fn param_list_span(self) -> DynLazySpan<'db> {
        match self {
            Self::Func(func) => func.span().params().into(),
            Self::VariantCtor(v) => v.span().tuple_type().into(),
        }
    }

    pub fn param_span(self, idx: usize) -> DynLazySpan<'db> {
        match self {
            Self::Func(func) => func.span().params().param(idx).into(),
            Self::VariantCtor(var) => var.span().tuple_type().elem_ty(idx).into(),
        }
    }

    /// Get the generic parameters for this callable.
    pub fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        match self {
            Self::Func(func) => func.param_set(db).params(db),
            Self::VariantCtor(var) => {
                use super::adt_def::lower_adt;
                let adt = lower_adt(db, var.enum_.into());
                adt.params(db)
            }
        }
    }

    /// Get the name of this callable.
    pub fn name(self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        match self {
            Self::Func(func) => func.name(db).to_opt().unwrap(),
            Self::VariantCtor(var) => {
                IdentId::new(db, var.name(db).unwrap().to_string())
            }
        }
    }

    /// Get the argument types for this callable.
    pub fn arg_tys(self, db: &'db dyn HirAnalysisDb) -> &'db [Binder<TyId<'db>>] {
        match self {
            Self::Func(func) => func.arg_tys(db),
            Self::VariantCtor(_var) => {
                // For now, return empty slice for variant constructors
                // since they need special handling
                &[]
            }
        }
    }

    /// Get the return type for this callable.
    pub fn ret_ty(self, db: &'db dyn HirAnalysisDb) -> Binder<TyId<'db>> {
        match self {
            Self::Func(func) => func.return_ty(db),
            Self::VariantCtor(var) => var.return_ty(db),
        }
    }

    /// Get the offset to explicit parameters position.
    pub fn offset_to_explicit_params_position(self, db: &'db dyn HirAnalysisDb) -> usize {
        match self {
            Self::Func(func) => func.param_set(db).offset_to_explicit_params_position(db),
            Self::VariantCtor(_) => 0, // Variant constructors don't have implicit self parameters
        }
    }

    /// Get the label for a specific parameter.
    pub fn param_label(self, db: &'db dyn HirAnalysisDb, idx: usize) -> Option<IdentId<'db>> {
        match self {
            Self::Func(func) => func.param_label(db, idx),
            Self::VariantCtor(_) => None, // Variant constructors don't have parameter labels
        }
    }

    /// Get the explicit generic parameters (excluding implicit self parameters).
    pub fn explicit_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        match self {
            Self::Func(func) => func.param_set(db).explicit_params(db),
            Self::VariantCtor(var) => {
                use super::adt_def::lower_adt;
                let adt = lower_adt(db, var.enum_.into());
                adt.params(db)
            }
        }
    }

    /// Get the label or name for a specific parameter.
    pub fn param_label_or_name(
        self,
        db: &'db dyn HirAnalysisDb,
        idx: usize,
    ) -> Option<crate::hir_def::FuncParamName<'db>> {
        match self {
            Self::Func(func) => func.param_label_or_name(db, idx),
            Self::VariantCtor(_) => None, // Variant constructors don't have parameter labels/names
        }
    }

    /// Get the receiver type (for methods).
    pub fn receiver_ty(self, db: &'db dyn HirAnalysisDb) -> Option<Binder<TyId<'db>>> {
        match self {
            Self::Func(func) => {
                if func.is_method(db) {
                    func.arg_tys(db).first().copied()
                } else {
                    None
                }
            }
            Self::VariantCtor(_) => None, // Variant constructors are not methods
        }
    }
}
