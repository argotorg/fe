use crate::hir_def::{EnumVariant, Func, FuncParamName, IdentId, scope_graph::ScopeId};
use crate::span::DynLazySpan;
use common::ingot::Ingot;
use salsa::Update;

use super::{adt_def::AdtRef, binder::Binder, ty_def::TyId, ty_lower::collect_generic_params};
use crate::analysis::HirAnalysisDb;

/// Callable view over either a function or an enum variant constructor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, Update)]
pub enum CallableDef<'db> {
    Func(Func<'db>),
    VariantCtor(EnumVariant<'db>),
}

/// Lower a callable from HIR; returns `None` if the function name is absent.
pub fn lower_func<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> Option<CallableDef<'db>> {
    func.name(db).to_opt()?;
    Some(CallableDef::Func(func))
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

    pub fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        match self {
            Self::Func(func) => collect_generic_params(db, func.into()).params(db),
            Self::VariantCtor(var) => {
                let adt = var.enum_.as_adt(db);
                adt.params(db)
            }
        }
    }

    pub fn explicit_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        match self {
            Self::Func(func) => collect_generic_params(db, func.into()).explicit_params(db),
            Self::VariantCtor(var) => {
                let adt = var.enum_.as_adt(db);
                adt.params(db)
            }
        }
    }

    pub fn offset_to_explicit_params_position(self, db: &'db dyn HirAnalysisDb) -> usize {
        match self {
            Self::Func(func) => {
                collect_generic_params(db, func.into()).offset_to_explicit_params_position(db)
            }
            Self::VariantCtor(_) => 0, // Variant constructors don't have implicit self parameters
        }
    }

    /// Callable name (if present). Variant ctors may be absent when the name is elided.
    pub fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        match self {
            Self::Func(func) => func.name(db).to_opt(),
            Self::VariantCtor(var) => var.ident(db),
        }
    }

    pub fn param_label(self, db: &'db dyn HirAnalysisDb, idx: usize) -> Option<IdentId<'db>> {
        match self {
            Self::Func(func) => func.param_label(db, idx),
            Self::VariantCtor(_) => None,
        }
    }

    pub fn param_label_or_name(
        self,
        db: &'db dyn HirAnalysisDb,
        idx: usize,
    ) -> Option<FuncParamName<'db>> {
        match self {
            Self::Func(func) => func.param_label_or_name(db, idx),
            Self::VariantCtor(_) => None,
        }
    }

    pub fn arg_tys(self, db: &'db dyn HirAnalysisDb) -> Vec<Binder<TyId<'db>>> {
        match self {
            Self::Func(func) => func.arg_tys(db),
            Self::VariantCtor(var) => {
                let adt = var.enum_.as_adt(db);
                let field = &adt.fields(db)[var.idx as usize];
                field.iter_types(db).collect()
            }
        }
    }

    pub fn ret_ty(self, db: &'db dyn HirAnalysisDb) -> Binder<TyId<'db>> {
        match self {
            Self::Func(func) => Binder::bind(func.return_ty(db)),
            Self::VariantCtor(var) => {
                let adt = var.enum_.as_adt(db);
                let mut ty = TyId::adt(db, adt);
                for &param in adt.params(db) {
                    ty = TyId::app(db, ty, param);
                }
                Binder::bind(ty)
            }
        }
    }

    pub fn receiver_ty(self, db: &'db dyn HirAnalysisDb) -> Option<Binder<TyId<'db>>> {
        match self {
            Self::Func(func) if func.is_method(db) => func.arg_tys(db).into_iter().next(),
            _ => None,
        }
    }
}
