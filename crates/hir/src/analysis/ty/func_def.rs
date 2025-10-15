use crate::{
    hir_def::{EnumVariant, Func, FuncParamName, IdentId, Partial, scope_graph::ScopeId},
    span::DynLazySpan,
};
use common::ingot::Ingot;

use super::{binder::Binder, ty_def::TyId, ty_lower::GenericParamTypeSet};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        trait_resolution::constraint::collect_func_def_constraints,
        ty_def::InvalidCause,
        ty_lower::{collect_generic_params, lower_hir_ty},
    },
};

/// Lower func to [`FuncDef`]. This function returns `None` iff the function
/// name is `Partial::Absent`.
#[salsa::tracked]
pub fn lower_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    // _assumptions: PredicateListId<'db>,
) -> Option<FuncDef<'db>> {
    let name = func.name(db).to_opt()?;
    let params_set = collect_generic_params(db, func.into());
    let assumptions = collect_func_def_constraints(db, func.into(), true).instantiate_identity();
    let args = match func.params(db) {
        Partial::Present(params) => params
            .data(db)
            .iter()
            .map(|arg| {
                let ty = arg
                    .ty
                    .to_opt()
                    .map(|ty| lower_hir_ty(db, ty, func.scope(), assumptions))
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError));
                Binder::bind(ty)
            })
            .collect(),
        Partial::Absent => vec![],
    };

    // When lowering the return type, we need to use assumptions that include
    // the function's own generic parameter constraints
    let ret_ty = func
        .ret_ty(db)
        .map(|ty| lower_hir_ty(db, ty, func.scope(), assumptions))
        .unwrap_or_else(|| TyId::unit(db));

    Some(FuncDef::new(
        db,
        func.into(),
        name,
        params_set,
        args,
        Binder::bind(ret_ty),
    ))
}

#[salsa::tracked]
#[derive(Debug)]
pub struct FuncDef<'db> {
    pub hir_def: HirFuncDefKind<'db>,

    pub name: IdentId<'db>,

    pub params_set: GenericParamTypeSet<'db>,

    /// Argument types of the function.
    #[return_ref]
    pub arg_tys: Vec<Binder<TyId<'db>>>,

    /// Return types of the function.
    pub ret_ty: Binder<TyId<'db>>,
}

impl<'db> FuncDef<'db> {
    pub fn ingot(self, db: &'db dyn HirAnalysisDb) -> Ingot<'db> {
        self.hir_def(db).ingot(db)
    }

    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> DynLazySpan<'db> {
        self.hir_def(db).name_span()
    }

    pub fn param_list_span(self, db: &'db dyn HirAnalysisDb) -> DynLazySpan<'db> {
        self.hir_def(db).param_list_span()
    }

    pub fn scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.hir_def(db).scope()
    }

    pub fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.params_set(db).params(db)
    }

    pub fn explicit_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.params_set(db).explicit_params(db)
    }

    pub fn receiver_ty(self, db: &'db dyn HirAnalysisDb) -> Option<Binder<TyId<'db>>> {
        self.is_method(db)
            .then(|| self.arg_tys(db).first().copied().unwrap())
    }

    pub fn is_method(self, db: &dyn HirAnalysisDb) -> bool {
        self.hir_def(db).is_method(db)
    }

    pub fn offset_to_explicit_params_position(self, db: &dyn HirAnalysisDb) -> usize {
        self.params_set(db).offset_to_explicit_params_position(db)
    }

    pub fn hir_func_def(self, db: &'db dyn HirAnalysisDb) -> Option<Func<'db>> {
        if let HirFuncDefKind::Func(func) = self.hir_def(db) {
            Some(func)
        } else {
            None
        }
    }

    pub fn param_span(self, db: &'db dyn HirAnalysisDb, idx: usize) -> DynLazySpan<'db> {
        self.hir_def(db).param_span(idx)
    }

    pub fn param_label(self, db: &'db dyn HirAnalysisDb, idx: usize) -> Option<IdentId<'db>> {
        self.hir_func_def(db)?.param_label(db, idx)
    }

    pub fn param_label_or_name(
        self,
        db: &'db dyn HirAnalysisDb,
        idx: usize,
    ) -> Option<FuncParamName<'db>> {
        self.hir_func_def(db)?.param_label_or_name(db, idx)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, salsa::Update)]
pub enum HirFuncDefKind<'db> {
    Func(Func<'db>),
    VariantCtor(EnumVariant<'db>),
}

impl<'db> HirFuncDefKind<'db> {
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
}
