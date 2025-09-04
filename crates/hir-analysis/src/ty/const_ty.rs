use hir::hir_def::{Body, Expr, IntegerId, LitKind, Partial};

use super::{
    ty_def::{InvalidCause, TyId, TyParam, TyVar},
    unify::UnificationTable,
};
use crate::{
    HirAnalysisDb,
    ty::ty_def::{Kind, TyBase, TyData, TyVarSort},
};

#[salsa::interned]
#[derive(Debug)]
pub struct ConstTyId<'db> {
    #[return_ref]
    pub(crate) data: ConstTyData<'db>,
}

#[salsa::tracked]
pub(crate) fn evaluate_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    expected_ty: Option<TyId<'db>>,
) -> ConstTyId<'db> {
    let ConstTyData::UnEvaluated(body) = const_ty.data(db) else {
        let const_ty_ty = const_ty.ty(db);
        return match check_const_ty(db, const_ty_ty, expected_ty, &mut UnificationTable::new(db)) {
            Ok(_) => const_ty,
            Err(cause) => {
                let ty = TyId::invalid(db, cause);
                return const_ty.swap_ty(db, ty);
            }
        };
    };

    let Partial::Present(expr) = body.expr(db).data(db, *body) else {
        let data = ConstTyData::Evaluated(
            EvaluatedConstTy::Invalid,
            TyId::invalid(db, InvalidCause::ParseError),
        );
        return ConstTyId::new(db, data);
    };

    let mut table = UnificationTable::new(db);
    let (resolved, ty) = match expr {
        Expr::Lit(LitKind::Bool(b)) => (
            EvaluatedConstTy::LitBool(*b),
            TyId::new(db, TyData::TyBase(TyBase::bool())),
        ),

        Expr::Lit(LitKind::Int(i)) => (
            EvaluatedConstTy::LitInt(*i),
            table.new_var(TyVarSort::Integral, &Kind::Star),
        ),

        _ => {
            return ConstTyId::new(
                db,
                ConstTyData::Evaluated(
                    EvaluatedConstTy::Invalid,
                    TyId::invalid(db, InvalidCause::InvalidConstTyExpr { body: *body }),
                ),
            );
        }
    };

    let data = match check_const_ty(db, ty, expected_ty, &mut table) {
        Ok(ty) => ConstTyData::Evaluated(resolved, ty),
        Err(err) => ConstTyData::Evaluated(resolved, TyId::invalid(db, err)),
    };

    ConstTyId::new(db, data)
}

// FIXME: When we add type inference, we need to use the inference engine to
// check the type of the expression instead of this function.
fn check_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty_ty: TyId<'db>,
    expected_ty: Option<TyId<'db>>,
    table: &mut UnificationTable<'db>,
) -> Result<TyId<'db>, InvalidCause<'db>> {
    if const_ty_ty.has_invalid(db) {
        return Err(InvalidCause::Other);
    }

    let Some(expected_ty) = expected_ty else {
        return Ok(const_ty_ty);
    };

    if table.unify(expected_ty, const_ty_ty).is_ok() {
        Ok(expected_ty)
    } else {
        let invalid = InvalidCause::ConstTyMismatch {
            expected: expected_ty,
            given: const_ty_ty,
        };
        Err(invalid)
    }
}

impl<'db> ConstTyId<'db> {
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match self.data(db) {
            ConstTyData::TyVar(_, ty) => *ty,
            ConstTyData::TyParam(_, ty) => *ty,
            ConstTyData::Evaluated(_, ty) => *ty,
            ConstTyData::UnEvaluated(_) => TyId::invalid(db, InvalidCause::Other),
        }
    }

    pub(super) fn pretty_print(self, db: &dyn HirAnalysisDb) -> String {
        match &self.data(db) {
            ConstTyData::TyVar(var, _) => var.pretty_print(),
            ConstTyData::TyParam(param, ty) => {
                format!("const {}: {}", param.pretty_print(db), ty.pretty_print(db))
            }
            ConstTyData::Evaluated(resolved, _) => resolved.pretty_print(db),
            ConstTyData::UnEvaluated(_) => "<unevaluated>".to_string(),
        }
    }

    pub(super) fn evaluate(
        self,
        db: &'db dyn HirAnalysisDb,
        expected_ty: Option<TyId<'db>>,
    ) -> Self {
        evaluate_const_ty(db, self, expected_ty)
    }

    pub(super) fn from_body(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> Self {
        let data = ConstTyData::UnEvaluated(body);
        Self::new(db, data)
    }

    pub fn from_opt_body(db: &'db dyn HirAnalysisDb, body: Partial<Body<'db>>) -> Self {
        match body {
            Partial::Present(body) => Self::from_body(db, body),
            Partial::Absent => Self::invalid(db, InvalidCause::ParseError),
        }
    }

    pub(super) fn invalid(db: &'db dyn HirAnalysisDb, cause: InvalidCause<'db>) -> Self {
        let resolved = EvaluatedConstTy::Invalid;
        let ty = TyId::invalid(db, cause);
        let data = ConstTyData::Evaluated(resolved, ty);
        Self::new(db, data)
    }

    fn swap_ty(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        let data = match self.data(db) {
            ConstTyData::TyVar(var, _) => ConstTyData::TyVar(var.clone(), ty),
            ConstTyData::TyParam(param, _) => ConstTyData::TyParam(param.clone(), ty),
            ConstTyData::Evaluated(evaluated, _) => ConstTyData::Evaluated(evaluated.clone(), ty),
            ConstTyData::UnEvaluated(_) => {
                return self;
            }
        };

        Self::new(db, data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstTyData<'db> {
    TyVar(TyVar<'db>, TyId<'db>),
    TyParam(TyParam<'db>, TyId<'db>),
    Evaluated(EvaluatedConstTy<'db>, TyId<'db>),
    UnEvaluated(Body<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EvaluatedConstTy<'db> {
    LitInt(IntegerId<'db>),
    LitBool(bool),
    Invalid,
}

impl EvaluatedConstTy<'_> {
    pub fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        match self {
            EvaluatedConstTy::LitInt(val) => {
                format!("{}", val.data(db))
            }
            EvaluatedConstTy::LitBool(val) => format!("{val}"),
            EvaluatedConstTy::Invalid => "<invalid>".to_string(),
        }
    }
}
