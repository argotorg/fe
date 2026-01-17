use crate::core::hir_def::{Body, Const, Expr, IdentId, IntegerId, LitKind, Partial};

use super::{
    trait_def::TraitInstId,
    ty_def::{InvalidCause, TyId, TyParam, TyVar},
    unify::UnificationTable,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    ty::ty_def::{Kind, TyBase, TyData, TyVarSort},
    ty::{trait_def::assoc_const_body_for_trait_inst, trait_resolution::PredicateListId},
};
use super::const_expr::ConstExprId;

#[salsa::interned]
#[derive(Debug)]
pub struct ConstTyId<'db> {
    #[return_ref]
    pub data: ConstTyData<'db>,
}

#[salsa::tracked]
pub(crate) fn evaluate_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    expected_ty: Option<TyId<'db>>,
) -> ConstTyId<'db> {
    let (body, const_ty_ty, _const_def) = match const_ty.data(db) {
        ConstTyData::UnEvaluated {
            body,
            ty,
            const_def,
        } => (*body, *ty, *const_def),
        _ => {
            let const_ty_ty = const_ty.ty(db);
            return match check_const_ty(
                db,
                const_ty_ty,
                expected_ty,
                &mut UnificationTable::new(db),
            ) {
                Ok(_) => const_ty,
                Err(cause) => {
                    let ty = TyId::invalid(db, cause);
                    return const_ty.swap_ty(db, ty);
                }
            };
        }
    };

    let expected_ty = expected_ty.or(const_ty_ty);

    let Partial::Present(expr) = body.expr(db).data(db, body) else {
        let data = ConstTyData::Evaluated(
            EvaluatedConstTy::Invalid,
            TyId::invalid(db, InvalidCause::ParseError),
        );
        return ConstTyId::new(db, data);
    };

    let expr = expr.clone();

    if let Expr::Path(path) = &expr {
        let Some(path) = path.to_opt() else {
            return ConstTyId::new(
                db,
                ConstTyData::Evaluated(
                    EvaluatedConstTy::Invalid,
                    TyId::invalid(db, InvalidCause::ParseError),
                ),
            );
        };

        let assumptions = PredicateListId::empty_list(db);
        if let Ok(resolved_path) = resolve_path(db, path, body.scope(), assumptions, true) {
            match resolved_path {
                PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                    if let TyData::ConstTy(const_ty) = ty.data(db) {
                        return const_ty.evaluate(db, expected_ty);
                    }
                }
                PathRes::Const(const_def, ty) => {
                    if let Some(body) = const_def.body(db).to_opt() {
                        let const_ty = ConstTyId::from_body(db, body, Some(ty), Some(const_def));
                        let expected = expected_ty.or(Some(ty));
                        return const_ty.evaluate(db, expected);
                    }
                }
                PathRes::TraitConst(_recv_ty, inst, name) => {
                    if let Some(const_ty) = const_ty_from_trait_const(db, inst, name) {
                        return const_ty.evaluate(db, expected_ty);
                    }
                }
                _ => {}
            }
        }

        // If the path failed to resolve but looks like a path to a value
        // (e.g., a trait associated const like `Type::CONST`), keep it
        // unevaluated and assume the expected type if available, avoiding
        // spurious diagnostics here. Downstream checks will validate usage.
        if path.parent(db).is_some() {
            return ConstTyId::from_body(db, body, expected_ty, None);
        }

        return ConstTyId::new(
            db,
            ConstTyData::Evaluated(
                EvaluatedConstTy::Invalid,
                TyId::invalid(db, InvalidCause::InvalidConstTyExpr { body }),
            ),
        );
    }

    let mut table = UnificationTable::new(db);
    let (resolved, ty) = match expr {
        Expr::Lit(LitKind::Bool(b)) => (
            EvaluatedConstTy::LitBool(b),
            TyId::new(db, TyData::TyBase(TyBase::bool())),
        ),

        Expr::Lit(LitKind::Int(i)) => (
            EvaluatedConstTy::LitInt(i),
            table.new_var(TyVarSort::Integral, &Kind::Star),
        ),

        _ => {
            return ConstTyId::new(
                db,
                ConstTyData::Evaluated(
                    EvaluatedConstTy::Invalid,
                    TyId::invalid(db, InvalidCause::InvalidConstTyExpr { body }),
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

pub(super) fn const_ty_from_trait_const<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    name: IdentId<'db>,
) -> Option<ConstTyId<'db>> {
    let body = assoc_const_body_for_trait_inst(db, inst, name).or_else(|| {
        let trait_ = inst.def(db);
        trait_.const_(db, name).and_then(|c| c.default_body(db))
    })?;

    let declared_ty = inst
        .def(db)
        .const_(db, name)
        .and_then(|v| v.ty_binder(db))
        .map(|b| b.instantiate(db, inst.args(db)));

    Some(ConstTyId::from_body(db, body, declared_ty, None))
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
            ConstTyData::Abstract(_, ty) => *ty,
            ConstTyData::UnEvaluated { ty, .. } => {
                ty.unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
            }
        }
    }

    pub(super) fn pretty_print(self, db: &dyn HirAnalysisDb) -> String {
        match &self.data(db) {
            ConstTyData::TyVar(var, _) => var.pretty_print(),
            ConstTyData::TyParam(param, ty) => {
                format!("const {}: {}", param.pretty_print(db), ty.pretty_print(db))
            }
            ConstTyData::Evaluated(resolved, _) => resolved.pretty_print(db),
            ConstTyData::Abstract(expr, _) => expr.pretty_print(db),
            ConstTyData::UnEvaluated {
                body, const_def, ..
            } => {
                if let Some(const_def) = const_def
                    && let Some(name) = const_def.name(db).to_opt()
                {
                    return format!("const {}", name.data(db));
                }

                let expr = body.expr(db);
                let Partial::Present(expr) = expr.data(db, *body) else {
                    return "const value".into();
                };

                match expr {
                    Expr::Lit(LitKind::Bool(value)) => format!("const {}", value),
                    Expr::Lit(LitKind::Int(int)) => format!("const {}", int.data(db)),
                    Expr::Lit(LitKind::String(string)) => format!("const \"{}\"", string.data(db)),
                    Expr::Path(path) if path.is_present() => {
                        format!("const {}", path.unwrap().pretty_print(db))
                    }
                    _ => "const value".into(),
                }
            }
        }
    }

    pub(super) fn evaluate(
        self,
        db: &'db dyn HirAnalysisDb,
        expected_ty: Option<TyId<'db>>,
    ) -> Self {
        evaluate_const_ty(db, self, expected_ty)
    }

    pub(super) fn from_body(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        ty: Option<TyId<'db>>,
        const_def: Option<Const<'db>>,
    ) -> Self {
        let data = ConstTyData::UnEvaluated {
            body,
            ty,
            const_def,
        };
        Self::new(db, data)
    }

    pub fn from_opt_body(db: &'db dyn HirAnalysisDb, body: Partial<Body<'db>>) -> Self {
        match body {
            Partial::Present(body) => Self::from_body(db, body, None, None),
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
            ConstTyData::Abstract(expr, _) => ConstTyData::Abstract(*expr, ty),
            ConstTyData::UnEvaluated {
                body, const_def, ..
            } => ConstTyData::UnEvaluated {
                body: *body,
                ty: Some(ty),
                const_def: *const_def,
            },
        };

        Self::new(db, data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstTyData<'db> {
    TyVar(TyVar<'db>, TyId<'db>),
    TyParam(TyParam<'db>, TyId<'db>),
    Evaluated(EvaluatedConstTy<'db>, TyId<'db>),
    Abstract(ConstExprId<'db>, TyId<'db>),
    UnEvaluated {
        body: Body<'db>,
        ty: Option<TyId<'db>>,
        const_def: Option<Const<'db>>,
    },
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
