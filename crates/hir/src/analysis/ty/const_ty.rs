use crate::core::hir_def::{
    Body, CallArg, Const, Expr, ExprId, IdentId, IntegerId, LitKind, Partial, PathId,
};

use super::{
    trait_def::TraitInstId,
    ty_def::{InvalidCause, TyId, TyParam, TyVar},
    ty_lower::{collect_generic_params, lower_generic_arg_list},
    unify::UnificationTable,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    ty::ty_def::{Kind, TyBase, TyData, TyVarSort},
    ty::{trait_def::assoc_const_body_for_trait_inst, trait_resolution::PredicateListId},
};
use crate::hir_def::{CallableDef, Func};
use super::const_expr::{ConstExpr, ConstExprId};

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

    if let Expr::Call(callee, call_args) = &expr {
        return evaluate_const_call_expr(db, body, *callee, call_args, expected_ty);
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

fn evaluate_const_expr_in_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr_id: ExprId,
    expected_ty: Option<TyId<'db>>,
) -> ConstTyId<'db> {
    let invalid = || invalid_const_ty_expr(db, body);
    let Partial::Present(expr) = expr_id.data(db, body) else {
        return ConstTyId::invalid(db, InvalidCause::ParseError);
    };

    match expr {
        Expr::Call(callee, call_args) => {
            evaluate_const_call_expr(db, body, *callee, call_args, expected_ty)
        }
        Expr::Path(path) => {
            let Some(path) = path.to_opt() else {
                return ConstTyId::invalid(db, InvalidCause::ParseError);
            };
            eval_const_path_expr(db, body, path, expected_ty).unwrap_or_else(invalid)
        }
        Expr::Lit(LitKind::Bool(b)) => {
            let mut table = UnificationTable::new(db);
            evaluated_const_ty_checked(
                db,
                &mut table,
                EvaluatedConstTy::LitBool(*b),
                TyId::new(db, TyData::TyBase(TyBase::bool())),
                expected_ty,
            )
        }
        Expr::Lit(LitKind::Int(i)) => {
            let mut table = UnificationTable::new(db);
            let ty = table.new_var(TyVarSort::Integral, &Kind::Star);
            evaluated_const_ty_checked(
                db,
                &mut table,
                EvaluatedConstTy::LitInt(*i),
                ty,
                expected_ty,
            )
        }
        _ => invalid(),
    }
}

fn evaluate_const_call_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    callee: ExprId,
    call_args: &[CallArg<'db>],
    expected_ty: Option<TyId<'db>>,
) -> ConstTyId<'db> {
    let invalid = || invalid_const_ty_expr(db, body);
    let Partial::Present(callee) = callee.data(db, body) else {
        return ConstTyId::invalid(db, InvalidCause::ParseError);
    };

    let Expr::Path(Partial::Present(path)) = callee else {
        return invalid();
    };

    let Some((func, callable_def, generic_args, full_args)) =
        resolve_const_fn_call(db, body, *path)
    else {
        return invalid();
    };

    let expected_arg_tys = callable_def.arg_tys(db);
    if call_args.len() != expected_arg_tys.len() {
        return invalid();
    }

    let Some(value_args) = call_args
        .iter()
        .zip(expected_arg_tys.iter())
        .enumerate()
        .map(|(idx, (arg, expected))| {
            if let Some(given_label) = arg.label
                && let Some(expected_label) = callable_def.param_label(db, idx)
                && !expected_label.is_self(db)
                && given_label != expected_label
            {
                return None;
            }

            let expected = expected.instantiate(db, &full_args);
            let evaluated = evaluate_const_expr_in_body(db, body, arg.expr, Some(expected));
            Some(TyId::const_ty(db, evaluated))
        })
        .collect()
    else {
        return invalid();
    };

    let mut table = UnificationTable::new(db);
    let ret_ty = callable_def.ret_ty(db).instantiate(db, &full_args);

    if func.is_extern(db) {
        let expr_id = ConstExprId::new(
            db,
            ConstExpr::ExternConstFnCall {
                func,
                generic_args,
                args: value_args,
            },
        );

        let ty =
            check_const_ty(db, ret_ty, expected_ty, &mut table).unwrap_or_else(|err| {
                TyId::invalid(db, err)
            });

        return ConstTyId::new(db, ConstTyData::Abstract(expr_id, ty));
    }

    evaluated_const_ty_checked(
        db,
        &mut table,
        EvaluatedConstTy::ConstFnCall {
            func,
            generic_args,
            value_args,
        },
        ret_ty,
        expected_ty,
    )
}

fn invalid_const_ty_expr<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> ConstTyId<'db> {
    ConstTyId::invalid(db, InvalidCause::InvalidConstTyExpr { body })
}

fn evaluated_const_ty_checked<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    resolved: EvaluatedConstTy<'db>,
    ty: TyId<'db>,
    expected_ty: Option<TyId<'db>>,
) -> ConstTyId<'db> {
    let ty =
        check_const_ty(db, ty, expected_ty, table).unwrap_or_else(|err| TyId::invalid(db, err));
    ConstTyId::new(db, ConstTyData::Evaluated(resolved, ty))
}

fn eval_const_path_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    path: PathId<'db>,
    expected_ty: Option<TyId<'db>>,
) -> Option<ConstTyId<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, path, body.scope(), assumptions, true).ok()? {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => match ty.data(db) {
            TyData::ConstTy(const_ty) => Some(const_ty.evaluate(db, expected_ty)),
            _ => None,
        },
        PathRes::Const(const_def, ty) => {
            let body = const_def.body(db).to_opt()?;
            let const_ty = ConstTyId::from_body(db, body, Some(ty), Some(const_def));
            Some(const_ty.evaluate(db, expected_ty.or(Some(ty))))
        }
        PathRes::TraitConst(_recv_ty, inst, name) => {
            Some(const_ty_from_trait_const(db, inst, name)?.evaluate(db, expected_ty))
        }
        _ => None,
    }
}

fn resolve_const_fn_call<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    path: PathId<'db>,
) -> Option<(Func<'db>, CallableDef<'db>, Vec<TyId<'db>>, Vec<TyId<'db>>)> {
    let assumptions = PredicateListId::empty_list(db);
    let PathRes::Func(func_ty) = resolve_path(
        db,
        path.strip_generic_args(db),
        body.scope(),
        assumptions,
        true,
    )
    .ok()?
    else {
        return None;
    };

    let TyData::TyBase(TyBase::Func(callable_def)) = func_ty.base_ty(db).data(db) else {
        return None;
    };
    let CallableDef::Func(func) = *callable_def else {
        return None;
    };
    if !func.is_const(db) {
        return None;
    }

    let param_set = collect_generic_params(db, func.into());
    let provided_explicit =
        lower_generic_arg_list(db, path.generic_args(db), body.scope(), assumptions);
    let explicit_args =
        param_set.complete_explicit_args_with_defaults(db, None, &provided_explicit, assumptions);
    if explicit_args.len() != param_set.explicit_params(db).len() {
        return None;
    }

    let mut full_args = param_set.params(db).to_vec();
    let offset = param_set.offset_to_explicit_params_position(db);
    full_args
        .get_mut(offset..offset + explicit_args.len())?
        .clone_from_slice(&explicit_args);

    Some((func, *callable_def, explicit_args, full_args))
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
    ConstFnCall {
        func: Func<'db>,
        generic_args: Vec<TyId<'db>>,
        value_args: Vec<TyId<'db>>,
    },
    Invalid,
}

impl EvaluatedConstTy<'_> {
    pub fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        match self {
            EvaluatedConstTy::LitInt(val) => {
                format!("{}", val.data(db))
            }
            EvaluatedConstTy::LitBool(val) => format!("{val}"),
            EvaluatedConstTy::ConstFnCall {
                func,
                generic_args,
                value_args,
            } => {
                let name = func
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).as_str())
                    .unwrap_or("<unknown>");

                let generic_args = if generic_args.is_empty() {
                    String::new()
                } else {
                    let generic_args = generic_args
                        .iter()
                        .map(|a| a.pretty_print(db).as_str())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("<{generic_args}>")
                };

                let value_args = value_args
                    .iter()
                    .map(|a| a.pretty_print(db).as_str())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{name}{generic_args}({value_args})")
            }
            EvaluatedConstTy::Invalid => "<invalid>".to_string(),
        }
    }
}
