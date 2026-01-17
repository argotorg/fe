use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, ToPrimitive, Zero};
use rustc_hash::FxHashMap;

use crate::analysis::{
    name_resolution::{PathRes, resolve_path},
    ty::{
        const_expr::{ConstExpr, ConstExprId},
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
        fold::{TyFoldable, TyFolder},
        trait_resolution::PredicateListId,
        ty_check::{ConstRef, LocalBinding, TypedBody, check_func_body},
        ty_def::{InvalidCause, PrimTy, TyBase, TyData, TyId, prim_int_bits},
    },
    HirAnalysisDb,
};
use crate::hir_def::{
    Body, CallableDef, Expr, ExprId, IntegerId, LitKind, Partial, Pat, PatId, PathId, Stmt,
    StmtId,
    expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp},
};

#[derive(Debug, Clone, Copy)]
pub(crate) struct CtfeConfig {
    pub step_limit: usize,
    pub recursion_limit: usize,
}

impl Default for CtfeConfig {
    fn default() -> Self {
        Self {
            step_limit: 10_000,
            recursion_limit: 64,
        }
    }
}

pub(crate) struct CtfeInterpreter<'db> {
    db: &'db dyn HirAnalysisDb,
    config: CtfeConfig,
    steps_left: usize,
    recursion_depth: usize,
}

#[derive(Debug, Clone)]
enum Value<'db> {
    Unit,
    Const(ConstTyId<'db>),
}

#[derive(Debug, Clone)]
enum Outcome<'db> {
    Value(Value<'db>),
    Return(Value<'db>),
}

#[derive(Default)]
struct Env<'db> {
    bindings: FxHashMap<LocalBinding<'db>, Value<'db>>,
}

impl<'db> CtfeInterpreter<'db> {
    pub(crate) fn new(db: &'db dyn HirAnalysisDb, config: CtfeConfig) -> Self {
        Self {
            db,
            steps_left: config.step_limit,
            recursion_depth: 0,
            config,
        }
    }

    pub(crate) fn eval_const_body(
        &mut self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        let root = body.expr(self.db);
        let mut env = Env::default();
        let generic_args = &[];
        let out = self.eval_expr(body, typed_body, generic_args, &mut env, root)?;
        let out = match out {
            Outcome::Return(v) | Outcome::Value(v) => v,
        };
        value_as_const(out, body, root)
    }

    fn tick(&mut self, body: Body<'db>, expr: ExprId) -> Result<(), InvalidCause<'db>> {
        if self.steps_left == 0 {
            return Err(InvalidCause::ConstEvalStepLimitExceeded { body, expr });
        }
        self.steps_left -= 1;
        Ok(())
    }

    fn eval_expr(
        &mut self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        generic_args: &[TyId<'db>],
        env: &mut Env<'db>,
        expr: ExprId,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        self.tick(body, expr)?;

        let Partial::Present(expr_data) = expr.data(self.db, body) else {
            return Err(InvalidCause::ParseError);
        };

        match expr_data {
            Expr::Lit(LitKind::Bool(flag)) => Ok(Outcome::Value(Value::Const(ConstTyId::new(
                self.db,
                ConstTyData::Evaluated(EvaluatedConstTy::LitBool(*flag), TyId::bool(self.db)),
            )))),

            Expr::Lit(LitKind::Int(int_id)) => {
                let ty = typed_body.expr_ty(self.db, expr);
                let value = normalize_int(self.db, ty, int_id.data(self.db).clone(), body, expr)?;
                let value = IntegerId::new(self.db, value);
                Ok(Outcome::Value(Value::Const(ConstTyId::new(
                    self.db,
                    ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), ty),
                ))))
            }

            Expr::Lit(_) => Err(InvalidCause::ConstEvalUnsupported { body, expr }),

            Expr::Path(Partial::Present(path)) => {
                self.eval_path_expr(body, typed_body, generic_args, env, *path, expr)
            }
            Expr::Path(Partial::Absent) => Err(InvalidCause::ParseError),

            Expr::Un(inner, op) => {
                let inner = self.eval_expr(body, typed_body, generic_args, env, *inner)?;
                let inner = match inner {
                    Outcome::Value(v) => v,
                    Outcome::Return(v) => return Ok(Outcome::Return(v)),
                };
                self.eval_unary(body, typed_body, expr, inner, *op)
            }

            Expr::Bin(lhs, rhs, op) => {
                self.eval_binary(body, typed_body, generic_args, env, expr, *lhs, *rhs, *op)
            }

            Expr::If(cond, then, else_) => {
                let cond = self.eval_expr(body, typed_body, generic_args, env, *cond)?;
                let cond = match cond {
                    Outcome::Value(v) => v,
                    Outcome::Return(v) => return Ok(Outcome::Return(v)),
                };
                let cond = value_as_bool(self.db, cond, body, expr)?;
                if cond {
                    self.eval_expr(body, typed_body, generic_args, env, *then)
                } else if let Some(else_) = else_ {
                    self.eval_expr(body, typed_body, generic_args, env, *else_)
                } else {
                    Ok(Outcome::Value(Value::Unit))
                }
            }

            Expr::Block(stmts) => self.eval_block(body, typed_body, generic_args, env, expr, stmts),

            Expr::Call(_, _) => self.eval_call_expr(body, typed_body, generic_args, env, expr),

            _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
        }
    }

    fn eval_block(
        &mut self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        generic_args: &[TyId<'db>],
        env: &mut Env<'db>,
        expr: ExprId,
        stmts: &[StmtId],
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        if stmts.is_empty() {
            return Ok(Outcome::Value(Value::Unit));
        }

        for (idx, stmt) in stmts.iter().enumerate() {
            let is_last = idx + 1 == stmts.len();
            let out = self.eval_stmt(body, typed_body, generic_args, env, *stmt)?;
            match out {
                Outcome::Return(v) => return Ok(Outcome::Return(v)),
                Outcome::Value(v) if is_last => return Ok(Outcome::Value(v)),
                Outcome::Value(_) => {}
            }
        }

        Err(InvalidCause::ConstEvalUnsupported { body, expr })
    }

    fn eval_stmt(
        &mut self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        generic_args: &[TyId<'db>],
        env: &mut Env<'db>,
        stmt: StmtId,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let Partial::Present(stmt_data) = stmt.data(self.db, body) else {
            return Err(InvalidCause::ParseError);
        };

        match stmt_data {
            Stmt::Let(pat, _ty, init) => {
                let Some(init) = init else {
                    return Err(InvalidCause::ConstEvalUnsupported {
                        body,
                        expr: body.expr(self.db),
                    });
                };
                let out = self.eval_expr(body, typed_body, generic_args, env, *init)?;
                let out = match out {
                    Outcome::Value(v) => v,
                    Outcome::Return(v) => return Ok(Outcome::Return(v)),
                };
                self.bind_pat(body, typed_body, env, *pat, out)?;
                Ok(Outcome::Value(Value::Unit))
            }

            Stmt::Expr(expr) => self.eval_expr(body, typed_body, generic_args, env, *expr),

            Stmt::Return(expr) => {
                let out = if let Some(expr) = expr {
                    self.eval_expr(body, typed_body, generic_args, env, *expr)?
                } else {
                    Outcome::Value(Value::Unit)
                };
                let value = match out {
                    Outcome::Value(v) | Outcome::Return(v) => v,
                };
                Ok(Outcome::Return(value))
            }

            _ => Err(InvalidCause::ConstEvalUnsupported {
                body,
                expr: body.expr(self.db),
            }),
        }
    }

    fn bind_pat(
        &mut self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        env: &mut Env<'db>,
        pat: PatId,
        value: Value<'db>,
    ) -> Result<(), InvalidCause<'db>> {
        let Partial::Present(pat_data) = pat.data(self.db, body) else {
            return Err(InvalidCause::ParseError);
        };

        match pat_data {
            Pat::WildCard => Ok(()),
            Pat::Path(..) => {
                let Some(binding) = typed_body.pat_binding(pat) else {
                    return Err(InvalidCause::ConstEvalUnsupported {
                        body,
                        expr: body.expr(self.db),
                    });
                };
                env.bindings.insert(binding, value);
                Ok(())
            }
            _ => Err(InvalidCause::ConstEvalUnsupported {
                body,
                expr: body.expr(self.db),
            }),
        }
    }

    fn eval_path_expr(
        &mut self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        generic_args: &[TyId<'db>],
        env: &mut Env<'db>,
        path: PathId<'db>,
        expr: ExprId,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        if let Some(binding) = typed_body.expr_binding(expr)
            && let Some(value) = env.bindings.get(&binding).cloned()
        {
            return Ok(Outcome::Value(value));
        }

        if let Some(cref) = typed_body.expr_const_ref(expr) {
            let expected_ty = typed_body.expr_ty(self.db, expr);
            let const_ty = self.eval_const_ref(cref, expected_ty)?;
            return Ok(Outcome::Value(Value::Const(const_ty)));
        }

        let assumptions = PredicateListId::empty_list(self.db);
        if let Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) =
            resolve_path(self.db, path, body.scope(), assumptions, true)
        {
            if let TyData::ConstTy(const_ty) = ty.data(self.db)
                && let ConstTyData::TyParam(param, _) = const_ty.data(self.db)
                && let Some(arg) = generic_args.get(param.idx)
                && let TyData::ConstTy(arg_const) = arg.data(self.db)
            {
                let arg_const = arg_const.evaluate(self.db, Some(arg_const.ty(self.db)));
                return Ok(Outcome::Value(Value::Const(arg_const)));
            }

            if let TyData::ConstTy(const_ty) = ty.data(self.db)
                && matches!(const_ty.data(self.db), ConstTyData::TyParam(..))
            {
                return Ok(Outcome::Value(Value::Const(*const_ty)));
            }
        }

        Err(InvalidCause::ConstEvalUnsupported { body, expr })
    }

    fn eval_const_ref(
        &mut self,
        cref: ConstRef<'db>,
        expected_ty: TyId<'db>,
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        let const_ty = match cref {
            ConstRef::Const(const_def) => {
                let body = const_def.body(self.db).to_opt().ok_or(InvalidCause::ParseError)?;
                ConstTyId::from_body(self.db, body, Some(expected_ty), Some(const_def))
            }
            ConstRef::TraitConst { inst, name } => {
                crate::analysis::ty::const_ty::const_ty_from_trait_const(self.db, inst, name)
                    .ok_or(InvalidCause::Other)?
            }
        };

        let evaluated = const_ty.evaluate(self.db, Some(expected_ty));
        evaluated
            .ty(self.db)
            .invalid_cause(self.db)
            .map(Err)
            .unwrap_or(Ok(evaluated))
    }

    fn eval_unary(
        &mut self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        expr: ExprId,
        inner: Value<'db>,
        op: UnOp,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        match op {
            UnOp::Plus => Ok(Outcome::Value(inner)),

            UnOp::Not => {
                let flag = value_as_bool(self.db, inner, body, expr)?;
                Ok(Outcome::Value(Value::Const(ConstTyId::new(
                    self.db,
                    ConstTyData::Evaluated(EvaluatedConstTy::LitBool(!flag), TyId::bool(self.db)),
                ))))
            }

            UnOp::Minus | UnOp::BitNot => {
                let ty = typed_body.expr_ty(self.db, expr);
                let (bits, _) = int_layout(self.db, ty, body, expr)?;
                let v = value_as_int(self.db, inner, body, expr)?;
                let (modulus, mask) = int_modulus_mask(bits);
                let out = match op {
                    UnOp::Minus => (modulus.clone() - (v % &modulus)) & &mask,
                    UnOp::BitNot => &mask ^ v,
                    _ => unreachable!(),
                };
                Ok(Outcome::Value(Value::Const(ConstTyId::new(
                    self.db,
                    ConstTyData::Evaluated(EvaluatedConstTy::LitInt(IntegerId::new(self.db, out)), ty),
                ))))
            }
        }
    }

    fn eval_binary(
        &mut self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        generic_args: &[TyId<'db>],
        env: &mut Env<'db>,
        expr: ExprId,
        lhs_expr: ExprId,
        rhs_expr: ExprId,
        op: BinOp,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        match op {
            BinOp::Logical(logical) => {
                let lhs = self.eval_expr(body, typed_body, generic_args, env, lhs_expr)?;
                let lhs = match lhs {
                    Outcome::Value(v) => v,
                    Outcome::Return(v) => return Ok(Outcome::Return(v)),
                };
                let lhs = value_as_bool(self.db, lhs, body, expr)?;
                match (logical, lhs) {
                    (LogicalBinOp::And, false) => {
                        return Ok(Outcome::Value(Value::Const(ConstTyId::new(
                            self.db,
                            ConstTyData::Evaluated(EvaluatedConstTy::LitBool(false), TyId::bool(self.db)),
                        ))));
                    }
                    (LogicalBinOp::Or, true) => {
                        return Ok(Outcome::Value(Value::Const(ConstTyId::new(
                            self.db,
                            ConstTyData::Evaluated(EvaluatedConstTy::LitBool(true), TyId::bool(self.db)),
                        ))));
                    }
                    _ => {}
                }

                let rhs = self.eval_expr(body, typed_body, generic_args, env, rhs_expr)?;
                let rhs = match rhs {
                    Outcome::Value(v) => v,
                    Outcome::Return(v) => return Ok(Outcome::Return(v)),
                };
                let rhs = value_as_bool(self.db, rhs, body, expr)?;
                let out = match logical {
                    LogicalBinOp::And => lhs && rhs,
                    LogicalBinOp::Or => lhs || rhs,
                };

                Ok(Outcome::Value(Value::Const(ConstTyId::new(
                    self.db,
                    ConstTyData::Evaluated(EvaluatedConstTy::LitBool(out), TyId::bool(self.db)),
                ))))
            }

            BinOp::Comp(comp) => {
                let lhs = self.eval_expr(body, typed_body, generic_args, env, lhs_expr)?;
                let lhs = match lhs {
                    Outcome::Value(v) => v,
                    Outcome::Return(v) => return Ok(Outcome::Return(v)),
                };
                let rhs = self.eval_expr(body, typed_body, generic_args, env, rhs_expr)?;
                let rhs = match rhs {
                    Outcome::Value(v) => v,
                    Outcome::Return(v) => return Ok(Outcome::Return(v)),
                };

                let operand_ty = typed_body.expr_ty(self.db, lhs_expr);
                let out = eval_cmp(self.db, operand_ty, lhs, rhs, body, expr, comp)?;
                Ok(Outcome::Value(Value::Const(out)))
            }

            BinOp::Arith(arith) => {
                let lhs = self.eval_expr(body, typed_body, generic_args, env, lhs_expr)?;
                let lhs = match lhs {
                    Outcome::Value(v) => v,
                    Outcome::Return(v) => return Ok(Outcome::Return(v)),
                };
                let rhs = self.eval_expr(body, typed_body, generic_args, env, rhs_expr)?;
                let rhs = match rhs {
                    Outcome::Value(v) => v,
                    Outcome::Return(v) => return Ok(Outcome::Return(v)),
                };

                let ty = typed_body.expr_ty(self.db, expr);
                let out = self.eval_arith_binop(body, expr, ty, lhs, rhs, arith)?;
                Ok(Outcome::Value(Value::Const(out)))
            }

            BinOp::Index => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
        }
    }

    fn eval_arith_binop(
        &mut self,
        body: Body<'db>,
        expr: ExprId,
        ty: TyId<'db>,
        lhs: Value<'db>,
        rhs: Value<'db>,
        op: ArithBinOp,
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        let (bits, signed) = int_layout(self.db, ty, body, expr)?;
        let lhs_u = value_as_int(self.db, lhs, body, expr)?;
        let rhs_u = value_as_int(self.db, rhs, body, expr)?;
        if matches!(op, ArithBinOp::Div | ArithBinOp::Rem) && rhs_u.is_zero() {
            return Err(InvalidCause::ConstEvalDivisionByZero { body, expr });
        }

        let (modulus, mask) = int_modulus_mask(bits);
        let out = match op {
            ArithBinOp::Add => (lhs_u + rhs_u) & &mask,
            ArithBinOp::Sub => (lhs_u + (&modulus - (rhs_u % &modulus))) & &mask,
            ArithBinOp::Mul => (lhs_u * rhs_u) & &mask,
            ArithBinOp::Pow => lhs_u.modpow(&rhs_u, &modulus),
            ArithBinOp::Div | ArithBinOp::Rem => {
                if signed {
                    let lhs_s = to_signed(bits, &lhs_u);
                    let rhs_s = to_signed(bits, &rhs_u);
                    let out_s = match op {
                        ArithBinOp::Div => lhs_s / rhs_s,
                        ArithBinOp::Rem => lhs_s % rhs_s,
                        _ => unreachable!(),
                    };
                    from_signed(bits, out_s)
                } else {
                    match op {
                        ArithBinOp::Div => lhs_u / rhs_u,
                        ArithBinOp::Rem => lhs_u % rhs_u,
                        _ => unreachable!(),
                    }
                }
            }
            ArithBinOp::LShift | ArithBinOp::RShift => {
                let shift = rhs_u.to_usize().unwrap_or(bits);
                if shift >= bits {
                    if matches!(op, ArithBinOp::RShift)
                        && signed
                        && is_negative(bits, &lhs_u)
                    {
                        mask.clone()
                    } else {
                        BigUint::zero()
                    }
                } else if matches!(op, ArithBinOp::LShift) {
                    (lhs_u << shift) & &mask
                } else if signed {
                    let lhs_s = to_signed(bits, &lhs_u);
                    from_signed(bits, lhs_s >> shift) & &mask
                } else {
                    (lhs_u >> shift) & &mask
                }
            }
            ArithBinOp::BitAnd => lhs_u & rhs_u,
            ArithBinOp::BitOr => lhs_u | rhs_u,
            ArithBinOp::BitXor => lhs_u ^ rhs_u,
        };

        Ok(ConstTyId::new(
            self.db,
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(IntegerId::new(self.db, out)), ty),
        ))
    }

    fn eval_call_expr(
        &mut self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        generic_args: &[TyId<'db>],
        env: &mut Env<'db>,
        expr: ExprId,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let Some(callable) = typed_body.callable_expr(expr) else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        };
        let CallableDef::Func(func) = callable.callable_def else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        };
        if !func.is_const(self.db) {
            return Err(InvalidCause::ConstEvalNonConstCall { body, expr });
        }

        let Partial::Present(Expr::Call(_callee, args)) = expr.data(self.db, body) else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        };

        let mut value_args = Vec::with_capacity(args.len());
        for arg in args {
            let out = self.eval_expr(body, typed_body, generic_args, env, arg.expr)?;
            let out = match out {
                Outcome::Value(v) => v,
                Outcome::Return(v) => return Ok(Outcome::Return(v)),
            };
            let out = value_as_const(out, body, expr)?;
            value_args.push(out);
        }

        if func.is_extern(self.db) {
            let ret_ty = typed_body.expr_ty(self.db, expr);
            let args = value_args
                .iter()
                .copied()
                .map(|v| TyId::const_ty(self.db, v))
                .collect::<Vec<_>>();
            let expr_id = ConstExprId::new(
                self.db,
                ConstExpr::ExternConstFnCall {
                    func,
                    generic_args: callable.generic_args().to_vec(),
                    args,
                },
            );
            let out = ConstTyId::new(self.db, ConstTyData::Abstract(expr_id, ret_ty));
            return Ok(Outcome::Value(Value::Const(out)));
        }

        let out = self.eval_user_const_fn_call(body, expr, func, callable.generic_args(), &value_args)?;
        Ok(Outcome::Value(Value::Const(out)))
    }

    fn eval_user_const_fn_call(
        &mut self,
        call_body: Body<'db>,
        call_expr: ExprId,
        func: crate::hir_def::Func<'db>,
        generic_args: &[TyId<'db>],
        value_args: &[ConstTyId<'db>],
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        if self.recursion_depth >= self.config.recursion_limit {
            return Err(InvalidCause::ConstEvalRecursionLimitExceeded {
                body: call_body,
                expr: call_expr,
            });
        }

        let Some(body) = func.body(self.db) else {
            return Err(InvalidCause::ConstEvalUnsupported {
                body: call_body,
                expr: call_expr,
            });
        };

        let (diags, typed_body) = check_func_body(self.db, func);
        if !diags.is_empty() {
            return Err(InvalidCause::ConstEvalUnsupported {
                body: call_body,
                expr: call_expr,
            });
        }

        let typed_body = instantiate_typed_body(self.db, typed_body.clone(), generic_args);

        let mut env = Env::default();
        for (idx, arg) in value_args.iter().copied().enumerate() {
            let Some(binding) = typed_body.param_binding(idx) else {
                return Err(InvalidCause::ConstEvalUnsupported {
                    body: call_body,
                    expr: call_expr,
                });
            };
            env.bindings.insert(binding, Value::Const(arg));
        }

        self.recursion_depth += 1;
        let root = body.expr(self.db);
        let out = self.eval_expr(body, &typed_body, generic_args, &mut env, root)?;
        self.recursion_depth -= 1;

        let out = match out {
            Outcome::Return(v) | Outcome::Value(v) => v,
        };
        value_as_const(out, body, root)
    }
}

fn instantiate_typed_body<'db>(
    db: &'db dyn HirAnalysisDb,
    typed_body: TypedBody<'db>,
    generic_args: &[TyId<'db>],
) -> TypedBody<'db> {
    struct GenericSubst<'a, 'db> {
        db: &'db dyn HirAnalysisDb,
        generic_args: &'a [TyId<'db>],
    }

    impl<'db> TyFolder<'db> for GenericSubst<'_, 'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            match ty.data(self.db) {
                TyData::TyParam(param) => self
                    .generic_args
                    .get(param.idx)
                    .copied()
                    .unwrap_or(ty),
                TyData::ConstTy(const_ty) => {
                    if let ConstTyData::TyParam(param, _) = const_ty.data(self.db)
                        && let Some(rep) = self.generic_args.get(param.idx).copied()
                    {
                        return rep;
                    }
                    ty.super_fold_with(db, self)
                }
                _ => ty.super_fold_with(db, self),
            }
        }
    }

    let mut subst = GenericSubst { db, generic_args };
    typed_body.fold_with(db, &mut subst)
}

fn value_as_const<'db>(
    value: Value<'db>,
    body: Body<'db>,
    expr: ExprId,
) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
    match value {
        Value::Const(c) => Ok(c),
        Value::Unit => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
    }
}

fn value_as_bool<'db>(
    db: &'db dyn HirAnalysisDb,
    value: Value<'db>,
    body: Body<'db>,
    expr: ExprId,
) -> Result<bool, InvalidCause<'db>> {
    let value = value_as_const(value, body, expr)?;
    match value.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitBool(flag), _) => Ok(*flag),
        _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
    }
}

fn value_as_int<'db>(
    db: &'db dyn HirAnalysisDb,
    value: Value<'db>,
    body: Body<'db>,
    expr: ExprId,
) -> Result<BigUint, InvalidCause<'db>> {
    let value = value_as_const(value, body, expr)?;
    match value.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => Ok(int_id.data(db).clone()),
        _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
    }
}

fn eval_cmp<'db>(
    db: &'db dyn HirAnalysisDb,
    operand_ty: TyId<'db>,
    lhs: Value<'db>,
    rhs: Value<'db>,
    body: Body<'db>,
    expr: ExprId,
    op: CompBinOp,
) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
    let bool_ty = TyId::bool(db);

    if operand_ty.is_bool(db) {
        let lhs = value_as_bool(db, lhs, body, expr)?;
        let rhs = value_as_bool(db, rhs, body, expr)?;
        let out = match op {
            CompBinOp::Eq => lhs == rhs,
            CompBinOp::NotEq => lhs != rhs,
            CompBinOp::Lt => lhs < rhs,
            CompBinOp::LtEq => lhs <= rhs,
            CompBinOp::Gt => lhs > rhs,
            CompBinOp::GtEq => lhs >= rhs,
        };
        return Ok(ConstTyId::new(
            db,
            ConstTyData::Evaluated(EvaluatedConstTy::LitBool(out), bool_ty),
        ));
    }

    let (bits, signed) = int_layout(db, operand_ty, body, expr)?;
    let lhs_u = value_as_int(db, lhs, body, expr)?;
    let rhs_u = value_as_int(db, rhs, body, expr)?;

    let out = if signed {
        let lhs_s = to_signed(bits, &lhs_u);
        let rhs_s = to_signed(bits, &rhs_u);
        match op {
            CompBinOp::Eq => lhs_s == rhs_s,
            CompBinOp::NotEq => lhs_s != rhs_s,
            CompBinOp::Lt => lhs_s < rhs_s,
            CompBinOp::LtEq => lhs_s <= rhs_s,
            CompBinOp::Gt => lhs_s > rhs_s,
            CompBinOp::GtEq => lhs_s >= rhs_s,
        }
    } else {
        match op {
            CompBinOp::Eq => lhs_u == rhs_u,
            CompBinOp::NotEq => lhs_u != rhs_u,
            CompBinOp::Lt => lhs_u < rhs_u,
            CompBinOp::LtEq => lhs_u <= rhs_u,
            CompBinOp::Gt => lhs_u > rhs_u,
            CompBinOp::GtEq => lhs_u >= rhs_u,
        }
    };

    Ok(ConstTyId::new(
        db,
        ConstTyData::Evaluated(EvaluatedConstTy::LitBool(out), bool_ty),
    ))
}

fn int_layout<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    body: Body<'db>,
    expr: ExprId,
) -> Result<(usize, bool), InvalidCause<'db>> {
    let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db) else {
        return Err(InvalidCause::ConstEvalUnsupported { body, expr });
    };
    let bits = prim_int_bits(*prim).ok_or(InvalidCause::ConstEvalUnsupported { body, expr })?;
    let signed = matches!(
        prim,
        PrimTy::I8
            | PrimTy::I16
            | PrimTy::I32
            | PrimTy::I64
            | PrimTy::I128
            | PrimTy::I256
            | PrimTy::Isize
    );
    Ok((bits, signed))
}

fn normalize_int<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    value: BigUint,
    body: Body<'db>,
    expr: ExprId,
) -> Result<BigUint, InvalidCause<'db>> {
    let (bits, _) = int_layout(db, ty, body, expr)?;
    let (_, mask) = int_modulus_mask(bits);
    Ok(value & mask)
}

fn int_modulus_mask(bits: usize) -> (BigUint, BigUint) {
    let modulus = BigUint::one() << bits;
    let mask = &modulus - BigUint::one();
    (modulus, mask)
}

fn is_negative(bits: usize, value: &BigUint) -> bool {
    if bits == 0 {
        return false;
    }
    let sign_bit = BigUint::one() << (bits - 1);
    (value & sign_bit) != BigUint::zero()
}

fn to_signed(bits: usize, value: &BigUint) -> BigInt {
    if bits == 0 {
        return BigInt::zero();
    }
    let modulus = BigInt::from_biguint(Sign::Plus, BigUint::one() << bits);
    let half = BigUint::one() << (bits - 1);
    if *value >= half {
        BigInt::from_biguint(Sign::Plus, value.clone()) - modulus
    } else {
        BigInt::from_biguint(Sign::Plus, value.clone())
    }
}

fn from_signed(bits: usize, value: BigInt) -> BigUint {
    let modulus = BigInt::from_biguint(Sign::Plus, BigUint::one() << bits);
    let v = ((value % &modulus) + &modulus) % &modulus;
    v.to_biguint().expect("mod result should be non-negative")
}
