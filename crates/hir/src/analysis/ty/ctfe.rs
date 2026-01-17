use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, ToPrimitive, Zero};
use rustc_hash::FxHashMap;

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    ty::{
        const_expr::{ConstExpr, ConstExprId},
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
        fold::{TyFoldable, TyFolder},
        trait_resolution::PredicateListId,
        ty_check::{
            ConstRef, LocalBinding, RecordLike, TypedBody, check_anon_const_body, check_func_body,
        },
        ty_def::{InvalidCause, PrimTy, TyBase, TyData, TyId, prim_int_bits},
    },
};
use crate::hir_def::{
    Body, CallableDef, Expr, ExprId, Field, IntegerId, LitKind, Partial, Pat, PatId, PathId, Stmt,
    StmtId, VariantKind,
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
    frames: Vec<Frame<'db>>,
}

#[derive(Debug, Clone)]
enum Value<'db> {
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

struct Frame<'db> {
    body: Body<'db>,
    typed_body: TypedBody<'db>,
    generic_args: Vec<TyId<'db>>,
    env: Env<'db>,
}

impl<'db> CtfeInterpreter<'db> {
    pub(crate) fn new(db: &'db dyn HirAnalysisDb, config: CtfeConfig) -> Self {
        Self {
            db,
            steps_left: config.step_limit,
            recursion_depth: 0,
            config,
            frames: Vec::new(),
        }
    }

    fn frame(&self) -> &Frame<'db> {
        self.frames.last().expect("ctfe frame missing")
    }

    fn frame_mut(&mut self) -> &mut Frame<'db> {
        self.frames.last_mut().expect("ctfe frame missing")
    }

    fn body(&self) -> Body<'db> {
        self.frame().body
    }

    fn typed_body(&self) -> &TypedBody<'db> {
        &self.frame().typed_body
    }

    fn generic_args(&self) -> &[TyId<'db>] {
        &self.frame().generic_args
    }

    fn env(&self) -> &Env<'db> {
        &self.frame().env
    }

    fn env_mut(&mut self) -> &mut Env<'db> {
        &mut self.frame_mut().env
    }

    fn with_frame<T>(
        &mut self,
        body: Body<'db>,
        typed_body: TypedBody<'db>,
        generic_args: Vec<TyId<'db>>,
        env: Env<'db>,
        f: impl FnOnce(&mut Self) -> Result<T, InvalidCause<'db>>,
    ) -> Result<T, InvalidCause<'db>> {
        self.frames.push(Frame {
            body,
            typed_body,
            generic_args,
            env,
        });
        let out = f(self);
        self.frames.pop();
        out
    }

    pub(crate) fn eval_const_body(
        &mut self,
        body: Body<'db>,
        typed_body: TypedBody<'db>,
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        self.with_frame(body, typed_body, Vec::new(), Env::default(), |this| {
            let root = body.expr(this.db);
            let out = this.eval_expr(root)?;
            Ok(value_as_const(match out {
                Outcome::Return(v) | Outcome::Value(v) => v,
            }))
        })
    }

    fn tick(&mut self, expr: ExprId) -> Result<(), InvalidCause<'db>> {
        if self.steps_left == 0 {
            return Err(InvalidCause::ConstEvalStepLimitExceeded {
                body: self.body(),
                expr,
            });
        }
        self.steps_left -= 1;
        Ok(())
    }

    fn eval_expr(&mut self, expr: ExprId) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let body = self.body();
        self.tick(expr)?;

        let Partial::Present(expr_data) = expr.data(self.db, body) else {
            return Err(InvalidCause::ParseError);
        };

        match expr_data {
            Expr::Lit(LitKind::Bool(flag)) => Ok(Outcome::Value(Value::Const(ConstTyId::new(
                self.db,
                ConstTyData::Evaluated(EvaluatedConstTy::LitBool(*flag), TyId::bool(self.db)),
            )))),

            Expr::Lit(LitKind::Int(int_id)) => {
                let ty = self.typed_body().expr_ty(self.db, expr);
                let value = normalize_int(self.db, ty, int_id.data(self.db).clone(), body, expr)?;
                let value = IntegerId::new(self.db, value);
                Ok(Outcome::Value(Value::Const(ConstTyId::new(
                    self.db,
                    ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), ty),
                ))))
            }

            Expr::Lit(_) => Err(InvalidCause::ConstEvalUnsupported { body, expr }),

            Expr::Path(Partial::Present(path)) => self.eval_path_expr(*path, expr),
            Expr::Path(Partial::Absent) => Err(InvalidCause::ParseError),

            Expr::Un(inner, op) => {
                let inner = match self.eval_expr(*inner)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
                };
                self.eval_unary(expr, inner, *op)
            }

            Expr::Bin(lhs, rhs, op) => self.eval_binary(expr, *lhs, *rhs, *op),

            Expr::If(cond, then, else_) => {
                let cond = match self.eval_expr(*cond)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
                };
                let cond = value_as_bool(self.db, cond, body, expr)?;
                if cond {
                    self.eval_expr(*then)
                } else if let Some(else_) = else_ {
                    self.eval_expr(*else_)
                } else {
                    Ok(Outcome::Value(Value::Const(unit_const(self.db))))
                }
            }

            Expr::Block(stmts) => self.eval_block(expr, stmts),

            Expr::Call(_, _) => self.eval_call_expr(expr),

            Expr::Tuple(elems) => self.eval_tuple(expr, elems),

            Expr::Array(elems) => self.eval_array(expr, elems),

            Expr::ArrayRep(elem, len) => self.eval_array_rep(expr, *elem, len),

            Expr::RecordInit(path, fields) => self.eval_record_init(expr, path, fields),

            Expr::Field(lhs, field) => self.eval_field(expr, *lhs, field),

            _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
        }
    }

    fn eval_block(
        &mut self,
        expr: ExprId,
        stmts: &[StmtId],
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let body = self.body();
        if stmts.is_empty() {
            return Ok(Outcome::Value(Value::Const(unit_const(self.db))));
        }

        for (idx, stmt) in stmts.iter().enumerate() {
            let is_last = idx + 1 == stmts.len();
            let out = self.eval_stmt(*stmt)?;
            match out {
                Outcome::Return(v) => return Ok(Outcome::Return(v)),
                Outcome::Value(v) if is_last => return Ok(Outcome::Value(v)),
                Outcome::Value(_) => {}
            }
        }

        Err(InvalidCause::ConstEvalUnsupported { body, expr })
    }

    fn eval_stmt(&mut self, stmt: StmtId) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let body = self.body();
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
                let out = match self.eval_expr(*init)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
                };
                self.bind_pat(*pat, out)?;
                Ok(Outcome::Value(Value::Const(unit_const(self.db))))
            }

            Stmt::Expr(expr) => self.eval_expr(*expr),

            Stmt::Return(expr) => {
                let value = expr.map(|expr| self.eval_expr(expr)).transpose()?.map_or(
                    Value::Const(unit_const(self.db)),
                    |out| match out {
                        Outcome::Value(v) | Outcome::Return(v) => v,
                    },
                );
                Ok(Outcome::Return(value))
            }

            _ => Err(InvalidCause::ConstEvalUnsupported {
                body,
                expr: body.expr(self.db),
            }),
        }
    }

    fn bind_pat(&mut self, pat: PatId, value: Value<'db>) -> Result<(), InvalidCause<'db>> {
        let body = self.body();
        let Partial::Present(pat_data) = pat.data(self.db, body) else {
            return Err(InvalidCause::ParseError);
        };

        match pat_data {
            Pat::WildCard => Ok(()),
            Pat::Path(..) => {
                let Some(binding) = self.typed_body().pat_binding(pat) else {
                    return Err(InvalidCause::ConstEvalUnsupported {
                        body,
                        expr: body.expr(self.db),
                    });
                };
                self.env_mut().bindings.insert(binding, value);
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
        path: PathId<'db>,
        expr: ExprId,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let body = self.body();
        if let Some(binding) = self.typed_body().expr_binding(expr)
            && let Some(value) = self.env().bindings.get(&binding).cloned()
        {
            return Ok(Outcome::Value(value));
        }

        if let Some(cref) = self.typed_body().expr_const_ref(expr) {
            let expected_ty = self.typed_body().expr_ty(self.db, expr);
            let const_ty = self.eval_const_ref(cref, expected_ty)?;
            return Ok(Outcome::Value(Value::Const(const_ty)));
        }

        let assumptions = PredicateListId::empty_list(self.db);
        if let Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) =
            resolve_path(self.db, path, body.scope(), assumptions, true)
        {
            if let TyData::ConstTy(const_ty) = ty.data(self.db)
                && let ConstTyData::TyParam(param, _) = const_ty.data(self.db)
                && let Some(arg) = self.generic_args().get(param.idx)
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
                let body = const_def
                    .body(self.db)
                    .to_opt()
                    .ok_or(InvalidCause::ParseError)?;
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
        expr: ExprId,
        inner: Value<'db>,
        op: UnOp,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let body = self.body();
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
                let ty = self.typed_body().expr_ty(self.db, expr);
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
                    ConstTyData::Evaluated(
                        EvaluatedConstTy::LitInt(IntegerId::new(self.db, out)),
                        ty,
                    ),
                ))))
            }
        }
    }

    fn eval_binary(
        &mut self,
        expr: ExprId,
        lhs_expr: ExprId,
        rhs_expr: ExprId,
        op: BinOp,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let body = self.body();
        match op {
            BinOp::Logical(logical) => {
                let lhs = match self.eval_expr(lhs_expr)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
                };
                let lhs = value_as_bool(self.db, lhs, body, expr)?;
                match (logical, lhs) {
                    (LogicalBinOp::And, false) => {
                        return Ok(Outcome::Value(Value::Const(ConstTyId::new(
                            self.db,
                            ConstTyData::Evaluated(
                                EvaluatedConstTy::LitBool(false),
                                TyId::bool(self.db),
                            ),
                        ))));
                    }
                    (LogicalBinOp::Or, true) => {
                        return Ok(Outcome::Value(Value::Const(ConstTyId::new(
                            self.db,
                            ConstTyData::Evaluated(
                                EvaluatedConstTy::LitBool(true),
                                TyId::bool(self.db),
                            ),
                        ))));
                    }
                    _ => {}
                }

                let rhs = match self.eval_expr(rhs_expr)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
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
                let lhs = match self.eval_expr(lhs_expr)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
                };
                let rhs = match self.eval_expr(rhs_expr)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
                };

                let operand_ty = self.typed_body().expr_ty(self.db, lhs_expr);
                let out = eval_cmp(self.db, operand_ty, lhs, rhs, body, expr, comp)?;
                Ok(Outcome::Value(Value::Const(out)))
            }

            BinOp::Arith(arith) => {
                let lhs = match self.eval_expr(lhs_expr)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
                };
                let rhs = match self.eval_expr(rhs_expr)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
                };

                let ty = self.typed_body().expr_ty(self.db, expr);
                let out = self.eval_arith_binop(expr, ty, lhs, rhs, arith)?;
                Ok(Outcome::Value(Value::Const(out)))
            }

            BinOp::Index => {
                let lhs = match self.eval_expr(lhs_expr)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
                };
                let rhs = match self.eval_expr(rhs_expr)? {
                    Outcome::Value(v) => v,
                    out => return Ok(out),
                };

                let idx = value_as_int(self.db, rhs, body, expr)?;
                let Some(idx) = idx.to_usize() else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                };

                let lhs_const = value_as_const(lhs);
                match lhs_const.data(self.db) {
                    ConstTyData::Evaluated(EvaluatedConstTy::Array(elems), _) => {
                        let Some(elem) = elems.get(idx).copied() else {
                            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                        };
                        let TyData::ConstTy(const_ty) = elem.data(self.db) else {
                            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                        };
                        Ok(Outcome::Value(Value::Const(*const_ty)))
                    }
                    _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
                }
            }
        }
    }

    fn eval_arith_binop(
        &mut self,
        expr: ExprId,
        ty: TyId<'db>,
        lhs: Value<'db>,
        rhs: Value<'db>,
        op: ArithBinOp,
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        let body = self.body();
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
                    if matches!(op, ArithBinOp::RShift) && signed && is_negative(bits, &lhs_u) {
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

    fn eval_tuple(
        &mut self,
        expr: ExprId,
        elems: &[ExprId],
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let mut values = Vec::with_capacity(elems.len());
        for &elem_expr in elems {
            let out = match self.eval_expr(elem_expr)? {
                Outcome::Value(v) => v,
                out => return Ok(out),
            };
            let const_ty = value_as_const(out);
            values.push(TyId::const_ty(self.db, const_ty));
        }

        let ty = self.typed_body().expr_ty(self.db, expr);
        let const_ty = ConstTyId::new(
            self.db,
            ConstTyData::Evaluated(EvaluatedConstTy::Tuple(values), ty),
        );
        Ok(Outcome::Value(Value::Const(const_ty)))
    }

    fn eval_array(
        &mut self,
        expr: ExprId,
        elems: &[ExprId],
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let mut values = Vec::with_capacity(elems.len());
        for &elem_expr in elems {
            let out = match self.eval_expr(elem_expr)? {
                Outcome::Value(v) => v,
                out => return Ok(out),
            };
            let const_ty = value_as_const(out);
            values.push(TyId::const_ty(self.db, const_ty));
        }

        let ty = self.typed_body().expr_ty(self.db, expr);
        let const_ty = ConstTyId::new(
            self.db,
            ConstTyData::Evaluated(EvaluatedConstTy::Array(values), ty),
        );
        Ok(Outcome::Value(Value::Const(const_ty)))
    }

    fn eval_array_rep(
        &mut self,
        expr: ExprId,
        elem_expr: ExprId,
        len: &Partial<Body<'db>>,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let body = self.body();
        let Some(len_body) = len.to_opt() else {
            return Err(InvalidCause::ParseError);
        };

        let expected_len_ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
        let typed_len_body = check_anon_const_body(self.db, len_body, expected_len_ty)
            .1
            .clone();

        let len_const = self.eval_const_body(len_body, typed_len_body)?;
        let ConstTyData::Evaluated(EvaluatedConstTy::LitInt(len), _) = len_const.data(self.db)
        else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        };
        let Some(len) = len.data(self.db).to_usize() else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        };

        let out = match self.eval_expr(elem_expr)? {
            Outcome::Value(v) => v,
            out => return Ok(out),
        };
        let elem_const = value_as_const(out);
        let elem_const = TyId::const_ty(self.db, elem_const);

        let values = std::iter::repeat_n(elem_const, len).collect::<Vec<_>>();
        let ty = self.typed_body().expr_ty(self.db, expr);
        let const_ty = ConstTyId::new(
            self.db,
            ConstTyData::Evaluated(EvaluatedConstTy::Array(values), ty),
        );
        Ok(Outcome::Value(Value::Const(const_ty)))
    }

    fn eval_record_init(
        &mut self,
        expr: ExprId,
        path: &Partial<PathId<'db>>,
        fields: &[Field<'db>],
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let body = self.body();
        let Partial::Present(path) = path else {
            return Err(InvalidCause::ParseError);
        };

        let assumptions = PredicateListId::empty_list(self.db);
        let resolved = resolve_path(self.db, *path, body.scope(), assumptions, true)
            .map_err(|_| InvalidCause::ConstEvalUnsupported { body, expr })?;

        let record_like = match resolved {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => RecordLike::from_ty(ty),
            PathRes::EnumVariant(variant) => RecordLike::from_variant(variant),
            _ => return Err(InvalidCause::ConstEvalUnsupported { body, expr }),
        };

        if !record_like.is_record(self.db) {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        }

        let field_count = match &record_like {
            RecordLike::Type(ty) => ty.field_count(self.db),
            RecordLike::EnumVariant(variant) => match variant.kind(self.db) {
                VariantKind::Record(fields) => fields.data(self.db).len(),
                _ => return Err(InvalidCause::ConstEvalUnsupported { body, expr }),
            },
        };

        let mut values = vec![None; field_count];
        for field in fields {
            let Some(label) = field.label_eagerly(self.db, body) else {
                return Err(InvalidCause::ConstEvalUnsupported { body, expr });
            };
            let Some(idx) = record_like.record_field_idx(self.db, label) else {
                return Err(InvalidCause::ConstEvalUnsupported { body, expr });
            };
            let out = match self.eval_expr(field.expr)? {
                Outcome::Value(v) => v,
                out => return Ok(out),
            };
            let const_ty = value_as_const(out);
            values[idx] = Some(TyId::const_ty(self.db, const_ty));
        }

        if values.iter().any(|v| v.is_none()) {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        }

        let values = values.into_iter().flatten().collect::<Vec<_>>();
        let ty = self.typed_body().expr_ty(self.db, expr);
        let const_ty = ConstTyId::new(
            self.db,
            ConstTyData::Evaluated(EvaluatedConstTy::Record(values), ty),
        );
        Ok(Outcome::Value(Value::Const(const_ty)))
    }

    fn eval_field(
        &mut self,
        expr: ExprId,
        lhs_expr: ExprId,
        field: &Partial<crate::hir_def::FieldIndex<'db>>,
    ) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let body = self.body();
        let Some(field) = field.to_opt() else {
            return Err(InvalidCause::ParseError);
        };

        let out = match self.eval_expr(lhs_expr)? {
            Outcome::Value(v) => v,
            out => return Ok(out),
        };
        let lhs_const = value_as_const(out);

        match (lhs_const.data(self.db), field) {
            (
                ConstTyData::Evaluated(EvaluatedConstTy::Tuple(elems), _),
                crate::hir_def::FieldIndex::Index(index),
            ) => {
                let Some(index) = index.data(self.db).to_usize() else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                };
                let Some(elem) = elems.get(index).copied() else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                };
                let TyData::ConstTy(const_ty) = elem.data(self.db) else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                };
                Ok(Outcome::Value(Value::Const(*const_ty)))
            }

            (
                ConstTyData::Evaluated(EvaluatedConstTy::Record(fields), _),
                crate::hir_def::FieldIndex::Ident(name),
            ) => {
                let lhs_ty = self.typed_body().expr_ty(self.db, lhs_expr);
                let record_like = RecordLike::from_ty(lhs_ty);
                let Some(idx) = record_like.record_field_idx(self.db, name) else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                };
                let Some(field) = fields.get(idx).copied() else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                };
                let TyData::ConstTy(const_ty) = field.data(self.db) else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                };
                Ok(Outcome::Value(Value::Const(*const_ty)))
            }

            _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
        }
    }

    fn eval_call_expr(&mut self, expr: ExprId) -> Result<Outcome<'db>, InvalidCause<'db>> {
        let body = self.body();
        let Some(callable) = self.typed_body().callable_expr(expr).cloned() else {
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
            let out = match self.eval_expr(arg.expr)? {
                Outcome::Value(v) => v,
                out => return Ok(out),
            };
            let out = value_as_const(out);
            value_args.push(out);
        }

        if func.is_extern(self.db) {
            let ret_ty = self.typed_body().expr_ty(self.db, expr);
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

        let out = self.eval_user_const_fn_call(expr, func, callable.generic_args(), &value_args)?;
        Ok(Outcome::Value(Value::Const(out)))
    }

    fn eval_user_const_fn_call(
        &mut self,
        expr: ExprId,
        func: crate::hir_def::Func<'db>,
        generic_args: &[TyId<'db>],
        value_args: &[ConstTyId<'db>],
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        let body = self.body();
        if self.recursion_depth >= self.config.recursion_limit {
            return Err(InvalidCause::ConstEvalRecursionLimitExceeded { body, expr });
        }

        let Some(body) = func.body(self.db) else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        };

        let (diags, typed_body) = check_func_body(self.db, func);
        if !diags.is_empty() {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        }

        let typed_body = instantiate_typed_body(self.db, typed_body.clone(), generic_args);

        let mut env = Env::default();
        for (idx, arg) in value_args.iter().copied().enumerate() {
            let Some(binding) = typed_body.param_binding(idx) else {
                return Err(InvalidCause::ConstEvalUnsupported { body, expr });
            };
            env.bindings.insert(binding, Value::Const(arg));
        }

        self.recursion_depth += 1;
        let out = self.with_frame(body, typed_body, generic_args.to_vec(), env, |this| {
            let root = body.expr(this.db);
            this.eval_expr(root)
        });
        self.recursion_depth -= 1;

        Ok(value_as_const(match out? {
            Outcome::Return(v) | Outcome::Value(v) => v,
        }))
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
                TyData::TyParam(param) => self.generic_args.get(param.idx).copied().unwrap_or(ty),
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

fn unit_const<'db>(db: &'db dyn HirAnalysisDb) -> ConstTyId<'db> {
    ConstTyId::new(
        db,
        ConstTyData::Evaluated(EvaluatedConstTy::Unit, TyId::unit(db)),
    )
}

fn value_as_const<'db>(value: Value<'db>) -> ConstTyId<'db> {
    match value {
        Value::Const(c) => c,
    }
}

fn value_as_bool<'db>(
    db: &'db dyn HirAnalysisDb,
    value: Value<'db>,
    body: Body<'db>,
    expr: ExprId,
) -> Result<bool, InvalidCause<'db>> {
    let value = value_as_const(value);
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
    let value = value_as_const(value);
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

        #[allow(clippy::bool_comparison)]
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
