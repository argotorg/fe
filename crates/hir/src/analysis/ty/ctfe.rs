use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, ToPrimitive, Zero};
use rustc_hash::FxHashMap;
use tiny_keccak::{Hasher, Keccak};

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    ty::{
        const_expr::{ConstExpr, ConstExprId},
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
        fold::{TyFoldable, TyFolder},
        trait_def::{TraitInstId, resolve_trait_method_instance},
        trait_resolution::PredicateListId,
        ty_check::{
            ConstRef, LocalBinding, RecordLike, TypedBody, check_anon_const_body, check_func_body,
        },
        ty_def::{InvalidCause, PrimTy, TyBase, TyData, TyId, prim_int_bits},
    },
};
use crate::hir_def::{
    Body, CallableDef, Expr, ExprId, Field, IntegerId, LitKind, MatchArm, Partial, Pat, PatId,
    PathId, Stmt, StmtId, VariantKind,
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
enum CtfeAbort<'db> {
    Return(ConstTyId<'db>),
    Invalid(InvalidCause<'db>),
}

impl<'db> From<InvalidCause<'db>> for CtfeAbort<'db> {
    fn from(cause: InvalidCause<'db>) -> Self {
        Self::Invalid(cause)
    }
}

type CtfeResult<'db, T> = Result<T, CtfeAbort<'db>>;
type CtfeEval<'db> = CtfeResult<'db, ConstTyId<'db>>;

#[derive(Default)]
struct Env<'db> {
    bindings: FxHashMap<LocalBinding<'db>, ConstTyId<'db>>,
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

    fn const_depends_on_param(&self, value: ConstTyId<'db>) -> bool {
        TyId::const_ty(self.db, value).has_param(self.db)
    }

    fn abstract_const(&self, expr: ConstExpr<'db>, ty: TyId<'db>) -> ConstTyId<'db> {
        let expr = ConstExprId::new(self.db, expr);
        ConstTyId::new(self.db, ConstTyData::Abstract(expr, ty))
    }

    fn with_frame<T>(
        &mut self,
        body: Body<'db>,
        typed_body: TypedBody<'db>,
        generic_args: Vec<TyId<'db>>,
        env: Env<'db>,
        f: impl FnOnce(&mut Self) -> CtfeResult<'db, T>,
    ) -> CtfeResult<'db, T> {
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
        let out = self.with_frame(body, typed_body, Vec::new(), Env::default(), |this| {
            this.eval_expr(this.body().expr(this.db))
        });

        match out {
            Ok(v) | Err(CtfeAbort::Return(v)) => Ok(v),
            Err(CtfeAbort::Invalid(cause)) => Err(cause),
        }
    }

    pub(crate) fn eval_expr_in_body(
        &mut self,
        body: Body<'db>,
        typed_body: TypedBody<'db>,
        generic_args: Vec<TyId<'db>>,
        expr: ExprId,
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        let out = self.with_frame(body, typed_body, generic_args, Env::default(), |this| {
            this.eval_expr(expr)
        });

        match out {
            Ok(v) | Err(CtfeAbort::Return(v)) => Ok(v),
            Err(CtfeAbort::Invalid(cause)) => Err(cause),
        }
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

    fn eval_expr(&mut self, expr: ExprId) -> CtfeEval<'db> {
        let body = self.body();
        self.tick(expr)?;

        let Partial::Present(expr_data) = expr.data(self.db, body) else {
            return Err(InvalidCause::ParseError.into());
        };

        match expr_data {
            Expr::Lit(LitKind::Bool(flag)) => Ok(lit_bool(self.db, *flag)),
            Expr::Lit(LitKind::Int(int_id)) => {
                let ty = self.typed_body().expr_ty(self.db, expr);
                let value = normalize_int(self.db, ty, int_id.data(self.db).clone(), body, expr)?;
                Ok(lit_int(self.db, ty, value))
            }

            Expr::Lit(LitKind::String(string_id)) => {
                let ty = self.typed_body().expr_ty(self.db, expr);
                Ok(lit_bytes(
                    self.db,
                    ty,
                    string_id.data(self.db).as_bytes().to_vec(),
                ))
            }

            Expr::Path(Partial::Present(path)) => self.eval_path_expr(*path, expr),
            Expr::Path(Partial::Absent) => Err(InvalidCause::ParseError.into()),
            Expr::Un(inner, op) => {
                let inner = self.eval_expr(*inner)?;
                self.eval_unary(expr, inner, *op)
            }
            Expr::Cast(inner, _) => self.eval_cast(expr, *inner),
            Expr::Bin(lhs, rhs, op) => self.eval_binary(expr, *lhs, *rhs, *op),
            Expr::If(cond, then, else_) => {
                let cond = self.eval_expr(*cond)?;
                let cond = const_as_bool(self.db, cond, body, expr)?;
                if cond {
                    self.eval_expr(*then)
                } else if let Some(else_) = else_ {
                    self.eval_expr(*else_)
                } else {
                    Ok(unit_const(self.db))
                }
            }

            Expr::Match(scrutinee, arms) => self.eval_match(expr, *scrutinee, arms),
            Expr::Block(stmts) => self.eval_block(stmts),
            Expr::Call(_, _) => self.eval_call_expr(expr),
            Expr::MethodCall(..) => self.eval_method_call_expr(expr),
            Expr::Tuple(elems) => self.eval_tuple(expr, elems),
            Expr::Array(elems) => self.eval_array(expr, elems),
            Expr::ArrayRep(elem, len) => self.eval_array_rep(expr, *elem, len),
            Expr::RecordInit(path, fields) => self.eval_record_init(expr, path, fields),
            Expr::Field(lhs, field) => self.eval_field(expr, *lhs, field),
            _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }.into()),
        }
    }

    fn eval_cast(&mut self, expr: ExprId, inner_expr: ExprId) -> CtfeEval<'db> {
        let body = self.body();
        let typed = self.typed_body();
        let to_ty = typed.expr_ty(self.db, expr);
        let (from_bits, from_is_signed) =
            int_layout(self.db, typed.expr_ty(self.db, inner_expr), body, expr)?;
        let (to_bits, _) = int_layout(self.db, to_ty, body, expr)?;
        let inner = self.eval_expr(inner_expr)?;
        let raw = match inner.data(self.db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => int_id.data(self.db),
            _ if self.const_depends_on_param(inner) => {
                return Ok(self.abstract_const(
                    ConstExpr::Cast {
                        expr: TyId::const_ty(self.db, inner),
                        to: to_ty,
                    },
                    to_ty,
                ));
            }
            _ => return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into()),
        };
        let value = if from_is_signed {
            to_signed(from_bits, raw)
        } else {
            BigInt::from_biguint(Sign::Plus, raw.clone())
        };
        Ok(lit_int(self.db, to_ty, from_signed(to_bits, value)))
    }

    fn eval_block(&mut self, stmts: &[StmtId]) -> CtfeEval<'db> {
        let mut last = unit_const(self.db);
        for stmt in stmts {
            last = self.eval_stmt(*stmt)?;
        }
        Ok(last)
    }

    fn eval_match(
        &mut self,
        expr: ExprId,
        scrutinee_expr: ExprId,
        arms: &Partial<Vec<MatchArm>>,
    ) -> CtfeEval<'db> {
        let body = self.body();
        let Some(arms) = arms.clone().to_opt() else {
            return Err(InvalidCause::ParseError.into());
        };

        let scrutinee = self.eval_expr(scrutinee_expr)?;
        if !matches!(scrutinee.data(self.db), ConstTyData::Evaluated(..)) {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
        }

        let base_bindings = self.env().bindings.clone();
        for arm in arms {
            let mut arm_bindings = base_bindings.clone();
            if !self.try_match_pat(expr, arm.pat, scrutinee, &mut arm_bindings)? {
                continue;
            }

            let old_bindings = std::mem::replace(&mut self.env_mut().bindings, arm_bindings);
            let result = self.eval_expr(arm.body);
            self.env_mut().bindings = old_bindings;
            return result;
        }

        Err(InvalidCause::ConstEvalUnsupported { body, expr }.into())
    }

    fn eval_stmt(&mut self, stmt: StmtId) -> CtfeEval<'db> {
        let body = self.body();
        let Partial::Present(stmt_data) = stmt.data(self.db, body) else {
            return Err(InvalidCause::ParseError.into());
        };

        match stmt_data {
            Stmt::Let(pat, _ty, init) => {
                let Some(init) = init else {
                    return Ok(unit_const(self.db));
                };
                let value = self.eval_expr(*init)?;
                self.bind_pat(*pat, value)?;
                Ok(unit_const(self.db))
            }

            Stmt::Expr(expr) => self.eval_expr(*expr),

            Stmt::Return(expr) => {
                let value = expr.map_or(Ok(unit_const(self.db)), |expr| self.eval_expr(expr))?;
                Err(CtfeAbort::Return(value))
            }

            _ => Err(InvalidCause::ConstEvalUnsupported {
                body,
                expr: body.expr(self.db),
            }
            .into()),
        }
    }

    fn try_match_pat(
        &mut self,
        expr: ExprId,
        pat: PatId,
        value: ConstTyId<'db>,
        bindings: &mut FxHashMap<LocalBinding<'db>, ConstTyId<'db>>,
    ) -> Result<bool, InvalidCause<'db>> {
        let body = self.body();
        let Partial::Present(pat_data) = pat.data(self.db, body) else {
            return Err(InvalidCause::ParseError);
        };

        match pat_data {
            Pat::WildCard | Pat::Rest => Ok(true),
            Pat::Lit(lit) => {
                let Partial::Present(lit) = lit else {
                    return Err(InvalidCause::ParseError);
                };
                match (lit, value.data(self.db)) {
                    (
                        LitKind::Bool(expected),
                        ConstTyData::Evaluated(EvaluatedConstTy::LitBool(actual), _),
                    ) => Ok(*expected == *actual),
                    (
                        LitKind::Int(expected),
                        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(actual), _),
                    ) => {
                        let ty = value.ty(self.db);
                        let expected =
                            normalize_int(self.db, ty, expected.data(self.db).clone(), body, expr)?;
                        let actual =
                            normalize_int(self.db, ty, actual.data(self.db).clone(), body, expr)?;
                        Ok(expected == actual)
                    }
                    (
                        LitKind::String(expected),
                        ConstTyData::Evaluated(EvaluatedConstTy::Bytes(actual), _),
                    ) => Ok(expected.data(self.db).as_bytes() == actual.as_slice()),
                    _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
                }
            }
            Pat::Path(_, is_mut) => {
                if *is_mut {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                }
                let Some(binding) = self.typed_body().pat_binding(pat) else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                };
                bindings.insert(binding, value);
                Ok(true)
            }
            Pat::Tuple(pats) => {
                let ConstTyData::Evaluated(EvaluatedConstTy::Tuple(elems), _) = value.data(self.db)
                else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                };

                let rest_idx = pats.iter().position(|pat| pat.is_rest(self.db, body));
                debug_assert!(
                    pats.iter().filter(|pat| pat.is_rest(self.db, body)).count() <= 1,
                    "tuple pattern contains multiple `..`"
                );

                match rest_idx {
                    None => {
                        debug_assert_eq!(pats.len(), elems.len(), "tuple pattern length mismatch");
                        for (&pat, &elem) in pats.iter().zip(elems.iter()) {
                            let const_ty = ty_as_const_ty(self.db, body, expr, elem)?;
                            if !self.try_match_pat(expr, pat, const_ty, bindings)? {
                                return Ok(false);
                            }
                        }
                    }
                    Some(rest) => {
                        let prefix_len = rest;
                        let suffix_len = pats.len() - rest - 1;
                        debug_assert!(
                            prefix_len + suffix_len <= elems.len(),
                            "tuple rest pattern is too long"
                        );

                        for (idx, &pat) in pats[..prefix_len].iter().enumerate() {
                            let elem = elems.get(idx).copied().unwrap();
                            let const_ty = ty_as_const_ty(self.db, body, expr, elem)?;
                            if !self.try_match_pat(expr, pat, const_ty, bindings)? {
                                return Ok(false);
                            }
                        }

                        let tail_start = elems.len().saturating_sub(suffix_len);
                        for (pat, elem) in pats[rest + 1..].iter().zip(&elems[tail_start..]) {
                            let const_ty = ty_as_const_ty(self.db, body, expr, *elem)?;
                            if !self.try_match_pat(expr, *pat, const_ty, bindings)? {
                                return Ok(false);
                            }
                        }
                    }
                }

                Ok(true)
            }
            Pat::Record(_path, fields) => {
                let ConstTyData::Evaluated(EvaluatedConstTy::Record(values), _) =
                    value.data(self.db)
                else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr });
                };

                let record_like = RecordLike::from_ty(value.ty(self.db));
                for field in fields {
                    if field.pat.is_rest(self.db, body) {
                        continue;
                    }
                    let label = field.label(self.db, body).unwrap();
                    let idx = record_like.record_field_idx(self.db, label).unwrap();
                    let field_value = values.get(idx).copied().unwrap();
                    let const_ty = ty_as_const_ty(self.db, body, expr, field_value)?;
                    if !self.try_match_pat(expr, field.pat, const_ty, bindings)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            Pat::Or(lhs, rhs) => {
                let mut lhs_bindings = bindings.clone();
                if self.try_match_pat(expr, *lhs, value, &mut lhs_bindings)? {
                    *bindings = lhs_bindings;
                    return Ok(true);
                }

                let mut rhs_bindings = bindings.clone();
                if self.try_match_pat(expr, *rhs, value, &mut rhs_bindings)? {
                    *bindings = rhs_bindings;
                    return Ok(true);
                }

                Ok(false)
            }
            _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
        }
    }

    fn bind_pat(&mut self, pat: PatId, value: ConstTyId<'db>) -> Result<(), InvalidCause<'db>> {
        let body = self.body();
        let expr = body.expr(self.db);
        let Partial::Present(pat_data) = pat.data(self.db, body) else {
            return Err(InvalidCause::ParseError);
        };

        match pat_data {
            Pat::WildCard => Ok(()),
            Pat::Rest => Ok(()),
            Pat::Path(..) => {
                let binding = self.typed_body().pat_binding(pat).unwrap();
                self.env_mut().bindings.insert(binding, value);
                Ok(())
            }
            Pat::Tuple(pats) => {
                let ConstTyData::Evaluated(EvaluatedConstTy::Tuple(elems), _) = value.data(self.db)
                else {
                    return Err(InvalidCause::ConstEvalUnsupported {
                        body,
                        expr: body.expr(self.db),
                    });
                };

                let rest_idx = pats.iter().position(|pat| pat.is_rest(self.db, body));
                match rest_idx {
                    None => {
                        debug_assert_eq!(pats.len(), elems.len(), "tuple pattern length mismatch");
                        for (&pat, &elem) in pats.iter().zip(elems.iter()) {
                            let const_ty = ty_as_const_ty(self.db, body, expr, elem)?;
                            self.bind_pat(pat, const_ty)?;
                        }
                    }
                    Some(rest) => {
                        let prefix_len = rest;
                        let suffix_len = pats.len() - rest - 1;
                        debug_assert!(
                            prefix_len + suffix_len <= elems.len(),
                            "tuple rest pattern is too long"
                        );

                        for (idx, &pat) in pats[..prefix_len].iter().enumerate() {
                            let elem = elems.get(idx).unwrap();
                            let const_ty = ty_as_const_ty(self.db, body, expr, *elem)?;
                            self.bind_pat(pat, const_ty)?;
                        }

                        let tail_start = elems.len().saturating_sub(suffix_len);
                        for (pat, elem) in pats[rest + 1..].iter().zip(&elems[tail_start..]) {
                            let const_ty = ty_as_const_ty(self.db, body, expr, *elem)?;
                            self.bind_pat(*pat, const_ty)?;
                        }
                    }
                }
                Ok(())
            }
            Pat::Record(_path, fields) => {
                let ConstTyData::Evaluated(EvaluatedConstTy::Record(values), _) =
                    value.data(self.db)
                else {
                    return Err(InvalidCause::ConstEvalUnsupported {
                        body,
                        expr: body.expr(self.db),
                    });
                };

                let record_like = RecordLike::from_ty(value.ty(self.db));
                for field in fields {
                    if field.pat.is_rest(self.db, body) {
                        continue;
                    }
                    let label = field.label(self.db, body).unwrap();
                    let idx = record_like.record_field_idx(self.db, label).unwrap();
                    let field_value = values.get(idx).copied().unwrap();
                    let const_ty = ty_as_const_ty(self.db, body, expr, field_value)?;
                    self.bind_pat(field.pat, const_ty)?;
                }

                Ok(())
            }
            _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
        }
    }

    fn eval_path_expr(&mut self, path: PathId<'db>, expr: ExprId) -> CtfeEval<'db> {
        let body = self.body();
        if let Some(binding) = self.typed_body().expr_binding(expr) {
            if let Some(value) = self.env().bindings.get(&binding).cloned() {
                return Ok(value);
            }
            if matches!(binding, LocalBinding::Param { .. }) {
                let ty = self.typed_body().expr_ty(self.db, expr);
                return Ok(self.abstract_const(ConstExpr::LocalBinding(binding), ty));
            }
        }

        if let Some(cref) = self.typed_body().expr_const_ref(expr) {
            let expected_ty = self.typed_body().expr_ty(self.db, expr);
            return Ok(self.eval_const_ref(cref, expected_ty)?);
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
                return Ok(arg_const.evaluate(self.db, Some(arg_const.ty(self.db))));
            }

            if let TyData::ConstTy(const_ty) = ty.data(self.db)
                && matches!(const_ty.data(self.db), ConstTyData::TyParam(..))
            {
                return Ok(*const_ty);
            }
        }

        Err(InvalidCause::ConstEvalUnsupported { body, expr }.into())
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
                let mut subst = GenericSubst {
                    generic_args: self.generic_args(),
                };
                let inst = inst.fold_with(self.db, &mut subst);
                let inst = if matches!(
                    inst.self_ty(self.db).data(self.db),
                    TyData::TyParam(_) | TyData::TyVar(_)
                ) {
                    if let Some(&self_arg) = self.generic_args().first() {
                        let mut args = inst.args(self.db).to_vec();
                        if let Some(arg) = args.first_mut() {
                            *arg = self_arg;
                        }
                        TraitInstId::new(
                            self.db,
                            inst.def(self.db),
                            args,
                            inst.assoc_type_bindings(self.db).clone(),
                        )
                    } else {
                        inst
                    }
                } else {
                    inst
                };

                if let Some(const_ty) =
                    crate::analysis::ty::const_ty::const_ty_from_trait_const(self.db, inst, name)
                {
                    const_ty
                } else {
                    return Ok(
                        self.abstract_const(ConstExpr::TraitConst { inst, name }, expected_ty)
                    );
                }
            }
        };

        let evaluated = const_ty.evaluate(self.db, Some(expected_ty));
        evaluated
            .ty(self.db)
            .invalid_cause(self.db)
            .map(Err)
            .unwrap_or(Ok(evaluated))
    }

    fn eval_unary(&mut self, expr: ExprId, inner: ConstTyId<'db>, op: UnOp) -> CtfeEval<'db> {
        let body = self.body();
        match op {
            UnOp::Plus => Ok(inner),
            UnOp::Not => match inner.data(self.db) {
                ConstTyData::Evaluated(EvaluatedConstTy::LitBool(flag), _) => {
                    Ok(lit_bool(self.db, !*flag))
                }
                _ if self.const_depends_on_param(inner) => {
                    let ty = self.typed_body().expr_ty(self.db, expr);
                    Ok(self.abstract_const(
                        ConstExpr::UnOp {
                            op,
                            expr: TyId::const_ty(self.db, inner),
                        },
                        ty,
                    ))
                }
                _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }.into()),
            },

            UnOp::Minus | UnOp::BitNot => {
                let ty = self.typed_body().expr_ty(self.db, expr);
                let (bits, _) = int_layout(self.db, ty, body, expr)?;
                let v = match inner.data(self.db) {
                    ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => {
                        int_id.data(self.db).clone()
                    }
                    _ if self.const_depends_on_param(inner) => {
                        return Ok(self.abstract_const(
                            ConstExpr::UnOp {
                                op,
                                expr: TyId::const_ty(self.db, inner),
                            },
                            ty,
                        ));
                    }
                    _ => return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into()),
                };
                let (modulus, mask) = int_modulus_mask(bits);
                let out = match op {
                    UnOp::Minus => (modulus.clone() - (v % &modulus)) & &mask,
                    UnOp::BitNot => &mask ^ v,
                    _ => unreachable!(),
                };
                Ok(lit_int(self.db, ty, out))
            }
        }
    }

    fn eval_binary(
        &mut self,
        expr: ExprId,
        lhs_expr: ExprId,
        rhs_expr: ExprId,
        op: BinOp,
    ) -> CtfeEval<'db> {
        let body = self.body();
        match op {
            BinOp::Logical(logical) => {
                let lhs = const_as_bool(self.db, self.eval_expr(lhs_expr)?, body, expr)?;
                match (logical, lhs) {
                    (LogicalBinOp::And, false) => return Ok(lit_bool(self.db, false)),
                    (LogicalBinOp::Or, true) => return Ok(lit_bool(self.db, true)),
                    _ => {}
                }

                let rhs = const_as_bool(self.db, self.eval_expr(rhs_expr)?, body, expr)?;
                Ok(lit_bool(
                    self.db,
                    match logical {
                        LogicalBinOp::And => lhs && rhs,
                        LogicalBinOp::Or => lhs || rhs,
                    },
                ))
            }

            BinOp::Comp(comp) => Ok(eval_cmp(
                self.db,
                self.typed_body().expr_ty(self.db, lhs_expr),
                self.eval_expr(lhs_expr)?,
                self.eval_expr(rhs_expr)?,
                body,
                expr,
                comp,
            )?),

            BinOp::Arith(arith) => {
                let lhs = self.eval_expr(lhs_expr)?;
                let rhs = self.eval_expr(rhs_expr)?;
                Ok(self.eval_arith_binop(expr, lhs, rhs, arith)?)
            }

            BinOp::Index => {
                let lhs = self.eval_expr(lhs_expr)?;
                let rhs = self.eval_expr(rhs_expr)?;

                let idx = const_as_int(self.db, rhs, body, expr)?;
                let Some(idx) = idx.to_usize() else {
                    return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
                };

                match lhs.data(self.db) {
                    ConstTyData::Evaluated(EvaluatedConstTy::Array(elems), _) => {
                        let Some(elem) = elems.get(idx).copied() else {
                            return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
                        };
                        Ok(ty_as_const_ty(self.db, body, expr, elem)?)
                    }
                    ConstTyData::Evaluated(EvaluatedConstTy::Bytes(bytes), _) => {
                        let Some(byte) = bytes.get(idx).copied() else {
                            return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
                        };
                        let ty = self.typed_body().expr_ty(self.db, expr);
                        Ok(lit_int(self.db, ty, BigUint::from(byte)))
                    }
                    _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }.into()),
                }
            }
        }
    }

    fn eval_arith_binop(
        &mut self,
        expr: ExprId,
        lhs: ConstTyId<'db>,
        rhs: ConstTyId<'db>,
        op: ArithBinOp,
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        let body = self.body();
        let ty = self.typed_body().expr_ty(self.db, expr);
        let (bits, signed) = int_layout(self.db, ty, body, expr)?;
        let (lhs_u, rhs_u) = match (lhs.data(self.db), rhs.data(self.db)) {
            (
                ConstTyData::Evaluated(EvaluatedConstTy::LitInt(lhs_int), _),
                ConstTyData::Evaluated(EvaluatedConstTy::LitInt(rhs_int), _),
            ) => (lhs_int.data(self.db).clone(), rhs_int.data(self.db).clone()),
            _ if self.const_depends_on_param(lhs) || self.const_depends_on_param(rhs) => {
                return Ok(self.abstract_const(
                    ConstExpr::ArithBinOp {
                        op,
                        lhs: TyId::const_ty(self.db, lhs),
                        rhs: TyId::const_ty(self.db, rhs),
                    },
                    ty,
                ));
            }
            _ => return Err(InvalidCause::ConstEvalUnsupported { body, expr }),
        };
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
            ArithBinOp::Range => return Err(InvalidCause::ConstEvalUnsupported { body, expr }),
        };

        Ok(lit_int(self.db, ty, out))
    }

    fn eval_const_elems(&mut self, elems: &[ExprId]) -> CtfeResult<'db, Vec<TyId<'db>>> {
        elems
            .iter()
            .map(|&expr| Ok(TyId::const_ty(self.db, self.eval_expr(expr)?)))
            .collect()
    }

    fn eval_tuple(&mut self, expr: ExprId, elems: &[ExprId]) -> CtfeEval<'db> {
        let values = self.eval_const_elems(elems)?;
        let ty = self.typed_body().expr_ty(self.db, expr);
        Ok(ConstTyId::new(
            self.db,
            ConstTyData::Evaluated(EvaluatedConstTy::Tuple(values), ty),
        ))
    }

    fn eval_array(&mut self, expr: ExprId, elems: &[ExprId]) -> CtfeEval<'db> {
        let ty = self.typed_body().expr_ty(self.db, expr);
        if is_u8_array_ty(self.db, ty) {
            let body = self.body();
            let bytes = elems
                .iter()
                .map(|&elem| Ok(const_as_u8(self.db, self.eval_expr(elem)?, body, expr)?))
                .collect::<CtfeResult<'db, Vec<_>>>()?;
            return Ok(lit_bytes(self.db, ty, bytes));
        }

        let values = self.eval_const_elems(elems)?;
        Ok(ConstTyId::new(
            self.db,
            ConstTyData::Evaluated(EvaluatedConstTy::Array(values), ty),
        ))
    }

    fn eval_array_rep(
        &mut self,
        expr: ExprId,
        elem_expr: ExprId,
        len: &Partial<Body<'db>>,
    ) -> CtfeEval<'db> {
        let body = self.body();
        let Some(len_body) = len.to_opt() else {
            return Err(InvalidCause::ParseError.into());
        };

        let expected_len_ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
        let (len_diags, typed_len_body) = check_anon_const_body(self.db, len_body, expected_len_ty);
        if !len_diags.is_empty() {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
        }
        let typed_len_body = typed_len_body.clone();
        let len = const_as_int(
            self.db,
            self.eval_const_body(len_body, typed_len_body)?,
            body,
            expr,
        )?;
        let Some(len) = len.to_usize() else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
        };

        let ty = self.typed_body().expr_ty(self.db, expr);
        if is_u8_array_ty(self.db, ty) {
            let elem = self.eval_expr(elem_expr)?;
            let byte = const_as_u8(self.db, elem, body, expr)?;
            let bytes = std::iter::repeat_n(byte, len).collect::<Vec<_>>();
            return Ok(lit_bytes(self.db, ty, bytes));
        }

        let elem_const = TyId::const_ty(self.db, self.eval_expr(elem_expr)?);
        let values = std::iter::repeat_n(elem_const, len).collect::<Vec<_>>();
        Ok(ConstTyId::new(
            self.db,
            ConstTyData::Evaluated(EvaluatedConstTy::Array(values), ty),
        ))
    }

    fn eval_record_init(
        &mut self,
        expr: ExprId,
        path: &Partial<PathId<'db>>,
        fields: &[Field<'db>],
    ) -> CtfeEval<'db> {
        let body = self.body();
        let Partial::Present(path) = path else {
            return Err(InvalidCause::ParseError.into());
        };

        let assumptions = PredicateListId::empty_list(self.db);
        let resolved = resolve_path(self.db, *path, body.scope(), assumptions, true)
            .map_err(|_| InvalidCause::ConstEvalUnsupported { body, expr })?;

        let record_like = match resolved {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => RecordLike::from_ty(ty),
            PathRes::EnumVariant(variant) => RecordLike::from_variant(variant),
            _ => return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into()),
        };

        if !record_like.is_record(self.db) {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
        }

        let field_count = match &record_like {
            RecordLike::Type(ty) => ty.field_count(self.db),
            RecordLike::EnumVariant(variant) => match variant.kind(self.db) {
                VariantKind::Record(fields) => fields.data(self.db).len(),
                _ => unreachable!("ctfe invariant: expected record enum variant"),
            },
        };

        let mut values = vec![None; field_count];
        for field in fields {
            let label = field.label_eagerly(self.db, body).unwrap();
            let idx = record_like.record_field_idx(self.db, label).unwrap();
            values[idx] = Some(TyId::const_ty(self.db, self.eval_expr(field.expr)?));
        }

        let values = values.into_iter().collect::<Option<Vec<_>>>().unwrap();
        let ty = self.typed_body().expr_ty(self.db, expr);
        Ok(ConstTyId::new(
            self.db,
            ConstTyData::Evaluated(EvaluatedConstTy::Record(values), ty),
        ))
    }

    fn eval_field(
        &mut self,
        expr: ExprId,
        lhs_expr: ExprId,
        field: &Partial<crate::hir_def::FieldIndex<'db>>,
    ) -> CtfeEval<'db> {
        let body = self.body();
        let Some(field) = field.to_opt() else {
            return Err(InvalidCause::ParseError.into());
        };

        let lhs = self.eval_expr(lhs_expr)?;

        match (lhs.data(self.db), field) {
            (
                ConstTyData::Evaluated(EvaluatedConstTy::Tuple(elems), _),
                crate::hir_def::FieldIndex::Index(index),
            ) => {
                let index = index.data(self.db).to_usize().unwrap();
                let elem = elems.get(index).copied().unwrap();
                Ok(ty_as_const_ty(self.db, body, expr, elem)?)
            }

            (
                ConstTyData::Evaluated(EvaluatedConstTy::Record(fields), _),
                crate::hir_def::FieldIndex::Ident(name),
            ) => {
                let lhs_ty = self.typed_body().expr_ty(self.db, lhs_expr);
                let record_like = RecordLike::from_ty(lhs_ty);
                let idx = record_like.record_field_idx(self.db, name).unwrap();
                let field = fields.get(idx).copied().unwrap();
                Ok(ty_as_const_ty(self.db, body, expr, field)?)
            }

            _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }.into()),
        }
    }

    fn eval_call_expr(&mut self, expr: ExprId) -> CtfeEval<'db> {
        let body = self.body();
        let Some(callable) = self.typed_body().callable_expr(expr).cloned() else {
            debug_assert!(false, "ctfe invariant: missing callable for call expr");
            return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
        };
        let CallableDef::Func(func) = callable.callable_def else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
        };
        if !func.is_const(self.db) {
            return Err(InvalidCause::ConstEvalNonConstCall { body, expr }.into());
        }

        let Partial::Present(Expr::Call(_callee, args)) = expr.data(self.db, body) else {
            unreachable!("ctfe invariant: eval_call_expr called on non-call expr");
        };

        let value_args = args
            .iter()
            .map(|arg| self.eval_expr(arg.expr))
            .collect::<CtfeResult<'db, Vec<_>>>()?;

        if func.is_extern(self.db) {
            let ret_ty = self.typed_body().expr_ty(self.db, expr);
            if let Some(value) = self.eval_extern_const_fn(expr, func, ret_ty, &value_args)? {
                return Ok(value);
            }
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
            return Ok(ConstTyId::new(
                self.db,
                ConstTyData::Abstract(expr_id, ret_ty),
            ));
        }

        let args_depend_on_params = value_args
            .iter()
            .copied()
            .any(|arg| self.const_depends_on_param(arg));
        match self.eval_user_const_fn_call(expr, func, callable.generic_args(), &value_args) {
            Ok(value) if args_depend_on_params && self.const_depends_on_param(value) => {
                let ret_ty = self.typed_body().expr_ty(self.db, expr);
                let args = value_args
                    .iter()
                    .copied()
                    .map(|v| TyId::const_ty(self.db, v))
                    .collect::<Vec<_>>();
                let expr_id = ConstExprId::new(
                    self.db,
                    ConstExpr::UserConstFnCall {
                        func,
                        generic_args: callable.generic_args().to_vec(),
                        args,
                    },
                );
                Ok(ConstTyId::new(
                    self.db,
                    ConstTyData::Abstract(expr_id, ret_ty),
                ))
            }
            Ok(value) => Ok(value),
            Err(InvalidCause::ConstEvalUnsupported { .. }) if args_depend_on_params => {
                let ret_ty = self.typed_body().expr_ty(self.db, expr);
                let args = value_args
                    .iter()
                    .copied()
                    .map(|v| TyId::const_ty(self.db, v))
                    .collect::<Vec<_>>();
                let expr_id = ConstExprId::new(
                    self.db,
                    ConstExpr::UserConstFnCall {
                        func,
                        generic_args: callable.generic_args().to_vec(),
                        args,
                    },
                );
                Ok(ConstTyId::new(
                    self.db,
                    ConstTyData::Abstract(expr_id, ret_ty),
                ))
            }
            Err(cause) => Err(cause.into()),
        }
    }

    fn eval_method_call_expr(&mut self, expr: ExprId) -> CtfeEval<'db> {
        let body = self.body();
        let Some(callable) = self.typed_body().callable_expr(expr).cloned() else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
        };
        let CallableDef::Func(mut func) = callable.callable_def else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
        };
        if !func.is_const(self.db) {
            return Err(InvalidCause::ConstEvalNonConstCall { body, expr }.into());
        }

        let Partial::Present(Expr::MethodCall(receiver, _method, _generic_args, args)) =
            expr.data(self.db, body)
        else {
            unreachable!("ctfe invariant: eval_method_call_expr called on non-call expr");
        };

        let receiver_value = self.eval_expr(*receiver)?;
        let receiver_ty = receiver_value.ty(self.db);
        let mut value_args = Vec::with_capacity(args.len() + 1);
        value_args.push(receiver_value);
        for arg in args {
            value_args.push(self.eval_expr(arg.expr)?);
        }

        let mut generic_args = callable.generic_args().to_vec();

        if let Some(inst) = callable.trait_inst() {
            let Some(name) = func.name(self.db).to_opt() else {
                return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
            };
            let inst = if matches!(
                inst.self_ty(self.db).data(self.db),
                TyData::TyParam(_) | TyData::TyVar(_)
            ) {
                let mut args = inst.args(self.db).to_vec();
                if let Some(self_arg) = args.first_mut() {
                    *self_arg = receiver_ty;
                }
                TraitInstId::new(
                    self.db,
                    inst.def(self.db),
                    args,
                    inst.assoc_type_bindings(self.db).clone(),
                )
            } else {
                inst
            };

            let trait_arg_len = inst.args(self.db).len();
            if generic_args.len() < trait_arg_len {
                return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
            }

            if let Some((impl_func, impl_args)) = resolve_trait_method_instance(self.db, inst, name)
            {
                func = impl_func;
                if !func.is_const(self.db) {
                    return Err(InvalidCause::ConstEvalNonConstCall { body, expr }.into());
                }

                let mut resolved_args = impl_args;
                resolved_args.extend_from_slice(&generic_args[trait_arg_len..]);
                generic_args = resolved_args;
            } else if func.body(self.db).is_none() {
                return Err(InvalidCause::ConstEvalUnsupported { body, expr }.into());
            }
        }

        if func.is_extern(self.db) {
            let ret_ty = self.typed_body().expr_ty(self.db, expr);
            if let Some(value) = self.eval_extern_const_fn(expr, func, ret_ty, &value_args)? {
                return Ok(value);
            }

            let args = value_args
                .iter()
                .copied()
                .map(|v| TyId::const_ty(self.db, v))
                .collect::<Vec<_>>();
            let expr_id = ConstExprId::new(
                self.db,
                ConstExpr::ExternConstFnCall {
                    func,
                    generic_args,
                    args,
                },
            );
            return Ok(ConstTyId::new(
                self.db,
                ConstTyData::Abstract(expr_id, ret_ty),
            ));
        }

        let args_depend_on_params = value_args
            .iter()
            .copied()
            .any(|arg| self.const_depends_on_param(arg));
        match self.eval_user_const_fn_call(expr, func, &generic_args, &value_args) {
            Ok(value) if args_depend_on_params && self.const_depends_on_param(value) => {
                let ret_ty = self.typed_body().expr_ty(self.db, expr);
                let args = value_args
                    .iter()
                    .copied()
                    .map(|v| TyId::const_ty(self.db, v))
                    .collect::<Vec<_>>();
                let expr_id = ConstExprId::new(
                    self.db,
                    ConstExpr::UserConstFnCall {
                        func,
                        generic_args: generic_args.clone(),
                        args,
                    },
                );
                Ok(ConstTyId::new(
                    self.db,
                    ConstTyData::Abstract(expr_id, ret_ty),
                ))
            }
            Ok(value) => Ok(value),
            Err(InvalidCause::ConstEvalUnsupported { .. }) if args_depend_on_params => {
                let ret_ty = self.typed_body().expr_ty(self.db, expr);
                let args = value_args
                    .iter()
                    .copied()
                    .map(|v| TyId::const_ty(self.db, v))
                    .collect::<Vec<_>>();
                let expr_id = ConstExprId::new(
                    self.db,
                    ConstExpr::UserConstFnCall {
                        func,
                        generic_args: generic_args.clone(),
                        args,
                    },
                );
                Ok(ConstTyId::new(
                    self.db,
                    ConstTyData::Abstract(expr_id, ret_ty),
                ))
            }
            Err(cause) => Err(cause.into()),
        }
    }

    fn eval_user_const_fn_call(
        &mut self,
        expr: ExprId,
        func: crate::hir_def::Func<'db>,
        generic_args: &[TyId<'db>],
        value_args: &[ConstTyId<'db>],
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        let call_body = self.body();
        if self.recursion_depth >= self.config.recursion_limit {
            return Err(InvalidCause::ConstEvalRecursionLimitExceeded {
                body: call_body,
                expr,
            });
        }

        let Some(func_body) = func.body(self.db) else {
            return Err(InvalidCause::ConstEvalUnsupported {
                body: call_body,
                expr,
            });
        };

        let (diags, typed_body) = check_func_body(self.db, func);
        if !diags.is_empty() {
            return Err(InvalidCause::ConstEvalUnsupported {
                body: call_body,
                expr,
            });
        }

        let typed_body = instantiate_typed_body(self.db, typed_body.clone(), generic_args);

        let mut env = Env::default();
        for (idx, arg) in value_args.iter().copied().enumerate() {
            let Some(binding) = typed_body.param_binding(idx) else {
                return Err(InvalidCause::ConstEvalUnsupported {
                    body: call_body,
                    expr,
                });
            };
            env.bindings.insert(binding, arg);
        }

        self.recursion_depth += 1;
        let out = self.with_frame(func_body, typed_body, generic_args.to_vec(), env, |this| {
            this.eval_expr(this.body().expr(this.db))
        });
        self.recursion_depth -= 1;

        match out {
            Ok(v) | Err(CtfeAbort::Return(v)) => Ok(v),
            Err(CtfeAbort::Invalid(cause)) => Err(cause),
        }
    }

    fn eval_extern_const_fn(
        &mut self,
        expr: ExprId,
        func: crate::hir_def::Func<'db>,
        ret_ty: TyId<'db>,
        args: &[ConstTyId<'db>],
    ) -> CtfeResult<'db, Option<ConstTyId<'db>>> {
        let Some(name) = func.name(self.db).to_opt() else {
            return Ok(None);
        };

        match name.data(self.db).as_str() {
            "__as_bytes" => Ok(Some(self.eval_intrinsic_as_bytes(expr, ret_ty, args)?)),
            "__keccak256" => Ok(Some(self.eval_intrinsic_keccak(expr, ret_ty, args)?)),
            _ => Ok(None),
        }
    }

    fn eval_intrinsic_as_bytes(
        &self,
        expr: ExprId,
        ret_ty: TyId<'db>,
        args: &[ConstTyId<'db>],
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        let body = self.body();
        if !is_u8_array_ty(self.db, ret_ty) {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        };
        let [value] = args else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        };
        let bytes = const_as_bytes(self.db, *value, body, expr)?;
        if let Some(len) = array_len(self.db, ret_ty)
            && bytes.len() != len
        {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        }
        Ok(lit_bytes(self.db, ret_ty, bytes))
    }

    fn eval_intrinsic_keccak(
        &self,
        expr: ExprId,
        ret_ty: TyId<'db>,
        args: &[ConstTyId<'db>],
    ) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
        let body = self.body();
        let [value] = args else {
            return Err(InvalidCause::ConstEvalUnsupported { body, expr });
        };

        let bytes = const_as_bytes(self.db, *value, body, expr)?;
        let mut hasher = Keccak::v256();
        hasher.update(&bytes);
        let mut out = [0u8; 32];
        hasher.finalize(&mut out);
        Ok(lit_int(self.db, ret_ty, BigUint::from_bytes_be(&out)))
    }
}

pub(super) fn instantiate_typed_body<'db>(
    db: &'db dyn HirAnalysisDb,
    typed_body: TypedBody<'db>,
    generic_args: &[TyId<'db>],
) -> TypedBody<'db> {
    let mut subst = GenericSubst { generic_args };
    typed_body.fold_with(db, &mut subst)
}

struct GenericSubst<'a, 'db> {
    generic_args: &'a [TyId<'db>],
}

impl<'db> TyFolder<'db> for GenericSubst<'_, 'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            TyData::TyParam(param) => self.generic_args.get(param.idx).copied().unwrap_or(ty),
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(param, _) = const_ty.data(db)
                    && let Some(rep) = self.generic_args.get(param.idx).copied()
                {
                    rep
                } else {
                    ty.super_fold_with(db, self)
                }
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}

fn unit_const<'db>(db: &'db dyn HirAnalysisDb) -> ConstTyId<'db> {
    ConstTyId::new(
        db,
        ConstTyData::Evaluated(EvaluatedConstTy::Unit, TyId::unit(db)),
    )
}

fn lit_bool<'db>(db: &'db dyn HirAnalysisDb, value: bool) -> ConstTyId<'db> {
    ConstTyId::new(
        db,
        ConstTyData::Evaluated(EvaluatedConstTy::LitBool(value), TyId::bool(db)),
    )
}

fn lit_int<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, value: BigUint) -> ConstTyId<'db> {
    ConstTyId::new(
        db,
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(IntegerId::new(db, value)), ty),
    )
}

fn lit_bytes<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, bytes: Vec<u8>) -> ConstTyId<'db> {
    ConstTyId::new(
        db,
        ConstTyData::Evaluated(EvaluatedConstTy::Bytes(bytes), ty),
    )
}

fn const_as_bool<'db>(
    db: &'db dyn HirAnalysisDb,
    value: ConstTyId<'db>,
    body: Body<'db>,
    expr: ExprId,
) -> Result<bool, InvalidCause<'db>> {
    match value.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitBool(flag), _) => Ok(*flag),
        _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
    }
}

fn const_as_int<'db>(
    db: &'db dyn HirAnalysisDb,
    value: ConstTyId<'db>,
    body: Body<'db>,
    expr: ExprId,
) -> Result<BigUint, InvalidCause<'db>> {
    match value.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => Ok(int_id.data(db).clone()),
        _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
    }
}

fn const_as_u8<'db>(
    db: &'db dyn HirAnalysisDb,
    value: ConstTyId<'db>,
    body: Body<'db>,
    expr: ExprId,
) -> Result<u8, InvalidCause<'db>> {
    let value = const_as_int(db, value, body, expr)?;
    value
        .to_u8()
        .ok_or(InvalidCause::ConstEvalUnsupported { body, expr })
}

fn const_as_bytes<'db>(
    db: &'db dyn HirAnalysisDb,
    value: ConstTyId<'db>,
    body: Body<'db>,
    expr: ExprId,
) -> Result<Vec<u8>, InvalidCause<'db>> {
    match value.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitBool(flag), _) => Ok(vec![u8::from(*flag)]),
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => {
            let ty = value.ty(db);
            let (bits, _) = int_layout(db, ty, body, expr)?;
            let width = bits / 8;
            let bytes = int_id.data(db).to_bytes_be();
            if bytes.len() > width {
                return Err(InvalidCause::ConstEvalUnsupported { body, expr });
            }
            let mut out = vec![0u8; width];
            let offset = width - bytes.len();
            out[offset..].copy_from_slice(&bytes);
            Ok(out)
        }
        ConstTyData::Evaluated(EvaluatedConstTy::Bytes(bytes), _) => Ok(bytes.clone()),
        ConstTyData::Evaluated(EvaluatedConstTy::Tuple(elems), _) => {
            let mut out = Vec::new();
            for &elem in elems.iter() {
                out.extend(const_as_bytes(
                    db,
                    ty_as_const_ty(db, body, expr, elem)?,
                    body,
                    expr,
                )?);
            }
            Ok(out)
        }
        ConstTyData::Evaluated(EvaluatedConstTy::Record(fields), _) => {
            let mut out = Vec::new();
            for &field in fields.iter() {
                out.extend(const_as_bytes(
                    db,
                    ty_as_const_ty(db, body, expr, field)?,
                    body,
                    expr,
                )?);
            }
            Ok(out)
        }
        ConstTyData::Evaluated(EvaluatedConstTy::Array(elems), _) => {
            let mut out = Vec::new();
            for &elem in elems.iter() {
                out.extend(const_as_bytes(
                    db,
                    ty_as_const_ty(db, body, expr, elem)?,
                    body,
                    expr,
                )?);
            }
            Ok(out)
        }
        _ => Err(InvalidCause::ConstEvalUnsupported { body, expr }),
    }
}

fn ty_as_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr: ExprId,
    ty: TyId<'db>,
) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
    let TyData::ConstTy(const_ty) = ty.data(db) else {
        debug_assert!(false, "ctfe invariant: expected nested TyData::ConstTy");
        return Err(InvalidCause::ConstEvalUnsupported { body, expr });
    };
    Ok(*const_ty)
}

fn eval_cmp<'db>(
    db: &'db dyn HirAnalysisDb,
    operand_ty: TyId<'db>,
    lhs: ConstTyId<'db>,
    rhs: ConstTyId<'db>,
    body: Body<'db>,
    expr: ExprId,
    op: CompBinOp,
) -> Result<ConstTyId<'db>, InvalidCause<'db>> {
    if operand_ty.is_bool(db) {
        let lhs = const_as_bool(db, lhs, body, expr)?;
        let rhs = const_as_bool(db, rhs, body, expr)?;

        let out = match op {
            CompBinOp::Eq => lhs == rhs,
            CompBinOp::NotEq => lhs != rhs,
            CompBinOp::Lt => !lhs && rhs,
            CompBinOp::LtEq => !lhs || rhs,
            CompBinOp::Gt => lhs && !rhs,
            CompBinOp::GtEq => lhs || !rhs,
        };
        return Ok(lit_bool(db, out));
    }

    let (bits, signed) = int_layout(db, operand_ty, body, expr)?;
    let lhs_u = const_as_int(db, lhs, body, expr)?;
    let rhs_u = const_as_int(db, rhs, body, expr)?;

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

    Ok(lit_bool(db, out))
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

fn is_u8_array_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    if !ty.is_array(db) {
        return false;
    }

    let (_, args) = ty.decompose_ty_app(db);
    matches!(args.first().copied(), Some(elem) if is_u8_ty(db, elem))
}

fn is_u8_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    matches!(
        ty.base_ty(db).data(db),
        TyData::TyBase(TyBase::Prim(PrimTy::U8))
    )
}

fn array_len<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<usize> {
    assert!(ty.is_array(db));

    let (_, args) = ty.decompose_ty_app(db);
    const_ty_to_usize(db, *args.get(1).unwrap())
}

fn const_ty_to_usize<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<usize> {
    let TyData::ConstTy(const_ty) = ty.data(db) else {
        return None;
    };

    match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => int_id.data(db).to_usize(),
        _ => None,
    }
}
