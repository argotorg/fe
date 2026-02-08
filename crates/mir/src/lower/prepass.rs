//! Prepass utilities for MIR lowering: ensures expressions have values and resolves consts.

use super::*;
use hir::analysis::name_resolution::{PathRes, resolve_path};
use hir::analysis::ty::const_eval::{ConstValue, try_eval_const_body, try_eval_const_ref};
use hir::analysis::ty::const_ty::{ConstTyData, EvaluatedConstTy};
use hir::analysis::ty::trait_resolution::PredicateListId;

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Helper to iterate expressions and conditionally force value lowering.
    ///
    /// # Parameters
    /// - `predicate`: Predicate that selects which expressions to visit.
    /// - `ensure`: Callback invoked for each matching expression to perform lowering.
    ///
    /// # Returns
    /// Nothing; mutates internal caches for selected expressions.
    pub(super) fn ensure_expr_values<P, F>(&mut self, predicate: P, mut ensure: F)
    where
        P: Fn(&Expr<'db>) -> bool,
        F: FnMut(&mut Self, ExprId),
    {
        let exprs = self.body.exprs(self.db);
        for expr_id in exprs.keys() {
            let Partial::Present(expr) = &exprs[expr_id] else {
                continue;
            };
            if predicate(expr) {
                ensure(self, expr_id);
            }
        }
    }

    /// Forces all const path expressions to lower into synthetic literals.
    ///
    /// # Returns
    /// Nothing; caches literal `ValueId`s for const paths.
    pub(super) fn ensure_const_expr_values(&mut self) {
        self.ensure_expr_values(
            |expr| matches!(expr, Expr::Path(..)),
            |this, expr_id| {
                if let Some(value_id) = this.try_const_expr(expr_id) {
                    this.builder
                        .body
                        .expr_values
                        .entry(expr_id)
                        .or_insert(value_id);
                }
            },
        );
    }

    /// Ensure that the given expression has a corresponding MIR value.
    ///
    /// # Parameters
    /// - `expr`: Expression to materialize into a value.
    ///
    /// # Returns
    /// The `ValueId` bound to the expression.
    pub(super) fn ensure_value(&mut self, expr: ExprId) -> ValueId {
        if let Some(&val) = self.builder.body.expr_values.get(&expr) {
            return val;
        }

        let value = match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => {
                let last_expr = stmts.iter().rev().find_map(|stmt_id| {
                    let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
                        return None;
                    };
                    if let Stmt::Expr(expr_id) = stmt {
                        Some(*expr_id)
                    } else {
                        None
                    }
                });
                if let Some(inner) = last_expr {
                    let val = self.ensure_value(inner);
                    self.builder.body.expr_values.insert(expr, val);
                    return val;
                }
                self.alloc_expr_value(expr)
            }
            _ => self.alloc_expr_value(expr),
        };

        self.builder.body.expr_values.insert(expr, value);
        value
    }

    /// Allocate the MIR value slot for an expression, handling special cases.
    ///
    /// # Parameters
    /// - `expr`: Expression to allocate a value for.
    ///
    /// # Returns
    /// The allocated `ValueId` (lowered call/field/const where applicable).
    pub(super) fn alloc_expr_value(&mut self, expr: ExprId) -> ValueId {
        if let Some(value) = self.try_const_expr(expr) {
            return value;
        }

        let ty = self.typed_body.expr_ty(self.db, expr);
        let mut repr = self.value_repr_for_expr(expr, ty);
        let origin = match expr.data(self.db, self.body) {
            Partial::Present(Expr::Lit(LitKind::Int(int_id))) => {
                ValueOrigin::Synthetic(SyntheticValue::Int(int_id.data(self.db).clone()))
            }
            Partial::Present(Expr::Lit(LitKind::Bool(flag))) => {
                ValueOrigin::Synthetic(SyntheticValue::Bool(*flag))
            }
            Partial::Present(Expr::Lit(LitKind::String(str_id))) => ValueOrigin::Synthetic(
                SyntheticValue::Bytes(str_id.data(self.db).as_bytes().to_vec()),
            ),
            Partial::Present(Expr::Path(_)) => {
                let expr_prop = self.typed_body.expr_prop(self.db, expr);
                if let Some(binding) = expr_prop.binding {
                    if self
                        .hir_func
                        .is_some_and(|func| extract_contract_function(self.db, func).is_some())
                        && matches!(binding, LocalBinding::EffectParam { .. })
                    {
                        // TODO: document/enforce this rule:
                        //   effect params on contract_init/contract_runtime must be zero-sized concrete types
                        debug_assert!(
                            crate::layout::ty_size_bytes(self.db, ty) == Some(0),
                            "contract entrypoint effect params must be concrete zero-sized providers; got `{}`",
                            ty.pretty_print(self.db)
                        );
                        ValueOrigin::Unit
                    } else if let Some(target) = self.code_region_target_from_ty(ty) {
                        ValueOrigin::FuncItem(target)
                    } else if let Some(local) = self.local_for_binding(binding) {
                        if self.effect_param_key_is_type(binding)
                            && matches!(repr, ValueRepr::Word)
                            && !crate::layout::is_zero_sized_ty(self.db, ty)
                        {
                            // Type-effect params are addressable providers; even when their runtime
                            // representation is a single word, treat them as `Ref` roots so lowering
                            // can emit `Place`-based loads/stores (including transparent newtype
                            // peeling over field-0 projections).
                            repr = ValueRepr::Ref(self.address_space_for_binding(&binding));
                        }
                        ValueOrigin::Local(local)
                    } else if let Some(target) = self.code_region_target(expr) {
                        ValueOrigin::FuncItem(target)
                    } else {
                        ValueOrigin::Expr(expr)
                    }
                } else if let Some(target) = self.code_region_target(expr) {
                    ValueOrigin::FuncItem(target)
                } else {
                    ValueOrigin::Expr(expr)
                }
            }
            Partial::Present(Expr::Un(inner, op)) => ValueOrigin::Unary {
                op: *op,
                inner: self.ensure_value(*inner),
            },
            Partial::Present(Expr::Cast(inner, _)) => ValueOrigin::TransparentCast {
                value: self.ensure_value(*inner),
            },
            Partial::Present(Expr::Bin(lhs, rhs, op)) => ValueOrigin::Binary {
                op: *op,
                lhs: self.ensure_value(*lhs),
                rhs: self.ensure_value(*rhs),
            },
            Partial::Present(Expr::If(..) | Expr::Match(..)) => {
                ValueOrigin::ControlFlowResult { expr }
            }
            Partial::Present(Expr::Block(..)) => ValueOrigin::Unit,
            _ if ty.is_tuple(self.db) && ty.field_count(self.db) == 0 => ValueOrigin::Unit,
            _ => ValueOrigin::Expr(expr),
        };

        self.builder
            .body
            .alloc_value(ValueData { ty, origin, repr })
    }

    /// Collect all argument expressions and their lowered values for a call or method call.
    ///
    /// # Parameters
    /// - `expr`: Expression id representing the call or method call.
    ///
    /// # Returns
    /// A tuple of lowered argument `ValueId`s and their corresponding `ExprId`s.
    pub(super) fn collect_call_args(
        &mut self,
        expr: ExprId,
    ) -> Option<(Vec<ValueId>, Vec<ExprId>)> {
        let exprs = self.body.exprs(self.db);
        let Partial::Present(expr_data) = &exprs[expr] else {
            return None;
        };
        match expr_data {
            Expr::Call(_, call_args) => {
                let mut args = Vec::with_capacity(call_args.len());
                let mut arg_exprs = Vec::with_capacity(call_args.len());
                for arg in call_args.iter() {
                    arg_exprs.push(arg.expr);
                    args.push(self.lower_expr(arg.expr));
                }
                Some((args, arg_exprs))
            }
            Expr::MethodCall(receiver, _, _, call_args) => {
                let mut args = Vec::with_capacity(call_args.len() + 1);
                let mut arg_exprs = Vec::with_capacity(call_args.len() + 1);
                arg_exprs.push(*receiver);
                args.push(self.lower_expr(*receiver));
                for arg in call_args.iter() {
                    arg_exprs.push(arg.expr);
                    args.push(self.lower_expr(arg.expr));
                }
                Some((args, arg_exprs))
            }
            _ => None,
        }
    }

    /// Attempts to resolve a path expression to a literal `const` value.
    ///
    /// # Parameters
    /// - `expr`: Path expression to resolve.
    ///
    /// # Returns
    /// A MIR `ValueId` referencing a synthetic literal when successful.
    pub(super) fn try_const_expr(&mut self, expr: ExprId) -> Option<ValueId> {
        let Partial::Present(Expr::Path(path)) = expr.data(self.db, self.body) else {
            return None;
        };
        let path = path.to_opt()?;

        if let Some(cref) = self.typed_body.expr_const_ref(expr) {
            if let hir::analysis::ty::ty_check::ConstRef::Const(const_def) = cref
                && let Some(&cached) = self.const_cache.get(&const_def)
            {
                return Some(cached);
            }

            let ty = self.typed_body.expr_ty(self.db, expr);
            let value = match try_eval_const_ref(self.db, cref, ty)? {
                ConstValue::Int(int) => SyntheticValue::Int(int),
                ConstValue::Bool(flag) => SyntheticValue::Bool(flag),
                ConstValue::Bytes(bytes) => SyntheticValue::Bytes(bytes),
            };

            let value_id = self.alloc_synthetic_value(ty, value);
            if let hir::analysis::ty::ty_check::ConstRef::Const(const_def) = cref {
                self.const_cache.insert(const_def, value_id);
            }
            return Some(value_id);
        }

        // Const generic parameter (e.g. `const SALT: u256`).
        if self.generic_args.is_empty() {
            return None;
        }
        if self.typed_body.expr_prop(self.db, expr).binding.is_some() {
            return None;
        }

        let assumptions = PredicateListId::empty_list(self.db);
        let resolved = resolve_path(self.db, path, self.body.scope(), assumptions, true).ok()?;
        let ty = match resolved {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => ty,
            _ => return None,
        };

        let TyData::ConstTy(const_ty) = ty.data(self.db) else {
            return None;
        };
        let ConstTyData::TyParam(param, _) = const_ty.data(self.db) else {
            return None;
        };
        let arg = *self.generic_args.get(param.idx)?;
        let TyData::ConstTy(const_arg) = arg.data(self.db) else {
            return None;
        };

        let expected_ty = self.typed_body.expr_ty(self.db, expr);
        let value = match const_arg.data(self.db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), _) => {
                SyntheticValue::Int(value.data(self.db).clone())
            }
            ConstTyData::Evaluated(EvaluatedConstTy::LitBool(flag), _) => {
                SyntheticValue::Bool(*flag)
            }
            ConstTyData::UnEvaluated { body, .. } => {
                match try_eval_const_body(self.db, *body, expected_ty)? {
                    ConstValue::Int(value) => SyntheticValue::Int(value),
                    ConstValue::Bool(flag) => SyntheticValue::Bool(flag),
                    ConstValue::Bytes(bytes) => SyntheticValue::Bytes(bytes),
                }
            }
            _ => return None,
        };

        Some(self.alloc_synthetic_value(expected_ty, value))
    }

    /// Allocates a synthetic literal value with the provided type.
    ///
    /// # Parameters
    /// - `ty`: Type of the synthetic literal.
    /// - `value`: Literal content to store.
    ///
    /// # Returns
    /// The new `ValueId` stored in the MIR body.
    pub(super) fn alloc_synthetic_value(
        &mut self,
        ty: TyId<'db>,
        value: SyntheticValue,
    ) -> ValueId {
        self.builder.body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Synthetic(value),
            repr: ValueRepr::Word,
        })
    }
}
