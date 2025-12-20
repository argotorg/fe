//! Prepass utilities for MIR lowering: ensures expressions have values and resolves consts.

use super::*;
use hir::analysis::ty::trait_def::assoc_const_body_for_trait_inst;
use num_traits::ToPrimitive;

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

    /// Forces all field expressions to have associated MIR values.
    ///
    /// # Returns
    /// Nothing; fills `expr_values` for field expressions.
    pub(super) fn ensure_field_expr_values(&mut self) {
        self.ensure_expr_values(
            |expr| matches!(expr, Expr::Field(..) | Expr::Bin(_, _, BinOp::Index)),
            |this, expr_id| {
                if let Some(value_id) = this.try_lower_field(expr_id) {
                    this.mir_body.expr_values.entry(expr_id).or_insert(value_id);
                }
            },
        );
    }

    /// Forces all call expressions (including method calls) to have associated MIR values.
    ///
    /// This is required for codegen, which only supports lowering call targets once they've been
    /// rewritten into MIR `Call`/`Intrinsic`/`Synthetic` values.
    pub(super) fn ensure_call_expr_values(&mut self) {
        self.ensure_expr_values(
            |expr| matches!(expr, Expr::Call(..) | Expr::MethodCall(..)),
            |this, expr_id| {
                this.ensure_value(expr_id);
            },
        );
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
            if !self.value_address_space.contains_key(&val) {
                let space = self.expr_address_space(expr);
                self.value_address_space.insert(val, space);
            }
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
        // Note: record_value_address_space is already called in alloc_expr_value.
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
        if let Some(value) = self.try_lower_call(expr) {
            self.record_value_address_space(expr, value);
            return value;
        }
        if let Some(value) = self.try_lower_field(expr) {
            self.record_value_address_space(expr, value);
            return value;
        }
        if let Some(value) = self.try_lower_index(expr) {
            self.record_value_address_space(expr, value);
            return value;
        }
        if let Some(value) = self.try_const_expr(expr) {
            self.record_value_address_space(expr, value);
            return value;
        }

        let ty = self.typed_body.expr_ty(self.db, expr);
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
                if let Some(binding) = self.typed_body.expr_prop(self.db, expr).binding
                    && let Some(local) = self.local_for_binding(binding)
                {
                    ValueOrigin::Local(local)
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

        let value = self.builder.body.alloc_value(ValueData { ty, origin });
        self.record_value_address_space(expr, value);
        value
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
                    args.push(self.ensure_value(arg.expr));
                }
                Some((args, arg_exprs))
            }
            Expr::MethodCall(receiver, _, _, call_args) => {
                let mut args = Vec::with_capacity(call_args.len() + 1);
                let mut arg_exprs = Vec::with_capacity(call_args.len() + 1);
                arg_exprs.push(*receiver);
                args.push(self.ensure_value(*receiver));
                for arg in call_args.iter() {
                    arg_exprs.push(arg.expr);
                    args.push(self.ensure_value(arg.expr));
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
        let mut visited = FxHashSet::default();
        self.const_literal_from_path(path, self.body.scope(), &mut visited)
    }

    pub(super) fn const_usize_from_body(&mut self, body: Body<'db>) -> Option<usize> {
        let expr_id = body.expr(self.db);
        let expr = match expr_id.data(self.db, body) {
            Partial::Present(expr) => expr,
            Partial::Absent => return None,
        };
        match expr {
            Expr::Lit(LitKind::Int(value)) => value.data(self.db).to_usize(),
            Expr::Path(path) => {
                let path = path.to_opt()?;
                let mut visited = FxHashSet::default();
                let value_id = self.const_literal_from_path(path, body.scope(), &mut visited)?;
                match &self.builder.body.value(value_id).origin {
                    ValueOrigin::Synthetic(SyntheticValue::Int(int)) => int.to_usize(),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Resolves the given path to a const definition in `scope` and lowers it to a literal.
    ///
    /// # Parameters
    /// - `path`: Path to resolve.
    /// - `scope`: Scope used for resolution.
    /// - `visited`: Set used to detect const evaluation cycles.
    ///
    /// # Returns
    /// The literal `ValueId` if resolution succeeds.
    fn const_literal_from_path(
        &mut self,
        path: PathId<'db>,
        scope: ScopeId<'db>,
        visited: &mut FxHashSet<Const<'db>>,
    ) -> Option<ValueId> {
        match resolve_path(
            self.db,
            path,
            scope,
            PredicateListId::empty_list(self.db),
            true,
        )
        .ok()?
        {
            PathRes::Const(const_def, ty) => self.const_literal_from_def(const_def, ty, visited),
            PathRes::TraitConst(ty, trait_inst, const_name) => {
                self.trait_const_literal_from_inst(ty, trait_inst, const_name, visited)
            }
            _ => None,
        }
    }

    fn trait_const_literal_from_inst(
        &mut self,
        ty: TyId<'db>,
        trait_inst: hir::analysis::ty::trait_def::TraitInstId<'db>,
        const_name: hir::hir_def::IdentId<'db>,
        visited: &mut FxHashSet<Const<'db>>,
    ) -> Option<ValueId> {
        let body = assoc_const_body_for_trait_inst(self.db, trait_inst, const_name)?;
        let expr_id = body.expr(self.db);
        let expr = match expr_id.data(self.db, body) {
            Partial::Present(expr) => expr,
            Partial::Absent => return None,
        };

        let const_scope = body.scope();
        match expr {
            Expr::Lit(LitKind::Int(value)) => Some(
                self.alloc_synthetic_value(ty, SyntheticValue::Int(value.data(self.db).clone())),
            ),
            Expr::Lit(LitKind::Bool(flag)) => {
                Some(self.alloc_synthetic_value(ty, SyntheticValue::Bool(*flag)))
            }
            Expr::Path(path) => path
                .to_opt()
                .and_then(|inner| self.const_literal_from_path(inner, const_scope, visited)),
            _ => None,
        }
    }

    /// Converts a concrete const definition into a MIR literal value.
    ///
    /// # Parameters
    /// - `const_def`: Const definition to evaluate.
    /// - `ty`: Type of the const.
    /// - `visited`: Set used to detect const evaluation cycles.
    ///
    /// # Returns
    /// Cached or newly allocated `ValueId` for the literal, or `None` on failure.
    fn const_literal_from_def(
        &mut self,
        const_def: Const<'db>,
        ty: TyId<'db>,
        visited: &mut FxHashSet<Const<'db>>,
    ) -> Option<ValueId> {
        if let Some(&value) = self.const_cache.get(&const_def) {
            return Some(value);
        }
        if !visited.insert(const_def) {
            return None;
        }
        let body = const_def.body(self.db).to_opt()?;
        let expr_id = body.expr(self.db);
        let expr = match expr_id.data(self.db, body) {
            Partial::Present(expr) => expr,
            Partial::Absent => {
                visited.remove(&const_def);
                return None;
            }
        };
        let const_scope = body.scope();
        let result = match expr {
            Expr::Lit(LitKind::Int(value)) => Some(
                self.alloc_synthetic_value(ty, SyntheticValue::Int(value.data(self.db).clone())),
            ),
            Expr::Lit(LitKind::Bool(flag)) => {
                Some(self.alloc_synthetic_value(ty, SyntheticValue::Bool(*flag)))
            }
            Expr::Path(path) => {
                if let Some(inner_path) = path.to_opt() {
                    self.const_literal_from_path(inner_path, const_scope, visited)
                } else {
                    None
                }
            }
            _ => None,
        };
        visited.remove(&const_def);
        if let Some(value_id) = result {
            self.const_cache.insert(const_def, value_id);
        }
        result
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
        })
    }
}
