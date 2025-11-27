//! Expression and value lowering helpers shared across the Yul emitter.

use hir::hir_def::{
    CallableDef, Expr, ExprId, LitKind, Stmt, StmtId,
    expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp},
};
use mir::{CallOrigin, ValueId, ValueOrigin, ir::SyntheticValue};

use crate::yul::{doc::YulDoc, state::BlockState};

use super::{
    YulError,
    function::FunctionEmitter,
    util::{function_name, try_collapse_cast_shim},
};

impl<'db> FunctionEmitter<'db> {
    /// Lowers a MIR `ValueId` into a Yul expression string.
    ///
    /// * `value_id` - Identifier selecting the MIR value.
    /// * `state` - Current bindings for previously-evaluated expressions.
    ///
    /// Returns the Yul expression referencing the value or an error if unsupported.
    pub(super) fn lower_value(
        &self,
        value_id: ValueId,
        state: &BlockState,
    ) -> Result<String, YulError> {
        // Check if this value was already bound to a temp in the current scope
        if let Some(temp) = state.value_temp(value_id.index()) {
            return Ok(temp.clone());
        }
        let value = self.mir_func.body.value(value_id);
        match &value.origin {
            ValueOrigin::Expr(expr_id) => {
                if let Some(temp) = self.match_values.get(expr_id) {
                    Ok(temp.clone())
                } else {
                    self.lower_expr(*expr_id, state)
                }
            }
            ValueOrigin::Call(call) => self.lower_call_value(call, state),
            ValueOrigin::Intrinsic(intr) => self.lower_intrinsic_value(intr, state),
            ValueOrigin::Synthetic(synth) => self.lower_synthetic_value(synth),
            _ => Err(YulError::Unsupported(
                "only expression-derived values are supported".into(),
            )),
        }
    }

    /// Lowers a HIR expression into a Yul expression string.
    ///
    /// * `expr_id` - Expression to render.
    /// * `state` - Binding state used for nested expressions.
    ///
    /// Returns the fully-lowered Yul expression.
    pub(super) fn lower_expr(
        &self,
        expr_id: ExprId,
        state: &BlockState,
    ) -> Result<String, YulError> {
        if let Some(temp) = self.expr_temps.get(&expr_id) {
            return Ok(temp.clone());
        }
        if let Some(temp) = self.match_values.get(&expr_id) {
            return Ok(temp.clone());
        }
        if let Some(value_id) = self.mir_func.body.expr_values.get(&expr_id) {
            let value = self.mir_func.body.value(*value_id);
            match &value.origin {
                ValueOrigin::Call(call) => return self.lower_call_value(call, state),
                ValueOrigin::Synthetic(synth) => {
                    return self.lower_synthetic_value(synth);
                }
                _ => {}
            }
        }

        let expr = self.expect_expr(expr_id)?;
        match expr {
            Expr::Lit(LitKind::Int(int_id)) => Ok(int_id.data(self.db).to_string()),
            Expr::Lit(LitKind::Bool(value)) => Ok(if *value { "1" } else { "0" }.into()),
            Expr::Lit(LitKind::String(str_id)) => Ok(format!(
                "0x{}",
                hex::encode(str_id.data(self.db).as_bytes())
            )),
            Expr::Un(inner, op) => {
                let value = self.lower_expr(*inner, state)?;
                match op {
                    UnOp::Minus => Ok(format!("sub(0, {value})")),
                    UnOp::Not => Ok(format!("iszero({value})")),
                    UnOp::Plus => Ok(value),
                    UnOp::BitNot => Ok(format!("not({value})")),
                }
            }
            Expr::Tuple(values) => {
                let parts = values
                    .iter()
                    .map(|expr| self.lower_expr(*expr, state))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(format!("tuple({})", parts.join(", ")))
            }
            Expr::Call(callee, call_args) => {
                let callee_expr = self.lower_expr(*callee, state)?;
                let mut lowered_args = Vec::with_capacity(call_args.len());
                for arg in call_args {
                    lowered_args.push(self.lower_expr(arg.expr, state)?);
                }
                if let Some(arg) = try_collapse_cast_shim(&callee_expr, &lowered_args)? {
                    return Ok(arg);
                }
                if lowered_args.is_empty() {
                    Ok(format!("{callee_expr}()"))
                } else {
                    Ok(format!("{callee_expr}({})", lowered_args.join(", ")))
                }
            }
            Expr::Bin(lhs, rhs, bin_op) => match bin_op {
                BinOp::Arith(op) => {
                    let left = self.lower_expr(*lhs, state)?;
                    let right = self.lower_expr(*rhs, state)?;
                    match op {
                        ArithBinOp::Add => Ok(format!("add({left}, {right})")),
                        ArithBinOp::Sub => Ok(format!("sub({left}, {right})")),
                        ArithBinOp::Mul => Ok(format!("mul({left}, {right})")),
                        ArithBinOp::Div => Ok(format!("div({left}, {right})")),
                        ArithBinOp::Rem => Ok(format!("mod({left}, {right})")),
                        ArithBinOp::Pow => Ok(format!("exp({left}, {right})")),
                        ArithBinOp::LShift => Ok(format!("shl({right}, {left})")),
                        ArithBinOp::RShift => Ok(format!("shr({right}, {left})")),
                        ArithBinOp::BitAnd => Ok(format!("and({left}, {right})")),
                        ArithBinOp::BitOr => Ok(format!("or({left}, {right})")),
                        ArithBinOp::BitXor => Ok(format!("xor({left}, {right})")),
                    }
                }
                BinOp::Comp(op) => {
                    let left = self.lower_expr(*lhs, state)?;
                    let right = self.lower_expr(*rhs, state)?;
                    let expr = match op {
                        CompBinOp::Eq => format!("eq({left}, {right})"),
                        CompBinOp::NotEq => format!("iszero(eq({left}, {right}))"),
                        CompBinOp::Lt => format!("lt({left}, {right})"),
                        CompBinOp::LtEq => format!("iszero(gt({left}, {right}))"),
                        CompBinOp::Gt => format!("gt({left}, {right})"),
                        CompBinOp::GtEq => format!("iszero(lt({left}, {right}))"),
                    };
                    Ok(expr)
                }
                BinOp::Logical(op) => {
                    let left = self.lower_expr(*lhs, state)?;
                    let right = self.lower_expr(*rhs, state)?;
                    let func = match op {
                        LogicalBinOp::And => "and",
                        LogicalBinOp::Or => "or",
                    };
                    Ok(format!("{func}({left}, {right})"))
                }
                _ => Err(YulError::Unsupported(
                    "only arithmetic/logical binary expressions are supported right now".into(),
                )),
            },
            Expr::Block(stmts) => {
                if let Some(expr) = self.last_expr(stmts) {
                    self.lower_expr(expr, state)
                } else {
                    Ok("0".into())
                }
            }
            Expr::Path(path) => {
                let original = self
                    .path_ident(*path)
                    .ok_or_else(|| YulError::Unsupported("unsupported path expression".into()))?;
                Ok(state.resolve_name(&original))
            }
            Expr::Field(..) => {
                if let Some(value_id) = self.mir_func.body.expr_values.get(&expr_id) {
                    self.lower_value(*value_id, state)
                } else {
                    let ty = self.mir_func.typed_body.expr_ty(self.db, expr_id);
                    Err(YulError::Unsupported(format!(
                        "field expressions should be rewritten before codegen (expr type {})",
                        ty.pretty_print(self.db)
                    )))
                }
            }
            Expr::RecordInit(..) => {
                if let Some(temp) = self.expr_temps.get(&expr_id) {
                    Ok(temp.clone())
                } else {
                    Err(YulError::Unsupported(
                        "record initializers should be lowered before codegen".into(),
                    ))
                }
            }
            other => Err(YulError::Unsupported(format!(
                "only simple expressions are supported: {other:?}"
            ))),
        }
    }

    /// Returns the last expression statement in a block, if any.
    ///
    /// * `stmts` - Slice of statement IDs to inspect.
    ///
    /// Returns the expression ID for the trailing expression statement when present.
    fn last_expr(&self, stmts: &[StmtId]) -> Option<ExprId> {
        stmts.iter().rev().find_map(|stmt_id| {
            let Ok(stmt) = self.expect_stmt(*stmt_id) else {
                return None;
            };
            if let Stmt::Expr(expr) = stmt {
                Some(*expr)
            } else {
                None
            }
        })
    }

    /// Lowers a MIR call into a Yul function invocation.
    ///
    /// * `call` - Call origin describing the callee and arguments.
    /// * `state` - Binding state used to lower argument expressions.
    ///
    /// Returns the Yul invocation string for the call.
    pub(super) fn lower_call_value(
        &self,
        call: &CallOrigin<'_>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        let callee = if let Some(name) = &call.resolved_name {
            name.clone()
        } else {
            match call.callable.callable_def {
                CallableDef::Func(func) => function_name(self.db, func),
                CallableDef::VariantCtor(_) => {
                    return Err(YulError::Unsupported(
                        "callable without hir function definition is not supported yet".into(),
                    ));
                }
            }
        };
        let mut lowered_args = Vec::with_capacity(call.args.len());
        for &arg in &call.args {
            lowered_args.push(self.lower_value(arg, state)?);
        }
        if let Some(arg) = try_collapse_cast_shim(&callee, &lowered_args)? {
            return Ok(arg);
        }
        if lowered_args.is_empty() {
            Ok(format!("{callee}()"))
        } else {
            Ok(format!("{callee}({})", lowered_args.join(", ")))
        }
    }

    /// Lowers special MIR synthetic values such as constants into Yul expressions.
    ///
    /// * `value` - Synthetic value emitted during MIR construction.
    ///
    /// Returns the literal Yul expression for the synthetic value.
    fn lower_synthetic_value(&self, value: &SyntheticValue) -> Result<String, YulError> {
        match value {
            SyntheticValue::Int(int) => Ok(int.to_string()),
            SyntheticValue::Bool(flag) => Ok(if *flag { "1" } else { "0" }.into()),
        }
    }

    /// Lowers expressions that may require extra statements (e.g. `if`).
    ///
    /// * `expr_id` - Expression to lower.
    /// * `docs` - Doc list to append emitted statements into.
    /// * `state` - Binding state for allocating temporaries.
    ///
    /// Returns either the inline expression or the name of a temporary containing the result.
    pub(super) fn lower_expr_with_statements(
        &mut self,
        expr_id: ExprId,
        docs: &mut Vec<YulDoc>,
        state: &mut BlockState,
    ) -> Result<String, YulError> {
        if let Some(temp) = self.expr_temps.get(&expr_id) {
            return Ok(temp.clone());
        }
        if let Some(temp) = self.match_values.get(&expr_id) {
            return Ok(temp.clone());
        }

        let expr = self.expect_expr(expr_id)?;
        if let Expr::If(cond, then_expr, else_expr) = expr {
            let temp = state.alloc_local();
            docs.push(YulDoc::line(format!("let {temp} := 0")));
            let cond_expr = self.lower_expr(*cond, state)?;
            let then_expr_str = self.lower_expr(*then_expr, state)?;
            docs.push(YulDoc::block(
                format!("if {cond_expr} "),
                vec![YulDoc::line(format!("{temp} := {then_expr_str}"))],
            ));
            if let Some(else_expr) = else_expr {
                let else_expr_str = self.lower_expr(*else_expr, state)?;
                docs.push(YulDoc::block(
                    format!("if iszero({cond_expr}) "),
                    vec![YulDoc::line(format!("{temp} := {else_expr_str}"))],
                ));
            }
            Ok(temp)
        } else {
            self.lower_expr(expr_id, state)
        }
    }

    /// Returns `true` when the given expression's type is the unit tuple.
    ///
    /// * `expr_id` - Expression identifier whose type should be tested.
    ///
    /// Returns `true` if the expression's type is the unit tuple.
    pub(super) fn expr_is_unit(&self, expr_id: ExprId) -> bool {
        let ty = self.mir_func.typed_body.expr_ty(self.db, expr_id);
        ty.is_tuple(self.db) && ty.field_count(self.db) == 0
    }
}
