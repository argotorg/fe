use driver::DriverDataBase;
use hir::hir_def::{
    Body, Contract, Expr, ExprId, Func, LitKind, Partial, Pat, PatId, PathId, Stmt, StmtId,
    TopLevelMod,
    expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp},
};
use mir::{
    CallOrigin, MirFunction, Terminator, ValueId, ValueOrigin, ir::SyntheticValue, lower_module,
};
use rustc_hash::FxHashMap;

use super::{
    doc::{YulDoc, render_docs},
    errors::YulError,
    state::BlockState,
};

mod control_flow;
mod statements;

#[derive(Debug)]
pub enum EmitModuleError {
    MirLower(mir::MirLowerError),
    Yul(YulError),
}

impl std::fmt::Display for EmitModuleError {
    /// Formats the underlying MIR or Yul error for user-facing diagnostics.
    ///
    /// * `f` - Target formatter supplied by the caller.
    ///
    /// Returns the formatting result from the standard library.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EmitModuleError::MirLower(err) => write!(f, "{err}"),
            EmitModuleError::Yul(err) => write!(f, "{err}"),
        }
    }
}

impl std::error::Error for EmitModuleError {}

/// Emits Yul for every function in the lowered MIR module.
///
/// * `db` - Driver database used to query compiler facts.
/// * `top_mod` - Root module to lower.
///
/// Returns a single Yul string containing all lowered functions followed by any
/// auto-generated contract runtimes, or [`EmitModuleError`] if MIR lowering or
/// Yul emission fails.
pub fn emit_module_yul(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<String, EmitModuleError> {
    let module = lower_module(db, top_mod).map_err(EmitModuleError::MirLower)?;
    let mut function_docs = Vec::new();
    for func in &module.functions {
        let emitter = YulEmitter::new(db, func).map_err(EmitModuleError::Yul)?;
        function_docs.extend(emitter.emit_doc().map_err(EmitModuleError::Yul)?);
    }
    let mut docs = Vec::new();
    let mut contract_docs = emit_contract_runtimes(db, top_mod, &function_docs);
    if contract_docs.is_empty() {
        docs = function_docs;
    } else {
        for mut contract_doc in contract_docs.drain(..) {
            docs.append(&mut contract_doc);
        }
    }
    let mut lines = Vec::new();
    render_docs(&docs, 0, &mut lines);
    Ok(join_lines(lines))
}

/// Builds Yul doc trees for every contract found in `top_mod`.
///
/// * `db` - Compiler database providing HIR access.
/// * `top_mod` - Module containing potential contract definitions.
///
/// Returns a vector of doc lists, one per contract, ready to append to the
/// module output.
fn emit_contract_runtimes(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    functions: &[YulDoc],
) -> Vec<Vec<YulDoc>> {
    top_mod
        .all_contracts(db)
        .iter()
        .copied()
        .filter_map(|contract| contract_runtime_object(db, contract, functions))
        .collect()
}

/// Converts a HIR contract into the doc tree describing its constructor/runtime object.
///
/// * `db` - Compiler database used to resolve the contract name.
/// * `contract` - Target contract to wrap.
///
/// Returns the constructed doc tree or `None` when the contract lacks a resolvable name.
fn contract_runtime_object(
    db: &DriverDataBase,
    contract: Contract<'_>,
    functions: &[YulDoc],
) -> Option<Vec<YulDoc>> {
    let name = contract
        .name(db)
        .to_opt()
        .map(|ident| ident.data(db).to_string())?;
    Some(render_contract_runtime_docs(&name, functions))
}

/// Creates a Yul doc tree describing the dispatcher-based runtime wrapper for `name`.
///
/// * `name` - Contract identifier used for the Yul `object`.
///
/// Returns the doc list containing constructor and runtime sections.
fn render_contract_runtime_docs(name: &str, functions: &[YulDoc]) -> Vec<YulDoc> {
    let mut runtime_body = Vec::new();
    if !functions.is_empty() {
        runtime_body.extend(functions.iter().cloned());
        runtime_body.push(YulDoc::line(String::new()));
    }
    // TODO: This is just temporary until we have a real dispatcher implementation.
    runtime_body.push(YulDoc::line("pop(dispatch())"));
    runtime_body.push(YulDoc::line("stop()"));
    vec![YulDoc::block(
        format!("object \"{name}\" "),
        vec![
            YulDoc::block(
                "code ",
                vec![
                    YulDoc::line("datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))"),
                    YulDoc::line("return(0, datasize(\"runtime\"))"),
                ],
            ),
            YulDoc::line(String::new()),
            YulDoc::block(
                "object \"runtime\" ",
                vec![YulDoc::block("code ", runtime_body)],
            ),
        ],
    )]
}

/// Joins rendered lines while trimming trailing whitespace-only entries.
///
/// * `lines` - Vector of rendered Yul lines.
///
/// Returns the normalized Yul output string.
fn join_lines(mut lines: Vec<String>) -> String {
    while lines.last().is_some_and(|line| line.is_empty()) {
        lines.pop();
    }
    lines.join("\n")
}

/// Lowers a single MIR function into the Yul document tree.
struct YulEmitter<'db> {
    db: &'db DriverDataBase,
    mir_func: &'db MirFunction<'db>,
    body: Body<'db>,
    /// Temporaries allocated for expression values that must be re-used later (e.g. struct ptrs).
    expr_temps: FxHashMap<ExprId, String>,
    match_values: FxHashMap<ExprId, String>,
    /// Number of MIR references per value so we can avoid evaluating them twice.
    value_use_counts: Vec<usize>,
}

impl<'db> YulEmitter<'db> {
    /// Constructs a new emitter for the given MIR function.
    ///
    /// * `db` - Driver database providing access to bodies and type info.
    /// * `mir_func` - MIR function to lower into Yul.
    ///
    /// Returns the initialized emitter or [`YulError::MissingBody`] if the
    /// function lacks a body.
    fn new(db: &'db DriverDataBase, mir_func: &'db MirFunction<'db>) -> Result<Self, YulError> {
        let body = mir_func
            .func
            .body(db)
            .ok_or_else(|| YulError::MissingBody(function_name(db, mir_func.func)))?;
        let value_use_counts = Self::collect_value_use_counts(&mir_func.body);
        Ok(Self {
            db,
            mir_func,
            body,
            expr_temps: FxHashMap::default(),
            match_values: FxHashMap::default(),
            value_use_counts,
        })
    }

    /// Counts how many MIR instructions/terminators use each `ValueId`.
    fn collect_value_use_counts(body: &mir::MirBody<'db>) -> Vec<usize> {
        let mut counts = vec![0; body.values.len()];
        for block in &body.blocks {
            for inst in &block.insts {
                match inst {
                    mir::MirInst::Let { value, .. } => {
                        if let Some(value) = value {
                            counts[value.index()] += 1;
                        }
                    }
                    mir::MirInst::Assign { value, .. }
                    | mir::MirInst::AugAssign { value, .. }
                    | mir::MirInst::Eval { value, .. }
                    | mir::MirInst::EvalExpr { value, .. } => {
                        counts[value.index()] += 1;
                    }
                    mir::MirInst::IntrinsicStmt { args, .. } => {
                        for arg in args {
                            counts[arg.index()] += 1;
                        }
                    }
                }
            }
            match &block.terminator {
                Terminator::Return(Some(value)) => counts[value.index()] += 1,
                Terminator::ReturnData { offset, size } => {
                    counts[offset.index()] += 1;
                    counts[size.index()] += 1;
                }
                Terminator::Branch { cond, .. } => counts[cond.index()] += 1,
                Terminator::Switch { discr, .. } => counts[discr.index()] += 1,
                Terminator::Return(None) | Terminator::Goto { .. } | Terminator::Unreachable => {}
            }
        }
        counts
    }

    /// Produces the final Yul docs for the current MIR function.
    ///
    /// Returns the document tree containing a single Yul `function` block or a
    /// [`YulError`] when lowering fails.
    fn emit_doc(mut self) -> Result<Vec<YulDoc>, YulError> {
        let func_name = self.mir_func.symbol_name.as_str();
        let (param_names, mut state) = self.init_function_state();
        let body_docs = self.emit_block(self.mir_func.body.entry, &mut state)?;
        let function_doc = YulDoc::block(
            format!(
                "{} ",
                self.format_function_signature(func_name, &param_names)
            ),
            body_docs,
        );
        Ok(vec![function_doc])
    }

    /// Initializes the `BlockState` with parameter bindings and returns their Yul names.
    ///
    /// Returns a tuple containing the ordered argument names and the populated block state.
    fn init_function_state(&self) -> (Vec<String>, BlockState) {
        let mut state = BlockState::new();
        let mut params_out = Vec::new();
        if let Some(params) = self.mir_func.func.params(self.db).to_opt() {
            for (idx, param) in params.data(self.db).iter().enumerate() {
                let original = param
                    .name
                    .to_opt()
                    .and_then(|name| name.ident())
                    .map(|id| id.data(self.db).to_string())
                    .unwrap_or_else(|| format!("arg{idx}"));
                let yul_name = original.clone();
                params_out.push(yul_name.clone());
                state.insert_binding(original, yul_name);
            }
        }
        (params_out, state)
    }

    /// Formats the Fe function name and parameters into a Yul signature.
    ///
    /// * `func_name` - Symbol exported to Yul.
    /// * `params` - Rendered parameter identifiers.
    ///
    /// Returns the textual `function ... -> ret` signature.
    fn format_function_signature(&self, func_name: &str, params: &[String]) -> String {
        let params_str = params.join(", ");
        if params.is_empty() {
            format!("function {func_name}() -> ret")
        } else {
            format!("function {func_name}({params_str}) -> ret")
        }
    }

    /// Lowers a MIR `ValueId` into a Yul expression string.
    /// Translates a MIR value into its Yul expression string.
    ///
    /// * `value_id` - Identifier selecting the MIR value.
    /// * `state` - Current bindings for previously-evaluated expressions.
    ///
    /// Returns the Yul expression referencing the value or an error if unsupported.
    fn lower_value(&self, value_id: ValueId, state: &BlockState) -> Result<String, YulError> {
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
    fn lower_expr(&self, expr_id: ExprId, state: &BlockState) -> Result<String, YulError> {
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

    /// Extracts the identifier bound by a pattern.
    ///
    /// * `pat_id` - Pattern pointing to the binding site.
    ///
    /// Returns the identifier string or a `YulError` if the pattern is unsupported.
    fn pattern_ident(&self, pat_id: PatId) -> Result<String, YulError> {
        let pat = self.expect_pat(pat_id)?;
        match pat {
            Pat::Path(path, _) => self
                .path_ident(*path)
                .ok_or_else(|| YulError::Unsupported("unsupported pattern path".into())),
            _ => Err(YulError::Unsupported(
                "only identifier patterns are supported".into(),
            )),
        }
    }

    /// Resolves an expression that should represent a path (e.g. assignment target).
    ///
    /// * `expr_id` - Expression expected to be a path/identifier.
    ///
    /// Returns the resolved identifier or an error if the expression is not a simple path.
    fn path_from_expr(&self, expr_id: ExprId) -> Result<String, YulError> {
        let expr = self.expect_expr(expr_id)?;
        if let Expr::Path(path) = expr {
            self.path_ident(*path)
                .ok_or_else(|| YulError::Unsupported("unsupported assignment target".into()))
        } else {
            Err(YulError::Unsupported(
                "only identifier assignments are supported".into(),
            ))
        }
    }

    /// Returns the identifier name represented by a path, if it is a plain ident.
    ///
    /// * `path` - Path extracted from the AST/HIR.
    ///
    /// Returns the identifier string when the path reduces to a single symbol.
    fn path_ident(&self, path: Partial<PathId<'_>>) -> Option<String> {
        let path = path.to_opt()?;
        path.as_ident(self.db)
            .map(|id| id.data(self.db).to_string())
    }

    /// Fetches the expression from HIR, converting missing data into `YulError`.
    ///
    /// * `expr_id` - Expression handle inside the current body.
    ///
    /// Returns the referenced expression node or an error if the expression is absent.
    fn expect_expr(&self, expr_id: ExprId) -> Result<&Expr<'db>, YulError> {
        match expr_id.data(self.db, self.body) {
            Partial::Present(expr) => Ok(expr),
            Partial::Absent => Err(YulError::Unsupported("expression data unavailable".into())),
        }
    }

    /// Fetches the pattern from HIR, converting missing data into `YulError`.
    ///
    /// * `pat_id` - Pattern identifier within the current body.
    ///
    /// Returns the pattern node or an error when unavailable.
    fn expect_pat(&self, pat_id: PatId) -> Result<&Pat<'db>, YulError> {
        match pat_id.data(self.db, self.body) {
            Partial::Present(pat) => Ok(pat),
            Partial::Absent => Err(YulError::Unsupported("unsupported pattern".into())),
        }
    }

    /// Fetches the statement from HIR, converting missing data into `YulError`.
    ///
    /// * `stmt_id` - Statement identifier within the current body.
    ///
    /// Returns the statement node or an error when unavailable.
    fn expect_stmt(&self, stmt_id: StmtId) -> Result<&Stmt<'db>, YulError> {
        match stmt_id.data(self.db, self.body) {
            Partial::Present(stmt) => Ok(stmt),
            Partial::Absent => Err(YulError::Unsupported("statement data unavailable".into())),
        }
    }

    /// Lowers a MIR call into a Yul function invocation.
    ///
    /// * `call` - Call origin describing the callee and arguments.
    /// * `state` - Binding state used to lower argument expressions.
    ///
    /// Returns the Yul invocation string for the call.
    fn lower_call_value(
        &self,
        call: &CallOrigin<'_>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        let callee = if let Some(name) = &call.resolved_name {
            name.clone()
        } else {
            let Some(func) = call.callable.func_def.hir_func_def(self.db) else {
                return Err(YulError::Unsupported(
                    "callable without hir function definition is not supported yet".into(),
                ));
            };
            function_name(self.db, func)
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
    fn lower_expr_with_statements(
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
    fn expr_is_unit(&self, expr_id: ExprId) -> bool {
        let ty = self.mir_func.typed_body.expr_ty(self.db, expr_id);
        ty.is_tuple(self.db) && ty.field_count(self.db) == 0
    }
}

/// Returns the display name of a function or `<anonymous>` if one does not exist.
///
/// * `func` - HIR function to name.
///
/// Returns the display string used for diagnostics and Yul names.
fn function_name(db: &DriverDataBase, func: Func<'_>) -> String {
    func.name(db)
        .to_opt()
        .map(|id| id.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".into())
}

/// Returns `true` when `name` matches one of the temporary casting shims
/// (`__{src}_as_{dst}`) used while the `as` syntax is unavailable.
///
/// * `name` - Candidate function identifier.
///
/// Returns `true` if the name obeys the shim convention.
fn is_cast_shim(name: &str) -> bool {
    cast_shim_parts(name).is_some()
}

/// Converts usages of cast shims into their lone argument so we don't emit fake calls.
///
/// * `name` - Function identifier for the shim.
/// * `args` - Already-lowered argument expressions.
///
/// Returns the collapsed argument when `name` is a shim, otherwise `None`.
fn try_collapse_cast_shim(name: &str, args: &[String]) -> Result<Option<String>, YulError> {
    if !is_cast_shim(name) {
        return Ok(None);
    }
    debug_assert_eq!(
        args.len(),
        1,
        "cast shims are expected to take a single argument"
    );
    let arg = args
        .first()
        .cloned()
        .ok_or_else(|| YulError::Unsupported("cast shim missing argument".into()))?;
    Ok(Some(arg))
}

/// Validates that a name follows the `__{src}_as_{dst}` convention and returns the parts.
///
/// * `name` - Candidate shim identifier.
///
/// Returns the `(src, dst)` tuple when the name matches the convention.
fn cast_shim_parts(name: &str) -> Option<(&str, &str)> {
    let stripped = name.strip_prefix("__")?;
    let (src, dst) = stripped.split_once("_as_")?;
    if src.is_empty() || dst.is_empty() {
        return None;
    }
    if !is_cast_ident(src) || !is_cast_ident(dst) {
        return None;
    }
    Some((src, dst))
}

/// Validates that a substring of a shim name matches the allowed identifier schema.
///
/// * `part` - Either the source or destination portion of the shim name.
///
/// Returns `true` if `part` contains only lowercase letters and digits.
fn is_cast_ident(part: &str) -> bool {
    part.chars()
        .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit())
}
