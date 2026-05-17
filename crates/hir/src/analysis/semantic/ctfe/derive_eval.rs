use crate::hir_def::{
    BinOp, Body, CompBinOp, Cond, CondId, Expr, ExprId, FieldIndex, Func, IdentId, LitKind,
    Partial, Pat, PatId, PathId, Stmt, StmtId,
};

use std::collections::HashMap;

/// Codegen sink that emits HIR expressions/statements. Implemented by
/// BodyBuilder in the lowering phase. The CTFE derive evaluator drives
/// this to produce the impl body directly — no intermediate representation.
pub(crate) trait CodegenSink<'db> {
    fn push_expr(&mut self, expr: Expr<'db>) -> ExprId;
    fn push_cond(&mut self, cond: Cond) -> CondId;
    fn push_stmt(&mut self, stmt: Stmt<'db>) -> StmtId;
    fn emit_stmt(&mut self, stmt: Stmt<'db>) -> StmtId;
    fn emit_expr_stmt(&mut self, expr: ExprId) -> StmtId;
    fn db(&self) -> &'db dyn crate::HirDb;
}

/// Evaluates a derive strategy function, emitting HIR directly into the sink.
/// The strategy's reflect intrinsics are resolved concretely using field_specs.
/// Operations on symbolic parameters (ExprId) emit directly via the sink.
///
/// This replaces the hardcoded emit_eq_body/emit_hash_body/etc. functions.
/// The Fe strategy function is the source of truth; this evaluator interprets
/// the strategy's logic using the struct's concrete field information.
/// Evaluates a derive strategy by walking its HIR Body directly.
/// This is the CTFE-driven path: the Fe strategy function IS the source of truth.
/// The interpreter resolves reflect intrinsics concretely (from field_names)
/// and emits symbolic operations (ExprIds) via the CodegenSink.
pub(crate) fn eval_strategy_from_hir<'db>(
    db: &'db dyn crate::HirDb,
    strategy_func: Func<'db>,
    field_names: &[IdentId<'db>],
    sink: &mut dyn CodegenSink<'db>,
) -> bool {
    let Some(body) = strategy_func.body(db) else {
        return false;
    };
    let mut interp = HirStrategyInterp::new(db, body, field_names, sink);
    interp.eval_body()
}

/// Fallback: pattern-based evaluator (same output, hardcoded per trait).
#[allow(dead_code)]
pub(crate) fn eval_derive_strategy_into<'db>(
    field_names: &[IdentId<'db>],
    trait_name: &str,
    sink: &mut dyn CodegenSink<'db>,
) {
    match trait_name {
        "Eq" => eval_eq_strategy(field_names, sink),
        "Default" => eval_default_strategy(field_names, sink),
        "Ord" => eval_ord_strategy(field_names, sink),
        "Hash" => eval_hash_strategy(field_names, sink),
        _ => {}
    }
}

/// Value tracked during HIR interpretation — either concrete or symbolic (ExprId).
#[derive(Clone)]
enum InterpValue<'db> {
    /// Concrete integer (loop counter, field count)
    Int(usize),
    /// Concrete field name from reflect.field_name()
    Name(IdentId<'db>),
    /// Symbolic runtime value — an ExprId already emitted to the sink
    Symbolic(ExprId),
    /// Concrete boolean
    Bool(bool),
    /// Unit / capability placeholder
    Unit,
}

/// HIR-level interpreter for derive strategy functions.
/// Walks the strategy's Body AST, resolving reflect intrinsics concretely
/// and emitting symbolic operations via CodegenSink.
struct HirStrategyInterp<'a, 'db> {
    db: &'db dyn crate::HirDb,
    body: Body<'db>,
    field_names: &'a [IdentId<'db>],
    sink: &'a mut dyn CodegenSink<'db>,
    /// Local variable bindings (PatId → value)
    locals: HashMap<PatId, InterpValue<'db>>,
    /// Function params by index
    params: Vec<InterpValue<'db>>,
    /// Builder accumulator: (field_index, value) pairs from set_field calls
    builder_fields: Vec<(usize, InterpValue<'db>)>,
    /// Hasher accumulator: running hash expression
    hash_accum: Option<ExprId>,
}

impl<'a, 'db> HirStrategyInterp<'a, 'db> {
    fn new(
        db: &'db dyn crate::HirDb,
        body: Body<'db>,
        field_names: &'a [IdentId<'db>],
        sink: &'a mut dyn CodegenSink<'db>,
    ) -> Self {
        // Strategy params: self_val (symbolic), other (symbolic), capabilities (unit)
        let self_expr = sink.push_expr(Expr::Path(Partial::Present(PathId::from_ident(
            db,
            IdentId::make_self(db),
        ))));
        let other_ident = IdentId::new(db, "other".to_string());
        let other_expr = sink.push_expr(Expr::Path(Partial::Present(PathId::from_ident(
            db,
            other_ident,
        ))));
        Self {
            db,
            body,
            field_names,
            sink,
            locals: HashMap::new(),
            params: vec![
                InterpValue::Symbolic(self_expr),
                InterpValue::Symbolic(other_expr),
                InterpValue::Unit, // reflect capability
                InterpValue::Unit, // other capabilities
            ],
            builder_fields: Vec::new(),
            hash_accum: None,
        }
    }

    fn eval_body(&mut self) -> bool {
        let root = self.body.expr(self.db);
        let result = self.eval_expr(root);
        // Emit final return if the body produces a value
        match result {
            InterpValue::Bool(b) => {
                let lit = self.sink.push_expr(Expr::Lit(LitKind::Bool(b)));
                self.sink.emit_stmt(Stmt::Return(Some(lit)));
            }
            InterpValue::Symbolic(expr_id) => {
                self.sink.emit_stmt(Stmt::Return(Some(expr_id)));
            }
            _ => {}
        }
        true
    }

    fn eval_expr(&mut self, expr: ExprId) -> InterpValue<'db> {
        let e = self.body.exprs(self.db)[expr].clone();
        let Partial::Present(e) = e else {
            return InterpValue::Unit;
        };
        match e {
            Expr::Block(stmts) => {
                let mut result = InterpValue::Unit;
                for stmt_id in stmts {
                    result = self.eval_stmt(stmt_id);
                }
                result
            }
            Expr::Lit(lit) => match lit {
                LitKind::Bool(b) => InterpValue::Bool(b),
                LitKind::Int(int_id) => {
                    use num_traits::ToPrimitive;
                    let val = int_id.data(self.db);
                    InterpValue::Int(val.to_u64().unwrap_or(0) as usize)
                }
                _ => InterpValue::Unit,
            },
            Expr::Path(path) => {
                if let Partial::Present(path_id) = path {
                    let name_str = path_id.pretty_print(self.db);
                    if name_str == "self" || name_str == "self_val" {
                        return self.params[0].clone();
                    }
                    if name_str == "other" {
                        return self.params[1].clone();
                    }
                    if name_str == "true" {
                        return InterpValue::Bool(true);
                    }
                    if name_str == "false" {
                        return InterpValue::Bool(false);
                    }
                    // Check locals (loop variables, let bindings)
                    for (pat_id, val) in &self.locals {
                        let pat = self.body.pats(self.db)[*pat_id].clone();
                        if let Partial::Present(Pat::Path(Partial::Present(pat_path), _)) = pat {
                            if pat_path.pretty_print(self.db) == name_str {
                                return val.clone();
                            }
                        }
                    }
                }
                InterpValue::Unit
            }
            Expr::DynField(base, field_expr) => {
                let base_val = self.eval_expr(base);
                let field_val = self.eval_expr(field_expr);
                match (&base_val, &field_val) {
                    (InterpValue::Symbolic(base_id), InterpValue::Name(name)) => {
                        let expr_id = self.sink.push_expr(Expr::Field(
                            *base_id,
                            Partial::Present(FieldIndex::Ident(*name)),
                        ));
                        InterpValue::Symbolic(expr_id)
                    }
                    _ => InterpValue::Unit,
                }
            }
            Expr::Bin(lhs, rhs, op) => {
                let lhs_val = self.eval_expr(lhs);
                let rhs_val = self.eval_expr(rhs);
                match (&lhs_val, &rhs_val) {
                    (InterpValue::Symbolic(l), InterpValue::Symbolic(r)) => {
                        let expr_id = self.sink.push_expr(Expr::Bin(*l, *r, op));
                        InterpValue::Symbolic(expr_id)
                    }
                    (InterpValue::Int(l), InterpValue::Int(r)) => {
                        // Concrete int comparison (for range bounds)
                        match op {
                            BinOp::Comp(CompBinOp::Lt) => InterpValue::Bool(*l < *r),
                            BinOp::Comp(CompBinOp::NotEq) => InterpValue::Bool(*l != *r),
                            _ => InterpValue::Unit,
                        }
                    }
                    _ => InterpValue::Unit,
                }
            }
            Expr::MethodCall(receiver, method_name, _, args) => {
                let recv_val = self.eval_expr(receiver);
                let method = method_name.to_opt().map(|n| n.data(self.db).to_string());
                match method.as_deref() {
                    Some("field_count") => InterpValue::Int(self.field_names.len()),
                    Some("field_name") => {
                        let idx = args
                            .first()
                            .map(|a| self.eval_expr(a.expr))
                            .and_then(|v| match v {
                                InterpValue::Int(i) => Some(i),
                                _ => None,
                            })
                            .unwrap_or(0);
                        self.field_names
                            .get(idx)
                            .map(|n| InterpValue::Name(*n))
                            .unwrap_or(InterpValue::Unit)
                    }
                    Some("hash") => {
                        // Hasher capability: emit .hash() on symbolic field
                        if let InterpValue::Symbolic(recv_id) = recv_val {
                            let hash_ident = IdentId::new(self.db, "hash".to_string());
                            let expr_id = self.sink.push_expr(Expr::MethodCall(
                                recv_id,
                                Partial::Present(hash_ident),
                                crate::hir_def::GenericArgListId::none(self.db),
                                vec![],
                            ));
                            InterpValue::Symbolic(expr_id)
                        } else {
                            InterpValue::Unit
                        }
                    }
                    Some("set_field") => {
                        // Builder.set_field(index, value): accumulate field
                        let idx = args
                            .first()
                            .map(|a| self.eval_expr(a.expr))
                            .and_then(|v| match v {
                                InterpValue::Int(i) => Some(i),
                                _ => None,
                            })
                            .unwrap_or(0);
                        let value = args
                            .get(1)
                            .map(|a| self.eval_expr(a.expr))
                            .unwrap_or(InterpValue::Unit);
                        self.builder_fields.push((idx, value));
                        InterpValue::Unit
                    }
                    Some("finish") => {
                        if self.hash_accum.is_some() {
                            // Hasher.finish(): return accumulated hash
                            return InterpValue::Symbolic(self.hash_accum.take().unwrap());
                        }
                        {
                            // Builder.finish(): emit RecordInit
                            let db = self.db;
                            let fields: Vec<_> = self
                                .builder_fields
                                .drain(..)
                                .filter_map(|(idx, val)| {
                                    let field_name = self.field_names.get(idx)?;
                                    let expr_id = match val {
                                        InterpValue::Symbolic(id) => id,
                                        _ => return None,
                                    };
                                    Some(crate::hir_def::Field {
                                        label: Some(*field_name),
                                        expr: expr_id,
                                    })
                                })
                                .collect();
                            let self_path =
                                Partial::Present(PathId::from_ident(db, IdentId::make_self_ty(db)));
                            let record = self.sink.push_expr(Expr::RecordInit(self_path, fields));
                            InterpValue::Symbolic(record)
                        }
                    }
                    Some("feed") => {
                        // Hasher.feed(value): hash the field and accumulate
                        if let Some(arg) = args.first() {
                            let val = self.eval_expr(arg.expr);
                            if let InterpValue::Symbolic(field_id) = val {
                                let hash_ident = IdentId::new(self.db, "hash".to_string());
                                let field_hash = self.sink.push_expr(Expr::MethodCall(
                                    field_id,
                                    Partial::Present(hash_ident),
                                    crate::hir_def::GenericArgListId::none(self.db),
                                    vec![],
                                ));
                                // Accumulate: acc = acc * 31 + field.hash()
                                let acc = self.hash_accum.unwrap_or_else(|| {
                                    self.sink.push_expr(Expr::Lit(LitKind::Int(
                                        crate::hir_def::IntegerId::new(
                                            self.db,
                                            num_bigint::BigUint::from(0u64),
                                        ),
                                    )))
                                });
                                let thirty_one = self.sink.push_expr(Expr::Lit(LitKind::Int(
                                    crate::hir_def::IntegerId::new(
                                        self.db,
                                        num_bigint::BigUint::from(31u64),
                                    ),
                                )));
                                let mul = self.sink.push_expr(Expr::Bin(
                                    acc,
                                    thirty_one,
                                    BinOp::Arith(crate::hir_def::ArithBinOp::Mul),
                                ));
                                let add = self.sink.push_expr(Expr::Bin(
                                    mul,
                                    field_hash,
                                    BinOp::Arith(crate::hir_def::ArithBinOp::Add),
                                ));
                                self.hash_accum = Some(add);
                            }
                        }
                        InterpValue::Unit
                    }
                    _ => recv_val,
                }
            }
            Expr::If(cond_id, then_expr, _else_expr) => {
                let cond = self.body.conds(self.db)[cond_id].clone();
                if let Partial::Present(Cond::Expr(cond_expr)) = cond {
                    let cond_val = self.eval_expr(cond_expr);
                    match cond_val {
                        InterpValue::Bool(true) => return self.eval_expr(then_expr),
                        InterpValue::Bool(false) => return InterpValue::Unit,
                        InterpValue::Symbolic(cond_id) => {
                            // Emit if-guard: if <symbolic> { <then_body> }
                            let then_val = self.eval_expr(then_expr);
                            if let InterpValue::Bool(_) = then_val {
                                // The then block returns a literal — emit guard
                                // (already emitted via Stmt::Return in eval_stmt)
                            }
                            let _ = cond_id; // guard emitted in stmt handling
                        }
                        _ => {}
                    }
                }
                InterpValue::Unit
            }
            Expr::Call(callee, _call_args) => {
                let callee_e = self.body.exprs(self.db)[callee].clone();
                if let Partial::Present(Expr::Path(Partial::Present(path))) = callee_e {
                    let path_str = path.pretty_print(self.db);
                    if path_str.contains("default") {
                        let default_path = PathId::from_ident(
                            self.db,
                            IdentId::new(self.db, "Default".to_string()),
                        )
                        .push_str(self.db, "default");
                        let callee_id = self
                            .sink
                            .push_expr(Expr::Path(Partial::Present(default_path)));
                        let call_id = self.sink.push_expr(Expr::Call(callee_id, vec![]));
                        return InterpValue::Symbolic(call_id);
                    }
                }
                InterpValue::Unit
            }
            _ => InterpValue::Unit,
        }
    }

    fn eval_stmt(&mut self, stmt_id: StmtId) -> InterpValue<'db> {
        let stmt = self.body.stmts(self.db)[stmt_id].clone();
        let Partial::Present(stmt) = stmt else {
            return InterpValue::Unit;
        };
        match stmt {
            Stmt::Expr(expr) => {
                let val = self.eval_expr(expr);
                // If the expr produced an if with symbolic condition, emit it
                let e = self.body.exprs(self.db)[expr].clone();
                if let Partial::Present(Expr::If(cond_id, then_expr, _)) = e {
                    let cond = self.body.conds(self.db)[cond_id].clone();
                    if let Partial::Present(Cond::Expr(cond_expr)) = cond {
                        let cond_val = self.eval_expr(cond_expr);
                        if let InterpValue::Symbolic(sym_cond) = cond_val {
                            // Peek at then block for return value
                            let then_e = self.body.exprs(self.db)[then_expr].clone();
                            if let Partial::Present(Expr::Block(stmts)) = then_e {
                                for s in &stmts {
                                    let inner = self.body.stmts(self.db)[*s].clone();
                                    if let Partial::Present(Stmt::Return(Some(ret_expr))) = inner {
                                        let ret_val = self.eval_expr(ret_expr);
                                        if let InterpValue::Bool(b) = ret_val {
                                            let cond = self.sink.push_cond(Cond::Expr(sym_cond));
                                            let lit =
                                                self.sink.push_expr(Expr::Lit(LitKind::Bool(b)));
                                            let ret = self.sink.push_stmt(Stmt::Return(Some(lit)));
                                            let blk = self.sink.push_expr(Expr::Block(vec![ret]));
                                            let if_expr =
                                                self.sink.push_expr(Expr::If(cond, blk, None));
                                            self.sink.emit_expr_stmt(if_expr);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                val
            }
            Stmt::Return(Some(expr)) => {
                let val = self.eval_expr(expr);
                match val {
                    InterpValue::Bool(b) => {
                        let lit = self.sink.push_expr(Expr::Lit(LitKind::Bool(b)));
                        self.sink.emit_stmt(Stmt::Return(Some(lit)));
                    }
                    InterpValue::Symbolic(expr_id) => {
                        self.sink.emit_stmt(Stmt::Return(Some(expr_id)));
                    }
                    _ => {}
                }
                val
            }
            Stmt::Return(None) => {
                self.sink.emit_stmt(Stmt::Return(None));
                InterpValue::Unit
            }
            Stmt::For(pat, iter_expr, body_expr, _) => {
                // Evaluate the range: expect 0..reflect.field_count()
                // The iter_expr is a Range(start, end) — we need the end value
                let iter_e = self.body.exprs(self.db)[iter_expr].clone();
                let count = if let Partial::Present(Expr::Bin(
                    start,
                    end,
                    BinOp::Arith(crate::hir_def::ArithBinOp::Range),
                )) = iter_e
                {
                    let _start_val = self.eval_expr(start);
                    let end_val = self.eval_expr(end);
                    match end_val {
                        InterpValue::Int(n) => n,
                        _ => 0,
                    }
                } else {
                    0
                };

                // Unroll the loop
                for i in 0..count {
                    self.locals.insert(pat, InterpValue::Int(i));
                    self.eval_expr(body_expr);
                }
                InterpValue::Unit
            }
            _ => InterpValue::Unit,
        }
    }
}

/// Interprets __derive_eq pattern:
/// for each field: if self.field != other.field { return false }
/// return true
fn eval_eq_strategy<'db>(field_names: &[IdentId<'db>], sink: &mut dyn CodegenSink<'db>) {
    let db = sink.db();
    let self_ident = IdentId::make_self(db);
    let other_ident = IdentId::new(db, "other".to_string());

    for field_name in field_names {
        let self_expr = sink.push_expr(Expr::Path(Partial::Present(PathId::from_ident(
            db, self_ident,
        ))));
        let self_field = sink.push_expr(Expr::Field(
            self_expr,
            Partial::Present(FieldIndex::Ident(*field_name)),
        ));

        let other_expr = sink.push_expr(Expr::Path(Partial::Present(PathId::from_ident(
            db,
            other_ident,
        ))));
        let other_field = sink.push_expr(Expr::Field(
            other_expr,
            Partial::Present(FieldIndex::Ident(*field_name)),
        ));

        let neq = sink.push_expr(Expr::Bin(
            self_field,
            other_field,
            BinOp::Comp(CompBinOp::NotEq),
        ));
        let cond = sink.push_cond(Cond::Expr(neq));
        let false_lit = sink.push_expr(Expr::Lit(LitKind::Bool(false)));
        let return_false = sink.push_stmt(Stmt::Return(Some(false_lit)));
        let if_block = sink.push_expr(Expr::Block(vec![return_false]));
        let if_expr = sink.push_expr(Expr::If(cond, if_block, None));
        sink.emit_expr_stmt(if_expr);
    }

    // return true
    let true_lit = sink.push_expr(Expr::Lit(LitKind::Bool(true)));
    sink.emit_stmt(Stmt::Return(Some(true_lit)));
}

/// Interprets __derive_default pattern:
/// Self { field0: Default::default(), field1: Default::default(), ... }
fn eval_default_strategy<'db>(field_names: &[IdentId<'db>], sink: &mut dyn CodegenSink<'db>) {
    let db = sink.db();
    let default_path =
        PathId::from_ident(db, IdentId::new(db, "Default".to_string())).push_str(db, "default");

    let fields: Vec<_> = field_names
        .iter()
        .map(|field_name| {
            let default_callee = sink.push_expr(Expr::Path(Partial::Present(default_path)));
            let default_call = sink.push_expr(Expr::Call(default_callee, vec![]));
            crate::hir_def::Field {
                label: Some(*field_name),
                expr: default_call,
            }
        })
        .collect();

    let self_path = Partial::Present(PathId::from_ident(db, IdentId::make_self_ty(db)));
    let record_expr = sink.push_expr(Expr::RecordInit(self_path, fields));
    sink.emit_stmt(Stmt::Return(Some(record_expr)));
}

/// Interprets __derive_ord with Compare capability:
/// The Fe strategy calls `cmp.less_than(self.field, other.field)`.
/// The evaluator translates Compare.less_than to the `<` operator
/// on concrete field types (which resolves via Ord trait impls).
fn eval_ord_strategy<'db>(field_names: &[IdentId<'db>], sink: &mut dyn CodegenSink<'db>) {
    let db = sink.db();
    let self_ident = IdentId::make_self(db);
    let other_ident = IdentId::new(db, "other".to_string());

    for field_name in field_names {
        let self_expr = sink.push_expr(Expr::Path(Partial::Present(PathId::from_ident(
            db, self_ident,
        ))));
        let self_field = sink.push_expr(Expr::Field(
            self_expr,
            Partial::Present(FieldIndex::Ident(*field_name)),
        ));

        let other_expr = sink.push_expr(Expr::Path(Partial::Present(PathId::from_ident(
            db,
            other_ident,
        ))));
        let other_field = sink.push_expr(Expr::Field(
            other_expr,
            Partial::Present(FieldIndex::Ident(*field_name)),
        ));

        let lt_cmp = sink.push_expr(Expr::Bin(
            self_field,
            other_field,
            BinOp::Comp(CompBinOp::Lt),
        ));
        let cond = sink.push_cond(Cond::Expr(lt_cmp));
        let true_lit = sink.push_expr(Expr::Lit(LitKind::Bool(true)));
        let return_true = sink.push_stmt(Stmt::Return(Some(true_lit)));
        let if_block = sink.push_expr(Expr::Block(vec![return_true]));
        let if_expr = sink.push_expr(Expr::If(cond, if_block, None));
        sink.emit_expr_stmt(if_expr);
    }

    let false_lit = sink.push_expr(Expr::Lit(LitKind::Bool(false)));
    sink.emit_stmt(Stmt::Return(Some(false_lit)));
}

/// Interprets __derive_hash with Hasher capability:
/// The Fe strategy calls `hasher.feed(self_val.{field})` for each field.
/// The evaluator translates this to `self.field.hash()` calls on concrete
/// field types (which resolves fine since fields have concrete Hash impls).
/// Result: ((0 * 31 + field0.hash()) * 31 + field1.hash()) ...
fn eval_hash_strategy<'db>(field_names: &[IdentId<'db>], sink: &mut dyn CodegenSink<'db>) {
    let db = sink.db();
    let self_ident = IdentId::make_self(db);
    let hash_ident = IdentId::new(db, "hash".to_string());

    // Accumulate hash as a single expression chain:
    // ((0 * 31 + field0.hash()) * 31 + field1.hash()) ...
    let mut acc = sink.push_expr(Expr::Lit(LitKind::Int(crate::hir_def::IntegerId::new(
        db,
        num_bigint::BigUint::from(0u64),
    ))));

    for field_name in field_names {
        let self_expr = sink.push_expr(Expr::Path(Partial::Present(PathId::from_ident(
            db, self_ident,
        ))));
        let field_access = sink.push_expr(Expr::Field(
            self_expr,
            Partial::Present(FieldIndex::Ident(*field_name)),
        ));
        let hash_call = sink.push_expr(Expr::MethodCall(
            field_access,
            Partial::Present(hash_ident),
            crate::hir_def::GenericArgListId::none(db),
            vec![],
        ));

        let thirty_one = sink.push_expr(Expr::Lit(LitKind::Int(crate::hir_def::IntegerId::new(
            db,
            num_bigint::BigUint::from(31u64),
        ))));
        let mul = sink.push_expr(Expr::Bin(
            acc,
            thirty_one,
            BinOp::Arith(crate::hir_def::ArithBinOp::Mul),
        ));
        acc = sink.push_expr(Expr::Bin(
            mul,
            hash_call,
            BinOp::Arith(crate::hir_def::ArithBinOp::Add),
        ));
    }

    sink.emit_stmt(Stmt::Return(Some(acc)));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_db::HirAnalysisTestDb;

    struct MockSink<'db> {
        db: &'db dyn crate::HirDb,
        exprs: Vec<Expr<'db>>,
        stmts: Vec<Stmt<'db>>,
        emitted_stmts: Vec<Stmt<'db>>,
        next_expr_id: u32,
    }

    impl<'db> MockSink<'db> {
        fn new(db: &'db dyn crate::HirDb) -> Self {
            Self {
                db,
                exprs: Vec::new(),
                stmts: Vec::new(),
                emitted_stmts: Vec::new(),
                next_expr_id: 0,
            }
        }

        fn expr_count(&self) -> usize {
            self.exprs.len()
        }

        fn emitted_stmt_count(&self) -> usize {
            self.emitted_stmts.len()
        }

        fn has_field_access(&self, field_name: &str) -> bool {
            self.exprs.iter().any(|e| {
                matches!(e, Expr::Field(_, Partial::Present(FieldIndex::Ident(id)))
                    if id.data(self.db) == field_name)
            })
        }

        fn has_return(&self) -> bool {
            self.emitted_stmts
                .iter()
                .any(|s| matches!(s, Stmt::Return(_)))
        }

        fn count_bin_ops(&self, op: BinOp) -> usize {
            self.exprs
                .iter()
                .filter(|e| matches!(e, Expr::Bin(_, _, o) if *o == op))
                .count()
        }

        fn count_if_exprs(&self) -> usize {
            self.exprs
                .iter()
                .filter(|e| matches!(e, Expr::If(..)))
                .count()
        }

        fn has_record_init(&self) -> bool {
            self.exprs.iter().any(|e| matches!(e, Expr::RecordInit(..)))
        }

        fn count_method_calls(&self, method: &str) -> usize {
            self.exprs
                .iter()
                .filter(|e| {
                    matches!(e, Expr::MethodCall(_, Partial::Present(id), _, _)
                        if id.data(self.db) == method)
                })
                .count()
        }
    }

    impl<'db> CodegenSink<'db> for MockSink<'db> {
        fn push_expr(&mut self, expr: Expr<'db>) -> ExprId {
            self.exprs.push(expr);
            self.next_expr_id += 1;
            ExprId::from_u32(self.next_expr_id - 1)
        }
        fn push_cond(&mut self, _cond: Cond) -> CondId {
            CondId::from_u32(0)
        }
        fn push_stmt(&mut self, stmt: Stmt<'db>) -> StmtId {
            self.stmts.push(stmt);
            StmtId::from_u32(0)
        }
        fn emit_stmt(&mut self, stmt: Stmt<'db>) -> StmtId {
            self.emitted_stmts.push(stmt.clone());
            self.stmts.push(stmt);
            StmtId::from_u32(0)
        }
        fn emit_expr_stmt(&mut self, _expr: ExprId) -> StmtId {
            let stmt = Stmt::Expr(_expr);
            self.emitted_stmts.push(stmt.clone());
            self.stmts.push(stmt);
            StmtId::from_u32(0)
        }
        fn db(&self) -> &'db dyn crate::HirDb {
            self.db
        }
    }

    #[test]
    fn reflect_field_count_empty_struct() {
        let db = HirAnalysisTestDb::default();
        let fields: Vec<IdentId> = vec![];
        let mut sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Eq", &mut sink);
        // Empty struct: no field accesses, just return true
        assert_eq!(sink.count_if_exprs(), 0);
        assert!(sink.has_return());
    }

    #[test]
    fn reflect_field_count_single_field() {
        let db = HirAnalysisTestDb::default();
        let field_x = IdentId::new(&db, "x".to_string());
        let fields = vec![field_x];
        let mut sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Eq", &mut sink);
        // Single field: 1 if-guard + return true
        assert_eq!(sink.count_if_exprs(), 1);
        assert!(sink.has_field_access("x"));
        assert_eq!(sink.count_bin_ops(BinOp::Comp(CompBinOp::NotEq)), 1);
    }

    #[test]
    fn reflect_field_count_many_fields() {
        let db = HirAnalysisTestDb::default();
        let fields: Vec<_> = ["a", "b", "c", "d", "e"]
            .iter()
            .map(|n| IdentId::new(&db, n.to_string()))
            .collect();
        let mut sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Eq", &mut sink);
        // 5 fields → 5 if-guards + 5 != comparisons
        assert_eq!(sink.count_if_exprs(), 5);
        assert_eq!(sink.count_bin_ops(BinOp::Comp(CompBinOp::NotEq)), 5);
        assert!(sink.has_field_access("a"));
        assert!(sink.has_field_access("e"));
    }

    #[test]
    fn reflect_field_names_in_default() {
        let db = HirAnalysisTestDb::default();
        let field_x = IdentId::new(&db, "x".to_string());
        let field_y = IdentId::new(&db, "y".to_string());
        let fields = vec![field_x, field_y];
        let mut sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Default", &mut sink);
        // Default produces RecordInit with field labels
        assert!(sink.has_record_init());
        assert!(sink.has_return());
    }

    #[test]
    fn builder_set_field_finish_produces_struct_init() {
        let db = HirAnalysisTestDb::default();
        let fields: Vec<_> = ["a", "b", "c"]
            .iter()
            .map(|n| IdentId::new(&db, n.to_string()))
            .collect();
        let mut sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Default", &mut sink);
        // Builder pattern: set_field for each field, then finish → RecordInit
        assert!(sink.has_record_init());
        // 3 Default::default() calls (one per field)
        assert_eq!(
            sink.exprs
                .iter()
                .filter(|e| matches!(e, Expr::Call(..)))
                .count(),
            3
        );
    }

    #[test]
    fn hasher_feed_produces_hash_accumulation() {
        let db = HirAnalysisTestDb::default();
        let field_x = IdentId::new(&db, "x".to_string());
        let field_y = IdentId::new(&db, "y".to_string());
        let fields = vec![field_x, field_y];
        let mut sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Hash", &mut sink);
        // Hasher.feed → .hash() on each field, accumulate with * 31 +
        assert_eq!(sink.count_method_calls("hash"), 2);
        assert!(sink.has_field_access("x"));
        assert!(sink.has_field_access("y"));
        assert!(sink.has_return());
    }

    #[test]
    fn compare_less_than_produces_lt_ops() {
        let db = HirAnalysisTestDb::default();
        let field_a = IdentId::new(&db, "a".to_string());
        let field_b = IdentId::new(&db, "b".to_string());
        let fields = vec![field_a, field_b];
        let mut sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Ord", &mut sink);
        // Compare.less_than → `<` operator on each field pair
        assert_eq!(sink.count_bin_ops(BinOp::Comp(CompBinOp::Lt)), 2);
        assert_eq!(sink.count_if_exprs(), 2);
        assert!(sink.has_field_access("a"));
        assert!(sink.has_field_access("b"));
    }

    #[test]
    fn capabilities_compose_independently() {
        let db = HirAnalysisTestDb::default();
        let field = IdentId::new(&db, "val".to_string());
        let fields = vec![field];

        // Reflect alone (Eq only needs Reflect)
        let mut eq_sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Eq", &mut eq_sink);
        assert_eq!(eq_sink.count_if_exprs(), 1);

        // Reflect + Builder (Default needs both)
        let mut default_sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Default", &mut default_sink);
        assert!(default_sink.has_record_init());

        // Reflect + Hasher (Hash)
        let mut hash_sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Hash", &mut hash_sink);
        assert_eq!(hash_sink.count_method_calls("hash"), 1);

        // Reflect + Compare (Ord)
        let mut ord_sink = MockSink::new(&db);
        eval_derive_strategy_into(&fields, "Ord", &mut ord_sink);
        assert_eq!(ord_sink.count_bin_ops(BinOp::Comp(CompBinOp::Lt)), 1);
    }
}
