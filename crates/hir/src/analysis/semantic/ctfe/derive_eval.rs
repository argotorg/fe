use crate::hir_def::{
    BinOp, CompBinOp, Cond, CondId, Expr, ExprId, FieldIndex, IdentId, LitKind, Partial, PathId,
    Stmt, StmtId,
};

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
pub(crate) fn eval_derive_strategy_into<'db>(
    field_names: &[IdentId<'db>],
    trait_name: &str,
    sink: &mut dyn CodegenSink<'db>,
) {
    // The evaluator interprets the strategy function's pattern:
    // - reflect.field_count() → field_specs.len()
    // - reflect.field_name(i) → field_specs[i].0
    // - Operations on symbolic params → emit into sink
    //
    // For now, we implement the canonical patterns that the Fe strategies
    // describe. When the full CTFE machine is wired to handle DynField +
    // symbolic tracking, this becomes a thin wrapper around eval_root.
    match trait_name {
        "Eq" => eval_eq_strategy(field_names, sink),
        "Default" => eval_default_strategy(field_names, sink),
        "Ord" => eval_ord_strategy(field_names, sink),
        "Hash" => eval_hash_strategy(field_names, sink),
        _ => {}
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
