use cranelift_entity::EntityRef;
use fe_hir::{
    analysis::{
        semantic::{SExpr, SLocalId, SStmtKind, SemanticInstance, identity_semantic_instance_key},
        ty::ty_check::{BodyOwner, LocalBinding, PathReadSemantics, check_func_body},
    },
    hir_def::{ExprId, ItemKind, Partial, PatId, Stmt},
    test_db::HirAnalysisTestDb,
};

fn find_func<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: fe_hir::hir_def::TopLevelMod<'db>,
    func_name: &str,
) -> fe_hir::hir_def::Func<'db> {
    top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == func_name) =>
            {
                Some(*func)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{func_name}`"))
}

fn nth_let(body: fe_hir::hir_def::Body<'_>, db: &HirAnalysisTestDb, idx: usize) -> (PatId, ExprId) {
    body.stmts(db)
        .iter()
        .filter_map(|(_, stmt)| match stmt {
            Partial::Present(Stmt::Let(pat, _, Some(init), _)) => Some((*pat, *init)),
            Partial::Present(
                Stmt::Let(_, _, None, _)
                | Stmt::While(..)
                | Stmt::For(..)
                | Stmt::Continue
                | Stmt::Break
                | Stmt::Return(..)
                | Stmt::Yield(..)
                | Stmt::Expr(..),
            )
            | Partial::Absent => None,
        })
        .nth(idx)
        .unwrap_or_else(|| panic!("missing let #{idx}"))
}

fn local_for_binding<'db>(
    body: &fe_hir::analysis::semantic::SemanticBody<'db>,
    binding: LocalBinding<'db>,
) -> SLocalId {
    body.locals
        .iter()
        .enumerate()
        .find_map(|(idx, local)| {
            (local.source == Some(binding)).then_some(SLocalId::from_u32(idx as u32))
        })
        .expect("missing local for binding")
}

fn assign_expr_for_dst<'db>(
    body: &'db fe_hir::analysis::semantic::SemanticBody<'db>,
    dst: SLocalId,
) -> &'db SExpr<'db> {
    body.blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            SStmtKind::Assign {
                dst: stmt_dst,
                expr,
            } if *stmt_dst == dst => Some(expr),
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing assignment for {dst:?}"))
}
#[test]
fn exact_binding_path_reads_preserve_binding_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "path_binding_reads.fe".into(),
        r#"
fn f() {
    let ptr: u256 = 1
    let at = ptr
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "f");
    let (diags, typed_body) = check_func_body(&db, func).clone();
    assert!(diags.is_empty(), "{diags:?}");

    let body = typed_body.body().expect("function body");
    let (src_pat, _) = nth_let(body, &db, 0);
    let (dst_pat, init) = nth_let(body, &db, 1);
    assert_eq!(
        typed_body.path_expr_read_semantics(init),
        Some(PathReadSemantics::ReuseLocal)
    );

    let src_binding = typed_body
        .pat_binding(src_pat)
        .expect("missing source binding");
    let dst_binding = typed_body
        .pat_binding(dst_pat)
        .expect("missing destination binding");
    let instance = SemanticInstance::new(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func, None)),
    );
    let sem_body = instance.body(&db);
    let dst = local_for_binding(sem_body, dst_binding);

    let SExpr::UseValue(src) = assign_expr_for_dst(sem_body, dst) else {
        panic!("binding assignment should read from the original local");
    };
    assert_eq!(
        sem_body.locals[src.value.index()].source,
        Some(src_binding),
        "preserved path read should use the original binding local directly"
    );
}
