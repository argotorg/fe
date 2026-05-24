use common::origin::OriginExportKey;
use fe_hir::{
    hir_def::{Body, ExprId, Func, StmtId, TopLevelMod},
    origin::{HirExprOrigin, HirOriginBodyOwnerKey, HirStmtOrigin},
    test_db::HirAnalysisTestDb,
};

fn origin_key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
}

fn find_func<'db>(db: &'db HirAnalysisTestDb, top_mod: TopLevelMod<'db>, name: &str) -> Func<'db> {
    top_mod
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| {
            func.name(db)
                .to_opt()
                .is_some_and(|ident| ident.data(db) == name)
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

fn body_for<'db>(db: &'db HirAnalysisTestDb, top_mod: TopLevelMod<'db>, name: &str) -> Body<'db> {
    find_func(db, top_mod, name)
        .body(db)
        .unwrap_or_else(|| panic!("function `{name}` should have a body"))
}

#[test]
fn hir_origin_export_keys_include_kind_owner_and_local_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "origin_export_keys.fe".into(),
        r#"
fn a() -> u256 {
    let x: u256 = 1
    x
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let body = body_for(&db, top_mod, "a");
    let local = 7;
    let owner_key = HirOriginBodyOwnerKey::new("body:a");

    let expr_key = HirExprOrigin::new(body, ExprId::from_u32(local)).export_key(&owner_key);
    let stmt_key = HirStmtOrigin::new(body, StmtId::from_u32(local)).export_key(&owner_key);

    assert_ne!(expr_key, stmt_key);
    assert_eq!(expr_key, origin_key("hir.expr", "body:a", "7"));
    assert_eq!(stmt_key, origin_key("hir.stmt", "body:a", "7"));
}

#[test]
fn hir_origin_body_owner_key_rejects_invalid_text() {
    assert!(HirOriginBodyOwnerKey::try_new("").is_err());
    assert!(HirOriginBodyOwnerKey::try_new("body\u{1f}a").is_err());
}
