use fe_hir::analysis::{
    semantic::{
        NDataProjection, NExpr, NIndex, NStatementKind, SemanticInstance,
        identity_semantic_instance_key, normalize_semantic_body,
    },
    ty::ty_check::BodyOwner,
};
use fe_hir::test_db::HirAnalysisTestDb;

#[test]
fn array_index_is_checked_before_its_read() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "array_index_is_checked_before_its_read.fe".into(),
        r#"fn read(values: [u256; 4], index: usize) -> u256 {
    values[index]
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let [func] = top_mod.all_funcs(&db).as_slice() else {
        panic!("expected exactly one function");
    };
    let instance = SemanticInstance::new(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*func, None)),
    );
    let normalized = normalize_semantic_body(&db, instance).expect("normalized body");

    let statements = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .collect::<Vec<_>>();
    let check = statements
        .iter()
        .position(|statement| {
            matches!(
                statement.kind,
                NStatementKind::CheckIndex {
                    index: NIndex::Value(_),
                    array,
                } if array.array_len(&db) == Some(4)
            )
        })
        .expect("a dynamic index into `[u256; 4]` is checked");
    let NStatementKind::CheckIndex {
        index: NIndex::Value(index),
        ..
    } = statements[check].kind
    else {
        unreachable!()
    };
    let takes_index =
        |projection: &NDataProjection| *projection == NDataProjection::Index(NIndex::Value(index));
    let uses_index = |expr: &NExpr<'_>| match expr {
        NExpr::ProjectValue { path, .. } => path.0.iter().any(takes_index),
        NExpr::Load { place, .. } => place.path.iter().any(takes_index),
        _ => false,
    };
    let read = statements
        .iter()
        .position(|statement| {
            matches!(&statement.kind, NStatementKind::Define { expr, .. } if uses_index(expr))
        })
        .expect("the read takes the checked index");
    assert!(check < read, "the check precedes the read");
}
