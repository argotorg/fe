use camino::Utf8PathBuf;
use fe_hir::analysis::ty::ty_contains_const_hole;
use fe_hir::hir_def::ItemKind;
use fe_hir::test_db::HirAnalysisTestDb;

#[test]
fn assoc_type_layout_holes_use_assumptions_for_collection() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("assoc_type_layout_holes_use_assumptions_for_collection.fe"),
        r#"
struct Slot<T, const ROOT: u256 = _> {}

trait HasSlot {
    type Assoc
}

fn f<T: HasSlot<Assoc = Slot<u256>>>(x: T::Assoc) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func) if func.name(&db).to_opt().is_some_and(|n| n.data(&db) == "f") => {
                Some(func)
            }
            _ => None,
        })
        .expect("missing `f` function");

    for ty in func.arg_tys(&db) {
        let ty = ty.instantiate_identity();
        assert!(
            !ty_contains_const_hole(&db, ty),
            "unelaborated const hole remained in function argument type: {ty:?}"
        );
    }
}
