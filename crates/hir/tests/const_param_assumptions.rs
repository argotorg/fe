use camino::Utf8PathBuf;
use fe_hir::test_db::HirAnalysisTestDb;

#[test]
fn const_param_types_use_assumptions_for_lowering() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("const_param_types_use_assumptions_for_lowering.fe"),
        r#"
trait HasRootTy {
    type RootTy
}

struct UsesAssocConstTy<T: HasRootTy<RootTy = u256>, const N: T::RootTy> {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}
