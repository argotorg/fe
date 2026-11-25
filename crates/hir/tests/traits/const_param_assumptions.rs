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

#[test]
fn const_fn_reading_assumption_in_where_clause_under_default_const_param_terminates() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "const_fn_reading_assumption_in_where_clause_under_default_const_param.fe",
        ),
        r#"
trait Len {
    const LEN: usize
}

trait Indexed<const I: usize> {
    const V: usize
}

const fn read_len<T: Len>() -> usize {
    T::LEN
}

struct S1<T, const L: usize = 5>
where
    T: Len + Indexed<{ read_len<T>() }>,
{
    t: T,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}
