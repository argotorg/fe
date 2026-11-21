use camino::Utf8PathBuf;
use fe_hir::test_db::HirAnalysisTestDb;

#[test]
fn a_const_in_a_type_checks_its_qualified_trait_visibility() {
    for ty in [
        "[u8; <u8 as hidden::Width>::N]",
        "Fixed<{ <u8 as hidden::Width>::N }>",
    ] {
        for public in ["", "pub "] {
            let mut db = HirAnalysisTestDb::default();
            let source = format!(
                "
pub mod hidden {{
    {public}trait Width {{ const N: usize }}
    impl Width for u8 {{ const N: usize = 2 }}
}}
pub struct Fixed<const N: usize> {{}}
pub type Array = {ty}
"
            );
            let file = db.new_stand_alone("qualified_const_visibility.fe".into(), &source);
            let (top_mod, _) = db.top_mod(file);
            if public.is_empty() {
                let diagnostics = db.run_on_top_mod(top_mod);
                assert!(
                    diagnostics
                        .iter()
                        .any(|diag| { diag.to_complete(&db).message == "`Width` is not visible" }),
                    "{source}"
                );
            } else {
                db.assert_no_diags(top_mod);
            }
        }
    }
}

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
