use camino::Utf8PathBuf;
use fe_hir::test_db::HirAnalysisTestDb;

/// A trait-const const-generic in a trait method signature
/// (`Slot<{ Self::N }>`) must stay abstract while `Self` is generic and
/// evaluate through the impl when checked there; this used to panic in
/// semantic body lowering ("const ref should resolve to a semantic
/// instance").
#[test]
fn trait_sig_const_generic_stays_abstract_and_checks() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_sig_const_generic_ice.fe"),
        r#"
struct Slot<const N: u32> {}

trait ConstSigCtx {
    const N: u32
    fn take(self, slot: Slot<{ Self::N }>) -> Slot<{ Self::N }>
}

struct ConstSigCtxStruct {}

impl ConstSigCtx for ConstSigCtxStruct {
    const N: u32 = 3

    fn take(self, slot: Slot<{ Self::N }>) -> Slot<{ Self::N }> {
        slot
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}
