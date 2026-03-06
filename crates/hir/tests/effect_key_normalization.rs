use camino::Utf8PathBuf;
use fe_hir::test_db::HirAnalysisTestDb;

#[test]
fn impl_method_effect_keys_match_after_assoc_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("impl_method_effect_keys_match_after_assoc_normalization.fe"),
        r#"
trait Cap<T> {}

trait HasSlot {
    type Assoc
}

struct Slot<T, const ROOT: u256 = _> {}
struct S {}

trait T {
    fn f<X>(self, x: X) uses (cap: Cap<X::Assoc>)
    where
        X: HasSlot<Assoc = Slot<u256>>
}

impl T for S {
    fn f<X>(self, x: X) uses (cap: Cap<Slot<u256>>)
    where
        X: HasSlot<Assoc = Slot<u256>>
    {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn impl_method_effect_keys_match_with_omitted_const_expr_defaults() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("impl_method_effect_keys_match_with_omitted_const_expr_defaults.fe"),
        r#"
const fn plus1(x: usize) -> usize {
    x + 1
}

trait Cap<T> {}

struct Slot<const N: usize, const M: usize = plus1(N)> {}
struct S {}

trait T {
    fn f(self) uses (cap: Cap<Slot<4>>)
}

impl T for S {
    fn f(self) uses (cap: Cap<Slot<4, 5>>) {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}
