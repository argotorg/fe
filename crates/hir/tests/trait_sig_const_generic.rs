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

/// A trait associated `const` with a default must stay abstract when its
/// instance is satisfied only by an assumption (`T: HasN`), so an impl that
/// overrides the default specializes correctly instead of being fixed to the
/// default. `slot_for(b)` infers `T = Big` (whose `N` is `4`); if `T::N` were
/// resolved to the trait default `1` while `T` was abstract, the signature
/// would bake to `Slot<1>` and the assignment would mismatch `Slot<4>`.
#[test]
fn trait_const_default_stays_abstract_under_assumption() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_const_default_stays_abstract_under_assumption.fe"),
        r#"
struct Slot<const N: u32> {}

trait HasN {
    const N: u32 = 1
}

struct Big {}
impl HasN for Big {
    const N: u32 = 4
}

fn slot_for<T: HasN>(_ t: T) -> Slot<{ T::N }> {
    Slot {}
}

fn use_it(b: Big) {
    let _s: Slot<4> = slot_for(b)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// The default is still used when a concrete impl inherits it: the fix only
/// suppresses the default for assumption-satisfied abstract instances, not for
/// a uniquely-selected concrete impl that does not override the const.
#[test]
fn trait_const_default_used_for_inheriting_concrete_impl() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_const_default_used_for_inheriting_concrete_impl.fe"),
        r#"
struct Slot<const N: u32> {}

trait HasN {
    const N: u32 = 7
}

struct Plain {}
impl HasN for Plain {}

fn slot_for(_ t: Plain) -> Slot<{ Plain::N }> {
    Slot {}
}

fn use_it(p: Plain) {
    let _s: Slot<7> = slot_for(p)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// A trait associated `const` is accepted as an array-repeat length while still
/// abstract (`[0; T::N]`), consistent with the const-param repeat path
/// (`[0; N]`), and specializes to an overriding impl's value at instantiation.
#[test]
fn trait_const_is_accepted_as_array_repeat_length() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_const_is_accepted_as_array_repeat_length.fe"),
        r#"
trait HasN {
    const N: usize = 1
}

struct Big {}
impl HasN for Big {
    const N: usize = 4
}

fn array_for<T: HasN>(_ t: T) -> [u8; T::N] {
    [0; T::N]
}

fn use_it(b: Big) {
    let _a: [u8; 4] = array_for(b)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}
