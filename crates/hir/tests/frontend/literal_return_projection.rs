use fe_hir::{
    analysis::initialize_analysis_pass,
    test_db::{HirAnalysisTestDb, format_diagnostics},
};

const DEFINITIONS: &str = r#"
trait Width {
    const N: usize
    const fn bytes(self) -> [u8; Self::N] { [0; Self::N] }
}
impl<const N: usize> Width for String<N> { const N: usize = N }
impl<T: Width> Width for (T,) { const N: usize = T::N }
impl<A: Width, B: Width> Width for (A, B) { const N: usize = A::N + B::N }
struct Packed<const N: usize> { bytes: [u8; N] }
const fn packed<T: Width>(_ value: T) -> Packed<T::N> { Packed { bytes: value.bytes() } }
const fn same<const N: usize>(_ a: String<N>, _ b: String<N>) -> (String<N>,) { (a,) }
"#;

#[test]
fn literal_return_projections_wait_for_inference_and_fallback() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "literal_projection.fe".into(),
        &format!(
            r#"{DEFINITIONS}
const SINGLE: [u8; 1] = ("a",).bytes()
const NESTED: [u8; 3] = (("a",), "bc").bytes()
const EMPTY: [u8; 0] = ("",).bytes()
const VALUE: Packed<3> = packed(("a", "bc"))
const fn result() -> [u8; 1] {{ ("a",).bytes() }}
fn argument(_ bytes: [u8; 1]) {{}}
fn caller() {{ argument(("a",).bytes()) }}
// The second argument supplies width 3; fallback must not freeze the first at 1.
const WIDENED: [u8; 3] = same("a", "abc" as String<3>).bytes()
"#
        ),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
}

#[test]
fn literal_return_projection_mismatches_remain_errors() {
    for (declaration, expected) in [
        ("const BAD: [u8; 2] = (\"a\",).bytes()", "type mismatch"),
        (
            "const BAD: Packed<2> = packed((\"a\", \"bc\"))",
            "type mismatch",
        ),
        ("const BAD: [u256; 1] = (\"a\",).bytes()", "type mismatch"),
        (
            "const fn bad() -> [u8; 2] { (\"a\",).bytes() }",
            "type mismatch",
        ),
        (
            "fn borrow(_ value: mut [u8; 1]) {}\nfn bad() { borrow((\"a\",).bytes()) }",
            "argument must be a place",
        ),
        (
            "fn borrow(_ value: mut [u8; 2]) {}\nfn bad() { let mut value = (\"a\",).bytes()\nborrow(mut value) }",
            "type mismatch",
        ),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "literal_projection_error.fe".into(),
            &format!("{DEFINITIONS}\n{declaration}\n"),
        );
        let (module, _) = db.top_mod(file);
        let diags = initialize_analysis_pass().run_on_module(&db, module);
        let rendered = format_diagnostics(&db, &diags);
        assert!(rendered.contains(expected), "{declaration}\n{rendered}");
        assert!(!rendered.contains("internal"), "{rendered}");
    }
}
