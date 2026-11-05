use fe_hir::test_db::HirAnalysisTestDb;

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
