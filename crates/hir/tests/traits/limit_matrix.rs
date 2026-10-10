//! A normalization limit decides a program only when the answer depends on
//! it (law 5 of the limits design). The families are in
//! `limit_families.rs`; this test checks them with the analysis passes.

use fe_hir::test_db::{HirAnalysisTestDb, initialize_test_analysis_pass};

include!("limit_families.rs");

/// The error codes of `src`, sorted. Identical diagnostics from different
/// passes count once, as the driver shows them.
fn codes(src: &str) -> Vec<String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("limit_matrix.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    let mut manager = initialize_test_analysis_pass();
    let mut seen = Vec::new();
    for diag in manager.run_on_module(&db, top_mod) {
        let diag = diag.to_complete(&db);
        if !seen.contains(&diag) {
            seen.push(diag);
        }
    }
    let mut codes: Vec<String> = seen
        .into_iter()
        .map(|diag| diag.error_code.to_string())
        .collect();
    codes.sort();
    codes
}

#[test]
fn a_limit_decides_only_answers_that_depend_on_it() {
    check_all(&codes, 8);
}

/// The unrelated trait and impl come from a dependency the caller never
/// imports from.
#[test]
fn method_invisible_trait_in_another_ingot() {
    use common::InputDb;
    fn touch(db: &mut HirAnalysisTestDb, url: &str, text: String) -> common::file::File {
        db.workspace()
            .touch(db, url::Url::parse(url).unwrap(), Some(text))
    }
    for depth in [3, 70] {
        let mut db = HirAnalysisTestDb::default();
        touch(
            &mut db,
            "file:///limit-matrix/dep/fe.toml",
            "[ingot]\nname = \"dep\"\nversion = \"0.1.0\"\n".to_string(),
        );
        touch(
            &mut db,
            "file:///limit-matrix/dep/src/lib.fe",
            "pub trait Nest { type Out }
pub struct N<T> { pub t: T }
impl<T: Nest> Nest for N<T> { type Out = T::Out }
impl Nest for u8 { type Out = u8 }
pub trait Show {}
impl Show for u8 {}
pub struct W<T> { pub t: T }
pub trait Other { fn get(self) -> u8 }
impl<T: Nest> Other for W<T> where T::Out: Show { fn get(self) -> u8 { 2 } }
"
            .to_string(),
        );
        touch(
            &mut db,
            "file:///limit-matrix/app/fe.toml",
            "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\ndep = { path = \"../dep\" }\n"
                .to_string(),
        );
        let app = touch(
            &mut db,
            "file:///limit-matrix/app/src/lib.fe",
            format!(
                "use dep::{{W, N}}
trait Get {{ fn get(self) -> u8 }}
impl<T> Get for W<T> {{ fn get(self) -> u8 {{ 1 }} }}
pub fn f(w: own W<{}>) -> u8 {{ w.get() }}
",
                chain(depth)
            ),
        );
        let (top_mod, _) = db.top_mod(app);
        let mut manager = initialize_test_analysis_pass();
        let diags: Vec<_> = manager
            .run_on_module(&db, top_mod)
            .into_iter()
            .map(|diag| diag.to_complete(&db).error_code.to_string())
            .collect();
        assert!(diags.is_empty(), "depth {depth}: {diags:?}");
    }
}
