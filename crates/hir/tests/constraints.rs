use std::path::Path;

use common::diagnostics::{CompleteDiagnostic, cmp_complete_diagnostics};
use dir_test::{Fixture, dir_test};
use fe_hir::{
    hir_def::TopLevelMod,
    test_db::{HirAnalysisTestDb, initialize_test_analysis_pass},
};

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/constraints",
    glob: "*.fe"
)]
fn constraints_standalone(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

fn diagnostics_for<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<CompleteDiagnostic> {
    let mut manager = initialize_test_analysis_pass();
    let mut diags: Vec<_> = manager
        .run_on_module(db, top_mod)
        .into_iter()
        .map(|diag| diag.to_complete(db))
        .collect();
    diags.sort_by(cmp_complete_diagnostics);
    diags
}

fn assert_unsatisfied_bound(diags: &[CompleteDiagnostic], expected: &str) {
    assert!(
        diags.iter().any(|diag| {
            diag.message == "trait bound is not satisfied"
                && diag
                    .sub_diagnostics
                    .iter()
                    .any(|sub| sub.message.contains(expected))
        }),
        "expected unsatisfied bound containing `{expected}`, got diagnostics: {diags:#?}"
    );
}

fn assert_required_by_bound(diags: &[CompleteDiagnostic], callable_name: &str) {
    let expected = format!("required by this bound on `{callable_name}`");
    assert!(
        diags.iter().any(|diag| {
            diag.sub_diagnostics
                .iter()
                .any(|sub| sub.message.contains(&expected))
        }),
        "expected call-bound note containing `{expected}`, got diagnostics: {diags:#?}"
    );
}

#[test]
fn free_function_calls_check_instantiated_constraints_without_effects() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "free_function_calls_check_instantiated_constraints_without_effects.fe".into(),
        r#"
trait Other {}

fn needs<T>(x: T)
where
    T: Other
{}

fn caller() {
    let x: u8 = 1
    needs(x)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_unsatisfied_bound(&diags, "`u8` doesn't implement `Other`");
}

#[test]
fn free_function_calls_accept_satisfied_constraints_without_effects() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "free_function_calls_accept_satisfied_constraints_without_effects.fe".into(),
        r#"
trait Other {}

impl Other for u8 {}

fn needs<T>(x: T)
where
    T: Other
{}

fn caller() {
    let x: u8 = 1
    needs(x)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn free_function_call_constraints_defer_through_if_join() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "free_function_call_constraints_defer_through_if_join.fe".into(),
        r#"
extern {
    fn todo() -> !
}

trait Other {}

fn make<T>() -> T
where
    T: Other
{
    todo()
}

fn caller(flag: bool) {
    let one: u8 = 1
    let x = if flag { make() } else { one }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_unsatisfied_bound(&diags, "`u8` doesn't implement `Other`");
}

#[test]
fn free_function_call_constraints_accept_if_join_after_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "free_function_call_constraints_accept_if_join_after_inference.fe".into(),
        r#"
extern {
    fn todo() -> !
}

trait Other {}

impl Other for u8 {}

fn make<T>() -> T
where
    T: Other
{
    todo()
}

fn caller(flag: bool) {
    let one: u8 = 1
    let x = if flag { make() } else { one }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn free_function_call_constraints_defer_through_array_literals() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "free_function_call_constraints_defer_through_array_literals.fe".into(),
        r#"
extern {
    fn todo() -> !
}

trait Other {}

fn make<T>() -> T
where
    T: Other
{
    todo()
}

fn caller() {
    let one: u8 = 1
    let xs = [make(), one]
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_unsatisfied_bound(&diags, "`u8` doesn't implement `Other`");
}

#[test]
fn free_function_call_constraints_accept_array_literals_after_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "free_function_call_constraints_accept_array_literals_after_inference.fe".into(),
        r#"
extern {
    fn todo() -> !
}

trait Other {}

impl Other for u8 {}

fn make<T>() -> T
where
    T: Other
{
    todo()
}

fn caller() {
    let one: u8 = 1
    let xs = [make(), one]
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn method_call_constraints_defer_through_if_join() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "method_call_constraints_defer_through_if_join.fe".into(),
        r#"
extern {
    fn todo() -> !
}

trait Other {}

trait Make {
    fn make<T>(self) -> T
    where
        T: Other
}

struct Factory {}

impl Make for Factory {
    fn make<T>(self) -> T
    where
        T: Other
    {
        todo()
    }
}

fn caller(flag: bool) {
    let one: u8 = 1
    let x = if flag { Factory {}.make() } else { one }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_unsatisfied_bound(&diags, "`u8` doesn't implement `Other`");
}

#[test]
fn method_call_constraints_accept_if_join_after_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "method_call_constraints_accept_if_join_after_inference.fe".into(),
        r#"
extern {
    fn todo() -> !
}

trait Other {}

impl Other for u8 {}

trait Make {
    fn make<T>(self) -> T
    where
        T: Other
}

struct Factory {}

impl Make for Factory {
    fn make<T>(self) -> T
    where
        T: Other
    {
        todo()
    }
}

fn caller(flag: bool) {
    let one: u8 = 1
    let x = if flag { Factory {}.make() } else { one }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn free_function_call_constraints_diagnose_generic_body_bounds() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "free_function_call_constraints_diagnose_generic_body_bounds.fe".into(),
        r#"
extern {
    fn todo() -> !
}

trait Other {}

fn make<T>() -> T
where
    T: Other
{
    todo()
}

fn caller<U>() -> U {
    make()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_unsatisfied_bound(&diags, "`U` doesn't implement `Other`");
    assert_required_by_bound(&diags, "make");
}

#[test]
fn deferred_method_call_constraints_substitute_self_assoc_bounds() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "deferred_method_call_constraints_substitute_self_assoc_bounds.fe".into(),
        r#"
trait Other {}

trait MakeFromU8 {
    type Assoc

    fn make(self, tag: u8)
    where
        Self::Assoc: Other
}

trait MakeFromBool {
    type Assoc

    fn make(self, tag: bool)
    where
        Self::Assoc: Other
}

struct Factory {}

impl MakeFromU8 for Factory {
    type Assoc = u8

    fn make(self, tag: u8)
    where
        Self::Assoc: Other
    {}
}

impl MakeFromBool for Factory {
    type Assoc = bool

    fn make(self, tag: bool)
    where
        Self::Assoc: Other
    {}
}

fn caller(tag: u8) {
    let factory = Factory {}
    factory.make(tag)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_unsatisfied_bound(&diags, "`u8` doesn't implement `Other`");
    assert_required_by_bound(&diags, "make");
}
