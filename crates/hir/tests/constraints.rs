use std::path::Path;

use common::diagnostics::{CompleteDiagnostic, cmp_complete_diagnostics};
use dir_test::{Fixture, dir_test};
use fe_hir::{
    hir_def::TopLevelMod,
    test_db::{HirAnalysisTestDb, initialize_analysis_pass},
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
    let mut manager = initialize_analysis_pass();
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

fn assert_const_predicate_diag(diags: &[CompleteDiagnostic]) {
    assert!(
        diags.iter().any(|diag| {
            diag.message.contains("const predicate")
                || diag
                    .sub_diagnostics
                    .iter()
                    .any(|sub| sub.message.contains("const predicate"))
                || diag
                    .notes
                    .iter()
                    .any(|note| note.contains("const predicate"))
        }),
        "expected const predicate diagnostic, got diagnostics: {diags:#?}"
    );
}

fn assert_const_predicate_required_by(diags: &[CompleteDiagnostic], expected: &str) {
    assert!(
        diags.iter().any(|diag| {
            diag.sub_diagnostics
                .iter()
                .any(|sub| sub.message.contains(expected))
        }),
        "expected const predicate source label containing `{expected}`, got diagnostics: {diags:#?}"
    );
}

fn assert_const_predicate_note(diags: &[CompleteDiagnostic], expected: &str) {
    assert!(
        diags
            .iter()
            .any(|diag| diag.notes.iter().any(|note| note.contains(expected))),
        "expected const predicate note containing `{expected}`, got diagnostics: {diags:#?}"
    );
}

fn assert_diag_message(diags: &[CompleteDiagnostic], expected: &str) {
    assert!(
        diags.iter().any(|diag| diag.message.contains(expected)),
        "expected diagnostic message containing `{expected}`, got diagnostics: {diags:#?}"
    );
}

#[test]
fn const_predicate_generic_caller_with_matching_assumption_passes() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_generic_caller_with_matching_assumption_passes.fe".into(),
        r#"
trait HasSize {
    const SIZE: u256
}

fn needs_big<T>()
where
    T: HasSize,
    T::SIZE >= 50
{}

fn caller<T>()
where
    T: HasSize,
    T::SIZE >= 50
{
    needs_big<T>()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn const_predicate_inferred_generic_call_rechecked_after_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_inferred_generic_call_rechecked_after_inference.fe".into(),
        r#"
trait HasSize {
    const SIZE: u256
}

struct Small {}

impl HasSize for Small {
    const SIZE: u256 = 1
}

fn needs_big<T>(value: T)
where
    T: HasSize,
    T::SIZE >= 50
{}

fn fail() {
    needs_big(Small {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_const_predicate_diag(&diags);
    assert_const_predicate_required_by(
        &diags,
        "required by this where-clause predicate on `needs_big`",
    );
}

#[test]
fn const_predicate_generic_caller_requires_matching_assumption() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_generic_caller_requires_matching_assumption.fe".into(),
        r#"
trait HasSize {
    const SIZE: u256
}

fn needs_big<T>()
where
    T: HasSize,
    T::SIZE >= 50
{}

fn fail<T>()
where
    T: HasSize
{
    needs_big<T>()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_const_predicate_diag(&diags);
    assert_diag_message(&diags, "missing const predicate evidence");
    assert_const_predicate_required_by(
        &diags,
        "required by this where-clause predicate on `needs_big`",
    );
}

#[test]
fn const_predicate_adt_instantiation_enforces_where_clause() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_adt_instantiation_enforces_where_clause.fe".into(),
        r#"
trait HasSize {
    const SIZE: u256
}

struct Small {}

impl HasSize for Small {
    const SIZE: u256 = 1
}

struct BigOnly<T>
where
    T: HasSize,
    T::SIZE >= 50
{
    value: T,
}

fn fail() {
    let value = BigOnly<Small> { value: Small {} }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_const_predicate_diag(&diags);
    assert_const_predicate_required_by(&diags, "required by this type's where-clause predicate");
}

#[test]
fn const_predicate_enum_variant_ctor_enforces_where_clause() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_enum_variant_ctor_enforces_where_clause.fe".into(),
        r#"
trait HasSize {
    const SIZE: u256
}

struct Small {}

impl HasSize for Small {
    const SIZE: u256 = 1
}

enum BigEnum<T>
where
    T: HasSize,
    T::SIZE >= 50
{
    Value(T)
}

fn fail() {
    let value = BigEnum::Value(Small {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_const_predicate_diag(&diags);
    assert_const_predicate_required_by(&diags, "required by this where-clause predicate on");
}

#[test]
fn const_predicate_trait_method_impl_cannot_drop_required_predicate() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_trait_method_impl_cannot_drop_required_predicate.fe".into(),
        r#"
trait HasSize {
    const SIZE: u256
}

trait Bounded {
    fn check<T>()
    where
        T: HasSize,
        T::SIZE >= 50
}

struct Checker {}

impl Bounded for Checker {
    fn check<T>()
    where
        T: HasSize
    {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_const_predicate_diag(&diags);
    assert_const_predicate_required_by(&diags, "trait method requires this const predicate");
    assert_const_predicate_note(
        &diags,
        "add the same const predicate to the implementation method's `where` clause",
    );
}

#[test]
fn const_predicate_impl_candidate_yields_residual_obligation() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_impl_candidate_yields_residual_obligation.fe".into(),
        r#"
trait HasSize {
    const SIZE: u256
}

trait Pack {}

impl<T> Pack for T
where
    T: HasSize,
    T::SIZE >= 50
{}

fn require_pack<T>()
where
    T: Pack
{}

fn fail<T>()
where
    T: HasSize
{
    require_pack<T>()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_const_predicate_diag(&diags);
    assert_const_predicate_required_by(
        &diags,
        "required by this where-clause predicate on an impl candidate",
    );
}

#[test]
fn const_predicate_declaration_rejects_non_bool_predicate() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_declaration_rejects_non_bool_predicate.fe".into(),
        r#"
fn bad<T>()
where
    1 + 1
{}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_const_predicate_diag(&diags);
}

#[test]
fn const_predicate_true_and_arithmetic_true_pass() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_true_and_arithmetic_true_pass.fe".into(),
        r#"
fn requires_true()
where
    true
{}

fn requires_arithmetic_true()
where
    1 + 1 == 2
{}

fn caller() {
    requires_true()
    requires_arithmetic_true()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn const_predicate_false_fails_as_disproved_constraint() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_false_fails_as_disproved_constraint.fe".into(),
        r#"
fn requires_false()
where
    false
{}

fn fail() {
    requires_false()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_diag_message(&diags, "const predicate is not satisfied");
    assert_const_predicate_required_by(
        &diags,
        "required by this where-clause predicate on `requires_false`",
    );
}

#[test]
fn const_predicate_arithmetic_false_fails_as_disproved_constraint() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_arithmetic_false_fails_as_disproved_constraint.fe".into(),
        r#"
fn requires_arithmetic_false()
where
    1 + 1 == 3
{}

fn fail() {
    requires_arithmetic_false()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_diag_message(&diags, "const predicate is not satisfied");
    assert_const_predicate_required_by(
        &diags,
        "required by this where-clause predicate on `requires_arithmetic_false`",
    );
}

#[test]
fn const_predicate_ctfe_error_is_not_reported_as_false() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_predicate_ctfe_error_is_not_reported_as_false.fe".into(),
        r#"
fn requires_eval()
where
    1 / 0 == 0
{}

fn fail() {
    requires_eval()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_diag_message(&diags, "const predicate could not be evaluated");
    assert!(
        diags
            .iter()
            .all(|diag| !diag.message.contains("not satisfied")),
        "expected CTFE error rather than false predicate diagnostic, got diagnostics: {diags:#?}"
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
