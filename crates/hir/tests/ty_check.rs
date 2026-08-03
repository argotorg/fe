use std::path::Path;

use common::diagnostics::{CompleteDiagnostic, cmp_complete_diagnostics};
use dir_test::{Fixture, dir_test};
use fe_hir::analysis::ty::{
    ty_check::{check_contract_recv_arm_body, check_func_body},
    ty_def::{Kind, TyId},
};
use fe_hir::hir_def::{Expr, Partial, TopLevelMod};
use fe_hir::span::LazySpan;
use fe_hir::test_db::{HirAnalysisTestDb, initialize_test_analysis_pass};
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/ty_check",
    glob: "**/*.fe"
)]
fn ty_check_standalone(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, mut prop_formatter) = db.top_mod(file);

    db.assert_no_diags(top_mod);

    for &func in top_mod.all_funcs(&db) {
        if let Some(body) = func.body(&db) {
            let typed_body = &check_func_body(&db, func).1;
            collect_body_props(&db, body, typed_body, &mut prop_formatter);
        }
    }

    for &contract in top_mod.all_contracts(&db) {
        let recvs = contract.recvs(&db);
        for (recv_idx, recv) in recvs.data(&db).iter().enumerate() {
            for (arm_idx, arm) in recv.arms.data(&db).iter().enumerate() {
                let typed_body =
                    &check_contract_recv_arm_body(&db, contract, recv_idx as u32, arm_idx as u32).1;
                collect_body_props(&db, arm.body, typed_body, &mut prop_formatter);
            }
        }
    }

    let res = prop_formatter.finish(&db);
    snap_test!(res, fixture.path());
}

#[test]
fn never_type_is_not_type_applicable() {
    let db = HirAnalysisTestDb::default();
    let never = TyId::never(&db);

    assert!(matches!(never.kind(&db), Kind::Star));
    assert!(never.applicable_ty(&db).is_none());
}

#[test]
fn never_for_iterator_reports_type_must_be_known() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "never_for_iterator_reports_type_must_be_known.fe".into(),
        r#"
extern {
    fn revert() -> !
}

fn trigger() {
    for x in revert() {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);

    assert!(
        diags
            .iter()
            .any(|diag| diag.message == "type must be known"),
        "{diags:#?}"
    );
    assert!(
        !diags
            .iter()
            .any(|diag| diag.message == "`Seq` needs to be implemented for !"),
        "{diags:#?}"
    );
}

#[test]
fn for_loop_rejects_array_seq_impl_with_unsatisfied_copy_constraint() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "noncopy_array_seq.fe".into(),
        r#"
use core::Copy

struct Boxed {
    value: u256,
}

fn direct() {
    let boxes = [
        Boxed { value: 20 },
        Boxed { value: 22 },
    ]
    for boxed in boxes {}
}

fn closure() {
    let boxes = [
        Boxed { value: 20 },
        Boxed { value: 22 },
    ]
    let consume = || {
        for boxed in boxes {}
    }
    consume.call_once()
}

fn generic_copy_bound<T: Copy>(_ boxes: [T; 2]) {
    for boxed in boxes {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    let seq_diags = diags
        .iter()
        .filter(|diag| diag.message.contains("`Seq` needs to be implemented"))
        .collect::<Vec<_>>();

    assert_eq!(seq_diags.len(), 2, "{diags:#?}");
    assert!(
        seq_diags
            .iter()
            .all(|diag| diag.message.contains("[Boxed; 2]")),
        "{diags:#?}"
    );
}

#[test]
fn for_loop_rejects_generic_array_without_copy_bound() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "generic_noncopy_array_seq.fe".into(),
        r#"
fn generic<T>(_ boxes: [T; 2]) {
    for boxed in boxes {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);

    assert!(
        diags.iter().any(|diag| {
            diag.message.contains("`Seq` needs to be implemented")
                && diag.message.contains("[T; 2]")
        }),
        "{diags:#?}"
    );
}

#[test]
fn inferred_closure_array_iteration_confirms_copy_constraint() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "inferred_closure_array_seq.fe".into(),
        r#"
struct Boxed {
    value: u256,
}

fn inferred_noncopy() {
    let consume = |boxes| {
        for boxed in boxes {}
    }
    consume.call([
        Boxed { value: 20 },
        Boxed { value: 22 },
    ])
}

fn inferred_copy() -> u256 {
    let sum = |values| {
        let mut total = 0
        for value in values {
            total += value
        }
        total
    }
    sum.call([20 as u256, 22])
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    let seq_diags = diags
        .iter()
        .filter(|diag| diag.message.contains("`Seq` needs to be implemented"))
        .collect::<Vec<_>>();

    assert_eq!(seq_diags.len(), 1, "{diags:#?}");
    assert!(seq_diags[0].message.contains("[Boxed; 2]"), "{diags:#?}");
}

#[test]
fn rejected_seq_candidate_rolls_back_before_viable_candidate() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "seq_candidate_rollback.fe".into(),
        r#"
use core::Seq

trait Blocked {}

struct One<T> {
    item: T,
}

impl<T: Blocked> Seq for One<T> {
    type Item = T

    fn len(self) -> usize {
        1
    }

    fn get(self, _ index: usize) -> T {
        self.item
    }
}

impl Seq for One<u256> {
    type Item = u256

    fn len(self) -> usize {
        1
    }

    fn get(self, _ index: usize) -> u256 {
        self.item
    }
}

fn sum_one(value: One<u256>) -> u256 {
    let mut total = 0
    for item in value {
        total += item
    }
    total
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    db.assert_no_diags(top_mod);
}

#[test]
fn invalid_const_fn_body_diagnostics_do_not_panic_during_const_eval() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "invalid_const_fn_body_diagnostics_do_not_panic_during_const_eval.fe".into(),
        r#"
const fn invalid_const() -> usize {
    pass
    missing_value
}

struct NeedsConst<const N: usize> {}

fn trigger() {
    let _x: NeedsConst<{ invalid_const() }>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);

    assert!(
        diagnostics_contain(&diags, "undefined variable `pass`"),
        "{diags:#?}"
    );
    assert!(
        diagnostics_contain(&diags, "undefined variable `missing_value`"),
        "{diags:#?}"
    );
    assert!(!diagnostics_contain(&diags, "const eval"), "{diags:#?}");
}

#[test]
fn generic_operator_ambiguity_preserves_checked_candidates() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "generic_operator_ambiguity_preserves_checked_candidates.fe".into(),
        r#"
use core::ops::Add

struct Box<T> {
    value: T,
}

impl<T: Copy> Copy for Box<T> {}

impl<T> Box<T> {
    fn new(value: own T) -> Box<T> {
        Box { value: value }
    }
}

impl<T: Copy> Add for Box<T> {
    fn add(own self, _ other: own Box<T>) -> Box<T> {
        self
    }
}

impl<T: Copy> Add<T> for Box<T> {
    fn add(own self, _ other: own T) -> Box<T> {
        self + Box::new(value: other)
    }
}

fn probe() -> u256 {
    let box: Box<u256> = Box::new(value: 1)
    (box + 2).value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    db.assert_no_diags(top_mod);
}

#[test]
fn trait_answer_cutoff_does_not_commit_a_deduplicated_partial_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "trait_answer_cutoff_does_not_commit_a_deduplicated_partial_type.fe".into(),
        r#"
trait Pick<T> {}

struct Subject {}
struct A {}
struct B {}

impl Pick<A> for Subject {}
impl Pick<A> for Subject {}
impl Pick<B> for Subject {}

extern {
    fn todo() -> !
}

fn pick<T, U: Pick<T>>(_value: U) -> T {
    todo()
}

fn probe(subject: Subject) {
    let _value = pick(subject)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let probe = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "probe")
        })
        .expect("missing `probe` function");
    let body = probe.body(&db).expect("missing `probe` body");
    let call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::Call(..))))
        .expect("missing `pick` call");
    let typed_body = &check_func_body(&db, probe).1;

    assert!(
        typed_body.expr_ty(&db, call).has_var(&db),
        "a truncated answer set must not determine the call's return type"
    );
}

#[test]
fn trait_answer_cutoff_commits_after_fallback_proves_one_instance() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "trait_answer_cutoff_commits_after_fallback_proves_one_instance.fe".into(),
        r#"
trait Pick<T> {}

struct Subject {}
struct A {}

impl Pick<A> for Subject {}
impl Pick<A> for Subject {}
impl Pick<A> for Subject {}

extern {
    fn todo() -> !
}

fn pick<T, U: Pick<T>>(_value: U) -> T {
    todo()
}

fn probe(subject: Subject) {
    let _value = pick(subject)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let probe = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "probe")
        })
        .expect("missing `probe` function");
    let body = probe.body(&db).expect("missing `probe` body");
    let call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::Call(..))))
        .expect("missing `pick` call");
    let call_ty = check_func_body(&db, probe).1.expr_ty(&db, call);

    assert_eq!(
        call_ty.pretty_print(&db).to_string(),
        "A",
        "a saturated fallback with one distinct instance should determine the return type",
    );
}

#[test]
fn incomplete_single_answer_does_not_commit_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "incomplete_single_answer_does_not_commit_inference.fe".into(),
        r#"
trait Pick<T> {}

struct Subject {}
struct A {}
struct Wrap<T> {}

impl Pick<A> for Subject {}
impl<SelfT, T> Pick<T> for SelfT
where
    Wrap<SelfT>: Pick<T>
{}

extern {
    fn todo() -> !
}

fn pick<T, U: Pick<T>>(_value: U) -> T {
    todo()
}

fn probe(subject: Subject) {
    let _value = pick(subject)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let probe = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "probe")
        })
        .expect("missing `probe` function");
    let body = probe.body(&db).expect("missing `probe` body");
    let call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::Call(..))))
        .expect("missing `pick` call");
    let call_ty = check_func_body(&db, probe).1.expr_ty(&db, call);

    assert!(
        call_ty.has_var(&db),
        "a partial answer from a depth-limited solve must not determine the return type",
    );
}

#[test]
fn incomplete_cutoff_fallback_does_not_commit_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "incomplete_cutoff_fallback_does_not_commit_inference.fe".into(),
        r#"
trait Pick<T> {}

struct Subject {}
struct A {}
struct Wrap<T> {}

impl Pick<A> for Subject {}
impl Pick<A> for Subject {}
impl<SelfT, T> Pick<T> for SelfT
where
    Wrap<SelfT>: Pick<T>
{}

extern {
    fn todo() -> !
}

fn pick<T, U: Pick<T>>(_value: U) -> T {
    todo()
}

fn probe(subject: Subject) {
    let _value = pick(subject)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let probe = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "probe")
        })
        .expect("missing `probe` function");
    let body = probe.body(&db).expect("missing `probe` body");
    let call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::Call(..))))
        .expect("missing `pick` call");
    let call_ty = check_func_body(&db, probe).1.expr_ty(&db, call);

    assert!(
        call_ty.has_var(&db),
        "an incomplete distinct-instance fallback must not determine the return type",
    );
}

#[test]
fn diverging_match_arm_does_not_fix_the_result_type_to_never() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "diverging_match_arm_does_not_fix_the_result_type_to_never.fe".into(),
        r#"
enum E {
    A,
    B,
}

fn probe(e: E) -> u256 {
    let value = match e {
        E::A => { return 0 }
        E::B => { 1 }
    }
    value + 1
}

fn reverse(e: E) -> u256 {
    let value = match e {
        E::A => { 1 }
        E::B => { return 0 }
    }
    value + 1
}

fn all_diverge(e: E) -> ! {
    match e {
        E::A => panic()
        E::B => panic()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    db.assert_no_diags(top_mod);
}

#[test]
fn option_map_should_infer_the_inner_type_from_closure() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "option_map_should_infer_the_inner_type_from_closure.fe".into(),
        r#"
use core::option
use core::functional::{Fn, FnOnce}
struct Doubler {}
impl FnOnce<(u256,), u256> for Doubler {
    fn call_once(self: own Self, _ args: own (u256,)) -> u256 {
        self.call(args.0)
    }
}
impl Fn<(u256,), u256> for Doubler {
    fn call(self, _ args: own (u256,)) -> u256 { args.0 * 2 }
}

fn map_no_annotation() {
    let n = Option::Some(42)
    let m = n.map(Doubler {})
    assert!(m.unwrap() == 84)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    db.assert_no_diags(top_mod);
}

#[test]
fn bound_inferred_call_result_is_usable_without_a_binding() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "bound_inferred_call_result_is_usable_without_a_binding.fe".into(),
        r#"
trait Pick<T> {}

struct Subject {}
struct Picker {}

impl Pick<u256> for Subject {}

extern {
    fn todo() -> !
}

fn pick<T, U: Pick<T>>(_ value: U) -> T {
    todo()
}

impl Picker {
    fn pick<T, U: Pick<T>>(self, _ value: U) -> T {
        todo()
    }
}

fn probe() -> u256 {
    let value = pick(Subject {}) + 1
    assert!(value == 1)
    let picker = Picker {}
    picker.pick(Subject {}) + 1
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    db.assert_no_diags(top_mod);
}

#[test]
fn eagerly_solved_bound_resolves_the_recorded_projection() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "eagerly_solved_bound_resolves_the_recorded_projection.fe".into(),
        r#"
trait Pick<T> {}
trait HasOutput {
    type Output
}

struct Subject {}
struct A {}
struct B {}
struct Picker {}

// Two implementors keep `T: HasOutput` from determining `T` on its own, so
// `T::Output` is still an unresolved projection when the call is recorded.
impl HasOutput for A {
    type Output = u256
}
impl HasOutput for B {
    type Output = bool
}

impl Pick<A> for Subject {}

extern {
    fn todo() -> !
}

fn pick<T: HasOutput, U: Pick<T>>(_ value: U) -> T::Output {
    todo()
}

impl Picker {
    fn pick<T: HasOutput, U: Pick<T>>(self, _ value: U) -> T::Output {
        todo()
    }
}

fn probe() {
    let _value = pick(Subject {})
    let picker = Picker {}
    let _method_value = picker.pick(Subject {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "probe")
        })
        .expect("missing `probe` function");
    let body = probe.body(&db).expect("missing `probe` body");
    let call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::Call(..))))
        .expect("missing `pick` call");
    let method_call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing `picker.pick` call");
    let typed_body = &check_func_body(&db, probe).1;

    assert_eq!(
        typed_body.expr_ty(&db, call).pretty_print(&db).to_string(),
        "u256",
        "solving `Subject: Pick<T>` at the call site must also resolve the recorded \
         `T::Output`, which folding the unification table alone cannot do",
    );
    assert_eq!(
        typed_body
            .expr_ty(&db, method_call)
            .pretty_print(&db)
            .to_string(),
        "u256",
        "a method call resolved without deferral must resolve its recorded \
         `T::Output` the same way a free function call does",
    );
}

#[test]
fn deferred_method_projection_is_retyped_after_bounds_are_solved() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "deferred_method_projection_is_retyped_after_bounds_are_solved.fe".into(),
        r#"
extern {
    fn todo() -> !
}

trait Pick<T> {}
trait HasOutput {
    type Output
}

struct A {}
struct B {}
struct Subject {}

// Two implementors keep `P: HasOutput` from determining `P` on its own, so
// `P::Output` is still an unresolved projection when the method resolves.
impl HasOutput for A {
    type Output = u256
}
impl HasOutput for B {
    type Output = bool
}

impl Pick<A> for Subject {}

struct S {}

// Two implementors of `Foo` make `s.foo(..)` ambiguous during the body pass,
// so the call is deferred until `resolve_deferred`. The marker argument, not
// the return type, is what picks the candidate: a concrete expected type
// cannot unify against `P::Output` while `P` is still open.
trait Foo<M> {
    fn foo<P: HasOutput, Q: Pick<P>>(self, _ marker: M, _ value: Q) -> P::Output
}

impl Foo<u8> for S {
    fn foo<P: HasOutput, Q: Pick<P>>(self, _ marker: u8, _ value: Q) -> P::Output {
        todo()
    }
}

impl Foo<bool> for S {
    fn foo<P: HasOutput, Q: Pick<P>>(self, _ marker: bool, _ value: Q) -> P::Output {
        todo()
    }
}

fn probe() {
    let s = S {}
    let _value = s.foo(true, Subject {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "probe")
        })
        .expect("missing `probe` function");
    let body = probe.body(&db).expect("missing `probe` body");
    let call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing `foo` call");
    let call_ty = check_func_body(&db, probe).1.expr_ty(&db, call);

    assert_eq!(
        call_ty.pretty_print(&db).to_string(),
        "u256",
        "bounds solved while resolving a deferred method must also retype the \
         recorded method call, which folding the unification table alone cannot do",
    );
}

#[test]
fn direct_call_should_not_report_type_mismatch() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "direct_call_should_not_report_type_mismatch.fe".into(),
        r#"
trait Pick<T> {}

trait HasOutput {
    type Output
}

struct A {}
struct B {}
struct Subject {}

impl HasOutput for A {
    type Output = u256
}

impl HasOutput for B {
    type Output = bool
}

impl Pick<A> for Subject {}

extern {
    fn todo() -> !
}

fn pick<T: HasOutput, U: Pick<T>>(_ value: U) -> T::Output {
    todo()
}

fn probe() {
    let _value: u256 = pick(Subject {})
}
"#,
    );

    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn deferred_method_is_resolved_to_the_expected_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "deferred_method_is_resolved_to_the_expected_type.fe".into(),
        r#"
trait Pick<T> {}

trait HasOutput {
    type Output
}

struct A {}
struct B {}
struct Subject {}
struct S {}

impl HasOutput for A {
    type Output = u256
}

impl HasOutput for B {
    type Output = bool
}

impl Pick<A> for Subject {}

extern {
    fn todo() -> !
}

trait Foo<M> {
    fn foo<P: HasOutput, Q: Pick<P>>(
        self,
        _ marker: M,
        _ value: Q,
    ) -> P::Output
}

impl Foo<u8> for S {
    fn foo<P: HasOutput, Q: Pick<P>>(
        self,
        _ marker: u8,
        _ value: Q,
    ) -> P::Output {
        todo()
    }
}

impl Foo<bool> for S {
    fn foo<P: HasOutput, Q: Pick<P>>(
        self,
        _ marker: bool,
        _ value: Q,
    ) -> P::Output {
        todo()
    }
}

fn probe() {
    let s = S {}
    let value: u256 = s.foo(true, Subject {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn deferred_method_return_mismatch_after_bounds_are_solved_is_diagnosed() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "deferred_method_return_mismatch_after_bounds_are_solved_is_diagnosed.fe".into(),
        r#"
trait Pick<T> {}

trait HasOutput {
    type Output
}

struct A {}
struct B {}
struct Subject {}
struct S {}

impl HasOutput for A {
    type Output = u256
}

impl HasOutput for B {
    type Output = bool
}

impl Pick<B> for Subject {}

extern {
    fn todo() -> !
}

trait Foo<M> {
    fn foo<P: HasOutput, Q: Pick<P>>(
        self,
        _ marker: M,
        _ value: Q,
    ) -> P::Output
}

impl Foo<u8> for S {
    fn foo<P: HasOutput, Q: Pick<P>>(
        self,
        _ marker: u8,
        _ value: Q,
    ) -> P::Output {
        todo()
    }
}

impl Foo<bool> for S {
    fn foo<P: HasOutput, Q: Pick<P>>(
        self,
        _ marker: bool,
        _ value: Q,
    ) -> P::Output {
        todo()
    }
}

fn probe() -> u256 {
    let s = S {}
    s.foo(true, Subject {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);

    assert!(
        diagnostics_contain(&diags, "expected `u256`, but `bool` is given"),
        "{diags:#?}",
    );
}

#[test]
fn statically_unreachable_match_arm_uses_result_only_as_inference_fallback() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "statically_unreachable_match_arm_uses_result_only_as_inference_fallback.fe".into(),
        r#"
enum Option<T> {
    Some(T),
    None,
}

fn probe() -> i32 {
    let result = match Option::Some(42) {
        Option::Some(value) => value * 2
        Option::None => 0
    }
    result
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    db.assert_no_diags(top_mod);
}

#[test]
fn statically_unreachable_match_arm_need_not_match_the_inferred_result() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "statically_unreachable_match_arm_need_not_match_the_inferred_result.fe".into(),
        r#"
enum Option<T> {
    Some(T),
    None,
}

fn probe() -> i32 {
    let result = match Option::Some(42) {
        Option::Some(value) => value
        Option::None => false
    }
    result
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    db.assert_no_diags(top_mod);
}

#[test]
fn statically_unreachable_match_arm_does_not_select_the_live_result_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "statically_unreachable_match_arm_does_not_select_the_live_result_type.fe".into(),
        r#"
enum Choice {
    Live,
    Dead,
}

enum Option<T> {
    Some(T),
    None,
}

fn probe() {
    let unresolved = match Choice::Live {
        Choice::Live => Option::None,
        Choice::Dead => Option::Some(1 as u256),
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);

    assert_eq!(diags.len(), 1, "{diags:#?}");
    assert_eq!(diags[0].message, "type annotation is needed", "{diags:#?}");
}

#[test]
fn statically_unreachable_owned_arm_does_not_join_capability_result() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "statically_unreachable_owned_arm_does_not_join_capability_result.fe".into(),
        r#"
enum Choice {
    Live,
    Dead,
}

struct Handle {
    target: mut u256,
}

fn probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }

    handle.target = match Choice::Live {
        Choice::Live => mut second,
        Choice::Dead => 3,
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    db.assert_no_diags(top_mod);
}

#[test]
fn recursive_const_used_as_array_index_reports_diagnostic_without_query_cycle() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "recursive_const_used_as_array_index_reports_diagnostic_without_query_cycle.fe".into(),
        r#"
const INDEX: usize = INDEX

fn main() {
    let values: [u8; 1] = [0]
    values[INDEX]
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);

    assert_eq!(diags.len(), 1, "{diags:#?}");
    assert_eq!(
        diags[0].message, "recursive constant definition",
        "{diags:#?}"
    );
}

#[test]
fn immutable_fields_are_not_required_when_init_has_no_normal_exit() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "immutable_fields_are_not_required_when_init_has_no_normal_exit.fe".into(),
        r#"
fn abort_declared_u256() -> u256 {
    core::panic()
}

fn abort_transitively() -> u256 {
    abort_declared_u256()
}

contract DirectAbort {
    value: u256

    init() {
        core::panic()
    }
}

contract TransitiveAbort {
    value: u256

    init() {
        abort_transitively()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);

    assert!(
        !diagnostics_contain(&diags, "immutable contract field is not initialized"),
        "{diags:#?}"
    );
    assert!(diags.is_empty(), "{diags:#?}");
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

fn diagnostics_contain(diags: &[CompleteDiagnostic], needle: &str) -> bool {
    diags.iter().any(|diag| {
        diag.message.contains(needle)
            || diag
                .sub_diagnostics
                .iter()
                .any(|sub_diag| sub_diag.message.contains(needle))
            || diag.notes.iter().any(|note| note.contains(needle))
    })
}

fn collect_body_props<'db>(
    db: &'db HirAnalysisTestDb,
    body: fe_hir::hir_def::Body<'db>,
    typed_body: &fe_hir::analysis::ty::ty_check::TypedBody<'db>,
    prop_formatter: &mut fe_hir::test_db::HirPropertyFormatter<'db>,
) {
    for expr in body.exprs(db).keys() {
        let span = expr.span(body);
        if span.resolve(db).is_none() {
            continue;
        }

        let ty = typed_body.expr_ty(db, expr);
        prop_formatter.push_prop(
            body.top_mod(db),
            span.into(),
            ty.pretty_print(db).to_string(),
        );
    }

    for pat in body.pats(db).keys() {
        let span = pat.span(body);
        if span.resolve(db).is_none() {
            continue;
        }

        let ty = typed_body.pat_ty(db, pat);
        prop_formatter.push_prop(
            body.top_mod(db),
            span.into(),
            ty.pretty_print(db).to_string(),
        );
    }
}
