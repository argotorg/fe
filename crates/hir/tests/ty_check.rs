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
    fn new(value: T) -> Box<T> {
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
"#,
    );
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
