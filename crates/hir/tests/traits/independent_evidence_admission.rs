use fe_hir::{
    analysis::{
        semantic::{EvalOutcome, GenericSubst, SemConstValue, eval_body_owner_const},
        ty::ty_check::BodyOwner,
    },
    hir_def::IdentId,
    test_db::{HirAnalysisTestDb, find_func},
};
fn diagnose(name: &str, source: &str, cause: &str) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(name.into(), source);
    let (module, _) = db.top_mod(file);
    let mut manager = fe_hir::test_db::initialize_test_analysis_pass();
    let diags: Vec<_> = manager
        .run_on_module(&db, module)
        .into_iter()
        .map(|d| d.to_complete(&db))
        .collect();
    println!("{name}: {diags:#?}");
    assert!(
        diags.iter().any(|d| d.message.contains(cause)),
        "missing intended cause {cause}: {diags:#?}"
    );
}
#[test]
fn private_seal_is_not_visible() {
    diagnose(
        "seal.fe",
        include_str!("admission_controls/source_cannot_construct_an_evidence_marker.fe"),
        "`seal` is not visible",
    );
}
#[test]
fn wrong_output_cannot_relabel_evidence() {
    diagnose(
        "wrong.fe",
        include_str!("admission_controls/wrong_equality.fe"),
        "type mismatch",
    );
}
#[test]
fn generic_forwarding_remains_accepted() {
    let mut db = HirAnalysisTestDb::default();
    let f=db.new_stand_alone("forward.fe".into(),include_str!("admission_controls/evidence_can_be_forwarded_generically_without_introducing_a_generic_goal.fe"));
    let (m, _) = db.top_mod(f);
    db.assert_no_diags(m);
}
#[test]
fn ctfe_payload_preserves_chosen_impl_and_full_goal() {
    let mut db = HirAnalysisTestDb::default();
    let f = db.new_stand_alone(
        "payload.fe".into(),
        include_str!(
            "admission_controls/constant_execution_issues_and_forwards_the_recorded_witness.fe"
        ),
    );
    let (m, _) = db.top_mod(f);
    db.assert_no_diags(m);
    let answer = find_func(&db, m, "answer");
    let owner = BodyOwner::Func(answer);
    let outcome =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    let EvalOutcome::Ready(v) = outcome else {
        panic!("{outcome:?}")
    };
    let SemConstValue::Evidence { ty, application } = v.value(&db) else {
        panic!("compiler-issued payload required")
    };
    assert_eq!(
        *ty,
        fe_hir::analysis::ty::ty_check::check_func_body(&db, answer)
            .1
            .result_ty()
    );
    assert_eq!(application.resolved().trait_inst(), application.goal());
    assert_eq!(application.goal().assoc_type_bindings(&db).len(), 1);
    let checked = application
        .revalidate(
            &db,
            fe_hir::analysis::ty::trait_resolution::TraitSolveCx::new(&db, answer.scope()),
            application.goal(),
        )
        .unwrap();
    assert_eq!(
        checked.associated_type(IdentId::new(&db, "Output")),
        Some(application.goal().assoc_type_bindings(&db)[&IdentId::new(&db, "Output")])
    );
    let expected = fe_hir::analysis::ty::trait_def::prove_selected_hir_application(
        &db,
        fe_hir::analysis::ty::trait_resolution::TraitSolveCx::new(&db, answer.scope()),
        m.all_impl_traits(&db)[0],
        application.goal(),
    )
    .unwrap();
    assert_eq!(
        checked.resolved().selected(),
        expected.resolved().selected()
    );
    assert_eq!(
        checked
            .associated_type(IdentId::new(&db, "Output"))
            .unwrap()
            .pretty_print(&db),
        "u64"
    );
    println!(
        "payload type={}, goal={}, selected original impl preserved",
        ty.pretty_print(&db),
        application.goal().pretty_print(&db, false)
    );
}
#[test]
fn runtime_selected_evidence_requires_compile_time_choice() {
    diagnose(
        "runtime.fe",
        r#"use core::obligations::{Evidence,prove}
trait Read {}
struct Subject {}
impl Read for Subject {}
fn choose(_ flag: bool) -> Evidence<Read<Subject>> { if flag { prove<Read<Subject>>() } else { prove<Read<Subject>>() } }
"#,
        "evidence choice must be known at compile time",
    );
}

#[test]
fn const_parameter_choice_and_runtime_use_remain_accepted() {
    let mut db = HirAnalysisTestDb::default();
    let f = db.new_stand_alone(
        "const_choice.fe".into(),
        r#"use core::obligations::{Evidence,prove}
trait Read {}
struct Subject {}
impl Read for Subject {}
const fn choose(_ flag: bool) -> Evidence<Read<Subject>> {
 if flag { prove<Read<Subject>>() } else { prove<Read<Subject>>() }
}
fn use_witness(_ flag: bool, _ evidence: Evidence<Read<Subject>>) -> u64 {
 if flag { 1 } else { 2 }
}
"#,
    );
    let (m, _) = db.top_mod(f);
    db.assert_no_diags(m);
}
#[test]
fn runtime_match_cannot_select_evidence_in_a_tuple() {
    diagnose(
        "runtime_match.fe",
        r#"use core::obligations::{Evidence,prove}
trait Read {}
struct Subject {}
impl Read for Subject {}
fn choose(_ flag: bool) -> (Evidence<Read<Subject>>, u64) {
 match flag { true => (prove<Read<Subject>>(), 1), false => (prove<Read<Subject>>(), 2) }
}
"#,
        "evidence choice must be known at compile time",
    );
}

#[test]
fn mutable_evidence_local_is_rejected_at_the_binding() {
    diagnose(
        "mutable.fe",
        r#"use core::obligations::{Evidence,prove}
trait Read {}
struct Subject {}
impl Read for Subject {}
fn choose() -> Evidence<Read<Subject>> {
 let mut witness = prove<Read<Subject>>()
 witness
}
"#,
        "evidence cannot be stored in a mutable binding",
    );
}
#[test]
fn runtime_argument_cannot_select_const_factory_result() {
    diagnose(
        "runtime_argument.fe",
        r#"use core::obligations::{Evidence,prove}
trait Read {}
struct Subject {}
impl Read for Subject {}
const fn factory(_ flag: bool) -> Evidence<Read<Subject>> {
 if flag { prove<Read<Subject>>() } else { prove<Read<Subject>>() }
}
fn choose(_ flag: bool) -> Evidence<Read<Subject>> { factory(flag) }
"#,
        "evidence choice must be known at compile time",
    );
}

#[test]
fn accepted_source_diagnostic_code_and_runtime_selector_spans_are_preserved() {
    for (name, source, fault) in [
        (
            "runtime_branch.fe",
            include_str!("admission_controls/policy_runtime_branch.fe"),
            "flag",
        ),
        (
            "ordinary_return_call.fe",
            include_str!("admission_controls/policy_ordinary_return_call.fe"),
            "source()",
        ),
        (
            "const_call_runtime_argument.fe",
            include_str!("admission_controls/policy_const_call_runtime_argument.fe"),
            "flag",
        ),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(name.into(), source);
        let (module, _) = db.top_mod(file);
        let mut manager = fe_hir::test_db::initialize_test_analysis_pass();
        let diagnostics: Vec<_> = manager
            .run_on_module(&db, module)
            .into_iter()
            .map(|diag| diag.to_complete(&db))
            .collect();
        let expected_start = source.rfind(fault).unwrap();
        assert!(
            diagnostics.iter().any(|diag| diag.message
                == "evidence choice must be known at compile time"
                && diag.error_code.local_code == 120
                && diag
                    .sub_diagnostics
                    .iter()
                    .any(|label| label.span.as_ref().is_some_and(|span| u32::from(
                        span.range.start()
                    )
                        as usize
                        == expected_start
                        && u32::from(span.range.end()) as usize == expected_start + fault.len()))),
            "original source cause/code and precise offending source required for {name}: {diagnostics:#?}"
        );
    }
}
