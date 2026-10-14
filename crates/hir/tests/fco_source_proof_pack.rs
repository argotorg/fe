use fe_hir::{
    analysis::{
        semantic::{EvalOutcome, GenericSubst, SemConstValue, eval_body_owner_const},
        ty::ty_check::BodyOwner,
    },
    test_db::{HirAnalysisTestDb, find_func, initialize_test_analysis_pass},
};
use std::path::PathBuf;

fn source(file: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fco_source_proof_controls")
        .join(file);
    std::fs::read_to_string(path).unwrap()
}

fn positive(file: &str, expected_equalities: usize, markers: bool) {
    let mut db = HirAnalysisTestDb::default();
    let text = source(file);
    let input = db.new_stand_alone(file.into(), &text);
    let (module, _) = db.top_mod(input);
    db.assert_no_diags(module);
    let issued = find_func(&db, module, "issued");
    let owner = BodyOwner::Func(issued);
    let outcome =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    let EvalOutcome::Ready(value) = outcome else {
        panic!("{file}: {outcome:?}")
    };
    let SemConstValue::Evidence { ty, application } = value.value(&db) else {
        panic!("{file}: actual compiler-issued payload required, got {value:?}")
    };
    assert_eq!(
        *ty,
        fe_hir::analysis::ty::ty_check::check_func_body(&db, issued)
            .1
            .result_ty()
    );
    assert_eq!(application.goal(), application.resolved().trait_inst());
    assert_eq!(
        application.goal().assoc_type_bindings(&db).len(),
        expected_equalities
    );
    let result = find_func(&db, module, "result");
    let owner = BodyOwner::Func(result);
    let outcome =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    let EvalOutcome::Ready(number) = outcome else {
        panic!("{file}: {outcome:?}")
    };
    assert_eq!(number.pretty_print(&db), "37");
    if markers {
        let local = find_func(&db, module, "prove");
        let owner = BodyOwner::Func(local);
        let EvalOutcome::Ready(value) =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]))
        else {
            panic!("local prove must execute ordinarily")
        };
        assert_eq!(value.pretty_print(&db), "9");
        let marker = find_func(&db, module, "marker_value");
        let owner = BodyOwner::Func(marker);
        let EvalOutcome::Ready(value) =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]))
        else {
            panic!("local marker must evaluate")
        };
        let SemConstValue::Struct { fields, .. } = value.value(&db) else {
            panic!("local Evidence spelling must not issue compiler evidence")
        };
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].pretty_print(&db), "false");
        let marker = find_func(&db, module, "marker_number");
        let owner = BodyOwner::Func(marker);
        let EvalOutcome::Ready(value) =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]))
        else {
            panic!("local Constraint record must evaluate")
        };
        let SemConstValue::Struct { fields, .. } = value.value(&db) else {
            panic!("local Constraint must stay an ordinary record")
        };
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].pretty_print(&db), "5");
    }
    println!(
        "CONTROL {file}: SOURCE_ACCEPTED; ACTUAL_EVIDENCE_PAYLOAD; equalities={expected_equalities}; CTFE_RESULT=37; markers={markers}"
    );
}

fn negative(file: &str, message: &str, reason: Option<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let text = source(file);
    let input = db.new_stand_alone(file.into(), &text);
    let (module, _) = db.top_mod(input);
    let diags: Vec<_> = initialize_test_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|diag| diag.to_complete(&db))
        .collect();
    println!("CONTROL {file}: DIAGNOSTICS {diags:#?}");
    assert!(
        diags.iter().any(|diag| diag.message == message
            && reason.is_none_or(|reason| format!("{diag:?}").contains(reason))),
        "expected exact causal rejection for {file}: {diags:#?}"
    );
    let fault = if file.ends_with("marker-counterfeit.fe") {
        "Evidence<Goal> { seal: false }"
    } else {
        "prove<Goal>()"
    };
    let start = text.find(fault).expect("independent source fault locator");
    let end = start + fault.len();
    assert!(
        diags
            .iter()
            .filter(|diag| diag.message == message)
            .any(|diag| diag.sub_diagnostics.iter().any(|sub| sub
                .span
                .as_ref()
                .is_some_and(|span| u32::from(span.range.start()) as usize == start
                    && u32::from(span.range.end()) as usize == end))),
        "{file}: intended source fault {start}..{end} must receive the rejection"
    );
    println!("CONTROL {file}: EXPECTED_CAUSE_AND_SOURCE_RANGE={start}..{end}");
}

#[test]
fn ordinary_goal_issues_a_real_const_payload() {
    positive("positive/ordinary.fe", 1, false)
}
#[test]
fn generic_forwarding_retains_the_real_const_payload() {
    positive("positive/generic-forward.fe", 1, false)
}
#[test]
fn ordinary_source_markers_keep_nominal_identity() {
    positive("positive/marker-identity.fe", 0, true)
}
#[test]
fn missing_prerequisite_cannot_issue_evidence() {
    negative(
        "negative/absent-prerequisite.fe",
        "trait evidence introduction failed",
        Some("not proved"),
    )
}
#[test]
fn incompatible_associated_output_cannot_issue_evidence() {
    negative(
        "negative/incompatible-output.fe",
        "trait evidence introduction failed",
        Some("not proved"),
    )
}
#[test]
fn ordinary_marker_cannot_become_core_evidence() {
    negative("negative/marker-counterfeit.fe", "type mismatch", None)
}
