use fe_hir::test_db::{HirAnalysisTestDb, initialize_test_analysis_pass};
fn diagnostics(source: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("review1797.fe".into(), source);
    let (module, _) = db.top_mod(file);
    let mut manager = initialize_test_analysis_pass();
    let diags: Vec<_> = manager
        .run_on_module(&db, module)
        .into_iter()
        .map(|d| d.to_complete(&db))
        .collect();
    format!("{diags:#?}")
}
#[test]
fn full_goal_equality_forwarding_rejects_relabel_and_accepts_identical_goal() {
    let source = include_str!("pr1797_review_controls/full-goal-wrong.fe");
    let actual = diagnostics(source);
    println!("WRONG GOAL {actual}");
    assert!(
        actual.contains("the opened evidence does not prove the requested associated equality"),
        "wrong equality must preserve its specific rejection cause: {actual}"
    );
    assert_eq!(
        diagnostics(&source.replace("Output = bool", "Output = u64")),
        "[]"
    );
}
#[test]
fn mutable_evidence_formal_is_rejected_but_ordinary_mutable_is_valid() {
    let actual = diagnostics(include_str!("pr1797_review_controls/mutable-formal.fe"));
    println!("MUTABLE FORMAL {actual}");
    assert!(
        actual.contains("evidence cannot be stored in a mutable binding"),
        "{actual}"
    );
    assert_eq!(
        diagnostics("fn ordinary(mut value: own u256) -> u256 { value = 2\n value }"),
        "[]"
    );
}

#[test]
fn constant_evidence_normalizes_layout_like_direct_factory() {
    let source = include_str!("pr1797_review_controls/constant-layout.fe");
    let twin = source.replace("with (W)", "with (wide())");
    assert_eq!(diagnostics(&twin), "[]", "direct factory twin valid");
    let actual = diagnostics(source);
    println!("CONSTANT LAYOUT {actual}");
    assert_eq!(actual, "[]");
}
#[test]
fn closed_factory_arguments_normalize_layout_like_direct_named_choice() {
    let source = include_str!("pr1797_review_controls/closed-factory-layout.fe");
    let twin = source.replace("with (wide(true))", "with (Wide)");
    assert_eq!(diagnostics(&twin), "[]", "direct named twin valid");
    let actual = diagnostics(source);
    println!("CLOSED FACTORY LAYOUT {actual}");
    assert_eq!(actual, "[]");
}
#[test]
fn nested_static_match_agrees_with_direct_match_and_runtime_match_is_rejected() {
    let source = include_str!("pr1797_review_controls/nested-match.fe");
    let direct = format!(
        "{}\nconst fn direct() -> Evidence<Goal> {{ match true {{ true => first(), false => second() }} }}",
        source.replace(
            "forward(match true { true => first(), false => second() })",
            "direct()"
        )
    );
    assert_eq!(diagnostics(&direct), "[]", "direct known match valid");
    let actual = diagnostics(source);
    println!("NESTED MATCH {actual}");
    assert_eq!(actual, "[]");
    let unknown = source
        .replace("fn check()", "fn check(_ flag: bool)")
        .replace("match true", "match flag");
    assert_ne!(
        diagnostics(&unknown),
        "[]",
        "runtime choice remains rejected"
    );
}

#[test]
fn generic_evidence_cannot_invent_an_associated_equality() {
    let source = "use core::obligations::{Evidence, prove}\ntrait Produce { type Output }\nfn forward<T>(_ witness: Evidence<Produce<T>>) -> Evidence<Produce<T, Output = bool>> { with (witness) { prove<Produce<T, Output = bool>>() } }\n";
    let actual = diagnostics(source);
    println!("GENERIC WRONG GOAL {actual}");
    assert!(
        actual.contains("the opened evidence does not prove the requested associated equality"),
        "{actual}"
    );
    assert_eq!(
        diagnostics(&source.replace(
            "Evidence<Produce<T>>",
            "Evidence<Produce<T, Output = bool>>"
        )),
        "[]"
    );
}

fn evaluate_known_control(source: &str, expected: &str) {
    use fe_hir::analysis::{
        semantic::{EvalOutcome, GenericSubst, eval_body_owner_const},
        ty::ty_check::BodyOwner,
    };
    use fe_hir::test_db::find_func;
    let source = source
        .replace("fn consume", "const fn consume")
        .replace("fn check", "const fn check");
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("review1797-execution.fe".into(), &source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "check"));
    let result =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    println!("KNOWN SOURCE EXECUTION {result:?}");
    let EvalOutcome::Ready(value) = result else {
        panic!("actual execution required: {result:?}");
    };
    assert_eq!(value.pretty_print(&db), expected);
}

#[test]
fn constant_evidence_layout_executes_selected_width() {
    evaluate_known_control(
        include_str!("pr1797_review_controls/constant-layout.fe"),
        "2",
    );
}
#[test]
fn closed_factory_layout_executes_selected_width() {
    evaluate_known_control(
        include_str!("pr1797_review_controls/closed-factory-layout.fe"),
        "2",
    );
}
#[test]
fn nested_known_match_executes_both_selected_alternatives() {
    let source = include_str!("pr1797_review_controls/nested-match.fe");
    evaluate_known_control(source, "2");
    evaluate_known_control(&source.replace("match true", "match false"), "3");
}

#[test]
fn shared_recovery_closed_if_uses_selected_method_and_both_branches() {
    use fe_hir::{
        analysis::{
            semantic::{EvalOutcome, GenericSubst, eval_body_owner_const},
            ty::ty_check::BodyOwner,
        },
        test_db::find_func,
    };
    let source = include_str!("pr1797_review_controls/closed-if-selected-method.fe");
    for (text, expected) in [
        (source.to_string(), "15"),
        (source.replace("if true", "if false"), "25"),
    ] {
        // The production fixture retains its native test entry. This control
        // returns its result through the explicit CTFE test API, whose roots
        // must be const and cannot call the native assertion function.
        let text = text
            .replace(
                "#[test]\nfn test_default_method_hijack()",
                "const fn test_default_method_hijack() -> u256",
            )
            .replace("assert(actual == 15)", "actual");
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("closed-if-selected-method.fe".into(), &text);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let owner = BodyOwner::Func(find_func(&db, module, "test_default_method_hijack"));
        let outcome =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let EvalOutcome::Ready(value) = outcome else {
            panic!("selected {expected} must execute: {outcome:?}");
        };
        assert_eq!(value.pretty_print(&db), expected);
    }
}

#[test]
fn shared_recovery_reproves_actual_closed_evidence() {
    evaluate_known_control(
        include_str!("pr1797_review_controls/reprove-closed-evidence.fe"),
        "2",
    );
}
