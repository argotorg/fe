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
    assert_ne!(
        actual, "[]",
        "wrong associated equality must be rejected before erasure"
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
