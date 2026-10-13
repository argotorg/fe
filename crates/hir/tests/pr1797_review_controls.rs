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

#[test]
fn captured_associated_constant_executes_closed_choices() {
    use fe_hir::{
        analysis::{
            semantic::{EvalOutcome, GenericSubst, eval_body_owner_const},
            ty::ty_check::BodyOwner,
        },
        test_db::find_func,
    };
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "closed-associated-constant.fe".into(),
        include_str!("pr1797_review_controls/phase-aware-associated-constant.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for (name, expected) in [("known_true", "2"), ("known_false", "3")] {
        let owner = BodyOwner::Func(find_func(&db, module, name));
        let result =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let EvalOutcome::Ready(value) = result else {
            panic!("closed choice {name} must execute: {result:?}")
        };
        assert_eq!(value.pretty_print(&db), expected);
    }
}

#[test]
fn captured_associated_constant_rejects_runtime_choice_with_source_cause() {
    use fe_hir::{
        analysis::{
            diagnostics::DiagnosticVoucher,
            semantic::{
                get_or_build_semantic_instance, identity_semantic_instance_key,
                normalize_runtime_semantic_body,
            },
            ty::ty_check::BodyOwner,
        },
        test_db::find_func,
    };
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "runtime-associated-constant.fe".into(),
        include_str!("pr1797_review_controls/phase-aware-associated-constant.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "choose"));
    let instance = get_or_build_semantic_instance(&db, identity_semantic_instance_key(&db, owner));
    let error = normalize_runtime_semantic_body(&db, instance).unwrap_err();
    let diagnostic = error
        .diagnostic()
        .expect("runtime source rejection retains a diagnostic")
        .to_complete(&db);
    assert_eq!(diagnostic.error_code.local_code, 120);
    assert_eq!(format!("{:?}", diagnostic.error_code.pass), "TyCheck");
    assert_eq!(
        diagnostic.message,
        "evidence choice must be known at compile time"
    );
    let source = include_str!("pr1797_review_controls/phase-aware-associated-constant.fe");
    let start = source.find("with (w)").expect("authored opening") + "with (".len();
    let primary = diagnostic.sub_diagnostics.first().expect("source label");
    assert_eq!(
        primary.message,
        "this runtime source cannot select an evidence value"
    );
    let range = primary.span.as_ref().expect("authored source span").range;
    assert_eq!(u32::from(range.start()) as usize, start);
    assert_eq!(u32::from(range.end()) as usize, start + 1);
}

#[test]
fn ordinary_evidence_transport_retains_selected_choice() {
    use fe_hir::{
        analysis::{
            semantic::{
                SConst, get_or_build_semantic_instance, identity_semantic_instance_key,
                normalize_runtime_semantic_body,
                normalized::{NExpr, NTerminatorKind},
            },
            ty::ty_check::{BodyOwner, ReturnProvenance},
        },
        test_db::find_func,
    };
    let source = include_str!("pr1797_review_controls/nonconst-transport.fe");
    assert_eq!(diagnostics(source), "[]");
    // Keep the transport and caller ordinary. Const methods let existing
    // normalization expose the actual selected result in the runtime body.
    let source = source.replace("fn value()", "const fn value()");
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("nonconst-transport.fe".into(), &source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let transport = BodyOwner::Func(find_func(&db, module, "non_const_id"));
    let transport_key = identity_semantic_instance_key(&db, transport);
    assert!(
        matches!(transport_key.typed_body(&db).return_provenance(&db), ReturnProvenance::Forwarded(sources) if sources.len() == 1)
    );
    let owner = BodyOwner::Func(find_func(&db, module, "check"));
    let instance = get_or_build_semantic_instance(&db, identity_semantic_instance_key(&db, owner));
    let normalized =
        normalize_runtime_semantic_body(&db, instance).expect("ordinary transport normalizes");
    let mut returns = 0;
    for block in &normalized.body.blocks {
        if let NTerminatorKind::Return(Some(operand)) = block.terminator.kind {
            let mut value = operand.value;
            loop {
                match normalized
                    .body
                    .defining_expr(value)
                    .expect("defined return value")
                    .1
                {
                    NExpr::Forward { src } => value = src.value,
                    NExpr::Const(SConst::Value(value)) => {
                        assert_eq!(value.value().pretty_print(&db), "2");
                        break;
                    }
                    other => panic!("selected method must normalize to its result: {other:?}"),
                }
            }
            returns += 1;
        }
    }
    assert_eq!(returns, 1);
}

#[test]
fn boxed_evidence_relay_preserves_ordinary_and_named_records() {
    let source = include_str!("pr1797_review_controls/boxed-relay.fe");
    assert_eq!(diagnostics(source), "[]");
    let source = source.split("#[test]").next().expect("declarations");
    let source = source
        .replace("fn score", "const fn score")
        .replace("fn relay", "const fn relay")
        .replace("fn local_repack", "const fn local_repack");
    for (call, expected) in [
        ("relay(prove<Score<Plain>>(), Plain { x: 7 })", "7"),
        (
            "with (Double) { relay(prove<Score<Plain>>(), Plain { x: 7 }) }",
            "14",
        ),
        (
            "with (Double) { local_repack(prove<Score<Plain>>(), Plain { x: 7 }) }",
            "14",
        ),
    ] {
        evaluate_known_control(
            &format!("{source}\nfn check() -> u256 {{ {call} }}"),
            expected,
        );
    }
}

#[test]
fn boxed_parameter_templates_wait_for_witness_delivery() {
    use fe_hir::analysis::semantic::collect_semantic_borrow_diagnostic_vouchers;
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "boxed-template.fe".into(),
        include_str!("pr1797_review_controls/boxed-relay.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, module)
        .into_iter()
        .map(|diagnostic| diagnostic.to_complete(&db))
        .collect::<Vec<_>>();
    assert!(
        diagnostics.is_empty(),
        "templates retain pending witness delivery while executable roots validate: {diagnostics:#?}"
    );
}

#[test]
fn selected_array_layout_normalizes_with_delivered_lexical_record() {
    use fe_hir::{
        analysis::{
            semantic::{
                get_or_build_semantic_instance, identity_semantic_instance_key,
                normalize_runtime_semantic_body,
            },
            ty::ty_check::BodyOwner,
        },
        test_db::find_func,
    };
    let source = include_str!("pr1797_review_controls/constant-layout.fe");
    for source in [source.to_owned(), source.replace("with (W)", "with (Wide)")] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("selected-array-layout.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let owner = BodyOwner::Func(find_func(&db, module, "check"));
        let instance =
            get_or_build_semantic_instance(&db, identity_semantic_instance_key(&db, owner));
        normalize_runtime_semantic_body(&db, instance)
            .expect("selected array size must agree across the call boundary");
    }
}

#[test]
fn const_choice_definition_defers_only_until_runtime_use() {
    use fe_hir::analysis::semantic::collect_semantic_borrow_diagnostic_vouchers;
    let original = include_str!("pr1797_review_controls/phase-aware-associated-constant.fe");
    let closed = original.split("#[test]").next().unwrap().to_owned()
        + "\nconst LEFT: u256 = choose(true)\nconst RIGHT: u256 = choose(false)\n#[test]\nfn closed_only() { assert(LEFT == 2)\nassert(RIGHT == 3) }\n";
    let literal_calls = original.split("#[test]").next().unwrap().to_owned()
        + "\nconst fn closed_helper() -> u256 { choose(true) }\n#[test]\nfn literal_calls() { assert(choose(true) == 2)\nassert(choose(false) == 3)\nassert(closed_helper() == 2) }\n";
    for (source, runtime) in [
        (closed.as_str(), false),
        (literal_calls.as_str(), false),
        (original, true),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("choice-definition-use.fe".into(), source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, module)
            .into_iter()
            .map(|diagnostic| diagnostic.to_complete(&db))
            .collect::<Vec<_>>();
        if runtime {
            assert!(
                diagnostics
                    .iter()
                    .any(|diagnostic| diagnostic.error_code.local_code == 120
                        && diagnostic.message == "evidence choice must be known at compile time"),
                "actual runtime use must retain source cause: {diagnostics:#?}"
            );
        } else {
            assert!(
                diagnostics.is_empty(),
                "closed CTFE uses do not execute the identity definition: {diagnostics:#?}"
            );
        }
    }
}
