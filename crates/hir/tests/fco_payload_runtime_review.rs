//! Isolated controls over frozen payload checkpoint 7d3410055.
use fe_hir::{
    analysis::ty::{trait_resolution::TraitSolveCx, ty_check::check_func_body},
    test_db::{HirAnalysisTestDb, find_func},
};
#[test]
fn concrete_forwarding_retains_one_checked_origin_decision() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "fco_review_forward.fe".into(),
        include_str!("fco_payload_controls/forward.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let function = find_func(&db, module, "witness");
    let (_, typed) = check_func_body(&db, function);
    let body = function.body(&db).unwrap();
    let retained = body
        .exprs(&db)
        .keys()
        .filter_map(|e| typed.callable_expr(e))
        .filter_map(|c| c.evidence_introduction())
        .collect::<Vec<_>>();
    assert_eq!(retained.len(), 1);
    let record = retained[0];
    assert_eq!(record.resolved().trait_inst(), record.goal());
    let checked = record
        .revalidate(&db, TraitSolveCx::new(&db, function.scope()), record.goal())
        .unwrap();
    assert_eq!(checked.resolved().selected(), record.resolved().selected());
}
#[test]
fn absence_and_output_mismatch_keep_not_proven_cause() {
    for (name, source) in [
        ("missing", include_str!("fco_payload_controls/missing.fe")),
        (
            "output",
            include_str!("fco_payload_controls/wrong_output.fe"),
        ),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(format!("fco_review_{name}.fe").into(), source);
        let (module, _) = db.top_mod(file);
        let mut manager = fe_hir::test_db::initialize_test_analysis_pass();
        let diags = manager
            .run_on_module(&db, module)
            .into_iter()
            .map(|d| d.to_complete(&db))
            .collect::<Vec<_>>();
        eprintln!("NEGATIVE_CAUSE_SPANS {name}: {diags:#?}");
        assert!(
            diags
                .iter()
                .any(|d| d.message == "trait evidence introduction failed"
                    && format!("{d:?}").contains("the requested trait goal was not proved")),
            "{name}: {diags:#?}"
        );
    }
}
#[test]
fn generic_introduction_stays_distinct_from_supported_forwarding() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "fco_review_generic.fe".into(),
        include_str!("fco_payload_controls/generic_introduction.fe"),
    );
    let (module, _) = db.top_mod(file);
    let mut manager = fe_hir::test_db::initialize_test_analysis_pass();
    let diags = manager
        .run_on_module(&db, module)
        .into_iter()
        .map(|d| d.to_complete(&db))
        .collect::<Vec<_>>();
    assert!(
        diags
            .iter()
            .any(|d| d.message == "trait evidence introduction failed"
                && format!("{d:?}").contains("expected one concrete trait goal")),
        "{diags:#?}"
    );
}

#[test]
fn issued_forwarding_keeps_actual_checked_application_and_origin() {
    use fe_hir::analysis::{
        semantic::{EvalOutcome, GenericSubst, SemConstValue, eval_body_owner_const},
        ty::ty_check::BodyOwner,
    };
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "fco_review_issued.fe".into(),
        include_str!("fco_payload_controls/issued.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let first = find_func(&db, module, "first");
    let forwarded = find_func(&db, module, "forwarded");
    let eval = |func| {
        let owner = BodyOwner::Func(func);
        let EvalOutcome::Ready(value) =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]))
        else {
            panic!("issued execution")
        };
        value
    };
    let issued = eval(first);
    let carried = eval(forwarded);
    let SemConstValue::Evidence {
        application: origin,
        ty,
    } = issued.value(&db)
    else {
        panic!("issued payload")
    };
    let SemConstValue::Evidence {
        application: transported,
        ty: carried_ty,
    } = carried.value(&db)
    else {
        panic!("forwarded payload")
    };
    assert_eq!(ty, carried_ty);
    assert_eq!(origin, transported);
    assert_eq!(origin.resolved().trait_inst(), origin.goal());
    assert_eq!(origin.goal().assoc_type_bindings(&db).len(), 1);
    let (_, typed) = check_func_body(&db, first);
    let body = first.body(&db).unwrap();
    let record = body
        .exprs(&db)
        .keys()
        .filter_map(|e| typed.callable_expr(e))
        .find_map(|c| c.evidence_introduction())
        .unwrap();
    assert_eq!(origin.resolved().selected(), record.resolved().selected());
    assert_eq!(origin.origin_context(), record.origin_context());
}

#[test]
fn intrinsic_registration_preserves_sizeof_and_user_prove_call() {
    use fe_hir::analysis::{
        semantic::{
            EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const,
        },
        ty::ty_check::{BodyOwner, SemanticExprLowering},
    };
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "intrinsic_twins.fe".into(),
        include_str!("fco_payload_controls/intrinsic_twins.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let named = find_func(&db, module, "same_spelling");
    let (_, typed) = check_func_body(&db, named);
    let body = named.body(&db).unwrap();
    let calls = body
        .exprs(&db)
        .keys()
        .filter_map(|expr| typed.semantic_expr_lowering(expr))
        .collect::<Vec<_>>();
    assert_eq!(calls.len(), 1);
    assert!(
        matches!(calls[0], SemanticExprLowering::Call { callable } if callable.evidence_introduction().is_none())
    );
    for (name, expected) in [("same_spelling", 7), ("ordinary_size", 8)] {
        let owner = BodyOwner::Func(find_func(&db, module, name));
        let outcome =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let EvalOutcome::Ready(value) = outcome else {
            panic!("{outcome:?}")
        };
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value: actual },
            ..
        } = value.value(&db)
        else {
            panic!("scalar control")
        };
        assert_eq!(actual.to_string(), expected.to_string());
    }
}
