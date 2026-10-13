//! Review observations over the lead's actual representation; no new evidence type.
use fe_mir::{
    Layout, RuntimeCarrier, RuntimeClass, build_runtime_package, format_runtime_body,
    verify_runtime_body,
};
use hir::{
    analysis::{
        semantic::{EvalOutcome, GenericSubst, SemConstId, SemConstValue, eval_body_owner_const},
        ty::{
            const_ty::{ConstTyData, ConstTyId},
            ty_check::BodyOwner,
            ty_def::{TyData, TyId},
        },
    },
    test_db::{HirAnalysisTestDb, find_func},
};
fn issued<'db>(
    db: &'db HirAnalysisTestDb,
    module: hir::hir_def::TopLevelMod<'db>,
    name: &str,
) -> SemConstId<'db> {
    let owner = BodyOwner::Func(find_func(db, module, name));
    let result = eval_body_owner_const(db, owner, GenericSubst::for_body_owner(db, owner, vec![]));
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    value
}
#[test]
fn observe_payload_identity_without_inventing_a_stable_key_policy() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "fco_identity.fe".into(),
        include_str!("fco_payload_controls/issued.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let first = issued(&db, module, "first");
    let second = issued(&db, module, "second");
    let SemConstValue::Evidence { application: a, .. } = first.value(&db) else {
        unreachable!()
    };
    let SemConstValue::Evidence { application: b, .. } = second.value(&db) else {
        unreachable!()
    };
    assert_eq!(a.goal(), b.goal());
    assert_eq!(a.resolved().selected(), b.resolved().selected());
    assert_ne!(a.origin_context(), b.origin_context());
    assert_ne!(
        first, second,
        "existing payload IDs retain distinct source applications"
    );
    // Matches the existing const_ty_from_sem_const Evidence -> Description route.
    let a_ty = TyId::new(
        &db,
        TyData::ConstTy(ConstTyId::new(&db, ConstTyData::Description(first))),
    );
    let b_ty = TyId::new(
        &db,
        TyData::ConstTy(ConstTyId::new(&db, ConstTyData::Description(second))),
    );
    let ak = fe_mir::runtime::stable_key::type_identity(&db, a_ty);
    let bk = fe_mir::runtime::stable_key::type_identity(&db, b_ty);
    eprintln!(
        "PAYLOAD_KEY_OBSERVATION {{\"distinct_origin\":true,\"distinct_payload\":true,\"same_type_key\":{},\"key_a\":{:?},\"key_b\":{:?}}}",
        ak == bk,
        ak,
        bk
    );
    // Equality of these identities is observation, not an invented expected-result
    // change. Lead must specify whether origin belongs to runtime type identity.
}
#[test]
fn actual_payload_runtime_lowering_is_verified_and_recorded() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "fco_runtime_evidence.fe".into(),
        include_str!("fco_payload_controls/issued.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let package = build_runtime_package(&db, module).unwrap();
    let program: &dyn fe_mir::MirDb = &db;
    let mut inspected = 0;
    for function in package.functions(&db) {
        let instance = function.instance(&db);
        let Some(sem) = instance.key(&db).semantic(&db) else {
            continue;
        };
        let BodyOwner::Func(func) = sem.key(&db).owner(&db) else {
            continue;
        };
        let Some(name) = func.name(&db).to_opt() else {
            continue;
        };
        if !["first", "second", "forwarded"].contains(&name.data(&db).as_str()) {
            continue;
        }
        let body = instance.body(&db);
        verify_runtime_body(&db, &program, &body).unwrap();
        assert_eq!(body.stmt_origins.len(), body.blocks.len());
        for (b, block) in body.blocks.iter().enumerate() {
            assert_eq!(body.stmt_origins[b].len(), block.stmts.len());
        }
        assert_eq!(body.terminator_origins.len(), body.blocks.len());
        let runtime_value_locals = body
            .locals
            .iter()
            .filter(|l| !matches!(l.carrier, RuntimeCarrier::Erased))
            .count();
        let mut seal_fields = 0;
        if let Some(RuntimeClass::AggregateValue { layout }) = &body.signature.ret
            && let Layout::Struct(fields) = layout.data(&db)
        {
            seal_fields = fields.fields.len();
        }
        eprintln!(
            "RUNTIME_EVIDENCE_OBSERVATION {{\"function\":{:?},\"runtime_value_locals\":{},\"return_fields\":{},\"verified\":true}}",
            name.data(&db),
            runtime_value_locals,
            seal_fields
        );
        eprintln!("{}", format_runtime_body(&db, &body));
        // No assertion that a bool seal is erased: record the actual interface,
        // then require the lead's erasure/allowed-representation contract.
        inspected += 1;
    }
    assert_eq!(
        inspected, 3,
        "all concrete source payload consumers must be inspected"
    );
}

#[test]
fn constant_payload_runtime_lowering_is_verified_and_recorded() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "fco_runtime_evidence.fe".into(),
        include_str!("fco_payload_controls/constant_consumer.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let package = build_runtime_package(&db, module).unwrap();
    let program: &dyn fe_mir::MirDb = &db;
    let mut inspected = 0;
    for function in package.functions(&db) {
        let instance = function.instance(&db);
        let Some(sem) = instance.key(&db).semantic(&db) else {
            continue;
        };
        let BodyOwner::Func(func) = sem.key(&db).owner(&db) else {
            continue;
        };
        let Some(name) = func.name(&db).to_opt() else {
            continue;
        };
        if !["consumed"].contains(&name.data(&db).as_str()) {
            continue;
        }
        let body = instance.body(&db);
        verify_runtime_body(&db, &program, &body).unwrap();
        assert_eq!(body.stmt_origins.len(), body.blocks.len());
        for (b, block) in body.blocks.iter().enumerate() {
            assert_eq!(body.stmt_origins[b].len(), block.stmts.len());
        }
        assert_eq!(body.terminator_origins.len(), body.blocks.len());
        let runtime_value_locals = body
            .locals
            .iter()
            .filter(|l| !matches!(l.carrier, RuntimeCarrier::Erased))
            .count();
        let mut seal_fields = 0;
        if let Some(RuntimeClass::AggregateValue { layout }) = &body.signature.ret
            && let Layout::Struct(fields) = layout.data(&db)
        {
            seal_fields = fields.fields.len();
        }
        eprintln!(
            "RUNTIME_EVIDENCE_OBSERVATION {{\"function\":{:?},\"runtime_value_locals\":{},\"return_fields\":{},\"verified\":true}}",
            name.data(&db),
            runtime_value_locals,
            seal_fields
        );
        eprintln!("{}", format_runtime_body(&db, &body));
        // No assertion that a bool seal is erased: record the actual interface,
        // then require the lead's erasure/allowed-representation contract.
        inspected += 1;
    }
    assert_eq!(
        inspected, 1,
        "all concrete source payload consumers must be inspected"
    );
}

#[test]
fn ordinary_forwarding_consumes_retained_introduction_before_borrow_admission() {
    use hir::analysis::{
        semantic::{SConst, SExpr, SStmtKind, SemOrigin},
        ty::ty_check::check_func_body,
    };
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "ordinary_forwarding.fe".into(),
        include_str!("fco_payload_controls/forward.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let witness = find_func(&db, module, "witness");
    let (_, typed) = check_func_body(&db, witness);
    let source = witness.body(&db).unwrap();
    let (source_expr, record) = source
        .exprs(&db)
        .keys()
        .find_map(|expr| {
            typed
                .callable_expr(expr)
                .and_then(|call| call.evidence_introduction())
                .map(|record| (expr, record))
        })
        .unwrap();
    let package = build_runtime_package(&db, module).unwrap();
    let program: &dyn fe_mir::MirDb = &db;
    let mut seen_witness = false;
    let mut seen_forward = false;
    for function in package.functions(&db) {
        let instance = function.instance(&db);
        let Some(sem) = instance.key(&db).semantic(&db) else {
            continue;
        };
        let BodyOwner::Func(func) = sem.key(&db).owner(&db) else {
            continue;
        };
        let body = instance.body(&db);
        verify_runtime_body(&db, &program, &body).unwrap();
        if func == witness {
            seen_witness = true;
            let semantic = sem.body(&db);
            let payload = semantic
                .blocks
                .iter()
                .flat_map(|block| &block.stmts)
                .find_map(|stmt| {
                    let SStmtKind::Assign {
                        expr: SExpr::Const(SConst::Value(value)),
                        ..
                    } = &stmt.kind
                    else {
                        return None;
                    };
                    let SemConstValue::Evidence { application, .. } = value.value().value(&db)
                    else {
                        return None;
                    };
                    Some((stmt.origin, *application))
                })
                .expect("the admitted introduction must produce the existing evidence constant");
            assert_eq!(payload.0, SemOrigin::Expr(source_expr));
            assert_eq!(payload.1, record);
            assert!(semantic.callees().iter().all(|callee| {
                let BodyOwner::Func(callee_func) = callee.key.owner(&db) else {
                    return true;
                };
                callee_func
                    .name(&db)
                    .to_opt()
                    .is_none_or(|name| name.data(&db) != "prove")
            }));
        }
        if func
            .name(&db)
            .to_opt()
            .is_some_and(|name| name.data(&db) == "forward")
        {
            seen_forward = true;
        }
    }
    assert!(
        seen_witness && seen_forward,
        "ordinary generic forwarding must be lowered, not dropped"
    );
}

#[test]
fn const_intrinsic_classification_does_not_bypass_argument_eligibility() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "wrong_arguments.fe".into(),
        include_str!("fco_payload_controls/wrong_arguments.fe"),
    );
    let (module, _) = db.top_mod(file);
    let mut manager = hir::test_db::initialize_test_analysis_pass();
    let diags = manager
        .run_on_module(&db, module)
        .into_iter()
        .map(|diag| diag.to_complete(&db))
        .collect::<Vec<_>>();
    eprintln!("ARGUMENT_ELIGIBILITY_CAUSE_SPANS {diags:#?}");
    assert!(
        diags.iter().any(|diag| diag.error_code.local_code == 23),
        "existing CallArgNumMismatch diagnostic must remain: {diags:#?}"
    );
    let result = fe_mir::build_runtime_package(&db, module);
    if let Ok(package) = &result {
        eprintln!(
            "INVALID_ARITY_ACCEPTED_PACKAGE {} functions",
            package.functions(&db).len()
        );
        for function in package.functions(&db) {
            eprintln!(
                "{}",
                fe_mir::format_runtime_body(&db, &function.instance(&db).body(&db))
            );
        }
    }
    assert!(
        result.is_err(),
        "arity-invalid checked proof calls cannot become accepted constants"
    );
}

const DISTINCT_CHOICE_SOURCE: &str = r#"
use core::obligations::{Evidence, prove}
trait Pick { const fn value() -> u256 }
struct Subject {}
impl Pick for Subject { const fn value() -> u256 { 1 } }
impl Pick for Subject as First { const fn value() -> u256 { 2 } }
impl Pick for Subject as Second { const fn value() -> u256 { 3 } }
type Goal = Pick<Subject>
const fn first() -> Evidence<Goal> { with (First) { prove<Goal>() } }
const fn second() -> Evidence<Goal> { with (Second) { prove<Goal>() } }
const fn pair_first() -> (Evidence<Goal>, bool) { (first(), false) }
const fn pair_second() -> (Evidence<Goal>, bool) { (second(), false) }
fn consume(_ witness: Evidence<Goal>) -> u256 { with (witness) { <Subject as Pick>::value() } }
"#;

#[test]
fn runtime_parameter_choice_identity_retains_the_selected_declaration() {
    use hir::analysis::{
        semantic::{EffectProviderSubst, ImplEnv, SemanticInstanceKey},
        ty::trait_resolution::PredicateListId,
    };
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "distinct_parameter_identity.fe".into(),
        DISTINCT_CHOICE_SOURCE,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "consume"));
    let mut identities = Vec::new();
    for name in ["first", "second"] {
        let value = issued(&db, module, name);
        let SemConstValue::Evidence { application, .. } = value.value(&db) else {
            panic!("issued tag required")
        };
        let key = SemanticInstanceKey::new(
            &db,
            owner,
            GenericSubst::for_body_owner(&db, owner, vec![]),
            EffectProviderSubst::empty(&db),
            ImplEnv::new(
                &db,
                owner.scope(),
                PredicateListId::empty_list(&db),
                Vec::new(),
                vec![(0, *application)],
                Vec::new(),
            ),
        );
        let instance = hir::analysis::semantic::get_or_build_semantic_instance(&db, key);
        identities.push((
            fe_mir::runtime::stable_key::semantic_instance_identity(&db, instance),
            fe_mir::runtime::stable_key::semantic_instance_symbol_identity(&db, instance),
        ));
    }
    assert_ne!(
        identities[0].0, identities[1].0,
        "different issued declarations must not alias in runtime ordering"
    );
    assert_ne!(
        identities[0].1, identities[1].1,
        "different issued declarations must not alias in runtime symbols"
    );
}

#[test]
fn named_method_body_identity_retains_the_named_declaration() {
    use hir::{
        analysis::{
            semantic::{EffectProviderSubst, ImplEnv, SemanticInstanceKey},
            ty::trait_resolution::Selection,
        },
        hir_def::IdentId,
    };
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("distinct_method_identity.fe".into(), DISTINCT_CHOICE_SOURCE);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let mut identities = Vec::new();
    for name in ["first", "second"] {
        let value = issued(&db, module, name);
        let SemConstValue::Evidence { application, .. } = value.value(&db) else {
            panic!("issued tag required")
        };
        let checked = application
            .revalidate(&db, application.origin_context(), application.goal())
            .unwrap();
        let Selection::Unique(method) = checked.method(IdentId::new(&db, "value")) else {
            panic!("selected method required")
        };
        let owner = BodyOwner::Func(method.body().unwrap());
        let key = SemanticInstanceKey::new(
            &db,
            owner,
            GenericSubst::for_body_owner(&db, owner, method.body_args().to_vec()),
            EffectProviderSubst::empty(&db),
            ImplEnv::empty(&db, owner.scope()),
        );
        identities.push(
            fe_mir::runtime::stable_key::semantic_instance_symbol_identity(
                &db,
                hir::analysis::semantic::get_or_build_semantic_instance(&db, key),
            ),
        );
    }
    assert_ne!(
        identities[0], identities[1],
        "named bodies with the same trait, Self and method name must remain distinct"
    );
}

#[test]
fn described_evidence_constants_retain_the_selected_declaration() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "distinct_description_identity.fe".into(),
        DISTINCT_CHOICE_SOURCE,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for names in [["first", "second"], ["pair_first", "pair_second"]] {
        let mut identities = Vec::new();
        for name in names {
            let value = issued(&db, module, name);
            let ty = TyId::new(
                &db,
                TyData::ConstTy(ConstTyId::new(&db, ConstTyData::Description(value))),
            );
            identities.push(fe_mir::runtime::stable_key::type_identity(&db, ty));
        }
        assert_ne!(
            identities[0], identities[1],
            "described issued constants with different declarations must not alias"
        );
    }
}

#[test]
fn evidence_factory_fault_keeps_original_cause_and_source() {
    assert_factory_failure_source(include_str!("fco_payload_controls/fault-factory.fe"));
}

#[test]
fn evidence_factory_fault_in_a_record_keeps_original_cause_and_source() {
    let source = include_str!("fco_payload_controls/fault-factory.fe")
        .replace("#[test]", "fn forward<T>(_ wrapped: Wrap<Evidence<Pick<T>>>) -> u256 { consume<T>(wrapped.inner) }\n#[test]")
        .replace("consume<Subject>(broken())", "forward<Subject>(Wrap { inner: broken() })");
    assert_factory_failure_source(&source);
}

fn assert_factory_failure_source(source: &str) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("fault-factory.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    assert!(
        fe_mir::build_test_runtime_package(&db, module, Some("never_fallback")).is_err(),
        "fault must never select the ordinary fallback"
    );
    let owner = BodyOwner::Func(find_func(&db, module, "never_fallback"));
    let key = hir::analysis::semantic::root_semantic_instance_key(&db, owner).unwrap();
    let instance = hir::analysis::semantic::get_or_build_semantic_instance(&db, key);
    let failure =
        hir::analysis::semantic::borrowck::check_semantic_borrows(&db, instance).unwrap_err();
    let diagnostic = failure
        .diagnostic()
        .expect("concrete fault requires a diagnostic");
    println!("FACTORY CAUSE {diagnostic:#?}");
    let rendered = format!("{diagnostic:?}");
    assert!(
        rendered.contains("division by zero in const context"),
        "{rendered}"
    );
    assert!(!rendered.contains("MissingBody"), "{rendered}");
    let start = source.find("1 / zero").unwrap();
    assert!(
        diagnostic.sub_diagnostics.iter().any(|label| label
            .span
            .as_ref()
            .is_some_and(|span| u32::from(span.range.start()) as usize == start
                && u32::from(span.range.end()) as usize == start + "1 / zero".len())),
        "original factory fault span required: {rendered}"
    );
}

#[test]
fn projection_depth_boundary_keeps_positive_and_explicit_limit_cause() {
    for (source, supported) in [
        (include_str!("fco_payload_controls/depth-64.fe"), true),
        (include_str!("fco_payload_controls/depth-65.fe"), false),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("projection-depth.fe".into(), source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let package = fe_mir::build_test_runtime_package(&db, module, Some("preserves"));
        if supported {
            let package = package.expect("unchanged depth64 positive must lower");
            assert!(!package.functions(&db).is_empty());
        } else {
            assert!(package.is_err());
            let owner = BodyOwner::Func(find_func(&db, module, "preserves"));
            let key = hir::analysis::semantic::root_semantic_instance_key(&db, owner).unwrap();
            let instance = hir::analysis::semantic::get_or_build_semantic_instance(&db, key);
            let failure = hir::analysis::semantic::borrowck::check_semantic_borrows(&db, instance)
                .unwrap_err();
            let diagnostic = failure
                .diagnostic()
                .expect("explicit discovery limit required");
            println!("DEPTH LIMIT {diagnostic:#?}");
            let rendered = format!("{diagnostic:?}");
            assert!(
                rendered.contains("evidence projection discovery exceeds the depth limit of 64"),
                "{rendered}"
            );
            assert!(!rendered.contains("MissingBody"), "{rendered}");
            assert!(diagnostic.primary_span().is_some());
        }
    }
}

#[test]
fn ordinary_deep_record_is_not_rejected_by_evidence_discovery_limit() {
    let source = include_str!("fco_payload_controls/depth-65.fe")
        .lines()
        .map(|line| {
            if line.starts_with("fn forward") {
                line.replace("Evidence<Pick<T>>", "u256")
                    .replace("consume<T>(", "(")
            } else if line.starts_with("fn preserves") {
                line.replace("inner: first()", "inner: 2")
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n");
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("ordinary-depth65.fe".into(), &source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let package = fe_mir::build_test_runtime_package(&db, module, Some("preserves"))
        .expect("ordinary deep records do not carry issued evidence");
    assert!(!package.functions(&db).is_empty());
}

fn check_source_runtime_choice_delivery(name: &str, callers: &str) {
    let mut db = HirAnalysisTestDb::default();
    let source = [
        r#"use core::obligations::{Evidence, prove}
trait Pick { const fn value() -> u256 }
struct Subject {}
impl Pick for Subject { const fn value() -> u256 { 1 } }
impl Pick for Subject as First { const fn value() -> u256 { 2 } }
impl Pick for Subject as Second { const fn value() -> u256 { 3 } }
type Goal = Pick<Subject>
const fn first() -> Evidence<Goal> { with (First) { prove<Goal>() } }
const fn second() -> Evidence<Goal> { with (Second) { prove<Goal>() } }
fn consume<T>(_ witness: Evidence<Pick<T>>) -> u256 { with (witness) { <T as Pick>::value() } }
"#,
        callers,
    ]
    .concat();
    let file = db.new_stand_alone(name.into(), &source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let first = issued(&db, module, "first");
    let second = issued(&db, module, "second");
    let expected: Vec<_> = [first, second]
        .iter()
        .map(|value| match value.value(&db) {
            SemConstValue::Evidence { application, .. } => application.resolved().selected(),
            _ => panic!("issued tag required"),
        })
        .collect();
    let package = build_runtime_package(&db, module).unwrap_or_else(|error| {
        let owner = BodyOwner::Func(find_func(&db, module, "second_result"));
        let key = hir::analysis::semantic::root_semantic_instance_key(&db, owner).unwrap();
        let instance = hir::analysis::semantic::get_or_build_semantic_instance(&db, key);
        eprintln!(
            "ROOT BORROW DETAILS {:#?}",
            hir::analysis::semantic::borrowck::check_semantic_borrows(&db, instance)
        );
        panic!("compiler-known arguments must specialize the generic consumer: {error:?}");
    });
    let program: &dyn fe_mir::MirDb = &db;
    let mut choices = Vec::new();
    for function in package.functions(&db) {
        let instance = function.instance(&db);
        verify_runtime_body(&db, &program, &instance.body(&db)).unwrap();
        let Some(semantic) = instance.key(&db).semantic(&db) else {
            continue;
        };
        let BodyOwner::Func(func) = semantic.key(&db).owner(&db) else {
            continue;
        };
        if func
            .name(&db)
            .to_opt()
            .is_some_and(|name| name.data(&db) == "forward")
        {
            let records = semantic
                .key(&db)
                .impl_env(&db)
                .projected_parameter_evidence(&db);
            assert!(
                !records.is_empty(),
                "aggregate formal must own its issued slots"
            );
            if records.len() == 2 {
                assert_eq!(records[0].0, records[1].0);
                assert_ne!(
                    records[0].1, records[1].1,
                    "declared projection slots remain distinct"
                );
                assert_eq!(
                    records[0].2.goal(),
                    records[1].2.goal(),
                    "goals coincide after substitution"
                );
                assert_ne!(
                    records[0].2.resolved().selected(),
                    records[1].2.resolved().selected(),
                    "slot choices cannot collapse with their goals"
                );
            }
        }
        if func
            .name(&db)
            .to_opt()
            .is_some_and(|name| name.data(&db) == "consume")
        {
            let records = semantic.key(&db).impl_env(&db).parameter_evidence(&db);
            assert_eq!(records.len(), 1);
            choices.push(records[0].1.resolved().selected());
        }
    }
    assert_eq!(
        choices.len(),
        2,
        "both source calls must retain a distinct consumer specialization"
    );
    for expected in expected {
        assert!(choices.contains(&expected));
    }
}

#[test]
fn source_runtime_calls_deliver_issued_choices_to_generic_callees() {
    check_source_runtime_choice_delivery(
        "direct_choice_delivery.fe",
        r#"fn first_result() -> u256 { consume<Subject>(first()) }
fn second_result() -> u256 { consume<Subject>(second()) }"#,
    );
}

#[test]
fn tuple_projections_deliver_issued_choices_to_generic_callees() {
    check_source_runtime_choice_delivery(
        "tuple_choice_delivery.fe",
        r#"fn first_result() -> u256 { let pair: (Evidence<Goal>, u256) = (first(), 7)
consume<Subject>(pair.0) }
fn second_result() -> u256 { let pair: (Evidence<Goal>, u256) = (second(), 7)
consume<Subject>(pair.0) }"#,
    );
}

#[test]
fn destructuring_delivers_issued_choices_to_generic_callees() {
    check_source_runtime_choice_delivery(
        "destructure_choice_delivery.fe",
        r#"fn first_result() -> u256 { let (witness, _tag): (Evidence<Goal>, u256) = (first(), 7)
consume<Subject>(witness) }
fn second_result() -> u256 { let (witness, _tag): (Evidence<Goal>, u256) = (second(), 7)
consume<Subject>(witness) }"#,
    );
}

#[test]
fn aggregate_factories_deliver_issued_choices_to_generic_callees() {
    check_source_runtime_choice_delivery(
        "factory_choice_delivery.fe",
        r#"const fn first_pair() -> (Evidence<Goal>, u256) { (first(), 7) }
const fn second_pair() -> (Evidence<Goal>, u256) { (second(), 7) }
fn first_result() -> u256 { let pair = first_pair()
consume<Subject>(pair.0) }
fn second_result() -> u256 { let pair = second_pair()
consume<Subject>(pair.0) }"#,
    );
}

#[test]
fn aggregate_parameters_deliver_issued_choices_to_generic_callees() {
    check_source_runtime_choice_delivery(
        "aggregate_parameter_choice_delivery.fe",
        r#"
fn forward<T>(_ pair: (Evidence<Pick<T>>, u256)) -> u256 { consume<T>(pair.0) }
fn first_result() -> u256 { forward<Subject>((first(), 7)) }
fn second_result() -> u256 { forward<Subject>((second(), 7)) }
"#,
    );
}

#[test]
fn aggregate_slots_keep_distinct_choices_when_goals_coincide() {
    check_source_runtime_choice_delivery(
        "coincident_aggregate_slots.fe",
        r#"
fn forward<T,U>(_ pair: (Evidence<Pick<T>>, Evidence<Pick<U>>)) -> u256 { consume<T>(pair.0) + consume<U>(pair.1) }
fn first_result() -> u256 { forward<Subject,Subject>((first(), second())) }
fn second_result() -> u256 { forward<Subject,Subject>((second(), first())) }
"#,
    );
}

#[test]
fn argument_taking_factories_forward_the_original_issued_choice() {
    check_source_runtime_choice_delivery(
        "argument_factory_choice_delivery.fe",
        r#"
use core::obligations::Constraint
const fn identity<G: Constraint>(_ witness: Evidence<G>) -> Evidence<G> { witness }
fn first_result() -> u256 { consume<Subject>(identity<Goal>(first())) }
fn second_result() -> u256 { consume<Subject>(identity<Goal>(second())) }
"#,
    );
}

#[test]
fn array_parameter_slots_deliver_distinct_issued_choices() {
    check_source_runtime_choice_delivery(
        "array_parameter_choice_delivery.fe",
        r#"
fn forward<T>(_ pair: [Evidence<Pick<T>>; 2]) -> u256 { consume<T>(pair[0]) + consume<T>(pair[1]) }
fn first_result() -> u256 { forward<Subject>([first(), second()]) }
fn second_result() -> u256 { forward<Subject>([second(), first()]) }
"#,
    );
}

#[test]
fn repeated_finite_record_wrappers_preserve_issued_choices() {
    check_source_runtime_choice_delivery(
        "repeated_record_wrappers.fe",
        r#"
struct Wrap<T> { inner: T }
fn forward<T>(_ wrapped: Wrap<Wrap<Evidence<Pick<T>>>>) -> u256 {
    consume<T>(wrapped.inner.inner)
}
fn first_result() -> u256 { forward<Subject>(Wrap { inner: Wrap { inner: first() } }) }
fn second_result() -> u256 { forward<Subject>(Wrap { inner: Wrap { inner: second() } }) }
"#,
    );
}

#[test]
fn enum_parameter_payloads_deliver_issued_choices() {
    check_source_runtime_choice_delivery(
        "enum_parameter_choice_delivery.fe",
        r#"
enum Wrapped<T> { Some(Evidence<Pick<T>>), None }
fn forward<T>(_ wrapped: Wrapped<T>) -> u256 {
    match wrapped {
        Wrapped::Some(witness) => consume<T>(witness),
        Wrapped::None => 0,
    }
}

fn first_result() -> u256 { forward<Subject>(Wrapped::Some(first())) }
fn second_result() -> u256 { forward<Subject>(Wrapped::Some(second())) }
"#,
    );
}

#[test]
fn distinct_enum_variant_slots_deliver_issued_choices() {
    check_source_runtime_choice_delivery(
        "distinct_enum_variant_choice_delivery.fe",
        r#"
enum Wrapped<T> { Left(Evidence<Pick<T>>), Right(Evidence<Pick<T>>) }
fn forward<T>(_ wrapped: Wrapped<T>) -> u256 {
    match wrapped {
        Wrapped::Left(witness) => consume<T>(witness),
        Wrapped::Right(witness) => consume<T>(witness),
    }
}
fn first_result() -> u256 { forward<Subject>(Wrapped::Left(first())) }
fn second_result() -> u256 { forward<Subject>(Wrapped::Right(second())) }
"#,
    );
}

#[test]
fn immutable_array_values_deliver_distinct_issued_choices() {
    check_source_runtime_choice_delivery(
        "array_local_choice_delivery.fe",
        r#"
fn forward<T>(_ pair: [Evidence<Pick<T>>; 2]) -> u256 { consume<T>(pair[0]) + consume<T>(pair[1]) }
fn first_result() -> u256 { let pair = [first(), second()]
forward<Subject>(pair) }
fn second_result() -> u256 { let pair = [second(), first()]
forward<Subject>(pair) }
"#,
    );
}

#[test]
fn runtime_array_index_cannot_select_a_witness() {
    let mut db = HirAnalysisTestDb::default();
    let source = r#"
use core::obligations::Evidence
trait Pick { const fn value() -> u256 }
fn consume<T>(_ witness: Evidence<Pick<T>>) -> u256 { with (witness) { <T as Pick>::value() } }
fn select<T>(_ pair: [Evidence<Pick<T>>; 2], _ index: usize) -> u256 { consume<T>(pair[index]) }
"#;
    let file = db.new_stand_alone("runtime_array_index.fe".into(), source);
    let (module, _) = db.top_mod(file);
    let diags: Vec<_> = hir::test_db::initialize_test_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|d| d.to_complete(&db))
        .collect();
    println!("RUNTIME ARRAY INDEX {diags:#?}");
    let start = source.rfind("index]").unwrap();
    assert!(diags.iter().any(
        |d| d.message == "evidence choice must be known at compile time"
            && d.sub_diagnostics.iter().any(|label| {
                label.span.as_ref().is_some_and(|span| {
                    u32::from(span.range.start()) as usize == start
                        && u32::from(span.range.end()) as usize == start + 5
                })
            })
    ));
}

#[test]
fn record_parameter_fields_deliver_distinct_issued_choices() {
    check_source_runtime_choice_delivery(
        "record_parameter_choice_delivery.fe",
        r#"
struct Pair<T> { left: Evidence<Pick<T>>, right: Evidence<Pick<T>> }
fn forward<T>(_ pair: Pair<T>) -> u256 { consume<T>(pair.left) + consume<T>(pair.right) }
fn first_result() -> u256 { forward<Subject>(Pair<Subject> { right: second(), left: first() }) }
fn second_result() -> u256 { let pair = Pair<Subject> { left: second(), right: first() }
forward<Subject>(pair) }
"#,
    );
}

#[test]
fn immutable_array_parameter_aliases_reuse_projected_records() {
    check_source_runtime_choice_delivery(
        "array_parameter_alias_delivery.fe",
        r#"
fn forward<T>(_ pair: [Evidence<Pick<T>>; 2]) -> u256 { consume<T>(pair[0]) + consume<T>(pair[1]) }
fn relay<T>(_ pair: [Evidence<Pick<T>>; 2]) -> u256 { let alias = pair
forward<T>(alias) }
fn first_result() -> u256 { relay<Subject>([first(), second()]) }
fn second_result() -> u256 { relay<Subject>([second(), first()]) }
"#,
    );
}

#[test]
fn array_fields_of_parameters_rebase_projected_records() {
    check_source_runtime_choice_delivery(
        "array_parameter_field_delivery.fe",
        r#"
fn forward<T>(_ pair: [Evidence<Pick<T>>; 2]) -> u256 { consume<T>(pair[0]) + consume<T>(pair[1]) }
fn relay<T>(_ pair: ([Evidence<Pick<T>>; 2], bool)) -> u256 { forward<T>(pair.0) }
fn first_result() -> u256 { relay<Subject>(([first(), second()], false)) }
fn second_result() -> u256 { relay<Subject>(([second(), first()], false)) }
"#,
    );
}
