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
    let package = build_runtime_package(&db, module)
        .expect("compiler-known arguments must specialize the generic consumer");
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
