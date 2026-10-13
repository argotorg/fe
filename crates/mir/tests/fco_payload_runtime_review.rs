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
        if let Some(RuntimeClass::AggregateValue { layout }) = &body.signature.ret {
            if let Layout::Struct(fields) = layout.data(&db) {
                seal_fields = fields.fields.len();
            }
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
        if let Some(RuntimeClass::AggregateValue { layout }) = &body.signature.ret {
            if let Layout::Struct(fields) = layout.data(&db) {
                seal_fields = fields.fields.len();
            }
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
