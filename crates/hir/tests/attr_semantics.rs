use fe_hir::test_db::HirAnalysisTestDb;
use fe_hir::{
    analysis::{
        semantic::{
            NExpr, NSStmtKind, SExpr, SStmtKind, SemanticCodeRegionRef, SemanticCodeRegionTarget,
            get_or_build_semantic_instance, identity_semantic_instance_key,
            normalize_semantic_body,
        },
        ty::ty_check::{BodyOwner, check_func_body},
    },
    hir_def::{InlineAttrErrorKind, ManualContractRootAttr, Partial, Stmt},
};

#[test]
fn expr_valued_inline_attr_stays_invalid() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "inline_attr_expr_value.fe".into(),
        r#"#[inline = 1 + 2]
fn bad() {}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let funcs = top_mod.all_funcs(&db);
    let [func] = funcs.as_slice() else {
        panic!("expected exactly one function");
    };

    assert_eq!(func.inline_hint(&db), None);
    assert_eq!(
        func.inline_attr_error(&db),
        Some(InlineAttrErrorKind::InvalidForm)
    );
}

#[test]
fn empty_inline_attr_parens_stay_invalid() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "inline_attr_empty_parens.fe".into(),
        r#"#[inline()]
fn bad() {}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let funcs = top_mod.all_funcs(&db);
    let [func] = funcs.as_slice() else {
        panic!("expected exactly one function");
    };

    assert_eq!(func.inline_hint(&db), None);
    assert_eq!(
        func.inline_attr_error(&db),
        Some(InlineAttrErrorKind::InvalidForm)
    );
}

#[test]
fn expr_valued_loop_unroll_attr_does_not_apply_hint() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "loop_unroll_expr_value.fe".into(),
        r#"fn bad(xs: [u256; 1]) {
    #[unroll = 1 + 2]
    for x in xs {}
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let funcs = top_mod.all_funcs(&db);
    let [func] = funcs.as_slice() else {
        panic!("expected exactly one function");
    };
    let body = func.body(&db).expect("expected function body");

    let unroll_hint = body.stmts(&db).iter().find_map(|(_, stmt)| match stmt {
        Partial::Present(Stmt::For(_, _, _, hint)) => Some(*hint),
        Partial::Present(_) | Partial::Absent => None,
    });

    assert_eq!(unroll_hint, Some(None));
}

#[test]
fn empty_loop_unroll_attr_parens_do_not_apply_hint() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "loop_unroll_empty_parens.fe".into(),
        r#"fn bad(xs: [u256; 1]) {
    #[unroll()]
    for x in xs {}
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let funcs = top_mod.all_funcs(&db);
    let [func] = funcs.as_slice() else {
        panic!("expected exactly one function");
    };
    let body = func.body(&db).expect("expected function body");

    let unroll_hint = body.stmts(&db).iter().find_map(|(_, stmt)| match stmt {
        Partial::Present(Stmt::For(_, _, _, hint)) => Some(*hint),
        Partial::Present(_) | Partial::Absent => None,
    });

    assert_eq!(unroll_hint, Some(None));
}

#[test]
fn unroll_never_attr_prevents_unrolling() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "loop_unroll_never.fe".into(),
        r#"fn bad(xs: [u256; 1]) {
    #[unroll(never)]
    for x in xs {}
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let funcs = top_mod.all_funcs(&db);
    let [func] = funcs.as_slice() else {
        panic!("expected exactly one function");
    };
    let body = func.body(&db).expect("expected function body");

    let unroll_hint = body.stmts(&db).iter().find_map(|(_, stmt)| match stmt {
        Partial::Present(Stmt::For(_, _, _, hint)) => Some(*hint),
        Partial::Present(_) | Partial::Absent => None,
    });

    assert_eq!(unroll_hint, Some(Some(false)));
}

#[test]
fn empty_payable_attr_parens_do_not_mark_init_payable() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "payable_init_empty_parens.fe".into(),
        r#"contract C {
    #[payable()]
    init() {}
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contracts = top_mod.all_contracts(&db);
    let [contract] = contracts.as_slice() else {
        panic!("expected exactly one contract");
    };
    let init = contract.init(&db).expect("expected contract init");

    assert!(!init.is_payable(&db));
}

#[test]
fn invalid_payable_init_attr_does_not_mark_init_payable() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "payable_init_invalid.fe".into(),
        r#"contract C {
    #[payable = 1]
    init() {}
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contracts = top_mod.all_contracts(&db);
    let [contract] = contracts.as_slice() else {
        panic!("expected exactly one contract");
    };
    let init = contract.init(&db).expect("expected contract init");

    assert!(!init.is_payable(&db));
}

#[test]
fn malformed_payable_does_not_mask_valid_init_payable_attr() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "payable_init_mixed_validity.fe".into(),
        r#"contract C {
    #[payable(foo)]
    #[payable]
    init() {}
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contracts = top_mod.all_contracts(&db);
    let [contract] = contracts.as_slice() else {
        panic!("expected exactly one contract");
    };
    let init = contract.init(&db).expect("expected contract init");

    assert!(init.is_payable(&db));
}

#[test]
fn invalid_payable_recv_arm_attr_does_not_mark_arm_payable() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "payable_recv_arm_invalid.fe".into(),
        r#"use std::abi::sol

msg M {
    #[selector = sol("ping()")]
    Ping,
}

contract C {
    recv M {
        #[payable(foo)]
        Ping {} {}
    }
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contracts = top_mod.all_contracts(&db);
    let [contract] = contracts.as_slice() else {
        panic!("expected exactly one contract");
    };
    let arm = contract
        .recv_arm(&db, 0, 0)
        .expect("expected one recv arm at 0:0");

    assert!(!arm.is_payable(&db));
}

#[test]
fn manual_contract_root_attr_parses_valid_forms() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "manual_contract_root_attr.fe".into(),
        r#"use std::evm::Evm

#[contract_init(Coin)]
fn init() uses (evm: mut Evm) {}

#[contract_runtime(Coin)]
fn runtime() uses (evm: mut Evm) {}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let funcs = top_mod.all_funcs(&db);
    let init = funcs
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "init")
        })
        .expect("missing init");
    let runtime = funcs
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "runtime")
        })
        .expect("missing runtime");

    assert_eq!(
        init.manual_contract_root_attr(&db),
        Some(ManualContractRootAttr::Init {
            contract_name: fe_hir::hir_def::StringId::new(&db, "Coin".to_string()),
        })
    );
    assert_eq!(
        runtime.manual_contract_root_attr(&db),
        Some(ManualContractRootAttr::Runtime {
            contract_name: fe_hir::hir_def::StringId::new(&db, "Coin".to_string()),
        })
    );
}

#[test]
fn code_region_intrinsics_lower_to_semantic_code_region_exprs() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "manual_contract_code_region.fe".into(),
        r#"use std::evm::Evm

#[contract_init(Coin)]
fn init() uses (evm: mut Evm) {
    let len = evm.code_region_len(runtime)
    let off = evm.code_region_offset(runtime)
}

#[contract_runtime(Coin)]
fn runtime() uses (evm: mut Evm) {}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let funcs = top_mod.all_funcs(&db);
    let init = funcs
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "init")
        })
        .expect("missing init");
    let runtime = funcs
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "runtime")
        })
        .expect("missing runtime");

    let (diags, typed_body) = check_func_body(&db, init);
    assert!(diags.is_empty(), "{diags:#?}");
    let body = init.body(&db).expect("missing init body");
    let refs = body
        .exprs(&db)
        .keys()
        .filter_map(|expr| typed_body.code_region_ref(&db, expr))
        .collect::<Vec<_>>();
    assert_eq!(refs.len(), 2);
    assert!(refs.iter().all(|region| matches!(
        region,
        SemanticCodeRegionRef::ManualContractRoot { func } if *func == runtime
    )));

    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(init)),
    );
    let body = instance.body(&db);
    let exprs = body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .filter_map(|stmt| match &stmt.kind {
            SStmtKind::Assign { expr, .. } => Some(expr),
            SStmtKind::Store { .. } => None,
        })
        .collect::<Vec<_>>();
    assert!(exprs.iter().any(|expr| matches!(
        expr,
        SExpr::CodeRegionLen {
            target: SemanticCodeRegionTarget::Resolved(
                SemanticCodeRegionRef::ManualContractRoot { func },
            ),
        }
            if *func == runtime
    )));
    assert!(exprs.iter().any(|expr| matches!(
        expr,
        SExpr::CodeRegionOffset {
            target: SemanticCodeRegionTarget::Resolved(
                SemanticCodeRegionRef::ManualContractRoot { func },
            ),
        }
            if *func == runtime
    )));
}

#[test]
fn direct_code_region_intrinsics_record_semantic_refs() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "manual_contract_code_region_direct.fe".into(),
        r#"use std::evm::Evm
use std::evm::intrinsic::{code_region_len, code_region_offset}

#[contract_init(Coin)]
fn init() uses (evm: mut Evm) {
    let len = code_region_len(runtime)
    let off = code_region_offset(runtime)
}

#[contract_runtime(Coin)]
fn runtime() uses (evm: mut Evm) {}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let funcs = top_mod.all_funcs(&db);
    let init = funcs
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "init")
        })
        .expect("missing init");
    let runtime = funcs
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "runtime")
        })
        .expect("missing runtime");

    let (diags, typed_body) = check_func_body(&db, init);
    assert!(diags.is_empty(), "{diags:#?}");
    let body = init.body(&db).expect("missing init body");
    let refs = body
        .exprs(&db)
        .keys()
        .filter_map(|expr| typed_body.code_region_ref(&db, expr))
        .collect::<Vec<_>>();
    assert_eq!(refs.len(), 2);
    assert!(refs.iter().all(|region| matches!(
        region,
        SemanticCodeRegionRef::ManualContractRoot { func } if *func == runtime
    )));
}

#[test]
fn create_contract_generic_forwarding_preserves_manual_root_refs() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "manual_contract_create_contract_forwarding.fe".into(),
        r#"use std::evm::Evm

#[contract_init(Coin)]
fn init() uses (evm: mut Evm) {
    evm.create_contract(runtime)
}

#[contract_runtime(Coin)]
fn runtime() uses (evm: mut Evm) {}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let funcs = top_mod.all_funcs(&db);
    let init = funcs
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "init")
        })
        .expect("missing init");
    let runtime = funcs
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "runtime")
        })
        .expect("missing runtime");

    let (diags, typed_body) = check_func_body(&db, init);
    assert!(diags.is_empty(), "{diags:#?}");
    let body = init.body(&db).expect("missing init body");
    assert!(body.exprs(&db).keys().any(|expr| matches!(
        typed_body.expr_code_region_ref(&db, expr),
        Some(SemanticCodeRegionRef::ManualContractRoot { func }) if func == runtime
    )));

    let init_instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(init)),
    );
    let mut pending = vec![init_instance];
    let mut seen = Vec::new();
    let mut saw_len = false;
    let mut saw_offset = false;
    while let Some(instance) = pending.pop() {
        let key = instance.key(&db);
        if seen.contains(&key) {
            continue;
        }
        seen.push(key);
        let body = instance.body(&db);
        for expr in body
            .blocks
            .iter()
            .flat_map(|block| block.stmts.iter())
            .filter_map(|stmt| match &stmt.kind {
                SStmtKind::Assign { expr, .. } => Some(expr),
                SStmtKind::Store { .. } => None,
            })
        {
            saw_len |= matches!(
                expr,
                SExpr::CodeRegionLen {
                    target: SemanticCodeRegionTarget::Resolved(
                        SemanticCodeRegionRef::ManualContractRoot { func },
                    ),
                }
                    if *func == runtime
            );
            saw_offset |= matches!(
                expr,
                SExpr::CodeRegionOffset {
                    target: SemanticCodeRegionTarget::Resolved(
                        SemanticCodeRegionRef::ManualContractRoot { func },
                    ),
                }
                    if *func == runtime
            );
        }
        pending.extend(
            instance
                .callees(&db)
                .iter()
                .map(|callee| get_or_build_semantic_instance(&db, callee.key)),
        );
    }
    assert!(saw_len);
    assert!(saw_offset);
}

#[test]
fn array_repeat_lowering_preserves_repeat_expr_through_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "array_repeat_lowering_keeps_repeat_expr_until_normalization.fe".into(),
        r#"fn repeat(x: u256) -> [u256; 4] {
    return [x; 4]
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let [func] = top_mod.all_funcs(&db).as_slice() else {
        panic!("expected exactly one function");
    };
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
    );
    let body = instance.body(&db);
    body.blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            SStmtKind::Assign {
                expr: SExpr::ArrayRepeat { ty, .. },
                ..
            } if ty.is_array(&db) => Some(()),
            SStmtKind::Assign { .. } | SStmtKind::Store { .. } => None,
        })
        .expect("expected array repeat in raw semantic lowering");

    let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
    let normalized_array_len = normalized
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            NSStmtKind::Assign {
                expr: NExpr::ArrayRepeat { ty, .. },
                ..
            } if ty.is_array(&db) => ty.array_len(&db),
            NSStmtKind::Assign { .. } | NSStmtKind::Store { .. } => None,
        })
        .expect("expected concrete array repeat to remain structural after normalization");
    assert_eq!(normalized_array_len, 4);
}

#[test]
fn generic_array_repeat_with_symbolic_len_normalizes_as_repeat() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "generic_array_repeat_with_symbolic_len_normalizes_as_repeat.fe".into(),
        r#"fn repeat<const N: usize>() {
    let _x: [u256; N] = [0; N]
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let [func] = top_mod.all_funcs(&db).as_slice() else {
        panic!("expected exactly one function");
    };
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
    );
    let body = instance.body(&db);
    body.blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            SStmtKind::Assign {
                expr: SExpr::ArrayRepeat { ty, .. },
                ..
            } if ty.is_array(&db) && ty.array_len(&db).is_none() => Some(()),
            SStmtKind::Assign { .. } | SStmtKind::Store { .. } => None,
        })
        .expect("expected symbolic array repeat in raw semantic lowering");

    let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
    normalized
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            NSStmtKind::Assign {
                expr: NExpr::ArrayRepeat { ty, .. },
                ..
            } if ty.is_array(&db) && ty.array_len(&db).is_none() => Some(()),
            NSStmtKind::Assign { .. } | NSStmtKind::Store { .. } => None,
        })
        .expect("expected symbolic array repeat to stay unexpanded after normalization");
}

#[test]
fn mut_self_aug_assign_receivers_lower_as_borrow_calls() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "mut_self_aug_assign_receivers_lower_as_borrow_calls.fe".into(),
        r#"
pub struct Tower {
    counts: [u256; 24],
}

impl Tower {
    pub fn add(mut self) -> u256 {
        let mut scan: usize = 24
        while scan != 0 {
            scan -= 1
            if self.counts[scan] != 0 {
                return self.counts[scan]
            }
        }
        0
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let add = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "add"))
        .expect("expected add method");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(add)),
    );
    let body = instance.body(&db);

    let borrow_dsts = body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .filter_map(|stmt| match &stmt.kind {
            SStmtKind::Assign {
                dst,
                expr:
                    SExpr::Borrow {
                        kind: fe_hir::analysis::ty::ty_def::BorrowKind::Mut,
                        ..
                    },
            } => Some(*dst),
            SStmtKind::Assign { .. } | SStmtKind::Store { .. } => None,
        })
        .collect::<std::collections::HashSet<_>>();

    assert!(
        body.blocks
            .iter()
            .flat_map(|block| block.stmts.iter())
            .any(|stmt| {
                matches!(
                    &stmt.kind,
                    SStmtKind::Assign {
                        expr: SExpr::Call { args, .. },
                        ..
                    } if args
                        .first()
                        .is_some_and(|receiver| borrow_dsts.contains(&receiver.value))
                )
            }),
        "augmented assignment receiver should lower through a synthesized mutable borrow:\n{body:#?}"
    );
}
