use std::collections::VecDeque;

use cranelift_entity::EntityRef;
use fe_hir::test_db::{HirAnalysisTestDb, format_diagnostics};
use fe_hir::{
    analysis::{
        initialize_analysis_pass,
        semantic::{
            CtfeError, LayoutEvidenceError, NDataProjection, NExpr, NIndex, NPlaceBase, NRootKind,
            NStatementKind, NValueDefinition, NormalizedArtifacts, ReadMode, SExpr, SStmtKind,
            STerminatorKind, SemanticAnalysisError, SemanticBodyAdmission, SemanticBorrowDiagKind,
            SemanticInstance, SemanticNormalizationFailure, canonicalize_semantic_consts,
            capability::{
                external::ExternalOrigin,
                guard::ValueOccurrence,
                handle::{HandleAddressSpace, OpaqueHandleOccurrence},
                path::{Projection as CapabilityProjection, RegionPath, StructuralPath},
                source::InputSource,
                value::{ValueInterner, ValueLimits},
            },
            check_semantic_borrows, check_semantic_noesc,
            collect_semantic_borrow_diagnostic_vouchers, contract_init_assigned_fields,
            get_or_build_semantic_instance, identity_semantic_instance_key, layout_evidence_body,
            normalize_semantic_body,
            normalized::{
                HandleOrigin, NLayoutBackingSource, NormalizedBodyVerifyError, normalize_raw_body,
                verify_normalized_body,
            },
            semantic_body_admission, semantic_borrow_summary,
        },
        ty::{
            ProviderAddressSpace,
            ty_check::{BodyOwner, LocalBinding},
            ty_def::{BorrowKind, TyData},
        },
    },
    hir_def::{ItemKind, Partial},
    projection::Projection,
};

fn borrow_diags(src: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    )
}

fn checked_borrow_diags(src: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    )
}

#[test]
fn blocked_invalid_body_is_not_admitted_by_semantic_consumers() {
    let source = r#"
fn invalid(result: mut u256) -> mut u256 uses (values: mut [mut u256; 2]) {
    values[0] = 1
    result
}

fn caller(result: mut u256) -> mut u256 uses (values: mut [mut u256; 2]) {
    invalid(result)
}
"#;
    assert!(borrow_diags(source).is_empty());

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    let instance_for = |name: &str| {
        top_mod
            .all_items(&db)
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|func_name| func_name.data(&db) == name) =>
                {
                    Some(get_or_build_semantic_instance(
                        &db,
                        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                    ))
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("missing `{name}` function"))
    };
    let instance = instance_for("invalid");

    let SemanticBodyAdmission::Blocked(blocked) = semantic_body_admission(&db, instance) else {
        panic!("invalid body should be blocked before normalization")
    };
    assert_eq!(blocked.instance, instance);
    assert!(!blocked.causes.is_empty());
    assert!(matches!(
        normalize_semantic_body(&db, instance),
        Err(SemanticNormalizationFailure::Blocked(_))
    ));
    assert!(matches!(
        check_semantic_borrows(&db, instance),
        Err(SemanticAnalysisError::Blocked(_))
    ));
    assert!(matches!(
        check_semantic_noesc(&db, instance),
        Err(SemanticAnalysisError::Blocked(_))
    ));
    assert!(matches!(
        semantic_borrow_summary(&db, instance),
        Err(SemanticAnalysisError::Blocked(_))
    ));
    assert!(matches!(
        layout_evidence_body(&db, instance),
        Err(LayoutEvidenceError::Blocked(_))
    ));
    assert!(matches!(
        canonicalize_semantic_consts(&db, instance),
        Err(CtfeError::InvalidBody { .. })
    ));

    let caller = instance_for("caller");
    let invalid_owner = instance.key(&db).owner(&db);
    let caller_owner = caller.key(&db).owner(&db);
    let Err(SemanticAnalysisError::Blocked(blocked)) = semantic_borrow_summary(&db, caller) else {
        panic!("blocked callee summary must keep its status through the caller")
    };
    assert_eq!(blocked.instance.key(&db).owner(&db), invalid_owner);
    assert_ne!(blocked.instance.key(&db).owner(&db), caller_owner);
    let Err(SemanticAnalysisError::Blocked(blocked)) = check_semantic_borrows(&db, caller) else {
        panic!("blocked callee analysis must keep its status through the caller")
    };
    assert_eq!(blocked.instance.key(&db).owner(&db), invalid_owner);
    assert_ne!(blocked.instance.key(&db).owner(&db), caller_owner);
}

#[test]
fn blocked_contract_body_keeps_its_declaration_layout_signature() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Rooted<const ROOT: u256 = _> {}

pub contract InvalidInit {
    values: [Rooted; 2]

    init() uses (values) {
        missing
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = contract_init_instance(&db, top_mod, "InvalidInit");
    assert!(matches!(
        semantic_body_admission(&db, instance),
        SemanticBodyAdmission::Blocked(_)
    ));
    let BodyOwner::ContractInit { contract } = instance.key(&db).owner(&db) else {
        unreachable!()
    };
    assert!(matches!(
        contract_init_assigned_fields(&db, contract),
        Err(SemanticNormalizationFailure::Blocked(_))
    ));

    let signature = instance.key(&db).layout_bundle_signature(&db);
    assert_eq!(signature.inputs.len(), 1);
    assert!(!signature.inputs[0].interface.schema.components.is_empty());
}

#[test]
fn generated_zero_field_abi_bodies_are_admitted() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
msg Empty {
    #[selector = 1]
    Ping,
}

#[error]
struct EmptyError {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let abi_funcs = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .filter(|func| {
            func.name(&db).to_opt().is_some_and(|name| {
                matches!(name.data(&db).as_str(), "payload_size" | "encode_to_ptr")
            })
        })
        .collect::<Vec<_>>();
    assert_eq!(abi_funcs.len(), 4);
    for func in abi_funcs {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
        assert!(matches!(
            semantic_body_admission(&db, instance),
            SemanticBodyAdmission::Ready(_)
        ));
    }
}

fn contract_init_instance<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: fe_hir::hir_def::TopLevelMod<'db>,
    contract_name: &str,
) -> SemanticInstance<'db> {
    top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Contract(contract)
                if contract
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == contract_name) =>
            {
                Some(get_or_build_semantic_instance(
                    db,
                    identity_semantic_instance_key(
                        db,
                        BodyOwner::ContractInit {
                            contract: *contract,
                        },
                    ),
                ))
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing contract init `{contract_name}`"))
}

fn mixed_returned_borrow_provenance_src() -> &'static str {
    r#"
struct Ledger {
    b: u256,
}

impl Ledger {
    fn pick_mixed(mut self, cond: bool, value: mut u256) -> mut u256 {
        if cond {
            value
        } else {
            mut self.b
        }
    }
}

fn add(by: u256) -> u256 uses (value: mut u256) {
    value += by
    value
}

pub contract Mixed {
    mut ledger: Ledger

    init() uses (mut ledger) {
        let mut local: u256 = 0
        let target = ledger.pick_mixed(cond: true, value: mut local)
        with (target) {
            add(by: 1)
        }
    }
}
"#
}

fn for_each_fixture_instance(
    src: &str,
    mut f: impl FnMut(&HirAnalysisTestDb, SemanticInstance<'_>),
) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    let mut pending = VecDeque::new();

    for item in top_mod.all_items(&db) {
        match item {
            ItemKind::Func(func) => pending.push_back(get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
            )),
            ItemKind::Contract(contract) => {
                pending.push_back(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(
                        &db,
                        BodyOwner::ContractInit {
                            contract: *contract,
                        },
                    ),
                ));
                for (recv_idx, recv) in contract.recvs(&db).data(&db).iter().enumerate() {
                    for arm_idx in 0..recv.arms.data(&db).len() {
                        pending.push_back(get_or_build_semantic_instance(
                            &db,
                            identity_semantic_instance_key(
                                &db,
                                BodyOwner::ContractRecvArm {
                                    contract: *contract,
                                    recv_idx: recv_idx as u32,
                                    arm_idx: arm_idx as u32,
                                },
                            ),
                        ));
                    }
                }
            }
            ItemKind::Const(_)
            | ItemKind::Mod(_)
            | ItemKind::Struct(_)
            | ItemKind::Enum(_)
            | ItemKind::Trait(_)
            | ItemKind::Impl(_)
            | ItemKind::ImplTrait(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::StaticAssert(_)
            | ItemKind::Use(_)
            | ItemKind::TopMod(_)
            | ItemKind::Body(_) => {}
        }
    }

    let mut seen = rustc_hash::FxHashSet::default();
    while let Some(instance) = pending.pop_front() {
        if !seen.insert(instance.key(&db)) {
            continue;
        }
        f(&db, instance);
        for callee in instance.callees(&db) {
            pending.push_back(get_or_build_semantic_instance(&db, callee.key));
        }
    }
}

fn owner_name(db: &HirAnalysisTestDb, owner: BodyOwner<'_>) -> String {
    match owner {
        BodyOwner::Func(func) => match func.name(db) {
            Partial::Present(name) => name.data(db).to_string(),
            Partial::Absent => "<fn>".to_string(),
        },
        BodyOwner::Const(const_) => match const_.name(db) {
            Partial::Present(name) => name.data(db).to_string(),
            Partial::Absent => "<const>".to_string(),
        },
        BodyOwner::AnonConstBody { .. } => "<anon const>".to_string(),
        BodyOwner::ContractInit { contract } => match contract.name(db) {
            Partial::Present(name) => format!("{}::__init__", name.data(db)),
            Partial::Absent => "<contract>::__init__".to_string(),
        },
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => match contract.name(db) {
            Partial::Present(name) => format!("{}::recv[{recv_idx}][{arm_idx}]", name.data(db)),
            Partial::Absent => format!("<contract>::recv[{recv_idx}][{arm_idx}]"),
        },
    }
}

fn normalized_func_body<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: fe_hir::hir_def::TopLevelMod<'db>,
    func_name: &str,
) -> NormalizedArtifacts<'db> {
    let instance = top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == func_name) =>
            {
                Some(get_or_build_semantic_instance(
                    db,
                    identity_semantic_instance_key(db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{func_name}`"));
    normalize_semantic_body(db, instance).expect("normalized body")
}

#[test]
fn self_referential_param_layout_backing_sources_use_the_param_root() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Pair {
    x: u256,
    y: u256,
}

fn rebuild(mut _ value: own Pair) -> Pair {
    let x = value.x
    value = Pair { x, y: value.y }
    value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let normalized = normalized_func_body(&db, top_mod, "rebuild");
    let source = normalized.body.owner.body(&db);
    let param_local = source
        .locals
        .iter()
        .enumerate()
        .find(|(_, local)| matches!(local.source, Some(LocalBinding::Param { idx: 0, .. })))
        .map(|(index, _)| fe_hir::analysis::semantic::SLocalId::new(index))
        .expect("missing value parameter");
    let param_root = normalized
        .body
        .roots
        .iter()
        .enumerate()
        .find_map(|(index, root)| {
            (matches!(root.kind, NRootKind::ParamPlace { param: 0 })
                && normalized
                    .layout_plan
                    .root_source(fe_hir::analysis::semantic::NRootId::new(index))
                    == Some(param_local))
            .then_some(fe_hir::analysis::semantic::NRootId::new(index))
        })
        .expect("mutable owned aggregate parameter must have a root");

    assert!(
        normalized
            .layout_plan
            .use_backings
            .iter()
            .any(|source| matches!(source.source, fe_hir::analysis::semantic::NLayoutBackingSource::Root { root, .. } if root == param_root))
    );
}

#[test]
fn branch_return_borrow_summary_flows_through_empty_entry_blocks() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Ledger {
    a: u256,
    b: u256,
    c: u256,
}

impl Ledger {
    fn pick(mut self, _ pick_c: bool) -> mut u256 {
        if pick_c {
            mut self.c
        } else {
            mut self.a
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "pick") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("pick instance");
    let summary = semantic_borrow_summary(&db, instance)
        .expect("borrow summary")
        .expect("borrow-returning function should produce a summary");
    let values = ValueInterner::new(&db, ValueLimits::default());
    let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
    assert_eq!(leaves.len(), 2, "unexpected summary: {summary:#?}");
    for field in [0, 2] {
        assert!(leaves.iter().any(|leaf| leaf.payload.source.origin
            == ExternalOrigin::Input(InputSource::slot(0, StructuralPath::default()))
            && leaf.payload.path
                == RegionPath::new([CapabilityProjection::Field(
                    fe_hir::analysis::semantic::FieldIndex(field)
                )])));
    }
    check_semantic_borrows(&db, instance).expect("borrowck should accept branch-returned borrow");
}

#[test]
fn forwarded_memory_borrow_param_keeps_incoming_loan_targets() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Holder {
    tag: u256,
}

impl Holder {
    fn forward(mut self, _ value: mut u256) -> mut u256 {
        value
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "forward") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("forward instance");
    let summary = semantic_borrow_summary(&db, instance)
        .expect("borrow summary")
        .expect("forward should produce a borrow summary");
    let values = ValueInterner::new(&db, ValueLimits::default());
    let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
    assert_eq!(leaves.len(), 1);
    assert_eq!(
        leaves[0].payload.source.origin,
        ExternalOrigin::Input(InputSource::slot(1, StructuralPath::default()))
    );
    check_semantic_borrows(&db, instance).expect("borrowck should accept forwarded borrows");
}

#[test]
fn contract_field_mut_borrow_matrix_fixture_borrowchecks() {
    for_each_fixture_instance(
        include_str!("../../fe/tests/fixtures/fe_test/contract_field_mut_borrow_matrix.fe"),
        |db, instance| {
            let raw = instance.body(db);
            let artifacts = normalize_raw_body(db, instance, raw, instance.assumptions(db))
                .unwrap_or_else(|error| {
                    panic!(
                        "phase-one normalization failed for {} ({:?}): {error:#?}",
                        owner_name(db, instance.key(db).owner(db)),
                        instance.key(db),
                    )
                });
            verify_normalized_body(db, &artifacts.body).unwrap_or_else(|error| {
                let type_detail = match error {
                    fe_hir::analysis::semantic::normalized::NormalizedBodyVerifyError::ForwardType {
                        result,
                        source,
                    } => format!(
                        "result_ty={} source_ty={}",
                        artifacts.body.value(result).expect("result value").ty.pretty_print(db),
                        artifacts.body.value(source).expect("source value").ty.pretty_print(db),
                    ),
                    fe_hir::analysis::semantic::normalized::NormalizedBodyVerifyError::StoreType {
                        value,
                        destination: fe_hir::analysis::semantic::normalized::NPlaceBase::Root(root),
                    } => format!(
                        "source_ty={} destination_ty={}",
                        artifacts.body.value(value).expect("source value").ty.pretty_print(db),
                        artifacts.body.root(root).expect("destination root").ty.pretty_print(db),
                    ),
                    _ => String::new(),
                };
                panic!(
                    "phase-one normalized body failed verification for {} ({:?}): {error:#?} {type_detail}\nraw={raw:#?}\nnormalized={:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                    artifacts.body,
                )
            });
            assert!(matches!(
                fe_hir::analysis::semantic::normalized::semantic_body_admission(db, instance),
                fe_hir::analysis::semantic::normalized::SemanticBodyAdmission::Ready(_),
            ));
            if let Err(diag) = check_semantic_borrows(db, instance) {
                panic!(
                    "borrowck failed for {} ({:?}): {diag:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                );
            }
        },
    );
}

#[test]
fn diverging_if_branch_does_not_forward_never_into_join_result() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
fn diverge() -> ! {
    core::panic()
}

fn choose(flag: bool) {
    if flag {
        diverge()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let choose = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "choose") =>
            {
                Some(*func)
            }
            _ => None,
        })
        .expect("missing `choose` function");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(choose)),
    );
    let raw = instance.body(&db);
    let mut saw_diverging_call = false;
    for block in &raw.blocks {
        for statement in &block.stmts {
            let SStmtKind::Assign { dst, expr } = &statement.kind else {
                continue;
            };
            if matches!(expr, SExpr::Call { .. }) && raw.locals[dst.index()].ty.is_never(&db) {
                saw_diverging_call = true;
                assert!(matches!(
                    block.terminator.kind,
                    STerminatorKind::Assert { message: None }
                ));
            }
            if let SExpr::Forward(source) = expr {
                assert_eq!(
                    raw.locals[dst.index()].ty,
                    raw.locals[source.value.index()].ty
                );
            }
        }
    }
    assert!(saw_diverging_call);

    let artifacts = normalize_raw_body(&db, instance, raw, instance.assumptions(&db))
        .expect("never-branch body should normalize");
    verify_normalized_body(&db, &artifacts.body)
        .expect("never-branch normalized body should verify");
}

#[test]
fn returned_storage_borrow_effect_args_are_finalized_in_normalized_body() {
    let mut saw_storage_add_effect = false;
    for_each_fixture_instance(
        include_str!("../../fe/tests/fixtures/fe_test/contract_field_mut_borrow_matrix.fe"),
        |db, instance| {
            let normalized = normalize_semantic_body(db, instance).expect("normalized body");
            for stmt in normalized
                .body
                .blocks
                .iter()
                .flat_map(|block| block.statements.iter())
            {
                let NStatementKind::Define {
                    expr:
                        NExpr::Call {
                            callee,
                            effect_args,
                            ..
                        },
                    ..
                } = &stmt.kind
                else {
                    continue;
                };
                let BodyOwner::Func(func) = callee.key.owner(db) else {
                    continue;
                };
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == "add")
                    && effect_args
                        .iter()
                        .any(|arg| arg.provider == Some(ProviderAddressSpace::Storage))
                {
                    saw_storage_add_effect = true;
                }
            }
        },
    );
    assert!(
        saw_storage_add_effect,
        "expected storage provider on normalized add effect arg"
    );
}

#[test]
fn mixed_returned_borrow_provenance_is_rejected_before_runtime_lowering() {
    let diags = borrow_diags(mixed_returned_borrow_provenance_src());

    assert!(
        diags.contains("provider provenance conflict in `fn Mixed::__init__`"),
        "{diags:?}"
    );
    assert!(
        diags.contains("effect argument may come from multiple address spaces"),
        "{diags:?}"
    );
}

#[test]
fn mixed_returned_borrow_provenance_poison_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        mixed_returned_borrow_provenance_src(),
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = contract_init_instance(&db, top_mod, "Mixed");

    let err = normalize_semantic_body(&db, instance)
        .expect_err("mixed provider provenance must poison normalization");
    let SemanticNormalizationFailure::InternalFailure(err) = err else {
        panic!("provider provenance conflict must be an internal normalization failure")
    };
    assert_eq!(err.kind, SemanticBorrowDiagKind::ProviderProvenanceConflict);
    assert_eq!(
        err.primary.message,
        "effect argument may come from multiple address spaces: memory, storage"
    );
}

#[test]
fn mixed_returned_borrow_provenance_poison_noesc() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        mixed_returned_borrow_provenance_src(),
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = contract_init_instance(&db, top_mod, "Mixed");

    let err = check_semantic_noesc(&db, instance)
        .expect_err("mixed provider provenance must poison noesc");
    let SemanticAnalysisError::Diagnostic(err) = err else {
        panic!("provider provenance conflict must produce a diagnostic")
    };
    assert_eq!(
        err.message,
        "provider provenance conflict in `fn Mixed::__init__`"
    );
    assert_eq!(
        err.sub_diagnostics[0].message,
        "effect argument may come from multiple address spaces: memory, storage"
    );
}

#[test]
fn mixed_returned_borrow_provenance_collects_one_diagnostic() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        mixed_returned_borrow_provenance_src(),
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    assert_eq!(
        diags.len(),
        1,
        "unexpected diagnostics: {:#?}",
        borrow_diags(mixed_returned_borrow_provenance_src())
    );
    let rendered = format_diagnostics(&db, &diags);
    assert!(
        rendered.contains("provider provenance conflict in `fn Mixed::__init__`"),
        "{rendered:?}"
    );
}

#[test]
fn reports_mut_borrow_conflict() {
    let diags = borrow_diags(
        r#"
fn bad() {
    let mut x: u256 = 0
    let p: mut u256 = mut x
    let q: mut u256 = mut x
    q = 1
    p = 2
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(
        diags.contains("cannot mutably borrow") || diags.contains("mutable borrow"),
        "{diags:?}",
    );
}

#[test]
fn mutable_enum_payload_reborrow_suspends_the_parent_loan() {
    let diags = borrow_diags(
        r#"
struct Item {
    value: u256,
}

impl Item {
    fn set(mut self, value: u256) {
        self.value = value
    }
}

enum Choice {
    Pair([Item; 2]),
    Triple([Item; 3]),
}

impl Choice {
    fn set(mut self, index: usize, value: u256) {
        match self {
            Choice::Pair(mut items) => items[index].set(value: value),
            Choice::Triple(mut items) => items[index].set(value: value),
        }
    }
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn mutable_enum_payload_reborrow_still_rejects_independent_aliases() {
    let diags = checked_borrow_diags(
        r#"
struct Item {
    value: u256,
}

enum Choice {
    Item(Item),
}

fn bad(choice: mut Choice) {
    match choice {
        Choice::Item(mut item) => {
            let first: mut u256 = mut item.value
            let second: mut u256 = mut item.value
            second = 1
            first = 2
        }
    }
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(diags.contains("cannot mutably borrow"), "{diags:?}");
}

#[test]
fn destructured_tuple_param_field_projection_resolves_its_carrier_root() {
    let diags = borrow_diags(
        r#"
struct Byte {
    val: u8,
}

fn read(input: (Byte, u256)) -> u8 {
    let (byte, _) = input
    byte.val
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn ordinary_effect_handle_fields_use_the_handle_backing_place() {
    let source = r#"
use core::{AddressSpace, EffectHandle}

struct TaggedPtr<T> {
    tag: u256,
    addr: u256,
}

impl<T> EffectHandle for TaggedPtr<T> {
    type Target = T
    const SPACE: AddressSpace = AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { tag: 1, addr: raw }
    }

    fn raw(self) -> u256 {
        self.addr
    }
}

fn identity(_ ptr: TaggedPtr<u256>) -> TaggedPtr<u256> {
    ptr
}

fn read_call_result() -> u256 {
    let ptr = identity(TaggedPtr { tag: 7, addr: 8 })
    ptr.tag
}

fn read_nested_array() -> u256 {
    let ptrs: [TaggedPtr<u256>; 2] = [
        TaggedPtr { tag: 7, addr: 8 },
        TaggedPtr { tag: 9, addr: 10 },
    ]
    ptrs[1].tag
}

fn mutate(mut _ ptr: own TaggedPtr<u256>) -> u256 {
    ptr.tag = 11
    ptr.tag
}
"#;
    let diags = borrow_diags(source);
    assert!(diags.is_empty(), "{diags}");

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    let normalized = normalized_func_body(&db, top_mod, "read_call_result");
    let (value, path) = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|stmt| match &stmt.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::ProjectValue { value, path },
                ..
            } if normalized.body.values[result.index()].ty.pretty_print(&db) == "u256"
                && matches!(
                    path.0.iter().next(),
                    Some(NDataProjection::Field(field)) if field.0 == 0
                ) =>
            {
                Some((value.value, path))
            }
            _ => None,
        })
        .expect("tag field read");
    assert!(
        normalized.body.values[value.index()]
            .ty
            .pretty_print(&db)
            .to_string()
            .starts_with("TaggedPtr<"),
        "ordinary handle field should project the handle value representation: {path:#?}"
    );
}

#[test]
fn ordinary_effect_handle_field_borrows_still_conflict() {
    let diags = borrow_diags(
        r#"
use core::{AddressSpace, EffectHandle}

struct TaggedPtr<T> {
    tag: u256,
    addr: u256,
}

impl<T> EffectHandle for TaggedPtr<T> {
    type Target = T
    const SPACE: AddressSpace = AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { tag: 1, addr: raw }
    }

    fn raw(self) -> u256 {
        self.addr
    }
}

fn conflict(mut _ ptr: own TaggedPtr<u256>) {
    let first: mut u256 = mut ptr.tag
    let second: mut u256 = mut ptr.tag
    second = 1
    first = 2
}
"#,
    );
    assert!(
        diags.contains("borrow conflict in `fn conflict`"),
        "{diags}"
    );
}

#[test]
fn reports_noesc_storage_escape_through_whole_assignment() {
    let diags = borrow_diags(
        r#"
struct Esc {
    h: mut u256,
    tag: u256,
}

pub contract NoEscStore {
    mut slot: Esc

    init() uses (mut slot) {
        let mut x: u256 = 0
        let e: Esc = Esc { h: mut x, tag: 0 }
        slot = e
    }
}
"#,
    );

    assert!(
        diags.contains("noesc violation in `fn NoEscStore::__init__`"),
        "{diags:?}"
    );
    assert!(diags.contains("cannot store `Esc` in storage"), "{diags:?}");
}

#[test]
fn reports_noesc_storage_escape_through_field_assignment() {
    let diags = borrow_diags(
        r#"
struct Esc {
    h: mut u256,
    tag: u256,
}

struct Wrapper {
    e: Esc,
}

pub contract NoEscFieldStore {
    mut slot: Wrapper

    init() uses (mut slot) {
        let mut x: u256 = 0
        let e: Esc = Esc { h: mut x, tag: 0 }
        slot.e = e
    }
}
"#,
    );

    assert!(
        diags.contains("noesc violation in `fn NoEscFieldStore::__init__`"),
        "{diags:?}"
    );
    assert!(diags.contains("cannot store `Esc` in storage"), "{diags:?}");
}

#[test]
fn reports_noesc_storage_escape_through_inline_aggregate_store() {
    let diags = borrow_diags(
        r#"
struct Esc {
    h: mut u256,
    tag: u256,
}

pub contract NoEscInlineStore {
    mut slot: Esc

    init() uses (mut slot) {
        let mut x: u256 = 0
        slot = Esc { h: mut x, tag: 0 }
    }
}
"#,
    );

    assert!(
        diags.contains("noesc violation in `fn NoEscInlineStore::__init__`"),
        "{diags:?}"
    );
    assert!(diags.contains("cannot store `Esc` in storage"), "{diags:?}");
}

#[test]
fn reports_noesc_storage_escape_for_ref_handle_in_stored_aggregate() {
    let diags = borrow_diags(
        r#"
struct Esc {
    h: ref u256,
    tag: u256,
}

pub contract NoEscRefStore {
    mut slot: Esc

    init() uses (mut slot) {
        let x: u256 = 0
        let e: Esc = Esc { h: ref x, tag: 0 }
        slot = e
    }
}
"#,
    );

    assert!(
        diags.contains("noesc violation in `fn NoEscRefStore::__init__`"),
        "{diags:?}"
    );
    assert!(diags.contains("cannot store `Esc` in storage"), "{diags:?}");
}

#[test]
fn reports_storage_borrow_passed_as_regular_function_argument() {
    let diags = borrow_diags(
        r#"
fn bump(_ handle: mut u256) {
    handle += 1
}

pub contract NoEscCallArg {
    mut slot: u256

    init() uses (mut slot) {
        bump(mut slot)
    }
}
"#,
    );

    assert!(
        diags.contains("noesc violation in `fn NoEscCallArg::__init__`"),
        "{diags:?}"
    );
    assert!(
        diags.contains("cannot pass `mut u256` from storage as function argument"),
        "{diags:?}"
    );
}

#[test]
fn allows_memory_noesc_values_and_memory_borrow_call_args() {
    let diags = borrow_diags(
        r#"
struct Esc {
    h: mut u256,
    tag: u256,
}

fn bump(_ handle: mut u256) {
    handle += 1
}

fn ok() {
    let mut x: u256 = 0
    let e: Esc = Esc { h: mut x, tag: 0 }
    let mut y: u256 = 1
    let mut dst: Esc = Esc { h: mut y, tag: 1 }
    dst = e
    let mut z: u256 = 2
    bump(mut z)
}
"#,
    );

    assert!(!diags.contains("noesc violation"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn generic_noesc_store_is_rejected_only_after_storage_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Box<T> {
    value: T,
}

fn store_generic<T>(value: own T) uses (slot: mut Box<T>) {
    slot = Box<T> { value }
}

pub contract GenericNoEsc {
    mut slot: Box<mut u256>

    init() uses (mut slot) {
        let mut x: u256 = 0
        store_generic<mut u256>(mut x)
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let store_generic = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "store_generic") =>
            {
                Some(*func)
            }
            _ => None,
        })
        .expect("store_generic function");
    let identity = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(store_generic)),
    );
    check_semantic_noesc(&db, identity).expect("generic identity noesc should be accepted");

    let init = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Contract(contract) => Some(get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(
                    &db,
                    BodyOwner::ContractInit {
                        contract: *contract,
                    },
                ),
            )),
            _ => None,
        })
        .expect("contract init instance");
    let specialized = init
        .callees(&db)
        .iter()
        .find_map(|callee| match callee.key.owner(&db) {
            BodyOwner::Func(func) if func == store_generic => {
                Some(get_or_build_semantic_instance(&db, callee.key))
            }
            _ => None,
        })
        .expect("specialized store_generic callee");
    let err = check_semantic_noesc(&db, specialized)
        .expect_err("specialized noesc store should be rejected");
    let SemanticAnalysisError::Diagnostic(err) = err else {
        panic!("noesc violation must produce a diagnostic")
    };
    assert!(
        err.message
            .contains("noesc violation in `fn store_generic`"),
        "{err:#?}"
    );
    assert!(
        format!("{err:#?}").contains("cannot store `Box<mut u256>` in storage"),
        "{err:#?}"
    );
}

#[test]
fn rejects_return_borrow_to_local() {
    let diags = borrow_diags(
        r#"
struct Pair {
    a: u256,
    b: u256,
}

fn bad() -> mut u256 {
    let mut x = Pair { a: 0, b: 0 }
    mut x.a
}
"#,
    );

    assert!(
        diags.contains("invalid return borrow in `fn bad`"),
        "{diags:?}"
    );
    assert!(
        diags.contains("cannot return a borrow to local"),
        "{diags:?}"
    );
}

#[test]
fn rejects_return_borrow_derived_from_uses_effect_parameter() {
    let diags = borrow_diags(
        r#"
struct Store {
    value: u256,
}

fn bad() -> mut u256 uses (store: mut Store) {
    mut store.value
}
"#,
    );

    assert!(
        diags.contains("invalid return borrow in `fn bad`"),
        "{diags:?}"
    );
    assert!(
        diags.contains("cannot return a borrow derived from an effect parameter"),
        "{diags:?}"
    );
}

#[test]
fn array_index_reads_do_not_hit_internal_borrowck_error() {
    let diags = borrow_diags(
        r#"
pub fn cast_u8_usize_cmp(indices: [u8; 8], i: usize, j: usize) -> u8 {
    let path = indices[i]
    if j < path as usize {
        return 1
    }
    if j == path as usize {
        return 2
    }
    if j > path as usize {
        return 3
    }
    0
}
"#,
    );

    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn raw_mem_allocate_does_not_report_move_conflict() {
    let diags = checked_borrow_diags(
        r#"
use std::evm::RawMem

fn allocate(bytes: u256) -> u256 uses (mem: mut RawMem) {
    let mut ptr = mem.mload(0x40)
    if ptr == 0 {
        ptr = 0x60
    }
    mem.mstore(addr: 0x40, value: ptr + bytes)
    ptr
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn code_region_fixture_does_not_report_move_conflict() {
    let diags = borrow_diags(include_str!("../../codegen/tests/fixtures/code_region.fe"));
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn create_contract_fixture_does_not_report_top_level_semantic_borrow_errors() {
    let diags = borrow_diags(include_str!(
        "../../codegen/tests/fixtures/create_contract.fe"
    ));
    assert!(!diags.contains("borrow conflict"), "{diags:?}");
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn reports_move_conflict_for_reused_owned_binding() {
    let diags = borrow_diags(
        r#"
struct Inner {}

fn bad(x: own Inner) {
    let y = x
    let z = x
}
"#,
    );

    assert!(diags.contains("move conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn reports_move_conflict_for_non_copy_projection_from_view_param() {
    let diags = borrow_diags(
        r#"
struct Wrapper {
    p: Pair,
}

struct Pair {
    x: u32,
    y: u32,
}

fn unwrap(w: Wrapper) -> Pair {
    let p = w.p
    p
}
"#,
    );

    assert!(diags.contains("move conflict in `fn unwrap`"), "{diags:?}");
    assert!(
        diags.contains("cannot move out of a view parameter"),
        "{diags:?}"
    );
}

#[test]
fn non_copy_projection_to_view_receiver_does_not_move_from_view_param() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Row {
    cells: [u256; 4],
}

impl Row {
    fn get_cell(self, col: usize) -> u256 {
        self.cells[col]
    }

    fn has_value(self, val: u256) -> bool {
        let mut c: usize = 0
        while c < 4 {
            if self.cells[c] == val {
                return true
            }
            c += 1
        }
        return false
    }
}

struct Board {
    rows: [Row; 4],
}

fn read_board(board: Board, row: usize, col: usize) -> u256 {
    board.rows[row].get_cell(col: col)
}

fn find_empty(board: Board, row: usize, col: usize) -> bool {
    if board.rows[row].has_value(val: 0) {
        return board.rows[row].get_cell(col: col) == 0
    }
    false
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    );
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );

    let normalized = normalized_func_body(&db, top_mod, "read_board");
    let row_read_mode = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|stmt| match &stmt.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::Load { mode, .. },
            } if normalized.body.values[result.index()].ty.pretty_print(&db) == "Row" => Some(mode),
            _ => None,
        })
        .expect("row projection read");
    assert_eq!(*row_read_mode, ReadMode::Read);
}

#[test]
fn non_copy_field_projection_to_view_receiver_does_not_move_from_mut_receiver() {
    let diags = borrow_diags(
        r#"
struct LockStore {
    active: bool,
}

impl LockStore {
    fn is_active(self) -> bool {
        self.active
    }
}

struct RegistryStore {
    lock_store: LockStore,
}

impl RegistryStore {
    fn check(mut self) -> bool {
        self.lock_store.is_active()
    }
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn nested_copy_projection_from_view_param_remains_allowed() {
    let diags = borrow_diags(
        r#"
struct Wrapper {
    p: Pair,
}

struct Pair {
    x: u32,
    y: u32,
}

fn read_x(w: Wrapper) -> u32 {
    w.p.x
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn non_copy_projection_move_does_not_report_conflict() {
    let diags = borrow_diags(
        r#"
struct E {}
struct Inner {}
struct Container {
    value: Inner,
}

fn sink(_ value: own Inner, _ e: mut E) {}

impl Container {
    fn enc(own self, e: mut E) {
        sink(self.value, mut e)
    }
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn generic_tuple_projection_move_does_not_report_conflict() {
    let diags = borrow_diags(
        r#"
struct E {}

fn sink<T>(_ value: own T, _ e: mut E) {}

trait Enc {
    fn enc(own self, e: mut E)
}

impl<T0> Enc for (T0,) {
    fn enc(own self, e: mut E) {
        sink<T0>(self.0, mut e)
    }
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn enum_variant_test_does_not_consume_owned_value() {
    let diags = borrow_diags(
        r#"
fn decode(word: u256) -> u64 {
    if let Option::Some(value) = word.downcast() {
        return value
    }
    0
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn nested_owned_enum_match_does_not_report_move_conflict() {
    let diags = borrow_diags(
        r#"
enum Inner {
    Unit,
    Value(u8),
}

enum Outer {
    First(Inner),
    Second(u8),
}

fn read(outer: own Outer) -> u8 {
    match outer {
        Outer::First(Inner::Unit) => 0
        Outer::First(Inner::Value(x)) => x
        Outer::Second(y) => y
    }
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn multi_field_owned_enum_match_does_not_report_move_conflict() {
    let diags = borrow_diags(
        r#"
struct Boxed {}

enum Pair {
    Both(Boxed, Boxed),
}

fn take(_ value: own Boxed) {}

fn read(pair: own Pair) {
    match pair {
        Pair::Both(lhs, rhs) => {
            take(lhs)
            take(rhs)
        }
    }
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn effect_handle_field_deref_fixture_does_not_report_semantic_borrow_errors() {
    let diags = borrow_diags(include_str!(
        "../../codegen/tests/fixtures/effect_handle_field_deref.fe"
    ));
    assert!(!diags.contains("borrow conflict"), "{diags:?}");
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn root_object_direct_values_preserve_provider_roots_in_normalized_borrowck() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
use std::evm::{Address, StorageMap}

struct TokenStore {
    balances: StorageMap<Address, u256>,
}

fn read_balance(addr: Address) -> u256 uses (store: TokenStore) {
    let balance = store.balances.get(key: addr)
    balance
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "read_balance") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("read_balance instance");
    if let Err(diag) = check_semantic_borrows(&db, instance) {
        panic!("{diag:?}");
    }
    let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
    let source = instance.body(&db);
    let (store_local, store) = source
        .locals
        .iter()
        .enumerate()
        .find_map(|(index, local)| match local.source {
            Some(fe_hir::analysis::ty::ty_check::LocalBinding::EffectParam { .. }) => {
                Some((fe_hir::analysis::semantic::SLocalId::new(index), local))
            }
            _ => None,
        })
        .expect("store effect binding");
    let (provider_root, root) = normalized
        .body
        .roots
        .iter()
        .enumerate()
        .find(|(_, root)| matches!(root.kind, NRootKind::Provider { .. }) && root.ty == store.ty)
        .map(|(index, root)| (fe_hir::analysis::semantic::NRootId::new(index), root))
        .expect("store binding must normalize to a provider root");
    assert_eq!(
        normalized.layout_plan.root_source(provider_root),
        None,
        "provider identity must not be represented by a source local"
    );
    assert_eq!(root.address_space, ProviderAddressSpace::Memory);
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                &statement.kind,
                NStatementKind::Define {
                    expr: NExpr::Load { place, .. },
                    ..
                } if place.base == NPlaceBase::Root(provider_root)
                    && matches!(place.path.iter().next(), Some(NDataProjection::Field(field)) if field.0 == 0)
            )
        })
    }));
    assert!(
        normalized
            .layout_plan
            .use_backings
            .iter()
            .any(|backing| matches!(
                backing.source,
                fe_hir::analysis::semantic::NLayoutBackingSource::Root { root, .. }
                    if root == provider_root
            )),
        "provider-rooted loads must retain runtime backing provenance for {store_local:?}"
    );
}

#[test]
fn ref_projection_preserves_place_borrow_lowering() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Pair {
    x: u256,
}

fn read(pair: Pair) -> u256 {
    let r: ref u256 = ref pair.x
    r
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "read") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("read instance");
    let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
    let borrow = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|stmt| match &stmt.kind {
            NStatementKind::Define {
                expr:
                    NExpr::Borrow {
                        place,
                        kind: BorrowKind::Ref,
                        ..
                    },
                ..
            } => Some(place),
            _ => None,
        })
        .expect("borrow expression");
    let NPlaceBase::CapabilityTarget { carrier } = borrow.base else {
        panic!("expected capability-target view-param place for ref projection: {borrow:#?}")
    };
    assert!(matches!(
        normalized.body.value(carrier).map(|value| value.definition),
        Some(NValueDefinition::EntryParam { param: 0 })
    ));
    assert_eq!(borrow.path.len(), 1);
    assert_eq!(
        borrow.path.iter().next(),
        Some(&NDataProjection::Field(
            fe_hir::analysis::semantic::FieldIndex(0)
        ))
    );
}

#[test]
fn nested_projection_through_borrow_field_uses_explicit_capability_target() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Data {
    x: u256,
}

struct View {
    d: ref Data,
}

fn read(v: own View) -> u256 {
    v.d.x
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let normalized = normalized_func_body(&db, top_mod, "read");
    check_semantic_borrows(&db, normalized.body.owner)
        .unwrap_or_else(|error| panic!("nested projection should borrowcheck: {error:#?}"));
    let (carrier, field) = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|statement| match &statement.kind {
            NStatementKind::Define {
                expr: NExpr::Load { place, .. },
                ..
            } => match place.base {
                NPlaceBase::CapabilityTarget { carrier } => Some((carrier, place)),
                NPlaceBase::Root(_) => None,
            },
            NStatementKind::Define { .. } | NStatementKind::Store { .. } => None,
        })
        .expect("projection through nested borrow field");
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                &statement.kind,
                NStatementKind::Define {
                    result,
                    expr: NExpr::ProjectValue { path, .. },
                } if *result == carrier
                    && path.0.iter().eq([&NDataProjection::Field(
                        fe_hir::analysis::semantic::FieldIndex(0),
                    )])
            )
        })
    }));
    assert_eq!(
        field.base,
        NPlaceBase::CapabilityTarget { carrier },
        "nested projection must follow the loaded capability carrier: {field:#?}",
    );
    assert_eq!(
        field.path.iter().collect::<Vec<_>>(),
        vec![&NDataProjection::Field(
            fe_hir::analysis::semantic::FieldIndex(0),
        )],
    );
}

#[test]
fn projected_direct_value_snapshots_keep_lineage_without_reviving_aliases() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Pair {
    x: u256,
}

struct Wrapper {
    pair: Pair,
}

fn read(wrapper: own Wrapper) -> u256 {
    let pair = wrapper.pair
    let copy = pair
    let r: ref Pair = ref copy
    r.x
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "read") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("read instance");
    let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
    let source = instance.body(&db);
    let pair_ty = source
        .locals
        .iter()
        .find(|local| {
            matches!(
                local.source,
                Some(fe_hir::analysis::ty::ty_check::LocalBinding::Local { .. })
            ) && local.ty.is_struct(&db)
        })
        .map(|local| local.ty)
        .expect("pair locals should exist");
    let locals = source
        .locals
        .iter()
        .enumerate()
        .filter_map(|(idx, local)| match local.source {
            Some(fe_hir::analysis::ty::ty_check::LocalBinding::Local { .. })
                if local.ty == pair_ty =>
            {
                Some(fe_hir::analysis::semantic::SLocalId::new(idx))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        locals.len(),
        2,
        "expected pair/copy locals, got {locals:#?}"
    );
    let [pair_local, copy_local] = locals.as_slice() else {
        panic!("expected pair/copy locals, got {locals:#?}")
    };
    let root_for = |local| {
        normalized
            .body
            .roots
            .iter()
            .enumerate()
            .find_map(|(index, root)| {
                let root_id = fe_hir::analysis::semantic::NRootId::new(index);
                (normalized.layout_plan.root_source(root_id) == Some(local)
                    && matches!(root.kind, NRootKind::LocalSlot { .. }))
                .then_some(root_id)
            })
            .unwrap_or_else(|| panic!("missing local-slot root for {local:?}"))
    };
    let copy_root = root_for(*copy_local);

    let pair_value = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|statement| match &statement.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::Forward { src },
            } if normalized.layout_plan.value_source(*result) == Some(*pair_local) => {
                Some((*result, src.value))
            }
            _ => None,
        })
        .expect("pair forwarding value");
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                &statement.kind,
                NStatementKind::Define {
                    result,
                    expr: NExpr::ProjectValue { path, .. },
                } if *result == pair_value.1
                    && path.0.iter().eq([&NDataProjection::Field(
                        fe_hir::analysis::semantic::FieldIndex(0),
                    )])
            )
        })
    }));
    assert!(normalized.body.roots.iter().enumerate().all(|(index, _)| {
        normalized
            .layout_plan
            .root_source(fe_hir::analysis::semantic::NRootId::new(index))
            != Some(*pair_local)
    }));
    let copy_value = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|statement| match statement.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::Forward { src },
            } if normalized.layout_plan.value_source(result) == Some(*copy_local)
                && src.value == pair_value.0 =>
            {
                Some(result)
            }
            _ => None,
        })
        .expect("copy forwarding value");
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                statement.kind,
                NStatementKind::Store { ref destination, value }
                    if destination.base == NPlaceBase::Root(copy_root)
                        && value.value == copy_value
            )
        })
    }));

    let borrow = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|stmt| match &stmt.kind {
            NStatementKind::Define {
                expr:
                    NExpr::Borrow {
                        place,
                        kind: BorrowKind::Ref,
                        ..
                    },
                ..
            } => Some(place),
            _ => None,
        })
        .expect("borrow expression");
    assert_eq!(borrow.base, NPlaceBase::Root(copy_root));
    assert!(borrow.path.is_empty());
}

#[test]
fn nested_place_reads_normalize_as_one_composite_place() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Table {
    used: [u8; 4],
    keys: [u256; 4],
    values: [u256; 4],
}

impl Table {
    fn get_used(self, _ slot: usize) -> u8 {
        self.used[slot]
    }

    fn get_keys(self, _ slot: usize) -> u256 {
        self.keys[slot]
    }

    fn get_values(self, _ slot: usize) -> u256 {
        self.values[slot]
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    for (name, field, elem_ty) in [
        ("get_used", 0, "u8"),
        ("get_keys", 1, "u256"),
        ("get_values", 2, "u256"),
    ] {
        let normalized = normalized_func_body(&db, top_mod, name);
        let mut saw_nested_read = false;
        for stmt in normalized
            .body
            .blocks
            .iter()
            .flat_map(|block| block.statements.iter())
        {
            let NStatementKind::Define {
                result,
                expr: NExpr::Load { place, .. },
            } = &stmt.kind
            else {
                continue;
            };
            let value = &normalized.body.values[result.index()];
            if value.ty.pretty_print(&db) == elem_ty {
                assert!(
                    matches!(
                        place.path.iter().cloned().collect::<Vec<_>>().as_slice(),
                        [
                            NDataProjection::Field(path_field),
                            NDataProjection::Index(NIndex::Value(_))
                        ] if usize::from(path_field.0) == field
                    ),
                    "unexpected nested place path in {name}: {:?}",
                    place.path
                );
                saw_nested_read = true;
            }
            assert!(
                !(value.ty.array_len(&db).is_some()
                    && place.path.iter().cloned().collect::<Vec<_>>()
                        == vec![NDataProjection::Field(
                            fe_hir::analysis::semantic::FieldIndex(field as u16)
                        )]),
                "unexpected intermediate whole-array read in {name}: {stmt:?}"
            );
        }
        assert!(
            saw_nested_read,
            "missing nested array element read in {name}"
        );
    }
}

#[test]
fn owned_aggregate_value_boundaries_stay_unrooted() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Table {
    used: [u8; 4],
}

impl Table {
    fn get(own self, _ slot: usize) -> u8 {
        let used = self.used
        used[slot]
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let normalized = normalized_func_body(&db, top_mod, "get");
    let source = normalized.body.owner.body(&db);

    let used_local = source
        .locals
        .iter()
        .enumerate()
        .find_map(|(idx, local)| match local.source {
            Some(fe_hir::analysis::ty::ty_check::LocalBinding::Local { .. })
                if local.ty.array_len(&db).is_some() =>
            {
                Some(fe_hir::analysis::semantic::SLocalId::new(idx))
            }
            _ => None,
        })
        .expect("owned array local");
    assert!(
        normalized.body.roots.iter().enumerate().all(|(index, _)| {
            let root_id = fe_hir::analysis::semantic::NRootId::new(index);
            normalized.layout_plan.root_source(root_id) != Some(used_local)
        }),
        "immutable owned array projection should not force a local-slot root"
    );
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                &statement.kind,
                NStatementKind::Define {
                    expr: NExpr::ProjectValue { path, .. },
                    ..
                } if path.0.iter().eq([&NDataProjection::Field(
                        fe_hir::analysis::semantic::FieldIndex(0),
                    )])
            )
        })
    }));

    let element_path = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|stmt| match &stmt.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::ProjectValue { path, .. },
            } if normalized.body.values[result.index()].ty.pretty_print(&db) == "u8" => Some(path),
            _ => None,
        })
        .expect("element read");
    assert!(
        matches!(
            element_path
                .0
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .as_slice(),
            [NDataProjection::Index(NIndex::Value(_))]
        ),
        "unexpected owned-local projection path: {:?}",
        element_path.0
    );
}

#[test]
fn zero_sized_aggregate_fixture_instances_normalize_and_borrowcheck() {
    for_each_fixture_instance(
        include_str!("../../codegen/tests/fixtures/zero_sized_aggregates.fe"),
        |db, instance| {
            let raw = instance.body(db);
            let artifacts = normalize_raw_body(db, instance, raw, instance.assumptions(db))
                .unwrap_or_else(|error| {
                    panic!(
                        "phase-one normalization failed for {} ({:?}): {error:#?}",
                        owner_name(db, instance.key(db).owner(db)),
                        instance.key(db),
                    )
                });
            verify_normalized_body(db, &artifacts.body).unwrap_or_else(|error| {
                panic!(
                    "phase-one normalized body failed verification for {} ({:?}): {error:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                )
            });
            if let Err(err) = normalize_semantic_body(db, instance) {
                panic!(
                    "normalize failed for {} ({:?}): {err:?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                );
            }
            if let Err(diag) = check_semantic_borrows(db, instance) {
                panic!(
                    "borrowck failed for {} ({:?}): {diag:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                );
            }
        },
    );
}

#[test]
fn if_let_fixture_instances_normalize_and_borrowcheck() {
    for_each_fixture_instance(
        include_str!("../../fe/tests/fixtures/fe_test/if_let_while_let.fe"),
        |db, instance| {
            let raw = instance.body(db);
            let artifacts = normalize_raw_body(db, instance, raw, instance.assumptions(db))
                .unwrap_or_else(|error| {
                    panic!(
                        "phase-one normalization failed for {} ({:?}): {error:#?}\nraw={raw:#?}",
                        owner_name(db, instance.key(db).owner(db)),
                        instance.key(db),
                    )
                });
            verify_normalized_body(db, &artifacts.body).unwrap_or_else(|error| {
                panic!(
                    "phase-one normalized body failed verification for {} ({:?}): {error:#?}\nraw={raw:#?}\nnormalized={:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                    artifacts.body,
                )
            });
            if let Err(diag) = check_semantic_borrows(db, instance) {
                panic!(
                    "borrowck failed for {} ({:?}): {diag:#?}\nraw={raw:#?}\nnormalized={:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                    artifacts.body,
                );
            }
        },
    );
}

#[test]
fn custom_effect_handle_fixture_instances_normalize_and_borrowcheck() {
    for_each_fixture_instance(
        include_str!("../../fe/tests/fixtures/fe_test/effect_handle_representation.fe"),
        |db, instance| {
            let raw = instance.body(db);
            let local_types = raw
                .locals
                .iter()
                .map(|local| {
                    format!(
                        "{} role={:?}",
                        local.ty.pretty_print(db),
                        local
                            .role
                            .root_provider(&raw.locals)
                            .map(|provider| provider.provider_ty.pretty_print(db)),
                    )
                })
                .collect::<Vec<_>>();
            let artifacts = normalize_raw_body(db, instance, raw, instance.assumptions(db))
                .unwrap_or_else(|error| {
                    panic!(
                        "phase-one normalization failed for {} ({:?}): {error:#?}\nlocal_types={local_types:#?}\nraw={raw:#?}",
                        owner_name(db, instance.key(db).owner(db)),
                        instance.key(db),
                    )
                });
            verify_normalized_body(db, &artifacts.body).unwrap_or_else(|error| {
                panic!(
                    "phase-one normalized body failed verification for {} ({:?}): {error:#?}\nraw={raw:#?}\nnormalized={:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                    artifacts.body,
                )
            });
            if let Err(diag) = check_semantic_borrows(db, instance) {
                panic!(
                    "borrowck failed for {} ({:?}): {diag:#?}\nraw={raw:#?}\nnormalized={:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                    artifacts.body,
                );
            }
        },
    );
}

#[test]
fn decompose_ty_app_handles_deep_ty_app_chains_iteratively() {
    let db = HirAnalysisTestDb::default();
    let arg = fe_hir::analysis::ty::ty_def::TyId::u256(&db);
    let mut ty = fe_hir::analysis::ty::ty_def::TyId::bool(&db);
    for _ in 0..10_000 {
        ty = fe_hir::analysis::ty::ty_def::TyId::new(&db, TyData::TyApp(ty, arg));
    }
    assert_eq!(
        ty.base_ty(&db),
        fe_hir::analysis::ty::ty_def::TyId::bool(&db)
    );
    assert_eq!(ty.generic_args(&db).len(), 10_000);
}

#[test]
fn erc20_has_role_self_ty_app_chain_is_acyclic() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        include_str!("../../codegen/tests/fixtures/erc20.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    let has_role = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "has_role") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("has_role fixture function");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*has_role)),
    );
    let ty = instance.body(&db).locals[0].ty;
    let mut seen = rustc_hash::FxHashSet::default();
    let mut cursor = ty;
    loop {
        assert!(seen.insert(cursor), "cyclic ty app chain at {:?}", cursor);
        match cursor.data(&db) {
            TyData::TyApp(lhs, _) => cursor = *lhs,
            _ => break,
        }
    }
}

#[test]
fn array_of_struct_place_lowers_with_resolved_index_then_field() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Subtree {
    left: u256,
    right: u256,
}

struct Tree {
    last_subtrees: [Subtree; 8],
}

fn write(mut tree: Tree, i: usize, h: u256) -> Tree {
    tree.last_subtrees[i].left = h
    tree
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "write") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("write instance");
    let body = instance.body(&db);
    let dst = body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            SStmtKind::Store { dst, .. } => Some(dst),
            SStmtKind::Assign { .. } => None,
        })
        .expect("store statement");

    assert_eq!(dst.path.len(), 3);
    let path = dst.path.iter().collect::<Vec<_>>();
    assert!(matches!(path[0], Projection::Field(0)));
    assert!(matches!(path[1], Projection::Index(_)));
    assert!(matches!(path[2], Projection::Field(0)));
}

#[test]
fn nested_borrowed_parameter_access_retains_each_capability_target() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "nested_input_referents.fe".into(),
        r#"
struct Inner { value: mut u256 }
struct Outer { inner: mut Inner }
fn nested(outer: mut Outer) -> mut u256 {
    mut outer.inner.value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let artifacts = normalized_func_body(&db, top_mod, "nested");
    let body = &artifacts.body;
    let carriers: Vec<_> = body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| match &statement.kind {
            NStatementKind::Define {
                expr: NExpr::Load { place, .. } | NExpr::Borrow { place, .. },
                ..
            } => match place.base {
                NPlaceBase::CapabilityTarget { carrier } => Some(carrier),
                NPlaceBase::Root(_) => None,
            },
            _ => None,
        })
        .collect();
    assert_eq!(carriers.len(), 3);
    assert!(carriers.windows(2).all(|pair| pair[0] != pair[1]));
}

#[test]
fn projected_capability_reborrows_load_the_handle_before_borrowing_its_referent() {
    for_each_fixture_instance(
        r#"
struct Inner { value: mut u256 }
struct Outer { inner: mut Inner }
fn shared(outer: ref Outer) -> ref u256 { ref outer.inner.value }
fn mutable_array(values: mut [mut u256; 2], index: usize) -> mut u256 { mut values[index] }
fn shared_array(values: ref [mut u256; 2], index: usize) -> ref u256 { ref values[index] }
fn local(value: mut u256) -> mut u256 {
    let mut holder = Inner { value }
    mut holder.value
}
"#,
        |db, instance| {
            let artifacts =
                normalize_semantic_body(db, instance).expect("projected reborrow must be admitted");
            let body = &artifacts.body;
            let expected = instance.normalized_result_ty(db);
            let (kind, target) = expected.as_borrow(db).unwrap();
            let (result, place) = body
                .blocks
                .iter()
                .flat_map(|block| &block.statements)
                .find_map(|statement| match &statement.kind {
                    NStatementKind::Define {
                        result,
                        expr:
                            NExpr::Borrow {
                                place,
                                kind: actual,
                                ..
                            },
                    } if *actual == kind && body.values[result.index()].ty == expected => {
                        Some((*result, place))
                    }
                    _ => None,
                })
                .expect("returned reborrow");
            assert_eq!(place.ty, target);
            assert!(place.path.is_empty());
            let NPlaceBase::CapabilityTarget { carrier } = place.base else {
                panic!("reborrow targets the stored handle")
            };
            assert_ne!(result, carrier);
            if owner_name(db, instance.key(db).owner(db)) == "local" {
                let backings: Vec<_> = artifacts.layout_plan.use_backings(carrier).collect();
                assert_eq!(
                    backings.len(),
                    1,
                    "the loaded handle retains its parameter backing"
                );
                assert!(backings[0].target.is_empty());
                let NLayoutBackingSource::Value { value, path } = &backings[0].source else {
                    panic!("the handle came from the incoming parameter")
                };
                assert!(path.is_empty());
                assert!(matches!(
                    body.values[value.index()].definition,
                    NValueDefinition::EntryParam { param: 0 }
                ));
            }
            let value = &body.values[carrier.index()];
            assert_eq!(value.ty.as_borrow(db).unwrap().1, target);
            let NValueDefinition::Statement { block, statement } = value.definition else {
                panic!("projected handle must be loaded")
            };
            assert!(matches!(
                &body.blocks[block.index()].statements[statement as usize].kind,
                NStatementKind::Define {
                    expr: NExpr::Load { .. },
                    ..
                }
            ));
        },
    );
}

#[test]
fn borrowing_scalar_fields_and_direct_parameters_keeps_the_existing_target() {
    for_each_fixture_instance(
        r#"
struct Plain { value: u256 }
fn field(value: mut Plain) -> mut u256 { mut value.value }
fn direct(value: mut u256) -> mut u256 { mut value }
"#,
        |db, instance| {
            let artifacts =
                normalize_semantic_body(db, instance).expect("ordinary borrow must be admitted");
            let body = &artifacts.body;
            assert!(body.blocks.iter().flat_map(|block| &block.statements).all(
                |statement| !matches!(
                    statement.kind,
                    NStatementKind::Define {
                        expr: NExpr::Load { .. },
                        ..
                    }
                )
            ));
            let place = body
                .blocks
                .iter()
                .flat_map(|block| &block.statements)
                .find_map(|statement| match &statement.kind {
                    NStatementKind::Define {
                        expr: NExpr::Borrow { place, .. },
                        ..
                    } => Some(place),
                    _ => None,
                })
                .unwrap();
            let NPlaceBase::CapabilityTarget { carrier } = place.base else {
                panic!("incoming parameter target")
            };
            assert!(matches!(
                body.values[carrier.index()].definition,
                NValueDefinition::EntryParam { param: 0 }
            ));
        },
    );
}

#[test]
fn stores_through_terminal_capability_fields_target_the_referent() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "terminal_capability_store.fe".into(),
        r#"
struct Wrap { handle: mut u256 }
fn inspect() {
    let mut value: u256 = 0
    let mut values = [Wrap { handle: mut value }]
    values[0].handle = 1
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func) => Some(*func),
            _ => None,
        })
        .expect("inspect function");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    assert!(
        !matches!(
            semantic_body_admission(&db, instance),
            SemanticBodyAdmission::Blocked(_)
        ),
        "store body must be HIR-valid"
    );
    let raw = instance.body(&db);
    let artifacts = normalize_raw_body(&db, instance, raw, instance.assumptions(&db))
        .expect("normalized operations");
    let stores: Vec<_> = artifacts
        .body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| {
            let NStatementKind::Store { destination, value } = &statement.kind else {
                return None;
            };
            Some((
                destination.base,
                destination.path.clone(),
                artifacts.body.values[value.value.index()]
                    .ty
                    .pretty_print(&db),
                destination.ty.pretty_print(&db),
            ))
        })
        .collect();
    assert_eq!(
        verify_normalized_body(&db, &artifacts.body),
        Ok(()),
        "store source/destination types: {stores:#?}"
    );
}

#[test]
fn terminal_capability_stores_preserve_loaded_carriers_and_layout_backings() {
    for_each_fixture_instance(
        r#"
struct Wrap { handle: mut u256 }
struct Outer { inner: mut Wrap }
fn local_struct(value: mut u256) {
    let mut holder = Wrap { handle: value }
    holder.handle = 1
}
fn local_array(value: mut u256) {
    let mut holders = [Wrap { handle: value }]
    holders[0].handle = 1
}
fn borrowed_struct(holder: mut Wrap) { holder.handle = 1 }
fn borrowed_array(holders: mut [Wrap; 2]) { holders[0].handle = 1 }
fn borrowed_handles(handles: mut [mut u256; 2]) { handles[1] = 1 }
fn nested(outer: mut Outer) { outer.inner.handle = 1 }
fn dynamic(holders: mut [Wrap; 2], index: usize) { holders[index].handle = 1 }
"#,
        |db, instance| {
            let artifacts =
                normalize_semantic_body(db, instance).expect("terminal store admission");
            let body = &artifacts.body;
            let stores: Vec<_> = body
                .blocks
                .iter()
                .flat_map(|block| &block.statements)
                .filter_map(|statement| {
                    let NStatementKind::Store { destination, .. } = &statement.kind else {
                        return None;
                    };
                    statement.source.map(|_| destination)
                })
                .collect();
            assert_eq!(stores.len(), 1);
            let destination = stores[0];
            assert!(destination.path.is_empty());
            let NPlaceBase::CapabilityTarget { carrier } = destination.base else {
                panic!("terminal store must follow its loaded capability: {destination:?}")
            };
            let NValueDefinition::Statement { block, statement } =
                body.values[carrier.index()].definition
            else {
                panic!("terminal store requires an explicit handle load")
            };
            let NStatementKind::Define {
                expr:
                    NExpr::Load {
                        place,
                        mode: ReadMode::Copy,
                    },
                ..
            } = &body.blocks[block.index()].statements[statement as usize].kind
            else {
                panic!("terminal carrier must be loaded from its structural slot")
            };
            assert_eq!(
                place.ty.as_borrow(db),
                Some((BorrowKind::Mut, destination.ty))
            );
            assert!(!place.path.is_empty());
        },
    );
}

#[test]
fn terminal_store_normalization_preserves_direct_targets_and_aggregate_replacement() {
    for_each_fixture_instance(
        r#"
struct Wrap { handle: mut u256 }
struct Scalar { value: u256 }
fn direct(value: mut u256) { value = 1 }
fn scalar_field(value: mut Scalar) { value.value = 1 }
fn whole_element(values: mut [Wrap; 1], replacement: mut u256) {
    values[0] = Wrap { handle: replacement }
}
fn whole_local_element(replacement: mut u256) {
    let mut value: u256 = 0
    let mut values = [Wrap { handle: mut value }]
    values[0] = Wrap { handle: replacement }
}
"#,
        |db, instance| {
            let artifacts =
                normalize_semantic_body(db, instance).expect("ordinary store admission");
            let body = &artifacts.body;
            for statement in body.blocks.iter().flat_map(|block| &block.statements) {
                if let NStatementKind::Store { destination, value } = &statement.kind {
                    assert_eq!(destination.ty, body.values[value.value.index()].ty);
                    if let NPlaceBase::CapabilityTarget { carrier } = destination.base {
                        assert!(
                            matches!(
                                body.values[carrier.index()].definition,
                                NValueDefinition::EntryParam { .. }
                            ),
                            "ordinary stores must keep their direct parameter target"
                        );
                    }
                }
            }
        },
    );
}

#[test]
fn call_result_aggregate_retains_embedded_borrow_loans() {
    let diags = checked_borrow_diags(
        r#"
struct Wrap {
    handle: mut u256,
    tag: u256,
}

fn wrap(handle: mut u256) -> Wrap {
    Wrap { handle, tag: 0 }
}

fn bad() {
    let mut value = 0
    let mut wrapped = wrap(handle: mut value)
    let alias = mut value
    alias = 1
    wrapped.handle = 2
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
}

#[test]
fn aggregate_return_cannot_hide_borrow_of_local() {
    let diags = borrow_diags(
        r#"
struct Wrap {
    handle: mut u256,
}

fn bad() -> Wrap {
    let mut value = 0
    Wrap { handle: mut value }
}
"#,
    );

    assert!(
        diags.contains("invalid return borrow in `fn bad`"),
        "{diags}"
    );
    assert!(
        diags.contains("cannot return a value that holds a borrow of local `value`"),
        "{diags}"
    );
}

#[test]
fn aggregate_call_arguments_check_embedded_borrow_aliases() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

fn write_both(mut left: own Borrowed, mut right: own Borrowed) {
    left.value = 1
    right.value = 2
}

fn bad() {
    let mut value = 0
    let borrowed = mut value
    let left = Borrowed { value: borrowed }
    let right = Borrowed { value: borrowed }
    write_both(left: left, right: right)
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
    assert!(
        diags.contains("call arguments require conflicting access"),
        "{diags}"
    );
}

#[test]
fn array_call_arguments_require_distinct_mutable_members() {
    let diags = borrow_diags(
        r#"
fn write_both(_ values: own [mut u256; 2]) {
    let first = values[0]
    let second = values[1]
    first = 1
    second = 2
}

fn read_both(_ values: own [ref u256; 2]) {}
fn write_one(_ values: own [mut u256; 1]) {}

fn valid() {
    let mut left = 0
    let mut right = 0
    write_both([mut left, mut right])
}

fn duplicate_mutable() {
    let mut value = 0
    let handle = mut value
    write_both([handle, handle])
}

fn duplicate_shared_is_valid() {
    let value = 0
    read_both([ref value, ref value])
}

fn length_one_is_valid() {
    let mut value = 0
    write_one([mut value])
}
"#,
    );

    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(
        diags.contains("borrow conflict in `fn duplicate_mutable`"),
        "{diags}"
    );
    assert!(
        !diags.contains("borrow conflict in `fn duplicate_shared_is_valid`"),
        "{diags}"
    );
    assert!(
        !diags.contains("borrow conflict in `fn length_one_is_valid`"),
        "{diags}"
    );
}

#[test]
fn nested_array_and_product_call_arguments_require_injective_borrows() {
    let diags = borrow_diags(
        r#"
struct BorrowArray {
    values: [mut u256; 2],
}

fn consume_nested(_ values: own [[mut u256; 2]; 2]) {}
fn consume_product(_ value: own BorrowArray) {}

fn duplicate_nested() {
    let mut value = 0
    let mut other_left = 0
    let mut other_right = 0
    let handle = mut value
    consume_nested([[handle, mut other_left], [mut other_right, handle]])
}

fn duplicate_in_product() {
    let mut value = 0
    let handle = mut value
    consume_product(BorrowArray { values: [handle, handle] })
}
"#,
    );

    assert!(
        diags.contains("borrow conflict in `fn duplicate_nested`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_in_product`"),
        "{diags}"
    );
}

#[test]
fn borrowed_aggregate_call_arguments_preserve_nested_borrows() {
    let diags = checked_borrow_diags(
        r#"
struct Pair {
    left: mut u256,
    right: mut u256,
}

fn consume_array(_ values: mut [mut u256; 2]) {}
fn consume_pair(_ pair: mut Pair) {}
fn consume_view(_ values: [mut u256; 2]) {}

fn valid_array() {
    let mut left = 0
    let mut right = 0
    let mut values = [mut left, mut right]
    consume_array(values: mut values)
}

fn duplicate_array() {
    let mut value = 0
    let handle = mut value
    let mut values = [handle, handle]
    consume_array(values: mut values)
}

fn valid_pair() {
    let mut left = 0
    let mut right = 0
    let mut pair = Pair { left: mut left, right: mut right }
    consume_pair(pair: mut pair)
}

fn duplicate_pair() {
    let mut value = 0
    let handle = mut value
    let mut pair = Pair { left: handle, right: handle }
    consume_pair(pair: mut pair)
}

fn duplicate_view_control() {
    let mut value = 0
    let handle = mut value
    let values = [handle, handle]
    consume_view(values: values)
}

fn duplicate_local_control() {
    let mut value: u256 = 0
    let handle = mut value
    let values = [handle, handle]
    let first = values[0]
    let second = values[1]
    first = 1
    second = 2
}
"#,
    );

    assert!(
        !diags.contains("borrow conflict in `fn valid_array`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_array`"),
        "{diags}"
    );
    assert!(
        !diags.contains("borrow conflict in `fn valid_pair`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_pair`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_view_control`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_local_control`"),
        "{diags}"
    );
}

#[test]
fn borrowed_aggregate_effect_arguments_preserve_nested_borrows() {
    let source = r#"
struct Pair {
    left: mut u256,
    right: mut u256,
}

fn consume() uses (pair: mut Pair) {
    let selected = pair.left
    selected = 1
}

fn valid_effect() {
    let mut left = 0
    let mut right = 0
    let mut pair = Pair { left: mut left, right: mut right }
    let target = mut pair
    with (target) {
        consume()
    }
}

fn duplicate_effect() {
    let mut value = 0
    let handle = mut value
    let mut pair = Pair { left: handle, right: handle }
    let target = mut pair
    with (target) {
        consume()
    }
}
"#;
    let diags = borrow_diags(source);

    assert!(
        !diags.contains("borrow conflict in `fn valid_effect`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_effect`"),
        "{diags}"
    );
}

#[test]
fn borrowed_aggregate_call_results_preserve_nested_borrows() {
    let diags = borrow_diags(
        r#"
struct Pair {
    left: mut u256,
    right: mut u256,
}

struct Target {
    first: u256,
    second: u256,
}

struct Carrier {
    handle: mut Target,
    plain: u256,
}

fn forward(_ pair: mut Pair) -> mut Pair {
    pair
}

fn forward_carrier(_ carrier: mut Carrier) -> mut Carrier {
    carrier
}

fn distinct_wrapper_and_descendant_targets() {
    let mut target = Target { first: 0, second: 0 }
    let mut carrier = Carrier { handle: mut target, plain: 0 }
    let returned = forward_carrier(carrier: mut carrier)
    let outer_field = mut returned.plain
    let nested = returned.handle
    let nested_field = mut nested.second
    nested_field = 1
    outer_field = 2
}

fn conflict_after_forwarding() {
    let mut left = 0
    let mut right = 0
    let mut pair = Pair { left: mut left, right: mut right }
    let returned = forward(pair: mut pair)
    let selected = returned.left
    let alias = mut left
    alias = 1
    selected = 2
}
"#,
    );

    assert!(
        !diags.contains("borrow conflict in `fn distinct_wrapper_and_descendant_targets`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn conflict_after_forwarding`"),
        "{diags}"
    );
}

#[test]
fn array_enum_variants_are_exclusive_only_within_one_element() {
    let diags = borrow_diags(
        r#"
enum Choice {
    A(mut u256),
    B(mut u256),
}

fn consume(_ values: own [Choice; 2]) {}

fn valid() {
    let mut left = 0
    let mut right = 0
    consume([Choice::A(mut left), Choice::B(mut right)])
}

fn aliases_across_elements() {
    let mut value = 0
    let handle = mut value
    consume([Choice::A(handle), Choice::B(handle)])
}
"#,
    );

    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(
        diags.contains("borrow conflict in `fn aliases_across_elements`"),
        "{diags}"
    );
}

#[test]
fn reading_one_returned_aggregate_borrow_does_not_retain_siblings() {
    let diags = borrow_diags(
        r#"
struct Pair {
    left: mut u256,
    right: mut u256,
}

fn forward(_ pair: own Pair) -> Pair {
    pair
}

fn valid() {
    let mut left = 0
    let mut right = 0
    let returned = forward(Pair { left: mut left, right: mut right })
    let selected = returned.left
    let other = mut right
    other = 1
    selected = 2
}
"#,
    );

    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn returned_array_family_preserves_constant_and_dynamic_aliasing() {
    let diags = borrow_diags(
        r#"
fn forward(_ values: own [mut u256; 2]) -> [mut u256; 2] {
    values
}

fn constant_sibling_is_disjoint() {
    let mut left = 0
    let mut right = 0
    let returned = forward([mut left, mut right])
    let first = returned[0]
    let other = mut right
    other = 1
    first = 2
}

fn dynamic_index_overlaps(index: usize) {
    let mut left = 0
    let mut right = 0
    let returned = forward([mut left, mut right])
    let selected = returned[index]
    let other = mut right
    other = 1
    selected = 2
}
"#,
    );

    assert!(
        !diags.contains("borrow conflict in `fn constant_sibling_is_disjoint`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn dynamic_index_overlaps`"),
        "{diags}"
    );
}

#[test]
fn parameter_array_family_checks_dynamic_member_aliasing() {
    let diags = borrow_diags(
        r#"
fn constant_siblings_are_disjoint(values: own [mut u256; 2]) {
    let first = values[0]
    let second = values[1]
    first = 1
    second = 2
}

fn dynamic_overlaps_constant(values: own [mut u256; 2], index: usize) {
    let selected = values[index]
    let first = values[0]
    selected = 1
    first = 2
}

fn dynamic_indices_may_alias(
    values: own [mut u256; 2],
    left_index: usize,
    right_index: usize,
) {
    let left = values[left_index]
    let right = values[right_index]
    left = 1
    right = 2
}
"#,
    );

    assert!(
        !diags.contains("borrow conflict in `fn constant_siblings_are_disjoint`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn dynamic_overlaps_constant`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn dynamic_indices_may_alias`"),
        "{diags}"
    );
}

#[test]
fn exact_array_overwrites_partition_symbolic_families() {
    let diags = borrow_diags(
        r#"
struct Wrap {
    handle: mut u256,
}

fn forward(_ values: own [Wrap; 2]) -> [Wrap; 2] {
    values
}

fn replace_first(
    mut _ values: own [Wrap; 2],
    replacement: own Wrap,
) -> [Wrap; 2] {
    values[0] = replacement
    values
}

fn local_replacement_releases_old_member() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let mut values = forward(
        [Wrap { handle: mut old_left }, Wrap { handle: mut right }],
    )
    values[0] = Wrap { handle: mut replacement }
    let released = mut old_left
    released = 1
    values[0].handle = 2
    values[1].handle = 3
}

fn sibling_remains_borrowed() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let mut values = forward(
        [Wrap { handle: mut old_left }, Wrap { handle: mut right }],
    )
    values[0] = Wrap { handle: mut replacement }
    let alias = mut right
    alias = 1
    values[1].handle = 2
}

fn helper_return_preserves_override() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let mut returned = replace_first(
        [Wrap { handle: mut old_left }, Wrap { handle: mut right }],
        replacement: Wrap { handle: mut replacement },
    )
    let released = mut old_left
    released = 1
    returned[0].handle = 2
    returned[1].handle = 3
}

fn conditional_replacement_keeps_old(condition: bool) {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let mut values = forward(
        [Wrap { handle: mut old_left }, Wrap { handle: mut right }],
    )
    if condition {
        values[0] = Wrap { handle: mut replacement }
    }
    let alias = mut old_left
    alias = 1
    values[0].handle = 2
}

fn replace_all_local_members(left: mut u256, right: mut u256) -> [Wrap; 2] {
    let mut old_left = 0
    let mut old_right = 0
    let mut values = forward(
        [Wrap { handle: mut old_left }, Wrap { handle: mut old_right }],
    )
    values[0] = Wrap { handle: left }
    values[1] = Wrap { handle: right }
    values
}

fn retain_one_local_member(left: mut u256) -> [Wrap; 2] {
    let mut old_left = 0
    let mut old_right = 0
    let mut values = forward(
        [Wrap { handle: mut old_left }, Wrap { handle: mut old_right }],
    )
    values[0] = Wrap { handle: left }
    values
}
"#,
    );

    assert!(
        !diags.contains("borrow conflict in `fn local_replacement_releases_old_member`"),
        "{diags}"
    );
    assert!(
        !diags.contains("borrow conflict in `fn helper_return_preserves_override`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn sibling_remains_borrowed`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn conditional_replacement_keeps_old`"),
        "{diags}"
    );
    assert!(
        !diags.contains("invalid return borrow in `fn replace_all_local_members`"),
        "{diags}"
    );
    assert!(
        diags.contains("invalid return borrow in `fn retain_one_local_member`"),
        "{diags}"
    );
}

#[test]
fn array_member_reborrow_suspends_only_that_parent_member() {
    let diags = checked_borrow_diags(
        r#"
fn forward(_ values: own [mut u256; 2]) -> [mut u256; 2] {
    values
}

fn reborrow(value: mut u256) -> mut u256 {
    value
}

fn valid() {
    let mut left = 0
    let mut right = 0
    let mut returned = forward([mut left, mut right])
    let first = reborrow(value: returned[0])
    returned[1] = 1
    first = 2
}

fn bad() {
    let mut left = 0
    let mut right = 0
    let mut returned = forward([mut left, mut right])
    let first = reborrow(value: returned[0])
    let alias = mut right
    alias = 1
    returned[1] = 2
    first = 3
}

fn transitive_bad() {
    let mut left = 0
    let mut right = 0
    let mut returned = forward([mut left, mut right])
    let first = reborrow(value: returned[0])
    let first_again = reborrow(value: first)
    let alias = mut right
    alias = 1
    returned[1] = 2
    first_again = 3
}
"#,
    );

    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
    assert!(
        diags.contains("borrow conflict in `fn transitive_bad`"),
        "{diags}"
    );
}

#[test]
fn mutually_exclusive_enum_borrow_slots_do_not_conflict() {
    let diags = borrow_diags(
        r#"
enum Choice {
    A(mut u256),
    B(mut u256),
}

struct Pair {
    left: Choice,
    right: Choice,
}

fn consume(_ choice: own Choice) {}
fn consume_pair(_ pair: own Pair) {}

fn valid(condition: bool) {
    let mut value = 0
    let choice = if condition {
        Choice::A(mut value)
    } else {
        Choice::B(mut value)
    }
    consume(choice)
}

fn bad() {
    let mut value = 0
    let borrowed = mut value
    let pair = Pair {
        left: Choice::A(borrowed),
        right: Choice::B(borrowed),
    }
    consume_pair(pair)
}
"#,
    );

    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
}

#[test]
fn reading_a_forwarded_borrow_as_a_value_drops_loan_state() {
    let diags = borrow_diags(
        r#"
struct Holder {
    tag: u256,
}

impl Holder {
    fn forward(mut self, _ value: mut u256) -> mut u256 {
        value
    }
}

fn valid() -> u256 {
    let mut holder = Holder { tag: 0 }
    let mut local = 7
    let value = holder.forward(mut local)
    value += 5
    value
}
"#,
    );

    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn mutable_receiver_reservation_activates_after_argument_evaluation() {
    let diags = borrow_diags(
        r#"
struct Cell {
    value: u256,
}

impl Cell {
    fn read(self) -> u256 {
        self.value
    }

    fn write(mut self, value: u256) {
        self.value = value
    }

    fn increment(mut self) -> u256 {
        self.value += 1
        self.value
    }
}

fn valid() {
    let mut cell = Cell { value: 1 }
    cell.write(value: cell.read())
    cell.write(value: cell.increment())
}

fn bad() -> u256 {
    let mut cell = Cell { value: 1 }
    let borrowed = ref cell.value
    cell.write(value: cell.read())
    borrowed
}
"#,
    );

    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
}

#[test]
fn recursive_aggregate_borrow_summary_converges() {
    let diags = checked_borrow_diags(
        r#"
struct Owner {
    value: u256,
}

struct Borrowed {
    value: mut u256,
}

fn borrow_value(owner: mut Owner, recurse: bool) -> Borrowed {
    if recurse {
        borrow_value(owner, recurse: false)
    } else {
        Borrowed { value: mut owner.value }
    }
}

fn bad() {
    let mut owner = Owner { value: 0 }
    let mut borrowed = borrow_value(owner: mut owner, recurse: true)
    let other = mut owner.value
    other = 1
    borrowed.value = 2
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
}

#[test]
fn opaque_aggregate_return_summary_is_conservative() {
    let diags = checked_borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

trait BorrowValue {
    fn borrow_value(mut self) -> Borrowed
}

fn bad<T: BorrowValue>(value: mut T) {
    let mut first = value.borrow_value()
    let mut second = value.borrow_value()
    first.value = 1
    second.value = 2
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
}

#[test]
fn opaque_array_result_does_not_assume_pointwise_family_correlation() {
    let diags = checked_borrow_diags(
        r#"
trait Permute {
    fn permute(self, values: own [mut u256; 2]) -> [mut u256; 2]
}

fn bad<T: Permute>(permuter: T) {
    let mut left = 0
    let mut right = 0
    let returned = permuter.permute(values: [mut left, mut right])
    let selected = returned[0]
    let alias = mut right
    alias = 1
    selected = 2
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
}

#[test]
fn mutable_input_poststates_match_inline_handle_replacement() {
    let diagnostics = borrow_diags(
        r#"struct Wrap {
    handle: mut u256,
}

fn replace(values: mut [Wrap; 1], replacement: mut u256) {
    values[0] = Wrap { handle: replacement }
}

fn call_replaces_nested_handle() {
    let mut first: u256 = 0
    let mut second: u256 = 0
    let mut values = [Wrap { handle: mut first }]
    replace(values: mut values, replacement: mut second)
    let competing = mut second
    values[0].handle = 1
    competing = 2
}

fn direct_replaces_nested_handle() {
    let mut first: u256 = 0
    let mut second: u256 = 0
    let mut values = [Wrap { handle: mut first }]
    values[0] = Wrap { handle: mut second }
    let competing = mut second
    values[0].handle = 1
    competing = 2
}
"#,
    );
    assert!(
        diagnostics.contains("borrow conflict in `fn call_replaces_nested_handle`"),
        "{diagnostics}"
    );
    assert!(
        diagnostics.contains("borrow conflict in `fn direct_replaces_nested_handle`"),
        "{diagnostics}"
    );
    assert!(
        !diagnostics.contains("internal borrow checking error"),
        "{diagnostics}"
    );
}

#[test]
fn opaque_handle_construction_and_summary_choices_preserve_declared_contracts() {
    let source = r#"
use core::{AddressSpace, EffectHandle}
struct Ptr { addr: u256 }
impl EffectHandle for Ptr {
    type Target = u256
    const SPACE: AddressSpace = AddressSpace::Memory
    fn from_raw(_ raw: u256) -> Self { Self { addr: raw } }
    fn raw(self) -> u256 { self.addr }
}
fn copied(_ raw: u256) -> [Ptr; 2] {
    let ptr = Ptr { addr: raw }
    [ptr, ptr]
}
fn constructed() -> [Ptr; 2] { [Ptr { addr: 32 }, Ptr { addr: 32 }] }
fn forwarded(_ raw: u256) -> [Ptr; 2] { copied(raw) }
"#;
    assert!(checked_borrow_diags(source).is_empty());
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("opaque_handles.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    for (name, expected_choices) in [("copied", 1), ("constructed", 2), ("forwarded", 1)] {
        let artifacts = normalized_func_body(&db, top_mod, name);
        let summary = semantic_borrow_summary(&db, artifacts.body.owner)
            .unwrap()
            .unwrap();
        let values = ValueInterner::new(&db, ValueLimits::default());
        let mut choices = Vec::new();
        for leaf in values.leaves(&summary.result, ValueOccurrence::Summary) {
            let ExternalOrigin::OpaqueHandle(source) = leaf.payload.source.origin else {
                panic!("missing opaque constructor source")
            };
            assert!(matches!(
                source.occurrence,
                OpaqueHandleOccurrence::Summary(_)
            ));
            assert_eq!(
                source.contract.address_space,
                HandleAddressSpace::Known(ProviderAddressSpace::Memory)
            );
            assert_eq!(source.contract.target_ty.pretty_print(&db), "u256");
            choices.push(source.occurrence);
        }
        choices.sort();
        choices.dedup();
        assert_eq!(choices.len(), expected_choices, "{name}");
    }
    let mut artifacts = normalized_func_body(&db, top_mod, "constructed");
    let expr = artifacts
        .body
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.statements)
        .find_map(|statement| {
            if let NStatementKind::Define {
                expr: expr @ NExpr::MakeHandle { .. },
                ..
            } = &mut statement.kind
            {
                Some(expr)
            } else {
                None
            }
        })
        .expect("explicit constructor");
    let NExpr::MakeHandle {
        origin: HandleOrigin::Opaque(contract),
        ..
    } = expr
    else {
        panic!("opaque constructor")
    };
    contract.address_space = HandleAddressSpace::Known(ProviderAddressSpace::Storage);
    assert_eq!(
        verify_normalized_body(&db, &artifacts.body),
        Err(NormalizedBodyVerifyError::InvalidHandleOrigin)
    );
}

#[test]
fn call_results_used_as_views_have_explicit_materialization() {
    let source = r#"
use core::num::IntWord
fn validate(input_len: u256, head_size: u256) {
    let args_len = i256::from_word(input_len)
    if args_len < i256::from_word(head_size) {}
}
"#;
    let diags = checked_borrow_diags(source);
    assert!(diags.is_empty(), "{diags}");
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("call_result_view.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    let artifacts = normalized_func_body(&db, top_mod, "validate");
    assert!(
        artifacts
            .body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .any(|statement| {
                let NStatementKind::Define {
                    expr: NExpr::MakeView { place, .. },
                    ..
                } = &statement.kind
                else {
                    return false;
                };
                let NPlaceBase::Root(root) = place.base else {
                    return false;
                };
                let NRootKind::Temporary { value } = artifacts.body.roots[root.index()].kind else {
                    return false;
                };
                let NValueDefinition::Statement { block, statement } =
                    artifacts.body.values[value.index()].definition
                else {
                    return false;
                };
                matches!(
                    artifacts.body.blocks[block.index()].statements[statement as usize].kind,
                    NStatementKind::Define {
                        expr: NExpr::Call { .. },
                        ..
                    }
                )
            })
    );
}

#[test]
fn shared_member_receivers_create_views_of_the_referent() {
    let diagnostics = checked_borrow_diags(
        r#"
trait Measure { fn size(self) -> usize }
impl Measure for u256 { fn size(self) -> usize { 1 } }
struct Wrapped<T> { base: ref T }
impl<T: Measure> Wrapped<T> {
    fn size(self) -> usize { self.base.size() }
}
fn measure(value: ref u256) -> usize {
    Wrapped { base: value }.size()
}
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn diverging_calls_do_not_construct_contextual_capability_results() {
    let diagnostics = checked_borrow_diags(
        r#"
struct Wrap { handle: mut u256 }
fn abort() -> ! { core::panic() }
fn unreachable_result() -> Wrap { abort() }
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn diverging_calls_still_check_argument_aliasing() {
    let diagnostics = checked_borrow_diags(
        r#"
fn abort(left: mut u256, right: mut u256) -> ! { core::panic() }
fn invalid(value: mut u256) { abort(left: value, right: value) }
"#,
    );
    assert!(diagnostics.contains("borrow conflict"), "{diagnostics}");
}

#[test]
fn nonreturning_wrappers_use_the_structural_summary_fixed_point() {
    let diagnostics = checked_borrow_diags(
        r#"
fn stop() { core::panic() }
fn middle() { stop() }
fn caller() {
    let mut value: u256 = 0
    let live = mut value
    middle()
    value = 1
    live = 2
}
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn generic_effect_handle_bounds_retain_their_declared_target() {
    let diagnostics = checked_borrow_diags(
        r#"
use core::{EffectHandle, EffectRef, Copy}
fn read_generic<H: EffectHandle>(_ handle: H) -> H::Target
    uses (value: H::Target) where H::Target: Copy { value }
fn write_generic<H: EffectHandle>(_ handle: H, value: H::Target)
    uses (target: mut H::Target) { target = value }
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn generic_opaque_handles_preserve_declared_address_spaces() {
    let source = r#"
use core::{AddressSpace, EffectHandle}
struct Ptr<const SP: AddressSpace> { raw: u256 }
impl<const SP: AddressSpace> EffectHandle for Ptr<SP> {
    type Target = u256
    const SPACE: AddressSpace = SP
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}
fn assumed<H: EffectHandle>(_ raw: u256) -> H { H::from_raw(raw) }
fn declared<const SP: AddressSpace>(_ raw: u256) -> Ptr<SP> { Ptr { raw } }
fn memory(_ raw: u256) -> Ptr<AddressSpace::Memory> {
    declared<AddressSpace::Memory>(raw)
}
fn storage(_ raw: u256) -> Ptr<AddressSpace::Storage> {
    assumed<Ptr<AddressSpace::Storage>>(raw)
}
"#;
    let diagnostics = checked_borrow_diags(source);
    assert!(diagnostics.is_empty(), "{diagnostics}");
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("generic_opaque_spaces.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    for (name, expected) in [
        ("assumed", None),
        ("declared", None),
        ("memory", Some(ProviderAddressSpace::Memory)),
        ("storage", Some(ProviderAddressSpace::Storage)),
    ] {
        let artifacts = normalized_func_body(&db, top_mod, name);
        let summary = semantic_borrow_summary(&db, artifacts.body.owner)
            .unwrap()
            .unwrap();
        let values = ValueInterner::new(&db, ValueLimits::default());
        let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
        assert!(!leaves.is_empty(), "missing handle source in {name}");
        for leaf in leaves {
            let ExternalOrigin::OpaqueHandle(source) = leaf.payload.source.origin else {
                panic!("missing opaque source in {name}")
            };
            let space = source.contract.address_space;
            assert_eq!(space.known(), expected, "{name}");
            if expected.is_none() {
                assert!(matches!(space, HandleAddressSpace::Declared { .. }));
                assert!(space.may_alias(HandleAddressSpace::Known(ProviderAddressSpace::Memory)));
                assert!(space.may_alias(HandleAddressSpace::Known(ProviderAddressSpace::Storage)));
            }
        }
    }
}

#[test]
fn loop_carried_borrows_preserve_previous_occurrences() {
    let diagnostics = checked_borrow_diags(
        r#"
struct Wrap { handle: mut u256 }
fn both(left: mut u256, right: mut u256) {
    left = 1
    right = 2
}
fn carried(x: mut u256, y: mut u256, again: bool) {
    let mut holder = Wrap { handle: y }
    while again {
        let next = mut x
        both(left: next, right: holder.handle)
        holder = Wrap { handle: next }
    }
}
fn independent(x: mut u256, y: mut u256, again: bool) {
    while again {
        let next = mut x
        both(left: next, right: mut y)
    }
}
"#,
    );
    assert!(
        diagnostics.contains("borrow conflict in `fn carried`"),
        "{diagnostics}"
    );
    assert!(
        !diagnostics.contains("borrow conflict in `fn independent`"),
        "{diagnostics}"
    );
    assert!(!diagnostics.contains("internal"), "{diagnostics}");
}

#[test]
fn nominal_handles_only_access_targets_at_effect_boundaries() {
    let diags = checked_borrow_diags(
        r#"
use core::{AddressSpace, EffectHandle, EffectRef, EffectRefMut}
struct Ptr { addr: u256 }
impl EffectHandle for Ptr {
    type Target = u256
    const SPACE: AddressSpace = AddressSpace::Memory
    fn from_raw(_ raw: u256) -> Self { Self { addr: raw } }
    fn raw(self) -> u256 { self.addr }
}
impl EffectRef<u256> for Ptr {}
impl EffectRefMut<u256> for Ptr {}
fn consume(first: Ptr, second: Ptr) {}
fn update() uses (value: mut u256) { value = 1 }
fn inspect() -> u256 uses (value: u256) { value }
fn valid() {
    let ptr = Ptr { addr: 32 }
    consume(first: ptr, second: ptr)
    with (ptr) { update() }
}
fn conflict() -> u256 {
    let mut value: u256 = 0
    let borrowed = mut value
    let ptr = Ptr { addr: 32 }
    let result = with (ptr) { inspect() }
    borrowed = 2
    result
}
"#,
    );
    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(
        diags.contains("borrow conflict in `fn conflict`"),
        "{diags}"
    );
}

fn boundary_provider_source(space: &str, target: &str, body: &str) -> String {
    format!(
        r#"
use core::{{AddressSpace, EffectHandle, EffectRef, EffectRefMut}}
struct Ptr {{ addr: u256 }}
impl EffectHandle for Ptr {{
    type Target = {target}
    const SPACE: AddressSpace = AddressSpace::{space}
    fn from_raw(_ raw: u256) -> Self {{ Self {{ addr: raw }} }}
    fn raw(self) -> u256 {{ self.addr }}
}}
impl EffectRef<{target}> for Ptr {{}}
impl EffectRefMut<{target}> for Ptr {{}}
{body}
"#
    )
}

#[test]
fn invalid_record_assignment_is_blocked_before_semantic_lowering() {
    let source = boundary_provider_source(
        "Memory",
        "u256",
        r#"
pub contract InvalidHandle {
    mut slot: Ptr
    init() uses (mut slot) { slot = Ptr { addr: 32 } }
}
"#,
    );
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("invalid_record_assignment.fe".into(), &source);
    let (top_mod, _) = db.top_mod(file);
    let instance = contract_init_instance(&db, top_mod, "InvalidHandle");
    assert!(matches!(
        semantic_body_admission(&db, instance),
        SemanticBodyAdmission::Blocked(_)
    ));
    assert!(matches!(
        normalize_semantic_body(&db, instance),
        Err(SemanticNormalizationFailure::Blocked(_))
    ));
    assert!(matches!(
        semantic_borrow_summary(&db, instance),
        Err(SemanticAnalysisError::Blocked(_))
    ));
    assert!(matches!(
        layout_evidence_body(&db, instance),
        Err(LayoutEvidenceError::Blocked(_))
    ));
    assert!(matches!(
        canonicalize_semantic_consts(&db, instance),
        Err(CtfeError::InvalidBody { .. })
    ));
    let mut passes = initialize_analysis_pass();
    let diags = format_diagnostics(&db, &passes.run_on_module(&db, top_mod));
    assert!(diags.contains("u256") && diags.contains("Ptr"), "{diags}");
}
