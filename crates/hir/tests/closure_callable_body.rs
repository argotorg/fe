use camino::Utf8PathBuf;
use common::indexmap::IndexMap;
use fe_hir::{
    analysis::{
        semantic::{
            NBorrowRoot, NExpr, NSPlaceRoot, NSStmtKind, NormalizedBindingLowering,
            NormalizedBodyFacts, ReadMode, SCallReturnProjectionStep, SExpr, SStmtKind,
            get_or_build_semantic_instance, identity_semantic_instance_key,
            normalize_semantic_body, semantic_borrow_summary,
        },
        ty::{
            closure::{ClosureCallTrait, implemented_closure_call_trait},
            const_ty::CallableInputLayoutHoleOrigin,
            corelib::resolve_core_trait,
            trait_def::TraitInstId,
            trait_resolution::{GoalSatisfiability, TraitSolveCx, is_goal_satisfiable},
            ty_check::{
                BodyOwner, ClosureCaptureAccess, ClosureCaptureConstruction, EffectArg,
                EffectPassMode, LocalBinding, ParamSite, ReturnIndexSource, ReturnProjectionStep,
                ReturnProvenance, ReturnSource, TypedCallableBody, check_func_body,
            },
            ty_def::{ClosureCallMode, TyId},
            ty_is_copy,
        },
    },
    hir_def::{CallableDef, Func, ItemKind, TopLevelMod},
    projection::Projection,
    test_db::{HirAnalysisTestDb, format_diagnostics},
};

fn find_func<'db>(db: &'db HirAnalysisTestDb, top_mod: TopLevelMod<'db>, name: &str) -> Func<'db> {
    top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|func_name| func_name.data(db) == name) =>
            {
                Some(*func)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

#[test]
fn unpacked_mut_closure_param_retains_binding_and_lowers_writes_as_stores() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("unpacked_mut_closure_param.fe"),
        r#"
fn probe() {
    let write = |target: mut| {
        target = 42
    }
    let mut value: u256 = 0
    write.call(mut value)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let probe = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(probe)),
    );
    let closure = probe
        .callees(&db)
        .iter()
        .find_map(|callee| {
            matches!(callee.key.owner(&db), BodyOwner::Closure { .. })
                .then(|| get_or_build_semantic_instance(&db, callee.key))
        })
        .expect("specialized closure instance");
    let normalized = normalize_semantic_body(&db, closure).expect("normalized closure body");
    let (local_id, local) = normalized
        .locals
        .iter()
        .enumerate()
        .find(|(_, local)| {
            matches!(
                local.source,
                Some(LocalBinding::Param {
                    site: ParamSite::Closure(_),
                    idx: 0,
                    ..
                })
            )
        })
        .map(|(idx, local)| {
            (
                fe_hir::analysis::semantic::SLocalId::from_u32(idx as u32),
                local,
            )
        })
        .expect("unpacked logical closure parameter");
    let NormalizedBindingLowering::CarrierLocal {
        root: Some(root), ..
    } = local.lowering
    else {
        panic!("mut closure parameter must be a rooted carrier: {local:?}");
    };
    assert!(
        matches!(
            normalized.root(root),
            Some(NBorrowRoot::LocalSlot { local }) if *local == local_id
        ),
        "an unpacked logical parameter is not a physical ABI parameter",
    );
    assert!(
        normalized.blocks.iter().any(|block| {
            block.stmts.iter().any(|stmt| {
                matches!(
                    &stmt.kind,
                    NSStmtKind::Store {
                        dst:
                            fe_hir::analysis::semantic::NSPlace {
                                root: NSPlaceRoot::CarrierDerefLocal(local),
                                ..
                            },
                        ..
                    } if *local == local_id
                )
            })
        }),
        "assignment through a mut closure parameter must lower as a store",
    );
}

fn source(
    origin: CallableInputLayoutHoleOrigin,
    projection: Vec<ReturnProjectionStep>,
) -> ReturnSource {
    ReturnSource {
        result_projection: Vec::new(),
        origin,
        projection,
    }
}

#[test]
fn closure_return_provenance_uses_its_own_inputs() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_return_provenance.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn make(_ captured: own Boxed) {
    let from_param = |value: own Boxed| -> Boxed { value }
    let from_capture = |_ unit: own ()| -> Boxed { captured }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, func);
    let mut saw_param = false;
    let mut saw_capture = false;
    for (_, info) in typed_body.closure_infos() {
        let callable = TypedCallableBody::new(BodyOwner::closure(&db, info.ty), typed_body);
        let ReturnProvenance::Forwarded(sources) = callable.return_provenance(&db) else {
            panic!(
                "closure return should be forwarded: {:?}",
                callable.return_provenance(&db)
            );
        };
        match info.captures.len() {
            0 => {
                assert_eq!(
                    sources,
                    vec![source(
                        CallableInputLayoutHoleOrigin::ValueParam(1),
                        vec![ReturnProjectionStep::Field(0)]
                    )]
                );
                saw_param = true;
            }
            1 => {
                assert_eq!(
                    sources,
                    vec![source(
                        CallableInputLayoutHoleOrigin::Receiver,
                        vec![ReturnProjectionStep::Field(0)]
                    )]
                );
                saw_capture = true;
            }
            count => panic!("unexpected capture count: {count}"),
        }
    }
    assert!(saw_param && saw_capture);
}

#[test]
fn closure_partial_return_sources_do_not_change_exact_provenance() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_partial_return_provenance.fe"),
        r#"
use core::option::Option::{self, Some}

fn make() {
    let maybe = |target: mut u256, flag: own bool| -> Option<mut u256> {
        let result = if flag {
            Some(target)
        } else {
            Option::None
        }
        result
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, func);
    let (_, closure) = typed_body
        .closure_infos()
        .next()
        .expect("missing closure metadata");
    let callable = TypedCallableBody::new(BodyOwner::closure(&db, closure.ty), typed_body);

    assert_eq!(callable.return_provenance(&db), ReturnProvenance::Fresh);
    assert_eq!(
        callable.forwarded_return_sources(&db),
        vec![ReturnSource {
            result_projection: vec![ReturnProjectionStep::VariantField {
                variant: 0,
                field: 0,
            }],
            origin: CallableInputLayoutHoleOrigin::ValueParam(1),
            projection: vec![ReturnProjectionStep::Field(0)],
        }]
    );
}

#[test]
fn closure_partial_return_sources_propagate_through_fresh_callees() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_partial_return_provenance_through_call.fe"),
        r#"
use core::option::Option::{self, Some}

fn maybe(_ target: mut u256, _ flag: bool) -> Option<mut u256> {
    if flag {
        Some(target)
    } else {
        Option::None
    }
}

fn make() {
    let indirect = |target: mut u256, flag: own bool| -> Option<mut u256> {
        maybe(target, flag)
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let maybe = find_func(&db, top_mod, "maybe");
    let (_, maybe_body) = check_func_body(&db, maybe);
    assert_eq!(
        TypedCallableBody::new(BodyOwner::Func(maybe), maybe_body).return_provenance(&db),
        ReturnProvenance::Fresh
    );

    let func = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, func);
    let (_, closure) = typed_body
        .closure_infos()
        .next()
        .expect("missing closure metadata");
    let callable = TypedCallableBody::new(BodyOwner::closure(&db, closure.ty), typed_body);

    assert_eq!(callable.return_provenance(&db), ReturnProvenance::Fresh);
    assert_eq!(
        callable.forwarded_return_sources(&db),
        vec![ReturnSource {
            result_projection: vec![ReturnProjectionStep::VariantField {
                variant: 0,
                field: 0,
            }],
            origin: CallableInputLayoutHoleOrigin::ValueParam(1),
            projection: vec![ReturnProjectionStep::Field(0)],
        }]
    );
}

#[test]
fn closure_partial_borrow_sources_are_lowered_at_call_sites() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_partial_call_return_sources.fe"),
        r#"
use core::option::Option::{self, Some}

struct Boxed {
    value: u256,
}

fn probe(_ flag: bool) {
    let maybe = |boxed: own Boxed, target: mut u256, flag: own bool|
        -> (Boxed, Option<mut u256>)
    {
        if flag {
            (boxed, Some(target))
        } else {
            (Boxed { value: 0 }, Option::None)
        }
    }
    let mut value = 0
    let held = maybe.call(Boxed { value: 1 }, mut value, flag)

    let masked = |boxed: own Boxed, target: mut u256, flag: own bool|
        -> (Boxed, Option<mut u256>)
    {
        let mut borrowed = Option::None
        if flag {
            borrowed = Some(target)
        }
        (boxed, borrowed)
    }
    let mut other = 0
    let other_held = masked.call(Boxed { value: 2 }, mut other, flag)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let semantic = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(probe)),
    );
    let closure_calls = semantic
        .body(&db)
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .filter_map(|stmt| {
            let SStmtKind::Assign {
                expr:
                    SExpr::Call {
                        callee,
                        return_sources,
                        return_sources_complete,
                        ..
                    },
                ..
            } = &stmt.kind
            else {
                return None;
            };
            matches!(callee.key.owner(&db), BodyOwner::Closure { .. }).then_some((
                callee.key,
                return_sources.as_ref(),
                *return_sources_complete,
            ))
        })
        .collect::<Vec<_>>();
    let (callee_key, return_sources, return_sources_complete) = closure_calls
        .iter()
        .copied()
        .find(|(key, _, _)| {
            matches!(
                key.callable_body(&db).return_provenance(&db),
                ReturnProvenance::Fresh
            )
        })
        .expect("missing Fresh closure call");
    assert!(
        !return_sources_complete,
        "a mixed forwarded/fresh return-source set must be marked incomplete"
    );

    let callable_body = callee_key.callable_body(&db);
    assert_eq!(
        callable_body.return_provenance(&db),
        ReturnProvenance::Fresh
    );
    assert_eq!(
        callable_body.forwarded_return_sources(&db).len(),
        2,
        "the may-analysis should retain both the owned and borrowed sources"
    );
    let borrow_summary =
        semantic_borrow_summary(&db, get_or_build_semantic_instance(&db, callee_key))
            .expect("closure borrow summary")
            .expect("borrowing closure should have a summary");
    assert_eq!(borrow_summary.len(), 1, "{borrow_summary:#?}");
    assert_eq!(return_sources.len(), 1, "{return_sources:#?}");
    assert_eq!(
        return_sources[0].result_projection,
        [
            SCallReturnProjectionStep::Field(1),
            SCallReturnProjectionStep::VariantField {
                variant: 0,
                field: 0,
            },
        ]
    );
    assert_eq!(
        return_sources[0].origin,
        CallableInputLayoutHoleOrigin::ValueParam(1)
    );
    assert_eq!(
        return_sources[0].projection,
        [SCallReturnProjectionStep::Field(1)]
    );

    let (masked_key, masked_sources, masked_sources_complete) = closure_calls
        .iter()
        .copied()
        .find(|(key, _, _)| {
            matches!(
                key.callable_body(&db).return_provenance(&db),
                ReturnProvenance::Forwarded(_)
            )
        })
        .expect("missing partially Forwarded closure call");
    assert!(
        !masked_sources_complete,
        "a mutable local with a fresh initializer and forwarded assignment is incomplete"
    );
    assert_eq!(masked_sources.len(), 2, "{masked_sources:#?}");
    assert!(masked_sources.iter().any(|source| {
        source.result_projection
            == [
                SCallReturnProjectionStep::Field(1),
                SCallReturnProjectionStep::VariantField {
                    variant: 0,
                    field: 0,
                },
            ]
            && source.origin == CallableInputLayoutHoleOrigin::ValueParam(1)
            && source.projection == [SCallReturnProjectionStep::Field(1)]
    }));
    let masked_summary =
        semantic_borrow_summary(&db, get_or_build_semantic_instance(&db, masked_key))
            .expect("masked closure borrow summary")
            .expect("masked borrowing closure should have a summary");
    assert_eq!(masked_summary.len(), 1, "{masked_summary:#?}");
}

#[test]
fn nested_closure_does_not_change_parent_return_provenance() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("parent_return_provenance.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn passthrough(_ value: own Boxed) -> Boxed {
    let unused = |_ unit: own ()| -> u256 { 1 }
    value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "passthrough");
    let (_, typed_body) = check_func_body(&db, func);
    let callable = TypedCallableBody::new(BodyOwner::Func(func), typed_body);
    assert_eq!(
        callable.return_provenance(&db),
        ReturnProvenance::Forwarded(vec![source(
            CallableInputLayoutHoleOrigin::ValueParam(0),
            Vec::new()
        )])
    );
}

#[test]
fn nested_noncopy_capture_makes_every_owning_closure_consuming() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("nested_noncopy_capture.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn make() {
    let boxed = Boxed { value: 42 }
    let outer = |_ unit: own ()| -> Boxed {
        let inner = |_ unit: own ()| -> Boxed { boxed }
        inner.call_once(())
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, func);
    let call_modes = typed_body
        .closure_infos()
        .map(|(_, info)| info.ty.call_mode(&db))
        .collect::<Vec<_>>();
    assert_eq!(
        call_modes,
        vec![ClosureCallMode::Consuming, ClosureCallMode::Consuming]
    );
}

#[test]
fn nested_copy_capture_resolves_reusable_after_outer_parameter_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("nested_inferred_copy_capture.fe"),
        r#"
fn probe() -> u256 {
    let make = |value: own| {
        let get = || value
        get
    }
    let get = make.call_once(21 as u256)
    get.call() + get.call()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let call_modes = typed_body
        .closure_infos()
        .map(|(_, info)| info.ty.call_mode(&db))
        .collect::<Vec<_>>();
    assert_eq!(
        call_modes,
        vec![ClosureCallMode::Reusable, ClosureCallMode::Reusable]
    );
}

#[test]
fn nested_copy_capture_call_waits_for_outer_parameter_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("nested_deferred_copy_capture_call.fe"),
        r#"
fn probe() -> u256 {
    let make = |value: own| {
        let get = || value
        get.call() + get.call()
    }
    make.call_once(21 as u256)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    assert!(
        typed_body
            .closure_infos()
            .all(|(_, info)| info.ty.call_mode(&db) == ClosureCallMode::Reusable)
    );
}

#[test]
fn nested_copy_pattern_capture_resolves_reusable_after_outer_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("nested_inferred_copy_pattern_capture.fe"),
        r#"
fn probe() -> u256 {
    let make = |pair: own| {
        let get = || {
            let (value,) = pair
            value
        }
        get
    }
    let get = make.call_once((21 as u256,))
    get.call() + get.call()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    assert!(
        typed_body
            .closure_infos()
            .all(|(_, info)| info.ty.call_mode(&db) == ClosureCallMode::Reusable)
    );
}

#[test]
fn inferred_copy_construction_propagates_reusability_through_nested_closures() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("nested_inferred_copy_capture_propagation.fe"),
        r#"
fn probe() -> u256 {
    let make = |value: own| {
        let outer = || {
            let inner = || value
            inner
        }
        outer
    }
    let outer = make.call_once(21 as u256)
    let left = outer.call()
    let right = outer.call()
    left.call() + right.call()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    assert!(
        typed_body
            .closure_infos()
            .all(|(_, info)| info.ty.call_mode(&db) == ClosureCallMode::Reusable)
    );
}

#[test]
fn nested_inferred_copy_capture_fn_obligation_waits_for_outer_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("nested_inferred_copy_capture_fn_bound.fe"),
        r#"
use core::functional::Fn
use core::option::Option::{self, Some}

fn invoke<T, F: Fn<(), T>>(_ function: F) -> T {
    function.call()
}

fn probe() -> u256 {
    let make = |value: own| {
        let get = || value
        invoke(get)
    }
    make.call_once(42 as u256)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn nested_inferred_noncopy_capture_does_not_satisfy_fn_obligation() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("nested_inferred_noncopy_capture_fn_bound.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

fn invoke<T, F: Fn<(), T>>(_ function: F) -> T {
    function.call()
}

fn probe() -> Boxed {
    let make = |value: own| {
        let take = || value
        invoke(take)
    }
    make.call_once(Boxed { value: 42 })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("trait bound is not satisfied") && rendered.contains("Fn"),
        "{rendered}"
    );
}

#[test]
fn concrete_specialization_does_not_reclassify_an_unbounded_generic_capture() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("generic_capture_specialization.fe"),
        r#"
fn generic<T>(_ value: own T) -> T {
    let take = || value
    take.call()
}

fn probe() -> u256 {
    generic(42 as u256)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("no method named `call` found"),
        "{rendered}"
    );
}

#[test]
fn copy_bound_makes_a_generic_capture_reusable() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("copy_bound_generic_capture.fe"),
        r#"
fn generic<T: Copy>(_ value: own T) -> T {
    let read = || value
    read.call()
    read.call()
}

fn probe() -> u256 {
    generic(42 as u256)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let generic = find_func(&db, top_mod, "generic");
    let (_, typed_body) = check_func_body(&db, generic);
    assert!(
        typed_body
            .closure_infos()
            .all(|(_, info)| info.ty.call_mode(&db) == ClosureCallMode::Reusable)
    );
}

#[test]
fn nested_inferred_noncopy_capture_remains_consuming() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("nested_inferred_noncopy_capture.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let make = |value: own| {
        let take = || value
        take
    }
    let take = make.call_once(Boxed { value: 42 })
    take.call_once().value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    assert_eq!(
        typed_body
            .closure_infos()
            .filter(|(_, info)| !info.ty.captures(&db).is_empty())
            .map(|(_, info)| info.ty.call_mode(&db))
            .collect::<Vec<_>>(),
        vec![ClosureCallMode::Consuming]
    );
}

#[test]
fn closure_copyability_is_structural_over_capture_types() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_copy_capture.fe"),
        r#"
fn probe() -> u256 {
    let offset = 1
    let add = |value: own u256| value + offset
    let copied = add

    let forty_two = || 42 as u256
    let copied_empty = forty_two

    add.call(20) + copied.call(20) + forty_two.call() + copied_empty.call() - 84
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    assert!(
        typed_body.closure_infos().all(|(_, info)| ty_is_copy(
            &db,
            probe.scope(),
            TyId::closure(&db, info.ty),
            typed_body.assumptions(),
        )),
        "empty closures and closures with only Copy captures must be Copy"
    );

    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_noncopy_capture.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let boxed = Boxed { value: 42 }
    let read = || boxed.value
    let moved = read
    read.call()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let closure = typed_body
        .closure_infos()
        .next()
        .expect("non-Copy capture closure")
        .1;
    assert!(!ty_is_copy(
        &db,
        probe.scope(),
        TyId::closure(&db, closure.ty),
        typed_body.assumptions(),
    ));
}

#[test]
fn discarded_noncopy_capture_makes_closure_consuming() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("discarded_noncopy_capture.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn make() {
    let boxed = Boxed { value: 42 }
    let consume = |_ unit: own ()| -> () {
        boxed
        ()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, func);
    let closure = typed_body
        .closure_infos()
        .next()
        .expect("missing closure")
        .1;
    assert_eq!(closure.ty.call_mode(&db), ClosureCallMode::Consuming);
    assert_eq!(closure.captures[0].access, ClosureCaptureAccess::Move);
}

#[test]
fn borrow_parameter_closures_implement_call_traits_with_capability_argument_packs() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_borrow_param_trait.fe"),
        r#"
fn make() {
    let borrow = |value: mut u256| -> u256 { value }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, func);
    let closure = typed_body
        .closure_infos()
        .next()
        .expect("missing borrow-parameter closure")
        .1
        .ty;
    for trait_name in ["Fn", "FnOnce"] {
        let trait_ = resolve_core_trait(&db, func.scope(), &["functional", trait_name])
            .unwrap_or_else(|| panic!("missing core `{trait_name}` trait"));
        assert!(
            implemented_closure_call_trait(
                &db,
                func.scope(),
                typed_body.assumptions(),
                closure,
                trait_,
            )
            .is_some(),
            "reusable borrow-parameter closure must implement `{trait_name}`",
        );
    }
}

#[test]
fn table_solver_enforces_closure_call_capabilities_and_signature() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_table_solver.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn make() {
    let read_box = Boxed { value: 1 }
    let take_box = Boxed { value: 2 }
    let reusable = || -> u256 { read_box.value }
    let consuming = || -> Boxed { take_box }
    let unary = |value: own u256| -> u256 { value }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, func);
    let closures = typed_body
        .closure_infos()
        .map(|(_, info)| info.ty)
        .collect::<Vec<_>>();
    let reusable = closures
        .iter()
        .copied()
        .find(|closure| {
            closure.call_mode(&db) == ClosureCallMode::Reusable && closure.params(&db).is_empty()
        })
        .expect("reusable closure");
    let consuming = closures
        .iter()
        .copied()
        .find(|closure| closure.call_mode(&db) == ClosureCallMode::Consuming)
        .expect("consuming closure");
    let unary = closures
        .iter()
        .copied()
        .find(|closure| closure.params(&db).len() == 1)
        .expect("unary closure");
    let assumptions = typed_body.assumptions();
    let solve_cx = TraitSolveCx::new(&db, func.scope()).with_assumptions(assumptions);

    let solve = |closure, trait_name, args, ret| {
        let trait_ = resolve_core_trait(&db, func.scope(), &["functional", trait_name])
            .unwrap_or_else(|| panic!("missing core `{trait_name}` trait"));
        let goal = TraitInstId::new(
            &db,
            trait_,
            vec![TyId::closure(&db, closure), args, ret],
            IndexMap::new(),
        );
        is_goal_satisfiable(&db, solve_cx, goal)
    };

    for (closure, trait_name, expected) in [
        (reusable, "Fn", true),
        (reusable, "FnOnce", true),
        (consuming, "Fn", false),
        (consuming, "FnOnce", true),
    ] {
        let result = solve(
            closure,
            trait_name,
            closure.args_pack_ty(&db),
            closure.ret_ty(&db),
        );
        assert_eq!(
            matches!(result, GoalSatisfiability::Satisfied(_)),
            expected,
            "unexpected solver result for {trait_name}: {result:?}",
        );
    }

    assert!(matches!(
        solve(
            reusable,
            "Fn",
            unary.args_pack_ty(&db),
            reusable.ret_ty(&db),
        ),
        GoalSatisfiability::UnSat(_)
    ));
    assert!(matches!(
        solve(reusable, "Fn", reusable.args_pack_ty(&db), TyId::bool(&db),),
        GoalSatisfiability::UnSat(_)
    ));
}

#[test]
fn deferred_closure_expressions_apply_contextual_result_capabilities() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("deferred_closure_result_capabilities.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

impl Boxed {
    fn get(self) -> u256 {
        self.value
    }
}

struct Choice {}

trait Choose<T> {
    fn choose(self) -> T
}

impl Choose<u256> for Choice {
    fn choose(self) -> u256 {
        42
    }
}

impl Choose<bool> for Choice {
    fn choose(self) -> bool {
        true
    }
}

fn pair<T, F: Fn<(view T,), view u256>, G: Fn<(view T,), u256>>(
    _ first: F,
    _ witness: G,
) {}

fn probe() {
    pair(
        |value| value.value,
        |value: Boxed| value.value,
    )
    pair(
        |value| value.get(),
        |value: Boxed| value.value,
    )
    pair(
        |value| value as u256,
        |value: u8| value as u256,
    )
    let selected: view u256 = Choice {}.choose()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn contextual_closure_inference_handles_methods_and_view_callable_params() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_method_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Apply {}

impl Apply {
    fn call_with<T, F: Fn<(view T,), u256>>(
        self,
        _ value: own T,
        _ function: view F,
    ) -> u256 {
        function.call(value)
    }
}

fn probe() -> u256 {
    Apply {}.call_with(Boxed { value: 42 }, |value| value.value)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn fn_signature_can_finish_inferred_copy_capture_before_capability_check() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_fn_signature_infers_capture.fe"),
        r#"
use core::functional::Fn

fn probe() {
    let make = |value: own| {
        let get = || value
        Fn<(), u256>::call(get)
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn direct_call_can_finish_inferred_copy_capture_before_capability_check() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_direct_call_infers_capture.fe"),
        r#"
fn probe() {
    let make = |value: own| {
        let get = || value
        let _: u256 = get()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn contextual_closure_return_capability_flows_through_blocks_and_aggregates() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_nested_return_capability_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}

fn read<F: Fn<(), view Boxed>>(_ function: F) -> u256 {
    function.call().value
}

fn read_wrapped<F: Fn<(), view Boxed>>(_ wrapped: own Wrapped<F>) -> u256 {
    wrapped.function.call().value
}

fn probe() -> u256 {
    let left = Boxed { value: 20 }
    let right = Boxed { value: 22 }
    read({
        || left
    }) + read_wrapped(Wrapped { function: || right })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn contextual_closure_inference_flows_through_aggregate_projections() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_aggregate_projection_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}

fn wrap<F>(function: own F) -> Wrapped<F> {
    Wrapped { function }
}

fn apply<F: Fn<(), view Boxed>>(_ function: F) {}

fn probe(_ index: usize) {
    let tuple_box = Boxed { value: 10 }
    apply((|| tuple_box,).0)

    let block_tuple_box = Boxed { value: 11 }
    apply({
        (|| block_tuple_box,).0
    })

    let array_box = Boxed { value: 11 }
    apply([|| array_box][0])

    let dynamic_array_box = Boxed { value: 12 }
    apply([|| dynamic_array_box][index])

    let record_box = Boxed { value: 13 }
    apply(Wrapped { function: || record_box }.function)

    let call_box = Boxed { value: 14 }
    apply(wrap(function: || call_box).function)

    let block_call_box = Boxed { value: 15 }
    apply({
        wrap(function: || block_call_box).function
    })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let closures = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .collect::<Vec<_>>();
    assert_eq!(closures.len(), 7, "{closures:#?}");
    assert!(
        closures.iter().all(|info| {
            info.ty.call_mode(&db) == ClosureCallMode::Reusable
                && info.captures.len() == 1
                && info.captures[0].access == ClosureCaptureAccess::Read
        }),
        "{closures:#?}",
    );
}

#[test]
fn projected_context_does_not_flow_into_an_unselected_record_field() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_unselected_record_field_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Pair<A, B> {
    selected: A,
    closure: B,
}

fn apply<F: Fn<(), view Boxed>>(_ function: F) {}

fn probe() {
    let boxed = Boxed { value: 42 }
    apply(Pair { selected: 0 as u256, closure: || boxed }.selected)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("trait bound") || rendered.contains("not implemented"),
        "{rendered}",
    );
}

#[test]
fn contextual_closure_inference_replays_semantic_call_receivers_and_operands() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_semantic_call_projection_context.fe"),
        r#"
use core::functional::Fn
use core::ops::{Add, Index, Neg}

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}

impl<F> Index<u256> for Wrapped<F> {
    type Output = F

    fn index(self, _ index: u256) -> F {
        core::panic()
    }
}

impl<F> Add<()> for Wrapped<F> {
    type Output = F

    fn add(own self, _ unit: own ()) -> F {
        self.function
    }
}

impl<F> Neg for Wrapped<F> {
    type Output = F

    fn neg(own self) -> F {
        self.function
    }
}

impl<F> Wrapped<F> {
    fn extract(own self) -> F {
        self.function
    }
}

struct Extractor<F> {}

fn extractor<F>() -> Extractor<F> {
    Extractor {}
}

impl<F> Add<Wrapped<F>> for Extractor<F> {
    type Output = F

    fn add(own self, _ wrapped: own Wrapped<F>) -> Self::Output {
        core::panic()
    }
}

struct TraitWrapped<F> {
    function: F,
}

trait Extract {
    type Output

    fn extract(own self) -> Self::Output
}

impl<F> Extract for TraitWrapped<F> {
    type Output = F

    fn extract(own self) -> F {
        self.function
    }
}

struct BoundedWrapped<F> {
    function: F,
}

impl<F: Fn<(), view Boxed>> Index<u256> for BoundedWrapped<F> {
    type Output = F

    fn index(self, _ index: u256) -> F {
        core::panic()
    }
}

struct DefaultWrapped<F> {
    function: F,
}

trait DefaultExtract {
    type Output

    fn default_extract(own self) -> Self::Output {
        core::panic()
    }
}

impl<F: Fn<(), view Boxed>> DefaultExtract for DefaultWrapped<F> {
    type Output = F
}

struct BoundedTraitWrapped<F> {
    function: F,
}

trait BoundedExtract {
    type Output

    fn bounded_extract(own self) -> Self::Output
}

impl<F: Fn<(), view Boxed>> BoundedExtract for BoundedTraitWrapped<F> {
    type Output = F

    fn bounded_extract(own self) -> F {
        self.function
    }
}

fn apply<F: Fn<(), view Boxed>>(_ function: F) {}

struct Apply {}

impl Apply {
    fn apply<F: Fn<(), view Boxed>>(self, _ function: F) {}
}

fn direct_probe(index: u256) {
    let literal_index = Boxed { value: 1 }
    apply(Wrapped { function: || literal_index }[0])

    let dynamic_index = Boxed { value: 2 }
    apply(Wrapped { function: || dynamic_index }[index])

    let add_lhs = Boxed { value: 3 }
    apply(Wrapped { function: || add_lhs } + ())

    let add_rhs = Boxed { value: 4 }
    apply(extractor() + (Wrapped { function: || add_rhs }))

    let negated = Boxed { value: 5 }
    apply(-Wrapped { function: || negated })

    let inherent = Boxed { value: 6 }
    apply(Wrapped { function: || inherent }.extract())

    let trait_method = Boxed { value: 7 }
    apply(TraitWrapped { function: || trait_method }.extract())

    let bounded = Boxed { value: 8 }
    apply(BoundedWrapped { function: || bounded }[index])

    let default_method = Boxed { value: 9 }
    apply(DefaultWrapped { function: || default_method }.default_extract())

    let ufcs = Boxed { value: 10 }
    apply(Extract::extract(TraitWrapped { function: || ufcs }))

    let bounded_ufcs = Boxed { value: 11 }
    apply(BoundedExtract::bounded_extract(BoundedTraitWrapped {
        function: || bounded_ufcs,
    }))
}

fn deferred_probe(index: u256) {
    let outer = |apply: own| {
        let bounded = Boxed { value: 12 }
        apply.apply(BoundedWrapped { function: || bounded }[index])

        let add_rhs = Boxed { value: 13 }
        apply.apply(extractor() + (Wrapped { function: || add_rhs }))

        let trait_method = Boxed { value: 14 }
        apply.apply(TraitWrapped { function: || trait_method }.extract())

        let ufcs = Boxed { value: 15 }
        apply.apply(Extract::extract(TraitWrapped { function: || ufcs }))

        let bounded_ufcs = Boxed { value: 16 }
        apply.apply(BoundedExtract::bounded_extract(BoundedTraitWrapped {
            function: || bounded_ufcs,
        }))
    }
    outer.call_once(Apply {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    for (func_name, expected_inner_closures) in [("direct_probe", 11), ("deferred_probe", 5)] {
        let func = find_func(&db, top_mod, func_name);
        let (_, typed_body) = check_func_body(&db, func);
        let closures = typed_body
            .closure_infos()
            .map(|(_, info)| info)
            .filter(|info| info.ty.params(&db).is_empty())
            .collect::<Vec<_>>();
        assert_eq!(
            closures.len(),
            expected_inner_closures,
            "{func_name}: {closures:#?}",
        );
        assert!(
            closures.iter().all(|info| {
                info.ty.call_mode(&db) == ClosureCallMode::Reusable
                    && info.captures.len() == 1
                    && info.captures[0].access == ClosureCaptureAccess::Read
            }),
            "{func_name}: {closures:#?}",
        );

        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
        normalize_semantic_body(&db, instance)
            .unwrap_or_else(|_| panic!("failed to normalize `{func_name}`"));
        for info in closures {
            let closure_instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::closure(&db, info.ty)),
            );
            normalize_semantic_body(&db, closure_instance).unwrap_or_else(|_| {
                panic!(
                    "failed to normalize semantic-call closure {:?} in `{func_name}`",
                    info.def
                )
            });
        }
    }
}

#[test]
fn semantic_call_closure_replay_rolls_back_an_incompatible_result_sibling() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_semantic_call_projection_sibling_rollback.fe"),
        r#"
use core::functional::Fn
use core::ops::Neg

struct Boxed {
    value: u256,
}

struct SiblingWrapped<F> {
    function: F,
}

impl<F> Neg for SiblingWrapped<F> {
    type Output = (F, u256)

    fn neg(own self) -> Self::Output {
        core::panic()
    }
}

fn incompatible_sibling<F: Fn<(), view Boxed>>(_ value: (F, bool)) {}

fn probe() {
    let boxed = Boxed { value: 42 }
    incompatible_sibling(-SiblingWrapped { function: || boxed })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("type mismatch")
            && rendered.contains("bool")
            && rendered.contains("u256"),
        "{rendered}",
    );
}

#[test]
fn semantic_call_closure_replay_rejects_conflicting_leaf_replacements() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_semantic_call_projection_leaf_conflict.fe"),
        r#"
use core::functional::{Fn, FnOnce}
use core::ops::Neg

struct Boxed {
    value: u256,
}

struct DuplicateWrapped<F> {
    function: F,
}

impl<F> Neg for DuplicateWrapped<F> {
    type Output = (F, F)

    fn neg(own self) -> Self::Output {
        core::panic()
    }
}

fn conflicting_leaves<
    F: Fn<(), view Boxed>,
    G: FnOnce<(), Boxed>,
>(_ value: (F, G)) {}

fn probe() {
    let boxed = Boxed { value: 42 }
    conflicting_leaves(-DuplicateWrapped { function: || boxed })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("trait bound") && rendered.contains("Fn"),
        "{rendered}",
    );
}

#[test]
fn contextual_reusable_closure_array_repetition_still_requires_copy() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_contextual_array_repetition_copy.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

fn accept<F: Fn<(), view Boxed>>(_ functions: [F; 2]) {}

fn probe() {
    let boxed = Boxed { value: 42 }
    accept([|| boxed; 2])
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("array repetition requires") && rendered.contains("Copy"),
        "{rendered}",
    );
}

#[test]
fn generic_fn_expectation_reaches_closure_in_if_with_diverging_branch() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_if_argument_with_diverging_branch.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

fn read<F: Fn<(), view Boxed>>(_ function: F) -> u256 {
    function.call().value
}

fn stop() -> ! {
    core::panic()
}

fn probe(_ flag: bool) -> u256 {
    let boxed = Boxed { value: 42 }
    read(if flag {
        || boxed
    } else {
        stop()
    })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let closures = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .collect::<Vec<_>>();
    assert_eq!(closures.len(), 1, "{closures:#?}");
    assert_eq!(closures[0].ty.call_mode(&db), ClosureCallMode::Reusable);
    assert_eq!(closures[0].captures.len(), 1, "{closures:#?}");
    assert_eq!(closures[0].captures[0].access, ClosureCaptureAccess::Read);
}

#[test]
fn generic_fn_expectation_reaches_closure_in_match_with_diverging_arm() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_match_argument_with_diverging_arm.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

enum Choice {
    Closure,
    Diverge,
}

fn read<F: Fn<(), view Boxed>>(_ function: F) -> u256 {
    function.call().value
}

fn stop() -> ! {
    core::panic()
}

fn probe(_ choice: Choice) -> u256 {
    let boxed = Boxed { value: 42 }
    read(match choice {
        Choice::Closure => || boxed,
        Choice::Diverge => stop(),
    })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let closures = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .collect::<Vec<_>>();
    assert_eq!(closures.len(), 1, "{closures:#?}");
    assert_eq!(closures[0].ty.call_mode(&db), ClosureCallMode::Reusable);
    assert_eq!(closures[0].captures.len(), 1, "{closures:#?}");
    assert_eq!(closures[0].captures[0].access, ClosureCaptureAccess::Read);
}

#[test]
fn generic_fn_expectation_does_not_merge_two_live_nominal_closures() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_if_argument_with_two_live_closures.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

fn read<F: Fn<(), view Boxed>>(_ function: F) -> u256 {
    function.call().value
}

fn probe(_ flag: bool) -> u256 {
    let left = Boxed { value: 20 }
    let right = Boxed { value: 22 }
    read(if flag {
        || left
    } else {
        || right
    })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(rendered.contains("type mismatch"), "{rendered}");
}

#[test]
fn contextual_closure_match_ignores_statically_unreachable_nominal_closures() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_dead_match_arm_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Apply {}

impl Apply {
    fn accept<F: Fn<(), view Boxed>>(self, _ function: F) {}
}

fn accept<F: Fn<(), view Boxed>>(_ function: F) {}

fn direct_probe() {
    let boxed = Boxed { value: 42 }
    accept(match true {
        true => || boxed,
        false => || Boxed { value: 0 },
    })
}

fn deferred_probe() {
    let outer = |apply: own| {
        let boxed = Boxed { value: 42 }
        apply.accept(match true {
            true => || boxed,
            false => || Boxed { value: 0 },
        })
    }
    outer.call_once(Apply {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    for func_name in ["direct_probe", "deferred_probe"] {
        let func = find_func(&db, top_mod, func_name);
        let (_, typed_body) = check_func_body(&db, func);
        let live = typed_body
            .closure_infos()
            .map(|(_, info)| info)
            .find(|info| {
                info.ty.params(&db).is_empty()
                    && info.captures.len() == 1
                    && info.captures[0].access == ClosureCaptureAccess::Read
            })
            .unwrap_or_else(|| panic!("missing live contextual closure in `{func_name}`"));
        assert_eq!(live.ty.call_mode(&db), ClosureCallMode::Reusable);

        let function_instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
        normalize_semantic_body(&db, function_instance)
            .unwrap_or_else(|_| panic!("failed to normalize `{func_name}`"));
        for (_, info) in typed_body.closure_infos() {
            let closure_instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::closure(&db, info.ty)),
            );
            normalize_semantic_body(&db, closure_instance).unwrap_or_else(|_| {
                panic!(
                    "failed to normalize closure {:?} in `{func_name}`",
                    info.def
                )
            });
        }
    }
}

#[test]
fn contextual_closure_match_keeps_distinct_reachable_closures_incompatible() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_live_match_arm_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

fn accept<F: Fn<(), view Boxed>>(_ function: F) {}

fn probe(_ flag: bool) {
    let left = Boxed { value: 20 }
    let right = Boxed { value: 22 }
    accept(match flag {
        true => || left,
        false => || right,
    })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(rendered.contains("type mismatch"), "{rendered}");
}

#[test]
fn deferred_method_resolution_replays_closure_return_capability_context() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_deferred_method_return_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Apply {}

impl Apply {
    fn read_with<F: Fn<(), view Boxed>>(self, _ function: F) -> u256 {
        function.call().value
    }
}

fn probe() -> u256 {
    let outer = |apply: own| {
        let boxed = Boxed { value: 42 }
        apply.read_with(|| boxed)
    }
    outer.call_once(Apply {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn deferred_method_replay_refines_structural_return_leaves() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_deferred_structural_return_context.fe"),
        r#"
use core::functional::{Fn, FnOnce}

struct Boxed {
    value: u256,
}

struct Pair<A, B> {
    first: A,
    second: B,
}

enum Choice<A, B> {
    Values(A, B),
}

struct Apply {}

impl Apply {
    fn accept_tuple<F: FnOnce<(), (view Boxed, Boxed)>>(self, _ function: F) {}

    fn accept_array<F: Fn<(), [view Boxed; 2]>>(self, _ function: F) {}

    fn accept_pair<F: FnOnce<(), Pair<view Boxed, Boxed>>>(self, _ function: F) {}

    fn accept_choice<F: FnOnce<(), Choice<view Boxed, Boxed>>>(self, _ function: F) {}

    fn read_cast<F: Fn<(), (view Boxed,)>>(self, _ function: F) -> u256 {
        function.call().0.value + function.call().0.value
    }

    fn accept_inner<F: Fn<(), view Boxed>>(self, _ function: F) {}
}

fn tuple_probe() {
    let outer = |apply: own| {
        let borrowed = Boxed { value: 20 }
        let moved = Boxed { value: 22 }
        apply.accept_tuple(|| ({ borrowed }, moved))
    }
    outer.call_once(Apply {})
}

fn array_probe() {
    let outer = |apply: own| {
        let left = Boxed { value: 20 }
        let right = Boxed { value: 22 }
        apply.accept_array(|| [left, right])
    }
    outer.call_once(Apply {})
}

fn repeat_probe() {
    let outer = |apply: own| {
        let value = Boxed { value: 21 }
        apply.accept_array(|| [value; 2])
    }
    outer.call_once(Apply {})
}

fn pair_probe() {
    let outer = |apply: own| {
        let borrowed = Boxed { value: 20 }
        let moved = Boxed { value: 22 }
        apply.accept_pair(|| Pair { first: borrowed, second: moved })
    }
    outer.call_once(Apply {})
}

fn choice_probe() {
    let outer = |apply: own| {
        let borrowed = Boxed { value: 20 }
        let moved = Boxed { value: 22 }
        apply.accept_choice(|| Choice::Values(borrowed, moved))
    }
    outer.call_once(Apply {})
}

fn cast_probe() -> u256 {
    let outer = |apply: own| {
        let value = Boxed { value: 21 }
        apply.read_cast(|| (value as Boxed,))
    }
    outer.call_once(Apply {})
}

fn nested_probe() {
    let value = Boxed { value: 42 }
    let outer = |apply: own| {
        apply.accept_inner(|| value)
    }
    outer.call_once(Apply {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let capture_accesses = |func_name: &str| {
        let func = find_func(&db, top_mod, func_name);
        let (_, typed_body) = check_func_body(&db, func);
        typed_body
            .closure_infos()
            .find(|(_, info)| info.ty.params(&db).is_empty())
            .map(|(_, info)| {
                (
                    info.ty.call_mode(&db),
                    info.captures
                        .iter()
                        .map(|capture| capture.access)
                        .collect::<Vec<_>>(),
                )
            })
            .unwrap_or_else(|| panic!("missing returned-value closure in `{func_name}`"))
    };

    for func_name in ["tuple_probe", "pair_probe", "choice_probe"] {
        assert_eq!(
            capture_accesses(func_name),
            (
                ClosureCallMode::Consuming,
                vec![ClosureCaptureAccess::Read, ClosureCaptureAccess::Move],
            ),
            "{func_name}",
        );
    }
    assert_eq!(
        capture_accesses("array_probe"),
        (
            ClosureCallMode::Reusable,
            vec![ClosureCaptureAccess::Read, ClosureCaptureAccess::Read],
        ),
    );
    assert_eq!(
        capture_accesses("repeat_probe"),
        (ClosureCallMode::Reusable, vec![ClosureCaptureAccess::Read],),
    );
    assert_eq!(
        capture_accesses("cast_probe"),
        (ClosureCallMode::Reusable, vec![ClosureCaptureAccess::Read],),
    );

    let nested_probe = find_func(&db, top_mod, "nested_probe");
    let (_, typed_body) = check_func_body(&db, nested_probe);
    let inner = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .find(|info| info.ty.params(&db).is_empty())
        .expect("missing inner closure");
    let outer = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .find(|info| !info.ty.params(&db).is_empty())
        .expect("missing outer closure");
    assert_eq!(inner.ty.call_mode(&db), ClosureCallMode::Reusable);
    assert_eq!(inner.captures[0].access, ClosureCaptureAccess::Read);
    assert_eq!(outer.ty.call_mode(&db), ClosureCallMode::Consuming);
    assert_eq!(outer.captures[0].access, ClosureCaptureAccess::Move);

    for func_name in ["cast_probe", "nested_probe"] {
        let func = find_func(&db, top_mod, func_name);
        let _ = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
    }
}

#[test]
fn deferred_closure_replay_preserves_independent_effect_call_metadata() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_deferred_effectful_wrapper.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Slot {}

struct Wrapped<F> {
    function: F,
}

fn wrap<F>(_ function: own F) -> Wrapped<F> uses (slot: Slot) {
    Wrapped { function }
}

struct Apply {}

impl Apply {
    fn read_wrapped<F: Fn<(), view Boxed>>(
        self,
        _ wrapped: own Wrapped<F>,
    ) -> u256 {
        wrapped.function.call().value
    }
}

fn probe() -> u256 uses (slot: Slot) {
    let outer = |apply: own| {
        let boxed = Boxed { value: 42 }
        apply.read_wrapped(wrap(|| boxed))
    }
    outer.call_once(Apply {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let replayed = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .find(|info| info.ty.params(&db).is_empty())
        .expect("missing closure passed through the effectful wrapper");
    assert_eq!(replayed.ty.call_mode(&db), ClosureCallMode::Reusable);
    assert_eq!(replayed.captures.len(), 1, "{replayed:#?}");
    assert_eq!(
        replayed.captures[0].access,
        ClosureCaptureAccess::Read,
        "{replayed:#?}"
    );

    let body = typed_body.body().expect("missing probe body");
    let wrap_call = body
        .exprs(&db)
        .keys()
        .find(|&expr| {
            let Some(callable) = typed_body.callable_expr(expr) else {
                return false;
            };
            let CallableDef::Func(func) = callable.callable_def() else {
                return false;
            };
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "wrap")
        })
        .expect("missing effectful wrapper call");
    let effect_args = typed_body
        .call_effect_args(wrap_call)
        .expect("replayed call lost its resolved effect argument");
    assert_eq!(effect_args.len(), 1, "{effect_args:#?}");
    assert!(
        effect_args[0].provider_is_external_to_closure,
        "late replay must retain the call site's original lexical closure context"
    );
    assert!(
        matches!(
            effect_args[0].arg,
            EffectArg::Binding(_) | EffectArg::Place(_)
        ),
        "the forwarded effect parameter must remain a bound provider argument: {effect_args:#?}"
    );

    let _ = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(probe)),
    );
}

#[test]
fn effect_resolution_remains_eager_enough_to_infer_call_generics() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_effect_driven_generic_inference.fe"),
        r#"
trait Marker<T> {
    fn mark(self)
}

struct Provider {}

impl Marker<u8> for Provider {
    fn mark(self) {}
}

fn infer<T>() uses (provider: Marker<T>) {}

fn probe(provider: own Provider) {
    with (provider) {
        infer()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let body = typed_body.body().expect("missing probe body");
    let infer_call = body
        .exprs(&db)
        .keys()
        .find(|&expr| {
            let Some(callable) = typed_body.callable_expr(expr) else {
                return false;
            };
            let CallableDef::Func(func) = callable.callable_def() else {
                return false;
            };
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "infer")
        })
        .expect("missing effect-driven generic call");
    let callable = typed_body
        .callable_expr(infer_call)
        .expect("missing inferred callable metadata");
    assert!(
        callable
            .generic_args()
            .iter()
            .any(|arg| arg.pretty_print(&db) == "u8"),
        "effect resolution must commit `T = u8`: {callable:#?}"
    );
    assert_eq!(
        typed_body
            .call_effect_args(infer_call)
            .expect("missing resolved effect argument")
            .len(),
        1
    );
}

#[test]
fn with_provider_capture_uses_source_binding_closure_depth() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_with_provider_source_depth.fe"),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

struct Slot {}

struct Ptr {
    raw: u256,
}

impl EffectHandle for Ptr {
    type Target = Slot

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

impl EffectRef<Slot> for Ptr {}

fn read() -> u256 uses (slot: Slot) {
    42
}

fn direct(ptr: own Ptr) -> u256 {
    let function = || {
        with (Slot = ptr) {
            read()
        }
    }
    function.call_once()
}

fn through_block(ptr: own Ptr) -> u256 {
    let function = || {
        with (Slot = { ptr }) {
            read()
        }
    }
    function.call_once()
}

fn through_if(ptr: own Ptr, _ flag: bool) -> u256 {
    let function = |flag: own| {
        with (Slot = if flag { ptr } else { ptr }) {
            read()
        }
    }
    function.call_once(flag)
}

fn local_provider(_ flag: bool) -> u256 {
    let function = |flag: own| {
        let ptr = Ptr { raw: 1 }
        with (Slot = if flag { ptr } else { ptr }) {
            read()
        }
    }
    function.call(flag) + function.call(flag)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    for func_name in ["direct", "through_block", "through_if"] {
        let func = find_func(&db, top_mod, func_name);
        let (_, typed_body) = check_func_body(&db, func);
        let closure = typed_body
            .closure_infos()
            .map(|(_, info)| info)
            .next()
            .expect("missing provider closure");
        assert_eq!(
            closure.ty.call_mode(&db),
            ClosureCallMode::Consuming,
            "{func_name}: {closure:#?}",
        );
        assert_eq!(closure.captures.len(), 1, "{func_name}: {closure:#?}");
        assert_eq!(
            closure.captures[0].construction,
            ClosureCaptureConstruction::Move,
            "{func_name}: {closure:#?}",
        );
        assert!(
            matches!(
                closure.captures[0].access,
                ClosureCaptureAccess::MoveIfNonCopy | ClosureCaptureAccess::Move
            ),
            "{func_name}: {closure:#?}",
        );

        let body = typed_body.body().expect("missing function body");
        let read_call = body
            .exprs(&db)
            .keys()
            .find(|&expr| {
                typed_body.callable_expr(expr).is_some_and(|callable| {
                    matches!(
                        callable.callable_def(),
                        CallableDef::Func(callee)
                            if callee
                                .name(&db)
                                .to_opt()
                                .is_some_and(|name| name.data(&db) == "read")
                    )
                })
            })
            .expect("missing effectful read call");
        let effect_args = typed_body
            .call_effect_args(read_call)
            .expect("missing resolved effect argument");
        assert_eq!(effect_args.len(), 1, "{func_name}: {effect_args:#?}");
        assert!(
            effect_args[0].provider_is_external_to_closure,
            "{func_name}: {effect_args:#?}",
        );

        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
        if func_name == "direct" {
            let normalized =
                normalize_semantic_body(&db, instance).expect("normalized direct-provider body");
            assert!(
                normalized.blocks.iter().any(|block| {
                    block.stmts.iter().any(|stmt| {
                        matches!(
                            &stmt.kind,
                            NSStmtKind::Assign {
                                expr: NExpr::AggregateMake { ty, fields },
                                ..
                            } if ty.as_closure(&db).is_some()
                                && fields.len() == 1
                                && fields[0].mode == ReadMode::Copy
                        )
                    })
                }),
                "runtime normalization must preserve the ABI carrier's physical copy",
            );
        }
    }

    let local = find_func(&db, top_mod, "local_provider");
    let (_, typed_body) = check_func_body(&db, local);
    let closure = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .next()
        .expect("missing local-provider closure");
    assert_eq!(closure.ty.call_mode(&db), ClosureCallMode::Reusable);
    assert!(closure.captures.is_empty(), "{closure:#?}");
    let body = typed_body.body().expect("missing function body");
    let read_call = body
        .exprs(&db)
        .keys()
        .find(|&expr| {
            typed_body.callable_expr(expr).is_some_and(|callable| {
                matches!(
                    callable.callable_def(),
                    CallableDef::Func(callee)
                        if callee
                            .name(&db)
                            .to_opt()
                            .is_some_and(|name| name.data(&db) == "read")
                )
            })
        })
        .expect("missing local-provider effectful call");
    assert!(
        !typed_body
            .call_effect_args(read_call)
            .expect("missing local-provider effect argument")[0]
            .provider_is_external_to_closure
    );
    let _ = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(local)),
    );
}

#[test]
fn with_provider_capture_rejects_distinct_dynamic_sources() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_with_distinct_provider_sources.fe"),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

struct Slot {}

struct Ptr {
    raw: u256,
}

impl EffectHandle for Ptr {
    type Target = Slot

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

impl EffectRef<Slot> for Ptr {}

fn read() -> u256 uses (slot: Slot) {
    42
}

fn probe(left: own Ptr, right: own Ptr, _ flag: bool) -> u256 {
    let function = |flag: own| {
        with (Slot = if flag { left } else { right }) {
            read()
        }
    }
    function.call_once(flag)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert_eq!(
        rendered
            .matches("effect provider used by a closure must be bound")
            .count(),
        1,
        "a dynamic provider with two distinct external sources must remain unbound:\n{rendered}",
    );
}

#[test]
fn ordinary_noncopy_value_capture_normalizes_as_a_move() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_ordinary_noncopy_capture_move.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn probe(boxed: own Boxed) -> u256 {
    let function = || boxed.value
    function.call_once()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let closure = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .next()
        .expect("missing ordinary capture closure");
    assert_eq!(closure.captures.len(), 1, "{closure:#?}");
    assert_eq!(
        closure.captures[0].construction,
        ClosureCaptureConstruction::Move,
        "{closure:#?}",
    );

    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(probe)),
    );
    let normalized =
        normalize_semantic_body(&db, instance).expect("normalized ordinary-capture body");
    assert!(
        normalized.blocks.iter().any(|block| {
            block.stmts.iter().any(|stmt| {
                matches!(
                    &stmt.kind,
                    NSStmtKind::Assign {
                        expr: NExpr::AggregateMake { ty, fields },
                        ..
                    } if ty.as_closure(&db).is_some()
                        && fields.len() == 1
                        && fields[0].mode == ReadMode::Move
                )
            })
        }),
        "ordinary non-Copy capture construction must retain a physical move",
    );
}

#[test]
fn late_resolved_effectful_method_records_external_effect_param_capture() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_late_effectful_method_capture.fe"),
        r#"
struct Slot {}

struct Apply {}

impl Apply {
    fn read(self) -> u256 uses (slot: Slot) {
        42
    }
}

fn probe() -> u256 uses (slot: Slot) {
    let outer = |apply: own| {
        apply.read()
    }
    outer.call_once(Apply {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let outer = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .next()
        .expect("missing outer closure");
    assert!(
        outer
            .captures
            .iter()
            .any(|capture| matches!(capture.binding, LocalBinding::EffectParam { .. })),
        "an effectful method resolved after closure checking must still capture its external provider: {outer:#?}"
    );

    let body = typed_body.body().expect("missing probe body");
    let read_call = body
        .exprs(&db)
        .keys()
        .find(|&expr| {
            let Some(callable) = typed_body.callable_expr(expr) else {
                return false;
            };
            let CallableDef::Func(func) = callable.callable_def() else {
                return false;
            };
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "read")
        })
        .expect("missing late-resolved effectful method call");
    let effect_args = typed_body
        .call_effect_args(read_call)
        .expect("missing late-resolved method effect argument");
    assert_eq!(effect_args.len(), 1, "{effect_args:#?}");
    assert!(
        effect_args[0].provider_is_external_to_closure,
        "late resolution must classify provider provenance at the original call site"
    );

    let _ = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(probe)),
    );
}

#[test]
fn late_resolved_effectful_method_preserves_noncopy_by_value_capture_mode() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_late_effectful_method_by_value_capture.fe"),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

struct Slot {}

struct Ptr {
    raw: u256,
}

impl EffectHandle for Ptr {
    type Target = Slot

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

impl EffectRef<Slot> for Ptr {}

struct Apply {}

impl Apply {
    fn read(self) -> u256 uses (slot: Slot) {
        42
    }
}

fn probe(ptr: own Ptr) -> u256 {
    with (Slot = ptr) {
        let outer = |apply: own| {
            apply.read()
        }
        outer.call_once(Apply {})
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let outer = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .next()
        .expect("missing outer closure");
    assert_eq!(
        outer.ty.call_mode(&db),
        ClosureCallMode::Consuming,
        "a non-Copy by-value effect provider must make the closure consuming: {outer:#?}"
    );
    assert!(
        outer.captures.iter().any(|capture| {
            matches!(
                capture.binding,
                LocalBinding::Param {
                    site: ParamSite::Func(_),
                    ..
                }
            ) && matches!(
                capture.access,
                ClosureCaptureAccess::MoveIfNonCopy | ClosureCaptureAccess::Move
            )
        }),
        "the by-value provider must be represented in the closure capture plan: {outer:#?}"
    );

    let body = typed_body.body().expect("missing probe body");
    let read_call = body
        .exprs(&db)
        .keys()
        .find(|&expr| {
            let Some(callable) = typed_body.callable_expr(expr) else {
                return false;
            };
            let CallableDef::Func(func) = callable.callable_def() else {
                return false;
            };
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "read")
        })
        .expect("missing late-resolved effectful method call");
    let effect_args = typed_body
        .call_effect_args(read_call)
        .expect("missing late-resolved method effect argument");
    assert_eq!(effect_args.len(), 1, "{effect_args:#?}");
    assert_eq!(effect_args[0].pass_mode, EffectPassMode::ByValue);
    assert!(effect_args[0].provider_is_external_to_closure);

    let _ = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(probe)),
    );
}

#[test]
fn late_resolved_by_value_effect_rejects_reusable_call() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_late_effectful_method_rejects_call.fe"),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

struct Slot {}

struct Ptr {
    raw: u256,
}

impl EffectHandle for Ptr {
    type Target = Slot

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

impl EffectRef<Slot> for Ptr {}

struct Apply {}

impl Apply {
    fn read(self) -> u256 uses (slot: Slot) {
        42
    }
}

fn probe(ptr: own Ptr) -> u256 {
    with (Slot = ptr) {
        let outer = |apply: own| {
            apply.read()
        }
        outer.call(Apply {})
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert_eq!(
        rendered.matches("no method named `call`").count(),
        1,
        "late capture finalization must invalidate the provisional reusable call exactly once:\n{rendered}"
    );
    assert_eq!(
        rendered.matches("trait bound is not satisfied").count(),
        0,
        "direct call revalidation must not also report a generic Fn failure:\n{rendered}"
    );
}

#[test]
fn late_resolved_by_value_effect_reselects_direct_call_as_fn_once() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_late_effectful_direct_call.fe"),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

struct Slot {}

struct Ptr {
    raw: u256,
}

impl EffectHandle for Ptr {
    type Target = Slot

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

impl EffectRef<Slot> for Ptr {}

struct Apply {}

impl Apply {
    fn read(self) -> u256 uses (slot: Slot) {
        42
    }
}

fn probe(ptr: own Ptr) -> u256 {
    with (Slot = ptr) {
        let outer = |apply: own| {
            apply.read()
        }
        outer(Apply {})
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let body = typed_body.body().expect("missing probe body");
    let direct_call = body
        .exprs(&db)
        .keys()
        .find(|&expr| {
            matches!(
                expr.data(&db, body),
                fe_hir::hir_def::Partial::Present(fe_hir::hir_def::Expr::Call(..))
            ) && typed_body
                .callable_expr(expr)
                .and_then(|callable| callable.trait_inst())
                .is_some()
        })
        .expect("missing direct closure call");
    let inst = typed_body
        .callable_expr(direct_call)
        .and_then(|callable| callable.trait_inst())
        .expect("direct closure call must retain its trait instance");
    assert_eq!(
        ClosureCallTrait::for_trait(&db, probe.scope(), inst.def(&db)),
        Some(ClosureCallTrait::FnOnce),
        "late capture finalization must reselect direct invocation as FnOnce",
    );
}

#[test]
fn late_resolved_by_value_effect_rejects_generic_fn_bound() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_late_effectful_method_rejects_fn_bound.fe"),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}
use core::functional::Fn

struct Slot {}

struct Ptr {
    raw: u256,
}

impl EffectHandle for Ptr {
    type Target = Slot

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

impl EffectRef<Slot> for Ptr {}

struct Apply {}

impl Apply {
    fn read(self) -> u256 uses (slot: Slot) {
        42
    }
}

fn require_reusable<F: Fn<(Apply,), u256>>(_ function: own F) {}

fn probe(ptr: own Ptr) {
    with (Slot = ptr) {
        let outer = |apply: own| {
            apply.read()
        }
        require_reusable(outer)
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert_eq!(
        rendered.matches("trait bound is not satisfied").count(),
        1,
        "late capture finalization must invalidate provisional generic Fn admission exactly once:\n{rendered}"
    );
    assert!(
        rendered.contains("required by this bound on `require_reusable`")
            && rendered.contains("Fn"),
        "the ordinary call-constraint context must be retained:\n{rendered}"
    );
    assert_eq!(
        rendered.matches("no method named `call`").count(),
        0,
        "generic bound revalidation must remain distinct from direct call revalidation:\n{rendered}"
    );
}

#[test]
fn nested_late_resolved_by_value_effect_rebuilds_all_closures() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_nested_late_effectful_method_capture.fe"),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

struct Slot {}

struct Ptr {
    raw: u256,
}

impl EffectHandle for Ptr {
    type Target = Slot

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

impl EffectRef<Slot> for Ptr {}

struct Apply {}

impl Apply {
    fn read(self) -> u256 uses (slot: Slot) {
        42
    }
}

fn probe(ptr: own Ptr) -> u256 {
    with (Slot = ptr) {
        let outer = |apply: own| {
            let inner = |nested_apply: own| {
                nested_apply.read()
            }
            inner.call_once(apply)
        }
        outer.call_once(Apply {})
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let closures = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .collect::<Vec<_>>();
    assert_eq!(closures.len(), 2, "{closures:#?}");
    for closure in &closures {
        assert_eq!(
            closure.ty.call_mode(&db),
            ClosureCallMode::Consuming,
            "both lexical closures must inherit the late non-Copy provider capture: {closure:#?}"
        );
        let provider_capture = closure
            .captures
            .iter()
            .find(|capture| {
                matches!(
                    capture.binding,
                    LocalBinding::Param {
                        site: ParamSite::Func(_),
                        ..
                    }
                )
            })
            .expect("missing propagated provider capture");
        assert_eq!(
            provider_capture.construction,
            ClosureCaptureConstruction::Move,
            "{closure:#?}"
        );
        assert!(
            matches!(
                provider_capture.access,
                ClosureCaptureAccess::MoveIfNonCopy | ClosureCaptureAccess::Move
            ),
            "{closure:#?}"
        );
    }

    let _ = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(probe)),
    );
}

#[test]
fn deferred_method_replays_wrapped_closure_explicit_return_accesses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_deferred_wrapped_explicit_return_context.fe"),
        r#"
use core::functional::Fn
use core::option::Option::{self, Some}

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}

struct Apply {}

fn wrap<F>(_ function: own F) -> Wrapped<F> {
    Wrapped { function }
}

trait Wrap {
    fn wrap<T>(self, _ function: own T) -> Wrapped<T>
}

struct Builder {}

impl Wrap for Builder {
    fn wrap<T>(self, _ function: own T) -> Wrapped<T> {
        Wrapped { function }
    }
}

impl Apply {
    fn read_wrapped<F: Fn<(), view Boxed>>(self, _ wrapped: own Wrapped<F>) -> u256 {
        wrapped.function.call().value
    }

    fn read_option<F: Fn<(), view Boxed>>(self, _ wrapped: own Option<F>) -> u256 {
        match wrapped {
            Some(function) => function.call().value,
            Option::None => 0,
        }
    }
}

fn probe(_ flag: bool) -> u256 {
    let outer = |apply: own, flag: own| {
        let left = Boxed { value: 20 }
        let right = Boxed { value: 22 }
        let wrapped = Boxed { value: 1 }
        let optional = Boxed { value: 2 }
        let trait_wrapped = Boxed { value: 3 }
        apply.read_wrapped(Wrapped { function: || {
            if flag {
                return left
            }
            return right
        }}) + apply.read_wrapped(wrap(|| wrapped))
            + apply.read_option(Some(|| optional))
            + apply.read_wrapped(Builder {}.wrap(|| trait_wrapped))
    }
    outer.call_once(Apply {}, flag)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let probe = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, probe);
    let inner = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .find(|info| info.return_exprs.len() == 3)
        .expect("missing all-explicit-return nested closure");
    assert_eq!(inner.return_exprs.len(), 3);
    assert!(
        inner
            .captures
            .iter()
            .all(|capture| capture.access == ClosureCaptureAccess::Read),
        "both the explicit and implicit returned captures must be replayed as reads",
    );
    assert!(
        inner
            .captures
            .iter()
            .all(|capture| capture.access_without_return == ClosureCaptureAccess::Read),
        "return-independent capture access must exclude every return site",
    );
    assert!(
        typed_body
            .closure_infos()
            .map(|(_, info)| info)
            .filter(|info| info.ty.params(&db).is_empty())
            .flat_map(|info| &info.captures)
            .all(|capture| capture.access == ClosureCaptureAccess::Read),
        "every closure replayed through an aggregate or wrapper must remain reusable",
    );

    let callable = TypedCallableBody::new(BodyOwner::closure(&db, inner.ty), typed_body);
    callable
        .owner_closure_body(&db)
        .expect("late closure replacement must keep descriptor metadata coherent");
    let _ = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::closure(&db, inner.ty)),
    );
    let outer = typed_body
        .closure_infos()
        .map(|(_, info)| info)
        .find(|info| !info.ty.params(&db).is_empty())
        .expect("missing outer closure");
    let _ = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::closure(&db, outer.ty)),
    );
}

#[test]
fn deferred_replay_preserves_repeated_generic_argument_constraints() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_deferred_repeated_generic_control.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}

fn repeat<F>(_ function: own F, _ witness: own F) -> Wrapped<F> {
    Wrapped { function }
}

struct Apply {}

impl Apply {
    fn read_wrapped<F: Fn<(), view Boxed>>(self, _ wrapped: own Wrapped<F>) -> u256 {
        wrapped.function.call().value
    }
}

fn probe() -> u256 {
    let outer = |apply: own, witness: own| {
        let boxed = Boxed { value: 42 }
        apply.read_wrapped(repeat(|| boxed, witness))
    }
    outer.call_once(Apply {}, core::panic())
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("trait bound is not satisfied")
            && rendered.contains("doesn't implement `Fn"),
        "the repeated `F` equality must reject the consuming closure after transactional replay: {rendered}",
    );
    assert!(
        !rendered.contains("type annotation is needed") && !rendered.contains("type must be known"),
        "the concrete repeated-argument conflict must not degrade into an inference fallback: {rendered}",
    );
}

#[test]
fn outer_call_result_context_constrains_closure_arguments_before_finalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_outer_call_result_context.fe"),
        r#"
use core::functional::Fn

fn infer_view<T, F: Fn<(view T,), T>>(_ function: F) -> T {
    core::panic()
}

fn probe() -> u256 {
    infer_view(|value| value)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn contextual_closure_inference_flows_through_tuple_arguments() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_tuple_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

fn apply_wrapped<F: Fn<(Boxed,), u256>>(_ wrapped: own (F,)) -> u256 {
    wrapped.0.call(Boxed { value: 42 })
}

fn probe() -> u256 {
    apply_wrapped((|value: own| value.value,))
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn contextual_closure_inference_flows_through_struct_arguments() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_struct_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}

fn apply_wrapped<F: Fn<(Boxed,), u256>>(_ wrapped: own Wrapped<F>) -> u256 {
    wrapped.function.call(Boxed { value: 42 })
}

fn probe() -> u256 {
    apply_wrapped(Wrapped { function: |value: own| value.value })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn contextual_closure_inference_flows_through_array_arguments() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_array_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

fn apply_wrapped<F: Fn<(Boxed,), u256>>(_ functions: own [F; 1]) -> u256 {
    functions[0].call(Boxed { value: 42 })
}

fn probe() -> u256 {
    apply_wrapped([|value: own| value.value])
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn contextual_closure_inference_flows_through_enum_arguments() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_enum_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

enum Wrapped<F> {
    Function(F),
    Empty,
}

fn apply_wrapped<F: Fn<(Boxed,), u256>>(_ wrapped: own Wrapped<F>) -> u256 {
    match wrapped {
        Wrapped::Function(function) => function.call(Boxed { value: 42 }),
        Wrapped::Empty => 0,
    }
}

fn probe() -> u256 {
    apply_wrapped(Wrapped::Function(|value: own| value.value))
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn contextual_closure_inference_flows_through_block_arguments() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_block_context.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

fn apply<F: Fn<(Boxed,), u256>>(_ function: F) -> u256 {
    function.call(Boxed { value: 42 })
}

fn probe() -> u256 {
    apply({
        |value: own| value.value
    })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn inferred_mut_closure_parameter_fields_are_assignable_after_call_resolution() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_inferred_mut_field_assignment.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let mut boxed = Boxed { value: 0 }
    let set = |target: mut| {
        target.value = 42
    }
    set.call(mut boxed)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn inferred_owned_closure_parameter_fields_can_be_borrowed_after_call_resolution() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_inferred_owned_field_borrow.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let borrow = |boxed: own| -> ref u256 { ref boxed.value }
    let returned = borrow.call_once(Boxed { value: 42 })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn deferred_closure_place_checks_use_resolved_capability_mutability() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_deferred_place_capabilities.fe"),
        r#"
struct Boxed {
    value: u256,
}

struct MutHolder {
    value: mut u256,
}

fn probe() {
    let mut boxed = Boxed { value: 0 }
    let borrow = |target: mut| -> mut u256 { mut target.value }
    let returned = borrow.call(mut boxed)
    returned = 42

    let mut value = 0
    let holder = MutHolder { value: mut value }
    let set = |target| {
        target.value = 42
    }
    set.call(holder)

    let mut array_value = 0
    let handles: [mut u256; 1] = [mut array_value]
    let set_index = |items| {
        items[0] = 42
    }
    set_index.call(handles)

    let mut borrowed_value = 0
    let borrowed_handles: [mut u256; 1] = [mut borrowed_value]
    let borrow_index = |items| -> mut u256 { mut items[0] }
    let returned = borrow_index.call(borrowed_handles)
    returned = 42

    let mut incremented_value = 41
    let incremented_handles: [mut u256; 1] = [mut incremented_value]
    let increment_index = |items| {
        items[0] += 1
    }
    increment_index.call(incremented_handles)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_deferred_immutable_places.fe"),
        r#"
struct Boxed {
    value: u256,
}

struct RefHolder {
    value: ref u256,
}

fn probe() {
    let set_owned = |target: own| {
        target.value = 42
    }
    set_owned.call_once(Boxed { value: 0 })

    let value = 0
    let mut holder = RefHolder { value: ref value }
    let set_ref = |target: mut| {
        target.value = 42
    }
    set_ref.call(mut holder)

    let array_value = 0
    let mut handles: [ref u256; 1] = [ref array_value]
    let set_index_ref = |items: mut| {
        items[0] = 42
    }
    set_index_ref.call(mut handles)

    let borrowed_array_value = 0
    let mut borrowed_handles: [ref u256; 1] = [ref borrowed_array_value]
    let borrow_index_ref = |items: mut| -> mut u256 { mut items[0] }
    let returned = borrow_index_ref.call(mut borrowed_handles)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert_eq!(
        rendered
            .matches("left-hand side of assignment is immutable")
            .count(),
        3,
        "{rendered}"
    );
    assert!(
        rendered.contains("mutable borrow requires a mutable place"),
        "{rendered}"
    );
}

#[test]
fn inferred_closure_results_copy_from_borrow_capabilities() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_copy_borrow_results.fe"),
        r#"
enum Maybe {
    None,
    Some(u256),
}

fn probe() -> u256 {
    let from_pattern = |input| match input {
        Maybe::None => 0
        Maybe::Some(value) => value
    }
    let from_reversed_pattern = |input| match input {
        Maybe::Some(value) => value
        Maybe::None => 0
    }
    let from_ref = |value: ref u256| if true { 0 } else { value }
    let from_reversed_ref = |value: ref u256| if true { value } else { 0 }
    let from_mut = |value: mut u256| if true { 0 } else { value }
    let mut mutable: u256 = 0
    from_pattern.call(Maybe::Some(40))
        + from_reversed_pattern.call(Maybe::Some(0))
        + from_ref.call(ref mutable)
        + from_reversed_ref.call(ref mutable)
        + from_mut.call(mut mutable)
        + 2
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn unconstrained_fn_carriers_infer_closure_parameter_capabilities() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_generic_carrier_inference.fe"),
        r#"
use core::functional::Fn

fn accept<A, R, F: Fn<(A,), R>>(_ function: F) {}
fn accept_pair<A, B, R, F: Fn<(A, B), R>>(_ function: F) {}
fn read_ref(_ value: ref u256) -> u256 { value }
fn read_mut(_ value: mut u256) -> u256 { value }
fn read_view(_ value: view u256) -> u256 { value }
fn take(_ value: own u256) -> u256 { value }
fn sum(_ left: ref u256, _ right: mut u256) -> u256 { left + right }

fn probe() {
    accept(|value: ref u256| -> u256 { value })
    accept(|value: mut u256| -> u256 { value })
    accept(|value: view u256| -> u256 { value })
    accept(|value: ref| -> u256 { read_ref(value) })
    accept(|value: mut| -> u256 { read_mut(value) })
    accept(|value: view| -> u256 { read_view(value) })
    accept(|value| -> u256 { read_view(value) })
    accept(|value: own| -> u256 { take(value) })
    accept_pair(|left: ref, right: mut| -> u256 { sum(left, right) })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn closure_parameter_layout_holes_are_inferred_monomorphically() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_layout_hole_parameter.fe"),
        r#"
use core::functional::Fn

struct Rooted<const ROOT: u256 = _> {
    value: u256,
}

struct Pair<const ROOT: u256 = _> {
    left: Rooted<ROOT>,
    right: Rooted<ROOT>,
}

struct Independent<const LEFT: u256 = _, const RIGHT: u256 = _> {
    left: Rooted<LEFT>,
    right: Rooted<RIGHT>,
}

fn apply_concrete<F: Fn<(view Rooted<9>,), u256>>(_ function: F) -> u256 {
    function.call(Rooted<9> { value: 8 })
}

fn probe() -> u256 {
    let read = |value: Rooted| -> u256 { value.value }
    let read_explicit = |value: Rooted<_>| -> u256 { value.value }
    let read_ref = |value: ref Rooted| -> u256 { value.value }
    let read_mut = |value: mut Rooted| -> u256 { value.value }
    let consume = |value: own Rooted| value
    let sum = |pair: Pair| -> u256 { pair.left.value + pair.right.value }
    let sum_independent =
        |pair: Independent| -> u256 { pair.left.value + pair.right.value }
    let sum_separate =
        |left: Rooted, right: Rooted| -> u256 { left.value + right.value }
    let rooted = Rooted<5> { value: 4 }
    let mut mutable = Rooted<6> { value: 5 }
    read.call(Rooted<7> { value: 2 })
        + read_explicit.call(Rooted<4> { value: 3 })
        + read_ref.call(ref rooted)
        + read_mut.call(mut mutable)
        + consume.call_once(Rooted<10> { value: 9 }).value
        + sum.call(Pair<8> {
            left: Rooted<8> { value: 20 },
            right: Rooted<8> { value: 20 },
        })
        + sum_independent.call(Independent<11, 12> {
            left: Rooted<11> { value: 6 },
            right: Rooted<12> { value: 7 },
        })
        + sum_separate.call(
            Rooted<13> { value: 6 },
            Rooted<14> { value: 7 },
        )
        + apply_concrete(|value: Rooted| value.value)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_layout_hole_monomorphism.fe"),
        r#"
struct Rooted<const ROOT: u256 = _> {
    value: u256,
}

fn probe() {
    let read = |value: Rooted| -> u256 { value.value }
    read.call(Rooted<7> { value: 1 })
    read.call(Rooted<8> { value: 2 })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(!rendered.contains("inferred const generic"), "{rendered}");
    assert!(rendered.contains("type mismatch"), "{rendered}");

    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_layout_hole_unresolved.fe"),
        r#"
struct Rooted<const ROOT: u256 = _> {
    value: u256,
}

fn probe() {
    let read = |value: Rooted| value.value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(rendered.contains("type annotation is needed"), "{rendered}");

    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_layout_hole_return.fe"),
        r#"
struct Rooted<const ROOT: u256 = _> {
    value: u256,
}

fn probe() {
    let make = || -> Rooted { Rooted<7> { value: 42 } }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(rendered.contains("inferred const generic"), "{rendered}");
}

#[test]
fn inferred_closure_parameter_types_are_monomorphic_across_calls() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_payload_monomorphism.fe"),
        r#"
fn probe() {
    let identity = |value| value
    identity.call(42 as u256)
    identity.call(true)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(rendered.contains("type mismatch"), "{rendered}");
    assert!(rendered.contains("bool"), "{rendered}");
    assert!(rendered.contains("u256"), "{rendered}");
}

#[test]
fn closure_return_type_is_inferred_from_only_explicit_returns() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_explicit_return_inference.fe"),
        r#"
fn probe() -> u256 {
    let choose = |flag| {
        if flag {
            return 20 as u256
        }
        return 22 as u256
    }
    choose.call(true)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "probe");
    let (_, typed_body) = check_func_body(&db, func);
    let closure = typed_body
        .closure_infos()
        .next()
        .expect("missing closure")
        .1;
    assert_eq!(closure.ty.ret_ty(&db), TyId::u256(&db));
}

#[test]
fn closure_call_arity_diagnostics_count_only_explicit_arguments() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_call_arity.fe"),
        r#"
fn probe() {
    let zero = || 42 as u256
    zero.call(())

    let one = |_ unit: own ()| 42 as u256
    one.call()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("expected 0 arguments, but 1 given"),
        "{rendered}"
    );
    assert!(
        rendered.contains("expected 1 arguments, but 0 given"),
        "{rendered}"
    );
}

#[test]
fn consuming_closure_call_method_does_not_fall_back_to_extension_trait() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("consuming_closure_call_extension_fallback.fe"),
        r#"
trait Hijack {
    fn call(self) -> u256
    fn call_once(self) -> u256
}

impl<T> Hijack for T {
    fn call(self) -> u256 {
        7
    }

    fn call_once(self) -> u256 {
        8
    }
}

struct Boxed {
    value: u256,
}

fn method_syntax_is_reserved() -> u256 {
    let boxed = Boxed { value: 42 }
    let take = || boxed
    take.call()
}

fn explicit_ufcs_remains_available() -> u256 {
    let boxed = Boxed { value: 42 }
    let take = || boxed
    Hijack::call(take)
}

fn borrowed_consuming_intrinsic_is_reserved() -> u256 {
    let boxed = Boxed { value: 42 }
    let take = || boxed
    let borrowed = ref take
    borrowed.call_once()
}

fn explicit_borrowed_ufcs_remains_available() -> u256 {
    let boxed = Boxed { value: 42 }
    let take = || boxed
    let borrowed = ref take
    Hijack::call_once(borrowed)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert_eq!(
        rendered.matches("method not found").count(),
        1,
        "{rendered}"
    );
    assert!(rendered.contains("no method named `call`"), "{rendered}");
    assert!(
        rendered.contains("expected `u256`, but `Boxed` is given"),
        "{rendered}"
    );
    assert!(
        rendered.contains("`own` argument requires an owned movable value"),
        "{rendered}"
    );
}

#[test]
fn deferred_replay_flows_through_trait_qualified_wrapper_calls() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_deferred_trait_qualified_wrapper.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}

struct Builder {}

trait Wrap {
    fn wrap<T>(self, _ function: own T) -> Wrapped<T>
}

impl Wrap for Builder {
    fn wrap<T>(self, _ function: own T) -> Wrapped<T> {
        Wrapped { function }
    }
}

struct Apply {}

impl Apply {
    fn read_wrapped<F: Fn<(), view Boxed>>(self, _ wrapped: own Wrapped<F>) -> u256 {
        wrapped.function.call().value
    }
}

fn probe() -> u256 {
    let outer = |apply: own| {
        let boxed = Boxed { value: 42 }
        apply.read_wrapped(Wrap::wrap(Builder {}, || boxed))
    }
    outer.call_once(Apply {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn deferred_replay_preserves_incompatible_concrete_result_components() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_deferred_concrete_result_component.fe"),
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}

fn wrap_with_tag<F>(_ function: own F) -> (u256, Wrapped<F>) {
    (0, Wrapped { function })
}

struct Apply {}

impl Apply {
    fn read_tagged<F: Fn<(), view Boxed>>(
        self,
        _ wrapped: own (bool, Wrapped<F>),
    ) -> u256 {
        wrapped.1.function.call().value
    }
}

fn probe() -> u256 {
    let outer = |apply: own| {
        let boxed = Boxed { value: 42 }
        apply.read_tagged(wrap_with_tag(|| boxed))
    }
    outer.call_once(Apply {})
}

fn probe_tuple() -> u256 {
    let outer = |apply: own| {
        let boxed = Boxed { value: 42 }
        apply.read_tagged((0 as u256, Wrapped { function: || boxed }))
    }
    outer.call_once(Apply {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(rendered.matches("type mismatch").count() >= 2, "{rendered}");
    assert!(rendered.contains("bool"), "{rendered}");
    assert!(rendered.contains("u256"), "{rendered}");
}

#[test]
fn duplicate_closure_parameter_names_are_rejected() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("duplicate_closure_parameter.fe"),
        r#"
fn probe() {
    let choose = |value: own u256, value: own bool| value
    let result: bool = choose.call_once(1, true)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("duplicate closure parameter `value`"),
        "{rendered}"
    );
}

#[test]
fn closure_parameter_annotations_enforce_function_ownership_invariants() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("invalid_closure_parameter_ownership.fe"),
        r#"
fn probe() {
    let invalid = |item: own ref u256| item
    let invalid_mut = |mut item| item
    let bad_param = |item: MissingParam| item
    let bad_return = || -> MissingReturn { 1 }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(rendered.contains("invalid `own` parameter"), "{rendered}");
    assert!(
        rendered.contains("`own` parameters must have owned types"),
        "{rendered}"
    );
    assert!(
        rendered.contains("invalid `mut` parameter syntax"),
        "{rendered}"
    );
    assert!(
        rendered.contains("`MissingParam` is not found"),
        "{rendered}"
    );
    assert!(
        rendered.contains("`MissingReturn` is not found"),
        "{rendered}"
    );
}

#[test]
fn concrete_closure_callees_do_not_inherit_unrelated_dispatch_witnesses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_callee_impl_env.fe"),
        r#"
fn sink(_ value: own u256) {}

fn invoke() {
    let first = |value: own| sink(value)
    let second = |value: own| sink(value)
    first.call(1)
    second.call(2)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let invoke = find_func(&db, top_mod, "invoke");
    let invoke_instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(invoke)),
    );
    let closure_keys = invoke_instance
        .callees(&db)
        .iter()
        .filter(|callee| matches!(callee.key.owner(&db), BodyOwner::Closure { .. }))
        .map(|callee| callee.key)
        .collect::<Vec<_>>();
    assert_eq!(closure_keys.len(), 2);

    let sink_keys = closure_keys
        .into_iter()
        .map(|key| {
            get_or_build_semantic_instance(&db, key)
                .callees(&db)
                .iter()
                .find_map(|callee| {
                    let BodyOwner::Func(func) = callee.key.owner(&db) else {
                        return None;
                    };
                    func.name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "sink")
                        .then_some(callee.key)
                })
                .expect("missing closure call to sink")
        })
        .collect::<Vec<_>>();
    assert_eq!(
        sink_keys[0], sink_keys[1],
        "a concrete callee key must not depend on which closure dispatch witness reached it"
    );
    let env = sink_keys[0].impl_env(&db);
    assert!(env.assumptions(&db).is_empty(&db));
    assert!(env.witnesses(&db).is_empty());
}

#[test]
fn schematic_default_method_callees_retain_their_proof_environment() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("schematic_default_method_impl_env.fe"),
        r#"
trait Probe {
    fn probe(self) {}
}

fn generic<T: Probe>(_ value: own T) {
    value.probe()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let generic = find_func(&db, top_mod, "generic");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(generic)),
    );
    let callee = instance
        .callees(&db)
        .iter()
        .find(|callee| {
            matches!(
                callee.key.owner(&db),
                BodyOwner::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "probe")
            )
        })
        .expect("missing generic default-method callee");
    let env = callee.key.impl_env(&db);
    assert!(!env.assumptions(&db).is_empty(&db));
    assert!(!env.witnesses(&db).is_empty());
}

#[test]
fn effect_specialized_closure_retains_instance_capture_metadata() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("effect_specialized_closure.fe"),
        r#"
use core::effect_ref::MemPtr
use core::functional::Fn

struct Slot<const ROOT: u256 = _> {}

fn capture_target() uses (target: Slot) {
    let handle = ref target
    let f = |_ unit: own ()| -> () {
        let captured = handle
    }
    f.call(())
}

pub fn invoke() {
    let provider: MemPtr<Slot<7>> = MemPtr::from_raw(0)
    with (provider) {
        capture_target()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let invoke = find_func(&db, top_mod, "invoke");
    let invoke_instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(invoke)),
    );
    let capture_key = invoke_instance
        .body(&db)
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|stmt| {
            let SStmtKind::Assign {
                expr: SExpr::Call { callee, .. },
                ..
            } = &stmt.kind
            else {
                return None;
            };
            let BodyOwner::Func(func) = callee.key.owner(&db) else {
                return None;
            };
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "capture_target")
                .then_some(callee.key)
        })
        .expect("missing specialized capture_target call");
    let capture_instance = get_or_build_semantic_instance(&db, capture_key);
    let closure_key = capture_instance
        .body(&db)
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|stmt| {
            let SStmtKind::Assign {
                expr: SExpr::Call { callee, .. },
                ..
            } = &stmt.kind
            else {
                return None;
            };
            matches!(callee.key.owner(&db), BodyOwner::Closure { .. }).then_some(callee.key)
        })
        .expect("missing specialized closure call");
    let BodyOwner::Closure { ty: closure, .. } = closure_key.owner(&db) else {
        unreachable!()
    };
    let callable = closure_key.callable_body(&db);
    let plan = callable
        .closure_capture_plan(&db, closure)
        .expect("specialized closure must retain its capture plan");
    assert_eq!(
        plan.iter()
            .map(|capture| capture.ty)
            .collect::<Vec<_>>()
            .as_slice(),
        closure.captures(&db),
    );
}

#[test]
fn typed_closure_descriptor_preserves_param_and_capture_field_order() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_descriptor_field_order.fe"),
        r#"
struct Left {
    value: u256,
}

struct Right {
    value: u256,
}

fn make(_ first: own Left, _ second: own Right) {
    let ordered = |alpha: own u256, beta: own u256| -> u256 {
        second.value + first.value + alpha + beta
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, func);
    let (_, info) = typed_body
        .closure_infos()
        .next()
        .expect("missing closure metadata");
    let callable = TypedCallableBody::new(BodyOwner::closure(&db, info.ty), typed_body);
    let (closure_body, receiver_mode) = callable
        .owner_closure_body(&db)
        .expect("closure metadata must form a coherent descriptor");

    assert_eq!(closure_body.ty(), info.ty);
    assert_eq!(
        closure_body.param_bindings().len(),
        info.ty.params(&db).len()
    );
    for (idx, binding) in closure_body.param_bindings().iter().enumerate() {
        assert!(matches!(
            binding,
            LocalBinding::Param {
                site: ParamSite::Closure(def),
                idx: binding_idx,
                ..
            } if *def == info.def && *binding_idx == idx
        ));
    }

    let captures = closure_body.captures(&db).collect::<Vec<_>>();
    assert_eq!(captures.len(), info.ty.captures(&db).len());
    assert_eq!(
        captures
            .iter()
            .map(|capture| capture.binding)
            .collect::<Vec<_>>(),
        info.captures
            .iter()
            .map(|capture| capture.binding)
            .collect::<Vec<_>>(),
    );
    assert_eq!(
        captures
            .iter()
            .map(|capture| capture.ty)
            .collect::<Vec<_>>()
            .as_slice(),
        info.ty.captures(&db),
    );
    assert_eq!(
        captures
            .iter()
            .map(|capture| capture.access)
            .collect::<Vec<_>>()
            .as_slice(),
        info.ty.capture_accesses(&db),
    );

    let physical_params = closure_body.physical_param_bindings(&db, receiver_mode);
    assert!(matches!(
        physical_params,
        [
            LocalBinding::Param {
                site: ParamSite::ClosureEnv(env_def),
                ..
            },
            LocalBinding::Param {
                site: ParamSite::ClosureArgs(args_def),
                ..
            }
        ] if env_def == info.def && args_def == info.def
    ));

    let closure = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::closure(&db, info.ty)),
    );
    let semantic_body = closure.body(&db);
    for (expected_field, binding) in closure_body.param_bindings().iter().copied().enumerate() {
        let local = semantic_body
            .locals
            .iter()
            .enumerate()
            .find_map(|(idx, local)| {
                (local.source == Some(binding))
                    .then_some(fe_hir::analysis::semantic::SLocalId::from_u32(idx as u32))
            })
            .expect("logical closure parameter local");
        assert!(semantic_body.blocks.iter().any(|block| {
            block.stmts.iter().any(|stmt| {
                matches!(
                    &stmt.kind,
                    SStmtKind::Assign {
                        dst,
                        expr: SExpr::Field { field, .. },
                    } if *dst == local && usize::from(field.0) == expected_field
                )
            })
        }));
    }
}

#[test]
fn dynamic_return_indices_retain_callable_input_provenance() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("dynamic_return_index.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn indexed(_ values: own [Boxed; 2], _ index: own usize) -> Boxed {
    let alias = index
    values[alias]
}

fn mutable_index(_ values: own [Boxed; 2], mut index: own usize) -> Boxed {
    index = 1
    values[index]
}

fn branch_index(
    _ values: own [Boxed; 2],
    _ indices: own [usize; 2],
    _ lane: own usize,
    _ left: own bool,
) -> Boxed {
    if left { values[indices[lane]] } else { values[0] }
}

fn set_index(_ index: mut usize) {
    index = 1
}

fn make(_ values: own [Boxed; 2], _ index: own usize) {
    let from_capture = |_ unit: own ()| -> Boxed { values[index] }
}

fn make_mut(_ values: own [Boxed; 2], _ index: mut usize) {
    let from_mut_capture = |_ unit: own ()| -> Boxed {
        set_index(index)
        values[index]
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let indexed = find_func(&db, top_mod, "indexed");
    let (_, indexed_body) = check_func_body(&db, indexed);
    assert_eq!(
        TypedCallableBody::new(BodyOwner::Func(indexed), indexed_body).return_provenance(&db),
        ReturnProvenance::Forwarded(vec![source(
            CallableInputLayoutHoleOrigin::ValueParam(0),
            vec![ReturnProjectionStep::DynamicIndex(ReturnIndexSource {
                origin: CallableInputLayoutHoleOrigin::ValueParam(1),
                projection: Vec::new(),
            })],
        )])
    );
    for name in ["mutable_index", "branch_index"] {
        let func = find_func(&db, top_mod, name);
        let (_, typed_body) = check_func_body(&db, func);
        let callable = TypedCallableBody::new(BodyOwner::Func(func), typed_body);
        assert_eq!(
            callable.return_provenance(&db),
            ReturnProvenance::Fresh,
            "{name} must transport the callee's actual carrier instead of replaying an unstable or branch-only index",
        );
        let expected = match name {
            "mutable_index" => vec![source(
                CallableInputLayoutHoleOrigin::ValueParam(0),
                vec![ReturnProjectionStep::AnyIndex],
            )],
            "branch_index" => vec![
                source(
                    CallableInputLayoutHoleOrigin::ValueParam(0),
                    vec![ReturnProjectionStep::ConstantIndex(0)],
                ),
                source(
                    CallableInputLayoutHoleOrigin::ValueParam(0),
                    vec![ReturnProjectionStep::AnyIndex],
                ),
            ],
            _ => unreachable!(),
        };
        assert_eq!(
            callable.forwarded_return_sources(&db),
            expected,
            "{name} should retain conservative may-provenance without changing its exact classification",
        );
    }

    let make = find_func(&db, top_mod, "make");
    let (_, make_body) = check_func_body(&db, make);
    let (_, closure) = make_body
        .closure_infos()
        .next()
        .expect("missing captured-index closure");
    let callable = TypedCallableBody::new(BodyOwner::closure(&db, closure.ty), make_body);
    assert_eq!(
        callable.return_provenance(&db),
        ReturnProvenance::Forwarded(vec![source(
            CallableInputLayoutHoleOrigin::Receiver,
            vec![
                ReturnProjectionStep::Field(0),
                ReturnProjectionStep::DynamicIndex(ReturnIndexSource {
                    origin: CallableInputLayoutHoleOrigin::Receiver,
                    projection: vec![ReturnProjectionStep::Field(1)],
                }),
            ],
        )])
    );

    let make_mut = find_func(&db, top_mod, "make_mut");
    let (_, make_mut_body) = check_func_body(&db, make_mut);
    let (_, closure) = make_mut_body
        .closure_infos()
        .next()
        .expect("missing mutable-index capture closure");
    let callable = TypedCallableBody::new(BodyOwner::closure(&db, closure.ty), make_mut_body);
    assert_eq!(
        callable.return_provenance(&db),
        ReturnProvenance::Fresh,
        "a mutable captured index can change in the callee and must not be replayed from its pre-call value",
    );
    assert_eq!(
        callable.forwarded_return_sources(&db),
        vec![source(
            CallableInputLayoutHoleOrigin::Receiver,
            vec![
                ReturnProjectionStep::Field(1),
                ReturnProjectionStep::AnyIndex,
            ],
        )],
        "may-provenance should conservatively retain the captured array source",
    );
}

#[test]
fn mutable_bindings_are_not_forwarded_as_callable_return_provenance() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("mutable_return_source.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn reassigned_param(mut value: own Boxed, _ replacement: own Boxed) -> Boxed {
    value = replacement
    value
}

fn reassigned_local(_ left: own Boxed, _ right: own Boxed) -> Boxed {
    let mut value = left
    value = right
    value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    for name in ["reassigned_param", "reassigned_local"] {
        let func = find_func(&db, top_mod, name);
        let (_, typed_body) = check_func_body(&db, func);
        let callable = TypedCallableBody::new(BodyOwner::Func(func), typed_body);
        assert_eq!(
            callable.return_provenance(&db),
            ReturnProvenance::Fresh,
            "{name} must return its actual carrier instead of replaying a declaration-time source",
        );
        assert_eq!(
            callable.forwarded_return_sources(&db),
            vec![
                source(CallableInputLayoutHoleOrigin::ValueParam(0), Vec::new()),
                source(CallableInputLayoutHoleOrigin::ValueParam(1), Vec::new()),
            ],
            "{name} should retain every conservative may-source",
        );
    }
}

#[test]
fn closure_call_materializes_captured_return_index_before_consuming_environment() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("captured_return_index_call.fe"),
        r#"
struct Boxed {
    value: u256,
}

fn invoke(_ values: own [Boxed; 2], _ index: own usize) -> Boxed {
    let get = |_ unit: own ()| -> Boxed { values[index] }
    get.call_once(())
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let invoke = find_func(&db, top_mod, "invoke");
    let semantic = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(invoke)),
    );
    let body = semantic.body(&db);
    let (call_result, index_local) = body
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|stmt| {
            let SStmtKind::Assign {
                expr:
                    SExpr::Call {
                        callee,
                        return_sources,
                        ..
                    },
                dst,
            } = &stmt.kind
            else {
                return None;
            };
            if !matches!(callee.key.owner(&db), BodyOwner::Closure { .. }) {
                return None;
            }
            let [source] = return_sources.as_ref() else {
                panic!("closure call should have one forwarded return source");
            };
            let [
                SCallReturnProjectionStep::Field(0),
                SCallReturnProjectionStep::DynamicIndex(index),
            ] = source.projection.as_slice()
            else {
                panic!("unexpected closure call return source: {source:?}");
            };
            Some((*dst, *index))
        })
        .expect("missing closure call return-index materialization");

    let index_source = body
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|stmt| {
            let SStmtKind::Assign {
                dst,
                expr: SExpr::ReadPlace { place, .. },
            } = &stmt.kind
            else {
                return None;
            };
            (*dst == index_local).then_some(place)
        })
        .expect("captured return index should be read into a call-site local");
    let mut path = index_source.path.iter();
    assert!(matches!(path.next(), Some(Projection::Field(1))));
    assert!(path.next().is_none());
    let normalized = normalize_semantic_body(&db, semantic)
        .expect("captured-index closure call should normalize");
    let facts = NormalizedBodyFacts::new(&normalized);
    assert!(
        facts
            .local_layout_source_uses(call_result)
            .contains(&index_local),
        "call-result layout metadata must retain the materialized index as a layout source",
    );
    assert!(
        !facts
            .local_value_source_uses(call_result)
            .contains(&index_local),
        "a layout-only index must not become a runtime value-source fallback",
    );
    assert!(
        facts
            .local_dependency_uses(call_result)
            .contains(&index_local),
        "return slicing and carrier invalidation must retain layout-only dependencies",
    );
}

#[test]
fn closure_loop_control_cannot_target_an_enclosing_loop() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_loop_control_boundary.fe"),
        r#"
fn probe() {
    while true {
        let bad_break = || { break }
        let bad_continue = || { continue }
        let valid = || {
            while true {
                break
            }
        }
        break
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert_eq!(
        rendered.matches("is not allowed outside of a loop").count(),
        2,
        "{rendered}"
    );
}

#[test]
fn contextual_closure_replay_covers_deferred_projections_aliases_and_casts() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_deferred_replay_matrix.fe"),
        r#"
use core::functional::Fn
use core::ops::Neg

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}

impl<F> Neg for Wrapped<F> {
    type Output = F

    fn neg(own self) -> F {
        self.function
    }
}

fn apply_boxed<F: Fn<(), view Boxed>>(_ function: F) {}
fn apply_number<F: Fn<(view u256,), u256>>(_ function: F) {}
fn apply_array<F: Fn<(view [u256; 2],), u256>>(_ function: F) {}

fn pending_field() {
    let boxed = Boxed { value: 1 }
    let outer = |wrapped: own| {
        apply_boxed(wrapped.function)
    }
    outer.call_once(Wrapped { function: || boxed })
}

fn pending_index() {
    let boxed = Boxed { value: 2 }
    let outer = |functions: own| {
        apply_boxed(functions[0])
    }
    outer.call_once([|| boxed])
}

fn pending_operator() {
    let boxed = Boxed { value: 3 }
    let outer = |wrapped: own| {
        apply_boxed(-wrapped)
    }
    outer.call_once(Wrapped { function: || boxed })
}

fn direct_alias() {
    let boxed = Boxed { value: 4 }
    let function = || boxed
    apply_boxed(function)
}

fn projected_alias() {
    let boxed = Boxed { value: 5 }
    let function = || boxed
    apply_boxed((function,).0)
}

fn stable_mutable_alias() {
    let boxed = Boxed { value: 6 }
    let mut function = || boxed
    apply_boxed(function)
}

fn stable_mutable_projection() {
    let boxed = Boxed { value: 7 }
    let mut function = || boxed
    apply_boxed((function,).0)
}

fn self_assigned_mutable_alias() {
    let boxed = Boxed { value: 8 }
    let mut function = || boxed
    function = function
    apply_boxed(function)
}

struct Apply {}

impl Apply {
    fn apply_boxed<F: Fn<(), view Boxed>>(self, _ function: F) {}
    fn apply_number<F: Fn<(view u256,), u256>>(self, _ function: F) {}
    fn apply_array<F: Fn<(view [u256; 2],), u256>>(self, _ function: F) {}
}

fn deferred_alias() {
    let outer = |apply: own| {
        let boxed = Boxed { value: 9 }
        let function = || boxed
        apply.apply_boxed(function)
    }
    outer.call_once(Apply {})
}

fn direct_cast() {
    apply_number(|value| value as u256)
}

fn deferred_cast() {
    let outer = |apply: own| {
        apply.apply_number(|value| value as u256)
    }
    outer.call_once(Apply {})
}

fn direct_for_loop() {
    apply_array(|values| {
        for value in values {
            let item = value
        }
        0
    })
}

fn deferred_for_loop() {
    let outer = |apply: own| {
        apply.apply_array(|values| {
            for value in values {
                let item = value
            }
            0
        })
    }
    outer.call_once(Apply {})
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    for func_name in [
        "pending_field",
        "pending_index",
        "pending_operator",
        "direct_alias",
        "projected_alias",
        "stable_mutable_alias",
        "stable_mutable_projection",
        "self_assigned_mutable_alias",
        "deferred_alias",
        "direct_cast",
        "deferred_cast",
        "direct_for_loop",
        "deferred_for_loop",
    ] {
        let func = find_func(&db, top_mod, func_name);
        let (_, typed_body) = check_func_body(&db, func);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
        normalize_semantic_body(&db, instance)
            .unwrap_or_else(|_| panic!("failed to normalize `{func_name}`"));
        for (_, info) in typed_body.closure_infos() {
            let closure = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::closure(&db, info.ty)),
            );
            normalize_semantic_body(&db, closure).unwrap_or_else(|_| {
                panic!(
                    "failed to normalize closure {:?} in `{func_name}`",
                    info.def
                )
            });
        }
    }
}

#[test]
fn contextual_closure_alias_replay_rejects_distinct_nominals_and_conflicting_contexts() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_alias_replay_negative.fe"),
        r#"
use core::functional::{Fn, FnOnce}

struct Boxed {
    value: u256,
}

fn read<F: Fn<(), view Boxed>>(_ function: F) {}
fn consume<F: FnOnce<(), Boxed>>(_ function: F) {}

fn reassigned_alias() {
    let boxed = Boxed { value: 1 }
    let replacement = Boxed { value: 2 }
    let mut function = || boxed
    function = || replacement
    read(function)
}

fn branch_reassigned_alias(flag: bool) {
    let boxed = Boxed { value: 3 }
    let replacement = Boxed { value: 4 }
    let mut function = || boxed
    if flag {
        function = || replacement
    }
    read((function,).0)
}

fn two_call_param_origins_are_isolated() {
    let left = Boxed { value: 5 }
    let right = Boxed { value: 6 }
    let outer = |function: own| {
        read(function)
    }
    outer.call(|| left)
    outer.call(|| right)
}

fn conflicting_contexts() {
    let boxed = Boxed { value: 7 }
    let function = || boxed
    read(function)
    consume(function)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert_eq!(
        rendered.matches("type mismatch").count(),
        3,
        "distinct assignment and call-site closure literals must retain nominal type identity: {rendered}",
    );
    assert!(
        rendered.contains("trait bound is not satisfied"),
        "conflicting callable contexts must preserve the concrete bound failure: {rendered}",
    );
    assert!(
        !rendered.contains("type annotation is needed"),
        "concrete alias failures must retain their real callable-bound diagnostic: {rendered}",
    );
}

#[test]
fn contextual_method_candidate_constraints_are_order_independent() {
    const PREFIX: &str = r#"
use core::functional::{Fn, FnOnce}

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}
"#;
    const REUSABLE: &str = r#"
trait ReusableExtract {
    type Output
    fn extract(own self) -> Self::Output
}

impl<F: Fn<(), view Boxed>> ReusableExtract for Wrapped<F> {
    type Output = F
    fn extract(own self) -> F { self.function }
}
"#;
    const CONSUMING: &str = r#"
trait ConsumingExtract {
    type Output
    fn extract(own self) -> Self::Output
}

impl<F: FnOnce<(), Boxed>> ConsumingExtract for Wrapped<F> {
    type Output = F
    fn extract(own self) -> F { self.function }
}
"#;
    const SUFFIX: &str = r#"
fn consume<F: FnOnce<(), Boxed>>(_ function: F) {}

fn probe() {
    let boxed = Boxed { value: 42 }
    consume(Wrapped { function: || boxed }.extract())
}
"#;

    for (file_name, first, second) in [
        (
            "closure_method_candidate_reusable_first.fe",
            REUSABLE,
            CONSUMING,
        ),
        (
            "closure_method_candidate_consuming_first.fe",
            CONSUMING,
            REUSABLE,
        ),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let source = [PREFIX, first, second, SUFFIX].concat();
        let file = db.new_stand_alone(Utf8PathBuf::from(file_name), &source);
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let probe = find_func(&db, top_mod, "probe");
        let (_, typed_body) = check_func_body(&db, probe);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(probe)),
        );
        normalize_semantic_body(&db, instance)
            .unwrap_or_else(|_| panic!("failed to normalize `{file_name}`"));
        for (_, info) in typed_body.closure_infos() {
            let closure = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::closure(&db, info.ty)),
            );
            normalize_semantic_body(&db, closure)
                .unwrap_or_else(|_| panic!("failed to normalize closure in `{file_name}`"));
        }
    }
}

#[test]
fn contextual_method_candidates_report_true_ambiguity_and_concrete_unsat() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_method_candidate_diagnostics.fe"),
        r#"
use core::functional::{Fn, FnOnce}

struct Boxed {
    value: u256,
}

struct Wrapped<F> {
    function: F,
}

struct UnsatWrapped<F> {
    function: F,
}

trait ReusableExtract {
    type Output
    fn extract(own self) -> Self::Output
}

impl<F: Fn<(), view Boxed>> ReusableExtract for Wrapped<F> {
    type Output = F
    fn extract(own self) -> F { self.function }
}

impl<F: Fn<(), view Boxed>> ReusableExtract for UnsatWrapped<F> {
    type Output = F
    fn extract(own self) -> F { self.function }
}

trait OnceExtract {
    type Output
    fn extract(own self) -> Self::Output
}

impl<F: FnOnce<(), view Boxed>> OnceExtract for Wrapped<F> {
    type Output = F
    fn extract(own self) -> F { self.function }
}

trait NumberExtract {
    fn extract(own self) -> u256
}

impl<F> NumberExtract for Wrapped<F> {
    fn extract(own self) -> u256 { 0 }
}

impl<F> NumberExtract for UnsatWrapped<F> {
    fn extract(own self) -> u256 { 0 }
}

fn read<F: Fn<(), view Boxed>>(_ function: F) {}
fn consume<F: FnOnce<(), Boxed>>(_ function: F) {}

fn truly_ambiguous() {
    let boxed = Boxed { value: 1 }
    read(Wrapped { function: || boxed }.extract())
}

fn concrete_unsat() {
    let boxed = Boxed { value: 2 }
    consume(UnsatWrapped { function: || boxed }.extract())
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("multiple trait candidates found"),
        "both reusable candidates must remain ambiguous: {rendered}",
    );
    assert!(
        rendered.contains("trait bound is not satisfied")
            && rendered.contains("`UnsatWrapped<|| -> Boxed>` doesn't implement `ReusableExtract")
            && rendered.contains("trait bound `|| -> Boxed: Fn"),
        "a concrete zero-viable candidate set must preserve its callable-bound error: {rendered}",
    );
    assert!(
        !rendered.contains("`u256` doesn't implement `FnOnce"),
        "candidate diagnostics must come from the incompatible closure-bound method, not the unconstrained numeric fallback: {rendered}",
    );
    assert!(
        !rendered.contains("type must be known"),
        "a concrete candidate failure must not be reported as an unknown receiver: {rendered}",
    );
}
