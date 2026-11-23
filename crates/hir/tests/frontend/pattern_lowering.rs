use cranelift_entity::EntityRef;
use fe_hir::{
    analysis::{
        semantic::{
            SExpr, SStmtKind, STerminatorKind, SemanticInstance, access::check_semantic_accesses,
            identity_semantic_instance_key, normalized::normalize_semantic_body,
            semantic_may_return,
        },
        ty::ty_check::{BodyOwner, check_contract_recv_arm_body, check_func_body},
    },
    hir_def::ItemKind,
    test_db::HirAnalysisTestDb,
};

fn find_func<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: fe_hir::hir_def::TopLevelMod<'db>,
    func_name: &str,
) -> fe_hir::hir_def::Func<'db> {
    top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == func_name) =>
            {
                Some(*func)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{func_name}`"))
}

fn first_assignment_ty<'db>(
    db: &'db HirAnalysisTestDb,
    body: &fe_hir::analysis::semantic::SemanticBody<'db>,
    pred: impl Fn(&SExpr<'db>) -> bool,
) -> String {
    body.blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            SStmtKind::Assign { dst, expr } if pred(expr) => {
                Some(body.locals[dst.index()].ty.pretty_print(db).to_string())
            }
            _ => None,
        })
        .expect("missing matching assignment")
}
#[test]
fn nested_wrapper_optional_projection_match_keeps_access_payload_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
msg Msg {
    #[selector = 1]
    Take -> u256,
}

struct Cell {
    value: u256,
}

impl Cell {
    fn try_get(mut self) -> Option<mut u256> {
        Option::Some(mut self.value)
    }
}

struct Wrapper {
    inner: Cell,
}

pub contract C {
    mut wrapped: Wrapper,

    recv Msg {
        Take -> u256 uses (mut wrapped) {
            match wrapped.inner.try_get() {
                Option::Some(value) => value
                Option::None => 0
            }
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contract = top_mod
        .all_contracts(&db)
        .iter()
        .copied()
        .find(|contract| {
            contract
                .name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "C")
        })
        .expect("missing contract");
    let (diags, _) = check_contract_recv_arm_body(&db, contract, 0, 0).clone();
    assert!(diags.is_empty(), "{diags:?}");

    let instance = SemanticInstance::new(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx: 0,
                arm_idx: 0,
            },
        ),
    );
    let body = instance.body(&db);

    assert_eq!(
        first_assignment_ty(&db, body, |expr| matches!(expr, SExpr::InspectPlace { .. })),
        "Option<mut u256>"
    );
}
#[test]
fn empty_enum_match_lowers_to_unreachable() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
enum Empty {}

fn eliminate(value: Empty) -> u8 {
    match value {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "eliminate");
    let (diags, _) = check_func_body(&db, func).clone();
    assert!(diags.is_empty(), "{diags:?}");

    let instance = SemanticInstance::new(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func, None)),
    );
    let body = instance.body(&db);

    assert!(
        body.blocks
            .iter()
            .any(|block| matches!(block.terminator.kind, STerminatorKind::Unreachable))
    );
}

#[test]
fn uninhabited_coercion_and_dead_projections_normalize_cleanly() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "uninhabited_lowering.fe".into(),
        r#"
use core::result::Result

enum Empty {}
struct Mixed {
    a: u256,
    b: !,
}

fn coerce_never(x: !) -> u256 {
    x
}

fn coerce_never_let(x: !) -> u256 {
    let y: u256 = x
    y
}

fn dead_proj(m: Mixed) -> u256 {
    m.a
}

fn dead_variant_proj(r: Result<Mixed, u256>) -> u256 {
    match r {
        Result::Ok(v) => v
        Result::Err(m) => m.a
    }
}

fn take_pair(_ a: mut u256, _ b: mut u256) {}

fn bad_borrow_in_dead_code(mut m: Mixed) {
    let r1 = mut m.a
    let r2 = mut m.a
    take_pair(r1, r2)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    for (name, expected_may_return) in [
        ("coerce_never", false),
        ("coerce_never_let", false),
        ("dead_proj", false),
        ("dead_variant_proj", true),
    ] {
        let func = find_func(&db, top_mod, name);
        let (diags, _) = check_func_body(&db, func).clone();
        assert!(diags.is_empty(), "{name}: {diags:?}");

        let instance = SemanticInstance::new(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func, None)),
        );
        normalize_semantic_body(&db, instance)
            .unwrap_or_else(|err| panic!("{name} failed normalization: {err:?}"));
        assert_eq!(
            semantic_may_return(&db, instance),
            expected_may_return,
            "{name} may_return mismatch"
        );
    }

    // Borrow checking must still catch aliasing bugs inside functions taking `Mixed`
    // (`Mixed` is uninhabited, NOT zero-sized in HIR).
    let bad = find_func(&db, top_mod, "bad_borrow_in_dead_code");
    let bad_instance = SemanticInstance::new(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(bad, None)),
    );
    let access_result = check_semantic_accesses(&db, bad_instance);
    assert!(
        access_result.is_err(),
        "expected borrow conflict in `bad_borrow_in_dead_code`, got Ok"
    );
}

// A condition that ends its block leaves the branches after it dead; they
// are not lowered, so nothing in them uses a value no path defines.
#[test]
fn conditions_that_end_their_block_normalize() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "dead_branches.fe".into(),
        r#"
struct D {
    x: u256,
}

impl core::ops::Drop for D {
    fn drop(own self) {}
}

fn mk() -> D {
    D { x: 1 }
}

fn never() -> ! {
    loop {}
}

fn returning_condition(_ x: u256) -> u256 {
    if { return 5 } {
        1
    } else {
        2
    }
}

fn chain_diverging_call() {
    if let _c = mk() && never() {}
}

fn chain_returning() {
    if let _c = mk() && { return } {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    for (name, expected_may_return) in [
        ("returning_condition", true),
        ("chain_diverging_call", false),
        ("chain_returning", true),
    ] {
        let func = find_func(&db, top_mod, name);
        let (diags, _) = check_func_body(&db, func).clone();
        assert!(diags.is_empty(), "{name}: {diags:?}");

        let instance = SemanticInstance::new(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func, None)),
        );
        normalize_semantic_body(&db, instance)
            .unwrap_or_else(|err| panic!("{name} failed normalization: {err:?}"));
        check_semantic_accesses(&db, instance)
            .unwrap_or_else(|err| panic!("{name} failed access checking: {err:?}"));
        assert_eq!(
            semantic_may_return(&db, instance),
            expected_may_return,
            "{name} may_return mismatch"
        );
    }
}
