use cranelift_entity::EntityRef;
use fe_hir::{
    analysis::{
        semantic::{
            SExpr, SStmtKind, STerminatorKind, SemOrigin, get_or_build_semantic_instance,
            identity_semantic_instance_key,
        },
        ty::ty_check::{BodyOwner, LocalBinding, check_contract_recv_arm_body, check_func_body},
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
fn option_mut_payload_extract_keeps_capability_carrier_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
fn take(opt: Option<mut u256>) -> u256 {
    match opt {
        Option::Some(value) => value
        Option::None => 0
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "take");
    let (diags, _) = check_func_body(&db, func).clone();
    assert!(diags.is_empty(), "{diags:?}");

    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let body = instance.body(&db);

    assert_eq!(
        first_assignment_ty(&db, body, |expr| matches!(
            expr,
            SExpr::ExtractEnumField { .. }
        )),
        "mut u256"
    );
}

#[test]
fn borrowed_record_projection_keeps_ref_carrier_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
struct Pair {
    a: u256,
}

fn read(x: ref Pair) -> u256 {
    match x {
        Pair { a } => a
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "read");
    let (diags, _) = check_func_body(&db, func).clone();
    assert!(diags.is_empty(), "{diags:?}");

    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let body = instance.body(&db);

    assert_eq!(
        first_assignment_ty(&db, body, |expr| matches!(expr, SExpr::Field { .. })),
        "ref u256"
    );
}

#[test]
fn default_view_binding_reads_the_parameter_place() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
struct Boxed {
    value: u256,
}

fn read(boxed: Boxed) -> u256 {
    let rebound = boxed
    rebound.value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "read");
    let (diags, _) = check_func_body(&db, func).clone();
    assert!(diags.is_empty(), "{diags:?}");

    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let body = instance.body(&db);
    let (dst, place) = body
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|stmt| match &stmt.kind {
            SStmtKind::Assign {
                dst,
                expr: SExpr::ReadPlace { place, .. },
            } if place.path.is_empty() => Some((*dst, place)),
            _ => None,
        })
        .expect("borrowed binding must read the parameter place");

    assert_eq!(
        body.locals[dst.index()].ty.pretty_print(&db).to_string(),
        "ref Boxed"
    );
    assert!(matches!(
        body.locals[place.local.index()].source,
        Some(LocalBinding::Param { .. })
    ));
}

#[test]
fn nested_wrapper_mutex_match_keeps_capability_payload_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
use std::evm::Mutex

msg Msg {
    #[selector = 1]
    Take -> u256,
}

struct Wrapper {
    inner: Mutex<u256>,
}

pub contract C {
    mut wrapped: Wrapper,

    recv Msg {
        Take -> u256 uses (mut wrapped) {
            match wrapped.inner.try_lock() {
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

    let instance = get_or_build_semantic_instance(
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
        first_assignment_ty(&db, body, |expr| matches!(
            expr,
            SExpr::ExtractEnumField { .. }
        )),
        "mut u256"
    );
}

#[test]
fn view_enum_destructuring_keeps_ref_payload_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
enum Maybe {
    Some(u256),
    None,
}

fn read(x: Maybe) -> u256 {
    match x {
        Maybe::Some(value) => 0
        Maybe::None => 0
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "read");
    let (diags, _) = check_func_body(&db, func).clone();
    assert!(diags.is_empty(), "{diags:?}");

    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let body = instance.body(&db);

    assert_eq!(
        first_assignment_ty(&db, body, |expr| matches!(expr, SExpr::ReadPlace { .. })),
        "ref u256"
    );
}

#[test]
fn empty_enum_match_lowers_to_terminal_failure() {
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

    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let body = instance.body(&db);

    assert!(body.blocks.iter().any(|block| matches!(
        block.terminator.kind,
        STerminatorKind::Assert { message: None }
    )));
}

#[test]
fn mut_enum_destructuring_keeps_mut_payload_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
struct Boxed {
    value: u256,
}

fn write(x: mut Option<Boxed>) {
    match x {
        Option::Some(value) => value.value = 42,
        Option::None => (),
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "write");
    let (diags, _) = check_func_body(&db, func).clone();
    assert!(diags.is_empty(), "{diags:?}");

    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let body = instance.body(&db);

    assert_eq!(
        first_assignment_ty(&db, body, |expr| matches!(expr, SExpr::ReadPlace { .. })),
        "mut Boxed"
    );
}

#[test]
fn statically_true_while_with_break_keeps_a_reachable_exit() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
fn completes() -> u256 {
    while true {
        break
    }
    7
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "completes");
    let (diags, _) = check_func_body(&db, func).clone();
    assert!(diags.is_empty(), "{diags:?}");

    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let body = instance.body(&db);
    let break_target = body
        .blocks
        .iter()
        .find_map(
            |block| match (&block.terminator.origin, &block.terminator.kind) {
                (SemOrigin::Stmt(_), STerminatorKind::Goto(target)) => Some(*target),
                _ => None,
            },
        )
        .expect("break must branch to the loop exit");

    assert!(
        matches!(
            &body
                .block(break_target)
                .expect("break target block")
                .terminator
                .kind,
            STerminatorKind::Return(Some(_))
        ),
        "the reachable break target must continue to the function return: {:#?}",
        body.blocks,
    );
}

#[test]
fn statically_known_casted_tuple_pattern_uses_a_direct_edge() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
fn choose() -> u256 {
    if let (true, 5) = (true, 5 as u256) {
        7
    } else {
        9
    }
}

fn bool_cast_match() -> u256 {
    if let 1 = (true as u8) { 7 } else { 9 }
}

fn bool_cast_miss() -> u256 {
    if let 0 = (true as u8) { 7 } else { 9 }
}

fn dynamic_narrow(value: u256) -> u256 {
    if let 1 = ((value >> 248) as u8) { 7 } else { 9 }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    for name in ["choose", "bool_cast_match", "bool_cast_miss"] {
        let func = find_func(&db, top_mod, name);
        let (diags, _) = check_func_body(&db, func).clone();
        assert!(diags.is_empty(), "{name}: {diags:?}");
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
        let body = instance.body(&db);
        assert!(
            body.blocks.iter().all(|block| !matches!(
                block.terminator.kind,
                STerminatorKind::Branch { .. } | STerminatorKind::MatchEnum { .. }
            )),
            "a fully known casted pattern must not emit a runtime decision in {name}: {:#?}",
            body.blocks,
        );
    }

    let dynamic = find_func(&db, top_mod, "dynamic_narrow");
    let (diags, _) = check_func_body(&db, dynamic).clone();
    assert!(diags.is_empty(), "dynamic_narrow: {diags:?}");
    let dynamic_instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(dynamic)),
    );
    let dynamic_body = dynamic_instance.body(&db);
    assert!(
        dynamic_body
            .blocks
            .iter()
            .any(|block| matches!(block.terminator.kind, STerminatorKind::Branch { .. })),
        "a dynamic narrowing cast must retain its runtime pattern decision: {:#?}",
        dynamic_body.blocks,
    );
}

#[test]
fn known_normal_boolean_values_prune_raw_condition_cfg() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pattern_lowering.fe".into(),
        r#"
fn while_or(flag: bool) {
    while flag || true {}
}

fn while_and(flag: bool) {
    while flag && false {}
}

fn assert_or(flag: bool) {
    assert!(flag || true)
}

fn assert_and(flag: bool) {
    assert!(flag && false)
}

fn logical_or(flag: bool) -> bool {
    flag || true
}

fn logical_and(flag: bool) -> bool {
    flag && false
}

fn escape_then_true(escape: bool) {
    if {
        if escape {
            return
        }
        true
    } {}
}

enum BoolChoice {
    True,
    False,
}

const YES: bool = true
const NO: bool = false
const PICK: BoolChoice = BoolChoice::True

fn const_true() {
    assert!(YES)
}

fn const_false() {
    assert!(NO)
}

fn const_match() {
    assert!(match PICK {
        BoolChoice::True => true,
        BoolChoice::False => false,
    })
}

fn match_all_true(choice: BoolChoice) {
    assert!(match choice {
        BoolChoice::True => true,
        BoolChoice::False => true,
    })
}

fn match_all_false(choice: BoolChoice) {
    assert!(match choice {
        BoolChoice::True => false,
        BoolChoice::False => false,
    })
}

fn match_mixed(choice: BoolChoice) {
    assert!(match choice {
        BoolChoice::True => true,
        BoolChoice::False => false,
    })
}

fn match_known() {
    assert!(match BoolChoice::True {
        BoolChoice::True => true,
        BoolChoice::False => false,
    })
}

fn match_escape(choice: BoolChoice) {
    assert!(match choice {
        BoolChoice::True => true,
        BoolChoice::False => return,
    })
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    for (name, expected_branches, expected_asserts) in [
        ("while_or", 1, 0),
        ("while_and", 1, 0),
        ("assert_or", 1, 0),
        ("assert_and", 1, 1),
        ("logical_or", 1, 0),
        ("logical_and", 1, 0),
        ("escape_then_true", 1, 0),
        ("const_true", 0, 0),
        ("const_false", 0, 1),
        ("const_match", 0, 0),
        ("match_all_true", 0, 0),
        ("match_all_false", 0, 1),
        ("match_mixed", 1, 1),
        ("match_known", 0, 0),
        ("match_escape", 0, 0),
    ] {
        let func = find_func(&db, top_mod, name);
        let (diags, _) = check_func_body(&db, func).clone();
        assert!(diags.is_empty(), "{name}: {diags:?}");

        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
        let body = instance.body(&db);
        let branch_count = body
            .blocks
            .iter()
            .filter(|block| matches!(block.terminator.kind, STerminatorKind::Branch { .. }))
            .count();

        assert_eq!(
            branch_count, expected_branches,
            "{name} has an imprecise normal-result branch: {body:#?}"
        );
        assert_eq!(
            body.blocks
                .iter()
                .filter(|block| matches!(block.terminator.kind, STerminatorKind::Assert { .. }))
                .count(),
            expected_asserts,
            "{name} has an imprecise assertion failure edge: {body:#?}"
        );
    }
}
