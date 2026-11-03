use cranelift_entity::EntityRef;
use fe_hir::{
    analysis::{
        semantic::{
            SExpr, SStmtKind, STerminatorKind, SemanticInstance, identity_semantic_instance_key,
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
        first_assignment_ty(&db, body, |expr| matches!(
            expr,
            SExpr::ExtractEnumField { .. }
        )),
        "mut u256"
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

    let instance = SemanticInstance::new(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func, None)),
    );
    let body = instance.body(&db);

    assert!(body.blocks.iter().any(|block| matches!(
        block.terminator.kind,
        STerminatorKind::Assert { message: None }
    )));
}
