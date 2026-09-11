use std::{
    env::{current_exe, var_os},
    process::Command,
};

use fe_hir::{
    analysis::{
        semantic::{
            ConstDesc, ConstRepr, CtfeConfig, CtfeError, EffectProviderSubst, EvalFailure,
            EvalOutcome, GenericSubst, ImplEnv, SExpr, SStmtKind, SemConstId, SemConstScalar,
            SemConstValue, SemOrigin, SemanticInstanceKey, array_const,
            const_computation_for_instance, describe_const_computation, eval_body_owner_const,
            force_const_computation, force_const_description, get_or_build_semantic_instance,
            identity_semantic_instance_key, int_const, reify_runtime_const,
            reify_runtime_const_for_ty, sem_const_ty, specialize_const_computation,
            specialize_const_description, tuple_const,
        },
        ty::{
            const_expr::{ConstExpr, ConstExprId, ConstInvocation},
            const_ty::{ConstTyData, ConstTyId, evaluate_type_level_int_const_expr},
            diagnostics::{TyDiagCollection, TyLowerDiag},
            ty_check::BodyOwner,
            ty_def::{PrimTy, TyBase, TyData, TyId},
        },
    },
    diagnosable::Diagnosable,
    hir_def::{ArithBinOp, Func, ItemKind, Partial, TopLevelMod, UnOp, attr::ArithmeticMode},
    span::LazySpan,
    test_db::HirAnalysisTestDb,
};
use num_bigint::BigInt;
use num_traits::ToPrimitive;

fn function<'db>(db: &'db HirAnalysisTestDb, module: TopLevelMod<'db>, name: &str) -> Func<'db> {
    module
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| matches!(func.name(db), Partial::Present(found) if found.data(db) == name))
        .unwrap_or_else(|| panic!("missing function {name}"))
}

fn root_error<'a, 'db>(error: &'a CtfeError<'db>) -> &'a CtfeError<'db> {
    match error {
        CtfeError::CalleeError { source, .. } => root_error(source),
        error => error,
    }
}

fn ready<'db>(outcome: EvalOutcome<'db, SemConstId<'db>>) -> SemConstId<'db> {
    match outcome {
        EvalOutcome::Ready(value) => value,
        other => panic!("expected CTFE value: {other:?}"),
    }
}

fn assert_same_value_tree(db: &HirAnalysisTestDb, lhs: SemConstId<'_>, rhs: SemConstId<'_>) {
    assert_eq!(sem_const_ty(db, lhs), sem_const_ty(db, rhs));
    match (lhs.value(db), rhs.value(db)) {
        (SemConstValue::Unit, SemConstValue::Unit) => {}
        (SemConstValue::Scalar { value: left, .. }, SemConstValue::Scalar { value: right, .. }) => {
            assert_eq!(left, right)
        }
        (SemConstValue::Tuple { elems: left, .. }, SemConstValue::Tuple { elems: right, .. })
        | (SemConstValue::Array { elems: left, .. }, SemConstValue::Array { elems: right, .. }) => {
            assert_eq!(left.len(), right.len());
            for (left, right) in left.iter().zip(right.iter()) {
                assert_same_value_tree(db, *left, *right);
            }
        }
        (
            SemConstValue::Struct { fields: left, .. },
            SemConstValue::Struct { fields: right, .. },
        ) => {
            assert_eq!(left.len(), right.len());
            for (left, right) in left.iter().zip(right.iter()) {
                assert_same_value_tree(db, *left, *right);
            }
        }
        (
            SemConstValue::Enum {
                variant: left_variant,
                fields: left,
                ..
            },
            SemConstValue::Enum {
                variant: right_variant,
                fields: right,
                ..
            },
        ) => {
            assert_eq!(left_variant, right_variant);
            assert_eq!(left.len(), right.len());
            for (left, right) in left.iter().zip(right.iter()) {
                assert_same_value_tree(db, *left, *right);
            }
        }
        _ => panic!(
            "replayed and direct values differ: {:?} versus {:?}",
            lhs.value(db),
            rhs.value(db)
        ),
    }
}

fn assert_specialization_law<'db>(
    db: &'db HirAnalysisTestDb,
    owner: BodyOwner<'db>,
    args: &[TyId<'db>],
) -> EvalOutcome<'db, SemConstId<'db>> {
    let origin = SemOrigin::Body(owner);
    let request =
        const_computation_for_instance(db, identity_semantic_instance_key(db, owner), Vec::new());
    let description = describe_const_computation(db, request, CtfeConfig::default())
        .into_ready()
        .expect("generic declaration must have a description");
    let specialized =
        specialize_const_description(db, &description, owner.scope(), owner.scope(), args, origin)
            .expect("description specialization must preserve its binder");
    let replayed = force_const_description(db, &specialized, CtfeConfig::default(), origin);
    let direct = eval_body_owner_const(db, owner, args.to_vec());
    match (&replayed, &direct) {
        (EvalOutcome::Ready(left), EvalOutcome::Ready(right)) => {
            assert_same_value_tree(db, left.value(), *right);
        }
        (
            EvalOutcome::Failed(EvalFailure::Ctfe(left)),
            EvalOutcome::Failed(EvalFailure::Ctfe(right)),
        ) => {
            let left = root_error(left);
            let right = root_error(right);
            assert_eq!(std::mem::discriminant(left), std::mem::discriminant(right));
            assert_eq!(format!("{left:?}"), format!("{right:?}"));
        }
        _ => panic!(
            "{owner:?} {args:?}: replayed and direct outcomes differ: {replayed:?} versus {direct:?}; description: {specialized:?}"
        ),
    }
    direct
}

fn assert_integer_result(
    db: &HirAnalysisTestDb,
    outcome: EvalOutcome<'_, SemConstId<'_>>,
    expected_ty: TyId<'_>,
    expected: u32,
) {
    let value = ready(outcome);
    assert_eq!(sem_const_ty(db, value), expected_ty);
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("expected an integer scalar: {:?}", value.value(db));
    };
    assert_eq!(value, BigInt::from(expected));
}

fn assert_signed_integer_result(
    db: &HirAnalysisTestDb,
    outcome: EvalOutcome<'_, SemConstId<'_>>,
    expected_ty: TyId<'_>,
    expected: i32,
) {
    let value = ready(outcome);
    assert_eq!(sem_const_ty(db, value), expected_ty);
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("expected a signed integer scalar");
    };
    assert_eq!(value, BigInt::from(expected));
}

fn assert_bool_result(
    db: &HirAnalysisTestDb,
    outcome: EvalOutcome<'_, SemConstId<'_>>,
    expected: bool,
) {
    let value = ready(outcome);
    assert_eq!(sem_const_ty(db, value), TyId::bool(db));
    assert!(matches!(
        value.value(db),
        SemConstValue::Scalar {
            value: SemConstScalar::Bool(actual),
            ..
        } if actual == expected
    ));
}

fn integer_const_arg<'db>(db: &'db HirAnalysisTestDb, ty: TyId<'db>, value: u32) -> TyId<'db> {
    TyId::new(
        db,
        TyData::ConstTy(ConstTyId::integer(db, ty, BigInt::from(value))),
    )
}

fn negative_const_arg<'db>(db: &'db HirAnalysisTestDb, ty: TyId<'db>, magnitude: u32) -> TyId<'db> {
    TyId::new(
        db,
        TyData::ConstTy(ConstTyId::new(
            db,
            ConstTyData::Abstract(
                ConstExprId::new(
                    db,
                    ConstExpr::UnOp {
                        op: UnOp::Minus,
                        mode: ArithmeticMode::Checked,
                        expr: integer_const_arg(db, ty, magnitude),
                    },
                ),
                ty,
            ),
        )),
    )
}

fn invocation<'db>(
    db: &'db HirAnalysisTestDb,
    func: Func<'db>,
    generic_args: Vec<TyId<'db>>,
    args: Vec<TyId<'db>>,
) -> ConstExpr<'db> {
    let owner = BodyOwner::Func(func);
    ConstExpr::Invocation(ConstInvocation {
        key: SemanticInstanceKey::new(
            db,
            owner,
            GenericSubst::new(db, generic_args),
            EffectProviderSubst::empty(db),
            ImplEnv::empty(db, owner.scope()),
        ),
        args,
        parameter_owner: owner.scope(),
    })
}

fn check_dependent_declaration(name: &str, result_ty: &str, expression: &str) {
    let source = format!(
        r#"
enum Maybe {{ Some(u8), None }}
impl Copy for Maybe {{}}
struct Marker<const N: usize> {{}}
impl<const N: usize> Marker<N> {{ const VALUE: {result_ty} = {expression} }}
const fn probe<const N: usize>() -> {result_ty} {{ {expression} }}
const fn nested<const N: usize>() -> usize {{ probe<N>() as usize }}
type Alias<const N: usize> = [u8; nested<N>()]
"#
    );
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(format!("dependent_{name}_generic.fe").into(), &source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for function_name in ["probe", "nested"] {
        let owner = BodyOwner::Func(function(&db, module, function_name));
        let outcome = eval_body_owner_const(&db, owner, vec![]);
        assert!(
            matches!(outcome, EvalOutcome::Blocked(_)),
            "{name}: {function_name} must block on its unresolved index: {outcome:?}"
        );
    }

    let file = db.new_stand_alone(
        format!("dependent_{name}_concrete.fe").into(),
        &format!(
            "{source}\nconst fn concrete() -> {result_ty} {{ Marker<1>::VALUE }}\nconst fn concrete_alias() -> u8 {{ let values: Alias<1> = [7; 7]\n values[6] }}"
        ),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let value = ready(eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "concrete")),
        vec![],
    ));
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(&db)
    else {
        panic!(
            "{name}: expected integer result, got {:?}",
            value.value(&db)
        );
    };
    assert_eq!(value.to_u8(), Some(7), "{name}");
    let alias_value = ready(eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "concrete_alias")),
        vec![],
    ));
    assert!(matches!(
        alias_value.value(&db),
        SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } if value.to_u8() == Some(7)
    ));
}

#[test]
fn generic_branch_declaration_replays_after_specialization() {
    check_dependent_declaration("branch", "u8", "if [true; N][0] { 7 } else { 9 }");
}

#[test]
fn generic_match_declaration_replays_after_specialization() {
    check_dependent_declaration(
        "match",
        "u8",
        "match [Maybe::Some(7); N][0] { Maybe::Some(value) => value, Maybe::None => 9 }",
    );
}

#[test]
fn generic_cast_declaration_replays_after_specialization() {
    check_dependent_declaration("cast", "u16", "[7 as u8; N][0] as u16");
}

#[test]
fn differential_generic_branch_match_cast_and_integer_term() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "differential_generic_values.fe".into(),
        r#"
enum Maybe { Some(u8), None }
impl Copy for Maybe {}
const fn branch<const N: usize>() -> u8 { if [true; N][0] { 7 } else { 9 } }
const fn matched<const N: usize>() -> u8 {
    match [Maybe::Some(7); N][0] {
        Maybe::Some(value) => value
        Maybe::None => 9
    }
}

const fn casted<const N: usize>() -> u16 { [7 as u8; N][0] as u16 }
const fn increment<const N: u8>() -> u8 { N + 1 }
const fn nested<const N: u8>() -> u8 { (N + 1) * 2 }
const fn via_call<const N: u8>() -> u8 { nested<N>() }
const fn plus1(value: u8) -> u8 { value + 1 }
const fn caller_arg<const N: u8>() -> u8 { plus1(value: N / 0) }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let u16_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U16)));
    for (name, ty) in [("branch", u8_ty), ("matched", u8_ty), ("casted", u16_ty)] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let one = integer_const_arg(&db, usize_ty, 1);
        assert_integer_result(&db, assert_specialization_law(&db, owner, &[one]), ty, 7);
        let zero = integer_const_arg(&db, usize_ty, 0);
        let outcome = assert_specialization_law(&db, owner, &[zero]);
        assert!(matches!(
            outcome,
            EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                if matches!(root_error(error), CtfeError::OutOfBounds { .. })
        ));
    }
    let increment = BodyOwner::Func(function(&db, module, "increment"));
    let seven = integer_const_arg(&db, u8_ty, 7);
    assert_integer_result(
        &db,
        assert_specialization_law(&db, increment, &[seven]),
        u8_ty,
        8,
    );
    let max = integer_const_arg(&db, u8_ty, 255);
    let outcome = assert_specialization_law(&db, increment, &[max]);
    assert!(matches!(
        outcome,
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::ArithmeticOverflow { .. })
    ));
    for name in ["nested", "via_call"] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let one = integer_const_arg(&db, u8_ty, 1);
        assert_integer_result(&db, assert_specialization_law(&db, owner, &[one]), u8_ty, 4);
        for value in [127, 255] {
            let arg = integer_const_arg(&db, u8_ty, value);
            let outcome = assert_specialization_law(&db, owner, &[arg]);
            assert!(matches!(
                outcome,
                EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                    if matches!(root_error(error), CtfeError::ArithmeticOverflow { .. })
            ));
        }
    }
    let owner = BodyOwner::Func(function(&db, module, "via_call"));
    let request =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, owner), Vec::new());
    let description = describe_const_computation(&db, request, CtfeConfig::default())
        .into_ready()
        .unwrap();
    let max = integer_const_arg(&db, u8_ty, 255);
    let specialized = specialize_const_description(
        &db,
        &description,
        owner.scope(),
        owner.scope(),
        &[max],
        SemOrigin::Body(owner),
    )
    .unwrap();
    let replayed = force_const_description(
        &db,
        &specialized,
        CtfeConfig::default(),
        SemOrigin::Body(owner),
    );
    let direct = eval_body_owner_const(&db, owner, vec![max]);
    match (replayed, direct) {
        (
            EvalOutcome::Failed(EvalFailure::Ctfe(replayed)),
            EvalOutcome::Failed(EvalFailure::Ctfe(direct)),
        ) => {
            let CtfeError::CalleeError {
                callee: left,
                source: left_source,
                origin: left_origin,
            } = replayed
            else {
                panic!("replayed term lost its callee frame: {replayed:?}");
            };
            let CtfeError::CalleeError {
                callee: right,
                source: right_source,
                origin: right_origin,
            } = direct
            else {
                panic!("direct call lost its callee frame: {direct:?}");
            };
            assert_eq!(left_origin, right_origin);
            assert_eq!(left_source, right_source);
            assert_eq!(
                left.key(&db),
                right.key(&db),
                "inlined term must retain its callee key"
            );
        }
        (replayed, direct) => {
            panic!("expected nested call failures: {replayed:?} versus {direct:?}")
        }
    }
    let owner = BodyOwner::Func(function(&db, module, "caller_arg"));
    let request =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, owner), Vec::new());
    let description = describe_const_computation(&db, request, CtfeConfig::default())
        .into_ready()
        .unwrap();
    let one = integer_const_arg(&db, u8_ty, 1);
    let specialized = specialize_const_description(
        &db,
        &description,
        owner.scope(),
        owner.scope(),
        &[one],
        SemOrigin::Body(owner),
    )
    .unwrap();
    let replayed = force_const_description(
        &db,
        &specialized,
        CtfeConfig::default(),
        SemOrigin::Body(owner),
    );
    let direct = eval_body_owner_const(&db, owner, vec![one]);
    match (replayed, direct) {
        (
            EvalOutcome::Failed(EvalFailure::Ctfe(replayed)),
            EvalOutcome::Failed(EvalFailure::Ctfe(direct)),
        ) => {
            assert!(matches!(replayed, CtfeError::DivisionByZero { .. }));
            assert_eq!(
                replayed, direct,
                "caller argument fault must retain the caller frame"
            );
        }
        (replayed, direct) => panic!("expected argument faults: {replayed:?} versus {direct:?}"),
    }
}

#[test]
fn invocation_term_keeps_callee_context_after_arguments() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "invocation_term_context.fe".into(),
        r#"
const fn selected<const N: usize>() -> u8 {
    if N == 0 { [7 as u8; N][0] } else { 7 }
}
const fn relay<const N: usize>() -> u8 { selected<N>() }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "relay"));
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let one = integer_const_arg(&db, usize_ty, 1);
    assert_integer_result(&db, assert_specialization_law(&db, owner, &[one]), u8_ty, 7);
    let zero = integer_const_arg(&db, usize_ty, 0);
    assert!(matches!(
        assert_specialization_law(&db, owner, &[zero]),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::OutOfBounds { .. })
    ));

    let request =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, owner), Vec::new());
    let description = describe_const_computation(&db, request, CtfeConfig::default())
        .into_ready()
        .unwrap();
    assert!(matches!(description.repr(), ConstRepr::Term(_)));
    let specialized = specialize_const_description(
        &db,
        &description,
        owner.scope(),
        owner.scope(),
        &[zero],
        SemOrigin::Body(owner),
    )
    .unwrap();
    let replayed = force_const_description(
        &db,
        &specialized,
        CtfeConfig::default(),
        SemOrigin::Body(owner),
    );
    let direct = eval_body_owner_const(&db, owner, vec![zero]);
    match (replayed, direct) {
        (
            EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::CalleeError {
                callee: left,
                origin: left_origin,
                source: left_source,
            })),
            EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::CalleeError {
                callee: right,
                origin: right_origin,
                source: right_source,
            })),
        ) => {
            assert_eq!(left_origin, right_origin);
            assert_eq!(left_source, right_source);
            assert_eq!(left.key(&db), right.key(&db));
        }
        (replayed, direct) => panic!("expected callee failures: {replayed:?} versus {direct:?}"),
    }
}

#[test]
fn pure_term_replay_keeps_source_assignment_order() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "pure_term_order.fe".into(),
        r#"
const fn reversed<const N: u8>() -> u8 {
    let first = N / 0
    let second = N + 1
    second + first
}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "reversed"));
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let max = integer_const_arg(&db, u8_ty, 255);
    let result = assert_specialization_law(&db, owner, &[max]);
    assert!(matches!(
        result,
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::DivisionByZero { .. })
    ));
}

#[test]
fn resource_limits_and_cache_environments_stay_separate() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "ctfe_resources.fe".into(),
        r#"
const fn recursive() -> u8 { recursive() }
const fn count<const N: usize>() -> usize {
    let mut i: usize = 0
    while i < N { i += 1 }
    i
}
const fn choose<const N: u8>() -> u8 { N }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);

    let recursive = BodyOwner::Func(function(&db, module, "recursive"));
    let request = const_computation_for_instance(
        &db,
        identity_semantic_instance_key(&db, recursive),
        Vec::new(),
    );
    let limited = force_const_computation(
        &db,
        request,
        CtfeConfig {
            recursion_limit: 4,
            ..CtfeConfig::default()
        },
    );
    assert!(matches!(
        limited,
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::RecursionLimitExceeded { .. })
    ));

    let count = BodyOwner::Func(function(&db, module, "count"));
    let template =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, count), Vec::new());
    for _ in 0..2 {
        assert!(matches!(
            force_const_computation(&db, template, CtfeConfig::default()),
            EvalOutcome::Blocked(_)
        ));
    }
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let large = integer_const_arg(&db, usize_ty, 1000);
    let specialized = specialize_const_computation(&db, template, count.scope(), &[large]).unwrap();
    let limited = force_const_computation(
        &db,
        specialized,
        CtfeConfig {
            step_limit: 10,
            ..CtfeConfig::default()
        },
    );
    assert!(matches!(
        limited,
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::StepLimitExceeded { .. })
    ));
    let forced = force_const_computation(&db, specialized, CtfeConfig::default())
        .into_ready()
        .unwrap();
    let direct = ready(eval_body_owner_const(&db, count, vec![large]));
    assert_same_value_tree(&db, forced.value(), direct);
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = forced.value().value(&db)
    else {
        panic!("count must return an integer");
    };
    assert_eq!(value.to_usize(), Some(1000));

    let choose = BodyOwner::Func(function(&db, module, "choose"));
    let template = const_computation_for_instance(
        &db,
        identity_semantic_instance_key(&db, choose),
        Vec::new(),
    );
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    for value in [1, 2, 1] {
        let arg = integer_const_arg(&db, u8_ty, value);
        let request = specialize_const_computation(&db, template, choose.scope(), &[arg]).unwrap();
        let result = force_const_computation(&db, request, CtfeConfig::default())
            .into_ready()
            .unwrap();
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value: actual },
            ..
        } = result.value().value(&db)
        else {
            panic!("choose must return an integer");
        };
        assert_eq!(actual.to_u32(), Some(value));
    }
}

#[test]
fn recursive_constant_queries_return_stable_failures() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "recursive_constants.fe".into(),
        r#"
const DIRECT: u8 = DIRECT
const LEFT: u8 = RIGHT
const RIGHT: u8 = LEFT
"#,
    );
    let (module, _) = db.top_mod(file);
    for name in ["DIRECT", "LEFT", "RIGHT"] {
        let item = module
            .all_items(&db)
            .iter()
            .copied()
            .find(|item| item.name(&db).is_some_and(|found| found.data(&db) == name))
            .unwrap_or_else(|| panic!("missing constant {name}"));
        let ItemKind::Const(constant) = item else {
            panic!("{name} is not a constant");
        };
        let owner = BodyOwner::Const(constant);
        let first = eval_body_owner_const(&db, owner, vec![]);
        let second = eval_body_owner_const(&db, owner, vec![]);
        assert_eq!(
            first, second,
            "recursive result must be stable across queries"
        );
        assert!(
            matches!(
                first,
                EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                    if matches!(root_error(error), CtfeError::RecursiveConst { .. })
            ),
            "{name}: {first:?}"
        );
        let request = const_computation_for_instance(
            &db,
            identity_semantic_instance_key(&db, owner),
            Vec::new(),
        );
        let forced = force_const_computation(&db, request, CtfeConfig::default());
        assert!(
            matches!(
                forced,
                EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                    if matches!(root_error(error), CtfeError::RecursiveConst { .. })
            ),
            "{name}: {forced:?}"
        );
        let described = describe_const_computation(&db, request, CtfeConfig::default());
        assert!(
            matches!(
                described,
                EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                    if matches!(root_error(error), CtfeError::RecursiveConst { .. })
            ),
            "{name}: {described:?}"
        );
    }
}

#[test]
fn blocked_mutating_requests_restart_from_original_state() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "blocked_mutating_requests.fe".into(),
        r#"
const fn mutate<const N: usize>() -> u8 {
    let mut values = [0 as u8; 2]
    values[0] = 7
    let selected = [values[0]; N][0]
    values[1] = selected
    values[0] + values[1]
}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "mutate"));
    let request =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, owner), Vec::new());
    let first = force_const_computation(&db, request, CtfeConfig::default());
    let second = force_const_computation(&db, request, CtfeConfig::default());
    assert_eq!(
        first, second,
        "blocked attempts must not publish mutable state"
    );
    assert!(matches!(first, EvalOutcome::Blocked(_)));

    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let one = integer_const_arg(&db, usize_ty, 1);
    let zero = integer_const_arg(&db, usize_ty, 0);
    for arg in [one, zero, one] {
        let specialized =
            specialize_const_computation(&db, request, owner.scope(), &[arg]).unwrap();
        let outcome = force_const_computation(&db, specialized, CtfeConfig::default());
        if arg == zero {
            assert!(matches!(
                outcome,
                EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                    if matches!(root_error(error), CtfeError::OutOfBounds { .. })
            ));
        } else {
            let value = outcome.into_ready().unwrap().value();
            assert_eq!(sem_const_ty(&db, value), u8_ty);
            let SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } = value.value(&db)
            else {
                panic!("mutate must return an integer");
            };
            assert_eq!(value.to_u8(), Some(14));
        }
    }
}

#[test]
fn differential_nested_record_enum_array_tuple_and_fixed_string() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "nested_const_values.fe".into(),
        r#"
enum Maybe { Some(u8), None }
impl Copy for Maybe {}
struct Marker<const N: usize> {
    values: [Maybe; N],
    pair: (u8, bool),
    text: String<3>,
}
const fn aggregate<const N: usize>() -> Marker<N> {
    Marker<N> {
        values: [Maybe::Some(7); N],
        pair: (9, true),
        text: "abc",
    }
}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "aggregate"));
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    for count in [0, 2] {
        let arg = integer_const_arg(&db, usize_ty, count);
        let value = ready(assert_specialization_law(&db, owner, &[arg]));
        let SemConstValue::Struct { fields, .. } = value.value(&db) else {
            panic!("expected a record value: {:?}", value.value(&db));
        };
        assert_eq!(fields.len(), 3);
        let SemConstValue::Array { elems, .. } = fields[0].value(&db) else {
            panic!("expected the first field to be an array");
        };
        assert_eq!(elems.len(), count as usize);
        for element in elems {
            let SemConstValue::Enum { fields, .. } = element.value(&db) else {
                panic!("expected an enum array element");
            };
            assert_eq!(fields.len(), 1);
            let SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } = fields[0].value(&db)
            else {
                panic!("expected enum integer payload");
            };
            assert_eq!(value.to_u8(), Some(7));
        }
        let SemConstValue::Tuple { elems, .. } = fields[1].value(&db) else {
            panic!("expected the second field to be a tuple");
        };
        assert_eq!(elems.len(), 2);
        assert!(matches!(
            elems[1].value(&db),
            SemConstValue::Scalar {
                value: SemConstScalar::Bool(true),
                ..
            }
        ));
        assert!(matches!(
            fields[2].value(&db),
            SemConstValue::Scalar {
                value: SemConstScalar::Bytes(bytes),
                ..
            } if bytes == b"abc"
        ));
    }
}

#[test]
fn differential_checked_wrapping_and_saturating_u8() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "scalar_arithmetic_modes.fe".into(),
        r#"
use core::ops::SaturatingAdd
const fn checked<const N: u8>() -> u8 { N + 1 }
#[arithmetic(unchecked)]
const fn wrapping<const N: u8>() -> u8 { N + 1 }
const fn saturating<const N: u8>() -> u8 { N.saturating_add(1) }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let one = integer_const_arg(&db, u8_ty, 1);
    let max = integer_const_arg(&db, u8_ty, 255);
    for name in ["checked", "wrapping", "saturating"] {
        let owner = BodyOwner::Func(function(&db, module, name));
        assert_integer_result(&db, assert_specialization_law(&db, owner, &[one]), u8_ty, 2);
        let outcome = assert_specialization_law(&db, owner, &[max]);
        match name {
            "checked" => assert!(matches!(
                outcome,
                EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                    if matches!(root_error(error), CtfeError::ArithmeticOverflow { .. })
            )),
            "wrapping" => assert_integer_result(&db, outcome, u8_ty, 0),
            "saturating" => assert_integer_result(&db, outcome, u8_ty, 255),
            _ => unreachable!(),
        }
    }
}

#[test]
fn differential_signed_widening_and_narrowing_casts() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "signed_casts.fe".into(),
        r#"
const fn widen<const N: i8>() -> i16 { N as i16 }
const fn narrow<const N: i16>() -> i8 { N.downcast_truncate() }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let i8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::I8)));
    let i16_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::I16)));
    for (name, input_ty, output_ty) in [("widen", i8_ty, i16_ty), ("narrow", i16_ty, i8_ty)] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let negative = negative_const_arg(&db, input_ty, 7);
        assert_signed_integer_result(
            &db,
            assert_specialization_law(&db, owner, &[negative]),
            output_ty,
            -7,
        );
    }
}

#[test]
fn differential_division_remainder_exponent_and_shift_edges() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "scalar_edge_cases.fe".into(),
        r#"
const fn divide<const N: i8>() -> i8 { N / 2 }
const fn remainder<const N: i8>() -> i8 { N % 2 }
const fn divide_by<const N: i8>() -> i8 { 7 / N }
const fn power<const N: i8>() -> i8 { 2 ** N }
const fn right_shift<const N: i8>() -> i8 { (-1 as i8) >> N }
const fn left_shift<const N: u8>() -> u8 { (1 as u8) << N }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let i8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::I8)));
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let negative_seven = negative_const_arg(&db, i8_ty, 7);
    for (name, expected) in [("divide", -3), ("remainder", -1)] {
        let owner = BodyOwner::Func(function(&db, module, name));
        assert_signed_integer_result(
            &db,
            assert_specialization_law(&db, owner, &[negative_seven]),
            i8_ty,
            expected,
        );
    }
    let zero = integer_const_arg(&db, i8_ty, 0);
    let divide_by = BodyOwner::Func(function(&db, module, "divide_by"));
    assert!(matches!(
        assert_specialization_law(&db, divide_by, &[zero]),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::DivisionByZero { .. })
    ));
    let negative_one = negative_const_arg(&db, i8_ty, 1);
    let power = BodyOwner::Func(function(&db, module, "power"));
    assert!(matches!(
        assert_specialization_law(&db, power, &[negative_one]),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::NegativeExponent { .. })
    ));
    let right_shift = BodyOwner::Func(function(&db, module, "right_shift"));
    assert_signed_integer_result(
        &db,
        assert_specialization_law(&db, right_shift, &[negative_one]),
        i8_ty,
        -1,
    );
    let left_shift = BodyOwner::Func(function(&db, module, "left_shift"));
    let eight = integer_const_arg(&db, u8_ty, 8);
    assert_integer_result(
        &db,
        assert_specialization_law(&db, left_shift, &[eight]),
        u8_ty,
        0,
    );
}

#[test]
fn differential_strictness_projection_branch_and_loop_matrix() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "differential_strictness.fe".into(),
        r#"
const fn fault<const N: usize>() -> u8 { [7 as u8; N][0] }
const fn overwritten<const N: usize>() -> u8 {
    let mut result = [7 as u8; N][0]
    result = 9
    result
}
const fn ignored_field<const N: usize>() -> u8 {
    let pair = ([7 as u8; N][0], 9 as u8)
    pair.1
}
const fn unused_call<const N: usize>() -> u8 {
    let _ = fault<N>()
    9
}
const fn zero_repeat<const N: usize>() -> u8 {
    let _ = [[7 as u8; N][0]; 0]
    9
}
const fn element_fault_before_index<const N: usize>() -> u8 {
    let values = [1 as u8 / (0 as u8)]
    values[N]
}
const fn blocked_before_fault<const N: usize>() -> u8 {
    let value = [7 as u8; N][0]
    let _ = 1 as u8 / (0 as u8)
    value
}
const fn dead_branch<const N: usize>() -> u8 {
    if true { 7 } else { [7 as u8; N][0] }
}
const fn selected_branch<const N: usize>() -> u8 {
    if N == 0 { [7 as u8; N][0] } else { 7 }
}
const fn short_circuit<const N: usize>() -> bool {
    false && [true; N][0]
}
const fn counted<const N: usize>() -> usize {
    let mut i: usize = 0
    while i < N { i += 1 }
    i
}
const fn unary_comparison<const N: usize>() -> bool {
    -[7 as i8; N][0] < 0
}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let zero = integer_const_arg(&db, usize_ty, 0);
    let one = integer_const_arg(&db, usize_ty, 1);
    let two = integer_const_arg(&db, usize_ty, 2);
    for name in ["overwritten", "ignored_field", "unused_call", "zero_repeat"] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let outcome = assert_specialization_law(&db, owner, &[zero]);
        assert!(
            matches!(
                outcome,
                EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                    if matches!(root_error(error), CtfeError::OutOfBounds { .. })
            ),
            "{name}: zero length must preserve the earlier fault"
        );
        assert_integer_result(&db, assert_specialization_law(&db, owner, &[one]), u8_ty, 9);
    }
    let owner = BodyOwner::Func(function(&db, module, "element_fault_before_index"));
    let generic_request =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, owner), Vec::new());
    assert!(matches!(
        force_const_computation(&db, generic_request, CtfeConfig::default()),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::DivisionByZero { .. })
    ));
    for length in [zero, one] {
        assert!(matches!(
            eval_body_owner_const(&db, owner, vec![length]),
            EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                if matches!(root_error(error), CtfeError::DivisionByZero { .. })
        ));
    }
    let owner = BodyOwner::Func(function(&db, module, "blocked_before_fault"));
    let generic_request =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, owner), Vec::new());
    assert!(matches!(
        force_const_computation(&db, generic_request, CtfeConfig::default()),
        EvalOutcome::Blocked(_)
    ));
    assert!(matches!(
        assert_specialization_law(&db, owner, &[zero]),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::OutOfBounds { .. })
    ));
    assert!(matches!(
        assert_specialization_law(&db, owner, &[one]),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::DivisionByZero { .. })
    ));
    assert_integer_result(
        &db,
        assert_specialization_law(
            &db,
            BodyOwner::Func(function(&db, module, "dead_branch")),
            &[zero],
        ),
        u8_ty,
        7,
    );
    let selected = BodyOwner::Func(function(&db, module, "selected_branch"));
    assert_integer_result(
        &db,
        assert_specialization_law(&db, selected, &[one]),
        u8_ty,
        7,
    );
    assert!(matches!(
        assert_specialization_law(&db, selected, &[zero]),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::OutOfBounds { .. })
    ));
    assert_bool_result(
        &db,
        assert_specialization_law(
            &db,
            BodyOwner::Func(function(&db, module, "short_circuit")),
            &[zero],
        ),
        false,
    );
    let counted = BodyOwner::Func(function(&db, module, "counted"));
    assert_integer_result(
        &db,
        assert_specialization_law(&db, counted, &[zero]),
        usize_ty,
        0,
    );
    assert_integer_result(
        &db,
        assert_specialization_law(&db, counted, &[two]),
        usize_ty,
        2,
    );
    let unary = BodyOwner::Func(function(&db, module, "unary_comparison"));
    assert_bool_result(&db, assert_specialization_law(&db, unary, &[one]), true);
    assert!(matches!(
        assert_specialization_law(&db, unary, &[zero]),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::OutOfBounds { .. })
    ));
}

#[test]
fn differential_unused_generics_arguments_and_symbolic_equality() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "differential_evaluation_obligations.fe".into(),
        r#"
extern { const fn foreign(_: usize) -> u8 }
const fn unsupported<const N: usize>() -> u8 { foreign(7) }
const fn unsupported_arg<const N: usize>() -> u8 { foreign(1 as usize / (0 as usize)) }
const fn unused_generic<const N: usize>() -> u8 { 7 }
const fn ignore(_ value: u8) -> u8 { 7 }
const fn faulty_arg<const N: usize>() -> u8 { ignore([7 as u8; N][0]) }
const fn equality<const N: usize>() -> bool {
    [1 as u8; N][0] == [2 as u8; N][0]
}
const fn early_fault<const N: usize>() -> u8 {
    let _ = 1 as u8 / (0 as u8)
    [7 as u8; N][0]
}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let zero = integer_const_arg(&db, usize_ty, 0);
    let one = integer_const_arg(&db, usize_ty, 1);
    for name in ["unsupported", "unused_generic"] {
        let owner = BodyOwner::Func(function(&db, module, name));
        for arg in [zero, one] {
            let outcome = assert_specialization_law(&db, owner, &[arg]);
            if name == "unsupported" {
                assert!(matches!(
                    outcome,
                    EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                        if matches!(root_error(error), CtfeError::NotConstEvaluable { .. })
                ));
            } else {
                assert_integer_result(&db, outcome, u8_ty, 7);
            }
        }
    }
    let owner = BodyOwner::Func(function(&db, module, "unsupported_arg"));
    let request =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, owner), Vec::new());
    let described = describe_const_computation(&db, request, CtfeConfig::default());
    let direct = eval_body_owner_const(&db, owner, vec![one]);
    match (described, direct) {
        (
            EvalOutcome::Failed(EvalFailure::Ctfe(described)),
            EvalOutcome::Failed(EvalFailure::Ctfe(direct)),
        ) => {
            assert!(matches!(described, CtfeError::DivisionByZero { .. }));
            assert_eq!(
                described, direct,
                "extern argument must fail at its operation"
            );
        }
        (described, direct) => {
            panic!("expected argument faults: {described:?} versus {direct:?}")
        }
    }
    let faulty_arg = BodyOwner::Func(function(&db, module, "faulty_arg"));
    assert_integer_result(
        &db,
        assert_specialization_law(&db, faulty_arg, &[one]),
        u8_ty,
        7,
    );
    assert!(matches!(
        assert_specialization_law(&db, faulty_arg, &[zero]),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::OutOfBounds { .. })
    ));
    let equality = BodyOwner::Func(function(&db, module, "equality"));
    let request = const_computation_for_instance(
        &db,
        identity_semantic_instance_key(&db, equality),
        Vec::new(),
    );
    assert!(matches!(
        force_const_computation(&db, request, CtfeConfig::default()),
        EvalOutcome::Blocked(_)
    ));
    assert_bool_result(&db, assert_specialization_law(&db, equality, &[one]), false);
    assert!(matches!(
        assert_specialization_law(&db, equality, &[zero]),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::OutOfBounds { .. })
    ));
    let early = BodyOwner::Func(function(&db, module, "early_fault"));
    let request =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, early), Vec::new());
    assert!(matches!(
        describe_const_computation(&db, request, CtfeConfig::default()),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::DivisionByZero { .. })
    ));
    assert!(matches!(
        eval_body_owner_const(&db, early, vec![one]),
        EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
            if matches!(root_error(error), CtfeError::DivisionByZero { .. })
    ));
}

#[test]
fn discarded_index_keeps_its_bounds_obligation() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "discarded_index.fe".into(),
        r#"
const fn discarded<const N: usize>() -> u8 {
    let _ = [7 as u8; N][0]
    9
}
const fn one() -> u8 { discarded<1>() }
const fn zero() -> u8 { discarded<0>() }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let generic = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "discarded")),
        vec![],
    );
    assert!(
        matches!(generic, EvalOutcome::Blocked(_)),
        "unresolved index must block the complete request: {generic:?}"
    );

    let one = ready(eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "one")),
        vec![],
    ));
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = one.value(&db)
    else {
        panic!("expected integer result, got {:?}", one.value(&db));
    };
    assert_eq!(value.to_u8(), Some(9));

    let zero = eval_body_owner_const(&db, BodyOwner::Func(function(&db, module, "zero")), vec![]);
    assert!(
        matches!(zero, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::OutOfBounds { .. })),
        "{zero:?}"
    );
}

#[test]
fn user_named_add_runs_its_body_after_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "user_named_add.fe".into(),
        r#"
const fn add<const N: usize>(x: usize, y: usize) -> usize {
    let mut values: [usize; N] = [0; N]
    values[0] = 1
    x + y + 40
}

struct Marker<const N: usize> {}
impl<const N: usize> Marker<N> {
    const VALUE: usize = add<N>(x: 2, y: 3)
}
const fn direct() -> usize { add<1>(x: 2, y: 3) }
const fn through_type() -> usize { Marker<1>::VALUE }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for name in ["direct", "through_type"] {
        let value = ready(eval_body_owner_const(
            &db,
            BodyOwner::Func(function(&db, module, name)),
            vec![],
        ));
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.value(&db)
        else {
            panic!(
                "{name}: expected integer result, got {:?}",
                value.value(&db)
            );
        };
        assert_eq!(value.to_usize(), Some(45), "{name}");
    }

    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let expr = ConstExprId::new(
        &db,
        invocation(
            &db,
            function(&db, module, "add"),
            vec![integer_const_arg(&db, usize_ty, 1)],
            vec![
                integer_const_arg(&db, usize_ty, 2),
                integer_const_arg(&db, usize_ty, 3),
            ],
        ),
    );
    assert!(
        evaluate_type_level_int_const_expr(&db, expr, usize_ty).is_none(),
        "a user call is not a primitive integer expression"
    );
}

#[test]
fn user_named_add_preserves_body_assertion_through_type_level_use() {
    let source = r#"
const fn add<const N: usize>(x: usize, y: usize) -> usize {
    assert!(N != 0, "add body failed")
    x + y + 40
}
struct Marker<const N: usize> {}
impl<const N: usize> Marker<N> {
    const VALUE: usize = add<N>(x: 2, y: 3)
}
trait Selected<const N: usize> {
    const VALUE: usize = add<N>(x: 2, y: 3)
}
struct TraitMarker<const N: usize> {}
impl<const N: usize> Selected<N> for TraitMarker<N> {}
"#;
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("user_named_add_assert_generic.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);

    let file = db.new_stand_alone(
        "user_named_add_assert_specialized.fe".into(),
        &format!(
            "{source}\nconst fn direct_bad() -> usize {{ add<0>(x: 2, y: 3) }}\nconst fn through_type_bad() -> usize {{ Marker<0>::VALUE }}\nconst fn through_trait_bad() -> usize {{ TraitMarker<0>::VALUE }}\ntype Bad = [u8; Marker<0>::VALUE]\ntype TraitBad = [u8; TraitMarker<0>::VALUE]"
        ),
    );
    let (module, _) = db.top_mod(file);
    for name in ["direct_bad", "through_type_bad", "through_trait_bad"] {
        let outcome =
            eval_body_owner_const(&db, BodyOwner::Func(function(&db, module, name)), vec![]);
        assert!(
            matches!(
                outcome,
                EvalOutcome::Failed(EvalFailure::Ctfe(ref error))
                    if matches!(root_error(error), CtfeError::AssertionFailed { message: Some(message), .. } if message == "add body failed")
            ),
            "{name}: {outcome:?}"
        );
    }
    for alias_name in ["Bad", "TraitBad"] {
        let alias = module
            .all_type_aliases(&db)
            .iter()
            .copied()
            .find(|alias| {
                matches!(alias.name(&db), Partial::Present(name) if name.data(&db) == alias_name)
            })
            .unwrap_or_else(|| panic!("missing {alias_name} alias"));
        let diags = alias.diags(&db);
        assert!(
            diags.iter().any(|diag| matches!(
                diag,
                TyDiagCollection::Ty(TyLowerDiag::ConstEvalAssertionFailed { message: Some(message), .. })
                    if message == "add body failed"
            )),
            "{alias_name}: {diags:#?}"
        );
    }
}

#[test]
fn user_extern_names_do_not_gain_core_intrinsic_semantics() {
    for name in ["add", "__checked_add"] {
        let mut db = HirAnalysisTestDb::default();
        let source = format!(
            "extern {{ const fn {name}(_: usize, _: usize) -> usize }}\nconst fn invoke() -> usize {{ {name}(2, 3) }}"
        );
        let file = db.new_stand_alone(format!("user_extern_{name}.fe").into(), &source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);

        let outcome = eval_body_owner_const(
            &db,
            BodyOwner::Func(function(&db, module, "invoke")),
            vec![],
        );
        assert!(
            matches!(outcome, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::NotConstEvaluable { .. })),
            "a user extern named {name} must be unsupported: {outcome:?}"
        );

        let invoke_owner = BodyOwner::Func(function(&db, module, "invoke"));
        let request = const_computation_for_instance(
            &db,
            identity_semantic_instance_key(&db, invoke_owner),
            Vec::new(),
        );
        let described = describe_const_computation(&db, request, CtfeConfig::default());
        assert!(
            matches!(described, EvalOutcome::Ready(ref desc) if matches!(desc.repr(), ConstRepr::Term(_))),
            "a type-level invocation keeps its identity: {described:?}"
        );
        let forced = force_const_computation(&db, request, CtfeConfig::default());
        assert!(
            matches!(forced, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::NotConstEvaluable { .. })),
            "a required closed invocation must fail: {forced:?}"
        );

        let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
        let expr = ConstExprId::new(
            &db,
            invocation(
                &db,
                function(&db, module, name),
                vec![],
                vec![
                    integer_const_arg(&db, usize_ty, 2),
                    integer_const_arg(&db, usize_ty, 3),
                ],
            ),
        );
        assert!(
            evaluate_type_level_int_const_expr(&db, expr, usize_ty).is_none(),
            "type-level reduction must not infer semantics from {name}"
        );
    }

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "user_extern_size_of.fe".into(),
        "extern { const fn size_of<T>() -> u256 }\nconst fn invoke() -> u256 { size_of<u8>() }",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let outcome = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "invoke")),
        vec![],
    );
    assert!(
        matches!(outcome, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::NotConstEvaluable { .. })),
        "a user extern named size_of must not resolve as the core intrinsic: {outcome:?}"
    );
}

#[test]
fn symbolic_integer_terms_keep_arithmetic_mode() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "arithmetic_mode.fe".into(),
        r#"
#[arithmetic(unchecked)]
const fn wrap<const N: u8>() -> u8 { N + 1 }
const fn check<const N: u8>() -> u8 { N + 1 }
const fn wrapped() -> u8 { wrap<255>() }
const fn checked() -> u8 { check<255>() }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let wrapped = ready(eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "wrapped")),
        vec![],
    ));
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = wrapped.value(&db)
    else {
        panic!("expected integer result, got {:?}", wrapped.value(&db));
    };
    assert_eq!(value.to_u8(), Some(0));
    let checked = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "checked")),
        vec![],
    );
    assert!(
        matches!(checked, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::ArithmeticOverflow { .. })),
        "{checked:?}"
    );

    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let lhs = integer_const_arg(&db, u8_ty, 255);
    let rhs = integer_const_arg(&db, u8_ty, 1);
    let term = |mode| {
        ConstExprId::new(
            &db,
            ConstExpr::ArithBinOp {
                op: ArithBinOp::Add,
                mode,
                lhs,
                rhs,
            },
        )
    };
    let checked = term(ArithmeticMode::Checked);
    let unchecked = term(ArithmeticMode::Unchecked);
    assert_ne!(checked, unchecked);
    assert!(evaluate_type_level_int_const_expr(&db, checked, u8_ty).is_none());
    let wrapped = evaluate_type_level_int_const_expr(&db, unchecked, u8_ty).unwrap();
    assert_eq!(
        wrapped.integer_value(&db).and_then(|value| value.to_u8()),
        Some(0)
    );
}

#[test]
fn immutable_request_replays_after_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "immutable_request.fe".into(),
        "const fn first<const N: usize>() -> u8 { [7 as u8; N][0] }",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "first"));
    let identity = identity_semantic_instance_key(&db, owner);
    let request = const_computation_for_instance(&db, identity, Vec::new());
    let description = describe_const_computation(&db, request, CtfeConfig::default())
        .into_ready()
        .expect("generic request should have a description");
    assert!(matches!(description.repr(), ConstRepr::Deferred(saved) if *saved == request));
    assert!(matches!(
        force_const_computation(&db, request, CtfeConfig::default()),
        EvalOutcome::Blocked(_)
    ));

    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    for (length, expected) in [(0, None), (1, Some(7))] {
        let specialized = specialize_const_computation(
            &db,
            request,
            owner.scope(),
            &[integer_const_arg(&db, usize_ty, length)],
        )
        .unwrap();
        if length == 1 {
            let limited = force_const_computation(
                &db,
                specialized,
                CtfeConfig {
                    step_limit: 0,
                    ..CtfeConfig::default()
                },
            );
            assert!(
                matches!(
                    limited,
                    EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::StepLimitExceeded { .. }))
                ),
                "small-budget result must be separate from the semantic request: {limited:?}"
            );
        }
        let outcome = force_const_computation(&db, specialized, CtfeConfig::default());
        match expected {
            Some(expected) => {
                let EvalOutcome::Ready(value) = outcome else {
                    panic!("specialized request failed: {outcome:?}");
                };
                let SemConstValue::Scalar {
                    value: SemConstScalar::Int { value },
                    ..
                } = value.value().value(&db)
                else {
                    panic!("expected scalar result");
                };
                assert_eq!(value.to_u8(), Some(expected));
            }
            None => assert!(
                matches!(
                    outcome,
                    EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::OutOfBounds { .. }))
                ),
                "zero length must fail: {outcome:?}"
            ),
        }
    }
}

#[test]
fn closed_term_inputs_force_with_their_recorded_arithmetic_mode() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "closed_term_input.fe".into(),
        "const fn identity(value: u8) -> u8 { value }",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "identity"));
    let key = identity_semantic_instance_key(&db, owner);
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let lhs = integer_const_arg(&db, u8_ty, 255);
    let rhs = integer_const_arg(&db, u8_ty, 1);

    for (mode, expected) in [
        (ArithmeticMode::Checked, None),
        (ArithmeticMode::Unchecked, Some(0)),
    ] {
        let term = ConstExprId::new(
            &db,
            ConstExpr::ArithBinOp {
                op: ArithBinOp::Add,
                mode,
                lhs,
                rhs,
            },
        );
        let request = const_computation_for_instance(
            &db,
            key,
            vec![ConstDesc::term(
                &db,
                owner.scope(),
                ConstTyId::new(&db, ConstTyData::Abstract(term, u8_ty)),
            )],
        );
        let outcome = force_const_computation(&db, request, CtfeConfig::default());
        match expected {
            Some(expected) => {
                let EvalOutcome::Ready(value) = outcome else {
                    panic!("closed unchecked term did not force: {outcome:?}");
                };
                let SemConstValue::Scalar {
                    value: SemConstScalar::Int { value },
                    ..
                } = value.value().value(&db)
                else {
                    panic!("expected integer value");
                };
                assert_eq!(value.to_u8(), Some(expected));
            }
            None => assert!(
                matches!(
                    outcome,
                    EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::ArithmeticOverflow { .. }))
                ),
                "closed checked term must retain its overflow: {outcome:?}"
            ),
        }
    }
}

#[test]
fn expression_arguments_preserve_caller_parameter_ownership() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "caller_const_expression_ownership.fe".into(),
        r#"
const fn first<const A: usize, const B: usize>() -> usize { A }
const fn code<const A: usize, const B: usize>() -> usize { A * 10 + B }
const fn mixed<const X: usize, const Y: usize>() -> usize { first<{ Y + 0 }, 1>() }
const fn swapped<const X: usize, const Y: usize>() -> usize { code<{ Y + 0 }, { X + 0 }>() }
const fn repeated<const X: usize, const Y: usize>() -> usize { code<{ Y + 0 }, { Y + 0 }>() }
const fn nested<const X: usize, const Y: usize>() -> usize { mixed<{ X + 0 }, { Y + 0 }>() }
struct Marker<const N: usize> {}
impl<const N: usize> Marker<N> { const fn read() -> usize { N } }
const fn inherited<const X: usize, const Y: usize>() -> usize { Marker<{ Y + 0 }>::read() }
trait Pick<const N: usize> {
    const A: usize = N
    const B: usize = Self::A
}
struct Selected<const M: usize, const N: usize> {}
impl<const M: usize, const N: usize> Pick<N> for Selected<M, N> {}
const fn selected<const X: usize, const Y: usize>() -> usize { Selected<{ X + 0 }, { Y + 0 }>::B }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    for (name, expected) in [
        ("mixed", 7),
        ("swapped", 73),
        ("repeated", 77),
        ("nested", 7),
        ("inherited", 7),
        ("selected", 7),
    ] {
        let owner = BodyOwner::Func(function(&db, module, name));
        assert!(
            matches!(
                eval_body_owner_const(&db, owner, Vec::new()),
                EvalOutcome::Blocked(_)
            ),
            "{name} requires a caller-owned value"
        );
        assert_integer_result(
            &db,
            assert_specialization_law(
                &db,
                owner,
                &[
                    integer_const_arg(&db, usize_ty, 3),
                    integer_const_arg(&db, usize_ty, 7),
                ],
            ),
            usize_ty,
            expected,
        );
    }

    let owner = BodyOwner::Func(function(&db, module, "first"));
    let identity = identity_semantic_instance_key(&db, owner);
    let outer_owner = BodyOwner::Func(function(&db, module, "mixed"));
    let outer = identity_semantic_instance_key(&db, outer_owner);
    let caller_y = outer.subst(&db).generic_args(&db)[1];
    let instance = get_or_build_semantic_instance(
        &db,
        SemanticInstanceKey::new(
            &db,
            owner,
            GenericSubst::new(&db, vec![caller_y, integer_const_arg(&db, usize_ty, 1)]),
            identity.effect_providers(&db),
            identity.impl_env(&db),
        ),
    );
    for (parameter, template) in [
        (identity.subst(&db).generic_args(&db)[0], true),
        (caller_y, false),
    ] {
        let term = ConstTyId::new(
            &db,
            ConstTyData::Abstract(
                ConstExprId::new(
                    &db,
                    ConstExpr::ArithBinOp {
                        op: ArithBinOp::Add,
                        mode: ArithmeticMode::Checked,
                        lhs: parameter,
                        rhs: integer_const_arg(&db, usize_ty, 0),
                    },
                ),
                usize_ty,
            ),
        );
        let value = SemConstId::new(&db, SemConstValue::Description(term));
        let result = if template {
            reify_runtime_const(&db, instance, value)
        } else {
            reify_runtime_const_for_ty(&db, instance, usize_ty, value)
        };
        assert!(
            result.is_none(),
            "unresolved caller Y must not reify as the callee's B"
        );
    }
}

#[test]
fn staged_request_specialization_preserves_parameter_owners() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "staged_request.fe".into(),
        r#"
const fn code<const A: usize, const B: usize>() -> usize { A * 10 + B }
fn outer<const X: usize, const Y: usize>() {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let code_owner = BodyOwner::Func(function(&db, module, "code"));
    let outer_owner = BodyOwner::Func(function(&db, module, "outer"));
    let template = const_computation_for_instance(
        &db,
        identity_semantic_instance_key(&db, code_owner),
        Vec::new(),
    );
    let outer = identity_semantic_instance_key(&db, outer_owner);
    let params = outer.subst(&db).generic_args(&db);
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let values = [3, 7].map(|value| integer_const_arg(&db, usize_ty, value));

    for (mapping, direct_args, expected) in [
        ([params[1], params[0]], [values[1], values[0]], 73),
        ([params[0], params[0]], [values[0], values[0]], 33),
    ] {
        let first =
            specialize_const_computation(&db, template, outer_owner.scope(), &mapping).unwrap();
        assert!(matches!(
            force_const_computation(&db, first, CtfeConfig::default()),
            EvalOutcome::Blocked(_)
        ));
        let staged =
            specialize_const_computation(&db, first, outer_owner.scope(), &values).unwrap();
        let direct =
            specialize_const_computation(&db, template, code_owner.scope(), &direct_args).unwrap();
        let staged = force_const_computation(&db, staged, CtfeConfig::default());
        let direct = force_const_computation(&db, direct, CtfeConfig::default());
        assert_eq!(staged, direct, "specialization law");
        let EvalOutcome::Ready(value) = staged else {
            panic!("expected staged ready value: {staged:?}");
        };
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.value().value(&db)
        else {
            panic!("expected scalar value");
        };
        assert_eq!(value.to_u32(), Some(expected));
    }
}

#[test]
fn whole_expression_terms_preserve_generic_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "generic_identity_body.fe".into(),
        "const fn identity<const N: usize>() -> usize { N }\nconst fn increment<const N: usize>() -> usize { N + 1 }\nconst fn plus1(_ x: usize) -> usize { x + 1 }\nconst fn call<const N: usize>() -> usize { plus1(N) }",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for name in ["identity", "increment", "call"] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let key = identity_semantic_instance_key(&db, owner);
        let request = const_computation_for_instance(&db, key, Vec::new());
        let description = describe_const_computation(&db, request, CtfeConfig::default())
            .into_ready()
            .expect("generic body has a description");
        match (name, description.repr()) {
            ("identity", ConstRepr::Term(term)) => {
                assert!(matches!(term.data(&db), ConstTyData::TyParam(..)));
            }
            ("increment" | "call", ConstRepr::Term(term)) => {
                let ConstTyData::Abstract(expr, _) = term.data(&db) else {
                    panic!("increment must retain an arithmetic term: {description:?}");
                };
                assert!(matches!(
                    expr.data(&db),
                    ConstExpr::ArithBinOp {
                        op: ArithBinOp::Add,
                        mode: ArithmeticMode::Checked,
                        ..
                    }
                ));
            }
            _ => panic!("{name} did not retain a whole-expression term: {description:?}"),
        }
    }
}

#[test]
fn repeated_invocations_share_identity_after_scope_transfer() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "repeated_invocations.fe".into(),
        "const fn value<const N: usize>() -> usize { if N == 0 { 1 / 0 } else { N } }\nconst fn left<const N: usize>() -> usize { value<N>() }\nconst fn right<const N: usize>() -> usize { value<N>() }\nfn outer<const X: usize, const Y: usize>() {}",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let outer_owner = BodyOwner::Func(function(&db, module, "outer"));
    let outer_key = identity_semantic_instance_key(&db, outer_owner);
    let params = outer_key.subst(&db).generic_args(&db);
    let describe = |name, arg| {
        let owner = BodyOwner::Func(function(&db, module, name));
        let request = const_computation_for_instance(
            &db,
            identity_semantic_instance_key(&db, owner),
            Vec::new(),
        );
        let request =
            specialize_const_computation(&db, request, outer_owner.scope(), &[arg]).unwrap();
        let description = describe_const_computation(&db, request, CtfeConfig::default())
            .into_ready()
            .expect("generic invocation has a description");
        let ConstRepr::Term(term) = description.repr() else {
            panic!("expected an invocation term: {description:?}");
        };
        let ConstTyData::Abstract(expr, _) = term.data(&db) else {
            panic!("expected an invocation expression: {description:?}");
        };
        let ConstExpr::Invocation(invocation) = expr.data(&db) else {
            panic!("expected a canonical invocation: {description:?}");
        };
        assert_eq!(invocation.parameter_owner, outer_owner.scope());
        assert_eq!(
            invocation.key.owner(&db),
            BodyOwner::Func(function(&db, module, "value"))
        );
        (*term, description)
    };
    let (left_x, left_description) = describe("left", params[0]);
    let (right_x, right_description) = describe("right", params[0]);
    let (right_y, _) = describe("right", params[1]);
    assert_eq!(left_x, right_x, "repeated calls must share an identity");
    assert_ne!(left_x, right_y, "argument identity must be retained");
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let zero = integer_const_arg(&db, usize_ty, 0);
    for description in [left_description, right_description] {
        let specialized = specialize_const_description(
            &db,
            &description,
            outer_owner.scope(),
            outer_owner.scope(),
            &[zero, zero],
            SemOrigin::Synthetic,
        )
        .expect("equal terms should remain specializable");
        let outcome = force_const_description(
            &db,
            &specialized,
            CtfeConfig::default(),
            SemOrigin::Synthetic,
        );
        assert!(
            matches!(outcome, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::DivisionByZero { .. })),
            "equal type-level identities must not erase the reached fault: {outcome:?}"
        );
    }
}

#[test]
fn ctfe_typed_read_through_frame_local_borrow_evaluates() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "ctfe_typed_read.fe".into(),
        "const fn read(_ value: ref u256) -> u256 { value }\nconst fn caller() -> u256 { let value: u256 = 7\n read(ref value) }",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let read = function(&db, module, "read");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(read)),
    );
    let body = instance.body(&db);
    assert!(
        body.blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .any(|stmt| {
                matches!(
                    &stmt.kind,
                    SStmtKind::Assign {
                        expr: SExpr::UseValue(_),
                        ..
                    }
                )
            })
    );
    let outcome = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "caller")),
        vec![],
    );
    // `ref value` borrows a local of the caller's own frame, so the typed read
    // in `read` loads it. Borrows of memory outside CTFE frames stay rejected
    // (`borrow_through_pointer_is_rejected` in the machine tests).
    assert_integer_result(&db, outcome, TyId::u256(&db), 7);
}

#[test]
fn runtime_reification_rejects_mismatched_scalar_and_array_element_types() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "reification_type_verification.fe".into(),
        "const fn anchor() -> [u8; 1] { [1] }\nconst fn zero() -> [u8; 0] { [] }",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "anchor"));
    let key = identity_semantic_instance_key(&db, owner);
    let instance = get_or_build_semantic_instance(&db, key);
    let array_ty = key.typed_body(&db).result_ty();
    let zero_array_ty =
        identity_semantic_instance_key(&db, BodyOwner::Func(function(&db, module, "zero")))
            .typed_body(&db)
            .result_ty();
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let u16_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U16)));
    let u8_value = int_const(&db, u8_ty, BigInt::from(1));
    let u16_value = int_const(&db, u16_ty, BigInt::from(1));
    assert!(reify_runtime_const_for_ty(&db, instance, u16_ty, u8_value).is_none());
    let viewed_u8 = int_const(&db, TyId::view_of(&db, u8_ty), BigInt::from(1));
    assert!(reify_runtime_const_for_ty(&db, instance, u8_ty, viewed_u8).is_some());
    assert!(reify_runtime_const_for_ty(&db, instance, u16_ty, viewed_u8).is_none());
    assert!(
        reify_runtime_const_for_ty(
            &db,
            instance,
            array_ty,
            array_const(&db, array_ty, vec![u16_value].into_boxed_slice()),
        )
        .is_none()
    );
    let expected = array_const(&db, array_ty, vec![u8_value].into_boxed_slice());
    assert_eq!(
        reify_runtime_const_for_ty(&db, instance, array_ty, expected),
        Some(expected)
    );
    let empty_tuple = tuple_const(&db, TyId::unit(&db), Box::new([]));
    assert!(reify_runtime_const_for_ty(&db, instance, zero_array_ty, empty_tuple).is_none());
    let empty_array = array_const(&db, zero_array_ty, Box::new([]));
    assert_eq!(
        reify_runtime_const_for_ty(&db, instance, zero_array_ty, empty_array),
        Some(empty_array)
    );
}

#[test]
fn term_extraction_bounds_expanding_recursion() {
    const CHILD: &str = "FE_CTFE_EXTRACTION_CHILD";
    if var_os(CHILD).is_none() {
        let output = Command::new(current_exe().unwrap())
            .args([
                "--exact",
                "term_extraction_bounds_expanding_recursion",
                "--nocapture",
            ])
            .env(CHILD, "1")
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "extraction subprocess failed: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        return;
    }

    for source in [
        "const fn recur<T>() -> usize { recur<(T,)>() }",
        "const fn recur<T>() -> usize { other<(T,)>() }\nconst fn other<T>() -> usize { recur<(T,)>() }",
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("expanding_recursion.fe".into(), source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let owner = BodyOwner::Func(function(&db, module, "recur"));
        let request = const_computation_for_instance(
            &db,
            identity_semantic_instance_key(&db, owner),
            Vec::new(),
        );
        let config = CtfeConfig {
            recursion_limit: 4,
            ..CtfeConfig::default()
        };
        let direct = force_const_computation(&db, request, config.clone());
        assert!(
            matches!(direct, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::RecursionLimitExceeded { .. })),
            "{direct:?}"
        );
        let described = describe_const_computation(&db, request, config);
        assert!(
            matches!(described, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::RecursionLimitExceeded { .. })),
            "{described:?}"
        );

        let mut db = HirAnalysisTestDb::default();
        let source = format!("{source}\ntype Alias = [u8; recur<u8>()]");
        let file = db.new_stand_alone("expanding_alias.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        let diagnostics = module.all_type_aliases(&db)[0].diags(&db);
        assert!(
            diagnostics.iter().any(|diag| matches!(
                diag,
                TyDiagCollection::Ty(TyLowerDiag::ConstEvalRecursionLimitExceeded(..))
            )),
            "{diagnostics:?}"
        );
    }
    // A finite chain can exceed the extractor's depth cap and still execute.
    let mut source = String::new();
    for (caller, callee) in (0..80).zip(1..=80) {
        source.push_str(&format!(
            "const fn f{caller}<const N: usize>() -> usize {{ f{callee}<N>() }}\n"
        ));
    }
    source.push_str("const fn f80<const N: usize>() -> usize { N }\n");
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("finite_chain.fe".into(), &source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "f0"));
    let request =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, owner), Vec::new());
    let config = CtfeConfig {
        recursion_limit: 128,
        ..CtfeConfig::default()
    };
    let description = describe_const_computation(&db, request, config.clone())
        .into_ready()
        .unwrap();
    assert!(matches!(
        force_const_description(&db, &description, config.clone(), SemOrigin::Body(owner)),
        EvalOutcome::Blocked(_)
    ));
    let ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let args = [TyId::new(
        &db,
        TyData::ConstTy(ConstTyId::integer(&db, ty, BigInt::from(7))),
    )];
    let specialized = specialize_const_description(
        &db,
        &description,
        owner.scope(),
        owner.scope(),
        &args,
        SemOrigin::Body(owner),
    )
    .unwrap();
    let replayed =
        force_const_description(&db, &specialized, config.clone(), SemOrigin::Body(owner))
            .into_ready()
            .unwrap();
    assert_eq!(replayed.value(), int_const(&db, ty, BigInt::from(7)));
    let specialized_request =
        specialize_const_computation(&db, request, owner.scope(), &args).unwrap();
    let direct = force_const_computation(&db, specialized_request, config)
        .into_ready()
        .unwrap();
    assert_same_value_tree(&db, replayed.value(), direct.value());
}

#[test]
fn specialized_alias_terms_preserve_faults_and_source_order() {
    for (declaration, argument, expression, overflow) in [
        (
            "type Generic<const N: usize> = [u8; { N / 0 }]",
            "1",
            "N / 0",
            false,
        ),
        (
            "struct Tiny<const N: u8> {}\ntype Generic<const N: u8> = Tiny<{ N + 1 }>",
            "255",
            "N + 1",
            true,
        ),
        (
            "struct Tiny<const N: i8> {}\ntype Generic<const N: i8> = Tiny<{ -N }>",
            "{ -128 }",
            "-N",
            true,
        ),
        (
            "struct Tiny<const N: u8> {}\ntype Generic<const N: u8> = Tiny<{ (N / 0) + (255 + N) }>",
            "1",
            "N / 0",
            false,
        ),
        (
            "struct Tiny<const N: u8> {}\ntype Generic<const N: u8> = Tiny<{ (255 + N) + (N / 0) }>",
            "1",
            "255 + N",
            true,
        ),
        (
            "const fn nested<const N: usize>() -> usize { N / 0 }\ntype Generic<const N: usize> = [u8; nested<N>()]",
            "1",
            "N / 0",
            false,
        ),
        (
            "type Inner<const N: usize> = [u8; { N / 0 }]\ntype Generic<const N: usize> = Inner<{ N + 0 }>",
            "1",
            "N / 0",
            false,
        ),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let generic_file = db.new_stand_alone("generic_alias.fe".into(), declaration);
        let (generic_module, _) = db.top_mod(generic_file);
        db.assert_no_diags(generic_module);

        let mut db = HirAnalysisTestDb::default();
        let source = format!(
            "{declaration}\ntype Bad = Generic<{argument}>\nfn consume(_ value: Bad) {{}}\n"
        );
        let file = db.new_stand_alone("specialized_alias.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        let alias = module.all_type_aliases(&db).iter().copied().find(|alias| {
            matches!(alias.name(&db), Partial::Present(name) if name.data(&db) == "Bad")
        }).unwrap();
        let diagnostics = alias.diags(&db);
        let span = diagnostics
            .iter()
            .find_map(|diag| match diag {
                TyDiagCollection::Ty(TyLowerDiag::ConstEvalArithmeticOverflow(span))
                    if overflow =>
                {
                    Some(span)
                }
                TyDiagCollection::Ty(TyLowerDiag::ConstEvalDivisionByZero(span)) if !overflow => {
                    Some(span)
                }
                _ => None,
            })
            .unwrap_or_else(|| {
                panic!("missing specific failure for {declaration}: {diagnostics:?}")
            });
        let span = span
            .resolve(&db)
            .expect("failure must retain its source occurrence");
        assert_eq!(
            &source[usize::from(span.range.start())..usize::from(span.range.end())],
            expression,
            "{declaration}"
        );
    }
}

#[test]
fn retained_terms_force_specialize_and_reify() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "retained_terms.fe".into(),
        r#"
const fn repeat<const V: u8, const L: usize>() -> [u8; L] { [V; L] }
const fn pair() -> (u8, u8) { (1, 2) }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "repeat"));
    let key = identity_semantic_instance_key(&db, owner);
    let array_ty = key.typed_body(&db).result_ty();
    let pair_ty =
        identity_semantic_instance_key(&db, BodyOwner::Func(function(&db, module, "pair")))
            .typed_body(&db)
            .result_ty();
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let u16_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U16)));
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let [value, len] = key.subst(&db).generic_args(&db).as_slice() else {
        panic!("repeat has two parameters")
    };
    let wrap = |term| TyId::new(&db, TyData::ConstTy(term));
    let term =
        |ty, expr| ConstTyId::new(&db, ConstTyData::Abstract(ConstExprId::new(&db, expr), ty));
    let repeat = term(
        array_ty,
        ConstExpr::ArrayRepeat {
            value: *value,
            len: *len,
        },
    );
    let cast = term(
        u16_ty,
        ConstExpr::Cast {
            expr: *value,
            to: u16_ty,
        },
    );
    let index = term(
        u8_ty,
        ConstExpr::ArrayIndex {
            array: wrap(repeat),
            index: integer_const_arg(&db, usize_ty, 1),
        },
    );
    let TyData::ConstTy(value_term) = value.data(&db) else {
        unreachable!()
    };
    let pair = ConstTyId::new(
        &db,
        ConstTyData::Description(tuple_const(
            &db,
            pair_ty,
            vec![
                int_const(&db, u8_ty, BigInt::from(3)),
                SemConstId::new(&db, SemConstValue::Description(*value_term)),
            ]
            .into_boxed_slice(),
        )),
    );
    let field = term(
        u8_ty,
        ConstExpr::Field {
            value: wrap(pair),
            index: 1,
        },
    );
    let args = [
        integer_const_arg(&db, u8_ty, 7),
        integer_const_arg(&db, usize_ty, 2),
    ];
    let instance = get_or_build_semantic_instance(
        &db,
        SemanticInstanceKey::new(
            &db,
            owner,
            GenericSubst::new(&db, args.to_vec()),
            key.effect_providers(&db),
            key.impl_env(&db),
        ),
    );
    for (name, retained) in [
        ("cast", cast),
        ("repeat", repeat),
        ("index", index),
        ("field", field),
    ] {
        let description = ConstDesc::term(&db, owner.scope(), retained);
        assert!(
            matches!(
                force_const_description(
                    &db,
                    &description,
                    CtfeConfig::default(),
                    SemOrigin::Body(owner)
                ),
                EvalOutcome::Blocked(_)
            ),
            "{name} must preserve its dependency"
        );
        let specialized = specialize_const_description(
            &db,
            &description,
            owner.scope(),
            owner.scope(),
            &args,
            SemOrigin::Body(owner),
        )
        .unwrap();
        let forced = force_const_description(
            &db,
            &specialized,
            CtfeConfig::default(),
            SemOrigin::Body(owner),
        )
        .into_ready()
        .unwrap_or_else(|| panic!("{name} must force"));
        assert_eq!(sem_const_ty(&db, forced.value()), specialized.ty());
        if name == "repeat" {
            let SemConstValue::Array { elems, .. } = forced.value().value(&db) else {
                panic!("repeat must produce an array")
            };
            assert_eq!(elems.as_ref(), &[int_const(&db, u8_ty, BigInt::from(7)); 2]);
        } else {
            let SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } = forced.value().value(&db)
            else {
                panic!("projection or cast must produce an integer")
            };
            assert_eq!(value, BigInt::from(7));
        }
        let ConstRepr::Term(retained) = specialized.repr() else {
            panic!("specialization retains a term before forcing")
        };
        let transported = SemConstId::new(&db, SemConstValue::Description(*retained));
        let reified = reify_runtime_const_for_ty(&db, instance, specialized.ty(), transported)
            .unwrap_or_else(|| panic!("{name} must reify through the same computation"));
        assert_same_value_tree(&db, forced.value(), reified);
    }

    let bad = term(
        u8_ty,
        ConstExpr::ArithBinOp {
            op: ArithBinOp::Div,
            mode: ArithmeticMode::Checked,
            lhs: *value,
            rhs: integer_const_arg(&db, u8_ty, 0),
        },
    );
    let bad_repeat = term(
        array_ty,
        ConstExpr::ArrayRepeat {
            value: wrap(bad),
            len: *len,
        },
    );
    let bad_pair = ConstTyId::new(
        &db,
        ConstTyData::Description(tuple_const(
            &db,
            pair_ty,
            vec![
                int_const(&db, u8_ty, BigInt::from(3)),
                SemConstId::new(&db, SemConstValue::Description(bad)),
            ]
            .into_boxed_slice(),
        )),
    );
    let unused_bad_field = term(
        u8_ty,
        ConstExpr::Field {
            value: wrap(bad_pair),
            index: 0,
        },
    );
    let bad_index = term(
        u8_ty,
        ConstExpr::ArrayIndex {
            array: wrap(repeat),
            index: integer_const_arg(&db, usize_ty, 2),
        },
    );
    let bad_cast = term(
        u16_ty,
        ConstExpr::Cast {
            expr: wrap(bad),
            to: u16_ty,
        },
    );
    for (retained, length, bounds) in [
        (bad_repeat, 0, false),
        (unused_bad_field, 2, false),
        (bad_index, 2, true),
        (bad_cast, 2, false),
    ] {
        let description = ConstDesc::term(&db, owner.scope(), retained);
        let args = [
            integer_const_arg(&db, u8_ty, 7),
            integer_const_arg(&db, usize_ty, length),
        ];
        let specialized = specialize_const_description(
            &db,
            &description,
            owner.scope(),
            owner.scope(),
            &args,
            SemOrigin::Body(owner),
        )
        .unwrap();
        let outcome = force_const_description(
            &db,
            &specialized,
            CtfeConfig::default(),
            SemOrigin::Body(owner),
        );
        assert!(
            matches!(outcome, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if if bounds { matches!(root_error(error), CtfeError::OutOfBounds { .. }) } else { matches!(root_error(error), CtfeError::DivisionByZero { .. }) }),
            "{outcome:?}"
        );
    }
}

#[test]
fn retained_terms_obey_step_and_depth_limits() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("term_limits.fe".into(), "const fn anchor() -> usize { 0 }");
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "anchor"));
    let ty = identity_semantic_instance_key(&db, owner)
        .typed_body(&db)
        .result_ty();
    let one = integer_const_arg(&db, ty, 1);
    let add = |lhs| {
        ConstTyId::new(
            &db,
            ConstTyData::Abstract(
                ConstExprId::new(
                    &db,
                    ConstExpr::ArithBinOp {
                        op: ArithBinOp::Add,
                        mode: ArithmeticMode::Checked,
                        lhs,
                        rhs: one,
                    },
                ),
                ty,
            ),
        )
    };
    let single = ConstDesc::term(&db, owner.scope(), add(one));
    let outcome = force_const_description(
        &db,
        &single,
        CtfeConfig {
            step_limit: 0,
            recursion_limit: 64,
        },
        SemOrigin::Body(owner),
    );
    assert!(
        matches!(
            outcome,
            EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::StepLimitExceeded { .. }))
        ),
        "zero budget: {outcome:?}"
    );
    let deep = (0..32).fold(one, |lhs, _| TyId::new(&db, TyData::ConstTy(add(lhs))));
    let TyData::ConstTy(deep) = deep.data(&db) else {
        unreachable!()
    };
    let deep = ConstDesc::term(&db, owner.scope(), *deep);
    let outcome = force_const_description(
        &db,
        &deep,
        CtfeConfig {
            step_limit: 1000,
            recursion_limit: 4,
        },
        SemOrigin::Body(owner),
    );
    assert!(
        matches!(
            outcome,
            EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::RecursionLimitExceeded { .. }))
        ),
        "depth budget: {outcome:?}"
    );
    let outcome =
        force_const_description(&db, &deep, CtfeConfig::default(), SemOrigin::Body(owner));
    assert_integer_result(
        &db,
        EvalOutcome::Ready(outcome.into_ready().unwrap().value()),
        ty,
        33,
    );

    let TyData::ConstTy(one_term) = one.data(&db) else {
        unreachable!()
    };
    let tuple = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Tuple(1))));
    let (_, nested) = (0..32).fold(
        (
            ty,
            SemConstId::new(&db, SemConstValue::Description(*one_term)),
        ),
        |(ty, value), _| {
            let ty = TyId::app(&db, tuple, ty);
            (ty, tuple_const(&db, ty, vec![value].into_boxed_slice()))
        },
    );
    let description = ConstDesc::term(
        &db,
        owner.scope(),
        ConstTyId::new(&db, ConstTyData::Description(nested)),
    );
    let limited = force_const_description(
        &db,
        &description,
        CtfeConfig {
            step_limit: 1000,
            recursion_limit: 4,
        },
        SemOrigin::Body(owner),
    );
    assert!(
        matches!(
            limited,
            EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::RecursionLimitExceeded { .. }))
        ),
        "partial aggregate depth: {limited:?}"
    );
    let value = force_const_description(
        &db,
        &description,
        CtfeConfig::default(),
        SemOrigin::Body(owner),
    )
    .into_ready()
    .unwrap();
    assert_eq!(sem_const_ty(&db, value.value()), description.ty());
    let mut leaf = value.value();
    while let SemConstValue::Tuple { elems, .. } = leaf.value(&db) {
        assert_eq!(elems.len(), 1);
        leaf = elems[0];
    }
    assert_integer_result(&db, EvalOutcome::Ready(leaf), ty, 1);
}

#[test]
fn term_invocations_share_budget_even_when_cached() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "term_call_limits.fe".into(),
        r#"
const fn work() -> usize {
    let mut i: usize = 0
    while i < 20 { i += 1 }
    i
}
const fn sum(x: usize, y: usize) -> usize { x + y }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let work = BodyOwner::Func(function(&db, module, "work"));
    let sum = BodyOwner::Func(function(&db, module, "sum"));
    let request =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, work), Vec::new());
    let force = |request, steps| {
        force_const_computation(
            &db,
            request,
            CtfeConfig {
                step_limit: steps,
                ..CtfeConfig::default()
            },
        )
    };
    let (mut low, mut high) = (0, 2000);
    assert!(matches!(force(request, high), EvalOutcome::Ready(_)));
    while high - low > 1 {
        let middle = (low + high) / 2;
        if matches!(force(request, middle), EvalOutcome::Ready(_)) {
            high = middle;
        } else {
            low = middle;
        }
    }
    let single_cost = high;
    let combined = const_computation_for_instance(
        &db,
        identity_semantic_instance_key(&db, sum),
        vec![
            ConstDesc::deferred(&db, request),
            ConstDesc::deferred(&db, request),
        ],
    );
    for _ in 0..2 {
        let outcome = force(combined, single_cost + 5);
        assert!(
            matches!(outcome, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::StepLimitExceeded { .. })),
            "sibling calls reset their budget: {outcome:?}"
        );
        let result = force(combined, single_cost * 2 + 100).into_ready().unwrap();
        assert_integer_result(
            &db,
            EvalOutcome::Ready(result.value()),
            combined.result_ty(&db),
            40,
        );
    }
}

#[test]
fn retained_aggregate_operations_check_operands_and_allocation() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "retained_aggregate_limits.fe".into(),
        r#"
const fn pair() -> (u8, u8) { (1, 2) }
const fn empty() -> [u8; 0] { [7; 0] }
const fn large() -> [u8; 1000] { [7; 1000] }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let pair = BodyOwner::Func(function(&db, module, "pair"));
    let result_ty = |name| {
        identity_semantic_instance_key(&db, BodyOwner::Func(function(&db, module, name)))
            .typed_body(&db)
            .result_ty()
    };
    let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
    let u16_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U16)));
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let wrong = tuple_const(
        &db,
        result_ty("pair"),
        vec![
            int_const(&db, u8_ty, 7.into()),
            int_const(&db, u16_ty, 8.into()),
        ]
        .into_boxed_slice(),
    );
    let field = ConstExpr::Field {
        value: TyId::new(
            &db,
            TyData::ConstTy(ConstTyId::new(&db, ConstTyData::Description(wrong))),
        ),
        index: 0,
    };
    let repeat = |element_ty, len| ConstExpr::ArrayRepeat {
        value: integer_const_arg(&db, element_ty, 7),
        len: integer_const_arg(&db, usize_ty, len),
    };
    for (expr, ty, allocation_failure) in [
        (field, u8_ty, false),
        (repeat(u16_ty, 0), result_ty("empty"), false),
        (repeat(u8_ty, 1000), result_ty("large"), true),
    ] {
        let description = ConstDesc::term(
            &db,
            pair.scope(),
            ConstTyId::new(&db, ConstTyData::Abstract(ConstExprId::new(&db, expr), ty)),
        );
        let outcome = force_const_description(
            &db,
            &description,
            CtfeConfig {
                step_limit: 100,
                ..CtfeConfig::default()
            },
            SemOrigin::Body(pair),
        );
        if allocation_failure {
            assert!(
                matches!(
                    outcome,
                    EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::StepLimitExceeded { .. }))
                ),
                "allocation must be bounded: {outcome:?}"
            );
            assert!(matches!(
                force_const_description(
                    &db,
                    &description,
                    CtfeConfig::default(),
                    SemOrigin::Body(pair)
                ),
                EvalOutcome::Ready(_)
            ));
        } else {
            assert!(
                matches!(outcome, EvalOutcome::Failed(EvalFailure::Invariant { .. })),
                "operand type mismatch must survive projection/zero extent: {outcome:?}"
            );
        }
    }
}

#[test]
fn selected_const_repeat_length_keeps_its_trait_projection() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "selected_repeat_length.fe".into(),
        r#"
use core::AsBytes
const fn digits(value: usize) -> usize {
    let mut remaining = value
    let mut digits: usize = 1
    while remaining >= 10 {
        remaining /= 10
        digits += 1
    }
    digits
}
struct Suffix<const LEN: usize> {}
impl<const LEN: usize> AsBytes for Suffix<LEN> {
    const N: usize = digits(value: LEN) + 2
    const fn as_bytes(self) -> [u8; Self::N] {
        let mut out: [u8; Self::N] = [0; Self::N]
        out[0] = 91
        out
    }
}

struct Forward<const LEN: usize> {}
impl<const LEN: usize> AsBytes for Forward<LEN> {
    const N: usize = LEN
    const fn as_bytes(self) -> [u8; Self::N] { [0; LEN] }
}
struct One<T> { value: T }
impl<T: AsBytes> AsBytes for One<T> {
    const N: usize = T::N
    const fn as_bytes(self) -> [u8; Self::N] { self.value.as_bytes() }
}
const fn bare() -> [u8; 3] { Forward<3> {}.as_bytes() }
const fn projected() -> [u8; 3] { One { value: [5 as u8; 3] }.as_bytes() }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for (name, expected) in [("bare", 0), ("projected", 5)] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let value = ready(eval_body_owner_const(&db, owner, Vec::new()));
        let SemConstValue::Array { elems, .. } = value.value(&db) else {
            panic!("expected array")
        };
        assert_eq!(elems.len(), 3);
        for element in elems.iter().copied() {
            assert_integer_result(
                &db,
                EvalOutcome::Ready(element),
                sem_const_ty(&db, element),
                expected,
            );
        }
    }
    // Executing this exact stdlib pattern encounters the separately tracked
    // local-borrow admission restriction. This regression checks the accepted
    // generic declaration and its trait-projection identity.
}

#[test]
fn body_demands_distinguish_dependency_failure_and_resource_limits() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "body_repeat_limits.fe".into(),
        r#"
const fn repeat<const N: usize>() -> [u8; N] { [7; N] }
const fn scalar<const N: usize>() -> usize { N }
extern { const fn extent() -> usize }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "repeat"));
    let template =
        const_computation_for_instance(&db, identity_semantic_instance_key(&db, owner), Vec::new());
    assert!(matches!(
        force_const_computation(&db, template, CtfeConfig::default()),
        EvalOutcome::Blocked(_)
    ));
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let opaque = ConstExprId::new(
        &db,
        invocation(&db, function(&db, module, "extent"), Vec::new(), Vec::new()),
    );
    let opaque = TyId::new(
        &db,
        TyData::ConstTy(ConstTyId::new(&db, ConstTyData::Abstract(opaque, usize_ty))),
    );
    let request = specialize_const_computation(&db, template, owner.scope(), &[opaque]).unwrap();
    let outcome = force_const_computation(&db, request, CtfeConfig::default());
    assert!(
        matches!(outcome, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::NotConstEvaluable { .. })),
        "closed unsupported extent is not a dependency: {outcome:?}"
    );
    let scalar_owner = BodyOwner::Func(function(&db, module, "scalar"));
    let scalar = const_computation_for_instance(
        &db,
        identity_semantic_instance_key(&db, scalar_owner),
        Vec::new(),
    );
    let scalar =
        specialize_const_computation(&db, scalar, scalar_owner.scope(), &[opaque]).unwrap();
    let outcome = force_const_computation(&db, scalar, CtfeConfig::default());
    assert!(
        matches!(outcome, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::NotConstEvaluable { .. })),
        "closed scalar description is not a dependency: {outcome:?}"
    );
    for length in [BigInt::from(1000), BigInt::from(usize::MAX) + 1] {
        let length = TyId::new(
            &db,
            TyData::ConstTy(ConstTyId::integer(&db, usize_ty, length)),
        );
        let request =
            specialize_const_computation(&db, template, owner.scope(), &[length]).unwrap();
        let outcome = force_const_computation(
            &db,
            request,
            CtfeConfig {
                step_limit: 100,
                ..CtfeConfig::default()
            },
        );
        assert!(
            matches!(outcome, EvalOutcome::Failed(EvalFailure::Ctfe(ref error)) if matches!(root_error(error), CtfeError::StepLimitExceeded { .. })),
            "repeat allocation is bounded: {outcome:?}"
        );
    }
}
