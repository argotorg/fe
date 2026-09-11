use std::collections::VecDeque;

use cranelift_entity::EntityRef;
use fe_hir::diagnosable::Diagnosable;
use fe_hir::test_db::{HirAnalysisTestDb, find_func};
use fe_hir::{
    analysis::{
        semantic::{
            CtfeError, EvalFailure, EvalOutcome, SConst, SExpr, SStmtKind, SemConstId,
            SemConstScalar, SemConstValue, canonicalize_semantic_consts, eval_body_owner_const,
            eval_body_owner_const_with_args, get_or_build_semantic_instance,
            identity_semantic_instance_key, reify_runtime_const_for_ty,
        },
        ty::{
            diagnostics::{BodyDiag, FuncBodyDiag, TyDiagCollection, TyLowerDiag},
            ty_check::{BodyOwner, check_func_body},
        },
    },
    hir_def::{ItemKind, Partial},
    span::LazySpan,
};
use num_traits::ToPrimitive;

/// Reads a fixture from `test_files/semantic_ctfe`, returning its real path
/// (so the file gets a real `file:` URL) and its text.
fn semantic_ctfe_fixture(name: &str) -> (camino::Utf8PathBuf, String) {
    let path = camino::Utf8PathBuf::from(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/test_files/semantic_ctfe"
    ))
    .join(name);
    let text = std::fs::read_to_string(&path).expect("fixture should be readable");
    (path, text)
}

/// Each fixture's `answer` borrows one of its own frame locals, possibly
/// through a call, and evaluates to 42.
#[dir_test::dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/semantic_ctfe/frame_local_borrows",
    glob: "*.fe"
)]
fn semantic_ctfe_evaluates_frame_local_borrows(fixture: dir_test::Fixture<&str>) {
    use fe_hir::analysis::ty::provider::ProviderAddressSpace;
    use fe_hir::projection::Projection;

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(fixture.path().into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let func = find_func(&db, top_mod, "answer");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let borrows_frame_local = instance
        .body(&db)
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .any(|stmt| {
            matches!(
                &stmt.kind,
                SStmtKind::Assign {
                    expr: SExpr::Borrow {
                        provider: Some(ProviderAddressSpace::Memory),
                        place,
                        ..
                    },
                    ..
                } if !place.path.iter().any(|projection| matches!(projection, Projection::Deref))
            )
        });
    assert!(borrows_frame_local, "`answer` must borrow a frame local");
    let value = match eval_body_owner_const(&db, BodyOwner::Func(func), vec![]) {
        EvalOutcome::Ready(value) => value,
        outcome => panic!("{outcome:?}"),
    };
    assert_eq!(value.pretty_print(&db), "42");
}

#[test]
fn semantic_ctfe_rejects_references_to_a_returning_frame() {
    let mut db = HirAnalysisTestDb::default();
    let (path, text) = semantic_ctfe_fixture("returning_frame_borrow.fe");
    let file = db.new_stand_alone(path, &text);
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "use_dangling");
    // Even direct evaluator clients that omit borrow analysis must get an
    // error, not a stale frame reference or a panic when that reference is read.
    let EvalOutcome::Failed(EvalFailure::Ctfe(error)) =
        eval_body_owner_const(&db, BodyOwner::Func(func), vec![])
    else {
        panic!("a dangling frame reference must fail evaluation");
    };
    // The failure happens in the callee and reaches the caller wrapped.
    let mut root = &error;
    while let CtfeError::CalleeError { source, .. } = root {
        root = &**source;
    }
    assert!(matches!(root, CtfeError::InvalidBorrow { .. }), "{error:?}");
}

#[test]
fn semantic_ctfe_preserves_const_and_anonymous_result_modes() {
    let mut db = HirAnalysisTestDb::default();
    let (path, text) = semantic_ctfe_fixture("const_borrow_result_modes.fe");
    let file = db.new_stand_alone(path, &text);
    let (top_mod, _) = db.top_mod(file);
    let constants = top_mod
        .all_items(&db)
        .iter()
        .copied()
        .filter_map(|item| match item {
            ItemKind::Const(const_) => Some(const_),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(constants.len(), 2);
    for const_ in constants {
        let expected = const_.ty(&db);
        for owner in [
            BodyOwner::Const(const_),
            BodyOwner::AnonConstBody {
                body: const_.body(&db).to_opt().unwrap(),
                expected,
            },
        ] {
            let result = eval_body_owner_const(&db, owner, vec![]);
            if expected.as_capability(&db).is_some() {
                assert!(
                    matches!(
                        result,
                        EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::InvalidBorrow { .. }))
                    ),
                    "{result:?}"
                );
            } else {
                assert_eq!(result.into_ready().unwrap().pretty_print(&db), "42");
            }
        }
    }
}

#[test]
fn semantic_const_operands_keep_formal_evidence_distinct() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_const_evidence.fe".into(),
        "fn echo<const N: usize>() -> usize { N }",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let func = module.all_funcs(&db)[0];
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    assert!(
        instance
            .body(&db)
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .any(|stmt| matches!(
                &stmt.kind,
                SStmtKind::Assign {
                    expr: SExpr::Const(SConst::Evidence(_)),
                    ..
                }
            ))
    );
}

#[test]
fn canonicalize_folds_const_calls_into_nested_aggregate_consts() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        r#"
const fn values() -> [u256; 4] {
    [1, 2, 3, 4]
}

fn wrap() {
    let args: ([u256; 4],) = (values(),)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .find(|func| matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "wrap"))
        .expect("expected wrap function");

    let semantic = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
    );
    assert!(std::ptr::eq(semantic.body(&db), semantic.body(&db)));
    let body = canonicalize_semantic_consts(&db, semantic)
        .expect("valid semantic body should be canonicalizable");
    assert!(std::ptr::eq(
        body,
        canonicalize_semantic_consts(&db, semantic)
            .expect("valid semantic body should remain canonicalizable")
    ));

    let found = body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .any(|stmt| {
            let SStmtKind::Assign {
                expr: SExpr::Const(SConst::Value(value)),
                ..
            } = &stmt.kind
            else {
                return false;
            };
            let SemConstValue::Tuple { elems, .. } = value.value().value(&db) else {
                return false;
            };
            let [array] = elems.as_ref() else {
                return false;
            };
            matches!(
                array.value(&db),
                SemConstValue::Array { elems, .. } if elems.len() == 4
            )
        });

    assert!(
        found,
        "expected canonicalization to fold const call into tuple(array) semantic const"
    );
}

#[test]
fn semantic_ctfe_evaluates_as_bytes_const_fns() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        include_str!("../../uitest/fixtures/ty_check/const_eval/user_const_fn_ok.fe"),
    );
    let (top_mod, _) = db.top_mod(file);

    for name in ["bytes_tail", "stor_code"] {
        let func = top_mod
            .all_funcs(&db)
            .iter()
            .find(
                |func| matches!(func.name(&db), Partial::Present(found) if found.data(&db) == name),
            )
            .copied()
            .unwrap_or_else(|| panic!("missing const fn `{name}`"));

        let value = match eval_body_owner_const(&db, BodyOwner::Func(func), Vec::new()) {
            EvalOutcome::Ready(value) => value,
            outcome => panic!("semantic CTFE failed for `{name}`: {outcome:?}"),
        };
        let ty = fe_hir::analysis::semantic::sem_const_ty(&db, value);
        assert!(
            !matches!(value.value(&db), SemConstValue::Description(..)),
            "`{name}` should lower to a value const, got {:?} with ty {}",
            value.value(&db),
            ty.pretty_print(&db)
        );
    }
}

#[test]
fn semantic_ctfe_evaluates_fixed_string_primitives() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe_string.fe".into(),
        r#"
use core::concat
use core::intrinsic

const fn padded_bytes() -> [u8; 8] {
    let s: String<8> = "COOL"
    s.as_bytes()
}

const fn roundtrip_bytes() -> [u8; 8] {
    let s: String<8> = "COOL"
    let bytes: [u8; 8] = s.as_bytes()
    let t: String<8> = String::from_bytes(bytes)
    t.as_bytes()
}

const fn cool_len() -> usize {
    let s: String<8> = "COOL"
    s.len()
}

const fn concat_bytes() -> [u8; 8] {
    let a: String<8> = "COOL"
    let b: String<8> = "COIN"
    let c: String<8> = concat(a, b)
    c.as_bytes()
}

const fn concat_uses_effective_len() -> [u8; 4] {
    let a: String<4> = "C"
    let b: String<4> = "O"
    let c: String<4> = concat(a, b)
    c.as_bytes()
}

const fn eq_true() -> bool {
    let a: String<8> = "COOL"
    let b: String<8> = "COOL"
    a == b
}

const fn eq_false() -> bool {
    let a: String<8> = "COOL"
    let b: String<8> = "COIN"
    a == b
}

const fn eq_from_bytes_matches_literal() -> bool {
    let bytes: [u8; 8] = [0, 0, 0, 0, 67, 79, 79, 76]
    let s: String<8> = String::from_bytes(bytes)
    s == "COOL"
}

const fn from_bytes_value() -> String<8> {
    let bytes: [u8; 8] = [0, 0, 0, 0, 67, 79, 79, 76]
    String::from_bytes(bytes)
}

const fn literal_value_4() -> String<4> {
    "COOL"
}

const fn literal_value_8() -> String<8> {
    "COOL"
}

const fn rhs_word_in_eq_from_bytes() -> u256 {
    let bytes: [u8; 8] = [0, 0, 0, 0, 67, 79, 79, 76]
    let s: String<8> = String::from_bytes(bytes)
    let rhs = "COOL"
    let _ok: bool = s == rhs
    rhs as u256
}

const fn s_word_in_eq_from_bytes() -> u256 {
    let bytes: [u8; 8] = [0, 0, 0, 0, 67, 79, 79, 76]
    let s: String<8> = String::from_bytes(bytes)
    s as u256
}

const fn rhs_value_in_eq_from_bytes() -> String<8> {
    let bytes: [u8; 8] = [0, 0, 0, 0, 67, 79, 79, 76]
    let s: String<8> = String::from_bytes(bytes)
    let rhs = "COOL"
    let _ok: bool = s == rhs
    rhs
}

const fn s_value_in_eq_from_bytes() -> String<8> {
    let bytes: [u8; 8] = [0, 0, 0, 0, 67, 79, 79, 76]
    String::from_bytes(bytes)
}

const fn high_word_string_roundtrip() -> u256 {
    (0x01000000434f4f4c as String<4>) as u256
}

const fn high_word_string_value() -> String<4> {
    0x01000000434f4f4c as String<4>
}

const fn high_word_string_as_bytes() -> [u8; 4] {
    let s: String<4> = 0x01000000434f4f4c as String<4>
    intrinsic::__as_bytes(s)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    fn find_func<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: fe_hir::hir_def::TopLevelMod<'db>,
        name: &str,
    ) -> fe_hir::hir_def::Func<'db> {
        top_mod
            .all_funcs(db)
            .iter()
            .copied()
            .find(|func| matches!(func.name(db), Partial::Present(found) if found.data(db) == name))
            .unwrap_or_else(|| panic!("missing const fn `{name}`"))
    }

    fn eval_bytes<'db>(db: &'db HirAnalysisTestDb, func: fe_hir::hir_def::Func<'db>) -> Vec<u8> {
        let value = match eval_body_owner_const(db, BodyOwner::Func(func), Vec::new()) {
            EvalOutcome::Ready(value) => value,
            err => {
                let name = func
                    .name(db)
                    .to_opt()
                    .map_or("<anon>", |n| n.data(db).as_str());
                let (diags, _) = check_func_body(db, func).clone();
                let mut note = String::new();
                for diag in diags.iter() {
                    if let FuncBodyDiag::NameRes(fe_hir::analysis::name_resolution::diagnostics::PathResDiag::MethodNotFound { method_name, .. }) = diag {
                        note.push_str(&format!("method-not-found: {}\n", method_name.data(db)));
                    }
                }
                panic!("semantic CTFE failed for `{name}`: {err:?}\n{note}{diags:#?}");
            }
        };
        match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Bytes(bytes),
                ..
            } => bytes.clone(),
            SemConstValue::Array { elems, .. } => elems
                .iter()
                .map(|elem| match elem.value(db) {
                    SemConstValue::Scalar {
                        value: SemConstScalar::Int { value },
                        ..
                    } => value
                        .to_u8()
                        .unwrap_or_else(|| panic!("expected u8 int const, got {value}")),
                    other => panic!("expected u8 scalar const element, got {other:?}"),
                })
                .collect(),
            other => panic!("expected bytes scalar const, got {other:?}"),
        }
    }

    fn eval_usize<'db>(db: &'db HirAnalysisTestDb, func: fe_hir::hir_def::Func<'db>) -> usize {
        let value = match eval_body_owner_const(db, BodyOwner::Func(func), Vec::new()) {
            EvalOutcome::Ready(value) => value,
            err => {
                let name = func
                    .name(db)
                    .to_opt()
                    .map_or("<anon>", |n| n.data(db).as_str());
                let (diags, _) = check_func_body(db, func).clone();
                panic!("semantic CTFE failed for `{name}`: {err:?}\n{diags:#?}");
            }
        };
        match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => value
                .to_usize()
                .unwrap_or_else(|| panic!("expected usize-sized int const, got {value}")),
            other => panic!("expected int scalar const, got {other:?}"),
        }
    }

    fn eval_bool<'db>(db: &'db HirAnalysisTestDb, func: fe_hir::hir_def::Func<'db>) -> bool {
        let value = match eval_body_owner_const(db, BodyOwner::Func(func), Vec::new()) {
            EvalOutcome::Ready(value) => value,
            err => {
                let name = func
                    .name(db)
                    .to_opt()
                    .map_or("<anon>", |n| n.data(db).as_str());
                let (diags, _) = check_func_body(db, func).clone();
                panic!("semantic CTFE failed for `{name}`: {err:?}\n{diags:#?}");
            }
        };
        match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Bool(flag),
                ..
            } => flag,
            other => panic!("expected bool scalar const, got {other:?}"),
        }
    }

    let padded = eval_bytes(&db, find_func(&db, top_mod, "padded_bytes"));
    assert_eq!(padded, vec![0, 0, 0, 0, 67, 79, 79, 76]);

    let roundtrip = eval_bytes(&db, find_func(&db, top_mod, "roundtrip_bytes"));
    assert_eq!(roundtrip, padded);

    let len = eval_usize(&db, find_func(&db, top_mod, "cool_len"));
    assert_eq!(len, 4);

    let concat_bytes = eval_bytes(&db, find_func(&db, top_mod, "concat_bytes"));
    assert_eq!(concat_bytes, vec![67, 79, 79, 76, 67, 79, 73, 78]);

    let effective = eval_bytes(&db, find_func(&db, top_mod, "concat_uses_effective_len"));
    assert_eq!(effective, vec![0, 0, 67, 79]);

    assert!(eval_bool(&db, find_func(&db, top_mod, "eq_true")));
    assert!(!eval_bool(&db, find_func(&db, top_mod, "eq_false")));
    let eq_from_bytes = eval_bool(
        &db,
        find_func(&db, top_mod, "eq_from_bytes_matches_literal"),
    );

    let from_bytes_const = eval_body_owner_const(
        &db,
        BodyOwner::Func(find_func(&db, top_mod, "from_bytes_value")),
        Vec::new(),
    )
    .into_ready()
    .expect("expected const evaluation success");
    let literal_4_const = eval_body_owner_const(
        &db,
        BodyOwner::Func(find_func(&db, top_mod, "literal_value_4")),
        Vec::new(),
    )
    .into_ready()
    .expect("expected const evaluation success");
    let literal_8_const = eval_body_owner_const(
        &db,
        BodyOwner::Func(find_func(&db, top_mod, "literal_value_8")),
        Vec::new(),
    )
    .into_ready()
    .expect("expected const evaluation success");
    let sem_eq = fe_hir::analysis::semantic::sem_const_eq(&db, from_bytes_const, literal_4_const);
    let sem_eq_lit8 =
        fe_hir::analysis::semantic::sem_const_eq(&db, from_bytes_const, literal_8_const);
    if !eq_from_bytes || !sem_eq {
        let from_dbg = match from_bytes_const.value(&db) {
            SemConstValue::Scalar {
                ty,
                value: SemConstScalar::Bytes(bytes),
            } => format!("{} {:?}", ty.pretty_print(&db), bytes),
            other => format!("{other:?}"),
        };
        let lit_dbg = match literal_4_const.value(&db) {
            SemConstValue::Scalar {
                ty,
                value: SemConstScalar::Bytes(bytes),
            } => format!("{} {:?}", ty.pretty_print(&db), bytes),
            other => format!("{other:?}"),
        };
        let rhs_word = eval_body_owner_const(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "rhs_word_in_eq_from_bytes")),
            Vec::new(),
        )
        .into_ready()
        .and_then(|value| match value.value(&db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => Some(value.clone()),
            _ => None,
        });
        let s_word = eval_body_owner_const(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "s_word_in_eq_from_bytes")),
            Vec::new(),
        )
        .into_ready()
        .and_then(|value| match value.value(&db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => Some(value.clone()),
            _ => None,
        });
        let rhs_value_eq = eval_body_owner_const(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "rhs_value_in_eq_from_bytes")),
            Vec::new(),
        )
        .into_ready()
        .map(|value| value.value(&db).clone());
        let s_value_eq = eval_body_owner_const(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "s_value_in_eq_from_bytes")),
            Vec::new(),
        )
        .into_ready()
        .map(|value| value.value(&db).clone());
        panic!(
            "unexpected equality results: eq_from_bytes_matches_literal={eq_from_bytes} sem_const_eq(lit4)={sem_eq} sem_const_eq(lit8)={sem_eq_lit8}\nfrom_bytes_value={from_dbg}\nliteral_value_4={lit_dbg}\nliteral_value_8={:?}\ns_word_in_eq_from_bytes={s_word:?}\nrhs_word_in_eq_from_bytes={rhs_word:?}\ns_value_in_eq_from_bytes={s_value_eq:?}\nrhs_value_in_eq_from_bytes={rhs_value_eq:?}",
            literal_8_const.value(&db),
        );
    }

    let high_roundtrip = eval_body_owner_const(
        &db,
        BodyOwner::Func(find_func(&db, top_mod, "high_word_string_roundtrip")),
        Vec::new(),
    )
    .into_ready()
    .expect("expected const evaluation success");
    match high_roundtrip.value(&db) {
        SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } => assert_eq!(value, num_bigint::BigInt::from(0x01000000434f4f4cu64)),
        other => panic!("expected int scalar const, got {other:?}"),
    }

    let high_bytes = eval_bytes(&db, find_func(&db, top_mod, "high_word_string_as_bytes"));
    assert_eq!(high_bytes, vec![67, 79, 79, 76]);

    let high_word_const = eval_body_owner_const(
        &db,
        BodyOwner::Func(find_func(&db, top_mod, "high_word_string_value")),
        Vec::new(),
    )
    .into_ready()
    .expect("expected const evaluation success");
    assert!(
        fe_hir::analysis::semantic::sem_const_eq(&db, high_word_const, literal_4_const),
        "CTFE string equality should ignore hidden high bytes outside String<N> capacity"
    );
}

#[test]
fn semantic_ctfe_fixed_string_concat_overflow_truncates() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe_string_overflow.fe".into(),
        r#"
use core::concat

const fn truncated_concat() -> [u8; 6] {
    let a: String<4> = "ABCD"
    let b: String<4> = "EFGH"
    let c: String<6> = concat(a, b)
    c.as_bytes()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            matches!(func.name(&db), Partial::Present(found) if found.data(&db) == "truncated_concat")
        })
        .expect("missing const fn `truncated_concat`");

    let value = eval_body_owner_const(&db, BodyOwner::Func(func), Vec::new())
        .into_ready()
        .unwrap();
    match value.value(&db) {
        SemConstValue::Scalar {
            value: SemConstScalar::Bytes(bytes),
            ..
        } => assert_eq!(bytes, vec![65, 66, 67, 68, 69, 70]),
        SemConstValue::Array { elems, .. } => {
            let bytes = elems
                .iter()
                .map(|elem| match elem.value(&db) {
                    SemConstValue::Scalar {
                        value: SemConstScalar::Int { value },
                        ..
                    } => value
                        .to_u8()
                        .unwrap_or_else(|| panic!("expected u8 int const, got {value}")),
                    other => panic!("expected u8 scalar const element, got {other:?}"),
                })
                .collect::<Vec<_>>();
            assert_eq!(bytes, vec![65, 66, 67, 68, 69, 70]);
        }
        other => panic!("expected bytes scalar const, got {other:?}"),
    }
}

#[test]
fn const_fn_match_has_no_const_body_diagnostic() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        include_str!("../test_files/ty_check/const_eval_const_fn_match.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .find(
            |func| matches!(func.name(&db), Partial::Present(found) if found.data(&db) == "choose"),
        )
        .copied()
        .expect("missing const fn `choose`");

    let (diags, _) = check_func_body(&db, func).clone();

    assert!(
        !diags.iter().any(|diag| matches!(
            diag,
            FuncBodyDiag::Body(
                BodyDiag::ConstFnEffectsNotAllowed(_)
                    | BodyDiag::ConstFnWithNotAllowed(_)
                    | BodyDiag::ConstFnNonConstCall { .. }
                    | BodyDiag::ConstFnEffectfulCall { .. }
            )
        )),
        "{diags:#?}"
    );
}

#[test]
fn invalid_const_fn_body_is_rejected_before_ctfe_lowering() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        r#"
const fn invalid_const() -> usize {
    pass
    missing_value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .find(|func| matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "invalid_const"))
        .copied()
        .expect("missing const fn `invalid_const`");

    let result =
        eval_body_owner_const_with_args(&db, BodyOwner::Func(func), Vec::new(), Vec::new());

    assert!(
        matches!(
            result,
            EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::InvalidBody { .. }))
        ),
        "expected invalid body CTFE error, got {result:?}"
    );
}

#[test]
fn invalid_call_like_const_fn_body_is_rejected_before_ctfe_lowering() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        r#"
const fn value(_x: u8) -> u256 {
    1
}

const fn invalid_call_like_expr() -> u256 {
    value(1) (2)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .find(|func| matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "invalid_call_like_expr"))
        .copied()
        .expect("missing const fn `invalid_call_like_expr`");
    let (diags, _) = check_func_body(&db, func).clone();

    assert!(
        diags
            .iter()
            .any(|diag| matches!(diag, FuncBodyDiag::Body(BodyDiag::NotCallable(..)))),
        "expected not-callable body diagnostic, got {diags:#?}"
    );

    let result =
        eval_body_owner_const_with_args(&db, BodyOwner::Func(func), Vec::new(), Vec::new());

    assert!(
        matches!(
            result,
            EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::InvalidBody { .. }))
        ),
        "expected invalid body CTFE error, got {result:?}"
    );
}

#[test]
fn type_alias_len_reports_nested_const_eval_error() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        include_str!(
            "../../uitest/fixtures/ty_check/const_eval/as_bytes_mixed_enum_unsupported.fe"
        ),
    );
    let (top_mod, _) = db.top_mod(file);
    let alias = top_mod
        .all_type_aliases(&db)
        .iter()
        .find(|alias| matches!(alias.name(&db), Partial::Present(name) if name.data(&db) == "Arr"))
        .copied()
        .expect("missing type alias `Arr`");
    let diags = alias.diags(&db);
    let [TyDiagCollection::Ty(TyLowerDiag::ConstEvalUnsupported(span))] = diags.as_slice() else {
        panic!("expected one const-eval unsupported diagnostic, got {diags:#?}");
    };
    let resolved = span
        .resolve(&db)
        .expect("diagnostic should resolve to source");
    let text = file.text(&db);

    assert_eq!(
        &text[resolved.range.start().into()..resolved.range.end().into()],
        "intrinsic::__as_bytes(Mixed::B)"
    );
}

#[test]
fn type_alias_len_preserves_specialized_bounds_failure() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_bounds_diagnostic.fe".into(),
        r#"
const fn first<const N: usize>() -> usize { [7 as usize; N][0] }
const BAD: usize = first<0>()
type ViaConst = [u8; BAD]
type Direct = [u8; first<0>()]
"#,
    );
    let (module, _) = db.top_mod(file);
    for alias in module.all_type_aliases(&db) {
        let diags = alias.diags(&db);
        assert!(
            diags.iter().any(|diag| matches!(
                diag,
                TyDiagCollection::Ty(TyLowerDiag::ConstEvalOutOfBounds(_))
            )),
            "expected a bounds diagnostic for {}: {diags:#?}",
            alias.name(&db).to_opt().unwrap().data(&db),
        );
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

#[test]
fn contract_init_fixed_array_arg_fixture_has_no_type_level_semantic_consts() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        include_str!("../../fe/tests/fixtures/fe_test/contract_init_fixed_array_arg.fe"),
    );
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
    let mut offenders = Vec::new();
    while let Some(instance) = pending.pop_front() {
        if !seen.insert(instance.key(&db)) {
            continue;
        }
        let body = canonicalize_semantic_consts(&db, instance)
            .expect("valid semantic body should be canonicalizable");
        for stmt in body.blocks.iter().flat_map(|block| block.stmts.iter()) {
            if let SStmtKind::Assign {
                expr: SExpr::Const(SConst::Description(value) | SConst::Evidence(value)),
                ..
            } = &stmt.kind
                && matches!(value.value(&db), SemConstValue::Description(..))
            {
                offenders.push(format!(
                    "{} args={:?} {:?} ty={} {:?}",
                    owner_name(&db, instance.key(&db).owner(&db)),
                    instance
                        .key(&db)
                        .subst(&db)
                        .generic_args(&db)
                        .iter()
                        .map(|ty| ty.pretty_print(&db))
                        .collect::<Vec<_>>(),
                    stmt.origin,
                    match value.value(&db) {
                        SemConstValue::Description(term) => term.pretty_print_concrete(&db),
                        _ => "<non-typelevel>".to_string(),
                    },
                    value.value(&db)
                ));
            }
        }
        for callee in instance.callees(&db) {
            pending.push_back(get_or_build_semantic_instance(&db, callee.key));
        }
    }

    assert!(
        offenders.is_empty(),
        "unexpected type-level semantic consts:\n{}",
        offenders.join("\n")
    );
}

#[test]
fn canonicalize_does_not_fold_abstract_checked_runtime_calls() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        r#"
fn test_add_overflow_u8() {
    let x: u8 = 255
    let y: u8 = x + 1
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .find(|func| {
            matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "test_add_overflow_u8")
        })
        .expect("expected test_add_overflow_u8 function");

    let semantic = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
    );
    let body = canonicalize_semantic_consts(&db, semantic)
        .expect("valid semantic body should be canonicalizable");

    assert!(
        body.blocks
            .iter()
            .flat_map(|block| block.stmts.iter())
            .any(|stmt| {
                matches!(
                    &stmt.kind,
                    SStmtKind::Assign {
                        expr: SExpr::Call { .. } | SExpr::Binary { .. },
                        ..
                    }
                )
            }),
        "checked runtime arithmetic should remain executable after semantic const canonicalization:\n{body:#?}"
    );
}

#[test]
fn optional_fold_miss_keeps_runtime_input_and_independent_const_fact() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "optional_fold_miss.fe".into(),
        r#"
fn evaluate(x: u8) -> u8 {
    let y: u8 = 7
    let unknown: u8 = x + 1
    let known: u8 = y + 1
    known
}
"#,
    );
    let (module, _) = db.top_mod(file);
    let func = module.all_funcs(&db)[0];
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let body = canonicalize_semantic_consts(&db, instance).unwrap();
    let expressions = body
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .filter_map(|stmt| {
            let SStmtKind::Assign { expr, .. } = &stmt.kind else {
                return None;
            };
            Some(expr)
        });
    let mut runtime_operation = false;
    let mut folded_eight = false;
    for expr in expressions {
        runtime_operation |= matches!(expr, SExpr::Call { .. } | SExpr::Binary { .. });
        folded_eight |= matches!(
            expr,
            SExpr::Const(SConst::Value(value))
                if matches!(value.value().value(&db), SemConstValue::Scalar {
                    value: SemConstScalar::Int { value }, ..
                } if value.to_u8() == Some(8))
        );
    }
    assert!(
        runtime_operation,
        "unknown input operation must remain executable"
    );
    assert!(folded_eight, "independent constant fact must still fold");
}

#[test]
fn canonicalize_compares_semantically_equal_consts_structurally() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        r#"
fn signed_mul_no_overflow_i8_neg_neg() {
    let a: i8 = -6
    let b: i8 = -3
    let ok: bool = a * b == 18
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .find(|func| {
            matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "signed_mul_no_overflow_i8_neg_neg")
        })
        .expect("expected signed_mul_no_overflow_i8_neg_neg function");

    let semantic = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
    );
    let body = canonicalize_semantic_consts(&db, semantic)
        .expect("valid semantic body should be canonicalizable");

    assert!(
        body.blocks
            .iter()
            .flat_map(|block| block.stmts.iter())
            .any(|stmt| {
                matches!(
                    &stmt.kind,
                    SStmtKind::Assign {
                        expr: SExpr::Const(SConst::Value(value)),
                        ..
                    } if matches!(
                        value.value().value(&db),
                        SemConstValue::Scalar {
                            value: SemConstScalar::Bool(true),
                            ..
                        }
                    )
                )
            }),
        "expected semantic CTFE to fold `a * b == 18` to `true`, got:\n{body:#?}"
    );
}

fn sem_const_has_type_level_leaf(db: &HirAnalysisTestDb, value: SemConstId<'_>) -> bool {
    match value.value(db) {
        SemConstValue::Unit | SemConstValue::Scalar { .. } => false,
        SemConstValue::Description(..) => true,
        SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => elems
            .iter()
            .copied()
            .any(|elem| sem_const_has_type_level_leaf(db, elem)),
        SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => fields
            .iter()
            .copied()
            .any(|field| sem_const_has_type_level_leaf(db, field)),
    }
}

fn sem_const_has_abstract_scalar_leaf(db: &HirAnalysisTestDb, value: SemConstId<'_>) -> bool {
    match value.value(db) {
        SemConstValue::Unit | SemConstValue::Description(..) => false,
        SemConstValue::Scalar { ty, .. } => ty.pretty_print(db) == "{integer}",
        SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => elems
            .iter()
            .copied()
            .any(|elem| sem_const_has_abstract_scalar_leaf(db, elem)),
        SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => fields
            .iter()
            .copied()
            .any(|field| sem_const_has_abstract_scalar_leaf(db, field)),
    }
}

#[test]
fn runtime_const_reification_removes_nested_type_level_leaves() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        r#"
fn first(xs: ref [u256; 2]) -> u256 {
    xs[0]
}

fn entry() -> u256 {
    first([10, 20])
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .find(|func| matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "entry"))
        .expect("expected entry function");

    let semantic = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
    );
    let body = canonicalize_semantic_consts(&db, semantic)
        .expect("valid semantic body should be canonicalizable");
    let expected_ty = top_mod
        .all_funcs(&db)
        .iter()
        .find(|func| matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "first"))
        .and_then(|func| func.arg_tys(&db).first().copied())
        .map(|ty| ty.instantiate_identity())
        .expect("expected first(xs: ref [u256; 2]) parameter type");
    let (_, args) = expected_ty.decompose_ty_app(&db);
    let array_expected_ty = args.first().copied().expect("ref pointee type");

    let array_const = body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| {
            let SStmtKind::Assign {
                expr: SExpr::Const(SConst::Value(value)),
                ..
            } = &stmt.kind
            else {
                return None;
            };
            matches!(value.value().value(&db), SemConstValue::Array { .. }).then_some(value.value())
        })
        .expect("expected array constant in canonicalized body");

    let reified = reify_runtime_const_for_ty(&db, semantic, array_expected_ty, array_const)
        .expect("array constant should reify");
    assert!(
        !sem_const_has_type_level_leaf(&db, reified),
        "reified runtime const should not contain nested descriptions: {:?}",
        reified.value(&db)
    );
    assert!(
        !sem_const_has_abstract_scalar_leaf(&db, reified),
        "reified runtime const should not contain nested abstract scalar leaves: {:?}",
        reified.value(&db)
    );
}

#[test]
fn canonicalize_invalidates_const_facts_for_mutating_calls() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        r#"
#[arithmetic(unchecked)]
fn wraps_after_aug_assign() -> bool {
    let mut x: u8 = 255
    x += 1
    x == 0
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .find(|func| {
            matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "wraps_after_aug_assign")
        })
        .expect("expected wraps_after_aug_assign function");

    let semantic = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
    );
    let body = canonicalize_semantic_consts(&db, semantic)
        .expect("valid semantic body should be canonicalizable");

    let mut saw_bool_call = false;
    let mut saw_const_false = false;
    for stmt in body.blocks.iter().flat_map(|block| block.stmts.iter()) {
        let SStmtKind::Assign { dst, expr } = &stmt.kind else {
            continue;
        };
        if !body.locals[dst.index()].ty.is_bool(&db) {
            continue;
        }
        saw_bool_call |= matches!(expr, SExpr::Call { .. });
        saw_const_false |= matches!(
            expr,
            SExpr::Const(SConst::Value(value))
                if matches!(
                    value.value().value(&db),
                    SemConstValue::Scalar {
                        value: SemConstScalar::Bool(false),
                        ..
                    }
                )
        );
    }

    assert!(
        saw_bool_call,
        "expected the post-call comparison to stay dynamic in canonicalized semantic body"
    );
    assert!(
        !saw_const_false,
        "mutating call should invalidate caller const facts instead of folding comparison to false"
    );
}

#[test]
fn canonicalize_concretizes_negated_min_literal_comparisons() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_ctfe.fe".into(),
        r#"
#[arithmetic(unchecked)]
fn negated_min_i8_compares_equal() -> bool {
    let x: i8 = -128
    let y: i8 = -x
    y == -128
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .find(|func| {
            matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "negated_min_i8_compares_equal")
        })
        .expect("expected negated_min_i8_compares_equal function");

    let semantic = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
    );
    let body = canonicalize_semantic_consts(&db, semantic)
        .expect("valid semantic body should be canonicalizable");

    let mut saw_const_true = false;
    let mut saw_const_false = false;
    for stmt in body.blocks.iter().flat_map(|block| block.stmts.iter()) {
        let SStmtKind::Assign { dst, expr } = &stmt.kind else {
            continue;
        };
        if !body.locals[dst.index()].ty.is_bool(&db) {
            continue;
        }
        saw_const_true |= matches!(
            expr,
            SExpr::Const(SConst::Value(value))
                if matches!(
                    value.value().value(&db),
                    SemConstValue::Scalar {
                        value: SemConstScalar::Bool(true),
                        ..
                    }
                )
        );
        saw_const_false |= matches!(
            expr,
            SExpr::Const(SConst::Value(value))
                if matches!(
                    value.value().value(&db),
                    SemConstValue::Scalar {
                        value: SemConstScalar::Bool(false),
                        ..
                    }
                )
        );
    }

    assert!(
        saw_const_true,
        "expected the comparison against -128 to fold to true after deferred primitive resolution"
    );
    assert!(
        !saw_const_false,
        "negated minimum integer literal should not stay abstract and fold to false"
    );
}

#[test]
fn canonicalize_tracks_mutation_through_aggregate_capabilities() {
    for (name, operation, folds) in [
        ("field", "holder.handle = 1", false),
        (
            "reborrow",
            "let alias = mut holder.handle\n alias = 1",
            false,
        ),
        (
            "array",
            "let mut values = [holder]\n values[index].handle = 1",
            false,
        ),
        (
            "nested",
            "let mut outer = Outer { inner: mut holder }\n outer.inner.handle = 1",
            false,
        ),
        ("value_call", "write(holder)", false),
        ("direct_effect", "with (counter) { write_direct() }", false),
        ("mutable_effect", "with (holder) { write_effect() }", false),
        ("borrowed_call", "write_borrowed(mut holder)", false),
        (
            "returned",
            "let alias = returned(holder)\n alias = 1",
            false,
        ),
        (
            "stored",
            "let mut values = [Wrap { handle: mut other }]\n values[0] = holder\n values[0].handle = 1",
            false,
        ),
        (
            "call_stored",
            "let mut values = [Wrap { handle: mut other }]\n replace(values: mut values, replacement: holder)\n values[0].handle = 1",
            false,
        ),
        (
            "loop",
            "while index == 0 { holder.handle = 1\n break }",
            false,
        ),
        ("reassigned_control", "holder.handle = 1\n value = 1", true),
        ("unrelated_control", "holder.handle = 1", true),
        ("shared_control", "read(ref unrelated)", true),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let result = if matches!(name, "unrelated_control" | "shared_control") {
            "unrelated == 7"
        } else if name == "direct_effect" {
            "counter.value == 1"
        } else {
            "value == 1"
        };
        let source = format!(
            r#"
struct Counter {{ value: u256 }}
struct Wrap {{ handle: mut u256 }}
fn write_direct() uses (counter: mut Counter) {{ counter.value = 1 }}
fn write_effect() uses (holder: mut Wrap) {{ holder.handle = 1 }}
struct Outer {{ inner: mut Wrap }}
fn read(_ value: ref u256) -> u256 {{ value }}
fn write(mut holder: own Wrap) {{ holder.handle = 1 }}
fn write_borrowed(_ holder: mut Wrap) {{ holder.handle = 1 }}
fn returned(holder: Wrap) -> mut u256 {{ holder.handle }}
fn replace(values: mut [Wrap; 1], replacement: Wrap) {{ values[0] = replacement }}
fn probe(index: usize) -> bool {{
    let unrelated: u256 = 7
    let mut counter = Counter {{ value: 0 }}
    let mut value: u256 = 0
    let mut other: u256 = 0
    let mut holder = Wrap {{ handle: mut value }}
    {operation}
    {result}
}}
"#
        );
        let file = db.new_stand_alone(format!("ctfe_{name}.fe").into(), &source);
        let (top_mod, _) = db.top_mod(file);
        for func in top_mod.all_funcs(&db) {
            let (diagnostics, _) = check_func_body(&db, *func).clone();
            assert!(diagnostics.is_empty(), "{name}: {diagnostics:#?}");
        }
        let func = top_mod.all_funcs(&db).iter().copied().find(|func| {
            matches!(func.name(&db), Partial::Present(name) if name.data(&db) == "probe")
        }).unwrap();
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
        let body = canonicalize_semantic_consts(&db, instance).unwrap();
        let mut constant_true = false;
        let mut dynamic_comparison = false;
        for statement in body.blocks.iter().flat_map(|block| &block.stmts) {
            if let SStmtKind::Assign { dst, expr } = &statement.kind
                && body.locals[dst.index()].ty.is_bool(&db)
            {
                match expr {
                    SExpr::Const(SConst::Value(value)) => {
                        assert!(
                            !matches!(
                                value.value().value(&db),
                                SemConstValue::Scalar {
                                    value: SemConstScalar::Bool(false),
                                    ..
                                }
                            ),
                            "{name}: stale comparison folded to false: {body:#?}"
                        );
                        constant_true |= matches!(
                            value.value().value(&db),
                            SemConstValue::Scalar {
                                value: SemConstScalar::Bool(true),
                                ..
                            }
                        );
                    }
                    SExpr::Call { .. } => dynamic_comparison = true,
                    _ => {}
                }
            }
        }
        assert_eq!(constant_true, folds, "{name}: {body:#?}");
        assert!(
            folds || dynamic_comparison,
            "{name}: missing comparison: {body:#?}"
        );
    }
}

#[test]
fn selected_trait_const_results_keep_the_callers_generic_parameters() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "selected_trait_const.fe".into(),
        r#"
trait Pick<const N: usize> {
    const A: usize = N
    const B: usize = Self::A
}
struct Single<const N: usize> {}
impl<const N: usize> Pick<N> for Single<N> {}
struct Marker<const M: usize, const N: usize> {}
impl<const M: usize, const N: usize> Pick<N> for Marker<M, N> {}
const fn single() -> usize { Single<7>::B }
const fn first() -> usize { Marker<3, 7>::B }
const fn second() -> usize { Marker<7, 3>::B }
fn exact_length(_ value: [u8; Single<7>::B]) {}
fn check_length() {
    let value: [u8; 7] = [0; 7]
    exact_length(value)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    for (name, expected) in [("single", 7), ("first", 7), ("second", 3)] {
        let value = eval_body_owner_const(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, name)),
            Vec::new(),
        )
        .into_ready()
        .expect("selected associated constant should evaluate");
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.value(&db)
        else {
            panic!(
                "expected an integer constant for {name}: {:?}",
                value.value(&db)
            );
        };
        assert_eq!(value.to_usize(), Some(expected));
    }
}

#[test]
fn generic_associated_const_records_are_typed_before_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "const_record_template.fe".into(),
        r#"
trait Build {
    type Value
    const VALUE: Self::Value
}
struct Marker<const N: usize> { value: u8 }
impl<const N: usize> Build for Marker<N> {
    type Value = Marker<N>
    const VALUE: Self::Value = Marker<N> { value: 7 }
}
impl<const N: usize> Marker<N> {
    const OTHER: Marker<N> = Marker<N> { value: 9 }
}
const fn first() -> u8 { Marker<2>::VALUE.value }
const fn second() -> u8 { Marker<3>::VALUE.value }
const fn inherent() -> u8 { Marker<3>::OTHER.value }
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    for (name, expected) in [("first", 7), ("second", 7), ("inherent", 9)] {
        let value = eval_body_owner_const(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, name)),
            Vec::new(),
        )
        .into_ready()
        .expect("generic const record should evaluate");
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.value(&db)
        else {
            panic!("expected scalar for {name}");
        };
        assert_eq!(value.to_usize(), Some(expected));
    }
}
