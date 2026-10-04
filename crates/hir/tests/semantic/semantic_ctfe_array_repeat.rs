use cranelift_entity::EntityRef;
use fe_hir::{
    analysis::{
        semantic::{
            CtfeError, EvalFailure, EvalOutcome, GenericSubst, SConst, SExpr, SStmtKind,
            SemConstId, SemConstScalar, SemConstValue, SemanticDiagnosticKind, SemanticInstanceKey,
            SemanticNormalizationFailure, canonicalize_semantic_consts, eval_body_owner_const,
            eval_const_instance, get_or_build_semantic_instance, identity_semantic_instance_key,
            normalize_runtime_semantic_body, normalize_semantic_body,
            normalized::{NLayoutLocals, NValueId},
            reify_runtime_const, reify_runtime_const_for_ty, sem_const_ty,
        },
        ty::{
            const_ty::ConstTyId,
            ty_check::BodyOwner,
            ty_def::{PrimTy, TyBase, TyData, TyId},
        },
    },
    hir_def::{Func, Partial, TopLevelMod},
    test_db::{HirAnalysisTestDb, format_diagnostics},
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

fn scalar(db: &HirAnalysisTestDb, value: SemConstId<'_>) -> usize {
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("expected integer, got {:?}", value.value(db));
    };
    value.to_usize().expect("small integer")
}

fn const_args<'db>(db: &'db HirAnalysisTestDb, len: u32, element: u32) -> [TyId<'db>; 2] {
    [(PrimTy::Usize, len), (PrimTy::U8, element)].map(|(prim, value)| {
        TyId::new(
            db,
            TyData::ConstTy(ConstTyId::integer(
                db,
                TyId::new(db, TyData::TyBase(TyBase::Prim(prim))),
                BigInt::from(value),
            )),
        )
    })
}

#[test]
fn symbolic_array_indices_specialize() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "symbolic_indices.fe".into(),
        r#"
struct Pair { value: u8 }
impl Copy for Pair {}
struct Marker<const N: usize, const I: usize> {}
const fn local<const N: usize, const I: usize>() -> u8 {
    let values = [13; N]
    values[I]
}
const fn changed<const N: usize, const I: usize>() -> [u8; N] {
    let mut values = [0; N]
    values[I] = 19
    values
}
impl<const N: usize, const I: usize> Marker<N, I> {
    const VALUE: u8 = [7; N][I]
    const CONCRETE: u8 = [3, 5, 11][I]
    const FIELD: u8 = [(Pair { value: 23 }, [29 as u8; N]); N][I].0.value
    const NESTED: u8 = [[31 as u8; N]; N][I][I]
    const LOCAL: u8 = local<N, I>()
    const CHANGED: [u8; N] = changed<N, I>()
}
const fn value() -> u8 { Marker<3, 1>::VALUE }
const fn concrete() -> u8 { Marker<3, 2>::CONCRETE }
const fn field() -> u8 { Marker<3, 1>::FIELD }
const fn nested() -> u8 { Marker<3, 2>::NESTED }
const fn local_value() -> u8 { Marker<3, 1>::LOCAL }
const fn changed_value() -> u8 { Marker<3, 1>::CHANGED[1] }
const fn unchanged_value() -> u8 { Marker<3, 1>::CHANGED[2] }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for (name, expected) in [
        ("value", 7),
        ("concrete", 11),
        ("field", 23),
        ("nested", 31),
        ("local_value", 13),
        ("changed_value", 19),
        ("unchanged_value", 0),
    ] {
        let value = eval_body_owner_const(
            &db,
            BodyOwner::Func(function(&db, module, name)),
            GenericSubst::for_body_owner(&db, BodyOwner::Func(function(&db, module, name)), vec![]),
        )
        .into_ready()
        .unwrap();
        assert_eq!(scalar(&db, value), expected, "{name}");
    }
}

#[test]
fn symbolic_array_indices_diagnose_invalid_specializations() {
    for (expression, len, index) in [
        ("[7; N][I]", 0, 0),
        ("[7; N][I]", 3, 3),
        ("[3, 5, 11][I]", 3, 3),
        ("[7; N][1 / I]", 3, 0),
        ("[10 / I; N][I]", 0, 0),
        ("[10 / I; N][I]", 3, 0),
        ("[Pair { value: 7 }; N][I].value", 3, 3),
        ("changed<N, I>()[0]", 3, 3),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let source = format!(
            r#"
struct Pair {{ value: usize }}
impl Copy for Pair {{}}
struct Marker<const N: usize, const I: usize> {{}}
const fn changed<const N: usize, const I: usize>() -> [usize; N] {{
    let mut values = [0; N]
    values[I] = 19
    values
}}
impl<const N: usize, const I: usize> Marker<N, I> {{
    const VALUE: usize = {expression}
}}
"#
        );
        let file = db.new_stand_alone("symbolic_index_template.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let file = db.new_stand_alone(
            "invalid_symbolic_indices.fe".into(),
            &format!("{source}\nconst BAD: usize = Marker<{len}, {index}>::VALUE"),
        );
        let (module, _) = db.top_mod(file);
        let diags = db.run_on_top_mod(module);
        let rendered = format_diagnostics(&db, &diags);
        assert!(!diags.is_empty(), "{expression}: len={len}, index={index}");
        assert!(!rendered.contains("internal"), "{rendered}");
    }
}

#[test]
fn symbolic_array_indices_reify_with_bounds_and_operand_checks() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "symbolic_index_reification.fe".into(),
        r#"
struct Pair { value: u8 }
impl Copy for Pair {}
const fn repeat<const N: usize, const I: usize>() -> u8 { [7; N][I] }
const fn concrete<const N: usize, const I: usize>() -> u8 { [3, 5, 11][I] }
const fn bytes<const N: usize, const I: usize>() -> u8 {
    let text: String<3> = "abc"
    text.as_bytes()[I]
}
const fn local<const N: usize, const I: usize>() -> u8 {
    let values = [13; N]
    values[I]
}
const fn field<const N: usize, const I: usize>() -> u8 { [Pair { value: 23 }; N][I].value }
const fn checked_index<const N: usize, const I: usize>() -> u8 { [7; N][1 / I] }
const fn checked_element<const N: usize, const I: usize>() -> usize { [10 / I; N][I] }
const fn store<const N: usize, const I: usize>() -> u8 {
    let mut values = [0; N]
    values[I] = 19
    values[I]
}
const fn changed<const N: usize, const I: usize>() -> u8 { store<N, I>() }
const fn fixed_store<const N: usize, const I: usize>() -> u8 {
    let mut values = [0; 3]
    values[I] = 29
    values[I]
}
const fn changed_fixed<const N: usize, const I: usize>() -> u8 { fixed_store<N, I>() }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for name in [
        "repeat",
        "concrete",
        "bytes",
        "local",
        "field",
        "checked_index",
        "checked_element",
        "changed",
        "changed_fixed",
    ] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let symbolic =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let identity = identity_semantic_instance_key(&db, owner);
        assert!(
            matches!(symbolic, EvalOutcome::Blocked(_)),
            "{name} must retain its unresolved index: {symbolic:?}"
        );
        for (len, index) in [(0, 0), (0, 1), (1, 0), (1, 1), (3, 1), (3, 2), (3, 3)] {
            let args = [len, index].map(|value: u32| {
                TyId::new(
                    &db,
                    TyData::ConstTy(ConstTyId::integer(
                        &db,
                        TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize))),
                        BigInt::from(value),
                    )),
                )
            });
            let key = SemanticInstanceKey::new(
                &db,
                owner,
                GenericSubst::for_body_owner(&db, owner, args.to_vec()),
                identity.effect_providers(&db),
                identity.impl_env(&db),
            );
            let instance = get_or_build_semantic_instance(&db, key);
            let reified = eval_const_instance(&db, instance).into_ready();
            let expected = match name {
                "concrete" => [3, 5, 11].get(index as usize).copied(),
                "bytes" => b"abc".get(index as usize).map(|byte| usize::from(*byte)),
                "changed_fixed" => (index < 3).then_some(29),
                "checked_index" => (index != 0 && 1 / index < len).then_some(7),
                "checked_element" => (index != 0 && index < len).then(|| 10 / index as usize),
                _ if index >= len => None,
                "repeat" => Some(7),
                "local" => Some(13),
                "field" => Some(23),
                "changed" => Some(19),
                _ => unreachable!(),
            };
            assert_eq!(
                reified.map(|value| scalar(&db, value)),
                expected,
                "{name}: len={len}, index={index}"
            );
        }
    }
}

#[test]
fn generic_inherent_const_array_repeats_specialize() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "array_repeat.fe".into(),
        r#"
struct Marker<const N: usize> {}
impl<const N: usize> Marker<N> {
    const VALUES: [u8; N] = [7; N]
    const MATRIX: [[u8; N]; 2] = [[9; N]; 2]
    const FIRST: u8 = Self::VALUES[0]
}
const fn first() -> u8 { Marker<1>::FIRST }
const fn last() -> u8 { Marker<3>::VALUES[2] }
const fn nested() -> u8 { Marker<4>::MATRIX[1][3] }
const fn empty() -> [u8; 0] { Marker<0>::VALUES }
const fn empty_nested() -> [[u8; 0]; 2] { Marker<0>::MATRIX }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for (name, expected) in [("first", 7), ("last", 7), ("nested", 9)] {
        let value = eval_body_owner_const(
            &db,
            BodyOwner::Func(function(&db, module, name)),
            GenericSubst::for_body_owner(&db, BodyOwner::Func(function(&db, module, name)), vec![]),
        )
        .into_ready()
        .unwrap();
        assert_eq!(scalar(&db, value), expected, "{name}");
    }
    let empty = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "empty")),
        GenericSubst::for_body_owner(&db, BodyOwner::Func(function(&db, module, "empty")), vec![]),
    )
    .into_ready()
    .unwrap();
    assert!(matches!(empty.value(&db), SemConstValue::Array { elems, .. } if elems.is_empty()));
    let nested = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "empty_nested")),
        GenericSubst::for_body_owner(
            &db,
            BodyOwner::Func(function(&db, module, "empty_nested")),
            vec![],
        ),
    )
    .into_ready()
    .unwrap();
    let SemConstValue::Array { elems, .. } = nested.value(&db) else {
        panic!("expected array")
    };
    assert_eq!(elems.len(), 2);
    assert!(elems.iter().all(
        |elem| matches!(elem.value(&db), SemConstValue::Array { elems, .. } if elems.is_empty())
    ));
}

#[test]
fn generic_record_const_array_repeats_specialize() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "record_repeat.fe".into(),
        r#"
struct Marker<const N: usize> { values: [u8; N] }
impl<const N: usize> Marker<N> {
    const VALUE: Marker<N> = Marker<N> { values: [9; N] }
}
const fn value() -> u8 { Marker<3>::VALUE.values[2] }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let value = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "value")),
        GenericSubst::for_body_owner(&db, BodyOwner::Func(function(&db, module, "value")), vec![]),
    )
    .into_ready()
    .unwrap();
    assert_eq!(scalar(&db, value), 9);
}

#[test]
fn symbolic_repeat_stores_defer_until_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "repeat_stores.fe".into(),
        r#"
struct Marker<const N: usize> { rows: [[u8; N]; 2] }
const fn changed<const N: usize>() -> [u8; N] {
    let mut values = [0; N]
    values[0] = 1
    values
}
const fn changed_record<const N: usize>() -> [[u8; N]; 2] {
    let mut value = Marker<N> { rows: [[7; N]; 2] }
    value.rows[0][1] = 2
    value.rows
}
const fn replaced<const N: usize>() -> [u8; N] {
    let mut values = [0; N]
    values = [3; N]
    values
}
impl<const N: usize> Marker<N> {
    const VALUES: [u8; N] = changed<N>()
    const ROWS: [[u8; N]; 2] = changed_record<N>()
    const REPLACED: [u8; N] = replaced<N>()
}
const fn first() -> u8 { Marker<1>::VALUES[0] }
const fn last() -> u8 { Marker<3>::VALUES[2] }
const fn nested() -> u8 { Marker<3>::ROWS[0][1] }
const fn sibling() -> u8 { Marker<3>::ROWS[1][1] }
const fn replacement() -> u8 { Marker<3>::REPLACED[2] }
const fn empty_replacement() -> [u8; 0] { Marker<0>::REPLACED }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for (name, expected) in [
        ("first", 1),
        ("last", 0),
        ("nested", 2),
        ("sibling", 7),
        ("replacement", 3),
    ] {
        let value = eval_body_owner_const(
            &db,
            BodyOwner::Func(function(&db, module, name)),
            GenericSubst::for_body_owner(&db, BodyOwner::Func(function(&db, module, name)), vec![]),
        )
        .into_ready()
        .unwrap();
        assert_eq!(scalar(&db, value), expected, "{name}");
    }
    let empty = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "empty_replacement")),
        GenericSubst::for_body_owner(
            &db,
            BodyOwner::Func(function(&db, module, "empty_replacement")),
            vec![],
        ),
    )
    .into_ready()
    .unwrap();
    assert!(matches!(empty.value(&db), SemConstValue::Array { elems, .. } if elems.is_empty()));
}

#[test]
fn symbolic_repeat_stores_preserve_bounds_checks() {
    for (len, index) in [(0, 0), (1, 1)] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "repeat_store_bounds.fe".into(),
            &format!(
                r#"
const fn changed<const N: usize>() -> [u8; N] {{
    let mut values = [0; N]
    values[{index}] = 1
    values
}}
struct Marker<const N: usize> {{}}
impl<const N: usize> Marker<N> {{
    const VALUES: [u8; N] = changed<N>()
}}
const fn bad() -> [u8; {len}] {{ changed<{len}>() }}
const BAD: [u8; {len}] = Marker<{len}>::VALUES
"#
            ),
        );
        let (module, _) = db.top_mod(file);
        let diags = db.run_on_top_mod(module);
        let rendered = format_diagnostics(&db, &diags);
        assert!(!diags.is_empty(), "out-of-bounds store must be diagnosed");
        assert!(!rendered.contains("internal"), "{rendered}");
        let result = eval_body_owner_const(
            &db,
            BodyOwner::Func(function(&db, module, "bad")),
            GenericSubst::for_body_owner(
                &db,
                BodyOwner::Func(function(&db, module, "bad")),
                vec![],
            ),
        );
        let EvalOutcome::Failed(EvalFailure::Ctfe(mut err)) = result else {
            panic!("expected CTFE failure: {result:?}");
        };
        while let CtfeError::CalleeError { source, .. } = err {
            err = *source;
        }
        assert!(matches!(err, CtfeError::OutOfBounds { .. }), "{err:?}");
    }
}

#[test]
fn symbolic_repeat_values_reify_after_element_and_length_substitution() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "repeat_reification.fe".into(),
        r#"
const fn repeat<const N: usize, const X: u8>() -> [[u8; N]; 2] { [[X; N]; 2] }
const fn project<const N: usize, const X: u8>() -> u8 { [X; N][1] }
const fn checked<const N: usize, const X: u8>() -> [[u8; N]; 2] { [[10 / X; N]; 2] }
const fn store<const N: usize>(_ value: u8) -> [[u8; N]; 2] {
    let mut rows = [[value; N]; 2]
    rows[0][1] = 10
    rows
}
const fn changed<const N: usize, const X: u8>() -> [[u8; N]; 2] { store<N>(X) }
const fn checked_changed<const N: usize, const X: u8>() -> [[u8; N]; 2] { store<N>(10 / X) }
struct Rows<const N: usize> { values: [[u8; N]; 2] }
const fn store_record<const N: usize>(_ value: u8) -> Rows<N> {
    let mut rows = Rows<N> { values: [[value; N]; 2] }
    rows.values[0][1] = 10
    rows
}
const fn record<const N: usize, const X: u8>() -> Rows<N> { store_record<N>(X) }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for name in [
        "repeat",
        "project",
        "checked",
        "changed",
        "checked_changed",
        "record",
    ] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let template =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let identity = identity_semantic_instance_key(&db, owner);
        assert!(matches!(template, EvalOutcome::Blocked(_)), "{template:?}");
        for (len, element) in [(0, 0), (0, 5), (1, 5), (2, 5), (4, 9)] {
            let args = const_args(&db, len, element);
            let key = SemanticInstanceKey::new(
                &db,
                owner,
                GenericSubst::for_body_owner(&db, owner, args.to_vec()),
                identity.effect_providers(&db),
                identity.impl_env(&db),
            );
            let expected = key.typed_body(&db).result_ty();
            let instance = get_or_build_semantic_instance(&db, key);
            let reified = match eval_const_instance(&db, instance) {
                EvalOutcome::Ready(value) => {
                    let reified = reify_runtime_const_for_ty(&db, instance, expected, value);
                    assert_eq!(
                        reify_runtime_const(&db, instance, value),
                        reified,
                        "{name} inferred result type"
                    );
                    reified
                }
                EvalOutcome::Failed(_) => None,
                blocked @ EvalOutcome::Blocked(_) => {
                    panic!("concrete instance blocked: {blocked:?}")
                }
            };
            if (matches!(name, "project" | "changed" | "checked_changed" | "record") && len < 2)
                || (matches!(name, "checked" | "checked_changed") && element == 0)
            {
                assert!(
                    reified.is_none(),
                    "{name} must retain its failure after substitution: len={len}, element={element}"
                );
                continue;
            }
            let reified = reified
                .unwrap_or_else(|| panic!("{name} failed to reify: len={len}, element={element}"));
            assert_eq!(sem_const_ty(&db, reified), expected, "{name} result type");
            assert!(
                !sem_const_ty(&db, reified).has_param(&db),
                "{name} result type"
            );
            let reified = if name == "record" {
                let SemConstValue::Struct { fields, .. } = reified.value(&db) else {
                    panic!("expected record")
                };
                fields[0]
            } else {
                reified
            };
            let expected = if matches!(name, "checked" | "checked_changed") {
                10 / element
            } else {
                element
            };
            if name == "project" {
                assert_eq!(scalar(&db, reified), element as usize);
            } else {
                let SemConstValue::Array { elems, .. } = reified.value(&db) else {
                    panic!("expected outer array")
                };
                assert_eq!(elems.len(), 2);
                assert!(!sem_const_ty(&db, reified).has_param(&db));
                for (row_index, row) in elems.iter().enumerate() {
                    assert!(!sem_const_ty(&db, *row).has_param(&db));
                    let SemConstValue::Array { elems, .. } = row.value(&db) else {
                        panic!("expected inner array")
                    };
                    assert_eq!(elems.len(), len as usize);
                    for (index, elem) in elems.iter().enumerate() {
                        let expected = if matches!(name, "changed" | "checked_changed" | "record")
                            && row_index == 0
                            && index == 1
                        {
                            10
                        } else {
                            expected as usize
                        };
                        assert_eq!(scalar(&db, *elem), expected);
                    }
                }
            }
        }
    }
}

#[test]
fn symbolic_repeat_element_access_preserves_bounds_checks() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "repeat_bounds.fe".into(),
        r#"
struct Marker<const N: usize> {}
impl<const N: usize> Marker<N> {
    const FIRST: u8 = [7; N][0]
}
const fn empty() -> u8 { Marker<0>::FIRST }
const BAD: u8 = Marker<0>::FIRST
"#,
    );
    let (module, _) = db.top_mod(file);
    let result = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "empty")),
        GenericSubst::for_body_owner(&db, BodyOwner::Func(function(&db, module, "empty")), vec![]),
    );
    assert!(
        matches!(result, EvalOutcome::Failed(_)),
        "empty repeated array must not produce an element: {result:?}"
    );
    assert!(
        !db.run_on_top_mod(module).is_empty(),
        "empty array indexing must be diagnosed"
    );
}

#[test]
fn symbolic_repeat_still_evaluates_its_element() {
    for source in [
        "fn runtime() -> u8 { 7 }\nstruct Marker<const N: usize> {}\nimpl<const N: usize> Marker<N> { const VALUE: [u8; N] = [runtime(); N] }",
        "struct Marker<const N: usize> {}\nimpl<const N: usize> Marker<N> { const VALUE: [u8; N] = [1 / 0; N] }",
        "fn runtime() -> u8 { 7 }\nconst BAD: [u8; 0] = [runtime(); 0]",
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("invalid_repeat.fe".into(), source);
        let (module, _) = db.top_mod(file);
        let diags = db.run_on_top_mod(module);
        assert!(
            !diags.is_empty(),
            "invalid repeat element was accepted: {source}"
        );
        let rendered = format_diagnostics(&db, &diags);
        assert!(!rendered.contains("internal"), "{rendered}");
    }
}

#[test]
fn trait_associated_lengths_and_elements_stay_symbolic() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "trait_repeat.fe".into(),
        r#"
trait Shape {
    const LEN: usize = 1
    const VALUE: u8 = 2
}
struct Large {}
impl Shape for Large {
    const LEN: usize = 4
    const VALUE: u8 = 9
}
struct Buffer<T> {}
impl<T: Shape> Buffer<T> {
    const VALUES: [u8; T::LEN] = [T::VALUE; T::LEN]
}
const fn value() -> u8 { Buffer<Large>::VALUES[3] }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let value = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "value")),
        GenericSubst::for_body_owner(&db, BodyOwner::Func(function(&db, module, "value")), vec![]),
    )
    .into_ready()
    .unwrap();
    assert_eq!(scalar(&db, value), 9);
}

#[test]
fn symbolic_repeat_enum_payloads_specialize() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "repeat_enum_payloads.fe".into(),
        r#"
enum Maybe<T> { None, Some(T) }
impl<T: Copy> Copy for Maybe<T> {}
struct Wrapped { item: Maybe<u8> }
impl Copy for Wrapped {}
struct Marker<const N: usize> {}
impl<const N: usize> Marker<N> {
    const VALUES: [Maybe<u8>; N] = [Maybe::Some(7); N]
    const NONE: [Maybe<u8>; N] = [Maybe::None; N]
    const NESTED: [(Wrapped, [Maybe<u8>; 1]); N] = [(Wrapped { item: Maybe::Some(9) }, [Maybe::Some(11); 1]); N]
}
const fn values() -> [Maybe<u8>; 3] { Marker<3>::VALUES }
const fn none() -> [Maybe<u8>; 2] { Marker<2>::NONE }
const fn nested() -> [(Wrapped, [Maybe<u8>; 1]); 2] { Marker<2>::NESTED }
const fn empty() -> [Maybe<u8>; 0] { Marker<0>::VALUES }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for (name, len) in [("values", 3), ("none", 2), ("nested", 2), ("empty", 0)] {
        let value = eval_body_owner_const(
            &db,
            BodyOwner::Func(function(&db, module, name)),
            GenericSubst::for_body_owner(&db, BodyOwner::Func(function(&db, module, name)), vec![]),
        )
        .into_ready()
        .unwrap();
        let SemConstValue::Array { elems, .. } = value.value(&db) else {
            panic!("expected array")
        };
        assert_eq!(elems.len(), len);
        for &elem in elems {
            let payloads = if name == "nested" {
                let SemConstValue::Tuple { elems, .. } = elem.value(&db) else {
                    panic!("expected tuple")
                };
                let SemConstValue::Struct { fields, .. } = elems[0].value(&db) else {
                    panic!("expected record")
                };
                let SemConstValue::Array { elems, .. } = elems[1].value(&db) else {
                    panic!("expected nested array")
                };
                vec![(fields[0], Some(9)), (elems[0], Some(11))]
            } else {
                vec![(elem, (name == "values").then_some(7))]
            };
            for (value, payload) in payloads {
                let SemConstValue::Enum {
                    variant, fields, ..
                } = value.value(&db)
                else {
                    panic!("expected enum")
                };
                assert_eq!(variant.0, u16::from(payload.is_some()));
                assert_eq!(fields.len(), usize::from(payload.is_some()));
                if let Some(expected) = payload {
                    assert_eq!(scalar(&db, fields[0]), expected);
                }
            }
        }
    }
}

#[test]
fn symbolic_repeat_enum_payloads_reify_after_substitution() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "repeat_enum_reification.fe".into(),
        r#"
enum Maybe<T> { None, Some(T) }
impl<T: Copy> Copy for Maybe<T> {}
struct Wrapped { item: Maybe<u8> }
impl Copy for Wrapped {}
const fn repeat<const N: usize, const X: u8>() -> [Wrapped; N] { [Wrapped { item: Maybe::Some(X) }; N] }
const fn checked<const N: usize, const X: u8>() -> [Wrapped; N] { [Wrapped { item: Maybe::Some(10 / X) }; N] }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for name in ["repeat", "checked"] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let symbolic =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let identity = identity_semantic_instance_key(&db, owner);
        assert!(matches!(symbolic, EvalOutcome::Blocked(_)), "{symbolic:?}");
        for (len, element) in [(0, 0), (0, 5), (2, 0), (3, 5)] {
            let args = const_args(&db, len, element);
            let key = SemanticInstanceKey::new(
                &db,
                owner,
                GenericSubst::for_body_owner(&db, owner, args.to_vec()),
                identity.effect_providers(&db),
                identity.impl_env(&db),
            );
            let reified =
                eval_const_instance(&db, get_or_build_semantic_instance(&db, key)).into_ready();
            if name == "checked" && element == 0 {
                assert!(
                    reified.is_none(),
                    "invalid payload must be retained even at length zero"
                );
                continue;
            }
            let reified =
                reified.unwrap_or_else(|| panic!("{name} failed: len={len}, element={element}"));
            assert!(!sem_const_ty(&db, reified).has_param(&db));
            let SemConstValue::Array { elems, .. } = reified.value(&db) else {
                panic!("expected array")
            };
            assert_eq!(elems.len(), len as usize);
            for elem in elems {
                let SemConstValue::Struct { fields, .. } = elem.value(&db) else {
                    panic!("expected record")
                };
                let SemConstValue::Enum {
                    variant, fields, ..
                } = fields[0].value(&db)
                else {
                    panic!("expected enum")
                };
                assert_eq!(variant.0, 1);
                assert_eq!(fields.len(), 1);
                assert_eq!(
                    scalar(&db, fields[0]),
                    if name == "checked" {
                        10 / element as usize
                    } else {
                        element as usize
                    }
                );
            }
        }
    }
}

#[test]
fn symbolic_repeat_field_projections_specialize() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "repeat_field_projections.fe".into(),
        r#"
struct Pair { value: u8 }
impl Copy for Pair {}
struct Marker<const N: usize> {}
impl<const N: usize> Marker<N> {
    const FIELD: u8 = [Pair { value: 7 }; N][0].value
    const TUPLE: u8 = [(9 as u8, 11 as u8); N][0].1
    const NESTED: u8 = [(Pair { value: 13 }, [17 as u8; 2]); N][0].0.value
    const ARRAY: u8 = [(Pair { value: 13 }, [17 as u8; 2]); N][0].1[1]
}
const fn field() -> u8 { Marker<3>::FIELD }
const fn tuple() -> u8 { Marker<1>::TUPLE }
const fn nested() -> u8 { Marker<2>::NESTED }
const fn array() -> u8 { Marker<2>::ARRAY }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for (name, expected) in [("field", 7), ("tuple", 11), ("nested", 13), ("array", 17)] {
        let value = eval_body_owner_const(
            &db,
            BodyOwner::Func(function(&db, module, name)),
            GenericSubst::for_body_owner(&db, BodyOwner::Func(function(&db, module, name)), vec![]),
        )
        .into_ready()
        .unwrap();
        assert_eq!(scalar(&db, value), expected, "{name}");
    }
}

#[test]
fn symbolic_repeat_field_projections_preserve_bounds_checks() {
    for projection in [
        "[Pair { value: 7 }; N][0].value",
        "[(9 as u8, 11 as u8); N][0].1",
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "repeat_field_bounds.fe".into(),
            &format!(
                r#"
struct Pair {{ value: u8 }}
impl Copy for Pair {{}}
struct Marker<const N: usize> {{}}
impl<const N: usize> Marker<N> {{ const VALUE: u8 = {projection} }}
const BAD: u8 = Marker<0>::VALUE
"#
            ),
        );
        let (module, _) = db.top_mod(file);
        let diags = db.run_on_top_mod(module);
        let rendered = format_diagnostics(&db, &diags);
        assert!(!diags.is_empty(), "out-of-bounds field access must fail");
        assert!(!rendered.contains("internal"), "{rendered}");
    }
}

#[test]
fn symbolic_repeat_field_projections_reify_after_substitution() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "repeat_field_reification.fe".into(),
        r#"
struct Pair { value: u8 }
impl Copy for Pair {}
const fn project<const N: usize, const X: u8>() -> u8 { [(Pair { value: X }, [X; 2]); N][0].0.value }
const fn checked<const N: usize, const X: u8>() -> u8 { [(Pair { value: X }, [10 / X; 2]); N][0].1[1] }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for name in ["project", "checked"] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let symbolic =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let identity = identity_semantic_instance_key(&db, owner);
        assert!(matches!(symbolic, EvalOutcome::Blocked(_)), "{symbolic:?}");
        for (len, element) in [(0, 5), (1, 0), (2, 5)] {
            let args = const_args(&db, len, element);
            let key = SemanticInstanceKey::new(
                &db,
                owner,
                GenericSubst::for_body_owner(&db, owner, args.to_vec()),
                identity.effect_providers(&db),
                identity.impl_env(&db),
            );
            let reified =
                eval_const_instance(&db, get_or_build_semantic_instance(&db, key)).into_ready();
            if len == 0 || (name == "checked" && element == 0) {
                assert!(
                    reified.is_none(),
                    "deferred projection must retain bounds and element errors"
                );
            } else {
                assert_eq!(
                    scalar(&db, reified.expect("concrete projected value")),
                    if name == "checked" {
                        10 / element as usize
                    } else {
                        element as usize
                    }
                );
            }
        }
    }
}

#[test]
fn runtime_const_reification_preserves_instantiated_aggregate_types() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "reification_expected_types.fe".into(),
        r#"
struct Marker<const A: usize, const B: usize> { value: u8 }
enum Wrapped<const A: usize, const B: usize> { Some(Marker<A, B>) }
const fn value<const A: usize, const B: usize>() -> (Marker<A, B>, [Wrapped<B, A>; 1]) {
    (Marker<A, B> { value: 7 }, [Wrapped::Some(Marker<B, A> { value: 9 })])
}
fn outer<const A: usize, const B: usize>() {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(function(&db, module, "value"));
    let template_value =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]))
            .into_ready()
            .unwrap();
    let identity = identity_semantic_instance_key(&db, owner);
    let outer =
        identity_semantic_instance_key(&db, BodyOwner::Func(function(&db, module, "outer")));
    let params = outer.subst(&db).generic_args(&db);
    for args in [[params[1], params[0]], [params[0], params[0]]] {
        let key = SemanticInstanceKey::new(
            &db,
            owner,
            GenericSubst::for_body_owner(&db, owner, args.to_vec()),
            identity.effect_providers(&db),
            identity.impl_env(&db),
        );
        let instance = get_or_build_semantic_instance(&db, key);
        let expected = key.typed_body(&db).result_ty();
        let reified = reify_runtime_const_for_ty(&db, instance, expected, template_value).unwrap();
        assert_eq!(
            sem_const_ty(&db, reified),
            expected,
            "explicit expected type must not be substituted again"
        );
        assert_eq!(
            reify_runtime_const(&db, instance, template_value),
            Some(reified),
            "declaration-owned expected type must be substituted once"
        );
        assert_eq!(
            reify_runtime_const_for_ty(&db, instance, expected, reified),
            Some(reified),
            "reification with the same explicit type must be stable"
        );
        let SemConstValue::Tuple { elems, .. } = reified.value(&db) else {
            panic!("expected tuple")
        };
        assert_eq!(sem_const_ty(&db, elems[0]).generic_args(&db), args);
        let SemConstValue::Array { ty, elems } = elems[1].value(&db) else {
            panic!("expected array")
        };
        assert_eq!(elems.len(), 1);
        let SemConstValue::Enum {
            ty: enum_ty,
            fields,
            ..
        } = elems[0].value(&db)
        else {
            panic!("expected enum")
        };
        assert_eq!(*enum_ty, ty.generic_args(&db)[0]);
        assert_eq!(enum_ty.generic_args(&db), [args[1], args[0]]);
        assert_eq!(
            sem_const_ty(&db, fields[0]).generic_args(&db),
            [args[1], args[0]]
        );
        let SemConstValue::Struct { fields, .. } = fields[0].value(&db) else {
            panic!("expected record")
        };
        assert_eq!(scalar(&db, fields[0]), 9);

        let body = canonicalize_semantic_consts(&db, instance).unwrap();
        let mut aggregate_count = 0;
        for stmt in body.blocks.iter().flat_map(|block| &block.stmts) {
            if let SStmtKind::Assign {
                dst,
                expr: SExpr::Const(SConst::Value(value)),
            } = &stmt.kind
                && matches!(
                    value.value().value(&db),
                    SemConstValue::Tuple { .. }
                        | SemConstValue::Array { .. }
                        | SemConstValue::Struct { .. }
                        | SemConstValue::Enum { .. }
                )
            {
                aggregate_count += 1;
                assert_eq!(
                    sem_const_ty(&db, value.value()),
                    body.local(*dst).unwrap().ty,
                    "canonicalized constant must retain its instantiated local type"
                );
            }
        }
        assert!(
            aggregate_count >= 4,
            "expected nested aggregate constants in the semantic body"
        );
    }
}

const DEPENDENT_LENGTHS: &str = r#"
const fn word_len(_ n: usize) -> usize { n / 32 + if n % 32 == 0 { 0 } else { 1 } }
const fn plus(_ n: usize) -> usize { n + 1 }
trait Width { const N: usize = 1 }
struct Two {}
impl Width for Two { const N: usize = 2 }
struct Five {}
impl Width for Five { const N: usize = 5 }
struct Packed<const N: usize> { words: [u256; word_len(N)] }
impl<const N: usize> Packed<N> {
    const ZERO: Packed<N> = Packed { words: [0; word_len(N)] }
    const fn filled(_ word: u256) -> Self { Self { words: [word; word_len(N)] } }
}
const fn packed_words<const N: usize>() -> [u256; word_len(N)] { Packed<N>::filled(7).words }
const fn zero_words<const N: usize>() -> [u256; word_len(N)] { Packed<N>::ZERO.words }
const fn computed<const N: usize>() -> [u8; { N + 1 }] { [1; { N + 1 }] }
const fn called<const N: usize>() -> [u8; plus(N)] { [2; plus(N)] }
const fn cast<const N: u8>() -> [u8; { N as usize }] { [3; { N as usize }] }
const fn summed<T0: Width, T1: Width>() -> [u8; { T0::N + T1::N }] { [4; { T0::N + T1::N }] }
const fn composed<const N: usize>() -> [u8; word_len(plus(N))] { [5; word_len(plus(N))] }
const fn branch<const N: usize>() -> u8 { [6 as u8; { if N == 0 { 1 } else { N } }][0] }
const fn bare<const N: usize>() -> [u8; N] { [7; N] }
const fn projected<T: Width>() -> [u8; T::N] { [8; T::N] }
const fn annotated<const N: usize>() -> [u8; word_len(N)] {
    let values: [u8; word_len(N)] = [9; word_len(N)]
    values
}
const fn conditional<const N: usize>() -> [u8; { if N > 1 && N < 9 { N } else if N == 0 { 1 } else { 2 } }] {
    [10; { if N > 1 && N < 9 { N } else if N == 0 { 1 } else { 2 } }]
}
const fn sized<T>() -> [u8; { core::size_of<T>() as usize }] { [11; { core::size_of<T>() as usize }] }
const fn concrete_sized() -> [u8; 32] { sized<u256>() }
const fn concrete_sized_small() -> [u8; 2] { sized<u16>() }
const fn concrete_summed() -> [u8; 7] { summed<Two, Five>() }
const fn concrete_projected() -> [u8; 5] { projected<Five>() }
const fn concrete_cast() -> [u8; 6] { cast<6>() }
const fn concrete_branch() -> u8 { branch<0>() }
"#;

fn array_elems<'db>(db: &'db HirAnalysisTestDb, value: SemConstId<'db>) -> Vec<usize> {
    let SemConstValue::Array { elems, .. } = value.value(db) else {
        panic!("expected array, got {:?}", value.value(db));
    };
    elems.iter().map(|elem| scalar(db, *elem)).collect()
}

#[test]
fn dependent_repeat_lengths_specialize() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("dependent_lengths.fe".into(), DEPENDENT_LENGTHS);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    let evaluate = |name: &str, len: Option<u32>| {
        let owner = BodyOwner::Func(function(&db, module, name));
        let Some(len) = len else {
            return eval_body_owner_const(
                &db,
                owner,
                GenericSubst::for_body_owner(&db, owner, vec![]),
            )
            .into_ready()
            .unwrap_or_else(|| panic!("{name} must evaluate"));
        };
        let identity = identity_semantic_instance_key(&db, owner);
        assert!(
            matches!(
                eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![])),
                EvalOutcome::Blocked(_)
            ),
            "{name} must wait for its length"
        );
        let arg = TyId::new(
            &db,
            TyData::ConstTy(ConstTyId::integer(&db, usize_ty, BigInt::from(len))),
        );
        let key = SemanticInstanceKey::new(
            &db,
            owner,
            GenericSubst::for_body_owner(&db, owner, vec![arg]),
            identity.effect_providers(&db),
            identity.impl_env(&db),
        );
        let instance = get_or_build_semantic_instance(&db, key);
        let value = eval_const_instance(&db, instance)
            .into_ready()
            .unwrap_or_else(|| panic!("{name}<{len}> must evaluate"));
        let expected = key.typed_body(&db).result_ty();
        let reified = reify_runtime_const_for_ty(&db, instance, expected, value)
            .unwrap_or_else(|| panic!("{name}<{len}> must reify"));
        assert_eq!(reify_runtime_const(&db, instance, value), Some(reified));
        assert_eq!(
            sem_const_ty(&db, reified),
            instance.normalized_ty(&db, expected),
            "{name}<{len}> type"
        );
        assert!(!sem_const_ty(&db, reified).has_param(&db));
        reified
    };
    for (bits, words) in [
        (0, 0),
        (1, 1),
        (31, 1),
        (32, 1),
        (33, 2),
        (63, 2),
        (64, 2),
        (65, 3),
    ] {
        for (name, element) in [("packed_words", 7), ("zero_words", 0)] {
            assert_eq!(
                array_elems(&db, evaluate(name, Some(bits))),
                vec![element; words],
                "{name}<{bits}>"
            );
        }
        assert_eq!(
            array_elems(&db, evaluate("annotated", Some(bits))),
            vec![9; words],
            "annotated<{bits}>"
        );
    }
    for (name, len, expected) in [
        ("computed", 0, vec![1]),
        ("computed", 3, vec![1; 4]),
        ("called", 3, vec![2; 4]),
        ("composed", 31, vec![5]),
        ("composed", 32, vec![5; 2]),
        ("bare", 0, vec![]),
        ("bare", 2, vec![7; 2]),
        ("conditional", 0, vec![10]),
        ("conditional", 1, vec![10; 2]),
        ("conditional", 5, vec![10; 5]),
        ("conditional", 9, vec![10; 2]),
    ] {
        assert_eq!(
            array_elems(&db, evaluate(name, Some(len))),
            expected,
            "{name}<{len}>"
        );
    }
    for (name, expected) in [
        ("concrete_summed", vec![4; 7]),
        ("concrete_projected", vec![8; 5]),
        ("concrete_cast", vec![3; 6]),
        ("concrete_sized", vec![11; 32]),
        ("concrete_sized_small", vec![11; 2]),
    ] {
        assert_eq!(array_elems(&db, evaluate(name, None)), expected, "{name}");
    }
    assert_eq!(scalar(&db, evaluate("concrete_branch", None)), 6);
}

#[test]
fn dependent_repeat_lengths_reject_unforceable_extents() {
    for (source, message) in [
        (
            "extern { const fn opaque() -> usize }\nfn closed() { let _values = [0 as u8; opaque()] }",
            "const value must be resolvable during type checking",
        ),
        (
            "fn runtime(_ n: usize) -> u8 { [0 as u8; { n + 1 }][0] }",
            "const value must be resolvable during type checking",
        ),
        (
            "fn ordinary(_ n: usize) -> usize { n }\nfn generic<const N: usize>() { let _values = [0 as u8; ordinary(N)] }",
            "non-const function call in const context",
        ),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("unforceable_length.fe".into(), source);
        let (module, _) = db.top_mod(file);
        let diags = db.run_on_top_mod(module);
        let rendered = format_diagnostics(&db, &diags);
        assert_eq!(diags.len(), 1, "{rendered}");
        assert!(rendered.contains(message), "{source}: {rendered}");
    }
}

#[test]
fn opaque_extern_lengths_fail_where_specialization_forces_them() {
    for expression in [
        "[7 as u8; opaque_of(N)][0]",
        "[7 as u8; { if N == 0 { opaque() } else { opaque() } }][0]",
        "[7 as u8; wrapped<N>()][0]",
    ] {
        let source = format!(
            r#"
extern {{
    const fn opaque() -> usize
    const fn opaque_of(_ n: usize) -> usize
}}
const fn wrapped<const N: usize>() -> usize {{ if N == 0 {{ opaque() }} else {{ opaque() }} }}
const fn probe<const N: usize>() -> u8 {{ {expression} }}
"#
        );
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("opaque_template.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "opaque_use.fe".into(),
            &format!("{source}const BAD: u8 = probe<1>()\n"),
        );
        let (module, _) = db.top_mod(file);
        let diags = db.run_on_top_mod(module);
        let rendered = format_diagnostics(&db, &diags);
        assert_eq!(diags.len(), 1, "{expression}: {rendered}");
        assert!(
            rendered.contains("cannot be evaluated at compile time"),
            "{expression}: {rendered}"
        );
        assert!(!rendered.contains("internal"), "{rendered}");
    }
}

#[test]
fn dependent_repeat_faults_report_at_concrete_use() {
    for (expression, call, fault) in [
        ("[7 as u8; { N - 1 }][0]", "probe<0, 1>()", "overflow"),
        ("[7 as u8; minus(N)][0]", "probe<0, 1>()", "overflow"),
        (
            "[7 as u8; { 10 / N }][0]",
            "probe<0, 1>()",
            "divide by zero",
        ),
        (
            "[7 as u8; [1, 2, 3][N - 1]][0]",
            "probe<4, 1>()",
            "index out of bounds",
        ),
        (
            "[7 as u8; { if N == 0 { 1 } else { 10 / (N - 1) } }][0]",
            "probe<1, 1>()",
            "divide by zero",
        ),
        (
            "{ let _values = [10 / X; word_len(N)]\n 0 }",
            "probe<0, 0>()",
            "divide by zero",
        ),
    ] {
        let source = format!(
            r#"
const fn word_len(_ n: usize) -> usize {{ n / 32 + if n % 32 == 0 {{ 0 }} else {{ 1 }} }}
const fn minus(_ n: usize) -> usize {{ n - 1 }}
const fn probe<const N: usize, const X: u8>() -> u8 {{ {expression} }}
"#
        );
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("dependent_fault_template.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "dependent_fault.fe".into(),
            &format!("{source}const VALID: u8 = probe<2, 1>()\nconst BAD: u8 = {call}\n"),
        );
        let (module, _) = db.top_mod(file);
        let diags = db.run_on_top_mod(module);
        let rendered = format_diagnostics(&db, &diags);
        assert_eq!(diags.len(), 1, "{expression}: {rendered}");
        assert!(rendered.contains(fault), "{expression}: {rendered}");
        assert!(!rendered.contains("internal"), "{rendered}");
    }
}

#[test]
fn runtime_admission_demands_specialized_layouts() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "runtime_repeat_admission.fe".into(),
        r#"
const fn minus(_ n: usize) -> usize { n - 1 }
struct Wrap<const N: usize> { values: [u8; minus(N)] }
fn runtime(_ value: u8) -> u8 { value }
fn first<const N: usize>() -> u8 { [7 as u8; { N - 1 }][0] }
fn unused<const N: usize>() { let _values = [7 as u8; { 10 / N }] }
fn make<const N: usize>(_ value: u8) -> [u8; { N - 1 }] { [value; { N - 1 }] }
fn receive<const N: usize>() { let _values = make<N>(runtime(7)) }
fn wrap<const N: usize>(_ value: u8) -> Wrap<N> { Wrap { values: [value; minus(N)] } }
fn receive_field<const N: usize>() { let _wrapped = wrap<N>(runtime(7)) }
fn produce<const N: usize>() -> [u8; { N - 1 }] { todo() }
fn returned<const N: usize>() -> u8 { produce<N>()[0] }
fn produced() -> u8 { produce<0>()[0] }
fn ignored<const N: usize>(_ wrapped: Wrap<N>) {}
fn take<const N: usize>(_ values: [u8; { N - 1 }]) -> u8 { 0 }
fn passes() -> u8 {
    let values: [u8; 0] = []
    take<0>(values)
}
fn passes_valid() -> u8 {
    let values: [u8; 1] = [1]
    take<2>(values)
}
struct Mixed<T, const N: usize> { values: [u8; { N - 1 }], item: T }
struct Masked<const N: usize> { huge: [u256; 0xffffffffffffffff], values: [u8; { N - 1 }] }
fn mixed<T>(_ mixed: Mixed<T, 0>) {}
fn mixed_valid<T>(_ mixed: Mixed<T, 1>) {}
fn masked(_ masked: Masked<0>) {}
fn masked_valid(_ masked: Masked<1>) {}
trait Has { type Item }
struct Faulty {}
struct Fine {}
impl Has for Faulty { type Item = Wrap<0> }
impl Has for Fine { type Item = Wrap<2> }
struct Outer<T: Has> { field: T::Item }
fn projected(_ outer: Outer<Faulty>) {}
fn projected_valid(_ outer: Outer<Fine>) {}
fn bounded<T: Has<Item = Wrap<0>>>(_ outer: Outer<T>) {}
fn bounded_valid<T: Has<Item = Wrap<2>>>(_ outer: Outer<T>) {}
struct Indexed<const N: usize> {}
impl<const N: usize> Has for Indexed<N> { type Item = [u8; { [3 as usize; 2][N] }] }
fn indexed(_ outer: Outer<Indexed<2>>) {}
fn indexed_valid(_ outer: Outer<Indexed<1>>) {}
fn measured(_ wrapped: Wrap<0>) -> usize { wrapped.values.len() }
fn measured_valid(_ wrapped: Wrap<2>) -> usize { wrapped.values.len() }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let usize_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    // A fault reaches a concrete instance through a repeat, a callee's return
    // or parameter type, or a parameter's field; folding a returned value
    // must not see it first. A field's fault is known even beside a generic
    // or an oversized field, or behind a projection the instance resolves,
    // including an impl's deferred extent, and when a method call on the
    // faulting value leaves the body unlowerable.
    for (name, len, valid) in [
        ("first", Some(2), true),
        ("first", Some(0), false),
        ("unused", Some(5), true),
        ("unused", Some(0), false),
        ("receive", Some(2), true),
        ("receive", Some(0), false),
        ("receive_field", Some(2), true),
        ("receive_field", Some(0), false),
        ("returned", Some(2), true),
        ("returned", Some(0), false),
        ("ignored", Some(2), true),
        ("ignored", Some(0), false),
        ("produced", None, false),
        ("passes", None, false),
        ("passes_valid", None, true),
        ("mixed", None, false),
        ("mixed_valid", None, true),
        ("masked", None, false),
        ("masked_valid", None, true),
        ("projected", None, false),
        ("projected_valid", None, true),
        ("bounded", None, false),
        ("bounded_valid", None, true),
        ("indexed", None, false),
        ("indexed_valid", None, true),
        ("measured", None, false),
        ("measured_valid", None, true),
    ] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let identity = identity_semantic_instance_key(&db, owner);
        let key = match len {
            Some(len) => {
                assert!(
                    normalize_semantic_body(&db, get_or_build_semantic_instance(&db, identity))
                        .is_ok(),
                    "{name} template must be admitted"
                );
                let arg = TyId::new(
                    &db,
                    TyData::ConstTy(ConstTyId::integer(&db, usize_ty, BigInt::from(len))),
                );
                SemanticInstanceKey::new(
                    &db,
                    owner,
                    GenericSubst::for_body_owner(&db, owner, vec![arg]),
                    identity.effect_providers(&db),
                    identity.impl_env(&db),
                )
            }
            None => identity,
        };
        let instance = get_or_build_semantic_instance(&db, key);
        for result in [
            normalize_semantic_body(&db, instance),
            normalize_runtime_semantic_body(&db, instance),
        ] {
            match result {
                Ok(_) => assert!(valid, "{name}<{len:?}> must be rejected"),
                Err(SemanticNormalizationFailure::Rejected(diag)) => {
                    assert!(!valid, "{name}<{len:?}> must be admitted: {diag:?}");
                    assert_eq!(diag.kind, SemanticDiagnosticKind::InvalidConcreteType);
                    assert_eq!(diag.secondaries.len(), 1, "{diag:?}");
                }
                Err(failure) => panic!("{name}<{len:?}>: unexpected failure {failure:?}"),
            }
        }
    }
}

#[test]
fn runtime_homes_share_the_normalized_value_types() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "runtime_homes.fe".into(),
        r#"
trait HasN { const N: usize }
struct Big {}
impl HasN for Big { const N: usize = 3 }
fn ret<T: HasN, const M: usize>() -> [u8; { T::N + M }] {
    let values: [u8; { T::N + M }] = [3; { T::N + M }]
    values
}
fn caller() -> u8 { ret<Big, 1>()[2] }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let caller = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(function(&db, module, "caller"))),
    );
    let key = caller
        .body(&db)
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|stmt| match &stmt.kind {
            SStmtKind::Assign {
                expr: SExpr::Call { callee, .. },
                ..
            } => Some(callee.key),
            _ => None,
        })
        .expect("caller calls ret");
    let instance = get_or_build_semantic_instance(&db, key);
    let runtime = normalize_runtime_semantic_body(&db, instance).expect("runtime body");
    let homes = NLayoutLocals::new(&db, &runtime.body, &runtime.layout_plan, instance.body(&db));
    for (index, value) in runtime.body.values.iter().enumerate() {
        let home = homes
            .value_local(NValueId::new(index))
            .expect("every value has a runtime home");
        assert_eq!(
            homes.locals[home.index()].ty,
            value.ty,
            "a folded value and its home must share one type identity"
        );
    }
}

#[test]
fn blocked_bodies_report_only_evaluation_faults() {
    // A body blocked by a reported type error stays blocked; one that also
    // consumes a faulting extent reports that fault, which type checking
    // left to concrete demand.
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "blocked_faults.fe".into(),
        r#"
const fn minus(_ n: usize) -> usize { n - 1 }
struct Wrap<const N: usize> { values: [u8; minus(N)] }
fn upstream(_ value: mut u256) -> mut u256 { value as mut u256 }
fn both(_ wrapped: Wrap<0>, _ value: mut u256) -> mut u256 {
    let _length = wrapped.values.len()
    value as mut u256
}
"#,
    );
    let (module, _) = db.top_mod(file);
    for (name, rejected) in [("upstream", false), ("both", true)] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let instance =
            get_or_build_semantic_instance(&db, identity_semantic_instance_key(&db, owner));
        match normalize_semantic_body(&db, instance) {
            Err(SemanticNormalizationFailure::Rejected(diag)) if rejected => {
                assert_eq!(diag.kind, SemanticDiagnosticKind::InvalidConcreteType);
            }
            Err(SemanticNormalizationFailure::Blocked(_)) if !rejected => {}
            other => panic!("{name}: unexpected admission {other:?}"),
        }
    }
}
