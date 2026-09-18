use fe_hir::{
    analysis::{
        semantic::{
            GenericSubst, SemConstId, SemConstScalar, SemConstValue, SemanticInstanceKey,
            eval_body_owner_const, get_or_build_semantic_instance, identity_semantic_instance_key,
            instantiate_with_generic_args, reify_runtime_const_for_ty, sem_const_ty,
        },
        ty::{
            const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
            ty_check::BodyOwner,
            ty_def::{PrimTy, TyBase, TyData, TyId},
        },
    },
    hir_def::{Func, IntegerId, Partial, TopLevelMod},
    test_db::{HirAnalysisTestDb, format_diagnostics},
};
use num_bigint::BigUint;
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
        let value =
            eval_body_owner_const(&db, BodyOwner::Func(function(&db, module, name)), vec![])
                .unwrap();
        assert_eq!(scalar(&db, value), expected, "{name}");
    }
    let empty = eval_body_owner_const(&db, BodyOwner::Func(function(&db, module, "empty")), vec![])
        .unwrap();
    assert!(matches!(empty.value(&db), SemConstValue::Array { elems, .. } if elems.is_empty()));
    let nested = eval_body_owner_const(
        &db,
        BodyOwner::Func(function(&db, module, "empty_nested")),
        vec![],
    )
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
    let value = eval_body_owner_const(&db, BodyOwner::Func(function(&db, module, "value")), vec![])
        .unwrap();
    assert_eq!(scalar(&db, value), 9);
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
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for name in ["repeat", "project", "checked"] {
        let owner = BodyOwner::Func(function(&db, module, name));
        let value = eval_body_owner_const(&db, owner, vec![]).unwrap();
        for (len, element) in [(0, 0), (0, 5), (1, 5), (2, 5), (4, 9)] {
            let args =
                [(PrimTy::Usize, len), (PrimTy::U8, element)].map(|(prim, value): (_, u32)| {
                    let ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(prim)));
                    TyId::new(
                        &db,
                        TyData::ConstTy(ConstTyId::new(
                            &db,
                            ConstTyData::Evaluated(
                                EvaluatedConstTy::LitInt(IntegerId::new(&db, BigUint::from(value))),
                                ty,
                            ),
                        )),
                    )
                });
            let identity = identity_semantic_instance_key(&db, owner);
            let key = SemanticInstanceKey::new(
                &db,
                owner,
                GenericSubst::new(&db, args.to_vec()),
                identity.effect_providers(&db),
                identity.impl_env(&db),
            );
            let expected = instantiate_with_generic_args(&db, sem_const_ty(&db, value), &args);
            let reified = reify_runtime_const_for_ty(
                &db,
                get_or_build_semantic_instance(&db, key),
                expected,
                value,
            );
            if (name == "project" && len < 2) || (name == "checked" && element == 0) {
                assert!(
                    reified.is_none(),
                    "{name} must retain its failure after substitution: len={len}, element={element}"
                );
                continue;
            }
            let reified = reified
                .unwrap_or_else(|| panic!("{name} failed to reify: len={len}, element={element}"));
            let expected = if name == "checked" {
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
                for row in elems.iter() {
                    let SemConstValue::Array { elems, .. } = row.value(&db) else {
                        panic!("expected inner array")
                    };
                    assert_eq!(elems.len(), len as usize);
                    assert!(
                        elems
                            .iter()
                            .all(|elem| scalar(&db, *elem) == expected as usize)
                    );
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
    let result =
        eval_body_owner_const(&db, BodyOwner::Func(function(&db, module, "empty")), vec![]);
    assert!(
        result.is_err(),
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
    let value = eval_body_owner_const(&db, BodyOwner::Func(function(&db, module, "value")), vec![])
        .unwrap();
    assert_eq!(scalar(&db, value), 9);
}
