use fe_hir::{
    analysis::{
        semantic::{
            EvalOutcome, GenericSubst, SemConstId, SemConstScalar, SemConstValue,
            eval_body_owner_const_with_args,
        },
        ty::{corelib::resolve_lib_type_path, ty_check::BodyOwner, ty_def::TyId},
    },
    core::meta::{MetaValue, RecordInput, RecordInputError},
    test_db::{HirAnalysisTestDb, find_func},
};

#[test]
fn enum_metadata_keeps_authored_nominal_identity_and_rejects_wrong_roles() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("enum-input.fe".into(),
        "enum Shape { Rect { a: u256, b: bool, c: u256 }, Empty, Pair(u256, bool) }\nenum Twin { Rect { a: u256, b: bool, c: u256 }, Empty, Pair(u256, bool) }\nstruct Plain { x: u256 }\nenum Open<T> { Value(T) }\nenum Bad { Value(Missing) }\n");
    let (module, _) = db.top_mod(file);
    let resolve = |name| resolve_lib_type_path(&db, module.scope(), name).unwrap();
    let shape = RecordInput::for_authored_type(&db, resolve("Shape")).unwrap();
    let twin = RecordInput::for_authored_type(&db, resolve("Twin")).unwrap();
    assert_ne!(shape, twin);
    assert_eq!(shape.ty(), resolve("Shape"));
    assert_eq!(shape.declaration().name(&db).unwrap().data(&db), "Shape");
    assert_eq!(shape.variant_count(&db), Ok(3));
    assert_eq!(
        shape.field_count(&db),
        Err(RecordInputError::WholeEnumHasNoFields)
    );
    assert_eq!(
        shape.field(&db, 0),
        Err(RecordInputError::WholeEnumHasNoFields)
    );
    let plain = RecordInput::for_authored_type(&db, resolve("Plain")).unwrap();
    assert_eq!(plain.variant_count(&db), Err(RecordInputError::NotEnum));
    assert_eq!(plain.field_count(&db), Ok(1));
    assert_eq!(
        RecordInput::for_authored_type(&db, TyId::u256(&db)),
        Err(RecordInputError::NotRecord)
    );
    assert_eq!(
        RecordInput::for_authored_type(&db, resolve("Open")),
        Err(RecordInputError::NotConcrete)
    );
    assert_eq!(
        RecordInput::for_authored_type(&db, resolve("Bad")),
        Err(RecordInputError::InvalidField)
    );
}

#[test]
fn enum_count_uses_checked_ctfe_and_genuine_carrier_authority() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("enum-provider.fe".into(),
        "use core::meta::{Record, variant_count, field_count}\nenum Shape { Rect(u256, bool), Empty, Pair(u256) }\nenum Empty {}\nstruct Plain { x: u256 }\npub struct Counterfeit { pub seal: bool }\nconst fn count(_ record: Record) -> usize { variant_count(record) + variant_count(record) }\nconst fn fields(_ record: Record) -> usize { field_count(record) }\n");
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let func = find_func(&db, module, "count");
    let carrier = resolve_lib_type_path(&db, func.scope(), "core::meta::Record").unwrap();
    let target = resolve_lib_type_path(&db, func.scope(), "Shape").unwrap();
    let input = RecordInput::for_authored_type(&db, target).unwrap();
    let issued = SemConstId::new(
        &db,
        SemConstValue::Meta {
            ty: carrier,
            value: MetaValue::Record(input),
        },
    );
    let eval = |name, value| {
        let owner = BodyOwner::Func(find_func(&db, module, name), None);
        eval_body_owner_const_with_args(
            &db,
            owner,
            GenericSubst::for_body_owner(&db, owner, vec![]),
            vec![value],
        )
    };
    let outcome = eval("count", issued);
    let EvalOutcome::Ready(value) = outcome else {
        panic!("{outcome:?}")
    };
    assert!(
        matches!(value.value(&db), SemConstValue::Scalar { value: SemConstScalar::Int { value }, .. } if value.to_string() == "6")
    );
    let empty = RecordInput::for_authored_type(
        &db,
        resolve_lib_type_path(&db, func.scope(), "Empty").unwrap(),
    )
    .unwrap();
    let outcome = eval(
        "count",
        SemConstId::new(
            &db,
            SemConstValue::Meta {
                ty: carrier,
                value: MetaValue::Record(empty),
            },
        ),
    );
    let EvalOutcome::Ready(value) = outcome else {
        panic!("{outcome:?}")
    };
    assert!(
        matches!(value.value(&db), SemConstValue::Scalar { value: SemConstScalar::Int { value }, .. } if value.to_string() == "0")
    );
    let refusal = eval("fields", issued);
    assert!(
        matches!(refusal, EvalOutcome::Failed(_))
            && format!("{refusal:?}").contains("whole enum has no selected payload"),
        "{refusal:?}"
    );
    let forged = SemConstId::new(
        &db,
        SemConstValue::Struct {
            ty: carrier,
            fields: vec![fe_hir::analysis::semantic::bool_const(&db, false)].into_boxed_slice(),
        },
    );
    let refusal = eval("count", forged);
    assert!(
        matches!(refusal, EvalOutcome::Failed(_))
            && format!("{refusal:?}").contains("variant_count requires a compiler-issued Record"),
        "{refusal:?}"
    );
    let counterfeit = resolve_lib_type_path(&db, func.scope(), "Counterfeit").unwrap();
    let wrong_origin = SemConstId::new(
        &db,
        SemConstValue::Meta {
            ty: counterfeit,
            value: MetaValue::Record(input),
        },
    );
    let refusal = eval("count", wrong_origin);
    assert!(
        matches!(refusal, EvalOutcome::Failed(_))
            && format!("{refusal:?}")
                .contains("record input payload has a different declared carrier type"),
        "{refusal:?}"
    );
    let plain = RecordInput::for_authored_type(
        &db,
        resolve_lib_type_path(&db, func.scope(), "Plain").unwrap(),
    )
    .unwrap();
    let refusal = eval(
        "count",
        SemConstId::new(
            &db,
            SemConstValue::Meta {
                ty: carrier,
                value: MetaValue::Record(plain),
            },
        ),
    );
    assert!(
        matches!(refusal, EvalOutcome::Failed(_))
            && format!("{refusal:?}").contains("variant_count requires a whole authored enum"),
        "{refusal:?}"
    );
}
