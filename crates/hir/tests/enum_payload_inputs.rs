use fe_hir::{
    analysis::{
        semantic::{
            EvalOutcome, GenericSubst, SemConstId, SemConstValue, eval_body_owner_const_with_args,
        },
        ty::{corelib::resolve_lib_type_path, ty_check::BodyOwner, ty_def::TyId},
    },
    core::meta::{MetaValue, RecordInput, RecordInputError},
    test_db::{HirAnalysisTestDb, find_func},
};

#[test]
fn authored_payload_fields_preserve_owner_variant_order_names_and_types() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("payload-metadata.fe".into(), "enum Shape { Rect { x: u256, y: u256, visible: bool }, Empty, Pair(u64, bool) }\nenum Twin { Rect { x: u256, y: u256, visible: bool }, Empty, Pair(u64, bool) }\nstruct Plain { x: u256 }\n");
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let resolve = |name| resolve_lib_type_path(&db, module.scope(), name).unwrap();
    let root = RecordInput::for_authored_type(&db, resolve("Shape")).unwrap();
    let rect = root.variant(&db, 0).unwrap();
    let empty = root.variant(&db, 1).unwrap();
    let pair = root.variant(&db, 2).unwrap();
    assert_eq!(
        [
            rect.field_count(&db).unwrap(),
            empty.field_count(&db).unwrap(),
            pair.field_count(&db).unwrap()
        ],
        [3, 0, 2]
    );
    assert_ne!(rect.field(&db, 0).unwrap(), rect.field(&db, 1).unwrap());
    assert_eq!(rect.selected_variant(), Some(0));
    assert_eq!(pair.selected_variant(), Some(2));
    assert_eq!(rect.ty(), root.ty());
    assert_eq!(pair.ty(), root.ty());
    assert_eq!(
        rect.field(&db, 2).unwrap().name(&db).unwrap().data(&db),
        "visible"
    );
    assert_eq!(rect.field(&db, 2).unwrap().ty(&db), TyId::bool(&db));
    assert_eq!(pair.field(&db, 1).unwrap().name(&db), None);
    assert_eq!(pair.field(&db, 1).unwrap().ty(&db), TyId::bool(&db));
    assert_eq!(
        pair.field(&db, 0).unwrap().ty(&db),
        TyId::new(
            &db,
            fe_hir::analysis::ty::ty_def::TyData::TyBase(
                fe_hir::analysis::ty::ty_def::TyBase::Prim(
                    fe_hir::analysis::ty::ty_def::PrimTy::U64
                )
            )
        )
    );
    assert_ne!(rect.field(&db, 2).unwrap(), pair.field(&db, 1).unwrap());
    let twin = RecordInput::for_authored_type(&db, resolve("Twin")).unwrap();
    assert_ne!(
        rect.field(&db, 0).unwrap(),
        twin.variant(&db, 0).unwrap().field(&db, 0).unwrap()
    );
    assert_eq!(
        root.variant(&db, 3),
        Err(RecordInputError::VariantOutsideEnum)
    );
    assert_eq!(
        empty.field(&db, 0),
        Err(RecordInputError::FieldOutsideRecord)
    );
    assert_eq!(
        rect.variant_count(&db),
        Err(RecordInputError::SelectedPayloadIsNotWholeEnum)
    );
    assert_eq!(
        rect.variant(&db, 0),
        Err(RecordInputError::SelectedPayloadIsNotWholeEnum)
    );
    assert_eq!(
        RecordInput::for_authored_type(&db, resolve("Plain"))
            .unwrap()
            .variant(&db, 0),
        Err(RecordInputError::NotEnum)
    );
}

#[test]
fn selected_record_fieldtype_and_boolean_query_use_existing_verified_ctfe() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("payload-provider.fe".into(), "use core::meta::{Record, FieldType, variant, field, field_type, type_is}\nenum Shape { Value(bool), Other(u256) }\npub struct Counterfeit { pub seal: bool }\nconst fn select(_ input: Record) -> Record { variant(input, 0) }\nconst fn inspect(_ input: Record) -> FieldType { field_type(field(input, 0)) }\nconst fn check(_ input: Record) -> bool { type_is<bool>(field_type(field(input, 0))) && !type_is<u256>(field_type(field(input, 0))) }\n");
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let resolve = |name| resolve_lib_type_path(&db, module.scope(), name).unwrap();
    let root = RecordInput::for_authored_type(&db, resolve("Shape")).unwrap();
    let carrier = resolve("core::meta::Record");
    let issue = |input| {
        SemConstId::new(
            &db,
            SemConstValue::Meta {
                ty: carrier,
                value: MetaValue::Record(input),
            },
        )
    };
    let eval = |name, value| {
        let owner = BodyOwner::Func(find_func(&db, module, name), None);
        eval_body_owner_const_with_args(
            &db,
            owner,
            GenericSubst::for_body_owner(&db, owner, vec![]),
            vec![value],
        )
    };
    let outcome = eval("select", issue(root));
    let EvalOutcome::Ready(selected) = outcome else {
        panic!("{outcome:?}")
    };
    assert!(
        matches!(selected.value(&db),SemConstValue::Meta {value:MetaValue::Record(input),..} if input.selected_variant()==Some(0) && input.ty()==root.ty())
    );
    let outcome = eval("inspect", selected);
    let EvalOutcome::Ready(value) = outcome else {
        panic!("{outcome:?}")
    };
    assert!(
        matches!(value.value(&db),SemConstValue::Meta {value:MetaValue::FieldType(field),..} if field.ty(&db)==TyId::bool(&db) && field.record().selected_variant()==Some(0))
    );
    let outcome = eval("check", selected);
    let EvalOutcome::Ready(value) = outcome else {
        panic!("{outcome:?}")
    };
    assert!(matches!(
        value.value(&db),
        SemConstValue::Scalar {
            value: fe_hir::analysis::semantic::SemConstScalar::Bool(true),
            ..
        }
    ));
    let wrong = SemConstId::new(
        &db,
        SemConstValue::Meta {
            ty: resolve("Counterfeit"),
            value: MetaValue::Record(root.variant(&db, 0).unwrap()),
        },
    );
    let refusal = eval("inspect", wrong);
    assert!(
        matches!(refusal, EvalOutcome::Failed(_))
            && format!("{refusal:?}").contains("different declared carrier type"),
        "{refusal:?}"
    );
}
