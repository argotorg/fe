use camino::Utf8PathBuf;
use fe_hir::analysis::ty::{
    const_ty::ConstTyData, corelib::resolve_lib_type_path, ty_contains_const_hole, ty_def::TyData,
};
use fe_hir::hir_def::{CallableDef, IdentId, ItemKind};
use fe_hir::test_db::HirAnalysisTestDb;

#[test]
fn assoc_type_layout_holes_use_assumptions_for_collection() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("assoc_type_layout_holes_use_assumptions_for_collection.fe"),
        r#"
struct Slot<T, const ROOT: u256 = _> {}

trait HasSlot {
    type Assoc
}

fn f<T: HasSlot<Assoc = Slot<u256>>>(x: T::Assoc) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func) if func.name(&db).to_opt().is_some_and(|n| n.data(&db) == "f") => {
                Some(func)
            }
            _ => None,
        })
        .expect("missing `f` function");

    let implicit_layout_params = CallableDef::Func(func)
        .params(&db)
        .iter()
        .filter(|ty| {
            matches!(
                ty.data(&db),
                TyData::ConstTy(const_ty)
                    if matches!(const_ty.data(&db), ConstTyData::TyParam(param, _) if param.is_implicit())
            )
        })
        .count();
    assert_eq!(implicit_layout_params, 1);

    for ty in func.arg_tys(&db) {
        let ty = ty.instantiate_identity();
        assert!(
            !ty_contains_const_hole(&db, ty),
            "unelaborated const hole remained in function argument type: {ty:?}"
        );
    }
}

#[test]
fn trait_effect_keys_collect_and_elaborate_layout_holes() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_effect_keys_collect_and_elaborate_layout_holes.fe"),
        r#"
trait Cap<T> {}

struct Slot<T, const ROOT: u256 = _> {}

fn f() uses (cap: Cap<Slot<u256>>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func) if func.name(&db).to_opt().is_some_and(|n| n.data(&db) == "f") => {
                Some(func)
            }
            _ => None,
        })
        .expect("missing `f` function");

    let implicit_layout_params = CallableDef::Func(func)
        .params(&db)
        .iter()
        .filter(|ty| {
            matches!(
                ty.data(&db),
                TyData::ConstTy(const_ty)
                    if matches!(const_ty.data(&db), ConstTyData::TyParam(param, _) if param.is_implicit())
            )
        })
        .count();
    assert_eq!(implicit_layout_params, 1);

    let effect_binding = func
        .effect_bindings(&db)
        .first()
        .expect("missing effect binding");
    let key_trait = effect_binding.key_trait.expect("missing trait effect key");
    assert!(
        key_trait
            .args(&db)
            .iter()
            .copied()
            .all(|arg| !ty_contains_const_hole(&db, arg)),
        "unelaborated const hole remained in trait effect key: {key_trait:?}"
    );
}

#[test]
fn type_effect_keys_use_assumptions_for_collection() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("type_effect_keys_use_assumptions_for_collection.fe"),
        r#"
trait HasRootTy {
    type RootTy
}

struct Slot<T: HasRootTy<RootTy = u256>, const ROOT: T::RootTy = _> {}

fn f<T: HasRootTy<RootTy = u256>>() uses (slot: Slot<T>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func) if func.name(&db).to_opt().is_some_and(|n| n.data(&db) == "f") => {
                Some(func)
            }
            _ => None,
        })
        .expect("missing `f` function");

    let implicit_layout_params = CallableDef::Func(func)
        .params(&db)
        .iter()
        .filter(|ty| {
            matches!(
                ty.data(&db),
                TyData::ConstTy(const_ty)
                    if matches!(const_ty.data(&db), ConstTyData::TyParam(param, _) if param.is_implicit())
            )
        })
        .count();
    assert_eq!(implicit_layout_params, 1);

    let effect_binding = func
        .effect_bindings(&db)
        .first()
        .expect("missing effect binding");
    let key_ty = effect_binding.key_ty.expect("missing type effect key");
    assert!(
        !ty_contains_const_hole(&db, key_ty),
        "unelaborated const hole remained in type effect key: {key_ty:?}"
    );
}

#[test]
fn contract_field_layout_uses_consistent_effect_handle_metadata() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_layout_uses_consistent_effect_handle_metadata.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

contract C {
    value: StorPtr<Slot<u256>>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Contract(contract)
                if contract
                    .name(&db)
                    .to_opt()
                    .is_some_and(|n| n.data(&db) == "C") =>
            {
                Some(contract)
            }
            _ => None,
        })
        .expect("missing `C` contract");

    let field_name = IdentId::new(&db, "value".to_string());
    let field_layout = contract
        .field_layout(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `value` field layout");
    let field_info = contract
        .fields(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `value` field info");
    let storage = resolve_lib_type_path(&db, contract.scope(), "core::effect_ref::Storage")
        .expect("missing storage address space");

    assert!(field_layout.is_provider);
    assert_eq!(field_layout.address_space, storage);
    assert_eq!(field_layout.declared_ty, field_info.declared_ty);
    assert_eq!(field_layout.target_ty, field_info.target_ty);
    assert_eq!(field_layout.is_provider, field_info.is_provider);
    assert!(
        !ty_contains_const_hole(&db, field_layout.declared_ty),
        "unelaborated const hole remained in contract field type: {:?}",
        field_layout.declared_ty
    );
    assert!(
        !ty_contains_const_hole(&db, field_layout.target_ty),
        "unelaborated const hole remained in contract field target type: {:?}",
        field_layout.target_ty
    );
}

#[test]
fn contract_field_layout_partitions_slots_by_address_space() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_layout_partitions_slots_by_address_space.fe"),
        r#"
use core::effect_ref::{MemPtr, StorPtr}

struct Slot<const ROOT: u256 = _> {}

contract C {
    storage0: StorPtr<Slot>
    memory0: MemPtr<Slot>
    storage1: StorPtr<Slot>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Contract(contract)
                if contract
                    .name(&db)
                    .to_opt()
                    .is_some_and(|n| n.data(&db) == "C") =>
            {
                Some(contract)
            }
            _ => None,
        })
        .expect("missing `C` contract");

    let layout = contract.field_layout(&db);
    let storage = resolve_lib_type_path(&db, contract.scope(), "core::effect_ref::Storage")
        .expect("missing storage address space");
    let memory = resolve_lib_type_path(&db, contract.scope(), "core::effect_ref::Memory")
        .expect("missing memory address space");

    let storage0 = layout
        .get(&IdentId::new(&db, "storage0".to_string()))
        .expect("missing `storage0` field");
    let memory0 = layout
        .get(&IdentId::new(&db, "memory0".to_string()))
        .expect("missing `memory0` field");
    let storage1 = layout
        .get(&IdentId::new(&db, "storage1".to_string()))
        .expect("missing `storage1` field");

    assert_eq!(storage0.address_space, storage);
    assert_eq!(memory0.address_space, memory);
    assert_eq!(storage1.address_space, storage);
    assert_eq!(storage0.slot_offset, 0);
    assert_eq!(memory0.slot_offset, 0);
    assert_eq!(storage1.slot_offset, 1);
    assert_eq!(storage0.slot_count, 1);
    assert_eq!(memory0.slot_count, 1);
    assert_eq!(storage1.slot_count, 1);
}

#[test]
fn contract_field_layout_reuses_repeated_placeholder_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_layout_reuses_repeated_placeholder_identity.fe"),
        r#"
use core::effect_ref::StorPtr

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

contract C {
    value: StorPtr<Repeated>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Contract(contract)
                if contract
                    .name(&db)
                    .to_opt()
                    .is_some_and(|n| n.data(&db) == "C") =>
            {
                Some(contract)
            }
            _ => None,
        })
        .expect("missing `C` contract");

    let field = contract
        .field_layout(&db)
        .get(&IdentId::new(&db, "value".to_string()))
        .cloned()
        .expect("missing `value` field");
    let target_fields = field.target_ty.field_types(&db);
    assert_eq!(target_fields.len(), 2);
    let left_root = target_fields[0]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left root const arg");
    let right_root = target_fields[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right root const arg");

    assert_eq!(field.slot_count, 1);
    assert_eq!(left_root, right_root);
    assert!(
        !ty_contains_const_hole(&db, field.target_ty),
        "unelaborated const hole remained in repeated target type: {:?}",
        field.target_ty
    );
}
