use camino::Utf8PathBuf;
use fe_hir::analysis::ty::{
    const_ty::{ConstTyData, EvaluatedConstTy},
    ty_check::{check_contract_recv_arm_body, check_func_body},
    ty_contains_const_hole,
    ty_def::TyData,
};
use fe_hir::core::semantic::{
    FieldStorageLayout, LayoutSelection, PlaceStep, RootRole, StoragePlace,
};
use fe_hir::hir_def::{
    CallableDef, Contract, Expr, ExprId, FieldIndex, Func, IdentId, ItemKind, Partial, Pat, PatId,
    TopLevelMod,
};
use fe_hir::test_db::HirAnalysisTestDb;

fn const_lit_usize<'db>(
    db: &'db HirAnalysisTestDb,
    ty: fe_hir::analysis::ty::ty_def::TyId<'db>,
) -> usize {
    let TyData::ConstTy(const_ty) = ty.data(db) else {
        panic!("expected const type, got {ty:?}");
    };
    let ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int), _) = const_ty.data(db) else {
        panic!(
            "expected evaluated integer const type, got {:?}",
            const_ty.data(db)
        );
    };
    int.data(db)
        .to_string()
        .parse()
        .expect("integer const should fit in usize")
}

fn find_func<'db>(db: &'db HirAnalysisTestDb, top_mod: TopLevelMod<'db>, name: &str) -> Func<'db> {
    top_mod
        .children_non_nested(db)
        .find_map(|item| match item {
            ItemKind::Func(func) if func.name(db).to_opt().is_some_and(|n| n.data(db) == name) => {
                Some(func)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing `{name}` function"))
}

fn find_method_call_expr<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>) -> ExprId {
    let body = func.body(db).expect("missing function body");
    body.exprs(db)
        .keys()
        .find(|expr| matches!(expr.data(db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing method call expression")
}

fn find_field_expr<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>, field_name: &str) -> ExprId {
    let body = func.body(db).expect("missing function body");
    body.exprs(db)
        .keys()
        .find(|expr| {
            matches!(
                expr.data(db, body),
                Partial::Present(Expr::Field(
                    _,
                    Partial::Present(FieldIndex::Ident(field))
                )) if field.data(db) == field_name
            )
        })
        .unwrap_or_else(|| panic!("missing `{field_name}` field expression"))
}

fn find_contract<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
    name: &str,
) -> Contract<'db> {
    top_mod
        .children_non_nested(db)
        .find_map(|item| match item {
            ItemKind::Contract(contract)
                if contract
                    .name(db)
                    .to_opt()
                    .is_some_and(|n| n.data(db) == name) =>
            {
                Some(contract)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing contract `{name}`"))
}

fn field_enumeration<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
    contract_name: &str,
    field_index: u32,
) -> FieldStorageLayout<'db> {
    find_contract(db, top_mod, contract_name)
        .storage_layout(db)
        .values()
        .find(|field| field.field.index == field_index)
        .cloned()
        .unwrap_or_else(|| panic!("missing field {field_index} in contract `{contract_name}`"))
}

fn allocated_fields<'db>(
    db: &'db HirAnalysisTestDb,
    contract: Contract<'db>,
) -> common::indexmap::IndexMap<IdentId<'db>, FieldStorageLayout<'db>> {
    contract
        .storage_layout(db)
        .allocated
        .as_ref()
        .expect("contract layout should be allocated")
        .fields
        .clone()
}

fn field_storage_layout<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
    contract_name: &str,
    field_name: &str,
) -> FieldStorageLayout<'db> {
    find_contract(db, top_mod, contract_name)
        .storage_layout(db)
        .get(&IdentId::new(db, field_name.to_string()))
        .cloned()
        .unwrap_or_else(|| panic!("missing field `{field_name}` in contract `{contract_name}`"))
}

fn counted_assigned_slots<'db>(field: &FieldStorageLayout<'db>) -> Vec<usize> {
    let mut slots = field
        .cells
        .iter()
        .filter(|cell| cell.role == RootRole::Counted)
        .map(|cell| cell.allocation.expect("missing counted allocation").slot)
        .collect::<Vec<_>>();
    slots.sort_unstable();
    slots
}

fn concrete_declared_ty<'db>(
    db: &'db HirAnalysisTestDb,
    field: &FieldStorageLayout<'db>,
) -> fe_hir::analysis::ty::ty_def::TyId<'db> {
    field
        .declared_concrete_ty(db, &LayoutSelection::default())
        .expect("declared view should have one scalar landing per root")
}

fn concrete_target_ty<'db>(
    db: &'db HirAnalysisTestDb,
    field: &FieldStorageLayout<'db>,
) -> fe_hir::analysis::ty::ty_def::TyId<'db> {
    field
        .target_concrete_ty(db, &LayoutSelection::default())
        .expect("target view should have one scalar landing per root")
}

fn storage_place<'db>(
    field: &FieldStorageLayout<'db>,
    steps: impl IntoIterator<Item = PlaceStep>,
) -> StoragePlace<'db> {
    steps
        .into_iter()
        .fold(StoragePlace::root(field.field), |place, step| {
            place.with_step(step)
        })
}

fn find_method_call_expr_named_in_body<'db>(
    db: &'db HirAnalysisTestDb,
    body: fe_hir::hir_def::Body<'db>,
    method_name: &str,
) -> ExprId {
    body.exprs(db)
        .keys()
        .find(|expr| {
            matches!(
                expr.data(db, body),
                Partial::Present(Expr::MethodCall(_, Partial::Present(name), _, _))
                    if name.data(db) == method_name
            )
        })
        .unwrap_or_else(|| panic!("missing method call `{method_name}`"))
}

fn find_binding_pat<'db>(
    db: &'db HirAnalysisTestDb,
    body: fe_hir::hir_def::Body<'db>,
    name: &str,
) -> PatId {
    body.pats(db)
        .keys()
        .find(|pat| {
            matches!(
                pat.data(db, body),
                Partial::Present(Pat::Path(Partial::Present(path), _))
                    if path.as_ident(db).is_some_and(|ident| ident.data(db) == name)
            )
        })
        .unwrap_or_else(|| panic!("missing binding pattern `{name}`"))
}

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
fn contract_field_mutex_try_lock_keeps_concrete_inner_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_mutex_try_lock_keeps_concrete_inner_type.fe"),
        r#"
use std::evm::{Address, Mutex, StorageMap}

msg Msg {
    #[selector = 1]
    Protected { user: Address } -> u256,
}

pub contract C {
    mut guarded_balances: Mutex<StorageMap<Address, u256>>,

    recv Msg {
        Protected { user } -> u256 uses (mut guarded_balances) {
            match guarded_balances.try_lock() {
                Option::Some(mut balances) => balances.get(key: user),
                Option::None => 0,
            }
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let field_layout = field_storage_layout(&db, top_mod, "C", "guarded_balances");
    let field_ty = concrete_target_ty(&db, &field_layout)
        .pretty_print(&db)
        .to_string();
    let recv = contract.recvs(&db).data(&db).first().expect("missing recv");
    let body = recv.arms.data(&db).first().expect("missing arm").body;
    let (diags, typed_body) = check_contract_recv_arm_body(&db, contract, 0, 0);
    let try_lock = find_method_call_expr_named_in_body(&db, body, "try_lock");
    let receiver_expr = match try_lock.data(&db, body) {
        Partial::Present(Expr::MethodCall(receiver, ..)) => *receiver,
        _ => panic!("try_lock expr is not a method call"),
    };
    let balances_pat = find_binding_pat(&db, body, "balances");
    let receiver_ty = typed_body
        .expr_ty(&db, receiver_expr)
        .pretty_print(&db)
        .to_string();
    let try_lock_ty = typed_body
        .expr_ty(&db, try_lock)
        .pretty_print(&db)
        .to_string();
    let balances_ty = typed_body
        .pat_ty(&db, balances_pat)
        .pretty_print(&db)
        .to_string();
    assert!(
        diags.is_empty(),
        "{}",
        fe_hir::analysis::diagnostics::format_diags(&db, diags.iter())
    );
    assert_eq!(field_ty, "Mutex<StorageMap<Address, u256, 0>>");
    assert_eq!(receiver_ty, "Mutex<StorageMap<Address, u256, 0>>");
    assert_eq!(try_lock_ty, "Option<mut StorageMap<Address, u256, 0>>");
    assert_eq!(balances_ty, "mut StorageMap<Address, u256, 0>");
}

#[test]
fn contract_fields_keep_required_aggregate_layout_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_fields_keep_required_aggregate_layout_args.fe"),
        r#"
use std::evm::StorageMap

struct Store {
    balances: StorageMap<u256, u256>,
    allowances: StorageMap<u256, u256>,
}

pub contract C {
    mut store: Store,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let field_name = IdentId::new(&db, "store".to_string());
    let field_layout = contract
        .storage_layout(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `store` field layout");
    let field_info = contract
        .fields(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `store` field info");

    assert_eq!(
        concrete_target_ty(&db, &field_layout)
            .pretty_print(&db)
            .to_string(),
        "Store"
    );
    assert_eq!(field_layout.target, field_info.target);
    assert_eq!(counted_assigned_slots(&field_layout), [0, 1]);
}

#[test]
fn contract_fields_identically_typed_storage_maps_get_distinct_roots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_fields_identically_typed_storage_maps_get_distinct_roots.fe"),
        r#"
use std::evm::{Address, StorageMap}

msg Msg {
    #[selector = 1]
    Sum { user: Address } -> u256,
}

pub contract C {
    mut a: StorageMap<Address, u256>,
    mut b: StorageMap<Address, u256>,

    recv Msg {
        Sum { user } -> u256 uses (mut a, mut b) {
            a.get(key: user) + b.get(key: user)
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let field_ty = |name: &str| {
        let field = layout
            .get(&IdentId::new(&db, name.to_string()))
            .unwrap_or_else(|| panic!("missing `{name}` field"));
        concrete_target_ty(&db, field).pretty_print(&db).to_string()
    };

    assert_eq!(field_ty("a"), "StorageMap<Address, u256, 0>");
    assert_eq!(field_ty("b"), "StorageMap<Address, u256, 1>");

    let (diags, _) = check_contract_recv_arm_body(&db, contract, 0, 0);
    assert!(
        diags.is_empty(),
        "{}",
        fe_hir::analysis::diagnostics::format_diags(&db, diags.iter())
    );
}

#[test]
fn contract_field_wrapper_storage_map_declared_and_target_share_root() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_wrapper_storage_map_declared_and_target_share_root.fe"),
        r#"
use std::evm::{Address, StorageMap}

struct Wrapper<T> {
    inner: T,
}

pub contract C {
    mut wrapped: Wrapper<StorageMap<Address, u256>>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let wrapped = field_storage_layout(&db, top_mod, "C", "wrapped");
    assert_eq!(
        concrete_declared_ty(&db, &wrapped)
            .pretty_print(&db)
            .to_string(),
        "Wrapper<StorageMap<Address, u256, 0>>"
    );
    assert_eq!(wrapped.declared, wrapped.target);
    assert_eq!(wrapped.slot_count, 1);
}

#[test]
fn contract_fields_preserve_nested_assigned_layout_views() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_fields_preserve_nested_assigned_layout_views.fe"),
        r#"
use std::evm::{Address, Mutex, StorageMap}

struct Wrapper<T> {
    inner: T,
}

msg Msg {
    #[selector = 1]
    Protected { user: Address } -> u256,
}

pub contract C {
    mut wrapped: Wrapper<Mutex<StorageMap<Address, u256>>>,

    recv Msg {
        Protected { user } -> u256 uses (mut wrapped) {
            match wrapped.inner.try_lock() {
                Option::Some(mut balances) => balances.get(key: user),
                Option::None => 0,
            }
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let field_name = IdentId::new(&db, "wrapped".to_string());
    let field_layout = contract
        .storage_layout(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `wrapped` field layout");
    let field_info = contract
        .fields(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `wrapped` field info");
    assert_eq!(
        concrete_target_ty(&db, &field_layout)
            .pretty_print(&db)
            .to_string(),
        "Wrapper<Mutex<StorageMap<Address, u256, 0>>>"
    );
    assert_eq!(field_layout.target, field_info.target);

    let recv = contract.recvs(&db).data(&db).first().expect("missing recv");
    let body = recv.arms.data(&db).first().expect("missing arm").body;
    let (diags, typed_body) = check_contract_recv_arm_body(&db, contract, 0, 0);
    assert!(
        diags.is_empty(),
        "{}",
        fe_hir::analysis::diagnostics::format_diags(&db, diags.iter())
    );

    let try_lock = find_method_call_expr_named_in_body(&db, body, "try_lock");
    let receiver_expr = match try_lock.data(&db, body) {
        Partial::Present(Expr::MethodCall(receiver, ..)) => *receiver,
        _ => panic!("try_lock expr is not a method call"),
    };
    let balances_pat = find_binding_pat(&db, body, "balances");
    assert_eq!(
        typed_body
            .expr_ty(&db, receiver_expr)
            .pretty_print(&db)
            .to_string(),
        "Mutex<StorageMap<Address, u256, 0>>"
    );
    assert_eq!(
        typed_body
            .expr_ty(&db, try_lock)
            .pretty_print(&db)
            .to_string(),
        "Option<mut StorageMap<Address, u256, 0>>"
    );
    assert_eq!(
        typed_body
            .pat_ty(&db, balances_pat)
            .pretty_print(&db)
            .to_string(),
        "mut StorageMap<Address, u256, 0>"
    );
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
        .effect_requirements(&db)
        .first()
        .expect("missing effect binding");
    let key_trait = effect_binding
        .key
        .key_trait()
        .expect("missing trait effect key");
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
fn trait_effect_keys_keep_distinct_omitted_hole_defaults() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_effect_keys_keep_distinct_omitted_hole_defaults.fe"),
        r#"
trait Cap<const LEFT: u256 = _, const RIGHT: u256 = _> {}

fn f() uses (cap: Cap) {}
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
    assert_eq!(implicit_layout_params, 2);

    let key_trait = func
        .effect_requirements(&db)
        .first()
        .expect("missing effect binding")
        .key
        .key_trait()
        .expect("missing trait effect key");
    let args = key_trait.args(&db);
    assert_eq!(args.len(), 3);
    assert_ne!(args[1], args[2]);
    assert!(
        args.iter()
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
        .effect_requirements(&db)
        .first()
        .expect("missing effect binding");
    let key_ty = effect_binding
        .key
        .key_ty()
        .expect("missing type effect key");
    assert!(
        !ty_contains_const_hole(&db, key_ty),
        "unelaborated const hole remained in type effect key: {key_ty:?}"
    );
}

#[test]
fn callable_value_params_keep_distinct_explicit_hole_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("callable_value_params_keep_distinct_explicit_hole_args.fe"),
        r#"
struct Pair<const LEFT: u256, const RIGHT: u256> {}

fn f(x: Pair<_, _>) {}
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
    assert_eq!(implicit_layout_params, 2);

    let arg_ty = func.arg_tys(&db)[0].instantiate_identity();
    let arg_ty = arg_ty.as_view(&db).unwrap_or(arg_ty);
    let args = arg_ty.generic_args(&db);
    assert_eq!(args.len(), 2);
    assert_ne!(args[0], args[1]);
    assert!(
        !ty_contains_const_hole(&db, arg_ty),
        "unelaborated const hole remained in callable parameter type: {arg_ty:?}"
    );
}

#[test]
fn callable_value_params_accept_explicit_hole_args_through_type_aliases() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "callable_value_params_accept_explicit_hole_args_through_type_aliases.fe",
        ),
        r#"
struct Pair<const LEFT: u256, const RIGHT: u256> {}
type PairAlias<const LEFT: u256, const RIGHT: u256> = Pair<LEFT, RIGHT>

fn f(x: PairAlias<_, _>) {}
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
    assert_eq!(implicit_layout_params, 2);

    let arg_ty = func.arg_tys(&db)[0].instantiate_identity();
    let arg_ty = arg_ty.as_view(&db).unwrap_or(arg_ty);
    let args = arg_ty.generic_args(&db);
    assert_eq!(args.len(), 2);
    assert_ne!(args[0], args[1]);
    assert!(
        !ty_contains_const_hole(&db, arg_ty),
        "unelaborated const hole remained in callable parameter type: {arg_ty:?}"
    );
}

#[test]
fn method_call_generic_holes_keep_distinct_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("method_call_generic_holes_keep_distinct_identity.fe"),
        r#"
struct Pair<const LEFT: usize, const RIGHT: usize> {}

struct Builder {}

impl Builder {
    fn pair<const LEFT: usize, const RIGHT: usize>(
        self,
        _: [u8; LEFT],
        _: [u8; RIGHT],
    ) -> Pair<LEFT, RIGHT> {
        Pair {}
    }
}

fn f(b: Builder) {
    let out = b.pair<_, _>([1], [1, 2])
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let method_call = find_method_call_expr(&db, func);
    let callable = typed_body
        .callable_expr(method_call)
        .expect("missing callable for method call");
    let ret_ty = typed_body.expr_ty(&db, method_call);
    let args = &callable.generic_args()[callable
        .callable_def
        .offset_to_explicit_params_position(&db)..];
    let ret_args = ret_ty.generic_args(&db);

    assert_eq!(args.len(), 2);
    assert_eq!(ret_args.len(), 2);
    assert_ne!(args[0], args[1]);
    assert_ne!(ret_args[0], ret_args[1]);
}

#[test]
fn method_call_generic_type_args_keep_distinct_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("method_call_generic_type_args_keep_distinct_identity.fe"),
        r#"
struct Slot<const ROOT: usize = _> {}
struct Pair<A, B> {}

struct Builder {}

impl Builder {
    fn pair<A, B>(self) -> Pair<A, B> {
        Pair {}
    }
}

fn f(b: Builder) {
    let out = b.pair<Slot, Slot>()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let method_call = find_method_call_expr(&db, func);
    let callable = typed_body
        .callable_expr(method_call)
        .expect("missing callable for method call");
    let ret_ty = typed_body.expr_ty(&db, method_call);
    let args = &callable.generic_args()[callable
        .callable_def
        .offset_to_explicit_params_position(&db)..];
    let ret_args = ret_ty.generic_args(&db);

    assert_eq!(args.len(), 2);
    assert_eq!(ret_args.len(), 2);
    assert_ne!(args[0], args[1]);
    assert_ne!(ret_args[0], ret_args[1]);

    let first_arg_root = args[0]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing first generic-arg root const");
    let second_arg_root = args[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing second generic-arg root const");
    let first_ret_root = ret_args[0]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing first return root const");
    let second_ret_root = ret_args[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing second return root const");

    assert_ne!(first_arg_root, second_arg_root);
    assert_ne!(first_ret_root, second_ret_root);
}

#[test]
fn repeated_call_generic_type_args_keep_distinct_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("repeated_call_generic_type_args_keep_distinct_identity.fe"),
        r#"
struct Slot<const ROOT: u256 = _> {}

fn consume<T>() {}

fn f() {
    consume<Slot>()
    consume<Slot>()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let body = func.body(&db).expect("missing function body");
    let calls = body
        .exprs(&db)
        .keys()
        .filter(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::Call(..))))
        .collect::<Vec<_>>();
    assert_eq!(calls.len(), 2);

    let root = |call| {
        let callable = typed_body
            .callable_expr(call)
            .expect("missing callable for direct call");
        callable.generic_args()[0]
            .generic_args(&db)
            .first()
            .copied()
            .expect("missing layout-root const")
    };

    assert_ne!(root(calls[0]), root(calls[1]));
}

#[test]
fn repeated_method_generic_type_args_keep_distinct_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("repeated_method_generic_type_args_keep_distinct_identity.fe"),
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Builder {}

impl Builder {
    fn consume<T>(self) {}
}

fn f(b: Builder) {
    b.consume<Slot>()
    b.consume<Slot>()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let body = func.body(&db).expect("missing function body");
    let calls = body
        .exprs(&db)
        .keys()
        .filter(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .collect::<Vec<_>>();
    assert_eq!(calls.len(), 2);

    let root = |call| {
        let callable = typed_body
            .callable_expr(call)
            .expect("missing callable for method call");
        let offset = callable
            .callable_def
            .offset_to_explicit_params_position(&db);
        callable.generic_args()[offset]
            .generic_args(&db)
            .first()
            .copied()
            .expect("missing layout-root const")
    };

    assert_ne!(root(calls[0]), root(calls[1]));
}

#[test]
fn repeated_deferred_method_generic_type_args_keep_distinct_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("repeated_deferred_method_generic_type_args_keep_distinct_identity.fe"),
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Builder {}

trait WithU8 {
    fn consume<T>(self, tag: u8)
}

trait WithBool {
    fn consume<T>(self, tag: bool)
}

impl WithU8 for Builder {
    fn consume<T>(self, tag: u8) {}
}

impl WithBool for Builder {
    fn consume<T>(self, tag: bool) {}
}

fn f(b: Builder, tag: u8) {
    b.consume<Slot>(tag)
    b.consume<Slot>(tag)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let body = func.body(&db).expect("missing function body");
    let calls = body
        .exprs(&db)
        .keys()
        .filter(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .collect::<Vec<_>>();
    assert_eq!(calls.len(), 2);

    let root = |call| {
        let callable = typed_body
            .callable_expr(call)
            .expect("missing callable for deferred method call");
        let offset = callable
            .callable_def
            .offset_to_explicit_params_position(&db);
        callable.generic_args()[offset]
            .generic_args(&db)
            .first()
            .copied()
            .expect("missing layout-root const")
    };

    assert_ne!(root(calls[0]), root(calls[1]));
}

#[test]
fn repeated_record_layout_holes_keep_distinct_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("repeated_record_layout_holes_keep_distinct_identity.fe"),
        r#"
struct Slot<const ROOT: u256 = _> {}

fn f() {
    let first = Slot {}
    let second = Slot {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let body = func.body(&db).expect("missing function body");
    let records = body
        .exprs(&db)
        .keys()
        .filter(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::RecordInit(..))))
        .collect::<Vec<_>>();
    assert_eq!(records.len(), 2);

    let root = |expr| typed_body.expr_ty(&db, expr).generic_args(&db)[0];
    assert_ne!(root(records[0]), root(records[1]));
}

#[test]
fn repeated_value_paths_with_layout_holes_keep_distinct_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("repeated_value_paths_with_layout_holes_keep_distinct_identity.fe"),
        r#"
enum Choice<const ROOT: u256 = _> {
    A,
}

fn f() {
    let first = Choice::A
    let second = Choice::A
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let body = func.body(&db).expect("missing function body");
    let paths = body
        .exprs(&db)
        .keys()
        .filter(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::Path(..))))
        .collect::<Vec<_>>();
    assert_eq!(paths.len(), 2);

    let root = |expr| typed_body.expr_ty(&db, expr).generic_args(&db)[0];
    assert_ne!(root(paths[0]), root(paths[1]));
}

#[test]
fn deferred_method_call_generic_holes_keep_distinct_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("deferred_method_call_generic_holes_keep_distinct_identity.fe"),
        r#"
struct Pair<const LEFT: usize, const RIGHT: usize> {}

struct Builder {}

trait WithU8 {
    fn pair<const LEFT: usize, const RIGHT: usize>(self, tag: u8) -> Pair<LEFT, RIGHT>
}

trait WithBool {
    fn pair<const LEFT: usize, const RIGHT: usize>(self, tag: bool) -> Pair<LEFT, RIGHT>
}

impl WithU8 for Builder {
    fn pair<const LEFT: usize, const RIGHT: usize>(self, tag: u8) -> Pair<LEFT, RIGHT> {
        Pair {}
    }
}

impl WithBool for Builder {
    fn pair<const LEFT: usize, const RIGHT: usize>(self, tag: bool) -> Pair<LEFT, RIGHT> {
        Pair {}
    }
}

fn f(b: Builder, tag: u8) {
    let out = b.pair<_, _>(tag)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let method_call = find_method_call_expr(&db, func);
    let callable = typed_body
        .callable_expr(method_call)
        .expect("missing callable for deferred method call");
    let ret_ty = typed_body.expr_ty(&db, method_call);
    let args = &callable.generic_args()[callable
        .callable_def
        .offset_to_explicit_params_position(&db)..];
    let ret_args = ret_ty.generic_args(&db);

    assert_eq!(args.len(), 2);
    assert_eq!(ret_args.len(), 2);
    assert_ne!(args[0], args[1]);
    assert_ne!(ret_args[0], ret_args[1]);
}

#[test]
fn callable_effect_keys_keep_distinct_explicit_hole_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("callable_effect_keys_keep_distinct_explicit_hole_args.fe"),
        r#"
struct Pair<const LEFT: u256, const RIGHT: u256> {}

fn f() uses (slot: Pair<_, _>) {}
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
    assert_eq!(implicit_layout_params, 2);

    let key_ty = func
        .effect_requirements(&db)
        .first()
        .expect("missing effect binding")
        .key
        .key_ty()
        .expect("missing type effect key");
    let args = key_ty.generic_args(&db);
    assert_eq!(args.len(), 2);
    assert_ne!(args[0], args[1]);
    assert!(
        !ty_contains_const_hole(&db, key_ty),
        "unelaborated const hole remained in callable effect key: {key_ty:?}"
    );
}

#[test]
fn callable_value_params_keep_distinct_omitted_default_path_occurrences() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "callable_value_params_keep_distinct_omitted_default_path_occurrences.fe",
        ),
        r#"
struct Slot<const ROOT: u256 = _> {}

fn f(x: (Slot, Slot)) {}
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
    assert_eq!(implicit_layout_params, 2);

    let arg_ty = func.arg_tys(&db)[0].instantiate_identity();
    let arg_ty = arg_ty.as_view(&db).unwrap_or(arg_ty);
    let fields = arg_ty.field_types(&db);
    assert_eq!(fields.len(), 2);
    let left_root = fields[0]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left root const arg");
    let right_root = fields[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right root const arg");

    assert_ne!(left_root, right_root);
    assert!(
        !ty_contains_const_hole(&db, arg_ty),
        "unelaborated const hole remained in callable parameter type: {arg_ty:?}"
    );
}

#[test]
fn callable_value_params_keep_distinct_repeated_type_args_in_generic_arg_lists() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "callable_value_params_keep_distinct_repeated_type_args_in_generic_arg_lists.fe",
        ),
        r#"
struct Slot<const ROOT: u256 = _> {}

struct Pair<A, B> {
    left: A,
    right: B,
}

fn f(x: Pair<Slot, Slot>) {
    let left = x.left
    let right = x.right
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let arg_ty = func.arg_tys(&db)[0].instantiate_identity();
    let arg_ty = arg_ty.as_view(&db).unwrap_or(arg_ty);
    let typed_body = check_func_body(&db, func).1.clone();
    let left_ty = typed_body.expr_ty(&db, find_field_expr(&db, func, "left"));
    let right_ty = typed_body.expr_ty(&db, find_field_expr(&db, func, "right"));
    let left_root = left_ty
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left root const arg");
    let right_root = right_ty
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right root const arg");

    assert_ne!(left_root, right_root);
    assert!(
        !ty_contains_const_hole(&db, arg_ty),
        "unelaborated const hole remained in callable parameter type: {arg_ty:?}"
    );
    assert!(
        !ty_contains_const_hole(&db, left_ty),
        "unelaborated const hole remained in left field projection type: {left_ty:?}"
    );
    assert!(
        !ty_contains_const_hole(&db, right_ty),
        "unelaborated const hole remained in right field projection type: {right_ty:?}"
    );
}

#[test]
fn callable_value_params_keep_distinct_omitted_type_default_applications() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "callable_value_params_keep_distinct_omitted_type_default_applications.fe",
        ),
        r#"
struct Slot<const ROOT: u256 = _> {}

struct Wrap<T = Slot> {
    value: T,
}

fn f(x: (Wrap, Wrap)) {
    let left = x.0.value
    let right = x.1.value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let body = func.body(&db).expect("missing body");
    let arg_ty = func.arg_tys(&db)[0].instantiate_identity();
    let arg_ty = arg_ty.as_view(&db).unwrap_or(arg_ty);
    let typed_body = check_func_body(&db, func).1.clone();
    let left_ty = typed_body.pat_ty(&db, find_binding_pat(&db, body, "left"));
    let right_ty = typed_body.pat_ty(&db, find_binding_pat(&db, body, "right"));
    let left_root = left_ty
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left root const arg");
    let right_root = right_ty
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right root const arg");

    assert_ne!(left_root, right_root);
    assert!(
        !ty_contains_const_hole(&db, arg_ty),
        "unelaborated const hole remained in callable parameter type: {arg_ty:?}"
    );
    assert!(
        !ty_contains_const_hole(&db, left_ty),
        "unelaborated const hole remained in left binding type: {left_ty:?}"
    );
    assert!(
        !ty_contains_const_hole(&db, right_ty),
        "unelaborated const hole remained in right binding type: {right_ty:?}"
    );
}

#[test]
fn callable_effect_keys_keep_distinct_omitted_default_alias_occurrences() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "callable_effect_keys_keep_distinct_omitted_default_alias_occurrences.fe",
        ),
        r#"
struct Slot<const ROOT: u256 = _> {}
type TwoSlots = (Slot, Slot)

fn f() uses (slots: TwoSlots) {}
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
    assert_eq!(implicit_layout_params, 2);

    let key_ty = func
        .effect_requirements(&db)
        .first()
        .expect("missing effect binding")
        .key
        .key_ty()
        .expect("missing type effect key");
    let fields = key_ty.field_types(&db);
    assert_eq!(fields.len(), 2);
    let left_root = fields[0]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left root const arg");
    let right_root = fields[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right root const arg");

    assert_ne!(left_root, right_root);
    assert!(
        !ty_contains_const_hole(&db, key_ty),
        "unelaborated const hole remained in callable effect key: {key_ty:?}"
    );
}

#[test]
fn trait_effect_keys_keep_distinct_omitted_type_default_applications() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_effect_keys_keep_distinct_omitted_type_default_applications.fe"),
        r#"
trait Cap<A, B> {}

struct Slot<const ROOT: u256 = _> {}
struct Wrap<T = Slot> {}

fn f() uses (cap: Cap<Wrap, Wrap>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
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
    assert_eq!(implicit_layout_params, 2);

    let key_trait = func
        .effect_requirements(&db)
        .first()
        .expect("missing effect binding")
        .key
        .key_trait()
        .expect("missing trait effect key");
    let args = key_trait.args(&db);
    assert_eq!(args.len(), 3);
    assert_ne!(args[1], args[2]);

    let left_root = args[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left wrap type arg")
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left root const arg");
    let right_root = args[2]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right wrap type arg")
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right root const arg");

    assert_ne!(left_root, right_root);
    assert!(
        args.iter()
            .copied()
            .all(|arg| !ty_contains_const_hole(&db, arg)),
        "unelaborated const hole remained in trait effect key: {key_trait:?}"
    );
}

#[test]
fn trait_effect_keys_keep_distinct_repeated_type_args_in_generic_arg_lists() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "trait_effect_keys_keep_distinct_repeated_type_args_in_generic_arg_lists.fe",
        ),
        r#"
trait Cap<A, B> {}

struct Slot<const ROOT: u256 = _> {}

fn f() uses (cap: Cap<Slot, Slot>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
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
    assert_eq!(implicit_layout_params, 2);

    let key_trait = func
        .effect_requirements(&db)
        .first()
        .expect("missing effect binding")
        .key
        .key_trait()
        .expect("missing trait effect key");
    let args = key_trait.args(&db);
    assert_eq!(args.len(), 3);
    assert_ne!(args[1], args[2]);

    let left_root = args[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left root const arg");
    let right_root = args[2]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right root const arg");

    assert_ne!(left_root, right_root);
    assert!(
        args.iter()
            .copied()
            .all(|arg| !ty_contains_const_hole(&db, arg)),
        "unelaborated const hole remained in trait effect key: {key_trait:?}"
    );
}

#[test]
fn derived_adt_layout_suffix_is_rejected_as_excess_generic_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "adt_fields_consume_layout_args_from_instantiated_explicit_field_types.fe",
        ),
        r#"
struct Slot<T, const ROOT: u256 = _> {}

struct Outer<U> {
    a: U,
    b: Slot<u256>,
}

fn takes_root_2(_: Slot<u256, 2>) {}
fn takes_root_3(_: Slot<u256, 3>) {}

fn f(x: Outer<Slot<u256>, 2, 3>) {
    takes_root_2(x.a)
    takes_root_3(x.b)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    let rendered = fe_hir::test_db::format_diagnostics(&db, &diags);
    assert!(rendered.contains("incorrect number of generic arguments for `Outer`"));
    assert!(rendered.contains("expected 1, given 3"));
}

#[test]
fn callable_value_params_collect_instantiated_adt_field_holes_for_omitted_layout_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "callable_value_params_collect_instantiated_adt_field_holes_for_omitted_layout_args.fe",
        ),
        r#"
struct Slot<T, const ROOT: u256 = _> {}

struct Outer<U> {
    a: U,
    b: Slot<u256>,
}

fn f(x: Outer<Slot<u256>>) {
    let a = x.a
    let b = x.b
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let field_a_ty = typed_body.expr_ty(&db, find_field_expr(&db, func, "a"));
    let field_b_ty = typed_body.expr_ty(&db, find_field_expr(&db, func, "b"));
    let first_root = field_a_ty
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing first field root const arg");
    let second_root = field_b_ty
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing second field root const arg");

    assert_ne!(first_root, second_root);
    assert!(
        matches!(
            first_root.data(&db),
            TyData::ConstTy(const_ty)
                if matches!(const_ty.data(&db), ConstTyData::TyParam(param, _) if param.is_implicit())
        ),
        "first projected field did not receive an implicit fallback layout arg: {first_root:?}"
    );
    assert!(
        matches!(
            second_root.data(&db),
            TyData::ConstTy(const_ty)
                if matches!(const_ty.data(&db), ConstTyData::TyParam(param, _) if param.is_implicit())
        ),
        "second projected field did not receive an implicit fallback layout arg: {second_root:?}"
    );
    assert!(
        !ty_contains_const_hole(&db, field_a_ty),
        "unelaborated const hole remained in first field projection type: {field_a_ty:?}"
    );
    assert!(
        !ty_contains_const_hole(&db, field_b_ty),
        "unelaborated const hole remained in second field projection type: {field_b_ty:?}"
    );
}

#[test]
fn callable_projection_uses_the_same_type_parameter_landing_rules_as_storage() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "callable_projection_uses_the_same_type_parameter_landing_rules_as_storage.fe",
        ),
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Pair<T> { left: T, right: T }

fn f(x: Pair<Slot>) {
    let left = x.left
    let right = x.right
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let left = typed_body.expr_ty(&db, find_field_expr(&db, func, "left"));
    let right = typed_body.expr_ty(&db, find_field_expr(&db, func, "right"));
    assert_ne!(
        left.generic_args(&db).first(),
        right.generic_args(&db).first(),
        "each written occurrence of a type parameter must receive its own landing root",
    );
}

#[test]
fn callable_value_params_reuse_repeated_placeholder_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("callable_value_params_reuse_repeated_placeholder_identity.fe"),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

fn f(x: Repeated) {}
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

    let arg_ty = func.arg_tys(&db)[0].instantiate_identity();
    let arg_ty = arg_ty.as_view(&db).unwrap_or(arg_ty);
    let fields = arg_ty.field_types(&db);
    assert_eq!(fields.len(), 2);
    let left_root = fields[0]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left root const arg");
    let right_root = fields[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right root const arg");

    assert_eq!(left_root, right_root);
    assert!(
        !ty_contains_const_hole(&db, arg_ty),
        "unelaborated const hole remained in callable parameter type: {arg_ty:?}"
    );
}

#[test]
fn callable_effect_keys_reuse_repeated_placeholder_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("callable_effect_keys_reuse_repeated_placeholder_identity.fe"),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

fn f() uses (slot: Repeated) {}
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

    let key_ty = func
        .effect_requirements(&db)
        .first()
        .expect("missing effect binding")
        .key
        .key_ty()
        .expect("missing type effect key");
    let fields = key_ty.field_types(&db);
    assert_eq!(fields.len(), 2);
    let left_root = fields[0]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left root const arg");
    let right_root = fields[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right root const arg");

    assert_eq!(left_root, right_root);
    assert!(
        !ty_contains_const_hole(&db, key_ty),
        "unelaborated const hole remained in callable effect key: {key_ty:?}"
    );
}

#[test]
fn callable_value_params_keep_distinct_placeholder_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("callable_value_params_keep_distinct_placeholder_identity.fe"),
        r#"
struct Leaf<const ROOT: u256> {}
type Distinct<const LEFT: u256 = _, const RIGHT: u256 = _> = (Leaf<LEFT>, Leaf<RIGHT>)

fn f(x: Distinct) {}
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
    assert_eq!(implicit_layout_params, 2);

    let arg_ty = func.arg_tys(&db)[0].instantiate_identity();
    let arg_ty = arg_ty.as_view(&db).unwrap_or(arg_ty);
    let fields = arg_ty.field_types(&db);
    assert_eq!(fields.len(), 2);
    let left_root = fields[0]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing left root const arg");
    let right_root = fields[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing right root const arg");

    assert_ne!(left_root, right_root);
    assert!(
        !ty_contains_const_hole(&db, arg_ty),
        "unelaborated const hole remained in callable parameter type: {arg_ty:?}"
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
        .storage_layout(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `value` field layout");
    let field_info = contract
        .fields(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `value` field info");
    assert!(field_layout.is_provider);
    assert_eq!(
        field_layout.address_space,
        fe_hir::analysis::ty::ProviderAddressSpace::Storage
    );
    assert_eq!(field_layout.declared, field_info.declared);
    assert_eq!(field_layout.target, field_info.target);
    assert_eq!(field_layout.is_provider, field_info.is_provider);
    assert!(
        !ty_contains_const_hole(&db, concrete_declared_ty(&db, &field_layout)),
        "unelaborated const hole remained in contract field type: {:?}",
        field_layout.declared.template
    );
    assert!(
        !ty_contains_const_hole(&db, concrete_target_ty(&db, &field_layout)),
        "unelaborated const hole remained in contract field target type: {:?}",
        field_layout.target.template
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

    let layout = allocated_fields(&db, contract);
    let storage0 = layout
        .get(&IdentId::new(&db, "storage0".to_string()))
        .expect("missing `storage0` field");
    let memory0 = layout
        .get(&IdentId::new(&db, "memory0".to_string()))
        .expect("missing `memory0` field");
    let storage1 = layout
        .get(&IdentId::new(&db, "storage1".to_string()))
        .expect("missing `storage1` field");

    use fe_hir::analysis::ty::ProviderAddressSpace;
    assert_eq!(storage0.address_space, ProviderAddressSpace::Storage);
    assert_eq!(memory0.address_space, ProviderAddressSpace::Memory);
    assert_eq!(storage1.address_space, ProviderAddressSpace::Storage);
    assert_eq!(storage0.slot_offset, 0);
    assert_eq!(memory0.slot_offset, 0);
    assert_eq!(storage1.slot_offset, 1);
    assert_eq!(storage0.slot_count, 1);
    assert_eq!(memory0.slot_count, 1);
    assert_eq!(storage1.slot_count, 1);
}

#[test]
fn contract_field_layout_shares_alias_formal_repeated_placeholder_identity() {
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

    let field = allocated_fields(&db, contract)
        .get(&IdentId::new(&db, "value".to_string()))
        .cloned()
        .expect("missing `value` field");

    assert_eq!(field.cells.len(), 1);
    for elem in [0, 1] {
        assert_eq!(
            field.assigned_slot_for_place(&storage_place(
                &field,
                [PlaceStep::ProviderTarget, PlaceStep::TupleElem(elem)],
            )),
            Some(0)
        );
    }
    assert!(field.declared.all_roots_classified(&db));
    assert!(field.target.all_roots_classified(&db));
    assert!(field.slot_basis.all_roots_classified(&db));
    assert_eq!(field.slot_count, 1);
}

/// Sibling occurrences of the same hole-bearing type share content-interned
/// HIR ids; their holes must still be distinct or storage slots alias.
#[test]
fn contract_field_sibling_identical_hole_types_get_distinct_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_sibling_identical_hole_types_get_distinct_slots.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

struct Pair {
    left: Slot<u256>,
    right: Slot<u256>,
}

contract C {
    pair: StorPtr<Pair>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let pair = layout
        .get(&IdentId::new(&db, "pair".to_string()))
        .expect("missing `pair` field");

    assert_eq!(pair.cells.len(), 2);
    assert_eq!(
        pair.assigned_slot_for_place(&storage_place(
            pair,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(0)],
        )),
        Some(0)
    );
    assert_eq!(
        pair.assigned_slot_for_place(&storage_place(
            pair,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(1)],
        )),
        Some(1)
    );
    assert!(pair.target.all_roots_classified(&db));
    assert_eq!(pair.slot_count, 2);
}

/// A hole-bearing type passed as one generic argument and reused by several
/// struct fields (`struct Pair<T> { left: T, right: T }` as `Pair<Slot<u256>>`)
/// must give each field a distinct storage slot. The explicit-arg hole is a
/// per-field template, not a single slot; reusing it as one trailing arg would
/// alias `left` and `right` onto the same slot.
#[test]
fn contract_field_repeated_generic_arg_hole_type_gets_distinct_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_repeated_generic_arg_hole_type_gets_distinct_slots.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

contract C {
    pair: StorPtr<Pair<Slot<u256>>>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let pair = layout
        .get(&IdentId::new(&db, "pair".to_string()))
        .expect("missing `pair` field");

    assert_eq!(pair.cells.len(), 2);
    for (field_idx, slot) in [(0, 0), (1, 1)] {
        assert_eq!(
            pair.assigned_slot_for_place(&storage_place(
                pair,
                [PlaceStep::ProviderTarget, PlaceStep::StructField(field_idx),],
            )),
            Some(slot)
        );
    }
    assert_ne!(pair.cells[0].root, pair.cells[1].root);
    assert!(pair.target.all_roots_classified(&db));
    assert_eq!(pair.slot_count, 2);
}

/// Three reused occurrences of a generic-argument hole must get three slots.
#[test]
fn contract_field_triple_generic_arg_hole_type_gets_distinct_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_triple_generic_arg_hole_type_gets_distinct_slots.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

struct Trio<T> {
    a: T,
    b: T,
    c: T,
}

contract C {
    trio: StorPtr<Trio<Slot<u256>>>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let trio = layout
        .get(&IdentId::new(&db, "trio".to_string()))
        .expect("missing `trio` field");

    assert_eq!(trio.cells.len(), 3);
    for (field_idx, slot) in [(0, 0), (1, 1), (2, 2)] {
        assert_eq!(
            trio.assigned_slot_for_place(&storage_place(
                trio,
                [PlaceStep::ProviderTarget, PlaceStep::StructField(field_idx),],
            )),
            Some(slot)
        );
    }
    assert_ne!(trio.cells[0].root, trio.cells[1].root);
    assert_ne!(trio.cells[0].root, trio.cells[2].root);
    assert_ne!(trio.cells[1].root, trio.cells[2].root);
    assert_eq!(trio.slot_count, 3);
}

/// A generic arg reused inside a single tuple-typed field (`f: (T, T)`) places
/// two storage slots in one field through a shared explicit-arg hole. Tuple
/// siblings are laid out at distinct slots, so the hole must split per element.
#[test]
fn contract_field_tuple_repeated_generic_arg_hole_gets_distinct_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_tuple_repeated_generic_arg_hole_gets_distinct_slots.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

struct S<T> {
    f: (T, T),
}

contract C {
    s: StorPtr<S<Slot<u256>>>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let s = layout
        .get(&IdentId::new(&db, "s".to_string()))
        .expect("missing `s` field");
    assert_eq!(s.cells.len(), 2);
    for (elem, slot) in [(0, 0), (1, 1)] {
        assert_eq!(
            s.assigned_slot_for_place(&storage_place(
                s,
                [
                    PlaceStep::ProviderTarget,
                    PlaceStep::StructField(0),
                    PlaceStep::TupleElem(elem),
                ],
            )),
            Some(slot)
        );
    }
    assert_ne!(s.cells[0].root, s.cells[1].root);
    assert_eq!(s.slot_count, 2);
}

#[test]
fn contract_field_nested_pair_generic_arg_hole_gets_distinct_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_nested_pair_generic_arg_hole_gets_distinct_slots.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

struct S<T> {
    p: Pair<T>,
}

contract C {
    s: StorPtr<S<Slot<u256>>>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let s = field_storage_layout(&db, top_mod, "C", "s");

    assert_eq!(
        s.assigned_slot_for_place(&storage_place(
            &s,
            [
                PlaceStep::ProviderTarget,
                PlaceStep::StructField(0),
                PlaceStep::StructField(0),
            ],
        )),
        Some(0)
    );
    assert_eq!(
        s.assigned_slot_for_place(&storage_place(
            &s,
            [
                PlaceStep::ProviderTarget,
                PlaceStep::StructField(0),
                PlaceStep::StructField(1),
            ],
        )),
        Some(1)
    );
    assert_eq!(s.slot_count, 2);
}

#[test]
fn contract_field_twice_alias_generic_arg_hole_gets_distinct_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_twice_alias_generic_arg_hole_gets_distinct_slots.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

type Twice<T> = (T, T)

contract C {
    value: StorPtr<Twice<Slot<u256>>>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let value = field_storage_layout(&db, top_mod, "C", "value");

    assert_eq!(
        value.assigned_slot_for_place(&storage_place(
            &value,
            [PlaceStep::ProviderTarget, PlaceStep::TupleElem(0)],
        )),
        Some(0)
    );
    assert_eq!(
        value.assigned_slot_for_place(&storage_place(
            &value,
            [PlaceStep::ProviderTarget, PlaceStep::TupleElem(1)],
        )),
        Some(1)
    );
    assert_eq!(value.slot_count, 2);
}

#[test]
fn contract_field_array_repeated_element_hole_uses_an_indexed_family() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_array_repeated_element_hole_is_rejected.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

struct S<T> {
    f: [T; 2],
}

contract C {
    s: StorPtr<S<Slot<u256>>>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let s = layout
        .get(&IdentId::new(&db, "s".to_string()))
        .expect("missing `s` field");

    assert_eq!(s.families.len(), 1);
    assert_eq!(s.families[0].extent, 2);
    assert_eq!(s.slot_count, 2);
    assert!(ty_contains_const_hole(&db, s.target.template));
    assert!(s.target.all_roots_classified(&db));
}

/// Repeated uses of one alias expand the same template; the template's holes
/// must split per use site.
#[test]
fn contract_field_repeated_alias_occurrences_get_distinct_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_repeated_alias_occurrences_get_distinct_slots.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

type M = Slot<u256>

struct Pair {
    left: M,
    right: M,
}

contract C {
    pair: StorPtr<Pair>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let pair = layout
        .get(&IdentId::new(&db, "pair".to_string()))
        .expect("missing `pair` field");

    assert_eq!(pair.cells.len(), 2);
    for (field_idx, slot) in [(0, 0), (1, 1)] {
        assert_eq!(
            pair.assigned_slot_for_place(&storage_place(
                pair,
                [PlaceStep::ProviderTarget, PlaceStep::StructField(field_idx),],
            )),
            Some(slot)
        );
    }
    assert_ne!(pair.cells[0].root, pair.cells[1].root);
    assert_eq!(pair.slot_count, 2);
}

#[test]
fn contract_field_layout_offsets_nested_holes_after_preceding_aggregate_fields() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "contract_field_layout_offsets_nested_holes_after_preceding_aggregate_fields.fe",
        ),
        r#"
use core::effect_ref::StorPtr

struct Slot<const ROOT: u256 = _> {}

struct TokenStore {
    total_supply: u256,
    balances: Slot,
    allowances: Slot,
}

contract C {
    store: StorPtr<TokenStore>
    mut after: u256
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let store = layout
        .get(&IdentId::new(&db, "store".to_string()))
        .expect("missing `store` field");
    let after = layout
        .get(&IdentId::new(&db, "after".to_string()))
        .expect("missing `after` field");

    // The holes must be offset past `total_supply`, which occupies the
    // aggregate's first slot; assigning them the aggregate base would alias
    // earlier storage.
    assert_eq!(
        store.assigned_slot_for_place(&storage_place(
            store,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(1)],
        )),
        Some(1)
    );
    assert_eq!(
        store.assigned_slot_for_place(&storage_place(
            store,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(2)],
        )),
        Some(2)
    );
    assert_eq!(store.slot_offset, 0);
    assert_eq!(store.slot_count, 3);
    assert_eq!(after.slot_offset, 3);
    assert!(store.target.all_roots_classified(&db));
}

#[test]
fn shadow_field_enumeration_splits_repeated_plain_slot_argument() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_field_enumeration_splits_repeated_plain_slot_argument.fe"),
        r#"
use std::evm::StorPtr

struct Slot<const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

contract C {
    values: StorPtr<Pair<Slot>>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let enumeration = field_enumeration(&db, top_mod, "C", 0);

    assert_eq!(enumeration.cells.len(), 2);
    assert_eq!(enumeration.place_roots.len(), 2);
    assert_ne!(enumeration.cells[0].root, enumeration.cells[1].root);
}

#[test]
fn shadow_field_enumeration_lands_alias_argument_per_pair_field() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_field_enumeration_shares_alias_formal_root.fe"),
        r#"
use std::evm::StorPtr

struct Slot<const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

type SharedPair<const ROOT: u256 = _> = Pair<Slot<ROOT>>

contract C {
    values: StorPtr<SharedPair>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let enumeration = field_enumeration(&db, top_mod, "C", 0);

    assert_eq!(enumeration.cells.len(), 2);
    assert_eq!(enumeration.place_roots.len(), 2);
    assert_ne!(enumeration.cells[0].root, enumeration.cells[1].root);
}

#[test]
fn shadow_field_enumeration_splits_alias_body_roots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_field_enumeration_splits_alias_body_roots.fe"),
        r#"
use std::evm::StorPtr

struct Slot<const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

type PairOfSlots = Pair<Slot>

contract C {
    values: StorPtr<PairOfSlots>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let enumeration = field_enumeration(&db, top_mod, "C", 0);

    assert_eq!(enumeration.cells.len(), 2);
    assert_eq!(enumeration.place_roots.len(), 2);
}

#[test]
fn shadow_field_enumeration_shares_definition_const_within_application() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_field_enumeration_shares_definition_const.fe"),
        r#"
use std::evm::StorPtr

struct Slot<const ROOT: u256> {}

struct Two<const ROOT: u256 = _> {
    left: Slot<ROOT>,
    right: Slot<ROOT>,
}

contract C {
    values: StorPtr<Two>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let enumeration = field_enumeration(&db, top_mod, "C", 0);

    assert_eq!(
        enumeration.cells.len(),
        1,
        "unexpected cells: {:#?}",
        enumeration.cells
    );
    assert_eq!(enumeration.place_roots.len(), 2);
    assert_eq!(enumeration.cells[0].occurrences.len(), 2);
}

#[test]
fn shadow_storage_layout_splits_nested_repeated_generic_arg_roots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_storage_layout_splits_nested_repeated_generic_arg_roots.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

struct S<T> {
    p: Pair<T>,
}

contract C {
    s: StorPtr<S<Slot<u256>>>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let s = field_storage_layout(&db, top_mod, "C", "s");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&s), vec![0, 1]);
    assert_eq!(
        s.assigned_slot_for_place(&storage_place(
            &s,
            [
                PlaceStep::ProviderTarget,
                PlaceStep::StructField(0),
                PlaceStep::StructField(0),
            ]
        )),
        Some(0)
    );
    assert_eq!(
        s.assigned_slot_for_place(&storage_place(
            &s,
            [
                PlaceStep::ProviderTarget,
                PlaceStep::StructField(0),
                PlaceStep::StructField(1),
            ]
        )),
        Some(1)
    );
    assert_eq!(s.slot_offset, 0);
    assert_eq!(s.slot_count, 2);
    assert_eq!(after.slot_offset, 2);
}

#[test]
fn shadow_storage_layout_lands_alias_argument_per_pair_field() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_storage_layout_shares_alias_formal_root.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

type SharedPair<const ROOT: u256 = _> = Pair<Slot<ROOT>>

contract C {
    values: StorPtr<SharedPair>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![0, 1]);
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(0)]
        )),
        Some(0)
    );
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(1)]
        )),
        Some(1)
    );
    assert_eq!(values.place_roots.len(), 2);
    assert_eq!(values.slot_count, 2);
    assert_eq!(after.slot_offset, 2);
}

#[test]
fn shadow_storage_layout_lands_pair_at_argument_per_pair_field() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_storage_layout_shares_pair_at_alias_formal_root.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

type PairAt<const ROOT: u256 = _> = Pair<Slot<u256, ROOT>>

contract C {
    values: StorPtr<PairAt>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![0, 1]);
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(0)]
        )),
        Some(0)
    );
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(1)]
        )),
        Some(1)
    );
    assert_eq!(values.slot_count, 2);
    assert_eq!(after.slot_offset, 2);
}

#[test]
fn shadow_storage_layout_mixed_alias_order_roots_follow_walk_order() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_storage_layout_mixed_alias_order_roots_follow_walk_order.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

type Mixed1<T> = (T, Slot<u256>)
type Mixed2<T> = (Slot<u256>, T)

contract C {
    x: StorPtr<Mixed1<Slot<u256>>>
    y: StorPtr<Mixed2<Slot<u256>>>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let x = field_storage_layout(&db, top_mod, "C", "x");
    let y = field_storage_layout(&db, top_mod, "C", "y");

    assert_eq!(counted_assigned_slots(&x), vec![0, 1]);
    assert_eq!(
        x.assigned_slot_for_place(&storage_place(
            &x,
            [PlaceStep::ProviderTarget, PlaceStep::TupleElem(0)]
        )),
        Some(0)
    );
    assert_eq!(
        x.assigned_slot_for_place(&storage_place(
            &x,
            [PlaceStep::ProviderTarget, PlaceStep::TupleElem(1)]
        )),
        Some(1)
    );
    assert_eq!(counted_assigned_slots(&y), vec![2, 3]);
    assert_eq!(
        y.assigned_slot_for_place(&storage_place(
            &y,
            [PlaceStep::ProviderTarget, PlaceStep::TupleElem(0)]
        )),
        Some(2)
    );
    assert_eq!(
        y.assigned_slot_for_place(&storage_place(
            &y,
            [PlaceStep::ProviderTarget, PlaceStep::TupleElem(1)]
        )),
        Some(3)
    );
}

#[test]
fn shadow_storage_layout_allows_explicit_concrete_duplicate_roots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_storage_layout_allows_explicit_concrete_duplicate_roots.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

contract C {
    values: StorPtr<Pair<Slot<u256, 5>>>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");
    let fields = values.target.template.field_types(&db);

    assert_eq!(const_lit_usize(&db, fields[0].generic_args(&db)[1]), 5);
    assert_eq!(const_lit_usize(&db, fields[1].generic_args(&db)[1]), 5);
    assert_eq!(values.slot_count, 0);
    assert_eq!(after.slot_offset, 0);
}

#[test]
fn shadow_storage_layout_splits_alias_formal_root_in_repeated_generic_arg() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "shadow_storage_layout_splits_alias_formal_root_in_repeated_generic_arg.fe",
        ),
        r#"
use core::effect_ref::StorPtr

struct Slot<const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

type AliasSlot<const ROOT: u256 = _> = Slot<ROOT>

contract C {
    values: StorPtr<Pair<AliasSlot>>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![0, 1]);
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(0)]
        )),
        Some(0)
    );
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(1)]
        )),
        Some(1)
    );
    assert_eq!(values.slot_count, 2);
    assert_eq!(after.slot_offset, 2);
}

#[test]
fn shadow_storage_layout_splits_alias_formal_root_in_nested_repeated_generic_arg() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "shadow_storage_layout_splits_alias_formal_root_in_nested_repeated_generic_arg.fe",
        ),
        r#"
use core::effect_ref::StorPtr

struct Slot<const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

struct S<T> {
    p: Pair<T>,
}

type AliasSlot<const ROOT: u256 = _> = Slot<ROOT>

contract C {
    values: StorPtr<S<AliasSlot>>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![0, 1]);
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [
                PlaceStep::ProviderTarget,
                PlaceStep::StructField(0),
                PlaceStep::StructField(0),
            ]
        )),
        Some(0)
    );
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [
                PlaceStep::ProviderTarget,
                PlaceStep::StructField(0),
                PlaceStep::StructField(1),
            ]
        )),
        Some(1)
    );
    assert_eq!(values.slot_count, 2);
    assert_eq!(after.slot_offset, 2);
}

#[test]
fn shadow_storage_layout_splits_alias_formal_root_in_repeated_tuple_alias_arg() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "shadow_storage_layout_splits_alias_formal_root_in_repeated_tuple_alias_arg.fe",
        ),
        r#"
use core::effect_ref::StorPtr

struct Slot<const ROOT: u256 = _> {}

type Twice<T> = (T, T)
type AliasSlot<const ROOT: u256 = _> = Slot<ROOT>

contract C {
    values: StorPtr<Twice<AliasSlot>>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![0, 1]);
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::TupleElem(0)]
        )),
        Some(0)
    );
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::TupleElem(1)]
        )),
        Some(1)
    );
    assert_eq!(values.slot_count, 2);
    assert_eq!(after.slot_offset, 2);
}

#[test]
fn shadow_storage_layout_splits_alias_formal_root_through_pair_alias_arg() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "shadow_storage_layout_splits_alias_formal_root_through_pair_alias_arg.fe",
        ),
        r#"
use core::effect_ref::StorPtr

struct Slot<const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

type PairAlias<T> = Pair<T>
type AliasSlot<const ROOT: u256 = _> = Slot<ROOT>

contract C {
    values: StorPtr<PairAlias<AliasSlot>>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![0, 1]);
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(0)]
        )),
        Some(0)
    );
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(1)]
        )),
        Some(1)
    );
    assert_eq!(values.slot_count, 2);
    assert_eq!(after.slot_offset, 2);
}

#[test]
fn shadow_storage_layout_groups_alias_formal_root_by_repeated_adt_landing_site() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "shadow_storage_layout_groups_alias_formal_root_by_repeated_adt_landing_site.fe",
        ),
        r#"
use core::effect_ref::StorPtr

struct Leaf<const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

type TwiceAt<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

contract C {
    values: StorPtr<Pair<TwiceAt>>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![0, 1]);
    for elem in [0, 1] {
        assert_eq!(
            values.assigned_slot_for_place(&storage_place(
                &values,
                [
                    PlaceStep::ProviderTarget,
                    PlaceStep::StructField(0),
                    PlaceStep::TupleElem(elem),
                ]
            )),
            Some(0)
        );
        assert_eq!(
            values.assigned_slot_for_place(&storage_place(
                &values,
                [
                    PlaceStep::ProviderTarget,
                    PlaceStep::StructField(1),
                    PlaceStep::TupleElem(elem),
                ]
            )),
            Some(1)
        );
    }
    assert_eq!(values.slot_count, 2);
    assert_eq!(after.slot_offset, 2);
}

#[test]
fn shadow_storage_layout_pair_alias_of_twice_at_has_two_landings() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_storage_layout_pair_alias_of_twice_at_known_oversplit.fe"),
        r#"
use core::effect_ref::StorPtr

struct Leaf<const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

type PairAlias<T> = Pair<T>
type TwiceAt<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

contract C {
    values: StorPtr<PairAlias<TwiceAt>>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");

    let left = [0, 1].map(|elem| {
        values
            .assigned_slot_for_place(&storage_place(
                &values,
                [
                    PlaceStep::ProviderTarget,
                    PlaceStep::StructField(0),
                    PlaceStep::TupleElem(elem),
                ],
            ))
            .expect("missing left landing root")
    });
    let right = [0, 1].map(|elem| {
        values
            .assigned_slot_for_place(&storage_place(
                &values,
                [
                    PlaceStep::ProviderTarget,
                    PlaceStep::StructField(1),
                    PlaceStep::TupleElem(elem),
                ],
            ))
            .expect("missing right landing root")
    });

    assert!(
        left.iter()
            .all(|left| right.iter().all(|right| left != right))
    );
    assert_eq!(left, [0, 0]);
    assert_eq!(right, [1, 1]);
    assert_eq!(values.slot_count, 2);
    assert_eq!(counted_assigned_slots(&values), vec![0, 1]);
}

#[test]
fn shadow_storage_layout_renamed_storptr_keeps_root_alias_formal_shared() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "shadow_storage_layout_renamed_storptr_keeps_root_alias_formal_shared.fe",
        ),
        r#"
use core::effect_ref::StorPtr as SP

struct Leaf<const ROOT: u256 = _> {}

type TwiceAt<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

contract C {
    values: SP<TwiceAt>
    after: SP<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![0]);
    for elem in [0, 1] {
        assert_eq!(
            values.assigned_slot_for_place(&storage_place(
                &values,
                [PlaceStep::ProviderTarget, PlaceStep::TupleElem(elem)]
            )),
            Some(0)
        );
    }
    assert_eq!(values.slot_count, 1);
    assert_eq!(after.slot_offset, 1);
}

#[test]
fn shadow_storage_layout_shares_tuple_alias_formal_root_outside_generic_arg() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "shadow_storage_layout_shares_tuple_alias_formal_root_outside_generic_arg.fe",
        ),
        r#"
use core::effect_ref::StorPtr

struct Leaf<const ROOT: u256 = _> {}

type TwiceAt<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

contract C {
    values: StorPtr<(TwiceAt, u256)>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![1]);
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [
                PlaceStep::ProviderTarget,
                PlaceStep::TupleElem(0),
                PlaceStep::TupleElem(0),
            ]
        )),
        Some(1)
    );
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [
                PlaceStep::ProviderTarget,
                PlaceStep::TupleElem(0),
                PlaceStep::TupleElem(1),
            ]
        )),
        Some(1)
    );
    assert_eq!(values.slot_count, 2);
    assert_eq!(after.slot_offset, 2);
}

#[test]
fn shadow_storage_layout_splits_associated_type_in_repeated_generic_arg() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "shadow_storage_layout_splits_associated_type_in_repeated_generic_arg.fe",
        ),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

struct Pair<T> {
    left: T,
    right: T,
}

trait HasElem {
    type Elem
}

struct X {}

impl HasElem for X {
    type Elem = Slot<u256>
}

contract C {
    values: StorPtr<Pair<X::Elem>>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![0, 1]);
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(0)]
        )),
        Some(0)
    );
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(1)]
        )),
        Some(1)
    );
    assert_eq!(values.slot_count, 2);
    assert_eq!(after.slot_offset, 2);
}

#[test]
fn shadow_storage_layout_shares_definition_const_within_application() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("shadow_storage_layout_shares_definition_const.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<const ROOT: u256> {}

struct Two<const ROOT: u256 = _> {
    left: Slot<ROOT>,
    right: Slot<ROOT>,
}

contract C {
    values: StorPtr<Two>
    after: StorPtr<u256>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let values = field_storage_layout(&db, top_mod, "C", "values");
    let after = field_storage_layout(&db, top_mod, "C", "after");

    assert_eq!(counted_assigned_slots(&values), vec![0]);
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(0)]
        )),
        Some(0)
    );
    assert_eq!(
        values.assigned_slot_for_place(&storage_place(
            &values,
            [PlaceStep::ProviderTarget, PlaceStep::StructField(1)]
        )),
        Some(0)
    );
    assert_eq!(values.place_roots.len(), 2);
    assert_eq!(values.slot_count, 1);
    assert_eq!(after.slot_offset, 1);
}

#[test]
fn contract_field_layout_counts_target_only_holes() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_layout_counts_target_only_holes.fe"),
        r#"
use core::effect_ref::EffectHandle

struct Payload<T, const ROOT: u256 = _> {}

struct Ptr<T> {
    raw: u256
}

impl<T> EffectHandle for Ptr<T> {
    type Target = Payload<T>

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Storage

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

contract C {
    first: Ptr<u256>
    mut second: u256
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

    let layout = allocated_fields(&db, contract);
    let first = layout
        .get(&IdentId::new(&db, "first".to_string()))
        .expect("missing `first` field");
    let second = layout
        .get(&IdentId::new(&db, "second".to_string()))
        .expect("missing `second` field");

    assert!(first.is_provider);
    assert_eq!(first.slot_count, 1);
    assert_eq!(second.slot_offset, 1);
    assert!(
        !ty_contains_const_hole(&db, concrete_target_ty(&db, first)),
        "concrete target-only layout view retained a hole: {:?}",
        first.target
    );
    assert!(first.target.all_roots_classified(&db));
}

#[test]
fn contract_field_layout_preserves_reordered_shared_target_holes() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_layout_preserves_reordered_shared_target_holes.fe"),
        r#"
use core::effect_ref::EffectHandle

struct Pair<const LEFT: u256, const RIGHT: u256> {}

struct Wrapper<const LEFT: u256 = _, const RIGHT: u256 = _> {
    raw: u256
}

impl<const LEFT: u256, const RIGHT: u256> EffectHandle for Wrapper<LEFT, RIGHT> {
    type Target = Pair<RIGHT, LEFT>

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Storage

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

contract C {
    first: Wrapper
    mut second: u256
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

    let layout = allocated_fields(&db, contract);
    let first = layout
        .get(&IdentId::new(&db, "first".to_string()))
        .expect("missing `first` field");
    let second = layout
        .get(&IdentId::new(&db, "second".to_string()))
        .expect("missing `second` field");
    let declared = concrete_declared_ty(&db, first);
    let target = concrete_target_ty(&db, first);
    let declared_args = declared.generic_args(&db);
    let target_args = target.generic_args(&db);

    assert!(first.is_provider);
    assert_eq!(declared_args.len(), 2);
    assert_eq!(target_args.len(), 2);
    assert_ne!(declared_args[0], declared_args[1]);
    assert_ne!(target_args[0], target_args[1]);
    assert_eq!(target_args[0], declared_args[1]);
    assert_eq!(target_args[1], declared_args[0]);
    assert_eq!(first.slot_count, 2);
    assert_eq!(second.slot_offset, 2);
    assert!(!ty_contains_const_hole(&db, declared));
    assert!(!ty_contains_const_hole(&db, target));
    assert!(first.declared.all_roots_classified(&db));
    assert!(first.target.all_roots_classified(&db));
}

#[test]
fn contract_field_layout_materializes_wrapper_only_holes() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_layout_ignores_wrapper_only_holes_for_slot_count.fe"),
        r#"
use core::effect_ref::EffectHandle

struct Wrapper<const ROOT: u256 = _> {
    raw: u256
}

impl<const ROOT: u256> EffectHandle for Wrapper<ROOT> {
    type Target = u256

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Storage

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

contract C {
    first: Wrapper
    mut second: u256
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

    let layout = allocated_fields(&db, contract);
    let first = layout
        .get(&IdentId::new(&db, "first".to_string()))
        .expect("missing `first` field");
    let second = layout
        .get(&IdentId::new(&db, "second".to_string()))
        .expect("missing `second` field");

    assert!(first.is_provider);
    assert_eq!(first.slot_count, 2);
    assert_eq!(second.slot_offset, 2);
    let wrapper_cell = first
        .cells
        .iter()
        .find(|cell| cell.role == RootRole::MaterializeOnly)
        .expect("wrapper-only root must have a real cell");
    assert_eq!(wrapper_cell.allocation.unwrap().slot, 1);
    assert!(!ty_contains_const_hole(
        &db,
        concrete_declared_ty(&db, first)
    ));
    assert!(first.declared.all_roots_classified(&db));
}

/// One placeholder shared by multiple enum variants must get a slot past
/// every variant's inline payload: variant payloads overlay, so a hole root
/// assigned at one variant's structural position can alias another variant's
/// inline data (here `B`'s `u256`), and the following field starts too early.
#[test]
fn contract_field_enum_variant_overlay_hole_past_inline_payload() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_enum_variant_overlay_hole_past_inline_payload.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

enum E<T> {
    A(T),
    B(u256, T),
}

contract C {
    e: StorPtr<E<Slot<u256>>>,
    after: StorPtr<u256>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let e = layout
        .get(&IdentId::new(&db, "e".to_string()))
        .expect("missing `e` field");
    let after = layout
        .get(&IdentId::new(&db, "after".to_string()))
        .expect("missing `after` field");

    // Inline span: tag (0) + widest payload (B's u256 at 1); the hole root
    // comes after, clear of both variants' inline data.
    for (variant, field) in [(0, 0), (1, 1)] {
        assert_eq!(
            e.assigned_slot_for_place(&storage_place(
                e,
                [
                    PlaceStep::ProviderTarget,
                    PlaceStep::EnumVariant(variant),
                    PlaceStep::EnumPayloadField(field),
                ],
            )),
            Some(2)
        );
    }
    assert_eq!(e.overlay_groups.len(), 1);
    assert_eq!(e.slot_count, 3);
    assert_eq!(after.slot_offset, 3);
}

/// Same layout regardless of which variant mentions the placeholder first.
#[test]
fn contract_field_enum_variant_overlay_holes_are_order_independent() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_enum_variant_overlay_holes_are_order_independent.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

enum E<T> {
    B(u256, T),
    A(T),
}

contract C {
    e: StorPtr<E<Slot<u256>>>,
    after: StorPtr<u256>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let e = layout
        .get(&IdentId::new(&db, "e".to_string()))
        .expect("missing `e` field");
    let after = layout
        .get(&IdentId::new(&db, "after".to_string()))
        .expect("missing `after` field");

    for (variant, field) in [(0, 1), (1, 0)] {
        assert_eq!(
            e.assigned_slot_for_place(&storage_place(
                e,
                [
                    PlaceStep::ProviderTarget,
                    PlaceStep::EnumVariant(variant),
                    PlaceStep::EnumPayloadField(field),
                ],
            )),
            Some(2)
        );
    }
    assert_eq!(e.overlay_groups.len(), 1);
    assert_eq!(e.slot_count, 3);
    assert_eq!(after.slot_offset, 3);
}

/// Inline data *following* a placeholder in the same variant must not be
/// overlapped either: the root goes past the whole inline span, not just the
/// components preceding it (a per-variant-maximum rule would fail here).
#[test]
fn contract_field_enum_variant_hole_before_trailing_inline_data() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_enum_variant_hole_before_trailing_inline_data.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

enum E<T> {
    A(T, u256),
    B(u256, T),
}

contract C {
    e: StorPtr<E<Slot<u256>>>,
    after: StorPtr<u256>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let e = layout
        .get(&IdentId::new(&db, "e".to_string()))
        .expect("missing `e` field");
    let after = layout
        .get(&IdentId::new(&db, "after".to_string()))
        .expect("missing `after` field");

    for (variant, field) in [(0, 0), (1, 1)] {
        assert_eq!(
            e.assigned_slot_for_place(&storage_place(
                e,
                [
                    PlaceStep::ProviderTarget,
                    PlaceStep::EnumVariant(variant),
                    PlaceStep::EnumPayloadField(field),
                ],
            )),
            Some(2)
        );
    }
    assert_eq!(e.overlay_groups.len(), 1);
    assert_eq!(e.slot_count, 3);
    assert_eq!(after.slot_offset, 3);
}

#[test]
fn contract_field_enum_same_variant_duplicate_root_gets_distinct_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_enum_same_variant_duplicate_root_gets_distinct_slots.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

enum E<T> {
    A(T, T),
}

contract C {
    e: StorPtr<E<Slot<u256>>>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let e = field_storage_layout(&db, top_mod, "C", "e");

    assert_eq!(e.slot_count, 3);
    assert_eq!(
        e.assigned_slot_for_place(&storage_place(
            &e,
            [
                PlaceStep::ProviderTarget,
                PlaceStep::EnumVariant(0),
                PlaceStep::EnumPayloadField(0),
            ],
        )),
        Some(1)
    );
    assert_eq!(
        e.assigned_slot_for_place(&storage_place(
            &e,
            [
                PlaceStep::ProviderTarget,
                PlaceStep::EnumVariant(0),
                PlaceStep::EnumPayloadField(1),
            ],
        )),
        Some(2)
    );
}

#[test]
fn contract_field_array_of_slot_wrappers_uses_a_symbolic_family() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_array_of_slot_wrappers_shares_one_root.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<T, const ROOT: u256 = _> {}

contract C {
    arr: StorPtr<[Slot<u256>; 3]>,
    after: StorPtr<u256>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let arr = layout
        .get(&IdentId::new(&db, "arr".to_string()))
        .expect("missing `arr` field");
    let after = layout
        .get(&IdentId::new(&db, "after".to_string()))
        .expect("missing `after` field");

    assert_eq!(arr.families.len(), 1);
    assert_eq!(arr.families[0].extent, 3);
    assert_eq!(arr.slot_count, 3);
    assert_eq!(after.slot_offset, 3);
    assert!(ty_contains_const_hole(&db, arr.target.template));
}

/// Transient-storage fields get their own slot space: roots restart at zero
/// independently of persistent storage, including symbolic root families.
#[test]
fn contract_field_transient_array_of_slot_wrappers_uses_transient_family() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_transient_array_of_slot_wrappers.fe"),
        r#"
use core::effect_ref::StorPtr
use std::evm::TStorPtr

struct Slot<T, const ROOT: u256 = _> {}

contract C {
    persistent: StorPtr<Slot<u256>>,
    tarr: TStorPtr<[Slot<u256>; 3]>,
    tafter: TStorPtr<u256>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let persistent = layout
        .get(&IdentId::new(&db, "persistent".to_string()))
        .expect("missing `persistent` field");
    let tarr = layout
        .get(&IdentId::new(&db, "tarr".to_string()))
        .expect("missing `tarr` field");
    let tafter = layout
        .get(&IdentId::new(&db, "tafter".to_string()))
        .expect("missing `tafter` field");

    assert_ne!(persistent.address_space, tarr.address_space);

    let persistent_root = concrete_target_ty(&db, persistent)
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing persistent ROOT const arg");
    assert_eq!(const_lit_usize(&db, persistent_root), 0);
    assert_eq!(persistent.slot_count, 1);
    assert_eq!(tarr.families.len(), 1);
    assert_eq!(tarr.families[0].extent, 3);
    assert_eq!(tarr.families[0].space, tarr.address_space);
    assert_eq!(tarr.slot_count, 3);
    assert_eq!(tafter.slot_offset, 3);
    assert!(ty_contains_const_hole(&db, tarr.target.template));
}

/// `Mutex` carries its reentrancy lock as a zero-sized `TSlot<bool>` field:
/// the lock slot is assigned from the contract's *transient* counter (the
/// param's ADT implements `core::effect_ref::StaticSlot`), shared with
/// `TStorPtr` provider fields, so lock bits can never collide with other
/// transient state — and the mutex consumes no persistent slot for the lock.
#[test]
fn contract_field_mutex_lock_slots_share_transient_counter() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_mutex_lock_slots_share_transient_counter.fe"),
        r#"
use std::evm::Mutex
use std::evm::effects::TStorPtr

contract C {
    t0: TStorPtr<bool>,
    t1: TStorPtr<bool>,
    mut m: Mutex<u256>,
    mut m2: Mutex<u256>,
    after: TStorPtr<bool>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = allocated_fields(&db, contract);
    let field = |name: &str| {
        layout
            .get(&IdentId::new(&db, name.to_string()))
            .expect("missing field")
    };
    let lock_slot = |name: &str| {
        field(name)
            .cells
            .iter()
            .find(|cell| cell.space == fe_hir::analysis::ty::ProviderAddressSpace::Transient)
            .and_then(|cell| cell.allocation)
            .map(|allocation| allocation.slot)
            .expect("missing transient lock allocation")
    };

    // Transient provider fields take 0 and 1; the two lock bits continue the
    // same counter; the next transient field lands after them.
    assert_eq!(field("t0").slot_offset, 0);
    assert_eq!(field("t1").slot_offset, 1);
    assert_eq!(lock_slot("m"), 2);
    assert_eq!(lock_slot("m2"), 3);
    assert_eq!(field("after").slot_offset, 4);

    // The lock consumes no persistent storage: one slot per mutex (the value).
    assert_eq!(field("m").slot_count, 1);
    assert_eq!(field("m").slot_offset, 0);
    assert_eq!(field("m2").slot_offset, 1);

    assert!(
        contract
            .storage_layout(&db)
            .field_errors(&IdentId::new(&db, "m"))
            .is_none()
    );
    assert!(
        contract
            .storage_layout(&db)
            .field_errors(&IdentId::new(&db, "m2"))
            .is_none()
    );
}

#[test]
fn contract_field_param_dependent_static_slot_space_uses_concrete_owner() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_param_dependent_static_slot_space_is_rejected.fe"),
        r#"
use core::effect_ref::{AddressSpace, StaticSlot}

struct ParamSlot<const SP: AddressSpace, const SLOT: u256 = _> {}

impl<const SP: AddressSpace, const SLOT: u256> StaticSlot for ParamSlot<SP, SLOT> {
    const SPACE: AddressSpace = SP
}

contract C {
    lock: ParamSlot<AddressSpace::TransientStorage>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    let contract = find_contract(&db, top_mod, "C");
    let field = allocated_fields(&db, contract)
        .get(&IdentId::new(&db, "lock".to_string()))
        .cloned()
        .expect("missing `lock` field");
    assert_eq!(field.cells.len(), 1);
    assert_eq!(
        field.cells[0].space,
        fe_hir::analysis::ty::ProviderAddressSpace::Transient
    );
    assert_eq!(field.cells[0].allocation.unwrap().slot, 0);
    assert_eq!(field.slot_count, 0);
}

/// A storage-slot (`u256`) const hole is a legitimate contract-field layout
/// hole: it is numbered as a slot and the field is accepted. Guards the
/// non-slot rejection below from over-rejecting real slots.
#[test]
fn contract_field_u256_slot_hole_is_not_rejected() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_u256_slot_hole_is_not_rejected.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<const ROOT: u256 = _> {}

contract C {
    value: StorPtr<Slot>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let field = allocated_fields(&db, contract)
        .get(&IdentId::new(&db, "value".to_string()))
        .cloned()
        .expect("missing `value` field");
    assert!(
        contract
            .storage_layout(&db)
            .field_errors(&IdentId::new(&db, "value"))
            .is_none()
    );
    assert_eq!(field.slot_count, 1);
}

/// A `usize` const hole is also a valid storage-slot index and must be accepted.
#[test]
fn contract_field_usize_slot_hole_is_not_rejected() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_usize_slot_hole_is_not_rejected.fe"),
        r#"
use core::effect_ref::StorPtr

struct Slot<const ROOT: usize = _> {}

contract C {
    value: StorPtr<Slot>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let field = allocated_fields(&db, contract)
        .get(&IdentId::new(&db, "value".to_string()))
        .cloned()
        .expect("missing `value` field");
    assert!(
        contract
            .storage_layout(&db)
            .field_errors(&IdentId::new(&db, "value"))
            .is_none()
    );
    assert_eq!(field.slot_count, 1);
}

/// A plain (non-provider) field with a defaulted non-slot const generic
/// (`const SP: AddressSpace = _`) is flagged: the hole is not a storage slot.
/// (`ContractAnalysisPass` turns the flag into `error[3-0040]`, covered
/// end-to-end by the `contract_field_nonprovider_addrspace_hole` uitest.)
#[test]
fn contract_field_nonprovider_addrspace_hole_is_flagged() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_nonprovider_addrspace_hole_is_flagged.fe"),
        r#"
use core::effect_ref::AddressSpace

struct Foo<const SP: AddressSpace = _> { value: u256 }

contract C {
    value: Foo,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contract = find_contract(&db, top_mod, "C");
    let errors = contract
        .storage_layout(&db)
        .field_errors(&IdentId::new(&db, "value".to_string()))
        .expect("field should be rejected");
    assert!(matches!(
        errors.first(),
        Some(fe_hir::core::semantic::ContractLayoutError::NonSlotContractLayoutHole { .. })
    ));
    assert!(contract.storage_layout(&db).allocated.is_none());
}

/// A non-`u256` integer hole (`const TAG: u8 = _`) is not a storage-slot index,
/// so it must be rejected rather than silently numbered as a slot.
#[test]
fn contract_field_non_u256_integer_hole_is_flagged() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_non_u256_integer_hole_is_flagged.fe"),
        r#"
struct Foo<const TAG: u8 = _> { value: u256 }

contract C {
    value: Foo,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contract = find_contract(&db, top_mod, "C");
    let errors = contract
        .storage_layout(&db)
        .field_errors(&IdentId::new(&db, "value".to_string()))
        .expect("field should be rejected");
    assert!(matches!(
        errors.first(),
        Some(fe_hir::core::semantic::ContractLayoutError::NonSlotContractLayoutHole { .. })
    ));
    assert!(contract.storage_layout(&db).allocated.is_none());
}

/// An `EffectHandle` field whose address space is left inferred (`const SPACE =
/// SP` with `SP` defaulted to `_`) is flagged: the field's storage space is
/// unknown. (`ContractAnalysisPass` turns the flag into `error[3-0041]`,
/// covered end-to-end by the `contract_field_handle_space_unresolved` uitest.)
#[test]
fn contract_field_handle_space_hole_is_flagged() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_handle_space_hole_is_flagged.fe"),
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Ptr<T, const SP: AddressSpace = _> { raw: u256 }

impl<T, const SP: AddressSpace> EffectHandle for Ptr<T, SP> {
    type Target = T

    const SPACE: AddressSpace = SP

    fn from_raw(_ raw: u256) -> Self {
        Ptr { raw }
    }

    fn raw(self) -> u256 {
        self.raw
    }
}

contract C {
    mut value: Ptr<u256>,
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contract = find_contract(&db, top_mod, "C");
    let errors = contract
        .storage_layout(&db)
        .field_errors(&IdentId::new(&db, "value".to_string()))
        .expect("field should be rejected");
    assert!(matches!(
        errors.first(),
        Some(fe_hir::core::semantic::ContractLayoutError::UnresolvedProviderSpace)
    ));
    assert!(contract.storage_layout(&db).allocated.is_none());
}

/// An explicit `_` const argument (here `String<_>`, whose `usize` length is a
/// byte capacity, not a storage slot) must be rejected: layout holes may only
/// come from a `= _` parameter default, not an explicit use-site argument.
#[test]
fn contract_field_explicit_const_hole_is_flagged() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_field_explicit_const_hole_is_flagged.fe"),
        "contract C { value: String<_> }",
    );
    let (top_mod, _) = db.top_mod(file);
    let contract = find_contract(&db, top_mod, "C");
    let errors = contract
        .storage_layout(&db)
        .field_errors(&IdentId::new(&db, "value".to_string()))
        .expect("field should be rejected");
    assert!(matches!(
        errors.first(),
        Some(fe_hir::core::semantic::ContractLayoutError::ExplicitContractLayoutHole { .. })
    ));
    assert!(contract.storage_layout(&db).allocated.is_none());
}
