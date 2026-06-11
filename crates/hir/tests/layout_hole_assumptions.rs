use camino::Utf8PathBuf;
use fe_hir::analysis::ty::{
    const_ty::{ConstTyData, EvaluatedConstTy},
    ty_check::{check_contract_recv_arm_body, check_func_body},
    ty_contains_const_hole,
    ty_def::{TyData, strip_derived_adt_layout_args},
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
    let field_ty = contract
        .fields(&db)
        .get(&IdentId::new(&db, "guarded_balances".to_string()))
        .expect("missing field")
        .target_ty
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
    assert_eq!(field_ty, "Mutex<StorageMap<Address, u256, 0>, 0>");
    assert_eq!(receiver_ty, "Mutex<StorageMap<Address, u256, 0>, 0>");
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
        .field_layout(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `store` field layout");
    let field_info = contract
        .fields(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `store` field info");

    assert_eq!(
        field_info.target_ty.pretty_print(&db).to_string(),
        "Store<0, 1>"
    );
    assert_eq!(
        strip_derived_adt_layout_args(&db, field_layout.target_ty),
        field_info.target_ty
    );
}

#[test]
fn contract_fields_strip_nested_wrapper_only_layout_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_fields_strip_nested_wrapper_only_layout_args.fe"),
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
        .field_layout(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `wrapped` field layout");
    let field_info = contract
        .fields(&db)
        .get(&field_name)
        .cloned()
        .expect("missing `wrapped` field info");
    assert_eq!(
        field_info.target_ty.pretty_print(&db).to_string(),
        "Wrapper<Mutex<StorageMap<Address, u256, 0>, 0>>"
    );
    assert_eq!(
        strip_derived_adt_layout_args(&db, field_layout.target_ty),
        field_info.target_ty
    );

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
        "Mutex<StorageMap<Address, u256, 0>, 0>"
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
fn adt_fields_consume_layout_args_from_instantiated_explicit_field_types() {
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
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let typed_body = check_func_body(&db, func).1.clone();
    let field_a_ty = typed_body.expr_ty(&db, find_field_expr(&db, func, "a"));
    let field_b_ty = typed_body.expr_ty(&db, find_field_expr(&db, func, "b"));
    let expected_a = find_func(&db, top_mod, "takes_root_2").arg_tys(&db)[0].instantiate_identity();
    let expected_a = expected_a.as_view(&db).unwrap_or(expected_a);
    let expected_b = find_func(&db, top_mod, "takes_root_3").arg_tys(&db)[0].instantiate_identity();
    let expected_b = expected_b.as_view(&db).unwrap_or(expected_b);

    assert_eq!(field_a_ty, expected_a);
    assert_eq!(field_b_ty, expected_b);
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
        .field_layout(&db)
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
    assert_eq!(
        strip_derived_adt_layout_args(&db, field_layout.declared_ty),
        field_info.declared_ty
    );
    assert_eq!(
        strip_derived_adt_layout_args(&db, field_layout.target_ty),
        field_info.target_ty
    );
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
    let layout = contract.field_layout(&db);
    let pair = layout
        .get(&IdentId::new(&db, "pair".to_string()))
        .expect("missing `pair` field");

    let fields = pair.target_ty.field_types(&db);
    assert_eq!(fields.len(), 2);
    let left_root = fields[0]
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing left root const arg");
    let right_root = fields[1]
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing right root const arg");

    assert_eq!(const_lit_usize(&db, left_root), 0);
    assert_eq!(const_lit_usize(&db, right_root), 1);
    assert_eq!(pair.slot_count, 2);
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
    let layout = contract.field_layout(&db);
    let pair = layout
        .get(&IdentId::new(&db, "pair".to_string()))
        .expect("missing `pair` field");

    let fields = pair.target_ty.field_types(&db);
    assert_eq!(fields.len(), 2);
    let left_root = fields[0]
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing left root const arg");
    let right_root = fields[1]
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing right root const arg");

    assert_ne!(
        left_root, right_root,
        "alias-expanded holes silently merged"
    );
    assert_eq!(const_lit_usize(&db, left_root), 0);
    assert_eq!(const_lit_usize(&db, right_root), 1);
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
    let layout = contract.field_layout(&db);
    let store = layout
        .get(&IdentId::new(&db, "store".to_string()))
        .expect("missing `store` field");
    let after = layout
        .get(&IdentId::new(&db, "after".to_string()))
        .expect("missing `after` field");

    let store_fields = store.target_ty.field_types(&db);
    assert_eq!(store_fields.len(), 3);
    let balances_root = store_fields[1]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing balances root const arg");
    let allowances_root = store_fields[2]
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing allowances root const arg");

    // The holes must be offset past `total_supply`, which occupies the
    // aggregate's first slot; assigning them the aggregate base would alias
    // earlier storage.
    assert_eq!(const_lit_usize(&db, balances_root), 1);
    assert_eq!(const_lit_usize(&db, allowances_root), 2);
    assert_eq!(store.slot_offset, 0);
    assert_eq!(store.slot_count, 3);
    assert_eq!(after.slot_offset, 3);
    assert!(
        !ty_contains_const_hole(&db, store.target_ty),
        "unelaborated const hole remained in nested aggregate layout type: {:?}",
        store.target_ty
    );
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

    let layout = contract.field_layout(&db);
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
        !ty_contains_const_hole(&db, first.target_ty),
        "unelaborated const hole remained in target-only layout type: {:?}",
        first.target_ty
    );
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

    let layout = contract.field_layout(&db);
    let first = layout
        .get(&IdentId::new(&db, "first".to_string()))
        .expect("missing `first` field");
    let second = layout
        .get(&IdentId::new(&db, "second".to_string()))
        .expect("missing `second` field");
    let declared_args = first.declared_ty.generic_args(&db);
    let target_args = first.target_ty.generic_args(&db);

    assert!(first.is_provider);
    assert_eq!(declared_args.len(), 2);
    assert_eq!(target_args.len(), 2);
    assert_ne!(declared_args[0], declared_args[1]);
    assert_ne!(target_args[0], target_args[1]);
    assert_eq!(target_args[0], declared_args[1]);
    assert_eq!(target_args[1], declared_args[0]);
    assert_eq!(first.slot_count, 2);
    assert_eq!(second.slot_offset, 2);
    assert!(
        !ty_contains_const_hole(&db, first.declared_ty),
        "unelaborated const hole remained in reordered wrapper layout type: {:?}",
        first.declared_ty
    );
    assert!(
        !ty_contains_const_hole(&db, first.target_ty),
        "unelaborated const hole remained in reordered target layout type: {:?}",
        first.target_ty
    );
}

#[test]
fn contract_field_layout_ignores_wrapper_only_holes_for_slot_count() {
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

    let layout = contract.field_layout(&db);
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
        !ty_contains_const_hole(&db, first.declared_ty),
        "unelaborated const hole remained in wrapper-only layout type: {:?}",
        first.declared_ty
    );
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
    let layout = contract.field_layout(&db);
    let e = layout
        .get(&IdentId::new(&db, "e".to_string()))
        .expect("missing `e` field");
    let after = layout
        .get(&IdentId::new(&db, "after".to_string()))
        .expect("missing `after` field");

    let root = e
        .target_ty
        .generic_args(&db)
        .first()
        .and_then(|arg| arg.generic_args(&db).get(1))
        .copied()
        .expect("missing ROOT const arg");

    // Inline span: tag (0) + widest payload (B's u256 at 1); the hole root
    // comes after, clear of both variants' inline data.
    assert_eq!(const_lit_usize(&db, root), 2);
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
    let layout = contract.field_layout(&db);
    let e = layout
        .get(&IdentId::new(&db, "e".to_string()))
        .expect("missing `e` field");
    let after = layout
        .get(&IdentId::new(&db, "after".to_string()))
        .expect("missing `after` field");

    let root = e
        .target_ty
        .generic_args(&db)
        .first()
        .and_then(|arg| arg.generic_args(&db).get(1))
        .copied()
        .expect("missing ROOT const arg");

    assert_eq!(const_lit_usize(&db, root), 2);
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
    let layout = contract.field_layout(&db);
    let e = layout
        .get(&IdentId::new(&db, "e".to_string()))
        .expect("missing `e` field");
    let after = layout
        .get(&IdentId::new(&db, "after".to_string()))
        .expect("missing `after` field");

    let root = e
        .target_ty
        .generic_args(&db)
        .first()
        .and_then(|arg| arg.generic_args(&db).get(1))
        .copied()
        .expect("missing ROOT const arg");

    assert_eq!(const_lit_usize(&db, root), 2);
    assert_eq!(e.slot_count, 3);
    assert_eq!(after.slot_offset, 3);
}

/// `[Slot<u256>; N]`: the element type is instantiated once, so all elements
/// share one hole and one storage root. The array's inline span is zero
/// (holes are out-of-line) and the field reserves exactly one slot for the
/// shared root — elements alias by construction, they are not N independent
/// slots.
#[test]
fn contract_field_array_of_slot_wrappers_shares_one_root() {
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
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.field_layout(&db);
    let arr = layout
        .get(&IdentId::new(&db, "arr".to_string()))
        .expect("missing `arr` field");
    let after = layout
        .get(&IdentId::new(&db, "after".to_string()))
        .expect("missing `after` field");

    let elem = arr
        .target_ty
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing array element type");
    let root = elem
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing ROOT const arg");

    assert_eq!(const_lit_usize(&db, root), 0);
    assert_eq!(arr.slot_count, 1);
    assert_eq!(after.slot_offset, 1);
}

/// Transient-storage fields get their own slot space: roots restart at zero
/// independently of persistent storage, and an array of transient slot
/// wrappers shares one root exactly like its persistent counterpart.
#[test]
fn contract_field_transient_array_of_slot_wrappers_uses_independent_space() {
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
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.field_layout(&db);
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

    let persistent_root = persistent
        .target_ty
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing persistent ROOT const arg");
    let tarr_elem = tarr
        .target_ty
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing transient array element type");
    let tarr_root = tarr_elem
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing transient ROOT const arg");

    // Same numeric root in disjoint address spaces: the slot counters are
    // independent, so the transient array starts at zero even though the
    // persistent field already occupies storage slot zero.
    assert_eq!(const_lit_usize(&db, persistent_root), 0);
    assert_eq!(persistent.slot_count, 1);
    assert_eq!(const_lit_usize(&db, tarr_root), 0);
    assert_eq!(tarr.slot_count, 1);
    assert_eq!(tafter.slot_offset, 1);
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
    let layout = contract.field_layout(&db);
    let field = |name: &str| {
        layout
            .get(&IdentId::new(&db, name.to_string()))
            .expect("missing field")
    };
    let lock_slot = |name: &str| {
        let arg = field(name)
            .target_ty
            .generic_args(&db)
            .get(1)
            .copied()
            .expect("missing lock slot arg");
        const_lit_usize(&db, arg)
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
}
