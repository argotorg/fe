use camino::Utf8PathBuf;
use common::diagnostics::{CompleteDiagnostic, cmp_complete_diagnostics};
use cranelift_entity::EntityRef;
use fe_hir::analysis::semantic::{
    GenericSubst, SemanticInstance, instantiate_typed_body, instantiated_effect_env,
    root_semantic_instance_key, typed_body_template, validate_instantiated_effect_env_key,
};
use fe_hir::analysis::ty::effects::{EffectKeyKind, place_effect_provider_param_index_map};
use fe_hir::analysis::ty::evidence::EvidenceList;
use fe_hir::analysis::ty::evidence::prove;
use fe_hir::analysis::ty::trait_resolution::TraitSolveCx;
use fe_hir::analysis::ty::ty_check::{
    BodyOwner, EffectPassMode, TypedBody, check_contract_recv_arm_body, check_func_body,
};
use fe_hir::analysis::ty::ty_def::{TyData, TyId};
use fe_hir::hir_def::{CallableDef, Contract, Expr, ExprId, Func, ItemKind, Partial, TopLevelMod};
use fe_hir::semantic::param_env;
use fe_hir::test_db::{HirAnalysisTestDb, initialize_test_analysis_pass};

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
        .unwrap_or_else(|| panic!("missing `{name}` contract"))
}

fn find_call_expr<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>) -> ExprId {
    let body = func.body(db).expect("missing function body");
    body.exprs(db)
        .keys()
        .find(|expr| matches!(expr.data(db, body), Partial::Present(Expr::Call(..))))
        .expect("missing call expression")
}

fn find_named_call_expr<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>, name: &str) -> ExprId {
    let body = func.body(db).expect("missing function body");
    body.exprs(db)
        .keys()
        .find(|expr| {
            let Partial::Present(Expr::Call(callee, _)) = expr.data(db, body) else {
                return false;
            };
            let Partial::Present(Expr::Path(Partial::Present(path))) = callee.data(db, body) else {
                return false;
            };
            path.as_ident(db)
                .is_some_and(|ident| ident.data(db) == name)
        })
        .unwrap_or_else(|| panic!("missing call expression for `{name}`"))
}

fn find_method_call_expr<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>) -> ExprId {
    let body = func.body(db).expect("missing function body");
    body.exprs(db)
        .keys()
        .find(|expr| matches!(expr.data(db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing method call expression")
}

fn find_call_expr_in_typed_body<'db>(
    db: &'db HirAnalysisTestDb,
    typed_body: &TypedBody<'db>,
) -> ExprId {
    let body = typed_body.body().expect("missing typed body");
    body.exprs(db)
        .keys()
        .find(|expr| matches!(expr.data(db, body), Partial::Present(Expr::Call(..))))
        .expect("missing call expression")
}

fn assert_single_trait_effect_arg<'db>(typed_body: &TypedBody<'db>, call_expr: ExprId) {
    let effect_args = typed_body
        .call_effect_args(call_expr)
        .expect("missing resolved effect args");
    assert_eq!(effect_args.len(), 1);
    assert_eq!(effect_args[0].key_kind, EffectKeyKind::Trait);
}

fn assert_single_type_effect_arg<'db>(typed_body: &TypedBody<'db>, call_expr: ExprId) {
    let effect_args = typed_body
        .call_effect_args(call_expr)
        .expect("missing resolved effect args");
    assert_eq!(effect_args.len(), 1);
    assert_eq!(effect_args[0].key_kind, EffectKeyKind::Type);
}

fn assert_trait_effect_provider_arg<'db>(
    db: &'db HirAnalysisTestDb,
    caller: Func<'db>,
    callee: Func<'db>,
    call_expr: ExprId,
    expected: &str,
) {
    let typed_body = check_func_body(db, caller).1.clone();
    let callable = typed_body
        .callable_expr(call_expr)
        .expect("missing callable for effectful call");
    let provider_arg_idx = place_effect_provider_param_index_map(db, callee)
        .first()
        .copied()
        .flatten()
        .expect("missing hidden provider arg index");
    let provider_arg = callable
        .generic_args()
        .get(provider_arg_idx)
        .copied()
        .expect("missing hidden provider generic arg");
    assert_eq!(provider_arg.pretty_print(db).to_string(), expected);
}

fn assert_callable_provider_arg<'db>(
    db: &'db HirAnalysisTestDb,
    caller: Func<'db>,
    call_expr: ExprId,
    expected: &str,
) {
    let typed_body = check_func_body(db, caller).1.clone();
    let callable = typed_body
        .callable_expr(call_expr)
        .expect("missing callable for effectful call");
    let CallableDef::Func(callee) = callable.callable_def else {
        panic!("expected function callable");
    };
    let provider_arg_idx = place_effect_provider_param_index_map(db, callee)
        .first()
        .copied()
        .flatten()
        .expect("missing hidden provider arg index");
    let provider_arg = callable
        .generic_args()
        .get(provider_arg_idx)
        .copied()
        .expect("missing hidden provider generic arg");
    assert_eq!(provider_arg.pretty_print(db).to_string(), expected);
}

fn assert_all_calls_have_callables<'db>(
    db: &'db HirAnalysisTestDb,
    owner: &str,
    typed_body: &TypedBody<'db>,
) {
    let body = typed_body.body().expect("missing typed body");
    for expr in body.exprs(db).keys() {
        if matches!(
            expr.data(db, body),
            Partial::Present(Expr::Call(..) | Expr::MethodCall(..))
        ) {
            assert!(
                typed_body.callable_expr(expr).is_some(),
                "missing callable for {owner} expression {expr:?}"
            );
        }
    }
}

fn assert_callable_generic_arg<'db>(
    db: &'db HirAnalysisTestDb,
    caller: Func<'db>,
    call_expr: ExprId,
    arg_idx: usize,
    expected: &str,
) {
    let typed_body = check_func_body(db, caller).1.clone();
    let callable = typed_body
        .callable_expr(call_expr)
        .expect("missing callable for effectful call");
    let generic_arg = callable
        .generic_args()
        .get(arg_idx)
        .copied()
        .expect("missing callable generic arg");
    assert_eq!(generic_arg.pretty_print(db).to_string(), expected);
}

fn diagnostics_for<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<CompleteDiagnostic> {
    let mut manager = initialize_test_analysis_pass();
    let mut diags: Vec<_> = manager
        .run_on_module(db, top_mod)
        .into_iter()
        .map(|diag| diag.to_complete(db))
        .collect();
    diags.sort_by(cmp_complete_diagnostics);
    diags
}

#[test]
fn impl_method_effect_keys_match_after_assoc_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("impl_method_effect_keys_match_after_assoc_normalization.fe"),
        r#"
trait Cap<T> {}

trait HasSlot {
    type Assoc
}

struct Slot<T> {}
struct S {}

trait T {
    fn f<X>(self, x: X) uses (cap: Cap<X::Assoc>)
    where
        X: HasSlot<Assoc = Slot<u256>>
}

impl T for S {
    fn f<X>(self, x: X) uses (cap: Cap<Slot<u256>>)
    where
        X: HasSlot<Assoc = Slot<u256>>
    {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn impl_method_effect_keys_match_with_omitted_const_expr_defaults() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("impl_method_effect_keys_match_with_omitted_const_expr_defaults.fe"),
        r#"
const fn plus1(_ x: usize) -> usize {
    x + 1
}

trait Cap<T> {}

struct Slot<const N: usize, const M: usize = plus1(N)> {}
struct S {}

trait T {
    fn f(self) uses (cap: Cap<Slot<4>>)
}

impl T for S {
    fn f(self) uses (cap: Cap<Slot<4, 5>>) {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn impl_method_effect_keys_match_with_substituted_omitted_const_expr_defaults() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "impl_method_effect_keys_match_with_substituted_omitted_const_expr_defaults.fe",
        ),
        r#"
const fn plus1(_ x: usize) -> usize {
    x + 1
}

trait Cap<T> {}

struct Slot<const N: usize, const M: usize = plus1(N)> {}
struct S<const N: usize> {}

trait T<const N: usize> {
    fn f(self) uses (cap: Cap<Slot<N>>)
}

impl<const N: usize> T<N> for S<N> {
    fn f(self) uses (cap: Cap<Slot<N, plus1(N)>>) {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn impl_method_effect_keys_match_after_trait_const_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("impl_method_effect_keys_match_after_trait_const_normalization.fe"),
        r#"
trait HasRoot {
    const ROOT: u256
}

trait Cap<T> {}

struct Slot<const ROOT: u256> {}
struct Root {}
struct S {}

impl HasRoot for Root {
    const ROOT: u256 = 7
}

trait T {
    fn f(self) uses (cap: Cap<Slot<Root::ROOT>>)
}

impl T for S {
    fn f(self) uses (cap: Cap<Slot<7>>) {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn ordinary_calls_use_keyed_trait_effect_witnesses_after_assoc_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "ordinary_calls_use_keyed_trait_effect_witnesses_after_assoc_normalization.fe",
        ),
        r#"
trait Cap<T> {}

trait HasSlot {
    type Assoc
}

struct Slot<T> {}

fn needs<X>(_: u256) uses (cap: Cap<X::Assoc>)
where
    X: HasSlot<Assoc = Slot<u256>>
{}

fn caller<X>() uses (cap: Cap<Slot<u256>>)
where
    X: HasSlot<Assoc = Slot<u256>>
{
    let out: () = needs<X>(x: 1)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let needs = find_func(&db, top_mod, "needs");
    let requirements = needs.effect_requirements(&db, None);
    let key = requirements[0].key.key_trait().expect("trait effect key");
    assert_eq!(
        key.args(&db)[1].generic_args(&db),
        &[TyId::u256(&db)],
        "associated type must normalize to Slot<u256>"
    );
    assert_eq!(
        place_effect_provider_param_index_map(&db, needs),
        &[Some(0)]
    );
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
}

#[test]
fn effect_provider_slots_follow_inherited_parameters() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "effect_provider_slots_follow_inherited_parameters.fe".into(),
        r#"
trait HasRoot { const ROOT: u256 }
trait Cap<T> {}
struct Slot<const ROOT: u256> {}
struct Holder<T> { value: T }

impl<T: HasRoot> Holder<T> {
    fn needs<U = T>() uses (fixed: Slot<0>, exact: Slot<T::ROOT>, cap: Cap<U>) {}
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let needs = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "needs")
        })
        .expect("missing associated function");
    assert_eq!(
        place_effect_provider_param_index_map(&db, needs),
        &[Some(1), Some(2), Some(3)]
    );
    let params = CallableDef::Func(needs).params(&db);
    assert_eq!(params.len(), 5);
    assert!(matches!(params[4].data(&db), TyData::TyParam(param) if param.idx == 4));
    db.assert_no_diags(top_mod);
}

#[test]
fn ordinary_calls_use_keyed_trait_effect_witnesses_after_trait_const_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "ordinary_calls_use_keyed_trait_effect_witnesses_after_trait_const_normalization.fe",
        ),
        r#"
trait Cap<T> {}

trait HasRoot {
    const ROOT: u256
}

struct Slot<const ROOT: u256> {}
struct Impl {}
struct Provider {}

impl HasRoot for Impl {
    const ROOT: u256 = 7
}

impl Cap<Slot<7>> for Provider {}

fn needs<T = Impl>() uses (cap: Cap<Slot<T::ROOT>>)
where
    T: HasRoot
{}

fn caller(p: own Provider) {
    with (Cap<Slot<7>> = p) {
        let out: () = needs<Impl>()
        let defaulted: () = needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Provider");
}

#[test]
fn ordinary_calls_use_forwarded_trait_const_effect_params() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("ordinary_calls_use_forwarded_trait_const_effect_params.fe"),
        r#"
trait HasRoot {
    const ROOT: u256
}

trait Cap<T> {}

struct Slot<const ROOT: u256> {}
struct S {}

impl HasRoot for S {
    const ROOT: u256 = 7
}

fn needs() uses (cap: Cap<Slot<S::ROOT>>) {}

fn caller() uses (cap: Cap<Slot<S::ROOT>>) {
    let out: () = needs()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
}

#[test]
fn keyed_with_trait_bindings_accept_schematic_providers_via_keyed_witnesses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "keyed_with_trait_bindings_accept_schematic_providers_via_keyed_witnesses.fe",
        ),
        r#"
trait Logger {
    fn log(self)
}

fn needs_logger() uses (logger: Logger) {}

fn with_logger<L: Logger>(logger: own L) {
    with (Logger = logger) {
        needs_logger()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let with_logger = find_func(&db, top_mod, "with_logger");
    let call_expr = find_call_expr(&db, with_logger);
    db.assert_no_diags(top_mod);
    let typed_body = check_func_body(&db, with_logger).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
}

#[test]
fn keyed_trait_effects_use_provider_type_for_with_bindings() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("keyed_trait_effects_use_provider_type_for_with_bindings.fe"),
        r#"
trait Logger {
    fn log(self)
}

struct Console {}

struct Ptr<T> {
    raw: *T
}

impl<T> Logger for Ptr<T> {
    fn log(self) {}
}

fn needs() uses (logger: Logger) {}

fn caller(p: own Ptr<Console>) {
    with (Logger = p) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Ptr<Console>");
}

#[test]
fn inherent_method_calls_keep_effect_provider_generic_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("inherent_method_calls_keep_effect_provider_generic_slots.fe"),
        r#"
use std::evm::{Address, Ctx, Evm}

struct Token {}

impl Token {
    fn mint(self) -> Address uses (ctx: Ctx) {
        ctx.caller()
    }
}

fn caller(token: Token) -> Address uses (evm: mut Evm) {
    with (Ctx = evm) {
        token.mint()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_method_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    let callable = typed_body
        .callable_expr(call_expr)
        .expect("missing method callable");
    let CallableDef::Func(method) = callable.callable_def() else {
        panic!("expected inherent method callable");
    };
    let provider_idx = place_effect_provider_param_index_map(&db, method)
        .first()
        .copied()
        .flatten()
        .expect("missing provider param slot for method effect");
    assert_eq!(
        callable
            .generic_args()
            .get(provider_idx)
            .expect("method callable missing provider generic arg")
            .pretty_print(&db)
            .to_string(),
        "Evm",
    );
}

#[test]
fn generic_inherent_method_calls_keep_effect_provider_generic_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("generic_inherent_method_calls_keep_effect_provider_generic_slots.fe"),
        r#"
use std::evm::{Address, Ctx, Evm, StorageMap}

struct Token<const BAL: u256, const ALLOW: u256> {
    balances: StorageMap<Address, u256>,
    allowances: StorageMap<(Address, Address), u256>,
}

impl<const BAL: u256, const ALLOW: u256> Token<BAL, ALLOW> {
    fn mint(self) -> Address uses (ctx: Ctx) {
        ctx.caller()
    }
}

fn caller(token: Token<0, 1>) -> Address uses (evm: mut Evm) {
    with (Ctx = evm) {
        token.mint()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_method_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    let callable = typed_body
        .callable_expr(call_expr)
        .expect("missing method callable");
    let CallableDef::Func(method) = callable.callable_def() else {
        panic!("expected inherent method callable");
    };
    let provider_idx = place_effect_provider_param_index_map(&db, method)
        .first()
        .copied()
        .flatten()
        .expect("missing provider param slot for method effect");
    assert_eq!(
        callable
            .generic_args()
            .get(provider_idx)
            .expect("method callable missing provider generic arg")
            .pretty_print(&db)
            .to_string(),
        "Evm",
    );
}

#[test]
fn forwarded_inherent_method_calls_keep_effect_provider_generic_slots() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("forwarded_inherent_method_calls_keep_effect_provider_generic_slots.fe"),
        r#"
use std::evm::{Address, Ctx, StorageMap}

struct Token<const BAL: u256, const ALLOW: u256> {
    balances: StorageMap<Address, u256>,
    allowances: StorageMap<(Address, Address), u256>,
}

impl<const BAL: u256, const ALLOW: u256> Token<BAL, ALLOW> {
    fn mint(self) -> Address uses (ctx: Ctx) {
        ctx.caller()
    }
}

fn caller(token: Token<0, 1>) -> Address uses (ctx: Ctx) {
    token.mint()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_method_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    let callable = typed_body
        .callable_expr(call_expr)
        .expect("missing method callable");
    let CallableDef::Func(method) = callable.callable_def() else {
        panic!("expected inherent method callable");
    };
    let provider_idx = place_effect_provider_param_index_map(&db, method)
        .first()
        .copied()
        .flatten()
        .expect("missing provider param slot for method effect");
    let provider_arg = callable
        .generic_args()
        .get(provider_idx)
        .expect("method callable missing provider generic arg");
    assert_ne!(
        provider_arg.pretty_print(&db).to_string(),
        "Self",
        "forwarded method call should not reuse the impl self slot for its effect provider",
    );
}

#[test]
fn keyed_trait_effects_do_not_accept_target_type_only_matches() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("keyed_trait_effects_do_not_accept_target_type_only_matches.fe"),
        r#"
trait Logger {
    fn log(self)
}

struct Console {}

impl Logger for Console {
    fn log(self) {}
}

fn needs() uses (logger: Logger) {}

fn caller(p: *Console) {
    with (Logger = p) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|diag| diag
            .message
            .contains("keyed effect binding `Logger` requires `*Console` to implement `Logger`",)),
        "expected keyed trait binding failure, got diagnostics: {diags:#?}"
    );
}
#[test]
fn invalid_keyed_with_bindings_shadow_outer_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("invalid_keyed_with_bindings_shadow_outer_providers.fe"),
        r#"
trait Logger {
    fn log(self)
}

struct Good {}
struct Bad {}

impl Logger for Good {
    fn log(self) {}
}

fn needs() uses (logger: Logger) {}

fn caller() {
    with (Logger = Good {}) {
        with (Logger = Bad {}) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0]
            .message
            .contains("keyed effect binding `Logger` requires `Bad` to implement `Logger`"),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "inner invalid keyed binding should shadow the outer provider"
    );
}

#[test]
fn invalid_keyed_with_bindings_shadow_same_frame_unkeyed_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("invalid_keyed_with_bindings_shadow_same_frame_unkeyed_providers.fe"),
        r#"
trait Logger {
    fn log(self)
}

struct Good {}
struct Bad {}

impl Logger for Good {
    fn log(self) {}
}

fn needs() uses (logger: Logger) {}

fn caller() {
    with (Logger = Bad {}, Good {}) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0]
            .message
            .contains("keyed effect binding `Logger` requires `Bad` to implement `Logger`"),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "same-frame invalid keyed binding should shadow unkeyed fallback providers"
    );
}

#[test]
fn keyed_with_bindings_take_precedence_over_same_frame_unkeyed_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "keyed_with_bindings_take_precedence_over_same_frame_unkeyed_providers.fe",
        ),
        r#"
trait Logger {
    fn log(self)
}

struct Keyed {}
struct Unkeyed {}

impl Logger for Keyed {
    fn log(self) {}
}

impl Logger for Unkeyed {
    fn log(self) {}
}

fn needs() uses (logger: Logger) {}

fn caller() {
    with (Logger = Keyed {}, Unkeyed {}) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Keyed");
}

#[test]
fn instantiated_keyed_with_bindings_shadow_outer_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("instantiated_keyed_with_bindings_shadow_outer_providers.fe"),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Good {}
struct Bad {}

impl Cap<u8> for Good {
    fn cap(self) {}
}

fn needs<T>() uses (cap: Cap<T>) {}

fn caller() {
    with (Cap<u8> = Good {}) {
        with (Cap<u8> = Bad {}) {
            needs<u8>()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0]
            .message
            .contains("keyed effect binding `Cap<u8>` requires `Bad` to implement `Cap<u8>`"),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "instantiated invalid keyed binding should shadow the outer provider"
    );
}

#[test]
fn instantiated_keyed_with_bindings_take_precedence_over_same_frame_unkeyed_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "instantiated_keyed_with_bindings_take_precedence_over_same_frame_unkeyed_providers.fe",
        ),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Keyed {}
struct Unkeyed {}

impl Cap<u8> for Keyed {
    fn cap(self) {}
}

impl Cap<u8> for Unkeyed {
    fn cap(self) {}
}

fn needs<T>() uses (cap: Cap<T>) {}

fn caller() {
    with (Cap<u8> = Keyed {}, Unkeyed {}) {
        needs<u8>()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Keyed");
}

#[test]
fn inferred_keyed_with_bindings_shadow_outer_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("inferred_keyed_with_bindings_shadow_outer_providers.fe"),
        r#"
trait Ctx<T> {
    fn ctx(self)
}

struct Good {}
struct Bad {}

impl Ctx<u8> for Good {
    fn ctx(self) {}
}

fn needs<T>(x: T) uses (ctx: Ctx<T>) {}

fn caller() {
    let x: u8 = 1
    with (Ctx<u8> = Good {}) {
        with (Ctx<u8> = Bad {}) {
            needs(x)
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0]
            .message
            .contains("keyed effect binding `Ctx<u8>` requires `Bad` to implement `Ctx<u8>`"),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "inferred instantiated keyed binding should shadow the outer provider"
    );
}

#[test]
fn inferred_type_keyed_with_bindings_shadow_outer_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("inferred_type_keyed_with_bindings_shadow_outer_providers.fe"),
        r#"
struct Storage<T> {
    value: T,
}

fn needs<T>(x: T) uses (store: Storage<T>) {}

fn caller() {
    let x: u8 = 1
    let good = Storage<u8> { value: x }
    let bad = Storage<u16> { value: 2 }
    with (Storage<u8> = good) {
        with (Storage<u8> = bad) {
            needs(x)
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0]
            .message
            .contains("keyed effect binding `Storage<u8>` requires a provider compatible with",),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "inferred instantiated type-keyed binding should shadow the outer provider"
    );
}

#[test]
fn normalized_keyed_with_bindings_take_precedence_after_assoc_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "normalized_keyed_with_bindings_take_precedence_after_assoc_normalization.fe",
        ),
        r#"
trait Cap<T> {
    fn cap(self)
}

trait HasTy {
    type Assoc
}

struct Keyed {}
struct Unkeyed {}

impl Cap<u256> for Keyed {
    fn cap(self) {}
}

impl Cap<u256> for Unkeyed {
    fn cap(self) {}
}

fn needs<T>() uses (cap: Cap<T>) {}

fn caller<X>()
where
    X: HasTy<Assoc = u256>
{
    with (Cap<u256> = Keyed {}, Unkeyed {}) {
        needs<X::Assoc>()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Keyed");
}

#[test]
fn ordinary_calls_use_keyed_type_effects_after_assoc_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("ordinary_calls_use_keyed_type_effects_after_assoc_normalization.fe"),
        r#"
trait HasTy {
    type Assoc
}

struct Storage<T> {
    value: T,
}

fn needs<T>() uses (store: Storage<T>) {}

fn caller<X>()
where
    X: HasTy<Assoc = u256>
{
    let store = Storage<u256> { value: 1 }
    with (Storage<u256> = store) {
        needs<X::Assoc>()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, call_expr);
}

#[test]
fn ordinary_calls_use_keyed_type_effects_after_trait_const_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "ordinary_calls_use_keyed_type_effects_after_trait_const_normalization.fe",
        ),
        r#"
trait HasRoot {
    const ROOT: u256
}

struct Slot<const ROOT: u256> {}
struct Impl {}

impl HasRoot for Impl {
    const ROOT: u256 = 7
}

fn needs<T = Impl>() uses (slot: Slot<T::ROOT>)
where
    T: HasRoot
{}

fn caller() {
    let slot = Slot<7> {}
    with (Slot<7> = slot) {
        let out: () = needs<Impl>()
        let defaulted: () = needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, call_expr);
}

#[test]
fn concrete_type_keyed_with_bindings_do_not_accept_wrong_explicit_keys() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("concrete_type_keyed_with_bindings_do_not_accept_wrong_explicit_keys.fe"),
        r#"
struct Storage<T> {
    value: T,
}

fn needs_u8() uses (store: Storage<u8>) {}

fn caller() {
    let good = Storage<u8> { value: 1 }
    with (Storage<u16> = good) {
        needs_u8()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|diag| {
            diag.message.contains(
                "keyed effect binding `Storage<u16>` requires a provider compatible with `concrete_type_keyed_with_bindings_do_not_accept_wrong_explicit_keys::Storage<u16>`",
            )
        }),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "wrong explicit type key should not satisfy a concrete effect request"
    );
}

#[test]
fn schematic_type_keyed_with_bindings_do_not_accept_wrong_explicit_keys() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "schematic_type_keyed_with_bindings_do_not_accept_wrong_explicit_keys.fe",
        ),
        r#"
struct Storage<T> {
    value: T,
}

fn needs<T>() uses (store: Storage<T>) {}

fn caller() {
    let good = Storage<u8> { value: 1 }
    with (Storage<u16> = good) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags
            .iter()
            .any(|diag| diag.message.contains("keyed effect binding `Storage<u16>`")),
        "expected keyed type binding failure, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "wrong explicit type key should not satisfy a schematic effect request"
    );
}

#[test]
fn schematic_type_keyed_with_bindings_do_not_accept_wrong_explicit_keys_via_provider_wrappers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "schematic_type_keyed_with_bindings_do_not_accept_wrong_explicit_keys_via_provider_wrappers.fe",
        ),
        r#"
struct Storage<T> {
    value: T,
}

fn needs<T>() uses (store: Storage<T>) {}

fn caller(p: *Storage<u8>) {
    with (Storage<u16> = p) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags
            .iter()
            .any(|diag| diag.message.contains("keyed effect binding `Storage<u16>`")),
        "expected keyed wrapper binding failure, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "wrong explicit type key should not satisfy a schematic effect request via wrappers"
    );
}

#[test]
fn schematic_type_keyed_with_bindings_accept_consistent_explicit_keys() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("schematic_type_keyed_with_bindings_accept_consistent_explicit_keys.fe"),
        r#"
struct Storage<T> {
    value: T,
}

fn needs<T>() uses (store: Storage<T>) {}

fn caller() {
    let good = Storage<u8> { value: 1 }
    with (Storage<u8> = good) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, call_expr);
}

#[test]
fn keyed_type_with_bindings_commit_explicit_keys_before_later_inference() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "keyed_type_with_bindings_commit_explicit_keys_before_later_inference.fe",
        ),
        r#"
extern {
    fn todo() -> !
}

struct Storage<T> {
    value: T,
}

fn make<T>() -> Storage<T> { unsafe { todo() } }
fn take(x: ref Storage<u8>) {}
fn needs<T>() uses (store: Storage<T>) {}

fn caller() {
    let x = make()
    with (Storage<u16> = x) {
        take(ref x)
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|diag| {
            diag.message.contains("type mismatch")
                && diag
                    .sub_diagnostics
                    .iter()
                    .any(|sub| sub.message.contains("Storage<u8>"))
                && diag
                    .sub_diagnostics
                    .iter()
                    .any(|sub| sub.message.contains("Storage<u16>"))
        }),
        "expected later inference to conflict with the committed explicit key, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "a later conflicting refinement should not consume the keyed type binding"
    );
}

#[test]
fn unresolved_outer_inference_type_queries_use_family_fallback() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("unresolved_outer_inference_type_queries_use_family_fallback.fe"),
        r#"
extern {
    fn todo() -> !
}

struct Storage<T> {
    value: T,
}

fn make<T>() -> T { unsafe { todo() } }
fn take(_: ref u8) {}
fn needs<T>(_: T) uses (store: Storage<T>) {}

fn caller(good: Storage<u8>, bad: Storage<u32>) {
    let x = make()
    with (Storage<u8> = good) {
        with (Storage<u16> = bad) {
            needs(x)
            take(ref x)
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0]
            .message
            .contains("keyed effect binding `Storage<u16>`"),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let needs_call = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, needs_call);
}

#[test]
fn unresolved_outer_inference_trait_queries_use_family_fallback() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("unresolved_outer_inference_trait_queries_use_family_fallback.fe"),
        r#"
extern {
    fn todo() -> !
}

trait Cap<T> {}

struct Storage<T> {
    value: T,
}

struct Good {}
struct Bad {}

impl Cap<Storage<u8>> for Good {}

fn make<T>() -> T { unsafe { todo() } }
fn take(_: ref u8) {}
fn needs<T>(_: T) uses (cap: Cap<Storage<T>>) {}

fn caller(good: own Good, bad: own Bad) {
    let x = make()
    with (Cap<Storage<u8>> = good) {
        with (Cap<Storage<u16>> = bad) {
            needs(x)
            take(ref x)
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0]
            .message
            .contains("keyed effect binding `Cap<Storage<u16>")
            && diags[0].message.contains("requires `Bad` to implement"),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let needs_call = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, needs_call);
}

#[test]
fn keyed_trait_with_bindings_require_stable_provider_types() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("keyed_trait_with_bindings_require_stable_provider_types.fe"),
        r#"
extern {
    fn todo() -> !
}

trait Logger {
    fn log(self)
}

struct Good {}
struct Bad {}

impl Logger for Good {
    fn log(self) {}
}

fn make<T>() -> T { unsafe { todo() } }
fn take(x: ref Bad) {}
fn needs() uses (logger: Logger) {}

fn caller() {
    let x = make()
    with (Logger = x) {
        take(ref x)
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags
            .iter()
            .any(|diag| diag.message.contains("type annotation is needed")),
        "expected unstable keyed trait binding rejection, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "unstable keyed trait binding should not resolve an effect argument"
    );
}

#[test]
fn unkeyed_trait_effect_candidates_use_folded_provider_types() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("unkeyed_trait_effect_candidates_use_folded_provider_types.fe"),
        r#"
extern {
    fn todo() -> !
}

trait Logger {
    fn log(self)
}

struct Good {}
struct Bad {}

impl Logger for Good {
    fn log(self) {}
}

fn make<T>() -> T { unsafe { todo() } }
fn take(x: ref Bad) {}
fn needs() uses (logger: Logger) {}

fn caller() {
    let x = make()
    unsafe {
        with (x) {
            take(ref x)
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(!diags.is_empty(), "expected diagnostics, got none");

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "unkeyed trait candidates should use the folded provider type and reject `Bad`"
    );
}

#[test]
fn unkeyed_trait_effect_candidates_do_not_accept_ambiguous_solver_results() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "unkeyed_trait_effect_candidates_do_not_accept_ambiguous_solver_results.fe",
        ),
        r#"
extern {
    fn todo() -> !
}

trait Logger {
    fn log(self)
}

struct Good {}
struct AlsoGood {}
struct Bad {}

impl Logger for Good {
    fn log(self) {}
}

impl Logger for AlsoGood {
    fn log(self) {}
}

fn make<T>() -> T { unsafe { todo() } }
fn take(x: ref Bad) {}
fn needs() uses (logger: Logger) {}

fn caller() {
    let x = make()
    unsafe {
        with (x) {
            needs()
            take(ref x)
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(!diags.is_empty(), "expected diagnostics, got none");

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "unkeyed trait candidates should not accept provisional solver results"
    );
}

#[test]
fn unkeyed_trait_effect_candidates_accept_stable_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("unkeyed_trait_effect_candidates_accept_stable_providers.fe"),
        r#"
trait Logger {
    fn log(self)
}

struct Good {}

impl Logger for Good {
    fn log(self) {}
}

fn needs() uses (logger: Logger) {}

fn caller(x: own Good) {
    unsafe {
        with (x) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
}

#[test]
fn unkeyed_trait_effect_candidates_commit_unique_solver_solutions() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("unkeyed_trait_effect_candidates_commit_unique_solver_solutions.fe"),
        r#"
trait Logger<T> {
    fn log(self)
}

struct Good {}

impl Logger<u8> for Good {
    fn log(self) {}
}

fn needs<T>() uses (logger: Logger<T>) {}

fn caller(x: own Good) {
    unsafe {
        with (x) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    let provider_arg_idx = place_effect_provider_param_index_map(&db, needs)
        .first()
        .copied()
        .flatten()
        .expect("missing hidden provider arg index");
    let callable = typed_body
        .callable_expr(call_expr)
        .expect("missing callable for effectful call");
    let inferred_arg_idx = (0..callable.generic_args().len())
        .find(|idx| *idx != provider_arg_idx)
        .expect("missing explicit generic arg");
    assert_callable_generic_arg(&db, caller, call_expr, inferred_arg_idx, "u8");
}

#[test]
fn free_function_calls_check_effect_inferred_constraints() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("free_function_calls_check_effect_inferred_constraints.fe"),
        r#"
trait Logger<T> {
    fn log(self)
}

trait Other {}

struct Good {}

impl Logger<u8> for Good {
    fn log(self) {}
}

fn needs<T>() uses (logger: Logger<T>)
where
    T: Other
{}

fn caller(x: own Good) {
    unsafe {
        with (x) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|diag| {
            diag.message == "trait bound is not satisfied"
                && diag
                    .sub_diagnostics
                    .iter()
                    .any(|sub| sub.message.contains("`u8` doesn't implement `Other`"))
        }),
        "expected unsatisfied inferred bound, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    let provider_arg_idx = place_effect_provider_param_index_map(&db, needs)
        .first()
        .copied()
        .flatten()
        .expect("missing hidden provider arg index");
    let callable = typed_body
        .callable_expr(call_expr)
        .expect("missing callable for effectful call");
    let inferred_arg_idx = (0..callable.generic_args().len())
        .find(|idx| *idx != provider_arg_idx)
        .expect("missing explicit generic arg");
    assert_callable_generic_arg(&db, caller, call_expr, inferred_arg_idx, "u8");
}

#[test]
fn free_function_calls_accept_satisfied_effect_inferred_constraints() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("free_function_calls_accept_satisfied_effect_inferred_constraints.fe"),
        r#"
trait Logger<T> {
    fn log(self)
}

trait Other {}

struct Good {}

impl Other for u8 {}

impl Logger<u8> for Good {
    fn log(self) {}
}

fn needs<T>() uses (logger: Logger<T>)
where
    T: Other
{}

fn caller(x: own Good) {
    unsafe {
        with (x) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    let provider_arg_idx = place_effect_provider_param_index_map(&db, needs)
        .first()
        .copied()
        .flatten()
        .expect("missing hidden provider arg index");
    let callable = typed_body
        .callable_expr(call_expr)
        .expect("missing callable for effectful call");
    let inferred_arg_idx = (0..callable.generic_args().len())
        .find(|idx| *idx != provider_arg_idx)
        .expect("missing explicit generic arg");
    assert_callable_generic_arg(&db, caller, call_expr, inferred_arg_idx, "u8");
}

#[test]
fn keyed_with_trait_bindings_normalize_assoc_requirements() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("keyed_with_trait_bindings_normalize_assoc_requirements.fe"),
        r#"
trait Cap<T> {
    fn cap(self)
}

trait HasSlot {
    type Assoc
}

struct Slot<T> {}
struct Provider {}

impl Cap<Slot<u256>> for Provider {
    fn cap(self) {}
}

fn needs<X>() uses (cap: Cap<X::Assoc>)
where
    X: HasSlot<Assoc = Slot<u256>>
{}

fn caller<X>(p: own Provider)
where
    X: HasSlot<Assoc = Slot<u256>>
{
    with (Cap<X::Assoc> = p) {
        needs<X>()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Provider");
}

#[test]
fn keyed_with_trait_bindings_normalize_trait_const_requirements() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("keyed_with_trait_bindings_normalize_trait_const_requirements.fe"),
        r#"
trait HasRoot {
    const ROOT: u256
}

trait Cap<T> {
    fn cap(self)
}

struct Slot<const ROOT: u256> {}
struct S {}
struct Provider {}

impl HasRoot for S {
    const ROOT: u256 = 7
}

impl Cap<Slot<7>> for Provider {
    fn cap(self) {}
}

fn needs() uses (cap: Cap<Slot<S::ROOT>>) {}

fn caller(p: own Provider) {
    with (Cap<Slot<S::ROOT>> = p) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Provider");
}

#[test]
fn keyed_with_trait_const_bindings_shadow_outer_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("keyed_with_trait_const_bindings_shadow_outer_providers.fe"),
        r#"
trait HasRoot {
    const ROOT: u256
}

trait Cap<T> {
    fn cap(self)
}

struct Slot<const ROOT: u256> {}
struct S {}
struct Good {}
struct Bad {}

impl HasRoot for S {
    const ROOT: u256 = 7
}

impl Cap<Slot<7>> for Good {
    fn cap(self) {}
}

fn needs() uses (cap: Cap<Slot<S::ROOT>>) {}

fn caller() {
    with (Cap<Slot<S::ROOT>> = Good {}) {
        with (Cap<Slot<S::ROOT>> = Bad {}) {
            let out: () = needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0]
            .message
            .contains("requires `Bad` to implement `Cap<Slot<7>>`"),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "trait-const keyed binding should shadow the outer provider"
    );
}

#[test]
fn method_calls_keep_invalid_keyed_trait_bindings_from_reaching_outer_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "method_calls_keep_invalid_keyed_trait_bindings_from_reaching_outer_providers.fe",
        ),
        r#"
trait Logger {
    fn log(self)

    fn needs(self) uses (logger: Logger) {
        logger.log()
    }
}

struct Good {}
struct Bad {}
struct Receiver {}

impl Logger for Good {
    fn log(self) {}
}

impl Logger for Receiver {
    fn log(self) {}
}

fn caller(provider: own Good, recv: own Receiver) {
    with (Logger = provider) {
        with (Logger = Bad {}) {
            recv.needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0]
            .message
            .contains("keyed effect binding `Logger` requires `Bad` to implement `Logger`"),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_method_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "inner invalid keyed binding should shadow the outer provider on method calls"
    );
}

#[test]
fn method_calls_prefer_same_frame_explicit_keyed_trait_bindings() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("method_calls_prefer_same_frame_explicit_keyed_trait_bindings.fe"),
        r#"
trait Logger {
    fn log(self)

    fn needs(self) uses (logger: Logger) {
        logger.log()
    }
}

struct Keyed {}
struct Unkeyed {}
struct Receiver {}

impl Logger for Keyed {
    fn log(self) {}
}

impl Logger for Unkeyed {
    fn log(self) {}
}

impl Logger for Receiver {
    fn log(self) {}
}

fn caller(keyed: own Keyed, recv: own Receiver, unkeyed: own Unkeyed) {
    with (Logger = keyed, unkeyed) {
        recv.needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_method_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_callable_provider_arg(&db, caller, call_expr, "Keyed");
}

#[test]
fn permuted_assoc_binding_order_keeps_exact_keyed_precedence() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("permuted_assoc_binding_order_keeps_exact_keyed_precedence.fe"),
        r#"
trait Cap {
    type A
    type B
    fn cap(self)
}

struct Keyed {}
struct Unkeyed {}

impl Cap for Keyed {
    type A = u8
    type B = u16
    fn cap(self) {}
}

impl Cap for Unkeyed {
    type A = u8
    type B = u16
    fn cap(self) {}
}

fn needs() uses (cap: Cap<A = u8, B = u16>) {}

fn caller() {
    with (Cap<B = u16, A = u8> = Keyed {}, Unkeyed {}) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Keyed");
}

#[test]
fn non_rigid_omitted_type_keyed_with_bindings_are_rejected() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("non_rigid_omitted_type_keyed_with_bindings_are_rejected.fe"),
        r#"
extern {
    fn todo() -> !
}

struct Storage<T> {
    value: T,
}

fn make<T>() -> Storage<T> { unsafe { todo() } }
fn needs() uses (store: Storage<u8>) {}

fn caller() {
    let x = make()
    with (Storage = x) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags
            .iter()
            .any(|diag| diag.message.contains("keyed effect binding `Storage`")),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "non-rigid omitted keyed witness should not satisfy the call"
    );
}

#[test]
fn invalid_omitted_type_key_barrier_shadows_outer_exact_provider() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("invalid_omitted_type_key_barrier_shadows_outer_exact_provider.fe"),
        r#"
struct Storage<T> {
    value: T,
}

fn needs() uses (store: Storage<u8>) {}

fn caller(good: Storage<u8>, bad: Storage<u16>) {
    with (Storage<u8> = good) {
        with (Storage = bad) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags
            .iter()
            .any(|diag| diag.message.contains("keyed effect binding `Storage`")),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "invalid omitted type barrier should shadow the outer exact provider"
    );
}

#[test]
fn invalid_omitted_nested_trait_key_barrier_shadows_outer_exact_provider() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "invalid_omitted_nested_trait_key_barrier_shadows_outer_exact_provider.fe",
        ),
        r#"
trait Cap<T> {}

struct Storage<T> {
    value: T,
}

struct Good {}
struct Bad {}

impl Cap<Storage<u8>> for Good {}

fn needs() uses (cap: Cap<Storage<u8>>) {}

fn caller(good: own Good, bad: own Bad) {
    with (Cap<Storage<u8>> = good) {
        with (Cap<Storage> = bad) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags
            .iter()
            .any(|diag| diag.message.contains("keyed effect binding `Cap<Storage>`")),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "invalid omitted nested trait barrier should shadow the outer exact provider"
    );
}

#[test]
fn invalid_projection_type_key_barrier_falls_back_to_conservative_family_barrier() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "invalid_projection_type_key_barrier_falls_back_to_conservative_family_barrier.fe",
        ),
        r#"
trait HasAssoc {
    type Assoc
}

struct Storage<T> {
    value: T,
}

fn needs() uses (store: Storage<u8>) {}

fn caller<X>(good: Storage<u8>, bad: Storage<u16>)
where
    X: HasAssoc
{
    with (Storage<u8> = good) {
        with (Storage<X::Assoc> = bad) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|diag| diag
            .message
            .contains("keyed effect binding `Storage<X::Assoc>`")),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "non-precise invalid type barriers should fall back to a conservative same-family barrier"
    );
}

#[test]
fn non_precise_invalid_tuple_alias_type_barriers_fall_back_to_conservative_same_family_barriers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "non_precise_invalid_tuple_alias_type_barriers_fall_back_to_conservative_same_family_barriers.fe",
        ),
        r#"
trait HasAssoc {
    type Assoc
}

type Pair<T> = (T, T)

fn needs() uses (slot: Pair<u8>) {}

fn caller<X>(good: Pair<u8>, bad: Pair<u16>)
where
    X: HasAssoc
{
    with (Pair<u8> = good) {
        with (Pair<X::Assoc> = bad) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|diag| diag
            .message
            .contains("keyed effect binding `Pair<X::Assoc>`")),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "non-precise invalid tuple-alias type barriers should conservatively shadow outer providers"
    );
}

#[test]
fn non_precise_invalid_nested_tuple_alias_type_barriers_fall_back_to_conservative_same_family_barriers()
 {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "non_precise_invalid_nested_tuple_alias_type_barriers_fall_back_to_conservative_same_family_barriers.fe",
        ),
        r#"
trait HasAssoc {
    type Assoc
}

struct Leaf<T> {}
type Pair<T> = (Leaf<T>, Leaf<T>)

fn needs() uses (slot: Pair<u8>) {}

fn caller<X>(good: Pair<u8>, bad: Pair<u16>)
where
    X: HasAssoc
{
    with (Pair<u8> = good) {
        with (Pair<X::Assoc> = bad) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|diag| diag
            .message
            .contains("keyed effect binding `Pair<X::Assoc>`")),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "nested non-ADT alias families should also conservatively shadow outer providers"
    );
}

#[test]
fn invalid_projection_trait_key_barrier_falls_back_to_conservative_family_barrier() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "invalid_projection_trait_key_barrier_falls_back_to_conservative_family_barrier.fe",
        ),
        r#"
trait HasAssoc {
    type Assoc
}

trait Cap<T> {}

struct Slot<T> {}
struct Good {}
struct Bad {}

impl Cap<Slot<u8>> for Good {}

fn needs() uses (cap: Cap<Slot<u8>>) {}

fn caller<X>(good: own Good, bad: own Bad)
where
    X: HasAssoc
{
    with (Cap<Slot<u8>> = good) {
        with (Cap<X::Assoc> = bad) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|diag| diag
            .message
            .contains("keyed effect binding `Cap<X::Assoc>`")),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "non-precise invalid trait barriers should fall back to a conservative same-family barrier"
    );
}

#[test]
fn forwarded_assoc_normalized_effect_params_seed_rigid_witnesses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("forwarded_assoc_normalized_effect_params_seed_rigid_witnesses.fe"),
        r#"
trait HasSlot {
    type Assoc
}

trait Cap<T> {}

struct Slot<T> {}

fn needs() uses (cap: Cap<Slot<u8>>) {}

fn caller<X>() uses (cap: Cap<X::Assoc>)
where
    X: HasSlot<Assoc = Slot<u8>>
{
    needs()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
}

#[test]
fn family_fallback_keyed_lookup_requires_an_actual_witness_match() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("family_fallback_keyed_lookup_requires_an_actual_witness_match.fe"),
        r#"
trait Cap<T> {
    fn cap(self)
}

trait HasTy {
    type Assoc
}

struct Good {}
struct Bad {}

impl Cap<u8> for Good {
    fn cap(self) {}
}

impl Cap<u16> for Bad {
    fn cap(self) {}
}

fn needs<X>() uses (cap: Cap<X::Assoc>)
where
    X: HasTy<Assoc = u8>
{}

fn caller<X>()
where
    X: HasTy<Assoc = u8>
{
    with (Cap<u8> = Good {}) {
        with (Cap<u16> = Bad {}) {
            needs<X>()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Good");
}

#[test]
fn family_fallback_considers_same_frame_keyed_and_unkeyed_candidates_together() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "family_fallback_considers_same_frame_keyed_and_unkeyed_candidates_together.fe",
        ),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Keyed {}
struct Unkeyed {}

impl Cap<u8> for Keyed {
    fn cap(self) {}
}

impl Cap<u8> for Unkeyed {
    fn cap(self) {}
}

fn needs<T>() uses (cap: Cap<T>) {}

fn caller(unkeyed: own Unkeyed) {
    with (Cap<u8> = Keyed {}, unkeyed) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags
            .iter()
            .any(|diag| diag.message.contains("multiple effect candidates found")),
        "expected ambiguous same-frame fallback candidates, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "same-frame keyed and unkeyed fallback candidates should not silently pick one"
    );
}

#[test]
fn contract_named_trait_effects_seed_forwarding_witnesses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_named_trait_effects_seed_forwarding_witnesses.fe"),
        r#"
use std::evm::Log

msg Msg {
    #[selector = 1]
    Ping,
}

fn emit() uses (log: mut Log) {}

contract C uses (log: mut Log) {
    recv Msg {
        Ping uses (mut log) {
            emit()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "C");
    let (diags, typed_body) = check_contract_recv_arm_body(&db, contract, 0, 0);
    assert!(
        diags.is_empty(),
        "{}",
        fe_hir::analysis::diagnostics::format_diags(&db, diags.iter())
    );
    let call_expr = find_call_expr_in_typed_body(&db, typed_body);
    assert_single_trait_effect_arg(typed_body, call_expr);
}

#[test]
fn by_ref_trait_provider_storage_bug_fixture_keeps_callables_on_all_typed_calls() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("by_ref_trait_provider_storage_bug.fe"),
        include_str!("../../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    for func in top_mod.all_funcs(&db) {
        let (_, typed_body) = check_func_body(&db, *func);
        if typed_body.body().is_none() {
            continue;
        }
        let name = func
            .name(&db)
            .to_opt()
            .map_or_else(|| "<fn>".to_string(), |name| name.data(&db).to_string());
        assert_all_calls_have_callables(&db, &name, typed_body);
    }

    for impl_trait in top_mod.all_impl_traits(&db) {
        for func in impl_trait.methods(&db) {
            let (_, typed_body) = check_func_body(&db, func);
            let name = func
                .name(&db)
                .to_opt()
                .map_or_else(|| "<fn>".to_string(), |name| name.data(&db).to_string());
            assert_all_calls_have_callables(&db, &format!("impl::{name}"), typed_body);
        }
    }

    for contract in top_mod.all_contracts(&db) {
        let recvs = contract.recvs(&db);
        for (recv_idx, recv) in recvs.data(&db).iter().enumerate() {
            for arm_idx in 0..recv.arms.data(&db).len() {
                let (_, typed_body) =
                    check_contract_recv_arm_body(&db, *contract, recv_idx as u32, arm_idx as u32);
                assert_all_calls_have_callables(
                    &db,
                    &format!("recv[{recv_idx}][{arm_idx}]"),
                    typed_body,
                );
            }
        }
    }
}

#[test]
fn impl_trait_method_typed_body_instantiation_substitutes_self() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("by_ref_trait_provider_storage_bug.fe"),
        include_str!("../../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let impl_trait = top_mod.all_impl_traits(&db)[0];
    let method = impl_trait
        .methods(&db)
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "sum")
        })
        .expect("missing impl-trait method");
    let inst = impl_trait
        .trait_inst(&db)
        .expect("missing impl trait instance");
    let resolved = prove(
        &db,
        TraitSolveCx::new(&db, impl_trait.scope())
            .with_assumptions(param_env(&db, ItemKind::ImplTrait(impl_trait))),
        inst,
    )
    .and_then(|evidence| evidence.answering(&db, inst))
    .and_then(|selected| {
        selected.method(
            &db,
            method.name(&db).to_opt().expect("missing method name"),
            None,
        )
    })
    .expect("missing resolved impl method");
    assert_eq!(resolved.body(), Some(method));

    let instantiated = instantiate_typed_body(
        &db,
        typed_body_template(&db, BodyOwner::Func(method, None)),
        GenericSubst::for_owner(&db, method.into(), resolved.body_args().to_vec()),
    );
    let self_binding = instantiated.param_binding(0).expect("missing self binding");
    let self_ty = instantiated.binding_ty(&db, self_binding);
    assert_eq!(self_ty.pretty_print(&db).to_string(), "Pair");
}

#[test]
fn impl_trait_method_self_paths_preserve_binding_ty_after_instantiation() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("by_ref_trait_provider_storage_bug.fe"),
        include_str!("../../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let impl_trait = top_mod.all_impl_traits(&db)[0];
    let method = impl_trait
        .methods(&db)
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "sum")
        })
        .expect("missing impl-trait method");
    let inst = impl_trait
        .trait_inst(&db)
        .expect("missing impl trait instance");
    let resolved = prove(
        &db,
        TraitSolveCx::new(&db, impl_trait.scope())
            .with_assumptions(param_env(&db, ItemKind::ImplTrait(impl_trait))),
        inst,
    )
    .and_then(|evidence| evidence.answering(&db, inst))
    .and_then(|selected| {
        selected.method(
            &db,
            method.name(&db).to_opt().expect("missing method name"),
            None,
        )
    })
    .expect("missing resolved impl method");

    let instantiated = instantiate_typed_body(
        &db,
        typed_body_template(&db, BodyOwner::Func(method, None)),
        GenericSubst::for_owner(&db, method.into(), resolved.body_args().to_vec()),
    );
    let body = instantiated.body().expect("missing typed body");
    let self_binding = instantiated.param_binding(0).expect("missing self binding");
    let binding_ty = instantiated.binding_ty(&db, self_binding);
    let self_exprs = body
        .exprs(&db)
        .keys()
        .filter(|expr| {
            matches!(expr.data(&db, body), Partial::Present(Expr::Path(_)))
                && instantiated.expr_binding(*expr) == Some(self_binding)
        })
        .collect::<Vec<_>>();
    assert!(!self_exprs.is_empty(), "expected self path expressions");
    for expr in self_exprs {
        assert_eq!(
            instantiated.expr_ty(&db, expr),
            binding_ty,
            "instantiated self path should preserve binding type for {expr:?}",
        );
    }
}

#[test]
fn use_ctx_semantic_body_keeps_receiver_as_effect_binding_local() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("by_ref_trait_provider_storage_bug.fe"),
        include_str!("../../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let use_ctx = find_func(&db, top_mod, "use_ctx");
    let instance = SemanticInstance::new(
        &db,
        fe_hir::analysis::semantic::identity_semantic_instance_key(
            &db,
            BodyOwner::Func(use_ctx, None),
        ),
    );
    let body = instance.body(&db);
    let args = body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            fe_hir::analysis::semantic::SStmtKind::Assign {
                expr: fe_hir::analysis::semantic::SExpr::Call { args, .. },
                ..
            } => Some(args),
            fe_hir::analysis::semantic::SStmtKind::Assign { .. }
            | fe_hir::analysis::semantic::SStmtKind::Store { .. }
            | fe_hir::analysis::semantic::SStmtKind::End { .. }
            | fe_hir::analysis::semantic::SStmtKind::Drop { .. } => None,
        })
        .unwrap_or_else(|| panic!("{body:#?}"));
    assert_eq!(args.len(), 1);
    let receiver = &body.locals[args[0].value.index()];
    assert!(
        matches!(
            receiver.role,
            fe_hir::analysis::semantic::SemanticLocalRole::DirectValue {
                provenance:
                    fe_hir::analysis::semantic::ValueProvenance::RootProvider(ref provider),
            } if provider.semantics.kind
                == fe_hir::analysis::ty::provider::ProviderKind::RootObject
        ),
        "{body:#?}"
    );
}

#[test]
fn zero_effect_arg_method_calls_finalize_under_forwarded_witnesses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("by_ref_trait_provider_storage_bug.fe"),
        include_str!("../../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let use_ctx = find_func(&db, top_mod, "use_ctx");
    let method_call = find_method_call_expr(&db, use_ctx);
    let typed_body = check_func_body(&db, use_ctx).1.clone();
    assert!(
        typed_body.call_effect_args(method_call).is_none(),
        "ctx.sum() should remain a zero-effect-arg call"
    );
    let callable = typed_body
        .callable_expr(method_call)
        .expect("missing callable for ctx.sum()");
    let CallableDef::Func(typed_target) = callable.callable_def() else {
        panic!("ctx.sum() should lower to a function call");
    };
    assert!(
        typed_target.containing_trait(&db).is_some(),
        "typed ctx.sum() should still target the trait method declaration"
    );

    let impl_trait = top_mod
        .all_impl_traits(&db)
        .iter()
        .copied()
        .find(|impl_trait| {
            impl_trait.methods(&db).any(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "sum")
            })
        })
        .expect("missing impl Ctx for Pair");
    let expected_sum = impl_trait
        .methods(&db)
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "sum")
        })
        .expect("missing impl sum method");

    let contract = find_contract(&db, top_mod, "ByRefTraitProviderStorageBug");
    let recv = SemanticInstance::new(
        &db,
        fe_hir::analysis::semantic::identity_semantic_instance_key(
            &db,
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx: 0,
                arm_idx: 0,
            },
        ),
    );
    let use_ctx = recv
        .callees(&db)
        .iter()
        .map(|callee| SemanticInstance::new(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func, _)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "use_ctx")
            )
        })
        .expect("missing instantiated use_ctx callee");
    let sum = use_ctx
        .callees(&db)
        .iter()
        .map(|callee| SemanticInstance::new(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func, _)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "sum")
            )
        })
        .expect("missing semantic sum callee");
    let BodyOwner::Func(sum_owner, _) = sum.key(&db).owner(&db) else {
        panic!("sum callee should be a function");
    };
    assert_eq!(
        sum_owner, expected_sum,
        "zero-effect-arg method call should resolve under final forwarded witnesses"
    );
    assert!(
        sum_owner.containing_impl_trait(&db).is_some(),
        "semantic sum callee should be the impl method, not the trait declaration"
    );
}

#[test]
fn impl_sum_semantic_body_uses_self_binding_directly() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("by_ref_trait_provider_storage_bug.fe"),
        include_str!("../../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let impl_trait = top_mod.all_impl_traits(&db)[0];
    let method = impl_trait
        .methods(&db)
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "sum")
        })
        .expect("missing impl-trait method");
    let inst = impl_trait
        .trait_inst(&db)
        .expect("missing impl trait instance");
    let resolved = prove(
        &db,
        TraitSolveCx::new(&db, impl_trait.scope())
            .with_assumptions(param_env(&db, ItemKind::ImplTrait(impl_trait))),
        inst,
    )
    .and_then(|evidence| evidence.answering(&db, inst))
    .and_then(|selected| {
        selected.method(
            &db,
            method.name(&db).to_opt().expect("missing method name"),
            None,
        )
    })
    .expect("missing resolved impl method");
    let instance = SemanticInstance::new(
        &db,
        fe_hir::analysis::semantic::SemanticInstanceKey::new(
            &db,
            BodyOwner::Func(method, None),
            GenericSubst::for_owner(&db, method.into(), resolved.body_args().to_vec()),
            fe_hir::analysis::semantic::EffectProviderSubst::empty(&db),
            fe_hir::analysis::semantic::ImplEnv::empty(&db, impl_trait.scope()),
            EvidenceList::none(&db),
        ),
    );
    let body = instance.body(&db);
    assert!(
        body.blocks
            .iter()
            .flat_map(|block| block.stmts.iter())
            .all(|stmt| match &stmt.kind {
                fe_hir::analysis::semantic::SStmtKind::Assign {
                    expr:
                        fe_hir::analysis::semantic::SExpr::Forward(local)
                        | fe_hir::analysis::semantic::SExpr::UseValue(local),
                    ..
                } => local.value != fe_hir::analysis::semantic::SLocalId::from_u32(0),
                fe_hir::analysis::semantic::SStmtKind::Assign { .. }
                | fe_hir::analysis::semantic::SStmtKind::Store { .. }
                | fe_hir::analysis::semantic::SStmtKind::End { .. }
                | fe_hir::analysis::semantic::SStmtKind::Drop { .. } => true,
            }),
        "{body:#?}"
    );
}

#[test]
fn pointer_values_are_direct_values() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("effect_handle_field_deref.fe"),
        include_str!("../../../codegen/tests/fixtures/effect_handle_field_deref.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "EffectHandleFieldDeref");
    let instance = SemanticInstance::new(
        &db,
        fe_hir::analysis::semantic::identity_semantic_instance_key(
            &db,
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx: 0,
                arm_idx: 1,
            },
        ),
    );
    let body = instance.body(&db);
    // The arm's first call is `extract_ptr(holder)`.
    let callee = body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            fe_hir::analysis::semantic::SStmtKind::Assign {
                expr: fe_hir::analysis::semantic::SExpr::Call { callee, .. },
                ..
            } => Some(callee.key),
            _ => None,
        })
        .unwrap_or_else(|| panic!("{body:#?}"));
    let callee = SemanticInstance::new(&db, callee);
    let callee_body = callee.body(&db);
    assert!(
        callee_body
            .blocks
            .iter()
            .flat_map(|block| block.stmts.iter())
            .all(|stmt| !matches!(
                &stmt.kind,
                fe_hir::analysis::semantic::SStmtKind::Assign {
                    expr: fe_hir::analysis::semantic::SExpr::Call { .. },
                    ..
                }
            )),
        "returning a pointer field calls nothing: {callee_body:#?}"
    );
    let carrier_local = callee_body
        .blocks
        .iter()
        .find_map(|block| match &block.terminator.kind {
            fe_hir::analysis::semantic::STerminatorKind::Return(Some(value)) => Some(value.value),
            fe_hir::analysis::semantic::STerminatorKind::Goto(_)
            | fe_hir::analysis::semantic::STerminatorKind::Branch { .. }
            | fe_hir::analysis::semantic::STerminatorKind::MatchEnum { .. }
            | fe_hir::analysis::semantic::STerminatorKind::Assert { .. }
            | fe_hir::analysis::semantic::STerminatorKind::Return(None)
            | fe_hir::analysis::semantic::STerminatorKind::Yield { .. } => None,
        })
        .unwrap_or_else(|| panic!("expected returned pointer value: {callee_body:#?}"));
    // A pointer is data, not a carrier of the place it addresses.
    assert!(
        matches!(
            callee_body.locals[carrier_local.index()].role,
            fe_hir::analysis::semantic::SemanticLocalRole::DirectValue { .. }
        ),
        "{callee_body:#?}"
    );
}

#[test]
fn function_effect_bindings_use_instantiated_provider_bindings() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("effect_handle_field_deref.fe"),
        include_str!("../../../codegen/tests/fixtures/effect_handle_field_deref.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "EffectHandleFieldDeref");
    let instance = SemanticInstance::new(
        &db,
        fe_hir::analysis::semantic::identity_semantic_instance_key(
            &db,
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx: 0,
                arm_idx: 0,
            },
        ),
    );
    let body = instance.body(&db);
    let callee = body.blocks[0]
        .stmts
        .iter()
        .find_map(|stmt| {
            let fe_hir::analysis::semantic::SStmtKind::Assign {
                expr: fe_hir::analysis::semantic::SExpr::Call { callee, .. },
                ..
            } = &stmt.kind
            else {
                return None;
            };
            let instance = SemanticInstance::new(&db, callee.key);
            match instance.key(&db).owner(&db) {
                BodyOwner::Func(func, _)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "write_cell") =>
                {
                    Some(instance)
                }
                BodyOwner::Func(_, _)
                | BodyOwner::Const(_)
                | BodyOwner::AnonConstBody { .. }
                | BodyOwner::ContractInit { .. }
                | BodyOwner::ContractRecvArm { .. }
                | BodyOwner::Closure { .. } => None,
            }
        })
        .unwrap_or_else(|| panic!("expected nested write_cell call in helper body: {body:#?}"));
    let callee_body = callee.body(&db);
    let effect_local = callee_body
        .locals
        .iter()
        .find(|local| {
            matches!(
                local.source,
                Some(fe_hir::analysis::ty::ty_check::LocalBinding::EffectParam { .. })
            )
        })
        .expect("write_cell should have an effect binding local");
    let fe_hir::analysis::semantic::SemanticLocalRole::PlaceBoundValue {
        provenance: fe_hir::analysis::semantic::PlaceProvenance::RootProvider(local_provider),
        value_ty,
    } = &effect_local.role
    else {
        panic!(
            "write_cell effect binding should be place-bound through its instantiated provider:\n{callee_body:#?}"
        );
    };
    // The provider is the dereferenced storage place itself.
    assert_eq!(
        local_provider.provider_ty.pretty_print(&db).to_string(),
        "Cell"
    );
    assert_eq!(
        local_provider.semantics.address_space,
        Some(fe_hir::analysis::ty::AddressSpace::Storage)
    );
    assert_eq!(local_provider.semantics.target_ty, Some(*value_ty));
    assert_eq!(value_ty.pretty_print(&db).to_string(), "Cell");
    let effect_binding = effect_local
        .source
        .expect("effect local should keep its source binding");
    assert_eq!(
        callee
            .binding_ty(&db, effect_binding)
            .pretty_print(&db)
            .to_string(),
        "Cell"
    );
    let env = instantiated_effect_env(&db, callee).expect("effect env should exist");
    let provider = env
        .providers(&db)
        .iter()
        .find(|provider| provider.provider_ty == local_provider.provider_ty)
        .expect("instantiated provider binding should exist");
    assert_eq!(provider.provider_ty.pretty_print(&db).to_string(), "Cell");
}

#[test]
fn root_effect_bindings_specialize_raw_storage_to_evm() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("explicit_raw_boundaries.fe"),
        include_str!("../../../codegen/tests/fixtures/explicit_raw_boundaries.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let entry = find_func(&db, top_mod, "explicit_raw_boundaries");
    let root = SemanticInstance::new(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(entry, None))
            .expect("root semantic instance should exist"),
    );
    let root_effect_binding = root
        .body(&db)
        .locals
        .iter()
        .find_map(|local| match local.source {
            Some(
                binding @ fe_hir::analysis::ty::ty_check::LocalBinding::EffectParam {
                    binding_name,
                    ..
                },
            ) if binding_name.data(&db) == "storage" => Some(binding),
            Some(_) | None => None,
        })
        .expect("expected root storage effect binding");
    assert_eq!(
        root.binding_ty(&db, root_effect_binding)
            .pretty_print(&db)
            .to_string(),
        "Evm"
    );

    let raw_store = root
        .body(&db)
        .callees()
        .into_iter()
        .map(|callee| SemanticInstance::new(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func, _)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "raw_store")
            )
        })
        .expect("expected specialized raw_store callee");
    let raw_store_effect_binding = raw_store
        .body(&db)
        .locals
        .iter()
        .find_map(|local| match local.source {
            Some(
                binding @ fe_hir::analysis::ty::ty_check::LocalBinding::EffectParam {
                    binding_name,
                    ..
                },
            ) if binding_name.data(&db) == "storage" => Some(binding),
            Some(_) | None => None,
        })
        .expect("expected specialized raw_store storage effect binding");
    assert_eq!(
        raw_store
            .binding_ty(&db, raw_store_effect_binding)
            .pretty_print(&db)
            .to_string(),
        "Evm"
    );

    let sstore = raw_store
        .body(&db)
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| {
            let fe_hir::analysis::semantic::SStmtKind::Assign {
                expr: fe_hir::analysis::semantic::SExpr::Call { callee, .. },
                ..
            } = &stmt.kind
            else {
                return None;
            };
            let instance = SemanticInstance::new(&db, callee.key);
            matches!(
                instance.key(&db).owner(&db),
                BodyOwner::Func(func, _)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "sstore")
                        && func.body(&db).is_some()
            )
            .then_some(instance)
        })
        .expect("expected concrete sstore impl call");
    assert!(matches!(
        sstore.key(&db).owner(&db),
        BodyOwner::Func(func, _) if func.body(&db).is_some()
    ));
}

#[test]
fn root_trait_effect_bindings_specialize_call_to_concrete_evm() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("address_call_method.fe"),
        include_str!("../../../fe/tests/fixtures/fe_test/address_call_method.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let entry = find_func(&db, top_mod, "test_address_call_method");
    let root = SemanticInstance::new(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(entry, None))
            .expect("root semantic instance should exist"),
    );
    let address_call = root
        .callees(&db)
        .iter()
        .map(|callee| SemanticInstance::new(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func, _)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "call")
                        && func.containing_trait(&db).is_none()
            )
        })
        .expect("expected specialized Address::call callee");
    let env = instantiated_effect_env(&db, address_call).expect("call effect env should exist");
    let call_provider_idx = env
        .requirements(&db)
        .iter()
        .find(|requirement| requirement.binding_name.data(&db) == "call")
        .and_then(|requirement| {
            env.resolutions(&db)
                .iter()
                .find(|resolution| resolution.requirement_idx == requirement.binding_idx)
        })
        .map(|resolution| resolution.provider_idx)
        .expect("expected resolved call effect requirement");
    let call_provider = env
        .providers(&db)
        .iter()
        .find(|provider| provider.provider_idx == call_provider_idx)
        .expect("expected specialized call provider binding");
    assert_eq!(call_provider.provider_ty.pretty_print(&db).clone(), "Evm");
    assert_eq!(
        call_provider
            .semantics
            .target_ty
            .expect("call provider target should be specialized")
            .pretty_print(&db)
            .to_string(),
        "Evm"
    );
}

#[test]
fn with_bound_plain_effect_params_specialize_to_memory_capabilities() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("effect_params.fe"),
        include_str!("../../../fe/tests/fixtures/fe_test/effect_params.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "test_non_evm_effect_param");
    let instance = SemanticInstance::new(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(caller, None))
            .expect("caller root instance should exist"),
    );
    let callee = instance
        .body(&db)
        .callees()
        .into_iter()
        .map(|callee| SemanticInstance::new(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func, _)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "bump_non_evm_effect_param")
            )
        })
        .expect("expected specialized bump_non_evm_effect_param callee");
    let env = instantiated_effect_env(&db, callee).expect("callee effect env should exist");
    let value_provider_idx = env
        .requirements(&db)
        .iter()
        .find(|requirement| requirement.binding_name.data(&db) == "value")
        .and_then(|requirement| {
            env.resolutions(&db)
                .iter()
                .find(|resolution| resolution.requirement_idx == requirement.binding_idx)
        })
        .map(|resolution| resolution.provider_idx)
        .expect("expected resolved value effect requirement");
    let value_provider = env
        .providers(&db)
        .iter()
        .find(|provider| provider.provider_idx == value_provider_idx)
        .expect("expected specialized value provider binding");
    assert_eq!(value_provider.provider_ty.pretty_print(&db).clone(), "u256");
    assert!(value_provider.is_mut);
    assert_eq!(
        value_provider
            .semantics
            .target_ty
            .expect("plain effect param provider target should be specialized")
            .pretty_print(&db)
            .to_string(),
        "u256"
    );
}

#[test]
fn free_function_effect_bindings_keep_explicit_memory_address_space() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("mutable_array_args_and_effects.fe"),
        include_str!("../../../fe/tests/fixtures/fe_test/mutable_array_args_and_effects.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "test_mut_array_effect_param");
    let instance = SemanticInstance::new(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(caller, None))
            .expect("caller root instance should exist"),
    );
    let write_effects_and_sum = instance
        .body(&db)
        .callees()
        .into_iter()
        .map(|callee| SemanticInstance::new(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func, _)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "write_effects_and_sum")
            )
        })
        .expect("expected write_effects_and_sum callee");
    let callee = write_effects_and_sum
        .body(&db)
        .callees()
        .into_iter()
        .map(|callee| SemanticInstance::new(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func, _)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "write_effect")
            )
        })
        .expect("expected write_effect callee");
    let env = instantiated_effect_env(&db, callee).expect("write_effect effect env should exist");
    let words_provider_idx = env
        .resolutions(&db)
        .iter()
        .find(|resolution| resolution.requirement_idx == 0)
        .expect("write_effect should resolve its first effect binding")
        .provider_idx;
    let provider = env
        .providers(&db)
        .iter()
        .find(|provider| provider.provider_idx == words_provider_idx)
        .expect("write_effect resolved provider should exist");
    assert_eq!(
        provider.semantics.address_space,
        Some(fe_hir::analysis::ty::AddressSpace::Memory),
        "free-function effect providers should keep explicit memory address spaces: {:#?}",
        env.providers(&db),
    );
}

#[test]
fn pointer_derefs_provide_their_pointer_spaces() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("local_effect_handle_provider_spaces.fe"),
        r#"
use core::ptr
use std::evm::{RawStorage, StorPtr}

struct Foo {
    value: u256,
}

fn bump_mem() uses (foo: mut Foo) {
    foo.value += 1
}

fn bump_stor() uses (foo: mut Foo) {
    foo.value += 1
}

fn test_spaces() uses (st: mut RawStorage) {
    let mp = ptr::alloc<Foo>()
    unsafe {
        with (mut *mp) {
            bump_mem()
        }
    }

    let sp = 0 as u256 as StorPtr<Foo>
    unsafe {
        with (mut *sp) {
            bump_stor()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "test_spaces");
    let typed_body = check_func_body(&db, func).1.clone();
    let mem_call = find_named_call_expr(&db, func, "bump_mem");
    let stor_call = find_named_call_expr(&db, func, "bump_stor");

    let mem_args = typed_body
        .call_effect_args(mem_call)
        .expect("bump_mem should have a resolved effect arg");
    assert_eq!(mem_args.len(), 1);
    assert_eq!(mem_args[0].pass_mode, EffectPassMode::ByPlace);
    assert_eq!(
        mem_args[0].provider,
        Some(fe_hir::analysis::ty::AddressSpace::Memory)
    );

    let stor_args = typed_body
        .call_effect_args(stor_call)
        .expect("bump_stor should have a resolved effect arg");
    assert_eq!(stor_args.len(), 1);
    assert_eq!(stor_args[0].pass_mode, EffectPassMode::ByPlace);
    assert_eq!(
        stor_args[0].provider,
        Some(fe_hir::analysis::ty::AddressSpace::Storage)
    );
}

#[test]
fn derefs_of_projected_pointers_provide_the_pointer_space() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("projected_handle_provider_spaces.fe"),
        r#"
use std::evm::{Evm, StorPtr}

struct Cell {
    value: u256,
}

struct Holder {
    ptr: StorPtr<Cell>,
}

fn write_cell(_ value: u256) uses (cell: mut Cell) {
    cell.value = value
}

fn test_projected(slot: u256, value: u256) uses (evm: mut Evm) {
    let holder = Holder { ptr: slot as StorPtr<Cell> }
    unsafe {
        with (mut *holder.ptr) {
            write_cell(value)
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "test_projected");
    let typed_body = check_func_body(&db, func).1.clone();
    let call_expr = find_named_call_expr(&db, func, "write_cell");
    let effect_args = typed_body
        .call_effect_args(call_expr)
        .expect("write_cell should have a resolved effect arg");
    assert_eq!(effect_args.len(), 1);
    assert_eq!(effect_args[0].pass_mode, EffectPassMode::ByPlace);
    assert_eq!(
        effect_args[0].provider,
        Some(fe_hir::analysis::ty::AddressSpace::Storage)
    );
}

#[test]
fn root_semantic_instance_keys_are_closed_under_root_provider_subst() {
    let mut db = HirAnalysisTestDb::default();
    let storage_map_file = db.new_stand_alone(
        Utf8PathBuf::from("storage_map_contract.fe"),
        include_str!("../../../codegen/tests/fixtures/storage_map_contract.fe"),
    );
    let (top_mod, _) = db.top_mod(storage_map_file);
    db.assert_no_diags(top_mod);
    for name in ["init", "runtime"] {
        let func = find_func(&db, top_mod, name);
        let key = root_semantic_instance_key(&db, BodyOwner::Func(func, None))
            .unwrap_or_else(|err| panic!("manual root `{name}` should synthesize a closed root semantic instance key: {err:?}"));
        validate_instantiated_effect_env_key(&db, key).unwrap_or_else(|err| {
            panic!(
                "manual root `{name}` should have a closed effect env under synthesized root subst: {err:?}"
            )
        });
    }

    let test_file = db.new_stand_alone(
        Utf8PathBuf::from("contract_init_fixed_array_arg.fe"),
        include_str!("../../../fe/tests/fixtures/fe_test/contract_init_fixed_array_arg.fe"),
    );
    let (top_mod, _) = db.top_mod(test_file);
    db.assert_no_diags(top_mod);
    let test_func = find_func(&db, top_mod, "test_contract_init_fixed_array_arg");
    let key =
        root_semantic_instance_key(&db, BodyOwner::Func(test_func, None)).unwrap_or_else(|err| {
            panic!("test root should synthesize a closed root semantic instance key: {err:?}")
        });
    validate_instantiated_effect_env_key(&db, key).unwrap_or_else(|err| {
        panic!("test root should have a closed effect env under synthesized root subst: {err:?}")
    });
}
