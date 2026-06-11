use camino::Utf8PathBuf;
use common::diagnostics::{CompleteDiagnostic, cmp_complete_diagnostics};
use cranelift_entity::EntityRef;
use fe_hir::analysis::diagnostics::DiagnosticVoucher;
use fe_hir::analysis::place::PlaceBase;
use fe_hir::analysis::semantic::{
    GenericSubst, get_or_build_semantic_instance, instantiate_typed_body, instantiated_effect_env,
    resolved_provider_binding_for_instance_effect, root_semantic_instance_key, typed_body_template,
    validate_instantiated_effect_env_key,
};
use fe_hir::analysis::ty::effects::{EffectKeyKind, place_effect_provider_param_index_map};
use fe_hir::analysis::ty::trait_def::resolve_trait_method_instance;
use fe_hir::analysis::ty::trait_resolution::TraitSolveCx;
use fe_hir::analysis::ty::ty_check::{
    BodyOwner, EffectArg, EffectPassMode, TypedBody, check_contract_recv_arm_body, check_func_body,
};
use fe_hir::hir_def::{CallableDef, Contract, Expr, ExprId, Func, ItemKind, Partial, TopLevelMod};
use fe_hir::test_db::{HirAnalysisTestDb, initialize_analysis_pass};

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

fn assert_effect_arg_pass_mode<'db>(
    typed_body: &TypedBody<'db>,
    call_expr: ExprId,
    expected: EffectPassMode,
) {
    let effect_args = typed_body
        .call_effect_args(call_expr)
        .expect("missing resolved effect args");
    assert_eq!(effect_args.len(), 1);
    assert_eq!(effect_args[0].pass_mode, expected);
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

fn assert_effect_arg_uses_param_binding<'db>(
    typed_body: &TypedBody<'db>,
    call_expr: ExprId,
    expected_binding: fe_hir::analysis::ty::ty_check::LocalBinding<'db>,
) {
    let effect_args = typed_body
        .call_effect_args(call_expr)
        .expect("missing resolved effect args");
    assert_eq!(effect_args.len(), 1);
    match &effect_args[0].arg {
        EffectArg::Place(place) => {
            assert_eq!(place.base, PlaceBase::Binding(expected_binding));
            assert!(place.projections.is_empty());
        }
        other => panic!("expected place effect arg, got {other:?}"),
    }
}

fn diagnostics_for<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<CompleteDiagnostic> {
    let mut manager = initialize_analysis_pass();
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

struct Slot<T, const ROOT: u256 = _> {}
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
fn ordinary_calls_use_keyed_trait_effect_witnesses_with_layout_holes() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("ordinary_calls_use_keyed_trait_effect_witnesses_with_layout_holes.fe"),
        r#"
trait Cap<T> {}

struct Slot<const ROOT: u256 = _> {}

fn needs(_: u256) uses (cap: Cap<Slot>) {}

fn caller() uses (cap: Cap<Slot>) {
    let out: () = needs(x: 1)
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

struct Slot<T, const ROOT: u256 = _> {}

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
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
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

fn needs<T>() uses (cap: Cap<Slot<T::ROOT>>)
where
    T: HasRoot
{}

fn caller(p: own Provider) {
    with (Cap<Slot<7>> = p) {
        let out: () = needs<Impl>()
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

struct Slot<const ROOT: u256 = _> {}
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
use core::effect_ref::{EffectHandle, EffectRef}

trait Logger {
    fn log(self)
}

struct Console {}

struct Ptr<T> {
    raw: u256
}

impl<T> Logger for Ptr<T> {
    fn log(self) {}
}

impl<T> EffectHandle for Ptr<T> {
    type Target = T

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl<T> EffectRef<T> for Ptr<T> {}

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
use core::effect_ref::{EffectHandle, EffectRef}

trait Logger {
    fn log(self)
}

struct Console {}

impl Logger for Console {
    fn log(self) {}
}

struct Ptr<T> {
    raw: u256
}

impl<T> EffectHandle for Ptr<T> {
    type Target = T

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl<T> EffectRef<T> for Ptr<T> {}

fn needs() uses (logger: Logger) {}

fn caller(p: Ptr<Console>) {
    with (Logger = p) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|diag| diag.message.contains(
            "keyed effect binding `Logger` requires `Ptr<Console>` to implement `Logger`",
        )),
        "expected keyed trait binding failure, got diagnostics: {diags:#?}"
    );
}

#[test]
fn keyed_trait_effects_do_not_accept_capability_wrapped_targets() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("keyed_trait_effects_do_not_accept_capability_wrapped_targets.fe"),
        r#"
trait Logger {
    fn log(self)
}

struct Console {}

impl Logger for Console {
    fn log(self) {}
}

fn needs() uses (logger: Logger) {}

fn caller(c: ref Console) {
    with (Logger = c) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|diag| {
            diag.message.contains(
                "keyed effect binding `Logger` requires `ref Console` to implement `Logger`",
            )
        }),
        "expected capability-wrapper trait binding failure, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "invalid ref provider should not resolve an effect argument"
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

fn needs<T>() uses (slot: Slot<T::ROOT>)
where
    T: HasRoot
{}

fn caller() {
    let slot = Slot<7> {}
    with (Slot<7> = slot) {
        let out: () = needs<Impl>()
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
fn layout_hole_type_keyed_with_bindings_shadow_outer_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("layout_hole_type_keyed_with_bindings_shadow_outer_providers.fe"),
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Other<const ROOT: u256 = _> {}

fn needs() uses (slot: Slot) {}

fn caller(good: Slot<1>, bad: Other<1>) {
    with (Slot = good) {
        with (Slot = bad) {
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
        diags[0].message.contains("Other<1>"),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "invalid layout-hole keyed type binding should shadow the outer provider"
    );
}

#[test]
fn layout_hole_type_keyed_with_bindings_reject_repeated_placeholder_mismatches() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "layout_hole_type_keyed_with_bindings_reject_repeated_placeholder_mismatches.fe",
        ),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

fn caller() {
    let bad = (Leaf<1> {}, Leaf<2> {})
    with (Repeated = bad) {
        let keep: u256 = 0
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags
            .iter()
            .any(|diag| diag.message.contains("keyed effect binding `Repeated`")),
        "expected repeated-placeholder mismatch to be rejected, got diagnostics: {diags:#?}"
    );
}

#[test]
fn layout_hole_type_identity_matching_preserves_repeated_placeholder_equality() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "layout_hole_type_identity_matching_preserves_repeated_placeholder_equality.fe",
        ),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

fn needs() uses (slot: Repeated) {}

fn caller(good: Repeated<1>, bad: Mixed<1, 2>) {
    with (Repeated = good) {
        with (Mixed<1, 2> = bad) {
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
    assert_single_type_effect_arg(&typed_body, call_expr);
}

#[test]
fn poisoned_layout_hole_type_keys_do_not_shadow_outer_exact_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("poisoned_layout_hole_type_keys_do_not_shadow_outer_exact_providers.fe"),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

fn needs() uses (slot: Mixed<1, 2>) {}

fn caller(good: Mixed<1, 2>, bad: Mixed<1, 2>) {
    with (Mixed<1, 2> = good) {
        with (Repeated = bad) {
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
            .any(|diag| diag.message.contains("keyed effect binding `Repeated`")),
        "expected invalid inner repeated key to be diagnosed, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, call_expr);
    assert_effect_arg_uses_param_binding(
        &typed_body,
        call_expr,
        typed_body
            .param_binding(0)
            .expect("missing outer provider binding"),
    );
}

#[test]
fn layout_hole_trait_identity_matching_preserves_repeated_placeholder_equality() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "layout_hole_trait_identity_matching_preserves_repeated_placeholder_equality.fe",
        ),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

struct Good {}
struct Bad {}

impl Cap<Repeated<1>> for Good {
    fn cap(self) {}
}

impl Cap<Mixed<1, 2>> for Bad {
    fn cap(self) {}
}

fn needs() uses (cap: Cap<Repeated>) {}

fn caller(good: own Good, bad: own Bad) {
    with (Cap<Repeated> = good) {
        with (Cap<Mixed<1, 2>> = bad) {
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
fn poisoned_layout_hole_trait_keys_do_not_shadow_outer_exact_witnesses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("poisoned_layout_hole_trait_keys_do_not_shadow_outer_exact_witnesses.fe"),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

struct Good {}
struct Bad {}

impl Cap<Mixed<1, 2>> for Good {
    fn cap(self) {}
}

impl Cap<Mixed<1, 2>> for Bad {
    fn cap(self) {}
}

fn needs() uses (cap: Cap<Mixed<1, 2>>) {}

fn caller(good: own Good, bad: own Bad) {
    with (Cap<Mixed<1, 2>> = good) {
        with (Cap<Repeated> = bad) {
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
            .contains("keyed effect binding `Cap<Repeated>`")),
        "expected invalid inner repeated trait key to be diagnosed, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let needs = find_func(&db, top_mod, "needs");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Good");
}

#[test]
fn omitted_layout_hole_type_keys_store_specialized_with_bindings() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("omitted_layout_hole_type_keys_store_specialized_with_bindings.fe"),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

fn needs() uses (slot: Repeated<1>) {}

fn caller(good: Repeated<1>, bad: Mixed<1, 2>) {
    with (Repeated<1> = good) {
        with (Mixed = bad) {
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
    assert_single_type_effect_arg(&typed_body, call_expr);
    assert_effect_arg_uses_param_binding(
        &typed_body,
        call_expr,
        typed_body
            .param_binding(0)
            .expect("missing outer provider binding"),
    );
}

#[test]
fn omitted_layout_hole_trait_keys_store_specialized_with_witnesses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("omitted_layout_hole_trait_keys_store_specialized_with_witnesses.fe"),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

struct Good {}
struct Bad {}

impl Cap<Repeated<1>> for Good {
    fn cap(self) {}
}

impl Cap<Mixed<1, 2>> for Bad {
    fn cap(self) {}
}

fn needs() uses (cap: Cap<Repeated<1>>) {}

fn caller(good: own Good, bad: own Bad) {
    with (Cap<Repeated<1>> = good) {
        with (Cap<Mixed> = bad) {
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
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Good");
}

#[test]
fn omitted_layout_hole_type_keys_with_hidden_provider_params_are_rejected() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "omitted_layout_hole_type_keys_with_hidden_provider_params_are_rejected.fe",
        ),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

fn needs() uses (slot: Repeated<1>) {}

fn caller(good: Repeated<1>, bad: Mixed) {
    with (Repeated<1> = good) {
        with (Mixed = bad) {
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
            .any(|diag| diag.message.contains("keyed effect binding `Mixed`")),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "a non-rigid hidden-provider keyed binding should not satisfy the call",
    );
}

#[test]
fn omitted_layout_hole_trait_keys_keep_hidden_provider_params_rigid() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("omitted_layout_hole_trait_keys_keep_hidden_provider_params_rigid.fe"),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

struct Good {}
struct Bad<const A: u256 = _, const B: u256 = _> {}

impl Cap<Repeated<1>> for Good {
    fn cap(self) {}
}

impl<const A: u256, const B: u256> Cap<Mixed<A, B>> for Bad<A, B> {
    fn cap(self) {}
}

fn needs() uses (cap: Cap<Repeated<1>>) {}

fn caller(good: own Good, bad: own Bad) {
    with (Cap<Repeated<1>> = good) {
        with (Cap<Mixed> = bad) {
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
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Good");
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
use core::effect_ref::{EffectHandle, EffectRef}

struct Storage<T> {
    value: T,
}

struct Ptr<T> {
    raw: u256
}

impl<T> EffectHandle for Ptr<T> {
    type Target = T

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl<T> EffectRef<T> for Ptr<T> {}

fn needs<T>() uses (store: Storage<T>) {}

fn caller(p: own Ptr<Storage<u8>>) {
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

fn make<T>() -> Storage<T> { todo() }
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
                    .any(|sub| sub.message.contains("ref Storage<u8>"))
                && diag
                    .sub_diagnostics
                    .iter()
                    .any(|sub| sub.message.contains("ref Storage<u16>"))
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

fn make<T>() -> T { todo() }
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

fn make<T>() -> T { todo() }
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
fn unkeyed_wrapper_type_effect_candidates_commit_specialized_wrapper_solutions() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "unkeyed_wrapper_type_effect_candidates_commit_specialized_wrapper_solutions.fe",
        ),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

extern {
    fn todo() -> !
}

struct Storage<T> {
    value: T,
}

struct Ptr<T> {
    raw: u256
}

impl EffectHandle for Ptr<Storage<u8>> {
    type Target = Storage<u8>

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl EffectRef<Storage<u8>> for Ptr<Storage<u8>> {}

fn make<T>() -> Ptr<T> { todo() }
fn take(x: ref Ptr<Storage<u16>>) {}
fn needs() uses (store: Storage<u8>) {}

fn caller() {
    let x = make()
    with (x) {
        needs()
        take(ref x)
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
                    .any(|sub| sub.message.contains("ref Ptr<Storage<u16>>"))
                && diag
                    .sub_diagnostics
                    .iter()
                    .any(|sub| sub.message.contains("ref Ptr<Storage<u8>>"))
        }),
        "expected later refinement to conflict with the committed wrapper proof, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let needs_call = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, needs_call);
}

#[test]
fn keyed_wrapper_type_effect_candidates_commit_specialized_wrapper_solutions() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "keyed_wrapper_type_effect_candidates_commit_specialized_wrapper_solutions.fe",
        ),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

extern {
    fn todo() -> !
}

struct Storage<T> {
    value: T,
}

struct Ptr<T> {
    raw: u256
}

impl EffectHandle for Ptr<Storage<u8>> {
    type Target = Storage<u8>

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl EffectRef<Storage<u8>> for Ptr<Storage<u8>> {}

fn make<T>() -> Ptr<T> { todo() }
fn take(x: ref Ptr<Storage<u16>>) {}
fn needs() uses (store: Storage<u8>) {}

fn caller() {
    let x = make()
    with (Storage<u8> = x) {
        needs()
        take(ref x)
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
                    .any(|sub| sub.message.contains("ref Ptr<Storage<u16>>"))
                && diag
                    .sub_diagnostics
                    .iter()
                    .any(|sub| sub.message.contains("ref Ptr<Storage<u8>>"))
        }),
        "expected later refinement to conflict with the committed keyed wrapper proof, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let needs_call = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, needs_call);
}

#[test]
fn wrapper_type_effect_candidates_accept_stable_specialized_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("wrapper_type_effect_candidates_accept_stable_specialized_providers.fe"),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

struct Storage<T> {
    value: T,
}

struct Ptr<T> {
    raw: u256
}

impl EffectHandle for Ptr<Storage<u8>> {
    type Target = Storage<u8>

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl EffectRef<Storage<u8>> for Ptr<Storage<u8>> {}

fn needs() uses (store: Storage<u8>) {}

fn caller(x: own Ptr<Storage<u8>>) {
    with (x) {
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
fn unkeyed_wrapper_type_effects_renormalize_projection_targets_after_later_proofs() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "unkeyed_wrapper_type_effects_renormalize_projection_targets_after_later_proofs.fe",
        ),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

extern {
    fn todo() -> !
}

trait HasTy {
    type Assoc
}

struct Storage<T> {
    value: T,
}

struct Wrap<T> {}
struct Ptr<T> {
    raw: u256
}
struct Impl {}

impl HasTy for Impl {
    type Assoc = u8
}

impl<T> EffectHandle for Ptr<Wrap<T>>
where
    T: HasTy
{
    type Target = Storage<T::Assoc>

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl EffectRef<Storage<u8>> for Ptr<Wrap<Impl>> {}

fn make<T>() -> Ptr<Wrap<T>>
where
    T: HasTy
{
    todo()
}
fn needs() uses (store: Storage<u8>) {}

fn caller() {
    let x = make()
    with (x) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, call_expr);
}

#[test]
fn keyed_wrapper_type_effects_renormalize_projection_targets_after_later_proofs() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "keyed_wrapper_type_effects_renormalize_projection_targets_after_later_proofs.fe",
        ),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

extern {
    fn todo() -> !
}

trait HasTy {
    type Assoc
}

struct Storage<T> {
    value: T,
}

struct Wrap<T> {}
struct Ptr<T> {
    raw: u256
}
struct Impl {}

impl HasTy for Impl {
    type Assoc = u8
}

impl<T> EffectHandle for Ptr<Wrap<T>>
where
    T: HasTy
{
    type Target = Storage<T::Assoc>

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl EffectRef<Storage<u8>> for Ptr<Wrap<Impl>> {}

fn make<T>() -> Ptr<Wrap<T>>
where
    T: HasTy
{
    todo()
}
fn needs() uses (store: Storage<u8>) {}

fn caller() {
    let x = make()
    with (Storage<u8> = x) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, call_expr);
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

fn make<T>() -> T { todo() }
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

fn make<T>() -> T { todo() }
fn take(x: ref Bad) {}
fn needs() uses (logger: Logger) {}

fn caller() {
    let x = make()
    with (x) {
        take(ref x)
        needs()
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

fn make<T>() -> T { todo() }
fn take(x: ref Bad) {}
fn needs() uses (logger: Logger) {}

fn caller() {
    let x = make()
    with (x) {
        needs()
        take(ref x)
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
    with (x) {
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
    with (x) {
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
    with (x) {
        needs()
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
    with (x) {
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
fn layout_hole_type_keyed_with_bindings_take_precedence_over_same_frame_unkeyed_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "layout_hole_type_keyed_with_bindings_take_precedence_over_same_frame_unkeyed_providers.fe",
        ),
        r#"
struct Slot<const ROOT: u256 = _> {}

fn needs() uses (slot: Slot) {}

fn caller(keyed: Slot<1>, unkeyed: Slot<2>) {
    with (Slot = keyed, unkeyed) {
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
    assert_effect_arg_uses_param_binding(
        &typed_body,
        call_expr,
        typed_body
            .param_binding(0)
            .expect("missing keyed param binding"),
    );
}

#[test]
fn concrete_layout_hole_type_keyed_bindings_shadow_outer_exact_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "concrete_layout_hole_type_keyed_bindings_shadow_outer_exact_providers.fe",
        ),
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Other<const ROOT: u256 = _> {}

fn needs_exact() uses (slot: Slot<u256>) {}

fn caller(bad: Other<1>) uses (slot: Slot<u256>) {
    with (Slot = bad) {
        needs_exact()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0].message.contains("Other<1>"),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "inner identity-only keyed type binding should shadow the outer exact effect param"
    );
}

#[test]
fn concrete_layout_hole_type_keyed_bindings_prefer_inner_identity_matches() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "concrete_layout_hole_type_keyed_bindings_prefer_inner_identity_matches.fe",
        ),
        r#"
struct Slot<const ROOT: u256 = _> {}

fn needs_exact() uses (slot: Slot<u256>) {}

fn caller(inner: Slot<1>) uses (slot: Slot<u256>) {
    with (Slot = inner) {
        needs_exact()
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
    assert_effect_arg_uses_param_binding(
        &typed_body,
        call_expr,
        typed_body
            .param_binding(0)
            .expect("missing explicit inner param binding"),
    );
}

#[test]
fn keyed_with_trait_bindings_normalize_layout_holes() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("keyed_with_trait_bindings_normalize_layout_holes.fe"),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Slot<const ROOT: u256 = _> {}
struct Provider {}

impl Cap<Slot<u256>> for Provider {
    fn cap(self) {}
}

fn needs() uses (cap: Cap<Slot>) {}

fn caller(p: own Provider) {
    with (Cap<Slot> = p) {
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
fn concrete_layout_hole_trait_keyed_bindings_shadow_outer_exact_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "concrete_layout_hole_trait_keyed_bindings_shadow_outer_exact_providers.fe",
        ),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Slot<const ROOT: u256 = _> {}
struct Good {}
struct Bad {}

impl Cap<Slot<u256>> for Good {
    fn cap(self) {}
}

fn needs_exact() uses (cap: Cap<Slot<u256>>) {}

fn caller() uses (cap: Cap<Slot<u256>>) {
    with (Cap<Slot> = Bad {}) {
        needs_exact()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(diags.len(), 1, "unexpected diagnostics: {diags:#?}");
    assert!(
        diags[0].message.contains(
            "keyed effect binding `Cap<Slot>` requires `Bad` to implement `Cap<Slot<_>>`"
        ),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "inner identity-only keyed trait binding should shadow the outer exact effect param"
    );
}

#[test]
fn layout_hole_trait_keyed_with_bindings_shadow_outer_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("layout_hole_trait_keyed_with_bindings_shadow_outer_providers.fe"),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Slot<const ROOT: u256 = _> {}
struct Good {}
struct Bad {}

impl Cap<Slot<u256>> for Good {
    fn cap(self) {}
}

fn needs() uses (cap: Cap<Slot>) {}

fn caller() {
    with (Cap<Slot> = Good {}) {
        with (Cap<Slot> = Bad {}) {
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
        diags[0].message.contains(
            "keyed effect binding `Cap<Slot>` requires `Bad` to implement `Cap<Slot<_>>`"
        ),
        "unexpected diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "invalid layout-hole keyed trait binding should shadow the outer provider"
    );
}

#[test]
fn concrete_layout_hole_trait_keyed_bindings_prefer_inner_identity_matches() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "concrete_layout_hole_trait_keyed_bindings_prefer_inner_identity_matches.fe",
        ),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Slot<const ROOT: u256 = _> {}
struct Inner {}

impl Cap<Slot<u256>> for Inner {
    fn cap(self) {}
}

fn needs_exact() uses (cap: Cap<Slot<u256>>) {}

fn caller(inner: own Inner) uses (cap: Cap<Slot<u256>>) {
    with (Cap<Slot> = inner) {
        needs_exact()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs_exact = find_func(&db, top_mod, "needs_exact");
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs_exact, call_expr, "Inner");
}

#[test]
fn layout_hole_trait_keyed_with_bindings_take_precedence_over_same_frame_unkeyed_providers() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "layout_hole_trait_keyed_with_bindings_take_precedence_over_same_frame_unkeyed_providers.fe",
        ),
        r#"
trait Cap<T> {
    fn cap(self)
}

struct Slot<const ROOT: u256 = _> {}
struct Keyed {}
struct Unkeyed {}

impl Cap<Slot<u256>> for Keyed {
    fn cap(self) {}
}

impl Cap<Slot<u256>> for Unkeyed {
    fn cap(self) {}
}

fn needs() uses (cap: Cap<Slot>) {}

fn caller() {
    with (Cap<Slot> = Keyed {}, Unkeyed {}) {
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

struct Slot<T, const ROOT: u256 = _> {}
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

struct Slot<const ROOT: u256 = _> {}
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

struct Slot<const ROOT: u256 = _> {}
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

fn make<T>() -> Storage<T> { todo() }
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
fn keyed_wrapper_type_witnesses_preserve_by_value_transport() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("keyed_wrapper_type_witnesses_preserve_by_value_transport.fe"),
        r#"
use core::effect_ref::{EffectHandle, EffectRef}

struct Storage<T> {
    value: T,
}

struct Ptr<T> {
    raw: u256
}

impl EffectHandle for Ptr<Storage<u8>> {
    type Target = Storage<u8>

    const SPACE: core::effect_ref::AddressSpace = core::effect_ref::AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl EffectRef<Storage<u8>> for Ptr<Storage<u8>> {}

fn needs() uses (store: Storage<u8>) {}

fn caller(x: own Ptr<Storage<u8>>) {
    with (Storage<u8> = x) {
        needs()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_named_call_expr(&db, caller, "needs");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, call_expr);
    assert_effect_arg_pass_mode(&typed_body, call_expr, EffectPassMode::ByValue);
    assert_callable_provider_arg(&db, caller, call_expr, "Ptr<Storage<u8>>");
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
fn contract_named_effects_with_omitted_layout_hole_keys_seed_specialized_witnesses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "contract_named_effects_with_omitted_layout_hole_keys_seed_specialized_witnesses.fe",
        ),
        r#"
struct Slot<const ROOT: u256 = _> {}

fn needs() uses (slot: Slot) {}

msg Msg {
    #[selector = 1]
    Ping,
}

contract C uses (slot: Slot) {
    recv Msg {
        Ping uses (slot) {
            needs()
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
    assert_single_type_effect_arg(typed_body, call_expr);
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
fn function_type_effect_params_forward_repeated_layout_hole_aliases() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("function_type_effect_params_forward_repeated_layout_hole_aliases.fe"),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

fn needs() uses (slot: Repeated<1>) {}

fn caller() uses (slot: Repeated) {
    needs()
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
fn conflicting_function_type_effect_forwarding_does_not_accept_both_calls() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "conflicting_function_type_effect_forwarding_does_not_accept_both_calls.fe",
        ),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

fn needs1() uses (slot: Repeated<1>) {}
fn needs2() uses (slot: Repeated<2>) {}

fn caller() uses (slot: Repeated) {
    needs1()
    needs2()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags
            .iter()
            .any(|diag| diag.message.contains("missing effect")),
        "expected missing effect diagnostic, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let needs1_call = find_named_call_expr(&db, caller, "needs1");
    let needs2_call = find_named_call_expr(&db, caller, "needs2");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(needs1_call).is_none()
            || typed_body.call_effect_args(needs2_call).is_none(),
        "the same forwarded type effect must not satisfy conflicting specializations",
    );
}

#[test]
fn repeated_function_type_effect_forwarding_reuses_the_same_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "repeated_function_type_effect_forwarding_reuses_the_same_specialization.fe",
        ),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

fn needs1() uses (slot: Repeated<1>) {}
fn needs2() uses (slot: Repeated<1>) {}

fn caller() uses (slot: Repeated) {
    needs1()
    needs2()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs1_call = find_named_call_expr(&db, caller, "needs1");
    let needs2_call = find_named_call_expr(&db, caller, "needs2");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_type_effect_arg(&typed_body, needs1_call);
    assert_single_type_effect_arg(&typed_body, needs2_call);
}

#[test]
fn function_type_effect_params_can_specialize_distinct_hidden_layout_params_once() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "function_type_effect_params_can_specialize_distinct_hidden_layout_params_once.fe",
        ),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

fn needs() uses (slot: Repeated<1>) {}

fn caller() uses (slot: Mixed) {
    needs()
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
fn function_trait_effect_params_forward_repeated_layout_hole_aliases() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("function_trait_effect_params_forward_repeated_layout_hole_aliases.fe"),
        r#"
trait Cap<T> {}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

fn needs() uses (cap: Cap<Repeated<1>>) {}

fn caller() uses (cap: Cap<Repeated>) {
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
fn conflicting_function_trait_effect_forwarding_does_not_accept_both_calls() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "conflicting_function_trait_effect_forwarding_does_not_accept_both_calls.fe",
        ),
        r#"
trait Cap<T> {}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

fn needs1() uses (cap: Cap<Repeated<1>>) {}
fn needs2() uses (cap: Cap<Repeated<2>>) {}

fn caller() uses (cap: Cap<Repeated>) {
    needs1()
    needs2()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags
            .iter()
            .any(|diag| diag.message.contains("missing effect")),
        "expected missing effect diagnostic, got diagnostics: {diags:#?}"
    );

    let caller = find_func(&db, top_mod, "caller");
    let needs1_call = find_named_call_expr(&db, caller, "needs1");
    let needs2_call = find_named_call_expr(&db, caller, "needs2");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert!(
        typed_body.call_effect_args(needs1_call).is_none()
            || typed_body.call_effect_args(needs2_call).is_none(),
        "the same forwarded trait effect must not satisfy conflicting specializations",
    );
}

#[test]
fn repeated_function_trait_effect_forwarding_reuses_the_same_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "repeated_function_trait_effect_forwarding_reuses_the_same_specialization.fe",
        ),
        r#"
trait Cap<T> {}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

fn needs1() uses (cap: Cap<Repeated<1>>) {}
fn needs2() uses (cap: Cap<Repeated<1>>) {}

fn caller() uses (cap: Cap<Repeated>) {
    needs1()
    needs2()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let needs1_call = find_named_call_expr(&db, caller, "needs1");
    let needs2_call = find_named_call_expr(&db, caller, "needs2");
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, needs1_call);
    assert_single_trait_effect_arg(&typed_body, needs2_call);
}

#[test]
fn function_trait_effect_params_can_specialize_distinct_hidden_layout_params_once() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "function_trait_effect_params_can_specialize_distinct_hidden_layout_params_once.fe",
        ),
        r#"
trait Cap<T> {}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

fn needs() uses (cap: Cap<Repeated<1>>) {}

fn caller() uses (cap: Cap<Mixed>) {
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
fn contract_named_type_effects_forward_repeated_layout_hole_aliases() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_named_type_effects_forward_repeated_layout_hole_aliases.fe"),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

fn needs() uses (slot: Repeated<1>) {}

msg Msg {
    #[selector = 1]
    Ping,
}

contract C uses (slot: Repeated) {
    recv Msg {
        Ping uses (slot) {
            needs()
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
    assert_single_type_effect_arg(typed_body, call_expr);
}

#[test]
fn contract_named_type_effects_do_not_overmatch_rigid_hidden_layout_params() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "contract_named_type_effects_do_not_overmatch_rigid_hidden_layout_params.fe",
        ),
        r#"
struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)
type Mixed<const A: u256 = _, const B: u256 = _> = (Leaf<A>, Leaf<B>)

fn needs() uses (slot: Repeated<1>) {}

msg Msg {
    #[selector = 1]
    Ping,
}

contract C uses (slot: Mixed) {
    recv Msg {
        Ping uses (slot) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contract = find_contract(&db, top_mod, "C");
    let (diags, typed_body) = check_contract_recv_arm_body(&db, contract, 0, 0);
    let complete_diags = diags
        .iter()
        .map(|diag| diag.to_complete(&db))
        .collect::<Vec<_>>();
    assert!(
        complete_diags
            .iter()
            .any(|diag| diag.message.contains("missing effect")),
        "expected missing effect diagnostic, got diagnostics: {complete_diags:#?}"
    );

    let call_expr = find_call_expr_in_typed_body(&db, typed_body);
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "contract-named forwarding witnesses must keep hidden params rigid"
    );
}

#[test]
fn contract_named_trait_effects_forward_repeated_layout_hole_aliases() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_named_trait_effects_forward_repeated_layout_hole_aliases.fe"),
        r#"
use std::evm::Evm

trait Cap<T> {}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

impl<const ROOT: u256> Cap<Repeated<ROOT>> for Evm {}

fn needs() uses (cap: Cap<Repeated<1>>) {}

msg Msg {
    #[selector = 1]
    Ping,
}

contract C uses (cap: Cap<Repeated>) {
    recv Msg {
        Ping uses (cap) {
            needs()
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
fn contract_named_trait_effects_specialize_matching_known_root_provider() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "contract_named_trait_effects_specialize_matching_known_root_provider.fe",
        ),
        r#"
use std::evm::Evm

trait Cap<T> {}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

impl Cap<Repeated<2>> for Evm {}

fn needs() uses (cap: Cap<Repeated<2>>) {}

msg Msg {
    #[selector = 1]
    Ping,
}

contract C uses (cap: Cap<Repeated>) {
    recv Msg {
        Ping uses (cap) {
            needs()
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
fn contract_named_trait_effects_specialize_to_known_root_provider() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("contract_named_trait_effects_specialize_to_known_root_provider.fe"),
        r#"
use std::evm::Evm

trait Cap<T> {}

struct Leaf<const ROOT: u256> {}
type Repeated<const ROOT: u256 = _> = (Leaf<ROOT>, Leaf<ROOT>)

impl Cap<Repeated<2>> for Evm {}

fn needs() uses (cap: Cap<Repeated<1>>) {}

msg Msg {
    #[selector = 1]
    Ping,
}

contract C uses (cap: Cap<Repeated>) {
    recv Msg {
        Ping uses (cap) {
            needs()
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let contract = find_contract(&db, top_mod, "C");
    let (diags, typed_body) = check_contract_recv_arm_body(&db, contract, 0, 0);
    assert!(
        !diags.is_empty(),
        "specific root-provider impl should not satisfy a different repeated key"
    );
    let call_expr = find_call_expr_in_typed_body(&db, typed_body);
    assert!(
        typed_body.call_effect_args(call_expr).is_none(),
        "contract-root seeding should specialize the trait witness to the known provider"
    );
}

#[test]
fn storage_map_effect_forwarding_keeps_concrete_hidden_layout_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("storage_map_effect_forwarding_keeps_concrete_hidden_layout_args.fe"),
        r#"
use std::evm::StorageMap

fn needs(_ addr: u256) -> u256
    uses (balances: StorageMap<u256, u256>)
{
    balances.get(key: addr)
}

fn caller() {
    let mut balances = StorageMap<u256, u256, 0>::new()
    with (balances) {
        let _ = needs(1)
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let call_expr = find_named_call_expr(&db, caller, "needs");
    assert_callable_generic_arg(&db, caller, call_expr, 0, "0");
    assert_callable_provider_arg(&db, caller, call_expr, "StorageMap<u256, u256, 0>");
}

#[test]
fn nested_storage_map_effect_forwarding_keeps_concrete_hidden_layout_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "nested_storage_map_effect_forwarding_keeps_concrete_hidden_layout_args.fe",
        ),
        r#"
use std::evm::StorageMap

fn needs(_ addr: u256) -> u256
    uses (balances: StorageMap<u256, u256>)
{
    balances.get(key: addr)
}

fn nested(_ addr: u256) -> u256
    uses (balances: StorageMap<u256, u256>)
{
    needs(addr)
}

fn caller() {
    let mut balances = StorageMap<u256, u256, 0>::new()
    with (balances) {
        let _ = nested(1)
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let nested_call = find_named_call_expr(&db, caller, "nested");
    assert_callable_generic_arg(&db, caller, nested_call, 0, "0");
    assert_callable_provider_arg(&db, caller, nested_call, "StorageMap<u256, u256, 0>");

    let nested = find_func(&db, top_mod, "nested");
    let root = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(caller))
            .expect("caller should have a root semantic instance"),
    );
    let nested_instance = root
        .callees(&db)
        .iter()
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| callee.key(&db).owner(&db) == BodyOwner::Func(nested))
        .expect("missing instantiated nested callee");
    assert_eq!(
        nested_instance.key(&db).subst(&db).generic_args(&db)[0]
            .pretty_print(&db)
            .to_string(),
        "0"
    );
    let env = instantiated_effect_env(&db, nested_instance).expect("missing nested effect env");
    assert_eq!(
        env.providers(&db)[0]
            .provider_ty
            .pretty_print(&db)
            .to_string(),
        "StorageMap<u256, u256, 0>"
    );

    let needs = find_func(&db, top_mod, "needs");
    let needs_instance = nested_instance
        .callees(&db)
        .iter()
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| callee.key(&db).owner(&db) == BodyOwner::Func(needs))
        .expect("missing instantiated needs callee");
    assert_eq!(
        needs_instance.key(&db).subst(&db).generic_args(&db)[0]
            .pretty_print(&db)
            .to_string(),
        "0"
    );
}

#[test]
fn projected_storage_map_effect_forwarding_keeps_concrete_hidden_layout_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "projected_storage_map_effect_forwarding_keeps_concrete_hidden_layout_args.fe",
        ),
        r#"
use std::evm::StorageMap

fn needs(_ addr: u256) -> u256
    uses (balances: StorageMap<u256, u256>)
{
    balances.get(key: addr)
}

fn nested(_ addr: u256) -> u256
    uses (balances: StorageMap<u256, u256>)
{
    needs(addr)
}

struct Store {
    balances: StorageMap<u256, u256>,
}

msg Msg {
    #[selector = 1]
    Get { addr: u256 } -> u256,
}

pub contract Test {
    store: Store,

    recv Msg {
        Get { addr } -> u256 uses (store) {
            with (store.balances) {
                nested(addr)
            }
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "Test");
    let nested = find_func(&db, top_mod, "nested");
    let recv = get_or_build_semantic_instance(
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
    let nested_instance = recv
        .callees(&db)
        .iter()
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| callee.key(&db).owner(&db) == BodyOwner::Func(nested))
        .expect("missing instantiated nested callee");
    assert_eq!(
        nested_instance.key(&db).subst(&db).generic_args(&db)[0]
            .pretty_print(&db)
            .to_string(),
        "0"
    );

    let needs = find_func(&db, top_mod, "needs");
    let needs_instance = nested_instance
        .callees(&db)
        .iter()
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| callee.key(&db).owner(&db) == BodyOwner::Func(needs))
        .expect("missing instantiated needs callee");
    assert_eq!(
        needs_instance.key(&db).subst(&db).generic_args(&db)[0]
            .pretty_print(&db)
            .to_string(),
        "0"
    );
}

#[test]
fn by_ref_trait_provider_storage_bug_fixture_keeps_callables_on_all_typed_calls() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("by_ref_trait_provider_storage_bug.fe"),
        include_str!("../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
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
            .map(|name| name.data(&db).to_string())
            .unwrap_or_else(|| "<fn>".to_string());
        assert_all_calls_have_callables(&db, &name, typed_body);
    }

    for impl_trait in top_mod.all_impl_traits(&db) {
        for func in impl_trait.methods(&db) {
            let (_, typed_body) = check_func_body(&db, func);
            let name = func
                .name(&db)
                .to_opt()
                .map(|name| name.data(&db).to_string())
                .unwrap_or_else(|| "<fn>".to_string());
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
        include_str!("../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
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
    let (resolved_method, impl_args) = resolve_trait_method_instance(
        &db,
        TraitSolveCx::new(&db, impl_trait.scope()),
        inst,
        method.name(&db).to_opt().expect("missing method name"),
    )
    .expect("missing resolved impl method");
    assert_eq!(resolved_method, method);

    let instantiated = instantiate_typed_body(
        &db,
        typed_body_template(&db, BodyOwner::Func(method)),
        GenericSubst::new(&db, impl_args),
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
        include_str!("../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
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
    let (_, impl_args) = resolve_trait_method_instance(
        &db,
        TraitSolveCx::new(&db, impl_trait.scope()),
        inst,
        method.name(&db).to_opt().expect("missing method name"),
    )
    .expect("missing resolved impl method");

    let instantiated = instantiate_typed_body(
        &db,
        typed_body_template(&db, BodyOwner::Func(method)),
        GenericSubst::new(&db, impl_args),
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
        include_str!("../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let use_ctx = find_func(&db, top_mod, "use_ctx");
    let instance = get_or_build_semantic_instance(
        &db,
        fe_hir::analysis::semantic::identity_semantic_instance_key(&db, BodyOwner::Func(use_ctx)),
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
            | fe_hir::analysis::semantic::SStmtKind::Store { .. } => None,
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
        include_str!("../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
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
    let recv = get_or_build_semantic_instance(
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
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func)
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
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "sum")
            )
        })
        .expect("missing semantic sum callee");
    let BodyOwner::Func(sum_owner) = sum.key(&db).owner(&db) else {
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
        include_str!("../../codegen/tests/fixtures/by_ref_trait_provider_storage_bug.fe"),
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
    let (_, impl_args) = resolve_trait_method_instance(
        &db,
        TraitSolveCx::new(&db, impl_trait.scope()),
        inst,
        method.name(&db).to_opt().expect("missing method name"),
    )
    .expect("missing resolved impl method");
    let instance = get_or_build_semantic_instance(
        &db,
        fe_hir::analysis::semantic::SemanticInstanceKey::new(
            &db,
            BodyOwner::Func(method),
            GenericSubst::new(&db, impl_args),
            fe_hir::analysis::semantic::EffectProviderSubst::empty(&db),
            fe_hir::analysis::semantic::ImplEnv::empty(&db, impl_trait.scope()),
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
                | fe_hir::analysis::semantic::SStmtKind::Store { .. } => true,
            }),
        "{body:#?}"
    );
}

#[test]
fn effect_handle_constructors_preserve_direct_carrier_roles() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("effect_handle_field_deref.fe"),
        include_str!("../../codegen/tests/fixtures/effect_handle_field_deref.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "EffectHandleFieldDeref");
    let instance = get_or_build_semantic_instance(
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
    let fe_hir::analysis::semantic::SStmtKind::Assign {
        expr: fe_hir::analysis::semantic::SExpr::Call { callee, .. },
        ..
    } = &body.blocks[0].stmts[0].kind
    else {
        panic!("{body:#?}");
    };
    let callee = get_or_build_semantic_instance(&db, callee.key);
    let callee_body = callee.body(&db);
    let carrier_local = callee_body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            fe_hir::analysis::semantic::SStmtKind::Assign {
                dst,
                expr: fe_hir::analysis::semantic::SExpr::Call { .. },
            } => Some(*dst),
            fe_hir::analysis::semantic::SStmtKind::Assign { .. }
            | fe_hir::analysis::semantic::SStmtKind::Store { .. } => None,
        })
        .unwrap_or_else(|| panic!("expected helper call in lowered body: {callee_body:#?}"));
    assert!(
        matches!(
            callee_body.locals[carrier_local.index()].role,
            fe_hir::analysis::semantic::SemanticLocalRole::DirectCarrier { .. }
        ),
        "{callee_body:#?}"
    );
}

#[test]
fn function_effect_bindings_use_instantiated_provider_bindings() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("effect_handle_field_deref.fe"),
        include_str!("../../codegen/tests/fixtures/effect_handle_field_deref.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let contract = find_contract(&db, top_mod, "EffectHandleFieldDeref");
    let instance = get_or_build_semantic_instance(
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
            let instance = get_or_build_semantic_instance(&db, callee.key);
            match instance.key(&db).owner(&db) {
                BodyOwner::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "write_cell") =>
                {
                    Some(instance)
                }
                BodyOwner::Func(_)
                | BodyOwner::Const(_)
                | BodyOwner::AnonConstBody { .. }
                | BodyOwner::ContractInit { .. }
                | BodyOwner::ContractRecvArm { .. } => None,
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
    assert_eq!(
        local_provider.provider_ty.pretty_print(&db).to_string(),
        "StorPtr<Cell>"
    );
    assert_eq!(
        local_provider.semantics.kind,
        fe_hir::analysis::ty::ProviderKind::Handle
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
    assert_eq!(
        provider.provider_ty.pretty_print(&db).to_string(),
        "StorPtr<Cell>"
    );
}

#[test]
fn root_effect_bindings_specialize_rawmem_to_evm() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("explicit_raw_boundaries.fe"),
        include_str!("../../codegen/tests/fixtures/explicit_raw_boundaries.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let entry = find_func(&db, top_mod, "explicit_raw_boundaries");
    let root = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(entry))
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
            ) if binding_name.data(&db) == "mem" => Some(binding),
            Some(_) | None => None,
        })
        .expect("expected root mem effect binding");
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
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func)
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
            ) if binding_name.data(&db) == "mem" => Some(binding),
            Some(_) | None => None,
        })
        .expect("expected specialized raw_store mem effect binding");
    assert_eq!(
        raw_store
            .binding_ty(&db, raw_store_effect_binding)
            .pretty_print(&db)
            .to_string(),
        "Evm"
    );

    let mstore = raw_store
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
            let instance = get_or_build_semantic_instance(&db, callee.key);
            matches!(
                instance.key(&db).owner(&db),
                BodyOwner::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "mstore")
                        && func.body(&db).is_some()
            )
            .then_some(instance)
        })
        .expect("expected concrete mstore impl call");
    assert!(matches!(
        mstore.key(&db).owner(&db),
        BodyOwner::Func(func) if func.body(&db).is_some()
    ));
}

#[test]
fn root_trait_effect_bindings_specialize_call_to_concrete_evm() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("address_call_method.fe"),
        include_str!("../../fe/tests/fixtures/fe_test/address_call_method.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let entry = find_func(&db, top_mod, "test_address_call_method");
    let root = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(entry))
            .expect("root semantic instance should exist"),
    );
    let address_call = root
        .callees(&db)
        .iter()
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func)
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
    assert_eq!(
        call_provider.provider_ty.pretty_print(&db).to_string(),
        "Evm"
    );
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
        include_str!("../../fe/tests/fixtures/fe_test/effect_params.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "test_non_evm_effect_param");
    let instance = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(caller))
            .expect("caller root instance should exist"),
    );
    let callee = instance
        .body(&db)
        .callees()
        .into_iter()
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func)
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
    assert_eq!(
        value_provider.provider_ty.pretty_print(&db).to_string(),
        "u256"
    );
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
fn root_effect_handle_bindings_specialize_provider_targets_to_underlying_values() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("explicit_raw_boundaries.fe"),
        include_str!("../../codegen/tests/fixtures/explicit_raw_boundaries.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let entry = find_func(&db, top_mod, "explicit_raw_boundaries");
    let root = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(entry))
            .expect("root semantic instance should exist"),
    );
    let raw_store = root
        .body(&db)
        .callees()
        .into_iter()
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "raw_store")
            )
        })
        .expect("expected specialized raw_store callee");
    let data_binding = raw_store
        .body(&db)
        .locals
        .iter()
        .find_map(|local| match local.source {
            Some(
                binding @ fe_hir::analysis::ty::ty_check::LocalBinding::EffectParam {
                    binding_name,
                    ..
                },
            ) if binding_name.data(&db) == "data" => Some(binding),
            Some(_) | None => None,
        })
        .expect("expected specialized raw_store data effect binding");
    let provider = resolved_provider_binding_for_instance_effect(&db, raw_store, data_binding)
        .expect("specialized data effect binding should resolve a provider");
    assert_eq!(
        provider.semantics.kind,
        fe_hir::analysis::ty::ProviderKind::Handle
    );
    assert_eq!(
        provider
            .semantics
            .target_ty
            .expect("specialized provider should keep its underlying target")
            .pretty_print(&db)
            .to_string(),
        "Data"
    );
}

#[test]
fn free_function_effect_calls_monomorphize_distinct_provider_bindings() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("free_function_effect_calls_monomorphize_distinct_provider_bindings.fe"),
        r#"
use std::evm::StorageMap

fn needs(_ addr: u256) -> u256
    uses (balances: StorageMap<u256, u256>)
{
    balances.get(key: addr)
}

fn caller() {
    let mut left = StorageMap<u256, u256, 0>::new()
    let mut right = StorageMap<u256, u256, 0>::new()
    with (left) {
        let _ = needs(1)
    }
    with (right) {
        let _ = needs(2)
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "caller");
    let instance = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(caller))
            .expect("caller root instance should exist"),
    );
    let callees = instance
        .body(&db)
        .callees()
        .into_iter()
        .filter(|callee| {
            matches!(
                callee.key.owner(&db),
                BodyOwner::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "needs")
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        callees.len(),
        2,
        "different free-function effect providers should produce distinct callee instances",
    );
    assert_ne!(callees[0].key, callees[1].key);
}

#[test]
fn free_function_effect_bindings_keep_explicit_memory_address_space() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("mutable_array_args_and_effects.fe"),
        include_str!("../../fe/tests/fixtures/fe_test/mutable_array_args_and_effects.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let caller = find_func(&db, top_mod, "test_mut_array_effect_param");
    let instance = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(caller))
            .expect("caller root instance should exist"),
    );
    let write_effects_and_sum = instance
        .body(&db)
        .callees()
        .into_iter()
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func)
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
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .find(|callee| {
            matches!(
                callee.key(&db).owner(&db),
                BodyOwner::Func(func)
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
        Some(fe_hir::analysis::ty::ProviderAddressSpace::Memory),
        "free-function effect providers should keep explicit memory address spaces: {:#?}",
        env.providers(&db),
    );
}

#[test]
fn local_effect_handle_calls_preserve_concrete_provider_spaces() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("local_effect_handle_provider_spaces.fe"),
        r#"
use std::evm::{MemPtr, RawMem, RawStorage, StorPtr}

struct Foo {
    value: u256,
}

fn bump_mem() uses (foo: mut Foo) {
    foo.value += 1
}

fn bump_stor() uses (foo: mut Foo) {
    foo.value += 1
}

fn test_spaces() uses (st: mut RawStorage, mem: mut RawMem) {
    let mp: MemPtr<Foo> = mem.mem_ptr(0x100)
    with (mp) {
        bump_mem()
    }

    let sp: StorPtr<Foo> = st.stor_ptr(0)
    with (sp) {
        bump_stor()
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
    assert_eq!(mem_args[0].pass_mode, EffectPassMode::ByValue);
    assert_eq!(
        mem_args[0].provider,
        Some(fe_hir::analysis::ty::ProviderAddressSpace::Memory)
    );

    let stor_args = typed_body
        .call_effect_args(stor_call)
        .expect("bump_stor should have a resolved effect arg");
    assert_eq!(stor_args.len(), 1);
    assert_eq!(stor_args[0].pass_mode, EffectPassMode::ByValue);
    assert_eq!(
        stor_args[0].provider,
        Some(fe_hir::analysis::ty::ProviderAddressSpace::Storage)
    );
}

#[test]
fn projected_handle_places_use_projected_provider_spaces() {
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
    let holder = Holder { ptr: evm.stor_ptr(slot) }
    with (holder.ptr) {
        write_cell(value)
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
    assert_eq!(effect_args[0].pass_mode, EffectPassMode::ByValue);
    assert_eq!(
        effect_args[0].provider,
        Some(fe_hir::analysis::ty::ProviderAddressSpace::Storage)
    );
}

#[test]
fn handle_typed_effect_bindings_use_by_value_transport() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("handle_typed_effect_bindings_use_by_value_transport.fe"),
        include_str!("../../codegen/tests/fixtures/explicit_raw_boundaries.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "explicit_raw_boundaries");
    let typed_body = check_func_body(&db, func).1.clone();
    let call_expr = find_named_call_expr(&db, func, "raw_store");
    let effect_args = typed_body
        .call_effect_args(call_expr)
        .expect("raw_store should have resolved effect args");
    assert_eq!(effect_args.len(), 2);
    assert_eq!(effect_args[0].pass_mode, EffectPassMode::ByValue);
    assert_eq!(
        effect_args[0]
            .instantiated_key_ty
            .expect("handle effect arg should retain its matched key carrier")
            .pretty_print(&db)
            .to_string(),
        "MemPtr<Data>"
    );
    assert_eq!(
        effect_args[0]
            .provider_target_ty
            .expect("handle effect arg should lower against its underlying provider target")
            .pretty_print(&db)
            .to_string(),
        "Data"
    );
    assert_eq!(
        effect_args[0].provider,
        Some(fe_hir::analysis::ty::ProviderAddressSpace::Memory)
    );
}

#[test]
fn root_semantic_instance_keys_are_closed_under_root_provider_subst() {
    let mut db = HirAnalysisTestDb::default();
    let storage_map_file = db.new_stand_alone(
        Utf8PathBuf::from("storage_map_contract.fe"),
        include_str!("../../codegen/tests/fixtures/storage_map_contract.fe"),
    );
    let (top_mod, _) = db.top_mod(storage_map_file);
    db.assert_no_diags(top_mod);
    for name in ["init", "runtime"] {
        let func = find_func(&db, top_mod, name);
        let key = root_semantic_instance_key(&db, BodyOwner::Func(func))
            .unwrap_or_else(|err| panic!("manual root `{name}` should synthesize a closed root semantic instance key: {err:?}"));
        validate_instantiated_effect_env_key(&db, key).unwrap_or_else(|err| {
            panic!(
                "manual root `{name}` should have a closed effect env under synthesized root subst: {err:?}"
            )
        });
    }

    let test_file = db.new_stand_alone(
        Utf8PathBuf::from("contract_init_fixed_array_arg.fe"),
        include_str!("../../fe/tests/fixtures/fe_test/contract_init_fixed_array_arg.fe"),
    );
    let (top_mod, _) = db.top_mod(test_file);
    db.assert_no_diags(top_mod);
    let test_func = find_func(&db, top_mod, "test_contract_init_fixed_array_arg");
    let key = root_semantic_instance_key(&db, BodyOwner::Func(test_func)).unwrap_or_else(|err| {
        panic!("test root should synthesize a closed root semantic instance key: {err:?}")
    });
    validate_instantiated_effect_env_key(&db, key).unwrap_or_else(|err| {
        panic!("test root should have a closed effect env under synthesized root subst: {err:?}")
    });
}
