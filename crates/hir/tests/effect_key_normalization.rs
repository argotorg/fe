use camino::Utf8PathBuf;
use common::diagnostics::{CompleteDiagnostic, cmp_complete_diagnostics};
use fe_hir::analysis::place::PlaceBase;
use fe_hir::analysis::ty::effects::{EffectKeyKind, place_effect_provider_param_index_map};
use fe_hir::analysis::ty::ty_check::{EffectArg, TypedBody, check_func_body};
use fe_hir::hir_def::{CallableDef, Expr, ExprId, Func, ItemKind, Partial, TopLevelMod};
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

fn find_call_expr<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>) -> ExprId {
    let body = func.body(db).expect("missing function body");
    body.exprs(db)
        .keys()
        .find(|expr| matches!(expr.data(db, body), Partial::Present(Expr::Call(..))))
        .expect("missing call expression")
}

fn find_method_call_expr<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>) -> ExprId {
    let body = func.body(db).expect("missing function body");
    body.exprs(db)
        .keys()
        .find(|expr| matches!(expr.data(db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing method call expression")
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
const fn plus1(x: usize) -> usize {
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

fn needs(x: u256) uses (cap: Cap<Slot>) {}

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

fn needs<X>(x: u256) uses (cap: Cap<X::Assoc>)
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
    let call_expr = find_call_expr(&db, caller);
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
    type AddressSpace = core::effect_ref::Memory

    fn from_raw(raw: u256) -> Self { Self { raw } }
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
    let call_expr = find_call_expr(&db, caller);
    let typed_body = check_func_body(&db, caller).1.clone();
    assert_single_trait_effect_arg(&typed_body, call_expr);
    assert_trait_effect_provider_arg(&db, caller, needs, call_expr, "Ptr<Console>");
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
    type AddressSpace = core::effect_ref::Memory

    fn from_raw(raw: u256) -> Self { Self { raw } }
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
    let call_expr = find_call_expr(&db, caller);
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
        diags[0].message.contains(
            "effect `Storage<T>` provided to `needs` has type `Storage<u16>`, but `Storage<u8>` is required",
        ),
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
