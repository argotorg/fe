use camino::Utf8PathBuf;
use fe_hir::analysis::ty::{
    effects::EffectKeyKind,
    ty_check::{TypedBody, check_func_body},
};
use fe_hir::hir_def::{Expr, ExprId, Func, ItemKind, Partial, TopLevelMod};
use fe_hir::test_db::HirAnalysisTestDb;

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

fn assert_single_trait_effect_arg<'db>(typed_body: &TypedBody<'db>, call_expr: ExprId) {
    let effect_args = typed_body
        .call_effect_args(call_expr)
        .expect("missing resolved effect args");
    assert_eq!(effect_args.len(), 1);
    assert_eq!(effect_args[0].key_kind, EffectKeyKind::Trait);
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
fn keyed_with_trait_bindings_accept_schematic_providers_via_unkeyed_fallback() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from(
            "keyed_with_trait_bindings_accept_schematic_providers_via_unkeyed_fallback.fe",
        ),
        r#"
trait Logger {
    fn log(self)
}

fn needs_logger() uses (logger: Logger) {}

fn with_logger<L: Logger>(logger: L) {
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
