use std::panic::{AssertUnwindSafe, catch_unwind};

use camino::Utf8PathBuf;
use fe_hir::analysis::ty::{
    const_ty::ConstTyData,
    ty_check::check_func_body,
    ty_def::{TyData, TyId},
};
use fe_hir::hir_def::{ItemKind, Partial, Pat};
use fe_hir::test_db::HirAnalysisTestDb;

fn find_func<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: fe_hir::hir_def::TopLevelMod<'db>,
    name: &str,
) -> fe_hir::hir_def::Func<'db> {
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

#[test]
fn type_effect_keys_keep_omitted_const_expr_defaults_unevaluated() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("type_effect_keys_keep_omitted_const_expr_defaults_unevaluated.fe"),
        r#"
const ROOT: u256 = 7

struct Slot<T, const N: u256 = ROOT> {}

fn f() uses (slot: Slot<u256>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let key_ty = func
        .effect_bindings(&db)
        .first()
        .and_then(|binding| binding.key_ty)
        .expect("missing type effect key");
    let default_arg = key_ty
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing defaulted const arg");

    let TyData::ConstTy(default_const) = default_arg.data(&db) else {
        panic!("expected const generic arg, got {default_arg:?}");
    };
    assert!(
        matches!(default_const.data(&db), ConstTyData::UnEvaluated { .. }),
        "default const arg was unexpectedly evaluated: {default_const:?}"
    );
    assert_eq!(default_const.ty(&db), TyId::u256(&db));
}

#[test]
fn type_alias_effect_keys_keep_omitted_const_expr_defaults_unevaluated() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("type_alias_effect_keys_keep_omitted_const_expr_defaults_unevaluated.fe"),
        r#"
const ROOT: u256 = 7

struct Raw<T, const N: u256> {}
type Slot<T, const N: u256 = ROOT> = Raw<T, N>

fn f() uses (slot: Slot<u256>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let key_ty = func
        .effect_bindings(&db)
        .first()
        .and_then(|binding| binding.key_ty)
        .expect("missing type effect key");
    let default_arg = key_ty
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing defaulted const arg");

    let TyData::ConstTy(default_const) = default_arg.data(&db) else {
        panic!("expected const generic arg, got {default_arg:?}");
    };
    assert!(
        matches!(default_const.data(&db), ConstTyData::UnEvaluated { .. }),
        "type-alias default const arg was unexpectedly evaluated: {:?}",
        default_const.data(&db)
    );
    assert_eq!(default_const.ty(&db), TyId::u256(&db));
}

#[test]
fn metadata_default_const_args_capture_prior_generic_bindings() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("metadata_default_const_args_capture_prior_generic_bindings.fe"),
        r#"
struct Slot<const BASE: u256, const N: u256 = BASE> {}

fn f() uses (slot: Slot<7>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let key_ty = func
        .effect_bindings(&db)
        .first()
        .and_then(|binding| binding.key_ty)
        .expect("missing type effect key");
    let base_arg = key_ty
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing explicit const arg");
    let default_arg = key_ty
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing defaulted const arg");

    let TyData::ConstTy(default_const) = default_arg.data(&db) else {
        panic!("expected const generic arg, got {default_arg:?}");
    };
    let ConstTyData::UnEvaluated { generic_args, .. } = default_const.data(&db) else {
        panic!("expected unevaluated default const arg, got {default_const:?}");
    };

    assert_eq!(generic_args.len(), 1);
    assert_eq!(generic_args[0], base_arg);
}

#[test]
fn trait_generic_args_still_validate_const_type_mismatches() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_generic_args_still_validate_const_type_mismatches.fe"),
        r#"
trait Cap<const N: u256> {}

fn f() uses (cap: Cap<false>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let result = catch_unwind(AssertUnwindSafe(|| db.assert_no_diags(top_mod)));

    assert!(
        result.is_err(),
        "trait generic arg type checking unexpectedly accepted a bool const for `u256`"
    );
}

#[test]
fn typed_body_preserves_metadata_only_const_defaults() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("typed_body_preserves_metadata_only_const_defaults.fe"),
        r#"
const fn plus1(x: usize) -> usize {
    x + 1
}

struct Foo<const N: usize, const M: usize = plus1(N)> {}

fn f() {
    let x: Foo<4>
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let body = func.body(&db).expect("missing function body");
    let typed_body = check_func_body(&db, func).1.clone();
    let x_pat = body
        .pats(&db)
        .keys()
        .find(|pat| {
            matches!(
                pat.data(&db, body),
                Partial::Present(Pat::Path(Partial::Present(path), _))
                    if path.as_ident(&db).is_some_and(|ident| ident.data(&db) == "x")
            )
        })
        .expect("missing `x` pattern");
    let x_ty = typed_body.pat_ty(&db, x_pat);
    let default_arg = x_ty
        .generic_args(&db)
        .get(1)
        .copied()
        .expect("missing defaulted const arg");

    let TyData::ConstTy(default_const) = default_arg.data(&db) else {
        panic!("expected const generic arg, got {default_arg:?}");
    };
    assert!(
        matches!(default_const.data(&db), ConstTyData::UnEvaluated { .. }),
        "typed-body const default was unexpectedly evaluated: {:?}",
        default_const.data(&db)
    );
}

#[test]
fn omitted_const_expr_defaults_unify_with_explicit_defaults() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("omitted_const_expr_defaults_unify_with_explicit_defaults.fe"),
        r#"
const fn plus1(x: usize) -> usize {
    x + 1
}

struct Foo<const N: usize, const M: usize = plus1(N)> {}

fn takes(_: Foo<4, 5>) {}

fn f(x: Foo<4>) {
    takes(x)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn type_alias_omitted_const_expr_defaults_unify_with_explicit_defaults() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("type_alias_omitted_const_expr_defaults_unify_with_explicit_defaults.fe"),
        r#"
const fn plus1(x: usize) -> usize {
    x + 1
}

struct Raw<const N: usize, const M: usize> {}
type Foo<const N: usize, const M: usize = plus1(N)> = Raw<N, M>

fn takes(_: Raw<4, 5>) {}

fn f(x: Foo<4>) {
    takes(x)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn metadata_only_default_const_args_validate_adt_defaults() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("metadata_only_default_const_args_validate_adt_defaults.fe"),
        r#"
struct Bad<const N: u256 = false> {}

fn f(x: Bad) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let result = catch_unwind(AssertUnwindSafe(|| db.assert_no_diags(top_mod)));

    assert!(
        result.is_err(),
        "metadata-only default validation unexpectedly accepted `false` as a `u256` default"
    );
}

#[test]
fn metadata_only_default_const_args_validate_type_alias_defaults() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("metadata_only_default_const_args_validate_type_alias_defaults.fe"),
        r#"
type Alias<const N: u256 = false> = u256

fn f(x: Alias) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let result = catch_unwind(AssertUnwindSafe(|| db.assert_no_diags(top_mod)));

    assert!(
        result.is_err(),
        "metadata-only alias default validation unexpectedly accepted `false` as a `u256` default"
    );
}

#[test]
fn type_alias_explicit_const_holes_validate_with_expected_const_type() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("type_alias_explicit_const_holes_validate_with_expected_const_type.fe"),
        r#"
type Alias<const N: u256> = bool

fn f(x: Alias<_>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn type_alias_explicit_const_args_still_validate_type_mismatches() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("type_alias_explicit_const_args_still_validate_type_mismatches.fe"),
        r#"
type Alias<const N: u256> = bool

fn f(x: Alias<false>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let result = catch_unwind(AssertUnwindSafe(|| db.assert_no_diags(top_mod)));

    assert!(
        result.is_err(),
        "type-alias explicit const arg validation unexpectedly accepted `false` as a `u256` arg"
    );
}

#[test]
fn scoped_field_instantiation_does_not_capture_unrelated_outer_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("scoped_field_instantiation_does_not_capture_unrelated_outer_args.fe"),
        r#"
struct Inner<const N: u256 = 5> {}

struct Outer<T> {
    x: Inner,
    y: T,
}

fn f(a: Outer<u8>, b: Outer<u16>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let outer_u8 = func.arg_tys(&db)[0]
        .instantiate_identity()
        .as_view(&db)
        .unwrap_or_else(|| panic!("expected view parameter"));
    let outer_u16 = func.arg_tys(&db)[1]
        .instantiate_identity()
        .as_view(&db)
        .unwrap_or_else(|| panic!("expected view parameter"));
    let inner_from_u8 = outer_u8.field_types(&db)[0];
    let inner_from_u16 = outer_u16.field_types(&db)[0];

    assert_eq!(inner_from_u8, inner_from_u16);

    let default_arg = inner_from_u8
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing defaulted const arg");
    let TyData::ConstTy(default_const) = default_arg.data(&db) else {
        panic!("expected const generic arg, got {default_arg:?}");
    };
    let ConstTyData::UnEvaluated { generic_args, .. } = default_const.data(&db) else {
        panic!("expected unevaluated default const arg, got {default_const:?}");
    };

    assert!(
        generic_args.is_empty(),
        "field projection leaked outer args into unrelated inner default: {generic_args:?}"
    );
}

#[test]
fn type_alias_instantiation_does_not_capture_unrelated_outer_args() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("type_alias_instantiation_does_not_capture_unrelated_outer_args.fe"),
        r#"
struct Inner<const N: u256 = 5> {}

type Alias<T> = (T, Inner)

fn f(a: Alias<u8>, b: Alias<u16>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f");
    let alias_u8 = func.arg_tys(&db)[0]
        .instantiate_identity()
        .as_view(&db)
        .unwrap_or_else(|| panic!("expected view parameter"));
    let alias_u16 = func.arg_tys(&db)[1]
        .instantiate_identity()
        .as_view(&db)
        .unwrap_or_else(|| panic!("expected view parameter"));
    let inner_from_u8 = alias_u8.field_types(&db)[1];
    let inner_from_u16 = alias_u16.field_types(&db)[1];

    assert_eq!(inner_from_u8, inner_from_u16);

    let default_arg = inner_from_u8
        .generic_args(&db)
        .first()
        .copied()
        .expect("missing defaulted const arg");
    let TyData::ConstTy(default_const) = default_arg.data(&db) else {
        panic!("expected const generic arg, got {default_arg:?}");
    };
    let ConstTyData::UnEvaluated { generic_args, .. } = default_const.data(&db) else {
        panic!("expected unevaluated default const arg, got {default_const:?}");
    };

    assert!(
        generic_args.is_empty(),
        "type-alias instantiation leaked outer args into unrelated inner default: {generic_args:?}"
    );
}
