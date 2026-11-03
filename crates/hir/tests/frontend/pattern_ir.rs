use fe_hir::analysis::ty::diagnostics::{BodyDiag, FuncBodyDiag};
use fe_hir::analysis::ty::pattern_ir::ValidatedPatKind;
use fe_hir::analysis::ty::ty_check::check_func_body;
use fe_hir::hir_def::{Partial, Pat};
use fe_hir::test_db::HirAnalysisTestDb;

fn with_func_body(
    src: &str,
    func_name: &str,
    f: impl for<'db> FnOnce(
        &'db HirAnalysisTestDb,
        Vec<FuncBodyDiag<'db>>,
        fe_hir::analysis::ty::ty_check::TypedBody<'db>,
    ),
) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(format!("{func_name}.fe").into(), src);
    let db = &db;
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| {
            func.name(db)
                .to_opt()
                .is_some_and(|name| name.data(db) == func_name)
        })
        .unwrap_or_else(|| panic!("function `{func_name}` not found"));
    let (diags, typed_body) = check_func_body(db, func).clone();
    f(db, diags, typed_body);
}

fn body_has_diag(diags: &[FuncBodyDiag<'_>], pred: impl Fn(&BodyDiag<'_>) -> bool) -> bool {
    diags
        .iter()
        .any(|diag| matches!(diag, FuncBodyDiag::Body(body) if pred(body)))
}

fn first_tuple_pat<'db>(
    db: &'db HirAnalysisTestDb,
    typed_body: &fe_hir::analysis::ty::ty_check::TypedBody<'db>,
) -> fe_hir::hir_def::PatId {
    let body = typed_body.body().unwrap();
    body.pats(db)
        .keys()
        .find(|pat| matches!(pat.data(db, body), Partial::Present(Pat::Tuple(..))))
        .unwrap()
}

fn first_or_pat<'db>(
    db: &'db HirAnalysisTestDb,
    typed_body: &fe_hir::analysis::ty::ty_check::TypedBody<'db>,
) -> fe_hir::hir_def::PatId {
    let body = typed_body.body().unwrap();
    body.pats(db)
        .keys()
        .find(|pat| matches!(pat.data(db, body), Partial::Present(Pat::Or(..))))
        .unwrap()
}

#[test]
fn invalid_match_arm_suppresses_unreachable_diagnostics() {
    with_func_body(
        r#"
enum Tag {
    A,
    B,
}

fn test(tag: Tag) -> u8 {
    match tag {
        Tag::A(_) => 0
        _ => 1
        Tag::B => 2
    }
}
"#,
        "test",
        |_db, diags, _typed_body| {
            assert!(
                body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::TupleVariantExpected { .. }
                )),
                "expected tuple-variant diagnostic, got {diags:?}",
            );
            assert!(
                !body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::UnreachablePattern { .. }
                )),
                "unexpected unreachable-pattern diagnostic: {diags:?}",
            );
        },
    );
}

#[test]
fn invalid_match_arm_suppresses_non_exhaustive_diagnostics() {
    with_func_body(
        r#"
enum Tag {
    A,
    B,
}

fn test(tag: Tag) -> u8 {
    match tag {
        Tag::A(_) => 0
    }
}
"#,
        "test",
        |_db, diags, _typed_body| {
            assert!(
                body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::TupleVariantExpected { .. }
                )),
                "expected tuple-variant diagnostic, got {diags:?}",
            );
            assert!(
                !body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::NonExhaustiveMatch { .. }
                )),
                "unexpected non-exhaustive-match diagnostic: {diags:?}",
            );
        },
    );
}

#[test]
fn duplicate_binding_pattern_has_no_semantic_root_and_suppresses_unreachable_diagnostics() {
    with_func_body(
        r#"
fn test(x: (u8, u8)) -> u8 {
    match x {
        (a, a) => 0
        _ => 1
    }
}
"#,
        "test",
        |db, diags, typed_body| {
            assert!(
                body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::DuplicatedBinding { .. }
                )),
                "expected duplicate-binding diagnostic, got {diags:?}",
            );
            assert!(
                !body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::UnreachablePattern { .. }
                )),
                "unexpected unreachable-pattern diagnostic: {diags:?}",
            );
            assert!(
                typed_body
                    .pattern_root(first_tuple_pat(db, &typed_body))
                    .is_none()
            );
        },
    );
}

#[test]
fn duplicate_binding_pattern_suppresses_non_exhaustive_diagnostics() {
    with_func_body(
        r#"
fn test(x: (u8, u8)) -> u8 {
    match x {
        (a, a) => 0
    }
}
"#,
        "test",
        |db, diags, typed_body| {
            assert!(
                body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::DuplicatedBinding { .. }
                )),
                "expected duplicate-binding diagnostic, got {diags:?}",
            );
            assert!(
                !body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::NonExhaustiveMatch { .. }
                )),
                "unexpected non-exhaustive-match diagnostic: {diags:?}",
            );
            assert!(
                typed_body
                    .pattern_root(first_tuple_pat(db, &typed_body))
                    .is_none()
            );
        },
    );
}

#[test]
fn duplicate_rest_pattern_has_no_semantic_root_and_suppresses_unreachable_diagnostics() {
    with_func_body(
        r#"
fn test(x: (u8, u8)) -> u8 {
    match x {
        (.., ..) => 0
        _ => 1
    }
}
"#,
        "test",
        |db, diags, typed_body| {
            assert!(
                body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::DuplicatedRestPat(..)
                )),
                "expected duplicate-rest diagnostic, got {diags:?}",
            );
            assert!(
                !body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::UnreachablePattern { .. }
                )),
                "unexpected unreachable-pattern diagnostic: {diags:?}",
            );
            assert!(
                typed_body
                    .pattern_root(first_tuple_pat(db, &typed_body))
                    .is_none()
            );
        },
    );
}

#[test]
fn duplicate_rest_pattern_suppresses_non_exhaustive_diagnostics() {
    with_func_body(
        r#"
fn test(x: (u8, u8)) -> u8 {
    match x {
        (.., ..) => 0
    }
}
"#,
        "test",
        |db, diags, typed_body| {
            assert!(
                body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::DuplicatedRestPat(..)
                )),
                "expected duplicate-rest diagnostic, got {diags:?}",
            );
            assert!(
                !body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::NonExhaustiveMatch { .. }
                )),
                "unexpected non-exhaustive-match diagnostic: {diags:?}",
            );
            assert!(
                typed_body
                    .pattern_root(first_tuple_pat(db, &typed_body))
                    .is_none()
            );
        },
    );
}

#[test]
fn or_pattern_bindings_are_rejected_and_not_seeded_into_scope() {
    with_func_body(
        r#"
enum E {
    A(u8),
    B,
}

fn test(e: E) -> u8 {
    match e {
        E::A(x) | E::B => x
    }
}
"#,
        "test",
        |db, diags, typed_body| {
            assert!(
                body_has_diag(&diags, |diag| matches!(diag, BodyDiag::BindingsInOrPat(..))),
                "expected or-pattern binding diagnostic, got {diags:?}",
            );
            assert!(
                body_has_diag(&diags, |diag| matches!(
                    diag,
                    BodyDiag::UndefinedVariable(_, ident, _) if ident.data(db) == "x"
                )),
                "expected undefined-variable diagnostic, got {diags:?}",
            );

            let body = typed_body.body().unwrap();
            let binding_pat = body
                .pats(db)
                .keys()
                .find(|pat| {
                    let Partial::Present(Pat::Path(Partial::Present(path), _)) = pat.data(db, body)
                    else {
                        return false;
                    };
                    path.as_ident(db).is_some_and(|ident| ident.data(db) == "x")
                })
                .unwrap();

            assert!(
                typed_body
                    .pattern_root(first_or_pat(db, &typed_body))
                    .is_none()
            );
            assert!(typed_body.pat_binding(binding_pat).is_none());
        },
    );
}

#[test]
fn exhaustive_or_patterns_are_irrefutable() {
    with_func_body(
        r#"
fn test(flag: bool) -> u8 {
    match flag {
        true | false => 0
    }
}
"#,
        "test",
        |db, diags, typed_body| {
            assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");

            let root = typed_body
                .pattern_root(first_or_pat(db, &typed_body))
                .unwrap();
            assert!(typed_body.pattern_store().is_irrefutable(db, root));
        },
    );
}
#[test]
fn record_fields_are_canonicalized_in_pattern_ir() {
    with_func_body(
        r#"
struct Triple {
    a: u8,
    b: u8,
    c: u8,
}

fn test(x: Triple) -> u8 {
    match x {
        Triple { c, a, .. } => 0
    }
}
"#,
        "test",
        |db, _diags, typed_body| {
            let body = typed_body.body().unwrap();
            let record_pat = body
                .pats(db)
                .keys()
                .find(|pat| matches!(pat.data(db, body), Partial::Present(Pat::Record(..))))
                .unwrap();
            let root = typed_body.pattern_root(record_pat).unwrap();
            let store = typed_body.pattern_store();
            let root = store.node(root);

            let ValidatedPatKind::Constructor { fields, .. } = root.kind() else {
                panic!("expected record constructor root, got {:?}", root.kind());
            };
            assert_eq!(fields.len(), 3);

            let first = store.node(fields[0]);
            let second = store.node(fields[1]);
            let third = store.node(fields[2]);

            assert!(matches!(
                first.kind(),
                ValidatedPatKind::Wildcard {
                    binding: Some(binding)
                } if binding.name.data(db) == "a"
            ));
            assert!(matches!(
                second.kind(),
                ValidatedPatKind::Wildcard { binding: None }
            ));
            assert!(matches!(
                third.kind(),
                ValidatedPatKind::Wildcard {
                    binding: Some(binding)
                } if binding.name.data(db) == "c"
            ));
        },
    );
}

#[test]
fn tuple_rest_is_expanded_in_pattern_ir() {
    with_func_body(
        r#"
fn test(x: (u8, u8, u8, u8)) -> u8 {
    match x {
        (a, .., d) => 0
    }
}
"#,
        "test",
        |db, _diags, typed_body| {
            let body = typed_body.body().unwrap();
            let tuple_pat = body
                .pats(db)
                .keys()
                .find(|pat| matches!(pat.data(db, body), Partial::Present(Pat::Tuple(..))))
                .unwrap();
            let root = typed_body.pattern_root(tuple_pat).unwrap();
            let store = typed_body.pattern_store();
            let root = store.node(root);

            let ValidatedPatKind::Constructor { fields, .. } = root.kind() else {
                panic!("expected tuple constructor root, got {:?}", root.kind());
            };
            assert_eq!(fields.len(), 4);

            assert!(matches!(
                store.node(fields[0]).kind(),
                ValidatedPatKind::Wildcard {
                    binding: Some(binding)
                } if binding.name.data(db) == "a"
            ));
            assert!(matches!(
                store.node(fields[1]).kind(),
                ValidatedPatKind::Wildcard { binding: None }
            ));
            assert!(matches!(
                store.node(fields[2]).kind(),
                ValidatedPatKind::Wildcard { binding: None }
            ));
            assert!(matches!(
                store.node(fields[3]).kind(),
                ValidatedPatKind::Wildcard {
                    binding: Some(binding)
                } if binding.name.data(db) == "d"
            ));
        },
    );
}

#[test]
fn bare_binding_patterns_shadow_functions() {
    with_func_body(
        r#"
fn input() -> u8 {
    0
}

fn test(x: u8) -> u8 {
    let input = x
    input
}
"#,
        "test",
        |db, diags, typed_body| {
            assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");

            let body = typed_body.body().unwrap();
            let binding_pat = body
                .pats(db)
                .keys()
                .find(|pat| matches!(pat.data(db, body), Partial::Present(Pat::Path(..))))
                .unwrap();
            let root = typed_body.pattern_root(binding_pat).unwrap();

            assert!(matches!(
                typed_body.pattern_store().node(root).kind(),
                ValidatedPatKind::Wildcard {
                    binding: Some(binding)
                } if binding.name.data(db) == "input"
            ));
            assert!(typed_body.pat_binding(binding_pat).is_some());
        },
    );
}
