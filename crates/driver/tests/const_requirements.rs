//! Const `where` requirements through the driver.
//!
//! Rejected programs are rendered uitest fixtures (`ty_check/const_where`,
//! `semantic_borrowck/const_where`). This file covers accepted programs,
//! whose `answer*` functions are evaluated and snapshotted, and relational
//! checks that compare one program across edits, query orders or printing.

use std::path::{Path, PathBuf};

use common::{
    InputDb,
    diagnostics::Severity,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use dir_test::{Fixture, dir_test};
use fe_driver::DriverDataBase;
use hir::{
    analysis::semantic::{EvalOutcome, eval_body_owner_const_with_args},
    analysis::ty::ty_check::BodyOwner,
    hir_def::Func,
};
use test_utils::snap_test;
use url::Url;

fn database() -> DriverDataBase {
    let mut db = DriverDataBase::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    db
}

/// Reads `tests/fixtures/const_requirements/<relative>` and returns its real
/// path and its text with line endings normalized.
fn fixture(relative: &str) -> (PathBuf, String) {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/const_requirements")
        .join(relative);
    let text = std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("cannot read {}: {error}", path.display()));
    (path, text.replace("\r\n", "\n"))
}

/// Adds `source` to the workspace under the real file `path`.
fn input(db: &mut DriverDataBase, path: &Path, source: &str) -> File {
    db.workspace().touch(
        db,
        Url::from_file_path(path).expect("fixture paths are absolute"),
        Some(source.to_owned()),
    )
}

/// Adds an in-memory variant of a fixture, such as its printed form, which
/// has no file of its own.
fn virtual_input(db: &mut DriverDataBase, name: &str, source: &str) -> File {
    db.workspace().touch(
        db,
        Url::parse(&format!("test-const-requirements:/{name}.fe")).unwrap(),
        Some(source.to_owned()),
    )
}

/// Returns `source` with the first occurrence of `from` replaced by `to`.
fn edited(source: &str, from: &str, to: &str) -> String {
    assert!(
        source.contains(from),
        "`{from}` does not occur in the fixture"
    );
    source.replacen(from, to, 1)
}

/// Rendered HIR errors, followed by semantic (MIR) errors.
fn diagnostics(db: &DriverDataBase, file: File) -> String {
    let ingot = db.top_mod(file).ingot(db);
    let hir = db.run_on_ingot(ingot);
    let mut errors = if hir.has_errors(db) {
        hir.format_diags(db)
    } else {
        String::new()
    };
    let semantic = db.mir_diagnostics_for_ingot(ingot);
    if semantic
        .iter()
        .any(|diagnostic| diagnostic.severity == Severity::Error)
    {
        errors.push_str(&db.format_complete_diagnostics(&semantic));
    }
    errors
}

fn named<'db>(db: &'db DriverDataBase, file: File, name: &str) -> Func<'db> {
    db.top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| {
            func.name(db)
                .to_opt()
                .is_some_and(|ident| ident.data(db) == name)
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

fn evaluate(db: &DriverDataBase, file: File, name: &str) -> String {
    match eval_body_owner_const_with_args(
        db,
        BodyOwner::Func(named(db, file, name)),
        Vec::new(),
        Vec::new(),
    ) {
        EvalOutcome::Ready(value) => value.pretty_print(db),
        outcome => panic!("failed to evaluate `{name}`: {outcome:?}"),
    }
}

/// Each accepted program must produce no errors. Its top-level `const fn`s
/// whose names start with `answer` are evaluated, in source order.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/const_requirements/accept",
    glob: "**/*.fe"
)]
fn accepted(fixture: Fixture<&str>) {
    let mut db = database();
    let source = fixture.content().replace("\r\n", "\n");
    let file = input(&mut db, Path::new(fixture.path()), &source);
    let errors = diagnostics(&db, file);
    assert!(errors.is_empty(), "unexpected diagnostics:\n{errors}");
    let mut values = String::new();
    for func in db.top_mod(file).all_funcs(&db).iter() {
        let Some(name) = func.name(&db).to_opt().filter(|_| func.is_const(&db)) else {
            continue;
        };
        let name = name.data(&db).to_string();
        if name.starts_with("answer") {
            values.push_str(&format!("{name}() = {}\n", evaluate(&db, file, &name)));
        }
    }
    snap_test!(values, fixture.path());
}

#[test]
fn ground_predicates_are_checked_and_preserved_by_printing() {
    let mut db = database();
    let (path, source) =
        fixture("relational/ground_predicates_are_checked_and_preserved_by_printing/positive.fe");
    let file = input(&mut db, &path, &source);
    let errors = diagnostics(&db, file);
    assert!(errors.is_empty(), "unexpected diagnostics:\n{errors}");
    assert_eq!(evaluate(&db, file, "consume"), "42");
    let printed = db.top_mod(file).pretty_print(&db);
    assert!(printed.contains("where"));
    let copy = virtual_input(&mut db, "printed", &printed);
    let copy_errors = diagnostics(&db, copy);
    assert!(
        copy_errors.is_empty(),
        "unexpected diagnostics:\n{copy_errors}"
    );
    assert_eq!(evaluate(&db, copy, "consume"), "42");
}

#[test]
fn predicate_edits_match_fresh_compilation() {
    use salsa::Setter;
    let mut db = database();
    let (path, base) = fixture("relational/predicate_edits_match_fresh_compilation/edit.fe");
    let file = input(&mut db, &path, "");
    let false_source = edited(&base, "true", "false");
    for (source, is_true) in [
        (base.clone(), true),
        (false_source.clone(), false),
        (base.clone(), true),
    ] {
        file.set_text(&mut db).to(source.clone());
        let warm = diagnostics(&db, file);
        let mut fresh = database();
        let fresh_file = input(&mut fresh, &path, &source);
        assert_eq!(warm, diagnostics(&fresh, fresh_file));
        assert_eq!(warm.is_empty(), is_true);
    }
}

#[test]
fn generic_requirement_edits_match_fresh_compilation_in_both_query_orders() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    let (path, base) = fixture(
        "relational/generic_requirement_edits_match_fresh_compilation_in_both_query_orders/edit_generic.fe",
    );
    for caller_first in [false, true] {
        let mut db = database();
        let file = input(&mut db, &path, &base);
        for (condition, n) in [("N > 0", 1), ("N > 1", 1), ("N > 0", 1), ("N > 0", 0)] {
            let source = if condition == "N > 1" {
                edited(&base, "N > 0", "N > 1")
            } else if n == 0 {
                edited(&base, "bounded<1>()", "bounded<0>()")
            } else {
                base.clone()
            };
            file.set_text(&mut db).to(source.clone());
            let func = named(&db, file, if caller_first { "answer" } else { "bounded" });
            let _ = check_func_body(&db, func);
            let warm = diagnostics(&db, file);
            let mut fresh = database();
            let fresh_file = input(&mut fresh, &path, &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
            assert_eq!(warm.is_empty(), condition == "N > 0" && n == 1);
        }
    }
}

#[test]
fn cyclic_requirements_reject_independently_of_query_order() {
    use hir::analysis::ty::ty_check::{check_anon_const_body, check_func_body};
    use hir::analysis::ty::ty_def::TyId;
    use hir::hir_def::WhereClauseOwner;
    let (path, source) = fixture(
        "relational/cyclic_requirements_reject_independently_of_query_order/cyclic_order.fe",
    );
    for predicate_first in [true, false] {
        let mut db = database();
        let file = input(&mut db, &path, &source);
        let func = named(&db, file, "cyclic");
        if predicate_first {
            let body = WhereClauseOwner::Func(func)
                .clause(&db)
                .id
                .const_predicates(&db)[0];
            let _ = check_anon_const_body(&db, body, TyId::bool(&db));
        } else {
            let _ = check_func_body(&db, named(&db, file, "answer"));
        }
        let warm = diagnostics(&db, file);
        assert!(warm.contains("recursive const requirement"), "{warm}");
        let mut fresh = database();
        let fresh_file = input(&mut fresh, &path, &source);
        assert_eq!(warm, diagnostics(&fresh, fresh_file));
    }
}

#[test]
fn cached_evaluation_is_not_requirement_evidence() {
    let (path, source) =
        fixture("relational/cached_evaluation_is_not_requirement_evidence/evaluation_first.fe");
    let mut db = database();
    let file = input(&mut db, &path, &source);
    let _ = eval_body_owner_const_with_args(
        &db,
        BodyOwner::Func(named(&db, file, "answer")),
        Vec::new(),
        Vec::new(),
    );
    let warm = diagnostics(&db, file);
    assert!(
        warm.contains("const requirement") && warm.contains("false"),
        "{warm}"
    );
    let mut fresh = database();
    let fresh_file = input(&mut fresh, &path, &source);
    assert_eq!(warm, diagnostics(&fresh, fresh_file));
}

#[test]
fn mutually_recursive_requirements_have_stable_diagnostics() {
    use hir::analysis::ty::ty_check::check_func_body;
    let (path, source) =
        fixture("relational/mutually_recursive_requirements_have_stable_diagnostics/mutual.fe");
    for first in ["first", "second", "answer"] {
        let mut db = database();
        let file = input(&mut db, &path, &source);
        let _ = check_func_body(&db, named(&db, file, first));
        let warm = diagnostics(&db, file);
        assert!(warm.contains("recursive const requirement"), "{warm}");
        let mut fresh = database();
        let fresh_file = input(&mut fresh, &path, &source);
        assert_eq!(warm, diagnostics(&fresh, fresh_file));
    }
}

#[test]
fn requirement_evaluation_cannot_reenter_an_unfinished_type_expression() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    let (path, base) = fixture(
        "relational/requirement_evaluation_cannot_reenter_an_unfinished_type_expression/type_cycle.fe",
    );
    for first in ["count", "answer"] {
        let mut db = database();
        let file = input(&mut db, &path, "");
        for cyclic in [true, false, true] {
            let source = if cyclic {
                base.clone()
            } else {
                edited(&base, "if flag<1>()", "if true")
            };
            file.set_text(&mut db).to(source.clone());
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), !cyclic, "{first}: {warm}");
            let mut fresh = database();
            let fresh_file = input(&mut fresh, &path, &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
        }
    }
}
