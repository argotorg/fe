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
