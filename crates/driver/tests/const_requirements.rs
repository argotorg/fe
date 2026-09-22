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

#[test]
fn scoped_premise_edits_match_fresh_databases() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    let (path, base) =
        fixture("relational/scoped_premise_edits_match_fresh_databases/scoped_edits.fe");
    for first in ["forward", "answer"] {
        let mut db = database();
        let file = input(&mut db, &path, "");
        for premise in ["M > 0", "M > 1", "M > 0"] {
            let source = if premise == "M > 1" {
                edited(&base, "M > 0", "M > 1")
            } else {
                base.clone()
            };
            file.set_text(&mut db).to(source.clone());
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(
                warm.is_empty(),
                premise == "M > 0",
                "{first}, {premise}: {warm}"
            );
            let mut fresh = database();
            let fresh_file = input(&mut fresh, &path, &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
        }
    }
}

#[test]
fn record_forwarding_and_edits_match_fresh_compilation() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    let (path, base) =
        fixture("relational/record_forwarding_and_edits_match_fresh_compilation/base.fe");
    for first in ["make", "answer"] {
        let mut db = database();
        let file = input(&mut db, &path, "");
        for (source, valid) in [
            (base.clone(), true),
            (edited(&base, "M > 0", "M > 1"), false),
            (edited(&base, "<1>", "<0>"), false),
            (base.clone(), true),
        ] {
            file.set_text(&mut db).to(source.clone());
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), valid, "{first}: {warm}");
            let mut fresh = database();
            let fresh_file = input(&mut fresh, &path, &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
            if valid {
                assert_eq!(evaluate(&db, file, "answer"), "42");
            }
        }
    }
}

#[test]
fn record_formation_cycles_reject_across_query_orders_and_edits() {
    use hir::analysis::ty::{
        ty_check::{check_anon_const_body, check_func_body},
        ty_def::TyId,
    };
    use hir::hir_def::{ItemKind, WhereClauseOwner};
    use salsa::Setter;
    let (path, base) =
        fixture("relational/record_formation_cycles_reject_across_query_orders_and_edits/base.fe");
    for predicate_first in [false, true] {
        let mut db = database();
        let file = input(&mut db, &path, "");
        for (source, cyclic) in [
            (base.clone(), true),
            (edited(&base, "Bounded<N>", "u256"), false),
            (base.clone(), true),
        ] {
            file.set_text(&mut db).to(source.clone());
            if predicate_first {
                let record = db
                    .top_mod(file)
                    .all_items(&db)
                    .iter()
                    .find_map(|item| match item {
                        ItemKind::Struct(record) => Some(*record),
                        _ => None,
                    })
                    .unwrap();
                let predicate = WhereClauseOwner::Struct(record)
                    .clause(&db)
                    .id
                    .const_predicates(&db)[0];
                let _ = check_anon_const_body(&db, predicate, TyId::bool(&db));
            } else {
                let _ = check_func_body(&db, named(&db, file, "use_it"));
            }
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), !cyclic, "{warm}");
            if cyclic {
                assert!(warm.contains("const requirement"), "{warm}");
            }
            let mut fresh = database();
            let fresh_file = input(&mut fresh, &path, &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
        }
    }
}

#[test]
fn enum_requirement_edits_match_fresh_compilation() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    let (path, base) = fixture("relational/enum_requirement_edits_match_fresh_compilation/base.fe");
    for first in ["make", "answer"] {
        let mut db = database();
        let file = input(&mut db, &path, "");
        for (source, valid) in [
            (base.clone(), true),
            (edited(&base, "M > 0", "M > 1"), false),
            (edited(&base, "<1>", "<0>"), false),
            (base.clone(), true),
        ] {
            file.set_text(&mut db).to(source.clone());
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), valid, "{first}: {warm}");
            let mut fresh = database();
            let fresh_file = input(&mut fresh, &path, &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
            if valid {
                assert_eq!(evaluate(&db, file, "answer"), "42");
            }
        }
    }
}

#[test]
fn enum_formation_cycles_reject_across_query_orders_and_edits() {
    use hir::analysis::ty::{
        ty_check::{check_anon_const_body, check_func_body},
        ty_def::TyId,
    };
    use hir::hir_def::{ItemKind, WhereClauseOwner};
    use salsa::Setter;
    let (path, base) =
        fixture("relational/enum_formation_cycles_reject_across_query_orders_and_edits/base.fe");
    for predicate_first in [false, true] {
        let mut db = database();
        let file = input(&mut db, &path, "");
        for (source, cyclic) in [
            (base.clone(), true),
            (edited(&base, "Choice<N>", "u256"), false),
            (base.clone(), true),
        ] {
            file.set_text(&mut db).to(source.clone());
            if predicate_first {
                let enum_ = db
                    .top_mod(file)
                    .all_items(&db)
                    .iter()
                    .find_map(|item| match item {
                        ItemKind::Enum(enum_) => Some(*enum_),
                        _ => None,
                    })
                    .unwrap();
                let predicate = WhereClauseOwner::Enum(enum_)
                    .clause(&db)
                    .id
                    .const_predicates(&db)[0];
                let _ = check_anon_const_body(&db, predicate, TyId::bool(&db));
            } else {
                let _ = check_func_body(&db, named(&db, file, "use_it"));
            }
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), !cyclic, "{warm}");
            if cyclic {
                assert!(warm.contains("const requirement"), "{warm}");
            }
            let mut fresh = database();
            let fresh_file = input(&mut fresh, &path, &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
        }
    }
}

#[test]
fn inherent_methods_check_receiver_and_method_arguments() {
    use salsa::Setter;
    for case in ["associated", "receiver", "qualified", "method_generic"] {
        let mut db = database();
        let (path, source) = fixture(&format!(
            "relational/inherent_methods_check_receiver_and_method_arguments/{case}.fe"
        ));
        let file = input(&mut db, &path, &source);
        let errors = diagnostics(&db, file);
        assert!(errors.is_empty(), "unexpected diagnostics:\n{errors}");
        assert_eq!(evaluate(&db, file, "answer"), "42");
        let edited_source = edited(&source, "Window<1>", "Window<0>");
        file.set_text(&mut db).to(edited_source);
        let errors = diagnostics(&db, file);
        assert!(
            errors.contains("const requirement") && errors.contains("false"),
            "{case}: {errors}"
        );
    }
    let mut db = database();
    let (path, source) =
        fixture("relational/inherent_methods_check_receiver_and_method_arguments/method_false.fe");
    let file = input(&mut db, &path, &source);
    let errors = diagnostics(&db, file);
    assert!(
        errors.contains("const requirement") && errors.contains("false"),
        "{errors}"
    );
}

#[test]
fn methods_forward_explicit_premises_across_binders() {
    use salsa::Setter;
    let mut db = database();
    let (forward_path, forward_base) =
        fixture("relational/methods_forward_explicit_premises_across_binders/forward.fe");
    let forward_file = input(&mut db, &forward_path, &forward_base);
    let errors = diagnostics(&db, forward_file);
    assert!(errors.is_empty(), "unexpected diagnostics:\n{errors}");
    assert_eq!(evaluate(&db, forward_file, "answer"), "42");
    let forward_edited = edited(&forward_base, "where A > 0, B > A", "where A > 0");
    forward_file.set_text(&mut db).to(forward_edited);
    assert!(diagnostics(&db, forward_file).contains("const requirement"));

    let (method_path, method_base) =
        fixture("relational/methods_forward_explicit_premises_across_binders/method_forward.fe");
    let method_file = input(&mut db, &method_path, &method_base);
    let errors = diagnostics(&db, method_file);
    assert!(errors.is_empty(), "unexpected diagnostics:\n{errors}");
    assert_eq!(evaluate(&db, method_file, "answer"), "42");
    let method_edited = edited(&method_base, "where N > 0", "");
    method_file.set_text(&mut db).to(method_edited);
    assert!(diagnostics(&db, method_file).contains("const requirement"));
}

#[test]
fn method_signatures_can_forward_but_formation_cannot() {
    use salsa::Setter;
    let mut db = database();
    let (signature_path, signature_base) =
        fixture("relational/method_signatures_can_forward_but_formation_cannot/signature.fe");
    let signature_file = input(&mut db, &signature_path, &signature_base);
    let errors = diagnostics(&db, signature_file);
    assert!(errors.is_empty(), "unexpected diagnostics:\n{errors}");
    assert_eq!(evaluate(&db, signature_file, "answer"), "42");
    let signature_edited = edited(&signature_base, "Window<1>", "Window<0>");
    signature_file.set_text(&mut db).to(signature_edited);
    assert!(diagnostics(&db, signature_file).contains("const requirement"));

    let (formation_path, formation_base) =
        fixture("relational/method_signatures_can_forward_but_formation_cannot/formation.fe");
    let formation_n = edited(&formation_base, "length<1>()", "length<N>()");
    let formation_file = input(&mut db, &formation_path, &formation_base);
    for (source, argument_is_one) in [(formation_base.clone(), true), (formation_n.clone(), false)]
    {
        formation_file.set_text(&mut db).to(source);
        let errors = diagnostics(&db, formation_file);
        assert_eq!(errors.is_empty(), argument_is_one, "{errors}");
        if !argument_is_one {
            assert!(errors.contains("const requirement"), "{errors}");
        }
    }
}

#[test]
fn method_signature_parameter_collection_is_query_order_independent() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    let (path, base) = fixture(
        "relational/method_signature_parameter_collection_is_query_order_independent/signature_order.fe",
    );
    let m0 = edited(&base, "read<2>(", "read<0>(");
    let n0 = edited(&base, "Window<1>", "Window<0>");
    for first in ["read", "answer"] {
        let mut db = database();
        let file = input(&mut db, &path, &base);
        for (source, valid) in [
            (base.clone(), true),
            (m0.clone(), false),
            (n0.clone(), false),
            (base.clone(), true),
        ] {
            file.set_text(&mut db).to(source.clone());
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), valid, "{first}: {warm}");
            let mut fresh = database();
            let fresh_file = input(&mut fresh, &path, &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
            if valid {
                assert_eq!(evaluate(&db, file, "answer"), "42");
            }
        }
    }
}

#[test]
fn method_requirement_edits_match_fresh_databases() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    let (path, base) =
        fixture("relational/method_requirement_edits_match_fresh_databases/edits.fe");
    let m1 = edited(&base, "take<2>()", "take<1>()");
    let n0 = edited(&base, "Window<1>", "Window<0>");
    for first in ["take", "answer"] {
        let mut db = database();
        let file = input(&mut db, &path, &base);
        for (source, valid) in [
            (base.clone(), true),
            (m1.clone(), false),
            (n0.clone(), false),
            (base.clone(), true),
        ] {
            file.set_text(&mut db).to(source.clone());
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), valid, "{first}: {warm}");
            let mut fresh = database();
            let fresh_file = input(&mut fresh, &path, &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
            if valid {
                assert_eq!(evaluate(&db, file, "answer"), "42");
            }
        }
    }
}

#[test]
fn method_type_parameters_and_self_constants_keep_their_identity() {
    use salsa::Setter;
    let mut db = database();
    let (type_param_path, type_param_base) = fixture(
        "relational/method_type_parameters_and_self_constants_keep_their_identity/type_param.fe",
    );
    let type_param_no = edited(&type_param_base, "Holder<Yes>", "Holder<No>");
    let type_param_file = input(&mut db, &type_param_path, &type_param_base);
    for (source, valid) in [
        (type_param_base.clone(), true),
        (type_param_no.clone(), false),
    ] {
        type_param_file.set_text(&mut db).to(source);
        let errors = diagnostics(&db, type_param_file);
        assert_eq!(errors.is_empty(), valid, "{errors}");
        if valid {
            assert_eq!(evaluate(&db, type_param_file, "answer"), "42");
        } else {
            assert!(
                errors.contains("const requirement") && errors.contains("false"),
                "{errors}"
            );
        }
    }

    let (self_const_path, self_const_base) = fixture(
        "relational/method_type_parameters_and_self_constants_keep_their_identity/self_const.fe",
    );
    let self_const_zero = edited(&self_const_base, "Window<1>", "Window<0>");
    let self_const_file = input(&mut db, &self_const_path, &self_const_base);
    for (source, valid) in [
        (self_const_base.clone(), true),
        (self_const_zero.clone(), false),
    ] {
        self_const_file.set_text(&mut db).to(source);
        let errors = diagnostics(&db, self_const_file);
        assert_eq!(errors.is_empty(), valid, "{errors}");
        if valid {
            assert_eq!(evaluate(&db, self_const_file, "answer"), "42");
        } else {
            assert!(
                errors.contains("const requirement") && errors.contains("false"),
                "{errors}"
            );
        }
    }
}

#[test]
fn method_formation_cycles_do_not_become_evidence() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    let (path, base) =
        fixture("relational/method_formation_cycles_do_not_become_evidence/cycle.fe");
    let non_cyclic = edited(&base, "where Window<N>::allowed()", "where true");
    for first in ["allowed", "answer"] {
        let mut db = database();
        let file = input(&mut db, &path, &base);
        for (source, cyclic) in [
            (base.clone(), true),
            (non_cyclic.clone(), false),
            (base.clone(), true),
        ] {
            file.set_text(&mut db).to(source.clone());
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), !cyclic, "{warm}");
            if cyclic {
                assert!(warm.contains("const requirement"), "{warm}");
            } else {
                assert_eq!(evaluate(&db, file, "answer"), "true");
            }
            let mut fresh = database();
            let fresh_file = input(&mut fresh, &path, &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
        }
    }
}
