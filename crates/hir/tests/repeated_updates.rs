use fe_hir::analysis::{
    analysis_pass::AnalysisPassManager,
    semantic::{get_or_build_semantic_instance, identity_semantic_instance_key},
    ty::ty_check::BodyOwner,
    ty::{AdtDefAnalysisPass, FuncAnalysisPass},
};
use fe_hir::hir_def::TopLevelMod;
use fe_hir::lower::map_file_to_mod;
use fe_hir::test_db::HirAnalysisTestDb;

use salsa::Setter;

#[test]
fn test_updated() {
    let mut db = HirAnalysisTestDb::default();
    let file_name = "file.fe";
    let versions = vec![
        r#"fn foo() {}"#,
        r#"use bla
           fn foo() {}"#,
        r#"use bla::bla
           fn foo() {}"#,
        r#"use bla::bla::bla
           fn foo() {}"#,
        r#"use bla::bla::bla::bla
           fn foo() {}"#,
    ];

    let file = db.new_stand_alone(file_name.into(), versions[0]);

    for version in versions {
        {
            let top_mod = map_file_to_mod(&db, file);
            let mut pass_manager = initialize_pass_manager();
            let _ = pass_manager.run_on_module(&db, top_mod);
        }

        {
            file.set_text(&mut db).to(version.into());
        }
    }
}

fn initialize_pass_manager() -> AnalysisPassManager {
    let mut pass_manager = AnalysisPassManager::new();
    // pass_manager.add_module_pass("Parsing", Box::new(ParsingPass {}));
    // pass_manager.add_module_pass("Import", Box::new(ImportAnalysisPass {}));
    pass_manager.add_module_pass("AdtDef", Box::new(AdtDefAnalysisPass {}));
    // pass_manager.add_module_pass("TypeAlias", Box::new(TypeAliasAnalysisPass {}));
    // pass_manager.add_module_pass("Trait", Box::new(TraitAnalysisPass {}));
    // pass_manager.add_module_pass("Impl", Box::new(ImplAnalysisPass {}));
    // pass_manager.add_module_pass("ImplTrait", Box::new(ImplTraitAnalysisPass {}));
    pass_manager.add_module_pass("Func", Box::new(FuncAnalysisPass {}));
    // pass_manager.add_module_pass("Body", Box::new(BodyAnalysisPass {}));
    pass_manager
}

fn cached_answers(
    db: &HirAnalysisTestDb,
    module: TopLevelMod<'_>,
    individual_first: bool,
) -> Vec<String> {
    for func in module.all_funcs(db) {
        if individual_first {
            for (idx, _) in func.params(db).enumerate() {
                assert!(func.arg_ty(db, idx).is_some());
            }
        } else {
            func.arg_tys(db);
        }
    }
    db.assert_no_diags(module);
    let mut answers = Vec::new();
    for &func in module.all_funcs(db) {
        let name = func.scope().pretty_path(db);
        let args = func.arg_tys(db);
        assert_eq!(args.len(), func.params(db).count());
        assert!(func.arg_ty(db, args.len()).is_none());
        for (idx, &arg) in args.iter().enumerate() {
            assert_eq!(Some(arg), func.arg_ty(db, idx));
            let ty = arg.instantiate_identity();
            answers.push(format!(
                "{name:?}({idx}): {} zst={}",
                ty.pretty_print(db),
                ty.is_zero_sized(db)
            ));
        }
        if func.body(db).is_some() {
            let instance = get_or_build_semantic_instance(
                db,
                identity_semantic_instance_key(db, BodyOwner::Func(func)),
            );
            for callee in instance
                .call_sites(db)
                .iter()
                .flatten()
                .filter_map(|site| site.callee)
            {
                let key = callee.key;
                let args = key
                    .subst(db)
                    .generic_args(db)
                    .iter()
                    .map(|ty| ty.pretty_print(db).to_string())
                    .collect::<Vec<_>>();
                answers.push(format!(
                    "{name:?} calls {:?} {args:?}",
                    key.owner(db).scope().pretty_path(db)
                ));
            }
        }
    }
    answers.sort();
    answers
}

#[test]
fn cached_parameter_types_follow_signature_layout_and_const_edits() {
    let versions = [
        "struct S {}\nfn target(_ x: S) {}\nfn caller() { target(S {}) }",
        "struct S { value: u256 }\nfn target(_ x: S) {}\nfn caller() { target(S { value: 1 }) }",
        "struct S {}\nfn target(_ x: S, _ y: bool) {}\nfn caller() { target(S {}, true) }",
        "struct S<const N: usize = _> {}\nfn target(_ x: S) {}\nfn caller() { target(S<3> {}) }",
        "struct S<const N: usize = _> {}\nfn target(_ x: S, _ y: S) {}\nfn caller() { target(S<3> {}, S<4> {}) }",
        "const N: usize = 2\nfn target(_ x: [u256; N]) {}\nfn caller() { target([1, 2]) }",
        "const N: usize = 3\nfn target(_ x: [u256; N]) {}\nfn caller() { target([1, 2, 3]) }",
        "enum S { Value(u256) }\nfn target(_ x: S) {}\nfn caller() { target(S::Value(1)) }",
        "enum S { Value(bool, u256) }\nfn target(_ x: S) {}\nfn caller() { target(S::Value(true, 1)) }",
    ];
    for individual_first in [false, true] {
        let mut db = HirAnalysisTestDb::default();
        let path = "cached_parameters.fe";
        let file = db.new_stand_alone(path.into(), versions[0]);
        for source in versions.iter().chain(versions.iter().rev()) {
            file.set_text(&mut db).to((*source).into());
            let (module, _) = db.top_mod(file);
            let actual = cached_answers(&db, module, individual_first);
            let mut fresh = HirAnalysisTestDb::default();
            let fresh_file = fresh.new_stand_alone(path.into(), source);
            let (fresh_module, _) = fresh.top_mod(fresh_file);
            assert_eq!(
                actual,
                cached_answers(&fresh, fresh_module, !individual_first),
                "{source}"
            );
        }
    }
}

#[test]
fn cached_trait_selection_follows_impl_and_associated_type_edits() {
    let versions = [
        "trait T { fn value() -> u256 { 1 } }\nimpl T for bool {}\nfn caller() -> u256 { <bool as T>::value() }",
        "trait T { fn value() -> u256 { 1 } }\nimpl T for bool { fn value() -> u256 { 2 } }\nfn caller() -> u256 { <bool as T>::value() }",
        "trait T { type Item\nfn take(_ value: Self::Item) {} }\nimpl T for bool { type Item = u256 }\nfn caller() { <bool as T>::take(1) }",
        "trait T { type Item\nfn take(_ value: Self::Item) {} }\nimpl T for bool { type Item = bool }\nfn caller() { <bool as T>::take(true) }",
    ];
    let mut db = HirAnalysisTestDb::default();
    let path = "cached_selection.fe";
    let file = db.new_stand_alone(path.into(), versions[0]);
    for source in versions.iter().chain(versions.iter().rev()) {
        file.set_text(&mut db).to((*source).into());
        let (module, _) = db.top_mod(file);
        let actual = cached_answers(&db, module, true);
        let mut fresh = HirAnalysisTestDb::default();
        let fresh_file = fresh.new_stand_alone(path.into(), source);
        let (fresh_module, _) = fresh.top_mod(fresh_file);
        assert_eq!(
            actual,
            cached_answers(&fresh, fresh_module, false),
            "{source}"
        );
    }
}
