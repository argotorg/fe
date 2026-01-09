use fe_hir::analysis::{
    analysis_pass::AnalysisPassManager,
    ty::{AdtDefAnalysisPass, FuncAnalysisPass},
};
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
    // pass_manager.add_module_pass(Box::new(ParsingPass {}));
    // pass_manager.add_module_pass(Box::new(ImportAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(AdtDefAnalysisPass {}));
    // pass_manager.add_module_pass(Box::new(TypeAliasAnalysisPass {}));
    // pass_manager.add_module_pass(Box::new(TraitAnalysisPass {}));
    // pass_manager.add_module_pass(Box::new(ImplAnalysisPass {}));
    // pass_manager.add_module_pass(Box::new(ImplTraitAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(FuncAnalysisPass {}));
    // pass_manager.add_module_pass(Box::new(BodyAnalysisPass {}));
    pass_manager
}
