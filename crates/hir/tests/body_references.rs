mod test_db;
use std::path::Path;

use dir_test::{Fixture, dir_test};
use fe_hir::{
    core::semantic::reference::{HasReferences, ReferenceView, Target},
    hir_def::ItemKind,
};
use test_db::HirAnalysisTestDb;
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/body_references",
    glob: "*.fe"
)]
fn body_references(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, mut prop_formatter) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // Find all functions and collect their body references
    let scope_graph = top_mod.scope_graph(&db);
    for item in scope_graph.items_dfs(&db) {
        if let ItemKind::Func(func) = item
            && let Some(body) = func.body(&db)
        {
            let refs = body.references(&db);
            for r in refs {
                match r {
                    ReferenceView::Path(pv) => {
                        // Get the path name for display
                        let path_name = pv
                            .path
                            .ident(&db)
                            .to_opt()
                            .map(|id| id.data(&db).to_string())
                            .unwrap_or_else(|| "<complex>".to_string());

                        // Try to resolve the target
                        let target_desc = match pv.target(&db) {
                            Some(Target::Scope(scope)) => scope.kind_name().to_string(),
                            Some(Target::Local { .. }) => "local".to_string(),
                            None => "unresolved".to_string(),
                        };

                        let annotation = format!("{} -> {}", path_name, target_desc);
                        prop_formatter.push_prop(top_mod, pv.span(), annotation);
                    }
                    ReferenceView::FieldAccess(fv) => {
                        let target_desc = match fv.target(&db) {
                            Some(scope) => format!("field -> {}", scope.kind_name()),
                            None => "field access".to_string(),
                        };
                        prop_formatter.push_prop(top_mod, fv.span(), target_desc);
                    }
                    ReferenceView::MethodCall(mv) => {
                        let target_desc = match mv.target(&db) {
                            Some(scope) => format!("method -> {}", scope.kind_name()),
                            None => "method call".to_string(),
                        };
                        prop_formatter.push_prop(top_mod, mv.span(), target_desc);
                    }
                    ReferenceView::UsePath(_) => {
                        // Use paths are handled at item level, not body level
                    }
                }
            }
        }
    }

    let res = prop_formatter.finish(&db);
    snap_test!(res, fixture.path());
}
