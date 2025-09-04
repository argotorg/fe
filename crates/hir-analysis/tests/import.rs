mod test_db;
use std::path::Path;

use dir_test::{Fixture, dir_test};
use fe_hir_analysis::{
    analysis_pass::ModuleAnalysisPass,
    name_resolution::{ImportAnalysisPass, NameDerivation, ResolvedImports, resolve_imports},
};
use hir::hir_def::Use;
use rustc_hash::FxHashMap;
use test_db::{HirAnalysisTestDb, HirPropertyFormatter};
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/imports",
    glob: "*.fe"
)]
fn import_standalone(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, mut prop_formatter) = db.top_mod(file);

    db.assert_no_diags(top_mod);

    let mut pass = ImportAnalysisPass {};
    let resolved_imports = resolve_imports(&db, top_mod.ingot(&db));
    let diags = pass.run_on_module(&db, top_mod);
    if !diags.is_empty() {
        panic!("Failed to resolve imports");
    }

    let res = format_imports(&db, &mut prop_formatter, &resolved_imports.1);
    snap_test!(res, fixture.path());
}

fn format_imports<'db>(
    db: &'db HirAnalysisTestDb,
    prop_formatter: &mut HirPropertyFormatter<'db>,
    imports: &ResolvedImports<'db>,
) -> String {
    let mut use_res_map: FxHashMap<Use, Vec<String>> = FxHashMap::default();

    for name_resolved in imports.named_resolved.values().flat_map(|r| r.values()) {
        for res in name_resolved.iter_ok() {
            match res.derivation {
                NameDerivation::NamedImported(use_) => use_res_map
                    .entry(use_)
                    .or_default()
                    .push(res.pretty_path(db).unwrap()),
                _ => unreachable!(),
            }
        }
    }

    for (_, glob_set) in imports.glob_resolved.iter() {
        for (&use_, res_set_with_ident) in glob_set.iter() {
            for (ident, res_set) in res_set_with_ident.iter() {
                let ident = ident.data(db);
                for res in res_set {
                    let def_path = res.pretty_path(db).unwrap();
                    let resolved = format!("{def_path} as {ident}");
                    use_res_map.entry(use_).or_default().push(resolved)
                }
            }
        }
    }
    for (use_, mut values) in use_res_map.into_iter() {
        let use_span = use_.span().into();
        values.sort_unstable();
        let imported_names = values.join(" | ");
        prop_formatter.push_prop(use_.top_mod(db), use_span, imported_names)
    }

    prop_formatter.finish(db)
}
