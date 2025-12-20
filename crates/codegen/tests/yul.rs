use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fe_codegen::emit_module_yul;
use hir::hir_def::ItemKind;
use test_utils::snap_test;
use url::Url;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures",
    glob: "*.fe"
)]
fn yul_snap(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file_url = Url::from_file_path(fixture.path()).expect("fixture path should be absolute");
    db.workspace().touch(
        &mut db,
        file_url.clone(),
        Some(fixture.content().to_string()),
    );
    let file = db
        .workspace()
        .get(&db, &file_url)
        .expect("file should be loaded");
    let top_mod = db.top_mod(file);

    // If the module contains any contracts, emit a desugared snapshot
    let has_contract = top_mod
        .children_non_nested(&db)
        .any(|item| matches!(item, ItemKind::Contract(_)));
    if has_contract {
        let desugared = top_mod.pretty_print_desugared(&db);
        let desugared_path = fixture.path().replace(".fe", ".desugared.fe");
        snap_test!(desugared, &desugared_path);
    }

    let output = match emit_module_yul(&db, top_mod) {
        Ok(yul) => yul,
        Err(err) => panic!("MIR ERROR: {err}"),
    };

    snap_test!(output, fixture.path());
}
