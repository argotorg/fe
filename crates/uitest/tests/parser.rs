use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use hir::analysis::analysis_pass::{AnalysisPassManager, ParsingPass};
use test_utils::snap_test;

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/parser",
    glob: "*.fe"
)]
fn run_parser(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        url::Url::from_file_path(fixture.path()).expect("path should be absolute"),
        Some(fixture.content().to_string()),
    );

    let top_mod = db.top_mod(file);

    let diags = db.run_on_file_with_pass_manager(top_mod, init_parser_pass());
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

fn init_parser_pass() -> AnalysisPassManager {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass(Box::new(ParsingPass {}));
    pass_manager
}

#[cfg(target_family = "wasm")]
mod wasm {
    use super::*;
    use test_utils::url_utils::UrlExt;
    use url::Url;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/parser",
        glob: "*.fe",
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn run_parser(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            <Url as UrlExt>::from_file_path_lossy(fixture.path()),
            Some(fixture.content().to_string()),
        );

        let top_mod = db.top_mod(file);
        db.run_on_top_mod(top_mod);
    }
}
