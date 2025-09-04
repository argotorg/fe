use dir_test::{Fixture, dir_test};

use test_utils::snap_test;

mod test_runner;
use test_runner::*;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/error_recovery/items",
    glob: "*.fe"
)]
fn test_item_list(fixture: Fixture<&str>) {
    let runner = TestRunner::item_list(false);
    let node = format! {"{:#?}", runner.run(fixture.content())};
    snap_test!(node, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/error_recovery/stmts",
    glob: "*.fe"
)]
fn test_stmt(fixture: Fixture<&str>) {
    let runner = TestRunner::stmt_list(false);
    let node = format! {"{:#?}", runner.run(fixture.content())};
    snap_test!(node, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/error_recovery/exprs",
    glob: "*.fe"
)]
fn test_expr(fixture: Fixture<&str>) {
    let runner = TestRunner::expr_list(false);
    let node = format! {"{:#?}", runner.run(fixture.content())};
    snap_test!(node, fixture.path());
}

#[cfg(target_family = "wasm")]
mod wasm {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/error_recovery/items",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_item_list(fixture: Fixture<&str>) {
        TestRunner::item_list(false).run(fixture.content());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/error_recovery/stmts",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_stmt(fixture: Fixture<&str>) {
        TestRunner::stmt_list(false).run(fixture.content());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/error_recovery/exprs",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_expr(fixture: Fixture<&str>) {
        TestRunner::expr_list(false).run(fixture.content());
    }
}
