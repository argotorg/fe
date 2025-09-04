use dir_test::{Fixture, dir_test};

use test_utils::snap_test;

mod test_runner;
use test_runner::*;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/items",
    glob: "*.fe"
)]
fn test_item_list(fixture: Fixture<&str>) {
    let runner = TestRunner::item_list(true);
    let node = format! {"{:#?}", runner.run(fixture.content())};
    snap_test!(node, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/structs",
    glob: "*.fe"
)]
fn test_struct(fixture: Fixture<&str>) {
    let runner = TestRunner::item_list(true);
    let node = format! {"{:#?}", runner.run(fixture.content())};
    snap_test!(node, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/stmts",
    glob: "*.fe"
)]
fn test_stmt(fixture: Fixture<&str>) {
    let runner = TestRunner::stmt_list(true);
    let node = format! {"{:#?}", runner.run(fixture.content())};
    snap_test!(node, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/exprs",
    glob: "*.fe"
    postfix: "expr"
)]
fn test_expr(fixture: Fixture<&str>) {
    let runner = TestRunner::expr_list(true);
    let node = format! {"{:#?}", runner.run(fixture.content())};
    snap_test!(node, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/pats",
    glob: "*.fe"
)]
fn test_pat(fixture: Fixture<&str>) {
    let runner = TestRunner::pat_list(true);
    let node = format! {"{:#?}", runner.run(fixture.content())};
    snap_test!(node, fixture.path());
}

#[cfg(target_family = "wasm")]
mod wasm {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[dir_test::dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/items",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_item_list(fixture: dir_test::Fixture<&str>) {
        TestRunner::item_list(true).run(fixture.content());
    }

    #[dir_test::dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/structs",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_struct(fixture: dir_test::Fixture<&str>) {
        TestRunner::item_list(true).run(fixture.content());
    }

    #[dir_test::dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/stmts",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_stmt(fixture: dir_test::Fixture<&str>) {
        TestRunner::stmt_list(true).run(fixture.content());
    }

    #[dir_test::dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/exprs",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_expr(fixture: dir_test::Fixture<&str>) {
        TestRunner::expr_list(true).run(fixture.content());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/pats",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_pat(fixture: Fixture<&str>) {
        TestRunner::pat_list(true).run(fixture.content());
    }
}
