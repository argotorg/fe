use dir_test::{Fixture, dir_test};

use test_utils::snap_test;

mod test_runner;
use test_runner::*;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/no_recovery/items",
    glob: "*.fe"
)]
fn test_item_list(fixture: Fixture<&str>) {
    let runner = TestRunner::item_list(false).set_recovery_mode(false);
    let errors = runner.run(fixture.content()).unwrap_err();
    let output = errors
        .iter()
        .map(|e| format!("{}@{:?}", e.msg(), e.range()))
        .collect::<Vec<_>>()
        .join("\n");
    snap_test!(output, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/no_recovery/stmts",
    glob: "*.fe"
)]
fn test_stmt(fixture: Fixture<&str>) {
    let runner = TestRunner::stmt_list(false).set_recovery_mode(false);
    let errors = runner.run(fixture.content()).unwrap_err();
    let output = errors
        .iter()
        .map(|e| format!("{}@{:?}", e.msg(), e.range()))
        .collect::<Vec<_>>()
        .join("\n");
    snap_test!(output, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/no_recovery/exprs",
    glob: "*.fe"
)]
fn test_expr(fixture: Fixture<&str>) {
    let runner = TestRunner::expr_list(false).set_recovery_mode(false);
    let errors = runner.run(fixture.content()).unwrap_err();
    let output = errors
        .iter()
        .map(|e| format!("{}@{:?}", e.msg(), e.range()))
        .collect::<Vec<_>>()
        .join("\n");
    snap_test!(output, fixture.path());
}

#[cfg(target_family = "wasm")]
mod wasm {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/no_recovery/items",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_item_list(fixture: Fixture<&str>) {
        TestRunner::item_list(false)
            .set_recovery_mode(false)
            .run(fixture.content());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/no_recovery/stmts",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_stmt(fixture: Fixture<&str>) {
        TestRunner::stmt_list(false)
            .set_recovery_mode(false)
            .run(fixture.content());
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files/no_recovery/exprs",
        glob: "*.fe"
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn test_expr(fixture: Fixture<&str>) {
        TestRunner::expr_list(false)
            .set_recovery_mode(false)
            .run(fixture.content());
    }
}
