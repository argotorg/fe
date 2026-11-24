use dir_test::{Fixture, dir_test};
use fe_parser::{RecoveryMode, SyntaxKind, SyntaxNode, parse_source_file};

use test_utils::{normalize::normalize_newlines, snap_test};

mod test_runner;
use test_runner::*;

#[test]
fn trait_items_and_trait_family_bounds_after_where_predicates() {
    for (source, traits, families) in [
        ("struct Wrapper<T>(T) where T: Copy\ntrait Next {}", 1, 0),
        (
            "struct Wrapper<T>(T) where T: Copy,\ntrait Next: Parent {}",
            1,
            0,
        ),
        (
            "struct Wrapper<T>(T) where T: Copy,\ntrait Family: Bound",
            0,
            1,
        ),
        (
            "struct Wrapper<T> where T: Copy,\ntrait Family: Bound {}",
            0,
            1,
        ),
    ] {
        let (green, errors) = parse_source_file(source, RecoveryMode::Recover);
        assert!(errors.is_empty(), "{source}\n{errors:?}");
        let root = SyntaxNode::new_root(green);
        assert_eq!(root.to_string(), source);
        for (kind, count) in [
            (SyntaxKind::Trait, traits),
            (SyntaxKind::TraitFamilyType, families),
        ] {
            assert_eq!(
                root.descendants()
                    .filter(|node| node.kind() == kind)
                    .count(),
                count,
                "{source}",
            );
        }
    }
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/items",
    glob: "*.fe"
)]
fn test_item_list(fixture: Fixture<&str>) {
    let runner = TestRunner::item_list(true);
    let (cst, _) = runner.run(fixture.content());
    let node = format!("{cst:#?}");
    assert_eq!(normalize_newlines(fixture.content()), cst.to_string());
    snap_test!(node, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/structs",
    glob: "*.fe"
)]
fn test_struct(fixture: Fixture<&str>) {
    let runner = TestRunner::item_list(true);
    let (cst, _) = runner.run(fixture.content());
    let node = format!("{cst:#?}");
    assert_eq!(normalize_newlines(fixture.content()), cst.to_string());
    snap_test!(node, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/stmts",
    glob: "*.fe"
)]
fn test_stmt(fixture: Fixture<&str>) {
    let runner = TestRunner::stmt_list(true);
    let (cst, _) = runner.run(fixture.content());
    let node = format!("{cst:#?}");
    assert_eq!(normalize_newlines(fixture.content()), cst.to_string());
    snap_test!(node, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/exprs",
    glob: "*.fe"
    postfix: "expr"
)]
fn test_expr(fixture: Fixture<&str>) {
    let runner = TestRunner::expr_list(true);
    let (cst, _) = runner.run(fixture.content());
    let node = format!("{cst:#?}");
    assert_eq!(normalize_newlines(fixture.content()), cst.to_string());
    snap_test!(node, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/syntax_node/pats",
    glob: "*.fe"
)]
fn test_pat(fixture: Fixture<&str>) {
    let runner = TestRunner::pat_list(true);
    let (cst, _) = runner.run(fixture.content());
    let node = format!("{cst:#?}");
    assert_eq!(normalize_newlines(fixture.content()), cst.to_string());
    snap_test!(node, fixture.path());
}

#[test]
fn line_start_star_begins_a_dereference_statement() {
    let source = r#"
fn choose(value: u256, pointer: *u256) -> u256 {
    value
    *pointer
}
"#;
    let (syntax, _) = TestRunner::item_list(true).run(source);

    assert_eq!(
        syntax
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::ExprStmt)
            .count(),
        2,
        "{syntax:#?}",
    );
    assert_eq!(
        syntax
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::UnExpr)
            .count(),
        1,
        "{syntax:#?}",
    );
    assert!(
        syntax
            .descendants()
            .all(|node| node.kind() != SyntaxKind::BinExpr),
        "{syntax:#?}",
    );
}

#[test]
fn star_operator_continuations_remain_binary() {
    let source = r#"
fn update(var value: own u256, rhs: u256) -> u256 {
    value
        *= rhs
    value
        ** rhs
}
"#;
    let (syntax, _) = TestRunner::item_list(true).run(source);

    assert_eq!(
        syntax
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::AugAssignExpr)
            .count(),
        1,
        "{syntax:#?}",
    );
    assert_eq!(
        syntax
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::BinExpr)
            .count(),
        1,
        "{syntax:#?}",
    );
    assert!(
        syntax
            .descendants()
            .all(|node| node.kind() != SyntaxKind::UnExpr),
        "{syntax:#?}",
    );
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
        let (cst, _) = TestRunner::item_list(true).run(fixture.content());
        assert_eq!(normalize_newlines(fixture.content()), cst.to_string());
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
        let (cst, _) = TestRunner::item_list(true).run(fixture.content());
        assert_eq!(normalize_newlines(fixture.content()), cst.to_string());
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
        let (cst, _) = TestRunner::stmt_list(true).run(fixture.content());
        assert_eq!(normalize_newlines(fixture.content()), cst.to_string());
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
        let (cst, _) = TestRunner::expr_list(true).run(fixture.content());
        assert_eq!(normalize_newlines(fixture.content()), cst.to_string());
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
        let (cst, _) = TestRunner::pat_list(true).run(fixture.content());
        assert_eq!(normalize_newlines(fixture.content()), cst.to_string());
    }
}
