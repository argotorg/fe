use dir_test::{Fixture, dir_test};
use fe_hir::hir_def::{LitKind, StringId};
use fe_hir::lower::map_file_to_mod;
use fe_hir::test_db::HirAnalysisTestDb;
use parser::{
    RecoveryMode, SyntaxNode,
    ast::{self, prelude::AstNode},
    parse_source_file,
};
use test_utils::snap_test;

/// Tests HIR pretty-printing by emitting all items and snapshotting the output.
/// Roundtrips are skipped because contracts desugar into helper modules.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pretty_print",
    glob: "**/*.fe"
)]
fn hir_pretty_print(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(fixture.path().into(), fixture.content());
    let top_mod = map_file_to_mod(&db, file);

    let output = top_mod.pretty_print(&db);

    snap_test!(output, fixture.path());
}

#[test]
fn string_literal_printing_roundtrips_decoded_values() {
    for (spelling, value, canonical) in [
        (r#""""#, "", r#""""#),
        (r#""\"\\\n\r\t""#, "\"\\\n\r\t", r#""\"\\\n\r\t""#),
        (r#""\\n""#, "\\n", r#""\\n""#),
        (r#""tail\\""#, "tail\\", r#""tail\\""#),
        (r#""'é🦀""#, "'é🦀", r#""'é🦀""#),
        ("\"e\u{301}\"", "e\u{301}", "\"e\u{301}\""),
        (
            "\"first\nsecond\r\t\"",
            "first\nsecond\r\t",
            r#""first\nsecond\r\t""#,
        ),
        ("\"\0\u{1b}\u{85}\"", "\0\u{1b}\u{85}", "\"\0\u{1b}\u{85}\""),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let literal = LitKind::String(StringId::from_str(&db, value));
        assert_eq!(literal.pretty_print(&db), canonical);

        // Exercise the expression, pattern and attribute callers of the printer.
        let source = format!(
            "#[example(value = {spelling})]\nfn literals() {{\nlet text = {spelling}\nmatch text {{\n{spelling} => (),\n_ => (),\n}}\n}}"
        );
        let (_, errors) = parse_source_file(&source, RecoveryMode::Recover);
        assert!(errors.is_empty(), "{source:?}: {errors:?}");
        let file = db.new_stand_alone("string_roundtrip.fe".into(), &source);
        let printed = map_file_to_mod(&db, file).pretty_print(&db);
        let (green, errors) = parse_source_file(&printed, RecoveryMode::Recover);
        assert!(errors.is_empty(), "{printed:?}: {errors:?}");
        let values: Vec<_> = SyntaxNode::new_root(green)
            .descendants()
            .filter_map(ast::Lit::cast)
            .filter_map(|lit| match lit.kind() {
                ast::LitKind::String(string) => Some(string.value().unwrap()),
                _ => None,
            })
            .collect();
        assert_eq!(values, [value; 3], "{printed:?}");
        assert_eq!(printed.matches(canonical).count(), 3, "{printed:?}");
    }
}
