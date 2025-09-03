use common::InputDb;
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use fe_semantic_query::SemanticIndex;
use hir::lower::{map_file_to_mod, parse_file_impl};
use hir_analysis::name_resolution::{resolve_with_policy, DomainPreference, PathResErrorKind};
use parser::SyntaxNode;
use test_utils::snap_test;
use url::Url;

fn collect_positions(root: &SyntaxNode) -> Vec<parser::TextSize> {
    use parser::{ast, ast::prelude::AstNode, SyntaxKind};
    fn walk(node: &SyntaxNode, positions: &mut Vec<parser::TextSize>) {
        match node.kind() {
            SyntaxKind::Ident => positions.push(node.text_range().start()),
            SyntaxKind::Path => {
                if let Some(path) = ast::Path::cast(node.clone()) {
                    for segment in path.segments() {
                        if let Some(ident) = segment.ident() {
                            positions.push(ident.text_range().start());
                        }
                    }
                }
            }
            SyntaxKind::PathType => {
                if let Some(pt) = ast::PathType::cast(node.clone()) {
                    if let Some(path) = pt.path() {
                        for segment in path.segments() {
                            if let Some(ident) = segment.ident() {
                                positions.push(ident.text_range().start());
                            }
                        }
                    }
                }
            }
            SyntaxKind::FieldExpr => {
                if let Some(fe) = ast::FieldExpr::cast(node.clone()) {
                    if let Some(tok) = fe.field_name() {
                        positions.push(tok.text_range().start());
                    }
                }
            }
            SyntaxKind::UsePath => {
                if let Some(up) = ast::UsePath::cast(node.clone()) {
                    for seg in up.into_iter() {
                        if let Some(tok) = seg.ident() { positions.push(tok.text_range().start()); }
                    }
                }
            }
            _ => {}
        }
        for child in node.children() {
            walk(&child, positions);
        }
    }
    let mut out = Vec::new();
    walk(root, &mut out);
    out.sort();
    out.dedup();
    out
}

fn line_col_from_cursor(cursor: parser::TextSize, s: &str) -> (usize, usize) {
    let mut line = 0usize;
    let mut col = 0usize;
    for (i, ch) in s.chars().enumerate() {
        if i == Into::<usize>::into(cursor) {
            return (line, col);
        }
        if ch == '\n' { line += 1; col = 0; } else { col += 1; }
    }
    (line, col)
}

fn format_snapshot(content: &str, lines: &[String]) -> String {
    let header = content
        .lines()
        .enumerate()
        .map(|(i, l)| format!("{i:?}: {l}"))
        .collect::<Vec<_>>()
        .join("\n");
    let body = lines.join("\n");
    format!("{header}\n---\n{body}")
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/test_files/goto", glob: "*.fe")]
fn test_goto_snapshot(fixture: Fixture<&str>) {
    if fixture.path().ends_with("use_paths.fe") {
        return;
    }
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path(fixture.path()).unwrap(),
        Some(fixture.content().to_string()),
    );
    let top_mod = map_file_to_mod(&db, file);

    // Parse and collect identifier/path-segment positions
    let green = parse_file_impl(&db, top_mod);
    let root = SyntaxNode::new_root(green);
    let positions = collect_positions(&root);

    let mut lines = Vec::new();
    for cursor in positions {
        // Use SemanticIndex to pick segment subpath and check def candidates count
        let count = SemanticIndex::goto_candidates_at_cursor(&db, &db, top_mod, cursor).len();

        // Resolve pretty path(s) for readability
        let pretty = if let Some((path, scope, seg_idx, _)) = SemanticIndex::at_cursor(&db, top_mod, cursor) {
            let seg_path = path.segment(&db, seg_idx).unwrap_or(path);
            match resolve_with_policy(&db, seg_path, scope, hir_analysis::ty::trait_resolution::PredicateListId::empty_list(&db), DomainPreference::Either) {
                Ok(res) => vec![res.pretty_path(&db).unwrap_or("<unknown>".into())],
                Err(err) => match err.kind {
                    PathResErrorKind::NotFound { bucket, .. } => bucket.iter_ok().filter_map(|nr| nr.pretty_path(&db)).collect(),
                    PathResErrorKind::Ambiguous(vec) => vec.into_iter().filter_map(|nr| nr.pretty_path(&db)).collect(),
                    _ => vec![],
                }
            }
        } else { vec![] };

        if !pretty.is_empty() || count > 0 {
            let (line, col) = line_col_from_cursor(cursor, fixture.content());
            let joined = pretty.join("\n");
            lines.push(format!("cursor position ({line}, {col}), {count} defs -> {joined}"));
        }
    }

    let snapshot = format_snapshot(fixture.content(), &lines);
    snap_test!(snapshot, fixture.path());
}
