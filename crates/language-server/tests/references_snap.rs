use common::InputDb;
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use fe_semantic_query::SemanticIndex;
use hir::{lower::map_file_to_mod, span::{DynLazySpan, LazySpan}, SpannedHirDb};
use parser::SyntaxNode;
use test_utils::snap_test;
use url::Url;

// Collect cursor positions: identifiers, path/type path segments, field accessors
fn collect_positions(root: &SyntaxNode) -> Vec<parser::TextSize> {
    use parser::{ast, ast::prelude::AstNode, SyntaxKind};
    fn walk(node: &SyntaxNode, out: &mut Vec<parser::TextSize>) {
        match node.kind() {
            SyntaxKind::Ident => out.push(node.text_range().start()),
            SyntaxKind::Path => {
                if let Some(path) = ast::Path::cast(node.clone()) {
                    for seg in path.segments() {
                        if let Some(id) = seg.ident() { out.push(id.text_range().start()); }
                    }
                }
            }
            SyntaxKind::PathType => {
                if let Some(pt) = ast::PathType::cast(node.clone()) {
                    if let Some(path) = pt.path() {
                        for seg in path.segments() {
                            if let Some(id) = seg.ident() { out.push(id.text_range().start()); }
                        }
                    }
                }
            }
            SyntaxKind::FieldExpr => {
                if let Some(fe) = ast::FieldExpr::cast(node.clone()) {
                    if let Some(tok) = fe.field_name() { out.push(tok.text_range().start()); }
                }
            }
            _ => {}
        }
        for ch in node.children() { walk(&ch, out); }
    }
    let mut v = Vec::new();
    walk(root, &mut v);
    v.sort(); v.dedup(); v
}

fn line_col_from_cursor(cursor: parser::TextSize, s: &str) -> (usize, usize) {
    let mut line=0usize; let mut col=0usize;
    for (i, ch) in s.chars().enumerate() {
        if i == Into::<usize>::into(cursor) { return (line, col); }
        if ch == '\n' { line+=1; col=0; } else { col+=1; }
    }
    (line, col)
}

fn format_snapshot(content: &str, lines: &[String]) -> String {
    let header = content.lines().enumerate().map(|(i,l)| format!("{i:?}: {l}")).collect::<Vec<_>>().join("\n");
    let body = lines.join("\n");
    format!("{header}\n---\n{body}")
}

fn to_lsp_location_from_span(db: &dyn InputDb, span: common::diagnostics::Span) -> Option<async_lsp::lsp_types::Location> {
    let url = span.file.url(db)?;
    let text = span.file.text(db);
    let starts: Vec<usize> = text.lines().scan(0, |st, ln| { let o=*st; *st+=ln.len()+1; Some(o)}).collect();
    let idx = |off: parser::TextSize| starts.binary_search(&Into::<usize>::into(off)).unwrap_or_else(|n| n.saturating_sub(1));
    let sl = idx(span.range.start()); let el = idx(span.range.end());
    let sc: usize = Into::<usize>::into(span.range.start()) - starts[sl];
    let ec: usize = Into::<usize>::into(span.range.end()) - starts[el];
    Some(async_lsp::lsp_types::Location{ uri:url, range: async_lsp::lsp_types::Range{
        start: async_lsp::lsp_types::Position::new(sl as u32, sc as u32), end: async_lsp::lsp_types::Position::new(el as u32, ec as u32)
    }})
}

fn pretty_enclosing(db: &dyn SpannedHirDb, top_mod: hir::hir_def::TopLevelMod, off: parser::TextSize) -> Option<String> {
    let items = top_mod.scope_graph(db).items_dfs(db);
    let mut best: Option<(hir::hir_def::ItemKind, u32)> = None;
    for it in items {
        let lazy = DynLazySpan::from(it.span());
        let Some(sp) = lazy.resolve(db) else { continue };
        if sp.range.contains(off) {
            let w: u32 = (sp.range.end() - sp.range.start()).into();
            match best { None => best=Some((it,w)), Some((_,bw)) if w< bw => best=Some((it,w)), _=>{} }
        }
    }
    best.and_then(|(it,_)| hir::hir_def::scope_graph::ScopeId::from_item(it).pretty_path(db))
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/test_files", glob: "goto_*.fe")]
fn refs_snapshot_for_goto_fixtures(fx: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(&mut db, Url::from_file_path(fx.path()).unwrap(), Some(fx.content().to_string()));
    let top = map_file_to_mod(&db, file);
    let green = hir::lower::parse_file_impl(&db, top);
    let root = SyntaxNode::new_root(green);
    let positions = collect_positions(&root);

    let mut lines = Vec::new();
    for cur in positions {
        let refs = SemanticIndex::find_references_at_cursor(&db, &db, top, cur);
        if refs.is_empty() { continue; }
        // Group refs by file basename with optional enclosing symbol
        use std::collections::{BTreeMap, BTreeSet};
        let mut grouped: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for r in refs {
            if let Some(sp) = r.span.resolve(&db) {
                if let Some(loc) = to_lsp_location_from_span(&db, sp.clone()) {
                    let path = loc.uri.path();
                    let fname = std::path::Path::new(path).file_name().and_then(|s| s.to_str()).unwrap_or(path);
                    let enc = pretty_enclosing(&db, top, sp.range.start());
                    let entry = match enc { Some(e) => format!("{} @ {}:{}", e, loc.range.start.line, loc.range.start.character),
                                            None => format!("{}:{}", loc.range.start.line, loc.range.start.character) };
                    grouped.entry(fname.to_string()).or_default().insert(entry);
                }
            }
        }
        let mut parts = Vec::new();
        for (f, set) in grouped.iter() { parts.push(format!("{}: {}", f, set.iter().cloned().collect::<Vec<_>>().join("; "))); }
        let (l,c) = line_col_from_cursor(cur, fx.content());
        lines.push(format!("cursor ({l}, {c}): {} refs -> {}", grouped.values().map(|s| s.len()).sum::<usize>(), parts.join(" | ")));
    }

    let snapshot = format_snapshot(fx.content(), &lines);
    // Write refs snapshot alongside goto snapshot per file
    let orig = std::path::Path::new(fx.path());
    let stem = orig.file_stem().and_then(|s| s.to_str()).unwrap_or("snapshot");
    let refs_name = format!("refs_{}.fe", stem);
    let refs_path = orig.with_file_name(refs_name);
    snap_test!(snapshot, refs_path.to_str().unwrap());
}

