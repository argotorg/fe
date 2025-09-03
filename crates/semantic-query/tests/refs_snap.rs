use common::InputDb;
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use fe_semantic_query::SemanticIndex;
use hir::{lower::map_file_to_mod, span::{DynLazySpan, LazySpan}, SpannedHirDb};
use parser::SyntaxNode;
use test_utils::snap_test;
use url::Url;
mod support;
use support::{collect_positions, line_col_from_cursor, format_snapshot, to_lsp_location_from_span};

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

#[dir_test(dir: "$CARGO_MANIFEST_DIR/test_files/goto", glob: "*.fe")]
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
        use std::collections::{BTreeMap, BTreeSet};
        let mut grouped: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for r in refs {
            if let Some(sp) = r.span.resolve(&db) {
                if let Some(loc) = to_lsp_location_from_span(&db, sp.clone()) {
                    let path = loc.uri.path();
                    let fname = std::path::Path::new(path).file_name().and_then(|s| s.to_str()).unwrap_or(path);
                    let enc = pretty_enclosing(&db, top, sp.range.start());
                    let entry = match enc { Some(e) => format!("{} @ {}:{}", e, loc.range.start.line, loc.range.start.character), None => format!("{}:{}", loc.range.start.line, loc.range.start.character) };
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
    let orig = std::path::Path::new(fx.path());
    let stem = orig.file_stem().and_then(|s| s.to_str()).unwrap_or("snapshot");
    let refs_name = format!("refs_{}.fe", stem);
    let refs_path = orig.with_file_name(refs_name);
    snap_test!(snapshot, refs_path.to_str().unwrap());
}
