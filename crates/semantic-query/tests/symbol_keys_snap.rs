use common::InputDb;
use dir_test::{dir_test, Fixture};
use driver::DriverDataBase;
use fe_semantic_query::SemanticQuery;
use hir::{lower::map_file_to_mod, span::LazySpan as _, SpannedHirDb};
use hir_analysis::HirAnalysisDb;
use test_utils::snap::{codespan_render_defs_refs, line_col_from_cursor};
use test_utils::snap_test;
use url::Url;

fn symbol_label<'db>(
    db: &'db dyn SpannedHirDb,
    adb: &'db dyn HirAnalysisDb,
    key: &fe_semantic_query::SymbolKey<'db>,
) -> String {
    use fe_semantic_query::SymbolKey;
    match key {
        SymbolKey::Scope(sc) => sc.pretty_path(db).unwrap_or("<scope>".into()),
        SymbolKey::EnumVariant(v) => v.scope().pretty_path(db).unwrap_or("<variant>".into()),
        SymbolKey::Method(fd) => {
            // Show container scope path + method name
            let name = fd.name(adb).data(db);
            let path = fd.scope(adb).pretty_path(db).unwrap_or_default();
            if path.is_empty() {
                format!("method {}", name)
            } else {
                format!("{}::{}", path, name)
            }
        }
        SymbolKey::FuncParam(item, idx) => {
            let path = hir::hir_def::scope_graph::ScopeId::from_item(*item)
                .pretty_path(db)
                .unwrap_or_default();
            format!("param#{} of {}", idx, path)
        }
        SymbolKey::Local(func, _bkey) => {
            let path = func.scope().pretty_path(db).unwrap_or_default();
            format!("local in {}", path)
        }
    }
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/test_files", glob: "*.fe")]
fn symbol_keys_snapshot(fx: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path(fx.path()).unwrap(),
        Some(fx.content().to_string()),
    );
    let top = map_file_to_mod(&db, file);

    // Modules in this ingot
    let ing = top.ingot(&db);
    let view = ing.files(&db);
    let mut modules: Vec<hir::hir_def::TopLevelMod> = Vec::new();
    for (_u, f) in view.iter() {
        if f.kind(&db) == Some(common::file::IngotFileKind::Source) {
            modules.push(map_file_to_mod(&db, f));
        }
    }
    if modules.is_empty() {
        modules.push(top);
    }

    // Build symbol index across modules
    let map = SemanticQuery::build_symbol_index_for_modules(&db, &modules);

    // Stable ordering of symbol keys via labels
    let mut entries: Vec<(String, fe_semantic_query::SymbolKey)> = map
        .keys()
        .map(|k| (symbol_label(&db, &db, k), k.clone()))
        .collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut out = String::new();
    for (label, key) in entries {
        // Gather def
        let def_opt = SemanticQuery::definition_for_symbol(&db, key)
            .and_then(|(_tm, span)| span.resolve(&db));
        // Gather refs across modules
        let refs = SemanticQuery::references_for_symbol(&db, top, key.clone());
        let mut refs_by_file: std::collections::BTreeMap<
            common::file::File,
            Vec<common::diagnostics::Span>,
        > = Default::default();
        for r in refs {
            if let Some(sp) = r.span.resolve(&db) {
                refs_by_file.entry(sp.file).or_default().push(sp);
            }
        }

        out.push_str(&format!("Symbol: {}\n", label));

        // Group by files that have def or refs
        let mut files: Vec<common::file::File> = refs_by_file.keys().cloned().collect();
        if let Some(d) = def_opt.as_ref() {
            if !files.contains(&d.file) {
                files.push(d.file);
            }
        }
        // Stable order by file URL path
        files.sort_by_key(|f| f.url(&db).map(|u| u.path().to_string()).unwrap_or_default());

        for f in files {
            let content = f.text(&db);
            let name = f
                .url(&db)
                .and_then(|u| {
                    u.path_segments()
                        .and_then(|mut s| s.next_back())
                        .map(|s| s.to_string())
                })
                .unwrap_or_else(|| "<file>".into());
            let mut defs_same: Vec<(std::ops::Range<usize>, String)> = Vec::new();
            let mut refs_same: Vec<(std::ops::Range<usize>, String)> = Vec::new();

            if let Some(def) = def_opt.as_ref().filter(|d| d.file == f) {
                let s: usize = Into::<usize>::into(def.range.start());
                let e: usize = Into::<usize>::into(def.range.end());
                let (l0, c0) = line_col_from_cursor(def.range.start(), &content);
                let (l, c) = (l0 + 1, c0 + 1);
                // total refs count across all files
                let total_refs = refs_by_file.values().map(|v| v.len()).sum::<usize>();
                defs_same.push((
                    s..e,
                    format!("defined here @ {}:{} ({} refs)", l, c, total_refs),
                ));
            }

            if let Some(v) = refs_by_file.get(&f) {
                let mut spans = v.clone();
                spans.sort_by_key(|sp| (sp.range.start(), sp.range.end()));
                for sp in spans {
                    let s: usize = Into::<usize>::into(sp.range.start());
                    let e: usize = Into::<usize>::into(sp.range.end());
                    let (l0, c0) = line_col_from_cursor(sp.range.start(), &content);
                    let (l, c) = (l0 + 1, c0 + 1);
                    refs_same.push((s..e, format!("{}:{}", l, c)));
                }
            }

            // Render codespan for this file for this symbol
            let block = codespan_render_defs_refs(&name, &content, &defs_same, &refs_same);
            out.push_str(&block);
            out.push('\n');
        }

        out.push('\n');
    }

    let orig = std::path::Path::new(fx.path());
    let stem = orig
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("snapshot");
    let combined_name = format!("{}.snap", stem);
    let combined_path = orig.with_file_name(combined_name);
    snap_test!(out, combined_path.to_str().unwrap());
}
