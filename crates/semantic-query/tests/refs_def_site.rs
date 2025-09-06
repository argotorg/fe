use common::InputDb;
use driver::DriverDataBase;
use fe_semantic_query::SemanticQuery;
use hir::lower::map_file_to_mod;
use hir::span::LazySpan as _;
use url::Url;

fn line_col_from_offset(text: &str, offset: parser::TextSize) -> (usize, usize) {
    let mut line = 0usize;
    let mut col = 0usize;
    for (i, ch) in text.chars().enumerate() {
        if i == Into::<usize>::into(offset) {
            return (line, col);
        }
        if ch == '\n' { line += 1; col = 0; } else { col += 1; }
    }
    (line, col)
}

#[test]
fn def_site_method_refs_include_ufcs() {
    // Load the existing fixture used by snapshots
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("test_files/methods_ufcs.fe");
    let content = std::fs::read_to_string(&fixture_path).expect("fixture present");

    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::from_file_path(&fixture_path).unwrap(), Some(content.clone()));
    let top = map_file_to_mod(&db, file);

    // Cursor at def-site method name: resolve exactly from HIR
    let mut cursor: Option<parser::TextSize> = None;
    for it in top.all_items(&db).iter() {
        if let hir::hir_def::ItemKind::Func(f) = *it {
            if let Some(name) = f.name(&db).to_opt() {
                if name.data(&db) == "new" {
                    if let Some(sp) = f.span().name().resolve(&db) {
                        // place cursor inside the ident
                        cursor = Some((Into::<u32>::into(sp.range.start()) + 1).into());
                        break;
                    }
                }
            }
        }
    }
    let cursor = cursor.expect("found def-site method name");
    let refs = SemanticQuery::at_cursor(&db, top, cursor).find_references();
    assert!(refs.len() >= 3, "expected at least 3 refs, got {}", refs.len());

    // Collect (line,col) pairs for readability
    let mut pairs: Vec<(usize, usize)> = refs
        .iter()
        .filter_map(|r| r.span.resolve(&db))
        .map(|sp| line_col_from_offset(&content, sp.range.start()))
        .collect();
    pairs.sort();
    pairs.dedup();

    // Expect exact presence of def (3,9) and both UFCS call sites: (4,42) and (8,20)
    let expected = vec![(3, 9), (4, 42), (8, 20)];
    for p in expected.iter() {
        assert!(pairs.contains(p), "missing expected reference at {:?}, got {:?}", p, pairs);
    }
}

#[test]
fn round_trip_invariant_param_and_local() {
    let content = r#"
fn main(x: i32) -> i32 { let y = x; return y }
"#;
    let tmp = std::env::temp_dir().join("round_trip_param_local.fe");
    std::fs::write(&tmp, content).unwrap();
    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::from_file_path(&tmp).unwrap(), Some(content.to_string()));
    let top = map_file_to_mod(&db, file);

    // Cursor on parameter usage 'x'  
    let cursor_x = parser::TextSize::from(content.find(" x; ").unwrap() as u32 + 1);
    if let Some(key) = SemanticQuery::at_cursor(&db, top, cursor_x).symbol_key() {
        if let Some((_tm, def_span)) = SemanticQuery::definition_for_symbol(&db, key) {
            let refs = SemanticQuery::references_for_symbol(&db, top, key);
            let def_resolved = def_span.resolve(&db).expect("def span resolve");
            assert!(refs.iter().any(|r| r.span.resolve(&db) == Some(def_resolved.clone())), "param def-site missing from refs");
        }
    } else {
        panic!("failed to resolve symbol at cursor_x");
    }

    // Cursor on local 'y' usage (in return statement)  
    let cursor_y = parser::TextSize::from(content.rfind("return y").unwrap() as u32 + 7);
    if let Some(key) = SemanticQuery::at_cursor(&db, top, cursor_y).symbol_key() {
        if let Some((_tm, def_span)) = SemanticQuery::definition_for_symbol(&db, key) {
            let refs = SemanticQuery::references_for_symbol(&db, top, key);
            let def_resolved = def_span.resolve(&db).expect("def span resolve");
            assert!(refs.iter().any(|r| r.span.resolve(&db) == Some(def_resolved.clone())), "local def-site missing from refs");
        }
    } else {
        panic!("failed to resolve symbol at cursor_y");
    }
}
