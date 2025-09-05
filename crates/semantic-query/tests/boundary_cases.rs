use common::InputDb;
use driver::DriverDataBase;
use fe_semantic_query::SemanticIndex;
use hir::lower::map_file_to_mod;
use hir::span::LazySpan as _;
use url::Url;

fn offset_of(text: &str, needle: &str) -> parser::TextSize {
    parser::TextSize::from(text.find(needle).expect("needle present") as u32)
}

// Boundary semantics are being finalized alongside half-open spans and selection policy.
// Ignored for now to keep the suite green while we land the analysis bridge.
#[test]
fn local_param_boundaries() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("test-fixtures")
        .join("local_param_boundary.fe");
    let content = std::fs::read_to_string(&fixture_path).unwrap();
    let tmp = std::env::temp_dir().join("boundary_local_param.fe");
    std::fs::write(&tmp, &content).unwrap();
    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::from_file_path(&tmp).unwrap(), Some(content.clone()));
    let top = map_file_to_mod(&db, file);

    // First character of local 'y' usage (the 'y' in 'return y')  
    let start_y = offset_of(&content, "return y") + parser::TextSize::from(7u32); // 7 = length of "return "
    let key_start = SemanticIndex::symbol_identity_at_cursor(&db, top, start_y)
        .expect("symbol at start of y");

    // Last character of 'y' usage is same as start here (single-char ident)
    let last_y = start_y; // single char
    let key_last = SemanticIndex::symbol_identity_at_cursor(&db, top, last_y)
        .expect("symbol at last char of y");
    assert_eq!(key_start, key_last, "identity should be stable across y span");

    // Immediately after local 'y' (half-open end): should not select
    let after_y = last_y + parser::TextSize::from(1u32);
    
    let symbol_after = SemanticIndex::symbol_identity_at_cursor(&db, top, after_y);
    
    assert!(symbol_after.is_none(), "no symbol immediately after y");

    // Parameter usage 'x' resolves to parameter identity
    let x_use = offset_of(&content, " x") + parser::TextSize::from(1u32);
    let key_param = SemanticIndex::symbol_identity_at_cursor(&db, top, x_use)
        .expect("symbol for param x usage");
    // Def span should match a param header in the function
    let (_tm, def_span) = SemanticIndex::definition_for_symbol(&db, key_param).expect("def for param");
    let def_res = def_span.resolve(&db).expect("resolve def span");
    let name_text = &content.as_str()[(Into::<usize>::into(def_res.range.start()))..(Into::<usize>::into(def_res.range.end()))];
    assert_eq!(name_text, "x");
}

#[test]
fn shadowing_param_by_local() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("test-fixtures")
        .join("shadow_local.fe");
    let content = std::fs::read_to_string(&fixture_path).unwrap();
    let tmp = std::env::temp_dir().join("boundary_shadow_local.fe");
    std::fs::write(&tmp, &content).unwrap();
    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::from_file_path(&tmp).unwrap(), Some(content.clone()));
    let top = map_file_to_mod(&db, file);

    // Cursor at the final 'x' usage should resolve to the local, not the param
    let use_x = offset_of(&content, "return x") + parser::TextSize::from(7u32); // 7 = length of "return "
    let key_use = SemanticIndex::symbol_identity_at_cursor(&db, top, use_x)
        .expect("symbol at x usage");

    // Def for resolved key should be the local 'x' binding
    let (_tm, def_span) = SemanticIndex::definition_for_symbol(&db, key_use).expect("def for x");
    let def_res = def_span.resolve(&db).expect("resolve def");
    let def_text = &content.as_str()[(Into::<usize>::into(def_res.range.start()))..(Into::<usize>::into(def_res.range.end()))];
    assert_eq!(def_text, "x");

    // Ensure that the key does not equal the param identity
    let param_pos = offset_of(&content, "(x:") + parser::TextSize::from(1u32);
    let param_key = SemanticIndex::symbol_identity_at_cursor(&db, top, param_pos).expect("param key");
    assert_ne!(format!("{:?}", key_use), format!("{:?}", param_key));
}
