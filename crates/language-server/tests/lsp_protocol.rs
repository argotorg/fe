use common::InputDb;
use driver::DriverDataBase;
use fe_semantic_query::SemanticQuery;
use hir::{lower::map_file_to_mod, span::LazySpan};
use url::Url;

/// Test that LSP protocol expects array response for multiple candidates
#[test]
fn lsp_goto_shape_array_for_multiple_candidates() {
    let mut db = DriverDataBase::default();
    let tmp = std::env::temp_dir().join("lsp_ambiguous.fe");
    // Multiple types with same name should result in array response format
    let content = r#"
mod a { pub struct T {} }
mod b { pub struct T {} }
use a::T
use b::T
fn f() { let _x: T }
"#;
    let file = db.workspace().touch(&mut db, Url::from_file_path(tmp).unwrap(), Some(content.to_string()));
    let top_mod = map_file_to_mod(&db, file);
    let cursor = content.rfind("T }").unwrap() as u32;
    let candidates = SemanticQuery::at_cursor(&db, top_mod, parser::TextSize::from(cursor)).goto_definition();
    
    // LSP protocol: multiple candidates should be returned as array for client to handle
    assert!(candidates.len() >= 2, "Expected multiple candidates for ambiguous symbol, got {}", candidates.len());
}

/// Test that LSP protocol expects scalar response for unambiguous candidates
#[test]
fn lsp_goto_shape_scalar_for_single_candidate() {
    let mut db = DriverDataBase::default();
    let tmp = std::env::temp_dir().join("lsp_unambiguous.fe");
    // Single unambiguous type should result in scalar response format
    let content = r#"
mod m { pub struct Foo {} }
fn f() { let _x: m::Foo }
"#;
    let file = db.workspace().touch(&mut db, Url::from_file_path(tmp).unwrap(), Some(content.to_string()));
    let top_mod = map_file_to_mod(&db, file);
    let cursor = content.find("Foo }").unwrap() as u32;
    let candidates = SemanticQuery::at_cursor(&db, top_mod, parser::TextSize::from(cursor)).goto_definition();
    
    // LSP protocol: single candidate should be returned as scalar for efficiency
    assert_eq!(candidates.len(), 1, "Expected single candidate for unambiguous symbol");
}

/// Test that references query returns appropriate data structure
#[test]
fn lsp_references_returns_structured_data() {
    let mut db = DriverDataBase::default();
    let tmp = std::env::temp_dir().join("lsp_references.fe");
    let content = r#"
struct Point { x: i32 }
fn main() { let p = Point { x: 42 }; let val = p.x; }
"#;
    let file = db.workspace().touch(&mut db, Url::from_file_path(tmp).unwrap(), Some(content.to_string()));
    let top_mod = map_file_to_mod(&db, file);
    let cursor = parser::TextSize::from(content.rfind("p.x").unwrap() as u32 + 2);
    let refs = SemanticQuery::at_cursor(&db, top_mod, cursor).find_references();
    
    // LSP protocol: references should include both definition and usage sites
    assert!(!refs.is_empty(), "Expected at least one reference location");
    
    // Each reference should have the necessary data for LSP Location conversion
    for r in &refs {
        assert!(r.span.resolve(&db).is_some(), "Reference should have resolvable span for LSP Location");
    }
}

/// Invariant: goto definition site must appear among references
#[test]
fn invariant_goto_def_in_references_local() {
    let mut db = DriverDataBase::default();
    let tmp = std::env::temp_dir().join("invariant_local_refs.fe");
    let content = r#"
fn f() { let x = 1; let _y = x; }
"#;
    let file = db.workspace().touch(&mut db, Url::from_file_path(&tmp).unwrap(), Some(content.to_string()));
    let top_mod = map_file_to_mod(&db, file);
    // Cursor on usage of x
    let off = content.rfind("x;").unwrap() as u32;
    let cursor = parser::TextSize::from(off);

    let query = SemanticQuery::at_cursor(&db, top_mod, cursor);
    let key = query.symbol_key().expect("symbol at cursor");
    let (_tm, def_span) = SemanticQuery::definition_for_symbol(&db, key).expect("def span");

    let refs = query.find_references();
    let def_res = def_span.resolve(&db).expect("resolve def span");
    let found = refs.into_iter().any(|r| {
        if let Some(sp) = r.span.resolve(&db) {
            sp.file == def_res.file && sp.range == def_res.range
        } else { false }
    });
    assert!(found, "definition site should appear among references");
}
