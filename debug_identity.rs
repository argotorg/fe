// Debug program to test identity_at_offset function
use common::InputDb;
use driver::DriverDataBase;
use hir::lower::map_file_to_mod;
use parser::TextSize;
use url::Url;

fn offset_of(text: &str, needle: &str) -> TextSize {
    TextSize::from(text.find(needle).expect("needle present") as u32)
}

fn main() {
    let content = r#"
fn main(x: i32) -> i32 { let y = x; y }
"#;
    let tmp = std::env::temp_dir().join("debug_identity.fe");
    std::fs::write(&tmp, content).unwrap();
    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::from_file_path(&tmp).unwrap(), Some(content.to_string()));
    let top = map_file_to_mod(&db, file);

    // Test: find identity of 'y' usage
    let y_pos = offset_of(content, "y }");
    println!("Looking for identity at offset {} (character: '{}')", 
             y_pos.into(): usize, 
             content.chars().nth(y_pos.into(): usize).unwrap_or('?'));
    
    // First check if there are any occurrences at this offset
    let occs = hir::source_index::occurrences_at_offset(&db, top, y_pos);
    println!("Found {} occurrences at offset:", occs.len());
    for (i, occ) in occs.iter().enumerate() {
        println!("  [{}]: {:?}", i, occ);
    }
    
    // Now try to get identity
    let identity = hir_analysis::lookup::identity_at_offset(&db, top, y_pos);
    println!("Identity result: {:?}", identity);
}