use common::InputDb;
use driver::DriverDataBase;
use std::path::PathBuf;

// Re-implement load_ingot_from_directory here since we can't import from the library crate
fn load_ingot_from_directory(db: &mut DriverDataBase, ingot_dir: &std::path::Path) {
    let ingot_url =
        url::Url::from_directory_path(ingot_dir).expect("Failed to create URL from directory path");

    let diagnostics = driver::init_ingot(db, &ingot_url);

    // In tests, we might want to panic on serious errors
    for diagnostic in &diagnostics {
        match diagnostic {
            driver::IngotInitDiagnostics::ConfigParseError { .. }
            | driver::IngotInitDiagnostics::ConfigDiagnostics { .. } => {
                panic!("Failed to resolve test ingot at {ingot_dir:?}: {diagnostic}");
            }
            _ => {
                // Log other diagnostics but don't panic
                eprintln!("Test ingot diagnostic for {ingot_dir:?}: {diagnostic}");
            }
        }
    }
}

/// Test that dependencies are not re-resolved when initializing multiple ingots
/// that share common dependencies.
///
/// This test creates a scenario where:
/// - Ingot A has no dependencies
/// - Ingot B depends on A
/// - Ingot C depends on B (and transitively on A)
///
/// When we load B then C, A should only be resolved once (when B is loaded).
#[test]
fn test_dependency_not_reresolved_across_ingots() {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let test_files_dir = PathBuf::from(&manifest_dir).join("test_files/dependency_reresolution");

    // Paths to our test ingots
    let ingot_a_dir = test_files_dir.join("ingot_a");
    let ingot_b_dir = test_files_dir.join("ingot_b");
    let ingot_c_dir = test_files_dir.join("ingot_c");

    // Create a single database instance that persists across all loads
    let mut db = DriverDataBase::default();

    // Load ingot B (which depends on A)
    // This should resolve both B and A
    load_ingot_from_directory(&mut db, &ingot_b_dir);

    // Get the graph state after loading B
    let graph_after_b = db.graph();
    let urls_after_b = graph_after_b.all_urls(&db);

    println!("URLs in graph after loading B: {:?}", urls_after_b.len());
    for url in &urls_after_b {
        println!("  - {}", url);
    }

    // Verify both B and A are in the graph
    let ingot_a_url = url::Url::from_directory_path(&ingot_a_dir).unwrap();
    let ingot_b_url = url::Url::from_directory_path(&ingot_b_dir).unwrap();

    assert!(
        graph_after_b.contains_url(&db, &ingot_a_url),
        "Ingot A should be in graph after loading B (which depends on A)"
    );
    assert!(
        graph_after_b.contains_url(&db, &ingot_b_url),
        "Ingot B should be in graph after loading B"
    );

    // Now load ingot C (which depends on B)
    // B and A are already in the graph, so they should NOT be re-resolved
    load_ingot_from_directory(&mut db, &ingot_c_dir);

    // Get the graph state after loading C
    let graph_after_c = db.graph();
    let urls_after_c = graph_after_c.all_urls(&db);

    println!("URLs in graph after loading C: {:?}", urls_after_c.len());
    for url in &urls_after_c {
        println!("  - {}", url);
    }

    // Verify C is now in the graph
    let ingot_c_url = url::Url::from_directory_path(&ingot_c_dir).unwrap();
    assert!(
        graph_after_c.contains_url(&db, &ingot_c_url),
        "Ingot C should be in graph after loading C"
    );

    // Verify A and B are still in the graph (they should be!)
    assert!(
        graph_after_c.contains_url(&db, &ingot_a_url),
        "Ingot A should still be in graph after loading C"
    );
    assert!(
        graph_after_c.contains_url(&db, &ingot_b_url),
        "Ingot B should still be in graph after loading C"
    );

    // Verify the graph has all 3 ingots
    assert_eq!(
        urls_after_c.len(),
        3,
        "Graph should contain exactly 3 ingots (A, B, C)"
    );

    // Verify dependency relationships
    let c_deps = graph_after_c.dependency_urls(&db, &ingot_c_url);
    assert!(c_deps.contains(&ingot_b_url), "C should depend on B");
    assert!(
        c_deps.contains(&ingot_a_url),
        "C should transitively depend on A"
    );
}

/// Test that calling init_ingot multiple times on the same ingot doesn't
/// re-resolve its dependencies.
#[test]
fn test_idempotent_ingot_loading() {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let test_files_dir = PathBuf::from(&manifest_dir).join("test_files/dependency_reresolution");

    let ingot_b_dir = test_files_dir.join("ingot_b");

    let mut db = DriverDataBase::default();

    // Load B the first time
    load_ingot_from_directory(&mut db, &ingot_b_dir);

    let graph_after_first = db.graph();
    let urls_after_first = graph_after_first.all_urls(&db);
    let count_after_first = urls_after_first.len();

    println!("URLs after first load: {}", count_after_first);

    // Load B again (should be idempotent)
    load_ingot_from_directory(&mut db, &ingot_b_dir);

    let graph_after_second = db.graph();
    let urls_after_second = graph_after_second.all_urls(&db);
    let count_after_second = urls_after_second.len();

    println!("URLs after second load: {}", count_after_second);

    // The graph should have the same number of URLs
    assert_eq!(
        count_after_first, count_after_second,
        "Graph should have same number of URLs after loading the same ingot twice"
    );
}
