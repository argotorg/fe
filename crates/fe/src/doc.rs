use camino::Utf8PathBuf;
use common::InputDb;
use doc_engine::server::{serve, DocServerConfig};
use doc_engine::{DocExtractor, DocIndex};
use driver::DriverDataBase;
use hir::hir_def::HirIngot;
use url::Url;

pub fn generate_docs(
    path: &Utf8PathBuf,
    output: Option<&Utf8PathBuf>,
    json: bool,
    serve_docs: bool,
    port: u16,
) {
    let mut db = DriverDataBase::default();

    let index = if path.is_file() && path.extension() == Some("fe") {
        extract_single_file(&mut db, path)
    } else if path.is_dir() {
        extract_ingot(&mut db, path)
    } else {
        eprintln!("Error: Path must be either a .fe file or a directory containing fe.toml");
        std::process::exit(1);
    };

    let Some(index) = index else {
        std::process::exit(1);
    };

    if serve_docs {
        // Start HTTP server
        let config = DocServerConfig {
            port,
            host: "127.0.0.1".to_string(),
            assets_path: None,
        };

        println!("Starting documentation server...");
        println!("Open http://127.0.0.1:{port} in your browser");
        println!("Press Ctrl+C to stop");

        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            if let Err(e) = serve(index, config).await {
                eprintln!("Server error: {e}");
                std::process::exit(1);
            }
        });
    } else if json {
        // Output JSON to stdout or file
        let json_output = serde_json::to_string_pretty(&index).unwrap();
        if let Some(output_path) = output {
            std::fs::write(output_path, &json_output).unwrap_or_else(|e| {
                eprintln!("Error writing to {output_path}: {e}");
                std::process::exit(1);
            });
            println!("Wrote documentation JSON to {output_path}");
        } else {
            println!("{json_output}");
        }
    } else {
        // Print summary
        print_doc_summary(&index);
    }
}

fn extract_single_file(db: &mut DriverDataBase, file_path: &Utf8PathBuf) -> Option<DocIndex> {
    let file_url = Url::from_file_path(file_path.canonicalize_utf8().unwrap()).ok()?;

    let content = std::fs::read_to_string(file_path).ok()?;
    db.workspace().touch(db, file_url.clone(), Some(content));

    let file = db.workspace().get(db, &file_url)?;
    let top_mod = db.top_mod(file);

    // Check for errors first
    let diags = db.run_on_top_mod(top_mod);
    if !diags.is_empty() {
        eprintln!("Warning: File has errors, documentation may be incomplete");
        diags.emit(db);
    }

    let extractor = DocExtractor::new(db);
    Some(extractor.extract_module(top_mod))
}

fn extract_ingot(db: &mut DriverDataBase, dir_path: &Utf8PathBuf) -> Option<DocIndex> {
    let canonical_path = dir_path.canonicalize_utf8().ok()?;
    let ingot_url = Url::from_directory_path(canonical_path.as_str()).ok()?;

    let init_diagnostics = driver::init_ingot(db, &ingot_url);
    if !init_diagnostics.is_empty() {
        for diagnostic in &init_diagnostics {
            eprintln!("Warning: {diagnostic}");
        }
    }

    let ingot = db.workspace().containing_ingot(db, ingot_url)?;

    // Check for errors
    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        eprintln!("Warning: Ingot has errors, documentation may be incomplete");
        diags.emit(db);
    }

    let root_mod = ingot.root_mod(db);
    let extractor = DocExtractor::new(db);
    Some(extractor.extract_module(root_mod))
}

fn print_doc_summary(index: &DocIndex) {
    println!("Fe Documentation Index");
    println!("======================");
    println!();
    println!("Items: {}", index.items.len());
    println!();

    // Group by kind
    let mut by_kind: std::collections::HashMap<&str, Vec<_>> = std::collections::HashMap::new();
    for item in &index.items {
        by_kind
            .entry(item.kind.display_name())
            .or_default()
            .push(item);
    }

    for (kind, items) in by_kind.iter() {
        println!("{kind}s ({}):", items.len());
        for item in items.iter().take(10) {
            let doc_preview = item
                .docs
                .as_ref()
                .map(|d| {
                    let summary = &d.summary;
                    if summary.len() > 60 {
                        format!("{}...", &summary[..60])
                    } else {
                        summary.clone()
                    }
                })
                .unwrap_or_default();

            if doc_preview.is_empty() {
                println!("  - {}", item.path);
            } else {
                println!("  - {} - {}", item.path, doc_preview);
            }
        }
        if items.len() > 10 {
            println!("  ... and {} more", items.len() - 10);
        }
        println!();
    }
}
