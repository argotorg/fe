use camino::Utf8PathBuf;
use common::InputDb;
use doc_engine::DocExtractor;
use doc_viewer::model::DocIndex;
use doc_viewer::server::{serve, DocServerConfig};
use driver::DriverDataBase;
use hir::hir_def::HirIngot;
use serde::{Deserialize, Serialize};
use url::Url;

/// Server info written by LSP for discovery
#[derive(Debug, Clone, Serialize, Deserialize)]
struct LspServerInfo {
    pid: u32,
    workspace_root: Option<String>,
    docs_url: Option<String>,
}

impl LspServerInfo {
    /// Read server info from a workspace
    fn read_from_workspace(workspace_root: &std::path::Path) -> Option<Self> {
        let info_path = workspace_root.join(".fe-lsp.json");
        let json = std::fs::read_to_string(&info_path).ok()?;
        serde_json::from_str(&json).ok()
    }

    /// Check if the LSP process is still running
    fn is_alive(&self) -> bool {
        // Simple check: see if the process exists
        std::path::Path::new(&format!("/proc/{}", self.pid)).exists()
            || cfg!(windows) // On Windows, just assume it's alive if the file exists
    }
}

pub fn generate_docs(
    path: &Utf8PathBuf,
    output: Option<&Utf8PathBuf>,
    json: bool,
    serve_docs: bool,
    port: u16,
    csr_mode: bool,
) {
    // First, check if there's a running LSP with docs server
    if serve_docs {
        let canonical_path = path.canonicalize_utf8().ok();
        let workspace_root = canonical_path
            .as_ref()
            .map(|p| {
                if p.is_file() {
                    p.parent().map(|p| p.as_std_path())
                } else {
                    Some(p.as_std_path())
                }
            })
            .flatten();

        if let Some(root) = workspace_root {
            if let Some(info) = LspServerInfo::read_from_workspace(root) {
                if info.is_alive() {
                    if let Some(docs_url) = &info.docs_url {
                        println!("Found running language server with documentation at:");
                        println!("  {}", docs_url);
                        println!();
                        println!("The language server keeps docs in sync with your code.");
                        println!("Open the URL above in your browser.");
                        return;
                    }
                }
            }
        }
    }

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

        if csr_mode {
            println!("Starting documentation server (CSR mode)...");
        } else {
            println!("Starting documentation server...");
        }
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

    let extractor = DocExtractor::new(db);
    let mut index = DocIndex::new();

    // Extract items from all modules with ingot-qualified paths (like LSP does)
    for top_mod in ingot.all_modules(db) {
        for item in top_mod.children_nested(db) {
            if let Some(doc_item) = extractor.extract_item_for_ingot(item, ingot) {
                index.items.push(doc_item);
            }
        }
    }

    // Build module tree with ingot-qualified paths
    let root_mod = ingot.root_mod(db);
    index.modules = extractor.build_module_tree_for_ingot(ingot, root_mod);

    // Extract and link trait implementations
    let trait_impl_links = extractor.extract_trait_impl_links(ingot);
    index.link_trait_impls(trait_impl_links);

    Some(index)
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
