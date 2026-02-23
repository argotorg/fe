use camino::Utf8PathBuf;
use common::InputDb;
use driver::DriverDataBase;
use fe_web::model::DocIndex;
use hir::hir_def::HirIngot;
use serde::{Deserialize, Serialize};
use url::Url;

use crate::extract::DocExtractor;

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
        std::path::Path::new(&format!("/proc/{}", self.pid)).exists() || cfg!(windows) // On Windows, just assume it's alive if the file exists
    }
}

#[allow(unused_variables)]
pub fn generate_docs(
    path: &Utf8PathBuf,
    output: Option<&Utf8PathBuf>,
    json: bool,
    serve_docs: bool,
    port: u16,
    static_site: bool,
    markdown_pages: bool,
    builtins: bool,
) {
    // First, check if there's a running LSP with docs server
    if serve_docs {
        let canonical_path = path.canonicalize_utf8().ok();
        let workspace_root = canonical_path.as_ref().and_then(|p| {
            if p.is_file() {
                p.parent().map(|p| p.as_std_path())
            } else {
                Some(p.as_std_path())
            }
        });

        if let Some(root) = workspace_root
            && let Some(info) = LspServerInfo::read_from_workspace(root)
            && info.is_alive()
            && let Some(docs_url) = &info.docs_url
        {
            println!("Found running language server with documentation at:");
            println!("  {}", docs_url);
            println!();
            println!("The language server keeps docs in sync with your code.");
            println!("Open the URL above in your browser.");
            return;
        }
    }

    let mut db = DriverDataBase::default();

    let index = if path.is_file() && path.extension() == Some("fe") {
        extract_single_file(&mut db, path)
    } else if path.is_dir() {
        // Check if this is a workspace (fe.toml with [workspace] section)
        let fe_toml = path.join("fe.toml");
        if fe_toml.is_file() {
            if let Ok(content) = std::fs::read_to_string(&fe_toml) {
                if let Ok(common::config::Config::Workspace(ws_config)) =
                    common::config::Config::parse(&content)
                {
                    extract_workspace(&mut db, path, &ws_config)
                } else {
                    extract_ingot(&mut db, path)
                }
            } else {
                extract_ingot(&mut db, path)
            }
        } else {
            extract_ingot(&mut db, path)
        }
    } else {
        eprintln!("Error: Path must be either a .fe file or a directory containing fe.toml");
        std::process::exit(1);
    };

    let Some(mut index) = index else {
        std::process::exit(1);
    };

    // Append builtin ingot docs when --builtins is set
    if builtins {
        use common::stdlib::{HasBuiltinCore, HasBuiltinStd};

        // Skip builtins that are already present (e.g. workspace that includes core/std)
        let existing_roots: std::collections::HashSet<String> =
            index.modules.iter().map(|m| m.name.clone()).collect();

        for (label, builtin_ingot) in [
            ("core", db.builtin_core()),
            ("std", db.builtin_std()),
        ] {
            if existing_roots.contains(label) {
                continue;
            }

            let extractor = DocExtractor::new(&db);
            for top_mod in builtin_ingot.all_modules(&db) {
                for item in top_mod.children_nested(&db) {
                    if let Some(doc_item) =
                        extractor.extract_item_for_ingot(item, builtin_ingot)
                    {
                        index.items.push(doc_item);
                    }
                }
            }
            let root_mod = builtin_ingot.root_mod(&db);
            index
                .modules
                .extend(extractor.build_module_tree_for_ingot(builtin_ingot, root_mod));

            let trait_impl_links = extractor.extract_trait_impl_links(builtin_ingot);
            index.link_trait_impls(trait_impl_links);

            let mod_count = builtin_ingot.all_modules(&db).len();
            println!("  Included builtin '{label}' ({mod_count} modules)");
        }

    }

    if static_site {
        let output_dir = output
            .map(|p| p.as_std_path().to_path_buf())
            .unwrap_or_else(|| std::path::PathBuf::from("docs"));

        // Generate SCIP for interactive navigation (best-effort)
        let scip_json = generate_scip_json_for_doc(&mut db, path, &mut index);

        if let Err(e) = fe_web::static_site::StaticSiteGenerator::generate_with_scip(
            &index,
            &output_dir,
            scip_json.as_deref(),
        ) {
            eprintln!("Error generating static docs: {e}");
            std::process::exit(1);
        }
        if scip_json.is_some() {
            println!("Static docs written to {} (with SCIP)", output_dir.display());
        } else {
            println!("Static docs written to {}", output_dir.display());
        }
        return;
    }

    if markdown_pages {
        let output_dir = output
            .map(|p| p.as_std_path().to_path_buf())
            .unwrap_or_else(|| std::path::PathBuf::from("docs"));
        if let Err(e) = fe_web::starlight::generate(&index, &output_dir, "/api") {
            eprintln!("Error generating markdown pages: {e}");
            std::process::exit(1);
        }
        println!("Markdown pages written to {}", output_dir.display());
        return;
    }

    #[cfg(feature = "doc-server")]
    if serve_docs {
        use crate::doc_serve::{DocServeConfig, serve_docs as serve};

        let config = DocServeConfig {
            port,
            host: "127.0.0.1".to_string(),
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
        return;
    }

    #[cfg(not(feature = "doc-server"))]
    if serve_docs {
        eprintln!("Error: doc-server feature not enabled. Rebuild with --features doc-server");
        std::process::exit(1);
    }

    if json {
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

fn extract_workspace(
    db: &mut DriverDataBase,
    workspace_root: &Utf8PathBuf,
    ws_config: &common::config::WorkspaceConfig,
) -> Option<DocIndex> {
    use common::config::WorkspaceMemberSelection;

    let members = ws_config
        .workspace
        .members_for_selection(WorkspaceMemberSelection::PrimaryOnly);

    if members.is_empty() {
        eprintln!("Error: Workspace has no members");
        return None;
    }

    println!(
        "Workspace with {} member(s): {}",
        members.len(),
        members
            .iter()
            .map(|m| m.name.as_deref().unwrap_or(m.path.as_str()))
            .collect::<Vec<_>>()
            .join(", ")
    );

    let mut combined = DocIndex::new();

    for member in &members {
        let member_path = Utf8PathBuf::from(workspace_root.join(member.path.as_str()));
        if !member_path.is_dir() {
            eprintln!(
                "Warning: Workspace member '{}' at {} not found, skipping",
                member.name.as_deref().unwrap_or(member.path.as_str()),
                member_path,
            );
            continue;
        }

        let member_name = member.name.as_deref().unwrap_or(member.path.as_str());
        println!("  Extracting docs for '{member_name}'...");

        // Each member gets its own fresh database to avoid cross-contamination
        let mut member_db = DriverDataBase::default();
        if let Some(member_index) = extract_ingot(&mut member_db, &member_path) {
            combined.items.extend(member_index.items);
            combined.modules.extend(member_index.modules);
        } else {
            eprintln!("  Warning: Failed to extract docs for '{member_name}'");
        }
    }

    if combined.items.is_empty() && combined.modules.is_empty() {
        eprintln!("Error: No documentation extracted from workspace members");
        return None;
    }


    Some(combined)
}

fn extract_ingot(db: &mut DriverDataBase, dir_path: &Utf8PathBuf) -> Option<DocIndex> {
    let canonical_path = dir_path.canonicalize_utf8().ok()?;
    let ingot_url = Url::from_directory_path(canonical_path.as_str()).ok()?;

    let had_diagnostics = driver::init_ingot(db, &ingot_url);
    if had_diagnostics {
        eprintln!("Warning: Ingot initialization produced diagnostics");
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

/// Generate SCIP JSON for embedding in static docs (best-effort).
///
/// Also enriches the DocIndex's `rich_signature` fields using the SCIP
/// symbol table before returning the JSON string.
///
/// Returns `None` if generation fails (SCIP is optional progressive enhancement).
fn generate_scip_json_for_doc(
    db: &mut DriverDataBase,
    path: &Utf8PathBuf,
    doc_index: &mut fe_web::model::DocIndex,
) -> Option<String> {
    let ingot_url = if path.is_dir() {
        let canonical = path.canonicalize_utf8().ok()?;
        Url::from_directory_path(canonical.as_str()).ok()?
    } else {
        let canonical = path.canonicalize_utf8().ok()?;
        let parent = canonical.parent()?;
        Url::from_directory_path(parent.as_str()).ok()?
    };

    match crate::scip_index::generate_scip(db, &ingot_url) {
        Ok(scip_index) => {
            // Enrich signatures before serializing
            crate::scip_index::enrich_signatures(doc_index, &scip_index);

            // Convert to JSON and inject doc URLs
            let json = crate::scip_index::scip_to_json_data(&scip_index);
            Some(crate::scip_index::inject_doc_urls(&json, doc_index))
        }
        Err(e) => {
            eprintln!("Warning: SCIP generation failed: {e}");
            None
        }
    }
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
