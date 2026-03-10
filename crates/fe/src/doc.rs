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
pub(crate) struct LspServerInfo {
    pub pid: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub port: Option<u16>,
    pub workspace_root: Option<String>,
    pub docs_url: Option<String>,
}

impl LspServerInfo {
    /// Read server info from a workspace
    pub(crate) fn read_from_workspace(workspace_root: &std::path::Path) -> Option<Self> {
        let info_path = workspace_root.join(".fe-lsp.json");
        let json = std::fs::read_to_string(&info_path).ok()?;
        serde_json::from_str(&json).ok()
    }

    /// Write server info to the workspace root.
    pub(crate) fn write_to_workspace(
        &self,
        workspace_root: &std::path::Path,
    ) -> std::io::Result<()> {
        let info_path = workspace_root.join(".fe-lsp.json");
        let json = serde_json::to_string_pretty(self).map_err(std::io::Error::other)?;
        std::fs::write(&info_path, json)
    }

    /// Remove the .fe-lsp.json file from a workspace.
    pub(crate) fn remove_from_workspace(workspace_root: &std::path::Path) {
        let info_path = workspace_root.join(".fe-lsp.json");
        let _ = std::fs::remove_file(info_path);
    }

    /// Check if the LSP process is still running (cross-platform).
    pub(crate) fn is_alive(&self) -> bool {
        #[cfg(unix)]
        {
            // `kill -0 pid` checks if a process exists without signalling it.
            // Returns exit code 0 if the process exists.
            std::process::Command::new("kill")
                .args(["-0", &self.pid.to_string()])
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .status()
                .is_ok_and(|s| s.success())
        }
        #[cfg(windows)]
        {
            // `tasklist /FI "PID eq <pid>"` outputs the process if it exists.
            std::process::Command::new("tasklist")
                .args(["/FI", &format!("PID eq {}", self.pid), "/NH"])
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::null())
                .output()
                .is_ok_and(|o| {
                    let out = String::from_utf8_lossy(&o.stdout);
                    out.contains(&self.pid.to_string())
                })
        }
        #[cfg(not(any(unix, windows)))]
        {
            true
        }
    }
}

#[allow(unused_variables, clippy::too_many_arguments)]
pub fn generate_docs(
    path: &Utf8PathBuf,
    output: Option<&Utf8PathBuf>,
    json: bool,
    serve_docs: bool,
    port: u16,
    static_site: bool,
    markdown_pages: bool,
    builtins: bool,
    base_url: &str,
) {
    // First, check if there's a running LSP with docs server
    if serve_docs {
        let canonical_path = path.canonicalize_utf8().ok();
        let start_dir = canonical_path.as_ref().and_then(|p| {
            if p.is_file() {
                p.parent().map(|p| p.as_std_path().to_path_buf())
            } else {
                Some(p.as_std_path().to_path_buf())
            }
        });

        // Walk ancestor directories to find .fe-lsp.json (it lives at project root,
        // which may be several levels above the given path).
        let found = start_dir.and_then(|dir| {
            let mut current = dir.as_path();
            loop {
                if let Some(info) = LspServerInfo::read_from_workspace(current)
                    && info.is_alive()
                {
                    return info.docs_url.clone();
                }
                current = current.parent()?;
            }
        });

        if let Some(docs_url) = &found {
            println!("Found running language server with documentation at:");
            println!("  {}", docs_url);
            println!();
            println!("The language server keeps docs in sync with your code.");
            println!("Open the URL above in your browser.");
            return;
        }
    }

    let mut db = DriverDataBase::default();
    let git_root = detect_git_root(path.as_std_path());

    let index = if path.is_file() && path.extension() == Some("fe") {
        extract_single_file(&mut db, path, git_root.as_deref())
    } else if path.is_dir() {
        // Check if this is a workspace (fe.toml with [workspace] section)
        let fe_toml = path.join("fe.toml");
        if fe_toml.is_file() {
            if let Ok(content) = std::fs::read_to_string(&fe_toml) {
                if let Ok(common::config::Config::Workspace(ws_config)) =
                    common::config::Config::parse(&content)
                {
                    extract_workspace(&mut db, path, &ws_config, git_root.as_deref())
                } else {
                    extract_ingot(&mut db, path, git_root.as_deref())
                }
            } else {
                extract_ingot(&mut db, path, git_root.as_deref())
            }
        } else {
            extract_ingot(&mut db, path, git_root.as_deref())
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

        for (label, builtin_ingot) in [("core", db.builtin_core()), ("std", db.builtin_std())] {
            if existing_roots.contains(label) {
                continue;
            }

            let extractor = make_extractor(&db, git_root.as_deref());
            for top_mod in builtin_ingot.all_modules(&db) {
                for item in top_mod.children_nested(&db) {
                    if let Some(doc_item) = extractor.extract_item_for_ingot(item, builtin_ingot) {
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

    // Generate SCIP for interactive navigation (best-effort).
    // This enriches rich_signature fields and produces JSON for embedding.
    let scip_json = generate_scip_json_for_doc(&mut db, path, &mut index, builtins);

    if static_site {
        let output_dir = output
            .map(|p| p.as_std_path().to_path_buf())
            .unwrap_or_else(|| std::path::PathBuf::from("docs"));

        // Auto-detect git source link base for GitHub links
        let source_link_base = detect_source_link_base(path.as_std_path());

        if let Err(e) = fe_web::static_site::StaticSiteGenerator::generate_full(
            &index,
            &output_dir,
            scip_json.as_deref(),
            source_link_base.as_deref(),
        ) {
            eprintln!("Error generating static docs: {e}");
            std::process::exit(1);
        }

        // When --json is also set, write docs.json alongside index.html
        if json {
            let merged = build_merged_json(&index, scip_json.as_deref());
            let json_path = output_dir.join("docs.json");
            std::fs::write(&json_path, &merged).unwrap_or_else(|e| {
                eprintln!("Error writing docs.json: {e}");
                std::process::exit(1);
            });
        }

        let suffix = match (scip_json.is_some(), json) {
            (true, true) => " (with SCIP + docs.json)",
            (true, false) => " (with SCIP)",
            (false, true) => " (with docs.json)",
            (false, false) => "",
        };
        println!("Static docs written to {}{suffix}", output_dir.display());
        return;
    }

    if markdown_pages {
        let output_dir = output
            .map(|p| p.as_std_path().to_path_buf())
            .unwrap_or_else(|| std::path::PathBuf::from("docs"));
        if let Err(e) = fe_web::starlight::generate(&index, &output_dir, base_url) {
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
            let source_link_base = detect_source_link_base(path.as_std_path());
            if let Err(e) = serve(index, config, scip_json, source_link_base).await {
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
        // Produce merged docs.json (DocIndex + SCIP) for web component consumption
        let merged = build_merged_json(&index, scip_json.as_deref());
        if let Some(output_path) = output {
            let output_dir = output_path.as_std_path();
            std::fs::create_dir_all(output_dir).unwrap_or_else(|e| {
                eprintln!("Error creating output directory {output_path}: {e}");
                std::process::exit(1);
            });
            let json_path = output_dir.join("docs.json");
            std::fs::write(&json_path, &merged).unwrap_or_else(|e| {
                eprintln!("Error writing docs.json: {e}");
                std::process::exit(1);
            });
            println!("Wrote docs.json to {output_path}");
        } else {
            println!("{merged}");
        }
    } else {
        // Print summary
        print_doc_summary(&index);
    }
}

/// Create a DocExtractor with optional git-root-relative source paths.
fn make_extractor<'db>(
    db: &'db dyn hir::SpannedHirDb,
    git_root: Option<&std::path::Path>,
) -> DocExtractor<'db> {
    let extractor = DocExtractor::new(db);
    if let Some(root) = git_root {
        extractor.with_root_path(root.to_path_buf())
    } else {
        extractor
    }
}

fn extract_single_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    git_root: Option<&std::path::Path>,
) -> Option<DocIndex> {
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

    let extractor = make_extractor(db, git_root);
    Some(extractor.extract_module(top_mod))
}

fn extract_workspace(
    db: &mut DriverDataBase,
    workspace_root: &Utf8PathBuf,
    ws_config: &common::config::WorkspaceConfig,
    git_root: Option<&std::path::Path>,
) -> Option<DocIndex> {
    use common::config::WorkspaceMemberSelection;

    let canonical_root = workspace_root
        .canonicalize_utf8()
        .unwrap_or_else(|_| workspace_root.clone());
    let base_url = match Url::from_directory_path(canonical_root.as_str()) {
        Ok(u) => u,
        Err(_) => {
            eprintln!("Error: Failed to build URL for workspace root: {canonical_root}");
            return None;
        }
    };

    let expanded = match resolver::workspace::expand_workspace_members(
        &ws_config.workspace,
        &base_url,
        WorkspaceMemberSelection::PrimaryOnly,
    ) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Error: Failed to expand workspace members: {e}");
            return None;
        }
    };

    if expanded.is_empty() {
        eprintln!("Error: Workspace has no members");
        return None;
    }

    println!(
        "Workspace with {} member(s): {}",
        expanded.len(),
        expanded
            .iter()
            .map(|m| m.name.as_deref().unwrap_or(m.path.as_str()))
            .collect::<Vec<_>>()
            .join(", ")
    );

    // Initialize all members in the shared db first so cross-ingot
    // references resolve correctly (e.g. ingot A imports ingot B).
    let mut member_entries: Vec<(String, Url)> = Vec::new();
    for member in &expanded {
        let member_name = member
            .name
            .as_deref()
            .unwrap_or(member.path.as_str())
            .to_string();

        println!("  Initializing '{member_name}'...");
        driver::init_ingot(db, &member.url);
        member_entries.push((member_name, member.url.clone()));
    }

    // Extract docs from each member using the shared db
    let mut combined = DocIndex::new();
    for (member_name, ingot_url) in &member_entries {
        println!("  Extracting docs for '{member_name}'...");

        let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
            eprintln!("  Warning: Could not find ingot for '{member_name}'");
            continue;
        };

        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            eprintln!("  Warning: '{member_name}' has errors, documentation may be incomplete");
            diags.emit(db);
        }

        let extractor = make_extractor(db, git_root);
        for top_mod in ingot.all_modules(db) {
            for item in top_mod.children_nested(db) {
                if let Some(doc_item) = extractor.extract_item_for_ingot(item, ingot) {
                    combined.items.push(doc_item);
                }
            }
        }

        let root_mod = ingot.root_mod(db);
        combined
            .modules
            .extend(extractor.build_module_tree_for_ingot(ingot, root_mod));

        let trait_impl_links = extractor.extract_trait_impl_links(ingot);
        combined.link_trait_impls(trait_impl_links);
    }

    if combined.items.is_empty() && combined.modules.is_empty() {
        eprintln!("Error: No documentation extracted from workspace members");
        return None;
    }

    Some(combined)
}

fn extract_ingot(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    git_root: Option<&std::path::Path>,
) -> Option<DocIndex> {
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

    let extractor = make_extractor(db, git_root);
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
/// For workspaces, generates SCIP for each member ingot and merges results.
/// Returns `None` if generation fails (SCIP is optional progressive enhancement).
fn generate_scip_json_for_doc(
    db: &mut DriverDataBase,
    path: &Utf8PathBuf,
    doc_index: &mut fe_web::model::DocIndex,
    include_builtins: bool,
) -> Option<String> {
    // Collect ingot URLs to generate SCIP for.
    // For a single ingot, this is just the path itself.
    // For a workspace, we find all user ingots loaded in the db.
    let mut ingot_urls = collect_ingot_urls(db, path);

    // Include builtin ingots (core/std) when --builtins is enabled
    if include_builtins {
        for base_url in [
            common::stdlib::BUILTIN_CORE_BASE_URL,
            common::stdlib::BUILTIN_STD_BASE_URL,
        ] {
            if let Ok(url) = url::Url::parse(base_url)
                && !ingot_urls.contains(&url)
            {
                ingot_urls.push(url);
            }
        }
    }

    if ingot_urls.is_empty() {
        return None;
    }

    // Generate SCIP for each ingot and merge into one index
    let mut combined_index = scip::types::Index::default();
    let mut any_succeeded = false;

    for ingot_url in &ingot_urls {
        match crate::scip_index::generate_scip(db, ingot_url) {
            Ok(mut scip_index) => {
                let project_root = ingot_url
                    .to_file_path()
                    .ok()
                    .and_then(|p| camino::Utf8PathBuf::from_path_buf(p).ok());

                if let Some(ref root) = project_root {
                    crate::scip_index::enrich_signatures(db, root, doc_index, &mut scip_index);
                }

                combined_index.documents.extend(scip_index.documents);
                any_succeeded = true;
            }
            Err(e) => {
                eprintln!("Warning: SCIP generation failed for {}: {e}", ingot_url);
            }
        }
    }

    if !any_succeeded {
        return None;
    }

    let json = crate::scip_index::scip_to_json_data(&combined_index);
    Some(crate::scip_index::inject_doc_urls(&json, doc_index))
}

/// Collect ingot URLs to generate SCIP for.
///
/// For a single ingot path, returns that path's URL.
/// For a workspace, reads fe.toml to find member paths.
fn collect_ingot_urls(db: &DriverDataBase, path: &Utf8PathBuf) -> Vec<Url> {
    // Try the path itself as a single ingot first
    if let Some(url) = path_to_ingot_url(path)
        && db.workspace().containing_ingot(db, url.clone()).is_some()
    {
        return vec![url];
    }

    // Workspace mode: read fe.toml to discover member ingot paths
    let fe_toml = path.join("fe.toml");
    let content = match std::fs::read_to_string(&fe_toml) {
        Ok(c) => c,
        Err(_) => return Vec::new(),
    };
    let ws_config = match common::config::Config::parse(&content) {
        Ok(common::config::Config::Workspace(ws)) => ws,
        _ => return Vec::new(),
    };

    let base_url = match Url::from_directory_path(path.as_str()) {
        Ok(u) => u,
        Err(_) => return Vec::new(),
    };

    let expanded = match resolver::workspace::expand_workspace_members(
        &ws_config.workspace,
        &base_url,
        common::config::WorkspaceMemberSelection::PrimaryOnly,
    ) {
        Ok(m) => m,
        Err(_) => return Vec::new(),
    };

    let mut urls = Vec::new();
    for member in &expanded {
        if db
            .workspace()
            .containing_ingot(db, member.url.clone())
            .is_some()
        {
            urls.push(member.url.clone());
        }
    }
    urls
}

fn path_to_ingot_url(path: &Utf8PathBuf) -> Option<Url> {
    if path.is_dir() {
        let canonical = path.canonicalize_utf8().ok()?;
        Url::from_directory_path(canonical.as_str()).ok()
    } else {
        let canonical = path.canonicalize_utf8().ok()?;
        let parent = canonical.parent()?;
        Url::from_directory_path(parent.as_str()).ok()
    }
}

/// Detect the git repository root directory.
fn detect_git_root(working_dir: &std::path::Path) -> Option<std::path::PathBuf> {
    let dir = if working_dir.is_file() {
        working_dir.parent()?
    } else {
        working_dir
    };
    std::process::Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .current_dir(dir)
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| std::path::PathBuf::from(String::from_utf8_lossy(&o.stdout).trim().to_string()))
}

/// The canonical GitHub repository for source links.
const CANONICAL_REPO: &str = "https://github.com/argotorg/fe";

/// Build a GitHub source link base using the canonical repo URL and the
/// current git commit hash.
///
/// Returns something like "https://github.com/ethereum/fe/blob/abc123def".
/// The repo URL is hardcoded so that builds from forks don't leak arbitrary
/// remote URLs into the generated docs.
fn detect_source_link_base(working_dir: &std::path::Path) -> Option<String> {
    let dir = if working_dir.is_file() {
        working_dir.parent()?
    } else {
        working_dir
    };

    // Get the commit hash
    let commit = std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(dir)
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())?;

    Some(format!("{}/blob/{}", CANONICAL_REPO, commit))
}

/// Build a merged JSON string containing both the DocIndex and SCIP data.
///
/// This is the single data file that web components consume via `data-src`.
/// The structure is: `{ "index": <DocIndex>, "scip": <SCIP data or null> }`
fn build_merged_json(index: &DocIndex, scip_json: Option<&str>) -> String {
    let index_value = serde_json::to_value(index).unwrap();
    let scip_value = scip_json
        .and_then(|s| serde_json::from_str::<serde_json::Value>(s).ok())
        .unwrap_or(serde_json::Value::Null);
    let merged = serde_json::json!({
        "index": index_value,
        "scip": scip_value,
    });
    serde_json::to_string_pretty(&merged).unwrap()
}

/// Write the fe-web.js component bundle to a file path.
pub fn write_bundle(path: &Utf8PathBuf) {
    let bundle = fe_web::assets::web_component_bundle();
    if let Some(parent) = path.parent()
        && !parent.as_str().is_empty()
    {
        std::fs::create_dir_all(parent).unwrap_or_else(|e| {
            eprintln!("Error creating directory {parent}: {e}");
            std::process::exit(1);
        });
    }
    std::fs::write(path, bundle).unwrap_or_else(|e| {
        eprintln!("Error writing bundle to {path}: {e}");
        std::process::exit(1);
    });
    println!("Wrote fe-web.js to {path}");
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
                        let trunc = &summary[..summary.floor_char_boundary(60)];
                        format!("{trunc}...")
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
