use camino::Utf8PathBuf;
use common::InputDb;
use driver::DriverDataBase;
use fe_web::model::DocIndex;
use hir::hir_def::HirIngot;
use semantic_indexing::extract::DocExtractor;
use semantic_indexing::scip_batch;
use semantic_indexing::tracked::{
    docs_for_ingot, module_tree_for_ingot, trait_impl_links_for_ingot,
};
use serde::{Deserialize, Serialize};
use url::Url;

/// Server info written by LSP for discovery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct LspServerInfo {
    #[serde(default = "default_lsp_server_info_schema_version")]
    pub schema_version: u32,
    pub pid: u32,
    #[serde(default)]
    pub started_at_ms: Option<u128>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub port: Option<u16>,
    pub workspace_root: Option<String>,
    pub docs_url: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lsp: Option<LspEndpointInfo>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub http: Option<HttpEndpointInfo>,
    #[serde(default)]
    pub capabilities: Vec<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub config_hash: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub auth: Option<LspAuthInfo>,
}

fn default_lsp_server_info_schema_version() -> u32 {
    0
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct LspEndpointInfo {
    pub transport: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub port: Option<u16>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ws_url: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct HttpEndpointInfo {
    pub base_url: String,
    pub docs_url: String,
    pub trace_api_url: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct LspAuthInfo {
    pub mode: String,
    pub token_file: String,
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
        let temp_path = workspace_root.join(".fe-lsp.json.tmp");
        std::fs::write(&temp_path, json)?;
        std::fs::rename(temp_path, info_path)
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

/// Outcome of inspecting an existing `.fe-lsp.json` at a workspace root
/// during startup of a new `fe lsp` instance.
///
/// This is purely diagnostic: a new instance should always proceed with
/// writing its own `.fe-lsp.json` (clobbering whatever was there), because
/// that file is an advisory discovery pointer, not a lock. But the
/// outcome of this check is important enough to log: `StaleFound` means
/// the previous process crashed without cleanup, `SiblingLive` means two
/// `fe lsp` instances are running concurrently for the same workspace
/// (possibly a Zed respawn-after-timeout), and `RootMismatch` is a
/// smoking gun for a workspace-root detection bug — two LSP instances
/// think they're serving different roots under the same directory.
#[derive(Debug)]
#[cfg_attr(not(feature = "lsp"), allow(dead_code))]
pub(crate) enum ExistingInstanceCheck {
    /// No `.fe-lsp.json` at the given workspace root. Normal first-launch state.
    None,
    /// A file existed but the recorded PID is not alive. Previous instance
    /// crashed or was SIGKILLed without running the cleanup path. The
    /// caller should remove the file before writing a fresh one.
    StaleFound {
        stale_pid: u32,
        recorded_workspace_root: Option<String>,
    },
    /// A file exists and the recorded PID is alive, and the recorded
    /// workspace_root matches ours. Two instances of `fe lsp` are running
    /// against the same workspace — this is the Zed "respawned the LSP
    /// before the old one finished" case.
    SiblingLive {
        sibling_pid: u32,
        sibling_docs_url: Option<String>,
    },
    /// A file exists and the recorded PID is alive, but the recorded
    /// workspace_root does NOT match ours. The two instances think
    /// they're serving different roots from the same `.fe-lsp.json` path
    /// — this is the smoking gun for a workspace-root detection bug, or
    /// for a renamed/moved project where the old file is stale but the
    /// PID happens to still be alive under an unrelated process (pid reuse).
    RootMismatch {
        other_pid: u32,
        other_workspace_root: Option<String>,
        our_workspace_root: String,
    },
    /// File exists but is malformed JSON or missing required fields. Treat
    /// as stale — almost certainly a leftover from an incompatible version.
    Malformed,
}

#[cfg_attr(not(feature = "lsp"), allow(dead_code))]
impl ExistingInstanceCheck {
    /// Inspect `<workspace_root>/.fe-lsp.json` and classify what we find.
    pub(crate) fn inspect(workspace_root: &std::path::Path) -> Self {
        let info_path = workspace_root.join(".fe-lsp.json");
        if !info_path.exists() {
            return Self::None;
        }
        let Some(info) = LspServerInfo::read_from_workspace(workspace_root) else {
            return Self::Malformed;
        };
        if !info.is_alive() {
            return Self::StaleFound {
                stale_pid: info.pid,
                recorded_workspace_root: info.workspace_root,
            };
        }
        let our_root_str = workspace_root.display().to_string();
        match info.workspace_root.as_deref() {
            Some(recorded) if recorded == our_root_str => Self::SiblingLive {
                sibling_pid: info.pid,
                sibling_docs_url: info.docs_url,
            },
            other => Self::RootMismatch {
                other_pid: info.pid,
                other_workspace_root: other.map(str::to_owned),
                our_workspace_root: our_root_str,
            },
        }
    }
}

#[allow(unused_variables)]
pub fn generate_docs(
    path: &Utf8PathBuf,
    output: Option<&Utf8PathBuf>,
    builtins: bool,
    stdlib_path: Option<&Utf8PathBuf>,
    action: Option<&crate::DocAction>,
) {
    // First, check if there's a running LSP with docs server
    if matches!(action, Some(crate::DocAction::Serve { .. })) {
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

    // Override embedded stdlib with on-disk version if --stdlib-path is given.
    // This allows generating docs for older stdlib versions using the latest fe binary.
    if let Some(stdlib_dir) = stdlib_path {
        if let Err(e) = common::stdlib::load_library_from_path(&mut db, stdlib_dir) {
            eprintln!("Failed to load stdlib from {stdlib_dir}: {e}");
            std::process::exit(1);
        }
        println!("  Loaded stdlib from {stdlib_dir}");
    }

    let git_root = detect_git_root(path.as_std_path());

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

    // Rewrite display_file paths relative to git root (for source links in static docs)
    if let Some(ref root) = git_root {
        for item in &mut index.items {
            if let Some(ref mut source) = item.source
                && let Ok(rel) = std::path::Path::new(&source.file).strip_prefix(root)
            {
                source.display_file = rel.to_string_lossy().to_string();
            }
        }
    }

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

            index
                .items
                .extend(docs_for_ingot(&db, builtin_ingot).clone());
            index
                .modules
                .extend(module_tree_for_ingot(&db, builtin_ingot).clone());
            index.link_trait_impls(trait_impl_links_for_ingot(&db, builtin_ingot).clone());

            let mod_count = builtin_ingot.all_modules(&db).len();
            println!("  Included builtin '{label}' ({mod_count} modules)");
        }
    }

    // Generate SCIP for interactive navigation (best-effort).
    // This enriches rich_signature fields and produces JSON for embedding.
    let scip_json = generate_scip_json_for_doc(&mut db, path, &mut index, builtins);

    match action {
        Some(crate::DocAction::Static { self_contained }) => {
            let output_dir = output
                .map(|p| p.as_std_path().to_path_buf())
                .unwrap_or_else(|| std::path::PathBuf::from("docs"));

            let source_link_base = detect_source_link_base(path.as_std_path());

            let result = if *self_contained {
                fe_web::static_site::StaticSiteGenerator::generate_full(
                    &index,
                    &output_dir,
                    scip_json.as_deref(),
                    source_link_base.as_deref(),
                )
            } else {
                fe_web::static_site::StaticSiteGenerator::generate_split(
                    &index,
                    &output_dir,
                    scip_json.as_deref(),
                    source_link_base.as_deref(),
                )
            };

            if let Err(e) = result {
                eprintln!("Error generating static docs: {e}");
                std::process::exit(1);
            }

            let mode = if *self_contained {
                "self-contained"
            } else {
                "split"
            };
            let suffix = if scip_json.is_some() {
                " (with SCIP)"
            } else {
                ""
            };
            println!(
                "Static docs written to {} ({mode}){suffix}",
                output_dir.display()
            );
        }
        Some(crate::DocAction::Json { merge }) => {
            let merged = build_merged_json(&index, scip_json.as_deref());
            if let Some(merge_target) = merge {
                // Merge into existing docs.json
                let target_path = merge_target.as_std_path();
                if target_path.exists() {
                    match merge_docs_json(target_path, &merged) {
                        Ok(()) => println!("Merged into {merge_target}"),
                        Err(e) => {
                            eprintln!("Error merging into {merge_target}: {e}");
                            std::process::exit(1);
                        }
                    }
                } else {
                    // Target doesn't exist yet, just write it
                    std::fs::write(target_path, &merged).unwrap_or_else(|e| {
                        eprintln!("Error writing {merge_target}: {e}");
                        std::process::exit(1);
                    });
                    println!("Wrote {merge_target}");
                }
            } else if let Some(output_path) = output {
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
        }
        Some(crate::DocAction::Pages { base_url }) => {
            let output_dir = output
                .map(|p| p.as_std_path().to_path_buf())
                .unwrap_or_else(|| std::path::PathBuf::from("docs"));
            if let Err(e) = fe_web::starlight::generate(&index, &output_dir, base_url) {
                eprintln!("Error generating markdown pages: {e}");
                std::process::exit(1);
            }
            println!("Markdown pages written to {}", output_dir.display());
        }
        Some(crate::DocAction::Serve { port }) => {
            #[cfg(feature = "doc-server")]
            {
                use crate::doc_serve::{DocServeConfig, serve_docs as serve};

                let config = DocServeConfig {
                    port: *port,
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
            }
            #[cfg(not(feature = "doc-server"))]
            {
                eprintln!(
                    "Error: doc-server feature not enabled. Rebuild with --features doc-server"
                );
                std::process::exit(1);
            }
        }
        Some(crate::DocAction::Bundle { .. }) => {
            unreachable!("Bundle is handled before generate_docs is called")
        }
        None => {
            print_doc_summary(&index);
        }
    }
}

fn extract_single_file(db: &mut DriverDataBase, file_path: &Utf8PathBuf) -> Option<DocIndex> {
    let canonical = file_path.canonicalize_utf8().ok()?;
    let file_url = Url::from_file_path(&canonical).ok()?;

    let content = std::fs::read_to_string(file_path).ok()?;
    db.workspace().touch(db, file_url.clone(), Some(content));

    let file = db.workspace().get(db, &file_url)?;
    let top_mod = db.top_mod(file);

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

        combined.items.extend(docs_for_ingot(db, ingot).clone());
        combined
            .modules
            .extend(module_tree_for_ingot(db, ingot).clone());
        combined.link_trait_impls(trait_impl_links_for_ingot(db, ingot).clone());
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

    let mut index = DocIndex::new();
    index.items.extend(docs_for_ingot(db, ingot).clone());
    index
        .modules
        .extend(module_tree_for_ingot(db, ingot).clone());
    index.link_trait_impls(trait_impl_links_for_ingot(db, ingot).clone());

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
    let mut combined_doc_urls = std::collections::HashMap::new();
    let mut any_succeeded = false;

    // Use the workspace root for relative path computation so SCIP document
    // paths match the display_file paths used by the doc extractor.
    // For single files, use the parent directory so strip_prefix produces a filename.
    let canonical = path.canonicalize_utf8().unwrap_or_else(|_| path.clone());
    let workspace_root = if canonical.is_file() {
        canonical
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or(canonical)
    } else {
        canonical
    };

    // Generate SCIP per-ingot via salsa-cached path, then enrich signatures
    for ingot_url in &ingot_urls {
        if let Ok(mut result) = scip_batch::generate_scip_with_root(db, ingot_url, &workspace_root)
        {
            let base_url = if ingot_url.to_file_path().is_ok() {
                None
            } else {
                Some(ingot_url)
            };
            scip_batch::enrich_signatures_with_base(
                db,
                &workspace_root,
                base_url,
                doc_index,
                &mut result.index,
            );

            combined_index.documents.extend(result.index.documents);
            combined_doc_urls.extend(result.doc_urls);
            any_succeeded = true;
        }
    }

    if !any_succeeded {
        return None;
    }

    Some(scip_batch::scip_to_json_data(
        &combined_index,
        &combined_doc_urls,
    ))
}

/// Collect ingot URLs to generate SCIP for.
///
/// For a single ingot path, returns that path's URL.
/// For a workspace, reads fe.toml to find member paths.
fn collect_ingot_urls(db: &DriverDataBase, path: &Utf8PathBuf) -> Vec<Url> {
    // Check fe.toml first to distinguish workspace roots from single ingots.
    // A workspace root has a [workspace] section and should expand to member URLs,
    // not be treated as an ingot itself (its config has no ingot metadata).
    let fe_toml = path.join("fe.toml");
    if let Ok(content) = std::fs::read_to_string(&fe_toml)
        && let Ok(common::config::Config::Workspace(ws_config)) =
            common::config::Config::parse(&content)
    {
        return collect_workspace_member_urls(db, path, &ws_config);
    }

    // Single ingot: the path itself
    if let Some(url) = path_to_ingot_url(path)
        && db.workspace().containing_ingot(db, url.clone()).is_some()
    {
        return vec![url];
    }

    Vec::new()
}

fn collect_workspace_member_urls(
    db: &DriverDataBase,
    path: &Utf8PathBuf,
    ws_config: &common::config::WorkspaceConfig,
) -> Vec<Url> {
    let canonical = path.canonicalize_utf8().unwrap_or_else(|_| path.clone());
    let base_url = match Url::from_directory_path(canonical.as_str()) {
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
/// The structure is: `{ "schema_version": N, "index": <DocIndex>, "scip": <SCIP data or null> }`
fn build_merged_json(index: &DocIndex, scip_json: Option<&str>) -> String {
    let mut index_value = serde_json::to_value(index).unwrap();
    fe_web::static_site::inject_html_bodies(&mut index_value);
    let scip_value = scip_json
        .and_then(|s| serde_json::from_str::<serde_json::Value>(s).ok())
        .unwrap_or(serde_json::Value::Null);
    let merged = serde_json::json!({
        "schema_version": fe_web::model::SCHEMA_VERSION,
        "compiler_version": env!("CARGO_PKG_VERSION"),
        "index": index_value,
        "scip": scip_value,
    });
    serde_json::to_string_pretty(&merged).unwrap()
}

/// Merge new docs JSON into an existing docs.json file.
///
/// Combines index items (deduplicating by path), modules, and SCIP data
/// (symbols and files). Preserves existing doc_urls when the new data has none.
fn merge_docs_json(target_path: &std::path::Path, new_json: &str) -> std::io::Result<()> {
    use serde_json::Value;

    let existing_str = std::fs::read_to_string(target_path)?;
    let mut existing: Value = serde_json::from_str(&existing_str).map_err(std::io::Error::other)?;
    let new: Value = serde_json::from_str(new_json).map_err(std::io::Error::other)?;

    // Merge index.items (deduplicate by path)
    if let (Some(ex_items), Some(new_items)) = (
        existing
            .pointer_mut("/index/items")
            .and_then(|v| v.as_array_mut()),
        new.pointer("/index/items").and_then(|v| v.as_array()),
    ) {
        let existing_paths: std::collections::HashSet<String> = ex_items
            .iter()
            .filter_map(|i| i.get("path").and_then(|p| p.as_str()).map(String::from))
            .collect();
        for item in new_items {
            if let Some(path) = item.get("path").and_then(|p| p.as_str())
                && !existing_paths.contains(path)
            {
                ex_items.push(item.clone());
            }
        }
    }

    // Merge index.modules
    if let (Some(ex_mods), Some(new_mods)) = (
        existing
            .pointer_mut("/index/modules")
            .and_then(|v| v.as_array_mut()),
        new.pointer("/index/modules").and_then(|v| v.as_array()),
    ) {
        let existing_names: std::collections::HashSet<String> = ex_mods
            .iter()
            .filter_map(|m| m.get("name").and_then(|n| n.as_str()).map(String::from))
            .collect();
        for module in new_mods {
            if let Some(name) = module.get("name").and_then(|n| n.as_str())
                && !existing_names.contains(name)
            {
                ex_mods.push(module.clone());
            }
        }
    }

    // Merge SCIP symbols (preserve existing doc_urls)
    if let (Some(Value::Object(ex_syms)), Some(Value::Object(new_syms))) = (
        existing.pointer_mut("/scip/symbols"),
        new.pointer("/scip/symbols"),
    ) {
        for (sym, info) in new_syms {
            if let Some(existing_info) = ex_syms.get_mut(sym) {
                // Preserve existing doc_url if new one is missing
                if let Some(existing_obj) = existing_info.as_object_mut()
                    && let Some(new_obj) = info.as_object()
                    && !existing_obj.contains_key("doc_url")
                    && let Some(url) = new_obj.get("doc_url")
                {
                    existing_obj.insert("doc_url".into(), url.clone());
                }
            } else {
                ex_syms.insert(sym.clone(), info.clone());
            }
        }
    }

    // Merge SCIP files
    if let (Some(Value::Object(ex_files)), Some(Value::Object(new_files))) = (
        existing.pointer_mut("/scip/files"),
        new.pointer("/scip/files"),
    ) {
        for (file, occs) in new_files {
            // Use filename when key is empty (single-file input)
            let key = if file.is_empty() {
                "input.fe".to_string()
            } else {
                file.clone()
            };
            if !ex_files.contains_key(&key) {
                ex_files.insert(key, occs.clone());
            }
        }
    }

    let output = serde_json::to_string_pretty(&existing).map_err(std::io::Error::other)?;
    std::fs::write(target_path, output)?;
    Ok(())
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

/// Write the fe-highlight.css syntax theme to a file path.
pub fn write_highlight_css(path: &Utf8PathBuf) {
    if let Some(parent) = path.parent()
        && !parent.as_str().is_empty()
    {
        std::fs::create_dir_all(parent).unwrap_or_else(|e| {
            eprintln!("Error creating directory {parent}: {e}");
            std::process::exit(1);
        });
    }
    std::fs::write(path, fe_web::assets::FE_HIGHLIGHT_CSS).unwrap_or_else(|e| {
        eprintln!("Error writing CSS to {path}: {e}");
        std::process::exit(1);
    });
    println!("Wrote fe-highlight.css to {path}");
}

/// Write the styles.css layout theme to a file path.
pub fn write_styles_css(path: &Utf8PathBuf) {
    if let Some(parent) = path.parent()
        && !parent.as_str().is_empty()
    {
        std::fs::create_dir_all(parent).unwrap_or_else(|e| {
            eprintln!("Error creating directory {parent}: {e}");
            std::process::exit(1);
        });
    }
    std::fs::write(path, fe_web::assets::STYLES_CSS).unwrap_or_else(|e| {
        eprintln!("Error writing CSS to {path}: {e}");
        std::process::exit(1);
    });
    println!("Wrote styles.css to {path}");
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
                        let trunc = &summary[..floor_char_boundary(summary, 60)];
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

fn floor_char_boundary(s: &str, idx: usize) -> usize {
    let idx = idx.min(s.len());
    if s.is_char_boundary(idx) {
        return idx;
    }

    let mut boundary = 0;
    for (offset, _) in s.char_indices() {
        if offset > idx {
            break;
        }
        boundary = offset;
    }
    boundary
}
