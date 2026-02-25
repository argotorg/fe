use camino::Utf8PathBuf;
use common::InputDb;
use common::config::{Config, WorkspaceConfig};
use driver::{DependencyTree, DriverDataBase, init_ingot, workspace_members};
use resolver::workspace::discover_context;
use smol_str::SmolStr;
use url::Url;

pub fn print_tree(path: &Utf8PathBuf) -> bool {
    let mut had_errors = false;
    let mut db = DriverDataBase::default();
    if let Some(name) = name_candidate(path) {
        match print_workspace_member_tree_by_name(&mut db, &name) {
            Ok(had_init_diagnostics) => {
                had_errors |= had_init_diagnostics;
            }
            Err(err) => {
                eprintln!("Error: {err}");
                had_errors = true;
            }
        }
        return had_errors;
    }

    let target_url = match ingot_url(path) {
        Ok(url) => url,
        Err(message) => {
            eprintln!("Error: {message}");
            return true;
        }
    };

    let had_init_diagnostics = init_ingot(&mut db, &target_url);
    if had_init_diagnostics {
        had_errors = true;
    }
    match config_from_db(&db, &target_url) {
        Ok(Some(Config::Workspace(workspace_config))) => {
            if let Err(err) = print_workspace_trees(&db, &workspace_config, &target_url) {
                eprintln!("Error: {err}");
                had_errors = true;
            }
            return had_errors;
        }
        Ok(_) => {}
        Err(err) => {
            eprintln!("Error: {err}");
            had_errors = true;
        }
    }

    let tree = DependencyTree::build(&db, &target_url);
    print!("{}", tree.display_to(common::color::ColorTarget::Stdout));
    had_errors
}

fn ingot_url(path: &Utf8PathBuf) -> Result<Url, String> {
    let canonical_path = path
        .canonicalize_utf8()
        .map_err(|_| format!("invalid or non-existent directory path: {path}"))?;

    if !canonical_path.is_dir() {
        return Err(format!("{path} is not a directory"));
    }

    Url::from_directory_path(canonical_path.as_str())
        .map_err(|_| format!("invalid directory path: {path}"))
}

fn name_candidate(path: &Utf8PathBuf) -> Option<String> {
    if path.exists() {
        return None;
    }
    let value = path.as_str();
    if value.is_empty() {
        return None;
    }
    if value.chars().all(|c| c.is_alphanumeric() || c == '_') {
        Some(value.to_string())
    } else {
        None
    }
}

fn config_from_db(db: &DriverDataBase, base_url: &Url) -> Result<Option<Config>, String> {
    let config_url = base_url
        .join("fe.toml")
        .map_err(|_| format!("Failed to locate fe.toml for {base_url}"))?;
    let Some(file) = db.workspace().get(db, &config_url) else {
        return Ok(None);
    };
    let config = Config::parse(file.text(db))
        .map_err(|err| format!("Failed to parse {config_url}: {err}"))?;
    Ok(Some(config))
}

fn print_workspace_member_tree_by_name(
    db: &mut DriverDataBase,
    name: &str,
) -> Result<bool, String> {
    let cwd = std::env::current_dir()
        .map_err(|err| format!("Failed to read current directory: {err}"))?;
    let cwd = Utf8PathBuf::from_path_buf(cwd)
        .map_err(|_| "Current directory is not valid UTF-8".to_string())?;
    let cwd_url = Url::from_directory_path(cwd.as_std_path())
        .map_err(|_| "Failed to convert current directory to URL".to_string())?;
    let discovery =
        discover_context(&cwd_url).map_err(|err| format!("Failed to discover context: {err}"))?;
    let workspace_url = discovery
        .workspace_root
        .ok_or_else(|| "No workspace config found in current directory hierarchy".to_string())?;

    let had_init_diagnostics = init_ingot(db, &workspace_url);
    let mut matches =
        db.dependency_graph()
            .workspace_members_by_name(db, &workspace_url, &SmolStr::new(name));

    if matches.is_empty() {
        return Err(format!("No workspace member named \"{name}\""));
    }
    if matches.len() > 1 {
        return Err(format!(
            "Multiple workspace members named \"{name}\"; specify a path instead"
        ));
    }

    let member = matches.remove(0);
    let tree = DependencyTree::build(db, &member.url);
    print!("{}", tree.display_to(common::color::ColorTarget::Stdout));
    Ok(had_init_diagnostics)
}

fn print_workspace_trees(
    db: &DriverDataBase,
    workspace_config: &WorkspaceConfig,
    workspace_url: &Url,
) -> Result<(), String> {
    let members = workspace_members(&workspace_config.workspace, workspace_url)?;
    if members.is_empty() {
        let paths: Vec<&str> = workspace_config
            .workspace
            .members
            .iter()
            .map(|m| m.path.as_str())
            .collect();
        if paths.is_empty() {
            eprintln!("Warning: No workspace members configured in fe.toml");
        } else {
            eprintln!(
                "Warning: No workspace members found. The configured member paths do not exist:\n  {}",
                paths.join("\n  ")
            );
        }
        return Ok(());
    }

    for (idx, member) in members.iter().enumerate() {
        if idx > 0 {
            println!();
        }
        let label = member
            .name
            .as_deref()
            .map(|name| name.to_string())
            .unwrap_or_else(|| member.url.to_string());
        println!("== {label} ==");
        let tree = DependencyTree::build(db, &member.url);
        print!("{}", tree.display_to(common::color::ColorTarget::Stdout));
    }
    Ok(())
}
