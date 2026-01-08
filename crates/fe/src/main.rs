#![allow(clippy::print_stderr, clippy::print_stdout)]
mod check;
#[cfg(not(target_arch = "wasm32"))]
mod tree;

use std::{env, fs};

use camino::Utf8PathBuf;
use check::check;
use clap::{Parser, Subcommand};
use colored::Colorize;
use common::config::{Manifest, WorkspaceMemberSelection};
use fmt as fe_fmt;
use glob::Pattern;
use similar::{ChangeTag, TextDiff};
use toml::Value;
use walkdir::WalkDir;

const DEFAULT_INGOT_VERSION: &str = "0.1.0";

#[derive(Debug, Clone, Parser)]
#[command(version, about, long_about = None)]
pub struct Options {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    Build,
    Check {
        #[arg(default_value_t = default_project_path())]
        path: Utf8PathBuf,
        #[arg(short, long)]
        core: Option<Utf8PathBuf>,
        #[arg(long)]
        dump_mir: bool,
        #[arg(long)]
        emit_yul_min: bool,
    },
    #[cfg(not(target_arch = "wasm32"))]
    Tree {
        path: Utf8PathBuf,
    },
    /// Format Fe source code.
    Fmt {
        /// Path to a Fe source file or directory. If omitted, formats all .fe files in the current project.
        path: Option<Utf8PathBuf>,
        /// Check if files are formatted, but do not write changes.
        #[arg(long)]
        check: bool,
    },
    Init {
        /// Path to create the ingot or workspace in. Defaults to current directory.
        path: Option<Utf8PathBuf>,
        /// Create a workspace instead of a single ingot.
        #[arg(long)]
        workspace: bool,
    },
    New,
}

fn default_project_path() -> Utf8PathBuf {
    driver::files::find_project_root().unwrap_or(Utf8PathBuf::from("."))
}

fn main() {
    let opts = Options::parse();
    run(&opts);
}
pub fn run(opts: &Options) {
    match &opts.command {
        Command::Build => eprintln!("`fe build` doesn't work at the moment"),
        Command::Check {
            path,
            core: _,
            dump_mir,
            emit_yul_min,
        } => {
            //: TODO readd custom core
            check(path, *dump_mir, *emit_yul_min);
        }
        #[cfg(not(target_arch = "wasm32"))]
        Command::Tree { path } => {
            tree::print_tree(path);
        }
        Command::Fmt { path, check } => {
            run_fmt(path.as_ref(), *check);
        }
        Command::Init { path, workspace } => {
            if let Err(err) = init_command(path.as_ref(), *workspace) {
                eprintln!("❌ {err}");
                std::process::exit(1);
            }
        }
        Command::New => eprintln!("`fe new` doesn't work at the moment"),
    }
}

fn run_fmt(path: Option<&Utf8PathBuf>, check: bool) {
    let config = fe_fmt::Config::default();

    // Collect files to format
    let files: Vec<Utf8PathBuf> = match path {
        Some(p) if p.is_file() => vec![p.clone()],
        Some(p) if p.is_dir() => collect_fe_files(p),
        Some(p) => {
            eprintln!("Path does not exist: {}", p);
            std::process::exit(1);
        }
        None => {
            // Find project root and format all .fe files in src/
            match driver::files::find_project_root() {
                Some(root) => collect_fe_files(&root.join("src")),
                None => {
                    eprintln!(
                        "No fe.toml found. Run from a Fe project directory or specify a path."
                    );
                    std::process::exit(1);
                }
            }
        }
    };

    if files.is_empty() {
        eprintln!("No .fe files found");
        std::process::exit(1);
    }

    let mut unformatted_files = Vec::new();
    let mut error_count = 0;

    for file in &files {
        match format_single_file(file, &config, check) {
            FormatResult::Unchanged => {}
            FormatResult::Formatted {
                original,
                formatted,
            } => {
                if check {
                    print_diff(file, &original, &formatted);
                    unformatted_files.push(file.clone());
                }
            }
            FormatResult::ParseError(errs) => {
                eprintln!("Skipping {} (parse errors):", file);
                for err in errs {
                    eprintln!("  {}", err.msg());
                }
            }
            FormatResult::IoError(err) => {
                eprintln!("Error processing {}: {}", file, err);
                error_count += 1;
            }
        }
    }

    if check && !unformatted_files.is_empty() {
        std::process::exit(1);
    }

    if error_count > 0 {
        std::process::exit(1);
    }
}

fn print_diff(path: &Utf8PathBuf, original: &str, formatted: &str) {
    let diff = TextDiff::from_lines(original, formatted);

    println!("{}", format!("Diff {}:", path).bold());
    for hunk in diff.unified_diff().context_radius(3).iter_hunks() {
        // Print hunk header
        println!("{}", format!("{}", hunk.header()).cyan());
        for change in hunk.iter_changes() {
            match change.tag() {
                ChangeTag::Delete => print!("{}", format!("-{}", change).red()),
                ChangeTag::Insert => print!("{}", format!("+{}", change).green()),
                ChangeTag::Equal => print!(" {}", change),
            };
        }
    }
    println!();
}

fn collect_fe_files(dir: &Utf8PathBuf) -> Vec<Utf8PathBuf> {
    if !dir.exists() {
        return Vec::new();
    }

    WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "fe"))
        .filter_map(|e| Utf8PathBuf::from_path_buf(e.into_path()).ok())
        .collect()
}

enum FormatResult {
    Unchanged,
    Formatted { original: String, formatted: String },
    ParseError(Vec<fe_fmt::ParseError>),
    IoError(std::io::Error),
}

fn format_single_file(path: &Utf8PathBuf, config: &fe_fmt::Config, check: bool) -> FormatResult {
    let original = match fs::read_to_string(path.as_std_path()) {
        Ok(s) => s,
        Err(e) => return FormatResult::IoError(e),
    };

    let formatted = match fe_fmt::format_str(&original, config) {
        Ok(f) => f,
        Err(fe_fmt::FormatError::ParseErrors(errs)) => return FormatResult::ParseError(errs),
        Err(fe_fmt::FormatError::Io(e)) => return FormatResult::IoError(e),
    };

    if formatted == original {
        return FormatResult::Unchanged;
    }

    if !check {
        if let Err(e) = fs::write(path.as_std_path(), &formatted) {
            return FormatResult::IoError(e);
        }
        println!("Formatted {}", path);
    }

    FormatResult::Formatted {
        original,
        formatted,
    }
}

fn init_command(path: Option<&Utf8PathBuf>, workspace: bool) -> Result<(), String> {
    let target = absolute_target(path)?;
    let workspace_root = if workspace {
        None
    } else {
        find_workspace_root(&target)
    };

    if target.exists() && target.is_file() {
        return Err(format!(
            "Target path {} exists and is a file; expected directory",
            target
        ));
    }

    if !target.exists() {
        fs::create_dir_all(&target)
            .map_err(|err| format!("Failed to create directory {target}: {err}"))?;
    }

    if workspace {
        init_workspace_layout(&target)
    } else {
        init_ingot_layout(&target, None)?;
        if let Some(root) = workspace_root {
            update_workspace_members(&root, &target)?;
        }
        Ok(())
    }
}

fn init_workspace_layout(base: &Utf8PathBuf) -> Result<(), String> {
    let ingots_dir = base.join("ingots");
    fs::create_dir_all(&ingots_dir)
        .map_err(|err| format!("Failed to create ingots directory {}: {err}", ingots_dir))?;

    let workspace_name = infer_workspace_name(base);
    let workspace_manifest = base.join("fe.toml");
    write_if_absent(
        &workspace_manifest,
        format!(
            r#"# Workspace manifest
name = "{}"
version = "{}"

# Members can be a flat array or a table with main/dev groups.
members = ["ingots/*"]
# members = {{ main = ["ingots/*"], dev = ["examples/*"] }}

# Named members enable name+version resolution inside the workspace.
# members = [
#   {{ path = "ingots/core", name = "core", version = "0.1.0" }},
#   {{ path = "ingots/utils", name = "utils", version = "0.2.0" }},
# ]

# Paths to exclude from member discovery.
exclude = ["target"]

# Optional workspace-level dependencies (applied by tooling that supports it).
# [dependencies]
# remote_util = {{ source = "https://example.com/fe.git", rev = "abcd1234", path = "ingots/util" }}

# Optional scripts for common tasks.
# [scripts]
# fmt = "fe fmt"
# test = "fe check"
"#,
            workspace_name, DEFAULT_INGOT_VERSION
        ),
    )?;

    Ok(())
}

fn init_ingot_layout(base: &Utf8PathBuf, explicit_name: Option<String>) -> Result<(), String> {
    fs::create_dir_all(base)
        .map_err(|err| format!("Failed to create ingot directory {base}: {err}"))?;

    let src_dir = base.join("src");
    fs::create_dir_all(&src_dir)
        .map_err(|err| format!("Failed to create src directory {src_dir}: {err}"))?;

    let name = explicit_name.unwrap_or_else(|| infer_ingot_name(base));
    let manifest = base.join("fe.toml");
    write_if_absent(
        &manifest,
        format!(
            "# Ingot manifest\n[ingot]\nname = \"{}\"\nversion = \"{}\"\n\n# Optional dependencies\n# [dependencies]\n# utils = {{ path = \"../utils\" }}\n",
            name, DEFAULT_INGOT_VERSION
        ),
    )?;

    let src_lib = src_dir.join("lib.fe");
    write_if_absent(&src_lib, "pub fn main() {\n    // add your code here\n}\n")?;

    Ok(())
}

fn write_if_absent(path: &Utf8PathBuf, content: impl AsRef<str>) -> Result<(), String> {
    if path.exists() {
        return Err(format!("Refusing to overwrite existing {}", path));
    }
    fs::write(path, content.as_ref()).map_err(|err| format!("Failed to write {}: {err}", path))?;
    Ok(())
}

fn infer_ingot_name(path: &Utf8PathBuf) -> String {
    let fallback = "ingot".to_string();
    let Some(stem) = path.file_name().map(|s| s.to_string()) else {
        return fallback;
    };
    let mut sanitized = String::new();
    for c in stem.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            sanitized.push(c);
        } else {
            sanitized.push('_');
        }
    }
    if sanitized.is_empty() {
        fallback
    } else {
        sanitized
    }
}

fn infer_workspace_name(path: &Utf8PathBuf) -> String {
    path.file_name()
        .map(|s| s.to_string())
        .unwrap_or_else(|| "workspace".to_string())
}

fn absolute_target(path: Option<&Utf8PathBuf>) -> Result<Utf8PathBuf, String> {
    let cwd =
        env::current_dir().map_err(|err| format!("Failed to read current directory: {err}"))?;
    let cwd = Utf8PathBuf::from_path_buf(cwd)
        .map_err(|_| "Current directory is not valid UTF-8".to_string())?;

    match path {
        Some(p) if p.is_absolute() => Ok(p.clone()),
        Some(p) => Ok(cwd.join(p)),
        None => Ok(cwd),
    }
}

fn find_workspace_root(target: &Utf8PathBuf) -> Option<Utf8PathBuf> {
    for ancestor in target.ancestors() {
        let manifest = ancestor.join("fe.toml");
        if manifest.is_file()
            && let Ok(content) = fs::read_to_string(manifest.as_std_path())
            && let Ok(manifest) = Manifest::parse(&content)
            && matches!(manifest, Manifest::Workspace(_))
        {
            return Some(ancestor.to_path_buf());
        }
    }
    None
}

fn update_workspace_members(
    workspace_root: &Utf8PathBuf,
    member_dir: &Utf8PathBuf,
) -> Result<(), String> {
    let manifest_path = workspace_root.join("fe.toml");
    let manifest_str = fs::read_to_string(manifest_path.as_std_path())
        .map_err(|err| format!("Failed to read {}: {err}", manifest_path))?;
    let manifest = Manifest::parse(&manifest_str)
        .map_err(|err| format!("Failed to parse {}: {err}", manifest_path))?;
    let workspace = match manifest {
        Manifest::Workspace(workspace_manifest) => workspace_manifest.workspace,
        Manifest::Ingot(_) => {
            return Err(format!("{} is not a workspace manifest", manifest_path));
        }
    };

    let relative_member = member_dir
        .strip_prefix(workspace_root)
        .map_err(|_| "Ingot path is not inside workspace".to_string())?
        .to_string();

    if relative_member.is_empty() {
        return Ok(());
    }

    if matches_any_pattern(&workspace.exclude, &relative_member, "exclude")? {
        return Err(format!(
            "Ingot path {relative_member} is excluded by workspace configuration"
        ));
    }

    let member_patterns = workspace_member_patterns(&workspace);
    if matches_any_pattern(&member_patterns, &relative_member, "member")? {
        return Ok(());
    }

    let mut value: Value = manifest_str
        .parse()
        .map_err(|err| format!("Failed to parse {}: {err}", manifest_path))?;
    let root_table = value
        .as_table_mut()
        .ok_or_else(|| format!("{} is not a workspace manifest", manifest_path))?;
    let members = workspace_members_array(root_table)?;

    if !members
        .iter()
        .any(|value| value.as_str() == Some(relative_member.as_str()))
    {
        members.push(Value::String(relative_member));
    }

    let updated = toml::to_string_pretty(&value)
        .map_err(|err| format!("Failed to serialize {}: {err}", manifest_path))?;
    fs::write(manifest_path.as_std_path(), updated)
        .map_err(|err| format!("Failed to write {}: {err}", manifest_path))?;
    Ok(())
}

fn workspace_member_patterns(
    workspace: &common::config::WorkspaceConfig,
) -> Vec<smol_str::SmolStr> {
    workspace
        .members_for_selection(WorkspaceMemberSelection::All)
        .into_iter()
        .map(|spec| spec.path)
        .collect()
}

fn matches_any_pattern(
    patterns: &[smol_str::SmolStr],
    candidate: &str,
    label: &str,
) -> Result<bool, String> {
    for pattern in patterns {
        let compiled = Pattern::new(pattern.as_str())
            .map_err(|err| format!("Invalid {label} pattern \"{pattern}\": {err}"))?;
        if compiled.matches(candidate) {
            return Ok(true);
        }
    }
    Ok(false)
}

fn workspace_members_array(root_table: &mut toml::value::Table) -> Result<&mut Vec<Value>, String> {
    let members_value = root_table
        .entry("members")
        .or_insert_with(|| Value::Array(Vec::new()));

    match members_value {
        Value::Array(array) => Ok(array),
        Value::Table(member_table) => {
            let main_value = member_table
                .entry("main")
                .or_insert_with(|| Value::Array(Vec::new()));
            main_value
                .as_array_mut()
                .ok_or_else(|| "members.main is not an array".to_string())
        }
        _ => Err("members is not an array or table".to_string()),
    }
}
