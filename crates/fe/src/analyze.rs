use std::collections::HashSet;

use camino::Utf8PathBuf;
use common::{
    InputDb,
    config::{Config, WorkspaceConfig},
    facts::{OwnedTypedFactSetExport, TypedFactRelationCount, TypedFactRelationSet, TypedFactSet},
    file::IngotFileKind,
};
use driver::{
    DriverDataBase,
    cli_target::{CliTarget, resolve_cli_target},
};
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::{
    RuntimePackage, build_runtime_package, build_test_runtime_package,
    legacy_runtime_package_origin_facts,
};
use salsa::Setter;
use serde::{Deserialize, Serialize};
use url::Url;

use crate::{
    AnalyzeFormat,
    dependency_diagnostics::DependencyIssues,
    workspace_ingot::{
        INGOT_REQUIRES_WORKSPACE_ROOT, WorkspaceMemberRef, select_workspace_member_paths,
    },
};

const ANALYZE_REPORT_SCHEMA_VERSION: u32 = 1;

#[derive(Debug)]
pub(crate) struct AnalyzeOutcome {
    pub(crate) has_errors: bool,
    pub(crate) output: String,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct AnalyzeOptions<'a> {
    profile: &'a str,
    format: AnalyzeFormat,
    include_tests: bool,
    include_origin_facts: bool,
    include_fact_relation_tables: bool,
    recovery_mode: bool,
}

impl<'a> AnalyzeOptions<'a> {
    pub(crate) fn new(
        profile: &'a str,
        format: AnalyzeFormat,
        include_tests: bool,
        include_origin_facts: bool,
        include_fact_relation_tables: bool,
        recovery_mode: bool,
    ) -> Result<Self, String> {
        if include_fact_relation_tables && !include_origin_facts {
            return Err(
                "`fe analyze --fact-relation-tables` requires `--origin-facts`".to_string(),
            );
        }
        if include_fact_relation_tables && format != AnalyzeFormat::Json {
            return Err("`fe analyze --fact-relation-tables` requires `--format json`".to_string());
        }
        Ok(Self {
            profile,
            format,
            include_tests,
            include_origin_facts,
            include_fact_relation_tables,
            recovery_mode,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum AnalyzePackageKind {
    Runtime,
    Tests,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct AnalyzeTargetReport {
    label: String,
    runtime_functions: usize,
    runtime_blocks: usize,
    runtime_statements: usize,
    runtime_terminators: usize,
    origin_facts: Option<AnalyzeOriginFactReport>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct AnalyzeOriginFactReport {
    total: usize,
    origin_nodes: usize,
    origin_links: usize,
    relation_counts: Vec<TypedFactRelationCount>,
    relation_tables: Option<TypedFactRelationSet>,
    facts: OwnedTypedFactSetExport,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct AnalyzeReport {
    schema_version: u32,
    profile: String,
    package_kind: AnalyzePackageKind,
    targets: Vec<AnalyzeTargetReport>,
}

#[allow(clippy::too_many_arguments)]
pub fn analyze(
    path: &Utf8PathBuf,
    ingot: Option<&str>,
    force_standalone: bool,
    profile: &str,
    format: AnalyzeFormat,
    include_tests: bool,
    include_origin_facts: bool,
    include_fact_relation_tables: bool,
    recovery_mode: bool,
) -> Result<bool, String> {
    let options = AnalyzeOptions::new(
        profile,
        format,
        include_tests,
        include_origin_facts,
        include_fact_relation_tables,
        recovery_mode,
    )?;
    let outcome = analyze_to_string(path, ingot, force_standalone, options)?;
    if !outcome.output.is_empty() {
        print!("{}", outcome.output);
    }
    Ok(outcome.has_errors)
}

pub(crate) fn analyze_to_string(
    path: &Utf8PathBuf,
    ingot: Option<&str>,
    force_standalone: bool,
    options: AnalyzeOptions<'_>,
) -> Result<AnalyzeOutcome, String> {
    let mut db = DriverDataBase::default();
    db.compiler_options()
        .set_recovery_mode(&mut db)
        .to(options.recovery_mode);
    db.compilation_settings()
        .set_profile(&mut db)
        .to(options.profile.into());

    let mut report = AnalyzeReport {
        schema_version: ANALYZE_REPORT_SCHEMA_VERSION,
        profile: options.profile.to_string(),
        package_kind: if options.include_tests {
            AnalyzePackageKind::Tests
        } else {
            AnalyzePackageKind::Runtime
        },
        targets: Vec::new(),
    };

    let target = resolve_cli_target(&mut db, path, force_standalone)?;
    let has_errors = match target {
        CliTarget::StandaloneFile(file_path) => {
            if ingot.is_some() {
                eprintln!("Error: {INGOT_REQUIRES_WORKSPACE_ROOT}");
                true
            } else {
                analyze_single_file(&mut db, &file_path, options, &mut report)
            }
        }
        CliTarget::Directory(dir_path) => {
            analyze_directory(&mut db, &dir_path, ingot, options, &mut report)
        }
    };

    report
        .targets
        .sort_by(|left, right| left.label.cmp(&right.label));
    let output = if has_errors {
        String::new()
    } else {
        render_report(&report, options.format)?
    };
    Ok(AnalyzeOutcome { has_errors, output })
}

fn analyze_single_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    options: AnalyzeOptions<'_>,
    report: &mut AnalyzeReport,
) -> bool {
    let canonical = match file_path.canonicalize_utf8() {
        Ok(path) => path,
        Err(err) => {
            eprintln!("Error: Cannot canonicalize {file_path}: {err}");
            return true;
        }
    };
    let file_url = match Url::from_file_path(&canonical) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid file path: {file_path}");
            return true;
        }
    };
    let content = match std::fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error: Failed to read file {file_path}: {err}");
            return true;
        }
    };

    db.workspace().touch(db, file_url.clone(), Some(content));
    let Some(file) = db.workspace().get(db, &file_url) else {
        eprintln!("Error: Could not process file {file_path}");
        return true;
    };
    let top_mod = db.top_mod(file);
    analyze_top_mod(db, file_path.as_str(), top_mod, options, report)
}

fn analyze_directory(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    ingot: Option<&str>,
    options: AnalyzeOptions<'_>,
    report: &mut AnalyzeReport,
) -> bool {
    let ingot_url = match dir_url(dir_path) {
        Ok(url) => url,
        Err(message) => {
            eprintln!("{message}");
            return true;
        }
    };

    if driver::init_ingot(db, &ingot_url) {
        return true;
    }

    let config = match config_from_db(db, &ingot_url) {
        Ok(Some(config)) => config,
        Ok(None) => {
            if ingot.is_some() {
                eprintln!("Error: {INGOT_REQUIRES_WORKSPACE_ROOT}");
            } else {
                eprintln!("Error: No fe.toml file found in the root directory");
            }
            return true;
        }
        Err(err) => {
            eprintln!("Error: {err}");
            return true;
        }
    };

    match config {
        Config::Workspace(workspace) => {
            analyze_workspace(db, dir_path, *workspace, ingot, options, report)
        }
        Config::Ingot(_) => {
            if ingot.is_some() {
                eprintln!("Error: {INGOT_REQUIRES_WORKSPACE_ROOT}");
                return true;
            }
            analyze_ingot_url(db, &ingot_url, options, report)
        }
    }
}

fn analyze_workspace(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    workspace_config: WorkspaceConfig,
    ingot: Option<&str>,
    options: AnalyzeOptions<'_>,
    report: &mut AnalyzeReport,
) -> bool {
    let workspace_url = match dir_url(dir_path) {
        Ok(url) => url,
        Err(message) => {
            eprintln!("{message}");
            return true;
        }
    };

    let members = match driver::workspace_members(&workspace_config.workspace, &workspace_url) {
        Ok(members) => members,
        Err(err) => {
            eprintln!("Error: Failed to resolve workspace members: {err}");
            return true;
        }
    };

    if members.is_empty() {
        return false;
    }

    let selected_member_paths = match select_workspace_member_paths(
        dir_path,
        dir_path,
        members
            .iter()
            .map(|member| WorkspaceMemberRef::new(member.path.as_path(), member.name.as_deref())),
        ingot,
    ) {
        Ok(paths) => paths.into_iter().collect::<HashSet<_>>(),
        Err(err) => {
            eprintln!("Error: {err}");
            return true;
        }
    };

    let mut seen = HashSet::new();
    let mut has_errors = false;
    for member in members {
        let member_path = dir_path.join(member.path.as_str());
        if !selected_member_paths.contains(&member_path) {
            continue;
        }
        has_errors |= analyze_ingot_and_dependencies(db, &member.url, options, report, &mut seen);
    }
    has_errors
}

fn analyze_ingot_url(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    options: AnalyzeOptions<'_>,
    report: &mut AnalyzeReport,
) -> bool {
    if db
        .workspace()
        .containing_ingot(db, ingot_url.clone())
        .is_none()
    {
        eprintln!("Error: Could not resolve ingot {ingot_url}");
        return true;
    }

    let mut seen = HashSet::new();
    analyze_ingot_and_dependencies(db, ingot_url, options, report, &mut seen)
}

fn analyze_ingot_and_dependencies(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    options: AnalyzeOptions<'_>,
    report: &mut AnalyzeReport,
    seen: &mut HashSet<Url>,
) -> bool {
    if !seen.insert(ingot_url.clone()) {
        return false;
    }

    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        eprintln!("Error: Could not resolve ingot {ingot_url}");
        return true;
    };

    if !ingot_has_source_files(db, ingot) {
        eprintln!("Error: Could not find source files for ingot {ingot_url}");
        return true;
    }

    let label = ingot
        .config(db)
        .and_then(|config| config.metadata.name)
        .map(|name| name.to_string())
        .unwrap_or_else(|| ingot_url.to_string());
    let mut has_errors = analyze_ingot_diagnostics(db, ingot);
    if !has_errors {
        has_errors |= analyze_top_mod(db, &label, ingot.root_mod(db), options, report);
    }

    let dependency_errors = DependencyIssues::collect(db, ingot_url, seen);
    if !dependency_errors.is_empty() {
        has_errors = true;
        eprint!("{}", dependency_errors.format(db));
    }

    has_errors
}

fn analyze_top_mod(
    db: &DriverDataBase,
    label: &str,
    top_mod: TopLevelMod<'_>,
    options: AnalyzeOptions<'_>,
    report: &mut AnalyzeReport,
) -> bool {
    if analyze_top_mod_diagnostics(db, top_mod, label) {
        return true;
    }

    let package = if options.include_tests {
        build_test_runtime_package(db, top_mod, None)
    } else {
        build_runtime_package(db, top_mod)
    };
    let package = match package {
        Ok(package) => package,
        Err(err) => {
            eprintln!("Error: Failed to build runtime package for {label}: {err}");
            return true;
        }
    };
    let origin_facts = options
        .include_origin_facts
        .then(|| legacy_runtime_package_origin_facts(db, package));
    report.targets.push(summarize_package(
        db,
        label,
        package,
        origin_facts,
        options.include_fact_relation_tables,
    ));
    false
}

fn analyze_top_mod_diagnostics(db: &DriverDataBase, top_mod: TopLevelMod<'_>, label: &str) -> bool {
    let hir_diags = db.run_on_top_mod(top_mod);
    let mut has_errors = false;
    let hir_has_errors = hir_diags.has_errors(db);
    if !hir_diags.is_empty() {
        eprintln!("errors in {label}");
        eprintln!();
        hir_diags.emit(db);
        has_errors = true;
    }

    let mir_diags = if hir_has_errors {
        Vec::new()
    } else {
        db.mir_diagnostics_for_top_mod(top_mod)
    };
    if !mir_diags.is_empty() {
        if !has_errors {
            eprintln!("errors in {label}");
            eprintln!();
        }
        db.emit_complete_diagnostics(&mir_diags);
        has_errors = true;
    }
    has_errors
}

fn analyze_ingot_diagnostics(db: &DriverDataBase, ingot: common::ingot::Ingot<'_>) -> bool {
    let hir_diags = db.run_on_ingot(ingot);
    let mut has_errors = false;
    let hir_has_errors = hir_diags.has_errors(db);
    if !hir_diags.is_empty() {
        hir_diags.emit(db);
        has_errors = true;
    }

    let mir_diags = if hir_has_errors {
        Vec::new()
    } else {
        db.mir_diagnostics_for_ingot(ingot)
    };
    if !mir_diags.is_empty() {
        db.emit_complete_diagnostics(&mir_diags);
        has_errors = true;
    }
    has_errors
}

fn summarize_package(
    db: &DriverDataBase,
    label: &str,
    package: RuntimePackage<'_>,
    origin_facts: Option<TypedFactSet>,
    include_fact_relation_tables: bool,
) -> AnalyzeTargetReport {
    let mut runtime_blocks = 0;
    let mut runtime_statements = 0;
    let mut runtime_terminators = 0;
    for function in package.functions(db) {
        let body = function.instance(db).body(db);
        runtime_blocks += body.blocks.len();
        runtime_statements += body
            .blocks
            .iter()
            .map(|block| block.stmts.len())
            .sum::<usize>();
        runtime_terminators += body.blocks.len();
    }

    AnalyzeTargetReport {
        label: label.to_string(),
        runtime_functions: package.functions(db).len(),
        runtime_blocks,
        runtime_statements,
        runtime_terminators,
        origin_facts: origin_facts.map(|facts| AnalyzeOriginFactReport {
            total: facts.len(),
            origin_nodes: facts.origin_node_count(),
            origin_links: facts.origin_link_count(),
            relation_counts: facts.relation_counts(),
            relation_tables: include_fact_relation_tables.then(|| facts.relation_tables()),
            facts: facts.export(),
        }),
    }
}

fn render_report(report: &AnalyzeReport, format: AnalyzeFormat) -> Result<String, String> {
    match format {
        AnalyzeFormat::Json => serde_json::to_string_pretty(report)
            .map(|mut json| {
                json.push('\n');
                json
            })
            .map_err(|err| format!("failed to render analyze JSON: {err}")),
        AnalyzeFormat::Text => {
            let mut out = String::new();
            out.push_str(&format!("Fe analysis ({:?})\n", report.package_kind));
            for target in &report.targets {
                out.push_str(&format!("\n{}\n", target.label));
                out.push_str(&format!(
                    "  runtime functions: {}\n",
                    target.runtime_functions
                ));
                out.push_str(&format!("  runtime blocks: {}\n", target.runtime_blocks));
                out.push_str(&format!(
                    "  runtime statements: {}\n",
                    target.runtime_statements
                ));
                out.push_str(&format!(
                    "  runtime terminators: {}\n",
                    target.runtime_terminators
                ));
                if let Some(origin_facts) = &target.origin_facts {
                    out.push_str(&format!("  origin facts: {}\n", origin_facts.total));
                    out.push_str(&format!("  origin nodes: {}\n", origin_facts.origin_nodes));
                    out.push_str(&format!("  origin links: {}\n", origin_facts.origin_links));
                }
            }
            Ok(out)
        }
    }
}

fn config_from_db(db: &DriverDataBase, ingot_url: &Url) -> Result<Option<Config>, String> {
    let config_url = ingot_url
        .join("fe.toml")
        .map_err(|_| format!("Failed to locate fe.toml for {ingot_url}"))?;
    let Some(file) = db.workspace().get(db, &config_url) else {
        return Ok(None);
    };
    let config = Config::parse(file.text(db))
        .map_err(|err| format!("Failed to parse {config_url}: {err}"))?;
    Ok(Some(config))
}

fn dir_url(path: &Utf8PathBuf) -> Result<Url, String> {
    let canonical_path = match path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            let cwd = std::env::current_dir()
                .map_err(|err| format!("Failed to read current directory: {err}"))?;
            let cwd = Utf8PathBuf::from_path_buf(cwd)
                .map_err(|_| "Current directory is not valid UTF-8".to_string())?;
            cwd.join(path)
        }
    };
    Url::from_directory_path(canonical_path.as_str())
        .map_err(|_| format!("Error: invalid or non-existent directory path: {path}"))
}

fn ingot_has_source_files(db: &DriverDataBase, ingot: common::ingot::Ingot<'_>) -> bool {
    ingot
        .files(db)
        .iter()
        .any(|(_, file)| matches!(file.kind(db), Some(IngotFileKind::Source)))
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;
    use tempfile::tempdir;

    fn json_options(include_tests: bool) -> AnalyzeOptions<'static> {
        AnalyzeOptions::new(
            "dev",
            AnalyzeFormat::Json,
            include_tests,
            false,
            false,
            false,
        )
        .unwrap()
    }

    fn json_options_with_origin_facts(include_relation_tables: bool) -> AnalyzeOptions<'static> {
        AnalyzeOptions::new(
            "dev",
            AnalyzeFormat::Json,
            false,
            true,
            include_relation_tables,
            false,
        )
        .unwrap()
    }

    fn analyze_report(output: &str) -> AnalyzeReport {
        serde_json::from_str(output).expect("analyze report should match schema")
    }

    #[test]
    fn analyze_standalone_file_reports_runtime_summary_json() {
        let temp = tempdir().expect("tempdir");
        let file_path = Utf8PathBuf::from_path_buf(temp.path().join("sample.fe")).unwrap();
        fs::write(
            file_path.as_std_path(),
            r#"
fn main() -> u256 {
    1
}
"#,
        )
        .expect("write fixture");

        let outcome =
            analyze_to_string(&file_path, None, true, json_options(false)).expect("analyze");

        assert!(!outcome.has_errors);
        let report = analyze_report(&outcome.output);
        assert_eq!(report.targets[0].label, file_path.as_str());
        assert!(report.targets[0].runtime_functions > 0);
        assert!(report.targets[0].runtime_statements > 0);
    }

    #[test]
    fn analyze_file_inside_ingot_uses_ingot_context_by_default() {
        let temp = tempdir().expect("tempdir");
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = root.join("src");
        fs::create_dir_all(src.as_std_path()).expect("create src");
        fs::write(
            root.join("fe.toml").as_std_path(),
            "[ingot]\nname = \"analyze_app\"\nversion = \"0.1.0\"\n",
        )
        .expect("write config");
        let file_path = src.join("lib.fe");
        fs::write(
            file_path.as_std_path(),
            r#"
fn main() -> u256 {
    1
}
"#,
        )
        .expect("write source");

        let outcome =
            analyze_to_string(&file_path, None, false, json_options(false)).expect("analyze");

        assert!(!outcome.has_errors);
        let report = analyze_report(&outcome.output);
        assert_eq!(report.targets[0].label, "analyze_app");
    }

    #[test]
    fn analyze_tests_mode_reports_test_package_kind() {
        let temp = tempdir().expect("tempdir");
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = root.join("src");
        fs::create_dir_all(src.as_std_path()).expect("create src");
        fs::write(
            root.join("fe.toml").as_std_path(),
            "[ingot]\nname = \"analyze_tests_app\"\nversion = \"0.1.0\"\n",
        )
        .expect("write config");
        let file_path = src.join("lib.fe");
        fs::write(
            file_path.as_std_path(),
            r#"
#[test]
fn test_sample() {
    let x: u256 = 1
}
"#,
        )
        .expect("write source");

        let outcome =
            analyze_to_string(&file_path, None, false, json_options(true)).expect("analyze");

        assert!(!outcome.has_errors);
        let report = analyze_report(&outcome.output);
        assert_eq!(report.package_kind, AnalyzePackageKind::Tests);
        assert!(report.targets[0].runtime_functions > 0);
    }

    #[test]
    fn analyze_origin_facts_are_report_views_over_typed_fact_set() {
        let temp = tempdir().expect("tempdir");
        let file_path = Utf8PathBuf::from_path_buf(temp.path().join("sample.fe")).unwrap();
        fs::write(
            file_path.as_std_path(),
            r#"
fn main() -> u256 {
    let x: u256 = 1
    x
}
"#,
        )
        .expect("write fixture");

        let outcome =
            analyze_to_string(&file_path, None, true, json_options_with_origin_facts(true))
                .expect("analyze");

        assert!(!outcome.has_errors);
        let report = analyze_report(&outcome.output);
        let origin_facts = report.targets[0]
            .origin_facts
            .as_ref()
            .expect("origin facts should be reported");
        assert!(origin_facts.origin_nodes > 0);
        assert_eq!(origin_facts.origin_nodes, origin_facts.facts.len());
        assert!(origin_facts.relation_tables.is_some());
    }
}
