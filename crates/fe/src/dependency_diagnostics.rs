use std::{
    collections::{HashSet, VecDeque},
    fmt::Write as _,
};

use common::{InputDb, diagnostics::CompleteDiagnostic};
use driver::{DriverDataBase, db::DiagnosticsCollection};
use hir::{Ingot, hir_def::TopLevelMod};
use url::Url;

use crate::workspace_ingot::ingot_has_source_files;

#[derive(Default)]
pub(crate) struct DependencyIssues<'db> {
    issues: Vec<DependencyIssue<'db>>,
}

pub(crate) struct CompilationDiagnostics<'db> {
    pub(crate) hir: DiagnosticsCollection<'db>,
    pub(crate) dependencies: DependencyIssues<'db>,
    pub(crate) mir: Vec<CompleteDiagnostic>,
}

enum DependencyIssue<'db> {
    MissingSourceFiles(Url),
    Diagnostics {
        url: Url,
        hir: DiagnosticsCollection<'db>,
        mir: Vec<CompleteDiagnostic>,
    },
}

impl DependencyIssue<'_> {
    fn format(&self, db: &DriverDataBase, out: &mut String) {
        let url = match self {
            Self::MissingSourceFiles(url) | Self::Diagnostics { url, .. } => url,
        };
        append_dependency_header(db, url, out);
        match self {
            DependencyIssue::MissingSourceFiles(url) => {
                let _ = writeln!(out, "Error: Could not find source files for ingot {url}");
            }
            DependencyIssue::Diagnostics { hir, mir, .. } => {
                if !hir.is_empty() {
                    out.push_str(&hir.format_diags(db));
                }
                if !mir.is_empty() {
                    out.push_str(&db.format_complete_diagnostics(mir));
                }
            }
        }
        if !out.ends_with('\n') {
            out.push('\n');
        }
    }
}

impl<'db> DependencyIssues<'db> {
    pub(crate) fn collect_all(db: &'db DriverDataBase, ingot_url: &Url) -> Self {
        let mut seen = HashSet::new();
        Self::collect(db, ingot_url, &mut seen)
    }

    pub(crate) fn collect(
        db: &'db DriverDataBase,
        ingot_url: &Url,
        seen: &mut HashSet<Url>,
    ) -> Self {
        let Some(root) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
            return Self { issues: Vec::new() };
        };
        seen.insert(root.base(db));

        let mut pending = root
            .dependencies(db)
            .into_iter()
            .map(|(_, url)| url)
            .collect::<VecDeque<_>>();
        let mut dependencies = Vec::new();
        while let Some(dependency_url) = pending.pop_front() {
            if !seen.insert(dependency_url.clone()) {
                continue;
            }
            let Some(ingot) = db.workspace().containing_ingot(db, dependency_url.clone()) else {
                continue;
            };
            pending.extend(ingot.dependencies(db).into_iter().map(|(_, url)| url));
            // The compiler-owned embedded libraries are immutable after database
            // initialization and are validated by dedicated whole-library tests. Source and
            // workspace replacements use ordinary URLs and remain in this validation closure.
            if matches!(dependency_url.scheme(), "builtin-core" | "builtin-std") {
                continue;
            }
            if !ingot_has_source_files(db, ingot) {
                dependencies.push(Err(dependency_url));
                continue;
            }
            let hir = db.run_on_ingot(ingot);
            dependencies.push(Ok((dependency_url, ingot, hir)));
        }

        let hir_has_errors = dependencies.iter().any(|dependency| match dependency {
            Ok((_, _, hir)) => hir.has_errors(db),
            Err(_) => true,
        });
        let issues =
            dependencies
                .into_iter()
                .filter_map(|dependency| match dependency {
                    Err(url) => Some(DependencyIssue::MissingSourceFiles(url)),
                    Ok((url, ingot, hir)) => {
                        let mir = if hir_has_errors {
                            Vec::new()
                        } else {
                            db.mir_diagnostics_for_ingot(ingot)
                        };
                        (!hir.is_empty() || !mir.is_empty())
                            .then_some(DependencyIssue::Diagnostics { url, hir, mir })
                    }
                })
                .collect();
        Self { issues }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.issues.is_empty()
    }

    pub(crate) fn message(&self) -> &'static str {
        if self.issues.len() == 1 {
            "Errors in dependency"
        } else {
            "Errors in dependencies"
        }
    }

    pub(crate) fn format(&self, db: &DriverDataBase) -> String {
        let mut out = String::new();
        let _ = writeln!(out, "Error: {}", self.message());
        for issue in &self.issues {
            issue.format(db, &mut out);
            out.push('\n');
        }
        out
    }
}

impl<'db> CompilationDiagnostics<'db> {
    pub(crate) fn for_top_mod(
        db: &'db DriverDataBase,
        top_mod: TopLevelMod<'db>,
        ingot_url: &Url,
    ) -> Self {
        Self::finish(
            db,
            db.run_on_top_mod(top_mod),
            || DependencyIssues::collect_all(db, ingot_url),
            || db.mir_diagnostics_for_top_mod(top_mod),
        )
    }

    pub(crate) fn for_ingot(db: &'db DriverDataBase, ingot: Ingot<'db>) -> Self {
        let ingot_url = ingot.base(db);
        Self::finish(
            db,
            db.run_on_ingot(ingot),
            || DependencyIssues::collect_all(db, &ingot_url),
            || db.mir_diagnostics_for_ingot(ingot),
        )
    }

    pub(crate) fn for_ingot_with_seen(
        db: &'db DriverDataBase,
        ingot: Ingot<'db>,
        seen: &mut HashSet<Url>,
    ) -> Self {
        let ingot_url = ingot.base(db);
        Self::finish(
            db,
            db.run_on_ingot(ingot),
            || DependencyIssues::collect(db, &ingot_url, seen),
            || db.mir_diagnostics_for_ingot(ingot),
        )
    }

    fn finish(
        db: &'db DriverDataBase,
        hir: DiagnosticsCollection<'db>,
        dependencies: impl FnOnce() -> DependencyIssues<'db>,
        mir: impl FnOnce() -> Vec<CompleteDiagnostic>,
    ) -> Self {
        if hir.has_errors(db) {
            return Self {
                hir,
                dependencies: DependencyIssues::default(),
                mir: Vec::new(),
            };
        }

        let dependencies = dependencies();
        let mir = if dependencies.is_empty() {
            mir()
        } else {
            Vec::new()
        };
        Self {
            hir,
            dependencies,
            mir,
        }
    }
}

fn append_dependency_header(db: &DriverDataBase, dependency_url: &Url, out: &mut String) {
    let dependency = if let Some(ingot) =
        db.workspace().containing_ingot(db, dependency_url.clone())
        && let Some(config) = ingot.config(db)
    {
        let name = config.metadata.name.as_deref().unwrap_or("unknown");
        if let Some(version) = &config.metadata.version {
            format!("Dependency: {name} (version: {version})")
        } else {
            format!("Dependency: {name}")
        }
    } else {
        "Dependency: <unknown>".to_string()
    };
    let _ = writeln!(out, "\n{dependency}\nURL: {dependency_url}\n");
}

#[cfg(test)]
mod tests {
    use common::{InputDb, ingot::IngotBaseUrl, stdlib::BUILTIN_CORE_BASE_URL};
    use driver::DriverDataBase;
    use url::Url;

    use super::CompilationDiagnostics;

    fn db_with_source_dependency(
        root_source: &str,
        dependency_name: &str,
        dependency_source: &str,
    ) -> (DriverDataBase, Url) {
        let mut db = DriverDataBase::default();
        let dependency_base = Url::parse("file:///dependency/").unwrap();
        dependency_base.touch(
            &mut db,
            "fe.toml".into(),
            Some(format!(
                "[ingot]\nname = \"{dependency_name}\"\nversion = \"1.0.0\"\n"
            )),
        );
        dependency_base.touch(
            &mut db,
            "src/lib.fe".into(),
            Some(dependency_source.to_string()),
        );

        let root_base = Url::parse("file:///root/").unwrap();
        root_base.touch(
            &mut db,
            "fe.toml".into(),
            Some(format!(
                "[ingot]\nname = \"root\"\nversion = \"1.0.0\"\n\n[dependencies]\n{dependency_name} = {{ path = \"../dependency\" }}\n"
            )),
        );
        root_base.touch(&mut db, "src/lib.fe".into(), Some(root_source.to_string()));
        (db, root_base.join("src/lib.fe").unwrap())
    }

    fn collect<'db>(db: &'db DriverDataBase, root_url: &Url) -> CompilationDiagnostics<'db> {
        let root_file = db.workspace().get(db, root_url).unwrap();
        CompilationDiagnostics::for_top_mod(db, db.top_mod(root_file), root_url)
    }

    #[test]
    fn invalid_root_stops_before_source_dependency_analysis() {
        let (db, root_url) = db_with_source_dependency(
            "fn root() { root_missing + }",
            "dep",
            "fn dep() { dep_missing + }",
        );
        let diagnostics = collect(&db, &root_url);

        assert!(diagnostics.hir.has_errors(&db));
        assert!(diagnostics.dependencies.is_empty());
        assert!(diagnostics.mir.is_empty());
    }

    #[test]
    fn source_dependency_errors_block_root_mir() {
        let (db, root_url) =
            db_with_source_dependency("fn root() {}", "dep", "fn dep() { dep_missing + }");
        let diagnostics = collect(&db, &root_url);
        let formatted = diagnostics.dependencies.format(&db);

        assert!(diagnostics.hir.is_empty());
        assert!(diagnostics.mir.is_empty());
        assert!(formatted.contains("Dependency: dep (version: 1.0.0)"));
        assert!(formatted.contains("expected expression"));
    }

    #[test]
    fn source_replacement_for_core_is_validated() {
        let (db, root_url) =
            db_with_source_dependency("fn root() {}", "core", "fn core() { core_missing + }");
        let diagnostics = collect(&db, &root_url);
        let formatted = diagnostics.dependencies.format(&db);

        assert!(diagnostics.hir.is_empty());
        assert!(diagnostics.mir.is_empty());
        assert!(formatted.contains("Dependency: core (version: 1.0.0)"));
        assert!(formatted.contains("expected expression"));
    }

    #[test]
    fn embedded_builtins_are_prevalidated() {
        let root_url = Url::parse("file:///dependency_diagnostics_root.fe").unwrap();
        let mut db = DriverDataBase::default();
        db.workspace()
            .touch(&mut db, root_url.clone(), Some("fn root() {}".to_string()));

        let core_url = Url::parse(BUILTIN_CORE_BASE_URL)
            .unwrap()
            .join("src/effect_ref.fe")
            .unwrap();
        let core_file = db.workspace().get(&db, &core_url).unwrap();
        let mut core_source = core_file.text(&db).to_string();
        core_source.push_str("\nfn invalid_self_ingot_path() { core::effect_ref::read(0) }\n");
        db.workspace().update(&mut db, core_url, core_source);

        let diagnostics = collect(&db, &root_url);

        assert!(diagnostics.hir.is_empty());
        assert!(diagnostics.dependencies.is_empty());
        assert!(diagnostics.mir.is_empty());
    }
}
