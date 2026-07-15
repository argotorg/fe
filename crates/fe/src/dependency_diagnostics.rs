use std::{
    collections::{HashSet, VecDeque},
    fmt::Write as _,
};

use common::{InputDb, diagnostics::CompleteDiagnostic};
use driver::{DriverDataBase, db::DiagnosticsCollection};
use url::Url;

use crate::workspace_ingot::ingot_has_source_files;

pub(crate) struct DependencyIssues<'db> {
    issues: Vec<DependencyIssue<'db>>,
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
    use common::{InputDb, stdlib::BUILTIN_CORE_BASE_URL};
    use driver::DriverDataBase;
    use url::Url;

    use super::DependencyIssues;

    #[test]
    fn builtin_hir_errors_are_reported_before_dependent_codegen() {
        let root_url = Url::parse("file:///dependency_diagnostics_root.fe").unwrap();
        let mut clean_db = DriverDataBase::default();
        clean_db.workspace().touch(
            &mut clean_db,
            root_url.clone(),
            Some("fn root() {}".to_string()),
        );
        assert!(DependencyIssues::collect_all(&clean_db, &root_url).is_empty());

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

        let issues = DependencyIssues::collect_all(&db, &root_url);
        let formatted = issues.format(&db);

        assert!(formatted.contains("Errors in dependency"));
        assert!(formatted.contains("Dependency: core"));
        assert!(formatted.contains("`core` is not found"));
    }
}
