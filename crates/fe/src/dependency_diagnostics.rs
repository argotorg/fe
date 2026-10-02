use std::{
    collections::{BTreeMap, HashSet, VecDeque, btree_map::Entry},
    fmt::Write as _,
};

use common::{
    InputDb,
    diagnostics::{CompleteDiagnostic, Severity},
};
use driver::{DriverDataBase, db::DiagnosticsCollection};
use hir::{Ingot, analysis::semantic::collect_blocked_semantic_bodies, hir_def::TopLevelMod};
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
            // Analyzing the whole embedded libraries is far too slow for every run. They are
            // validated by dedicated whole-library tests, and `collect_reached_builtins`
            // reports errors in the modules a compilation actually reaches. Source and
            // workspace replacements use ordinary URLs and remain in this validation closure.
            if is_embedded_builtin(&dependency_url) {
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

    /// Reports HIR diagnostics of the embedded core and std modules that the
    /// MIR analysis of `roots` reached despite errors.
    ///
    /// `collect` skips the embedded libraries, so an error in one of their
    /// bodies first shows up here: as a blocked body, which the borrow pass
    /// leaves silent, or as an internal error located in the library. Only the
    /// modules involved are analyzed, which keeps clean runs free of cost.
    fn collect_reached_builtins(
        db: &'db DriverDataBase,
        roots: &[TopLevelMod<'db>],
        mir: &[CompleteDiagnostic],
    ) -> Self {
        let blocked = roots
            .iter()
            .flat_map(|root| collect_blocked_semantic_bodies(db, *root))
            .map(|body| body.instance.key(db).owner(db).scope().top_mod(db));
        let mir_errors = mir
            .iter()
            .filter(|diag| diag.severity == Severity::Error)
            .filter_map(|diag| diag.primary_span())
            .map(|span| db.top_mod(span.file));

        let mut seen = HashSet::new();
        let mut by_ingot = BTreeMap::<Url, DiagnosticsCollection<'db>>::new();
        for module in blocked.chain(mir_errors) {
            let url = module.ingot(db).base(db);
            if !is_embedded_builtin(&url) || !seen.insert(module) {
                continue;
            }
            let hir = db.run_on_top_mod(module);
            if !hir.has_errors(db) {
                continue;
            }
            match by_ingot.entry(url) {
                Entry::Vacant(entry) => {
                    entry.insert(hir);
                }
                Entry::Occupied(mut entry) => entry.get_mut().append(hir),
            }
        }
        let issues = by_ingot
            .into_iter()
            .map(|(url, hir)| DependencyIssue::Diagnostics {
                url,
                hir,
                mir: Vec::new(),
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
            || vec![top_mod],
        )
    }

    pub(crate) fn for_ingot(db: &'db DriverDataBase, ingot: Ingot<'db>) -> Self {
        let ingot_url = ingot.base(db);
        Self::finish(
            db,
            db.run_on_ingot(ingot),
            || DependencyIssues::collect_all(db, &ingot_url),
            || db.mir_diagnostics_for_ingot(ingot),
            || ingot_modules(db, ingot),
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
            || ingot_modules(db, ingot),
        )
    }

    fn finish(
        db: &'db DriverDataBase,
        hir: DiagnosticsCollection<'db>,
        dependencies: impl FnOnce() -> DependencyIssues<'db>,
        mir: impl FnOnce() -> Vec<CompleteDiagnostic>,
        roots: impl FnOnce() -> Vec<TopLevelMod<'db>>,
    ) -> Self {
        if hir.has_errors(db) {
            return Self {
                hir,
                dependencies: DependencyIssues::default(),
                mir: Vec::new(),
            };
        }

        let dependencies = dependencies();
        if !dependencies.is_empty() {
            return Self {
                hir,
                dependencies,
                mir: Vec::new(),
            };
        }

        let mir = mir();
        let builtins = DependencyIssues::collect_reached_builtins(db, &roots(), &mir);
        if !builtins.is_empty() {
            return Self {
                hir,
                dependencies: builtins,
                mir: Vec::new(),
            };
        }
        Self {
            hir,
            dependencies,
            mir,
        }
    }
}

/// The compiler-owned embedded libraries. Source and workspace replacements of
/// core and std use ordinary URLs and are validated like other dependencies.
fn is_embedded_builtin(url: &Url) -> bool {
    matches!(url.scheme(), "builtin-core" | "builtin-std")
}

fn ingot_modules<'db>(db: &'db DriverDataBase, ingot: Ingot<'db>) -> Vec<TopLevelMod<'db>> {
    hir::lower::module_tree(db, ingot).all_modules().collect()
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

    /// Appends `core_addition` to the embedded `core::text` module and returns a
    /// database whose standalone root file calls into it.
    fn db_with_embedded_core_addition(
        core_addition: &str,
        root_source: &str,
    ) -> (DriverDataBase, Url) {
        let root_url = Url::parse("file:///dependency_diagnostics_root.fe").unwrap();
        let mut db = DriverDataBase::default();
        db.workspace()
            .touch(&mut db, root_url.clone(), Some(root_source.to_string()));

        let core_url = Url::parse(BUILTIN_CORE_BASE_URL)
            .unwrap()
            .join("src/text.fe")
            .unwrap();
        let core_file = db.workspace().get(&db, &core_url).unwrap();
        let mut core_source = core_file.text(&db).to_string();
        core_source.push_str(core_addition);
        db.workspace().update(&mut db, core_url, core_source);
        (db, root_url)
    }

    #[test]
    fn reached_embedded_builtin_blocked_body_reports_type_error() {
        // An invalid cast blocks the body; the borrow pass alone reports nothing.
        let (db, root_url) = db_with_embedded_core_addition(
            "\npub fn injected_cast(_ p: *u8) -> u256 { *p as u256 }\n",
            "use core::text\nfn root(p: *u8) -> u256 { text::injected_cast(p) }\n",
        );
        let diagnostics = collect(&db, &root_url);
        let formatted = diagnostics.dependencies.format(&db);

        assert!(diagnostics.hir.is_empty());
        assert!(diagnostics.mir.is_empty(), "{formatted}");
        assert!(formatted.contains("Dependency: core"), "{formatted}");
        assert!(formatted.contains("URL: builtin-core:/"), "{formatted}");
        assert!(
            formatted.contains("cast is not provably lossless"),
            "{formatted}"
        );
    }

    #[test]
    fn reached_embedded_builtin_internal_error_reports_type_error() {
        // Indexing with `u256` instead of `usize` otherwise surfaces as an
        // internal borrow checking error located in the builtin body.
        let (db, root_url) = db_with_embedded_core_addition(
            "\npub fn injected_index(_ len: u256) -> u256 {\n    let arr: ptr::MemArray<u256> = ptr::alloc_array(len)\n    arr[len]\n}\n",
            "use core::text\nfn root() -> u256 { text::injected_index(1) }\n",
        );
        let diagnostics = collect(&db, &root_url);
        let formatted = diagnostics.dependencies.format(&db);

        assert!(diagnostics.hir.is_empty());
        assert!(diagnostics.mir.is_empty(), "{formatted}");
        assert!(formatted.contains("URL: builtin-core:/"), "{formatted}");
        assert!(
            formatted.contains("expected `usize`, but `u256` is given"),
            "{formatted}"
        );
        assert!(!formatted.contains("internal"), "{formatted}");
    }
}
