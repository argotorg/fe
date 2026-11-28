//! The embedded `core` and `std` ingots are inputs of high durability, so a
//! change to the user's files or dependency graph leaves their queries
//! verified without walking their dependencies.

use std::sync::{Arc, Mutex};

use camino::Utf8PathBuf;
use common::{
    InputDb, SalsaEventCounters,
    compilation::Target,
    ingot::IngotBaseUrl,
    stdlib::{BUILTIN_CORE_BASE_URL, BUILTIN_STD_BASE_URL, load_library_from_path},
};
use fe_driver::DriverDataBase;
use hir::{
    analysis::{HirAnalysisDb, initialize_analysis_pass, semantic::SemanticAccessAnalysisPass},
    hir_def::HirIngot,
};
use salsa::Setter;
use tempfile::TempDir;
use url::Url;

/// The diagnostics of every analysis pass over `core` and `std`.
#[salsa::tracked(returns(copy))]
fn builtin_diagnostic_count(db: &dyn HirAnalysisDb) -> usize {
    let mut passes = initialize_analysis_pass();
    passes.add_module_pass("SemanticAccess", Box::new(SemanticAccessAnalysisPass));
    [BUILTIN_CORE_BASE_URL, BUILTIN_STD_BASE_URL]
        .into_iter()
        .map(|base| {
            let ingot = Url::parse(base).unwrap().ingot(db).unwrap();
            passes.run_on_module_tree(db, ingot.module_tree(db)).len()
        })
        .sum()
}

/// The files of `core` and `std`.
#[salsa::tracked(returns(copy))]
fn builtin_file_count(db: &dyn HirAnalysisDb) -> usize {
    [BUILTIN_CORE_BASE_URL, BUILTIN_STD_BASE_URL]
        .into_iter()
        .map(|base| {
            let ingot = Url::parse(base).unwrap().ingot(db).unwrap();
            ingot.files(db).iter().count()
        })
        .sum()
}

/// A user module that uses `core` and `std`.
fn user_lib(name: &str) -> String {
    format!(
        "use std::evm::Address\n\npub fn {name}() -> Option<Address> {{\n    Option::Some(Address::zero())\n}}\n"
    )
}

struct Fixture {
    db: DriverDataBase,
    counters: Arc<Mutex<SalsaEventCounters>>,
    user: Url,
}

impl Fixture {
    /// A database with a user ingot that uses `std`, analyzed along with the
    /// builtins.
    fn new() -> Self {
        let counters = Arc::new(Mutex::new(SalsaEventCounters::default()));
        let mut db = DriverDataBase::with_salsa_event_counters(Arc::clone(&counters));
        let user = Url::parse("file:///user/").unwrap();
        user.touch(
            &mut db,
            Utf8PathBuf::from("fe.toml"),
            Some("[ingot]\nname = \"user\"\nversion = \"0.1.0\"\n".into()),
        );
        user.touch(
            &mut db,
            Utf8PathBuf::from("src/lib.fe"),
            Some(user_lib("first")),
        );
        let mut fixture = Self { db, counters, user };
        fixture.analyze();
        fixture
    }

    fn edit_user_lib(&mut self, name: &str) {
        let lib = self.user.join("src/lib.fe").unwrap();
        self.db
            .workspace()
            .update(&mut self.db, lib, user_lib(name));
    }

    fn analyze(&mut self) {
        assert_eq!(builtin_diagnostic_count(&self.db), 0);
        assert_eq!(builtin_file_count(&self.db), self.builtin_files());
        let user = self.user.ingot(&self.db).unwrap();
        let diags = self.db.run_on_ingot(user);
        assert!(diags.is_empty(), "{}", diags.format_diags(&self.db));
    }

    fn builtin_files(&self) -> usize {
        [BUILTIN_CORE_BASE_URL, BUILTIN_STD_BASE_URL]
            .into_iter()
            .map(|base| {
                self.db
                    .workspace()
                    .all_files(&self.db)
                    .filter(|(url, _)| url.as_str().starts_with(base))
                    .count()
            })
            .sum()
    }

    /// The salsa events of `f`.
    fn events(&self, f: impl FnOnce(&DriverDataBase)) -> SalsaEventCounters {
        *self.counters.lock().unwrap() = SalsaEventCounters::default();
        f(&self.db);
        self.counters.lock().unwrap().clone()
    }

    /// Checks that the builtin queries are verified by their durability
    /// alone: one validation each, and nothing executed.
    fn assert_builtins_reused(&mut self, change: &str) {
        let events = self.events(|db| {
            assert_eq!(builtin_diagnostic_count(db), 0);
            builtin_file_count(db);
        });
        assert_eq!(
            (events.will_execute, events.did_validate_memoized_value),
            (0, 2),
            "{change}: {events:?}"
        );
        self.analyze();
    }
}

#[test]
fn user_changes_skip_builtin_validation() {
    let mut fixture = Fixture::new();
    fixture.edit_user_lib("second");
    fixture.assert_builtins_reused("editing a user file");

    let extra = fixture.user.touch(
        &mut fixture.db,
        Utf8PathBuf::from("src/extra.fe"),
        Some("pub fn extra() {}\n".into()),
    );
    fixture.assert_builtins_reused("adding a user file");

    let extra = extra.url(&fixture.db).unwrap();
    fixture.db.workspace().remove(&mut fixture.db, &extra);
    fixture.assert_builtins_reused("removing a user file");

    let user = fixture.user.clone();
    let graph = fixture.db.dependency_graph();
    graph.register_root_ingot(&mut fixture.db, &user);
    graph.ensure_node(&mut fixture.db, &user);
    fixture.assert_builtins_reused("changing the dependency graph");
}

#[test]
fn settings_changes_invalidate_builtins() {
    let mut fixture = Fixture::new();
    let settings = fixture.db.compilation_settings();
    settings.set_target(&mut fixture.db).to(Target::Native);
    let events = fixture.events(|db| assert_eq!(builtin_diagnostic_count(db), 0));
    assert!(events.will_execute > 0, "changing the target: {events:?}");
    fixture.analyze();
    fixture.edit_user_lib("second");
    fixture.assert_builtins_reused("editing a user file after changing the target");

    let options = fixture.db.compiler_options();
    options.set_recovery_mode(&mut fixture.db).to(false);
    let events = fixture.events(|db| assert_eq!(builtin_diagnostic_count(db), 0));
    assert!(
        events.will_execute > 0,
        "changing the recovery mode: {events:?}"
    );
}

#[test]
fn library_path_replaces_builtins() {
    let mut fixture = Fixture::new();
    let embedded = builtin_file_count(&fixture.db);

    let library = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(library.path().to_path_buf()).unwrap();
    for (name, lib) in [
        ("core", "pub fn core_fn() {}\n"),
        ("std", "pub fn std_fn() {}\n"),
    ] {
        let dir = root.join(name);
        std::fs::create_dir_all(dir.join("src")).unwrap();
        std::fs::write(
            dir.join("fe.toml"),
            format!("[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n"),
        )
        .unwrap();
        std::fs::write(dir.join("src/lib.fe"), lib).unwrap();
    }
    load_library_from_path(&mut fixture.db, &root).unwrap();

    let events = fixture.events(|db| assert_eq!(builtin_file_count(db), 4));
    assert!(events.will_execute > 0, "loading a library: {events:?}");
    assert_ne!(embedded, 4);
    let core_lib = Url::parse(BUILTIN_CORE_BASE_URL)
        .unwrap()
        .join("src/lib.fe")
        .unwrap();
    let file = fixture.db.workspace().get(&fixture.db, &core_lib).unwrap();
    assert_eq!(file.text(&fixture.db), "pub fn core_fn() {}\n");

    // The replacement is as durable as the embedded library.
    fixture.edit_user_lib("second");
    let events = fixture.events(|db| assert_eq!(builtin_file_count(db), 4));
    assert_eq!(
        (events.will_execute, events.did_validate_memoized_value),
        (0, 1),
        "editing a user file after loading a library: {events:?}"
    );
}
