//! The generation rules, and the coherence of implementations across
//! dependencies, on real ingot layouts: a dependency and its dependents, each
//! with a manifest and a source directory. A provider reads
//! authored text, the implementation headers an author wrote, and what its
//! dependencies are; it does not read what generation produces in its own
//! ingot, and generated items do not change what authored code resolves to.
use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_ingots};
use hir::analysis::{
    semantic::{EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const},
    ty::ty_check::BodyOwner,
};
use url::Url;

const ROOT: &str = "file:///tmp/fe-package-boundary/";

fn url(path: &str) -> Url {
    Url::parse(&format!("{ROOT}{path}")).unwrap()
}

fn project(files: &[(&str, &str)]) -> DriverDataBase {
    let mut db = DriverDataBase::default();
    for (path, text) in files {
        db.workspace()
            .update(&mut db, url(path), (*text).to_string());
    }
    db
}

fn manifest(name: &str, dependencies: &[(&str, &str)]) -> String {
    let mut text = format!("[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n");
    if !dependencies.is_empty() {
        text += "\n[dependencies]\n";
        for (alias, path) in dependencies {
            text += &format!("{alias} = {{ path = \"../{path}\" }}\n");
        }
    }
    text
}

/// The ingots of a layout: `(directory, dependencies as (alias, directory),
/// source of src/lib.fe)`, and further files as `(path, text)`.
struct Layout {
    files: Vec<(String, String)>,
}

impl Layout {
    fn new() -> Self {
        Self { files: Vec::new() }
    }

    fn ingot(mut self, dir: &str, dependencies: &[(&str, &str)], lib: &str) -> Self {
        self.files
            .push((format!("{dir}/fe.toml"), manifest(dir, dependencies)));
        self.files
            .push((format!("{dir}/src/lib.fe"), lib.to_string()));
        self
    }

    fn db(&self) -> DriverDataBase {
        let files = self
            .files
            .iter()
            .map(|(path, text)| (path.as_str(), text.as_str()))
            .collect::<Vec<_>>();
        project(&files)
    }
}

/// Prepare the ingot at `root`; the authored database must not change.
fn prepare(layout: &Layout, root: &str) -> Result<DriverDataBase, String> {
    let db = layout.db();
    let result = prepare_ingots(&db, &[url(&format!("{root}/"))]);
    assert!(db.generation_inputs().requests(&db).is_empty());
    for (path, text) in &layout.files {
        let file = db.workspace().get(&db, &url(path)).unwrap();
        assert_eq!(file.text(&db).as_str(), text, "{path} was rewritten");
    }
    match result {
        Ok(Some(prepared)) => Ok(prepared),
        Ok(None) => Err("nothing to prepare".to_string()),
        Err(error) => Err(error.to_string()),
    }
}

fn accepted(layout: &Layout, root: &str) -> DriverDataBase {
    prepare(layout, root).unwrap_or_else(|cause| panic!("expected preparation to succeed: {cause}"))
}

fn refused(layout: &Layout, root: &str) -> String {
    match prepare(layout, root) {
        Ok(_) => panic!("expected preparation to be refused"),
        Err(cause) => cause,
    }
}

/// All diagnostics of an ingot of a prepared view, formatted.
fn errors(db: &DriverDataBase, root: &str) -> String {
    let ingot = db
        .workspace()
        .containing_ingot(db, url(&format!("{root}/")))
        .expect("indexed ingot");
    let diagnostics = db.run_on_ingot(ingot);
    if diagnostics.has_errors(db) {
        diagnostics.format_diags(db)
    } else {
        String::new()
    }
}

/// The value of a const function written in `dir/src/lib.fe`, or in a
/// generated item of its module.
fn value(db: &DriverDataBase, dir: &str, name: &str) -> String {
    value_at(db, &url(&format!("{dir}/src/lib.fe")), name)
}

fn value_at(db: &DriverDataBase, file: &Url, name: &str) -> String {
    let dir = file.as_str();
    let file = db
        .workspace()
        .get(db, file)
        .unwrap_or_else(|| panic!("missing file {dir}"));
    let function = db
        .top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|f| f.name(db).to_opt().is_some_and(|n| n.data(db) == name))
        .unwrap_or_else(|| panic!("missing function `{name}` in {dir}"));
    let owner = BodyOwner::Func(function, None);
    let EvalOutcome::Ready(value) = eval_body_owner_const(
        db,
        owner,
        GenericSubst::for_body_owner(db, owner, Vec::new()),
    ) else {
        panic!("`{name}` did not evaluate");
    };
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("`{name}` is not an integer");
    };
    value.to_string()
}

const PROVIDER_VIEW: &str = "what generation produces in its own package is not visible to it";
const HIDDEN: &str = "would be hidden";

// ---- A provider reads authored text only, in an ingot ------------------------

const SCOPED_READS_INHERENT: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
struct SeedP {}
impl DeriveInherent for SeedP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn seed() -> u256 { 42 } }
    }
}
struct Token {}
impl Token with (SeedP)
struct Pair {}
struct A {}
impl Derive<trait Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = Token::seed()
        quote { const fn score() -> u256 { ${n} } }
    }
}
const fn run() -> u256 { with (A) { Pair::score() } }
";

const INHERENT_READS_SCOPED: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
trait Seed { const fn seed() -> u256 }
struct Token {}
struct B {}
impl Derive<trait Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn seed() -> u256 { 42 } }
    }
}
struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = with (B) { Token::seed() }
        quote { pub const fn run() -> u256 { ${n} } }
    }
}
struct Pair {}
impl Pair with (Make)
";

const INHERENT_READS_INHERENT: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
struct SeedP {}
impl DeriveInherent for SeedP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn seed() -> u256 { 42 } }
    }
}
struct Token {}
impl Token with (SeedP)
struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = Token::seed()
        quote { pub const fn run() -> u256 { ${n} } }
    }
}
struct Pair {}
impl Pair with (Make)
";

/// The three ways one provider can read another's output give the same answer
/// in an ingot as in a standalone file: refused, with the rule.
#[test]
fn a_provider_cannot_read_another_providers_output_in_an_ingot() {
    for (name, source) in [
        ("scoped reads inherent header", SCOPED_READS_INHERENT),
        ("inherent header reads scoped", INHERENT_READS_SCOPED),
        (
            "inherent header reads inherent header",
            INHERENT_READS_INHERENT,
        ),
    ] {
        let layout = Layout::new().ingot("app", &[], source);
        let cause = refused(&layout, "app");
        assert!(cause.contains(PROVIDER_VIEW), "{name}: {cause}");
    }
}

/// A private helper of the dependency that wrote the provider stays available
/// to the code the provider generates in the dependent.
#[test]
fn a_dependency_providers_private_helper_serves_its_generated_code() {
    let lib = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item, field_count}
const fn boost(_ n: usize) -> u256 { n as u256 + 40 }
pub struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { pub const fn generated_count() -> u256 { boost(${n}) } }
    }
}
";
    let app = "pub struct Pair { a: u256, b: u256 }
impl Pair with (lib::Make)
pub const fn run() -> u256 { Pair::generated_count() }
";
    let layout = Layout::new()
        .ingot("lib", &[], lib)
        .ingot("app", &[("lib", "lib")], app);
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "run"), "42");
}

// ---- Generated items do not change what authored code resolves to -----------

const B6_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use lib::Token
// A trait of the dependent, implemented by hand for the dependency's type.
pub trait Seed { const fn seed(self) -> u256 }
impl Seed for Token { const fn seed(self) -> u256 { 1 } }

pub struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let t = Token {}
        let seen = t.seed()
        quote { pub const fn seen_by_provider() -> u256 { ${seen} } }
    }
}
pub struct Pair {}
impl Pair with (Make)

pub const fn read() -> u256 {
    let t = Token {}
    t.seed()
}
";

/// The dependency's inherent method is handwritten: the dependent's provider
/// and the final program both call it, and agree.
#[test]
fn a_handwritten_dependency_method_is_seen_alike_by_provider_and_program() {
    let lib = "pub struct Token {}\nimpl Token { pub const fn seed(self) -> u256 { 2 } }\n";
    let layout = Layout::new()
        .ingot("lib", &[], lib)
        .ingot("app", &[("lib", "lib")], B6_APP);
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "seen_by_provider"), "2");
    assert_eq!(value(&prepared, "app", "read"), "2");
}

/// Without any inherent method the dependent's trait method is the only one.
#[test]
fn a_dependency_without_the_method_is_seen_alike_by_provider_and_program() {
    let lib = "pub struct Token {}\n";
    let layout = Layout::new()
        .ingot("lib", &[], lib)
        .ingot("app", &[("lib", "lib")], B6_APP);
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "seen_by_provider"), "1");
    assert_eq!(value(&prepared, "app", "read"), "1");
}

/// The dependency's inherent method is generated. The dependent's provider
/// reads the dependency's output as ordinary code, so the provider and the
/// final program see the same method (2). Before, the provider saw its own
/// trait method (1) and the final program called the generated inherent
/// method (2).
#[test]
fn a_generated_dependency_method_is_seen_alike_by_provider_and_program() {
    let lib = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub struct AddSeed {}
impl DeriveInherent for AddSeed {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn seed(self) -> u256 { 2 } }
    }
}
pub struct Token {}
impl Token with (AddSeed)
";
    let layout = Layout::new()
        .ingot("lib", &[], lib)
        .ingot("app", &[("lib", "lib")], B6_APP);
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "seen_by_provider"), "2");
    assert_eq!(value(&prepared, "app", "read"), "2");
    assert_eq!(errors(&prepared, "app"), "");
}

// ---- A dependency is prepared and sealed before its dependents ----------------

const SEED_LIB: &str = "use core::derive::Derive
use core::meta::{Record, Code, Item}
pub trait Seed { const fn seed() -> u256 }
pub struct SeedP {}
impl Derive<trait Seed> for SeedP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn seed() -> u256 { 42 } }
    }
}
pub struct Token {}
impl Seed for Token with (SeedP)
";

/// The provider of an inherent header of a dependent reads an implementation
/// that the dependency generated. It is part of the dependency's sealed output,
/// so it is ordinary code for the dependent.
#[test]
fn an_inherent_header_provider_reads_a_dependencys_generated_implementation() {
    let app = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use lib::{Seed, Token}
pub struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = Token::seed()
        quote { pub const fn run() -> u256 { ${n} } }
    }
}
pub struct Pair {}
impl Pair with (Make)
";
    let layout = Layout::new()
        .ingot("lib", &[], SEED_LIB)
        .ingot("app", &[("lib", "lib")], app);
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "run"), "42");
    assert_eq!(errors(&prepared, "app"), "");
}

/// The same for a scoped provider.
#[test]
fn a_scoped_provider_reads_a_dependencys_generated_implementation() {
    let app = "use core::derive::Derive
use core::meta::{Record, Code, Item}
use lib::{Seed, Token}
pub trait Score { const fn score() -> u256 }
pub struct Pair {}
pub struct A {}
impl Derive<trait Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = Token::seed()
        quote { const fn score() -> u256 { ${n} } }
    }
}
pub const fn run() -> u256 { with (A) { Pair::score() } }
";
    let layout = Layout::new()
        .ingot("lib", &[], SEED_LIB)
        .ingot("app", &[("lib", "lib")], app);
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "run"), "42");
    assert_eq!(errors(&prepared, "app"), "");
}

/// Three ingots in a line: each generated layer is read by the next one's
/// provider, so the order of preparation is the order of the dependencies.
#[test]
fn a_chain_of_three_ingots_is_prepared_dependencies_first() {
    let mid = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use base::{Seed, Token}
pub struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = Token::seed() + 1
        quote { pub const fn mid_value() -> u256 { ${n} } }
    }
}
pub struct Marker {}
impl Marker with (Make)
";
    let app = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use mid::Marker
pub struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = Marker::mid_value() + 1
        quote { pub const fn top_value() -> u256 { ${n} } }
    }
}
pub struct Top {}
impl Top with (Make)
";
    let layout = Layout::new()
        .ingot("base", &[], SEED_LIB)
        .ingot("mid", &[("base", "base")], mid)
        .ingot("app", &[("mid", "mid")], app);
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "top_value"), "44");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(errors(&prepared, "mid"), "");
}

// ---- The configuration a dependent forces on a dependency --------------------

/// The dependency's provider overflows a `u8` when arithmetic is checked and
/// wraps to 0 when it is not.
const OVERFLOW_LIB: &str = "use core::derive::Derive
use core::meta::{Record, Code, Item}
pub trait Seed { const fn seed() -> u256 }
pub struct SeedP {}
impl Derive<trait Seed> for SeedP {
    const fn derive(_ record: Record) -> Code<Item> {
        let x: u8 = 255
        let y: u8 = x + 1
        let z: u256 = y as u256
        quote { const fn seed() -> u256 { ${z} } }
    }
}
pub struct Token {}
impl Seed for Token with (SeedP)
";
const READ_SEED: &str = "use lib::{Seed, Token}
pub const fn read() -> u256 { Token::seed() }
";

fn forcing_manifest(name: &str, setting: Option<&str>) -> String {
    let setting = setting
        .map(|mode| format!("dependency-arithmetic = \"{mode}\"\n"))
        .unwrap_or_default();
    format!(
        "[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n{setting}\n[dependencies]\nlib = {{ path = \"../lib\" }}\n"
    )
}

/// Ingots on disk, loaded the way the command line loads them: the setting a
/// dependent forces on its dependencies is applied only when the manifests are
/// read from their directories.
struct OnDisk {
    dir: tempfile::TempDir,
}

impl OnDisk {
    fn new(files: &[(&str, String)]) -> Self {
        let dir = tempfile::tempdir().unwrap();
        for (path, text) in files {
            let full = dir.path().join(path);
            std::fs::create_dir_all(full.parent().unwrap()).unwrap();
            std::fs::write(full, text).unwrap();
        }
        Self { dir }
    }

    fn url(&self, path: &str) -> Url {
        let full = self.dir.path().join(path);
        if path.ends_with('/') || full.is_dir() {
            Url::from_directory_path(full).unwrap()
        } else {
            Url::from_file_path(full).unwrap()
        }
    }

    /// Load the ingots at `roots`; the flag is `true` when loading reported
    /// an error.
    fn load(&self, roots: &[&str]) -> (DriverDataBase, bool) {
        let mut db = DriverDataBase::default();
        let mut reported = false;
        for root in roots {
            reported |= fe_driver::init_ingot(&mut db, &self.url(root));
        }
        (db, reported)
    }
}

fn overflow_files(setting: Option<&str>) -> Vec<(&'static str, String)> {
    vec![
        ("lib/fe.toml", manifest("lib", &[])),
        ("lib/src/lib.fe", OVERFLOW_LIB.to_string()),
        ("app/fe.toml", forcing_manifest("app", setting)),
        ("app/src/lib.fe", READ_SEED.to_string()),
    ]
}

/// A dependent that leaves the setting alone runs the dependency's provider
/// with checked arithmetic, and the overflow refuses the build.
#[test]
fn a_dependency_provider_overflows_under_checked_arithmetic() {
    let disk = OnDisk::new(&overflow_files(None));
    let (db, reported) = disk.load(&["app/"]);
    assert!(!reported);
    let error = prepare_ingots(&db, &[disk.url("app/")])
        .err()
        .expect("the overflow refuses the build");
    assert!(error.cause.contains("arithmetic overflow"), "{error}");
    assert!(error.file.as_str().ends_with("lib/src/lib.fe"), "{error}");
}

/// A dependent that forces unchecked arithmetic on its dependencies runs the
/// dependency's provider under that setting, and the generated value is the
/// wrapped one. The dependency is prepared in a view that carries the forced
/// setting.
#[test]
fn a_dependent_can_force_the_arithmetic_of_a_dependency_provider() {
    let disk = OnDisk::new(&overflow_files(Some("unchecked")));
    let (db, reported) = disk.load(&["app/"]);
    assert!(!reported);
    let prepared = prepare_ingots(&db, &[disk.url("app/")])
        .unwrap_or_else(|error| panic!("{error}"))
        .expect("the dependency requests generation");
    assert_eq!(
        value_at(&prepared, &disk.url("app/src/lib.fe"), "read"),
        "0"
    );
}

fn two_dependents(left: Option<&str>, right: Option<&str>) -> Vec<(&'static str, String)> {
    let mut files = overflow_files(None);
    files.truncate(2);
    files.extend([
        ("left/fe.toml", forcing_manifest("left", left)),
        ("left/src/lib.fe", READ_SEED.to_string()),
        ("right/fe.toml", forcing_manifest("right", right)),
        ("right/src/lib.fe", READ_SEED.to_string()),
        (
            "root/fe.toml",
            "[ingot]\nname = \"root\"\nversion = \"0.1.0\"\n\n[dependencies]\nleft = { path = \"../left\" }\nright = { path = \"../right\" }\n".to_string(),
        ),
        (
            "root/src/lib.fe",
            "pub const fn r() -> u256 { left::read() + right::read() }\n".to_string(),
        ),
    ]);
    files
}

/// Two dependents of one dependency, one forcing unchecked arithmetic and one
/// leaving the setting alone, give the dependency one configuration for the
/// whole build.
#[test]
fn one_dependency_has_one_configuration_for_all_its_dependents() {
    let disk = OnDisk::new(&two_dependents(None, Some("unchecked")));
    let (db, reported) = disk.load(&["root/"]);
    assert!(!reported);
    let prepared = prepare_ingots(&db, &[disk.url("root/")])
        .unwrap_or_else(|error| panic!("{error}"))
        .expect("the dependency requests generation");
    assert_eq!(value_at(&prepared, &disk.url("root/src/lib.fe"), "r"), "0");
}

/// Two dependents that force different settings on one dependency are a
/// conflict; loading reports it, as before.
#[test]
fn conflicting_settings_for_one_dependency_are_still_refused() {
    let disk = OnDisk::new(&two_dependents(Some("checked"), Some("unchecked")));
    let (_, reported) = disk.load(&["root/"]);
    assert!(reported);
}

// ---- Declared headers are facts, across ingots ------------------------------

const SIZE_LIB: &str = "use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}
pub trait Size { const fn size() -> usize }
pub struct CountP {}
impl Derive<trait Size> for CountP {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { const fn size() -> usize { ${n} } }
    }
}
";

/// A header written in the ingot declares that the implementation exists, so
/// a target whose bound needs it is admitted: the header for `Holder` admits
/// its field type `Wrap<Leaf>` through the header for `Leaf`.
#[test]
fn a_written_header_is_a_fact_for_target_admission_in_an_ingot() {
    let app = "use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}
trait Size { const fn size() -> usize }
struct CountP {}
impl Derive<trait Size> for CountP {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { const fn size() -> usize { ${n} } }
    }
}
struct Leaf { v: u256 }
impl Size for Leaf with (CountP)
struct Wrap<A: Size> { inner: A }
struct Holder { w: Wrap<Leaf> }
impl Size for Holder with (CountP)
const fn run() -> usize { Leaf::size() + Holder::size() }
";
    let layout = Layout::new().ingot("app", &[], app);
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "run"), "2");
    assert_eq!(errors(&prepared, "app"), "");
}

/// A header written in a dependency serves a dependent's target admission.
#[test]
fn a_dependencys_written_header_is_a_fact_for_its_dependent() {
    let lib =
        format!("{SIZE_LIB}pub struct Leaf {{ pub v: u256 }}\nimpl Size for Leaf with (CountP)\n");
    let app = "use lib::{Size, Leaf, CountP}
struct Wrap<A: Size> { inner: A }
struct Holder { w: Wrap<Leaf> }
impl Size for Holder with (CountP)
const fn run() -> usize { Leaf::size() + Holder::size() }
";
    let layout = Layout::new()
        .ingot("lib", &[], &lib)
        .ingot("app", &[("lib", "lib")], app);
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "run"), "2");
    assert_eq!(errors(&prepared, "app"), "");
}

const LIBRARY_TRAIT_LIB: &str = "pub trait Plain { const fn value() -> u256 }
";

const LIBRARY_TRAIT_APP: &str = "use core::derive::Derive
use core::meta::{Record, Code, Item}
use lib::TRAIT
pub struct Key {}
struct Seven {}
impl Derive<trait TRAIT> for Seven {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn value() -> u256 { 7 } }
    }
}
impl TRAIT for Key with (Seven)
const fn run() -> u256 { Key::value() }
";

/// A header is a written implementation, so it may implement a trait of
/// another package as a handwritten one does: the trait need not belong to the
/// package that writes the header.
#[test]
fn a_header_may_implement_a_trait_of_a_dependency() {
    let layout = Layout::new().ingot("lib", &[], LIBRARY_TRAIT_LIB).ingot(
        "app",
        &[("lib", "lib")],
        &LIBRARY_TRAIT_APP.replace("TRAIT", "Plain"),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "run"), "7");
    assert_eq!(errors(&prepared, "app"), "");
}

// ---- Coherence reads the whole dependency closure -----------------------------

const BASE: &str = "pub trait T { const fn f() -> u256 }
impl<U> T for U { const fn f() -> u256 { 1 } }
pub const fn via_base<U: T>() -> u256 { U::f() }
";

const MID: &str = "pub use base::T\npub use base::via_base\n";

/// An implementation for a local type overlaps the blanket implementation of
/// the trait's home ingot. With the home as a direct dependency the overlap is
/// a conflict.
#[test]
fn an_overlap_with_a_direct_dependencys_blanket_implementation_is_a_conflict() {
    let app = "use base::T
pub struct X {}
impl T for X { const fn f() -> u256 { 2 } }
";
    let layout = Layout::new()
        .ingot("base", &[], BASE)
        .ingot("app", &[("base", "base")], app);
    let db = layout.db();
    assert!(
        errors(&db, "app").contains("5-0001"),
        "{}",
        errors(&db, "app")
    );
}

/// The same overlap through a re-export: the trait comes from `mid`, and its
/// home, `base`, is a dependency of `mid` only. The overlap is the same
/// conflict, not an implementation that passes checking and then has no
/// selected implementation in code generation.
#[test]
fn an_overlap_with_a_transitive_dependencys_blanket_implementation_is_a_conflict() {
    let app = "use mid::T
pub struct X {}
impl T for X { const fn f() -> u256 { 2 } }
";
    let layout = Layout::new()
        .ingot("base", &[], BASE)
        .ingot("mid", &[("base", "base")], MID)
        .ingot("app", &[("mid", "mid")], app);
    let db = layout.db();
    assert!(
        errors(&db, "app").contains("5-0001"),
        "{}",
        errors(&db, "app")
    );
}

/// A named implementation is outside implicit search and never conflicts.
#[test]
fn a_named_implementation_beside_a_transitive_blanket_implementation_is_accepted() {
    let app = "use mid::T
pub struct X {}
impl T for X as Mine { const fn f() -> u256 { 2 } }
";
    let layout = Layout::new()
        .ingot("base", &[], BASE)
        .ingot("mid", &[("base", "base")], MID)
        .ingot("app", &[("mid", "mid")], app);
    let db = layout.db();
    assert_eq!(errors(&db, "app"), "");
}

/// One ingot reached through two paths is read once, and no implementation
/// conflicts with itself or with an unrelated one.
#[test]
fn a_diamond_of_dependencies_has_no_false_conflict() {
    let app = "use left::T
pub struct X {}
pub trait Other { const fn g() -> u256 }
impl Other for X { const fn g() -> u256 { 3 } }
pub const fn run() -> u256 { left::via_base<u256>() + right::via_base<X>() }
";
    let layout = Layout::new()
        .ingot("base", &[], BASE)
        .ingot("left", &[("base", "base")], MID)
        .ingot("right", &[("base", "base")], MID)
        .ingot("app", &[("left", "left"), ("right", "right")], app);
    let db = layout.db();
    assert_eq!(errors(&db, "app"), "");
}

// ---- A generated member does not replace an authored member of the same use --

const TRAIT_CONST: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Tr { const C: u256 }
pub struct X {}
impl Tr for X { const C: u256 = 1 }
pub struct Y {}
pub struct Inh {}
impl DeriveInherent for Inh {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const C: u256 = 2 }
    }
}
impl TARGET with (Inh)
pub const fn read_c() -> u256 { X::C }
pub struct Observe {}
impl DeriveInherent for Observe {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = read_c()
        quote { pub const fn seen() -> u256 { ${n} } }
    }
}
pub struct Other {}
impl Other with (Observe)
";

/// A generated inherent constant, here a member of an inherent header, outranks
/// a trait constant of the same name. The method form was refused; the constant
/// form is refused alike.
#[test]
fn a_generated_inherent_constant_cannot_replace_a_trait_constant() {
    let layout = Layout::new().ingot("app", &[], &TRAIT_CONST.replace("TARGET", "X"));
    let cause = refused(&layout, "app");
    assert!(cause.contains(HIDDEN), "{cause}");
    assert!(cause.contains("the generated `C`"), "{cause}");
    // An inherent constant of another type changes nothing.
    let layout = Layout::new().ingot("app", &[], &TRAIT_CONST.replace("TARGET", "Y"));
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "seen"), "1");
    assert_eq!(value(&prepared, "app", "read_c"), "1");
}

// ---- A header says an implementation exists, not what its members are --------

const ASSOC_TYPE: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item, field, field_type, type_is}
pub trait Tr {
    type Out = u64
    const fn f() -> u256
}
pub struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            type Out = u256
            const fn f() -> u256 { 1 }
        }
    }
}
pub struct S {}
IMPL
pub struct Observe {}
impl DeriveInherent for Observe {
    const fn derive(_ record: Record) -> Code<Item> {
        let wide: bool = type_is<u256>(field_type(field(record, 0)))
        let n: u256 = if wide { 256 } else { 64 }
        quote { pub const fn seen_width() -> u256 { ${n} } }
    }
}
pub struct Holder { pub x: <S as Tr>::Out }
impl Holder with (Observe)
";

/// The trait gives `Out` a default, `u64`, and the generated implementation
/// gives it another value, `u256`. While a provider runs, the header declares
/// only that the implementation exists. The provider must not read the trait's
/// default as the type of `S::Out`, since the final program gives the
/// field another type.
#[test]
fn a_header_only_implementation_has_no_associated_type_for_a_provider() {
    let layout = Layout::new().ingot(
        "app",
        &[],
        &ASSOC_TYPE.replace("IMPL", "impl Tr for S with (P)"),
    );
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("only declared by a written header"),
        "{cause}"
    );
    assert!(cause.contains("S::Out"), "{cause}");
}

/// A written implementation states its associated type, so the provider reads
/// it and the program agrees.
#[test]
fn a_written_implementation_has_its_associated_type_for_a_provider() {
    let written = "impl Tr for S {\n    type Out = u256\n    const fn f() -> u256 { 1 }\n}";
    let layout = Layout::new().ingot("app", &[], &ASSOC_TYPE.replace("IMPL", written));
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "seen_width"), "256");
    assert_eq!(errors(&prepared, "app"), "");
}

// ---- Whether an ingot is prepared does not depend on an unrelated item --------

const GENERIC_ID_LIB: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub struct EmitId {}
impl DeriveInherent for EmitId {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn id<T>(_ t: own T) -> T { t } }
    }
}
pub struct Marker {}
impl Marker with (EmitId)
";

const WITH_ID_APP: &str = "use core::derive::Derive
use core::meta::{Record, Code, Item}
pub struct X {}
pub trait Tr { const fn f(self) -> u256 }
pub struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn f(self) -> u256 { 3 } }
    }
}
pub const fn run() -> u256 { with (P) { lib::Marker::id(X {}).f() } }
CONTROL
";

/// A `with` block in the dependent opens a provider on a value that passes
/// through a function the dependency generated. The block's demand exists only
/// where the dependency's output is visible, so the dependent is prepared in a
/// view that holds it.
#[test]
fn a_with_block_using_a_dependencys_generated_function_is_prepared() {
    let app = WITH_ID_APP.replace("CONTROL", "");
    let layout =
        Layout::new()
            .ingot("lib", &[], GENERIC_ID_LIB)
            .ingot("app", &[("lib", "lib")], &app);
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "run"), "3");
}

/// The same program with an unrelated `with` block beside it gives the same
/// answer. Before, the unrelated block alone made the ingot be prepared.
#[test]
fn an_unrelated_with_block_does_not_change_whether_the_dependent_is_prepared() {
    let control = "pub const fn control() -> u256 { with (P) { X {}.f() } }";
    let app = WITH_ID_APP.replace("CONTROL", control);
    let layout =
        Layout::new()
            .ingot("lib", &[], GENERIC_ID_LIB)
            .ingot("app", &[("lib", "lib")], &app);
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "run"), "3");
    assert_eq!(value(&prepared, "app", "control"), "3");
}

/// Checking a body that opens a provider without preparing it says what is
/// missing: a generated implementation. The label no longer claims a
/// compile-time-only value is used at runtime.
#[test]
fn an_unprepared_scoped_provider_is_not_reported_as_a_runtime_context_error() {
    use hir::analysis::diagnostics::DiagnosticVoucher;
    let source = "use core::derive::Derive
use core::meta::{Record, Code, Item}
pub trait Tr { const fn f() -> u256 }
pub struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn f() -> u256 { 3 } }
    }
}
pub struct X {}
pub fn run() -> u256 { with (P) { X::f() } }
";
    let db = Layout::new().ingot("app", &[], source).db();
    let file = db.workspace().get(&db, &url("app/src/lib.fe")).unwrap();
    let run = db
        .top_mod(file)
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|f| f.name(&db).to_opt().is_some_and(|n| n.data(&db) == "run"))
        .expect("run");
    let (diagnostics, _) = hir::analysis::ty::ty_check::check_func_body(&db, run);
    let pending = diagnostics
        .iter()
        .map(|diagnostic| diagnostic.to_complete(&db))
        .find(|diagnostic| {
            diagnostic
                .message
                .contains("prepared generated implementation")
        })
        .expect("the unprepared provider is reported");
    let label = pending.sub_diagnostics[0].message.clone();
    assert!(!label.contains("runtime context"), "{label}");
    assert!(label.contains("generated"), "{label}");
}

// ---- Each ingot is sealed once, however many dependents read it ---------------

/// A diamond: the base generates, two middle ingots read the base's output in
/// their own providers, and the root reads both. Every ingot is prepared once
/// and each dependent sees the output of the ones below it.
#[test]
fn a_diamond_of_generating_ingots_is_sealed_once_per_ingot() {
    let base = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub struct Emit {}
impl DeriveInherent for Emit {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn base_value() -> u256 { 1 } }
    }
}
pub struct Marker {}
impl Marker with (Emit)
";
    let middle = |name: &str, add: u32| {
        format!(
            "use core::derive::{{Derive, DeriveInherent}}
use core::meta::{{Record, Code, Item}}
pub struct Make {{}}
impl DeriveInherent for Make {{
    const fn derive(_ record: Record) -> Code<Item> {{
        let n = base::Marker::base_value() + {add}
        quote {{ pub const fn {name}_value() -> u256 {{ ${{n}} }} }}
    }}
}}
pub struct Marker {{}}
impl Marker with (Make)
"
        )
    };
    let root = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = left::Marker::left_value() * 10 + right::Marker::right_value()
        quote { pub const fn root_value() -> u256 { ${n} } }
    }
}
pub struct Marker {}
impl Marker with (Make)
";
    let layout = Layout::new()
        .ingot("base", &[], base)
        .ingot("left", &[("base", "base")], &middle("left", 1))
        .ingot("right", &[("base", "base")], &middle("right", 2))
        .ingot("root", &[("left", "left"), ("right", "right")], root);
    let prepared = accepted(&layout, "root");
    assert_eq!(value(&prepared, "root", "root_value"), "23");
    assert_eq!(errors(&prepared, "root"), "");
}

// ---- Provider and header diagnostics say what is wrong -------------------------

const TYPO_PROVIDER: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n: u256 = true
        quote { pub const fn v() -> u256 { ${n} } }
    }
}
struct S {}
impl S with (Make)
";

/// A provider body error that has nothing to do with generation is reported
/// without the note about the provider's view.
#[test]
fn a_provider_error_unrelated_to_generation_carries_no_view_note() {
    let layout = Layout::new().ingot("app", &[], TYPO_PROVIDER);
    let cause = refused(&layout, "app");
    assert!(cause.contains("type mismatch"), "{cause}");
    assert!(!cause.contains("Share an ordinary helper"), "{cause}");
    assert!(
        !cause.contains("generation produces in its own package"),
        "{cause}"
    );
}

/// A provider that reads a method only generation supplies, here a member of
/// an inherent header, is told the rule.
#[test]
fn a_provider_error_about_a_generated_method_carries_the_view_note() {
    let source = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
struct SeedP {}
impl DeriveInherent for SeedP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn seed() -> u256 { 42 } }
    }
}
struct Token {}
impl Token with (SeedP)
struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = Token::seed()
        quote { pub const fn v() -> u256 { ${n} } }
    }
}
struct Pair {}
impl Pair with (Make)
";
    let layout = Layout::new().ingot("app", &[], source);
    let cause = refused(&layout, "app");
    assert!(cause.contains("Share an ordinary helper"), "{cause}");
}

const READ_THROUGH_HEADER: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Tr {
    const K: u256 = 1
    const fn f() -> u256 { 10 }
}
pub struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const K: u256 = 2
            const fn f() -> u256 { 20 }
        }
    }
}
pub struct S {}
impl Tr for S with (P)
const fn read_k<T: Tr>() -> u256 { READ }
pub struct Observe {}
impl DeriveInherent for Observe {
    const fn derive(_ record: Record) -> Code<Item> {
        let n: u256 = read_k<S>()
        quote { pub const fn seen() -> u256 { ${n} } }
    }
}
pub struct Holder {}
impl Holder with (Observe)
";

/// A call and a constant read through a header-only implementation are refused
/// with the same account: the implementation is only declared by a header.
/// Neither is reported as an internal error or as a missing unique value.
#[test]
fn a_call_and_a_constant_through_a_header_are_refused_alike() {
    for read in ["T::f()", "T::K"] {
        let layout = Layout::new().ingot("app", &[], &READ_THROUGH_HEADER.replace("READ", read));
        let cause = refused(&layout, "app");
        assert!(
            cause.contains("only declared by a written header"),
            "{read}: {cause}"
        );
        assert!(
            !cause.contains("internal access checking error"),
            "{read}: {cause}"
        );
        assert!(!cause.contains("no unique value"), "{read}: {cause}");
    }
}

// ---- A generated member that its type already has is refused -----------------
//
// An inherent member outranks a trait member of the same name, without any
// name being involved. With generated items as a lower tier, a path keeps
// naming the trait member, so a generated inherent member of that name is never
// selected. It is refused at its request, with an accepted control that
// differs only in the name of the generated member. The generated member is a
// member of an inherent header, `impl X with (Inh)`.

/// `src` with the generated constant named `generated`.
fn named(src: &str, generated: &str) -> String {
    src.replace("GEN", generated)
}

fn assert_member_refusal(cause: &str, name: &str) {
    assert!(cause.contains(HIDDEN), "{cause}");
    assert!(
        cause.contains(&format!("the generated `{name}`")),
        "{cause}"
    );
    assert!(
        cause.contains(&format!(
            "`X` already has the constant `{name}` of the trait `Tr`"
        )),
        "{cause}"
    );
    assert!(cause.contains("app/src/lib.fe"), "{cause}");
    assert!(cause.contains("rename"), "{cause}");
}

const TRAIT_CONST_IN_TYPE: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item, FieldQuery, field, field_type, query_field}
pub trait Tr { const N: usize }
pub struct X {}
impl Tr for X { const N: usize = 2 }
pub struct Inh {}
impl DeriveInherent for Inh {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const GEN: usize = 5 }
    }
}
impl X with (Inh)
struct LenQ<A> {}
impl<const K: usize> FieldQuery<usize> for LenQ<[u8; K]> {
    const fn evaluate() -> usize { K }
}
pub struct Observe {}
impl DeriveInherent for Observe {
    const fn derive(_ record: Record) -> Code<Item> {
        let n: usize = query_field<LenQ, usize>(field_type(field(record, 0)))
        quote { pub const fn seen_len() -> usize { ${n} } }
    }
}
pub struct S { pub d: [u8; X::N] }
impl S with (Observe)
";

/// The field type `[u8; X::N]` names the trait constant. A generated inherent
/// constant `N` of `X` would give the final record another length than the
/// one the provider read.
#[test]
fn a_generated_inherent_constant_cannot_hide_a_trait_constant_in_a_field_type() {
    let layout = Layout::new().ingot("app", &[], &named(TRAIT_CONST_IN_TYPE, "N"));
    assert_member_refusal(&refused(&layout, "app"), "N");
    let layout = Layout::new().ingot("app", &[], &named(TRAIT_CONST_IN_TYPE, "L"));
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "seen_len"), "2");
    assert_eq!(errors(&prepared, "app"), "");
}

const TRAIT_CONST_IN_PATTERN: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Tr { const N: u256 }
pub struct X {}
impl Tr for X { const N: u256 = 7 }
pub struct Inh {}
impl DeriveInherent for Inh {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const GEN: u256 = 5 }
    }
}
impl X with (Inh)
pub const fn classify(_ v: u256) -> u256 {
    match v {
        X::N => 1
        _ => 2
    }
}
pub struct Observe {}
impl DeriveInherent for Observe {
    const fn derive(_ record: Record) -> Code<Item> {
        let c = classify(7)
        quote { pub const fn seen() -> u256 { ${c} } }
    }
}
struct O {}
impl O with (Observe)
pub const fn at_seven() -> u256 { classify(7) }
";

/// A constant pattern by path, `X::N`, names the trait constant.
#[test]
fn a_generated_inherent_constant_cannot_hide_a_trait_constant_in_a_pattern() {
    let layout = Layout::new().ingot("app", &[], &named(TRAIT_CONST_IN_PATTERN, "N"));
    assert_member_refusal(&refused(&layout, "app"), "N");
    let layout = Layout::new().ingot("app", &[], &named(TRAIT_CONST_IN_PATTERN, "L"));
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "seen"), "1");
    assert_eq!(value(&prepared, "app", "at_seven"), "1");
}

const TRAIT_CONST_AS_ARGUMENT: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Tr { const N: usize }
pub struct X {}
impl Tr for X { const N: usize = 2 }
pub struct Inh {}
impl DeriveInherent for Inh {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const GEN: usize = 5 }
    }
}
impl X with (Inh)
pub const fn width<const K: usize>() -> usize { K }
pub const fn via_argument() -> usize { width<X::N>() }
pub struct Observe {}
impl DeriveInherent for Observe {
    const fn derive(_ record: Record) -> Code<Item> {
        let c = via_argument()
        quote { pub const fn seen() -> usize { ${c} } }
    }
}
struct O {}
impl O with (Observe)
";

/// A generic argument in a body, `width<X::N>()`, names the trait constant.
#[test]
fn a_generated_inherent_constant_cannot_hide_a_trait_constant_in_a_generic_argument() {
    let layout = Layout::new().ingot("app", &[], &named(TRAIT_CONST_AS_ARGUMENT, "N"));
    assert_member_refusal(&refused(&layout, "app"), "N");
    let layout = Layout::new().ingot("app", &[], &named(TRAIT_CONST_AS_ARGUMENT, "L"));
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "seen"), "2");
    assert_eq!(value(&prepared, "app", "via_argument"), "2");
}

const TRAIT_CONST_IN_HEADER: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}
pub trait Tr { const N: u256 }
pub struct X {}
impl Tr for X { const N: u256 = 10 }
pub struct Inh {}
impl DeriveInherent for Inh {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const GEN: u256 = 77 }
    }
}
impl X with (Inh)
pub trait Report { const fn payload_bits() -> u256 }
pub struct Scaled<const K: u256> {}
impl<const K: u256> Derive<trait Report> for Scaled<K> {
    const fn derive(_ record: Record) -> Code<Item> {
        let k = K
        quote { const fn payload_bits() -> u256 { ${k} } }
    }
}
pub struct Plain<const K: u256> {}
impl Report for Plain<X::N> with (Scaled<X::N>)
pub const fn bits() -> u256 { Plain<10>::payload_bits() }
";

/// An implementation header names the trait constant in its type and in its
/// provider argument; the provider's members and the header's type must agree.
#[test]
fn a_generated_inherent_constant_cannot_hide_a_trait_constant_in_a_header() {
    let layout = Layout::new().ingot("app", &[], &named(TRAIT_CONST_IN_HEADER, "N"));
    assert_member_refusal(&refused(&layout, "app"), "N");
    let layout = Layout::new().ingot("app", &[], &named(TRAIT_CONST_IN_HEADER, "L"));
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "bits"), "10");
}

// ---- Selection among traits keeps its authored answer ------------------------
//
// A call `x.m()` selects the only candidate even when its trait is not in
// scope, and among several only those in scope. A generated implementation of
// another trait with the same method adds a candidate, which would change the
// answer. The selection runs without the generated implementations first, so
// the authored answer stays.
//
// The generated implementations of this and the following sections are
// written in the body of `carrier`, the member that the provider `Gen` gives
// to the inherent header `impl G with (Gen)`: generated code that no header
// declares.

/// The provider `Gen` of the inherent header `impl G with (Gen)`, whose only
/// member, `carrier`, writes `generated` in its body. The program must import
/// `Derive`, `Inherent`, `Record`, `Code` and `Item`.
fn generated_in_member(generated: &str) -> String {
    format!(
        "pub struct Gen {{}}
impl DeriveInherent for Gen {{
    const fn derive(_ record: Record) -> Code<Item> {{
        quote {{
            pub fn carrier() -> u256 {{
                {generated}
                0
            }}
        }}
    }}
}}
pub struct G {{}}
impl G with (Gen)
"
    )
}

const TRAIT_SCOPE_HEAD: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
mod a {
    pub trait Tr1 { const fn m(self) -> u256 }
    pub trait Tr1c { const C: u256 }
}
pub trait Tr2 { const fn m(self) -> u256 }
pub trait Tr2c { const C: u256 }
pub trait Marker {}
pub struct X {}
";

const TRAIT_SCOPE_TAIL: &str = "pub struct Observe {}
impl DeriveInherent for Observe {
    const fn derive(_ record: Record) -> Code<Item> {
        let c = read()
        quote { pub const fn seen() -> u256 { ${c} } }
    }
}
pub struct O {}
impl O with (Observe)
";

fn trait_scope_app(middle: &str) -> String {
    format!("{TRAIT_SCOPE_HEAD}{middle}\n{TRAIT_SCOPE_TAIL}")
}

/// `X` implements `a::Tr1` (not imported, the only candidate) and a generator
/// adds `impl Tr2 for X` with the same method name. `Tr2` is imported at the
/// root, so with both candidates the scope filter would pick `Tr2::m`.
#[test]
fn a_generated_trait_implementation_does_not_change_a_method_call() {
    let middle = "impl a::Tr1 for X { const fn m(self) -> u256 { 1 } }
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Tr2 for X { const fn m(self) -> u256 { 2 } }
                0
            }
        }
    }
}
pub struct G {}
impl G with (Gen)
pub const fn read() -> u256 {
    let x = X {}
    x.m()
}";
    let layout = Layout::new().ingot("app", &[], &trait_scope_app(middle));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "seen"), "1");
    assert_eq!(value(&prepared, "app", "read"), "1");
}

/// The generated implementation only makes an authored blanket implementation
/// of `Tr2` apply, through `Marker`. The blanket implementation was not a
/// candidate in the program as written.
#[test]
fn a_generated_marker_does_not_make_a_blanket_implementation_a_candidate() {
    let middle = "impl<T: Marker> Tr2 for T { const fn m(self) -> u256 { 2 } }
impl a::Tr1 for X { const fn m(self) -> u256 { 1 } }
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Marker for X {}
                0
            }
        }
    }
}
pub struct G {}
impl G with (Gen)
pub const fn read() -> u256 {
    let x = X {}
    x.m()
}";
    let layout = Layout::new().ingot("app", &[], &trait_scope_app(middle));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "seen"), "1");
    assert_eq!(value(&prepared, "app", "read"), "1");
}

/// The same for an associated constant: `X::C` keeps the constant of
/// `a::Tr1c`, the only candidate, when a generated `impl Tr2c for X` adds one.
#[test]
fn a_generated_trait_implementation_does_not_change_an_associated_constant() {
    let middle = "impl a::Tr1c for X { const C: u256 = 1 }
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Tr2c for X { const C: u256 = 2 }
                0
            }
        }
    }
}
pub struct G {}
impl G with (Gen)
pub const fn read() -> u256 { X::C }";
    let layout = Layout::new().ingot("app", &[], &trait_scope_app(middle));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "seen"), "1");
    assert_eq!(value(&prepared, "app", "read"), "1");
}

/// Control: where no authored implementation answers, the generated one fills
/// the gap, for a method call and for an associated constant.
#[test]
fn a_generated_trait_implementation_fills_a_method_and_a_constant_gap() {
    let middle = "pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Tr2 for X { const fn m(self) -> u256 { 2 } }
                impl Tr2c for X { const C: u256 = 3 }
                0
            }
        }
    }
}
pub struct G {}
impl G with (Gen)
pub const fn read() -> u256 { 0 }
pub const fn call() -> u256 {
    let x = X {}
    x.m()
}
pub const fn constant() -> u256 { X::C }";
    let layout = Layout::new().ingot("app", &[], &trait_scope_app(middle));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "call"), "2");
    assert_eq!(value(&prepared, "app", "constant"), "3");
}

// ---- Implementations that carry behaviour, and shared member names ------------

/// All diagnostics of an ingot of a prepared view, errors and warnings.
fn all_diagnostics(db: &DriverDataBase, root: &str) -> String {
    let ingot = db
        .workspace()
        .containing_ingot(db, url(&format!("{root}/")))
        .expect("indexed ingot");
    db.run_on_ingot(ingot).format_diags(db)
}

const BEHAVIOUR_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use core::ops::Drop
use core::marker::{Pinned, Storage}
pub trait Other {}
pub struct X { v: u256 }
pub struct Emit {}
impl DeriveInherent for Emit {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl IMPL for X { BODY }
                0
            }
        }
    }
}
pub struct T {}
impl T with (Emit)
";

fn behaviour_app(implemented: &str, body: &str) -> String {
    BEHAVIOUR_APP
        .replace("IMPL", implemented)
        .replace("BODY", body)
}

/// Whether a type has a `Drop` implementation decides what authored code runs
/// where a value goes out of use. A generated one would change authored code
/// without any name or member being in the way, so it is refused.
#[test]
fn a_generated_drop_implementation_is_refused() {
    let layout = Layout::new().ingot("app", &[], &behaviour_app("Drop", "fn drop(own self) {}"));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Drop` for `X`"),
        "{cause}"
    );
    assert!(cause.contains("is refused"), "{cause}");
    assert!(cause.contains("app/src/lib.fe"), "{cause}");
    assert!(
        cause.contains("write the implementation in the package's own code"),
        "{cause}"
    );
    // A generated implementation of an ordinary trait is lawful.
    let layout = Layout::new().ingot("app", &[], &behaviour_app("Other", ""));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// A `Pinned` implementation decides where a value may live.
#[test]
fn a_generated_pinned_implementation_is_refused() {
    let layout = Layout::new().ingot("app", &[], &behaviour_app("Pinned", "type Space = Storage"));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Pinned` for `X`"),
        "{cause}"
    );
    assert!(cause.contains("where a value may be kept"), "{cause}");
}

/// A generated implementation that adds a method name its type already has
/// through another trait is a warning, not an error: derive pairs often share
/// a method name, and the call keeps selecting the existing trait's method.
#[test]
fn a_generated_trait_implementation_that_shares_a_method_name_is_a_warning() {
    let middle = "impl a::Tr1 for X { const fn m(self) -> u256 { 1 } }
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Tr2 for X { const fn m(self) -> u256 { 2 } }
                0
            }
        }
    }
}
pub struct G {}
impl G with (Gen)
pub const fn read() -> u256 {
    let x = X {}
    x.m()
}";
    let layout = Layout::new().ingot("app", &[], &trait_scope_app(middle));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    let diagnostics = all_diagnostics(&prepared, "app");
    assert!(
        diagnostics
            .contains("the generated implementation of `Tr2` adds `m`, which `X` already has"),
        "{diagnostics}"
    );
    assert!(
        diagnostics.contains("through the trait `Tr1`"),
        "{diagnostics}"
    );
    assert!(diagnostics.contains("warning"), "{diagnostics}");
    // Controls: the same program with another method name, and the same
    // program without the generated implementation, have nothing to report.
    let fresh = middle.replace(
        "impl Tr2 for X { const fn m(self)",
        "impl Tr3 for X { const fn n(self)",
    );
    let layout = Layout::new().ingot(
        "app",
        &[],
        &trait_scope_app(&fresh).replace(
            "pub trait Tr2 {",
            "pub trait Tr3 { const fn n(self) -> u256 }\npub trait Tr2 {",
        ),
    );
    let prepared = accepted(&layout, "app");
    assert!(!all_diagnostics(&prepared, "app").contains("already has"));
}

// ---- A provider evaluates only bodies that check without errors --------------

const ILL_TYPED_CALLEE: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait A {}
pub struct W<T> {}
impl<T: A> W<T> { pub const fn m() -> u256 { 1 } }
pub struct X {}
IMPL_A
pub const fn read() -> u256 { W<X>::m() }
pub struct Observe {}
impl DeriveInherent for Observe {
    const fn derive(_ record: Record) -> Code<Item> {
        let c = read()
        quote { pub const fn seen() -> u256 { ${c} } }
    }
}
struct O {}
impl O with (Observe)
";

const GENERATE_A: &str = "pub struct GenA {}
impl DeriveInherent for GenA {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl A for X {}
                0
            }
        }
    }
}
struct G {}
impl G with (GenA)";

/// `read` needs `X: A`, which another provider's output supplies. In the view
/// the provider runs in, `read` does not check, so the provider must not
/// evaluate it: it would read a value that depends on output it cannot see.
#[test]
fn a_provider_cannot_evaluate_a_body_that_needs_another_providers_output() {
    let layout = Layout::new().ingot("app", &[], &ILL_TYPED_CALLEE.replace("IMPL_A", GENERATE_A));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the provider evaluates `read`, whose body does not check without errors"),
        "{cause}"
    );
    assert!(cause.contains(PROVIDER_VIEW), "{cause}");
    // The same program with the implementation written by hand checks in
    // every view, and the provider may evaluate `read`.
    let layout = Layout::new().ingot(
        "app",
        &[],
        &ILL_TYPED_CALLEE.replace("IMPL_A", "impl A for X {}"),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "seen"), "1");
}

/// An authored blanket implementation `impl<T: Marker> Tr for T` does not
/// apply to `X`, which is not a `Marker`: it is no member of `X`, so a
/// generated inherent method `m`, a member of the inherent header
/// `impl X with (Gen)`, is not hidden by it, and the call finds it.
#[test]
fn an_authored_blanket_method_whose_bound_is_unmet_does_not_hide_a_generated_method() {
    let source = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Marker {}
pub trait Tr { const fn m(self) -> u256 }
impl<T: Marker> Tr for T { const fn m(self) -> u256 { 2 } }
pub struct X {}
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn m(self) -> u256 { 5 } }
    }
}
impl X with (Gen)
pub const fn read() -> u256 {
    let x = X {}
    x.m()
}
";
    let layout = Layout::new().ingot("app", &[], source);
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "read"), "5");
}

const UNMET_BLANKET: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Marker {}
pub trait Tr { const fn m(self) -> u256 }
impl<T: Marker> Tr for T { const fn m(self) -> u256 { 2 } }
pub struct X {}
GENERATED
pub const fn read() -> u256 {
    let x = X {}
    x.m()
}
";

/// A generated `Marker` implementation makes the authored blanket
/// implementation apply: the call, which found no method in the program as
/// written, finds the blanket method. The generated item fills the gap.
#[test]
fn a_generated_marker_makes_an_authored_blanket_method_apply() {
    let source = UNMET_BLANKET.replace("GENERATED", &generated_in_member("impl Marker for X {}"));
    let layout = Layout::new().ingot("app", &[], &source);
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "read"), "2");
}

// ---- A dependency's generated items are sealed for its dependents -------------

const SEALED_LIB: &str = "use core::derive::Derive
use core::meta::{Record, Code, Item}
pub trait T1 { const fn m() -> u256 }
pub trait T2 { const fn m() -> u256 }
pub struct X {}
impl T1 for X { const fn m() -> u256 { 1 } }
pub struct Gen {}
impl Derive<trait T2> for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn m() -> u256 { 2 } }
    }
}
impl T2 for X with (Gen)
";

const SEALED_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use lib::X
pub const fn read() -> u256 { X::m() }
UNRELATED";

const UNRELATED_GENERATION: &str = "pub struct Fresh {}
impl DeriveInherent for Fresh {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn fresh_name() -> u256 { 3 } }
    }
}
pub struct H {}
impl H with (Fresh)
";

/// The dependency generates, for a header it wrote, a second implementation of
/// a method name that it implements itself. The dependent's call `X::m()` is
/// selected among both, which are the dependency's sealed items. Whether the
/// dependent generates something unrelated must not change that selection.
#[test]
fn a_dependents_unrelated_generation_does_not_change_a_dependency_method_call() {
    let report = |unrelated: &str| {
        let layout = Layout::new().ingot("lib", &[], SEALED_LIB).ingot(
            "app",
            &[("lib", "lib")],
            &SEALED_APP.replace("UNRELATED", unrelated),
        );
        let prepared = accepted(&layout, "app");
        errors(&prepared, "app")
    };
    let without = report("");
    let with = report(UNRELATED_GENERATION);
    assert_eq!(with, without);
}

// ---- Generated code is generated wherever it is written -----------------------
//
// An item is generated code of its ingot when generation put it among a
// module's items, gave it to an authored derive header, or when it is written
// inside either, such as an implementation in the body of a generated
// function. Every table and every refusal asks that one question.

const NESTED_DROP_HEAD: &str = "use core::meta::{Record, Code, Item}
use core::derive::Derive
use core::ops::Drop
use core::ptr
pub struct Counted { counter: *u256 }
";

const NESTED_DROP_BODY: &str = "impl Drop for Counted {
    fn drop(own self) { unsafe { *self.counter += 1 } }
}
";

fn nested_drop_in_generated_function(nested: &str) -> String {
    format!(
        "{NESTED_DROP_HEAD}use core::derive::DeriveInherent
pub struct Gen {{}}
impl DeriveInherent for Gen {{
    const fn derive(_ record: Record) -> Code<Item> {{
        quote {{
            pub fn helper() -> u256 {{
                {nested}
                0
            }}
        }}
    }}
}}
struct G {{}}
impl G with (Gen)
"
    )
}

fn nested_drop_in_header_member(nested: &str) -> String {
    format!(
        "{NESTED_DROP_HEAD}pub trait Tr {{ fn v() -> u256 }}
pub struct S {{}}
pub struct P {{}}
impl Derive<trait Tr> for P {{
    const fn derive(_ record: Record) -> Code<Item> {{
        quote {{
            fn v() -> u256 {{
                {nested}
                0
            }}
        }}
    }}
}}
impl Tr for S with (P)
"
    )
}

/// A `Drop` implementation written inside the body of a generated function,
/// here a member of an inherent header, is a generated `Drop` implementation:
/// it decides what authored code runs where a `Counted` goes out of use.
#[test]
fn a_drop_implementation_nested_in_a_generated_function_is_refused() {
    let layout = Layout::new().ingot(
        "app",
        &[],
        &nested_drop_in_generated_function(NESTED_DROP_BODY),
    );
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Drop` for `Counted`"),
        "{cause}"
    );
    // Control: the same generated function without the nested implementation.
    let layout = Layout::new().ingot("app", &[], &nested_drop_in_generated_function(""));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// The same for a member that a provider gives to an authored derive header.
#[test]
fn a_drop_implementation_nested_in_a_derive_header_member_is_refused() {
    let layout = Layout::new().ingot("app", &[], &nested_drop_in_header_member(NESTED_DROP_BODY));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Drop` for `Counted`"),
        "{cause}"
    );
    // Control: the same member without the nested implementation.
    let layout = Layout::new().ingot("app", &[], &nested_drop_in_header_member(""));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// Only an item that generation published into a module is bound by its name
/// there. A derive header's members join the header, so a package whose only
/// generation is headers has one answer for every name and import, and the
/// two tiers share their cache entries; the tables of implementations still
/// have two, because a member may hold an implementation.
#[test]
fn a_package_that_only_derives_headers_publishes_no_item() {
    use hir::analysis::name_resolution::{Tier, ingot_has_generated_items, ingot_publishes_items};
    fn ingot_of(db: &DriverDataBase) -> hir::Ingot<'_> {
        db.workspace()
            .containing_ingot(db, url("app/"))
            .expect("indexed ingot")
    }
    let layout = Layout::new().ingot("app", &[], &header_only_selection(""));
    let prepared = accepted(&layout, "app");
    let ingot = ingot_of(&prepared);
    assert!(*ingot_has_generated_items(&prepared, ingot));
    assert!(!*ingot_publishes_items(&prepared, ingot));
    assert_eq!(
        Tier::Authored.in_ingot_for_names(&prepared, ingot),
        Tier::All
    );
    assert_eq!(Tier::Authored.in_ingot(&prepared, ingot), Tier::Authored);
}

fn header_only_selection(nested: &str) -> String {
    format!(
        "use core::meta::{{Record, Code, Item}}
use core::derive::Derive
mod a {{
    pub trait Tr1 {{ const fn m(self) -> u256 }}
}}
pub trait Tr2 {{ const fn m(self) -> u256 }}
pub trait Tr {{ const fn v() -> u256 }}
pub struct X {{}}
impl a::Tr1 for X {{ const fn m(self) -> u256 {{ 1 }} }}
pub struct S {{}}
pub struct P {{}}
impl Derive<trait Tr> for P {{
    const fn derive(_ record: Record) -> Code<Item> {{
        quote {{
            const fn v() -> u256 {{
                {nested}
                0
            }}
        }}
    }}
}}
impl Tr for S with (P)
pub const fn read() -> u256 {{
    let x = X {{}}
    x.m()
}}
"
    )
}

/// A package whose only generation is a derive header has the same tiers as
/// any other. `x.m()` selects the only candidate, `a::Tr1::m`, even though
/// its trait is not imported; a `Tr2` implementation nested in the header's
/// member adds a candidate that the scope filter would prefer.
#[test]
fn a_package_that_only_derives_headers_keeps_authored_selection() {
    let nested = "impl Tr2 for X { const fn m(self) -> u256 { 2 } }";
    let layout = Layout::new().ingot("app", &[], &header_only_selection(nested));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "read"), "1");
    // Control: without the nested implementation the answer is the same.
    let layout = Layout::new().ingot("app", &[], &header_only_selection(""));
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "read"), "1");
}

// ---- A choice between implementations keeps its written answer ----------------
//
// Some decisions prefer one implementation to another: a call `f(x)` of a
// value runs `Fn` if it is implemented and `FnMut` otherwise, a `for` loop
// runs `Producer` before `Driver`, a `uses` requirement is answered by the
// innermost `with` frame that can. They are made among the implementations
// written in the package and its dependencies. A generated implementation
// that would make them differently is refused where it is requested; one
// that fills a gap nothing written answers is accepted.

const CALLABLE_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub struct Double {}
pub struct Plain {}
WRITTEN
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                GENERATED
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
pub const fn run() -> u256 {
    var d = Double {}
    d(5)
}
OBSERVER";

const CALLABLE_OBSERVER: &str = "pub struct Observe {}
impl DeriveInherent for Observe {
    const fn derive(_ record: Record) -> Code<Item> {
        let c = run()
        quote { pub const fn seen() -> u256 { ${c} } }
    }
}
struct O {}
impl O with (Observe)
";

const WRITTEN_FN_MUT: &str = "impl FnMut(u256) -> u256 for Double {
    const fn call_mut(mut self, _ x: u256) -> u256 { x * 2 }
}";
const GENERATED_FN: &str =
    "impl Fn(u256) -> u256 for Double { const fn call(self, _ x: u256) -> u256 { x * 3 } }";

fn callable_app(written: &str, generated: &str) -> String {
    CALLABLE_APP
        .replace("WRITTEN", written)
        .replace("GENERATED", generated)
        .replace("OBSERVER", CALLABLE_OBSERVER)
}

/// A provider cannot evaluate a body that needs a generated implementation to
/// check, so a program that depends on one has no observer.
fn callable_app_without_observer(written: &str, generated: &str) -> String {
    CALLABLE_APP
        .replace("WRITTEN", written)
        .replace("GENERATED", generated)
        .replace("OBSERVER", "")
}

/// A written `FnMut` implementation answers the call; a generated `Fn` one
/// would be tried first and run another method than the one the provider saw.
#[test]
fn a_generated_fn_implementation_cannot_outrank_a_written_fn_mut() {
    let layout = Layout::new().ingot("app", &[], &callable_app(WRITTEN_FN_MUT, GENERATED_FN));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Fn"),
        "{cause}"
    );
    assert!(cause.contains("for `Double`"), "{cause}");
    assert!(cause.contains("requested at"), "{cause}");
    assert!(cause.contains("app/src/lib.fe"), "{cause}");
    assert!(
        cause.contains("which implementation a call of a value runs"),
        "{cause}"
    );
    assert!(cause.contains("is refused"), "{cause}");
    // Control: a generator that adds nothing to `Double`.
    let layout = Layout::new().ingot(
        "app",
        &[],
        &callable_app(WRITTEN_FN_MUT, "pub const fn other() -> u256 { 1 }"),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "run"), "10");
    assert_eq!(value(&prepared, "app", "seen"), "10");
}

/// `Fn` is one of the traits whose presence decides behaviour, so a generated
/// implementation is refused whether or not written code calls a value of the
/// type. A header requests it, like any other trait of that kind.
#[test]
fn a_generated_fn_implementation_is_refused_though_nothing_written_answers() {
    let layout = Layout::new().ingot("app", &[], &callable_app_without_observer("", GENERATED_FN));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Fn(u256) -> _` for `Double`"),
        "{cause}"
    );
    assert!(
        cause.contains("which implementation a call of a value runs"),
        "{cause}"
    );
    assert!(
        cause.contains("request it with a header `impl Fn(u256) -> _ for Type with (Provider)`"),
        "{cause}"
    );
    assert!(
        !cause.contains("cannot ask for a callable trait"),
        "{cause}"
    );
}

/// The header is the request form of a callable: it is a written
/// implementation, so it answers a call as a written `Fn` does, before a
/// written `FnMut`, which a generated `Fn` could not outrank.
#[test]
fn a_header_requests_a_callable_where_a_generated_one_is_refused() {
    let header = format!(
        "use core::derive::Derive
use core::functional::Fn
use core::meta::{{Record, Code, Item}}
pub struct Double {{}}
pub struct Calls {{}}
impl Derive<trait Fn(u256) -> u256> for Calls {{
    const fn derive(_ record: Record) -> Code<Item> {{
        quote {{ const fn call(self, _ x: u256) -> u256 {{ x * 3 }} }}
    }}
}}
impl Fn(u256) -> u256 for Double with (Calls)
{WRITTEN_FN_MUT}
pub const fn run() -> u256 {{
    var d = Double {{}}
    d(5)
}}
"
    );
    let prepared = accepted(&Layout::new().ingot("app", &[], &header), "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "run"), "15");
    // Control: without the header the written `FnMut` answers.
    let written = header.replace("impl Fn(u256) -> u256 for Double with (Calls)\n", "");
    let layout = Layout::new().ingot("app", &[], &written);
    assert_eq!(value(&layout.db(), "app", "run"), "10");
}

/// The same when a provider gives the `Fn` implementation to an authored
/// derive header, inside the body of a member.
#[test]
fn a_fn_implementation_in_a_derive_header_member_cannot_outrank_a_written_fn_mut() {
    let src = |nested: &str| {
        format!(
            "use core::derive::Derive
use core::meta::{{Record, Code, Item}}
pub struct Double {{}}
{WRITTEN_FN_MUT}
pub const fn run() -> u256 {{
    var d = Double {{}}
    d(5)
}}
pub trait Tr {{ const fn v() -> u256 }}
pub struct S {{}}
pub struct P {{}}
impl Derive<trait Tr> for P {{
    const fn derive(_ record: Record) -> Code<Item> {{
        quote {{
            const fn v() -> u256 {{
                {nested}
                0
            }}
        }}
    }}
}}
impl Tr for S with (P)
"
        )
    };
    let layout = Layout::new().ingot("app", &[], &src(GENERATED_FN));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Fn"),
        "{cause}"
    );
    assert!(
        cause.contains("which implementation a call of a value runs"),
        "{cause}"
    );
    let layout = Layout::new().ingot("app", &[], &src(""));
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "run"), "10");
}

// ---- The protocol of a `for` loop keeps its written answer --------------------

const LOOP_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use core::iter::{Collection, Driver, Producer}
pub struct D {}
impl Copy for D {}
WRITTEN
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                GENERATED
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
pub fn run() -> u256 {
    let xs: [u256; 3] = [1, 2, 3]
    var s = 0
    for x in xs.by(D {}) {
        s += x
    }
    s
}
";

const WRITTEN_DRIVER: &str = "impl Driver for D {
    type Base = [u256; 3]
    type State = usize
    shape Step = mut? u256
    fn start(self, _ base: [u256; 3]) -> usize { 0 }
    fn has(self, _ base: [u256; 3], _ s: usize) -> bool { s < 3 }
    fn next(self, _ base: [u256; 3], _ s: usize) -> usize { s + 1 }
    fn at(self, _ base: mut? [u256; 3], _ s: usize) -> mut? u256 { Collection::at(mut? base, s) }
}";

const GENERATED_PRODUCER: &str = "impl Producer for D {
    type Base = [u256; 3]
    type State = usize
    type Out = u256
    fn start(self, _ base: [u256; 3]) -> usize { 0 }
    fn has(self, _ base: [u256; 3], _ s: usize) -> bool { s < 3 }
    fn next(self, _ base: [u256; 3], _ s: usize) -> usize { s + 1 }
    fn produce(mut self, _ base: [u256; 3], _ s: usize) -> u256 { 100 }
}";

fn loop_app(written: &str, generated: &str) -> String {
    LOOP_APP
        .replace("WRITTEN", written)
        .replace("GENERATED", generated)
}

/// A producer is tried before a driver, so a generated `Producer` for a type
/// with a written `Driver` would make the loop run `produce` instead of `at`.
#[test]
fn a_generated_producer_cannot_outrank_a_written_driver() {
    let layout = Layout::new().ingot("app", &[], &loop_app(WRITTEN_DRIVER, GENERATED_PRODUCER));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Producer"),
        "{cause}"
    );
    assert!(cause.contains("for `D`"), "{cause}");
    assert!(cause.contains("requested at"), "{cause}");
    assert!(
        cause.contains("which protocol a `for` loop runs"),
        "{cause}"
    );
    // Control: a generator that adds nothing to `D` leaves the written driver.
    let layout = Layout::new().ingot(
        "app",
        &[],
        &loop_app(WRITTEN_DRIVER, "pub const fn other() -> u256 { 1 }"),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// `Producer` is one of the traits whose presence decides behaviour, so a
/// generated implementation is refused whether or not a loop runs over a type
/// that a written `Driver` covers. A header can request it, because it takes
/// no parameter.
#[test]
fn a_generated_producer_is_refused_though_nothing_written_answers() {
    let layout = Layout::new().ingot("app", &[], &loop_app("", GENERATED_PRODUCER));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Producer` for `D`"),
        "{cause}"
    );
    assert!(
        cause.contains("which protocol a `for` loop runs"),
        "{cause}"
    );
    assert!(
        cause.contains("request it with a header `impl Producer for Type with (Provider)`"),
        "{cause}"
    );
}

// ---- The provider that answers a `uses` requirement keeps its written answer --

const EFFECT_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Rd { fn rd(self) -> u256 }
pub trait Other { fn other(self) -> u256 }
pub struct A {}
impl Rd for A { fn rd(self) -> u256 { 1 } }
pub struct B {}
impl Other for B { fn other(self) -> u256 { 7 } }
fn read() -> u256 uses (c: Rd) { c.rd() }
fn read_other() -> u256 uses (o: Other) { o.other() }
pub fn run() -> u256 {
    let a = A {}
    let b = B {}
    with (a) {
        with (b) {
            read() + read_other()
        }
    }
}
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                GENERATED
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
";

/// The innermost `with` frame whose provider implements `Rd` answers `read`.
/// `B` does not implement `Rd` in the written code, so `a` answers; a generated
/// `impl Rd for B` would make the inner frame answer instead.
#[test]
fn a_generated_implementation_cannot_make_an_inner_provider_answer() {
    let layout = Layout::new().ingot(
        "app",
        &[],
        &EFFECT_APP.replace("GENERATED", "impl Rd for B { fn rd(self) -> u256 { 2 } }"),
    );
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Rd"),
        "{cause}"
    );
    assert!(cause.contains("for `B`"), "{cause}");
    assert!(cause.contains("requested at"), "{cause}");
    assert!(
        cause.contains("which provider answers a `uses` requirement"),
        "{cause}"
    );
    // A body makes this decision, so the change is found there.
    assert!(cause.contains("it would change"), "{cause}");
    // Control: a generator that adds nothing to `B`.
    let layout = Layout::new().ingot(
        "app",
        &[],
        &EFFECT_APP.replace("GENERATED", "pub const fn other() -> u256 { 1 }"),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// Where no `with` frame has a provider that implements `Rd` in the written
/// code, a generated implementation fills the gap.
#[test]
fn a_generated_implementation_lets_a_provider_answer_where_none_did() {
    let src = EFFECT_APP
        .replace("impl Rd for A { fn rd(self) -> u256 { 1 } }", "")
        .replace("GENERATED", "impl Rd for A { fn rd(self) -> u256 { 1 } }");
    let layout = Layout::new().ingot("app", &[], &src);
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

// ---- Facts that depend on an implementation being absent ----------------------
//
// Whether a type is `Copy`, has a `Drop` or `Pinned` implementation, is
// `SolPacked` or a snapshot is decided among the implementations written in
// the package and its dependencies. A generated implementation that would turn
// such a fact on for a type that written code uses is refused where it is
// requested; where written code does not use the type that way, it is a
// lawful addition.

const COPY_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub struct P { pub v: u256 }
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                GENERATED
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
";

/// A generated `Copy` implementation would turn a use after a move, which the
/// written code reports, into a copy.
#[test]
fn a_generated_copy_implementation_is_refused() {
    let layout = Layout::new().ingot(
        "app",
        &[],
        &COPY_APP.replace("GENERATED", "impl Copy for P {}"),
    );
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Copy` for `P`"),
        "{cause}"
    );
    assert!(
        cause.contains("whether a value is copied or moved"),
        "{cause}"
    );
    assert!(cause.contains("requested at"), "{cause}");
    // Control: a generated implementation of another trait.
    let layout = Layout::new().ingot(
        "app",
        &[],
        &COPY_APP.replace("GENERATED", "pub const fn other() -> u256 { 1 }"),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

// ---- Traits whose presence decides behaviour, refused from definitions --------
//
// A trait that declares `#[decides = "..."]` in core or std (`Copy`, `Drop`,
// `Snapshot`, `Pinned`, `StateIndex`, `SolPacked`, `ErrorVariant`, `Fn`,
// `FnMut`, `Producer`, `Driver`) changes what written code does by being
// implemented. A generated implementation of one is refused where it is
// requested, and so is a generated implementation of any trait that would
// make a written implementation of one apply to more types. Neither depends
// on whether written code uses the type.

const PRESENCE_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use core::error::ErrorVariant
use core::marker::{Pinned, Snapshot, Storage}
use core::ops::{Location, StateIndex}
use std::abi::Sol
pub trait Marker {}
pub trait Outer {}
pub struct P { pub v: u256 }
pub struct X {}
pub struct W<T> { pub v: u256 }
#[abi]
pub struct MyErr { pub code: u256 }
pub struct Cells { pub len: u256 }
WRITTEN
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                GENERATED
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
";

fn presence_app(written: &str, generated: &str) -> String {
    PRESENCE_APP
        .replace("WRITTEN", written)
        .replace("GENERATED", generated)
}

const OTHER: &str = "pub const fn other() -> u256 { 1 }";

/// `ErrorVariant<Sol>` decides what data a revert with a value carries: a
/// generated implementation would change the revert data of a written
/// `unwrap()` from the plain encoding to a selector-prefixed one.
#[test]
fn a_generated_error_variant_implementation_is_refused() {
    let generated = "impl ErrorVariant<Sol> for MyErr { const SELECTOR: u32 = 0x12345678 }";
    let layout = Layout::new().ingot("app", &[], &presence_app("", generated));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `ErrorVariant"),
        "{cause}"
    );
    assert!(cause.contains("for `MyErr`"), "{cause}");
    assert!(cause.contains("it could change"), "{cause}");
    assert!(
        cause.contains("what data a revert with a value carries"),
        "{cause}"
    );
    assert!(cause.contains("requested at"), "{cause}");
    // A header can ask for it: the provider gets the bare trait.
    assert!(
        cause
            .contains("request it with a header `impl ErrorVariant<Sol> for Type with (Provider)`"),
        "{cause}"
    );
    let layout = Layout::new().ingot("app", &[], &presence_app("", OTHER));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// A generated `Snapshot` for a type that no written code uses is refused as
/// well: the rule is about the definition, not about a use.
#[test]
fn a_generated_snapshot_implementation_is_refused_for_an_unused_type() {
    let layout = Layout::new().ingot(
        "app",
        &[],
        &presence_app("impl Copy for P {}", "impl Snapshot for P {}"),
    );
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Snapshot` for `P`"),
        "{cause}"
    );
    assert!(
        cause.contains("whether a view takes a value by copy"),
        "{cause}"
    );
    assert!(
        cause.contains("request it with a header `impl Snapshot for Type with (Provider)`"),
        "{cause}"
    );
}

const GENERATED_STATE_INDEX: &str = "unsafe impl StateIndex for Cells {
    type Key = u256
    type Entry = u256
    fn locate(self, _ root: u256, _ key: u256) -> Location {
        Location { slot: root + key, offset: 0 }
    }
}";

#[test]
fn a_generated_state_index_implementation_is_refused() {
    let written = "impl Pinned for Cells { type Space = Storage }";
    let layout = Layout::new().ingot("app", &[], &presence_app(written, GENERATED_STATE_INDEX));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `StateIndex` for `Cells`"),
        "{cause}"
    );
    assert!(
        cause.contains("whether indexing a collection is a call or an entry"),
        "{cause}"
    );
}

/// A written `Copy` implementation bounded by `Marker` applies to `W<X>` only
/// if `X` is a `Marker`: the generated `Marker` implementation is refused
/// although nothing written uses `W<X>`.
#[test]
fn a_generated_marker_cannot_make_a_written_copy_apply() {
    let written = "impl<T: Marker> Copy for W<T> {}";
    let layout = Layout::new().ingot("app", &[], &presence_app(written, "impl Marker for X {}"));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Marker` for `X`"),
        "{cause}"
    );
    assert!(
        cause.contains("whether a value is copied or moved"),
        "{cause}"
    );
    assert!(cause.contains("for `W<T>`"), "{cause}");
    // The refusal follows from the definitions: nothing written uses `W<X>`.
    assert!(cause.contains("it could change"), "{cause}");
    // A header counts as written, so it is the way to ask for the marker.
    assert!(
        cause.contains("request it with a header `impl Marker for Type with (Provider)`"),
        "{cause}"
    );
    // Control: written, the same implementation is lawful, and a generated
    // implementation of a trait that no such bound mentions is too.
    let layout = Layout::new().ingot(
        "app",
        &[],
        &presence_app(&format!("{written}\nimpl Marker for X {{}}"), OTHER),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    let layout = Layout::new().ingot("app", &[], &presence_app(written, "impl Outer for X {}"));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// The bound can lead to the generated implementation through a written
/// implementation of another trait.
#[test]
fn a_generated_marker_cannot_make_a_written_copy_apply_through_another_trait() {
    let written = "impl<T: Outer> Copy for W<T> {}
impl<T: Marker> Outer for T {}";
    let layout = Layout::new().ingot("app", &[], &presence_app(written, "impl Marker for X {}"));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Marker` for `X`"),
        "{cause}"
    );
    assert!(cause.contains("for `W<T>`"), "{cause}");
}

/// A chain of any length leads to the generated implementation: here five
/// written implementations connect `Copy` to `Marker`, and two traits bound
/// each other, which the search must not follow forever.
#[test]
fn a_generated_marker_cannot_make_a_written_copy_apply_through_a_long_chain() {
    let written = "pub trait L1 {}
pub trait L2 {}
pub trait L3 {}
pub trait L4 {}
pub trait L5 {}
impl<T: L1> Copy for W<T> {}
impl<T: L2> L1 for T {}
impl<T: L3> L2 for T {}
impl<T: L4> L3 for T {}
impl<T: L5> L4 for T {}
impl<T: Marker> L5 for T {}
impl<T: Marker> Outer for T {}
pub trait Ping {}
pub trait Pong {}
impl<T: Pong> Ping for T {}
impl<T: Ping> Pong for T {}
impl<T: Ping> Pinned for W<T> { type Space = Storage }";
    let layout = Layout::new().ingot("app", &[], &presence_app(written, "impl Marker for X {}"));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Marker` for `X`"),
        "{cause}"
    );
    assert!(cause.contains("for `W<T>`"), "{cause}");
    // Two traits that bound each other lead back to themselves, and a
    // generated implementation of one is refused; the search ends.
    let layout = Layout::new().ingot("app", &[], &presence_app(written, "impl Pong for X {}"));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Pong` for `X`"),
        "{cause}"
    );
    // Control: a trait that no bound leads to.
    let layout = Layout::new().ingot("app", &[], &presence_app(written, "impl Outer for X {}"));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// A bound on a projection is met by whichever type the projection comes to
/// be, so a generated implementation of its trait might widen the written
/// implementation: `W<X>` is `Copy` once `Marker` is implemented for the
/// associated type of `X`.
#[test]
fn a_generated_marker_cannot_make_a_written_copy_apply_through_a_projection() {
    let written = "pub trait Tr { type Assoc }
pub struct Y {}
impl Tr for X { type Assoc = Y }
impl<T: Tr> Copy for W<T> where T::Assoc: Marker {}";
    let layout = Layout::new().ingot("app", &[], &presence_app(written, "impl Marker for Y {}"));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Marker` for `Y`"),
        "{cause}"
    );
    assert!(
        cause.contains("whether a value is copied or moved"),
        "{cause}"
    );
    let layout = Layout::new().ingot("app", &[], &presence_app(written, OTHER));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// A named implementation is chosen by name and decides nothing implicitly,
/// so the rule for generated implementations does not refuse it. A named
/// `Copy` is refused where it is declared, whether written or generated.
#[test]
fn a_generated_named_implementation_of_copy_is_refused_only_as_a_declaration() {
    let layout = Layout::new().ingot(
        "app",
        &[],
        &presence_app("", "impl Copy for P as Shared {}"),
    );
    let report = match prepare(&layout, "app") {
        Ok(prepared) => errors(&prepared, "app"),
        Err(cause) => cause,
    };
    assert!(
        report.contains("`Copy` cannot have a named implementation"),
        "{report}"
    );
    assert!(
        !report.contains("the generated implementation of `Copy`"),
        "{report}"
    );
}

const CODEC_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use core::ops::LaneCodec
use std::evm::{SolLanes, SolPacked}
pub trait Marker {}
pub struct X { pub a: u256 }
pub struct W<T> { pub a: u256 }
impl<T: Marker> SolPacked for W<T> {
    const BITS: u256 = 8
    fn to_packed(own self) -> u256 { self.a }
    fn from_packed(_ bits: u256) -> W<T> { W<T> { a: bits } }
}
pub const fn bits() -> u256 { <SolLanes as LaneCodec<USED>>::BITS }
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                GENERATED
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
";

const GENERATED_SOL_PACKED: &str = "impl SolPacked for X {
    const BITS: u256 = 8
    fn to_packed(own self) -> u256 { self.a }
    fn from_packed(_ bits: u256) -> X { X { a: bits } }
}";

fn codec_app(used: &str, generated: &str) -> String {
    CODEC_APP
        .replace("USED", used)
        .replace("GENERATED", generated)
}

/// Without a `SolPacked` implementation a type takes whole slots, which
/// `bits()` reads. A generated `SolPacked` implementation would pack it.
#[test]
fn a_generated_sol_packed_cannot_change_a_storage_codec() {
    let layout = Layout::new().ingot("app", &[], &codec_app("X", GENERATED_SOL_PACKED));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `SolPacked"),
        "{cause}"
    );
    assert!(cause.contains("for `X`"), "{cause}");
    assert!(cause.contains("how a stored value is packed"), "{cause}");
    assert!(cause.contains("requested at"), "{cause}");
    // Control: nothing generated, the whole-slot codec.
    let layout = Layout::new().ingot(
        "app",
        &[],
        &codec_app("X", "pub const fn other() -> u256 { 1 }"),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "bits"), "256");
    // The refusal does not depend on written code reading the codec of `X`:
    // `SolPacked` is a trait whose presence decides how a type is packed,
    // wherever it is stored.
    let layout = Layout::new().ingot("app", &[], &codec_app("u256", GENERATED_SOL_PACKED));
    let cause = refused(&layout, "app");
    assert!(cause.contains("how a stored value is packed"), "{cause}");
    assert!(
        cause.contains("request it with a header `impl SolPacked for Type with (Provider)`"),
        "{cause}"
    );
}

/// The same when a written, bounded `SolPacked` implementation applies to
/// `W<X>` only because a generated `Marker` implementation meets its bound.
/// No `SolPacked` implementation is generated, and the generated `Marker`
/// implementation is the one refused.
#[test]
fn a_generated_marker_cannot_make_a_written_sol_packed_apply() {
    let layout = Layout::new().ingot("app", &[], &codec_app("W<X>", "impl Marker for X {}"));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Marker` for `X`"),
        "{cause}"
    );
    assert!(cause.contains("how a stored value is packed"), "{cause}");
    assert!(cause.contains("for `W<T>`"), "{cause}");
    let layout = Layout::new().ingot(
        "app",
        &[],
        &codec_app("W<X>", "pub const fn other() -> u256 { 1 }"),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(value(&prepared, "app", "bits"), "256");
}

const PIN_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use core::marker::{Pinned, Storage}
pub trait Marker {}
pub struct Wrap<T> { pub v: u256 }
impl<T: Marker> Pinned for Wrap<T> {
    type Space = Storage
}
pub struct X {}
pub fn run() -> u256 {
    var w = Wrap<X> { v: 1 }
    w.v = 2
    w.v
}
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                GENERATED
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
";

/// A written bounded `Pinned` implementation pins `Wrap<X>` only if `X` is a
/// `Marker`. A generated `Marker` implementation would pin the local `w`.
#[test]
fn a_generated_marker_cannot_make_a_written_pinned_apply() {
    let layout = Layout::new().ingot(
        "app",
        &[],
        &PIN_APP.replace("GENERATED", "impl Marker for X {}"),
    );
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Marker` for `X`"),
        "{cause}"
    );
    assert!(cause.contains("where a value may be kept"), "{cause}");
    let layout = Layout::new().ingot(
        "app",
        &[],
        &PIN_APP.replace("GENERATED", "pub const fn other() -> u256 { 1 }"),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

// ---- Operators select like methods --------------------------------------------

const OPERATOR_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use core::ops::Add
pub struct X { pub v: u256 }
impl Add<u256> for X {
    type Output = u256
    const fn add(own self, _ rhs: own u256) -> u256 { self.v + rhs }
}
pub const fn run() -> u256 {
    let x = X { v: 1 }
    x + 1
}
pub const fn small() -> u256 {
    let x = X { v: 1 }
    let y: u8 = 1
    x + y
}
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                GENERATED
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
";

const GENERATED_ADD_U8: &str = "impl Add<u8> for X {
    type Output = u256
    const fn add(own self, _ rhs: own u8) -> u256 { 100 }
}";

/// The written `Add<u256>` answers `x + 1`; a generated `Add<u8>` also accepts
/// the literal `1`, which made the use ambiguous at the written line.
#[test]
fn a_generated_operator_implementation_does_not_make_a_written_use_ambiguous() {
    let src = OPERATOR_APP.replace("GENERATED", GENERATED_ADD_U8);
    let src = src.replace(
        "pub const fn small() -> u256 {\n    let x = X { v: 1 }\n    let y: u8 = 1\n    x + y\n}\n",
        "",
    );
    let layout = Layout::new().ingot("app", &[], &src);
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "run"), "2");
}

/// A use that no written implementation fits is answered by the generated one.
#[test]
fn a_generated_operator_implementation_fills_a_gap_nothing_written_answers() {
    let layout = Layout::new().ingot(
        "app",
        &[],
        &OPERATOR_APP.replace("GENERATED", GENERATED_ADD_U8),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "run"), "2");
    assert_eq!(value(&prepared, "app", "small"), "100");
}

/// A call by method syntax takes the arguments into account as the operator
/// does: `x.add(1)` keeps the written `Add<u256>`, and `x.add(y)` with a `u8`
/// finds the generated `Add<u8>`, because the written candidate does not
/// accept it. Before, the written method was selected by its name alone and
/// the call was refused at the written line.
#[test]
fn a_generated_method_fills_a_gap_when_the_written_one_does_not_accept_the_arguments() {
    let src = OPERATOR_APP
        .replace("GENERATED", GENERATED_ADD_U8)
        .replace("    x + 1\n", "    x.add(1)\n")
        .replace("    x + y\n", "    x.add(y)\n");
    let layout = Layout::new().ingot("app", &[], &src);
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "run"), "2");
    assert_eq!(value(&prepared, "app", "small"), "100");
}

/// A string literal takes its type from the parameter it is passed to: a byte
/// array where the parameter is one. It is not typed before the choice, so
/// `x.m("abc")` keeps the written candidate that takes bytes, and the generated
/// candidate that takes a `String<3>` does not replace it.
#[test]
fn a_generated_method_does_not_replace_a_written_one_for_a_string_literal() {
    let src = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Tk<A> { const fn m(self, _ a: A) -> u256 }
pub struct X {}
impl Tk<[u8; 3]> for X { const fn m(self, _ a: [u8; 3]) -> u256 { 1 } }
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Tk<String<3>> for X { const fn m(self, _ a: String<3>) -> u256 { 2 } }
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
pub const fn run() -> u256 {
    let x = X {}
    x.m(\"abc\")
}
";
    let layout = Layout::new().ingot("app", &[], src);
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
    assert_eq!(value(&prepared, "app", "run"), "1");
}

/// Without the generated implementation the same call is an ordinary
/// mismatch, so the gap is the generated implementation's alone.
#[test]
fn a_written_method_that_does_not_accept_the_arguments_is_a_mismatch() {
    let src = OPERATOR_APP
        .replace("GENERATED", "pub const fn other() -> u256 { 1 }")
        .replace("    x + 1\n", "    x.add(1)\n")
        .replace("    x + y\n", "    x.add(y)\n");
    let layout = Layout::new().ingot("app", &[], &src);
    let prepared = accepted(&layout, "app");
    let diagnostics = errors(&prepared, "app");
    assert!(diagnostics.contains("u8"), "{diagnostics}");
    assert!(!diagnostics.is_empty(), "{diagnostics}");
}

// ---- A generated implementation that makes a selection ambiguous --------------
//
// A unique selection (inference of a trait parameter, one implementation per
// type) that a generated implementation turns into an ambiguity or an overlap
// is an error where the selection is made, with the message that a written
// implementation would give. It is never a silent change of meaning, so no
// refusal at the request is needed: generation cannot change which
// implementation a written use selects, only make the use fail.

const AMBIGUITY_APP: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Conv<T> { fn conv(self) -> T }
pub struct X {}
impl Conv<u256> for X { fn conv(self) -> u256 { 5 } }
WRITTEN
fn conv_of<S: Conv<T>, T>(_ s: S) -> T { s.conv() }
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                GENERATED
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
pub fn run() -> bool {
    let y = conv_of(X {})
    y == 5
}
";

const CONV_U8: &str = "impl Conv<u8> for X { fn conv(self) -> u8 { 6 } }";

#[test]
fn a_generated_implementation_that_makes_inference_ambiguous_is_an_error_at_the_use() {
    let generated = AMBIGUITY_APP
        .replace("WRITTEN", "")
        .replace("GENERATED", CONV_U8);
    let layout = Layout::new().ingot("app", &[], &generated);
    let diagnostics = errors(&accepted(&layout, "app"), "app");
    assert!(
        diagnostics.contains("type annotation is needed"),
        "{diagnostics}"
    );
    // Written, the same implementation gives the same error.
    let written = AMBIGUITY_APP
        .replace("WRITTEN", CONV_U8)
        .replace("GENERATED", OTHER);
    let layout = Layout::new().ingot("app", &[], &written);
    let diagnostics = errors(&accepted(&layout, "app"), "app");
    assert!(
        diagnostics.contains("type annotation is needed"),
        "{diagnostics}"
    );
    // Control: neither.
    let plain = AMBIGUITY_APP
        .replace("WRITTEN", "")
        .replace("GENERATED", OTHER);
    let layout = Layout::new().ingot("app", &[], &plain);
    assert_eq!(errors(&accepted(&layout, "app"), "app"), "");
}

#[test]
fn a_generated_implementation_that_makes_written_implementations_overlap_is_an_error() {
    let app = |generated: &str| {
        let carrier = generated_in_member(generated);
        format!(
            "use core::derive::{{Derive, DeriveInherent}}
use core::meta::{{Record, Code, Item}}
pub trait Marker {{}}
pub trait Tr {{ fn v() -> u256 }}
impl<T: Marker> Tr for T {{ fn v() -> u256 {{ 1 }} }}
pub struct X {{}}
impl Tr for X {{ fn v() -> u256 {{ 2 }} }}
pub fn run() -> u256 {{ X::v() }}
{carrier}"
        )
    };
    let layout = Layout::new().ingot("app", &[], &app("impl Marker for X {}"));
    let diagnostics = errors(&accepted(&layout, "app"), "app");
    assert!(
        diagnostics.contains("conflicting trait implementations"),
        "{diagnostics}"
    );
    let layout = Layout::new().ingot("app", &[], &app(OTHER));
    assert_eq!(errors(&accepted(&layout, "app"), "app"), "");
}

// ---- A path through two globs of a dependency --------------------------------

/// A dependency whose `api` re-exports two modules that both define `f`, by
/// hand. Nothing is generated, so the authored view is the program, and `f` is
/// ambiguous whether it is reached by a path or by a glob import of `api`.
#[test]
fn a_path_through_two_written_globs_of_a_dependency_is_ambiguous() {
    let lib = "pub mod a { pub fn f() -> u256 { 1 } }
pub mod b { pub fn f() -> u256 { 2 } }
pub mod api {
    pub use super::a::*
    pub use super::b::*
}
";
    let app = |body: &str| {
        let layout = Layout::new()
            .ingot("lib", &[], lib)
            .ingot("app", &[("lib", "lib")], body);
        errors(&layout.db(), "app")
    };
    let by_path = app("pub fn r() -> u256 { lib::api::f() }");
    let by_glob = app("use lib::api::*\npub fn r() -> u256 { f() }");
    assert!(by_path.contains("`f` is ambiguous"), "{by_path}");
    assert!(by_glob.contains("`f` is ambiguous"), "{by_glob}");
}

/// A written generic function reads the codec of `W<T>`, which takes whole
/// slots while `W<T>` has no `SolPacked` implementation for any `T`. A
/// generated blanket `SolPacked` implementation for `W<T>` would pack it.
#[test]
fn a_generated_blanket_sol_packed_cannot_change_a_generic_storage_codec() {
    let src = |generated: &str| {
        CODEC_APP
            .replace(
                "pub const fn bits() -> u256 { <SolLanes as LaneCodec<USED>>::BITS }",
                "pub const fn bits<T>() -> u256 { <SolLanes as LaneCodec<W<T>>>::BITS }",
            )
            .replace("GENERATED", generated)
    };
    let blanket = "impl<T> SolPacked for W<T> {
    const BITS: u256 = 8
    fn to_packed(own self) -> u256 { self.a }
    fn from_packed(_ bits: u256) -> W<T> { W<T> { a: bits } }
}";
    let layout = Layout::new().ingot("app", &[], &src(blanket));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `SolPacked"),
        "{cause}"
    );
    assert!(cause.contains("how a stored value is packed"), "{cause}");
    let layout = Layout::new().ingot("app", &[], &src("pub const fn other() -> u256 { 1 }"));
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// A dependency declares a bounded `Pinned` implementation and writes the
/// `Marker` implementation that meets its bound. Both are ordinary items for
/// the dependent, so its `Wrap<X>` is pinned by every query alike: a `var` of
/// it is refused as state-only. Generating the `Marker` implementation
/// instead is refused in the dependency, so no dependent meets a pin that
/// generation changed.
#[test]
fn a_dependents_pin_of_a_bounded_type_sees_the_dependencys_marker() {
    let lib = |marker: &str| {
        format!(
            "use core::meta::{{Record, Code, Item}}
use core::marker::{{Pinned, Storage}}
pub trait Marker {{}}
pub struct Wrap<T> {{ pub v: u256 }}
impl<T: Marker> Pinned for Wrap<T> {{
    type Space = Storage
}}
pub struct X {{}}
{marker}
"
        )
    };
    let app = "use lib::{Wrap, X}
pub fn run() -> u256 {
    var w = Wrap<X> { v: 1 }
    w.v = 2
    w.v
}
";
    // Nothing is generated, so there is nothing to prepare: the authored view
    // is the program.
    let layout = Layout::new()
        .ingot("lib", &[], &lib("impl Marker for X {}"))
        .ingot("app", &[("lib", "lib")], app);
    let diagnostics = errors(&layout.db(), "app");
    assert!(diagnostics.contains("is state-only"), "{diagnostics}");
    let carrier = generated_in_member("impl Marker for X {}");
    let generated = lib(&format!(
        "use core::derive::{{Derive, DeriveInherent}}\n{carrier}"
    ));
    let layout = Layout::new().ingot("lib", &[], &generated);
    let cause = refused(&layout, "lib");
    assert!(
        cause.contains("the generated implementation of `Marker` for `X`"),
        "{cause}"
    );
    assert!(cause.contains("where a value may be kept"), "{cause}");
}

/// The bounded `Pinned` implementation is a dependency's, and the package
/// being prepared generates the `Marker` that meets its bound and uses
/// `Wrap<X>` in written code. The pin is read from the package that asks, so
/// the generation is refused there, as when the implementation is the
/// package's own.
#[test]
fn a_generated_marker_cannot_make_a_dependencys_pinned_implementation_apply() {
    let lib = "use core::marker::{Pinned, Storage}
pub trait Marker {}
pub struct Wrap<T> { pub v: u256 }
impl<T: Marker> Pinned for Wrap<T> {
    type Space = Storage
}
";
    let app = |generated: &str| {
        let carrier = generated_in_member(generated);
        format!(
            "use core::derive::{{Derive, DeriveInherent}}
use core::meta::{{Record, Code, Item}}
use lib::{{Marker, Wrap}}
pub struct X {{}}
pub fn run() -> u256 {{
    var w = Wrap<X> {{ v: 1 }}
    w.v = 2
    w.v
}}
{carrier}"
        )
    };
    let layout = Layout::new().ingot("lib", &[], lib).ingot(
        "app",
        &[("lib", "lib")],
        &app("impl Marker for X {}"),
    );
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Marker` for `X`"),
        "{cause}"
    );
    assert!(cause.contains("where a value may be kept"), "{cause}");
    let layout = Layout::new().ingot("lib", &[], lib).ingot(
        "app",
        &[("lib", "lib")],
        &app("pub const fn other() -> u256 { 1 }"),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// A written generic function reads the codec of `W<T>` under the assumption
/// `T: Marker`. A generated `SolPacked` implementation bounded by `Marker`
/// would pack `W<T>` in that function, so the comparison reads the type under
/// the assumptions of the body.
#[test]
fn a_generated_bounded_sol_packed_cannot_change_a_codec_under_a_bound() {
    let src = |generated: &str| {
        CODEC_APP
            .replace(
                "pub const fn bits() -> u256 { <SolLanes as LaneCodec<USED>>::BITS }",
                "pub const fn bits<T: Marker>() -> u256 { <SolLanes as LaneCodec<W<T>>>::BITS }",
            )
            .replace("GENERATED", generated)
    };
    let bounded = "impl<T: Marker> SolPacked for W<T> {
    const BITS: u256 = 8
    fn to_packed(own self) -> u256 { self.a }
    fn from_packed(_ bits: u256) -> W<T> { W<T> { a: bits } }
}";
    // The written implementation of `W<T>` is bounded by `Marker` as well, so
    // it is the one that applies; remove it for this program.
    let without_written = |text: String| {
        let start = text.find("impl<T: Marker> SolPacked for W<T> {").unwrap();
        let end = start + text[start..].find("\n}\n").unwrap() + 3;
        format!("{}{}", &text[..start], &text[end..])
    };
    let layout = Layout::new().ingot("app", &[], &without_written(src(bounded)));
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `SolPacked"),
        "{cause}"
    );
    assert!(cause.contains("how a stored value is packed"), "{cause}");
    let layout = Layout::new().ingot(
        "app",
        &[],
        &without_written(src("pub const fn other() -> u256 { 1 }")),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// A generated function calls a value whose type has a written `FnMut`, and a
/// generated `Fn` implementation exists. Who calls the value does not matter:
/// the generated implementation is refused where it is requested, so no code
/// in the package runs it.
#[test]
fn a_generated_function_cannot_call_through_a_generated_fn_implementation() {
    let src = format!(
        "use core::derive::{{Derive, DeriveInherent}}
use core::meta::{{Record, Code, Item}}
pub struct Double {{}}
{WRITTEN_FN_MUT}
pub struct Gen {{}}
impl DeriveInherent for Gen {{
    const fn derive(_ record: Record) -> Code<Item> {{
        quote {{
            pub fn carrier() -> u256 {{
                {GENERATED_FN}
                0
            }}
            pub const fn helper() -> u256 {{
                var d = Double {{}}
                d(5)
            }}
        }}
    }}
}}
struct G {{}}
impl G with (Gen)
"
    );
    let layout = Layout::new().ingot("app", &[], &src);
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Fn(u256) -> _` for `Double`"),
        "{cause}"
    );
}

// ---- A chain through a dependency, and a call with a closure ------------------

/// The written implementation is in a dependency and the marker is generated
/// in the dependent: the dependent cannot make `lib::W<X>` copy by generating
/// `impl lib::Marker for X`, although no code of the dependency changes.
#[test]
fn a_generated_marker_cannot_make_a_dependencys_copy_implementation_apply() {
    let lib = "pub trait Marker {}
pub struct W<T> { pub v: u256 }
impl<T: Marker> Copy for W<T> {}
";
    let app = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use lib::{Marker, W}
pub struct X {}
pub fn run() -> u256 {
    let w = W<X> { v: 3 }
    let a = w
    let b = w
    a.v + b.v
}
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Marker for X {}
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
";
    let layout = Layout::new()
        .ingot("lib", &[], lib)
        .ingot("app", &[("lib", "lib")], app);
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Marker` for `X`"),
        "{cause}"
    );
    assert!(
        cause.contains("whether a value is copied or moved"),
        "{cause}"
    );
    assert!(cause.contains("for `W<T>`"), "{cause}");
    // Control: the marker written in the dependent is lawful and `W<X>` copies.
    let written = app.replace("impl G with (Gen)\n", "impl Marker for X {}\n");
    let layout = Layout::new()
        .ingot("lib", &[], lib)
        .ingot("app", &[("lib", "lib")], &written);
    assert_eq!(errors(&layout.db(), "app"), "");
}

/// A call with a closure argument has no argument types before the callee is
/// chosen, so the written candidate is kept and the closure is typed from its
/// parameter, as in an ordinary call; the generated candidate (here from a
/// second trait with a method of the same name) is not tried.
#[test]
fn a_generated_method_does_not_replace_a_written_one_for_a_closure_argument() {
    let src = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Tk { fn m<F: Fn(u256) -> u256>(self, _ f: F) -> u256 uses F::E }
pub trait Tk2 { fn m(self, _ f: u8) -> u256 }
pub struct X {}
impl Tk for X {
    fn m<F: Fn(u256) -> u256>(self, _ f: F) -> u256 uses F::E { f(1) }
}
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Tk2 for X { fn m(self, _ f: u8) -> u256 { 2 } }
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
pub fn run() -> u256 {
    let x = X {}
    x.m(|a| a + 1)
}
";
    let layout = Layout::new().ingot("app", &[], src);
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

// ---- The refusal follows the chain of bounds in full ---------------------------
//
// A generated implementation is refused if a written implementation of a
// deciding trait (`#[decides]`) is bounded on its trait, or on a trait that a
// written implementation, chain after chain, bounds on it. No bound is
// skipped for the type it is written for, for the trait that decides, or for
// the visibility of the trait, and the arguments of a bound are never
// compared with those of the generated implementation: every case below
// changes what a written `for` loop, a copy or a call does in the package that
// generates, which a narrower rule would not refuse (each was found as a
// counterexample to a narrower rule during review). Any change that stops
// refusing one of them fails here.
//
// The cases that follow share a shape: a program in which the generated item
// is the whole difference. The generated item is refused at its request; the
// same item written in the package is accepted and the program has no errors,
// so a user can still ask for it.

const CHAIN_HEAD: &str = "use core::meta::{Record, Code, Item}
use core::iter::{Collection, Driver, Identity, Producer}
use core::ops::StateIndex
use std::evm::{StorageKey, StorageMap}
pub trait Marker {}
pub trait Gate {}
pub struct X {}
pub struct P { pub v: u256 }
impl Copy for P {}
";

/// A `Driver` for `$ty` over an array of three, as the lowest link of a chain.
fn array_driver(generics: &str, ty: &str) -> String {
    format!(
        "impl{generics} Driver for {ty} {{
    type Base = [u256; 3]
    type State = usize
    shape Step = mut? u256
    fn start(self, _ base: [u256; 3]) -> usize {{ 0 }}
    fn has(self, _ base: [u256; 3], _ s: usize) -> bool {{ s < 3 }}
    fn next(self, _ base: [u256; 3], _ s: usize) -> usize {{ s + 1 }}
    fn at(self, _ base: mut? [u256; 3], _ s: usize) -> mut? u256 {{ Collection::at(mut? base, s) }}
}}
"
    )
}

/// A `Collection` of two elements of `self.v` for `$ty`.
fn two_elements(generics: &str, ty: &str) -> String {
    format!(
        "impl{generics} Collection for {ty} {{
    type Cursor = usize
    type Item = u256
    fn start(self) -> usize {{ 0 }}
    fn has(self, _ c: usize) -> bool {{ c < 2 }}
    fn next(self, _ c: usize) -> usize {{ c + 1 }}
    fn at(mut? self, _ c: usize) -> mut? u256 {{ mut? self.v }}
}}
"
    )
}

/// The adapter that the chain cases share: `Ad<T>` drives over an array when
/// `T: Marker`, and `Wr<D>` is a collection that drives when its parameter
/// does. A loop over `xs.by(Wr<Ad<X>> { v: 1000 })` sums 2000 without the
/// marker (`Wr` is read as a collection) and 6 with it.
fn wrapped_driver(link: &str) -> String {
    format!(
        "pub struct Ad<T> {{}}
impl<T> Copy for Ad<T> {{}}
{}
pub struct Wr<D> {{ pub v: u256 }}
impl<D> Copy for Wr<D> {{}}
{}
{}
",
        array_driver("<T: Marker>", "Ad<T>"),
        two_elements("<D>", "Wr<D>"),
        array_driver(&format!("<D: {link}>"), "Wr<D>"),
    )
}

fn wrapped_run(adapter: &str) -> String {
    format!(
        "pub fn run() -> u256 {{
    let xs: [u256; 3] = [1, 2, 3]
    var sum = 0
    for x in xs.by({adapter}) {{ sum += x }}
    sum
}}
"
    )
}

/// The source of a case: the shared head, the written items, and the inherent
/// header `impl G with (Gen)` whose generated member writes `generated` in its
/// body.
fn chain_source(written: &str, generated: &str) -> String {
    let carrier = generated_in_member(generated);
    format!(
        "{CHAIN_HEAD}{written}
use core::derive::{{Derive, DeriveInherent}}
{carrier}"
    )
}

/// The generated item is refused at its request, naming it and offering the
/// ways to resolve it; the same item written in the package is accepted
/// without errors.
fn assert_refused_but_writable(written: &str, generated: &str, refusal: &str) {
    let layout = Layout::new().ingot("app", &[], &chain_source(written, generated));
    let cause = refused(&layout, "app");
    assert!(cause.contains(refusal), "{cause}");
    assert!(cause.contains("it could change"), "{cause}");
    assert!(
        cause.contains("write the implementation in the package's own code"),
        "{cause}"
    );
    let layout = Layout::new().ingot("app", &[], &format!("{CHAIN_HEAD}{written}\n{generated}\n"));
    // No request is left, so the package is read as it is written.
    assert_eq!(errors(&layout.db(), "app"), "");
}

/// The link is a bound on the deciding trait itself: `Wr<D>` drives when `D`
/// drives, and `Ad<X>` drives once `X` is a `Marker`.
#[test]
fn a_marker_reaches_a_loop_through_a_bound_on_driver() {
    let written = format!(
        "{}{}",
        wrapped_driver("Driver"),
        wrapped_run("Wr<Ad<X>> { v: 1000 }")
    );
    assert_refused_but_writable(
        &written,
        "impl Marker for X {}",
        "the generated implementation of `Marker` for `X`",
    );
}

/// The link is a bound on a callable trait: `Ad<T>` is an `FnMut` once `T` is
/// a `Marker`, and `Wr<D>` drives when `D` is an `FnMut`.
#[test]
fn a_marker_reaches_a_loop_through_a_bound_on_fn_mut() {
    let written = format!(
        "pub struct Ad<T> {{}}
impl<T> Copy for Ad<T> {{}}
impl<T: Marker> FnMut(u256) -> u256 for Ad<T> {{ fn call_mut(mut self, _ x: u256) -> u256 {{ x }} }}
pub struct Wr<D> {{ pub v: u256 }}
impl<D> Copy for Wr<D> {{}}
{}
{}
{}",
        two_elements("<D>", "Wr<D>"),
        array_driver("<D: FnMut(u256) -> u256>", "Wr<D>"),
        wrapped_run("Wr<Ad<X>> { v: 1000 }"),
    );
    assert_refused_but_writable(
        &written,
        "impl Marker for X {}",
        "the generated implementation of `Marker` for `X`",
    );
}

/// The link is a trait that decides nothing: `Gate` is implemented for every
/// driver, and `Wr<D>` drives when `D: Gate`.
#[test]
fn a_marker_reaches_a_loop_through_a_trait_that_decides_nothing() {
    let written = format!(
        "pub struct Ad<T> {{}}
impl<T> Copy for Ad<T> {{}}
{}
impl<D: Driver> Gate for D {{}}
pub struct Wr<D> {{ pub v: u256 }}
impl<D> Copy for Wr<D> {{}}
{}
{}
{}",
        array_driver("<T: Marker>", "Ad<T>"),
        two_elements("<D>", "Wr<D>"),
        array_driver("<D: Gate>", "Wr<D>"),
        wrapped_run("Wr<Ad<X>> { v: 1000 }"),
    );
    assert_refused_but_writable(
        &written,
        "impl Marker for X {}",
        "the generated implementation of `Marker` for `X`",
    );
}

/// The link is a `Copy` implementation bounded on a driver: without the marker
/// two uses of the value are a move conflict, with it they compile.
#[test]
fn a_marker_reaches_a_copy_through_a_bound_on_driver() {
    let written = format!(
        "pub struct Ad<T> {{}}
impl<T> Copy for Ad<T> {{}}
{}
pub struct Wc<D> {{ pub v: u256 }}
impl<D: Driver> Copy for Wc<D> {{}}
pub fn run() -> u256 {{
    let w = Wc<Ad<X>> {{ v: 3 }}
    let a = w
    let b = w
    a.v + b.v
}}
",
        array_driver("<T: Marker>", "Ad<T>"),
    );
    assert_refused_but_writable(
        &written,
        "impl Marker for X {}",
        "the generated implementation of `Marker` for `X`",
    );
}

/// A type that is a `Producer` over one base and a `Driver` over another: the
/// producer runs unless the marker makes the driver apply, and the driver is
/// found by a nearer receiver.
#[test]
fn a_marker_changes_which_protocol_a_producer_and_a_driver_for_one_type_run() {
    let written = format!(
        "pub struct R {{ pub v: u256 }}
impl Copy for R {{}}
{}
pub struct Ad<T> {{}}
impl<T> Copy for Ad<T> {{}}
impl<T> Producer for Ad<T> {{
    type Base = [u256; 3]
    type State = usize
    type Out = u256
    fn start(self, _ base: [u256; 3]) -> usize {{ 0 }}
    fn has(self, _ base: [u256; 3], _ s: usize) -> bool {{ s < 3 }}
    fn next(self, _ base: [u256; 3], _ s: usize) -> usize {{ s + 1 }}
    fn produce(mut self, _ base: [u256; 3], _ s: usize) -> u256 {{ Collection::at(base, s) * 100 }}
}}
impl<T: Marker> Driver for Ad<T> {{
    type Base = R
    type State = usize
    shape Step = mut? u256
    fn start(self, _ base: R) -> usize {{ 0 }}
    fn has(self, _ base: R, _ s: usize) -> bool {{ s < 2 }}
    fn next(self, _ base: R, _ s: usize) -> usize {{ s + 1 }}
    fn at(self, _ base: mut? R, _ s: usize) -> mut? u256 {{ Collection::at(mut? base, s) }}
}}
{}",
        two_elements("", "R"),
        "pub fn run() -> u256 {
    let xs: [u256; 3] = [1, 2, 3]
    var sum = 0
    for x in xs.by(R { v: 7 }).by(Ad<X> {}) { sum += x }
    sum
}
",
    );
    assert_refused_but_writable(
        &written,
        "impl Marker for X {}",
        "the generated implementation of `Marker` for `X`",
    );
}

/// The same bound on a trait that every `Collection` derive produces: a
/// generated `Collection` for `P` makes the driver of `Ad<B: Collection>`
/// apply to `P` at a farther receiver than the producer of `Ad<B>`.
#[test]
fn a_generated_collection_changes_which_protocol_a_producer_and_a_driver_run() {
    let written = "impl P { pub fn ad(self) -> Ad<P> { Ad<P> {} } }
pub struct Ad<B> {}
impl<B> Copy for Ad<B> {}
impl<B> Producer for Ad<B> {
    type Base = [u256; 3]
    type State = usize
    type Out = u256
    fn start(self, _ base: [u256; 3]) -> usize { 0 }
    fn has(self, _ base: [u256; 3], _ s: usize) -> bool { s < 3 }
    fn next(self, _ base: [u256; 3], _ s: usize) -> usize { s + 1 }
    fn produce(mut self, _ base: [u256; 3], _ s: usize) -> u256 { Collection::at(base, s) * 100 }
}
impl<B: mut? Collection> Driver for Ad<B> {
    type Base = B
    type State = B::Cursor
    shape Step = mut? B::Item
    fn start(self, _ base: B) -> B::Cursor uses B::E { base.start() }
    fn has(self, _ base: B, _ s: B::Cursor) -> bool uses B::E { base.has(s) }
    fn next(self, _ base: B, _ s: B::Cursor) -> B::Cursor uses B::E { base.next(s) }
    fn at(self, _ base: mut? B, _ s: B::Cursor) -> mut? B::Item uses B::E { Collection::at(mut? base, s) }
}
pub fn run() -> u256 {
    let xs: [u256; 3] = [1, 2, 3]
    var sum = 0
    for x in xs.by(P { v: 7 }).ad() { sum += x }
    sum
}
";
    assert_refused_but_writable(
        written,
        &two_elements("", "P"),
        "the generated implementation of `Collection` for `P`",
    );
}

/// A generated `Collection` for a local type reaches the `Driver` that core
/// writes for `Identity<B>`, which a wrapper that drives when its parameter
/// does passes on.
#[test]
fn a_generated_collection_reaches_a_loop_through_the_identity_driver_of_core() {
    let written = format!(
        "pub struct Wr<D> {{ pub v: u256 }}
impl<D> Copy for Wr<D> {{}}
{}
{}
{}",
        two_elements("<D>", "Wr<D>"),
        array_driver("<D: Driver>", "Wr<D>"),
        wrapped_run("Wr<Identity<P>> { v: 1000 }"),
    );
    assert_refused_but_writable(
        &written,
        &two_elements("", "P"),
        "the generated implementation of `Collection` for `P`",
    );
}

/// A generated `StorageKey` for the key type of a stored map reaches the
/// `StateIndex` that std writes for `StorageMap<K, V>`, which a wrapper that
/// drives when its parameter is a `StateIndex` passes on.
#[test]
fn a_generated_storage_key_reaches_a_loop_through_the_state_index_of_std() {
    let written = format!(
        "pub struct Wr<D> {{ pub v: u256 }}
impl<D> Copy for Wr<D> {{}}
{}
{}
{}",
        two_elements("<D>", "Wr<D>"),
        array_driver("<D: StateIndex>", "Wr<D>"),
        wrapped_run("Wr<StorageMap<P, u256>> { v: 1000 }"),
    );
    let generated = "impl StorageKey for P {
    fn encoded_len(self) -> u256 { 32 }
    unsafe fn write_key(ptr: *u8, self) { u256::write_key(ptr: ptr, self.v) }
}";
    assert_refused_but_writable(
        &written,
        generated,
        "the generated implementation of `StorageKey` for `P`",
    );
}

/// The bound is on a trait that the dependency does not export, and the
/// dependent's provider emits its implementation: a quote resolves its names
/// where the provider is declared, so the generating package implements a
/// trait it cannot name.
#[test]
fn a_provider_of_a_dependency_implements_the_private_trait_of_a_bound() {
    let lib = format!(
        "use core::derive::{{Derive, DeriveInherent}}
use core::meta::{{Record, Code, Item}}
use core::iter::{{Collection, Driver}}
trait Sealed {{}}
pub struct Ad<T> {{ pub v: u256 }}
impl<T> Copy for Ad<T> {{}}
{}
{}
pub struct Seal {{}}
impl DeriveInherent for Seal {{
    const fn derive(_ record: Record) -> Code<Item> {{
        quote {{
            pub fn carrier() -> u256 {{
                impl Sealed for ${{record}} {{}}
                0
            }}
        }}
    }}
}}
",
        two_elements("<T>", "Ad<T>"),
        array_driver("<T: Sealed>", "Ad<T>"),
    );
    let app = "use lib::{Ad, Seal}
pub struct X {}
impl X with (Seal)
pub fn run() -> u256 {
    let xs: [u256; 3] = [1, 2, 3]
    var sum = 0
    for x in xs.by(Ad<X> { v: 1000 }) { sum += x }
    sum
}
";
    let layout = Layout::new()
        .ingot("lib", &[], &lib)
        .ingot("app", &[("lib", "lib")], app);
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Sealed` for `X`"),
        "{cause}"
    );
    assert!(
        cause.contains("which protocol a `for` loop runs"),
        "{cause}"
    );
    // Control: without the request the loop reads `Ad<X>` as a collection.
    let plain = app.replace("impl X with (Seal)\n", "");
    let layout = Layout::new()
        .ingot("lib", &[], &lib)
        .ingot("app", &[("lib", "lib")], &plain);
    assert_eq!(errors(&layout.db(), "app"), "");
}

/// The trait is declared without `pub` and exported by `pub use`: the
/// dependent names it and implements it in its own provider.
#[test]
fn a_reexported_trait_of_a_bound_is_implemented_by_the_dependent() {
    let lib = format!(
        "use core::iter::{{Collection, Driver}}
trait Sealed {{}}
pub use Sealed as Open
pub struct Ad<T> {{ pub v: u256 }}
impl<T> Copy for Ad<T> {{}}
{}
{}",
        two_elements("<T>", "Ad<T>"),
        array_driver("<T: Sealed>", "Ad<T>"),
    );
    let app = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use lib::{Ad, Open}
pub struct X {}
pub fn run() -> u256 {
    let xs: [u256; 3] = [1, 2, 3]
    var sum = 0
    for x in xs.by(Ad<X> { v: 1000 }) { sum += x }
    sum
}
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Open for X {}
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
";
    let layout = Layout::new()
        .ingot("lib", &[], &lib)
        .ingot("app", &[("lib", "lib")], app);
    let cause = refused(&layout, "app");
    assert!(
        cause.contains("the generated implementation of `Sealed` for `X`")
            || cause.contains("the generated implementation of `Open` for `X`"),
        "{cause}"
    );
    assert!(
        cause.contains("which protocol a `for` loop runs"),
        "{cause}"
    );
    // The dependent can write the implementation itself.
    let written = app.replace("impl G with (Gen)\n", "impl Open for X {}\n");
    let layout = Layout::new()
        .ingot("lib", &[], &lib)
        .ingot("app", &[("lib", "lib")], &written);
    assert_eq!(errors(&layout.db(), "app"), "");
}

/// Whether the type has the alternative of the deciding trait does not matter:
/// a generated marker that a written `Driver`, `Producer` or `Fn` is bounded
/// on is refused for a type that has no other protocol (where the marker would
/// only turn an error into a working program) as for one that has.
#[test]
fn a_marker_a_written_choice_is_bounded_on_is_refused_without_an_alternative() {
    let driver = format!(
        "pub struct Ad<T> {{}}
impl<T> Copy for Ad<T> {{}}
{}",
        array_driver("<T: Marker>", "Ad<T>")
    );
    let producer = "pub struct Ad<T> {}
impl<T> Copy for Ad<T> {}
impl<T: Marker> Producer for Ad<T> {
    type Base = [u256; 3]
    type State = usize
    type Out = u256
    fn start(self, _ base: [u256; 3]) -> usize { 0 }
    fn has(self, _ base: [u256; 3], _ s: usize) -> bool { s < 3 }
    fn next(self, _ base: [u256; 3], _ s: usize) -> usize { s + 1 }
    fn produce(mut self, _ base: [u256; 3], _ s: usize) -> u256 { Collection::at(base, s) * 100 }
}
";
    let call = "pub struct W<T> {}
impl<T: Marker> Fn(u256) -> u256 for W<T> { fn call(self, _ x: u256) -> u256 { x * 3 } }
";
    for (written, deciding) in [
        (driver.as_str(), "which protocol a `for` loop runs"),
        (producer, "which protocol a `for` loop runs"),
        (call, "which implementation a call of a value runs"),
    ] {
        let layout =
            Layout::new().ingot("app", &[], &chain_source(written, "impl Marker for X {}"));
        let cause = refused(&layout, "app");
        assert!(
            cause.contains("the generated implementation of `Marker` for `X`"),
            "{cause}"
        );
        assert!(cause.contains(deciding), "{cause}");
    }
}

/// A bound on a trait that the package of the written implementation does not
/// export is followed like any other, whatever the trait is implemented for:
/// the generating package can reach such a trait through a re-export or a
/// provider of the dependency, so no reading of the declaration says that it
/// cannot be implemented.
#[test]
fn a_bound_on_a_trait_a_dependency_does_not_export_is_followed_like_any_other() {
    let lib = |extra: &str| {
        format!(
            "pub trait Marker {{}}
trait Sealed {{}}
{extra}
pub struct W<T> {{ pub v: u256 }}
impl<T: Marker + Sealed> Copy for W<T> {{}}
"
        )
    };
    let app = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
use lib::Marker
pub struct X {}
pub struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Marker for X {}
                0
            }
        }
    }
}
struct G {}
impl G with (Gen)
";
    let copy = "whether a value is copied or moved";
    // `Sealed` is implemented for a fixed type only, or for every type.
    for extra in ["impl Sealed for u8 {}", "impl<T> Sealed for T {}"] {
        let layout =
            Layout::new()
                .ingot("lib", &[], &lib(extra))
                .ingot("app", &[("lib", "lib")], app);
        let cause = refused(&layout, "app");
        assert!(
            cause.contains("the generated implementation of `Marker` for `X`"),
            "{cause}"
        );
        assert!(cause.contains(copy), "{cause}");
    }
    // On another parameter of the implementation, and on an associated type.
    let other_parameter = "pub trait Marker {}
trait Sealed {}
impl Sealed for u8 {}
pub struct W<T, U> { pub v: u256 }
impl<T: Marker, U: Sealed> Copy for W<T, U> {}
";
    let projection = "pub trait Marker {}
pub trait HasAssoc { type Assoc }
trait Sealed {}
impl Sealed for u8 {}
pub struct W<T> { pub v: u256 }
impl<T: Marker + HasAssoc> Copy for W<T> where T::Assoc: Sealed {}
";
    let projected_app = app
        .replace("use lib::Marker", "use lib::{HasAssoc, Marker}")
        .replace(
            "pub struct X {}",
            "pub struct X {}\nimpl HasAssoc for X { type Assoc = u8 }",
        );
    for (dependency, source) in [(other_parameter, app), (projection, projected_app.as_str())] {
        let layout =
            Layout::new()
                .ingot("lib", &[], dependency)
                .ingot("app", &[("lib", "lib")], source);
        let cause = refused(&layout, "app");
        assert!(
            cause.contains("the generated implementation of `Marker` for `X`"),
            "{cause}"
        );
        assert!(cause.contains(copy), "{cause}");
    }
}

/// The traits that derives of collections, codecs and storage keys produce are
/// reached by implementations of core and std that deciding traits carry, so
/// each is refused as a generated implementation and offers the header, which
/// counts as written (see `complete_member_derive` for each header).
#[test]
fn a_generated_collection_codec_or_storage_key_is_refused_and_offers_the_header() {
    const HEAD: &str = "use core::meta::{Record, Code, Item}\npub struct P { pub v: u256 }\n";
    const COLLECTION: &str = "impl Collection for P {
    type Cursor = usize
    type Item = u256
    fn start(self) -> usize { 0 }
    fn has(self, _ c: usize) -> bool { c < 3 }
    fn next(self, _ c: usize) -> usize { c + 1 }
    fn at(mut? self, _ c: usize) -> mut? u256 { mut? self.v }
}";
    const ABI_SIZE: &str = "impl AbiSize for P {
    const HEAD_SIZE: u256 = 32
    const IS_DYNAMIC: bool = false
}";
    let cases = [
        (
            "Collection",
            "use core::iter::Collection\n",
            String::new(),
            COLLECTION.to_string(),
        ),
        (
            "Bidirectional",
            "use core::iter::{Collection, Bidirectional}\n",
            COLLECTION.to_string(),
            "impl Bidirectional for P {
    fn last(self) -> usize { 2 }
    fn prev(self, _ c: usize) -> usize { c.wrapping_sub(1) }
}"
            .to_string(),
        ),
        (
            "AbiSize",
            "use core::abi::AbiSize\n",
            String::new(),
            ABI_SIZE.to_string(),
        ),
        (
            "Decode<Sol>",
            "use core::abi::{AbiSize, Decode, AbiDecoder, ByteInput}\nuse std::abi::Sol\n",
            ABI_SIZE.to_string(),
            "impl Decode<Sol> for P {
    fn decode_payload<D: AbiDecoder<Sol>, I: ByteInput>(_ d: mut D, _ input: I) -> Self {
        let _ = input
        P { v: 0 }
    }
}"
            .to_string(),
        ),
        (
            "AbiSpan<Sol>",
            "use core::abi::{AbiSize, AbiSpan, ByteInput}\nuse std::abi::Sol\n",
            ABI_SIZE.to_string(),
            "impl AbiSpan<Sol> for P {
    fn payload_end<I: ByteInput>(_: I, base: u256, pos: u256) -> u256 { pos + 32 }
}"
            .to_string(),
        ),
        (
            "StorageKey",
            "use std::evm::StorageKey\n",
            String::new(),
            "impl StorageKey for P {
    fn encoded_len(self) -> u256 { 32 }
    unsafe fn write_key(ptr: *u8, self) { let _ = ptr }
}"
            .to_string(),
        ),
        (
            "SolWordRepr",
            "use std::abi::sol::SolWordRepr\n",
            String::new(),
            "impl SolWordRepr for P {
    const BITS: u256 = 8
    fn canonical_word(self) -> u256 { self.v }
}"
            .to_string(),
        ),
        (
            "LaneCodec<u8>",
            "use core::ops::LaneCodec\n",
            String::new(),
            "impl LaneCodec<u8> for P {
    const BITS: u256 = 8
    fn get(_ word: u256, offset: u256) -> u8 { 0 }
    fn set(_ word: u256, offset: u256, value: own u8) -> u256 { word }
}"
            .to_string(),
        ),
    ];
    for (name, imports, written, generated) in cases {
        let carrier = generated_in_member(&generated);
        let src = format!(
            "{imports}{HEAD}{written}
use core::derive::{{Derive, DeriveInherent}}
{carrier}"
        );
        let layout = Layout::new().ingot("app", &[], &src);
        let cause = refused(&layout, "app");
        assert!(
            cause.contains(&format!("the generated implementation of `{name}` for `P`")),
            "{name}: {cause}"
        );
        assert!(
            cause.contains(&format!(
                "request it with a header `impl {name} for Type with (Provider)`"
            )),
            "{name}: {cause}"
        );
    }
}

/// A struct that `#[abi]` expands takes a header like any other struct: a
/// generated `Copy` for it is refused and offers the header, and the header is
/// accepted and makes the value copyable.
#[test]
fn a_generated_copy_for_an_abi_struct_is_refused_and_its_header_is_accepted() {
    const HEAD: &str = "use core::derive::Derive
use core::marker::Copy
use core::meta::{Code, Item, Record}
#[abi]
pub struct P { pub v: u256 }
";
    let carrier = generated_in_member("impl Copy for P {}");
    let generated = format!(
        "{HEAD}use core::derive::DeriveInherent
{carrier}"
    );
    let cause = refused(&Layout::new().ingot("app", &[], &generated), "app");
    assert!(
        cause.contains("the generated implementation of `Copy` for `P`"),
        "{cause}"
    );
    assert!(
        cause.contains("request it with a header `impl Copy for Type with (Provider)`"),
        "{cause}"
    );
    let header = format!(
        "{HEAD}struct Marks {{}}
impl Derive<trait Copy> for Marks {{
    const fn derive(_ record: Record) -> Code<Item> {{ quote {{}} }}
}}
impl Copy for P with (Marks)
const fn twice(_ p: own P) -> u256 {{
    let a = p
    let b = p
    a.v + b.v
}}
const fn run() -> u256 {{ twice(P {{ v: 3 }}) }}
"
    );
    let prepared = accepted(&Layout::new().ingot("app", &[], &header), "app");
    assert_eq!(value(&prepared, "app", "run"), "6");
    assert_eq!(errors(&prepared, "app"), "");
}

// ---- Every trait that a bound names is followed ------------------------------------
//
// The refusal goes by trait alone. A trait is followed if a bound of a written
// implementation of a deciding trait names it anywhere: as the trait of the
// bound, inside a projection of the bound (`<T as HasA>::A: Gate`), or inside
// a const expression argument, and so on through the bounds of written
// implementations of those. The cases below each change which protocol a
// written `for` loop runs through a trait that is not the trait of any bound;
// a rule that looked only at the trait of a bound, or compared the arguments
// of a bound with those of the generated implementation, accepted them.

/// An adapter `Ad<T>` that reads as a collection (a loop sums 2000) and drives
/// over an array (a loop sums 6) when `bound` holds, with the items that the
/// cases below share.
fn projected_driver(driver_header: &str, extra: &str) -> String {
    format!(
        "pub trait HasA {{ type A }}
pub trait HasB {{ type B }}
pub struct Y {{}}
pub struct Z {{}}
impl Gate for u8 {{}}
pub struct Ad<T> {{ pub v: u256 }}
impl<T> Copy for Ad<T> {{}}
{}
{}
{extra}
{}",
        two_elements("<T>", "Ad<T>"),
        array_driver_where(driver_header),
        wrapped_run("Ad<Y> { v: 1000 }"),
    )
}

/// A `Driver` for `Ad<T>` whose header (generics, `Driver for` and a `where`
/// clause) is `header`.
fn array_driver_where(header: &str) -> String {
    array_driver("<T>", "Ad<T>").replacen("impl<T> Driver for Ad<T>", header, 1)
}

/// The trait of a projection that a bound names is followed: `HasA` is not the
/// trait of any bound, but a generated implementation of it decides what
/// `<T as HasA>::A` is and so whether the bound on `Gate` holds. Written as a
/// qualified projection, with no bound on its trait.
#[test]
fn a_trait_named_inside_a_qualified_projection_of_a_bound_is_followed() {
    assert_refused_but_writable(
        &projected_driver("impl<T> Driver for Ad<T> where <T as HasA>::A: Gate", ""),
        "impl HasA for Y { type A = u8 }",
        "the generated implementation of `HasA` for `Y`",
    );
}

/// The same with the shorthand `T::A`, beside a bound on the trait.
#[test]
fn a_trait_named_inside_a_projection_of_a_bound_is_followed_in_the_shorthand() {
    assert_refused_but_writable(
        &projected_driver(
            "impl<T: HasA> Driver for Ad<T> where T::A: Gate",
            "impl HasB for Z { type B = u8 }",
        ),
        "impl HasA for Y { type A = u8 }",
        "the generated implementation of `HasA` for `Y`",
    );
}

/// A projection of a projection: the trait of the inner one is generated.
#[test]
fn a_trait_named_inside_a_nested_projection_of_a_bound_is_followed() {
    assert_refused_but_writable(
        &projected_driver(
            "impl<T: HasA> Driver for Ad<T> where <T::A as HasB>::B: Gate",
            "impl HasA for Y { type A = Z }",
        ),
        "impl HasB for Z { type B = u8 }",
        "the generated implementation of `HasB` for `Z`",
    );
}

/// Through another written implementation: `Gate2` holds when the projection
/// meets `Gate`, and the driver is bounded on `Gate2`.
#[test]
fn a_trait_named_inside_a_projection_is_followed_through_another_implementation() {
    assert_refused_but_writable(
        &projected_driver(
            "impl<T: Gate2> Driver for Ad<T>",
            "pub trait Gate2 {}\nimpl<T> Gate2 for T where <T as HasA>::A: Gate {}",
        ),
        "impl HasA for Y { type A = u8 }",
        "the generated implementation of `HasA` for `Y`",
    );
}

/// In the arguments of a trait that the bound is on.
#[test]
fn a_trait_named_inside_the_arguments_of_a_bound_is_followed() {
    assert_refused_but_writable(
        &projected_driver(
            "impl<T> Driver for Ad<T> where u8: Conv<<T as HasA>::A>",
            "pub trait Conv<U> {}\nimpl Conv<u8> for u8 {}",
        ),
        "impl HasA for Y { type A = u8 }",
        "the generated implementation of `HasA` for `Y`",
    );
}

/// A trait that no bound names is still accepted.
#[test]
fn a_trait_that_no_bound_names_is_still_accepted_beside_a_projection() {
    let written = projected_driver("impl<T> Driver for Ad<T> where <T as HasA>::A: Gate", "");
    let layout = Layout::new().ingot(
        "app",
        &[],
        &chain_source(
            &format!("{written}\npub trait Unrelated {{}}"),
            "impl Unrelated for Y {}",
        ),
    );
    let prepared = accepted(&layout, "app");
    assert_eq!(errors(&prepared, "app"), "");
}

/// A driver over `Ad<const N: usize>` with the bound `Chk<{ N + 1 }>: Okay`.
fn arithmetic_driver() -> String {
    format!(
        "pub trait Okay {{}}
pub struct Chk<const N: usize> {{}}
pub struct Ad<const N: usize> {{ pub v: u256 }}
impl<const N: usize> Copy for Ad<N> {{}}
{}
{}
{}",
        two_elements("<const N: usize>", "Ad<N>"),
        array_driver("<const N: usize>", "Ad<N> where Chk<{ N + 1 }>: Okay"),
        wrapped_run("Ad<3> { v: 1000 }"),
    )
}

/// A bound whose arguments are const expressions is followed by its trait,
/// whatever the expression evaluates to: `Chk<{ N + 1 }>` is not `Chk<4>` to
/// the unifier.
#[test]
fn a_bound_with_a_const_expression_argument_is_followed() {
    assert_refused_but_writable(
        &arithmetic_driver(),
        "impl Okay for Chk<4> {}",
        "the generated implementation of `Okay` for `Chk<4>`",
    );
}

/// The bound names a type other than the one that the generated implementation
/// is for; only the trait is compared.
#[test]
fn a_bound_is_followed_by_its_trait_when_the_generated_arguments_differ() {
    assert_refused_but_writable(
        &arithmetic_driver(),
        "impl Okay for Chk<9> {}",
        "the generated implementation of `Okay` for `Chk<9>`",
    );
}

/// A trait const as the argument: a value that only an implementation fixes.
#[test]
fn a_bound_with_a_trait_const_argument_is_followed() {
    let written = format!(
        "pub trait Okay {{}}
pub trait Tr {{ const N: usize }}
pub struct Chk<const N: usize> {{}}
pub struct Y {{}}
impl Tr for Y {{ const N: usize = 4 }}
pub struct Ad<T> {{ pub v: u256 }}
impl<T> Copy for Ad<T> {{}}
{}
{}
{}",
        two_elements("<T>", "Ad<T>"),
        array_driver("<T: Tr>", "Ad<T> where Chk<{ <T as Tr>::N }>: Okay"),
        wrapped_run("Ad<Y> { v: 1000 }"),
    );
    assert_refused_but_writable(
        &written,
        "impl Okay for Chk<4> {}",
        "the generated implementation of `Okay` for `Chk<4>`",
    );
}

// ---- Traits named inside a const expression of a bound ----
//
// A bound with a const expression that names no parameter is not an error when
// it cannot be met: `Chk<{ <Y as Tr>::N }>: Okay` is false while nothing
// implements `Tr` for `Y`, and the written implementation it bounds then does
// not apply. An implementation of `Tr` for `Y` makes the bound true, so the
// trait counts as named by the bound wherever the expression uses it, however
// the expression would evaluate.

/// A written `LaneCodec<u8>` whose bound is `bound: Okay`, used by a written
/// `StateIndex` that packs its entries with it. `decl`, `generics` and `args`
/// give `CellLanes` a parameter list, and `lanes` is the `Lanes` type.
fn const_bound_lanes(pre: &str, bound: &str, params: (&str, &str, &str), lanes: &str) -> String {
    let (decl, generics, args) = params;
    format!(
        "use core::marker::{{Pinned, Storage}}
use core::ops::{{LaneCodec, Location}}
pub trait Okay {{}}
pub trait Tr {{ const N: usize }}
pub struct Chk<const N: usize> {{}}
impl Okay for Chk<4> {{}}
pub struct Y {{}}
{pre}
pub struct CellLanes{decl} {{}}
impl{generics} LaneCodec<u8> for CellLanes{args} where {bound}: Okay {{
    const BITS: u256 = 8
    fn get(_ word: u256, offset: u256) -> u8 {{ 77 }}
    fn set(_ word: u256, offset: u256, value: own u8) -> u256 {{ word }}
}}
pub struct Cells {{ len: u256 }}
impl Pinned for Cells {{ type Space = Storage }}
unsafe impl StateIndex for Cells {{
    type Key = u256
    type Entry = u8
    type Lanes = {lanes}
    fn locate(self, _ root: u256, _ key: u256) -> Location {{
        Location {{ slot: root + 100 + key, offset: 0 }}
    }}
}}
"
    )
}

const TR_FOR_Y: &str = "impl Tr for Y { const N: usize = 4 }";
const TR_REFUSED: &str = "the generated implementation of `Tr` for `Y`";

/// The bound is on a concrete const expression: the package prepares with no
/// item, the item is refused, and the same item written in the package is
/// accepted.
fn assert_const_bound_refused(pre: &str, bound: &str, generated: &str, refusal: &str) {
    let written = const_bound_lanes(pre, bound, ("", "", ""), "CellLanes");
    assert_refused_but_writable(&written, generated, refusal);
}

/// The same without the item, and with an item that nothing names.
fn assert_const_bound_controls(pre: &str, bound: &str) {
    let written = const_bound_lanes(pre, bound, ("", "", ""), "CellLanes");
    let layout = Layout::new().ingot("app", &[], &format!("{CHAIN_HEAD}{written}\n"));
    assert_eq!(errors(&layout.db(), "app"), "");
    let layout = Layout::new().ingot("app", &[], &chain_source(&written, "impl Marker for X {}"));
    assert_eq!(errors(&accepted(&layout, "app"), "app"), "");
}

/// A trait const in a bound of a written `LaneCodec`, and nothing else names
/// the trait: a generated implementation makes the bound true and changes how
/// a stored `u8` is read.
#[test]
fn a_trait_named_only_in_a_concrete_const_expression_of_a_bound_is_followed() {
    let bound = "Chk<{ <Y as Tr>::N }>";
    assert_const_bound_refused("", bound, TR_FOR_Y, TR_REFUSED);
    assert_const_bound_controls("", bound);
}

/// The expression calls a const function that is bounded on the trait.
#[test]
fn a_trait_bounding_a_const_function_called_in_a_bound_is_followed() {
    let pre = "pub const fn nof<T: Tr>() -> usize { T::N }";
    let bound = "Chk<{ nof<Y>() }>";
    assert_const_bound_refused(pre, bound, TR_FOR_Y, TR_REFUSED);
    assert_const_bound_controls(pre, bound);
}

/// The trait is named in the body of a const function that the expression
/// calls, and in the body of a function that function calls.
#[test]
fn a_trait_named_in_the_body_of_a_called_const_function_is_followed() {
    let pre = "pub const fn nofy() -> usize { <Y as Tr>::N }
pub const fn outer() -> usize { nofy() + 0 }";
    assert_const_bound_refused(pre, "Chk<{ nofy() }>", TR_FOR_Y, TR_REFUSED);
    assert_const_bound_refused(pre, "Chk<{ outer() }>", TR_FOR_Y, TR_REFUSED);
}

/// A trait constant named as a pattern, as in `match 4 { Y::N => .. }`, is
/// read from the body of a const function that the bound calls, as the same
/// constant is read when it is named in an expression.
#[test]
fn a_trait_const_used_as_a_pattern_in_a_bound_is_followed() {
    assert_const_bound_refused(
        "pub const fn nofy() -> usize { match 4 { Y::N => 4, _ => 9 } }",
        "Chk<{ nofy() }>",
        TR_FOR_Y,
        TR_REFUSED,
    );
}

/// Other ways to write the use: a path through the type, arithmetic around
/// it, and a condition.
#[test]
fn a_trait_const_used_by_path_arithmetic_or_condition_in_a_bound_is_followed() {
    for bound in [
        "Chk<{ Y::N }>",
        "Chk<{ Tr::N }>",
        "Chk<{ <Y as Tr>::N + 0 }>",
        "Chk<{ if <Y as Tr>::N > 3 { 4 } else { 9 } }>",
    ] {
        assert_const_bound_refused("", bound, TR_FOR_Y, TR_REFUSED);
    }
}

/// The condition of a `where { ... }` predicate is a const expression of the
/// bounds too. A condition that cannot be checked is an error without the
/// item, so the generated item fills a gap; it is refused like the other
/// forms, whether the use is a path through the trait, through the type or
/// a call.
#[test]
fn a_trait_named_in_a_const_where_condition_is_followed() {
    for (pre, condition, generated, refusal) in [
        ("", "{ <Y as Tr>::N == 4 }", TR_FOR_Y, TR_REFUSED),
        ("", "{ Y::N == 4 }", TR_FOR_Y, TR_REFUSED),
        (
            "pub trait Cnt { const fn count() -> usize }",
            "{ Y::count() == 4 }",
            "impl Cnt for Y { const fn count() -> usize { 4 } }",
            "the generated implementation of `Cnt` for `Y`",
        ),
    ] {
        let written = const_bound_lanes(pre, condition, ("", "", ""), "CellLanes")
            .replace(&format!("{condition}: Okay"), condition);
        assert_refused_but_writable(&written, generated, refusal);
    }
}

/// A method called with method syntax, and an operator on a type of the
/// package: neither names its trait in the source.
#[test]
fn a_method_or_operator_of_a_trait_in_a_const_expression_of_a_bound_is_followed() {
    assert_const_bound_refused(
        "pub trait Cnt2 { fn count(self) -> usize }",
        "Chk<{ Y {}.count() }>",
        "impl Cnt2 for Y { fn count(self) -> usize { 4 } }",
        "the generated implementation of `Cnt2` for `Y`",
    );
    assert_const_bound_refused(
        "",
        "Chk<{ if (Y {}) == (Y {}) { 4 } else { 9 } }>",
        "impl core::ops::Eq for Y { fn eq(self, _ other: Y) -> bool { true } }",
        "the generated implementation of `Eq<Y>` for `Y`",
    );
}

/// A method of the trait, called by path.
#[test]
fn a_trait_method_called_in_a_const_expression_of_a_bound_is_followed() {
    let pre = "pub trait Cnt { const fn count() -> usize }";
    assert_const_bound_refused(
        pre,
        "Chk<{ <Y as Cnt>::count() }>",
        "impl Cnt for Y { const fn count() -> usize { 4 } }",
        "the generated implementation of `Cnt` for `Y`",
    );
}

/// A trait named only by a projection written as a generic argument of the
/// call, or as the type of a `let`. Inference normalizes the projection to
/// the type it selects once an implementation exists.
#[test]
fn a_trait_named_by_a_projection_inside_a_const_expression_is_followed() {
    let pre = "pub trait HasA { type A }
pub trait Wd { const W: usize }
impl Wd for u256 { const W: usize = 4 }
pub const fn width<T: Wd>() -> usize { T::W }";
    for bound in [
        "Chk<{ width<<Y as HasA>::A>() }>",
        "Chk<{ width<Y::A>() }>",
        "Chk<{ <<Y as HasA>::A as Wd>::W }>",
        "Chk<{ let x: <Y as HasA>::A = 0
4 }>",
    ] {
        assert_const_bound_refused(
            pre,
            bound,
            "impl HasA for Y { type A = u256 }",
            "the generated implementation of `HasA` for `Y`",
        );
    }
}

/// A constant of an inherent implementation that applies only when a trait is
/// implemented.
#[test]
fn a_trait_bounding_an_inherent_constant_used_in_a_bound_is_followed() {
    let pre = "pub struct W<T> {}
impl<T: Tr> W<T> { pub const M: usize = 4 }";
    assert_const_bound_refused(pre, "Chk<{ W<Y>::M }>", TR_FOR_Y, TR_REFUSED);
}

/// The expression is an argument of a trait, and is behind a type alias.
#[test]
fn a_trait_named_in_a_const_trait_argument_or_alias_in_a_bound_is_followed() {
    let written = const_bound_lanes(
        "pub trait Cnv<const N: usize> {}
impl Cnv<4> for Y {}",
        "Y: Cnv<{ <Y as Tr>::N }>",
        ("", "", ""),
        "CellLanes",
    )
    .replace("Y: Cnv<{ <Y as Tr>::N }>: Okay", "Y: Cnv<{ <Y as Tr>::N }>");
    assert_refused_but_writable(&written, TR_FOR_Y, TR_REFUSED);
    assert_const_bound_refused(
        "pub type A = Chk<{ <Y as Tr>::N }>",
        "A",
        TR_FOR_Y,
        TR_REFUSED,
    );
}

/// An expression that also names a parameter of the implementation, whether
/// the parameter is a const or a type.
#[test]
fn a_trait_named_in_a_const_expression_that_mixes_in_a_parameter_is_followed() {
    for (pre, bound) in [
        ("", "Chk<{ <Y as Tr>::N + M }>"),
        (
            "pub const fn nof<T: Tr>() -> usize { T::N }",
            "Chk<{ nof<Y>() + M }>",
        ),
    ] {
        let written = const_bound_lanes(
            pre,
            bound,
            ("<const M: usize>", "<const M: usize>", "<M>"),
            "CellLanes<0>",
        );
        assert_refused_but_writable(&written, TR_FOR_Y, TR_REFUSED);
    }
    let written = const_bound_lanes(
        "",
        "Chk<{ <Y as Tr>::N }>",
        ("<T>", "<T>", "<T>"),
        "CellLanes<u8>",
    );
    assert_refused_but_writable(&written, TR_FOR_Y, TR_REFUSED);
}

/// A projection written as `Y::A` for a concrete `Y` lowers to the type that
/// the selected implementation gives `A`, with no mention of the trait. While
/// no implementation of `HasA` for `Y` exists the bound `Y::A: Okay` holds
/// vacuously; the generated one makes it false, because `Zn` is not `Okay`.
#[test]
fn a_shorthand_projection_on_a_concrete_type_in_a_bound_is_followed() {
    let pre = "pub trait HasA { type A }
pub struct Zn {}";
    assert_const_bound_refused(
        pre,
        "Y::A",
        "impl HasA for Y { type A = Zn }",
        "the generated implementation of `HasA` for `Y`",
    );
    assert_const_bound_controls(pre, "Y::A");
}

/// Integer arithmetic in a const expression is a use of the operator traits of
/// core at primitive types. No generated implementation can extend those (a
/// trait of core for types of core is an error in another package), so they
/// are not named, and a typical derive of `Add` is still accepted.
#[test]
fn integer_arithmetic_in_a_const_expression_of_a_bound_names_no_trait() {
    let written = format!("use core::ops::Add\n{}", arithmetic_driver());
    let generated =
        "impl Add for P { fn add(own self, _ other: own P) -> P { P { v: self.v + other.v } } }";
    let layout = Layout::new().ingot("app", &[], &chain_source(&written, generated));
    assert_eq!(errors(&accepted(&layout, "app"), "app"), "");
}

/// A dependency writes the bound, and the package that depends on it
/// generates the implementation of the trait for a type of its own.
#[test]
fn a_trait_named_in_a_const_expression_of_a_dependencys_bound_is_followed() {
    let lib = "use core::ops::LaneCodec
pub trait Okay {}
pub trait Tr { const N: usize }
pub struct Chk<const N: usize> {}
impl Okay for Chk<4> {}
pub struct CellLanes<T> {}
impl<T> LaneCodec<u8> for CellLanes<T> where Chk<{ <T as Tr>::N }>: Okay {
    const BITS: u256 = 8
    fn get(_ word: u256, offset: u256) -> u8 { 77 }
    fn set(_ word: u256, offset: u256, value: own u8) -> u256 { word }
}
";
    let app = |item: &str| {
        let carrier = generated_in_member(item);
        format!(
            "use core::derive::{{Derive, DeriveInherent}}
use core::meta::{{Record, Code, Item}}
use lib::Tr
pub trait Other {{}}
pub struct Mine {{}}
{carrier}"
        )
    };
    let layout = Layout::new().ingot("lib", &[], lib).ingot(
        "app",
        &[("lib", "lib")],
        &app("impl Tr for Mine { const N: usize = 4 }"),
    );
    let cause = refused(&layout, "app");
    assert!(
        cause.contains(TR_REFUSED.replace("`Y`", "`Mine`").as_str()),
        "{cause}"
    );
    assert!(cause.contains("it could change"), "{cause}");
    let layout = Layout::new().ingot("lib", &[], lib).ingot(
        "app",
        &[("lib", "lib")],
        &app("impl Other for Mine {}"),
    );
    let _ = accepted(&layout, "app");
}

// ---- The message names the implementation that brought the trait in ----

/// A library writes `impl<T: Eq> Collection for Bag<T>`. Core's `Driver` for
/// `Identity<B>` names `Collection`, so `Eq` joins the traits that a dependent
/// cannot generate. The refusal names the library's implementation and its
/// bound, then the deciding implementation of core that reaches it, and keeps
/// the remedy.
#[test]
fn the_refusal_names_the_implementation_of_a_dependency_that_names_the_trait() {
    let lib = "use core::iter::Collection
use core::ops::Eq
pub struct Bag<T> { pub v: u256 }
impl<T> Copy for Bag<T> {}
impl<T: Eq> Collection for Bag<T> {
    type Cursor = usize
    type Item = u256
    fn start(self) -> usize { 0 }
    fn has(self, _ c: usize) -> bool { c < 2 }
    fn next(self, _ c: usize) -> usize { c + 1 }
    fn at(mut? self, _ c: usize) -> mut? u256 { mut? self.v }
}
";
    let app = |item: &str| {
        let carrier = generated_in_member(item);
        format!(
            "use core::derive::{{Derive, DeriveInherent}}
use core::meta::{{Record, Code, Item}}
use core::ops::Eq
{carrier}pub struct P {{ pub v: u256 }}
"
        )
    };
    let eq = "impl Eq for P { fn eq(self, _ other: P) -> bool { self.v == other.v } }";
    let layout = Layout::new()
        .ingot("lib", &[], lib)
        .ingot("app", &[("lib", "lib")], &app(eq));
    let cause = refused(&layout, "app");
    for part in [
        "the generated implementation of `Eq<P>` for `P`",
        "it could change which protocol a `for` loop runs",
        "The written implementation of `Collection` for `Bag<T>` in package `lib` has the bound `T: Eq`, which names `Eq`.",
        "`Collection` is named by a bound of the written implementation of `Driver` for `Identity<B>` in package `core`",
        "write the implementation in the package's own code, request it with a header",
    ] {
        assert!(cause.contains(part), "{part}\n{cause}");
    }
    // Without the library's implementation the item is accepted.
    let layout = Layout::new()
        .ingot("lib", &[], "pub struct Unused {}\n")
        .ingot("app", &[("lib", "lib")], &app(eq));
    let _ = accepted(&layout, "app");
}

/// The bound is written in the package, in a const expression: the refusal
/// quotes it as it is written.
#[test]
fn the_refusal_quotes_the_bound_of_the_deciding_implementation_as_written() {
    let written = const_bound_lanes("", "Chk<{ <Y as Tr>::N }>", ("", "", ""), "CellLanes");
    let layout = Layout::new().ingot("app", &[], &chain_source(&written, TR_FOR_Y));
    let cause = refused(&layout, "app");
    for part in [
        "The written implementation of `LaneCodec<u8>` for `CellLanes` in this package applies to the types that meet its bounds",
        "the bound `Chk<{ <Y as Tr>::N }>: Okay` names `Tr`",
        "so this implementation of `Tr` could add types that meet them.",
    ] {
        assert!(cause.contains(part), "{part}\n{cause}");
    }
}

// ---- Traits that the compiler consults through an associated type or a block ----
//
// `LaneCodec` is consulted by the compiler although no bound of a written
// implementation names it: a place-indexed collection packs its elements by
// the `LaneCodec` of its `Lanes` type. It declares `#[decides = "..."]`, so a
// generated implementation of it is refused like any other deciding trait.
//
// `MsgVariant` is not one: a `recv` block asks for `MsgVariant<Sol>` of the
// message type it names by ordinary selection and treats a missing one as an
// error, so a generated implementation can only fill a gap or conflict.

/// A second ABI for a message type: a client generator for it would write
/// `MsgVariant<Alt>`, and the dispatcher of a written contract would answer
/// to its selector instead of the written one.
const ALT_ABI: &str = "use core::abi::{Abi, AbiDecoder, ByteInput, Decode, Encode, AbiFrame}
use core::message::MsgVariant
use std::abi::Sol
pub struct Alt {}
pub struct AltDec { pub pos: u256, pub base: u256 }
impl AbiDecoder<Alt> for AltDec {
    fn base(self) -> u256 { self.base }
    fn set_base(mut self, _ new_base: u256) { self.base = new_base }
    fn pos(self) -> u256 { self.pos }
    fn set_pos(mut self, _ new_pos: u256) { self.pos = new_pos }
    fn read_u8<I: ByteInput>(mut self, _ input: I) -> u8 { 0 }
    fn read_word<I: ByteInput>(mut self, _ input: I) -> u256 { 0 }
    fn peek_word<I: ByteInput>(self, _ input: I, _ at: u256) -> u256 { 0 }
}
impl Abi for Alt {
    type Selector = u32
    type Decoder = AltDec
    const SELECTOR_SIZE: u256 = 4
    fn selector_from_prefix(_ prefix: u256) -> u32 { 0 }
    fn decode_bool_word(_ w: u256) -> bool { false }
    fn decode_u8_word(_ w: u256) -> u8 { 0 }
    fn decode_u16_word(_ w: u256) -> u16 { 0 }
    fn decode_u32_word(_ w: u256) -> u32 { 0 }
    fn decode_u64_word(_ w: u256) -> u64 { 0 }
    fn decode_u128_word(_ w: u256) -> u128 { 0 }
    fn decode_i8_word(_ w: u256) -> i8 { 0 }
    fn decode_i16_word(_ w: u256) -> i16 { 0 }
    fn decode_i32_word(_ w: u256) -> i32 { 0 }
    fn decode_i64_word(_ w: u256) -> i64 { 0 }
    fn decode_i128_word(_ w: u256) -> i128 { 0 }
    fn decode_error() -> ! { <Sol as Abi>::decode_error() }
    fn validate_runtime_head(input_len: u256, base: u256, head_size: u256) {}
    fn decoder_new() -> AltDec { AltDec { pos: 0, base: 0 } }
    fn decoder_with_base(_ base: u256) -> AltDec { AltDec { pos: base, base } }
}
msg M {
    #[selector = 0x11111111]
    Ping { x: u256 } -> u256,
}
impl Encode<Alt> for M::Ping { fn encode(self, _ out: AbiFrame) {} }
impl Decode<Alt> for M::Ping {
    fn decode_payload<D: AbiDecoder<Alt>, I: ByteInput>(_ d: mut D, _ input: I) -> Self {
        M::Ping { x: 0 }
    }
}
pub contract C {
    recv M {
        Ping { x } -> u256 { x + 1 }
    }
}
";

/// The `MsgVariant` that gives a `recv` arm its selector and return type.
const SOL_ARM: &str = "use core::abi::{AbiDecoder, AbiSize, ByteInput, Decode, Encode, AbiFrame}
use core::message::MsgVariant
use std::abi::Sol
impl AbiSize for P {
    const HEAD_SIZE: u256 = 32
    const IS_DYNAMIC: bool = false
}
impl Encode<Sol> for P { fn encode(self, _ out: AbiFrame) { let _ = out } }
impl Decode<Sol> for P {
    fn decode_payload<D: AbiDecoder<Sol>, I: ByteInput>(_ d: mut D, _ input: I) -> Self {
        let _ = input
        P { v: 0 }
    }
}
pub contract C {
    recv {
        P { v } -> u256 { v + 1 }
    }
}
pub const fn selector() -> u256 { <P as MsgVariant<Sol>>::SELECTOR as u256 }
";

/// A generated implementation for a second ABI is an ordinary item of the
/// package: it is accepted, readable, and no `recv` block answers to it. The
/// block's ABI keeps the selector of the message that the `msg` block wrote.
#[test]
fn a_generated_msg_variant_for_another_abi_is_accepted_and_the_blocks_abi_keeps_its_selector() {
    let written = format!(
        "{ALT_ABI}
pub const fn sol_selector() -> u256 {{ <M::Ping as MsgVariant<Sol>>::SELECTOR as u256 }}
pub const fn alt_selector() -> u256 {{ <M::Ping as MsgVariant<Alt>>::SELECTOR as u256 }}
"
    );
    let generated =
        "impl MsgVariant<Alt> for M::Ping { const SELECTOR: u32 = 0x22222222  type Return = u256 }";
    let layout = Layout::new().ingot("app", &[], &chain_source(&written, generated));
    let db = accepted(&layout, "app");
    assert_eq!(errors(&db, "app"), "");
    assert_eq!(value(&db, "app", "sol_selector"), "286331153", "0x11111111");
    assert_eq!(value(&db, "app", "alt_selector"), "572662306", "0x22222222");
}

/// A generated implementation for the ABI of the block fills the gap of an arm
/// that names a type with none: without it the arm is the error 8-0049, with
/// it the arm takes its selector.
#[test]
fn a_generated_msg_variant_for_a_plain_type_fills_the_gap_of_a_recv_arm() {
    let generated =
        "impl MsgVariant<Sol> for P { const SELECTOR: u32 = 0x12345678  type Return = u256 }";
    let control = Layout::new().ingot("app", &[], &format!("{CHAIN_HEAD}{SOL_ARM}"));
    let cause = errors(&control.db(), "app");
    assert!(cause.contains("8-0049"), "{cause}");

    let layout = Layout::new().ingot("app", &[], &chain_source(SOL_ARM, generated));
    let db = accepted(&layout, "app");
    assert_eq!(errors(&db, "app"), "");
    assert_eq!(value(&db, "app", "selector"), "305419896", "0x12345678");
}

/// A generated implementation beside one that is written is the ordinary
/// conflict of two implementations: the `msg` block writes one for each of its
/// variants, and a package may write one for any type.
#[test]
fn a_generated_msg_variant_beside_a_written_one_is_a_conflict() {
    let generated =
        "impl MsgVariant<Sol> for M::Ping { const SELECTOR: u32 = 0x22222222  type Return = u256 }";
    let layout = Layout::new().ingot("app", &[], &chain_source(ALT_ABI, generated));
    let db = accepted(&layout, "app");
    let cause = errors(&db, "app");
    assert!(cause.contains("5-0001"), "{cause}");
    assert!(
        cause.contains("conflicting trait implementations"),
        "{cause}"
    );

    let written = format!(
        "{SOL_ARM}impl MsgVariant<Sol> for P {{ const SELECTOR: u32 = 0x11111111  type Return = u256 }}\n"
    );
    let generated =
        "impl MsgVariant<Sol> for P { const SELECTOR: u32 = 0x12345678  type Return = u256 }";
    let layout = Layout::new().ingot("app", &[], &chain_source(&written, generated));
    let db = accepted(&layout, "app");
    let cause = errors(&db, "app");
    assert!(cause.contains("5-0001"), "{cause}");
}

/// A message type with a conditional implementation is a message when its
/// bound holds. The bound's trait is no longer refused because a deciding
/// implementation names it, and a generated implementation of it makes the arm
/// valid like any other gap fill.
#[test]
fn a_generated_trait_in_the_bound_of_a_written_msg_variant_fills_a_gap() {
    let written = "use core::abi::{AbiDecoder, AbiSize, ByteInput, Decode, Encode, AbiFrame}
use core::message::MsgVariant
use std::abi::Sol
pub struct W<T> { pub value: u8 }
impl<T> AbiSize for W<T> {
    const HEAD_SIZE: u256 = 32
    const IS_DYNAMIC: bool = false
}
impl<T> Encode<Sol> for W<T> { fn encode(self, _ out: AbiFrame) { let _ = out } }
impl<T> Decode<Sol> for W<T> {
    fn decode_payload<D: AbiDecoder<Sol>, I: ByteInput>(_ d: mut D, _ input: I) -> Self {
        let _ = input
        W { value: 0 }
    }
}
impl<T: Gate> MsgVariant<Sol> for W<T> { const SELECTOR: u32 = 0x12345678  type Return = u8 }
pub contract C {
    recv {
        W<X> { value } -> u8 { value }
    }
}
";
    let control = Layout::new().ingot("app", &[], &format!("{CHAIN_HEAD}{written}"));
    let cause = errors(&control.db(), "app");
    assert!(cause.contains("8-0049"), "{cause}");

    let layout = Layout::new().ingot("app", &[], &chain_source(written, "impl Gate for X {}"));
    let db = accepted(&layout, "app");
    assert_eq!(errors(&db, "app"), "");
}

/// The `LaneCodec` of the `Lanes` type of a written `StateIndex` decides
/// whether its elements share a slot: without one they keep their type's
/// layout, with one they are packed and read through its `get` and `set`.
#[test]
fn a_generated_lane_codec_cannot_change_how_a_written_state_index_packs() {
    let written = "use core::marker::{Pinned, Storage}
use core::ops::{LaneCodec, Location}
pub struct CellLanes {}
pub struct Cells { len: u256 }
impl Pinned for Cells { type Space = Storage }
unsafe impl StateIndex for Cells {
    type Key = u256
    type Entry = u8
    type Lanes = CellLanes
    fn locate(self, _ root: u256, _ key: u256) -> Location {
        Location { slot: root + 100 + key, offset: 0 }
    }
}
";
    assert_refused_but_writable(
        written,
        "impl LaneCodec<u8> for CellLanes {
    const BITS: u256 = 8
    fn get(_ word: u256, offset: u256) -> u8 { 77 }
    fn set(_ word: u256, offset: u256, value: own u8) -> u256 { word }
}",
        "the generated implementation of `LaneCodec<u8>` for `CellLanes`",
    );
}

// ---- Scoped requests of several dependencies in one round ----------------------

/// The dependency `gd` writes a function whose block opens a provider; the
/// `fn write` is generic, so the request exists only for a concrete call.
const BATCH_GD: &str = "use core::derive::Derive
use core::meta::{Code, Item, Record}
use std::evm::{SolArray, SolPacked}

trait Gate {
    fn code(_ value: u8) -> u8
}
pub struct Arg { value: u8 }
impl Gate for Arg {
    fn code(_ value: u8) -> u8 { value }
}
struct ProvideGate {}
impl Derive<trait Gate> for ProvideGate {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn code(_ value: u8) -> u8 { value ^ 1 } }
    }
}
pub struct Elem<K, T> { pub value: u8 }
impl<T: Gate> SolPacked for Elem<u8, T> {
    const BITS: u256 = 8
    fn from_packed(_ word: u256) -> Self {
        Elem { value: T::code(word.downcast_truncate()) }
    }
    fn to_packed(own self) -> u256 { T::code(self.value) as u256 }
}
pub fn write<K>(array: mut SolArray<Elem<K, Arg>>) {
    with (ProvideGate) { array[0] = Elem { value: 7 } }
}
pub const fn seed() -> u8 { 3 }
";

/// The dependency `dep` depends on `gd`, writes the same shape and reads a
/// function of `gd` in its provider.
const BATCH_DEP: &str = "use core::derive::Derive
use core::meta::{Code, Item, Record}
use std::evm::{SolArray, SolPacked}
use gd::seed

trait Gate2 {
    fn code(_ value: u8) -> u8
}
pub struct Arg2 { value: u8 }
impl Gate2 for Arg2 {
    fn code(_ value: u8) -> u8 { value }
}
struct ProvideGate2 {}
impl Derive<trait Gate2> for ProvideGate2 {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = seed()
        quote { fn code(_ value: u8) -> u8 { value ^ ${n} } }
    }
}
pub struct Elem2<K, T> { pub value: u8 }
impl<T: Gate2> SolPacked for Elem2<u8, T> {
    const BITS: u256 = 8
    fn from_packed(_ word: u256) -> Self {
        Elem2 { value: T::code(word.downcast_truncate()) }
    }
    fn to_packed(own self) -> u256 { T::code(self.value) as u256 }
}
pub fn write2<K>(array: mut SolArray<Elem2<K, Arg2>>) {
    with (ProvideGate2) { array[0] = Elem2 { value: 7 } }
}
";

/// `app` calls both functions, so one round holds a request for each
/// dependency, one of which depends on the other.
#[test]
fn dependency_demands_in_one_batch_do_not_depend_on_order() {
    let head = "use std::evm::SolArray
use gd::{Arg, Elem, write}
use dep::{Arg2, Elem2, write2}
";
    let first =
        "pub fn first(array: mut SolArray<Elem<u8, Arg>>) { write<u8>(array: mut array) }\n";
    let second =
        "pub fn second(array: mut SolArray<Elem2<u8, Arg2>>) { write2<u8>(array: mut array) }\n";
    let mut requests = Vec::new();
    for app in [
        format!("{head}{first}{second}"),
        format!("{head}{second}{first}"),
    ] {
        let layout = Layout::new()
            .ingot("gd", &[], BATCH_GD)
            .ingot("dep", &[("gd", "gd")], BATCH_DEP)
            .ingot("app", &[("dep", "dep"), ("gd", "gd")], &app);
        let prepared = accepted(&layout, "app");
        for root in ["app", "dep", "gd"] {
            assert_eq!(errors(&prepared, root), "", "{root}");
        }
        let mut found = prepared
            .generation_inputs()
            .requests(&prepared)
            .iter()
            .map(|request| format!("{request:?}"))
            .collect::<Vec<_>>();
        assert_eq!(found.len(), 2, "one request for each dependency");
        found.sort();
        requests.push(found);
    }
    assert_eq!(requests[0], requests[1]);
}
