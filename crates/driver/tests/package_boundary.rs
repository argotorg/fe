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
const SCOPED_READS_INHERENT: &str = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
struct SeedP {}
impl Derive<Inherent> for SeedP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn seed() -> u256 { 42 } }
    }
}
struct Token {}
impl Token with (SeedP)
struct Pair {}
struct A {}
impl Derive<Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = Token::seed()
        quote { const fn score() -> u256 { ${n} } }
    }
}
const fn run() -> u256 { with (A) { Pair::score() } }
";

const INHERENT_READS_SCOPED: &str = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
trait Seed { const fn seed() -> u256 }
struct Token {}
struct B {}
impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn seed() -> u256 { 42 } }
    }
}
struct Make {}
impl Derive<Inherent> for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = with (B) { Token::seed() }
        quote { pub const fn run() -> u256 { ${n} } }
    }
}
struct Pair {}
impl Pair with (Make)
";

const INHERENT_READS_INHERENT: &str = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
struct SeedP {}
impl Derive<Inherent> for SeedP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn seed() -> u256 { 42 } }
    }
}
struct Token {}
impl Token with (SeedP)
struct Make {}
impl Derive<Inherent> for Make {
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
    let lib = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item, field_count}
const fn boost(_ n: usize) -> u256 { n as u256 + 40 }
pub struct Make {}
impl Derive<Inherent> for Make {
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

const B6_APP: &str = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
use lib::Token
// A trait of the dependent, implemented by hand for the dependency's type.
pub trait Seed { const fn seed(self) -> u256 }
impl Seed for Token { const fn seed(self) -> u256 { 1 } }

pub struct Make {}
impl Derive<Inherent> for Make {
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
    let lib = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
pub struct AddSeed {}
impl Derive<Inherent> for AddSeed {
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
impl Derive<Seed> for SeedP {
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
    let app = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
use lib::{Seed, Token}
pub struct Make {}
impl Derive<Inherent> for Make {
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
impl Derive<Score> for A {
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
    let mid = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
use base::{Seed, Token}
pub struct Make {}
impl Derive<Inherent> for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = Token::seed() + 1
        quote { pub const fn mid_value() -> u256 { ${n} } }
    }
}
pub struct Marker {}
impl Marker with (Make)
";
    let app = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
use mid::Marker
pub struct Make {}
impl Derive<Inherent> for Make {
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
impl Derive<Seed> for SeedP {
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

/// A header written in the ingot declares that the implementation exists, so
/// a target whose bound needs it is admitted: the header for `Holder` admits
/// its field type `Wrap<Leaf>` through the header for `Leaf`.
#[test]
fn a_written_header_is_a_fact_for_target_admission_in_an_ingot() {
    let app = "use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}
trait Size { const fn size() -> usize }
struct CountP {}
impl Derive<Size> for CountP {
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

const ASSOC_TYPE: &str = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item, field, field_type, type_is}
pub trait Tr {
    type Out = u64
    const fn f() -> u256
}
pub struct P {}
impl Derive<Tr> for P {
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
impl Derive<Inherent> for Observe {
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

const GENERIC_ID_LIB: &str = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
pub struct EmitId {}
impl Derive<Inherent> for EmitId {
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
impl Derive<Tr> for P {
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
impl Derive<Tr> for P {
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
    let base = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
pub struct Emit {}
impl Derive<Inherent> for Emit {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn base_value() -> u256 { 1 } }
    }
}
pub struct Marker {}
impl Marker with (Emit)
";
    let middle = |name: &str, add: u32| {
        format!(
            "use core::derive::{{Derive, Inherent}}
use core::meta::{{Record, Code, Item}}
pub struct Make {{}}
impl Derive<Inherent> for Make {{
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
    let root = "use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}
pub struct Make {}
impl Derive<Inherent> for Make {
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
