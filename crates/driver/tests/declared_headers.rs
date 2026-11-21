//! A written implementation header, `impl Trait for Type with (Provider)`,
//! states that the implementation exists. Admission and providers may rely on
//! that fact. They may not run the implementation's members, which preparation
//! has not produced, and an inherent header `impl Type with (Provider)`
//! declares no trait fact.
use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::analysis::{
    semantic::{EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const},
    ty::ty_check::BodyOwner,
};
use url::Url;

const URL: &str = "file:///tmp/declared-headers.fe";

fn prepare(source: &str) -> Result<DriverDataBase, String> {
    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::parse(URL).unwrap(), Some(source.into()));
    prepare_file(&db, file)
        .map_err(|error| error.cause)?
        .ok_or_else(|| "expected generation output".to_string())
}

fn value_of(db: &DriverDataBase, name: &str) -> String {
    let file = db
        .workspace()
        .get(db, &Url::parse(URL).unwrap())
        .expect("prepared file");
    let function = db
        .top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|f| f.name(db).to_opt().is_some_and(|n| n.data(db) == name))
        .unwrap_or_else(|| panic!("missing function `{name}`"));
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

/// A target that names a struct whose
/// declared bound is met only by a generated implementation.
const NESTED: &str = "use core::derive::Derive
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
LEAF
struct Wrap<A: Size> { inner: A }
struct Holder { w: Wrap<Leaf> }
impl Size for Holder with (CountP)
const fn run() -> usize { Leaf::size() + Holder::size() }
";

#[test]
fn a_written_header_is_a_fact_for_target_admission() {
    let source = NESTED.replace("LEAF", "impl Size for Leaf with (CountP)");
    let output = prepare(&source).expect("the header declares Leaf: Size");
    assert_eq!(value_of(&output, "run"), "2");
}

/// The handwritten twin is accepted, so the header form removes the
/// asymmetry between handwritten and generated facts.
#[test]
fn a_handwritten_twin_is_accepted() {
    let source = NESTED.replace(
        "LEAF",
        "impl Size for Leaf { const fn size() -> usize { 1 } }",
    );
    let output = prepare(&source).expect("handwritten implementation");
    assert_eq!(value_of(&output, "run"), "2");
}

/// The same through a provider that queries the
/// fields' implementations: the query needs the fact, not the members.
const QUERY: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item, field, field_type, query_field, FieldQuery}
trait Tag { const fn tag() -> u256 }
struct TagP {}
impl Derive<trait Tag> for TagP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn tag() -> u256 { 7 } }
    }
}
struct Probe<T> {}
impl<T: Tag> FieldQuery<u256> for Probe<T> {
    const fn evaluate() -> u256 { 5 }
}
struct Inner { a: u256 }
INNER
struct ProbeP {}
impl DeriveInherent for ProbeP {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = query_field<Probe, u256>(field_type(field(record, 0)))
        quote { pub const fn probed() -> u256 { ${n} } }
    }
}
struct Outer { inner: Inner }
impl Outer with (ProbeP)
const fn run() -> u256 { Outer::probed() + Inner::tag() }
";

#[test]
fn a_header_serves_a_provider_that_queries_the_field_type() {
    let source = QUERY.replace("INNER", "impl Tag for Inner with (TagP)");
    let output = prepare(&source).expect("the query needs only the fact");
    assert_eq!(value_of(&output, "run"), "12");
}

/// A header states that the implementation exists, not what it contains. A
/// provider that runs a member of it, or reads the trait's default in its
/// place, would observe something the final program does not use.
#[test]
fn a_provider_cannot_run_the_members_of_a_header() {
    let source = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
trait Size { const fn size() -> u256 { 1 } }
struct BigP {}
impl Derive<trait Size> for BigP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn size() -> u256 { 99 } }
    }
}
struct Leaf { v: u256 }
impl Size for Leaf with (BigP)
struct PeekP {}
impl DeriveInherent for PeekP {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = PEEK
        quote { pub const fn seen() -> u256 { ${n} } }
    }
}
struct S {}
impl S with (PeekP)
const fn read_seen() -> u256 { S::seen() }
";
    let result = prepare(&source.replace("PEEK", "Leaf::size()"));
    assert!(
        result.is_err(),
        "a provider ran a member of a header or the trait's default in its place"
    );
    // The control: the same provider reads a handwritten implementation.
    let hand = source
        .replace(
            "impl Size for Leaf with (BigP)",
            "impl Size for Leaf { const fn size() -> u256 { 99 } }",
        )
        .replace("PEEK", "Leaf::size()");
    let output = prepare(&hand).expect("a handwritten implementation can be run");
    assert_eq!(value_of(&output, "read_seen"), "99");
}

/// A written header is a fact during provider admission. The prepared file
/// still needs the implementation to pass its ordinary conformance check.
#[test]
fn a_header_that_is_not_delivered_fails_conformance() {
    let source = NESTED
        .replace("LEAF", "impl Size for Leaf with (EmptyP)")
        .replace(
            "struct CountP {}",
            "struct EmptyP {}\nimpl Derive<trait Size> for EmptyP {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn unrelated() -> usize { 0 } }\n    }\n}\nstruct CountP {}",
        );
    let output = prepare(&source).expect("the provider output can be checked");
    let file = output
        .workspace()
        .get(&output, &Url::parse(URL).unwrap())
        .unwrap();
    let diagnostics = output.run_on_top_mod(output.top_mod(file));
    let errors = diagnostics.format_diags(&output);
    assert!(diagnostics.has_errors(&output), "{errors}");
    assert!(
        errors.contains("method `unrelated` is not defined in trait `Size`"),
        "{errors}"
    );
    assert!(errors.contains("missing implementations: size"), "{errors}");
}

/// A written header satisfies a `where` bound on a sibling derive target.
#[test]
fn a_written_header_satisfies_a_where_bound_on_a_sibling_target() {
    let source = "use core::derive::Derive
use core::meta::{Code, Item, Record}

pub trait Score {
    const fn score() -> usize
}

pub struct Provider {}
impl Derive<trait Score> for Provider {
    const fn derive(_ target: Record) -> Code<Item> {
        quote {
            const fn score() -> usize { 1 }
        }
    }
}

pub struct Inner {
    pub x: usize,
}
impl Score for Inner with (Provider)

pub struct Wrapper<T: Score> {
    pub value: T,
}

pub struct Outer {
    pub wrapped: Wrapper<Inner>,
}
impl Score for Outer with (Provider)

const fn run() -> usize { Inner::score() + Outer::score() }
";
    let output = prepare(source).expect("Inner: Score header satisfies Wrapper<Inner> bound");
    assert_eq!(value_of(&output, "run"), "2");
}

/// Two mutual written headers both pass target admission in the input view.
#[test]
fn mutual_written_headers_both_pass_admission() {
    let source = "use core::derive::Derive
use core::meta::{Code, Item, Record}

pub trait Score {
    const fn score() -> usize
}

pub struct Provider {}
impl Derive<trait Score> for Provider {
    const fn derive(_ target: Record) -> Code<Item> {
        quote {
            const fn score() -> usize { 3 }
        }
    }
}

pub struct Boxed<T: Score> {
    pub marker: usize,
}

pub struct Left {
    pub peer: Boxed<Right>,
}
impl Score for Left with (Provider)

pub struct Right {
    pub peer: Boxed<Left>,
}
impl Score for Right with (Provider)

const fn run() -> usize { Left::score() + Right::score() }
";
    let output = prepare(source).expect("mutual written headers satisfy each other's bounds");
    assert_eq!(value_of(&output, "run"), "6");
}

/// Calling `Pair::score()` inside the provider for `impl Score for Pair with (Fields)`
/// is rejected with the declared-only message.
#[test]
fn calling_a_target_header_method_inside_its_provider_is_rejected_as_declared_only() {
    let source = "use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> usize }
struct Fields {}
impl Derive<trait Score> for Fields {
    const fn derive(_ r: Record) -> Code<Item> {
        let n = Pair::score()
        quote { const fn score() -> usize { ${n} } }
    }
}
pub struct Pair { a: u256 }
impl Score for Pair with (Fields)
";
    let cause = prepare(source)
        .err()
        .expect("calling a declared-only header method must be refused");
    assert!(
        cause.contains("only declared by a written header"),
        "{cause}"
    );
}

/// An inherent header `impl Type with (Provider)` names no trait, so it
/// declares no trait fact and providers in the same package cannot see its
/// generated functions or constants while they run.
#[test]
fn a_provider_cannot_see_the_members_of_an_inherent_header() {
    let source = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
struct BigP {}
impl DeriveInherent for BigP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub const COUNT: u256 = 42
            pub const fn size() -> u256 { 99 }
        }
    }
}
struct Leaf { v: u256 }
impl Leaf with (BigP)
struct PeekP {}
impl DeriveInherent for PeekP {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = PEEK
        quote { pub const fn seen() -> u256 { ${n} } }
    }
}
struct S {}
impl S with (PeekP)
const fn run() -> u256 { S::seen() }
";
    for peek in ["Leaf::size()", "Leaf::COUNT"] {
        let result = prepare(&source.replace("PEEK", peek));
        assert!(
            result.is_err(),
            "a provider saw `{peek}` from an inherent header before preparation completed"
        );
    }

    // Self-call inside the inherent provider is also unseen in the input view.
    let self_call = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
struct SelfP {}
impl DeriveInherent for SelfP {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = Leaf::size()
        quote { pub const fn size() -> u256 { ${n} } }
    }
}
struct Leaf { v: u256 }
impl Leaf with (SelfP)
";
    assert!(
        prepare(self_call).is_err(),
        "an inherent provider must not see its own generated member in the input view"
    );

    // Control: a handwritten inherent implementation is visible to the provider.
    let hand = source
        .replace(
            "impl Leaf with (BigP)",
            "impl Leaf {\n    pub const COUNT: u256 = 42\n    pub const fn size() -> u256 { 99 }\n}",
        )
        .replace("PEEK", "Leaf::size() + Leaf::COUNT");
    let output = prepare(&hand).expect("a handwritten inherent implementation can be run");
    assert_eq!(value_of(&output, "run"), "141");
}

/// A package whose only generation is headers (trait headers and inherent
/// headers) has generated items but publishes no module-level item.
#[test]
fn a_package_that_only_derives_headers_publishes_no_item() {
    use hir::{
        analysis::name_resolution::{
            NameDomain, ingot_has_generated_items, ingot_publishes_items, resolve_ident_to_bucket,
        },
        hir_def::{IdentId, PathId, scope_graph::ScopeId},
    };

    let header_only = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub trait Tr { const fn v() -> u256 }
pub struct S { pub x: u256 }
pub struct TraitP {}
impl Derive<trait Tr> for TraitP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn v() -> u256 { 10 } }
    }
}
pub struct InherentP {}
impl DeriveInherent for InherentP {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub const INHERENT_C: u256 = 20
            pub const fn inherent_v() -> u256 { 30 }
        }
    }
}
impl Tr for S with (TraitP)
impl S with (InherentP)
pub const fn run() -> u256 { <S as Tr>::v() + S::INHERENT_C + S::inherent_v() }
";
    let output = prepare(header_only).expect("header-only package prepares");
    let file = output
        .workspace()
        .get(&output, &Url::parse(URL).unwrap())
        .expect("prepared file");
    let top_mod = output.top_mod(file);
    let ingot = top_mod.ingot(&output);
    assert!(*ingot_has_generated_items(&output, ingot));
    assert!(!*ingot_publishes_items(&output, ingot));
    assert_eq!(value_of(&output, "run"), "60");

    let mod_scope = ScopeId::from_item(top_mod.into());
    for member_name in ["v", "INHERENT_C", "inherent_v"] {
        let bucket = resolve_ident_to_bucket(
            &output,
            PathId::from_ident(&output, IdentId::new(&output, member_name.to_string())),
            mod_scope,
        );
        assert!(
            bucket.pick(NameDomain::VALUE).is_err(),
            "header member `{member_name}` must not be published as a module-level value"
        );
    }
}
