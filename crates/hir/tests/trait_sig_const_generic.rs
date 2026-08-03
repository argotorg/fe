use camino::Utf8PathBuf;
use fe_hir::test_db::{HirAnalysisTestDb, format_diagnostics};

/// A trait-const const-generic in a trait method signature
/// (`Slot<{ Self::N }>`) must stay abstract while `Self` is generic and
/// evaluate through the impl when checked there; this used to panic in
/// semantic body lowering ("const ref should resolve to a semantic
/// instance").
#[test]
fn trait_sig_const_generic_stays_abstract_and_checks() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_sig_const_generic_ice.fe"),
        r#"
struct Slot<const N: u32> {}

trait ConstSigCtx {
    const N: u32
    fn take(self, slot: own Slot<{ Self::N }>) -> Slot<{ Self::N }>
}

struct ConstSigCtxStruct {}

impl ConstSigCtx for ConstSigCtxStruct {
    const N: u32 = 3

    fn take(self, slot: own Slot<{ Self::N }>) -> Slot<{ Self::N }> {
        slot
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// A trait associated `const` with a default must stay abstract when its
/// instance is satisfied only by an assumption (`T: HasN`), so an impl that
/// overrides the default specializes correctly instead of being fixed to the
/// default. `slot_for(b)` infers `T = Big` (whose `N` is `4`); if `T::N` were
/// resolved to the trait default `1` while `T` was abstract, the signature
/// would bake to `Slot<1>` and the assignment would mismatch `Slot<4>`.
#[test]
fn trait_const_default_stays_abstract_under_assumption() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_const_default_stays_abstract_under_assumption.fe"),
        r#"
struct Slot<const N: u32> {}

trait HasN {
    const N: u32 = 1
}

struct Big {}
impl HasN for Big {
    const N: u32 = 4
}

fn slot_for<T: HasN>(_ t: T) -> Slot<{ T::N }> {
    Slot {}
}

fn use_it(b: Big) {
    let _s: Slot<4> = slot_for(b)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// The default is still used when a concrete impl inherits it: the fix only
/// suppresses the default for assumption-satisfied abstract instances, not for
/// a uniquely-selected concrete impl that does not override the const.
#[test]
fn trait_const_default_used_for_inheriting_concrete_impl() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_const_default_used_for_inheriting_concrete_impl.fe"),
        r#"
struct Slot<const N: u32> {}

trait HasN {
    const N: u32 = 7
}

struct Plain {}
impl HasN for Plain {}

fn slot_for(_ t: Plain) -> Slot<{ Plain::N }> {
    Slot {}
}

fn use_it(p: Plain) {
    let _s: Slot<7> = slot_for(p)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// A trait associated `const` is accepted as an array-repeat length while still
/// abstract (`[0; T::N]`), consistent with the const-param repeat path
/// (`[0; N]`), and specializes to an overriding impl's value at instantiation.
#[test]
fn trait_const_is_accepted_as_array_repeat_length() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_const_is_accepted_as_array_repeat_length.fe"),
        r#"
trait HasN {
    const N: usize = 1
}

struct Big {}
impl HasN for Big {
    const N: usize = 4
}

fn array_for<T: HasN>(_ t: T) -> [u8; T::N] {
    [0; T::N]
}

fn use_it(b: Big) {
    let _a: [u8; 4] = array_for(b)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// Salsa keys const evaluation by the full generic substitution. A recursive
/// associated const that grows that substitution therefore never repeats a
/// query key and used to overflow the compiler stack instead of reporting a
/// source diagnostic.
#[test]
fn growing_assoc_const_specialization_is_diagnosed() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("growing_assoc_const_specialization.fe"),
        r#"
trait HasN {
    const N: u256
}

struct Seed {}
struct Wrap<T> {}

impl HasN for Seed {
    const N: u256 = 0
}

impl<T> HasN for Wrap<T> {
    const N: u256 = Wrap<Wrap<T>>::N
}

const VALUE: u256 = Wrap<Seed>::N
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);

    assert_eq!(
        rendered
            .matches("associated const `N` has a recursive definition")
            .count(),
        1,
        "{rendered}"
    );
    assert_eq!(
        rendered.matches("recursive constant definition").count(),
        1,
        "{rendered}"
    );
}

/// The definition-site fallback graph is intentionally conservative, but a
/// concrete evaluation result takes precedence. A statically dead recursive
/// reference must not make an otherwise finite generic const definition
/// recursive.
#[test]
fn statically_dead_growing_assoc_const_reference_is_accepted() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("statically_dead_growing_assoc_const_reference.fe"),
        r#"
trait HasN {
    const N: usize
}

struct Seed {}
struct Wrap<T> {}

impl<T> HasN for Wrap<T> {
    const N: usize = if false {
        Wrap<Wrap<T>>::N
    } else {
        1
    }
}

fn value() -> [u8; 1] {
    [0; Wrap<Seed>::N]
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// Explicit trait qualification follows a different resolution path from
/// `Type::CONST`; it must still reach the same specialization guard.
#[test]
fn qualified_growing_assoc_const_specialization_is_diagnosed() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("qualified_growing_assoc_const_specialization.fe"),
        r#"
trait HasN {
    const N: u256
}

struct Seed {}
struct Wrap<T> {}

impl HasN for Seed {
    const N: u256 = 0
}

impl<T> HasN for Wrap<T> {
    const N: u256 = <Wrap<Wrap<T>> as HasN>::N
}

const VALUE: u256 = Wrap<Seed>::N
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);

    assert_eq!(
        rendered
            .matches("associated const `N` has a recursive definition")
            .count(),
        1,
        "{rendered}"
    );
    assert_eq!(
        rendered.matches("recursive constant definition").count(),
        1,
        "{rendered}"
    );
}

/// Inherited trait defaults use the trait instance's generic arguments rather
/// than an impl body's arguments. Semantic lowering must resolve that default
/// body through the same path as type-level const evaluation; otherwise a
/// recursive invalid result reaches lowering as an unresolved const ref.
#[test]
fn growing_inherited_assoc_const_default_is_diagnosed_without_lowering_panic() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("growing_inherited_assoc_const_default.fe"),
        r#"
trait HasN {
    const N: u256 = Wrap<Self>::N
}

struct Seed {}
struct Wrap<T> {}

impl HasN for Seed {}
impl<T> HasN for Wrap<T> {}

const VALUE: u256 = Seed::N
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);

    assert_eq!(
        rendered.matches("has a recursive definition").count(),
        2,
        "{rendered}"
    );
    assert_eq!(
        rendered.matches("recursive constant definition").count(),
        1,
        "{rendered}"
    );
}

/// A default can construct a larger receiver and still terminate when the
/// selected implementation overrides the referenced const. Recursion is about
/// the selected body graph, not growth in a single reference in isolation.
#[test]
fn inherited_assoc_const_default_can_terminate_at_override() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("inherited_assoc_const_default_can_terminate_at_override.fe"),
        r#"
trait HasN {
    const N: u256 = Wrap<Self>::N
}

struct Seed {}
struct Wrap<T> {}

impl HasN for Seed {}

impl<T> HasN for Wrap<T> {
    const N: u256 = 7
}

const VALUE: u256 = Seed::N
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// Anonymous const bodies declared by a trait inherit the trait's generic
/// parameter bounds as well as its synthetic `Self: Trait` predicate.
#[test]
fn inherited_assoc_const_default_sees_trait_generic_bound() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("inherited_assoc_const_default_sees_trait_generic_bound.fe"),
        r#"
trait HasN {
    const N: u256
}

trait Choose<T: HasN> {
    const N: u256 = T::N
}

struct Seed {}
struct Host {}

impl HasN for Seed {
    const N: u256 = 5
}

impl Choose<Seed> for Host {}

const VALUE: u256 = Host::N
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// Explicit trait qualification takes a distinct path through lookup and
/// obligation solving, and must receive the same trait body assumptions.
#[test]
fn qualified_inherited_assoc_const_default_sees_trait_generic_bound() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("qualified_inherited_assoc_const_default_sees_trait_generic_bound.fe"),
        r#"
trait HasN {
    const N: u256
}

trait Choose<T: HasN> {
    const N: u256 = <T as HasN>::N
}

struct Seed {}
struct Host {}

impl HasN for Seed {
    const N: u256 = 5
}

impl Choose<Seed> for Host {}

const VALUE: u256 = Host::N
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// Re-entering a generic const body is valid when every changed argument is a
/// structural subterm of the active specialization. This finite blanket-impl
/// chain must reach the concrete `Seed` implementation.
#[test]
fn decreasing_assoc_const_specialization_terminates() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("decreasing_assoc_const_specialization.fe"),
        r#"
trait HasN {
    const N: u256
}

struct Seed {}
struct Wrap<T> {}

impl HasN for Seed {
    const N: u256 = 0
}

impl<T: HasN> HasN for Wrap<T> {
    const N: u256 = T::N
}

const VALUE: u256 = Wrap<Wrap<Seed>>::N
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// Structural descent is a multiset relation across generic arguments. The
/// smaller argument may already appear in another coordinate, so matching
/// each callee argument to a distinct caller argument is required to observe
/// the strict decrease.
#[test]
fn decreasing_assoc_const_specialization_with_duplicate_coordinate_terminates() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("decreasing_assoc_const_specialization_with_duplicate_coordinate.fe"),
        r#"
trait HasN {
    const N: u256
}

struct Seed {}
struct Pair<A, B> {}

impl HasN for Seed {
    const N: u256 = 0
}

impl<A: HasN, B> HasN for Pair<A, B> {
    const N: u256 = A::N
}

const VALUE: u256 = Pair<Pair<Seed, Seed>, Seed>::N
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// The guard is body-cycle based, not limited to direct self-reference: a
/// mutually recursive pair that grows the specialization is rejected at both
/// definitions.
#[test]
fn mutually_growing_assoc_const_specializations_are_diagnosed() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("mutually_growing_assoc_const_specializations.fe"),
        r#"
trait Pair {
    const A: u256
    const B: u256
}

struct Seed {}
struct Wrap<T> {}

impl<T> Pair for Wrap<T> {
    const A: u256 = Wrap<Wrap<T>>::B
    const B: u256 = Wrap<Wrap<T>>::A
}

const VALUE: u256 = Wrap<Seed>::A
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);

    assert_eq!(
        rendered.matches("has a recursive definition").count(),
        2,
        "{rendered}"
    );
    assert_eq!(
        rendered.matches("recursive constant definition").count(),
        1,
        "{rendered}"
    );
}

/// Duplicate generic coordinates form a dense equality graph. Recursion
/// rejection must use polynomial bipartite matching rather than enumerate all
/// coordinate permutations.
#[test]
fn duplicate_coordinate_assoc_const_recursion_is_diagnosed_without_backtracking() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("duplicate_coordinate_assoc_const_recursion.fe"),
        r#"
trait HasN {
    const N: u256
}

struct Seed {}
struct Many<A, B, C, D, E, F, G, H, I, J, K, L> {}

impl<A, B, C, D, E, F, G, H, I, J, K, L> HasN
    for Many<A, B, C, D, E, F, G, H, I, J, K, L>
{
    const N: u256 = Many<A, B, C, D, E, F, G, H, I, J, K, L>::N
}

const VALUE: u256 =
    Many<Seed, Seed, Seed, Seed, Seed, Seed, Seed, Seed, Seed, Seed, Seed, Seed>::N
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);

    assert_eq!(
        rendered
            .matches("associated const `N` has a recursive definition")
            .count(),
        1,
        "{rendered}"
    );
    assert_eq!(
        rendered.matches("recursive constant definition").count(),
        1,
        "{rendered}"
    );
}

/// A same-impl dependency can specialize a more deeply nested receiver and
/// still terminate when it moves to a different, concrete sibling const body.
#[test]
fn finite_nested_assoc_const_dependency_is_accepted() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("finite_nested_assoc_const_dependency.fe"),
        r#"
trait Pair {
    const A: u256
    const B: u256
}

struct Seed {}
struct Wrap<T> {}

impl<T> Pair for Wrap<T> {
    const A: u256 = Wrap<Wrap<T>>::B
    const B: u256 = 42
}

const VALUE: u256 = Wrap<Seed>::A
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// Re-entering one generic const body at an unrelated receiver is not itself
/// a recursive definition. Resolution may switch to a concrete implementation
/// on the following edge and terminate.
#[test]
fn finite_incomparable_assoc_const_specializations_are_accepted() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("finite_incomparable_assoc_const_specializations.fe"),
        r#"
trait Eval {
    const VALUE: usize
}

trait Next {
    const NEXT: usize
}

trait Root {
    const VALUE: usize
}

struct EvalWrap<T> {}
struct Small {}
struct Large {}
struct Marker {}

impl Next for Small {
    const NEXT: usize = EvalWrap<Large>::VALUE
}

impl Next for Large {
    const NEXT: usize = 11
}

impl<T: Next> Eval for EvalWrap<T> {
    const VALUE: usize = T::NEXT
}

impl Root for Marker {
    const VALUE: usize = EvalWrap<Small>::VALUE
}

fn value() -> [u8; 11] {
    [0; Marker::VALUE]
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// Definition-site dependency traversal must not depend on whether another,
/// dead branch happened to pre-visit an intermediate specialization.
#[test]
fn finite_incomparable_assoc_const_diamond_is_order_independent() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("finite_incomparable_assoc_const_diamond.fe"),
        r#"
trait Eval {
    const VALUE: u256
}

trait Next {
    const NEXT: u256
}

trait Root {
    const VALUE: u256
}

struct EvalWrap<T> {}
struct Small {}
struct Large {}
struct Marker {}

impl Next for Small {
    const NEXT: u256 = EvalWrap<Large>::VALUE
}

impl Next for Large {
    const NEXT: u256 = 11
}

impl<T: Next> Eval for EvalWrap<T> {
    const VALUE: u256 = T::NEXT
}

impl Root for Marker {
    const VALUE: u256 = if false {
        Small::NEXT
    } else {
        EvalWrap<Small>::VALUE
    }
}

const RESULT: u256 = Marker::VALUE
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// Adjacent specializations can be incomparable while a later state grows an
/// older active state. Comparing every active frame preserves deterministic
/// rejection of this alternating unbounded family.
#[test]
fn alternating_incomparable_assoc_const_growth_is_diagnosed() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("alternating_incomparable_assoc_const_growth.fe"),
        r#"
trait Eval {
    const VALUE: u256
}

trait Step<B> {
    const NEXT: u256
}

struct Pair<A, B> {}
struct Left<T> {}
struct Right<T> {}
struct Base {}
struct Seed {}
struct Other {}

impl<A: Step<B>, B> Eval for Pair<A, B> {
    const VALUE: u256 = <A as Step<B>>::NEXT
}

impl<T> Step<Seed> for Left<T> {
    const NEXT: u256 = Pair<Right<Left<T>>, Other>::VALUE
}

impl<T> Step<Other> for Right<T> {
    const NEXT: u256 = Pair<Left<Right<T>>, Seed>::VALUE
}

const VALUE: u256 = Pair<Left<Base>, Seed>::VALUE
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);

    assert_eq!(
        rendered.matches("has a recursive definition").count(),
        2,
        "{rendered}"
    );
    assert_eq!(
        rendered.matches("recursive constant definition").count(),
        1,
        "{rendered}"
    );
}

/// Inherent associated consts use a separate resolution record but share the
/// same specialization-family guard.
#[test]
fn growing_inherent_const_specialization_is_diagnosed() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("growing_inherent_const_specialization.fe"),
        r#"
struct Wrap<T> {}

impl<T> Wrap<T> {
    const N: u256 = Wrap<Wrap<T>>::N
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);

    assert_eq!(
        rendered.matches("recursive constant definition").count(),
        1,
        "{rendered}"
    );
}

#[test]
fn finite_nested_inherent_const_dependency_is_accepted() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("finite_nested_inherent_const_dependency.fe"),
        r#"
struct Seed {}
struct Wrap<T> {}

impl<T> Wrap<T> {
    const A: u256 = Wrap<Wrap<T>>::B
    const B: u256 = 42
}

const VALUE: u256 = Wrap<Seed>::A
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// Ground const-generic arithmetic must be compared by its numeric value.
/// Treating `{ 20 - 1 }` as a syntax tree containing `20` reverses the
/// progress relation and rejects this finite countdown. Its depth also exceeds
/// the unproven-specialization ceiling, proving that established numeric
/// descent is exempt.
#[test]
fn decreasing_const_generic_assoc_specialization_terminates() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("decreasing_const_generic_assoc_specialization.fe"),
        r#"
trait Eval {
    const VALUE: usize
}

struct Count<const N: usize> {}

impl<const N: usize> Eval for Count<N> {
    const VALUE: usize = if N == 0 {
        0
    } else {
        Count<{ N - 1 }>::VALUE
    }
}

fn value() -> [u8; 0] {
    [0; Count<20>::VALUE]
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

/// A numeric decrease proves that a specialization family is not cycling, but
/// it does not bound its depth. Signed integers can descend for an enormous
/// number of distinct specializations, so the compiler must stop before the
/// host stack is exhausted even though every individual step makes progress.
#[test]
fn deeply_decreasing_const_generic_assoc_specialization_is_bounded() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("deeply_decreasing_const_generic_assoc_specialization.fe"),
        r#"
trait Eval {
    const VALUE: i256
}

struct Count<const N: i256> {}

impl<const N: i256> Eval for Count<N> {
    const VALUE: i256 = Count<{ N - 1 }>::VALUE
}

const VALUE: i256 = Count<0>::VALUE
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);

    assert_eq!(
        rendered
            .matches("const evaluation exceeded the recursion limit")
            .count(),
        1,
        "{rendered}"
    );
}

/// Numeric normalization must preserve rejection in the other direction:
/// each concrete specialization is larger than its active caller.
#[test]
fn growing_const_generic_assoc_specialization_is_diagnosed() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("growing_const_generic_assoc_specialization.fe"),
        r#"
trait Eval {
    const VALUE: usize
}

struct Count<const N: usize> {}

impl<const N: usize> Eval for Count<N> {
    const VALUE: usize = Count<{ N + 1 }>::VALUE
}

const VALUE: usize = Count<0>::VALUE
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);

    assert_eq!(
        rendered.matches("has a recursive definition").count(),
        1,
        "{rendered}"
    );
    assert_eq!(
        rendered.matches("recursive constant definition").count(),
        1,
        "{rendered}"
    );
}

/// Associated-type normalization can deconstruct a receiver and rebuild a
/// larger composite without retaining the previous composite as an exact
/// subtree. Such an unproven specialization family must hit the explicit
/// recursion limit instead of expanding compiler queries forever.
#[test]
fn projection_rebuild_assoc_specialization_is_bounded() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("projection_rebuild_assoc_specialization.fe"),
        r#"
trait HasN {
    const N: usize
}

trait Advance {
    type Out: HasN
}

struct Seed {}
struct Wrap<T> {}
struct State<A, B> {}

impl<A, B> Advance for State<A, B> {
    type Out = State<Wrap<A>, Wrap<B>>
}

impl<T: Advance> HasN for T {
    const N: usize = <T::Out as HasN>::N
}

const VALUE: usize = State<Seed, Seed>::N
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);

    assert_eq!(
        rendered
            .matches("const evaluation exceeded the recursion limit")
            .count(),
        1,
        "{rendered}"
    );
}

/// A projection rebuild is not recursive merely because it re-enters the same
/// blanket const body once. Concrete dispatch may select a different
/// associated-type implementation on the next step and reach a base body.
#[test]
fn finite_projection_rebuild_assoc_specialization_terminates() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("finite_projection_rebuild_assoc_specialization.fe"),
        r#"
trait HasN {
    const N: usize
}

trait Advance {
    type Out: HasN
}

struct Seed {}
struct Wrap<T> {}
struct State<A, B> {}
struct End {}

impl Advance for State<Seed, Seed> {
    type Out = State<Wrap<Seed>, Wrap<Seed>>
}

impl Advance for State<Wrap<Seed>, Wrap<Seed>> {
    type Out = End
}

impl HasN for End {
    const N: usize = 29
}

impl<T: Advance> HasN for T {
    const N: usize = <T::Out as HasN>::N
}

fn value() -> [u8; 29] {
    [0; State<Seed, Seed>::N]
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}
