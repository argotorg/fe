// The families of the limit matrix, shared by the analysis test
// (`crates/hir/tests/traits/limit_matrix.rs`) and the command-line test
// (`crates/fe/tests/limit_matrix.rs`), which runs the whole pipeline.
//
// Each family asks one question (a method call, an operator, an associated
// type or constant, a projection, a trait bound, an effect provider) that
// one candidate answers. A second, unrelated candidate has a where clause
// `T::Out: Show` whose type needs `depth` nested projections; the nesting
// limit is 64. Every family runs with both declaration orders, at depth 3
// and 70, with the unrelated where clause false and true (true adds
// `impl Show for u8`).
//
// At depth 3 nothing reaches a limit, so the result is the language's
// answer without limits. At depth 70 the result must be the same, unless the
// family is marked `AtLimit::Limit`: there the unrelated candidate could
// change the answer (it is a real rival, the only candidate, an overlap, or
// an effect provider that could be the one in scope), and the answer is the
// limit (law 5 of the limits design).

const PRELUDE: &str = "trait Nest { type Out }
struct N<T> { t: T }
impl<T: Nest> Nest for N<T> { type Out = T::Out }
impl Nest for u8 { type Out = u8 }
trait Show {}
impl Show for bool {}
trait Never {}
struct W<T> { t: T }
";

/// Error codes that name a normalization limit.
const LIMIT_CODES: &[&str] = &["3-0058", "2-0022"];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AtLimit {
    /// The unrelated candidate cannot change the answer.
    Same,
    /// The answer depends on the candidate over the limit.
    Limit,
}

struct Family {
    name: &'static str,
    /// The candidate that answers the question, with its declarations.
    main: &'static str,
    /// The unrelated candidate; `{D}` is the deep type argument.
    rival: &'static str,
    /// The question; `{D}` is the deep type argument.
    question: &'static str,
    at_limit: AtLimit,
}

fn chain(depth: usize) -> String {
    format!("{}u8{}", "N<".repeat(depth), ">".repeat(depth))
}

fn source(family: &Family, rival_first: bool, depth: usize, show: bool) -> String {
    let deep = chain(depth);
    let rival = family.rival.replace("{D}", &deep);
    let main = family.main.replace("{D}", &deep);
    let question = family.question.replace("{D}", &deep);
    let show = if show { "impl Show for u8 {}\n" } else { "" };
    let (first, second) = if rival_first {
        (rival, main)
    } else {
        (main, rival)
    };
    format!("{PRELUDE}{show}{first}\n{second}\n{question}\n")
}

/// Checks every cell of `family` with `codes`, which gives the sorted error
/// codes of a program. Returns the family's table if a cell is wrong.
fn check_family(family: &Family, codes: &dyn Fn(&str) -> Vec<String>) -> Option<String> {
    let mut failure = None;
    let mut table = Vec::new();
    for rival_first in [false, true] {
        for show in [false, true] {
            let shallow = codes(&source(family, rival_first, 3, show));
            let deep_src = source(family, rival_first, 70, show);
            let deep = codes(&deep_src);
            let ok = match family.at_limit {
                AtLimit::Same => deep == shallow,
                AtLimit::Limit => {
                    !deep.is_empty() && deep.iter().all(|code| LIMIT_CODES.contains(&&**code))
                }
            };
            table.push(format!(
                "rival_first={rival_first} show={show}: depth 3 {shallow:?}, depth 70 {deep:?}"
            ));
            if !ok && failure.is_none() {
                failure = Some(deep_src);
            }
        }
    }
    failure.map(|program| {
        format!(
            "{} ({:?} expected at depth 70):\n{}\nfirst failing program:\n{}",
            family.name,
            family.at_limit,
            table.join("\n"),
            program
        )
    })
}

/// Checks every family, `threads` at a time, and fails with the tables of
/// the families that are wrong.
fn check_all(codes: &(dyn Fn(&str) -> Vec<String> + Sync), threads: usize) {
    let next = std::sync::atomic::AtomicUsize::new(0);
    let failures = std::sync::Mutex::new(Vec::new());
    std::thread::scope(|scope| {
        for _ in 0..threads {
            scope.spawn(|| {
                loop {
                    let idx = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    let Some(family) = FAMILIES.get(idx) else {
                        break;
                    };
                    if let Some(failure) = check_family(family, codes) {
                        failures.lock().unwrap().push(failure);
                    }
                }
            });
        }
    });
    let failures = failures.into_inner().unwrap();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

macro_rules! families {
    ($($name:ident: $at_limit:ident, $main:expr, $rival:expr, $question:expr;)*) => {
        const FAMILIES: &[Family] = &[
            $(Family {
                name: stringify!($name),
                main: $main,
                rival: $rival,
                question: $question,
                at_limit: AtLimit::$at_limit,
            },)*
        ];
    };
}

const GET: &str = "trait Get { fn get(self) -> u8 }
impl<T> Get for W<T> { fn get(self) -> u8 { 1 } }";
const OTHER_GET_HIDDEN: &str = "mod m {
    use super::{W, Nest, Show}
    pub trait Other { fn get(self) -> u8 }
    impl<T: Nest> Other for W<T> where T::Out: Show { fn get(self) -> u8 { 2 } }
}";
const OTHER_GET_VISIBLE: &str = "trait Other { fn get(self) -> u8 }
impl<T: Nest> Other for W<T> where T::Out: Show { fn get(self) -> u8 { 2 } }";
const CONV: &str = "trait Conv<X> { fn conv(self) -> X }
impl<T> Conv<u8> for W<T> { fn conv(self) -> u8 { 1 } }";
const CONV_BOOL: &str =
    "impl<T: Nest> Conv<bool> for W<T> where T::Out: Show { fn conv(self) -> bool { true } }";
const MARKER_CONV: &str = "trait Conv<X> {}
impl<T> Conv<bool> for W<T> {}";
const MARKER_CONV_U8: &str = "impl<T: Nest> Conv<u8> for W<T> where T::Out: Show {}";
const PICK_METHOD: &str = "trait Pick { fn pick(self) -> u8 }
impl<T, U> Pick for W<T> where W<T>: Conv<U>, U: Show { fn pick(self) -> u8 { 1 } }
fn f(w: own W<{D}>) -> u8 { w.pick() }";
const HAS_VALUE: &str = "trait HasValue { const VALUE: u8 }
impl<T> HasValue for W<T> { const VALUE: u8 = 1 }";
const HAS_ITEM: &str = "trait HasItem { type Item }
impl<T> HasItem for W<T> { type Item = u8 }";
const LOGGER: &str = "trait Logger { fn log(self) -> u8 }
struct Console {}
impl Logger for Console { fn log(self) -> u8 { 1 } }
fn needs_logger() -> u8 uses (logger: Logger) { logger.log() }";
const DEEP_HANDLE: &str = "use core::effect_ref::{AddressSpace, EffectHandle, EffectRef, EffectRefMut}
struct Deep<T> { p: *u8, t: T }
impl<T> EffectHandle for Deep<T> {
    type Target = u8
    const SPACE: AddressSpace = AddressSpace::Memory
    type Raw = *u8
    fn raw(self) -> *u8 { self.p }
}
fn needs_mut() -> u8 uses (x: mut u8) { x }";
const DEEP_HANDLE_REF: &str = "use core::effect_ref::{AddressSpace, EffectHandle, EffectRef, EffectRefMut}
struct Deep<T> { p: *u8, t: T }
impl<T> EffectHandle for Deep<T> {
    type Target = u8
    const SPACE: AddressSpace = AddressSpace::Memory
    type Raw = *u8
    fn raw(self) -> *u8 { self.p }
}
impl<T> EffectRef<u8> for Deep<T> {}
fn needs_mut() -> u8 uses (x: mut u8) { x }";
const DEEP_LOGGER: &str = "struct Deep<T> { t: T }
impl<T: Nest> Logger for Deep<T> where T::Out: Show { fn log(self) -> u8 { 2 } }";
const TWO_HEADERS: &str = "trait Tr { type Out }
impl<T> Tr for W<T> { type Out = u8 }";
const MARKER_TR: &str = "trait Tr {}
impl<T> Tr for W<T> {}";
const NEED_TR: &str = "fn need<X: Tr>() {}
fn h() { need<W<{D}>>() }";

families! {
    // Method lookup: a trait that is not in scope.
    method_invisible_trait: Same, GET, OTHER_GET_HIDDEN,
        "fn f(w: own W<{D}>) -> u8 { w.get() }";
    method_invisible_blanket: Same,
        "trait Get { fn get(self) -> u8 }
impl<T> Get for N<T> { fn get(self) -> u8 { 1 } }",
        "mod m {
    use super::{Nest, Show}
    pub trait Other { fn get(self) -> u8 }
    impl<T: Nest> Other for T where T::Out: Show { fn get(self) -> u8 { 2 } }
}",
        "fn f(w: own {D}) -> u8 { w.get() }";
    method_invisible_bound_on_wrapper: Same, GET,
        "mod m {
    use super::{W, Nest, Show}
    pub trait Other { fn get(self) -> u8 }
    impl<T: Nest> Other for W<T> where W<T::Out>: Show { fn get(self) -> u8 { 2 } }
}",
        "fn f(w: own W<{D}>) -> u8 { w.get() }";
    method_invisible_default_method: Same, GET,
        "mod m {
    use super::{W, Nest, Show}
    pub trait Other { fn get(self) -> u8 { 2 } }
    impl<T: Nest> Other for W<T> where T::Out: Show {}
}",
        "fn f(w: own W<{D}>) -> u8 { w.get() }";
    method_invisible_supertrait: Same, GET,
        "mod m {
    use super::{W, Nest, Show}
    pub trait Base {}
    impl<T: Nest> Base for W<T> where T::Out: Show {}
    pub trait Other: Base { fn get(self) -> u8 }
    impl<T: Nest> Other for W<T> where W<T>: Base { fn get(self) -> u8 { 2 } }
}",
        "fn f(w: own W<{D}>) -> u8 { w.get() }";
    method_invisible_impl_outside_module: Same,
        "trait Vis { fn go(self) -> u8 }
impl<T> Vis for W<T> { fn go(self) -> u8 { 1 } }",
        "mod hid { pub trait Hid { fn go(self) -> u8 } }
impl<T: Nest> hid::Hid for W<T> where T::Out: Show { fn go(self) -> u8 { 2 } }",
        "fn m(w: W<{D}>) -> u8 { w.go() }";
    method_path_on_type: Same, GET, OTHER_GET_HIDDEN,
        "fn f(w: own W<{D}>) -> u8 { W<{D}>::get(w) }";
    method_through_assumption: Same, GET, OTHER_GET_HIDDEN,
        "fn g<X: Get>(_ x: own X) -> u8 { x.get() }
fn f(w: own W<{D}>) -> u8 { g(w) }";
    method_inherent_first: Same,
        "impl<T> W<T> { fn get(self) -> u8 { 1 } }", OTHER_GET_VISIBLE,
        "fn f(w: own W<{D}>) -> u8 { w.get() }";
    method_trait_qualified: Same, GET, OTHER_GET_VISIBLE,
        "fn f(w: own W<{D}>) -> u8 { Get::get(w) }";
    // Method lookup: a rival in scope with a method of the same name.
    method_visible_rival: Limit, GET, OTHER_GET_VISIBLE,
        "fn f(w: own W<{D}>) -> u8 { w.get() }";
    // Method lookup: the refuting where clause comes first or last.
    method_refuted_limit_first: Same,
        "trait Vis { fn go(self) -> u8 }
impl<T> Vis for W<T> { fn go(self) -> u8 { 1 } }",
        "trait Hid { fn go(self) -> u8 }
impl<T: Nest> Hid for W<T> where T::Out: Show, T: Never { fn go(self) -> u8 { 2 } }",
        "fn m(w: W<{D}>) -> u8 { w.go() }";
    method_refuted_never_first: Same,
        "trait Vis { fn go(self) -> u8 }
impl<T> Vis for W<T> { fn go(self) -> u8 { 1 } }",
        "trait Hid { fn go(self) -> u8 }
impl<T: Nest> Hid for W<T> where T: Never, T::Out: Show { fn go(self) -> u8 { 2 } }",
        "fn m(w: W<{D}>) -> u8 { w.go() }";
    // Method lookup: an inherent impl whose receiver has two projection
    // equalities, one over the limit and one that fails, in either order.
    method_inherent_key_limit_first: Same,
        "struct Three<A, B, C> {}
trait Flag { type Is }
impl<T> Flag for N<T> { type Is = bool }
impl<T> Three<T, u8, u8> { fn go(self) -> u8 { 1 } }",
        "impl<T: Nest + Flag> Three<T, T::Out, T::Is> { fn go(self) -> u8 { 2 } }",
        "fn m(w: Three<{D}, u8, u8>) -> u8 { w.go() }";
    method_inherent_key_refuted_first: Same,
        "struct Three<A, B, C> {}
trait Flag { type Is }
impl<T> Flag for N<T> { type Is = bool }
impl<T> Three<T, u8, u8> { fn go(self) -> u8 { 1 } }",
        "impl<T: Nest + Flag> Three<T, T::Is, T::Out> { fn go(self) -> u8 { 2 } }",
        "fn m(w: Three<{D}, u8, u8>) -> u8 { w.go() }";
    // Method lookup: only the equality over the limit, so the answer is open.
    method_inherent_key_limit_only: Limit,
        "struct Three<A, B, C> {}
impl<T> Three<T, u8, bool> { fn go(self) -> u8 { 1 } }",
        "impl<T: Nest> Three<T, T::Out, bool> { fn go(self) -> u8 { 2 } }",
        "fn m(w: Three<{D}, u8, bool>) -> u8 { w.go() }";
    // Method lookup: two traits have the method; the call's arguments decide
    // which applies once the receiver is known. The rival's first parameter
    // reaches the limit and its second is refuted by the argument, or not.
    method_argument_refuted_after_limit: Same,
        "trait A { fn choose(self, _ x: u8, _ y: u8) -> u8 }
impl<T> A for W<T> { fn choose(self, _ x: u8, _ y: u8) -> u8 { 1 } }",
        "trait B { type X
    fn choose(self, _ x: Self::X, _ y: bool) -> u8 }
impl<T: Nest> B for W<T> { type X = T::Out
    fn choose(self, _ x: T::Out, _ y: bool) -> u8 { 2 } }",
        "fn f(w: own W<{D}>, a: u8, b: u8) -> u8 { w.choose(a, b) }";
    // A return type that cannot match rules the candidate out too.
    method_return_refuted_after_limit: Same,
        "trait A { fn choose(self, _ x: u8) -> u8 }
impl<T> A for W<T> { fn choose(self, _ x: u8) -> u8 { 1 } }",
        "trait B { type X
    fn choose(self, _ x: Self::X) -> bool }
impl<T: Nest> B for W<T> { type X = T::Out
    fn choose(self, _ x: T::Out) -> bool { true } }",
        "fn f(w: own W<{D}>, a: u8) -> u8 {
    let r: u8 = w.choose(a)
    r
}";
    method_argument_limit_not_refuted: Limit,
        "trait A { fn choose(self, _ x: u8, _ y: u8) -> u8 }
impl<T> A for W<T> { fn choose(self, _ x: u8, _ y: u8) -> u8 { 1 } }",
        "trait B { type X
    fn choose(self, _ x: Self::X, _ y: u8) -> u8 }
impl<T: Nest> B for W<T> { type X = T::Out
    fn choose(self, _ x: T::Out, _ y: u8) -> u8 { 2 } }",
        "fn f(w: own W<{D}>, a: u8, b: u8) -> u8 { w.choose(a, b) }";
    // Method lookup: the trait argument is fixed later.
    method_expected_type_picks_impl: Same, CONV, CONV_BOOL,
        "fn f(w: own W<{D}>) -> u8 {
    let x: u8 = w.conv()
    x
}";
    method_qualified_trait_argument: Same, CONV, CONV_BOOL,
        "fn f(w: own W<{D}>) -> u8 { Conv<u8>::conv(w) }";
    operator_operand_picks_impl: Same,
        "use core::ops::Add
impl<T> Add<u8> for W<T> {
    type Output = u8
    fn add(own self, _ other: own u8) -> u8 { other }
}",
        "impl<T: Nest> Add<bool> for W<T> where T::Out: Show {
    type Output = u8
    fn add(own self, _ other: own bool) -> u8 { 2 }
}",
        "fn f(w: own W<{D}>, one: u8) -> u8 { w + one }";
    // Deferred bounds whose types are known only after inference.
    bound_inferred_later: Same, CONV, CONV_BOOL,
        "fn take<X: Conv<U>, U>(_ x: own X) -> U { x.conv() }
fn f(w: own W<{D}>) -> u8 {
    let r = take(w)
    r
}";
    bound_annotated: Same, CONV, CONV_BOOL,
        "fn take<X: Conv<U>, U>(_ x: own X) -> U { x.conv() }
fn f(w: own W<{D}>) -> u8 {
    let r: u8 = take(w)
    r
}";
    bound_projection_inferred_later: Same,
        "trait Tr<X> { type Out }
impl<T> Tr<u8> for W<T> { type Out = u8 }
trait Mk<X> { fn mk(self) -> X }
impl<T> Mk<u8> for W<T> { fn mk(self) -> u8 { 1 } }",
        "impl<T: Nest> Tr<bool> for W<T> where T::Out: Show { type Out = bool }",
        "fn proj<X: Tr<Y> + Mk<Y>, Y>(_ x: own X) -> Y { x.mk() }
fn f(w: own W<{D}>) -> u8 {
    let r = proj(w)
    r
}";
    // Trait solving: a sub-goal with an inference variable.
    solver_method_subgoal: Same, MARKER_CONV, MARKER_CONV_U8, PICK_METHOD;
    solver_bound_subgoal: Same, MARKER_CONV, MARKER_CONV_U8,
        "trait Pick {}
impl<T, U> Pick for W<T> where W<T>: Conv<U>, U: Show {}
fn need<X: Pick>(_ x: own X) {}
fn f(w: own W<{D}>) { need(w) }";
    solver_function_where_clause: Same, MARKER_CONV, MARKER_CONV_U8,
        "trait Pick {}
impl<T, U> Pick for W<T> where W<T>: Conv<U>, U: Show {}
fn need<X: Pick>(_ x: own X) {}
fn f(w: own W<{D}>) where W<{D}>: Pick { need(w) }";
    // Trait solving: two impls for the same goal, one refuted.
    solver_refuted_limit_first: Same, MARKER_TR,
        "impl<T: Nest> Tr for W<T> where T::Out: Show, T: Never {}", NEED_TR;
    solver_refuted_never_first: Same, MARKER_TR,
        "impl<T: Nest> Tr for W<T> where T: Never, T::Out: Show {}", NEED_TR;
    solver_other_impl_holds: Same,
        "trait A {}
impl<T> A for N<T> {}
trait Tr {}
impl<T: A> Tr for W<T> {}",
        "impl<T: Nest> Tr for W<T> where T::Out: Show {}", NEED_TR;
    solver_only_impl_over_limit: Limit, "trait Tr {}",
        "impl<T: Nest> Tr for W<T> where T::Out: Show {}", NEED_TR;
    // Associated constants and types looked up by name.
    const_unrelated_blanket: Same,
        "trait HasValue { const VALUE: u8 }
impl<T> HasValue for N<T> { const VALUE: u8 = 1 }",
        "trait Other { const OTHER: u8 }
impl<T: Nest> Other for T where T::Out: Show { const OTHER: u8 = 2 }",
        "fn f() -> u8 { {D}::VALUE }";
    const_unrelated_default: Same, HAS_VALUE,
        "trait Other { type Thing = u8
 const OTHER: u8 = 2 }
impl<T: Nest> Other for W<T> where T::Out: Show {}",
        "fn f() -> u8 { W<{D}>::VALUE }";
    const_refuted_limit_first: Same, HAS_VALUE,
        "trait Other { const VALUE: u8 }
impl<T: Nest> Other for W<T> where T::Out: Show, T: Never { const VALUE: u8 = 2 }",
        "fn c() -> u8 { W<{D}>::VALUE }";
    const_refuted_never_first: Same, HAS_VALUE,
        "trait Other { const VALUE: u8 }
impl<T: Nest> Other for W<T> where T: Never, T::Out: Show { const VALUE: u8 = 2 }",
        "fn c() -> u8 { W<{D}>::VALUE }";
    const_rival: Limit, HAS_VALUE,
        "trait Other { const VALUE: u8 }
impl<T: Nest> Other for W<T> where T::Out: Show { const VALUE: u8 = 2 }",
        "fn f() -> u8 { W<{D}>::VALUE }";
    const_invisible_rival: Limit, HAS_VALUE,
        "mod m {
    use super::{W, Nest, Show}
    pub trait Other { const VALUE: u8 }
    impl<T: Nest> Other for W<T> where T::Out: Show { const VALUE: u8 = 2 }
}",
        "fn f() -> u8 { W<{D}>::VALUE }";
    type_other_item: Same, HAS_ITEM,
        "trait Other { type Thing }
impl<T: Nest> Other for W<T> where T::Out: Show { type Thing = u8 }",
        "fn f(x: W<{D}>::Item) -> u8 { x }";
    type_qualified_other_item: Same, HAS_ITEM,
        "trait Other { type Thing }
impl<T: Nest> Other for W<T> where T::Out: Show { type Thing = u8 }",
        "fn f(x: <W<{D}> as HasItem>::Item) -> u8 { x }";
    type_same_value_rival: Same, HAS_ITEM,
        "trait Other { type Item }
impl<T: Nest> Other for W<T> where T::Out: Show { type Item = u8 }",
        "fn f(x: W<{D}>::Item) -> u8 { x }";
    type_rival: Limit, HAS_ITEM,
        "trait Other { type Item }
impl<T: Nest> Other for W<T> where T::Out: Show { type Item = bool }",
        "fn f(x: W<{D}>::Item) -> u8 { x }";
    // Projections with a complete trait header.
    projection_other_trait_argument: Same,
        "trait Tr<X> { type Out }
impl<T> Tr<bool> for W<T> { type Out = bool }",
        "impl<T: Nest> Tr<u8> for W<T> where T::Out: Show { type Out = u8 }",
        "fn f(x: <W<{D}> as Tr<bool>>::Out) -> bool { x }";
    projection_other_trait_argument_in_body: Same,
        "trait Tr<X> { type Out
 fn make(self) -> Self::Out }
impl<T> Tr<bool> for W<T> { type Out = bool
 fn make(self) -> bool { true } }",
        "impl<T: Nest> Tr<u8> for W<T> where T::Out: Show { type Out = u8
 fn make(self) -> u8 { 1 } }",
        "fn f(w: own W<{D}>) -> bool {
    let x: <W<{D}> as Tr<bool>>::Out = Tr<bool>::make(w)
    x
}";
    projection_refuted_limit_first: Same, TWO_HEADERS,
        "impl<T: Nest> Tr for W<T> where T::Out: Show, T: Never { type Out = u16 }",
        "fn f(x: <W<{D}> as Tr>::Out) -> u8 { x }";
    projection_refuted_never_first: Same, TWO_HEADERS,
        "impl<T: Nest> Tr for W<T> where T: Never, T::Out: Show { type Out = u16 }",
        "fn f(x: <W<{D}> as Tr>::Out) -> u8 { x }";
    projection_same_definition: Same,
        "trait A {}
impl<T> A for N<T> {}
trait Tr { type Out }
impl<T: A> Tr for W<T> { type Out = u8 }",
        "impl<T: Nest> Tr for W<T> where T::Out: Show { type Out = u8 }",
        "fn f(x: <W<{D}> as Tr>::Out) -> u8 { x }";
    projection_only_impl_over_limit: Limit, "trait Tr { type Out }",
        "impl<T: Nest> Tr for W<T> where T::Out: Show { type Out = u8 }",
        "fn f(x: <W<{D}> as Tr>::Out) -> u8 { x }";
    // The question itself needs the deep type.
    overlap_needs_where_clause: Limit, "trait Tr {}\nimpl Tr for W<{D}> {}",
        "impl<T: Nest> Tr for W<T> where T::Out: Show {}", "";
    inherent_key_projection: Limit,
        "struct Pair<A, B> { a: A, b: B }
impl<X> Pair<X, bool> { fn m(self) -> u8 { 1 } }",
        "impl<T: Nest> Pair<T, T::Out> { fn m(self) -> u8 { 2 } }",
        "fn f(p: own Pair<{D}, bool>) -> u8 { p.m() }";
    // Effect providers: a provider over the limit may be the one in scope.
    effect_keyed: Limit, LOGGER, DEEP_LOGGER,
        "fn f(d: own Deep<{D}>) -> u8 {
    with (Logger = Console {}) {
        with (Logger = d) {
            needs_logger()
        }
    }
}";
    effect_unkeyed_inner: Limit, LOGGER, DEEP_LOGGER,
        "fn f(d: own Deep<{D}>) -> u8 {
    with (Console {}) {
        with (d) {
            needs_logger()
        }
    }
}";
    effect_unkeyed_only: Limit, LOGGER, DEEP_LOGGER,
        "fn f(d: own Deep<{D}>) -> u8 {
    with (d) {
        needs_logger()
    }
}";
    effect_unkeyed_same_frame: Limit, LOGGER, DEEP_LOGGER,
        "fn f(d: own Deep<{D}>) -> u8 {
    with (Console {}, d) {
        needs_logger()
    }
}";
    // Effect handles: the handle traits are proved one after the other.
    effect_handle_refuted_after_limit: Same, DEEP_HANDLE,
        "impl<T: Nest> EffectRef<u8> for Deep<T> where T::Out: Show {}",
        "fn f(d: own Deep<{D}>) -> u8 uses (outer: mut u8) {
    with (d) {
        needs_mut()
    }
}";
    effect_handle_mut_limit: Limit, DEEP_HANDLE_REF,
        "impl<T: Nest> EffectRefMut<u8> for Deep<T> where T::Out: Show {}",
        "fn f(h: own Deep<{D}>) -> u8 {
    with (u8 = h) {
        needs_mut()
    }
}";
    // Effect providers: the provider named as the requirement is, among
    // others over the limit, however many.
    effect_named_among_many: Same, LOGGER, DEEP_LOGGER,
        "fn f(logger: own Console, d0: own Deep<{D}>, d1: own Deep<{D}>, d2: own Deep<{D}>, d3: own Deep<{D}>, d4: own Deep<{D}>, d5: own Deep<{D}>, d6: own Deep<{D}>) -> u8 {
    with (logger, d0, d1, d2, d3, d4, d5, d6) {
        needs_logger()
    }
}";
    effect_named_unknown_among_many: Limit, LOGGER, DEEP_LOGGER,
        "fn f(logger: own Deep<{D}>, d1: own Deep<{D}>, d2: own Deep<{D}>, d3: own Deep<{D}>, d4: own Deep<{D}>, d5: own Deep<{D}>) -> u8 {
    with (logger, d1, d2, d3, d4, d5) {
        needs_logger()
    }
}";
}

