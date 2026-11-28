# Named implementations

A trait implementation can have a selection name:

```fe
struct Item {}
trait Price { const fn price() -> u256 }
impl Price for Item { const fn price() -> u256 { 100 } }
impl Price for Item as Wholesale { const fn price() -> u256 { 70 } }

const fn price<T: Price>() -> u256 { T::price() }
const fn ordinary_helper() -> u256 { Item::price() }
const fn chosen() -> u256 { with (Wholesale) { price<Item>() } }
```

`chosen()` returns 70. A named implementation is unavailable to ordinary
implicit selection. Its name is private unless declared with `pub impl`;
normal lexical scopes, imports and module visibility apply to the name.
Implementation names have their own namespace, so a name can also denote a
value. Two implementations in one module, or in one block of a body, cannot
share a name. A `with (Name)` or `with (m::Name)` whose path is both an
implementation and a value in scope, such as a constant or a local binding, is
rejected as ambiguous. An implementation whose name is not visible where the
operand is written does not hide a visible value of the same name.
A local named implementation inherits its enclosing generic parameters; its
explicit parameters follow them and can be supplied as `Name<Args>`.
`Copy`, `Drop`, `Snapshot`, `Pinned` and `ErrorVariant` cannot have a named implementation: the compiler uses whether
a type implements them, never a choice made with `with`, so the declaration is
rejected.

A `with` operand adds a candidate for every matching trait goal, including
conditions of other implementations. The nearest matching operand wins: an
inner `with` overrides an outer one. One `with` gives each trait goal at most
one implementation, so two operands that could answer the same goal, in
either order and even if they name the same implementation, are rejected.
The compiler also rejects openings in one list when unresolved associated
types prevent it from proving that their headers are disjoint. Use separate
nested `with` blocks to give the openings an explicit priority.
The chosen implementation's conditions must hold: failure does not fall back
to an ordinary implementation. Leaving the block restores the enclosing choices.
An unapplied generic name can match any application of its header;
`with (Name<T>)` restricts it to the written arguments.
Header matching uses the associated types of those arguments. For example, if
`T::Out` is `u16`, the header `Pick<T::Out>` matches `Pick<u16>`. A condition
requiring `T::Out = u8` is checked after that match. A call to `Pick<u8>` can
still use its ordinary implementation.
Trait headers compare the never type `!` only with itself. Expressions of
type `!` still coerce in expression positions.

Calls pass proofs only for the bounds the callee declares. Thus a call to
`ordinary_helper()` inside `with (Wholesale)` still returns 100. A generic
body with both `T::price()` under `T: Price` and a concrete `Item::price()`
observes 70 and 100 respectively when called as `body<Item>()` in that block.
Moving an operation into a helper with different bounds can change which
implementation it uses. Collection operations such as hashing and comparison
follow the same rule: only the declared dictionaries cross a call boundary.

Trait-keyed effect providers carry their declared dictionaries too. A provider
supplied under a named implementation uses that implementation for operations
through its declared trait bound. This holds whether the provider and
implementation appear in one `with` or either enclosing block, and when
another function forwards the same effect. An unbounded helper that supplies
its own provider still uses its ordinary dictionary.

A function passed as a value to a callable bound, as in `run(helper<u256>)`,
carries proofs of its own bounds made where it is passed, as the closure
`|| helper<u256>()` would.

## Supertraits: one dictionary per bound

An implied supertrait dictionary comes from its subtrait's proof. An unrelated
lexical override does not replace it. Conditions used to construct that proof
retain their own chosen dictionaries. For example, extending the declarations
above:

```fe
trait Sub: Price {
    const fn extra() -> u256 { Self::price() + 1 }
}
impl Sub for Item {}
const fn via_sub<T: Sub>() -> u256 { T::price() * 1000 + T::extra() }
```

`with (Wholesale) { via_sub<Item>() }` returns 100101: its two operations
observe 100 and 101. The ordinary `Sub` proof carries its ordinary `Price`
dictionary. A conditional subtrait implementation instead projects through
the actual dictionaries used to satisfy its conditions.

Every occurrence of the same bound in a callee's signature must receive the
same dictionary. Consistent duplicates are accepted. If two subtrait proofs
project incompatible dictionaries for one supertrait, the call is rejected.
This also applies when the supertrait is explicitly repeated: a call under
`Wholesale` to `fn repeated<T: Sub + Price>()` is rejected for `Item`, because
`Sub` implies the ordinary `Price` proof while the explicit `Price` bound
selects `Wholesale`. Choosing either silently would make dispatch depend on
bound order. Distinct, explicitly qualified evidence paths are deferred.

Agreement is between routes to one goal of the callee as its signature
declares it. Two bounds that the signature does not identify are two goals,
even when a call makes their types equal, and may receive different
implementations:

```fe
trait Holder { type Out: Pick }
impl Holder for Fixed { type Out = Item }
fn target<H: Holder<Out = Item>, T: Pick>() -> u256 {
    <H::Out as Pick>::value() * 10 + T::value()
}
```

Here `Pick::value` is 1 for `Item` and 2 for the named implementation `Alt`.
`with (Alt) { target<Fixed, Item>() }` returns 12: `T: Pick` is chosen at
the call, and the `Item: Pick` that `H` implies is fixed where
`impl Holder for Fixed` is declared. Written as `H: Holder<Out = T>`, the
signature makes the two one goal, and the same call is rejected.

The same agreement applies transitively through implementation conditions,
including private prerequisites of an implementation whose public trait has
no supertraits. Calls, implementation methods and associated-constant bodies
receive projections of those actual prerequisite dictionaries. A constant
default cannot bypass a disagreement that would reject the equivalent call.

A bound that a trait declares on an associated type, such as `type Cursor: Copy`,
is implied in the same way: its dictionary comes from the proof of the
implementation that defines the type, made where that implementation is
declared, with the actual dictionaries of its conditions.

A dictionary also holds the dictionaries of the bounds on its own associated
types, proved where its implementation is declared. When an associated type
leads back to the same bound, that bound is reached twice. With
`trait Chain { type Next: Chain }` and `impl Chain for End as E2 { type Next = End }`,
a function `fn g<H: Holder<Out = End>>()` called as `with (E2) { g<Bx<End>>() }`
receives `End: Chain` both as `E2` and as the ordinary implementation that `E2`
holds for `Next`, and the call is rejected. Receiving the bound through a type
parameter, as `fn f<T: Chain>()` called as `with (E2) { f<End>() }`, is accepted
and uses `E2`.

Bounds on associated types are followed for four levels below a bound that an
equality makes concrete, each level fixed by the implementation that defines
it. If a dictionary that holds a named implementation has bounds below that
limit, the call is rejected, even when only one route reaches them. A level
whose bounds lead only to bounds already reached is followed completely.

## Opening the ordinary implementation

A named implementation's methods see their own implementation. A default
method that a named implementation keeps is checked with the trait, so
`Self::price()` in its body calls the named implementation while a concrete
`Item::price()` calls the ordinary one. A `with` block inside a named
implementation's method is nearer than the implementation itself:
`with (V) { Self::price() }` there calls `V`. A wrapper can explicitly open the
ordinary implementation using a qualified trait path:

```fe
impl Price for Item as Markup {
    const fn price() -> u256 {
        with (<Item as Price>) { Item::price() + 1 }
    }
}
```

`with (<Type as Trait>)` is the first-version spelling for opening an ordinary
implementation. `with (Markup) { Item::price() }` therefore returns 101 without
recursing into `Markup`. The header `Type: Trait` alone selects the
implementation, and exactly one ordinary implementation must match it. Its
conditions are proved where the operand is written, with the bounds in scope,
the enclosing choices and the other operands of the same `with`, so the order
of the operands does not matter, and an opening whose conditions cannot hold
is rejected even when the block does not use it. Once opened, the
implementation is a candidate like a named one. This operand does not clear
the whole choice scope or change which dictionaries cross a call boundary.

## Associated outputs and types

A named implementation of a trait with associated types, associated effect
rows or associated constants requires a unique, source-declared ordinary
implementation covering its header under its declared conditions. Named
implementations must agree with other source-declared implementations, ordinary
and named, on these outputs wherever their headers overlap, including where
an unresolved associated type or constant leaves open whether they do. Output
agreement does not depend on which implementation conditions happen to hold at
an opening. Constants compare using the compiler's existing constant normalization;
for example, `10` agrees with `5 + 5`, but not with `11`.

Agreement is also checked where a choice answers a concrete goal, including an
ordinary implementation whose conditions hold only through a choice and
implementations in other ingots: the implementation that answers must give the
outputs that ordinary normalization gives the goal, and a goal with outputs
needs an ordinary implementation there. A use that breaks this is rejected.

```fe
trait Size { const N: usize }
impl Size for Item { const N: usize = 3 }
impl Size for Item as Sized { const N: usize = 3 }
```

These declarations are accepted. Changing the named `N` to `4` is rejected;
removing the ordinary implementation is also rejected. A named implementation
of a trait containing only methods needs no ordinary counterpart.

The ordinary implementation supplies the output representative used by type
normalization. Agreement only at overlapping headers would leave named-only
applications without that representative: a bound such as `T: Size` could be
proved while `[u256; T::N]` could not normalize. Compiler-provided ordinary
implementations do not yet qualify as output representatives or participate
in this comparison. Extending this boundary requires corresponding output
comparison support or normalization through evidence.

An ordinary derive header written in the package can supply the representative
after its provider has prepared the associated members. A named implementation
must agree with those prepared outputs.

A written type whose formation reads a chosen implementation is rejected until
type normalization consumes evidence. Types formed before the block can still
be used there, and an unbounded concrete helper retains its ordinary behavior
in a type constant. This restriction also covers const methods used to form
types, even though their return values may differ between implementations,
type aliases, and a constant argument of a written type that opens its own
`with` block. A const helper can select an implementation inside its own
function body. A type constant can use that helper's result with ordinary
arguments and bounds.

## Choosing for a whole module

A module can name the implementations its code uses once, in a `with`
declaration:

```fe
struct Item {}
trait Price { const fn price() -> u256 }
impl Price for Item { const fn price() -> u256 { 100 } }
impl Price for Item as Wholesale { const fn price() -> u256 { 70 } }
impl Price for Item as Clearance { const fn price() -> u256 { 30 } }

mod shop {
    use super::{Clearance, Item, Price, Wholesale}
    with (Wholesale)

    pub const fn quote() -> u256 { Item::price() }
    pub const fn sale() -> u256 { with (Clearance) { Item::price() } }

    pub mod back {
        use super::super::{Item, Price}
        pub const fn quote() -> u256 { Item::price() }
    }

    pub mod outlet {
        use super::super::{Clearance, Item, Price}
        with (Clearance)
        pub const fn quote() -> u256 { Item::price() }
    }
}

const fn plain() -> u256 { Item::price() }
```

With a `#[test]` function added that asserts `plain() == 100`,
`shop::quote() == 70`, `shop::sale() == 30`, `shop::back::quote() == 70` and
`shop::outlet::quote() == 30`, the file passes under `fe test prices.fe`.

The declaration is the outermost choice frame of every body written in the
module and in its child modules, child files included. Each such body is
checked as if it were wrapped in `with (Wholesale) { .. }`. The nearest frame
answers a goal: the declaration of a child module, then a `with` block, then,
inside the methods of an implementation, that implementation's own entry, which
is nearer than the module's frame but farther than a `with` block in the
method. A function item written inside a body is a body of the module and gets
the frame; a `with` block does not reach items nested inside it. Importing a
module brings its names and not its frame. A module has one declaration, and it
can stand anywhere among the items.

The frame covers functions and methods, the default methods of a trait (in the
module of the trait), the initializers of named, associated and inherent
constants, the defaults of a trait's constants (in the module of the trait),
`static_assert` conditions, `where` conditions on functions, `init` and `recv`
bodies of contracts, closures, and the members that a derive header generates.
It does not cover constants written inside a type, nor `where` conditions on a
struct, an enum, an implementation or a trait: those are facts about a type and
use the ordinary choice.

Operands are implementations only: a named implementation (`Name`,
`path::Name`, `Name<Args>`) or an ordinary one opened as `<Type as Trait>`.
Names are looked up in the scope of the module, with the visibility rules of a
block operand. A value, an effect provider or a type is refused with 2-0021
(`with (Item)` reports that `Item` is a type, not an implementation).
Values and providers are written in a `with` block. An operand whose
implementation needs a value that is checked under its own module's frame is
refused at the operand (2-0022). As in a block, a choice is a candidate and
never an assumption: a chosen implementation whose conditions fail is an
error, and two operands that could answer one goal are refused.

A module's `with` does not change an associated type: a named implementation
that would change one is refused (5-0005). A constant is one value, fixed under
the frame of the module that declares it. A type that names a constant whose
value reads a frame's choice is refused (8-0138), whichever module writes the
type and whether the constant is named directly, in braces, through another
constant, or as an associated or inherent constant. A constant function called
inside a type runs under the frame of its own module, so such a type can follow
that frame, as the `sealed` example shows; the block form behaves the same.
A constant function may read the frame, and its value is the one it returns:

```fe
struct Array<const N: u256> {}

mod shop {
    use super::{Array, Item, Price, Wholesale}
    with (Wholesale)

    pub const BASE: u256 = Item::price()
    pub const fn base() -> u256 { BASE }

    pub fn sealed(a: Array<{ base() }>) -> Array<70> { a }
    pub fn refused(_ a: Array<BASE>) {}
}
```

`sealed` is accepted, and `refused` reports 8-0138 at `BASE`. An
implementation header such as `impl Size for Array<{ BASE }>` is a type like
any other, so the same rule applies to it.

A header's requirements are proved at the header, under its module's frame.
The bound on an associated type of an implementation declared in a framed
module, and the provider bounds of a derive header, are proved with that
module's choices, whoever reads them:

```fe
trait Holder { type A: Show }

mod framed {
    use super::{Foo, Holder, Loud, Show}
    with (Loud)

    pub struct Slot {}
    impl Holder for Slot { type A = Foo }
}

const fn shown<H: Holder>() -> u256 { <H::A as Show>::show() }
```

Here `Show` is a trait with `const fn show() -> u256`, `Foo` implements it
with 100, and `Loud` is a named implementation for `Foo` with 70.
`shown<framed::Slot>()` returns 70 even though `shown` is written outside the
module, and `Foo::show()` outside the module still returns 100. A closed
projection written outside the module,
`<<framed::Slot as Holder>::A as Show>::show()`, reads the ordinary
implementation and returns 100.

Some code is generated from the program, and a frame does not reach all of
it. The `Drop` implementation of a type is a fact of the type. A panic
payload and a hash computed for a written expression receive no choice. The
entry code of a contract is planned under the frame of the module that holds
the contract: the decoding of `init` arguments, the reading of each message and
the encoding of each return value. `fe check` and `fe build` read the same
plan, so a chosen implementation whose prerequisite nothing satisfies is
refused at the arm with 16-0024 in both. A contract in a module that
declares `with (Twice)`, where `Twice` is a named `Encode<Sol>` for the return
type, encodes its return value with `Twice`; the same contract in a module
without the declaration uses the ordinary encoder.

Limits:

- The frame is built from names alone, before any constant of the module is
  evaluated. An operand is therefore chosen when its trait has a method of
  the called name and its self type can match, and it is refused if that
  choice needs the calling constant. With `with (<Array<70> as Show>)` in a
  module that declares `const K: u256 = Array<70>::show()` and
  `impl Show for Array<{ K }>`, the operand is refused with 2-0022, and the
  header with 8-0138.
- A default declared in a module without a declaration, and used in a
  signature of a framed module, takes the ordinary value: the default
  `Array<const N: u256 = { Item::price() }>` is `Array<100>` in the signature
  of a function of `shop`, not `Array<70>`.
- The language server does not resolve the names in a module's `with`: hover,
  go to definition and rename do not apply to them, and the formatter keeps
  the operands on one line.

## Indexed storage

A storage entry uses the `StateIndex::locate` dictionary proved where the
indexing operation is checked. Its prerequisites follow the same choice rules
as written calls. Thus a named `StorageKey` can send a key to a different map
location, including through a generic method's declared bound. Packed entries
also retain the selected `LaneCodec::get` and `set` dictionaries. If a selected
codec's prerequisites fail, packed access reports that failure instead of using
an ordinary codec. Whole-slot access does not require unused codec methods.
An ordinary codec obligation that depends on a generic element can be completed
after substitution; this completion preserves any enclosing selected dictionary.

For example, write 5 through the ordinary key encoding and 6 through a named
encoding that adds 1000 to the key. Reading through those respective encodings
returns 5 and 6. The choice changes the indexing computation; associated layout
outputs still obey ordinary-output agreement. It does not move a contract's
field allocation or change the entry's type, address space, or lane width.

The unsafe `StateIndex` contract separates distinct keys within one dictionary.
It does not promise that locations from different dictionaries are disjoint.
The access checker conservatively treats such entries as potentially overlapping,
even when their literal keys differ.

Implementation names are compile-time operands of `with`. This feature does
not add public evidence values or a `prove` expression.

A trait family is written `trait Name` or `trait Name<Args>`, where the
arguments are those of the trait after its subject. It names the trait with its
subject left open and has kind `* -> Constraint`. Applying it to a type `T`, as
`P<T>` for a parameter `P: * -> Constraint`, gives a type of kind `Constraint`:
the goal that `T` implements the trait. A parameter of that kind is declared
`G: (Constraint)`. These types describe goals and have no values, so no
constant or function parameter can have one. A default that does not depend on
the subject is part of the family, so `trait Fixed` and `trait Fixed<4>` are
the same family when `4` is the default of `trait Fixed<const N: usize = 4>`.
