# Accesses and projections

Fe has mutable value semantics: every value has one owner, and a reference
is never a value. Code reaches another owner's data through an *access*,
which exists only as a parameter, a binding, or the result of a projection,
and ends at its last use.

## Parameter modes

```fe
fn total(_ xs: [u256; 4]) -> u256 { .. }    // view: read the caller's place
fn bump(_ x: mut u256) { x += 1 }           // mut: exclusive for the call
fn consume(_ item: own Item) { .. }         // own: the callee takes the value
```

A view or `mut` argument is a place of the caller's (`bump(mut counter)`);
an owned argument is moved, or copied if its type is `Copy`. A projection's
`mut` result is marked like a place (`bump(mut xs.at(0))`), unless its
type has no value, as a view's does (`fill(buf.span())`).

A parameter may be a pattern, written after `_` since it has no label:
`fn sum(_ (a, b): (u256, u256))`. The body binds its names as a `let` of the
parameter would, so from a view of a tuple a `Copy` part is copied and any
other part must be bound `ref` (`_ (i, ref x): (usize, Buf)`). A function without
a body, such as a trait method's declaration, names its parameters.

A parameter may also take several accesses at once as a *product*: a tuple
whose parts carry their own modes, as in `_ (i, ref x): (own usize, ref T)`.
A part without a mode is `ref`, and a `mut?` part takes its function's
mode. The argument is either a tuple of the parts, each written as an
argument of its part's mode (`bump((mut y, ref x))`), or a projection's
result that grants them (`swap(p.both())`). The parts are accesses of the
call, as separate arguments are. So `add_to((mut x, ref x))` is rejected, as
is a part that overlaps another argument or an effect, while
`add_to((mut p.a, ref p.b))` is accepted. No single name holds a product:

- a function with a body destructures it;
- the parts carry its modes, so `own (own u256, ref u256)` is an error;
- a receiver cannot be one.

A closure's parameter may be a product too, and a function taking one is a
callable value of that shape (see [closures](closures.md)).

A view argument that is a scalar or a handle (an integer, `bool`,
`Address`, a raw pointer, or any `core::marker::Snapshot` type) is a
*snapshot*: the callee gets a copy, and the caller may change the original
during the call. Every other value, including `Copy` arrays and structs and
storage collections, is viewed in place: the caller cannot write it while the
call, or a projection's result over it, is in use.

```fe
fn first(_ xs: [u256; 4]) -> ref u256 { ref xs[0] }

let x = ref first(xs)
xs[1] = 5        // rejected: `x` views `xs` in place
```

## Bindings

`let` binds an immutable value, and `var` a mutable one. An access is
written: `let h = mut p.x` and `let r = ref p.x` bind an access to a place,
with the marker on the initializer. Only a type with no value, a `#[view]`
such as `MemSlice` or a state-only type such as a storage collection, binds
its access under a plain name ([Projections](#projections), [Views](#views)). An access is open from the binding to
its last use, and while it is open no other access may overlap it unless
both read: `p.x` cannot be read while `h` is open, nor written while `r` is.
`let mut x = …` is not Fe: write `var x = …` for a mutable value, or
`let x = mut p` for a mutable access.

```fe
var total = 0          // a mutable value
let h = mut p.x        // a mutable access to `p.x`
let r = ref p.y        // a read access to `p.y`
```

In a pattern, a binding's marker sits on its name: `x` takes a value,
`var x` a mutable value, `ref x` a read access and `mut x` a mutable access.
A pattern matched against a place opens each marked binding's access on that
binding's own part of the place, and nothing else: in
`match p { Pair { ref a, mut b, .. } => … }`, `p.a` is held for reading and
`p.b` for writing, and `p.c` stays free. `mut x` needs a mutable place or
access behind it, and `ref` or `mut` on an owned value is an error. Markers
go on the names only: `match ref x`, `if let Some(v) = mut o` and
`let (a, b) = mut t` are errors, fixed by marking each name
(`match x { V(ref v) => … }`). A marker on the scrutinee would hold all of
it, or make plain names write through.

A plain name takes its value as `let x = e` does: a copy of a `Copy` value,
and otherwise a move where one is possible. A pattern on a place moves only
what its names take, so `match s.status { Status::A => …, _ => … }` leaves
`s.status` whole however often it is matched. A move out of an owned place
leaves it partially moved; a move through a `mut` access leaves a hole that
must be restored through that access before it closes:

```fe
fn take(_ x: mut Option<MemBuffer>) -> Option<MemBuffer> {
    let v = x                 // moves out, leaving a hole in `x`
    x = Option::None          // restores it
    v
}
```

`core::mem::replace(mut x, v)` and `core::mem::take(mut x)` do this in one
call, as does `x.take()` on an `Option`.

A value reached through a view or a `ref` access, through an effect, or in
storage, cannot move; bind it `ref` to read it in place. An effect lends its
place: a non-`Copy` value under it is read in place or passed on to a callee's
effects, never taken.

An owned parameter is immutable unless declared `var x: own T`, and an owned
receiver unless declared `var own self`. An access
cannot be stored in a field, in storage, passed as `own` or returned from an
ordinary function. `ref` and `mut` do not appear in types.

## Projections

A function whose result is `ref T`, `mut T`, or a shape with such a
component, is a projection: it grants its caller an access.

```fe
fn first(mut self) -> mut T { mut self.items[0] }

fn entry(mut self, _ key: u256) -> mut u256 {
    var tmp = self.get(key)
    yield mut tmp
    self.set(key, tmp)
}
```

A projection's result binds like a place. A plain name takes a value: a
`ref` result of a `Copy` type is copied, and the access ends at once, so
`let v = xs[i]` is an owned value and `xs` may change while `v` lives. A
`ref` result of any other type is an error whose fix is `let v = ref xs[i]`,
which keeps the access; `(i, ref x)` and `for ref x in xs` do the same for a
component and an element. Whether a type is `Copy` is decided by the
body's declared bounds, never by an instance. A whole `mut` result is bound
`mut` as well: `let x = mut v.first()` holds the access, and a plain
`let x = v.first()` is an error, since its value would discard the writes
the call asks for; `var x = v.first()` copies the element and warns,
since writes to `x` reach nothing. The components of a `mut` result
take values as a place's do, so `Some(mut v)` writes through and a plain
`Some(v)` is an immutable copy. A type with no value reading binds its
access under a plain name, `mut` for a whole `mut` result and a read access
otherwise: a `#[view]` result (`let s = buf.span()`) and a state-only one
(`let m = store.balances`); `let t = s` reborrows the access the same way.

Copying a result ends the projection's session at the binding, so its slide
runs there rather than at the binding's last use.

A projection yields once on each path that completes, implicitly at its
tail or with `yield`. The code after a `yield`, its *slide*, runs when the
caller's last use of the access has passed. While the caller uses the
access, the projection's session reserves the places its arguments named.

A yield of a value, such as `-> ref u32 { self.word.low() }`, grants a
temporary in the projection's frame holding it, as `let tmp = ..; ref tmp`
would. A `ref` yield of a place views the place; a `mut` yield grants a
place only when it is named `mut`, since `self.v` alone reads a copy. A
`mut` yield of a frame temporary with no slide to write it back is a
warning.

A shape groups owned values and accesses, `-> (usize, ref T)`, and the
caller destructures it with `let (i, x) = ..`. Two `mut` components must be
structurally disjoint, as in `(mut self.a, mut self.b)`. An optional shape,
`-> Option<mut T>` or `-> Result<E, mut T>`, is consumed with `if let`,
`match`, `let .. else` or `?`. Its empty variant grants nothing and runs no
slide: it may only be returned before the `yield`, and a `yield` of it,
written out or forwarded from another projection as in
`yield self.entry(k)`, ends the projection there, as `?` does.

Each access a projection grants lies in one address space, its *result
space*, which the compiler infers from the body for each instance. A place
lies where its path leads: a step into a pinned type moves it into the pin's
space, an entry into its collection's, and a packed field or entry is
reached through a memory copy, so its grants lie in memory. Every yield of a
component must lie in one space:

```fe
struct Guarded { value: u256, hits: TSlot<u256> }

impl Guarded {
    // rejected: the yields lie in storage and transient storage
    fn pick(mut self, _ c: bool) -> mut u256 {
        if c { mut self.value } else { mut self.hits.value }
    }
}
```

A tuple holds accesses only as such a shape. Accesses the checker cannot see
are disjoint form an *unsafe split*, an `unsafe` block whose tail is a tuple
of them: one session whose reservations are all of theirs, and whose grants
the block promises are disjoint. Its components are places, such as raw
pointer dereferences, or calls of `#[raw_place]` projections: a projection
declared `#[raw_place]` has nothing to resume after its yield, since no code
runs there, it keeps no access or session of its own open across it, and it
yields no place of its own frame, which is checked where it is written. Two
raw-slot collections, whose `at` constructors are raw places, are held at
once this way, and `MemArray::pair_mut` is one inside a library function:

```fe
let (mut a, mut b) = unsafe { (SolArray<u256>::at(x), SolArray<u256>::at(y)) }
a.push(1)                           // all storage stays reserved until both end
b.push(2)
```

## `mut?` items

A function whose signature writes `mut?` has a *mode*: it is used at `ref`
or at `mut`, and `mut?` is `ref` or `mut` accordingly, wherever `ref` and
`mut` may be written.

```fe
pub fn at(mut? self, _ index: usize) -> mut? T {
    unsafe { mut? *self.ptr_at(index) }
}

pub fn split_at(mut? self, _ mid: usize) -> (mut? Self, mut? Self) { .. }
```

Its body is checked once per mode: the `ref` body cannot write through a
`mut?` place, and the `mut` body keeps `mut`'s rules, such as the
disjointness of two `mut?` components. A call is at `ref` unless a use of
its result asks for `mut`: a `mut` receiver (`xs.at(i).bump()`), a `mut`
access or binding (`let x = mut xs.at(i)`, `Some(mut x)`), a `mut`
argument, an assignment (`xs.at(i) = 1`) or a `mut` yield. A `mut` binding
of any part of an unsafe split asks every access in it for `mut`, since the
split is one session. At `mut`, the
call's receiver is accessed mutably, and any other argument its `mut?`
parameters take must be written as a `mut` access, `f(mut p)`. Since only a
use of its result asks for `mut`, a `mut?` function's result must have a
`mut?` part; without one, its `mut` body would never run.

A trait method may be `mut?`. A bound says which modes a generic body may
call it at: `T: Collection` grants `at` at `ref`, and `T: mut Collection`
at both. An implementation grants `mut` when each of the trait's `mut?`
methods has a `mut` mode there: a `mut?` method, or a second method of the
same name that takes `mut` where the trait method takes `mut?`, for an
element its two modes compute differently:

```fe
/// Stores each element doubled.
struct Doubled {
    words: [u256; 4],
}

impl Index<usize> for Doubled {
    type Output = u256

    fn index(self, _ index: usize) -> ref u256 {
        self.words[index] / 2
    }

    fn index(mut self, _ index: usize) -> mut u256 {
        var value = self.words[index] / 2
        yield mut value
        self.words[index] = value * 2
    }
}
```

An implementation with only a `ref` method, such as a range's `at`, is no
`mut Collection`. A `mut?` bound (`D: mut? Driver`) is `mut` where its
item is used at `mut`: `impl<D: mut? Driver> Driver for Enumerate<D>` makes
`Enumerate<D>` a `mut Driver` exactly when `D` is one, and a
separate `mut` body of the implementation takes `D` as one. Only a function
or an implementation is used at a mode, so only their bounds may be `mut?`;
a struct's or a trait's bounds, including those of its associated types
(`type Item: mut Slot`) and of `Self`, are `mut` or plain.

Each body of a `mut?` function defines its own closures, and a `mut?` in
one, such as a parameter `c: mut? T`, is that body's mode.

An effect may be `mut?` as well: the `ref` body only reads it, and the `mut`
body may write it.

```fe
fn slot() -> mut? u256 uses (store: mut? Store) {
    mut? store.n
}
```

A call at `ref` needs only read authority, `uses (store: Store)`, and one at
`mut` needs the effect `mut`. The two bodies of an implementation's `mut?`
method take the trait method's `mut?` effect at their own modes. An effect
row (`uses E = (..)`) and a contract's `uses` have no mode, so each of their
effects is `mut` or read-only.

## Associated shapes

A trait may declare an associated shape, `shape Part`, which each
implementation gives as an owned value, an access, a product of them or
another shape, and which its methods may return:

```fe
trait Holds {
    shape Part

    fn part(mut? self) -> Self::Part
}

impl Holds for Pair {
    shape Part = (mut? u256, mut? u256)

    fn part(mut? self) -> (mut? u256, mut? u256) {
        (mut? self.a, mut? self.b)
    }
}
```

A shape is named at a mode, `H::Part<ref>` or `H::Part<mut>`, and only a
`mut?` item's own signature names it without one, at the item's mode; a
concrete type's is named the same way, `Pair::Part<ref>`. Only an
associated shape takes a mode argument. An implementation's shape keeps a
contract at each mode: its accesses are at most the mode, so an access that
is `mut` at `mut` is written `mut?`, and `shape Part = mut u256` is
rejected, as is a shape parameter, whose accesses no one knows; and a shape
a trait method returns, the trait's own or a super-trait's, has an access in
every implementation of that trait, so the method is a projection in each (a
traversal of owned values is a producer). A trait's default shape keeps the
same contract.

Before `H` is known, `H::Part<ref>` lists no parts. Generic code passes a
value of it on whole: it returns it, alone or as part of a returned
product, or passes it to a parameter of the same shape, `_ p: H::Part<ref>`,
at most once. A `let`, a pattern, a field read or a copy of it is an error.
A product's part, `(own usize, H::Part<ref>)`, a closure's parameter and a
callable bound, `F: Fn(H::Part<ref>) -> bool`, take one the same way. A
bound that fixes the shape at a mode makes its parts known there, to bind,
copy and destructure; each mode is fixed on its own:

```fe
fn bump<H: mut Holds<Part<ref> = (ref u256, ref u256), Part<mut> = (mut u256, mut u256)>>(
    _ h: mut H,
) {
    let (mut a, _) = h.part()
    a += 1
}
```

## Views

A type marked `#[view]`, such as `MemSlice`, describes memory its owner
holds. It has no value: no `own` parameter or owned result, no `Copy`, no
field of that type, and nothing for `var` to hold; a plain binding of one is
its read access. A span such as `buffer.span()` is a `ref` projection, so it
keeps the buffer reserved until its last use.

A view may also be accessed mutably. `span` is `mut?`, and taken at `mut`
it holds the buffer mutably until the span's last use; writes through the
span reach the buffer:

```fe
let s = mut buffer.span()
s[0] = 10
let (mut head, mut tail) = s.split_at(4)
for mut b in tail { b = 0 }
```

A view never leaves a hole: a move out of a `mut` access of one is rejected,
including inside generic code such as a `swap<T>` called with two spans, so
two spans cannot exchange the owners they point into. A projection that
yields a `mut` view must hold something mutably, a `mut` parameter or
effect.

## Unsafe fields

A field marked `unsafe` holds a value its type's safe methods trust, such as
an owner's pointer and length:

```fe
pub struct MemBuffer {
    unsafe ptr: *u8,
    unsafe len: u256,
    unsafe capacity: u256,
}
```

Initializing or writing an `unsafe` field takes an `unsafe` block or an
`unsafe fn` body: a literal that sets it, an assignment to it, or a `mut`
access to it, whether written `mut buf.len`, taken by a `mut self` method,
bound `mut` in a pattern, opened by a `for mut` loop or a match, or provided
by `with (buf.len)` to a call that uses it as a `mut` effect. Reading
one is safe, so an `unsafe` field may be `pub`: readable anywhere, writable
only in `unsafe` code. Moving or assigning a whole value is safe, since each
value carries its own invariant. The block is where the code vouches that the
new value keeps the type's promise:

```fe
pub fn truncate(mut self, _ len: u256) {
    if len > self.len {
        index_out_of_bounds()
    }
    // Shrinking keeps every byte of the extent initialized.
    unsafe { self.len = len }
}
```

Only struct and enum variant fields can be `unsafe`.

## Unsafe traits

A trait is declared `unsafe` when the compiler relies on a contract its
implementations keep without checking it, and each implementation is an
`unsafe impl` that asserts it; an `unsafe impl` of a safe trait is an error.
`core::ops::StateIndex` is one: the access checker trusts its `locate` to keep
entries inside the collection's region. The methods of an `unsafe impl` are
ordinary safe code, and calling them is safe. A dependency's `unsafe impl`
counts as unsafe code where it is used, as its `unsafe` blocks do, so it needs
`allow_unsafe = true`.

```fe
unsafe impl<K: StorageKey + Copy, V> StateIndex for StorageMap<K, V> {
    ..
}
```

## Indexing

`c[k]` has one syntax and two kinds of implementation. Indexing a fixed array
`[T; N]` is built in. A collection whose elements live in storage or
transient storage implements `core::ops::StateIndex` (below): it only says
where an entry lies, and the compiler opens the entry's place, so its users
need no raw storage authority. Any other collection implements
`core::ops::Index`, whose `index` is a `mut?` projection: its bodies may
check bounds, compute an element or write one back, and an element's access
reserves the whole collection until its last use. `c[k]` calls it at `mut`
where the context needs a mutable place, which needs `C: mut Index<K>`. A memory owner such as `MemArray` yields an element through its pointer
in an `unsafe` block; growing it takes `mut self`, so it cannot reallocate
under an open element access.

## Storage collections

A storage collection, such as a `StorageMap`, `StoragePackedArray` or
`TSlot`, is an owner at its storage place: the contract layout gives it a
slot, and its entries lie at slots derived from that one (a map's at
`keccak(key, slot)`). `m[k]` names an entry's place with no call
(`core::ops::StateIndex`), so `store.days[k].steps += 1` reads and writes one
slot. The access checker sees an entry as the collection's place indexed by a
key: entries at distinct literal keys are disjoint, and any other two
overlap. A `mut` access to a struct holding collections covers their
entries. A collection's methods take `self` or `mut self` and declare no
effects: the authority is the access to its place, as for any data.

A library type becomes a collection with an `unsafe impl` of
`core::ops::StateIndex` and by pinning itself (`core::marker::Pinned`,
below) to the state space its entries live in: `Key` and `Entry` are the
index and the element type, and `locate(root, key)` returns the entry's
`Location`, its slot and bit offset, for the collection rooted at slot
`root`. `locate` declares no effects; the compiler calls it once when an
access to `c[k]` opens and does the loads, stores and masking itself. It
must keep every entry inside the collection's own region, hashing per entry
or bounding the key, and distinct keys apart, which the `unsafe impl`
asserts. `std::evm::layout` has Solidity's derivations for it:
`mapping_slot(root, key)`, `array_data_slot(root)` and `element_location`,
so a mapping-like `locate` is
`Location { slot: mapping_slot(root, key), offset: 0 }`.

A collection may pack small elements into lanes of shared slots, as a
`SolArray` does for Solidity's packed array elements. Its `Lanes` codec
(`core::ops::LaneCodec<Entry>`) packs an element coded in at most 128
`BITS`; an element its codec does not pack keeps its storage layout, and
`locate` steps by `core::intrinsic::storage_slots<T>()`, the slots it takes.
`SolArray`'s codec, `SolLanes`, uses an element's `SolPacked` encoding, and
the compiler gives any other element `BITS` of 256 per slot its storage
layout takes. A
lane is read and written in place, but a `mut` binding, argument, provided
effect or yield of one works on a memory copy that is stored back into the
lane when the access ends.

```fe
let x = mut balances[from]
balances.set(key: to, value: 0)     // rejected: `x` holds an entry of `balances`
x -= amount
```

A type implementing `core::marker::Pinned` is *pinned* to its `Space`,
`core::marker::Storage` or `core::marker::Transient`, or another pinned
type's (`type Space = P::Space`):

```fe
impl<K, V> Pinned for StorageMap<K, V> {
    type Space = Storage
}
```

Implementing the trait is the whole declaration, so a library collection or
cell is pinned the same way. A type
holding a pinned type is *state-only*: never a value. It is not constructed,
copied, moved out of storage or held in a memory aggregate; code reaches it
through an access, a `mut` or view parameter, or an effect, and a plain
binding of one (`let m = store.balances`) is its read access. A place's space
follows its path: a field or element of a pinned type lies in the type's
space, so a `TSlot` and its value, `slot.value`, lie in transient storage
whatever holds the `TSlot`. A raw-slot constructor,
`unsafe fn at(slot) -> mut? Self uses (storage: mut? RawStorage)`, places a
collection at a runtime slot, read-only unless its use asks for `mut`
(`let a = unsafe { mut SolArray<u256>::at(0) }`), and its caller vouches that
nothing else uses that slot. A `with` block may install a provider naming storage only when its
place lies in one of the function's effects, under raw storage authority, or
in an `unsafe` block.

## External calls

A call that may start an external execution can reenter the contract and
access any of its storage, so it conflicts with every open storage access,
including those of the projections whose results the caller still uses.
Close storage accesses, or let their last use pass, before such a call.
