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
A local named implementation inherits its enclosing generic parameters; its
explicit parameters follow them and can be supplied as `Name<Args>`.

A `with` operand adds a candidate for every matching trait goal, including
conditions of other implementations. The nearest matching operand wins.
Its conditions must hold: failure does not fall back to an ordinary
implementation. Leaving the block restores the enclosing choices.
An unapplied generic name can match any application of its header;
`with (Name<T>)` restricts it to the written arguments.

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

The same agreement applies transitively through implementation conditions,
including private prerequisites of an implementation whose public trait has
no supertraits. Calls, implementation methods and associated-constant bodies
receive projections of those actual prerequisite dictionaries. A constant
default cannot bypass a disagreement that would reject the equivalent call.

## Opening the ordinary implementation

A named implementation's methods see their own implementation. A wrapper can
explicitly open the ordinary implementation using a qualified trait path:

```fe
impl Price for Item as Markup {
    const fn price() -> u256 {
        with (<Item as Price>) { Item::price() + 1 }
    }
}
```

`with (<Type as Trait>)` is the first-version spelling for opening an ordinary
implementation. `with (Markup) { Item::price() }` therefore returns 101 without
recursing into `Markup`. The qualified operand must resolve uniquely to an
ordinary implementation without lexical choices or bound assumptions. Once
opened, that implementation is a candidate in the surrounding solve context:
its conditions still use applicable choices. This operand does not clear the
whole choice scope or change which dictionaries cross a call boundary.

## Associated outputs and types

A named implementation of a trait with associated types, associated effect
rows or associated constants requires a unique, source-declared ordinary
implementation covering its header under its declared conditions. Named
implementations must agree with other source-declared implementations, ordinary
and named, on these outputs wherever their headers overlap. Output agreement
does not depend on which implementation conditions happen to hold at an
opening. Constants compare using the compiler's existing constant normalization;
for example, `10` agrees with `5 + 5`, but not with `11`.

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
proved while `[u256; T::N]` could not normalize. Compiler-generated ordinary
implementations do not yet qualify as output representatives or participate
in this comparison. Extending this boundary requires corresponding output
comparison support or normalization through evidence.

A written type whose formation reads a chosen implementation is rejected until
type normalization consumes evidence. Types formed before the block can still
be used there, and an unbounded concrete helper retains its ordinary behavior
in a type constant. This restriction also covers const methods used to form
types, even though their return values may differ between implementations.

## Indexed storage

A storage entry uses the `StateIndex::locate` dictionary proved where the
indexing operation is checked. Its prerequisites follow the same choice rules
as written calls. Thus a named `StorageKey` can send a key to a different map
location, including through a generic method's declared bound. Packed entries
also retain the selected `LaneCodec::get` and `set` dictionaries.

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
not add public evidence values, a `prove` expression, or a `Constraint` kind.
