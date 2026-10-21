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

An implied supertrait dictionary comes from its subtrait's proof. An unrelated
lexical override does not replace it. Conditions used to construct that proof
retain their own chosen dictionaries. If two declared subtrait proofs project
incompatible dictionaries for one supertrait, the call is diagnosed.

A named implementation's methods see their own implementation. A wrapper can
explicitly open the ordinary implementation using a qualified trait path:

```fe
impl Price for Item as Markup {
    const fn price() -> u256 {
        with (<Item as Price>) { Item::price() + 1 }
    }
}
```

The qualified ordinary reference requires an ordinary implementation of that
goal; it does not refer to the enclosing named implementation.

A named implementation that supplies associated outputs also requires an ordinary
implementation covering its header under its declared conditions. This gives
type normalization an ordinary output representative until it consumes evidence.
Named implementations must agree on associated types, associated effect rows
and associated constants wherever their headers overlap. Constants compare
using the compiler's existing constant normalization. A written type whose
formation reads a chosen implementation is rejected until type normalization
consumes evidence. Types formed before the block can still be used there, and
an unbounded concrete helper retains its ordinary behavior in a type constant.

Implementation names are compile-time operands of `with`. This feature does
not add public evidence values, a `prove` expression, or a `Constraint` kind.
