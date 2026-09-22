# Const requirements

A `where` clause can hold boolean conditions next to its type bounds. A
predicate without `:` is a const condition and must have type `bool`:

```fe
const LIMIT: u256 = 8
const fn allowed(_ size: u256) -> bool { size < LIMIT }

fn operation() where allowed(3), !false {}
```

Type and trait predicates keep their existing syntax and meaning. Parenthesize
a block condition, as in `where ({ ... })`, to distinguish it from the item's
body.

## Ground conditions

A condition that mentions no generic parameter is checked at its declaration,
even if nothing uses the item. Only evaluation to `true` succeeds. Type errors,
non-const operations, failed execution, recursion and exhausted evaluation
limits are errors. Conditions go through ordinary semantic borrow and layout
checking as anonymous const bodies. They add no solver assumptions and do not
filter impl candidates.

Generic functions and records support conditions on their parameters, as
described below. Const conditions in generic enums, traits, impls and
associated functions are rejected, including a trait's implicit `Self` scope.

A predicate that is a lone path naming a type, as in `where T`, is almost
always a missing trait bound, so it is reported as one.

## Generic functions

A top-level generic function can state conditions on its parameters:

```fe
const fn bounded<const N: usize>() -> u256 where N > 0 { 42 }

const fn forward<const COUNT: usize>() -> u256 where COUNT > 0 {
    bounded<COUNT>()
}

const fn answer() -> u256 { forward<1>() }
```

The declaration checks each condition's type and that it uses only const
operations. Each use of the function discharges its conditions after
inference, so `bounded<0>()` is rejected even though the body never reads `N`.
Function values and calls inside constant initializers, array lengths and other
anonymous constant bodies use the same check. A ground condition on an unused
generic function is still checked at its declaration; conditions that mention
parameters become obligations of the callers.

A concrete condition is evaluated with ordinary compile-time evaluation and its
limits. A generic caller discharges a condition only by stating the same
condition itself: after substituting the call's arguments, the two resolved,
typed expressions must be identical. Parameter names do not matter; declaration
identities, parameter positions, operations and arithmetic mode do. Forwarded
expressions may use literals, const paths, unary and binary operations, casts
and calls to const functions. This is exact forwarding, not implication:
`N > 1` does not establish `N > 0`. Blocks and control flow can be evaluated
with concrete arguments but cannot be forwarded. A requirement cannot
establish itself.

Anonymous constants in an ordinary function's signature or body can forward
that function's conditions. A predicate cannot use any of the function's
conditions to justify itself, including through constants nested inside it,
whatever the clause order. Nested declarations do not inherit the function's
conditions. For example, a constrained helper can compute an array length in a
signature:

```fe
const fn empty_length<const N: usize>() -> usize where N > 0 { 0 }
const fn consume<const COUNT: usize>(
    _ values: [u8; { empty_length<COUNT>() }]
) -> u256 where COUNT > 0 { 42 }
const fn answer() -> u256 { consume<1>([]) }
```

The matching condition permits the helper call in the signature, and
`consume<0>([])` still fails even though the helper returns zero for every
input.

Inference and requirement discharge are separate queries, so evaluation can
read a completed inference while a condition is being checked. Neither
inference results nor evaluation results certify that a program meets its
requirements; the compiler's diagnostics do. The checks are shared front-end
logic, and no backend has its own evaluator for them.

## Generic records

A generic struct can state conditions. Every concrete use must satisfy them,
including construction, unused type annotations, defaults, aliases, fields and
trait arguments. A generic use needs an exactly matching condition after
substitution, as with generic function calls.

```fe
struct Bounded<const N: usize> where N > 0 { value: u256 }

const fn make<const COUNT: usize>() -> Bounded<COUNT> where COUNT > 0 {
    Bounded { value: 42 }
}
const fn answer() -> u256 {
    let item = make<1>()
    item.value
}
```

`Bounded<0>` is rejected even when no value of it is constructed. Mentioning
`Bounded<N>` does not establish `N > 0`: a generic function that uses the type
states the matching condition. A containing record can state its own condition
and forward it into its fields. A predicate cannot use the record's conditions
to justify itself, including in nested anonymous constants.

Declared type positions and inferred body types use the same checker; inferred
types are checked after inference. An unmet condition is reported once, where
the type enters: at the innermost written type, or at the expression that
instantiates it. An enclosing type, a call's generic arguments, and a binding,
block, or branch that only carries the type do not report it again.

A record with conditions must be fully applied where it is used as a type.
Passing the unapplied constructor through a higher-kinded parameter is
rejected, because nothing would carry its conditions to later applications.
Fully applied aliases work, and a generic alias cannot acquire or drop the
record's obligations.
