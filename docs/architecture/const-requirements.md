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

Const conditions in generic scopes are rejected, including conditions that
happen to look constant and a trait's implicit `Self` scope.

A predicate that is a lone path naming a type, as in `where T`, is almost
always a missing trait bound, so it is reported as one.
