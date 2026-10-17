# Iteration

A `for` loop traverses a collection by a cursor, a value the loop holds,
and projects each element from the collection as it goes.

```fe
for x in xs { total += x }              // a copy of each `Copy` element
for ref a in accounts { total += a.n }  // a read access to each element
for var x in xs { x += 1 }              // a mutable copy of each element
for mut x in xs { x *= 2 }              // a `mut` access to each element
for (i, x) in xs by enumerate() { .. }  // a driver
for y in xs by map(|x: u256| x * 2) { .. }
```

The loop's pattern binds each element as a `let` pattern binds a
projection's result: a plain name copies a `Copy` element and is an error
for any other (bind it `ref`), except that a `#[view]` or state-only one,
which has no value to copy, binds its read access. The item pattern chooses
the traversal: a `mut` item calls `at_mut` and opens the base mutably, as a
`mut` binding opens a matched place, so the base needs no marker and
`for x in mut xs` is an error. A method chain traverses mutably the same
way: `for (i, mut x) in xs.enumerate()`.

`Collection` (`core::iter`) is the protocol: `start` and `next` move a
cursor, `at` projects the element at it. `CollectionMut` adds `at_mut` with
`mut` access for loops whose item is `mut`, and `Bidirectional` traverses in
reverse. Arrays, `MemArray`, `MemVec`, `MemSlice`, ranges, `SolArray` and
`SolEnumerableSet` are collections, and so is `DynArray`, whose loop decodes
each element as it reaches it; a collection declares the effects its
methods need (`uses E`) and the space of the places it yields (`space S`).

A loop that copies each item (a `Copy` element bound by a plain name or
`var`, or a producer's value) holds
no access on its base between steps, so the body may change the collection,
and `at` reads each element afresh. A projected or `mut` traversal holds the
element's session during the body, which then cannot change the base; so
does a `while let` whose condition opens a session, until the back edge.

## Drivers and producers

`for x in xs by d` traverses `xs` through the driver `d`, which keeps its own
state and never holds the collection. The standard drivers are
`reversed()`, `enumerate()`, `take(n)`, `skip(n)`, `step_by(k)` and
`filter(p)`. A driver that yields an extra value with each element, such as
`enumerate`, binds a pair: `for (i, x) in xs by enumerate()`. A producer,
such as `map(f)`, yields owned values instead of accesses.

Drivers combine as methods, `xs.reversed().enumerate().take(3)`, and a
method chain on a collection is sugar for `by`:
`for x in xs.filter(|x| x > 1)` is `for x in xs by filter(..)`.

A two-base driver traverses two bases in step: `for (x, y) in (a, b) by zip()`
views both until either ends, and `for (mut x, mut y) in (a, b) by zip()`
mutates both, which needs `a` and `b` disjoint. The items are both `mut` or
neither.

## Higher-order operations

Collections provide `fold`, `any`, `count` and `find`. `fold` owns its
accumulator and views each element, so neither needs to be `Copy`:

```fe
let totals = accounts.fold(Totals { sum: 0 }, |acc, account| {
    Totals { sum: acc.sum + account.balance }
})
```

`MemArray::collect(xs, map(f))` collects a producer's values into an array,
and `MemVec::collect` into a growable one.
See [closures](closures.md) for the callables these take.
