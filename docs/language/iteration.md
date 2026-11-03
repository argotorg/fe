# Iteration

A `for` loop traverses a collection by a cursor, a value the loop holds,
and projects each element from the collection as it goes.

```fe
for x in xs { total += x }              // a copy of each `Copy` element
for ref a in accounts { total += a.n }  // a read access to each element
for var x in xs { x += 1 }              // a mutable copy of each element
for mut x in xs { x *= 2 }              // a `mut` access to each element
for (i, x) in xs.enumerate() { .. }     // a driver's step: an index and an element
for y in xs.map(|x| x * 2) { .. }       // a producer
```

The loop's pattern binds each step as a `let` pattern binds a projection's
result: a plain name copies a `Copy` element and is an error for any other
(bind it `ref`), except that a `#[view]` or state-only one, which has no
value to copy, binds its read access. An owned part of a step, such as an
index, binds by value. The pattern chooses the traversal: a `mut` binding
of an element calls `at` at `mut` (see [`mut?` items](accesses.md#mut-items))
and opens the base mutably, as a `mut` binding opens a matched place, so the
base needs no marker and `for x in mut xs` is an error. A method chain
traverses mutably the same way: `for (i, mut x) in xs.enumerate()`.

`Collection` (`core::iter`) is the protocol: `start` and `next` move a
cursor, `has` tests whether it names an element, and `at` projects the
element at it. A loop tests each cursor with `has` before its body runs,
calls `at` only on a cursor `has` holds for, and steps with `next` from a
cursor `has` accepted; the position after the last element (or, in reverse,
before the first) is a cursor that names none. `at` is `mut?`: a loop whose
pattern binds an element `mut` takes it at `mut`, which needs a
`mut Collection` (or a `mut Driver`), and `Bidirectional` (`last`, `prev`)
traverses in reverse. Arrays, `MemArray`, `MemVec`, `MemSlice`, ranges,
`SolArray` and `SolEnumerableSet` are collections, and so is `DynArray`,
whose loop decodes each element as it reaches it; a collection declares the
effects its methods need (`uses E`).

A loop whose pattern copies or discards every element of a step (a `Copy`
element bound by a plain name or `var`, `_`, or a producer's value) holds no
access on its base between steps, so the body may change the collection, and
`at` reads each element afresh. A body that removes the element it is at
leaves `next` a cursor that names none, and `has` tests the one `next`
returns, as always. A `ref` or `mut` element holds its step's session during
the body, which then cannot change the base, and the session ends before the
loop steps; so does a `while let` whose condition opens a session, until the
back edge.

## Drivers and producers

A method chain on a collection traverses it through a driver, which keeps
its own state and never holds the collection: in `for x in xs.reversed()`,
the loop holds `xs`, the receiver whose type the driver names as its
`Base`, and evaluates it once. The standard drivers are `reversed()`,
`enumerate()`, `take(n)`, `skip(n)`, `step_by(k)` and `filter(p)`, and they
combine as methods: `xs.reversed().enumerate().take(3)`.

Each position of a driver yields a step, its `shape Step`: the element
accesses it projects, with what it adds beside them. `xs.enumerate()` steps
are `(own usize, ref T)`, `xs.enumerate().enumerate()` steps are
`(own usize, (own usize, ref T))`, and the other standard drivers keep their
inner driver's. A closure a driver calls takes the step whole and may
destructure it, `xs.enumerate().filter(|(i, x)| i % 2 == 0)`. A producer,
such as `map(f)`, yields owned values instead of steps:
`xs.enumerate().map(|(i, x)| i as u256 * x)`. A driver may call its inner
traversal more than it yields: `filter(p)` projects each element to test it
and again to yield it, and `take(0)` still starts its inner driver.

A driver is an ordinary value, built, stored, returned and passed to
functions. It names the type of the base it runs over (`Identity<B>`,
`Take<Reversed<B>>`), and `xs.by(d)` runs one over `xs`, which must have
that type: `for x in xs.by(d)`. `d` moves into the loop, as a producer
does, so one that is not `Copy` runs a single loop, and a driver passed as
a view parameter must be `Copy`. Generic code names the step it binds:

```fe
fn sum_by<C: Collection, D: Copy + Driver<Base = C, Step<ref> = ref u256>>(_ xs: C, _ d: D) -> u256
    uses (C::E, D::E)
{
    var s = 0
    for x in xs.by(d) {
        s += x
    }
    s
}
```

## Higher-order operations

Collections provide `fold`, `any`, `count` and `find`. `fold` owns its
accumulator and views each element, so neither needs to be `Copy`:

```fe
let totals = accounts.fold(Totals { sum: 0 }, |acc, account| {
    Totals { sum: acc.sum + account.balance }
})
```

`MemArray::collect(xs, xs.map(f))` collects a producer's values into an
array, and `MemVec::collect` into a growable one: the first argument is the
base the producer runs over.
See [closures](closures.md) for the callables these take.
