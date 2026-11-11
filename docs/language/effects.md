# Effects

A function's `uses` clause lists the effects its callers provide: places
and capabilities it reaches without taking them as arguments.

```fe
fn settle() uses (log: Log, totals: mut Totals) { .. }
```

## Keys

An effect is identified by its *key*, the type or trait after the colon;
its name only binds it in the body. A type key is provided by a place of
that type, a trait key by a place of a type implementing the trait. A
`uses` list names each key once, so `uses (a: mut Store, b: Store)` is an
error, as are a key an implementation's row (`uses E = (..)`) names twice
and one a method's own list repeats from the row spliced into it. Distinct
keys stay distinct even if they may be one type, such as `uses (a: T, b: U)`
or two trait keys.

A body binds each effect by its name, the label or else the key's last
segment, so the effects of a `uses` list a body sees bind each name once:
`uses (s: Store, s: Log)` and `uses (Store, other::Store)` are errors, in a
function (with its implementation's row spliced in), a row, a contract, an
`init` or a recv arm. A row entry such as `A::E` binds no name. An effect
that binds a parameter's name would hide it, and is an error too.

## Providers

A call takes each of its callee's effects from the innermost scope that
provides the key: a `with` block, then the enclosing ones, then the
function's own effects. One scope gives a key one provider:

```fe
with (store, other) { bump() }           // rejected: two `Store` providers
with (Store = other, store) { bump() }   // `other`: the keyed binding
with (store) { with (other) { bump() } } // `other`: the inner block
with (Store = a, Store = b) { .. }       // rejected: one key twice
```

A keyed binding `Key = value` provides only `Key`, ahead of the scope's
unkeyed providers; an unkeyed provider provides its type and the traits it
implements. Names never choose: neither the callee's label nor the
provider's name takes part, so two providers of one key in a scope are
ambiguous where a callee needs the key, and `with (Key = value)` chooses.

Rows compose: `uses (A::E, B::E)` asks for each effect of both rows, and
two of them of one key, such as a `store: mut Store` in each, take the
key's one provider.

## Aliasing

Effects follow the rule parameters do. A call's arguments and effects
reach disjoint places, except that two may both read one: a `mut` effect
may not overlap another effect or an argument of the same call.

```fe
fn caller(_ totals: mut Totals) {
    with (totals.log, mut totals) { settle() }   // rejected: `log` lies in `totals`
}
```

A read-only effect of a snapshot type (a scalar or handle: an integer,
`bool`, `Address`, a raw pointer, or any `core::marker::Snapshot` type) is a
snapshot, as a view argument of one is: the callee gets a copy taken at the
call, from memory, storage or any other space, and the copy overlaps
nothing. So the call's other effects and arguments may write its place, and
the callee reads the value as it was at the call:

```fe
fn read_n() -> u256 uses (n: u256, c: mut C) {
    c.n += 10
    n                                  // `c.n` as it was at the call
}

with (C = c, u256 = c.n) { read_n() }  // accepted, as `f(c.n, mut c)` is
```

A capability that can start an external execution (`Call`, `Create`,
`Evm`) or address slots directly (`RawStorage`, `RawOps`) reaches all
persistent and transient state, so it may not be provided beside a `mut`
effect in storage, nor a `mut` one beside any effect in storage other than
a snapshot. Two such capabilities may be provided together.

The callee therefore takes its effects to be disjoint, as its parameters
are: `settle` iterates `log` while it writes `totals`. The exception is two
effects whose keys one provider may satisfy: generic keys that may be one
type (`uses (a: mut T, b: U)`), two trait keys, or a trait key and a type
that may implement it. A caller may give both one provider, so the callee
takes them to be one place, whole, or disjoint, never one part of the
other; the caller may give them two places only if those are disjoint or
are two such effects of its own, each whole.

```fe
fn pair<T: Bump, U: Bump>() uses (x: mut T, y: mut U) { .. }

with (counter) { pair<Counter, Counter>() }   // one place for `x` and `y`
```

A packed storage lane provided to `mut` effects is a memory copy stored
back when the call returns, one copy for every effect it provides.
