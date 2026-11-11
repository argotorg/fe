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

A capability that can start an external execution (`Call`, `Create`,
`Evm`) or address slots directly (`RawStorage`, `RawOps`) reaches all
persistent and transient state, so it may not be provided beside a `mut`
effect in storage, nor a `mut` one beside any effect in storage. Two such
capabilities may be provided together.

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

A read-only effect of a snapshot type provided by a memory place is passed
as a copy, as a view parameter is, unless another effect of the callee may
be the same place and is `mut`. A packed storage lane provided to `mut`
effects is a memory copy stored back when the call returns, one copy for
every effect it provides.
