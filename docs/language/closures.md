# Closures

A closure is a function value written inline:

```fe
let inc = |x: u256| x + 1
let total = xs.fold(0, |acc, x| acc + x)
let max = |a: own u256, b: u256| -> u256 { if b > a { b } else { a } }
```

Each parameter has a mode and a type, as a function parameter does
(`x: u256` is a view, `x: own u256` owned, `x: mut u256` exclusive). Either
may be left out: `|x|`, or `|x: own|` for a mode without a type. The return
type is optional too.

A parameter may be a pattern, which takes its argument apart as a `let`
would: `pairs.fold(0, |acc, (a, b)| acc + a * b)`, `|Point { x, y }| x + y`.
A closure passed one tuple takes it apart with a pattern, `|(i, x)|`, not
as two parameters. A parameter may also be a product of accesses, which its
pattern takes apart as a function's would:
`|(mut a, b): (mut u256, ref u256)| a += b`.

## Callable shapes

Because modes are not types, a closure's type is described by its *shape*,
the modes of its parameters and their types. Bounds name shapes directly:

```fe
fn fold<A, F: FnMut(own A, Self::Item) -> A>(self, _ init: own A, _ f: mut F) -> A
    uses (E, F::E)
```

`Fn(own A, B) -> R` is the core trait `Fn<(own A, B), Out = R>`, whose
`call(self, ..A)` views the callable; `FnMut(..)` is `FnMut<(..)>`, whose
`call_mut(mut self, ..A)` mutates it. The trait's argument is the shape of
the parameter list, of kind `shape`, and `..A` spreads it into the method's
parameters. `-> _` leaves `Out` open, as `Fn(A, B) -> _`. A closure
implements `Fn` and `FnMut` at the shape of its own parameters, with `Out`
its result.

A trait of your own can take a parameter list the same way:

```fe
trait Apply<shape A> {
    type Out

    fn apply(self, ..A) -> Self::Out
}

impl Apply<(own u256, ref u256)> for Adder {
    type Out = u256

    fn apply(self, _ a: own u256, _ b: u256) -> u256 {
        self.bias + a + b
    }
}
```

A spread is last, only in a trait method without a body; an impl writes the
parameters out, unlabeled, and a call passes one argument for each.

Unannotated parameters take their modes and types from the shape bound of
the parameter the closure is passed to, after the arguments before it: in
`xs.fold(0, |acc, x| acc + x)`, `acc` is `own u256` and `x` is a view of the
item. The `filter` and `map` adapters on a driver take its step, so
`xs.enumerate().filter(|(i, x)| i % 2 == 0)` destructures an owned index and
a view of the element. A closure passed to a function without such a bound
annotates its parameters. A closure literal, or a function named by its path,
may be passed to a `mut` parameter directly.

## Calls

A callable value is called like a function, `f(x)`. The call runs the
value's `Fn` `call`, or `call_mut` when only an `FnMut` bound is
known, which takes `f` as `mut`. This calls a closure, a parameter bound by
`Fn` or `FnMut`, and a value of any type that implements one; another method
named `call` does not make a type callable. `f.call(x)` and `f.call_mut(x)`
name the methods explicitly. A field holding a
callable is called as `(h.f)(x)`, since `h.f(x)` calls a method `f`.

```fe
let add = |a: u256, b: u256| a + b
let five = add(2, 3)

fn apply<F: Fn(u256) -> u256>(_ f: F, _ x: u256) -> u256 uses F::E {
    f(x)
}
```

## Functions as callable values

A function item implements `Fn` and `FnMut` at the shape of its parameters
too, so a
named function, associated function or trait method can be passed where a
closure can: `xs.fold(0, add)` with `fn add(_ acc: own u256, _ x: u256) ->
u256`. Its row is the function's own effects. A generic function's arguments
are inferred from the bound, as in `apply(identity, 9)`. An `unsafe fn`, a
projection (every `mut?` function is one), a function with a spread, and a
variant constructor are not callable values.

## Captures

A closure captures what its body uses from the enclosing body by copy, for
`Copy` values, or by move. The captures are the closure's own, and read-only:
writing to one, taking `mut` access to it, or moving a non-`Copy` capture out
of the body is an error. A closure cannot capture an access (a view
parameter, a `ref` or `mut` binding) of a non-`Copy` value; pass the value as
a parameter instead. Thread state through parameters and results:
`fold(init, |acc, x| acc + x)`.

## Effects

The effects a closure's body uses, by naming an enclosing effect binding or
by calling a function that requires one, are not captured: they are the
closure's row `E`, which the caller of each call provides. A higher-order
function forwards it with `uses F::E`:

```fe
fn count<P: Fn(Self::Item) -> bool>(self, _ p: P) -> usize uses (E, P::E)

fn matching(_ xs: [u256; 4]) -> usize uses (store: Store) {
    xs.count(|x| x == store.n)
}
```

The provider is the one in scope where `count` is called, so a closure made
under `with (Store = a)` and passed to a higher-order function called under
`with (Store = b)` uses `b`. A `with` inside the closure's body provides the
effect itself. A row the body needs, such as `C::E` of a collection it
captures, is part of the closure's row too.

A closure cannot be used in a constant context.
