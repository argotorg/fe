# Generating code from types

A provider is a type that implements `Derive`. Its `derive` function runs while the program is compiled, reads the fields or variants of a type and returns code. A header, an implementation written with `with (Provider)` in place of a body, asks for that code. Compiler messages call it a derive header. Neither word is related to the header `Type: Trait` of an implementation in `named-implementations.md` or to effect providers:

```fe
use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}

trait Arity { const fn arity() -> usize }

struct CountFields {}
impl Derive<Arity> for CountFields {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { const fn arity() -> usize { ${n} } }
    }
}

struct Pair { a: u256, b: u256 }
impl Arity for Pair with (CountFields)

struct Line { a: u256, b: u256, c: u256 }
impl Arity for Line { const fn arity() -> usize { 100 } }

fn scoped() -> usize { with (CountFields) { Line::arity() } }
fn plain() -> usize { Line::arity() }

#[test]
fn the_header_and_the_block() {
    assert!(Pair::arity() == 2)
    assert!(scoped() == 3)
    assert!(plain() == 100)
}
```

`fe test` on this file reports `1 passed`. `Pair::arity()` returns 2: the compiler runs `CountFields::derive` for `Pair` and uses the returned function as the body of `impl Arity for Pair`. The block `with (CountFields) { .. }` uses the provider for the `Arity` goals that are proved inside it. `scoped()` returns 3, the number of fields of `Line`, while `plain()` still returns 100.

A header and a block are both called a request. Before a program is checked, the compiler prepares it: it runs the provider of every request and adds the returned code to a copy of the program. Generated code is then checked and used like code that was written.

## Providers

A provider implements `core::derive::Derive<G>`:

```fe
pub trait Derive<G> {
    const fn derive(_ record: Record) -> Code<Item>
}
```

`G` is the trait that the provider implements, or `Inherent` for members of the type itself. `record` stands for the type that the request names. A type that does not implement `Derive<G>` is refused as a provider:

```
`NotAProvider` has no `impl Derive<Inherent>`: the inherent header `impl S with (NotAProvider)` needs a provider that implements `Derive<Inherent>`
```

`derive` is a `const fn`, so it runs in the compile-time evaluator and stops at the evaluator's limits. A provider that does not end is refused with `existing CTFE step limit exceeded`.

The returned code is the body of the implementation that the header names: functions, constants and associated types. The body is checked like a written one: a method that the trait does not declare, a method that is missing and a method with the wrong return type are each an error. A `struct` cannot be quoted: `only functions, constants, types and impls can be quoted`.

## Reading a type

`core::meta` reads the shape of `record`.

- For a struct: `field_count(record)`, `field(record, i)` for the `i`-th field in declaration order, and `field_type(field)` for its type.
- For an enum: `variant_count(record)` and `variant(record, i)`, which returns the record of one variant's payload. The field functions then apply to that record. `field_count` of a whole enum is refused with `field_count requires a struct; a whole enum has no selected payload`, and `variant_count` of a struct with `variant_count requires a whole authored enum`.
- `type_is<T>(ty)` tells whether a field type is `T`, for `T` one of `bool`, `u64` and `u256`. Any other `T` is refused with `type_is supports only bool, u64, and u256 candidates`.
- `query_field<F, R>(ty)` asks the trait system about a field type `T`: it evaluates `FieldQuery<R>::evaluate()` of `F<T>`. With `impl<T: Weight> FieldQuery<usize> for Weigh<T> { const fn evaluate() -> usize { T::weight() } }`, the call `query_field<Weigh, usize>(ty)` returns the `weight()` of the field type.

`std::derive::fold_fields<S>(record)` folds the fields of a struct, or of an enum payload, in declaration order into one expression. The step type `S` implements `FieldStep`: `unit()` is the code for a record without fields, and `step(acc, field)` extends the code `acc` that was built from the earlier fields.

## Writing code

`quote { .. }` makes code a value of type `Code<K>`. The kind `K` is `Item` for functions, constants, associated types and implementations, and `Expr`, `Pat`, `Arm` or `Where` for the fragments that are put together into them. `${..}` splices a value, a field or other code into a template.

```fe
use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item, Expr, Field}
use std::derive::{FieldStep, fold_fields}

struct EqStep {}
impl FieldStep for EqStep {
    const fn unit() -> Code<Expr> {
        quote { true }
    }
    const fn step(_ acc: Code<Expr>, _ f: Field) -> Code<Expr> {
        quote(other) { ${acc} && self.${f} == other.${f} }
    }
}

pub struct StructEq {}
impl Derive<Inherent> for StructEq {
    const fn derive(_ record: Record) -> Code<Item> {
        let body = fold_fields<EqStep>(record)
        quote(other) {
            pub fn same(self, _ other: Self) -> bool {
                ${body}
            }
        }
    }
}

struct Point { x: u256, y: u256 }
impl Point with (StructEq)

#[test]
fn points_are_compared_field_by_field() {
    assert!(Point { x: 1, y: 2 }.same(Point { x: 1, y: 2 }))
    assert!(!Point { x: 1, y: 2 }.same(Point { x: 1, y: 3 }))
}
```

`EqStep` builds `true && self.x == other.x && self.y == other.y` for `Point`, and `StructEq` places it in the body of `same`. `impl Point with (StructEq)` is an inherent header: the generated function becomes a member of `Point`.

A name in a template resolves where the template is written, not where the request is. A provider can therefore call its own helper functions from any module:

```fe
use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}

trait Score { fn score(self) -> usize }

mod provider {
    use super::Score
    use core::derive::Derive
    use core::meta::{Record, Code, Item, field_count}

    fn bonus(_ count: usize) -> usize { count + 40 }

    pub struct WithBonus {}
    impl Derive<Score> for WithBonus {
        const fn derive(_ record: Record) -> Code<Item> {
            let count = field_count(record)
            quote { fn score(self) -> usize { bonus(${count}) } }
        }
    }
}

fn bonus(_ count: usize) -> usize { count + 900 }

struct Pair { a: u256, b: u256 }
impl Score for Pair with (provider::WithBonus)

#[test]
fn the_template_calls_the_helper_next_to_the_provider() {
    assert!(Pair { a: 1, b: 2 }.score() == 42)
}
```

`score()` returns 42, from the `bonus` next to the provider, and not 902 from the `bonus` of the module that wrote the header.

A name that a template takes from the place where it is spliced is listed in parentheses, as in `quote(other) { .. }`. In `inherent_fold.fe` both the template of `step` and the template of `derive` list `other`. Without the list the name is not found in the template that uses it:

```
undefined variable `other`
```

## Enums

`variant(record, i)` selects one variant. `binders(variant, group)` makes a pattern that binds every field of that variant, and `pattern.at(field)` is the expression for the field that the pattern binds, within the arm that uses the pattern. Two patterns for the same variant in one arm need different group numbers, here 0 and 1. Using the same number twice is refused: `binder group already bound in an enclosing pattern`.

```fe
use core::derive::{Derive, Inherent}
use core::meta::{Arm, Code, Expr, Item, Record, binders, empty, field, field_count, variant, variant_count}

struct EnumEq {}
impl Derive<Inherent> for EnumEq {
    const fn derive(_ record: Record) -> Code<Item> {
        var arms = empty<Arm>()
        var i: usize = 0
        while i < variant_count(record) {
            let v = variant(record, i)
            let left = binders(v, 0)
            let right = binders(v, 1)

            var same: Code<Expr> = quote { true }
            var j: usize = 0
            while j < field_count(v) {
                let f = field(v, j)
                same = quote { ${same} && ${left.at(f)} == ${right.at(f)} }
                j += 1
            }

            arms = quote(other) {
                ${arms}
                ${left} => match other {
                    ${right} => ${same}
                    _ => false
                }
            }
            i += 1
        }
        quote(other) {
            pub fn same(self, _ other: Self) -> bool {
                match self {
                    ${arms}
                }
            }
        }
    }
}

enum Shape { Dot, Line(u256), Rect { w: u256, h: u256 } }
impl Shape with (EnumEq)

#[test]
fn variants_and_payloads_are_compared() {
    assert!(Shape::Dot.same(Shape::Dot))
    assert!(Shape::Rect { w: 1, h: 2 }.same(Shape::Rect { w: 1, h: 2 }))
    assert!(!Shape::Rect { w: 1, h: 2 }.same(Shape::Rect { w: 1, h: 3 }))
    assert!(!Shape::Line(1).same(Shape::Dot))
}
```

## Requests

- `impl Type with (Provider)` adds inherent members. The provider implements `Derive<Inherent>`.
- `impl Trait for Type with (Provider)` implements a trait of the same package, including its associated constants and types. The provider implements `Derive<Trait>`.
- `with (Provider) { .. }` provides the trait implementation for the goals that the block proves, and only inside it. The provider implements `Derive<Trait>`.

A header may be generic, as in `impl<T> Wrapper<T> with (Width)`. Its target is a struct or an enum; a header for another type is refused. A request may be written in any module, provided every field of its target is visible there; in the module that defines the type, its private fields are visible. Otherwise the request is refused:

```
a derive header requires every field of its target to be visible where the header is written
```

A header under a `#[cfg(..)]` attribute that does not hold is not a request: its provider does not run, and the file is accepted as if the header were absent. There is no attribute `#[generate(..)]`; on an item it is ignored, like any other attribute that the language does not know.

## What the checks see

- Generated code is checked as ordinary code. An error in it is reported at the quoted text that wrote it.
- A provider does not read what another provider generated. Calling `First::one()` in a provider, where `one` comes from another header, is refused with the note `a provider runs against authored text and the implementation headers an author wrote; what generation produces in its own package is not visible to it`. A provider that opens another provider in a `with` block is refused before any provider runs. A function that two providers need is an ordinary function that both call.
- A written header counts as a fact when another request is admitted. With the header `impl Size for Leaf with (CountP)` and a struct `Wrap<A: Size>`, a second header whose target contains `Wrap<Leaf>` is admitted, because the first header states that `Leaf: Size`. Without the first header the second is refused: ``trait bound is not satisfied; `Leaf` doesn't implement `Size` ``.

## Packages and dependencies

`fe check` and `fe test` prepare a package together with the packages that it depends on. A header may be written in any file of a package, and a provider may be written in a dependency. The dependency `lib` has this `src/lib.fe`:

```fe
use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item, field_count}

pub struct Arity {}
impl Derive<Inherent> for Arity {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { pub const fn arity() -> usize { ${n} } }
    }
}

// A type of this package that is given a generated method here.
pub struct Version { pub major: u256, pub minor: u256, pub patch: u256 }
impl Version with (Arity)
```

The package `app` lists `lib = { path = "../lib" }` under `[dependencies]` in its `fe.toml`, and has this `src/lib.fe`:

```fe
use lib::{Arity, Version}

struct Pair { a: u256, b: u256 }
impl Pair with (Arity)

#[test]
fn a_provider_of_a_dependency_generates_a_method_here() {
    assert!(Pair::arity() == 2)
}

#[test]
fn what_a_dependency_generates_is_an_ordinary_item_for_its_dependents() {
    assert!(Version::arity() == 3)
}
```

`fe test` in `app` reports `2 passed`.

- What a dependency generates is part of that dependency for the packages that depend on it, as the second test shows.
- A dependency is prepared before the packages that use it. A provider in `app` can therefore read what `lib` generated: it can call `Version::arity()` and splice the result.
- A provider does not read what its own package generates. The refusal and its note are the ones of a single file.
- Two files of a package may use each other's providers. A header in `left.fe` may name a provider that is written in `right.fe`, and the reverse.
- A provider that fails in a dependency refuses the packages that depend on it. The error is reported at the dependency's file, for example `division by zero in const context` at the place in `lib/src/lib.fe` that divides.
- The check that two implementations of a trait do not overlap covers every package that a package depends on, directly or not, with or without generated code. The package `base` has this `src/lib.fe`:

  ```fe
  pub trait T { const fn f() -> u256 }
  impl<U> T for U { const fn f() -> u256 { 1 } }
  ```

  The package `mid` depends on `base` and contains `pub use base::T`. The package `app` depends on `mid` and has this `src/lib.fe`:

  ```fe
  use mid::T
  pub struct X {}
  impl T for X { const fn f() -> u256 { 2 } }
  ```

  `fe check` in `app` reports `error[5-0001]: conflicting trait implementations`.

## What is refused

- A header or a block for a trait of another package, including a trait of a dependency: ``a derive header can implement only a trait of its own package; `Arity` is defined in another package`` and `a scoped derive supports only traits of its own package`.
- A block whose goal is about a type parameter, such as `T::score()` inside `with (Provider) { .. }`, reports `scoped derive requires a concrete authored record`. A goal about a record applied to a type parameter, such as `Wrap<T>::score()`, reports `scoped derive requires a ground target`.
- A header for a trait that has parameters besides its target type, or associated type bindings: ``a derive header can implement only a trait that has no parameters besides its target type and no associated type bindings; `Conv` has more``.
- A generated member that an existing member hides: ``The generated method `size` of `Box1` would never be selected: `Box1` already has a written `size` ``. The existing member may be written in an inherent implementation or in a trait implementation. Where it belongs to a trait implementation the message is inexact: the generated inherent member would be selected in place of the written one, so the header is refused, but not because the generated member would never be selected.
- A generated function that contains an implementation of a trait that the compiler consults to choose behaviour. The list is fixed: `Drop`, `Copy`, `Snapshot`, `Pinned`, `StateIndex`, `LaneCodec`, `ErrorVariant`, `Fn`, `FnMut`, `Producer`, `Driver` and `SolPacked`. The message begins ``the generated implementation of `Drop` for `Counted` is refused: it could change what runs where a value goes out of use``.
- A header inside a function body: `a derive header inside a function body is not supported; declare the header in a module`.
- A glob import, such as `use m::*`, in a block of a quotation: ``a glob import is not supported inside a quotation; list the names to import, as in `use m::{A, B}` ``. An import that lists its names declares them in the quotation's block, as in a function body. Quotation content is checked when a request runs the provider. A quotation that no request reaches is never lowered.
- A named implementation of `FieldQuery` is refused because `query_field` evaluates it outside every `with`.

## Limits

- Headers and blocks work for traits of the package that the request is in.
- Only the hidden-member refusal and the list of twelve traits keep generated code from changing what written code does. A generated implementation of a trait that is not on the list, nested in a generated function, is accepted even where it changes which implementation an existing call selects: a call that returned 1 returns 2.
