# Generating code from types

A provider is a type that implements `Derive` or `DeriveInherent`. Its `derive` function runs while the program is compiled, reads the fields or variants of a type and returns code. A header, an implementation written with `with (Provider)` in place of a body, asks for that code. Compiler messages call it a derive header. Neither word is related to the header `Type: Trait` of an implementation in `named-implementations.md` or to effect providers:

```fe
use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}

trait Arity { const fn arity() -> usize }

struct CountFields {}
impl Derive<trait Arity> for CountFields {
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

Every written derive header requests code during preparation. A provider opened in a block runs when checking a goal needs the implementation from that provider. The compiler checks the returned code and then retries the goal.

## Providers

A trait provider implements `core::derive::Derive<P>`.

```fe
pub trait Derive<P: * -> Constraint> {
    const fn derive(_ record: Record) -> Code<Item>
}
```

The parameter `P` describes the trait and its supplied arguments, with the implementing type left open. An inherent provider implements `DeriveInherent` with the same method signature. Both receive the requested type as `record`. A missing provider is diagnosed at the requesting header.

```
`NotAProvider` has no `impl DeriveInherent`: the inherent header `impl S with (NotAProvider)` needs a provider that implements `DeriveInherent`
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
use core::derive::{Derive, DeriveInherent}
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
impl DeriveInherent for StructEq {
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
    impl Derive<trait Score> for WithBonus {
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
use core::derive::{Derive, DeriveInherent}
use core::meta::{Arm, Code, Expr, Item, Record, binders, empty, field, field_count, variant, variant_count}

struct EnumEq {}
impl DeriveInherent for EnumEq {
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

- `impl Type with (Provider)` adds inherent members. The provider implements `DeriveInherent`.
- `impl Trait for Type with (Provider)` implements a trait, including its associated constants and types. The trait may be one of the package, of `core`, of `std` or of a dependency. The provider implements `Derive<trait Trait>`.
- `with (Provider) { .. }` provides the trait implementation for the goals that the block proves, and only inside it. The provider implements `Derive<trait Trait>`. The trait and the concrete target record must both be declared in the package that contains the block. If all scoped openings are inapplicable, ordinary lookup proceeds. A selected provider whose prerequisites fail reports an error.

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
use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item, field_count}

pub struct Arity {}
impl DeriveInherent for Arity {
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

## Generated code only fills gaps

In the package that asks for generated code, written code keeps its meaning: a generated item is used only where nothing written answers. A block `with (Provider) { .. }` is the exception, because it asks for the change in the text: inside the block its provider answers the goals of its trait in place of a written implementation, as `scoped()` in the first example shows. Written code is the package's own text, its dependencies, its imports and the prelude, and the headers that the package wrote.

A scoped request can supply the selected implementation for a call in the
block or a prerequisite of another implementation chosen there. The provider
and the generated implementation use their own scopes. Choices in the block
travel through a call as evidence for that call's declared bounds.

Generated code may contain further items, such as an implementation inside a generated function. The provider `Gen` below writes one for `Tr2`. The call `X { a: 0 }.m()` is already answered by the written implementation of `Tr1`, so it keeps calling that:

```fe
use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}

mod a {
    pub trait Tr1 { fn m(self) -> u256 }
}
trait Tr2 { fn m(self) -> u256 }

struct X { a: u256 }
impl a::Tr1 for X { fn m(self) -> u256 { 1 } }

struct Gen {}
impl DeriveInherent for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn carrier() -> u256 {
                impl Tr2 for X { fn m(self) -> u256 { 2 } }
                0
            }
        }
    }
}

struct G { a: u256 }
impl G with (Gen)

#[test]
fn the_written_implementation_answers_the_call() {
    assert!(X { a: 0 }.m() == 1)
}
```

`fe test` passes and prints a warning that names the trait to use for the generated method:

```
warning[6-0041]: the generated implementation of `Tr2` adds `m`, which `X` already has
```

```
`X::m` keeps selecting `Tr1`; name the trait to use the generated one: `Tr2::m`
```

Where nothing written answers, the generated implementation answers: with the line `impl a::Tr1 for X { .. }` removed, `X { a: 0 }.m()` returns 2.

A generated item that written code hides is refused at its request, because it could never be selected. A generated inherent method `size` is refused when the type already has an inherent method `size`: ``the generated `size` requested at .. would be hidden: `Box1` already has an inherent method `size` of `Box1` ..``, where `..` stands for places that the message names.

### Deciding traits

A trait that the compiler consults to choose behaviour declares `#[decides = "reason"]`: for example whether a value is copied or moved, what runs where a value goes out of use, or how a stored value is packed. A generated implementation of a deciding trait is refused at its request, because it would change what written code does without any change to that code. So is a generated implementation of a trait that a bound of a written implementation of a deciding trait names, directly or through further bounds. With the written implementation `impl<T: Marker + Copy> Copy for W<T> {}`, a generated implementation of `Marker` is refused: ``the generated implementation of `Marker` for `X` requested at .. is refused: it could change whether a value is copied or moved for `W<T>` ..``. The message ends with the ways out: write the implementation, request it with a header, or leave it out.

The deciding traits of `core` and `std` are `Copy`, `Snapshot`, `Pinned`, `Drop`, `StateIndex`, `LaneCodec`, `Encode`, `ErrorVariant`, `Fn`, `FnMut`, `Driver`, `Producer`, `WrappingAdd`, `WrappingSub`, `WrappingMul`, `WrappingPow`, `WrappingNeg` and `SolPacked`. Only the traits of `core` and `std` count: `#[decides]` on a trait of any other package is accepted and has no effect.

### Headers for traits of other packages

A header counts as written code, so it is the way to ask a provider for an implementation of a deciding trait, or of any trait of `core`, `std` or a dependency. This header makes drop glue run the member that the provider wrote:

```fe
use core::derive::Derive
use core::meta::{Record, Code, Item}
use core::ops::Drop
use core::ptr

struct CountDrops {}
impl Derive<trait Drop> for CountDrops {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            fn drop(own self) {
                unsafe { *self.counter += 1 }
            }
        }
    }
}

struct Counted { counter: *u256 }
impl Drop for Counted with (CountDrops)

#[test]
fn drop_glue_runs_the_member_a_provider_wrote() {
    let counter = ptr::alloc<u256>()
    unsafe { *counter = 0 }
    {
        let _a = Counted { counter }
        let _b = Counted { counter }
    }
    assert(unsafe { *counter } == 2)
}
```

`fe test` reports `1 passed`: the counter is 2 after the two `Counted` values go out of scope.

The header `impl Conv<u8> for Pair with (Provider)` asks for `Derive<trait Conv<u8>>`. A single registration `impl<A> Derive<trait Conv<A>> for Provider` can handle several arguments. A generator can obtain the selected type with `type_ref<A>()` and splice the returned value into type syntax with `\${...}`.

Omitting a trailing argument and writing its default give the same provider candidates. For example, `Eq` and `Eq<Pair>` for `Pair` both select `Derive<trait Eq>`. `Eq<u8>` for `Pair` selects `Derive<trait Eq<u8>>`. If the provider registers both `Derive<trait Eq>` and `Derive<trait Eq<Pair>>`, either spelling for `Pair` gives an ambiguous request.

A marker trait takes an empty output, `empty<Item>()`. A callable provider names its signature, as in `Derive<trait Fn(u256) -> u256>`. A provider for a header with inferred output uses `Derive<trait Fn(u256) -> _>` and supplies `Out`. The target may be an authored struct expanded by `#[abi]`, `#[error]` or `#[event]`, or the struct of a `msg` variant.

### Unsafe code in generated code

Unsafe code that a provider generates is accepted only if every package that contributed to it is trusted: the package whose text contains the `unsafe` block, and each package whose quotation spliced that text into the code that runs. The package that asked for the code is not a contributor. A package is trusted when it is a package being checked, `core` or `std`, or a dependency that a trusted package lists with `allow_unsafe = true`. The package `dep` has this `src/lib.fe`:

```fe
use core::derive::Derive
use core::meta::{Record, Code, Item}

pub trait Reading { fn read(self) -> u256 }

pub struct ReadsCell {}
impl Derive<trait Reading> for ReadsCell {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn read(self) -> u256 { unsafe { *self.cell } } }
    }
}
```

The package `app` depends on `dep`, implements `Reading` for a type `Cell` with `impl Reading for Cell with (ReadsCell)` and calls `read()` in a test. `fe test` in `app` is refused with ``error[16-0015]: call reaches unsafe code in `dep`, which is not allowed to use it``. The error labels the call and the `unsafe` block that `dep` wrote, and names `allow_unsafe = true` as the way to trust `dep`. With `dep = { path = "../dep", allow_unsafe = true }` in the `fe.toml` of `app` the test passes.

The check also follows code that runs without a written call. A `Drop` implementation that `dep` generates, with an `unsafe` block, is refused at the binding whose value goes out of scope. The same holds for the `Decode` and `Encode` implementations that a contract's dispatcher applies to the arguments and return values of `recv` arms and to the arguments of `init`, and for a `#[test]` function that an untrusted package contributed to.

## What is refused

- A block for a trait of another package: ``a scoped derive supports traits of the same package; for a trait of another package, write a header `impl Trait for Type with (Provider)` ``.
- A block whose goal is about a type parameter, such as `T::score()` inside `with (Provider) { .. }`, reports `scoped derive requires a concrete authored record`. A goal about a record applied to a type parameter, such as `Wrap<T>::score()`, reports `scoped derive requires a ground target`.
- A provider whose trait family does not match the requested trait and its arguments.
- A generated item that written code hides; a generated implementation of a deciding trait, or of a trait that a bound of a written implementation of a deciding trait names; unsafe code that a package that is not trusted contributed to.
- A header inside a function body: `a derive header inside a function body is not supported; declare the header in a module`.
- A glob import, such as `use m::*`, in a block of a quotation: ``a glob import is not supported inside a quotation; list the names to import, as in `use m::{A, B}` ``. An import that lists its names declares them in the quotation's block, as in a function body. Quotation content is checked when a request runs the provider. A quotation that no request reaches is never lowered.
- A provider that reads what another provider generated, or that does not end.
- A named implementation of `FieldQuery` is refused because `query_field` evaluates it outside every `with`.

## Limits

- Only `core` and `std` declare deciding traits.
- The bound rule follows bounds by trait identity and does not ask whether the program would otherwise work, so it can refuse more than needed. The message names the written implementation whose bound applies; a header is the way to ask for such an implementation.
- An `unsafe impl` without functions is judged where it is declared, so an untrusted package cannot generate one even where nothing uses it.
- A block accepts only a trait of its own package.
