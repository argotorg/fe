# Closures architecture

A closure is sugar for a struct of captures and a function of the callable
traits `Fn` and `FnMut` ([language](../language/closures.md)). The compiler
keeps that structure without generating items: a closure's type, its
implementations of the callable traits, and its body's instances are all
derived from the closure expression.

## Types

`ClosureTy` (`ty_def`) is interned from the closure's definition (body and
expression), the parent's generic arguments, the capture types (the
environment's fields), the parameters' shapes (a mode and type, or a
product's parts), the result, and the row's components. Closure types unify
structurally. A closure environment is a product like a tuple
(`TyId::is_product`): its fields are its captures. A product parameter's
inputs are its parts, as a function's are (`TypedBody::for_closure`).

## Callable trait implementations

`ty::closure` is the single home for the relationship between a callable
value and the callable traits. A callable value (`CallableValue`) is a
closure or a function item, other than an `unsafe fn`, a projection, a
function with a spread, or a variant constructor.
`CallableTrait::of` recognizes `core::functional`'s `Fn<shape A>` and
`FnMut<shape A>`; an instance's parameters are those its shape argument
lists (`callable_params`). A callable value implements both at
`TyId::params_shape` of its own parameters, with `Out` its result. The trait
solver adds a `Callable` clause when a goal's self type is a callable value,
building an `ImplementorOrigin::Callable` implementor whose shape it unifies
with the goal's; the normalizer resolves `Out` to the value's result; the row
`E` expands to a closure's components or a function's own effects
(`effects::rows`); method selection adds both traits' methods as candidates,
`Fn`'s `call` and `FnMut`'s `call_mut`. A goal whose self type is a function
item with inference variables still solves: the function is known, and the
shape determines its generic arguments. The methods' `..A` spread is
expanded from the instance's shape like any other (`Callable::params`).

A call `f(x)` whose callee is not a function item is a value call
(`check_value_call`): method selection restricted to the callable traits
(`select_callable_value_method`) picks `call`, else `call_mut`, and the call
is checked as that method's call with the callee as the receiver. The typed
body records value calls (`TypedBody::call_receiver`), so lowering and the
later checks treat the callee as the receiver.

## Checking

A closure body is checked inline in its parent's body, so it sees the
enclosing bindings and its types are inferred together with the parent's.
Uses of a binding registered outside the closure are its captures. The
parent's effect environment is swapped out for the body: an effect the body
needs and its `with` blocks do not provide becomes a component of the row,
seeded as the body's own effect on first use, and the body is generic over
the component's provider through a closure-owned provider parameter, assumed
to implement a trait key's trait for the rest of the body
(`TyParam::row_effect_provider`). A function body names its rows' components
the same way where its view expands every row, so a `Bidirectional` method can
use the components of `Collection`'s row `E`.

An argument's closure expectation comes from the callee's `Fn` or `FnMut`
bound on the parameter (`Callable::closure_arg_expectation`), whose shape
lists the parameters' modes and types.

## Lowering

The closure expression lowers to the aggregate of its captures. Its body is
the owner `BodyOwner::Closure`, one per receiver mode (`self` for `Fn`,
`mut self` for `FnMut`), whose typed body is the parent's viewed as the
closure's (`TypedBody::for_closure`: the environment and the closure's
parameters are its parameters). `closure_regions` tells which expressions
belong to which closure body, so each instance resolves only its own call
sites. Captures are read from the environment's fields.

A callable trait's `call` on a closure resolves to the closure body's instance
(`semantic_callee_key_with_assumptions`): its generic substitution is the
parent's arguments the closure type carries, and its effects are the
components of `call`'s row in order, with the providers the call binds;
`RowProviderSubst` replaces the body's provider parameters with them.

On a function item, `call` resolves as a call of the function itself, with
`call`'s row components mapped to the function's effects in order. The item
holds nothing, so the call site drops its receiver (`drops_receiver`): an
instance's semantic body passes the function the operands after it.
