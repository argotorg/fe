# Access checker architecture

Fe has mutable value semantics: references are not values. A `ref` or `mut`
access exists only as a parameter mode, a named binding (`let h = mut p.x`),
or the result of a projection (a function returning `ref T`, `mut T`, or a
shape such as `(usize, mut T)` or `Option<mut T>`). The access checker
([`semantic/access`](../../crates/hir/src/analysis/semantic/access)) enforces
one rule over each semantic instance's normalized body: two open accesses to
overlapping places conflict unless both read. It also checks moves and
initialization.

Three principles keep it small and predictable:

- **Signatures are the call contract.** A call is checked against the
  callee's signature: its data arguments and the footprints of its declared
  effects. No callee body is summarized.
- **Extents are explicit.** Lowering places an `end` after the last use of
  every access, before any folding, so acceptance never depends on
  optimization.
- **Raw memory is a trust boundary.** Raw pointers and raw storage operations
  require `unsafe` and are not modeled.

## Pipeline

```mermaid
flowchart TD
    Typed[Typed HIR: modes and shapes] --> Raw[Raw semantic IR]
    Raw --> Elaborate[end elaboration and yields]
    Elaborate --> Normalized[Verified normalized body]
    Normalized --> Check[Access check]
    Normalized --> Refine[Provisional address-space refinements]
    Check --> Gate[Runtime lowering gate]
    Refine --> Instances[Per-space callee instantiation]
```

- The type checker sees plain types: a parameter carries a mode, a binding an
  access kind, a projection's return a shape. Semantic lowering represents an
  access by a *carrier* value (`View`, `BorrowRef`, `BorrowMut` types that
  exist only in the IR).
- A projection's every yield site is a `Yield` terminator whose resume block
  runs the slide and returns. A tail or `return` yield has an empty slide; a
  `yield` statement's slide is the code after it, except on a sum shape's
  empty variant, which grants nothing and so returns at once, as `?`'s exit
  does: a variant constructor is decided statically, and any other yield,
  such as a forwarded `yield self.entry(k)`, tests its value. Each yield site
  yields on its own path.
- `end` elaboration (`lower/elaborate.rs`) computes liveness on the raw body.
  An access (a borrow or a projection call's session) stays open while its
  carrier or any carrier derived from it may be used. It ends right after its
  last use on each path; an access that dies on a control-flow edge ends at
  the start of the successor, on an edge block of its own when the successor
  has other predecessors. Of the accesses ending at one point, borrows close
  first, then sessions finish newest first, so a session finishes before any
  session it depends on.
- `check_semantic_accesses` checks one instance. The runtime lowering gate
  requires it for every concrete instance, and the diagnostics pass checks
  each item's identity instance and, transitively, its concrete callees.

## Calls and receivers

For a call `r.m(args)` the elaboration order is fixed: the receiver's root is
resolved first (a temporary is evaluated, and hoisted into a local for a
`mut` receiver), the arguments are evaluated, the receiver chain's
projection sessions open innermost first, and the receiver's access opens
last with its mode. A receiver place rooted at a binding, or in a
projection's grant, is lowered whole after the arguments, index operands
included (`Receiver::Place(None)`), so the arguments may read and write it
and the call sees what they wrote; an argument that moves it leaves the
receiver's access a use after a move. `prepare_place_root` prepares the
receiver of the innermost projection call on a place's path before the
arguments. An assignment follows the same order: its target's root is
resolved first, the same way, then the value is evaluated, then the target's
sessions open. An operator's left operand is not a receiver: both operands
are ordinary arguments, evaluated left to right. A variable operand that
other operands follow (a call's argument, an aggregate's field, an
operator's left operand, an assignment's value) is taken where it appears
(`take_operand`): copied if `Copy`, moved if owned, and a view argument of
a mutable local opens there, so the later operands' writes follow it and
their uses of a moved variable are uses after a move.

A view argument's transport follows its type (`ty_is_snapshot`): a snapshot,
meaning a scalar or handle (a primitive, a raw pointer, an effect handle or
a `core::marker::Snapshot` type such as `Address`), is passed
by copy and opens no access, and a projection's snapshot parameter is a copy
its session owns. Every other aggregate, `Copy` or not, is viewed in place:
the call reads its place for the call's duration, a projection's session
reserves it, and a yield from it is the caller's place. Lowering may still
pass a `Copy` aggregate to an ordinary function by value; the checker traces
the value back to its place.

## Tokens and places

The analysis resolves carriers to *tokens*:

| Token | Created by | Meaning |
| --- | --- | --- |
| Input | `ref`/`mut` parameters | The caller's place, open for the whole body. |
| Access | borrows and views | A named or call-duration access of resolved places. |
| Reservation | projection calls | A session reserves each argument carrier and each effect domain it is given. |
| Grant | projection calls | One per access component the session yields, derived from the session's reservations. |
| Handle | provider handles | Names a domain; confers no exclusivity. |

An abstract place is a base and a path (fields, variant fields, constant or
dynamic indices). A path may end in `KeySpace`: the entries of the static
slot handles the place holds (a map's keyed slots), which lie apart from the
place itself. Key-space regions overlap each other by their paths and raw
state, never a data access, so a `mut` access of a struct holding a map does
not conflict with an operation on the map's entries.

- `Root`: storage the body owns: locals, owned parameters, temporaries.
- `Param(i)`: the place data parameter `i` names. Its address space is part of
  the instance (`EffectProviderSubst::param_spaces`), refined per call site.
- `Domain(d)`: a resource domain: a contract field, an effect parameter, a
  root provider, or a dynamic domain for a handle of unknown provenance.
  Distinct contract fields are disjoint; other domains of one address space
  may alias, since a caller may supply overlapping providers. Where no caller
  decides a parameter's or an effect provider's space, as in a generic
  function's own check, a place of a state-only type lies in its pin's space,
  or else in storage, never in memory (`TyId::default_space`). A place that
  holds nothing conflicts with nothing: a zero-sized type, or the provider of
  a sealed std EVM capability (`Ctx`, `Log`, `Call`, …), which only the
  zero-sized `Evm` implements, even as a generic helper's effect.
- `Grant { session, component }`: what a session granted. Places derived from
  one grant overlap; sibling components of a split are disjoint. A grant lies
  in the address space its projection's result space exports.
- `State(space)`: every slot of an address space, the footprint of external
  execution and raw storage authority.
- `Raw`: memory reached through a raw pointer; never checked.

A place reached through a carrier that holds no access, or by dereferencing
a value that is not a pointer, has no base: once the flow is solved, an
operation on it is an internal error, never an access of memory.

Places in different spaces are disjoint. A place's space is its root's
unless a pinned type or an entry along its path moves it (below), so an
entry of a map lies in storage whatever holds the map.

An access through a carrier is authorized by the tokens it was derived from,
so a nested access (`let g = mut h.x`) does not conflict with `h`, and `h` is
usable again once `g` ends. A write through a `ref` access is rejected.

## Interference

Each operation is checked against the open tokens it is not derived from:

- Place reads, writes, moves, borrows and views access their resolved places.
- A call accesses each carrier argument for its duration with the parameter's
  mode, and each declared effect's footprint: the supplied place or the
  places a handle value names, or all persistent and transient state for a
  capability that can start external executions (`Call`, `Create`, `Evm`) or addresses slots
  directly (`RawStorage`). Immutable authority only reads.
- Calls that start external executions themselves (call intrinsics and
  methods of the reentrant std capabilities) access all state.
- An entry of a place-indexed collection (`m[k]`) is the collection's place
  extended by an index step: a constant index for a literal key, `[*]` for
  any other.

So `let v = mut store.value` held across a call whose effects permit
reentrancy is rejected, while closing the access before the call is accepted.
A callee's data parameters must be compatible with its own effects: passing a
storage place as `mut` to a function declaring `mut Call` is rejected.

## Projections

A projection call opens a session: reservations for its arguments and its
`uses` domains, and grants for the yielded components. Sessions are not a
stack; they end at their `end`, in any order. In the projection's body:

- Creating a grant is an access of its component's mode on the yielded
  place, checked against every access the suspended frame keeps open; only
  the grant's own ancestors are exempt. The exported referent must be
  initialized, and an owned component may not overlap an access component.
- Each component of every yield site names places in one address space per
  instantiation. That space is the projection's *result space*
  (`projection_result_spaces`), inferred from the body and exported with the
  instance: callers combine grants by their result spaces, never by bodies.
  A component no yield grants, such as the payload of a variant the
  projection never returns, has none, and neither does a grant of one.
  Lowering holds the class it returns each component in to its space
  (`check_projection_result_spaces`); a disagreement is an internal error.
  A place's space is its root's, unless a pinned type or an entry along its
  path moves it into the type's pin or the collection's
  (`access::space::PathSpace`): a `TSlot` held in a storage struct, and its
  `mut self.lock.value`, are transient. The innermost such step decides,
  and a lane's grants lie in memory.
- The `mut` components of a split are structurally disjoint.
- A `#[raw_place]` projection has nothing to resume: after each yield it
  only ends the accesses its grant derives from, a session among them is a
  `#[raw_place]` call's, and it yields no place of its own frame. An unsafe
  split takes places and calls of such projections as its components.
- Every path that completes yields exactly once; a sum shape's empty variant
  is returned only before the yield, and the ramp's sessions still finish on
  that exit.

A `mut` yield of a place in the projection's own frame with no slide after it
is a warning: its writes are discarded.

At runtime every projection call is inlined into its caller
(`mir/src/runtime/lower/inline.rs`): the ramp replaces the call, each `end`
of the session runs its own copy of the slide, and a projection with several
yield sites records which one its ramp took. A projection's grant has the
runtime class its body yields. A sum shape is not a value: when each yield
builds a variant and the caller only tests the variant and binds its fields,
each yield goes straight to the caller's case for the variant it built, with
the variant's fields as locals, so no enum value carries a grant. The class
of a returned enum takes each variant's fields from the values built as that
variant. Recursion through projection calls cannot be inlined and is
rejected by the checker.

## Moves and initialization

A forward analysis tracks possibly moved paths of values, local roots, `mut`
parameters and the referents of `mut` accesses, joined at control-flow
merges. A place may be moved out of through a stable `mut` access (a binding,
a grant or a parameter) only if the access restores it before it ends, or, for
a parameter, before the function returns or yields; a hole blocks reads and
arguments in between. `[*]` is a may-alias abstraction: moving out of an
element at a dynamic index is rejected, and a store through one reinitializes
nothing. Storage and transient slots never hold a hole, and nothing moves out
of a `ref` access.

## Constant evaluation

CTFE evaluates a projection call as a session: the callee frame runs until its
`Yield`, stays suspended while the caller uses the grant, and runs its slide
when the caller reaches the session's `end`.
