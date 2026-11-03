# Destruction and memory owners

Every owned value is destroyed exactly once, when its owner gives it up. No
call is written for it: the compiler inserts each destruction where the
value's lifetime ends, and a type that holds a resource releases it in its
`core::ops::Drop` implementation.

## When values are destroyed

- A binding is destroyed at the end of the block that binds it, after the
  bindings declared after it. A `var` declared without a value belongs to
  the block declaring it, wherever an assignment gives it one, and is
  destroyed there if it has one.
- A parameter taken `own` is destroyed when the function returns, after the
  function's bindings. So is an owned part of a parameter's pattern or
  product (`_ (a, _): (own T, own T)`) that the pattern leaves unbound.
- A temporary is destroyed at the end of its statement. A `let` that binds
  an access (`let s = ref make().span()`) keeps the statement's temporaries
  until the end of its block, as long as the binding that may borrow them.
- A block whose value is an access keeps its bindings until the end of the
  enclosing block, for the same reason. Here `b` lives as long as `s` may
  view it:

  ```fe
  let s = {
      let b = MemBuffer::alloc(4)
      b.span()
  }
  ```
- A loop body's bindings are destroyed at the end of each iteration.
- `return`, `break`, `continue`, a failing `?` and a `let .. else` that
  leaves destroy what the scopes they leave hold.
- An assignment to an owned place evaluates the new value, destroys the old
  one, then stores the new one. An assignment through a `mut` access or an
  indexed element (`v[i] = x`) does the same.
- A projection's own locals are destroyed when the access it grants ends.

A value that moved is not destroyed where it moved from, and a partial move
destroys only the fields left behind. Where a value moved on some paths
only, a flag decides at run time.

`core::mem::drop(x)` ends a value's lifetime early, and `core::mem::forget(x)`
gives it up without destroying it, which leaks what it owns but is safe.

Destroying a value runs its type's `drop`, if the type implements `Drop`,
then destroys its fields in declaration order: an enum's are those of the
variant it holds, and an array's its elements in order.

```fe
struct Guarded { buffer: MemBuffer, log: Log }

impl Drop for Guarded {
    fn drop(own self) {
        // runs first; then `buffer` and `log` are destroyed
    }
}
```

## `Drop`

- `drop` declares no effects: it runs where no call is written. It may call
  `unsafe` code, such as a host's `free`.
- An implementation covers the whole type (`impl<T> Drop for Buf<T>`), and a
  type with one is never `Copy`.
- Nothing calls `drop` directly; use `core::mem::drop`.
- Nothing moves out of a field of a type that implements `Drop`, except that
  type's own `drop`. A conversion that hands its allocation to another owner
  reads the fields it needs, then `forget`s `self`.
- Compile-time code cannot destroy a value whose destruction might run a
  `drop`: a `const fn` may move such a value, but not let it go out of scope.

A panic runs no destructors: it reverts on EVM, and natively it reports the
panic on stderr and aborts.

## Raw places

Writing or moving through a raw pointer dereference destroys nothing:
`*p = v` initializes `*p` whatever it held, as does a write to a field
through the pointer (`p.field = v` with `p: *S`), and `let v = *p` moves
out without leaving a record. Raw memory is outside ownership, so unsafe code
that fills uninitialized slots writes them through raw places
(`*array.ptr_at(i) = v`), not through `array[i] = v`, which would destroy
the slot's previous, uninitialized contents. `core::ptr::drop_in_place(p)`
destroys the value at `p`.

## Memory owners

`core::ptr` has one family of owners of linear memory. Each frees its
allocation when destroyed:

| Owner | Holds |
| --- | --- |
| `MemBuffer` | a growable byte buffer |
| `FixedMemBuffer<N>` | `N` bytes |
| `MemArray<T>` | a fixed number of elements |
| `MemVec<T>` | a growable array of elements |
| `Box<T>` | one value in its own allocation |

`MemArray`, `MemVec` and `Box` also destroy the elements they hold.
`MemSlice<T>` and `MemSpan` are views of an owner's memory and own nothing.

A request above `2**64 - 1` bytes reverts with `Panic(0x41)`, as
Solidity's does, and a native host out of memory traps. The operations
that make or grow an owner have fallible forms that report failure
instead, leaving the owner as it was: `try_with_capacity`, `try_reserve`,
`try_resize`, `try_push` (which hands the value back), `try_from_fn`,
`try_from_span` and `Box::try_new`. On EVM these fail only for requests
above `2**64 - 1` bytes. Others, such as `extend_from_slice`, allocate
only when their owner lacks the room, so code that must not revert
reserves the room first.

A `Box` lets a type hold itself: a generic type that keeps its argument
behind a pointer, as `Box<T>` and `MemVec<T>` do, does not hold that
argument in place. Destroying such a value recurses once per level, so a
very deep one, such as a long list of boxed nodes, can exhaust the stack.

```fe
struct Node { value: u256, next: Option<Box<Node>> }
struct Tree { value: u256, children: MemVec<Tree> }
```

Bulk work goes through slices: `copy_from` copies a slice of the same
length, `copy_within` moves a range within one, and `fill` and `zero` set
every element.

```fe
dest.slice(offset: 32, len: n).copy_from(source.slice(offset: 0, len: n))
buffer.span().copy_within(source: 0, dest: 5, len: 33)
```

## Targets

Native targets free memory with the host's `free`. An empty owner, such as
`MemVec::new()` or `MemBuffer::empty()`, takes no memory from the host until
it grows. EVM memory is never reclaimed, so there freeing does nothing, and a
destruction that does nothing is not emitted: values of types whose `drop`
has no effect on EVM, such as the memory owners of plain values, compile to
the same code as if they had none. `core::mem::needs_drop<T>()` tells code
that destroys values in bulk whether destroying a `T` does anything on the
target being compiled.
