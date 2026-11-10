# Inline and cold functions

Attributes on a function guide how the compiler builds its calls. They change
the code generated, never what it does.

| Attribute | Meaning |
|---|---|
| `#[inline]` | Prefer inlining the function into its callers, within the optimizer's growth budget. |
| `#[inline(always)]` | Inline the function at every call. |
| `#[inline(never)]` | Never inline the function. |
| `#[cold]` | The function is rarely called. |

## `#[cold]`

A cold function is one a program calls only off its usual path, such as one
that reports a fault. One attribute on the function covers every call to it:

```fe
#[cold]
fn out_of_gas(pc: usize) -> Status {
    Status::OutOfGas { pc }
}

fn charge(gas: u64, cost: u64, pc: usize) -> Status {
    if cost > gas {
        return out_of_gas(pc)
    }
    Status::Charged { left: gas - cost }
}
```

- The optimizer does not inline a cold function into a caller that is not
  cold itself, so its code stays in one place, out of its callers. A cold
  caller inlines it like any other function.
- In native code, a block that calls a cold function, or ends in a tail call
  to one, is laid out after the rest of its function, and a branch to it
  falls through to its other side. An object places cold functions after
  all the others.
- On the EVM only the first applies: the block layout is unchanged.

`#[cold]` applies to free functions, methods, trait methods and `extern`
functions; native code lays out a call to a cold `extern` function as it
does one to a cold Fe function. On a trait method it covers the trait's
default body; an impl's method takes its own attributes. On any other item
`#[cold]` is an error, as are `#[cold(..)]` and `#[cold = ..]`.

## With `#[inline]`

`#[cold]` combines with each `#[inline]` form:

- `#[cold] #[inline(always)]` inlines the function at every call, as
  `#[inline(always)]` alone does. An inlined call no longer marks its path
  as cold.
- `#[cold] #[inline]` inlines the function only into cold callers: `#[cold]`
  outweighs the preference.
- `#[cold] #[inline(never)]` never inlines the function. `#[cold]` alone
  already keeps it out of callers that are not cold.
