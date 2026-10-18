# Contract storage layout

The layout gives each `mut` contract field a root slot, in declaration order.
Storage, transient storage and code each number their own fields, so a
transient field never moves a persistent one. A field takes as many
consecutive slots as its type needs. Struct fields narrower than a word are
packed into shared slots the way Solidity packs struct members. Immutable
fields live in the contract's code.

A field packs when its value is narrower than a word, like a Solidity value
type: a `bool`, an integer narrower than `u256`, or a newtype of one (a
struct with a single field, which takes its field's bytes). A newtype with a
`std::evm::SolPacked` encoding narrower than its field takes its `BITS`
instead, so `Address` takes 20 bytes, `FixedBytes<N>` takes `N` and the
odd-width integers such as `Uint24` take their width. Its field's value lies
in those bytes, and writing a value they cannot hold reverts, as its
`SolPacked` encoding does. Any other struct, tuple, array or enum takes whole
slots of its own, and the field after it starts a new slot, as in Solidity.

A field that mixes spaces, such as a `Mutex<T>` (a transient lock beside a
storage value), has one root slot that both spaces read: it takes its whole
range in each, and is placed among the storage fields. Fields that lie only in
transient storage fill the transient numbers the others leave free.

A storage collection, such as a `StorageMap`, `SolArray` or `TSlot`, is an
owner at its slot, pinned to the space its `Pinned` implementation names.
It takes whole slots of its own, never sharing one with packed neighbors,
and at least one even when it has no fields, since its slot number is what
identifies its contents: a `StorageMap`'s slot is reserved and never
written, as a Solidity mapping's is. A zero-sized type takes no slot.
It keeps itself and its contents at that slot number in that space: a map's
entry for `key` lies at `keccak256(key ++ slot)` in storage, and a `TSlot`'s
value lies in transient storage from the slot's own number on, taking a slot
per word. A
collection nested in a struct or a map entry derives its slot from where it
lies, as Solidity nests mappings.

## Clearing

A slot no one wrote reads as zero, so in storage and transient storage the
all-zero representation is a value of every type: `false`, the zero
address, an enum's first variant, an empty collection. Clearing an element
zeroes its slots, or only its bits of a slot it shares.
`core::ops::clear_storage(mut c, key)` clears the element at `key` of any
collection whose elements live in such a space; `StorageMap::remove(key)`,
`SolArray::pop()`, `clear_at(i)` and `clear()`, `TSlot::clear()`,
`StorageHeap::pop()` and `SolEnumerableSet::clear()` are built on it.

Only a `core::marker::Clearable` element is cleared: scalars, raw pointers
and aggregates of clearable types, which the compiler recognizes without an
`impl`. A value holding a storage collection is not clearable, since its
collection's entries lie outside its own slots: removing a map entry whose
value holds a map is an error, not a shallow `delete` whose old entries
would reappear. Such a value's scalar fields stay assignable, and its
collections are cleared through their own methods. A map has no `clear`:
its keys cannot be enumerated. Resetting an element to a default value is
an ordinary assignment, `arr[i] = T::default()`.

## Explicit slots

`#[slot(e)]` places a `mut` field at slot `e` instead of the next free one.
`e` is any constant `u256` expression, including `const fn` calls and
`core::keccak`. The field's nested structs and entries derive from `e` as
from any root.

```fe
use core::keccak
use std::evm::{Address, SolArray, StorageMap}

struct Main {
    owner: Address,
    total: u256,
}

pub contract Store {
    // ERC-7201 namespaced storage for the namespace `example.main`.
    #[slot(keccak(keccak("example.main") - 1) & !(0xff as u256))]
    mut main: Main,
    #[slot(0x7a9f)]
    mut legacy: SolArray<u256>,
    // Counted as usual, skipping the slots placed fields take.
    mut balances: StorageMap<Address, u256>,
}
```

- Fields without `#[slot]` are numbered as before, but skip the slots that
  placed fields take in the spaces they take slots in.
- Two placed fields whose slots overlap in one address space are an error.
  Storage and transient storage are separate spaces, so a storage field and
  a transient field may share a slot number. A field takes its slots in
  every space its parts lie in: a `TSlot<u256>` at slot 5 takes transient
  slot 5 only, so a placed storage field may share slot 5 but a transient one may
  not, and a struct holding a `TSlot` and a `u256` takes its slots in both.
- `#[slot]` on an immutable field, which lives in code, or on a struct
  field is an error.
- The checker sees an ordinary root: the attribute changes only where the
  field's slots are.
