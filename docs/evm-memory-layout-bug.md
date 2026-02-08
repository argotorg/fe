# EVM Memory Layout Bug: Sub-Word Types Corrupt Adjacent Fields

## Summary

The Fe compiler's memory layout for structs packs fields at their natural byte
sizes (e.g. `bool` = 1 byte, `u8` = 1 byte), but the EVM's `MSTORE`/`MLOAD`
instructions always operate on 32-byte words. When a struct contains sub-word
fields, writing one field clobbers the next 31 bytes of memory, corrupting
adjacent fields.

This affects **both** the Yul and Sonatina backends — the bug is in the shared
layout computation (`crates/mir/src/layout.rs`), not the backend codegen.

## Root Cause

### Layout computation gives sub-word offsets

`ty_size_bytes()` returns the natural size of each type:
- `bool` → 1 byte
- `u8` → 1 byte
- `u16` → 2 bytes
- `u256` → 32 bytes

`field_offset_bytes()` sums preceding field sizes, so for:

```fe
struct SFixed {
    mag: u256,   // offset 0,  size 32
    neg: bool,   // offset 32, size 1
}
```

Field `neg` is at byte offset 32, and `mag` is at byte offset 0.

### EVM stores are always 32 bytes wide

Both backends use 32-byte-wide store/load instructions:
- **Sonatina**: `Mstore(addr, val, I256)` / `Mload(addr, I256)`
- **Yul**: `mstore(addr, val)` / `mload(addr)`

When storing `neg` (a `bool`) at offset 32, the instruction writes a full
32-byte word starting at offset 32 — but `neg` only needs 1 byte. The
remaining 31 bytes overwrite whatever follows in memory (typically the next
allocation or another struct's data).

### Concrete example

```
Memory layout of SFixed { mag: u256, neg: bool }:

Offset 0:   [mag: 32 bytes          ]
Offset 32:  [neg: 1 byte][...31 bytes of adjacent memory clobbered...]
```

If two `SFixed` values are allocated consecutively:

```
Offset 0:   [a.mag: 32 bytes]
Offset 32:  [a.neg: 1 byte][CORRUPTED — next 31 bytes overwritten]
Offset 33:  [b.mag: should start here, but a.neg's mstore already clobbered it]
```

## Reproduction

The `mandelbrot.fe` test exercises this bug. The `SFixed` struct has:
```fe
struct SFixed {
    mag: u256,
    neg: bool,  // sub-word field triggers the bug
}
```

Run the test:
```bash
cargo test -p fe --test cli_output mandelbrot -- --nocapture
```

The test fails because `bool` field stores corrupt adjacent memory. As a
workaround, the field can be changed to `neg: u256` (wastes 31 bytes per
struct but avoids corruption).

A minimal reproduction is in `mandelbrot_minimal.fe` — it defines a struct
with `neg: bool` and exercises operator trait dispatch, which triggers the
corruption path.

## Affected Code

### Layout computation (shared)
- `crates/mir/src/layout.rs` — `ty_size_bytes()`, `field_offset_bytes()`,
  `field_offset_bytes_or_word_aligned()`

### Sonatina backend stores
- `crates/codegen/src/sonatina/mod.rs` — `store_word_to_place()` uses
  `Mstore(..., Type::I256)` unconditionally

### Yul backend stores
- `crates/codegen/src/yul/` — `mstore()` is always 32-byte

## Proposed Fix

The fix should be in the **layout layer**, not the backends, so both backends
benefit.

### Option A: Word-padded layout (recommended)

Round every field's size up to 32 bytes for memory layout purposes. This wastes
memory but matches EVM semantics exactly:

```rust
/// Round a type's memory footprint to the next EVM word boundary.
pub fn ty_size_bytes_word_padded(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    let raw = ty_size_bytes(db, ty)?;
    Some(raw.next_multiple_of(WORD_SIZE_BYTES))
}
```

Then use `ty_size_bytes_word_padded()` in `field_offset_bytes()` when computing
memory-space offsets. Storage offsets are already slot-based and unaffected.

Tradeoffs:
- Memory usage increases for structs with sub-word fields
- Simple to implement and reason about
- Consistent with how Solidity handles memory layout
- No changes needed in either backend's codegen

### Option B: Masked stores

Keep packed layout but emit masked read-modify-write sequences for sub-word
stores. For storing a `bool` at offset 32:

```
// Pseudocode:
existing = mload(offset & ~31)         // load aligned word
mask = ~(0xFF << (bit_offset))         // clear target byte
existing = existing & mask
existing = existing | (value << bit_offset)
mstore(offset & ~31, existing)         // write back
```

Tradeoffs:
- Memory-efficient but significantly more complex codegen
- Every sub-word store becomes 4+ instructions
- Both backends would need the masked store logic
- Likely not worth the complexity for EVM where memory is cheap

### Recommendation

**Option A** is strongly preferred. It matches Solidity's behavior, is simple
to implement, and the memory overhead is negligible on EVM (gas cost of memory
expansion is very low compared to computation).

## GitHub Issue Sketch

**Title**: Sub-word struct fields (bool, u8, etc.) corrupt adjacent memory on EVM

**Labels**: `bug`, `codegen`, `evm`

**Body**:

> Struct fields with sub-word types (`bool`, `u8`, `u16`, etc.) are laid out at
> their natural byte size, but EVM `MSTORE` always writes 32 bytes. This causes
> writes to sub-word fields to clobber adjacent memory.
>
> **Reproduction**: `mandelbrot.fe` test with `neg: bool` in `SFixed` struct.
>
> **Affected**: Both Yul and Sonatina backends (shared layout code).
>
> **Proposed fix**: Word-pad field sizes in memory layout computation so every
> field is aligned to 32-byte boundaries. Storage layout (slot-based) is not
> affected.
>
> See `docs/evm-memory-layout-bug.md` for full analysis.
