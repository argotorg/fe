# Fe Shape Addressing Spec

This crate computes derived content-address views over phase-owned compiler
identity. Shape hashes are not compiler identity and are not semantic
equivalence claims. Every shape node that represents a compiler entity is keyed
by `OriginExportKey`, or by a derived key owned by an `OriginExportKey`.

## Schema

- `schema_version`: `1`
- `internal_digest_algorithm`: `blake3-256`
- `canonical_codec`: tagged, length-delimited binary records
- `magic`: `fe.shape`

Every durable hash input starts with:

```text
fe.shape
schema_version(u32)
algorithm(string)
level(string)
dimension(string)
view_mode(string)
cycle_policy(string)
record_tag(string)
length_delimited_payload(...)
```

The policy id is itself a BLAKE3-256 digest over the schema version, algorithm,
level, selected dimensions, view mode, and cycle policy. It is a policy
configuration fingerprint, not a join key: per-dimension digests do not depend
on which other dimensions a policy selects.

Graphs are trees of ordered `child` containment edges plus per-node fields.
Each per-dimension graph digest ends with a reserved trailing `u32` count that
is always zero in schema version 1.

## Dimensions

- `structure`: node kind, ordered child labels/order, and phase adapter
  structure fields. It excludes user names, literal values, and raw stable keys
  in anonymous mode.
- `names`: user-visible identifiers, symbols, field names when semantically
  name-bearing, and display names selected by a phase adapter.
- `constants`: literals, selectors, byte strings, numeric constants, and gas
  constants only when the policy explicitly includes gas.
- `types`: type constructors, widths, storage classes, ABI shapes, pointer or
  location types, and other type-only shape descriptors.
- `trace_events`: compiler events, synthetic-origin classifications, storage
  decisions, optimization snapshots, and instrumentation-only facts.

No field may enter a dimension implicitly. Phase adapters must choose a dimension
for every field they emit.

## View Modes

- `identity_bound`: hash payloads include canonical shape node keys. This mode
  is for durable compiler artifact fingerprints and exact trace/debug linking.
- `anonymous_shape`: hash payloads exclude node keys and use structural
  canonical ordering. This mode is for fuzzing buckets and shape similarity.

Reports must say "same under policy" and must not claim semantic equivalence.

## Cycle Policies

- `reject`: fail when child edges contain a cycle. This is the only policy in
  schema version 1.

## Golden Digests

Concrete digest values are pinned by the `golden_digests_are_pinned` test in
`src/lib.rs`. Any change to the preimage encoding (field order, separators,
length prefixes, the reserved trailing count) fails that test and requires a
deliberate, versioned format change. The mutation matrix the implementation
tests preserve:

- Rename-only changes affect `names` and identity-bound hashes, but not
  anonymous `structure`, `constants`, or `types`.
- Literal-only changes affect `constants`, but not `structure`, `names`, or
  `types`.
- Type-only changes affect `types`; they affect `structure` only if the adapter
  declares the type constructor structural for that phase.
- Child order changes affect `structure`.
- Child label changes affect `structure`.
- Stable key changes affect identity-bound digests, but not anonymous structure
  digests when node content and topology are unchanged.
- Child cycles fail under `reject`.
