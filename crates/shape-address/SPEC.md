# Fe Shape Addressing Spec

This crate computes derived content-address views over phase-owned compiler
identity. Shape hashes are not compiler identity and are not semantic
equivalence claims. Every shape node that represents a compiler entity is keyed
by `OriginExportKey`, or by a derived key owned by an `OriginExportKey`.

## Schema

- `schema_version`: `1`
- `internal_digest_algorithm`: `blake3-256`
- `external_cid_algorithm`: `sha2-256-multihash` reserved for exported CIDs
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
level, selected dimensions, view mode, and cycle policy.

## Dimensions

- `structure`: node kind, ordered child labels/order, graph edge labels,
  endpoint topology, and phase adapter structure fields. It excludes user names,
  literal values, and raw stable keys in anonymous mode.
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

## Edge Taxonomy

- `child`: ordered containment or ownership. It is Merkle-recursive and must be
  acyclic when the cycle policy is `reject`.
- `graph`: CFG, dataflow, call, or reference edge. It is aggregated as sorted
  edge records and is not recursively followed.
- `dependency`: selected recursive dependency edge. It may be cyclic only under
  `condense_scc`.
- `origin`: cross-phase or causal origin edge. It is excluded from per-phase
  shape hashes unless a separate trace/origin view explicitly includes it.

All edge labels and edge direction are hash-sensitive. Graph edges must never
suppress child content hashing.

## View Modes

- `identity_bound`: hash payloads include canonical shape node keys. This mode
  is for durable compiler artifact fingerprints and exact trace/debug linking.
- `anonymous_shape`: hash payloads exclude node keys and use structural
  canonical ordering. This mode is for fuzzing buckets and shape similarity.

Reports must say "same under policy" and must not claim semantic equivalence.

## Cycle Policies

- `reject`: fail when recursive child/dependency edges contain a cycle.
- `non_recursive_graph_edges`: hash acyclic children and aggregate cyclic graph
  edges non-recursively.
- `condense_scc`: compute strongly connected components over selected recursive
  edges, hash each component deterministically, then hash the condensation DAG.

## Golden Fixture Expectations

The fixture corpus in `golden-fixtures.json` defines the mutation matrix that
implementation tests must preserve:

- Rename-only changes affect `names` and identity-bound hashes, but not
  anonymous `structure`, `constants`, or `types`.
- Literal-only changes affect `constants`, but not `structure`, `names`, or
  `types`.
- Type-only changes affect `types`; they affect `structure` only if the adapter
  declares the type constructor structural for that phase.
- Child order changes affect `structure`.
- Child label changes affect `structure`.
- Graph edge insertion order does not affect any digest.
- Graph edge label or direction changes affect `structure`.
- Stable key changes affect identity-bound digests, but not anonymous structure
  digests when node content and topology are unchanged.
- Child/dependency cycles fail under `reject` and hash deterministically under
  `condense_scc`.

