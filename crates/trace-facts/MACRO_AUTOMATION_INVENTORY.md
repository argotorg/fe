# Macro Automation Inventory

This document records the pre-macro state for the trace schema and shape
automation push. The goal is to reduce mechanical projection boilerplate while
preserving the SSOT invariant: compiler phases own facts and identities; derived
systems consume those facts.

## Current Boilerplate Surfaces

| Surface | Current state | Automation target |
| --- | --- | --- |
| Trace relation projection | 44 manual `impl TraceRelation for ...` blocks in `crates/trace-facts/src/relation.rs` | Generate relation name, schema, and row encoding from fact structs |
| Trace fact dispatch | `TraceFact` manually matches variants for relation name, schema, and row dispatch | Registry macro or generated companion dispatch |
| Generic validation references | 93 direct `require_node(...)` calls across 54 validator helper functions | Generate primary-key and origin-reference metadata from fact fields |
| Shape adapters | 9 shape adapter entry points manually add local fields/children/edges | Derive local shape fields; keep graph topology phase-owned |
| Legacy analyze facts | `common::facts` remains for `fe analyze --origin-facts` compatibility | Keep quarantined; do not allow trace/debug/query imports |

## Representative Files

| File | What to preserve |
| --- | --- |
| `crates/trace-facts/src/fact.rs` | Fact structs remain the schema SSOT |
| `crates/trace-facts/src/relation.rs` | Relation output must remain byte-for-byte equivalent for migrated facts |
| `crates/trace-facts/src/validate.rs` | Domain-specific validation stays explicit; generic refs can be generated |
| `crates/shape-address/src/lib.rs` | Hashing, ordering, and cycle policies remain centralized |
| `crates/hir/src/shape.rs` | HIR graph ownership remains explicit |
| `crates/mir/src/shape.rs` | MIR graph ownership remains explicit |
| `crates/codegen/src/shape.rs` | Bytecode graph ownership remains explicit |
| `crates/common/src/facts.rs` | Legacy-only fact projection remains quarantined |

## Baseline Checks

Focused baseline checks before macro migration:

```text
cargo test -p fe-trace-facts relation::tests::typed_facts_define_base_relation_schema_and_row -q
```

Result: passed.

```text
cargo test -p fe-codegen bytecode_shape_separates_opcode_structure_from_push_immediate_constants -q
```

Result: passed.

## First Migration Slice

The first macro slice should migrate only:

- `OriginNodeFact`
- `OriginEdgeFact`
- `InstructionFact`
- `InstructionExtentFact`

Acceptance for this slice:

- Existing relation rows remain unchanged.
- Datalog base export still consumes relation projection through the same public API.
- The derive generates primary-key and origin-reference metadata for later generic validation.
- No compiler phase emitter hides semantic decisions behind macros.
