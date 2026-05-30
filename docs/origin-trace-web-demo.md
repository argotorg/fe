# Origin Trace Web Demo

The origin trace web demo is a developer-only trace-through viewer over compiler trace facts.
It is a derived view: compiler phases own origin keys and facts; the browser only renders
their transitive closures across source, HIR, MIR, Sonatina, and bytecode.

## Incremental Source Mode

Run the demo directly from a Fe source file:

```sh
cargo run -p fe -- dev trace web-demo \
  --source fib_demo.fe \
  --serve \
  --port 5179
```

Open `http://127.0.0.1:5179/`.

Source mode defaults to `--optimize 0` for this tech demo because the current
trace stream preserves the clearest source-to-bytecode edges before optimization.
Passing `--optimize 2` is useful for auditing optimized-code attribution, but
some closures may honestly stop at Sonatina post-opt until the backend records
the final PC edges for transformed or moved values. The viewer labels that as an
attribution gap; it should not be read as evidence that the source was dead.

This mode keeps one `DriverDataBase` alive, updates the source `File` salsa input when
the watched file changes, and re-renders from the new trace facts. The cards at the top
show salsa event counters:

- `query execs`: salsa queries that actually executed for this render.
- `memo reuse`: memoized values validated and reused without re-execution.
- `render ms`: end-to-end trace render time for this generation.

Small source edits should produce a much lower `query execs` count and a non-zero
`memo reuse` count after the cold first generation.

Pass `--out /tmp/fe-origin-trace.html` only if you also want the live server to
mirror the current page to disk on each successful render.

## Offline JSONL Mode

Render an existing validated trace bundle:

```sh
cargo run -p fe -- dev trace web-demo \
  --from /tmp/trace.jsonl \
  --out /tmp/fe-origin-trace.html
```

Offline mode is useful for sharing a standalone HTML artifact, but it cannot demonstrate
salsa caching because it consumes already-serialized JSONL.

## Closure Audit

To audit the same transitive closures without opening the browser:

```sh
cargo run -p fe -- dev trace audit-closures \
  --source fib_demo.fe \
  --optimize 2
```

The audit is deterministic and intentionally conservative. It classifies each
closure into buckets such as `good_many_to_many`, `optimized_attribution_gap`,
`source_only`, `missing_bytecode`, `too_broad`, and `foreign_source`. A
`source_only` closure is not automatically suspicious: many HIR subexpressions do
not correspond to standalone target instructions. Use `--format json` to produce
compact evidence packs for cheaper agent review. The agent should only review
closures that the deterministic audit flags as suspicious.
