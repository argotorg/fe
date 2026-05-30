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
closure with one primary classification plus multi-label symptoms. Primary
classifications include `good_many_to_many`, `source_only_expected`,
`source_span_sibling_unlowered`, `preopt_elision_gap`, and
`optimized_attribution_gap`; suspicious primary classifications also include
`missing_source_unexplained`. Symptoms include `missing_bytecode`, `too_broad`,
and `foreign_source`.

The audit also groups exact input source spans. A source-only HIR origin that
shares a span with another HIR origin reaching MIR/Sonatina/bytecode is reported
as `source_span_sibling_unlowered`, which is informational rather than
suspicious. Mixed source-span groups are included with target-connected members
and source-only members, including their stable root keys and highest reached
phase, so cases like `a = b` can be reviewed as one source operation without
collapsing distinct HIR identities.

Each closure also reports `highest_phase_reached`, making it easier to sort
agent review by whether a closure stopped at HIR, MIR, Sonatina pre-opt,
Sonatina post-opt, or bytecode. Use `--format json` to produce compact evidence
packs for cheaper agent review. The agent should only review closures that the
deterministic audit flags as suspicious.

Closure construction and audit classification live in `trace-query`, not in the
web demo renderer. The CLI/web layer renders the derived closure and audit
reports so future LSP, agent, or browser views consume the same query behavior.

The web component uses the audit annotations across every representation panel:
source, HIR, MIR, Sonatina, loop contents, and bytecode rows all receive the
same closure badges, phase reachability chips, suspicious/good styling, and
source-span group inspector. Clicking a grouped source span still preserves
per-origin closure cards, so same-span HIR siblings are visible without
collapsing identity.
