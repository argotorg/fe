# Test failure tracing

`fe test --explain-failure` combines compiler debug facts with observations from
actual EVM test deployment and execution. It does not replay a test to obtain its
explanation. `--call-trace` and the optional `--trace-evm` ring can observe the same
execution. The ordinary path remains unchanged when the flag is absent.

The compiler emits facts for the exact test wrappers and reachable contract
artifacts returned by that build. The runner checks creation/runtime identities
before resolving PCs. Constructor arguments are separated at compiler-declared
init-code boundaries. An immutable runtime is accepted only after observing a
successful, matching deployment with the compiler-declared tail length and
verifying the deployed prefix; later calls match its full actual hash. Address
reuse does not reuse a source association. DELEGATECALL uses the implementation's
code identity and retains the separate storage-context address.

Source classification and confidence come from the existing DebugBundle policy.
Generated or ambiguous code stays explicit, and an optimized revert may map to a
standard-library helper rather than the user's expression. Nested failures and
matching payloads are observations, not proof of propagation or root cause.
Custom-error decoding uses typed error metadata associated with verified code;
unknown code and selector collisions retain raw bytes. Wrapped bytes fields are
shown structurally without guessing their meaning.

## Capture and report bounds

The default capture retains at most 4,096 frames, 16 KiB per input/output payload,
and 4 MiB of payload bytes overall. Code identity can hash at most 256 distinct
compiler-provided boundaries. Truncation is explicit in JSON and explanations.
Terminal PCs are independent of the opcode ring, so ring eviction cannot erase a
retained frame's terminal instruction. Full step/stack history is opt-in and uses
the existing trace-limit options. Compiler facts and source snapshots scale with
the compiled program and are separate from these execution-capture limits.

Existing report archives gain versioned `execution-trace.json` and
`artifact-manifest.json`, plus `debug-bundle.json`, `compiler-facts.json` and a
rendered `explanation.txt`. Sources live under `sources/by-hash/`, keyed by their
build-time content. Compiler/profile/recovery/optimization metadata and code
identities accompany the facts. Test artifact directories include the full hash
of the test name, and existing suite staging isolates parallel suites. Rendering
uses these snapshots, never the current workspace; a regression test rebuilds
an explanation from archived data after editing the original source.

## Overhead experiment

Run the manual experiments with:

```sh
cargo test -p fe-contract-harness --lib measure_failure_tracing_ -- --ignored --nocapture
```

The following indicative measurements were taken on 2026-09-28 on x86_64 Linux,
on an AMD Ryzen AI 9 365 with Rust 1.97.0, using an unoptimized Rust build
and O0 EVM codegen. Compilation numbers are the
median of three fresh compiler databases, including frontend checks. Execution
numbers are medians of three batches of 1,000 small calls or 100 stress calls.
No timing thresholds are imposed by the tests.

| Measurement | Ordinary | With capture/debug facts | Retained JSON |
| --- | ---: | ---: | ---: |
| Compile a small test | 1,035 ms | 1,064 ms | 271,815 bytes of debug metadata |
| Compile a test deploying/calling a contract | 9,982 ms | 9,967 ms | 19,820,266 bytes of debug metadata |
| Execute STOP | 4.8 µs | 25.2 µs | 803 bytes of execution trace |
| Execute 200 child calls | 334 µs | 4,062 µs | 141,893 bytes of execution trace |

The small compilation differences are within the noise of this local experiment.
These measurements isolate frontend/codegen and harness capture: they exclude
CLI startup, typed error-index construction, source-resolution index construction,
rendering and report I/O. Serialized sizes are not peak heap usage. Debug facts
can be large even for a small source program that reaches substantial library
code. The feature therefore remains opt-in; release-build performance must be
measured separately before considering any default-on behavior.

The first execution experiment also exposed checksum-address formatting in the
recorder. Storing plain hexadecimal addresses reduced the measured small-call
capture from 364 to 25 microseconds and the 200-child capture from 16,761 to
4,062 microseconds. Legacy normalized call-trace formatting remains unchanged.
