# Fe CLI behavior (current implementation)

This document describes the **current** behavior of the `fe` CLI as implemented in this repository (crate `crates/fe`).
It is not a stability guarantee.

## Conventions

### Global options

- `--color <auto|always|never>`: controls colored output (default: `auto`).

### Output streams

- **Stdout**: “normal” command output (e.g. artifact paths, formatted file paths, dependency trees).
- **Stderr**: diagnostics, errors, warnings, and hints.

### Message prefixes

User-facing diagnostics follow these conventions:

- `Error: ...` for errors (typically causes non-zero exit for `build`, `check`, `test`, `fmt --check`).
- `Warning: ...` for non-fatal warnings.
- `Hint: ...` for suggestions following an error.

The CLI intentionally does **not** use emoji/icon markers.

### Exit codes

- `0` on success.
- `1` on failure.
- `2` on CLI usage/argument parsing errors (emitted by `clap`, e.g. unknown flags or missing values).

Notable exceptions:

- `fe check` / `fe build` / `fe tree` on a workspace root with **no members** prints a warning explaining why (no members configured, or configured paths don't exist) and exits `0`.

### Paths and UTF-8

Many CLI paths use `camino::Utf8PathBuf` internally. If a relevant path (including the current directory) is not valid UTF-8, the CLI may error.

### Colors

Some subcommands emit ANSI-colored output:

- `fe fmt --check` prints colored diffs.
- `fe test` prints colored `ok` / `FAILED`.
- `fe tree` renders cycle nodes in red via ANSI escape codes.

Color emission is controlled by `--color` and respects common environment conventions (`CLICOLOR_FORCE`, `NO_COLOR`, `CLICOLOR`) in `auto` mode.

## Target resolution (paths vs workspace member names)

Several subcommands take a single “target” argument which can be:

- a **standalone** `.fe` file,
- a **directory** containing `fe.toml` (ingot root or workspace root),
- or a **workspace member name** (when run from within a workspace context).

The general rules are:

1) **`fe.toml` file paths are rejected**. Pass the containing directory instead.
2) A path that is a **file** must end in `.fe` to be treated as a source file.
3) A path that is a **directory** must contain `fe.toml` to be treated as a project.
4) A **workspace member name** is only considered when the argument:
   - looks like a name (ASCII alphanumeric and `_`), and
   - the current working directory is inside a workspace context that contains a matching member.
   - If the argument also exists as a filesystem path, the CLI requires that the name and path refer to the same member (see disambiguation below).
   - Note: name lookup uses the workspace’s “default selection” of members. If the workspace `fe.toml` sets `default-members`, only those members are considered by default; non-default members may need to be targeted by path.

### Disambiguation: “name” vs existing path

If the argument both:

- looks like a workspace member name, **and**
- exists as a path,

then the CLI requires that they refer to the **same** workspace member; otherwise it errors with a disambiguation message (e.g. “argument matches a workspace member name but does not match the provided path”).

### Standalone vs ingot context for `.fe` files

For `build` and `check`:

- If you pass a `.fe` file that lives under an **ingot** (nearest ancestor `fe.toml` parses as an ingot config), the command runs in **ingot context** (so imports resolve as they would from the ingot).
  - Override: `--standalone` forces standalone mode for that `.fe` file target.
- If you pass a `.fe` file that lives under a **workspace root** (nearest ancestor `fe.toml` parses as a workspace config), the command treats the file as **standalone** unless you explicitly target the workspace/ingot by passing a directory or member name.

## `fe build`

Compiles Fe contracts to EVM bytecode.

- `--backend sonatina` (default): generates bytecode directly (no `solc` required).
- `--backend yul`: emits Yul and invokes `solc`.

### Synopsis

```
fe build [--standalone] [--contract <name>] [--backend <backend>] [--opt-level <level>] [--optimize] [--solc <path>] [--out-dir <dir>] [--report [--report-out <out>] [--report-failed-only]] [path]
```

If `path` is omitted, it defaults to `.`.

### Inputs

`fe build` accepts:

- a `.fe` file path (standalone mode unless the file is inside an ingot; see above),
- an ingot directory (contains `fe.toml` parsing as `[ingot]`),
- a workspace root directory (contains `fe.toml` parsing as `[workspace]`),
- a workspace member name (when run from inside a workspace).

### Output directory

- Standalone `.fe` file default: `<file parent>/out`
- Ingot directory default: `<ingot root>/out`
- Workspace root default: `<workspace root>/out`
- Override: `--out-dir <dir>`
  - If `<dir>` is relative, it is resolved relative to the **current working directory**.
  - Note: default output directories are derived from the **canonicalized** (absolute) target path, so the printed `Wrote ...` paths are absolute by default unless `--out-dir` is set.

### What gets built

#### Standalone `.fe` file target

- The compiler analyzes the file’s top-level module.
- Contracts are discovered and, by default, **all** contracts in that module are built.

#### Ingot target

- The ingot and its dependencies are resolved/initialized.
- `src/lib.fe` is treated as the ingot’s root module. If it is missing, the CLI behaves as if an empty `src/lib.fe` existed (a “phantom” root module).
- Contracts are discovered across the ingot’s entire source set (`src/**/*.fe` top-level modules), not only `src/lib.fe`.
- By default, **all** discovered contracts are built.

#### Workspace root target

Workspace builds use a **flat output directory**:

- All member artifacts are written directly into the same `out` directory.
- Before building, the CLI checks for **artifact name collisions** across workspace members:
  - Artifact filenames are derived from a sanitized contract name (see below).
  - Collision detection is **case-insensitive** (e.g. `Foo` and `foo` collide) to avoid filesystem-dependent behavior on case-insensitive filesystems.
  - If multiple contracts (possibly from different members) map to the same artifact base name, the build errors and lists the conflicts.

Workspace member selection:

- The set of members considered is the workspace’s “default selection”.
  - If `default-members` is present, only those members are built.
  - Otherwise, all discovered members are built (including any `dev` members).
- Workspace builds skip members with **zero** contracts; if all selected members have zero contracts, the build fails with `Error: No contracts found to build`.

### Contract selection: `--contract <name>`

- For standalone files and ingots:
  - If `<name>` exists, only that contract is built.
  - If not found, the build errors and prints “Available contracts:” with a list.
- For workspace roots:
  - If **exactly one** workspace member contains the contract, that member is built for that contract.
  - If **zero** members contain the contract, it errors.
    - It also prints “Available contracts:” for the workspace (unique contract names), capped at `50`, followed by `... and N more` if applicable.
  - If **multiple** members contain the contract, it errors and prints a “Matches:” list and a hint to build a specific member by name or path.

### Backend selection: `--backend <backend>`

`fe build` currently supports:

- `sonatina` (default): directly generates EVM bytecode via Sonatina IR.
  - `--opt-level <level>` controls Sonatina optimizations (see below).
  - `--solc` is ignored; if passed, it prints `Warning: --solc is only used with --backend yul; ignoring --solc`.
  - `FE_SOLC_PATH` is ignored.
- `yul`: emits Yul and invokes `solc` to produce bytecode.

### Solc selection (Yul backend only): `--solc` and `FE_SOLC_PATH`

When `--backend yul`, `fe build` invokes `solc` using:

1) `--solc <path>` if provided (highest priority),
2) `FE_SOLC_PATH` if set,
3) otherwise `solc` resolved via `PATH`.

If `solc` fails, `fe build` prints an error and a hint:

```
Error: solc failed for contract "<name>": <details>
Hint: install solc, set FE_SOLC_PATH, or pass --solc <path>.
```

### Optimization

Optimization is controlled by `--opt-level <level>` (and the `--optimize` shorthand).

Defaults:

- `--opt-level` defaults to `1` (for both backends).

Backend behavior:

- Yul backend: `--opt-level 0` disables the solc optimizer, and `--opt-level 1` / `2` enables it.
  - Note: `--opt-level 2` currently has no additional effect over `--opt-level 1` for solc; the CLI prints a warning.
- Sonatina backend: `--opt-level <level>` controls Sonatina optimizations:
  - `0`: none
  - `1`: balanced (default)
  - `2`: aggressive

`--optimize` is shorthand for `--opt-level 1`.

- If you pass `--optimize --opt-level 0`, the CLI errors.
- If you pass `--optimize --opt-level 2`, it has no effect.

### Artifacts and filenames

For each built contract, `fe build` writes:

- `<out>/<contract>.bin` (deploy bytecode, hex + trailing newline)
- `<out>/<contract>.runtime.bin` (runtime bytecode, hex + trailing newline)

For the Sonatina backend, `.bin` is the **init section** bytes and `.runtime.bin` is the **runtime section** bytes.

The on-screen output is per-artifact:

```
Wrote <out>/<name>.bin
Wrote <out>/<name>.runtime.bin
```

Filenames are “sanitized” from contract names:

- Allowed: ASCII alphanumeric, `_`, `-`
- Other characters become `_`
- If the sanitized name is empty, it becomes `contract`

This sanitization is also what the workspace collision check uses.

### Reports: `--report`

`fe build` can optionally write a `.tar.gz` debugging report (useful for sharing failures):

- `--report`: enable report generation.
- `--report-out <out>`: output path (default: `fe-build-report.tar.gz`).
- `--report-failed-only`: only write the report if `fe build` fails.

The build report is best-effort and includes:

- `inputs/`: the ingot or `.fe` file inputs (same rules as `fe check` / `fe test`).
- `artifacts/`: emitted IR (e.g. Yul) and bytecode artifacts (when available).
- `errors/`: best-effort captured errors/panics (if any).
- `meta/`: environment and tool metadata.

## `fe check`

Type-checks and analyzes Fe code (no bytecode output).

### Synopsis

```
fe check [--standalone] [--dump-mir] [--report [--report-out <out>] [--report-failed-only]] [path]
```

If `path` is omitted, it defaults to `.`.

### Inputs

Same target resolution rules as `fe build`:

- `.fe` file, ingot directory, workspace root directory, or workspace member name.

### Workspace behavior

- `fe check <workspace-root>` checks all members in the workspace’s default selection.
  - If the workspace `fe.toml` sets `default-members`, only those member paths are checked.
  - Otherwise, all discovered members are checked.
- If the selection is empty, it prints a warning explaining why (no members configured, or configured paths don't exist on disk) and exits `0`.

### Dependency errors

When checking an ingot with dependencies, if downstream ingots have errors, `fe check` prints a summary line:

- `Error: Downstream ingot has errors`
- or `Error: Downstream ingots have errors`

Then, for each dependency with errors, it prints a short header (name/version when available) and its URL, followed by emitted diagnostics.

### Optional outputs

- `--dump-mir`: prints MIR for the root module (only when there are no analysis errors).

### Reports: `--report`

`fe check` can optionally write a `.tar.gz` debugging report (useful for sharing failures):

- `--report`: enable report generation.
- `--report-out <out>`: output path (default: `fe-check-report.tar.gz`).
- `--report-failed-only`: only write the report if `fe check` fails.

The check report is analysis-only and includes:

- `inputs/`: the ingot or `.fe` file inputs.
- `errors/`: diagnostics output (when available).
- `artifacts/`: MIR dump (when available).
- `meta/`: environment and tool metadata.

## `fe tree`

Prints the ingot dependency tree.

### Synopsis

```
fe tree [path]
```

If `path` is omitted, it defaults to `.`.

### Inputs

`fe tree` accepts:

- a directory path (ingot root or workspace root),
- a workspace member name (when run from inside a workspace).

Unlike `build`/`check`, `fe tree` does not take a `.fe` file target.

### Output format

The tree output is a text tree using `├──`/`└──` connectors.

Annotations:

- Cycle closures are labeled with ` [cycle]`.
- Local → remote edges are labeled with ` [remote]`.
- Nodes that are part of a cycle are rendered in red via ANSI escape codes.
  - This respects `--color` (and `NO_COLOR` in `auto` mode).

### Workspace roots

When the target is a workspace root, `fe tree` prints a separate tree per member, each preceded by:

```
== <member name> ==
```

### Diagnostics and exit status

If `fe tree` prints any `Error:` diagnostics (including ingot initialization diagnostics like dependency cycles), it exits `1`.

Even when it exits `1`, it still prints the dependency tree for the target (best-effort).

## `fe fmt`

Formats Fe source code.

### Synopsis

```
fe fmt [path] [--check]
```

### Inputs

- If `path` is a file: formats that single file.
- If `path` is a directory: formats all `.fe` files under that directory (recursive).
- If `path` is omitted: finds the current project root (via `fe.toml`) and formats all `.fe` files under `<root>/src`.

### `--check`

- Does not write changes.
- Prints a unified diff for each file that would change.
- Exits `1` if any files are unformatted (or if IO errors occur).

## `fe test`

Runs Fe tests via the test harness (revm-based execution).

### Synopsis

```
fe test [--filter <pattern>] [--jobs <n>] [--show-logs] [--debug[=<mode>]] [--backend <backend>] [--solc <path>] [--opt-level <level>] [--optimize] [--trace-evm] [--trace-evm-keep <n>] [--trace-evm-stack-n <n>] [--sonatina-symtab] [--debug-dir <dir>] [--report [--report-out <out>]] [--report-dir <dir> [--report-failed-only]] [--call-trace] [path]...
```

### Inputs

- Zero or more paths (files or directories).
- Supports glob patterns (e.g. `crates/fe/tests/fixtures/fe_test/*.fe`).
- When omitted, defaults to the current project root (like `cargo test`).

### Discovery and filtering

- Tests are functions marked with a `#[test]` attribute.
- `--filter <pattern>` is a substring match against the test’s name.

### Execution

- `--jobs <n>` controls how many suites run in parallel (`0` = auto).

### Debugging

- `--debug[=<failures|all>]` prints Yul output when using the Yul backend.
- `--trace-evm`, `--trace-evm-keep`, `--trace-evm-stack-n` enable EVM opcode tracing.
- `--sonatina-symtab` dumps the Sonatina runtime symbol table (function offsets/sizes).
- `--debug-dir <dir>` writes debug outputs (traces, symtabs) into a directory.
- `--call-trace` prints a normalized call trace for each test (backend comparison).

### Output

- Per-test output is similar to Rust’s harness: `test <name> ... ok` / `FAILED` (colored).
- `--show-logs` prints EVM logs (when available).
- A summary is printed if at least one test ran.
- If a suite has no tests, it prints `Warning: No tests found in <path>` and continues (exit code is still `0` if there are no failures elsewhere).

### Reports: `--report` and `--report-dir`

- `--report` writes a single `.tar.gz` report (default output: `fe-test-report.tar.gz`).
- `--report-dir <dir>` writes one `.tar.gz` report per input suite into `<dir>` (useful with globs).
- `--report-failed-only` only writes per-suite reports for failing suites (requires `--report-dir`).

### Solc dependency

Default backend is `sonatina`.

When `--backend yul`, `fe test` compiles generated Yul using `solc` selected by:

1) `--solc <path>` if provided (highest priority),
2) `FE_SOLC_PATH` if set,
3) otherwise `solc` resolved via `PATH`.

Optimization flags:

- Optimization is controlled by `--opt-level <level>` (and the `--optimize` shorthand).
- Sonatina backend: `--opt-level <level>` controls the optimization pipeline.
- Yul backend: `--opt-level 0` disables the solc optimizer, and `--opt-level 1` / `2` enables it.
  - Note: `--opt-level 2` currently has no additional effect over `--opt-level 1` for Yul/solc; the CLI prints a warning.
- `--optimize` is shorthand for `--opt-level 1`.
  - If you pass `--optimize --opt-level 0`, the CLI errors.

When `--backend sonatina`, `fe test` generates bytecode directly and does not require `solc` (`--solc` is ignored with a warning).

## `fe new`

Creates a new ingot or workspace layout.

### Synopsis

```
fe new [--workspace] [--name <name>] [--version <version>] <path>
```

### Behavior

- `fe new <path>` creates an ingot:
  - `<path>/fe.toml` (ingot config)
  - `<path>/src/lib.fe` (conventional root module; missing `src/lib.fe` is treated as an empty root module)
- `fe new --workspace <path>` creates a workspace root with `<path>/fe.toml`.

Safety checks:

- Refuses to overwrite an existing `fe.toml` or `src/lib.fe`.
- Errors if the target path exists and is a file.

Workspace suggestion:

- After creating an ingot, if an enclosing workspace is detected, `fe new` may print a suggestion to add the ingot path to the workspace’s `members` (or `members.main` for grouped configs).
- If workspace member discovery fails, it prints a warning:

```
Warning: failed to check workspace members: <details>
```

## `fe completion`

Generates shell completion scripts for `fe`.

### Synopsis

```
fe completion <shell>
```

This writes the completion script to **stdout**. Supported shells are determined by `clap_complete` (commonly: `bash`, `zsh`, `fish`, `powershell`, `elvish`).

## `fe lsif`

Generates an LSIF index for code navigation.

### Synopsis

```
fe lsif [-o, --output <path>] [path]
```

- If `path` is omitted, it defaults to `.`.
- If `--output` is omitted, the index is written to **stdout**.

## `fe scip`

Generates a SCIP index for code navigation.

### Synopsis

```
fe scip [-o, --output <path>] [path]
```

- If `path` is omitted, it defaults to `.`.
- `--output` defaults to `index.scip`.
