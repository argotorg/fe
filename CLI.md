# Fe CLI behavior (current implementation)

This document is a **behavior writeup** for the `fe` CLI as implemented in this repository (crate `crates/fe`).
It is intended as a scratchpad/reference for correctness/UX review; it is not a stability guarantee.

## Conventions

### Global options

- `-v`, `--verbose`: enables verbose resolver output (primarily relevant when resolving dependencies, especially remote dependencies).

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

Notable exceptions:

- `fe tree` currently prints errors but does **not** set a failing exit code (it tends to exit `0` even when it prints `Error:` lines).
- `fe check` / `fe build` on a workspace root with **no members** prints `Warning: No workspace members found` and exits `0`.

### Paths and UTF-8

Many CLI paths use `camino::Utf8PathBuf` internally. If a relevant path (including the current directory) is not valid UTF-8, the CLI may error.

### Colors

Some subcommands emit ANSI-colored output:

- `fe fmt --check` prints colored diffs.
- `fe test` prints colored `ok` / `FAILED`.
- `fe tree` renders cycle nodes in red via ANSI escape codes.

Color emission is not currently uniform across all outputs (e.g. `fe tree` uses explicit escape codes).

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
   - does **not** exist as a filesystem path, and
   - looks like a name (ASCII alphanumeric and `_`), and
   - the current working directory is inside a workspace context that contains a matching member.
   - Note: name lookup uses the workspace’s “default selection” of members. If the workspace `fe.toml` sets `default-members`, only those members are considered by default; non-default members may need to be targeted by path.

### Disambiguation: “name” vs existing path

If the argument both:

- looks like a workspace member name, **and**
- exists as a path,

then the CLI requires that they refer to the **same** workspace member; otherwise it errors with a disambiguation message (e.g. “argument matches a workspace member name but does not match the provided path”).

### Standalone vs ingot context for `.fe` files

For `build` and `check`:

- If you pass a `.fe` file that lives under an **ingot** (nearest ancestor `fe.toml` parses as an ingot config), the command runs in **ingot context** (so imports resolve as they would from the ingot).
- If you pass a `.fe` file that lives under a **workspace root** (nearest ancestor `fe.toml` parses as a workspace config), the command treats the file as **standalone** unless you explicitly target the workspace/ingot by passing a directory or member name.

There is currently **no flag** to force standalone mode when the file is inside an ingot.

## `fe build`

Compiles Fe contracts to EVM bytecode by emitting Yul and invoking `solc`.

### Synopsis

```
fe build [--contract <name>] [--optimize] [--solc <path>] [--out-dir <dir>] [path]
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

### What gets built

#### Standalone `.fe` file target

- The compiler analyzes the file’s top-level module.
- Contracts are discovered and, by default, **all** contracts in that module are built.

#### Ingot target

- The ingot and its dependencies are resolved/initialized.
- Contracts are discovered from the ingot’s root module.
- By default, **all** contracts are built.

#### Workspace root target

Workspace builds use a **flat output directory**:

- All member artifacts are written directly into the same `out` directory.
- Before building, the CLI checks for **artifact name collisions** across workspace members:
  - Artifact filenames are derived from a sanitized contract name (see below).
  - If multiple contracts (possibly from different members) map to the same artifact base name, the build errors and lists the conflicts.

Workspace member selection:

- The set of members considered is the workspace’s “default selection”.
  - If `default-members` is present, only those members are built.
  - Otherwise, all discovered members are built (including any `dev` members).

### Contract selection: `--contract <name>`

- For standalone files and ingots:
  - If `<name>` exists, only that contract is built.
  - If not found, the build errors and prints “Available contracts:” with a list.
- For workspace roots:
  - If **exactly one** workspace member contains the contract, that member is built for that contract.
  - If **zero** members contain the contract, it errors.
  - If **multiple** members contain the contract, it errors and prints a “Matches:” list and a hint to build a specific member by name or path.

### Solc selection: `--solc` and `FE_SOLC_PATH`

`fe build` invokes `solc` using:

1) `--solc <path>` if provided (highest priority),
2) `FE_SOLC_PATH` if set,
3) otherwise `solc` resolved via `PATH`.

If `solc` fails, `fe build` prints an error and a hint:

```
Error: solc failed for contract "<name>": <details>
Hint: install solc, set FE_SOLC_PATH, or pass --solc <path>.
```

### Optimizer

`--optimize` enables the optimizer in the standard JSON input passed to `solc`.

### Artifacts and filenames

For each built contract, `fe build` writes:

- `<out>/<contract>.bin` (deploy bytecode, hex + trailing newline)
- `<out>/<contract>.runtime.bin` (runtime bytecode, hex + trailing newline)

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

## `fe check`

Type-checks and analyzes Fe code (no bytecode output).

### Synopsis

```
fe check [--dump-mir] [--emit-yul-min] [path]
```

If `path` is omitted, it defaults to `.`.

### Inputs

Same target resolution rules as `fe build`:

- `.fe` file, ingot directory, workspace root directory, or workspace member name.

### Workspace behavior

- `fe check <workspace-root>` checks all members in the workspace’s default selection.
  - If the workspace `fe.toml` sets `default-members`, only those member paths are checked.
  - Otherwise, all discovered members are checked.
- If the selection is empty, it prints:

```
Warning: No workspace members found
```

and exits `0`.

### Note: `--core`

The CLI currently accepts `--core`, but it is not wired up (it is ignored).

### Dependency errors

When checking an ingot with dependencies, if downstream ingots have errors, `fe check` prints a summary line:

- `Error: Downstream ingot has errors`
- or `Error: Downstream ingots have errors`

Then, for each dependency with errors, it prints a short header (name/version when available) and its URL, followed by emitted diagnostics.

### Optional outputs

- `--dump-mir`: prints MIR for the root module (only when there are no analysis errors).
- `--emit-yul-min`: prints a minimal Yul output for the root module (only when there are no analysis errors).

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

### Workspace roots

When the target is a workspace root, `fe tree` prints a separate tree per member, each preceded by:

```
== <member name> ==
```

### Diagnostics and exit status

`fe tree` currently prioritizes printing information over strict exit status:

- It may print `Error:` diagnostics (e.g. cycles) and still exit `0`.
- It may print both the diagnostic and a tree rendering in the same invocation.

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
fe test [--filter <pattern>] [--show-logs] [path]
```

### Inputs

- A `.fe` file: tests are discovered in that file.
- A directory: treated as an ingot root (resolved/initialized via `fe.toml`).

### Discovery and filtering

- Tests are functions marked with a `#[test]` attribute.
- `--filter <pattern>` is a substring match against the test’s HIR name, symbol name, and display name.

### Output

- Per-test output is similar to Rust’s harness: `test <name> ... ok` / `FAILED` (colored).
- `--show-logs` prints EVM logs (when available).
- A summary is printed if at least one test ran; if no tests are found, it prints `No tests found` and exits `0`.

### Solc dependency

`fe test` compiles generated Yul using `solc` (via `FE_SOLC_PATH` / `PATH`). There is currently no `--solc` override for `fe test`.

## `fe new`

Creates a new ingot or workspace layout.

### Synopsis

```
fe new [--workspace] [--name <name>] [--version <version>] <path>
```

### Behavior

- `fe new <path>` creates an ingot:
  - `<path>/fe.toml` (ingot config)
  - `<path>/src/lib.fe`
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
