# Fe Workspaces

Fe supports **workspaces**: a single root `fe.toml` that groups multiple ingots (“packages”) and enables:

- Member discovery via paths or globs (e.g. `ingots/*`).
- Dependency resolution between members by `name`.
- Workspace-level defaults (e.g. a shared version for members that omit one).

> Note: “Workspace” here refers to Fe ingot workspaces (`fe.toml`), not the Rust/Cargo workspace used to build this repository.

This document describes the workspace feature as implemented in the current codebase.

## Concepts

- **Workspace root**: a directory containing a `fe.toml` that parses as a workspace config.
- **Workspace member**: an ingot directory listed (directly or via glob) under the workspace’s `members`.
- **Ingot**: a directory containing a `fe.toml` that parses as an ingot config plus a `src/` tree.

## Minimal layout

```
my-ws/
  fe.toml               # workspace root config
  ingots/
    app/
      fe.toml           # ingot config
      src/lib.fe
    lib/
      fe.toml
      src/lib.fe
```

## Workspace config (`fe.toml`)

The canonical form is a `[workspace]` table:

```toml
[workspace]
name = "my-ws"          # optional
version = "0.1.0"       # optional; can be used as a default member version

# Members can be a flat array…
members = ["ingots/*"]

# …or a grouped table:
# members = { main = ["ingots/*"], dev = ["examples/*"] }

# Optional: restrict the default selection to specific member *paths*.
# If omitted, the default selection is all members.
# default-members = ["ingots/app"]

# Optional: glob patterns to exclude from member discovery.
# exclude = ["target", "ingots/**/generated/*"]
```

Note: `members` is required; a workspace without members emits a diagnostic.

### Member entries

Each entry in `members` can be:

- A string path or glob pattern (relative to the workspace root), e.g. `"ingots/*"`.
- An inline table with a `path` plus optional `name` and `version`:

```toml
[workspace]
members = [
  { path = "ingots/core", name = "core", version = "0.1.0" },
  { path = "ingots/std",  name = "std" },
]
```

Behavior:

- If `name` and/or `version` are provided, they are treated as **constraints** and must match the member’s ingot metadata; otherwise workspace initialization fails with a metadata mismatch error.
- If a member entry includes `name` or `version`, its `path` must not include glob metacharacters (`*`, `?`, `[`).
- Workspace member names must be unique within a workspace; multiple members with the same `name` are rejected (even if their versions differ).

### Member selection (`default-members`, `members.main`/`members.dev`)

Workspaces support two ways to group members:

- A flat `members = ["…"]` array.
- A grouped `members = { main = […], dev = […] }` table.

`default-members` (if present) restricts the **default selection** to a specific list of member *paths*.
This applies after member discovery, so you can select specific members even if `members` uses globs (e.g. `members = ["ingots/*"]` with `default-members = ["ingots/app"]`).

Current CLI behavior:

- `fe check <workspace-root>` and `fe tree <workspace-root>` operate on the **default selection**:
  - If `default-members` is present, only those member paths are used (they may come from either `main` or `dev`).
  - If `default-members` is not present, all discovered members are used (including any `dev` members).

### Default member version

If a workspace has `version = "…"`, and a member ingot omits `[ingot].version`, the driver uses the workspace version as a fallback for that member during initialization.

### Other workspace fields

The workspace parser also accepts:

- `[scripts]` (string map)
- `[resolution]` (table; unknown keys are preserved)
- `[profiles]` and `[metadata]` (tables)

These fields are parsed and validated. `[dependencies]` is also used for workspace-level dependency imports (see below).

## Workspace-level dependencies (member → workspace dependency)

Workspaces can define shared dependencies in the root `fe.toml` under a top-level `[dependencies]` table (same shape as ingot dependencies):

```toml
[workspace]
members = ["ingots/*"]

[dependencies]
utils = { path = "/home/alice/projects/my-ws/vendor/utils", name = "utils", version = "0.1.0" }
remote_math = { source = "https://example.com/fe.git", rev = "abcd1234", path = "ingots/math" }
```

Members can “import” a workspace dependency by referencing its alias with `= true` (or with an optional version):

```toml
[dependencies]
utils = true
```

Resolution rules (current behavior):

- For `alias = true` / `alias = "x.y.z"` / `alias = { ... }` that resolve as `WorkspaceCurrent`, the resolver first checks whether the current workspace defines a dependency with that `alias`. If so, it uses the workspace-defined dependency.
- If the workspace does not define a dependency with that alias, the resolver falls back to workspace-member-by-name lookup (see “Workspace dependency resolution (member → member)”).
- Workspace dependency aliases must not conflict with workspace member names; if they do, workspace initialization fails with a conflict diagnostic.

## Member discovery and safety checks

Member discovery expands `members` and applies `exclude` patterns relative to the workspace root.

Safety checks enforced during discovery:

- Member globs and exclude globs are not allowed to “escape” the workspace root directory (including via `..` segments or symlinks).
- Only directories are treated as members (non-directories are skipped).

## Workspace dependency resolution (member → member)

Within an ingot, dependency entries can resolve to a workspace member when the dependency has a `name` (and optionally a `version`):

```toml
[dependencies]
lib = true             # resolves by name "lib" within the current workspace
lib = "0.1.0"          # resolves by name "lib" + version "0.1.0"
lib = { name = "lib" } # same as `true`, but explicit
```

Resolution rules (current behavior):

- If the current ingot is part of a workspace, the resolver looks up members by `name` (workspace member names are required to be unique).
- If a `version` is provided, it must match the selected member’s metadata.
- If a workspace member cannot be found (or the current ingot can’t be associated with a workspace), resolution fails with a workspace lookup/resolution error.

Remote workspaces are supported: a git dependency pointing at a workspace root can resolve a specific member by name (optionally requiring a specific version).

## CLI behavior

`fe check` and `fe tree` accept:

- A directory path (workspace root or ingot root).
- A single `.fe` file path (standalone mode).
- A workspace member **name** (if run from within a workspace context).

`fe.toml` file paths are intentionally rejected; pass the containing directory instead.

Creating projects:

- `fe new --workspace <path>` creates a workspace root with `fe.toml` (members are empty by default).
- `fe new <path>` creates a single ingot; if run inside an existing workspace, it prints a suggestion to add the ingot’s relative path to the workspace’s members.

Notes on `fe new`:

- `fe new` refuses to overwrite existing `fe.toml` / `src/lib.fe` files.
- When run inside an enclosing workspace, `fe new` prints a member suggestion as follows:
  - If `members` is a table, it suggests adding the new path to `members.main`.
  - If the new ingot is already covered by an existing `members` glob (or is already listed explicitly), it prints no suggestion.
