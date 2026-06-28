# tree-sitter-fe

The [tree-sitter](https://tree-sitter.github.io/) grammar for the Fe language.

## Source vs. generated files

The grammar is defined in **`grammar.js`**, with a hand-written external scanner
in **`src/scanner.c`**. These are the only sources tracked in git.

The tree-sitter CLI compiles `grammar.js` into a parser. Those outputs are
**not tracked in git** (they're large build artifacts that bloat diffs — a small
grammar change can rewrite tens of thousands of lines of `parser.c`):

- `src/parser.c`
- `src/grammar.json`
- `src/node-types.json`
- `src/tree_sitter/` (runtime headers emitted by the CLI)

They are regenerated from `grammar.js` automatically.

## Building and testing

`cargo build` / `cargo test` regenerate `src/parser.c` on demand via
`bindings/rust/build.rs` whenever it is missing or `grammar.js` is newer. This
requires a `tree-sitter` CLI:

- **Preferred (pinned, reproducible):** install the version pinned in
  `package-lock.json` into `node_modules/`:

  ```sh
  npm ci --ignore-scripts      # installs deps without the unused Node addon build
  npm rebuild tree-sitter-cli  # fetches the pinned tree-sitter CLI binary
  ```

  `build.rs` prefers `node_modules/.bin/tree-sitter` when present.

- **Fallback:** any `tree-sitter` CLI on your `PATH` (output may differ slightly
  by CLI version, but `--abi=14` keeps it compatible with the `tree-sitter`
  0.24.x runtime).

From the repo root, `make test` performs the pinned-CLI generation and then runs
the workspace test suite, including the grammar test
(`crates/parser/tests/tree_sitter_parse.rs`, which parses every `.fe` fixture
against the freshly generated grammar). CI performs the same generation step
before building.
