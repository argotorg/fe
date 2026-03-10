# Vendored Dependencies

## tree-sitter.js + tree-sitter.wasm

The web-tree-sitter runtime (Emscripten build of the tree-sitter C library).

- **Source:** https://github.com/tree-sitter/tree-sitter
- **License:** MIT (see LICENSE-tree-sitter)
- **Version:** 0.24.5

These files are the Emscripten/WASM build of tree-sitter's C core, used for
client-side syntax highlighting in the browser. They are not built from this
repository.

## tree-sitter-fe.wasm

The Fe language grammar compiled to WASM. Built from `crates/tree-sitter-fe/`.
To rebuild after grammar changes:

```sh
cd crates/tree-sitter-fe
tree-sitter build --wasm .
cp tree-sitter-fe.wasm ../fe-web/vendor/
```

Requires `tree-sitter` CLI and `emcc` (Emscripten).
