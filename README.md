# Fe

Fe is a Rust-like, statically typed language for the Ethereum Virtual Machine
(EVM), with explicit effects, message-passing contracts, and an integrated
toolchain.

> **Status:** Fe 26.x is **not production-ready**. See the
> [Fe 26 release announcement](https://blog.fe-lang.org/posts/fe26-a-fresh-start/)
> for context.

- Website: <https://fe-lang.org>
- Docs: <https://fe-lang.org/getting-started/what-is-fe/>
- Blog: <https://blog.fe-lang.org>
- Zulip: <https://fe-lang.zulipchat.com/join/dqvssgylulrmjmp2dx7vcbrq/>

## Install

Use `feup`:

```bash
curl -fsSL https://raw.githubusercontent.com/argotorg/fe/master/feup/feup.sh | bash
```

See <https://fe-lang.org> for language documentation, examples, and other
installation options.

## CLI

The compiler is exposed through the `fe` binary. See [`CLI.md`](./CLI.md) for
the command reference.

## Repository Layout

- `crates/` - compiler crates, CLI, language server, and supporting tools
- `ingots/core/` - `core` ingot, built into every compilation
- `ingots/std/` - Fe standard library
- `feup/` - the `feup` installer script
- `newsfragments/` - release note fragments consumed by eisenbote (see `eisenbote.toml`)

## Development

Run the workspace tests:

```bash
cargo test --workspace
```

For optimized tests with debug assertions enabled, use
`cargo nextest run --cargo-profile test-release --workspace`. CI and `make test`
use this optimized profile too.

Snapshot tests use [`insta`](https://insta.rs/):

```bash
cargo insta review
cargo insta accept --workspace
```

## Contributing

Non-trivial language or architecture changes should start as a discussion on
GitHub or Zulip. For bug fixes and small improvements, a PR against `master` is
fine.

## License

Licensed under the [Apache License, Version 2.0](./LICENSE-APACHE).
