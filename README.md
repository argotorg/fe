
# Fe

The Fe compiler is in the late stages of a major compiler rewrite. The `master` branch supports `fe check`/`fe test` and includes an experimental `fe build` for EVM bytecode generation (default backend: Sonatina), but language/codegen coverage is still evolving.
For the older version of the compiler, see the [legacy branch](https://github.com/ethereum/fe/tree/legacy).

## Overview

Fe is a statically typed language for the Ethereum Virtual Machine (EVM). The syntax and type system is similar to rust's, with the addition of higher-kinded types. We're exploring additional type system, syntax, and semantic changes.

## Debugging tests

`fe test` has a few flags that are useful when debugging runtime/codegen issues:

- EVM trace (last 400 steps): `RUSTC_WRAPPER= cargo run -q -p fe -- test --backend sonatina --trace-evm --trace-evm-keep 400 --trace-evm-stack-n 18 <path/to/test.fe>`
- Sonatina symtab + stackify traces written to files: `RUSTC_WRAPPER= cargo run -q -p fe -- test --backend sonatina --sonatina-symtab --sonatina-stackify-trace --sonatina-stackify-filter solencoder --debug-dir target/fe-debug <path/to/test.fe>`

## Community

- Twitter: [@official_fe](https://twitter.com/official_fe)
- Chat:
  - We've recently moved to [Zulip](https://fe-lang.zulipchat.com/join/dqvssgylulrmjmp2dx7vcbrq/)
  - The [Discord](https://discord.gg/ywpkAXFjZH) server is still live, but our preference is zulip.

## License

Licensed under Apache License, Version 2.0.
