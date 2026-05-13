# Fe

Fe is a Rust-like, statically typed language for the Ethereum Virtual Machine (EVM), with explicit effects, message-passing contracts, and an integrated toolchain.

> **Status:** Fe 26.x is **not production-ready**. See the [Fe 26 release announcement](https://blog.fe-lang.org/posts/fe26-a-fresh-start/) for context.

- Website: <https://fe-lang.org>
- Docs: <https://fe-lang.org/getting-started/what-is-fe/>
- Blog: <https://blog.fe-lang.org>

## Why Fe?

- **Explicit effects.** A function's `uses` clause declares every capability it needs — storage access, event emission, external calls, contract creation. Side effects are visible in the signature.
- **Explicit mutability.** Bindings, storage fields, and effect parameters are immutable unless marked `mut`. The compiler rejects writes to immutable state.
- **Message passing.** Contracts expose their ABI through `msg` types and handle calls in `recv` blocks, mirroring the EVM's transaction-based execution model.
- **Modern type system.** Pattern matching with exhaustiveness checks, generics with trait bounds, `Option<T>` and `Result<E, T>` instead of nulls, and higher-kinded types.
- **Compile-time evaluation.** `const fn` with mutable locals, loops, and pattern matching; `static_assert(...)` for compile-time checks; Solidity function selectors are computed at compile time via `sol("…")`.
- **Solidity ABI compatibility.** Standard 4-byte selectors, Solidity-compatible custom errors (`#[error]`), `assert_msg(...)` reverts with `Error(string)`, and arithmetic overflow reverts with `Panic(uint256)`.

## A taste of Fe

A vault contract with traits, pattern matching, and explicit effects:

```fe
use std::abi::sol

// Enums as error types
enum VaultError {
    InsufficientFunds,
    ZeroAmount,
}

// Traits define shared behavior
trait Validate {
    fn validate(self) -> Result<VaultError, u256>
}

struct Withdrawal {
    balance: u256,
    amount: u256,
}

impl Validate for Withdrawal {
    fn validate(self) -> Result<VaultError, u256> {
        if self.amount == 0 {
            return Result::Err(VaultError::ZeroAmount)
        }
        if self.balance < self.amount {
            return Result::Err(VaultError::InsufficientFunds)
        }
        Result::Ok(self.balance - self.amount)
    }
}

// Events with indexed fields for efficient filtering
#[event]
struct Deposited {
    #[indexed]
    owner: Address,
    amount: u256,
}

struct VaultStore {
    balances: StorageMap<Address, u256>,
}

// Message interface defines the contract's public ABI
msg VaultMsg {
    #[selector = sol("deposit()")]
    Deposit {},
    #[selector = sol("withdraw(uint256)")]
    Withdraw { amount: u256 },
    #[selector = sol("balanceOf(address)")]
    BalanceOf { addr: Address } -> u256,
}

// Effects declared explicitly, no hidden state access
pub contract Vault uses (ctx: Ctx, log: mut Log) {
    mut store: VaultStore

    recv VaultMsg {
        Deposit {} uses (ctx, mut store, mut log) {
            let who = ctx.caller()
            store.balances.set(
                key: who,
                value: store.balances.get(key: who) + ctx.value()
            )
            log.emit(event: Deposited { owner: who, amount: ctx.value() })
        }

        // Pattern match on Result for control flow
        Withdraw { amount } uses (ctx, mut store) {
            let who = ctx.caller()
            let req = Withdrawal {
                balance: store.balances.get(key: who),
                amount
            }
            match req.validate() {
                Ok(new_bal) => { store.balances.set(key: who, value: new_bal) }
                Err(e) => { revert(e) }
            }
        }

        BalanceOf { addr } -> u256 uses (store) {
            store.balances.get(key: addr)
        }
    }
}
```

See more examples in the [examples section](https://fe-lang.org/examples/erc20/) of the docs.

## Install

### feup (recommended)

```bash
curl -fsSL https://raw.githubusercontent.com/argotorg/fe/master/feup/feup.sh | bash
```

This installs the `fe` compiler and the `feup` toolchain manager into `~/.fe/bin/` and adds it to your `PATH`. Pre-built binaries are available for Linux, macOS (Intel and Apple Silicon), and Windows on x86_64/ARM64.

### Homebrew

```bash
brew install fe-lang/tap/fe
```

### From source

```bash
git clone https://github.com/argotorg/fe.git
cd fe
cargo install --path crates/fe
```

Requires a recent [Rust toolchain](https://rustup.rs/).

## The toolchain

A single `fe` binary ships the full workflow:

| Command          | Purpose                                                  |
| ---------------- | -------------------------------------------------------- |
| `fe new`         | Scaffold a new ingot or workspace                        |
| `fe check`       | Type-check and analyze without producing bytecode        |
| `fe build`       | Compile contracts to EVM bytecode (via Sonatina)         |
| `fe test`        | Run `#[test]` functions in an integrated EVM sandbox     |
| `fe fmt`         | Format Fe source code                                    |
| `fe doc`         | Generate browsable HTML documentation                    |
| `fe tree`        | Show the ingot dependency tree                           |
| `fe lsif`/`scip` | Emit code-navigation indexes                             |

Code is organized into **ingots** (Fe's package format). Multiple ingots can be grouped into a workspace via a top-level `fe.toml`. Dependencies are resolved from local paths or remote git sources via a sparse-checkout-aware resolver.

A separate `fe-language-server` binary provides LSP integration (diagnostics, go-to-definition, completions) for popular editors.

For the full CLI reference, see [`CLI.md`](./CLI.md).

## Debugging tests

`fe test` has a few flags that are useful when debugging runtime/codegen issues:

```bash
# EVM trace, last 400 steps, stack depth 18
RUSTC_WRAPPER= cargo run -q -p fe -- test \
    --trace-evm --trace-evm-keep 400 --trace-evm-stack-n 18 \
    <path/to/test.fe>

# Write EVM traces to files
RUSTC_WRAPPER= cargo run -q -p fe -- test \
    --trace-evm --debug-dir target/fe-debug \
    <path/to/test.fe>
```

## Repository layout

- `crates/` — compiler crates (parser, HIR, type checker, MIR, codegen, CLI, language server, …)
- `ingots/core/` — `core` ingot (built into every compilation)
- `ingots/std/` — Fe standard library
- `feup/` — the `feup` installer script
- `newsfragments/` — release notes fragments (consumed by towncrier)
- `openspec/` — change proposals and specifications (see [`openspec/AGENTS.md`](./openspec/AGENTS.md))

## Contributing

Contributions are welcome. Non-trivial language or architecture changes start as a proposal under `openspec/changes/`; see [`openspec/AGENTS.md`](./openspec/AGENTS.md) for the workflow. For bug fixes and small improvements, a PR against `master` is fine.

To build and test the whole workspace:

```bash
cargo test --workspace --exclude fe
```

Snapshot tests use [`insta`](https://insta.rs/). Run `cargo insta accept --workspace` to accept new snapshots.

## Community

- Zulip: <https://fe-lang.zulipchat.com/join/dqvssgylulrmjmp2dx7vcbrq/> (primary chat)
- Discord: <https://discord.gg/ywpkAXFjZH> (still live, but Zulip is preferred)
- Twitter/X: [@official_fe](https://twitter.com/official_fe)
- Issues: <https://github.com/argotorg/fe/issues>

## License

Licensed under the [Apache License, Version 2.0](./LICENSE-APACHE).
