//! How native code panics. The reports are `core::rt` functions: each reports
//! the panic on stderr, flushes the C streams and aborts, so output written
//! before a panic is not lost. A module that traps instead calls none of them.

use mir::NativeRuntimeFunc;

/// The prefix of the symbols the compiler defines, which only core's
/// `extern` blocks may declare.
pub(super) const COMPILER_SYMBOL_PREFIX: &str = "__fe_";

/// How native code ends at a panic or a revert. The EVM ignores it, and only
/// the `cranelift` feature compiles native code.
#[derive(Clone, Copy, PartialEq, Eq)]
#[cfg_attr(not(feature = "cranelift"), allow(dead_code))]
pub(crate) enum NativePanics {
    /// Report the panic on stderr and abort.
    Abort,
    /// As `Abort`, except that revert data goes to the host's `__fe_revert`,
    /// as the `fe test` launcher defines it.
    HostRevert,
    /// As `HostRevert`, except that a panic without data traps: a WebAssembly
    /// test has no stderr of its own to report it on.
    #[cfg_attr(not(feature = "wasm"), allow(dead_code))]
    HostRevertOrTrap,
    /// Trap without a report.
    Trap,
}

/// A way native code panics.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum NativePanic {
    /// A panic without data, as `core::panic`.
    Trap,
    /// A revert with data.
    Revert,
    /// A revert with Solidity `Panic(uint256)` data.
    Code,
}

impl NativePanics {
    /// The `core::rt` function that ends native code at `panic`, or `None`
    /// when the module traps.
    pub(super) fn function(self, panic: NativePanic) -> Option<NativeRuntimeFunc> {
        Some(match (self, panic) {
            (Self::Trap, _) | (Self::HostRevertOrTrap, NativePanic::Trap) => return None,
            (_, NativePanic::Trap) => NativeRuntimeFunc::ReportTrap,
            (Self::Abort, NativePanic::Revert) => NativeRuntimeFunc::ReportRevert,
            (Self::Abort, NativePanic::Code) => NativeRuntimeFunc::ReportPanic,
            (Self::HostRevert | Self::HostRevertOrTrap, NativePanic::Revert) => {
                NativeRuntimeFunc::HostRevert
            }
            (Self::HostRevert | Self::HostRevertOrTrap, NativePanic::Code) => {
                NativeRuntimeFunc::HostPanic
            }
        })
    }
}
