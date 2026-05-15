/// Dispatch strategies for contract/actor method invocation across targets.
///
/// Fe's contract model is fundamentally an actor model:
/// - `recv` arms = message handlers
/// - `init` = constructor
/// - synthetic runtime_root = mailbox dispatcher
/// - effects = capabilities
///
/// Each target implements the mailbox differently:
/// - EVM: calldata selector-based dispatch (4-byte function selector)
/// - WASM: exported functions (each recv arm = named export)
/// - Native: symbol table entries (each recv arm = extern "C" fn)

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DispatchStrategy {
    /// EVM: ABI-encoded calldata with 4-byte function selectors.
    /// The runtime_root function reads calldata, extracts the selector,
    /// and dispatches to the matching recv arm via conditional branches.
    Evm {
        /// Whether to include the fallback/receive function
        has_fallback: bool,
    },

    /// WASM: Each recv arm becomes a named WASM export.
    /// No dispatcher needed — the host calls exports directly.
    /// Storage effects are provided via host imports.
    Wasm {
        /// Import namespace for storage/host calls
        import_namespace: String,
    },

    /// Native: Each recv arm becomes an extern "C" symbol.
    /// No dispatcher needed — the linker resolves calls.
    Native,
}

impl DispatchStrategy {
    pub fn evm() -> Self {
        Self::Evm { has_fallback: false }
    }

    pub fn wasm() -> Self {
        Self::Wasm {
            import_namespace: "env".to_string(),
        }
    }

    pub fn native() -> Self {
        Self::Native
    }

    pub fn needs_synthetic_root(&self) -> bool {
        matches!(self, Self::Evm { .. })
    }

    pub fn needs_abi_encoding(&self) -> bool {
        matches!(self, Self::Evm { .. })
    }

    pub fn exports_recv_arms_directly(&self) -> bool {
        matches!(self, Self::Wasm { .. } | Self::Native)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dispatch_strategy_properties() {
        let evm = DispatchStrategy::evm();
        assert!(evm.needs_synthetic_root());
        assert!(evm.needs_abi_encoding());
        assert!(!evm.exports_recv_arms_directly());

        let wasm = DispatchStrategy::wasm();
        assert!(!wasm.needs_synthetic_root());
        assert!(!wasm.needs_abi_encoding());
        assert!(wasm.exports_recv_arms_directly());

        let native = DispatchStrategy::native();
        assert!(!native.needs_synthetic_root());
        assert!(native.exports_recv_arms_directly());
    }
}
