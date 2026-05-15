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

/// Storage provider trait for non-EVM targets.
/// EVM has built-in SLOAD/SSTORE. Other targets need an explicit provider.
pub trait StorageProvider {
    fn sload(&self, key: [u8; 32]) -> [u8; 32];
    fn sstore(&mut self, key: [u8; 32], value: [u8; 32]);
}

/// In-memory storage for testing non-EVM contract execution.
#[derive(Default)]
pub struct InMemoryStorage {
    slots: std::collections::HashMap<[u8; 32], [u8; 32]>,
}

impl StorageProvider for InMemoryStorage {
    fn sload(&self, key: [u8; 32]) -> [u8; 32] {
        self.slots.get(&key).copied().unwrap_or([0u8; 32])
    }

    fn sstore(&mut self, key: [u8; 32], value: [u8; 32]) {
        self.slots.insert(key, value);
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

    #[test]
    fn in_memory_storage_round_trip() {
        let mut storage = InMemoryStorage::default();
        let key = [1u8; 32];
        let value = [42u8; 32];

        assert_eq!(storage.sload(key), [0u8; 32]);
        storage.sstore(key, value);
        assert_eq!(storage.sload(key), value);

        let key2 = [2u8; 32];
        assert_eq!(storage.sload(key2), [0u8; 32]);
    }
}
