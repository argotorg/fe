use rustc_hash::FxHashMap;

/// Tracks the mapping between Fe bindings and their Yul local names within a block.
#[derive(Clone)]
pub(super) struct BlockState {
    locals: FxHashMap<String, String>,
    next_local: usize,
}

impl BlockState {
    /// Creates a fresh state with no locals registered.
    pub(super) fn new() -> Self {
        Self {
            locals: FxHashMap::default(),
            next_local: 0,
        }
    }

    /// Allocates a new temporary Yul variable name.
    pub(super) fn alloc_local(&mut self) -> String {
        let name = format!("v{}", self.next_local);
        self.next_local += 1;
        name
    }

    /// Inserts or updates the mapping for a Fe binding to a Yul name.
    pub(super) fn insert_binding(&mut self, binding: String, name: String) -> String {
        self.locals.insert(binding, name.clone());
        name
    }

    /// Returns the known Yul name for a binding, if it exists.
    pub(super) fn binding(&self, binding: &str) -> Option<String> {
        self.locals.get(binding).cloned()
    }

    /// Resolves a binding to an existing Yul name or falls back to the original identifier.
    pub(super) fn resolve_name(&self, binding: &str) -> String {
        self.binding(binding).unwrap_or_else(|| binding.to_string())
    }
}
