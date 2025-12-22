use std::rc::Rc;

use rustc_hash::FxHashMap;
use std::cell::Cell;

/// Tracks the mapping between Fe bindings and their Yul local names within a block.
#[derive(Clone)]
pub(super) struct BlockState {
    locals: FxHashMap<String, String>,
    /// API stub: mapping from Fe bindings to the Yul expression representing their address.
    /// Populated for future reference semantics support.
    place_exprs: FxHashMap<String, String>,
    next_local: Rc<Cell<usize>>,
    /// Mapping from MIR ValueId index to Yul temp name for values bound in this scope.
    value_temps: FxHashMap<usize, String>,
}

impl BlockState {
    /// Creates a fresh state with no locals registered.
    pub(super) fn new() -> Self {
        Self {
            locals: FxHashMap::default(),
            place_exprs: FxHashMap::default(),
            next_local: Rc::new(Cell::new(0)),
            value_temps: FxHashMap::default(),
        }
    }

    /// Caches the Yul temp name for a MIR value.
    pub(super) fn insert_value_temp(&mut self, value_idx: usize, temp: String) {
        self.value_temps.insert(value_idx, temp);
    }

    /// Returns the cached Yul temp name for a MIR value, if it exists.
    pub(super) fn value_temp(&self, value_idx: usize) -> Option<&String> {
        self.value_temps.get(&value_idx)
    }

    /// Allocates a new temporary Yul variable name.
    pub(super) fn alloc_local(&mut self) -> String {
        let current = self.next_local.get();
        let name = format!("v{}", current);
        self.next_local.set(current + 1);
        name
    }

    /// Inserts or updates the mapping for a Fe binding to a Yul name.
    pub(super) fn insert_binding(&mut self, binding: String, name: String) -> String {
        self.locals.insert(binding, name.clone());
        name
    }

    /// Caches a Yul expression that represents the address for a binding.
    pub(super) fn insert_place_expr(&mut self, binding: String, expr: String) {
        self.place_exprs.insert(binding, expr);
    }

    /// Returns the known Yul name for a binding, if it exists.
    pub(super) fn binding(&self, binding: &str) -> Option<String> {
        self.locals.get(binding).cloned()
    }

    /// API stub: returns the Yul address expression for a binding, if cached.
    /// For future reference semantics support.
    #[allow(dead_code)]
    pub(super) fn resolve_place(&self, binding: &str) -> Option<String> {
        self.place_exprs.get(binding).cloned()
    }

    /// Resolves a binding to an existing Yul name or falls back to the original identifier.
    pub(super) fn resolve_name(&self, binding: &str) -> String {
        self.binding(binding).unwrap_or_else(|| binding.to_string())
    }
}
