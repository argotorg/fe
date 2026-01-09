use std::rc::Rc;

use mir::LocalId;
use rustc_hash::FxHashMap;
use std::cell::Cell;

/// Tracks the mapping between Fe bindings and their Yul local names within a block.
#[derive(Clone)]
pub(super) struct BlockState {
    locals: FxHashMap<LocalId, String>,
    next_local: Rc<Cell<usize>>,
    /// Mapping from MIR ValueId index to Yul temp name for values bound in this scope.
    value_temps: FxHashMap<usize, String>,
}

impl BlockState {
    /// Creates a fresh state with no locals registered.
    pub(super) fn new() -> Self {
        Self {
            locals: FxHashMap::default(),
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

    /// Inserts or updates the mapping for a MIR local to a Yul name/expression.
    pub(super) fn insert_local(&mut self, local: LocalId, name: String) -> String {
        self.locals.insert(local, name.clone());
        name
    }

    /// Returns the known Yul name for a local, if it exists.
    pub(super) fn local(&self, local: LocalId) -> Option<String> {
        self.locals.get(&local).cloned()
    }

    /// Resolves a local to its lowered Yul name/expression.
    pub(super) fn resolve_local(&self, local: LocalId) -> Option<String> {
        self.local(local)
    }
}
