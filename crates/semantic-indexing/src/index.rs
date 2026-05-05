use radix_immutable::{StringPrefixView, StringTrie, Trie};
use salsa::{Setter, Update};

/// A single indexed symbol definition.
///
/// Keyed by SCIP symbol string in the trie (e.g.
/// `"fe fe pkg 0.1.0 module/Struct#field."`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexSymbol {
    pub symbol: String,
    pub display_name: String,
    /// Maps to `scip::types::symbol_information::Kind` as i32.
    pub kind: i32,
    /// SCIP-format documentation: signature in fenced block + docstring.
    pub documentation: Vec<String>,
    pub enclosing_symbol: String,
    pub def_location: IndexLocation,
    pub doc_url: Option<String>,
}

/// A single reference occurrence.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexReference {
    pub symbol: String,
    pub location: IndexLocation,
    /// `scip::types::SymbolRole::Definition` flag.
    pub role: i32,
}

/// Format-neutral source location. Range is pre-computed in SCIP format
/// (UTF-8 byte offsets from line start) so both SCIP (direct) and LSIF
/// (requires re-reading file text for UTF-16 conversion) can use it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexLocation {
    pub relative_path: String,
    pub file_url: String,
    /// SCIP range: `[line, col_start, col_end]` or `[sl, sc, el, ec]`.
    pub range: Vec<i32>,
}

/// The semantic index: a salsa input holding symbol and reference data
/// in prefix-structured tries for efficient incremental updates.
///
/// Create with `SemanticIndex::new(db, ...)`. Hold the returned ID.
/// Functions in this crate accept `&dyn salsa::Database` — any salsa db works.
#[salsa::input]
#[derive(Debug)]
pub struct SemanticIndex {
    pub symbols: StringTrie<String, IndexSymbol>,
    pub references: StringTrie<String, Vec<IndexReference>>,
}

impl SemanticIndex {
    pub fn empty(db: &dyn salsa::Database) -> Self {
        SemanticIndex::new(db, Trie::new(), Trie::new())
    }

    pub fn upsert_symbols(
        &self,
        db: &mut dyn salsa::Database,
        entries: Vec<(String, IndexSymbol)>,
    ) {
        let mut trie = self.symbols(db);
        for (key, sym) in entries {
            trie = trie.insert(key, sym);
        }
        self.set_symbols(db).to(trie);
    }

    pub fn upsert_references(
        &self,
        db: &mut dyn salsa::Database,
        refs_by_symbol: Vec<(String, Vec<IndexReference>)>,
    ) {
        let mut trie = self.references(db);
        for (key, refs) in refs_by_symbol {
            trie = trie.insert(key, refs);
        }
        self.set_references(db).to(trie);
    }

    pub fn symbols_for_prefix(
        &self,
        db: &dyn salsa::Database,
        prefix: String,
    ) -> StringPrefixView<String, IndexSymbol> {
        self.symbols(db).view_subtrie(prefix)
    }

    pub fn references_to(
        &self,
        db: &dyn salsa::Database,
        symbol: &str,
    ) -> Option<Vec<IndexReference>> {
        self.references(db).get(&symbol.to_string()).cloned()
    }
}

// --- salsa::Update impls ---

unsafe impl Update for IndexSymbol {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old = unsafe { &mut *old_pointer };
        if *old == new_value {
            false
        } else {
            *old = new_value;
            true
        }
    }
}

unsafe impl Update for IndexReference {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old = unsafe { &mut *old_pointer };
        if *old == new_value {
            false
        } else {
            *old = new_value;
            true
        }
    }
}

unsafe impl Update for IndexLocation {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old = unsafe { &mut *old_pointer };
        if *old == new_value {
            false
        } else {
            *old = new_value;
            true
        }
    }
}
