//! WASM query module for browser-side documentation lookup
//!
//! Provides `DocStore` — a WASM-friendly wrapper around `DocIndex`
//! that can be constructed from JSON and queried from JavaScript.

use wasm_bindgen::prelude::*;

use crate::model::DocIndex;

/// A documentation store that can be queried from JavaScript.
///
/// Constructed from a JSON string (serialized `DocIndex`), then
/// queried for items by path, search, or module tree.
#[wasm_bindgen]
pub struct DocStore {
    index: DocIndex,
}

#[wasm_bindgen]
impl DocStore {
    /// Create a new DocStore from a JSON string.
    ///
    /// The JSON should be a serialized `DocIndex`.
    #[wasm_bindgen(constructor)]
    pub fn new(json: &str) -> Result<DocStore, JsError> {
        let index: DocIndex =
            serde_json::from_str(json).map_err(|e| JsError::new(&e.to_string()))?;
        Ok(DocStore { index })
    }

    /// Find an item by its URL path (e.g., "my_crate::MyStruct/struct").
    ///
    /// Returns the item as a JSON string, or null if not found.
    #[wasm_bindgen(js_name = "getItem")]
    pub fn get_item(&self, url_path: &str) -> Option<String> {
        self.index
            .find_by_url(url_path)
            .and_then(|item| serde_json::to_string(item).ok())
    }

    /// Search items by query string.
    ///
    /// Returns a JSON array of matching items (max 20).
    pub fn search(&self, query: &str) -> String {
        let results: Vec<_> = self.index.search(query).into_iter().take(20).collect();
        serde_json::to_string(&results).unwrap_or_else(|_| "[]".to_string())
    }

    /// Return the module tree as a JSON string.
    #[wasm_bindgen(js_name = "moduleTree")]
    pub fn module_tree(&self) -> String {
        serde_json::to_string(&self.index.modules).unwrap_or_else(|_| "[]".to_string())
    }

    /// Return the total number of documented items.
    #[wasm_bindgen(js_name = "itemCount")]
    pub fn item_count(&self) -> usize {
        self.index.items.len()
    }
}
