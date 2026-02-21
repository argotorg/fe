//! Fe Web — documentation model types and web components
//!
//! This crate provides:
//! - `model`: Documentation data model (DocIndex, DocItem, etc.)
//! - `markdown`: Markdown-to-HTML rendering
//! - `wasm` (feature-gated): WASM query module for browser-side doc lookup

pub mod markdown;
pub mod model;

#[cfg(feature = "wasm")]
pub mod wasm;
