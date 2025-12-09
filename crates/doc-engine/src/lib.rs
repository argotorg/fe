//! Fe Documentation Engine
//!
//! This crate provides documentation extraction from Fe's HIR.
//! It traverses the HIR and produces a `DocIndex` that can be rendered
//! by the `doc-viewer` crate.
//!
//! # Architecture
//!
//! - `extract`: Logic for traversing HIR and extracting documentation
//!
//! The extracted `DocIndex` is defined in `doc-viewer` and can be:
//! - Rendered as HTML by the doc-viewer server
//! - Serialized to JSON for static site generation
//! - Sent to a WASM client for client-side rendering

pub mod extract;

// Re-export model from doc-viewer for convenience
pub use doc_viewer::model;
pub use doc_viewer::model::*;

pub use extract::DocExtractor;
