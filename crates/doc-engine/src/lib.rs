//! Fe Documentation Engine
//!
//! This crate provides documentation extraction from Fe's HIR.
//! It traverses the HIR and produces a `DocIndex` that can be rendered
//! by the `doc-viewer` crate or consumed by the `fe-web` WASM module.
//!
//! # Architecture
//!
//! - `extract`: Logic for traversing HIR and extracting documentation
//!
//! The extracted `DocIndex` is defined in `fe-web` and can be:
//! - Rendered as HTML by the doc-viewer server
//! - Serialized to JSON for static site generation
//! - Sent to a WASM client for client-side rendering

pub mod extract;

// Re-export model from fe-web for convenience
pub use fe_web::model;
pub use fe_web::model::*;

pub use extract::DocExtractor;
pub use extract::{qualify_path_with_ingot_name, scope_to_doc_path};
