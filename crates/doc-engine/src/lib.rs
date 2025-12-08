//! Fe Documentation Engine
//!
//! This crate provides documentation extraction, rendering, and serving for Fe projects.
//! It supports both static site generation and dynamic, LSP-driven documentation browsing.
//!
//! # Architecture
//!
//! - `model`: Data structures representing extracted documentation
//! - `extract`: Logic for traversing HIR and extracting documentation
//! - `render`: Leptos components for rendering documentation
//! - `server`: Axum-based HTTP server for dynamic doc serving (behind `ssr` feature)

pub mod extract;
pub mod model;
pub mod render;

#[cfg(feature = "ssr")]
pub mod server;

pub use extract::DocExtractor;
pub use model::*;
