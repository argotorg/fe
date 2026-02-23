//! Fe Web — documentation model types and web components
//!
//! This crate provides:
//! - `model`: Documentation data model (DocIndex, DocItem, etc.)
//! - `markdown`: Markdown-to-HTML rendering
//! - `assets`: Embedded CSS/JS for static doc sites
//! - `static_site`: Static site generator (`fe doc --static`)
//! - `wasm` (feature-gated): WASM query module for browser-side doc lookup

pub mod assets;
pub mod escape;
pub mod highlight;
pub mod markdown;
pub mod model;
pub mod static_site;

#[cfg(feature = "wasm")]
pub mod wasm;
