//! Fe Documentation Viewer
//!
//! SSR Leptos components for rendering Fe documentation.
//! Uses server-side rendering with live updates via JavaScript.

pub mod components;

// Re-export model and markdown from fe-web
pub use fe_web::markdown;
pub use fe_web::model;

#[cfg(feature = "ssr")]
pub mod ssr_components;

#[cfg(feature = "ssr")]
pub mod server;

pub use components::*;
pub use model::*;
