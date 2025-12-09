//! Fe Documentation Viewer
//!
//! SSR Leptos components for rendering Fe documentation.
//! Uses server-side rendering with live updates via JavaScript.

pub mod components;
pub mod markdown;
pub mod model;

#[cfg(feature = "ssr")]
pub mod ssr_components;

#[cfg(feature = "ssr")]
pub mod server;

pub use components::*;
pub use model::*;
