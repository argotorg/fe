//! Back-compat shim so existing `crate::hir_def::semantic` paths continue to work.
//! The canonical semantic traversal lives at `crate::core::semantic`.

pub use crate::core::semantic::*;
