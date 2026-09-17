//! Canonical capability algebra over verified semantic values.
//!
//! The structural solver and boundary policies consume this module; runtime layout does not
//! participate in index identity, capability slots, or guarded value equality.
mod decision;
pub mod guard;
pub mod index;
pub mod path;
pub mod region;
pub mod semantics;
pub mod shape;
pub mod value;

#[cfg(test)]
mod tests;
