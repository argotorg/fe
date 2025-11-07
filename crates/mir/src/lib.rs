pub mod ir;
mod lower;

pub use ir::{BasicBlockId, MirBody, MirFunction, MirModule, Terminator};
pub use lower::{lower_module, MirLowerError, MirLowerResult};
