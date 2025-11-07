pub mod ir;
mod lower;

pub use ir::{BasicBlockId, MirBody, MirFunction, MirInst, MirModule, Terminator};
pub use lower::{lower_module, MirLowerError, MirLowerResult};
