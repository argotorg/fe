use id_arena::Id;

pub type BasicBlockId = Id<BasicBlock>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BasicBlock {}
