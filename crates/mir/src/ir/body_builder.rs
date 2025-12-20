use super::{BasicBlock, BasicBlockId, MirBody, MirInst, Terminator};

#[derive(Debug)]
pub struct BodyBuilder<'db> {
    pub body: MirBody<'db>,
    current_block: Option<BasicBlockId>,
}

impl<'db> BodyBuilder<'db> {
    pub fn new() -> Self {
        let mut body = MirBody::new();
        let entry = body.push_block(BasicBlock::new());
        Self {
            body,
            current_block: Some(entry),
        }
    }

    pub fn build(self) -> MirBody<'db> {
        self.body
    }

    pub fn entry_block(&self) -> BasicBlockId {
        self.body.entry
    }

    pub fn current_block(&self) -> Option<BasicBlockId> {
        self.current_block
    }

    pub fn make_block(&mut self) -> BasicBlockId {
        self.body.push_block(BasicBlock::new())
    }

    pub fn move_to_block(&mut self, block: BasicBlockId) {
        self.current_block = Some(block);
    }

    pub fn clear_current_block(&mut self) {
        self.current_block = None;
    }

    pub fn push_inst_in(&mut self, block: BasicBlockId, inst: MirInst<'db>) {
        self.body.block_mut(block).push_inst(inst);
    }

    pub fn set_block_terminator(&mut self, block: BasicBlockId, term: Terminator<'db>) {
        self.body.block_mut(block).set_terminator(term);
    }

    pub fn terminate_current(&mut self, term: Terminator<'db>) {
        if let Some(block) = self.current_block {
            self.set_block_terminator(block, term);
            self.current_block = None;
        }
    }
}

impl<'db> Default for BodyBuilder<'db> {
    fn default() -> Self {
        Self::new()
    }
}
