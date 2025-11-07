use fxhash::FxHashMap;

use super::{basic_block::BasicBlockId, function::BodyDataStore, inst::InstId};

#[derive(Debug, Clone, PartialEq, Eq)]
/// Represents basic block order and instruction order.
pub struct BodyOrder {
    blocks: FxHashMap<BasicBlockId, BlockNode>,
    insts: FxHashMap<InstId, InstNode>,
    entry_block: BasicBlockId,
    last_block: BasicBlockId,
}
impl BodyOrder {
    pub fn new(entry_block: BasicBlockId) -> Self {
        let entry_block_node = BlockNode::default();
        let mut blocks = FxHashMap::default();
        blocks.insert(entry_block, entry_block_node);

        Self {
            blocks,
            insts: FxHashMap::default(),
            entry_block,
            last_block: entry_block,
        }
    }

    /// Returns an entry block of a function body.
    pub fn entry(&self) -> BasicBlockId {
        self.entry_block
    }

    /// Returns a last block of a function body.
    pub fn last_block(&self) -> BasicBlockId {
        self.last_block
    }

    /// Returns `true` if a block doesn't contain any block.
    pub fn is_block_empty(&self, block: BasicBlockId) -> bool {
        self.first_inst(block).is_none()
    }

    /// Returns `true` if a function body contains a given `block`.
    pub fn is_block_inserted(&self, block: BasicBlockId) -> bool {
        self.blocks.contains_key(&block)
    }

    /// Returns a number of block in a function.
    pub fn block_num(&self) -> usize {
        self.blocks.len()
    }

    /// Returns a previous block of a given block.
    ///
    /// # Panics
    /// Panics if `block` is not inserted yet.
    pub fn prev_block(&self, block: BasicBlockId) -> Option<BasicBlockId> {
        self.blocks[&block].prev
    }

    /// Returns a next block of a given block.
    ///
    /// # Panics
    /// Panics if `block` is not inserted yet.
    pub fn next_block(&self, block: BasicBlockId) -> Option<BasicBlockId> {
        self.blocks[&block].next
    }

    /// Returns `true` is a given `inst` is inserted.
    pub fn is_inst_inserted(&self, inst: InstId) -> bool {
        self.insts.contains_key(&inst)
    }

    /// Returns first instruction of a block if exists.
    ///
    /// # Panics
    /// Panics if `block` is not inserted yet.
    pub fn first_inst(&self, block: BasicBlockId) -> Option<InstId> {
        self.blocks[&block].first_inst
    }

    /// Returns a terminator instruction of a block.
    ///
    /// # Panics
    /// Panics if
    /// 1. `block` is not inserted yet.
    pub fn terminator(&self, store: &BodyDataStore, block: BasicBlockId) -> Option<InstId> {
        let last_inst = self.last_inst(block)?;
        if store.is_terminator(last_inst) {
            Some(last_inst)
        } else {
            None
        }
    }

    /// Returns `true` if a `block` is terminated.
    pub fn is_terminated(&self, store: &BodyDataStore, block: BasicBlockId) -> bool {
        self.terminator(store, block).is_some()
    }

    /// Returns a last instruction of a block.
    ///
    /// # Panics
    /// Panics if `block` is not inserted yet.
    pub fn last_inst(&self, block: BasicBlockId) -> Option<InstId> {
        self.blocks[&block].last_inst
    }

    /// Returns a previous instruction of a given `inst`.
    ///
    /// # Panics
    /// Panics if `inst` is not inserted yet.
    pub fn prev_inst(&self, inst: InstId) -> Option<InstId> {
        self.insts[&inst].prev
    }

    /// Returns a next instruction of a given `inst`.
    ///
    /// # Panics
    /// Panics if `inst` is not inserted yet.
    pub fn next_inst(&self, inst: InstId) -> Option<InstId> {
        self.insts[&inst].next
    }

    /// Returns a block to which a given `inst` belongs.
    ///
    /// # Panics
    /// Panics if `inst` is not inserted yet.
    pub fn inst_block(&self, inst: InstId) -> BasicBlockId {
        self.insts[&inst].block
    }

    /// Returns an iterator which iterates all basic blocks in a function body
    /// in pre-order.
    pub fn iter_block(&self) -> impl Iterator<Item = BasicBlockId> + '_ {
        BlockIter {
            next: Some(self.entry_block),
            blocks: &self.blocks,
        }
    }

    /// Returns an iterator which iterates all instruction in a given `block` in
    /// pre-order.
    ///
    /// # Panics
    /// Panics if `block` is not inserted yet.
    pub fn iter_inst(&self, block: BasicBlockId) -> impl Iterator<Item = InstId> + '_ {
        InstIter {
            next: self.blocks[&block].first_inst,
            insts: &self.insts,
        }
    }

    /// Appends a given `block` to a function body.
    ///
    /// # Panics
    /// Panics if a given `block` is already inserted to a function.
    pub fn append_block(&mut self, block: BasicBlockId) {
        debug_assert!(!self.is_block_inserted(block));

        let mut block_node = BlockNode::default();
        let last_block = self.last_block;
        let last_block_node = &mut self.block_mut(last_block);
        last_block_node.next = Some(block);
        block_node.prev = Some(last_block);

        self.blocks.insert(block, block_node);
        self.last_block = block;
    }

    /// Inserts a given `block` before a `before` block.
    ///
    /// # Panics
    /// Panics if
    /// 1. a given `block` is already inserted.
    /// 2. a given `before` block is NOTE inserted yet.
    pub fn insert_block_before_block(&mut self, block: BasicBlockId, before: BasicBlockId) {
        debug_assert!(self.is_block_inserted(before));
        debug_assert!(!self.is_block_inserted(block));

        let mut block_node = BlockNode::default();

        match self.blocks[&before].prev {
            Some(prev) => {
                block_node.prev = Some(prev);
                self.block_mut(prev).next = Some(block);
            }
            None => self.entry_block = block,
        }

        block_node.next = Some(before);
        self.block_mut(before).prev = Some(block);
        self.blocks.insert(block, block_node);
    }

    /// Inserts a given `block` after a `after` block.
    ///
    /// # Panics
    /// Panics if
    /// 1. a given `block` is already inserted.
    /// 2. a given `after` block is NOTE inserted yet.
    pub fn insert_block_after_block(&mut self, block: BasicBlockId, after: BasicBlockId) {
        debug_assert!(self.is_block_inserted(after));
        debug_assert!(!self.is_block_inserted(block));

        let mut block_node = BlockNode::default();

        match self.blocks[&after].next {
            Some(next) => {
                block_node.next = Some(next);
                self.block_mut(next).prev = Some(block);
            }
            None => self.last_block = block,
        }
        block_node.prev = Some(after);
        self.block_mut(after).next = Some(block);
        self.blocks.insert(block, block_node);
    }

    /// Remove a given `block` from a function. All instructions in a block are
    /// also removed.
    ///
    /// # Panics
    /// Panics if
    /// 1. a given `block` is NOT inserted.
    /// 2. a `block` is the last one block in a function.
    pub fn remove_block(&mut self, block: BasicBlockId) {
        debug_assert!(self.is_block_inserted(block));
        debug_assert!(self.block_num() > 1);

        // Remove all insts in a `block`.
        let mut next_inst = self.first_inst(block);
        while let Some(inst) = next_inst {
            next_inst = self.next_inst(inst);
            self.remove_inst(inst);
        }

        // Remove `block`.
        let block_node = &self.blocks[&block];
        let prev_block = block_node.prev;
        let next_block = block_node.next;
        match (prev_block, next_block) {
            // `block` is in the middle of a function.
            (Some(prev), Some(next)) => {
                self.block_mut(prev).next = Some(next);
                self.block_mut(next).prev = Some(prev);
            }
            // `block` is the last block of a function.
            (Some(prev), None) => {
                self.block_mut(prev).next = None;
                self.last_block = prev;
            }
            // `block` is the first block of a function.
            (None, Some(next)) => {
                self.block_mut(next).prev = None;
                self.entry_block = next
            }
            (None, None) => {
                unreachable!()
            }
        }

        self.blocks.remove(&block);
    }

    /// Appends `inst` to the end of a `block`
    ///
    /// # Panics
    /// Panics if
    /// 1. a given `block` is NOT inserted.
    /// 2. a given `inst` is already inserted.
    pub fn append_inst(&mut self, inst: InstId, block: BasicBlockId) {
        debug_assert!(self.is_block_inserted(block));
        debug_assert!(!self.is_inst_inserted(inst));

        let mut inst_node = InstNode::new(block);

        if let Some(last_inst) = self.blocks[&block].last_inst {
            inst_node.prev = Some(last_inst);
            self.inst_mut(last_inst).next = Some(inst);
        } else {
            self.block_mut(block).first_inst = Some(inst);
        }

        self.block_mut(block).last_inst = Some(inst);
        self.insts.insert(inst, inst_node);
    }

    /// Prepends `inst` to the beginning of a `block`
    ///
    /// # Panics
    /// Panics if
    /// 1. a given `block` is NOT inserted.
    /// 2. a given `inst` is already inserted.
    pub fn prepend_inst(&mut self, inst: InstId, block: BasicBlockId) {
        debug_assert!(self.is_block_inserted(block));
        debug_assert!(!self.is_inst_inserted(inst));

        let mut inst_node = InstNode::new(block);

        if let Some(first_inst) = self.blocks[&block].first_inst {
            inst_node.next = Some(first_inst);
            self.inst_mut(first_inst).prev = Some(inst);
        } else {
            self.block_mut(block).last_inst = Some(inst);
        }

        self.block_mut(block).first_inst = Some(inst);
        self.insts.insert(inst, inst_node);
    }

    /// Insert `inst` before `before` inst.
    ///
    /// # Panics
    /// Panics if
    /// 1. a given `before` is NOT inserted.
    /// 2. a given `inst` is already inserted.
    pub fn insert_inst_before_inst(&mut self, inst: InstId, before: InstId) {
        debug_assert!(self.is_inst_inserted(before));
        debug_assert!(!self.is_inst_inserted(inst));

        let before_inst_node = &self.insts[&before];
        let block = before_inst_node.block;
        let mut inst_node = InstNode::new(block);

        match before_inst_node.prev {
            Some(prev) => {
                inst_node.prev = Some(prev);
                self.inst_mut(prev).next = Some(inst);
            }
            None => self.block_mut(block).first_inst = Some(inst),
        }
        inst_node.next = Some(before);
        self.inst_mut(before).prev = Some(inst);
        self.insts.insert(inst, inst_node);
    }

    /// Insert `inst` after `after` inst.
    ///
    /// # Panics
    /// Panics if
    /// 1. a given `after` is NOT inserted.
    /// 2. a given `inst` is already inserted.
    pub fn insert_inst_after(&mut self, inst: InstId, after: InstId) {
        debug_assert!(self.is_inst_inserted(after));
        debug_assert!(!self.is_inst_inserted(inst));

        let after_inst_node = &self.insts[&after];
        let block = after_inst_node.block;
        let mut inst_node = InstNode::new(block);

        match after_inst_node.next {
            Some(next) => {
                inst_node.next = Some(next);
                self.inst_mut(next).prev = Some(inst);
            }
            None => self.block_mut(block).last_inst = Some(inst),
        }
        inst_node.prev = Some(after);
        self.inst_mut(after).next = Some(inst);
        self.insts.insert(inst, inst_node);
    }

    /// Remove instruction from the function body.
    ///
    /// # Panics
    /// Panics if a given `inst` is not inserted.
    pub fn remove_inst(&mut self, inst: InstId) {
        debug_assert!(self.is_inst_inserted(inst));

        let inst_node = &self.insts[&inst];
        let inst_block = inst_node.block;
        let prev_inst = inst_node.prev;
        let next_inst = inst_node.next;
        match (prev_inst, next_inst) {
            (Some(prev), Some(next)) => {
                self.inst_mut(prev).next = Some(next);
                self.inst_mut(next).prev = Some(prev);
            }
            (Some(prev), None) => {
                self.inst_mut(prev).next = None;
                self.block_mut(inst_block).last_inst = Some(prev);
            }
            (None, Some(next)) => {
                self.inst_mut(next).prev = None;
                self.block_mut(inst_block).first_inst = Some(next);
            }
            (None, None) => {
                let block_node = self.block_mut(inst_block);
                block_node.first_inst = None;
                block_node.last_inst = None;
            }
        }

        self.insts.remove(&inst);
    }

    fn block_mut(&mut self, block: BasicBlockId) -> &mut BlockNode {
        self.blocks.get_mut(&block).unwrap()
    }

    fn inst_mut(&mut self, inst: InstId) -> &mut InstNode {
        self.insts.get_mut(&inst).unwrap()
    }
}

struct BlockIter<'a> {
    next: Option<BasicBlockId>,
    blocks: &'a FxHashMap<BasicBlockId, BlockNode>,
}

impl Iterator for BlockIter<'_> {
    type Item = BasicBlockId;

    fn next(&mut self) -> Option<BasicBlockId> {
        let next = self.next?;
        self.next = self.blocks[&next].next;
        Some(next)
    }
}

struct InstIter<'a> {
    next: Option<InstId>,
    insts: &'a FxHashMap<InstId, InstNode>,
}

impl Iterator for InstIter<'_> {
    type Item = InstId;

    fn next(&mut self) -> Option<InstId> {
        let next = self.next?;
        self.next = self.insts[&next].next;
        Some(next)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
/// A helper struct to track a basic block order in a function body.
struct BlockNode {
    /// A previous block.
    prev: Option<BasicBlockId>,

    /// A next block.
    next: Option<BasicBlockId>,

    /// A first instruction of a block.
    first_inst: Option<InstId>,

    /// A last instruction of a block.
    last_inst: Option<InstId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A helper struct to track a instruction order in a basic block.
struct InstNode {
    /// An block to which a inst belongs.
    block: BasicBlockId,

    /// A previous instruction.
    prev: Option<InstId>,

    /// A next instruction.
    next: Option<InstId>,
}

impl InstNode {
    fn new(block: BasicBlockId) -> Self {
        Self {
            block,
            prev: None,
            next: None,
        }
    }
}
