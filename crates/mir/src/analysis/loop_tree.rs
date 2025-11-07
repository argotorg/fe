use fxhash::FxHashMap;
use id_arena::{Arena, Id};

use super::{cfg::ControlFlowGraph, domtree::DomTree};
use crate::ir::BasicBlockId;

#[derive(Debug, Default, Clone)]
pub struct LoopTree {
    /// Stores loops.
    /// The index of an outer loops is guaranteed to be lower than its inner
    /// loops because loops are found in RPO.
    loops: Arena<Loop>,

    /// Maps blocks to its contained loop.
    /// If the block is contained by multiple nested loops, then the block is
    /// mapped to the innermost loop.
    block_to_loop: FxHashMap<BasicBlockId, LoopId>,
}

pub type LoopId = Id<Loop>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Loop {
    /// A header of the loop.
    pub header: BasicBlockId,

    /// A parent loop that includes the loop.
    pub parent: Option<LoopId>,

    /// Child loops that the loop includes.
    pub children: Vec<LoopId>,
}

impl LoopTree {
    pub fn compute(cfg: &ControlFlowGraph, domtree: &DomTree) -> Self {
        let mut tree = LoopTree::default();

        // Find loop headers in RPO, this means outer loops are guaranteed to be
        // inserted first, then its inner loops are inserted.
        for &block in domtree.rpo() {
            for &pred in cfg.preds(block) {
                if domtree.dominates(block, pred) {
                    let loop_data = Loop {
                        header: block,
                        parent: None,
                        children: Vec::new(),
                    };

                    tree.loops.alloc(loop_data);
                    break;
                }
            }
        }

        tree.analyze_loops(cfg, domtree);

        tree
    }

    /// Returns all blocks in the loop.
    pub fn iter_blocks_post_order<'a, 'b>(
        &'a self,
        cfg: &'b ControlFlowGraph,
        lp: LoopId,
    ) -> BlocksInLoopPostOrder<'a, 'b> {
        BlocksInLoopPostOrder::new(self, cfg, lp)
    }

    /// Returns all loops in a function body.
    /// An outer loop is guaranteed to be iterated before its inner loops.
    pub fn loops(&self) -> impl Iterator<Item = LoopId> + '_ {
        self.loops.iter().map(|(id, _)| id)
    }

    /// Returns number of loops found.
    pub fn loop_num(&self) -> usize {
        self.loops.len()
    }

    /// Returns `true` if the `block` is in the `lp`.
    pub fn is_block_in_loop(&self, block: BasicBlockId, lp: LoopId) -> bool {
        let mut loop_of_block = self.loop_of_block(block);
        while let Some(cur_lp) = loop_of_block {
            if lp == cur_lp {
                return true;
            }
            loop_of_block = self.parent_loop(cur_lp);
        }
        false
    }

    /// Returns header block of the `lp`.
    pub fn loop_header(&self, lp: LoopId) -> BasicBlockId {
        self.loops[lp].header
    }

    /// Get parent loop of the `lp` if exists.
    pub fn parent_loop(&self, lp: LoopId) -> Option<LoopId> {
        self.loops[lp].parent
    }

    /// Returns the loop that the `block` belongs to.
    /// If the `block` belongs to multiple loops, then returns the innermost
    /// loop.
    pub fn loop_of_block(&self, block: BasicBlockId) -> Option<LoopId> {
        self.block_to_loop.get(&block).copied()
    }

    /// Analyze loops. This method does
    /// 1. Mapping each blocks to its contained loop.
    /// 2. Setting parent and child of the loops.
    fn analyze_loops(&mut self, cfg: &ControlFlowGraph, domtree: &DomTree) {
        let mut worklist = vec![];

        // Iterate loops reversely to ensure analyze inner loops first.
        let loops_rev: Vec<_> = self.loops.iter().rev().map(|(id, _)| id).collect();
        for cur_lp in loops_rev {
            let cur_lp_header = self.loop_header(cur_lp);

            // Add predecessors of the loop header to worklist.
            for &block in cfg.preds(cur_lp_header) {
                if domtree.dominates(cur_lp_header, block) {
                    worklist.push(block);
                }
            }

            while let Some(block) = worklist.pop() {
                match self.block_to_loop.get(&block).copied() {
                    Some(lp_of_block) => {
                        let outermost_parent = self.outermost_parent(lp_of_block);

                        // If outermost parent is current loop, then the block is already visited.
                        if outermost_parent == cur_lp {
                            continue;
                        } else {
                            self.loops[cur_lp].children.push(outermost_parent);
                            self.loops[outermost_parent].parent = cur_lp.into();

                            let lp_header_of_block = self.loop_header(lp_of_block);
                            worklist.extend(cfg.preds(lp_header_of_block));
                        }
                    }

                    // If the block is not mapped to any loops, then map it to the loop.
                    None => {
                        self.map_block(block, cur_lp);
                        // If block is not loop header, then add its predecessors to the worklist.
                        if block != cur_lp_header {
                            worklist.extend(cfg.preds(block));
                        }
                    }
                }
            }
        }
    }

    /// Returns the outermost parent loop of `lp`. If `lp` doesn't have any
    /// parent, then returns `lp` itself.
    fn outermost_parent(&self, mut lp: LoopId) -> LoopId {
        while let Some(parent) = self.parent_loop(lp) {
            lp = parent;
        }
        lp
    }

    /// Map `block` to `lp`.
    fn map_block(&mut self, block: BasicBlockId, lp: LoopId) {
        self.block_to_loop.insert(block, lp);
    }
}

pub struct BlocksInLoopPostOrder<'a, 'b> {
    lpt: &'a LoopTree,
    cfg: &'b ControlFlowGraph,
    lp: LoopId,
    stack: Vec<BasicBlockId>,
    block_state: FxHashMap<BasicBlockId, BlockState>,
}

impl<'a, 'b> BlocksInLoopPostOrder<'a, 'b> {
    fn new(lpt: &'a LoopTree, cfg: &'b ControlFlowGraph, lp: LoopId) -> Self {
        let loop_header = lpt.loop_header(lp);

        Self {
            lpt,
            cfg,
            lp,
            stack: vec![loop_header],
            block_state: FxHashMap::default(),
        }
    }
}

impl Iterator for BlocksInLoopPostOrder<'_, '_> {
    type Item = BasicBlockId;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&block) = self.stack.last() {
            match self.block_state.get(&block) {
                // The block is already visited, but not returned from the iterator,
                // so mark the block as `Finished` and return the block.
                Some(BlockState::Visited) => {
                    let block = self.stack.pop().unwrap();
                    self.block_state.insert(block, BlockState::Finished);
                    return Some(block);
                }

                // The block is already returned, so just remove the block from the stack.
                Some(BlockState::Finished) => {
                    self.stack.pop().unwrap();
                }

                // The block is not visited yet, so push its unvisited in-loop successors to the
                // stack and mark the block as `Visited`.
                None => {
                    self.block_state.insert(block, BlockState::Visited);
                    for &succ in self.cfg.succs(block) {
                        if !self.block_state.contains_key(&succ)
                            && self.lpt.is_block_in_loop(succ, self.lp)
                        {
                            self.stack.push(succ);
                        }
                    }
                }
            }
        }

        None
    }
}

enum BlockState {
    Visited,
    Finished,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{body_builder::BodyBuilder, FunctionBody, FunctionId, SourceInfo, TypeId};

    fn compute_loop(func: &FunctionBody) -> LoopTree {
        let cfg = ControlFlowGraph::compute(func);
        let domtree = DomTree::compute(&cfg);
        LoopTree::compute(&cfg, &domtree)
    }

    fn body_builder() -> BodyBuilder {
        BodyBuilder::new(FunctionId(0), SourceInfo::dummy())
    }

    #[test]
    fn simple_loop() {
        let mut builder = body_builder();

        let entry = builder.current_block();
        let block1 = builder.make_block();
        let block2 = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(false, dummy_ty);
        builder.branch(v0, block1, block2, SourceInfo::dummy());

        builder.move_to_block(block1);
        builder.jump(entry, SourceInfo::dummy());

        builder.move_to_block(block2);
        let dummy_value = builder.make_unit(dummy_ty);
        builder.ret(dummy_value, SourceInfo::dummy());

        let func = builder.build();

        let lpt = compute_loop(&func);

        assert_eq!(lpt.loop_num(), 1);
        let lp = lpt.loops().next().unwrap();

        assert!(lpt.is_block_in_loop(entry, lp));
        assert_eq!(lpt.loop_of_block(entry), Some(lp));

        assert!(lpt.is_block_in_loop(block1, lp));
        assert_eq!(lpt.loop_of_block(block1), Some(lp));

        assert!(!lpt.is_block_in_loop(block2, lp));
        assert!(lpt.loop_of_block(block2).is_none());

        assert_eq!(lpt.loop_header(lp), entry);
    }

    #[test]
    fn nested_loop() {
        let mut builder = body_builder();

        let entry = builder.current_block();
        let block1 = builder.make_block();
        let block2 = builder.make_block();
        let block3 = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(false, dummy_ty);
        builder.branch(v0, block1, block3, SourceInfo::dummy());

        builder.move_to_block(block1);
        builder.branch(v0, entry, block2, SourceInfo::dummy());

        builder.move_to_block(block2);
        builder.jump(block1, SourceInfo::dummy());

        builder.move_to_block(block3);
        let dummy_value = builder.make_unit(dummy_ty);
        builder.ret(dummy_value, SourceInfo::dummy());

        let func = builder.build();

        let lpt = compute_loop(&func);

        assert_eq!(lpt.loop_num(), 2);
        let mut loops = lpt.loops();
        let outer_lp = loops.next().unwrap();
        let inner_lp = loops.next().unwrap();

        assert!(lpt.is_block_in_loop(entry, outer_lp));
        assert!(!lpt.is_block_in_loop(entry, inner_lp));
        assert_eq!(lpt.loop_of_block(entry), Some(outer_lp));

        assert!(lpt.is_block_in_loop(block1, outer_lp));
        assert!(lpt.is_block_in_loop(block1, inner_lp));
        assert_eq!(lpt.loop_of_block(block1), Some(inner_lp));

        assert!(lpt.is_block_in_loop(block2, outer_lp));
        assert!(lpt.is_block_in_loop(block2, inner_lp));
        assert_eq!(lpt.loop_of_block(block2), Some(inner_lp));

        assert!(!lpt.is_block_in_loop(block3, outer_lp));
        assert!(!lpt.is_block_in_loop(block3, inner_lp));
        assert!(lpt.loop_of_block(block3).is_none());

        assert!(lpt.parent_loop(outer_lp).is_none());
        assert_eq!(lpt.parent_loop(inner_lp), Some(outer_lp));

        assert_eq!(lpt.loop_header(outer_lp), entry);
        assert_eq!(lpt.loop_header(inner_lp), block1);
    }
}
