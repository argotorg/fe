//! This module contains implementation of `Post Dominator Tree`.

use id_arena::{ArenaBehavior, DefaultArenaBehavior};

use super::{cfg::ControlFlowGraph, domtree::DomTree};

use crate::ir::{BasicBlock, BasicBlockId, FunctionBody};

#[derive(Debug)]
pub struct PostDomTree {
    /// Dummy entry block to calculate post dom tree.
    dummy_entry: BasicBlockId,
    /// Canonical dummy exit block to calculate post dom tree. All blocks ends
    /// with `return` has an edge to this block.
    dummy_exit: BasicBlockId,

    /// Dominator tree of reverse control flow graph.
    domtree: DomTree,
}

impl PostDomTree {
    pub fn compute(func: &FunctionBody) -> Self {
        let mut rcfg = ControlFlowGraph::compute(func);

        let real_entry = rcfg.entry();

        let dummy_entry = Self::make_dummy_block();
        let dummy_exit = Self::make_dummy_block();
        // Add edges from dummy entry block to real entry block and dummy exit block.
        rcfg.add_edge(dummy_entry, real_entry);
        rcfg.add_edge(dummy_entry, dummy_exit);

        // Add edges from real exit blocks to dummy exit block.
        for exit in std::mem::take(&mut rcfg.exits) {
            rcfg.add_edge(exit, dummy_exit);
        }

        rcfg.reverse_edge(dummy_exit, vec![dummy_entry]);
        let domtree = DomTree::compute(&rcfg);

        Self {
            dummy_entry,
            dummy_exit,
            domtree,
        }
    }

    pub fn post_idom(&self, block: BasicBlockId) -> PostIDom {
        match self.domtree.idom(block).unwrap() {
            block if block == self.dummy_entry => PostIDom::DummyEntry,
            block if block == self.dummy_exit => PostIDom::DummyExit,
            other => PostIDom::Block(other),
        }
    }

    /// Returns `true` if block is reachable from the exit blocks.
    pub fn is_reachable(&self, block: BasicBlockId) -> bool {
        self.domtree.is_reachable(block)
    }

    fn make_dummy_block() -> BasicBlockId {
        let arena_id = DefaultArenaBehavior::<BasicBlock>::new_arena_id();
        DefaultArenaBehavior::new_id(arena_id, 0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostIDom {
    DummyEntry,
    DummyExit,
    Block(BasicBlockId),
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ir::{body_builder::BodyBuilder, FunctionId, SourceInfo, TypeId};

    fn body_builder() -> BodyBuilder {
        BodyBuilder::new(FunctionId(0), SourceInfo::dummy())
    }

    #[test]
    fn test_if_else_merge() {
        let mut builder = body_builder();
        let then_block = builder.make_block();
        let else_block = builder.make_block();
        let merge_block = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        builder.branch(v0, then_block, else_block, SourceInfo::dummy());

        builder.move_to_block(then_block);
        builder.jump(merge_block, SourceInfo::dummy());

        builder.move_to_block(else_block);
        builder.jump(merge_block, SourceInfo::dummy());

        builder.move_to_block(merge_block);
        let dummy_value = builder.make_unit(dummy_ty);
        builder.ret(dummy_value, SourceInfo::dummy());

        let func = builder.build();

        let post_dom_tree = PostDomTree::compute(&func);
        let entry_block = func.order.entry();
        assert_eq!(
            post_dom_tree.post_idom(entry_block),
            PostIDom::Block(merge_block)
        );
        assert_eq!(
            post_dom_tree.post_idom(then_block),
            PostIDom::Block(merge_block)
        );
        assert_eq!(
            post_dom_tree.post_idom(else_block),
            PostIDom::Block(merge_block)
        );
        assert_eq!(post_dom_tree.post_idom(merge_block), PostIDom::DummyExit);
    }

    #[test]
    fn test_if_else_return() {
        let mut builder = body_builder();
        let then_block = builder.make_block();
        let else_block = builder.make_block();
        let merge_block = builder.make_block();

        let dummy_ty = TypeId(0);
        let dummy_value = builder.make_unit(dummy_ty);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        builder.branch(v0, then_block, else_block, SourceInfo::dummy());

        builder.move_to_block(then_block);
        builder.jump(merge_block, SourceInfo::dummy());

        builder.move_to_block(else_block);
        builder.ret(dummy_value, SourceInfo::dummy());

        builder.move_to_block(merge_block);
        builder.ret(dummy_value, SourceInfo::dummy());

        let func = builder.build();

        let post_dom_tree = PostDomTree::compute(&func);
        let entry_block = func.order.entry();
        assert_eq!(post_dom_tree.post_idom(entry_block), PostIDom::DummyExit,);
        assert_eq!(
            post_dom_tree.post_idom(then_block),
            PostIDom::Block(merge_block),
        );
        assert_eq!(post_dom_tree.post_idom(else_block), PostIDom::DummyExit);
        assert_eq!(post_dom_tree.post_idom(merge_block), PostIDom::DummyExit);
    }

    #[test]
    fn test_if_non_else() {
        let mut builder = body_builder();
        let then_block = builder.make_block();
        let merge_block = builder.make_block();

        let dummy_ty = TypeId(0);
        let dummy_value = builder.make_unit(dummy_ty);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        builder.branch(v0, then_block, merge_block, SourceInfo::dummy());

        builder.move_to_block(then_block);
        builder.jump(merge_block, SourceInfo::dummy());

        builder.move_to_block(merge_block);
        builder.ret(dummy_value, SourceInfo::dummy());

        let func = builder.build();

        let post_dom_tree = PostDomTree::compute(&func);
        let entry_block = func.order.entry();
        assert_eq!(
            post_dom_tree.post_idom(entry_block),
            PostIDom::Block(merge_block),
        );
        assert_eq!(
            post_dom_tree.post_idom(then_block),
            PostIDom::Block(merge_block),
        );
        assert_eq!(post_dom_tree.post_idom(merge_block), PostIDom::DummyExit);
    }

    #[test]
    fn test_loop() {
        let mut builder = body_builder();
        let block1 = builder.make_block();
        let block2 = builder.make_block();
        let block3 = builder.make_block();
        let block4 = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);

        builder.branch(v0, block1, block2, SourceInfo::dummy());

        builder.move_to_block(block1);
        builder.jump(block3, SourceInfo::dummy());

        builder.move_to_block(block2);
        builder.branch(v0, block3, block4, SourceInfo::dummy());

        builder.move_to_block(block3);
        let dummy_value = builder.make_unit(dummy_ty);
        builder.ret(dummy_value, SourceInfo::dummy());

        builder.move_to_block(block4);
        builder.jump(block2, SourceInfo::dummy());

        let func = builder.build();

        let post_dom_tree = PostDomTree::compute(&func);
        let entry_block = func.order.entry();
        assert_eq!(
            post_dom_tree.post_idom(entry_block),
            PostIDom::Block(block3),
        );
        assert_eq!(post_dom_tree.post_idom(block1), PostIDom::Block(block3));
        assert_eq!(post_dom_tree.post_idom(block2), PostIDom::Block(block3));
        assert_eq!(post_dom_tree.post_idom(block3), PostIDom::DummyExit);
        assert_eq!(post_dom_tree.post_idom(block4), PostIDom::Block(block2));
    }

    #[test]
    fn test_pd_complex() {
        let mut builder = body_builder();
        let block1 = builder.make_block();
        let block2 = builder.make_block();
        let block3 = builder.make_block();
        let block4 = builder.make_block();
        let block5 = builder.make_block();
        let block6 = builder.make_block();
        let block7 = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);

        builder.branch(v0, block1, block2, SourceInfo::dummy());

        builder.move_to_block(block1);
        builder.jump(block6, SourceInfo::dummy());

        builder.move_to_block(block2);
        builder.branch(v0, block3, block4, SourceInfo::dummy());

        builder.move_to_block(block3);
        builder.jump(block5, SourceInfo::dummy());

        builder.move_to_block(block4);
        builder.jump(block5, SourceInfo::dummy());

        builder.move_to_block(block5);
        builder.jump(block6, SourceInfo::dummy());

        builder.move_to_block(block6);
        builder.jump(block7, SourceInfo::dummy());

        builder.move_to_block(block7);
        let dummy_value = builder.make_unit(dummy_ty);
        builder.ret(dummy_value, SourceInfo::dummy());

        let func = builder.build();

        let post_dom_tree = PostDomTree::compute(&func);
        let entry_block = func.order.entry();
        assert_eq!(
            post_dom_tree.post_idom(entry_block),
            PostIDom::Block(block6),
        );
        assert_eq!(post_dom_tree.post_idom(block1), PostIDom::Block(block6));
        assert_eq!(post_dom_tree.post_idom(block2), PostIDom::Block(block5));
        assert_eq!(post_dom_tree.post_idom(block3), PostIDom::Block(block5));
        assert_eq!(post_dom_tree.post_idom(block4), PostIDom::Block(block5));
        assert_eq!(post_dom_tree.post_idom(block5), PostIDom::Block(block6));
        assert_eq!(post_dom_tree.post_idom(block6), PostIDom::Block(block7));
        assert_eq!(post_dom_tree.post_idom(block7), PostIDom::DummyExit);
    }
}
