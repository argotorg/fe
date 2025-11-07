//! This module contains dominator tree related structs.
//!
//! The algorithm is based on Keith D. Cooper., Timothy J. Harvey., and Ken
//! Kennedy.: A Simple, Fast Dominance Algorithm: <https://www.cs.rice.edu/~keith/EMBED/dom.pdf>

use std::collections::BTreeSet;

use fxhash::FxHashMap;

use crate::ir::BasicBlockId;

use super::cfg::ControlFlowGraph;

#[derive(Debug, Clone)]
pub struct DomTree {
    doms: FxHashMap<BasicBlockId, BasicBlockId>,
    /// CFG sorted in reverse post order.
    rpo: Vec<BasicBlockId>,
}

impl DomTree {
    pub fn compute(cfg: &ControlFlowGraph) -> Self {
        let mut doms = FxHashMap::default();
        doms.insert(cfg.entry(), cfg.entry());
        let mut rpo: Vec<_> = cfg.post_order().collect();
        rpo.reverse();

        let mut domtree = Self { doms, rpo };

        let block_num = domtree.rpo.len();

        let mut rpo_nums = FxHashMap::default();
        for (i, &block) in domtree.rpo.iter().enumerate() {
            rpo_nums.insert(block, (block_num - i) as u32);
        }

        let mut changed = true;
        while changed {
            changed = false;
            for &block in domtree.rpo.iter().skip(1) {
                let processed_pred = match cfg
                    .preds(block)
                    .iter()
                    .find(|pred| domtree.doms.contains_key(pred))
                {
                    Some(pred) => *pred,
                    _ => continue,
                };
                let mut new_dom = processed_pred;

                for &pred in cfg.preds(block) {
                    if pred != processed_pred && domtree.doms.contains_key(&pred) {
                        new_dom = domtree.intersect(new_dom, pred, &rpo_nums);
                    }
                }
                if Some(new_dom) != domtree.doms.get(&block).copied() {
                    changed = true;
                    domtree.doms.insert(block, new_dom);
                }
            }
        }

        domtree
    }

    /// Returns the immediate dominator of the `block`.
    /// Returns None if the `block` is unreachable from the entry block, or the
    /// `block` is the entry block itself.
    pub fn idom(&self, block: BasicBlockId) -> Option<BasicBlockId> {
        if self.rpo[0] == block {
            return None;
        }
        self.doms.get(&block).copied()
    }

    /// Returns `true` if block1 strictly dominates block2.
    pub fn strictly_dominates(&self, block1: BasicBlockId, block2: BasicBlockId) -> bool {
        let mut current_block = block2;
        while let Some(block) = self.idom(current_block) {
            if block == block1 {
                return true;
            }
            current_block = block;
        }

        false
    }

    /// Returns `true` if block1 dominates block2.
    pub fn dominates(&self, block1: BasicBlockId, block2: BasicBlockId) -> bool {
        if block1 == block2 {
            return true;
        }

        self.strictly_dominates(block1, block2)
    }

    /// Returns `true` if block is reachable from the entry block.
    pub fn is_reachable(&self, block: BasicBlockId) -> bool {
        self.idom(block).is_some()
    }

    /// Returns blocks in RPO.
    pub fn rpo(&self) -> &[BasicBlockId] {
        &self.rpo
    }

    fn intersect(
        &self,
        mut b1: BasicBlockId,
        mut b2: BasicBlockId,
        rpo_nums: &FxHashMap<BasicBlockId, u32>,
    ) -> BasicBlockId {
        while b1 != b2 {
            while rpo_nums[&b1] < rpo_nums[&b2] {
                b1 = self.doms[&b1];
            }
            while rpo_nums[&b2] < rpo_nums[&b1] {
                b2 = self.doms[&b2]
            }
        }

        b1
    }

    /// Compute dominance frontiers of each blocks.
    pub fn compute_df(&self, cfg: &ControlFlowGraph) -> DFSet {
        let mut df = DFSet::default();

        for &block in &self.rpo {
            let preds = cfg.preds(block);
            if preds.len() < 2 {
                continue;
            }

            for pred in preds {
                let mut runner = *pred;
                while self.doms.get(&block) != Some(&runner) && self.is_reachable(runner) {
                    df.0.entry(runner).or_default().insert(block);
                    runner = self.doms[&runner];
                }
            }
        }

        df
    }
}

/// Dominance frontiers of each blocks.
#[derive(Default, Debug)]
pub struct DFSet(FxHashMap<BasicBlockId, BTreeSet<BasicBlockId>>);

impl DFSet {
    /// Returns all dominance frontieres of a `block`.
    pub fn frontiers(
        &self,
        block: BasicBlockId,
    ) -> Option<impl Iterator<Item = BasicBlockId> + '_> {
        self.0.get(&block).map(|set| set.iter().copied())
    }

    /// Returns number of frontier blocks of a `block`.
    pub fn frontier_num(&self, block: BasicBlockId) -> usize {
        self.0.get(&block).map(BTreeSet::len).unwrap_or(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ir::{body_builder::BodyBuilder, FunctionBody, FunctionId, SourceInfo, TypeId};

    fn calc_dom(func: &FunctionBody) -> (DomTree, DFSet) {
        let cfg = ControlFlowGraph::compute(func);
        let domtree = DomTree::compute(&cfg);
        let df = domtree.compute_df(&cfg);
        (domtree, df)
    }

    fn body_builder() -> BodyBuilder {
        BodyBuilder::new(FunctionId(0), SourceInfo::dummy())
    }

    #[test]
    fn dom_tree_if_else() {
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

        let (dom_tree, df) = calc_dom(&func);
        let entry_block = func.order.entry();
        assert_eq!(dom_tree.idom(entry_block), None);
        assert_eq!(dom_tree.idom(then_block), Some(entry_block));
        assert_eq!(dom_tree.idom(else_block), Some(entry_block));
        assert_eq!(dom_tree.idom(merge_block), Some(entry_block));

        assert_eq!(df.frontier_num(entry_block), 0);
        assert_eq!(df.frontier_num(then_block), 1);
        assert_eq!(
            df.frontiers(then_block).unwrap().next().unwrap(),
            merge_block
        );
        assert_eq!(
            df.frontiers(else_block).unwrap().next().unwrap(),
            merge_block
        );
        assert_eq!(df.frontier_num(merge_block), 0);
    }

    #[test]
    fn unreachable_edge() {
        let mut builder = body_builder();

        let block1 = builder.make_block();
        let block2 = builder.make_block();
        let block3 = builder.make_block();
        let block4 = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        builder.branch(v0, block1, block2, SourceInfo::dummy());

        builder.move_to_block(block1);
        builder.jump(block4, SourceInfo::dummy());

        builder.move_to_block(block2);
        builder.jump(block4, SourceInfo::dummy());

        builder.move_to_block(block3);
        builder.jump(block4, SourceInfo::dummy());

        builder.move_to_block(block4);
        let dummy_value = builder.make_unit(dummy_ty);
        builder.ret(dummy_value, SourceInfo::dummy());

        let func = builder.build();

        let (dom_tree, _) = calc_dom(&func);
        let entry_block = func.order.entry();
        assert_eq!(dom_tree.idom(entry_block), None);
        assert_eq!(dom_tree.idom(block1), Some(entry_block));
        assert_eq!(dom_tree.idom(block2), Some(entry_block));
        assert_eq!(dom_tree.idom(block3), None);
        assert!(!dom_tree.is_reachable(block3));
        assert_eq!(dom_tree.idom(block4), Some(entry_block));
    }

    #[test]
    fn dom_tree_complex() {
        let mut builder = body_builder();

        let block1 = builder.make_block();
        let block2 = builder.make_block();
        let block3 = builder.make_block();
        let block4 = builder.make_block();
        let block5 = builder.make_block();
        let block6 = builder.make_block();
        let block7 = builder.make_block();
        let block8 = builder.make_block();
        let block9 = builder.make_block();
        let block10 = builder.make_block();
        let block11 = builder.make_block();
        let block12 = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        builder.branch(v0, block2, block1, SourceInfo::dummy());

        builder.move_to_block(block1);
        builder.branch(v0, block6, block3, SourceInfo::dummy());

        builder.move_to_block(block2);
        builder.branch(v0, block7, block4, SourceInfo::dummy());

        builder.move_to_block(block3);
        builder.branch(v0, block6, block5, SourceInfo::dummy());

        builder.move_to_block(block4);
        builder.branch(v0, block7, block2, SourceInfo::dummy());

        builder.move_to_block(block5);
        builder.branch(v0, block10, block8, SourceInfo::dummy());

        builder.move_to_block(block6);
        builder.jump(block9, SourceInfo::dummy());

        builder.move_to_block(block7);
        builder.jump(block12, SourceInfo::dummy());

        builder.move_to_block(block8);
        builder.jump(block11, SourceInfo::dummy());

        builder.move_to_block(block9);
        builder.jump(block8, SourceInfo::dummy());

        builder.move_to_block(block10);
        builder.jump(block11, SourceInfo::dummy());

        builder.move_to_block(block11);
        builder.branch(v0, block12, block2, SourceInfo::dummy());

        builder.move_to_block(block12);
        let dummy_value = builder.make_unit(dummy_ty);
        builder.ret(dummy_value, SourceInfo::dummy());

        let func = builder.build();

        let (dom_tree, _) = calc_dom(&func);
        let entry_block = func.order.entry();
        assert_eq!(dom_tree.idom(entry_block), None);
        assert_eq!(dom_tree.idom(block1), Some(entry_block));
        assert_eq!(dom_tree.idom(block2), Some(entry_block));
        assert_eq!(dom_tree.idom(block3), Some(block1));
        assert_eq!(dom_tree.idom(block4), Some(block2));
        assert_eq!(dom_tree.idom(block5), Some(block3));
        assert_eq!(dom_tree.idom(block6), Some(block1));
        assert_eq!(dom_tree.idom(block7), Some(block2));
        assert_eq!(dom_tree.idom(block8), Some(block1));
        assert_eq!(dom_tree.idom(block9), Some(block6));
        assert_eq!(dom_tree.idom(block10), Some(block5));
        assert_eq!(dom_tree.idom(block11), Some(block1));
        assert_eq!(dom_tree.idom(block12), Some(entry_block));
    }
}
