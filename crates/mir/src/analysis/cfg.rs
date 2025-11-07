use fxhash::FxHashMap;

use crate::ir::{BasicBlockId, FunctionBody, InstId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ControlFlowGraph {
    entry: BasicBlockId,
    blocks: FxHashMap<BasicBlockId, BlockNode>,
    pub(super) exits: Vec<BasicBlockId>,
}

impl ControlFlowGraph {
    pub fn compute(func: &FunctionBody) -> Self {
        let entry = func.order.entry();
        let mut cfg = Self {
            entry,
            blocks: FxHashMap::default(),
            exits: vec![],
        };

        for block in func.order.iter_block() {
            let terminator = func
                .order
                .terminator(&func.store, block)
                .expect("a block must have terminator");
            cfg.analyze_terminator(func, terminator);
        }

        cfg
    }

    pub fn entry(&self) -> BasicBlockId {
        self.entry
    }

    pub fn preds(&self, block: BasicBlockId) -> &[BasicBlockId] {
        self.blocks[&block].preds()
    }

    pub fn succs(&self, block: BasicBlockId) -> &[BasicBlockId] {
        self.blocks[&block].succs()
    }

    pub fn post_order(&self) -> CfgPostOrder {
        CfgPostOrder::new(self)
    }

    pub(super) fn add_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        self.node_mut(to).push_pred(from);
        self.node_mut(from).push_succ(to);
    }

    pub(super) fn reverse_edge(&mut self, new_entry: BasicBlockId, new_exits: Vec<BasicBlockId>) {
        for (_, block) in self.blocks.iter_mut() {
            block.reverse_edge()
        }

        self.entry = new_entry;
        self.exits = new_exits;
    }

    fn analyze_terminator(&mut self, func: &FunctionBody, terminator: InstId) {
        let block = func.order.inst_block(terminator);
        let branch_info = func.store.branch_info(terminator);
        if branch_info.is_not_a_branch() {
            self.node_mut(block);
            self.exits.push(block)
        } else {
            for dest in branch_info.block_iter() {
                self.add_edge(block, dest)
            }
        }
    }

    fn node_mut(&mut self, block: BasicBlockId) -> &mut BlockNode {
        self.blocks.entry(block).or_default()
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
struct BlockNode {
    preds: Vec<BasicBlockId>,
    succs: Vec<BasicBlockId>,
}

impl BlockNode {
    fn push_pred(&mut self, pred: BasicBlockId) {
        self.preds.push(pred);
    }

    fn push_succ(&mut self, succ: BasicBlockId) {
        self.succs.push(succ);
    }

    fn preds(&self) -> &[BasicBlockId] {
        &self.preds
    }

    fn succs(&self) -> &[BasicBlockId] {
        &self.succs
    }

    fn reverse_edge(&mut self) {
        std::mem::swap(&mut self.preds, &mut self.succs)
    }
}

pub struct CfgPostOrder<'a> {
    cfg: &'a ControlFlowGraph,
    node_state: FxHashMap<BasicBlockId, NodeState>,
    stack: Vec<BasicBlockId>,
}

impl<'a> CfgPostOrder<'a> {
    fn new(cfg: &'a ControlFlowGraph) -> Self {
        let stack = vec![cfg.entry()];

        Self {
            cfg,
            node_state: FxHashMap::default(),
            stack,
        }
    }
}

impl Iterator for CfgPostOrder<'_> {
    type Item = BasicBlockId;

    fn next(&mut self) -> Option<BasicBlockId> {
        while let Some(&block) = self.stack.last() {
            let node_state = self.node_state.entry(block).or_default();
            if *node_state == NodeState::Unvisited {
                *node_state = NodeState::Visited;
                for &succ in self.cfg.succs(block) {
                    let pred_state = self.node_state.entry(succ).or_default();
                    if *pred_state == NodeState::Unvisited {
                        self.stack.push(succ);
                    }
                }
            } else {
                self.stack.pop().unwrap();
                if *node_state != NodeState::Finished {
                    *node_state = NodeState::Finished;
                    return Some(block);
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NodeState {
    Unvisited,
    Visited,
    Finished,
}

impl Default for NodeState {
    fn default() -> Self {
        Self::Unvisited
    }
}
