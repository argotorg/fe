use std::collections::BTreeSet;

/// Reachability-aware control-flow analysis over dense block indices.
pub struct ControlFlowAnalysis {
    reachable: BTreeSet<usize>,
    dominators: Vec<BTreeSet<usize>>,
}

impl ControlFlowAnalysis {
    pub fn new(entry: usize, predecessors: &[Vec<usize>]) -> Self {
        let block_count = predecessors.len();
        let mut successors = vec![Vec::new(); block_count];
        for (block, preds) in predecessors.iter().enumerate() {
            for &pred in preds {
                if pred < block_count {
                    successors[pred].push(block);
                }
            }
        }
        let mut reachable = BTreeSet::new();
        let mut stack = (entry < block_count)
            .then_some(entry)
            .into_iter()
            .collect::<Vec<_>>();
        while let Some(block) = stack.pop() {
            if reachable.insert(block) {
                stack.extend(successors[block].iter().copied());
            }
        }

        let mut dominators = vec![BTreeSet::new(); block_count];
        for &block in &reachable {
            dominators[block] = reachable.clone();
        }
        if reachable.contains(&entry) {
            dominators[entry] = BTreeSet::from([entry]);
        }
        let mut changed = true;
        while changed {
            changed = false;
            for &block in &reachable {
                if block == entry {
                    continue;
                }
                let mut preds = predecessors[block]
                    .iter()
                    .copied()
                    .filter(|pred| reachable.contains(pred));
                let mut next = preds
                    .next()
                    .map(|first| dominators[first].clone())
                    .unwrap_or_default();
                for pred in preds {
                    next = next.intersection(&dominators[pred]).copied().collect();
                }
                next.insert(block);
                if next != dominators[block] {
                    dominators[block] = next;
                    changed = true;
                }
            }
        }
        Self {
            reachable,
            dominators,
        }
    }

    pub fn is_backedge(&self, from: usize, to: usize) -> bool {
        self.reachable.contains(&from)
            && self.reachable.contains(&to)
            && self.dominators[from].contains(&to)
    }

    pub fn natural_loop_members(
        &self,
        predecessors: &[Vec<usize>],
        header: usize,
        latch: usize,
    ) -> Vec<usize> {
        if !self.is_backedge(latch, header) {
            return Vec::new();
        }
        let mut members = BTreeSet::from([header, latch]);
        let mut stack = (latch != header)
            .then_some(latch)
            .into_iter()
            .collect::<Vec<_>>();
        while let Some(block) = stack.pop() {
            for predecessor in predecessors[block].iter().copied() {
                if predecessor >= self.dominators.len()
                    || !self.dominators[predecessor].contains(&header)
                    || !members.insert(predecessor)
                {
                    continue;
                }
                if predecessor != header {
                    stack.push(predecessor);
                }
            }
        }
        members.into_iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::ControlFlowAnalysis;

    #[test]
    fn unreachable_cycle_is_not_a_loop() {
        let predecessors = vec![vec![], vec![2], vec![1]];
        let analysis = ControlFlowAnalysis::new(0, &predecessors);
        assert!(!analysis.is_backedge(1, 2));
        assert!(!analysis.is_backedge(2, 1));
    }

    #[test]
    fn unreachable_predecessor_does_not_hide_or_join_reachable_loop() {
        // 0 -> 1 -> 2 -> 1, with unreachable 3 -> 2.
        let predecessors = vec![vec![], vec![0, 2], vec![1, 3], vec![]];
        let analysis = ControlFlowAnalysis::new(0, &predecessors);
        assert!(analysis.is_backedge(2, 1));
        assert_eq!(analysis.natural_loop_members(&predecessors, 1, 2), [1, 2]);
    }

    #[test]
    fn self_loop_excludes_preheader() {
        let predecessors = vec![vec![], vec![0, 1]];
        let analysis = ControlFlowAnalysis::new(0, &predecessors);
        assert_eq!(analysis.natural_loop_members(&predecessors, 1, 1), [1]);
    }

    #[test]
    fn multiple_latches_form_distinct_natural_loops() {
        // 0 -> 1, 1 -> 2/3, and both latches return to header 1.
        let predecessors = vec![vec![], vec![0, 2, 3], vec![1], vec![1]];
        let analysis = ControlFlowAnalysis::new(0, &predecessors);
        assert_eq!(analysis.natural_loop_members(&predecessors, 1, 2), [1, 2]);
        assert_eq!(analysis.natural_loop_members(&predecessors, 1, 3), [1, 3]);
    }

    #[test]
    fn backedges_match_path_removal_oracle_for_all_three_block_graphs() {
        // Independent definition: a reachable header dominates a latch iff
        // removing the header makes the latch unreachable from the entry.
        fn reachable(edges: u16, entry: usize, target: usize, removed: Option<usize>) -> bool {
            let mut seen = [false; 3];
            let mut pending = vec![entry];
            while let Some(block) = pending.pop() {
                if Some(block) == removed || seen[block] {
                    continue;
                }
                seen[block] = true;
                for next in 0..3 {
                    if edges & (1 << (block * 3 + next)) != 0 {
                        pending.push(next);
                    }
                }
            }
            seen[target]
        }
        for edges in 0u16..(1 << 9) {
            let predecessors = (0..3)
                .map(|to| {
                    (0..3)
                        .filter(|from| edges & (1 << (from * 3 + to)) != 0)
                        .collect()
                })
                .collect::<Vec<Vec<usize>>>();
            for entry in 0..3 {
                let analysis = ControlFlowAnalysis::new(entry, &predecessors);
                for from in 0..3 {
                    for to in 0..3 {
                        if edges & (1 << (from * 3 + to)) == 0 {
                            continue;
                        }
                        let expected = reachable(edges, entry, from, None)
                            && !reachable(edges, entry, from, Some(to));
                        assert_eq!(
                            analysis.is_backedge(from, to),
                            expected,
                            "edges={edges:#b}, entry={entry}, edge={from}->{to}"
                        );
                    }
                }
            }
        }
    }
}
