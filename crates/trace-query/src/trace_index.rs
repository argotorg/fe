use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

use common::origin::OriginExportKey;
use serde::{Deserialize, Serialize};
use trace_facts::{
    OriginEdgeFact, OriginEdgeLabel, OriginEdgeTraversalClass, SourceSpanFact, TraceFact,
    TraceSnapshot,
};

#[derive(Debug)]
pub struct TraceIndex<'a> {
    edges_by_from: BTreeMap<OriginExportKey, Vec<&'a OriginEdgeFact>>,
    source_spans: BTreeMap<OriginExportKey, &'a SourceSpanFact>,
    reachable_cache:
        RefCell<BTreeMap<(OriginExportKey, TraceReachabilityPolicy), BTreeSet<OriginExportKey>>>,
    cache_hits: Cell<usize>,
    cache_misses: Cell<usize>,
}

impl<'a> TraceIndex<'a> {
    pub fn new(snapshot: &'a TraceSnapshot) -> Self {
        let mut edges_by_from = BTreeMap::<OriginExportKey, Vec<&OriginEdgeFact>>::new();
        let mut source_spans = BTreeMap::new();
        for fact in snapshot.facts() {
            match fact {
                TraceFact::OriginEdge(edge) => {
                    edges_by_from
                        .entry(edge.from.clone())
                        .or_default()
                        .push(edge);
                }
                TraceFact::SourceSpan(span) => {
                    source_spans.insert(span.origin.clone(), span);
                }
                _ => {}
            }
        }
        Self {
            edges_by_from,
            source_spans,
            reachable_cache: RefCell::new(BTreeMap::new()),
            cache_hits: Cell::new(0),
            cache_misses: Cell::new(0),
        }
    }

    pub fn cache_stats(&self) -> TraceIndexCacheStats {
        TraceIndexCacheStats {
            hits: self.cache_hits.get(),
            misses: self.cache_misses.get(),
            entries: self.reachable_cache.borrow().len(),
        }
    }

    pub fn origin_reaches(
        &self,
        from: &OriginExportKey,
        to: &OriginExportKey,
        policy: TraceReachabilityPolicy,
    ) -> bool {
        self.reachable_targets(from, policy).contains(to)
    }

    pub fn reachable_targets(
        &self,
        root: &OriginExportKey,
        policy: TraceReachabilityPolicy,
    ) -> BTreeSet<OriginExportKey> {
        let key = (root.clone(), policy);
        if let Some(cached) = self.reachable_cache.borrow().get(&key) {
            self.cache_hits.set(self.cache_hits.get() + 1);
            return cached.clone();
        }
        self.cache_misses.set(self.cache_misses.get() + 1);
        let reachable = self.compute_reachable(root, policy);
        self.reachable_cache
            .borrow_mut()
            .insert(key, reachable.clone());
        reachable
    }

    pub fn source_candidates_for_instruction(
        &self,
        instruction: &OriginExportKey,
        policy: TraceReachabilityPolicy,
    ) -> Vec<OriginExportKey> {
        self.reachable_targets(instruction, policy)
            .into_iter()
            .filter(|key| is_precise_source_candidate(key) && self.source_spans.contains_key(key))
            .collect()
    }

    pub fn phase_frontier(
        &self,
        root: &OriginExportKey,
        policy: TraceReachabilityPolicy,
    ) -> BTreeMap<TracePhase, BTreeSet<OriginExportKey>> {
        let mut frontier = BTreeMap::<TracePhase, BTreeSet<OriginExportKey>>::new();
        if let Some(phase) = TracePhase::from_key(root) {
            frontier.entry(phase).or_default().insert(root.clone());
        }
        for key in self.reachable_targets(root, policy) {
            if let Some(phase) = TracePhase::from_key(&key) {
                frontier.entry(phase).or_default().insert(key);
            }
        }
        frontier
    }

    pub fn highest_phase_reached(
        &self,
        root: &OriginExportKey,
        policy: TraceReachabilityPolicy,
    ) -> Option<TracePhase> {
        self.phase_frontier(root, policy)
            .keys()
            .next_back()
            .copied()
    }

    pub fn reachability_witness_path(
        &self,
        from: &OriginExportKey,
        to: &OriginExportKey,
        policy: TraceReachabilityPolicy,
    ) -> Option<Vec<TraceEdgeStep>> {
        let mut queue = VecDeque::from([from.clone()]);
        let mut seen = BTreeSet::from([from.clone()]);
        let mut parent = BTreeMap::<OriginExportKey, TraceEdgeStep>::new();
        while let Some(key) = queue.pop_front() {
            for edge in self.edges_by_from.get(&key).into_iter().flatten() {
                if !policy.allows_edge(edge) {
                    continue;
                }
                if !seen.insert(edge.to.clone()) {
                    continue;
                }
                parent.insert(
                    edge.to.clone(),
                    TraceEdgeStep {
                        from: edge.from.clone(),
                        to: edge.to.clone(),
                        label: edge.label,
                    },
                );
                if &edge.to == to {
                    return Some(reconstruct_path(from, to, &parent));
                }
                queue.push_back(edge.to.clone());
            }
        }
        None
    }

    fn compute_reachable(
        &self,
        root: &OriginExportKey,
        policy: TraceReachabilityPolicy,
    ) -> BTreeSet<OriginExportKey> {
        let mut reachable = BTreeSet::new();
        let mut stack = vec![root.clone()];
        while let Some(key) = stack.pop() {
            if !reachable.insert(key.clone()) {
                continue;
            }
            for edge in self.edges_by_from.get(&key).into_iter().flatten() {
                if policy.allows_edge(edge) {
                    stack.push(edge.to.clone());
                }
            }
        }
        reachable.remove(root);
        reachable
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraceIndexCacheStats {
    pub hits: usize,
    pub misses: usize,
    pub entries: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraceEdgeStep {
    pub from: OriginExportKey,
    pub to: OriginExportKey,
    pub label: OriginEdgeLabel,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TraceReachabilityPolicy {
    ExactOnly,
    ExactPlusSynthetic,
    ExactPlusContextual,
    DebugAttribution,
    UiHighlighting,
}

impl TraceReachabilityPolicy {
    pub fn allows_edge(self, edge: &OriginEdgeFact) -> bool {
        self.allows_class(edge.traversal_class())
    }

    pub const fn allows_class(self, class: OriginEdgeTraversalClass) -> bool {
        match self {
            Self::ExactOnly => is_exact_class(class),
            Self::ExactPlusSynthetic => {
                is_exact_class(class) || matches!(class, OriginEdgeTraversalClass::Synthetic)
            }
            Self::ExactPlusContextual | Self::DebugAttribution => {
                !matches!(class, OriginEdgeTraversalClass::Unmapped)
            }
            Self::UiHighlighting => !matches!(class, OriginEdgeTraversalClass::Unmapped),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TracePhase {
    Source,
    Hir,
    Mir,
    SonatinaPre,
    SonatinaPost,
    Backend,
    Bytecode,
    Runtime,
}

impl TracePhase {
    pub fn from_key(key: &OriginExportKey) -> Option<Self> {
        let kind = key.kind();
        if kind.starts_with("source.") {
            Some(Self::Source)
        } else if kind.starts_with("hir.") {
            Some(Self::Hir)
        } else if kind.starts_with("mir.") || kind.starts_with("runtime.") {
            Some(Self::Mir)
        } else if kind.starts_with("sonatina.pre") {
            Some(Self::SonatinaPre)
        } else if kind.starts_with("sonatina.post") || kind.starts_with("sonatina.") {
            Some(Self::SonatinaPost)
        } else if kind.starts_with("backend.") {
            Some(Self::Backend)
        } else if kind.starts_with("bytecode.") || kind == "code.object" {
            Some(Self::Bytecode)
        } else if kind.starts_with("execution.") || kind.starts_with("runtime.step") {
            Some(Self::Runtime)
        } else {
            None
        }
    }
}

const fn is_exact_class(class: OriginEdgeTraversalClass) -> bool {
    matches!(
        class,
        OriginEdgeTraversalClass::ExactAttribution | OriginEdgeTraversalClass::SnapshotAlias
    )
}

fn is_precise_source_candidate(key: &OriginExportKey) -> bool {
    key.kind() != "code.object" && key.kind() != "source.file"
}

fn reconstruct_path(
    from: &OriginExportKey,
    to: &OriginExportKey,
    parent: &BTreeMap<OriginExportKey, TraceEdgeStep>,
) -> Vec<TraceEdgeStep> {
    let mut key = to.clone();
    let mut path = Vec::new();
    while &key != from {
        let Some(step) = parent.get(&key) else {
            break;
        };
        path.push(step.clone());
        key = step.from.clone();
    }
    path.reverse();
    path
}

#[cfg(test)]
mod tests {
    use common::origin::OriginExportKey;
    use trace_facts::{
        CompilerPhase, OriginEdgeFact, OriginEdgeLabel, OriginNodeFact, OriginNodeKind,
        SourceFileFact, SourceSpanFact, TraceBundle, TraceFact, TraceMetadata, TraceSnapshot,
    };

    use super::{TraceIndex, TracePhase, TraceReachabilityPolicy};

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(key: &OriginExportKey) -> TraceFact {
        TraceFact::OriginNode(OriginNodeFact::new(
            key.clone(),
            OriginNodeKind::new(key.kind()),
        ))
    }

    fn snapshot(facts: Vec<TraceFact>) -> TraceSnapshot {
        TraceSnapshot::new(TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string()],
                "demo.fe",
                vec!["optimize=2".to_string()],
            ),
            facts,
        ))
        .unwrap()
    }

    #[test]
    fn exact_policy_does_not_cross_synthetic_backend_or_unmapped_edges() {
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let source = key("hir.expr", "demo", "expr:0");
        let synthetic = key("hir.expr", "demo", "expr:synthetic");
        let backend = key("backend.event", "demo", "event:0");
        let unmapped = key("hir.expr", "demo", "expr:unmapped");
        let facts = vec![
            node(&instruction),
            node(&source),
            node(&synthetic),
            node(&backend),
            node(&unmapped),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                source.clone(),
                OriginEdgeLabel::LoweredFrom,
                None,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                synthetic.clone(),
                OriginEdgeLabel::SyntheticFor,
                None,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                backend.clone(),
                OriginEdgeLabel::BackendPrepared,
                None,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                unmapped.clone(),
                OriginEdgeLabel::Unmapped,
                None,
            )),
        ];
        let snapshot = snapshot(facts);
        let index = TraceIndex::new(&snapshot);

        assert!(index.origin_reaches(&instruction, &source, TraceReachabilityPolicy::ExactOnly));
        assert!(!index.origin_reaches(
            &instruction,
            &synthetic,
            TraceReachabilityPolicy::ExactOnly
        ));
        assert!(!index.origin_reaches(&instruction, &backend, TraceReachabilityPolicy::ExactOnly));
        assert!(!index.origin_reaches(
            &instruction,
            &unmapped,
            TraceReachabilityPolicy::UiHighlighting
        ));
        assert!(index.origin_reaches(
            &instruction,
            &synthetic,
            TraceReachabilityPolicy::ExactPlusSynthetic
        ));
        assert!(index.origin_reaches(
            &instruction,
            &backend,
            TraceReachabilityPolicy::ExactPlusContextual
        ));
    }

    #[test]
    fn repeated_reachability_queries_hit_cache() {
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let source = key("hir.expr", "demo", "expr:0");
        let facts = vec![
            node(&instruction),
            node(&source),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                source,
                OriginEdgeLabel::LoweredFrom,
                None,
            )),
        ];
        let snapshot = snapshot(facts);
        let index = TraceIndex::new(&snapshot);

        index.reachable_targets(&instruction, TraceReachabilityPolicy::ExactOnly);
        index.reachable_targets(&instruction, TraceReachabilityPolicy::ExactOnly);

        assert_eq!(index.cache_stats().misses, 1);
        assert_eq!(index.cache_stats().hits, 1);
        assert_eq!(index.cache_stats().entries, 1);
    }

    #[test]
    fn source_candidates_and_phase_frontier_are_derived_from_exact_paths() {
        let file = key("source.file", "demo", "demo.fe");
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let sonatina = key("sonatina.post.inst", "demo", "inst:0");
        let source = key("hir.expr", "demo", "expr:0");
        let facts = vec![
            node(&file),
            node(&instruction),
            node(&sonatina),
            node(&source),
            TraceFact::SourceFile(SourceFileFact::new(
                file.clone(),
                "file:///demo.fe",
                "demo.fe",
                "blake3:0000000000000000000000000000000000000000000000000000000000000001",
                Some(0),
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(source.clone(), file, 0, 1, 1, 1, 1, 2)),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                sonatina,
                OriginEdgeLabel::EmittedFrom,
                None,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                key("sonatina.post.inst", "demo", "inst:0"),
                source.clone(),
                OriginEdgeLabel::LoweredFrom,
                None,
            )),
        ];
        let snapshot = snapshot(facts);
        let index = TraceIndex::new(&snapshot);

        assert_eq!(
            index.source_candidates_for_instruction(
                &instruction,
                TraceReachabilityPolicy::ExactOnly
            ),
            vec![source]
        );
        assert_eq!(
            index.highest_phase_reached(&instruction, TraceReachabilityPolicy::ExactOnly),
            Some(TracePhase::Bytecode)
        );
        assert!(
            index
                .phase_frontier(&instruction, TraceReachabilityPolicy::ExactOnly)
                .contains_key(&TracePhase::SonatinaPost)
        );
    }

    #[test]
    fn source_candidates_ignore_code_object_whole_file_spans() {
        let file = key("source.file", "demo", "demo.fe");
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let code_object = key("code.object", "demo", "runtime");
        let source = key("hir.expr", "demo", "expr:0");
        let facts = vec![
            node(&file),
            node(&instruction),
            node(&code_object),
            node(&source),
            TraceFact::SourceFile(SourceFileFact::new(
                file.clone(),
                "file:///demo.fe",
                "demo.fe",
                "blake3:0000000000000000000000000000000000000000000000000000000000000001",
                Some(0),
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                code_object.clone(),
                file.clone(),
                0,
                100,
                1,
                1,
                9,
                1,
            )),
            TraceFact::SourceSpan(SourceSpanFact::new(
                source.clone(),
                file,
                10,
                11,
                2,
                4,
                2,
                5,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                code_object,
                OriginEdgeLabel::EmittedFrom,
                None,
            )),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                source.clone(),
                OriginEdgeLabel::LoweredFrom,
                None,
            )),
        ];
        let snapshot = snapshot(facts);
        let index = TraceIndex::new(&snapshot);

        assert_eq!(
            index.source_candidates_for_instruction(
                &instruction,
                TraceReachabilityPolicy::ExactOnly
            ),
            vec![source]
        );
    }

    #[test]
    fn exact_policy_treats_bytecode_code_object_edges_as_structural() {
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let code_object = key("code.object", "demo", "runtime");
        let facts = vec![
            node(&instruction),
            node(&code_object),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                code_object.clone(),
                OriginEdgeLabel::EmittedFrom,
                Some(CompilerPhase::BytecodeEmission),
            )),
        ];
        let snapshot = snapshot(facts);
        let index = TraceIndex::new(&snapshot);

        assert!(!index.origin_reaches(
            &instruction,
            &code_object,
            TraceReachabilityPolicy::ExactOnly
        ));
        assert!(index.origin_reaches(
            &instruction,
            &code_object,
            TraceReachabilityPolicy::UiHighlighting
        ));
    }

    #[test]
    fn exact_policy_rejects_bytecode_frontend_edges_from_emission_phase() {
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let runtime_stmt = key("runtime.stmt", "demo", "block:0:stmt:0");
        let facts = vec![
            node(&instruction),
            node(&runtime_stmt),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                instruction.clone(),
                runtime_stmt.clone(),
                OriginEdgeLabel::LoweredFrom,
                Some(CompilerPhase::BytecodeEmission),
            )),
        ];
        let snapshot = snapshot(facts);
        let index = TraceIndex::new(&snapshot);

        assert!(!index.origin_reaches(
            &instruction,
            &runtime_stmt,
            TraceReachabilityPolicy::ExactOnly
        ));
        assert!(index.origin_reaches(
            &instruction,
            &runtime_stmt,
            TraceReachabilityPolicy::DebugAttribution
        ));
    }

    #[test]
    fn exact_policy_crosses_snapshot_alias_for_optimized_attribution_continuity() {
        let post = key("sonatina.postopt.inst", "demo", "inst:post");
        let pre = key("sonatina.preopt.inst", "demo", "inst:pre");
        let facts = vec![
            node(&post),
            node(&pre),
            TraceFact::OriginEdge(OriginEdgeFact::new(
                post.clone(),
                pre.clone(),
                OriginEdgeLabel::PreservedSnapshotIdentity,
                Some(CompilerPhase::SonatinaPostOpt),
            )),
        ];
        let snapshot = snapshot(facts);
        let index = TraceIndex::new(&snapshot);

        assert!(index.origin_reaches(&post, &pre, TraceReachabilityPolicy::ExactOnly));
    }
}
