use std::{collections::VecDeque, convert::Infallible};

use cranelift_entity::{EntityRef, PrimaryMap, SecondaryMap, entity_impl};
use dataflow::{SparseAnalysis, solve_sparse};
use hir::analysis::{
    semantic::{
        SLocalId, SemanticInstance,
        borrowck::{NSTerminatorKind, NormalizedSemanticBody, normalize_semantic_body},
    },
    ty::ty_def::TyId,
};
use rustc_hash::FxHashSet;
use salsa::Update;

use crate::{
    db::MirDb,
    instance::{RuntimeInstanceKey, RuntimeInstanceSource},
    runtime::{RuntimeCarrier, RuntimeClass, RuntimeExitBehavior},
};

use super::{
    classify::{
        AssignmentId, BodyEnv, BodyStaticFacts, InferClassCache, RuntimeVisibleReturnPlan,
        default_return_class, desired_runtime_return_plan, selected_visible_return_for_local,
    },
    infer::{desired_runtime_value_carrier, merge_runtime_carrier, seed_root_provider_carriers},
    interface::runtime_visible_binding_plans,
};
use crate::runtime::synthetic::runtime_synthetic_exit_behavior;

#[derive(Clone, Debug, PartialEq, Eq, Update)]
pub(crate) enum StaticRuntimeReturnDecision<'db> {
    Known(Option<RuntimeClass<'db>>),
    Dynamic,
}

#[derive(Clone)]
pub(crate) struct RuntimeReturnSummary<'db> {
    pub(crate) semantic_body: NormalizedSemanticBody<'db>,
    pub(crate) facts: BodyStaticFacts<'db>,
    pub(crate) return_plan: RuntimeVisibleReturnPlan<'db>,
    pub(crate) default_return_class: Option<RuntimeClass<'db>>,
    pub(crate) param_locals: Box<[SLocalId]>,
    pub(crate) return_locals: Box<[SLocalId]>,
    pub(crate) slice_assignment_ids: PrimaryMap<SliceAssignmentId, AssignmentId>,
    pub(crate) slice_assignment_positions: SecondaryMap<AssignmentId, Option<SliceAssignmentId>>,
    pub(crate) slice_assignments_by_local: Vec<Vec<AssignmentId>>,
    pub(crate) slice_dynamic_dependents_by_local: Vec<Vec<SLocalId>>,
}

impl<'db> PartialEq for RuntimeReturnSummary<'db> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl<'db> Eq for RuntimeReturnSummary<'db> {}

unsafe impl<'db> salsa::Update for RuntimeReturnSummary<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        unsafe {
            *old_pointer = new_value;
        }
        true
    }
}

impl<'db> RuntimeReturnSummary<'db> {
    fn build(db: &'db dyn MirDb, semantic: SemanticInstance<'db>) -> Self {
        let semantic_body = normalize_semantic_body(db, semantic).unwrap_or_else(|err| {
            panic!(
                "semantic normalization failed for {:?}: {err:?}",
                semantic.key(db)
            )
        });
        let facts = BodyStaticFacts::new(db, &semantic_body);
        let param_locals = runtime_visible_binding_plans(db, semantic)
            .iter()
            .map(|entry| entry.local)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let return_locals = semantic_body
            .blocks
            .iter()
            .filter_map(|block| match &block.terminator.kind {
                NSTerminatorKind::Return(Some(value)) => Some(value.local),
                NSTerminatorKind::Goto(_)
                | NSTerminatorKind::Branch { .. }
                | NSTerminatorKind::MatchEnum { .. }
                | NSTerminatorKind::Assert { .. }
                | NSTerminatorKind::Return(None) => None,
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let env = BodyEnv::new(db, &semantic_body, &facts);
        let mut return_plan = desired_runtime_return_plan(db, semantic);
        let mut default_return_class = default_return_class(db, semantic);
        if matches!(return_plan, RuntimeVisibleReturnPlan::Erased) {
            let mut fallback = None;
            let mut all_fallbacks_match = true;
            for local in return_locals.iter().copied() {
                let Some(class) = env.root_transport_fallback_class(local) else {
                    continue;
                };
                match &fallback {
                    Some(fallback) if fallback != &class => all_fallbacks_match = false,
                    None => fallback = Some(class),
                    Some(_) => {}
                }
            }
            if fallback.is_some() {
                return_plan = RuntimeVisibleReturnPlan::PassActual;
                if default_return_class.is_none() && all_fallbacks_match {
                    default_return_class = fallback;
                }
            }
        }

        let mut needed_assignments = FxHashSet::default();
        let mut needed_locals = FxHashSet::default();
        let mut pending = return_locals.iter().copied().collect::<VecDeque<_>>();
        while let Some(local) = pending.pop_front() {
            if !needed_locals.insert(local) {
                continue;
            }
            for dependency in facts.source_locals(local).iter().copied() {
                pending.push_back(dependency);
            }
            for assign_id in facts.assignments_defining_local(local).iter().copied() {
                if needed_assignments.insert(assign_id) && facts.assignment(assign_id).is_some() {
                    for used in facts.assignment_uses(assign_id).iter().copied() {
                        pending.push_back(used);
                    }
                }
            }
        }

        let mut slice_assignment_ids = needed_assignments.into_iter().collect::<Vec<_>>();
        slice_assignment_ids.sort_unstable();
        let slice_assignment_ids: PrimaryMap<SliceAssignmentId, AssignmentId> =
            slice_assignment_ids.into_iter().collect();
        let mut slice_assignment_positions = SecondaryMap::new();
        slice_assignment_positions.resize(facts.assignments().len());
        for (slice_idx, &assign_id) in slice_assignment_ids.iter() {
            slice_assignment_positions[assign_id] = Some(slice_idx);
        }

        let mut slice_assignments_by_local = vec![Vec::new(); semantic_body.locals.len()];
        for (_, &assign_id) in slice_assignment_ids.iter() {
            facts
                .assignment(assign_id)
                .unwrap_or_else(|| panic!("missing sliced assignment {assign_id:?}"));
            for used in facts.assignment_uses(assign_id).iter().copied() {
                slice_assignments_by_local[used.index()].push(assign_id);
            }
        }

        let mut slice_dynamic_dependents_by_local = vec![Vec::new(); semantic_body.locals.len()];
        for local in needed_locals.iter().copied() {
            for dependency in facts.source_locals(local).iter().copied() {
                slice_dynamic_dependents_by_local[dependency.index()].push(local);
            }
        }

        Self {
            semantic_body,
            facts,
            return_plan,
            default_return_class,
            param_locals,
            return_locals,
            slice_assignment_ids,
            slice_assignment_positions,
            slice_assignments_by_local,
            slice_dynamic_dependents_by_local,
        }
    }

    fn env(&self, db: &'db dyn MirDb) -> BodyEnv<'_, 'db> {
        BodyEnv::new(db, &self.semantic_body, &self.facts)
    }
}

#[salsa::tracked(return_ref)]
pub(crate) fn runtime_return_summary<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> RuntimeReturnSummary<'db> {
    RuntimeReturnSummary::build(db, semantic)
}

#[salsa::tracked(
    cycle_fn=runtime_return_class_cycle_recover,
    cycle_initial=runtime_return_class_cycle_initial
)]
pub(crate) fn runtime_return_class<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> Option<RuntimeClass<'db>> {
    let semantic = key.semantic(db)?;
    if let StaticRuntimeReturnDecision::Known(class) = static_runtime_return_decision(db, semantic)
    {
        return class;
    }
    let summary = runtime_return_summary(db, semantic);
    evaluate_runtime_return_class(db, summary, key.params(db), &mut |callee_key| {
        runtime_return_class(db, callee_key)
    })
}

pub(crate) fn runtime_exit_behavior<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> RuntimeExitBehavior {
    match key.source(db) {
        RuntimeInstanceSource::Semantic(semantic) => {
            if semantic.known_never_returns(db) {
                RuntimeExitBehavior::NeverReturns
            } else {
                RuntimeExitBehavior::MayReturn
            }
        }
        RuntimeInstanceSource::Synthetic(synthetic) => {
            runtime_synthetic_exit_behavior(synthetic.spec(db).clone())
        }
    }
}

#[salsa::tracked]
pub(crate) fn static_runtime_return_decision<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> StaticRuntimeReturnDecision<'db> {
    if semantic.key(db).typed_body(db).result_ty() == TyId::unit(db) {
        return StaticRuntimeReturnDecision::Known(None);
    }
    match desired_runtime_return_plan(db, semantic) {
        RuntimeVisibleReturnPlan::Exact(class) => StaticRuntimeReturnDecision::Known(Some(class)),
        RuntimeVisibleReturnPlan::Erased
        | RuntimeVisibleReturnPlan::Constrained(_)
        | RuntimeVisibleReturnPlan::PassActual => StaticRuntimeReturnDecision::Dynamic,
    }
}

fn runtime_return_class_cycle_initial<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> Option<RuntimeClass<'db>> {
    let semantic = key.semantic(db)?;
    if let StaticRuntimeReturnDecision::Known(class) = static_runtime_return_decision(db, semantic)
    {
        return class;
    }
    runtime_return_summary(db, semantic)
        .default_return_class
        .clone()
}

fn runtime_return_class_cycle_recover<'db>(
    _db: &'db dyn MirDb,
    _value: &Option<RuntimeClass<'db>>,
    _count: u32,
    _key: RuntimeInstanceKey<'db>,
) -> salsa::CycleRecoveryAction<Option<RuntimeClass<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

pub(crate) fn evaluate_runtime_return_class<'db>(
    db: &'db dyn MirDb,
    summary: &RuntimeReturnSummary<'db>,
    params: &[RuntimeClass<'db>],
    lookup: &mut impl FnMut(RuntimeInstanceKey<'db>) -> Option<RuntimeClass<'db>>,
) -> Option<RuntimeClass<'db>> {
    let carriers = ReturnSliceInferer::new(db, summary, params, lookup).run();
    let env = summary.env(db);
    let mut returned = Vec::new();
    for local in summary.return_locals.iter().copied() {
        let Some(selected) =
            selected_visible_return_for_local(env, local, &summary.return_plan, &carriers)
        else {
            return summary.default_return_class.clone();
        };
        returned.push(selected.class);
    }
    let Some(first) = returned.pop() else {
        return summary.default_return_class.clone();
    };
    if returned.iter().all(|class| class == &first) {
        Some(first)
    } else {
        summary.default_return_class.clone()
    }
}

struct ReturnSliceInferer<'summary, 'lookup, 'db> {
    db: &'db dyn MirDb,
    summary: &'summary RuntimeReturnSummary<'db>,
    carriers: Vec<RuntimeCarrier<'db>>,
    class_cache: InferClassCache<'db>,
    pending_dependents: Vec<SliceAssignmentId>,
    lookup: &'lookup mut dyn FnMut(RuntimeInstanceKey<'db>) -> Option<RuntimeClass<'db>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct SliceAssignmentId(u32);
entity_impl!(SliceAssignmentId);

impl<'summary, 'lookup, 'db> ReturnSliceInferer<'summary, 'lookup, 'db> {
    fn new(
        db: &'db dyn MirDb,
        summary: &'summary RuntimeReturnSummary<'db>,
        params: &[RuntimeClass<'db>],
        lookup: &'lookup mut dyn FnMut(RuntimeInstanceKey<'db>) -> Option<RuntimeClass<'db>>,
    ) -> Self {
        let mut carriers = vec![RuntimeCarrier::Erased; summary.semantic_body.locals.len()];
        for (class, local) in params.iter().zip(summary.param_locals.iter().copied()) {
            carriers[local.index()] = RuntimeCarrier::Value(class.clone());
        }
        Self {
            db,
            summary,
            carriers,
            class_cache: InferClassCache::new(summary.semantic_body.locals.len()),
            pending_dependents: Vec::new(),
            lookup,
        }
    }

    fn run(mut self) -> Vec<RuntimeCarrier<'db>> {
        seed_root_provider_carriers(self.summary.env(self.db), &mut self.carriers);
        solve_sparse(&mut self, &mut ());
        self.carriers
    }

    fn set_carrier(&mut self, local: SLocalId, desired: RuntimeCarrier<'db>) -> bool {
        let current = self
            .carriers
            .get(local.index())
            .cloned()
            .unwrap_or(RuntimeCarrier::Erased);
        let desired = merge_runtime_carrier(
            self.db,
            &self.summary.semantic_body.locals[local.index()],
            current,
            desired,
        );
        if self.carriers[local.index()] == desired {
            return false;
        }
        self.carriers[local.index()] = desired;
        self.class_cache.note_carrier_changed(local);
        true
    }

    fn collect_local_change_dependents(&mut self, changed_local: SLocalId) {
        let mut pending = vec![changed_local];
        let mut seen = vec![false; self.summary.semantic_body.locals.len()];
        let mut queued = SecondaryMap::with_default(false);
        queued.resize(self.summary.slice_assignment_ids.len());
        self.pending_dependents.clear();
        while let Some(local) = pending.pop() {
            if std::mem::replace(&mut seen[local.index()], true) {
                continue;
            }
            self.class_cache.invalidate_local_dynamic_facts(local);
            for &assign_id in &self.summary.slice_assignments_by_local[local.index()] {
                let Some(slice_id) = self.summary.slice_assignment_positions[assign_id] else {
                    continue;
                };
                if !queued[slice_id] {
                    queued[slice_id] = true;
                    self.pending_dependents.push(slice_id);
                }
            }
            for dependent in self.summary.slice_dynamic_dependents_by_local[local.index()]
                .iter()
                .copied()
            {
                pending.push(dependent);
            }
        }
    }
}

impl<'summary, 'lookup, 'db> SparseAnalysis for ReturnSliceInferer<'summary, 'lookup, 'db> {
    type Node = SliceAssignmentId;
    type State = ();
    type Error = Infallible;

    fn node_count(&self) -> usize {
        self.summary.slice_assignment_ids.len()
    }

    fn seed_nodes(&self) -> Vec<Self::Node> {
        self.summary.slice_assignment_ids.keys().collect()
    }

    fn step(&mut self, node: Self::Node, _: &mut Self::State) -> Result<bool, Self::Error> {
        self.pending_dependents.clear();
        let assign_id = self.summary.slice_assignment_ids[node];
        let assign = self.summary.facts.assignment(assign_id).unwrap_or_else(|| {
            panic!("missing sliced assignment facts for statement {assign_id:?}")
        });
        let local = &self.summary.semantic_body.locals[assign.dst.index()];
        let stmt = &self.summary.semantic_body.blocks[assign.block_idx].stmts[assign.stmt_idx];
        let expr = match &stmt.kind {
            hir::analysis::semantic::NSStmtKind::Assign { expr, .. } => expr,
            hir::analysis::semantic::NSStmtKind::Store { .. } => {
                panic!(
                    "sliced assignment facts point to non-assignment statement: block={} stmt={}",
                    assign.block_idx, assign.stmt_idx
                )
            }
        };
        let env = self.summary.env(self.db);
        let class = env.expr_direct_class(
            &self.carriers,
            assign.block_idx,
            assign.stmt_idx,
            expr,
            Some(&mut self.class_cache),
            self.lookup,
        );
        let Some(class) = class else {
            return Ok(false);
        };
        let desired =
            desired_runtime_value_carrier(self.db, local, class, env.scope(), env.assumptions());
        if !self.set_carrier(assign.dst, desired) {
            return Ok(false);
        }
        self.collect_local_change_dependents(assign.dst);
        Ok(true)
    }

    fn dependents(&self, _node: Self::Node, out: &mut Vec<Self::Node>) {
        out.extend(self.pending_dependents.iter().copied());
    }
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::{
        analysis::{
            semantic::{
                SemanticLocalKind, get_or_build_semantic_instance, root_semantic_instance_key,
            },
            ty::ty_check::BodyOwner,
        },
        hir_def::TopLevelMod,
    };
    use url::Url;

    use crate::{
        build_runtime_package,
        runtime::{RExpr, RStmt, RTerminator, RuntimeCarrier, RuntimeClass, RuntimeExitBehavior},
    };

    use super::*;
    use crate::runtime::{
        lower::{infer::LocalStateInferer, interface::runtime_param_locals},
        package::runtime_instance_for_semantic,
    };

    fn semantic_instance_for_named_func<'db>(
        db: &'db DriverDataBase,
        top_mod: TopLevelMod<'db>,
        name: &str,
    ) -> SemanticInstance<'db> {
        let func = top_mod
            .all_funcs(db)
            .iter()
            .copied()
            .find(|func| {
                func.name(db)
                    .to_opt()
                    .is_some_and(|func_name| func_name.data(db) == name)
            })
            .unwrap_or_else(|| panic!("missing function `{name}`"));
        let key = root_semantic_instance_key(db, BodyOwner::Func(func))
            .unwrap_or_else(|err| panic!("failed to root semantic function instance: {err:?}"));
        get_or_build_semantic_instance(db, key)
    }

    fn legacy_return_class_for_key<'db>(
        db: &'db DriverDataBase,
        key: RuntimeInstanceKey<'db>,
    ) -> Option<RuntimeClass<'db>> {
        let semantic = key
            .semantic(db)
            .expect("legacy return-class inference only applies to semantic runtime instances");
        let summary = RuntimeReturnSummary::build(db, semantic);
        let env = summary.env(db);
        let inferred = LocalStateInferer::new(
            env,
            key.params(db),
            &runtime_param_locals(db, semantic, key.params(db)),
        )
        .run();
        let mut returned = Vec::new();
        for local in summary.return_locals.iter().copied() {
            let Some(selected) = selected_visible_return_for_local(
                env,
                local,
                &summary.return_plan,
                &inferred.carriers,
            ) else {
                return summary.default_return_class.clone();
            };
            returned.push(selected.class);
        }
        let Some(first) = returned.pop() else {
            return summary.default_return_class.clone();
        };
        if returned.iter().all(|class| class == &first) {
            Some(first)
        } else {
            summary.default_return_class.clone()
        }
    }

    fn assert_static_exact_return_matches_full_inference(source: &str, name: &str) {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(&format!("file:///{name}.fe")).unwrap();
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(source.to_string()));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, name);
        let key = runtime_instance_for_semantic(&db, semantic).key(&db);

        assert!(
            matches!(
                desired_runtime_return_plan(&db, semantic),
                RuntimeVisibleReturnPlan::Exact(_)
            ),
            "`{name}` should exercise the static exact return-class path"
        );
        assert!(
            matches!(
                static_runtime_return_decision(&db, semantic),
                StaticRuntimeReturnDecision::Known(Some(_))
            ),
            "`{name}` should use the semantic-level static return decision"
        );
        assert_eq!(
            runtime_return_class(&db, key),
            legacy_return_class_for_key(&db, key),
            "static exact return class should match full-body carrier inference"
        );
    }

    fn assert_runtime_exit_behavior(
        source: &str,
        case_name: &str,
        expected: &[(&str, RuntimeExitBehavior)],
    ) {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(&format!("file:///{case_name}.fe")).unwrap();
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(source.to_string()));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        for &(name, exit) in expected {
            let semantic = semantic_instance_for_named_func(&db, top_mod, name);
            let runtime = runtime_instance_for_semantic(&db, semantic);
            assert_eq!(runtime.exit_behavior(&db), exit, "`{name}` exit behavior");
        }
    }

    #[test]
    fn ordinary_unit_call_may_return_normally() {
        assert_runtime_exit_behavior(
            r#"
fn helper() {}

fn caller() {
    helper()
}
"#,
            "ordinary_unit_call_may_return_normally",
            &[
                ("helper", RuntimeExitBehavior::MayReturn),
                ("caller", RuntimeExitBehavior::MayReturn),
            ],
        );
    }

    #[test]
    fn panic_wrappers_are_known_never_returning() {
        assert_runtime_exit_behavior(
            r#"
fn fail() {
    core::panic()
}

fn fail_indirect() {
    fail()
}
"#,
            "panic_wrappers_are_known_never_returning",
            &[
                ("fail", RuntimeExitBehavior::NeverReturns),
                ("fail_indirect", RuntimeExitBehavior::NeverReturns),
            ],
        );
    }

    #[test]
    fn mixed_panic_branch_may_return_normally() {
        assert_runtime_exit_behavior(
            r#"
fn maybe(flag: bool) {
    if flag {
        core::panic()
    }
}
"#,
            "mixed_panic_branch_may_return_normally",
            &[("maybe", RuntimeExitBehavior::MayReturn)],
        );
    }

    #[test]
    fn semantic_nonreturning_wrapper_calls_lower_as_terminal_calls() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///semantic_nonreturning_wrapper_calls_lower_as_terminal_calls.fe")
                .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Pair {
    a: u256,
    b: u256,
}

extern {
    fn todo() -> !
}

fn fail() -> ! {
    core::panic()
}

fn fail_declared_u256() -> u256 {
    core::panic()
}

fn fail_declared_pair() -> Pair {
    core::panic()
}

fn caller_unit() {
    fail()
}

fn caller_u256_from_never() -> u256 {
    fail()
}

fn caller_u256_from_declared_u256() -> u256 {
    fail_declared_u256()
}

fn caller_pair_from_declared_pair() -> Pair {
    fail_declared_pair()
}

fn caller_u256_from_extern_never() -> u256 {
    todo()
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        for caller_name in [
            "caller_unit",
            "caller_u256_from_never",
            "caller_u256_from_declared_u256",
            "caller_pair_from_declared_pair",
            "caller_u256_from_extern_never",
        ] {
            let caller = semantic_instance_for_named_func(&db, top_mod, caller_name);
            let body = runtime_instance_for_semantic(&db, caller).body(&db);

            assert!(
                body.blocks
                    .iter()
                    .any(|block| matches!(block.terminator, RTerminator::TerminalCall { .. })),
                "`{caller_name}` should terminal-call its nonreturning callee:\n{body:#?}"
            );
            assert!(
                body.blocks.iter().all(|block| {
                    block.stmts.iter().all(|stmt| {
                        !matches!(
                            stmt,
                            RStmt::Assign {
                                expr: RExpr::Call { .. },
                                ..
                            }
                        )
                    })
                }),
                "`{caller_name}` should not lower its nonreturning callee as a normal call:\n{body:#?}"
            );
        }
    }

    #[test]
    fn exact_scalar_return_class_does_not_need_body_inference() {
        assert_static_exact_return_matches_full_inference(
            r#"
fn exact_scalar_return_class_does_not_need_body_inference() -> u256 {
    42
}
"#,
            "exact_scalar_return_class_does_not_need_body_inference",
        );
    }

    #[test]
    fn exact_aggregate_return_class_does_not_need_body_inference() {
        assert_static_exact_return_matches_full_inference(
            r#"
struct Pair {
    a: u256,
    b: u256,
}

fn exact_aggregate_return_class_does_not_need_body_inference() -> Pair {
    Pair { a: 1, b: 2 }
}
"#,
            "exact_aggregate_return_class_does_not_need_body_inference",
        );
    }

    #[test]
    fn unit_return_class_is_statically_known_absent() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///unit_return_class_is_statically_known_absent.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
fn helper() {}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "helper");
        let key = runtime_instance_for_semantic(&db, semantic).key(&db);

        assert_eq!(
            static_runtime_return_decision(&db, semantic),
            StaticRuntimeReturnDecision::Known(None)
        );
        assert_eq!(runtime_return_class(&db, key), None);
    }

    #[test]
    fn borrow_return_class_remains_dynamic() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse("file:///borrow_return_class_remains_dynamic.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Holder {
    value: u256,
}

impl Holder {
    fn value_mut(mut self) -> mut u256 {
        mut self.value
    }
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "value_mut");

        assert!(
            matches!(
                static_runtime_return_decision(&db, semantic),
                StaticRuntimeReturnDecision::Dynamic
            ),
            "borrow-derived returns depend on the returned source transport"
        );
    }

    #[test]
    fn return_slice_includes_all_definitions_of_returned_local() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///return_slice_includes_all_definitions_of_returned_local.fe")
                .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
fn choose(_ flag: bool) -> u256 {
    let mut x = 1
    if flag {
        x = 2
    }
    x
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "choose");
        let summary = RuntimeReturnSummary::build(&db, semantic);
        let return_local = *summary
            .return_locals
            .first()
            .expect("choose should return one local");
        let return_defs = summary
            .facts
            .assignments()
            .iter()
            .filter(|(_, assignment)| assignment.dst == return_local)
            .count();
        let sliced_return_defs = summary
            .slice_assignment_ids
            .iter()
            .filter(|&(_, &assign_id)| {
                summary
                    .facts
                    .assignment(assign_id)
                    .is_some_and(|assignment| assignment.dst == return_local)
            })
            .count();

        assert_eq!(
            return_defs, 2,
            "expected `choose` to define the returned local twice"
        );
        assert_eq!(
            sliced_return_defs, return_defs,
            "return slice should keep every definition of the returned local"
        );
    }

    #[test]
    fn provider_root_return_slice_matches_full_inference() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///provider_root_return_slice_matches_full_inference.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Pair {
    a: u256,
    b: u256,
}

fn id_ctx() -> Pair uses (ctx: Pair) {
    ctx
}

msg Msg {
    #[selector = 1]
    Go -> u256
}

pub contract C {
    mut ctx: Pair

    init() uses (mut ctx) {
        ctx = Pair { a: 1, b: 2 }
    }

    recv Msg {
        Go -> u256 uses (ctx) {
            let pair = with (ctx) { id_ctx() }
            pair.a + pair.b
        }
    }
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let package = build_runtime_package(&db, top_mod).expect("runtime package");
        let function = package
            .functions(&db)
            .iter()
            .copied()
            .find(|function| function.symbol(&db).contains("id_ctx"))
            .expect("missing specialized id_ctx runtime function");
        let key = function.instance(&db).key(&db);

        assert_eq!(
            runtime_return_class(&db, key),
            legacy_return_class_for_key(&db, key),
            "provider-root return slice should match full-body carrier inference:\ninstance={key:#?}"
        );
    }

    #[test]
    fn owned_aggregate_temporaries_match_full_inference_in_return_slices() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(
            "file:///owned_aggregate_temporaries_match_full_inference_in_return_slices.fe",
        )
        .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
fn first(_ arr: [u8; 4]) -> u8 {
    let local = arr
    local[0]
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "first");
        let instance = runtime_instance_for_semantic(&db, semantic);
        let key = instance.key(&db);
        let summary = RuntimeReturnSummary::build(&db, semantic);
        let env = summary.env(&db);

        let legacy = LocalStateInferer::new(
            env,
            key.params(&db),
            &runtime_param_locals(&db, semantic, key.params(&db)),
        )
        .run();
        let mut lookup_return_class = |key| runtime_return_class(&db, key);
        let sliced =
            ReturnSliceInferer::new(&db, &summary, key.params(&db), &mut lookup_return_class).run();
        let locals = summary
            .semantic_body
            .locals
            .iter()
            .enumerate()
            .filter_map(|(idx, local)| {
                (idx >= summary.param_locals.len()
                    && matches!(local.facts.interface, SemanticLocalKind::DirectValue)
                    && local.facts.root_demand.needs_projectable_owned_storage())
                .then_some(SLocalId::from_u32(idx as u32))
            })
            .collect::<Vec<_>>();
        assert_eq!(locals.len(), 1, "expected one owned aggregate temporary");
        let local = locals[0];

        assert_eq!(
            sliced[local.index()],
            legacy.carriers[local.index()],
            "return slice should infer the same owned aggregate temporary carrier as the full solver"
        );
        assert!(matches!(
            sliced[local.index()],
            RuntimeCarrier::Value(RuntimeClass::Ref { .. })
        ));
    }
}
