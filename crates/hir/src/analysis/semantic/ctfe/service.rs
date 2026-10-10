use cranelift_entity::EntityRef;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        EffectProviderSubst, ImplEnv, SConst, SExpr, SLocalId, SStmt, SStmtKind, STerminatorKind,
        SemConstId, SemConstScalar, SemConstValue, SemOrigin, SemanticBody, SemanticInstance,
        SemanticInstanceKey, SemanticLocalRole, array_const, bool_const,
        consts::{instantiate_const_template, retype_verified_sem_const},
        enum_const, execute_scalar_cast, execute_source_int_binary, execute_source_int_unary,
        get_or_build_semantic_instance, int_const, int_ty_shape, normalize_int_to_shape,
        sem_const_from_ty, sem_const_ty, struct_const, tuple_const,
    },
    ty::{
        const_expr::{ConstExpr, ConstExprId, ConstInvocation},
        const_ty::{
            ConstTyData, ConstTyId, const_ty_from_assoc_const_use,
            const_ty_from_inherent_const_use, const_ty_from_sem_const,
        },
        corelib::{
            PrimitiveWrapperCallKind, core_primitive_wrapper_call_kind, ctfe_extern_intrinsic_kind,
        },
        subst::instantiate_scoped_into,
        ty_check::BodyOwner,
        ty_def::{TyData, TyId},
    },
};
use crate::hir_def::scope_graph::ScopeId;
use crate::hir_def::{ArithBinOp, BinOp, CompBinOp, UnOp};

use super::{
    canonicalize::block_successors,
    machine::{
        CtfeConfig, CtfeError, execute_resolved_const_computation_with_steps, primitive_error,
        sem_const_dependency,
    },
    outcome::{
        BlockedInfo, ConstDemandKind, ConstDependency, EvalFailure, EvalOutcome, EvalResult,
        EvalStop,
    },
    request::{
        ConstComputationId, ConstDesc, ConstEntry, ConstRepr, TermCallFrame, TermProvenance,
        VerifiedConstValueId,
    },
};

pub fn const_computation_for_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    inputs: Vec<ConstDesc<'db>>,
) -> ConstComputationId<'db> {
    let owner = key.owner(db);
    ConstComputationId::new(
        db,
        ConstEntry::Resolved(key),
        inputs,
        key.typed_body(db).result_ty(),
        owner.scope(),
        SemOrigin::Body(owner),
    )
}

pub fn specialize_const_computation<'db>(
    db: &'db dyn HirAnalysisDb,
    computation: ConstComputationId<'db>,
    new_owner: ScopeId<'db>,
    args: &[TyId<'db>],
) -> Result<ConstComputationId<'db>, EvalFailure<'db>> {
    let from_owner = computation.parameter_owner(db);
    let origin = computation.origin(db);
    let fold_ty = |ty| instantiate_scoped_into(db, ty, from_owner, new_owner, args);
    let inputs = computation
        .inputs(db)
        .iter()
        .map(|input| specialize_const_description(db, input, from_owner, new_owner, args, origin))
        .collect::<Result<Vec<_>, _>>()?;

    let entry = match computation.entry(db) {
        ConstEntry::Resolved(key) => ConstEntry::Resolved(instantiate_scoped_into(
            db, key, from_owner, new_owner, args,
        )),
        ConstEntry::Associated(use_) => {
            let folded = instantiate_scoped_into(db, use_, from_owner, new_owner, args);
            let scope = if use_.origin_scope() == from_owner {
                new_owner
            } else {
                use_.origin_scope()
            };
            ConstEntry::Associated(folded.with_env(scope, folded.assumptions()))
        }
        ConstEntry::Inherent(use_) => {
            let folded = instantiate_scoped_into(db, use_, from_owner, new_owner, args);
            let scope = if use_.origin_scope() == from_owner {
                new_owner
            } else {
                use_.origin_scope()
            };
            ConstEntry::Inherent(folded.with_env(scope, folded.assumptions()))
        }
    };
    Ok(ConstComputationId::new(
        db,
        entry,
        inputs,
        fold_ty(computation.result_ty(db)),
        new_owner,
        origin,
    ))
}

/// Specialize only descriptions owned by the source binder. Descriptions
/// owned by another scope retain their existing parameter context.
pub fn specialize_const_description<'db>(
    db: &'db dyn HirAnalysisDb,
    description: &ConstDesc<'db>,
    from_owner: ScopeId<'db>,
    new_owner: ScopeId<'db>,
    args: &[TyId<'db>],
    origin: SemOrigin<'db>,
) -> Result<ConstDesc<'db>, EvalFailure<'db>> {
    if description.parameter_owner() != from_owner {
        return Ok(description.clone());
    }
    let fold_ty = |ty| instantiate_scoped_into(db, ty, from_owner, new_owner, args);
    let ty = fold_ty(description.ty());
    match description.repr() {
        ConstRepr::Value(value) => {
            let value = retype_verified_sem_const(db, value.value(), ty).ok_or_else(|| {
                EvalFailure::Invariant {
                    origin,
                    message: "verified input could not be specialized to its type".into(),
                }
            })?;
            let value =
                VerifiedConstValueId::from_complete_execution(db, value).map_err(|message| {
                    EvalFailure::Invariant {
                        origin,
                        message: message.into(),
                    }
                })?;
            Ok(ConstDesc::value(db, value, new_owner))
        }
        ConstRepr::Term(term) => {
            let wrapped = TyId::const_ty(db, *term);
            let folded = fold_ty(wrapped);
            let TyData::ConstTy(const_ty) = folded.data(db) else {
                return Err(EvalFailure::Invariant {
                    origin,
                    message: "term specialization changed its representation".into(),
                });
            };
            let specialized = if let Some(provenance) = description.term_provenance() {
                let provenance =
                    specialize_term_provenance(db, provenance, from_owner, new_owner, args)?;
                if provenance.term != *const_ty {
                    return Err(EvalFailure::Invariant {
                        origin,
                        message: "term provenance changed its specialized result".into(),
                    });
                }
                ConstDesc::term_with_provenance(db, new_owner, provenance)
            } else {
                ConstDesc::term(db, new_owner, *const_ty)
            };
            if specialized.ty() != ty {
                return Err(EvalFailure::Invariant {
                    origin,
                    message: "term specialization changed its result type".into(),
                });
            }
            Ok(specialized)
        }
        ConstRepr::Deferred(deferred) => {
            let specialized = specialize_const_computation(db, *deferred, new_owner, args)?;
            if specialized.result_ty(db) != ty {
                return Err(EvalFailure::Invariant {
                    origin,
                    message: "deferred input type changed during specialization".into(),
                });
            }
            Ok(ConstDesc::deferred(db, specialized))
        }
    }
}

fn specialize_term_provenance<'db>(
    db: &'db dyn HirAnalysisDb,
    provenance: &TermProvenance<'db>,
    from_owner: ScopeId<'db>,
    new_owner: ScopeId<'db>,
    args: &[TyId<'db>],
) -> Result<TermProvenance<'db>, EvalFailure<'db>> {
    let folded = instantiate_scoped_into(
        db,
        TyId::const_ty(db, provenance.term),
        from_owner,
        new_owner,
        args,
    );
    let TyData::ConstTy(term) = folded.data(db) else {
        return Err(EvalFailure::Invariant {
            origin: provenance.origin,
            message: "term provenance lost its constant representation".into(),
        });
    };
    Ok(TermProvenance {
        term: *term,
        origin: provenance.origin,
        operands: provenance
            .operands
            .iter()
            .map(|operand| specialize_term_provenance(db, operand, from_owner, new_owner, args))
            .collect::<Result<_, _>>()?,
        frames: provenance
            .frames
            .iter()
            .map(|frame| TermCallFrame {
                origin: frame.origin,
                callee: instantiate_scoped_into(db, frame.callee, from_owner, new_owner, args),
            })
            .collect(),
        operation_order: provenance.operation_order,
        opaque: provenance.opaque,
    })
}

/// A whole-expression term can be described without attempting concrete
/// execution. If the term cannot be proven to need an unresolved fact, use the
/// strict machine so closed faults retain their precise operation provenance.
pub fn describe_const_computation<'db>(
    db: &'db dyn HirAnalysisDb,
    computation: ConstComputationId<'db>,
    config: CtfeConfig,
) -> EvalOutcome<'db, ConstDesc<'db>> {
    let mut cx = ForceContext::new(config.clone());
    let extracted = extract_whole_const_term(db, computation, &config);
    if let Some(provenance) = extracted.as_ref()
        && let ConstTyData::Abstract(expr, _) = provenance.term.data(db)
        && let ConstExpr::Invocation(invocation) = expr.data(db)
        && let BodyOwner::Func(func) = invocation.key.owner(db)
        && func.is_extern(db)
        && func.body(db).is_none()
        && ctfe_extern_intrinsic_kind(db, func).is_none()
    {
        for (index, arg) in invocation.args.iter().copied().enumerate() {
            let child = match term_child_provenance(
                db,
                Some(provenance),
                arg,
                index,
                computation.origin(db),
            ) {
                Ok(child) => child,
                Err(stop) => return EvalOutcome::from(Err(stop)),
            };
            match force_const_term_operand(db, arg, &mut cx, computation.origin(db), child) {
                Ok(_) => {}
                Err(EvalStop::Blocked(_)) => break,
                Err(EvalStop::Failed(failure)) => return EvalOutcome::Failed(failure),
            }
        }
        return EvalOutcome::Ready(ConstDesc::term_with_provenance(
            db,
            computation.parameter_owner(db),
            provenance.clone(),
        ));
    }
    if let Some(provenance) = extracted
        && matches!(
            force_const_term_operand(
                db,
                TyId::const_ty(db, provenance.term),
                &mut cx,
                computation.origin(db),
                Some(&provenance),
            ),
            Err(EvalStop::Blocked(_))
        )
    {
        return EvalOutcome::Ready(ConstDesc::term_with_provenance(
            db,
            computation.parameter_owner(db),
            provenance,
        ));
    }
    match force_const_computation(db, computation, config) {
        EvalOutcome::Ready(value) => {
            EvalOutcome::Ready(ConstDesc::value(db, value, computation.parameter_owner(db)))
        }
        EvalOutcome::Blocked(_) => EvalOutcome::Ready(ConstDesc::deferred(db, computation)),
        EvalOutcome::Failed(failure) => EvalOutcome::Failed(failure),
    }
}

/// Accept only an expression graph in which every assignment contributes to
/// the result, and whose branches rejoin. This rules out discarded bounds
/// checks, calls, stores, and other obligations that a return-only slice
/// would erase.
fn extract_whole_const_term<'db>(
    db: &'db dyn HirAnalysisDb,
    computation: ConstComputationId<'db>,
    config: &CtfeConfig,
) -> Option<TermProvenance<'db>> {
    let ConstEntry::Resolved(key) = computation.entry(db) else {
        return None;
    };
    if !computation.inputs(db).is_empty() {
        return None;
    }
    let mut extraction = TermExtraction {
        next_order: 0,
        remaining: config.step_limit.min(256),
        depth_limit: config.recursion_limit.min(64),
    };
    let term = extract_pure_body_term(db, key, &[], &[], &[], &mut extraction)?;
    let mut observed = 0;
    (term.term.ty(db) == computation.result_ty(db)
        && term.preserves_source_order(&mut observed)
        && observed == extraction.next_order)
        .then_some(term)
}

// Extraction is an optional description optimization. A bounded attempt falls
// back to the original computation; only the executor reports resource faults.
struct TermExtraction {
    next_order: usize,
    remaining: usize,
    depth_limit: usize,
}

fn operation_provenance<'db>(
    term: ConstTyId<'db>,
    origin: SemOrigin<'db>,
    operands: Vec<TermProvenance<'db>>,
    frames: &[TermCallFrame<'db>],
    extraction: &mut TermExtraction,
) -> TermProvenance<'db> {
    let operation_order = extraction.next_order;
    extraction.next_order += 1;
    TermProvenance {
        term,
        origin,
        operands,
        frames: frames.to_vec(),
        operation_order,
        opaque: false,
    }
}

fn extract_pure_body_term<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    inputs: &[TermProvenance<'db>],
    stack: &[BodyOwner<'db>],
    frames: &[TermCallFrame<'db>],
    extraction: &mut TermExtraction,
) -> Option<TermProvenance<'db>> {
    // Execution rejects a non-const function, so a term must not erase it.
    if stack.len() >= extraction.depth_limit
        || stack.contains(&key.owner(db))
        || matches!(key.owner(db), BodyOwner::Func(func) if !func.is_const(db))
    {
        return None;
    }
    extraction.remaining = extraction.remaining.checked_sub(1)?;
    let mut stack = stack.to_vec();
    stack.push(key.owner(db));
    let body = get_or_build_semantic_instance(db, key)
        .admitted_body(db)
        .ok()?;
    if body.entry_locals.len() != inputs.len() || body.blocks.is_empty() {
        return None;
    }
    // The walk charges a step for each block it reaches, so a body that
    // reaches more blocks than remain cannot be described. Counting them
    // within that bound also bounds the branch-join analysis, which spans
    // only the reachable blocks.
    let mut reachable = vec![false; body.blocks.len()];
    let mut pending = vec![0];
    let mut count = 0;
    while let Some(block) = pending.pop() {
        if std::mem::replace(reachable.get_mut(block)?, true) {
            continue;
        }
        count += 1;
        if count > extraction.remaining {
            return None;
        }
        pending.extend(
            block_successors(&body.blocks[block].terminator.kind)
                .into_iter()
                .map(|block| block.index()),
        );
    }
    let mut state = BodyTerms {
        terms: vec![None; body.locals.len()],
        predecessors: vec![Vec::new(); body.locals.len()],
        assigned: vec![false; body.locals.len()],
    };
    for (local, term) in body.entry_locals.iter().zip(inputs) {
        let slot = body.locals.get(local.index())?;
        let compatible = match slot.role {
            SemanticLocalRole::DirectValue { .. } => slot.ty == term.term.ty(db),
            SemanticLocalRole::PlaceCarrier {
                provider: None,
                value_ty,
            } => value_ty == term.term.ty(db),
            _ => false,
        };
        if !compatible || state.assigned[local.index()] {
            return None;
        }
        state.terms[local.index()] = Some(term.clone());
        state.assigned[local.index()] = true;
    }
    let mut walk = BodyTermWalk {
        db,
        key,
        body,
        stack,
        frames,
        extraction,
        reachable,
        post_dominators: None,
    };
    let BodyTermExit::Return(result) = walk.walk(0, None, &mut state)? else {
        return None;
    };
    let mut used = vec![false; state.terms.len()];
    let mut pending = vec![result];
    while let Some(local) = pending.pop() {
        let index = local.index();
        if !state.assigned.get(index).copied()? {
            return None;
        }
        if !used[index] {
            used[index] = true;
            pending.extend(state.predecessors[index].iter().copied());
        }
    }
    if state
        .assigned
        .iter()
        .zip(&used)
        .any(|(assigned, used)| assigned != used)
    {
        return None;
    }
    let term = state.terms.get(result.index()).cloned().flatten()?;
    (term.term.ty(db) == key.typed_body(db).result_ty()).then_some(term)
}

/// The terms a path through a body has defined so far.
#[derive(Clone)]
struct BodyTerms<'db> {
    terms: Vec<Option<TermProvenance<'db>>>,
    predecessors: Vec<Vec<SLocalId>>,
    assigned: Vec<bool>,
}

enum BodyTermExit {
    Join,
    Return(SLocalId),
}

struct BodyTermWalk<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    body: &'db SemanticBody<'db>,
    stack: Vec<BodyOwner<'db>>,
    frames: &'a [TermCallFrame<'db>],
    extraction: &'a mut TermExtraction,
    reachable: Vec<bool>,
    post_dominators: Option<Vec<FxHashSet<usize>>>,
}

impl<'db> BodyTermWalk<'_, 'db> {
    /// Extracts the blocks from `current` until control reaches `join`, or the
    /// body returns when there is none. A branch becomes a select term over
    /// its arms, so only the arm execution takes is ever forced.
    fn walk(
        &mut self,
        mut current: usize,
        join: Option<usize>,
        state: &mut BodyTerms<'db>,
    ) -> Option<BodyTermExit> {
        loop {
            self.extraction.remaining = self.extraction.remaining.checked_sub(1)?;
            let block = self.body.blocks.get(current)?;
            for stmt in &block.stmts {
                self.assign(stmt, state)?;
            }
            match &block.terminator.kind {
                STerminatorKind::Goto(next) if Some(next.index()) == join => {
                    return Some(BodyTermExit::Join);
                }
                STerminatorKind::Goto(next) => current = next.index(),
                STerminatorKind::Return(Some(result)) if join.is_none() => {
                    return Some(BodyTermExit::Return(result.value));
                }
                STerminatorKind::Branch {
                    cond,
                    then_bb,
                    else_bb,
                } => {
                    let merge = self.join_of(current)?;
                    let cond_term = state.terms.get_mut(cond.value.index())?.take()?;
                    let mut arms = Vec::with_capacity(2);
                    for entry in [then_bb, else_bb] {
                        let mut arm = state.clone();
                        let BodyTermExit::Join = self.walk(entry.index(), Some(merge), &mut arm)?
                        else {
                            return None;
                        };
                        arms.push(arm);
                    }
                    let [then_arm, else_arm] = <[_; 2]>::try_from(arms).ok()?;
                    self.select(
                        cond.value,
                        cond_term,
                        block.terminator.origin,
                        then_arm,
                        else_arm,
                        state,
                    )?;
                    if Some(merge) == join {
                        return Some(BodyTermExit::Join);
                    }
                    current = merge;
                }
                STerminatorKind::Return(_)
                | STerminatorKind::MatchEnum { .. }
                | STerminatorKind::Assert { .. } => return None,
            }
        }
    }

    /// Where both arms of the branch at `block` meet again: its immediate
    /// post-dominator.
    fn join_of(&mut self, block: usize) -> Option<usize> {
        let post_dominators = self.post_dominators.get_or_insert_with(|| {
            // Unreachable blocks keep no successors or post-dominators; no
            // reachable block leads to them.
            let successors = self
                .body
                .blocks
                .iter()
                .zip(&self.reachable)
                .map(|(block, &reachable)| {
                    block_successors(&block.terminator.kind)
                        .into_iter()
                        .filter(|_| reachable)
                        .map(|block| block.index())
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();
            let all = (0..successors.len())
                .filter(|&block| self.reachable[block])
                .collect::<FxHashSet<_>>();
            let mut post_dominators = successors
                .iter()
                .enumerate()
                .map(|(block, successors)| {
                    if !self.reachable[block] {
                        FxHashSet::default()
                    } else if successors.is_empty() {
                        FxHashSet::from_iter([block])
                    } else {
                        all.clone()
                    }
                })
                .collect::<Vec<_>>();
            let mut changed = true;
            while changed {
                changed = false;
                for (block, targets) in successors.iter().enumerate().rev() {
                    let Some((first, rest)) = targets.split_first() else {
                        continue;
                    };
                    let mut next = post_dominators[*first].clone();
                    for target in rest {
                        next.retain(|post| post_dominators[*target].contains(post));
                    }
                    next.insert(block);
                    changed |= next != post_dominators[block];
                    post_dominators[block] = next;
                }
            }
            post_dominators
        });
        post_dominators[block]
            .iter()
            .copied()
            .filter(|post| *post != block)
            .max_by_key(|post| post_dominators[*post].len())
    }

    /// Joins the arms of a branch into the state before it. Each local both
    /// arms define for the join becomes a select over the condition. An arm
    /// must not consume a term defined before the branch, which execution
    /// evaluates whichever arm it takes.
    fn select(
        &mut self,
        cond: SLocalId,
        cond_term: TermProvenance<'db>,
        origin: SemOrigin<'db>,
        then_arm: BodyTerms<'db>,
        else_arm: BodyTerms<'db>,
        state: &mut BodyTerms<'db>,
    ) -> Option<()> {
        let mut cond_term = Some(cond_term);
        for index in 0..state.terms.len() {
            if state.assigned[index] {
                if state.terms[index].is_some()
                    && (then_arm.terms[index].is_none() || else_arm.terms[index].is_none())
                {
                    return None;
                }
                continue;
            }
            let mut predecessors = then_arm.predecessors[index].clone();
            predecessors.extend(else_arm.predecessors[index].iter().copied());
            match (&then_arm.terms[index], &else_arm.terms[index]) {
                (Some(then_term), Some(else_term)) => {
                    let ty = self.body.locals[index].ty;
                    if then_term.term.ty(self.db) != ty || else_term.term.ty(self.db) != ty {
                        return None;
                    }
                    let cond_term = cond_term.take()?;
                    let term = ConstTyId::new(
                        self.db,
                        ConstTyData::Abstract(
                            ConstExprId::new(
                                self.db,
                                ConstExpr::Select {
                                    cond: TyId::const_ty(self.db, cond_term.term),
                                    then: TyId::const_ty(self.db, then_term.term),
                                    otherwise: TyId::const_ty(self.db, else_term.term),
                                },
                            ),
                            ty,
                        ),
                    );
                    state.terms[index] = Some(operation_provenance(
                        term,
                        origin,
                        vec![cond_term, then_term.clone(), else_term.clone()],
                        self.frames,
                        self.extraction,
                    ));
                    predecessors.push(cond);
                }
                (None, None) => {}
                // One arm defines a value the join never sees.
                (Some(_), None) | (None, Some(_)) => return None,
            }
            state.assigned[index] = then_arm.assigned[index] || else_arm.assigned[index];
            state.predecessors[index] = predecessors;
        }
        Some(())
    }

    /// Extracts one assignment, moving each operand's term into its user.
    fn assign(&mut self, stmt: &SStmt<'db>, state: &mut BodyTerms<'db>) -> Option<()> {
        let db = self.db;
        let key = self.key;
        self.extraction.remaining = self.extraction.remaining.checked_sub(1)?;
        let SStmtKind::Assign { dst, expr } = &stmt.kind else {
            return None;
        };
        let index = dst.index();
        let local = self.body.locals.get(index)?;
        if state.assigned[index] || !matches!(local.role, SemanticLocalRole::DirectValue { .. }) {
            return None;
        }
        // A term tree cannot duplicate a source operation. Moving each
        // operand also avoids exponential cloning before the order check.
        let terms = &mut state.terms;
        let mut read = |local: SLocalId| terms.get_mut(local.index()).and_then(Option::take);
        // Terms compare integers and booleans, as primitive comparisons do.
        let comparable = |term: ConstTyId<'db>| {
            let ty = term.ty(db);
            (int_ty_shape(db, ty).is_some() || ty.is_bool(db)).then_some(())
        };
        let (term, deps) = match expr {
            SExpr::Const(
                constant @ (SConst::Value(..) | SConst::Description(..) | SConst::Evidence(..)),
            ) => {
                let value = match constant {
                    SConst::Value(value) => value.value(),
                    SConst::Description(value) | SConst::Evidence(value) => *value,
                    _ => unreachable!(),
                };
                let term = const_ty_from_sem_const(db, value);
                // Evidence deliberately retains a declaration's formal
                // parameter for runtime ABI selection. Translate only this
                // explicit template; other operands are already in context.
                let term = if matches!(constant, SConst::Evidence(_)) {
                    instantiate_const_template(db, get_or_build_semantic_instance(db, key), term)
                } else {
                    term
                };
                let mut provenance = operation_provenance(
                    term,
                    stmt.origin,
                    Vec::new(),
                    self.frames,
                    self.extraction,
                );
                provenance.opaque = true;
                (provenance, Vec::new())
            }
            SExpr::Const(SConst::Ref(reference))
                if matches!(reference.instance(db).owner(db), BodyOwner::Const(..)) =>
            {
                let mut nested_frames = self.frames.to_vec();
                nested_frames.push(TermCallFrame {
                    origin: reference.origin(db),
                    callee: reference.instance(db),
                });
                (
                    extract_pure_body_term(
                        db,
                        reference.instance(db),
                        &[],
                        &self.stack,
                        &nested_frames,
                        self.extraction,
                    )?,
                    Vec::new(),
                )
            }
            SExpr::Forward(operand) | SExpr::UseValue(operand) => {
                (read(operand.value)?, vec![operand.value])
            }
            SExpr::Cast { value, .. } if int_ty_shape(db, local.ty).is_some() => {
                let operand = read(value.value)?;
                int_ty_shape(db, operand.term.ty(db))?;
                let term = ConstTyId::new(
                    db,
                    ConstTyData::Abstract(
                        ConstExprId::new(
                            db,
                            ConstExpr::Cast {
                                expr: TyId::const_ty(db, operand.term),
                                to: local.ty,
                            },
                        ),
                        local.ty,
                    ),
                );
                (
                    operation_provenance(
                        term,
                        stmt.origin,
                        vec![operand],
                        self.frames,
                        self.extraction,
                    ),
                    vec![value.value],
                )
            }
            SExpr::Unary { op, value } if matches!(op, UnOp::Plus | UnOp::Minus | UnOp::Not) => {
                let operand = read(value.value)?;
                let term = ConstTyId::new(
                    db,
                    ConstTyData::Abstract(
                        ConstExprId::new(
                            db,
                            ConstExpr::UnOp {
                                op: *op,
                                mode: self.body.template_owner.arithmetic_mode(db),
                                expr: TyId::const_ty(db, operand.term),
                            },
                        ),
                        local.ty,
                    ),
                );
                (
                    operation_provenance(
                        term,
                        stmt.origin,
                        vec![operand],
                        self.frames,
                        self.extraction,
                    ),
                    vec![value.value],
                )
            }
            SExpr::Binary {
                op: BinOp::Arith(op),
                lhs,
                rhs,
            } if matches!(
                op,
                ArithBinOp::Add
                    | ArithBinOp::Sub
                    | ArithBinOp::Mul
                    | ArithBinOp::Div
                    | ArithBinOp::Rem
                    | ArithBinOp::Pow
            ) =>
            {
                let lhs_term = read(lhs.value)?;
                let rhs_term = read(rhs.value)?;
                let term = ConstTyId::new(
                    db,
                    ConstTyData::Abstract(
                        ConstExprId::new(
                            db,
                            ConstExpr::ArithBinOp {
                                op: *op,
                                mode: self.body.template_owner.arithmetic_mode(db),
                                lhs: TyId::const_ty(db, lhs_term.term),
                                rhs: TyId::const_ty(db, rhs_term.term),
                            },
                        ),
                        local.ty,
                    ),
                );
                (
                    operation_provenance(
                        term,
                        stmt.origin,
                        vec![lhs_term, rhs_term],
                        self.frames,
                        self.extraction,
                    ),
                    vec![lhs.value, rhs.value],
                )
            }
            SExpr::Binary {
                op: BinOp::Comp(op),
                lhs,
                rhs,
            } => {
                let lhs_term = read(lhs.value)?;
                let rhs_term = read(rhs.value)?;
                comparable(lhs_term.term)?;
                let term = ConstTyId::new(
                    db,
                    ConstTyData::Abstract(
                        ConstExprId::new(
                            db,
                            ConstExpr::Compare {
                                op: *op,
                                lhs: TyId::const_ty(db, lhs_term.term),
                                rhs: TyId::const_ty(db, rhs_term.term),
                            },
                        ),
                        local.ty,
                    ),
                );
                (
                    operation_provenance(
                        term,
                        stmt.origin,
                        vec![lhs_term, rhs_term],
                        self.frames,
                        self.extraction,
                    ),
                    vec![lhs.value, rhs.value],
                )
            }
            SExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } if effect_args.is_empty() => {
                let BodyOwner::Func(func) = callee.key.owner(db) else {
                    return None;
                };
                let operands = args
                    .iter()
                    .map(|arg| read(arg.value))
                    .collect::<Option<Vec<_>>>()?;
                let term = if let Some(kind) = core_primitive_wrapper_call_kind(db, func, local.ty)
                {
                    let expr = match (kind, operands.as_slice()) {
                        (
                            PrimitiveWrapperCallKind::Unary(
                                op @ (UnOp::Plus | UnOp::Minus | UnOp::Not),
                            ),
                            [value],
                        ) => ConstExpr::UnOp {
                            op,
                            mode: self.body.template_owner.arithmetic_mode(db),
                            expr: TyId::const_ty(db, value.term),
                        },
                        (
                            PrimitiveWrapperCallKind::Binary(BinOp::Arith(
                                op @ (ArithBinOp::Add
                                | ArithBinOp::Sub
                                | ArithBinOp::Mul
                                | ArithBinOp::Div
                                | ArithBinOp::Rem
                                | ArithBinOp::Pow),
                            )),
                            [lhs, rhs],
                        ) => ConstExpr::ArithBinOp {
                            op,
                            mode: self.body.template_owner.arithmetic_mode(db),
                            lhs: TyId::const_ty(db, lhs.term),
                            rhs: TyId::const_ty(db, rhs.term),
                        },
                        (PrimitiveWrapperCallKind::Binary(BinOp::Comp(op)), [lhs, rhs])
                            if comparable(lhs.term).is_some() =>
                        {
                            ConstExpr::Compare {
                                op,
                                lhs: TyId::const_ty(db, lhs.term),
                                rhs: TyId::const_ty(db, rhs.term),
                            }
                        }
                        _ => return None,
                    };
                    let term = ConstTyId::new(
                        db,
                        ConstTyData::Abstract(ConstExprId::new(db, expr), local.ty),
                    );
                    operation_provenance(
                        term,
                        stmt.origin,
                        operands.clone(),
                        self.frames,
                        self.extraction,
                    )
                } else {
                    let mut nested_frames = self.frames.to_vec();
                    nested_frames.push(TermCallFrame {
                        origin: stmt.origin,
                        callee: callee.key,
                    });
                    let saved_order = self.extraction.next_order;
                    let inlined = if !func.is_extern(db)
                        && !func.is_associated_func(db)
                        && callee.key.effect_providers(db).providers(db).is_empty()
                    {
                        extract_pure_body_term(
                            db,
                            callee.key,
                            &operands,
                            &self.stack,
                            &nested_frames,
                            self.extraction,
                        )
                    } else {
                        None
                    };
                    if let Some(term) = inlined {
                        term
                    } else if !func.is_associated_func(db)
                        && callee.key.effect_providers(db).providers(db).is_empty()
                    {
                        self.extraction.next_order = saved_order;
                        let term = ConstTyId::new(
                            db,
                            ConstTyData::Abstract(
                                ConstExprId::new(
                                    db,
                                    ConstExpr::Invocation(ConstInvocation::new(
                                        db,
                                        callee.key,
                                        operands
                                            .iter()
                                            .map(|operand| TyId::const_ty(db, operand.term))
                                            .collect(),
                                    )),
                                ),
                                local.ty,
                            ),
                        );
                        operation_provenance(
                            term,
                            stmt.origin,
                            operands,
                            self.frames,
                            self.extraction,
                        )
                    } else {
                        extract_pure_body_term(
                            db,
                            callee.key,
                            &operands,
                            &self.stack,
                            &nested_frames,
                            self.extraction,
                        )?
                    }
                };
                (term, args.iter().map(|arg| arg.value).collect())
            }
            _ => return None,
        };
        if term.term.ty(db) != local.ty {
            return None;
        }
        state.terms[index] = Some(term);
        state.predecessors[index] = deps;
        state.assigned[index] = true;
        Some(())
    }
}

/// A forcing attempt shares one budget across term operands and body calls.
/// Const-item queries inside the machine retain the established query policy.
struct ForceContext {
    config: CtfeConfig,
    steps: usize,
    depth: usize,
}

impl ForceContext {
    fn new(config: CtfeConfig) -> Self {
        Self {
            config,
            steps: 0,
            depth: 0,
        }
    }

    fn charge<'db>(&mut self, count: usize, origin: SemOrigin<'db>) -> EvalResult<'db, ()> {
        if count > self.config.step_limit.saturating_sub(self.steps) {
            return Err(CtfeError::StepLimitExceeded { origin }.into());
        }
        self.steps += count;
        Ok(())
    }

    fn force_computation<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        computation: ConstComputationId<'db>,
    ) -> EvalOutcome<'db, VerifiedConstValueId<'db>> {
        let config = CtfeConfig {
            step_limit: self.config.step_limit.saturating_sub(self.steps),
            recursion_limit: self.config.recursion_limit.saturating_sub(self.depth),
        };
        let (outcome, steps) = force_const_computation_with_cost(db, computation, config);
        self.steps = self.steps.saturating_add(steps);
        outcome
    }

    fn execute_body<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        key: SemanticInstanceKey<'db>,
        args: Vec<SemConstId<'db>>,
        origin: SemOrigin<'db>,
    ) -> EvalOutcome<'db, SemConstId<'db>> {
        let config = CtfeConfig {
            recursion_limit: self.config.recursion_limit.saturating_sub(self.depth),
            ..self.config.clone()
        };
        execute_resolved_const_computation_with_steps(
            db,
            key,
            args,
            config,
            origin,
            &mut self.steps,
        )
    }
}

fn force_const_term_operand<'db>(
    db: &'db dyn HirAnalysisDb,
    operand: TyId<'db>,
    cx: &mut ForceContext,
    origin: SemOrigin<'db>,
    provenance: Option<&TermProvenance<'db>>,
) -> EvalResult<'db, SemConstId<'db>> {
    let origin = provenance.map_or(origin, |provenance| provenance.origin);
    if cx.depth >= cx.config.recursion_limit {
        return Err(CtfeError::RecursionLimitExceeded { origin }.into());
    }
    cx.charge(1, origin)?;
    cx.depth += 1;
    let result = force_const_term_operand_impl(db, operand, cx, origin, provenance);
    cx.depth -= 1;
    let value = result?;
    VerifiedConstValueId::from_complete_execution(db, value).map_err(|message| {
        EvalStop::Failed(EvalFailure::Invariant {
            origin,
            message: message.into(),
        })
    })?;
    let TyData::ConstTy(term) = operand.data(db) else {
        unreachable!()
    };
    if sem_const_ty(db, value) != term.ty(db) {
        return Err(EvalStop::Failed(EvalFailure::Invariant {
            origin,
            message: "constant operand value differs from its declared type".into(),
        }));
    }
    Ok(value)
}

fn force_const_term_operand_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    operand: TyId<'db>,
    cx: &mut ForceContext,
    origin: SemOrigin<'db>,
    provenance: Option<&TermProvenance<'db>>,
) -> EvalResult<'db, SemConstId<'db>> {
    let TyData::ConstTy(const_ty) = operand.data(db) else {
        return Err(CtfeError::InvalidOperation {
            origin,
            message: "constant term operand is not a constant value".into(),
        }
        .into());
    };
    if let Some(provenance) = provenance
        && provenance.term != *const_ty
    {
        return Err(EvalStop::Failed(EvalFailure::Invariant {
            origin,
            message: "term occurrence differs from its canonical operand".into(),
        }));
    }
    let origin = provenance.map_or(origin, |provenance| provenance.origin);
    match const_ty.data(db) {
        ConstTyData::Computation {
            description,
            source,
        } => {
            match force_const_description_in_context(
                db,
                description,
                cx,
                SemOrigin::Body(source.owner(db)),
            ) {
                EvalOutcome::Ready(value) => Ok(value.value()),
                EvalOutcome::Blocked(info) => Err(EvalStop::Blocked(info)),
                EvalOutcome::Failed(EvalFailure::Ctfe(error)) => Err(CtfeError::CalleeError {
                    origin,
                    callee: SemanticInstance::new(db, *source),
                    source: Box::new(error),
                }
                .into()),
                EvalOutcome::Failed(failure) => Err(EvalStop::Failed(failure)),
            }
        }
        ConstTyData::Description(value) => force_immutable_description(db, *value, cx, origin),
        ConstTyData::Value(..) => sem_const_from_ty(db, operand).ok_or_else(|| {
            CtfeError::InvalidOperation {
                origin,
                message: "constant term operand has an invalid value".into(),
            }
            .into()
        }),
        ConstTyData::Abstract(term, ty) => match term.data(db) {
            ConstExpr::TraitConst(use_) => {
                let request = ConstComputationId::new(
                    db,
                    ConstEntry::Associated(*use_),
                    Vec::new(),
                    *ty,
                    use_.origin_scope(),
                    origin,
                );
                match cx.force_computation(db, request) {
                    EvalOutcome::Ready(value) => Ok(value.value()),
                    EvalOutcome::Blocked(info) => Err(EvalStop::Blocked(info)),
                    EvalOutcome::Failed(failure) => Err(EvalStop::Failed(failure)),
                }
            }
            ConstExpr::InherentConst(use_) => {
                let request = ConstComputationId::new(
                    db,
                    ConstEntry::Inherent(*use_),
                    Vec::new(),
                    *ty,
                    use_.origin_scope(),
                    origin,
                );
                match cx.force_computation(db, request) {
                    EvalOutcome::Ready(value) => Ok(value.value()),
                    EvalOutcome::Blocked(info) => Err(EvalStop::Blocked(info)),
                    EvalOutcome::Failed(failure) => Err(EvalStop::Failed(failure)),
                }
            }
            _ => force_const_term(db, *term, *ty, cx, origin, provenance),
        },
        ConstTyData::TyParam(..) | ConstTyData::TyVar(..) | ConstTyData::Hole(..) => {
            Err(EvalStop::Blocked(BlockedInfo::new(
                ConstDemandKind::Value,
                ConstDependency::Value(operand),
                origin,
            )))
        }
        ConstTyData::UnEvaluated {
            body,
            ty: Some(expected),
            template_ty,
            capture,
            ..
        } => {
            let owner = BodyOwner::AnonConstBody {
                body: *body,
                expected: template_ty.unwrap_or(*expected),
            };
            let key = SemanticInstanceKey::new(
                db,
                owner,
                capture.generic_subst(db),
                EffectProviderSubst::empty(db),
                ImplEnv::empty(db, owner.scope()),
            );
            let request = const_computation_for_instance(db, key, Vec::new());
            cx.force_computation(db, request)
                .into_result()
                .map(|value| value.value())
                .map_err(|stop| match stop {
                    EvalStop::Failed(EvalFailure::Ctfe(error)) => CtfeError::CalleeError {
                        origin,
                        callee: SemanticInstance::new(db, key),
                        source: Box::new(error),
                    }
                    .into(),
                    stop => stop,
                })
        }
        ConstTyData::UnEvaluated { ty: None, .. } => Err(CtfeError::InvalidBody { origin }.into()),
        ConstTyData::Invalid(..) => Err(CtfeError::InvalidOperation {
            origin,
            message: "constant term operand has an invalid value".into(),
        }
        .into()),
    }
}

fn force_immutable_description<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
    cx: &mut ForceContext,
    origin: SemOrigin<'db>,
) -> EvalResult<'db, SemConstId<'db>> {
    let fields = match value.value(db) {
        SemConstValue::Description(term) => {
            return force_const_term_operand(db, TyId::const_ty(db, *term), cx, origin, None);
        }
        SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => elems,
        SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => fields,
        SemConstValue::Unit | SemConstValue::Scalar { .. } => return Ok(value),
    };
    let fields = fields
        .iter()
        .copied()
        .map(|field| {
            force_const_term_operand(
                db,
                TyId::const_ty(db, ConstTyId::new(db, ConstTyData::Description(field))),
                cx,
                origin,
                None,
            )
        })
        .collect::<EvalResult<'db, Vec<_>>>()?
        .into_boxed_slice();
    Ok(match value.value(db) {
        SemConstValue::Tuple { ty, .. } => tuple_const(db, *ty, fields),
        SemConstValue::Array { ty, .. } => array_const(db, *ty, fields),
        SemConstValue::Struct { ty, .. } => struct_const(db, *ty, fields),
        SemConstValue::Enum { ty, variant, .. } => enum_const(db, *ty, *variant, fields),
        _ => unreachable!("only aggregates reach recursive description forcing"),
    })
}

fn term_child_provenance<'a, 'db>(
    db: &'db dyn HirAnalysisDb,
    provenance: Option<&'a TermProvenance<'db>>,
    operand: TyId<'db>,
    index: usize,
    origin: SemOrigin<'db>,
) -> EvalResult<'db, Option<&'a TermProvenance<'db>>> {
    let Some(provenance) = provenance else {
        return Ok(None);
    };
    if provenance.opaque {
        return Ok(None);
    }
    let Some(child) = provenance.operands.get(index) else {
        return Err(EvalStop::Failed(EvalFailure::Invariant {
            origin,
            message: "term occurrence is missing an operand origin".into(),
        }));
    };
    if !matches!(operand.data(db), TyData::ConstTy(term) if child.term == *term) {
        return Err(EvalStop::Failed(EvalFailure::Invariant {
            origin,
            message: "term operand differs from its recorded occurrence".into(),
        }));
    }
    Ok(Some(child))
}

/// Force a description without rebuilding its original request from a
/// post-execution machine state. Pure terms and deferred bodies use their
/// recorded binder and environment; verified values are already complete.
pub fn force_const_description<'db>(
    db: &'db dyn HirAnalysisDb,
    description: &ConstDesc<'db>,
    config: CtfeConfig,
    origin: SemOrigin<'db>,
) -> EvalOutcome<'db, VerifiedConstValueId<'db>> {
    force_const_description_in_context(db, description, &mut ForceContext::new(config), origin)
}

fn force_const_description_in_context<'db>(
    db: &'db dyn HirAnalysisDb,
    description: &ConstDesc<'db>,
    cx: &mut ForceContext,
    origin: SemOrigin<'db>,
) -> EvalOutcome<'db, VerifiedConstValueId<'db>> {
    let value = match description.repr() {
        ConstRepr::Value(value) => value.value(),
        ConstRepr::Deferred(deferred) => match cx.force_computation(db, *deferred) {
            EvalOutcome::Ready(value) => value.value(),
            EvalOutcome::Blocked(info) => return EvalOutcome::Blocked(info),
            EvalOutcome::Failed(failure) => return EvalOutcome::Failed(failure),
        },
        ConstRepr::Term(term) => {
            match force_const_term_operand(
                db,
                TyId::const_ty(db, *term),
                cx,
                origin,
                description.term_provenance(),
            ) {
                Ok(value) => value,
                Err(stop) => return EvalOutcome::from(Err(stop)),
            }
        }
    };
    if let Some(dependency) = sem_const_dependency(db, value) {
        return EvalOutcome::Blocked(BlockedInfo::new(ConstDemandKind::Value, dependency, origin));
    }
    if sem_const_ty(db, value) != description.ty() {
        return EvalOutcome::Failed(EvalFailure::Invariant {
            origin,
            message: "description value type differs from its immutable type".into(),
        });
    }
    match VerifiedConstValueId::from_complete_execution(db, value) {
        Ok(value) => EvalOutcome::Ready(value),
        Err(message) => EvalOutcome::Failed(EvalFailure::Invariant {
            origin,
            message: message.into(),
        }),
    }
}

/// Force an already instantiated type-system term through the common service.
/// Callers without source occurrences use a synthetic origin and retain their
/// own optional-fold or required-value diagnostic policy.
pub(crate) fn force_const_term_value<'db>(
    db: &'db dyn HirAnalysisDb,
    term: ConstTyId<'db>,
    config: CtfeConfig,
    origin: SemOrigin<'db>,
) -> EvalOutcome<'db, VerifiedConstValueId<'db>> {
    force_const_term_value_with_steps(db, term, config, origin, &mut 0)
}

pub(super) fn force_const_term_value_with_steps<'db>(
    db: &'db dyn HirAnalysisDb,
    term: ConstTyId<'db>,
    config: CtfeConfig,
    origin: SemOrigin<'db>,
    steps: &mut usize,
) -> EvalOutcome<'db, VerifiedConstValueId<'db>> {
    let mut cx = ForceContext::new(config);
    cx.steps = *steps;
    let result = force_const_term_operand(db, TyId::const_ty(db, term), &mut cx, origin, None);
    *steps = cx.steps;
    match result {
        Ok(value) => match VerifiedConstValueId::from_complete_execution(db, value) {
            Ok(value) => EvalOutcome::Ready(value),
            Err(message) => EvalOutcome::Failed(EvalFailure::Invariant {
                origin,
                message: message.into(),
            }),
        },
        Err(stop) => EvalOutcome::from(Err(stop)),
    }
}

fn term_integer<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
    origin: SemOrigin<'db>,
) -> EvalResult<'db, BigInt> {
    let SemConstValue::Scalar {
        ty,
        value: SemConstScalar::Int { value },
    } = value.value(db)
    else {
        return Err(CtfeError::InvalidOperation {
            origin,
            message: "expected an integer constant term operand".into(),
        }
        .into());
    };
    let Some((bits, signed)) = int_ty_shape(db, *ty) else {
        return Err(CtfeError::InvalidOperation {
            origin,
            message: "integer constant term operand has no integer type".into(),
        }
        .into());
    };
    Ok(normalize_int_to_shape(value.clone(), bits, signed))
}

fn term_operation_error<'db>(
    db: &'db dyn HirAnalysisDb,
    mut error: CtfeError<'db>,
    provenance: Option<&TermProvenance<'db>>,
) -> EvalStop<'db> {
    if let Some(provenance) = provenance {
        for frame in provenance.frames.iter().rev() {
            error = CtfeError::CalleeError {
                origin: frame.origin,
                callee: get_or_build_semantic_instance(db, frame.callee),
                source: Box::new(error),
            };
        }
    }
    error.into()
}

fn force_const_term<'db>(
    db: &'db dyn HirAnalysisDb,
    term: ConstExprId<'db>,
    result_ty: TyId<'db>,
    cx: &mut ForceContext,
    origin: SemOrigin<'db>,
    provenance: Option<&TermProvenance<'db>>,
) -> EvalResult<'db, SemConstId<'db>> {
    if let ConstExpr::Invocation(invocation) = term.data(db) {
        // Inputs are forced here, so the request is a resolved call owned by
        // its callee, as `const_computation_for_instance` builds one.
        let scope = invocation.key.owner(db).scope();
        let inputs = invocation
            .args
            .iter()
            .copied()
            .enumerate()
            .map(|(index, arg)| match arg.data(db) {
                TyData::ConstTy(term) => {
                    let child = term_child_provenance(db, provenance, arg, index, origin)?;
                    Ok(if let Some(child) = child {
                        ConstDesc::term_with_provenance(db, scope, child.clone())
                    } else {
                        ConstDesc::term(db, scope, *term)
                    })
                }
                _ => Err(EvalStop::Failed(EvalFailure::Invariant {
                    origin,
                    message: "invocation input is not a constant term".into(),
                })),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut values = Vec::with_capacity(inputs.len());
        for input in inputs {
            match force_const_description_in_context(db, &input, cx, origin) {
                EvalOutcome::Ready(value) => values.push(ConstDesc::value(db, value, scope)),
                EvalOutcome::Blocked(info) => return Err(EvalStop::Blocked(info)),
                EvalOutcome::Failed(failure) => return Err(EvalStop::Failed(failure)),
            }
        }
        let request = ConstComputationId::new(
            db,
            ConstEntry::Resolved(invocation.key),
            values,
            result_ty,
            scope,
            origin,
        );
        let wraps_callee = matches!(invocation.key.owner(db), BodyOwner::Func(func) if func.is_const(db) && !func.is_extern(db));
        return match cx.force_computation(db, request) {
            EvalOutcome::Ready(value) => Ok(value.value()),
            EvalOutcome::Blocked(mut info) => {
                if wraps_callee {
                    info.trace.push(invocation.key);
                }
                if let Some(provenance) = provenance {
                    info.trace
                        .extend(provenance.frames.iter().rev().map(|frame| frame.callee));
                }
                Err(EvalStop::Blocked(info))
            }
            EvalOutcome::Failed(EvalFailure::Ctfe(error)) => {
                let error = if wraps_callee {
                    CtfeError::CalleeError {
                        origin,
                        callee: get_or_build_semantic_instance(db, invocation.key),
                        source: Box::new(error),
                    }
                } else {
                    error
                };
                Err(term_operation_error(db, error, provenance))
            }
            EvalOutcome::Failed(failure) => Err(EvalStop::Failed(failure)),
        };
    }
    let operation_error = |error| term_operation_error(db, error, provenance);
    let boolean = |value: SemConstId<'db>| match value.value(db) {
        SemConstValue::Scalar {
            value: SemConstScalar::Bool(value),
            ..
        } => Ok(*value),
        _ => Err(operation_error(CtfeError::InvalidOperation {
            origin,
            message: "expected a boolean constant term operand".into(),
        })),
    };
    match term.data(db) {
        ConstExpr::Select {
            cond,
            then,
            otherwise,
        } => {
            let child = term_child_provenance(db, provenance, *cond, 0, origin)?;
            let taken = boolean(force_const_term_operand(db, *cond, cx, origin, child)?)?;
            let (index, arm) = if taken { (1, *then) } else { (2, *otherwise) };
            let child = term_child_provenance(db, provenance, arm, index, origin)?;
            return force_const_term_operand(db, arm, cx, origin, child);
        }
        ConstExpr::Compare { op, lhs, rhs } => {
            let lhs_child = term_child_provenance(db, provenance, *lhs, 0, origin)?;
            let lhs = force_const_term_operand(db, *lhs, cx, origin, lhs_child)?;
            let rhs_child = term_child_provenance(db, provenance, *rhs, 1, origin)?;
            let rhs = force_const_term_operand(db, *rhs, cx, origin, rhs_child)?;
            let ordering = if let (Ok(lhs), Ok(rhs)) = (boolean(lhs), boolean(rhs)) {
                lhs.cmp(&rhs)
            } else {
                term_integer(db, lhs, origin)?.cmp(&term_integer(db, rhs, origin)?)
            };
            let holds = match op {
                CompBinOp::Eq => ordering.is_eq(),
                CompBinOp::NotEq => ordering.is_ne(),
                CompBinOp::Lt => ordering.is_lt(),
                CompBinOp::LtEq => ordering.is_le(),
                CompBinOp::Gt => ordering.is_gt(),
                CompBinOp::GtEq => ordering.is_ge(),
            };
            return Ok(bool_const(db, holds));
        }
        ConstExpr::UnOp {
            op: UnOp::Not,
            expr,
            ..
        } => {
            let child = term_child_provenance(db, provenance, *expr, 0, origin)?;
            let value = boolean(force_const_term_operand(db, *expr, cx, origin, child)?)?;
            return Ok(bool_const(db, !value));
        }
        ConstExpr::Cast { expr, to } => {
            if *to != result_ty {
                return Err(EvalStop::Failed(EvalFailure::Invariant {
                    origin,
                    message: "cast target differs from its result type".into(),
                }));
            }
            let child = term_child_provenance(db, provenance, *expr, 0, origin)?;
            let value = force_const_term_operand(db, *expr, cx, origin, child)?;
            return execute_scalar_cast(db, result_ty, value)
                .map_err(|fault| operation_error(primitive_error(origin, fault)));
        }
        ConstExpr::ArrayRepeat { value, len } => {
            let child = term_child_provenance(db, provenance, *value, 0, origin)?;
            let value = force_const_term_operand(db, *value, cx, origin, child)?;
            if !result_ty.is_array(db)
                || result_ty.generic_args(db).first().copied() != Some(sem_const_ty(db, value))
            {
                return Err(EvalStop::Failed(EvalFailure::Invariant {
                    origin,
                    message: "repeat element differs from its array element type".into(),
                }));
            }
            let child = term_child_provenance(db, provenance, *len, 1, origin)?;
            let len = force_const_term_operand(db, *len, cx, origin, child)?;
            let len = term_integer(db, len, origin)?
                .to_usize()
                .ok_or_else(|| operation_error(CtfeError::StepLimitExceeded { origin }))?;
            cx.charge(len, origin)?;
            return Ok(array_const(
                db,
                result_ty,
                vec![value; len].into_boxed_slice(),
            ));
        }
        ConstExpr::ArrayIndex { array: base, .. } | ConstExpr::Field { value: base, .. } => {
            let child = term_child_provenance(db, provenance, *base, 0, origin)?;
            let base = force_const_term_operand(db, *base, cx, origin, child)?;
            let index = match term.data(db) {
                ConstExpr::ArrayIndex { index, .. } => {
                    let child = term_child_provenance(db, provenance, *index, 1, origin)?;
                    let index = force_const_term_operand(db, *index, cx, origin, child)?;
                    term_integer(db, index, origin)?
                        .to_usize()
                        .ok_or_else(|| operation_error(CtfeError::OutOfBounds { origin }))?
                }
                ConstExpr::Field { index, .. } => *index,
                _ => unreachable!(),
            };
            let value = match (term.data(db), base.value(db)) {
                (ConstExpr::ArrayIndex { .. }, SemConstValue::Array { elems, .. })
                | (
                    ConstExpr::Field { .. },
                    SemConstValue::Tuple { elems, .. }
                    | SemConstValue::Struct { fields: elems, .. },
                ) => elems.get(index).copied(),
                (
                    ConstExpr::ArrayIndex { .. },
                    SemConstValue::Scalar {
                        value: SemConstScalar::Bytes(bytes),
                        ..
                    },
                ) => bytes
                    .get(index)
                    .map(|byte| int_const(db, TyId::u8(db), BigInt::from(*byte))),
                _ => {
                    return Err(operation_error(CtfeError::InvalidOperation {
                        origin,
                        message: "invalid const projection".into(),
                    }));
                }
            };
            return value.ok_or_else(|| operation_error(CtfeError::OutOfBounds { origin }));
        }
        _ => {}
    }
    let Some((bits, signed)) = int_ty_shape(db, result_ty) else {
        return Err(CtfeError::NotConstEvaluable { origin }.into());
    };
    let value = match term.data(db) {
        ConstExpr::ArithBinOp { op, mode, lhs, rhs } => {
            let lhs_provenance = term_child_provenance(db, provenance, *lhs, 0, origin)?;
            let rhs_provenance = term_child_provenance(db, provenance, *rhs, 1, origin)?;
            let lhs = term_integer(
                db,
                force_const_term_operand(db, *lhs, cx, origin, lhs_provenance)?,
                origin,
            )?;
            let rhs = term_integer(
                db,
                force_const_term_operand(db, *rhs, cx, origin, rhs_provenance)?,
                origin,
            )?;
            execute_source_int_binary(db, result_ty, *mode, *op, lhs, rhs).map_err(|fault| {
                term_operation_error(db, primitive_error(origin, fault), provenance)
            })?
        }
        ConstExpr::UnOp { op, mode, expr } => {
            let child = term_child_provenance(db, provenance, *expr, 0, origin)?;
            let value = term_integer(
                db,
                force_const_term_operand(db, *expr, cx, origin, child)?,
                origin,
            )?;
            execute_source_int_unary(db, result_ty, *mode, *op, value).map_err(|fault| {
                term_operation_error(db, primitive_error(origin, fault), provenance)
            })?
        }
        _ => return Err(CtfeError::NotConstEvaluable { origin }.into()),
    };
    Ok(int_const(
        db,
        result_ty,
        normalize_int_to_shape(value, bits, signed),
    ))
}

pub fn force_const_computation<'db>(
    db: &'db dyn HirAnalysisDb,
    computation: ConstComputationId<'db>,
    config: CtfeConfig,
) -> EvalOutcome<'db, VerifiedConstValueId<'db>> {
    force_const_computation_with_cost(db, computation, config).0
}

#[salsa::tracked(cycle_initial=force_const_computation_cycle_initial, cycle_fn=force_const_computation_cycle_recover)]
fn force_const_computation_with_cost<'db>(
    db: &'db dyn HirAnalysisDb,
    computation: ConstComputationId<'db>,
    config: CtfeConfig,
) -> (EvalOutcome<'db, VerifiedConstValueId<'db>>, usize) {
    let mut cx = ForceContext::new(config);
    let outcome = force_const_computation_in_context(db, computation, &mut cx);
    (outcome, cx.steps)
}

fn force_const_computation_in_context<'db>(
    db: &'db dyn HirAnalysisDb,
    computation: ConstComputationId<'db>,
    cx: &mut ForceContext,
) -> EvalOutcome<'db, VerifiedConstValueId<'db>> {
    let origin = computation.origin(db);
    if computation.result_ty(db).has_invalid(db) {
        return EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::InvalidBody { origin }));
    }
    let mut args = Vec::with_capacity(computation.inputs(db).len());
    for input in computation.inputs(db) {
        match force_const_description_in_context(db, input, cx, origin) {
            EvalOutcome::Ready(value) => args.push(value.value()),
            EvalOutcome::Blocked(info) => return EvalOutcome::Blocked(info),
            EvalOutcome::Failed(failure) => return EvalOutcome::Failed(failure),
        }
    }

    let result = match computation.entry(db) {
        ConstEntry::Resolved(key) => cx.execute_body(db, key, args, origin),
        ConstEntry::Associated(use_) => {
            let Some(const_ty) = const_ty_from_assoc_const_use(db, use_) else {
                let dependent = use_
                    .inst()
                    .args(db)
                    .iter()
                    .chain(use_.inst().assoc_type_bindings(db).values())
                    .any(|ty| ty.has_param(db) || ty.has_var(db) || ty.has_projection(db));
                return if dependent {
                    EvalOutcome::Blocked(BlockedInfo::new(
                        ConstDemandKind::CallableSelection,
                        ConstDependency::AssociatedSelection(use_),
                        origin,
                    ))
                } else {
                    EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::InvalidOperation {
                        origin,
                        message: "associated constant selection has no unique value".into(),
                    }))
                };
            };
            force_selected_const_ty(
                db,
                const_ty,
                computation.result_ty(db),
                ImplEnv::new(
                    db,
                    use_.origin_scope(),
                    use_.assumptions(),
                    vec![use_.inst()],
                ),
                cx,
                origin,
            )
        }
        ConstEntry::Inherent(use_) => {
            let Some(const_ty) = const_ty_from_inherent_const_use(db, use_) else {
                let receiver = use_.receiver_ty();
                return if receiver.has_param(db)
                    || receiver.has_var(db)
                    || receiver.has_projection(db)
                {
                    EvalOutcome::Blocked(BlockedInfo::new(
                        ConstDemandKind::CallableSelection,
                        ConstDependency::InherentSelection(use_),
                        origin,
                    ))
                } else {
                    EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::InvalidOperation {
                        origin,
                        message: "inherent constant has no matching value".into(),
                    }))
                };
            };
            force_selected_const_ty(
                db,
                const_ty,
                computation.result_ty(db),
                ImplEnv::new(db, use_.origin_scope(), use_.assumptions(), Vec::new()),
                cx,
                origin,
            )
        }
    };
    match result {
        EvalOutcome::Ready(value) => {
            if let Some(dependency) = sem_const_dependency(db, value) {
                return EvalOutcome::Blocked(BlockedInfo::new(
                    ConstDemandKind::Value,
                    dependency,
                    origin,
                ));
            }
            if sem_const_ty(db, value) != computation.result_ty(db) {
                return EvalOutcome::Failed(EvalFailure::Invariant {
                    origin,
                    message: "CTFE result type differs from its immutable request".into(),
                });
            }
            match VerifiedConstValueId::from_complete_execution(db, value) {
                Ok(value) => EvalOutcome::Ready(value),
                Err(message) => EvalOutcome::Failed(EvalFailure::Invariant {
                    origin,
                    message: message.into(),
                }),
            }
        }
        EvalOutcome::Blocked(info) => EvalOutcome::Blocked(info),
        EvalOutcome::Failed(failure) => EvalOutcome::Failed(failure),
    }
}

fn force_const_computation_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    computation: ConstComputationId<'db>,
    _config: CtfeConfig,
) -> (EvalOutcome<'db, VerifiedConstValueId<'db>>, usize) {
    (
        EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::RecursiveConst {
            origin: computation.origin(db),
        })),
        0,
    )
}

fn force_const_computation_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &(EvalOutcome<'db, VerifiedConstValueId<'db>>, usize),
    _count: u32,
    _computation: ConstComputationId<'db>,
    _config: CtfeConfig,
) -> salsa::CycleRecoveryAction<(EvalOutcome<'db, VerifiedConstValueId<'db>>, usize)> {
    salsa::CycleRecoveryAction::Iterate
}

fn force_selected_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    expected: TyId<'db>,
    impl_env: ImplEnv<'db>,
    cx: &mut ForceContext,
    origin: SemOrigin<'db>,
) -> EvalOutcome<'db, SemConstId<'db>> {
    if const_ty.ty(db).has_invalid(db) {
        return EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::InvalidBody { origin }));
    }
    match const_ty.data(db) {
        ConstTyData::UnEvaluated {
            body,
            ty,
            template_ty,
            capture,
            ..
        } => {
            let owner = BodyOwner::AnonConstBody {
                body: *body,
                expected: template_ty.or(*ty).unwrap_or(expected),
            };
            let key = SemanticInstanceKey::new(
                db,
                owner,
                capture.generic_subst(db),
                EffectProviderSubst::empty(db),
                impl_env,
            );
            cx.execute_body(db, key, Vec::new(), origin)
        }
        ConstTyData::Computation { .. }
        | ConstTyData::Description(..)
        | ConstTyData::Abstract(..) => EvalOutcome::from(force_const_term_operand(
            db,
            TyId::const_ty(db, const_ty),
            cx,
            origin,
            None,
        )),
        ConstTyData::Value(..) => {
            let value = sem_const_from_ty(db, TyId::new(db, TyData::ConstTy(const_ty)));
            match value {
                Some(value) => EvalOutcome::Ready(value),
                None => EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::InvalidOperation {
                    origin,
                    message: "selected constant has an invalid value".into(),
                })),
            }
        }
        ConstTyData::Invalid(..) => {
            EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::InvalidOperation {
                origin,
                message: "selected constant has an invalid value".into(),
            }))
        }
        ConstTyData::TyParam(..) | ConstTyData::TyVar(..) | ConstTyData::Hole(..) => {
            EvalOutcome::Blocked(BlockedInfo::new(
                ConstDemandKind::Value,
                ConstDependency::Value(TyId::new(db, TyData::ConstTy(const_ty))),
                origin,
            ))
        }
    }
}
