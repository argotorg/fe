//! Candidate loop frontiers and their normalized-effect proof obligations.
use cranelift_entity::EntityRef;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            capability::{
                external::{ExternalOrigin, MemoryOffset},
                footprint::{AccessExtent, AccessFootprint},
                guard::Guard,
                index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
                path::RegionPath,
                region::{OverlapResult, RegionRoot, RegionSet, SymbolicPlace},
                state::CapabilityValue,
                value::Guarded,
            },
            normalized::{
                NBlockId, NExpr, NPlaceBase, NRootKind, NStatementKind, NTerminatorKind,
                NValueDefinition, NValueId, NormalizedBody, NormalizedBodyVerifyError,
            },
        },
        ty::{
            corelib::{
                MemoryAccessKind, PrimitiveWrapperCallKind, core_primitive_wrapper_call_kind,
            },
            ty_check::BodyOwner,
            ty_def::BorrowKind,
        },
    },
    hir_def::{ArithBinOp, BinOp, CompBinOp},
};

use super::{
    control::{LoopEdge, ValidatedLoop, validated_loops},
    scalar::integer_model,
    solver::{BorrowSummaryMode, Borrowck},
};

#[derive(Clone, Debug)]
pub(super) struct FrontierCandidate {
    pub loop_region: ValidatedLoop,
    pub frontier_root: NPlaceBase,
    pub header_value: NValueId,
    pub bound: NValueId,
    pub condition: NValueId,
    pub body: NBlockId,
    pub full_exit: NBlockId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum FrontierRejection {
    UnsupportedHeader,
    UnsupportedCondition,
    UnsupportedFrontier,
    UnsupportedBound,
    UnsupportedScalarType,
    UnsupportedLoopShape,
    MissingZeroBase,
    MissingStore,
    MissingIncrement,
    UnsupportedArithmetic,
    AmbiguousStore,
    SelectorChanged,
    FrontierChanged,
    UnsupportedFamily,
    InterferingEffect,
    PendingCallee,
}

#[derive(Clone, Debug)]
pub(super) struct FrontierStep {
    pub candidate: FrontierCandidate,
    pub store_statement: usize,
    pub increment_statement: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ContentsCertificateKind {
    Prefix,
    LastWrite,
}

#[derive(Clone, Debug)]
pub(super) struct VerifiedFrontierEffects<'db> {
    pub step: FrontierStep,
    pub kind: ContentsCertificateKind,
    pub family: RegionRoot<'db>,
    pub family_scope: BinderScope,
    pub member: IndexExpr<'db>,
    pub store_value: NValueId,
}

#[derive(Clone, Debug)]
pub(super) struct PrefixCertificate<'db> {
    pub family: RegionRoot<'db>,
    pub family_scope: BinderScope,
    pub coverage: Guard<'db>,
    pub contents: CapabilityValue<'db>,
    pub exit: LoopEdge,
}

pub(super) fn frontier_candidates<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
) -> Result<Vec<Result<FrontierCandidate, FrontierRejection>>, NormalizedBodyVerifyError> {
    Ok(validated_loops(body)?
        .into_iter()
        .map(|loop_region| frontier_candidate(db, body, loop_region))
        .collect())
}

fn frontier_candidate<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    loop_region: ValidatedLoop,
) -> Result<FrontierCandidate, FrontierRejection> {
    // The supported loop is a header and one straight-line body block.
    if loop_region.blocks.len() != 2 {
        return Err(FrontierRejection::UnsupportedLoopShape);
    }
    let NTerminatorKind::Branch {
        cond,
        then_target,
        else_target,
    } = &body.blocks[loop_region.header.index()].terminator.kind
    else {
        return Err(FrontierRejection::UnsupportedHeader);
    };
    if !loop_region.blocks.contains(&then_target.block)
        || loop_region.blocks.contains(&else_target.block)
    {
        return Err(FrontierRejection::UnsupportedHeader);
    }
    let NValueDefinition::Statement { block, statement } =
        body.values[cond.value.index()].definition
    else {
        return Err(FrontierRejection::UnsupportedCondition);
    };
    if block != loop_region.header {
        return Err(FrontierRejection::UnsupportedCondition);
    }
    let NStatementKind::Define { expr, .. } =
        &body.blocks[block.index()].statements[statement as usize].kind
    else {
        return Err(FrontierRejection::UnsupportedCondition);
    };
    let (lhs, rhs) = match expr {
        NExpr::Binary {
            op: BinOp::Comp(CompBinOp::Lt),
            lhs,
            rhs,
        } => (lhs.value, rhs.value),
        NExpr::Call { callee, args, .. } => {
            let BodyOwner::Func(function) = callee.key.owner(db) else {
                return Err(FrontierRejection::UnsupportedCondition);
            };
            if core_primitive_wrapper_call_kind(db, function, body.values[cond.value.index()].ty)
                != Some(PrimitiveWrapperCallKind::Binary(BinOp::Comp(CompBinOp::Lt)))
            {
                return Err(FrontierRejection::UnsupportedCondition);
            }
            let [lhs, rhs] = args.as_ref() else {
                return Err(FrontierRejection::UnsupportedCondition);
            };
            (lhs.value, rhs.value)
        }
        _ => return Err(FrontierRejection::UnsupportedCondition),
    };
    let NValueDefinition::Statement { block, statement } = body.values[lhs.index()].definition
    else {
        return Err(FrontierRejection::UnsupportedFrontier);
    };
    if block != loop_region.header {
        return Err(FrontierRejection::UnsupportedFrontier);
    }
    let NStatementKind::Define {
        expr: NExpr::Load { place, .. },
        ..
    } = &body.blocks[block.index()].statements[statement as usize].kind
    else {
        return Err(FrontierRejection::UnsupportedFrontier);
    };
    let NPlaceBase::Root(root) = place.base else {
        return Err(FrontierRejection::UnsupportedFrontier);
    };
    if !place.path.is_empty()
        || !matches!(body.roots[root.index()].kind, NRootKind::LocalSlot { .. })
    {
        return Err(FrontierRejection::UnsupportedFrontier);
    }
    // A scalar operand may reach the comparison through copies of the parameter.
    let mut bound = rhs;
    while let Some((_, NExpr::Forward { src })) = body.defining_expr(bound) {
        bound = src.value;
    }
    if !matches!(
        body.values[bound.index()].definition,
        NValueDefinition::EntryParam { .. }
    ) {
        return Err(FrontierRejection::UnsupportedBound);
    }
    if integer_model(db, body.values[lhs.index()].ty)
        != integer_model(db, body.values[bound.index()].ty)
        || !integer_model(db, body.values[lhs.index()].ty).is_some_and(|(_, signed)| !signed)
    {
        return Err(FrontierRejection::UnsupportedScalarType);
    }
    Ok(FrontierCandidate {
        loop_region,
        frontier_root: place.base,
        header_value: lhs,
        bound,
        condition: cond.value,
        body: then_target.block,
        full_exit: else_target.block,
    })
}

impl<'db> Borrowck<'db> {
    pub(super) fn verify_frontier_structure(
        &self,
        candidate: FrontierCandidate,
    ) -> Result<FrontierStep, FrontierRejection> {
        let loop_region = &candidate.loop_region;
        // Besides copies of the bound, the header only reads the frontier and compares.
        let bound = self.index(candidate.bound);
        let header: Vec<_> = self.body.blocks[loop_region.header.index()]
            .statements
            .iter()
            .filter(|statement| {
                !matches!(&statement.kind,
                    NStatementKind::Define { result, expr: NExpr::Forward { .. } }
                        if self.index(*result) == bound)
            })
            .collect();
        if !loop_region.blocks.contains(&candidate.body)
            || loop_region.backedges.len() != 1
            || loop_region.backedges[0].from != candidate.body
            || loop_region.backedges[0].to != loop_region.header
            || loop_region.exits.len() != 1
            || loop_region.exits[0].from != loop_region.header
            || loop_region.exits[0].to != candidate.full_exit
            || loop_region.entries.is_empty()
            || !matches!(header.as_slice(), [value, condition]
                if matches!(&value.kind,
                    NStatementKind::Define { result, .. } if *result == candidate.header_value)
                && matches!(&condition.kind,
                    NStatementKind::Define { result, .. } if *result == candidate.condition))
        {
            return Err(FrontierRejection::UnsupportedLoopShape);
        }
        for entry in &loop_region.entries {
            let block = &self.body.blocks[entry.from.index()];
            let Some(NStatementKind::Store { destination, value }) =
                block.statements.last().map(|statement| &statement.kind)
            else {
                return Err(FrontierRejection::MissingZeroBase);
            };
            if destination.base != candidate.frontier_root
                || !destination.path.is_empty()
                || self.index(value.value) != IndexExpr::Const(0)
            {
                return Err(FrontierRejection::MissingZeroBase);
            }
        }
        let body = &self.body.blocks[candidate.body.index()];
        if !matches!(&body.terminator.kind, NTerminatorKind::Goto(target) if target.block == loop_region.header)
            || self.terminal[candidate.body.index()].is_none()
        {
            return Err(FrontierRejection::UnsupportedLoopShape);
        }
        let Some((
            increment_statement,
            NStatementKind::Define {
                result,
                expr: NExpr::Call { callee, args, .. },
                ..
            },
        )) = body
            .statements
            .iter()
            .enumerate()
            .next_back()
            .map(|(index, statement)| (index, &statement.kind))
        else {
            return Err(FrontierRejection::MissingIncrement);
        };
        let BodyOwner::Func(function) = callee.key.owner(self.db) else {
            return Err(FrontierRejection::UnsupportedArithmetic);
        };
        if core_primitive_wrapper_call_kind(self.db, function, self.body.values[result.index()].ty)
            != Some(PrimitiveWrapperCallKind::Assign(BinOp::Arith(
                ArithBinOp::Add,
            )))
        {
            return Err(FrontierRejection::UnsupportedArithmetic);
        }
        let [receiver, unit] = args.as_ref() else {
            return Err(FrontierRejection::MissingIncrement);
        };
        let NValueDefinition::Statement { block, statement } =
            self.body.values[receiver.value.index()].definition
        else {
            return Err(FrontierRejection::MissingIncrement);
        };
        let NStatementKind::Define {
            expr:
                NExpr::Borrow {
                    place,
                    kind: BorrowKind::Mut,
                    ..
                },
            ..
        } = &self.body.blocks[block.index()].statements[statement as usize].kind
        else {
            return Err(FrontierRejection::MissingIncrement);
        };
        if block != candidate.body
            || place.base != candidate.frontier_root
            || !place.path.is_empty()
            || self.index(unit.value) != IndexExpr::Const(1)
            || self.body.values[unit.value.index()].ty
                != self.body.values[candidate.header_value.index()].ty
        {
            return Err(FrontierRejection::MissingIncrement);
        }
        let stores: Vec<_> = body
            .statements
            .iter()
            .take(increment_statement)
            .enumerate()
            .filter(|(_, statement)| self.stores_capability(statement))
            .map(|(index, _)| index)
            .collect();
        let [store_statement] = stores.as_slice() else {
            return Err(FrontierRejection::MissingStore);
        };
        Ok(FrontierStep {
            candidate,
            store_statement: *store_statement,
            increment_statement,
        })
    }

    pub(super) fn verify_frontier_effects(
        &self,
        step: FrontierStep,
        kind: ContentsCertificateKind,
    ) -> Result<VerifiedFrontierEffects<'db>, FrontierRejection> {
        let block = step.candidate.body.index();
        let NStatementKind::Store { destination, value } =
            &self.body.blocks[block].statements[step.store_statement].kind
        else {
            return Err(FrontierRejection::MissingStore);
        };
        let before = &self.before[block][step.store_statement];
        let region = self.resolve_region(before, destination);
        let [clause] = region.clauses() else {
            return Err(FrontierRejection::AmbiguousStore);
        };
        if region.definite_write().is_none()
            || !before.guard().implies(&clause.guard)
            || !clause.payload.path.is_empty()
            || clause.payload.views.iter().next().is_some()
        {
            return Err(FrontierRejection::AmbiguousStore);
        }
        let RegionRoot::External(actual) = &clause.payload.root else {
            return Err(FrontierRejection::UnsupportedFamily);
        };
        let selector = match &actual.origin {
            ExternalOrigin::Memory {
                offset: MemoryOffset::Element(_, selector),
                ..
            } => *selector,
            _ if kind == ContentsCertificateKind::LastWrite => IndexExpr::Const(0),
            _ => return Err(FrontierRejection::UnsupportedFamily),
        };
        let header_index = self.index(step.candidate.header_value);
        let expected_selector = match kind {
            ContentsCertificateKind::Prefix => header_index,
            ContentsCertificateKind::LastWrite => IndexExpr::Const(0),
        };
        if !before.guard().proves_equal(selector, expected_selector) {
            return Err(FrontierRejection::SelectorChanged);
        }
        let families: Vec<_> = before
            .storage()
            .filter_map(|(root, contents)| {
                let RegionRoot::External(source) = root else {
                    return None;
                };
                let member = match &source.origin {
                    ExternalOrigin::Memory {
                        offset: MemoryOffset::Element(_, member),
                        ..
                    } if member.bound_namespace() == Some(IndexNamespace::InputSlot)
                        && contents.scope().variables().collect::<Vec<_>>() == vec![*member] =>
                    {
                        *member
                    }
                    _ if kind == ContentsCertificateKind::LastWrite
                        && contents.scope().variables().next().is_none() =>
                    {
                        IndexExpr::Const(0)
                    }
                    _ => return None,
                };
                if source.uncertain()
                    || source.is_reachable()
                    || contents.shape() != before.value(value.value).shape()
                {
                    return None;
                }
                let matched = source.match_instance(contents.scope(), actual, region.scope())?;
                let write = matched.write?;
                (clause.guard.implies(&matched.guard) && write.guard.proves_equal(member, selector))
                    .then(|| (root.clone(), contents.scope().clone(), member))
            })
            .collect();
        let [(family, family_scope, member)] = families.as_slice() else {
            return Err(FrontierRejection::UnsupportedFamily);
        };
        let scope = BinderScope::default();
        let prior = if *member == IndexExpr::Const(0) {
            RegionSet::singleton(&scope, family.clone(), RegionPath::default())
        } else {
            let (opened, witness) = scope.bind(IndexNamespace::Existential);
            let substitution = IndexSubst::new(family_scope, &opened, [(*member, witness)])
                .map_err(|_| FrontierRejection::UnsupportedFamily)?;
            let prior_root = family.substitute(self.db, &substitution);
            let prior_guard = match kind {
                ContentsCertificateKind::Prefix => {
                    Guard::always(&opened).with_bound(witness, header_index)
                }
                ContentsCertificateKind::LastWrite => {
                    Guard::always(&opened).with_equality(witness, IndexExpr::Const(0))
                }
            }
            .ok_or(FrontierRejection::UnsupportedFamily)?;
            RegionSet::new(
                &scope,
                [Guarded {
                    guard: prior_guard,
                    payload: SymbolicPlace {
                        root: prior_root,
                        path: RegionPath::default(),
                        views: Default::default(),
                    },
                }],
            )
        };
        let NPlaceBase::Root(frontier_root) = step.candidate.frontier_root else {
            return Err(FrontierRejection::UnsupportedFrontier);
        };
        let frontier = RegionSet::singleton(
            &scope,
            self.inventory.roots[frontier_root.index()].clone(),
            RegionPath::default(),
        );
        for loop_block in &step.candidate.loop_region.blocks {
            for (index, operation) in self.operations[loop_block.index()].iter().enumerate() {
                let after_store =
                    *loop_block == step.candidate.body && index > step.store_statement;
                let increment =
                    *loop_block == step.candidate.body && index == step.increment_statement;
                let interferes = |footprint: AccessFootprint<'_, 'db>| {
                    if kind == ContentsCertificateKind::LastWrite
                        && *loop_block == step.candidate.body
                        && index < step.store_statement
                    {
                        return false;
                    }
                    !matches!(
                        AccessFootprint::typed(&prior).overlap(self.db, footprint),
                        OverlapResult::Disjoint
                    ) || (after_store
                        && !matches!(
                            AccessFootprint::typed(&region).overlap(self.db, footprint),
                            OverlapResult::Disjoint
                        ))
                };
                let changes_frontier = |footprint: AccessFootprint<'_, 'db>| {
                    !matches!(
                        AccessFootprint::typed(&frontier).overlap(self.db, footprint),
                        OverlapResult::Disjoint
                    ) && !(increment
                        && matches!(footprint.extent, AccessExtent::Typed)
                        && frontier.provably_covers(footprint.region))
                };
                for access in &operation.accesses {
                    if matches!(
                        access.kind,
                        MemoryAccessKind::Write | MemoryAccessKind::Move
                    ) && !(*loop_block == step.candidate.body
                        && index == step.store_statement
                        && access.kind == MemoryAccessKind::Write
                        && access.region == region)
                        && interferes(AccessFootprint::typed(&access.region))
                    {
                        return Err(FrontierRejection::InterferingEffect);
                    }
                    if matches!(
                        access.kind,
                        MemoryAccessKind::Write | MemoryAccessKind::Move
                    ) && changes_frontier(AccessFootprint::typed(&access.region))
                    {
                        return Err(FrontierRejection::FrontierChanged);
                    }
                }
                for access in &operation.calls {
                    if matches!(
                        access.access.kind,
                        MemoryAccessKind::Write | MemoryAccessKind::Move
                    ) && interferes(access.access.footprint())
                    {
                        return Err(FrontierRejection::InterferingEffect);
                    }
                    if matches!(
                        access.access.kind,
                        MemoryAccessKind::Write | MemoryAccessKind::Move
                    ) && changes_frontier(access.access.footprint())
                    {
                        return Err(FrontierRejection::FrontierChanged);
                    }
                }
                if operation.availability.as_ref().is_some_and(|availability| {
                    [&availability.reinitialized, &availability.unavailable]
                        .into_iter()
                        .any(|region| interferes(AccessFootprint::typed(region)))
                }) || operation
                    .births
                    .iter()
                    .any(|birth| birth.selector(family, family_scope).is_some())
                {
                    return Err(FrontierRejection::InterferingEffect);
                }
                if operation.availability.as_ref().is_some_and(|availability| {
                    [&availability.reinitialized, &availability.unavailable]
                        .into_iter()
                        .any(|region| changes_frontier(AccessFootprint::typed(region)))
                }) {
                    return Err(FrontierRejection::FrontierChanged);
                }
                if let NStatementKind::Define {
                    result,
                    expr: NExpr::Call { .. },
                } = &self.body.blocks[loop_block.index()].statements[index].kind
                    && self.calls.get(result).is_some_and(|call| call.pending)
                {
                    return Err(FrontierRejection::PendingCallee);
                }
            }
        }
        Ok(VerifiedFrontierEffects {
            store_value: value.value,
            step,
            kind,
            family: family.clone(),
            family_scope: family_scope.clone(),
            member: *member,
        })
    }

    pub(super) fn complete_prefix_certificate(
        &mut self,
        proof: VerifiedFrontierEffects<'db>,
    ) -> Result<PrefixCertificate<'db>, FrontierRejection> {
        let backedge = proof.step.candidate.loop_region.backedges[0];
        let iteration = self
            .inventory
            .loops
            .feedback(backedge.from, backedge.to)
            .ok_or(FrontierRejection::UnsupportedLoopShape)?;
        let block = proof.step.candidate.body.index();
        let before = &self.before[block][proof.step.store_statement];
        let mut state = before.clone();
        let loops = &self.inventory.loops;
        state.forget_iteration(
            &mut self.inventory.values,
            Some(before),
            |index| loops.repeats_index(iteration, index),
            |index| loops.drops_fact(iteration, index),
            |occurrence| loops.repeats_occurrence(iteration, occurrence),
        );
        let contents = state.value(proof.store_value);
        let lift = IndexSubst::new(contents.scope(), &proof.family_scope, [])
            .map_err(|_| FrontierRejection::UnsupportedFamily)?;
        let contents = self.inventory.values.substitute(contents, &lift);
        let coverage = match proof.kind {
            ContentsCertificateKind::Prefix => Guard::always(&proof.family_scope)
                .with_bound(proof.member, self.index(proof.step.candidate.bound)),
            ContentsCertificateKind::LastWrite => Guard::always(&proof.family_scope)
                .with_equality(proof.member, IndexExpr::Const(0))
                .and_then(|guard| {
                    guard.with_bound(IndexExpr::Const(0), self.index(proof.step.candidate.bound))
                }),
        }
        .ok_or(FrontierRejection::UnsupportedScalarType)?;
        Ok(PrefixCertificate {
            family: proof.family,
            family_scope: proof.family_scope,
            coverage,
            contents,
            exit: proof.step.candidate.loop_region.exits[0],
        })
    }

    /// Candidates are proved only for a final summary, over the conservative
    /// fixed point of a stable inventory.
    pub(super) fn prove_prefix_certificates(&mut self) -> Vec<PrefixCertificate<'db>> {
        if self.summary_mode != BorrowSummaryMode::Final {
            return Vec::new();
        }
        let mut certificates = Vec::new();
        for candidate in self.frontiers.clone() {
            let Ok(step) = self.verify_frontier_structure(candidate) else {
                continue;
            };
            certificates.extend(
                [
                    ContentsCertificateKind::Prefix,
                    ContentsCertificateKind::LastWrite,
                ]
                .into_iter()
                .find_map(|kind| {
                    self.verify_frontier_effects(step.clone(), kind)
                        .and_then(|effects| self.complete_prefix_certificate(effects))
                        .ok()
                }),
            );
        }
        certificates
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{
            semantic::{
                capability::{guard::ValueOccurrence, loan::CapabilityRef},
                get_or_build_semantic_instance, identity_semantic_instance_key,
            },
            ty::ty_check::BodyOwner,
        },
        test_db::{HirAnalysisTestDb, find_func},
    };

    fn solved<'db>(db: &'db mut HirAnalysisTestDb, source: &str, name: &str) -> Borrowck<'db> {
        let file = db.new_stand_alone(format!("{name}.fe").into(), source);
        let db = &*db;
        let (module, _) = db.top_mod(file);
        let instance = get_or_build_semantic_instance(
            db,
            identity_semantic_instance_key(db, BodyOwner::Func(find_func(db, module, name))),
        );
        let mut checker = Borrowck::new(db, instance).unwrap();
        checker.solve().unwrap();
        checker
    }

    /// Every pointer in the value names a fresh allocation, never an unknown seed.
    fn only_fresh_pointers<'db>(
        checker: &Borrowck<'db>,
        value: &CapabilityValue<'db>,
        at: NValueId,
    ) -> bool {
        let leaves = checker
            .inventory
            .values
            .leaves(value, ValueOccurrence::Value(at));
        !leaves.is_empty()
            && leaves.iter().all(|leaf| {
                matches!(&leaf.payload, CapabilityRef::Address(region)
                    if region.clauses().iter().all(|clause| {
                        matches!(&clause.payload.root,
                            RegionRoot::External(source) if source.is_fresh_allocation() && !source.uncertain())
                    }))
            })
    }

    /// Capability-returning calls in `block` with an integer argument: the
    /// statement index, the result, and that argument's index.
    fn indexed_reads<'a, 'db>(
        checker: &'a Borrowck<'db>,
        block: NBlockId,
    ) -> impl Iterator<Item = (usize, NValueId, IndexExpr<'db>)> + 'a {
        checker.body.blocks[block.index()]
            .statements
            .iter()
            .enumerate()
            .filter_map(|(index, statement)| match &statement.kind {
                NStatementKind::Define {
                    result,
                    expr: NExpr::Call { args, .. },
                } if checker.inventory.shapes[result.index()].contains_capability(checker.db) => {
                    args.iter()
                        .find(|argument| {
                            checker.body.values[argument.value.index()]
                                .ty
                                .is_integral(checker.db)
                        })
                        .map(|argument| (index, *result, checker.index(argument.value)))
                }
                _ => None,
            })
    }

    #[test]
    fn staged_span_loop_certifies_fresh_prefix_contents() {
        let mut db = HirAnalysisTestDb::default();
        let mut checker = solved(
            &mut db,
            r#"
use core::ptr
fn stage_and_read(_ cursor: mut u256, _ count: u256) -> u256 {
    let mut children = ptr::MemArray<ptr::MemSpan>::new_uninit(count)
    let mut i: u256 = 0
    while i < count {
        let data = ptr::MemBuffer::alloc(32)
        *ptr::cast<u8, u256>(data.ptr()) = 7
        children[i as usize] = data.span()
        i += 1
    }
    if count == 0 { return 0 }
    let child = children[0]
    cursor += 1
    *ptr::cast<u8, u256>(child.ptr())
}
"#,
            "stage_and_read",
        );
        let db = checker.db;
        let [candidate] = checker.frontiers.as_slice() else {
            panic!("expected one frontier candidate: {:?}", checker.frontiers);
        };
        let candidate = candidate.clone();
        assert!(!checker.failed_prefix_certificates);
        assert_eq!(checker.prefix_certificates.len(), 1);
        let step = checker.verify_frontier_structure(candidate).unwrap();
        let statements = &checker.body.blocks[step.candidate.body.index()].statements;
        assert!(checker.stores_capability(&statements[step.store_statement]));
        assert_eq!(step.increment_statement, statements.len() - 1);
        let proof = checker
            .verify_frontier_effects(step, ContentsCertificateKind::Prefix)
            .unwrap();
        let certificate = checker.complete_prefix_certificate(proof).unwrap();
        let member = certificate.family_scope.variables().next().unwrap();
        assert_eq!(member.bound_namespace(), Some(IndexNamespace::InputSlot));
        assert!(certificate.coverage.indices().contains(&member));

        // `children[0]` after the loop reads certified fresh contents.
        let fill = checker.frontiers[0].loop_region.blocks.clone();
        let (read_block, (statement, child, _)) = (0..checker.body.blocks.len())
            .map(NBlockId::new)
            .filter(|block| !fill.contains(block))
            .find_map(|block| {
                indexed_reads(&checker, block)
                    .find(|(_, _, selector)| *selector == IndexExpr::Const(0))
                    .map(|read| (block, read))
            })
            .expect("read of the first staged member");
        let state = checker.before[read_block.index()][statement].clone();
        let zero = IndexSubst::new(
            &certificate.family_scope,
            &BinderScope::default(),
            [(member, IndexExpr::Const(0))],
        )
        .unwrap();
        assert!(
            state
                .guard()
                .implies(&certificate.coverage.substitute(&zero).unwrap())
        );
        let zero_region = RegionSet::singleton(
            &BinderScope::default(),
            certificate.family.substitute(db, &zero),
            RegionPath::default(),
        );
        let reachable_zero = zero_region.with_guard(state.guard());
        assert!(
            state
                .certified_initialized_region(&reachable_zero)
                .provably_covers(&reachable_zero)
        );
        let direct = state
            .read_region(
                db,
                &mut checker.inventory.values,
                &zero_region,
                certificate.contents.shape(),
                ValueOccurrence::Value(child),
            )
            .unwrap();
        let after = &checker.before[read_block.index()][statement + 1];
        assert!(only_fresh_pointers(&checker, &direct, child), "typed read");
        assert!(
            only_fresh_pointers(&checker, after.value(child), child),
            "index result"
        );
    }

    #[test]
    fn repeated_single_slot_has_a_nonempty_last_write_certificate() {
        let mut db = HirAnalysisTestDb::default();
        let mut checker = solved(
            &mut db,
            r#"
use core::ptr
fn repeat_cell(_ cursor: mut u256, _ count: u256) -> u256 {
    let mut children = ptr::MemArray<ptr::MemSpan>::new_uninit(1)
    let mut i: u256 = 0
    while i < count {
        let noise = ptr::MemBuffer::alloc(32)
        *ptr::cast<u8, u256>(noise.ptr()) = 4
        let data = ptr::MemBuffer::alloc(32)
        *ptr::cast<u8, u256>(data.ptr()) = 7
        children[0] = data.span()
        i += 1
    }
    if count == 0 { return 0 }
    let child = children[0]
    cursor += 1
    *ptr::cast<u8, u256>(child.ptr())
}
"#,
            "repeat_cell",
        );
        let candidate = checker.frontiers[0].clone();
        let step = checker
            .verify_frontier_structure(candidate.clone())
            .unwrap();
        assert_eq!(
            checker
                .verify_frontier_effects(step.clone(), ContentsCertificateKind::Prefix)
                .map(|_| ())
                .unwrap_err(),
            FrontierRejection::UnsupportedFamily,
            "a fixed cell has no member to extend a prefix over"
        );
        let proof = checker
            .verify_frontier_effects(step, ContentsCertificateKind::LastWrite)
            .unwrap();
        let certificate = checker.complete_prefix_certificate(proof).unwrap();
        assert_eq!(checker.prefix_certificates.len(), 1);
        assert!(certificate.family_scope.variables().next().is_none());
        assert!(
            certificate
                .coverage
                .indices()
                .contains(&checker.index(candidate.bound))
        );
    }

    #[test]
    fn unsupported_fill_loops_report_their_rejection() {
        let mut db = HirAnalysisTestDb::default();
        for (case, (header, store, increment, expected)) in [
            (
                "let mut i: u256 = 0\n    while i <= count {",
                "children[i as usize] = data.span()",
                "i += 1",
                FrontierRejection::UnsupportedCondition,
            ),
            (
                "let mut i: u256 = 0\n    while i < 4 {",
                "children[i as usize] = data.span()",
                "i += 1",
                FrontierRejection::UnsupportedBound,
            ),
            (
                "let mut i: u256 = 0\n    while i < count {",
                "if i > 0 { children[i as usize] = data.span() }",
                "i += 1",
                FrontierRejection::UnsupportedLoopShape,
            ),
            (
                "let mut i: u256 = 1\n    while i < count {",
                "children[i as usize] = data.span()",
                "i += 1",
                FrontierRejection::MissingZeroBase,
            ),
            (
                "let mut i: u256 = 0\n    while i < count {",
                "children[i as usize] = data.span()",
                "i += 2",
                FrontierRejection::MissingIncrement,
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let source = format!(
                r#"
use core::ptr
fn fill{case}(_ count: u256) {{
    let mut children = ptr::MemArray<ptr::MemSpan>::new_uninit(count)
    {header}
        let data = ptr::MemBuffer::alloc(32)
        {store}
        {increment}
    }}
}}
"#
            );
            let checker = solved(&mut db, &source, &format!("fill{case}"));
            let rejection = frontier_candidates(checker.db, &checker.body)
                .unwrap()
                .into_iter()
                .next()
                .expect("one loop")
                .and_then(|candidate| checker.verify_frontier_structure(candidate))
                .map(|_| ())
                .unwrap_err();
            assert_eq!(rejection, expected, "{header} / {store} / {increment}");
            assert!(checker.prefix_certificates.is_empty());
        }
    }

    #[test]
    fn staged_span_second_reader_uses_certified_prefix() {
        let mut db = HirAnalysisTestDb::default();
        let mut checker = solved(
            &mut db,
            r#"
use core::ptr
fn stage_and_sum(_ cursor: mut u256, _ count: u256) -> u256 {
    let mut children = ptr::MemArray<ptr::MemSpan>::new_uninit(count)
    let mut i: u256 = 0
    while i < count {
        let data = ptr::MemBuffer::alloc(32)
        *ptr::cast<u8, u256>(data.ptr()) = 7
        children[i as usize] = data.span()
        i += 1
    }
    let mut j: u256 = 0
    let mut sum: u256 = 0
    while j < count {
        let child = children[j as usize]
        cursor += 1
        sum += *ptr::cast<u8, u256>(child.ptr())
        j += 1
    }
    sum
}
"#,
            "stage_and_sum",
        );
        let db = checker.db;
        let reader = checker
            .frontiers
            .iter()
            .find(|candidate| {
                !checker.body.blocks[candidate.body.index()]
                    .statements
                    .iter()
                    .any(|statement| checker.stores_capability(statement))
            })
            .unwrap()
            .clone();
        assert!(
            checker
                .scalar
                .selectors
                .contains(&checker.index(reader.header_value))
        );
        assert!(!checker.failed_prefix_certificates);
        let [certificate] = checker.prefix_certificates.as_slice() else {
            panic!("expected one fill-loop certificate");
        };
        let certificate = certificate.clone();
        assert!(
            checker.before[reader.loop_region.header.index()]
                .iter()
                .chain(&checker.before[reader.body.index()])
                .all(|state| state
                    .certified()
                    .iter()
                    .any(|certified| certified.family == certificate.family))
        );
        let (statement, result, selector) = indexed_reads(&checker, reader.body).next().unwrap();
        let state = checker.before[reader.body.index()][statement].clone();
        let member = certificate.family_scope.variables().next().unwrap();
        let substitution = IndexSubst::new(
            &certificate.family_scope,
            &BinderScope::default(),
            [(member, selector)],
        )
        .unwrap();
        assert!(
            state
                .guard()
                .implies(&certificate.coverage.substitute(&substitution).unwrap())
        );
        let selected = RegionSet::singleton(
            &BinderScope::default(),
            certificate.family.substitute(db, &substitution),
            RegionPath::default(),
        );
        let direct = state
            .read_region(
                db,
                &mut checker.inventory.values,
                &selected,
                certificate.contents.shape(),
                ValueOccurrence::Value(result),
            )
            .unwrap();
        let after = &checker.before[reader.body.index()][statement + 1];
        assert!(only_fresh_pointers(&checker, &direct, result), "typed read");
        assert!(
            only_fresh_pointers(&checker, after.value(result), result),
            "index result"
        );
    }
}
