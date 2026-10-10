use std::convert::Infallible;

use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{ForwardCfgAnalysis, JoinSemiLattice, solve_forward_cfg};
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SConst, SLocalId, SemanticBody,
        normalized::{
            NBlockId, NExpr, NLayoutLocals, NLayoutPlan, NStatementKind, NTerminatorKind,
            NormalizedBody,
        },
    },
    ty::{
        CallableLayoutParamPort, CallableLayoutPort, LayoutBundleComponent,
        LayoutBundleComponentKey, LayoutBundleInterface,
        const_ty::ConstTyData,
        ty_def::{TyData, TyId},
    },
};

use super::{
    LayoutEvidenceBody, LayoutEvidenceComponentValue, LayoutEvidenceConstBinding,
    LayoutEvidenceExpr, LayoutEvidenceLocalId, LayoutEvidenceOperand, LayoutEvidenceVerifyError,
    layout_const_param_uses,
};

fn operand_ty<'db>(
    body: &LayoutEvidenceBody<'db>,
    operand: &LayoutEvidenceOperand<'db>,
) -> Result<TyId<'db>, LayoutEvidenceVerifyError> {
    match operand {
        LayoutEvidenceOperand::Local(local) => body
            .locals
            .get(local.index())
            .map(|local| local.ty)
            .ok_or(LayoutEvidenceVerifyError::InvalidOperand(*local)),
        LayoutEvidenceOperand::Constant(value) => Ok(value.ty),
    }
}

fn expr_ty<'db>(
    body: &LayoutEvidenceBody<'db>,
    expr: &LayoutEvidenceExpr<'db>,
    call_output: Option<&'db LayoutBundleInterface<'db>>,
    block: usize,
    statement: usize,
) -> Result<TyId<'db>, LayoutEvidenceVerifyError> {
    match expr {
        LayoutEvidenceExpr::Use(operand) => operand_ty(body, operand),
        LayoutEvidenceExpr::CallResult { component } => call_output
            .filter(|output| output.is_runtime(*component))
            .and_then(|output| output.schema.component(*component))
            .map(|output| output.ty)
            .ok_or(LayoutEvidenceVerifyError::InvalidCallResult {
                block,
                statement,
                component: *component,
            }),
    }
}

fn const_binding_candidates<'db>(
    db: &'db dyn HirAnalysisDb,
    source: &SemanticBody<'db>,
    body: &LayoutEvidenceBody<'db>,
    param: TyId<'db>,
) -> (Vec<LayoutEvidenceConstBinding<'db>>, bool) {
    let binding_matches =
        |component: &LayoutBundleComponent<'db>| component.supplied_const_params.contains(&param);
    let mut candidates = Vec::new();
    let mut is_layout_dependency = false;
    for (local_idx, local) in source.locals.iter().enumerate() {
        let Some(origin) = local
            .source
            .and_then(|source| source.callable_input_origin(db))
        else {
            continue;
        };
        let value = &body.semantic_values[local_idx];
        for (component, value) in value.schema.components.iter().zip(&value.components) {
            is_layout_dependency |= component.dependent_const_params.contains(&param);
            if !binding_matches(component) {
                continue;
            }
            let value = match value {
                LayoutEvidenceComponentValue::Known(value) => {
                    LayoutEvidenceOperand::Constant(value.clone())
                }
                LayoutEvidenceComponentValue::Dynamic(local) => {
                    LayoutEvidenceOperand::Local(*local)
                }
            };
            candidates.push(LayoutEvidenceConstBinding {
                param,
                source: CallableLayoutParamPort::Input(CallableLayoutPort {
                    origin,
                    component: component.port.clone(),
                }),
                value,
            });
        }
    }
    let signature = body.owner.key(db).layout_bundle_signature(db);
    for (_, component) in signature.output_witnesses.runtime_components() {
        is_layout_dependency |= component.dependent_const_params.contains(&param);
        if !binding_matches(component) {
            continue;
        }
        let source = CallableLayoutParamPort::OutputWitness(component.port.clone());
        if let Some((idx, _)) = body
            .locals
            .iter()
            .enumerate()
            .find(|(_, local)| local.param.as_ref() == Some(&source))
        {
            candidates.push(LayoutEvidenceConstBinding {
                param,
                source,
                value: LayoutEvidenceOperand::Local(LayoutEvidenceLocalId::from_u32(idx as u32)),
            });
        }
    }
    (candidates, is_layout_dependency)
}

fn verify_const_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    source: &SemanticBody<'db>,
    body: &LayoutEvidenceBody<'db>,
    kind: &NStatementKind<'db>,
    bindings: &[LayoutEvidenceConstBinding<'db>],
    block: usize,
    statement_idx: usize,
) -> Result<(), LayoutEvidenceVerifyError> {
    let uses = match kind {
        NStatementKind::Define {
            expr: NExpr::Const(SConst::Evidence(value) | SConst::Description(value)),
            ..
        } => layout_const_param_uses(db, *value),
        NStatementKind::Define { .. } | NStatementKind::Store { .. } => Vec::new(),
    };
    let mut expected = Vec::new();
    for param in uses {
        let (candidates, is_layout_dependency) = const_binding_candidates(db, source, body, param);
        match candidates.as_slice() {
            [candidate] => expected.push(candidate.clone()),
            [] if !is_layout_dependency => {}
            [] => {
                return Err(LayoutEvidenceVerifyError::InvalidConstBinding {
                    block,
                    statement: statement_idx,
                });
            }
            [_, _, ..] => {
                return Err(LayoutEvidenceVerifyError::InvalidConstBinding {
                    block,
                    statement: statement_idx,
                });
            }
        }
    }
    if expected.len() != bindings.len() {
        return Err(LayoutEvidenceVerifyError::InvalidConstBinding {
            block,
            statement: statement_idx,
        });
    }
    for (candidate, binding) in expected.iter().zip(bindings) {
        let param = candidate.param;
        let TyData::ConstTy(const_ty) = param.data(db) else {
            return Err(LayoutEvidenceVerifyError::InvalidConstBinding {
                block,
                statement: statement_idx,
            });
        };
        let ConstTyData::TyParam(_, scalar_ty) = const_ty.data(db) else {
            return Err(LayoutEvidenceVerifyError::InvalidConstBinding {
                block,
                statement: statement_idx,
            });
        };
        if binding.param != param
            || candidate != binding
            || operand_ty(body, &binding.value)? != *scalar_ty
        {
            return Err(LayoutEvidenceVerifyError::InvalidConstBinding {
                block,
                statement: statement_idx,
            });
        }
    }
    Ok(())
}

fn block_successors(kind: &NTerminatorKind<'_>) -> Vec<NBlockId> {
    kind.successors()
        .into_iter()
        .map(|target| target.block)
        .collect()
}

#[derive(Clone, Default, PartialEq, Eq)]
struct DefinedLocals {
    reached: bool,
    evidence: FxHashSet<LayoutEvidenceLocalId>,
}

impl JoinSemiLattice for DefinedLocals {
    fn join_into(&mut self, other: &Self) -> bool {
        if !other.reached {
            return false;
        }
        if !self.reached {
            *self = other.clone();
            return true;
        }
        let before = self.evidence.len();
        self.evidence.retain(|local| other.evidence.contains(local));
        before != self.evidence.len()
    }
}

struct DefinitionAnalysis<'a, 'db> {
    normalized: &'a NormalizedBody<'db>,
    body: &'a LayoutEvidenceBody<'db>,
    successors: SecondaryMap<NBlockId, Vec<NBlockId>>,
}

impl<'a, 'db> DefinitionAnalysis<'a, 'db> {
    fn new(normalized: &'a NormalizedBody<'db>, body: &'a LayoutEvidenceBody<'db>) -> Self {
        let mut successors = SecondaryMap::new();
        successors.resize(normalized.blocks.len());
        for (idx, block) in normalized.blocks.iter().enumerate() {
            successors[NBlockId::new(idx)] = block_successors(&block.terminator.kind)
                .into_iter()
                .filter(|target| target.index() < normalized.blocks.len())
                .collect();
        }
        Self {
            normalized,
            body,
            successors,
        }
    }
}

impl ForwardCfgAnalysis for DefinitionAnalysis<'_, '_> {
    type Block = NBlockId;
    type State = DefinedLocals;
    type Error = Infallible;

    fn block_count(&self) -> usize {
        self.normalized.blocks.len()
    }

    fn seed_blocks(&self) -> Vec<Self::Block> {
        vec![self.normalized.entry]
    }

    fn bottom(&self) -> Self::State {
        DefinedLocals::default()
    }

    fn initialize(
        &mut self,
        entry_states: &mut SecondaryMap<Self::Block, Self::State>,
    ) -> Result<(), Self::Error> {
        let entry = &mut entry_states[self.normalized.entry];
        entry.reached = true;
        entry.evidence.extend(self.body.params.iter().copied());
        Ok(())
    }

    fn transfer(
        &mut self,
        block: Self::Block,
        in_state: &Self::State,
    ) -> Result<Self::State, Self::Error> {
        let mut state = in_state.clone();
        for statement in &self.normalized.blocks[block.index()].statements {
            state.evidence.extend(
                self.body
                    .statement(statement.id)
                    .expect("statement identity set was verified")
                    .assignments
                    .iter()
                    .map(|assignment| assignment.dst),
            );
        }
        Ok(state)
    }

    fn successors(&self, block: Self::Block) -> &[Self::Block] {
        &self.successors[block]
    }
}

fn verify_expr_definitions(
    expr: &LayoutEvidenceExpr<'_>,
    evidence: &FxHashSet<LayoutEvidenceLocalId>,
    block: usize,
    statement: usize,
) -> Result<(), LayoutEvidenceVerifyError> {
    match expr {
        LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Local(local))
            if !evidence.contains(local) =>
        {
            Err(LayoutEvidenceVerifyError::UndefinedLocal {
                block,
                statement: Some(statement),
                local: *local,
            })
        }
        LayoutEvidenceExpr::Use(_) | LayoutEvidenceExpr::CallResult { .. } => Ok(()),
    }
}

fn verify_definitions(
    normalized: &NormalizedBody<'_>,
    body: &LayoutEvidenceBody<'_>,
) -> Result<(), LayoutEvidenceVerifyError> {
    if normalized.blocks.is_empty() {
        return Ok(());
    }
    let entry_states = solve_forward_cfg(&mut DefinitionAnalysis::new(normalized, body));
    let unreachable = DefinedLocals {
        evidence: body.params.iter().copied().collect(),
        ..DefinedLocals::default()
    };
    for (block_idx, block) in normalized.blocks.iter().enumerate() {
        let entry = &entry_states[NBlockId::new(block_idx)];
        let entry = if entry.reached { entry } else { &unreachable };
        let mut defined_evidence = entry.evidence.clone();
        for (statement_idx, normalized_statement) in block.statements.iter().enumerate() {
            if let NStatementKind::Define { result, .. } = normalized_statement.kind {
                for binding in &body.constant_bindings[result.index()] {
                    if let LayoutEvidenceOperand::Local(local) = &binding.value
                        && !defined_evidence.contains(local)
                    {
                        return Err(LayoutEvidenceVerifyError::UndefinedLocal {
                            block: block_idx,
                            statement: Some(statement_idx),
                            local: *local,
                        });
                    }
                }
            }
            let statement = body
                .statement(normalized_statement.id)
                .expect("statement identity set was verified");
            if let Some(call) = &statement.call {
                for arg in &call.args {
                    verify_expr_definitions(
                        &arg.value,
                        &defined_evidence,
                        block_idx,
                        statement_idx,
                    )?;
                }
            }
            for assignment in &statement.assignments {
                verify_expr_definitions(
                    &assignment.expr,
                    &defined_evidence,
                    block_idx,
                    statement_idx,
                )?;
                defined_evidence.insert(assignment.dst);
            }
        }
        for local in body.terminators[block_idx]
            .returns
            .iter()
            .filter_map(|returned| match &returned.value {
                LayoutEvidenceOperand::Local(local) => Some(*local),
                LayoutEvidenceOperand::Constant(_) => None,
            })
        {
            if !defined_evidence.contains(&local) {
                return Err(LayoutEvidenceVerifyError::UndefinedLocal {
                    block: block_idx,
                    statement: None,
                    local,
                });
            }
        }
    }
    Ok(())
}

fn dynamic_locals(value: &super::LayoutEvidenceValue<'_>) -> Vec<LayoutEvidenceLocalId> {
    value
        .components
        .iter()
        .filter_map(|component| match component {
            LayoutEvidenceComponentValue::Known(_) => None,
            LayoutEvidenceComponentValue::Dynamic(local) => Some(*local),
        })
        .collect()
}

fn verify_statement_id_set(
    normalized: &NormalizedBody<'_>,
    body: &LayoutEvidenceBody<'_>,
) -> Result<(), LayoutEvidenceVerifyError> {
    let expected: usize = normalized
        .blocks
        .iter()
        .map(|block| block.statements.len())
        .sum();
    if body.statements.len() != expected {
        return Err(LayoutEvidenceVerifyError::StatementCount {
            expected,
            actual: body.statements.len(),
        });
    }
    let mut seen = FxHashSet::default();
    for (block, normalized_block) in normalized.blocks.iter().enumerate() {
        for (statement, normalized_statement) in normalized_block.statements.iter().enumerate() {
            let id = normalized_statement.id;
            if body.statement(id).is_none() {
                return Err(LayoutEvidenceVerifyError::InvalidStatementId {
                    block,
                    statement,
                    id,
                });
            }
            if !seen.insert(id) {
                return Err(LayoutEvidenceVerifyError::DuplicateStatementId(id));
            }
        }
    }
    Ok(())
}

/// Verifies the statement-identity contract between layout evidence and the
/// fully canonicalized runtime semantic body.
///
/// Layout evidence is derived from a non-folding semantic view so aggregate
/// construction remains visible. Runtime canonicalization may replace value
/// expressions, but it must preserve statement identity, and it may erase a
/// call only when that call has no runtime layout-evidence ABI.
pub fn verify_layout_evidence_runtime_compatibility<'db>(
    db: &'db dyn HirAnalysisDb,
    runtime: &NormalizedBody<'db>,
    layout_plan: &NLayoutPlan<'db>,
    source: &SemanticBody<'db>,
    body: &LayoutEvidenceBody<'db>,
) -> Result<(), LayoutEvidenceVerifyError> {
    let representations = NLayoutLocals::new(db, runtime, layout_plan, source);
    if body.owner != runtime.owner {
        return Err(LayoutEvidenceVerifyError::OwnerMismatch);
    }
    if body.template_owner != runtime.template_owner {
        return Err(LayoutEvidenceVerifyError::TemplateOwnerMismatch);
    }
    if body.semantic_values.len() != representations.locals.len() {
        return Err(LayoutEvidenceVerifyError::SemanticValueCount {
            expected: representations.locals.len(),
            actual: body.semantic_values.len(),
        });
    }
    if body.terminators.len() != runtime.blocks.len() {
        return Err(LayoutEvidenceVerifyError::BlockCount {
            expected: runtime.blocks.len(),
            actual: body.terminators.len(),
        });
    }
    verify_statement_id_set(runtime, body)?;
    for (block_idx, runtime_block) in runtime.blocks.iter().enumerate() {
        for (statement_idx, runtime_statement) in runtime_block.statements.iter().enumerate() {
            let evidence_statement = body
                .statement(runtime_statement.id)
                .expect("statement identity set was verified");
            let runtime_callee = match &runtime_statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { callee, .. },
                    ..
                } if callee
                    .key
                    .layout_bundle_signature(db)
                    .has_runtime_evidence() =>
                {
                    Some(*callee)
                }
                NStatementKind::Define { .. } | NStatementKind::Store { .. } => None,
            };
            if evidence_statement.call.is_some() != runtime_callee.is_some() {
                return Err(LayoutEvidenceVerifyError::CallPresence {
                    block: block_idx,
                    statement: statement_idx,
                });
            }
            if let (Some(call), Some(callee)) = (&evidence_statement.call, runtime_callee)
                && call.callee != callee
            {
                return Err(LayoutEvidenceVerifyError::CallCalleeMismatch {
                    block: block_idx,
                    statement: statement_idx,
                });
            }
        }
    }
    verify_definitions(runtime, body)
}

pub fn verify_layout_evidence_body<'db>(
    db: &'db dyn HirAnalysisDb,
    normalized: &NormalizedBody<'db>,
    layout_plan: &NLayoutPlan<'db>,
    source: &SemanticBody<'db>,
    body: &LayoutEvidenceBody<'db>,
) -> Result<(), LayoutEvidenceVerifyError> {
    let representations = NLayoutLocals::new(db, normalized, layout_plan, source);
    if body.constant_bindings.len() != normalized.values.len() {
        return Err(LayoutEvidenceVerifyError::ConstantBindingCount {
            expected: normalized.values.len(),
            actual: body.constant_bindings.len(),
        });
    }
    for (block_idx, block) in normalized.blocks.iter().enumerate() {
        for (statement_idx, statement) in block.statements.iter().enumerate() {
            let bindings = match statement.kind {
                NStatementKind::Define { result, .. } => {
                    body.constant_bindings[result.index()].as_ref()
                }
                NStatementKind::Store { .. } => &[],
            };
            verify_const_bindings(
                db,
                source,
                body,
                &statement.kind,
                bindings,
                block_idx,
                statement_idx,
            )?;
        }
    }

    if body.owner != normalized.owner {
        return Err(LayoutEvidenceVerifyError::OwnerMismatch);
    }
    if body.template_owner != normalized.template_owner {
        return Err(LayoutEvidenceVerifyError::TemplateOwnerMismatch);
    }
    if body.semantic_values.len() != representations.locals.len() {
        return Err(LayoutEvidenceVerifyError::SemanticValueCount {
            expected: representations.locals.len(),
            actual: body.semantic_values.len(),
        });
    }
    if body.terminators.len() != normalized.blocks.len() {
        return Err(LayoutEvidenceVerifyError::BlockCount {
            expected: normalized.blocks.len(),
            actual: body.terminators.len(),
        });
    }
    verify_statement_id_set(normalized, body)?;
    body.output
        .validate()
        .map_err(|error| LayoutEvidenceVerifyError::InvalidInterface { local: None, error })?;

    let mut referenced = FxHashSet::default();
    for (local_idx, value) in body.semantic_values.iter().enumerate() {
        let semantic_local = SLocalId::from_u32(local_idx as u32);
        value
            .schema
            .validate()
            .map_err(|error| LayoutEvidenceVerifyError::InvalidSchema {
                local: Some(semantic_local),
                error,
            })?;
        if value.components.len() != value.schema.components.len() {
            return Err(LayoutEvidenceVerifyError::ComponentValueCount {
                local: semantic_local,
                expected: value.schema.components.len(),
                actual: value.components.len(),
            });
        }
        for ((component_id, schema), component) in
            value.schema.indexed_components().zip(&value.components)
        {
            match component {
                LayoutEvidenceComponentValue::Known(value) => {
                    if value.ty != schema.ty
                        || matches!(value.base, super::LayoutEvidenceBase::Root(root)
                            if root.const_ty_ty(db) != Some(value.ty))
                        || !matches!(schema.representative, LayoutBundleComponentKey::Static(expected)
                            if value.base == super::LayoutEvidenceBase::Root(expected))
                    {
                        return Err(LayoutEvidenceVerifyError::InvalidComponentValue {
                            local: semantic_local,
                            component: component_id,
                        });
                    }
                }
                LayoutEvidenceComponentValue::Dynamic(local) => {
                    let Some(metadata) = body.locals.get(local.index()) else {
                        return Err(LayoutEvidenceVerifyError::InvalidEvidenceLocal(*local));
                    };
                    if metadata.semantic_local != Some(semantic_local)
                        || metadata.component != component_id
                        || metadata.ty != schema.ty
                        || metadata.param
                            != representations.locals[local_idx]
                                .source
                                .and_then(|source| source.callable_input_origin(db))
                                .map(|origin| {
                                    CallableLayoutParamPort::Input(CallableLayoutPort {
                                        origin,
                                        component: schema.port.clone(),
                                    })
                                })
                    {
                        return Err(LayoutEvidenceVerifyError::InvalidComponentValue {
                            local: semantic_local,
                            component: component_id,
                        });
                    }
                    if !referenced.insert(*local) {
                        return Err(LayoutEvidenceVerifyError::DuplicateEvidenceLocal(*local));
                    }
                }
            }
        }
    }
    let signature = body.owner.key(db).layout_bundle_signature(db);
    if body.output != signature.output {
        return Err(LayoutEvidenceVerifyError::OutputMismatch);
    }
    let mut expected_params = Vec::new();
    for param in signature.runtime_params() {
        let evidence_local = match &param.source {
            CallableLayoutParamPort::Input(port) => {
                let local = source
                    .locals
                    .iter()
                    .position(|local| {
                        local
                            .source
                            .and_then(|source| source.callable_input_origin(db))
                            == Some(port.origin)
                    })
                    .ok_or(LayoutEvidenceVerifyError::MissingInput(port.origin))?;
                let value = &body.semantic_values[local];
                if value
                    .schema
                    .component(param.component_id)
                    .is_none_or(|component| component.port != param.component.port)
                {
                    return Err(LayoutEvidenceVerifyError::InvalidParams);
                }
                match value.components.get(param.component_id.index()) {
                    Some(LayoutEvidenceComponentValue::Dynamic(local)) => *local,
                    Some(LayoutEvidenceComponentValue::Known(_)) | None => {
                        return Err(LayoutEvidenceVerifyError::InvalidParams);
                    }
                }
            }
            CallableLayoutParamPort::OutputWitness(_) => {
                let candidates = body
                    .locals
                    .iter()
                    .enumerate()
                    .filter(|(_, local)| local.param.as_ref() == Some(&param.source))
                    .collect::<Vec<_>>();
                let [(idx, local)] = candidates.as_slice() else {
                    return Err(LayoutEvidenceVerifyError::InvalidParams);
                };
                if local.semantic_local.is_some()
                    || local.component != param.component_id
                    || local.ty != param.component.ty
                {
                    return Err(LayoutEvidenceVerifyError::InvalidParams);
                }
                let local = LayoutEvidenceLocalId::from_u32(*idx as u32);
                if !referenced.insert(local) {
                    return Err(LayoutEvidenceVerifyError::DuplicateEvidenceLocal(local));
                }
                local
            }
        };
        expected_params.push(evidence_local);
    }
    if body.params != expected_params {
        return Err(LayoutEvidenceVerifyError::InvalidParams);
    }
    if let Some(local) = (0..body.locals.len())
        .map(|idx| LayoutEvidenceLocalId::from_u32(idx as u32))
        .find(|local| !referenced.contains(local))
    {
        return Err(LayoutEvidenceVerifyError::OrphanEvidenceLocal(local));
    }

    for (block_idx, normalized_block) in normalized.blocks.iter().enumerate() {
        for (statement_idx, normalized_statement) in normalized_block.statements.iter().enumerate()
        {
            let statement = body
                .statement(normalized_statement.id)
                .expect("statement identity set was verified");
            let (dst, call_signature, result_used) = match &normalized_statement.kind {
                NStatementKind::Define {
                    result,
                    expr: NExpr::Call { callee, .. },
                } => (
                    representations.value_local(*result).ok_or(
                        LayoutEvidenceVerifyError::UnmappedValue {
                            block: block_idx,
                            statement: statement_idx,
                        },
                    )?,
                    Some(callee.key.layout_bundle_signature(db)),
                    normalized.value_is_used(*result),
                ),
                NStatementKind::Define { result, .. } => (
                    representations.value_local(*result).ok_or(
                        LayoutEvidenceVerifyError::UnmappedValue {
                            block: block_idx,
                            statement: statement_idx,
                        },
                    )?,
                    None,
                    normalized.value_is_used(*result),
                ),
                NStatementKind::Store { value, .. } => (
                    representations.value_local(value.value).ok_or(
                        LayoutEvidenceVerifyError::UnmappedValue {
                            block: block_idx,
                            statement: statement_idx,
                        },
                    )?,
                    None,
                    true,
                ),
            };
            let call_output = call_signature.as_ref().map(|signature| &signature.output);
            if statement.call.is_some()
                != call_signature
                    .as_ref()
                    .is_some_and(|signature| signature.has_runtime_evidence())
            {
                return Err(LayoutEvidenceVerifyError::CallPresence {
                    block: block_idx,
                    statement: statement_idx,
                });
            }
            if let (
                NStatementKind::Define {
                    expr: NExpr::Call { callee, .. },
                    ..
                },
                Some(call),
            ) = (&normalized_statement.kind, &statement.call)
            {
                if call.callee != *callee {
                    return Err(LayoutEvidenceVerifyError::CallCalleeMismatch {
                        block: block_idx,
                        statement: statement_idx,
                    });
                }
                let signature = callee.key.layout_bundle_signature(db);
                let expected = signature
                    .runtime_params()
                    .map(|param| (param.source, param.component))
                    .collect::<Vec<_>>();
                if call.args.len() != expected.len() {
                    return Err(LayoutEvidenceVerifyError::CallArgCount {
                        block: block_idx,
                        statement: statement_idx,
                        expected: expected.len(),
                        actual: call.args.len(),
                    });
                }
                for (arg, (target, expected)) in call.args.iter().zip(expected) {
                    let actual = expr_ty(body, &arg.value, None, block_idx, statement_idx)?;
                    if arg.target != target || actual != expected.ty {
                        return Err(LayoutEvidenceVerifyError::RootTypeMismatch);
                    }
                }
                let expected_results = if result_used {
                    signature.output.runtime_descriptor_count()
                } else {
                    0
                };
                let actual_results = statement
                    .assignments
                    .iter()
                    .filter(|assignment| {
                        matches!(assignment.expr, LayoutEvidenceExpr::CallResult { .. })
                    })
                    .count();
                if actual_results != expected_results {
                    return Err(LayoutEvidenceVerifyError::CallResultCount {
                        block: block_idx,
                        statement: statement_idx,
                        expected: expected_results,
                        actual: actual_results,
                    });
                }
            }
            if matches!(normalized_statement.kind, NStatementKind::Define { .. }) {
                let expected = if result_used {
                    dynamic_locals(&body.semantic_values[dst.index()])
                } else {
                    Vec::new()
                };
                if statement.assignments.len() != expected.len() {
                    return Err(LayoutEvidenceVerifyError::AssignmentCount {
                        block: block_idx,
                        statement: statement_idx,
                        expected: expected.len(),
                        actual: statement.assignments.len(),
                    });
                }
                for (assignment, expected) in statement.assignments.iter().zip(expected) {
                    if assignment.dst != expected {
                        return Err(LayoutEvidenceVerifyError::InvalidAssignmentTarget {
                            block: block_idx,
                            statement: statement_idx,
                            local: assignment.dst,
                        });
                    }
                }
            }
            for assignment in &statement.assignments {
                let Some(metadata) = body.locals.get(assignment.dst.index()) else {
                    return Err(LayoutEvidenceVerifyError::InvalidAssignmentTarget {
                        block: block_idx,
                        statement: statement_idx,
                        local: assignment.dst,
                    });
                };
                if matches!(&normalized_statement.kind, NStatementKind::Define { .. })
                    && metadata.semantic_local != Some(dst)
                {
                    return Err(LayoutEvidenceVerifyError::InvalidAssignmentTarget {
                        block: block_idx,
                        statement: statement_idx,
                        local: assignment.dst,
                    });
                }
                let actual = expr_ty(
                    body,
                    &assignment.expr,
                    call_output,
                    block_idx,
                    statement_idx,
                )?;
                if actual != metadata.ty {
                    return Err(LayoutEvidenceVerifyError::RootTypeMismatch);
                }
                if let LayoutEvidenceExpr::CallResult { component } = assignment.expr {
                    let output = call_output
                        .filter(|output| output.is_runtime(component))
                        .and_then(|output| output.schema.component(component));
                    let destination = metadata
                        .semantic_local
                        .and_then(|local| body.semantic_values.get(local.index()))
                        .and_then(|value| value.schema.component(metadata.component));
                    if !matches!((output, destination), (Some(output), Some(destination))
                        if output.port == destination.port)
                    {
                        return Err(LayoutEvidenceVerifyError::InvalidCallResult {
                            block: block_idx,
                            statement: statement_idx,
                            component,
                        });
                    }
                }
            }
        }
        let returned_local = match normalized_block.terminator.kind {
            NTerminatorKind::Return(Some(value)) => representations.value_local(value.value),
            NTerminatorKind::Goto(_)
            | NTerminatorKind::Branch { .. }
            | NTerminatorKind::MatchEnum { .. }
            | NTerminatorKind::Assert { .. }
            | NTerminatorKind::Return(None) => None,
        };
        let expected_returns = returned_local.map_or(0, |_| body.output.runtime_descriptor_count());
        let terminator = body
            .terminator(crate::analysis::semantic::SBlockId::from_u32(
                block_idx as u32,
            ))
            .expect("block count was verified");
        if terminator.returns.len() != expected_returns {
            return Err(LayoutEvidenceVerifyError::ReturnCount {
                block: block_idx,
                expected: expected_returns,
                actual: terminator.returns.len(),
            });
        }
        let expected = body.output.runtime_components();
        for (evidence_return, (component_id, expected)) in terminator.returns.iter().zip(expected) {
            if evidence_return.component != component_id {
                return Err(LayoutEvidenceVerifyError::ReturnComponentMismatch {
                    block: block_idx,
                    component: component_id,
                });
            }
            if operand_ty(body, &evidence_return.value)? != expected.ty {
                return Err(LayoutEvidenceVerifyError::RootTypeMismatch);
            }
            if let Some(returned) = returned_local {
                let value = &body.semantic_values[returned.index()];
                let source = value
                    .schema
                    .component_by_port(&expected.port)
                    .and_then(|(id, _)| value.components.get(id.index()));
                let matches = match source {
                    Some(LayoutEvidenceComponentValue::Known(value)) => {
                        evidence_return.value == LayoutEvidenceOperand::Constant(value.clone())
                    }
                    Some(LayoutEvidenceComponentValue::Dynamic(local)) => {
                        evidence_return.value == LayoutEvidenceOperand::Local(*local)
                    }
                    None => false,
                };
                if !matches {
                    return Err(LayoutEvidenceVerifyError::ReturnComponentMismatch {
                        block: block_idx,
                        component: component_id,
                    });
                }
            }
        }
    }
    verify_definitions(normalized, body)
}
