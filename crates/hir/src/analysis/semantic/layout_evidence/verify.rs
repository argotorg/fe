use std::hash::Hash;

use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{NExpr, NSStmtKind, NSTerminatorKind, NormalizedSemanticBody, SConst, SLocalId},
    ty::{
        CallableLayoutParamPort, CallableLayoutPort, LayoutBundleComponent,
        LayoutBundleComponentKey, LayoutBundleInterface, LayoutMapTy,
        const_ty::ConstTyData,
        ty_def::{PrimTy, TyBase, TyData, TyId},
    },
};

use super::{
    LayoutEvidenceBody, LayoutEvidenceComponentValue, LayoutEvidenceConstBinding,
    LayoutEvidenceExpr, LayoutEvidenceIndex, LayoutEvidenceLocalId, LayoutEvidenceOperand,
    LayoutEvidenceStatement, LayoutEvidenceVerifyError, layout_const_param_uses,
};

fn operand_map_ty<'db>(
    body: &LayoutEvidenceBody<'db>,
    operand: &LayoutEvidenceOperand<'db>,
) -> Result<LayoutMapTy<'db>, LayoutEvidenceVerifyError> {
    match operand {
        LayoutEvidenceOperand::Local(local) => body
            .locals
            .get(local.index())
            .map(|local| local.map_ty.clone())
            .ok_or(LayoutEvidenceVerifyError::InvalidOperand(*local)),
        LayoutEvidenceOperand::Constant(value) => Ok(value.map_ty.clone()),
    }
}

fn verify_projection_indices(
    db: &dyn HirAnalysisDb,
    normalized: &NormalizedSemanticBody<'_>,
    indices: &[LayoutEvidenceIndex],
    dimensions: &[usize],
) -> Result<(), LayoutEvidenceVerifyError> {
    if indices.is_empty() || indices.len() > dimensions.len() {
        return Err(LayoutEvidenceVerifyError::InvalidProjection);
    }
    for (index, dimension) in indices.iter().zip(dimensions) {
        match index {
            LayoutEvidenceIndex::Constant(index) if *index >= *dimension => {
                return Err(LayoutEvidenceVerifyError::InvalidProjection);
            }
            LayoutEvidenceIndex::Dynamic(index)
                if !normalized.local(*index).is_some_and(|local| {
                    matches!(
                        local.ty.data(db),
                        TyData::TyBase(TyBase::Prim(PrimTy::Usize))
                    )
                }) =>
            {
                return Err(LayoutEvidenceVerifyError::InvalidIndexLocal(*index));
            }
            LayoutEvidenceIndex::Constant(_) | LayoutEvidenceIndex::Dynamic(_) => {}
        }
    }
    Ok(())
}

fn expr_map_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    normalized: &NormalizedSemanticBody<'db>,
    body: &LayoutEvidenceBody<'db>,
    expr: &LayoutEvidenceExpr<'db>,
    call_output: Option<&'db LayoutBundleInterface<'db>>,
    block: usize,
    statement: usize,
) -> Result<LayoutMapTy<'db>, LayoutEvidenceVerifyError> {
    match expr {
        LayoutEvidenceExpr::Use(operand) => operand_map_ty(body, operand),
        LayoutEvidenceExpr::Project { source, indices } => {
            let source_ty = operand_map_ty(body, source)?;
            verify_projection_indices(db, normalized, indices, &source_ty.dimensions)?;
            source_ty
                .projected(indices.len())
                .ok_or(LayoutEvidenceVerifyError::MapTypeMismatch)
        }
        LayoutEvidenceExpr::Array { elements } => {
            let Some(first) = elements.first() else {
                return Err(LayoutEvidenceVerifyError::EmptyArray);
            };
            let element_ty =
                expr_map_ty(db, normalized, body, first, call_output, block, statement)?;
            for element in &elements[1..] {
                let actual =
                    expr_map_ty(db, normalized, body, element, call_output, block, statement)?;
                if actual != element_ty {
                    return Err(LayoutEvidenceVerifyError::MapTypeMismatch);
                }
            }
            let mut dimensions = Vec::with_capacity(element_ty.dimensions.len() + 1);
            dimensions.push(elements.len());
            dimensions.extend_from_slice(&element_ty.dimensions);
            Ok(LayoutMapTy {
                scalar_ty: element_ty.scalar_ty,
                dimensions,
            })
        }
        LayoutEvidenceExpr::Repeat { len, element } => {
            let element_ty =
                expr_map_ty(db, normalized, body, element, call_output, block, statement)?;
            if *len == 0 {
                return Err(LayoutEvidenceVerifyError::MapTypeMismatch);
            }
            let mut dimensions = Vec::with_capacity(element_ty.dimensions.len() + 1);
            dimensions.push(*len);
            dimensions.extend_from_slice(&element_ty.dimensions);
            Ok(LayoutMapTy {
                scalar_ty: element_ty.scalar_ty,
                dimensions,
            })
        }
        LayoutEvidenceExpr::Update {
            source,
            indices,
            value,
        } => {
            let source_ty = operand_map_ty(body, source)?;
            verify_projection_indices(db, normalized, indices, &source_ty.dimensions)?;
            let value_ty = expr_map_ty(db, normalized, body, value, call_output, block, statement)?;
            if source_ty.projected(indices.len()).as_ref() != Some(&value_ty) {
                return Err(LayoutEvidenceVerifyError::MapTypeMismatch);
            }
            Ok(source_ty)
        }
        LayoutEvidenceExpr::CallResult { component } => call_output
            .filter(|output| output.is_runtime(*component))
            .and_then(|output| output.schema.component(*component))
            .map(|output| output.map_ty())
            .ok_or(LayoutEvidenceVerifyError::InvalidCallResult {
                block,
                statement,
                component: *component,
            }),
    }
}

fn expr_inputs(
    expr: &LayoutEvidenceExpr<'_>,
    evidence_locals: &mut Vec<LayoutEvidenceLocalId>,
    index_locals: &mut Vec<SLocalId>,
) {
    let push_operand = |locals: &mut Vec<LayoutEvidenceLocalId>,
                        operand: &LayoutEvidenceOperand<'_>| {
        if let LayoutEvidenceOperand::Local(local) = operand {
            locals.push(*local);
        }
    };
    let push_indices = |locals: &mut Vec<SLocalId>, indices: &[LayoutEvidenceIndex]| {
        locals.extend(indices.iter().filter_map(|index| match index {
            LayoutEvidenceIndex::Dynamic(index) => Some(*index),
            LayoutEvidenceIndex::Constant(_) => None,
        }));
    };
    match expr {
        LayoutEvidenceExpr::Use(operand) => push_operand(evidence_locals, operand),
        LayoutEvidenceExpr::Project { source, indices } => {
            push_operand(evidence_locals, source);
            push_indices(index_locals, indices);
        }
        LayoutEvidenceExpr::Array { elements } => {
            for element in elements {
                expr_inputs(element, evidence_locals, index_locals);
            }
        }
        LayoutEvidenceExpr::Repeat { element, .. } => {
            expr_inputs(element, evidence_locals, index_locals)
        }
        LayoutEvidenceExpr::Update {
            source,
            indices,
            value,
        } => {
            push_operand(evidence_locals, source);
            push_indices(index_locals, indices);
            expr_inputs(value, evidence_locals, index_locals);
        }
        LayoutEvidenceExpr::CallResult { .. } => {}
    }
}

fn const_binding_candidates<'db>(
    db: &'db dyn HirAnalysisDb,
    normalized: &NormalizedSemanticBody<'db>,
    body: &LayoutEvidenceBody<'db>,
    param: TyId<'db>,
) -> (Vec<LayoutEvidenceConstBinding<'db>>, bool) {
    let binding_matches =
        |component: &LayoutBundleComponent<'db>| component.supplied_const_params.contains(&param);
    let mut candidates = Vec::new();
    let mut is_layout_dependency = false;
    for (local_idx, local) in normalized.locals.iter().enumerate() {
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
    normalized: &NormalizedSemanticBody<'db>,
    body: &LayoutEvidenceBody<'db>,
    kind: &NSStmtKind<'db>,
    statement: &LayoutEvidenceStatement<'db>,
    block: usize,
    statement_idx: usize,
) -> Result<(), LayoutEvidenceVerifyError> {
    let uses = match kind {
        NSStmtKind::Assign {
            expr: NExpr::Const(SConst::Value(value)),
            ..
        } => layout_const_param_uses(db, *value),
        NSStmtKind::Assign { .. } | NSStmtKind::Store { .. } => Vec::new(),
    };
    let mut expected = Vec::new();
    for param in uses {
        let (candidates, is_layout_dependency) =
            const_binding_candidates(db, normalized, body, param);
        match candidates.as_slice() {
            [candidate] if operand_map_ty(body, &candidate.value)?.rank() == 0 => {
                expected.push(candidate.clone());
            }
            [_] => {
                return Err(LayoutEvidenceVerifyError::InvalidConstBinding {
                    block,
                    statement: statement_idx,
                });
            }
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
    if expected.len() != statement.const_bindings.len() {
        return Err(LayoutEvidenceVerifyError::InvalidConstBinding {
            block,
            statement: statement_idx,
        });
    }
    for (candidate, binding) in expected.iter().zip(&statement.const_bindings) {
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
            || operand_map_ty(body, &binding.value)?.scalar_ty != *scalar_ty
        {
            return Err(LayoutEvidenceVerifyError::InvalidConstBinding {
                block,
                statement: statement_idx,
            });
        }
    }
    Ok(())
}

fn successors(kind: &NSTerminatorKind<'_>) -> Vec<usize> {
    match kind {
        NSTerminatorKind::Goto(target) => vec![target.index()],
        NSTerminatorKind::Branch {
            then_bb, else_bb, ..
        } => vec![then_bb.index(), else_bb.index()],
        NSTerminatorKind::MatchEnum { cases, default, .. } => cases
            .iter()
            .map(|(_, target)| target.index())
            .chain(default.iter().map(|target| target.index()))
            .collect(),
        NSTerminatorKind::Assert { .. } | NSTerminatorKind::Return(_) => Vec::new(),
    }
}

fn definition_inputs<T: Copy + Eq + Hash>(
    all: FxHashSet<T>,
    params: FxHashSet<T>,
    defs: &[FxHashSet<T>],
    predecessors: &[Vec<usize>],
    reachable: &FxHashSet<usize>,
) -> Vec<FxHashSet<T>> {
    let mut inputs = vec![all; defs.len()];
    inputs[0] = params;
    loop {
        let outputs = inputs
            .iter()
            .zip(defs)
            .map(|(input, defs)| input.union(defs).copied().collect::<FxHashSet<_>>())
            .collect::<Vec<_>>();
        let mut changed = false;
        for block in 1..defs.len() {
            if !reachable.contains(&block) {
                continue;
            }
            let mut preds = predecessors[block]
                .iter()
                .filter(|predecessor| reachable.contains(predecessor));
            let Some(first) = preds.next() else {
                continue;
            };
            let mut input = outputs[*first].clone();
            for predecessor in preds {
                input.retain(|local| outputs[*predecessor].contains(local));
            }
            if inputs[block] != input {
                inputs[block] = input;
                changed = true;
            }
        }
        if !changed {
            return inputs;
        }
    }
}

fn verify_expr_definitions(
    expr: &LayoutEvidenceExpr<'_>,
    evidence: &FxHashSet<LayoutEvidenceLocalId>,
    semantic: &FxHashSet<SLocalId>,
    block: usize,
    statement: usize,
) -> Result<(), LayoutEvidenceVerifyError> {
    let mut index_locals = Vec::new();
    let mut evidence_locals = Vec::new();
    expr_inputs(expr, &mut evidence_locals, &mut index_locals);
    for local in index_locals {
        if !semantic.contains(&local) {
            return Err(LayoutEvidenceVerifyError::UndefinedIndexLocal {
                block,
                statement,
                local,
            });
        }
    }
    for local in evidence_locals {
        if !evidence.contains(&local) {
            return Err(LayoutEvidenceVerifyError::UndefinedLocal {
                block,
                statement: Some(statement),
                local,
            });
        }
    }
    Ok(())
}

fn verify_definitions(
    normalized: &NormalizedSemanticBody<'_>,
    body: &LayoutEvidenceBody<'_>,
) -> Result<(), LayoutEvidenceVerifyError> {
    if normalized.blocks.is_empty() {
        return Ok(());
    }
    let evidence_all = (0..body.locals.len())
        .map(|idx| LayoutEvidenceLocalId::from_u32(idx as u32))
        .collect::<FxHashSet<_>>();
    let evidence_defs = normalized
        .blocks
        .iter()
        .map(|block| {
            block
                .stmts
                .iter()
                .flat_map(|statement| {
                    body.statement(statement.id)
                        .expect("statement identity set was verified")
                        .assignments
                        .iter()
                        .map(|assignment| assignment.dst)
                })
                .collect::<FxHashSet<_>>()
        })
        .collect::<Vec<_>>();
    let semantic_all = (0..normalized.locals.len())
        .map(|idx| SLocalId::from_u32(idx as u32))
        .collect::<FxHashSet<_>>();
    let semantic_defs = normalized
        .blocks
        .iter()
        .map(|block| {
            block
                .stmts
                .iter()
                .filter_map(|statement| match statement.kind {
                    NSStmtKind::Assign { dst, .. } => Some(dst),
                    NSStmtKind::Store { .. } => None,
                })
                .collect::<FxHashSet<_>>()
        })
        .collect::<Vec<_>>();
    let mut predecessors = vec![Vec::new(); normalized.blocks.len()];
    let mut reachable = FxHashSet::from_iter([0]);
    let mut work = vec![0];
    while let Some(block) = work.pop() {
        for successor in successors(&normalized.blocks[block].terminator.kind) {
            if let Some(preds) = predecessors.get_mut(successor) {
                preds.push(block);
                if reachable.insert(successor) {
                    work.push(successor);
                }
            }
        }
    }
    let evidence_params = body.params.iter().copied().collect::<FxHashSet<_>>();
    let semantic_params = normalized
        .entry_locals
        .iter()
        .copied()
        .collect::<FxHashSet<_>>();
    let evidence_inputs = definition_inputs(
        evidence_all,
        evidence_params.clone(),
        &evidence_defs,
        &predecessors,
        &reachable,
    );
    let semantic_inputs = definition_inputs(
        semantic_all,
        semantic_params.clone(),
        &semantic_defs,
        &predecessors,
        &reachable,
    );
    for (block_idx, block) in normalized.blocks.iter().enumerate() {
        let mut defined_evidence = if reachable.contains(&block_idx) {
            evidence_inputs[block_idx].clone()
        } else {
            evidence_params.clone()
        };
        let mut defined_semantic = if reachable.contains(&block_idx) {
            semantic_inputs[block_idx].clone()
        } else {
            semantic_params.clone()
        };
        for (statement_idx, normalized_statement) in block.stmts.iter().enumerate() {
            let statement = body
                .statement(normalized_statement.id)
                .expect("statement identity set was verified");
            for binding in &statement.const_bindings {
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
            if let Some(call) = &statement.call {
                for arg in &call.args {
                    verify_expr_definitions(
                        &arg.value,
                        &defined_evidence,
                        &defined_semantic,
                        block_idx,
                        statement_idx,
                    )?;
                }
            }
            for assignment in &statement.assignments {
                verify_expr_definitions(
                    &assignment.expr,
                    &defined_evidence,
                    &defined_semantic,
                    block_idx,
                    statement_idx,
                )?;
                defined_evidence.insert(assignment.dst);
            }
            if let NSStmtKind::Assign { dst, .. } = normalized_statement.kind {
                defined_semantic.insert(dst);
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
    normalized: &NormalizedSemanticBody<'_>,
    body: &LayoutEvidenceBody<'_>,
) -> Result<(), LayoutEvidenceVerifyError> {
    let expected = normalized
        .blocks
        .iter()
        .map(|block| block.stmts.len())
        .sum();
    if body.statements.len() != expected {
        return Err(LayoutEvidenceVerifyError::StatementCount {
            expected,
            actual: body.statements.len(),
        });
    }
    let mut seen = FxHashSet::default();
    for (block, normalized_block) in normalized.blocks.iter().enumerate() {
        for (statement, normalized_statement) in normalized_block.stmts.iter().enumerate() {
            if body.statement(normalized_statement.id).is_none() {
                return Err(LayoutEvidenceVerifyError::InvalidStatementId {
                    block,
                    statement,
                    id: normalized_statement.id,
                });
            }
            if !seen.insert(normalized_statement.id) {
                return Err(LayoutEvidenceVerifyError::DuplicateStatementId(
                    normalized_statement.id,
                ));
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
    runtime: &NormalizedSemanticBody<'db>,
    body: &LayoutEvidenceBody<'db>,
) -> Result<(), LayoutEvidenceVerifyError> {
    if body.owner != runtime.owner {
        return Err(LayoutEvidenceVerifyError::OwnerMismatch);
    }
    if body.template_owner != runtime.template_owner {
        return Err(LayoutEvidenceVerifyError::TemplateOwnerMismatch);
    }
    if body.semantic_values.len() != runtime.locals.len() {
        return Err(LayoutEvidenceVerifyError::SemanticValueCount {
            expected: runtime.locals.len(),
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
        for (statement_idx, runtime_statement) in runtime_block.stmts.iter().enumerate() {
            let evidence_statement = body
                .statement(runtime_statement.id)
                .expect("statement identity set was verified");
            let runtime_callee = match &runtime_statement.kind {
                NSStmtKind::Assign {
                    expr: NExpr::Call { callee, .. },
                    ..
                } if callee
                    .key
                    .layout_bundle_signature(db)
                    .has_runtime_evidence() =>
                {
                    Some(*callee)
                }
                NSStmtKind::Assign { .. } | NSStmtKind::Store { .. } => None,
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
    Ok(())
}

pub fn verify_layout_evidence_body<'db>(
    db: &'db dyn HirAnalysisDb,
    normalized: &NormalizedSemanticBody<'db>,
    body: &LayoutEvidenceBody<'db>,
) -> Result<(), LayoutEvidenceVerifyError> {
    if body.owner != normalized.owner {
        return Err(LayoutEvidenceVerifyError::OwnerMismatch);
    }
    if body.template_owner != normalized.template_owner {
        return Err(LayoutEvidenceVerifyError::TemplateOwnerMismatch);
    }
    if body.semantic_values.len() != normalized.locals.len() {
        return Err(LayoutEvidenceVerifyError::SemanticValueCount {
            expected: normalized.locals.len(),
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
                    if value.map_ty != schema.map_ty()
                        || value.strides.len() != value.map_ty.rank()
                        || matches!(value.base, super::LayoutEvidenceBase::Root(root)
                            if root.const_ty_ty(db) != Some(value.map_ty.scalar_ty))
                        || !matches!(schema.representative, Some(LayoutBundleComponentKey::Static(expected))
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
                        || metadata.map_ty != schema.map_ty()
                        || metadata.param
                            != normalized.locals[local_idx]
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
                let local = normalized
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
                    || local.map_ty != param.component.map_ty()
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
        for (statement_idx, normalized_statement) in normalized_block.stmts.iter().enumerate() {
            let statement = body
                .statement(normalized_statement.id)
                .expect("statement identity set was verified");
            let (dst, call_signature) = match &normalized_statement.kind {
                NSStmtKind::Assign {
                    dst,
                    expr: NExpr::Call { callee, .. },
                } => (*dst, Some(callee.key.layout_bundle_signature(db))),
                NSStmtKind::Assign { dst, .. } => (*dst, None),
                NSStmtKind::Store { src, .. } => (src.local, None),
            };
            let call_output = call_signature.as_ref().map(|signature| &signature.output);
            verify_const_bindings(
                db,
                normalized,
                body,
                &normalized_statement.kind,
                statement,
                block_idx,
                statement_idx,
            )?;
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
                NSStmtKind::Assign {
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
                    let actual = expr_map_ty(
                        db,
                        normalized,
                        body,
                        &arg.value,
                        None,
                        block_idx,
                        statement_idx,
                    )?;
                    if arg.target != target || actual != expected.map_ty() {
                        return Err(LayoutEvidenceVerifyError::MapTypeMismatch);
                    }
                }
                let expected_results = signature.output.runtime_descriptor_count();
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
            if matches!(normalized_statement.kind, NSStmtKind::Assign { .. }) {
                let expected = dynamic_locals(&body.semantic_values[dst.index()]);
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
                if matches!(&normalized_statement.kind, NSStmtKind::Assign { .. })
                    && metadata.semantic_local != Some(dst)
                {
                    return Err(LayoutEvidenceVerifyError::InvalidAssignmentTarget {
                        block: block_idx,
                        statement: statement_idx,
                        local: assignment.dst,
                    });
                }
                let actual = expr_map_ty(
                    db,
                    normalized,
                    body,
                    &assignment.expr,
                    call_output,
                    block_idx,
                    statement_idx,
                )?;
                if actual != metadata.map_ty {
                    return Err(LayoutEvidenceVerifyError::MapTypeMismatch);
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
            NSTerminatorKind::Return(Some(value)) => Some(value.local),
            NSTerminatorKind::Goto(_)
            | NSTerminatorKind::Branch { .. }
            | NSTerminatorKind::MatchEnum { .. }
            | NSTerminatorKind::Assert { .. }
            | NSTerminatorKind::Return(None) => None,
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
            let actual = operand_map_ty(body, &evidence_return.value)?;
            if actual != expected.map_ty() {
                return Err(LayoutEvidenceVerifyError::MapTypeMismatch);
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
