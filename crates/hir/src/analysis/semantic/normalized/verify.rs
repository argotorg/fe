use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{FieldIndex, SemanticInstance, VariantIndex, normalized::*},
    ty::{
        adt_def::AdtRef,
        ty_def::{CapabilityKind, PrimTy, TyBase, TyData, TyId},
        ty_is_noesc,
    },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NormalizedBodyVerifyError {
    MissingEntry,
    MissingValue(NValueId),
    MissingRoot(NRootId),
    InvalidRoot(NRootId),
    MissingBlock(NBlockId),
    DefinitionMismatch(NValueId),
    DefinitionCount {
        value: NValueId,
        count: usize,
    },
    DuplicateEntryParam(u32),
    UseBeforeDefinition {
        value: NValueId,
        block: NBlockId,
    },
    SuccessorArity {
        block: NBlockId,
        expected: usize,
        actual: usize,
    },
    SuccessorType {
        block: NBlockId,
        index: usize,
    },
    InvalidPlaceBase,
    InvalidProjection,
    PlaceType,
    InvalidIndexType(NValueId),
    OperandType,
    ForwardType {
        result: NValueId,
        source: NValueId,
    },
    LoadType,
    StoreType {
        value: NValueId,
        destination: NPlaceBase,
    },
    ImmutableMutation {
        place: NPlaceBase,
        capability: Option<CapabilityKind>,
    },
    BorrowType {
        result: NValueId,
        expected: crate::analysis::ty::ty_def::BorrowKind,
        actual: Option<crate::analysis::ty::ty_def::BorrowKind>,
        target_matches: bool,
    },
    ExpressionType,
    ScalarCapability,
    ScalarOperandCapability(NValueId),
    InvalidRepack,
}

pub fn verify_normalized_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
) -> Result<(), NormalizedBodyVerifyError> {
    if body.block(body.entry).is_none() {
        return Err(NormalizedBodyVerifyError::MissingEntry);
    }

    let mut definitions = vec![0usize; body.values.len()];
    let mut entry_params = FxHashSet::default();
    for (index, value) in body.values.iter().enumerate() {
        let id = NValueId::new(index);
        match value.definition {
            NValueDefinition::EntryParam { param } => {
                if !entry_params.insert(param) {
                    return Err(NormalizedBodyVerifyError::DuplicateEntryParam(param));
                }
                definitions[index] += 1;
            }
            NValueDefinition::BlockParam { block, index } => {
                let block = body
                    .block(block)
                    .ok_or(NormalizedBodyVerifyError::MissingBlock(block))?;
                if block.params.get(index as usize) != Some(&id) {
                    return Err(NormalizedBodyVerifyError::DefinitionMismatch(id));
                }
            }
            NValueDefinition::Statement { block, statement } => {
                let block = body
                    .block(block)
                    .ok_or(NormalizedBodyVerifyError::MissingBlock(block))?;
                if !matches!(
                    block.statements.get(statement as usize),
                    Some(NStatement { kind: NStatementKind::Define { result, .. }, .. }) if *result == id
                ) {
                    return Err(NormalizedBodyVerifyError::DefinitionMismatch(id));
                }
            }
        }
    }

    for (index, root) in body.roots.iter().enumerate() {
        let root_id = NRootId::new(index);
        if root.ty.has_invalid(db) {
            return Err(NormalizedBodyVerifyError::InvalidPlaceBase);
        }
        if let NRootKind::CapabilityRepresentation { carrier } = root.kind {
            let carrier_ty = body
                .value(carrier)
                .ok_or(NormalizedBodyVerifyError::MissingValue(carrier))?
                .ty;
            if carrier_ty != root.ty || carrier_ty.as_capability(db).is_none() {
                return Err(NormalizedBodyVerifyError::InvalidRoot(root_id));
            }
        }
    }

    for block in &body.blocks {
        for param in &block.params {
            *definitions
                .get_mut(param.index())
                .ok_or(NormalizedBodyVerifyError::MissingValue(*param))? += 1;
        }
        for statement in &block.statements {
            match &statement.kind {
                NStatementKind::Define { result, expr } => {
                    *definitions
                        .get_mut(result.index())
                        .ok_or(NormalizedBodyVerifyError::MissingValue(*result))? += 1;
                    verify_expr(db, body, *result, expr)?;
                }
                NStatementKind::Store { destination, value } => {
                    verify_place(db, body, destination)?;
                    let source_ty = operand_ty(body, *value)?;
                    if source_ty != destination.ty {
                        return Err(NormalizedBodyVerifyError::StoreType {
                            value: value.value,
                            destination: destination.base,
                        });
                    }
                    verify_mutation(db, body, destination, true)?;
                }
            }
        }
        verify_terminator(db, body, &block.terminator.kind)?;
    }

    for (index, count) in definitions.into_iter().enumerate() {
        if count != 1 {
            return Err(NormalizedBodyVerifyError::DefinitionCount {
                value: NValueId::new(index),
                count,
            });
        }
    }
    verify_value_dominance(body)
}

fn verify_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    result: NValueId,
    expr: &NExpr<'db>,
) -> Result<(), NormalizedBodyVerifyError> {
    let result_ty = body
        .value(result)
        .ok_or(NormalizedBodyVerifyError::MissingValue(result))?
        .ty;
    let mut operand_error = None;
    expr.for_each_value_operand(|operand| {
        if operand_error.is_none() && body.value(operand.value).is_none() {
            operand_error = Some(NormalizedBodyVerifyError::MissingValue(operand.value));
        }
    });
    if let Some(error) = operand_error {
        return Err(error);
    }
    let mut place_error = None;
    expr.for_each_place_operand(|place| {
        if place_error.is_none() {
            place_error = verify_place(db, body, place).err();
        }
    });
    if let Some(error) = place_error {
        return Err(error);
    }

    match expr {
        NExpr::Forward { src } if operand_ty(body, *src)? != result_ty => {
            Err(NormalizedBodyVerifyError::ForwardType {
                result,
                source: src.value,
            })
        }
        NExpr::ProjectValue { value, path } => {
            let projected = project_path_ty(
                db,
                body.owner,
                &body.values,
                operand_ty(body, *value)?,
                &path.0,
            )?;
            (projected == result_ty)
                .then_some(())
                .ok_or(NormalizedBodyVerifyError::OperandType)
        }
        NExpr::Load { place, .. } if place.ty != result_ty => {
            Err(NormalizedBodyVerifyError::LoadType)
        }
        NExpr::Borrow { place, kind, .. } => {
            let result_borrow = result_ty.as_borrow(db);
            if result_borrow
                .is_none_or(|(result_kind, target)| result_kind != *kind || target != place.ty)
            {
                return Err(NormalizedBodyVerifyError::BorrowType {
                    result,
                    expected: *kind,
                    actual: result_borrow.map(|(kind, _)| kind),
                    target_matches: result_borrow.is_some_and(|(_, target)| target == place.ty),
                });
            }
            Ok(())
        }
        NExpr::Unary { value, .. } => {
            verify_scalar_ty(db, body, *value)?;
            verify_scalar_result(db, result_ty)
        }
        NExpr::Binary { lhs, rhs, .. } => {
            verify_scalar_ty(db, body, *lhs)?;
            verify_scalar_ty(db, body, *rhs)?;
            verify_scalar_result(db, result_ty)
        }
        NExpr::ScalarCast { value, to } => {
            verify_scalar_ty(db, body, *value)?;
            if *to != result_ty {
                return Err(NormalizedBodyVerifyError::ExpressionType);
            }
            verify_scalar_result(db, result_ty)
        }
        NExpr::GetEnumTag { .. }
        | NExpr::IsEnumVariant { .. }
        | NExpr::CodeRegionRef { .. }
        | NExpr::CodeRegionOffset { .. }
        | NExpr::CodeRegionLen { .. }
            if ty_is_noesc(db, result_ty) =>
        {
            Err(NormalizedBodyVerifyError::ScalarCapability)
        }
        NExpr::StructuralRepack { value, mapping } => {
            verify_repack(db, body, operand_ty(body, *value)?, result_ty, mapping)
        }
        NExpr::ArrayRepeat { ty, value } => {
            let (_, args) = ty.decompose_ty_app(db);
            if *ty != result_ty
                || !ty.is_array(db)
                || args.first().copied() != Some(operand_ty(body, *value)?)
            {
                return Err(NormalizedBodyVerifyError::ExpressionType);
            }
            Ok(())
        }
        NExpr::AggregateMake { ty, fields } => {
            let field_tys = fields
                .iter()
                .map(|field| operand_ty(body, *field))
                .collect::<Result<Vec<_>, _>>()?;
            let fields_match = if ty.is_array(db) {
                let element = ty
                    .decompose_ty_app(db)
                    .1
                    .first()
                    .copied()
                    .map(|element| body.owner.normalized_ty(db, element));
                ty.array_len(db) == Some(fields.len())
                    && element.is_some_and(|element| {
                        field_tys.iter().all(|field_ty| *field_ty == element)
                    })
            } else {
                body.owner.normalized_field_types(db, *ty).as_slice() == field_tys
            };
            if *ty != result_ty || !fields_match {
                return Err(NormalizedBodyVerifyError::ExpressionType);
            }
            Ok(())
        }
        NExpr::EnumMake {
            enum_ty,
            variant,
            fields,
        } => {
            let adt = enum_ty
                .adt_def(db)
                .ok_or(NormalizedBodyVerifyError::ExpressionType)?;
            let Some(expected) = adt.fields(db).get(variant.0 as usize) else {
                return Err(NormalizedBodyVerifyError::ExpressionType);
            };
            if *enum_ty != result_ty || expected.num_types() != fields.len() {
                return Err(NormalizedBodyVerifyError::ExpressionType);
            }
            for (index, field) in fields.iter().enumerate() {
                if operand_ty(body, *field)?
                    != body
                        .owner
                        .normalized_enum_variant_field_tys(db, *enum_ty, *variant)[index]
                {
                    return Err(NormalizedBodyVerifyError::ExpressionType);
                }
            }
            Ok(())
        }
        NExpr::Forward { .. }
        | NExpr::Load { .. }
        | NExpr::CodeRegionRef { .. }
        | NExpr::Const(_)
        | NExpr::GetEnumTag { .. }
        | NExpr::IsEnumVariant { .. }
        | NExpr::CodeRegionOffset { .. }
        | NExpr::CodeRegionLen { .. }
        | NExpr::Call { .. } => Ok(()),
    }
}

fn verify_terminator<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    terminator: &NTerminatorKind<'db>,
) -> Result<(), NormalizedBodyVerifyError> {
    match terminator {
        NTerminatorKind::Goto(target) => verify_successor(body, target),
        NTerminatorKind::Branch {
            cond,
            then_target,
            else_target,
        } => {
            if !operand_ty(body, *cond)?.is_bool(db) {
                return Err(NormalizedBodyVerifyError::OperandType);
            }
            verify_successor(body, then_target)?;
            verify_successor(body, else_target)
        }
        NTerminatorKind::MatchEnum {
            value,
            enum_ty,
            cases,
            default,
        } => {
            let value_ty = operand_ty(body, *value)?;
            let match_ty = value_ty
                .as_capability(db)
                .map_or(value_ty, |(_, target)| target);
            let adt = enum_ty
                .adt_def(db)
                .ok_or(NormalizedBodyVerifyError::OperandType)?;
            if match_ty != *enum_ty {
                return Err(NormalizedBodyVerifyError::OperandType);
            }
            let mut variants = FxHashSet::default();
            for (variant, target) in cases {
                if !variants.insert(*variant) || variant.0 as usize >= adt.fields(db).len() {
                    return Err(NormalizedBodyVerifyError::OperandType);
                }
                verify_successor(body, target)?;
            }
            if let Some(target) = default {
                verify_successor(body, target)?;
            }
            Ok(())
        }
        NTerminatorKind::Return(Some(value)) => {
            let _ = operand_ty(body, *value)?;
            Ok(())
        }
        NTerminatorKind::Assert { .. } | NTerminatorKind::Return(None) => {
            let _ = db;
            Ok(())
        }
    }
}

fn verify_successor(
    body: &NormalizedBody<'_>,
    successor: &NSuccessor,
) -> Result<(), NormalizedBodyVerifyError> {
    let block = body
        .block(successor.block)
        .ok_or(NormalizedBodyVerifyError::MissingBlock(successor.block))?;
    if block.params.len() != successor.args.len() {
        return Err(NormalizedBodyVerifyError::SuccessorArity {
            block: successor.block,
            expected: block.params.len(),
            actual: successor.args.len(),
        });
    }
    for (index, (param, arg)) in block.params.iter().zip(successor.args.iter()).enumerate() {
        let param_ty = body
            .value(*param)
            .ok_or(NormalizedBodyVerifyError::MissingValue(*param))?
            .ty;
        let arg_ty = operand_ty(body, *arg)?;
        if param_ty != arg_ty {
            return Err(NormalizedBodyVerifyError::SuccessorType {
                block: successor.block,
                index,
            });
        }
    }
    Ok(())
}

fn verify_place<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    place: &NPlace<'db>,
) -> Result<(), NormalizedBodyVerifyError> {
    let base_ty = body
        .place_base_ty(db, place.base)
        .ok_or(NormalizedBodyVerifyError::InvalidPlaceBase)?;
    let projected = project_path_ty(db, body.owner, &body.values, base_ty, &place.path)?;
    (projected == place.ty)
        .then_some(())
        .ok_or(NormalizedBodyVerifyError::PlaceType)
}

pub(crate) fn project_path_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    values: &[NValue<'db>],
    mut ty: TyId<'db>,
    path: &NDataPath,
) -> Result<TyId<'db>, NormalizedBodyVerifyError> {
    ty = instance.normalized_ty(db, ty);
    for projection in path.iter() {
        ty = match *projection {
            NDataProjection::Field(FieldIndex(field)) => instance
                .normalized_field_types(db, ty)
                .get(field as usize)
                .copied()
                .ok_or(NormalizedBodyVerifyError::InvalidProjection)?,
            NDataProjection::VariantField {
                variant: VariantIndex(variant),
                field: FieldIndex(field),
            } => {
                let adt = ty
                    .adt_def(db)
                    .ok_or(NormalizedBodyVerifyError::InvalidProjection)?;
                let variant = variant as usize;
                let field = field as usize;
                if adt
                    .fields(db)
                    .get(variant)
                    .is_none_or(|fields| field >= fields.num_types())
                {
                    return Err(NormalizedBodyVerifyError::InvalidProjection);
                }
                *instance
                    .normalized_enum_variant_field_tys(db, ty, VariantIndex(variant as u16))
                    .get(field)
                    .ok_or(NormalizedBodyVerifyError::InvalidProjection)?
            }
            NDataProjection::Index(index) => {
                match index {
                    NIndex::Const(index) => {
                        if ty.array_len(db).is_some_and(|len| index >= len) {
                            return Err(NormalizedBodyVerifyError::InvalidProjection);
                        }
                    }
                    NIndex::Value(value) => {
                        let index_ty = values
                            .get(value.index())
                            .ok_or(NormalizedBodyVerifyError::MissingValue(value))?
                            .ty;
                        if !matches!(
                            index_ty.data(db),
                            TyData::TyBase(TyBase::Prim(PrimTy::Usize))
                        ) {
                            return Err(NormalizedBodyVerifyError::InvalidIndexType(value));
                        }
                    }
                }
                let (_, args) = ty.decompose_ty_app(db);
                instance.normalized_ty(
                    db,
                    *args
                        .first()
                        .filter(|_| ty.is_array(db))
                        .ok_or(NormalizedBodyVerifyError::InvalidProjection)?,
                )
            }
        };
    }
    Ok(ty)
}

fn operand_ty<'db>(
    body: &NormalizedBody<'db>,
    operand: NOperand,
) -> Result<TyId<'db>, NormalizedBodyVerifyError> {
    body.value(operand.value)
        .map(|value| value.ty)
        .ok_or(NormalizedBodyVerifyError::MissingValue(operand.value))
}

fn verify_scalar_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    operand: NOperand,
) -> Result<(), NormalizedBodyVerifyError> {
    if ty_is_noesc(db, operand_ty(body, operand)?) {
        Err(NormalizedBodyVerifyError::ScalarOperandCapability(
            operand.value,
        ))
    } else {
        Ok(())
    }
}

fn verify_scalar_result(
    db: &dyn HirAnalysisDb,
    result_ty: TyId<'_>,
) -> Result<(), NormalizedBodyVerifyError> {
    if ty_is_noesc(db, result_ty) {
        Err(NormalizedBodyVerifyError::ScalarCapability)
    } else {
        Ok(())
    }
}

fn verify_mutation<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    place: &NPlace<'db>,
    allow_local_initialization: bool,
) -> Result<(), NormalizedBodyVerifyError> {
    let capability = match place.base {
        NPlaceBase::CapabilityTarget { carrier } => body
            .value(carrier)
            .and_then(|value| value.ty.as_capability(db))
            .map(|(kind, _)| kind),
        NPlaceBase::Root(_) => None,
    };
    let mutable = match place.base {
        NPlaceBase::Root(root) => {
            let root = body
                .root(root)
                .ok_or(NormalizedBodyVerifyError::MissingRoot(root))?;
            root.mutability == crate::analysis::semantic::Mutability::Mutable
                || (allow_local_initialization
                    && place.path.is_empty()
                    && matches!(root.kind, NRootKind::LocalSlot { .. }))
        }
        NPlaceBase::CapabilityTarget { .. } => capability == Some(CapabilityKind::Mut),
    };
    if mutable {
        Ok(())
    } else {
        Err(NormalizedBodyVerifyError::ImmutableMutation {
            place: place.base,
            capability,
        })
    }
}

fn verify_value_dominance(body: &NormalizedBody<'_>) -> Result<(), NormalizedBodyVerifyError> {
    let mut predecessors = vec![Vec::new(); body.blocks.len()];
    for (block_index, block) in body.blocks.iter().enumerate() {
        for successor in terminator_successors(&block.terminator.kind) {
            predecessors
                .get_mut(successor.index())
                .ok_or(NormalizedBodyVerifyError::MissingBlock(successor))?
                .push(NBlockId::new(block_index));
        }
    }
    let mut reachable = vec![false; body.blocks.len()];
    let mut pending = vec![body.entry];
    while let Some(block) = pending.pop() {
        let Some(is_reachable) = reachable.get_mut(block.index()) else {
            return Err(NormalizedBodyVerifyError::MissingBlock(block));
        };
        if *is_reachable {
            continue;
        }
        *is_reachable = true;
        pending.extend(terminator_successors(
            &body.blocks[block.index()].terminator.kind,
        ));
    }
    let reachable_blocks = reachable
        .iter()
        .enumerate()
        .filter_map(|(block, reachable)| reachable.then_some(NBlockId::new(block)))
        .collect::<FxHashSet<_>>();
    let mut dominators = reachable
        .iter()
        .enumerate()
        .map(|(block, reachable)| {
            if !reachable || block == body.entry.index() {
                FxHashSet::from_iter([NBlockId::new(block)])
            } else {
                reachable_blocks.clone()
            }
        })
        .collect::<Vec<_>>();
    loop {
        let mut changed = false;
        for block_index in 0..body.blocks.len() {
            if !reachable[block_index] || block_index == body.entry.index() {
                continue;
            }
            let mut incoming = predecessors[block_index]
                .iter()
                .filter(|predecessor| reachable[predecessor.index()]);
            let mut next = incoming
                .next()
                .map(|predecessor| dominators[predecessor.index()].clone())
                .unwrap_or_default();
            for predecessor in incoming {
                next.retain(|dominator| dominators[predecessor.index()].contains(dominator));
            }
            next.insert(NBlockId::new(block_index));
            if next != dominators[block_index] {
                dominators[block_index] = next;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    for (block_index, block) in body.blocks.iter().enumerate() {
        let block_id = NBlockId::new(block_index);
        for (statement_index, statement) in block.statements.iter().enumerate() {
            let mut uses = Vec::new();
            match &statement.kind {
                NStatementKind::Define { expr, .. } => {
                    expr.for_each_value_operand(|operand| uses.push(operand.value));
                    expr.for_each_place_operand(|place| {
                        collect_place_values(body, place, &mut uses)
                    });
                }
                NStatementKind::Store { destination, value } => {
                    uses.push(value.value);
                    collect_place_values(body, destination, &mut uses);
                }
            }
            for value in uses {
                verify_value_dominates_use(body, &dominators, value, block_id, statement_index)?;
            }
        }
        let mut uses = Vec::new();
        collect_terminator_values(&block.terminator.kind, &mut uses);
        for value in uses {
            verify_value_dominates_use(body, &dominators, value, block_id, block.statements.len())?;
        }
    }
    Ok(())
}

fn terminator_successors(terminator: &NTerminatorKind<'_>) -> Vec<NBlockId> {
    match terminator {
        NTerminatorKind::Goto(target) => vec![target.block],
        NTerminatorKind::Branch {
            then_target,
            else_target,
            ..
        } => vec![then_target.block, else_target.block],
        NTerminatorKind::MatchEnum { cases, default, .. } => cases
            .iter()
            .map(|(_, target)| target.block)
            .chain(default.iter().map(|target| target.block))
            .collect(),
        NTerminatorKind::Assert { .. } | NTerminatorKind::Return(_) => Vec::new(),
    }
}

fn collect_place_values(body: &NormalizedBody<'_>, place: &NPlace<'_>, values: &mut Vec<NValueId>) {
    match place.base {
        NPlaceBase::CapabilityTarget { carrier } => values.push(carrier),
        NPlaceBase::Root(root) => {
            if let Some(NRoot {
                kind: NRootKind::CapabilityRepresentation { carrier },
                ..
            }) = body.root(root)
            {
                values.push(*carrier);
            }
        }
    }
    values.extend(place.path.iter().filter_map(|projection| match projection {
        NDataProjection::Index(NIndex::Value(value)) => Some(*value),
        NDataProjection::Field(_)
        | NDataProjection::VariantField { .. }
        | NDataProjection::Index(NIndex::Const(_)) => None,
    }));
}

fn collect_terminator_values(terminator: &NTerminatorKind<'_>, values: &mut Vec<NValueId>) {
    match terminator {
        NTerminatorKind::Goto(target) => {
            values.extend(target.args.iter().map(|argument| argument.value));
        }
        NTerminatorKind::Branch {
            cond,
            then_target,
            else_target,
        } => {
            values.push(cond.value);
            values.extend(then_target.args.iter().map(|argument| argument.value));
            values.extend(else_target.args.iter().map(|argument| argument.value));
        }
        NTerminatorKind::MatchEnum {
            value,
            cases,
            default,
            ..
        } => {
            values.push(value.value);
            for (_, target) in cases {
                values.extend(target.args.iter().map(|argument| argument.value));
            }
            if let Some(target) = default {
                values.extend(target.args.iter().map(|argument| argument.value));
            }
        }
        NTerminatorKind::Return(Some(value)) => values.push(value.value),
        NTerminatorKind::Assert { .. } | NTerminatorKind::Return(None) => {}
    }
}

fn verify_value_dominates_use(
    body: &NormalizedBody<'_>,
    dominators: &[FxHashSet<NBlockId>],
    value: NValueId,
    use_block: NBlockId,
    use_statement: usize,
) -> Result<(), NormalizedBodyVerifyError> {
    let definition = body
        .value(value)
        .ok_or(NormalizedBodyVerifyError::MissingValue(value))?
        .definition;
    let dominates = match definition {
        NValueDefinition::EntryParam { .. } => true,
        NValueDefinition::BlockParam { block, .. } => {
            block == use_block || dominators[use_block.index()].contains(&block)
        }
        NValueDefinition::Statement { block, statement } => {
            if block == use_block {
                (statement as usize) < use_statement
            } else {
                dominators[use_block.index()].contains(&block)
            }
        }
    };
    if dominates {
        Ok(())
    } else {
        Err(NormalizedBodyVerifyError::UseBeforeDefinition {
            value,
            block: use_block,
        })
    }
}

fn verify_repack<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    source: TyId<'db>,
    target: TyId<'db>,
    mapping: &StructuralRepack,
) -> Result<(), NormalizedBodyVerifyError> {
    let mut expected_sources = Vec::new();
    collect_structural_leaves(
        db,
        body.owner,
        source,
        NDataPath::empty(),
        &mut expected_sources,
        &mut FxHashSet::default(),
    )?;
    let mut expected_targets = Vec::new();
    collect_structural_leaves(
        db,
        body.owner,
        target,
        NDataPath::empty(),
        &mut expected_targets,
        &mut FxHashSet::default(),
    )?;
    let mut sources = FxHashSet::default();
    let mut targets = FxHashSet::default();
    for (target_path, source_path) in &mapping.fields {
        if !sources.insert(source_path.clone()) || !targets.insert(target_path.clone()) {
            return Err(NormalizedBodyVerifyError::InvalidRepack);
        }
        let source_ty = project_path_ty(db, body.owner, &body.values, source, source_path)?;
        let target_ty = project_path_ty(db, body.owner, &body.values, target, target_path)?;
        if source_ty != target_ty {
            return Err(NormalizedBodyVerifyError::InvalidRepack);
        }
    }
    (sources == FxHashSet::from_iter(expected_sources)
        && targets == FxHashSet::from_iter(expected_targets))
    .then_some(())
    .ok_or(NormalizedBodyVerifyError::InvalidRepack)
}

fn collect_structural_leaves<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    mut ty: TyId<'db>,
    path: NDataPath,
    leaves: &mut Vec<NDataPath>,
    visiting: &mut FxHashSet<TyId<'db>>,
) -> Result<(), NormalizedBodyVerifyError> {
    ty = instance.normalized_ty(db, ty);
    if !visiting.insert(ty) {
        return Ok(());
    }
    if ty.as_capability(db).is_some() {
        leaves.push(path);
    } else if ty.is_array(db) {
        if ty.array_len(db) != Some(0) {
            let element = ty
                .decompose_ty_app(db)
                .1
                .first()
                .copied()
                .ok_or(NormalizedBodyVerifyError::InvalidRepack)?;
            collect_structural_leaves(
                db,
                instance,
                instance.normalized_ty(db, element),
                path.appended(NDataProjection::Index(NIndex::Const(0))),
                leaves,
                visiting,
            )?;
        }
    } else if let Some(adt) = ty.adt_def(db)
        && matches!(adt.adt_ref(db), AdtRef::Enum(_))
    {
        for (variant, fields) in adt.fields(db).iter().enumerate() {
            for field in 0..fields.num_types() {
                collect_structural_leaves(
                    db,
                    instance,
                    instance.normalized_enum_variant_field_tys(
                        db,
                        ty,
                        VariantIndex(variant as u16),
                    )[field],
                    path.appended(NDataProjection::VariantField {
                        variant: VariantIndex(variant as u16),
                        field: FieldIndex(field as u16),
                    }),
                    leaves,
                    visiting,
                )?;
            }
        }
    } else {
        let fields = instance.normalized_field_types(db, ty);
        if fields.is_empty() {
            if !ty.is_zero_sized(db) {
                leaves.push(path);
            }
        } else {
            for (field, field_ty) in fields.into_iter().enumerate() {
                collect_structural_leaves(
                    db,
                    instance,
                    *field_ty,
                    path.appended(NDataProjection::Field(FieldIndex(field as u16))),
                    leaves,
                    visiting,
                )?;
            }
        }
    }
    visiting.remove(&ty);
    Ok(())
}
