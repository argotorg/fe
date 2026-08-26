use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            FieldIndex, Mutability, SConst, SemanticInstance, VariantIndex,
            get_or_build_semantic_instance,
            lower::{effect_param_site, enum_tag_ty},
            normalized::*,
            sem_const_ty,
        },
        ty::{
            adt_def::AdtRef,
            provider::{ProviderLayoutEvidence, provider_semantics},
            ty_check::EffectPassMode,
            ty_def::{BorrowKind, CapabilityKind, PrimTy, TyBase, TyData, TyId},
            ty_is_copy, ty_is_noesc,
        },
    },
    core::semantic::EffectEnvView,
    hir_def::{ArithBinOp, BinOp, UnOp},
};

use super::normalize::structural_types_are_boundary_compatible;

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
    InvalidReadMode,
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
        expected: BorrowKind,
        actual: Option<BorrowKind>,
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
        NExpr::Load { place, mode } => {
            if place.ty != result_ty {
                return Err(NormalizedBodyVerifyError::LoadType);
            }
            if *mode == ReadMode::Move
                && matches!(
                    place.base,
                    NPlaceBase::Root(root)
                        if body.root(root).is_some_and(|root| matches!(root.kind, NRootKind::Provider { .. }))
                )
            {
                return Err(NormalizedBodyVerifyError::InvalidReadMode);
            }
            Ok(())
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
            if matches!(kind, BorrowKind::Mut) {
                verify_mutation(db, body, place, false)?;
            }
            Ok(())
        }
        NExpr::Unary { op, value } => {
            verify_scalar_ty(db, body, *value)?;
            verify_scalar_result(db, result_ty)?;
            let value_ty = operand_ty(body, *value)?;
            let valid = match op {
                UnOp::Plus | UnOp::Minus | UnOp::BitNot => {
                    value_ty == result_ty && value_ty.is_integral(db)
                }
                UnOp::Not => value_ty.is_bool(db) && result_ty.is_bool(db),
                UnOp::Mut | UnOp::Ref => false,
            };
            valid
                .then_some(())
                .ok_or(NormalizedBodyVerifyError::ExpressionType)
        }
        NExpr::Binary { op, lhs, rhs } => {
            verify_scalar_ty(db, body, *lhs)?;
            verify_scalar_ty(db, body, *rhs)?;
            verify_scalar_result(db, result_ty)?;
            let lhs_ty = operand_ty(body, *lhs)?;
            let rhs_ty = operand_ty(body, *rhs)?;
            let valid = match op {
                BinOp::Arith(ArithBinOp::Range) => false,
                BinOp::Arith(op) => {
                    lhs_ty == rhs_ty
                        && lhs_ty == result_ty
                        && (lhs_ty.is_integral(db)
                            || lhs_ty.is_bool(db)
                                && matches!(
                                    op,
                                    ArithBinOp::BitAnd | ArithBinOp::BitOr | ArithBinOp::BitXor
                                ))
                }
                BinOp::Comp(_) => {
                    lhs_ty == rhs_ty
                        && result_ty.is_bool(db)
                        && (lhs_ty.is_integral(db) || lhs_ty.is_bool(db) || lhs_ty.is_string(db))
                }
                BinOp::Logical(_) | BinOp::Index => false,
            };
            valid
                .then_some(())
                .ok_or(NormalizedBodyVerifyError::ExpressionType)
        }
        NExpr::ScalarCast { value, to } => {
            verify_scalar_ty(db, body, *value)?;
            if *to != result_ty {
                return Err(NormalizedBodyVerifyError::ExpressionType);
            }
            verify_scalar_result(db, result_ty)
        }
        NExpr::Const(value) => {
            let value_ty = match value {
                SConst::Value(value) => sem_const_ty(db, *value),
                SConst::Ref(value) => value.ty(db),
            };
            let value_ty = body.owner.normalized_ty(db, value_ty);
            if matches!(value_ty.as_capability(db), Some((CapabilityKind::Mut, _)))
                || matches!(result_ty.as_capability(db), Some((CapabilityKind::Mut, _)))
            {
                return Err(NormalizedBodyVerifyError::ExpressionType);
            }
            let value_ty = capability_target_ty(db, value_ty);
            let result_ty = capability_target_ty(db, result_ty);
            type_is_boundary_compatible(db, body, value_ty, result_ty)
                .then_some(())
                .ok_or(NormalizedBodyVerifyError::ExpressionType)
        }
        NExpr::GetEnumTag { value } => {
            let enum_ty = capability_target_ty(db, operand_ty(body, *value)?);
            if enum_ty.as_enum(db).is_none() {
                return Err(NormalizedBodyVerifyError::ExpressionType);
            }
            let expected = enum_tag_ty(db, enum_ty);
            (result_ty == expected)
                .then_some(())
                .ok_or(NormalizedBodyVerifyError::ExpressionType)
        }
        NExpr::IsEnumVariant { value, variant } => {
            let enum_ty = capability_target_ty(db, operand_ty(body, *value)?);
            let adt = enum_ty
                .adt_def(db)
                .filter(|adt| matches!(adt.adt_ref(db), AdtRef::Enum(_)))
                .ok_or(NormalizedBodyVerifyError::ExpressionType)?;
            (result_ty.is_bool(db) && (variant.0 as usize) < adt.fields(db).len())
                .then_some(())
                .ok_or(NormalizedBodyVerifyError::ExpressionType)
        }
        NExpr::CodeRegionOffset { .. } | NExpr::CodeRegionLen { .. } => (result_ty
            == TyId::u256(db))
        .then_some(())
        .ok_or(NormalizedBodyVerifyError::ExpressionType),
        NExpr::Call {
            callee,
            args,
            effect_args,
            ..
        } => {
            let callee = get_or_build_semantic_instance(db, callee.key);
            let typed_body = callee.key(db).typed_body(db);
            let mut param_tys = Vec::new();
            while let Some(binding) = typed_body.param_binding(param_tys.len()) {
                param_tys.push(callee.normalized_binding_ty(db, binding));
            }
            if args.len() != param_tys.len() {
                return Err(NormalizedBodyVerifyError::ExpressionType);
            }
            for (arg, expected) in args.iter().zip(param_tys) {
                let actual = operand_ty(body, *arg)?;
                if !expected.has_invalid(db)
                    && !call_arg_type_is_compatible(db, body, actual, expected)
                {
                    return Err(NormalizedBodyVerifyError::ExpressionType);
                }
            }
            let requirements = effect_param_site(callee.key(db).owner(db))
                .map(|site| EffectEnvView::new(site).requirements(db))
                .unwrap_or_default();
            let mut effect_bindings = FxHashSet::default();
            for arg in effect_args {
                let Some(requirement) = requirements
                    .iter()
                    .find(|requirement| requirement.binding_idx == arg.binding_idx)
                else {
                    return Err(NormalizedBodyVerifyError::ExpressionType);
                };
                let shape_matches = matches!(
                    (&arg.pass_mode, &arg.arg),
                    (EffectPassMode::ByPlace, NEffectArgValue::Place(_))
                        | (
                            EffectPassMode::ByTempPlace | EffectPassMode::ByValue,
                            NEffectArgValue::Value(_)
                        )
                );
                if !effect_bindings.insert(arg.binding_idx)
                    || requirement.is_mut != arg.required_mut
                    || !shape_matches
                    || arg.target_ty.is_some_and(|ty| ty.has_invalid(db))
                {
                    return Err(NormalizedBodyVerifyError::ExpressionType);
                }
                let arg_ty = match &arg.arg {
                    NEffectArgValue::Place(place) => place.ty,
                    NEffectArgValue::Value(value) => operand_ty(body, *value)?,
                };
                let semantics = provider_semantics(
                    db,
                    body.template_owner.scope(),
                    body.owner.assumptions(db),
                    arg_ty,
                );
                if !matches!(semantics.evidence, ProviderLayoutEvidence::InvalidHandle(_))
                    && let Some(target_ty) = arg.target_ty
                    && !structural_types_are_boundary_compatible(
                        db,
                        body.owner,
                        semantics.target_ty.unwrap_or(arg_ty),
                        target_ty,
                    )
                {
                    return Err(NormalizedBodyVerifyError::ExpressionType);
                }
                if arg.required_mut
                    && let NEffectArgValue::Place(place) = &arg.arg
                {
                    verify_mutation(db, body, place, false)?;
                }
            }
            if effect_bindings.len() != requirements.len()
                && typed_body.smir_lowering_issues(db).is_empty()
            {
                return Err(NormalizedBodyVerifyError::ExpressionType);
            }
            let expected = callee.normalized_result_ty(db);
            let compatible = expected.has_invalid(db)
                || type_is_boundary_compatible(db, body, result_ty, expected)
                || expected.is_never(db);
            compatible
                .then_some(())
                .ok_or(NormalizedBodyVerifyError::ExpressionType)
        }
        NExpr::CodeRegionRef { .. } if ty_is_noesc(db, result_ty) => {
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
            } else if ty.is_tuple(db)
                || ty
                    .adt_def(db)
                    .is_some_and(|adt| matches!(adt.adt_ref(db), AdtRef::Struct(_)))
            {
                body.owner.normalized_field_types(db, *ty).as_slice() == field_tys
            } else {
                false
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
                .filter(|adt| matches!(adt.adt_ref(db), AdtRef::Enum(_)))
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
        NExpr::Forward { .. } | NExpr::CodeRegionRef { .. } => Ok(()),
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
                .filter(|adt| matches!(adt.adt_ref(db), AdtRef::Enum(_)))
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
            let actual = operand_ty(body, *value)?;
            let expected = body.owner.normalized_result_ty(db);
            let compatible =
                expected.has_invalid(db) || type_is_boundary_compatible(db, body, actual, expected);
            compatible
                .then_some(())
                .ok_or(NormalizedBodyVerifyError::OperandType)
        }
        NTerminatorKind::Return(None) => {
            if body.template_owner.body(db).is_some() {
                let expected = body.owner.normalized_result_ty(db);
                (expected == TyId::unit(db) || expected.has_invalid(db))
                    .then_some(())
                    .ok_or(NormalizedBodyVerifyError::OperandType)?;
            }
            Ok(())
        }
        NTerminatorKind::Assert { .. } => Ok(()),
    }
}

fn capability_target_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
    ty.as_capability(db).map_or(ty, |(_, target)| target)
}

fn type_is_boundary_compatible<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    actual: TyId<'db>,
    expected: TyId<'db>,
) -> bool {
    if actual == expected {
        return true;
    }
    if let Some((kind, actual)) = actual.as_capability(db)
        && expected.as_capability(db).is_none()
        && (kind == CapabilityKind::View
            || ty_is_copy(
                db,
                body.template_owner.scope(),
                actual,
                body.owner.assumptions(db),
            ))
    {
        return type_is_boundary_compatible(db, body, actual, expected);
    }
    match (actual.as_capability(db), expected.as_capability(db)) {
        (Some((actual_kind, actual)), Some((expected_kind, expected))) => {
            actual_kind == expected_kind
                && structural_types_are_boundary_compatible(db, body.owner, actual, expected)
        }
        (None, None) => structural_types_are_boundary_compatible(db, body.owner, actual, expected),
        (Some(_), None) | (None, Some(_)) => false,
    }
}

fn call_arg_type_is_compatible<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    actual: TyId<'db>,
    expected: TyId<'db>,
) -> bool {
    let Some((expected_kind, expected_target)) = expected.as_capability(db) else {
        return type_is_boundary_compatible(db, body, actual, expected);
    };
    if let Some((actual_kind, actual_target)) = actual.as_capability(db) {
        actual_kind.rank() >= expected_kind.rank()
            && type_is_boundary_compatible(db, body, actual_target, expected_target)
    } else {
        // Borrow boundaries can receive a value representation and select or
        // materialize its backing at runtime. The verifier owns the underlying
        // type contract, while explicit-borrow diagnostics remain a type-checking
        // concern.
        type_is_boundary_compatible(db, body, actual, expected_target)
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
                    .filter(|adt| matches!(adt.adt_ref(db), AdtRef::Enum(_)))
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
    let ty = operand_ty(body, operand)?;
    if ty_is_noesc(db, ty) {
        Err(NormalizedBodyVerifyError::ScalarOperandCapability(
            operand.value,
        ))
    } else if !ty_has_scalar_repr(db, ty) {
        Err(NormalizedBodyVerifyError::ExpressionType)
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
    } else if !ty_has_scalar_repr(db, result_ty) {
        Err(NormalizedBodyVerifyError::ExpressionType)
    } else {
        Ok(())
    }
}

fn ty_has_scalar_repr<'db>(db: &'db dyn HirAnalysisDb, mut ty: TyId<'db>) -> bool {
    let mut visiting = FxHashSet::default();
    while ty.is_tuple(db) || ty.is_struct(db) {
        if !visiting.insert(ty) {
            return false;
        }
        let fields = ty.field_types(db);
        if fields.len() != 1 {
            return false;
        }
        ty = fields[0];
    }
    ty.is_integral(db) || ty.is_bool(db) || ty.is_string(db)
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
        NPlaceBase::CapabilityTarget { carrier } => {
            capability == Some(CapabilityKind::Mut)
                || carrier_has_mutable_view_authority(db, body, carrier)
        }
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

fn carrier_has_mutable_view_authority<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    carrier: NValueId,
) -> bool {
    let Some(value) = body.value(carrier) else {
        return false;
    };
    if value.mutability != Mutability::Mutable {
        return false;
    }
    value_has_mutable_view_origin(db, body, carrier, &mut FxHashSet::default())
}

fn value_has_mutable_view_origin<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    value: NValueId,
    visiting: &mut FxHashSet<NValueId>,
) -> bool {
    if !visiting.insert(value) {
        return false;
    }
    let Some(value_data) = body.value(value) else {
        return false;
    };
    if value_data.mutability == Mutability::Mutable
        && value_data
            .ty
            .as_capability(db)
            .is_some_and(|(kind, _)| kind == CapabilityKind::View)
    {
        return true;
    }
    let NValueDefinition::Statement { block, statement } = value_data.definition else {
        return false;
    };
    let Some(NStatement {
        kind: NStatementKind::Define { expr, .. },
        ..
    }) = body
        .block(block)
        .and_then(|block| block.statements.get(statement as usize))
    else {
        return false;
    };
    let source = match expr {
        NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. } => Some(src.value),
        NExpr::ProjectValue { value, .. } => Some(value.value),
        NExpr::Load { place, .. } | NExpr::Borrow { place, .. } => match place.base {
            NPlaceBase::CapabilityTarget { carrier } => Some(carrier),
            NPlaceBase::Root(_) => None,
        },
        NExpr::CodeRegionRef { .. }
        | NExpr::Const(_)
        | NExpr::Unary { .. }
        | NExpr::Binary { .. }
        | NExpr::ScalarCast { .. }
        | NExpr::ArrayRepeat { .. }
        | NExpr::AggregateMake { .. }
        | NExpr::EnumMake { .. }
        | NExpr::GetEnumTag { .. }
        | NExpr::IsEnumVariant { .. }
        | NExpr::Call { .. }
        | NExpr::CodeRegionOffset { .. }
        | NExpr::CodeRegionLen { .. } => None,
    };
    source.is_some_and(|source| value_has_mutable_view_origin(db, body, source, visiting))
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
