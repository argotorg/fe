use cranelift_entity::EntityRef;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{NOperand, NSLocal, SLocalId, SemOrigin, SemanticInstance, SemanticLocalKind},
    },
    projection::{IndexSource, Projection},
};

use super::{
    diagnostics::{normalized_body_internal_diag, operand_origin},
    ir::{
        NBorrowRoot, NExpr, NSPlace, NSPlaceRoot, NSStmtKind, NSTerminator, NSTerminatorKind,
        NormalizedBindingLowering, NormalizedSemanticBody, ReadMode, SemanticBorrowDiagnostic,
        local_has_runtime_move_semantics,
    },
};

pub fn verify_normalized_semantic_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedSemanticBody<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    for (local_idx, local) in body.locals.iter().enumerate() {
        let local_id = SLocalId::from_u32(local_idx as u32);
        let verify_rooted_place = |place: &NSPlace<'db>, label: &str| {
            if let Some(root) = place.root.borrow_root() {
                if body.root(root).is_none() {
                    return Err(normalized_body_internal_diag(
                        db,
                        instance,
                        body,
                        SemOrigin::Body(body.template_owner),
                        format!("{label} {} has missing borrow root", local_id.index()),
                    ));
                }
            } else if !matches!(place.root, NSPlaceRoot::CarrierDerefLocal(_)) {
                return Err(normalized_body_internal_diag(
                    db,
                    instance,
                    body,
                    SemOrigin::Body(body.template_owner),
                    format!("{label} {} has missing borrow root", local_id.index()),
                ));
            }
            Ok(())
        };
        match (&local.facts.interface, &local.lowering) {
            (SemanticLocalKind::Erased, NormalizedBindingLowering::Erased)
            | (SemanticLocalKind::DirectValue, NormalizedBindingLowering::ValueLocal { .. })
            | (
                SemanticLocalKind::PlaceBoundValue,
                NormalizedBindingLowering::PlaceBoundValue { .. },
            )
            | (
                SemanticLocalKind::PlaceCarrier | SemanticLocalKind::DirectCarrier,
                NormalizedBindingLowering::CarrierLocal { .. },
            ) => {}
            _ => {
                return Err(normalized_body_internal_diag(
                    db,
                    instance,
                    body,
                    SemOrigin::Body(body.template_owner),
                    format!(
                        "normalized local {} has mismatched interface/lowering: {:?} vs {:?}",
                        local_id.index(),
                        local.facts.interface,
                        local.lowering,
                    ),
                ));
            }
        }
        match &local.lowering {
            NormalizedBindingLowering::ValueLocal { place } => {
                verify_rooted_place(place, "value local")?;
            }
            NormalizedBindingLowering::PlaceBoundValue { place, .. } => {
                verify_rooted_place(place, "place-bound local")?;
            }
            NormalizedBindingLowering::CarrierLocal { root, .. } => {
                if let Some(root) = root
                    && body.root(*root).is_none()
                {
                    return Err(normalized_body_internal_diag(
                        db,
                        instance,
                        body,
                        SemOrigin::Body(body.template_owner),
                        format!("carrier local {} has missing borrow root", local_id.index()),
                    ));
                }
            }
            NormalizedBindingLowering::Erased => {}
        }
        if let Some(place) = local.snapshot_source_place() {
            verify_rooted_place(place, "snapshot source place for local")?;
        }
    }

    for block in &body.blocks {
        for stmt in &block.stmts {
            match &stmt.kind {
                NSStmtKind::Assign { dst, expr } => {
                    verify_local_exists(db, instance, body, stmt.origin, *dst)?;
                    verify_expr(db, instance, body, stmt.origin, expr)?;
                }
                NSStmtKind::Store { dst, src } => {
                    verify_place(db, instance, body, stmt.origin, dst)?;
                    verify_local_exists(db, instance, body, stmt.origin, src.local)?;
                }
            }
        }
        verify_terminator(db, instance, body, &block.terminator)?;
    }
    Ok(())
}

fn verify_terminator<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedSemanticBody<'db>,
    term: &NSTerminator<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    match &term.kind {
        NSTerminatorKind::Goto(bb) => {
            if body.block(*bb).is_none() {
                return Err(normalized_body_internal_diag(
                    db,
                    instance,
                    body,
                    term.origin,
                    format!("missing normalized block {}", bb.index()),
                ));
            }
        }
        NSTerminatorKind::Branch {
            cond,
            then_bb,
            else_bb,
        } => {
            verify_operand(db, instance, body, term.origin, *cond)?;
            if body.block(*then_bb).is_none() || body.block(*else_bb).is_none() {
                return Err(normalized_body_internal_diag(
                    db,
                    instance,
                    body,
                    term.origin,
                    "branch target is missing".to_string(),
                ));
            }
        }
        NSTerminatorKind::MatchEnum {
            value,
            cases,
            default,
            ..
        } => {
            verify_operand(db, instance, body, term.origin, *value)?;
            if cases.iter().any(|(_, bb)| body.block(*bb).is_none())
                || default.is_some_and(|bb| body.block(bb).is_none())
            {
                return Err(normalized_body_internal_diag(
                    db,
                    instance,
                    body,
                    term.origin,
                    "match target is missing".to_string(),
                ));
            }
        }
        NSTerminatorKind::Return(Some(value)) => {
            verify_operand(db, instance, body, term.origin, *value)?;
        }
        NSTerminatorKind::Assert { .. } => {}
        NSTerminatorKind::Return(None) => {}
    }
    Ok(())
}

fn verify_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedSemanticBody<'db>,
    origin: SemOrigin<'db>,
    expr: &NExpr<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    expr.try_for_each_value_operand(|value| verify_operand(db, instance, body, origin, value))?;
    expr.try_for_each_place_operand(|place| verify_place(db, instance, body, origin, place))?;
    if let NExpr::ReadPlace {
        place,
        mode: ReadMode::Move,
    } = expr
        && !place_move_is_valid(body, place)
    {
        return Err(normalized_body_internal_diag(
            db,
            instance,
            body,
            origin,
            "move read is invalid for this normalized place".to_string(),
        ));
    }
    Ok(())
}

fn verify_operand<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedSemanticBody<'db>,
    origin: SemOrigin<'db>,
    operand: NOperand,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    let origin = operand_origin(operand, origin);
    let local = verify_local_exists(db, instance, body, origin, operand.local)?;
    if operand.mode == ReadMode::Move
        && !local_has_runtime_move_semantics(db, local, &body.borrow_roots)
    {
        return Err(normalized_body_internal_diag(
            db,
            instance,
            body,
            origin,
            format!(
                "move read is invalid for normalized local {}",
                operand.local.index()
            ),
        ));
    }
    Ok(())
}

fn verify_local_exists<'db, 'a>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &'a NormalizedSemanticBody<'db>,
    origin: SemOrigin<'db>,
    local: SLocalId,
) -> Result<&'a NSLocal<'db>, SemanticBorrowDiagnostic<'db>> {
    body.local(local).ok_or_else(|| {
        normalized_body_internal_diag(
            db,
            instance,
            body,
            origin,
            format!("missing normalized local {}", local.index()),
        )
    })
}

fn verify_place<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedSemanticBody<'db>,
    origin: SemOrigin<'db>,
    place: &NSPlace<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    match place.root {
        NSPlaceRoot::Root(root) => {
            if body.root(root).is_none() {
                return Err(normalized_body_internal_diag(
                    db,
                    instance,
                    body,
                    origin,
                    format!("missing normalized borrow root {}", root.index()),
                ));
            }
        }
        NSPlaceRoot::CarrierDerefLocal(local) => {
            let local = verify_local_exists(db, instance, body, origin, local)?;
            if !matches!(
                local.lowering,
                NormalizedBindingLowering::CarrierLocal { .. }
            ) {
                return Err(normalized_body_internal_diag(
                    db,
                    instance,
                    body,
                    origin,
                    "carrier-deref place root does not reference a carrier local".to_string(),
                ));
            }
        }
    }
    for proj in place.path.iter() {
        if let Projection::Index(IndexSource::Dynamic(index)) = proj {
            verify_local_exists(db, instance, body, origin, *index)?;
        }
    }
    Ok(())
}

fn place_move_is_valid<'db>(body: &NormalizedSemanticBody<'db>, place: &NSPlace<'db>) -> bool {
    match place.root {
        NSPlaceRoot::Root(root) => match body.root(root) {
            Some(NBorrowRoot::Param { local, .. }) | Some(NBorrowRoot::LocalSlot { local }) => {
                body.local(*local).is_some_and(|local| {
                    matches!(
                        local.lowering,
                        NormalizedBindingLowering::ValueLocal { .. }
                            | NormalizedBindingLowering::PlaceBoundValue { .. }
                    )
                })
            }
            Some(NBorrowRoot::Provider { .. }) => false,
            None => false,
        },
        NSPlaceRoot::CarrierDerefLocal(_) => true,
    }
}
