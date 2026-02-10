use hir::analysis::HirAnalysisDb;
use hir::analysis::ty::ty_def::TyId;
use hir::projection::Projection;

use crate::core_lib::CoreLib;
use crate::ir::{
    AddressSpaceKind, LocalData, LocalId, MirBackend, MirBody, MirInst, MirProjectionPath,
    MirStage, Place, Rvalue, SourceInfoId, ValueData, ValueId, ValueOrigin, ValueRepr,
};
use crate::{layout, repr};

fn alloc_local<'db>(locals: &mut Vec<LocalData<'db>>, data: LocalData<'db>) -> LocalId {
    let id = LocalId(locals.len() as u32);
    locals.push(data);
    id
}

fn alloc_value<'db>(values: &mut Vec<ValueData<'db>>, data: ValueData<'db>) -> ValueId {
    let id = ValueId(values.len() as u32);
    values.push(data);
    id
}

fn resolve_place_root_local<'db>(values: &[ValueData<'db>], mut value: ValueId) -> Option<LocalId> {
    loop {
        match &values.get(value.index())?.origin {
            ValueOrigin::PlaceRoot(local) => return Some(*local),
            ValueOrigin::TransparentCast { value: inner } => value = *inner,
            _ => return None,
        }
    }
}

fn apply_transparent_field0_chain<'db>(
    db: &'db dyn HirAnalysisDb,
    mut base_ty: TyId<'db>,
    projection: &MirProjectionPath<'db>,
) -> Option<TyId<'db>> {
    for proj in projection.iter() {
        let Projection::Field(0) = proj else {
            return None;
        };
        base_ty = repr::transparent_newtype_field_ty(db, base_ty)?;
    }
    Some(base_ty)
}

fn repr_for_plain_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
    fallback_space: AddressSpaceKind,
) -> ValueRepr {
    match repr::repr_kind_for_ty(db, core, ty) {
        repr::ReprKind::Zst | repr::ReprKind::Word => ValueRepr::Word,
        repr::ReprKind::Ptr(space) => ValueRepr::Ptr(space),
        repr::ReprKind::Ref => ValueRepr::Ref(fallback_space),
    }
}

fn place_base_spill_owner<'db>(
    values: &[ValueData<'db>],
    owner_for_spill_local: &[Option<LocalId>],
    mut base: ValueId,
) -> Option<LocalId> {
    loop {
        let origin = &values.get(base.index())?.origin;
        match origin {
            ValueOrigin::TransparentCast { value } => base = *value,
            ValueOrigin::Local(local) => {
                return owner_for_spill_local.get(local.index()).copied()?;
            }
            ValueOrigin::PlaceRef(place) => base = place.base,
            _ => return None,
        }
    }
}

fn rewrite_place_root_value<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    values: &mut Vec<ValueData<'db>>,
    locals: &[LocalData<'db>],
    value: ValueId,
) -> Option<ValueId> {
    let local = resolve_place_root_local(values, value)?;
    let target_ty = values[value.index()].ty;
    let local_ty = locals[local.index()].ty;
    let local_repr = repr_for_plain_ty(db, core, local_ty, locals[local.index()].address_space);
    let local_value = alloc_value(
        values,
        ValueData {
            ty: local_ty,
            origin: ValueOrigin::Local(local),
            source: SourceInfoId::SYNTHETIC,
            repr: local_repr,
        },
    );
    (target_ty == local_ty).then_some(local_value).or_else(|| {
        Some(alloc_value(
            values,
            ValueData {
                ty: target_ty,
                origin: ValueOrigin::TransparentCast { value: local_value },
                source: SourceInfoId::SYNTHETIC,
                repr: local_repr,
            },
        ))
    })
}

pub(crate) fn lower_capability_to_repr<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    backend: MirBackend,
    body: &mut MirBody<'db>,
) {
    body.assert_stage(MirStage::Capability);

    let initial_values_len = body.values.len();

    let desired_repr: Vec<ValueRepr> = {
        let mut memo: Vec<Option<ValueRepr>> = vec![None; initial_values_len];

        fn origin_address_space<'db>(
            values: &[ValueData<'db>],
            locals: &[LocalData<'db>],
            origin: &ValueOrigin<'db>,
        ) -> Option<AddressSpaceKind> {
            match origin {
                ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) => {
                    locals.get(local.index()).map(|l| l.address_space)
                }
                ValueOrigin::PlaceRef(place) => {
                    crate::ir::try_value_address_space_in(values, locals, place.base)
                }
                ValueOrigin::MoveOut { place } => {
                    crate::ir::try_value_address_space_in(values, locals, place.base)
                }
                ValueOrigin::TransparentCast { value } => {
                    origin_address_space(values, locals, &values.get(value.index())?.origin)
                }
                _ => None,
            }
        }

        fn compute<'db>(
            db: &'db dyn HirAnalysisDb,
            core: &CoreLib<'db>,
            values: &[ValueData<'db>],
            locals: &[LocalData<'db>],
            memo: &mut [Option<ValueRepr>],
            value: ValueId,
        ) -> ValueRepr {
            if let Some(repr) = memo.get(value.index()).and_then(|slot| *slot) {
                return repr;
            }

            let data = &values[value.index()];
            let repr = match &data.origin {
                ValueOrigin::TransparentCast { value: inner } => {
                    compute(db, core, values, locals, memo, *inner)
                }
                _ if data.ty.as_capability(db).is_some() => {
                    match repr::repr_kind_for_ty(db, core, data.ty) {
                        repr::ReprKind::Zst | repr::ReprKind::Word => ValueRepr::Word,
                        repr::ReprKind::Ptr(space_kind) => {
                            if matches!(space_kind, AddressSpaceKind::Memory)
                                && let Some(space) =
                                    origin_address_space(values, locals, &data.origin)
                            {
                                ValueRepr::Ptr(space)
                            } else {
                                ValueRepr::Ptr(space_kind)
                            }
                        }
                        repr::ReprKind::Ref => {
                            let space = origin_address_space(values, locals, &data.origin)
                                .unwrap_or(AddressSpaceKind::Memory);
                            ValueRepr::Ref(space)
                        }
                    }
                }
                _ => data.repr,
            };

            memo[value.index()] = Some(repr);
            repr
        }

        (0..initial_values_len)
            .map(|idx| {
                compute(
                    db,
                    core,
                    &body.values,
                    &body.locals,
                    &mut memo,
                    ValueId(idx as u32),
                )
            })
            .collect()
    };

    let mut locals_need_spill = vec![false; body.locals.len()];

    for (idx, value) in body.values.iter().enumerate().take(initial_values_len) {
        let (ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place }) = &value.origin else {
            continue;
        };
        let Some(local) = resolve_place_root_local(&body.values, place.base) else {
            continue;
        };

        let is_word_capability_place =
            value.ty.as_capability(db).is_some() && desired_repr[idx].address_space().is_none();
        if !is_word_capability_place {
            locals_need_spill[local.index()] = true;
        }
    }

    for block in &body.blocks {
        for inst in &block.insts {
            let mut check_place = |place: &Place<'db>| {
                let Some(local) = resolve_place_root_local(&body.values, place.base) else {
                    return;
                };
                if place.projection.is_empty() {
                    return;
                }
                let base_ty = body.local(local).ty;
                if apply_transparent_field0_chain(db, base_ty, &place.projection).is_none() {
                    locals_need_spill[local.index()] = true;
                }
            };
            match inst {
                MirInst::Assign {
                    rvalue: Rvalue::Load { place },
                    ..
                } => check_place(place),
                MirInst::Store { place, .. }
                | MirInst::InitAggregate { place, .. }
                | MirInst::SetDiscriminant { place, .. } => check_place(place),
                MirInst::Assign { .. } | MirInst::BindValue { .. } => {}
            }
        }
    }

    let spill_local_for_owner: Vec<Option<LocalId>> = {
        let mut mapping = vec![None; body.locals.len()];
        let locals = &mut body.locals;
        for (idx, needs_spill) in locals_need_spill.iter().copied().enumerate() {
            if !needs_spill {
                continue;
            }
            let owner = LocalId(idx as u32);
            let owner_data = &locals[owner.index()];
            let spill = alloc_local(
                locals,
                LocalData {
                    name: format!("spill{}", owner.index()),
                    ty: owner_data.ty,
                    is_mut: false,
                    source: SourceInfoId::SYNTHETIC,
                    address_space: AddressSpaceKind::Memory,
                },
            );
            body.spill_slots.insert(owner, spill);
            mapping[owner.index()] = Some(spill);
        }
        mapping
    };

    let owner_for_spill_local: Vec<Option<LocalId>> = {
        let mut mapping = vec![None; body.locals.len()];
        for (&owner, &spill) in &body.spill_slots {
            if mapping.len() <= spill.index() {
                mapping.resize(body.locals.len(), None);
            }
            mapping[spill.index()] = Some(owner);
        }
        mapping
    };

    let mut spill_addr_value_for_owner: Vec<Option<ValueId>> = vec![None; body.locals.len()];
    for (owner_idx, spill_local) in spill_local_for_owner.iter().copied().enumerate() {
        let Some(spill_local) = spill_local else {
            continue;
        };
        let owner = LocalId(owner_idx as u32);
        let owner_ty = body.local(owner).ty;
        let spill_value = alloc_value(
            &mut body.values,
            ValueData {
                ty: owner_ty,
                origin: ValueOrigin::Local(spill_local),
                source: SourceInfoId::SYNTHETIC,
                repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            },
        );
        spill_addr_value_for_owner[owner.index()] = Some(spill_value);
    }

    for idx in 0..initial_values_len {
        body.values[idx].repr = desired_repr[idx];
    }

    for idx in 0..initial_values_len {
        let desired = desired_repr[idx];
        let is_capability = body.values[idx].ty.as_capability(db).is_some();
        let origin = body.values[idx].origin.clone();
        let new_origin = match origin {
            ValueOrigin::PlaceRef(mut place) => {
                if let Some(local) = resolve_place_root_local(&body.values, place.base) {
                    if is_capability && desired.address_space().is_none() {
                        let local_ty = body.local(local).ty;
                        let projected_ty =
                            apply_transparent_field0_chain(db, local_ty, &place.projection)
                                .unwrap_or(local_ty);
                        let local_repr =
                            repr_for_plain_ty(db, core, local_ty, body.local(local).address_space);
                        let local_value = alloc_value(
                            &mut body.values,
                            ValueData {
                                ty: local_ty,
                                origin: ValueOrigin::Local(local),
                                source: SourceInfoId::SYNTHETIC,
                                repr: local_repr,
                            },
                        );
                        let source_value = if projected_ty == local_ty {
                            local_value
                        } else {
                            alloc_value(
                                &mut body.values,
                                ValueData {
                                    ty: projected_ty,
                                    origin: ValueOrigin::TransparentCast { value: local_value },
                                    source: SourceInfoId::SYNTHETIC,
                                    repr: local_repr,
                                },
                            )
                        };
                        ValueOrigin::TransparentCast {
                            value: source_value,
                        }
                    } else {
                        let spill_base = spill_addr_value_for_owner[local.index()].expect(
                            "missing spill slot for PlaceRoot-backed PlaceRef value (repr pass bug)",
                        );
                        place.base = spill_base;
                        ValueOrigin::PlaceRef(place)
                    }
                } else {
                    ValueOrigin::PlaceRef(place)
                }
            }
            ValueOrigin::MoveOut { mut place } => {
                if let Some(local) = resolve_place_root_local(&body.values, place.base) {
                    if is_capability && desired.address_space().is_none() {
                        let local_ty = body.local(local).ty;
                        let projected_ty =
                            apply_transparent_field0_chain(db, local_ty, &place.projection)
                                .unwrap_or(local_ty);
                        let local_repr =
                            repr_for_plain_ty(db, core, local_ty, body.local(local).address_space);
                        let local_value = alloc_value(
                            &mut body.values,
                            ValueData {
                                ty: local_ty,
                                origin: ValueOrigin::Local(local),
                                source: SourceInfoId::SYNTHETIC,
                                repr: local_repr,
                            },
                        );
                        let source_value = if projected_ty == local_ty {
                            local_value
                        } else {
                            alloc_value(
                                &mut body.values,
                                ValueData {
                                    ty: projected_ty,
                                    origin: ValueOrigin::TransparentCast { value: local_value },
                                    source: SourceInfoId::SYNTHETIC,
                                    repr: local_repr,
                                },
                            )
                        };
                        ValueOrigin::TransparentCast {
                            value: source_value,
                        }
                    } else {
                        let spill_base = spill_addr_value_for_owner[local.index()].expect(
                            "missing spill slot for PlaceRoot-backed MoveOut value (repr pass bug)",
                        );
                        place.base = spill_base;
                        ValueOrigin::MoveOut { place }
                    }
                } else {
                    ValueOrigin::MoveOut { place }
                }
            }
            other => other,
        };
        body.values[idx].origin = new_origin;
    }

    let (blocks, values, locals) = (&mut body.blocks, &mut body.values, &body.locals);
    for block in blocks {
        let mut rewritten: Vec<MirInst<'db>> = Vec::with_capacity(block.insts.len());
        for inst in std::mem::take(&mut block.insts) {
            match inst {
                MirInst::Assign {
                    source,
                    dest,
                    rvalue: Rvalue::Load { place },
                } => {
                    if let Some(local) = resolve_place_root_local(values, place.base) {
                        let loaded_ty = dest
                            .map(|dest| locals[dest.index()].ty)
                            .unwrap_or(values[place.base.index()].ty);
                        let base_ty = locals[local.index()].ty;
                        let Some(projected_ty) =
                            apply_transparent_field0_chain(db, base_ty, &place.projection)
                        else {
                            let Some(spill_base) = spill_addr_value_for_owner
                                .get(local.index())
                                .copied()
                                .flatten()
                            else {
                                rewritten.push(MirInst::Assign {
                                    source,
                                    dest,
                                    rvalue: Rvalue::Load { place },
                                });
                                continue;
                            };
                            rewritten.push(MirInst::Assign {
                                source,
                                dest,
                                rvalue: Rvalue::Load {
                                    place: Place::new(spill_base, place.projection),
                                },
                            });
                            continue;
                        };

                        if projected_ty != loaded_ty {
                            let Some(spill_base) = spill_addr_value_for_owner
                                .get(local.index())
                                .copied()
                                .flatten()
                            else {
                                rewritten.push(MirInst::Assign {
                                    source,
                                    dest,
                                    rvalue: Rvalue::Load { place },
                                });
                                continue;
                            };
                            rewritten.push(MirInst::Assign {
                                source,
                                dest,
                                rvalue: Rvalue::Load {
                                    place: Place::new(spill_base, place.projection),
                                },
                            });
                            continue;
                        }

                        let local_repr = repr_for_plain_ty(
                            db,
                            core,
                            base_ty,
                            locals[local.index()].address_space,
                        );
                        let local_value = alloc_value(
                            values,
                            ValueData {
                                ty: base_ty,
                                origin: ValueOrigin::Local(local),
                                source: SourceInfoId::SYNTHETIC,
                                repr: local_repr,
                            },
                        );
                        let loaded_value = if loaded_ty == base_ty {
                            local_value
                        } else {
                            alloc_value(
                                values,
                                ValueData {
                                    ty: loaded_ty,
                                    origin: ValueOrigin::TransparentCast { value: local_value },
                                    source: SourceInfoId::SYNTHETIC,
                                    repr: local_repr,
                                },
                            )
                        };

                        rewritten.push(MirInst::Assign {
                            source,
                            dest,
                            rvalue: Rvalue::Value(loaded_value),
                        });
                        continue;
                    }

                    rewritten.push(MirInst::Assign {
                        source,
                        dest,
                        rvalue: Rvalue::Load { place },
                    });

                    if let Some(dest) = dest
                        && spill_local_for_owner
                            .get(dest.index())
                            .copied()
                            .flatten()
                            .is_some()
                        && !layout::is_zero_sized_ty(db, locals[dest.index()].ty)
                    {
                        let spill_base = spill_addr_value_for_owner[dest.index()].unwrap();
                        let repr = repr_for_plain_ty(
                            db,
                            core,
                            locals[dest.index()].ty,
                            locals[dest.index()].address_space,
                        );
                        let value = alloc_value(
                            values,
                            ValueData {
                                ty: locals[dest.index()].ty,
                                origin: ValueOrigin::Local(dest),
                                source: SourceInfoId::SYNTHETIC,
                                repr,
                            },
                        );
                        rewritten.push(MirInst::Store {
                            source: SourceInfoId::SYNTHETIC,
                            place: Place::new(spill_base, MirProjectionPath::new()),
                            value,
                        });
                    }
                }
                MirInst::Assign {
                    source,
                    dest: Some(dest),
                    rvalue,
                } if spill_local_for_owner
                    .get(dest.index())
                    .copied()
                    .flatten()
                    .is_some()
                    && !layout::is_zero_sized_ty(db, locals[dest.index()].ty) =>
                {
                    rewritten.push(MirInst::Assign {
                        source,
                        dest: Some(dest),
                        rvalue: rvalue.clone(),
                    });
                    let spill_base = spill_addr_value_for_owner[dest.index()].unwrap();
                    let repr = repr_for_plain_ty(
                        db,
                        core,
                        locals[dest.index()].ty,
                        locals[dest.index()].address_space,
                    );
                    let value = alloc_value(
                        values,
                        ValueData {
                            ty: locals[dest.index()].ty,
                            origin: ValueOrigin::Local(dest),
                            source: SourceInfoId::SYNTHETIC,
                            repr,
                        },
                    );
                    rewritten.push(MirInst::Store {
                        source: SourceInfoId::SYNTHETIC,
                        place: Place::new(spill_base, MirProjectionPath::new()),
                        value,
                    });
                }
                MirInst::Assign {
                    source,
                    dest,
                    rvalue,
                } => {
                    let rvalue = match rvalue {
                        Rvalue::Value(value) => {
                            rewrite_place_root_value(db, core, values, locals, value)
                                .map(Rvalue::Value)
                                .unwrap_or(Rvalue::Value(value))
                        }
                        Rvalue::Call(mut call) => {
                            for value in call.args.iter_mut().chain(call.effect_args.iter_mut()) {
                                if let Some(rewritten) =
                                    rewrite_place_root_value(db, core, values, locals, *value)
                                {
                                    *value = rewritten;
                                }
                            }
                            Rvalue::Call(call)
                        }
                        Rvalue::Intrinsic { op, mut args } => {
                            for value in &mut args {
                                if let Some(rewritten) =
                                    rewrite_place_root_value(db, core, values, locals, *value)
                                {
                                    *value = rewritten;
                                }
                            }
                            Rvalue::Intrinsic { op, args }
                        }
                        other => other,
                    };
                    rewritten.push(MirInst::Assign {
                        source,
                        dest,
                        rvalue,
                    });
                }
                MirInst::Store {
                    source,
                    place,
                    value,
                } => {
                    if let Some(local) = resolve_place_root_local(values, place.base) {
                        if place.projection.is_empty() {
                            rewritten.push(MirInst::Assign {
                                source,
                                dest: Some(local),
                                rvalue: Rvalue::Value(value),
                            });
                            if spill_local_for_owner
                                .get(local.index())
                                .copied()
                                .flatten()
                                .is_some()
                                && !layout::is_zero_sized_ty(db, locals[local.index()].ty)
                            {
                                let spill_base = spill_addr_value_for_owner[local.index()].unwrap();
                                let repr = repr_for_plain_ty(
                                    db,
                                    core,
                                    locals[local.index()].ty,
                                    locals[local.index()].address_space,
                                );
                                let value = alloc_value(
                                    values,
                                    ValueData {
                                        ty: locals[local.index()].ty,
                                        origin: ValueOrigin::Local(local),
                                        source: SourceInfoId::SYNTHETIC,
                                        repr,
                                    },
                                );
                                rewritten.push(MirInst::Store {
                                    source: SourceInfoId::SYNTHETIC,
                                    place: Place::new(spill_base, MirProjectionPath::new()),
                                    value,
                                });
                            }
                            continue;
                        }

                        let Some(spill_base) = spill_addr_value_for_owner
                            .get(local.index())
                            .copied()
                            .flatten()
                        else {
                            rewritten.push(MirInst::Store {
                                source,
                                place,
                                value,
                            });
                            continue;
                        };
                        let place = Place::new(spill_base, place.projection);
                        rewritten.push(MirInst::Store {
                            source,
                            place,
                            value,
                        });
                        let update_place = Place::new(spill_base, MirProjectionPath::new());
                        rewritten.push(MirInst::Assign {
                            source: SourceInfoId::SYNTHETIC,
                            dest: Some(local),
                            rvalue: Rvalue::Load {
                                place: update_place,
                            },
                        });
                        continue;
                    }

                    rewritten.push(MirInst::Store {
                        source,
                        place: place.clone(),
                        value,
                    });

                    let Some(owner) =
                        place_base_spill_owner(values, &owner_for_spill_local, place.base)
                    else {
                        continue;
                    };
                    let Some(spill_base) = spill_addr_value_for_owner
                        .get(owner.index())
                        .copied()
                        .flatten()
                    else {
                        continue;
                    };

                    if place.projection.is_empty()
                        && locals[owner.index()].ty == values[value.index()].ty
                    {
                        rewritten.push(MirInst::Assign {
                            source: SourceInfoId::SYNTHETIC,
                            dest: Some(owner),
                            rvalue: Rvalue::Value(value),
                        });
                    } else {
                        let update_place = Place::new(spill_base, MirProjectionPath::new());
                        rewritten.push(MirInst::Assign {
                            source: SourceInfoId::SYNTHETIC,
                            dest: Some(owner),
                            rvalue: Rvalue::Load {
                                place: update_place,
                            },
                        });
                    }
                }
                MirInst::InitAggregate {
                    source,
                    place,
                    inits,
                } => {
                    if let Some(local) = resolve_place_root_local(values, place.base) {
                        let Some(spill_base) = spill_addr_value_for_owner
                            .get(local.index())
                            .copied()
                            .flatten()
                        else {
                            rewritten.push(MirInst::InitAggregate {
                                source,
                                place,
                                inits,
                            });
                            continue;
                        };
                        let place = Place::new(spill_base, place.projection);
                        rewritten.push(MirInst::InitAggregate {
                            source,
                            place,
                            inits,
                        });
                        let update_place = Place::new(spill_base, MirProjectionPath::new());
                        rewritten.push(MirInst::Assign {
                            source: SourceInfoId::SYNTHETIC,
                            dest: Some(local),
                            rvalue: Rvalue::Load {
                                place: update_place,
                            },
                        });
                        continue;
                    }

                    rewritten.push(MirInst::InitAggregate {
                        source,
                        place: place.clone(),
                        inits,
                    });

                    let Some(owner) =
                        place_base_spill_owner(values, &owner_for_spill_local, place.base)
                    else {
                        continue;
                    };
                    let Some(spill_base) = spill_addr_value_for_owner
                        .get(owner.index())
                        .copied()
                        .flatten()
                    else {
                        continue;
                    };
                    let update_place = Place::new(spill_base, MirProjectionPath::new());
                    rewritten.push(MirInst::Assign {
                        source: SourceInfoId::SYNTHETIC,
                        dest: Some(owner),
                        rvalue: Rvalue::Load {
                            place: update_place,
                        },
                    });
                }
                MirInst::SetDiscriminant {
                    source,
                    place,
                    variant,
                } => {
                    if let Some(local) = resolve_place_root_local(values, place.base) {
                        let Some(spill_base) = spill_addr_value_for_owner
                            .get(local.index())
                            .copied()
                            .flatten()
                        else {
                            rewritten.push(MirInst::SetDiscriminant {
                                source,
                                place,
                                variant,
                            });
                            continue;
                        };
                        let place = Place::new(spill_base, place.projection);
                        rewritten.push(MirInst::SetDiscriminant {
                            source,
                            place,
                            variant,
                        });
                        let update_place = Place::new(spill_base, MirProjectionPath::new());
                        rewritten.push(MirInst::Assign {
                            source: SourceInfoId::SYNTHETIC,
                            dest: Some(local),
                            rvalue: Rvalue::Load {
                                place: update_place,
                            },
                        });
                        continue;
                    }

                    rewritten.push(MirInst::SetDiscriminant {
                        source,
                        place: place.clone(),
                        variant,
                    });

                    let Some(owner) =
                        place_base_spill_owner(values, &owner_for_spill_local, place.base)
                    else {
                        continue;
                    };
                    let Some(spill_base) = spill_addr_value_for_owner
                        .get(owner.index())
                        .copied()
                        .flatten()
                    else {
                        continue;
                    };
                    let update_place = Place::new(spill_base, MirProjectionPath::new());
                    rewritten.push(MirInst::Assign {
                        source: SourceInfoId::SYNTHETIC,
                        dest: Some(owner),
                        rvalue: Rvalue::Load {
                            place: update_place,
                        },
                    });
                }
                MirInst::BindValue { source, value } => {
                    let value =
                        rewrite_place_root_value(db, core, values, locals, value).unwrap_or(value);
                    rewritten.push(MirInst::BindValue { source, value });
                }
            }
        }
        block.insts = rewritten;
    }

    let mut spill_prelude = Vec::new();
    for (owner, spill) in body.spill_slots.clone() {
        spill_prelude.push(MirInst::Assign {
            source: SourceInfoId::SYNTHETIC,
            dest: Some(spill),
            rvalue: Rvalue::Alloc {
                address_space: AddressSpaceKind::Memory,
            },
        });
        let owner_ty = body.local(owner).ty;
        if layout::is_zero_sized_ty(db, owner_ty) {
            continue;
        }
        let spill_base = spill_addr_value_for_owner[owner.index()].unwrap();
        let repr = repr_for_plain_ty(db, core, owner_ty, body.local(owner).address_space);
        let owner_value = alloc_value(
            &mut body.values,
            ValueData {
                ty: owner_ty,
                origin: ValueOrigin::Local(owner),
                source: SourceInfoId::SYNTHETIC,
                repr,
            },
        );
        spill_prelude.push(MirInst::Store {
            source: SourceInfoId::SYNTHETIC,
            place: Place::new(spill_base, MirProjectionPath::new()),
            value: owner_value,
        });
    }
    if !spill_prelude.is_empty() {
        let entry = body.entry.index();
        let entry_insts = &mut body.blocks[entry].insts;
        let mut new_insts = spill_prelude;
        new_insts.extend(std::mem::take(entry_insts));
        *entry_insts = new_insts;
    }

    body.stage = MirStage::Repr(backend);
}
