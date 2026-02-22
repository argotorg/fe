use hir::analysis::HirAnalysisDb;
use hir::analysis::ty::ty_def::{CapabilityKind, TyId};
use hir::hir_def::EnumVariant;
use hir::projection::Projection;

use crate::core_lib::CoreLib;
use crate::ir::{
    AddressSpaceKind, BasicBlock, LocalData, LocalId, MirBackend, MirBody, MirInst,
    MirProjectionPath, MirStage, Place, Rvalue, SourceInfoId, ValueData, ValueId, ValueOrigin,
    ValueRepr,
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

fn is_scalar_ref_inner_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
) -> bool {
    matches!(
        repr::repr_kind_for_ty(db, core, ty),
        repr::ReprKind::Word | repr::ReprKind::Zst | repr::ReprKind::Ptr(_)
    )
}

fn is_scalar_ref_word_capability_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
    desired: ValueRepr,
) -> bool {
    ty.as_capability(db).is_some_and(|(kind, inner)| {
        matches!(kind, CapabilityKind::Ref)
            && desired.address_space().is_none()
            && is_scalar_ref_inner_ty(db, core, inner)
    })
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

fn value_spill_owner<'db>(
    values: &[ValueData<'db>],
    owner_for_spill_local: &[Option<LocalId>],
    mut value: ValueId,
) -> Option<LocalId> {
    loop {
        let origin = &values.get(value.index())?.origin;
        match origin {
            ValueOrigin::TransparentCast { value: inner } => value = *inner,
            ValueOrigin::Local(local) => {
                return owner_for_spill_local.get(local.index()).copied()?;
            }
            ValueOrigin::PlaceRef(place) => {
                return place_base_spill_owner(values, owner_for_spill_local, place.base);
            }
            ValueOrigin::MoveOut { place } => {
                return place_base_spill_owner(values, owner_for_spill_local, place.base);
            }
            _ => return None,
        }
    }
}

fn compute_owner_for_spill_local<'db>(
    blocks: &[BasicBlock<'db>],
    values: &[ValueData<'db>],
    mut owner_for_spill_local: Vec<Option<LocalId>>,
) -> Vec<Option<LocalId>> {
    let mut changed = true;
    while changed {
        changed = false;
        for block in blocks {
            for inst in &block.insts {
                let MirInst::Assign {
                    dest: Some(dest),
                    rvalue: Rvalue::Value(value),
                    ..
                } = inst
                else {
                    continue;
                };
                if owner_for_spill_local
                    .get(dest.index())
                    .copied()
                    .flatten()
                    .is_some()
                {
                    continue;
                }
                let Some(owner) = value_spill_owner(values, &owner_for_spill_local, *value) else {
                    continue;
                };
                owner_for_spill_local[dest.index()] = Some(owner);
                changed = true;
            }
        }
    }
    owner_for_spill_local
}

struct LowerReprCtx<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    core: &'a CoreLib<'db>,
    values: &'a mut Vec<ValueData<'db>>,
    locals: &'a [LocalData<'db>],
    spill_local_for_owner: &'a [Option<LocalId>],
    owner_for_spill_local: &'a [Option<LocalId>],
    spill_addr_value_for_owner: &'a [Option<ValueId>],
}

impl<'db, 'a> LowerReprCtx<'db, 'a> {
    fn local_repr(&self, local: LocalId) -> ValueRepr {
        repr_for_plain_ty(
            self.db,
            self.core,
            self.locals[local.index()].ty,
            self.locals[local.index()].address_space,
        )
    }

    fn has_spill_slot(&self, local: LocalId) -> bool {
        self.spill_local_for_owner
            .get(local.index())
            .copied()
            .flatten()
            .is_some()
            && !layout::is_zero_sized_ty(self.db, self.locals[local.index()].ty)
    }

    fn spill_base_for_owner(&self, owner: LocalId) -> Option<ValueId> {
        self.spill_addr_value_for_owner
            .get(owner.index())
            .copied()
            .flatten()
    }

    fn owner_for_place_base(&self, base: ValueId) -> Option<LocalId> {
        place_base_spill_owner(self.values, self.owner_for_spill_local, base)
    }

    fn emit_spill_sync_for_local(&mut self, local: LocalId) -> Option<MirInst<'db>> {
        if !self.has_spill_slot(local) {
            return None;
        }
        let spill_base = self.spill_base_for_owner(local).expect(
            "spill slot must have a spill base address value (repr pass invariant violated)",
        );
        let value = alloc_value(
            self.values,
            ValueData {
                ty: self.locals[local.index()].ty,
                origin: ValueOrigin::Local(local),
                source: SourceInfoId::SYNTHETIC,
                repr: self.local_repr(local),
            },
        );
        Some(MirInst::Store {
            source: SourceInfoId::SYNTHETIC,
            place: Place::new(spill_base, MirProjectionPath::new()),
            value,
        })
    }

    fn emit_local_reload_from_spill(&self, local: LocalId) -> Option<MirInst<'db>> {
        let spill_base = self.spill_base_for_owner(local)?;
        let update_place = Place::new(spill_base, MirProjectionPath::new());
        Some(MirInst::Assign {
            source: SourceInfoId::SYNTHETIC,
            dest: Some(local),
            rvalue: Rvalue::Load {
                place: update_place,
            },
        })
    }

    fn emit_owner_reload_for_place_base(&self, base: ValueId) -> Option<MirInst<'db>> {
        let owner = self.owner_for_place_base(base)?;
        self.emit_local_reload_from_spill(owner)
    }

    fn emit_owner_refresh_after_store(
        &self,
        place: &Place<'db>,
        value: ValueId,
    ) -> Option<MirInst<'db>> {
        let owner = self.owner_for_place_base(place.base)?;
        let spill_base = self.spill_base_for_owner(owner)?;
        if place.projection.is_empty()
            && self.locals[owner.index()].ty == self.values[value.index()].ty
        {
            return Some(MirInst::Assign {
                source: SourceInfoId::SYNTHETIC,
                dest: Some(owner),
                rvalue: Rvalue::Value(value),
            });
        }
        let update_place = Place::new(spill_base, MirProjectionPath::new());
        Some(MirInst::Assign {
            source: SourceInfoId::SYNTHETIC,
            dest: Some(owner),
            rvalue: Rvalue::Load {
                place: update_place,
            },
        })
    }

    fn rewrite_assign_load(
        &mut self,
        source: SourceInfoId,
        dest: Option<LocalId>,
        place: Place<'db>,
        out: &mut Vec<MirInst<'db>>,
    ) {
        let loaded_ty = dest
            .map(|dest| self.locals[dest.index()].ty)
            .unwrap_or(self.values[place.base.index()].ty);
        let base_ty = self.values[place.base.index()].ty;

        // Loading a transparent field-0 projection from a non-by-ref base is a
        // representation-preserving cast, not a memory read.
        if !place.projection.is_empty()
            && loaded_ty.as_capability(self.db).is_some()
            && !self.values[place.base.index()].repr.is_ref()
            && resolve_place_root_local(self.values, place.base).is_none()
            && let Some(projected_ty) =
                apply_transparent_field0_chain(self.db, base_ty, &place.projection)
            && projected_ty == loaded_ty
        {
            let base_repr = self.values[place.base.index()].repr;
            let loaded_value = if loaded_ty == base_ty {
                place.base
            } else {
                alloc_value(
                    self.values,
                    ValueData {
                        ty: loaded_ty,
                        origin: ValueOrigin::TransparentCast { value: place.base },
                        source: SourceInfoId::SYNTHETIC,
                        repr: base_repr,
                    },
                )
            };
            out.push(MirInst::Assign {
                source,
                dest,
                rvalue: Rvalue::Value(loaded_value),
            });
            return;
        }

        if let Some(local) = resolve_place_root_local(self.values, place.base) {
            let Some(projected_ty) =
                apply_transparent_field0_chain(self.db, base_ty, &place.projection)
            else {
                let Some(spill_base) = self.spill_base_for_owner(local) else {
                    out.push(MirInst::Assign {
                        source,
                        dest,
                        rvalue: Rvalue::Load { place },
                    });
                    return;
                };
                out.push(MirInst::Assign {
                    source,
                    dest,
                    rvalue: Rvalue::Load {
                        place: Place::new(spill_base, place.projection),
                    },
                });
                return;
            };

            if projected_ty != loaded_ty {
                let Some(spill_base) = self.spill_base_for_owner(local) else {
                    out.push(MirInst::Assign {
                        source,
                        dest,
                        rvalue: Rvalue::Load { place },
                    });
                    return;
                };
                out.push(MirInst::Assign {
                    source,
                    dest,
                    rvalue: Rvalue::Load {
                        place: Place::new(spill_base, place.projection),
                    },
                });
                return;
            }

            let local_value = alloc_value(
                self.values,
                ValueData {
                    ty: base_ty,
                    origin: ValueOrigin::Local(local),
                    source: SourceInfoId::SYNTHETIC,
                    repr: self.local_repr(local),
                },
            );
            let loaded_value = if loaded_ty == base_ty {
                local_value
            } else {
                alloc_value(
                    self.values,
                    ValueData {
                        ty: loaded_ty,
                        origin: ValueOrigin::TransparentCast { value: local_value },
                        source: SourceInfoId::SYNTHETIC,
                        repr: self.local_repr(local),
                    },
                )
            };
            out.push(MirInst::Assign {
                source,
                dest,
                rvalue: Rvalue::Value(loaded_value),
            });
            return;
        }

        out.push(MirInst::Assign {
            source,
            dest,
            rvalue: Rvalue::Load { place },
        });
        if let Some(dest) = dest
            && let Some(spill_sync) = self.emit_spill_sync_for_local(dest)
        {
            out.push(spill_sync);
        }
    }

    fn rewrite_assign_spill_dest(
        &mut self,
        source: SourceInfoId,
        dest: LocalId,
        rvalue: Rvalue<'db>,
        out: &mut Vec<MirInst<'db>>,
    ) {
        out.push(MirInst::Assign {
            source,
            dest: Some(dest),
            rvalue: rvalue.clone(),
        });
        if let Some(spill_sync) = self.emit_spill_sync_for_local(dest) {
            out.push(spill_sync);
        }
    }

    fn rewrite_assign_generic(
        &mut self,
        source: SourceInfoId,
        dest: Option<LocalId>,
        rvalue: Rvalue<'db>,
        out: &mut Vec<MirInst<'db>>,
    ) {
        let rvalue = match rvalue {
            Rvalue::Value(value) => {
                rewrite_place_root_value(self.db, self.core, self.values, self.locals, value)
                    .map(Rvalue::Value)
                    .unwrap_or(Rvalue::Value(value))
            }
            Rvalue::Call(mut call) => {
                for value in call.args.iter_mut().chain(call.effect_args.iter_mut()) {
                    if let Some(rewritten) = rewrite_place_root_value(
                        self.db,
                        self.core,
                        self.values,
                        self.locals,
                        *value,
                    ) {
                        *value = rewritten;
                    }
                }
                Rvalue::Call(call)
            }
            Rvalue::Intrinsic { op, mut args } => {
                for value in &mut args {
                    if let Some(rewritten) = rewrite_place_root_value(
                        self.db,
                        self.core,
                        self.values,
                        self.locals,
                        *value,
                    ) {
                        *value = rewritten;
                    }
                }
                Rvalue::Intrinsic { op, args }
            }
            other => other,
        };
        out.push(MirInst::Assign {
            source,
            dest,
            rvalue,
        });
    }

    fn rewrite_store_inst(
        &mut self,
        source: SourceInfoId,
        place: Place<'db>,
        value: ValueId,
        out: &mut Vec<MirInst<'db>>,
    ) {
        if let Some(local) = resolve_place_root_local(self.values, place.base) {
            let local_ty = self.locals[local.index()].ty;
            let local_place_ty = local_ty
                .as_capability(self.db)
                .map(|(_, inner)| inner)
                .unwrap_or(local_ty);

            if place.projection.is_empty() {
                out.push(MirInst::Assign {
                    source,
                    dest: Some(local),
                    rvalue: Rvalue::Value(value),
                });
                if let Some(spill_sync) = self.emit_spill_sync_for_local(local) {
                    out.push(spill_sync);
                }
                return;
            }

            if let Some(projected_ty) =
                apply_transparent_field0_chain(self.db, local_place_ty, &place.projection)
            {
                let stored_place_value = if self.values[value.index()].ty == local_place_ty {
                    Some(value)
                } else if self.values[value.index()].ty == projected_ty {
                    Some(alloc_value(
                        self.values,
                        ValueData {
                            ty: local_place_ty,
                            origin: ValueOrigin::TransparentCast { value },
                            source: SourceInfoId::SYNTHETIC,
                            repr: repr_for_plain_ty(
                                self.db,
                                self.core,
                                local_place_ty,
                                self.locals[local.index()].address_space,
                            ),
                        },
                    ))
                } else {
                    None
                };

                if let Some(stored_place_value) = stored_place_value {
                    let assign_value = if local_ty == local_place_ty {
                        stored_place_value
                    } else {
                        alloc_value(
                            self.values,
                            ValueData {
                                ty: local_ty,
                                origin: ValueOrigin::TransparentCast {
                                    value: stored_place_value,
                                },
                                source: SourceInfoId::SYNTHETIC,
                                repr: self.local_repr(local),
                            },
                        )
                    };
                    out.push(MirInst::Assign {
                        source,
                        dest: Some(local),
                        rvalue: Rvalue::Value(assign_value),
                    });
                    if let Some(spill_sync) = self.emit_spill_sync_for_local(local) {
                        out.push(spill_sync);
                    }
                    return;
                }
            }

            let Some(spill_base) = self.spill_base_for_owner(local) else {
                out.push(MirInst::Store {
                    source,
                    place,
                    value,
                });
                return;
            };
            out.push(MirInst::Store {
                source,
                place: Place::new(spill_base, place.projection),
                value,
            });
            if let Some(reload) = self.emit_local_reload_from_spill(local) {
                out.push(reload);
            }
            return;
        }

        out.push(MirInst::Store {
            source,
            place: place.clone(),
            value,
        });
        if let Some(refresh) = self.emit_owner_refresh_after_store(&place, value) {
            out.push(refresh);
        }
    }

    fn rewrite_place_inst_with_reload(
        &mut self,
        place: Place<'db>,
        out: &mut Vec<MirInst<'db>>,
        make_inst: impl FnOnce(Place<'db>) -> MirInst<'db>,
    ) {
        if let Some(local) = resolve_place_root_local(self.values, place.base) {
            let rewritten = if let Some(spill_base) = self.spill_base_for_owner(local) {
                let Place { projection, .. } = place;
                Place::new(spill_base, projection)
            } else {
                place
            };
            out.push(make_inst(rewritten));
            if let Some(reload) = self.emit_local_reload_from_spill(local) {
                out.push(reload);
            }
            return;
        }

        let base = place.base;
        out.push(make_inst(place));
        if let Some(reload) = self.emit_owner_reload_for_place_base(base) {
            out.push(reload);
        }
    }

    fn rewrite_init_aggregate_inst(
        &mut self,
        source: SourceInfoId,
        place: Place<'db>,
        inits: Vec<(MirProjectionPath<'db>, ValueId)>,
        out: &mut Vec<MirInst<'db>>,
    ) {
        self.rewrite_place_inst_with_reload(place, out, |place| MirInst::InitAggregate {
            source,
            place,
            inits,
        });
    }

    fn rewrite_set_discriminant_inst(
        &mut self,
        source: SourceInfoId,
        place: Place<'db>,
        variant: EnumVariant<'db>,
        out: &mut Vec<MirInst<'db>>,
    ) {
        self.rewrite_place_inst_with_reload(place, out, |place| MirInst::SetDiscriminant {
            source,
            place,
            variant,
        });
    }

    fn rewrite_bind_value_inst(
        &mut self,
        source: SourceInfoId,
        value: ValueId,
        out: &mut Vec<MirInst<'db>>,
    ) {
        let value = rewrite_place_root_value(self.db, self.core, self.values, self.locals, value)
            .unwrap_or(value);
        out.push(MirInst::BindValue { source, value });
    }

    fn rewrite_inst(&mut self, inst: MirInst<'db>, out: &mut Vec<MirInst<'db>>) {
        match inst {
            MirInst::Assign {
                source,
                dest,
                rvalue: Rvalue::Load { place },
            } => self.rewrite_assign_load(source, dest, place, out),
            MirInst::Assign {
                source,
                dest: Some(dest),
                rvalue,
            } if self.has_spill_slot(dest) => {
                self.rewrite_assign_spill_dest(source, dest, rvalue, out)
            }
            MirInst::Assign {
                source,
                dest,
                rvalue,
            } => self.rewrite_assign_generic(source, dest, rvalue, out),
            MirInst::Store {
                source,
                place,
                value,
            } => self.rewrite_store_inst(source, place, value, out),
            MirInst::InitAggregate {
                source,
                place,
                inits,
            } => self.rewrite_init_aggregate_inst(source, place, inits, out),
            MirInst::SetDiscriminant {
                source,
                place,
                variant,
            } => self.rewrite_set_discriminant_inst(source, place, variant, out),
            MirInst::BindValue { source, value } => {
                self.rewrite_bind_value_inst(source, value, out)
            }
        }
    }

    fn rewrite_block(&mut self, block: &mut BasicBlock<'db>) {
        let mut rewritten = Vec::with_capacity(block.insts.len());
        for inst in std::mem::take(&mut block.insts) {
            self.rewrite_inst(inst, &mut rewritten);
        }
        block.insts = rewritten;
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

struct PlaceOriginRewriteCtx<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    core: &'a CoreLib<'db>,
    values: &'a mut Vec<ValueData<'db>>,
    locals: &'a [LocalData<'db>],
    spill_addr_value_for_owner: &'a [Option<ValueId>],
}

impl<'db> PlaceOriginRewriteCtx<'db, '_> {
    fn rewrite_place_like_origin(
        &mut self,
        mut place: Place<'db>,
        desired: ValueRepr,
        as_move_out: bool,
    ) -> ValueOrigin<'db> {
        let make_place_origin = |place: Place<'db>| {
            if as_move_out {
                ValueOrigin::MoveOut { place }
            } else {
                ValueOrigin::PlaceRef(place)
            }
        };

        let Some(local) = resolve_place_root_local(self.values, place.base) else {
            return make_place_origin(place);
        };

        let local_ty = self.locals[local.index()].ty;
        if let Some(projected_ty) =
            apply_transparent_field0_chain(self.db, local_ty, &place.projection)
            && (desired.address_space().is_none() || !place.projection.is_empty())
        {
            let local_repr = repr_for_plain_ty(
                self.db,
                self.core,
                local_ty,
                self.locals[local.index()].address_space,
            );
            let local_value = alloc_value(
                self.values,
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
                    self.values,
                    ValueData {
                        ty: projected_ty,
                        origin: ValueOrigin::TransparentCast { value: local_value },
                        source: SourceInfoId::SYNTHETIC,
                        repr: local_repr,
                    },
                )
            };
            return ValueOrigin::TransparentCast {
                value: source_value,
            };
        }

        let local_space = self.locals[local.index()].address_space;
        let spill_base = self.spill_addr_value_for_owner[local.index()].unwrap_or_else(|| {
            alloc_value(
                self.values,
                ValueData {
                    ty: local_ty,
                    origin: ValueOrigin::Local(local),
                    source: SourceInfoId::SYNTHETIC,
                    repr: repr_for_plain_ty(self.db, self.core, local_ty, local_space),
                },
            )
        });
        place.base = spill_base;
        make_place_origin(place)
    }
}

fn rewrite_place_like_origin<'db>(
    ctx: &mut PlaceOriginRewriteCtx<'db, '_>,
    place: Place<'db>,
    desired: ValueRepr,
    as_move_out: bool,
) -> ValueOrigin<'db> {
    ctx.rewrite_place_like_origin(place, desired, as_move_out)
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
                ValueOrigin::FieldPtr(field_ptr) => Some(field_ptr.addr_space),
                ValueOrigin::TransparentCast { value } => {
                    origin_address_space(values, locals, &values.get(value.index())?.origin)
                }
                _ => None,
            }
        }

        fn capability_space_from_origin<'db>(
            values: &[ValueData<'db>],
            locals: &[LocalData<'db>],
            origin: &ValueOrigin<'db>,
        ) -> Option<AddressSpaceKind> {
            match origin {
                ValueOrigin::TransparentCast { value } => {
                    capability_space_from_origin(values, locals, &values.get(value.index())?.origin)
                }
                ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) => {
                    crate::ir::lookup_local_capability_space(
                        locals,
                        *local,
                        &MirProjectionPath::new(),
                    )
                    .or_else(|| locals.get(local.index()).map(|l| l.address_space))
                }
                ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
                    if let Some((local, prefix)) =
                        crate::ir::resolve_local_projection_root(values, place.base)
                    {
                        let projection = prefix.concat(&place.projection);
                        if let Some(space) =
                            crate::ir::lookup_local_capability_space(locals, local, &projection)
                        {
                            return Some(space);
                        }
                    }
                    crate::ir::try_value_address_space_in(values, locals, place.base)
                }
                ValueOrigin::FieldPtr(field_ptr) => Some(field_ptr.addr_space),
                _ => origin_address_space(values, locals, origin),
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
                                    capability_space_from_origin(values, locals, &data.origin)
                            {
                                ValueRepr::Ptr(space)
                            } else {
                                ValueRepr::Ptr(space_kind)
                            }
                        }
                        repr::ReprKind::Ref => {
                            let space = capability_space_from_origin(values, locals, &data.origin)
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
        if is_scalar_ref_word_capability_ty(db, core, value.ty, desired_repr[idx]) {
            continue;
        }
        let Some(local) = resolve_place_root_local(&body.values, place.base) else {
            continue;
        };

        let is_word_place = desired_repr[idx].address_space().is_none();
        let local_ty = body.local(local).ty;
        let is_transparent_place =
            apply_transparent_field0_chain(db, local_ty, &place.projection).is_some();
        if !(is_word_place && is_transparent_place) {
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
                let base_ty = body.values[place.base.index()].ty;
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
                    capability_spaces: Vec::new(),
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

    for (value, desired) in body
        .values
        .iter_mut()
        .zip(desired_repr.iter().copied())
        .take(initial_values_len)
    {
        value.repr = desired;
    }

    {
        let mut place_rewrite_ctx = PlaceOriginRewriteCtx {
            db,
            core,
            values: &mut body.values,
            locals: &body.locals,
            spill_addr_value_for_owner: &spill_addr_value_for_owner,
        };
        for (idx, desired) in desired_repr
            .iter()
            .copied()
            .enumerate()
            .take(initial_values_len)
        {
            let origin = place_rewrite_ctx.values[idx].origin.clone();
            let new_origin = match origin {
                ValueOrigin::PlaceRef(place) => {
                    rewrite_place_like_origin(&mut place_rewrite_ctx, place, desired, false)
                }
                ValueOrigin::MoveOut { place } => {
                    rewrite_place_like_origin(&mut place_rewrite_ctx, place, desired, true)
                }
                other => other,
            };
            place_rewrite_ctx.values[idx].origin = new_origin;
        }
    }

    let owner_for_spill_local =
        compute_owner_for_spill_local(&body.blocks, &body.values, owner_for_spill_local);

    {
        let (blocks, values, locals) = (&mut body.blocks, &mut body.values, &body.locals);
        let mut ctx = LowerReprCtx {
            db,
            core,
            values,
            locals,
            spill_local_for_owner: &spill_local_for_owner,
            owner_for_spill_local: &owner_for_spill_local,
            spill_addr_value_for_owner: &spill_addr_value_for_owner,
        };
        for block in blocks {
            ctx.rewrite_block(block);
        }
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
        if !body.param_locals.contains(&owner) && !body.effect_param_locals.contains(&owner) {
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

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::analysis::ty::adt_def::AdtRef;
    use hir::analysis::ty::ty_def::TyId;
    use hir::hir_def::{EnumVariant, TopLevelMod};
    use hir::projection::Projection;
    use url::Url;

    use crate::{
        MirBackend, MirInst, core_lib::CoreLib, ir::AddressSpaceKind, ir::BasicBlock,
        ir::FieldPtrOrigin, ir::LocalData, ir::LocalId, ir::MirBody, ir::MirStage, ir::Place,
        ir::Rvalue, ir::SourceInfoId, ir::Terminator, ir::ValueData, ir::ValueOrigin,
        ir::ValueRepr,
    };

    use super::lower_capability_to_repr;

    fn load_func_param_ty<'db>(
        db: &'db DriverDataBase,
        top_mod: TopLevelMod<'db>,
        func_name: &str,
    ) -> (CoreLib<'db>, TyId<'db>) {
        let hir_func = top_mod
            .all_funcs(db)
            .iter()
            .copied()
            .find(|func| {
                func.name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == func_name)
            })
            .expect("function should exist");
        let core = CoreLib::new(db, hir_func.scope());
        let capability_ty = hir_func
            .params(db)
            .next()
            .expect("parameter should exist")
            .ty(db);
        (core, capability_ty)
    }

    fn make_place_root_body<'db>(local_ty: TyId<'db>) -> (MirBody<'db>, LocalId, crate::ValueId) {
        let mut body = MirBody::new();
        body.stage = MirStage::Capability;
        body.blocks.push(BasicBlock {
            insts: Vec::new(),
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: None,
            },
        });
        let local = body.alloc_local(LocalData {
            name: "owner".to_string(),
            ty: local_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            capability_spaces: Vec::new(),
        });
        let base = body.alloc_value(ValueData {
            ty: local_ty,
            origin: ValueOrigin::PlaceRoot(local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
        });
        (body, local, base)
    }

    fn projection_is_field0<'db>(path: &crate::MirProjectionPath<'db>) -> bool {
        let mut it = path.iter();
        matches!(it.next(), Some(Projection::Field(0))) && it.next().is_none()
    }

    fn projection_is_field1<'db>(path: &crate::MirProjectionPath<'db>) -> bool {
        let mut it = path.iter();
        matches!(it.next(), Some(Projection::Field(1))) && it.next().is_none()
    }

    #[test]
    fn field_ptr_origin_preserves_address_space_for_capability_values() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///field_ptr_origin_preserves_address_space.fe").unwrap();
        let src = r#"
pub fn field_ptr_origin_preserves_address_space(x: mut u256) {}
"#;
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let (core, capability_ty) =
            load_func_param_ty(&db, top_mod, "field_ptr_origin_preserves_address_space");

        let mut body = MirBody::new();
        body.stage = MirStage::Capability;
        body.blocks.push(BasicBlock {
            insts: Vec::new(),
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: None,
            },
        });
        body.locals.push(LocalData {
            name: "base".to_string(),
            ty: capability_ty,
            is_mut: false,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Storage,
            capability_spaces: Vec::new(),
        });
        let base = body.alloc_value(ValueData {
            ty: capability_ty,
            origin: ValueOrigin::Local(crate::ir::LocalId(0)),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Storage),
        });
        let field_ptr = body.alloc_value(ValueData {
            ty: capability_ty,
            origin: ValueOrigin::FieldPtr(FieldPtrOrigin {
                base,
                offset_bytes: 0,
                addr_space: AddressSpaceKind::Storage,
            }),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
        });

        lower_capability_to_repr(&db, &core, MirBackend::EvmYul, &mut body);

        assert_eq!(
            body.values[field_ptr.index()].repr,
            ValueRepr::Ptr(AddressSpaceKind::Storage),
            "FieldPtr-based capability values must preserve their originating address space",
        );
    }

    #[test]
    fn store_place_root_projection_rewrites_through_spill_and_reloads_owner() {
        let mut db = DriverDataBase::default();
        let url = Url::parse(
            "file:///store_place_root_projection_rewrites_through_spill_and_reloads_owner.fe",
        )
        .unwrap();
        let src = r#"
struct Pair {
    a: u256,
    b: u256,
}

pub fn store_place_root_projection_rewrites_through_spill_and_reloads_owner(x: mut Pair) {}
"#;
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let (core, pair_capability_ty) = load_func_param_ty(
            &db,
            top_mod,
            "store_place_root_projection_rewrites_through_spill_and_reloads_owner",
        );
        let (_, pair_ty) = pair_capability_ty
            .as_capability(&db)
            .expect("param should be a capability");
        let u256_ty = pair_ty.field_types(&db)[0];

        let (mut body, owner, base) = make_place_root_body(pair_capability_ty);
        let value = body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Synthetic(crate::ir::SyntheticValue::Int(1u8.into())),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
        });
        body.blocks[0].insts.push(MirInst::Store {
            source: SourceInfoId::SYNTHETIC,
            place: Place::new(
                base,
                crate::MirProjectionPath::from_projection(Projection::Field(1)),
            ),
            value,
        });

        lower_capability_to_repr(&db, &core, MirBackend::EvmYul, &mut body);

        let spill = *body
            .spill_slots
            .get(&owner)
            .expect("owner should have a spill slot");
        let mut saw_spill_store = false;
        let mut saw_owner_reload = false;
        for inst in &body.blocks[0].insts {
            match inst {
                MirInst::Store { place, .. } => {
                    if projection_is_field1(&place.projection)
                        && matches!(body.value(place.base).origin, ValueOrigin::Local(local) if local == spill)
                    {
                        saw_spill_store = true;
                    }
                }
                MirInst::Assign {
                    dest: Some(local),
                    rvalue: Rvalue::Load { place },
                    ..
                } => {
                    if *local == owner
                        && place.projection.is_empty()
                        && matches!(body.value(place.base).origin, ValueOrigin::Local(base_local) if base_local == spill)
                    {
                        saw_owner_reload = true;
                    }
                }
                _ => {}
            }
        }
        assert!(saw_spill_store, "store should target spill-backed place");
        assert!(
            saw_owner_reload,
            "store should reload the owner local from spill"
        );
    }

    #[test]
    fn init_aggregate_place_root_projection_rewrites_through_spill_and_reloads_owner() {
        let mut db = DriverDataBase::default();
        let url = Url::parse(
            "file:///init_aggregate_place_root_projection_rewrites_through_spill_and_reloads_owner.fe",
        )
        .unwrap();
        let src = r#"
struct Pair {
    a: u256,
    b: u256,
}

pub fn init_aggregate_place_root_projection_rewrites_through_spill_and_reloads_owner(x: mut Pair) {}
"#;
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let (core, pair_capability_ty) = load_func_param_ty(
            &db,
            top_mod,
            "init_aggregate_place_root_projection_rewrites_through_spill_and_reloads_owner",
        );
        let (_, pair_ty) = pair_capability_ty
            .as_capability(&db)
            .expect("param should be a capability");
        let u256_ty = pair_ty.field_types(&db)[0];

        let (mut body, owner, base) = make_place_root_body(pair_capability_ty);
        let value = body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Synthetic(crate::ir::SyntheticValue::Int(1u8.into())),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
        });
        body.blocks[0].insts.push(MirInst::InitAggregate {
            source: SourceInfoId::SYNTHETIC,
            place: Place::new(
                base,
                crate::MirProjectionPath::from_projection(Projection::Field(1)),
            ),
            inits: vec![(crate::MirProjectionPath::new(), value)],
        });

        lower_capability_to_repr(&db, &core, MirBackend::EvmYul, &mut body);

        let spill = *body
            .spill_slots
            .get(&owner)
            .expect("owner should have a spill slot");
        let mut saw_spill_init = false;
        let mut saw_owner_reload = false;
        for inst in &body.blocks[0].insts {
            match inst {
                MirInst::InitAggregate { place, .. } => {
                    if projection_is_field1(&place.projection)
                        && matches!(body.value(place.base).origin, ValueOrigin::Local(local) if local == spill)
                    {
                        saw_spill_init = true;
                    }
                }
                MirInst::Assign {
                    dest: Some(local),
                    rvalue: Rvalue::Load { place },
                    ..
                } => {
                    if *local == owner
                        && place.projection.is_empty()
                        && matches!(body.value(place.base).origin, ValueOrigin::Local(base_local) if base_local == spill)
                    {
                        saw_owner_reload = true;
                    }
                }
                _ => {}
            }
        }
        assert!(
            saw_spill_init,
            "init aggregate should target spill-backed place"
        );
        assert!(
            saw_owner_reload,
            "init aggregate should reload the owner local from spill"
        );
    }

    #[test]
    fn set_discriminant_place_root_projection_rewrites_through_spill_and_reloads_owner() {
        let mut db = DriverDataBase::default();
        let url = Url::parse(
            "file:///set_discriminant_place_root_projection_rewrites_through_spill_and_reloads_owner.fe",
        )
        .unwrap();
        let src = r#"
enum E {
    A,
    B,
}

struct Wrap {
    e: E,
    pad: u256,
}

pub fn set_discriminant_place_root_projection_rewrites_through_spill_and_reloads_owner(x: mut Wrap) {}
"#;
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let (core, wrap_capability_ty) = load_func_param_ty(
            &db,
            top_mod,
            "set_discriminant_place_root_projection_rewrites_through_spill_and_reloads_owner",
        );
        let (_, wrap_ty) = wrap_capability_ty
            .as_capability(&db)
            .expect("param should be a capability");
        let enum_ty = wrap_ty.field_types(&db)[0];
        let adt = enum_ty.adt_def(&db).expect("enum field should be an ADT");
        let AdtRef::Enum(enum_def) = adt.adt_ref(&db) else {
            panic!("field should be an enum type");
        };
        let variant_b = EnumVariant::new(enum_def, 1);

        let (mut body, owner, base) = make_place_root_body(wrap_capability_ty);
        body.blocks[0].insts.push(MirInst::SetDiscriminant {
            source: SourceInfoId::SYNTHETIC,
            place: Place::new(
                base,
                crate::MirProjectionPath::from_projection(Projection::Field(0)),
            ),
            variant: variant_b,
        });

        lower_capability_to_repr(&db, &core, MirBackend::EvmYul, &mut body);

        let spill = *body
            .spill_slots
            .get(&owner)
            .expect("owner should have a spill slot");
        let mut saw_spill_discriminant = false;
        let mut saw_owner_reload = false;
        for inst in &body.blocks[0].insts {
            match inst {
                MirInst::SetDiscriminant { place, .. } => {
                    if projection_is_field0(&place.projection)
                        && matches!(body.value(place.base).origin, ValueOrigin::Local(local) if local == spill)
                    {
                        saw_spill_discriminant = true;
                    }
                }
                MirInst::Assign {
                    dest: Some(local),
                    rvalue: Rvalue::Load { place },
                    ..
                } => {
                    if *local == owner
                        && place.projection.is_empty()
                        && matches!(body.value(place.base).origin, ValueOrigin::Local(base_local) if base_local == spill)
                    {
                        saw_owner_reload = true;
                    }
                }
                _ => {}
            }
        }
        assert!(
            saw_spill_discriminant,
            "set discriminant should target spill-backed place"
        );
        assert!(
            saw_owner_reload,
            "set discriminant should reload the owner local from spill"
        );
    }
}
