use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    analysis::{
        semantic::{FieldIndex, SLocalId, SemOrigin, VariantIndex},
        ty::{provider::ProviderAddressSpace, ty_def::TyId},
    },
    projection::{IndexSource, Projection},
};

use super::{
    super::{
        analyses::BorrowSummaryMode,
        ir::{
            MemorySummaryId, NExpr, NOperand, NSPlace, NSProjectionPath, NSStmt, NSStmtKind,
            PointerAddressSpaces, SemanticBorrowDiagnostic,
        },
        pointer::{is_pointer_bearing_type, pointer_slots},
    },
    BorrowCanonCx, BorrowRoot, CanonPlace, LoanId, PointerTargets, State, WritePrecision,
    summary::{BorrowSummaryResolver, ResolvedPointerStore},
};

pub(in super::super) struct BorrowStateTransferCx<'a, 'db> {
    canon: BorrowCanonCx<'a, 'db>,
    summaries: BorrowSummaryResolver<'a, 'db>,
    loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
}

impl<'a, 'db> BorrowStateTransferCx<'a, 'db> {
    pub(in super::super) fn new(
        canon: BorrowCanonCx<'a, 'db>,
        loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
        summary_mode: BorrowSummaryMode,
    ) -> Self {
        Self {
            canon,
            summaries: BorrowSummaryResolver::new(canon, summary_mode),
            loan_for_local,
        }
    }

    fn assign_pointer_targets_for_expr(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        expr: &NExpr<'db>,
        call_summary: Option<MemorySummaryId<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let Some(dst_ty) = self.canon.body.local(dst).map(|local| local.ty) else {
            return Ok(());
        };
        if dst_ty.as_borrow(self.canon.db).is_some()
            || !is_pointer_bearing_type(self.canon.db, dst_ty)
        {
            return Ok(());
        }
        match expr {
            NExpr::Use(src) => {
                self.copy_pointer_targets_from_value(state, dst, src.local, dst_ty, origin)
            }
            NExpr::ReadPlace { place, .. } => {
                self.copy_pointer_targets_from_place(state, dst, place, dst_ty, origin)
            }
            NExpr::AggregateMake { ty, fields } => {
                self.assign_aggregate_pointer_targets(state, dst, *ty, fields, origin)
            }
            NExpr::ArrayRepeat { ty, value } => {
                self.assign_array_repeat_pointer_targets(state, dst, *ty, *value, origin)
            }
            NExpr::EnumMake {
                enum_ty,
                variant,
                fields,
            } => self.assign_enum_pointer_targets(state, dst, *enum_ty, *variant, fields, origin),
            NExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => self
                .copy_pointer_targets_from_enum_field(state, dst, *value, *variant, *field, origin),
            NExpr::Cast { value, to } if is_pointer_bearing_type(self.canon.db, *to) => {
                let src_ty = self.canon.body.local(value.local).map(|local| local.ty);
                if src_ty.is_some_and(|ty| is_pointer_bearing_type(self.canon.db, ty)) {
                    self.copy_pointer_targets_from_value(state, dst, value.local, dst_ty, origin)
                } else {
                    // Non-pointer-to-pointer casts model raw address construction
                    // such as integer-to-pointer. Address-of or borrow-to-pointer
                    // lowering must preserve Local/Provider provenance before it
                    // reaches this fallback.
                    self.assign_unknown_pointer_targets(state, dst, dst_ty);
                    Ok(())
                }
            }
            NExpr::Call {
                args, effect_args, ..
            } => {
                let summary = call_summary.expect("call expression must have a memory summary");
                let dst_bases = self.canon.canonicalize_value_base(state, dst);
                for (output, targets) in self.summaries.resolve_return_pointer_targets(
                    state,
                    args,
                    effect_args,
                    summary,
                    origin,
                )? {
                    for base in &dst_bases {
                        let key = self
                            .canon
                            .pointer_slot_place(base.root.clone(), base.proj.concat(&output));
                        state.assign_pointer_targets(key, targets.clone());
                    }
                }
                Ok(())
            }
            _ => {
                self.assign_unknown_pointer_targets(state, dst, dst_ty);
                Ok(())
            }
        }
    }

    fn apply_pointer_stores(&self, state: &mut State<'db>, stores: Vec<ResolvedPointerStore<'db>>) {
        for store in stores {
            let weak = store.weak || self.write_precision(&store.written) == WritePrecision::Weak;
            for place in store.written {
                state.update_pointer_targets(
                    self.canon.pointer_slot_place(place.root, place.proj),
                    store.targets.clone(),
                    weak,
                );
            }
        }
    }

    fn clear_pointer_targets_for_local(&self, state: &mut State<'db>, local: SLocalId) {
        let Some(local_data) = self.canon.body.local(local) else {
            return;
        };
        for base in self.canon.structural_local_places(local) {
            self.clear_physical_pointer_slots(state, &base, local_data.ty);
        }
    }

    fn copy_pointer_targets_from_value(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        src: SLocalId,
        dst_ty: TyId<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let dst_bases = self.canon.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.canon.db, dst_ty) {
            let targets = self
                .canon
                .pointer_targets_for_value_path(state, src, &slot.path, origin)?;
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.canon
                        .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn copy_pointer_targets_from_place(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        src: &NSPlace<'db>,
        dst_ty: TyId<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let dst_bases = self.canon.canonicalize_value_base(state, dst);
        let src_bases = self.canon.canonicalize_place(state, src, origin)?;
        for slot in pointer_slots(self.canon.db, dst_ty) {
            let keys = self
                .canon
                .canonicalize_path_from_places(&src_bases, &slot.path, state, origin)?;
            let targets = self.canon.pointer_targets_for_keys(state, keys, origin)?;
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.canon
                        .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn assign_array_repeat_pointer_targets(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        ty: TyId<'db>,
        value: NOperand,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let dst_bases = self.canon.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.canon.db, ty) {
            let Some(suffix) = split_array_slot(&slot.path, false) else {
                continue;
            };
            let targets =
                self.canon
                    .pointer_targets_for_value_path(state, value.local, &suffix, origin)?;
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.canon
                        .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn assign_aggregate_pointer_targets(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        ty: TyId<'db>,
        fields: &[NOperand],
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let dst_bases = self.canon.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.canon.db, ty) {
            let targets = if let Some((field_idx, suffix)) = split_aggregate_slot(&slot.path) {
                let Some(field) = fields.get(field_idx) else {
                    continue;
                };
                self.canon
                    .pointer_targets_for_value_path(state, field.local, &suffix, origin)?
            } else if let Some(suffix) = split_array_slot(&slot.path, true) {
                let mut targets = PointerTargets::default();
                for field in fields {
                    let field_targets = self.canon.pointer_targets_for_value_path(
                        state,
                        field.local,
                        &suffix,
                        origin,
                    )?;
                    targets.join_into(&field_targets);
                }
                targets
            } else {
                continue;
            };
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.canon
                        .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn assign_enum_pointer_targets(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        enum_ty: TyId<'db>,
        variant: VariantIndex,
        fields: &[NOperand],
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let dst_bases = self.canon.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.canon.db, enum_ty) {
            let Some((field_idx, suffix)) = split_enum_slot(&slot.path, variant) else {
                continue;
            };
            let Some(field) = fields.get(field_idx) else {
                continue;
            };
            let targets =
                self.canon
                    .pointer_targets_for_value_path(state, field.local, &suffix, origin)?;
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.canon
                        .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn copy_pointer_targets_from_enum_field(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        value: NOperand,
        variant: VariantIndex,
        field: FieldIndex,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let Some(enum_ty) = self.canon.body.local(value.local).map(|local| local.ty) else {
            return Ok(());
        };
        let Some(dst_ty) = self.canon.body.local(dst).map(|local| local.ty) else {
            return Ok(());
        };
        let field_path = NSProjectionPath::from_projection(Projection::VariantField {
            enum_ty,
            variant,
            field_idx: field.0 as usize,
        });
        let dst_bases = self.canon.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.canon.db, dst_ty) {
            let path = field_path.concat(&slot.path);
            let targets =
                self.canon
                    .pointer_targets_for_value_path(state, value.local, &path, origin)?;
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.canon
                        .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn assign_unknown_pointer_targets(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        dst_ty: TyId<'db>,
    ) {
        let dst_bases = self.canon.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.canon.db, dst_ty) {
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.canon
                        .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    PointerTargets::unknown(PointerAddressSpaces::one(
                        ProviderAddressSpace::Memory,
                    )),
                );
            }
        }
    }

    pub(in super::super) fn seed_param_pointer_targets(
        &self,
        state: &mut State<'db>,
        local: SLocalId,
        param_idx: u32,
    ) {
        let Some(local_data) = self.canon.body.local(local) else {
            return;
        };
        let base = CanonPlace {
            root: BorrowRoot::Param(param_idx),
            proj: NSProjectionPath::default(),
        };
        self.seed_structural_pointer_targets(state, base, local_data.ty);
    }

    fn seed_structural_pointer_targets(
        &self,
        state: &mut State<'db>,
        base: CanonPlace<'db>,
        ty: TyId<'db>,
    ) {
        for slot in pointer_slots(self.canon.db, ty) {
            let key = self
                .canon
                .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path));
            let target = CanonPlace {
                root: base.root.clone(),
                proj: base.proj.concat(&slot.path).concat(&slot.target_suffix),
            };
            state
                .assign_pointer_targets(key, PointerTargets::known(FxHashSet::from_iter([target])));
        }
    }

    pub(in super::super) fn apply_stmt(
        &self,
        state: &mut State<'db>,
        stmt: &NSStmt<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                let call_summary = if let NExpr::Call {
                    callee,
                    args,
                    effect_args,
                    ..
                } = expr
                {
                    let updates = self.summaries.resolve_call_updates(
                        state,
                        callee,
                        args,
                        effect_args,
                        stmt.origin,
                    )?;
                    self.apply_pointer_stores(state, updates.stores);
                    if !updates.may_return {
                        state.mark_unreachable();
                        return Ok(());
                    }
                    Some(updates.summary)
                } else {
                    None
                };
                if self
                    .canon
                    .body
                    .local(*dst)
                    .is_some_and(|local| local.ty.as_borrow(self.canon.db).is_none())
                {
                    self.clear_pointer_targets_for_local(state, *dst);
                }
                let loans = match expr {
                    NExpr::Use(src) => {
                        let loans = state.loans_in(src.local);
                        if loans.is_empty() {
                            self.loan_for_local
                                .get(dst)
                                .copied()
                                .map(|loan| FxHashSet::from_iter([loan]))
                                .unwrap_or_default()
                        } else {
                            loans
                        }
                    }
                    NExpr::Borrow { .. } | NExpr::Call { .. } => self
                        .loan_for_local
                        .get(dst)
                        .copied()
                        .map(|loan| FxHashSet::from_iter([loan]))
                        .unwrap_or_default(),
                    _ => FxHashSet::default(),
                };
                state.assign_loans(*dst, loans);
                self.assign_pointer_targets_for_expr(state, *dst, expr, call_summary, stmt.origin)
            }
            NSStmtKind::Store { dst, src } => {
                let Some(src_ty) = self.canon.body.local(src.local).map(|local| local.ty) else {
                    return Ok(());
                };
                let written = self.canon.canonicalize_place(state, dst, stmt.origin)?;
                let precision = self.write_precision(&written);
                if !is_pointer_bearing_type(self.canon.db, src_ty) {
                    self.invalidate_scalar_store_pointer_targets(state, &written, precision);
                    return Ok(());
                }
                if precision == WritePrecision::Strong {
                    for place in &written {
                        self.clear_physical_pointer_slots(state, place, src_ty);
                    }
                }
                for slot in pointer_slots(self.canon.db, src_ty) {
                    let targets = self.canon.pointer_targets_for_value_path(
                        state,
                        src.local,
                        &slot.path,
                        stmt.origin,
                    )?;
                    for base in &written {
                        let key = self
                            .canon
                            .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path));
                        state.update_pointer_targets(
                            key,
                            targets.clone(),
                            precision == WritePrecision::Weak,
                        );
                    }
                }
                Ok(())
            }
        }
    }

    fn invalidate_scalar_store_pointer_targets(
        &self,
        state: &mut State<'db>,
        written: &FxHashSet<CanonPlace<'db>>,
        precision: WritePrecision,
    ) {
        for place in written {
            let key = self
                .canon
                .pointer_slot_place(place.root.clone(), place.proj.clone());
            state.invalidate_pointer_targets(
                key,
                PointerTargets::unknown(PointerAddressSpaces::one(ProviderAddressSpace::Memory)),
                precision == WritePrecision::Weak,
            );
        }
    }

    fn write_precision(&self, written: &FxHashSet<CanonPlace<'db>>) -> WritePrecision {
        if written.len() == 1
            && written.iter().all(|place| {
                self.canon
                    .pointer_slot_place(place.root.clone(), place.proj.clone())
                    .is_precise_slot()
            })
        {
            WritePrecision::Strong
        } else {
            WritePrecision::Weak
        }
    }

    fn clear_physical_pointer_slots(
        &self,
        state: &mut State<'db>,
        base: &CanonPlace<'db>,
        ty: TyId<'db>,
    ) {
        for slot in pointer_slots(self.canon.db, ty) {
            let key = self
                .canon
                .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path));
            if !key.may_name_multiple_slots() {
                state.clear_pointer_target(&key);
            }
        }
    }
}

fn split_aggregate_slot<'db>(
    path: &NSProjectionPath<'db>,
) -> Option<(usize, NSProjectionPath<'db>)> {
    let mut iter = path.iter();
    let field_idx = match iter.next()? {
        Projection::Field(field_idx) => *field_idx,
        Projection::Index(IndexSource::Constant(idx)) => *idx,
        _ => return None,
    };
    let mut suffix = NSProjectionPath::default();
    for projection in iter {
        suffix.push(projection.clone());
    }
    Some((field_idx, suffix))
}

fn split_array_slot<'db>(
    path: &NSProjectionPath<'db>,
    wildcard_only: bool,
) -> Option<NSProjectionPath<'db>> {
    let mut iter = path.iter();
    let is_array_slot = match iter.next()? {
        Projection::Index(IndexSource::Any) => true,
        Projection::Index(_) => !wildcard_only,
        _ => false,
    };
    if !is_array_slot {
        return None;
    }
    let mut suffix = NSProjectionPath::default();
    for projection in iter {
        suffix.push(projection.clone());
    }
    Some(suffix)
}

fn split_enum_slot<'db>(
    path: &NSProjectionPath<'db>,
    expected_variant: VariantIndex,
) -> Option<(usize, NSProjectionPath<'db>)> {
    let mut iter = path.iter();
    let field_idx = match iter.next()? {
        Projection::VariantField {
            variant, field_idx, ..
        } if *variant == expected_variant => *field_idx,
        _ => return None,
    };
    let mut suffix = NSProjectionPath::default();
    for projection in iter {
        suffix.push(projection.clone());
    }
    Some((field_idx, suffix))
}
