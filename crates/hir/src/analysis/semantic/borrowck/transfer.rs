use std::{cell::RefCell, fmt, rc::Rc};

use cranelift_entity::SecondaryMap;
use dataflow::JoinSemiLattice;
use rustc_hash::FxHashMap;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{LayoutBackingProjection, SLocalId},
        ty::ty_is_noesc,
    },
    projection::{IndexSource, Projection},
};

use super::{
    guard::{ExistentialId, IndexExpr, ValueScope},
    ir::{
        NBorrowRoot, NBorrowRootId, NExpr, NSPlace, NSPlaceRoot, NSProjectionPath, NSStmt,
        NSStmtKind, NormalizedSemanticBody, layout_path_for_semantic_projection,
        semantic_projection_ty,
    },
    loan::{LoanId, LoanRef},
    shape::{FieldKey, ShapeChildren, ShapeId, SlotPath, SlotProjection, capability_shape},
    summary::{SummaryPath, SummaryProjection},
    value::{GuardedLeaf, ValueId, ValueInterner},
};

pub(crate) type BorrowStateValueId<'db> = ValueId<'db, LoanRef>;
pub(crate) type BorrowValueInterner<'db> = ValueInterner<'db, LoanRef>;
pub(crate) type SharedBorrowValueInterner<'db> = Rc<RefCell<BorrowValueInterner<'db>>>;

pub(crate) fn shared_value_interner<'db>(
    db: &'db dyn HirAnalysisDb,
) -> SharedBorrowValueInterner<'db> {
    Rc::new(RefCell::new(ValueInterner::new(db)))
}

pub(super) struct BorrowTransferCx<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    body: &'a NormalizedSemanticBody<'db>,
    loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
    constant_indices: &'a SecondaryMap<SLocalId, Option<usize>>,
}

impl<'a, 'db> BorrowTransferCx<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        body: &'a NormalizedSemanticBody<'db>,
        loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
        constant_indices: &'a SecondaryMap<SLocalId, Option<usize>>,
    ) -> Self {
        Self {
            db,
            body,
            loan_for_local,
            constant_indices,
        }
    }

    pub(super) fn apply_stmt(
        &self,
        state: &mut BorrowState<'db>,
        stmt: &NSStmt<'db>,
        call_result_loans: Option<&[(SummaryPath, LoanId)]>,
    ) {
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                let Some(dst_shape) = self.local_shape(*dst) else {
                    state.clear(*dst);
                    return;
                };
                let value = match expr {
                    NExpr::Use(src) => self.own_loan_value(state, *dst).unwrap_or_else(|| {
                        self.propagated_value(state, *dst, state.value(src.local))
                    }),
                    NExpr::Borrow { .. } => self
                        .own_loan_value(state, *dst)
                        .unwrap_or_else(|| state.empty(dst_shape)),
                    NExpr::Call { .. } => {
                        if let Some(own) = self.own_loan_value(state, *dst) {
                            own
                        } else if let Some(call_result_loans) = call_result_loans {
                            summary_loan_value(
                                state.interner(),
                                dst_shape,
                                call_result_loans.iter().map(|(path, loan)| {
                                    (path.clone(), LoanRef::for_summary(*loan, path))
                                }),
                            )
                            .unwrap_or_else(|| state.empty(dst_shape))
                        } else {
                            state.empty(dst_shape)
                        }
                    }
                    NExpr::AggregateMake { fields, .. } => {
                        match &dst_shape.data(self.db).children {
                            ShapeChildren::Product {
                                fields: shape_fields,
                            } => {
                                let fields = shape_fields
                                    .iter()
                                    .enumerate()
                                    .map(|(idx, (key, shape))| {
                                        let value = fields
                                            .get(idx)
                                            .and_then(|field| state.value(field.local))
                                            .unwrap_or_else(|| state.empty(*shape));
                                        (*key, value)
                                    })
                                    .collect::<Vec<_>>();
                                state.product(dst_shape, fields)
                            }
                            ShapeChildren::Array { elem, .. } => {
                                let fields = fields
                                    .iter()
                                    .enumerate()
                                    .map(|(idx, field)| {
                                        (
                                            idx,
                                            state
                                                .value(field.local)
                                                .unwrap_or_else(|| state.empty(*elem)),
                                        )
                                    })
                                    .collect::<Vec<_>>();
                                state.array_exact(dst_shape, fields)
                            }
                            ShapeChildren::None | ShapeChildren::Sum { .. } => {
                                state.empty(dst_shape)
                            }
                        }
                    }
                    NExpr::EnumMake {
                        variant, fields, ..
                    } => {
                        let ShapeChildren::Sum { variants } = &dst_shape.data(self.db).children
                        else {
                            return state.assign(*dst, state.empty(dst_shape));
                        };
                        let Some(variant_shape) = variants.iter().find_map(|(candidate, shape)| {
                            (*candidate == *variant).then_some(*shape)
                        }) else {
                            return state.assign(*dst, state.empty(dst_shape));
                        };
                        let ShapeChildren::Product {
                            fields: shape_fields,
                        } = &variant_shape.data(self.db).children
                        else {
                            return state.assign(*dst, state.empty(dst_shape));
                        };
                        let fields = shape_fields
                            .iter()
                            .enumerate()
                            .map(|(idx, (key, shape))| {
                                let value = fields
                                    .get(idx)
                                    .and_then(|field| state.value(field.local))
                                    .unwrap_or_else(|| state.empty(*shape));
                                (*key, value)
                            })
                            .collect::<Vec<_>>();
                        let variant_value = state.product(variant_shape, fields);
                        state.sum_variant(dst_shape, *variant, variant_value)
                    }
                    NExpr::ArrayRepeat { value, .. } => {
                        let ShapeChildren::Array { elem, .. } = dst_shape.data(self.db).children
                        else {
                            return state.assign(*dst, state.empty(dst_shape));
                        };
                        let value = state
                            .value(value.local)
                            .unwrap_or_else(|| state.empty(elem));
                        state.array_repeat(dst_shape, value)
                    }
                    NExpr::ExtractEnumField {
                        value,
                        variant,
                        field,
                    } => {
                        let projected = self.local_shape(value.local).and_then(|shape| {
                            let projection = [LayoutBackingProjection::VariantField {
                                variant: *variant,
                                field: *field,
                            }];
                            let path = slot_path_for_layout(self.db, shape, &projection)?;
                            state.project(value.local, &path, ValueScope::Local(value.local))
                        });
                        self.propagated_value(state, *dst, projected)
                    }
                    NExpr::ReadPlace { place, .. } => {
                        self.own_loan_value(state, *dst).unwrap_or_else(|| {
                            let projected = self.place_base_local(place).and_then(|base| {
                                let shape = self.local_shape(base)?;
                                let projection = self.layout_path(&place.path)?;
                                let path = slot_path_for_layout(self.db, shape, &projection)?;
                                state.project(base, &path, ValueScope::Local(base))
                            });
                            self.propagated_value(state, *dst, projected)
                        })
                    }
                    _ => state.empty(dst_shape),
                };
                state.assign(*dst, value);
            }
            NSStmtKind::Store { dst, src } => {
                if let NSPlaceRoot::Root(root) = dst.root
                    && let Some(base) = self.root_base_local(root)
                {
                    let path = self.materialize_constant_indices(&dst.path);
                    if self
                        .body
                        .place_root_ty(&dst.root)
                        .and_then(|ty| semantic_projection_ty(self.db, ty, &path))
                        .is_some_and(|(_, traverses_capability)| traverses_capability)
                    {
                        return;
                    }
                    let (Some(base_shape), Some(projection), Some(src_shape)) = (
                        self.local_shape(base),
                        layout_path_for_semantic_projection(&path),
                        self.local_shape(src.local),
                    ) else {
                        return;
                    };
                    let Some(path) = slot_path_for_layout(self.db, base_shape, &projection) else {
                        return;
                    };
                    let replacement = state
                        .value(src.local)
                        .unwrap_or_else(|| state.empty(src_shape));
                    state.replace(base, base_shape, &path, replacement);
                }
            }
        }
    }

    fn propagated_value(
        &self,
        state: &BorrowState<'db>,
        dst: SLocalId,
        value: Option<BorrowStateValueId<'db>>,
    ) -> BorrowStateValueId<'db> {
        let shape = self
            .local_shape(dst)
            .expect("normalized destination local must have a capability shape");
        if self.body.local(dst).is_some_and(|local| {
            local.ty.as_capability(self.db).is_some() || ty_is_noesc(self.db, local.ty)
        }) && let Some(value) = value
            && state.interner().borrow().shape(value) == shape
        {
            value
        } else {
            state.empty(shape)
        }
    }

    fn own_loan_value(
        &self,
        state: &BorrowState<'db>,
        local: SLocalId,
    ) -> Option<BorrowStateValueId<'db>> {
        let shape = self.local_shape(local)?;
        self.loan_for_local
            .get(&local)
            .copied()
            .map(|loan| state.direct_loan(shape, loan))
    }

    fn local_shape(&self, local: SLocalId) -> Option<ShapeId<'db>> {
        self.body
            .local(local)
            .map(|local| capability_shape(self.db, local.ty))
    }

    fn root_base_local(&self, root: NBorrowRootId) -> Option<SLocalId> {
        match self.body.root(root)? {
            NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => Some(*local),
            NBorrowRoot::Provider { .. } => None,
        }
    }

    fn place_base_local(&self, place: &NSPlace<'db>) -> Option<SLocalId> {
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => Some(local),
            NSPlaceRoot::Root(root) => self.root_base_local(root),
        }
    }

    fn materialize_constant_indices(&self, path: &NSProjectionPath<'db>) -> NSProjectionPath<'db> {
        let mut out = NSProjectionPath::new();
        for projection in path.iter() {
            out.push(match projection {
                Projection::Index(IndexSource::Dynamic(index))
                    if let Some(index) = self.constant_indices[*index] =>
                {
                    Projection::Index(IndexSource::Constant(index))
                }
                projection => projection.clone(),
            });
        }
        out
    }

    fn layout_path(&self, path: &NSProjectionPath<'db>) -> Option<Vec<LayoutBackingProjection>> {
        layout_path_for_semantic_projection(&self.materialize_constant_indices(path))
    }
}

#[derive(Clone)]
pub(crate) struct BorrowState<'db> {
    values: FxHashMap<SLocalId, BorrowStateValueId<'db>>,
    interner: SharedBorrowValueInterner<'db>,
}

impl fmt::Debug for BorrowState<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("BorrowState")
            .field("values", &self.values)
            .finish_non_exhaustive()
    }
}

impl PartialEq for BorrowState<'_> {
    fn eq(&self, other: &Self) -> bool {
        debug_assert!(Rc::ptr_eq(&self.interner, &other.interner));
        self.values == other.values
    }
}

impl Eq for BorrowState<'_> {}

impl<'db> BorrowState<'db> {
    pub(crate) fn new(interner: SharedBorrowValueInterner<'db>) -> Self {
        Self {
            values: FxHashMap::default(),
            interner,
        }
    }

    pub(crate) fn value(&self, local: SLocalId) -> Option<BorrowStateValueId<'db>> {
        self.values.get(&local).copied()
    }

    pub(crate) fn interner(&self) -> &SharedBorrowValueInterner<'db> {
        &self.interner
    }

    pub(crate) fn empty(&self, shape: ShapeId<'db>) -> BorrowStateValueId<'db> {
        self.interner.borrow_mut().empty(shape)
    }

    pub(crate) fn direct_loan(&self, shape: ShapeId<'db>, loan: LoanId) -> BorrowStateValueId<'db> {
        direct_loan_value(&self.interner, shape, loan)
    }

    pub(crate) fn product(
        &self,
        shape: ShapeId<'db>,
        fields: impl IntoIterator<Item = (FieldKey, BorrowStateValueId<'db>)>,
    ) -> BorrowStateValueId<'db> {
        self.interner.borrow_mut().product(shape, fields)
    }

    pub(crate) fn sum_variant(
        &self,
        shape: ShapeId<'db>,
        variant: crate::analysis::semantic::VariantIndex,
        value: BorrowStateValueId<'db>,
    ) -> BorrowStateValueId<'db> {
        self.interner
            .borrow_mut()
            .sum_variant(shape, variant, value)
    }

    pub(crate) fn array_repeat(
        &self,
        shape: ShapeId<'db>,
        value: BorrowStateValueId<'db>,
    ) -> BorrowStateValueId<'db> {
        self.interner.borrow_mut().array_repeat(shape, value)
    }

    pub(crate) fn array_exact(
        &self,
        shape: ShapeId<'db>,
        values: impl IntoIterator<Item = (usize, BorrowStateValueId<'db>)>,
    ) -> BorrowStateValueId<'db> {
        self.interner.borrow_mut().array_exact(shape, values)
    }

    pub(crate) fn assign(&mut self, local: SLocalId, value: BorrowStateValueId<'db>) {
        if self.interner.borrow().is_empty(value) {
            self.values.remove(&local);
        } else {
            self.values.insert(local, value);
        }
    }

    pub(crate) fn clear(&mut self, local: SLocalId) {
        self.values.remove(&local);
    }

    pub(crate) fn leaves_in(
        &self,
        local: SLocalId,
        scope: ValueScope,
    ) -> Vec<GuardedLeaf<LoanRef>> {
        self.value(local).map_or_else(Vec::new, |value| {
            self.interner.borrow().enumerate_leaves(value, scope)
        })
    }

    pub(crate) fn leaves(
        &self,
        value: BorrowStateValueId<'db>,
        scope: ValueScope,
    ) -> Vec<GuardedLeaf<LoanRef>> {
        self.interner.borrow().enumerate_leaves(value, scope)
    }

    pub(crate) fn project(
        &self,
        local: SLocalId,
        path: &SlotPath<IndexExpr>,
        scope: ValueScope,
    ) -> Option<BorrowStateValueId<'db>> {
        let value = self.value(local)?;
        Some(self.interner.borrow_mut().project(value, path, scope))
    }

    pub(crate) fn replace(
        &mut self,
        local: SLocalId,
        shape: ShapeId<'db>,
        path: &SlotPath<IndexExpr>,
        replacement: BorrowStateValueId<'db>,
    ) {
        let value = self.value(local).unwrap_or_else(|| self.empty(shape));
        let value = self.interner.borrow_mut().replace(value, path, replacement);
        self.assign(local, value);
    }

    pub(crate) fn locals(&self) -> impl Iterator<Item = SLocalId> + '_ {
        self.values.keys().copied()
    }
}

impl JoinSemiLattice for BorrowState<'_> {
    fn join_into(&mut self, other: &Self) -> bool {
        debug_assert!(Rc::ptr_eq(&self.interner, &other.interner));
        let mut changed = false;
        for (local, other_value) in &other.values {
            let joined = self
                .values
                .get(local)
                .copied()
                .map_or(*other_value, |value| {
                    self.interner.borrow_mut().join(value, *other_value)
                });
            if self.values.insert(*local, joined) != Some(joined) {
                changed = true;
            }
        }
        changed
    }
}

pub(crate) fn direct_loan_value<'db>(
    interner: &SharedBorrowValueInterner<'db>,
    shape: ShapeId<'db>,
    loan: LoanId,
) -> BorrowStateValueId<'db> {
    let mut interner = interner.borrow_mut();
    let empty = interner.empty(shape);
    interner.with_direct(empty, LoanRef::new(loan))
}

pub(crate) fn summary_loan_value<'db>(
    interner: &SharedBorrowValueInterner<'db>,
    shape: ShapeId<'db>,
    leaves: impl IntoIterator<Item = (SummaryPath, LoanRef)>,
) -> Option<BorrowStateValueId<'db>> {
    let db = interner.borrow().db();
    let leaves = leaves
        .into_iter()
        .map(|(path, loan)| Some((slot_path_for_summary(db, shape, &path)?, loan)))
        .collect::<Option<Vec<_>>>()?;
    slot_loan_value(interner, shape, leaves)
}

pub(crate) fn slot_loan_value<'db>(
    interner: &SharedBorrowValueInterner<'db>,
    shape: ShapeId<'db>,
    leaves: impl IntoIterator<Item = (SlotPath<IndexExpr>, LoanRef)>,
) -> Option<BorrowStateValueId<'db>> {
    let leaves = leaves
        .into_iter()
        .map(|(path, payload)| GuardedLeaf {
            path,
            guard: super::guard::Guard::always(),
            payload_guard: super::guard::Guard::always(),
            payload,
        })
        .collect::<Vec<_>>();
    interner.borrow_mut().reconstruct(shape, &leaves)
}

pub(crate) fn slot_path_for_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    shape: ShapeId<'db>,
    path: &SummaryPath,
) -> Option<SlotPath<IndexExpr>> {
    let mut shape = shape;
    let mut steps = Vec::with_capacity(path.as_slice().len());
    for projection in path.as_slice() {
        let (step, child) = match (projection, &shape.data(db).children) {
            (SummaryProjection::Field(field), ShapeChildren::Product { fields }) => {
                let (key, child) = fields.iter().find(|(key, _)| key.index() == *field)?;
                (SlotProjection::Field(*key), *child)
            }
            (
                SummaryProjection::VariantField { variant, field },
                ShapeChildren::Sum { variants },
            ) => {
                let variant_shape = variants
                    .iter()
                    .find_map(|(candidate, shape)| (*candidate == *variant).then_some(*shape))?;
                let ShapeChildren::Product { fields } = &variant_shape.data(db).children else {
                    return None;
                };
                let child = fields
                    .iter()
                    .find_map(|(key, shape)| (key.index() == *field).then_some(*shape))?;
                (
                    SlotProjection::VariantField {
                        variant: *variant,
                        field: *field,
                    },
                    child,
                )
            }
            (SummaryProjection::Index(index), ShapeChildren::Array { elem, .. }) => {
                (SlotProjection::Index(*index), *elem)
            }
            _ => return None,
        };
        steps.push(step);
        shape = child;
    }
    Some(SlotPath::from_steps(steps))
}

pub(crate) fn slot_path_for_layout<'db>(
    db: &'db dyn HirAnalysisDb,
    shape: ShapeId<'db>,
    path: &[LayoutBackingProjection],
) -> Option<SlotPath<IndexExpr>> {
    let mut shape = shape;
    let mut steps = Vec::with_capacity(path.len());
    for (depth, projection) in path.iter().copied().enumerate() {
        let (step, child) = match (projection, &shape.data(db).children) {
            (LayoutBackingProjection::Field(field), ShapeChildren::Product { fields }) => {
                let (key, child) = fields.iter().find(|(key, _)| key.index() == field)?;
                (SlotProjection::Field(*key), *child)
            }
            (
                LayoutBackingProjection::VariantField { variant, field },
                ShapeChildren::Sum { variants },
            ) => {
                let variant_shape = variants
                    .iter()
                    .find_map(|(candidate, shape)| (*candidate == variant).then_some(*shape))?;
                let ShapeChildren::Product { fields } = &variant_shape.data(db).children else {
                    return None;
                };
                let child = fields
                    .iter()
                    .find_map(|(key, shape)| (key.index() == field).then_some(*shape))?;
                (SlotProjection::VariantField { variant, field }, child)
            }
            (LayoutBackingProjection::Index(index), ShapeChildren::Array { elem, .. }) => (
                SlotProjection::Index(index.map_or_else(
                    || IndexExpr::Existential(ExistentialId(depth as u32)),
                    IndexExpr::Const,
                )),
                *elem,
            ),
            (LayoutBackingProjection::IndexFamily(family), ShapeChildren::Array { elem, .. }) => (
                SlotProjection::Index(IndexExpr::ResultParam(super::guard::ResultIndexId(
                    family as u32,
                ))),
                *elem,
            ),
            _ => return None,
        };
        steps.push(step);
        shape = child;
    }
    Some(SlotPath::from_steps(steps))
}
