//! Operation meaning shared by liveness, conflicts, availability, and summaries.
//!
//! Address computation reads its carriers and indices. Access to the selected
//! contents is separate: a store can reinitialize them without reading them,
//! whereas even a non-consuming view requires an available source.
use cranelift_entity::EntityRef;

use super::{
    NDataPath, NDataProjection, NDropTarget, NEffectArgValue, NExpr, NIndex, NOperand, NPlace,
    NPlaceBase, NStatementKind, NStructuralPath, NSuccessor, NTerminatorKind, NValueId,
    NormalizedBody, ReadMode,
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        corelib::MemoryAccessKind,
        ty_def::{BorrowKind, TyId},
    },
};

#[derive(Clone, Copy, Debug)]
pub enum AccessTarget<'a, 'db> {
    Value {
        operand: NOperand,
        /// A projected move consumes only this portion of the value.
        path: Option<&'a NStructuralPath>,
    },
    Place(&'a NPlace<'db>),
}

#[derive(Clone, Copy, Debug)]
pub struct OperationAccess<'a, 'db> {
    pub target: AccessTarget<'a, 'db>,
    pub kind: MemoryAccessKind,
}

impl<'a, 'db> OperationAccess<'a, 'db> {
    fn operand(db: &'db dyn HirAnalysisDb, body: &NormalizedBody<'db>, operand: NOperand) -> Self {
        Self {
            target: AccessTarget::Value {
                operand,
                path: None,
            },
            kind: read_access(db, body.values[operand.value.index()].ty, operand.mode),
        }
    }
}

fn read_access(db: &dyn HirAnalysisDb, ty: TyId<'_>, mode: ReadMode) -> MemoryAccessKind {
    if mode == ReadMode::Move && ty.as_capability(db).is_none() {
        MemoryAccessKind::Move
    } else {
        MemoryAccessKind::Read
    }
}

impl<'db> NStatementKind<'db> {
    pub fn accesses<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        body: &NormalizedBody<'db>,
    ) -> Vec<OperationAccess<'a, 'db>> {
        let mut accesses = Vec::new();
        match self {
            // The access checker gives a drop its own meaning: it consumes
            // whatever is initialized, which no operand access describes.
            Self::End { .. } | Self::Drop { .. } => {}
            Self::Store { destination, value } => {
                accesses.push(OperationAccess::operand(db, body, *value));
                accesses.push(OperationAccess {
                    target: AccessTarget::Place(destination),
                    kind: MemoryAccessKind::Write,
                });
            }
            Self::Define { expr, .. } => match expr {
                NExpr::Load { place, mode } => accesses.push(OperationAccess {
                    target: AccessTarget::Place(place),
                    kind: read_access(db, place.ty, *mode),
                }),
                NExpr::MakeView { place, .. } => accesses.push(OperationAccess {
                    target: AccessTarget::Place(place),
                    kind: MemoryAccessKind::Read,
                }),
                NExpr::Borrow { place, kind, .. } => accesses.push(OperationAccess {
                    target: AccessTarget::Place(place),
                    kind: match kind {
                        BorrowKind::Ref => MemoryAccessKind::Read,
                        BorrowKind::Mut => MemoryAccessKind::MutAccess,
                    },
                }),
                NExpr::ProjectValue { value, path } => {
                    let mut access = OperationAccess::operand(db, body, *value);
                    access.target = AccessTarget::Value {
                        operand: *value,
                        path: Some(path),
                    };
                    accesses.push(access);
                }
                NExpr::Call {
                    args, effect_args, ..
                } => {
                    accesses.extend(
                        args.iter()
                            .map(|arg| OperationAccess::operand(db, body, *arg)),
                    );
                    for effect in effect_args {
                        accesses.push(match &effect.arg {
                            NEffectArgValue::Value(value) => {
                                OperationAccess::operand(db, body, *value)
                            }
                            NEffectArgValue::Place(place) => OperationAccess {
                                target: AccessTarget::Place(place),
                                kind: if effect.required_mut {
                                    MemoryAccessKind::MutAccess
                                } else {
                                    MemoryAccessKind::Read
                                },
                            },
                        });
                    }
                }
                NExpr::Forward { src }
                | NExpr::StructuralRepack { value: src, .. }
                | NExpr::Unary { value: src, .. }
                | NExpr::PointerCast { value: src, .. }
                | NExpr::ScalarCast { value: src, .. }
                | NExpr::ArrayRepeat { value: src, .. }
                | NExpr::GetEnumTag { value: src }
                | NExpr::IsEnumVariant { value: src, .. } => {
                    accesses.push(OperationAccess::operand(db, body, *src));
                }
                NExpr::Binary { lhs, rhs, .. } => {
                    accesses.extend([lhs, rhs].map(|arg| OperationAccess::operand(db, body, *arg)));
                }
                NExpr::AggregateMake { fields, .. } | NExpr::EnumMake { fields, .. } => {
                    accesses.extend(
                        fields
                            .iter()
                            .map(|arg| OperationAccess::operand(db, body, *arg)),
                    );
                }
                NExpr::CodeRegionRef { .. }
                | NExpr::Const(_)
                | NExpr::CodeRegionOffset { .. }
                | NExpr::CodeRegionLen { .. } => {}
            },
        }
        // Address operands never consume the containing value. Their availability
        // is required even for stores that do not require available contents.
        let mut address_values = Vec::new();
        if let Self::Drop {
            target: NDropTarget::Place(place),
        } = self
        {
            address_values.extend(place_address_values(place));
        }
        for access in &accesses {
            match access.target {
                AccessTarget::Place(place) => address_values.extend(place_address_values(place)),
                AccessTarget::Value { path, .. } => {
                    address_values.extend(path.into_iter().flat_map(|path| path_indices(&path.0)));
                }
            }
        }
        accesses.extend(address_values.into_iter().map(|value| OperationAccess {
            target: AccessTarget::Value {
                operand: NOperand {
                    value,
                    origin: None,
                    mode: ReadMode::Read,
                },
                path: None,
            },
            kind: MemoryAccessKind::Read,
        }));
        accesses
    }
}

/// The values computing a place's address: the carrier or pointer it is
/// reached through, and its dynamic indices.
fn place_address_values<'a>(place: &'a NPlace<'_>) -> impl Iterator<Item = NValueId> + 'a {
    let base = match place.base {
        NPlaceBase::CapabilityTarget { carrier: value } | NPlaceBase::Deref { pointer: value } => {
            Some(value)
        }
        NPlaceBase::Root(_) => None,
    };
    base.into_iter().chain(path_indices(&place.path))
}

pub fn path_indices(path: &NDataPath) -> impl Iterator<Item = NValueId> + '_ {
    path.iter().filter_map(|projection| match projection {
        NDataProjection::Index(NIndex::Value(value)) | NDataProjection::Entry(value) => {
            Some(*value)
        }
        NDataProjection::Index(NIndex::Const(_))
        | NDataProjection::Field(_)
        | NDataProjection::VariantField { .. } => None,
    })
}

impl<'db> NTerminatorKind<'db> {
    /// The condition or returned value, excluding edge-specific phi arguments.
    pub fn access<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        body: &NormalizedBody<'db>,
    ) -> Option<OperationAccess<'a, 'db>> {
        match self {
            Self::Branch { cond, .. }
            | Self::MatchEnum { value: cond, .. }
            | Self::Return(Some(cond))
            | Self::Yield { value: cond, .. } => Some(OperationAccess::operand(db, body, *cond)),
            Self::Goto(_) | Self::Assert { .. } | Self::Return(None) => None,
        }
    }
}

impl NSuccessor {
    /// Phi transfers evaluate all arguments before assigning the block parameters.
    pub fn accesses<'a, 'db>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        body: &'a NormalizedBody<'db>,
    ) -> impl Iterator<Item = OperationAccess<'a, 'db>> + 'a
    where
        'db: 'a,
    {
        self.args
            .iter()
            .map(|operand| OperationAccess::operand(db, body, *operand))
    }
}
