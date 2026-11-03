use num_traits::ToPrimitive;

use crate::analysis::{
    HirAnalysisDb,
    semantic::SemConstValue,
    ty::{
        const_ty::{ConstTyData, ConstTyId, normalize_const_tys_for_comparison},
        ty_def::{TyData, TyId},
    },
};

/// Array lengths preserve the complete normalized const expression. They never
/// share the namespace of runtime values or lexical array-member binders.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ArrayLength<'db> {
    Known(usize),
    Symbolic(ConstTyId<'db>),
}

impl<'db> ArrayLength<'db> {
    pub(crate) fn from_ty(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<Self> {
        // Deferred defaults and eagerly lowered extents must name the same
        // length in shapes, repacks, and specialization keys.
        let TyData::ConstTy(value) = normalize_const_tys_for_comparison(db, ty).data(db) else {
            return None;
        };
        let value = *value;
        if let Some(integer) = value.integer_value(db) {
            return integer.to_usize().map(Self::Known);
        }
        match value.data(db) {
            ConstTyData::TyParam(..)
            | ConstTyData::Abstract(..)
            | ConstTyData::Computation { .. }
            | ConstTyData::UnEvaluated { .. } => Some(Self::Symbolic(value)),
            ConstTyData::Description(description)
                if matches!(description.value(db), SemConstValue::Description(..)) =>
            {
                Some(Self::Symbolic(value))
            }
            ConstTyData::TyVar(..)
            | ConstTyData::Value(..)
            | ConstTyData::Description(..)
            | ConstTyData::Invalid(..) => None,
        }
    }

    pub fn known(self) -> Option<usize> {
        match self {
            Self::Known(len) => Some(len),
            Self::Symbolic(_) => None,
        }
    }
}
