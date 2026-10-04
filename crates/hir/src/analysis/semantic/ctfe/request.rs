use salsa::Update;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SConst, SemConstId, SemConstValue, SemOrigin, SemanticInstanceKey,
        consts::{
            retype_verified_sem_const, verify_sem_const_description_shape, verify_sem_const_shape,
        },
        sem_const_ty,
    },
    ty::{
        assoc_const::{AssocConstUse, InherentConstUse},
        const_ty::{ConstTyData, ConstTyId, const_ty_from_sem_const},
        fold::{TyFoldable, TyFolder},
        ty_def::{TyData, TyId},
        visitor::{TyVisitable, TyVisitor},
    },
};
use crate::hir_def::scope_graph::ScopeId;

use super::machine::sem_const_dependency;

/// A selected body retains the complete instance environment. An unresolved
/// associated constant retains its contextual selection recipe instead.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum ConstEntry<'db> {
    Resolved(SemanticInstanceKey<'db>),
    Associated(AssocConstUse<'db>),
    Inherent(InherentConstUse<'db>),
}

/// Original owned input descriptions in semantic argument order. No machine
/// reference or mutable location can be represented by this type.
#[salsa::interned]
#[derive(Debug)]
pub struct ConstComputationId<'db> {
    pub entry: ConstEntry<'db>,
    #[return_ref]
    pub inputs: Vec<ConstDesc<'db>>,
    pub result_ty: TyId<'db>,
    pub parameter_owner: ScopeId<'db>,
    pub origin: SemOrigin<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct VerifiedConstValueId<'db>(SemConstId<'db>);

impl<'db> VerifiedConstValueId<'db> {
    pub(crate) fn from_complete_execution(
        db: &'db dyn HirAnalysisDb,
        value: SemConstId<'db>,
    ) -> Result<Self, &'static str> {
        verify_sem_const_shape(db, value)?;
        Ok(Self(value))
    }

    pub fn value(self) -> SemConstId<'db> {
        self.0
    }

    /// Children of a verified aggregate inherit its complete shape proof.
    pub(super) fn aggregate_children(self, db: &'db dyn HirAnalysisDb) -> Vec<Self> {
        match self.0.value(db) {
            SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => {
                elems.iter().copied().map(Self).collect()
            }
            SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => {
                fields.iter().copied().map(Self).collect()
            }
            _ => unreachable!("verified aggregate children requested from scalar"),
        }
    }

    /// One child of a verified aggregate, found without reading its siblings.
    pub(super) fn aggregate_child(self, db: &'db dyn HirAnalysisDb, idx: usize) -> Option<Self> {
        match self.0.value(db) {
            SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => {
                elems.get(idx)
            }
            SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => {
                fields.get(idx)
            }
            _ => unreachable!("verified aggregate child requested from scalar"),
        }
        .copied()
        .map(Self)
    }
}

impl<'db> SConst<'db> {
    /// Classify a source-produced constant before it enters semantic IR.
    /// Invalid typed literals remain explicit for diagnostic recovery.
    pub(crate) fn from_trusted_source(db: &'db dyn HirAnalysisDb, value: SemConstId<'db>) -> Self {
        if sem_const_dependency(db, value).is_some() {
            if verify_sem_const_description_shape(db, value).is_err() {
                return Self::Invalid(value);
            }
            Self::Description(value)
        } else {
            match VerifiedConstValueId::from_complete_execution(db, value) {
                Ok(value) => Self::Value(value),
                Err(_) => Self::Invalid(value),
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum ConstRepr<'db> {
    Value(VerifiedConstValueId<'db>),
    Term(ConstTyId<'db>),
    Deferred(ConstComputationId<'db>),
}

/// Source occurrences for an extracted term. Canonical terms may be shared by
/// distinct uses, so operation origins live on the description instead.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub(crate) struct TermCallFrame<'db> {
    pub origin: SemOrigin<'db>,
    pub callee: SemanticInstanceKey<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub(crate) struct TermProvenance<'db> {
    pub term: ConstTyId<'db>,
    pub origin: SemOrigin<'db>,
    pub operands: Vec<TermProvenance<'db>>,
    pub frames: Vec<TermCallFrame<'db>>,
    pub operation_order: usize,
    pub opaque: bool,
}

impl TermProvenance<'_> {
    pub(crate) fn preserves_source_order(&self, next: &mut usize) -> bool {
        if !self
            .operands
            .iter()
            .all(|operand| operand.preserves_source_order(next))
            || self.operation_order != *next
        {
            return false;
        }
        *next += 1;
        true
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct ConstDesc<'db> {
    ty: TyId<'db>,
    parameter_owner: ScopeId<'db>,
    repr: ConstRepr<'db>,
    term_provenance: Option<TermProvenance<'db>>,
}

impl<'db> ConstDesc<'db> {
    pub fn value(
        db: &'db dyn HirAnalysisDb,
        value: VerifiedConstValueId<'db>,
        parameter_owner: ScopeId<'db>,
    ) -> Self {
        Self {
            ty: sem_const_ty(db, value.value()),
            parameter_owner,
            repr: ConstRepr::Value(value),
            term_provenance: None,
        }
    }

    pub fn term(
        db: &'db dyn HirAnalysisDb,
        parameter_owner: ScopeId<'db>,
        term: ConstTyId<'db>,
    ) -> Self {
        Self {
            ty: term.ty(db),
            parameter_owner,
            repr: ConstRepr::Term(term),
            term_provenance: None,
        }
    }

    pub(crate) fn term_with_provenance(
        db: &'db dyn HirAnalysisDb,
        parameter_owner: ScopeId<'db>,
        provenance: TermProvenance<'db>,
    ) -> Self {
        let mut description = Self::term(db, parameter_owner, provenance.term);
        description.term_provenance = Some(provenance);
        description
    }

    pub fn deferred(db: &'db dyn HirAnalysisDb, computation: ConstComputationId<'db>) -> Self {
        Self {
            ty: computation.result_ty(db),
            parameter_owner: computation.parameter_owner(db),
            repr: ConstRepr::Deferred(computation),
            term_provenance: None,
        }
    }

    pub(crate) fn with_ty(mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<Self> {
        if self.ty == ty {
            return Some(self);
        }
        self.repr = match self.repr {
            ConstRepr::Value(value) => ConstRepr::Value(
                VerifiedConstValueId::from_complete_execution(
                    db,
                    retype_verified_sem_const(db, value.value(), ty)?,
                )
                .ok()?,
            ),
            ConstRepr::Term(term) => {
                let term = term.swap_ty(db, ty);
                if term.ty(db) != ty {
                    return None;
                }
                if let Some(provenance) = &mut self.term_provenance {
                    provenance.term = term;
                }
                ConstRepr::Term(term)
            }
            ConstRepr::Deferred(_) => return None,
        };
        self.ty = ty;
        Some(self)
    }

    pub fn ty(&self) -> TyId<'db> {
        self.ty
    }

    pub fn repr(&self) -> &ConstRepr<'db> {
        &self.repr
    }

    pub fn parameter_owner(&self) -> ScopeId<'db> {
        self.parameter_owner
    }

    pub(crate) fn term_provenance(&self) -> Option<&TermProvenance<'db>> {
        self.term_provenance.as_ref()
    }
}

impl<'db> TyFoldable<'db> for ConstDesc<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let ty = folder.fold_ty(db, self.ty);
        let parameter_owner = folder.fold_scope(self.parameter_owner);
        let repr = match self.repr {
            ConstRepr::Value(value) => {
                let value = folder.fold_ty(
                    db,
                    TyId::const_ty(db, const_ty_from_sem_const(db, value.value())),
                );
                let TyData::ConstTy(value) = value.data(db) else {
                    unreachable!("folded constant value must remain a constant")
                };
                match value.data(db) {
                    ConstTyData::Value(value) => ConstRepr::Value(*value),
                    _ => ConstRepr::Term(*value),
                }
            }
            ConstRepr::Term(term) => {
                let term = folder.fold_ty(db, TyId::const_ty(db, term));
                let TyData::ConstTy(term) = term.data(db) else {
                    unreachable!("folded constant term must remain a constant")
                };
                ConstRepr::Term(*term)
            }
            ConstRepr::Deferred(computation) => {
                ConstRepr::Deferred(computation.fold_with(db, folder))
            }
        };
        Self {
            ty,
            parameter_owner,
            repr,
            term_provenance: self
                .term_provenance
                .map(|provenance| provenance.fold_with(db, folder)),
        }
    }
}

impl<'db> TyVisitable<'db> for ConstDesc<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.ty.visit_with(visitor);
        match self.repr {
            ConstRepr::Value(value) => TyId::const_ty(
                visitor.db(),
                const_ty_from_sem_const(visitor.db(), value.value()),
            )
            .visit_with(visitor),
            ConstRepr::Term(term) => TyId::const_ty(visitor.db(), term).visit_with(visitor),
            ConstRepr::Deferred(computation) => computation.visit_with(visitor),
        }
        if let Some(provenance) = &self.term_provenance {
            provenance.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for TermProvenance<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let term = folder.fold_ty(db, TyId::const_ty(db, self.term));
        let TyData::ConstTy(term) = term.data(db) else {
            unreachable!("folded term occurrence must remain a constant")
        };
        Self {
            term: *term,
            operands: self
                .operands
                .into_iter()
                .map(|operand| operand.fold_with(db, folder))
                .collect(),
            frames: self
                .frames
                .into_iter()
                .map(|frame| TermCallFrame {
                    origin: frame.origin,
                    callee: frame.callee.fold_with(db, folder),
                })
                .collect(),
            ..self
        }
    }
}

impl<'db> TyVisitable<'db> for TermProvenance<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        // Provenance terms also participate in folding and specialization;
        // visit every occurrence in the same order as the folder.
        TyId::const_ty(visitor.db(), self.term).visit_with(visitor);
        for operand in &self.operands {
            operand.visit_with(visitor);
        }
        for frame in &self.frames {
            frame.callee.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for ConstComputationId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let entry = match self.entry(db) {
            ConstEntry::Resolved(key) => ConstEntry::Resolved(key.fold_with(db, folder)),
            ConstEntry::Associated(use_) => ConstEntry::Associated(use_.fold_with(db, folder)),
            ConstEntry::Inherent(use_) => ConstEntry::Inherent(use_.fold_with(db, folder)),
        };
        let inputs = self
            .inputs(db)
            .iter()
            .cloned()
            .map(|input| input.fold_with(db, folder))
            .collect::<Vec<_>>();
        let result_ty = folder.fold_ty(db, self.result_ty(db));
        Self::new(
            db,
            entry,
            inputs,
            result_ty,
            folder.fold_scope(self.parameter_owner(db)),
            self.origin(db),
        )
    }
}

impl<'db> TyVisitable<'db> for ConstComputationId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        let db = visitor.db();
        self.result_ty(db).visit_with(visitor);
        match self.entry(db) {
            ConstEntry::Resolved(key) => key.visit_with(visitor),
            ConstEntry::Associated(use_) => use_.visit_with(visitor),
            ConstEntry::Inherent(use_) => use_.visit_with(visitor),
        }
        for input in self.inputs(db) {
            input.visit_with(visitor);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum ConstUsePolicy {
    AllowDependent,
    RequireValue,
    OptionalFold,
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;

    use super::*;
    use crate::{
        analysis::semantic::{
            SemConstScalar, SemConstValue, array_const, bool_const, bytes_const, int_const,
        },
        test_db::HirAnalysisTestDb,
    };

    #[test]
    fn const_description_visits_provenance_layout_holes_in_fold_order() {
        use crate::analysis::{
            semantic::{EffectProviderSubst, GenericSubst, ImplEnv},
            ty::{
                layout_holes::collect_layout_hole_tys_in_order,
                ty_check::BodyOwner,
                ty_def::{PrimTy, TyBase},
            },
        };

        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("const_provenance.fe".into(), "fn f<const N: usize>() {}");
        let (module, _) = db.top_mod(file);
        let func = module.all_funcs(&db)[0];
        let prim_ty = |prim| TyId::new(&db, TyData::TyBase(TyBase::Prim(prim)));
        let term = ConstTyId::hole_with_ty(&db, TyId::u8(&db));
        let operand = ConstTyId::hole_with_ty(&db, prim_ty(PrimTy::U16));
        let frame_arg = TyId::const_ty(&db, ConstTyId::hole_with_ty(&db, prim_ty(PrimTy::Usize)));
        let description = ConstDesc::term_with_provenance(
            &db,
            func.scope(),
            TermProvenance {
                term,
                origin: SemOrigin::Synthetic,
                operands: vec![TermProvenance {
                    term: operand,
                    origin: SemOrigin::Synthetic,
                    operands: Vec::new(),
                    frames: Vec::new(),
                    operation_order: 0,
                    opaque: false,
                }],
                frames: vec![TermCallFrame {
                    origin: SemOrigin::Synthetic,
                    callee: SemanticInstanceKey::new(
                        &db,
                        BodyOwner::Func(func),
                        GenericSubst::for_owner(&db, func.into(), vec![frame_arg]),
                        EffectProviderSubst::empty(&db),
                        ImplEnv::empty(&db, func.scope()),
                    ),
                }],
                operation_order: 1,
                opaque: false,
            },
        );
        assert_eq!(
            collect_layout_hole_tys_in_order(&db, description),
            vec![
                TyId::u8(&db),
                TyId::u8(&db),
                prim_ty(PrimTy::U16),
                prim_ty(PrimTy::Usize),
            ],
        );
    }

    #[test]
    fn verified_value_rejects_wrong_scalar_and_aggregate_payloads() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "verified_ctfe_payload.fe".into(),
            "const fn array() -> [u8; 2] { [1, 2] }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let owner = crate::analysis::ty::ty_check::BodyOwner::Func(module.all_funcs(&db)[0]);
        let array_ty = crate::analysis::semantic::identity_semantic_instance_key(&db, owner)
            .typed_body(&db)
            .result_ty();
        let u8_ty = TyId::u8(&db);
        let valid = int_const(&db, u8_ty, BigInt::from(7));
        assert!(VerifiedConstValueId::from_complete_execution(&db, valid).is_ok());
        assert!(matches!(
            SConst::from_trusted_source(&db, valid),
            SConst::Value(value) if value.value() == valid
        ));
        let valid_array = array_const(&db, array_ty, vec![valid, valid].into_boxed_slice());
        assert!(VerifiedConstValueId::from_complete_execution(&db, valid_array).is_ok());
        let valid_bytes = bytes_const(&db, array_ty, vec![1, 2]);
        assert!(VerifiedConstValueId::from_complete_execution(&db, valid_bytes).is_ok());

        let wrong_scalar = SemConstId::new(
            &db,
            SemConstValue::Scalar {
                ty: TyId::bool(&db),
                value: SemConstScalar::Int {
                    value: BigInt::from(1),
                },
            },
        );
        assert!(VerifiedConstValueId::from_complete_execution(&db, wrong_scalar).is_err());
        assert!(matches!(
            SConst::from_trusted_source(&db, wrong_scalar),
            SConst::Invalid(value) if value == wrong_scalar
        ));
        let out_of_range = SemConstId::new(
            &db,
            SemConstValue::Scalar {
                ty: u8_ty,
                value: SemConstScalar::Int {
                    value: BigInt::from(256),
                },
            },
        );
        assert!(VerifiedConstValueId::from_complete_execution(&db, out_of_range).is_err());
        let wrong_bytes = bytes_const(&db, array_ty, vec![1]);
        assert!(VerifiedConstValueId::from_complete_execution(&db, wrong_bytes).is_err());
        let wrong_len = array_const(&db, array_ty, vec![valid].into_boxed_slice());
        assert!(VerifiedConstValueId::from_complete_execution(&db, wrong_len).is_err());
        let wrong_element = array_const(
            &db,
            array_ty,
            vec![bool_const(&db, true), valid].into_boxed_slice(),
        );
        assert!(VerifiedConstValueId::from_complete_execution(&db, wrong_element).is_err());
        let symbolic = SemConstId::new(
            &db,
            SemConstValue::Description(ConstTyId::hole_with_ty(&db, u8_ty)),
        );
        assert!(VerifiedConstValueId::from_complete_execution(&db, symbolic).is_err());
        assert!(matches!(
            SConst::from_trusted_source(&db, symbolic),
            SConst::Description(value) if value == symbolic
        ));
    }
}
