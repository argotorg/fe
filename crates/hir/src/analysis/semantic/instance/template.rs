use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            const_ty::ConstTyData,
            fold::{TyFoldable, TyFolder},
            trait_def::TraitInstId,
            trait_resolution::PredicateListId,
            ty_check::{BodyOwner, EffectProviderSpecialization, TypedBody, infer_body},
            ty_def::{TyData, TyId},
        },
    },
    hir_def::scope_graph::ScopeId,
};

#[derive(Clone, Debug)]
pub struct TypedBodyTemplate<'db> {
    pub owner: BodyOwner<'db>,
    pub body: TypedBody<'db>,
}

pub fn typed_body_template<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> TypedBodyTemplate<'db> {
    let typed_body = infer_body(db, owner).1.clone();

    TypedBodyTemplate {
        owner,
        body: typed_body,
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct GenericSubst<'db> {
    #[return_ref]
    pub generic_args: Vec<TyId<'db>>,
}

impl<'db> GenericSubst<'db> {
    pub fn empty(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new())
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct ImplEnv<'db> {
    pub normalization_scope: ScopeId<'db>,
    pub assumptions: PredicateListId<'db>,
    #[return_ref]
    pub witnesses: Vec<TraitInstId<'db>>,
}

impl<'db> ImplEnv<'db> {
    pub fn empty(db: &'db dyn HirAnalysisDb, normalization_scope: ScopeId<'db>) -> Self {
        Self::new(
            db,
            normalization_scope,
            PredicateListId::empty_list(db),
            Vec::new(),
        )
    }

    /// Canonical environment for a trait method that has been resolved to a
    /// concrete impl body: the impl body's own scope, no caller assumptions,
    /// and just the single resolving witness. Keeping this independent of the
    /// caller lets identical resolved instances deduplicate.
    pub fn for_resolved_trait_method(
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
        witness: TraitInstId<'db>,
    ) -> Self {
        Self::new(
            db,
            owner.scope(),
            PredicateListId::empty_list(db),
            vec![witness],
        )
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct EffectProviderSubst<'db> {
    #[return_ref]
    pub providers: Vec<EffectProviderSpecialization<'db>>,
}

impl<'db> EffectProviderSubst<'db> {
    pub fn empty(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new())
    }
}

pub fn instantiate_typed_body<'db>(
    db: &'db dyn HirAnalysisDb,
    template: TypedBodyTemplate<'db>,
    subst: GenericSubst<'db>,
) -> TypedBody<'db> {
    instantiate_with_generic_args(db, template.body, subst.generic_args(db))
}

pub fn instantiate_with_generic_args<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    generic_args: &[TyId<'db>],
) -> T
where
    T: TyFoldable<'db>,
{
    let mut folder = GenericInstantiator { generic_args };
    value.fold_with(db, &mut folder)
}

struct GenericInstantiator<'a, 'db> {
    generic_args: &'a [TyId<'db>],
}

impl<'db> TyFolder<'db> for GenericInstantiator<'_, 'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            TyData::TyParam(param) => self.generic_args.get(param.idx).copied().unwrap_or(ty),
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(param, _) = const_ty.data(db)
                    && let Some(replacement) = self.generic_args.get(param.idx).copied()
                {
                    replacement
                } else {
                    ty.super_fold_with(db, self)
                }
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}
