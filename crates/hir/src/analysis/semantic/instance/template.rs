use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            const_ty::lexical_const_body_owner,
            provider::AddressSpace,
            subst::substitute_complete,
            trait_def::TraitInstId,
            trait_resolution::PredicateListId,
            ty_check::{BodyOwner, TypedBody, infer_body},
            ty_def::TyId,
            ty_lower::CompleteSubst,
        },
    },
    hir_def::{CallableDef, GenericParamOwner, scope_graph::ScopeId},
    semantic::ProviderBinding,
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
    pub mapping: Option<CompleteSubst<'db>>,
}

impl<'db> GenericSubst<'db> {
    pub fn none(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, None)
    }

    pub fn complete(db: &'db dyn HirAnalysisDb, mapping: CompleteSubst<'db>) -> Self {
        Self::new(db, Some(mapping))
    }

    pub fn for_owner(
        db: &'db dyn HirAnalysisDb,
        owner: GenericParamOwner<'db>,
        args: Vec<TyId<'db>>,
    ) -> Self {
        let mapping = CompleteSubst::for_owner(db, owner, args).unwrap_or_else(|error| {
            panic!("invalid semantic substitution for {owner:?}: {error:?}")
        });
        Self::complete(db, mapping)
    }

    /// The substitution for evaluating `owner` with `args`, one per full-schema
    /// slot of its generic context: a function's own schema, or an anonymous
    /// const body's enclosing declaration. Without arguments the body's
    /// parameters stay symbolic: a function takes its identity substitution.
    pub fn for_body_owner(
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
        args: Vec<TyId<'db>>,
    ) -> Self {
        let generic_owner = match owner {
            // A closure's body shares its parent's generic context.
            BodyOwner::Closure { def, .. } => {
                return BodyOwner::closure_parent(db, def).map_or_else(
                    || Self::none(db),
                    |parent| Self::for_body_owner(db, parent, args),
                );
            }
            BodyOwner::Func(func, _) if args.is_empty() => {
                return Self::for_owner(
                    db,
                    func.into(),
                    CallableDef::Func(func).params(db).to_vec(),
                );
            }
            BodyOwner::Func(func, _) => Some(func.into()),
            BodyOwner::AnonConstBody { body, .. } if !args.is_empty() => {
                lexical_const_body_owner(db, body)
            }
            _ => None,
        };
        if let Some(generic_owner) = generic_owner {
            Self::for_owner(db, generic_owner, args)
        } else {
            assert!(
                args.is_empty(),
                "{owner:?} has no generic context for {args:?}"
            );
            Self::none(db)
        }
    }

    pub fn generic_args(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.mapping(db).as_ref().map_or(&[], CompleteSubst::values)
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct ImplEnv<'db> {
    #[returns(copy)]
    pub normalization_scope: ScopeId<'db>,
    #[returns(copy)]
    pub assumptions: PredicateListId<'db>,
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
}

/// What an instance is specialized on beyond its generic arguments: the
/// providers its effects are bound to, and the address spaces of the places
/// its data parameters name (memory unless listed).
#[salsa::interned]
#[derive(Debug)]
pub struct EffectProviderSubst<'db> {
    pub providers: Vec<ProviderBinding<'db>>,
    pub param_spaces: Vec<(u32, AddressSpace)>,
}

impl<'db> EffectProviderSubst<'db> {
    pub fn empty(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new(), Vec::new())
    }
}

pub fn instantiate_typed_body<'db>(
    db: &'db dyn HirAnalysisDb,
    template: TypedBodyTemplate<'db>,
    subst: GenericSubst<'db>,
) -> TypedBody<'db> {
    if let BodyOwner::Func(func, _) = template.owner {
        let mapping = subst
            .mapping(db)
            .as_ref()
            .expect("function body requires a semantic substitution domain");
        assert_eq!(
            mapping.domain().schema(db).owner(db),
            func.into(),
            "semantic substitution belongs to a different function",
        );
    }
    subst
        .mapping(db)
        .as_ref()
        .map_or(template.body.clone(), |mapping| {
            substitute_complete(db, template.body, mapping)
                .expect("typed body must use its semantic substitution domain")
        })
}
