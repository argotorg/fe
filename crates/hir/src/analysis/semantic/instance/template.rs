use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            const_ty::lexical_const_body_owner,
            subst::substitute_complete,
            trait_def::TraitInstId,
            trait_resolution::PredicateListId,
            ty_check::{
                BodyOwner, EffectProviderSpecialization, TypedBody, check_anon_const_body,
                check_const_body, check_contract_init_body, check_contract_recv_arm_body,
                check_func_body,
            },
            ty_def::TyId,
            ty_lower::CompleteSubst,
        },
    },
    hir_def::{CallableDef, GenericParamOwner, scope_graph::ScopeId},
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
    let typed_body = match owner {
        BodyOwner::Func(func) => check_func_body(db, func).1.clone(),
        BodyOwner::Const(const_) => check_const_body(db, const_).1.clone(),
        BodyOwner::AnonConstBody { body, expected } => {
            check_anon_const_body(db, body, expected).1.clone()
        }
        BodyOwner::ContractInit { contract } => check_contract_init_body(db, contract).1.clone(),
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => check_contract_recv_arm_body(db, contract, recv_idx, arm_idx)
            .1
            .clone(),
    };

    TypedBodyTemplate {
        owner,
        body: typed_body,
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct GenericSubst<'db> {
    #[return_ref]
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
            BodyOwner::Func(func) if args.is_empty() => {
                return Self::for_owner(
                    db,
                    func.into(),
                    CallableDef::Func(func).params(db).to_vec(),
                );
            }
            BodyOwner::Func(func) => Some(func.into()),
            BodyOwner::AnonConstBody { body, .. } if !args.is_empty() => {
                lexical_const_body_owner(db, body)
            }
            _ => None,
        };
        match generic_owner {
            Some(generic_owner) => Self::for_owner(db, generic_owner, args),
            None => {
                assert!(
                    args.is_empty(),
                    "{owner:?} has no generic context for {args:?}"
                );
                Self::none(db)
            }
        }
    }

    pub fn generic_args(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.mapping(db).as_ref().map_or(&[], CompleteSubst::values)
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
    if let BodyOwner::Func(func) = template.owner {
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
