use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            const_ty::lexical_const_body_owner,
            subst::substitute_complete,
            trait_def::{ImplApplicationRecord, TraitInstId},
            trait_resolution::PredicateListId,
            ty_check::{BodyOwner, ReturnProjectionStep, TypedBody, infer_body},
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

#[salsa::interned(constructor = from_parts)]
#[derive(Debug)]
pub struct ImplEnv<'db> {
    pub normalization_scope: ScopeId<'db>,
    pub assumptions: PredicateListId<'db>,
    #[return_ref]
    pub witnesses: Vec<TraitInstId<'db>>,
    /// Issued argument choices, keyed by the callee parameter index.
    #[return_ref]
    pub parameter_evidence: Vec<(usize, ImplApplicationRecord<'db>)>,
    /// Issued records nested within an aggregate parameter, keyed by source slot.
    #[return_ref]
    pub projected_parameter_evidence:
        Vec<(usize, Vec<ReturnProjectionStep>, ImplApplicationRecord<'db>)>,
    /// In-force checked applications inherited by this instance.
    #[return_ref]
    pub lexical_applications: Vec<ImplApplicationRecord<'db>>,
}

impl<'db> ImplEnv<'db> {
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        witnesses: Vec<TraitInstId<'db>>,
        parameters: Vec<(usize, ImplApplicationRecord<'db>)>,
        lexical: Vec<ImplApplicationRecord<'db>>,
    ) -> Self {
        Self::from_parts(
            db,
            scope,
            assumptions,
            witnesses,
            parameters,
            Vec::new(),
            lexical,
        )
    }

    pub fn with_projected_parameter_evidence(
        self,
        db: &'db dyn HirAnalysisDb,
        records: Vec<(usize, Vec<ReturnProjectionStep>, ImplApplicationRecord<'db>)>,
    ) -> Self {
        Self::from_parts(
            db,
            self.normalization_scope(db),
            self.assumptions(db),
            self.witnesses(db).clone(),
            self.parameter_evidence(db).clone(),
            records,
            self.lexical_applications(db).clone(),
        )
    }

    /// Replace only the supplied parameter slots with records from actual issued values.
    pub(crate) fn with_argument_evidence_values(
        self,
        db: &'db dyn HirAnalysisDb,
        values: &[(usize, crate::analysis::semantic::SemConstId<'db>)],
    ) -> Self {
        use crate::analysis::semantic::{SemConstId, SemConstValue};
        fn collect<'db>(
            db: &'db dyn HirAnalysisDb,
            index: usize,
            value: SemConstId<'db>,
            path: Vec<ReturnProjectionStep>,
            flat: &mut Vec<(usize, ImplApplicationRecord<'db>)>,
            projected: &mut Vec<(usize, Vec<ReturnProjectionStep>, ImplApplicationRecord<'db>)>,
        ) {
            match value.value(db) {
                SemConstValue::Evidence { application, .. } => {
                    if path.is_empty() {
                        flat.push((index, *application));
                    } else {
                        projected.push((index, path, *application));
                    }
                }
                SemConstValue::Tuple { elems, .. } => {
                    for (field, &value) in elems.iter().enumerate() {
                        let mut path = path.clone();
                        path.push(ReturnProjectionStep::Field(
                            u16::try_from(field).expect("tuple index"),
                        ));
                        collect(db, index, value, path, flat, projected);
                    }
                }
                SemConstValue::Array { elems, .. } => {
                    for (field, &value) in elems.iter().enumerate() {
                        let mut path = path.clone();
                        path.push(ReturnProjectionStep::ConstantIndex(field));
                        collect(db, index, value, path, flat, projected);
                    }
                }
                SemConstValue::Struct { fields, .. } => {
                    for (field, &value) in fields.iter().enumerate() {
                        let mut path = path.clone();
                        path.push(ReturnProjectionStep::Field(
                            u16::try_from(field).expect("field index"),
                        ));
                        collect(db, index, value, path, flat, projected);
                    }
                }
                SemConstValue::Enum {
                    variant, fields, ..
                } => {
                    for (field, &value) in fields.iter().enumerate() {
                        let mut path = path.clone();
                        path.push(ReturnProjectionStep::VariantField {
                            variant: variant.0,
                            field: u16::try_from(field).expect("variant field index"),
                        });
                        collect(db, index, value, path, flat, projected);
                    }
                }
                _ => {}
            }
        }
        let supplied = |index: &usize| values.iter().any(|(supplied, _)| supplied == index);
        let mut flat = self
            .parameter_evidence(db)
            .iter()
            .filter(|(index, _)| !supplied(index))
            .copied()
            .collect();
        let mut projected = self
            .projected_parameter_evidence(db)
            .iter()
            .filter(|(index, _, _)| !supplied(index))
            .cloned()
            .collect();
        for &(index, value) in values {
            collect(db, index, value, Vec::new(), &mut flat, &mut projected);
        }
        Self::from_parts(
            db,
            self.normalization_scope(db),
            self.assumptions(db),
            self.witnesses(db).clone(),
            flat,
            projected,
            self.lexical_applications(db).clone(),
        )
    }

    pub fn empty(db: &'db dyn HirAnalysisDb, normalization_scope: ScopeId<'db>) -> Self {
        Self::new(
            db,
            normalization_scope,
            PredicateListId::empty_list(db),
            Vec::new(),
            Vec::new(),
            Vec::new(),
        )
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct EffectProviderSubst<'db> {
    #[return_ref]
    pub providers: Vec<ProviderBinding<'db>>,
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
