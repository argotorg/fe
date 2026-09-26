//! Simultaneous generic substitution in a declared parameter domain.

use super::{
    const_ty::ConstCaptureEnv,
    fold::{TyFoldable, TyFolder},
    ty_def::TyId,
    ty_lower::{CompleteSubst, ParamSchemaId, SubstError},
};
use crate::{
    analysis::HirAnalysisDb,
    hir_def::{GenericParamOwner, scope_graph::ScopeId},
};

/// Substitutes original declaration occurrences exactly once. Replacement
/// values already belong to the destination context and are never traversed
/// by this application.
pub(crate) fn substitute_complete<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    subst: &CompleteSubst<'db>,
) -> Result<T, SubstError<'db>>
where
    T: TyFoldable<'db>,
{
    let mut folder = SubstFolder { subst, error: None };
    let value = value.fold_with(db, &mut folder);
    folder.error.map_or(Ok(value), Err)
}

/// Whether `ty` is a non-effect parameter owned by `schema`'s declaration.
/// Such an occurrence that does not resolve through the schema belongs to
/// another basis of the same declaration and must not pass through unchanged.
pub(crate) fn is_owned_by_schema<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    schema: ParamSchemaId<'db>,
) -> bool {
    ty.as_generic_param(db)
        .is_some_and(|param| param.owner == schema.owner(db).scope() && !param.is_effect())
}

struct SubstFolder<'a, 'db> {
    subst: &'a CompleteSubst<'db>,
    error: Option<SubstError<'db>>,
}

impl<'db> TyFolder<'db> for SubstFolder<'_, 'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        let schema = self.subst.domain().schema(db);
        if let Some(key) = schema.original_key(db, ty) {
            if let Some(replacement) = self.subst.get(db, key) {
                return replacement;
            }
            self.error.get_or_insert(SubstError::MissingArgument {
                domain: self.subst.domain(),
                key,
            });
            return ty;
        }

        if is_owned_by_schema(db, ty, schema) {
            self.error.get_or_insert(SubstError::WrongBasis {
                schema,
                occurrence: ty,
            });
            return ty;
        }

        ty.super_fold_with(db, self)
    }

    fn fold_const_capture(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        capture: &ConstCaptureEnv<'db>,
    ) -> ConstCaptureEnv<'db> {
        if let Some(bound) = capture.bind_identity_with(db, self.subst) {
            return match bound {
                Ok(bound) => bound,
                Err(error) => {
                    self.error.get_or_insert(error);
                    capture.clone()
                }
            };
        }
        capture.fold_ranges(db, self)
    }
}

/// Instantiates the non-effect parameters owned by `owner` with `args`, one
/// per full-schema slot, and transfers retained scope context from `owner` to
/// `new_owner`. Occurrences owned by other declarations keep their original
/// ownership, so a value mixing several parameter contexts can be specialized
/// one context at a time.
pub(crate) fn instantiate_scoped_into<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    owner: ScopeId<'db>,
    new_owner: ScopeId<'db>,
    args: &[TyId<'db>],
) -> T
where
    T: TyFoldable<'db>,
{
    let subst = GenericParamOwner::from_item_opt(owner.item())
        .filter(|param_owner| param_owner.scope() == owner)
        .and_then(|param_owner| CompleteSubst::for_owner(db, param_owner, args.to_vec()).ok());
    let mut folder = ScopedSubstFolder {
        owner,
        new_owner,
        args,
        subst,
    };
    value.fold_with(db, &mut folder)
}

struct ScopedSubstFolder<'a, 'db> {
    owner: ScopeId<'db>,
    new_owner: ScopeId<'db>,
    args: &'a [TyId<'db>],
    subst: Option<CompleteSubst<'db>>,
}

impl<'db> TyFolder<'db> for ScopedSubstFolder<'_, 'db> {
    fn fold_scope(&mut self, scope: ScopeId<'db>) -> ScopeId<'db> {
        if scope == self.owner {
            self.new_owner
        } else {
            scope
        }
    }

    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        if let Some(param) = ty.as_generic_param(db)
            && param.owner == self.owner
            && !param.is_effect()
            && let Some(arg) = self.args.get(param.idx)
        {
            return *arg;
        }
        ty.super_fold_with(db, self)
    }

    fn fold_const_capture(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        capture: &ConstCaptureEnv<'db>,
    ) -> ConstCaptureEnv<'db> {
        if let Some(subst) = &self.subst
            && let Some(Ok(bound)) = capture.bind_identity_with(db, subst)
        {
            return bound;
        }
        capture.fold_ranges(db, self)
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use common::indexmap::IndexMap;

    use super::*;
    use crate::{
        analysis::ty::{
            const_ty::ConstTyData,
            generic_defaults::{GenericDefault, generic_default},
            trait_def::TraitInstId,
            ty_def::TyData,
            ty_lower::{ParamBasis, ParamDomainId, PartialSubst, SourceParamIndex, param_schema},
        },
        hir_def::{CallableDef, IdentId},
        test_db::{HirAnalysisTestDb, find_func},
    };

    #[test]
    fn substitution_is_simultaneous_and_isolates_foreign_owners() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("subst_laws.fe"),
            "trait Out<T> { type Item }\nfn f<T, U, const N: u256, const M: u256>() {}\nfn g<X>() {}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = find_func(&db, module, "f");
        let params = CallableDef::Func(func).params(&db);
        let foreign = CallableDef::Func(find_func(&db, module, "g")).params(&db)[0];
        let schema = param_schema(&db, func.into(), ParamBasis::Full);
        let domain = ParamDomainId::full(&db, schema);
        let original = vec![params[0], params[1], params[2], params[3], foreign];
        let identity = CompleteSubst::new(domain, &db, params.to_vec()).unwrap();
        assert_eq!(
            substitute_complete(&db, original.clone(), &identity).unwrap(),
            original
        );

        let swapped = vec![params[1], params[0], params[3], params[2]];
        let sigma = CompleteSubst::new(domain, &db, swapped.clone()).unwrap();
        assert_eq!(
            substitute_complete(&db, original.clone(), &sigma).unwrap(),
            [swapped.as_slice(), &[foreign]].concat()
        );
        let composed = sigma
            .values()
            .iter()
            .copied()
            .map(|value| substitute_complete(&db, value, &sigma).unwrap())
            .collect();
        let composed = CompleteSubst::new(domain, &db, composed).unwrap();
        assert_eq!(
            substitute_complete(&db, original.clone(), &composed).unwrap(),
            substitute_complete(
                &db,
                substitute_complete(&db, original.clone(), &sigma).unwrap(),
                &sigma
            )
            .unwrap()
        );

        let trait_ = module.all_traits(&db)[0];
        let item = IdentId::new(&db, "Item".to_string());
        let mut bindings = IndexMap::new();
        bindings.insert(item, params[0]);
        let predicate = TraitInstId::new(&db, trait_, vec![params[0], params[1]], bindings);
        let mapped = substitute_complete(&db, predicate, &sigma).unwrap();
        assert_eq!(mapped.args(&db), &[params[1], params[0]]);
        assert_eq!(mapped.assoc_type_bindings(&db)[&item], params[1]);
    }

    #[test]
    fn partial_substitution_reports_missing_dependencies() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("partial_subst_laws.fe"),
            "fn f<T, U = T>() {}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = find_func(&db, module, "f");
        let params = CallableDef::Func(func).params(&db);
        let schema = param_schema(&db, func.into(), ParamBasis::Full);
        let t_key = schema.source_key(&db, SourceParamIndex(0)).unwrap();
        let u_key = schema.source_key(&db, SourceParamIndex(1)).unwrap();
        let domain = schema
            .allowed_default_dependencies(&db, SourceParamIndex(1))
            .unwrap();
        let prefix = CompleteSubst::new(domain, &db, vec![TyId::bool(&db)]).unwrap();
        assert_eq!(
            substitute_complete(&db, params[0], &prefix).unwrap(),
            TyId::bool(&db)
        );
        assert!(matches!(
            substitute_complete(&db, params[1], &prefix),
            Err(SubstError::MissingArgument { key, .. }) if key == u_key
        ));

        let mut partial = PartialSubst::new(&db, ParamDomainId::full(&db, schema));
        partial.bind(&db, t_key, TyId::bool(&db)).unwrap();
        assert_eq!(
            substitute_complete(&db, params.to_vec(), &partial.residualize(&db)).unwrap(),
            vec![TyId::bool(&db), params[1]]
        );
    }

    #[test]
    fn full_substitution_rejects_source_basis_occurrences() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("subst_basis.fe"),
            "struct Slot<const ROOT: u256 = _> {}\ntrait T<X> { fn f<Y>(_ value: Slot, _ extra: Y) {} }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = module
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "f")
            })
            .expect("missing trait method");
        let source = param_schema(&db, func.into(), ParamBasis::Source);
        let full = param_schema(&db, func.into(), ParamBasis::Full);
        let own_key = source.source_key(&db, SourceParamIndex(0)).unwrap();
        let source_formal = source
            .formal_at(&db, source.slot_for(&db, own_key).unwrap())
            .unwrap();
        let domain = ParamDomainId::full(&db, full);
        let values = domain
            .slots(&db)
            .map(|slot| full.formal_at(&db, slot).unwrap())
            .collect();
        let identity = CompleteSubst::new(domain, &db, values).unwrap();
        assert!(matches!(
            substitute_complete(&db, source_formal, &identity),
            Err(SubstError::WrongBasis { .. })
        ));
    }

    #[test]
    fn deferred_const_capture_composes_in_its_declared_domain() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("capture_composition.fe"),
            "fn f<const N: usize, const M: usize, T = [u8; { N + M }]>() {}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = find_func(&db, module, "f");
        let params = CallableDef::Func(func).params(&db);
        let GenericDefault::Type(default) = generic_default(&db, func.into(), 2)
            .as_ref()
            .unwrap()
            .as_ref()
            .unwrap()
        else {
            panic!("expected type default")
        };
        let template = default.instantiate_identity();
        let schema = param_schema(&db, func.into(), ParamBasis::Full);
        let domain = schema
            .allowed_default_dependencies(&db, SourceParamIndex(2))
            .unwrap();
        let swapped = CompleteSubst::new(domain, &db, vec![params[1], params[0]]).unwrap();
        let once = substitute_complete(&db, template, &swapped).unwrap();
        let twice = substitute_complete(&db, once, &swapped).unwrap();
        let identity = CompleteSubst::new(domain, &db, params[..2].to_vec()).unwrap();
        let composed = substitute_complete(&db, template, &identity).unwrap();
        assert_eq!(twice, composed);

        let TyData::ConstTy(length) = once.generic_args(&db)[1].data(&db) else {
            panic!("expected deferred array length")
        };
        let ConstTyData::UnEvaluated { capture, .. } = length.data(&db) else {
            panic!("expected deferred array length capture")
        };
        let mapping = capture.complete(&db).unwrap();
        assert_eq!(mapping.domain(), domain);
        assert_eq!(mapping.values(), &[params[1], params[0]]);
    }
}
