//! The default value of a trait constant, checked for each implementation that
//! inherits it.
//!
//! A default whose declared type mentions `Self`, an associated type of `Self`
//! or a parameter of the trait has no one type: `const NEG: Self::X = -3 as i8`
//! is right where `X` is `i8` and wrong where it is `u256`. It is checked where
//! it is written only for what does not depend on the implementation, and again
//! for each implementation that inherits it, exactly as if the implementation
//! had written the same text with its own types (`BodyOwner::InheritedConst`).

use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            const_ty::const_body_assumptions,
            diagnostics::{BodyDiag, FuncBodyDiag},
            normalize::normalize_ty,
            subst::substitute_complete,
            trait_lower::lower_impl_trait,
            trait_resolution::{PredicateListId, constraint::collect_constraints},
            ty_def::{AssocTy, TyId, TyParam},
            ty_lower::CompleteSubst,
            visitor::{TyVisitable, TyVisitor, walk_assoc_ty},
        },
    },
    core::semantic::TraitAssocConstView,
    hir_def::{Body, IdentId, ImplTrait, Trait, scope_graph::ScopeId},
};

use super::{
    BodyOwner, check_anon_const_body, check_body, diag_depends_on_param_instantiation,
    inherited_const_ctfe_diags,
};

/// Whether a trait constant's default is checked per inheriting
/// implementation: its declared type mentions a parameter of the trait.
pub(crate) fn default_checked_per_implementation<'db>(
    db: &'db dyn HirAnalysisDb,
    constant: TraitAssocConstView<'db>,
) -> bool {
    constant
        .ty(db)
        .is_some_and(|ty| ty.has_param(db) && !ty.has_invalid(db))
}

/// What the check of a trait's default for an implementation that inherits it
/// stands on.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct InheritedConstSetup<'db> {
    pub(crate) trait_: Trait<'db>,
    pub(crate) name: IdentId<'db>,
    /// The substitution of the trait's parameters, `Self` included, by the
    /// implementation's trait arguments, which are written in the
    /// implementation's own (identity) parameters.
    pub(crate) sigma: CompleteSubst<'db>,
    /// The type of the constant as the trait declares it.
    pub(crate) declared: TyId<'db>,
    /// The declared type for this implementation.
    pub(crate) expected: TyId<'db>,
    /// What the body is checked under, before its bounds are elaborated: the
    /// bounds of the implementation, as for a constant it writes itself. The
    /// trait's own bounds are not among them: the implementation stands for
    /// `Self`, and what it needs is proved by it.
    pub(crate) assumptions: PredicateListId<'db>,
    /// What the names of the body resolve under, as the trait wrote them,
    /// before their bounds are elaborated.
    pub(crate) written: PredicateListId<'db>,
}

/// The setup of the default `body` for `implementation`, if `body` is the
/// default of a constant of the trait that `implementation` implements.
pub(crate) fn inherited_const_setup<'db>(
    db: &'db dyn HirAnalysisDb,
    implementation: ImplTrait<'db>,
    body: Body<'db>,
) -> Option<InheritedConstSetup<'db>> {
    let implementor = lower_impl_trait(db, implementation)?;
    let trait_ = implementor.trait_def(db);
    let constant = trait_
        .assoc_consts(db)
        .find(|constant| constant.default_body(db) == Some(body))?;
    let name = constant.name(db)?;
    let declared = constant.ty(db)?;
    let sigma =
        CompleteSubst::for_owner(db, trait_.into(), implementor.trait_(db).args(db).to_vec())
            .ok()?;
    // The body is checked as the implementation's own constant is: under the
    // bounds of the implementation, for which an instance receives proofs. The
    // names in the body resolve where the trait wrote them, in terms of the
    // trait's `Self`, under the trait's bounds, and what they resolve to is
    // instantiated afterwards (`TyCheckEnv::instantiate_scope_ty`).
    let assumptions = collect_constraints(db, implementation.into()).instantiate_identity();
    let written = const_body_assumptions(db, body.scope());
    let expected = normalize_ty(
        db,
        substitute_complete(db, declared, &sigma).ok()?,
        body.scope(),
        assumptions.extend_all_bounds(db),
    );
    Some(InheritedConstSetup {
        trait_,
        name,
        sigma,
        declared,
        expected,
        assumptions,
        written,
    })
}

/// A default that `implementation` inherits and that is checked for it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct InheritedConstBody<'db> {
    pub(crate) name: IdentId<'db>,
    pub(crate) body: Body<'db>,
    pub(crate) trait_: Trait<'db>,
    /// The type the trait declares for the constant.
    pub(crate) declared: TyId<'db>,
}

impl<'db> InheritedConstBody<'db> {
    pub(crate) fn owner(self, implementation: ImplTrait<'db>) -> BodyOwner<'db> {
        BodyOwner::InheritedConst {
            implementation,
            body: self.body,
        }
    }
}

/// The defaults that `implementation` inherits and that are checked for it
/// (`default_checked_per_implementation`): those of the trait's constants that
/// the implementation gives no value.
pub(crate) fn inherited_const_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    implementation: ImplTrait<'db>,
) -> Vec<InheritedConstBody<'db>> {
    let Some(implementor) = lower_impl_trait(db, implementation) else {
        return Vec::new();
    };
    let trait_ = implementor.trait_def(db);
    // A trait that the compiler generates is not checked where it is written
    // (`check_trait_const_default_bodies`), nor for its implementations.
    if trait_.origin(db).generated().is_some() {
        return Vec::new();
    }
    trait_
        .assoc_consts(db)
        .filter_map(|constant| {
            let name = constant.name(db)?;
            let body = constant.default_body(db)?;
            if !default_checked_per_implementation(db, constant)
                || implementation
                    .assoc_consts(db)
                    .any(|own| own.name(db) == Some(name) && own.value_body(db).is_some())
            {
                return None;
            }
            Some(InheritedConstBody {
                name,
                body,
                trait_,
                declared: constant.ty(db)?,
            })
        })
        .collect()
}

/// Whether `ty` still mentions a parameter of the trait whose scope is
/// `trait_scope`, or its `Self`.
pub(super) fn mentions_param_of<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    trait_scope: ScopeId<'db>,
) -> bool {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        trait_scope: ScopeId<'db>,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_param(&mut self, ty_param: &TyParam<'db>) {
            self.found |= ty_param.owner == self.trait_scope;
        }

        fn visit_const_param(&mut self, ty_param: &TyParam<'db>, _: TyId<'db>) {
            self.found |= ty_param.owner == self.trait_scope;
        }
    }

    let mut finder = Finder {
        db,
        trait_scope,
        found: false,
    };
    ty.visit_with(&mut finder);
    finder.found
}

/// The diagnostics of the defaults that `implementation` inherits, each
/// reported at the implementation. A default that the trait's own check already
/// refuses is refused there and reports nothing more: the trait's check defers
/// only the diagnostics that depend on the instantiation
/// (`diag_depends_on_param_instantiation`). The body is checked as every body is, requirement
/// checks included (`check_body`).
pub(super) fn check_inherited_defaults<'db>(
    db: &'db dyn HirAnalysisDb,
    implementation: ImplTrait<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let mut diags = Vec::new();
    for inherited in inherited_const_bodies(db, implementation) {
        let at_trait = &check_anon_const_body(db, inherited.body, inherited.declared).0;
        if !at_trait
            .iter()
            .all(|diag| diag_depends_on_param_instantiation(db, diag))
        {
            continue;
        }
        let Some(setup) = inherited_const_setup(db, implementation, inherited.body) else {
            continue;
        };
        let assoc_types = assoc_types_of_self(db, &setup);
        let owner = inherited.owner(implementation);
        // A body that type-checks is evaluated, as the value of a constant that the
        // implementation writes is.
        let checked = &check_body(db, owner).0;
        let evaluated = if checked.is_empty() {
            inherited_const_ctfe_diags(db, owner, inherited.body, setup.expected)
        } else {
            Vec::new()
        };
        let found = checked
            .iter()
            .map(|diag| (diag, false))
            .chain(evaluated.iter().map(|diag| (diag, true)));
        for (diag, evaluation) in found {
            diags.push(
                BodyDiag::InheritedConstDefault {
                    primary: implementation.span().trait_ref().into(),
                    const_name: inherited.name,
                    assoc_types: assoc_types.clone(),
                    inner: Box::new(diag.clone()),
                    evaluation,
                }
                .into(),
            );
        }
    }
    diags
}

/// Each associated type of `Self` that the declared type of the constant
/// mentions, with its value in the implementation.
fn assoc_types_of_self<'db>(
    db: &'db dyn HirAnalysisDb,
    setup: &InheritedConstSetup<'db>,
) -> Vec<(IdentId<'db>, TyId<'db>)> {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        trait_: Trait<'db>,
        found: Vec<AssocTy<'db>>,
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_assoc_ty(&mut self, assoc_ty: &AssocTy<'db>) {
            if assoc_ty.trait_.def(self.db) == self.trait_ && !self.found.contains(assoc_ty) {
                self.found.push(*assoc_ty);
            }
            walk_assoc_ty(self, assoc_ty);
        }
    }

    let mut finder = Finder {
        db,
        trait_: setup.trait_,
        found: Vec::new(),
    };
    setup.declared.visit_with(&mut finder);
    let assumptions = setup.assumptions.extend_all_bounds(db);
    finder
        .found
        .into_iter()
        .filter_map(|assoc| {
            let ty = substitute_complete(
                db,
                TyId::assoc_ty(db, assoc.trait_, assoc.name),
                &setup.sigma,
            )
            .ok()?;
            let scope = setup.trait_.scope();
            Some((assoc.name, normalize_ty(db, ty, scope, assumptions)))
        })
        .collect()
}
