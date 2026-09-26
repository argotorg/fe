use super::{
    fold::TyFoldable,
    subst::substitute_complete,
    ty_def::TyId,
    ty_lower::{CompleteSubst, ParamSchemaId, SubstError},
};
use crate::{analysis::HirAnalysisDb, hir_def::GenericParamOwner};

/// A declaration template whose parameters are interpreted by the full
/// parameter schema of `owner`. Instantiation always uses that schema, so a
/// substitution built for another declaration is rejected rather than leaving
/// the template's parameters untouched. A closed template (no owner) binds no
/// parameters, e.g. contract field types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binder<'db, T> {
    owner: Option<GenericParamOwner<'db>>,
    value: T,
}
unsafe impl<T> salsa::Update for Binder<'_, T>
where
    T: salsa::Update,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        unsafe {
            let old_value = &mut *old_pointer;
            let owner_changed = old_value.owner != new_value.owner;
            old_value.owner = new_value.owner;
            T::maybe_update(&mut old_value.value, new_value.value) | owner_changed
        }
    }
}

impl<'db, T> Binder<'db, T> {
    pub const fn bind(owner: GenericParamOwner<'db>, value: T) -> Self {
        Binder {
            owner: Some(owner),
            value,
        }
    }

    pub const fn closed(value: T) -> Self {
        Binder { owner: None, value }
    }

    /// Returns the template in declaration coordinates. This is not an
    /// instantiation; use it only where the declaration's own parameters are
    /// the intended interpretation.
    pub fn instantiate_identity(self) -> T {
        self.value
    }

    /// Borrows the template in declaration coordinates, for data known not to
    /// depend on the bound parameters.
    pub fn skip_binder(&self) -> &T {
        &self.value
    }
}

impl<'db, T> Binder<'db, T>
where
    T: TyFoldable<'db>,
{
    /// Instantiates the template with `args`, one per full-schema slot.
    pub fn instantiate(self, db: &'db dyn HirAnalysisDb, args: &[TyId<'db>]) -> T {
        let Some(owner) = self.owner else {
            assert!(args.is_empty(), "closed binder instantiated with arguments");
            return self.value;
        };
        let subst = CompleteSubst::for_owner(db, owner, args.to_vec())
            .unwrap_or_else(|error| panic!("invalid binder arguments for {owner:?}: {error:?}"));
        self.instantiate_subst(db, &subst)
            .unwrap_or_else(|error| panic!("failed to instantiate binder of {owner:?}: {error:?}"))
    }

    /// Instantiates the template with a substitution over this declaration's
    /// schema or a restricted domain of it.
    pub fn instantiate_subst(
        self,
        db: &'db dyn HirAnalysisDb,
        subst: &CompleteSubst<'db>,
    ) -> Result<T, SubstError<'db>> {
        if self.owner.map(|owner| ParamSchemaId::full(db, owner)) != Some(subst.domain().schema(db))
        {
            return Err(SubstError::InvalidDomain(subst.domain()));
        }
        substitute_complete(db, self.value, subst)
    }
}
