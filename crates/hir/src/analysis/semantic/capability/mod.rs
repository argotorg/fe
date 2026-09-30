//! Canonical capability algebra over verified semantic values.
//!
//! The structural solver and boundary policies consume this module; runtime layout does not
//! participate in index identity, capability slots, or guarded value equality.
pub mod birth;
mod decision;
pub mod external;
pub mod footprint;
pub mod guard;
pub mod handle;
pub mod index;
pub mod loan;
pub mod opaque;
pub mod path;
pub mod region;
pub mod repack;
pub mod semantics;
pub mod shape;
pub mod source;
pub mod state;
pub mod value;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub(crate) use decision::INTERN_ATTEMPTS as DECISION_INTERN_ATTEMPTS;

#[cfg(test)]
pub(crate) mod test_roots {
    use super::{
        external::{ExternalSource, ReferentContract},
        handle::HandleAddressSpace,
        region::RegionRoot,
        source::InputSource,
    };
    use crate::analysis::{HirAnalysisDb, semantic::normalized::NRootId, ty::ty_def::TyId};
    pub fn local(db: &dyn HirAnalysisDb, root: NRootId) -> RegionRoot<'_> {
        RegionRoot::Root {
            root,
            contract: ReferentContract::memory(db, TyId::u256(db)),
        }
    }
    pub fn input<'db>(db: &'db dyn HirAnalysisDb, source: InputSource<'db>) -> ExternalSource<'db> {
        ExternalSource::input(
            source,
            ReferentContract::new(db, TyId::u256(db), HandleAddressSpace::Unspecified),
            false,
        )
    }
}
