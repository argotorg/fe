//! The space of a sub-place: where a path from a place in a known space
//! takes it. Spec 19's `space(P.f) = pin(F)`: a step into a pinned type
//! moves the place into the pin's space, as does an entry of a collection,
//! which lies in its collection's pinned space; a lane, a packed field or entry of a state word, has memory
//! grants, the memory copies it is accessed through.
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::SemanticInstance,
        ty::{provider::AddressSpace, ty_def::TyId},
    },
    core::semantic::storage_field_lanes,
};

/// What one step of a place's path is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PlaceStep {
    /// A field or tuple component, by index.
    Field(usize),
    /// An array element.
    Index,
    /// An entry of a `StateIndex` collection.
    Entry,
    /// A field of an enum variant's payload.
    Variant,
}

/// Where a path takes a place, step by step.
#[derive(Clone, Copy, Debug)]
pub(crate) struct PathSpace {
    /// The space the innermost pinned type or entry along the path moved
    /// the place into.
    pub moved: Option<AddressSpace>,
    /// Whether the path crosses a lane.
    pub lane: bool,
}

impl PathSpace {
    /// At a root of type `root`.
    pub fn new<'db>(db: &'db dyn HirAnalysisDb, root: TyId<'db>) -> Self {
        Self {
            moved: root.pin(db),
            lane: false,
        }
    }

    /// One step, `(container, step, result)` from a place of type
    /// `container` to one of type `result`, on a path whose root lies in
    /// `start`.
    pub fn step<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        start: Option<AddressSpace>,
        (container, step, result): (TyId<'db>, PlaceStep, TyId<'db>),
    ) {
        match step {
            PlaceStep::Entry => {
                if let Some(space) = instance.pinned_space(db, container) {
                    self.moved = Some(space);
                }
                self.lane |= instance.state_index_lanes(db, container).is_some();
            }
            PlaceStep::Field(field)
                if matches!(
                    self.moved.or(start),
                    Some(AddressSpace::Storage | AddressSpace::Transient)
                ) =>
            {
                self.lane |= storage_field_lanes(db, container)
                    .get(field)
                    .copied()
                    .unwrap_or(false);
            }
            PlaceStep::Field(_) | PlaceStep::Index | PlaceStep::Variant => {}
        }
        if let Some(space) = result.pin(db) {
            self.moved = Some(space);
        }
    }
}
