//! Abstract places: the storage a normalized place denotes after resolving
//! carriers to the accesses they hold.
use smallvec::SmallVec;

use crate::analysis::{
    semantic::normalized::{NDataPath, NDataProjection, NIndex, NRootId, NValueId},
    ty::provider::AddressSpace,
};

/// Where an abstract place starts.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Base {
    /// Body storage: local slots, owned parameter slots, temporaries and
    /// capability representations.
    Root(NRootId),
    /// The place data parameter `i` names. Distinct data parameters are
    /// disjoint unless both are views, because the caller checked its
    /// arguments.
    Param(u32),
    /// A resource domain, by index into the analysis's domain table.
    Domain(u32),
    /// Component `component` of the grant of the projection session whose
    /// call defines `session`.
    Grant { session: NValueId, component: u16 },
    /// Every slot of an address space, as an external execution or a raw
    /// storage operation reaches it.
    State(AddressSpace),
    /// Memory reached through a raw pointer. Accesses to it are not checked.
    Raw,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Step {
    Field(u16),
    Variant {
        variant: u16,
        field: u16,
    },
    /// A constant index, or `None` for a dynamic one.
    Index(Option<usize>),
}

impl Step {
    fn overlaps(self, other: Self) -> bool {
        match (self, other) {
            (Self::Field(lhs), Self::Field(rhs)) => lhs == rhs,
            (
                Self::Variant {
                    variant: lhs_variant,
                    field: lhs_field,
                },
                Self::Variant {
                    variant: rhs_variant,
                    field: rhs_field,
                },
            ) => lhs_variant != rhs_variant || lhs_field == rhs_field,
            (Self::Index(Some(lhs)), Self::Index(Some(rhs))) => lhs == rhs,
            _ => true,
        }
    }
}

pub(super) type Path = SmallVec<Step, 4>;

/// The steps of a normalized path; `constant` resolves index values that
/// are literal constants.
pub(super) fn path_of(path: &NDataPath, constant: impl Fn(NValueId) -> Option<usize>) -> Path {
    path.iter()
        .map(|projection| match *projection {
            NDataProjection::Field(field) => Step::Field(field.0),
            NDataProjection::VariantField { variant, field } => Step::Variant {
                variant: variant.0,
                field: field.0,
            },
            NDataProjection::Index(NIndex::Const(index)) => Step::Index(Some(index)),
            // An entry is an element at a key, `[*]` unless the key is a
            // literal.
            NDataProjection::Index(NIndex::Value(value)) | NDataProjection::Entry(value) => {
                Step::Index(constant(value))
            }
        })
        .collect()
}

/// Two paths from the same base overlap when neither diverges from the
/// other.
pub(super) fn paths_overlap(lhs: &[Step], rhs: &[Step]) -> bool {
    lhs.iter().zip(rhs).all(|(lhs, rhs)| lhs.overlaps(*rhs))
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct AbsPlace {
    pub base: Base,
    pub path: Path,
    /// The space a pinned type or an entry along the path moved the place
    /// into, as a `TSlot` and its value lie in transient storage whatever
    /// holds the `TSlot`. `None` keeps the base's.
    pub space: Option<AddressSpace>,
    /// Whether the path crosses a lane, a packed field or entry of a state
    /// word: a retained grant to the place is a memory copy, while accesses
    /// still reserve the lane in its own space.
    pub lane: bool,
}

impl AbsPlace {
    pub fn new(base: Base) -> Self {
        Self {
            base,
            path: Path::new(),
            space: None,
            lane: false,
        }
    }

    /// The place `suffix` reaches from this one, in `space` when a pinned
    /// type or an entry along the suffix moves it there, and in a lane when
    /// the suffix crosses one.
    pub fn extended(&self, suffix: &[Step], space: Option<AddressSpace>, lane: bool) -> Self {
        let mut path = self.path.clone();
        path.extend_from_slice(suffix);
        Self {
            base: self.base,
            path,
            space: space.or(self.space),
            lane: self.lane || lane,
        }
    }
}
