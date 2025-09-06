use crate::span::DynLazySpan;
use crate::{
    path_view::{PathView, SegmentKind},
    span::lazy_spans::LazyPathSpan,
};

/// The kind of sub-span to select within a path segment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathAnchorKind {
    Ident,
    GenericArgs,
    Segment,
    /// The trait name in a qualified type segment, e.g., `<T as Trait>` → `Trait`.
    TraitName,
}

/// A structural anchor describing which segment and which part to highlight.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathAnchor {
    pub seg_idx: usize,
    pub kind: PathAnchorKind,
}

/// Heuristics for selecting anchors in typical error cases. These are pure and
/// depend only on `PathView` structure.
pub struct AnchorPicker;

impl AnchorPicker {
    /// Unresolved tail of a path. Prefer the last segment's ident; if the last
    /// segment is qualified type, pick the trait name; otherwise segment.
    pub fn pick_unresolved_tail<V: PathView>(view: &V) -> PathAnchor {
        let n = view.segments();
        let idx = n.saturating_sub(1);
        Self::pick_preferred(view, idx)
    }

    /// Invalid segment at `seg_idx`.
    pub fn pick_invalid_segment<V: PathView>(view: &V, seg_idx: usize) -> PathAnchor {
        Self::pick_preferred(view, seg_idx)
    }

    /// Visibility error at `seg_idx`: prefer ident if present.
    pub fn pick_visibility_error<V: PathView>(view: &V, seg_idx: usize) -> PathAnchor {
        if let Some(info) = view.segment_info(seg_idx) {
            if info.has_ident {
                return PathAnchor {
                    seg_idx,
                    kind: PathAnchorKind::Ident,
                };
            }
        }
        PathAnchor {
            seg_idx,
            kind: PathAnchorKind::Segment,
        }
    }

    fn pick_preferred<V: PathView>(view: &V, seg_idx: usize) -> PathAnchor {
        match view.segment_info(seg_idx) {
            Some(info) => match info.kind {
                SegmentKind::QualifiedType => PathAnchor {
                    seg_idx,
                    kind: PathAnchorKind::TraitName,
                },
                SegmentKind::Plain => {
                    if info.has_ident {
                        PathAnchor {
                            seg_idx,
                            kind: PathAnchorKind::Ident,
                        }
                    } else if info.has_generic_args {
                        PathAnchor {
                            seg_idx,
                            kind: PathAnchorKind::GenericArgs,
                        }
                    } else {
                        PathAnchor {
                            seg_idx,
                            kind: PathAnchorKind::Segment,
                        }
                    }
                }
            },
            None => PathAnchor {
                seg_idx,
                kind: PathAnchorKind::Segment,
            },
        }
    }
}

/// Map to a DynLazySpan without resolving to a concrete Span.
pub fn map_path_anchor_to_dyn_lazy<'db>(
    lazy_path: LazyPathSpan<'db>,
    anchor: PathAnchor,
) -> DynLazySpan<'db> {
    let seg = lazy_path.segment(anchor.seg_idx);
    match anchor.kind {
        PathAnchorKind::Ident => seg.ident().into(),
        PathAnchorKind::GenericArgs => seg.generic_args().into(),
        PathAnchorKind::Segment => seg.into_atom().into(),
        PathAnchorKind::TraitName => seg.qualified_type().trait_qualifier().name().into(),
    }
}
