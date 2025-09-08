use crate::{
    hir_def::{PathId, PathKind},
    HirDb,
};

/// Structural classification of a path segment.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SegmentKind {
    /// A regular identifier segment, possibly with generic arguments.
    Plain,
    /// A qualified type segment like `<T as Trait>`.
    QualifiedType,
}

/// Minimal, structural facts about a path segment.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SegmentInfo {
    pub kind: SegmentKind,
    pub has_ident: bool,
    pub has_generic_args: bool,
}

/// A unified, structural view over paths. Implementations may wrap HIR or AST
/// representations; consumers must not rely on concrete types.
pub trait PathView {
    /// Number of segments in the path.
    fn segments(&self) -> usize;
    /// Facts about the `idx`-th segment, if it exists.
    fn segment_info(&self, idx: usize) -> Option<SegmentInfo>;
}

/// HIR adapter implementing `PathView` for `PathId`.
pub struct HirPathAdapter<'db> {
    pub db: &'db dyn HirDb,
    pub path: PathId<'db>,
}

impl<'db> HirPathAdapter<'db> {
    pub fn new(db: &'db dyn HirDb, path: PathId<'db>) -> Self {
        Self { db, path }
    }
}

impl PathView for HirPathAdapter<'_> {
    fn segments(&self) -> usize {
        self.path.len(self.db)
    }

    fn segment_info(&self, idx: usize) -> Option<SegmentInfo> {
        let seg = self.path.segment(self.db, idx)?;
        let info = match seg.kind(self.db) {
            PathKind::Ident {
                ident,
                generic_args,
            } => SegmentInfo {
                kind: SegmentKind::Plain,
                has_ident: ident.is_present(),
                has_generic_args: !generic_args.is_empty(self.db),
            },
            PathKind::QualifiedType { .. } => SegmentInfo {
                kind: SegmentKind::QualifiedType,
                has_ident: false,
                has_generic_args: false,
            },
        };
        Some(info)
    }
}
