use super::{GenericArgListId, IdentId, TraitRefId, TypeId};
use crate::{HirDb, hir_def::Partial};

#[salsa::interned]
#[derive(Debug)]
pub struct PathId<'db> {
    pub kind: PathKind<'db>,
    pub parent: Option<PathId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathKind<'db> {
    Ident {
        ident: Partial<IdentId<'db>>,
        generic_args: GenericArgListId<'db>,
    },
    QualifiedType {
        type_: TypeId<'db>,
        trait_: TraitRefId<'db>,
    },
}

impl<'db> PathId<'db> {
    pub fn from_str(db: &'db dyn HirDb, s: &str) -> Self {
        Self::from_ident(db, IdentId::new(db, s.to_string()))
    }

    pub fn from_ident(db: &'db dyn HirDb, ident: IdentId<'db>) -> Self {
        Self::new(
            db,
            PathKind::Ident {
                ident: Partial::Present(ident),
                generic_args: GenericArgListId::none(db),
            },
            None,
        )
    }

    pub fn from_segments(db: &'db dyn HirDb, segments: &[&str]) -> Self {
        debug_assert!(!segments.is_empty());
        let mut segs = segments.iter();
        let mut path = PathId::from_ident(db, IdentId::new(db, segs.next().unwrap().to_string()));
        for s in segs {
            path = path.push_ident(db, IdentId::new(db, s.to_string()));
        }
        path
    }

    pub fn self_ty(db: &'db dyn HirDb, args: GenericArgListId<'db>) -> Self {
        Self::new(
            db,
            PathKind::Ident {
                ident: Partial::Present(IdentId::make_self_ty(db)),
                generic_args: args,
            },
            None,
        )
    }

    pub fn len(self, db: &dyn HirDb) -> usize {
        if let Some(parent) = self.parent(db) {
            parent.len(db) + 1
        } else {
            1
        }
    }

    pub fn segment_index(self, db: &dyn HirDb) -> usize {
        self.len(db) - 1
    }

    pub fn segment(self, db: &'db dyn HirDb, idx: usize) -> Option<PathId<'db>> {
        if idx == self.segment_index(db) {
            Some(self)
        } else {
            self.parent(db).and_then(|p| p.segment(db, idx))
        }
    }

    pub fn root_ident(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        if let Some(parent) = self.parent(db) {
            parent.root_ident(db)
        } else {
            match self.kind(db) {
                PathKind::Ident { ident, .. } => ident.to_opt(),
                PathKind::QualifiedType { .. } => None,
            }
        }
    }

    pub fn as_ident(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        if self.parent(db).is_none() {
            match self.kind(db) {
                PathKind::Ident {
                    ident,
                    generic_args,
                } if generic_args.is_empty(db) => ident.to_opt(),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn is_bare_ident(self, db: &dyn HirDb) -> bool {
        self.as_ident(db).is_some()
    }

    pub fn is_self_ty(self, db: &dyn HirDb) -> bool {
        if self.parent(db).is_none() {
            match self.kind(db) {
                PathKind::Ident { ident, .. } if ident.is_present() => {
                    ident.unwrap().is_self_ty(db)
                }
                _ => false,
            }
        } else {
            false
        }
    }

    pub fn replace_root(
        self,
        from: IdentId<'db>,
        to: IdentId<'db>,
        db: &'db dyn HirDb,
    ) -> PathId<'db> {
        match self.parent(db) {
            Some(parent) => parent.replace_root(from, to, db).push(db, self.kind(db)),
            None => {
                let kind = match self.kind(db) {
                    PathKind::Ident {
                        ident,
                        generic_args,
                    } if ident == from => PathKind::Ident {
                        ident: Partial::Present(to),
                        generic_args,
                    },
                    kind => kind,
                };
                PathId::new(db, kind, None)
            }
        }
    }

    pub fn is_core_lib_path(self, db: &'db dyn HirDb) -> bool {
        match self.parent(db) {
            Some(parent) => parent.is_core_lib_path(db),
            None => self
                .as_ident(db)
                .map(|id| id.data(db) == "core")
                .unwrap_or_default(),
        }
    }

    pub fn push(self, db: &'db dyn HirDb, kind: PathKind<'db>) -> Self {
        Self::new(db, kind, Some(self))
    }

    pub fn push_str(self, db: &'db dyn HirDb, s: &str) -> Self {
        self.push_ident(db, IdentId::new(db, s.to_string()))
    }

    pub fn push_ident(self, db: &'db dyn HirDb, ident: IdentId<'db>) -> Self {
        self.push(
            db,
            PathKind::Ident {
                ident: Partial::Present(ident),
                generic_args: GenericArgListId::none(db),
            },
        )
    }

    pub fn push_str_args(
        self,
        db: &'db dyn HirDb,
        s: &str,
        generic_args: GenericArgListId<'db>,
    ) -> Self {
        self.push_ident_args(db, IdentId::new(db, s.to_string()), generic_args)
    }

    pub fn push_ident_args(
        self,
        db: &'db dyn HirDb,
        ident: IdentId<'db>,
        generic_args: GenericArgListId<'db>,
    ) -> Self {
        self.push(
            db,
            PathKind::Ident {
                ident: Partial::Present(ident),
                generic_args,
            },
        )
    }

    pub fn ident(self, db: &'db dyn HirDb) -> Partial<IdentId<'db>> {
        match self.kind(db) {
            PathKind::Ident { ident, .. } => ident,
            PathKind::QualifiedType { .. } => Partial::Absent,
        }
    }

    pub fn generic_args(self, db: &'db dyn HirDb) -> GenericArgListId<'db> {
        match self.kind(db) {
            PathKind::Ident { generic_args, .. } => generic_args,
            PathKind::QualifiedType { .. } => GenericArgListId::none(db),
        }
    }

    pub fn strip_generic_args(self, db: &'db dyn HirDb) -> PathId<'db> {
        let parent = self.parent(db).map(|p| p.strip_generic_args(db));
        let kind = match self.kind(db) {
            PathKind::Ident { ident, .. } => PathKind::Ident {
                ident,
                generic_args: GenericArgListId::none(db),
            },
            kind @ PathKind::QualifiedType { .. } => kind,
        };
        PathId::new(db, kind, parent)
    }

    pub fn pretty_print(self, db: &dyn HirDb) -> String {
        fn space_adjacent_angles(s: &str) -> String {
            let mut out = String::with_capacity(s.len());
            let mut prev: Option<char> = None;
            for ch in s.chars() {
                if matches!((prev, ch), (Some('<'), '<') | (Some('>'), '>')) {
                    out.push(' ');
                }
                out.push(ch);
                prev = Some(ch);
            }
            out
        }

        let this = match self.kind(db) {
            PathKind::Ident {
                ident,
                generic_args,
            } => {
                format!(
                    "{}{}",
                    ident.to_opt().map_or("_", |id| id.data(db)),
                    generic_args.pretty_print(db)
                )
            }
            PathKind::QualifiedType { type_, trait_ } => {
                format!(
                    "<{} as {}>",
                    type_.pretty_print(db),
                    trait_.pretty_print(db)
                )
            }
        };

        if let Some(parent) = self.parent(db) {
            space_adjacent_angles(&(parent.pretty_print(db) + "::" + &this))
        } else {
            space_adjacent_angles(&this)
        }
    }
}
