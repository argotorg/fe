use super::{GenericArgListId, IdentId, TraitRefId, TypeId};
use crate::{HirDb, hir_def::Partial};
use common::{ingot::Ingot, stdlib::BUILTIN_CORE_BASE_URL};

#[salsa::interned]
#[derive(Debug)]
pub struct PathId<'db> {
    #[returns(copy)]
    pub kind: PathKind<'db>,
    #[returns(copy)]
    pub parent: Option<PathId<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum PathKind<'db> {
    Ident {
        ident: Partial<IdentId<'db>>,
        generic_args: GenericArgListId<'db>,
    },
    BoundIngot {
        ingot: Ingot<'db>,
        display: IdentId<'db>,
    },
    BoundItem {
        item: super::scope_graph::ScopeId<'db>,
        display: PathId<'db>,
    },
    InScope {
        scope: super::scope_graph::ScopeId<'db>,
        local_ident: Option<IdentId<'db>>,
        display: PathId<'db>,
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
                PathKind::BoundIngot { display, .. } => Some(display),
                PathKind::BoundItem { display, .. } => display.root_ident(db),
                PathKind::InScope { display, .. } => display.root_ident(db),
                PathKind::QualifiedType { .. } => None,
            }
        }
    }

    pub fn hygiene_scope(self, db: &'db dyn HirDb) -> Option<super::scope_graph::ScopeId<'db>> {
        if let Some(parent) = self.parent(db) {
            parent.hygiene_scope(db)
        } else {
            match self.kind(db) {
                PathKind::InScope { scope, .. } => Some(scope),
                _ => None,
            }
        }
    }

    pub fn local_ident(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        if self.parent(db).is_none() {
            match self.kind(db) {
                PathKind::Ident {
                    ident,
                    generic_args,
                } if generic_args.is_empty(db) => ident.to_opt(),
                PathKind::InScope {
                    local_ident,
                    display,
                    ..
                } if display.generic_args(db).is_empty(db) => {
                    local_ident.or_else(|| display.ident(db).to_opt())
                }
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn as_ident(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        if self.parent(db).is_none() {
            match self.kind(db) {
                PathKind::Ident {
                    ident,
                    generic_args,
                } if generic_args.is_empty(db) => ident.to_opt(),
                PathKind::InScope { display, .. } => display.as_ident(db),
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
                PathKind::InScope { display, .. } => display.is_self_ty(db),
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
        if let Some(parent) = self.parent(db) {
            parent.replace_root(from, to, db).push(db, self.kind(db))
        } else {
            let kind = match self.kind(db) {
                PathKind::Ident {
                    ident,
                    generic_args,
                } if ident == from => PathKind::Ident {
                    ident: Partial::Present(to),
                    generic_args,
                },
                PathKind::InScope {
                    scope,
                    local_ident,
                    display,
                } => PathKind::InScope {
                    scope,
                    local_ident,
                    display: display.replace_root(from, to, db),
                },
                kind => kind,
            };
            PathId::new(db, kind, None)
        }
    }

    pub fn is_core_lib_path(self, db: &'db dyn HirDb) -> bool {
        match self.parent(db) {
            Some(parent) => parent.is_core_lib_path(db),
            None => match self.kind(db) {
                PathKind::BoundIngot { ingot, .. } => {
                    ingot.base(db).as_str() == BUILTIN_CORE_BASE_URL
                }
                PathKind::BoundItem { display, .. } => display.is_core_lib_path(db),
                PathKind::InScope { display, .. } => display.is_core_lib_path(db),
                _ => self.as_ident(db).is_some_and(|id| id.data(db) == "core"),
            },
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
            PathKind::BoundIngot { display, .. } => Partial::Present(display),
            PathKind::BoundItem { display, .. } => display.ident(db),
            PathKind::InScope { display, .. } => display.ident(db),
            PathKind::QualifiedType { .. } => Partial::Absent,
        }
    }

    pub fn generic_args(self, db: &'db dyn HirDb) -> GenericArgListId<'db> {
        match self.kind(db) {
            PathKind::Ident { generic_args, .. } => generic_args,
            PathKind::BoundIngot { .. } => GenericArgListId::none(db),
            PathKind::BoundItem { display, .. } => display.generic_args(db),
            PathKind::InScope { display, .. } => display.generic_args(db),
            PathKind::QualifiedType { .. } => GenericArgListId::none(db),
        }
    }

    pub fn with_generic_args(
        self,
        db: &'db dyn HirDb,
        generic_args: GenericArgListId<'db>,
    ) -> Option<Self> {
        let kind = match self.kind(db) {
            PathKind::Ident { ident, .. } => PathKind::Ident {
                ident,
                generic_args,
            },
            PathKind::BoundItem { item, display } => PathKind::BoundItem {
                item,
                display: display.with_generic_args(db, generic_args)?,
            },
            PathKind::InScope {
                scope,
                local_ident,
                display,
            } => PathKind::InScope {
                scope,
                local_ident,
                display: display.with_generic_args(db, generic_args)?,
            },
            _ => return None,
        };
        Some(Self::new(db, kind, self.parent(db)))
    }

    pub fn strip_generic_args(self, db: &'db dyn HirDb) -> PathId<'db> {
        let parent = self.parent(db).map(|p| p.strip_generic_args(db));
        let kind = match self.kind(db) {
            PathKind::Ident { ident, .. } => PathKind::Ident {
                ident,
                generic_args: GenericArgListId::none(db),
            },
            kind @ PathKind::BoundIngot { .. } => kind,
            PathKind::BoundItem { item, display } => PathKind::BoundItem {
                item,
                display: display.strip_generic_args(db),
            },
            PathKind::InScope {
                scope,
                local_ident,
                display,
            } => PathKind::InScope {
                scope,
                local_ident,
                display: display.strip_generic_args(db),
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
            PathKind::BoundIngot { display, .. } => display.data(db).clone(),
            PathKind::BoundItem { display, .. } => display.pretty_print(db),
            PathKind::InScope { display, .. } => display.pretty_print(db),
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
