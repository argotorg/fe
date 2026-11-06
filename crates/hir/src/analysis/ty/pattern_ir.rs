use cranelift_entity::{EntityRef, entity_impl};
use rustc_hash::FxHashMap;
use salsa::SalsaValue;
use smallvec1::SmallVec;

use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::adt_def::{AdtRef, instantiate_adt_field_shape};
use crate::analysis::ty::fold::{TyFoldable, TyFolder};
use crate::analysis::ty::pattern_analysis::is_exhaustive;
use crate::analysis::ty::ty_def::TyId;
use crate::analysis::ty::visitor::{TyVisitable, TyVisitor};
use crate::core::hir_def::{EnumVariant, FieldParent, IdentId, LitKind, PatId, VariantKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct BindingRef<'db> {
    pub name: IdentId<'db>,
    pub representative_pat: PatId,
}

#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub struct ValidatedPat<'db> {
    // Match-analysis type only. This intentionally stays separate from the final
    // binding type and from the carrier/source type used during semantic lowering.
    match_ty: TyId<'db>,
    kind: ValidatedPatKind<'db>,
}

impl<'db> ValidatedPat<'db> {
    pub fn new(match_ty: TyId<'db>, kind: ValidatedPatKind<'db>) -> Self {
        Self { match_ty, kind }
    }

    pub fn match_ty(&self) -> TyId<'db> {
        self.match_ty
    }

    pub fn kind(&self) -> &ValidatedPatKind<'db> {
        &self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub enum ValidatedPatKind<'db> {
    Wildcard {
        binding: Option<BindingRef<'db>>,
    },
    Constructor {
        ctor: ConstructorKind<'db>,
        fields: Vec<ValidatedPatId>,
    },
    Or(Vec<ValidatedPatId>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct ValidatedPatId(u32);
entity_impl!(ValidatedPatId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum PatternAnalysisStatus {
    Ready(ValidatedPatId),
    Invalid,
}

impl PatternAnalysisStatus {
    pub fn ready_root(self) -> Option<ValidatedPatId> {
        match self {
            Self::Ready(root) => Some(root),
            Self::Invalid => None,
        }
    }

    pub fn is_ready(self) -> bool {
        matches!(self, Self::Ready(..))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, SalsaValue)]
pub struct PatternStore<'db> {
    nodes: Vec<ValidatedPat<'db>>,
    roots_by_pat: FxHashMap<PatId, ValidatedPatId>,
}

impl<'db> PatternStore<'db> {
    pub fn alloc(&mut self, node: ValidatedPat<'db>) -> ValidatedPatId {
        let id = ValidatedPatId::new(self.nodes.len());
        self.nodes.push(node);
        id
    }

    pub fn node(&self, id: ValidatedPatId) -> &ValidatedPat<'db> {
        &self.nodes[id.index()]
    }

    pub fn set_root(&mut self, pat: PatId, root: ValidatedPatId) {
        self.roots_by_pat.insert(pat, root);
    }

    pub fn clear_root(&mut self, pat: PatId) {
        self.roots_by_pat.remove(&pat);
    }

    pub fn root(&self, pat: PatId) -> Option<ValidatedPatId> {
        self.roots_by_pat.get(&pat).copied()
    }

    /// A pattern is irrefutable when it alone is exhaustive. A product is
    /// proven field by field, so that the exhaustiveness analysis, which is
    /// bounded, only sees or-patterns.
    pub fn is_irrefutable(&self, db: &'db dyn HirAnalysisDb, id: ValidatedPatId) -> bool {
        match self.node(id).kind() {
            ValidatedPatKind::Wildcard { .. } => true,
            ValidatedPatKind::Constructor { ctor, fields } => {
                let only_ctor = match ctor {
                    ConstructorKind::Type(_) => true,
                    ConstructorKind::Variant(variant, _) => variant.enum_.len_variants(db) == 1,
                    ConstructorKind::Literal(..) => false,
                };
                only_ctor && fields.iter().all(|&field| self.is_irrefutable(db, field))
            }
            ValidatedPatKind::Or(pats) => {
                pats.iter().any(|&pat| self.is_irrefutable(db, pat))
                    || is_exhaustive(db, self, pats, self.node(id).match_ty)
            }
        }
    }

    /// Whether `id` binds a name anywhere in it.
    pub fn binds(&self, id: ValidatedPatId) -> bool {
        match self.node(id).kind() {
            ValidatedPatKind::Wildcard { binding } => binding.is_some(),
            ValidatedPatKind::Constructor { fields, .. } => {
                fields.iter().any(|&field| self.binds(field))
            }
            ValidatedPatKind::Or(pats) => pats.iter().any(|&pat| self.binds(pat)),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ConstructorKind<'db> {
    Variant(EnumVariant<'db>, TyId<'db>),
    Type(TyId<'db>),
    Literal(LitKind<'db>, TyId<'db>),
}

impl<'db> ConstructorKind<'db> {
    pub fn field_types(&self, db: &'db dyn HirAnalysisDb) -> Vec<TyId<'db>> {
        match self {
            Self::Variant(variant, ty) => {
                if let Some(adt_def) = ty.adt_def(db) {
                    let args = ty.generic_args(db);
                    adt_def
                        .fields(db)
                        .get(variant.idx as usize)
                        .map(|field_list| {
                            field_list
                                .iter_types(db)
                                .enumerate()
                                .map(|(field_idx, _)| {
                                    instantiate_adt_field_shape(
                                        db,
                                        adt_def,
                                        variant.idx as usize,
                                        field_idx,
                                        args,
                                    )
                                })
                                .collect()
                        })
                        .unwrap_or_default()
                } else {
                    Vec::new()
                }
            }
            Self::Type(ty) => ty.field_types(db),
            Self::Literal(_, _) => Vec::new(),
        }
    }

    /// The names of a record constructor's fields, in order; `None` for a
    /// field whose name doesn't parse.
    pub fn field_names(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<SmallVec<[Option<IdentId<'db>>; 4]>> {
        let field_parent = match self {
            Self::Variant(variant, _) if matches!(variant.kind(db), VariantKind::Record(..)) => {
                Some(FieldParent::Variant(*variant))
            }
            Self::Type(ty) => match ty.adt_def(db)?.adt_ref(db) {
                AdtRef::Struct(struct_) => Some(FieldParent::Struct(struct_)),
                _ => None,
            },
            Self::Variant(..) | Self::Literal(..) => None,
        }?;
        Some(
            field_parent
                .fields(db)
                .map(|field| field.name(db))
                .collect(),
        )
    }

    pub fn arity(&self, db: &'db dyn HirAnalysisDb) -> usize {
        match self {
            Self::Variant(variant, _) => match variant.kind(db) {
                VariantKind::Unit => 0,
                VariantKind::Tuple(types) => types.data(db).len(),
                VariantKind::Record(fields) => fields.data(db).len(),
            },
            Self::Type(ty) => ty.field_count(db),
            Self::Literal(_, _) => 0,
        }
    }
}

impl<'db> ConstructorKind<'db> {
    fn ty(&self) -> TyId<'db> {
        match self {
            Self::Variant(_, ty) | Self::Type(ty) | Self::Literal(_, ty) => *ty,
        }
    }

    fn with_ty(self, ty: TyId<'db>) -> Self {
        match self {
            Self::Variant(variant, _) => Self::Variant(variant, ty),
            Self::Type(_) => Self::Type(ty),
            Self::Literal(lit, _) => Self::Literal(lit, ty),
        }
    }
}

impl<'db> TyVisitable<'db> for PatternStore<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        for node in &self.nodes {
            node.match_ty.visit_with(visitor);
            if let ValidatedPatKind::Constructor { ctor, .. } = &node.kind {
                ctor.ty().visit_with(visitor);
            }
        }
    }
}

impl<'db> TyFoldable<'db> for PatternStore<'db> {
    fn super_fold_with<F>(mut self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        for node in &mut self.nodes {
            node.match_ty = node.match_ty.fold_with(db, folder);
            if let ValidatedPatKind::Constructor { ctor, .. } = &mut node.kind {
                *ctor = ctor.with_ty(ctor.ty().fold_with(db, folder));
            }
        }
        self
    }
}
