use cranelift_entity::{EntityRef, entity_impl};
use rustc_hash::FxHashMap;
use salsa::Update;
use smallvec1::SmallVec;

use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::adt_def::{AdtRef, instantiate_adt_field_shape};
use crate::analysis::ty::fold::{TyFoldable, TyFolder};
use crate::analysis::ty::ty_def::TyId;
use crate::analysis::ty::visitor::{TyVisitable, TyVisitor};
use crate::core::hir_def::{EnumVariant, FieldParent, IdentId, LitKind, PatId, VariantKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct BindingRef<'db> {
    pub name: IdentId<'db>,
    pub representative_pat: PatId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct PatternMatchTy<'db>(TyId<'db>);

impl<'db> PatternMatchTy<'db> {
    pub fn new(ty: TyId<'db>) -> Self {
        Self(ty)
    }

    pub fn raw(self) -> TyId<'db> {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ValidatedPat<'db> {
    // Match-analysis type only. This intentionally stays separate from the final
    // binding type and from the carrier/source type used during semantic lowering.
    match_ty: PatternMatchTy<'db>,
    kind: ValidatedPatKind<'db>,
}

impl<'db> ValidatedPat<'db> {
    pub fn new(match_ty: TyId<'db>, kind: ValidatedPatKind<'db>) -> Self {
        Self {
            match_ty: PatternMatchTy::new(match_ty),
            kind,
        }
    }

    pub fn match_ty(&self) -> PatternMatchTy<'db> {
        self.match_ty
    }

    pub fn kind(&self) -> &ValidatedPatKind<'db> {
        &self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ValidatedPatId(u32);
entity_impl!(ValidatedPatId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum PatternAnalysisStatus {
    Ready(ValidatedPatId),
    Invalid,
    Unsupported,
}

impl PatternAnalysisStatus {
    pub fn ready_root(self) -> Option<ValidatedPatId> {
        match self {
            Self::Ready(root) => Some(root),
            Self::Invalid | Self::Unsupported => None,
        }
    }

    pub fn is_ready(self) -> bool {
        matches!(self, Self::Ready(..))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Update)]
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

    pub fn iter(&self) -> impl Iterator<Item = &ValidatedPat<'db>> {
        self.nodes.iter()
    }

    pub fn is_irrefutable(&self, db: &'db dyn HirAnalysisDb, id: ValidatedPatId) -> bool {
        match self.node(id).kind() {
            ValidatedPatKind::Wildcard { .. } => true,
            ValidatedPatKind::Constructor { ctor, fields } => match ctor {
                ConstructorKind::Type(_) => {
                    fields.iter().all(|field| self.is_irrefutable(db, *field))
                }
                ConstructorKind::Variant(variant, _) if variant.enum_.len_variants(db) == 1 => {
                    fields.iter().all(|field| self.is_irrefutable(db, *field))
                }
                ConstructorKind::Variant(..) | ConstructorKind::Literal(..) => false,
            },
            ValidatedPatKind::Or(pats) => {
                pats.iter().any(|pat| self.is_irrefutable(db, *pat))
                    || crate::analysis::ty::pattern_analysis::is_exhaustive(
                        db,
                        self,
                        pats,
                        self.node(id).match_ty().raw(),
                    )
            }
        }
    }

    pub fn mir_unsupported_reason(&self, id: ValidatedPatId) -> Option<&'static str> {
        match self.node(id).kind() {
            ValidatedPatKind::Wildcard { .. } => None,
            ValidatedPatKind::Constructor { ctor, fields } => match ctor {
                ConstructorKind::Variant(..) | ConstructorKind::Type(_) => fields
                    .iter()
                    .find_map(|field| self.mir_unsupported_reason(*field)),
                ConstructorKind::Literal(LitKind::Int(_) | LitKind::Bool(_), _) => fields
                    .iter()
                    .find_map(|field| self.mir_unsupported_reason(*field)),
                ConstructorKind::Literal(LitKind::String(_), _) => {
                    Some("string literal patterns are not supported in MIR lowering")
                }
            },
            ValidatedPatKind::Or(pats) => pats
                .iter()
                .find_map(|pat| self.mir_unsupported_reason(*pat)),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
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

    pub fn field_names(&self, db: &'db dyn HirAnalysisDb) -> Option<SmallVec<[IdentId<'db>; 4]>> {
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
                .filter_map(|field| field.name(db))
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

pub fn ctor_variant_num<'db>(db: &'db dyn HirAnalysisDb, ctor: &ConstructorKind<'db>) -> usize {
    match ctor {
        ConstructorKind::Variant(variant, _) => variant.enum_.len_variants(db),
        ConstructorKind::Type(_) => 1,
        ConstructorKind::Literal(LitKind::Bool(_), _) => 2,
        ConstructorKind::Literal(LitKind::Int(_), _)
        | ConstructorKind::Literal(LitKind::String(_), _) => usize::MAX,
    }
}

impl<'db> TyVisitable<'db> for BindingRef<'db> {
    fn visit_with<V>(&self, _visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
    }
}

impl<'db> TyFoldable<'db> for BindingRef<'db> {
    fn super_fold_with<F>(self, _db: &'db dyn HirAnalysisDb, _folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self
    }
}

impl<'db> TyVisitable<'db> for ValidatedPatId {
    fn visit_with<V>(&self, _visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
    }
}

impl<'db> TyFoldable<'db> for ValidatedPatId {
    fn super_fold_with<F>(self, _db: &'db dyn HirAnalysisDb, _folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self
    }
}

impl<'db> TyVisitable<'db> for PatternAnalysisStatus {
    fn visit_with<V>(&self, _visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
    }
}

impl<'db> TyFoldable<'db> for PatternAnalysisStatus {
    fn super_fold_with<F>(self, _db: &'db dyn HirAnalysisDb, _folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self
    }
}

impl<'db> TyVisitable<'db> for ValidatedPat<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.match_ty.visit_with(visitor);
        self.kind.visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for ValidatedPat<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            match_ty: self.match_ty.fold_with(db, folder),
            kind: self.kind.fold_with(db, folder),
        }
    }
}

impl<'db> TyVisitable<'db> for PatternMatchTy<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.0.visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for PatternMatchTy<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self(self.0.fold_with(db, folder))
    }
}

impl<'db> TyVisitable<'db> for ValidatedPatKind<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::Wildcard { binding } => {
                if let Some(binding) = binding {
                    binding.visit_with(visitor);
                }
            }
            Self::Constructor { ctor, fields } => {
                ctor.visit_with(visitor);
                fields.visit_with(visitor);
            }
            Self::Or(pats) => pats.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for ValidatedPatKind<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self {
            Self::Wildcard { binding } => Self::Wildcard {
                binding: binding.map(|binding| binding.fold_with(db, folder)),
            },
            Self::Constructor { ctor, fields } => Self::Constructor {
                ctor: ctor.fold_with(db, folder),
                fields: fields.fold_with(db, folder),
            },
            Self::Or(pats) => Self::Or(pats.fold_with(db, folder)),
        }
    }
}

impl<'db> TyVisitable<'db> for ConstructorKind<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::Variant(_, ty) | Self::Type(ty) | Self::Literal(_, ty) => ty.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for ConstructorKind<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self {
            Self::Variant(variant, ty) => Self::Variant(variant, ty.fold_with(db, folder)),
            Self::Type(ty) => Self::Type(ty.fold_with(db, folder)),
            Self::Literal(lit, ty) => Self::Literal(lit, ty.fold_with(db, folder)),
        }
    }
}

impl<'db> TyVisitable<'db> for PatternStore<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.nodes.visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for PatternStore<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            nodes: self.nodes.fold_with(db, folder),
            roots_by_pat: self.roots_by_pat,
        }
    }
}
