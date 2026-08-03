use cranelift_entity::{EntityRef, entity_impl};
use rustc_hash::FxHashMap;
use salsa::Update;
use smallvec1::SmallVec;

use crate::analysis::HirAnalysisDb;
use crate::analysis::semantic::{SemConstId, SemConstScalar, SemConstValue};
use crate::analysis::ty::adt_def::{AdtRef, instantiate_adt_field_shape};
use crate::analysis::ty::fold::{TyFoldable, TyFolder};
use crate::analysis::ty::ty_def::TyId;
use crate::analysis::ty::visitor::{TyVisitable, TyVisitor};
use crate::core::hir_def::{
    EnumVariant, FieldParent, IdentId, IntegerId, LitKind, PatId, VariantKind,
};

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

    pub fn has_binding(&self, id: ValidatedPatId) -> bool {
        match self.node(id).kind() {
            ValidatedPatKind::Wildcard { binding } => binding.is_some(),
            ValidatedPatKind::Constructor { fields, .. } | ValidatedPatKind::Or(fields) => {
                fields.iter().any(|field| self.has_binding(*field))
            }
        }
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum KnownScrutineeMatch {
    Never,
    Maybe,
    Always,
}

/// A statically-known scrutinee shape used to prove pattern reachability.
///
/// Constructor children retain the source field order expected by validated
/// patterns. An unknown child preserves a known outer constructor without
/// making an optimistic claim about a refutable payload pattern.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum KnownPatternScrutinee<'db> {
    Unknown,
    Variant {
        variant: EnumVariant<'db>,
        fields: Box<[KnownPatternScrutinee<'db>]>,
    },
    Type {
        ty: TyId<'db>,
        fields: Box<[KnownPatternScrutinee<'db>]>,
    },
    Literal(LitKind<'db>),
}

impl<'db> KnownPatternScrutinee<'db> {
    pub(crate) fn variant(
        variant: EnumVariant<'db>,
        fields: impl IntoIterator<Item = Self>,
    ) -> Self {
        Self::Variant {
            variant,
            fields: fields.into_iter().collect(),
        }
    }

    pub(crate) fn type_constructor(ty: TyId<'db>, fields: impl IntoIterator<Item = Self>) -> Self {
        Self::Type {
            ty,
            fields: fields.into_iter().collect(),
        }
    }

    pub(crate) fn known_bool(&self) -> Option<bool> {
        match self {
            Self::Literal(LitKind::Bool(value)) => Some(*value),
            Self::Unknown | Self::Variant { .. } | Self::Type { .. } | Self::Literal(_) => None,
        }
    }
}

/// Converts an evaluated constant into the same structural vocabulary used
/// for direct HIR constructors. Unsupported scalar encodings remain unknown.
pub(crate) fn known_pattern_scrutinee_from_const<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
) -> KnownPatternScrutinee<'db> {
    match value.value(db) {
        SemConstValue::Unit => {
            KnownPatternScrutinee::type_constructor(TyId::unit(db), std::iter::empty())
        }
        SemConstValue::Scalar {
            value: SemConstScalar::Bool(value),
            ..
        } => KnownPatternScrutinee::Literal(LitKind::Bool(value)),
        SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } => value
            .to_biguint()
            .map_or(KnownPatternScrutinee::Unknown, |value| {
                KnownPatternScrutinee::Literal(LitKind::Int(IntegerId::new(db, value)))
            }),
        SemConstValue::Scalar {
            value: SemConstScalar::Bytes(_),
            ..
        }
        | SemConstValue::TypeLevel { .. } => KnownPatternScrutinee::Unknown,
        SemConstValue::Tuple { ty, elems } | SemConstValue::Array { ty, elems } => {
            KnownPatternScrutinee::type_constructor(
                ty,
                elems
                    .iter()
                    .map(|field| known_pattern_scrutinee_from_const(db, *field)),
            )
        }
        SemConstValue::Struct { ty, fields } => KnownPatternScrutinee::type_constructor(
            ty,
            fields
                .iter()
                .map(|field| known_pattern_scrutinee_from_const(db, *field)),
        ),
        SemConstValue::Enum {
            ty,
            variant,
            fields,
        } => {
            let Some(enum_) = ty.as_enum(db) else {
                return KnownPatternScrutinee::Unknown;
            };
            if usize::from(variant.0) >= enum_.len_variants(db) {
                return KnownPatternScrutinee::Unknown;
            }
            KnownPatternScrutinee::variant(
                EnumVariant::new(enum_, usize::from(variant.0)),
                fields
                    .iter()
                    .map(|field| known_pattern_scrutinee_from_const(db, *field)),
            )
        }
    }
}

/// Whether a validated single pattern can match or miss its scrutinee.
///
/// This is shared by type-checker completion analysis and semantic CFG
/// lowering so `if let` / `while let` agree about statically unreachable
/// branches.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct PatternBranchReachability {
    pub can_match: bool,
    pub can_miss: bool,
}

impl PatternBranchReachability {
    pub(crate) const MATCH_ONLY: Self = Self {
        can_match: true,
        can_miss: false,
    };
    pub(crate) const MISS_ONLY: Self = Self {
        can_match: false,
        can_miss: true,
    };
    pub(crate) const BOTH: Self = Self {
        can_match: true,
        can_miss: true,
    };
}

/// Returns the statically-known branch reachability for one pattern test.
///
/// An irrefutable pattern always matches. For a direct, statically-known enum
/// variant, the same recursive variant proof used for `match` arms can also
/// prove that a refutable pattern always matches or always misses. `None`
/// means the pattern has not been validated yet, so callers must remain
/// conservative.
pub(crate) fn single_pattern_branch_reachability<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    pat: PatId,
    known_scrutinee: Option<&KnownPatternScrutinee<'db>>,
) -> Option<PatternBranchReachability> {
    let root = store.root(pat)?;
    if store.is_irrefutable(db, root) {
        return Some(PatternBranchReachability::MATCH_ONLY);
    }

    Some(match known_scrutinee {
        Some(scrutinee) => match known_scrutinee_matches(db, store, root, scrutinee) {
            KnownScrutineeMatch::Never => PatternBranchReachability::MISS_ONLY,
            KnownScrutineeMatch::Maybe => PatternBranchReachability::BOTH,
            KnownScrutineeMatch::Always => PatternBranchReachability::MATCH_ONLY,
        },
        None => PatternBranchReachability::BOTH,
    })
}

/// Returns which match arms can be selected, in source order, for a direct,
/// statically-known scrutinee constructor.
///
/// `None` means one of the patterns has not been validated yet, so callers
/// must conservatively keep every arm. Once an arm always matches, later arms
/// are unreachable under first-match semantics.
pub(crate) fn known_scrutinee_arm_reachability<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    pats: impl IntoIterator<Item = PatId>,
    scrutinee: &KnownPatternScrutinee<'db>,
) -> Option<Vec<bool>> {
    let roots = pats
        .into_iter()
        .map(|pat| store.root(pat))
        .collect::<Option<Vec<_>>>()?;
    let mut still_unmatched = true;
    Some(
        roots
            .into_iter()
            .map(|root| {
                if !still_unmatched {
                    return false;
                }
                match known_scrutinee_matches(db, store, root, scrutinee) {
                    KnownScrutineeMatch::Never => false,
                    KnownScrutineeMatch::Maybe => true,
                    KnownScrutineeMatch::Always => {
                        still_unmatched = false;
                        true
                    }
                }
            })
            .collect(),
    )
}

fn known_scrutinee_matches<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    pat: ValidatedPatId,
    scrutinee: &KnownPatternScrutinee<'db>,
) -> KnownScrutineeMatch {
    if matches!(scrutinee, KnownPatternScrutinee::Unknown) {
        return if store.is_irrefutable(db, pat) {
            KnownScrutineeMatch::Always
        } else {
            KnownScrutineeMatch::Maybe
        };
    }

    match store.node(pat).kind() {
        ValidatedPatKind::Wildcard { .. } => KnownScrutineeMatch::Always,
        ValidatedPatKind::Constructor {
            ctor: ConstructorKind::Variant(candidate, _),
            fields,
        } => match scrutinee {
            KnownPatternScrutinee::Variant { variant, .. } if candidate != variant => {
                KnownScrutineeMatch::Never
            }
            KnownPatternScrutinee::Variant {
                fields: known_fields,
                ..
            } => known_constructor_fields_match(db, store, fields, known_fields),
            KnownPatternScrutinee::Type { .. } | KnownPatternScrutinee::Literal(_) => {
                KnownScrutineeMatch::Never
            }
            KnownPatternScrutinee::Unknown => unreachable!("handled above"),
        },
        ValidatedPatKind::Constructor {
            ctor: ConstructorKind::Literal(candidate, _),
            ..
        } => match scrutinee {
            KnownPatternScrutinee::Literal(value) if *candidate == *value => {
                KnownScrutineeMatch::Always
            }
            KnownPatternScrutinee::Literal(_)
            | KnownPatternScrutinee::Variant { .. }
            | KnownPatternScrutinee::Type { .. } => KnownScrutineeMatch::Never,
            KnownPatternScrutinee::Unknown => unreachable!("handled above"),
        },
        ValidatedPatKind::Constructor {
            ctor: ConstructorKind::Type(candidate),
            fields,
        } => match scrutinee {
            KnownPatternScrutinee::Type {
                ty,
                fields: known_fields,
            } if candidate == ty => known_constructor_fields_match(db, store, fields, known_fields),
            KnownPatternScrutinee::Type { .. } => KnownScrutineeMatch::Never,
            KnownPatternScrutinee::Variant { .. } | KnownPatternScrutinee::Literal(_) => {
                KnownScrutineeMatch::Never
            }
            KnownPatternScrutinee::Unknown => unreachable!("handled above"),
        },
        ValidatedPatKind::Or(pats) => {
            let mut result = KnownScrutineeMatch::Never;
            for pat in pats {
                match known_scrutinee_matches(db, store, *pat, scrutinee) {
                    KnownScrutineeMatch::Always => return KnownScrutineeMatch::Always,
                    KnownScrutineeMatch::Maybe => result = KnownScrutineeMatch::Maybe,
                    KnownScrutineeMatch::Never => {}
                }
            }
            result
        }
    }
}

fn known_constructor_fields_match<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    patterns: &[ValidatedPatId],
    values: &[KnownPatternScrutinee<'db>],
) -> KnownScrutineeMatch {
    if patterns.len() != values.len() {
        return KnownScrutineeMatch::Maybe;
    }

    let mut result = KnownScrutineeMatch::Always;
    for (pattern, value) in patterns.iter().zip(values) {
        match known_scrutinee_matches(db, store, *pattern, value) {
            KnownScrutineeMatch::Never => return KnownScrutineeMatch::Never,
            KnownScrutineeMatch::Maybe => result = KnownScrutineeMatch::Maybe,
            KnownScrutineeMatch::Always => {}
        }
    }
    result
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
