use std::collections::hash_map::Entry;

use crate::{
    hir_def::{
        FieldParent, IdentId, VariantKind as HirVariantKind,
        scope_graph::ScopeId,
    },
    span::DynLazySpan,
};
use rustc_hash::FxHashMap;

use super::{TyChecker, env::LocalBinding};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, ResolvedVariant, diagnostics::PathResDiag, is_scope_visible_from},
    ty::{
        adt_def::{AdtDef, AdtField, AdtRef},
        diagnostics::{BodyDiag, FuncBodyDiag},
        ty_def::{InvalidCause, TyData, TyId},
    },
};

impl<'db> TyId<'db> {
    pub(crate) fn adt_ref(&self, db: &'db dyn HirAnalysisDb) -> Option<AdtRef<'db>> {
        self.adt_def(db).map(|def| def.adt_ref(db))
    }

    pub(crate) fn adt_def(&self, db: &'db dyn HirAnalysisDb) -> Option<AdtDef<'db>> {
        let base = self.decompose_ty_app(db).0;
        match base.data(db) {
            TyData::TyBase(base) => base.adt(),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub(super) enum ResolvedPathInBody<'db> {
    Reso(PathRes<'db>),
    Binding(LocalBinding<'db>),
    NewBinding(IdentId<'db>),

    #[allow(dead_code)]
    Diag(FuncBodyDiag<'db>),
    Invalid,
}

pub(super) struct RecordInitChecker<'tc, 'db, 'a> {
    pub(super) tc: &'tc mut TyChecker<'db>,
    data: &'a RecordLike<'db>,
    already_given: FxHashMap<IdentId<'db>, DynLazySpan<'db>>,
    invalid_field_given: bool,
}

impl<'tc, 'db, 'a> RecordInitChecker<'tc, 'db, 'a> {
    /// Create a new `RecordInitChecker` for the given record path.
    ///
    /// ## Panics
    /// Panics if the given `data` is not a record.
    pub(super) fn new(tc: &'tc mut TyChecker<'db>, data: &'a RecordLike<'db>) -> Self {
        assert!(data.is_record(tc.db));

        Self {
            tc,
            data,
            already_given: FxHashMap::default(),
            invalid_field_given: false,
        }
    }

    /// Feed a label to the checker.
    /// Returns the type of the field if the label is valid, otherwise returns
    /// an error.
    pub(super) fn feed_label(
        &mut self,
        label: Option<IdentId<'db>>,
        field_span: DynLazySpan<'db>,
    ) -> Result<TyId<'db>, FuncBodyDiag<'db>> {
        let label = match label {
            Some(label) => match self.already_given.entry(label) {
                Entry::Occupied(first_use) => {
                    let diag = BodyDiag::DuplicatedRecordFieldBind {
                        primary: field_span.clone(),
                        first_use: first_use.get().clone(),
                        name: label,
                    };

                    self.invalid_field_given = true;
                    return Err(diag.into());
                }

                Entry::Vacant(entry) => {
                    entry.insert(field_span.clone());
                    label
                }
            },

            None => {
                let diag = BodyDiag::ExplicitLabelExpectedInRecord {
                    primary: field_span,
                    hint: self.data.initializer_hint(self.tc.db),
                };

                self.invalid_field_given = true;
                return Err(diag.into());
            }
        };

        let Some(ty) = self.data.record_field_ty(self.tc.db, label) else {
            let diag = BodyDiag::RecordFieldNotFound {
                span: field_span,
                label,
            };

            self.invalid_field_given = true;
            return Err(diag.into());
        };

        let field_scope = self.data.record_field_scope(self.tc.db, label).unwrap();
        if is_scope_visible_from(self.tc.db, field_scope, self.tc.env.scope()) {
            Ok(ty)
        } else {
            let diag = PathResDiag::Invisible(field_span, label, field_scope.name_span(self.tc.db));

            self.invalid_field_given = true;
            Err(diag.into())
        }
    }

    /// Finalize the checker and return an error if there are missing fields.
    pub(super) fn finalize(
        self,
        initializer_span: DynLazySpan<'db>,
        allow_missing_field: bool,
    ) -> Result<(), FuncBodyDiag<'db>> {
        if !self.invalid_field_given && !allow_missing_field {
            let expected_labels = self.data.record_labels(self.tc.db);
            let missing_fields: Vec<_> = expected_labels
                .iter()
                .filter(|f| !self.already_given.contains_key(f))
                .cloned()
                .collect();

            if !missing_fields.is_empty() {
                let diag = BodyDiag::MissingRecordFields {
                    primary: initializer_span,
                    missing_fields,
                    hint: self.data.initializer_hint(self.tc.db),
                };

                return Err(diag.into());
            }
        }

        Ok(())
    }
}

/// Enum that can represent different types of records (structs or variants)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RecordLike<'db> {
    Type(TyId<'db>),
    Variant(ResolvedVariant<'db>),
}

impl<'db> RecordLike<'db> {
    pub fn is_record(&self, db: &'db dyn HirAnalysisDb) -> bool {
        match self {
            RecordLike::Type(ty) => ty
                .adt_ref(db)
                .is_some_and(|adt_ref| matches!(adt_ref, AdtRef::Struct(_) | AdtRef::Contract(_))),
            RecordLike::Variant(variant) => {
                matches!(variant.kind(db), HirVariantKind::Record(..))
            }
        }
    }

    pub fn record_field_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<TyId<'db>> {
        match self {
            RecordLike::Type(ty) => {
                let adt_def = ty.adt_def(db)?;
                let (hir_field_list_id, adt_field_list_ref) = match adt_def.adt_ref(db) {
                    AdtRef::Struct(s) => Some((s.fields(db), &adt_def.fields(db)[0])),
                    AdtRef::Contract(c) => Some((c.fields(db), &adt_def.fields(db)[0])),
                    _ => None,
                }?;

                let field_idx = hir_field_list_id.field_idx(db, name)?;
                let args = ty.generic_args(db);
                let field_ty = adt_field_list_ref.ty(db, field_idx).instantiate(db, args);

                if field_ty.is_star_kind(db) {
                    Some(field_ty)
                } else {
                    Some(TyId::invalid(db, InvalidCause::Other))
                }
            }
            RecordLike::Variant(variant) => {
                let adt_def = variant.ty.adt_def(db)?;
                let (hir_field_list_id, adt_field_list_ref) = match variant.kind(db) {
                    HirVariantKind::Record(fields_id) => {
                        Some((fields_id, &adt_def.fields(db)[variant.variant.idx as usize]))
                    }
                    _ => None,
                }?;

                let field_idx = hir_field_list_id.field_idx(db, name)?;
                let args = variant.ty.generic_args(db);
                let field_ty = adt_field_list_ref.ty(db, field_idx).instantiate(db, args);

                if field_ty.is_star_kind(db) {
                    Some(field_ty)
                } else {
                    Some(TyId::invalid(db, InvalidCause::Other))
                }
            }
        }
    }

    pub fn record_field_idx(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<usize> {
        match self {
            RecordLike::Type(ty) => {
                let adt_def = ty.adt_def(db)?;
                let parent = match adt_def.adt_ref(db) {
                    AdtRef::Struct(s) => FieldParent::Struct(s),
                    AdtRef::Contract(c) => FieldParent::Contract(c),
                    _ => return None,
                };
                parent
                    .fields(db)
                    .enumerate()
                    .find(|(_, v)| v.name(db) == Some(name))
                    .map(|(i, _)| i)
            }
            RecordLike::Variant(variant) => {
                if !matches!(variant.kind(db), HirVariantKind::Record(_)) {
                    return None;
                }
                let parent = FieldParent::Variant(variant.variant);
                parent
                    .fields(db)
                    .enumerate()
                    .find(|(_, v)| v.name(db) == Some(name))
                    .map(|(i, _)| i)
            }
        }
    }

    pub fn record_field_scope(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<ScopeId<'db>> {
        match self {
            RecordLike::Type(ty) => {
                let field_idx = RecordLike::Type(*ty).record_field_idx(db, name)?;
                let adt_ref = ty.adt_ref(db)?;
                let parent = match adt_ref {
                    AdtRef::Struct(s) => FieldParent::Struct(s),
                    AdtRef::Contract(c) => FieldParent::Contract(c),
                    _ => return None,
                };
                Some(ScopeId::Field(parent, field_idx as u16))
            }
            RecordLike::Variant(variant) => {
                let field_idx = RecordLike::Variant(*variant).record_field_idx(db, name)?;
                let parent = FieldParent::Variant(variant.variant);
                Some(ScopeId::Field(parent, field_idx as u16))
            }
        }
    }

    pub fn record_labels(&self, db: &'db dyn HirAnalysisDb) -> Vec<IdentId<'db>> {
        match self {
            RecordLike::Type(ty) => {
                let Some(adt_ref) = ty.adt_ref(db) else {
                    return Vec::default();
                };
                let parent = match adt_ref {
                    AdtRef::Struct(s) => FieldParent::Struct(s),
                    AdtRef::Contract(c) => FieldParent::Contract(c),
                    _ => return Vec::default(),
                };
                parent.fields(db).filter_map(|v| v.name(db)).collect()
            }
            RecordLike::Variant(variant) => {
                if !matches!(variant.kind(db), HirVariantKind::Record(_)) {
                    return Vec::default();
                }
                let parent = FieldParent::Variant(variant.variant);
                parent.fields(db).filter_map(|v| v.name(db)).collect()
            }
        }
    }

    pub fn initializer_hint(&self, db: &'db dyn HirAnalysisDb) -> Option<String> {
        match self {
            RecordLike::Type(ty) => {
                if ty.adt_ref(db).is_some() {
                    let AdtRef::Struct(s) = ty.adt_ref(db)? else {
                        return None;
                    };

                    let name = s.name(db).unwrap().data(db);
                    let init_args = s.format_initializer_args(db);
                    Some(format!("{name}{init_args}"))
                } else {
                    None
                }
            }
            RecordLike::Variant(variant) => {
                let expected_sub_pat = variant.variant.def(db).format_initializer_args(db);
                let path = variant.path.pretty_print(db);
                Some(format!("{path}{expected_sub_pat}"))
            }
        }
    }

    pub fn kind_name(&self, db: &'db dyn HirAnalysisDb) -> String {
        match self {
            RecordLike::Type(ty) => {
                if let Some(adt_ref) = ty.adt_ref(db) {
                    adt_ref.kind_name().to_string()
                } else if ty.is_func(db) {
                    "fn".to_string()
                } else {
                    ty.pretty_print(db).to_string()
                }
            }
            RecordLike::Variant(variant) => match variant.kind(db) {
                HirVariantKind::Unit => "unit variant",
                HirVariantKind::Tuple(_) => "tuple variant",
                HirVariantKind::Record(_) => "record variant",
            }
            .to_string(),
        }
    }

    pub fn from_ty(ty: TyId<'db>) -> Self {
        RecordLike::Type(ty)
    }

    pub fn from_variant(variant: ResolvedVariant<'db>) -> Self {
        RecordLike::Variant(variant)
    }
}
