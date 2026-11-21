//! Bounded structural input for checked generation.
//!
//! These handles retain existing semantic type and declaration identities.
//! They are not a context, a proof record, or a second reflection database.
//! Output publication must happen after provider evaluation has finished.

use parser::ast::{self, prelude::*};
use salsa::SalsaValue;

use crate::{
    analysis::{HirAnalysisDb, ty::ty_def::TyId},
    hir_def::{Enum, IdentId, Struct, VariantKind},
    span::HirOrigin,
};

/// Issue one checked metadata argument for evaluation of an already-finalized
/// provider instance. The constructor stays within HIR's issued-value boundary.
pub fn record_input_argument<'db>(
    db: &'db dyn HirAnalysisDb,
    input: RecordInput<'db>,
    parameter_owner: crate::hir_def::scope_graph::ScopeId<'db>,
) -> Result<crate::analysis::semantic::ctfe::ConstDesc<'db>, &'static str> {
    use crate::analysis::semantic::ctfe::{ConstDesc, VerifiedConstValueId};
    use crate::analysis::semantic::{SemConstId, SemConstValue};
    let ty = crate::analysis::ty::corelib::resolve_lib_type_path(
        db,
        input.scope(),
        "core::meta::Record",
    )
    .ok_or("missing canonical Record carrier")?;
    let value = SemConstId::new(
        db,
        SemConstValue::Meta {
            ty,
            value: MetaValue::Record(input),
        },
    );
    Ok(ConstDesc::value(
        db,
        VerifiedConstValueId::from_complete_execution(db, value)?,
        parameter_owner,
    ))
}

/// Search existing semantic type arguments and instantiated fields. Repeated
/// nominal expansion is stopped independently from argument traversal.
pub fn ty_contains_meta_carrier<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    use crate::analysis::ty::{
        adt_def::instantiate_adt_field_shape, normalize::normalize_ty,
        trait_resolution::PredicateListId, ty_def::TyData,
    };
    let mut pending = vec![(ty, Vec::new())];
    let mut seen = rustc_hash::FxHashSet::default();
    let mut expanded = rustc_hash::FxHashSet::default();
    while let Some((ty, ancestors)) = pending.pop() {
        let ty = match ty.data(db) {
            TyData::AssocTy(assoc) => normalize_ty(
                db,
                ty,
                assoc.trait_.def(db).scope(),
                PredicateListId::empty_list(db),
            ),
            _ => ty,
        };
        if is_canonical_meta_carrier_ty(db, ty) {
            return Some(ty);
        }
        let blocked_by_cycle = ty.adt_def(db).is_some_and(|adt| ancestors.contains(&adt));
        if !seen.insert((ty, blocked_by_cycle)) {
            continue;
        }
        pending.extend(
            ty.generic_args(db)
                .iter()
                .copied()
                .map(|arg| (arg, ancestors.clone())),
        );
        if let Some(adt) = ty.adt_def(db)
            && !blocked_by_cycle
            && !crate::semantic::adt_grows(db, adt)
            && expanded.insert((adt, ty.generic_args(db)))
        {
            let mut field_ancestors = ancestors;
            field_ancestors.push(adt);
            for (variant, fields) in adt.fields(db).iter().enumerate() {
                pending.extend((0..fields.num_types()).map(|field| {
                    (
                        normalize_ty(
                            db,
                            instantiate_adt_field_shape(
                                db,
                                adt,
                                variant,
                                field,
                                ty.generic_args(db),
                            ),
                            adt.scope(db),
                            PredicateListId::empty_list(db),
                        ),
                        field_ancestors.clone(),
                    )
                }));
            }
        }
    }
    None
}

/// Completed compile-time aggregates must contain genuine issued tokens at
/// every metadata leaf. Ordinary seal-shaped structs never substitute for them.
pub(crate) fn issued_metadata_leaves<'db>(
    db: &'db dyn HirAnalysisDb,
    value: crate::analysis::semantic::SemConstId<'db>,
) -> bool {
    use crate::analysis::semantic::SemConstValue;
    if is_canonical_meta_carrier_ty(db, crate::analysis::semantic::sem_const_ty(db, value)) {
        return matches!(
            value.value(db),
            SemConstValue::Meta { .. } | SemConstValue::ItemCode { .. }
        );
    }
    match value.value(db) {
        SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => {
            elems.iter().all(|value| issued_metadata_leaves(db, *value))
        }
        SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => fields
            .iter()
            .all(|value| issued_metadata_leaves(db, *value)),
        SemConstValue::Description(..) => false,
        _ => true,
    }
}

/// Canonical compile-time carriers are opaque checking tokens, not their
/// private source seal representation. Authority is the actual library ingot.
pub fn is_canonical_meta_carrier_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    let ty = ty.as_view(db).unwrap_or(ty);
    let (base, _) = ty.decompose_ty_app(db);
    let crate::analysis::ty::ty_def::TyData::TyBase(crate::analysis::ty::ty_def::TyBase::Adt(adt)) =
        base.data(db)
    else {
        return false;
    };
    let scope = adt.scope(db);
    [
        "core::meta::Record",
        "core::meta::Field",
        "core::meta::FieldType",
        "core::meta::TypeRef",
        "core::meta::Code",
    ]
    .iter()
    .any(|path| crate::analysis::ty::corelib::resolve_lib_type_path(db, scope, path) == Some(base))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RecordOwner<'db> {
    Struct(Struct<'db>),
    Enum(Enum<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RecordInput<'db> {
    ty: TyId<'db>,
    owner: RecordOwner<'db>,
    variant_index: Option<usize>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct RecordField<'db> {
    record: RecordInput<'db>,
    index: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum MetaValue<'db> {
    Record(RecordInput<'db>),
    Field(RecordField<'db>),
    FieldType(RecordField<'db>),
    TypeRef {
        ty: TyId<'db>,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
    },
}

impl<'db> MetaValue<'db> {
    pub(crate) fn scope(self) -> crate::hir_def::scope_graph::ScopeId<'db> {
        match self {
            Self::Record(record) => record.scope(),
            Self::Field(field) | Self::FieldType(field) => field.record().scope(),
            Self::TypeRef { scope, .. } => scope,
        }
    }

    pub(crate) fn carrier_path(self) -> &'static str {
        match self {
            Self::Record(_) => "core::meta::Record",
            Self::Field(_) => "core::meta::Field",
            Self::FieldType(_) => "core::meta::FieldType",
            Self::TypeRef { .. } => "core::meta::TypeRef",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RecordInputError {
    NotConcrete,
    NotRecord,
    NotAuthored,
    NotEnum,
    WholeEnumHasNoFields,
    InvalidField,
    InvisibleField,
    FieldOutsideRecord,
    VariantOutsideEnum,
    SelectedPayloadIsNotWholeEnum,
}

impl<'db> RecordInput<'db> {
    /// Identify the annotation's actual declaration in its owning source module.
    /// The declaration is the struct written at `syntax`, including the struct
    /// that `#[abi]`, `#[error]` or `#[event]` expands from it.
    pub fn for_authored_syntax(
        db: &'db dyn HirAnalysisDb,
        module: crate::hir_def::TopLevelMod<'db>,
        syntax: &ast::Struct,
    ) -> Result<Self, RecordInputError> {
        let root = crate::lower::top_mod_ast(db, module);
        let range = syntax.syntax().text_range();
        let declaration = module
            .all_structs(db)
            .iter()
            .find(|decl| decl.origin(db).authored_range(root.syntax()) == Some(range))
            .ok_or(RecordInputError::NotAuthored)?;
        let adt = crate::analysis::ty::adt_def::AdtRef::Struct(*declaration).as_adt(db);
        let ty = TyId::foldl(db, TyId::adt(db, adt), adt.params(db));
        Self::for_authored_adt_ty(db, ty)
    }

    pub fn for_authored_item(
        db: &'db dyn HirAnalysisDb,
        module: crate::hir_def::TopLevelMod<'db>,
        item: &ast::Item,
    ) -> Result<Self, RecordInputError> {
        match item.kind() {
            Some(ast::ItemKind::Struct(syntax)) => Self::for_authored_syntax(db, module, &syntax),
            Some(ast::ItemKind::Enum(syntax)) => {
                let origin = HirOrigin::raw(&syntax);
                let declaration = module
                    .all_enums(db)
                    .iter()
                    .find(|decl| *decl.origin(db) == origin)
                    .ok_or(RecordInputError::NotAuthored)?;
                let adt = crate::analysis::ty::adt_def::AdtRef::Enum(*declaration).as_adt(db);
                let ty = TyId::foldl(db, TyId::adt(db, adt), adt.params(db));
                Self::for_authored_adt_ty(db, ty)
            }
            _ => Err(RecordInputError::NotRecord),
        }
    }

    /// Issue a view of an already resolved, concrete authored record.
    /// The driver supplies the target of the authored generation request.
    /// No Fe constructor issues this handle.
    pub fn for_authored_type(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Result<Self, RecordInputError> {
        if !ty.is_star_kind(db)
            || !crate::analysis::ty::const_ty::ty_is_fully_ground(db, ty)
            || ty.has_invalid(db)
        {
            return Err(RecordInputError::NotConcrete);
        }
        Self::for_authored_adt_ty(db, ty)
    }

    /// Issue a complete record view only where its fields may be observed.
    /// Omitting hidden fields would describe a different record, so admission
    /// fails before the requesting provider can execute.
    pub fn for_authored_type_from(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        requesting_scope: crate::hir_def::scope_graph::ScopeId<'db>,
    ) -> Result<Self, RecordInputError> {
        if !crate::analysis::ty::const_ty::ty_is_fully_ground(db, ty) {
            return Err(RecordInputError::NotConcrete);
        }
        Self::issue_from(db, ty, requesting_scope)
    }

    /// Issue a view of the target of an explicit derive header that declares
    /// binders: ground except for those binders, whose fields have the
    /// header's parameters as their types. The header is the requesting scope.
    pub fn for_header_type(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        header: crate::hir_def::ItemKind<'db>,
    ) -> Result<Self, RecordInputError> {
        if !ground_over_header(db, ty, header) {
            return Err(RecordInputError::NotConcrete);
        }
        Self::issue_from(db, ty, header.scope())
    }

    fn issue_from(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        requesting_scope: crate::hir_def::scope_graph::ScopeId<'db>,
    ) -> Result<Self, RecordInputError> {
        use crate::{
            analysis::name_resolution::is_scope_visible_from,
            hir_def::{EnumVariant, FieldParent, scope_graph::ScopeId},
        };
        // Authorize raw declaration members before instantiating their types.
        // Hidden field metadata may itself require compile-time evaluation.
        let owner = Self::authored_owner(db, ty)?;
        let fields_visible = |parent, count| {
            (0..count).all(|index| {
                is_scope_visible_from(db, ScopeId::Field(parent, index as u16), requesting_scope)
            })
        };
        let visible = match owner {
            RecordOwner::Struct(declaration) => fields_visible(
                FieldParent::Struct(declaration),
                declaration.hir_fields(db).data(db).len(),
            ),
            RecordOwner::Enum(declaration) => declaration
                .variants_list(db)
                .data(db)
                .iter()
                .enumerate()
                .all(|(index, variant)| match variant.kind {
                    VariantKind::Record(fields) => fields_visible(
                        FieldParent::Variant(EnumVariant::new(declaration, index)),
                        fields.data(db).len(),
                    ),
                    // Tuple payloads have no individually authored field
                    // visibility, just like ordinary variant construction.
                    VariantKind::Unit | VariantKind::Tuple(_) => true,
                }),
        };
        if !visible {
            return Err(RecordInputError::InvisibleField);
        }
        Self::for_authored_adt_ty(db, ty)
    }

    fn authored_owner(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Result<RecordOwner<'db>, RecordInputError> {
        if !ty.is_star_kind(db) || ty.has_invalid(db) {
            return Err(RecordInputError::NotConcrete);
        }
        let adt = ty.adt_def(db).ok_or(RecordInputError::NotRecord)?;
        let owner = match adt.adt_ref(db) {
            crate::analysis::ty::adt_def::AdtRef::Struct(declaration) => {
                // The struct that `#[abi]`, `#[error]` or `#[event]` expands,
                // and the struct of a `msg` variant, are authored declarations
                // with the fields as written.
                if !declaration.origin(db).is_authored() {
                    return Err(RecordInputError::NotAuthored);
                }
                RecordOwner::Struct(declaration)
            }
            crate::analysis::ty::adt_def::AdtRef::Enum(declaration) => {
                if !matches!(declaration.origin(db), HirOrigin::Raw(_)) {
                    return Err(RecordInputError::NotAuthored);
                }
                RecordOwner::Enum(declaration)
            }
        };
        Ok(owner)
    }

    fn for_authored_adt_ty(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Result<Self, RecordInputError> {
        let owner = Self::authored_owner(db, ty)?;
        let adt = ty.adt_def(db).ok_or(RecordInputError::NotRecord)?;
        for (variant, fields) in adt.fields(db).iter().enumerate() {
            for field in 0..fields.num_types() {
                if crate::analysis::ty::adt_def::instantiate_adt_field_shape(
                    db,
                    adt,
                    variant,
                    field,
                    ty.generic_args(db),
                )
                .has_invalid(db)
                {
                    return Err(RecordInputError::InvalidField);
                }
            }
        }
        Ok(Self {
            ty,
            owner,
            variant_index: None,
        })
    }

    pub fn ty(self) -> TyId<'db> {
        self.ty
    }

    pub fn owner(self) -> RecordOwner<'db> {
        self.owner
    }

    pub fn variant_index(self) -> Option<usize> {
        self.variant_index
    }

    pub fn selected_variant(self) -> Option<usize> {
        self.variant_index
    }

    pub fn declaration(self) -> crate::analysis::ty::adt_def::AdtRef<'db> {
        match self.owner {
            RecordOwner::Struct(s) => crate::analysis::ty::adt_def::AdtRef::Struct(s),
            RecordOwner::Enum(e) => crate::analysis::ty::adt_def::AdtRef::Enum(e),
        }
    }

    pub fn scope(self) -> crate::hir_def::scope_graph::ScopeId<'db> {
        match self.owner {
            RecordOwner::Struct(s) => s.scope(),
            RecordOwner::Enum(e) => e.scope(),
        }
    }

    pub fn validate(self, db: &'db dyn HirAnalysisDb) -> Result<(), &'static str> {
        let base = Self::for_authored_adt_ty(db, self.ty)
            .map_err(|_| "record input no longer identifies its authored type")?;
        if base.owner != self.owner {
            return Err("record input owner differs from its authored type");
        }
        if let Some(variant) = self.variant_index {
            base.variant(db, variant)
                .map_err(|_| "record input variant is out of bounds")?;
        }
        Ok(())
    }

    /// Whole authored enum count; selected payloads are outside this checkpoint.
    pub fn variant_count(self, db: &'db dyn HirAnalysisDb) -> Result<usize, RecordInputError> {
        let RecordOwner::Enum(e) = self.owner else {
            return Err(RecordInputError::NotEnum);
        };
        if self.variant_index.is_some() {
            return Err(RecordInputError::SelectedPayloadIsNotWholeEnum);
        }
        Ok(e.variants_list(db).data(db).len())
    }

    /// Select an authored payload in declaration order; retain nominal owner.
    pub fn variant(
        self,
        db: &'db dyn HirAnalysisDb,
        index: usize,
    ) -> Result<Self, RecordInputError> {
        if index >= self.variant_count(db)? {
            return Err(RecordInputError::VariantOutsideEnum);
        }
        Ok(Self {
            ty: self.ty,
            owner: self.owner,
            variant_index: Some(index),
        })
    }

    pub fn field_count(self, db: &'db dyn HirAnalysisDb) -> Result<usize, RecordInputError> {
        match (self.owner, self.variant_index) {
            (RecordOwner::Struct(s), None) => Ok(s.hir_fields(db).data(db).len()),
            (RecordOwner::Enum(e), Some(v)) => Ok(match e.variants_list(db).data(db)[v].kind {
                VariantKind::Unit => 0,
                VariantKind::Tuple(t) => t.data(db).len(),
                VariantKind::Record(f) => f.data(db).len(),
            }),
            _ => Err(RecordInputError::WholeEnumHasNoFields),
        }
    }

    pub fn for_authored_enum_syntax(
        db: &'db dyn HirAnalysisDb,
        module: crate::hir_def::TopLevelMod<'db>,
        syntax: &ast::Enum,
    ) -> Result<Self, RecordInputError> {
        let origin = HirOrigin::raw(syntax);
        let declaration = module
            .all_enums(db)
            .iter()
            .find(|decl| *decl.origin(db) == origin)
            .ok_or(RecordInputError::NotAuthored)?;
        let adt = crate::analysis::ty::adt_def::AdtRef::Enum(*declaration).as_adt(db);
        let ty = TyId::foldl(db, TyId::adt(db, adt), adt.params(db));
        Self::for_authored_adt_ty(db, ty)
    }

    pub fn field(
        self,
        db: &'db dyn HirAnalysisDb,
        index: usize,
    ) -> Result<RecordField<'db>, RecordInputError> {
        if index >= self.field_count(db)? {
            return Err(RecordInputError::FieldOutsideRecord);
        }
        Ok(RecordField {
            record: self,
            index,
        })
    }

    pub fn freeze(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<common::generation::FrozenRecordRef, &'static str> {
        use common::generation::{FrozenRecordOwnerKind, FrozenRecordRef};
        let (owner_kind, top_mod, target_range) = match self.owner {
            RecordOwner::Struct(s) => {
                let top_mod = s.top_mod(db);
                let root = crate::lower::top_mod_ast(db, top_mod);
                let Some(range) = s.origin(db).authored_range(root.syntax()) else {
                    return Err("record input is not authored");
                };
                (FrozenRecordOwnerKind::Struct, top_mod, range)
            }
            RecordOwner::Enum(e) => {
                let top_mod = e.top_mod(db);
                let root = crate::lower::top_mod_ast(db, top_mod);
                let HirOrigin::Raw(ptr) = e.origin(db) else {
                    return Err("record input is not authored");
                };
                (
                    FrozenRecordOwnerKind::Enum,
                    top_mod,
                    ptr.to_node(root.syntax()).syntax().text_range(),
                )
            }
        };
        let target_file = top_mod
            .file(db)
            .url(db)
            .ok_or("record owner has no source URL")?;
        let adt = self
            .ty
            .adt_def(db)
            .ok_or("record input has no authored declaration")?;
        let identity = TyId::foldl(db, TyId::adt(db, adt), adt.params(db));
        let args = if self.ty == identity {
            None
        } else {
            Some(
                self.ty
                    .generic_args(db)
                    .iter()
                    .map(|arg| freeze_generic_arg(db, *arg))
                    .collect::<Result<Vec<_>, _>>()?,
            )
        };
        Ok(FrozenRecordRef {
            owner_kind,
            target_file,
            target_range,
            variant_index: self.variant_index,
            args,
        })
    }
}

impl<'db> RecordField<'db> {
    pub fn record(self) -> RecordInput<'db> {
        self.record
    }

    pub fn index(self) -> usize {
        self.index
    }

    pub fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        match (self.record.owner, self.record.variant_index) {
            (RecordOwner::Struct(s), None) => s.hir_fields(db).data(db)[self.index].name.to_opt(),
            (RecordOwner::Enum(e), Some(v)) => match e.variants_list(db).data(db)[v].kind {
                VariantKind::Record(f) => f.data(db)[self.index].name.to_opt(),
                VariantKind::Tuple(_) | VariantKind::Unit => None,
            },
            _ => None,
        }
    }

    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match (self.record.owner, self.record.variant_index) {
            (RecordOwner::Struct(_), None) => self.record.ty.field_types(db)[self.index],
            (RecordOwner::Enum(_), Some(v)) => {
                let adt = self.record.ty.adt_def(db).expect("enum ADT");
                // As for a struct field: an associated type is normalized, as
                // layouts see it.
                crate::analysis::ty::normalize::normalize_ty(
                    db,
                    crate::analysis::ty::adt_def::instantiate_adt_field_shape(
                        db,
                        adt,
                        v,
                        self.index,
                        self.record.ty.generic_args(db),
                    ),
                    adt.scope(db),
                    crate::analysis::ty::trait_resolution::PredicateListId::empty_list(db),
                )
            }
            _ => unreachable!("field belongs to struct or enum variant"),
        }
    }

    pub fn freeze(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<common::generation::FrozenFieldRef, &'static str> {
        Ok(common::generation::FrozenFieldRef {
            record: self.record.freeze(db)?,
            index: self.index,
        })
    }
}

/// Whether `ty` is ground except for parameters `header` itself declares.
pub(crate) fn ground_over_header<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    header: crate::hir_def::ItemKind<'db>,
) -> bool {
    use crate::analysis::ty::{const_ty::ConstTyData, ty_def::TyData};
    use crate::hir_def::scope_graph::ScopeId;
    let declared = |param: &crate::analysis::ty::ty_def::TyParam<'db>| {
        !param.is_implicit()
            && matches!(
                param.scope(db),
                ScopeId::GenericParam(owner, _) if owner == header
            )
    };
    match ty.data(db) {
        TyData::TyParam(param) => declared(param),
        TyData::TyApp(abs, arg) => {
            ground_over_header(db, *abs, header) && ground_over_header(db, *arg, header)
        }
        TyData::ConstTy(constant) => match constant.data(db) {
            ConstTyData::TyParam(param, _) => declared(param),
            _ => crate::analysis::ty::const_ty::ty_is_fully_ground(db, ty),
        },
        _ => crate::analysis::ty::const_ty::ty_is_fully_ground(db, ty),
    }
}

/// Freeze a type of the authored view by declaration identity, for rebinding
/// in the output view. Declarations cross as file and authored range,
/// primitives by name, constants as scalars and the derive header's own
/// parameters by position; no semantic identity of this database does.
pub(crate) fn freeze_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Result<common::generation::FrozenTy, &'static str> {
    use crate::analysis::ty::{
        provider::AddressSpace,
        ty_def::{PrimTy, TyBase, TyData},
    };
    use common::generation::{FrozenGenericArg, FrozenTy};
    match ty.data(db) {
        TyData::Never => return Ok(FrozenTy::Never),
        TyData::TyParam(param) => return header_param_index(db, param).map(FrozenTy::HeaderParam),
        _ => {}
    }
    let (base, args) = ty.decompose_ty_app(db);
    let args = args
        .iter()
        .map(|arg| freeze_generic_arg(db, *arg))
        .collect::<Result<Vec<_>, _>>()?;
    let unsupported = "a spliced type must be a record, primitive, tuple, array or pointer, applied to ground arguments or the derive header's parameters";
    let types = |args: Vec<FrozenGenericArg>| {
        args.into_iter()
            .map(|arg| match arg {
                FrozenGenericArg::Type(ty) => Ok(ty),
                FrozenGenericArg::Const(_) => Err(unsupported),
            })
            .collect::<Result<Vec<_>, _>>()
    };
    match base.data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => Ok(FrozenTy::Declared {
            decl: freeze_decl(db, adt.adt_ref(db))?,
            args,
        }),
        TyData::TyBase(TyBase::Prim(PrimTy::Tuple(_))) => Ok(FrozenTy::Tuple(types(args)?)),
        TyData::TyBase(TyBase::Prim(PrimTy::Array)) => match <[_; 2]>::try_from(args) {
            Ok([FrozenGenericArg::Type(elem), FrozenGenericArg::Const(len)]) => {
                Ok(FrozenTy::Array {
                    elem: Box::new(elem),
                    len,
                })
            }
            _ => Err(unsupported),
        },
        TyData::TyBase(TyBase::Prim(PrimTy::Ptr(AddressSpace::Memory))) => {
            match <[_; 1]>::try_from(types(args)?) {
                Ok([pointee]) => Ok(FrozenTy::Ptr(Box::new(pointee))),
                Err(_) => Err(unsupported),
            }
        }
        TyData::TyBase(TyBase::Prim(primitive)) => {
            let hir_primitive = crate::hir_def::prim_ty::PrimTy::all_types()
                .iter()
                .find(|candidate| {
                    crate::analysis::ty::ty_def::TyBase::from(**candidate)
                        == TyBase::Prim(*primitive)
                })
                .ok_or(unsupported)?;
            Ok(FrozenTy::Primitive {
                name: hir_primitive.name(db).data(db).to_string(),
                args,
            })
        }
        _ => Err(unsupported),
    }
}

fn freeze_generic_arg<'db>(
    db: &'db dyn HirAnalysisDb,
    arg: TyId<'db>,
) -> Result<common::generation::FrozenGenericArg, &'static str> {
    use crate::analysis::{
        semantic::SemConstValue,
        ty::{const_ty::ConstTyData, ty_def::TyData},
    };
    use common::generation::{FrozenConst, FrozenGenericArg};
    let TyData::ConstTy(constant) = arg.data(db) else {
        return Ok(FrozenGenericArg::Type(freeze_ty(db, arg)?));
    };
    if let ConstTyData::TyParam(param, _) = constant.data(db) {
        return Ok(FrozenGenericArg::Const(FrozenConst::HeaderParam(
            header_param_index(db, param)?,
        )));
    }
    let ConstTyData::Value(value) = constant.evaluate(db, None).data(db) else {
        return Err("a spliced type's constant argument must be evaluated");
    };
    let SemConstValue::Scalar { ty, value } = value.value().value(db) else {
        return Err("a spliced type's constant argument must be a boolean or integer");
    };
    Ok(FrozenGenericArg::Const(FrozenConst::Value(
        ItemCode::freeze_scalar(db, *ty, value)?,
    )))
}

/// The position of a parameter declared by an authored implementation header,
/// the only binders a frozen type may name: the request's own header.
fn header_param_index<'db>(
    db: &'db dyn HirAnalysisDb,
    param: &crate::analysis::ty::ty_def::TyParam<'db>,
) -> Result<usize, &'static str> {
    use crate::hir_def::{ItemKind, scope_graph::ScopeId};
    match param.scope(db) {
        ScopeId::GenericParam(ItemKind::ImplTrait(_) | ItemKind::Impl(_), index)
            if !param.is_implicit() =>
        {
            Ok(index as usize)
        }
        _ => Err("a spliced type may name only its derive header's own parameters"),
    }
}

fn freeze_decl<'db>(
    db: &'db dyn HirAnalysisDb,
    declaration: crate::analysis::ty::adt_def::AdtRef<'db>,
) -> Result<common::generation::FrozenDeclRef, &'static str> {
    use crate::analysis::ty::adt_def::AdtRef;
    let not_authored = "a spliced type names a declaration that is not authored";
    let (top_mod, range) = match declaration {
        AdtRef::Struct(s) => {
            let top_mod = s.top_mod(db);
            let root = crate::lower::top_mod_ast(db, top_mod);
            let range = s
                .origin(db)
                .authored_range(root.syntax())
                .ok_or(not_authored)?;
            (top_mod, range)
        }
        AdtRef::Enum(e) => match e.origin(db) {
            HirOrigin::Raw(pointer) => (e.top_mod(db), pointer.syntax_node_ptr().text_range()),
            _ => return Err(not_authored),
        },
    };
    Ok(common::generation::FrozenDeclRef {
        file: top_mod
            .file(db)
            .url(db)
            .ok_or("a spliced type's declaration has no source URL")?,
        range,
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct QuoteHoleNode {
    node: parser::SyntaxNode,
}

impl QuoteHoleNode {
    pub(crate) fn is_hole_kind(kind: parser::SyntaxKind) -> bool {
        use parser::SyntaxKind::*;
        matches!(
            kind,
            QuoteHole | QuoteHoleExpr | QuoteHolePat | QuoteHoleArm | QuoteHoleType
        )
    }

    pub(crate) fn syntax(&self) -> &parser::SyntaxNode {
        &self.node
    }

    pub(crate) fn value(&self) -> Option<ast::Expr> {
        self.node.children().find_map(ast::Expr::cast)
    }
}

pub const MAX_QUOTE_TEMPLATE_BYTES: usize = 4096;
pub const MAX_QUOTE_HOLES: usize = 64;
pub const MAX_GENERATED_AST_NODES: usize = 4096;

/// Take `cost` nodes from the remaining expansion budget of a generated code
/// tree.
fn charge_expanded_nodes(remaining_nodes: &mut usize, cost: usize) -> Result<(), &'static str> {
    if cost > *remaining_nodes {
        return Err("generated code tree exceeds the expanded AST node limit of 4096");
    }
    *remaining_nodes -= cost;
    Ok(())
}

/// The patterns a binder pattern is copied into: one pattern for the payload
/// and one binding for each of its fields. A whole enum has no payload to
/// bind; lowering refuses it, so it expands to the one refused pattern.
fn binder_pattern_nodes<'db>(db: &'db dyn HirAnalysisDb, record: RecordInput<'db>) -> usize {
    1 + record.field_count(db).unwrap_or(0)
}

fn producer_block_range(quote: &ast::QuoteExpr) -> Option<parser::TextRange> {
    let block = quote.syntax().ancestors().find_map(ast::BlockExpr::cast)?;
    let block_node = block
        .syntax()
        .parent()
        .filter(|parent| ast::UnsafeExpr::can_cast(parent.kind()))
        .unwrap_or_else(|| block.syntax().clone());
    Some(block_node.text_range())
}

fn is_all_code_hole_block<'db>(
    db: &'db dyn HirAnalysisDb,
    quote: &ast::QuoteExpr,
    holes: &[crate::analysis::semantic::SemConstId<'db>],
    expected_kind: common::generation::FrozenCodeKind,
) -> bool {
    let Some(block) = quote.body() else {
        return false;
    };
    let stmts = block.stmts().collect::<Vec<_>>();
    !stmts.is_empty()
        && block.items().next().is_none()
        && stmts.len() == holes.len()
        && stmts.iter().all(|stmt| {
            matches!(
                stmt.kind(),
                ast::StmtKind::Expr(expr)
                    if expr
                        .expr()
                        .is_some_and(|e| matches!(e.kind(), ast::ExprKind::QuoteHole(_)))
            )
        })
        && holes.iter().all(|hole| {
            matches!(
                hole.value(db),
                crate::analysis::semantic::SemConstValue::ItemCode { code, .. }
                    if code.kind() == expected_kind
            )
        })
}

/// One bounded source walk shared by producer lowering, construction and
/// output copying. Unsupported nested constructors never lower missing holes.
pub(crate) fn quote_holes(quote: &ast::QuoteExpr) -> Result<Vec<QuoteHoleNode>, &'static str> {
    let mut holes = Vec::new();
    for (index, node) in quote
        .syntax()
        .descendants()
        .take(MAX_GENERATED_AST_NODES + 1)
        .enumerate()
    {
        if index == MAX_GENERATED_AST_NODES {
            return Err("item quotation exceeds the construction node limit of 4096");
        }
        if ast::QuoteExpr::can_cast(node.kind()) && node != *quote.syntax() {
            return Err(
                "nested quote expressions are not supported by this bounded item quotation",
            );
        }
        if QuoteHoleNode::is_hole_kind(node.kind()) {
            if node
                .ancestors()
                .skip(1)
                .take_while(|ancestor| ancestor != quote.syntax())
                .any(|ancestor| QuoteHoleNode::is_hole_kind(ancestor.kind()))
            {
                return Err("nested quotation holes are not supported");
            }
            if holes.len() == MAX_QUOTE_HOLES {
                return Err("item quotation exceeds the hole limit of 64");
            }
            holes.push(QuoteHoleNode { node });
        }
    }
    if usize::from(quote.syntax().text_range().len()) > MAX_QUOTE_TEMPLATE_BYTES {
        return Err("item quotation exceeds the template byte limit of 4096");
    }
    Ok(holes)
}

pub(crate) fn quote_item_declarations(
    quote: &ast::QuoteExpr,
) -> Result<Vec<ast::Item>, &'static str> {
    if let Some(items) = quote.item_list() {
        // A shape is a member, never one of these declarations: not dropped.
        if items
            .syntax()
            .children()
            .any(|child| ast::TraitShapeItem::can_cast(child.kind()))
        {
            return Err("a quoted shape is an associated shape member of an implementation");
        }
        return Ok(items.into_iter().collect());
    }
    if let Some(block) = quote.body() {
        if block.stmts().next().is_some() {
            return Err("item quotation requires item declarations");
        }
        return Ok(block.items().collect());
    }
    Err("item quotation requires item declarations")
}

pub fn code_kind_from_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<common::generation::FrozenCodeKind> {
    use common::generation::FrozenCodeKind;
    let ty = ty.as_view(db).unwrap_or(ty);
    let (base, args) = ty.decompose_ty_app(db);
    let crate::analysis::ty::ty_def::TyData::TyBase(crate::analysis::ty::ty_def::TyBase::Adt(adt)) =
        base.data(db)
    else {
        return None;
    };
    let scope = adt.scope(db);
    if crate::analysis::ty::corelib::resolve_lib_type_path(db, scope, "core::meta::Code")
        != Some(base)
        || args.len() != 1
    {
        return None;
    }
    let marker = args[0].as_view(db).unwrap_or(args[0]);
    for (path, kind) in [
        ("core::meta::Item", FrozenCodeKind::Item),
        ("core::meta::Expr", FrozenCodeKind::Expr),
        ("core::meta::Pat", FrozenCodeKind::Pat),
        ("core::meta::Arm", FrozenCodeKind::Arm),
        ("core::meta::Where", FrozenCodeKind::Where),
    ] {
        if crate::analysis::ty::corelib::resolve_lib_type_path(db, scope, path) == Some(marker) {
            return Some(kind);
        }
    }
    None
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum CodeData<'db> {
    Template {
        kind: common::generation::FrozenCodeKind,
        key: crate::analysis::semantic::SemanticInstanceKey<'db>,
        expression: crate::hir_def::ExprId,
        holes: Box<[crate::analysis::semantic::SemConstId<'db>]>,
    },
    Empty {
        kind: common::generation::FrozenCodeKind,
    },
    VariantBinders {
        record: RecordInput<'db>,
        group: usize,
    },
    VariantBinderField {
        record: RecordInput<'db>,
        group: usize,
        field_index: usize,
    },
}

/// A checked code template or hygienic binder fragment in the existing producer
/// instance, with completed splice values. No generated declaration has been published yet.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ItemCode<'db> {
    data: CodeData<'db>,
}

impl<'db> ItemCode<'db> {
    pub fn empty(kind: common::generation::FrozenCodeKind) -> Self {
        Self {
            data: CodeData::Empty { kind },
        }
    }

    pub fn variant_binders(record: RecordInput<'db>, group: usize) -> Self {
        Self {
            data: CodeData::VariantBinders { record, group },
        }
    }

    pub fn variant_binder_field(
        record: RecordInput<'db>,
        group: usize,
        field_index: usize,
    ) -> Self {
        Self {
            data: CodeData::VariantBinderField {
                record,
                group,
                field_index,
            },
        }
    }

    pub fn data(&self) -> &CodeData<'db> {
        &self.data
    }

    pub fn kind(&self) -> common::generation::FrozenCodeKind {
        use common::generation::FrozenCodeKind;
        match &self.data {
            CodeData::Template { kind, .. } | CodeData::Empty { kind } => *kind,
            CodeData::VariantBinders { .. } => FrozenCodeKind::Pat,
            CodeData::VariantBinderField { .. } => FrozenCodeKind::Expr,
        }
    }

    pub fn for_quote(
        db: &'db dyn HirAnalysisDb,
        key: crate::analysis::semantic::SemanticInstanceKey<'db>,
        expression: crate::hir_def::ExprId,
        holes: Box<[crate::analysis::semantic::SemConstId<'db>]>,
    ) -> Result<Self, &'static str> {
        Self::for_quote_with_kind(
            db,
            common::generation::FrozenCodeKind::Item,
            key,
            expression,
            holes,
        )
    }

    pub(crate) fn for_quote_with_kind(
        db: &'db dyn HirAnalysisDb,
        kind: common::generation::FrozenCodeKind,
        key: crate::analysis::semantic::SemanticInstanceKey<'db>,
        expression: crate::hir_def::ExprId,
        holes: Box<[crate::analysis::semantic::SemConstId<'db>]>,
    ) -> Result<Self, &'static str> {
        let value = Self {
            data: CodeData::Template {
                kind,
                key,
                expression,
                holes,
            },
        };
        value.validate(db)?;
        Ok(value)
    }

    pub fn producer(&self) -> crate::analysis::semantic::SemanticInstanceKey<'db> {
        match &self.data {
            CodeData::Template { key, .. } => *key,
            _ => panic!("not a template quote"),
        }
    }

    pub fn holes(&self) -> &[crate::analysis::semantic::SemConstId<'db>] {
        match &self.data {
            CodeData::Template { holes, .. } => holes,
            _ => &[],
        }
    }

    fn freeze_scalar(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        scalar: &crate::analysis::semantic::SemConstScalar,
    ) -> Result<common::generation::FrozenScalar, &'static str> {
        use crate::analysis::semantic::SemConstScalar;
        use common::generation::FrozenScalar;
        match scalar {
            SemConstScalar::Bool(value) => Ok(FrozenScalar::Bool(*value)),
            SemConstScalar::Int { value } => {
                let crate::analysis::ty::ty_def::TyData::TyBase(
                    crate::analysis::ty::ty_def::TyBase::Prim(primitive),
                ) = ty.data(db)
                else {
                    return Err("integer splice must have a completed primitive type");
                };
                if !primitive.is_integral() {
                    return Err("integer splice must have a completed primitive type");
                }
                let hir_primitive = crate::hir_def::prim_ty::PrimTy::all_types()
                    .iter()
                    .find(|candidate| {
                        crate::analysis::ty::ty_def::TyBase::from(**candidate)
                            == crate::analysis::ty::ty_def::TyBase::Prim(*primitive)
                    })
                    .expect("integral semantic primitive has a HIR primitive");
                let primitive = hir_primitive.name(db).data(db).to_string();
                let (sign, bytes) = value.to_bytes_be();
                Ok(FrozenScalar::Int {
                    negative: sign == num_bigint::Sign::Minus,
                    bytes,
                    primitive,
                })
            }
            SemConstScalar::Bytes(_) => {
                Err("bounded output copying supports boolean and integer splices")
            }
        }
    }

    /// Freeze this bounded same-file template for an output database. Scalar
    /// values retain their primitive type; source remains the authored AST.
    pub fn freeze_function(
        &self,
        db: &'db dyn HirAnalysisDb,
        target: crate::File,
    ) -> Result<common::generation::FrozenFunctionTemplate, &'static str> {
        use crate::analysis::semantic::SemConstValue;
        use common::generation::FrozenFunctionTemplate;
        self.validate(db)?;
        let CodeData::Template { key, holes, .. } = &self.data else {
            return Err("bounded output copying requires a template quote");
        };
        let body = key.owner(db).body(db).ok_or("quote producer has no body")?;
        if body.top_mod(db).file(db) != target {
            return Err("bounded generation requires quote and target in the same source file");
        }
        let quote = self.syntax(db)?;
        let items = quote_item_declarations(&quote)?;
        if items.len() != 1 {
            return Err("this bounded item quotation requires one function declaration");
        }
        let Some(ast::ItemKind::Func(function)) = items[0].kind() else {
            return Err("this bounded item quotation requires one function declaration");
        };
        let signature = function.sig();
        if signature
            .params()
            .is_some_and(|params| params.into_iter().next().is_some())
            || signature.generic_params().is_some()
            || signature.where_clause().is_some()
            || signature.uses_clause().is_some()
        {
            return Err(
                "bounded output copying requires a zero-argument nongeneric function without requirements or effects",
            );
        }
        let producer_block = producer_block_range(&quote);
        let producer_module = quote
            .syntax()
            .ancestors()
            .find_map(ast::Mod::cast)
            .map(|module| module.syntax().text_range());
        let holes = holes
            .iter()
            .map(|value| match value.value(db) {
                SemConstValue::Scalar { ty, value } => Self::freeze_scalar(db, *ty, value),
                _ => Err("bounded output copying supports boolean and integer splices"),
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(FrozenFunctionTemplate {
            target_file: target
                .url(db)
                .ok_or("generation target has no source URL")?,
            quote_range: quote.syntax().text_range(),
            producer_module,
            producer_block,
            holes,
        })
    }

    pub fn freeze(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<common::generation::FrozenCode, &'static str> {
        self.validate(db)?;
        let mut remaining_nodes = MAX_GENERATED_AST_NODES;
        self.freeze_with_budget(db, &mut remaining_nodes)
    }

    fn freeze_with_budget(
        &self,
        db: &'db dyn HirAnalysisDb,
        remaining_nodes: &mut usize,
    ) -> Result<common::generation::FrozenCode, &'static str> {
        use crate::analysis::semantic::SemConstValue;
        use common::generation::{FrozenCode, FrozenHoleValue};
        match &self.data {
            CodeData::Empty { kind } => Ok(FrozenCode::Empty { kind: *kind }),
            CodeData::VariantBinders { record, group } => {
                charge_expanded_nodes(remaining_nodes, binder_pattern_nodes(db, *record))?;
                Ok(FrozenCode::VariantBinders {
                    record: record.freeze(db)?,
                    group: *group,
                })
            }
            CodeData::VariantBinderField {
                record,
                group,
                field_index,
            } => {
                charge_expanded_nodes(remaining_nodes, 1)?;
                Ok(FrozenCode::VariantBinderField {
                    record: record.freeze(db)?,
                    group: *group,
                    field_index: *field_index,
                })
            }
            CodeData::Template {
                kind, key, holes, ..
            } => {
                let body = key.owner(db).body(db).ok_or("quote producer has no body")?;
                let quote_file = body
                    .top_mod(db)
                    .file(db)
                    .url(db)
                    .ok_or("quote producer has no source URL")?;
                let quote = self.syntax(db)?;
                let node_count = quote
                    .syntax()
                    .descendants()
                    .take(MAX_GENERATED_AST_NODES + 1)
                    .count();
                if node_count > *remaining_nodes {
                    return Err("generated code tree exceeds the expanded AST node limit of 4096");
                }
                *remaining_nodes -= node_count;
                let producer_block = producer_block_range(&quote);
                let producer_module = quote
                    .syntax()
                    .ancestors()
                    .find_map(ast::Mod::cast)
                    .map(|module| module.syntax().text_range());
                let holes = holes
                    .iter()
                    .map(|value| match value.value(db) {
                        SemConstValue::Scalar { ty, value } => Ok(FrozenHoleValue::Scalar(
                            Self::freeze_scalar(db, *ty, value)?,
                        )),
                        SemConstValue::Meta { value, .. } => match value {
                            MetaValue::Record(record) => {
                                Ok(FrozenHoleValue::Record(record.freeze(db)?))
                            }
                            MetaValue::Field(field) => {
                                Ok(FrozenHoleValue::Field(field.freeze(db)?))
                            }
                            MetaValue::TypeRef { ty, .. } => {
                                Ok(FrozenHoleValue::Type(freeze_ty(db, *ty)?))
                            }
                            MetaValue::FieldType(field) => {
                                let record = field.record();
                                let adt = record.ty().adt_def(db)
                                    .ok_or("field type splice has no authored record")?;
                                let identity = RecordInput {
                                    ty: TyId::foldl(db, TyId::adt(db, adt), adt.params(db)),
                                    ..record
                                };
                                let authored_field = RecordField { record: identity, index: field.index() };
                                // A type written without the record's parameters keeps its
                                // written form; one over them is the instantiated type.
                                if field.ty(db) != authored_field.ty(db) {
                                    return Ok(FrozenHoleValue::Type(freeze_ty(
                                        db,
                                        crate::analysis::ty::normalize::normalize_ty(
                                            db,
                                            field.ty(db),
                                            record.scope(),
                                            crate::analysis::ty::trait_resolution::PredicateListId::empty_list(db),
                                        ),
                                    )?));
                                }
                                Ok(FrozenHoleValue::FieldType(field.freeze(db)?))
                            }
                        },
                        SemConstValue::ItemCode { code, .. } => Ok(FrozenHoleValue::Code(
                            code.freeze_with_budget(db, remaining_nodes)?,
                        )),
                        _ => Err("unsupported quotation splice value"),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(FrozenCode::Template {
                    kind: *kind,
                    quote_file,
                    quote_range: quote.syntax().text_range(),
                    producer_module,
                    producer_block,
                    holes,
                })
            }
        }
    }

    pub fn freeze_request(
        &self,
        db: &'db dyn HirAnalysisDb,
        target: crate::File,
    ) -> Result<common::generation::FrozenGenerationRequest, &'static str> {
        if self.kind() != common::generation::FrozenCodeKind::Item {
            return Err("generation request requires Code<Item>");
        }
        Ok(common::generation::FrozenGenerationRequest {
            scoped_impl: None,
            target_file: target
                .url(db)
                .ok_or("generation target has no source URL")?,
            request_impl: None,
            code: self.freeze(db)?,
        })
    }

    pub fn syntax(&self, db: &'db dyn HirAnalysisDb) -> Result<ast::QuoteExpr, &'static str> {
        let CodeData::Template {
            key, expression, ..
        } = &self.data
        else {
            return Err("code value has no authored quote syntax");
        };
        let body = key.owner(db).body(db).ok_or("quote producer has no body")?;
        let HirOrigin::Raw(ptr) = body.source_map(db).expr_map.node_to_source(*expression) else {
            return Err("item code must retain its authored quote origin");
        };
        let root = crate::lower::top_mod_ast(db, body.top_mod(db));
        let ast::ExprKind::Quote(quote) = ptr.to_node(root.syntax()).kind() else {
            return Err("item code source is not a quotation");
        };
        Ok(quote)
    }

    pub(crate) fn validate(&self, db: &'db dyn HirAnalysisDb) -> Result<(), &'static str> {
        let mut remaining_nodes = MAX_GENERATED_AST_NODES;
        self.validate_with_budget(db, &mut remaining_nodes)
    }

    fn validate_with_budget(
        &self,
        db: &'db dyn HirAnalysisDb,
        remaining_nodes: &mut usize,
    ) -> Result<(), &'static str> {
        use crate::analysis::semantic::SemConstValue;
        use common::generation::FrozenCodeKind;
        match &self.data {
            CodeData::Empty { .. } => return Ok(()),
            CodeData::VariantBinders { record, .. } => {
                return charge_expanded_nodes(remaining_nodes, binder_pattern_nodes(db, *record));
            }
            CodeData::VariantBinderField { .. } => {
                return charge_expanded_nodes(remaining_nodes, 1);
            }
            CodeData::Template { .. } => {}
        }
        let CodeData::Template {
            kind,
            key,
            expression,
            holes,
        } = &self.data
        else {
            unreachable!();
        };
        let body = key.owner(db).body(db).ok_or("quote producer has no body")?;
        let crate::hir_def::Partial::Present(crate::hir_def::Expr::ItemQuote(source_holes)) =
            expression.data(db, body)
        else {
            return Err("item code does not identify a checked quote expression");
        };
        if source_holes.len() != holes.len() {
            return Err("item code splice arity differs from its source");
        }
        let quote = self.syntax(db)?;
        let syntax_holes = quote_holes(&quote)?;
        if syntax_holes.len() != holes.len() {
            return Err("item code splice arity differs from its source");
        }
        let node_count = quote
            .syntax()
            .descendants()
            .take(MAX_GENERATED_AST_NODES + 1)
            .count();
        if node_count > *remaining_nodes {
            return Err("generated code tree exceeds the expanded AST node limit of 4096");
        }
        *remaining_nodes -= node_count;

        let all_code_hole_block = matches!(
            *kind,
            FrozenCodeKind::Item | FrozenCodeKind::Arm | FrozenCodeKind::Where
        ) && is_all_code_hole_block(db, &quote, holes, *kind);

        for (syntax_hole, value) in syntax_holes.iter().zip(holes.iter()) {
            crate::analysis::semantic::consts::verify_sem_const_shape(db, *value)?;
            if !all_code_hole_block {
                match syntax_hole.syntax().kind() {
                    parser::SyntaxKind::QuoteHoleType => match value.value(db) {
                        SemConstValue::Meta {
                            value: MetaValue::Record(record),
                            ..
                        } if record.variant_index().is_none() => {}
                        SemConstValue::Meta {
                            value: MetaValue::FieldType(_) | MetaValue::TypeRef { .. },
                            ..
                        } => {}
                        _ => {
                            return Err(
                                "type quotation hole requires a record, field type, or checked type reference",
                            );
                        }
                    },
                    parser::SyntaxKind::QuoteHolePat => match value.value(db) {
                        SemConstValue::Scalar { .. } => {}
                        SemConstValue::ItemCode { code, .. }
                            if code.kind() == FrozenCodeKind::Pat => {}
                        _ => {
                            return Err(
                                "pattern quotation hole requires Code<Pat> or scalar splice",
                            );
                        }
                    },
                    parser::SyntaxKind::QuoteHoleArm => match value.value(db) {
                        SemConstValue::ItemCode { code, .. }
                            if code.kind() == FrozenCodeKind::Arm => {}
                        _ => return Err("match-arm quotation hole requires Code<Arm> splice"),
                    },
                    parser::SyntaxKind::QuoteHole => {
                        let parent_kind = syntax_hole.syntax().parent().map(|p| p.kind());
                        match parent_kind {
                            Some(parser::SyntaxKind::WhereClause) => match value.value(db) {
                                SemConstValue::ItemCode { code, .. }
                                    if code.kind() == FrozenCodeKind::Where => {}
                                _ => {
                                    return Err(
                                        "where-clause quotation hole requires Code<Where> splice",
                                    );
                                }
                            },
                            Some(
                                parser::SyntaxKind::QuoteItemList
                                | parser::SyntaxKind::ImplItemList
                                | parser::SyntaxKind::TraitItemList,
                            ) => match value.value(db) {
                                SemConstValue::ItemCode { code, .. }
                                    if code.kind() == FrozenCodeKind::Item => {}
                                _ => {
                                    return Err("item quotation hole requires Code<Item> splice");
                                }
                            },
                            _ => match value.value(db) {
                                SemConstValue::Meta {
                                    value: MetaValue::Field(_),
                                    ..
                                } => {}
                                _ => {
                                    return Err(
                                        "field or member quotation hole requires Field<T, F> splice",
                                    );
                                }
                            },
                        }
                    }
                    parser::SyntaxKind::QuoteHoleExpr => {
                        let is_quote_body_stmt = syntax_hole
                            .syntax()
                            .parent()
                            .and_then(|p| p.parent())
                            .is_some_and(|gp| {
                                gp.kind() == parser::SyntaxKind::BlockExpr
                                    && gp.parent().as_ref() == Some(quote.syntax())
                            });
                        let is_multi_entry_block_stmt = syntax_hole
                            .syntax()
                            .parent()
                            .and_then(|p| p.parent())
                            .and_then(ast::BlockExpr::cast)
                            .is_some_and(|b| {
                                b.stmts().nth(1).is_some() || b.items().next().is_some()
                            });
                        if is_quote_body_stmt
                            && matches!(
                                *kind,
                                FrozenCodeKind::Item | FrozenCodeKind::Arm | FrozenCodeKind::Where
                            )
                        {
                            match (*kind, value.value(db)) {
                                (FrozenCodeKind::Item, SemConstValue::ItemCode { code, .. })
                                    if code.kind() == FrozenCodeKind::Item => {}
                                (FrozenCodeKind::Item, _) => {
                                    return Err("item quotation hole requires Code<Item> splice");
                                }
                                (FrozenCodeKind::Arm, SemConstValue::ItemCode { code, .. })
                                    if code.kind() == FrozenCodeKind::Arm => {}
                                (FrozenCodeKind::Arm, _) => {
                                    return Err(
                                        "match-arm quotation hole requires Code<Arm> splice",
                                    );
                                }
                                (FrozenCodeKind::Where, SemConstValue::ItemCode { code, .. })
                                    if code.kind() == FrozenCodeKind::Where => {}
                                _ => {
                                    return Err(
                                        "where-clause quotation hole requires Code<Where> splice",
                                    );
                                }
                            }
                        } else if *kind == FrozenCodeKind::Pat {
                            match value.value(db) {
                                SemConstValue::Scalar { .. } => {}
                                SemConstValue::ItemCode { code, .. }
                                    if code.kind() == FrozenCodeKind::Pat => {}
                                _ => {
                                    return Err(
                                        "pattern quotation hole requires Code<Pat> or scalar splice",
                                    );
                                }
                            }
                        } else {
                            match value.value(db) {
                                SemConstValue::Scalar { .. }
                                | SemConstValue::Meta {
                                    value: MetaValue::Field(_),
                                    ..
                                } => {}
                                SemConstValue::ItemCode { code, .. }
                                    if code.kind() == FrozenCodeKind::Expr
                                        || (is_multi_entry_block_stmt
                                            && code.kind() == FrozenCodeKind::Item) => {}
                                _ => {
                                    return Err(
                                        "expression quotation hole requires Code<Expr>, Field<T, F>, or scalar splice",
                                    );
                                }
                            }
                        }
                    }
                    _ => return Err("unsupported quotation hole syntax"),
                }
            }

            match value.value(db) {
                SemConstValue::Scalar { .. } | SemConstValue::Meta { .. } => {}
                SemConstValue::ItemCode { code, .. } => {
                    code.validate_with_budget(db, remaining_nodes)?;
                }
                _ => {
                    return Err(
                        "this bounded item quotation requires scalar or metadata splice values",
                    );
                }
            }
        }
        match *kind {
            FrozenCodeKind::Item => {
                if let Some(items) = quote.item_list() {
                    for item in items {
                        if !matches!(
                            item.kind(),
                            Some(
                                ast::ItemKind::Func(_)
                                    | ast::ItemKind::Const(_)
                                    | ast::ItemKind::TypeAlias(_)
                                    | ast::ItemKind::Impl(_)
                                    | ast::ItemKind::ImplTrait(_)
                            )
                        ) {
                            return Err(
                                "item quotation supports function, const, type, impl, and trait impl declarations",
                            );
                        }
                    }
                    return Ok(());
                }
                if all_code_hole_block {
                    return Ok(());
                }
                let block = quote
                    .body()
                    .ok_or("item quotation requires item declarations")?;
                if block.stmts().next().is_some() {
                    return Err("item quotation requires item declarations");
                }
                // An empty quotation is no declarations at all: the output of
                // a provider for a marker trait, which has no members.
                for item in block.items() {
                    if !matches!(
                        item.kind(),
                        Some(
                            ast::ItemKind::Func(_)
                                | ast::ItemKind::Const(_)
                                | ast::ItemKind::TypeAlias(_)
                                | ast::ItemKind::Impl(_)
                                | ast::ItemKind::ImplTrait(_)
                        )
                    ) {
                        return Err(
                            "item quotation supports function, const, type, impl, and trait impl declarations",
                        );
                    }
                }
            }
            FrozenCodeKind::Arm => {
                if quote.arm_list().is_none() && !all_code_hole_block {
                    return Err("match-arm quotation requires match arms");
                }
            }
            FrozenCodeKind::Where => {
                if quote.where_clause().is_none() && !all_code_hole_block {
                    return Err("where-clause quotation requires where predicates");
                }
            }
            FrozenCodeKind::Expr => {
                if quote.item_list().is_some()
                    || quote.arm_list().is_some()
                    || quote.where_clause().is_some()
                    || quote.body().is_none()
                {
                    return Err("expression quotation requires a block expression body");
                }
            }
            FrozenCodeKind::Pat => {
                let valid_pat_block = quote.body().is_some_and(|block| {
                    block.items().next().is_none() && block.stmts().count() == 1
                });
                if quote.pattern().is_none() && !valid_pat_block {
                    return Err("pattern quotation requires a pattern");
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum NativeDeriveProviderError<'db> {
    Message(String),
    Target(crate::analysis::ty::ty_check::GroundTypeAdmissionError<'db>),
    Provider(crate::analysis::ty::ty_check::GroundTypeAdmissionError<'db>),
}
impl From<String> for NativeDeriveProviderError<'_> {
    fn from(message: String) -> Self {
        Self::Message(message)
    }
}
impl From<&str> for NativeDeriveProviderError<'_> {
    fn from(message: &str) -> Self {
        Self::Message(message.into())
    }
}

/// Explicit bounded Derive request through the ordinary trait selection path.
pub fn native_derive_provider<'db>(
    db: &'db dyn HirAnalysisDb,
    request: crate::hir_def::ImplTrait<'db>,
    provider_ty: TyId<'db>,
) -> Result<
    (
        crate::analysis::semantic::SemanticInstanceKey<'db>,
        RecordInput<'db>,
    ),
    NativeDeriveProviderError<'db>,
> {
    // A header may declare binders and requirements, as a handwritten one
    // does; its target is then admitted under the header's own premises.
    let goal = request.candidate_derive_trait_inst(db).map_err(|_| {
        if matches!(
            request.ty(db).data(db),
            crate::analysis::ty::ty_def::TyData::TyParam(_)
        ) {
            "a derive header cannot implement a trait for a bare type parameter: `impl<T> Trait for T` applies to every type that meets its bounds, and a provider derives members from one record; write this implementation by hand"
        } else {
            "cannot resolve the requested trait and target: the trait must be in scope, and a header cannot implement a trait of another package for a type of another package"
        }
    })?;
    finalized_derive_provider(
        db,
        DeriveKey::Trait(goal),
        provider_ty,
        request.into(),
        request.scope(),
        Some(request.into()),
        (request.span().ty().into(), request.span().into()),
    )
}

/// What a provider is asked for, which keys the `Derive` implementation it
/// needs: the members of an implementation of a trait (`Derive<trait Eq>`), or the
/// inherent members that an inherent header asks for (`DeriveInherent`).
#[derive(Clone, Copy)]
enum DeriveKey<'db> {
    Trait(crate::analysis::ty::trait_def::TraitInstId<'db>),
    Inherent(crate::hir_def::Impl<'db>),
}

impl<'db> DeriveKey<'db> {
    /// The record the provider reads: the trait's self type, or the type of
    /// the inherent header.
    fn target(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match self {
            Self::Trait(goal) => goal.args(db)[0],
            Self::Inherent(request) => request.ty(db),
        }
    }
}

/// Validate a concrete request and finalize its ordinary provider call once.
/// `key` says what the provider is asked for, and so which record it reads.
fn finalized_derive_provider<'db>(
    db: &'db dyn HirAnalysisDb,
    key: DeriveKey<'db>,
    provider_ty: TyId<'db>,
    origin: crate::hir_def::ItemKind<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    header: Option<crate::hir_def::ItemKind<'db>>,
    spans: (crate::span::DynLazySpan<'db>, crate::span::DynLazySpan<'db>),
) -> Result<
    (
        crate::analysis::semantic::SemanticInstanceKey<'db>,
        RecordInput<'db>,
    ),
    NativeDeriveProviderError<'db>,
> {
    use crate::analysis::ty::{
        corelib::resolve_core_trait, trait_def::TraitInstId, trait_resolution::PredicateListId,
    };
    let target = key.target(db);
    let requested_by_header = header.is_some();
    let assumptions = header.map_or_else(
        || PredicateListId::empty_list(db),
        |header| crate::core::semantic::param_env(db, header),
    );
    // A target over the header's own binders is issued and admitted under
    // the header's premises; every other target must be ground.
    let header = header.filter(|_| !crate::analysis::ty::const_ty::ty_is_fully_ground(db, target));
    let input = match header {
        Some(header) => RecordInput::for_header_type(db, target, header),
        None => RecordInput::for_authored_type_from(db, target, scope),
    }
    .map_err(|cause| match cause {
        RecordInputError::InvisibleField => {
            "a derive header requires every field of its target to be visible where the header is written"
                .to_string()
        }
        RecordInputError::NotAuthored => format!(
            "{} cannot be provided for `{}`: it is not a struct or enum written in source, so a provider has no record to read; write the implementation by hand",
            if requested_by_header {
                "a derive header"
            } else {
                "a derived implementation"
            },
            target.pretty_print(db),
        ),
        _ => format!("a derive header requires a concrete record: {cause:?}"),
    })?;
    match header {
        Some(header) => crate::analysis::ty::ty_check::admit_header_type_application(
            db,
            input.ty(),
            header,
            spans.0.clone(),
        ),
        None => crate::analysis::ty::ty_check::admit_ground_type_application(
            db,
            input.ty(),
            origin,
            spans.0.clone(),
        ),
    }
    .map_err(NativeDeriveProviderError::Target)?;
    if provider_ty.has_param(db) || provider_ty.has_invalid(db) || !provider_ty.is_star_kind(db) {
        return Err("a derive header requires a concrete provider type".into());
    }
    crate::analysis::ty::ty_check::admit_ground_type_application(
        db,
        provider_ty,
        origin,
        spans.1.clone(),
    )
    .map_err(NativeDeriveProviderError::Provider)?;
    let derive = match key {
        DeriveKey::Trait(_) => resolve_core_trait(db, scope, &["derive", "Derive"]),
        DeriveKey::Inherent(_) => resolve_core_trait(db, scope, &["derive", "DeriveInherent"]),
    }
    .ok_or("missing canonical derive protocol")?;
    let args = match key {
        DeriveKey::Trait(goal) => {
            let family = crate::analysis::ty::trait_resolution::derive::select_family(
                db,
                scope,
                assumptions,
                derive,
                provider_ty,
                goal,
            )?;
            vec![provider_ty, family]
        }
        DeriveKey::Inherent(_) => vec![provider_ty],
    };
    let inst = TraitInstId::new_simple(db, derive, args);
    let derive_method = derive
        .method_defs(db)
        .get(&IdentId::new(db, "derive"))
        .copied()
        .ok_or("canonical Derive protocol has no derive method")?;
    let finalized = crate::analysis::semantic::generated_callee(
        db,
        scope,
        assumptions,
        derive_method,
        Some(inst),
        &[],
    )
    .map_err(|cause| match (key, cause) {
        (DeriveKey::Inherent(request), _) => {
            let header_ty = request
                .type_ref(db)
                .to_opt()
                .map(|ty| ty.pretty_print(db))
                .unwrap_or_else(|| target.pretty_print(db).to_string());
            let provider_path = request
                .derive_provider(db)
                .map(|path| path.pretty_print(db))
                .unwrap_or_else(|| provider_ty.pretty_print(db).to_string());
            format!(
                "`{}` has no `impl DeriveInherent`: the inherent header `impl {header_ty} with ({provider_path})` needs a provider that implements `DeriveInherent`",
                provider_ty.pretty_print(db),
            )
        }
        (
            DeriveKey::Trait(_),
            crate::analysis::ty::trait_def::MethodArgMapError::MissingBody,
        ) => {
            let family = inst.args(db)[1].pretty_print(db);
            format!(
                "`{provider}` has no `impl Derive<{family}>` that applies",
                provider = provider_ty.pretty_print(db),
            )
        }
        (DeriveKey::Trait(_), cause) => format!(
            "no ordinary Derive<{}> provider implementation was admitted: {cause:?}",
            inst.args(db)[1].pretty_print(db),
        ),
    })?;
    let proof = finalized
        .evidence
        .ok_or("provider application has no retained proof")?;
    crate::analysis::ty::ty_check::retained_bound_requirements(
        db,
        proof,
        inst,
        scope,
        spans.1.clone(),
        requested_by_header,
    )
    .map_err(|(cause, diagnostics)| {
        NativeDeriveProviderError::Provider(
            crate::analysis::ty::ty_check::GroundTypeAdmissionError {
                kind: crate::analysis::ty::ty_check::GroundTypeAdmissionKind::Bound {
                    goal: inst,
                    cause,
                },
                diagnostics,
            },
        )
    })?;
    Ok((finalized.key, input))
}

/// The ordinary protocol keys that describe an elaborated goal on this subject.
/// A declared trailing default can be omitted only when applying the smaller
/// family reconstructs the complete arguments in the request's environment.
pub(crate) fn derive_families<'db>(
    db: &'db dyn HirAnalysisDb,
    goal: crate::analysis::ty::trait_def::TraitInstId<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: crate::analysis::ty::trait_resolution::PredicateListId<'db>,
) -> Vec<TyId<'db>> {
    use crate::analysis::ty::{
        normalize::normalize_ty,
        ty_def::{TraitFamilyId, TyData},
    };

    let trait_ = goal.def(db);
    let explicit = &goal.args(db)[1..];
    let required = trait_.param_set(db).required_explicit_param_count(db);
    let make = |length| {
        TraitFamilyId::new(
            db,
            trait_,
            explicit[..length].to_vec(),
            goal.assoc_type_bindings(db).clone(),
            // Provider code is shared across the ordinary receiver modes.
            // The complete request separately retains its actual mode.
            crate::hir_def::ModeMarker::None,
        )
    };
    let mut families = Vec::new();
    if required < explicit.len() {
        let normalized = normalize_ty(db, goal, scope, assumptions);
        for length in required..explicit.len() {
            let family = make(length);
            let applied = family.apply(db, goal.self_ty(db));
            if let TyData::ConstraintTerm(rebuilt) = applied.data(db)
                && normalize_ty(db, *rebuilt, scope, assumptions).args(db) == normalized.args(db)
            {
                families.push(TyId::new(db, TyData::TraitFamily(family)));
            }
        }
    }
    families.push(TyId::new(db, TyData::TraitFamily(make(explicit.len()))));
    families
}

/// Freeze an elaboration dependency without evaluating metadata or selecting a provider.
pub fn freeze_generation_request<'db>(
    db: &'db dyn HirAnalysisDb,
    request: crate::analysis::ty::trait_resolution::generation::GenerationRequest<'db>,
) -> Result<common::generation::ScopedDeriveDemand, String> {
    let goal = request.goal(db);
    if let Some(reason) = request.refusal(db) {
        return Err(reason.to_string());
    }
    let opening = request.opening(db);
    let mode = match goal.mode(db) {
        crate::hir_def::ModeMarker::None => common::generation::ScopedDeriveMode::Ref,
        crate::hir_def::ModeMarker::Mut => common::generation::ScopedDeriveMode::Mut,
        crate::hir_def::ModeMarker::MaybeMut => {
            return Err("scoped derive requires a settled trait access mode".into());
        }
    };
    if !opening.may_offer(db, goal.def(db)) {
        return Err("scoped derive request does not match its advertised trait".into());
    }
    if !goal.assoc_type_bindings(db).is_empty()
        || goal.def(db).has_assoc_outputs(db)
        || !goal.def(db).consts(db).is_empty()
    {
        return Err("scoped derive requires a goal without associated outputs".into());
    }
    let trait_args = &goal.args(db)[1..];
    if trait_args
        .iter()
        .any(|&arg| !crate::analysis::ty::const_ty::ty_is_fully_ground(db, arg))
    {
        return Err("scoped derive requires ground trait arguments".into());
    }
    let trait_args = trait_args
        .iter()
        .map(|&arg| {
            freeze_generic_arg(db, arg).map_err(|_| {
                format!(
                    "scoped derive cannot preserve trait argument `{}`. Use a written implementation header.",
                    arg.pretty_print(db),
                )
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    if goal.def(db).ingot(db) != opening.scope(db).top_mod(db).ingot(db) {
        return Err(
            "a scoped derive supports traits of the same package; for a trait of another package, write a header `impl Trait for Type with (Provider)`"
                .into(),
        );
    }
    // Solver discovery freezes declarations only. RecordInput validates and
    // instantiates fields, so it belongs to admission outside solver queries.
    let (base, _) = goal.self_ty(db).decompose_ty_app(db);
    let crate::analysis::ty::ty_def::TyData::TyBase(crate::analysis::ty::ty_def::TyBase::Adt(adt)) =
        base.data(db)
    else {
        return Err("scoped derive requires a concrete authored record".into());
    };
    if !goal.self_ty(db).is_star_kind(db) {
        return Err("scoped derive requires a fully applied authored record".into());
    }
    let target_item = adt.adt_ref(db).as_item();
    let target_module = target_item.scope().top_mod(db);
    if target_module.ingot(db) != opening.scope(db).top_mod(db).ingot(db) {
        return Err(
            "scoped derive currently requires a target declared in the requesting ingot".into(),
        );
    }
    let target_root = crate::lower::top_mod_ast(db, target_module);
    let (owner_kind, target_range) = match adt.adt_ref(db) {
        crate::analysis::ty::adt_def::AdtRef::Struct(record) => {
            let Some(range) = record.origin(db).authored_range(target_root.syntax()) else {
                return Err("scoped target is not authored".into());
            };
            (common::generation::FrozenRecordOwnerKind::Struct, range)
        }
        crate::analysis::ty::adt_def::AdtRef::Enum(record) => {
            let HirOrigin::Raw(pointer) = record.origin(db) else {
                return Err("scoped target is not authored".into());
            };
            (
                common::generation::FrozenRecordOwnerKind::Enum,
                pointer.to_node(target_root.syntax()).syntax().text_range(),
            )
        }
    };
    // A generic target keeps its actual arguments, which must be ground: a
    // scoped opening has no header whose binders a generated item could name.
    let target_args = goal.self_ty(db).decompose_ty_app(db).1;
    let args = if target_args.is_empty() {
        None
    } else {
        if !crate::analysis::ty::const_ty::ty_is_fully_ground(db, goal.self_ty(db)) {
            return Err("scoped derive requires a ground target".into());
        }
        Some(
            target_args
                .iter()
                .map(|arg| freeze_generic_arg(db, *arg))
                .collect::<Result<Vec<_>, _>>()?,
        )
    };
    // The demand does not carry the provider: an opening is identified by the
    // authored `with` binding, and each view recovers the provider type from
    // that binding (`provider_opening_at` refuses a binding that yields two).
    // A provider application, generic or not, is therefore checked where it
    // is used, by `finalized_derive_provider`, which requires it to be ground.
    let trait_ = goal.def(db);
    let module = trait_.top_mod(db);
    let root = crate::lower::top_mod_ast(db, module);
    let HirOrigin::Raw(pointer) = trait_.origin(db) else {
        return Err("scoped derive requires an authored trait declaration".into());
    };
    Ok(common::generation::ScopedDeriveDemand {
        mode,
        opening: opening.key(db).clone(),
        trait_ref: common::generation::FrozenTraitRef {
            file: module
                .file(db)
                .url(db)
                .ok_or("trait declaration has no source URL")?,
            range: pointer.to_node(root.syntax()).syntax().text_range(),
        },
        trait_args,
        target: common::generation::FrozenRecordRef {
            owner_kind,
            target_file: target_module
                .file(db)
                .url(db)
                .ok_or("target declaration has no source URL")?,
            target_range,
            variant_index: None,
            args,
        },
    })
}

/// Rebind a frozen type in this view by declaration identity: the semantic
/// twin of lowering's `TypeId::thaw`, for a scoped demand's target.
fn thaw_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    frozen: &common::generation::FrozenTy,
) -> Result<TyId<'db>, String> {
    use crate::analysis::ty::ty_def::{PrimTy, TyBase, TyData};
    use common::generation::FrozenTy;
    let args = |args: &[common::generation::FrozenGenericArg]| {
        args.iter()
            .map(|arg| thaw_generic_arg(db, arg))
            .collect::<Result<Vec<_>, _>>()
    };
    let base = |prim: PrimTy| TyId::new(db, TyData::TyBase(TyBase::Prim(prim)));
    Ok(match frozen {
        FrozenTy::Declared {
            decl,
            args: decl_args,
        } => TyId::foldl(db, thaw_decl(db, decl)?, &args(decl_args)?),
        FrozenTy::Primitive {
            name,
            args: prim_args,
        } => {
            let primitive =
                crate::hir_def::prim_ty::PrimTy::from_name(db, IdentId::new(db, name.clone()))
                    .ok_or("a frozen primitive is not a builtin type")?;
            let base = TyId::new(db, TyData::TyBase(TyBase::from(primitive)));
            TyId::foldl(db, base, &args(prim_args)?)
        }
        FrozenTy::Tuple(elems) => {
            let elems = elems
                .iter()
                .map(|elem| thaw_ty(db, elem))
                .collect::<Result<Vec<_>, _>>()?;
            TyId::foldl(db, base(PrimTy::Tuple(elems.len())), &elems)
        }
        FrozenTy::Array { elem, len } => TyId::foldl(
            db,
            base(PrimTy::Array),
            &[thaw_ty(db, elem)?, thaw_const(db, len)?],
        ),
        FrozenTy::Ptr(pointee) => TyId::ptr_to(db, thaw_ty(db, pointee)?),
        FrozenTy::Never => TyId::never(db),
        FrozenTy::HeaderParam(_) => {
            return Err("a scoped demand names no derive header parameters".into());
        }
    })
}

fn thaw_generic_arg<'db>(
    db: &'db dyn HirAnalysisDb,
    arg: &common::generation::FrozenGenericArg,
) -> Result<TyId<'db>, String> {
    match arg {
        common::generation::FrozenGenericArg::Type(ty) => thaw_ty(db, ty),
        common::generation::FrozenGenericArg::Const(value) => thaw_const(db, value),
    }
}

fn thaw_const<'db>(
    db: &'db dyn HirAnalysisDb,
    frozen: &common::generation::FrozenConst,
) -> Result<TyId<'db>, String> {
    use crate::analysis::semantic::{SemConstId, SemConstScalar, SemConstValue};
    use crate::analysis::ty::ty_def::{TyBase, TyData};
    use common::generation::{FrozenConst, FrozenScalar};
    let FrozenConst::Value(scalar) = frozen else {
        return Err("a scoped demand names no derive header parameters".into());
    };
    let primitive = |name: &str| {
        crate::hir_def::prim_ty::PrimTy::from_name(db, IdentId::new(db, name.to_string()))
            .map(|primitive| TyId::new(db, TyData::TyBase(TyBase::from(primitive))))
            .ok_or("a frozen constant has no builtin type")
    };
    let (ty, value) = match scalar {
        FrozenScalar::Bool(value) => (primitive("bool")?, SemConstScalar::Bool(*value)),
        FrozenScalar::Int {
            negative,
            bytes,
            primitive: name,
        } => {
            let sign = if *negative {
                num_bigint::Sign::Minus
            } else {
                num_bigint::Sign::Plus
            };
            (
                primitive(name)?,
                SemConstScalar::Int {
                    value: num_bigint::BigInt::from_bytes_be(sign, bytes),
                },
            )
        }
    };
    let value = SemConstId::new(db, SemConstValue::Scalar { ty, value });
    Ok(TyId::const_ty(
        db,
        crate::analysis::ty::const_ty::const_ty_from_sem_const(db, value),
    ))
}

/// The struct or enum written at `decl` in this view.
fn thaw_decl<'db>(
    db: &'db dyn HirAnalysisDb,
    decl: &common::generation::FrozenDeclRef,
) -> Result<TyId<'db>, String> {
    use crate::analysis::ty::adt_def::AdtRef;
    let file = db
        .workspace()
        .get(db, &decl.file)
        .ok_or("a scoped target argument names a declaration outside the provider's input files")?;
    let module = crate::lower::map_file_to_mod_impl(db, file);
    let written_at = |origin: Option<ast::SyntaxNodePtr>| {
        origin.is_some_and(|pointer| pointer.text_range() == decl.range)
    };
    let root = crate::lower::top_mod_ast(db, module);
    let declaration = module
        .all_structs(db)
        .iter()
        .copied()
        .find(|s| s.origin(db).authored_range(root.syntax()) == Some(decl.range))
        .map(AdtRef::Struct)
        .or_else(|| {
            module
                .all_enums(db)
                .iter()
                .copied()
                .find(|e| {
                    written_at(match e.origin(db) {
                        HirOrigin::Raw(pointer) => Some(pointer.syntax_node_ptr()),
                        _ => None,
                    })
                })
                .map(AdtRef::Enum)
        })
        .ok_or("a frozen declaration is missing in unchanged source")?;
    Ok(TyId::adt(db, declaration.as_adt(db)))
}

/// Rebind an accumulated dependency through exact declarations and the checked
/// opening table. Provider type and proofs are never reconstructed from text.
pub fn rebind_generation_request<'db>(
    db: &'db dyn HirAnalysisDb,
    demand: &common::generation::ScopedDeriveDemand,
) -> Result<crate::analysis::ty::trait_resolution::generation::GenerationRequest<'db>, String> {
    use crate::analysis::ty::{
        adt_def::AdtRef,
        trait_def::TraitInstId,
        trait_resolution::generation::{GenerationRequest, provider_opening_at},
    };
    let trait_file = db
        .workspace()
        .get(db, &demand.trait_ref.file)
        .ok_or("requested trait file is missing")?;
    let trait_module = crate::lower::map_file_to_mod_impl(db, trait_file);
    let trait_root = crate::lower::top_mod_ast(db, trait_module);
    let trait_ = trait_module
        .all_traits(db)
        .iter()
        .copied()
        .find(|trait_| {
            let HirOrigin::Raw(pointer) = trait_.origin(db) else {
                return false;
            };
            pointer.to_node(trait_root.syntax()).syntax().text_range() == demand.trait_ref.range
        })
        .ok_or("requested trait declaration is missing")?;
    let target_file = db
        .workspace()
        .get(db, &demand.target.target_file)
        .ok_or("requested target file is missing")?;
    let target_module = crate::lower::map_file_to_mod_impl(db, target_file);
    let target_root = crate::lower::top_mod_ast(db, target_module);
    let target = match demand.target.owner_kind {
        common::generation::FrozenRecordOwnerKind::Struct => target_module
            .all_structs(db)
            .iter()
            .copied()
            .find(|target| {
                target.origin(db).authored_range(target_root.syntax())
                    == Some(demand.target.target_range)
            })
            .map(AdtRef::Struct),
        common::generation::FrozenRecordOwnerKind::Enum => target_module
            .all_enums(db)
            .iter()
            .copied()
            .find(|target| {
                let HirOrigin::Raw(pointer) = target.origin(db) else {
                    return false;
                };
                pointer.to_node(target_root.syntax()).syntax().text_range()
                    == demand.target.target_range
            })
            .map(AdtRef::Enum),
    }
    .ok_or("requested target declaration is missing")?;
    let opening = provider_opening_at(db, &demand.opening, trait_)
        .ok_or("request has no unique checked authored provider opening")?;
    let mode = match demand.mode {
        common::generation::ScopedDeriveMode::Ref => crate::hir_def::ModeMarker::None,
        common::generation::ScopedDeriveMode::Mut => crate::hir_def::ModeMarker::Mut,
    };
    let args = demand
        .target
        .args
        .iter()
        .flatten()
        .map(|arg| thaw_generic_arg(db, arg))
        .collect::<Result<Vec<_>, _>>()?;
    let target = TyId::foldl(db, TyId::adt(db, target.as_adt(db)), &args);
    let mut args = vec![target];
    for arg in &demand.trait_args {
        args.push(thaw_generic_arg(db, arg)?);
    }
    let goal = TraitInstId::new_simple(db, trait_, args).with_mode(db, mode);
    let request = GenerationRequest::new(db, opening, goal);
    if freeze_generation_request(db, request)? != *demand {
        return Err("requested identities changed while preparing unchanged source".into());
    }
    Ok(request)
}

/// Translate one immutable prepared binding to its admitted named candidate.
/// No proof or database-local item ID crosses between preparation views.
#[salsa::tracked(returns(copy))]
pub fn scoped_derive_implementation<'db>(
    db: &'db dyn HirAnalysisDb,
    opening: crate::analysis::ty::trait_resolution::generation::ProviderOpening<'db>,
    goal: crate::analysis::ty::trait_def::TraitInstId<'db>,
) -> Option<crate::analysis::ty::trait_def::ImplementorId<'db>> {
    use crate::analysis::ty::trait_resolution::generation::GenerationRequest;
    let demand = freeze_generation_request(db, GenerationRequest::new(db, opening, goal)).ok()?;
    let implementation = crate::lower::prepared_scoped_item(db, &demand)?;
    crate::analysis::ty::trait_lower::lower_impl_trait(db, implementation)
}

/// Scoped requests use the same finalized ordinary provider boundary as explicit requests.
pub fn scoped_derive_provider<'db>(
    db: &'db dyn HirAnalysisDb,
    opening: crate::analysis::ty::trait_resolution::generation::ProviderOpening<'db>,
    goal: crate::analysis::ty::trait_def::TraitInstId<'db>,
    origin: crate::hir_def::ItemKind<'db>,
    span: crate::span::DynLazySpan<'db>,
) -> Result<
    (
        crate::analysis::semantic::SemanticInstanceKey<'db>,
        RecordInput<'db>,
    ),
    NativeDeriveProviderError<'db>,
> {
    use crate::analysis::ty::trait_resolution::generation::GenerationRequest;
    freeze_generation_request(db, GenerationRequest::new(db, opening, goal))
        .map_err(NativeDeriveProviderError::Message)?;
    finalized_derive_provider(
        db,
        DeriveKey::Trait(goal),
        opening.provider_ty(db),
        origin,
        opening.scope(db),
        None,
        (span.clone(), span),
    )
}

/// Recover the exact authored header, never a first-by-name implementation.
/// The scope of the authored `mod` written at `range` in `module`.
pub fn authored_mod_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    module: crate::hir_def::TopLevelMod<'db>,
    range: parser::TextRange,
) -> Option<crate::hir_def::scope_graph::ScopeId<'db>> {
    module
        .all_mods(db)
        .iter()
        .copied()
        .find(|mod_| {
            matches!(mod_.origin(db), HirOrigin::Raw(pointer)
                if pointer.syntax_node_ptr().text_range() == range)
        })
        .map(|mod_| mod_.scope())
}

pub fn authored_derive_request<'db>(
    db: &'db dyn HirAnalysisDb,
    module: crate::hir_def::TopLevelMod<'db>,
    ast: &ast::ImplTrait,
) -> Option<crate::hir_def::ImplTrait<'db>> {
    let root = crate::core::lower::top_mod_ast(db, module);
    module
        .all_impl_traits(db)
        .iter()
        .copied()
        .find(|implementation| {
            let HirOrigin::Raw(ptr) = implementation.origin(db) else {
                return false;
            };
            implementation.scope().syntax_top_mod(db) == module
                && ptr.to_node(root.syntax()).syntax().text_range() == ast.syntax().text_range()
        })
}

/// The header syntax of an implementation, read from the module it was
/// written in: a generated implementation is written in its producer's quote.
fn impl_trait_syntax<'db>(
    db: &'db dyn HirAnalysisDb,
    implementation: crate::hir_def::ImplTrait<'db>,
) -> Option<(crate::hir_def::TopLevelMod<'db>, ast::ImplTrait)> {
    let HirOrigin::Raw(ptr) = implementation.origin(db) else {
        return None;
    };
    let module = implementation.scope().syntax_top_mod(db);
    let root = crate::core::lower::top_mod_ast(db, module);
    Some((module, ptr.to_node(root.syntax())))
}

/// An explicit derive header `impl Tr for T with (P)`. Every member under it
/// is generated from the provider's quotations, not authored here.
pub(crate) fn is_derive_request(
    db: &dyn HirAnalysisDb,
    request: crate::hir_def::ImplTrait<'_>,
) -> bool {
    impl_trait_syntax(db, request).is_some_and(|(_, ast)| ast.derive_provider().is_some())
}

/// A header-only request is not an implementation before its output plan exists:
/// preparation has not given it members yet, and a program is not accepted
/// until it has. The header is still a fact. In the input view of a
/// preparation, which sets `declared_headers`, lookup treats it as an
/// implementor whose members are unavailable, so admission and providers can
/// rely on its existence and cannot run it. Everywhere else it is no
/// implementation and supplies no proof.
pub(crate) fn is_unfulfilled_derive_request(
    db: &dyn HirAnalysisDb,
    request: crate::hir_def::ImplTrait<'_>,
) -> bool {
    let Some((module, ast)) = impl_trait_syntax(db, request) else {
        return false;
    };
    // A plan only fulfils a header written in its target file; a quoted header
    // has no authored identity to prepare against and stays unfulfilled.
    ast.derive_provider().is_some()
        && !(module == request.top_mod(db)
            && db.generation_inputs().requests(db).iter().any(|plan| {
                plan.request_impl == Some(ast.syntax().text_range())
                    && Some(plan.target_file.clone()) == module.file(db).url(db)
            }))
}

/// Whether `request` is a header that lookup must not treat as an implementor:
/// a header-only request outside the input view of a preparation.
pub(crate) fn is_pending_derive_request(
    db: &dyn HirAnalysisDb,
    request: crate::hir_def::ImplTrait<'_>,
) -> bool {
    is_unfulfilled_derive_request(db, request) && !db.generation_inputs().declared_headers(db)
}

/// Explicit inherent derive request, `impl Type with (Provider)`: the provider
/// implements `DeriveInherent` and supplies the members of this one inherent
/// implementation. The target is admitted under the header's own binders and
/// requirements, as for a trait header.
pub fn native_inherent_derive_provider<'db>(
    db: &'db dyn HirAnalysisDb,
    request: crate::hir_def::Impl<'db>,
    provider_ty: TyId<'db>,
) -> Result<
    (
        crate::analysis::semantic::SemanticInstanceKey<'db>,
        RecordInput<'db>,
    ),
    NativeDeriveProviderError<'db>,
> {
    finalized_derive_provider(
        db,
        DeriveKey::Inherent(request),
        provider_ty,
        request.into(),
        request.scope(),
        Some(request.into()),
        (request.span().target_ty().into(), request.span().into()),
    )
}

/// Recover the exact authored inherent header, never a first-by-type
/// implementation.
pub fn authored_inherent_derive_request<'db>(
    db: &'db dyn HirAnalysisDb,
    module: crate::hir_def::TopLevelMod<'db>,
    ast: &ast::Impl,
) -> Option<crate::hir_def::Impl<'db>> {
    module.all_impls(db).iter().copied().find(|implementation| {
        let HirOrigin::Raw(ptr) = implementation.origin(db) else {
            return false;
        };
        implementation.scope().syntax_top_mod(db) == module
            && ptr.syntax_node_ptr().text_range() == ast.syntax().text_range()
    })
}

/// Whether `request` is an inherent header, `impl Type with (Provider)`, that
/// preparation has not given its members yet. As for a trait header, a plan
/// fulfils only a header written in its target file.
pub(crate) fn is_unfulfilled_inherent_derive_request(
    db: &dyn HirAnalysisDb,
    request: crate::hir_def::Impl<'_>,
) -> bool {
    let HirOrigin::Raw(ptr) = request.origin(db) else {
        return false;
    };
    let module = request.scope().syntax_top_mod(db);
    let range = ptr.syntax_node_ptr().text_range();
    request.derive_provider(db).is_some()
        && !(module == request.top_mod(db)
            && db.generation_inputs().requests(db).iter().any(|plan| {
                plan.request_impl == Some(range)
                    && Some(plan.target_file.clone()) == module.file(db).url(db)
            }))
}
