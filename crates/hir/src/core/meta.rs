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

/// Search existing semantic type arguments and instantiated fields. Repeated
/// nominal expansion is stopped independently from argument traversal.
pub fn ty_contains_meta_carrier<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    let mut pending = vec![ty];
    let mut seen = rustc_hash::FxHashSet::default();
    let mut expanded = rustc_hash::FxHashSet::default();
    while let Some(ty) = pending.pop() {
        if !seen.insert(ty) {
            continue;
        }
        if is_canonical_meta_carrier_ty(db, ty) {
            return Some(ty);
        }
        pending.extend(ty.generic_args(db).iter().copied());
        if let Some(adt) = ty.adt_def(db)
            && expanded.insert(adt)
        {
            for (variant, fields) in adt.fields(db).iter().enumerate() {
                pending.extend((0..fields.num_types()).map(|field| {
                    crate::analysis::ty::adt_def::instantiate_adt_field_shape(
                        db,
                        adt,
                        variant,
                        field,
                        ty.generic_args(db),
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
}

impl<'db> MetaValue<'db> {
    pub fn input(self) -> RecordInput<'db> {
        match self {
            Self::Record(record) => record,
            Self::Field(field) | Self::FieldType(field) => field.record(),
        }
    }

    pub(crate) fn carrier_path(self) -> &'static str {
        match self {
            Self::Record(_) => "core::meta::Record",
            Self::Field(_) => "core::meta::Field",
            Self::FieldType(_) => "core::meta::FieldType",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RecordInputError {
    NotConcrete,
    NotRecord,
    NotAuthored,
    InvalidField,
    FieldOutsideRecord,
}

impl<'db> RecordInput<'db> {
    /// Identify the annotation's actual declaration in its owning source module.
    pub fn for_authored_syntax(
        db: &'db dyn HirAnalysisDb,
        module: crate::hir_def::TopLevelMod<'db>,
        syntax: &ast::Struct,
    ) -> Result<Self, RecordInputError> {
        let origin = HirOrigin::raw(syntax);
        let declaration = module
            .all_structs(db)
            .iter()
            .find(|decl| *decl.origin(db) == origin)
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

    fn for_authored_adt_ty(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Result<Self, RecordInputError> {
        if !ty.is_star_kind(db) || ty.has_invalid(db) {
            return Err(RecordInputError::NotConcrete);
        }
        let adt = ty.adt_def(db).ok_or(RecordInputError::NotRecord)?;
        let owner = match adt.adt_ref(db) {
            crate::analysis::ty::adt_def::AdtRef::Struct(declaration) => {
                if !matches!(declaration.origin(db), HirOrigin::Raw(_)) {
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

    pub fn declaration(self) -> Struct<'db> {
        match self.owner {
            RecordOwner::Struct(s) => s,
            RecordOwner::Enum(_) => panic!("record input is an enum"),
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

    pub fn variant_count(self, db: &'db dyn HirAnalysisDb) -> usize {
        if self.variant_index.is_some() {
            return 0;
        }
        match self.owner {
            RecordOwner::Struct(_) => 0,
            RecordOwner::Enum(e) => e.variants_list(db).data(db).len(),
        }
    }

    pub fn variant(
        self,
        db: &'db dyn HirAnalysisDb,
        index: usize,
    ) -> Result<Self, RecordInputError> {
        let RecordOwner::Enum(e) = self.owner else {
            return Err(RecordInputError::FieldOutsideRecord);
        };
        if self.variant_index.is_some() || index >= e.variants_list(db).data(db).len() {
            return Err(RecordInputError::FieldOutsideRecord);
        }
        Ok(Self {
            ty: self.ty,
            owner: self.owner,
            variant_index: Some(index),
        })
    }

    pub fn field_count(self, db: &'db dyn HirAnalysisDb) -> usize {
        match (self.owner, self.variant_index) {
            (RecordOwner::Struct(s), None) => s.hir_fields(db).data(db).len(),
            (RecordOwner::Enum(e), Some(v)) => match e.variants_list(db).data(db)[v].kind {
                VariantKind::Unit => 0,
                VariantKind::Tuple(t) => t.data(db).len(),
                VariantKind::Record(f) => f.data(db).len(),
            },
            _ => 0,
        }
    }

    pub fn field(
        self,
        db: &'db dyn HirAnalysisDb,
        index: usize,
    ) -> Result<RecordField<'db>, RecordInputError> {
        if index >= self.field_count(db) {
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
                let HirOrigin::Raw(ptr) = s.origin(db) else {
                    return Err("record input is not authored");
                };
                (
                    FrozenRecordOwnerKind::Struct,
                    top_mod,
                    ptr.to_node(root.syntax()).syntax().text_range(),
                )
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
        Ok(FrozenRecordRef {
            owner_kind,
            target_file,
            target_range,
            variant_index: self.variant_index,
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
                crate::analysis::ty::adt_def::instantiate_adt_field_shape(
                    db,
                    adt,
                    v,
                    self.index,
                    self.record.ty.generic_args(db),
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
/// and one binding for each of its fields.
fn binder_pattern_nodes<'db>(db: &'db dyn HirAnalysisDb, record: RecordInput<'db>) -> usize {
    1 + record.field_count(db)
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
        if function.attr_list().is_some_and(|attrs| {
            attrs.into_iter().any(|attr| match attr.kind() {
                ast::AttrKind::Normal(normal) => {
                    normal.path().is_some_and(|path| path.text() == "generate")
                }
                ast::AttrKind::DocComment(_) => false,
            })
        }) {
            return Err("quoted functions cannot carry #[generate] attributes");
        }
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
                            MetaValue::FieldType(field) => {
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
            target_file: target
                .url(db)
                .ok_or("generation target has no source URL")?,
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
                            value: MetaValue::Record(_) | MetaValue::FieldType(_),
                            ..
                        } => {}
                        _ => {
                            return Err(
                                "type quotation hole requires Type<T> or FieldType<T, F> splice",
                            );
                        }
                    },
                    parser::SyntaxKind::QuoteHolePat => match value.value(db) {
                        SemConstValue::Scalar { .. }
                        | SemConstValue::Meta {
                            value: MetaValue::Record(_) | MetaValue::Field(_),
                            ..
                        } => {}
                        SemConstValue::ItemCode { code, .. }
                            if code.kind() == FrozenCodeKind::Pat => {}
                        _ => {
                            return Err(
                                "pattern quotation hole requires Code<Pat>, Type<T>, Field<T, F>, or scalar splice",
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
                        if *kind == FrozenCodeKind::Pat {
                            match value.value(db) {
                                SemConstValue::Scalar { .. }
                                | SemConstValue::Meta {
                                    value: MetaValue::Record(_) | MetaValue::Field(_),
                                    ..
                                } => {}
                                SemConstValue::ItemCode { code, .. }
                                    if code.kind() == FrozenCodeKind::Pat => {}
                                _ => {
                                    return Err(
                                        "pattern quotation hole requires Code<Pat>, Type<T>, Field<T, F>, or scalar splice",
                                    );
                                }
                            }
                        } else {
                            match value.value(db) {
                                SemConstValue::Scalar { .. }
                                | SemConstValue::Meta {
                                    value: MetaValue::Record(_) | MetaValue::Field(_),
                                    ..
                                } => {}
                                SemConstValue::ItemCode { code, .. }
                                    if code.kind() == FrozenCodeKind::Expr => {}
                                _ => {
                                    return Err(
                                        "expression quotation hole requires Code<Expr>, Type<T>, Field<T, F>, or scalar splice",
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
                                    | ast::ItemKind::Impl(_)
                                    | ast::ItemKind::ImplTrait(_)
                            )
                        ) {
                            return Err(
                                "item quotation supports function, impl, and trait impl declarations",
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
                let items = block.items().collect::<Vec<_>>();
                if items.is_empty() {
                    return Err("item quotation requires item declarations");
                }
                for item in items {
                    if !matches!(
                        item.kind(),
                        Some(
                            ast::ItemKind::Func(_)
                                | ast::ItemKind::Impl(_)
                                | ast::ItemKind::ImplTrait(_)
                        )
                    ) {
                        return Err(
                            "item quotation supports function, impl, and trait impl declarations",
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
                if !valid_pat_block {
                    return Err("pattern quotation requires a pattern");
                }
            }
        }
        Ok(())
    }
}
