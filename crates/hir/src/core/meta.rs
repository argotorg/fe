//! Bounded structural input for checked generation.
//!
//! These handles retain existing semantic type and declaration identities.
//! They are not a context, a proof record, or a second reflection database.
//! Output publication must happen after provider evaluation has finished.

use parser::ast::{self, prelude::*};
use salsa::SalsaValue;

use crate::{
    analysis::{HirAnalysisDb, ty::ty_def::TyId},
    hir_def::{IdentId, Struct},
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
pub struct RecordInput<'db> {
    ty: TyId<'db>,
    declaration: Struct<'db>,
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
        Self::for_authored_type(db, TyId::adt(db, adt))
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
        let adt = ty.adt_def(db).ok_or(RecordInputError::NotRecord)?;
        let crate::analysis::ty::adt_def::AdtRef::Struct(declaration) = adt.adt_ref(db) else {
            return Err(RecordInputError::NotRecord);
        };
        if !matches!(declaration.origin(db), HirOrigin::Raw(_)) {
            return Err(RecordInputError::NotAuthored);
        }
        if ty.field_types(db).iter().any(|ty| ty.has_invalid(db)) {
            return Err(RecordInputError::InvalidField);
        }
        Ok(Self { ty, declaration })
    }

    pub fn ty(self) -> TyId<'db> {
        self.ty
    }

    pub fn declaration(self) -> Struct<'db> {
        self.declaration
    }

    pub fn field_count(self, db: &'db dyn HirAnalysisDb) -> usize {
        self.declaration.hir_fields(db).data(db).len()
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
}

impl<'db> RecordField<'db> {
    pub fn record(self) -> RecordInput<'db> {
        self.record
    }

    pub fn index(self) -> usize {
        self.index
    }

    pub fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        self.record.declaration.hir_fields(db).data(db)[self.index]
            .name
            .to_opt()
    }

    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.record.ty.field_types(db)[self.index]
    }
}

/// A checked item template in the existing producer instance, with completed
/// scalar splice values. No generated declaration has been published yet.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ItemCode<'db> {
    key: crate::analysis::semantic::SemanticInstanceKey<'db>,
    expression: crate::hir_def::ExprId,
    holes: Box<[crate::analysis::semantic::SemConstId<'db>]>,
}

/// One bounded source walk shared by producer lowering, construction and
/// output copying. Unsupported nested constructors never lower missing holes.
pub(crate) fn quote_holes(quote: &ast::QuoteExpr) -> Result<Vec<ast::QuoteHoleExpr>, &'static str> {
    let mut holes = Vec::new();
    for (index, node) in quote.syntax().descendants().take(4097).enumerate() {
        if index == 4096 {
            return Err("item quotation exceeds the construction node limit of 4096");
        }
        if ast::QuoteExpr::can_cast(node.kind()) && node != *quote.syntax() {
            return Err(
                "nested quote expressions are not supported by this bounded item quotation",
            );
        }
        if let Some(hole) = ast::QuoteHoleExpr::cast(node.clone()) {
            if node
                .ancestors()
                .skip(1)
                .take_while(|ancestor| ancestor != quote.syntax())
                .any(|ancestor| ast::QuoteHoleExpr::can_cast(ancestor.kind()))
            {
                return Err("nested quotation holes are not supported");
            }
            holes.push(hole);
        }
    }
    Ok(holes)
}

impl<'db> ItemCode<'db> {
    pub(crate) fn for_quote(
        db: &'db dyn HirAnalysisDb,
        key: crate::analysis::semantic::SemanticInstanceKey<'db>,
        expression: crate::hir_def::ExprId,
        holes: Box<[crate::analysis::semantic::SemConstId<'db>]>,
    ) -> Result<Self, &'static str> {
        let value = Self {
            key,
            expression,
            holes,
        };
        value.validate(db)?;
        Ok(value)
    }

    pub fn producer(&self) -> crate::analysis::semantic::SemanticInstanceKey<'db> {
        self.key
    }
    pub fn holes(&self) -> &[crate::analysis::semantic::SemConstId<'db>] {
        &self.holes
    }

    /// Freeze this bounded same-file template for an output database. Scalar
    /// values retain their primitive type; source remains the authored AST.
    pub fn freeze_function(
        &self,
        db: &'db dyn HirAnalysisDb,
        target: crate::File,
    ) -> Result<common::generation::FrozenFunctionTemplate, &'static str> {
        use crate::analysis::semantic::{SemConstScalar, SemConstValue};
        use common::generation::{FrozenFunctionTemplate, FrozenScalar};
        self.validate(db)?;
        let body = self
            .key
            .owner(db)
            .body(db)
            .ok_or("quote producer has no body")?;
        if body.top_mod(db).file(db) != target {
            return Err("bounded generation requires quote and target in the same source file");
        }
        let quote = self.syntax(db)?;
        let ast::ItemKind::Func(function) = quote
            .body()
            .unwrap()
            .items()
            .next()
            .unwrap()
            .kind()
            .unwrap()
        else {
            unreachable!()
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
        let producer_block = quote
            .syntax()
            .ancestors()
            .find_map(ast::BlockExpr::cast)
            .map(|block| {
                block
                    .syntax()
                    .parent()
                    .and_then(ast::UnsafeExpr::cast)
                    .map_or_else(
                        || block.syntax().text_range(),
                        |unsafe_expr| unsafe_expr.syntax().text_range(),
                    )
            });
        let producer_module = quote
            .syntax()
            .ancestors()
            .find_map(ast::Mod::cast)
            .map(|module| module.syntax().text_range());
        let holes = self
            .holes
            .iter()
            .map(|value| match value.value(db) {
                SemConstValue::Scalar {
                    value: SemConstScalar::Bool(value),
                    ..
                } => Ok(FrozenScalar::Bool(*value)),
                SemConstValue::Scalar {
                    ty,
                    value: SemConstScalar::Int { value },
                } => {
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
                        primitive: primitive.clone(),
                    })
                }
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

    pub fn syntax(&self, db: &'db dyn HirAnalysisDb) -> Result<ast::QuoteExpr, &'static str> {
        let body = self
            .key
            .owner(db)
            .body(db)
            .ok_or("quote producer has no body")?;
        let HirOrigin::Raw(ptr) = body.source_map(db).expr_map.node_to_source(self.expression)
        else {
            return Err("item code must retain its authored quote origin");
        };
        let root = crate::lower::top_mod_ast(db, body.top_mod(db));
        let ast::ExprKind::Quote(quote) = ptr.to_node(root.syntax()).kind() else {
            return Err("item code source is not a quotation");
        };
        Ok(quote)
    }

    pub(crate) fn validate(&self, db: &'db dyn HirAnalysisDb) -> Result<(), &'static str> {
        let body = self
            .key
            .owner(db)
            .body(db)
            .ok_or("quote producer has no body")?;
        let crate::hir_def::Partial::Present(crate::hir_def::Expr::ItemQuote(holes)) =
            self.expression.data(db, body)
        else {
            return Err("item code does not identify a checked quote expression");
        };
        if holes.len() != self.holes.len() {
            return Err("item code splice arity differs from its source");
        }
        for value in &self.holes {
            crate::analysis::semantic::consts::verify_sem_const_shape(db, *value)?;
            if !matches!(
                value.value(db),
                crate::analysis::semantic::SemConstValue::Scalar { .. }
            ) {
                return Err("this bounded item quotation requires scalar splice values");
            }
        }
        let quote = self.syntax(db)?;
        quote_holes(&quote)?;
        let block = quote.body().ok_or("item quote has no body")?;
        if block.stmts().next().is_some() {
            return Err("item quotation requires item declarations");
        }
        let items = block.items().collect::<Vec<_>>();
        let [item] = items.as_slice() else {
            return Err("this bounded item quotation requires one function declaration");
        };
        let Some(ast::ItemKind::Func(function)) = item.kind() else {
            return Err("this bounded item quotation requires one function declaration");
        };
        if function
            .attr_list()
            .is_some_and(|attrs| attrs.normal_attrs_named("generate").next().is_some())
        {
            return Err("quoted functions cannot carry #[generate] attributes");
        }
        Ok(())
    }
}
