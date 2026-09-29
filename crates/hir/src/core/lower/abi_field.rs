//! Lowering shared by `#[event]` and `#[error]` structs: the struct item, the
//! syntactic check of each field type, and the diagnostics both report.

use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::{FileLowerCtxt, attr::lower_attrs_without_named, hir_builder::HirBuilder};
use crate::{
    hir_def::{
        FieldDef, FieldDefListId, GenericParamListId, IdentId, Partial, PathId, Struct, TypeId,
        TypeKind, WhereClauseId,
    },
    span::DesugaredOrigin,
};

/// ABI field contexts that share field-type validation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbiFieldContext {
    Event,
    Error,
}

impl AbiFieldContext {
    /// The attribute that marks a struct of this kind.
    fn attr_name(self) -> &'static str {
        match self {
            Self::Event => "event",
            Self::Error => "error",
        }
    }
}

/// Diagnostics for unsupported field types in ABI-bearing structs.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbiFieldDiagnostic {
    pub context: AbiFieldContext,
    pub ty: String,
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
}

/// A problem with an `#[event]` or `#[error]` struct declaration found while
/// lowering it or while checking its indexed fields.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbiRecordDiagnostic {
    pub kind: AbiRecordDiagnosticKind,
    pub file: common::file::File,
    /// Range of the primary span (attribute, type, or item name).
    pub primary_range: parser::TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AbiRecordDiagnosticKind {
    /// An `#[event]` or `#[error]` struct has generic parameters.
    GenericStruct(AbiFieldContext),
    /// A struct is marked both `#[error]` and `#[event]`.
    EventErrorAttrConflict,
    TooManyIndexedFields {
        indexed_count: usize,
    },
    IndexedDynamicField {
        ty: String,
    },
}

impl AbiRecordDiagnosticKind {
    /// The kind of struct whose lowering reports this diagnostic.
    pub fn context(&self) -> AbiFieldContext {
        match self {
            Self::GenericStruct(context) => *context,
            Self::EventErrorAttrConflict => AbiFieldContext::Error,
            Self::TooManyIndexedFields { .. } | Self::IndexedDynamicField { .. } => {
                AbiFieldContext::Event
            }
        }
    }
}

/// The struct item of an `#[event]` or `#[error]` declaration, with the
/// caller's parsed fields.
pub(super) struct LoweredAbiRecord<'db, F> {
    pub(super) struct_: Struct<'db>,
    pub(super) fields: F,
    /// The struct's type and name, present when its impls can be generated:
    /// its fields are valid, it has no generic parameters, and it is named.
    pub(super) generated: Option<(TypeId<'db>, String)>,
}

/// Lowers the struct item of an `#[event]` or `#[error]` declaration: strips
/// the struct's attribute, reports generic parameters, and lowers the fields
/// with `parse_fields`, which returns the HIR fields, whether they are valid,
/// and what else the caller's impls need.
pub(super) fn lower_abi_record_struct<'db, O, F>(
    builder: &mut HirBuilder<'_, 'db, O>,
    ast: &ast::Struct,
    context: AbiFieldContext,
    parse_fields: impl FnOnce(&mut FileLowerCtxt<'db>) -> (Vec<FieldDef<'db>>, bool, F),
) -> LoweredAbiRecord<'db, F>
where
    O: Clone + Into<DesugaredOrigin>,
{
    let db = builder.db();
    let file = builder.ctxt().top_mod().file(db);
    let struct_name_token = ast.name();
    let struct_name = struct_name_token.as_ref().map(|n| n.text().to_string());

    let attributes =
        lower_attrs_without_named(builder.ctxt(), ast.attr_list(), context.attr_name());
    let vis = super::lower_visibility(ast);
    let generic_params = GenericParamListId::lower_ast_opt(builder.ctxt(), ast.generic_params());
    let is_generic = !generic_params.data(db).is_empty();
    if is_generic {
        let range = ast
            .generic_params()
            .map_or_else(|| ast.syntax().text_range(), |g| g.syntax().text_range());
        AbiRecordDiagnostic {
            kind: AbiRecordDiagnosticKind::GenericStruct(context),
            file,
            primary_range: range,
        }
        .accumulate(db);
    }

    let where_clause = WhereClauseId::lower_ast_opt(builder.ctxt(), ast.where_clause());
    let (hir_fields, fields_are_valid, fields) = parse_fields(builder.ctxt());
    let fields_hir = FieldDefListId::new(db, hir_fields);
    let name_ident = IdentId::lower_token_partial(builder.ctxt(), struct_name_token);

    let struct_ = builder.struct_item(
        name_ident,
        attributes,
        vis,
        generic_params,
        where_clause,
        fields_hir,
    );

    let generated = match (name_ident.to_opt(), struct_name) {
        (Some(name_ident), Some(struct_name)) if fields_are_valid && !is_generic => {
            let self_ty = TypeId::new(
                db,
                TypeKind::Path(Partial::Present(PathId::from_ident(db, name_ident))),
            );
            Some((self_ty, struct_name))
        }
        _ => None,
    };

    LoweredAbiRecord {
        struct_,
        fields,
        generated,
    }
}

/// Whether the declared type of `field` is a path. Generated impls name each
/// field type in a path (`<FieldType as SolCompat>::SOL_TYPE`), so any other
/// type is reported here, and the struct gets no impls.
pub(super) fn check_abi_record_field_ty<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    field: &ast::RecordFieldDef,
    ty: TypeId<'db>,
    context: AbiFieldContext,
) -> bool {
    let db = ctxt.db();
    if let TypeKind::Path(Partial::Present(_)) = ty.data(db) {
        return true;
    }
    AbiFieldDiagnostic {
        context,
        ty: ty.pretty_print(db),
        file: ctxt.top_mod().file(db),
        primary_range: field
            .ty()
            .map_or_else(|| field.syntax().text_range(), |t| t.syntax().text_range()),
    }
    .accumulate(db);
    false
}
