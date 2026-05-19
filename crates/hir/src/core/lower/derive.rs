use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::{
    FileLowerCtxt,
    attr::{has_named_attr, named_attr_specs},
    hir_builder::HirBuilder,
};
use crate::{
    analysis::semantic::ctfe::derive_eval::CodegenSink,
    hir_def::{
        AttrListId, Cond, Expr, ExprId, FieldDef, FieldDefListId, FuncModifiers, FuncParam,
        FuncParamMode, FuncParamName, GenericParamListId, IdentId, Partial, PathId, Stmt, StmtId,
        Struct, TraitRefId, TypeId, TypeKind, Visibility,
    },
    span::DeriveDesugared,
};

#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeriveError {
    pub kind: DeriveErrorKind,
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
    pub struct_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeriveErrorKind {
    DeriveOnEnum,
    DeriveOnGenericStruct { trait_name: String },
    UnknownDeriveTrait { name: String },
}

pub const KNOWN_DERIVE_TRAITS: &[&str] = &["Eq", "Default", "Abi", "Hash", "Ord", "Event", "Error"];

pub(super) fn is_derive_struct(ast: &ast::Struct) -> bool {
    has_named_attr(ast.attr_list(), "derive")
}

pub(super) fn report_derive_attr_on_non_struct_item<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    item_kind: &'static str,
) {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    for attr in named_attr_specs(attrs, "derive") {
        if item_kind == "enum" {
            DeriveError {
                kind: DeriveErrorKind::DeriveOnEnum,
                file,
                primary_range: attr.range,
                struct_name: None,
            }
            .accumulate(db);
        }
    }
}

fn parse_derive_trait_names(attrs: Option<ast::AttrList>) -> Vec<String> {
    let mut names = Vec::new();
    let specs = named_attr_specs(attrs, "derive");
    for spec in specs {
        for arg in &spec.args {
            if let Some(key) = &arg.key {
                names.push(key.clone());
            }
        }
    }
    names
}

/// Find a `#[derive_strategy]` function in the core ingot by name.
pub(crate) fn find_strategy_func<'db>(
    db: &'db dyn crate::HirDb,
    ingot: common::ingot::Ingot<'db>,
    strategy_name: &str,
) -> Option<crate::hir_def::Func<'db>> {
    use crate::hir_def::HirIngot;

    let core_ingot = ingot
        .resolved_external_ingots(db)
        .iter()
        .find(|(name, _)| name.data(db) == "core")
        .map(|(_, ing)| *ing);

    let search_ingot = if ingot.kind(db) == common::ingot::IngotKind::Core {
        Some(ingot)
    } else {
        core_ingot
    };

    search_ingot.and_then(|core| {
        core.all_funcs(db)
            .iter()
            .find(|func| {
                func.name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == strategy_name)
                    && func.attributes(db).has_attr(db, "derive_strategy")
            })
            .copied()
    })
}

struct DeriveTraitSpec {
    trait_name: &'static str,
    strategy_name: &'static str,
    method_name: &'static str,
    trait_path: &'static [&'static str],
    has_self_param: bool,
    has_other_param: bool,
    returns_bool: bool,
}

const DERIVE_TRAIT_SPECS: &[DeriveTraitSpec] = &[
    DeriveTraitSpec {
        trait_name: "Eq",
        strategy_name: "__derive_eq",
        method_name: "eq",
        trait_path: &["ops", "Eq"],
        has_self_param: true,
        has_other_param: true,
        returns_bool: true,
    },
    DeriveTraitSpec {
        trait_name: "Hash",
        strategy_name: "__derive_hash",
        method_name: "hash",
        trait_path: &["ops", "Hash"],
        has_self_param: true,
        has_other_param: false,
        returns_bool: false,
    },
    DeriveTraitSpec {
        trait_name: "Ord",
        strategy_name: "__derive_ord",
        method_name: "lt",
        trait_path: &["ops", "Ord"],
        has_self_param: true,
        has_other_param: true,
        returns_bool: true,
    },
    DeriveTraitSpec {
        trait_name: "Default",
        strategy_name: "__derive_default",
        method_name: "default",
        trait_path: &["default", "Default"],
        has_self_param: false,
        has_other_param: false,
        returns_bool: false,
    },
];

/// Lower a `#[derive(...)]` struct.
///
/// Creates the struct AND generates impl bodies via CTFE evaluation of
/// derive strategy functions. This is the same pipeline as msg/event/error
/// desugaring — all generated HIR goes through HirBuilder during lowering.
pub(super) fn lower_derive_struct<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: ast::Struct,
) -> Struct<'db> {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    let derive_desugared = DeriveDesugared {
        derive_struct: parser::ast::AstPtr::new(&ast),
    };
    let mut builder = HirBuilder::new(ctxt, derive_desugared);

    let struct_name_token = ast.name();
    let struct_name = struct_name_token.as_ref().map(|n| n.text().to_string());

    let trait_names = parse_derive_trait_names(ast.attr_list());

    for name in &trait_names {
        if !KNOWN_DERIVE_TRAITS.contains(&name.as_str()) {
            let specs = named_attr_specs(ast.attr_list(), "derive");
            let range = specs
                .first()
                .map(|s| s.range)
                .unwrap_or_else(|| ast.syntax().text_range());
            DeriveError {
                kind: DeriveErrorKind::UnknownDeriveTrait { name: name.clone() },
                file,
                primary_range: range,
                struct_name: struct_name.clone(),
            }
            .accumulate(db);
        }
    }

    let attributes = AttrListId::lower_ast_opt(builder.ctxt(), ast.attr_list());
    let vis = super::lower_visibility(&ast);
    let generic_params = GenericParamListId::lower_ast_opt(builder.ctxt(), ast.generic_params());
    let is_generic = !generic_params.data(db).is_empty();

    if is_generic {
        for name in &trait_names {
            if KNOWN_DERIVE_TRAITS.contains(&name.as_str()) {
                let range = ast
                    .generic_params()
                    .map(|g| g.syntax().text_range())
                    .unwrap_or_else(|| ast.syntax().text_range());
                DeriveError {
                    kind: DeriveErrorKind::DeriveOnGenericStruct {
                        trait_name: name.clone(),
                    },
                    file,
                    primary_range: range,
                    struct_name: struct_name.clone(),
                }
                .accumulate(db);
            }
        }
    }

    let where_clause =
        crate::hir_def::WhereClauseId::lower_ast_opt(builder.ctxt(), ast.where_clause());

    let parsed_fields = parse_struct_fields(builder.ctxt(), &ast);
    let fields_hir = FieldDefListId::new(db, parsed_fields.hir_fields);
    let name_ident = IdentId::lower_token_partial(builder.ctxt(), struct_name_token);

    let struct_ = builder.struct_item(
        name_ident,
        attributes,
        vis,
        generic_params,
        where_clause,
        fields_hir,
    );

    if is_generic || !parsed_fields.is_valid {
        return struct_;
    }

    let Some(struct_name_ident) = name_ident.to_opt() else {
        return struct_;
    };

    let self_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_ident(db, struct_name_ident))),
    );

    let ingot = builder.top_mod().ingot(db);

    // Generate trait impls for each recognized derive trait.
    for trait_name in &trait_names {
        match trait_name.as_str() {
            "Ord" => {
                generate_ord_impl(&mut builder, self_ty, &parsed_fields.field_specs, ingot);
            }
            "Abi" | "Event" | "Error" => {
                generate_abi_size_impl(&mut builder, self_ty, &parsed_fields.field_specs);
            }
            _ => {
                if let Some(spec) = DERIVE_TRAIT_SPECS
                    .iter()
                    .find(|s| s.trait_name == trait_name)
                {
                    generate_derive_impl(
                        &mut builder,
                        spec,
                        self_ty,
                        &parsed_fields.field_specs,
                        ingot,
                        struct_,
                    );
                }
            }
        }
    }

    struct_
}

fn generate_derive_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, DeriveDesugared>,
    spec: &DeriveTraitSpec,
    self_ty: TypeId<'db>,
    field_specs: &[(IdentId<'db>, TypeId<'db>)],
    ingot: common::ingot::Ingot<'db>,
    struct_def: Struct<'db>,
) {
    let db = builder.db();
    let roots = builder.roots();

    let trait_path = {
        let mut path = PathId::from_ident(db, roots.core);
        for seg in spec.trait_path {
            path = path.push_str(db, seg);
        }
        path
    };
    let trait_ref = TraitRefId::new(db, Partial::Present(trait_path));

    let method_name = builder.ident(spec.method_name);
    let generic_params = builder.empty_generic_params();

    let params = build_derive_params(builder, spec);
    let ret_ty = if spec.returns_bool {
        Some(builder.ty_ident(builder.ident("bool")))
    } else if spec.trait_name == "Default" {
        Some(builder.self_ty())
    } else if spec.trait_name == "Hash" {
        Some(builder.ty_ident(builder.ident("u256")))
    } else {
        None
    };

    let field_specs_owned: Vec<_> = field_specs.to_vec();

    builder.impl_trait(trait_ref, self_ty, |builder| {
        builder.func_with_body(
            method_name,
            generic_params,
            params,
            ret_ty,
            FuncModifiers::new(Visibility::Private, false, false, false),
            |body| {
                emit_derive_body(body, spec, &field_specs_owned, ingot, struct_def);
            },
        );
    });
}

const ORD_METHODS: &[(&str, &str)] = &[
    ("lt", "__derive_ord"),
    ("le", "__derive_le"),
    ("gt", "__derive_gt"),
    ("ge", "__derive_ge"),
];

fn generate_ord_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, DeriveDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &[(IdentId<'db>, TypeId<'db>)],
    ingot: common::ingot::Ingot<'db>,
) {
    let db = builder.db();
    let roots = builder.roots();

    let trait_path = PathId::from_ident(db, roots.core)
        .push_str(db, "ops")
        .push_str(db, "Ord");
    let trait_ref = TraitRefId::new(db, Partial::Present(trait_path));

    let generic_params = builder.empty_generic_params();
    let self_param = FuncParam {
        mode: FuncParamMode::View,
        is_mut: false,
        has_ref_prefix: false,
        has_own_prefix: false,
        is_label_suppressed: false,
        name: Partial::Present(FuncParamName::Ident(IdentId::make_self(db))),
        ty: Partial::Present(builder.self_ty()),
        self_ty_fallback: true,
    };
    let other_ident = builder.ident("other");
    let other_param = FuncParam {
        mode: FuncParamMode::View,
        is_mut: false,
        has_ref_prefix: false,
        has_own_prefix: false,
        is_label_suppressed: true,
        name: Partial::Present(FuncParamName::Ident(other_ident)),
        ty: Partial::Present(builder.self_ty()),
        self_ty_fallback: true,
    };
    let ret_ty = Some(builder.ty_ident(builder.ident("bool")));

    let field_specs_owned: Vec<_> = field_specs.to_vec();
    let field_names: Vec<_> = field_specs_owned.iter().map(|(name, _)| *name).collect();

    builder.impl_trait(trait_ref, self_ty, |builder| {
        for (method, strategy_name) in ORD_METHODS {
            let name = builder.ident(method);
            let params = builder.params([self_param.clone(), other_param.clone()]);
            builder.func_with_body(
                name,
                generic_params,
                params,
                ret_ty,
                FuncModifiers::new(Visibility::Private, false, false, false),
                |body| {
                    let Some(strategy) = find_strategy_func(body.db(), ingot, strategy_name) else {
                        let lit = body.push_expr(Expr::Lit(crate::hir_def::LitKind::Bool(false)));
                        body.emit_stmt(Stmt::Return(Some(lit)));
                        return;
                    };
                    assert!(
                        crate::analysis::semantic::ctfe::derive_eval::eval_strategy_from_hir(
                            body.db(),
                            strategy,
                            &field_names,
                            body,
                        ),
                        "CTFE derive strategy evaluation failed for Ord::{method}",
                    );
                },
            );
        }
    });
}

fn build_derive_params<'db>(
    builder: &mut HirBuilder<'_, 'db, DeriveDesugared>,
    spec: &DeriveTraitSpec,
) -> crate::hir_def::FuncParamListId<'db> {
    let db = builder.db();
    let mut params = Vec::new();

    if spec.has_self_param {
        // View mode `self` to match trait signatures (e.g. `fn eq(self, ...)`)
        params.push(FuncParam {
            mode: FuncParamMode::View,
            is_mut: false,
            has_ref_prefix: false,
            has_own_prefix: false,
            is_label_suppressed: false,
            name: Partial::Present(FuncParamName::Ident(IdentId::make_self(db))),
            ty: Partial::Present(builder.self_ty()),
            self_ty_fallback: true,
        });
    }

    if spec.has_other_param {
        let other_ident = builder.ident("other");
        params.push(FuncParam {
            mode: FuncParamMode::View,
            is_mut: false,
            has_ref_prefix: false,
            has_own_prefix: false,
            is_label_suppressed: true,
            name: Partial::Present(FuncParamName::Ident(other_ident)),
            ty: Partial::Present(builder.self_ty()),
            self_ty_fallback: true,
        });
    }

    builder.params(params)
}

impl<'db> CodegenSink<'db> for super::hir_builder::BodyBuilder<'_, 'db, DeriveDesugared> {
    fn push_expr(&mut self, expr: Expr<'db>) -> ExprId {
        super::hir_builder::BodyBuilder::push_expr(self, expr)
    }
    fn push_cond(&mut self, cond: Cond) -> crate::hir_def::CondId {
        super::hir_builder::BodyBuilder::push_cond(self, cond)
    }
    fn push_stmt(&mut self, stmt: Stmt<'db>) -> StmtId {
        self.push_stmt_raw(stmt)
    }
    fn emit_stmt(&mut self, stmt: Stmt<'db>) -> StmtId {
        super::hir_builder::BodyBuilder::emit_stmt(self, stmt)
    }
    fn emit_expr_stmt(&mut self, expr: ExprId) -> StmtId {
        super::hir_builder::BodyBuilder::emit_expr_stmt(self, expr)
    }
}

fn emit_derive_body<'db>(
    body: &mut super::hir_builder::BodyBuilder<'_, 'db, DeriveDesugared>,
    spec: &DeriveTraitSpec,
    field_specs: &[(IdentId<'db>, TypeId<'db>)],
    ingot: common::ingot::Ingot<'db>,
    _struct_def: Struct<'db>,
) {
    let field_names: Vec<_> = field_specs.iter().map(|(name, _)| *name).collect();

    let Some(strategy_func) = find_strategy_func(body.db(), ingot, spec.strategy_name) else {
        let unit = body.push_expr(Expr::Lit(crate::hir_def::LitKind::Bool(false)));
        body.emit_stmt(Stmt::Return(Some(unit)));
        return;
    };

    assert!(
        crate::analysis::semantic::ctfe::derive_eval::eval_strategy_from_hir(
            body.db(),
            strategy_func,
            &field_names,
            body,
        ),
        "CTFE derive strategy evaluation failed for {:?}",
        spec.trait_name,
    );
}

/// Generate `impl AbiSize for Struct { const HEAD_SIZE: u256 = ...; const IS_DYNAMIC: bool = ... }`
fn generate_abi_size_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, DeriveDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &[(IdentId<'db>, TypeId<'db>)],
) {
    let db = builder.db();
    let roots = builder.roots();

    let trait_path = PathId::from_ident(db, roots.core)
        .push_str(db, "abi")
        .push_str(db, "AbiSize");
    let trait_ref = TraitRefId::new(db, Partial::Present(trait_path));

    let field_specs_owned: Vec<_> = field_specs.to_vec();
    builder.impl_trait_assocs_build(trait_ref, self_ty, |builder| {
        let consts = vec![
            super::msg::create_head_size_assoc_const(builder, &field_specs_owned),
            super::msg::create_is_dynamic_assoc_const(builder, &field_specs_owned),
        ];
        (vec![], consts)
    });
}

struct ParsedStructFields<'db> {
    hir_fields: Vec<FieldDef<'db>>,
    field_specs: Vec<(IdentId<'db>, TypeId<'db>)>,
    is_valid: bool,
}

fn parse_struct_fields<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
) -> ParsedStructFields<'db> {
    let mut hir_fields = Vec::new();
    let mut field_specs = Vec::new();
    let mut is_valid = true;

    let Some(fields) = ast.fields() else {
        return ParsedStructFields {
            hir_fields,
            field_specs,
            is_valid,
        };
    };

    for field in fields {
        let attrs = AttrListId::lower_ast_opt(ctxt, field.attr_list());
        let name_tok = field.name();
        let name_ident = IdentId::lower_token_partial(ctxt, name_tok);
        let ty_ref = TypeId::lower_ast_partial(ctxt, field.ty());
        let vis = super::lower_field_visibility(&field);

        hir_fields.push(FieldDef::new(attrs, name_ident, ty_ref, vis));

        let (Some(name_ident), Some(ty)) = (name_ident.to_opt(), ty_ref.to_opt()) else {
            is_valid = false;
            continue;
        };

        field_specs.push((name_ident, ty));
    }

    ParsedStructFields {
        hir_fields,
        field_specs,
        is_valid,
    }
}
