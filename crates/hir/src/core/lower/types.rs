use parser::ast::{self, prelude::*};

use super::FileLowerCtxt;
use crate::core::hir_def::{
    AssocTypeGenericArg, Body, GenericArg, GenericArgListId, IdentId, ItemKind, Partial, PathId,
    PathKind, TraitRefId, TupleTypeId, TypeGenericArg, TypeId, TypeKind, TypeMode, VariantKind,
    params::{FuncParamMode, GenericParamListId, callable_shape_name},
    scope_graph::ScopeId,
};

impl<'db> TypeId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Type) -> Self {
        let kind = match ast.kind() {
            ast::TypeKind::QuoteHole(hole) => {
                let db = ctxt.db();
                match ctxt.current_hole(hole.syntax().text_range()) {
                    Some(common::generation::FrozenHoleValue::Record(record_ref)) => {
                        let item = ctxt.resolve_record_item(&record_ref);
                        let params = match item {
                            ItemKind::Struct(s) => s.generic_params(db),
                            ItemKind::Enum(e) => e.generic_params(db),
                            _ => GenericParamListId::new(db, Vec::new()),
                        };
                        let generic_args = if params.data(db).is_empty() {
                            GenericArgListId::none(db)
                        } else {
                            let args: Vec<_> = params
                                .data(db)
                                .iter()
                                .map(|param| {
                                    let ty = param.name().to_opt().map(|ident| {
                                        TypeId::new(
                                            db,
                                            TypeKind::Path(Partial::Present(PathId::from_ident(
                                                db, ident,
                                            ))),
                                        )
                                    });
                                    match param {
                                        crate::hir_def::params::GenericParam::Const(_) => {
                                            GenericArg::Const(
                                                crate::hir_def::params::ConstGenericArg {
                                                    value: param
                                                        .name()
                                                        .to_opt()
                                                        .map(|name| {
                                                            Body::lower_const_param_name(ctxt, name)
                                                        })
                                                        .into(),
                                                },
                                            )
                                        }
                                        _ => GenericArg::Type(TypeGenericArg { ty: ty.into() }),
                                    }
                                })
                                .collect();
                            GenericArgListId::new(db, args, true)
                        };
                        let display = PathId::new(
                            db,
                            PathKind::Ident {
                                ident: item.name(db).into(),
                                generic_args,
                            },
                            None,
                        );
                        let path = PathId::new(
                            db,
                            PathKind::BoundItem {
                                item: ScopeId::Item(item),
                                display,
                            },
                            None,
                        );
                        TypeKind::Path(Partial::Present(path))
                    }
                    Some(common::generation::FrozenHoleValue::FieldType(field_ref)) => {
                        let record_item = ctxt.resolve_record_item(&field_ref.record);
                        let resolved = match (record_item, field_ref.record.variant_index) {
                            (ItemKind::Struct(s), None) => s
                                .fields(db)
                                .data(db)
                                .get(field_ref.index)
                                .and_then(|f| f.type_ref.to_opt()),
                            (ItemKind::Enum(e), Some(v)) => {
                                match e.variants_list(db).data(db).get(v).map(|v| &v.kind) {
                                    Some(VariantKind::Tuple(tuple_id)) => tuple_id
                                        .data(db)
                                        .get(field_ref.index)
                                        .and_then(|t| t.to_opt()),
                                    Some(VariantKind::Record(fields_id)) => fields_id
                                        .data(db)
                                        .get(field_ref.index)
                                        .and_then(|f| f.type_ref.to_opt()),
                                    _ => None,
                                }
                            }
                            _ => None,
                        };
                        if let Some(ty) = resolved {
                            return bind_type_to_scope(db, ty, record_item);
                        }
                        ctxt.record_generation_error(
                            hole.syntax().text_range(),
                            "invalid field type splice",
                        );
                        TypeKind::Path(Partial::Absent)
                    }
                    _ => {
                        ctxt.record_generation_error(
                            hole.syntax().text_range(),
                            "type quotation hole requires Type<T> or FieldType<T, F> splice",
                        );
                        TypeKind::Path(Partial::Absent)
                    }
                }
            }

            ast::TypeKind::Ptr(ty) => {
                let inner = Self::lower_ast_partial(ctxt, ty.inner());
                if ty.star2().is_some() {
                    let inner = Partial::Present(TypeId::new(ctxt.db(), TypeKind::Ptr(inner)));
                    TypeKind::Ptr(inner)
                } else {
                    TypeKind::Ptr(inner)
                }
            }

            ast::TypeKind::Mode(ty) => {
                let mode = match ty.mode() {
                    Some(ast::TypeMode::Mut(_)) => TypeMode::Mut,
                    Some(ast::TypeMode::MaybeMut(_)) => TypeMode::MaybeMut,
                    Some(ast::TypeMode::Ref(_)) => TypeMode::Ref,
                    Some(ast::TypeMode::Own(_)) => TypeMode::Own,
                    None => TypeMode::Ref,
                };
                let inner = Self::lower_ast_partial(ctxt, ty.inner());
                TypeKind::Mode(mode, inner)
            }

            ast::TypeKind::Path(ty) => {
                let path = PathId::lower_ast_partial(ctxt, ty.path());
                TypeKind::Path(path)
            }

            ast::TypeKind::Tuple(ty) => TypeKind::Tuple(TupleTypeId::lower_ast(ctxt, ty)),

            ast::TypeKind::Array(ty) => {
                let elem_ty = Self::lower_ast_partial(ctxt, ty.elem_ty());
                let body = ty
                    .len()
                    .map(|ast| Body::lower_ast_nameless(ctxt, ast))
                    .into();
                TypeKind::Array(elem_ty, body)
            }

            ast::TypeKind::Never(_) => TypeKind::Never,
        };

        TypeId::new(ctxt.db(), kind)
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::Type>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}

impl<'db> TupleTypeId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TupleType) -> Self {
        let mut elem_tys = Vec::new();
        for elem in ast {
            elem_tys.push(Some(TypeId::lower_ast(ctxt, elem)).into());
        }
        TupleTypeId::new(ctxt.db(), elem_tys)
    }
}

impl<'db> TraitRefId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitRef) -> Self {
        let path = match ast.fn_shape() {
            // `Fn(own A, B) -> R` is `Fn<(own A, B), Out = R>`: the parameter
            // list is the shape argument, `-> _` leaves `Out` open, and no
            // result is `()`.
            Some(shape) => shape.name().zip(shape.params()).map(|(name, params)| {
                let db = ctxt.db();
                let params = TupleTypeId::lower_ast(ctxt, params).to_ty(db);
                let mut args = vec![GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(params),
                })];
                if !shape.infers_ret() {
                    let ret = shape.ret_ty().map_or_else(
                        || TupleTypeId::new(db, Vec::new()).to_ty(db),
                        |ty| TypeId::lower_ast(ctxt, ty),
                    );
                    args.push(GenericArg::AssocType(AssocTypeGenericArg {
                        name: Partial::Present(IdentId::new(db, "Out".to_string())),
                        mode: None,
                        ty: Partial::Present(ret),
                    }));
                }
                ctxt.core_path().push_str(db, "functional").push_str_args(
                    db,
                    name.text(),
                    GenericArgListId::new(db, args, true),
                )
            }),
            None => ast.path().map(|ast| PathId::lower_ast(ctxt, ast)),
        };
        Self::new(ctxt.db(), Partial::from(path))
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::TraitRef>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}

fn record_param_names<'db>(db: &'db dyn crate::HirDb, item: ItemKind<'db>) -> Vec<IdentId<'db>> {
    let params = match item {
        ItemKind::Struct(s) => s.generic_params(db),
        ItemKind::Enum(e) => e.generic_params(db),
        _ => return Vec::new(),
    };
    params
        .data(db)
        .iter()
        .filter_map(|p| p.name().to_opt())
        .collect()
}

pub(super) fn bind_path_to_scope<'db>(
    db: &'db dyn crate::HirDb,
    path: PathId<'db>,
    item: ItemKind<'db>,
    param_names: &[IdentId<'db>],
) -> PathId<'db> {
    let scope = ScopeId::Item(item);
    let parent = path
        .parent(db)
        .map(|p| bind_path_to_scope(db, p, item, param_names));
    match path.kind(db) {
        PathKind::Ident {
            ident,
            generic_args,
        } => {
            let generic_args = bind_generic_arg_list_to_scope(db, generic_args, item, param_names);
            if parent.is_none()
                && let Partial::Present(ident_id) = ident
            {
                if ident_id.is_ingot(db) {
                    return PathId::new(
                        db,
                        PathKind::BoundIngot {
                            ingot: scope.ingot(db),
                            display: ident_id,
                        },
                        None,
                    );
                }
                if !ident_id.is_self_ty(db) && !param_names.contains(&ident_id) {
                    let display = PathId::new(
                        db,
                        PathKind::Ident {
                            ident,
                            generic_args,
                        },
                        None,
                    );
                    return PathId::new(
                        db,
                        PathKind::InScope {
                            scope,
                            local_ident: Some(ident_id),
                            display,
                        },
                        None,
                    );
                }
            }
            PathId::new(
                db,
                PathKind::Ident {
                    ident,
                    generic_args,
                },
                parent,
            )
        }
        PathKind::QualifiedType { type_, trait_ } => {
            let type_ = bind_type_to_scope_with_params(db, type_, item, param_names);
            let trait_ = bind_trait_ref_to_scope(db, trait_, item, param_names);
            PathId::new(db, PathKind::QualifiedType { type_, trait_ }, parent)
        }
        PathKind::BoundIngot { ingot, display } => {
            PathId::new(db, PathKind::BoundIngot { ingot, display }, parent)
        }
        PathKind::BoundItem {
            item: bound_item,
            display,
        } => {
            let generic_args =
                bind_generic_arg_list_to_scope(db, display.generic_args(db), item, param_names);
            let display = display
                .with_generic_args(db, generic_args)
                .unwrap_or(display);
            PathId::new(
                db,
                PathKind::BoundItem {
                    item: bound_item,
                    display,
                },
                parent,
            )
        }
        PathKind::InScope {
            scope: existing_scope,
            local_ident,
            display,
        } => {
            let generic_args =
                bind_generic_arg_list_to_scope(db, display.generic_args(db), item, param_names);
            let display = display
                .with_generic_args(db, generic_args)
                .unwrap_or(display);
            PathId::new(
                db,
                PathKind::InScope {
                    scope: existing_scope,
                    local_ident,
                    display,
                },
                parent,
            )
        }
    }
}

fn bind_generic_arg_list_to_scope<'db>(
    db: &'db dyn crate::HirDb,
    args: GenericArgListId<'db>,
    item: ItemKind<'db>,
    param_names: &[IdentId<'db>],
) -> GenericArgListId<'db> {
    if !args.is_given(db) && args.data(db).is_empty() {
        return args;
    }
    let new_args: Vec<_> = args
        .data(db)
        .iter()
        .map(|arg| match arg {
            GenericArg::Type(ty_arg) => GenericArg::Type(TypeGenericArg {
                ty: ty_arg
                    .ty
                    .to_opt()
                    .map(|t| bind_type_to_scope_with_params(db, t, item, param_names))
                    .into(),
            }),
            GenericArg::Const(c) => GenericArg::Const(c.clone()),
            GenericArg::AssocType(assoc) => GenericArg::AssocType(AssocTypeGenericArg {
                name: assoc.name,
                ty: assoc
                    .ty
                    .to_opt()
                    .map(|t| bind_type_to_scope_with_params(db, t, item, param_names))
                    .into(),
            }),
        })
        .collect();
    GenericArgListId::new(db, new_args, args.is_given(db))
}

pub(super) fn bind_trait_ref_to_scope<'db>(
    db: &'db dyn crate::HirDb,
    trait_ref: TraitRefId<'db>,
    item: ItemKind<'db>,
    param_names: &[IdentId<'db>],
) -> TraitRefId<'db> {
    let path: Partial<PathId<'db>> = trait_ref
        .path(db)
        .to_opt()
        .map(|p| bind_path_to_scope(db, p, item, param_names))
        .into();
    TraitRefId::new(db, path)
}

pub(super) fn bind_type_to_scope<'db>(
    db: &'db dyn crate::HirDb,
    ty: TypeId<'db>,
    item: ItemKind<'db>,
) -> TypeId<'db> {
    let param_names = record_param_names(db, item);
    bind_type_to_scope_with_params(db, ty, item, &param_names)
}

fn bind_type_to_scope_with_params<'db>(
    db: &'db dyn crate::HirDb,
    ty: TypeId<'db>,
    item: ItemKind<'db>,
    param_names: &[IdentId<'db>],
) -> TypeId<'db> {
    let kind = match ty.data(db) {
        TypeKind::Ptr(inner) => TypeKind::Ptr(
            inner
                .to_opt()
                .map(|t| bind_type_to_scope_with_params(db, t, item, param_names))
                .into(),
        ),
        TypeKind::Mode(mode, inner) => TypeKind::Mode(
            *mode,
            inner
                .to_opt()
                .map(|t| bind_type_to_scope_with_params(db, t, item, param_names))
                .into(),
        ),
        TypeKind::Path(path) => TypeKind::Path(
            path.to_opt()
                .map(|p| bind_path_to_scope(db, p, item, param_names))
                .into(),
        ),
        TypeKind::Tuple(tuple_id) => {
            let elems: Vec<Partial<TypeId<'db>>> = tuple_id
                .data(db)
                .iter()
                .map(|elem| {
                    elem.to_opt()
                        .map(|t| bind_type_to_scope_with_params(db, t, item, param_names))
                        .into()
                })
                .collect();
            TypeKind::Tuple(TupleTypeId::new(db, elems))
        }
        TypeKind::Array(elem, len) => TypeKind::Array(
            elem.to_opt()
                .map(|t| bind_type_to_scope_with_params(db, t, item, param_names))
                .into(),
            *len,
        ),
        TypeKind::Primitive(p) => TypeKind::Primitive(*p),
        TypeKind::Never => TypeKind::Never,
    };
    TypeId::new(db, kind)
}

pub(super) fn bind_generic_param_to_scope<'db>(
    db: &'db dyn crate::HirDb,
    param: crate::core::hir_def::params::GenericParam<'db>,
    item: ItemKind<'db>,
) -> crate::core::hir_def::params::GenericParam<'db> {
    use crate::core::hir_def::params::{
        ConstGenericParam, GenericParam, TypeBound, TypeGenericParam,
    };
    let param_names = record_param_names(db, item);
    match param {
        GenericParam::Type(tp) => GenericParam::Type(TypeGenericParam {
            name: tp.name,
            bounds: tp
                .bounds
                .into_iter()
                .map(|b| match b {
                    TypeBound::Trait(tr, mode) => {
                        TypeBound::Trait(bind_trait_ref_to_scope(db, tr, item, &param_names), mode)
                    }
                    TypeBound::Kind(k) => TypeBound::Kind(k),
                })
                .collect(),
            default_ty: tp
                .default_ty
                .map(|t| bind_type_to_scope_with_params(db, t, item, &param_names)),
        }),
        GenericParam::Const(cp) => GenericParam::Const(ConstGenericParam {
            name: cp.name,
            ty: cp
                .ty
                .to_opt()
                .map(|t| bind_type_to_scope_with_params(db, t, item, &param_names))
                .into(),
            default: cp.default,
        }),
    }
}

pub(super) fn bind_where_predicate_to_scope<'db>(
    db: &'db dyn crate::HirDb,
    pred: crate::core::hir_def::params::WhereClausePredicate<'db>,
    item: ItemKind<'db>,
) -> crate::core::hir_def::params::WhereClausePredicate<'db> {
    use crate::core::hir_def::params::{TypeBound, WhereClausePredicate, WherePredicate};
    let param_names = record_param_names(db, item);
    match pred {
        WhereClausePredicate::Type(wp) => WhereClausePredicate::Type(WherePredicate {
            ty: wp
                .ty
                .to_opt()
                .map(|t| bind_type_to_scope_with_params(db, t, item, &param_names))
                .into(),
            bounds: wp
                .bounds
                .into_iter()
                .map(|b| match b {
                    TypeBound::Trait(tr, mode) => {
                        TypeBound::Trait(bind_trait_ref_to_scope(db, tr, item, &param_names), mode)
                    }
                    TypeBound::Kind(k) => TypeBound::Kind(k),
                })
                .collect(),
        }),
        WhereClausePredicate::Const(body) => WhereClausePredicate::Const(body),
    }
}
