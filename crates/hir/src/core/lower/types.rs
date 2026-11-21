use common::ingot::IngotKind;
use parser::ast::{self, prelude::*};

use super::FileLowerCtxt;
use crate::core::hir_def::{
    AssocTypeGenericArg, AuthoredFieldType, AuthoredScope, Body, Expr, GenericArg,
    GenericArgListId, IdentId, ItemKind, NodeStore, Partial, Pat, PathId, PathKind, Stmt,
    TrackedItemVariant, TraitRefId, TupleTypeId, TypeGenericArg, TypeId, TypeKind, TypeMode,
    WithBinding, params::ConstGenericArg,
};

impl<'db> TypeId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Type) -> Self {
        let kind = match ast.kind() {
            ast::TypeKind::QuoteHole(hole) => {
                let db = ctxt.db();
                match ctxt.current_hole(hole.syntax().text_range()) {
                    Some(common::generation::FrozenHoleValue::Record(ref record_ref))
                        if record_ref.variant_index.is_none()
                            && let Some(args) = &record_ref.args =>
                    {
                        // The record's actual arguments.
                        let record = ctxt.authored_record(record_ref);
                        let generic_args = GenericArgListId::thaw(ctxt, args);
                        TypeKind::Path(Partial::Present(record.path(db, generic_args)))
                    }
                    Some(common::generation::FrozenHoleValue::Type(frozen)) => {
                        return TypeId::thaw(ctxt, &frozen);
                    }
                    Some(common::generation::FrozenHoleValue::Record(record_ref))
                        if record_ref.variant_index.is_none() =>
                    {
                        // The record over its own parameters, which the generated
                        // item declares.
                        let record = ctxt.authored_record(&record_ref);
                        let params: Vec<_> = record
                            .generic_params()
                            .into_iter()
                            .flatten()
                            .map(|param| {
                                let name = match param.kind() {
                                    ast::GenericParamKind::Type(p) => p.name(),
                                    ast::GenericParamKind::Const(p) => p.name(),
                                    ast::GenericParamKind::Shape(p) => p.name(),
                                }
                                .map(|name| IdentId::new(db, name.text().to_string()));
                                (
                                    name,
                                    matches!(param.kind(), ast::GenericParamKind::Const(_)),
                                )
                            })
                            .collect();
                        let generic_args = if params.is_empty() {
                            GenericArgListId::none(db)
                        } else {
                            let args: Vec<_> = params
                                .into_iter()
                                .map(|(name, is_const)| {
                                    if is_const {
                                        GenericArg::Const(crate::hir_def::params::ConstGenericArg {
                                            value: name
                                                .map(|name| {
                                                    Body::lower_const_param_name(ctxt, name)
                                                })
                                                .into(),
                                        })
                                    } else {
                                        let ty = name.map(|ident| {
                                            TypeId::new(
                                                db,
                                                TypeKind::Path(Partial::Present(
                                                    PathId::from_ident(db, ident),
                                                )),
                                            )
                                        });
                                        GenericArg::Type(TypeGenericArg { ty: ty.into() })
                                    }
                                })
                                .collect();
                            GenericArgListId::new(db, args, true)
                        };
                        TypeKind::Path(Partial::Present(record.path(db, generic_args)))
                    }
                    Some(common::generation::FrozenHoleValue::FieldType(field_ref)) => {
                        let record = ctxt.authored_record(&field_ref.record);
                        let variant = field_ref.record.variant_index;
                        if record.field_has_type(variant, field_ref.index) {
                            return TypeId::new(
                                db,
                                TypeKind::AuthoredField(AuthoredFieldType::new(
                                    db,
                                    record.scope,
                                    variant,
                                    field_ref.index,
                                )),
                            );
                        }
                        ctxt.record_generation_error(
                            hole.syntax().text_range(),
                            "invalid field type splice",
                        );
                        TypeKind::Path(Partial::Absent)
                    }
                    Some(common::generation::FrozenHoleValue::FieldTypes { record, count }) => {
                        let authored = ctxt.authored_record(&record);
                        let source_count = match authored.fields(db, record.variant_index) {
                            Some(super::AuthoredFields::Unit) => Some(0),
                            Some(super::AuthoredFields::Tuple(count)) => Some(count),
                            Some(super::AuthoredFields::Named(fields)) => Some(fields.len()),
                            None => None,
                        };
                        if source_count != Some(count) {
                            ctxt.record_generation_error(
                                hole.syntax().text_range(),
                                "invalid field type tree splice",
                            );
                            TypeKind::Path(Partial::Absent)
                        } else {
                            let fields: Vec<_> = (0..count)
                                .map(|index| {
                                    Partial::Present(TypeId::new(
                                        db,
                                        TypeKind::AuthoredField(AuthoredFieldType::new(
                                            db,
                                            authored.scope,
                                            record.variant_index,
                                            index,
                                        )),
                                    ))
                                })
                                .collect();
                            return lower_field_type_tree(db, &fields);
                        }
                    }
                    _ => {
                        ctxt.record_generation_error(
                            hole.syntax().text_range(),
                            "type quotation hole requires a record, field type, or checked type reference",
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
            ast::TypeKind::TraitFamily(family) => {
                TypeKind::TraitFamily(TraitRefId::lower_ast_partial(ctxt, family.trait_ref()))
            }
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

fn lower_field_type_tree<'db>(
    db: &'db dyn crate::HirDb,
    fields: &[Partial<TypeId<'db>>],
) -> TypeId<'db> {
    let elements = match fields {
        [] | [_] => fields.to_vec(),
        _ => {
            let split = crate::analysis::ty::field_tree_split(fields.len());
            let left = lower_field_type_tree(db, &fields[..split]);
            let right = lower_field_type_tree(db, &fields[split..]);
            vec![Partial::Present(left), Partial::Present(right)]
        }
    };
    TupleTypeId::new(db, elements).to_ty(db)
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
                ctxt.builtin_ingot_path(IngotKind::Core, "core")
                    .push_str(db, "functional")
                    .push_str_args(db, name.text(), GenericArgListId::new(db, args, true))
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

/// How binding a copied declaration's types into another scope treats the
/// anonymous constant bodies they contain: array lengths, constant generic
/// arguments and defaults, and constant where conditions.
pub(super) trait BindBodies<'db> {
    fn db(&self) -> &'db dyn crate::HirDb;

    fn bind_body(
        &mut self,
        body: Body<'db>,
        scope: AuthoredScope<'db>,
        param_names: &[IdentId<'db>],
    ) -> Body<'db>;
}

/// Lowering copies each body into the scope being lowered, with its names
/// bound as the declaration's other names are (`bind_body_to_scope`).
impl<'db> BindBodies<'db> for FileLowerCtxt<'db> {
    fn db(&self) -> &'db dyn crate::HirDb {
        FileLowerCtxt::db(self)
    }

    fn bind_body(
        &mut self,
        body: Body<'db>,
        scope: AuthoredScope<'db>,
        param_names: &[IdentId<'db>],
    ) -> Body<'db> {
        bind_body_to_scope(self, body, scope, param_names)
    }
}

/// A spliced field type is bound after lowering, when no body can be added to
/// the module that receives it. Its bodies stay in the field's record, and type
/// lowering reads them there with the record's parameters named where the type
/// is spliced (`ty_lower::lower_opt_const_body`).
struct KeepBodies<'db>(&'db dyn crate::HirDb);

impl<'db> BindBodies<'db> for KeepBodies<'db> {
    fn db(&self) -> &'db dyn crate::HirDb {
        self.0
    }

    fn bind_body(
        &mut self,
        body: Body<'db>,
        _scope: AuthoredScope<'db>,
        _param_names: &[IdentId<'db>],
    ) -> Body<'db> {
        body
    }
}

pub(super) fn bind_path_to_scope<'db, B: BindBodies<'db>>(
    bodies: &mut B,
    path: PathId<'db>,
    scope: AuthoredScope<'db>,
    param_names: &[IdentId<'db>],
) -> PathId<'db> {
    let db = bodies.db();
    let parent = path
        .parent(db)
        .map(|p| bind_path_to_scope(bodies, p, scope, param_names));
    match path.kind(db) {
        PathKind::Ident {
            ident,
            generic_args,
        } => {
            let generic_args =
                bind_generic_arg_list_to_scope(bodies, generic_args, scope, param_names);
            if parent.is_none()
                && let Partial::Present(ident_id) = ident
            {
                if ident_id.is_ingot(db) {
                    return PathId::new(
                        db,
                        PathKind::BoundIngot {
                            scope,
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
            PathId::intern(
                db,
                PathKind::Ident {
                    ident,
                    generic_args,
                },
                parent,
                path.local_quote_origin(db),
            )
        }
        PathKind::QualifiedType { type_, trait_ } => {
            let type_ = bind_type_to_scope_with_params(bodies, type_, scope, param_names);
            let trait_ = bind_trait_ref_to_scope(bodies, trait_, scope, param_names);
            PathId::new(db, PathKind::QualifiedType { type_, trait_ }, parent)
        }
        PathKind::BoundIngot { scope, display } => {
            PathId::new(db, PathKind::BoundIngot { scope, display }, parent)
        }
        PathKind::BoundItem {
            item: bound_item,
            display,
        } => {
            let generic_args = bind_generic_arg_list_to_scope(
                bodies,
                display.generic_args(db),
                scope,
                param_names,
            );
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
            let generic_args = bind_generic_arg_list_to_scope(
                bodies,
                display.generic_args(db),
                scope,
                param_names,
            );
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

fn bind_generic_arg_list_to_scope<'db, B: BindBodies<'db>>(
    bodies: &mut B,
    args: GenericArgListId<'db>,
    scope: AuthoredScope<'db>,
    param_names: &[IdentId<'db>],
) -> GenericArgListId<'db> {
    let db = bodies.db();
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
                    .map(|t| bind_type_to_scope_with_params(bodies, t, scope, param_names))
                    .into(),
            }),
            GenericArg::Const(c) => GenericArg::Const(ConstGenericArg {
                value: c
                    .value
                    .to_opt()
                    .map(|b| bodies.bind_body(b, scope, param_names))
                    .into(),
            }),
            GenericArg::Mode(mode) => GenericArg::Mode(*mode),
            GenericArg::AssocType(assoc) => GenericArg::AssocType(AssocTypeGenericArg {
                name: assoc.name,
                mode: assoc.mode,
                ty: assoc
                    .ty
                    .to_opt()
                    .map(|t| bind_type_to_scope_with_params(bodies, t, scope, param_names))
                    .into(),
            }),
            GenericArg::Infer => GenericArg::Infer,
        })
        .collect();
    GenericArgListId::new(db, new_args, args.is_given(db))
}

pub(super) fn bind_trait_ref_to_scope<'db, B: BindBodies<'db>>(
    bodies: &mut B,
    trait_ref: TraitRefId<'db>,
    scope: AuthoredScope<'db>,
    param_names: &[IdentId<'db>],
) -> TraitRefId<'db> {
    let db = bodies.db();
    let path: Partial<PathId<'db>> = trait_ref
        .path(db)
        .to_opt()
        .map(|p| bind_path_to_scope(bodies, p, scope, param_names))
        .into();
    TraitRefId::new(db, path)
}

/// A record field's declared type as a type splice copies it, bound after
/// lowering: its names in the record's scope, the record's generic parameters
/// left to the use site.
pub(crate) fn bind_type_to_scope<'db>(
    db: &'db dyn crate::HirDb,
    ty: TypeId<'db>,
    scope: AuthoredScope<'db>,
    item: ItemKind<'db>,
) -> TypeId<'db> {
    let param_names = record_param_names(db, item);
    bind_type_to_scope_with_params(&mut KeepBodies(db), ty, scope, &param_names)
}

fn bind_type_to_scope_with_params<'db, B: BindBodies<'db>>(
    bodies: &mut B,
    ty: TypeId<'db>,
    scope: AuthoredScope<'db>,
    param_names: &[IdentId<'db>],
) -> TypeId<'db> {
    let db = bodies.db();
    let kind = match ty.data(db).clone() {
        TypeKind::Ptr(inner) => TypeKind::Ptr(
            inner
                .to_opt()
                .map(|t| bind_type_to_scope_with_params(bodies, t, scope, param_names))
                .into(),
        ),
        TypeKind::Mode(mode, inner) => TypeKind::Mode(
            mode,
            inner
                .to_opt()
                .map(|t| bind_type_to_scope_with_params(bodies, t, scope, param_names))
                .into(),
        ),
        TypeKind::Path(path) => TypeKind::Path(
            path.to_opt()
                .map(|p| bind_path_to_scope(bodies, p, scope, param_names))
                .into(),
        ),
        TypeKind::Tuple(tuple_id) => {
            let elems: Vec<Partial<TypeId<'db>>> = tuple_id
                .data(db)
                .iter()
                .map(|elem| {
                    elem.to_opt()
                        .map(|t| bind_type_to_scope_with_params(bodies, t, scope, param_names))
                        .into()
                })
                .collect();
            TypeKind::Tuple(TupleTypeId::new(db, elems))
        }
        TypeKind::Array(elem, len) => TypeKind::Array(
            elem.to_opt()
                .map(|t| bind_type_to_scope_with_params(bodies, t, scope, param_names))
                .into(),
            len.to_opt()
                .map(|b| bodies.bind_body(b, scope, param_names))
                .into(),
        ),
        TypeKind::Primitive(p, args) => TypeKind::Primitive(
            p,
            bind_generic_arg_list_to_scope(bodies, args, scope, param_names),
        ),
        TypeKind::Never => TypeKind::Never,
        TypeKind::TraitFamily(reference) => TypeKind::TraitFamily(
            reference
                .to_opt()
                .map(|reference| bind_trait_ref_to_scope(bodies, reference, scope, param_names))
                .into(),
        ),
        // Already bound in its record's declaration.
        TypeKind::AuthoredField(field) => TypeKind::AuthoredField(field),
    };
    TypeId::new(db, kind)
}

fn bind_body_to_scope<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    body: Body<'db>,
    scope: AuthoredScope<'db>,
    param_names: &[IdentId<'db>],
) -> Body<'db> {
    let db = ctxt.db();
    let id = ctxt.joined_id(TrackedItemVariant::NamelessBody);
    let body_top_mod = body.top_mod(db);
    let quote_top_mod = (body_top_mod != ctxt.top_mod()).then_some(body_top_mod);
    ctxt.enter_body_scope_with_top_mod(id, quote_top_mod);

    let mut local_and_param_names = param_names.to_vec();
    for (_, pat) in body.pats(db).iter() {
        if let Partial::Present(Pat::Path(Partial::Present(path), _)) = pat
            && let Some(ident) = path.as_ident(db)
            && !local_and_param_names.contains(&ident)
        {
            local_and_param_names.push(ident);
        }
    }

    let mut pats = NodeStore::new();
    for (_, pat) in body.pats(db).iter() {
        let rebound = match pat {
            Partial::Present(Pat::Path(Partial::Present(path), marker))
                if path.parent(db).is_some() =>
            {
                Partial::Present(Pat::Path(
                    Partial::Present(bind_path_to_scope(
                        ctxt,
                        *path,
                        scope,
                        &local_and_param_names,
                    )),
                    *marker,
                ))
            }
            Partial::Present(Pat::PathTuple(path, elems)) => Partial::Present(Pat::PathTuple(
                path.to_opt()
                    .map(|p| bind_path_to_scope(ctxt, p, scope, &local_and_param_names))
                    .into(),
                elems.clone(),
            )),
            Partial::Present(Pat::Record(path, fields)) => Partial::Present(Pat::Record(
                path.to_opt()
                    .map(|p| bind_path_to_scope(ctxt, p, scope, &local_and_param_names))
                    .into(),
                fields.clone(),
            )),
            other => other.clone(),
        };
        pats.push(rebound);
    }

    let mut stmts = NodeStore::new();
    for (_, stmt) in body.stmts(db).iter() {
        let rebound = match stmt {
            Partial::Present(Stmt::Let(pat, ty, init, else_expr)) => Partial::Present(Stmt::Let(
                *pat,
                ty.map(|t| bind_type_to_scope_with_params(ctxt, t, scope, &local_and_param_names)),
                *init,
                *else_expr,
            )),
            other => other.clone(),
        };
        stmts.push(rebound);
    }

    let mut exprs = NodeStore::new();
    for (expr_id, expr) in body.exprs(db).iter() {
        if matches!(expr, Partial::Present(Expr::Block(..))) {
            ctxt.enter_block_scope();
            ctxt.leave_block_scope(expr_id);
        }
        let rebound = match expr {
            Partial::Present(Expr::Path(path)) => Partial::Present(Expr::Path(
                path.to_opt()
                    .map(|p| bind_path_to_scope(ctxt, p, scope, &local_and_param_names))
                    .into(),
            )),
            Partial::Present(Expr::RecordInit(path, fields)) => Partial::Present(Expr::RecordInit(
                path.to_opt()
                    .map(|p| bind_path_to_scope(ctxt, p, scope, &local_and_param_names))
                    .into(),
                fields.clone(),
            )),
            Partial::Present(Expr::MethodCall(recv, name, generic_args, args)) => {
                Partial::Present(Expr::MethodCall(
                    *recv,
                    *name,
                    bind_generic_arg_list_to_scope(
                        ctxt,
                        *generic_args,
                        scope,
                        &local_and_param_names,
                    ),
                    args.clone(),
                ))
            }
            Partial::Present(Expr::Cast(inner, ty)) => Partial::Present(Expr::Cast(
                *inner,
                ty.to_opt()
                    .map(|t| bind_type_to_scope_with_params(ctxt, t, scope, &local_and_param_names))
                    .into(),
            )),
            Partial::Present(Expr::ArrayRep(val, len)) => Partial::Present(Expr::ArrayRep(
                *val,
                len.to_opt()
                    .map(|b| bind_body_to_scope(ctxt, b, scope, &local_and_param_names))
                    .into(),
            )),
            Partial::Present(Expr::Closure {
                params,
                ret_ty,
                body: closure_body,
            }) => Partial::Present(Expr::Closure {
                params: *params,
                ret_ty: ret_ty.map(|t| {
                    bind_type_to_scope_with_params(ctxt, t, scope, &local_and_param_names)
                }),
                body: *closure_body,
            }),
            Partial::Present(Expr::With(bindings, inner)) => Partial::Present(Expr::With(
                bindings
                    .iter()
                    .map(|b| WithBinding {
                        key_path: b.key_path.map(|kp| {
                            kp.to_opt()
                                .map(|p| bind_path_to_scope(ctxt, p, scope, &local_and_param_names))
                                .into()
                        }),
                        value: b.value,
                    })
                    .collect(),
                *inner,
            )),
            other => other.clone(),
        };
        exprs.push(rebound);
    }

    let new_body = Body::new(
        db,
        id,
        body.expr(db),
        body.body_kind(db),
        stmts,
        exprs,
        body.conds(db).clone(),
        pats,
        ctxt.top_mod(),
        ctxt.lazy_func(),
        // The copy keeps the original's source map, origin and quotations; its
        // expressions keep their ids.
        body.quote_origins(db).clone(),
        body.source_map(db).clone(),
        body.origin(db).clone(),
    );
    ctxt.leave_item_scope(new_body);
    new_body
}

pub(super) fn bind_generic_param_to_scope<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    param: crate::core::hir_def::params::GenericParam<'db>,
    scope: AuthoredScope<'db>,
    item: ItemKind<'db>,
) -> crate::core::hir_def::params::GenericParam<'db> {
    use crate::core::hir_def::params::{
        ConstGenericParam, GenericParam, TypeBound, TypeGenericParam,
    };
    let param_names = record_param_names(ctxt.db(), item);
    match param {
        GenericParam::Type(tp) => GenericParam::Type(TypeGenericParam {
            name: tp.name,
            bounds: tp
                .bounds
                .into_iter()
                .map(|b| match b {
                    TypeBound::Trait(tr, mode) => TypeBound::Trait(
                        bind_trait_ref_to_scope(ctxt, tr, scope, &param_names),
                        mode,
                    ),
                    TypeBound::Kind(k) => TypeBound::Kind(k),
                })
                .collect(),
            default_ty: tp
                .default_ty
                .map(|t| bind_type_to_scope_with_params(ctxt, t, scope, &param_names)),
        }),
        GenericParam::Const(cp) => GenericParam::Const(ConstGenericParam {
            name: cp.name,
            ty: cp
                .ty
                .to_opt()
                .map(|t| bind_type_to_scope_with_params(ctxt, t, scope, &param_names))
                .into(),
            default: cp
                .default
                .map(|b| bind_body_to_scope(ctxt, b, scope, &param_names)),
        }),
    }
}

pub(super) fn bind_where_predicate_to_scope<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    pred: crate::core::hir_def::params::WhereClausePredicate<'db>,
    scope: AuthoredScope<'db>,
    item: ItemKind<'db>,
) -> crate::core::hir_def::params::WhereClausePredicate<'db> {
    use crate::core::hir_def::params::{TypeBound, WhereClausePredicate, WherePredicate};
    let param_names = record_param_names(ctxt.db(), item);
    match pred {
        WhereClausePredicate::Type(wp) => WhereClausePredicate::Type(WherePredicate {
            ty: wp
                .ty
                .to_opt()
                .map(|t| bind_type_to_scope_with_params(ctxt, t, scope, &param_names))
                .into(),
            bounds: wp
                .bounds
                .into_iter()
                .map(|b| match b {
                    TypeBound::Trait(tr, mode) => TypeBound::Trait(
                        bind_trait_ref_to_scope(ctxt, tr, scope, &param_names),
                        mode,
                    ),
                    TypeBound::Kind(k) => TypeBound::Kind(k),
                })
                .collect(),
        }),
        WhereClausePredicate::Const(body) => {
            WhereClausePredicate::Const(bind_body_to_scope(ctxt, body, scope, &param_names))
        }
    }
}

impl<'db> TypeId<'db> {
    /// A ground type frozen in the authored view, rebound in this view by the
    /// same declarations: never by name lookup at the place it is spliced.
    pub(super) fn thaw(
        ctxt: &mut FileLowerCtxt<'db>,
        frozen: &common::generation::FrozenTy,
    ) -> Self {
        use common::generation::FrozenTy;
        let db = ctxt.db();
        let kind = match frozen {
            FrozenTy::Declared { decl, args } => {
                let args = GenericArgListId::thaw(ctxt, args);
                TypeKind::Path(Partial::Present(ctxt.authored_decl_path(decl, args)))
            }
            FrozenTy::Primitive { name, args } => {
                let primitive =
                    crate::hir_def::prim_ty::PrimTy::from_name(db, IdentId::new(db, name.clone()))
                        .expect("validated frozen primitive");
                TypeKind::Primitive(primitive, GenericArgListId::thaw(ctxt, args))
            }
            FrozenTy::Tuple(elems) => {
                let elems = elems
                    .iter()
                    .map(|elem| Partial::Present(Self::thaw(ctxt, elem)))
                    .collect::<Vec<_>>();
                TypeKind::Tuple(TupleTypeId::new(db, elems))
            }
            FrozenTy::Array { elem, len } => TypeKind::Array(
                Partial::Present(Self::thaw(ctxt, elem)),
                Partial::Present(Body::thaw_const(ctxt, len)),
            ),
            FrozenTy::Ptr(pointee) => TypeKind::Ptr(Partial::Present(Self::thaw(ctxt, pointee))),
            FrozenTy::Never => TypeKind::Never,
            FrozenTy::HeaderParam(index) => TypeKind::Path(ctxt.header_param_path(*index)),
        };
        Self::new(db, kind)
    }
}

impl<'db> GenericArgListId<'db> {
    pub(super) fn thaw(
        ctxt: &mut FileLowerCtxt<'db>,
        args: &[common::generation::FrozenGenericArg],
    ) -> Self {
        use common::generation::FrozenGenericArg;
        let db = ctxt.db();
        if args.is_empty() {
            return Self::none(db);
        }
        let args = args
            .iter()
            .map(|arg| match arg {
                FrozenGenericArg::Type(ty) => GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(TypeId::thaw(ctxt, ty)),
                }),
                FrozenGenericArg::Const(value) => {
                    GenericArg::Const(crate::hir_def::params::ConstGenericArg {
                        value: Partial::Present(Body::thaw_const(ctxt, value)),
                    })
                }
            })
            .collect::<Vec<_>>();
        Self::new(db, args, true)
    }
}

impl<'db> Body<'db> {
    fn thaw_const(ctxt: &mut FileLowerCtxt<'db>, frozen: &common::generation::FrozenConst) -> Self {
        use common::generation::FrozenConst;
        match frozen {
            FrozenConst::Value(value) => Self::lower_frozen_scalar(ctxt, value),
            FrozenConst::HeaderParam(index) => {
                let path = ctxt.header_param_path(*index);
                Self::lower_bound_path(ctxt, path)
            }
        }
    }
}
