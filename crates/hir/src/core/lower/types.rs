use common::ingot::IngotKind;
use parser::ast::{self};

use super::FileLowerCtxt;
use crate::core::hir_def::{
    AssocTypeGenericArg, Body, GenericArg, GenericArgListId, IdentId, Partial, PathId, TraitRefId,
    TupleTypeId, TypeGenericArg, TypeId, TypeKind, TypeMode,
};

impl<'db> TypeId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Type) -> Self {
        let kind = match ast.kind() {
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
