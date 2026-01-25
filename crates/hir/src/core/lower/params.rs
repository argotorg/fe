use parser::ast::{self};

use super::FileLowerCtxt;
use crate::core::hir_def::{Body, IdentId, Partial, TypeId, params::*};

impl<'db> GenericArgListId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::GenericArgList) -> Self {
        let args = ast
            .into_iter()
            .map(|arg| GenericArg::lower_ast(ctxt, arg))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), args, true)
    }

    pub(super) fn lower_ast_opt(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::GenericArgList>,
    ) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::none(ctxt.db()))
    }
}

impl<'db> GenericParamListId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::GenericParamList) -> Self {
        let params = ast
            .into_iter()
            .map(|param| GenericParam::lower_ast(ctxt, param))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), params)
    }

    pub(super) fn lower_ast_opt(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::GenericParamList>,
    ) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::new(ctxt.db(), Vec::new()))
    }
}

impl<'db> FuncParamListId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::FuncParamList) -> Self {
        let params = ast
            .into_iter()
            .map(|param| FuncParam::lower_ast(ctxt, param))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), params)
    }
}

impl<'db> WhereClauseId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::WhereClause) -> Self {
        let predicates = ast
            .into_iter()
            .map(|pred| WherePredicate::lower_ast(ctxt, pred))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), predicates)
    }

    pub(super) fn lower_ast_opt(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::WhereClause>,
    ) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::new(ctxt.db(), Vec::new()))
    }
}

impl<'db> TypeGenericParam<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TypeGenericParam) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let bounds = ast
            .bounds()
            .map(|bounds| {
                bounds
                    .into_iter()
                    .map(|bound| TypeBound::lower_ast(ctxt, bound))
                    .collect()
            })
            .unwrap_or_default();
        let default_ty = ast.default_ty().map(|ty| TypeId::lower_ast(ctxt, ty));

        Self {
            name,
            bounds,
            default_ty,
        }
    }
}

impl<'db> ConstGenericParam<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::ConstGenericParam) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        Self { name, ty }
    }
}

impl<'db> GenericArg<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::GenericArg) -> Self {
        match ast.kind() {
            ast::GenericArgKind::Type(type_param) => {
                TypeGenericArg::lower_ast(ctxt, type_param).into()
            }
            ast::GenericArgKind::Const(const_param) => {
                ConstGenericArg::lower_ast(ctxt, const_param).into()
            }
            ast::GenericArgKind::AssocType(assoc_type_param) => {
                AssocTypeGenericArg::lower_ast(ctxt, assoc_type_param).into()
            }
        }
    }
}

impl<'db> TypeGenericArg<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TypeGenericArg) -> Self {
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        Self { ty }
    }
}

impl<'db> ConstGenericArg<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::ConstGenericArg) -> Self {
        let body = ast
            .expr()
            .map(|expr| Body::lower_ast_nameless(ctxt, expr))
            .into();

        Self { body }
    }
}

impl<'db> AssocTypeGenericArg<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::AssocTypeGenericArg) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        Self { name, ty }
    }
}

impl<'db> GenericParam<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::GenericParam) -> Self {
        match ast.kind() {
            ast::GenericParamKind::Type(type_param) => {
                TypeGenericParam::lower_ast(ctxt, type_param).into()
            }
            ast::GenericParamKind::Const(const_param) => {
                ConstGenericParam::lower_ast(ctxt, const_param).into()
            }
        }
    }
}

impl<'db> FuncParam<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::FuncParam) -> Self {
        let is_mut = ast.mut_token().is_some();
        let is_label_suppressed = ast.is_label_suppressed();
        let name = ast.name().map(|ast| FuncParamName::lower_ast(ctxt, ast));

        let self_ty_fallback =
            name.is_some_and(|name| name.is_self(ctxt.db())) && ast.ty().is_none();

        let ty = if self_ty_fallback {
            Partial::Present(TypeId::fallback_self_ty(ctxt.db()))
        } else {
            TypeId::lower_ast_partial(ctxt, ast.ty())
        };

        Self {
            is_mut,
            is_label_suppressed,
            name: name.into(),
            ty,
            self_ty_fallback,
        }
    }
}

impl<'db> WherePredicate<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::WherePredicate) -> Self {
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let bounds = ast
            .bounds()
            .map(|bounds| {
                bounds
                    .into_iter()
                    .map(|bound| TypeBound::lower_ast(ctxt, bound))
                    .collect()
            })
            .unwrap_or_default();
        Self { ty, bounds }
    }
}

impl<'db> TypeBound<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TypeBound) -> Self {
        if let Some(trait_bound) = ast.trait_bound() {
            Self::Trait(TraitRefId::lower_ast(ctxt, trait_bound))
        } else {
            Self::Kind(KindBound::lower_ast_opt(ctxt, ast.kind_bound()))
        }
    }
}

impl KindBound {
    fn lower_ast_opt(_ctxt: &mut FileLowerCtxt<'_>, ast: Option<ast::KindBound>) -> Partial<Self> {
        let Some(ast) = ast else {
            return Partial::Absent;
        };

        if let Some(abs) = ast.abs() {
            let lhs = KindBound::lower_ast_opt(_ctxt, abs.lhs())
                .to_opt()
                .map(Box::new)
                .into();

            let rhs = KindBound::lower_ast_opt(_ctxt, abs.rhs())
                .to_opt()
                .map(Box::new)
                .into();

            Partial::Present(KindBound::Abs(lhs, rhs))
        } else if ast.mono().is_some() {
            Partial::Present(KindBound::Mono)
        } else {
            Partial::Absent
        }
    }
}

impl<'db> FuncParamName<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::FuncParamName) -> Self {
        match ast {
            ast::FuncParamName::Ident(name) => {
                FuncParamName::Ident(IdentId::lower_token(ctxt, name))
            }
            ast::FuncParamName::SelfParam(_) => FuncParamName::Ident(IdentId::make_self(ctxt.db())),
            ast::FuncParamName::Underscore(_) => FuncParamName::Underscore,
        }
    }
}
