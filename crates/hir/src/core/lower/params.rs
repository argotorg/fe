use parser::{SyntaxKind, ast};

use super::FileLowerCtxt;
use crate::core::hir_def::{
    Body, BodyKind, IdentId, Partial, TrackedItemVariant, TypeId, TypeKind, TypeMode, params::*,
};

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
            .enumerate()
            .map(|(idx, param)| FuncParam::lower_ast(ctxt, param, idx))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), params)
    }
}

impl<'db> WhereClauseId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::WhereClause) -> Self {
        let mut const_idx = 0;
        let mut predicates = Vec::new();
        Self::lower_ast_into(ctxt, ast, &mut const_idx, &mut predicates);
        Self::new(ctxt.db(), predicates)
    }

    fn lower_ast_into(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: ast::WhereClause,
        const_idx: &mut usize,
        out: &mut Vec<WhereClausePredicate<'db>>,
    ) {
        for pred in ast.iter() {
            match pred.kind() {
                ast::WherePredicateKind::Type(pred) => {
                    out.push(WhereClausePredicate::Type(WherePredicate::lower_ast(
                        ctxt, pred,
                    )));
                }
                ast::WherePredicateKind::Const(pred) => {
                    let body = Body::lower_ast_with_variant(
                        ctxt,
                        pred.expr(),
                        None,
                        TrackedItemVariant::WhereConstPredicate(*const_idx as u32),
                        BodyKind::Anonymous,
                    );
                    *const_idx += 1;
                    out.push(WhereClausePredicate::Const(body));
                }
                ast::WherePredicateKind::Hole(hole) => {
                    use parser::ast::prelude::*;
                    if let Some(common::generation::FrozenHoleValue::Code(code)) =
                        ctxt.current_hole(hole.syntax().text_range())
                    {
                        Self::lower_code_into(
                            ctxt,
                            &code,
                            hole.syntax().text_range(),
                            const_idx,
                            out,
                        );
                    } else {
                        ctxt.record_generation_error(
                            hole.syntax().text_range(),
                            "where-clause quotation hole requires Code<Where> splice",
                        );
                    }
                }
            }
        }
    }

    fn lower_code_into(
        ctxt: &mut FileLowerCtxt<'db>,
        code: &common::generation::FrozenCode,
        origin_range: parser::TextRange,
        const_idx: &mut usize,
        out: &mut Vec<WhereClausePredicate<'db>>,
    ) {
        use common::generation::{FrozenCode, FrozenCodeKind, FrozenHoleValue};
        use parser::ast::prelude::*;
        match code {
            FrozenCode::Empty { .. } => {}
            FrozenCode::Template {
                kind: FrozenCodeKind::Where,
                quote_file,
                quote_range,
                producer_module,
                producer_block,
                holes,
            } => {
                let (quote, frame) = ctxt.resolve_quote_template(
                    quote_file,
                    *quote_range,
                    *producer_module,
                    *producer_block,
                    true,
                    holes.clone(),
                );
                for (name, range) in &frame.open_names {
                    ctxt.file_scoped_error(
                        frame.quote_top_mod.file(ctxt.db()),
                        *range,
                        format!("quoted open name `{name}` has no enclosing binder"),
                    );
                }
                ctxt.quote_frames.push(frame);
                if let Some(where_clause) = quote.where_clause().and_then(|w| w.clause()) {
                    Self::lower_ast_into(ctxt, where_clause, const_idx, out);
                } else if let Some(block) = quote.body() {
                    for stmt in block.stmts() {
                        if let ast::StmtKind::Expr(expr_stmt) = stmt.kind()
                            && let Some(expr) = expr_stmt.expr()
                            && let ast::ExprKind::QuoteHole(hole) = expr.kind()
                            && let Some(FrozenHoleValue::Code(nested)) =
                                ctxt.current_hole(hole.syntax().text_range())
                        {
                            Self::lower_code_into(
                                ctxt,
                                &nested,
                                hole.syntax().text_range(),
                                const_idx,
                                out,
                            );
                        } else {
                            ctxt.record_generation_error(
                                stmt.syntax().text_range(),
                                "where-clause quotation requires where predicates",
                            );
                        }
                    }
                } else {
                    ctxt.record_generation_error(
                        *quote_range,
                        "where-clause quotation requires where predicates",
                    );
                }
                ctxt.quote_frames.pop();
            }
            _ => {
                ctxt.record_generation_error(
                    origin_range,
                    "where-clause quotation hole requires Code<Where> splice",
                );
            }
        }
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
        let default = ast
            .default_expr()
            .map(|expr| Body::lower_ast_nameless(ctxt, expr));
        Self { name, ty, default }
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
            ast::GenericArgKind::Mode(mode) => Self::Mode(if mode.is_mut() {
                TypeMode::Mut
            } else {
                TypeMode::Ref
            }),
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
        let value = ast
            .expr()
            .map(|expr| Body::lower_ast_nameless(ctxt, expr))
            .into();

        Self { value }
    }
}

impl<'db> AssocTypeGenericArg<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::AssocTypeGenericArg) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let mode = ast.mode().map(|mode| {
            if mode.is_mut() {
                TypeMode::Mut
            } else {
                TypeMode::Ref
            }
        });
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        Self { name, mode, ty }
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
            // A type parameter of the `shape` kind.
            ast::GenericParamKind::Shape(shape_param) => TypeGenericParam {
                name: IdentId::lower_token_partial(ctxt, shape_param.name()),
                bounds: vec![TypeBound::Kind(Partial::Present(KindBound::Shape))],
                default_ty: None,
            }
            .into(),
        }
    }
}

impl<'db> FuncParam<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::FuncParam, idx: usize) -> Self {
        let has_mut_token = ast.mut_token().is_some();
        let mut is_mut = has_mut_token || ast.var_token().is_some();
        let is_ref = ast.ref_token().is_some();
        let is_own = ast.own_token().is_some();
        let is_maybe_mut = ast.maybe_mut_token().is_some();
        let is_label_suppressed = ast.is_label_suppressed();
        let name = if ast.spread_token().is_some() {
            Some(FuncParamName::Spread)
        } else if ast.pat().is_some() {
            Some(FuncParamName::Destructured(FuncParamName::destructured(
                ctxt.db(),
                idx,
            )))
        } else {
            ast.name().map(|ast| FuncParamName::lower_ast(ctxt, ast))
        };

        let name_is_self = name.is_some_and(|name| name.is_self(ctxt.db()));
        let self_ty_fallback = name_is_self && ast.ty().is_none();
        let receiver_prefixed_mode = if is_ref {
            Some(TypeMode::Ref)
        } else if is_own {
            Some(TypeMode::Own)
        } else if is_maybe_mut {
            Some(TypeMode::MaybeMut)
        } else {
            None
        };

        let ty = if self_ty_fallback {
            let fallback_self = TypeId::fallback_self_ty(ctxt.db());
            if is_ref {
                is_mut = false;
                Partial::Present(TypeId::new(
                    ctxt.db(),
                    TypeKind::Mode(TypeMode::Ref, Partial::Present(fallback_self)),
                ))
            } else if is_own {
                Partial::Present(TypeId::new(
                    ctxt.db(),
                    TypeKind::Mode(TypeMode::Own, Partial::Present(fallback_self)),
                ))
            } else if is_maybe_mut {
                Partial::Present(TypeId::new(
                    ctxt.db(),
                    TypeKind::Mode(TypeMode::MaybeMut, Partial::Present(fallback_self)),
                ))
            } else if has_mut_token {
                is_mut = false;
                Partial::Present(TypeId::new(
                    ctxt.db(),
                    TypeKind::Mode(TypeMode::Mut, Partial::Present(fallback_self)),
                ))
            } else {
                is_mut = false;
                Partial::Present(fallback_self)
            }
        } else {
            let base_ty = TypeId::lower_ast_partial(ctxt, ast.ty());
            if name_is_self && let Some(mode) = receiver_prefixed_mode {
                base_ty
                    .to_opt()
                    .map(|inner| {
                        TypeId::new(ctxt.db(), TypeKind::Mode(mode, Partial::Present(inner)))
                    })
                    .into()
            } else {
                base_ty
            }
        };

        let mode = match ty.to_opt().map(|ty| ty.data(ctxt.db())) {
            Some(TypeKind::Mode(TypeMode::Own, _)) => FuncParamMode::Own,
            Some(TypeKind::Mode(TypeMode::Mut, _)) => FuncParamMode::Mut,
            _ => FuncParamMode::View,
        };
        // `mut` before a name, which the parser reports, is taken as `var`
        // only where `var` is valid, so it is reported once.
        if has_mut_token && !name_is_self && mode != FuncParamMode::Own {
            is_mut = false;
        }

        Self {
            mode,
            is_mut,
            has_ref_prefix: is_ref,
            has_own_prefix: is_own,
            has_mut_prefix: has_mut_token,
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
            let mode = match ast.mode_token().map(|token| token.kind()) {
                Some(SyntaxKind::MutKw) => ModeMarker::Mut,
                Some(SyntaxKind::MaybeMutKw) => ModeMarker::MaybeMut,
                _ => ModeMarker::None,
            };
            Self::Trait(TraitRefId::lower_ast(ctxt, trait_bound), mode)
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
