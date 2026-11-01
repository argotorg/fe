use parser::ast::{self, prelude::*};

use super::body::BodyCtxt;
use crate::{
    hir_def::{
        GenericArgListId, IdentId, ItemKind, LitKind, Partial, PathId, PathKind, VariantKind,
        pat::*, scope_graph::ScopeId,
    },
    span::HirOrigin,
};

impl<'db> Pat<'db> {
    pub(super) fn lower_ast(ctxt: &mut BodyCtxt<'_, 'db>, ast: ast::Pat) -> PatId {
        let pat = match &ast.kind() {
            ast::PatKind::QuoteHole(hole) => {
                use common::generation::{FrozenHoleValue, FrozenScalar};
                match ctxt.f_ctxt.current_hole(hole.syntax().text_range()) {
                    Some(FrozenHoleValue::Code(code)) => {
                        return Self::lower_code(ctxt, &code, &ast);
                    }
                    Some(FrozenHoleValue::Scalar(FrozenScalar::Bool(value))) => {
                        Pat::Lit(Partial::Present(LitKind::Bool(value)))
                    }
                    Some(FrozenHoleValue::Scalar(FrozenScalar::Int {
                        negative: false,
                        bytes,
                        ..
                    })) => {
                        let integer = crate::hir_def::IntegerId::new(
                            ctxt.f_ctxt.db(),
                            num_bigint::BigUint::from_bytes_be(&bytes),
                        );
                        Pat::Lit(Partial::Present(LitKind::Int(integer)))
                    }
                    _ => {
                        ctxt.f_ctxt.record_generation_error(
                            hole.syntax().text_range(),
                            "pattern quotation hole requires Code<Pat> or literal splice",
                        );
                        return ctxt.push_missing_pat();
                    }
                }
            }

            ast::PatKind::WildCard(_) => Pat::WildCard,

            ast::PatKind::Rest(_) => Pat::Rest,

            ast::PatKind::Lit(lit_pat) => {
                let lit_kind = lit_pat
                    .lit()
                    .and_then(|lit| LitKind::lower_ast(ctxt.f_ctxt, lit))
                    .into();
                Pat::Lit(lit_kind)
            }

            ast::PatKind::Tuple(tup) => {
                let elems = match tup.elems() {
                    Some(elems) => elems.iter().map(|pat| Pat::lower_ast(ctxt, pat)).collect(),
                    None => vec![],
                };
                Pat::Tuple(elems)
            }

            ast::PatKind::Path(path_ast) => {
                let path = PathId::lower_ast_partial(ctxt.f_ctxt, path_ast.path());
                if let Partial::Present(path_id) = path {
                    let db = ctxt.f_ctxt.db();
                    if path_id.parent(db).is_none()
                        && path_id.generic_args(db).is_empty(db)
                        && let Some(local_ident) = path_id.local_ident(db)
                    {
                        ctxt.record_local_binder(local_ident, path_id.ident(db).to_opt());
                    }
                }
                let marker = if path_ast.mut_token().is_some() {
                    BindingMarker::Mut
                } else if path_ast.maybe_mut_token().is_some() {
                    BindingMarker::MaybeMut
                } else if path_ast.ref_token().is_some() {
                    BindingMarker::Ref
                } else if path_ast.var_token().is_some() {
                    BindingMarker::Var
                } else {
                    BindingMarker::Plain
                };
                Pat::Path(path, marker)
            }

            ast::PatKind::PathTuple(path_tup) => {
                let path = PathId::lower_ast_partial(ctxt.f_ctxt, path_tup.path());
                let elems = match path_tup.elems() {
                    Some(elems) => elems.iter().map(|pat| Pat::lower_ast(ctxt, pat)).collect(),
                    None => vec![],
                };
                Pat::PathTuple(path, elems)
            }

            ast::PatKind::Record(record) => {
                let path = PathId::lower_ast_partial(ctxt.f_ctxt, record.path());
                let fields = match record.fields() {
                    Some(fields) => fields
                        .iter()
                        .map(|f| RecordPatField::lower_ast(ctxt, &f))
                        .collect(),
                    None => vec![],
                };
                Pat::Record(path, fields)
            }

            ast::PatKind::Or(or) => {
                let lhs = Self::lower_ast_opt(ctxt, or.lhs());
                let rhs = Self::lower_ast_opt(ctxt, or.rhs());
                Pat::Or(lhs, rhs)
            }
        };

        ctxt.push_pat(pat, HirOrigin::raw(&ast))
    }

    /// Lowers the pattern of a `let`, or of a `var` that marks its name
    /// `var`; so does `let mut x`, which the parser reports.
    pub(super) fn lower_let_ast(
        ctxt: &mut BodyCtxt<'_, 'db>,
        ast: Option<ast::Pat>,
        is_var: bool,
    ) -> PatId {
        let Some(ast) = ast else {
            return ctxt.push_missing_pat();
        };
        if let ast::PatKind::Path(path_ast) = ast.kind()
            && (is_var || path_ast.mut_token().is_some())
        {
            let path = PathId::lower_ast_partial(ctxt.f_ctxt, path_ast.path());
            if let Partial::Present(path_id) = path {
                let db = ctxt.f_ctxt.db();
                if path_id.parent(db).is_none()
                    && path_id.generic_args(db).is_empty(db)
                    && let Some(local_ident) = path_id.local_ident(db)
                {
                    ctxt.record_local_binder(local_ident, path_id.ident(db).to_opt());
                }
            }
            return ctxt.push_pat(Pat::Path(path, BindingMarker::Var), HirOrigin::raw(&ast));
        }
        Pat::lower_ast(ctxt, ast)
    }

    pub(super) fn lower_ast_opt(ctxt: &mut BodyCtxt<'_, 'db>, ast: Option<ast::Pat>) -> PatId {
        if let Some(ast) = ast {
            Pat::lower_ast(ctxt, ast)
        } else {
            ctxt.push_missing_pat()
        }
    }

    pub(super) fn lower_code(
        ctxt: &mut BodyCtxt<'_, 'db>,
        code: &common::generation::FrozenCode,
        origin_ast: &ast::Pat,
    ) -> PatId {
        use common::generation::{FrozenCode, FrozenCodeKind};
        match code {
            FrozenCode::VariantBinders { record, group } => {
                if ctxt.active_binder_groups.iter().any(|(_, g)| *g == *group) {
                    ctxt.f_ctxt.record_generation_error(
                        origin_ast.syntax().text_range(),
                        "binder group already bound in an enclosing pattern",
                    );
                    return ctxt.push_missing_pat();
                }
                ctxt.active_binder_groups.push((record.clone(), *group));
                let db = ctxt.f_ctxt.db();
                match (
                    ctxt.f_ctxt.resolve_record_item(record),
                    record.variant_index,
                ) {
                    (ItemKind::Enum(enum_), Some(variant_idx)) => {
                        let variant_def = &enum_.variants_list(db).data(db)[variant_idx];
                        let enum_display = PathId::new(
                            db,
                            PathKind::Ident {
                                ident: enum_.name(db),
                                generic_args: GenericArgListId::none(db),
                            },
                            None,
                        );
                        let enum_path = PathId::new(
                            db,
                            PathKind::BoundItem {
                                item: ScopeId::Item(ItemKind::Enum(enum_)),
                                display: enum_display,
                            },
                            None,
                        );
                        let variant_path = Partial::Present(PathId::new(
                            db,
                            PathKind::Ident {
                                ident: variant_def.name,
                                generic_args: GenericArgListId::none(db),
                            },
                            Some(enum_path),
                        ));
                        match &variant_def.kind {
                            VariantKind::Unit => ctxt.push_pat(
                                Pat::Path(variant_path, BindingMarker::Plain),
                                HirOrigin::raw(origin_ast),
                            ),
                            VariantKind::Tuple(tuple_id) => {
                                let elems = (0..tuple_id.data(db).len())
                                    .map(|field_idx| {
                                        let ident = IdentId::new(
                                            db,
                                            format!("\0binder_{group}_{field_idx}"),
                                        );
                                        let path = Partial::Present(PathId::from_ident(db, ident));
                                        ctxt.push_pat(
                                            Pat::Path(path, BindingMarker::Plain),
                                            HirOrigin::raw(origin_ast),
                                        )
                                    })
                                    .collect();
                                ctxt.push_pat(
                                    Pat::PathTuple(variant_path, elems),
                                    HirOrigin::raw(origin_ast),
                                )
                            }
                            VariantKind::Record(fields_id) => {
                                let fields = fields_id
                                    .data(db)
                                    .iter()
                                    .enumerate()
                                    .map(|(field_idx, field_def)| {
                                        let ident = IdentId::new(
                                            db,
                                            format!("\0binder_{group}_{field_idx}"),
                                        );
                                        let path = Partial::Present(PathId::from_ident(db, ident));
                                        let pat = ctxt.push_pat(
                                            Pat::Path(path, BindingMarker::Plain),
                                            HirOrigin::raw(origin_ast),
                                        );
                                        RecordPatField {
                                            label: field_def.name,
                                            pat,
                                        }
                                    })
                                    .collect();
                                ctxt.push_pat(
                                    Pat::Record(variant_path, fields),
                                    HirOrigin::raw(origin_ast),
                                )
                            }
                        }
                    }
                    (ItemKind::Struct(struct_), None) => {
                        let struct_display = PathId::new(
                            db,
                            PathKind::Ident {
                                ident: struct_.name(db),
                                generic_args: GenericArgListId::none(db),
                            },
                            None,
                        );
                        let struct_path = Partial::Present(PathId::new(
                            db,
                            PathKind::BoundItem {
                                item: ScopeId::Item(ItemKind::Struct(struct_)),
                                display: struct_display,
                            },
                            None,
                        ));
                        let fields = struct_
                            .fields(db)
                            .data(db)
                            .iter()
                            .enumerate()
                            .map(|(field_idx, field_def)| {
                                let ident =
                                    IdentId::new(db, format!("\0binder_{group}_{field_idx}"));
                                let path = Partial::Present(PathId::from_ident(db, ident));
                                let pat = ctxt.push_pat(
                                    Pat::Path(path, BindingMarker::Plain),
                                    HirOrigin::raw(origin_ast),
                                );
                                RecordPatField {
                                    label: field_def.name,
                                    pat,
                                }
                            })
                            .collect();
                        ctxt.push_pat(Pat::Record(struct_path, fields), HirOrigin::raw(origin_ast))
                    }
                    _ => {
                        ctxt.f_ctxt.record_generation_error(
                            origin_ast.syntax().text_range(),
                            "invalid record for variant binders",
                        );
                        ctxt.push_missing_pat()
                    }
                }
            }
            FrozenCode::Template {
                kind: FrozenCodeKind::Pat,
                quote_file,
                quote_range,
                producer_module,
                producer_block,
                holes,
            } => {
                let (quote, frame) = ctxt.f_ctxt.resolve_quote_template(
                    quote_file,
                    *quote_range,
                    *producer_module,
                    *producer_block,
                    true,
                    holes.clone(),
                );
                let quote_file_handle = frame.quote_top_mod.file(ctxt.f_ctxt.db());
                let open_checks: Vec<_> = frame
                    .open_names
                    .iter()
                    .map(|(name, range)| {
                        (
                            name.clone(),
                            *range,
                            ctxt.f_ctxt.resolve_sub_quote_local_ident(name),
                        )
                    })
                    .collect();
                ctxt.f_ctxt.quote_frames.push(frame);
                let pat_id = if let Some(pat) = quote.pattern() {
                    Self::lower_ast(ctxt, pat)
                } else if let Some(block) = quote.body()
                    && block.items().next().is_none()
                    && block.stmts().count() == 1
                    && let Some(stmt) = block.stmts().next()
                    && let ast::StmtKind::Expr(expr_stmt) = stmt.kind()
                    && let Some(expr) = expr_stmt.expr()
                {
                    Self::lower_expr_as_pat(ctxt, expr, origin_ast)
                } else {
                    ctxt.f_ctxt.record_generation_error(
                        *quote_range,
                        "pattern quotation requires a pattern",
                    );
                    ctxt.push_missing_pat()
                };
                ctxt.f_ctxt.quote_frames.pop();
                for (name, range, target_ident) in open_checks {
                    if !ctxt.local_binders.contains(&target_ident) {
                        ctxt.f_ctxt.file_scoped_error(
                            quote_file_handle,
                            range,
                            format!("quoted open name `{name}` has no enclosing binder"),
                        );
                    }
                }
                pat_id
            }
            _ => {
                ctxt.f_ctxt.record_generation_error(
                    origin_ast.syntax().text_range(),
                    "pattern quotation hole requires Code<Pat> splice",
                );
                ctxt.push_missing_pat()
            }
        }
    }

    fn lower_expr_as_pat(
        ctxt: &mut BodyCtxt<'_, 'db>,
        expr: ast::Expr,
        origin_ast: &ast::Pat,
    ) -> PatId {
        match expr.kind() {
            ast::ExprKind::QuoteHole(hole) => {
                use common::generation::{FrozenHoleValue, FrozenScalar};
                match ctxt.f_ctxt.current_hole(hole.syntax().text_range()) {
                    Some(FrozenHoleValue::Code(code)) => Self::lower_code(ctxt, &code, origin_ast),
                    Some(FrozenHoleValue::Scalar(FrozenScalar::Bool(value))) => ctxt.push_pat(
                        Pat::Lit(Partial::Present(LitKind::Bool(value))),
                        HirOrigin::raw(origin_ast),
                    ),
                    Some(FrozenHoleValue::Scalar(FrozenScalar::Int {
                        negative: false,
                        bytes,
                        ..
                    })) => {
                        let integer = crate::hir_def::IntegerId::new(
                            ctxt.f_ctxt.db(),
                            num_bigint::BigUint::from_bytes_be(&bytes),
                        );
                        ctxt.push_pat(
                            Pat::Lit(Partial::Present(LitKind::Int(integer))),
                            HirOrigin::raw(origin_ast),
                        )
                    }
                    _ => {
                        ctxt.f_ctxt.record_generation_error(
                            hole.syntax().text_range(),
                            "pattern quotation hole requires Code<Pat> or literal splice",
                        );
                        ctxt.push_missing_pat()
                    }
                }
            }
            ast::ExprKind::Paren(paren) => match paren.expr() {
                Some(inner) => Self::lower_expr_as_pat(ctxt, inner, origin_ast),
                None => ctxt.push_missing_pat(),
            },
            ast::ExprKind::Tuple(tup) => {
                let elems = tup
                    .elems()
                    .map(|elem| match elem {
                        Some(elem) => Self::lower_expr_as_pat(ctxt, elem, origin_ast),
                        None => ctxt.push_missing_pat(),
                    })
                    .collect();
                ctxt.push_pat(Pat::Tuple(elems), HirOrigin::raw(origin_ast))
            }
            ast::ExprKind::Path(path_expr) => {
                if path_expr.syntax().text() == "_" {
                    ctxt.push_pat(Pat::WildCard, HirOrigin::raw(origin_ast))
                } else {
                    let path = PathId::lower_ast_partial(ctxt.f_ctxt, path_expr.path());
                    if let Partial::Present(path_id) = path {
                        let db = ctxt.f_ctxt.db();
                        if path_id.parent(db).is_none()
                            && path_id.generic_args(db).is_empty(db)
                            && let Some(local_ident) = path_id.local_ident(db)
                        {
                            ctxt.record_local_binder(local_ident, path_id.ident(db).to_opt());
                        }
                    }
                    ctxt.push_pat(
                        Pat::Path(path, BindingMarker::Plain),
                        HirOrigin::raw(origin_ast),
                    )
                }
            }
            ast::ExprKind::Call(call)
                if let Some(callee) = call.callee()
                    && let ast::ExprKind::Path(path_expr) = callee.kind() =>
            {
                let path = PathId::lower_ast_partial(ctxt.f_ctxt, path_expr.path());
                let elems = call
                    .args()
                    .map(|args| {
                        args.into_iter()
                            .map(|arg| match arg.expr() {
                                Some(e) => Self::lower_expr_as_pat(ctxt, e, origin_ast),
                                None => ctxt.push_missing_pat(),
                            })
                            .collect()
                    })
                    .unwrap_or_default();
                ctxt.push_pat(Pat::PathTuple(path, elems), HirOrigin::raw(origin_ast))
            }
            ast::ExprKind::RecordInit(record_init) => {
                let path = PathId::lower_ast_partial(ctxt.f_ctxt, record_init.path());
                let fields = record_init
                    .fields()
                    .map(|fields| {
                        fields
                            .into_iter()
                            .map(|f| {
                                let label = if let Some(hole) = f.label_hole() {
                                    ctxt.f_ctxt
                                        .lower_field_hole_ident(hole.syntax().text_range())
                                } else {
                                    IdentId::lower_token_partial(ctxt.f_ctxt, f.label())
                                };
                                let pat = match f.expr() {
                                    Some(e) => Self::lower_expr_as_pat(ctxt, e, origin_ast),
                                    None => ctxt.push_missing_pat(),
                                };
                                RecordPatField { label, pat }
                            })
                            .collect()
                    })
                    .unwrap_or_default();
                ctxt.push_pat(Pat::Record(path, fields), HirOrigin::raw(origin_ast))
            }
            ast::ExprKind::Lit(lit_expr) => {
                let lit_kind = lit_expr
                    .lit()
                    .and_then(|lit| LitKind::lower_ast(ctxt.f_ctxt, lit))
                    .into();
                ctxt.push_pat(Pat::Lit(lit_kind), HirOrigin::raw(origin_ast))
            }
            // `a | b` parses as the bitwise-or expression; as a pattern it
            // is the alternative of the two.
            ast::ExprKind::Bin(bin)
                if matches!(bin.op(), Some(ast::BinOp::Arith(ast::ArithBinOp::BitOr(_)))) =>
            {
                let lhs = match bin.lhs() {
                    Some(lhs) => Self::lower_expr_as_pat(ctxt, lhs, origin_ast),
                    None => ctxt.push_missing_pat(),
                };
                let rhs = match bin.rhs() {
                    Some(rhs) => Self::lower_expr_as_pat(ctxt, rhs, origin_ast),
                    None => ctxt.push_missing_pat(),
                };
                ctxt.push_pat(Pat::Or(lhs, rhs), HirOrigin::raw(origin_ast))
            }
            _ => {
                ctxt.f_ctxt.record_generation_error(
                    expr.syntax().text_range(),
                    "unsupported pattern syntax in pattern quotation",
                );
                ctxt.push_missing_pat()
            }
        }
    }
}

impl<'db> RecordPatField<'db> {
    fn lower_ast(ctxt: &mut BodyCtxt<'_, 'db>, ast: &ast::RecordPatField) -> RecordPatField<'db> {
        let label = if let Some(hole) = ast.syntax().children().find_map(ast::QuoteHole::cast) {
            ctxt.f_ctxt
                .lower_field_hole_ident(hole.syntax().text_range())
        } else {
            IdentId::lower_token_partial(ctxt.f_ctxt, ast.name())
        };
        let pat = ast
            .pat()
            .map(|pat| Pat::lower_ast(ctxt, pat))
            .unwrap_or_else(|| ctxt.push_missing_pat());
        RecordPatField { label, pat }
    }
}
