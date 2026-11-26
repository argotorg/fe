use crate::core::hir_def::{
    GenericArgListId, IdentId, Partial, PathId, PathKind, TraitRefId, TypeId,
};
use common::generation::{FrozenCode, FrozenCodeKind, FrozenHoleValue};
use parser::ast::{self, GenericArgsOwner, prelude::*};

use super::FileLowerCtxt;

impl<'db> PathId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Path) -> Self {
        let db = ctxt.db();

        let mut path: Option<Self> = None;
        let mut segments = ast.into_iter().peekable();
        while let Some(seg) = segments.next() {
            match seg.kind() {
                Some(ast::PathSegmentKind::QualifiedType(qualified)) => {
                    let type_ = TypeId::lower_ast_partial(ctxt, qualified.ty());
                    let trait_ = TraitRefId::lower_ast_partial(ctxt, qualified.trait_qualifier());

                    if let (Partial::Present(type_), Partial::Present(trait_)) = (type_, trait_) {
                        let kind = PathKind::QualifiedType { type_, trait_ };
                        path = Some(Self::new(db, kind, path));
                    } else {
                        // If either type or trait names are missing, just make an empty path segment
                        path = Some(Self::new(
                            db,
                            PathKind::Ident {
                                ident: Partial::Absent,
                                generic_args: GenericArgListId::none(db),
                            },
                            path,
                        ));
                    }
                }
                Some(ast::PathSegmentKind::Ingot(_))
                    if path.is_none()
                        && let Some(scope) = ctxt.quote_producer_in_other_ingot() =>
                {
                    path = Some(Self::new(
                        db,
                        PathKind::BoundIngot {
                            scope,
                            display: IdentId::make_ingot(db),
                        },
                        None,
                    ));
                }
                Some(ast::PathSegmentKind::Ident(ident))
                    if path.is_none() && ctxt.quote_names_resolve_at_producer() =>
                {
                    let frame = ctxt.quote_frames.last().unwrap();
                    let origin = frame.path_origin();
                    let by_splice = open_name_bound_by_splice(ctxt, seg.syntax(), ident.text());
                    let is_bound = quote_name_is_bound(ctxt, seg.syntax(), ident.text(), None)
                        || frame.destination_names.contains(ident.text())
                        || by_splice;
                    let is_member_quote = frame.is_member_quote;
                    if by_splice {
                        ctxt.record_bound_name(ident.text());
                    }
                    let display_ident = IdentId::lower_token(ctxt, ident.clone());
                    let generic_args = GenericArgListId::lower_ast_opt(ctxt, seg.generic_args());
                    let kind = PathKind::Ident {
                        ident: Partial::Present(display_ident),
                        generic_args,
                    };
                    if is_bound {
                        let local_quote_origin = segments.peek().is_some().then_some(origin);
                        path = Some(Self::intern(db, kind, None, local_quote_origin));
                        continue;
                    }

                    let local_ident = if is_member_quote {
                        display_ident
                    } else {
                        ctxt.resolve_sub_quote_local_ident(ident.text())
                    };
                    let display = Self::new(db, kind, None);
                    path = Some(Self::new(
                        db,
                        PathKind::InScope {
                            scope: origin.producer,
                            local_ident: Some(local_ident),
                            display,
                        },
                        None,
                    ));
                }
                Some(ast::PathSegmentKind::Super(_))
                    if path.is_none() && ctxt.quote_names_resolve_at_producer() =>
                {
                    let display_ident = IdentId::make_super(db);
                    let generic_args = GenericArgListId::lower_ast_opt(ctxt, seg.generic_args());
                    let producer_scope = ctxt.quote_frames.last().unwrap().producer;
                    let display = Self::new(
                        db,
                        PathKind::Ident {
                            ident: Partial::Present(display_ident),
                            generic_args,
                        },
                        None,
                    );
                    path = Some(Self::new(
                        db,
                        PathKind::InScope {
                            scope: producer_scope,
                            local_ident: Some(display_ident),
                            display,
                        },
                        None,
                    ));
                }
                Some(seg_kind) => {
                    let ident = match seg_kind {
                        ast::PathSegmentKind::Ingot(_) => Some(IdentId::make_ingot(db)),
                        ast::PathSegmentKind::Super(_) => Some(IdentId::make_super(db)),
                        ast::PathSegmentKind::SelfTy(_) => Some(IdentId::make_self_ty(db)),
                        ast::PathSegmentKind::Self_(_) => Some(IdentId::make_self(db)),
                        ast::PathSegmentKind::Ident(ident) => {
                            Some(IdentId::lower_token(ctxt, ident))
                        }
                        ast::PathSegmentKind::QualifiedType(_) => unreachable!(),
                    }
                    .into();

                    let generic_args = GenericArgListId::lower_ast_opt(ctxt, seg.generic_args());
                    let kind = PathKind::Ident {
                        ident,
                        generic_args,
                    };

                    path = Some(Self::new(db, kind, path));
                }
                None => {
                    // Missing segment kind, create an Ident with absent ident
                    let generic_args = GenericArgListId::lower_ast_opt(ctxt, seg.generic_args());
                    let kind = PathKind::Ident {
                        ident: Partial::Absent,
                        generic_args,
                    };
                    path = Some(Self::new(db, kind, path));
                }
            }
        }

        path.expect("ast::Path must contain at least 1 segment")
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::Path>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}

/// A quoted fragment's own generic binders, function parameters, and block item
/// declarations are copied into the destination scope. Free names keep the
/// fragment's producer origin, even where the destination declares an item of
/// the same name.
///
/// The top-level items of a fragment spliced into a block (the active frame is
/// a sub-quote) become items of that block, so they bind the fragment's own
/// uses as well. The items that a statement `${items}` of a block splices bind
/// a name of the template only if the template lists it (`open_name_bound_by_splice`).
/// The members of a fragment spliced into an `impl` are not in scope as bare
/// names, so they do not.
pub(super) fn quote_name_is_bound(
    ctxt: &FileLowerCtxt<'_>,
    syntax: &parser::SyntaxNode,
    name: &str,
    except_item: Option<&parser::SyntaxNode>,
) -> bool {
    let spliced_into_body = ctxt
        .quote_frames
        .last()
        .is_some_and(|frame| frame.is_sub_quote);
    let binds = |item: ast::Item| {
        except_item.is_none_or(|except| item.syntax() != except) && item_declares_name(&item, name)
    };
    syntax
        .ancestors()
        .take_while(|node| ast::QuoteExpr::cast(node.clone()).is_none())
        .any(|node| {
            if generic_param_names(&node)
                .iter()
                .any(|declared| declared == name)
            {
                return true;
            }
            if let Some(func) = ast::Func::cast(node.clone()) {
                let sig = func.sig();
                if sig.params().is_some_and(|params| {
                    params.into_iter().any(|param| match param.name() {
                        Some(ast::FuncParamName::Ident(ident)) => ident.text() == name,
                        Some(ast::FuncParamName::SelfParam(_)) => name == "self",
                        _ => false,
                    })
                }) {
                    return true;
                }
                if let Some(uses) = sig.uses_clause() {
                    let (list, single) = (uses.param_list(), uses.param());
                    if list
                        .into_iter()
                        .flatten()
                        .chain(single)
                        .any(|p| p.name().is_some_and(|n| n.syntax().text() == name))
                    {
                        return true;
                    }
                }
            }
            if spliced_into_body && let Some(items) = ast::QuoteItemList::cast(node.clone()) {
                return items
                    .syntax()
                    .children()
                    .filter_map(ast::Item::cast)
                    .any(binds);
            }
            if let Some(block) = ast::BlockExpr::cast(node) {
                return block.items().any(binds);
            }
            false
        })
}

/// The generic parameters that `node` declares: those of its own parameter
/// list, or of the signature of the function it is.
fn generic_param_names(node: &parser::SyntaxNode) -> Vec<String> {
    let list = node
        .children()
        .find_map(ast::GenericParamList::cast)
        .or_else(|| {
            node.children()
                .find_map(ast::FuncSignature::cast)
                .and_then(|signature| signature.generic_params())
        });
    list.into_iter()
        .flatten()
        .filter_map(|param| match param.kind() {
            ast::GenericParamKind::Type(param) => {
                param.name().map(|ident| ident.text().to_string())
            }
            ast::GenericParamKind::Const(param) => {
                param.name().map(|ident| ident.text().to_string())
            }
            ast::GenericParamKind::Shape(param) => {
                param.name().map(|ident| ident.text().to_string())
            }
        })
        .collect()
}

/// The generic parameters of the items of the template around `syntax`, from
/// the innermost outward: the binders that a `where` clause written there can
/// name.
pub(super) fn enclosing_generic_param_names(
    syntax: &parser::SyntaxNode,
) -> rustc_hash::FxHashSet<String> {
    syntax
        .ancestors()
        .take_while(|node| ast::QuoteExpr::cast(node.clone()).is_none())
        .flat_map(|node| generic_param_names(&node))
        .collect()
}

/// Whether `name` is a name that the template lists as open and that an item
/// spliced by a statement `${items}` of a block around `syntax` declares. The
/// spliced item is then the binder of the name where the template is lowered,
/// as a parameter or a `let` is for any open name; a name that the template
/// does not list resolves where the template is written, whatever a caller
/// splices in.
pub(super) fn open_name_bound_by_splice(
    ctxt: &FileLowerCtxt<'_>,
    syntax: &parser::SyntaxNode,
    name: &str,
) -> bool {
    ctxt.quote_frames
        .last()
        .is_some_and(|frame| frame.open_names.iter().any(|(open, _)| open == name))
        && syntax
            .ancestors()
            .take_while(|node| ast::QuoteExpr::cast(node.clone()).is_none())
            .filter_map(ast::BlockExpr::cast)
            .any(|block| {
                block
                    .stmts()
                    .any(|stmt| block_splice_declares(ctxt, &stmt, name))
            })
}

/// Whether the statement `stmt` of a block is a hole that splices item code
/// declaring `name`. Lowering declares those items in the block that holds the
/// statement (`lower_block_item_code`).
fn block_splice_declares(ctxt: &FileLowerCtxt<'_>, stmt: &ast::Stmt, name: &str) -> bool {
    if let ast::StmtKind::Expr(expr_stmt) = stmt.kind()
        && let Some(expr) = expr_stmt.expr()
        && let ast::ExprKind::QuoteHole(hole) = expr.kind()
        && let Some(FrozenHoleValue::Code(code)) = ctxt.current_hole(hole.syntax().text_range())
    {
        return item_code_declares(ctxt, &code, name);
    }
    false
}

/// Whether the items that the item code `code` splices into a block include one
/// that declares `name`, directly or through the item code it splices in turn.
fn item_code_declares(ctxt: &FileLowerCtxt<'_>, code: &FrozenCode, name: &str) -> bool {
    let FrozenCode::Template {
        kind: FrozenCodeKind::Item,
        quote_file,
        quote_range,
        holes,
        ..
    } = code
    else {
        return false;
    };
    let (_, quote) = ctxt.quote_template_syntax(quote_file, *quote_range);
    let holes: Vec<_> = crate::core::meta::quote_holes(&quote)
        .expect("checked frozen quote retains valid bounded syntax")
        .into_iter()
        .map(|hole| hole.syntax().text_range())
        .zip(holes)
        .collect();
    let splices = |hole: &parser::SyntaxNode| {
        holes.iter().any(|(range, value)| {
            *range == hole.text_range()
                && matches!(value, FrozenHoleValue::Code(nested) if item_code_declares(ctxt, nested, name))
        })
    };
    if let Some(item_list) = quote.item_list() {
        item_list.syntax().children().any(|child| {
            ast::Item::cast(child.clone()).is_some_and(|item| item_declares_name(&item, name))
                || ast::QuoteHole::cast(child).is_some_and(|hole| splices(hole.syntax()))
        })
    } else if let Some(block) = quote.body() {
        block.items().any(|item| item_declares_name(&item, name))
            || block.stmts().any(|stmt| {
                matches!(
                    stmt.kind(),
                    ast::StmtKind::Expr(expr_stmt)
                        if matches!(
                            expr_stmt.expr().map(|expr| expr.kind()),
                            Some(ast::ExprKind::QuoteHole(hole)) if splices(hole.syntax())
                        )
                )
            })
    } else {
        false
    }
}

fn item_declares_name(item: &ast::Item, name: &str) -> bool {
    match item.kind() {
        Some(ast::ItemKind::Func(f)) => f.sig().name().is_some_and(|n| n.text() == name),
        Some(ast::ItemKind::Struct(s)) => s.name().is_some_and(|n| n.text() == name),
        Some(ast::ItemKind::Enum(e)) => e.name().is_some_and(|n| n.text() == name),
        Some(ast::ItemKind::TypeAlias(t)) => t.alias().is_some_and(|n| n.text() == name),
        Some(ast::ItemKind::Const(c)) => c.name().is_some_and(|n| n.text() == name),
        Some(ast::ItemKind::Trait(t)) => t.name().is_some_and(|n| n.text() == name),
        Some(ast::ItemKind::Mod(m)) => m.name().is_some_and(|n| n.text() == name),
        Some(ast::ItemKind::Contract(c)) => c.name().is_some_and(|n| n.text() == name),
        Some(ast::ItemKind::Msg(m)) => m.name().is_some_and(|n| n.text() == name),
        Some(ast::ItemKind::Use(u)) => u
            .use_tree()
            .is_some_and(|tree| use_tree_binds_name(&tree, name, None)),
        _ => false,
    }
}

/// Whether a block-local `use` binds `name`: the alias of a leaf if it has one,
/// else the last segment of its path, and for a `self` leaf the segment before
/// it. A glob leaf binds nothing here; a glob import inside a quotation is
/// refused where the `use` is lowered.
fn use_tree_binds_name(
    tree: &ast::UseTree,
    name: &str,
    enclosing: Option<ast::UsePathSegment>,
) -> bool {
    let last = tree.path().and_then(|path| path.into_iter().last());
    if let Some(children) = tree.children() {
        let enclosing = last.or(enclosing);
        return children
            .into_iter()
            .any(|child| use_tree_binds_name(&child, name, enclosing.clone()));
    }
    if let Some(alias) = tree.alias() {
        return alias.ident().is_some_and(|ident| ident.text() == name);
    }
    match last.as_ref().and_then(|segment| segment.kind()) {
        Some(ast::UsePathSegmentKind::Ident(ident)) => ident.text() == name,
        Some(ast::UsePathSegmentKind::Self_(_)) => enclosing
            .and_then(|segment| segment.ident())
            .is_some_and(|ident| ident.text() == name),
        _ => false,
    }
}
