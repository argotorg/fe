use crate::core::hir_def::{
    GenericArgListId, IdentId, Partial, PathId, PathKind, TraitRefId, TypeId,
};
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
                    let is_bound =
                        quote_name_is_bound(seg.syntax(), ident.text(), frame.is_sub_quote, None);
                    let is_member_quote = frame.is_member_quote;
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
/// The top-level items of a fragment spliced into a block (`spliced_into_body`)
/// become items of that block, so they bind the fragment's own uses as well.
/// The members of a fragment spliced into an `impl` are not in scope as bare
/// names, so they do not.
pub(super) fn quote_name_is_bound(
    syntax: &parser::SyntaxNode,
    name: &str,
    spliced_into_body: bool,
    except_item: Option<&parser::SyntaxNode>,
) -> bool {
    let binds = |item: ast::Item| {
        except_item.is_none_or(|except| item.syntax() != except) && item_declares_name(&item, name)
    };
    syntax
        .ancestors()
        .take_while(|node| ast::QuoteExpr::cast(node.clone()).is_none())
        .any(|node| {
            if let Some(params) = node
                .children()
                .find_map(ast::GenericParamList::cast)
                .or_else(|| {
                    node.children()
                        .find_map(ast::FuncSignature::cast)
                        .and_then(|signature| signature.generic_params())
                })
                && params.into_iter().any(|param| match param.kind() {
                    ast::GenericParamKind::Type(param) => {
                        param.name().is_some_and(|ident| ident.text() == name)
                    }
                    ast::GenericParamKind::Const(param) => {
                        param.name().is_some_and(|ident| ident.text() == name)
                    }
                    ast::GenericParamKind::Shape(param) => {
                        param.name().is_some_and(|ident| ident.text() == name)
                    }
                })
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
