use crate::core::hir_def::{
    GenericArgListId, IdentId, Partial, PathId, PathKind, TraitRefId, TypeId,
};
use parser::ast::{self, GenericArgsOwner, prelude::*};

use super::FileLowerCtxt;

impl<'db> PathId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Path) -> Self {
        let db = ctxt.db();

        let mut path: Option<Self> = None;
        for seg in ast.into_iter() {
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
                        && let Some(ingot) = ctxt.current_quote_ingot() =>
                {
                    path = Some(Self::new(
                        db,
                        PathKind::BoundIngot {
                            ingot,
                            display: IdentId::make_ingot(db),
                        },
                        None,
                    ));
                }
                Some(ast::PathSegmentKind::Ident(ident))
                    if path.is_none()
                        && ctxt
                            .quote_frames
                            .last()
                            .is_some_and(|f| f.is_sub_quote || f.is_member_quote)
                        && !(ctxt.quote_frames.last().is_some_and(|f| f.is_member_quote)
                            && member_generic_is_bound(&seg, ident.text())) =>
                {
                    let display_ident = IdentId::lower_token(ctxt, ident.clone());
                    let local_ident = if ctxt
                        .quote_frames
                        .last()
                        .is_some_and(|frame| frame.is_member_quote)
                    {
                        display_ident
                    } else {
                        ctxt.resolve_sub_quote_local_ident(ident.text())
                    };
                    let generic_args = GenericArgListId::lower_ast_opt(ctxt, seg.generic_args());
                    let producer_scope = ctxt.quote_frames.last().unwrap().producer_scope;
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
                            local_ident: Some(local_ident),
                            display,
                        },
                        None,
                    ));
                }
                Some(ast::PathSegmentKind::Super(_))
                    if path.is_none()
                        && ctxt
                            .quote_frames
                            .last()
                            .is_some_and(|f| f.is_sub_quote || f.is_member_quote) =>
                {
                    let display_ident = IdentId::make_super(db);
                    let generic_args = GenericArgListId::lower_ast_opt(ctxt, seg.generic_args());
                    let producer_scope = ctxt.quote_frames.last().unwrap().producer_scope;
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

/// A member fragment's own generic binders are copied into the destination
/// method/impl. Free type names keep the fragment's producer origin.
fn member_generic_is_bound(segment: &ast::PathSegment, name: &str) -> bool {
    segment
        .syntax()
        .ancestors()
        .take_while(|node| ast::QuoteExpr::cast(node.clone()).is_none())
        .filter_map(|node| {
            node.children()
                .find_map(ast::GenericParamList::cast)
                .or_else(|| {
                    node.children()
                        .find_map(ast::FuncSignature::cast)
                        .and_then(|signature| signature.generic_params())
                })
        })
        .flat_map(|params| params.into_iter())
        .any(|param| match param.kind() {
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
}
