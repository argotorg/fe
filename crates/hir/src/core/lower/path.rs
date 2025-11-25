use crate::core::hir_def::{
    GenericArgListId, IdentId, Partial, PathId, PathKind, TraitRefId, TypeId,
};
use parser::ast::{self, GenericArgsOwner};

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
