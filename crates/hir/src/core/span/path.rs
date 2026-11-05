use parser::ast::{self, prelude::*};

use super::{
    LazySpanAtom, define_lazy_span_node,
    params::{LazyGenericArgListSpan, LazyTraitRefSpan},
    transition::{LazyArg, LazyTransitionFn, ResolvedOrigin},
    types::LazyTySpan,
};

// A trait ref written as a callable shape, `Fn(own A) -> R`, names the
// path `core::functional::Fn<(own A), Out = R>` it lowers to with no path
// syntax: each of the path's segments is the shape, whose generic arguments
// are its parameter list and its result.
define_lazy_span_node!(LazyPathSpan, ast::Path, @idx {});

impl<'db> LazyPathSpan<'db> {
    pub fn segment(mut self, idx: usize) -> LazyPathSegmentSpan<'db> {
        fn f(origin: ResolvedOrigin, arg: LazyArg) -> ResolvedOrigin {
            let LazyArg::Idx(idx) = arg else {
                unreachable!()
            };
            origin.map(|node| match ast::Path::cast(node.clone()) {
                Some(path) => path
                    .into_iter()
                    .nth(idx)
                    .map(|segment| segment.syntax().clone().into()),
                None => ast::FnShape::cast(node).map(|shape| shape.syntax().clone().into()),
            })
        }

        self.0.push(LazyTransitionFn {
            f,
            arg: LazyArg::Idx(idx),
        });
        LazyPathSegmentSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyPathSegmentSpan,
    ast::PathSegment,
    @node {
        (qualified_type, qualified_type, LazyQualifiedTypeSpan),
    }
);
impl<'db> LazyPathSegmentSpan<'db> {
    pub fn into_atom(self) -> LazySpanAtom<'db> {
        LazySpanAtom(self.0)
    }

    pub fn ident(mut self) -> LazySpanAtom<'db> {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin.map(|node| match ast::PathSegment::cast(node.clone()) {
                Some(segment) => segment.ident().map(Into::into),
                None => ast::FnShape::cast(node)?.name().map(Into::into),
            })
        }

        self.0.push(LazyTransitionFn {
            f,
            arg: LazyArg::None,
        });
        LazySpanAtom(self.0)
    }

    pub fn generic_args(mut self) -> LazyGenericArgListSpan<'db> {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin.map(|node| match ast::PathSegment::cast(node.clone()) {
                Some(segment) => segment
                    .generic_args()
                    .map(|args| args.syntax().clone().into()),
                None => ast::FnShape::cast(node).map(|shape| shape.syntax().clone().into()),
            })
        }

        self.0.push(LazyTransitionFn {
            f,
            arg: LazyArg::None,
        });
        LazyGenericArgListSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyQualifiedTypeSpan,
    ast::QualifiedType,
    @node {
        (ty, ty, LazyTySpan),
        (trait_qualifier, trait_qualifier, LazyTraitRefSpan),
    }
);
