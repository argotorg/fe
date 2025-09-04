use parser::ast::{self, prelude::*};

use super::{LazySpanAtom, define_lazy_span_node};
use crate::span::{
    DesugaredOrigin,
    transition::{LazyArg, LazyTransitionFn, ResolvedOrigin, ResolvedOriginKind},
};

define_lazy_span_node!(LazyUsePathSpan);
impl<'db> LazyUsePathSpan<'db> {
    pub fn segment(mut self, idx: usize) -> LazyUsePathSegmentSpan<'db> {
        fn f(origin: ResolvedOrigin, arg: LazyArg) -> ResolvedOrigin {
            let LazyArg::Idx(idx) = arg else {
                unreachable!()
            };

            origin
                .map(|node| {
                    ast::UsePath::cast(node)
                        .and_then(|f| f.into_iter().nth(idx))
                        .map(|n| n.syntax().clone().into())
                })
                .map_desugared(|root, desugared| match desugared {
                    DesugaredOrigin::Use(use_) => use_
                        .path
                        .get(idx)
                        .map(|ptr| ResolvedOriginKind::Node(ptr.syntax_node_ptr().to_node(&root)))
                        .unwrap_or_else(|| ResolvedOriginKind::None),
                })
        }

        let lazy_transition = LazyTransitionFn {
            f,
            arg: LazyArg::Idx(idx),
        };

        self.0.push(lazy_transition);
        LazyUsePathSegmentSpan(self.0)
    }
}

define_lazy_span_node!(LazyUsePathSegmentSpan);
impl<'db> LazyUsePathSegmentSpan<'db> {
    pub fn into_atom(self) -> LazySpanAtom<'db> {
        LazySpanAtom(self.0)
    }
}

define_lazy_span_node!(LazyUseAliasSpan, ast::UseAlias,);

impl<'db> LazyUseAliasSpan<'db> {
    pub fn name(mut self) -> LazySpanAtom<'db> {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin
                .map(|node| {
                    ast::UseAlias::cast(node)
                        .and_then(|a| a.alias())
                        .map(|n| n.into())
                })
                .map_desugared(|root, desugared| match desugared {
                    DesugaredOrigin::Use(use_) => use_
                        .alias
                        .and_then(|ptr| ptr.to_node(&root).alias().map(ResolvedOriginKind::Token))
                        .unwrap_or_else(|| ResolvedOriginKind::None),
                })
        }

        let lazy_transition = LazyTransitionFn {
            f,
            arg: LazyArg::None,
        };
        self.0.push(lazy_transition);

        LazySpanAtom(self.0)
    }
}
