use parser::ast::{self, prelude::*};

use super::{
    define_lazy_span_node,
    transition::{LazyArg, LazyTransitionFn, ResolvedOrigin},
    types::LazyTySpan,
};
use crate::span::{LazySpanAtom, path::LazyPathSpan};

define_lazy_span_node!(
    LazyFuncParamListSpan,
    ast::FuncParamList,
    @idx {
        (param, LazyFuncParamSpan),
    }
);

define_lazy_span_node!(
    LazyGenericParamListSpan,
    ast::GenericParamList,
    @idx {
        (param, LazyGenericParamSpan),
    }
);

define_lazy_span_node!(LazyGenericArgListSpan, ast::GenericArgList, @idx {});

impl<'db> LazyGenericArgListSpan<'db> {
    /// Argument `idx`. A callable shape's are its parameter list and its
    /// result, `Out`.
    pub fn arg(mut self, idx: usize) -> LazyGenericArgSpan<'db> {
        fn f(origin: ResolvedOrigin, arg: LazyArg) -> ResolvedOrigin {
            let LazyArg::Idx(idx) = arg else {
                unreachable!()
            };
            origin.map(|node| match ast::GenericArgList::cast(node.clone()) {
                Some(args) => args
                    .into_iter()
                    .nth(idx)
                    .map(|arg| arg.syntax().clone().into()),
                None => {
                    let shape = ast::FnShape::cast(node)?;
                    match idx {
                        0 => shape.params().map(|params| params.syntax().clone().into()),
                        _ => Some(
                            shape
                                .ret_ty()
                                .map_or_else(|| shape.syntax().clone(), |ty| ty.syntax().clone())
                                .into(),
                        ),
                    }
                }
            })
        }

        self.0.push(LazyTransitionFn {
            f,
            arg: LazyArg::Idx(idx),
        });
        LazyGenericArgSpan(self.0)
    }
}
define_lazy_span_node!(
    LazyWhereClauseSpan,
    ast::WhereClause,
    @token {
        (where_token, where_kw),
    }
    @idx {
        (predicate, LazyWherePredicateSpan),
    }
);

define_lazy_span_node!(
    LazyFuncParamSpan,
    ast::FuncParam,
    @token {
       (mut_kw, mut_token),
       (ref_kw, ref_token),
       (own_kw, own_token),
       (var_kw, var_token),
    }
    @node {
       (name, name, LazySpanAtom),
       (ty, ty, LazyTySpan),
    }
);

impl<'db> LazyFuncParamSpan<'db> {
    pub fn fallback_self_ty(self) -> LazyTySpan<'db> {
        LazyTySpan(self.name().0)
    }
}

define_lazy_span_node!(
    LazyUsesClauseSpan,
    ast::UsesClause,
    @node {
        (param_list, param_list, LazyUsesParamListSpan),
        (param, param, LazyUsesParamSpan),
    }
);

define_lazy_span_node!(
    LazyUsesParamListSpan,
    ast::UsesParamList,
    @idx {
        (param, LazyUsesParamSpan),
    }
);

define_lazy_span_node!(
    LazyUsesParamSpan,
    ast::UsesParam,
    @token {
        (mut_kw, mut_token),
    }
    @node {
        (ty, ty, LazyTySpan),
    }
);

impl<'db> LazyUsesClauseSpan<'db> {
    /// Returns the span for the `idx`-th uses param, supporting both
    /// `uses (..)` and single-parameter `uses ..` forms.
    pub fn param_idx(mut self, idx: usize) -> LazyUsesParamSpan<'db> {
        use crate::span::transition::{LazyArg, LazyTransitionFn, ResolvedOrigin};
        use parser::ast::prelude::*;

        fn f(origin: ResolvedOrigin, arg: LazyArg) -> ResolvedOrigin {
            let idx = match arg {
                LazyArg::Idx(i) => i,
                _ => 0,
            };
            origin.map(|node| {
                ast::UsesClause::cast(node).and_then(|u| {
                    if let Some(list) = u.param_list() {
                        list.into_iter().nth(idx).map(|n| n.syntax().clone().into())
                    } else if idx == 0 {
                        u.param().map(|n| n.syntax().clone().into())
                    } else {
                        None
                    }
                })
            })
        }

        self.0.push(LazyTransitionFn {
            f,
            arg: LazyArg::Idx(idx),
        });
        LazyUsesParamSpan(self.0)
    }
}

impl<'db> LazyUsesParamSpan<'db> {
    /// Span atom for the optional name (identifier or underscore) of a uses param.
    pub fn name(mut self) -> LazySpanAtom<'db> {
        use crate::span::transition::{LazyArg, LazyTransitionFn, ResolvedOrigin};
        use parser::ast::prelude::*;

        fn f(origin: ResolvedOrigin, _arg: LazyArg) -> ResolvedOrigin {
            origin.map(|node| {
                ast::UsesParam::cast(node)
                    .and_then(|p| p.name())
                    .map(|n| n.syntax().into())
            })
        }

        self.0.push(LazyTransitionFn {
            f,
            arg: LazyArg::None,
        });
        LazySpanAtom(self.0)
    }
}

define_lazy_span_node!(LazyGenericParamSpan, ast::GenericParam);
impl<'db> LazyGenericParamSpan<'db> {
    pub fn into_type_param(self) -> LazyTypeGenericParamSpan<'db> {
        LazyTypeGenericParamSpan(self.0)
    }

    pub fn into_const_param(self) -> LazyConstGenericParamSpan<'db> {
        LazyConstGenericParamSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyTypeGenericParamSpan,
    ast::TypeGenericParam,
    @token {
        (name, name),
    }
    @node {
        (bounds, bounds, LazyTypeBoundListSpan),
        (default_ty, default_ty, LazyTySpan),
    }
);

define_lazy_span_node!(
    LazyConstGenericParamSpan,
    ast::ConstGenericParam,
    @token {
        (const_token, const_kw),
        (name, name),
    }
    @node {
        (ty, ty, LazyTySpan),
    }
);

define_lazy_span_node!(LazyGenericArgSpan);
impl<'db> LazyGenericArgSpan<'db> {
    pub fn into_type_arg(self) -> LazyTypeGenericArgSpan<'db> {
        LazyTypeGenericArgSpan(self.0)
    }

    pub fn into_assoc_type_arg(self) -> LazyAssocTypeGenericArgSpan<'db> {
        LazyAssocTypeGenericArgSpan(self.0)
    }
}

define_lazy_span_node!(LazyTypeGenericArgSpan, ast::TypeGenericArg, @node {});

impl<'db> LazyTypeGenericArgSpan<'db> {
    /// The argument's type, or a callable shape's parameter list, a type
    /// already.
    pub fn ty(mut self) -> LazyTySpan<'db> {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin.map(|node| match ast::TypeGenericArg::cast(node.clone()) {
                Some(arg) => arg.ty().map(|ty| ty.syntax().clone().into()),
                None => ast::Type::cast(node).map(|ty| ty.syntax().clone().into()),
            })
        }

        self.0.push(LazyTransitionFn {
            f,
            arg: LazyArg::None,
        });
        LazyTySpan(self.0)
    }
}

define_lazy_span_node!(
    LazyAssocTypeGenericArgSpan,
    ast::AssocTypeGenericArg,
    @token {
        (name, name),
    }
    @node {
        (generic_args, generic_args, LazyGenericArgListSpan),
    }
);

impl<'db> LazyAssocTypeGenericArgSpan<'db> {
    /// The bound type, or a callable shape's result, a type already.
    pub fn ty(mut self) -> LazyTySpan<'db> {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin.map(|node| match ast::AssocTypeGenericArg::cast(node.clone()) {
                Some(arg) => arg.ty().map(|ty| ty.syntax().clone().into()),
                None => ast::Type::cast(node).map(|ty| ty.syntax().clone().into()),
            })
        }

        self.0.push(LazyTransitionFn {
            f,
            arg: LazyArg::None,
        });
        LazyTySpan(self.0)
    }
}

define_lazy_span_node!(
    LazyWherePredicateSpan,
    ast::WherePredicate,
    @node {
        (ty, ty, LazyTySpan),
        (bounds, bounds, LazyTypeBoundListSpan),
    }
);

define_lazy_span_node! {
    LazyTypeBoundListSpan,
    ast::TypeBoundList,
    @idx {
        (bound, LazyTypeBoundSpan),
    }
}

define_lazy_span_node!(
    LazyTypeBoundSpan,
    ast::TypeBound,
    @node {
        (trait_bound, trait_bound, LazyTraitRefSpan),
        (kind_bound, kind_bound, LazyKindBoundSpan),
    }
);

define_lazy_span_node!(LazyTraitRefSpan, ast::TraitRef, @node {});

impl<'db> LazyTraitRefSpan<'db> {
    /// The trait's path, or the callable shape, `Fn(own A) -> R`, written in
    /// its place.
    pub fn path(mut self) -> LazyPathSpan<'db> {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin.map(|node| {
                let trait_ref = ast::TraitRef::cast(node)?;
                trait_ref
                    .path()
                    .map(|path| path.syntax().clone())
                    .or_else(|| trait_ref.fn_shape().map(|shape| shape.syntax().clone()))
                    .map(Into::into)
            })
        }

        self.0.push(LazyTransitionFn {
            f,
            arg: LazyArg::None,
        });
        LazyPathSpan(self.0)
    }

    /// The trait's name: its path's last segment, or a callable shape's
    /// `Fn` or `FnMut`.
    pub fn name(mut self) -> LazySpanAtom<'db> {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin.map(|node| {
                let trait_ref = ast::TraitRef::cast(node)?;
                match trait_ref.path() {
                    Some(path) => path.into_iter().last()?.ident(),
                    None => trait_ref.fn_shape()?.name(),
                }
                .map(Into::into)
            })
        }

        self.0.push(LazyTransitionFn {
            f,
            arg: LazyArg::None,
        });
        LazySpanAtom(self.0)
    }
}

define_lazy_span_node!(
    LazyKindBoundSpan,
    ast::KindBound,
    @node {
        (abs, abs, LazyKindBoundAbsSpan),
        (mono, mono, LazyKindBoundMonoSpan),
    }
);

define_lazy_span_node!(
    LazyKindBoundAbsSpan,
    ast::KindBoundAbs,
    @token {
        (arrow, arrow),
    }
    @node {
        (lhs, lhs, LazyKindBoundSpan),
        (rhs, rhs, LazyKindBoundSpan),
    }
);

define_lazy_span_node! {LazyKindBoundMonoSpan, ast::LazyKindBoundMono}
