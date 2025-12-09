use parser::ast;

use super::{
    LazySpanAtom, define_lazy_span_node,
    params::{LazyGenericArgListSpan, LazyTraitRefSpan},
    types::LazyTySpan,
};

define_lazy_span_node!(
    LazyPathSpan,
    ast::Path,
    @idx {
        (segment, LazyPathSegmentSpan),
    }
);

define_lazy_span_node!(
    LazyPathSegmentSpan,
    ast::PathSegment,
    @token {
        (ident, ident),
    }
    @node {
        (generic_args, generic_args, LazyGenericArgListSpan),
        (qualified_type, qualified_type, LazyQualifiedTypeSpan),
    }
);
impl<'db> LazyPathSegmentSpan<'db> {
    pub fn into_atom(self) -> LazySpanAtom<'db> {
        LazySpanAtom(self.0)
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
