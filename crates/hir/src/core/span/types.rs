use parser::ast;

use super::define_lazy_span_node;
use crate::span::{item::LazyBodySpan, path::LazyPathSpan};

define_lazy_span_node!(LazyTySpan);
impl<'db> LazyTySpan<'db> {
    /// Convert this [`LazyTySpan`] into a [`LazyPathTypeSpan`].
    ///
    /// If the type that is pointed to by this is not a path type, the result
    /// span will point to the same span of the original type.
    pub fn into_path_type(self) -> LazyPathTypeSpan<'db> {
        LazyPathTypeSpan(self.0)
    }

    /// Convert this [`LazyTySpan`] into a [`LazyPtrTypeSpan`].
    ///
    /// If the type that is pointed to by this is not a pointer type, the result
    /// span will point to the same span of the original type.
    pub fn into_ptr_type(self) -> LazyPtrTypeSpan<'db> {
        LazyPtrTypeSpan(self.0)
    }

    /// Convert this [`LazyTySpan`] into a [`LazyTupleTypeSpan`].
    ///
    /// If the type that is pointed to by this is not a tuple type, the result
    /// span will point to the same span of the original type.
    pub fn into_tuple_type(self) -> LazyTupleTypeSpan<'db> {
        LazyTupleTypeSpan(self.0)
    }

    /// convert this [`LazyTySpan`] into a [`LazyArrayTypeSpan`].
    ///
    /// If the type that is pointed to by this is not an array type, the result
    /// span will point to the same span of the original type.
    pub fn into_array_type(self) -> LazyArrayTypeSpan<'db> {
        LazyArrayTypeSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyPtrTypeSpan,
    ast::PtrType,
    @token {
        (star, star),
    }
    @node {
        (pointee, inner, LazyTySpan),
    }
);

define_lazy_span_node!
(
    LazyPathTypeSpan,
    ast::PathType,
    @node {
        (path, path, LazyPathSpan),
    }
);

define_lazy_span_node!(
    LazyTupleTypeSpan,
    ast::TupleType,
    @token {
        (l_paren, l_paren),
        (r_paren, r_paren),
    }
    @idx {
        (elem_ty, LazyTySpan),
    }
);

define_lazy_span_node!(
    LazyArrayTypeSpan,
    ast::ArrayType,
    @token {
        (l_bracket, l_bracket),
        (r_bracket, r_bracket),
    }
    @node {
        (elem, elem_ty, LazyTySpan),
        (len, len, LazyBodySpan),
    }
);
