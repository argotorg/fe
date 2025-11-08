use parser::ast;

use super::define_lazy_span_node;

define_lazy_span_node!(
    LazyAttrListSpan,
    ast::AttrList,
    @idx {
        (attr, LazyAttrSpan),
    }
);

define_lazy_span_node!(LazyAttrSpan);
impl<'db> LazyAttrSpan<'db> {
    pub fn into_normal_attr(self) -> LazyNormalAttrSpan<'db> {
        LazyNormalAttrSpan(self.0)
    }

    pub fn into_doc_comment_attr(self) -> LazyDocCommentAttrSpan<'db> {
        LazyDocCommentAttrSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyNormalAttrSpan,
    ast::NormalAttr,
    @token {
        (name, name),
    }
    @node {
        (args, args, LazyAttrArgListSpan),
    }
);

define_lazy_span_node!(
    LazyDocCommentAttrSpan,
    ast::DocCommentAttr,
    @token {
        (doc, doc),
    }
);

define_lazy_span_node!(
    LazyAttrArgListSpan,
    ast::AttrArgList,
    @idx {
        (arg, LazyAttrArgSpan),
    }
);

define_lazy_span_node!(
    LazyAttrArgSpan,
    ast::AttrArg,
    @token {
        (key, key),
        (value, value),
    }
);
