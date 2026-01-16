use common::diagnostics::Span;
use parser::ast::{self, AstPtr, SyntaxNodePtr, prelude::*};
use salsa::Update;

use crate::{
    HirDb, SpannedHirDb,
    core::hir_def::{
        Body, Const, Contract, Enum, Func, Impl, ImplTrait, Mod, Struct, TopLevelMod, Trait,
        TypeAlias, Use,
    },
    core::lower::top_mod_ast,
};

pub mod attr;
pub mod expr;
pub mod item;
pub mod params;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod types;
pub mod use_tree;

pub(crate) mod transition;

pub mod lazy_spans {
    pub use super::{
        DynLazySpan, LazyLitSpan, LazySpan, LazySpanAtom,
        attr::{
            LazyAttrArgListSpan, LazyAttrArgSpan, LazyAttrListSpan, LazyAttrSpan,
            LazyDocCommentAttrSpan, LazyNormalAttrSpan,
        },
        expr::{
            LazyAssignExprSpan, LazyAugAssignExprSpan, LazyBinExprSpan, LazyCallArgListSpan,
            LazyCallArgSpan, LazyCallExprSpan, LazyCastExprSpan, LazyExprSpan, LazyFieldExprSpan,
            LazyFieldListSpan, LazyFieldSpan, LazyLitExprSpan, LazyMatchArmListSpan,
            LazyMatchArmSpan, LazyMatchExprSpan, LazyMethodCallExprSpan, LazyPathExprSpan,
            LazyRecordInitExprSpan, LazyUnExprSpan,
        },
        item::{
            LazyBodySpan, LazyConstSpan, LazyContractRecvSpan, LazyContractSpan, LazyEnumSpan,
            LazyFieldDefListSpan, LazyFieldDefSpan, LazyFuncSignatureSpan, LazyFuncSpan,
            LazyImplSpan, LazyImplTraitSpan, LazyItemModifierSpan, LazyItemSpan, LazyModSpan,
            LazyRecvArmListSpan, LazyRecvArmSpan, LazyStructSpan, LazyTopModSpan, LazyTraitSpan,
            LazyTypeAliasSpan, LazyUseSpan, LazyVariantDefListSpan, LazyVariantDefSpan,
        },
        params::{
            LazyConstGenericParamSpan, LazyFuncParamListSpan, LazyFuncParamSpan,
            LazyGenericArgListSpan, LazyGenericArgSpan, LazyGenericParamListSpan,
            LazyGenericParamSpan, LazyKindBoundSpan, LazyTraitRefSpan, LazyTypeBoundListSpan,
            LazyTypeBoundSpan, LazyTypeGenericArgSpan, LazyUsesClauseSpan, LazyUsesParamListSpan,
            LazyUsesParamSpan, LazyWhereClauseSpan, LazyWherePredicateSpan,
        },
        pat::{
            LazyLitPatSpan, LazyPatSpan, LazyPathPatSpan, LazyPathTuplePatSpan,
            LazyRecordPatFieldListSpan, LazyRecordPatFieldSpan, LazyRecordPatSpan,
        },
        path::{LazyPathSegmentSpan, LazyPathSpan},
        stmt::{LazyLetStmtSpan, LazyStmtSpan},
        types::{
            LazyArrayTypeSpan, LazyPathTypeSpan, LazyPtrTypeSpan, LazyTupleTypeSpan, LazyTySpan,
        },
        use_tree::{LazyUseAliasSpan, LazyUsePathSegmentSpan, LazyUsePathSpan},
    };
}

/// This struct represents a dynamic lazy span, which can be converted from all
/// types that implement [`LazySpan`] in this module. We want to avoid `dyn
/// LazySpan` usage because it doesn't implement `Clone` and `Eq` which leads to
/// a lot of difficulties in salsa integration
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct DynLazySpan<'db>(pub(super) Option<SpanTransitionChain<'db>>);
impl<'db> DynLazySpan<'db> {
    pub fn invalid() -> Self {
        Self(None)
    }

    pub fn top_mod(&self, db: &'db dyn HirDb) -> Option<TopLevelMod<'db>> {
        self.0.as_ref().map(|chain| chain.top_mod(db))
    }
}
impl LazySpan for DynLazySpan<'_> {
    fn resolve(&self, db: &dyn SpannedHirDb) -> Option<Span> {
        if let Some(chain) = &self.0 {
            chain.resolve(db)
        } else {
            None
        }
    }
}

pub trait SpanDowncast<'db> {
    fn downcast(dyn_span: DynLazySpan<'db>) -> Option<Self>
    where
        Self: Sized;
}

/// The trait provides a way to extract [`Span`](common::diagnostics::Span) from
/// types which don't have a span information directly, but can be resolved into
/// a span lazily.
pub trait LazySpan {
    fn resolve(&self, db: &dyn SpannedHirDb) -> Option<Span>;
}

pub fn toplevel_ast(db: &dyn SpannedHirDb, item: TopLevelMod) -> HirOrigin<ast::Root> {
    HirOrigin::raw(&top_mod_ast(db, item))
}

pub fn mod_ast<'db>(db: &'db dyn SpannedHirDb, item: Mod<'db>) -> &'db HirOrigin<ast::Mod> {
    item.origin(db)
}

pub fn func_ast<'db>(db: &'db dyn SpannedHirDb, item: Func<'db>) -> &'db HirOrigin<ast::Func> {
    item.origin(db)
}

pub fn struct_ast<'db>(
    db: &'db dyn SpannedHirDb,
    item: Struct<'db>,
) -> &'db HirOrigin<ast::Struct> {
    item.origin(db)
}

pub fn contract_ast<'db>(
    db: &'db dyn SpannedHirDb,
    item: Contract<'db>,
) -> &'db HirOrigin<ast::Contract> {
    item.origin(db)
}

pub fn enum_ast<'db>(db: &'db dyn SpannedHirDb, item: Enum<'db>) -> &'db HirOrigin<ast::Enum> {
    item.origin(db)
}

pub fn type_alias_ast<'db>(
    db: &'db dyn SpannedHirDb,
    item: TypeAlias<'db>,
) -> &'db HirOrigin<ast::TypeAlias> {
    item.origin(db)
}

pub fn impl_ast<'db>(db: &'db dyn SpannedHirDb, item: Impl<'db>) -> &'db HirOrigin<ast::Impl> {
    item.origin(db)
}

pub fn trait_ast<'db>(db: &'db dyn SpannedHirDb, item: Trait<'db>) -> &'db HirOrigin<ast::Trait> {
    item.origin(db)
}

pub fn impl_trait_ast<'db>(
    db: &'db dyn SpannedHirDb,
    item: ImplTrait<'db>,
) -> &'db HirOrigin<ast::ImplTrait> {
    item.origin(db)
}

pub fn const_ast<'db>(db: &'db dyn SpannedHirDb, item: Const<'db>) -> &'db HirOrigin<ast::Const> {
    item.origin(db)
}

pub fn use_ast<'db>(db: &'db dyn SpannedHirDb, item: Use<'db>) -> &'db HirOrigin<ast::Use> {
    item.origin(db)
}

pub fn body_ast<'db>(db: &'db dyn SpannedHirDb, item: Body<'db>) -> &'db HirOrigin<ast::Expr> {
    item.origin(db)
}

pub fn body_source_map<'db>(
    db: &'db dyn SpannedHirDb,
    item: Body<'db>,
) -> &'db crate::core::hir_def::BodySourceMap {
    item.source_map(db)
}

/// This enum represents the origin of the HIR node in a file.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum HirOrigin<T>
where
    T: AstNode,
{
    /// The HIR node is created by direct lowering from the corresponding AST.
    Raw(AstPtr<T>),
    /// The HIR node is created by expanding attributes.
    /// The `SyntaxNode` points to the callsite of the attribute.
    Expanded(SyntaxNodePtr),
    /// The HIR node is the result of desugaring in the lower phase from AST to
    /// HIR. e.g., `a += b` is desugared into `a = a + b`.
    Desugared(DesugaredOrigin),

    /// The HIR node is created by the compiler and not directly from the AST.
    /// This is used for nodes that conceptually exist in user code (such as
    /// implicit imports) but don't have a concrete syntax location.
    Synthetic,

    /// The HIR node is created by the compiler and not directly from the AST.
    /// This is only used with `Invalid` nodes that don't have a corresponding
    /// AST node.
    /// e.g., the RHS of `a + ` is represented as `Invalid` node but there is no
    /// corresponding origin.
    #[default]
    None,
}

impl<T> HirOrigin<T>
where
    T: AstNode<Language = parser::FeLang>,
{
    pub(crate) fn raw(ast: &T) -> Self {
        Self::Raw(AstPtr::new(ast))
    }

    pub(crate) fn desugared(origin: impl Into<DesugaredOrigin>) -> Self {
        Self::Desugared(origin.into())
    }

    pub(crate) fn synthetic() -> Self {
        Self::Synthetic
    }
}

/// This enum represents the origin of the HIR node which is desugared into
/// other HIR node kinds.
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum DesugaredOrigin {
    /// The HIR node is the result of desugaring a AST use.
    /// In HIR lowering, nested use tree is flattened into a single use path.
    Use(UseDesugared),
    /// The HIR node is the result of desugaring a `msg` block.
    /// `msg` blocks are desugared into modules containing structs and trait impls.
    Msg(MsgDesugared),
}

/// Tracks the origin of HIR nodes desugared from a `msg` block.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MsgDesugared {
    /// The original `msg` AST node.
    pub msg: AstPtr<ast::Msg>,
    /// If this is a desugared variant, the index of the variant.
    pub variant_idx: Option<usize>,
    /// Which part of the msg/variant to point to.
    pub focus: MsgDesugaredFocus,
}

/// Specifies which part of a desugared msg block to point to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum MsgDesugaredFocus {
    /// Point to the entire msg block (or variant if variant_idx is set).
    #[default]
    Block,
    /// Point to the variant name.
    VariantName,
    /// Point to the selector attribute value (if any).
    Selector,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseDesugared {
    pub root: AstPtr<ast::Use>,
    pub path: Vec<AstPtr<ast::UsePathSegment>>,
    pub alias: Option<AstPtr<ast::UseAlias>>,
    focus: DesugaredUseFocus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum DesugaredUseFocus {
    Root,
    Path,
    Alias,
}

impl UseDesugared {
    pub(super) fn new(ast: &ast::Use) -> Self {
        Self {
            root: AstPtr::new(ast),
            path: vec![],
            alias: None,
            focus: DesugaredUseFocus::Root,
        }
    }

    pub(super) fn add_alias(&mut self, alias: &ast::UseAlias) {
        self.alias = Some(AstPtr::new(alias))
    }

    pub(super) fn push_seg(&mut self, seg: &ast::UsePathSegment) {
        self.path.push(AstPtr::new(seg));
    }
}

use transition::define_lazy_span_node;

use self::transition::SpanTransitionChain;

define_lazy_span_node!(LazySpanAtom);
impl<'db> LazySpanAtom<'db> {
    pub(super) fn into_lit_span(self) -> LazyLitSpan<'db> {
        LazyLitSpan(self.0)
    }
}
define_lazy_span_node!(LazyLitSpan);
