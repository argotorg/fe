use cranelift_entity::entity_impl;

use super::{
    Body, GenericArgListId, IdentId, IntegerId, LitKind, Partial, PatId, PathId, StmtId, TypeId,
};
use crate::{HirDb, span::expr::LazyExprSpan};

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum Expr<'db> {
    Lit(LitKind<'db>),
    Block(Vec<StmtId>),
    /// The first `ExprId` is the lhs, the second is the rhs.
    Bin(ExprId, ExprId, BinOp),
    Un(ExprId, UnOp),
    /// `expr as Type`
    Cast(ExprId, Partial<TypeId<'db>>),
    /// (callee, call args)
    Call(ExprId, Vec<CallArg<'db>>),
    /// (receiver, method_name, generic args, call args)
    MethodCall(
        ExprId,
        Partial<IdentId<'db>>,
        GenericArgListId<'db>,
        Vec<CallArg<'db>>,
    ),
    Path(Partial<PathId<'db>>),
    /// The record construction expression.
    /// The fist `PathId` is the record type, the second is the record fields.
    RecordInit(Partial<PathId<'db>>, Vec<Field<'db>>),
    Field(ExprId, Partial<FieldIndex<'db>>),
    Tuple(Vec<ExprId>),
    Array(Vec<ExprId>),

    /// The size of the rep should be the body instead of expression, because it
    /// should be resolved as a constant expression.
    ArrayRep(ExprId, Partial<Body<'db>>),

    /// The first `ExprId` is the condition, the second is the then branch, the
    /// third is the else branch.
    /// In case `else if`, the third is the lowered into `If` expression.
    If(ExprId, ExprId, Option<ExprId>),

    /// The first `ExprId` is the scrutinee, the second is the arms.
    Match(ExprId, Partial<Vec<MatchArm>>),

    /// The `Assign` Expression. The first `ExprId` is the destination of the
    /// assignment, and the second `ExprId` is the rhs value of the binding.
    Assign(ExprId, ExprId),

    AugAssign(ExprId, ExprId, ArithBinOp),

    /// `with (K = v, ..) { body }`
    With(Vec<WithBinding<'db>>, ExprId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub struct ExprId(u32);
entity_impl!(ExprId);

impl ExprId {
    pub fn span(self, body: Body) -> LazyExprSpan {
        LazyExprSpan::new(body, self)
    }

    pub fn data<'db>(self, db: &'db dyn HirDb, body: Body<'db>) -> &'db Partial<Expr<'db>> {
        &body.exprs(db)[self]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum FieldIndex<'db> {
    /// The field is indexed by its name.
    /// `field.foo`.
    Ident(IdentId<'db>),
    /// The field is indexed by its integer.
    /// `field.0`.
    Index(IntegerId<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct MatchArm {
    pub pat: PatId,
    pub body: ExprId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, salsa::Update)]
pub enum BinOp {
    Arith(ArithBinOp),
    Comp(CompBinOp),
    Logical(LogicalBinOp),
    /// `[]`
    Index,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArithBinOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
    /// `**`
    Pow,
    /// `<<`
    LShift,
    /// `>>`
    RShift,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `^`
    BitXor,
    /// `..`
    Range,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompBinOp {
    /// `==`
    Eq,
    /// `!=`
    NotEq,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>`
    Gt,
    /// `>=`
    GtEq,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LogicalBinOp {
    /// `&&`
    And,
    /// `||`
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `!`
    Not,
    /// `~`
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct CallArg<'db> {
    pub label: Option<IdentId<'db>>,
    pub expr: ExprId,
}

impl<'db> CallArg<'db> {
    /// Returns the label of the argument if
    /// 1. the argument has an explicit label. or
    /// 2. If 1. is not true, then the argument is labeled when the expression
    ///    is a path expression and the path is an identifier.
    pub fn label_eagerly(&self, db: &'db dyn HirDb, body: Body<'db>) -> Option<IdentId<'db>> {
        if let Some(label) = self.label {
            return Some(label);
        };

        let Partial::Present(Expr::Path(Partial::Present(path))) = self.expr.data(db, body) else {
            return None;
        };

        path.as_ident(db)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct Field<'db> {
    pub label: Option<IdentId<'db>>,
    pub expr: ExprId,
}

impl<'db> Field<'db> {
    /// Returns the label of the field if
    /// 1. the filed has an explicit label. or
    /// 2. If 1. is not true, then the field is labeled when the expression is a
    ///    path expression and the path is an identifier.
    pub fn label_eagerly(&self, db: &'db dyn HirDb, body: Body<'db>) -> Option<IdentId<'db>> {
        if let Some(label) = self.label {
            return Some(label);
        };

        let Partial::Present(Expr::Path(Partial::Present(path))) = self.expr.data(db, body) else {
            return None;
        };

        path.as_ident(db)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct WithBinding<'db> {
    /// Effect key path (e.g. `Ctx` / `Storage<u8>`). When absent, the binding is
    /// shorthand and the key is inferred from the bound value and effect usage.
    pub key_path: Option<Partial<PathId<'db>>>,
    pub value: ExprId,
}
