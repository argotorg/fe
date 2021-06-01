use crate::node::Node;
use fe_common::{Span, Spanned};
use serde::{Deserialize, Serialize};
use std::fmt;
use vec1::Vec1;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Module {
    pub body: Vec<Node<ModuleStmt>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ModuleStmt {
    Pragma {
        version_requirement: Node<String>,
    },
    TypeDef {
        name: Node<String>,
        typ: Node<TypeDesc>,
    },
    SimpleImport {
        names: Vec<Node<SimpleImportName>>,
    },
    FromImport {
        path: Node<FromImportPath>,
        names: Node<FromImportNames>,
    },
    ContractDef {
        name: Node<String>,
        fields: Vec<Node<Field>>,
        body: Vec<Node<ContractStmt>>,
    },
    StructDef {
        name: Node<String>,
        fields: Vec<Node<Field>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum TypeDesc {
    Unit,
    Base {
        base: String,
    },
    Array {
        typ: Box<Node<TypeDesc>>,
        dimension: usize,
    },
    Tuple {
        items: Vec1<Node<TypeDesc>>,
    },
    Generic {
        base: Node<String>,
        args: Node<Vec<GenericArg>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum GenericArg {
    TypeDesc(Node<TypeDesc>),
    Int(Node<usize>),
}
impl Spanned for GenericArg {
    fn span(&self) -> Span {
        match self {
            GenericArg::TypeDesc(node) => node.span,
            GenericArg::Int(node) => node.span,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct SimpleImportName {
    pub path: Vec<Node<String>>,
    pub alias: Option<Node<String>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum FromImportPath {
    Absolute {
        path: Vec<Node<String>>,
    },
    Relative {
        parent_level: usize,
        path: Vec<Node<String>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum FromImportNames {
    Star,
    List(Vec<Node<FromImportName>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct FromImportName {
    pub name: Node<String>,
    pub alias: Option<Node<String>>,
}

/// struct or contract field, with optional 'pub' and 'const' qualifiers
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Field {
    pub is_pub: bool,
    pub is_const: bool,
    pub name: Node<String>,
    pub typ: Node<TypeDesc>,
    pub value: Option<Node<Expr>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ContractStmt {
    EventDef {
        name: Node<String>,
        fields: Vec<Node<EventField>>,
    },
    FuncDef {
        is_pub: bool,
        name: Node<String>,
        args: Vec<Node<FuncDefArg>>,
        return_type: Option<Node<TypeDesc>>,
        body: Vec<Node<FuncStmt>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct EventField {
    pub is_idx: bool,
    pub name: Node<String>,
    pub typ: Node<TypeDesc>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct FuncDefArg {
    pub name: Node<String>,
    pub typ: Node<TypeDesc>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum FuncStmt {
    Return {
        value: Option<Node<Expr>>,
    },
    VarDecl {
        target: Node<VarDeclTarget>,
        typ: Node<TypeDesc>,
        value: Option<Node<Expr>>,
    },
    Assign {
        target: Node<Expr>,
        value: Node<Expr>,
    },
    AugAssign {
        target: Node<Expr>,
        op: Node<BinOperator>,
        value: Node<Expr>,
    },
    For {
        target: Node<String>,
        iter: Node<Expr>,
        body: Vec<Node<FuncStmt>>,
    },
    While {
        test: Node<Expr>,
        body: Vec<Node<FuncStmt>>,
    },
    If {
        test: Node<Expr>,
        body: Vec<Node<FuncStmt>>,
        or_else: Vec<Node<FuncStmt>>,
    },
    Assert {
        test: Node<Expr>,
        msg: Option<Node<Expr>>,
    },
    Emit {
        name: Node<String>,
        args: Node<Vec<Node<CallArg>>>,
    },
    Expr {
        value: Node<Expr>,
    },
    Pass,
    Break,
    Continue,
    Revert,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum VarDeclTarget {
    Name(String),
    Tuple(Vec<Node<VarDeclTarget>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Expr {
    Ternary {
        if_expr: Box<Node<Expr>>,
        test: Box<Node<Expr>>,
        else_expr: Box<Node<Expr>>,
    },
    BoolOperation {
        left: Box<Node<Expr>>,
        op: Node<BoolOperator>,
        right: Box<Node<Expr>>,
    },
    BinOperation {
        left: Box<Node<Expr>>,
        op: Node<BinOperator>,
        right: Box<Node<Expr>>,
    },
    UnaryOperation {
        op: Node<UnaryOperator>,
        operand: Box<Node<Expr>>,
    },
    CompOperation {
        left: Box<Node<Expr>>,
        op: Node<CompOperator>,
        right: Box<Node<Expr>>,
    },
    Attribute {
        value: Box<Node<Expr>>,
        attr: Node<String>,
    },
    Subscript {
        value: Box<Node<Expr>>,
        index: Box<Node<Expr>>,
    },
    Call {
        func: Box<Node<Expr>>,
        generic_args: Option<Node<Vec<GenericArg>>>,
        args: Node<Vec<Node<CallArg>>>,
    },
    List {
        elts: Vec<Node<Expr>>,
    },
    Tuple {
        elts: Vec<Node<Expr>>,
    },
    Bool(bool),
    Name(String),
    Num(String),
    Str(String),
    Unit,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct CallArg {
    pub label: Option<Node<String>>,
    pub value: Node<Expr>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum BoolOperator {
    And,
    Or,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum BinOperator {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Pow,
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Invert,
    Not,
    UAdd,
    USub,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum CompOperator {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
}

impl fmt::Display for BoolOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BoolOperator::*;
        match self {
            And => write!(f, "and"),
            Or => write!(f, "or"),
        }
    }
}

impl fmt::Display for BinOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinOperator::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mult => write!(f, "*"),
            Div => write!(f, "/"),
            Mod => write!(f, "%"),
            Pow => write!(f, "**"),
            LShift => write!(f, "<<"),
            RShift => write!(f, ">>"),
            BitOr => write!(f, "|"),
            BitXor => write!(f, "^"),
            BitAnd => write!(f, "&"),
        }
    }
}

impl fmt::Display for CompOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CompOperator::*;
        match self {
            Eq => write!(f, "=="),
            NotEq => write!(f, "!="),
            Lt => write!(f, "<"),
            LtE => write!(f, "<="),
            Gt => write!(f, ">"),
            GtE => write!(f, ">="),
        }
    }
}
