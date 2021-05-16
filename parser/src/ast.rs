use serde::{Deserialize, Serialize};

use crate::node::Node;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Module {
    pub body: Vec<Node<ModuleStmt>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ModuleStmt {
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
    Base {
        base: String,
    },
    Array {
        typ: Box<Node<TypeDesc>>,
        dimension: usize,
    },
    Tuple {
        items: Vec<Node<TypeDesc>>,
    },
    Generic {
        base: Node<String>,
        args: Vec<Node<GenericArg>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum GenericArg {
    TypeDesc(TypeDesc),
    Int(usize),
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
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum CallArg {
    Arg(Node<Expr>),
    Kwarg(Kwarg), // TODO: inline Kwarg struct here
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Kwarg {
    pub name: Node<String>,
    pub value: Box<Node<Expr>>,
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
