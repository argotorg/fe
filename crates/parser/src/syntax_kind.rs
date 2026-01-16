//! This module contains the definition of the [`SyntaxKind`].

use logos::Logos;
use num_enum::TryFromPrimitive;

/// The definition of the `SyntaxKind'.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Logos, TryFromPrimitive)]
#[repr(u16)]
pub enum SyntaxKind {
    // Atom kinds. These are leaf nodes.
    InvalidToken = 0,

    #[regex(r"(\n|\r\n|\r)+")]
    Newline,
    #[regex(r"[ ]+")]
    WhiteSpace,
    /// `foo`
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
    /// `1`, `0b1010`, `0o77`, `0xff`
    #[regex("[0-9]+(?:_[0-9]+)*")]
    #[regex("0[bB][0-1]+")]
    #[regex("0[oO][0-7]+")]
    #[regex("0[xX][0-9a-fA-F]+")]
    Int,
    /// "MyString"
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    /// `(`
    #[token("(")]
    LParen,
    /// `)`
    #[token(")")]
    RParen,
    /// `{`
    #[token("{")]
    LBrace,
    /// `}`
    #[token("}")]
    RBrace,
    /// `[`
    #[token("[")]
    LBracket,
    /// `]`
    #[token("]")]
    RBracket,
    /// `:`
    #[token(":")]
    Colon,
    /// `::`
    #[token("::")]
    Colon2,
    /// `;`
    #[token(";")]
    SemiColon,
    /// `.`
    #[token(".")]
    Dot,
    /// `..`
    #[token("..")]
    Dot2,
    /// `,`
    #[token(",")]
    Comma,
    /// `->`
    #[token("->")]
    Arrow,
    /// `=>`
    #[token("=>")]
    FatArrow,
    /// `_`
    #[token("_", priority = 3)]
    Underscore,
    /// `#`
    #[token("#")]
    Pound,
    /// `// Comment`
    #[regex(r"//[^\n\r]*")]
    Comment,
    /// `/// DocComment`
    #[regex(r"///[^\n\r]*")]
    DocComment,

    /// `+`
    #[token("+")]
    Plus,
    /// `-`
    #[token("-")]
    Minus,
    /// `*`
    #[token("*")]
    Star,
    #[token("**")]
    Star2,
    /// `/`
    #[token("/")]
    Slash,
    /// `%`
    #[token("%")]
    Percent,
    #[token("~")]
    Tilde,
    #[token("!")]
    Not,
    #[token("^")]
    Hat,
    /// `&`
    #[token("&")]
    Amp,
    /// `&&`
    #[token("&&")]
    Amp2,
    /// `|`
    #[token("|")]
    Pipe,
    /// `||`
    #[token("||")]
    Pipe2,
    /// `<`
    #[token("<")]
    Lt,
    /// `>`
    #[token(">")]
    Gt,
    /// `=`
    #[token("=")]
    Eq,
    /// `==`
    #[token("==")]
    Eq2,
    /// `!=`
    #[token("!=")]
    NotEq,

    /// `as`'
    #[token("as")]
    AsKw,
    /// `true'
    #[token("true")]
    TrueKw,
    /// `false`
    #[token("false")]
    FalseKw,
    /// `break`
    #[token("break")]
    BreakKw,
    /// `uses`
    #[token("uses")]
    UsesKw,
    /// `continue`
    #[token("continue")]
    ContinueKw,
    /// `contract`
    #[token("contract")]
    ContractKw,
    /// `msg`
    #[token("msg")]
    MsgKw,
    /// `fn`
    #[token("fn")]
    FnKw,
    /// `mod`
    #[token("mod")]
    ModKw,
    /// `const`
    #[token("const")]
    ConstKw,
    /// `if`
    #[token("if")]
    IfKw,
    /// `else`
    #[token("else")]
    ElseKw,
    /// `match`
    #[token("match")]
    MatchKw,
    /// `for`
    #[token("for")]
    ForKw,
    /// `in`
    #[token("in")]
    InKw,
    /// `where`
    #[token("where")]
    WhereKw,
    /// `while`
    #[token("while")]
    WhileKw,
    /// `pub`
    #[token("pub")]
    PubKw,
    /// `return`
    #[token("return")]
    ReturnKw,
    /// `self`
    #[token("self")]
    SelfKw,
    #[token("Self")]
    SelfTypeKw,
    /// `struct`
    #[token("struct")]
    StructKw,
    /// `enum`
    #[token("enum")]
    EnumKw,
    /// `trait`
    #[token("trait")]
    TraitKw,
    /// `impl`
    #[token("impl")]
    ImplKw,
    /// `type`
    #[token("type")]
    TypeKw,
    /// `let`
    #[token("let")]
    LetKw,
    /// `mut`
    #[token("mut")]
    MutKw,
    /// `use`
    #[token("use")]
    UseKw,
    /// `extern`
    #[token("extern")]
    ExternKw,
    /// `unsafe`
    #[token("unsafe")]
    UnsafeKw,
    /// `ingot`
    #[token("ingot")]
    IngotKw,
    /// `super`
    #[token("super")]
    SuperKw,

    /// `<<`
    LShift,
    /// `>>`
    RShift,
    /// `<=`
    LtEq,
    /// `>=`
    GtEq,

    /// `1', `false`, `"String"
    Lit,

    // Expressions. These are non-leaf nodes.
    /// { statement-list }
    BlockExpr,
    /// `x + 1`
    BinExpr,
    /// `!x`
    UnExpr,
    /// `expr as Type`
    CastExpr,
    /// `foo(x, y)`
    CallExpr,
    /// `(arg: 1, y)`
    CallArgList,
    /// `arg: 1`, `y`
    CallArg,
    /// `foo.bar(x, y)`
    MethodCallExpr,
    /// `<i32, u256, N + 2>`
    GenericArgList,
    /// `T`
    TypeGenericArg,
    /// `1`
    ConstGenericArg,
    /// `Output = u64`
    AssocTypeGenericArg,
    /// `FOO::Bar`
    PathExpr,
    /// `Foo { x: 1, y: "String"` }`
    RecordInitExpr,
    /// `{ x: 1, y: "String"` }`
    RecordFieldList,
    /// `x: 1`
    RecordField,
    /// `foo.bar`, `foo.0`
    FieldExpr,
    /// `foo[1]`
    IndexExpr,
    /// `(x ,y)`
    TupleExpr,
    /// `[x, y, z]`
    ArrayExpr,
    /// `[x; 4]`
    ArrayRepExpr,
    /// `(Effect = value, ..)` list for `with`
    WithParamList,
    /// `Effect = value` entry for `with`
    WithParam,
    /// `1`
    LitExpr,
    /// `if x { 1 } else { 2 }`
    IfExpr,
    /// `match x { pat => { .. } }`
    MatchExpr,
    /// `with (Effect = value, ..) { Block }`
    WithExpr,
    /// `(1 + 2)`
    ParenExpr,
    /// x = 1
    AssignExpr,
    /// x += 1
    AugAssignExpr,

    // Statements. These are non-leaf nodes.
    /// `let x = 1`
    LetStmt,
    /// `for x in y {..}`
    ForStmt,
    /// `while expr {..}`
    WhileStmt,
    /// `continue`
    ContinueStmt,
    /// `break`
    BreakStmt,

    /// `return 1`
    ReturnStmt,
    /// `1`
    ExprStmt,

    // Patterns. These are non-leaf nodes.
    /// `_`
    WildCardPat,
    /// `..`
    RestPat,
    /// `x`
    LitPat,
    /// `(x, y)`
    TuplePat,
    /// `(x, y)`
    TuplePatElemList,
    /// `Enum::Variant`
    PathPat,
    /// `Enum::Variant(x, y)`
    PathTuplePat,
    /// `Struct {x, y}`
    RecordPat,
    /// `{a: b, y}`
    RecordPatFieldList,
    /// `a: b`
    RecordPatField,
    /// `pat1 | pat2`
    OrPat,

    // MatchArms.
    // `pat => { stmtlist }`
    MatchArm,
    MatchArmList,

    // Items. These are non-leaf nodes.
    Item,
    /// `mod s { .. }`
    Mod,
    /// `fn foo(x: i32) -> i32 { .. }`
    Func,
    /// `struct Foo { .. }`
    Struct,
    /// `contract Foo { .. }`
    Contract,
    /// leading fields inside a contract body
    ContractFields,
    /// `init(...) { ... }` block inside a contract
    ContractInit,
    /// `recv ... { ... }` block inside a contract
    ContractRecv,
    /// list of arms inside a recv block
    RecvArmList,
    /// `Pattern -> RetTy uses (...) { body }`
    RecvArm,
    /// `enum Foo { .. }`
    Enum,
    /// `type Foo = i32`
    TypeAlias,
    /// `impl Foo { .. }`
    Impl,
    /// `{ fn ... }`
    ImplItemList,
    /// `trait Foo {..}`
    Trait,
    /// `: Trait + Trait2`
    SuperTraitList,
    /// `type Foo` or `type Foo: SomeTrait = Bar`
    TraitTypeItem,
    /// `const FOO: Ty` or `const FOO: Ty = expr`
    TraitConstItem,
    /// `{ fn foo() {..} }`
    TraitItemList,
    /// `impl Trait for Foo { .. }`
    ImplTrait,
    /// `const FOO: i32 = 1`
    Const,
    /// `use foo::{Foo as Foo1, bar::Baz}`
    Use,
    /// `msg Erc20Msg { ... }`
    Msg,
    /// `foo::{Foo as Foo1, bar::Baz}`
    UseTree,
    /// `{Foo as Foo1, bar::Baz}`
    UseTreeList,
    /// `Foo::Bar`, `Foo::*`,`*`.
    UsePath,
    /// `Foo`, `self`
    UsePathSegment,
    /// `as Foo`
    UseTreeRename,
    /// `extern { .. }`
    Extern,
    /// `extern { .. }`
    ExternItemList,
    ItemList,

    // Types. These are non-leaf nodes.
    /// `*i32`
    PtrType,
    /// `foo::Type<T, U + 2>`
    PathType,
    /// `Self`
    SelfType,
    /// `(i32, foo::Bar)`
    TupleType,
    /// `[i32; 4]`
    ArrayType,
    /// `!`
    NeverType,

    // Paths. These are non-leaf nodes.
    /// `Segment1::Segment2`
    Path,
    /// `Segment1`
    PathSegment,
    /// `<Foo as SomeTrait>`
    QualifiedType,

    /// `#[attr]`
    Attr,
    /// `(key1: value1, key2: value2)`
    AttrArgList,
    /// `key: value`
    AttrArg,
    /// `value` in an attribute argument
    AttrArgValue,
    /// `/// Comment`
    DocCommentAttr,
    AttrList,

    /// `pub`
    Visibility,

    /// `x: i32`
    RecordFieldDef,
    /// `{x: i32, y: u32}`
    RecordFieldDefList,

    VariantDef,
    VariantDefList,

    /// `T`
    /// `T: Trait`
    TypeGenericParam,
    /// `const N: usize`
    ConstGenericParam,
    /// `<T: Trait, U, const N: usize>`
    GenericParamList,

    /// fn foo<T, U>(t: T) -> U
    FuncSignature,
    /// `(x: i32, _ y: mut i32)`
    FuncParamList,

    /// `_ x: mut i32`
    FnParam,

    /// `uses` clause attached to a function signature
    UsesClause,
    /// `uses` parameter list
    UsesParamList,
    /// a single `uses` parameter
    UsesParam,

    /// `foo::Trait1 + Trait2`
    TypeBoundList,
    /// `TraitBound` or `TypeKind`.
    TypeBound,
    /// `Trait1<Arg, ...>`
    TraitRef,
    /// `* -> *` or `(*-> *) -> *`
    KindBoundAbs,
    /// `*`.
    KindBoundMono,
    /// `where Option<T>: Trait1 + Trait2`
    WhereClause,
    /// `Option<T>: Trait1 + Trait2`
    WherePredicate,
    /// `Transfer { to: Address, amount: u256 } -> bool`
    MsgVariant,
    /// `{ to: Address, amount: u256 }`
    MsgVariantParams,
    /// `TotalSupply, Balance { addr: Address }`
    MsgVariantList,

    /// Root node of the input source.
    Root,

    /// Represents an error branch.
    Error,
}

impl SyntaxKind {
    /// Returns `true` if this is a trivia token.
    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::WhiteSpace | SyntaxKind::Comment)
    }

    /// Returns `true` if the token is a literal leaf.
    pub fn is_literal_leaf(self) -> bool {
        matches!(
            self,
            SyntaxKind::Int | SyntaxKind::String | SyntaxKind::TrueKw | SyntaxKind::FalseKw
        )
    }

    pub fn is_open_bracket_kind(self) -> bool {
        matches!(
            self,
            SyntaxKind::LBrace | SyntaxKind::LParen | SyntaxKind::LBracket | SyntaxKind::Lt
        )
    }

    /// Returns its corresponding open bracket kind if it is a close bracket
    /// kind.
    pub fn corresponding_open_bracket_kind(self) -> Option<Self> {
        match self {
            SyntaxKind::RBrace => Some(SyntaxKind::LBrace),
            SyntaxKind::RParen => Some(SyntaxKind::LParen),
            SyntaxKind::RBracket => Some(SyntaxKind::LBracket),
            SyntaxKind::Gt => Some(SyntaxKind::Lt),
            _ => None,
        }
    }

    pub(crate) fn is_modifier_head(self) -> bool {
        matches!(self, SyntaxKind::PubKw | SyntaxKind::UnsafeKw)
    }

    pub(crate) fn is_item_head(self) -> bool {
        self.is_modifier_head()
            || matches!(
                self,
                SyntaxKind::ModKw
                    | SyntaxKind::FnKw
                    | SyntaxKind::StructKw
                    | SyntaxKind::ContractKw
                    | SyntaxKind::MsgKw
                    | SyntaxKind::EnumKw
                    | SyntaxKind::TypeKw
                    | SyntaxKind::ImplKw
                    | SyntaxKind::TraitKw
                    | SyntaxKind::ConstKw
                    | SyntaxKind::UseKw
                    | SyntaxKind::ExternKw
            )
    }

    pub fn describe(self) -> &'static str {
        match self {
            SyntaxKind::Newline => "newline",
            SyntaxKind::Ident => "identifier",
            SyntaxKind::Int => "integer",
            SyntaxKind::String => "string literal",
            SyntaxKind::LParen => "`(`",
            SyntaxKind::RParen => "`)`",
            SyntaxKind::LBrace => "`{`",
            SyntaxKind::RBrace => "`}`",
            SyntaxKind::LBracket => "`[`",
            SyntaxKind::RBracket => "`]`",
            SyntaxKind::Colon => "`:`",
            SyntaxKind::Colon2 => "`::`",
            SyntaxKind::SemiColon => "`;`",
            SyntaxKind::Dot => "`.`",
            SyntaxKind::Dot2 => "`..`",
            SyntaxKind::Comma => "`,`",
            SyntaxKind::Arrow => "`->`",
            SyntaxKind::FatArrow => "`=>`",
            SyntaxKind::Underscore => "`_`",
            SyntaxKind::Pound => "`#`",
            SyntaxKind::Plus => "`+`",
            SyntaxKind::Minus => "`-`",
            SyntaxKind::Star => "`*`",
            SyntaxKind::Star2 => "`**`",
            SyntaxKind::Slash => "`/`",
            SyntaxKind::Percent => "`%`",
            SyntaxKind::Tilde => "`~`",
            SyntaxKind::Not => "`!`",
            SyntaxKind::Hat => "`^`",
            SyntaxKind::Amp => "`&`",
            SyntaxKind::Amp2 => "`&&`",
            SyntaxKind::Pipe => "`|`",
            SyntaxKind::Pipe2 => "`||`",
            SyntaxKind::Lt => "`<`",
            SyntaxKind::Gt => "`>`",
            SyntaxKind::Eq => "`=`",
            SyntaxKind::Eq2 => "`==`",
            SyntaxKind::NotEq => "`!=`",
            SyntaxKind::AsKw => "`as`",
            SyntaxKind::TrueKw => "`true`",
            SyntaxKind::FalseKw => "`false`",
            SyntaxKind::BreakKw => "`break`",
            SyntaxKind::UsesKw => "`uses`",
            SyntaxKind::ContinueKw => "`continue`",
            SyntaxKind::ContractKw => "`contract`",
            SyntaxKind::MsgKw => "`msg`",
            SyntaxKind::FnKw => "`fn`",
            SyntaxKind::ModKw => "`mod`",
            SyntaxKind::ConstKw => "`const`",
            SyntaxKind::IfKw => "`if`",
            SyntaxKind::ElseKw => "`else`",
            SyntaxKind::MatchKw => "`match`",
            SyntaxKind::ForKw => "`for`",
            SyntaxKind::InKw => "`in`",
            SyntaxKind::WhereKw => "`where`",
            SyntaxKind::WhileKw => "`while`",
            SyntaxKind::PubKw => "`pub`",
            SyntaxKind::ReturnKw => "`return`",
            SyntaxKind::SelfKw => "`self`",
            SyntaxKind::SelfTypeKw => "`Self`",
            SyntaxKind::StructKw => "`struct`",
            SyntaxKind::EnumKw => "`enum`",
            SyntaxKind::TraitKw => "`trait`",
            SyntaxKind::ImplKw => "`impl`",
            SyntaxKind::TypeKw => "`type`",
            SyntaxKind::LetKw => "`let`",
            SyntaxKind::MutKw => "`mut`",
            SyntaxKind::UseKw => "`use`",
            SyntaxKind::ExternKw => "`extern`",
            SyntaxKind::UnsafeKw => "`unsafe`",
            SyntaxKind::IngotKw => "`ingot`",
            SyntaxKind::SuperKw => "`super`",
            SyntaxKind::LShift => "`<<`",
            SyntaxKind::RShift => "`>>`",
            SyntaxKind::LtEq => "`<=`",
            SyntaxKind::GtEq => "`>=`",

            SyntaxKind::PathType => "type",
            SyntaxKind::TraitRef => "trait name",
            SyntaxKind::PathSegment => "path segment",
            SyntaxKind::PathPat => "pattern",
            SyntaxKind::QualifiedType => "qualified type",

            SyntaxKind::ArrayExpr => "array definition",
            SyntaxKind::RecordFieldDef => "field",
            SyntaxKind::IndexExpr => "index expression",
            SyntaxKind::BlockExpr => "block",
            SyntaxKind::TypeBound => "type bound",
            SyntaxKind::CallArgList => "function call arguments",

            SyntaxKind::InvalidToken => unimplemented!(),
            SyntaxKind::WhiteSpace => "whitespace",
            SyntaxKind::Comment => "comment",
            SyntaxKind::DocComment => "doc comment",

            SyntaxKind::Lit => "literal",
            SyntaxKind::BinExpr => "binary expression",
            SyntaxKind::UnExpr => "unary expression",
            SyntaxKind::CastExpr => "cast expression",
            SyntaxKind::CallExpr => "function call expression",
            SyntaxKind::CallArg => "function call argument",
            SyntaxKind::MethodCallExpr => "method call expression",
            SyntaxKind::GenericArgList => "generic type argument list",
            SyntaxKind::TypeGenericArg => "generic type argument",
            SyntaxKind::ConstGenericArg => "generic const argument",
            SyntaxKind::AssocTypeGenericArg => "associated type argument",
            SyntaxKind::PathExpr => "path",
            SyntaxKind::RecordInitExpr => "record initialization expression",
            SyntaxKind::RecordFieldList => "record field list",
            SyntaxKind::RecordField => "field",
            SyntaxKind::FieldExpr => "field",
            SyntaxKind::TupleExpr => "tuple expression",
            SyntaxKind::ArrayRepExpr => "array expression",
            SyntaxKind::WithParamList => "`with` parameter list",
            SyntaxKind::WithParam => "`with` parameter",
            SyntaxKind::LitExpr => "literal expression",
            SyntaxKind::IfExpr => "`if` expression",
            SyntaxKind::MatchExpr => "`match` expression",
            SyntaxKind::WithExpr => "`with` expression",
            SyntaxKind::ParenExpr => "parenthesized expression",
            SyntaxKind::AssignExpr => "assignment expression",
            SyntaxKind::AugAssignExpr => "augmented assignment expression",
            SyntaxKind::LetStmt => "`let` statement",
            SyntaxKind::ForStmt => "`for` statement",
            SyntaxKind::WhileStmt => "`while` statement",
            SyntaxKind::ContinueStmt => "`continue` statement",
            SyntaxKind::BreakStmt => "`break` statement",
            SyntaxKind::ReturnStmt => "`return` statement",
            SyntaxKind::ExprStmt => "`expr` statement",
            SyntaxKind::WildCardPat => "wildcard pattern",
            SyntaxKind::RestPat => "`..` pattern",
            SyntaxKind::LitPat => "literal pattern",
            SyntaxKind::TuplePat => "tuple pattern",
            SyntaxKind::TuplePatElemList => "tuple pattern element list",
            SyntaxKind::PathTuplePat => "path tuple pattern",
            SyntaxKind::RecordPat => "record pattern",
            SyntaxKind::RecordPatFieldList => "record pattern field list",
            SyntaxKind::RecordPatField => "record pattern field",
            SyntaxKind::OrPat => "`or` pattern",
            SyntaxKind::MatchArm => "`match` arm",
            SyntaxKind::MatchArmList => "`match` arm list",
            SyntaxKind::Item => "item",
            SyntaxKind::Mod => "`mod`",
            SyntaxKind::Func => "function definition",
            SyntaxKind::Struct => "struct definition",
            SyntaxKind::Contract => "contract definition",
            SyntaxKind::ContractFields => "contract fields",
            SyntaxKind::ContractInit => "contract init block",
            SyntaxKind::ContractRecv => "contract recv block",
            SyntaxKind::Msg => "message definition",
            SyntaxKind::MsgVariant => "message variant",
            SyntaxKind::MsgVariantParams => "message variant parameters",
            SyntaxKind::MsgVariantList => "message variants",
            SyntaxKind::Enum => "enum definition",
            SyntaxKind::TypeAlias => "type alias",
            SyntaxKind::Impl => "`impl` block",
            SyntaxKind::ImplItemList => "`impl` item list",
            SyntaxKind::Trait => "trait definition",
            SyntaxKind::SuperTraitList => "supertrait list",
            SyntaxKind::TraitItemList => "`trait` item list",
            SyntaxKind::TraitTypeItem => "`trait` type item",
            SyntaxKind::TraitConstItem => "`trait` const item",
            SyntaxKind::ImplTrait => "`impl` trait block",
            SyntaxKind::Const => "const definition",
            SyntaxKind::Use => "`use` statement",
            SyntaxKind::UseTree => "`use` tree",
            SyntaxKind::UseTreeList => "`use` tree list",
            SyntaxKind::UsePath => "`use` path",
            SyntaxKind::UsePathSegment => "`use` path segment",
            SyntaxKind::UseTreeRename => "`use as` rename",
            SyntaxKind::Extern => "`extern` block",
            SyntaxKind::ExternItemList => "`extern` body",
            SyntaxKind::ItemList => "item list",
            SyntaxKind::PtrType => "pointer type",
            SyntaxKind::SelfType => "`Self` type",
            SyntaxKind::TupleType => "tuple type definition",
            SyntaxKind::NeverType => "never type",
            SyntaxKind::ArrayType => "array type definition",
            SyntaxKind::Path => "path",
            SyntaxKind::Attr => "attribute",
            SyntaxKind::AttrArgList => "attribute argument list",
            SyntaxKind::AttrArg => "attribute argument",
            SyntaxKind::AttrArgValue => "attribute argument value",
            SyntaxKind::DocCommentAttr => "doc comment",
            SyntaxKind::AttrList => "attribute list",
            SyntaxKind::Visibility => "visibility modifier",
            SyntaxKind::RecordFieldDefList => "record field list",
            SyntaxKind::VariantDef => "`enum` variant definition",
            SyntaxKind::VariantDefList => "`enum` variant list",
            SyntaxKind::TypeGenericParam => "generic type parameter",
            SyntaxKind::ConstGenericParam => "constant generic parameter",
            SyntaxKind::GenericParamList => "generic parameter list",
            SyntaxKind::FuncSignature => "function signature",
            SyntaxKind::FuncParamList => "function parameter list",
            SyntaxKind::FnParam => "function parameter",
            SyntaxKind::TypeBoundList => "type bound list",
            SyntaxKind::KindBoundAbs => "kind bound",
            SyntaxKind::KindBoundMono => "kind bound",
            SyntaxKind::WhereClause => "`where` clause",
            SyntaxKind::WherePredicate => "`where` predicate",
            SyntaxKind::UsesClause => "`uses` clause",
            SyntaxKind::UsesParamList => "`uses` parameter list",
            SyntaxKind::UsesParam => "`uses` parameter",
            SyntaxKind::RecvArmList => "recv arm list",
            SyntaxKind::RecvArm => "recv arm",
            SyntaxKind::Root => "module",
            SyntaxKind::Error => todo!(),
        }
    }

    pub fn is_token(self) -> bool {
        matches!(
            self,
            SyntaxKind::Newline
                | SyntaxKind::Ident
                | SyntaxKind::Int
                | SyntaxKind::String
                | SyntaxKind::LParen
                | SyntaxKind::RParen
                | SyntaxKind::LBrace
                | SyntaxKind::RBrace
                | SyntaxKind::LBracket
                | SyntaxKind::RBracket
                | SyntaxKind::Colon
                | SyntaxKind::Colon2
                | SyntaxKind::SemiColon
                | SyntaxKind::Dot
                | SyntaxKind::Dot2
                | SyntaxKind::Comma
                | SyntaxKind::Arrow
                | SyntaxKind::FatArrow
                | SyntaxKind::Underscore
                | SyntaxKind::Pound
                | SyntaxKind::Plus
                | SyntaxKind::Minus
                | SyntaxKind::Star
                | SyntaxKind::Star2
                | SyntaxKind::Slash
                | SyntaxKind::Percent
                | SyntaxKind::Tilde
                | SyntaxKind::Not
                | SyntaxKind::Hat
                | SyntaxKind::Amp
                | SyntaxKind::Amp2
                | SyntaxKind::Pipe
                | SyntaxKind::Pipe2
                | SyntaxKind::Lt
                | SyntaxKind::Gt
                | SyntaxKind::Eq
                | SyntaxKind::Eq2
                | SyntaxKind::NotEq
                | SyntaxKind::AsKw
                | SyntaxKind::TrueKw
                | SyntaxKind::FalseKw
                | SyntaxKind::BreakKw
                | SyntaxKind::UsesKw
                | SyntaxKind::ContinueKw
                | SyntaxKind::ContractKw
                | SyntaxKind::FnKw
                | SyntaxKind::ModKw
                | SyntaxKind::ConstKw
                | SyntaxKind::IfKw
                | SyntaxKind::ElseKw
                | SyntaxKind::MatchKw
                | SyntaxKind::ForKw
                | SyntaxKind::InKw
                | SyntaxKind::WhereKw
                | SyntaxKind::WhileKw
                | SyntaxKind::PubKw
                | SyntaxKind::ReturnKw
                | SyntaxKind::SelfKw
                | SyntaxKind::SelfTypeKw
                | SyntaxKind::StructKw
                | SyntaxKind::EnumKw
                | SyntaxKind::TraitKw
                | SyntaxKind::ImplKw
                | SyntaxKind::TypeKw
                | SyntaxKind::LetKw
                | SyntaxKind::MutKw
                | SyntaxKind::UseKw
                | SyntaxKind::ExternKw
                | SyntaxKind::UnsafeKw
                | SyntaxKind::IngotKw
                | SyntaxKind::SuperKw
                | SyntaxKind::LShift
                | SyntaxKind::RShift
                | SyntaxKind::LtEq
                | SyntaxKind::GtEq
        )
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
