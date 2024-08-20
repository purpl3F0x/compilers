#![cfg_attr(debug_assertions, allow(dead_code))]

use core::fmt;

use super::*;

// todo: make AST printer

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    // Primitive Types
    Int,
    Byte,
    Void,
    // Array Types
    Array(Box<TypeKind>),
    // Reference Types
    Ref(Box<TypeKind>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Int(IntType),
    Byte(char),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrefixOperator {
    Plus,
    Minus,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InfixOperator {
    // Mathematical Operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Comparisson Operators
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterOrEqual,
    LessOrEqual,
    // Logic Operators
    LogicAnd,
    LogicOr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LValueAST<'a> {
    String(internment::Intern<String>),

    Identifier(&'a str),

    ArraySubscript { id: &'a str, expr: Box<ExprAST<'a>> },
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind<'a> {
    Error,

    Literal(Literal),

    // Local(&'a str),
    LValue(LValueAST<'a>),

    PrefixOp { op: PrefixOperator, expr: Box<ExprAST<'a>> },

    InfixOp { lhs: Box<ExprAST<'a>>, op: InfixOperator, rhs: Box<ExprAST<'a>> },

    FunctionCall(FnCallAST<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprAST<'a> {
    pub span: Span,
    pub kind: ExprKind<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConditionAST<'a> {
    Error,

    BoolConst(bool),

    PrefixOp { op: PrefixOperator, expr: Box<ConditionAST<'a>> },

    ExprComparison { lhs: Box<ExprAST<'a>>, op: InfixOperator, rhs: Box<ExprAST<'a>> },

    InfixLogicOp { lhs: Box<ConditionAST<'a>>, op: InfixOperator, rhs: Box<ConditionAST<'a>> },
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementAST<'a> {
    Error,

    Assignment { lvalue: LValueAST<'a>, expr: ExprAST<'a> },

    Expr(ExprAST<'a>),
    Compound(Vec<StatementAST<'a>>),

    FunctionCall(FnCallAST<'a>),

    If { condition: ConditionAST<'a>, then: Box<StatementAST<'a>>, else_: Option<Box<StatementAST<'a>>> },

    While { condition: ConditionAST<'a>, body: Box<StatementAST<'a>> },

    Return(Option<ExprAST<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDefAST<'a> {
    pub name: &'a str,
    pub type_: Type,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayDefAST<'a> {
    pub name: &'a str,
    pub type_: Type,
    pub size: IntType,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LocalDefinitionAST<'a> {
    FunctionDef(FunctionAST<'a>),
    VarDef(VarDefAST<'a>),
    ArrayDef(ArrayDefAST<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionAST<'a> {
    pub name: &'a str,
    pub r_type: Type,

    pub params: Vec<VarDefAST<'a>>,
    pub locals: Vec<LocalDefinitionAST<'a>>,
    pub body: Vec<StatementAST<'a>>,

    /// The span of the whole function, can derive the span of the body by subtracting the span of the signature
    pub span: Span,
    /// The span of the signature of the function
    pub signature_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCallAST<'a> {
    pub name: &'a str,
    pub args: Vec<ExprAST<'a>>,
    pub span: Span,
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            InfixOperator::Add => "+",
            InfixOperator::Sub => "-",
            InfixOperator::Mul => "*",
            InfixOperator::Div => "/",
            InfixOperator::Mod => "%",
            InfixOperator::Equal => "==",
            InfixOperator::NotEqual => "!=",
            InfixOperator::Greater => ">",
            InfixOperator::Less => "<",
            InfixOperator::GreaterOrEqual => ">=",
            InfixOperator::LessOrEqual => "<=",
            InfixOperator::LogicAnd => "&",
            InfixOperator::LogicOr => "|",
        };
        write!(f, "{}", op)
    }
}
