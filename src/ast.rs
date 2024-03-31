#![cfg_attr(debug_assertions, allow(dead_code))]

use core::fmt;

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    // Primitive Types
    Int,
    Byte,
    Void,

    // Array Types
    Array(Box<Type>),
    // Reference Types
    Ref(Box<Type>),
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
pub enum ExprAST<'a> {
    Error,

    Literal(Literal),

    // Local(&'a str),
    LValue(LValueAST<'a>),

    PrefixOp {
        op: PrefixOperator,
        expr: Box<ExprAST<'a>>,
    },

    InfixOp {
        lhs: Box<ExprAST<'a>>,
        op: InfixOperator,
        rhs: Box<ExprAST<'a>>,
    },

    Call {
        function: &'a str,
        args: Vec<ExprAST<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConditionAST<'a> {
    Error,

    BoolConst(bool),

    PrefixOp {
        op: PrefixOperator,
        expr: Box<ConditionAST<'a>>,
    },

    ExprComparison {
        lhs: Box<ExprAST<'a>>,
        op: InfixOperator,
        rhs: Box<ExprAST<'a>>,
    },

    InfixLogicOp {
        lhs: Box<ConditionAST<'a>>,
        op: InfixOperator,
        rhs: Box<ConditionAST<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementAST<'a> {
    Error,

    Null,

    Assignment {
        lvalue: LValueAST<'a>,
        expr: ExprAST<'a>,
    },

    Expr(ExprAST<'a>),
    Compound(Vec<StatementAST<'a>>),

    FunctionCall {
        function: &'a str,
        args: Vec<ExprAST<'a>>,
    },

    If {
        condition: ConditionAST<'a>,
        then: Box<StatementAST<'a>>,
        else_: Option<Box<StatementAST<'a>>>,
    },

    While {
        condition: ConditionAST<'a>,
        body: Box<StatementAST<'a>>,
    },

    Return(Option<ExprAST<'a>>),
}

#[derive(Clone, Debug, PartialEq)]

pub struct VarDefAST<'a> {
    pub name: &'a str,
    pub type_: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LocalDefinitionAST<'a> {
    FunctionDef(FnAST<'a>),
    VarDef {
        name: &'a str,
        type_: Type,
    },
    ArrayDef {
        name: &'a str,
        type_: Type,
        size: IntType,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnAST<'a> {
    pub name: &'a str,
    pub r_type: Type,

    pub params: Vec<VarDefAST<'a>>,
    pub locals: Vec<LocalDefinitionAST<'a>>,
    pub body: Vec<StatementAST<'a>>,
}

