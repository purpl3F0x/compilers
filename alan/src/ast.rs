#![cfg_attr(debug_assertions, allow(dead_code))]

use core::fmt;

use super::*;

// todo: make AST printer

#[derive(Clone, Debug, PartialEq, Serialize)]
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

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Literal {
    Int(IntType),
    Byte(char),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum PrefixOperator {
    Plus,
    Minus,
    Not,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
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
pub enum LValueKind<'a> {
    String(internment::Intern<String>),
    Identifier(&'a str),
    ArraySubscript { id: &'a str, expr: Box<ExprAST<'a>> },
}

#[derive(Clone, Debug, PartialEq)]
pub struct LValueAST<'a> {
    pub kind: LValueKind<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
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

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum ConditionKind<'a> {
    Error,

    BoolConst(bool),

    PrefixOp { op: PrefixOperator, expr: Box<ConditionAST<'a>> },

    ExprComparison { lhs: Box<ExprAST<'a>>, op: InfixOperator, rhs: Box<ExprAST<'a>> },

    InfixLogicOp { lhs: Box<ConditionAST<'a>>, op: InfixOperator, rhs: Box<ConditionAST<'a>> },
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConditionAST<'a> {
    pub span: Span,
    pub kind: ConditionKind<'a>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum StatementKind<'a> {
    Error,

    Assignment {
        lvalue: LValueAST<'a>,
        expr: ExprAST<'a>,
    },

    Expr(ExprAST<'a>),
    Compound(Vec<StatementAST<'a>>),

    FunctionCall(FnCallAST<'a>),

    If {
        condition: ConditionAST<'a>,
        then: Box<StatementAST<'a>>,
        else_: Option<Box<StatementAST<'a>>>,
    },

    While {
        condition: ConditionAST<'a>,
        body: Box<StatementAST<'a>>,
    },

    #[serde(serialize_with = "serialize_return")]
    Return(Option<ExprAST<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StatementAST<'a> {
    pub span: Span,
    pub kind: StatementKind<'a>,
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

#[derive(Clone, Debug, PartialEq, Serialize)]
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

//****************************************/
//*             Pretty Printers          */
//****************************************/
use serde::{ser::SerializeStruct, Serialize};

impl Serialize for Type {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Type", 2)?;
        state.serialize_field("span", &self.span.to_string())?;
        state.serialize_field("kind", &self.kind)?;

        state.end()
    }
}

impl Serialize for LValueKind<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            LValueKind::String(s) => serializer.serialize_str(s),
            LValueKind::Identifier(s) => serializer.serialize_str(s),
            LValueKind::ArraySubscript { id, expr } => {
                let mut state = serializer.serialize_struct("ArraySubscript", 2)?;
                state.serialize_field("id", id)?;
                state.serialize_field("expr", expr)?;
                state.end()
            }
        }
    }
}

impl Serialize for LValueAST<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("LValue", 2)?;
        state.serialize_field("span", &self.span.to_string())?;
        state.serialize_field("kind", &self.kind)?;
        state.end()
    }
}

impl Serialize for VarDefAST<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("VarDef", 3)?;
        state.serialize_field("span", &self.span.to_string())?;
        state.serialize_field("name", &self.name)?;
        state.serialize_field("type", &self.type_)?;
        state.end()
    }
}

impl Serialize for ExprAST<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Expr", 2)?;
        state.serialize_field("span", &self.span.to_string())?;
        state.serialize_field("kind", &self.kind)?;
        state.end()
    }
}

impl Serialize for ArrayDefAST<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("ArrayDef", 4)?;
        state.serialize_field("span", &self.span.to_string())?;
        state.serialize_field("name", &self.name)?;
        state.serialize_field("type", &self.type_)?;
        state.serialize_field("size", &self.size)?;
        state.end()
    }
}

impl Serialize for FnCallAST<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("FnCall", 3)?;
        state.serialize_field("span", &self.span.to_string())?;
        state.serialize_field("name", &self.name)?;
        state.serialize_field("args", &self.args)?;
        state.end()
    }
}

impl Serialize for ConditionAST<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Condition", 2)?;
        state.serialize_field("span", &self.span.to_string())?;
        state.serialize_field("kind", &self.kind)?;
        state.end()
    }
}

impl Serialize for StatementAST<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Statement", 2)?;
        state.serialize_field("span", &self.span.to_string())?;
        state.serialize_field("kind", &self.kind)?;
        state.end()
    }
}

impl Serialize for FunctionAST<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Function", 6)?;
        state.serialize_field("span", &self.span.to_string())?;
        state.serialize_field("name", &self.name)?;
        state.serialize_field("args", &self.params)?;
        state.serialize_field("rtype", &self.r_type)?;
        state.serialize_field("locals", &self.locals)?;
        state.serialize_field("body", &self.body)?;
        state.end()
    }
}

pub fn serialize_return<S>(ret: &Option<ExprAST<'_>>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut state;
    if let Some(e) = ret {
        state = serializer.serialize_struct("Return", 1)?;
        state.serialize_field("expr", e)?;
    } else {
        state = serializer.serialize_struct("Return", 0)?;
    }
    state.end()
}

impl FunctionAST<'_> {
    pub fn print(&self) -> std::io::Result<()> {
        let config: ptree::PrintConfig = ptree::PrintConfig::from_env();
        let serialized = serde_value::to_value(&self).unwrap();
        ptree::print_tree_with(&serialized, &config)
    }
}
