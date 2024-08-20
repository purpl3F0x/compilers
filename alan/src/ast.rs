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

//****************************************/
//*             Pretty Printers          */
//****************************************/
// Helper trait for pretty-printing ASTs
trait PrettyPrint {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result;
}

impl PrettyPrint for TypeKind {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        match self {
            TypeKind::Int => write!(f, "{}Int", ind),
            TypeKind::Byte => write!(f, "{}Byte", ind),
            TypeKind::Void => write!(f, "{}Void", ind),
            TypeKind::Array(inner) => {
                write!(f, "{}Array(", ind)?;
                inner.pretty_print(f, indent + 2)
            }
            TypeKind::Ref(inner) => {
                write!(f, "{}Ref(", ind)?;
                inner.pretty_print(f, indent + 2)
            }
        }
    }
}

impl PrettyPrint for Type {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        self.kind.pretty_print(f, indent)
    }
}

impl PrettyPrint for Literal {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        match self {
            Literal::Int(value) => write!(f, "{}Int({})", ind, value),
            Literal::Byte(c) => write!(f, "{}Byte({})", ind, c),
        }
    }
}

impl PrettyPrint for PrefixOperator {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        let op = match self {
            PrefixOperator::Plus => "+",
            PrefixOperator::Minus => "-",
            PrefixOperator::Not => "!",
        };
        write!(f, "{}{}", ind, op)
    }
}

impl PrettyPrint for InfixOperator {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        write!(f, "{}{}", ind, self)
    }
}

impl<'a> PrettyPrint for LValueAST<'a> {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        match self {
            LValueAST::String(s) => write!(f, "{}String({:?})", ind, s),
            LValueAST::Identifier(id) => write!(f, "{}Identifier({})", ind, id),
            LValueAST::ArraySubscript { id, expr } => {
                write!(f, "{}ArraySubscript\n", ind)?;
                write!(f, "{}  id: {}\n", ind, id)?;
                write!(f, "{}  expr: \n", ind)?;
                expr.pretty_print(f, indent + 4)
            }
        }
    }
}

impl<'a> PrettyPrint for ExprKind<'a> {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        match self {
            ExprKind::Error => write!(f, "{}Error", ind),
            ExprKind::Literal(lit) => {
                write!(f, "{}Literal\n", ind)?;
                lit.pretty_print(f, indent + 2)
            }
            ExprKind::LValue(lvalue) => {
                write!(f, "{}LValue\n", ind)?;
                lvalue.pretty_print(f, indent + 2)
            }
            ExprKind::PrefixOp { op, expr } => {
                write!(f, "{}PrefixOp\n", ind)?;
                op.pretty_print(f, indent + 2)?;
                write!(f, "\n  expr: \n")?;
                expr.pretty_print(f, indent + 2)
            }
            ExprKind::InfixOp { lhs, op, rhs } => {
                write!(f, "{}InfixOp\n", ind)?;
                write!(f, "{}  lhs: \n", ind)?;
                lhs.pretty_print(f, indent + 2)?;
                write!(f, "\n{}  op: ", ind)?;
                op.pretty_print(f, 0)?;
                write!(f, "\n{}  rhs: \n", ind)?;
                rhs.pretty_print(f, indent + 2)
            }
            ExprKind::FunctionCall(fn_call) => {
                write!(f, "{}FunctionCall\n", ind)?;
                fn_call.pretty_print(f, indent + 2)
            }
        }
    }
}

impl<'a> PrettyPrint for ExprAST<'a> {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        write!(f, "{}ExprAST\n", ind)?;
        write!(f, "{}  span: {:?}\n", ind, self.span)?;
        write!(f, "{}  kind: \n", ind)?;
        self.kind.pretty_print(f, indent + 2)
    }
}

impl<'a> PrettyPrint for ConditionAST<'a> {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        match self {
            ConditionAST::Error => write!(f, "{}Error", ind),
            ConditionAST::BoolConst(value) => write!(f, "{}BoolConst({})", ind, value),
            ConditionAST::PrefixOp { op, expr } => {
                write!(f, "{}PrefixOp\n", ind)?;
                op.pretty_print(f, indent + 2)?;
                write!(f, "\n  expr: \n")?;
                expr.pretty_print(f, indent + 2)
            }
            ConditionAST::ExprComparison { lhs, op, rhs } => {
                write!(f, "{}ExprComparison\n", ind)?;
                write!(f, "{}  lhs: \n", ind)?;
                lhs.pretty_print(f, indent + 2)?;
                write!(f, "\n{}  op: ", ind)?;
                op.pretty_print(f, 0)?;
                write!(f, "\n{}  rhs: \n", ind)?;
                rhs.pretty_print(f, indent + 2)
            }
            ConditionAST::InfixLogicOp { lhs, op, rhs } => {
                write!(f, "{}InfixLogicOp\n", ind)?;
                write!(f, "{}  lhs: \n", ind)?;
                lhs.pretty_print(f, indent + 2)?;
                write!(f, "\n{}  op: ", ind)?;
                op.pretty_print(f, 0)?;
                write!(f, "\n{}  rhs: \n", ind)?;
                rhs.pretty_print(f, indent + 2)
            }
        }
    }
}

impl<'a> PrettyPrint for StatementAST<'a> {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        match self {
            StatementAST::Error => write!(f, "{}Error", ind),
            StatementAST::Assignment { lvalue, expr } => {
                write!(f, "{}Assignment\n", ind)?;
                write!(f, "{}  lvalue: \n", ind)?;
                lvalue.pretty_print(f, indent + 2)?;
                write!(f, "\n{}  expr: \n", ind)?;
                expr.pretty_print(f, indent + 2)
            }
            StatementAST::Expr(expr) => {
                write!(f, "{}Expr\n", ind)?;
                expr.pretty_print(f, indent + 2)
            }
            StatementAST::Compound(statements) => {
                write!(f, "{}Compound\n", ind)?;
                for stmt in statements {
                    write!(f, "\n")?;
                    stmt.pretty_print(f, indent + 2)?;
                }
                Ok(())
            }
            StatementAST::FunctionCall(fn_call) => {
                write!(f, "{}FunctionCall\n", ind)?;
                fn_call.pretty_print(f, indent + 2)
            }
            StatementAST::If { condition, then, else_ } => {
                write!(f, "{}If\n", ind)?;
                write!(f, "{}  condition: \n", ind)?;
                condition.pretty_print(f, indent + 2)?;
                write!(f, "\n{}  then: \n", ind)?;
                then.pretty_print(f, indent + 2)?;
                if let Some(else_stmt) = else_ {
                    write!(f, "\n{}  else: \n", ind)?;
                    else_stmt.pretty_print(f, indent + 2)?
                }
                Ok(())
            }
            StatementAST::While { condition, body } => {
                write!(f, "{}While\n", ind)?;
                write!(f, "{}  condition: \n", ind)?;
                condition.pretty_print(f, indent + 2)?;
                write!(f, "\n{}  body: \n", ind)?;
                body.pretty_print(f, indent + 2)
            }
            StatementAST::Return(opt_expr) => {
                write!(f, "{}Return\n", ind)?;
                if let Some(expr) = opt_expr {
                    write!(f, "{}  expr: \n", ind)?;
                    expr.pretty_print(f, indent + 2)?;
                }
                Ok(())
            }
        }
    }
}

impl<'a> PrettyPrint for VarDefAST<'a> {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        write!(f, "{}VarDef\n", ind)?;
        write!(f, "{}  name: {}\n", ind, self.name)?;
        write!(f, "{}  type: \n", ind)?;
        self.type_.pretty_print(f, indent + 2)
    }
}

impl<'a> PrettyPrint for ArrayDefAST<'a> {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        write!(f, "{}ArrayDef\n", ind)?;
        write!(f, "{}  name: {}\n", ind, self.name)?;
        write!(f, "{}  type: \n", ind)?;
        write!(f, "{}  span: {:?}\n", ind, self.span)?;
        self.type_.pretty_print(f, indent + 2)?;
        write!(f, "\n{}  size: {}\n", ind, self.size)?;
        write!(f, "{}  span: {:?}\n", ind, self.span)
    }
}

impl<'a> PrettyPrint for LocalDefinitionAST<'a> {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        match self {
            LocalDefinitionAST::FunctionDef(func) => {
                write!(f, "{}FunctionDef\n", ind)?;
                func.pretty_print(f, indent + 2)
            }
            LocalDefinitionAST::VarDef(var) => {
                write!(f, "{}VarDef\n", ind)?;
                var.pretty_print(f, indent + 2)
            }
            LocalDefinitionAST::ArrayDef(arr) => {
                write!(f, "{}ArrayDef\n", ind)?;
                arr.pretty_print(f, indent + 2)
            }
        }
    }
}

impl<'a> PrettyPrint for FunctionAST<'a> {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        write!(f, "{}FunctionAST\n", ind)?;
        write!(f, "{}  name: {}\n", ind, self.name)?;
        write!(f, "{}  return type: \n", ind)?;
        self.r_type.pretty_print(f, indent + 2)?;
        write!(f, "\n{}  params:\n", ind)?;
        for param in &self.params {
            write!(f, "\n")?;
            param.pretty_print(f, indent + 2)?;
        }
        write!(f, "\n{}  locals:\n", ind)?;
        for local in &self.locals {
            write!(f, "\n")?;
            local.pretty_print(f, indent + 2)?;
        }
        write!(f, "\n{}  body:\n", ind)?;
        for stmt in &self.body {
            write!(f, "\n")?;
            stmt.pretty_print(f, indent + 2)?;
        }
        write!(f, "\n{}  span: {:?}\n", ind, self.span)?;
        write!(f, "{}  signature span: {:?}\n", ind, self.signature_span)
    }
}

impl<'a> PrettyPrint for FnCallAST<'a> {
    fn pretty_print(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let ind = "  ".repeat(indent);
        write!(f, "{}FnCallAST\n", ind)?;
        write!(f, "{}  name: {}\n", ind, self.name)?;
        write!(f, "{}  args:\n", ind)?;
        for arg in &self.args {
            write!(f, "\n")?;
            arg.pretty_print(f, indent + 2)?;
        }
        write!(f, "\n{}  span: {:?}\n", ind, self.span)
    }
}

// Display implementation for the root level of the AST
impl<'a> fmt::Display for FunctionAST<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_print(f, 0)
    }
}
