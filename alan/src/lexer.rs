#![allow(unused_imports)]

use super::IntType;

use logos::Span;
pub use logos::{Lexer, Logos};

use std::fmt;
use std::num::ParseIntError;

use likely_stable::{likely, unlikely};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexingError
{
    InvalidInteger,
    IntergerOverflow,

    NonAsciiCharacter(Span),
    EmptyCharLiteral,
    InvalidEscapeCode,

    UntermnatedStringLiteral,

    #[default]
    LexerError,
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = LexingError)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip r"--[^\n]*" )] // Signle line comments
#[logos(skip r"\(\*(?:[^*]|\*[^\)])*\*\)")]
pub enum Token<'input>
{
    // Seperators
    #[token("(")]
    ParentheseisOpen,

    #[token(")")]
    ParentheseisClose,

    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    #[token("[")]
    BracketOpen,

    #[token("]")]
    BracketClose,

    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token(";")]
    SemiColon,

    // Operators
    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("%")]
    Mod,

    #[token("!")]
    Not,

    #[token("=")]
    Assign,

    #[token("==")]
    Equals,

    #[token("!=")]
    NotEquals,

    #[token("<=")]
    LessOrEqual,

    #[token(">=")]
    GreaterOrEqual,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("&")]
    And,

    #[token("|")]
    Or,

    // Keywords
    #[token("byte")]
    Byte,

    #[token("else")]
    Else,

    #[token("false")]
    False,

    #[token("if")]
    If,

    #[token("int")]
    Int,

    #[token("proc")]
    Proc,

    #[token("reference")]
    Ref,

    #[token("return")]
    Return,

    #[token("while")]
    While,

    #[token("true")]
    True,

    // Identifiers
    #[regex("[_a-zA-Z][_0-9a-zA-Z]*")]
    Identifier(&'input str),

    // Number Constants
    #[regex("[0-9]+", |lex| lex.slice().parse())]
    #[regex("[0-9]+[A-Za-z\\\\.]+", |_| Err(LexingError::InvalidInteger) )]
    // TODO: maybe ?? (?:(?:[\.\+\*\\\=])[\-\+])\d+
    NumberConst(IntType),

    // Charater Constants
    #[regex("'([^\']|\')'", |lex| lex.slice().chars().nth(1).to_owned())]
    #[regex(r"'\\.'", char_escape_code)]
    #[regex(r"'\\x[[:xdigit:]]{2}'", |lex| {
        let n = u8::from_str_radix(&lex.slice()[3..5], 16).unwrap();
        if likely(n < 0x80u8) {
            return Ok(n as char)
        }
        Err(LexingError::NonAsciiCharacter(lex.span()))
    })]
    CharConst(char),

    #[regex(
        r#""([^\\\"]|[\\]["\\\/ntr0\'\"]|\\x[[:xdigit:]]{2})*""#,
        string_literal
    )]
    #[regex(
        r#""([^\\\"]|[\\]["\\\/ntr0\'\"]|\\x[[:xdigit:]]{2})*"#,
        |_| Err(LexingError::UntermnatedStringLiteral)
    )]
    StringConst(internment::Intern<String>),

    Error(LexingError),
}

fn char_escape_code<'input>(lex: &mut Lexer<'input, Token<'input>>) -> Result<char, LexingError>
{
    let mut chars = lex.slice().chars();

    match chars.nth(2) {
        Some('n') => Ok('\n'),
        Some('t') => Ok('\t'),
        Some('r') => Ok('\r'),
        Some('0') => Ok('\0'),
        Some('\\') => Ok('\\'),
        Some('\'') => Ok('\''),
        Some('\"') => Ok('\"'),
        _ => Err(LexingError::InvalidEscapeCode),
    }
}

fn string_literal<'input>(
    lex: &mut Lexer<'input, Token<'input>>,
) -> Result<internment::Intern<String>, LexingError>
{
    let mut chars = lex.slice().chars();

    let mut s = String::new();

    chars.next(); // consume \"

    while let Some(c) = chars.next() {
        if c == '"' {
            break;
        }

        if likely(c != '\\') {
            s.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => s.push('\n'),
            Some('t') => s.push('\t'),
            Some('r') => s.push('\r'),
            Some('0') => s.push('\0'),
            Some('\\') => s.push('\\'),
            Some('\'') => s.push('\''),
            Some('\"') => s.push('\"'),
            Some('x') => {
                let mut hex = String::new();
                for _ in 0..2 {
                    match chars.next() {
                        Some(c) => hex.push(c),
                        None => return Err(LexingError::InvalidEscapeCode),
                    }
                }
                match u8::from_str_radix(&hex, 16) {
                    Ok(n) => {
                        if likely(n < 0x80u8) {
                            s.push(n as char)
                        } else {
                            let mut span = lex.span();
                            span.start += s.len();
                            span.end = span.start + 4;
                            return Err(LexingError::NonAsciiCharacter(span));
                        }
                    }
                    Err(_) => return Err(LexingError::InvalidEscapeCode),
                }
            }
            _ => return Err(LexingError::InvalidEscapeCode),
        };
    }
    Ok(internment::Intern::new(s))
}

impl From<ParseIntError> for LexingError
{
    fn from(err: ParseIntError) -> Self
    {
        use std::num::IntErrorKind::*;
        match err.kind() {
            PosOverflow | NegOverflow => LexingError::IntergerOverflow,
            _ => LexingError::InvalidInteger,
        }
    }
}

impl LexingError
{
    pub fn get_message(&self) -> String
    {
        use core::mem::size_of;
        match self {
            Self::IntergerOverflow => {
                format!(
                    "constant too large for type of Int(int{}), valid range is ({} to {})",
                    size_of::<IntType>() * 8,
                    IntType::MIN,
                    IntType::MAX
                )
            }
            _ => todo!("LexingError::get_message incomplete"),
        }
    }
}

impl<'input> fmt::Display for Token<'input>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self {
            Self::ParentheseisOpen => write!(f, "("),
            Self::ParentheseisClose => write!(f, ")"),
            Self::BraceOpen => write!(f, "{{"),
            Self::BraceClose => write!(f, "}}"),
            Self::BracketOpen => write!(f, "["),
            Self::BracketClose => write!(f, "]"),
            Self::Comma => write!(f, ","),
            Self::Colon => write!(f, ":"),
            Self::SemiColon => write!(f, ";"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::Not => write!(f, "!"),
            Self::Assign => write!(f, "="),
            Self::Equals => write!(f, "=="),
            Self::NotEquals => write!(f, "!="),
            Self::LessOrEqual => write!(f, "<="),
            Self::GreaterOrEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::Greater => write!(f, ">"),
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Byte => write!(f, "byte"),
            Self::Else => write!(f, "else"),
            Self::False => write!(f, "false"),
            Self::If => write!(f, "if"),
            Self::Int => write!(f, "int"),
            Self::Proc => write!(f, "proc"),
            Self::Ref => write!(f, "reference"),
            Self::Return => write!(f, "return"),
            Self::While => write!(f, "while"),
            Self::True => write!(f, "true"),
            Self::Identifier(ident) => write!(f, "{}", ident),
            Self::NumberConst(n) => write!(f, "{}", n),
            Self::CharConst(c) => write!(f, "{}", c),
            Self::StringConst(s) => write!(f, "\"{}\"", s),
            Self::Error(e) => write!(f, "Error({})", e),
        }
    }
}

impl fmt::Display for LexingError
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self {
            LexingError::InvalidInteger => write!(f, "Invalid Integer"),

            LexingError::IntergerOverflow => write!(f, "Invalid Interger Value"),

            LexingError::NonAsciiCharacter(_span) => write!(f, "Non-Ascii character"),

            LexingError::LexerError => write!(f, "Unexpected token"),

            _ => todo!("fmt::Display for LexingError incomplete"),
        }
    }
}
