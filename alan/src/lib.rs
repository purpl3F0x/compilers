#![feature(option_zip, trait_alias)]

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;

use libc::c_int;

pub type IntType = c_int;

use chumsky::{input::ValueInput, prelude::*};

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

