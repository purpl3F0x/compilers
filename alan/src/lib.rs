#![feature(option_zip, trait_alias)]

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;

use libc::c_int;

pub type IntType = c_int;

use chumsky::input::{SpannedInput, ValueInput};

pub type Span = chumsky::prelude::SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

pub use chumsky::input::Stream;
pub use chumsky::prelude::*;
pub use chumsky::Parser;

#[inline]
pub fn get_token_iter<'input>(
    lexer: lexer::Lexer<'input, lexer::Token<'input>>,
) -> impl Iterator<Item = Spanned<lexer::Token<'input>>>
{
    lexer.spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(e) => (lexer::Token::Error(e), span.into()),
    })
}

#[inline]
pub fn token_iter_to_stream<'input>(
    token_iter: impl Iterator<Item = Spanned<lexer::Token<'input>>>,
    input: &'input str,
) -> SpannedInput<
    lexer::Token<'input>,
    SimpleSpan,
    Stream<impl Iterator<Item = Spanned<lexer::Token<'input>>>>,
>
{
    Stream::from_iter(token_iter)
        // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
        // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
        .spanned((input.len()..input.len()).into())
}
