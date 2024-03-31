use alan::{
    lexer::{LexingError, Token},
    parser::{parse_condition, parse_expr, parse_function, parse_lvalue, parse_stmt},
};

use ariadne::{sources, Color, Label, Report, ReportKind};

use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;

fn main() {
    let filename = "filename.alan";
    let input = "2 + 3 + 4 /2 +a";

    let lex = Token::lexer(input);

    let token_iter = lex.spanned().map(|(tok, span)| match tok {
        // Turn the `Range<usize>` spans logos gives us into chumsky's `SimpleSpan` via `Into`, because it's easier
        // to work with
        Ok(tok) => (tok, span.into()),
        Err(e) => (Token::Error(e), span.into()),
    });

    let token_stream = Stream::from_iter(token_iter)
        // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
        // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
        .spanned((input.len()..input.len()).into());

    let (_tokens, errs) = parse_expr().parse(token_stream).into_output_errors();
    

    println!("{:#?}", _tokens.unwrap());

    errs.into_iter().map(|e| e.map_token(|c| c)).for_each(|e| {
        // println!("{:?}", e.found());
        let s = e.found().unwrap();

        match s {
            Token::Error(e) => {
                println!("Error: {:?}", e);
                return;
            }
            _ => {}
        }

        Report::build(ReportKind::Error, filename, e.span().start)
            .with_message(e.to_string())
            .with_label(
                Label::new((filename, e.span().into_range()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(e.contexts().map(|(label, span)| {
                Label::new((filename, span.into_range()))
                    .with_message(format!("while parsing this {}", label))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .eprint(sources([(filename, input)]))
            .unwrap()
    });

    // let input = r" jj(l : int[], j: reference byte ): int limit: int[99]; {}";

    // let lex = Token::lexer(input);

    // let token_iter = lex.spanned().map(|(tok, span)| match tok {
    //     // Turn the `Range<usize>` spans logos gives us into chumsky's `SimpleSpan` via `Into`, because it's easier
    //     // to work with
    //     Ok(tok) => (tok, span.into()),
    //     Err(e) => (Token::Error(e), span.into()),
    // });

    // let token_stream = Stream::from_iter(token_iter)
    //     // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
    //     // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
    //     .spanned((input.len()..input.len()).into());

    // let (_tokens, _errs) = parse_function().parse(token_stream).into_output_errors();

    // println!("{:#?}", _tokens);
    // println!("{:#?}", _errs);
}
