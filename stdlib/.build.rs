use core::borrow;
use std::borrow::Borrow;
use std::fs::File;
use std::io::prelude::*;
use std::panic;
use std::path::Path;

use alan::lexer::Token;
use alan::parser::extern_func_parser;
use chumsky::prelude::*;
use logos::Logos;

// This module will build a macro to generate inkwell function binidings
fn main()
{
    // println!("cargo:rerun-if-changed=stdlib.alan");

    let file_path = Path::new("./stdlib.alan");
    let mut file: File = File::open(file_path).expect("Could not open file");

    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Failed to read file");

    let lex = Token::lexer(contents.as_str());

    let token_iter = lex.spanned().map(|(tok, span)| match tok {
        // Turn the `Range<usize>` spans logos gives us into chumsky's `SimpleSpan` via `Into`, because it's easier
        // to work with
        Ok(tok) => (tok, span.into()),
        Err(e) => (Token::Error(e), span.into()),
    });

    let token_stream = chumsky::input::Stream::from_iter(token_iter)
        // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
        // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
        .spanned::<alan::lexer::Token<'_>, SimpleSpan>((contents.len()..contents.len()).into());

    let (tokens, errs) = extern_func_parser()
        .parse(token_stream)
        .into_output_errors();

    if errs.len() != 0 {
        panic!("Errors: {:?}", errs);
    }

    let tokens = tokens.unwrap();

    for func in &tokens {
        let ret_type = match func.r_type {
            alan::ast::Type::Int => "int_type",
            alan::ast::Type::Byte => "char_type",
            alan::ast::Type::Void => "void",
            _ => todo!(), // Alan can only return primitives (for now)
        };

        println!("{} -> {:?}", func.name, ret_type);
        // not very pretty. but works for now
        for arg in &func.params {
            let arg_type = match arg.type_.borrow() {
                alan::ast::Type::Int => "int_type",
                alan::ast::Type::Byte => "char_type",
                alan::ast::Type::Ref(ref_) => {
                    match ref_.borrow(){
                        alan::ast::Type::Int => "int_type_ptr",
                        alan::ast::Type::Byte => "char_type_ptr",
                        alan::ast::Type::Array(ar) => {
                            match ar.borrow(){
                                alan::ast::Type::Int => "int_type_ptr",
                                alan::ast::Type::Byte => "char_type_ptr",
                                _ => todo!(),
                            }
                        }
                        _ => todo!(),
                    }
                }

                _ => todo!("hi"),
            };
            println!("\t{:#?} - {:#?}", arg.name, arg_type);
        }
    }
}
