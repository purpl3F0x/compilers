mod arg_parser;
use arg_parser::*;

use alan::codegen as cgen;
use alan::lexer::*;
use alan::parser::parse_function;
use alan::Parser as alanParser;

use std::process::exit;

#[allow(dead_code)]
#[allow(unused_variables)]
use std::{
    // fmt::{write, Write},
    // fmt::{write, Debug},
    fs::File,
    io::{stdin, stdout, BufWriter, Error as ioError, Read, Write},
};

fn handle_io_error(err: ioError, what: Option<&str>) -> !
{
    use concolor;
    use yansi::Paint;

    if concolor::get(concolor::Stream::Stderr).color() {
        eprint!("{} {}", "error:".red().bold(), err);
    } else {
        eprintln!("error: {}", err);
    }

    if let Some(what) = what {
        eprintln!("{}: {}", ", when attempting to access: ", what.bold());
    } else {
        eprintln!();
    }

    exit(err.raw_os_error().unwrap_or(1));
}

fn main()
{
    let args = Args::parse();

    let mut src_buffer = String::new();
    let mut src_file_name: Option<String> = None;

    let mut imm_file_name = String::new();
    let mut asm_file_name = String::new();

    let mut imm_stream: Option<BufWriter<Box<dyn std::io::Write>>> = None; // create a Boxed writer to either write to a file or stdout
    let mut asm_stream: Option<BufWriter<Box<dyn std::io::Write>>> = None;

    /*
    Open source file and create output files
    */
    if let Some(src_file) = args.src_file {
        src_buffer = std::fs::read_to_string(&src_file).unwrap_or_else(|err| {
            handle_io_error(err, src_file.as_path().to_str());
        });
        src_file_name = Some(src_file.display().to_string());

        // Create output files
        // Use set_extension instead of with_extension, so it doesn't produce ..imm/.asm in files without extension
        let mut imm_file_path = src_file.clone();
        let mut asm_file_path = src_file.clone();
        imm_file_path.set_extension("imm");
        asm_file_path.set_extension("asm");

        imm_file_name = imm_file_path.display().to_string();
        asm_file_name = asm_file_path.display().to_string();

        let imm_file = File::create(&imm_file_path).unwrap_or_else(|err| {
            handle_io_error(err, imm_file_path.to_str());
        });

        let asm_file = File::create(&asm_file_path).unwrap_or_else(|err| {
            handle_io_error(err, Some(imm_file_name.as_str()));
        });

        imm_stream = Some(BufWriter::new(Box::new(imm_file)));
        asm_stream = Some(BufWriter::new(Box::new(asm_file)));
    } else if args.stdio {
        println!("Reading from stdin and writing to stdout");
        stdin()
            .read_to_string(&mut src_buffer)
            .unwrap_or_else(|err| {
                handle_io_error(err, Some("stdin"));
            });

        asm_stream = Some(BufWriter::new(Box::new(stdout().lock())));
        asm_file_name = "stdout".to_string();
    } else if args.stdio_intermediate {
        println!("Reading from stdin and writing intermediate code to stdout");
        stdin()
            .read_to_string(&mut src_buffer)
            .unwrap_or_else(|err| {
                handle_io_error(err, Some("stdin"));
            });

        imm_stream = Some(BufWriter::new(Box::new(stdout().lock())));
        imm_file_name = "stdout".to_string();
    } else {
        unreachable!();
    }

    // * Start the Lexer
    let lex = alan::lexer::Token::lexer(src_buffer.as_str());

    let token_iter = alan::get_token_iter(lex);
    let token_stream = alan::token_iter_to_stream(token_iter, src_buffer.as_str());

    // * Parse
    let (tokens, errs) = parse_function().parse(token_stream).into_output_errors();

    // * Check for errors
    if errs.len() > 0 {
        // todo: use ariadne to print errors
        for e in errs {
            let span = e.span();
            let s = e.found().unwrap();
            match s {
                Token::Error(e) => {
                    println!("Error at ({}): {}", span, e);
                    return;
                }
                _ => {}
            }
        }
        exit(1);
    }
    //* Run Sementic
    // todo: implement semantic analysis

    //* Compile the AST
    let top = tokens.unwrap();

    // Create llvm context
    let mut context = cgen::Context::create();
    let mut compiler = cgen::Compiler::new(&mut context);
    if let Some(src_file_name) = src_file_name {
        // src_file_name += "42";
        compiler.set_source_file_name(&src_file_name);
    }

    let res = compiler.compile(&top);

    if Ok(()) != res {
        println!("Compilation failed, what? {}", res.unwrap_err());
        exit(1);
    }

    if args.optimize {
        compiler.optimize();
    }

    let s = compiler.imm_as_string();
    println!("{}", s);

    // let s = compiler.asm_as_string();
    // println!("{}", s);
}
