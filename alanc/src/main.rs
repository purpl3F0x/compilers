mod arg_parser;
use arg_parser::*;

mod error_printer;
use error_printer::*;

use alan::codegen as cgen;
use alan::lexer::*;
use alan::parser::parse_function;
use alan::Parser as alanParser;

use yansi::Paint;

use std::io::{stdin, Error as ioError, Read};
use std::process::exit;

fn handle_io_error(err: ioError, what: Option<&str>) -> ! {
    eprint!("{} {}", "error:".red().bold(), err);
    if let Some(what) = what {
        eprint!("{}: {}", ", when attempting to access: ", what.bold());
    } else {
        eprintln!();
    }

    exit(err.raw_os_error().unwrap_or(1));
}

fn main() {
    let args = Args::parse();

    let mode = args.mode();

    let mut src_buffer = String::new();
    let src_file_name;

    let mut imm_file_name = String::new();
    let mut asm_file_name = String::new();
    let mut outfile_name = String::new();

    // *Open source file and create output files
    if let Some(src_file) = args.src_file {
        src_buffer = std::fs::read_to_string(&src_file).unwrap_or_else(|err| {
            handle_io_error(err, src_file.as_path().to_str());
        });
        src_file_name = src_file.display().to_string();

        // Create output files
        // Use set_extension instead of with_extension, so it doesn't produce ..imm/.asm in files without extension
        let imm_file_path = src_file.with_extension("imm");
        let asm_file_path = src_file.with_extension("asm");
        let outfile_path = src_file.with_extension("exe");

        imm_file_name = imm_file_path.file_name().unwrap().to_str().unwrap().to_string();
        asm_file_name = asm_file_path.file_name().unwrap().to_str().unwrap().to_string();
        if let Some(output) = args.output {
            outfile_name = output;
        } else {
            outfile_name = outfile_path.file_name().unwrap().to_str().unwrap().to_string();
        }
    } else if args.stdio {
        src_file_name = "stdin".to_string();

        stdin().read_to_string(&mut src_buffer).unwrap_or_else(|err| {
            handle_io_error(err, Some("stdin"));
        });
    } else if args.stdio_intermediate {
        src_file_name = "stdin".to_string();

        stdin().read_to_string(&mut src_buffer).unwrap_or_else(|err| {
            handle_io_error(err, Some("stdin"));
        });
    } else {
        unreachable!();
    }

    // * Start the Lexer
    let lex = alan::lexer::Token::lexer(src_buffer.as_str());

    let token_iter = alan::get_token_iter(lex);
    let token_stream = alan::token_iter_to_stream(token_iter, src_buffer.as_str());

    // * Parse
    let (tokens, errs) = parse_function().parse(token_stream).into_output_errors();

    // * Check exr errors
    if errs.len() > 0 {
        // todo: use ariadne to print errors
        for e in errs {
            report_parse_error(&src_file_name, src_buffer.as_str(), &e);
        }

        println!("exiting...");
        exit(1);
    }

    //* Compile the AST
    let top = tokens.unwrap();

    //* Dump AST ??
    if args.dump_ast {
        top.print().unwrap_or_else(|e| {
            handle_io_error(e, Some("failed printing AST"));
        });

        exit(0);
    }

    //* Compile
    let mut context = cgen::Context::create();
    let mut compiler = cgen::Compiler::new(&mut context, args.target).unwrap_or_else(|e| {
        report_compiler_error(&src_file_name, src_buffer.as_str(), &e);
        exit(1);
    });

    compiler.set_source_file_name(&src_file_name);

    let res = compiler.compile(&top);

    //* Everything ok ?
    if let Some(e) = res.as_ref().err() {
        report_compiler_error(&src_file_name, src_buffer.as_str(), e);
        exit(1);
    }

    //* Optimize ?
    if args.optimize {
        compiler.optimize();
    }

    //* Write to output
    // let s = compiler.imm_as_string();
    // println!("{}", s);

    //* Write to output
    match mode {
        FileMode::File => {
            let asm_path = std::path::Path::new(&asm_file_name);

            compiler.imm_to_file(&imm_file_name).ok();
            compiler.asm_to_file(&asm_path).ok();
        }
        FileMode::Stdio => {
            println!("{}", compiler.asm_as_string());
            exit(0);
        }
        FileMode::StdioIntermediate => {
            println!("{}", compiler.imm_as_string());
            exit(0);
        }
    }

    //* Link files with clang
    let libalan_path = std::env::current_exe().unwrap().parent().unwrap().join("libalan.a");

    let compile_cmd = std::process::Command::new("clang")
        .args(&["-o", outfile_name.as_str(), &asm_file_name, libalan_path.to_str().unwrap()])
        .output()
        .expect("failed to compile, make sure clang is installed");

    if !compile_cmd.status.success() {
        eprintln!("{}", "Compilation failed".red());
        eprintln!("{}", String::from_utf8_lossy(&compile_cmd.stderr));
        exit(1);
    }

    //* Done
    eprintln!("{}", "Compilation was successful".green());
}
