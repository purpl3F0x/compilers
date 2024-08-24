pub use clap::Parser;

use clap::{
    builder::styling::{AnsiColor as Ansi, Styles},
    ArgGroup,
};
use shadow_rs::shadow;
use std::ffi::OsStr;
use std::path::PathBuf;
use yansi::Paint;

shadow!(build);

fn my_dragon() -> String {
    format!(
        "{}\n{}\n{}{}\n{}{}\n{}{}\n",
        r"                     \`-\`-._".green(),
        r"                         \` )`. `-.__      ,".green(),
        r"      '' , . _       ".red(),
        r"_,-._;'_,-`__,-'    ,/".green(),
        r"     : `. ` , _' :- ".red(),
        "'--'._ ' `------._,-;'".green(),
        r"      `- ,`- '            ".red(),
        r"`--..__,,---' ".green()
    )
}

fn my_long_version() -> String {
    use build::*;

    format!(
        "{}\n\n{}\nAuthor: {}\n\n{}\n\nbranch: {}\ncommit_hash: {}\nbuild_time: {}\nbuild_env: {} - {}\nhost-os: {}\nLLVM Version:  {}",
        PKG_VERSION.magenta(),
        env!("CARGO_PKG_DESCRIPTION"),
        env!("CARGO_PKG_AUTHORS").yellow(),
        my_dragon(),
        BRANCH.blue(),
        SHORT_COMMIT.blue(),
        BUILD_TIME.blue(),
        RUST_VERSION.blue(),
        RUST_CHANNEL.blue(),
        alan::codegen::Compiler::system_triple().blue(),
        alan::codegen::Compiler::llvm_version().blue(),
    )
}

const MY_AWESOME_STYLE: Styles = Styles::styled()
    .header(Ansi::Green.on_default().bold())
    .usage(Ansi::Green.on_default().bold())
    .literal(Ansi::Cyan.on_default().bold())
    .placeholder(Ansi::Blue.on_default());

#[derive(Parser, Debug, Clone)]
#[command(
    version,
    long_version =  my_long_version(),
    disable_version_flag = true,
    author,
    about, // set from description  in Cargo.toml
    long_about = None,
    after_help = "For more information, visit https://github.com/purpl3F0x/compilers",
    styles = MY_AWESOME_STYLE,
    group(
        ArgGroup::new("input")
            .required(true)
            .args(&["src_file", "stdio", "stdio_intermediate"])),
    help_template = "\n
{before-help}{name} {version}
{about-with-newline}
{author-with-newline}\n
{usage-heading} {usage}

{all-args}{after-help}\n
",
)]
pub struct Args {
    /// Input file
    #[arg(value_name="SOURCE_FILE", value_parser = valid_input_file)]
    pub src_file: Option<PathBuf>,

    // ? add output file
    // /// Place the output into <OUTPUT_FILE>
    // #[arg(short, long, value_name = "OUTPUT_FILE",  requires = "src_file")]
    // pub output: Option<String>,
    /// Enable optimizations
    #[arg(value_enum, short = 'O')]
    pub optimize: bool,

    /// Dump AST to stdout
    #[arg(long = "dump-ast")]
    pub dump_ast: bool,

    /// Read program from stdin, and produce output code to stdout.
    #[arg(short = 'f')]
    pub stdio: bool,

    /// Read program from stdout, and produce intermediate code to stdout.
    #[arg(short = 'i')]
    pub stdio_intermediate: bool,

    /// Print version information (use --version for full version info)
    #[arg(short = 'v', long = "version", action = clap::ArgAction::Version)]
    pub version: Option<bool>,

    /// Specify LLVM target triple, for cross-compilation
    #[arg(long = "target")]
    pub target: Option<String>,
}

fn valid_input_file(s: &str) -> Result<PathBuf, String> {
    let s = PathBuf::from(s);
    if s.exists() {
        let extension = s.extension();

        if extension == Some(OsStr::new("imm")) || extension == Some(OsStr::new("asm")) {
            return Err(format!(
                "File {:?} is not a valid input file,as extension {:?} conflicts with output file and will be overwritten",
                s,
                extension.unwrap()
            ));
        }
        return Ok(s);
    } else {
        return Err(format!("File {:?} does not exist", s));
    }
}

pub enum FileMode {
    File,
    Stdio,
    StdioIntermediate,
}

impl Args {
    pub fn mode(&self) -> FileMode {
        if Self::parse().stdio {
            return FileMode::Stdio;
        } else if Self::parse().stdio_intermediate {
            return FileMode::StdioIntermediate;
        } else {
            return FileMode::File;
        }
    }
}
