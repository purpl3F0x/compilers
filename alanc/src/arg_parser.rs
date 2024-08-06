pub use clap::Parser;

use build::CLAP_LONG_VERSION;
use clap::{
    builder::styling::{AnsiColor as Ansi, Styles},
    ArgGroup,
};
use shadow_rs::shadow;
use std::ffi::OsStr;
use std::path::PathBuf;

shadow!(build);

const MY_AWESOME_STYLE: Styles = Styles::styled()
    .header(Ansi::Green.on_default().bold())
    .usage(Ansi::Green.on_default().bold())
    .literal(Ansi::Cyan.on_default().bold())
    .placeholder(Ansi::Blue.on_default());

#[derive(Parser, Debug)]
#[command(
    version,
    long_version = CLAP_LONG_VERSION,
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
{before-help}{name} {version}\n
{author-with-newline}{about-with-newline}\n
{usage-heading} {usage}

{all-args}{after-help}\n
",
)]
pub struct Args
{
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

    /// Read program from stdin, and produce output code to stdout.
    #[arg(short = 'f')]
    pub stdio: bool,

    /// Read program from stdout, and produce intermediate code to stdout.
    #[arg(short = 'i')]
    pub stdio_intermediate: bool,

    /// Print version information (use --version for full version info)
    #[arg(short = 'v', long = "version", action = clap::ArgAction::Version)]
    pub version: Option<bool>,
}

fn valid_input_file(s: &str) -> Result<PathBuf, String>
{
    let s = PathBuf::from(s);
    if s.exists() {
        let extension = s.extension();

        if extension == Some(OsStr::new("imm")) || extension == Some(OsStr::new("asm")) {
            return Err(format!(
                "File {:?} is not a valid input file,as extension {:?} conflicts with output file and will be overwritten",
                s, extension.unwrap()
            ));
        }
        return Ok(s);
    } else {
        return Err(format!("File {:?} does not exist", s));
    }
}

pub enum FileMode
{
    File,
    Stdio,
    StdioIntermediate,
}

impl Args
{
    pub fn mode() -> FileMode
    {
        if Self::parse().stdio {
            return FileMode::Stdio;
        } else if Self::parse().stdio_intermediate {
            return FileMode::StdioIntermediate;
        } else {
            return FileMode::File;
        }
    }
}
