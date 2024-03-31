use alan::lexer::Token;
use logos::Logos;

fn main() {
    use ariadne::{Color, Config, Fmt, Label, Report, ReportKind, Source};
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::Path;

    let file_path = Path::new("./examples/bsort.alan");
    let mut file: File = File::open(file_path).expect("Could not open file");
    let filename = file_path.file_name().unwrap().to_str().unwrap();

    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Failed to read file");

    let mut lex = Token::lexer(contents.as_str());

    while let Some(token) = lex.next() {
        match token {
            Err(e) => {
                Report::build(ReportKind::Error, &filename, 12)
                    .with_message(format!("{}", e))
                    .with_config(Config::default().with_compact(true))
                    .with_label(
                        Label::new((&filename, lex.span()))
                            .with_message(format!("{}", e.get_message().fg(Color::Red)))
                            .with_color(Color::Red),
                    )
                    .with_note("note")
                    .with_config(
                        Config::default()
                            .with_cross_gap(false)
                            .with_compact(false)
                            .with_underlines(false)
                            .with_tab_width(2),
                    )
                    .finish()
                    .eprint((&filename, Source::from(contents)))
                    .unwrap();
                break;
            }
            _ => {}
        }
    }
}
