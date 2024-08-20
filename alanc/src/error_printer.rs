use alan::lexer::*;
use ariadne::{Color, Config, Fmt, Label, Report, ReportKind, Source};

pub fn print_error(filename: &str, source: &str, error: &alan::Rich<Token>) {
    let span_range = error.span().into_range();
    let span_start = span_range.start;
    let reason = error.reason();
    let found = error.found().unwrap();

    let mut report = Report::build(ReportKind::Error, &filename, span_start)
        .with_config(Config::default().with_cross_gap(false).with_compact(false).with_underlines(true).with_tab_width(4));

    if let Token::Error(e) = found {
        match e {
            LexingError::NonAsciiCharacter(char_range) => {
                report = report.with_message(format!("{}", e)).with_label(
                    Label::new((&filename, char_range.clone())).with_message(format!("{}", e.get_message())).with_color(Color::Red),
                );
            }
            _ => {
                report = report
                    .with_message(format!("{}", e))
                    .with_label(Label::new((&filename, span_range)).with_message(format!("{}", e.get_message())).with_color(Color::Red));
            }
        }
        if let Some(note) = e.get_note() {
            report = report.with_note(note);
        }
    } else {
        report = report
            .with_message(format!("{}", "Unexpected token".fg(Color::Red)))
            .with_label(Label::new((&filename, span_range)).with_message(format!("{}", reason)).with_color(Color::Red));
    }

    report.finish().eprint((&filename, Source::from(source))).unwrap();
}
