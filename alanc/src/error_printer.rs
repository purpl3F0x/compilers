use alan::codegen::{IRError, SemanticError};
use alan::lexer::*;

use ariadne::{Color, Config, Fmt, Label, Report, ReportKind, Source};

pub fn report_parse_error(filename: &str, source: &str, error: &alan::Rich<Token>) {
    let span_range = error.span().into_range();
    let span_start = span_range.start;
    let reason = error.reason();
    let found = error.found().unwrap();

    let mut report = Report::build(ReportKind::Error, &filename, span_start)
        .with_config(Config::default().with_cross_gap(true).with_compact(false).with_underlines(true).with_tab_width(4));

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

pub fn report_compiler_error(filename: &str, source: &str, error: &IRError) {
    match error {
        IRError::SemanticError(e) => report_semantic_error(filename, source, e),

        _ => {
            eprint!("{}", "Compilation Failed\nReason: ".fg(Color::Red));
            eprint!("{}\n", error.fg(Color::Red));
        }
    }
}

pub fn report_semantic_error(filename: &str, source: &str, error: &SemanticError) {
    let mut builder;

    match error {
        SemanticError::TopHasArguments { signature_span } => {
            builder = Report::build(ReportKind::Error, &filename, signature_span.start)
                .with_message(format!("Top level function has arguments"))
                .with_label(
                    Label::new((&filename, signature_span.into_range()))
                        .with_message(format!("Top level function should not have arguments"))
                        .with_color(Color::Red),
                );
        }

        SemanticError::TopIsNotAProc { signature_span, ret_ty, r_ty_span } => {
            builder = Report::build(ReportKind::Error, &filename, signature_span.start)
                .with_message(format!("Top level function not a proc"))
                .with_label(
                    Label::new((&filename, r_ty_span.into_range()))
                        .with_message(format!(
                            "Top level function should return a {}, instead found {}",
                            "proc".fg(Color::Green),
                            ret_ty.fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                )
        }

        SemanticError::AssignResultOfProc { span, decl_span: definition_span } => {
            builder = Report::build(ReportKind::Error, &filename, span.start)
                .with_message(format!("Void value not ignored, as ought to be"))
                .with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(format!("Functions with type of proc, cannot be assigned to variables").fg(Color::Red))
                        .with_color(Color::Red),
                );

            if definition_span.end > 0 {
                builder = builder.with_label(
                    Label::new((&filename, definition_span.into_range())).with_message(format!("function defined here").fg(Color::Blue)),
                );
            }
        }

        SemanticError::UndeclaredIdentifier { span, name } => {
            builder = Report::build(ReportKind::Error, &filename, span.start)
                .with_message(format!("Undeclared identifier"))
                .with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(format!("Use of undeclared identifier: `{}`", name).fg(Color::Red))
                        .with_color(Color::Red),
                )
                .with_note(format!("consider declaring `{}`", name));
        }

        SemanticError::SubscriptingNonArray { span, ty, decl_span } => {
            builder = Report::build(ReportKind::Error, &filename, span.start)
                .with_message(format!("Subscripting non-array"))
                .with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(format!("Subscripting non-array type: `{}`", ty).fg(Color::Red))
                        .with_color(Color::Red),
                )
                .with_label(
                    Label::new((&filename, decl_span.into_range())).with_message(format!("Defined here as `{}`", ty).fg(Color::Blue)),
                );
        }

        SemanticError::MissmatchedArgumentCount { span, expected, found, decl_span, func_signature } => {
            builder =
                Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Mismatched argument count")).with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(
                            format!("Function expected {} argument{}, found {}", expected, (if *expected != 1 { "s" } else { "" }), found)
                                .fg(Color::Red),
                        )
                        .with_color(Color::Red),
                );

            if decl_span.end > 0 {
                builder = builder
                    .with_label(Label::new((&filename, decl_span.into_range())).with_message(format!("Defined here").fg(Color::Blue)));
            } else {
                builder = builder.with_note(format!("Function has signature: {}", func_signature.fg(Color::Blue)));
            }
        }

        SemanticError::UndeclaredFunction { span, name } => {
            builder = Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Undeclared function")).with_label(
                Label::new((&filename, span.into_range()))
                    .with_message(format!("Use of undeclared function: `{}`", name).fg(Color::Red))
                    .with_color(Color::Red),
            )
        }

        SemanticError::ArgumentTypeMismatch { span, arg_index, expected, found, decl_span, func_signature } => {
            builder = Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Argument type mismatch")).with_label(
                Label::new((&filename, span.into_range()))
                    .with_message(format!(
                        "Argument {} expected type `{}`, found `{}`",
                        arg_index + 1,
                        expected.fg(Color::Green),
                        found.fg(Color::Red)
                    ))
                    .with_color(Color::Red),
            );

            if decl_span.end > 0 {
                builder = builder
                    .with_label(Label::new((&filename, decl_span.into_range())).with_message(format!("Defined here").fg(Color::Blue)));
            } else {
                builder = builder.with_note(format!("Function has signature: {}", func_signature.fg(Color::Blue)));
            }
        }

        SemanticError::ConstantAsRef { span, ty, ref_ty, decl_span } => {
            builder = Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Constant as reference")).with_label(
                Label::new((&filename, span.into_range()))
                    .with_message(format!(
                        "Constant of type `{}` cannot be used as reference of type `{}`",
                        ty.fg(Color::Red),
                        ref_ty.fg(Color::Green)
                    ))
                    .with_color(Color::Red),
            );

            if decl_span.end > 0 {
                builder = builder
                    .with_label(Label::new((&filename, decl_span.into_range())).with_message(format!("Defined here").fg(Color::Blue)));
            }
        }

        SemanticError::AssignToString { span } => {
            builder = Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Assigning to string")).with_label(
                Label::new((&filename, span.into_range()))
                    .with_message(format!("Cannot assign to string literal").fg(Color::Red))
                    .with_color(Color::Red),
            );
        }

        SemanticError::AssignTypeMismatch { span, expected, found, decl_span } => {
            builder = Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Mismatched types ")).with_label(
                Label::new((&filename, span.into_range()))
                    .with_message(format!(
                        "Assigment type mismatch, r-value has type of `{}`, but tried to assign to `{}`",
                        found.fg(Color::Red),
                        expected.fg(Color::Green)
                    ))
                    .with_color(Color::Red),
            );

            if decl_span.end > 0 {
                builder = builder
                    .with_label(Label::new((&filename, decl_span.into_range())).with_message(format!("Defined here").fg(Color::Blue)));
            }

            if !(expected.is_primitive() || expected.is_primitive()) {
                builder = builder.with_note(format!("Cannot assign to non-primitive types"));
            }

            if (expected.is_primitive() || expected.is_primitive_reference()) && (found.is_primitive() || found.is_primitive_reference()) {
                let lhs_ty = if expected.is_primitive_reference() { expected.get_inner_type().unwrap() } else { expected };

                builder = builder.with_note(format!(
                    "Consider casting the r-value to `{}`, using `{}`",
                    expected.fg(Color::Green),
                    (if lhs_ty.is_byte() { "shrink()" } else { "extend()" }).fg(Color::Green)
                ));
            }
        }

        SemanticError::UnusedReturnValue { span, ty, decl_span, ret_ty_span } => {
            builder = Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Unused return value")).with_label(
                Label::new((&filename, span.into_range()))
                    .with_message(format!("Unused return value of type `{}`", ty.fg(Color::Red)))
                    .with_color(Color::Red),
            );

            if decl_span.end > 0 {
                builder = builder
                    .with_label(
                        Label::new((&filename, decl_span.into_range())).with_message(format!("Function defined here").fg(Color::Blue)),
                    )
                    .with_label(
                        Label::new((&filename, ret_ty_span.into_range()))
                            .with_message(format!("with return type").fg(Color::Yellow))
                            .with_color(Color::Yellow),
                    );
            }
        }

        SemanticError::MissmatchedReturnType { span, expected, found } => {
            builder = Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Mismatched return type")).with_label(
                Label::new((&filename, span.into_range()))
                    .with_message(format!("Expected return type `{}`, found `{}`", expected.fg(Color::Green), found.fg(Color::Red)))
                    .with_color(Color::Red),
            );
        }

        SemanticError::RedeclaringIdentifier { span, name, decl_span } => {
            builder = Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Redeclaring identifier")).with_label(
                Label::new((&filename, span.into_range()))
                    .with_message(format!("Redecleration of identifier `{}`", name).fg(Color::Red))
                    .with_color(Color::Red),
            );

            if decl_span.end > 0 {
                builder = builder.with_label(
                    Label::new((&filename, decl_span.into_range())).with_message(format!("already delcared here").fg(Color::Blue)),
                );
            }
        }

        SemanticError::RedeclaringFunction { span, name, decl_span } => {
            builder = Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Redeclaring function")).with_label(
                Label::new((&filename, span.into_range()))
                    .with_message(format!("Redecleration of function `{}`", name).fg(Color::Red))
                    .with_color(Color::Red),
            );

            if decl_span.end > 0 {
                builder = builder.with_label(
                    Label::new((&filename, decl_span.into_range())).with_message(format!("already delcared here").fg(Color::Blue)),
                );
            }
        }

        SemanticError::ReachEndOfNonVoidFunction { span, ret_ty } => {
            builder =
                Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Reach end of non-void function")).with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(
                            format!("Control flow reached the end of a non-proc, Function expected to return a `{}`", ret_ty)
                                .fg(Color::Red),
                        )
                        .with_color(Color::Red),
                );
        }

        SemanticError::MultipleArgumentsWithSameName { span, name, span1, span2 } => {
            builder = Report::build(ReportKind::Error, &filename, span.start)
                .with_message(format!("Multiple arguments with same name"))
                .with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(format!("Multiple arguments with the same name `{}`", name).fg(Color::Red))
                        .with_color(Color::Red),
                );

            builder = builder
                .with_label(
                    Label::new((&filename, span1.into_range()))
                        .with_message(format!("First declared here").fg(Color::Blue))
                        .with_color(Color::Blue),
                )
                .with_label(
                    Label::new((&filename, span2.into_range()))
                        .with_message(format!("and re-declared here").fg(Color::Yellow))
                        .with_color(Color::Yellow),
                );
        }

        SemanticError::PtrAsPrimitive { span, ty } => {
            builder =
                Report::build(ReportKind::Error, &filename, span.start).with_message(format!("Non-primitive as expression")).with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(format!("got type `{}` inside an expression, expecting a primitive (int, byte)", ty.fg(Color::Red)))
                        .with_color(Color::Red),
                );
        }

        SemanticError::MissmatchExprTypes { span, lhs, rhs, lhs_span, rhs_span, lhs_decl_span, rhs_decl_span } => {
            builder = Report::build(ReportKind::Error, &filename, span.start)
                .with_message(format!("Mismatched expression types"))
                .with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(
                            format!(
                                "{}`{}`{}`{}`",
                                " Mismatched types, lhs: ".fg(Color::Red),
                                lhs.fg(Color::Magenta),
                                " and rhs: ".fg(Color::Red),
                                rhs.fg(Color::Blue)
                            )
                            .fg(Color::Red),
                        )
                        .with_color(Color::Red)
                        .with_order(4),
                )
                .with_label(
                    Label::new((&filename, lhs_span.into_range()))
                        .with_message(format!("{}", lhs).fg(Color::Magenta))
                        .with_color(Color::Magenta)
                        .with_order(3),
                )
                .with_label(
                    Label::new((&filename, rhs_span.into_range()))
                        .with_message(format!("{}", rhs).fg(Color::Blue))
                        .with_color(Color::Blue)
                        .with_order(2),
                );

            if let Some(span) = lhs_decl_span {
                builder = builder.with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(format!("lhs declared here").fg(Color::Yellow))
                        .with_color(Color::Yellow)
                        .with_order(1),
                );
            }

            if let Some(span) = rhs_decl_span {
                builder = builder.with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(format!("rhs declared here").fg(Color::Yellow))
                        .with_color(Color::Yellow)
                        .with_order(0),
                );
            }
        }

        SemanticError::ArrayAsNonRef { span, ty, decl_span } => {
            builder = Report::build(ReportKind::Error, &filename, decl_span.start)
                .with_message(format!("Array Argument passed non-reference"))
                .with_label(
                    Label::new((&filename, decl_span.into_range()))
                        .with_message(format!("When declaring this function").fg(Color::Red))
                        .with_order(0),
                )
                .with_label(
                    Label::new((&filename, span.into_range()))
                        .with_message(format!("argument has type of array, but it's not passed as refernc `{}`", ty).fg(Color::Red))
                        .with_color(Color::Yellow)
                        .with_order(1),
                );
        }
    }

    builder
        .with_config(Config::default().with_cross_gap(true).with_compact(false).with_underlines(true).with_tab_width(4))
        .finish()
        .eprint((&filename, Source::from(source)))
        .unwrap();
}
