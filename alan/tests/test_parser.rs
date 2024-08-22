mod parser_tests {
    use alan::ast::*;
    use alan::lexer::*;
    use alan::parser::*;
    use alan::Span;
    use logos::Logos;

    use crate::parser_tests::LValueKind::Identifier;

    use chumsky::{input::Stream, prelude::*};

    fn parse_expr_test(input: &str) -> ExprAST {
        let lex = Token::lexer(input);

        let token_iter = lex.spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(e) => (Token::Error(e), span.into()),
        });

        let token_stream = Stream::from_iter(token_iter).spanned((input.len()..input.len()).into());

        parse_expr().parse(token_stream).unwrap()
    }

    fn parse_function_test(input: &str) -> FunctionAST {
        let lex = Token::lexer(input);

        let token_iter = lex.spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(e) => (Token::Error(e), span.into()),
        });

        let token_stream = Stream::from_iter(token_iter).spanned((input.len()..input.len()).into());

        parse_function().parse(token_stream).unwrap()
    }

    #[test]
    fn test_expr() {
        let input = "(((((((((((((((((((((((( 42 + 17) + 42)))))))))))))))))))))))";
        let res = parse_expr_test(input);
        assert_eq!(
            res,
            ExprAST {
                span: Span::new(23, 38),
                kind: ExprKind::InfixOp {
                    lhs: Box::new(ExprAST {
                        span: Span::new(25, 32),
                        kind: ExprKind::InfixOp {
                            lhs: Box::new(ExprAST { span: Span::new(25, 27), kind: ExprKind::Literal(Literal::Int(42)) }),
                            op: InfixOperator::Add,
                            rhs: Box::new(ExprAST { span: Span::new(30, 32), kind: ExprKind::Literal(Literal::Int(17)) }),
                        },
                    }),
                    op: InfixOperator::Add,
                    rhs: Box::new(ExprAST { span: Span::new(36, 38), kind: ExprKind::Literal(Literal::Int(42)) }),
                },
            },
            "{:#?}",
            res
        );

        let input = " 11 + 12 * 13 + (14 - 15) % 16";
        let res = parse_expr_test(input);
        assert_eq!(
            res,
            ExprAST {
                span: Span::new(1, 30),
                kind: ExprKind::InfixOp {
                    lhs: Box::new(ExprAST {
                        span: Span::new(1, 13),
                        kind: ExprKind::InfixOp {
                            lhs: Box::new(ExprAST { span: Span::new(1, 3), kind: ExprKind::Literal(Literal::Int(11)) }),
                            op: InfixOperator::Add,
                            rhs: Box::new(ExprAST {
                                span: Span::new(6, 13),
                                kind: ExprKind::InfixOp {
                                    lhs: Box::new(ExprAST { span: Span::new(6, 8), kind: ExprKind::Literal(Literal::Int(12)) }),
                                    op: InfixOperator::Mul,
                                    rhs: Box::new(ExprAST { span: Span::new(11, 13), kind: ExprKind::Literal(Literal::Int(13)) }),
                                },
                            }),
                        },
                    }),
                    op: InfixOperator::Add,
                    rhs: Box::new(ExprAST {
                        span: Span::new(16, 30),
                        kind: ExprKind::InfixOp {
                            lhs: Box::new(ExprAST {
                                span: Span::new(17, 24),
                                kind: ExprKind::InfixOp {
                                    lhs: Box::new(ExprAST { span: Span::new(17, 19), kind: ExprKind::Literal(Literal::Int(14)) }),
                                    op: InfixOperator::Sub,
                                    rhs: Box::new(ExprAST { span: Span::new(22, 24), kind: ExprKind::Literal(Literal::Int(15)) }),
                                },
                            }),
                            op: InfixOperator::Mod,
                            rhs: Box::new(ExprAST { span: Span::new(28, 30), kind: ExprKind::Literal(Literal::Int(16)) }),
                        },
                    }),
                },
            },
            "{:#?}",
            res
        );

        let input = "a + b() + c(42, 1 + 7, a[i]) + a[42]";
        let res = parse_expr_test(input);
        assert_eq!(
            res,
            ExprAST {
                span: Span::new(0, 36),
                kind: ExprKind::InfixOp {
                    lhs: Box::new(ExprAST {
                        span: Span::new(0, 28),
                        kind: ExprKind::InfixOp {
                            lhs: Box::new(ExprAST {
                                span: Span::new(0, 7),
                                kind: ExprKind::InfixOp {
                                    lhs: Box::new(ExprAST {
                                        span: Span::new(0, 1),
                                        kind: ExprKind::LValue(LValueAST { kind: LValueKind::Identifier("a"), span: Span::new(0, 1) })
                                    }),
                                    op: InfixOperator::Add,
                                    rhs: Box::new(ExprAST {
                                        span: Span::new(4, 7),
                                        kind: ExprKind::FunctionCall(FnCallAST { name: "b", args: vec![], span: Span::new(4, 7) }),
                                    }),
                                },
                            }),
                            op: InfixOperator::Add,
                            rhs: Box::new(ExprAST {
                                span: Span::new(10, 28),
                                kind: ExprKind::FunctionCall(FnCallAST {
                                    name: "c",
                                    args: vec![
                                        ExprAST { span: Span::new(12, 14), kind: ExprKind::Literal(Literal::Int(42)) },
                                        ExprAST {
                                            span: Span::new(16, 21),
                                            kind: ExprKind::InfixOp {
                                                lhs: Box::new(ExprAST {
                                                    span: Span::new(16, 17),
                                                    kind: ExprKind::Literal(Literal::Int(1)),
                                                }),
                                                op: InfixOperator::Add,
                                                rhs: Box::new(ExprAST {
                                                    span: Span::new(20, 21),
                                                    kind: ExprKind::Literal(Literal::Int(7)),
                                                }),
                                            },
                                        },
                                        ExprAST {
                                            span: Span::new(23, 27),
                                            kind: ExprKind::LValue(LValueAST {
                                                kind: LValueKind::ArraySubscript {
                                                    id: "a",
                                                    expr: Box::new(ExprAST {
                                                        span: Span::new(25, 26),
                                                        kind: ExprKind::LValue(LValueAST {
                                                            kind: LValueKind::Identifier("i"),
                                                            span: Span::new(25, 26)
                                                        }),
                                                    }),
                                                },
                                                span: Span::new(23, 27)
                                            }),
                                        },
                                    ],
                                    span: Span::new(10, 28),
                                }),
                            }),
                        },
                    }),
                    op: InfixOperator::Add,
                    rhs: Box::new(ExprAST {
                        span: Span::new(31, 36),
                        kind: ExprKind::LValue(LValueAST {
                            kind: LValueKind::ArraySubscript {
                                id: "a",
                                expr: Box::new(ExprAST { span: Span::new(33, 35), kind: ExprKind::Literal(Literal::Int(42)) }),
                            },
                            span: Span::new(31, 36)
                        }),
                    }),
                },
            },
            "{:#?}",
            res
        );
    }

    #[test]
    fn test_function() {
        let input = r#"test_fun(x :int, y: byte, z : reference byte[]) : proc
            x : int;
            y : int[100];
            (* This is a comment *)
        { ;;; -- A comment
         x = 42; 
         }
        "#;

        let res: FunctionAST = parse_function_test(input);

        let expected_ast = FunctionAST {
            name: "test_fun",
            r_type: Type {
                kind: TypeKind::Void,
                span: Span::new(50, 54), // Span for "proc"
            },
            params: vec![
                VarDefAST {
                    name: "x",
                    type_: Type {
                        kind: TypeKind::Int,
                        span: Span::new(12, 15), // Span for "int"
                    },
                    span: Span::new(9, 15), // Span for "x :int"
                },
                VarDefAST {
                    name: "y",
                    type_: Type {
                        kind: TypeKind::Byte,
                        span: Span::new(20, 24), // Span for "byte"
                    },
                    span: Span::new(17, 24), // Span for "y: byte"
                },
                VarDefAST {
                    name: "z",
                    type_: Type {
                        kind: TypeKind::Ref(Box::new(TypeKind::Array(Box::new(TypeKind::Byte)))),
                        span: Span::new(30, 46), // Span for "reference byte[]"
                    },
                    span: Span::new(26, 46), // Span for "z : reference byte[]"
                },
            ],
            locals: vec![
                LocalDefinitionAST::VarDef(VarDefAST {
                    name: "x",
                    type_: Type {
                        kind: TypeKind::Int,
                        span: Span::new(71, 74), // Span for "int"
                    },
                    span: Span::new(67, 75), // Span for "x : int"
                }),
                LocalDefinitionAST::ArrayDef(ArrayDefAST {
                    name: "y",
                    type_: Type {
                        kind: TypeKind::Int,
                        span: Span::new(92, 95), // Span for "int"
                    },
                    size: 100,
                    span: Span::new(88, 101), // Span for "y : int[100]"
                }),
            ],
            body: vec![StatementAST {
                kind: StatementKind::Assignment {
                    lvalue: LValueAST { kind: LValueKind::Identifier("x"), span: Span::new(174, 175) }, // Span for "x"
                    expr: ExprAST {
                        span: Span::new(178, 180), // Span for "42"
                        kind: ExprKind::Literal(Literal::Int(42)),
                    },
                },
                span: Span::new(174, 181), // Span for "x = 42;"
            }],
            span: Span::new(0, 193),          // Span for the entire function
            signature_span: Span::new(0, 54), // Span for the function signature
        };

        assert_eq!(res, expected_ast, "{:#?}", res);

        println!("{:#?}", res);
    }
}
