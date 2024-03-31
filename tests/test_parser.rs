mod parser_tests {
    use alan::ast::*;
    use alan::lexer::*;
    use alan::parser::*;
    use chumsky::error::Error;
    use logos::Logos;
    use logos::Span;

    use chumsky::{
        input::{Stream, ValueInput},
        prelude::*,
    };

    fn parse_expr_test(input: &str) -> ExprAST {
        let lex = Token::lexer(input);

        let token_iter = lex.spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(e) => (Token::Error(e), span.into()),
        });

        let token_stream = Stream::from_iter(token_iter).spanned((input.len()..input.len()).into());

        parse_expr().parse(token_stream).unwrap()
    }

    #[test]
    fn test_expr() {
        let input = "(((((((((((((((((((((((( 42 + 17) + 42)))))))))))))))))))))))";
        let res = parse_expr_test(input);
        assert_eq!(
            res,
            ExprAST::InfixOp {
                lhs: Box::new(ExprAST::Literal(Literal::Int(42))),
                op: InfixOperator::Add,
                rhs: Box::new(ExprAST::InfixOp {
                    lhs: Box::new(ExprAST::Literal(Literal::Int(17))),
                    op: InfixOperator::Add,
                    rhs: Box::new(ExprAST::Literal(Literal::Int(42))),
                }),
            },
            "{:#?}",
            res
        );

        let input = " 11 + 12 * 13 + (14 - 15) % 16";
        let res = parse_expr_test(input);
        assert_eq!(
            res,
            ExprAST::InfixOp {
                lhs: Box::new(ExprAST::InfixOp {
                    lhs: Box::new(ExprAST::Literal(Literal::Int(16),)),
                    op: InfixOperator::Mod,
                    rhs: Box::new(ExprAST::InfixOp {
                        lhs: Box::new(ExprAST::Literal(Literal::Int(15))),
                        op: InfixOperator::Sub,
                        rhs: Box::new(ExprAST::Literal(Literal::Int(14))),
                    }),
                }),
                op: InfixOperator::Add,
                rhs: Box::new(ExprAST::InfixOp {
                    lhs: Box::new(ExprAST::InfixOp {
                        lhs: Box::new(ExprAST::Literal(Literal::Int(13))),
                        op: InfixOperator::Mul,
                        rhs: Box::new(ExprAST::Literal(Literal::Int(12))),
                    }),
                    op: InfixOperator::Add,
                    rhs: Box::new(ExprAST::Literal(Literal::Int(11))),
                }),
            },
            "{:#?}",
            res
        );

        let input = "a + b() + c(42, 1 + 7, a[i]) + a[42]";
        let res = parse_expr_test(input);
        assert_eq!(
            res,
            ExprAST::InfixOp {
                lhs: Box::new(ExprAST::LValue(LValueAST::ArraySubscript {
                    id: "a",
                    expr: Box::new(ExprAST::Literal(Literal::Int(42))),
                },)),
                op: InfixOperator::Add,
                rhs: Box::new(ExprAST::InfixOp {
                    lhs: Box::new(ExprAST::Call {
                        function: "c",
                        args: vec![
                            ExprAST::Literal(Literal::Int(42)),
                            ExprAST::InfixOp {
                                lhs: Box::new(ExprAST::Literal(Literal::Int(7))),
                                op: InfixOperator::Add,
                                rhs: Box::new(ExprAST::Literal(Literal::Int(1))),
                            },
                            ExprAST::LValue(LValueAST::ArraySubscript {
                                id: "a",
                                expr: Box::new(ExprAST::LValue(LValueAST::Identifier("i"))),
                            },),
                        ],
                    }),
                    op: InfixOperator::Add,
                    rhs: Box::new(ExprAST::InfixOp {
                        lhs: Box::new(ExprAST::Call {
                            function: "b",
                            args: vec![],
                        }),
                        op: InfixOperator::Add,
                        rhs: Box::new(ExprAST::LValue(LValueAST::Identifier("a"))),
                    }),
                }),
            },
            "{:#?}",
            res
        );
    }
}

