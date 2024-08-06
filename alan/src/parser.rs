use super::*;

use ast::*;
use chumsky::combinator::To;
use lexer::*;

// TODO: lhs rhs bug on operations !!

pub fn parse_lvalue<'src, I>(
) -> impl Parser<'src, I, LValueAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select! {
       Token::Identifier(id) => id,
    }
    .labelled("identifier");

    let subscript = ident
        .clone()
        .then(parse_expr().delimited_by(just(Token::BracketOpen), just(Token::BracketClose)))
        .map(|(id, expr)| LValueAST::ArraySubscript {
            id,
            expr: Box::new(expr),
        });

    let string_const = select! {
        Token::StringConst(s) => LValueAST::String(s),
    };

    let lvalue = string_const
        .or(subscript)
        .or(ident.clone().map(LValueAST::Identifier));

    lvalue.labelled("l-value").as_context()
}

pub fn parse_fncall<'src, I>(
) -> impl Parser<'src, I, FnCallAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select! {
       Token::Identifier(id) => id,
    }
    .labelled("identifier");

    // A list of expressions
    let items = parse_expr()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>();

    let call = ident
        .then(items.delimited_by(
            just(Token::ParentheseisOpen),
            just(Token::ParentheseisClose),
        ))
        .map(|(name, args)| FnCallAST { name, args })
        .labelled("function call");
    call
}

pub fn parse_expr<'src, I>(
) -> impl Parser<'src, I, ExprAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let literal = select! {
            Token::NumberConst(x) => ExprAST::Literal(Literal::Int(x)),
            Token::CharConst(c) => ExprAST::Literal(Literal::Byte(c)),
        }
        .labelled("literal");

        let ident = select! {
           Token::Identifier(id) => id,
        }
        .labelled("identifier");

        // A list of expressions
        let items = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>();

        // Function calls have very high precedence so we prioritise them
        let call = ident
            .then(items.delimited_by(
                just(Token::ParentheseisOpen),
                just(Token::ParentheseisClose),
            ))
            .map(|(name, args)| FnCallAST { name, args })
            .labelled("function call");

        let subscript = ident
            .then(
                expr.clone()
                    .delimited_by(just(Token::BracketOpen), just(Token::BracketClose)),
            )
            .map(|(id, expr)| LValueAST::ArraySubscript {
                id,
                expr: Box::new(expr),
            });

        let string_const = select! {
            Token::StringConst(s) => LValueAST::String(s),
        };

        let lvalue = string_const
            .or(subscript)
            .or(ident.clone().map(LValueAST::Identifier))
            .map(ExprAST::LValue)
            .labelled("l-value");

        let fn_call_into_expr = call.map(ExprAST::FunctionCall);

        let atom = literal
            // Atoms can also just be normal expressions, but surrounded with parentheses
            .or(expr.clone().delimited_by(
                just(Token::ParentheseisOpen),
                just(Token::ParentheseisClose),
            ))
            // Attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with(via_parser(nested_delimiters(
                Token::ParentheseisOpen,
                Token::ParentheseisClose,
                [
                    (Token::BraceOpen, Token::BraceClose),
                    (Token::BracketOpen, Token::BracketClose),
                ],
                |_| (ExprAST::Error),
            )))
            .or(fn_call_into_expr)
            .or(lvalue)
            .boxed();

        let prefix = select! {
            Token::Plus => PrefixOperator::Plus,
            Token::Minus => PrefixOperator::Minus,
        }
        .repeated()
        .foldr(atom, |op, rhs| ExprAST::PrefixOp {
            op,
            expr: Box::new(rhs),
        });

        let mul = prefix.clone().foldl(
            (select! {
                Token::Mul => InfixOperator::Mul,
                Token::Div => InfixOperator::Div,
                Token::Mod => InfixOperator::Mod,
            })
            .then(prefix)
            .repeated(),
            |lhs, (op, rhs)| ExprAST::InfixOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
        );

        let sum = mul.clone().foldl(
            (select! {
                Token::Plus => InfixOperator::Add,
                Token::Minus => InfixOperator::Sub,
            })
            .then(mul)
            .repeated(),
            |lhs, (op, rhs)| ExprAST::InfixOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
        );

        sum.labelled("expression").as_context()
    })
}

pub fn parse_condition<'src, I>(
) -> impl Parser<'src, I, ConditionAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|cond| {
        let bool_literal = select! {
            Token::True => ConditionAST::BoolConst(true),
            Token::False => ConditionAST::BoolConst(false),
        }
        .labelled("bool const");

        let atom = bool_literal
            // Atoms can also just be normal expressions, but surrounded with parentheses
            .or(cond.clone().delimited_by(
                just(Token::ParentheseisOpen),
                just(Token::ParentheseisClose),
            ))
            // Attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with(via_parser(nested_delimiters(
                Token::ParentheseisOpen,
                Token::ParentheseisClose,
                [
                    (Token::BraceOpen, Token::BraceClose),
                    (Token::BracketOpen, Token::BracketClose),
                ],
                |_| (ConditionAST::Error),
            )))
            .boxed();

        let prefix = select! {
            Token::Not => PrefixOperator::Not,
        }
        .repeated()
        .foldr(atom, |op, rhs| ConditionAST::PrefixOp {
            op,
            expr: Box::new(rhs),
        });

        let compare_op = select! {
            Token::Equals => InfixOperator::Equal,
            Token::NotEquals => InfixOperator::NotEqual,
            Token::Greater => InfixOperator::Greater,
            Token::Less => InfixOperator::Less,
            Token::GreaterOrEqual => InfixOperator::GreaterOrEqual,
            Token::LessOrEqual => InfixOperator::LessOrEqual,
        };
        let compare_operations =
            parse_expr()
                .then(compare_op)
                .then(parse_expr())
                .map(|((lhs, op), rhs)| ConditionAST::ExprComparison {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                });

        let logic_and = prefix.clone().or(compare_operations.clone()).foldl(
            (select! {
                Token::And => InfixOperator::LogicAnd,
            })
            .then(prefix.clone().or(compare_operations))
            .repeated(),
            |lhs, (op, rhs)| ConditionAST::InfixLogicOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
        );

        let logic_or = logic_and.clone().foldl(
            (select! {
                Token::Or => InfixOperator::LogicOr,
            })
            .then(logic_and.clone())
            .repeated(),
            |lhs, (op, rhs)| ConditionAST::InfixLogicOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
        );

        logic_or.labelled("condition").as_context()
    })
}

pub fn parse_stmt<'src, I>(
) -> impl Parser<'src, I, StatementAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|stmt| {
        let stmt_end = just(Token::SemiColon).labelled(";");

        let null_stmt = stmt_end.clone().map(|_| StatementAST::Null).labelled(";");

        let assigment = parse_lvalue()
            .then_ignore(just(Token::Assign))
            .then(parse_expr())
            .then_ignore(stmt_end.clone())
            .map(|(lvalue, expr)| StatementAST::Assignment { lvalue, expr })
            .boxed()
            .labelled("assigment");

        let compount_stmt_inner = stmt
            .clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
            .map(|stmts| StatementAST::Compound(stmts))
            .recover_with(via_parser(nested_delimiters(
                Token::BraceOpen,
                Token::BraceOpen,
                [
                    (Token::ParentheseisOpen, Token::ParentheseisClose),
                    (Token::BracketOpen, Token::BracketClose),
                ],
                |_| StatementAST::Error,
            )))
            .boxed()
            .labelled("compound statement");

        let call = parse_fncall()
            .then_ignore(stmt_end.clone())
            .map(|call| StatementAST::FunctionCall(call));

        let if_else = recursive(|if_| {
            just(Token::If)
                .then(parse_condition().delimited_by(
                    just(Token::ParentheseisOpen),
                    just(Token::ParentheseisClose),
                ))
                .then(stmt.clone())
                .then(just(Token::Else).ignore_then(stmt.clone().or(if_)).or_not())
                .map(|(((_, condition), then), else_)| StatementAST::If {
                    condition,
                    then: Box::new(then),
                    else_: else_.map(|v| Box::new(v)),
                })
        });

        let while_ = just(Token::While)
            .ignored()
            .then(parse_condition().delimited_by(
                just(Token::ParentheseisOpen),
                just(Token::ParentheseisClose),
            ))
            .then(stmt.clone())
            .map(|((_, condition), body)| StatementAST::While {
                condition,
                body: Box::new(body),
            });

        let return_ = parse_expr()
            .or_not()
            .delimited_by(just(Token::Return), stmt_end.clone())
            .map(|expr| StatementAST::Return(expr))
            .labelled("return statement");

        assigment
            .or(compount_stmt_inner)
            .or(call)
            .or(if_else)
            .or(while_)
            .or(return_)
            .or(null_stmt)
            .labelled("statement")
            .as_context()
    })
}

pub fn parse_compont_stmt<'src, I>(
) -> impl Parser<'src, I, Vec<StatementAST<'src>>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let compount_stmt = parse_stmt()
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
        .recover_with(via_parser(nested_delimiters(
            Token::BraceOpen,
            Token::BraceOpen,
            [
                (Token::ParentheseisOpen, Token::ParentheseisClose),
                (Token::BracketOpen, Token::BracketClose),
            ],
            |_| vec![StatementAST::Error],
        )))
        .labelled("compound statement");

    compount_stmt.labelled("compound statement").as_context()
}

pub fn parse_function<'src, I>(
) -> impl Parser<'src, I, FunctionAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|func| {
        let ident = select! {
           Token::Identifier(id) => id,
        }
        .labelled("identifier");

        let data_type = select! {
            Token::Int => Type::Int,
            Token::Byte => Type::Byte,
        }
        .labelled("data-type");

        let ftype = data_type
            .then_ignore(just(Token::BracketOpen).then(just(Token::BracketClose)))
            .map(|t| Type::Array(Box::new(t)))
            .or(data_type);

        let parameters_type = just(Token::Ref)
            .or_not()
            .then(ftype)
            .clone()
            .map(|(r, type_)| match r {
                Some(_) => Type::Ref(Box::new(type_)),
                None => type_,
            })
            .labelled("parameter type");

        let fparam = ident
            .clone()
            .then_ignore(just(Token::Colon))
            .then(parameters_type)
            .map(|(name, type_)| VarDefAST { name, type_ })
            .labelled("function parameter");

        let fparams = fparam
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(
                just(Token::ParentheseisOpen),
                just(Token::ParentheseisClose),
            )
            .labelled("function parameters");

        let r_type = data_type
            .or(select! {
                    Token::Proc => Type::Void,
            })
            .labelled("return type");

        let local_var_def = ident
            .clone()
            .then_ignore(just(Token::Colon))
            .then(data_type)
            .then(
                select! {Token::NumberConst(size)=>size}
                    .delimited_by(just(Token::BracketOpen), just(Token::BracketClose))
                    .or_not(),
            )
            .then_ignore(just(Token::SemiColon))
            .map(|((name, type_), size)| match size {
                Some(size) => LocalDefinitionAST::ArrayDef { name, type_, size },
                None => LocalDefinitionAST::VarDef { name, type_ },
            });

        let local_def = local_var_def.or(func.map(LocalDefinitionAST::FunctionDef));

        let locals = local_def
            .repeated()
            .collect::<Vec<_>>()
            .labelled("locals definitions");

        let func_def = ident
            .then(fparams)
            .then_ignore(just(Token::Colon))
            .then(r_type)
            .then(locals)
            .then(parse_compont_stmt())
            .map(|((((name, params), r_type), locals), body)| FunctionAST {
                name,
                r_type,
                params,
                body,
                locals,
            });

        func_def.labelled("function definition").as_context()
    })
}

// This parser will be used to parse the stdlib functions, and generate extern llvm binindings
// pub fn extern_func_parser<'src, I>(
// ) -> impl Parser<'src, I, Vec<FunctionAST<'src>>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
// where
//     I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
// {
//     // Most of the code is copy paste from parse_function
//     // but it's easier to have a separate function for the extern functions

//     recursive(|func| {
//         let ident = select! {
//            Token::Identifier(id) => id,
//         }
//         .labelled("identifier");

//         let data_type = select! {
//             Token::Int => Type::Int,
//             Token::Byte => Type::Byte,
//         }
//         .labelled("data-type");

//         let ftype = data_type
//             .then_ignore(just(Token::BracketOpen).then(just(Token::BracketClose)))
//             .map(|t| Type::Array(Box::new(t)))
//             .or(data_type);

//         let parameters_type = just(Token::Ref)
//             .or_not()
//             .then(ftype)
//             .clone()
//             .map(|(r, type_)| match r {
//                 Some(_) => Type::Ref(Box::new(type_)),
//                 None => type_,
//             })
//             .labelled("parameter type");

//         let fparam = ident
//             .clone()
//             .then_ignore(just(Token::Colon))
//             .then(parameters_type)
//             .map(|(name, type_)| VarDefAST { name, type_ })
//             .labelled("function parameter");

//         let fparams = fparam
//             .separated_by(just(Token::Comma))
//             .collect::<Vec<_>>()
//             .delimited_by(
//                 just(Token::ParentheseisOpen),
//                 just(Token::ParentheseisClose),
//             )
//             .labelled("function parameters");

//         let r_type = data_type
//             .or(select! {
//                     Token::Proc => Type::Void,
//             })
//             .labelled("return type");

//         let func_def = ident
//             .then(fparams)
//             .then_ignore(just(Token::Colon))
//             .then(r_type)
//             .then_ignore(just(Token::SemiColon))
//             .map(|((name, params), r_type)| FunctionAST {
//                 name,
//                 r_type,
//                 params,
//                 locals: vec![],
//                 body: vec![],
//             });

//         func_def.repeated().collect::<Vec<_>>()
//     })
// }
