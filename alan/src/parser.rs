use super::*;

use ast::*;
// use chumsky::combinator::To;
use lexer::*;

pub fn parse_lvalue<'src, I>() -> impl Parser<'src, I, LValueAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
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
        .map_with(|(id, expr), e| LValueAST { kind: LValueKind::ArraySubscript { id, expr: Box::new(expr) }, span: e.span() });

    let string_const = select! {
        Token::StringConst(s) => LValueKind::String(s),
    }
    .map_with(|kind, e| LValueAST { kind, span: e.span() });

    let single_ident = ident.map_with(|id, e| LValueAST { kind: LValueKind::Identifier(id), span: e.span() });

    let lvalue = string_const.or(subscript).or(single_ident);

    lvalue.labelled("l-value").as_context()
}

pub fn parse_fncall<'src, I>() -> impl Parser<'src, I, FnCallAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select! {
       Token::Identifier(id) => id,
    }
    .labelled("identifier");

    // A list of expressions
    let items = parse_expr().separated_by(just(Token::Comma)).collect::<Vec<_>>();

    let call = ident
        .then(items.delimited_by(just(Token::ParenthesisOpen), just(Token::ParenthesisClose)))
        .map_with(|(name, args), e| FnCallAST { name, args, span: e.span() })
        .labelled("function call");
    call
}

pub fn parse_expr<'src, I>() -> impl Parser<'src, I, ExprAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let literal = select! {
            Token::NumberConst(x) => ExprKind::Literal(Literal::Int(x)),
            Token::CharConst(c) => ExprKind::Literal(Literal::Byte(c)),
        }
        .map_with(|kind, e| ExprAST { kind, span: e.span() })
        .labelled("literal");

        let ident = select! {
           Token::Identifier(id) => id,
        }
        .labelled("identifier");

        // A list of expressions
        let items = expr.clone().separated_by(just(Token::Comma)).collect::<Vec<_>>();

        // Function calls have very high precedence so we prioritize them
        let call = ident
            .then(items.delimited_by(just(Token::ParenthesisOpen), just(Token::ParenthesisClose)))
            .map_with(|(name, args), e| FnCallAST { name, args, span: e.span() })
            .labelled("function call");

        let subscript = ident
            .then(expr.clone().delimited_by(just(Token::BracketOpen), just(Token::BracketClose)))
            .map_with(|(id, expr), e| LValueAST { kind: LValueKind::ArraySubscript { id, expr: Box::new(expr) }, span: e.span() });

        let string_const = select! {
            Token::StringConst(s) => LValueKind::String(s),
        }
        .map_with(|kind, e| LValueAST { kind, span: e.span() });

        let single_ident = ident.map_with(|id, e| LValueAST { kind: LValueKind::Identifier(id), span: e.span() });

        let lvalue = string_const
            .or(subscript)
            .or(single_ident)
            .map_with(|kind, e| ExprAST { kind: ExprKind::LValue(kind), span: e.span() })
            .labelled("l-value")
            .as_context();

        let fn_call_into_expr = call.map_with(|call, e| ExprAST { kind: ExprKind::FunctionCall(call), span: e.span() });

        let atom = literal
            // Atoms can also just be normal expressions, but surrounded with parentheses
            .or(expr.clone().delimited_by(just(Token::ParenthesisOpen), just(Token::ParenthesisClose)))
            // Attempt to recover anything that looks like a parenthesized expression but contains errors
            .recover_with(via_parser(nested_delimiters(
                Token::ParenthesisOpen,
                Token::ParenthesisClose,
                [(Token::BraceOpen, Token::BraceClose), (Token::BracketOpen, Token::BracketClose)],
                |_| (ExprAST { kind: ExprKind::Error, span: Span::new(0, 0) }),
            )))
            .or(fn_call_into_expr)
            .or(lvalue)
            .boxed();

        let prefix = select! {
            Token::Plus => PrefixOperator::Plus,
            Token::Minus => PrefixOperator::Minus,
        }
        .repeated()
        .foldr_with(atom, |op: PrefixOperator, rhs, e| ExprAST { kind: ExprKind::PrefixOp { op, expr: Box::new(rhs) }, span: e.span() });

        let mul = prefix.clone().foldl_with(
            (select! {
                Token::Mul => InfixOperator::Mul,
                Token::Div => InfixOperator::Div,
                Token::Mod => InfixOperator::Mod,
            })
            .then(prefix)
            .repeated(),
            |lhs, (op, rhs), e| ExprAST { kind: ExprKind::InfixOp { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }, span: e.span() },
        );

        let sum = mul.clone().foldl_with(
            (select! {
                Token::Plus => InfixOperator::Add,
                Token::Minus => InfixOperator::Sub,
            })
            .then(mul)
            .repeated(),
            |lhs, (op, rhs), e| ExprAST { kind: ExprKind::InfixOp { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }, span: e.span() },
        );

        sum.labelled("expression").as_context()
    })
}

pub fn parse_condition<'src, I>() -> impl Parser<'src, I, ConditionAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|cond| {
        let bool_literal = select! {
            Token::True => ConditionKind::BoolConst(true),
            Token::False => ConditionKind::BoolConst(false),
        }
        .map_with(|kind, e| ConditionAST { kind, span: e.span() })
        .labelled("bool const");

        let atom = bool_literal
            // Atoms can also just be normal expressions, but surrounded with parentheses
            .or(cond.clone().delimited_by(just(Token::ParenthesisOpen), just(Token::ParenthesisClose)))
            // Attempt to recover anything that looks like a parenthesized expression but contains errors
            .recover_with(via_parser(nested_delimiters(
                Token::ParenthesisOpen,
                Token::ParenthesisClose,
                [(Token::BraceOpen, Token::BraceClose), (Token::BracketOpen, Token::BracketClose)],
                |_| (ConditionAST { kind: ConditionKind::Error, span: Span::new(0, 0) }),
            )))
            .boxed();

        let prefix = select! {
            Token::Not => PrefixOperator::Not,
        }
        .repeated()
        .foldr_with(atom, |op, rhs, e| ConditionAST { kind: ConditionKind::PrefixOp { op, expr: Box::new(rhs) }, span: e.span() });

        let compare_op = select! {
            Token::Equals => InfixOperator::Equal,
            Token::NotEquals => InfixOperator::NotEqual,
            Token::Greater => InfixOperator::Greater,
            Token::Less => InfixOperator::Less,
            Token::GreaterOrEqual => InfixOperator::GreaterOrEqual,
            Token::LessOrEqual => InfixOperator::LessOrEqual,
        };
        let compare_operations = parse_expr().then(compare_op).then(parse_expr()).map_with(|((lhs, op), rhs), e| ConditionAST {
            kind: ConditionKind::ExprComparison { lhs: Box::new(lhs), op, rhs: Box::new(rhs) },
            span: e.span(),
        });

        let logic_and = prefix.clone().or(compare_operations.clone()).foldl_with(
            (select! {
                Token::And => InfixOperator::LogicAnd,
            })
            .then(prefix.clone().or(compare_operations))
            .repeated(),
            |lhs, (op, rhs), e| ConditionAST {
                kind: ConditionKind::InfixLogicOp { lhs: Box::new(lhs), op, rhs: Box::new(rhs) },
                span: e.span(),
            },
        );

        let logic_or = logic_and.clone().foldl_with(
            (select! {
                Token::Or => InfixOperator::LogicOr,
            })
            .then(logic_and.clone())
            .repeated(),
            |lhs, (op, rhs), e| ConditionAST {
                kind: ConditionKind::InfixLogicOp { lhs: Box::new(lhs), op, rhs: Box::new(rhs) },
                span: e.span(),
            },
        );

        logic_or.labelled("condition").as_context()
    })
}

pub fn parse_stmt<'src, I>() -> impl Parser<'src, I, StatementAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|stmt| {
        let stmt_end = just(Token::SemiColon).labelled(";");

        let null_stmt = stmt_end.clone().ignored().repeated();

        let assigment = parse_lvalue()
            .then_ignore(just(Token::Assign))
            .then(parse_expr())
            .then_ignore(stmt_end.clone())
            .map_with(|(lvalue, expr), e| StatementAST { kind: StatementKind::Assignment { lvalue, expr }, span: e.span() })
            .boxed()
            .labelled("assigment");

        let compount_stmt_inner = stmt
            .clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
            .map_with(|stmts, e| StatementAST { kind: StatementKind::Compound(stmts), span: e.span() })
            .recover_with(via_parser(nested_delimiters(
                Token::BraceOpen,
                Token::BraceOpen,
                [(Token::ParenthesisOpen, Token::ParenthesisClose), (Token::BracketOpen, Token::BracketClose)],
                |_| StatementAST { kind: StatementKind::Error, span: Span::new(0, 0) },
            )))
            .boxed()
            .labelled("compound statement");

        let call = parse_fncall()
            .then_ignore(stmt_end.clone())
            .map_with(|call, e| StatementAST { kind: StatementKind::FunctionCall(call), span: e.span() });

        let if_else = recursive(|if_| {
            just(Token::If)
                .then(parse_condition().delimited_by(just(Token::ParenthesisOpen), just(Token::ParenthesisClose)))
                .then(stmt.clone())
                .then(just(Token::Else).ignore_then(stmt.clone().or(if_)).or_not())
                .map_with(|(((_, condition), then), else_), e| StatementAST {
                    kind: StatementKind::If { condition, then: Box::new(then), else_: else_.map(|v| Box::new(v)) },
                    span: e.span(),
                })
        });

        let while_ = just(Token::While)
            .ignored()
            .then(parse_condition().delimited_by(just(Token::ParenthesisOpen), just(Token::ParenthesisClose)))
            .then(stmt.clone())
            .map_with(|((_, condition), body), e| StatementAST {
                kind: StatementKind::While { condition, body: Box::new(body) },
                span: e.span(),
            });

        let return_ = parse_expr()
            .or_not()
            .delimited_by(just(Token::Return), stmt_end.clone())
            .map_with(|expr, e| StatementAST { kind: StatementKind::Return(expr), span: e.span() })
            .labelled("return statement");

        assigment
            .or(compount_stmt_inner)
            .or(call)
            .or(if_else)
            .or(while_)
            .or(return_)
            .padded_by(null_stmt)
            .labelled("statement")
            .as_context()
    })
}

pub fn parse_compont_stmt<'src, I>() -> impl Parser<'src, I, Vec<StatementAST<'src>>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
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
            [(Token::ParenthesisOpen, Token::ParenthesisClose), (Token::BracketOpen, Token::BracketClose)],
            |_| vec![StatementAST { kind: StatementKind::Error, span: Span::new(0, 0) }],
        )))
        .labelled("compound statement");

    compount_stmt.labelled("compound statement").as_context()
}

pub fn parse_function<'src, I>() -> impl Parser<'src, I, FunctionAST<'src>, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|func| {
        let ident = select! {
           Token::Identifier(id) => id,
        }
        .labelled("identifier");

        let data_type = select! {
            Token::Int => TypeKind::Int,
            Token::Byte => TypeKind::Byte,
        }
        .labelled("data-type");

        let data_type_mapped = data_type.map_with(|type_, e| (Type { kind: type_, span: e.span() }));

        let ftype = data_type
            .then_ignore(just(Token::BracketOpen).then(just(Token::BracketClose)))
            .map_with(|t, e| Type { kind: TypeKind::Array(Box::new(t)), span: e.span() })
            .or(data_type_mapped);

        let parameters_type = just(Token::Ref)
            .or_not()
            .then(ftype)
            .clone()
            .map_with(|(r, type_), e| match r {
                Some(_) => Type { kind: TypeKind::Ref(Box::new(type_.kind)), span: e.span() },
                None => type_,
            })
            .labelled("parameter type");

        let fparam = ident
            .clone()
            .then_ignore(just(Token::Colon))
            .then(parameters_type)
            .map_with(|(name, type_), e| VarDefAST { name, type_, span: e.span() })
            .labelled("function parameter");

        let fparams = fparam
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ParenthesisOpen), just(Token::ParenthesisClose))
            .labelled("function parameters");

        let r_type = data_type
            .or(select! {
                    Token::Proc => TypeKind::Void,
            })
            .map_with(|type_, e| Type { kind: type_, span: e.span() })
            .labelled("return type");

        let local_var_def = ident
            .clone()
            .then_ignore(just(Token::Colon))
            .then(data_type_mapped)
            .then(select! {Token::NumberConst(size)=>size}.delimited_by(just(Token::BracketOpen), just(Token::BracketClose)).or_not())
            .then_ignore(just(Token::SemiColon))
            .map_with(|((name, type_), size), e| match size {
                Some(size) => LocalDefinitionAST::ArrayDef(ArrayDefAST { name, type_, size, span: e.span() }),
                None => LocalDefinitionAST::VarDef(VarDefAST { name, type_, span: e.span() }),
            });

        let local_def = local_var_def.or(func.map(LocalDefinitionAST::FunctionDef));

        let locals = local_def.repeated().collect::<Vec<_>>().labelled("locals definitions");

        let func_def = ident
            .then(fparams)
            .then_ignore(just(Token::Colon))
            .then(r_type)
            .map_with(|((name, params), r_type), e| (name, params, r_type, e.span()))
            .then(locals)
            .then(parse_compont_stmt())
            .map_with(|(((name, params, r_type, signature_span), locals), body), e| FunctionAST {
                name,
                r_type,
                params,
                body,
                locals,
                signature_span,
                span: e.span(),
            });

        func_def.labelled("function definition").as_context()
    })
}

// This parser will be used to parse the stdlib functions, and generate extern llvm binindings
// pub fn extern_func_parser<'src, I>(
// ) -> impl Parser<'src, I, Vec<FunctionPrototypeAST<'src>>, extra::Err<Rich<'src, Token<'src>, Span>>>
//        + Clone
// where
//     I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
// {
//     // Most of the code is copy paste from parse_function
//     // but it's easier to have a separate function for the extern functions

//     recursive(|_| {
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
//             .or_not()
//             .clone()
//             .then_ignore(just(Token::Colon))
//             .then(parameters_type)
//             .map(|(name, type_)| VarDefAST {
//                 name: name.unwrap_or(""),
//                 type_,
//             })
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
//             .map(|((name, params), r_type)| FunctionPrototypeAST {
//                 name,
//                 r_type,
//                 params,
//             });

//         func_def.repeated().collect::<Vec<_>>()
//     })
// }
