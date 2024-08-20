#[cfg(test)]
mod lexer_tests {
    use alan::lexer::{LexingError, Token};
    use logos::Logos;
    use logos::Span;

    #[test]
    fn test_identidiers() {
        let tokens: Vec<_> = Token::lexer("foo foo42 foo_42 _foo_42_ foo42 42foo").spanned().collect();
        assert_eq!(
            tokens,
            &[
                (Ok(Token::Identifier("foo")), 0..3),
                (Ok(Token::Identifier("foo42")), 4..9),
                (Ok(Token::Identifier("foo_42")), 10..16),
                (Ok(Token::Identifier("_foo_42_")), 17..25),
                (Ok(Token::Identifier("foo42")), 26..31),
                (Err(LexingError::InvalidInteger), 32..37)
            ]
        )
    }

    #[test]
    fn test_numbers() {
        let tokens: Vec<_> = Token::lexer("42 17 170000000000000000000000000000000000000").spanned().collect();
        assert_eq!(
            tokens,
            &[(Ok(Token::NumberConst(42)), 0..2), (Ok(Token::NumberConst(17)), 3..5), (Err(LexingError::IntergerOverflow), 6..45),]
        )
    }

    #[test]
    fn test_chars() {
        let tokens: Vec<_> = Token::lexer(r#"'a' '"' '\0' '\\' '\x77' '\xFF' '\g' '\'' '\"'"#).spanned().collect();
        assert_eq!(
            tokens,
            &[
                (Ok(Token::CharConst('a')), 0..3),
                (Ok(Token::CharConst('"')), 4..7),
                (Ok(Token::CharConst('\0')), 8..12),
                (Ok(Token::CharConst('\\')), 13..17),
                (Ok(Token::CharConst('w')), 18..24),
                (Err(LexingError::NonAsciiCharacter(Span { start: 25, end: 31 })), 25..31),
                (Err(LexingError::InvalidEscapeCode), 32..36),
                (Ok(Token::CharConst('\'')), 37..41),
                (Ok(Token::CharConst('\"')), 42..46),
            ]
        )
    }

    #[test]
    fn test_strings() {
        use internment::Intern;
        let tokens: Vec<_> = Token::lexer(r#" "Hello World! 42\x77\0\n\"" "this is an invalid ascii char \xFE" "#).spanned().collect();
        assert_eq!(
            tokens,
            &[
                (Ok(Token::StringConst(Intern::new("Hello World! 42w\0\n\"".to_owned()))), 1..28),
                (Err(LexingError::NonAsciiCharacter(Span { start: 60, end: 64 })), 29..65),
            ]
        )
    }

    #[test]
    fn test_comments() {
        let tokens: Vec<_> = Token::lexer(" 42 (*....\n ***\n iuigu 76..*) 17 -- single \n 42").spanned().collect();

        assert_eq!(
            tokens,
            &[(Ok(Token::NumberConst(42)), 1..3), (Ok(Token::NumberConst(17)), 30..32), (Ok(Token::NumberConst(42)), 45..47),]
        );
    }

    #[test]
    fn test_operators() {
        let tokens: Vec<_> = Token::lexer(" + - * / % [] () {} == != > <= & | = , ; :").spanned().collect();

        assert_eq!(
            tokens,
            &[
                (Ok(Token::Plus), 1..2),
                (Ok(Token::Minus), 3..4),
                (Ok(Token::Mul), 5..6),
                (Ok(Token::Div), 7..8),
                (Ok(Token::Mod), 9..10),
                (Ok(Token::BracketOpen), 11..12),
                (Ok(Token::BracketClose), 12..13),
                (Ok(Token::ParentheseisOpen), 14..15),
                (Ok(Token::ParentheseisClose), 15..16),
                (Ok(Token::BraceOpen), 17..18),
                (Ok(Token::BraceClose), 18..19),
                (Ok(Token::Equals), 20..22),
                (Ok(Token::NotEquals), 23..25),
                (Ok(Token::Greater), 26..27),
                (Ok(Token::LessOrEqual), 28..30),
                (Ok(Token::And), 31..32),
                (Ok(Token::Or), 33..34),
                (Ok(Token::Assign), 35..36),
                (Ok(Token::Comma), 37..38),
                (Ok(Token::SemiColon), 39..40),
                (Ok(Token::Colon), 41..42)
            ]
        )
    }

    #[test]
    fn test_keywords() {
        let tokens: Vec<_> = Token::lexer("byte else false if int proc reference return while true").spanned().collect();

        assert_eq!(
            tokens,
            &[
                (Ok(Token::Byte), 0..4),
                (Ok(Token::Else), 5..9),
                (Ok(Token::False), 10..15),
                (Ok(Token::If), 16..18),
                (Ok(Token::Int), 19..22),
                (Ok(Token::Proc), 23..27),
                (Ok(Token::Ref), 28..37),
                (Ok(Token::Return), 38..44),
                (Ok(Token::While), 45..50),
                (Ok(Token::True), 51..55),
            ]
        );
    }
}
