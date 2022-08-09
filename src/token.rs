/*

void main() {
    int x;
    x = 1;
}



*/

use nom::{
    branch::*, bytes::complete::*, character::complete::*, character::*, combinator::*, multi::*,
    sequence::*, IResult
};
use nom_locate::{LocatedSpan, position};

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'i> {
    pub span: Span<'i>,
    pub kind: TokenType<'i>,
}


#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType<'i> {
    // Ops
    Equals,
    Assign,
    Plus,
    RightShift,
    // Symbols
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    SemiColon,
    // Built-in types
    Void,
    Int,
    // Keywords
    Return,
    If,
    // Values
    IntLiteral(i64),
    Ident(&'i str),
}
impl<'i> TokenType<'i> {    
    pub fn is_equals(&self) -> bool {
        match self {
            TokenType::Equals => true,
            _ => false,
        }
    }
    pub fn is_assign(&self) -> bool {
        match self {
            TokenType::Assign => true,
            _ => false,
        }
    }
    pub fn is_plus(&self) -> bool {
        match self {
            TokenType::Plus => true,
            _ => false,
        }
    }
    pub fn is_right_shift(&self) -> bool {
        match self {
            TokenType::RightShift => true,
            _ => false,
        }
    }
    pub fn is_open_paren(&self) -> bool {
        match self {
            TokenType::OpenParen => true,
            _ => false,
        }
    }
    pub fn is_close_paren(&self) -> bool {
        match self {
            TokenType::CloseParen => true,
            _ => false,
        }
    }
    pub fn is_open_brace(&self) -> bool {
        match self {
            TokenType::OpenBrace => true,
            _ => false,
        }
    }
    pub fn is_close_brace(&self) -> bool {
        match self {
            TokenType::CloseBrace => true,
            _ => false,
        }
    }
    pub fn is_semi_colon(&self) -> bool {
        match self {
            TokenType::SemiColon => true,
            _ => false,
        }
    }
    pub fn is_void(&self) -> bool {
        match self {
            TokenType::Void => true,
            _ => false,
        }
    }
    pub fn is_int(&self) -> bool {
        match self {
            TokenType::Int => true,
            _ => false,
        }
    }
    pub fn is_return(&self) -> bool {
        match self {
            TokenType::Return => true,
            _ => false,
        }
    }
    pub fn is_if(&self) -> bool {
        match self {
            TokenType::If => true,
            _ => false,
        }
    }
    pub fn is_int_literal(&self) -> bool {
        match self {
            TokenType::IntLiteral(_) => true,
            _ => false,
        }
    }
    pub fn is_ident(&self) -> bool {
        match self {
            TokenType::Ident(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        return self.is_equals()
        || self.is_assign()
        || self.is_plus()
        || self.is_right_shift()
        || self.is_open_paren()
        || self.is_close_paren()
        || self.is_open_brace()
        || self.is_close_brace()
        || self.is_semi_colon();
    }

    pub fn is_keyword(&self) -> bool {
        return self.is_if() || self.is_return();
    }

    pub fn is_ctype(&self) -> bool {
        return self.is_void() || self.is_int();
    }
}

impl<'i> core::fmt::Display for TokenType<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Equals => write!(f, "=="),
            TokenType::Assign => write!(f, "="),
            TokenType::Plus => write!(f, "+"),
            TokenType::RightShift => write!(f, ">>"),
            TokenType::OpenParen => write!(f, "("),
            TokenType::CloseParen => write!(f, ")"),
            TokenType::OpenBrace => write!(f, "{{"),
            TokenType::CloseBrace => write!(f, "}}"),
            TokenType::SemiColon => write!(f, ";"),
            TokenType::Void => write!(f, "void"),
            TokenType::Int => write!(f, "int"),
            TokenType::Return => write!(f, "return"),
            TokenType::If => write!(f, "if"),
            TokenType::IntLiteral(value) => write!(f, "{}", value),
            TokenType::Ident(ident) => write!(f, "{}", ident),
        }
    }
}

pub fn parse_token(input: Span) -> IResult<Span, Token> {
    let (input, token_type) = alt((
        map(tag("=="), |_| TokenType::Equals),
        map(char('='), |_| TokenType::Assign),
        map(char('+'), |_| TokenType::Plus),
        map(tag(">>"), |_| TokenType::RightShift),
        map(char('('), |_| TokenType::OpenParen),
        map(char(')'), |_| TokenType::CloseParen),
        map(char('{'), |_| TokenType::OpenBrace),
        map(char('}'), |_| TokenType::CloseBrace),
        map(char(';'), |_| TokenType::SemiColon),
        map(tag("void"), |_| TokenType::Void),
        map(tag("int"), |_| TokenType::Int),
        map(tag("return"), |_| TokenType::Return),
        map(tag("if"), |_| TokenType::If),
        map(parse_int_lit, |res| TokenType::IntLiteral(res)),
        map(parse_ident, |res| TokenType::Ident(res.fragment())),
    ))(input)?;
    let (input, span) = position(input)?;
    Ok((input, Token { span, kind: token_type }))
}

/* fn tokenize_line(input: Span) -> IResult<Span, Vec<Token>> {
    delimited(multispace0, parse_token, third)
} */

pub fn tokenize(mut input: Span) -> IResult<Span, Vec<Token>> {
    let mut tokens = vec![];
    while !input.is_empty() {
        let (rest, token) = preceded(multispace0, parse_token)(input)?;
        let (rest, _) = multispace0(rest)?;
        tokens.push(token);
        input = rest;
    }
    Ok((input, tokens))    
}

fn parse_int_lit(input: Span) -> IResult<Span, i64> {
    // we can have either a decimal or a hex literal
    let hex_parser = map_res(recognize(pair(tag("0x"), hex_digit1)), |val: Span| {
        i64::from_str_radix(val.fragment(), 16)
    });
    let dec_parser = map_res(recognize(pair(opt(char('-')), digit1)), |val: Span| {
        i64::from_str_radix(val.fragment(), 10)
    });
    alt((hex_parser, dec_parser))(input)
}

fn parse_ident(input: Span) -> IResult<Span, Span> {
    recognize(pair(
        alt((tag("_"), alpha1)),
        many0(alt((tag("_"), alphanumeric1))),
    ))(input)
}

// Tests module
#[cfg(test)]
mod tests {
    use nom_locate::position;

    use crate::token::{tokenize, parse_ident, TokenType};
    use super::Span;

    #[test]
    fn test_ident_parsing() {
        let inputs = [
            ("_", "_", ""),
            ("_123", "_123", ""),
            ("a123", "a123", ""),
            ("abc", "abc", ""),
            ("_abc", "_abc", ""),
            ("foo()", "foo", "()"),
            ("x;", "x", ";"),
        ];
        for (input, exp_ident, not_ident) in inputs.iter() {
            let (rem, res_ident) = parse_ident(Span::new(input)).unwrap();
            assert_eq!(res_ident.fragment(), exp_ident);
            assert_eq!(rem.fragment(), not_ident);
        }
    }

    #[test]
    fn test_invalid_ident_parsing() {
        let inputs = ["123", "123abc", "()abc"];
        for input in inputs.iter() {
            let res = parse_ident(Span::new(input));
            assert!(res.is_err());
        }
    }

    #[test]
    fn test_tokenizing() {
        let input = Span::new("void main() {}");
        let (rest, tokens) = match tokenize(input) {
            Ok((rest, tokens)) => (rest, tokens),
            Err(e) => {
                panic!("Error {e}");
            },
        };
        println!("{rest}");
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].kind, TokenType::Void);
        assert_eq!(tokens[1].kind, TokenType::Ident("main"));
        assert_eq!(tokens[2].kind, TokenType::OpenParen);
        assert_eq!(tokens[3].kind, TokenType::CloseParen);
        assert_eq!(tokens[4].kind, TokenType::OpenBrace);
        assert_eq!(tokens[5].kind, TokenType::CloseBrace);
    }

    #[test]
    #[rustfmt::skip::macros(vec)]
    fn test_tokenizing_hard() {
        /*
        void main() {
            int x;
            x = 1;
            return;
        }
        */
        let code = vec![
            TokenType::Void, TokenType::Ident("main"), TokenType::OpenParen, TokenType::CloseParen, TokenType::OpenBrace,
                TokenType::Int, TokenType::Ident("x"), TokenType::SemiColon,
                TokenType::Ident("x"), TokenType::Assign, TokenType::IntLiteral(1), TokenType::SemiColon,
                TokenType::Return, TokenType::SemiColon,
            TokenType::CloseBrace,
        ];
        let text_code: String = code.iter().map(|token| token.to_string()).collect::<Vec<_>>().join(" ");
        let (rest, tokens) = tokenize(Span::new(&text_code)).unwrap();
        let rest = rest.fragment();
        assert!(rest.is_empty());
        assert_eq!(tokens.len(), code.len());
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token.kind, code[i]);
        }
    }

    #[test]
    #[rustfmt::skip::macros(vec)]
    fn test_tokenizing_if() {
        let text_code = r#"
        void main() {
            int x = 2;
            if(x == 2) {
                x = 3;
            }
            return;
        }
        "#;
        let code = vec![
            TokenType::Void, TokenType::Ident("main"), TokenType::OpenParen, TokenType::CloseParen, TokenType::OpenBrace,
                TokenType::Int, TokenType::Ident("x"), TokenType::Assign, TokenType::IntLiteral(2), TokenType::SemiColon,
                TokenType::If, TokenType::OpenParen, TokenType::Ident("x"), TokenType::Equals, TokenType::IntLiteral(2), TokenType::CloseParen, TokenType::OpenBrace, 
                    TokenType::Ident("x"), TokenType::Assign, TokenType::IntLiteral(3), TokenType::SemiColon, 
                TokenType::CloseBrace, 
                TokenType::Return, TokenType::SemiColon, 
            TokenType::CloseBrace,
        ];
        let text_code: String = code.iter().map(|token| token.to_string()).collect::<Vec<_>>().join(" ");
        let (rest, tokens) = tokenize(Span::new(&text_code)).unwrap();
        assert!(rest.is_empty());
        assert_eq!(tokens.len(), code.len());
        // assert_eq!(tokens, code);
    }
}
