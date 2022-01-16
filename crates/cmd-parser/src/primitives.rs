use super::{CompletionResult, Parsable, ParseError, ParseResult, Parser};
use crate::tokens::{complete_enum, has_tokens, skip_token_no_ws, skip_ws, take_token, Token};
use std::borrow::{Borrow, Cow};
use std::fmt;
use std::marker::PhantomData;
use std::num::{IntErrorKind, ParseFloatError, ParseIntError};
use std::str::FromStr;

fn complete_token_single(input: &str) -> CompletionResult<'_> {
    if !has_tokens(input) {
        return CompletionResult::empty();
    }
    if input.starts_with("--") {
        return CompletionResult::Unrecognized;
    }
    let remaining = skip_token_no_ws(input);
    match remaining.chars().next() {
        Some(ch) if ch.is_whitespace() => CompletionResult::Consumed(skip_ws(remaining)),
        _ => CompletionResult::empty(),
    }
}

macro_rules! no_state_parser {
    ($name:ident) => {
        #[derive(Clone, Copy)]
        pub struct $name<T> {
            _phantom: PhantomData<T>,
        }

        impl<T> fmt::Debug for $name<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(stringify!($name)).finish()
            }
        }

        impl<T> Default for $name<T> {
            fn default() -> Self {
                $name {
                    _phantom: Default::default(),
                }
            }
        }
    };
}

macro_rules! no_state_parsable {
    ($type:ty, $parser:ident) => {
        impl<Ctx> Parsable<Ctx> for $type {
            type Parser = $parser<$type>;
        }
    };
}

no_state_parser!(IntegerParser);
no_state_parsable!(i8, IntegerParser);
no_state_parsable!(u8, IntegerParser);
no_state_parsable!(i16, IntegerParser);
no_state_parsable!(u16, IntegerParser);
no_state_parsable!(i32, IntegerParser);
no_state_parsable!(u32, IntegerParser);
no_state_parsable!(i64, IntegerParser);
no_state_parsable!(u64, IntegerParser);
no_state_parsable!(i128, IntegerParser);
no_state_parsable!(u128, IntegerParser);
no_state_parsable!(isize, IntegerParser);
no_state_parsable!(usize, IntegerParser);

impl<T, Ctx> Parser<Ctx> for IntegerParser<T>
where
    T: FromStr<Err = ParseIntError>,
{
    type Value = T;

    fn create(_ctx: Ctx) -> Self {
        Self::default()
    }

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
        let (token, remaining) = take_token(input);
        match token {
            Token::Attribute(attribute) => {
                ParseResult::Unrecognized(ParseError::unknown_attribute(attribute))
            }
            Token::Text(token) if token.is_empty() => ParseError::token_required("integer").into(),
            Token::Text(token) => match token.parse() {
                Ok(value) => ParseResult::Parsed(value, remaining),
                Err(error) => {
                    let error_label: Option<Cow<'static, str>> = match error.kind() {
                        IntErrorKind::PosOverflow => Some("too large".into()),
                        IntErrorKind::NegOverflow => Some("too small".into()),
                        _ => None,
                    };
                    ParseError::token_parse(token, error_label, "integer").into()
                }
            },
        }
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        complete_token_single(input)
    }
}

no_state_parser!(RealParser);
no_state_parsable!(f32, RealParser);
no_state_parsable!(f64, RealParser);

impl<T, Ctx> Parser<Ctx> for RealParser<T>
where
    T: FromStr<Err = ParseFloatError>,
{
    type Value = T;

    fn create(_ctx: Ctx) -> Self {
        Self::default()
    }

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
        let (token, remaining) = take_token(input);
        match token {
            Token::Attribute(attribute) => {
                ParseResult::Unrecognized(ParseError::unknown_attribute(attribute))
            }
            Token::Text(token) if token.is_empty() => {
                ParseError::token_required("real number").into()
            }
            Token::Text(token) => match token.parse() {
                Ok(value) => ParseResult::Parsed(value, remaining),
                Err(_) => ParseResult::Failed(ParseError::token_parse(token, None, "real number")),
            },
        }
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        complete_token_single(input)
    }
}

#[derive(Debug, Default)]
pub struct StringParser;

impl<Ctx> Parser<Ctx> for StringParser {
    type Value = String;

    fn create(_ctx: Ctx) -> Self {
        StringParser
    }

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
        let (token, remaining) = take_token(input);
        match token {
            Token::Attribute(attribute) => {
                ParseResult::Unrecognized(ParseError::unknown_attribute(attribute))
            }
            Token::Text(token) if token.is_empty() => ParseError::token_required("string").into(),
            Token::Text(token) => ParseResult::Parsed(token.into_owned(), remaining),
        }
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        complete_token_single(input)
    }
}

impl<Ctx> Parsable<Ctx> for String {
    type Parser = StringParser;
}

#[derive(Debug, Default)]
pub struct BooleanParser;

impl<Ctx> Parser<Ctx> for BooleanParser {
    type Value = bool;

    fn create(_ctx: Ctx) -> Self {
        BooleanParser
    }

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
        let (token, remaining) = take_token(input);
        match token {
            Token::Attribute(attribute) => {
                ParseResult::Unrecognized(ParseError::unknown_attribute(attribute))
            }
            Token::Text(token) if token.is_empty() => ParseError::token_required("boolean").into(),
            Token::Text(token) => match token.borrow() {
                "true" | "t" | "yes" | "y" => ParseResult::Parsed(true, remaining),
                "false" | "f" | "no" | "n" => ParseResult::Parsed(false, remaining),
                _ => ParseResult::Failed(ParseError::token_parse(token, None, "boolean")),
            },
        }
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        complete_enum(input, &["false", "no", "true", "yes"])
    }
}

impl<Ctx> Parsable<Ctx> for bool {
    type Parser = BooleanParser;
}

#[cfg(test)]
mod tests {
    use super::{BooleanParser, IntegerParser, RealParser, StringParser};
    use crate::{CompletionResult, ParseError, ParseResult, Parser};

    mod integer_parser {
        use super::*;

        #[test]
        fn debug() {
            assert_eq!(
                &format!("{:?}", IntegerParser::<i8>::default()),
                "IntegerParser"
            );
        }

        #[test]
        fn parse_u8() {
            let parser = IntegerParser::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "15 abc"),
                ParseResult::Parsed(15, "abc")
            );
        }

        #[test]
        fn parse_unknown_attribure() {
            let parser = IntegerParser::<u16>::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "--unknown abc"),
                ParseResult::Unrecognized(ParseError::unknown_attribute("unknown"))
            );
        }

        #[test]
        fn parse_error() {
            let parser = IntegerParser::<i16>::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "123456781234567 abc"),
                ParseResult::Failed(ParseError::token_parse(
                    "123456781234567".into(),
                    Some("too large".into()),
                    "integer"
                ))
            );
        }

        #[test]
        fn parse_error_no_description() {
            let parser = IntegerParser::<i16>::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "abc"),
                ParseResult::Failed(ParseError::token_parse("abc".into(), None, "integer"))
            );
        }

        #[test]
        fn parse_error_empty_string() {
            let parser = IntegerParser::<i16>::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "  "),
                ParseResult::Failed(ParseError::token_required("integer"))
            );
        }

        #[test]
        fn suggest_empty_string() {
            let parser = IntegerParser::<i16>::create(());
            assert_eq!(
                Parser::<()>::complete(&parser, ""),
                CompletionResult::empty(),
            );
        }

        #[test]
        fn suggest_attribute() {
            let parser = IntegerParser::<i16>::create(());
            assert_eq!(
                Parser::<()>::complete(&parser, "--attribute"),
                CompletionResult::Unrecognized,
            );
        }

        #[test]
        fn suggest_last_token() {
            let parser = IntegerParser::<i16>::create(());
            assert_eq!(
                Parser::<()>::complete(&parser, "123"),
                CompletionResult::empty(),
            );
        }

        #[test]
        fn suggest_followed_by_space() {
            let parser = IntegerParser::<i16>::create(());
            assert_eq!(
                Parser::<()>::complete(&parser, "123 456"),
                CompletionResult::Consumed("456"),
            );
        }
    }

    mod real_parser {
        use super::*;

        #[test]
        fn debug() {
            assert_eq!(&format!("{:?}", RealParser::<f64>::default()), "RealParser");
        }

        #[test]
        fn parse_f64() {
            let parser = RealParser::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "3.2 abc"),
                ParseResult::Parsed(3.2, "abc")
            );
        }

        #[test]
        fn parse_error() {
            let parser = RealParser::<f64>::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "abc"),
                ParseResult::Failed(ParseError::token_parse("abc".into(), None, "real number"))
            );
        }

        #[test]
        fn parse_unknown_attribure() {
            let parser = RealParser::<f64>::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "--unknown abc"),
                ParseResult::Unrecognized(ParseError::unknown_attribute("unknown"))
            );
        }

        #[test]
        fn parse_error_empty_string() {
            let parser = RealParser::<f64>::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, ""),
                ParseResult::Failed(ParseError::token_required("real number"))
            );
        }
    }

    mod string_parser {
        use super::*;

        #[test]
        fn parse_string() {
            let parser = StringParser::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "abc def"),
                ParseResult::Parsed("abc".to_string(), "def")
            );
        }

        #[test]
        fn parse_empty() {
            let parser = StringParser::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, ""),
                ParseResult::Failed(ParseError::token_required("string")),
            );
        }

        #[test]
        fn parse_unknown_attribure() {
            let parser = StringParser::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "--unknown abc"),
                ParseResult::Unrecognized(ParseError::unknown_attribute("unknown"))
            );
        }
    }

    mod boolean_parser {
        use super::*;

        #[test]
        fn parse_success() {
            let parser = BooleanParser::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "true 1"),
                ParseResult::Parsed(true, "1")
            );
            assert_eq!(
                Parser::<()>::parse(&parser, "t 1"),
                ParseResult::Parsed(true, "1")
            );
            assert_eq!(
                Parser::<()>::parse(&parser, "yes 1"),
                ParseResult::Parsed(true, "1")
            );
            assert_eq!(
                Parser::<()>::parse(&parser, "y 1"),
                ParseResult::Parsed(true, "1")
            );
            assert_eq!(
                Parser::<()>::parse(&parser, "false 1"),
                ParseResult::Parsed(false, "1")
            );
            assert_eq!(
                Parser::<()>::parse(&parser, "f 1"),
                ParseResult::Parsed(false, "1")
            );
            assert_eq!(
                Parser::<()>::parse(&parser, "no 1"),
                ParseResult::Parsed(false, "1")
            );
            assert_eq!(
                Parser::<()>::parse(&parser, "n 1"),
                ParseResult::Parsed(false, "1")
            );
        }

        #[test]
        fn parse_unknown() {
            let parser = BooleanParser::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "unknown"),
                ParseResult::Failed(ParseError::token_parse("unknown".into(), None, "boolean"))
            );
        }

        #[test]
        fn parse_empty() {
            let parser = BooleanParser::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, ""),
                ParseResult::Failed(ParseError::token_required("boolean"))
            );
        }

        #[test]
        fn parse_unknown_attribure() {
            let parser = BooleanParser::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, "--unknown abc"),
                ParseResult::Unrecognized(ParseError::unknown_attribute("unknown"))
            );
        }

        #[test]
        fn suggestions() {
            let parser = BooleanParser::create(());
            assert_eq!(
                Parser::<()>::complete(&parser, "t"),
                CompletionResult::Suggestions(vec!["rue".into()])
            );
            assert_eq!(
                Parser::<()>::complete(&parser, "f"),
                CompletionResult::Suggestions(vec!["alse".into()])
            );
            assert_eq!(
                Parser::<()>::complete(&parser, "y"),
                CompletionResult::Suggestions(vec!["es".into()])
            );
            assert_eq!(
                Parser::<()>::complete(&parser, "n"),
                CompletionResult::Suggestions(vec!["o".into()])
            );
            assert_eq!(
                Parser::<()>::complete(&parser, "m"),
                CompletionResult::empty()
            );
            assert_eq!(
                Parser::<()>::complete(&parser, "false next"),
                CompletionResult::Consumed("next")
            );
        }
    }
}
