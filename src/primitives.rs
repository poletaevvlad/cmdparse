use super::{CompletionResult, Parser};
use crate::utils::{has_tokens, skip_token_no_ws, skip_ws, take_token};
use crate::{ParseError, ParseResult};
use std::borrow::Cow;
use std::fmt;
use std::marker::PhantomData;
use std::num::{IntErrorKind, ParseFloatError, ParseIntError};
use std::str::FromStr;

fn complete_token_single(input: &str) -> CompletionResult<'_> {
    if !has_tokens(input) {
        return CompletionResult::empty();
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

no_state_parser!(IntegerParser);

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
            Some(token) => match token.parse() {
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
            None => ParseError::token_required("integer").into(),
        }
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        complete_token_single(input)
    }
}

no_state_parser!(RealParser);

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
            Some(token) => match token.parse() {
                Ok(value) => ParseResult::Parsed(value, remaining),
                Err(_) => ParseResult::Failed(ParseError::token_parse(token, None, "real number")),
            },
            None => ParseError::token_required("real number").into(),
        }
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        complete_token_single(input)
    }
}

#[cfg(test)]
mod tests {
    use super::{IntegerParser, RealParser};
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
        fn parse_error_empty_string() {
            let parser = RealParser::<f64>::create(());
            assert_eq!(
                Parser::<()>::parse(&parser, ""),
                ParseResult::Failed(ParseError::token_required("real number"))
            );
        }
    }
}
