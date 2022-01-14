use super::{CompletionResult, Parser};
use crate::utils::take_token;
use crate::{ParseError, ParseResult};
use std::borrow::Cow;
use std::fmt;
use std::marker::PhantomData;
use std::num::{IntErrorKind, ParseIntError};
use std::str::FromStr;

#[derive(Clone, Copy)]
pub struct IntegerParser<T> {
    _phantom: PhantomData<T>,
}

impl<T> fmt::Debug for IntegerParser<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IntegerParser").finish()
    }
}

impl<T> Default for IntegerParser<T> {
    fn default() -> Self {
        Self {
            _phantom: Default::default(),
        }
    }
}

impl<T, Ctx> Parser<Ctx> for IntegerParser<T>
where
    T: FromStr<Err = ParseIntError>,
{
    type Value = T;

    fn create(_ctx: Ctx) -> Self {
        IntegerParser::default()
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

    fn complete<'a>(&self, _input: &'a str) -> CompletionResult<'a> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::IntegerParser;
    use crate::{ParseError, ParseResult, Parser};

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
    }
}
