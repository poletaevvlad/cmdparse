use super::{CompletionResult, Parsable, ParseResult, Parser};
use crate::error::{ParseError, UnrecognizedToken};
use crate::tokens::{TokenStream, TokenValue};
use crate::utils::complete_variants;
use std::borrow::{Borrow, Cow};
use std::collections::HashSet;
use std::fmt;
use std::marker::PhantomData;
use std::num::{IntErrorKind, ParseFloatError, ParseIntError};
use std::str::FromStr;

fn complete_token_single(input: TokenStream<'_>) -> CompletionResult<'_> {
    match input.take() {
        Some(Ok((token, remaining))) => match token.value() {
            TokenValue::Text(_) if token.is_last() => CompletionResult::complete(HashSet::new()),
            TokenValue::Text(_) => CompletionResult::consumed(remaining),
            TokenValue::Attribute(_) => CompletionResult::unrecognized(input),
        },
        Some(Err(_)) | None => CompletionResult::failed(),
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

    fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        let (token, remaining) = input
            .take()
            .transpose()?
            .ok_or_else(|| ParseError::token_required().expected("integer"))?;
        match token.value() {
            TokenValue::Text(text) => match text.parse_string().parse() {
                Ok(value) => Ok((value, remaining)),
                Err(error) => {
                    let message = match error.kind() {
                        IntErrorKind::PosOverflow => Some("too large"),
                        IntErrorKind::NegOverflow => Some("too small"),
                        _ => None,
                    };
                    Err(ParseError::invalid(token, message.map(Cow::Borrowed))
                        .expected("integer")
                        .into())
                }
            },
            TokenValue::Attribute(_) => Err(UnrecognizedToken::new(token, remaining).into()),
        }
    }

    fn complete<'a>(&self, input: TokenStream<'a>) -> CompletionResult<'a> {
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

    fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        let (token, remaining) = input
            .take()
            .transpose()?
            .ok_or_else(|| ParseError::token_required().expected("real number"))?;
        match token.value() {
            TokenValue::Text(text) => match text.parse_string().parse() {
                Ok(value) => Ok((value, remaining)),
                Err(_) => Err(ParseError::invalid(token, None)
                    .expected("real number")
                    .into()),
            },
            TokenValue::Attribute(_) => Err(UnrecognizedToken::new(token, remaining).into()),
        }
    }

    fn complete<'a>(&self, input: TokenStream<'a>) -> CompletionResult<'a> {
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

    fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        let (token, remaining) = input
            .take()
            .transpose()?
            .ok_or_else(|| ParseError::token_required().expected("string"))?;
        match token.value() {
            TokenValue::Text(text) => Ok((ToString::to_string(&text.parse_string()), remaining)),
            TokenValue::Attribute(_) => Err(UnrecognizedToken::new(token, remaining).into()),
        }
    }

    fn complete<'a>(&self, input: TokenStream<'a>) -> CompletionResult<'a> {
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

    fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        let (token, remaining) = input
            .take()
            .transpose()?
            .ok_or_else(|| ParseError::token_required().expected("boolean"))?;
        match token.value() {
            TokenValue::Text(text) => match text.parse_string().borrow() {
                "true" | "t" | "yes" | "y" => Ok((true, remaining)),
                "false" | "f" | "no" | "n" => Ok((false, remaining)),
                _ => Err(ParseError::invalid(token, None).expected("boolean").into()),
            },
            TokenValue::Attribute(_) => Err(UnrecognizedToken::new(token, remaining).into()),
        }
    }

    fn complete<'a>(&self, input: TokenStream<'a>) -> CompletionResult<'a> {
        match input.take() {
            Some(Ok((token, remaining))) => match token.value() {
                TokenValue::Text(text) if token.is_last() => {
                    let text = text.parse_string();
                    CompletionResult::complete(
                        complete_variants(&text, &["false", "no", "true", "yes"])
                            .map(Cow::Borrowed)
                            .collect(),
                    )
                }
                TokenValue::Text(_) => CompletionResult::consumed(remaining),
                TokenValue::Attribute(_) => CompletionResult::unrecognized(input),
            },
            Some(Err(_)) | None => CompletionResult::failed(),
        }
    }
}

impl<Ctx> Parsable<Ctx> for bool {
    type Parser = BooleanParser;
}

#[cfg(test)]
mod tests {
    use super::{IntegerParser, RealParser};
    use crate::error::{ParseError, ParseFailure};
    use crate::primitives::BooleanParser;
    use crate::tokens::{token_macro::token, TokenStream};
    use crate::{Parsable, Parser};
    use std::{borrow::Cow, collections::HashSet};

    macro_rules! test_parse {
        ($name:ident, $type:ty, $text:literal => Ok($value:expr)) => {
            #[test]
            #[allow(clippy::bool_assert_comparison)]
            fn $name() {
                let parser = <$type as Parsable<()>>::new_parser(());

                let stream = TokenStream::new($text);
                let (result, remaining) = Parser::<()>::parse(&parser, stream).unwrap();
                assert_eq!(result, $value);
                assert!(remaining.peek().is_none());

                let mut input = $text.to_string();
                input.push_str(" abc");
                let stream = TokenStream::new(&input);
                let (result, remaining) = Parser::<()>::parse(&parser, stream).unwrap();
                assert_eq!(result, $value);
                assert_eq!(remaining.peek().unwrap().unwrap(), token!("abc", last));
            }
        };
        ($name:ident, $type:ty, $text:literal => Err($err:expr)) => {
            #[test]
            fn $name() {
                let parser = <$type as Parsable<()>>::new_parser(());
                let stream = TokenStream::new($text);
                let failure = Parser::<()>::parse(&parser, stream).unwrap_err();
                match failure {
                    ParseFailure::Error(error) => assert_eq!(error, $err),
                    ParseFailure::Unrecognized(_) => panic!("expected Error, got {:?}", failure),
                }
            }
        };
    }

    macro_rules! test_unrecognized_attribute {
        ($name:ident, $type:ty) => {
            #[test]
            fn $name() {
                let parser = <$type as Parsable<()>>::new_parser(());
                let stream = TokenStream::new("--unrecognized abc");
                let failure = Parser::<()>::parse(&parser, stream).unwrap_err();
                match failure {
                    ParseFailure::Error(_) => panic!("expected Unrecognized, got {:?}", failure),
                    ParseFailure::Unrecognized(unrecognized) => {
                        assert_eq!(unrecognized.token(), token!(--"unrecognized"));
                        assert_eq!(
                            unrecognized.remaining().peek().unwrap().unwrap(),
                            token!("abc", last)
                        );
                    }
                }
            }
        };
    }

    mod integer_parser {
        use super::*;

        #[test]
        fn debug() {
            assert_eq!(
                &format!("{:?}", IntegerParser::<i8>::default()),
                "IntegerParser"
            );
        }

        test_parse!(parse_u8, u8, "15" => Ok(15));
        test_unrecognized_attribute!(unrecognized_attr, i32);
        test_parse!(parse_invalid, u16, "abc" => Err(ParseError::invalid(token!("abc", last), None).expected("integer")));
        test_parse!(parse_too_large, u16, "999999999" => Err(ParseError::invalid(token!("999999999", last), Some("too large".into())).expected("integer")));
        test_parse!(parse_empty_string, u16, "" => Err(ParseError::token_required().expected("integer")));

        #[test]
        fn suggest_empty_string() {
            let parser = IntegerParser::<i16>::create(());
            let stream = TokenStream::new("");
            let result = Parser::<()>::complete(&parser, stream);
            assert!(!result.value_consumed);
            assert!(result.suggestions.is_empty());
            assert!(result.remaining.is_none());
        }

        #[test]
        fn suggest_attribute() {
            let parser = IntegerParser::<i16>::create(());
            let stream = TokenStream::new("--unknown abc");
            let result = Parser::<()>::complete(&parser, stream);
            assert!(!result.value_consumed);
            assert!(result.suggestions.is_empty());
            assert_eq!(
                result.remaining.unwrap().peek().unwrap().unwrap(),
                token!(--"unknown")
            );
        }

        #[test]
        fn suggest_last_token() {
            let parser = IntegerParser::<i16>::create(());
            let stream = TokenStream::new("abc");
            let result = Parser::<()>::complete(&parser, stream);
            assert!(result.value_consumed);
            assert!(result.suggestions.is_empty());
            assert!(result.remaining.is_none());
        }

        #[test]
        fn suggest_with_space() {
            let parser = IntegerParser::<i16>::create(());
            let stream = TokenStream::new("abc ");
            let result = Parser::<()>::complete(&parser, stream);
            assert!(result.value_consumed);
            assert!(result.suggestions.is_empty());
            assert!(result.remaining.is_some());
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
            let parser = <f64 as Parsable<()>>::new_parser(());

            let stream = TokenStream::new("3.2");
            let (result, remaining) = Parser::<()>::parse(&parser, stream).unwrap();
            assert!((result - 3.2).abs() < f64::EPSILON);
            assert!(remaining.peek().is_none());

            let stream = TokenStream::new("3.2 abc");
            let (result, remaining) = Parser::<()>::parse(&parser, stream).unwrap();
            assert!((result - 3.2).abs() < f64::EPSILON);
            assert_eq!(remaining.peek().unwrap().unwrap(), token!("abc", last));
        }

        test_parse!(parse_error, f64, "abc" => Err(ParseError::invalid(token!("abc", last), None).expected("real number")));
        test_parse!(parse_empty, f64, "" => Err(ParseError::token_required().expected("real number")));
        test_unrecognized_attribute!(unrecognized_attr, f32);
    }

    mod string_parser {
        use super::*;

        test_parse!(parse_string, String, "abc" => Ok("abc".to_string()));
        test_parse!(parse_empty, String, "" => Err(ParseError::token_required().expected("string")));
        test_unrecognized_attribute!(unrecognized_attr, String);
    }

    mod boolean_parser {
        use super::*;

        test_parse!(parse_bool_true, bool, "true" => Ok(true));
        test_parse!(parse_bool_yes, bool, "yes" => Ok(true));
        test_parse!(parse_bool_false, bool, "false" => Ok(false));
        test_parse!(parse_bool_no, bool, "no" => Ok(false));
        test_unrecognized_attribute!(unrecognized_attr, bool);
        test_parse!(parse_empty_string, bool, "" => Err(ParseError::token_required().expected("boolean")));
        test_parse!(parse_invalie, bool, "abc" => Err(ParseError::invalid(token!("abc", last), None).expected("boolean")));

        #[test]
        fn suggest_empty_string() {
            let parser = BooleanParser::create(());
            let stream = TokenStream::new("");
            let result = Parser::<()>::complete(&parser, stream);
            assert!(!result.value_consumed);
            assert!(result.suggestions.is_empty());
            assert!(result.remaining.is_none());
        }

        #[test]
        fn suggest_attribute() {
            let parser = BooleanParser::create(());
            let stream = TokenStream::new("--unknown abc");
            let result = Parser::<()>::complete(&parser, stream);
            assert!(!result.value_consumed);
            assert!(result.suggestions.is_empty());
            assert_eq!(
                result.remaining.unwrap().peek().unwrap().unwrap(),
                token!(--"unknown")
            );
        }

        #[test]
        fn suggest_last_token() {
            let parser = BooleanParser::create(());
            let cases = vec![
                ("t", vec!["rue"]),
                ("f", vec!["alse"]),
                ("y", vec!["es"]),
                ("n", vec!["o"]),
                ("m", vec![]),
            ];

            for (input, expected) in cases {
                let stream = TokenStream::new(input);
                let result = Parser::<()>::complete(&parser, stream);
                assert!(result.value_consumed);
                assert!(result.remaining.is_none());
                assert_eq!(
                    result.suggestions,
                    HashSet::from_iter(expected.iter().copied().map(Cow::Borrowed))
                );
            }
        }

        #[test]
        fn suggest_with_space() {
            let parser = IntegerParser::<i16>::create(());
            let stream = TokenStream::new("tr ");
            let result = Parser::<()>::complete(&parser, stream);
            assert!(result.value_consumed);
            assert!(result.suggestions.is_empty());
            assert!(result.remaining.is_some());
        }
    }
}
