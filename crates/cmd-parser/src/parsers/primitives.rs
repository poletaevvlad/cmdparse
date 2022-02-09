use crate::error::{ParseError, UnrecognizedToken};
use crate::tokens::{TokenStream, TokenValue};
use crate::utils::complete_variants;
use crate::{CompletionResult, Parsable, ParseResult, Parser};
use std::borrow::{Borrow, Cow};
use std::fmt;
use std::marker::PhantomData;
use std::num::{IntErrorKind, ParseIntError};
use std::str::FromStr;

fn complete_token_single(input: TokenStream<'_>) -> CompletionResult<'_> {
    match input.take() {
        Some(Ok((token, remaining))) => match token.value() {
            TokenValue::Text(_) if token.is_last() => CompletionResult::new_final(true),
            TokenValue::Text(_) => CompletionResult::new(remaining, true),
            TokenValue::Attribute(_) => CompletionResult::new(input, false),
        },
        Some(Err(_)) | None => CompletionResult::new_final(false),
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

no_state_parsable!(std::num::NonZeroI8, IntegerParser);
no_state_parsable!(std::num::NonZeroU8, IntegerParser);
no_state_parsable!(std::num::NonZeroI16, IntegerParser);
no_state_parsable!(std::num::NonZeroU16, IntegerParser);
no_state_parsable!(std::num::NonZeroI32, IntegerParser);
no_state_parsable!(std::num::NonZeroU32, IntegerParser);
no_state_parsable!(std::num::NonZeroI64, IntegerParser);
no_state_parsable!(std::num::NonZeroU64, IntegerParser);
no_state_parsable!(std::num::NonZeroI128, IntegerParser);
no_state_parsable!(std::num::NonZeroU128, IntegerParser);
no_state_parsable!(std::num::NonZeroIsize, IntegerParser);
no_state_parsable!(std::num::NonZeroUsize, IntegerParser);

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
                        IntErrorKind::Zero => Some("cannot be zero"),
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

no_state_parser!(FromStrParser);
no_state_parsable!(f32, FromStrParser);
no_state_parsable!(f64, FromStrParser);
no_state_parsable!(std::net::Ipv4Addr, FromStrParser);
no_state_parsable!(std::net::Ipv6Addr, FromStrParser);
no_state_parsable!(std::net::IpAddr, FromStrParser);
no_state_parsable!(std::net::SocketAddrV4, FromStrParser);
no_state_parsable!(std::net::SocketAddrV6, FromStrParser);
no_state_parsable!(std::net::SocketAddr, FromStrParser);

impl<T: FromStr, Ctx> Parser<Ctx> for FromStrParser<T> {
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
                    CompletionResult::new_final(true).add_suggestions(
                        complete_variants(&text, &["false", "no", "true", "yes"])
                            .map(Cow::Borrowed),
                    )
                }
                TokenValue::Text(_) => CompletionResult::new(remaining, true),
                TokenValue::Attribute(_) => CompletionResult::new(input, false),
            },
            Some(Err(_)) | None => CompletionResult::new_final(false),
        }
    }
}

impl<Ctx> Parsable<Ctx> for bool {
    type Parser = BooleanParser;
}

#[cfg(test)]
mod tests {
    use super::{FromStrParser, IntegerParser};
    use crate::error::{ParseError, ParseFailure};
    use crate::testing::{test_complete, token};
    use crate::tokens::TokenStream;
    use crate::{Parsable, Parser};

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
        test_parse!(
            parse_non_zero_u8_zero, std::num::NonZeroU8,
            "0" => Err(ParseError::invalid(token!("0", last), Some("cannot be zero".into())).expected("integer"))
        );
        test_parse!(parse_non_zero_u8_non_zero, std::num::NonZeroU8, "5" => Ok(std::num::NonZeroU8::new(5).unwrap()));
        test_unrecognized_attribute!(unrecognized_attr, i32);
        test_parse!(parse_invalid, u16, "abc" => Err(ParseError::invalid(token!("abc", last), None).expected("integer")));
        test_parse!(parse_too_large, u16, "999999999" => Err(ParseError::invalid(token!("999999999", last), Some("too large".into())).expected("integer")));
        test_parse!(parse_empty_string, u16, "" => Err(ParseError::token_required().expected("integer")));

        test_complete!(complete_empty_string, i16, "" => {
            consumed: false,
            remaining: None,
            suggestions: [],
        });
        test_complete!(complete_attribute, i16, "--unknown" => {
            consumed: false,
            remaining: Some(Some(token!(--"unknown", last))),
            suggestions: [],
        });
        test_complete!(complete_last_token, i16, "abc" => {
            consumed: true,
            remaining: None,
            suggestions: [],
        });
        test_complete!(complete_with_space, i16, "abc " => {
            consumed: true,
            remaining: Some(None),
            suggestions: [],
        });
    }

    mod from_str_parser {
        use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};

        use super::*;

        #[test]
        fn debug() {
            assert_eq!(
                &format!("{:?}", FromStrParser::<f64>::default()),
                "FromStrParser"
            );
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

        test_parse!(parse_f64_error, f64, "abc" => Err(ParseError::invalid(token!("abc", last), None).expected("real number")));
        test_parse!(parse_f64_empty, f64, "" => Err(ParseError::token_required().expected("real number")));
        test_unrecognized_attribute!(unrecognized_attr, f32);

        test_parse!(parse_ipv4, Ipv4Addr, "127.0.0.1" => Ok(Ipv4Addr::new(127, 0, 0, 1)));
        test_parse!(parse_ipv4_generic, IpAddr, "127.0.0.1" => Ok(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1))));
        test_parse!(parse_ipv6, Ipv6Addr, "::1" => Ok(Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 1)));
        test_parse!(parse_ipv6_generic, IpAddr, "::1" => Ok(IpAddr::V6(Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 1))));
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

        test_complete!(complete_empty_string, bool, "" => {
            consumed: false,
            remaining: None,
            suggestions: [],
        });

        test_complete!(complete_atribute, bool, "--unknown abc" => {
            consumed: false,
            remaining: Some(Some(token!(--"unknown"))),
            suggestions: [],
        });

        test_complete!(complete_f, bool, "f" => {consumed: true, remaining: None, suggestions: ["alse"]});
        test_complete!(complete_t, bool, "t" => {consumed: true, remaining: None, suggestions: ["rue"]});
        test_complete!(complete_y, bool, "y" => {consumed: true, remaining: None, suggestions: ["es"]});
        test_complete!(complete_n, bool, "n" => {consumed: true, remaining: None, suggestions: ["o"]});
        test_complete!(complete_m, bool, "m" => {consumed: true, remaining: None, suggestions: []});

        test_complete!(complete_consumed, bool, "fal " => {
            consumed: true,
            remaining: Some(None),
            suggestions: [],
        });
    }
}
