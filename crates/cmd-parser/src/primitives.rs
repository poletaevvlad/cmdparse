use crate::error::{ParseError, UnrecognizedToken};
use crate::tokens::{TokenStream, TokenValue};

use super::{CompletionResult, Parsable, ParseResult, Parser};
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt;
use std::marker::PhantomData;
use std::num::{IntErrorKind, ParseIntError};
use std::str::FromStr;

fn complete_token_single(input: TokenStream<'_>) -> CompletionResult<'_> {
    match input.take() {
        Some(Ok((token, remaining))) => match token.value() {
            TokenValue::Text(_) => CompletionResult {
                remaining: if token.is_last() {
                    None
                } else {
                    Some(remaining)
                },
                value_consumed: true,
                suggestions: HashSet::new(),
            },
            TokenValue::Attribute(_) => CompletionResult {
                remaining: Some(input),
                value_consumed: false,
                suggestions: HashSet::new(),
            },
        },
        Some(Err(_)) | None => CompletionResult {
            remaining: None,
            value_consumed: false,
            suggestions: HashSet::new(),
        },
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
        match input.take().transpose()? {
            Some((token, remaining)) => match token.value() {
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
            },
            None => Err(ParseError::token_required().expected("integer").into()),
        }
    }

    fn complete<'a>(&self, input: TokenStream<'a>) -> CompletionResult<'a> {
        complete_token_single(input)
    }
}

// no_state_parser!(RealParser);
// no_state_parsable!(f32, RealParser);
// no_state_parsable!(f64, RealParser);

// impl<T, Ctx> Parser<Ctx> for RealParser<T>
// where
//     T: FromStr<Err = ParseFloatError>,
// {
//     type Value = T;

//     fn create(_ctx: Ctx) -> Self {
//         Self::default()
//     }

//     fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
//         let (token, remaining) = take_token(input);
//         match token {
//             Token::Attribute(attribute) => ParseResult::UnrecognizedAttribute(attribute, remaining),
//             Token::Text(token) if token.is_empty() => {
//                 ParseResult::Failed(ParseError::token_required("real number"))
//             }
//             Token::Text(token) => match token.parse() {
//                 Ok(value) => ParseResult::Parsed(value, remaining),
//                 Err(_) => ParseResult::Failed(ParseError::token_parse(token, None, "real number")),
//             },
//         }
//     }

//     fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
//         complete_token_single(input)
//     }
// }

// #[derive(Debug, Default)]
// pub struct StringParser;

// impl<Ctx> Parser<Ctx> for StringParser {
//     type Value = String;

//     fn create(_ctx: Ctx) -> Self {
//         StringParser
//     }

//     fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
//         let (token, remaining) = take_token(input);
//         match token {
//             Token::Attribute(attribute) => ParseResult::UnrecognizedAttribute(attribute, remaining),
//             Token::Text(token) if token.is_empty() => {
//                 ParseResult::Failed(ParseError::token_required("string"))
//             }
//             Token::Text(token) => ParseResult::Parsed(token.into_owned(), remaining),
//         }
//     }

//     fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
//         complete_token_single(input)
//     }
// }

// impl<Ctx> Parsable<Ctx> for String {
//     type Parser = StringParser;
// }

// #[derive(Debug, Default)]
// pub struct BooleanParser;

// impl<Ctx> Parser<Ctx> for BooleanParser {
//     type Value = bool;

//     fn create(_ctx: Ctx) -> Self {
//         BooleanParser
//     }

//     fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
//         let (token, remaining) = take_token(input);
//         match token {
//             Token::Attribute(attribute) => ParseResult::UnrecognizedAttribute(attribute, remaining),
//             Token::Text(token) if token.is_empty() => {
//                 ParseResult::Failed(ParseError::token_required("boolean"))
//             }
//             Token::Text(token) => match token.borrow() {
//                 "true" | "t" | "yes" | "y" => ParseResult::Parsed(true, remaining),
//                 "false" | "f" | "no" | "n" => ParseResult::Parsed(false, remaining),
//                 _ => ParseResult::Failed(ParseError::token_parse(token, None, "boolean")),
//             },
//         }
//     }

//     fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
//         complete_enum(input, &["false", "no", "true", "yes"])
//     }
// }

// impl<Ctx> Parsable<Ctx> for bool {
//     type Parser = BooleanParser;
// }

#[cfg(test)]
mod tests {
    use super::IntegerParser;
    use crate::error::ParseFailure;
    use crate::tokens::{token_macro::token, TokenStream};
    use crate::{Parsable, Parser};

    macro_rules! test_parse {
        ($name:ident, $type:ty, $text:literal => Ok($value:expr)) => {
            #[test]
            fn $name() {
                let parser = <$type as Parsable<()>>::new_parser(());

                let stream = TokenStream::from_str($text);
                let (result, remaining) = Parser::<()>::parse(&parser, stream).unwrap();
                assert_eq!(result, $value);
                assert!(remaining.peek().is_none());

                let mut input = $text.to_string();
                input.push_str(" abc");
                let stream = TokenStream::from_str(&input);
                let (result, remaining) = Parser::<()>::parse(&parser, stream).unwrap();
                assert_eq!(result, $value);
                assert_eq!(remaining.peek().unwrap().unwrap(), token!("abc", last));
            }
        };
        ($name:ident, $type:ty, $text:literal => Err($err:expr)) => {
            #[test]
            fn $name() {
                let parser = <$type as Parsable<()>>::new_parser(());
                let stream = TokenStream::from_str($text);
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
                let stream = TokenStream::from_str("--unrecognized abc");
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
        use crate::error::ParseError;

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

    // mod real_parser {
    //     use super::*;

    //     #[test]
    //     fn debug() {
    //         assert_eq!(&format!("{:?}", RealParser::<f64>::default()), "RealParser");
    //     }

    //     #[test]
    //     fn parse_f64() {
    //         let parser = RealParser::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "3.2 abc"),
    //             ParseResult::Parsed(3.2, "abc")
    //         );
    //     }

    //     #[test]
    //     fn parse_error() {
    //         let parser = RealParser::<f64>::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "abc"),
    //             ParseResult::Failed(ParseError::token_parse("abc".into(), None, "real number"))
    //         );
    //     }

    //     #[test]
    //     fn parse_unknown_attribure() {
    //         let parser = RealParser::<f64>::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "--unknown abc"),
    //             ParseResult::UnrecognizedAttribute("unknown".into(), "abc")
    //         );
    //     }

    //     #[test]
    //     fn parse_error_empty_string() {
    //         let parser = RealParser::<f64>::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, ""),
    //             ParseResult::Failed(ParseError::token_required("real number"))
    //         );
    //     }
    // }

    // mod string_parser {
    //     use super::*;

    //     #[test]
    //     fn parse_string() {
    //         let parser = StringParser::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "abc def"),
    //             ParseResult::Parsed("abc".to_string(), "def")
    //         );
    //     }

    //     #[test]
    //     fn parse_empty() {
    //         let parser = StringParser::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, ""),
    //             ParseResult::Failed(ParseError::token_required("string")),
    //         );
    //     }

    //     #[test]
    //     fn parse_unknown_attribure() {
    //         let parser = StringParser::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "--unknown abc"),
    //             ParseResult::UnrecognizedAttribute("unknown".into(), "abc")
    //         );
    //     }
    // }

    // mod boolean_parser {
    //     use super::*;

    //     #[test]
    //     fn parse_success() {
    //         let parser = BooleanParser::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "true 1"),
    //             ParseResult::Parsed(true, "1")
    //         );
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "t 1"),
    //             ParseResult::Parsed(true, "1")
    //         );
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "yes 1"),
    //             ParseResult::Parsed(true, "1")
    //         );
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "y 1"),
    //             ParseResult::Parsed(true, "1")
    //         );
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "false 1"),
    //             ParseResult::Parsed(false, "1")
    //         );
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "f 1"),
    //             ParseResult::Parsed(false, "1")
    //         );
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "no 1"),
    //             ParseResult::Parsed(false, "1")
    //         );
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "n 1"),
    //             ParseResult::Parsed(false, "1")
    //         );
    //     }

    //     #[test]
    //     fn parse_unknown() {
    //         let parser = BooleanParser::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "unknown"),
    //             ParseResult::Failed(ParseError::token_parse("unknown".into(), None, "boolean"))
    //         );
    //     }

    //     #[test]
    //     fn parse_empty() {
    //         let parser = BooleanParser::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, ""),
    //             ParseResult::Failed(ParseError::token_required("boolean"))
    //         );
    //     }

    //     #[test]
    //     fn parse_unknown_attribure() {
    //         let parser = BooleanParser::create(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "--unknown abc"),
    //             ParseResult::UnrecognizedAttribute("unknown".into(), "abc")
    //         );
    //     }

    //     #[test]
    //     fn suggestions() {
    //         let parser = BooleanParser::create(());
    //         assert_eq!(
    //             Parser::<()>::complete(&parser, "t"),
    //             CompletionResult::Suggestions(vec!["rue".into()])
    //         );
    //         assert_eq!(
    //             Parser::<()>::complete(&parser, "f"),
    //             CompletionResult::Suggestions(vec!["alse".into()])
    //         );
    //         assert_eq!(
    //             Parser::<()>::complete(&parser, "y"),
    //             CompletionResult::Suggestions(vec!["es".into()])
    //         );
    //         assert_eq!(
    //             Parser::<()>::complete(&parser, "n"),
    //             CompletionResult::Suggestions(vec!["o".into()])
    //         );
    //         assert_eq!(
    //             Parser::<()>::complete(&parser, "m"),
    //             CompletionResult::empty()
    //         );
    //         assert_eq!(
    //             Parser::<()>::complete(&parser, "false next"),
    //             CompletionResult::Consumed("next")
    //         );
    //     }
    // }
}
