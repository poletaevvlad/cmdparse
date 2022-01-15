mod error;
mod primitives;
mod sequences;
mod utils;

pub use cmd_parser_derive::*;
pub use error::ParseError;
use std::borrow::{Borrow, Cow};
use std::ops::Deref;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::Duration;
pub use utils::*; // TODO

#[derive(Debug, PartialEq)]
pub enum ParseResult<'a, T> {
    Unrecognized(ParseError<'a>),
    Parsed(T, &'a str),
    Failed(ParseError<'a>),
}

impl<'a, T> From<ParseError<'a>> for ParseResult<'a, T> {
    fn from(error: ParseError<'a>) -> Self {
        ParseResult::Failed(error)
    }
}

impl<'a, T> ParseResult<'a, T> {
    pub fn into_parsed_or_failed(self) -> Self {
        match self {
            ParseResult::Unrecognized(error) => ParseResult::Failed(error),
            result => result,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CompletionResult<'a> {
    Consumed(&'a str),
    Suggestions(Vec<Cow<'static, str>>),
}

impl<'a> CompletionResult<'a> {
    pub(crate) const fn empty() -> Self {
        CompletionResult::Suggestions(Vec::new())
    }
}

pub trait Parser<Ctx> {
    type Value;

    fn create(ctx: Ctx) -> Self;
    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value>;
    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a>;
}

pub trait Parsable<Ctx> {
    type Parser: Parser<Ctx>;

    fn new_parser(ctx: Ctx) -> Self::Parser {
        Self::Parser::create(ctx)
    }
}

pub trait CmdParsable: Sized {
    fn parse_cmd_raw(input: &str) -> Result<(Self, &str), ParseError<'_>>;

    fn parse_cmd_full(input: &str) -> Result<Self, ParseError<'_>> {
        let (cmd, remaining) = Self::parse_cmd_raw(input)?;
        if let Some(token) = take_token(remaining).0 {
            Err(ParseError::unexpected_token(token))
        } else {
            Ok(cmd)
        }
    }
}

pub fn parse_inner<T: CmdParsable>(mut input: &str) -> Result<(T, &str), ParseError<'_>> {
    input = skip_ws(input);

    if let Some(input) = input.strip_prefix('(') {
        let (value, mut remaining) = T::parse_cmd_raw(input)?;
        if remaining.starts_with(')') {
            remaining = skip_ws(&remaining[1..]);
        } else {
            let (token, _) = take_token(remaining);
            if let Some(token) = token {
                return Err(ParseError::unexpected_token(token));
            }
        }
        Ok((value, remaining))
    } else {
        T::parse_cmd_raw(input)
    }
}

pub fn parse_cmd_token<T: FromStr>(
    input: &str,
    expected: impl Into<Cow<'static, str>>,
) -> Result<(T, &str), ParseError<'_>>
where
    T::Err: std::fmt::Display,
{
    let (token, input) = take_token(input);
    match token.as_ref().map(|token| token.deref().parse()) {
        Some(Ok(value)) => Ok((value, input)),
        Some(Err(error)) => Err(ParseError::token_parse(
            token.unwrap(),
            Some(error.to_string().into()),
            expected.into(),
        )),
        None => Err(ParseError::token_required(expected)),
    }
}

impl CmdParsable for bool {
    fn parse_cmd_raw(input: &str) -> Result<(Self, &str), ParseError<'_>> {
        let (token, remaining) = take_token(input);
        let token = match token {
            Some(token) => token,
            None => return Err(ParseError::token_required("boolean")),
        };
        let value = match token.borrow() {
            "true" | "t" | "yes" | "y" => true,
            "false" | "f" | "no" | "n" => false,
            _ => return Err(ParseError::token_parse(token, None, "boolean")),
        };
        Ok((value, remaining))
    }
}

use std::num::IntErrorKind;

macro_rules! gen_parsable_int {
    ($type:ty) => {
        impl CmdParsable for $type {
            fn parse_cmd_raw(input: &str) -> Result<(Self, &str), ParseError<'_>> {
                let (token, remaining) = take_token(input);
                match token {
                    Some(token) => {
                        token
                            .parse::<$type>()
                            .map(|num| (num, remaining))
                            .map_err(|error| {
                                let error_label: Option<Cow<'static, str>> = match error.kind() {
                                    IntErrorKind::PosOverflow => Some("too large".into()),
                                    IntErrorKind::NegOverflow => Some("too small".into()),
                                    _ => None,
                                };
                                ParseError::token_parse(token, error_label, "integer")
                            })
                    }
                    None => Err(ParseError::token_required("integer")),
                }
            }
        }
    };
}

gen_parsable_int!(u8);
gen_parsable_int!(i8);
gen_parsable_int!(u16);
gen_parsable_int!(i16);
gen_parsable_int!(u32);
gen_parsable_int!(i32);
gen_parsable_int!(u64);
gen_parsable_int!(i64);
gen_parsable_int!(usize);
gen_parsable_int!(isize);
gen_parsable_int!(u128);
gen_parsable_int!(i128);

macro_rules! gen_parsable_float {
    ($type:ty) => {
        impl CmdParsable for $type {
            fn parse_cmd_raw(input: &str) -> Result<(Self, &str), ParseError<'_>> {
                let (token, remaining) = take_token(input);
                match token {
                    Some(token) => token
                        .parse()
                        .map(|num| (num, remaining))
                        .map_err(|_| ParseError::token_parse(token, None, "real number")),
                    None => Err(ParseError::token_required("real number")),
                }
            }
        }
    };
}

gen_parsable_float!(f32);
gen_parsable_float!(f64);

impl CmdParsable for String {
    fn parse_cmd_raw(input: &str) -> Result<(Self, &str), ParseError<'_>> {
        let (token, remaining) = take_token(input);
        match token {
            Some(token) => Ok((token.into_owned(), remaining)),
            None => Err(ParseError::token_required("string")),
        }
    }
}

impl<T: CmdParsable> CmdParsable for Vec<T> {
    fn parse_cmd_raw(mut input: &str) -> Result<(Self, &str), ParseError<'_>> {
        let mut result = Vec::new();
        while has_tokens(input) {
            let (item, remaining) = parse_inner(input)?;
            input = remaining;
            result.push(item);
        }
        Ok((result, input))
    }
}

macro_rules! gen_parsable_tuple {
    ($($name:ident)*) => {
        impl<$($name: CmdParsable),*> CmdParsable for ($($name),*,) {
            #[allow(non_snake_case)]
            fn parse_cmd_raw(input: &str) -> Result<(Self, &str), ParseError<'_>> {
                $(let ($name, input) = parse_inner(input)?;)*
                Ok((($($name),*,), input))
            }
        }
    }
}

gen_parsable_tuple!(T1);
gen_parsable_tuple!(T1 T2);
gen_parsable_tuple!(T1 T2 T3);
gen_parsable_tuple!(T1 T2 T3 T4);
gen_parsable_tuple!(T1 T2 T3 T4 T5);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6 T7);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6 T7 T8);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6 T7 T8 T9);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15);
gen_parsable_tuple!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16);

impl CmdParsable for Duration {
    fn parse_cmd_raw(input: &str) -> Result<(Self, &str), ParseError<'_>> {
        let (token, input) = take_token(input);
        match token {
            Some(token) => {
                let mut parts = token.rsplitn(3, ':');
                let mut seconds: f64 = match parts.next().unwrap().parse() {
                    Ok(seconds) => seconds,
                    Err(_) => return Err(ParseError::token_parse(token, None, "duration")),
                };

                let mut multiplier = 60.0;
                for part in parts {
                    match part.parse::<u64>() {
                        Ok(part) => {
                            seconds += multiplier * part as f64;
                            multiplier *= 60.0;
                        }
                        Err(_) => return Err(ParseError::token_parse(token, None, "duration")),
                    }
                }
                Ok((Duration::from_secs_f64(seconds), input))
            }
            None => Err(ParseError::token_required("duration")),
        }
    }
}

impl CmdParsable for PathBuf {
    fn parse_cmd_raw(input: &str) -> Result<(Self, &str), ParseError<'_>> {
        let (token, input) = take_token(input);
        match token {
            Some(token) => {
                let path = PathBuf::from(token.into_owned());
                Ok((path, input))
            }
            None => Err(ParseError::token_required("path")),
        }
    }
}

impl<T: CmdParsable> CmdParsable for Box<T> {
    fn parse_cmd_raw(input: &str) -> Result<(Self, &str), ParseError<'_>> {
        let (result, input) = T::parse_cmd_raw(input)?;
        Ok((Box::new(result), input))
    }
}

impl<T: CmdParsable> CmdParsable for Option<T> {
    fn parse_cmd_raw(input: &str) -> Result<(Self, &str), ParseError<'_>> {
        if has_tokens(input) {
            let (value, input) = T::parse_cmd_raw(input)?;
            Ok((Some(value), input))
        } else {
            Ok((None, input))
        }
    }
}

pub fn string_parse_all(input: &str) -> Result<(String, &str), ParseError<'_>> {
    let mut remaining = input;
    loop {
        remaining = skip_token(remaining);
        if remaining.is_empty() || remaining.starts_with(')') {
            break;
        }
    }
    let length = input.len() - remaining.len();
    let result = input[..length].trim_end().to_string();
    Ok((result, remaining))
}

#[cfg(test)]
mod tests {
    use super::{parse_inner, CmdParsable};

    mod parse_boolean {
        use super::*;

        #[test]
        fn success() {
            assert_eq!(bool::parse_cmd_raw("true 1").unwrap(), (true, "1"));
            assert_eq!(bool::parse_cmd_raw("t 1").unwrap(), (true, "1"));
            assert_eq!(bool::parse_cmd_raw("yes 1").unwrap(), (true, "1"));
            assert_eq!(bool::parse_cmd_raw("y 1").unwrap(), (true, "1"));
            assert_eq!(bool::parse_cmd_raw("false 1").unwrap(), (false, "1"));
            assert_eq!(bool::parse_cmd_raw("f 1").unwrap(), (false, "1"));
            assert_eq!(bool::parse_cmd_raw("no 1").unwrap(), (false, "1"));
            assert_eq!(bool::parse_cmd_raw("n 1").unwrap(), (false, "1"));
        }

        #[test]
        fn unknown_variant() {
            assert_eq!(
                &bool::parse_cmd_raw("unknown").unwrap_err().to_string(),
                "invalid boolean \"unknown\""
            );
        }

        #[test]
        fn missing() {
            assert_eq!(
                &bool::parse_cmd_raw("").unwrap_err().to_string(),
                "expected boolean"
            );
        }
    }

    mod parse_string {
        use super::*;

        #[test]
        fn parse_string() {
            assert_eq!(
                String::parse_cmd_raw("abc def").unwrap(),
                ("abc".to_string(), "def")
            );
        }

        #[test]
        fn missing_string() {
            assert_eq!(
                &String::parse_cmd_raw("").unwrap_err().to_string(),
                "expected string"
            );
        }

        #[test]
        fn unexpected_token() {
            assert_eq!(
                parse_inner::<String>("(first second)")
                    .unwrap_err()
                    .to_string(),
                "unexpected token: \"second\""
            );
        }
    }

    mod parse_path {
        use super::*;
        use std::path::PathBuf;

        #[test]
        fn success() {
            assert_eq!(
                PathBuf::parse_cmd_raw("/usr/bin/bash 1").unwrap(),
                (PathBuf::from("/usr/bin/bash".to_string()), "1")
            );
        }

        #[test]
        fn missing() {
            assert_eq!(
                &PathBuf::parse_cmd_raw("").unwrap_err().to_string(),
                "expected path"
            );
        }
    }

    #[test]
    fn parse_box() {
        assert_eq!(
            Box::<u8>::parse_cmd_raw("10 20").unwrap(),
            (Box::new(10), "20")
        );
    }

    mod parse_vec {
        use super::*;

        #[test]
        fn parse_vec() {
            let (vector, remaining) = Vec::<u8>::parse_cmd_raw("10 20 30 40 50").unwrap();
            assert_eq!(vector, vec![10, 20, 30, 40, 50]);
            assert!(remaining.is_empty());
        }

        #[test]
        fn parse_vec_empty() {
            let (vector, remaining) = Vec::<u8>::parse_cmd_raw("").unwrap();
            assert_eq!(vector, vec![]);
            assert!(remaining.is_empty());
        }

        #[test]
        fn empty_parenthesis() {
            let (vector, remaining) = parse_inner::<Vec<u8>>("() 10 20").unwrap();
            assert_eq!(vector, vec![]);
            assert_eq!(remaining, "10 20");
        }

        #[test]
        fn stops_at_parenthesis() {
            let (vector, remaining) = parse_inner::<Vec<u8>>("(10 20) 30 40").unwrap();
            assert_eq!(vector, vec![10, 20]);
            assert_eq!(remaining, "30 40");
        }
    }

    mod parse_option {
        use super::*;

        #[test]
        fn parse_some() {
            assert_eq!(
                Option::<u32>::parse_cmd_raw("10  a").unwrap(),
                (Some(10), "a")
            );
        }

        #[test]
        fn parse_none() {
            assert_eq!(Option::<u32>::parse_cmd_raw("").unwrap(), (None, ""));
        }
    }

    mod parse_tuples {
        use super::*;

        #[test]
        fn parse_tuples() {
            let (result, remaining) =
                <(u8, u8, (u8, u8), (u8,))>::parse_cmd_raw("10 20 30 40 50 60").unwrap();
            assert_eq!(result, (10, 20, (30, 40), (50,)));
            assert_eq!(remaining, "60");
        }

        #[test]
        fn too_many_values() {
            assert_eq!(
                parse_inner::<(u8, u8)>("(10 20 30)")
                    .unwrap_err()
                    .to_string(),
                "unexpected token: \"30\""
            );
        }

        #[test]
        fn too_few_values() {
            assert_eq!(
                &<(u8, u8)>::parse_cmd_raw("(10)").unwrap_err().to_string(),
                "expected integer"
            );
        }
    }

    mod parse_duration {
        use super::*;
        use std::time::Duration;

        #[test]
        fn parse_seconds() {
            let (result, _) = Duration::parse_cmd_raw("10").unwrap();
            assert_eq!(result, Duration::from_secs(10));
        }

        #[test]
        fn parse_seconds_decimal() {
            let (result, _) = Duration::parse_cmd_raw("10.4").unwrap();
            assert_eq!(result, Duration::from_secs_f64(10.4));
        }

        #[test]
        fn parse_munites_seconds() {
            let (result, _) = Duration::parse_cmd_raw("14:10").unwrap();
            assert_eq!(result, Duration::from_secs(14 * 60 + 10));
        }

        #[test]
        fn parse_hours_munites_seconds() {
            let (result, _) = Duration::parse_cmd_raw("2:14:10.4").unwrap();
            assert_eq!(
                result,
                Duration::from_secs_f64(2.0 * 3600.0 + 14.0 * 60.0 + 10.4)
            );
        }

        #[test]
        fn too_many_parts() {
            Duration::parse_cmd_raw("4:2:14:10").unwrap_err();
        }

        #[test]
        fn invalid_symbol() {
            Duration::parse_cmd_raw("1s4:10").unwrap_err();
        }
    }

    mod parse_sting_all {
        use crate::string_parse_all;

        #[test]
        fn empty_string() {
            let (result, _) = string_parse_all("").unwrap();
            assert!(result.is_empty());
        }

        #[test]
        fn single_token() {
            let (result, remaining) = string_parse_all("abc  ").unwrap();
            assert_eq!(&result, "abc");
            assert_eq!(remaining, "");
        }

        #[test]
        fn multiple_token() {
            let (result, remaining) = string_parse_all("abc def ghi  ").unwrap();
            assert_eq!(&result, "abc def ghi");
            assert_eq!(remaining, "");
        }

        #[test]
        fn with_quotes() {
            let (result, remaining) = string_parse_all("abc \"))\" ghi  ").unwrap();
            assert_eq!(&result, "abc \"))\" ghi");
            assert_eq!(remaining, "");
        }

        #[test]
        fn stops_at_paren() {
            let (result, remaining) = string_parse_all("abc ) ghi").unwrap();
            assert_eq!(&result, "abc");
            assert_eq!(remaining, ") ghi");
        }
    }
}
