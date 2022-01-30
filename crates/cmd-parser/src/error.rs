use std::borrow::Cow;
use std::fmt;

use crate::token_stream::{Token, TokenStream};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParseErrorKind {
    TokenParse,
    TokenRequired,
    UnexpectedToken,
    UnknownVariant,
    UnknownAttribute,
}

#[derive(Debug, PartialEq)]
enum ParseErrorVariant<'a> {
    TokenParse(Cow<'a, str>, Option<Cow<'static, str>>),
    TokenRequired,
    UnexpectedToken(Cow<'a, str>),
    UnknownVariant(Cow<'a, str>),
    UnknownAttribute(Cow<'a, str>),
}

impl<'a> ParseErrorVariant<'a> {
    fn as_kind(&self) -> ParseErrorKind {
        match self {
            ParseErrorVariant::TokenParse(_, _) => ParseErrorKind::TokenParse,
            ParseErrorVariant::TokenRequired => ParseErrorKind::TokenRequired,
            ParseErrorVariant::UnexpectedToken(_) => ParseErrorKind::UnexpectedToken,
            ParseErrorVariant::UnknownVariant(_) => ParseErrorKind::UnknownVariant,
            ParseErrorVariant::UnknownAttribute(_) => ParseErrorKind::UnknownAttribute,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseError<'a> {
    variant: ParseErrorVariant<'a>,
    expected: Cow<'static, str>,
}

impl<'a> ParseError<'a> {
    pub fn unexpected_token(token: Cow<'a, str>) -> Self {
        ParseError {
            variant: ParseErrorVariant::UnexpectedToken(token),
            expected: "".into(),
        }
    }

    pub fn token_required(expected: impl Into<Cow<'static, str>>) -> Self {
        ParseError {
            variant: ParseErrorVariant::TokenRequired,
            expected: expected.into(),
        }
    }

    pub fn token_parse(
        token: Cow<'a, str>,
        error: Option<Cow<'static, str>>,
        expected: impl Into<Cow<'static, str>>,
    ) -> Self {
        ParseError {
            variant: ParseErrorVariant::TokenParse(token, error),
            expected: expected.into(),
        }
    }

    pub fn unknown_attribute(token: impl Into<Cow<'a, str>>) -> Self {
        ParseError {
            variant: ParseErrorVariant::UnknownAttribute(token.into()),
            expected: "".into(),
        }
    }

    pub fn unknown_variant(token: impl Into<Cow<'a, str>>) -> Self {
        ParseError {
            variant: ParseErrorVariant::UnknownVariant(token.into()),
            expected: "".into(),
        }
    }

    pub fn into_static(self) -> ParseError<'static> {
        ParseError {
            variant: match self.variant {
                ParseErrorVariant::TokenParse(token, error) => {
                    ParseErrorVariant::TokenParse(Cow::Owned(token.into_owned()), error)
                }
                ParseErrorVariant::TokenRequired => ParseErrorVariant::TokenRequired,
                ParseErrorVariant::UnexpectedToken(token) => {
                    ParseErrorVariant::UnexpectedToken(Cow::Owned(token.into_owned()))
                }
                ParseErrorVariant::UnknownVariant(token) => {
                    ParseErrorVariant::UnknownVariant(Cow::Owned(token.into_owned()))
                }
                ParseErrorVariant::UnknownAttribute(token) => {
                    ParseErrorVariant::UnknownAttribute(Cow::Owned(token.into_owned()))
                }
            },
            expected: self.expected,
        }
    }

    pub fn kind(&self) -> ParseErrorKind {
        self.variant.as_kind()
    }
}

impl<'a> std::error::Error for ParseError<'a> {}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.variant {
            ParseErrorVariant::TokenParse(token, error) => {
                f.write_fmt(format_args!("invalid {} \"{}\"", self.expected, token))?;
                if let Some(error) = error {
                    f.write_fmt(format_args!(": {}", error))?;
                }
                Ok(())
            }
            ParseErrorVariant::TokenRequired => {
                f.write_fmt(format_args!("expected {}", self.expected))
            }
            ParseErrorVariant::UnexpectedToken(token) => {
                f.write_fmt(format_args!("unexpected token: \"{}\"", token))
            }
            ParseErrorVariant::UnknownVariant(variant) => {
                f.write_fmt(format_args!("unknown variant: \"{}\"", variant))
            }
            ParseErrorVariant::UnknownAttribute(attribute) => {
                f.write_fmt(format_args!("unknown attribute: \"{}\"", attribute))
            }
        }
    }
}

pub enum ParseFailure<'a> {
    Error(ParseError<'a>),
    Unrecognized(UnrecognizedToken<'a>),
}

impl<'a, E: Into<ParseError<'a>>> From<E> for ParseFailure<'a> {
    fn from(error: E) -> Self {
        ParseFailure::Error(error.into())
    }
}

pub struct UnrecognizedToken<'a> {
    token: Token<'a>,
    remaining: TokenStream<'a>,
}

impl<'a> UnrecognizedToken<'a> {
    pub fn new(token: Token<'a>, remaining: TokenStream<'a>) -> Self {
        UnrecognizedToken { token, remaining }
    }

    pub fn token(&self) -> Token<'a> {
        self.token
    }

    pub fn remaining(&self) -> &TokenStream<'a> {
        &self.remaining
    }
}

impl<'a> From<UnrecognizedToken<'a>> for ParseFailure<'a> {
    fn from(unrecognized: UnrecognizedToken<'a>) -> Self {
        ParseFailure::Unrecognized(unrecognized)
    }
}
