use std::borrow::Cow;
use std::fmt;

#[derive(Debug, PartialEq)]
enum ParseErrorKind<'a> {
    TokenParse(Cow<'a, str>, Option<Cow<'static, str>>),
    TokenRequired,
    UnexpectedToken(Cow<'a, str>),
    UnknownVariant(Cow<'a, str>),
    UnknownAttribute(Cow<'a, str>),
}

#[derive(Debug, PartialEq)]
pub struct ParseError<'a> {
    kind: ParseErrorKind<'a>,
    expected: Cow<'static, str>,
}

impl<'a> ParseError<'a> {
    pub fn unexpected_token(token: Cow<'a, str>) -> Self {
        ParseError {
            kind: ParseErrorKind::UnexpectedToken(token),
            expected: "".into(),
        }
    }

    pub fn token_required(expected: impl Into<Cow<'static, str>>) -> Self {
        ParseError {
            kind: ParseErrorKind::TokenRequired,
            expected: expected.into(),
        }
    }

    pub fn token_parse(
        token: Cow<'a, str>,
        error: Option<Cow<'static, str>>,
        expected: impl Into<Cow<'static, str>>,
    ) -> Self {
        ParseError {
            kind: ParseErrorKind::TokenParse(token, error),
            expected: expected.into(),
        }
    }

    pub fn unknown_attribute(token: impl Into<Cow<'a, str>>) -> Self {
        ParseError {
            kind: ParseErrorKind::UnknownAttribute(token.into()),
            expected: "".into(),
        }
    }

    pub fn unknown_variant(token: Cow<'a, str>, expected: impl Into<Cow<'static, str>>) -> Self {
        ParseError {
            kind: ParseErrorKind::UnknownVariant(token),
            expected: expected.into(),
        }
    }

    pub fn into_static(self) -> ParseError<'static> {
        ParseError {
            kind: match self.kind {
                ParseErrorKind::TokenParse(token, error) => {
                    ParseErrorKind::TokenParse(Cow::Owned(token.into_owned()), error)
                }
                ParseErrorKind::TokenRequired => ParseErrorKind::TokenRequired,
                ParseErrorKind::UnexpectedToken(token) => {
                    ParseErrorKind::UnexpectedToken(Cow::Owned(token.into_owned()))
                }
                ParseErrorKind::UnknownVariant(token) => {
                    ParseErrorKind::UnknownVariant(Cow::Owned(token.into_owned()))
                }
                ParseErrorKind::UnknownAttribute(token) => {
                    ParseErrorKind::UnknownAttribute(Cow::Owned(token.into_owned()))
                }
            },
            expected: self.expected,
        }
    }
}

impl<'a> std::error::Error for ParseError<'a> {}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ParseErrorKind::TokenParse(token, error) => {
                f.write_fmt(format_args!("invalid {} \"{}\"", self.expected, token))?;
                if let Some(error) = error {
                    f.write_fmt(format_args!(": {}", error))?;
                }
                Ok(())
            }
            ParseErrorKind::TokenRequired => {
                f.write_fmt(format_args!("expected {}", self.expected))
            }
            ParseErrorKind::UnexpectedToken(token) => {
                f.write_fmt(format_args!("unexpected token: \"{}\"", token))
            }
            ParseErrorKind::UnknownVariant(variant) => {
                f.write_fmt(format_args!("unknown variant: \"{}\"", variant))
            }
            ParseErrorKind::UnknownAttribute(attribute) => {
                f.write_fmt(format_args!("unknown attribute: \"{}\"", attribute))
            }
        }
    }
}
