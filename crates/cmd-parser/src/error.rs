use crate::tokens::{Token, TokenStream, TokenValue};
use std::borrow::Cow;
use std::fmt;

#[derive(Debug)]
pub struct UnbalancedParenthesis;

#[derive(Debug, PartialEq)]
enum ParseErrorVariant<'a> {
    Invalid {
        token: Token<'a>,
        message: Option<Cow<'static, str>>,
    },
    Unknown(Token<'a>),
    TokenRequired,
    UnbalancedParenthesis,
    Custom(Cow<'static, str>),
}

#[derive(Debug, PartialEq)]
pub struct ParseError<'a> {
    variant: ParseErrorVariant<'a>,
    expected: Option<Cow<'static, str>>,
}

impl<'a> ParseError<'a> {
    pub fn unknown(token: Token<'a>) -> Self {
        ParseError {
            variant: ParseErrorVariant::Unknown(token),
            expected: None,
        }
    }

    pub fn token_required() -> Self {
        ParseError {
            variant: ParseErrorVariant::TokenRequired,
            expected: None,
        }
    }

    pub fn unbalanced_parenthesis() -> Self {
        ParseError {
            variant: ParseErrorVariant::UnbalancedParenthesis,
            expected: None,
        }
    }

    pub fn invalid(token: Token<'a>, message: Option<Cow<'static, str>>) -> Self {
        ParseError {
            variant: ParseErrorVariant::Invalid { token, message },
            expected: None,
        }
    }

    pub fn custom(message: impl Into<Cow<'static, str>>) -> Self {
        ParseError {
            variant: ParseErrorVariant::Custom(message.into()),
            expected: None,
        }
    }
    pub fn expected(mut self, expected: impl Into<Cow<'static, str>>) -> Self {
        self.expected = Some(expected.into());
        self
    }
}

impl<'a> From<UnbalancedParenthesis> for ParseError<'a> {
    fn from(_: UnbalancedParenthesis) -> Self {
        ParseError::unbalanced_parenthesis()
    }
}

impl<'a> std::error::Error for ParseError<'a> {}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.variant {
            ParseErrorVariant::Invalid { token, message } => {
                f.write_fmt(format_args!("cannot parse {}", token.value().into_inner()))?;
                if let Some(message) = message {
                    f.write_fmt(format_args!(" ({})", message))?;
                }
            }
            ParseErrorVariant::Unknown(unknown) => match unknown.value() {
                TokenValue::Text(text) => {
                    f.write_fmt(format_args!("unrecognized token: {}", text))?;
                }
                TokenValue::Attribute(attr) => {
                    f.write_fmt(format_args!("unrecognized attribute: {}", attr))?;
                }
            },
            ParseErrorVariant::TokenRequired => f.write_str("not enough tokens")?,
            ParseErrorVariant::UnbalancedParenthesis => f.write_str("unbalanced parenthesis")?,
            ParseErrorVariant::Custom(message) => f.write_str(message)?,
        }

        if let Some(expected) = &self.expected {
            f.write_fmt(format_args!(", expected {}", expected))?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum ParseFailure<'a> {
    Error(ParseError<'a>),
    Unrecognized(UnrecognizedToken<'a>),
}

impl<'a, E: Into<ParseError<'a>>> From<E> for ParseFailure<'a> {
    fn from(error: E) -> Self {
        ParseFailure::Error(error.into())
    }
}

#[derive(Debug)]
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

impl<'a> UnrecognizedToken<'a> {
    pub fn into_error(self) -> ParseError<'a> {
        ParseError::unknown(self.token())
    }
}

#[cfg(test)]
mod tests {
    use super::ParseError;
    use crate::testing::token;

    mod error_display {
        use super::*;

        #[test]
        fn invalid() {
            let error = ParseError::invalid(token!("<<token>>"), None).expected("integer");
            assert_eq!(
                &error.to_string(),
                "cannot parse \"<<token>>\", expected integer"
            );
        }

        #[test]
        fn invalid_with_message() {
            let error = ParseError::invalid(token!("<<token>>"), Some("not a number".into()));
            assert_eq!(
                &error.to_string(),
                "cannot parse \"<<token>>\" (not a number)"
            );
        }

        #[test]
        fn unknown_attribute() {
            let error = ParseError::unknown(token!(--"<<attr>>"));
            assert_eq!(&error.to_string(), "unrecognized attribute: \"<<attr>>\"");
        }

        #[test]
        fn unknown_text() {
            let error = ParseError::unknown(token!("<<text>>"));
            assert_eq!(&error.to_string(), "unrecognized token: \"<<text>>\"");
        }
    }
}
