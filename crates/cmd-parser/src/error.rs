//! Types related to the failure conditions of the parsing process.
//!
//! The parsing failure can be indicated by [`ParseError`], [`UnrecognizedToken`], or
//! [`ParseFailure`]. See [`Parser`](super::Parser) for documentation on the relationship between
//! these types and how each is used in the parsing process

use crate::tokens::{Token, TokenStream, UnbalancedParenthesis};
use std::borrow::Cow;
use std::fmt;

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

/// Unrecoverable parsing error
///
/// This type represents a parsing error caused by the invalid data. This error is propagated up
/// the call chain and most often is returned to the parsing initiator. Creation of this error
/// usually means that an unrecoverable error has occurred (with few exceptions).
///
/// `ParseError` may optionally contain a name of the type (in the user-readable format) parsing
/// of which has caused the error.
#[derive(Debug, PartialEq)]
pub struct ParseError<'a> {
    variant: ParseErrorVariant<'a>,
    expected: Option<Cow<'static, str>>,
}

impl<'a> ParseError<'a> {
    /// Creates a new error that represents a situation when the token (either an attribute or a
    /// text token representing an enum variant) is not recognized by the parser.
    ///
    /// Note that if the unrecognized token in the first token in the input stream, it should
    /// return an [`UnrecognizedToken`] instead. If the [`UnrecognizedToken`] is returned by some
    /// other parser and the current value is already partially parsed, this [`UnrecognizedToken`]
    /// can be converted into a `ParseError` which would be equivalent to calling this method.
    pub fn unknown(token: Token<'a>) -> Self {
        ParseError {
            variant: ParseErrorVariant::Unknown(token),
            expected: None,
        }
    }

    /// Creates a new error representing a situation when a token (most often a text token) is
    /// required by the parser, but the end of a token stream has already been reached.
    pub fn token_required() -> Self {
        ParseError {
            variant: ParseErrorVariant::TokenRequired,
            expected: None,
        }
    }

    /// Creates a new error representing a punctuation error: a situation when parenthesis opened
    /// or closed unexpectedly.
    ///
    /// Calling this method is equivalent to converting [`UnbalancedParenthesis`] error into
    /// [`ParseError`].
    pub fn unbalanced_parenthesis() -> Self {
        ParseError {
            variant: ParseErrorVariant::UnbalancedParenthesis,
            expected: None,
        }
    }

    /// Creates a new error representing a failure to parse the `token`. This error occurs when the
    /// token's contents are invalid, for example a token contains non-numeric characters if the
    /// number is expected. Parsers implementation may choose to provide a message describing why
    /// the parsing has failed, it will be shown when the error is formatted.
    ///
    /// This function takes a Token that the parser failed to handle and an optional message text.
    pub fn invalid(token: Token<'a>, message: Option<Cow<'static, str>>) -> Self {
        ParseError {
            variant: ParseErrorVariant::Invalid { token, message },
            expected: None,
        }
    }

    /// Creates an error with a custom message. No semantics are associated with this error, it can
    /// be used when neither of other error types is applicable, for example when performing
    /// validation.
    pub fn custom(message: impl Into<Cow<'static, str>>) -> Self {
        ParseError {
            variant: ParseErrorVariant::Custom(message.into()),
            expected: None,
        }
    }

    /// Updates the error and assigns a human-readable name of a type the parser was trying to
    /// parse when if failed.
    ///
    /// This value is displayed when `ParseError` is formatted.
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
                f.write_fmt(format_args!("cannot parse {}", token.into_raw_lexeme()))?;
                if let Some(message) = message {
                    f.write_fmt(format_args!(" ({})", message))?;
                }
            }
            ParseErrorVariant::Unknown(Token::Text(text)) => {
                f.write_fmt(format_args!("unrecognized token: {}", text))?;
            }
            ParseErrorVariant::Unknown(Token::Attribute(attr)) => {
                f.write_fmt(format_args!("unrecognized attribute: {}", attr))?;
            }
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

/// Value’s failed parsing result
///
/// The parsing process can fail in two different ways:
///  * The input stream contains invalid tokens: the number, order, or contents of tokens is
///    incorrect. Such errors are represented by the `ParseError` struct.
///  * The input may be correct, but the current parser does not recognize the input stream's first
///    token. In such case, another parser may recognize this token. Such situations arise when
///    dealing with optional fields, variable number of fields, etc. Such situations are
///    represented by the `UnexpectedToken` struct.
///
/// `ParseFailure` enum's variants encapsulates both these structs.
#[derive(Debug)]
pub enum ParseFailure<'a> {
    /// The parsing failed with an unrecoverable error
    Error(ParseError<'a>),

    /// The token is not recognized by the parser
    Unrecognized(UnrecognizedToken<'a>),
}

impl<'a> ParseFailure<'a> {
    /// Returns a reference to the [`ParseError`] if this `ParseFailure` corresponds to an error or
    /// `None` otherwise.
    pub fn as_error(&self) -> Option<&ParseError<'a>> {
        if let ParseFailure::Error(error) = self {
            Some(error)
        } else {
            None
        }
    }

    /// Returns a reference to the [`UnrecognizedToken`] if this `ParseFailure` corresponds to a
    /// failure due to an unrecognized token or `None` otherwise.
    pub fn as_unrecognized(&self) -> Option<&UnrecognizedToken<'a>> {
        if let ParseFailure::Unrecognized(unrecognized) = self {
            Some(unrecognized)
        } else {
            None
        }
    }
}

impl<'a, E: Into<ParseError<'a>>> From<E> for ParseFailure<'a> {
    fn from(error: E) -> Self {
        ParseFailure::Error(error.into())
    }
}

/// Parsing failure due to an unrecognized attribute or enum discriminator
///
/// This struct encapsulates a token that the parser tried to consume but did not recognize, along
/// with a [`TokenStream`] containing tokens that directly follow this token. In other words, it
/// should be constructed from both values returned by [`TokenStream::take`].
#[derive(Debug)]
pub struct UnrecognizedToken<'a> {
    token: Token<'a>,
    remaining: TokenStream<'a>,
}

impl<'a> UnrecognizedToken<'a> {
    /// Create a new `UnrecognizedToken` instance from a token that was not recognized by the
    /// parser and the remaining tokens stream.
    pub fn new(token: Token<'a>, remaining: TokenStream<'a>) -> Self {
        UnrecognizedToken { token, remaining }
    }

    /// Returns a token that was not recognized by the parser.
    pub fn token(&self) -> Token<'a> {
        self.token
    }

    /// Returns a token stream that represents tokens that follow the `token`.
    pub fn remaining(&self) -> &TokenStream<'a> {
        &self.remaining
    }

    /// Converts this `UnrecognizedToken` into a [`ParseError`].
    pub fn into_error(self) -> ParseError<'a> {
        ParseError::unknown(self.token())
    }
}

impl<'a> From<UnrecognizedToken<'a>> for ParseFailure<'a> {
    fn from(unrecognized: UnrecognizedToken<'a>) -> Self {
        ParseFailure::Unrecognized(unrecognized)
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
