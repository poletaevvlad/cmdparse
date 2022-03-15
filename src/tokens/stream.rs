use super::lexing::{skip_ws, take_lexeme, Lexeme};
use super::{Token, UnbalancedParenthesis};
use crate::error::{ParseError, ParseFailure};
use crate::{CompletionResult, ParseResult};

/// Representation of the input as a sequence of tokens
///
/// The `TokenStrem` holds the reference of the input stream and is responsible for tokenizing it
/// into a sequence of Tokens. TokenStream is immutable, all operations that extract tokens from
/// the stream must return another instance of a TokenStream representing tokens remaining in the
/// stream.
///
/// Opening and closing parenthesis have special meaning for the token streams. They are intended
/// to enclose tokens related to the same value to avoid ambiguity when parsing nested structures
/// with unknown number of required arguments, such as nested vectors. TokenStream aims to make
/// handling parenthesis as transparent as possible:
///  * attempt to take a token from a stream fails with an UnbalancedParenthesis error if the
///    non-consumed portion of the input starts with an opening parenthesis;
///  * the token stream is considered to be empty if the non-consumed portion of the input start
///    with a closing parenthesis;
///  * to handle nested structures, use either [`with_nested`](TokenStream::with_nested) or
///    [`complete_nested`](TokenStream::complete_nested). These methods take a closure that returns
///    a result and a `TokenStream` representing the remainder of the stream that wasn't consumed
///    during parsing. Token stream checks that all tokens consumed by the inner closure have been
///    processed and return an appropriate error otherwise.
#[derive(Debug, Clone, Copy)]
pub struct TokenStream<'a> {
    remaining: &'a str,
    all_consumed: bool,
    next_lexeme: Option<Lexeme<'a>>,
}

impl<'a> TokenStream<'a> {
    /// Creates a new `TokenStream` from an input string slice.
    pub fn new(input: &'a str) -> Self {
        let (next_lexeme, remaining) = take_lexeme(skip_ws(input));
        TokenStream {
            all_consumed: input.is_empty(),
            remaining,
            next_lexeme,
        }
    }

    /// Returns `true` if the last token of the stream has been consumed.
    ///
    /// More specifically, it returns `true` if the non-consumed portion of the input string
    /// is empty or contains an arbitrary number of whitespace characters followed by an octothorp
    /// (`#`), a closing parenthesis, or the end of a string.
    pub fn is_empty(&self) -> bool {
        self.peek().is_none()
    }

    /// Returns `true` if the entirety of the input string was consumed and the last token is not
    /// followed by any whitespace characters or a comment.
    ///
    /// This is useful when performing completion: suggestions should only be generated for the
    /// last token of the stream if it isn't followed by any other character.
    ///
    /// If `is_all_consumed` returns true, then `is_empty` must return true, while reverse is not
    /// true.
    pub fn is_all_consumed(&self) -> bool {
        self.all_consumed
    }

    fn advance(&self) -> TokenStream<'a> {
        let (next_lexeme, remaining) = take_lexeme(skip_ws(self.remaining));
        TokenStream {
            all_consumed: self.remaining.is_empty(),
            remaining,
            next_lexeme,
        }
    }

    /// Returns the next token in the token stream, if any. This is an efficient operation that
    /// does not cause any lexing operations and should be preferred when the token will not be
    /// consumed.
    ///
    /// It returns:
    ///  * `Some(Ok(token))` if the stream is not empty;
    ///  * `Some(Err(UnbalancedParenthesis))` if the remaining part of the string starts with a
    ///    closing parenthesis;
    ///  * `None` if the stream is empty.
    pub fn peek(&self) -> Option<Result<Token<'a>, UnbalancedParenthesis>> {
        match self.next_lexeme {
            Some(Lexeme::ClosingParen) => None,
            Some(lexeme) => Some(Token::from_lexeme(lexeme)),
            None => None,
        }
    }

    /// Returns the next token in the stream and an instance of the [`TokenStream`] representing
    /// the remaining tokens. The behavior is similar to [`peek`](TokenStream::peek) with the same
    /// returned value with addition of a `TokenStream` instance.
    pub fn take(&self) -> Option<Result<(Token<'a>, TokenStream<'a>), UnbalancedParenthesis>> {
        match self.peek() {
            Some(Ok(token)) => Some(Ok((token, self.advance()))),
            Some(Err(error)) => Some(Err(error)),
            None => None,
        }
    }

    /// Executes a closure on an inner portion of the token stream. The closure passed to this
    /// function returns either a parsed value along with the remaining `TokenSream`, or a parsing
    /// failure.
    ///
    /// This method behaves differently depending on whether the non-consumed portion of the input
    /// string starts with an opening parenthesis. If not, the value returned by the closure as is.
    ///
    /// Otherwise, this functions consumes the parenthesis, and calls the closure. `with_nested`
    /// then returns a parse failure if the closure call failed or if there are any not consumed
    /// tokens remaining. Note, that in case the closure fails due to an [`UnrecognizedToken`]
    /// it's converted into an error.
    pub fn with_nested<R, F: FnOnce(TokenStream<'a>) -> ParseResult<'a, R>>(
        &self,
        callback: F,
    ) -> ParseResult<'a, R> {
        let (has_parens, stream) = self.enter_nested();
        let (result, stream) = match callback(stream) {
            Ok(sucess) => sucess,
            Err(error) if !has_parens => return Err(error),
            Err(error @ ParseFailure::Error(_)) => return Err(error),
            Err(ParseFailure::Unrecognized(unrecognized)) => {
                return Err(unrecognized.into_error().into())
            }
        };
        match stream.exit_nested(has_parens) {
            Ok(remaining) => Ok((result, remaining)),
            Err(Ok(token)) => Err(ParseError::unknown(token).into()),
            Err(Err(error)) => Err(error.into()),
        }
    }

    /// Executes a closure on an inner portion of the token stream. If the non-consumed portion of
    /// the input stream starts with an opening parenthesis, this function attempts to skip all
    /// tokens until the corresponding closing parenthesis. If no such parenthesis is found or if
    /// there is no opening parenthesis, the closure is called and its execution result is
    /// returned.
    pub fn complete_nested<F: FnOnce(TokenStream<'a>) -> CompletionResult<'a>>(
        &self,
        callback: F,
    ) -> CompletionResult<'a> {
        let (has_parens, stream) = self.enter_nested();
        if has_parens {
            let mut end_stream = stream;
            let mut paren_depth = 1;
            while paren_depth > 0 {
                match end_stream.next_lexeme {
                    Some(Lexeme::OpeningParen) => paren_depth += 1,
                    Some(Lexeme::ClosingParen) => paren_depth -= 1,
                    Some(_) => (),
                    None => break,
                }
                end_stream = end_stream.advance();
            }

            if paren_depth == 0 {
                return CompletionResult::new(end_stream, true);
            }
        }

        callback(stream)
    }

    fn enter_nested(&self) -> (bool, TokenStream<'a>) {
        if let Some(Lexeme::OpeningParen) = self.next_lexeme {
            (true, self.advance())
        } else {
            (false, *self)
        }
    }

    fn exit_nested(
        &self,
        has_parens: bool,
    ) -> Result<TokenStream<'a>, Result<Token<'a>, UnbalancedParenthesis>> {
        if !has_parens {
            return Ok(*self);
        };

        match self.next_lexeme {
            Some(Lexeme::ClosingParen) => Ok(self.advance()),
            Some(_) => Err(self.peek().unwrap()),
            None => Ok(*self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TokenStream;
    use crate::testing::token;
    use crate::tokens::{Token, UnbalancedParenthesis};
    use crate::CompletionResult;
    use std::collections::BTreeSet;

    fn assert_takes<'a>(stream: TokenStream<'a>, expected: Token<'a>) -> TokenStream<'a> {
        let peeked = stream.peek().unwrap().unwrap();
        let (taken, stream) = stream.take().unwrap().unwrap();
        assert_eq!(peeked, taken);
        assert_eq!(peeked, expected);
        stream
    }

    #[test]
    fn taking_tokens() {
        let stream = TokenStream::new("first --second third");
        let stream = assert_takes(stream, token!("first"));
        assert!(!stream.is_all_consumed());
        let stream = assert_takes(stream, token!(--"second"));
        assert!(!stream.is_all_consumed());
        let stream = assert_takes(stream, token!("third"));
        assert!(stream.is_all_consumed());
        assert!(stream.peek().is_none());
        assert!(stream.take().is_none());
    }

    #[test]
    fn takes_nested_structures() {
        let stream = TokenStream::new("first (--second third) fourth");
        let stream = assert_takes(stream, token!("first"));

        assert!(matches!(stream.peek(), Some(Err(UnbalancedParenthesis))));
        assert!(matches!(stream.take(), Some(Err(UnbalancedParenthesis))));

        let (result, stream) = stream
            .with_nested(|stream| {
                let stream = assert_takes(stream, token!(--"second"));
                let stream = assert_takes(stream, token!("third"));
                assert!(matches!(stream.peek(), None));
                assert!(matches!(stream.take(), None));
                Ok((true, stream))
            })
            .unwrap();
        assert!(result);

        assert_takes(stream, token!("fourth"));
    }

    #[test]
    fn takes_nested_structures_no_parens() {
        let stream = TokenStream::new("first --second third fourth");
        let stream = assert_takes(stream, token!("first"));
        let (_, stream) = stream
            .with_nested(|stream| {
                let stream = assert_takes(stream, token!(--"second"));
                let stream = assert_takes(stream, token!("third"));
                Ok(((), stream))
            })
            .unwrap();
        assert_takes(stream, token!("fourth"));
    }

    #[test]
    fn nested_struct_remaining_tokens() {
        let stream = TokenStream::new("(first second) third");

        assert!(matches!(stream.peek(), Some(Err(UnbalancedParenthesis))));
        assert!(matches!(stream.take(), Some(Err(UnbalancedParenthesis))));

        let error = stream
            .with_nested(|stream| {
                let stream = assert_takes(stream, token!("first"));
                Ok(((), stream))
            })
            .unwrap_err();
        assert_eq!(
            error.as_error().unwrap().to_string(),
            "unrecognized token: \"second\""
        );
    }

    #[test]
    fn nested_struct_remaining_paren() {
        let stream = TokenStream::new("(first (second)) third");
        let error = stream
            .with_nested(|stream| Ok(((), assert_takes(stream, token!("first")))))
            .unwrap_err();
        assert_eq!(
            error.as_error().unwrap().to_string(),
            "unbalanced parenthesis"
        );
    }

    #[test]
    fn completes_nested_skips_if_closed() {
        let stream = TokenStream::new("(first (()(second))) third");
        let result = stream.complete_nested(|_input| panic!("should not be called"));
        assert!(result.value_consumed);
        assert!(result.suggestions.is_empty());
        assert_takes(result.remaining.unwrap(), token!("third"));
    }

    #[test]
    fn completes_nested_calls_parser_if_not_closed() {
        let stream = TokenStream::new("(first (()(second) third");
        let result = stream.complete_nested(|input| {
            assert_takes(input, token!("first"));
            CompletionResult::new_final(true).add_suggestions(["abc".into()])
        });
        assert!(result.value_consumed);
        assert_eq!(result.suggestions, BTreeSet::from(["abc".into()]));
        assert!(result.remaining.is_none());
    }
}
