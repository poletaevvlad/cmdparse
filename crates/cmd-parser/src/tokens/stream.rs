use super::lexing::{skip_ws, take_lexeme, Lexeme, LexemeKind};
use super::Token;
use crate::error::{ParseError, UnbalancedParenthesis};
use crate::{CompletionResult, ParseFailure, ParseResult};

#[derive(Debug, Clone, Copy)]
pub struct TokenStream<'a> {
    remaining: &'a str,
    next_lexeme: Option<Lexeme<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        let (next_lexeme, remaining) = take_lexeme(skip_ws(input));
        TokenStream {
            remaining,
            next_lexeme,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.peek().is_none()
    }

    fn advance(&self) -> TokenStream<'a> {
        let (next_lexeme, remaining) = take_lexeme(self.remaining);
        TokenStream {
            remaining,
            next_lexeme,
        }
    }

    pub fn peek(&self) -> Option<Result<Token<'a>, UnbalancedParenthesis>> {
        match self.next_lexeme {
            Some(lexeme) if matches!(lexeme.kind, LexemeKind::ClosingParen) => None,
            Some(lexeme) => Some(Token::from_lexeme(lexeme)),
            None => None,
        }
    }

    pub fn take(&self) -> Option<Result<(Token<'a>, TokenStream<'a>), UnbalancedParenthesis>> {
        match self.peek() {
            Some(Ok(token)) => Some(Ok((token, self.advance()))),
            Some(Err(error)) => Some(Err(error)),
            None => None,
        }
    }

    pub fn with_nested<R, F: FnOnce(TokenStream<'a>) -> ParseResult<'a, R>>(
        &self,
        callback: F,
    ) -> ParseResult<'a, R> {
        let (guard, stream) = self.enter_nested();
        let has_parents = guard.has_parens();
        let (result, stream) = match callback(stream) {
            Ok(sucess) => sucess,
            Err(error) if !has_parents => return Err(error),
            Err(error @ ParseFailure::Error(_)) => return Err(error),
            Err(ParseFailure::Unrecognized(unrecognized)) => {
                return Err(unrecognized.into_error().into())
            }
        };
        match stream.exit_nested(guard) {
            Ok(remaining) => Ok((result, remaining)),
            Err(Ok(token)) => Err(ParseError::unknown(token).into()),
            Err(Err(error)) => Err(error.into()),
        }
    }

    pub fn complete_nested<F: FnOnce(TokenStream<'a>) -> CompletionResult<'a>>(
        &self,
        callback: F,
    ) -> CompletionResult<'a> {
        let (guard, stream) = self.enter_nested();
        if guard.has_parens() {
            let mut end_stream = stream;
            let mut paren_depth = 1;
            while paren_depth > 0 {
                match end_stream.next_lexeme {
                    Some(lexeme) if matches!(lexeme.kind, LexemeKind::OpeningParen) => {
                        paren_depth += 1;
                    }
                    Some(lexeme) if matches!(lexeme.kind, LexemeKind::ClosingParen) => {
                        paren_depth -= 1;
                    }
                    Some(_) => (),
                    None => break,
                }
                end_stream = end_stream.advance();
            }

            if paren_depth == 0 {
                return CompletionResult::consumed(end_stream);
            }
        }

        callback(stream)
    }

    pub fn enter_nested(&self) -> (NestingGuard, TokenStream<'a>) {
        match self.next_lexeme {
            Some(lexeme) if matches!(lexeme.kind, LexemeKind::OpeningParen) => {
                (NestingGuard { has_parens: true }, self.advance())
            }
            Some(_) | None => (NestingGuard { has_parens: false }, *self),
        }
    }

    pub fn exit_nested(
        &self,
        guard: NestingGuard,
    ) -> Result<TokenStream<'a>, Result<Token<'a>, UnbalancedParenthesis>> {
        if !guard.has_parens {
            return Ok(*self);
        };

        match self.next_lexeme {
            Some(lexeme) => {
                if matches!(lexeme.kind, LexemeKind::ClosingParen) {
                    Ok(self.advance())
                } else {
                    Err(self.peek().unwrap())
                }
            }
            None => Ok(*self),
        }
    }
}

pub struct NestingGuard {
    has_parens: bool,
}

impl NestingGuard {
    fn has_parens(&self) -> bool {
        self.has_parens
    }
}

#[cfg(test)]
mod tests {
    use crate::testing::token;
    use std::collections::HashSet;

    use super::TokenStream;
    use crate::tokens::{Token, UnbalancedParenthesis};
    use crate::CompletionResult;

    fn assert_takes<'a>(stream: TokenStream<'a>, expected: Token) -> TokenStream<'a> {
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
        let stream = assert_takes(stream, token!(--"second"));
        let stream = assert_takes(stream, token!("third", last));
        assert!(stream.peek().is_none());
        assert!(stream.take().is_none());
    }

    #[test]
    fn takes_nested_structures() {
        let stream = TokenStream::new("first (--second third) fourth");
        let stream = assert_takes(stream, token!("first"));

        assert!(matches!(stream.peek(), Some(Err(UnbalancedParenthesis))));
        assert!(matches!(stream.take(), Some(Err(UnbalancedParenthesis))));

        let (guard, stream) = stream.enter_nested();
        let stream = assert_takes(stream, token!(--"second"));
        let stream = assert_takes(stream, token!("third"));

        assert!(matches!(stream.peek(), None));
        assert!(matches!(stream.take(), None));

        let stream = stream.exit_nested(guard).unwrap();

        assert_takes(stream, token!("fourth", last));
    }

    #[test]
    fn takes_nested_structures_no_parens() {
        let stream = TokenStream::new("first --second third fourth");
        let stream = assert_takes(stream, token!("first"));
        let (guard, stream) = stream.enter_nested();
        let stream = assert_takes(stream, token!(--"second"));
        let stream = assert_takes(stream, token!("third"));
        let stream = stream.exit_nested(guard).unwrap();
        assert_takes(stream, token!("fourth", last));
    }

    #[test]
    fn nested_struct_remaining_tokens() {
        let stream = TokenStream::new("(first second) third");

        assert!(matches!(stream.peek(), Some(Err(UnbalancedParenthesis))));
        assert!(matches!(stream.take(), Some(Err(UnbalancedParenthesis))));

        let (guard, stream) = stream.enter_nested();
        let stream = assert_takes(stream, token!("first"));

        let error = stream.exit_nested(guard).unwrap_err();
        assert_eq!(error.unwrap(), token!("second"));
    }

    #[test]
    fn nested_struct_remaining_paren() {
        let stream = TokenStream::new("(first (second)) third");

        let (guard, stream) = stream.enter_nested();
        let stream = assert_takes(stream, token!("first"));

        let error = stream.exit_nested(guard).unwrap_err();
        assert!(matches!(error, Err(UnbalancedParenthesis)));
    }

    #[test]
    fn completes_nested_skips_if_closed() {
        let stream = TokenStream::new("(first (()(second))) third");
        let result = stream.complete_nested(|_input| panic!("should not be called"));
        assert!(result.value_consumed);
        assert!(result.suggestions.is_empty());
        assert_takes(result.remaining.unwrap(), token!("third", last));
    }

    #[test]
    fn completes_nested_calls_parser_if_not_closed() {
        let stream = TokenStream::new("(first (()(second) third");
        let result = stream.complete_nested(|input| {
            assert_takes(input, token!("first"));
            CompletionResult::complete(HashSet::from(["abc".into()]))
        });
        assert!(result.value_consumed);
        assert_eq!(result.suggestions, HashSet::from(["abc".into()]));
        assert!(result.remaining.is_none());
    }
}
