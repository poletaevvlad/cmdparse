use super::lexing::{skip_ws, take_lexeme, Lexeme, LexemeKind};
use super::Token;
use crate::error::UnbalancedParenthesis;

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

    pub fn enter_nested(&self) -> (NestingGuard, TokenStream) {
        match self.next_lexeme {
            Some(lexeme) if matches!(lexeme.kind, LexemeKind::OpeningParen) => {
                (NestingGuard { in_parens: true }, self.advance())
            }
            Some(_) | None => (NestingGuard { in_parens: false }, *self),
        }
    }

    pub fn exit_nested(
        &self,
        guard: NestingGuard,
    ) -> Result<TokenStream, Result<Token<'a>, UnbalancedParenthesis>> {
        if !guard.in_parens {
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
    in_parens: bool,
}

#[cfg(test)]
mod tests {
    use super::TokenStream;
    use crate::tokens::{token_macro::token, Token, UnbalancedParenthesis};

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
}
