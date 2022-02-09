pub mod error;
pub mod parsers;
pub mod testing;
pub mod tokens;
pub mod utils;

use std::borrow::Cow;
use std::collections::BTreeSet;

pub use cmd_parser_derive::Parsable;
use error::ParseError;
pub use error::ParseFailure;
use tokens::TokenStream;

pub type ParseResult<'a, T> = Result<(T, TokenStream<'a>), ParseFailure<'a>>;

#[derive(Debug)]
pub struct CompletionResult<'a> {
    pub remaining: Option<TokenStream<'a>>,
    pub value_consumed: bool,
    pub suggestions: BTreeSet<Cow<'static, str>>,
}

impl<'a> CompletionResult<'a> {
    pub fn new_final(consumed: bool) -> Self {
        CompletionResult {
            remaining: None,
            value_consumed: consumed,
            suggestions: BTreeSet::new(),
        }
    }

    pub fn new(remaining: TokenStream<'a>, consumed: bool) -> Self {
        CompletionResult {
            remaining: Some(remaining),
            value_consumed: consumed,
            suggestions: BTreeSet::new(),
        }
    }

    pub fn set_consumed(mut self, consumed: bool) -> Self {
        self.value_consumed = consumed;
        self
    }

    pub fn add_suggestions(
        mut self,
        suggestions: impl IntoIterator<Item = Cow<'static, str>>,
    ) -> Self {
        self.suggestions.extend(suggestions.into_iter());
        self
    }
}

pub trait Parser<Ctx> {
    type Value;

    fn create(ctx: Ctx) -> Self;
    fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value>;
    fn complete<'a>(&self, input: TokenStream<'a>) -> CompletionResult<'a>;
}

pub trait Parsable<Ctx> {
    type Parser: Parser<Ctx, Value = Self>;

    fn new_parser(ctx: Ctx) -> Self::Parser {
        Self::Parser::create(ctx)
    }
}

fn parse_with_parser<Ctx, P: Parser<Ctx>>(input: &str, parser: P) -> Result<P::Value, ParseError> {
    let tokens = TokenStream::new(input);
    match parser.parse(tokens) {
        Ok((result, remaining)) => match remaining.peek() {
            Some(Ok(token)) => Err(ParseError::unknown(token)),
            Some(Err(err)) => Err(err.into()),
            None => Ok(result),
        },
        Err(ParseFailure::Error(err)) => Err(err),
        Err(ParseFailure::Unrecognized(unrecognized)) => Err(unrecognized.into_error()),
    }
}

pub fn parse_parser<Ctx, P: Parser<Ctx>>(input: &str, ctx: Ctx) -> Result<P::Value, ParseError> {
    parse_with_parser(input, P::create(ctx))
}

pub fn parse<Ctx, T: Parsable<Ctx>>(input: &str, ctx: Ctx) -> Result<T, ParseError> {
    parse_with_parser(input, T::new_parser(ctx))
}

pub fn complete_parser<Ctx, P: Parser<Ctx>>(input: &str, ctx: Ctx) -> BTreeSet<Cow<'static, str>> {
    let tokens = TokenStream::new(input);
    P::create(ctx).complete(tokens).suggestions
}

pub fn complete<Ctx, T: Parsable<Ctx>>(input: &str, ctx: Ctx) -> BTreeSet<Cow<'static, str>> {
    let tokens = TokenStream::new(input);
    T::new_parser(ctx).complete(tokens).suggestions
}
