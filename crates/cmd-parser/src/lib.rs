mod error;
pub mod primitives;
// pub mod sequences;
pub mod tokens;

use std::borrow::Cow;
use std::collections::HashSet;

// pub use cmd_parser_derive::Parsable;
pub use error::ParseFailure;
use tokens::TokenStream;

type ParseResult<'a, T> = Result<(T, TokenStream<'a>), ParseFailure<'a>>;

type Suggestions = HashSet<Cow<'static, str>>;

#[derive(Debug)]
pub struct CompletionResult<'a> {
    remaining: Option<TokenStream<'a>>,
    value_consumed: bool,
    suggestions: Suggestions,
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
