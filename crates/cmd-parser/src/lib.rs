pub mod error;
pub mod primitives;
pub mod sequences;
pub mod testing;
pub mod tokens;
pub mod utils;

use std::borrow::Cow;
use std::collections::HashSet;

pub use cmd_parser_derive::Parsable;
pub use error::ParseFailure;
use tokens::TokenStream;

type ParseResult<'a, T> = Result<(T, TokenStream<'a>), ParseFailure<'a>>;

#[derive(Debug)]
pub struct CompletionResult<'a> {
    remaining: Option<TokenStream<'a>>,
    value_consumed: bool,
    suggestions: HashSet<Cow<'static, str>>,
}

impl<'a> CompletionResult<'a> {
    fn unrecognized(previous: TokenStream<'a>) -> Self {
        CompletionResult {
            remaining: Some(previous),
            value_consumed: false,
            suggestions: HashSet::new(),
        }
    }

    fn consumed(remaining: TokenStream<'a>) -> Self {
        CompletionResult {
            remaining: Some(remaining),
            value_consumed: true,
            suggestions: HashSet::new(),
        }
    }

    fn complete(suggestions: HashSet<Cow<'static, str>>) -> Self {
        CompletionResult {
            remaining: None,
            value_consumed: true,
            suggestions,
        }
    }

    fn failed() -> Self {
        CompletionResult {
            remaining: None,
            value_consumed: false,
            suggestions: HashSet::new(),
        }
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
