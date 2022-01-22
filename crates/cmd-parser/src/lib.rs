mod error;
pub mod primitives;
pub mod sequences;
pub mod tokens;

pub use cmd_parser_derive::Parsable;
pub use error::ParseError;
use std::borrow::Cow;

#[derive(Debug, PartialEq)]
pub enum ParseResult<'a, T> {
    Unrecognized(ParseError<'a>),
    Parsed(T, &'a str),
    Failed(ParseError<'a>),
}

impl<'a, T> ParseResult<'a, T> {
    pub fn map<R, F: FnOnce(T) -> R>(self, func: F) -> ParseResult<'a, R> {
        match self {
            ParseResult::Unrecognized(error) => ParseResult::Unrecognized(error),
            ParseResult::Parsed(result, remaining) => ParseResult::Parsed(func(result), remaining),
            ParseResult::Failed(error) => ParseResult::Failed(error),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CompletionResult<'a> {
    Consumed(&'a str),
    Unrecognized,
    Suggestions(Vec<Cow<'static, str>>),
}

impl<'a> CompletionResult<'a> {
    pub(crate) const fn empty() -> Self {
        CompletionResult::Suggestions(Vec::new())
    }
}

pub trait Parser<Ctx> {
    type Value;

    fn create(ctx: Ctx) -> Self;
    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value>;
    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a>;
}

pub trait Parsable<Ctx> {
    type Parser: Parser<Ctx, Value = Self>;

    fn new_parser(ctx: Ctx) -> Self::Parser {
        Self::Parser::create(ctx)
    }
}
