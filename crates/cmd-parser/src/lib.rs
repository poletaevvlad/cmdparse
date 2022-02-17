pub mod error;
pub mod parsers;
pub mod testing;
pub mod tokens;
pub mod utils;

use std::borrow::Cow;
use std::collections::BTreeSet;

#[doc(hidden)]
pub use cmd_parser_derive::Parsable;
use error::ParseError;
use error::ParseFailure;
use tokens::TokenStream;

/// The result value returned by the individual parsers
///
/// [`Parser::parse`] either succeedes inwhich case it returns a value that was parsed and the
/// remaining tokens in the token stream, or fails with a [`ParseFailure`]. This type alias
/// represents the return value .
pub type ParseResult<'a, T> = Result<(T, TokenStream<'a>), ParseFailure<'a>>;

/// Completion suggestions and processing metadata
///
/// `CompletionResult` are returned by the [`Parser::complete`] method and are used to indicate
/// what completions the parser suggests and how other parsers should proceed with the completion
/// process.
///
/// See the documentation for the [`Parser::complete`] method for more information on how values of
/// this type should be created by the parser.
#[derive(Debug)]
pub struct CompletionResult<'a> {
    /// The token stream with thich the parser should continue the completion process
    ///
    /// If `remaining` is [`None`] the completion processing should stop. This indicates that the
    /// last token was encountered and suggestions for it was determined and there is no ambiguity
    /// whether this token would be consumed by this parser or any other parser. Also, `remaining`
    /// is set to [`None`] in case of an invalid input such that completion is not possible.
    pub remaining: Option<TokenStream<'a>>,

    /// Indicates wheather at least one token was consumed by the parser
    ///
    /// Some parsers may behave diffirently whether the nested parser recognizes any token or not.
    /// For example if `value_consumed` is `false` the parser may try to handle an attribute that
    /// is not recognized by the inner parser but may be recognized by the current parser or the
    /// parent one.
    pub value_consumed: bool,

    /// The set of suggestions for the last token of a stream
    ///
    /// Multiple inner parsers can return suggestions for the same token. Parser should combine all
    /// of these suggestions into one set.
    pub suggestions: BTreeSet<Cow<'static, str>>,
}

impl<'a> CompletionResult<'a> {
    /// Creates a new `CompletionResult` that indicates that the completion process is finished
    ///
    /// This constructor sets `remaining` as None, and `value_consumed` equal to the `consumed`
    /// attribute.
    pub fn new_final(consumed: bool) -> Self {
        CompletionResult {
            remaining: None,
            value_consumed: consumed,
            suggestions: BTreeSet::new(),
        }
    }

    /// Creates a new `CompletionResult` that allows the continuation of the completion process
    ///
    /// This constructor sets the `remaining` and `value_consumed` fields equal to the first and
    /// second attribute respectively
    pub fn new(remaining: TokenStream<'a>, consumed: bool) -> Self {
        CompletionResult {
            remaining: Some(remaining),
            value_consumed: consumed,
            suggestions: BTreeSet::new(),
        }
    }

    /// Updates the `value_consumed` status of the `CompletionResult`
    pub fn set_consumed(mut self, consumed: bool) -> Self {
        self.value_consumed = consumed;
        self
    }

    /// Extends the set of completion suggestions
    pub fn add_suggestions(
        mut self,
        suggestions: impl IntoIterator<Item = Cow<'static, str>>,
    ) -> Self {
        self.suggestions.extend(suggestions.into_iter());
        self
    }
}

/// Definition of the parsing and completion algorithm for some type
///
/// This trait is fundamental for the functionality of `cmd_parser`. The implementers must define
/// two operations: parsing (converting the input [`TokenStream`] into a value of a target type)
/// and completion (generating the set of possible completions for the last meaningfull token in
/// the input stream).
///
/// Most often the types being parsed are compound, meaning they contain multiple fields with
/// different parsers. It is best to keep parsers as simple as possible and delegate most of the
/// work to the child parsers. To ensure correct interaction between parsers custom implementation
/// must follow the parsing protocol. The detailed rules are described in the documentation for
/// each of the methods.
///
/// Please note, that in most cases writing the parser by hand is not necessary. `Parser` is
/// automatically generated for any type that derives [`Parsable`]. The name of the generated
/// parser is constructed by appending the word `Parser` to the end of the type.
///
/// # Context
///
/// The `Parser` trait is generic over an arbitrary context. Context is passed into the constructor
/// of each parser and is intended to make parsers configurable, meaning their behavior can depend
/// on some information available at runtime.
///
/// The following example demonstrates how to implement the parser for a variant-like data
/// dependant on the runtime data.
///
/// ```
/// use cmd_parser::{Parser, CompletionResult, ParseResult, parse_parser, complete_parser};
/// use cmd_parser::tokens::{TokenStream, Token};
/// use cmd_parser::error::{ParseError, UnrecognizedToken};
/// use std::borrow::Cow;
/// use std::collections::{BTreeSet, HashMap};
///
/// struct RuntimeContext { variables: HashMap<String, u32> }
///
/// struct VariableParser<'c> { ctx: &'c RuntimeContext, }
///
/// impl<'c> Parser<&'c RuntimeContext> for VariableParser<'c> {
///     type Value = u32;
///     
///     fn create(ctx: &'c RuntimeContext) -> Self {
///         VariableParser{ ctx }
///     }
///
///     fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
///         match input.take().transpose()? {
///             None => Err(ParseError::token_required().expected("variable").into()),
///             Some((attr @ Token::Attribute(_), remaining)) => {
///                 Err(UnrecognizedToken::new(attr, remaining).into())
///             }
///             Some((token @ Token::Text(text), remaining)) => {
///                 let text = text.parse_string();
///                 match self.ctx.variables.get(&text as &str) {
///                     Some(value) => Ok((*value, remaining)),
///                     None => Err(UnrecognizedToken::new(token, remaining).into()),
///                 }
///             }
///         }
///     }
///
///     fn complete<'a>(&self, input: TokenStream<'a>) -> CompletionResult<'a> {
///         match input.take() {
///             Some(Err(_)) | None => CompletionResult::new_final(false),
///             Some(Ok((Token::Attribute(_), _))) => CompletionResult::new(input, false),
///             Some(Ok((Token::Text(text), remaining))) if remaining.is_all_consumed() => {
///                 let text = text.parse_string();
///                 CompletionResult::new_final(true).add_suggestions(
///                     self.ctx.variables.keys()
///                         .filter_map(|key| key.strip_prefix(&text as &str))
///                         .map(|suggestion| Cow::Owned(suggestion.to_string()))
///                 )
///             }
///             Some(Ok((Token::Text(_), remaining))) => CompletionResult::new(remaining, true),
///         }
///     }
/// }
///
/// # fn main() -> Result<(), ParseError<'static>> {
/// let context = RuntimeContext {
///     variables: HashMap::from([("var-1".to_string(), 10), ("var-2".to_string(), 20)]),
/// };
///
/// assert_eq!(parse_parser::<_, VariableParser>("var-1", &context)?, 10);
/// assert_eq!(parse_parser::<_, VariableParser>("var-2", &context)?, 20);
/// assert_eq!(
///     complete_parser::<_, VariableParser>("va", &context),
///     BTreeSet::from(["r-1".into(), "r-2".into()]),
/// );
/// # Ok(())
/// # }
/// ```
///
/// Parser implementation should be as generic as possible to avolid type errors when integrating
/// with other parsers.
pub trait Parser<Ctx> {
    /// The type that this parser will parse the input stream into.
    type Value;

    /// Creates a new parser with a given context.
    ///
    /// If the parser with call other parsers, their instances should be created inside the
    /// `create` method. This constructor should pass the context down the tree of parsers.
    ///
    /// ```
    /// # use cmd_parser::{Parser, CompletionResult, ParseResult};
    /// # use cmd_parser::tokens::TokenStream;
    /// struct TupleParser<P> {
    ///     inner_parser: P
    /// }
    ///
    /// impl<Ctx, P: Parser<Ctx>> Parser<Ctx> for TupleParser<P> {
    ///     type Value = (P::Value, P::Value);
    ///     
    ///     fn create(ctx: Ctx) -> Self {
    ///         TupleParser {
    ///             inner_parser: P::create(ctx),
    ///         }
    ///     }
    ///     // ...
    ///     # fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
    ///     #     todo!()
    ///     # }
    ///     #
    ///     # fn complete<'a>(&self, input: TokenStream<'a>) -> CompletionResult<'a> {
    ///     #     todo!()
    ///     # }
    /// }
    /// ```
    ///
    /// If the parser need to pass context to multiple parsers it should constrain the context to
    /// implement [`Clone`].
    fn create(ctx: Ctx) -> Self;

    /// Parsers the begining of the token stream into a `Value`
    ///
    /// This function performs the parsing of the input stream: it repeatedly consumes tokens from
    /// the token stream and then produces one of the following return values:
    ///
    /// 1. `Ok((value, remaining))` in case of the correctly parsed sequence of tokens. Here
    ///    `value` is the result of the parsing and it has type `Self::Value`, and remaining is the
    ///    token stream representing the set of tokens that wasn't consumed
    /// 2. Err(ParseFailure::Error(error)) in case the parser failed with an error indicating the
    ///    malformed input. See [`ParseError`]
    /// 3. Err(ParseFailure::Unexpected(unexpected_token)) if the *first* token in the input stream
    ///    is an attribute or an enum variant discriminator that the parser did not recognize
    ///
    /// To be interoperable with other parsers the `parse` implementation must follow the
    /// parsing protocol:
    ///
    ///  * if the *first* token in the input stream is an attribute and the parser does not
    ///    recognize this attribute, it should return `Err(UnexpectedToken::new(token,
    ///    remaining).into())` where `token` is the attribute that was not recognized, and
    ///    `remaining` is the token stream consisting of tokens following `token`.
    ///  * if the parser expects the enum variant disctiminator and the *first* token of the input
    ///    is not recognized as such, it should return `Err(UnexpectedToken::new(token,
    ///    remaining).into())` with the same values as described above.
    ///  * the parser must not return `UnexpectedToken` result with any token other than the first
    ///    token from the input stream; if it recieves this value from the inner parser, it must
    ///    convert it into the equivalent error if the parser was not called on the original input.
    ///  * when all required tokens has been successfully consumed parser should continue to take
    ///    tokens until a text token or an attribute that is not recognized is encountered
    fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value>;

    /// Constructs the completion suggestions for the last token of the input stream
    ///
    /// It returns a [`CompletionResult`] which contains a set of suggestions and metadata
    /// instructing how the parent parser should proceed with suggestions generation. If the parser
    /// calls multiple parsers's `complete` in a row, it should collect the results they produce.
    ///
    /// Note, that `complete` produces suggestions only for the last token in the stream if and
    /// only if there are no characters following it, so only for tokens for which the remaining
    /// stream's [`is_all_consumed`](TokenStream::is_all_consumed) returns `true`.
    ///
    /// Parser should return:
    ///
    /// | Return value                             | Scenario                                                                                                                                                                                                                |
    /// |------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
    /// | `CompletionResult::new_final(false)`     | The end of the stream is reached or invalid punctuation is encoutered or the processes cannot continue due to input being invalid suffitiently so that it is not possible to derive its structure                       |
    /// | `CompletionResult::new_final(true)`      | The last token is reached and intirety of the input string is consumed. Parser should include completions for this token if possible                                                                                    |
    /// | `CompletionResult::new(input, false)`    | The token is the fist in the token stream and parser does not recognize it (following the same protocol described for `parse` method). Parser should include completion suggestions that would make token recognizable  |
    /// | `CompletionReuslt::new(remaining, true)` | The parser successfully consumed all tokens that are required for parsing the value. `remaining` must start with the first non-consumed token if any. The parser should include suggestions for that non-consumed token |
    fn complete<'a>(&self, input: TokenStream<'a>) -> CompletionResult<'a>;
}

/// Sets the default parser for a given type
///
/// This trait allows the users of a type to avoid specifying the parser explicitly.
///
/// This trait can be proceduraly derived for any struct or enum if all its inner types are
/// `Parsable` or have exlicit parser specified.
pub trait Parsable<Ctx> {
    /// The parser type for this type
    type Parser: Parser<Ctx, Value = Self>;

    /// Creates a new parser of the type `Self::Parser`
    ///
    /// This is equivalent to calling the parser's [`create`](Parser::create) method. This method
    /// is privided for convenience and the implementers should not have a need to override its
    /// behavior.
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

/// Parsers a value from an input string using an explicitly specified parser
///
/// This function takes an input string slice as an input and a context, and returns a value parsed
/// from the input or an error. `parse` ensures that all tokens from the input string were consumed
/// and the input is valid.
///
/// This function is different from [`parse`] in that is expects a parser as its second geeneric
/// parameter. The value returned by `parse_parser` does not need to implement [`Parsable`].
/// `parse_parser` most commonly used with custom parsers.
///
/// # Example:
///
/// ```
/// use cmd_parser::parse_parser;
/// use cmd_parser::parsers::{IntegerParser, StringParser, tuples::TupleParser2};
///
/// type ExplicitParser = TupleParser2<(), IntegerParser<u64>, StringParser>;
/// # fn main() -> Result<(), cmd_parser::error::ParseError<'static>> {
/// let value = parse_parser::<_, ExplicitParser>("42 fourty-two", ())?;
/// assert_eq!(value, (42, "fourty-two".to_string()));
/// # Ok(())
/// # }
/// ```
pub fn parse_parser<Ctx, P: Parser<Ctx>>(input: &str, ctx: Ctx) -> Result<P::Value, ParseError> {
    parse_with_parser(input, P::create(ctx))
}

/// Parsers a [`Parsable`] value from an input string
///
/// This function takes an input string slice as an input and a context, and returns a value parsed
/// from the input or an error. `parse` ensures that all tokens from the input string were consumed
/// and the input is valid.
///
/// # Example:
///
/// ```
/// use cmd_parser::parse;
///
/// # fn main() -> Result<(), cmd_parser::error::ParseError<'static>> {
/// let value: (u64, String) = parse("42 fourty-two", ())?;
/// assert_eq!(value, (42, "fourty-two".to_string()));
/// # Ok(())
/// # }
/// ```
pub fn parse<Ctx, T: Parsable<Ctx>>(input: &str, ctx: Ctx) -> Result<T, ParseError> {
    parse_with_parser(input, T::new_parser(ctx))
}

/// Computes the completion suggestiosns for a value using an explicit parser
///
/// `compute_parser` takes an input as a string slice with a parsing context and returns a set of
/// completion suggestions for the last token in the input if any.
///
/// This function is similar to `complete` but is expects a parser as its second generic parameter.
/// It is intended to be used with custom parsers or for types that don't implement [`Parsable`].
///
/// # Examples
///
/// ```
/// use cmd_parser::complete_parser;
/// use cmd_parser::parsers::BooleanParser;
/// use std::collections::BTreeSet;
///
/// let suggestions = complete_parser::<_, BooleanParser>("tr", ());
/// assert_eq!(suggestions, BTreeSet::from(["ue".into()]));
/// ```
pub fn complete_parser<Ctx, P: Parser<Ctx>>(input: &str, ctx: Ctx) -> BTreeSet<Cow<'static, str>> {
    let tokens = TokenStream::new(input);
    P::create(ctx).complete(tokens).suggestions
}

/// Computes the completion suggestiosns for a [`Parsable`] value
///
/// `compute` takes an input as a string slice with a parsing context and returns a set of
/// completion suggestions for the last token in the input if any.
///
/// # Examples
///
/// ```
/// use cmd_parser::complete;
/// use std::collections::BTreeSet;
///
/// let suggestions = complete::<_, bool>("tr", ());
/// assert_eq!(suggestions, BTreeSet::from(["ue".into()]));
/// ```
pub fn complete<Ctx, T: Parsable<Ctx>>(input: &str, ctx: Ctx) -> BTreeSet<Cow<'static, str>> {
    let tokens = TokenStream::new(input);
    T::new_parser(ctx).complete(tokens).suggestions
}
