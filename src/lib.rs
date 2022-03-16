#![doc(html_root_url = "https://docs.rs/cmdparse/0.1.0")]
#![warn(missing_docs)]

//! `cmdparse` is, as the name suggests, parses user commands into arbitrary Rust types.
//!
//! Generally, this crate can be viewed as a data deserialization framework. It defines a syntax
//! designed to be easy to entered interactively and includes utilities for transforming the input
//! in this format into arbitrary Rust types, as well as automatically suggesting completions for
//! incomplete user input.
//!
//! It is not suitable for parsing command line arguments, even though the syntax it supports is
//! fairly similar to what those would look like. Instead, it was designed to be used for parsing
//! commands entered interactively inside the application. Of course, you are not limited to this
//! use case and free to use `cmdparse` as a generic data deserialization framework in any way
//! you like.
//!
//! # Examples
//!
//! Let’s consider the following example. It defines a struct `MailSendCommand` and derives
//! [`Parsable`] trait for it. This is enough to be able to parse it.
//!
//! ```
//! use cmdparse::{Parsable, parse};
//!
//! #[derive(Debug, PartialEq, Eq, Parsable)]
//! struct MailSendCommand {
//!    text: String,
//!    #[cmd(attr(subject), default = "\"no subject\".to_string()")]
//!    subject: String,
//!    #[cmd(attr(to))]
//!    to: Vec<String>,
//! }
//!
//! # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
//! let input = "\"Hello, world\" --to user1@example.com user2@example.com --subject Greeting";
//! let result = parse::<_, MailSendCommand>(input, ())?;
//! assert_eq!(result, MailSendCommand {
//!     text: "Hello, world".to_string(),
//!     subject: "Greeting".to_string(),
//!     to: vec!["user1@example.com".to_string(), "user2@example.com".to_string()],
//! });
//! # Ok(())
//! # }
//! ```
//!
//! This example demonstrates several features of `cmdparse`:
//!
//!  * Parsing functionality can be automatically derived for an arbitrary struct or enum as long
//!    as the inner types are [`Parsable`] or there is an appropriate [`Parser`] for them. (To
//!    learn about the distinction between parsable and parser, read documentation for these traits).
//!  * Derived parser is configurable: you may make fields either required or optional. Optional
//!    fields can be specified via a name attribute (`--` token). They can have a default value
//!    explicitly specified (see default attribute on the `subject` field) or not (`to` field
//!    defaults to an empty vector, as per its [`Default`] implementation)
//!  * Parsable values can contain nested parsable values: `MailSendCommand` is parsable, it
//!    contains a [`Vec`] which is parsable and in repeatedly parses [`String`]s that are parsable.
//!    Note how `cmdparse` recognized that the list of email addresses finished when it
//!    encountered the attribute that neither [`String`] nor [`Vec`] recognizes.
//!
//! `cmdparse` can generate completion suggestions:
//!
//! ```
//! # use cmdparse::{Parsable, parse};
//! use cmdparse::complete;
//! use std::collections::BTreeSet;
//!
//! # #[derive(Debug, PartialEq, Eq, Parsable)]
//! # struct MailSendCommand {
//! #    text: String,
//! #    #[cmd(attr(subject), default = "\"no subject\".to_string()")]
//! #    subject: String,
//! #    #[cmd(attr(to))]
//! #    to: Vec<String>,
//! # }
//! # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
//! let suggestions = complete::<_, MailSendCommand>("\"Hello, world\" --", ());
//! assert_eq!(suggestions, BTreeSet::from(["to".into(), "subject".into()]));
//! # Ok(())
//! # }
//! ```
//!
//! It also supports parsing enums. In case of enum, it expects a discriminator (automatically
//! converted into kebab-case by the [`Parsable`] derive macro):
//!
//! ```
//! use cmdparse::{parse, Parsable};
//!
//! #[derive(Debug, PartialEq, Eq, Parsable)]
//! enum Priority {
//!    High,
//!    Medium,
//!    Low,
//! }
//!
//! impl Default for Priority {
//!     fn default() -> Self {
//!         Priority::Medium
//!     }
//! }
//!
//! #[derive(Debug, PartialEq, Eq, Parsable)]
//! enum Command {
//!     AddTask(String, #[cmd(attr(priority))] Priority),
//!     Remove(usize),
//! }
//!
//! # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
//! assert_eq!(
//!     parse::<_, Command>("add-task parse-all-commands", ())?,
//!     Command::AddTask("parse-all-commands".to_string(), Priority::Medium),
//! );
//! assert_eq!(
//!     parse::<_, Command>("add-task enjoy-your-day --priority high", ())?,
//!     Command::AddTask("enjoy-your-day".to_string(), Priority::High),
//! );
//! assert_eq!(parse::<_, Command>("remove 1", ())?, Command::Remove(1));
//! # Ok(())
//! # }
//! ```
//!
//! # Syntax
//!
//! The syntax that `cmdparse` supports is fairly minimal. The parsing machinery sees the input as
//! a sequence of tokens. Token is any sequence of characters separated by whitespaces. If you wish
//! to include a whitespace in the token, you may enclose any substring of the input into a pair of
//! quotation marks (either double or singular); `cmdparse` supports escaping these symbols
//! inside quoted tokens with a slash (`\`).
//!
//! Input can contain a comment beginning with an octothorp (`#`). Octothorps within quoted tokens
//! are not considered beginning a comment.
//!
//! The meaning of the token and attributes are highly specific to each parser. Generally, each
//! parser consumes tokens sequentially until each required field’s value is filled. It also
//! handles attributes in any order and at arbitrary positions.
//!
//! Due to the nature of the commands' syntax, parsing can seem ambiguous. For example,
//! `cmdparse` can parse nested structs such as `Vec<Vec<u32>>`. It may be confusing to the end
//! user, how would a sequence of numbers be interpreted (they all will be put in the only item of
//! the outer vector). It is best to design your command to be simple and avoid highly nested
//! structures for the better user experience. In some cases, complexity is unavoidable. In such
//! situations, users may find useful an ability to group tokens, belonging to the same data
//! structure, with parenthesis: `(` and `)`. This way, users can express a value `vec![vec![1, 2],
//! vec![3, 4, 5]]` as `(1 2) (3 4 5)`.
//!
//! More details about how the tokenization and the parsing algorithm are documented in the
//! [`tokens`] module’s and [`Parser`] trait’s documentation.
pub mod error;
pub mod parsers;
pub mod testing;
pub mod tokens;

#[doc(hidden)]
pub use cmdparse_derive::Parsable;
use error::ParseError;
use error::ParseFailure;
use std::borrow::Cow;
use std::collections::BTreeSet;
use tokens::TokenStream;

/// The result value returned by the individual parsers
///
/// [`Parser::parse`] either succeeds, in which case it returns a value that was parsed and the
/// remaining tokens in the token stream, or fails with a [`ParseFailure`]. This type alias
/// represents the return value.
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
    /// The token stream with which the parser should continue the completion process
    ///
    /// If `remaining` is `None` the completion process should stop. This indicates that the last
    /// token was encountered, suggestions for it was computed and there is no ambiguity whether
    /// this token should be consumed by this parser or any other parser. Also, remaining is set to
    /// `None` in case of an invalid input such that completion is not possible.
    pub remaining: Option<TokenStream<'a>>,

    /// Indicates whether at least one token was consumed by the parser
    ///
    /// Some parsers may behave differently whether the nested parser recognizes any token or not.
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
    /// Creates a new `CompletionResult` that indicates that the completion process is finished.
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

    /// Creates a new `CompletionResult` that allows the continuation of the completion process.
    ///
    /// This constructor sets the `remaining` and `value_consumed` fields equal to the first and
    /// second attribute respectively.
    pub fn new(remaining: TokenStream<'a>, consumed: bool) -> Self {
        CompletionResult {
            remaining: Some(remaining),
            value_consumed: consumed,
            suggestions: BTreeSet::new(),
        }
    }

    /// Updates the `value_consumed` status of the `CompletionResult`.
    pub fn set_consumed(mut self, consumed: bool) -> Self {
        self.value_consumed = consumed;
        self
    }

    /// Extends the set of completion suggestions.
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
/// This trait is fundamental for the functionality of `cmdparse`. The implementers must define
/// two operations: parsing (converting the input [`TokenStream`] into a value of a target type)
/// and completion (generating the set of possible completions for the last meaningful token in the
/// input stream).
///
/// Most often, the types being parsed are compound, meaning they contain multiple fields with
/// different parsers. It is best to keep parsers as simple as possible and delegate most of the
/// work to the child parsers. To ensure correct interaction between parsers, custom
/// implementations must follow the parsing protocol. The rules are described in the documentation
/// for each of the `Parser`'s methods.
///
/// Please note, that in most cases writing the parser by hand isn't necessary. Parser is
/// automatically generated for any type that derives [`Parsable`]. The name of the generated
/// parser is constructed by appending the word Parser to the end of the type.
///
/// # Context
///
/// The `Parser` trait is generic over an arbitrary context. Context is passed as an argument to both
/// of the `Parser`'s methods and is intended to make parsers configurable, meaning their behavior
/// can depend on some information available at runtime.
///
/// The following example demonstrates how to implement the parser for a variant-like data,
/// dependent on data available at runtime.
///
/// ```
/// use cmdparse::{Parser, CompletionResult, ParseResult, parse_parser, complete_parser};
/// use cmdparse::tokens::{TokenStream, Token};
/// use cmdparse::error::{ParseError, UnrecognizedToken};
/// use std::borrow::Cow;
/// use std::collections::{BTreeSet, HashMap};
///
/// struct RuntimeContext { variables: HashMap<String, u32> }
///
/// #[derive(Default)]
/// struct VariableParser;
///
/// impl<'c> Parser<&'c RuntimeContext> for VariableParser {
///     type Value = u32;
///
///     fn parse<'a>(&self, input: TokenStream<'a>, ctx: &'c RuntimeContext) -> ParseResult<'a, Self::Value> {
///         match input.take().transpose()? {
///             None => Err(ParseError::token_required().expected("variable").into()),
///             Some((attr @ Token::Attribute(_), remaining)) => {
///                 Err(UnrecognizedToken::new(attr, remaining).into())
///             }
///             Some((token @ Token::Text(text), remaining)) => {
///                 let text = text.parse_string();
///                 match ctx.variables.get(&text as &str) {
///                     Some(value) => Ok((*value, remaining)),
///                     None => Err(UnrecognizedToken::new(token, remaining).into()),
///                 }
///             }
///         }
///     }
///
///     fn complete<'a>(&self, input: TokenStream<'a>, ctx: &'c RuntimeContext) -> CompletionResult<'a> {
///         match input.take() {
///             Some(Err(_)) | None => CompletionResult::new_final(false),
///             Some(Ok((Token::Attribute(_), _))) => CompletionResult::new(input, false),
///             Some(Ok((Token::Text(text), remaining))) if remaining.is_all_consumed() => {
///                 let text = text.parse_string();
///                 CompletionResult::new_final(true).add_suggestions(
///                     ctx.variables.keys()
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
/// Parser implementation should be as generic as possible to avoid type errors when integrating
/// with other parsers.
pub trait Parser<Ctx>: Default {
    /// The type that this parser will parse the input stream into.
    type Value;

    /// Parsers the beginning of the token stream into a `Value`.
    ///
    /// This function performs the parsing of the input stream: it repeatedly consumes tokens from
    /// the token stream and then produces one of the following return values:
    ///  * `Ok((value, remaining))` in case of the correctly parsed sequence of tokens. Here
    ///    `value` is the result of the parsing, (it has type `Self::Value`), and remaining is the
    ///    token stream representing the set of tokens that wasn’t consumed;
    ///  * `Err(ParseFailure::Error(error))` in case the parser failed with an error indicating the
    ///    malformed input. See [`ParseError`];
    ///  * `Err(ParseFailure::Unexpected(unexpected_token))` if the first token in the input stream
    ///    is an attribute or an enum variant discriminator that the parser does not recognize.
    ///
    /// To be interoperable with other parsers, the parse implementation must follow the parsing
    /// protocol:
    ///  * if the first token in the input stream is an attribute and the parser does not recognize
    ///    this attribute, it should return `Err(UnexpectedToken::new(token, remaining).into())`
    ///    where `token` is the attribute that was not recognized, and `remaining` is the token
    ///    stream consisting of tokens directly following token;
    ///  * if the parser expects the enum variant discriminator and the first token of the input is
    ///    not recognized as such, it should return `Err(UnexpectedToken::new(token,
    ///    remaining).into())` with the same values as described above;
    ///  * the parser must not return [`UnexpectedToken`] result with any token other than the first
    ///    token of the input stream; if it receives this value from an inner parser, it must convert
    ///    it into the equivalent error if the parser was not called on the original input;
    ///  * when all required tokens are successfully consumed parser should continue to take tokens
    ///    until a text token or an attribute that is not recognized is encountered (this is not
    ///    necessary if parser does not expect attributes)
    fn parse<'a>(&self, input: TokenStream<'a>, ctx: Ctx) -> ParseResult<'a, Self::Value>;

    /// Constructs the completion suggestions for the last token of the input stream
    ///
    /// It returns a [`CompletionResult`] which contains a set of suggestions and metadata
    /// instructing how the parent parser should proceed with suggestions generation. If the parser
    /// calls multiple parsers' `complete` in a row, it should collect the results they produce.
    ///
    /// Note, that `complete` produces suggestions for the last token in the stream (if and
    /// only if there are no characters following it, so only for tokens for which the remaining
    /// stream's [`is_all_consumed`](TokenStream::is_all_consumed) returns `true`) or when the end
    /// of stream is reached.
    ///
    /// Parser should return:
    ///
    /// | Return value                             | Scenario                                                                                                                                                                                                                                                           |
    /// |------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
    /// | `CompletionResult::new_final(false)`     | The end of the stream is reached, or invalid punctuation is encountered, or the processes cannot continue due to input being invalid sufficiently so that it is not possible to derive its structure.                                                              |
    /// | `CompletionResult::new_final(true)`      | The last token is reached and the entirety of the input string is consumed. Parser should include completions for this token if possible.                                                                                                                          |
    /// | `CompletionResult::new(input, false)`    | The token is the first in the token stream and the parser does not recognize it (following the same protocol described in the documentation for [`parse`](Parser::parse) method). Parser should include completion suggestions that would make token recognizable. |
    /// | `CompletionReuslt::new(remaining, true)` | The parser successfully consumed all tokens that are required for parsing the value. `remaining` must start with the first non-consumed token, if any. The parser should include suggestions for that non-consumed token.                                          |
    fn complete<'a>(&self, input: TokenStream<'a>, ctx: Ctx) -> CompletionResult<'a>;
}

/// Sets the default parser for a given type
///
/// This trait allows the users of a type to avoid specifying the parser explicitly.
///
/// This trait can be procedurally derived for any struct or enum if all its inner types are
/// Parsable or have explicit parser specified.
///
/// # Derive macro
///
/// The `Parsable` derive macro accepts attributes that modify parsing behavior. These attributes
/// are specified in the form `#[cmd(...)]` attributes can be specifed in the same parenthesis
/// separated by commas or separately: `#[cmd(default, attr(field))]` and `#[cmd(default)]
/// #[cmd(attr(field))]` are equivalent.
///
/// ## Type attribute
///
/// The following attributes are applied to the entire struct or enum for which the trait is being
/// derived.
///
/// ### `ctx = "type-name"`, `ctx_bound = "trait-names"`
///
/// Restricts the type of the parsing context in case of ctx attribute or bounds the generic
/// parsing context to the specific trait or collection of traits in case `ctx_bound` attribute is
/// used. This is needed when one or more inner parser restricts the type of the context it uses.
///
/// The following example demonstrates the creation of a custom parser that requires a specific
/// parsing context and restricting the context type in the derived trait implementation.
///
/// ```
/// use cmdparse::{parse, tokens::TokenStream, CompletionResult, Parsable, Parser, ParseResult};
///
/// #[derive(Debug, Copy, Clone, PartialEq, Eq)]
/// enum LengthUnit { Cm, In }
///
/// #[derive(Clone)]
/// struct ParsingContext {
///     unit: LengthUnit,
/// }
///
/// #[derive(Debug, PartialEq)]
/// struct Length(f64, LengthUnit);
///
/// #[derive(Default)]
/// struct LengthParser;
///
/// impl Parser<ParsingContext> for LengthParser {
///     type Value = Length;
///
///     fn parse<'a>(&self, input: TokenStream<'a>, ctx: ParsingContext) -> ParseResult<'a, Self::Value> {
///         let unit = ctx.unit;
///         let parser = <f64 as Parsable<ParsingContext>>::Parser::default();
///         let (value, remaining) = parser.parse(input, ctx)?;
///         Ok((Length(value, unit), remaining))
///     }
///
///     fn complete<'a>(&self, input: TokenStream<'a>, ctx: ParsingContext) -> CompletionResult<'a> {
///         let parser = <f64 as Parsable<ParsingContext>>::Parser::default();
///         parser.complete(input, ctx)
///     }
/// }
///
/// impl Parsable<ParsingContext> for Length {
///     type Parser = LengthParser;
/// }
///
/// #[derive(Debug, PartialEq, Parsable)]
/// #[cmd(ctx = "ParsingContext")]
/// struct Size {
///     height: Length,
///     width: Length,
/// }
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// assert_eq!(
///     parse::<_, Size>("10 20", ParsingContext{ unit: LengthUnit::Cm })?,
///     Size {
///         height: Length(10.0, LengthUnit::Cm),
///         width: Length(20.0, LengthUnit::Cm)
///     }
/// );
/// # Ok(())
/// # }
/// ```
///
/// ## Field attributes
///
/// The following attributes can be used for the struct’s or enum variant’s fields.
///
/// ### `parser = "parser-type-name"`
///
/// Specifies a custom parser used for a field.
///
/// This attribute is useful in situations where the required parser is different from the parser
/// defined by the `Parsable` trait (which is used by default) or when implementation of `Parsable`
/// is not possible (e.g. when dealing with types defined in a foreign crate).
///
/// The following example demonstrates how to use `TransformParser` for data validation.
///
/// ```
/// use cmdparse::parsers::{TransformParser, ParsableTransformation};
/// use cmdparse::error::ParseError;
/// use cmdparse::Parsable;
///
/// struct Number01RangeValidator;
///
/// impl ParsableTransformation<f64> for Number01RangeValidator {
///     type Input = f64;
///
///     fn transform(input: Self::Input) -> Result<f64, ParseError<'static>> {
///         if input < 0.0 || input >= 1.0 {
///             Err(ParseError::custom("must be between 0 and 1"))
///         } else {
///             Ok(input)
///         }
///     }
/// }
///
/// #[derive(Debug, Parsable)]
/// struct Point(
///     #[cmd(parser = "TransformParser<<f64 as Parsable<CmdParserCtx>>::Parser, Number01RangeValidator, f64>")] f64,
///     #[cmd(parser = "TransformParser<<f64 as Parsable<CmdParserCtx>>::Parser, Number01RangeValidator, f64>")] f64,
/// );
/// ```
///
/// ### `default` or `default = "value"` without `attr`
///
/// If the `default` attribute is used on a field, this field will not be parsed. Instead, when
/// constructing the containing instance, the parser uses a default value (if value is not
/// specified) or a specific value (specified after `=` sign).
///
/// ```
/// use cmdparse::{Parsable, parse};
///
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// struct MyStruct(#[cmd(default)] u8, #[cmd(default = "5")] u8, u8);
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// assert_eq!(parse::<_, MyStruct>("24", ())?, MyStruct(0, 5, 24));
/// # Ok(())
/// # }
/// ```
///
/// ### `attr(attribute = "value")` or `attr(attribute)`
///
/// Indicates that the field is optional, it can be specified by the user using a named attribute.
/// This attribute comes in two variants: when “value” is specified, the field’s value is taken
/// from the expression in the attribute, otherwise the attribute token must be followed by the
/// field value’s tokens.
///
/// ```
/// use cmdparse::{Parsable, parse};
///
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// enum Color{ Red, Green, Blue }
///
/// impl Default for Color {
///     fn default() -> Self {
///         Color::Green
///     }
/// }
///
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// struct MyStruct {
///     #[cmd(attr(important = "true"))] is_important: bool,
///     #[cmd(attr(color))] color: Color,
/// }
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// assert_eq!(
///     parse::<_, MyStruct>("--important", ())?,
///     MyStruct { color: Color::Green, is_important: true },
/// );
/// assert_eq!(
///     parse::<_, MyStruct>("--color red", ())?,
///     MyStruct { color: Color::Red, is_important: false },
/// );
/// # Ok(())
/// # }
/// ```
///
/// #### In combination with `default = "value"`
///
/// If an optional field’s value is not specified, the default value is used instead, as determined
/// by the implementation of `Default` trait. This can be overridden by specifying a default value
/// using `default` attribute.
///
/// ```
/// use cmdparse::{Parsable, parse};
///
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// struct MyStruct(#[cmd(default = "5", attr(value))] u8);
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// assert_eq!(parse::<_, MyStruct>("--value 10", ())?, MyStruct(10));
/// assert_eq!(parse::<_, MyStruct>("", ())?, MyStruct(5));
/// # Ok(())
/// # }
/// ```
///
/// ### `alias_value(alias = "alias", value="value")`
///
/// Used for enum variant’s fields. Specifies the value for a field if the specific alias is used
/// as enum’s discriminator. An `alias` can be either a name of a variant (converted into
/// kebab-case), a renamed variant name (via `rename` attribute), or an alias defined using `alias`
/// attribute.
///
/// ```
/// use cmdparse::{Parsable, parse};
///
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// enum MyEnum {
///     #[cmd(alias = "enable", alias = "disable")]
///     SetEnabled(
///         #[cmd(
///             alias_value(alias = "enable", value = "true"),
///             alias_value(alias = "disable", value = "false")
///         )] bool
///     )
/// }
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// assert_eq!(parse::<_, MyEnum>("enable", ())?, MyEnum::SetEnabled(true));
/// assert_eq!(parse::<_, MyEnum>("disable", ())?, MyEnum::SetEnabled(false));
/// # Ok(())
/// # }
/// ```
///
/// ## Enum variant attributes
///
/// These attributes are applicable to enum variants. Generally, `cmdparse` expects a
/// discriminator—the variant’s name in kebab-case followed by tokens for its fields if any exist.
///
/// ### `rename = "name"`
///
/// Changes the name of the variant’s discriminator. The variant cannot be parsed using its
/// original name.
///
/// ```
/// use cmdparse::{Parsable, parse};
///
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// enum MyEnum {
///     #[cmd(rename = "first")] One,
///     #[cmd(rename = "second")] Two,
/// }
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// assert_eq!(parse::<_, MyEnum>("first", ())?, MyEnum::One);
/// assert!(parse::<_, MyEnum>("one", ()).is_err());
/// # Ok(())
/// # }
/// ```
///
/// ### `alias = "alias"`
///
/// Adds an alias for the variant. Variant can have an arbitrary number of aliases and the value
/// can be parsed using any of this. Specifying an alias does not prevent the usage of the
/// variant’s original name.
///
/// ```
/// use cmdparse::{Parsable, parse};
///
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// enum Color {
///     Black,
///     White,
///     #[cmd(alias = "grey")] Gray,
/// }
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// assert_eq!(parse::<_, Color>("grey", ())?, Color::Gray);
/// assert_eq!(parse::<_, Color>("gray", ())?, Color::Gray);
/// # Ok(())
/// # }
/// ```
///
/// ### `ignore`
///
/// Disables the parsing of the variant. Note that this does not prevent assigning aliases to the
/// variant.
///
/// ```
/// use cmdparse::{Parsable, parse};
///
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// enum MyEnum {
///     Command,
///     #[cmd(ignore)] NonInteractive,
/// }
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// assert!(parse::<_, MyEnum>("non-interactive", ()).is_err());
/// # Ok(())
/// # }
/// ```
///
/// ### `transparent`
///
/// Indicates that a variant can be parsed without a discriminator. This can be used when splitting
/// a large enum into several smaller ones is desirable.
///
/// ```
/// use cmdparse::{Parsable, parse};
///
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// enum Subcommand { First, Second }
///
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// enum Command {
///     #[cmd(transparent)]
///     Subcommand(Subcommand),
///     Third,
/// }
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// assert_eq!(parse::<_, Command>("first", ())?, Command::Subcommand(Subcommand::First));
/// assert_eq!(parse::<_, Command>("third", ())?, Command::Third);
/// # Ok(())
/// # }
/// ```
///
/// ### `transparent_no_error`
///
/// Functions similarly to `transparent` but does not terminate parsing on failure. It is useful
/// when the first field of this variant is not an enum.
///
/// ```
/// use cmdparse::{Parsable, parse};
///
/// #[derive(Debug, PartialEq,Parsable)]
/// enum Value {
///     #[cmd(transparent_no_error)] Integer(i64),
///     #[cmd(transparent_no_error)] Real(f64),
///     #[cmd(transparent_no_error)] Boolean(bool),
/// }
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// assert_eq!(parse::<_, Value>("0.4", ())?, Value::Real(0.4));
/// assert_eq!(parse::<_, Value>("12", ())?, Value::Integer(12));
/// assert_eq!(parse::<_, Value>("true", ())?, Value::Boolean(true));
/// # Ok(())
/// # }
/// ```
///
/// Note that in the example above, the orders in which the enum variants are declared matters:
/// `cmdparse` tries to parse transparent variants in order in which they are declared and
/// returns the first successfully parsed result.
pub trait Parsable<Ctx> {
    /// The parser type for this type
    type Parser: Parser<Ctx, Value = Self>;
}

/// Parsers a value from an input string using an explicitly specified parser
///
/// This function takes an input string slice as an input and a context, and returns a value parsed
/// from the input or an error. `parse` ensures that all tokens from the input string were
/// consumed, and the input is valid.
///
/// This function is different from [`parse`] in that is expects a parser as its second generic
/// parameter. The value returned by `parse_parser` does not need to implement [`Parsable`].
/// `parse_parser` most commonly used with custom parsers.
///
/// # Example:
///
/// ```
/// use cmdparse::parse_parser;
/// use cmdparse::parsers::{IntegerParser, StringParser, tuples::TupleParser2};
///
/// type ExplicitParser = TupleParser2<IntegerParser<u64>, StringParser>;
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// let value = parse_parser::<_, ExplicitParser>("42 fourty-two", ())?;
/// assert_eq!(value, (42, "fourty-two".to_string()));
/// # Ok(())
/// # }
/// ```
pub fn parse_parser<Ctx, P: Parser<Ctx>>(input: &str, ctx: Ctx) -> Result<P::Value, ParseError> {
    let tokens = TokenStream::new(input);
    match P::default().parse(tokens, ctx) {
        Ok((result, remaining)) => match remaining.peek() {
            Some(Ok(token)) => Err(ParseError::unknown(token)),
            Some(Err(err)) => Err(err.into()),
            None => Ok(result),
        },
        Err(ParseFailure::Error(err)) => Err(err),
        Err(ParseFailure::Unrecognized(unrecognized)) => Err(unrecognized.into_error()),
    }
}

/// Parsers a [`Parsable`] value from an input string
///
/// This function takes an input string slice as an input and a context, and returns a value parsed
/// from the input or an error. `parse` ensures that all tokens from the input string were
/// consumed, and the input is valid.
///
/// # Example:
///
/// ```
/// use cmdparse::parse;
///
/// # fn main() -> Result<(), cmdparse::error::ParseError<'static>> {
/// let value: (u64, String) = parse("42 fourty-two", ())?;
/// assert_eq!(value, (42, "fourty-two".to_string()));
/// # Ok(())
/// # }
/// ```
pub fn parse<Ctx, T: Parsable<Ctx>>(input: &str, ctx: Ctx) -> Result<T, ParseError> {
    parse_parser::<_, T::Parser>(input, ctx)
}

/// Computes the completion suggestions for a value using an explicit parser
///
/// `compute_parser` takes an input as a string slice with a parsing context and returns a set of
/// completion suggestions for the last token in the input, if any.
///
/// This function is similar to [`complete`] but is expects a parser as its second generic parameter.
/// It is intended to be used with custom parsers or for types that don’t implement [`Parsable`].
///
/// # Examples
///
/// ```
/// use cmdparse::complete_parser;
/// use cmdparse::parsers::BooleanParser;
/// use std::collections::BTreeSet;
///
/// let suggestions = complete_parser::<_, BooleanParser>("tr", ());
/// assert_eq!(suggestions, BTreeSet::from(["ue".into()]));
/// ```
pub fn complete_parser<Ctx, P: Parser<Ctx>>(input: &str, ctx: Ctx) -> BTreeSet<Cow<'static, str>> {
    let tokens = TokenStream::new(input);
    P::default().complete(tokens, ctx).suggestions
}

/// Computes the completion suggestiosns for a `Parsable` value
///
/// `compute` takes an input as a string slice with a parsing context and returns a set of
/// completion suggestions for the last token in the input if any.
///
/// # Examples
///
/// ```
/// use cmdparse::complete;
/// use std::collections::BTreeSet;
///
/// let suggestions = complete::<_, bool>("tr", ());
/// assert_eq!(suggestions, BTreeSet::from(["ue".into()]));
/// ```
pub fn complete<Ctx, T: Parsable<Ctx>>(input: &str, ctx: Ctx) -> BTreeSet<Cow<'static, str>> {
    let tokens = TokenStream::new(input);
    T::Parser::default().complete(tokens, ctx).suggestions
}
