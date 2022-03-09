//! Splitting the input stream into a sequence of tokens
//!
//! `cmd_parser`’s parsers do not work on the input string directly. Instead, they operate on the
//! token stream — an iterator-like sequence of tokens, each representing a text payload or an
//! attribute name (a token with preceding `--`). Token stream does not include whitespaces or
//! comments (substrings beginning from the first octothorp (`#`) in the input). A token may
//! include whitespace characters if its content is surrounded by quotation marks (either `'` or
//! `"`).
//!
//! For example, let’s consider the following string and the sequence of tokens it is going to be
//! parsed into:
//!
//! ```
//! # use cmd_parser::tokens::{TokenStream, Token, RawLexeme, UnbalancedParenthesis};
//! #
//! # fn main() -> Result<(), UnbalancedParenthesis> {
//! let input = r#"send-message --to user@example.com --subject "Hello, \"world\"" # sending an email"#;
//!
//! let mut token_stream = TokenStream::new(input);
//! let mut tokens = Vec::new();
//! while let Some(result) = token_stream.take() {
//!     let (token, stream) = result?;
//!     token_stream = stream;
//!     tokens.push(token);
//! }
//!
//! let expected = vec![
//!     Token::Text(RawLexeme::new("send-message")),
//!     Token::Attribute(RawLexeme::new("to")),
//!     Token::Text(RawLexeme::new("user@example.com")),
//!     Token::Attribute(RawLexeme::new("subject")),
//!     Token::Text(RawLexeme::new(r#""Hello, \"world\"""#)),
//! ];
//! assert_eq!(tokens, expected);
//! # Ok(())
//! # }
//! ```
//!
//! Note the following:
//!  * The token stream is represented by the instance of a [`TokenStream`]. It is immutable
//!    (`take` method returns another instance representing the remainder of the input stream).
//!  * Each [`Token`] can be either `Text` or `Attribute`. All whitespaces and comments are
//!    discarded from the stream.
//!  * The contents of the token is a [`RawLexeme`] &mdash; a thin wrapper around an input slice.
//!    Each [`RawLexeme`] can be parsed into the intended representation:
//! ```
//! # use cmd_parser::tokens::RawLexeme;
//! let lexeme = RawLexeme::new(r#""Hello, \"world\"""#);
//! assert_eq!(&lexeme.parse_string(), r#"Hello, "world""#);
//! ```

mod lexing;
mod stream;

use lexing::Lexeme;
use std::borrow::Cow;
use std::fmt::{self, Write};
pub use stream::TokenStream;

/// A wrapper type for a slice of the input string corresponding to a single lexeme
///
/// RawLexeme holds a string slice containing characters that belong to a single token: either text
/// or the attribute. In case of attributes, leading &ldquo;`--`&rdquo; is not included.
///
/// This struct exists to make direct matches and comparisons intentionally difficult. The token
/// may be enclosed in quotation marks and contain escaped characters, so direct comparisons
/// between input slices may cause unexpected bugs: the token "abc\"def" should be considered equal
/// to abc"def and slice comparisons would not produce such result.
///
/// When a parser implementation needs to access the lexeme’s contents, it should call the
/// [`parse_string`](RawLexeme::parse_string) method.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawLexeme<'a>(&'a str);

impl<'a> RawLexeme<'a> {
    /// Creates a new `RawLexeme` instance
    pub fn new(text: &'a str) -> Self {
        RawLexeme(text)
    }

    /// Parses a lexeme: for a string surrounded by the quotation marks, returns an owned string
    /// without quotation marks and with escaped characters replaced. For a string without quotes,
    /// this function returns a slice reference as is.
    ///
    /// Note that the quotes don’t need to be closed. If absent, the closing quotation mark is
    /// implied.
    pub fn parse_string(self) -> Cow<'a, str> {
        let text = self.0;

        let first_char = text.chars().next();
        if let Some(quote @ '\'' | quote @ '"') = first_char {
            let mut string = String::new();
            let mut escaped = false;
            let text = if text.ends_with(quote) {
                &text[1..text.len() - 1]
            } else {
                &text[1..]
            };
            for ch in text.chars() {
                match ch {
                    ch if escaped => {
                        string.push(ch);
                        escaped = false;
                    }
                    '\\' => escaped = true,
                    ch => string.push(ch),
                }
            }
            Cow::Owned(string)
        } else {
            Cow::Borrowed(text)
        }
    }
}

impl<'a> fmt::Display for RawLexeme<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let first_char = self.0.chars().next();
        if let Some(quote @ '\'' | quote @ '"') = first_char {
            f.write_str(self.0)?;
            if !self.0.ends_with(quote) {
                f.write_char(quote)?;
            }
            Ok(())
        } else {
            f.write_fmt(format_args!("\"{}\"", self.0))
        }
    }
}

/// An item of the token stream
///
/// Values of this type represent the minimal unit containing within a token stream. Each token can
/// represent either an attribute (a lexeme preceded by two consecutive `-` characters) or text (a
/// lexeme without preceding dashes). A lexeme can be either a sequence of any non-whitespace
/// characters or a sequence of any characters enclosed in quotation marks (`"` or `'`). In the
/// latter case, lexemes may contain quotation marks of the same kind as the enclosing one, only if
/// it is preceded by the slash (`\`).
///
/// The following table demonstrates some examples of valid tokens:
///
/// | Token type | Examples                                                                 |
/// |------------|--------------------------------------------------------------------------|
/// | Text       | `text`, `"quoted string"`, `'with single quotes'`, `"with \"escaping\""` |
/// | Attribute  |`--attribute`, `--"attributes can be escaped too"`, `--`                  |
///
/// It two dashes are followed by a whitespace character, an octothorp (`#`) indicating the start
/// of a comment, or parenthesis (either opening or closing), then it's interpreted as a valid
/// empty attribute. The same is not the case for the text tokens. The only way to have an empty
/// text token is to enclose an empty string in quotation marks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
    Text(RawLexeme<'a>),
    Attribute(RawLexeme<'a>),
}

impl<'a> Token<'a> {
    /// Returns a raw lexeme referencing the token's contents, discarding information about its
    /// kind.
    pub fn into_raw_lexeme(self) -> RawLexeme<'a> {
        match self {
            Token::Text(inner) => inner,
            Token::Attribute(inner) => inner,
        }
    }

    /// Returns true if the tokens represents an attribute.
    pub fn is_attribute(&self) -> bool {
        matches!(self, Token::Attribute(_))
    }

    /// Returns true if the tokens represents text.
    pub fn is_text(&self) -> bool {
        matches!(self, Token::Text(_))
    }

    fn from_lexeme(lexeme: Lexeme<'a>) -> Result<Self, UnbalancedParenthesis> {
        match lexeme {
            Lexeme::OpeningParen | Lexeme::ClosingParen => Err(UnbalancedParenthesis),
            Lexeme::Text(text) => Ok(Token::Text(RawLexeme::new(text))),
            Lexeme::Attribute(attr) => Ok(Token::Attribute(RawLexeme::new(attr))),
        }
    }
}

/// An error representing the fact that parenthesis is encountered when trying to take a token from
/// the token stream.
#[derive(Debug)]
pub struct UnbalancedParenthesis;

/// Computes all possible completions for a `token` from the possible complete token set of `variants`
///
/// `variants` **must** be ordered. If `variants` is not ordered, the behavior of this function is
/// unspecified.
///
/// `complete_variants` returns an iterator of string slices from variants that start with `token`
/// and with prefix removed.
///
/// # Example
///
/// ```
/// use cmd_parser::tokens::complete_variants;
///
/// // Note that VARIANTS is sorted
/// const VARIANTS: &[&str] = &["fifth", "first", "fourth", "second", "third"];
///
/// let suggestions: Vec<_> = complete_variants("fi", VARIANTS).collect();
/// assert_eq!(suggestions, vec!["fth", "rst"]);
/// ```
pub fn complete_variants<'a, 'b>(
    token: &'b str,
    variants: &'b [&'a str],
) -> impl Iterator<Item = &'a str> + 'b {
    let index = variants.binary_search(&token).unwrap_or_else(|idx| idx);
    variants[index..]
        .iter()
        .map(move |variant| variant.strip_prefix(token))
        .take_while(Option::is_some)
        .map(|suggestion| suggestion.unwrap()) // Iterator::take_while is unstable, unwrap is safe, Nones are filtered out
        .filter(|suggestion| !suggestion.is_empty())
}

#[cfg(test)]
mod tests {
    use super::RawLexeme;

    mod format_raw_lexeme {
        use super::*;

        #[test]
        fn format_simple() {
            assert_eq!(RawLexeme::new("simple").to_string(), "\"simple\"");
        }

        #[test]
        fn format_quoted() {
            assert_eq!(RawLexeme::new("'quoted'").to_string(), "'quoted'");
            assert_eq!(RawLexeme::new("\"quoted\"").to_string(), "\"quoted\"");
        }

        #[test]
        fn format_quoted_partial() {
            assert_eq!(RawLexeme::new("'quoted").to_string(), "'quoted'");
            assert_eq!(RawLexeme::new("\"quoted").to_string(), "\"quoted\"");
        }
    }

    mod parse_raw_lexeme {
        use super::*;
        use std::borrow::Cow;

        macro_rules! test_parse {
            ($name:ident, $text:literal => $variant:ident($result:literal)) => {
                #[test]
                fn $name() {
                    let result = RawLexeme::new($text).parse_string();
                    assert_eq!(result, Cow::Borrowed($result));
                    assert!(matches!(result, Cow::$variant(_)));
                }
            };
        }

        test_parse!(empty, "" => Borrowed(""));
        test_parse!(non_empty, "abc" => Borrowed("abc"));
        test_parse!(quoted_empty_single, "''" => Owned(""));
        test_parse!(quoted_empty_double, "\"\"" => Owned(""));
        test_parse!(quoted_non_empty_single, "'abc \\\' def \\\" fgh'" => Owned("abc \' def \" fgh"));
        test_parse!(quoted_non_empty_double, "\"abc \\\' def \\\" fgh\"" => Owned("abc \' def \" fgh"));
        test_parse!(quoted_not_terminated_single, "'abc" => Owned("abc"));
        test_parse!(quoted_not_terminated_double, "\"abc" => Owned("abc"));
    }
}
