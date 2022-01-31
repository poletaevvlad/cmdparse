mod lexing;
mod stream;

use crate::error::UnbalancedParenthesis;
use lexing::{Lexeme, LexemeKind};
use std::{
    borrow::Cow,
    fmt::{self, Write},
};
pub use stream::{NestingGuard, TokenStream};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawLexeme<'a>(&'a str);

impl<'a> RawLexeme<'a> {
    pub(crate) fn from_str(text: &'a str) -> Self {
        RawLexeme(text)
    }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenValue<T> {
    Text(T),
    Attribute(T),
}

impl<T> TokenValue<T> {
    pub fn map<R, F: FnOnce(T) -> R>(self, f: F) -> TokenValue<R> {
        match self {
            TokenValue::Text(text) => TokenValue::Text(f(text)),
            TokenValue::Attribute(attr) => TokenValue::Attribute(f(attr)),
        }
    }

    pub fn into_inner(self) -> T {
        match self {
            TokenValue::Text(inner) => inner,
            TokenValue::Attribute(inner) => inner,
        }
    }

    pub fn is_attribute(&self) -> bool {
        matches!(self, TokenValue::Attribute(_))
    }

    pub fn is_text(&self) -> bool {
        matches!(self, TokenValue::Text(_))
    }
}

impl<'a> TokenValue<RawLexeme<'a>> {
    pub fn parse_string(self) -> TokenValue<Cow<'a, str>> {
        self.map(RawLexeme::parse_string)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    value: TokenValue<RawLexeme<'a>>,
    is_last: bool,
}

impl<'a> Token<'a> {
    #[cfg(test)]
    pub(crate) fn from_parts(value: TokenValue<RawLexeme<'a>>, is_last: bool) -> Self {
        Token { value, is_last }
    }

    fn from_lexeme(lexeme: Lexeme<'a>) -> Result<Self, UnbalancedParenthesis> {
        let token_value = match lexeme.kind {
            LexemeKind::OpeningParen | LexemeKind::ClosingParen => {
                return Err(UnbalancedParenthesis)
            }
            LexemeKind::Text(text) => TokenValue::Text(text),
            LexemeKind::Attribute(attr) => TokenValue::Attribute(attr),
        };
        Ok(Token {
            value: token_value.map(RawLexeme::from_str),
            is_last: lexeme.is_last,
        })
    }

    pub fn is_last(&self) -> bool {
        self.is_last
    }

    pub fn value(&self) -> TokenValue<RawLexeme<'a>> {
        self.value
    }
}

#[cfg(test)]
pub(crate) mod token_macro {}

#[cfg(test)]
mod tests {
    use super::RawLexeme;

    mod format_raw_lexeme {
        use super::*;

        #[test]
        fn format_simple() {
            assert_eq!(RawLexeme::from_str("simple").to_string(), "\"simple\"");
        }

        #[test]
        fn format_quoted() {
            assert_eq!(RawLexeme::from_str("'quoted'").to_string(), "'quoted'");
            assert_eq!(RawLexeme::from_str("\"quoted\"").to_string(), "\"quoted\"");
        }

        #[test]
        fn format_quoted_partial() {
            assert_eq!(RawLexeme::from_str("'quoted").to_string(), "'quoted'");
            assert_eq!(RawLexeme::from_str("\"quoted").to_string(), "\"quoted\"");
        }
    }

    mod parse_raw_lexeme {
        use super::*;
        use std::borrow::Cow;

        macro_rules! test_parse {
            ($name:ident, $text:literal => $variant:ident($result:literal)) => {
                #[test]
                fn $name() {
                    let result = RawLexeme::from_str($text).parse_string();
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
