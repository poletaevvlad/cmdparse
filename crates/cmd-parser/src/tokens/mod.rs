mod lexing;
mod stream;

use lexing::{Lexeme, LexemeKind};
use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawLexeme<'a>(&'a str);

impl<'a> RawLexeme<'a> {
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

#[derive(Debug)]
pub struct UnexpectedPunctuation;

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
    fn from_lexeme(lexeme: Lexeme<'a>) -> Result<Self, UnexpectedPunctuation> {
        let token_value = match lexeme.kind {
            LexemeKind::OpeningParen | LexemeKind::ClosingParen => {
                return Err(UnexpectedPunctuation)
            }
            LexemeKind::Text(text) => TokenValue::Text(text),
            LexemeKind::Attribute(attr) => TokenValue::Attribute(attr),
        };
        Ok(Token {
            value: token_value.map(RawLexeme),
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
pub(crate) mod token_macro {

    macro_rules! token {
        (--$text:literal $(, $mod:ident)?) => {
            $crate::tokens::Token {
                value: $crate::tokens::TokenValue::Attribute($crate::tokens::RawLexeme($text)),
                is_last: token!(@internal is_last $($mod)?),
            }
        };
        ($text:literal $(, $mod:ident)?) => {
            $crate::tokens::Token {
                value: $crate::tokens::TokenValue::Text($crate::tokens::RawLexeme($text)),
                is_last: token!(@internal is_last $($mod)?),
            }
        };

        (@internal is_last last) => {true};
        (@internal is_last ) => {false};
    }

    pub(crate) use token;
}

#[cfg(test)]
mod tests {
    use super::RawLexeme;

    mod parse_raw_lexeme {
        use super::*;
        use std::borrow::Cow;

        macro_rules! test_parse {
            ($name:ident, $text:literal => $variant:ident($result:literal)) => {
                #[test]
                fn $name() {
                    let result = RawLexeme($text).parse_string();
                    assert_eq!(result, Cow::Borrowed($result));
                    assert!(matches!(result, Cow::$variant(_)));
                }
            };
        }

        test_parse!(emty, "" => Borrowed(""));
        test_parse!(non_empty, "abc" => Borrowed("abc"));
        test_parse!(quoted_empty_single, "''" => Owned(""));
        test_parse!(quoted_empty_double, "\"\"" => Owned(""));
        test_parse!(quoted_non_empty_single, "'abc \\\' def \\\" fgh'" => Owned("abc \' def \" fgh"));
        test_parse!(quoted_non_empty_double, "\"abc \\\' def \\\" fgh\"" => Owned("abc \' def \" fgh"));
        test_parse!(quoted_not_terminated_single, "'abc" => Owned("abc"));
        test_parse!(quoted_not_terminated_double, "\"abc" => Owned("abc"));
    }
}
