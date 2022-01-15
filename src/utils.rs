use crate::{CompletionResult, ParseError, ParseResult, Parser};
use std::borrow::Cow;

pub fn parse_inner<'a, Ctx, P: Parser<Ctx>>(
    mut input: &'a str,
    parser: &P,
) -> ParseResult<'a, P::Value> {
    input = skip_ws(input);

    if let Some(input) = input.strip_prefix('(') {
        let (result, mut remaining) = match parser.parse(input) {
            ParseResult::Parsed(result, remaining) => (result, remaining),
            result => return result,
        };
        if remaining.starts_with(')') {
            remaining = skip_ws(&remaining[1..]);
        } else {
            let (token, _) = take_token(remaining);
            if let Some(token) = token {
                return ParseError::unexpected_token(token).into();
            }
        }
        ParseResult::Parsed(result, remaining)
    } else {
        parser.parse(input)
    }
}

pub fn complete_inner<'a, Ctx, P: Parser<Ctx>>(
    mut input: &'a str,
    parser: &P,
) -> CompletionResult<'a> {
    input = skip_ws(input);

    if let Some(input) = input.strip_prefix('(') {
        match parser.complete(input) {
            CompletionResult::Consumed(remaining) => {
                let remaining = remaining.strip_prefix(')').unwrap_or(remaining);
                CompletionResult::Consumed(remaining)
            }
            result => result,
        }
    } else {
        parser.complete(input)
    }
}

pub fn has_tokens(input: &str) -> bool {
    !input.is_empty() && !input.starts_with(')') && !input.starts_with('#')
}

pub fn skip_ws(mut input: &str) -> &str {
    loop {
        let mut chars = input.chars();
        match chars.next() {
            Some(ch) if ch.is_whitespace() => {
                input = chars.as_str();
            }
            None | Some(_) => return input,
        }
    }
}

pub fn take_token_no_ws(mut input: &str) -> (Option<Cow<'_, str>>, &str) {
    if input.starts_with(')') || input.starts_with('#') {
        return (None, input);
    }

    let token_start = input;
    if input.starts_with('"') || input.starts_with('\'') {
        let mut result = String::new();
        let mut chars = input.chars();
        let quote_ch = chars.next().unwrap();
        let mut escaped = false;
        for ch in &mut chars {
            if escaped {
                result.push(ch);
                escaped = false;
            } else {
                match ch {
                    ch if ch == quote_ch => break,
                    '\\' => escaped = true,
                    ch => result.push(ch),
                }
            }
        }
        (Some(result.into()), chars.as_str())
    } else {
        loop {
            let mut chars = input.chars();
            match chars.next() {
                Some(ch)
                    if !ch.is_whitespace() && ch != '"' && ch != '\'' && ch != ')' && ch != '#' =>
                {
                    input = chars.as_str();
                }
                _ => break,
            }
        }
        let token = &token_start[..(token_start.len() - input.len())];
        if !token.is_empty() {
            (Some(Cow::Borrowed(token)), input)
        } else {
            (None, input)
        }
    }
}

pub fn take_token(input: &str) -> (Option<Cow<'_, str>>, &str) {
    let (token, remaining) = take_token_no_ws(input);
    (token, skip_ws(remaining))
}

pub fn skip_token_no_ws(mut input: &str) -> &str {
    if input.starts_with(')') || input.starts_with('#') {
        return input;
    }

    if input.starts_with('"') || input.starts_with('\'') {
        let mut chars = input.chars();
        let quote_ch = chars.next().unwrap();
        let mut escaped = false;
        for ch in &mut chars {
            if !escaped {
                match ch {
                    ch if ch == quote_ch => break,
                    '\\' => escaped = true,
                    _ => {}
                }
            } else {
                escaped = false;
            }
        }
        chars.as_str()
    } else {
        loop {
            let mut chars = input.chars();
            match chars.next() {
                Some(ch)
                    if !ch.is_whitespace() && ch != '"' && ch != '\'' && ch != ')' && ch != '#' =>
                {
                    input = chars.as_str();
                }
                _ => break,
            }
        }
        input
    }
}

pub fn skip_token(input: &str) -> &str {
    skip_ws(skip_token_no_ws(input))
}

/// Computes suggestions from a list of string slices. The list **must** be sorted.
pub fn complete_enum<'a>(input: &'a str, variants: &[&'static str]) -> CompletionResult<'a> {
    let (token, remaining) = take_token_no_ws(input);
    match token {
        Some(_) if !remaining.is_empty() => CompletionResult::Consumed(skip_ws(remaining)),
        Some(token) => {
            let index = variants
                .binary_search(&token.as_ref())
                .unwrap_or_else(|idx| idx);
            let suggestions = variants[index..]
                .iter()
                .map(|variant| variant.strip_prefix(token.as_ref()))
                .take_while(Option::is_some)
                .map(|suggestion| Cow::Borrowed(suggestion.unwrap())) // Iterator::take_while is unstable, unwrap is safe, Nones are filtered out
                .filter(|suggestion| !suggestion.is_empty())
                .collect();
            CompletionResult::Suggestions(suggestions)
        }
        None => CompletionResult::empty(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::skip_token;
    use std::borrow::Cow;

    #[test]
    fn skip_whitespace() {
        assert_eq!(skip_ws("  tail"), "tail");
        assert_eq!(skip_ws("tail"), "tail");
        assert_eq!(skip_ws(""), "");
    }

    #[test]
    fn empty_string() {
        assert_eq!(take_token(""), (None, ""));
        assert_eq!(skip_token_no_ws(""), "");
        assert_eq!(skip_token(""), "");
    }

    #[test]
    fn whitespace_only() {
        assert_eq!(take_token("   "), (None, ""));
        assert_eq!(skip_token_no_ws("   "), "   ");
        assert_eq!(skip_token("   "), "");
    }

    #[test]
    fn comment() {
        assert_eq!(take_token("#comment"), (None, "#comment"));
        assert_eq!(skip_token_no_ws("#comment"), "#comment");
        assert_eq!(skip_token("#comment"), "#comment");
    }

    #[test]
    fn takes_entire_string() {
        assert_eq!(take_token("abcdef"), (Some(Cow::Borrowed("abcdef")), ""));
        assert_eq!(skip_token_no_ws("abcdef"), "");
        assert_eq!(skip_token("abcdef"), "");
    }

    #[test]
    fn takes_until_comment() {
        assert_eq!(
            take_token("abcdef#comment"),
            (Some(Cow::Borrowed("abcdef")), "#comment")
        );
        assert_eq!(skip_token("abcdef#comment"), "#comment");
    }

    #[test]
    fn takes_entire_string_with_whitespaces() {
        assert_eq!(take_token("abcdef  "), (Some(Cow::Borrowed("abcdef")), ""));
        assert_eq!(skip_token_no_ws("abcdef  "), "  ");
        assert_eq!(skip_token("abcdef  "), "");
    }

    #[test]
    fn tokenizes_multiple() {
        let mut input = "first second third";
        let mut tokens = Vec::new();
        loop {
            let (token, remaining) = take_token(input);
            let remaining2 = skip_token(input);
            assert_eq!(remaining, remaining2);
            if let Some(token) = token {
                tokens.push(token);
            } else {
                break;
            }
            input = remaining;
        }
        assert_eq!(tokens, vec!["first", "second", "third"]);
    }

    #[test]
    fn empty_quoted_string() {
        assert_eq!(take_token("''  a"), (Some(Cow::Owned(String::new())), "a"));
        assert_eq!(skip_token("''  a"), "a");
        assert_eq!(
            take_token("\"\"  a"),
            (Some(Cow::Owned(String::new())), "a")
        );
        assert_eq!(skip_token("\"\"  a"), "a");
    }

    #[test]
    fn non_empty_quoted_string() {
        assert_eq!(
            take_token("'abc \"def'  a"),
            (Some(Cow::Owned("abc \"def".to_string())), "a")
        );
        assert_eq!(skip_token("'abc \"def'  a"), "a");

        assert_eq!(
            take_token("\"abc 'def\"  a"),
            (Some(Cow::Owned("abc 'def".to_string())), "a")
        );
        assert_eq!(skip_token("\"abc 'def\"  a"), "a");
    }

    #[test]
    fn string_with_escape_sequence() {
        assert_eq!(
            take_token(r#"'"\'\\\a'  a"#),
            (Some(Cow::Owned(r#""'\a"#.to_string())), "a")
        );
        assert_eq!(skip_token(r#"'"\'\\\a'  a"#), "a");
        assert_eq!(
            take_token(r#""\"'\\\a"  a"#),
            (Some(Cow::Owned(r#""'\a"#.to_string())), "a")
        );
        assert_eq!(skip_token(r#""\"'\\\a"  a"#), "a");
    }

    #[test]
    fn token_followed_by_string() {
        assert_eq!(
            take_token("abc\"def\""),
            (Some(Cow::Borrowed("abc")), "\"def\"")
        );
        assert_eq!(skip_token("abc\"def\""), "\"def\"");
        assert_eq!(
            take_token("abc'def'"),
            (Some(Cow::Borrowed("abc")), "'def'")
        );
        assert_eq!(skip_token("abc'def'"), "'def'");
    }

    mod complete_enum_tests {
        use super::*;

        const VARIANTS: &[&str] = &["back", "bat", "before", "end", "endianness", "ending"];

        #[test]
        fn empty_string() {
            assert_eq!(complete_enum("", VARIANTS), CompletionResult::empty());
        }

        #[test]
        fn consumed() {
            assert_eq!(
                complete_enum("unknown another", VARIANTS),
                CompletionResult::Consumed("another")
            );
        }

        #[test]
        fn consumed_space() {
            assert_eq!(
                complete_enum("unknown ", VARIANTS),
                CompletionResult::Consumed("")
            );
        }

        macro_rules! assert_completion {
            ($input:literal, [$($expected:literal),*]) => {
                assert_eq!(
                    complete_enum($input, VARIANTS),
                    CompletionResult::Suggestions(vec![$($expected.into()),*])
                );
            };
        }

        #[test]
        fn completion() {
            assert_completion!("apricot", []);
            assert_completion!("cat", []);
            assert_completion!("ferrot", []);

            assert_completion!("b", ["ack", "at", "efore"]);
            assert_completion!("ba", ["ck", "t"]);
            assert_completion!("bac", ["k"]);
            assert_completion!("back", []);
            assert_completion!("backs", []);
            assert_completion!("be", ["fore"]);
            assert_completion!("bet", []);

            assert_completion!("e", ["nd", "ndianness", "nding"]);
            assert_completion!("end", ["ianness", "ing"]);
            assert_completion!("endi", ["anness", "ng"]);
            assert_completion!("ending", []);
            assert_completion!("endings", []);
            assert_completion!("endianness", []);
        }
    }
}
