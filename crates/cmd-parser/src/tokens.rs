use crate::CompletionResult;
use std::borrow::Cow;

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

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Text(Cow<'a, str>),
    Attribute(Cow<'a, str>),
}

pub fn take_string(mut input: &str) -> (Cow<'_, str>, &str) {
    if input.starts_with(')') || input.starts_with('#') {
        return ("".into(), input);
    }

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
        (result.into(), chars.as_str())
    } else {
        let token_start = input;
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
        (
            token_start[..(token_start.len() - input.len())].into(),
            input,
        )
    }
}

pub fn take_token_no_ws(input: &str) -> (Token<'_>, &str) {
    match input.strip_prefix("--") {
        Some(input) => {
            let (result, remaining) = take_string(input);
            (Token::Attribute(result), remaining)
        }
        None => {
            let (result, remaining) = take_string(input);
            (Token::Text(result), remaining)
        }
    }
}

pub fn take_token(input: &str) -> (Token<'_>, &str) {
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

pub fn complete_variants<'a>(token: &str, variants: &[&'a str]) -> Vec<Cow<'a, str>> {
    let index = variants.binary_search(&token).unwrap_or_else(|idx| idx);
    variants[index..]
        .iter()
        .map(|variant| variant.strip_prefix(token))
        .take_while(Option::is_some)
        .map(|suggestion| Cow::Borrowed(suggestion.unwrap())) // Iterator::take_while is unstable, unwrap is safe, Nones are filtered out
        .filter(|suggestion| !suggestion.is_empty())
        .collect()
}

/// Computes suggestions from a list of string slices. The list **must** be sorted.
pub fn complete_enum<'a>(input: &'a str, variants: &[&'static str]) -> CompletionResult<'a> {
    let (token, remaining) = take_token_no_ws(input);
    if !remaining.is_empty() {
        return CompletionResult::Consumed(skip_ws(remaining));
    }

    match token {
        Token::Text(token) if !token.is_empty() => {
            let suggestions = complete_variants(token.as_ref(), variants);
            CompletionResult::Suggestions(suggestions)
        }
        _ => CompletionResult::empty(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::borrow::Cow;

    #[test]
    fn skip_whitespace() {
        assert_eq!(skip_ws("  tail"), "tail");
        assert_eq!(skip_ws("tail"), "tail");
        assert_eq!(skip_ws(""), "");
    }

    mod tokens {
        use super::*;

        #[test]
        fn empty_string() {
            assert_eq!(take_token(""), (Token::Text("".into()), ""));
            assert_eq!(skip_token_no_ws(""), "");

            assert_eq!(take_token("--"), (Token::Attribute("".into()), ""));
            assert_eq!(skip_token_no_ws("--"), "");
        }

        #[test]
        fn whitespace_only() {
            assert_eq!(take_token("   "), (Token::Text("".into()), ""));
            assert_eq!(skip_token_no_ws("   "), "   ");

            assert_eq!(take_token("--   "), (Token::Attribute("".into()), ""));
            assert_eq!(skip_token_no_ws("--   "), "   ");
        }

        #[test]
        fn comment() {
            assert_eq!(take_token("#comment"), (Token::Text("".into()), "#comment"));
            assert_eq!(skip_token_no_ws("#comment"), "#comment");

            assert_eq!(
                take_token("--#comment"),
                (Token::Attribute("".into()), "#comment")
            );
            assert_eq!(skip_token_no_ws("--#comment"), "#comment");
        }

        #[test]
        fn takes_entire_string() {
            assert_eq!(
                take_token("abcdef"),
                (Token::Text(Cow::Borrowed("abcdef")), "")
            );
            assert_eq!(skip_token_no_ws("abcdef"), "");

            assert_eq!(
                take_token("--abcdef"),
                (Token::Attribute(Cow::Borrowed("abcdef")), "")
            );
            assert_eq!(skip_token_no_ws("--abcdef"), "");
        }

        #[test]
        fn takes_until_comment() {
            assert_eq!(
                take_token("abcdef#comment"),
                (Token::Text("abcdef".into()), "#comment")
            );
            assert_eq!(skip_token_no_ws("abcdef#comment"), "#comment");

            assert_eq!(
                take_token("--abcdef#comment"),
                (Token::Attribute("abcdef".into()), "#comment")
            );
            assert_eq!(skip_token_no_ws("--abcdef#comment"), "#comment");
        }

        #[test]
        fn takes_entire_string_with_whitespaces() {
            assert_eq!(take_token("abcdef  "), (Token::Text("abcdef".into()), ""));
            assert_eq!(skip_token_no_ws("abcdef  "), "  ");

            assert_eq!(
                take_token("--abcdef  "),
                (Token::Attribute("abcdef".into()), "")
            );
            assert_eq!(skip_token_no_ws("--abcdef  "), "  ");
        }

        #[test]
        fn tokenizes_multiple() {
            let mut input = "first second --attribute third";
            let mut tokens = Vec::new();
            while has_tokens(input) {
                let (token, remaining) = take_token(input);
                let remaining2 = skip_ws(skip_token_no_ws(input));
                assert_eq!(remaining, remaining2);
                match token {
                    Token::Text(text) => tokens.push(text.to_string()),
                    Token::Attribute(attr) => tokens.push(format!("--({})", attr)),
                }
                input = remaining;
            }
            assert_eq!(
                tokens,
                vec![
                    "first".to_string(),
                    "second".to_string(),
                    "--(attribute)".to_string(),
                    "third".to_string(),
                ]
            );
        }

        #[test]
        fn empty_quoted_string() {
            assert_eq!(take_token("''  a"), (Token::Text("".into()), "a"));
            assert_eq!(skip_token_no_ws("''  a"), "  a");
            assert_eq!(take_token("\"\"  a"), (Token::Text("".into()), "a"));
            assert_eq!(skip_token_no_ws("\"\"  a"), "  a");

            assert_eq!(take_token("--''  a"), (Token::Attribute("".into()), "a"));
            assert_eq!(take_token("--\"\"  a"), (Token::Attribute("".into()), "a"));
        }

        #[test]
        fn non_empty_quoted_string() {
            assert_eq!(
                take_token("'abc \"def'  a"),
                (Token::Text("abc \"def".into()), "a")
            );
            assert_eq!(
                take_token("--'abc \"def'  a"),
                (Token::Attribute("abc \"def".into()), "a")
            );
            assert_eq!(skip_token_no_ws("'abc \"def'  a"), "  a");

            assert_eq!(
                take_token("\"abc 'def\"  a"),
                (Token::Text("abc 'def".into()), "a")
            );
            assert_eq!(skip_token_no_ws("\"abc 'def\"  a"), "  a");
        }

        #[test]
        fn string_with_escape_sequence() {
            assert_eq!(
                take_token(r#"'"\'\\\a'  a"#),
                (Token::Text(r#""'\a"#.into()), "a")
            );
            assert_eq!(
                take_token(r#"--'"\'\\\a'  a"#),
                (Token::Attribute(r#""'\a"#.into()), "a")
            );
            assert_eq!(skip_token_no_ws(r#"'"\'\\\a'  a"#), "  a");
            assert_eq!(
                take_token(r#""\"'\\\a"  a"#),
                (Token::Text(r#""'\a"#.into()), "a")
            );
            assert_eq!(skip_token_no_ws(r#""\"'\\\a"  a"#), "  a");
        }

        #[test]
        fn token_followed_by_string() {
            assert_eq!(
                take_token("abc\"def\""),
                (Token::Text("abc".into()), "\"def\"")
            );
            assert_eq!(skip_token_no_ws("abc\"def\""), "\"def\"");
            assert_eq!(take_token("abc'def'"), (Token::Text("abc".into()), "'def'"));
            assert_eq!(skip_token_no_ws("abc'def'"), "'def'");
        }
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
