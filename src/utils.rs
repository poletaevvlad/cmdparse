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

pub fn take_token(mut input: &str) -> (Option<Cow<'_, str>>, &str) {
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
        (Some(result.into()), skip_ws(chars.as_str()))
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
            (Some(Cow::Borrowed(token)), skip_ws(input))
        } else {
            (None, skip_ws(input))
        }
    }
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
}
