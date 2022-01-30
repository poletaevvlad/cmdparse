#[derive(Debug, Clone, Copy)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub(crate) enum LexemeKind<'a> {
    OpeningParen,
    ClosingParen,
    Text(&'a str),
    Attribute(&'a str),
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub(crate) struct Lexeme<'a> {
    pub(crate) kind: LexemeKind<'a>,
    pub(crate) is_last: bool,
}

pub(crate) fn skip_ws(mut input: &str) -> &str {
    loop {
        let mut chars = input.chars();
        match chars.next() {
            Some(ch) if ch.is_whitespace() => (),
            Some('#') => return "",
            _ => return input,
        }
        input = chars.as_str();
    }
}

pub(crate) fn take_lexeme(input: &str) -> (Option<Lexeme<'_>>, &str) {
    let (kind, remaining) = take_lexeme_kind(input);
    (
        kind.map(|kind| Lexeme {
            kind,
            is_last: remaining.is_empty(),
        }),
        skip_ws(remaining),
    )
}

fn take_lexeme_kind(input: &str) -> (Option<LexemeKind<'_>>, &str) {
    let (input, is_attribute) = match input.strip_prefix("--") {
        Some(attr) => (attr, true),
        None => (input, false),
    };

    let mut remaining = input;
    let mut chars = input.chars();
    match chars.next() {
        Some('(') if !is_attribute => return (Some(LexemeKind::OpeningParen), chars.as_str()),
        Some(')') if !is_attribute => return (Some(LexemeKind::ClosingParen), chars.as_str()),
        Some(quote @ ('"' | '\'')) => {
            let mut escaped = false;
            for ch in &mut chars {
                if escaped {
                    escaped = false;
                } else {
                    match ch {
                        ch if ch == quote => break,
                        '\\' => escaped = true,
                        _ => (),
                    }
                }
            }
            remaining = chars.as_str();
        }
        Some(_) => {
            while let Some(ch) = chars.next() {
                if ch.is_whitespace() || ch == '"' || ch == '\'' || ch == ')' || ch == '#' {
                    break;
                }
                remaining = chars.as_str();
            }
        }
        None => (),
    }

    let text = &input[..(input.len() - remaining.len())];
    match is_attribute {
        true => (Some(LexemeKind::Attribute(text)), remaining),
        false if text.is_empty() => (None, remaining),
        false => (Some(LexemeKind::Text(text)), remaining),
    }
}

#[cfg(test)]
mod tests {
    use super::{skip_ws, take_lexeme, Lexeme, LexemeKind};

    #[test]
    fn skip_ws_tests() {
        assert_eq!(skip_ws(""), "");
        assert_eq!(skip_ws("   "), "");
        assert_eq!(skip_ws("  abc"), "abc");
        assert_eq!(skip_ws("  # comment"), "");
        assert_eq!(skip_ws("# comment"), "");
    }

    macro_rules! lexeme {
        ('(' $(, $mod:ident)?) => {
            Lexeme {
                kind: LexemeKind::OpeningParen,
                is_last: lexeme!(@internal_is_last $($mod)?),
            }
        };
        (')' $(, $mod:ident)?) => {
            Lexeme {
                kind: LexemeKind::ClosingParen,
                is_last: lexeme!(@internal_is_last $($mod)?),
            }
        };
        (--$text:literal $(, $mod:ident)?) => {
            Lexeme {
                kind: LexemeKind::Attribute($text),
                is_last: lexeme!(@internal_is_last $($mod)?),
            }
        };
        ($text:literal $(, $mod:ident)?) => {
            Lexeme {
                kind: LexemeKind::Text($text),
                is_last: lexeme!(@internal_is_last $($mod)?),
            }
        };

        (@internal_is_last last) => { true };
        (@internal_is_last) => { false };
    }

    #[test]
    fn take_lexeme_empty() {
        assert_eq!(take_lexeme(""), (None, ""));
        assert_eq!(take_lexeme("--  "), (Some(lexeme!(--"")), ""));
        assert_eq!(take_lexeme("--"), (Some(lexeme!(--"", last)), ""));
    }

    #[test]
    fn takes_entire_string() {
        assert_eq!(take_lexeme("abcdef"), (Some(lexeme!("abcdef", last)), ""));
        assert_eq!(
            take_lexeme("--abcdef"),
            (Some(lexeme!(--"abcdef", last)), "")
        );

        assert_eq!(take_lexeme("abcdef  "), (Some(lexeme!("abcdef")), ""));
        assert_eq!(take_lexeme("--abcdef  "), (Some(lexeme!(--"abcdef")), ""));
    }

    #[test]
    fn takes_until_comment() {
        assert_eq!(take_lexeme("abcdef#comment"), (Some(lexeme!("abcdef")), ""));
        assert_eq!(
            take_lexeme("--abcdef#comment"),
            (Some(lexeme!(--"abcdef")), "")
        );
    }

    #[test]
    fn takes_opening_paren() {
        assert_eq!(take_lexeme("(abc"), (Some(lexeme!('(')), "abc"));
        assert_eq!(take_lexeme("("), (Some(lexeme!('(', last)), ""));
    }

    #[test]
    fn takes_closing_paren() {
        assert_eq!(take_lexeme(")abc"), (Some(lexeme!(')')), "abc"));
        assert_eq!(take_lexeme(")"), (Some(lexeme!(')', last)), ""));
    }

    #[test]
    fn empty_quoted_string() {
        assert_eq!(take_lexeme("''  a"), (Some(lexeme!("''")), "a"));
        assert_eq!(take_lexeme("\"\"  a"), (Some(lexeme!("\"\"")), "a"));
        assert_eq!(take_lexeme("''"), (Some(lexeme!("''", last)), ""));
        assert_eq!(take_lexeme("\"\""), (Some(lexeme!("\"\"", last)), ""));

        assert_eq!(take_lexeme("--''  a"), (Some(lexeme!(--"''")), "a"));
        assert_eq!(take_lexeme("--\"\"  a"), (Some(lexeme!(--"\"\"")), "a"));
        assert_eq!(take_lexeme("--''"), (Some(lexeme!(--"''", last)), ""));
        assert_eq!(take_lexeme("--\"\""), (Some(lexeme!(--"\"\"", last)), ""));
    }

    #[test]
    fn non_empty_quoted_string() {
        assert_eq!(
            take_lexeme(r#"'abc \'\"def'  a"#),
            (Some(lexeme!(r#"'abc \'\"def'"#)), "a")
        );
        assert_eq!(
            take_lexeme(r#"--'abc \'\"def'  a"#),
            (Some(lexeme!(--r#"'abc \'\"def'"#)), "a")
        );

        assert_eq!(
            take_lexeme(r#""abc \'\"def"  a"#),
            (Some(lexeme!(r#""abc \'\"def""#)), "a")
        );
        assert_eq!(
            take_lexeme(r#"--"abc \'\"def"  a"#),
            (Some(lexeme!(--r#""abc \'\"def""#)), "a")
        );
    }

    #[test]
    fn non_terminated_quoted_string() {
        assert_eq!(
            take_lexeme("'abc def"),
            (Some(lexeme!("'abc def", last)), "")
        );
        assert_eq!(
            take_lexeme("\"abc def"),
            (Some(lexeme!("\"abc def", last)), "")
        );
    }

    #[test]
    fn lexeme_followed_by_string() {
        assert_eq!(take_lexeme("abc\"def\""), (Some(lexeme!("abc")), "\"def\""));
        assert_eq!(take_lexeme("abc'def'"), (Some(lexeme!("abc")), "'def'"));
    }

    #[test]
    fn lexeme_followed_by_lexeme() {
        assert_eq!(take_lexeme("\"abc\"def"), (Some(lexeme!("\"abc\"")), "def"));
        assert_eq!(take_lexeme("'abc'def"), (Some(lexeme!("'abc'")), "def"));

        assert_eq!(
            take_lexeme("\"abc\"--def"),
            (Some(lexeme!("\"abc\"")), "--def")
        );
        assert_eq!(take_lexeme("'abc'--def"), (Some(lexeme!("'abc'")), "--def"));
    }

    #[test]
    fn lex_multiple() {
        let mut input = "first (second --attribute) third";
        let mut lexemes = Vec::new();
        loop {
            let (lexeme, remaining) = take_lexeme(input);
            if let Some(lexeme) = lexeme {
                lexemes.push(lexeme);
                input = remaining;
            } else {
                assert!(remaining.is_empty());
                break;
            };
        }

        assert_eq!(
            lexemes,
            vec![
                lexeme!("first"),
                lexeme!('('),
                lexeme!("second"),
                lexeme!(--"attribute"),
                lexeme!(')'),
                lexeme!("third", last),
            ]
        );
    }
}
