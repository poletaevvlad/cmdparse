#[derive(Debug, Clone, Copy)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub(crate) enum Lexeme<'a> {
    OpeningParen,
    ClosingParen,
    Text(&'a str),
    Attribute(&'a str),
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
    let (input, is_attribute) = match input.strip_prefix("--") {
        Some(attr) => (attr, true),
        None => (input, false),
    };

    let mut remaining = input;
    let mut chars = input.chars();
    match chars.next() {
        Some('(') if !is_attribute => return (Some(Lexeme::OpeningParen), chars.as_str()),
        Some(')') if !is_attribute => return (Some(Lexeme::ClosingParen), chars.as_str()),
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
        Some(mut ch) => loop {
            if ch.is_whitespace() || ch == '"' || ch == '\'' || ch == ')' || ch == '#' {
                break;
            }
            remaining = chars.as_str();
            ch = match chars.next() {
                Some(ch) => ch,
                None => break,
            }
        },
        None => (),
    }

    let text = &input[..(input.len() - remaining.len())];
    match is_attribute {
        true => (Some(Lexeme::Attribute(text)), remaining),
        false if text.is_empty() => (None, remaining),
        false => (Some(Lexeme::Text(text)), remaining),
    }
}

#[cfg(test)]
mod tests {
    use super::{skip_ws, take_lexeme, Lexeme};

    #[test]
    fn skip_ws_tests() {
        assert_eq!(skip_ws(""), "");
        assert_eq!(skip_ws("   "), "");
        assert_eq!(skip_ws("  abc"), "abc");
        assert_eq!(skip_ws("  # comment"), "");
        assert_eq!(skip_ws("# comment"), "");
    }

    #[test]
    fn take_lexeme_empty() {
        assert_eq!(take_lexeme(""), (None, ""));
        assert_eq!(take_lexeme("--  "), (Some(Lexeme::Attribute("")), "  "));
        assert_eq!(take_lexeme("--"), (Some(Lexeme::Attribute("")), ""));
    }

    #[test]
    fn takes_entire_string() {
        assert_eq!(take_lexeme("abcdef"), (Some(Lexeme::Text("abcdef")), ""));
        assert_eq!(
            take_lexeme("--abcdef"),
            (Some(Lexeme::Attribute("abcdef")), "")
        );

        assert_eq!(
            take_lexeme("abcdef  "),
            (Some(Lexeme::Text("abcdef")), "  ")
        );
        assert_eq!(
            take_lexeme("--abcdef  "),
            (Some(Lexeme::Attribute("abcdef")), "  ")
        );
    }

    #[test]
    fn takes_single_char() {
        assert_eq!(take_lexeme("a"), (Some(Lexeme::Text("a")), ""));
        assert_eq!(take_lexeme("--a"), (Some(Lexeme::Attribute("a")), ""));

        assert_eq!(take_lexeme("a "), (Some(Lexeme::Text("a")), " "));
        assert_eq!(take_lexeme("--a "), (Some(Lexeme::Attribute("a")), " "));
    }

    #[test]
    fn takes_until_comment() {
        assert_eq!(
            take_lexeme("abcdef#comment"),
            (Some(Lexeme::Text("abcdef")), "#comment")
        );
        assert_eq!(
            take_lexeme("--abcdef#comment"),
            (Some(Lexeme::Attribute("abcdef")), "#comment")
        );
    }

    #[test]
    fn takes_opening_paren() {
        assert_eq!(take_lexeme("(abc"), (Some(Lexeme::OpeningParen), "abc"));
        assert_eq!(take_lexeme("("), (Some(Lexeme::OpeningParen), ""));
    }

    #[test]
    fn takes_closing_paren() {
        assert_eq!(take_lexeme(")abc"), (Some(Lexeme::ClosingParen), "abc"));
        assert_eq!(take_lexeme(")"), (Some(Lexeme::ClosingParen), ""));
    }

    #[test]
    fn empty_quoted_string() {
        assert_eq!(take_lexeme("''  a"), (Some(Lexeme::Text("''")), "  a"));
        assert_eq!(take_lexeme("\"\"  a"), (Some(Lexeme::Text("\"\"")), "  a"));
        assert_eq!(take_lexeme("''"), (Some(Lexeme::Text("''")), ""));
        assert_eq!(take_lexeme("\"\""), (Some(Lexeme::Text("\"\"")), ""));

        assert_eq!(
            take_lexeme("--''  a"),
            (Some(Lexeme::Attribute("''")), "  a")
        );
        assert_eq!(
            take_lexeme("--\"\"  a"),
            (Some(Lexeme::Attribute("\"\"")), "  a")
        );
        assert_eq!(take_lexeme("--''"), (Some(Lexeme::Attribute("''")), ""));
        assert_eq!(take_lexeme("--\"\""), (Some(Lexeme::Attribute("\"\"")), ""));
    }

    #[test]
    fn non_empty_quoted_string() {
        assert_eq!(
            take_lexeme(r#"'abc \'\"def'  a"#),
            (Some(Lexeme::Text(r#"'abc \'\"def'"#)), "  a")
        );
        assert_eq!(
            take_lexeme(r#"--'abc \'\"def'  a"#),
            (Some(Lexeme::Attribute(r#"'abc \'\"def'"#)), "  a")
        );

        assert_eq!(
            take_lexeme(r#""abc \'\"def"  a"#),
            (Some(Lexeme::Text(r#""abc \'\"def""#)), "  a")
        );
        assert_eq!(
            take_lexeme(r#"--"abc \'\"def"  a"#),
            (Some(Lexeme::Attribute(r#""abc \'\"def""#)), "  a")
        );
    }

    #[test]
    fn non_terminated_quoted_string() {
        assert_eq!(
            take_lexeme("'abc def"),
            (Some(Lexeme::Text("'abc def")), "")
        );
        assert_eq!(
            take_lexeme("\"abc def"),
            (Some(Lexeme::Text("\"abc def")), "")
        );
    }

    #[test]
    fn lexeme_followed_by_string() {
        assert_eq!(
            take_lexeme("abc\"def\""),
            (Some(Lexeme::Text("abc")), "\"def\"")
        );
        assert_eq!(
            take_lexeme("abc'def'"),
            (Some(Lexeme::Text("abc")), "'def'")
        );
    }

    #[test]
    fn lexeme_followed_by_lexeme() {
        assert_eq!(
            take_lexeme("\"abc\"def"),
            (Some(Lexeme::Text("\"abc\"")), "def")
        );
        assert_eq!(
            take_lexeme("'abc'def"),
            (Some(Lexeme::Text("'abc'")), "def")
        );

        assert_eq!(
            take_lexeme("\"abc\"--def"),
            (Some(Lexeme::Text("\"abc\"")), "--def")
        );
        assert_eq!(
            take_lexeme("'abc'--def"),
            (Some(Lexeme::Text("'abc'")), "--def")
        );
    }

    #[test]
    fn lex_multiple() {
        let mut input = "first (second --attribute) third";
        let mut lexemes = Vec::new();
        loop {
            let (lexeme, remaining) = take_lexeme(input);
            if let Some(lexeme) = lexeme {
                lexemes.push(lexeme);
                input = skip_ws(remaining);
            } else {
                assert!(remaining.is_empty());
                break;
            };
        }

        assert_eq!(
            lexemes,
            vec![
                Lexeme::Text("first"),
                Lexeme::OpeningParen,
                Lexeme::Text("second"),
                Lexeme::Attribute("attribute"),
                Lexeme::ClosingParen,
                Lexeme::Text("third"),
            ]
        );
    }
}
