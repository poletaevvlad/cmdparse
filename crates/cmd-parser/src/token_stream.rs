pub struct TokenStream<'a> {
    remaining: &'a str,
    next_token: Option<Token<'a>>,
}

impl<'a> TokenStream<'a> {
    fn new(remaining: &'a str) -> Self {
        let (first_token, remaining) = take_token(skip_ws(remaining));
        println!("{:?}, {:?}", first_token, remaining);
        match first_token {
            "" => {
                debug_assert_eq!(remaining, "");
                TokenStream {
                    remaining: "",
                    next_token: None,
                }
            }
            first_token => TokenStream {
                remaining: skip_ws(remaining),
                next_token: Some(Token {
                    text: first_token,
                    is_last: remaining.is_empty(),
                }),
            },
        }
    }

    fn peek(&self) -> Option<Token<'a>> {
        self.next_token
    }

    fn take(&mut self) -> Option<Token<'a>> {
        let current = self.next_token.take();

        let (next_token, remaining) = take_token(self.remaining);
        if !next_token.is_empty() {
            self.next_token = Some(Token {
                text: next_token,
                is_last: remaining.is_empty(),
            });
        }
        self.remaining = skip_ws(remaining);

        current
    }
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Token<'a> {
    text: &'a str,
    is_last: bool,
}

fn skip_ws(mut input: &str) -> &str {
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

fn take_token(input: &str) -> (&str, &str) {
    let mut chars = input.chars();

    let mut remaining = input;
    match chars.next() {
        Some('(' | ')') => remaining = chars.as_str(),
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

    (&input[..(input.len() - remaining.len())], remaining)
}

#[cfg(test)]
mod tests {
    use super::{Token, TokenStream};

    macro_rules! token {
        ($text:literal) => {
            (Token {
                text: $text,
                is_last: false,
            })
        };
        ($text:literal, last) => {
            (Token {
                text: $text,
                is_last: true,
            })
        };
    }

    fn assert_tokenize(input: &str, expected: &[Token]) {
        let mut stream = TokenStream::new(input);
        let mut expected_iter = expected.iter();
        loop {
            let peeked = stream.peek();
            let token = stream.take();
            let expected_token = expected_iter.next().copied();

            assert_eq!(peeked, token);
            assert_eq!(peeked, expected_token);

            if peeked.is_none() && expected_token.is_none() {
                break;
            }
        }
    }

    mod tokenizatiion {
        use super::*;

        macro_rules! test_tokenization {
            ($name:ident, $input:literal => [$($token:expr),*]) => {
                #[test]
                fn $name() {
                    assert_tokenize($input, &[$($token),*] as &[Token])
                }
            };
        }

        test_tokenization!(empty, "" => []);
        test_tokenization!(ws_only, "  " => []);
        test_tokenization!(ws_only_with_comment, " #comment" => []);
        test_tokenization!(comment_only, "#comment" => []);

        test_tokenization!(single_token, "abc" => [token!("abc", last)]);
        test_tokenization!(single_token_ws, "abc " => [token!("abc")]);
        test_tokenization!(single_token_comment, "abc#comment" => [token!("abc")]);

        test_tokenization!(muliple_tokens, "( abc  ) def (ghi)" => [
            token!("("), token!("abc"), token!(")"), token!("def"),
            token!("("), token!("ghi"), token!(")", last)
        ]);

        test_tokenization!(with_quotes, r#""\"\'" '\"\''"# => [
            token!(r#""\"\'""#), token!(r#"'\"\''"#, last)
        ]);
    }
}
