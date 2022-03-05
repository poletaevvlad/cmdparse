use cmd_parser::error::{ParseError, UnrecognizedToken};
use cmd_parser::tokens::{Token, TokenStream};
use cmd_parser::{parse, CompletionResult, Parsable, ParseResult, Parser};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq, Hash)]
struct Key(String);

type ParsingContext<'a> = &'a HashMap<Key, String>;

#[derive(Default)]
struct KeyParser;

impl<'c> Parser<ParsingContext<'c>> for KeyParser {
    type Value = Key;

    fn parse<'a>(
        &self,
        input: TokenStream<'a>,
        _ctx: ParsingContext<'c>,
    ) -> ParseResult<'a, Self::Value> {
        match input.take().transpose()? {
            Some((Token::Text(text), remaining)) => {
                let key = Key(text.parse_string().to_string());
                Ok((key, remaining))
            }
            Some((token @ Token::Attribute(_), remaining)) => {
                Err(UnrecognizedToken::new(token, remaining).into())
            }
            None => Err(ParseError::token_required().expected("key").into()),
        }
    }

    fn complete<'a>(
        &self,
        input: TokenStream<'a>,
        ctx: ParsingContext<'c>,
    ) -> CompletionResult<'a> {
        match input.take() {
            Some(Ok((Token::Text(text), remaining))) if remaining.is_all_consumed() => {
                let text = text.parse_string();
                CompletionResult::new_final(true).add_suggestions(ctx.keys().filter_map(|key| {
                    key.0
                        .strip_prefix(&text as &str)
                        .and_then(|key| match key.is_empty() {
                            true => None,
                            false => Some(key.to_string().into()),
                        })
                }))
            }
            Some(Ok((Token::Text(_), remaining))) => CompletionResult::new(remaining, true),
            Some(Ok((Token::Attribute(_), _))) => CompletionResult::new(input, false),
            Some(Err(_)) | None => CompletionResult::new_final(false),
        }
    }
}

impl<'a> Parsable<ParsingContext<'a>> for Key {
    type Parser = KeyParser;
}

#[derive(Debug, Parsable)]
#[cmd(ctx = "ParsingContext<'_>")]
enum Command {
    Set(Key, String),
    Get(Key),
    Del(Vec<Key>),
    GetSubstr {
        key: Key,
        length: usize,
        #[cmd(attr(start))]
        start: usize,
    },
    GetStrlen(Key),
}

fn main() {
    let mut data = HashMap::new();

    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("storage> ");
        match readline {
            Ok(line) => match parse::<_, Command>(&line, &data) {
                Ok(Command::Set(key, value)) => {
                    data.insert(key, value);
                    println!("OK");
                }
                Ok(Command::Get(key)) => {
                    let value: Option<&str> = data.get(&key).map(String::as_str);
                    println!("{}", value.unwrap_or("(nil)"));
                }
                Ok(Command::Del(keys)) => {
                    for key in keys {
                        data.remove(&key);
                    }
                    println!("OK");
                }
                Ok(Command::GetSubstr { key, length, start }) => match data.get(&key) {
                    Some(value) => {
                        for ch in value.chars().skip(start).take(length) {
                            print!("{}", ch);
                        }
                        println!();
                    }
                    None => println!("(nil)"),
                },
                Ok(Command::GetStrlen(key)) => match data.get(&key) {
                    Some(value) => println!("{}", value.chars().count()),
                    None => println!("(nil)"),
                },
                Err(err) => println!("Error: {}", err),
            },
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {}", err);
                break;
            }
        }
    }
}
