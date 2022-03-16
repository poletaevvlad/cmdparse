[![Github](https://img.shields.io/github/last-commit/poletaevvlad/cmdparse "Documentation")](https://github.com/poletaevvlad/cmdparse) [![MIT/Apache 2.0](https://img.shields.io/crates/l/cmdparse "MIT/Apache 2.0")](https://crates.io/crates/cmdparse) [![Crates.io](https://img.shields.io/crates/v/cmdparse "Crates.io")](https://crates.io/crates/cmdparse) [![Documentation](https://img.shields.io/docsrs/cmdparse "Documentation")](https://docs.rs/cmdparse)

# cmdparse

`cmdparse` is, as the name suggests, parses user commands into arbitrary Rust types.

Generally, this crate can be viewed as a data deserialization framework. It defines a syntax
designed to be easy to entered interactively and includes utilities for transforming the input
in this format into arbitrary Rust types, as well as automatically suggesting completions for
incomplete user input.

It is not suitable for parsing command line arguments, even though the syntax it supports is
fairly similar to what those would look like. Instead, it was designed to be used for parsing
commands entered interactively inside the application. Of course, you are not limited to this
use case and free to use `cmdparse` as a generic data deserialization framework in any way
you like.

## Examples

Let’s consider the following example. It defines a struct `MailSendCommand` and derives
`Parsable` trait for it. This is enough to be able to parse it.

```rust
use cmdparse::{Parsable, parse};

#[derive(Debug, PartialEq, Eq, Parsable)]
struct MailSendCommand {
   text: String,
   #[cmd(attr(subject), default = "\"no subject\".to_string()")]
   subject: String,
   #[cmd(attr(to))]
   to: Vec<String>,
}

let input = "\"Hello, world\" --to user1@example.com user2@example.com --subject Greeting";
let result = parse::<_, MailSendCommand>(input, ())?;
assert_eq!(result, MailSendCommand {
    text: "Hello, world".to_string(),
    subject: "Greeting".to_string(),
    to: vec!["user1@example.com".to_string(), "user2@example.com".to_string()],
});
```

This example demonstrates several features of `cmdparse`:

 * Parsing functionality can be automatically derived for an arbitrary struct or enum as long
   as the inner types are `Parsable` or there is an appropriate `Parser` for them. (To
   learn about the distinction between parsable and parser, read documentation for these traits).
 * Derived parser is configurable: you may make fields either required or optional. Optional
   fields can be specified via a name attribute (`--` token). They can have a default value
   explicitly specified (see default attribute on the `subject` field) or not (`to` field
   defaults to an empty vector, as per its `Default` implementation)
 * Parsable values can contain nested parsable values: `MailSendCommand` is parsable, it
   contains a `Vec` which is parsable and in repeatedly parses `String`s that are parsable.
   Note how `cmdparse` recognized that the list of email addresses finished when it
   encountered the attribute that neither `String` nor `Vec` recognizes.

`cmdparse` can generate completion suggestions:

```rust
use cmdparse::complete;
use std::collections::BTreeSet;

let suggestions = complete::<_, MailSendCommand>("\"Hello, world\" --", ());
assert_eq!(suggestions, BTreeSet::from(["to".into(), "subject".into()]));
```

It also supports parsing enums. In case of enum, it expects a discriminator (automatically
converted into kebab-case by the `Parsable` derive macro):

```rust
use cmdparse::{parse, Parsable};

#[derive(Debug, PartialEq, Eq, Parsable)]
enum Priority {
   High,
   Medium,
   Low,
}

impl Default for Priority {
    fn default() -> Self {
        Priority::Medium
    }
}

#[derive(Debug, PartialEq, Eq, Parsable)]
enum Command {
    AddTask(String, #[cmd(attr(priority))] Priority),
    Remove(usize),
}

assert_eq!(
    parse::<_, Command>("add-task parse-all-commands", ())?,
    Command::AddTask("parse-all-commands".to_string(), Priority::Medium),
);
assert_eq!(
    parse::<_, Command>("add-task enjoy-your-day --priority high", ())?,
    Command::AddTask("enjoy-your-day".to_string(), Priority::High),
);
assert_eq!(parse::<_, Command>("remove 1", ())?, Command::Remove(1));
```

## Syntax

The syntax that `cmdparse` supports is fairly minimal. The parsing machinery sees the input as
a sequence of tokens. Token is any sequence of characters separated by whitespaces. If you wish
to include a whitespace in the token, you may enclose any substring of the input into a pair of
quotation marks (either double or singular); `cmdparse` supports escaping these symbols
inside quoted tokens with a slash (`\`).

Input can contain a comment beginning with an octothorp (`#`). Octothorps within quoted tokens
are not considered beginning a comment.

The meaning of the token and attributes are highly specific to each parser. Generally, each
parser consumes tokens sequentially until each required field’s value is filled. It also
handles attributes in any order and at arbitrary positions.

Due to the nature of the commands' syntax, parsing can seem ambiguous. For example,
`cmdparse` can parse nested structs such as `Vec<Vec<u32>>`. It may be confusing to the end
user, how would a sequence of numbers be interpreted (they all will be put in the only item of
the outer vector). It is best to design your command to be simple and avoid highly nested
structures for the better user experience. In some cases, complexity is unavoidable. In such
situations, users may find useful an ability to group tokens, belonging to the same data
structure, with parenthesis: `(` and `)`. This way, users can express a value `vec![vec![1, 2],
vec![3, 4, 5]]` as `(1 2) (3 4 5)`.

More details about how the tokenization and the parsing algorithm are documented in the
`tokens` module’s and `Parser` trait’s documentation.

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
