use cmd_parser::{complete, Parsable};
use std::collections::HashSet;

macro_rules! test {
    ($name:ident, $type:ty, $( $input:literal => [$($expected:literal),*] ),* $(,)?) => {
        #[test]
        fn $name() {
            $(
            let result = complete::<(), $type>($input, ());
            let expected = HashSet::from([$($expected.into()),*]);
            assert_eq!(result, expected, "failed: \"{}\"", $input);
            )*
        }
    };
}

#[derive(Parsable)]
struct First {
    required: u16,
    #[cmd(attr(attr_first))]
    optional: u16,
}

#[derive(Parsable)]
struct Second {
    required: u16,
    #[cmd(attr(attr_second_1))]
    optional_1: u16,
    #[cmd(attr(attr_second_2))]
    optional_2: u16,
}

#[derive(Parsable)]
struct Combined(First, Second);

test!(
    struct_attributes_shared, Combined,
    "--" => ["attr-first"],
    "5 --" => ["attr-first", "attr-second-1", "attr-second-2"],
    "5 9 --" => ["attr-second-1", "attr-second-2"],
);

test!(
    tuple_attributes_shared, (First, Second),
    "--" => ["attr-first"],
    "5 --" => ["attr-first", "attr-second-1", "attr-second-2"],
    "5 9 --" => ["attr-second-1", "attr-second-2"],
);
