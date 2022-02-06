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
    _required: u16,
    #[cmd(attr(attr_first))]
    _optional: u16,
}

#[derive(Parsable, Default)]
struct Second {
    _required: u16,
    #[cmd(attr(attr_second_1))]
    _optional_1: u16,
    #[cmd(attr(attr_second_2))]
    _optional_2: u16,
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

#[derive(Parsable)]
struct CombinedWithOptional(First, #[cmd(attr(inner))] Second);

test!(
    attr_within_attribute, CombinedWithOptional,
    "--" => ["inner", "attr-first"],
    "--attr-first --" => [],
    "--attr-first 5 --" => ["attr-first"],
    "--attr-first 5 4 --" => ["attr-first", "inner"],
    "--attr-first 5 4 --inner --" => ["attr-second-1", "attr-second-2"],
    "--attr-first 5 4 --inner 5 --" => ["attr-second-1", "attr-second-2", "inner"],
);

#[derive(Parsable)]
struct Third {
    #[cmd(attr(attr_third_1 = "1"))]
    _optional_1: u16,
}

#[derive(Parsable)]
struct WithOption(First, Option<Second>, Third);

test!(
    with_option_type, WithOption,
    "--" => ["attr-first"],
    "5 --" => ["attr-first", "attr-second-1", "attr-second-2", "attr-third-1"],
    "5 4 --" => ["attr-second-1", "attr-second-2", "attr-third-1"],
);

test!(
    with_option_type_tuple, (First, Option<Second>, Third),
    "--" => ["attr-first"],
    "5 --" => ["attr-first", "attr-second-1", "attr-second-2", "attr-third-1"],
    "5 4 --" => ["attr-second-1", "attr-second-2", "attr-third-1"],
);

#[derive(Parsable)]
enum Barrier {
    X,
}

test!(
    with_barrier, (First, Barrier, Second),
    "--" => ["attr-first"],
    "5 x --" => ["attr-second-1", "attr-second-2"],
);

test!(
    vec_with_barrier, Vec<(First, Barrier, Second)>,
    "--" => ["attr-first"],
    "5 x --" => ["attr-second-1", "attr-second-2"],
    "5 x 3 --" => ["attr-second-1", "attr-second-2", "attr-first"],
);

#[derive(Parsable)]
enum EnumFirst {
    VariantA1,
    VariantA2,
}

#[derive(Parsable)]
enum EnumSecond {
    VariantB1,
    VariantB2,
}

#[derive(Parsable)]
enum EnumThird {
    VariantC1,
    VariantC2,
}

test!(
    variants_tuple, (EnumFirst, EnumSecond, EnumThird),
    "variant-" => ["a1", "a2"],
    "variant-a2 variant-" => ["b1", "b2"],
    "variant-a2 variant-b1 variant-" => ["c1", "c2"],
);

#[derive(Parsable)]
struct VariantTuple(EnumFirst, EnumSecond, EnumThird);

test!(
    variants_tuple_struct, VariantTuple,
    "variant-" => ["a1", "a2"],
    "variant-a2 variant-" => ["b1", "b2"],
    "variant-a2 variant-b1 variant-" => ["c1", "c2"],
);

test!(
    variants_option_tuple, (EnumFirst, Option<EnumSecond>, EnumThird),
    "variant-" => ["a1", "a2"],
    "variant-a2 variant-" => ["b1", "b2", "c1", "c2"],
    "variant-a2 variant-b1 variant-" => ["c1", "c2"],
);

#[derive(Parsable)]
struct VariantOptionTuple(EnumFirst, Option<EnumSecond>, EnumThird);

test!(
    variants_option_tuple_struct, VariantOptionTuple,
    "variant-" => ["a1", "a2"],
    "variant-a2 variant-" => ["b1", "b2", "c1", "c2"],
    "variant-a2 variant-b1 variant-" => ["c1", "c2"],
);

#[derive(Parsable)]
struct OptionVariant(EnumFirst, Option<EnumSecond>);

test!(
    vec_with_option_variant_struct, Vec<OptionVariant>,
    "variant-" => ["a1", "a2"],
    "variant-a2 variant-" => ["b1", "b2", "a1", "a2"],
    "variant-a2 variant-a1 variant-" => ["b1", "b2", "a1", "a2"],
    "variant-a2 variant-a1 variant-b1 variant-" => ["a1", "a2"],
);
