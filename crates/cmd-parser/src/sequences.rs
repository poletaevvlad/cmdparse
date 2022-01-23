use crate::error::ParseError;
use crate::tokens::{has_tokens, skip_ws, take_token, Token};
use crate::{CompletionResult, Parsable, ParseResult, Parser};
use std::cmp::Ord;
use std::collections::{BTreeSet, HashSet, LinkedList, VecDeque};
use std::hash::Hash;
use std::marker::PhantomData;

pub fn parse_inner<'a, Ctx, P: Parser<Ctx>>(
    mut input: &'a str,
    parser: &P,
) -> ParseResult<'a, P::Value> {
    input = skip_ws(input);

    if let Some(input) = input.strip_prefix('(') {
        let (result, mut remaining) = match parser.parse(input) {
            ParseResult::Parsed(result, remaining) => (result, remaining),
            ParseResult::UnrecognizedAttribute(attr, _) => {
                return ParseResult::Failed(ParseError::unknown_attribute(attr));
            }
            ParseResult::UnrecognizedVariant(variant) => {
                return ParseResult::UnrecognizedVariant(variant)
            }
            ParseResult::Failed(error) => return ParseResult::Failed(error),
        };
        if remaining.starts_with(')') {
            remaining = skip_ws(&remaining[1..]);
        } else {
            let (token, _) = take_token(remaining);
            match token {
                Token::Text(text) if text.is_empty() => {}
                Token::Text(text) => {
                    return ParseResult::Failed(ParseError::unexpected_token(text))
                }
                Token::Attribute(attribute) => {
                    return ParseResult::Failed(ParseError::unknown_attribute(attribute))
                }
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
            CompletionResult::Unrecognized => CompletionResult::empty(),
            result => result,
        }
    } else {
        parser.complete(input)
    }
}

pub trait ParsableCollection<Ctx> {
    type Item: Parsable<Ctx>;
    fn append(&mut self, item: Self::Item);
}

macro_rules! impl_parsable_collection {
    ($ty:ty $(where T: $bound_1:ident $(+ $bound:ident)*)? { $append:item }) => {
        impl<Ctx, T: Parsable<Ctx> $(+ $bound_1 $(+ $bound)*)?> ParsableCollection<Ctx> for $ty {
            type Item = T;
            $append
        }

        impl<Ctx, T: Parsable<Ctx> $(+ $bound_1 $(+ $bound)*)?> Parsable<Ctx> for $ty {
            type Parser = CollectionParser<Ctx, Self>;
        }
    };
}

impl_parsable_collection! {Vec<T> {
   fn append(&mut self, item: T) {
     self.push(item);
   }
}}

impl_parsable_collection! {VecDeque<T> {
   fn append(&mut self, item: T) {
     self.push_back(item);
   }
}}

impl_parsable_collection! {LinkedList<T> {
   fn append(&mut self, item: T) {
     self.push_back(item);
   }
}}

impl_parsable_collection! {HashSet<T> where T: Eq + Hash {
   fn append(&mut self, item: T) {
     self.insert(item);
   }
}}

impl_parsable_collection! {BTreeSet<T> where T: Eq + Hash + Ord {
   fn append(&mut self, item: T) {
     self.insert(item);
   }
}}

pub struct CollectionParser<Ctx, C: ParsableCollection<Ctx>> {
    _collection_phanton: PhantomData<C>,
    inner_parser: <C::Item as Parsable<Ctx>>::Parser,
}

impl<Ctx, C: ParsableCollection<Ctx> + Default> Parser<Ctx> for CollectionParser<Ctx, C> {
    type Value = C;

    fn create(ctx: Ctx) -> Self {
        CollectionParser {
            _collection_phanton: PhantomData,
            inner_parser: <C::Item as Parsable<Ctx>>::new_parser(ctx),
        }
    }

    fn parse<'a>(&self, mut input: &'a str) -> ParseResult<'a, Self::Value> {
        let mut is_first = true;
        let mut result = C::default();
        while has_tokens(input) {
            let parsed = parse_inner(input, &self.inner_parser);
            match parsed {
                ParseResult::Parsed(value, remaining) => {
                    input = remaining;
                    result.append(value);
                }
                ParseResult::UnrecognizedAttribute(attribute, remaining) => {
                    if is_first {
                        return ParseResult::UnrecognizedAttribute(attribute, remaining);
                    }
                    break;
                }
                ParseResult::UnrecognizedVariant(variant) if is_first => {
                    return ParseResult::UnrecognizedVariant(variant)
                }
                ParseResult::UnrecognizedVariant(variant) => {
                    return ParseResult::Failed(ParseError::unknown_variant(variant))
                }
                ParseResult::Failed(err) => return ParseResult::Failed(err),
            }
            is_first = false;
        }
        ParseResult::Parsed(result, input)
    }

    fn complete<'a>(&self, mut input: &'a str) -> CompletionResult<'a> {
        let mut is_first = true;
        while has_tokens(input) {
            match complete_inner(input, &self.inner_parser) {
                CompletionResult::Consumed(remaining) => input = remaining,
                result @ CompletionResult::Unrecognized if is_first => return result,
                CompletionResult::Unrecognized => return CompletionResult::Consumed(input),
                result @ CompletionResult::Suggestions(_) => return result,
            }
            is_first = false;
        }
        CompletionResult::Consumed(input)
    }
}

pub struct DefaultValueParser<T> {
    _phantom: PhantomData<T>,
}

impl<Ctx, T: Default> Parser<Ctx> for DefaultValueParser<T> {
    type Value = T;

    fn create(_ctx: Ctx) -> Self {
        DefaultValueParser {
            _phantom: PhantomData,
        }
    }

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
        ParseResult::Parsed(<T as Default>::default(), input)
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        CompletionResult::Consumed(input)
    }
}

impl<Ctx> Parsable<Ctx> for () {
    type Parser = DefaultValueParser<()>;
}

impl<Ctx, T> Parsable<Ctx> for PhantomData<T> {
    type Parser = DefaultValueParser<PhantomData<T>>;
}

macro_rules! gen_parsable_tuple {
    ($parser_name:ident, $param_first:ident $($param:ident)*) => {
        #[allow(non_snake_case)]
        pub struct $parser_name<Ctx, $param_first: Parsable<Ctx>, $($param: Parsable<Ctx>),*> {
            $param_first: $param_first::Parser,
            $(
                $param: $param::Parser,
            )*
        }

        impl<Ctx: Clone, $param_first: Parsable<Ctx>, $($param: Parsable<Ctx>),*> Parser<Ctx> for $parser_name<Ctx, $param_first, $($param),*> {
            type Value = ($param_first, $($param,)*);

            fn create(ctx: Ctx) -> Self {
                $parser_name {
                    $param_first: $param_first::new_parser(ctx.clone()),
                    $($param: $param::new_parser(ctx.clone())),*
                }
            }

            #[allow(non_snake_case)]
            fn parse<'a>(&self, mut input: &'a str) -> ParseResult<'a, Self::Value> {
                let $param_first = match parse_inner(input, &self.$param_first) {
                    ParseResult::Parsed(value, remaining) => {
                        input = remaining;
                        value
                    }
                    ParseResult::UnrecognizedAttribute(token, remaining) => {
                        return ParseResult::UnrecognizedAttribute(token, remaining)
                    }
                    ParseResult::UnrecognizedVariant(variant) => {
                        return ParseResult::UnrecognizedVariant(variant)
                    }
                    ParseResult::Failed(err) => {
                        return ParseResult::Failed(err)
                    }
                };
                $(
                    let $param = match parse_inner(input, &self.$param) {
                        ParseResult::Parsed(value, remaining) => {
                            input = remaining;
                            value
                        }
                        ParseResult::UnrecognizedAttribute(attr, _) => return ParseResult::Failed(ParseError::unknown_attribute(attr)),
                        ParseResult::UnrecognizedVariant(variant) => return ParseResult::Failed(ParseError::unknown_variant(variant)),
                         ParseResult::Failed(err) => {
                            return ParseResult::Failed(err)
                        }
                    };
                )*
                ParseResult::Parsed(($param_first, $($param,)*), input)
            }

            fn complete<'a>(&self, mut input: &'a str) -> CompletionResult<'a> {
                match complete_inner(input, &self.$param_first) {
                    CompletionResult::Consumed(remaining) => input = remaining,
                    result @ CompletionResult::Suggestions(_) => return result,
                    result @ CompletionResult::Unrecognized => return result,
                }
                $(
                    match complete_inner(input, &self.$param) {
                        CompletionResult::Consumed(remaining) => input = remaining,
                        CompletionResult::Unrecognized => return CompletionResult::empty(),
                        result => return result,
                    }
                )*
                CompletionResult::Consumed(input)
            }
        }

        impl<Ctx: Clone, $param_first: Parsable<Ctx>, $($param: Parsable<Ctx>),*> Parsable<Ctx> for ($param_first, $($param,)*) {
            type Parser = $parser_name<Ctx, $param_first, $($param),*>;
        }
    }
}

gen_parsable_tuple!(TupleParser1, T1);
gen_parsable_tuple!(TupleParser2, T1 T2);
gen_parsable_tuple!(TupleParser3, T1 T2 T3);
gen_parsable_tuple!(TupleParser4, T1 T2 T3 T4);
gen_parsable_tuple!(TupleParser5, T1 T2 T3 T4 T5);
gen_parsable_tuple!(TupleParser6, T1 T2 T3 T4 T5 T6);
gen_parsable_tuple!(TupleParser7, T1 T2 T3 T4 T5 T6 T7);
gen_parsable_tuple!(TupleParser8, T1 T2 T3 T4 T5 T6 T7 T8);
gen_parsable_tuple!(TupleParser9, T1 T2 T3 T4 T5 T6 T7 T8 T9);
gen_parsable_tuple!(TupleParser10, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10);
gen_parsable_tuple!(TupleParser11, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11);
gen_parsable_tuple!(TupleParser12, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12);
gen_parsable_tuple!(TupleParser13, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13);
gen_parsable_tuple!(TupleParser14, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14);
gen_parsable_tuple!(TupleParser15, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15);
gen_parsable_tuple!(TupleParser16, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16);

pub struct BoxParser<Ctx, T: Parsable<Ctx>> {
    inner_parser: T::Parser,
}

impl<Ctx, T: Parsable<Ctx>> Parser<Ctx> for BoxParser<Ctx, T> {
    type Value = Box<T>;

    fn create(ctx: Ctx) -> Self {
        BoxParser {
            inner_parser: T::new_parser(ctx),
        }
    }

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
        self.inner_parser.parse(input).map(Box::new)
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        self.inner_parser.complete(input)
    }
}

impl<Ctx, T: Parsable<Ctx>> Parsable<Ctx> for Box<T> {
    type Parser = BoxParser<Ctx, T>;
}

pub struct OptionParser<Ctx, T: Parsable<Ctx>> {
    inner_parser: T::Parser,
}

impl<Ctx, T: Parsable<Ctx>> Parser<Ctx> for OptionParser<Ctx, T> {
    type Value = Option<T>;

    fn create(ctx: Ctx) -> Self {
        OptionParser {
            inner_parser: T::new_parser(ctx),
        }
    }

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
        if has_tokens(input) {
            match self.inner_parser.parse(input).map(Some) {
                ParseResult::UnrecognizedAttribute(_, _) => ParseResult::Parsed(None, input),
                result => result,
            }
        } else {
            ParseResult::Parsed(None, input)
        }
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        self.inner_parser.complete(input)
    }
}

impl<Ctx, T: Parsable<Ctx>> Parsable<Ctx> for Option<T> {
    type Parser = OptionParser<Ctx, T>;
}

#[cfg(test)]
mod tests {
    use crate::{CompletionResult, Parsable, ParseError, ParseResult, Parser};

    mod collection_parser {
        use super::*;
        use std::collections::{BTreeSet, HashSet, LinkedList, VecDeque};

        #[test]
        fn parse_empty() {
            let parser = <Vec<i32> as Parsable<()>>::new_parser(());
            assert_eq!(parser.parse(""), ParseResult::Parsed(vec![], ""));
        }

        #[test]
        fn parse_flat_vec() {
            let parser = <Vec<i32> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 3 4 5"),
                ParseResult::Parsed(vec![1, 2, 3, 4, 5], "")
            );
        }

        #[test]
        fn parse_flat_vec_deque() {
            let parser = <VecDeque<i32> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 3 4 5"),
                ParseResult::Parsed(VecDeque::from([1, 2, 3, 4, 5]), "")
            );
        }

        #[test]
        fn parse_flat_linked_list() {
            let parser = <LinkedList<i32> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 3 4 5"),
                ParseResult::Parsed(LinkedList::from([1, 2, 3, 4, 5]), "")
            );
        }

        #[test]
        fn parse_flat_hash_set() {
            let parser = <HashSet<i32> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 3 4 5 3 4"),
                ParseResult::Parsed(HashSet::from([1, 2, 3, 4, 5]), "")
            );
        }

        #[test]
        fn parse_flat_btree_set() {
            let parser = <BTreeSet<i32> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 3 4 5 3 4"),
                ParseResult::Parsed(BTreeSet::from([1, 2, 3, 4, 5]), "")
            );
        }

        #[test]
        fn parse_error() {
            let parser = <Vec<i32> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 nan 3 4"),
                ParseResult::Failed(ParseError::token_parse("nan".into(), None, "integer"))
            );
        }

        #[test]
        fn parse_nested() {
            let parser = <Vec<Vec<i32>> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("() (1 2 3) 4 5 6 7"),
                ParseResult::Parsed(vec![vec![], vec![1, 2, 3], vec![4, 5, 6, 7]], "")
            );
        }

        #[test]
        fn suggest_first() {
            let parser = <Vec<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("tr"),
                CompletionResult::Suggestions(vec!["ue".into()])
            );
        }

        #[test]
        fn suggest_not_first() {
            let parser = <Vec<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("true fa"),
                CompletionResult::Suggestions(vec!["lse".into()])
            );
        }

        #[test]
        fn suggestion_consumed() {
            let parser = <Vec<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("true false "),
                CompletionResult::Consumed("")
            );
        }

        #[test]
        fn suggestion_nested() {
            let parser = <Vec<Vec<bool>> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("(true false) (false) (fal"),
                CompletionResult::Suggestions(vec!["se".into()]),
            );
        }

        #[test]
        fn suggestion_nested_closed() {
            let parser = <Vec<Vec<bool>> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("(true false) (false) (fal)"),
                CompletionResult::Consumed(""),
            );
        }

        #[test]
        fn stops_on_unknown_attribute() {
            let parser = <Vec<i32> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 3 --unknown 4 5"),
                ParseResult::Parsed(vec![1, 2, 3], "--unknown 4 5"),
            );
            assert_eq!(
                parser.complete("1 2 3 --unknown 4 5"),
                CompletionResult::Consumed("--unknown 4 5"),
            );
        }

        #[test]
        fn stops_on_unknown_attribute_with_nested_vecs() {
            let parser = <Vec<Vec<i32>> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("(1 2) 3 --unknown 4 5"),
                ParseResult::Parsed(vec![vec![1, 2], vec![3]], "--unknown 4 5"),
            );
            assert_eq!(
                parser.complete("1 2 3 --unknown 4 5"),
                CompletionResult::Consumed("--unknown 4 5"),
            );
        }

        #[test]
        fn stops_on_unknown_attribute_on_first_nested_vec() {
            let parser = <Vec<Vec<i32>> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("--unknown (1 2) (3 4) 5"),
                ParseResult::UnrecognizedAttribute("unknown".into(), "(1 2) (3 4) 5"),
            );
            assert_eq!(
                parser.complete("--unknown (1 2) (3 4) 5"),
                CompletionResult::Unrecognized,
            );
        }

        #[test]
        fn stops_on_unknown_attribute_on_first_nested_vec_inside_parenthesis() {
            let parser = <Vec<Vec<i32>> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("(--unknown 1 2) (3 4) 5"),
                ParseResult::Failed(ParseError::unknown_attribute("unknown")),
            );
            assert_eq!(
                parser.complete("(--unknown 1 2) (3 4) 5"),
                CompletionResult::empty(),
            );
        }

        #[test]
        fn fails_on_unknown_attribure_with_parenthesis() {
            let parser = <Vec<Vec<i32>> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("(1 2) (3 --unknown) 4 5"),
                ParseResult::Failed(ParseError::unknown_attribute("unknown")),
            );
        }
    }

    mod tuple_parser {
        use super::*;

        #[test]
        fn parse_tuple() {
            let parser =
                <(u8, (u16, bool), (i32, i32, i32), (bool,)) as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 true 4 5 6 false remaining"),
                ParseResult::Parsed((1, (2, true), (4, 5, 6), (false,)), "remaining"),
            );
            assert_eq!(
                parser.parse("1 (2 true) (4 5 6) (false) remaining"),
                ParseResult::Parsed((1, (2, true), (4, 5, 6), (false,)), "remaining"),
            );
        }

        #[test]
        fn parse_wrong_tokens_number() {
            let parser = <(u8, (u8, u8)) as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2"),
                ParseResult::Failed(ParseError::token_required("integer"))
            );
            assert_eq!(
                parser.parse("1 (3 4 5)"),
                ParseResult::Failed(ParseError::unexpected_token("5".into()))
            );
        }

        #[test]
        fn parse_ivalid_token() {
            let parser = <(u8, u8) as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("5 abc"),
                ParseResult::Failed(ParseError::token_parse("abc".into(), None, "integer"))
            );
        }

        #[test]
        fn parses_unrecognized_if_starts_with_unknown_attribute() {
            let parser = <(u8, u8) as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("--unknown"),
                ParseResult::UnrecognizedAttribute("unknown".into(), ""),
            );
            assert_eq!(parser.complete("--unknown"), CompletionResult::Unrecognized,);
        }

        #[test]
        fn parse_error_if_contains_unknown_attribute() {
            let parser = <(u8, u8) as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 --unknown"),
                ParseResult::Failed(ParseError::unknown_attribute("unknown")),
            );
            assert_eq!(parser.complete("1 --unknown"), CompletionResult::empty(),);
        }

        #[test]
        fn completion() {
            let parser = <(u8, (bool, u8)) as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("5 fa"),
                CompletionResult::Suggestions(vec!["lse".into()])
            );
            assert_eq!(parser.complete("5 false 4"), CompletionResult::empty());
            assert_eq!(
                parser.complete("5 false 4 "),
                CompletionResult::Consumed("")
            );
            assert_eq!(
                parser.complete("5 false 4 6"),
                CompletionResult::Consumed("6")
            );
        }

        #[test]
        fn stops_vec_of_tuples() {
            let parser = <Vec<((u8, i16), bool)> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 true 4 5 false --unknown"),
                ParseResult::Parsed(vec![((1, 2), true), ((4, 5), false)], "--unknown"),
            );
        }

        #[test]
        fn fails_parsing_vec_of_tuples_if_unknown_attribute_in_the_middle_of_tuple() {
            let parser = <Vec<((u8, i16), bool)> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 true 4 5 --unknown"),
                ParseResult::Failed(ParseError::unknown_attribute("unknown")),
            );
        }
    }

    mod box_parser {
        use super::*;

        #[test]
        fn parse() {
            let parser = <Box<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("true 10"),
                ParseResult::Parsed(Box::new(true), "10")
            );
        }

        #[test]
        fn completion() {
            let parser = <Box<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("tr"),
                CompletionResult::Suggestions(vec!["ue".into()])
            );
        }
    }

    mod option_parser {
        use super::*;

        #[test]
        fn parse_some() {
            let parser = <Option<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("true remaining"),
                ParseResult::Parsed(Some(true), "remaining")
            );
        }

        #[test]
        fn parse_none_on_unknown_attribute() {
            let parser = <Option<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("--unknown"),
                ParseResult::Parsed(None, "--unknown")
            );
        }

        #[test]
        fn parse_none() {
            let parser = <Option<bool> as Parsable<()>>::new_parser(());
            assert_eq!(parser.parse(""), ParseResult::Parsed(None, ""));
            assert_eq!(parser.parse(")"), ParseResult::Parsed(None, ")"));
        }

        #[test]
        fn parse_tuple_of_options() {
            let parser = <Vec<(bool, Option<bool>)> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("true false true false"),
                ParseResult::Parsed(vec![(true, Some(false)), (true, Some(false))], "")
            );
            assert_eq!(
                parser.parse("true false true"),
                ParseResult::Parsed(vec![(true, Some(false)), (true, None)], "")
            );
            assert_eq!(
                parser.parse("true false true --unknown"),
                ParseResult::Parsed(vec![(true, Some(false)), (true, None)], "--unknown")
            );
        }

        #[test]
        fn completion() {
            let parser = <Option<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("tr"),
                CompletionResult::Suggestions(vec!["ue".into()])
            );
        }
    }

    mod parse_default {
        use super::*;
        use std::marker::PhantomData;

        #[test]
        fn parser_tuple_0() {
            let parser = <() as Parsable<()>>::new_parser(());
            assert_eq!(
                Parser::<()>::parse(&parser, "any"),
                ParseResult::Parsed((), "any")
            );
            assert_eq!(
                Parser::<()>::complete(&parser, "any"),
                CompletionResult::Consumed("any")
            );
        }

        #[test]
        fn parser_phantom_data() {
            let parser = <PhantomData<u8> as Parsable<()>>::new_parser(());
            assert_eq!(
                Parser::<()>::parse(&parser, "any"),
                ParseResult::Parsed(PhantomData, "any")
            );
            assert_eq!(
                Parser::<()>::complete(&parser, "any"),
                CompletionResult::Consumed("any")
            );
        }
    }
}
