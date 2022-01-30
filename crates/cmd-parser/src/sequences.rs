use crate::error::ParseError;
use crate::tokens::{TokenStream, TokenValue};
use crate::{CompletionResult, Parsable, ParseFailure, ParseResult, Parser};
use std::cmp::Ord;
use std::collections::{BTreeSet, HashSet, LinkedList, VecDeque};
use std::hash::Hash;
use std::marker::PhantomData;

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

    fn parse<'a>(&self, mut input: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        let mut is_first = true;
        let mut result = C::default();
        while !input.is_empty() {
            let item_result = input.with_nested(|input| self.inner_parser.parse(input));
            match item_result {
                Ok((value, remaining)) => {
                    result.append(value);
                    input = remaining;
                }
                Err(ParseFailure::Error(error)) => return Err(error.into()),
                Err(unrecognized @ ParseFailure::Unrecognized(_)) if is_first => {
                    return Err(unrecognized)
                }
                Err(ParseFailure::Unrecognized(unrecognized)) => {
                    match unrecognized.token().value() {
                        TokenValue::Attribute(_) => break,
                        TokenValue::Text(_) => {
                            return Err(ParseError::unknown(unrecognized.token()).into())
                        }
                    }
                }
            }
            is_first = false;
        }
        Ok((result, input))
    }

    fn complete<'a>(&self, mut input: TokenStream<'a>) -> CompletionResult<'a> {
        let mut is_first = true;
        while !input.is_empty() {
            let item_result = input.complete_nested(|input| self.inner_parser.complete(input));
            if let Some(remaining) = item_result.remaining {
                input = remaining;
            } else {
                return item_result;
            }

            if !item_result.value_consumed {
                if is_first {
                    return item_result;
                } else if matches!(input.peek(), Some(Ok(token)) if token.value().is_attribute()) {
                    return CompletionResult::consumed(input);
                } else {
                    return CompletionResult::failed();
                }
            }

            is_first = false;
        }
        CompletionResult::consumed(input)
    }
}

// pub struct DefaultValueParser<T> {
//     _phantom: PhantomData<T>,
// }
//
// impl<Ctx, T: Default> Parser<Ctx> for DefaultValueParser<T> {
//     type Value = T;
//
//     fn create(_ctx: Ctx) -> Self {
//         DefaultValueParser {
//             _phantom: PhantomData,
//         }
//     }
//
//     fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
//         ParseResult::Parsed(<T as Default>::default(), input)
//     }
//
//     fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
//         CompletionResult::Consumed(input)
//     }
// }
//
// impl<Ctx> Parsable<Ctx> for () {
//     type Parser = DefaultValueParser<()>;
// }
//
// impl<Ctx, T> Parsable<Ctx> for PhantomData<T> {
//     type Parser = DefaultValueParser<PhantomData<T>>;
// }
//
// macro_rules! gen_parsable_tuple {
//     ($parser_name:ident, $param_first:ident $($param:ident)*) => {
//         #[allow(non_snake_case)]
//         pub struct $parser_name<Ctx, $param_first: Parsable<Ctx>, $($param: Parsable<Ctx>),*> {
//             $param_first: $param_first::Parser,
//             $(
//                 $param: $param::Parser,
//             )*
//         }
//
//         impl<Ctx: Clone, $param_first: Parsable<Ctx>, $($param: Parsable<Ctx>),*> Parser<Ctx> for $parser_name<Ctx, $param_first, $($param),*> {
//             type Value = ($param_first, $($param,)*);
//
//             fn create(ctx: Ctx) -> Self {
//                 $parser_name {
//                     $param_first: $param_first::new_parser(ctx.clone()),
//                     $($param: $param::new_parser(ctx.clone())),*
//                 }
//             }
//
//             #[allow(non_snake_case)]
//             fn parse<'a>(&self, mut input: &'a str) -> ParseResult<'a, Self::Value> {
//                 let $param_first = match parse_inner(input, &self.$param_first) {
//                     ParseResult::Parsed(value, remaining) => {
//                         input = remaining;
//                         value
//                     }
//                     ParseResult::UnrecognizedAttribute(token, remaining) => {
//                         return ParseResult::UnrecognizedAttribute(token, remaining)
//                     }
//                     ParseResult::UnrecognizedVariant(variant) => {
//                         return ParseResult::UnrecognizedVariant(variant)
//                     }
//                     ParseResult::Failed(err) => {
//                         return ParseResult::Failed(err)
//                     }
//                 };
//                 $(
//                     let $param = match parse_inner(input, &self.$param) {
//                         ParseResult::Parsed(value, remaining) => {
//                             input = remaining;
//                             value
//                         }
//                         ParseResult::UnrecognizedAttribute(attr, _) => return ParseResult::Failed(ParseError::unknown_attribute(attr)),
//                         ParseResult::UnrecognizedVariant(variant) => return ParseResult::Failed(ParseError::unknown_variant(variant)),
//                          ParseResult::Failed(err) => {
//                             return ParseResult::Failed(err)
//                         }
//                     };
//                 )*
//                 ParseResult::Parsed(($param_first, $($param,)*), input)
//             }
//
//             fn complete<'a>(&self, mut input: &'a str) -> CompletionResult<'a> {
//                 match complete_inner(input, &self.$param_first) {
//                     CompletionResult::Consumed(remaining) => input = remaining,
//                     result @ CompletionResult::Suggestions(_) => return result,
//                     result @ CompletionResult::Unrecognized => return result,
//                 }
//                 $(
//                     match complete_inner(input, &self.$param) {
//                         CompletionResult::Consumed(remaining) => input = remaining,
//                         CompletionResult::Unrecognized => return CompletionResult::empty(),
//                         result => return result,
//                     }
//                 )*
//                 CompletionResult::Consumed(input)
//             }
//         }
//
//         impl<Ctx: Clone, $param_first: Parsable<Ctx>, $($param: Parsable<Ctx>),*> Parsable<Ctx> for ($param_first, $($param,)*) {
//             type Parser = $parser_name<Ctx, $param_first, $($param),*>;
//         }
//     }
// }
//
// gen_parsable_tuple!(TupleParser1, T1);
// gen_parsable_tuple!(TupleParser2, T1 T2);
// gen_parsable_tuple!(TupleParser3, T1 T2 T3);
// gen_parsable_tuple!(TupleParser4, T1 T2 T3 T4);
// gen_parsable_tuple!(TupleParser5, T1 T2 T3 T4 T5);
// gen_parsable_tuple!(TupleParser6, T1 T2 T3 T4 T5 T6);
// gen_parsable_tuple!(TupleParser7, T1 T2 T3 T4 T5 T6 T7);
// gen_parsable_tuple!(TupleParser8, T1 T2 T3 T4 T5 T6 T7 T8);
// gen_parsable_tuple!(TupleParser9, T1 T2 T3 T4 T5 T6 T7 T8 T9);
// gen_parsable_tuple!(TupleParser10, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10);
// gen_parsable_tuple!(TupleParser11, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11);
// gen_parsable_tuple!(TupleParser12, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12);
// gen_parsable_tuple!(TupleParser13, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13);
// gen_parsable_tuple!(TupleParser14, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14);
// gen_parsable_tuple!(TupleParser15, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15);
// gen_parsable_tuple!(TupleParser16, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16);
//
// pub struct BoxParser<Ctx, T: Parsable<Ctx>> {
//     inner_parser: T::Parser,
// }
//
// impl<Ctx, T: Parsable<Ctx>> Parser<Ctx> for BoxParser<Ctx, T> {
//     type Value = Box<T>;
//
//     fn create(ctx: Ctx) -> Self {
//         BoxParser {
//             inner_parser: T::new_parser(ctx),
//         }
//     }
//
//     fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
//         self.inner_parser.parse(input).map(Box::new)
//     }
//
//     fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
//         self.inner_parser.complete(input)
//     }
// }
//
// impl<Ctx, T: Parsable<Ctx>> Parsable<Ctx> for Box<T> {
//     type Parser = BoxParser<Ctx, T>;
// }
//
// pub struct OptionParser<Ctx, T: Parsable<Ctx>> {
//     inner_parser: T::Parser,
// }
//
// impl<Ctx, T: Parsable<Ctx>> Parser<Ctx> for OptionParser<Ctx, T> {
//     type Value = Option<T>;
//
//     fn create(ctx: Ctx) -> Self {
//         OptionParser {
//             inner_parser: T::new_parser(ctx),
//         }
//     }
//
//     fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
//         if has_tokens(input) {
//             match self.inner_parser.parse(input).map(Some) {
//                 ParseResult::UnrecognizedAttribute(_, _) => ParseResult::Parsed(None, input),
//                 result => result,
//             }
//         } else {
//             ParseResult::Parsed(None, input)
//         }
//     }
//
//     fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
//         self.inner_parser.complete(input)
//     }
// }
//
// impl<Ctx, T: Parsable<Ctx>> Parsable<Ctx> for Option<T> {
//     type Parser = OptionParser<Ctx, T>;
// }

#[cfg(test)]
mod tests {
    use crate::error::{ParseError, UnrecognizedToken};
    use crate::tokens::token_macro::token;
    use crate::tokens::{TokenStream, TokenValue};
    use crate::{CompletionResult, Parsable, ParseFailure, ParseResult, Parser};

    #[derive(PartialEq, Eq, Debug)]
    struct MockEnum;

    struct MockEnumParser;

    impl<Ctx> Parser<Ctx> for MockEnumParser {
        type Value = MockEnum;

        fn create(_ctx: Ctx) -> Self {
            MockEnumParser
        }

        fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
            let (token, remaining) = input.take().ok_or_else(ParseError::token_required)??;
            match token.value() {
                TokenValue::Text(text) => {
                    let text = text.parse_string();
                    if &text == "variant" {
                        Ok((MockEnum, remaining))
                    } else {
                        Err(UnrecognizedToken::new(token, remaining).into())
                    }
                }
                TokenValue::Attribute(_) => Err(UnrecognizedToken::new(token, remaining).into()),
            }
        }

        fn complete<'a>(&self, _input: TokenStream<'a>) -> CompletionResult<'a> {
            todo!()
        }
    }

    impl<Ctx> Parsable<Ctx> for MockEnum {
        type Parser = MockEnumParser;
    }

    macro_rules! test_parse {
        ($name:ident, $type:ty, $input:literal => Ok($value:expr, $next_token:expr)) => {
            #[test]
            fn $name() {
                let parser = <$type as Parsable<()>>::new_parser(());
                let stream = TokenStream::new($input);
                let (result, remaining) = parser.parse(stream).unwrap();
                assert_eq!(result, $value);
                assert_eq!(remaining.peek().transpose().unwrap(), $next_token);
            }
        };
        ($name:ident, $type:ty, $input:literal => Error($error:expr)) => {
            #[test]
            fn $name() {
                let parser = <$type as Parsable<()>>::new_parser(());
                let stream = TokenStream::new($input);
                let error = parser.parse(stream).unwrap_err();
                match error {
                    ParseFailure::Error(error) => assert_eq!(error, $error),
                    ParseFailure::Unrecognized(unrecognized) => {
                        panic!("expected Error, but found {:?}", unrecognized)
                    }
                }
            }
        };
        ($name:ident, $type:ty, $input:literal => Unrecognized($token:expr, $next_token:expr)) => {
            #[test]
            fn $name() {
                let parser = <$type as Parsable<()>>::new_parser(());
                let stream = TokenStream::new($input);
                let error = parser.parse(stream).unwrap_err();
                match error {
                    ParseFailure::Error(error) => {
                        panic!("expected Unrecognized, but found {:?}", error)
                    }
                    ParseFailure::Unrecognized(unrecognized) => {
                        assert_eq!(unrecognized.token(), $token);
                        assert_eq!(
                            unrecognized.remaining().peek().transpose().unwrap(),
                            $next_token
                        );
                    }
                }
            }
        };
    }

    macro_rules! test_complete {
        ($name:ident, $type:ty, $input:literal => { consumed: $consumed:expr, remaining: $remaining:expr, suggestions: [$($suggestion:expr),*] $(,)?}) => {
            #[test]
            #[allow(clippy::bool_assert_comparison)]
            fn $name() {
                let parser = <$type as Parsable<()>>::new_parser(());
                let stream = TokenStream::new($input);
                let result = parser.complete(stream);
                assert_eq!(result.suggestions, HashSet::from([$($suggestion.into()),*]));
                assert_eq!(result.value_consumed, $consumed);
                assert_eq!(result.remaining.map(|input| input.peek().transpose().unwrap()), $remaining);
            }
        };
    }

    mod collection_parser {
        use super::*;
        use std::collections::{BTreeSet, HashSet, LinkedList, VecDeque};

        test_parse!(parse_empty, Vec<i32>, "" => Ok(vec![], None));
        test_parse!(
            parse_flat_vec, Vec<i32>,
            "1 2 3 4 5" => Ok(vec![1, 2, 3, 4, 5], None)
        );
        test_parse!(
            parse_flat_vec_deque, VecDeque<i32>,
            "1 2 3 4 5" => Ok(VecDeque::from([1, 2, 3, 4, 5]), None)
        );
        test_parse!(
            parse_flat_linked_list, LinkedList<i32>,
            "1 2 3 4 5" => Ok(LinkedList::from([1, 2, 3, 4, 5]), None)
        );
        test_parse!(
            parse_flat_hash_set, HashSet<i32>,
            "1 2 3 4 5" => Ok(HashSet::from([1, 2, 3, 4, 5]), None)
        );
        test_parse!(
            parse_flat_btree_set, BTreeSet<i32>,
            "1 2 3 4 5" => Ok(BTreeSet::from([1, 2, 3, 4, 5]), None)
        );
        test_parse!(
            parse_nested, Vec<Vec<i32>>,
            "() (1 2 3) 4 5 6 7" => Ok(vec![vec![], vec![1, 2, 3], vec![4, 5, 6, 7]], None)
        );

        test_parse!(
            stops_on_unknown_attribute, Vec<i32>,
            "1 2 3 --unknown 4 5" => Ok(vec![1, 2, 3], Some(token!(--"unknown")))
        );
        test_parse!(
            stops_on_unknown_attribute_with_nested_vecs, Vec<Vec<i32>>,
            "(1 2) 3 --unknown 4 5" => Ok(vec![vec![1, 2], vec![3]], Some(token!(--"unknown")))
        );

        test_parse!(
            stops_on_unknown_attribute_on_first_item, Vec<i32>,
            "--unknown 1 2" => Unrecognized(token!(--"unknown"), Some(token!("1")))
        );
        test_parse!(
            stops_on_unknown_attribute_on_first_nested_vec, Vec<Vec<i32>>,
            "--unknown 0 (1 2) (3 4) 5" => Unrecognized(token!(--"unknown"), Some(token!("0")))
        );

        test_parse!(
            parse_error, Vec<i32>,
            "1 2 nan 3 4" => Error(ParseError::invalid(token!("nan"), None).expected("integer"))
        );
        test_parse!(
            fails_on_unknown_attribure_with_parenthesis, Vec<Vec<i32>>,
            "(1 2) (3 --unknown) 4 5" => Error(ParseError::unknown(token!(--"unknown")))
        );
        test_parse!(
            fails_on_unknown_attribure_with_parenthesis_first, Vec<Vec<i32>>,
            "(1 2) (--unknown 3) 4 5" => Error(ParseError::unknown(token!(--"unknown")))
        );
        test_parse!(
            stops_on_unknown_attribute_on_first_nested_vec_inside_parenthesis, Vec<Vec<i32>>,
            "(--unknown 1 2) (3 4) 5" => Error(ParseError::unknown(token!(--"unknown")))
        );

        test_parse!(
            returns_unrecognized_variant_if_first_is_unrecognized, Vec<MockEnum>,
            "unknown variant" => Unrecognized(token!("unknown"), Some(token!("variant", last)))
        );
        test_parse!(
            fails_if_first_is_unrecognized_in_parenthesis, Vec<Vec<MockEnum>>,
            "(unknown variant)" => Error(ParseError::unknown(token!("unknown")))
        );
        test_parse!(
            fails_if_variant_is_unrecognized, Vec<MockEnum>,
            "variant unknown" => Error(ParseError::unknown(token!("unknown", last)))
        );

        test_complete!(complete_first, Vec<bool>, "tr" => {
            consumed: true,
            remaining: None,
            suggestions: ["ue"],
        });

        test_complete!(complete_not_first, Vec<bool>, "true fa" => {
            consumed: true,
            remaining: None,
            suggestions: ["lse"],
        });

        test_complete!(complete_consumed, Vec<bool>, "true false " => {
            consumed: true,
            remaining: Some(None),
            suggestions: [],
        });

        test_complete!(suggestion_stops_on_unknown_arg, Vec<bool>, "true --unknown false " => {
            consumed: true,
            remaining: Some(Some(token!(--"unknown"))),
            suggestions: [],
        });

        test_complete!(suggestion_stops_on_unknown_arg_first, Vec<bool>, "--unknown true false " => {
            consumed: false,
            remaining: Some(Some(token!(--"unknown"))),
            suggestions: [],
        });

        test_complete!(suggestion_nested, Vec<Vec<bool>>, "(true false) (false) (fal" => {
            consumed: true,
            remaining: None,
            suggestions: ["se"],
        });

        test_complete!(suggestion_nested_closed, Vec<Vec<bool>>, "(true false) (false) (fal)" => {
            consumed: true,
            remaining: Some(None),
            suggestions: [],
        });
    }

    // mod tuple_parser {
    //     use super::*;

    //     #[test]
    //     fn parse_tuple() {
    //         let parser =
    //             <(u8, (u16, bool), (i32, i32, i32), (bool,)) as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("1 2 true 4 5 6 false remaining"),
    //             ParseResult::Parsed((1, (2, true), (4, 5, 6), (false,)), "remaining"),
    //         );
    //         assert_eq!(
    //             parser.parse("1 (2 true) (4 5 6) (false) remaining"),
    //             ParseResult::Parsed((1, (2, true), (4, 5, 6), (false,)), "remaining"),
    //         );
    //     }

    //     #[test]
    //     fn parse_wrong_tokens_number() {
    //         let parser = <(u8, (u8, u8)) as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("1 2"),
    //             ParseResult::Failed(ParseError::token_required("integer"))
    //         );
    //         assert_eq!(
    //             parser.parse("1 (3 4 5)"),
    //             ParseResult::Failed(ParseError::unexpected_token("5".into()))
    //         );
    //     }

    //     #[test]
    //     fn parse_ivalid_token() {
    //         let parser = <(u8, u8) as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("5 abc"),
    //             ParseResult::Failed(ParseError::token_parse("abc".into(), None, "integer"))
    //         );
    //     }

    //     #[test]
    //     fn parses_unrecognized_if_starts_with_unknown_attribute() {
    //         let parser = <(u8, u8) as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("--unknown"),
    //             ParseResult::UnrecognizedAttribute("unknown".into(), ""),
    //         );
    //         assert_eq!(parser.complete("--unknown"), CompletionResult::Unrecognized,);
    //     }

    //     #[test]
    //     fn parse_error_if_contains_unknown_attribute() {
    //         let parser = <(u8, u8) as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("1 --unknown"),
    //             ParseResult::Failed(ParseError::unknown_attribute("unknown")),
    //         );
    //         assert_eq!(parser.complete("1 --unknown"), CompletionResult::empty(),);
    //     }

    //     #[test]
    //     fn completion() {
    //         let parser = <(u8, (bool, u8)) as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.complete("5 fa"),
    //             CompletionResult::Suggestions(vec!["lse".into()])
    //         );
    //         assert_eq!(parser.complete("5 false 4"), CompletionResult::empty());
    //         assert_eq!(
    //             parser.complete("5 false 4 "),
    //             CompletionResult::Consumed("")
    //         );
    //         assert_eq!(
    //             parser.complete("5 false 4 6"),
    //             CompletionResult::Consumed("6")
    //         );
    //     }

    //     #[test]
    //     fn stops_vec_of_tuples() {
    //         let parser = <Vec<((u8, i16), bool)> as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("1 2 true 4 5 false --unknown"),
    //             ParseResult::Parsed(vec![((1, 2), true), ((4, 5), false)], "--unknown"),
    //         );
    //     }

    //     #[test]
    //     fn fails_parsing_vec_of_tuples_if_unknown_attribute_in_the_middle_of_tuple() {
    //         let parser = <Vec<((u8, i16), bool)> as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("1 2 true 4 5 --unknown"),
    //             ParseResult::Failed(ParseError::unknown_attribute("unknown")),
    //         );
    //     }

    //     #[test]
    //     fn returns_unrecognized_variant_if_first_is_unrecognized() {
    //         let parser = <(MockEnum, MockEnum) as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("unknown variant"),
    //             ParseResult::UnrecognizedVariant("unknown".into()),
    //         );
    //     }

    //     #[test]
    //     fn fails_if_variant_is_unrecognized() {
    //         let parser = <(MockEnum, MockEnum) as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("variant unknown variant"),
    //             ParseResult::Failed(ParseError::unknown_variant("unknown")),
    //         );
    //     }
    // }

    // mod box_parser {
    //     use super::*;

    //     #[test]
    //     fn parse() {
    //         let parser = <Box<bool> as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("true 10"),
    //             ParseResult::Parsed(Box::new(true), "10")
    //         );
    //     }

    //     #[test]
    //     fn completion() {
    //         let parser = <Box<bool> as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.complete("tr"),
    //             CompletionResult::Suggestions(vec!["ue".into()])
    //         );
    //     }
    // }

    // mod option_parser {
    //     use super::*;

    //     #[test]
    //     fn parse_some() {
    //         let parser = <Option<bool> as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("true remaining"),
    //             ParseResult::Parsed(Some(true), "remaining")
    //         );
    //     }

    //     #[test]
    //     fn parse_none_on_unknown_attribute() {
    //         let parser = <Option<bool> as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("--unknown"),
    //             ParseResult::Parsed(None, "--unknown")
    //         );
    //     }

    //     #[test]
    //     fn parse_none() {
    //         let parser = <Option<bool> as Parsable<()>>::new_parser(());
    //         assert_eq!(parser.parse(""), ParseResult::Parsed(None, ""));
    //         assert_eq!(parser.parse(")"), ParseResult::Parsed(None, ")"));
    //     }

    //     #[test]
    //     fn parse_tuple_of_options() {
    //         let parser = <Vec<(bool, Option<bool>)> as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.parse("true false true false"),
    //             ParseResult::Parsed(vec![(true, Some(false)), (true, Some(false))], "")
    //         );
    //         assert_eq!(
    //             parser.parse("true false true"),
    //             ParseResult::Parsed(vec![(true, Some(false)), (true, None)], "")
    //         );
    //         assert_eq!(
    //             parser.parse("true false true --unknown"),
    //             ParseResult::Parsed(vec![(true, Some(false)), (true, None)], "--unknown")
    //         );
    //     }

    //     #[test]
    //     fn completion() {
    //         let parser = <Option<bool> as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             parser.complete("tr"),
    //             CompletionResult::Suggestions(vec!["ue".into()])
    //         );
    //     }
    // }

    // mod parse_default {
    //     use super::*;
    //     use std::marker::PhantomData;

    //     #[test]
    //     fn parser_tuple_0() {
    //         let parser = <() as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "any"),
    //             ParseResult::Parsed((), "any")
    //         );
    //         assert_eq!(
    //             Parser::<()>::complete(&parser, "any"),
    //             CompletionResult::Consumed("any")
    //         );
    //     }

    //     #[test]
    //     fn parser_phantom_data() {
    //         let parser = <PhantomData<u8> as Parsable<()>>::new_parser(());
    //         assert_eq!(
    //             Parser::<()>::parse(&parser, "any"),
    //             ParseResult::Parsed(PhantomData, "any")
    //         );
    //         assert_eq!(
    //             Parser::<()>::complete(&parser, "any"),
    //             CompletionResult::Consumed("any")
    //         );
    //     }
    // }
}
