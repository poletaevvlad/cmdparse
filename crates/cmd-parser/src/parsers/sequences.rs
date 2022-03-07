use crate::error::{ParseError, ParseFailure};
use crate::tokens::{Token, TokenStream};
use crate::{CompletionResult, Parsable, ParseResult, Parser};
use std::cmp::Ord;
use std::collections::{BTreeSet, HashSet, LinkedList, VecDeque};
use std::hash::Hash;
use std::marker::PhantomData;

/// Parse any one-dimensional collection of items
///
/// See [`CollectionParser`] documentation for details.
pub trait ParsableCollection {
    /// The type of a collection member.
    type Item;

    /// Adds an item to the collection.
    fn append(&mut self, item: Self::Item);
}

macro_rules! impl_parsable_collection {
    ($ty:ty $(where T: $bound_1:ident $(+ $bound:ident)*)? { $append:item }) => {
        impl<T $(: $bound_1 $(+ $bound)*)?> ParsableCollection for $ty {
            type Item = T;
            $append
        }

        impl<Ctx: Clone, T: Parsable<Ctx> $(+ $bound_1 $(+ $bound)*)?> Parsable<Ctx> for $ty {
            type Parser = CollectionParser<Self, T::Parser>;
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

/// Parser implementation for any one-dimensional collection of items
///
/// `CollectionParser` sequentially parses items using their default parser and constructs an
/// instance of a [`ParsableCollection`]. The iteration stops if any of the parsing attempts fails
/// with an error, when the end of the token stream is reached (all tokens are consumed or either
/// the comment or the closing parenthesis is encountered) or when the item parser fails because of
/// an unrecognized attribute.
///
/// Because `CollectionParser` tries to consume as many tokens as possible it may cause
/// difficulties with multiple collections in a sequence. Consider the issue of parsing nested
/// collections, for example `Vec<Vec<i32>>. If the items are presented sequentially in the input
/// stream all of the tokens are going to be consumed by the first item.
///
/// ```
/// use cmd_parser::parse;
///
/// # fn main() -> Result<(), cmd_parser::error::ParseError<'static>> {
/// let result = parse::<_, Vec<Vec<i32>>>("1 2 3 4 5", ())?;
/// assert_eq!(result, vec![vec![1, 2, 3, 4, 5]]);
/// # Ok(())
/// # }
/// ```
///
/// This problem can be resolved by enclosing each of the inner collections' items with
/// parenthesis:
///
/// ```
/// # use cmd_parser::parse;
/// # fn main() -> Result<(), cmd_parser::error::ParseError<'static>> {
/// let result = parse::<_, Vec<Vec<i32>>>("(1 2 3) (4 5)", ())?;
/// assert_eq!(result, vec![vec![1, 2, 3], vec![4, 5]]);
/// # Ok(())
/// # }
/// ```
/// # Custom collections
///
/// `cmd_parser` implements [`Parsable`] using `CollectionParser` as a default parser for
/// collections from the Rust's standard library: [`Vec`], [`VecDeque`], [`LinkedList`],
/// [`HashSet`], [`BTreeSet`].
///
/// It is easy to extend this list with a custom collection. To do so, one need to implement
/// [`Default`], [`ParsableCollection`], and [`Parsable`] traits.
///
/// ```
/// use cmd_parser::parsers::{CollectionParser, ParsableCollection};
/// use cmd_parser::{parse, Parsable};
///
/// #[derive(Default, Debug, PartialEq, Eq)]
/// struct MyBitArray(u128);
///
/// impl ParsableCollection for MyBitArray {
///     type Item = bool;
///
///     fn append(&mut self, item: Self::Item) {
///         self.0 = self.0 << 1 | (item as u128);
///     }
/// }
///
/// impl<Ctx: Clone> Parsable<Ctx> for MyBitArray {
///     type Parser = CollectionParser<Self, <bool as Parsable<Ctx>>::Parser>;
/// }
///
/// # fn main() -> Result<(), cmd_parser::error::ParseError<'static>> {
/// let result = parse::<_, MyBitArray>("true false false true true", ())?;
/// assert_eq!(result, MyBitArray(0b10011));
/// # Ok(())
/// # }
/// ```
pub struct CollectionParser<C, P> {
    _collection_phanton: PhantomData<C>,
    _parser_phantom: PhantomData<P>,
}

impl<C, P> Default for CollectionParser<C, P> {
    fn default() -> Self {
        Self {
            _collection_phanton: PhantomData,
            _parser_phantom: PhantomData,
        }
    }
}

impl<Ctx: Clone, C: ParsableCollection + Default, P: Parser<Ctx, Value = C::Item>> Parser<Ctx>
    for CollectionParser<C, P>
{
    type Value = C;

    fn parse<'a>(&self, mut input: TokenStream<'a>, ctx: Ctx) -> ParseResult<'a, Self::Value> {
        let parser = P::default();
        let mut is_first = true;
        let mut result = C::default();
        while !input.is_empty() {
            let item_result = input.with_nested(|input| parser.parse(input, ctx.clone()));
            match item_result {
                Ok((value, remaining)) => {
                    result.append(value);
                    input = remaining;
                }
                Err(ParseFailure::Error(error)) => return Err(error.into()),
                Err(unrecognized @ ParseFailure::Unrecognized(_)) if is_first => {
                    return Err(unrecognized)
                }
                Err(ParseFailure::Unrecognized(unrecognized)) => match unrecognized.token() {
                    Token::Attribute(_) => break,
                    Token::Text(_) => return Err(ParseError::unknown(unrecognized.token()).into()),
                },
            }
            is_first = false;
        }
        Ok((result, input))
    }

    fn complete<'a>(&self, mut input: TokenStream<'a>, ctx: Ctx) -> CompletionResult<'a> {
        let parser = P::default();
        let mut is_first = true;
        let mut suggestions = BTreeSet::new();
        while !input.is_empty() {
            let item_result = input.complete_nested(|input| parser.complete(input, ctx.clone()));
            if let Some(remaining) = item_result.remaining {
                input = remaining;
            } else {
                return item_result.add_suggestions(suggestions);
            }

            if !item_result.value_consumed {
                let result = if is_first {
                    item_result
                } else if matches!(input.peek(), Some(Ok(token)) if token.is_attribute()) {
                    CompletionResult::new(input, true).add_suggestions(item_result.suggestions)
                } else {
                    CompletionResult::new_final(false).add_suggestions(item_result.suggestions)
                };
                return result.add_suggestions(suggestions);
            }

            suggestions.extend(item_result.suggestions);
            is_first = false;
        }
        CompletionResult::new(input, true).add_suggestions(suggestions)
    }
}

/// Parser implementation that always returns a default value of its generic argument
///
/// This parser never fails, does not consume any tokens nor recognizes any attributes.
///
/// `DefaultValueParser` is used for types such as [`PhantomData`] or `()`
pub struct DefaultValueParser<T> {
    _phantom: PhantomData<T>,
}

impl<T> Default for DefaultValueParser<T> {
    fn default() -> Self {
        DefaultValueParser {
            _phantom: PhantomData,
        }
    }
}

impl<Ctx, T: Default> Parser<Ctx> for DefaultValueParser<T> {
    type Value = T;

    fn parse<'a>(&self, input: TokenStream<'a>, _ctx: Ctx) -> ParseResult<'a, Self::Value> {
        Ok((<T as Default>::default(), input))
    }

    fn complete<'a>(&self, input: TokenStream<'a>, _ctx: Ctx) -> CompletionResult<'a> {
        CompletionResult::new(input, true)
    }
}

impl<Ctx> Parsable<Ctx> for () {
    type Parser = DefaultValueParser<()>;
}

impl<Ctx, T> Parsable<Ctx> for PhantomData<T> {
    type Parser = DefaultValueParser<PhantomData<T>>;
}

/// Parser implementation for tuples of different sizes
///
/// Due to the limitations of Rust's type system, tuples of different sizes must have distinct
/// parsers. The parsers for tuples for up to 16 elements are defined in this module. These types
/// are rearely need to be used on its own due to the fact that corresponding tuples implement
/// [`Parsable`] trait.
///
/// For a tuple to be parsable, every type it contains must implement [`Parsable`] trait that
/// supports compatible contexts.
///
/// # Examples
/// ```
/// use cmd_parser::parse;
///
/// # fn main() -> Result<(), cmd_parser::error::ParseError<'static>> {
/// let value = parse::<_, (u8, i32, bool)>("10 42 false", ())?;
/// assert_eq!(value, (10, 42, false));
/// # Ok(())
/// # }
/// ```
pub mod tuples {
    use super::*;

    macro_rules! gen_parsable_tuple {
        ($parser_name:ident, $param_first:ident $($param:ident)*) => {
            /// Parser implementation of a tuple of a specific size
            #[allow(non_snake_case)]
            pub struct $parser_name<$param_first, $($param),*> {
                $param_first: PhantomData<$param_first>,
                $( $param: PhantomData<$param>, )*
            }

            impl<$param_first $(, $param)*> Default for $parser_name<$param_first $(, $param)*> {
                fn default() -> Self {
                    $parser_name {
                        $param_first: PhantomData,
                        $( $param: PhantomData, )*
                    }
                }
            }

            impl<Ctx: Clone, $param_first: Parser<Ctx>, $($param: Parser<Ctx>),*> Parser<Ctx> for $parser_name<$param_first, $($param),*> {
                type Value = ($param_first::Value, $($param::Value,)*);

                #[allow(non_snake_case, unused_mut)]
                fn parse<'a>(&self, input: TokenStream<'a>, ctx: Ctx) -> ParseResult<'a, Self::Value> {
                    let parser = $param_first::default();
                    let ($param_first, mut input) = input.with_nested(|input| parser.parse(input, ctx.clone()))?;
                    $(
                        let parser = $param::default();
                        let $param = match input.with_nested(|input| parser.parse(input, ctx.clone())) {
                            Ok((value, remaining)) => {
                                input = remaining;
                                value
                            }
                            Err(ParseFailure::Unrecognized(unrecognized)) => return Err(unrecognized.into_error().into()),
                            Err(error) => return Err(error),
                        };
                    )*
                    Ok((($param_first, $($param,)*), input))
                }

                #[allow(unused_mut)]
                fn complete<'a>(&self, mut input: TokenStream<'a>, ctx: Ctx) -> CompletionResult<'a> {
                    let parser = $param_first::default();
                    let result = input.complete_nested(|input| parser.complete(input, ctx.clone()));
                    if let Some(remaining) = result.remaining {
                        input = remaining;
                    } else {
                        return result;
                    }
                    if !result.value_consumed {
                        return result;
                    }
                    let mut suggestions = result.suggestions;

                    $(
                        let parser = $param::default();
                        let result = input.complete_nested(|input| parser.complete(input, ctx.clone()));
                        if let Some(remaining) = result.remaining {
                            input = remaining;
                        } else {
                            return result.add_suggestions(suggestions);
                        }
                        if !result.value_consumed {
                            return CompletionResult::new_final(true).add_suggestions(result.suggestions).add_suggestions(suggestions);
                        }
                        suggestions.extend(result.suggestions);
                     )*
                     CompletionResult::new(input, true).add_suggestions(suggestions)
                }
            }

            impl<Ctx: Clone, $param_first: Parsable<Ctx>, $($param: Parsable<Ctx>),*> Parsable<Ctx> for ($param_first, $($param,)*) {
                type Parser = $parser_name<$param_first::Parser, $($param::Parser),*>;
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
}

/// Parser implementation for [`Option<T>`]
///
/// This parser calls delegates the parsing and completion to the parser passed as its generic
/// parameter type and returns `Some` if the parsing succeded, `None` if the inner parser reported
/// an unrecognized attribute or if the token stream is empty. If the inner parser fails due to an
/// error, this parser fails also.
///
/// When performing completion, the value is always considered to be consumed.
pub struct OptionParser<P> {
    _inner: PhantomData<P>,
}

impl<P> Default for OptionParser<P> {
    fn default() -> Self {
        Self {
            _inner: PhantomData,
        }
    }
}

impl<Ctx, P: Parser<Ctx>> Parser<Ctx> for OptionParser<P> {
    type Value = Option<P::Value>;

    fn parse<'a>(&self, input: TokenStream<'a>, ctx: Ctx) -> ParseResult<'a, Self::Value> {
        if input.is_empty() {
            Ok((None, input))
        } else {
            let parser = P::default();
            match parser.parse(input, ctx) {
                Ok((value, remaining)) => Ok((Some(value), remaining)),
                Err(error @ ParseFailure::Error(_)) => Err(error),
                Err(ParseFailure::Unrecognized(unrecognized)) => {
                    if let Token::Attribute(_) = unrecognized.token() {
                        Ok((None, input))
                    } else {
                        Err(unrecognized.into())
                    }
                }
            }
        }
    }

    fn complete<'a>(&self, input: TokenStream<'a>, ctx: Ctx) -> CompletionResult<'a> {
        P::default().complete(input, ctx).set_consumed(true)
    }
}

impl<Ctx, T: Parsable<Ctx>> Parsable<Ctx> for Option<T> {
    type Parser = OptionParser<T::Parser>;
}

/// Generic fallible transformation between two types for parsing
///
/// See the documentation for [`TransformParser`] for more details.
pub trait ParsableTransformation<O> {
    /// The type that is going to be parsed using its default parser and then transformed.
    type Input;

    /// Performs the transformation.
    fn transform(input: Self::Input) -> Result<O, ParseError<'static>>;
}

/// Parser implementation that performs type converstion and validation after the parsing is
/// complete
///
/// This parser delegates the parsing and completion to the underlying parser. If parsing is
/// successfull, it calls the [`ParsableTransformation`]'s `transform` method which maps the parsed
/// value onto another type.
///
/// # Examples
///
/// This parser can be used for type converstion (as it is done for `T -> Box<T>`) or for data
/// validation.
///
/// For example, you can implement [`Parsable`] for a newtype that would use the inner data's
/// default parser implementation (without using derive macro):
///
/// ```
/// use cmd_parser::{Parsable, parse};
/// use cmd_parser::error::ParseError;
/// use cmd_parser::parsers::{TransformParser, ParsableTransformation};
///
/// #[derive(Debug, PartialEq, Eq)]
/// struct PostId(usize);
///
/// impl ParsableTransformation<PostId> for PostId {
///     type Input = usize;
///
///     fn transform(input: usize) -> Result<PostId, ParseError<'static>> {
///         Ok(PostId(input))
///     }
/// }
///
/// impl<Ctx> Parsable<Ctx> for PostId {
///     type Parser = TransformParser<<usize as Parsable<Ctx>>::Parser, PostId, PostId>;
/// }
///
/// # fn main() -> Result<(), cmd_parser::error::ParseError<'static>> {
/// let value = parse::<_, PostId>("42", ())?;
/// assert_eq!(value, PostId(42));
/// # Ok(())
/// # }
/// ```
///
/// Note that the example above can be greatly simplified by using a Parsable derive macro.
/// Although the implementation details are going to be different, the following example is
/// functionally equivalent to the one above:
///
/// ```
/// # use cmd_parser::{Parsable, parse};
/// #
/// #[derive(Debug, PartialEq, Eq, Parsable)]
/// struct PostId(usize);
///
/// # fn main() -> Result<(), cmd_parser::error::ParseError<'static>> {
/// let value = parse::<_, PostId>("42", ())?;
/// assert_eq!(value, PostId(42));
/// # Ok(())
/// # }
/// ```
///
/// It is not important which trait implements [`ParsableTransformation`]. The trait is designed
/// this way to circumvent the limitations of the Rust's type system: foreign traits cannot be
/// implemented for foreign types. Also, it allows the user to define multiple different
/// transformations for the same type which is useful for data validation.
///
/// ```
/// use cmd_parser::{Parsable, parse_parser};
/// use cmd_parser::error::ParseError;
/// use cmd_parser::parsers::{TransformParser, ParsableTransformation};
///
/// struct IsPowerOfTwo;
///
/// impl ParsableTransformation<u64> for IsPowerOfTwo {
///     type Input = u64;
///
///     fn transform(input: u64) -> Result<u64, ParseError<'static>> {
///         if input.is_power_of_two() {
///             Ok(input)
///         } else {
///             Err(ParseError::custom("not a power of two"))
///         }
///     }
/// }
///
/// type PowerOfTwoParser<Ctx> =
///     TransformParser<<u64 as Parsable<Ctx>>::Parser, IsPowerOfTwo, u64>;
///
/// # fn main() {
/// assert!(parse_parser::<_, PowerOfTwoParser<()>>("16", ()).is_ok());
/// assert!(parse_parser::<_, PowerOfTwoParser<()>>("14", ()).is_err());
/// # }
/// ```
pub struct TransformParser<P, T, O> {
    _parser_phantom: PhantomData<P>,
    _transformer_phantom: PhantomData<T>,
    _output_phantom: PhantomData<O>,
}

impl<P, T, O> Default for TransformParser<P, T, O> {
    fn default() -> Self {
        TransformParser {
            _parser_phantom: PhantomData,
            _transformer_phantom: PhantomData,
            _output_phantom: PhantomData,
        }
    }
}

impl<Ctx, P, T, O> Parser<Ctx> for TransformParser<P, T, O>
where
    P: Parser<Ctx>,
    T: ParsableTransformation<O, Input = P::Value>,
{
    type Value = O;

    fn parse<'a>(&self, input: TokenStream<'a>, ctx: Ctx) -> ParseResult<'a, Self::Value> {
        let parser = P::default();
        let (value, remaining) = parser.parse(input, ctx)?;
        let transformed = <T as ParsableTransformation<O>>::transform(value)?;
        Ok((transformed, remaining))
    }

    fn complete<'a>(&self, input: TokenStream<'a>, ctx: Ctx) -> CompletionResult<'a> {
        let parser = P::default();
        parser.complete(input, ctx)
    }
}

impl<T> ParsableTransformation<Box<T>> for T {
    type Input = Self;

    fn transform(input: Self::Input) -> Result<Box<T>, ParseError<'static>> {
        Ok(Box::new(input))
    }
}

impl<Ctx, T: Parsable<Ctx>> Parsable<Ctx> for Box<T> {
    type Parser = TransformParser<T::Parser, T, Box<T>>;
}

#[cfg(test)]
mod tests {
    use crate::error::{ParseError, UnrecognizedToken};
    use crate::testing::{test_complete, test_parse, token};
    use crate::tokens::{Token, TokenStream};
    use crate::{CompletionResult, Parsable, ParseResult, Parser};

    #[derive(PartialEq, Eq, Debug)]
    struct MockEnum;

    #[derive(Default)]
    struct MockEnumParser;

    impl<Ctx> Parser<Ctx> for MockEnumParser {
        type Value = MockEnum;

        fn parse<'a>(&self, input: TokenStream<'a>, _ctx: Ctx) -> ParseResult<'a, Self::Value> {
            let (token, remaining) = input.take().ok_or_else(ParseError::token_required)??;
            match token {
                Token::Text(text) => {
                    let text = text.parse_string();
                    if &text == "variant" {
                        Ok((MockEnum, remaining))
                    } else {
                        Err(UnrecognizedToken::new(token, remaining).into())
                    }
                }
                Token::Attribute(_) => Err(UnrecognizedToken::new(token, remaining).into()),
            }
        }

        fn complete<'a>(&self, _input: TokenStream<'a>, _ctx: Ctx) -> CompletionResult<'a> {
            todo!()
        }
    }

    impl<Ctx> Parsable<Ctx> for MockEnum {
        type Parser = MockEnumParser;
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
            "unknown variant" => Unrecognized(token!("unknown"), Some(token!("variant")))
        );
        test_parse!(
            fails_if_first_is_unrecognized_in_parenthesis, Vec<Vec<MockEnum>>,
            "(unknown variant)" => Error(ParseError::unknown(token!("unknown")))
        );
        test_parse!(
            fails_if_variant_is_unrecognized, Vec<MockEnum>,
            "variant unknown" => Error(ParseError::unknown(token!("unknown")))
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

    mod tuple_parser {
        use super::*;

        test_parse!(
            parse_tuple, (u8, (u16, bool), (i32, i32, i32), (bool,)),
            "1 2 true 4 5 6 false remaining" => Ok((1, (2, true), (4, 5, 6), (false,)), Some(token!("remaining")))
        );
        test_parse!(
            parse_tuple_parens, (u8, (u16, bool), (i32, i32, i32), (bool,)),
            "1 (2 true) (4 5 6) (false) remaining" => Ok((1, (2, true), (4, 5, 6), (false,)), Some(token!("remaining")))
        );
        test_parse!(
            too_few_tokens, (u8, (u8, u8)),
            "1 2" => Error(ParseError::token_required().expected("integer"))
        );
        test_parse!(
            too_many_tokens, (u8, (u8, u8)),
            "1 (3 4 5)" => Error(ParseError::unknown(token!("5")))
        );
        test_parse!(
            invalid_token, (u8, u8),
            "5 abc" => Error(ParseError::invalid(token!("abc"), None).expected("integer"))
        );
        test_parse!(
            unrecognized_if_starts_with_unknown_attribute, (u8, u8),
            "--unknown 5" => Unrecognized(token!(--"unknown"), Some(token!("5")))
        );
        test_parse!(
            error_if_contains_unknown_attribute, (u8, u8),
            "1 --unknown" => Error(ParseError::unknown(token!(--"unknown")))
        );
        test_parse!(
            vec_of_tuples, Vec<((u8, i16), bool)>,
            "1 2 true 4 5 false --unknown" => Ok(vec![((1, 2), true), ((4, 5), false)], Some(token!(--"unknown")))
        );
        test_parse!(
            returns_unrecognized_variant_if_first_is_unrecognized, (MockEnum, MockEnum),
            "unknown variant" => Unrecognized(token!("unknown"), Some(token!("variant")))
        );
        test_parse!(
            fails_if_variant_is_unrecognized, (MockEnum, MockEnum),
            "variant unknown remaining" => Error(ParseError::unknown(token!("unknown")))
        );

        test_complete!(complete_suggestions, (u8, (bool, u8)), "5 fa" => {
            consumed: true,
            remaining: None,
            suggestions: ["lse"],
        });
        test_complete!(complete_consumed, (u8, (bool, u8)), "5 false 4 6" => {
            consumed: true,
            remaining: Some(Some(token!("6"))),
            suggestions: [],
        });
        test_complete!(complete_unexpected_attr, (u8, (bool, u8)), "5 false --unknown 6" => {
            consumed: true,
            remaining: None,
            suggestions: [],
        });
        test_complete!(complete_unexpected_attr_first, (u8, (bool, u8)), "--unknown 5 false 4 6" => {
            consumed: false,
            remaining: Some(Some(token!(--"unknown"))),
            suggestions: [],
        });
    }

    mod box_parser {
        use super::*;

        test_parse!(
            parse, Box<bool>,
            "true 10" => Ok(Box::new(true), Some(token!("10")))
        );
        test_complete!(completion, Box<bool>, "tr" => {
            consumed: true,
            remaining: None,
            suggestions: ["ue"],
        });
    }

    mod option_parser {
        use super::*;

        test_parse!(
            parse_some, Option<bool>,
            "true remaining" => Ok(Some(true), Some(token!("remaining")))
        );
        test_parse!(
            parse_none_empty, Option<bool>,
            "" => Ok(None, None)
        );
        test_parse!(
            parse_none_on_unknown_attribute, Option<bool>,
            "--unknown" => Ok(None, Some(token!(--"unknown")))
        );

        test_complete!(complete, Option<bool>, "tr" => {
            consumed: true,
            remaining: None,
            suggestions: ["ue"]
        });

        test_complete!(complete_consumed, Option<bool>, "true " => {
            consumed: true,
            remaining: Some(None),
            suggestions: []
        });

        test_parse!(
            tuple_of_options_all, Vec<(bool, Option<bool>)>,
            "true false true false" => Ok(vec![(true, Some(false)), (true, Some(false))], None)
        );
        test_parse!(
            tuple_of_options_missing, Vec<(bool, Option<bool>)>,
            "true false true" => Ok(vec![(true, Some(false)), (true, None)], None)
        );
        test_parse!(
            tuple_of_options_unknown_attr, Vec<(bool, Option<bool>)>,
            "true false true --unknown" => Ok(vec![(true, Some(false)), (true, None)], Some(token!(--"unknown")))
        );
    }

    mod parse_default {
        use super::*;
        use std::marker::PhantomData;

        test_parse!(
            parse_union, (),
            "any" => Ok((), Some(token!("any")))
        );
        test_parse!(
            parse_phantom_data, PhantomData<u8>,
            "any" => Ok(PhantomData, Some(token!("any")))
        );
    }
}
