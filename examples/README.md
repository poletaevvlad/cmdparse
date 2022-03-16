# Examples

There are several examples that demonstrate how to use `cmdparse`. These are
more complete then examples provided in the documentation, as they include
handling user input from stdin using
[`rustyline`](https://github.com/kkawakam/rustyline) crate. Note that
`rustyline` is not required to use `cmdparse`, it was chosen for easier
demonstration of completion capabilities.


## `key_value`

```
$ cargo run --example key_value
storage> set fe iron
OK
storage> set o oxygen
OK
storage> get fe
iron
storage> get-strlen o
6
storage> get-substr o 3
oxy
storage> get-substr o 3 --start 3
gen
```

This example program demonstrates how `cmdparse` can be used to build
redis-like in-memory key-value storage with an interactive CLI interface.

**Demonstrated functionality**:

* *Custom parser with a custom context*: a custom context (in this case a
  simple immutable reference to the hash map is sufficient) along with a custom
  parser for automatic completion of keys.
* Usage of `Parsable` derive macro for a user-defined enum.
* Accepting the user input with `rustyline` with support for autocompletion.


## `arithmetic`

```
$ cargo run --example arithmetic
>> / (+ 1 sqrt 5) 2
<< 1.618033988749895
>> / 1 * (/ * 2 sqrt 2 9801) 1103
<< 3.1415927300133055
```

This example demonstrates less than conventional usage of `cmdparse`: it
evaluates an expression written in the [Polish (prefix)
notation](https://en.wikipedia.org/wiki/Polish_notation) as part of the parsing
process.

**Demonstrated functionality**:

* Usage of `ParsableTransformation` for data validation and converting the
  value to another as part of the parsing process.
* Usage of `parser`, `rename`, `transparent` attributes of the `Parsable` derive macro.


## smallvec

```
$ cargo run --example smallvec
>> 1 2 3 4
<< [1, 2, 3, 4]
>> 9 5 7 1 8 4 6 2 3
<< [9, 5, 7, 1, 8, 4, 6, 2, 3]
>>
<< []
```

This example demonstrates how to implement a parser for a collection that is
defined in a foreign trait. The difficulty lies in the fact that it's not
possible to define `ParsableCollection` trait on such type directly because
neither `ParsableCollection` trait nor the collection type itself are defined
in the user's crate.

**Demonstrated functionality**:

* Using `ParsableCollection` trait to implement a parser for a custom linear
  collection.
* Using `ParsableTransformation` along with newtype pattern to easily convert
  between type when parsing.
