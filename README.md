# ParserCombinators

This is a proof of concept for a parser combinator library based on algebraic effects and handlers in the Flix programming language.

# Disclaimer

Although the library is complete and functional, this project is more of an experiment to explore effect-oriented programming than anything else. Also it should be noted that the derivation of semantics from the build parser might be very unergonomic. This is due to a the feature of polymorphic effects not yet being implemented into Flix as of Version 0.6.0. The parsers will thus only be able to produce a semantic result of the type that is hard coded into their signatures.

# Project Overview

The library provides higher order parsers that can be combined in order to build recursive-decent parsers. Note that this generally confines the capabilites of the parser to context free languages. Additionally when closely aligning the parser construction with an existing grammar, the grammar can not be ambiguous or contain left recursive productions!

The difference between this library and most parser combinator libraries it that it uses side effects and appropriate handlers instead of monads to facilitate the combination of higher order parsers. [Hutton, Graham, and Erik Meijer. "Monadic parser combinators." (2011)](https://cspages.ucalgary.ca/~robin/class/521/class-handout.pdf#page=282)
 provides a great introduction into the concepts of parser combinators. A comprehensive tutorial on to the concept of algebraic handlers and their usage can be found in [Pretnar, Matija. "An introduction to algebraic effects and handlers. invited tutorial paper." (2015)](https://www.eff-lang.org/handlers-tutorial.pdf). Generally parser-combinators are a popular example for possible application of algebraic effects. This project is mostly inspired by the works in [Xie, Ningning, and Daan Leijen. "Effect handlers in Haskell, evidently." (2020)](https://xnning.github.io/papers/haskell-evidently.pdf) and [Lindley, Sam. "Algebraic effects and effect handlers for idioms and arrows." (2014)](https://homepages.inf.ed.ac.uk/slindley/papers/aeia.pdf).

 Other similar projects can be found here:
- [Flix parser combinators library](https://github.com/jaschdoc/flix-parsers)
- [Koka Parser example](https://github.com/koka-lang/koka/blob/dev/samples/handlers/parser.kk)

# Usage

Te library provides some atomic parsers that consume a singular character.

- `symbol(x)` - accepts the char 'x'
- `letter()`- accepts any letter
- `digit()` - accepts any digit

The `Parse.sat` effect can easily be used to create custom parsers. It takes in a predicate (Char -> Bool) as a parameter to decide a language where every word is exactly one character.

Alongside the atomic parser we have menas to combine them to inductively build more complex parsers that accept more complex languages.

- `choice(p1, p2)` - accepts the union of the languages accepted by p1 and p2
- `seq(p1, p2)` - accepts the concatenation of the languages accepted by p1 and p2
- `many(p)` - accepts the Kleene closure of p
- `many1(p)` - accepts any amount greater than one of repititive concatenation of a language to itself

The construction of parsers is then similar to the construction of regular expressions or context free grammars. The combination of parser works almost identically to the inductive definition of regular expressions:

```
// L = (a | b)*
def parseL() : PResult \ Parse = many(_ -> choice(_ -> symbol('a'), _ -> symbol('b')))
```

For context free grammar we construct a parser for each non-terminal:

```
// S -> ZQ
// Z -> 0Z | 0
// Q -> 1Q | 1

def parseS() : PResult \ Parse = seq(_ -> parseZ(), _ -> parseQ())

def parseZ() : PResult \ Parse =
    choice(
        _ -> seq(_ -> symbol('0'), _ -> parseZ()),
        _ -> symbol('0')
    )

def parseQ() : PResult \ Parse =
    choice(
        _ -> seq(_ -> symbol('1'), _ -> parseQ()),
        _ -> symbol('1')
    )
```

For some more examples of languages see the [tests](test/TestLanguages.flix).

The main function to initiate the decision process is:

`invokeParser(input : String, parser : Unit -> PResult \ Parse, transform : Char -> Sem, op : (Sem, Sem) -> Sem, e : Sem): PResult`

where the input and parser parameters are concerned with the parsing portion of the library. The other parameters specify how the semantic shall be derived while parsing. The latter part is still very rudimentary and especially limited due to the afformentioned absence of polymporphic effects in Flix at this stage (0.6.0). Examples of calls could look like this:

`invokeParser(input, parser, x -> x::Nil, (l,r) -> l ::: r, Nil)`

The semantic will then be constructed as follows:
- Each atomic parser casts the read Char into a singleton list via `transform = x -> x::Nil`
- seqentially applied parser combine their semantic results by concatenating them via `op = (l,r) = l ::: r`
- lastly the we provide `e = Nil` as the neutral element of our `op`-function
