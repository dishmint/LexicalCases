# Lexical Patterns

A lexical pattern is a [StringExpression](https://reference.wolfram.com/language/ref/StringExpression.html) that includes lexical categories.

Replacement rules allow for extraction of certain matches:
```Mathematica
StringExpression["Alice ", verb:TextType["Verb"], " ", TextType["Preposition"], " ", OptionalLexicalPattern["the"], " ",TextType["Noun"], WordBoundary] :> verb
```

LexicalCases will interpret the following alongside valid StringExpression patterns:
* BoundedString[s] — Matches a string surrounded by explicit WordBoundaries
* [TextType](./TextType.md) — Matches a lexical category
* Opt — Matches 0 or 1 instances of its arguments
* Sandwich — sandwiches expr with some other expr
* Words — matches n words
