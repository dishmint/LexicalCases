# LexicalPattern

A LexicalPattern describes a string pattern that includes lexical categories. It's essentially implemented on top of [StringExpression](https://reference.wolfram.com/language/ref/StringExpression.html).

Replacement rules allow for extraction of certain matches:
```Mathematica
LexicalPattern["Alice ", verb:TextType["Verb"], " ", TextType["Preposition"], " ", OptionalLexicalPattern["the"], " ",TextType["Noun"], WordBoundary] :> verb
```

Supported expressions are:
* [Strings](https://reference.wolfram.com/language/ref/String.html) — Matches a string
* [TextType](./TextType.md) — Matches a lexical category
* OptionalLexicalPattern — Matches 0 or 1 instances of its arguments
* OrderlessLexicalPattern — Matches its arguments in any order
* Most system Pattern symbols work, like `WordCharacter` or `DigitCharacter` for example. Use `$LexicalPatternValidHeads` to see supported symbols.
