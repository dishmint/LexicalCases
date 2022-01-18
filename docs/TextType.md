# TextType

A `TextType` represents a lexical category. The syntax is `TextType[type]`. ([List of Types](https://reference.wolfram.com/language/guide/TextContentTypes.html))

TextType example:
```Mathematica
"This"~~TextType["Verb"]~~"a"~~TextType["Adjective"]~~TextType["Noun"]~~"!"
```

The Wolfram Language knows about entities, as well as parts of speech. Below, `"Ocean"|"Person"` replaces the `Noun` type from the previous example. Note how TextTypes can contain alternatives.
```Mathematica
"This"~~TextType["Verb"]~~"a"~~TextType["Adjective"]~~TextType["Ocean"|"Person"]~~"!"]
```

Use `Structure` to visualize the pattern structure.

_Note: TextTypes that match large sections of text (Phrase types for example) may cause memory errors when searching over articles._
