# TextType

A `TextType` represents a lexical category. The syntax is `TextType[type]`. Find a list of types [here](https://reference.wolfram.com/language/guide/TextContentTypes.html).

A sample pattern with TextType's
```Mathematica
LexicalPattern["This ", TextType["Verb"]," a ",TextType["Adjective"]," ", TextType["Noun"],"!"]
```

The Wolfram Language knows about entities, as well as parts of speech. Below, `"Ocean"|"Person"` replaces the `Noun` type from the previous example. Note how TextTypes can contain alternatives.
```Mathematica
LexicalPattern["This ", TextType["Verb"]," a ",TextType["Adjective"]," ", TextType["Ocean"|"Person"],"!"]
```

Use `ToTextElementStructure` to visualize the pattern structure.

_Note: TextTypes that match large sections of text (Phrase types for example) may cause memory errors when searching over articles._
