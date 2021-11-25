# LexicalCases

Search text for lexical patterns.

---
## Overview

Find text matching a lexical pattern.

For example, the following will match text of the form "this is a cool place", or "this is a wonderful book".
```Mathematica
TextPattern["this is a ", TextType["Adjective"], " ", TextType["Noun"]]
```


---
## Usage:

LexicalCases looks for cases of TextPattern in a text. The first argument is the source text, the second argument is the text pattern.

```Mathematica
LexicalCases[sourcetext, TextPattern[...]]
```

TextPattern supports the following constructs:
* TextPatternSequence: A sequence of text pattern objects or strings
* OptionalTextPattern: An optional text pattern or string
* OrderlessTextPattern: An orderless sequence of text pattern objects or strings
* TextType[type]: Text content recognized by the Wolfram Language, find a full list of types [here](https://reference.wolfram.com/language/guide/TextContentTypes.html).
