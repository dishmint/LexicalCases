# TextStructureCases

Extract abstract text patterns from a source text.

---
## Overview

Find bits of text that match a pattern specified by TextPattern.

For example, the following will match strings of the form "this is a cool place", or "this is a wonderful book".
```Mathematica
TextPattern["this is a ", TextType["Adjective"], " ", TextType["Noun"]]
```


---
## Usage:

TextStructureCases looks for cases of TextPattern in a text. The first argument is the source text, the second argument is the text pattern.

```Mathematica
TextStructureCases[sourcetext, TextPattern[...]]
```

TextPattern supports the following constructs:
* TextPatternSequence: A sequence of text pattern objects or strings
* OptionalTextPattern: An optional text pattern or string
* OrderlessTextPattern: An orderless sequence of text pattern objects or strings
* TextType[type]: Text content recognized by the Wolfram Language, full list of types can eb found [here](https://reference.wolfram.com/language/guide/TextContentTypes.html).
