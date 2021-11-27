# LexicalCases

## Installation

After cloning the repo or downloading the individual files, load the definitions using `Get`:

```Mathematica
Get["path/to/LexicalCases.wl"]
Get["path/to/LexicalCasesTests.wl"]
```


## Introduction

[LexicalCases](./LexicalCases.md) allows one to search for lexical patterns. The results are contained in a [LexicalSummary](./LexicalSummary.md) object (see the doc for a listing of properties).


> A pattern representing the structure "This is a :Adverb: :Adjective:!"
```Mathematica
LexicalPattern["This is ", TextType["Adverb"], " ", TextType["Adjective"],"!"]
```

> Search a string with this pattern
```Mathematica
LexicalCases["This is really cool!", LexicalPattern["This is ", TextType["Adverb"], " ", TextType["Adjective"],"!"]]
```

> Extract data from the summary object
```Mathematica
ls = LexicalCases["This is really cool!", LexicalPattern["This is ", TextType["Adverb"], " ", TextType["Adjective"],"!"]];
ls["Data"]

{<|"Match" -> "This is really cool!", "Position" -> {{1, 20}}|>}
```

For more information see the following:
* [LexicalCases](./LexicalCases.md)
* [LexicalPattern](./LexicalPattern.md)
* [LexicalSummary](./LexicalSummary.md)
* [TextType](./TextType.md)

For similar Wolfram Language functions see the following:
* [TextCases](https://reference.wolfram.com/language/ref/TextCases.html)
* [TextContents](https://reference.wolfram.com/language/ref/TextContents.html)
* [TextContentTypes](https://reference.wolfram.com/language/guide/TextContentTypes.html)
