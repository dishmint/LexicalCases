# LexicalCases

## Installation

After cloning the repo or downloading the individual files, load the definitions using `Get`:

```Mathematica
Get["path/to/LexicalCases.wl"]
Get["path/to/LexicalCasesTests.wl"]
```


## Introduction

[LexicalCases](https://github.com/dishmint/LexicalCases/wiki/LexicalCases) allows one to search for lexical patterns, returning a [LexicalSummary](./LexicalSummary.md) object with support for properties like match counts.

A sample LexicalPattern
```Mathematica
LexicalPattern["This is ", TextType["Adverb"], " ", TextType["Adjective"],"!"]
```

Call LexicalCases with this pattern
```Mathematica
LexicalCases["This is really cool!", LexicalPattern["This is ", TextType["Adverb"], " ", TextType["Adjective"],"!"]]
```

Extract the data
```Mathematica
In[] := ls = LexicalCases["This is really cool!", LexicalPattern["This is ", TextType["Adverb"], " ", TextType["Adjective"],"!"]]
Out[] = {<|"Match" -> "This is really cool!", "Position" -> {{1, 20}}|>}
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
