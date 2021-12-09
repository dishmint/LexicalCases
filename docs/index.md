# LexicalCases

## Introduction

The Wolfram Language supports text search, parsing, and analysis in the following ways: substring search with [StringCases](https://reference.wolfram.com/language/ref/StringCases.html), entity extraction with [TextCases](https://reference.wolfram.com/language/ref/TextCases.html), and entity recognition with [TextContents](https://reference.wolfram.com/language/ref/TextContents.html). When one wants to find specific structures, the [Containing](https://reference.wolfram.com/language/ref/Containing.html) wrapper allows for defining rich, hierarchical lexical structures, but it isn't flexible. LexicalPatterns,  a pattern containing strings, pattern objects and lexical categories, provides this "low-level" control, which LexicalCases compiles into StringExpressions for searching. Below you will find basic [examples](#examples), for more information consult the [Resources](#resources) section.

## Installation

After cloning the repo or downloading the individual files, load the definitions using `Get`:

```Mathematica
Get["path/to/LexicalCases.wl"]
Get["path/to/LexicalCasesTests.wl"]
```


---
A [LexicalPattern](./LexicalPattern.md) is a string pattern whose elements include text content types (those listed [here](https://reference.wolfram.com/language/guide/TextContentTypes.html)). [LexicalCases](./LexicalCases.md) searches for these patterns in strings. The result is a [summary](./LexicalSummary.md) object which supporting  properties for data access and analysis (see the doc or query the summary object's `"Properties"` for a listing).

## Examples
#### A pattern representing the structure "This \<Verb\> a \<Adjective\> \<Noun\>!"

```Mathematica
LexicalPattern["This ", TextType["Verb"], " a ", TextType["Adjective"], " ", TextType["Noun"], "!"]
```
---
#### Visualize its structure
![Text Element Structure of a Lexical Pattern](./assets/images/LexicalPattern-TextElementStructure.png)


```Mathematica
LexicalPatternStructure[LexicalPattern["This ", TextType["Verb"], " a ", TextType["Adjective"], " ", TextType["Noun"], "!"]]
```
---
#### Find cases of the pattern in a string
![Lexical Cases Example on a string](./assets/images/LexicalCases-Example1.png)


```Mathematica
LexicalCases["This is really cool!", LexicalPattern["This is ", TextType["Adverb"], " ", TextType["Adjective"],"!"]]
```
---
#### Use replacement rules to extract pieces of the pattern
![Lexical Cases Example with Rule on a string](./assets/images/LexicalCases-Example1_Rule.png)


```Mathematica
LexicalCases["This is a cool thing!", LexicalPattern["This ", TextType["Verb"], " a ", adj : TextType["Adjective"], " ", TextType["Noun"], "!"] :> adj]
```

---
## Resources

For more information see the following:
* [LexicalCases](./LexicalCases.md)
* [LexicalPattern](./LexicalPattern.md)
* [LexicalSummary](./LexicalSummary.md)
* [TextType](./TextType.md)

Related Wolfram Language Documentation:
* [TextCases](https://reference.wolfram.com/language/ref/TextCases.html)
* [TextContents](https://reference.wolfram.com/language/ref/TextContents.html)
* [TextContentTypes](https://reference.wolfram.com/language/guide/TextContentTypes.html)
