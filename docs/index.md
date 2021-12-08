# LexicalCases

## Introduction

The Wolfram Language supports text search, text analysis, and text parsing in various ways. Search for substrings with [StringCases](https://reference.wolfram.com/language/ref/StringCases.html), entities with [TextCases]((https://reference.wolfram.com/language/ref/TextCases.html),  or return all examples of identifiable entities with [TextContents](https://reference.wolfram.com/language/ref/TextContents.html) etc. The current approach to search for lexical patterns is to use the Containing wrapper, though it isn't a flexible strategy. With LexicalCases, one can search for complex and refined lexical patterns.


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
ToTextElementStructure[LexicalPattern["This ", TextType["Verb"], " a ", TextType["Adjective"], " ", TextType["Noun"], "!"]]
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
