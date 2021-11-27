# LexicalCases

## Introduction

[LexicalCases](./LexicalCases.md) allows one to search for lexical patterns. The searchterm is defined by a [LexicalPattern](./LexicalPattern.md), and results are contained in a [LexicalSummary](./LexicalSummary.md) object (see the doc for supported properties).

## Installation

After cloning the repo or downloading the individual files, load the definitions using `Get`:

```Mathematica
Get["path/to/LexicalCases.wl"]
Get["path/to/LexicalCasesTests.wl"]
```

---
## Examples
#### A pattern representing the structure "This :Verb: a :Adjective: :Noun:!"

```Mathematica
LexicalPattern["This ", TextType["Verb"], " a ", TextType["Adjective"], " ", TextType["Noun"], "!"]
```
---
#### Visualize a LexicalPattern's structure
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
