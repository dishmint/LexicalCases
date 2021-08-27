# TextSequenceCases

Extract and analyze text type sequences with the Wolfram Language.

---
## Background

I needed a function that could show me common adjectives associated with a particular topic or object. I wrote a function to search relevant Wikipedia articles for particular sequences like ("adjective", "air craft"), but I wanted to support more text types.

Introducing the TextPattern: an arrangement of text and text content types. Here's a basic example of what it looks like:

For example:
```Mathematica
TextPattern["this","is", "a", TextType["Adjective"], TextType["Noun"]]
```

TextPatterns are Pattern Objects that expand during the evaluation of TextSequenceCases based on the sourcetext.

---
## Usage:

TextSequenceCases looks for sequences in a text matching a TextPattern.

```Mathematica
TextSequenceCases[sourcetext, TextPattern[...]]
```
---
## How does it work?

### 1 — TextPattern Expansion
* Replaces TextType[<type>] with all occurrences of that type in source-text
* Converts Text Pattern Objects to built-in WL Pattern Objects
### 2 — Source Text Tokenization
* Applies TextWords to sourcetext
### 3 — TextPattern SequenceCases
```Mathematica
SequenceCases[tokenizedSourceText, ExpandedTextPattern]
```
