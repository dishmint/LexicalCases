# TextSequenceCases

Extract and analyze text type sequences with the Wolfram Language.

---
## Background

I needed a function that could show me common adjectives associated with a particular topic or object. I was able to write it, but I wanted to support more text types.

Facilitating this is the concept of a TextPattern: an arrangement of text and text content types.

For example:
```Mathematica
TextPattern["this","is", "a", TextType["Adjective"], TextType["Noun"]]
```

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
