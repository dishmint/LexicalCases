# TextSequenceCases

Extract and analyze text type sequences with the Wolfram Language.

This package introduces the concept of a TextPattern: an arrangement of text and text content types.

For example:
```Mathematica
TextPattern["this","is", "a", TextType["Adjective"], TextType["Noun"]]
```

---
Usage:

TextSequenceCases looks for sequences in a text matching a TextPattern.

```Mathematica
TextSequenceCases[sourcetext, TextPattern[...]]
```
---
How does it work?

### 1 — TextPattern Expansion
* Replaces TextType[<type>] with all occurrences of that type in source-text
* Converts Text Pattern Objects to built-in WL Pattern Objects
### 2 — Source Text Tokenization
* Applies TextWords to sourcetext
### 3 — TextPattern SequenceCases
```Mathematica
SequenceCases[tokenizedSourceText, ExpandedTextPattern]
```
