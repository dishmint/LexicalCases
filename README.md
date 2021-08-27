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
Under the hood

TextSequenceCases works thusly:

1. Expands the TextPattern
	1.1 Replaces TextType[<type>] with all occurrences of that type in source-text
	1.2 Converts Text Pattern Objects to built-in WL Pattern Objects
2. Tokenizes source-text
	2.1 Applies TextWords to source-text
3. Looks for Sequences in tokenized-source-text matching the expanded TextPattern
	3.1 SequenceCases[tokenized-source-text, expanded-text-pattern]
