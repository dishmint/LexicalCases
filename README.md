# LexicalCases [EXPERIMENTAL]

Extract substrings matching a lexical pattern.

## Install
Load the paclet from the Paclet Repository
```Mathematica
PacletInstall[ResourceObject["FaizonZaman/LexicalCases"]]
Needs["LexicalCases`"]
```

_Supports v14.0+_

## Usage

Search strings, files or wikipedia articles for a lexical pattern.

```Mathematica
oosp = ExampleData[{"Text", "OriginOfSpecies"}];
oospPattern = Sandwich[WordToken[2], BoundToken["specie"|"species"]];

oospResults = LexicalCases[oosp, oospPattern]
```


---

All [Text Content Types](https://reference.wolfram.com/language/guide/TextContentTypes.html) can be used, however, some will take unreasonably long to expand, especially if it's meant to represent a hefty piece of text, like a topic type. The basic parts of speech types are good ones to start with:

```Mathematica
alice = ExampleData[{"Text", "AliceInWonderland"}];
alicePattern = "Alice" ~~ TypeToken["Verb"] ~~ TypeToken["Adverb"];

aliceResults = LexicalCases[alice, alicePattern]
```

---

Use lexical patterns in `StringCases`, `StringPosition` and `StringmatchQ` by wrapping the pattern with `LexicalPattern`.

Here's an example creating an operator of StringCases:
```Mathematica
aliceOp = StringCases[LexicalPattern["Alice" ~~ TypeToken["Verb"] ~~ TypeToken["Adverb"]]];
```

---

The paclet documentation includes additional examples, or visit [LexicalCases](https://resources.wolframcloud.com/PacletRepository/resources/FaizonZaman/LexicalCases/) on the Wolfram Paclet Repository.
