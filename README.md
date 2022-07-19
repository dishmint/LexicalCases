# LexicalCases [EXPERIMENTAL]

Extract substrings matching a lexical pattern.

## Install
Load the paclet from the Paclet Repository
```Mathematica
PacletInstall[ResourceObject["FaizonZaman/LexicalCases"]]
Needs["LexicalCases`]
```

_v12.3+ supported_

## Usage

Search strings, files or wikipedia articles for a lexical pattern.

<img width="992" alt="Screen Shot 2022-02-04 at 5 21 50 AM" src="https://user-images.githubusercontent.com/18143853/152512756-d19d2b1b-6472-45b1-b5d7-a73351021bc6.png">

```Mathematica
oosp = ExampleData[{"Text", "OriginOfSpecies"}];
oospPattern = Sandwich[WordToken[2], BoundToken["specie"|"species"]];

oospResults = LexicalCases[oosp, oospPattern]
```


---

There's support for most text content types. In this example I've used parts-of-speech types, and grouped the results by occurrence count.

<img width="735" alt="Screen Shot 2022-03-25 at 1 05 59 AM" src="https://user-images.githubusercontent.com/18143853/160058221-cb27304b-b1c0-4f78-8e4d-421622aac264.png">

```Mathematica
alice = ExampleData[{"Text", "AliceInWonderland"}];
alicePattern = "Alice" ~~ TextType["Verb"] ~~ TextType["Adverb"];

aliceResults = LexicalCases[alice, alicePattern]
```

---

Use lexical patterns in `StringCases`, `StringPosition` and `StringmatchQ` by wrapping the pattern with `LexicalPattern`.

<img width="1642" alt="Screen Shot 2022-03-25 at 1 10 31 AM" src="https://user-images.githubusercontent.com/18143853/160058465-f7630599-80a3-4ce6-bc82-407e6d216385.png">

Use LexicalPattern in operator forms of string functions.
```Mathematica
aliceOp = StringCases[LexicalPattern["Alice" ~~ TextType["Verb"] ~~ TextType["Adverb"]]];
```

---

See the included Paclet documentation for additional examples.
