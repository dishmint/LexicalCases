# LexicalCases [EXPERIMENTAL]

Extract substrings matching a lexical pattern.

## Install
After cloning this repository or downloading the paclet file, use PacletInstall and Needs to load definitions.
```Mathematica
PacletInstall["path/to/LexicalCases-0.0.1.paclet"]
Needs["LexicalCases`"]
```

_v12.3+ supported_

## Usage

[LexicalCases](https://dishmint.github.io/LexicalCases/LexicalCases.html) takes a string as its first argument, and a lexical pattern (a [StringExpression](https://reference.wolfram.com/language/ref/StringExpression.html) including TextTypes) as its second argument. A [LexicalSummary](https://dishmint.github.io/LexicalCases/LexicalSummary.html) object is returned with properties to access and manipulate the results.

```Mathematica
LexicalCases["This is a cool string", "This is a "~~TextType["Adjective"]~~" string"]]
```

Visit the [documentation](https://dishmint.github.io/LexicalCases/) for additional examples.

