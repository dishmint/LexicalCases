# LexicalCases

Extract substrings that match a lexical pattern. Lexical patterns describe string patterns that include lexical categories, like parts of speech, or wolfram language enities.

## Install
After cloning this repository or downloading the package files, use `Get` to load definitions
```Mathematica
Get["path/to/LexicalCases.wl"]
Get["path/to/LexicalCasesTests.wl"]
```

## Usage

[LexicalCases](https://github.com/dishmint/LexicalCases/wiki/LexicalCases) takes a string as its first argument, and a lexical pattern as its second argument.

```Mathematica
LexicalPattern["This is a cool string", LexicalPattern["This is a ", TextType["Adjective"], " string"]]
```

For additional syntax, visit the [LexicalCases](https://github.com/dishmint/LexicalCases/wiki/LexicalCases) wiki

