# LexicalCases

Extract substrings matching a lexical pattern.

## Install
After cloning this repository or downloading the package files, use `Get` to load definitions
```Mathematica
Get["path/to/LexicalCases.wl"]
Get["path/to/LexicalCasesTests.wl"]
```

## Usage

[LexicalCases](https://dishmint.github.io/LexicalCases/LexicalCases.html) takes a string as its first argument, and a lexical pattern as its second argument.

```Mathematica
LexicalPattern["This is a cool string", LexicalPattern["This is a ", TextType["Adjective"], " string"]]
```

For additional syntax, visit the [LexicalCases](https://dishmint.github.io/LexicalCases/LexicalCases.html) documentation.

